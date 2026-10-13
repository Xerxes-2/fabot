/// Answers "how many" and "who casts", never "who does what": that is the
/// matcher's.
module Fabot.Core.Economy.Quota

open Fabot.Core
open Fabot.Core.Vocabulary
open Fabot.Core.World
open Fabot.Core.Economy.Bodies
open Fabot.Core.Economy.Derived
open Fabot.Core.Economy.Pool

[<NoEquality; NoComparison>]
type Seat =
    {
        Pattern: Pattern
        At: Pos
        Parts: int
        Colony: int option
        Body: BodyPart list
        Return: float option
    }

type Want =
    {
        Pattern: Pattern
        Wanted: int
        Standing: int
    }

[<NoEquality; NoComparison>]
type Cast =
    {
        Colony: string
        Spawn: string
        Body: BodyPart list
        Name: string
        Floor: bool
    }

[<NoEquality; NoComparison>]
type Plan =
    {
        Wants: Want list
        Seats: Seat list
        Casts: Cast list
    }

let lead (body: Body) (walk: int) : int =
    body.Size * Engine.spawnTicksPerPart + max 0 walk

/// Out of the count inside its successor's lead, so the successor is cast
/// while it still works and arrives as it dies.
let expiring (body: Body) (walk: int) : bool = body.TicksToLive <= lead body walk

let private ceilDiv (n: int) (d: int) = (n + d - 1) / d

/// N parts add `N × reservePerClaim` a tick while the reservation decays one.
/// Never under two: one part only holds the reservation still.
let claimPartsFor (held: int) : int =
    let perTick =
        ceilDiv (max 0 (Engine.reservationCap - held)) Engine.claimLifetime + 1

    max 2 (ceilDiv perTick Engine.reservePerClaim)

let private lifeOf (pattern: Pattern) =
    if pattern.IsClaimer then
        Engine.claimLifetime
    else
        Engine.creepLifetime

let private partsOf (pattern: Pattern) (body: BodyPart list) = partCountIn body (keyPartOf pattern)

[<NoEquality; NoComparison>]
type private Demand =
    {
        Pattern: Pattern
        At: Pos
        Wanted: int
        Seats: int
        Spill: float
        Value: BodyPart list -> float option
        mutable Left: int
        mutable SeatsLeft: int
    }

let private seated pattern at wanted seats value =
    {
        Pattern = pattern
        At = at
        Wanted = max 0 wanted
        Seats = max 0 seats
        Spill = 0.0
        Value = value
        Left = max 0 wanted
        SeatsLeft = max 0 seats
    }

let private demand pattern at wanted value =
    seated pattern at wanted System.Int32.MaxValue value

let private bodyFor (colony: Colony) (d: Demand) : BodyPart list option =
    if d.Pattern.IsHarvester && d.At.Room = colony.Room then
        homeHarvester d.Wanted colony.BankCapacity
    else
        sized d.Pattern d.Wanted colony.BankCapacity

let private homeOf (colony: Colony) = colony.Spawns.[0].Pos

let private sinkOf (derived: Derived) (ci: int) =
    derived.Grounds.[ci].Stores
    |> Array.tryFind (fun s -> s.Owned)
    |> Option.map (fun s -> s.Pos)
    |> Option.defaultValue (homeOf derived.Colonies.[ci])

let private nearest
    (distance: Pos -> Pos -> int option)
    (colonies: Colony[])
    (can: int -> bool)
    (at: Pos)
    =
    let mutable best = -1
    let mutable least = 0

    for ci in 0 .. colonies.Length - 1 do
        if can ci then
            match distance (homeOf colonies.[ci]) at with
            | Some ticks when best < 0 || ticks < least ->
                best <- ci
                least <- ticks
            | _ -> ()

    if best < 0 then None else Some best

/// Ticks a body of the parts walks loaded from a tile to within a range of
/// another.
type Leg = BodyPart list -> Pos -> Pos -> int -> int option

/// The walk there loaded and back empty: an empty Carry weighs nothing, so
/// each leg is its own flood toward `work`.
let private roundTrip (leg: Leg) (body: BodyPart list) (fetch: Pos) (work: Pos) (range: int) =
    let empty = body |> List.filter (fun p -> not p.IsCarry)

    match leg body fetch work range, leg empty fetch work range with
    | Some there, Some back -> Some(max 2 (there + back))
    | _ -> None

/// `lifetimes` is the owner's number (`ARCHITECTURE.md` §5).
let reserveTarget (lifetimes: float) (level: int) : int =
    float (
        Engine.controllerStructures "tower" level
        * Engine.towerEnergyCost
        * Engine.creepLifetime
    )
    * lifetimes
    |> int

let private ownerOf (distance: Pos -> Pos -> int option) (derived: Derived) (gi: int) =
    let colonies = derived.Colonies

    if gi < colonies.Length then
        Some gi
    else
        let ground = derived.Grounds.[gi]

        ground.Controller
        |> Option.map (fun c -> c.Pos)
        |> Option.orElse (ground.Sources |> Array.tryHead |> Option.map (fun s -> s.Pos))
        |> Option.bind (nearest distance colonies (fun _ -> true))

/// The backlog is spread over a creep's life, so a hauler cast now clears it
/// before it dies. A dig that falls on the floor is the workers' to fetch.
let private haulFlows (ground: Ground) (s: SourceFact) (post: Pos) (carried: float) =
    match s.Container with
    | None -> []
    | Some id ->
        let stored =
            ground.Stores |> Array.sumBy (fun st -> if st.Id = id then st.Energy else 0)

        let piled =
            ground.Piles
            |> Array.sumBy (fun p ->
                if p.Resource = Energy && Pos.within 1 p.Pos s.Pos then
                    p.Amount
                else
                    0)

        let left =
            float s.Dug - carried + float (stored + piled) / float Engine.creepLifetime

        match ground.Receiver with
        | Some r when r.Role.IsHub && carried > 0.0 -> [ post, left; r.Pos, carried ]
        | _ -> [ post, left ]

/// A hub receiver drains without limit: the carriers empty it into the storage.
let private linkDrain (derived: Derived) (ground: Ground) : float =
    match ground.Receiver, ground.Controller with
    | Some r, Some c when r.Role.IsController ->
        let work =
            Array.append derived.Bodies derived.Ovens
            |> Array.sumBy (fun b -> if upgrades b && b.Pos.Room = ground.Room then b.Work else 0)

        let drain = work * Engine.upgradeDrainPerWork

        float (
            if c.Level >= Engine.controllerMaxLevel then
                min drain Engine.maxUpgradePerTick
            else
                drain
        )
    | Some _, _ -> infinity
    | None, _ -> 0.0

/// A haul trip is to the dearest sink, not the nearest: a ring fills in a
/// trip, and the flow that goes on all day is to the far sink.
let private groundDemands (distance: Pos -> Pos -> int option) (leg: Leg) (derived: Derived) =
    let demands = ResizeArray<Demand>()
    let life = float Engine.creepLifetime
    let colonies = derived.Colonies
    let carries = Array.zeroCreate<float> colonies.Length
    let visits = Array.zeroCreate<int> colonies.Length
    let flows = Array.zeroCreate<float> colonies.Length
    let digs = Array.init colonies.Length (fun _ -> ResizeArray<int * Demand>())

    let sinksOf ci =
        let ground = derived.Grounds.[ci]

        homeOf colonies.[ci]
        :: (Array.append
                (Option.toArray ground.Feed)
                (ground.Stores |> Array.filter (fun st -> st.Owned))
            |> Array.map (fun st -> st.Pos)
            |> List.ofArray)

    derived.Grounds
    |> Array.iteri (fun gi ground ->
        let owner = ownerOf distance derived gi
        let mutable drain = linkDrain derived ground

        for s in ground.Sources do
            match s.Post with
            | Some post ->
                // A post with no container yet piles its dig, and a pile decays.
                let spill = if Option.isSome s.Container then 0.0 else Price.pileDecay 1

                let dig =
                    { seated Pattern.Harvester s.Pos (digWork s.Rate) 1 (fun body ->
                          Some(
                              (float (min s.Rate (partCountIn body Work * Engine.harvestPerWork))
                               - spill)
                              * life
                          )) with
                        Spill = spill
                    }

                demands.Add dig

                match owner with
                | Some ci when Option.isSome s.Container ->
                    let carried =
                        if Option.isSome s.Link then
                            min (float s.Dug) drain
                        else
                            0.0

                    drain <- drain - carried
                    let mutable hauled = false

                    for fetch, flow in haulFlows ground s post carried do
                        let trip =
                            sized Pattern.Hauler Engine.maxBodyParts colonies.[ci].BankCapacity
                            |> Option.map (fun body ->
                                sinksOf ci
                                |> List.choose (fun sink -> roundTrip leg body fetch sink 1)
                                |> List.fold max 0)
                            |> Option.defaultValue 0

                        if trip > 0 then
                            hauled <- true
                            carries.[ci] <- carries.[ci] + flow * float trip

                            // A body that dies leaves its visit undone while its
                            // successor walks a room out: no remote container
                            // may fill in twice the gap between visits.
                            if gi >= colonies.Length then
                                visits.[ci] <-
                                    visits.[ci]
                                    + int (
                                        ceil (
                                            2.0 * flow * float trip
                                            / float Engine.containerCapacity
                                        )
                                    )

                            flows.[ci] <- flows.[ci] + flow

                    if hauled then
                        digs.[ci].Add(s.Rate, dig)
                | _ -> ()
            | None -> ()

        match ground.Controller with
        | Some c when ground.Reservable && gi >= colonies.Length ->
            let gain = float (reserveGain ground * Engine.claimLifetime)

            demands.Add(
                seated Pattern.Claimer c.Pos (claimPartsFor ground.OurReservation) 1 (fun _ ->
                    Some gain)
            )
        | _ -> ())

    for ci in 0 .. colonies.Length - 1 do
        if carries.[ci] > 0.0 then
            let perBody =
                sized Pattern.Hauler Engine.maxBodyParts colonies.[ci].BankCapacity
                |> Option.map (fun body -> partCountIn body Carry)
                |> Option.defaultValue 0

            let wanted =
                max
                    (int (ceil (carries.[ci] / float Engine.carryPartCapacity)))
                    (visits.[ci] * perBody)

            let flow = flows.[ci]
            let digs = digs.[ci]

            demands.Add(
                demand Pattern.Hauler (homeOf colonies.[ci]) wanted (fun body ->
                    let moved = float (partCountIn body Carry) * flow / float wanted

                    // A haul moves only what a dig leaves, so the harvesters
                    // are bought first.
                    let dug =
                        digs
                        |> Seq.sumBy (fun (rate, dig) ->
                            min rate ((dig.Wanted - dig.Left) * Engine.harvestPerWork))

                    Some(min (float dug) moved * life))
            )

    for c in derived.Claims do
        demands.Add(demand Pattern.Claimer c.Pos 1 (fun _ -> None))

    demands

let private spendDemands
    (distance: Pos -> Pos -> int option)
    (leg: Leg)
    (derived: Derived)
    (others: ResizeArray<Demand>)
    =
    let colonies = derived.Colonies
    let n = colonies.Length
    let life = float Engine.creepLifetime
    let income = Array.zeroCreate<float> n
    let owed = Array.zeroCreate<float> n
    let upkeep = Array.zeroCreate<float> n

    derived.Grounds
    |> Array.iteri (fun gi ground ->
        match ownerOf distance derived gi with
        | Some ci ->
            income.[ci] <- income.[ci] + float ground.Posted
            owed.[ci] <- owed.[ci] + float (ground.Sites |> Array.sumBy (fun s -> s.Left)) / life
        | None -> ())

    for d in others do
        match nearest distance colonies (fun _ -> true) d.At with
        | Some ci ->
            match bodyFor colonies.[ci] d with
            | Some body when partsOf d.Pattern body > 0 ->
                let bodies = min d.Seats (ceilDiv d.Wanted (partsOf d.Pattern body))

                upkeep.[ci] <-
                    upkeep.[ci]
                    + float bodies
                      * (float (Engine.bodyCost body) / float (lifeOf d.Pattern) + d.Spill)
            | _ -> ()
        | None -> ()

    colonies
    |> Array.mapi (fun ci colony ->
        let ground = derived.Grounds.[ci]
        let at = colony.Controller.Pos
        let surplus = income.[ci] - upkeep.[ci]
        let storage = ground.Stores |> Array.tryFind (fun s -> s.Owned)

        let reserve, stock =
            match storage with
            | Some s -> reserveTarget derived.ReserveLifetimes colony.Controller.Level, s.Energy
            | None -> 0, 0

        let save = float (reserve - stock) / life |> min surplus |> max 0.0

        let upgradable =
            if colony.Controller.Level >= Engine.controllerMaxLevel then
                Engine.maxUpgradePerTick
            else
                System.Int32.MaxValue

        let upgrader =
            sized Pattern.Upgrader upgradable colony.BankCapacity
            |> Option.filter (fun body ->
                Option.isSome ground.Buffer
                && standing (partCountIn body Work) (partCountIn body Carry))

        // No more upgraders than the buffer seats or the Work the controller
        // takes: one past them draws nothing.
        let upgraders, upgraderWork, upgradersDrain =
            match upgrader with
            | Some body ->
                let work = partCountIn body Work
                let seats = float (min ground.BufferSeats.Length (upgradable / work))

                let lifeCost =
                    float (work * Engine.upgradeDrainPerWork) * life + float (Engine.bodyCost body)

                let onIncome =
                    (surplus - save - owed.[ci]) * life / lifeCost |> max 0.0 |> floor |> min seats

                let onStock =
                    float (stock - reserve) / lifeCost
                    |> max 0.0
                    |> floor
                    |> min (seats - onIncome)

                int (onIncome + onStock), work, onIncome * lifeCost / life
            | None -> 0, 0, 0.0

        let rest = surplus - save - upgradersDrain

        // A worker drains the controller while its load lasts, then walks to
        // its energy and back, to each dig as often as it yields.
        let range = Task.rangeOf (Task.Upgrade colony.Controller.Id)

        let fetches =
            match ground.Buffer, storage with
            | Some buffer, _ -> [| buffer.Pos, 1 |]
            | None, Some storage -> [| storage.Pos, 1 |]
            | None, None ->
                ground.Sources |> Array.map (fun s -> Option.defaultValue s.Pos s.Post, s.Dug)

        let cast = sized Pattern.Worker Engine.maxBodyParts colony.BankCapacity

        let spend =
            match cast with
            | Some body when partsOf Pattern.Worker body > 0 ->
                let work = partsOf Pattern.Worker body

                let mutable weighted = 0
                let mutable weights = 0

                for fetch, weight in fetches do
                    match roundTrip leg body fetch at range with
                    | Some trip ->
                        weighted <- weighted + weight * trip
                        weights <- weights + weight
                    | None -> ()

                let upgrading =
                    float (partCountIn body Carry * Engine.carryPartCapacity)
                    / float (work * Engine.upgradeDrainPerWork)

                if weights = 0 then
                    0
                else
                    let drain =
                        float Engine.upgradeDrainPerWork * upgrading
                        / (upgrading + float weighted / float weights)

                    let upkeep =
                        float (Engine.bodyCost body) / float (work * lifeOf Pattern.Worker)

                    int (ceil (max 0.0 rest / (drain + upkeep)))
            | _ -> 0

        let bodyWork = cast |> Option.map (partsOf Pattern.Worker) |> Option.defaultValue 1

        let floorBodies =
            if ground.Sites.Length > 0 || ground.Repairs.Length > 0 then
                2
            else
                1

        // A crew digs a source whose container does not stand, and builds it.
        // Not knowing a source's seats is no crew.
        let crews =
            ground.Sources
            |> Array.sumBy (fun s ->
                match s.Seats with
                | Some seats when Option.isNone s.Container ->
                    seats - (if Option.isSome s.Post then 1 else 0)
                | _ -> 0)

        [|
            seated
                Pattern.Worker
                at
                (max spend (floorBodies * bodyWork) + crews * bodyWork)
                (match ground.UpgradeSeats with
                 | Some seats -> max floorBodies seats + crews
                 | None -> System.Int32.MaxValue)
                (fun body -> Some(float (partsOf Pattern.Worker body) * life))
            seated Pattern.Upgrader at (upgraders * upgraderWork) upgraders (fun body ->
                Some(float (partsOf Pattern.Upgrader body) * life))
        |])
    |> Array.concat

/// The upgraders last: they spend what every other seat leaves. A tie keeps
/// the demands' order (`List.sortBy` is stable).
let private castOrder (seat: Seat) : int * float =
    match seat.Return, seat.Pattern with
    | None, _ -> 0, 0.0
    | Some _, Pattern.Upgrader -> 3, 0.0
    | Some r, Pattern.Worker -> 2, -r
    | Some r, _ -> 1, -r

/// A seat its bank cannot pay yields to the next it can: an order, never a
/// veto.
let plan
    (distance: Pos -> Pos -> int option)
    (leg: Leg)
    (walk: bool -> Body -> Pos -> int option)
    (derived: Derived)
    (pool: Offer[])
    (assignments: Map<string, string>)
    : Plan =
    let colonies = derived.Colonies
    let others = groundDemands distance leg derived

    let all = Array.append (others.ToArray()) (spendDemands distance leg derived others)

    let standing = System.Collections.Generic.Dictionary<string, int>()

    // The walk at the body's own pace bounds the fresh one (an empty body is
    // never slower), so the fresh walk is asked only inside it.
    let expiringNow (body: Body) =
        let spawnTime = lead body 0

        if body.TicksToLive <= spawnTime then
            true
        else
            match nearest distance colonies (fun _ -> true) body.Pos with
            | Some ci ->
                let home = homeOf colonies.[ci]

                let walked fresh =
                    walk fresh body home
                    |> Option.orElse (distance home body.Pos)
                    |> Option.defaultValue 0

                expiring body (walked false) && expiring body (walked true)
            | None -> false

    for body in Array.append derived.Bodies derived.Ovens do
        if not (expiringNow body) then
            let pattern = nameOf body.Pattern
            let parts = keyPartsIn body.Pattern body

            standing.[pattern] <-
                (if standing.ContainsKey pattern then
                     standing.[pattern]
                 else
                     0)
                + parts

            // The nearest open demand of the pattern; an unreachable one last.
            let mutable pick = -1
            let mutable least = System.Int32.MaxValue

            for i in 0 .. all.Length - 1 do
                let d = all.[i]

                if nameOf d.Pattern = pattern && d.Left > 0 && d.SeatsLeft > 0 then
                    let ticks = distance body.Pos d.At |> Option.defaultValue System.Int32.MaxValue

                    if pick < 0 || ticks < least then
                        pick <- i
                        least <- ticks

            if pick >= 0 then
                let d = all.[pick]
                d.Left <- max 0 (d.Left - parts)
                d.SeatsLeft <- max 0 (d.SeatsLeft - 1)

    let wants =
        allPatterns
        |> List.map (fun pattern ->
            {
                Pattern = pattern
                Wanted =
                    all
                    |> Array.sumBy (fun d ->
                        if nameOf d.Pattern = nameOf pattern then d.Wanted else 0)
                Standing =
                    if standing.ContainsKey(nameOf pattern) then
                        standing.[nameOf pattern]
                    else
                        0
            })

    let bank = colonies |> Array.map (fun c -> c.Bank)

    let idle =
        colonies
        |> Array.map (fun c ->
            c.Spawns |> Array.filter (fun s -> Option.isNone s.Spawning) |> ResizeArray)

    let casts = ResizeArray<Cast>()

    let castAt (ci: int) (pattern: Pattern) (body: BodyPart list) (floor: bool) =
        let spawn = idle.[ci].[0]
        idle.[ci].RemoveAt 0
        bank.[ci] <- bank.[ci] - Engine.bodyCost body

        casts.Add
            {
                Colony = colonies.[ci].Room
                Spawn = spawn.Id
                Body = body
                Name = castName pattern derived.Time spawn.Name
                Floor = floor
            }

    // The anchor floor: with no harvester, nothing digs what refills the bank.
    colonies
    |> Array.iteri (fun ci colony ->
        let ground = derived.Grounds.[ci]

        let digging =
            Array.append derived.Bodies derived.Ovens
            |> Array.exists (fun b -> b.Pattern.IsHarvester && b.Pos.Room = colony.Room)

        match ground.Sources |> Array.tryFind (fun s -> Option.isSome s.Post) with
        | Some s when idle.[ci].Count > 0 && not digging && colony.Bank < colony.BankCapacity ->
            match homeHarvester (digWork s.Rate) bank.[ci] with
            | Some body when Engine.bodyCost body <= bank.[ci] ->
                castAt ci Pattern.Harvester body true
            | _ -> ()
        | _ -> ())

    // The supply floor: with nobody to refill it, the bank never fills again.
    let roomOf = System.Collections.Generic.Dictionary<string, string>()

    for offer in pool do
        roomOf.[offer.Id] <- offer.Target.Room

    let serves (room: string) (body: Body) =
        (body.Pattern.IsWorker || body.Pattern.IsHauler)
        && (body.Pos.Room = room
            || (match Map.tryFind body.Name assignments with
                | Some id ->
                    match roomOf.TryGetValue id with
                    | true, r -> r = room
                    | _ -> false
                | None -> false))

    colonies
    |> Array.iteri (fun ci colony ->
        if idle.[ci].Count > 0 && not (derived.Bodies |> Array.exists (serves colony.Room)) then
            match sized Pattern.Worker Engine.maxBodyParts bank.[ci] with
            | Some body -> castAt ci Pattern.Worker body true
            | None -> ())

    let richest =
        colonies |> Array.sortByDescending (fun c -> c.BankCapacity) |> Array.tryHead

    let tickWorth ci =
        float derived.Grounds.[ci].Income / float colonies.[ci].Spawns.Length

    let seats = ResizeArray<Seat>()

    for d in all do
        let caster =
            nearest
                distance
                colonies
                (fun ci ->
                    idle.[ci].Count > 0
                    && Engine.bodyCost (blockOf d.Pattern) <= colonies.[ci].BankCapacity)
                d.At

        let body =
            caster
            |> Option.map (fun ci -> colonies.[ci])
            |> Option.orElse richest
            |> Option.bind (fun colony -> bodyFor colony d)

        let mutable left = d.Left
        let mutable seatsLeft = d.SeatsLeft
        let mutable go = true

        while go && left > 0 && seatsLeft > 0 do
            match body with
            | Some body when partsOf d.Pattern body > 0 ->
                let ret =
                    d.Value body
                    |> Option.map (fun v ->
                        match caster with
                        | Some ci ->
                            Price.bodyReturn
                                v
                                (Engine.bodyCost body)
                                (Engine.spawnTicksPerPart * List.length body)
                                (tickWorth ci)
                        | None -> 0.0)

                seats.Add
                    {
                        Pattern = d.Pattern
                        At = d.At
                        Parts = left
                        Colony = caster
                        Body = body
                        Return = ret
                    }

                left <- left - partsOf d.Pattern body
                seatsLeft <- seatsLeft - 1
            | _ -> go <- false

    let ordered = seats |> List.ofSeq |> List.sortBy castOrder

    for seat in ordered do
        match seat.Colony with
        | Some ci when idle.[ci].Count > 0 && Engine.bodyCost seat.Body <= bank.[ci] ->
            castAt ci seat.Pattern seat.Body false
        | _ -> ()

    {
        Wants = wants
        Seats = ordered
        Casts = List.ofSeq casts
    }
