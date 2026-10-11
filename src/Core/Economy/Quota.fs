/// How many bodies of each pattern the world wants, and which colony casts the
/// next one. Answers "how many" and "who casts", never "who does what": that
/// is the matcher's. Demand is counted in each pattern's key part (`Bodies`):
/// harvest WORK to cover a source, haul CARRY to move it, CLAIM to hold a
/// controller, and worker WORK to spend what the rest leaves.
module Fabot.Core.Economy.Quota

open Fabot.Core
open Fabot.Core.Vocabulary
open Fabot.Core.World
open Fabot.Core.Economy.Bodies
open Fabot.Core.Economy.Derived
open Fabot.Core.Economy.Pool

/// One body the world wants cast: its pattern, where its work is, the key
/// parts it is sized to, the colony that casts it (the nearest with an idle
/// spawn whose bank holds the pattern's block; `None`: no colony can), the
/// body that colony makes, and what the body returns on its cost (`None`: no
/// energy price, cast ahead of every priced seat).
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

/// A pattern's world totals, in its key part: what the work wants and what
/// stands (or bakes) to do it.
type Want =
    {
        Pattern: Pattern
        Wanted: int
        Standing: int
    }

/// A body to spawn: the colony and spawn that cast it, the parts, the name,
/// and whether a floor (the anchor's or the supply's) cast it rather than a
/// seat.
[<NoEquality; NoComparison>]
type Cast =
    {
        Colony: string
        Spawn: string
        Body: BodyPart list
        Name: string
        Floor: bool
    }

/// The tick's quota, decided once: the totals, the seats in casting order,
/// the casts.
[<NoEquality; NoComparison>]
type Plan =
    {
        Wants: Want list
        Seats: Seat list
        Casts: Cast list
    }

/// The ticks a body's successor needs before it can take over: its spawn
/// time and `walk`, the ticks from the casting spawn to the body's work.
let lead (body: Body) (walk: int) : int =
    body.Size * Engine.spawnTicksPerPart + max 0 walk

/// Whether a body is out of the count: its life is at or under its
/// successor's lead, so the successor is cast while it still works and
/// arrives as it dies.
let expiring (body: Body) (walk: int) : bool = body.TicksToLive <= lead body walk

let private ceilDiv (n: int) (d: int) = (n + d - 1) / d

/// The CLAIM parts that bring a reservation holding `held` ticks to the cap
/// inside one claimer's life: N parts add `N × reservePerClaim` a tick while
/// the reservation decays one, so `(N × reservePerClaim − 1) × claimLifetime
/// ≥ cap − held`. Never under two: one part only holds the reservation still.
let claimPartsFor (held: int) : int =
    let perTick =
        ceilDiv (max 0 (Engine.reservationCap - held)) Engine.claimLifetime + 1

    max 2 (ceilDiv perTick Engine.reservePerClaim)

/// A pattern's body life, in ticks.
let private lifeOf (pattern: Pattern) =
    if pattern = Pattern.Claimer then
        Engine.claimLifetime
    else
        Engine.creepLifetime

let private partsOf (pattern: Pattern) (body: BodyPart list) = partCountIn body (keyPartOf pattern)

/// One demand: a pattern's key parts wanted at a place, the most bodies
/// the place seats, and what a body of it yields there over its life.
[<NoEquality; NoComparison>]
type private Demand =
    {
        Pattern: Pattern
        At: Pos
        Wanted: int
        Seats: int
        /// Energy a tick each body's work loses on the floor.
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

/// The body a colony casts for a demand: the most its bank buys, the key
/// parts held to what the demand wants. The one place a body is chosen.
let private bodyFor (colony: Colony) (d: Demand) : BodyPart list option =
    if d.Pattern = Pattern.Harvester && d.At.Room = colony.Room then
        homeHarvester d.Wanted colony.BankCapacity
    else
        sized d.Pattern d.Wanted colony.BankCapacity

/// Where a colony's bodies leave from: its first spawn.
let private homeOf (colony: Colony) = colony.Spawns.[0].Pos

/// Where a colony banks: its storage, or its first spawn.
let private sinkOf (derived: Derived) (ci: int) =
    derived.Grounds.[ci].Stores
    |> Array.tryFind (fun s -> s.Owned)
    |> Option.map (fun s -> s.Pos)
    |> Option.defaultValue (homeOf derived.Colonies.[ci])

/// The index of the nearest colony allowed by `can`, by `distance` from its
/// home; ties to the first.
let private nearest
    (distance: Pos -> Pos -> int option)
    (colonies: Colony[])
    (can: int -> bool)
    (at: Pos)
    =
    let mutable best = -1
    let mutable bestTicks = System.Int32.MaxValue

    for ci in 0 .. colonies.Length - 1 do
        if can ci then
            match distance (homeOf colonies.[ci]) at with
            | Some ticks when ticks < bestTicks ->
                best <- ci
                bestTicks <- ticks
            | _ -> ()

    if best < 0 then None else Some best

/// The ticks a body of the parts walks loaded from a tile to within a range
/// of another: the Atlas's answer, `None` where it does not get there.
type Leg = BodyPart list -> Pos -> Pos -> int -> int option

/// The ticks of a load's round trip between `fetch` and within `range` of
/// `work`: the walk there loaded, twice. The walk back empty is no slower, and
/// one flood toward `work` (the one the bodies delivering there read) prices
/// both. Never under two.
let private roundTrip (leg: Leg) (body: BodyPart list) (fetch: Pos) (work: Pos) (range: int) =
    leg body fetch work range |> Option.map (fun there -> max 2 (2 * there))

/// The energy a colony keeps in its storage before it spends the surplus:
/// every tower its level allows firing for `lifetimes` creep lifetimes, the
/// owner's number (`ARCHITECTURE.md` §5). Nothing is kept without a storage.
let reserveTarget (lifetimes: float) (level: int) : int =
    float (
        Engine.controllerStructures "tower" level
        * Engine.towerEnergyCost
        * Engine.creepLifetime
    )
    * lifetimes
    |> int

/// The colony a ground's work counts to: its own, else the nearest to its
/// controller or first source.
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

/// The energy a tick one source's haul must move: what its post digs, and
/// what stands in its container and beside it over a creep's life, so a
/// hauler cast now clears the backlog before it dies. Zero until its
/// container is built: a dig that falls on the floor is the workers' to fetch.
let private haulFlow (ground: Ground) (s: SourceFact) : float =
    match s.Container with
    | None -> 0.0
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

        float s.Dug + float (stored + piled) / float Engine.creepLifetime

/// What the ground wants beyond the spenders: a harvester on each source's
/// post, the haul from each built container, a reserver for each declared
/// room's reservable controller, a claimer for each declared claim. A source
/// with no post is dug by the workers' seat crews (`spendDemands`).
///
/// The haul is one demand a colony: its containers' flows (`haulFlow`), each
/// times its round trip to the dearest sink it feeds (the spawn, the buffer,
/// the storage), over a CARRY's load, rounded once. The dearest, not the
/// nearest: a ring fills in a trip, and the flow that goes on all day is to
/// the far sink.
let private groundDemands (distance: Pos -> Pos -> int option) (leg: Leg) (derived: Derived) =
    let demands = ResizeArray<Demand>()
    let life = float Engine.creepLifetime
    let colonies = derived.Colonies
    let carries = Array.zeroCreate<float> colonies.Length
    let flows = Array.zeroCreate<float> colonies.Length
    let digs = Array.init colonies.Length (fun _ -> ResizeArray<int * Demand>())

    let sinksOf ci =
        homeOf colonies.[ci]
        :: (derived.Grounds.[ci].Stores
            |> Array.filter (fun st -> st.Kind = StoreKind.Buffer || st.Owned)
            |> Array.map (fun st -> st.Pos)
            |> List.ofArray)

    derived.Grounds
    |> Array.iteri (fun gi ground ->
        let owner = ownerOf distance derived gi

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
                    let trip =
                        sized Pattern.Hauler Engine.maxBodyParts colonies.[ci].BankCapacity
                        |> Option.map (fun body ->
                            sinksOf ci
                            |> List.choose (fun sink -> roundTrip leg body post sink 1)
                            |> List.fold max 0)
                        |> Option.defaultValue 0

                    if trip > 0 then
                        let flow = haulFlow ground s
                        carries.[ci] <- carries.[ci] + flow * float trip
                        flows.[ci] <- flows.[ci] + flow
                        digs.[ci].Add(s.Rate, dig)
                | _ -> ()
            | None -> ()

        match ground.Controller with
        | Some c when ground.Reservable && gi >= colonies.Length ->
            let gain =
                float (
                    ground.Sources.Length
                    * (Engine.heldOutputPerTick - Engine.neutralOutputPerTick)
                    * Engine.claimLifetime
                )

            demands.Add(
                demand Pattern.Claimer c.Pos (claimPartsFor ground.OurReservation) (fun _ ->
                    Some gain)
            )
        | _ -> ())

    for ci in 0 .. colonies.Length - 1 do
        if carries.[ci] > 0.0 then
            let wanted = int (ceil (carries.[ci] / float Engine.carryPartCapacity))
            let flow = flows.[ci]
            let digs = digs.[ci]

            demands.Add(
                demand Pattern.Hauler (homeOf colonies.[ci]) wanted (fun body ->
                    let moved = float (partCountIn body Carry) * flow / float wanted

                    // What the standing harvesters dig: a haul moves only
                    // what a dig leaves, so the harvesters are bought first.
                    let dug =
                        digs
                        |> Seq.sumBy (fun (rate, dig) ->
                            min rate ((dig.Wanted - dig.Left) * Engine.harvestPerWork))

                    Some(min (float dug) moved * life))
            )

    for c in derived.Claims do
        demands.Add(demand Pattern.Claimer c.Pos 1 (fun _ -> None))

    demands

/// What a colony spends its surplus on: the upgraders standing at its buffer
/// and the workers that build, repair and upgrade.
///
/// - The surplus a tick: what the posts of built containers dig, less every
///   other body's upkeep.
/// - With a storage, the reserve (`reserveTarget`) comes first: the gap
///   saved over a creep's life, never more than the surplus.
/// - The energy the standing sites still owe, over a creep's life, is the
///   workers'. The upgraders are hired out of what is left, and out of the
///   stock above the reserve, in whole bodies of a lifetime's drain and body
///   (rounded down), only while a buffer stands and the bank casts a
///   standing body.
/// - The workers spend the rest at the pace their loads allow (rounded up),
///   never under two bodies while anything is to be built or repaired, else
///   one; and a crew on each seat of a home source with no post.
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

        // The upgrader the bank casts, where a buffer stands to seat it and
        // the cast is a standing body.
        let upgrader =
            sized Pattern.Upgrader Engine.maxBodyParts colony.BankCapacity
            |> Option.filter (fun body ->
                Option.isSome ground.Buffer
                && standing (partCountIn body Work) (partCountIn body Carry))

        // No more upgraders than the buffer seats: one past them draws nothing.
        let upgraders, upgraderWork, upgradersDrain =
            match upgrader with
            | Some body ->
                let work = partCountIn body Work
                let seats = float ground.BufferSeats.Length

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
        // its energy and back: the buffer where one stands, else the storage,
        // else the sources' digs (a post's container, else the rock), each
        // as often as it yields.
        let range = Task.rangeOf (Task.Upgrade colony.Controller.Id)

        let fetches =
            match ground.Buffer, storage with
            | Some buffer, _ -> [| buffer.Pos, 1 |]
            | None, Some storage -> [| storage.Pos, 1 |]
            | None, None ->
                ground.Sources |> Array.map (fun s -> Option.defaultValue s.Pos s.Post, s.Dug)

        let cast = sized Pattern.Worker Engine.maxBodyParts colony.BankCapacity

        // The WORK on the body the bank casts the rest keeps: each puts into
        // the controller a tick what its loads and their round trips allow,
        // and pays its share of its body.
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

                // No energy a worker reaches: nothing to spend.
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

        // Not knowing a source's seats is no crew.
        let crews =
            ground.Sources
            |> Array.sumBy (fun s ->
                if Option.isNone s.Post && s.Seats < System.Int32.MaxValue then
                    s.Seats
                else
                    0)

        [|
            seated
                Pattern.Worker
                at
                (max spend (floorBodies * bodyWork) + crews * bodyWork)
                (max floorBodies ground.UpgradeSeats + crews)
                (fun body -> Some(float (partsOf Pattern.Worker body) * life))
            seated Pattern.Upgrader at (upgraders * upgraderWork) upgraders (fun body ->
                Some(float (partsOf Pattern.Upgrader body) * life))
        |])
    |> Array.concat

/// Where a seat is cast in the order, first lowest: the seats with no
/// price, then the ground's by what the body returns on its cost, then the
/// workers the same way, and the upgraders last: they spend what every other
/// seat leaves. Stable, so a tie keeps the demands' order.
let private castOrder (seat: Seat) : int * float =
    match seat.Return, seat.Pattern with
    | None, _ -> 0, 0.0
    | Some _, Pattern.Upgrader -> 3, 0.0
    | Some r, Pattern.Worker -> 2, -r
    | Some r, _ -> 1, -r

/// The tick's quota. `assignments` is last tick's match, `pool` this tick's
/// offers: together they say which colony a body serves. `walk fresh body
/// from` is the ticks from `from` to the body's work at the pace of the body
/// fresh from a spawn (`fresh`), or as it walks now, which is never faster
/// (`None`: no work to walk to, and the tiles to where it stands are walked).
///
/// - Demand is counted once; each standing body and each body in an oven
///   counts toward the nearest demand of its pattern still short, taking one
///   of its seats, unless it is expiring: inside the lead of a successor cast
///   at the nearest colony.
/// - A colony with a post, no harvester standing or baking in its room and
///   a bank short of full casts a harvester from what is banked first, or
///   nothing digs what refills it (the anchor floor).
/// - A colony no worker or hauler standing in it or assigned to it can
///   refill casts a worker from what is banked, or its bank never fills
///   again (the supply floor).
/// - Each seat goes to its nearest colony with an idle spawn whose bank holds
///   the pattern's block, sized to that bank, cast in `castOrder`. A seat
///   its bank cannot pay yields to the next it can: an order, never a veto.
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

    let standing = System.Collections.Generic.Dictionary<Pattern, int>()

    // Whether a body is inside its successor's lead. The walk at the body's
    // own pace bounds the fresh one (an empty body is never slower) and reads
    // the flood its own walk does; the fresh walk is asked only inside it.
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
            let pattern = body.Pattern
            let parts = keyPartsIn pattern body

            standing.[pattern] <-
                (match standing.TryGetValue pattern with
                 | true, n -> n
                 | _ -> 0)
                + parts

            let mutable best = -1
            let mutable bestTicks = System.Int32.MaxValue

            for k in 0 .. all.Length - 1 do
                let d = all.[k]

                if d.Pattern = pattern && d.Left > 0 && d.SeatsLeft > 0 then
                    let ticks =
                        distance body.Pos d.At |> Option.defaultValue (System.Int32.MaxValue - 1)

                    if ticks < bestTicks then
                        best <- k
                        bestTicks <- ticks

            if best >= 0 then
                all.[best].Left <- max 0 (all.[best].Left - parts)
                all.[best].SeatsLeft <- max 0 (all.[best].SeatsLeft - 1)

    let wants =
        allPatterns
        |> List.map (fun pattern ->
            {
                Pattern = pattern
                Wanted = all |> Array.sumBy (fun d -> if d.Pattern = pattern then d.Wanted else 0)
                Standing =
                    match standing.TryGetValue pattern with
                    | true, n -> n
                    | _ -> 0
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

    // The anchor floor.
    colonies
    |> Array.iteri (fun ci colony ->
        let ground = derived.Grounds.[ci]

        let digging =
            Array.append derived.Bodies derived.Ovens
            |> Array.exists (fun b -> b.Pattern = Pattern.Harvester && b.Pos.Room = colony.Room)

        match ground.Sources |> Array.tryFind (fun s -> Option.isSome s.Post) with
        | Some s when idle.[ci].Count > 0 && not digging && colony.Bank < colony.BankCapacity ->
            match homeHarvester (digWork s.Rate) bank.[ci] with
            | Some body when Engine.bodyCost body <= bank.[ci] ->
                castAt ci Pattern.Harvester body true
            | _ -> ()
        | _ -> ())

    // The supply floor.
    let roomOf = System.Collections.Generic.Dictionary<string, string>()

    for offer in pool do
        roomOf.[offer.Id] <- offer.Target.Room

    let serves (room: string) (body: Body) =
        (body.Pattern = Pattern.Worker || body.Pattern = Pattern.Hauler)
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

    // The seats, each sized and priced at the colony that casts it.
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
