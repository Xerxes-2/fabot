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
/// and whether the supply floor cast it rather than a seat.
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

/// A harvest crew's WORK past the source's saturation: the dig finishes the
/// source inside its regeneration window despite the ticks a body loses
/// walking and dropping its load. A named margin.
let harvestSpareWork = 1

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

/// What a haul from a source delivers a tick, of the `allowed` it digs, by
/// the colony that casts it: all of it where the colony banks in a storage;
/// else its share of what the spawn draws, the standing bodies' upkeep, the
/// rest of the dig left for the workers to fetch. A haul feeds only sinks.
let private haulShare (distance: Pos -> Pos -> int option) (derived: Derived) =
    let draw = Array.zeroCreate<float> derived.Colonies.Length

    for b in derived.Bodies do
        match nearest distance derived.Colonies (fun _ -> true) b.Pos with
        | Some ci ->
            let cost =
                b.Work * Engine.partCost Work
                + b.Carry * Engine.partCost Carry
                + b.Move * Engine.partCost Move
                + b.Claim * Engine.partCost Claim

            draw.[ci] <- draw.[ci] + float cost / float (lifeOf b.Pattern)
        | None -> ()

    fun (caster: int option) (allowed: int) ->
        match caster with
        | Some ci when not (derived.Grounds.[ci].Stores |> Array.exists (fun s -> s.Owned)) ->
            let income = max 1 derived.Grounds.[ci].Income
            min allowed (int (ceil (draw.[ci] * float allowed / float income)))
        | _ -> allowed

/// What the ground wants beyond the workers: harvest and haul for each
/// source, a reserver for each declared room's reservable controller, a
/// claimer for each declared claim.
///
/// A source is dug by bodies standing at its seats: its WORK wanted is its
/// output's, its bodies no more than its seats, so where the seats bind the
/// body with the more WORK a seat yields the more and outprices the rest. The
/// dig falls where it is dug (a pile, or the container a body stands on), and
/// a haul moves it.
let private groundDemands (distance: Pos -> Pos -> int option) (leg: Leg) (derived: Derived) =
    let demands = ResizeArray<Demand>()
    let life = float Engine.creepLifetime
    let hauled = haulShare distance derived

    derived.Grounds
    |> Array.iteri (fun gi ground ->
        for s in ground.Sources do
            let rate = s.Rate

            // With no container a body's dig piles under it, and a pile decays
            // while it stands.
            let spill = if Option.isSome s.Container then 0.0 else Price.pileDecay 1

            let dig =
                { seated
                      Pattern.Harvester
                      s.Pos
                      (ceilDiv rate Engine.harvestPerWork + harvestSpareWork)
                      s.Seats
                      (fun body ->
                          Some(
                              (float (min rate (partCountIn body Work * Engine.harvestPerWork))
                               - spill)
                              * life
                          )) with
                    Spill = spill
                }

            demands.Add dig

            // The round trip to the nearest colony's sink, at the pace of the
            // hauler it casts, and the dig its seats allow at the harvester it
            // casts; a haul nobody can price hires nobody.
            let caster = nearest distance derived.Colonies (fun _ -> true) s.Pos

            let trip =
                caster
                |> Option.bind (fun ci ->
                    sized Pattern.Hauler Engine.maxBodyParts derived.Colonies.[ci].BankCapacity
                    |> Option.bind (fun body -> roundTrip leg body s.Pos (sinkOf derived ci) 1))

            let allowed =
                match caster |> Option.bind (fun ci -> bodyFor derived.Colonies.[ci] dig) with
                | Some body ->
                    let perSeat = partCountIn body Work * Engine.harvestPerWork

                    if s.Seats >= ceilDiv rate perSeat then
                        rate
                    else
                        s.Seats * perSeat
                | None -> rate

            match trip with
            | Some trip ->
                demands.Add(
                    demand
                        Pattern.Hauler
                        s.Pos
                        (ceilDiv (hauled caster allowed * trip) Engine.carryPartCapacity)
                        (fun body ->
                            let moved =
                                float (partCountIn body Carry * Engine.carryPartCapacity)
                                / float trip

                            // What the standing harvesters dig there: a
                            // haul moves only what a dig leaves, so a
                            // source's harvesters are bought first.
                            let dug = min rate ((dig.Wanted - dig.Left) * Engine.harvestPerWork)

                            Some(min (float dug) moved * life))
                )
            | None -> ()

        match ground.Controller with
        | Some c when ground.Reservable && gi >= derived.Colonies.Length ->
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

    for c in derived.Claims do
        demands.Add(demand Pattern.Claimer c.Pos 1 (fun _ -> None))

    demands

/// Each colony's worker WORK: the surplus its income leaves once every body's
/// upkeep and spill is paid, the workers' own upkeep included, spent upgrading
/// at the pace a worker's loads allow; no more bodies than its controller
/// seats. A colony's own room counts to it, a declared room and a demand to
/// the nearest colony. Never under one: a colony with no worker builds and
/// upgrades nothing.
let private workerDemands
    (distance: Pos -> Pos -> int option)
    (leg: Leg)
    (derived: Derived)
    (others: ResizeArray<Demand>)
    =
    let colonies = derived.Colonies
    let n = colonies.Length
    let income = Array.zeroCreate n
    let upkeep = Array.zeroCreate<float> n

    derived.Grounds
    |> Array.iteri (fun gi ground ->
        let owner =
            if gi < n then
                Some gi
            else
                ground.Controller
                |> Option.map (fun c -> c.Pos)
                |> Option.orElse (ground.Sources |> Array.tryHead |> Option.map (fun s -> s.Pos))
                |> Option.bind (nearest distance colonies (fun _ -> true))

        match owner with
        | Some ci -> income.[ci] <- income.[ci] + ground.Income
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
        let surplus = float income.[ci] - upkeep.[ci] |> max 0.0

        // A worker drains the controller while its load lasts, then walks to
        // its energy and back: the storage where one stands, else the
        // sources' digs, each as often as it yields.
        let ground = derived.Grounds.[ci]
        let at = colony.Controller.Pos
        let range = Task.rangeOf (Task.Upgrade colony.Controller.Id)

        let fetches =
            match ground.Stores |> Array.tryFind (fun s -> s.Owned) with
            | Some storage -> [| storage.Pos, 1 |]
            | None -> ground.Sources |> Array.map (fun s -> s.Pos, s.Rate)

        // What a WORK of the unit worker (the body each WORK past the
        // standing ones is cast as) puts into the controller a tick over its
        // loads and their round trips; the WORK the surplus keeps, each
        // draining and paying its share of its body.
        let drain, spend =
            match sized Pattern.Worker 1 colony.BankCapacity with
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
                    0.0, 0
                else
                    let drain =
                        float Engine.upgradeDrainPerWork * upgrading
                        / (upgrading + float weighted / float weights)

                    let upkeep =
                        float (Engine.bodyCost body) / float (work * lifeOf Pattern.Worker)

                    drain, int (surplus / (drain + upkeep))
            | _ -> 0.0, 0

        seated Pattern.Worker at (max 1 spend) (max 1 ground.UpgradeSeats) (fun body ->
            Some(float (partsOf Pattern.Worker body) * drain * float Engine.creepLifetime)))

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
/// - A colony no worker or hauler standing in it or assigned to it can
///   refill casts a worker from what is banked first, or its bank never
///   fills again (the supply floor).
/// - Each seat goes to its nearest colony with an idle spawn whose bank holds
///   the pattern's block, sized to that bank. Seats with no price are cast
///   first, then by what that colony's body returns on its cost, dearest
///   first.
/// - A colony waits for its bank to pay its dearest seat rather than spend
///   it on a cheaper one.
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

    let all =
        Array.append (others.ToArray()) (workerDemands distance leg derived others)

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

    let ordered =
        let all = List.ofSeq seats

        (all |> List.filter (fun s -> Option.isNone s.Return))
        @ (all
           |> List.filter (fun s -> Option.isSome s.Return)
           |> List.sortBy (fun s -> -(Option.defaultValue 0.0 s.Return)))

    let waiting = Array.create colonies.Length false

    for seat in ordered do
        match seat.Colony with
        | Some ci when not waiting.[ci] && idle.[ci].Count > 0 ->
            if Engine.bodyCost seat.Body <= bank.[ci] then
                castAt ci seat.Pattern seat.Body false
            else
                waiting.[ci] <- true
        | _ -> ()

    {
        Wants = wants
        Seats = ordered
        Casts = List.ofSeq casts
    }
