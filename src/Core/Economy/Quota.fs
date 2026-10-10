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

/// Whether a body is out of the count: its life is at or under the spawn time
/// of its successor, which is cast while it still works.
let expiring (body: Body) : bool =
    body.TicksToLive <= body.Size * Engine.spawnTicksPerPart

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

/// One demand: a pattern's key parts wanted at a place, and what a body of
/// it yields there over its life.
[<NoEquality; NoComparison>]
type private Demand =
    {
        Pattern: Pattern
        At: Pos
        Wanted: int
        Value: BodyPart list -> float option
        mutable Left: int
    }

let private demand pattern at wanted value =
    {
        Pattern = pattern
        At = at
        Wanted = max 0 wanted
        Value = value
        Left = max 0 wanted
    }

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

/// What the ground wants beyond the workers: harvest and haul for each
/// source a container stands at, a reserver for each declared room's
/// reservable controller, a claimer for each declared claim.
let private groundDemands (distance: Pos -> Pos -> int option) (derived: Derived) =
    let demands = ResizeArray<Demand>()
    let life = float Engine.creepLifetime

    derived.Grounds
    |> Array.iteri (fun gi ground ->
        for s in ground.Sources do
            // A source with no container is dug by the workers who carry it;
            // a hauler only moves what a dig leaves in a store.
            if Option.isSome s.Container then
                let rate = s.Rate

                let dig =
                    demand
                        Pattern.Harvester
                        s.Pos
                        (ceilDiv rate Engine.harvestPerWork + harvestSpareWork)
                        (fun body ->
                            Some(
                                float (min rate (partCountIn body Work * Engine.harvestPerWork))
                                * life
                            ))

                demands.Add dig

                // The round trip to the nearest colony's sink; a haul nobody
                // can price hires nobody.
                let trip =
                    derived.Colonies
                    |> Array.mapi (fun ci _ -> distance s.Pos (sinkOf derived ci))
                    |> Array.choose id
                    |> Array.sort
                    |> Array.tryHead
                    |> Option.map (fun t -> 2 * max 1 t)

                match trip with
                | Some trip ->
                    demands.Add(
                        demand
                            Pattern.Hauler
                            s.Pos
                            (ceilDiv (rate * trip) Engine.carryPartCapacity)
                            (fun body ->
                                let moved =
                                    float (partCountIn body Carry * Engine.carryPartCapacity)
                                    / float trip

                                // What the standing harvesters dig there: a
                                // haul moves only what a dig leaves, so a
                                // source's harvesters are bought first.
                                let dug =
                                    min rate ((dig.Wanted - dig.Left) * Engine.harvestPerWork)

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

/// Each colony's worker WORK: the sources without a container that its
/// workers dig, and the surplus its income leaves once the other demands'
/// bodies are paid for, spent upgrading. A colony's own room counts to it,
/// a declared room and a demand to the nearest colony. Never under one: a
/// colony with no worker builds and upgrades nothing.
let private workerDemands
    (distance: Pos -> Pos -> int option)
    (derived: Derived)
    (others: ResizeArray<Demand>)
    =
    let colonies = derived.Colonies
    let n = colonies.Length
    let dig = Array.zeroCreate n
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
        | Some ci ->
            income.[ci] <- income.[ci] + ground.Income

            for s in ground.Sources do
                if Option.isNone s.Container then
                    dig.[ci] <- dig.[ci] + ceilDiv s.Rate Engine.harvestPerWork
        | None -> ())

    for d in others do
        match nearest distance colonies (fun _ -> true) d.At with
        | Some ci ->
            match sized d.Pattern d.Wanted colonies.[ci].BankCapacity with
            | Some body when partsOf d.Pattern body > 0 ->
                let bodies = ceilDiv d.Wanted (partsOf d.Pattern body)

                upkeep.[ci] <-
                    upkeep.[ci] + float (bodies * Engine.bodyCost body) / float (lifeOf d.Pattern)
            | _ -> ()
        | None -> ()

    colonies
    |> Array.mapi (fun ci colony ->
        let surplus = float income.[ci] - upkeep.[ci] |> max 0.0
        let spend = int (surplus / float Engine.upgradeDrainPerWork)

        demand Pattern.Worker colony.Controller.Pos (max 1 (dig.[ci] + spend)) (fun body ->
            Some(float (Engine.bodyCost body))))

/// The tick's quota. `assignments` is last tick's match, `pool` this tick's
/// offers: together they say which colony a body serves.
///
/// - Demand is counted once; each standing body and each body in an oven
///   counts toward the nearest demand of its pattern still short.
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
    (derived: Derived)
    (pool: Offer[])
    (assignments: Map<string, string>)
    : Plan =
    let colonies = derived.Colonies
    let others = groundDemands distance derived
    let all = Array.append (others.ToArray()) (workerDemands distance derived others)
    let standing = System.Collections.Generic.Dictionary<Pattern, int>()

    for body in Array.append derived.Bodies derived.Ovens do
        if not (expiring body) then
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

                if d.Pattern = pattern && d.Left > 0 then
                    let ticks =
                        distance body.Pos d.At |> Option.defaultValue (System.Int32.MaxValue - 1)

                    if ticks < bestTicks then
                        best <- k
                        bestTicks <- ticks

            if best >= 0 then
                all.[best].Left <- max 0 (all.[best].Left - parts)

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
    let richest = colonies |> Array.fold (fun m c -> max m c.BankCapacity) 0

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

        let capacity =
            match caster with
            | Some ci -> colonies.[ci].BankCapacity
            | None -> richest

        let mutable left = d.Left
        let mutable go = true

        while go && left > 0 do
            match sized d.Pattern left capacity with
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
