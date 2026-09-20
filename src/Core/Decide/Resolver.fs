/// The Resolver: arbitrated movement (ADR-0001). Prices each mover's step,
/// matches steps to tiles so two bodies never claim one, and returns the move
/// Intents with the Verdicts explaining them.
[<AutoOpen>]
module Fabot.Core.Decide.Resolver

open Fabot.Core
open Fabot.Core.Types

/// Creeps with no Task rank below every task in arbitration.
let private idleRank = System.Int32.MaxValue

/// Register one creep's Move Intent — every creep gets one: a traveller heads
/// its next step and tails a sidestep, an arrived body heads its own tile and
/// tails its area's neighbours then the ground outside, an idle body heads the
/// first step off the idle ground and tails its own tile, and a body with a
/// Task it cannot reach is parked on the Task's rank. The order is the whole
/// of the preference. One tile is never a candidate: the creep's own when it
/// is a Seam (#142), since a creep ending its tick on the ring is moved out of
/// the room again. The Task goes to `stepToward` beside the area, which is
/// what gives a creep matched across a border somewhere to walk. A body
/// crossing for a room it cannot see keeps crossing (#151): the vision grace
/// hands the mover the room its target was last seen in and nothing else,
/// which is enough for `Atlas.stepTowardRoom`, and it is its own arrival that
/// ends the darkness.
let private moveIntentFor
    (rankOf: Task -> int)
    (idleGround: string -> Set<Pos> * Set<RoomPos>)
    (threats: Threats)
    atlas
    (creep: string)
    (at: RoomPos)
    (task: Task option)
    (crossing: string option)
    : MoveIntent =
    // The room the creep stands in and the only room its candidates are tiles
    // of (#145).
    let room = at.Room
    let pos = RoomPos.pos at
    let here = RoomPos.at room
    let beside = Atlas.adjacentWalkableIn atlas room pos
    let onSeam = Atlas.standsOnSeam atlas creep && not (List.isEmpty beside)

    // Where this creep may stay: its own tile, unless that tile is a Seam
    // with ground beside it to walk onto.
    let staying = if onSeam then [] else [ pos ]

    // Every branch below decides three things and no more: the rank it pushes
    // at, the tiles it will accept in the order it wants them tried, and the
    // Work Area the arbitration charges a push out of. The rest is stated here
    // once, so a branch cannot forget the room stamp.
    let intent rank (candidates: Pos list) area =
        {
            Creep = creep
            Pos = at
            Rank = rank
            Candidates = candidates |> List.map here
            Area = area
        }

    // A parked body has a Task it cannot reach, so it is standing where its
    // Work Area is not.
    let parked rank =
        intent rank (staying @ beside) Set.empty

    // The detours behind a step: the ground beside this creep that also lies
    // beside the step it asked for — a way *around* the tile it wanted and
    // never a way back down the lane it came up. Without the tail a creep
    // whose one candidate is held by a body that cannot move stands still for
    // as long as that body does; with the whole neighbourhood in it, a
    // traveller queued behind a merely fatigued creep would back away and
    // return every other tick.
    let detour step =
        if onSeam then
            beside |> List.filter ((<>) step)
        else
            let around = Atlas.adjacentWalkableIn atlas room step |> Set.ofList
            beside |> List.filter (fun tile -> Set.contains tile around)

    // A body that has not arrived: the step it asked for, the ways around it,
    // and no Work Area at all — it is standing outside the one it is walking
    // to, so nothing it is pushed off is work.
    let travelling rank step =
        intent rank (step :: detour step) Set.empty

    // The graced holder's crossing (#151), asked before the Task branches
    // because it is the one body with neither. No Seam to that room — it is
    // not next door — and it falls through to the idle rule below. Which chain
    // it crosses on is the compass's and not the price's, so where the room is
    // reachable round either of two corners the grace can turn a creep at the
    // border and the returning vision turn it back (#288, #297).
    let crossingStep =
        match task, crossing with
        | None, Some room -> Atlas.stepTowardRoom atlas creep room |> Option.map RoomPos.pos
        | _ -> None

    match crossingStep, task with
    | Some step, _ ->
        // Walking toward a room it cannot even see, so it pushes at the idle
        // rank.
        travelling idleRank step
    | None, None ->
        // The room's idle ground and the ground just off it: any way off runs
        // through one of those tiles, so the goal set stays the perimeter
        // rather than the whole room.
        let ground, offGround = idleGround room

        // The tail, ordered off the idle ground first: `arbitrate` re-houses a
        // displaced body on the first free tile of its list, so an unordered
        // tail puts a body shoved off the ground straight back onto it, and
        // two idle bodies trade the one tile off the ground every tick.
        let off, on = beside |> List.partition (fun tile -> not (Set.contains tile ground))

        let tail = staying @ off @ on

        // The way off, less this tick's Reach — the subtraction `areaFor`
        // makes below, made here too because a work-heavy body has no Flee,
        // so a step out of a safe pocket into an attacker is one nothing walks
        // it back from. Nowhere safe off the ground is nowhere to go: it parks.
        let stepOff =
            if Set.contains pos ground then
                let reach = Threats.reachIn threats room

                offGround
                |> Set.filter (fun tile -> not (Set.contains (RoomPos.pos tile) reach))
                |> Atlas.firstStepWithin atlas creep
                |> Option.map RoomPos.pos
            else
                None

        // A body with no Task is working from nowhere: shoving it off costs
        // the chain nothing.
        intent
            idleRank
            (match stepOff with
             | Some step -> step :: (tail |> List.filter ((<>) step))
             | None -> tail)
            Set.empty
    | None, Some task ->
        // The area less this tick's Reach: a creep works from the safe half of
        // its Work Area rather than abandoning the Task because one corner is
        // hot. Read against the set the Atlas already holds rather than a
        // narrowed copy of it.
        let area = areaFor threats atlas creep task

        if Set.contains (here pos) area then
            let inside, outside =
                beside |> List.partition (fun tile -> Set.contains (here tile) area)

            // The one body that has arrived: the tiles it may be shuffled
            // between for nothing, and the border the arbitration charges for
            // pushing it over — the area less this tick's Reach, the same set
            // the candidates were partitioned on.
            intent (rankOf task) (pos :: (inside @ outside)) area
        else
            match stepToward atlas creep task area |> Option.map RoomPos.pos with
            | Some step -> travelling (rankOf task) step
            | None -> parked (rankOf task)

/// The push a rank carries into the arbitration's arithmetic: a *weight*, so a
/// chain seating two bodies on the steps they asked for can outweigh one body
/// pushed off its own. Positive and never rising with rank, and `idleRank`
/// lands on the smallest weight rather than none, or a crowd of idle bodies
/// would be a wall no traveller could walk into. The ladder's rungs are divided
/// back out, rounding to the **nearest** tier (#237): a rung is a claim about
/// which of two Tasks a creep should take, never about how hard it pushes, and
/// a two-rung step rounded past its own tier when the window was one rung
/// wide. A tier owns the ranks from `tierRungs / 2` above it to
/// `tierRungs / 2 - 1` below, a tie going to the deeper tier, so a rung may be
/// half a tier at the most (`Pool.Rung`). Exported for the tests that walk it.
let weightOfRank (rank: int) : int =
    max 1 (ceilDiv (priorityOfTier Stock - rank - tierRungs / 2) tierRungs + 2)

/// The room's matching while the arbitration runs: a tile's holder and a
/// holder's tile, one relation written both ways because the search reads it
/// both ways — the tile to find whom to displace, the creep to find what to
/// vacate. Injective in both directions by construction: a creep's own entry is
/// rewritten by the assignment that moves it, and the chain's initiator has its
/// tile emptied before the search starts.
type private Matching =
    {
        Holder: Map<RoomPos, string>
        Tile: Map<string, RoomPos>
    }

/// Resolver core: the room's Move Intents matched onto its tiles by a weighted
/// augmenting search — sy-harabi's traffic manager, read and rewritten rather
/// than linked (unlicensed, and it issues the engine's moves itself), without
/// its hash-shuffled candidate order (every tie here falls to the lowest x then
/// y), its cost-matrix threshold (a crowd is priced, never impassable) or its
/// free-tiles-first order (incompatible with the head-and-tail list). A room's
/// pass is O(creeps x 8). The intents are offered in a deterministic order:
/// travellers before stayers (a stayer settled first walls off a traveller's
/// only path), then by rank, then by name. Only a strictly positive chain is
/// taken, and dropping the Map the search returned is the whole rollback.
let private arbitrate
    (occupants: Map<RoomPos, string>)
    (blocked: Set<RoomPos>)
    (moveIntents: MoveIntent list)
    : Map<string, RoomPos> =
    let byCreep = moveIntents |> List.map (fun i -> i.Creep, i) |> Map.ofList

    let headOf (intent: MoveIntent) = List.tryHead intent.Candidates

    // A creep whose first candidate is the tile it stands on: it asked to
    // stay, and it is displaceable by anybody.
    let staying (intent: MoveIntent) = headOf intent = Some intent.Pos

    let place creep tile (m: Matching) =
        {
            Holder = Map.add tile creep m.Holder
            Tile = Map.add creep tile m.Tile
        }

    let vacate creep (m: Matching) =
        match Map.tryFind creep m.Tile with
        | Some tile ->
            {
                Holder = Map.remove tile m.Holder
                Tile = Map.remove creep m.Tile
            }
        | None -> m

    // What a creep landing on `tile` is worth to the chain. The eviction
    // charge is levied here, on the landing, because that is where the search
    // knows where the body ended up; `cost` below is charged on the push. It
    // is the weight **and one**: a chain that ends with its initiator stepping
    // aside onto a tail scores exactly 1, so at the bare weight an eviction
    // would be worth the same as walking round as soon as the arriving body is
    // one tier up.
    let gain initiator (intent: MoveIntent) tile =
        if headOf intent = Some tile then
            weightOfRank intent.Rank
        elif not (Set.isEmpty intent.Area) && not (Set.contains tile intent.Area) then
            -(weightOfRank intent.Rank + 1)
        elif initiator then
            1
        else
            0

    // What taking `tile` off its occupant costs the chain: the weight of a
    // step the occupant had asked for and is being denied. A stayer is denied
    // nothing here — it asked for the tile it is standing on, and what its
    // displacement costs depends on where it lands, which `gain` above prices.
    let cost (occupant: MoveIntent) tile =
        if headOf occupant = Some tile && not (staying occupant) then
            weightOfRank occupant.Rank
        else
            0

    // `visited` is threaded through and never rolled back, the one thing the
    // search borrows from its mutable original: a creep the chain has already
    // tried to rehouse is not tried again inside the same outer search, which
    // bounds the work at one expansion per creep per creep.
    let rec augment
        (initiator: bool)
        (visited: Set<string>)
        (score: int)
        (intent: MoveIntent)
        (candidates: RoomPos list)
        (m: Matching)
        : Set<string> * (int * Matching) option =
        let visited = Set.add intent.Creep visited

        let rec walk visited tiles =
            match tiles with
            | [] -> visited, None
            | tile :: rest ->
                if Set.contains tile blocked then
                    walk visited rest
                else
                    let score = score + gain initiator intent tile

                    match Map.tryFind tile m.Holder with
                    | None ->
                        if score > 0 then
                            visited, Some(score, place intent.Creep tile m)
                        else
                            walk visited rest
                    | Some held when Set.contains held visited -> walk visited rest
                    | Some held ->
                        match Map.tryFind held byCreep with
                        | None -> walk visited rest
                        | Some occupant ->
                            // The occupant keeps its tile filed under its name
                            // while its own search runs, and `place` below
                            // overwrites that entry once the chain comes back.
                            let visited, outcome =
                                augment
                                    false
                                    visited
                                    (score - cost occupant tile)
                                    occupant
                                    (occupant.Candidates |> List.filter ((<>) tile))
                                    m

                            match outcome with
                            | Some(total, settled) when total > 0 ->
                                visited, Some(total, place intent.Creep tile settled)
                            | _ -> walk visited rest

        walk visited candidates

    // The identity matching: everybody standing in the room, whether or not
    // it registered an intent, so an occupant with none is a wall the search
    // finds by looking rather than a tile somebody has to remember to block.
    let start =
        {
            Holder = occupants
            Tile =
                occupants
                |> Map.toList
                |> List.map (fun (tile, creep) -> creep, tile)
                |> Map.ofList
        }

    // Whether an intent has nothing left to ask for: it holds the tile it
    // asked for, or it is an arrived body an earlier chain shuffled within its
    // own area. Re-initiating the second would collect its rank's whole weight
    // for being put back on a tile it never chose to leave, and three such
    // phantom gains in one chain buy the very eviction `gain` prices.
    let asked (intent: MoveIntent) (m: Matching) =
        let held = Map.tryFind intent.Creep m.Tile

        held = headOf intent
        || (staying intent
            && (match held with
                | Some tile -> Set.contains tile intent.Area
                | None -> false))

    let settled =
        (start, moveIntents |> List.sortBy (fun i -> staying i, i.Rank, i.Creep))
        ||> List.fold (fun m intent ->
            if asked intent m then
                m
            else
                match
                    augment true Set.empty 0 intent intent.Candidates (vacate intent.Creep m)
                with
                | _, Some(_, settled) -> settled
                | _, None -> m)

    // The creeps that registered an intent and nobody else: what the pass
    // above reads back is each *rested* creep's settled tile, and a fatigued
    // occupant is answered for out of `Blocked` and `Occupants` instead.
    settled.Tile |> Map.filter (fun creep _ -> Map.containsKey creep byCreep)

/// Direction of a single step between adjacent tiles.
let private directionTo (from: Pos) (dest: Pos) : Direction option =
    match sign (dest.X - from.X), sign (dest.Y - from.Y) with
    | 0, -1 -> Some Top
    | 1, -1 -> Some TopRight
    | 1, 0 -> Some Right
    | 1, 1 -> Some BottomRight
    | 0, 1 -> Some Bottom
    | -1, 1 -> Some BottomLeft
    | -1, 0 -> Some Left
    | -1, -1 -> Some TopLeft
    | _ -> None

/// One room's arbitration, settled: what each of its rested creeps was
/// settled on and what it asked for first, and the fatigued creeps' tiles
/// and the occupants the settlement was made against — the four things
/// the Verdicts read back, all keyed on that room's tiles alone (#145).
type private RoomPass =
    {
        /// Each rested creep's settled standing tile.
        Standing: Map<string, RoomPos>
        /// Each rested creep's preferred standing tile: the head of its
        /// candidate list — a Move Intent's candidates are never empty.
        Preferences: Map<string, RoomPos>
        /// The tiles no intent in this pass may be settled onto and no
        /// chain may run through: the fatigued creeps' and the foreign
        /// bodies' that nobody in the fold holds (#220).
        Blocked: Set<RoomPos>
        /// Who stands where at tick start.
        Occupants: Map<RoomPos, string>
    }

/// Resolver, first half: one colony's Move Intents, unarbitrated. Every rested
/// creep the Atlas places registers one; a fatigued creep registers none — the
/// engine would answer its move with ERR_TIRED — and its tile is a wall for the
/// tick (ADR-0008). Takes the tick's assigned Task per creep as data; a creep
/// absent from the map is idle, unless it is in `crossings` — the vision
/// grace's holders and the room each is still walking toward (#151). Rerouted
/// is settled here rather than in the pass, because it is the one movement
/// Verdict the arbitration does not answer: it compares this creep's priced
/// first step against the step the same body would take were no tile occupied,
/// which is a second flood on this colony's Atlas.
let movementOf
    (view: ColonyView)
    atlas
    (threats: Threats)
    (pool: PooledTask list)
    (assigned: Map<string, Task>)
    (crossings: Map<string, string>)
    (verbose: Set<string>)
    : Movement =
    // The push each assigned Task carries into arbitration is its own pooled
    // priority, read off the pool rather than re-derived: the mover and the
    // Matcher order the colony's work by one number or they order it by two.
    let priorities = pool |> List.map (fun p -> taskId p.Task, p.Priority) |> Map.ofList

    let priorityOf task =
        Map.tryFind (taskId task) priorities |> Option.defaultValue idleRank

    let tired =
        view.Creeps
        |> List.choose (fun c -> if c.Fatigue > 0 then Some c.Name else None)
        |> Set.ofList

    let placed = Atlas.placedCreeps atlas

    // The ground an idle body steps off, and the ring of ground just outside
    // it that any step off has to land on — one pair per room some body of
    // ours idles in, and none at all for a tick where every body has a Task,
    // which is the tick that must pay nothing for this rule. `idleGroundIn`
    // and not `workingGroundIn`: the working ground is the Layout's question
    // and this is the mover's, and the two sets part company at the Storage's
    // and the refill cluster's standing tiles (#268).
    let idleGrounds =
        placed
        |> List.filter (fun (name, _) ->
            not (Map.containsKey name assigned)
            // A graced holder is walking a crossing, not idling (#151).
            && not (Map.containsKey name crossings)
            && not (Set.contains name tired))
        |> List.map (fun (_, at) -> at.Room)
        |> List.distinct
        |> List.map (fun room ->
            let ground = Atlas.idleGroundIn atlas room

            let off =
                ground
                |> Set.toList
                |> List.collect (Atlas.adjacentWalkableIn atlas room)
                |> List.filter (fun tile -> not (Set.contains tile ground))
                |> Set.ofList
                |> RoomPos.setAt room

            room, (ground, off))
        |> Map.ofList

    let idleGround room =
        Map.tryFind room idleGrounds |> Option.defaultValue (Set.empty, Set.empty)

    let rerouted name task =
        let area = areaFor threats atlas name task

        match
            Atlas.firstStep atlas name task area,
            Atlas.firstStepIgnoringTraffic atlas name task area
        with
        | Some priced, Some blind -> priced <> blind
        | _ -> false

    {
        Order = view.Creeps |> List.map (fun c -> c.Name)
        Placed = placed
        Tired = tired
        Foreign = view.Foreign
        Intents =
            placed
            |> List.filter (fun (name, _) -> not (Set.contains name tired))
            |> List.map (fun (name, at) ->
                moveIntentFor
                    priorityOf
                    idleGround
                    threats
                    atlas
                    name
                    at
                    (Map.tryFind name assigned)
                    (Map.tryFind name crossings))
        Rerouted =
            placed
            |> List.choose (fun (name, _) ->
                if Set.contains name verbose then
                    match Map.tryFind name assigned with
                    | Some task when rerouted name task -> Some name
                    | _ -> None
                else
                    None)
            |> Set.ofList
    }

/// Resolver, second half: the room passes, and the move Intents and movement
/// Verdicts they settle. One pass per room over **every** creep of ours
/// standing in it, whichever colony registered its intent (#220) — a room two
/// colonies work is one room, and half its traffic arbitrated against the other
/// half read as empty is how a body ends up claiming a tile the engine will
/// never let it into. Once per room, and never across two (#145): geometry
/// crosses the Seam and arbitration does not, so the tiles keying each room's
/// occupants, walls and intents carry the room they are in, or two creeps on
/// one coordinate of two rooms would collapse into one occupant. What is *not*
/// arbitrated is the border tile: two creeps aiming at one exit from its two
/// sides are never checked against each other. What crosses the Seam is the
/// *destination* (#142): a creep matched to an outpost's Task is arbitrated at
/// home over a home tile, and the next tick that room's pass walks it off the
/// ring onto its own floor.
let resolveRooms (movements: Movement list) : Intent list * Verdict list =
    let tired =
        (Set.empty, movements) ||> List.fold (fun acc m -> Set.union acc m.Tired)

    let everywhere = movements |> List.collect (fun m -> m.Placed)

    let passOf =
        everywhere
        |> List.map (fun (_, at) -> at.Room)
        |> List.distinct
        |> List.map (fun room ->
            let here = everywhere |> List.filter (fun (_, at) -> at.Room = room)
            let occupants = here |> List.map (fun (name, at) -> at, name) |> Map.ofList

            // The bodies no intent in this fold can move: the fatigued, and
            // the foreign tiles nobody standing here answered for.
            let foreign =
                (Set.empty, movements)
                ||> List.fold (fun acc m ->
                    Set.union acc (m.Foreign |> Set.filter (fun tile -> tile.Room = room)))
                |> Set.filter (fun tile -> not (Map.containsKey tile occupants))

            let blocked =
                here
                |> List.choose (fun (name, at) ->
                    if Set.contains name tired then Some at else None)
                |> Set.ofList
                |> Set.union foreign

            let moveIntents =
                movements
                |> List.collect (fun m -> m.Intents |> List.filter (fun i -> i.Pos.Room = room))

            room,
            {
                Standing = arbitrate occupants blocked moveIntents
                Preferences =
                    moveIntents
                    |> List.map (fun i -> i.Creep, List.head i.Candidates)
                    |> Map.ofList
                Blocked = blocked
                Occupants = occupants
            })
        |> Map.ofList

    // Every placed creep beside its room's pass, colony by colony and in
    // each colony's own view creep order: the order the Intents and
    // Verdicts leave in.
    let rows =
        movements
        |> List.collect (fun m ->
            let placed = m.Placed |> Map.ofList

            m.Order
            |> List.choose (fun name ->
                Map.tryFind name placed
                |> Option.map (fun at -> m, name, at, Map.find at.Room passOf)))

    let intents =
        rows
        |> List.choose (fun (_, name, at, pass) ->
            Map.tryFind name pass.Standing
            |> Option.bind (fun settled -> directionTo (RoomPos.pos at) (RoomPos.pos settled))
            |> Option.map (fun direction -> MoveCreep(name, direction)))

    // Who holds a tile this creep did not get, in this creep's room: the
    // creep settled on it, or the fatigued occupant whose blocked tile
    // pre-claimed it. A foreign body's tile is blocked and has no occupant
    // here, which is what leaves a creep stalled rather than yielded.
    let counterpartAt (pass: RoomPass) tile self =
        pass.Standing
        |> Map.tryPick (fun name settled ->
            if settled = tile && name <> self then Some name else None)
        |> Option.orElse (
            if Set.contains tile pass.Blocked then
                Map.tryFind tile pass.Occupants
            else
                None
        )

    let verdicts =
        rows
        |> List.collect (fun (movement, name, _, pass) ->
            if Set.contains name tired then
                [ Verdict.Grounded name ]
            else
                let reroute =
                    if Set.contains name movement.Rerouted then
                        [ Verdict.Rerouted name ]
                    else
                        []

                let yielded =
                    match Map.tryFind name pass.Preferences, Map.tryFind name pass.Standing with
                    | Some preferred, Some settled when settled <> preferred ->
                        match counterpartAt pass preferred name with
                        | Some other -> [ Verdict.Yielded(name, other) ]
                        | None -> [ Verdict.Stalled name ]
                    | _ -> []

                reroute @ yielded)

    intents, verdicts

/// Resolver, single colony: this colony's movement, arbitrated against nobody
/// else's.
let resolve
    (view: ColonyView)
    atlas
    (threats: Threats)
    (pool: PooledTask list)
    (assigned: Map<string, Task>)
    (crossings: Map<string, string>)
    (verbose: Set<string>)
    : Intent list * Verdict list =
    resolveRooms [ movementOf view atlas threats pool assigned crossings verbose ]
