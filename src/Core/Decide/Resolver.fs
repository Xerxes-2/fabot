/// The Resolver: arbitrated movement (ADR 0001). Prices each mover's step,
/// matches steps to tiles so two bodies never claim one, and returns the move
/// Intents with the Verdicts explaining them (ADR 0009).
[<AutoOpen>]
module Fabot.Core.Decide.Resolver

open Fabot.Core
open Fabot.Core.Types

/// Creeps with no Task rank below every task in arbitration.
let private idleRank = System.Int32.MaxValue

/// Register one creep's Move Intent — every creep gets one (ADR 0001). A creep
/// travelling toward its Work Area wants exactly its next path step; one
/// already inside is force-registered "stay put, displaceable within the Work
/// Area"; one with no Task, or no way to reach its area, is parked: stay put,
/// displaceable to any adjacent walkable tile. The displacement tiles are its
/// own room's, the Resolver arbitrating each projected room by itself (ADR
/// 0041's Consequences). **Every candidate list is the head it asked for and a
/// tail after it** (#219). A traveller heads its step and tails the ground that
/// lies beside both it and that step — a sidestep around the tile it wanted. A
/// creep inside its area heads its own tile, then the area's neighbours, then
/// the ground outside it. Head and tail are read differently by the arbitration
/// below — the head is what the score pays a creep's whole weight for, a tail
/// is a detour worth the least positive thing — so the order is the whole of
/// the preference. Without a tail a creep whose one candidate is held stands
/// still for as long as that body does; it is a sidestep and never a retreat,
/// which is the half ADR 0008 keeps. One tile is never a candidate: the creep's
/// own, when that tile is a Seam (#142). The ring is no room's ground (ADR
/// 0036) and a creep that ends its tick on it is moved out of the room again,
/// so "stay put" there is a bounce across the border every other tick. The Task
/// goes to `stepToward` beside the area, and that is what gives a creep matched
/// across a border somewhere to walk (#142): its Work Area is empty here by
/// construction (ADR 0041), so without the Task it would park on a Task it was
/// priced for and never move. A [[guard]]'s area is the one that is *not* empty
/// across that border — it is the raided room's ring, filed under that room (ADR
/// 0056) — and it crosses through the same seam all the same, the Task naming
/// the room the step is aimed at.
///
/// **A body with no Task parks off the [[idle ground]]** (`Atlas.idleGroundIn`;
/// #241, widening ADR 0022 from the Layout to the mover, and #268 widening the
/// mover's set past the Layout's). The Seats and the Upgrade Work Area are the
/// tiles the colony works *from*, and an idle body standing on one costs
/// exactly what a clustered structure there would: the tile. W13S28's north
/// pocket is eight tiles of Upgrade Work Area with the [[buffer]] container
/// inside it, so its five refill tiles are working ground to the last one; two
/// idle upgraders parked on them for 190 ticks and the hauler carrying the
/// energy that would have un-idled them never got in — the buffer stayed empty
/// because the bodies waiting on it were standing where its feed had to stand.
/// The [[storage]]'s and the [[refill cluster]]'s standing tiles jam the same
/// way and are outside ADR 0022's set, which is why the mover reads a set of
/// its own: "where standing idle blocks somebody" is a fact about traffic and
/// is strictly wider than "where work happens" (#268).
/// So an idle body's head is the first step off that ground, and its own tile
/// falls into the tail behind it: where there is nowhere off the ground to go,
/// it parks exactly as before. Two things ride on that head. The goal set is
/// taken less this tick's Reach, as every tasked candidate already is (ADR
/// 0033) — a [[work-heavy body]] has no Flee, so a step off a safe pocket into
/// an attacker is one nothing walks back — and the tail is ordered off the
/// ground first, because a body shoved aside off the ground and re-housed onto
/// it is a body that steps off again next tick, which is the very swap #241
/// was opened about. The rule is the idle body's alone — a body with a Task it
/// cannot reach parks on the Task's own rank and keeps its tile, because it is
/// not the ground it is standing on that is stopping it.
///
/// **A body crossing for a room it cannot see keeps crossing** (#151). The
/// vision grace holds a creep's assignment while its target's room is dark, and
/// a held assignment whose Task left the pool with the vision reaches the mover
/// as `crossing`: the room the target was last seen in, and nothing else about
/// it. That is enough to walk — the step is toward the near side of a Seam,
/// which is border layer and memoised terrain (`Atlas.stepTowardRoom`) — and it
/// is the whole reason the grace is worth having: a body that stops walking
/// arrives no sooner than the one that turned round, and it is its own arrival
/// that ends the darkness. It pushes at `idleRank`, because the Task it is
/// walking for is in no pool to be priced against the ones that are.
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
    // The room the creep stands in and the only room its candidates are
    // tiles of (#145): it rides on the tile now (ADR 0052 decision 2)
    // rather than beside it as a field of its own.
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
    // Work Area the arbitration charges a push out of (#267). Which body, which
    // tile, and the room every candidate is stamped with (#145) are the same in
    // all of them, so they are stated here once — a branch cannot forget the
    // stamp.
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
    // never a way back down the lane it came up. Both halves are load-bearing.
    // Without the tail a creep whose one candidate is held by a body that
    // cannot move stands still for as long as that body does; with the whole
    // neighbourhood in it, a traveller queued behind a merely fatigued creep
    // would back away and return every other tick, and ADR 0008's answer —
    // wait in place — is the right one.
    let detour step =
        if onSeam then
            beside |> List.filter ((<>) step)
        else
            let around = Atlas.adjacentWalkableIn atlas room step |> Set.ofList
            beside |> List.filter (fun tile -> Set.contains tile around)

    // A body that has not arrived: the step it asked for, the ways around it,
    // and no Work Area at all — it is standing outside the one it is walking
    // to, so nothing it is pushed off is work (#267). `parked`'s sibling, and
    // the same reason for existing: a rule two branches each have to remember
    // is a rule one of them can forget.
    let travelling rank step =
        intent rank (step :: detour step) Set.empty

    // The graced holder's crossing (#151), asked before the Task branches
    // because it is the one body with neither: its Task left the pool with its
    // target's room's vision, so there is nothing to price and nothing to act
    // on, and the room name is the whole of what the mover was handed. No Seam
    // to that room — it is not next door — and it falls through to the idle
    // rule below, which is what it is until the vision comes back. Which chain
    // it crosses on is the compass's and not the price's, so where the room is
    // reachable round either of two corners the grace can turn a creep at the
    // border and the returning vision turn it back (#288, #297).
    let crossingStep =
        match task, crossing with
        | None, Some room -> Atlas.stepTowardRoom atlas creep room |> Option.map RoomPos.pos
        | _ -> None

    match crossingStep, task with
    | Some step, _ ->
        // Walking, and toward a room it cannot even see, so it pushes at the
        // idle rank: the Task it walks for is in no pool to be priced against
        // the ones that are.
        travelling idleRank step
    | None, None ->
        // The room's [[idle ground]] and the ground just off it: any way off runs
        // through one of those tiles, so the nearest of them is the nearest
        // standing room there is outside the colony's workplaces and the lanes
        // that feed them, and the goal set stays the perimeter rather than the
        // whole room.
        let ground, offGround = idleGround room

        // The tail, ordered off the idle ground first — the same job the
        // other two branches give their own tails. `arbitrate` re-houses a
        // displaced body on the first free tile of its list, so an unordered
        // tail puts a body shoved off the ground straight back onto it, and
        // the two idle bodies trade the one tile off the ground every tick.
        let off, on = beside |> List.partition (fun tile -> not (Set.contains tile ground))

        let tail = staying @ off @ on

        // The way off, less this tick's Reach (ADR 0033) — the subtraction
        // `areaFor` makes below, made here too because this is the branch it
        // is hardest on: ADR 0033 gives a [[work-heavy body]] no Flee, so a
        // step out of a safe pocket into an attacker is one nothing walks it
        // back from. Nowhere safe off the ground is nowhere to go: it parks.
        let stepOff =
            if Set.contains pos ground then
                let reach = Threats.reachIn threats room

                offGround
                |> Set.filter (fun tile -> not (Set.contains (RoomPos.pos tile) reach))
                |> Atlas.firstStepWithin atlas creep
                |> Option.map RoomPos.pos
            else
                None

        // A body with no Task is working from nowhere, which is the whole of
        // #241's rule: the ground it stands on is somebody else's to work from,
        // and shoving it off costs the chain nothing (#267).
        intent
            idleRank
            (match stepOff with
             | Some step -> step :: (tail |> List.filter ((<>) step))
             | None -> tail)
            Set.empty
    | None, Some task ->
        // The area less this tick's Reach (ADR 0033): a creep works from the
        // safe half of its Work Area rather than abandoning the Task because
        // one corner is hot, and its steps go nowhere else. Read against the
        // set the Atlas already holds rather than a narrowed copy of it.
        let area = areaFor threats atlas creep task

        if Set.contains (here pos) area then
            let inside, outside =
                beside |> List.partition (fun tile -> Set.contains (here tile) area)

            // The one body that has arrived: the tiles it may be shuffled
            // between for nothing, and the border the arbitration charges for
            // pushing it over (#267). The area less this tick's Reach, the same
            // set the candidates were partitioned on — a tile the Reach took is
            // not somewhere this body is working from.
            intent (rankOf task) (pos :: (inside @ outside)) area
        else
            match stepToward atlas creep task area |> Option.map RoomPos.pos with
            | Some step -> travelling (rankOf task) step
            | None -> parked (rankOf task)

/// The push a rank carries into the arbitration's arithmetic. `Rank` stays the
/// fold's deterministic sort key below, and this is the second reading the
/// augmenting search needs: a *weight*, so a chain seating two bodies on the
/// steps they asked for can outweigh one body pushed off its own. Positive and
/// never rising with rank, so the ladder's order carries over, and `idleRank`
/// lands on the smallest weight there is rather than none: a body with no Task
/// pushes with something, or a crowd of idle bodies would be a wall no
/// traveller could walk into. The ladder's rungs are divided back out,
/// **rounding to the tier** (ADR 0052 decision 6): a [[priority]] the Planner
/// stepped a rung inside its tier is a claim about which of two Tasks a creep
/// should take and never about how hard it should push through a corridor.
///
/// The rounding is to the **nearest** tier, and it was one rung wide until #237
/// — the ceiling slid by a single `priorityStep`, which was every step that
/// existed when it was written. A rung is not the only step any more: a full
/// source container's Withdraw steps two (#216 R5) and a rescued Repair steps
/// two inside Surplus (#284), and both of those rounded *past* their own tier
/// and pushed a whole weight harder than the tier they belong to — in a
/// corridor, the body holding a full container's Withdraw shoving aside the one
/// holding the spawn's Refill. Sliding by half a tier instead makes the window
/// one whole tier wide and centred on it, so every rung the ladder admits lands
/// on its own tier's weight, and no tier's weight moves — the tiers themselves
/// sit on the grid's multiples either way.
///
/// What the window asks of the ladder in return is `Pool.tierRungs`' own rule,
/// and the arithmetic it is stated against is here: a tier owns the ranks from
/// `tierRungs / 2` above it to `tierRungs / 2 - 1` below, a tie going to the
/// deeper tier. Every rung steps a Task **up**, so a rung may be half a tier at
/// the most; `Pool.Rung` is the vocabulary that keeps a new one from being
/// written without this line being checked against it.
///
/// Exported for the tests that walk it (#237): one unit of push weight is one
/// whole tier, which ADR 0001's eviction price reads, and the rungs in between
/// are the Matcher's business alone.
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
/// augmenting search (#219). The algorithm is sy-harabi's traffic manager, read
/// and rewritten rather than linked — that library is unlicensed and issues the
/// engine's moves itself, which ADR 0001 and ADR 0009 each refuse. What is
/// deliberately not taken from it: its hash-shuffled candidate order, because
/// every tie in this bot falls to the lowest x then y; its cost-matrix
/// threshold, because a crowd is priced here and never made impassable (ADR
/// 0008); and its free-tiles-first candidate order, which is incompatible with
/// the head-and-tail list beside it. Preference order costs nil: a room's pass
/// is O(creeps x 8). The state starts as the identity — every creep holds the
/// tile it stands on — and the intents are offered one at a time in a
/// deterministic order: travellers before stayers (a stayer settled first walls
/// off a traveller's only path), then by rank, then by name. A creep already
/// holding its first candidate is left where it is, and so is one an earlier
/// chain has already shuffled to another tile of the area it works from
/// (#267); any other is lifted off its tile and searched for an augmenting
/// path. A path is a chain of displacements ending on a free tile, and its
/// `score` is the chain's **net** priority: a
/// creep landing on the candidate it asked for first adds its rank's whole
/// weight, the chain's initiator adds the smallest weight there is for landing
/// on a tail instead, a creep merely shuffled out of the way adds nothing, and
/// a creep pushed off a step *it* had asked for subtracts its own weight. A
/// creep pushed off a tile it merely stands on costs nothing, which is ADR
/// 0001's essential rule as arithmetic. Only a strictly positive chain is
/// taken, and dropping the Map the search returned is the whole rollback.
///
/// **Yielding is a move inside the area, and eviction from it is priced**
/// (#267). "Merely stands on" was read off the candidate list — a body whose
/// head is its own tile asked for nothing and so was free to push anywhere —
/// and that reading gave away the one thing ADR 0001 was written to protect:
/// an arrived body could be shoved clean out of its Work Area for nothing, and
/// walked back in next tick at the same price, which in W13S28's Upgrade
/// pocket was a traveller and an upgrader trading the mouth every tick for as
/// long as the scan ran. So a displaced body's landing is read against the
/// area it arrived in (`MoveIntent.Area`): inside it the shuffle is free, as
/// this decision's rule requires, and outside it the chain pays that body's
/// rank's weight and the sidestep it is priced against. A body with no area is
/// unchanged — the free shuffle it always was. The price is only half of it:
/// the fold must also stop *reopening* a body it has already shuffled inside
/// its own area, or that body re-initiates and is paid its rank's whole weight
/// for landing back on the tile it never chose to leave, and three of those
/// phantom gains in one chain buy the very eviction this priced — the same two
/// bodies trading the same mouth for ever on a pocket whose free tile is
/// merely not adjacent.
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

    // What a creep landing on `tile` is worth to the chain. A tail tile is
    // worth the smallest weight there is, and only to the creep the search
    // started from: that creep asked to move and could not have the tile it
    // asked for, and stepping aside is what empties a lane (#219). For a body
    // the chain is shuffling out of somebody's way it is worth nothing —
    // unless the tile lies outside the Work Area it had arrived in, where it
    // is worth *minus* its rank's weight and the sidestep above (#267). ADR
    // 0001's rule is that a creep with slack yields, and the slack a working
    // body has is its own area: pushed to another of its tiles it goes on
    // working and the chain owes it nothing, pushed off the area it stops
    // working and the chain pays for that. The charge is levied here, on the
    // landing, because that is where the search knows where the body ended up;
    // `cost` below is charged on the push, where it does not. A body with no
    // area — a traveller, a parked or idle one — is landing nowhere it was
    // working, so it is the free shuffle this rule leaves exactly as it was.
    //
    // The sidestep is why the charge is the weight **and one**, and it is not a
    // fudge: a chain that ends with its initiator stepping aside onto a tail
    // scores exactly 1, so an eviction priced at the occupant's bare weight
    // makes taking a body off its work worth the same as walking round it as
    // soon as the arriving body is one tier up the ladder — one tier and never
    // one rung, `weightOfRank` above dividing the ladder's rungs back out, so
    // one unit of push weight is one whole tier. Live that reads as the
    // [[buffer]]'s own hauler evicted from the tile it feeds from by the sixth
    // upgrader walking into a full pocket — the ring #241 opened on, the
    // served displacing the server, arrived at through eviction instead of
    // through parking. So a body is taken off its work only by a chain worth
    // strictly more than the sidestep that is the alternative to it.
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

    // Whether an intent has nothing left to ask for: it is holding the tile it
    // asked for, or — the arrived body an earlier chain has already shuffled —
    // it is holding another tile of the area it works from. The second half is
    // the fold's side of #267's rule. A yield inside the area is finished
    // business, and an arrived body that re-initiated after one would collect
    // its rank's whole weight for being put back on the tile it never chose to
    // leave: three such phantom gains in one chain buy the eviction this
    // ticket prices at the weight and the sidestep, which is the two-cycle
    // surviving its own fix on a pocket with a free tile that is not adjacent.
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
        /// chain may run through: the fatigued creeps' (ADR 0008) and the
        /// [[foreign bodies]]' that nobody in the fold holds (#220).
        Blocked: Set<RoomPos>
        /// Who stands where at tick start.
        Occupants: Map<RoomPos, string>
    }

/// Resolver, first half: one colony's Move Intents, unarbitrated. Every rested
/// creep the Atlas places registers one (ADR 0001); a fatigued creep registers
/// none — the engine would answer its move with ERR_TIRED — and its tile is a
/// wall for the tick, so nobody plans a step through it (ADR 0008). Takes the
/// tick's assigned Task per creep as data; a creep absent from the map is idle,
/// unless it is in `crossings` — the vision grace's holders and the room each
/// is still walking toward (#151), which is the one assignment that reaches
/// here without a Task because the Task left the pool with its room's vision.
/// Rerouted is settled here rather than in the pass, because it is the one
/// movement Verdict the arbitration does not answer: it compares this creep's
/// priced first step against the step the same body would take were no tile
/// occupied, which is a second flood on this colony's Atlas.
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
    // [[priority]] (ADR 0052 decision 6), read off the pool rather than
    // re-derived: the mover and the Matcher order the colony's work by one
    // number or they order it by two.
    let priorities = pool |> List.map (fun p -> taskId p.Task, p.Priority) |> Map.ofList

    let priorityOf task =
        Map.tryFind (taskId task) priorities |> Option.defaultValue idleRank

    let tired =
        view.Creeps
        |> List.choose (fun c -> if c.Fatigue > 0 then Some c.Name else None)
        |> Set.ofList

    let placed = Atlas.placedCreeps atlas

    // The ground an idle body steps off (#241, widened by #268), and the ring
    // of ground just outside it that any step off has to land on — one pair per
    // room some body of ours idles in, and none at all for a tick where every
    // body has a Task, which is the tick that must pay nothing for this rule.
    // `idleGroundIn` and not `workingGroundIn`: the [[working ground]] is the
    // Layout's question and this is the mover's, and the two sets part company
    // at the [[storage]]'s and the [[refill cluster]]'s standing tiles (#268).
    let idleGrounds =
        placed
        |> List.filter (fun (name, _) ->
            not (Map.containsKey name assigned)
            // A graced holder is walking a crossing, not idling (#151): it
            // steps off nothing and it is not the [[idle ground]]'s problem.
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
/// never let it into. Once per room, and never across two (#145): arbitrated
/// movement is a room's (ADR 0001, ADR 0008), and ADR 0041's Consequences keep
/// it so — geometry crosses the Seam and arbitration does not. The tiles keying
/// each room's occupants, walls and intents carry the room they are in (ADR
/// 0052 decision 2), or two creeps on one coordinate of two rooms would
/// collapse into one occupant. What is *not* arbitrated is the border tile: two
/// creeps aiming at one exit from its two sides are never checked against each
/// other, which ADR 0041 accepts in as many words. What crosses the Seam is the
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
