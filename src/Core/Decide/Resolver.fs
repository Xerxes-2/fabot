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
/// the room again — unless across is where its step is taking it (#436). The
/// Task goes to `stepToward` beside the area, which is what gives a creep
/// matched across a border somewhere to walk. A body
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
    (runs: bool)
    // A body walking home out of an evacuating room, round its Reach (#512).
    (walksRound: bool)
    // What it walks round where it walks round a Reach, first choice first
    // (`movementOf.roundsOf`), asked only where it does.
    (rounds: unit -> (Set<Pos> * (int -> bool) option) list)
    // The home's shelter for an idle body at home that walks in to it and
    // stays on it (#514); empty for every other.
    (shelter: Set<RoomPos>)
    // For a kiting ranger, the tiles of its room it walks round in place of
    // the Reach (#510); None for every other body.
    (kite: Set<Pos> option)
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
            // Read after the branch priced its step, which filed the plan.
            Onward =
                match candidates with
                | head :: _ when head <> pos ->
                    Atlas.stepPlanOf atlas creep
                    |> Option.filter (fun (first, _) -> first = here head)
                    |> Option.map snd
                | _ -> None
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
    // to, so nothing it is pushed off is work. A step onto its own tile is the
    // crossing it stands on, and it holds that and nothing else.
    let travelling rank step =
        if step = pos then
            intent rank [ pos ] Set.empty
        else
            intent rank (step :: detour step) Set.empty

    // A body standing outside its room's `reach`, the tiles of the last of
    // `avoids`, walks round it (#472): the step that never enters the first
    // of `avoids` a walk keeps out of — its tiles and, where given, those its
    // test answers for by index —, else the plain one where that stays out
    // of `reach`, else none: it waits at the edge, planning no step for the
    // light tick to replay. Inside it, or with none, the plain step.
    let round
        (avoids: (Set<Pos> * (int -> bool) option) list)
        (avoiding: Set<Pos> -> (int -> bool) option -> RoomPos option)
        (plain: unit -> RoomPos option)
        =
        let reach = fst (List.last avoids)

        if Set.isEmpty reach || Set.contains pos reach then
            plain ()
        else
            match avoids |> List.tryPick (fun (avoid, also) -> avoiding avoid also) with
            | Some step -> Some step
            | None ->
                let step =
                    plain ()
                    |> Option.filter (fun tile ->
                        not (tile.Room = room && Set.contains (RoomPos.pos tile) reach))

                if step.IsNone then
                    Atlas.dropStep atlas creep

                step

    // The graced holder's crossing (#151), asked before the Task branches
    // because it is the one body with neither. No Seam to that room — it is
    // not next door — and it falls through to the idle rule below. It crosses
    // on the cheapest chain, as the price does (#297, #485). A body walking
    // home out of an evacuating room walks round its Reach, a Work-heavy one
    // too (#512).
    let crossingStep =
        match task, crossing with
        | None, Some target ->
            let avoids = if walksRound then rounds () else [ Set.empty, None ]

            round
                avoids
                (fun avoid also -> Atlas.stepTowardRoomWhere atlas creep avoid also target)
                (fun () -> Atlas.stepTowardRoom atlas creep Set.empty target)
            |> Option.map RoomPos.pos
        | _ -> None

    // The walk in to the shelter (#514), round the Reach as a runner's is,
    // every tick until the body stands on it. One standing in a Reach — a
    // Work-heavy body on its footing, which has no Flee — takes the plain
    // step inward, even into the Reach, rather than stand still in it
    // (`round`).
    let shelterStep () =
        if task.IsNone && not (Set.isEmpty shelter) && not (Set.contains at shelter) then
            let avoids =
                if Set.isEmpty (Threats.reachIn threats room) then
                    [ Set.empty, None ]
                else
                    rounds ()

            round
                avoids
                (fun avoid also ->
                    Atlas.firstStepAvoidingWhere
                        atlas
                        creep
                        avoid
                        (Option.defaultValue (fun _ -> false) also)
                        shelter)
                (fun () -> Atlas.firstStepWithin atlas creep shelter)
            |> Option.map RoomPos.pos
        else
            None

    match crossingStep |> Option.orElseWith shelterStep, task with
    | Some step, _ ->
        // Walking toward a room it cannot even see, or in to the shelter with
        // no Task, so it pushes at the idle rank.
        travelling idleRank step
    | None, None ->
        // The room's idle ground and the ground just off it: any way off runs
        // through one of those tiles, so the goal set stays the perimeter
        // rather than the whole room.
        let ground, offGround = idleGround room

        // A body on the shelter steps nowhere off it (#514), neither off the
        // idle ground nor when shoved.
        let sheltered (tile: Pos) =
            Set.isEmpty shelter || Set.contains (here tile) shelter

        // The tail, ordered off the idle ground first: `arbitrate` re-houses a
        // displaced body on the first free tile of its list, so an unordered
        // tail puts a body shoved off the ground straight back onto it, and
        // two idle bodies trade the one tile off the ground every tick.
        let off, on =
            beside
            |> List.filter sheltered
            |> List.partition (fun tile -> not (Set.contains tile ground))

        let tail = staying @ off @ on

        // The way off, less this tick's Reach — the subtraction `areaFor`
        // makes below, made here too because a work-heavy body on its footing
        // has no Flee, so a step out of a safe pocket into an attacker is one
        // nothing walks it back from. Nowhere safe off the ground is nowhere
        // to go: it parks.
        let stepOff =
            if Set.contains pos ground then
                let reach = Threats.reachIn threats room

                offGround
                |> Set.filter (fun tile ->
                    not (Set.contains (RoomPos.pos tile) reach) && sheltered (RoomPos.pos tile))
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
            // A body that runs from a Threat, standing outside its room's
            // Reach, walks round it (#472), or waits at its edge where no way
            // round reaches the work. Flee's own walk is the way out of one.
            // A Defend's holder walks round the tiles outside its line a raid melee
            // reaches (#504), and waits inside it where no way round reaches
            // its rampart. A kiting ranger walks round its own set (#510),
            // across the border too.
            let plain () = stepToward atlas creep task area

            let step =
                match kite with
                | Some avoid ->
                    round
                        [ avoid, None ]
                        (fun avoid _ ->
                            match area |> Seq.tryHead with
                            | Some tile when tile.Room <> room ->
                                Atlas.stepTowardRoom atlas creep avoid tile.Room
                            | _ -> Atlas.firstStepAvoiding atlas creep avoid area)
                        plain
                | None when runs && task <> Flee ->
                    let avoids =
                        match task with
                        | Defend _ -> [ Threats.defendAvoidIn threats room, None ]
                        | _ when Set.isEmpty (Threats.reachIn threats room) -> [ Set.empty, None ]
                        | _ -> rounds ()

                    // A walk to work in another room crosses toward it
                    // (`stepTowardRoom`), round what it avoids here (#527):
                    // its Work Area is empty from across a border, so asked
                    // of the room the work is in.
                    let workRoom =
                        area
                        |> Seq.tryHead
                        |> Option.map (fun tile -> tile.Room)
                        |> Option.orElseWith (fun () -> roomOfWork atlas task)

                    round
                        avoids
                        (fun avoid also ->
                            match workRoom with
                            | Some other when other <> room ->
                                Atlas.stepTowardRoomWhere atlas creep avoid also other
                            | _ ->
                                Atlas.firstStepAvoidingWhere
                                    atlas
                                    creep
                                    avoid
                                    (Option.defaultValue (fun _ -> false) also)
                                    area)
                        plain
                | None -> plain ()

            match step |> Option.map RoomPos.pos with
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

    // A body with no Task standing where it is: its tile is anybody's (#562).
    // Not a parked body, which has a Task and chose its tile — a Defend
    // holder waiting inside its line.
    let idle (intent: MoveIntent) =
        staying intent && intent.Rank = idleRank

    // Where an idle body is tried once a mover shoves it off its tile:
    // anywhere else first, then the mover's own tile (a swap), and the
    // mover's way on last (#562) — up the lane ahead of it is where the mover
    // shoves it again the next tick, and the tick after. Every other body
    // keeps its own order.
    let rehouse (mover: MoveIntent) (occupant: MoveIntent) (tiles: RoomPos list) =
        if idle occupant then
            let last = List.choose id [ Some mover.Pos; mover.Onward ]

            (tiles |> List.filter (fun tile -> not (List.contains tile last)))
            @ (last |> List.filter (fun tile -> List.contains tile tiles))
        else
            tiles

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
                                    (occupant.Candidates
                                     |> List.filter ((<>) tile)
                                     |> rehouse intent occupant)
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
    // own area, or an idle body an earlier chain shoved anywhere. Re-initiating the second would collect its rank's whole weight
    // for being put back on a tile it never chose to leave, and three such
    // phantom gains in one chain buy the very eviction `gain` prices; the
    // third would walk off the tile `rehouse` chose onto the mover's way on
    // (#562).
    let asked (intent: MoveIntent) (m: Matching) =
        let held = Map.tryFind intent.Creep m.Tile

        held = headOf intent
        || (staying intent
            && (match held with
                | Some tile -> idle intent || Set.contains tile intent.Area
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

    // Asked only on a tick with a Reach: the mover's one reader of it.
    let runners =
        if Map.isEmpty threats.Reach then
            Set.empty
        else
            view.Creeps
            // And a [[defender]], to its rampart (#504), and any holder of the
            // home's Defend (#546).
            |> List.filter (fun c ->
                runsFromThreats view atlas c
                || isNamedFor defenderPattern c.Name
                || (match Map.tryFind c.Name assigned with
                    | Some(Defend _) -> true
                    | _ -> false))
            |> List.map (fun c -> c.Name)
            |> Set.ofList

    // A fighter walking to a fight in another room than the one it stands in
    // walks round what out-damages it here (movement S2, #533): the fight it
    // walks to is not this room's.
    let passers =
        if Map.isEmpty threats.Reach then
            Set.empty
        else
            placed
            |> List.filter (fun (name, at) ->
                match Map.tryFind name assigned with
                | Some(Guard room)
                | Some(Assault room) -> room <> at.Room
                | _ -> false)
            |> List.map fst
            |> Set.ofList
            |> Set.filter (fun name ->
                view.Creeps |> List.exists (fun c -> c.Name = name && isFighterBody c))

    // What a body walks round in the room it stands in, first choice first
    // (movement S2): the room's Reach — for a fighter, the tiles of it where
    // what strikes within the horizon out-damages its heal —, where the body
    // would stand at its next decision, with the tiles a hostile strikes
    // before it could be on them and gone (`Threats.unsafeFor`); then, where
    // no walk keeps out of those, the Reach alone. The last is the one its
    // own tile and its plain step are read against.
    let roundsOf name room =
        let reach = Threats.reachIn threats room

        match view.Creeps |> List.tryFind (fun c -> c.Name = name) with
        | None -> [ reach, None ]
        | Some creep ->
            let fighter = isFighterBody creep
            let heal = Engine.healPower * partCount creep.Body Heal

            let standing =
                if fighter && heal > 0 then
                    reach
                    |> Set.filter (fun tile ->
                        Threats.damageAt threats room view.Tuning.ReachHorizon tile > heal)
                else
                    reach

            match Threats.unsafeFor threats atlas creep fighter with
            | Some timed -> [ standing, Some timed; standing, None ]
            | None -> [ standing, None ]

    // The bodies walking home out of an evacuating room, which walk round its
    // Reach (#512), and every idle fighter walking home (#533), or it walks
    // back into the Reach Flee walked it out of: asked, as the runners are,
    // only on a tick with one.
    let home = SpatialInfo.homeName view.Spatial

    let roundWalkers =
        if Map.isEmpty threats.Reach then
            Set.empty
        else
            let fighters =
                view.Creeps
                |> List.filter isFighterBody
                |> List.map (fun c -> c.Name)
                |> Set.ofList

            placed
            |> List.filter (fun (name, at) ->
                Map.tryFind name crossings = Some home
                && (Set.contains at.Room view.Evacuating || Set.contains name fighters))
            |> List.map fst
            |> Set.ofList

    // The idle bodies at home while it has a shelter (#514): any but a
    // Fighter, each walking in to it until it stands on it and staying on it
    // after. Read off where the body stands and nothing remembered, so a walk
    // begun beside an exit is kept every tick until it arrives. Asked only on
    // a tick with a shelter; a tired body issues no step at all.
    let sheltering =
        if Map.containsKey home threats.Shelter then
            view.Creeps
            |> List.filter (fun creep ->
                not (isFighterBody creep)
                && not (Map.containsKey creep.Name assigned)
                && not (Map.containsKey creep.Name crossings)
                && not (Set.contains creep.Name tired)
                && Atlas.creepRoom atlas creep.Name = Some home)
            |> List.map (fun creep -> creep.Name)
            |> Set.ofList
        else
            Set.empty

    // What a Guard's holder walks round is its Brief's (`Brief.Avoid`).
    let avoidFor name =
        match Map.tryFind name assigned with
        | Some(Guard room) -> (Threats.briefOf threats room).Avoid
        | _ -> Avoid.Runner

    // A ranger kites (#510); a resident's walk onto its ramparts under a raid
    // crosses the Reach, and is no kite.
    let kiters =
        view.Creeps
        |> List.filter (fun c -> isRangerBody c && avoidFor c.Name = Avoid.MeleeStepAndSwing)
        |> List.map (fun c -> c.Name)
        |> Set.ofList

    let stepAndSwing = Engine.meleeRange + 1

    let avoidOf name (at: RoomPos) =
        if Set.contains name kiters then
            let camp =
                match Map.tryFind at.Room view.ExitHolds with
                | Some hold -> tilesWithin stepAndSwing hold.Exit
                | None -> []

            let strike =
                view.Hostiles
                |> List.filter (fun h -> h.Pos.Room = at.Room && HostileInfo.melee view.Hostiles h)
                |> List.collect (fun h -> tilesWithin stepAndSwing (RoomPos.pos h.Pos))

            Some(Set.ofList (camp @ strike))
        elif avoidFor name = Avoid.Reach then
            Some(Threats.reachIn threats at.Room)
        else
            None

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
        |> List.map (fun room -> room, Atlas.idleGroundAndRingIn atlas room)
        |> Map.ofList

    let idleGround room =
        Map.tryFind room idleGrounds |> Option.defaultValue (Set.empty, Set.empty)

    // A fatigued walker issues no step this tick, so the light tick after it
    // had none to replay and it stood still the tick its fatigue cleared
    // (user, 2026-10-04): its plan is its own tile, then the step it would
    // take. The light tick re-issues it until the engine lets it move.
    for name, at in placed do
        match Map.tryFind name assigned with
        | Some task when Set.contains name tired ->
            let area = areaFor threats atlas name task

            if not (Set.contains at area) then
                match
                    Atlas.withoutRecording atlas name (fun () -> stepToward atlas name task area)
                with
                | Some step when step.Room = at.Room && step <> at ->
                    Atlas.recordStep atlas name (at, step)
                | _ -> ()
        | _ -> ()

    // A reading and not a move, so it leaves the step plan the mover left for
    // the light tick as it was (`Atlas.withoutRecording`): it prices the
    // Task's own crossing where a Guard's mover prices the named room's.
    let rerouted name task =
        let area = areaFor threats atlas name task

        Atlas.withoutRecording atlas name (fun () ->
            match
                Atlas.firstStep atlas name task area,
                Atlas.firstStepIgnoringTraffic atlas name task area
            with
            | Some priced, Some blind -> priced <> blind
            | _ -> false)

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
                    (Set.contains name runners || Set.contains name passers)
                    (Set.contains name roundWalkers)
                    (fun () -> roundsOf name at.Room)
                    (match Map.tryFind home threats.Shelter with
                     | Some ground when Set.contains name sheltering -> ground.Force()
                     | _ -> Set.empty)
                    (avoidOf name at)
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
///
/// `claims` are the tiles the bodies of colonies that do not decide this tick
/// step onto (a forced tick's replay, #549): their steps are issued whatever
/// this pass settles, so a claimed tile is a wall to every intent here, and an
/// idle body of ours standing on one steps off it where it can (#562) — a step
/// it is denied is a step the replayed body is.
let resolveRooms (claims: Set<RoomPos>) (movements: Movement list) : Intent list * Verdict list =
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

            let moveIntents =
                movements
                |> List.collect (fun m -> m.Intents |> List.filter (fun i -> i.Pos.Room = room))

            // A claim moves only an idle body: a body with a Task on a
            // claimed tile — a Defend holder on its rampart — keeps it, the
            // claim is void, and the replayed step into it fails.
            let claimed =
                let inRoom = claims |> Set.filter (fun tile -> tile.Room = room)

                if Set.isEmpty inRoom then
                    inRoom
                else
                    moveIntents
                    |> List.filter (fun i -> i.Rank <> idleRank)
                    |> List.fold (fun kept i -> Set.remove i.Pos kept) inRoom

            let blocked =
                here
                |> List.choose (fun (name, at) ->
                    if Set.contains name tired then Some at else None)
                |> Set.ofList
                |> Set.union foreign
                |> Set.union claimed

            // An idle body standing on a claimed tile asks for its other
            // tiles instead: its own is not a tile it may keep. With none it
            // stays, and the replayed step into it fails.
            let moveIntents =
                if Set.isEmpty claimed then
                    moveIntents
                else
                    moveIntents
                    |> List.map (fun i ->
                        if Set.contains i.Pos claimed then
                            match
                                i.Candidates
                                |> List.filter (fun tile -> not (Set.contains tile claimed))
                            with
                            | [] -> i
                            | rest -> { i with Candidates = rest }
                        else
                            i)

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
    resolveRooms Set.empty [ movementOf view atlas threats pool assigned crossings verbose ]
