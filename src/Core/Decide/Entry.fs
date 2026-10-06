/// The seam: `decide : ColonyView -> Assignments -> Intent list * Assignments`,
/// the single entry the shell calls, and the census signature the plan memo is
/// keyed on.
[<AutoOpen>]
module Fabot.Core.Decide.Entry

open Fabot.Core
open Fabot.Core.Types

/// Join the Matcher's Assignments back onto the Planner's pool: the tick's
/// assigned Task per creep, as data for the Emitter and the Resolver.
let private assignedTasks (tasks: Task list) (assignments: Assignments) : Map<string, Task> =
    let byId = tasks |> List.map (fun t -> taskId t, t) |> Map.ofList

    assignments
    |> Map.toList
    |> List.choose (fun (name, tid) -> Map.tryFind tid byId |> Option.map (fun t -> name, t))
    |> Map.ofList

/// One room's signed census as last joined: the kind and tile of each id it
/// signs, in the layer's id order, beside the three joins they made. The
/// joins are a function of those pairs and the room's name alone, so a room
/// walked to the same pairs is the same three strings, recalled rather than
/// formatted and sorted again — a fixture's id holds its kind and tile, and
/// most ticks a room's pairs do not move.
type private SignedCensus =
    {
        Kinds: TargetKind[]
        Tiles: Pos[]
        Standing: string
        Pending: string
        Minerals: string
    }

/// The last `SignedCensus` per colony and room. Across ticks, so it holds
/// strings and the census's own kind and tile values and nothing else of a
/// tick; checked pair by pair, so a stale entry is a miss and never a wrong
/// answer; emptied when it outgrows `signedKept`. A `Fresh` tree, so `Map.add`
/// keeps a comparer that captures nothing (#401), and swapped whole, so a
/// reader on another thread sees one map or the other.
let mutable private signedCensuses: Map<string, SignedCensus> = Fresh.mapOfList []

/// More colony-rooms than the world projects: a sweep, not a ceiling.
let private signedKept = 256

/// The three joins of one room's census (`signaturesOf`): each signed id's
/// kind and tile formatted, sorted, and joined, recalled when the pairs are
/// the last walk's.
let private signedCensusOf
    (home: string)
    (room: string)
    (positions: Map<string, Pos>)
    (kinds: Map<string, TargetKind>)
    : SignedCensus =
    let signedKinds = ResizeArray<TargetKind>()
    let signedTiles = ResizeArray<Pos>()

    for KeyValue(id, tile) in positions do
        match Map.tryFind id kinds with
        | Some(Structure _ as kind)
        | Some(Site _ as kind)
        | Some(Mineral _ as kind) ->
            signedKinds.Add kind
            signedTiles.Add tile
        | _ -> ()

    let key = $"{home}|{room}"

    let same (held: SignedCensus) =
        let mutable alike = held.Kinds.Length = signedKinds.Count
        let mutable i = 0

        while alike && i < signedKinds.Count do
            alike <-
                (obj.ReferenceEquals(held.Kinds.[i], signedKinds.[i])
                 || held.Kinds.[i] = signedKinds.[i])
                && (obj.ReferenceEquals(held.Tiles.[i], signedTiles.[i])
                    || held.Tiles.[i] = signedTiles.[i])

            i <- i + 1

        alike

    match Map.tryFind key signedCensuses with
    | Some held when same held -> held
    | _ ->
        let standingIds = ResizeArray<string>()
        let pendingIds = ResizeArray<string>()
        let mineralIds = ResizeArray<string>()

        for i in 0 .. signedKinds.Count - 1 do
            let tile = signedTiles.[i]

            match signedKinds.[i] with
            | Structure built -> standingIds.Add $"{built}@{room}:{tile.X},{tile.Y}"
            | Site built -> pendingIds.Add $"{built}@{room}:{tile.X},{tile.Y}"
            | _ -> mineralIds.Add $"Mineral@{room}:{tile.X},{tile.Y}"

        let joined (ids: ResizeArray<string>) =
            ids |> List.ofSeq |> List.sort |> String.concat ";"

        let signed =
            {
                Kinds = signedKinds.ToArray()
                Tiles = signedTiles.ToArray()
                Standing = joined standingIds
                Pending = joined pendingIds
                Minerals = joined mineralIds
            }

        let kept =
            if Map.count signedCensuses >= signedKept then
                Fresh.mapOfList []
            else
                signedCensuses

        signedCensuses <- Map.add key signed kept
        signed

/// ADR-0017
/// ADR-0044
/// The census signature: a string over exactly the inputs the census-derived
/// plans read — the standing structures, our pending sites, the tiles another
/// player's sites hold (#248), the controller level, the home room, who holds
/// each projected room, and every colony's stage. Creeps, stores, hits, piles,
/// hostiles, the bank and the tick are invisible to it. The hold is signed as
/// the *rate* and not the reservation's `TicksToEnd`, which decays every tick.
/// It signs every projected room, because the hauler quota reads every one and
/// the round trip floods that room's grid; the pending census spans every room
/// too, because the walk table's far leg floods the goal room's grid.
///
/// Per room first, flat second (#388): the plan reads the whole census, but the
/// walk tables beside it read one room's grid each and are evicted per room
/// (`PlanMemo.RoomSignatures`, `Atlas.evictRooms`). One walk builds both; what
/// a reader compares is equality, so the layout of the string is free.
let private signaturesOf (view: ColonyView) : Map<string, string> * string =
    let spatial = view.Spatial
    let home = SpatialInfo.homeName spatial

    // The home controller's level: it gates the allowance the placement
    // filters on and, derived, the horizon the clustered placement is sized
    // at, so a level-up moves which tiles the plan holds. The road
    // reservation no longer reads it, so removing this key would be wrong
    // for the cluster alone. The bank Capacity the hauler quota prices at is
    // a function of it too.
    let level =
        view.Controller
        |> Option.map (fun c -> string c.Level)
        |> Option.defaultValue ""

    // Every colony of ours and its stage, in room-name order: a room leaving
    // the map moves the string as surely as one changing stage does.
    let stages =
        view.Stages
        |> Map.toList
        |> List.map (fun (room, stage) -> $"{room}:{stage}")
        |> String.concat ","

    // One walk for the three id-keyed halves: each room's placed ids, each
    // looked up once in the flat kind census, sorted per half afterwards —
    // the same strings in the same order as three walks of the census with a
    // placement search per id, because a placed id reaches exactly one layer.
    // An id placed in a layer but absent from the census is skipped. The old
    // shape was 4% of a `pair --level 7` tick by inclusive samples
    // (2026-09-18, #370), most of it `placementOf` searching every room per
    // id; the A/B came back inside the clock's spread.
    // Rebuilt and not mapped: `Map.map` would keep whichever comparer built
    // the world's Rooms — `Fresh`'s in the shell, a test's or the harness's
    // elsewhere — and the memo outlives the tick (#401).
    // A room held for a stray alone (#570) is signed by neither half: no
    // plan or walk table of the colony's reads it, and its coming and going
    // replans nothing.
    let rooms =
        spatial.Rooms
        |> Map.toList
        |> List.filter (fun (room, _) -> not (Set.contains room spatial.StrayRooms))
        |> List.map (fun (room, layer) ->
            room,
            // The Thorium deposits on their tiles: the Layout reads them
            // twice (the extractor's tile and the mineral container's Seats)
            // and `Atlas.workingGroundIn` at every level. The mod deletes an
            // exhausted deposit outright, and unsigned, the memo would keep
            // handing back a plan naming a container on the Seat of a rock
            // that is gone.
            let signed = signedCensusOf home room layer.TargetPositions spatial.TargetKinds

            // The tiles another player's construction sites hold (#248), with
            // the one thing known about such a site in the kind slot: a
            // rival's site reaches the projection as a tile and nothing else
            // (`RoomLayer.RivalSites`). The Layout's tile clause reads these,
            // so a rival building on the container's pick between two ticks
            // must throw the plan away; unsigned, the memo would hand back
            // the very `PlaceConstructionSite` the engine is refusing. Signed
            // for every projected room, deliberately wider than the one
            // reader (`planLayout` plans the home room alone): over-invalidating
            // is a recompute where a missed input is a stall until a reset.
            let rivals =
                layer.RivalSites
                |> Set.toList
                |> List.map (fun tile -> $"Rival@{room}:{tile.X},{tile.Y}")
                |> String.concat ";"

            let stage =
                Map.tryFind room view.Stages |> Option.map string |> Option.defaultValue ""

            // A rival's room is entered by no chain (#444), so its owner
            // changing moves every route the walk tables were priced along.
            let taken = if Set.contains room spatial.RivalRooms then "Taken" else ""

            $"{home}|{level}|{stage}|{taken}|{signed.Standing}|{signed.Pending}|{rivals}|{signed.Minerals}")
        |> Fresh.mapOfList

    // The rate a room's sources are priced at, and the empty rate for a room
    // vision answered for not at all — a third answer, different from either
    // rate. In the flat signature alone: the hauler quota reads it, no walk
    // table does, and in the per-room half a crossed room's vision coming and
    // going evicted W15S28's spawn walks every tick (82% of its pops,
    // 2026-10-05).
    // Beside it, whether somebody else holds the controller, which an outpost
    // road's sites wait on (#548) and its payback reads (who holds it).
    let held room =
        Map.tryFind room view.RoomControl
        |> Option.map (fun control ->
            let other =
                RoomControlInfo.heldByOther control
                |> Option.map (fun hold -> $"/{hold.Holder}")
                |> Option.defaultValue ""

            $"{heldRateOf control}{other}")
        |> Option.defaultValue ""

    // A room the colony only crosses or harasses (#463) signs its census per
    // room, for the walk tables, and in the flat signature only
    // whether a rival holds it, the one thing of it the plan reads: its
    // narrowing leaves no structure or site in the census, so its rate would
    // move with vision alone, and a rival creep's coming and going replanned
    // the colony. A room with a rock of ours in it is priced at its rate, so
    // a declared outpost under harassment keeps the whole line.
    let priced =
        view.Sources
        |> List.choose (fun s -> SpatialInfo.roomOf spatial s.Id)
        |> Set.ofList

    let unplanned =
        Set.union view.Crossed (Facts.harassRooms view)
        |> Set.filter (fun room -> not (Set.contains room priced))

    // The head and then the rooms, one to a line, which no field can carry:
    // the room set itself is signed by the join, and `signatureMoves` reads
    // the parts back.
    let flat =
        rooms
        |> Map.toList
        |> List.map (fun (room, signature) ->
            if Set.contains room unplanned then
                let taken = if Set.contains room spatial.RivalRooms then "Taken" else ""
                $"\n{room}={taken}"
            else
                $"\n{room}={held room}|{signature}")
        |> String.concat ""

    // Whether the player's sites stand at the outpost roads' ceiling (#548):
    // the count moves with every colony's sites, the side of the line rarely.
    let full = view.PlayerSites >= view.Tuning.OutpostRoadSiteCeiling

    // The ore dug this tick: the hauler quota's mine term opens and shuts with
    // the dig gate, which reads stores no census carries.
    let ore = Facts.oreDug view |> String.concat ","

    // Whether the lab cluster's Storage floor is met: the Layout places a lab
    // off it, and it reads a store no census carries. Signed only while it
    // is, so no other home's signature moves.
    let labs = if Facts.labsFunded view then "\n~labs" else ""

    rooms, $"{home}|{level}|{stages}{flat}\n~sites={full}\n~ore={ore}{labs}"

let censusSignature (view: ColonyView) : string = snd (signaturesOf view)

/// A flat signature's parts by name: the head's three, then each room's.
let private signatureParts (signature: string) : Map<string, string> =
    match signature.Split('\n') |> List.ofArray with
    | head :: rooms ->
        let headParts =
            match head.Split('|') with
            | [| home; level; stages |] -> [ "home", home; "level", level; "stages", stages ]
            | _ -> [ "head", head ]

        let roomParts =
            rooms
            |> List.map (fun line ->
                let at = line.IndexOf '='

                if at < 0 then
                    line, ""
                else
                    line.Substring(0, at), line.Substring(at + 1))

        Map.ofList (headParts @ roomParts)
    | [] -> Map.empty

/// What moved between two flat signatures (#463): the head's parts by name
/// (`home`, `level`, `stages`), then the rooms, a room on one side only
/// included. Read on a replan alone, so its cost is the replan's.
let signatureMoves (before: string) (after: string) : string list =
    let was = signatureParts before
    let is = signatureParts after

    Set.union (Map.keys was |> Set.ofSeq) (Map.keys is |> Set.ofSeq)
    |> Set.toList
    |> List.filter (fun part -> Map.tryFind part was <> Map.tryFind part is)
    |> List.sortBy (fun part ->
        match part with
        | "home" -> 0
        | "level" -> 1
        | "stages" -> 2
        | _ -> 3)

/// The per-room half of the census signature (#388): what
/// `PlanMemo.RoomSignatures` carries and `Atlas.evictRooms` is told the
/// difference of.
let roomSignatures (view: ColonyView) : Map<string, string> = fst (signaturesOf view)

/// The view with where each body's load came from settled, before anything
/// reads a body.
let private withDrawn (view: ColonyView) (assignments: Assignments) : ColonyView =
    { view with
        Creeps =
            view.Creeps
            |> List.map (fun creep ->
                { creep with
                    DrawnFrom = drawnFrom view assignments creep
                })
    }

/// The tiles of our bodies this tick's one room pass shoves for free (#600,
/// `ColonyView.Loose`), every colony's, by home: a body rested and holding no
/// Task at the last match (`holding`), of a colony deciding this tick (`idle`
/// names those replaying, whose bodies are the pass's walls), and of a
/// colony that may decide its fight alone, a fighter, the one body such a
/// decide moves (`joinFight`). The one statement of who is loose, which every
/// colony's walk reads off the view; the pass shoves such a body as it
/// shoves any idle one (#562).
let private looseTiles
    (holding: Assignments)
    (idle: Set<string>)
    (fightAlone: Set<string>)
    (views: (string * ColonyView) list)
    : Set<RoomPos> =
    views
    |> List.filter (fun (home, _) -> not (Set.contains home idle))
    |> List.collect (fun (home, view) ->
        view.Creeps
        |> List.filter (fun creep ->
            creep.Fatigue = 0
            && not (Map.containsKey creep.Name holding)
            && (not (Set.contains home fightAlone) || isFighterBody creep))
        |> List.choose (fun creep -> SpatialInfo.creepPlacementOf view.Spatial creep.Name))
    |> Set.ofList

/// The Atlas a decide prices from, its tables recalled off the memo and each
/// room's evicted where its census moved, beside the per-room and flat
/// signatures and the tables handed to it.
let private recallAtlas (view: ColonyView) (memo: PlanMemo option) =
    let signedRooms, signature = signaturesOf view

    // ADR-0032
    // The memo's tables go to the Atlas whatever the plan's signature did:
    // every entry is a pure function of the walking grids and Seam bands of
    // the rooms it names, which the per-room signatures sign. An ask the
    // decision layer narrowed for itself — a Guard's ring off this tick's
    // Threats — keys on tiles that move every tick and rides a slot of its
    // own (`FarFieldMemo.Narrowed`). The tables used to go whole or empty on
    // the flat signature, which made every replan a re-flood of the lot;
    // measured by count 2026-09-20 (#388, `docs/profiling.md`): 109,258 heap
    // pops a tick against 9,554 quiet, and evicting per room took the
    // perturbed tick to 91,920 with the harness moving the home room.
    let walks, farFields =
        match memo with
        | Some m ->
            m.Walks,
            {
                SeamWalks = m.SeamWalks
                PerCensus = m.FarFields
                Narrowed = m.Narrowed
                Lines = m.Lines
                RoadFloods = m.RoadFloods
                CreepFloods = m.CreepFloods
            }
        | None -> FarFieldMemo.walks (), FarFieldMemo.empty ()

    let atlas = Atlas.ofViewRecalling walks farFields view

    // The rooms whose census moved since the tables were filled, including a
    // room on one side only. Evicted before any flood is forced. Read off the
    // memo's per-room stamp and not settled by the plan's recall: on a
    // deferred turn the two are different ticks', and a recalled plan
    // carrying tables a dark tick filled is exactly #372.
    match memo with
    | Some m when m.RoomSignatures <> signedRooms ->
        let moved =
            Set.union (Map.keys m.RoomSignatures |> Set.ofSeq) (Map.keys signedRooms |> Set.ofSeq)
            |> Set.filter (fun room ->
                Map.tryFind room m.RoomSignatures <> Map.tryFind room signedRooms)

        if not moved.IsEmpty then
            Atlas.evictRooms atlas m.RivalRooms moved
    | _ -> ()

    memo |> Option.iter (fun m -> Atlas.evictAvoided atlas m.AvoidRooms)

    signedRooms, signature, walks, farFields, atlas

/// A memo handed on with this tick's stamps: the per-room signatures, the
/// rooms a rival holds and the rooms a non-fighter goes round.
let private stamped
    (view: ColonyView)
    (signedRooms: Map<string, string>)
    (memo: PlanMemo)
    : PlanMemo =
    { memo with
        RoomSignatures = signedRooms
        RivalRooms = view.Spatial.RivalRooms
        AvoidRooms = view.Spatial.AvoidRooms
    }

/// The row sizing as the fight-only ticks after it carry it: rebuilt by
/// `Fresh`, for it outlives the tick (#401).
let private carriedSizing (sizing: RowSizing) : RowSizing =
    { sizing with
        AnchorPostCaps = sizing.AnchorPostCaps |> Map.toSeq |> Fresh.mapOfSeq
    }

/// The fight pass's inputs to the match, one colony's: the Threats and
/// their Briefs, the structures' fight, the fight Tasks and their pool. All
/// of it lives within the tick.
[<NoEquality; NoComparison>]
type private FightPass =
    {
        Threats: Threats
        /// The towers', safe mode's and the rampart doors' acts.
        Towers: Lazy<Intent list>
        /// Flee and the combat Tasks (`planFightTasks`).
        Tasks: Lazy<Task list>
        /// Their pool (`planFightPool`).
        Pool: Lazy<PooledTask list>
    }

/// The fight pass's head off the guarded outposts (`OutpostFacts.Guarded`):
/// the Threats now, the rest when first read, so a fight-only decide the
/// economy hands back prices none of it.
let private fightPass
    (view: ColonyView)
    atlas
    (held: HeldTaskFacts)
    (guarded: string list)
    : FightPass =
    let threats = threatsOfHeld view atlas held
    let tasks = lazy (planFightTasks view atlas threats held guarded)

    {
        Threats = threats
        Towers =
            lazy
                (planSafeMode view atlas held
                 @ planFire view atlas
                 @ planTowerHeal view atlas
                 @ planRampartDoors view)
        Tasks = tasks
        Pool = lazy (planFightPool threats tasks.Value)
    }

/// The fight pass's tail, over the bodies `movers` names (every body where
/// None): the Emitter's acts, laid among the rest by `around`, the strike and
/// heal reflexes over every body, the melee glue and the steps. The Intents
/// (`movers`' alone, where named, the glue after) and the movement.
let private fightActs
    (view: ColonyView)
    atlas
    (threats: Threats)
    (pool: PooledTask list)
    (assigned: Map<string, Task>)
    (crossings: Map<string, string>)
    (verbose: Set<string>)
    (movers: (string -> bool) option)
    (around: Intent list -> Intent list)
    : Intent list * Movement =
    let intents =
        emit view atlas threats assigned
        |> around
        |> Fabot.Core.IntentPlan.create
        |> function
            | Ok selected ->
                strikeReflex view atlas threats assigned selected
                |> healReflex view threats
                |> Fabot.Core.IntentPlan.intents
            | Error conflict -> invalidOp $"Conflicting creep intents: %A{conflict}"

    // The reflexes run over every body, so a fighter's heal still reaches an
    // economy body; a body `movers` leaves out has its own reflex elsewhere.
    let intents =
        match movers with
        | None -> intents
        | Some moves ->
            intents
            |> List.filter (fun intent -> IntentPlan.actor intent |> Option.forall moves)

    // A melee fighter steps onto the target it swings at (#535): its move is
    // that step, and the mover leaves its tile standing, as a tired body's.
    let glued = glue view atlas threats assigned

    let movement =
        let stepping =
            glued
            |> List.choose (function
                | MoveCreep(name, _) -> Some name
                | _ -> None)
            |> Set.ofList

        let moved =
            match movers with
            | None -> movementOf view atlas threats pool assigned crossings verbose
            | Some moves -> movementAmong moves view atlas threats pool assigned crossings verbose

        if Set.isEmpty stepping then
            moved
        else
            { moved with
                Tired = Set.union moved.Tired stepping
                Intents =
                    moved.Intents
                    |> List.filter (fun step -> not (Set.contains step.Creep stepping))
            }

    intents @ glued, movement

/// What the world's fight hands one colony (`worldFights`): the Briefs of the
/// world's combat Tasks it pools none of itself but may send a fighter to,
/// those Tasks, and each world Task's caps, its owner's. Lives within the
/// tick.
[<NoEquality; NoComparison>]
type private WorldFight =
    {
        Briefs: Map<Task, Brief>
        Tasks: Task list
        Caps: Map<Task, Capacity>
    }

/// A colony alone in the world: its own fight.
let private ownFight =
    {
        Briefs = Map.empty
        Tasks = []
        Caps = Map.empty
    }

/// A colony's decide opened as far as its fight's head: its Threats and its
/// own combat Tasks, which the world's Brief table reads, and the rest of
/// the decide once the world's fight is known (`Join`). Lives within the
/// tick.
[<NoEquality; NoComparison>]
type private Opening =
    {
        Home: string
        View: ColonyView
        Threats: Threats
        /// Flee and its own combat Tasks (`planFightTasks`), on first read.
        FightTasks: Lazy<Task list>
        Fighters: CreepInfo list
        Join: WorldFight -> Joining
    }

/// A colony's decide joined to the world's fight: its side of the match and
/// the shares it brings, and the rest of the decide off the matches its
/// bodies were judged in (`Finish`) — None for a fight-only decide the
/// economy hands back.
and [<NoEquality; NoComparison>] private Joining =
    {
        Context: MatchContext
        Fight: MatchShare
        Economy: MatchShare option
        Finish: MatchOutcome list -> Decision option
    }

/// A Brief's place down `BriefKind.order`, a Task of its own first.
let private kindRank (brief: Brief) =
    match brief.Duty with
    | Duty.Guard kind -> BriefKind.order |> List.tryFindIndex ((=) kind) |> Option.defaultValue 0
    | _ -> 0

/// The Threats a colony's fighters are priced, emitted and moved in: its
/// own, the world's Briefs laid in beside them, each in the place of an own
/// Brief of the same Task whose kind comes later down `BriefKind.order`.
let private foughtThreats (threats: Threats) (world: WorldFight) : Threats =
    if Map.isEmpty world.Briefs then
        threats
    else
        { threats with
            Briefs =
                (threats.Briefs, world.Briefs)
                ||> Map.fold (fun briefs task brief ->
                    match Map.tryFind task briefs with
                    | Some mine when kindRank mine <= kindRank brief -> briefs
                    | _ -> Map.add task brief briefs)
        }

/// The fight pool over the world's Tasks too, each of the world's combat
/// Tasks capped as its owner caps it: one count of holders over the fleet.
let private worldPool
    (threats: Threats)
    (tasks: Task list)
    (world: WorldFight)
    (own: Lazy<PooledTask list>)
    : PooledTask list =
    if List.isEmpty world.Tasks && Map.isEmpty world.Caps then
        own.Value
    else
        planFightPool threats tasks
        |> List.map (fun pooled ->
            match Map.tryFind pooled.Task world.Caps with
            | Some capacity -> { pooled with Capacity = capacity }
            | None -> pooled)

/// A colony's assignments off the matches its bodies were judged in.
let private nextOf (creeps: CreepInfo list) (outcomes: MatchOutcome list) : Assignments =
    creeps
    |> List.choose (fun creep ->
        outcomes
        |> List.tryPick (fun outcome -> Map.tryFind creep.Name outcome.Assigned)
        |> Option.map (fun tid -> creep.Name, tid))
    |> Map.ofList

/// One colony's match alone: its fighters, then its economy from the
/// holdings they left.
let private matchAlone (joining: Joining) (assignments: Assignments) (verbose: Set<string>) =
    matchAcross
        [ joining.Context ]
        []
        Map.empty
        ([ [ 0, joining.Fight ] ]
         @ (joining.Economy |> Option.map (fun share -> [ 0, share ]) |> Option.toList))
        assignments
        verbose

/// A whole decide as far as its fight's head: what the rest of it reads.
[<NoEquality; NoComparison>]
type private WholeHead =
    {
        View: ColonyView
        Atlas: Atlas.Atlas
        Plan: PlanMemo
        Held: HeldTaskFacts
        Outposts: OutpostFacts
        Fight: FightPass
        Verbose: Set<string>
    }

/// A whole decide up to its match: what the rest of it reads.
[<NoEquality; NoComparison>]
type private WholeTail =
    {
        Head: WholeHead
        /// The Threats its fighters are priced, emitted and moved in
        /// (`foughtThreats`).
        Fought: Threats
        Defense: Intent list
        Consignment: Intent list
        Spawn: Intent list
        OutpostSites: Intent list
        Waiting: WaitingContainer list
        Quotas: Quotas
        Overflow: HaulOverflow
        Busy: BusyReason list
        Sizing: RowSizing
        /// Every Task the match may assign, the world's among them.
        Tasks: Task list
        Pool: PooledTask list
        EconomyPool: PooledTask list
        Fighters: CreepInfo list
        EconomyBodies: CreepInfo list
    }

/// A whole decide opened (`Opening`): plan, match, emit, move, beside the
/// colony steps (spawns, sites), all pricing from one Atlas built up front.
let rec private openWhole
    (view: ColonyView)
    (assignments: Assignments)
    (verbose: Set<string>)
    (memo: PlanMemo option)
    (turn: ReplanTurn)
    : Opening =
    let view = withDrawn view assignments

    // The signature is read before the Atlas is built, because the Atlas is
    // one of the things it decides: the plan is recalled whole on the flat
    // signature, the tables beside it per room on the per-room half.
    let signedRooms, signature, walks, farFields, atlas = recallAtlas view memo
    let recalled = memo |> Option.filter (fun m -> m.Signature = signature)

    // The rooms a rival holds, and the rooms a non-fighter goes round, stamped
    // beside the per-room signatures on every memo handed on: the tables are
    // this tick's whatever the plan is.
    let rivals = view.Spatial.RivalRooms
    let avoided = view.Spatial.AvoidRooms

    // Whose turn it is to re-plan (#357, `turn`). A stale memo is not a
    // wrong plan, only an old one — the reservations are level-blind and a
    // site already in the world outlives the Intent that placed it — while
    // four colonies re-planning in the same tick measured 487 ms of the
    // engine's 500 ms ceiling. A colony whose turn has not come serves what
    // it has; both keep a signature this tick's census cannot match, so the
    // plan is owed and the next turn pays it.
    let plan =
        match recalled with
        // The recalled memo goes on with its plan and the tables this tick
        // filled. The room stamp is **this tick's** all the same: on the tick
        // a census returns after a deferred turn (#372) the stamp is the dark
        // tick's, and handed on as it was it would evict and re-flood that
        // room every tick until the next replan.
        | Some m ->
            { m with
                RoomSignatures = signedRooms
                RivalRooms = rivals
                AvoidRooms = avoided
            }
        // A colony whose turn has not come (#357) serves its stale plan but
        // carries the tick's own tables on it, stamped with this tick's
        // per-room signatures: a census that moves and moves back recalls the
        // plan and evicts the entries the intermediate tick laid (#372).
        | None when turn = ReplanTurn.Waiting ->
            match memo with
            | Some stale ->
                { stale with
                    RoomSignatures = signedRooms
                    RivalRooms = rivals
                    AvoidRooms = avoided
                    Walks = walks
                    SeamWalks = farFields.SeamWalks
                    FarFields = farFields.PerCensus
                    Narrowed = farFields.Narrowed
                    Lines = farFields.Lines
                    RoadFloods = farFields.RoadFloods
                    CreepFloods = farFields.CreepFloods
                }
            | None ->
                let deferred =
                    PlanMemo.deferred
                        signedRooms
                        walks
                        farFields.SeamWalks
                        farFields.PerCensus
                        farFields.Narrowed
                        farFields.Lines
                        farFields.RoadFloods
                        farFields.CreepFloods

                { deferred with
                    RivalRooms = rivals
                    AvoidRooms = avoided
                }
        | None ->
            let (siteIntents,
                 servedFootings,
                 unservedFootings,
                 unroutedTrunks,
                 deferredContainers,
                 outpostRoads,
                 _) =
                planLayout view atlas

            let quota, demandRows, load = haulerDemandOf view atlas

            {
                Signature = signature
                RoomSignatures = signedRooms
                RivalRooms = rivals
                AvoidRooms = avoided
                SiteIntents = siteIntents
                UnservedFootings = unservedFootings
                ServedFootings = servedFootings
                UnroutedTrunks = unroutedTrunks
                DeferredContainers = deferredContainers
                OutpostRoads = outpostRoads
                HaulerQuota = quota
                HaulerDemand = demandRows
                HaulerLoad = load
                Walks = walks
                SeamWalks = farFields.SeamWalks
                FarFields = farFields.PerCensus
                Narrowed = farFields.Narrowed
                Lines = farFields.Lines
                RoadFloods = farFields.RoadFloods
                CreepFloods = farFields.CreepFloods
            }

    // The narrow facts the Planner reads about the colony's own decisions:
    // the task ids its living creeps hold, their holders and which still
    // carry Thorium, off the table the Matcher wrote last tick. Derived
    // before the Threats, which lay each Assault's ground.
    let held = heldTaskFacts view assignments

    // The outpost chain's answers, derived once (#383): before this
    // `claimTargets` ran 11.4 times a tick and `outpostControllers` 9.1,
    // each walking the kind census. The fight pass reads its guarded rooms.
    let outposts = outpostFactsOf view atlas

    // The fight pass's head (`fightPass`): the tick's Threats, derived once
    // and shared by every reader, the structures' fight, the fight Tasks and
    // their pool.
    let fight = fightPass view atlas held outposts.Guarded

    let head =
        {
            View = view
            Atlas = atlas
            Plan = plan
            Held = held
            Outposts = outposts
            Fight = fight
            Verbose = verbose
        }

    {
        Home = SpatialInfo.homeName view.Spatial
        View = view
        Threats = fight.Threats
        FightTasks = fight.Tasks
        Fighters = view.Creeps |> List.filter isFighterBody
        Join = joinWhole head
    }

/// A whole decide joined to the world's fight (`Joining`).
and private joinWhole (head: WholeHead) (world: WorldFight) : Joining =
    let view, atlas, plan = head.View, head.Atlas, head.Plan
    let held, outposts, fight = head.Held, head.Outposts, head.Fight
    let threats = fight.Threats
    let fought = foughtThreats threats world

    // The outpost's source containers, beside the memoised Layout and never
    // inside it: derived fresh every tick for the reason on the rule itself.
    let outpostContainers, waitingContainers = planOutpostContainers view atlas

    let outpostSiteIntents = outpostContainers @ planFirstSpawns view atlas

    let defenseIntents = fight.Towers.Value

    // The consignment's send (#349), beside the defence reflexes because it is
    // the same kind of thing: a structure's own verb, read off the view.
    let consigned = planConsignment view

    let sendingIn intents =
        intents
        |> List.choose (function
            | SendFromTerminal(id, _, _, _) -> Some id
            | _ -> None)
        |> Set.ofList

    let aidIntents = planAid view (sendingIn consigned)

    // The lab cluster's ore, behind the Thorium and the aid: one send a
    // terminal a tick.
    let oreSends = planOreSend view (sendingIn (consigned @ aidIntents))

    let consignmentIntents =
        consigned
        @ aidIntents
        @ oreSends
        @ planReaction view
        @ planObserver view
        @ planExtractorMove view atlas

    // The pool is derived before the spawns, and the dependency runs one way:
    // the worker row's floor asks the pool, and nothing in the pool reads a
    // spawn Intent.
    let sizing = rowSizingOf view atlas threats outposts held

    // The fight pass, carved out of the match: the fighters
    // (`isFighterBody`) are kept and matched over Flee and the combat Tasks
    // first, every other body over Flee and the economy's after them. It
    // reads the view's fighters, the Atlas, the Threats and their Briefs,
    // `held`, the guarded outposts and the row sizing (a relief's lead);
    // it leaves the fighters' assignments and Verdicts. The structures' fight
    // (`defenseIntents`), the Emitter, the reflexes and the one Resolver
    // still run over both passes' bodies together. The whole list is
    // `planTasks`'s: the fight's at its head.
    let tasks = fight.Tasks.Value @ planEconomyTasks view atlas held outposts

    // Why the colony is busy (#523), once a tick for the upgrader row and the
    // buffer's rank.
    let busy = busyReasons view tasks (not (List.isEmpty aidIntents))

    // Every entry's priority and capacity, set once here and read by the
    // Matcher and the mover: the fight pass's off the Briefs, the economy's
    // after them, and the whole pool the two in pool order (`planPools`). The
    // fight pass's over the world's combat Tasks too, each capped as its
    // owner caps it (`worldPool`).
    let fightTasks = fight.Tasks.Value @ world.Tasks
    let fightPool = worldPool fought fightTasks world fight.Pool

    let economyPool =
        planPoolHolding held view atlas busy (List.filter (safetyTier >> not) tasks)

    let pool = fightPool @ economyPool

    // A far field whose Task is not in the tick's list is dead weight the
    // census never evicts (#392). Every far leg priced this tick — by the
    // Matcher off the pool, by the mover off the assignments — is a Task in
    // this list; a graced crossing (#151) prices none.
    Atlas.evictFarFieldsExcept atlas (Set.ofList (tasks @ world.Tasks))

    // The overflow term rides beside the memoised flow number and never
    // inside it: it reads the containers' stock, which moves every tick.
    let overflow = haulOverflowOf view atlas plan.HaulerDemand

    let spawnIntents, quotas =
        planSpawns
            view
            atlas
            outposts
            sizing
            threats
            tasks
            busy
            (withOverflow plan.HaulerQuota overflow)

    let fighters, economyBodies = view.Creeps |> List.partition isFighterBody

    let tail =
        {
            Head = head
            Fought = fought
            Defense = defenseIntents
            Consignment = consignmentIntents
            Spawn = spawnIntents
            OutpostSites = outpostSiteIntents
            Waiting = waitingContainers
            Quotas = quotas
            Overflow = overflow
            Busy = busy
            Sizing = sizing
            Tasks = fightTasks @ List.filter (safetyTier >> not) tasks
            Pool = pool
            EconomyPool = economyPool
            Fighters = fighters
            EconomyBodies = economyBodies
        }

    {
        Context =
            {
                View = view
                Atlas = atlas
                Sizing = sizing
                Threats = fought
                Pool = pool
                PooledElsewhere = false
            }
        Fight =
            {
                Bodies = fighters
                Offered = fightPool
            }
        Economy =
            Some
                {
                    Bodies = economyBodies
                    // Flee is both sides': the fight pool's entry, ahead.
                    Offered = List.filter (fun pooled -> pooled.Task = Flee) fightPool @ economyPool
                }
        Finish = finishWhole tail
    }

/// A whole decide finished off the matches its bodies were judged in.
and private finishWhole (tail: WholeTail) (outcomes: MatchOutcome list) : Decision option =
    let head = tail.Head
    let view, atlas, plan, outposts = head.View, head.Atlas, head.Plan, head.Outposts
    let verbose = head.Verbose
    let threats = tail.Fought
    let tasks, pool, economyPool = tail.Tasks, tail.Pool, tail.EconomyPool
    let fighters, economyBodies = tail.Fighters, tail.EconomyBodies
    let defenseIntents, consignmentIntents = tail.Defense, tail.Consignment
    let spawnIntents, outpostSiteIntents = tail.Spawn, tail.OutpostSites

    let quotas, overflow, busy, sizing =
        tail.Quotas, tail.Overflow, tail.Busy, tail.Sizing

    let waitingContainers = tail.Waiting
    let next = nextOf view.Creeps outcomes
    let verdicts = verdictsOf view.Creeps outcomes

    let assigned = assignedTasks tasks next

    // The other half of the vision grace (#151): the holders the Matcher kept
    // against a Task in no pool, and the room each one's target was last seen
    // in. They reach the Emitter as nothing and the mover as a crossing. A
    // keep with no pooled Task is a graced one by construction.
    //
    // And an idle guard standing outside home walks home (#416): a Guard beyond
    // the hop budget from an outpost is unpriceable from there, and priced from
    // home, so the body waits where every guarded room is in reach.
    let crossings =
        let graced =
            next
            |> Map.toList
            |> List.filter (fun (name, _) -> not (Map.containsKey name assigned))
            |> List.choose (fun (name, tid) ->
                lastSeenIn view tid |> Option.map (fun room -> name, room))

        let home = SpatialInfo.homeName view.Spatial

        // So does any idle body standing in an evacuating room (#512): there
        // is no work there for it to wait for.
        let homeward =
            view.Creeps
            |> List.filter (fun creep -> not (Map.containsKey creep.Name assigned))
            |> List.choose (fun creep ->
                Atlas.creepRoom atlas creep.Name
                |> Option.filter (fun room ->
                    room <> home && (isFighterBody creep || Set.contains room view.Evacuating))
                |> Option.map (fun _ -> creep.Name, home))

        // Graced last: `Map.ofList` keeps the later entry, so a guard walking a
        // held Task's crossing keeps that room over home.
        homeward @ graced |> Map.ofList

    // The Emitter's acts among every other: a task's pickup owns this creep's
    // channel. Otherwise the reflex picks its last reachable pile, preserving
    // the engine's former last-write choice without emitting overwritten
    // calls. Different creeps may still share a pile.
    let among taskIntents =
        let taskPickers =
            taskIntents
            |> List.choose (function
                | PickupPile(name, _) -> Some name
                | _ -> None)
            |> Set.ofList

        let pickupIntents =
            planPickups view atlas assigned
            |> List.filter (function
                | PickupPile(name, _) -> not (Set.contains name taskPickers)
                | _ -> true)

        // The signature (#381), shaped after the pickup reflex: no walk, no
        // Task, no competition.
        let signIntents = planSignatures view atlas Colony.signatureFor

        defenseIntents
        @ consignmentIntents
        @ spawnIntents
        @ plan.SiteIntents
        @ outpostSiteIntents
        @ signIntents
        @ pickupIntents
        @ taskIntents

    let intents, movement =
        fightActs view atlas threats pool assigned crossings verbose None among

    // A refiller that poured this tick has nothing for the light tick after it
    // to replay, a pour being one-shot, so it stood still a tick per member
    // (user, 2026-10-04). Its next act is laid now, off the pour this tick
    // makes: the next hungry member beside it, or else a step toward the rest
    // (`Atlas.recordStep`, ahead of `stepPlans` below). A cadence past 2 lays
    // one pour per light tick it replays, each into the next hungry member
    // beside while the load lasts (`Decision.Later`).
    let pours =
        match Atlas.cluster atlas with
        | None -> []
        | Some cluster ->
            view.Creeps
            |> List.choose (fun creep ->
                let poured =
                    intents
                    |> List.tryPick (function
                        | TransferEnergyToStructure(name, target, Energy) when name = creep.Name ->
                            Some target
                        | _ -> None)

                match
                    Map.tryFind creep.Name assigned, poured, Atlas.creepTile atlas creep.Name
                with
                | Some(Refill(spawnId, Energy)), Some target, Some at when spawnId = cluster.Spawn ->
                    let roomOf id =
                        Map.tryFind id cluster.Members |> Option.defaultValue 0

                    let hungry = RefillCluster.hungry cluster

                    // The pours after the one this tick makes, a light tick's
                    // each: `light` counts them, `filled` the members poured.
                    let rec onward light (filled: string list) left =
                        let rest =
                            hungry
                            |> List.filter (fun id -> not (List.contains id filled))
                            |> List.choose (fun id ->
                                Atlas.positionOf atlas id |> Option.map (fun tile -> id, tile))
                            |> List.filter (fun (_, tile) -> tile.Room = at.Room)

                        let beside =
                            rest
                            |> List.choose (fun (id, tile) ->
                                RoomPos.range at tile
                                |> Option.filter (fun r -> r <= 1)
                                |> Option.map (fun r -> r, id))

                        if light >= view.Tuning.Cadence || left <= 0 || List.isEmpty rest then
                            []
                        else
                            match beside with
                            | _ :: _ ->
                                let _, id = List.min beside

                                TransferEnergyToStructure(creep.Name, id, Energy)
                                :: onward (light + 1) (id :: filled) (left - min left (roomOf id))
                            | [] when light > 1 -> []
                            | [] ->
                                let goals =
                                    rest
                                    |> List.collect (fun (_, tile) ->
                                        Atlas.adjacentWalkableIn
                                            atlas
                                            tile.Room
                                            (RoomPos.pos tile)
                                        |> List.map (RoomPos.at tile.Room))
                                    |> Set.ofList

                                match
                                    Atlas.withoutRecording atlas creep.Name (fun () ->
                                        Atlas.firstStepWithin atlas creep.Name goals)
                                with
                                | Some step when step.Room = at.Room && step <> at ->
                                    Atlas.recordStep atlas creep.Name (at, step)
                                | _ -> ()

                                []

                    match
                        onward 1 [ target ] (creep.Energy - min creep.Energy (roomOf target))
                    with
                    | [] -> None
                    | laid -> Some laid
                | _ -> None)

    let nextPours = pours |> List.map List.head

    // The hauler's arrivals and the refiller's pours for the light tick
    // `light` (2 and on), each act at its own index.
    let laterOf (byCreep: Intent list list) light =
        byCreep |> List.choose (List.tryItem (light - 1))

    // A hauler stepping into its Task's Work Area this tick acts on arrival,
    // which the tick after it, light, issues (#529): withheld now for reach
    // alone, it stood a tick idle per leg. Off the tile it asks to step onto,
    // the Emitter's own act read there (`Emitter.arrival`), never on a
    // channel one of the body's acts this tick holds. A cadence past 2 reads
    // each later light tick's act off the step plan's tile it lands on then.
    let arrivals =
        let bodies = view.Creeps |> List.map (fun creep -> creep.Name, creep) |> Map.ofList

        movement.Intents
        |> List.choose (fun step ->
            // The tile it wants most; the arbitration may hand it another,
            // and the engine refuses the act out of reach.
            match
                Map.tryFind step.Creep assigned,
                Map.tryFind step.Creep bodies,
                List.tryHead step.Candidates
            with
            | Some task, Some creep, Some landing when landing <> step.Pos ->
                let later =
                    match Atlas.stepPlanOf atlas step.Creep with
                    | Some(first :: beyond) when first = landing ->
                        List.truncate (view.Tuning.Cadence - 2) beyond
                    | _ -> []

                landing :: later
                |> List.map (
                    arrival view atlas threats creep task
                    >> Option.filter (fun act ->
                        intents |> List.forall (IntentPlan.compatible act))
                )
                |> Some
            | _ -> None)

    // Read after the mover, which is what fills the table. A pair whose tiles
    // name two rooms is no plan a light tick could walk.
    let steps = Atlas.stepPlans atlas |> Fresh.mapOfSeq

    // What the colony casts for and parks are its own Threats' answers: a
    // world Brief laid in is another colony's to cast for.
    let own = head.Fight.Threats

    {
        Intents = intents
        Assignments = next
        Memo = plan
        Verdicts = verdicts
        Movement = movement
        Quotas =
            { quotas with
                HaulerLoad = plan.HaulerLoad
                HaulerDemand = plan.HaulerDemand
                Overflow = overflow
                Busy = busy
            }
        HaulOverflow = overflow
        OutpostRooms = outposts.Declared
        WaitingContainers = waitingContainers
        Steps = steps
        Next = nextPours @ List.choose List.head arrivals
        Later =
            [
                for light in 2 .. view.Tuning.Cadence - 1 ->
                    laterOf pours light
                    @ List.choose (List.tryItem (light - 1) >> Option.flatten) arrivals
            ]
        Aims = aims view atlas threats assigned
        Volleys = volleys view atlas threats assigned
        Drawn =
            view.Creeps
            |> List.choose (fun creep ->
                creep.DrawnFrom |> Option.map (fun store -> creep.Name, store))
            |> Map.ofList
        Fight =
            {
                Sizing = carriedSizing sizing
                Asks = castAsks view own outposts.Guarded
                Fighters = fighters |> Seq.map _.Name |> Fresh.setOfSeq
                EconomyPooled = not (List.isEmpty economyPool)
                OutpostRooms = outposts.Declared
                Held =
                    economyBodies
                    |> Seq.choose (fun (creep: CreepInfo) ->
                        Map.tryFind creep.Name assigned
                        |> Option.map (fun task -> creep.Name, task))
                    |> Fresh.mapOfSeq
                Parked =
                    (let walledOff = walledOffBy view atlas own

                     view.Creeps
                     |> Seq.filter (fun creep ->
                         Map.tryFind creep.Name assigned |> Option.exists (walledOff creep))
                     |> Seq.map _.Name
                     |> Fresh.setOfSeq)
            }
    }
    |> Some

/// The decision seam: a colony view in, Decision out, with this colony's
/// movement left unarbitrated on it — a room's traffic is not one colony's
/// decision, so the caller folds every colony's together and arbitrates each
/// room once (`resolveRooms`). Plan, match, emit, move, beside the colony steps
/// (spawns, sites), all pricing from one Atlas built up front; a colony alone
/// in the world (`openWhole`, `matchAlone`).
let decideUnarbitrated
    (view: ColonyView)
    (assignments: Assignments)
    (verbose: Set<string>)
    (memo: PlanMemo option)
    (turn: ReplanTurn)
    : Decision =
    let joining = (openWhole view assignments verbose memo turn).Join ownFight

    match joining.Finish [ matchAlone joining assignments verbose ] with
    | Some decision -> decision
    | None -> invalidOp "A whole decide is never handed back"

/// A fight-only decide (`LightTick.fightOnly`): the fight pass alone, on the inputs a
/// whole decide reads — the Threats, the structures' fight (`defenseIntents`'
/// towers, safe mode and rampart doors), the fight pool and the fighters'
/// match, emit, reflexes and steps — with the economy's few answers it reads
/// carried off the last whole decide (`carry`: a relief's row sizing, and
/// whether the economy pooled anything, which an idle fighter's reason reads)
/// and the guarded rooms derived again, as the Threats derive them. Every
/// economy body keeps its Task (`assignments`), issues nothing and takes no
/// step: the caller replays its plan. Its Intents are the fighters' and the
/// structures' fight; its movement registers the fighters alone.
///
/// None where the economy would decide otherwise, so the caller decides the
/// colony whole this tick: a body across the fighter line since the carry,
/// a cast ask moved or owed a spawn standing idle — spawning is the
/// economy's —, an economy body a Reach has walled off from its Task since
/// (`walledOffBy`), or a fighter's step an economy body stands in. The last
/// is read once the match is made, so the opening is None for the rest and
/// the joined decide's `Finish` None for it.
let rec private openFight
    (view: ColonyView)
    (assignments: Assignments)
    (verbose: Set<string>)
    (memo: PlanMemo)
    (carry: FightCarry)
    : Opening option =
    let view = withDrawn view assignments
    let fighters, economyBodies = view.Creeps |> List.partition isFighterBody
    let fighting = fighters |> List.map _.Name |> Set.ofList

    if fighting <> carry.Fighters then
        None
    else

        let signedRooms, _, _, _, atlas = recallAtlas view (Some memo)
        let held = heldTaskFacts view assignments

        // `outpostFactsOf`'s guarded rooms, off the two facts of the chain they
        // read, as the Threats derive them.
        let guarded =
            Facts.guardedRooms view atlas (Facts.declaredOutposts view) (Facts.fuelledErrands view)

        let fight = fightPass view atlas held guarded
        let threats = fight.Threats

        // An economy body a Reach has walled off from its Task since the
        // carry (#591): only a whole decide's Matcher lets it go to other
        // work, and its replay would park it at the edge until the next one.
        let walledOffAnew =
            lazy
                (let walledOff = walledOffBy view atlas threats

                 economyBodies
                 |> List.exists (fun creep ->
                     not (Set.contains creep.Name carry.Parked)
                     && match
                         Map.tryFind creep.Name carry.Held, Map.tryFind creep.Name assignments
                        with
                        | Some task, Some tid when taskId task = tid -> walledOff creep task
                        | _ -> false))

        if
            castAsks view threats guarded <> carry.Asks
            || castOwed view carry.Asks
            || walledOffAnew.Force()
        then
            None
        else
            Some
                {
                    Home = SpatialInfo.homeName view.Spatial
                    View = view
                    Threats = threats
                    FightTasks = fight.Tasks
                    Fighters = fighters
                    Join = joinFight view assignments verbose memo carry signedRooms atlas fight
                }

/// A fight-only decide joined to the world's fight (`Joining`).
and private joinFight
    (view: ColonyView)
    (assignments: Assignments)
    (verbose: Set<string>)
    (memo: PlanMemo)
    (carry: FightCarry)
    (signedRooms: Map<string, string>)
    atlas
    (fight: FightPass)
    (world: WorldFight)
    : Joining =
    let fighters, economyBodies = view.Creeps |> List.partition isFighterBody
    let fighting = fighters |> List.map _.Name |> Set.ofList
    let threats = foughtThreats fight.Threats world
    let tasks = fight.Tasks.Value @ world.Tasks
    let pool = worldPool threats tasks world fight.Pool

    let finish (outcomes: MatchOutcome list) : Decision option =
        let defenseIntents = fight.Towers.Value
        let next = nextOf view.Creeps outcomes
        let verdicts = verdictsOf view.Creeps outcomes

        let assigned = assignedTasks tasks next

        // `decideUnarbitrated`'s crossings, of the fighters: a graced keep's last
        // room, else home for an idle one standing away.
        let crossings =
            let home = SpatialInfo.homeName view.Spatial

            let graced =
                next
                |> Map.toList
                |> List.filter (fun (name, _) -> not (Map.containsKey name assigned))
                |> List.choose (fun (name, tid) ->
                    lastSeenIn view tid |> Option.map (fun room -> name, room))

            let homeward =
                fighters
                |> List.filter (fun creep -> not (Map.containsKey creep.Name assigned))
                |> List.choose (fun creep ->
                    Atlas.creepRoom atlas creep.Name
                    |> Option.filter (fun room -> room <> home)
                    |> Option.map (fun _ -> creep.Name, home))

            homeward @ graced |> Map.ofList

        // The reflexes run over every body as on a whole decide, so a fighter's
        // heal still reaches an economy body; an economy body's own reflex is its
        // replay's.
        let intents, movement =
            fightActs
                view
                atlas
                threats
                pool
                assigned
                crossings
                verbose
                (Some(fun name -> Set.contains name fighting))
                (fun emitted -> defenseIntents @ emitted)

        let economy = economyBodies |> List.map _.Name |> Set.ofList

        // A fighter whose wanted tile an economy body stands on: only a
        // whole decide's arbitration shoves or swaps that body, and here
        // it is a wall — a worker parked on a rampart, a hauler in a choke.
        let blocked =
            let walls =
                Atlas.placedCreeps atlas
                |> List.filter (fun (name, _) -> Set.contains name economy)
                |> List.map snd
                |> Set.ofList

            movement.Intents
            |> List.exists (fun step ->
                match step.Candidates with
                | wanted :: _ -> wanted <> step.Pos && Set.contains wanted walls
                | [] -> false)

        if blocked then
            None
        else

            Some
                {
                    Intents = intents
                    Assignments =
                        (next, assignments)
                        ||> Map.fold (fun acc name tid ->
                            if Set.contains name economy then
                                Map.add name tid acc
                            else
                                acc)
                    Memo = stamped view signedRooms memo
                    Verdicts = verdicts
                    Movement = movement
                    Quotas = Quotas.silent
                    HaulOverflow = HaulOverflow.none
                    OutpostRooms = carry.OutpostRooms
                    WaitingContainers = []
                    Steps =
                        Atlas.stepPlans atlas
                        |> Seq.filter (fun (name, _) -> Set.contains name fighting)
                        |> Fresh.mapOfSeq
                    Next = []
                    Later = []
                    Aims = aims view atlas threats assigned
                    Volleys = volleys view atlas threats assigned
                    Drawn =
                        view.Creeps
                        |> List.choose (fun creep ->
                            creep.DrawnFrom |> Option.map (fun store -> creep.Name, store))
                        |> Map.ofList
                    Fight = carry
                }

    {
        Context =
            {
                View = view
                Atlas = atlas
                Sizing = carry.Sizing
                Threats = threats
                Pool = pool
                PooledElsewhere = carry.EconomyPooled
            }
        Fight = { Bodies = fighters; Offered = pool }
        Economy = None
        Finish = finish
    }

/// A fight-only decide (`openFight`) of a colony alone in the world.
let decideFight
    (view: ColonyView)
    (assignments: Assignments)
    (verbose: Set<string>)
    (memo: PlanMemo)
    (carry: FightCarry)
    : Decision option =
    openFight view assignments verbose memo carry
    |> Option.bind (fun opening ->
        let joining = opening.Join ownFight
        joining.Finish [ matchAlone joining assignments verbose ])

/// The decision seam a shell with one colony — and the whole suite — asks for:
/// `decideUnarbitrated`'s answer with this colony's movement folded back in
/// through `resolveRooms`.
let decide
    (view: ColonyView)
    (assignments: Assignments)
    (verbose: Set<string>)
    (memo: PlanMemo option)
    : Decision =
    // One colony alone is always its own turn to re-plan: the budget #357
    // added staggers colonies against each other.
    let view =
        { view with
            Loose =
                looseTiles
                    assignments
                    Set.empty
                    Set.empty
                    [ SpatialInfo.homeName view.Spatial, view ]
        }

    let decision = decideUnarbitrated view assignments verbose memo ReplanTurn.Now
    let moveIntents, moveVerdicts = resolveRooms Set.empty [ decision.Movement ]

    { decision with
        Intents = decision.Intents @ moveIntents
        Verdicts = decision.Verdicts @ moveVerdicts
    }

/// What one tick's colonies decide off, beside their views: the shell's
/// tick (`Main`) and the arena's alike.
[<NoEquality; NoComparison>]
type ColonyInputs =
    {
        /// Which lane matches the fight (`Tuning.FightLane`, or its live
        /// override): every view is put on it, and the world lane decides
        /// the colonies together (`decideWorld`).
        Lane: FightLane
        /// Crossings from a colony's home to a room, as `Facts.judge` ranks
        /// a raided room's answerer by.
        Hops: string -> string -> int option
        /// Last tick's assignments, every colony's in one table.
        Holding: Assignments
        Verbose: Set<string>
        /// Each colony's plan memo, by home.
        PlanMemos: Map<string, PlanMemo>
        /// Each colony's last whole decide's carry, by home.
        Carries: Map<string, FightCarry>
        /// The colonies that decide nothing this tick: replayed, or waiting
        /// out the reset tick.
        Idle: Set<string>
        /// The colonies whose every fact is a fight's (`LightTick.fightOnly`),
        /// each deciding its fight alone where it has a memo and a carry and
        /// the economy does not hand it back.
        FightAlone: Set<string>
        /// The colony index whose turn it is to re-plan (#357), -1 for none;
        /// None where every colony re-plans on its own turn.
        TurnIndex: int option
    }

/// One tick's decisions over every colony.
type ColonyDecisions<'Stamp> =
    {
        /// Every colony with its view, judged and its observer aimed, in
        /// colony order.
        Views: (Colony * ColonyView) list
        /// Each decision in the order it was made — the fight-only ones
        /// first —, with the view it read and the stamp taken after it.
        Decided: (Colony * ColonyView * Decision * 'Stamp) list
        /// The colonies that decided their fight alone.
        FoughtAlone: Set<string>
        /// The turn the whole decides were handed, -1 where it waits.
        Turn: int
        /// Whether the turn's colony decided no plan this tick, so the turn
        /// stays its.
        TurnWaits: bool
    }

/// Each raided room's one verdict, priced once over the fleet and the
/// world's vision and laid in each view (`Facts.judge`): a colony that does
/// not decide (`idle`) sends only the bodies its plan already holds there.
let judgeColonies
    (hops: string -> string -> int option)
    (assignments: Assignments)
    (idle: Set<string>)
    (colonies: (Colony * ColonyView) list)
    : ColonyView list =
    colonies
    |> List.map (fun (colony, view) ->
        ({
            Cut = view
            Owns = Colony.declaredRooms colony
            Decides = not (Set.contains colony.Home idle)
        }
        : Facts.Judged))
    |> Facts.judge hops assignments

/// ADR-0091
/// The world's fight, one Brief per combat Task world-wide: of the colonies
/// that pool a Guard or a Defend, its owner's — a home's Defend the home's;
/// a Guard its declarer's, else the room's verdict's Defender or answerer,
/// else the fewest crossings from it, then by home. Each Task is capped as
/// its owner caps it, whichever colony pools it, so one count of holders
/// over the fleet fills it. A colony with a fighter is handed the owner's
/// Brief of each such Task it pools none of in a room it projects. An
/// Assault stays its declarer's alone.
let private worldFights
    (hops: string -> string -> int option)
    (openings: (Colony * Opening) list)
    : Map<string, WorldFight> =
    let claims =
        openings
        |> List.collect (fun (colony, opening) ->
            opening.FightTasks.Value
            |> List.choose (fun task ->
                match task with
                | Guard room
                | Defend room ->
                    Threats.briefFor opening.Threats task
                    |> Option.map (fun brief -> task, (room, colony, opening, brief))
                | _ -> None))
        |> List.groupBy fst
        |> List.map (fun (task, claimed) -> task, List.map snd claimed)

    let owned =
        claims
        |> List.map (fun (task, claimed) ->
            let room, _, _, _ = List.head claimed

            // A room's kind is the first claim down `BriefKind.order`, every
            // colony's claims read as one room's: a resident room's Guard is
            // its residents' colony's.
            let first = claimed |> List.map (fun (_, _, _, brief) -> kindRank brief) |> List.min

            let ranked = claimed |> List.filter (fun (_, _, _, brief) -> kindRank brief = first)

            let named home =
                ranked |> List.tryFind (fun (_, colony, _, _) -> colony.Home = home)

            let judged () =
                ranked
                |> List.tryPick (fun (_, _, opening, _) ->
                    Map.tryFind room opening.View.World.Answers)
                |> Option.bind (fun verdict ->
                    verdict.Value.Defender |> Option.orElse verdict.Value.Answerer)
                |> Option.bind named

            let owner =
                (match task with
                 | Defend home -> named home
                 | _ -> None)
                |> Option.orElse (
                    ranked
                    |> List.tryFind (fun (_, colony, _, _) ->
                        Set.contains room (Colony.declaredRooms colony))
                )
                |> Option.orElse (judged ())
                |> Option.defaultWith (fun () ->
                    ranked
                    |> List.minBy (fun (_, colony, _, _) ->
                        hops colony.Home room |> Option.defaultValue System.Int32.MaxValue,
                        colony.Home))

            let _, ownerColony, ownerOpening, brief = owner

            // Its caps: the owner's crowds, never fewer, each at the most any
            // colony that caps it asks — each colony's Brief counts only the
            // rows its own bodies answer (a hauler's kill asks a ranger only
            // of the colony that has one).
            let capacity = brief.Capacity ownerOpening.Threats

            let caps =
                claimed
                |> List.map (fun (_, _, opening, claim) -> (claim.Capacity opening.Threats).Caps)

            let merged =
                capacity.Caps
                |> Map.map (fun scope limit ->
                    caps |> List.choose (Map.tryFind scope) |> List.fold max limit)

            task,
            (room, ownerColony.Home, brief, { capacity with Caps = merged }),
            List.length claimed > 1)

    openings
    |> List.map (fun (colony, opening) ->
        let own = opening.FightTasks.Value |> Set.ofList
        let fights = not (List.isEmpty opening.Fighters)

        let handed =
            owned
            |> List.filter (fun (task, (room, owner, _, _), shared) ->
                (shared || owner <> colony.Home)
                && (Set.contains task own
                    || fights && Map.containsKey room opening.View.Spatial.Rooms))

        // An own Brief of a world Task whose kind comes later down
        // `BriefKind.order` than the owner's gives way to it (#601): the
        // room is fought as its kind, whichever colony's body holds it.
        let yields task (brief: Brief) =
            Set.contains task own
            && Threats.briefFor opening.Threats task
               |> Option.exists (fun mine -> kindRank mine > kindRank brief)

        // Its own Briefs of each room's fight: a body one of them admits
        // answers the room through it, so another colony's Brief there
        // admits it not (a resident keeps its Guard of a child's home, never
        // the child's Defend beside it; a melee guard its Guard admits none
        // of may still take that Defend).
        let briefedIn (room: string) =
            handed
            |> List.choose (fun (task, (_, _, brief, _), _) ->
                if yields task brief then Some task else None)
            |> Set.ofList
            |> Set.difference own
            |> List.ofSeq
            |> List.choose (fun task ->
                match task with
                | Guard at
                | Defend at when at = room -> Threats.briefFor opening.Threats task
                | _ -> None)

        let laid =
            handed
            |> List.filter (fun (task, (_, _, brief, _), _) ->
                not (Set.contains task own) || yields task brief)

        let narrowed (room: string) (brief: Brief) =
            match briefedIn room with
            | [] -> brief
            | briefs ->
                { brief with
                    Admits =
                        fun creep ->
                            brief.Admits creep
                            && not (briefs |> List.exists (fun mine -> mine.Admits creep))
                }

        colony.Home,
        {
            Briefs =
                laid
                |> List.map (fun (task, (room, _, brief, _), _) -> task, narrowed room brief)
                |> Map.ofList
            Tasks =
                laid
                |> List.choose (fun (task, _, _) ->
                    if Set.contains task own then None else Some task)
            Caps =
                handed
                |> List.map (fun (task, (_, _, _, capacity), _) -> task, capacity)
                |> Map.ofList
        })
    |> Map.ofList

/// The world lane's tick (`FightLane.World`): every deciding colony opened —
/// the fight-only ones (`openFight`) off the judged views, every other whole
/// (`openWhole`) off the aimed ones —, the world's fight laid in
/// (`worldFights`), one match over every fighter of every colony
/// (`matchAcross`, a colony that decides nothing counting the holdings its
/// fighters keep), then each colony's economy matched from the holdings its
/// fighters left, and each decide finished, its fighters' acts and steps
/// its own. A fight-only colony a fighter's step hands back to a whole
/// decide is decided whole, and the lane run again without it.
let private decideWorld
    (inputs: ColonyInputs)
    (stamp: unit -> 'Stamp)
    (colonies: (Colony * ColonyView) list)
    (judged: ColonyView list)
    : ColonyDecisions<'Stamp> =
    let rec lane (alone: Set<string>) =
        let fought =
            List.zip colonies judged
            |> List.filter (fun ((colony, _), _) -> Set.contains colony.Home alone)
            |> List.choose (fun ((colony, _), view) ->
                Option.map2
                    (fun memo carry -> openFight view inputs.Holding inputs.Verbose memo carry)
                    (Map.tryFind colony.Home inputs.PlanMemos)
                    (Map.tryFind colony.Home inputs.Carries)
                |> Option.flatten
                |> Option.map (fun opening -> colony, view, opening))

        let fightOnly = fought |> List.map (fun (colony, _, _) -> colony.Home) |> Set.ofList

        let turnWaits =
            match inputs.TurnIndex with
            | Some turn ->
                colonies
                |> List.tryItem turn
                |> Option.exists (fun (colony, _) ->
                    Set.contains colony.Home inputs.Idle || Set.contains colony.Home fightOnly)
            | None -> false

        let turn =
            match inputs.TurnIndex with
            | Some turn when not turnWaits -> turn
            | _ -> -1

        let views =
            judged
            |> Layout.aimObservers (Set.union inputs.Idle fightOnly)
            |> List.zip (colonies |> List.map fst)

        let whole =
            views
            |> List.indexed
            |> List.filter (fun (_, (colony, _)) ->
                not (Set.contains colony.Home inputs.Idle)
                && not (Set.contains colony.Home fightOnly))
            |> List.map (fun (index, (colony, view)) ->
                let whose =
                    match inputs.TurnIndex with
                    | Some _ when index <> turn -> ReplanTurn.Waiting
                    | _ -> ReplanTurn.Now

                colony,
                view,
                openWhole
                    view
                    inputs.Holding
                    inputs.Verbose
                    (Map.tryFind colony.Home inputs.PlanMemos)
                    whose)

        let opened = fought @ whole

        let fights =
            worldFights
                inputs.Hops
                (opened |> List.map (fun (colony, _, opening) -> colony, opening))

        let joined =
            opened
            |> List.map (fun (colony, view, opening) ->
                colony, view, opening, opening.Join fights.[colony.Home])

        // The fighters of a colony that decides nothing keep their Tasks,
        // counted against the world's caps.
        let bystanders =
            colonies
            |> List.filter (fun (colony, _) -> Set.contains colony.Home inputs.Idle)
            |> List.collect (fun (_, view) -> view.Creeps |> List.filter isFighterBody)

        let seed =
            bystanders
            |> List.choose (fun creep ->
                Map.tryFind creep.Name inputs.Holding |> Option.map (fun tid -> creep.Name, tid))
            |> Map.ofList

        let fleet =
            matchAcross
                (joined |> List.map (fun (_, _, _, joining) -> joining.Context))
                bystanders
                seed
                [ joined |> List.mapi (fun k (_, _, _, joining) -> k, joining.Fight) ]
                inputs.Holding
                inputs.Verbose

        let finished =
            joined
            |> List.map (fun (colony, view, opening, joining) ->
                let economy =
                    joining.Economy
                    |> Option.map (fun share ->
                        let fighters =
                            opening.Fighters
                            |> List.choose (fun creep ->
                                Map.tryFind creep.Name fleet.Assigned
                                |> Option.map (fun tid -> creep.Name, tid))
                            |> Map.ofList

                        matchAcross
                            [ joining.Context ]
                            []
                            fighters
                            [ [ 0, share ] ]
                            inputs.Holding
                            inputs.Verbose)

                let decision = joining.Finish(fleet :: Option.toList economy)
                colony, view, decision, decision |> Option.map (fun _ -> stamp ()))

        match
            finished
            |> List.choose (fun (colony, _, decision, _) ->
                match decision with
                | None -> Some colony.Home
                | Some _ -> None)
        with
        | [] ->
            {
                Views = views
                Decided =
                    finished
                    |> List.choose (fun (colony, view, decision, stamped) ->
                        Option.map2
                            (fun decision stamped -> colony, view, decision, stamped)
                            decision
                            stamped)
                FoughtAlone = fightOnly
                Turn = turn
                TurnWaits = turnWaits
            }
        | handed -> lane (Set.difference fightOnly (Set.ofList handed))

    lane inputs.FightAlone

/// Every colony's tick decided: the views judged (`judgeColonies`), the
/// fight-only decides first (`decideFight`), so the turn and the observers'
/// looks read the colonies that did fight alone, never the candidates a
/// decide handed back — the fight reads no observer's look —, then every
/// observer's one look aimed together (`Layout.aimObservers`), a fight-only
/// or idle colony's replaying its last, then each other colony decided
/// whole (`decideUnarbitrated`). `stamp` is read after each decision.
let decideColonies
    (inputs: ColonyInputs)
    (stamp: unit -> 'Stamp)
    (colonies: (Colony * ColonyView) list)
    : ColonyDecisions<'Stamp> =
    // Every view on the inputs' lane, which its rules read off its Tuning.
    let colonies =
        colonies
        |> List.map (fun (colony, view) ->
            colony,
            if view.Tuning.FightLane = inputs.Lane then
                view
            else
                { view with
                    Tuning =
                        { view.Tuning with
                            FightLane = inputs.Lane
                        }
                })

    // And on the one set of loose tiles the tick's room pass shoves for free.
    let loose =
        colonies
        |> List.map (fun (colony, view) -> colony.Home, view)
        |> looseTiles inputs.Holding inputs.Idle inputs.FightAlone

    let colonies =
        colonies |> List.map (fun (colony, view) -> colony, { view with Loose = loose })

    let judged = judgeColonies inputs.Hops inputs.Holding inputs.Idle colonies

    match inputs.Lane with
    | FightLane.World -> decideWorld inputs stamp colonies judged
    | FightLane.PerColony ->

        let fought =
            List.zip (colonies |> List.map fst) judged
            |> List.filter (fun (colony, _) -> Set.contains colony.Home inputs.FightAlone)
            |> List.choose (fun (colony, view) ->
                Option.map2
                    (fun memo carry -> decideFight view inputs.Holding inputs.Verbose memo carry)
                    (Map.tryFind colony.Home inputs.PlanMemos)
                    (Map.tryFind colony.Home inputs.Carries)
                |> Option.flatten
                |> Option.map (fun decision -> colony, view, decision, stamp ()))

        let fightOnly =
            fought |> List.map (fun (colony, _, _, _) -> colony.Home) |> Set.ofList

        let turnWaits =
            match inputs.TurnIndex with
            | Some turn ->
                colonies
                |> List.tryItem turn
                |> Option.exists (fun (colony, _) ->
                    Set.contains colony.Home inputs.Idle || Set.contains colony.Home fightOnly)
            | None -> false

        let turn =
            match inputs.TurnIndex with
            | Some turn when not turnWaits -> turn
            | _ -> -1

        let views =
            judged
            |> Layout.aimObservers (Set.union inputs.Idle fightOnly)
            |> List.zip (colonies |> List.map fst)

        // The decision is bound and then tupled, and the turn is a DU
        // (`ReplanTurn`), because the first shape shipped broken: as an expression
        // inside the tuple with a `bool` last argument, the turn arrived as
        // JavaScript `undefined`, `not undefined` is `true`, and no layout was
        // planned at all (#357). Only `npm run profile` caught it.
        let whole =
            views
            |> List.indexed
            |> List.filter (fun (_, (colony, _)) ->
                not (Set.contains colony.Home inputs.Idle)
                && not (Set.contains colony.Home fightOnly))
            |> List.map (fun (index, (colony, view)) ->
                let memo = Map.tryFind colony.Home inputs.PlanMemos

                let whose =
                    match inputs.TurnIndex with
                    | Some _ when index <> turn -> ReplanTurn.Waiting
                    | _ -> ReplanTurn.Now

                let decision = decideUnarbitrated view inputs.Holding inputs.Verbose memo whose

                colony, view, decision, stamp ())

        {
            Views = views
            Decided = fought @ whole
            FoughtAlone = fightOnly
            Turn = turn
            TurnWaits = turnWaits
        }
