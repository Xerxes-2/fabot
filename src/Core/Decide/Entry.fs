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

/// One colony's census signed (`signaturesOf`).
type private Signed =
    {
        /// The per-room half (`PlanMemo.RoomSignatures`).
        Rooms: Map<string, string>
        /// What vision read of each room (`PlanMemo.RoomCensuses`).
        Censuses: Map<string, RoomCensus>
        /// The flat signature.
        Signature: string
        /// The dark rooms signed by their last sighting (`PlanMemo.PaidDark`).
        Remembered: Set<string>
    }

/// ADR-0017
/// ADR-0044
/// The census signature: a string over exactly the inputs the census-derived
/// plans read — the standing structures, our pending sites, the tiles another
/// player's sites hold (#248), the controller level, the home room, who holds
/// each projected room, and every colony's stage. Creeps, stores, hits, piles,
/// hostiles, the bank and the tick are invisible to it. The hold is signed as
/// the *rate* and not the reservation's `TicksToEnd`, which decays every tick.
/// The flat signature signs every room of the economy's (#667: a room the
/// fight alone projects, `SpatialInfo.FightRooms`, no plan reads), because the
/// hauler quota reads every one and the round trip floods that room's grid;
/// the per-room signatures sign every projected room but a stray's, the
/// pending census included, because a walk table's far leg floods its goal
/// room's grid, the fight's tables among them.
///
/// Per room first, flat second (#388): the plan reads the whole census, but the
/// walk tables beside it read one room's grid each and are evicted per room
/// (`PlanMemo.RoomSignatures`, `Atlas.evictRooms`). One walk builds both; what
/// a reader compares is equality, so the layout of the string is free.
///
/// A room out of sight (#666) is signed by what stood there when it was last
/// seen, so our vision coming and going moves no memo: its census and its
/// controller's hold as the memo last stamped them (`PlanMemo.RoomCensuses`),
/// everything the colony decides of it — its stage, a rival's hold, whether
/// it is only crossed — read fresh. The projection stays dark — no decision
/// reads a remembered structure —, so a plan paid while such a room is dark
/// is owed again once it is seen (`PlanMemo.PaidDark`). A room never seen
/// signs what it projects, which no flip moves.
let private signaturesOf (view: ColonyView) (memo: PlanMemo option) : Signed =
    let spatial = view.Spatial
    let home = SpatialInfo.homeName spatial
    let dark = ColonyView.dark view

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
    // The rate a room's sources are priced at, and the empty rate for a room
    // vision answered for not at all — a third answer, different from either
    // rate. In the flat signature alone: the hauler quota reads it, no walk
    // table does, and in the per-room half a crossed room's vision coming and
    // going evicted W15S28's spawn walks every tick (82% of its pops,
    // 2026-10-05). A dark room the memo remembers reads its last sighting's.
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

    // Each room's per-room signature, what vision read of it — the last
    // sighting's for a dark room the memo remembers — and whether it was.
    let signedRooms =
        spatial.Rooms
        |> Map.toList
        |> List.filter (fun (room, _) -> not (Set.contains room spatial.StrayRooms))
        |> List.map (fun (room, layer) ->
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

            let projected = $"{signed.Standing}|{signed.Pending}|{rivals}|{signed.Minerals}"

            let remembered =
                match memo |> Option.bind (fun m -> Map.tryFind room m.RoomCensuses) with
                | Some last when dark room -> Some last
                | _ -> None

            let census =
                remembered
                |> Option.defaultWith (fun () -> { Census = projected; Held = held room })

            room, $"{home}|{level}|{stage}|{taken}|{census.Census}", census, remembered.IsSome)

    let rooms =
        signedRooms
        |> List.map (fun (room, signature, _, _) -> room, signature)
        |> Fresh.mapOfList

    let censuses =
        signedRooms
        |> List.map (fun (room, _, census, _) -> room, census)
        |> Fresh.mapOfList

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
        Set.union view.Crossed (Facts.operationRooms view)
        |> Set.filter (fun room -> not (Set.contains room priced))

    // The economy's rooms alone (#667): the plan's chains enter no room the
    // fight alone projects (`Atlas.routes`), and its pricing reads none, so
    // a sweep cast or a walk home coming and going replans nothing. Its
    // per-room signature stays, for the fight's walk tables.
    let economic =
        signedRooms
        |> List.filter (fun (room, _, _, _) -> not (spatial.FightRooms.Contains room))

    // The head and then the rooms, one to a line, which no field can carry:
    // the room set itself is signed by the join, and `signatureMoves` reads
    // the parts back.
    let flat =
        economic
        |> List.map (fun (room, signature, census, _) ->
            if Set.contains room unplanned then
                let taken = if Set.contains room spatial.RivalRooms then "Taken" else ""
                $"\n{room}={taken}"
            else
                $"\n{room}={census.Held}|{signature}")
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

    {
        Rooms = rooms
        Censuses = censuses
        Signature = $"{home}|{level}|{stages}{flat}\n~sites={full}\n~ore={ore}{labs}"
        // Only a room whose census the flat signature reads (#667): a crossed
        // room's is its Taken alone, read fresh, so its sighting owes nothing.
        Remembered =
            economic
            |> List.choose (fun (room, _, _, remembered) ->
                if remembered && not (Set.contains room unplanned) then
                    Some room
                else
                    None)
            |> Fresh.setOfSeq
    }

let censusSignature (view: ColonyView) : string = (signaturesOf view None).Signature

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
let roomSignatures (view: ColonyView) : Map<string, string> = (signaturesOf view None).Rooms

/// What vision read of each room, as the signature signs it
/// (`PlanMemo.RoomCensuses`).
let roomCensuses (view: ColonyView) : Map<string, RoomCensus> = (signaturesOf view None).Censuses

/// The view with where each body's load came from settled, before anything
/// reads a body.
let private withDrawn (view: ColonyView) (assignments: Assignments) : ColonyView =
    let drawn =
        List.map (fun creep ->
            { creep with
                DrawnFrom = drawnFrom view assignments creep
            })

    { view with
        Creeps = drawn view.Creeps
        Fighters = drawn view.Fighters
    }

/// The tiles of our bodies that leave them this tick (#649,
/// `ColonyView.Leaving`), every colony's, by home: the one statement of who
/// is traffic, which every walk reads off the view. A tile is occupied only
/// by a body still standing on it when a walker gets there. A body of ours
/// on its room's border ring is gone from it whatever it does, the engine
/// carrying a body that ends its tick there into the neighbour; a rested one
/// elsewhere is gone either way it leaves:
/// - it is on the move (`CreepInfo.Walking`), whichever colony holds it, the
///   first step left to the arbitration;
/// - or this tick's one room pass shoves it for free (#600): holding no Task
///   at the last match (`holding`), of a colony deciding this tick (`idle`
///   names those replaying, whose bodies are the pass's walls), and of a
///   colony that may decide its fight alone, a fighter, the one body such a
///   decide moves (`joinFight`); the pass shoves it as it shoves any idle
///   one (#562).
/// A fatigued body stays, as does one that stopped (seated at its work, or
/// held), and every body not ours.
let leavingTiles
    (holding: Assignments)
    (idle: Set<string>)
    (fightAlone: Set<string>)
    (views: (string * ColonyView) list)
    : Set<RoomPos> =
    views
    |> List.collect (fun (home, view) ->
        let shoves (creep: CreepInfo) =
            not (Set.contains home idle)
            && not (Map.containsKey creep.Name holding)
            && (not (Set.contains home fightAlone) || isFighterBody creep)

        ColonyView.bodies view
        |> List.choose (fun creep ->
            SpatialInfo.creepPlacementOf view.Spatial creep.Name
            |> Option.filter (fun tile ->
                // A fatigued body on the ring is carried back onto it every
                // other tick: gone this tick, back the next.
                Seam.onRing (RoomPos.pos tile)
                || creep.Fatigue = 0 && (creep.Walking || shoves creep))))
    |> Set.ofList

/// The Atlas a decide prices from, its tables recalled off the memo and each
/// room's evicted where its census moved, beside the per-room and flat
/// signatures and the tables handed to it.
let private recallAtlas (view: ColonyView) (memo: PlanMemo option) =
    let signed = signaturesOf view memo
    let signedRooms = signed.Rooms

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
                DarkRead = m.DarkRead
            }
        | None -> FarFieldMemo.walks (), FarFieldMemo.empty ()

    let atlas = Atlas.ofViewRecalling walks farFields view

    // The rooms whose census moved since the tables were filled, including a
    // room on one side only, and every room an entry was flooded over out of
    // sight that vision answers for again (#666). Evicted before any flood is
    // forced. Read off the memo's per-room stamp and not settled by the
    // plan's recall: on a deferred turn the two are different ticks', and a
    // recalled plan carrying tables a dark tick filled is exactly #372.
    match memo with
    | Some m ->
        let censused =
            if m.RoomSignatures = signedRooms then
                Set.empty
            else
                Set.union
                    (Map.keys m.RoomSignatures |> Set.ofSeq)
                    (Map.keys signedRooms |> Set.ofSeq)
                |> Set.filter (fun room ->
                    Map.tryFind room m.RoomSignatures <> Map.tryFind room signedRooms)

        let moved = Set.union censused (Atlas.litRooms atlas)

        if not moved.IsEmpty then
            Atlas.evictRooms atlas moved

        Atlas.evictAvoided atlas m.AvoidRooms
        Atlas.evictSides atlas m.FightRooms
    | None -> ()

    signed, walks, farFields, atlas

/// Whether the memo's plan is owed for a room it was paid blind to (#666):
/// one of `PaidDark` is out of sight no more, this tick or since. Latched on
/// every memo handed on, whatever the turn, so the colony's next turn pays it
/// though the room be dark again by then.
let private dueOf (view: ColonyView) (memo: PlanMemo) : bool =
    memo.Due
    || memo.PaidDark |> Set.exists (fun room -> not (ColonyView.dark view room))

/// A memo handed on with this tick's stamps: the per-room signatures, what
/// vision read of each room, whether its plan is owed, the rooms a
/// non-fighter goes round and the rooms the fight alone projects.
let private stamped (view: ColonyView) (signed: Signed) (memo: PlanMemo) : PlanMemo =
    { memo with
        RoomSignatures = signed.Rooms
        RoomCensuses = signed.Censuses
        Due = dueOf view memo
        AvoidRooms = view.Spatial.AvoidRooms
        FightRooms = view.Spatial.FightRooms
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

/// Whether a holder of the colony is called to the kill in a room this tick
/// (`killCalls`, the Matcher's own selection, read before the match by the
/// gate's refusals a call can meet, `callAdmits`): a trespass's kill no
/// holder is called to is cast for (#652). Selected on first read.
let private calledTo
    (view: ColonyView)
    atlas
    (fight: FightPass)
    (assignments: Assignments)
    : string -> bool =
    let called =
        lazy
            (killCalls
                view
                atlas
                fight.Threats
                fight.Pool.Value
                assignments
                (callAdmits view atlas fight.Threats assignments)
             |> Map.values
             |> Set.ofSeq)

    fun room -> called.Value.Contains room

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
    let glued =
        glue view atlas threats assigned (glueChases view atlas threats assigned)

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
/// those Tasks, each world Task's caps, its owner's, and the fight asks it
/// casts for (`Caster.assign`). Lives within the tick.
[<NoEquality; NoComparison>]
type private WorldFight =
    {
        Briefs: Map<Task, Brief>
        Tasks: Task list
        Caps: Map<Task, Capacity>
        /// None for a colony alone, which casts its own asks.
        Casts: Cast list option
    }

/// A colony alone in the world: its own fight.
let private ownFight =
    {
        Briefs = Map.empty
        Tasks = []
        Caps = Map.empty
        Casts = None
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
        /// What it asks the world's Caster for (`Quota.castsOf`), on first
        /// read.
        Casts: Lazy<Cast list>
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

/// A Brief's place down `BriefKind.order`, a Task of its own first; a
/// contested room's raid pursued (#510) in a chase's place, ahead of another
/// colony's raid there; a contested room's prey after its raid or its quiet
/// look (#656), as one colony's raid there was ahead of another's kill.
let private kindRank (brief: Brief) =
    let placeOf kind =
        BriefKind.order |> List.tryFindIndex ((=) kind) |> Option.defaultValue 0

    match Brief.flavourOf brief with
    | Some _ when Brief.pursued brief -> 2 * placeOf BriefKind.Chase
    | Some kind -> 2 * placeOf kind + (if Brief.isPrey brief then 1 else 0)
    | None -> 0

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
        /// Every body's: what the fight, the Emitter and the mover read.
        View: ColonyView
        /// The economy's (`economyOf`): what the plan, the pool and the
        /// quotas read.
        Economy: ColonyView
        Atlas: Atlas.Atlas
        Plan: PlanMemo
        Held: HeldTaskFacts
        Outposts: OutpostFacts
        Fight: FightPass
        /// Whether a holder is called to a room's kill (`calledTo`).
        Calls: string -> bool
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
        /// The fight asks it casts for (`Quotas.Casts`).
        Casts: Cast list
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
    // Every body in the view the fight, the Emitter and the mover read; the
    // economy's own beside it, which sees no fighter.
    let drawn = withDrawn view assignments
    let economy = economyOf drawn
    let view = ColonyView.fought drawn

    // The signature is read before the Atlas is built, because the Atlas is
    // one of the things it decides: the plan is recalled whole on the flat
    // signature, the tables beside it per room on the per-room half.
    let signed, walks, farFields, atlas = recallAtlas view memo

    let recalled =
        memo
        |> Option.filter (fun m -> m.Signature = signed.Signature && not (dueOf view m))

    // The per-room signatures, the census each signs and the rooms a
    // non-fighter goes round, stamped on every memo handed on: the tables
    // are this tick's whatever the plan is.
    let stampedOn = stamped view signed

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
        | Some m -> stampedOn m
        // A colony whose turn has not come (#357) serves its stale plan but
        // carries the tick's own tables on it, stamped with this tick's
        // per-room signatures: a census that moves and moves back recalls the
        // plan and evicts the entries the intermediate tick laid (#372).
        | None when turn = ReplanTurn.Waiting ->
            match memo with
            | Some stale ->
                stampedOn
                    { stale with
                        Walks = walks
                        SeamWalks = farFields.SeamWalks
                        FarFields = farFields.PerCensus
                        Narrowed = farFields.Narrowed
                        Lines = farFields.Lines
                        RoadFloods = farFields.RoadFloods
                        CreepFloods = farFields.CreepFloods
                        DarkRead = farFields.DarkRead
                    }
            | None -> stampedOn (PlanMemo.deferred signed.Rooms signed.Censuses walks farFields)
        // A plan paid this tick read a dark room's projection, not its last
        // sighting (#666): it is owed again once the room is seen.
        | None ->
            let (siteIntents,
                 servedFootings,
                 unservedFootings,
                 unroutedTrunks,
                 deferredContainers,
                 outpostRoads,
                 _) =
                planLayout economy atlas

            let quota, demandRows, load = haulerDemandOf economy atlas

            {
                Signature = signed.Signature
                RoomSignatures = signed.Rooms
                RoomCensuses = signed.Censuses
                PaidDark = signed.Remembered
                Due = false
                Paid = view.Time
                AvoidRooms = view.Spatial.AvoidRooms
                FightRooms = view.Spatial.FightRooms
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
                DarkRead = farFields.DarkRead
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
            Economy = economy
            Atlas = atlas
            Plan = plan
            Held = held
            Outposts = outposts
            Fight = fight
            Calls = calledTo view atlas fight assignments
            Verbose = verbose
        }

    {
        Home = SpatialInfo.homeName view.Spatial
        View = view
        Threats = fight.Threats
        FightTasks = fight.Tasks
        Casts = lazy (castsOf view fight.Threats outposts.Guarded head.Calls)
        Fighters = economy.Fighters
        Join = joinWhole head
    }

/// A whole decide joined to the world's fight (`Joining`).
and private joinWhole (head: WholeHead) (world: WorldFight) : Joining =
    let view, economy, atlas, plan = head.View, head.Economy, head.Atlas, head.Plan
    let held, fight = head.Held, head.Fight
    let threats = fight.Threats
    let fought = foughtThreats threats world

    // The fighting rows cast for the Caster's share of the world's asks
    // where it handed one, else for the colony's own, a trespass's kill
    // priced as its own cast (`ownCasts`).
    let casts =
        world.Casts
        |> Option.defaultWith (fun () -> ownCasts view threats head.Outposts.Guarded head.Calls)

    let outposts =
        { head.Outposts with
            Casts = Some casts
        }

    // The outpost's source containers, beside the memoised Layout and never
    // inside it: derived fresh every tick for the reason on the rule itself.
    let outpostContainers, waitingContainers = planOutpostContainers economy atlas

    let outpostSiteIntents = outpostContainers @ planFirstSpawns economy atlas

    let defenseIntents = fight.Towers.Value

    // The consignment's send (#349), beside the defence reflexes because it is
    // the same kind of thing: a structure's own verb, read off the view.
    let consigned = planConsignment economy

    let sendingIn intents =
        intents
        |> List.choose (function
            | SendFromTerminal(id, _, _, _) -> Some id
            | _ -> None)
        |> Set.ofList

    let aidIntents = planAid economy (sendingIn consigned)

    // The lab cluster's ore, behind the Thorium and the aid: one send a
    // terminal a tick.
    let oreSends = planOreSend economy (sendingIn (consigned @ aidIntents))

    let consignmentIntents =
        consigned
        @ aidIntents
        @ oreSends
        @ planReaction economy
        @ planObserver economy
        @ planExtractorMove economy atlas

    // The pool is derived before the spawns, and the dependency runs one way:
    // the worker row's floor asks the pool, and nothing in the pool reads a
    // spawn Intent.
    let sizing = rowSizingOf economy atlas threats outposts held

    // The fight pass, carved out of the match: the fighters
    // (`isFighterBody`) are kept and matched over Flee and the combat Tasks
    // first, every other body over Flee and the economy's after them. It
    // reads the view's fighters, the Atlas, the Threats and their Briefs,
    // `held`, the guarded outposts and the row sizing (a relief's lead);
    // it leaves the fighters' assignments and Verdicts. The structures' fight
    // (`defenseIntents`), the Emitter, the reflexes and the one Resolver
    // still run over both passes' bodies together. The whole list is
    // `planTasks`'s: the fight's at its head.
    let tasks = fight.Tasks.Value @ planEconomyTasks economy atlas held outposts

    // Why the colony is busy (#523), once a tick for the upgrader row and the
    // buffer's rank.
    let busy = busyReasons economy tasks (not (List.isEmpty aidIntents))

    // Every entry's priority and capacity, set once here and read by the
    // Matcher and the mover: the fight pass's off the Briefs, the economy's
    // after them, and the whole pool the two in pool order (`planPools`). The
    // fight pass's over the world's combat Tasks too, each capped as its
    // owner caps it (`worldPool`).
    let fightTasks = fight.Tasks.Value @ world.Tasks
    let fightPool = worldPool fought fightTasks world fight.Pool

    let economyPool =
        planPoolHolding held economy atlas busy (List.filter (safetyTier >> not) tasks)

    let pool = fightPool @ economyPool

    // A far field whose Task is not in the tick's list is dead weight the
    // census never evicts (#392). Every far leg priced this tick — by the
    // Matcher off the pool, by the mover off the assignments — is a Task in
    // this list; a graced crossing (#151) prices none.
    Atlas.evictFarFieldsExcept atlas (Set.ofList (tasks @ world.Tasks))

    // The overflow term rides beside the memoised flow number and never
    // inside it: it reads the containers' stock, which moves every tick.
    let overflow = haulOverflowOf economy atlas plan.HaulerDemand

    let spawnIntents, quotas =
        planSpawns
            economy
            atlas
            outposts
            sizing
            threats
            tasks
            busy
            (withOverflow plan.HaulerQuota overflow)

    let fighters, economyBodies = economy.Fighters, economy.Creeps

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
            Casts = casts
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
        // is no work there for it to wait for. A fighter walks to its
        // caster's home (`ColonyView.idleHome`).
        let homeward =
            view.Creeps
            |> List.filter (fun creep -> not (Map.containsKey creep.Name assigned))
            |> List.choose (fun creep ->
                let fighter = isFighterBody creep

                let target =
                    if fighter then
                        ColonyView.idleHome view creep.Name
                    else
                        home

                Atlas.creepRoom atlas creep.Name
                |> Option.filter (fun room ->
                    room <> target && (fighter || Set.contains room view.Evacuating))
                |> Option.map (fun _ -> creep.Name, target))

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
                Casts = tail.Casts
            }
        HaulOverflow = overflow
        OutpostRooms = outposts.Declared
        WaitingContainers = waitingContainers
        Steps = steps
        Tracks = Atlas.tracks atlas
        Forces = forces view threats assigned
        Next = nextPours @ List.choose List.head arrivals
        Later =
            [
                for light in 2 .. view.Tuning.Cadence - 1 ->
                    laterOf pours light
                    @ List.choose (List.tryItem (light - 1) >> Option.flatten) arrivals
            ]
        Aims = aims view atlas threats assigned
        Volleys = volleys view atlas threats assigned
        Mired = mired view atlas threats assigned
        Drawn =
            view.Creeps
            |> List.choose (fun creep ->
                creep.DrawnFrom |> Option.map (fun store -> creep.Name, store))
            |> Map.ofList
        Fight =
            {
                Fighters = fighters |> Seq.map _.Name |> Fresh.setOfSeq
            }
        Economy =
            {
                Sizing = carriedSizing sizing
                Asks = castAsks view own outposts.Guarded head.Calls
                Economy = economyBodies |> Seq.map _.Name |> Fresh.setOfSeq
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
                Unfled =
                    (match pool |> List.tryFind (fun pooled -> pooled.Task = Flee) with
                     | None -> Fresh.setOfSeq Seq.empty
                     | Some flee ->
                         economyBodies
                         |> Seq.filter (fun creep ->
                             Map.tryFind creep.Name assigned <> Some Flee
                             && applicable view own atlas creep flee)
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
/// structures' fight; its movement registers the fighters alone. Which
/// fighters it holds is no economy fact: one adopted or let go since the
/// carry is the world fight's, and the economy replays.
///
/// None where the economy would decide otherwise, so the caller decides the
/// colony whole this tick: an economy body new since the carry (a fighter
/// disarmed across the line), its own cast asks moved, an economy body a
/// Reach has walled off from its Task since (`walledOffBy`) or one Flee
/// applies to that the carry held on other work, a body its
/// share of the world's asks (`Caster.assign`; its own, alone) owes a spawn
/// standing idle — spawning is the economy's —, or a fighter's step an
/// economy body stands in. The last two are read once the world's fight is
/// laid in, so the opening is None for the rest and the joined decide's
/// `Finish` None for them.
let rec private openFight
    (view: ColonyView)
    (assignments: Assignments)
    (verbose: Set<string>)
    (memo: PlanMemo)
    (carry: EconomyCarry)
    : Opening option =
    let drawn = withDrawn view assignments
    let economy = economyOf drawn
    let view = ColonyView.fought drawn
    let fighters, economyBodies = economy.Fighters, economy.Creeps

    if economyBodies |> List.map _.Name |> Set.ofList <> carry.Economy then
        None
    else

        let signed, _, _, atlas = recallAtlas view (Some memo)
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

        // An economy body Flee applies to now that the carry left on other
        // work (#669): only a whole decide's Matcher moves it to Flee, and its
        // replay would walk it on under fire until the next one. A body the
        // carry holds on Flee replays that flight; one it left off a Flee that
        // applied (`carry.Unfled`) would be left off it again.
        let fleesAnew =
            lazy
                (fight.Pool.Value
                 |> List.tryFind (fun pooled -> pooled.Task = Flee)
                 |> Option.exists (fun flee ->
                     economyBodies
                     |> List.exists (fun creep ->
                         Map.tryFind creep.Name carry.Held <> Some Flee
                         && not (Set.contains creep.Name carry.Unfled)
                         && applicable view threats atlas creep flee)))

        let calls = calledTo view atlas fight assignments

        if
            castAsks view threats guarded calls <> carry.Asks
            || walledOffAnew.Force()
            || fleesAnew.Force()
        then
            None
        else
            Some
                {
                    Home = SpatialInfo.homeName view.Spatial
                    View = view
                    Threats = threats
                    FightTasks = fight.Tasks
                    Casts = lazy (castsOf view threats guarded calls)
                    Fighters = fighters
                    Join = joinFight view economy assignments verbose memo carry signed atlas fight
                }

/// A fight-only decide joined to the world's fight (`Joining`): every body's
/// view, and the economy's (`economyOf`).
and private joinFight
    (view: ColonyView)
    (economy: ColonyView)
    (assignments: Assignments)
    (verbose: Set<string>)
    (memo: PlanMemo)
    (carry: EconomyCarry)
    (signed: Signed)
    atlas
    (fight: FightPass)
    (world: WorldFight)
    : Joining =
    let fighters, economyBodies = economy.Fighters, economy.Creeps
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
        // room, else its caster's home for an idle one standing away.
        let crossings =
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
                    let target = ColonyView.idleHome view creep.Name

                    Atlas.creepRoom atlas creep.Name
                    |> Option.filter (fun room -> room <> target)
                    |> Option.map (fun _ -> creep.Name, target))

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

        // A body its share of the world's asks owes an idle spawn: its
        // own asks alone (`carry.Asks`) where no Caster weighed them.
        let owed () =
            castOwed
                view
                (match world.Casts with
                 | Some share -> share |> List.map _.Ask
                 | None -> carry.Asks)

        if owed () || blocked then
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
                    Memo = stamped view signed memo
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
                    Tracks = Atlas.tracks atlas
                    Forces = forces view threats assigned
                    Next = []
                    Later = []
                    Aims = aims view atlas threats assigned
                    Volleys = volleys view atlas threats assigned
                    Mired = mired view atlas threats assigned
                    Drawn =
                        view.Creeps
                        |> List.choose (fun creep ->
                            creep.DrawnFrom |> Option.map (fun store -> creep.Name, store))
                        |> Map.ofList
                    Fight = { Fighters = Fresh.setOfSeq fighting }
                    Economy = carry
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
    (carry: EconomyCarry)
    : Decision option =
    openFight (Operations.layOne assignments view) assignments verbose memo carry
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
    // added staggers colonies against each other. Its sweep laid in as the
    // world's tick lays it (`judgeColonies`), with no stored latch.
    let view =
        { Operations.layOne assignments view with
            Leaving =
                leavingTiles
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
        /// Crossings from a colony's home to a room: a Task's owner where
        /// no colony declares it (`worldFights`), and a cast's walk
        /// (`Caster.assign`).
        Hops: string -> string -> int option
        /// Last tick's assignments, every colony's in one table.
        Holding: Assignments
        Verbose: Set<string>
        /// Each colony's plan memo, by home.
        PlanMemos: Map<string, PlanMemo>
        /// Each colony's last whole decide's economy carry, by home.
        Carries: Map<string, EconomyCarry>
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
        /// The code's operations' records as last stored (#663 S7c,
        /// `Memory.fabot.ops.code`): the latch each moves on from.
        Operations: OpRecord list
        /// The code's operation specs the shell admitted
        /// (`Operations.declared`): the limits each runs under.
        Declared: OpSpecWire list
    }

/// A colony's own step that threw (#628): the colony decides nothing this
/// tick and every other colony decides on. `Phase` names the step: `fight`
/// and `open` its opening, `join` its share of the world's fight, `finish`
/// its match and emit, `handed` a fight-only decide handed back whole.
[<NoEquality; NoComparison>]
type ColonyFault =
    {
        /// The faulted colony's home.
        Colony: string
        Phase: string
        Error: exn
    }

/// The name the world lane's own steps are stamped under
/// (`ColonyDecisions.Lane`): no room is named it.
[<Literal>]
let worldLane = "world"

/// One tick's decisions over every colony.
type ColonyDecisions<'Stamp> =
    {
        /// Every colony with its view, judged and its observer aimed, in
        /// colony order.
        Views: (Colony * ColonyView) list
        /// Each decision in the order it was made — the fight-only ones
        /// first —, with the view it read and the stamp taken after it.
        Decided: (Colony * ColonyView * Decision * 'Stamp) list
        /// The stamps taken before the first decision was made (#666), in
        /// the order taken: the world's own steps under `worldLane` — the
        /// judging, the observers' aim, the world's fight, the casting and
        /// the one fleet match —, each colony's opening and join under its
        /// home. Without them every one of these was charged to the first
        /// colony to decide.
        Lane: (string * 'Stamp) list
        /// The colonies that decided their fight alone.
        FoughtAlone: Set<string>
        /// The turn the whole decides were handed, -1 where it waits.
        Turn: int
        /// Whether the turn's colony decided no plan this tick, so the turn
        /// stays its. A colony that faulted on its turn spent it.
        TurnWaits: bool
        /// The colonies whose own step threw, each with no decision this
        /// tick, in the order they threw; then each whose towers' pass
        /// threw too (`towers`).
        Faulted: ColonyFault list
        /// Each faulted colony's towers, safe mode and rampart doors
        /// (`towersAlone`): no replay repeats a structure's fight.
        Towers: (string * Intent list) list
        /// Whether every colony opened a whole decide this tick: none
        /// replayed, none fought alone, none threw. Only such a tick is
        /// folded into the operation ledger.
        Whole: bool
        /// What the operation ledger reads of this tick's decide
        /// (`Ledger.observe`), on first read: the shell's alone, after
        /// every decision, on a tick every colony fought.
        Operations: Lazy<Ledger.Observed>
        /// The code's operations' records moved on this tick
        /// (`Operations.lay`), and what moved: the shell stores them, the
        /// latch the next tick moves on from.
        Operated: OpRecord list * OpEvent list
    }

/// One colony's step run alone: its throw is its own fault, handed back
/// beside the other colonies' decisions rather than through them.
let private alone (home: string) (phase: string) (step: unit -> 'a) : Result<'a, ColonyFault> =
    try
        Ok(step ())
    with error ->
        Error
            {
                Colony = home
                Phase = phase
                Error = error
            }

/// The steps that ran, and the faults of those that threw.
let private sorted (results: Result<'a, ColonyFault> list) : 'a list * ColonyFault list =
    results
    |> List.choose (function
        | Ok ran -> Some ran
        | Error _ -> None),
    results
    |> List.choose (function
        | Error fault -> Some fault
        | Ok _ -> None)

/// Each raided room's one verdict, priced once over the fleet and the
/// world's vision and laid in each view (`Facts.judge`): a colony that does
/// not decide (`idle`) sends only the bodies its plan already holds there.
/// The operations laid in first (`Operations.lay`, moved on from their
/// stored records), so the verdicts price the room each works and no other
/// candidate. And each contested room's one danger (`WorldDanger.lay`). The
/// records come back moved on, with what moved.
let judgeColonies
    (hops: string -> string -> int option)
    (assignments: Assignments)
    (idle: Set<string>)
    (declared: OpSpecWire list)
    (records: OpRecord list)
    (colonies: (Colony * ColonyView) list)
    : ColonyView list * (OpRecord list * OpEvent list) =
    let laid, records, events =
        colonies |> List.map snd |> Operations.lay declared assignments records

    List.zip colonies laid
    |> List.map (fun ((colony, _), view) ->
        ({
            Cut = view
            Owns = Colony.declaredRooms colony
            Decides = not (Set.contains colony.Home idle)
        }
        : Facts.Judged))
    |> Facts.judge hops assignments
    |> WorldDanger.lay,
    (records, events)

/// ADR-0091
/// The world's fight, one Brief per combat Task world-wide: of the colonies
/// that pool a Guard or a Defend, its owner's — a home's Defend the home's;
/// a Guard its declarer's, else the fewest crossings from it, then by home.
/// Each Task is capped as its owner caps it, whichever colony pools it, so
/// one count of holders over the fleet fills it. A colony with a fighter is
/// handed the owner's Brief of each such Task it pools none of in a room it
/// projects. An Assault stays its declarer's alone. Beside each colony's
/// fight, each Task's owner, whose ask the Caster weighs (`Caster.assign`);
/// no colony's casts yet. And each Task's owner's Brief with its view, as
/// the operation ledger reads them (`Ledger.observe`).
let private worldFights
    (hops: string -> string -> int option)
    (openings: (Colony * Opening) list)
    : Map<string, WorldFight> * Map<Task, string> * (Task * Brief * ColonyView) list =
    let claims =
        openings
        |> List.collect (fun (colony, opening) ->
            opening.FightTasks.Value
            |> List.choose (fun task ->
                match task with
                | Guard room
                | Defend room
                | Operate(_, room) ->
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

            let owner =
                (match task with
                 | Defend home ->
                     ranked |> List.tryFind (fun (_, colony, _, _) -> colony.Home = home)
                 | _ -> None)
                |> Option.orElse (
                    ranked
                    |> List.tryFind (fun (_, colony, _, _) ->
                        Set.contains room (Colony.declaredRooms colony))
                )
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
                | Defend at
                | Operate(_, at) when at = room -> Threats.briefFor opening.Threats task
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
            Casts = None
        })
    |> Map.ofList,
    owned |> List.map (fun (task, (_, owner, _, _), _) -> task, owner) |> Map.ofList,
    owned
    |> List.choose (fun (task, (_, owner, brief, _), _) ->
        openings
        |> List.tryFind (fun (colony, _) -> colony.Home = owner)
        |> Option.map (fun (_, opening) -> task, brief, opening.View))

/// A faulted colony's structures' fight alone (#628): its towers' fire and
/// heal, safe mode and the rampart doors, off its view and a fresh Atlas,
/// as a whole decide's fight pass lays them (`fightPass`'s `Towers`). No
/// memo is read: the faulted decide's may be what threw.
let private towersAlone (view: ColonyView) (assignments: Assignments) : Intent list =
    let view = ColonyView.fought (withDrawn view assignments)
    let atlas = Atlas.ofView view
    let held = heldTaskFacts view assignments

    planSafeMode view atlas held
    @ planFire view atlas
    @ planTowerHeal view atlas
    @ planRampartDoors view

/// The world lane's tick: every deciding colony opened —
/// the fight-only ones (`openFight`) off the judged views, every other whole
/// (`openWhole`) off the aimed ones —, the world's fight laid in
/// (`worldFights`), one match over every fighter of every colony
/// (`matchAcross`, a colony that decides nothing counting the holdings its
/// fighters keep), then each colony's economy matched from the holdings its
/// fighters left, and each decide finished, its fighters' acts and steps
/// its own. The world's fight is decided once: a fight-only colony a
/// fighter's step hands back to a whole decide is decided whole off that
/// match.
let private decideWorld
    (inputs: ColonyInputs)
    (stamp: unit -> 'Stamp)
    (colonies: (Colony * ColonyView) list)
    (judged: ColonyView list)
    (operated: OpRecord list * OpEvent list)
    : ColonyDecisions<'Stamp> =
    // The world lane's stamps, observe-only: the judging before this is the
    // world's.
    let lane = ResizeArray<string * 'Stamp>()
    let stampAs (whose: string) = lane.Add((whose, stamp ()))
    stampAs worldLane

    let fought, fightFaults =
        List.zip colonies judged
        |> List.filter (fun ((colony, _), _) -> Set.contains colony.Home inputs.FightAlone)
        |> List.map (fun ((colony, _), view) ->
            let opened =
                alone colony.Home "fight" (fun () ->
                    Option.map2
                        (fun memo carry ->
                            openFight view inputs.Holding inputs.Verbose memo carry)
                        (Map.tryFind colony.Home inputs.PlanMemos)
                        (Map.tryFind colony.Home inputs.Carries)
                    |> Option.flatten
                    |> Option.map (fun opening -> colony, view, opening))

            stampAs colony.Home
            opened)
        |> sorted

    let fought = List.choose id fought

    let fightOnly = fought |> List.map (fun (colony, _, _) -> colony.Home) |> Set.ofList

    // A colony whose fight threw opens nothing whole either: it decides
    // nothing this tick.
    let fightFaulted = fightFaults |> List.map _.Colony |> Set.ofList

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

    stampAs worldLane

    // The turn is bound and is a DU (`ReplanTurn`), because the first
    // shape shipped broken: as an expression inside the call with a
    // `bool` last argument, the turn arrived as JavaScript `undefined`,
    // `not undefined` is `true`, and no layout was planned at all
    // (#357). Only `npm run profile` caught it.
    let whole, openFaults =
        views
        |> List.indexed
        |> List.filter (fun (_, (colony, _)) ->
            not (Set.contains colony.Home inputs.Idle)
            && not (Set.contains colony.Home fightOnly)
            && not (Set.contains colony.Home fightFaulted))
        |> List.map (fun (index, (colony, view)) ->
            let whose =
                match inputs.TurnIndex with
                | Some _ when index <> turn -> ReplanTurn.Waiting
                | _ -> ReplanTurn.Now

            let opened =
                alone colony.Home "open" (fun () ->
                    colony,
                    view,
                    openWhole
                        view
                        inputs.Holding
                        inputs.Verbose
                        (Map.tryFind colony.Home inputs.PlanMemos)
                        whose)

            stampAs colony.Home
            opened)
        |> sorted

    let opened = fought @ whole

    // Every colony that opened no decide for a throw: its fighters keep
    // their Tasks as an idle colony's do.
    let unopened = fightFaults @ openFaults |> List.map _.Colony |> Set.ofList

    let fights, owners, briefs =
        worldFights inputs.Hops (opened |> List.map (fun (colony, _, opening) -> colony, opening))

    stampAs worldLane

    // Who casts each of the world's fight asks (`Caster.assign`): every
    // colony's fleet and oven read, an idle colony asking nothing. A
    // fight-only colony handed a body its spawn owes is decided whole
    // (`joinFight`).
    let casts =
        List.zip colonies judged
        |> List.map (fun ((colony, _), view) ->
            match opened |> List.tryFind (fun (decided, _, _) -> decided.Home = colony.Home) with
            | Some(_, _, opening) ->
                ({
                    Home = colony.Home
                    View = opening.View
                    Asks = opening.Casts.Value
                }
                : Caster.Bidder)
            | None ->
                {
                    Home = colony.Home
                    View = ColonyView.fought view
                    Asks = []
                })
        |> Caster.assign inputs.Hops inputs.Holding owners

    stampAs worldLane

    let fights =
        fights
        |> Map.map (fun home fight ->
            { fight with
                Casts = Some(Map.tryFind home casts |> Option.defaultValue [])
            })

    let joined, joinFaults =
        opened
        |> List.map (fun (colony, view, opening) ->
            let joining =
                alone colony.Home "join" (fun () ->
                    colony, view, opening, opening.Join fights.[colony.Home])

            stampAs colony.Home
            joining)
        |> sorted

    // The fighters of a colony that decides nothing keep their Tasks,
    // counted against the world's caps: an idle colony's, and one whose
    // opening or join threw.
    let unjoined = joinFaults |> List.map _.Colony |> Set.ofList |> Set.union unopened

    let bystanders =
        colonies
        |> List.filter (fun (colony, _) ->
            Set.contains colony.Home inputs.Idle || Set.contains colony.Home unjoined)
        |> List.collect (fun (_, view) -> ColonyView.bodies view |> List.filter isFighterBody)

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

    stampAs worldLane

    // A colony whose finish throws has had its fighters matched in the one
    // fleet match above: the Tasks they won there are lost with its
    // decision, and the shell replays its last plan for them instead.
    let finished, finishFaults =
        joined
        |> List.map (fun (colony, view, opening, joining) ->
            alone colony.Home "finish" (fun () ->
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
                colony, view, decision, decision |> Option.map (fun _ -> stamp ())))
        |> sorted

    // A fight-only colony whose fighter's step an economy body stands in
    // is decided whole off the one match: its fighters keep what the
    // world's fight gave them, its economy is matched from the holdings
    // they left. Off its aimed view, which holds no look of its observer
    // this tick, and on the turn as it fell.
    let handed, handedFaults =
        views
        |> List.indexed
        |> List.filter (fun (_, (colony, _)) ->
            finished
            |> List.exists (fun (decided, _, decision, _) ->
                decided.Home = colony.Home && Option.isNone decision))
        |> List.map (fun (index, (colony, view)) ->
            alone colony.Home "handed" (fun () ->
                let whose =
                    match inputs.TurnIndex with
                    | Some _ when index <> turn -> ReplanTurn.Waiting
                    | _ -> ReplanTurn.Now

                let opening =
                    openWhole
                        view
                        inputs.Holding
                        inputs.Verbose
                        (Map.tryFind colony.Home inputs.PlanMemos)
                        whose

                let joining = opening.Join fights.[colony.Home]

                let fighters =
                    opening.Fighters
                    |> List.choose (fun creep ->
                        Map.tryFind creep.Name fleet.Assigned
                        |> Option.map (fun tid -> creep.Name, tid))
                    |> Map.ofList

                let economy =
                    joining.Economy
                    |> Option.map (fun share ->
                        matchAcross
                            [ joining.Context ]
                            []
                            fighters
                            [ [ 0, share ] ]
                            inputs.Holding
                            inputs.Verbose)

                match joining.Finish(fleet :: Option.toList economy) with
                | Some decision -> colony, view, decision, stamp ()
                | None -> invalidOp "A whole decide is never handed back"))
        |> sorted

    let faults = fightFaults @ openFaults @ joinFaults @ finishFaults @ handedFaults

    let faulted = faults |> List.map _.Colony |> Set.ofList

    // Each faulted colony's towers and safe mode, under a guard of their own:
    // a home under a raid keeps its structures' fight while its decide
    // throws, and a throw here costs that pass alone.
    let towers, towerFaults =
        views
        |> List.filter (fun (colony, _) -> Set.contains colony.Home faulted)
        |> List.map (fun (colony, view) ->
            alone colony.Home "towers" (fun () -> colony.Home, towersAlone view inputs.Holding))
        |> sorted

    {
        Views = views
        Decided =
            (finished
             |> List.choose (fun (colony, view, decision, stamped) ->
                 Option.map2
                     (fun decision stamped -> colony, view, decision, stamped)
                     decision
                     stamped))
            @ handed
        Lane = List.ofSeq lane
        FoughtAlone =
            handed
            |> List.fold (fun alone (colony, _, _, _) -> Set.remove colony.Home alone) fightOnly
            |> fun alone -> Set.difference alone faulted
        Turn = turn
        TurnWaits = turnWaits
        Faulted = faults @ towerFaults
        Towers = towers
        Whole = List.length whole = List.length colonies
        Operations = lazy (Ledger.observe judged briefs fleet.Assigned)
        Operated = operated
    }

/// Every colony's tick decided: the views judged (`judgeColonies`), then the
/// world's fight and each colony's economy (`decideWorld`), the fight-only
/// colonies opened first, so the turn and the observers' looks read the
/// colonies that did fight alone — the fight reads no observer's look.
/// `stamp` is read after each decision. A colony's own step that throws
/// costs that colony its decision (`Faulted`); a throw in a step every
/// colony shares (the judging, the world's fight, the one match) is the
/// tick's.
let decideColonies
    (inputs: ColonyInputs)
    (stamp: unit -> 'Stamp)
    (colonies: (Colony * ColonyView) list)
    : ColonyDecisions<'Stamp> =
    // Every view on the one set of tiles our bodies leave this tick. Laid
    // before any colony faults: a faulted colony's creeps may count as
    // shovable here though they replay their plan's steps (#628).
    let leaving =
        colonies
        |> List.map (fun (colony, view) -> colony.Home, view)
        |> leavingTiles inputs.Holding inputs.Idle inputs.FightAlone

    let colonies =
        colonies
        |> List.map (fun (colony, view) -> colony, { view with Leaving = leaving })

    let judged, operated =
        judgeColonies
            inputs.Hops
            inputs.Holding
            inputs.Idle
            inputs.Declared
            inputs.Operations
            colonies

    decideWorld inputs stamp colonies judged operated
