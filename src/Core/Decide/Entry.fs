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

    let joined (ids: ResizeArray<string>) =
        ids |> List.ofSeq |> List.sort |> String.concat ";"

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
            let standingIds = ResizeArray<string>()
            let pendingIds = ResizeArray<string>()
            let mineralIds = ResizeArray<string>()

            // The Thorium deposits on their tiles: the Layout reads them
            // twice (the extractor's tile and the mineral container's Seats)
            // and `Atlas.workingGroundIn` at every level. The mod deletes an
            // exhausted deposit outright, and unsigned, the memo would keep
            // handing back a plan naming a container on the Seat of a rock
            // that is gone.
            for KeyValue(id, tile) in layer.TargetPositions do
                match Map.tryFind id spatial.TargetKinds with
                | Some(Structure built) -> standingIds.Add $"{built}@{room}:{tile.X},{tile.Y}"
                | Some(Site built) -> pendingIds.Add $"{built}@{room}:{tile.X},{tile.Y}"
                | Some(Mineral _) -> mineralIds.Add $"Mineral@{room}:{tile.X},{tile.Y}"
                | _ -> ()

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

            $"{home}|{level}|{stage}|{taken}|{joined standingIds}|{joined pendingIds}|{rivals}|{joined mineralIds}")
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

/// The decision seam: a colony view in, Decision out, with this colony's
/// movement left unarbitrated on it — a room's traffic is not one colony's
/// decision, so the caller folds every colony's together and arbitrates each
/// room once (`resolveRooms`). Plan, match, emit, move, beside the colony steps
/// (spawns, sites), all pricing from one Atlas built up front.
let decideUnarbitrated
    (view: ColonyView)
    (assignments: Assignments)
    (verbose: Set<string>)
    (memo: PlanMemo option)
    (turn: ReplanTurn)
    : Decision =
    // Where each body's load came from, settled before anything reads a body.
    let view =
        { view with
            Creeps =
                view.Creeps
                |> List.map (fun creep ->
                    { creep with
                        DrawnFrom = drawnFrom view assignments creep
                    })
        }

    let signedRooms, signature = signaturesOf view
    // The signature is read before the Atlas is built, because the Atlas is
    // one of the things it decides: the plan is recalled whole on the flat
    // signature, the tables beside it per room on the per-room half.
    let recalled = memo |> Option.filter (fun m -> m.Signature = signature)

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

    // The tick's Threats, derived once and shared by every reader.
    let threats = threatsOfHeld view atlas held

    // The outpost's source containers, beside the memoised Layout and never
    // inside it: derived fresh every tick for the reason on the rule itself.
    let outpostContainers, waitingContainers = planOutpostContainers view atlas

    let outpostSiteIntents = outpostContainers @ planFirstSpawns view atlas

    let defenseIntents =
        planSafeMode view atlas held
        @ planFire view atlas
        @ planTowerHeal view atlas
        @ planRampartDoors view

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
    // spawn Intent. The outpost chain's answers are derived once (#383):
    // before this `claimTargets` ran 11.4 times a tick and
    // `outpostControllers` 9.1, each walking the kind census.
    let outposts = outpostFactsOf view atlas

    let sizing = rowSizingOf view atlas threats outposts held

    let tasks = planTasks view atlas threats held outposts

    // Why the colony is busy (#523), once a tick for the upgrader row and the
    // buffer's rank.
    let busy = busyReasons view tasks (not (List.isEmpty aidIntents))

    // Every entry's priority and capacity, set once here and read by the
    // Matcher and the mover.
    let pool = planPoolHolding held view atlas threats busy tasks

    // A far field whose Task is not in the tick's list is dead weight the
    // census never evicts (#392). Every far leg priced this tick — by the
    // Matcher off the pool, by the mover off the assignments — is a Task in
    // this list; a graced crossing (#151) prices none.
    Atlas.evictFarFieldsExcept atlas (Set.ofList tasks)

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

    let next, verdicts = matchCreeps view atlas sizing threats pool assignments verbose
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

    let taskIntents = emit view atlas threats assigned

    // A task's pickup owns this creep's channel. Otherwise the reflex picks
    // its last reachable pile, preserving the engine's former last-write choice
    // without emitting overwritten calls. Different creeps may still share a pile.
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

    // The signature (#381), shaped after the pickup reflex: no walk, no Task,
    // no competition.
    let signIntents = planSignatures view atlas Colony.signatureFor

    let intents =
        defenseIntents
        @ consignmentIntents
        @ spawnIntents
        @ plan.SiteIntents
        @ outpostSiteIntents
        @ signIntents
        @ pickupIntents
        @ taskIntents
        |> Fabot.Core.IntentPlan.create
        |> function
            | Ok selected ->
                strikeReflex view atlas threats assigned selected
                |> healReflex view threats
                |> Fabot.Core.IntentPlan.intents
            | Error conflict -> invalidOp $"Conflicting creep intents: %A{conflict}"

    // A melee fighter steps onto the target it swings at (#535): its move is
    // that step, and the mover leaves its tile standing, as a tired body's.
    let glued = glue view atlas threats assigned

    let intents = intents @ glued

    let movement =
        let stepping =
            glued
            |> List.choose (function
                | MoveCreep(name, _) -> Some name
                | _ -> None)
            |> Set.ofList

        let moved = movementOf view atlas threats pool assigned crossings verbose

        if Set.isEmpty stepping then
            moved
        else
            { moved with
                Tired = Set.union moved.Tired stepping
                Intents =
                    moved.Intents
                    |> List.filter (fun step -> not (Set.contains step.Creep stepping))
            }

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
    }

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
    let decision = decideUnarbitrated view assignments verbose memo ReplanTurn.Now
    let moveIntents, moveVerdicts = resolveRooms Set.empty [ decision.Movement ]

    { decision with
        Intents = decision.Intents @ moveIntents
        Verdicts = decision.Verdicts @ moveVerdicts
    }
