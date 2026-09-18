/// A Task's identity across ticks, and the colony-level predicates read all over
/// the pipeline: the stage a room stands at, whether the colony is independent,
/// and which of its structures are hungry.
[<AutoOpen>]
module Fabot.Core.Decide.Facts

open Fabot.Core
open Fabot.Core.Types

/// The resource a Task id names, and **nothing at all for energy** (ADR 0057
/// decision 3). A Task id is a key carried across ticks — in Memory, in the
/// Assignments the anti-thrash reads back, and in every `observe` transition
/// line — so the spelling of the colony's energy work is frozen where it was:
/// widening every `withdraw:` id would orphan every standing assignment on the
/// tick the bundle carrying it is deployed, for a resource no store in this
/// colony held until the extractor stood. What a second resource needs is a
/// *distinct* id at the same store, which a suffix gives it.
let private resourceSuffix =
    function
    | Energy -> ""
    | Thorium -> ":Thorium"

/// Stable identity of a Task across ticks; what Assignments point at.
let taskId =
    function
    | Harvest sourceId -> $"harvest:{sourceId}"
    | Withdraw(storeId, resource) -> $"withdraw:{storeId}{resourceSuffix resource}"
    | Refill(structureId, resource) -> $"refill:{structureId}{resourceSuffix resource}"
    | Build siteId -> $"build:{siteId}"
    | Repair structureId -> $"repair:{structureId}"
    | Upgrade controllerId -> $"upgrade:{controllerId}"
    | Reserve controllerId -> $"reserve:{controllerId}"
    | Claim controllerId -> $"claim:{controllerId}"
    // One Reclaim per declared [[errand]], identified by the object the
    // declaration names — which is the engine's own id, so the Memory key
    // survives every tick the room is dark and the relay has gapped.
    | Reclaim reactorId -> $"reclaim:{reactorId}"
    // The same suffix on the third Task to name a resource (#311). An energy
    // pile's id is byte-identical to the one #167 froze, for `resourceSuffix`'s
    // own reason — a pile outlives a tick and an assignment to it is a Memory
    // key — and the Thorium pile beside it is a distinct id at the same tile.
    | Pickup(pileId, resource) -> $"pickup:{pileId}{resourceSuffix resource}"
    // One Flee for the whole colony: it has no target to be identified by,
    // and every creep inside a Reach is running from the same thing.
    | Flee -> "flee"
    // One Guard per raided room, identified by the room and never by what
    // stands in it (ADR 0056): a raid that loses a creep, or moves one a tile,
    // is the same fight and must be the same identity, or anti-thrash would
    // re-match the [[guard]] mid-swing.
    | Guard room -> $"guard:{room}"

/// The **target** inside a Task id: the engine's own object id, which is what
/// `taskId` writes between its first colon and the next one — a Task id has two
/// colons since ADR 0057 decision 3 put a resource on the [[withdraw]] and the
/// [[refill]], and neither an object id nor a room name has ever held one.
/// `None` for Flee, the one id naming no target. Read by the vision grace,
/// which holds an id and no Task —
/// the Task it named has left the pool — and stays blind to Task kinds doing
/// it (ADR 0052 decision 6): what it needs is the id the room's census files
/// that target under, and every kind spells that the same way.
let private taskTarget (tid: string) : string option =
    match tid.IndexOf ':' with
    | -1 -> None
    | colon ->
        let rest = tid.Substring(colon + 1)

        match rest.IndexOf ':' with
        | -1 -> Some rest
        | next -> Some(rest.Substring(0, next))

/// The vision grace's one question (#151): the room a held Task's target was
/// last seen standing in, when that room has gone **dark** inside
/// `Tuning.VisionGrace` and not longer. `None` is every other answer — the room
/// is in view this tick, it was never seen, the darkness has outlasted the
/// grace, or the id names no target at all.
///
/// A Task leaves the pool for two opposite reasons and its id alone cannot tell
/// them apart: the target was destroyed, or the room carrying it went dark. The
/// pool stays gated on vision and rightly so (ADR 0004) — a blind room hands
/// over an empty site list and no rule may price what nobody can see — so what
/// this asks is about **looking** and never about the target. Two readers, and
/// they are the two halves of one rule: the Matcher keeps the assignment, and
/// the mover walks its holder at the room the answer names (`Atlas.stepTowardRoom`).
/// Written once because a second spelling is free to disagree about which of
/// them is holding a creep still.
let internal lastSeenIn (view: ColonyView) (tid: string) : string option =
    match taskTarget tid with
    | None -> None
    | Some target ->
        view.Sightings
        |> Map.tryPick (fun room sighting ->
            if
                sighting.Tick < view.Time
                && view.Time - sighting.Tick <= view.Tuning.VisionGrace
                && Set.contains target sighting.Targets.Value
            then
                Some room
            else
                None)

/// The [[stage]] of the colony whose home is the named room, off the one
/// derivation the shell ran for the tick (ADR 0052 decision 3). `None` for a
/// room no colony of ours lives in — undeclared, unclaimed, or one nothing
/// could see — which is the answer every reader here already gives such a room.
let internal roomStage (view: ColonyView) room = Map.tryFind room view.Stages

/// This colony's own stage: its home room's entry. Always present for a living
/// colony (ADR 0047 decision 1), so `None` is the projection that cannot place
/// its own controller, and every reader gives that colony the answer it gives
/// one standing under the line.
let internal homeStage (view: ColonyView) =
    roomStage view (SpatialInfo.homeName view.Spatial)

/// Whether this colony has outgrown its bootstrap window: `Independent`,
/// at `Tuning.BootstrapLevel` or past it (ADR 0052 decision 3). The one
/// question the Layout's two gates and the Repair pool's rampart line ask
/// — a colony below it is still buying the economy those spends serve.
let internal isIndependent (view: ColonyView) = homeStage view = Some Independent

/// Whether this colony keeps ramparts this tick (ADR 0034 as #214 amends it):
/// it is `Independent`, past the bootstrap line and one stage past the engine's
/// own unlock. The covering rule's one gate and the floor's one gate, one
/// spelling for both: a colony below it places no rampart and counts none of
/// its standing ramparts hungry, so the three a child raised at RCL2 decay away
/// instead of holding four workers to a floor derived for a home with a tower
/// and a Storage behind it.
let internal keepsRamparts (view: ColonyView) = isIndependent view

/// The facts the Planner reads about the assignment table (ADR 0061, ADR
/// 0067), derived once in `Entry` and handed down to the pool. `All` is read as
/// `Set.contains (taskId t) held.All`; `WithThorium` binds delivery persistence
/// to the living holder that still carries the paid-for load.
type HeldTaskFacts =
    {
        All: Set<string>
        WithThorium: Set<string>
    }

[<RequireQualifiedAccess>]
module HeldTaskFacts =
    let empty =
        {
            All = Set.empty
            WithThorium = Set.empty
        }

/// The Planner's narrow view of the assignment table. Task ids are kept
/// — **spelled forward, never parsed**, so nothing here has to pull a target
/// out of a string and a `Withdraw` and a `Repair` on one container are
/// different keys by construction.
///
/// **Filtered to the living**, which is the whole of the join: `Assignments`
/// arrives from Memory and may name a creep that died last tick. The Matcher
/// drops those silently, but `planTasks` runs first, and a colony must not hold
/// a Task open on the strength of a body that is not there.
///
/// **Not filtered to task kinds**: the table holds task *ids*, and picking a
/// kind out of it means reading a kind out of a string, which is the parsing this seam exists to
/// avoid. The whole table is the honest set — "these are the Tasks this colony
/// holds" — and a reader that wants one kind writes the key it wants and asks,
/// as `isHungry` does. One boolean per candidate is still what any reader gets.
let internal heldTaskFacts (view: ColonyView) (assignments: Assignments) : HeldTaskFacts =
    let living = view.Creeps |> List.map (fun creep -> creep.Name, creep) |> Map.ofList

    assignments
    |> Map.fold
        (fun facts name tid ->
            match Map.tryFind name living with
            | None -> facts
            | Some creep ->
                {
                    All = Set.add tid facts.All
                    WithThorium =
                        if creep.Thorium > 0 then
                            Set.add tid facts.WithThorium
                        else
                            facts.WithThorium
                })
        HeldTaskFacts.empty

/// Whether a structure of this kind, carrying these hits, is hungry: its own
/// line, read off the kind (ADR 0034) and — for the decaying kinds — off
/// whether anybody is already repairing it (ADR 0061). A rampart sits below its
/// floor, the Keep below full — it does not decay, so below max means damaged.
/// A kind with no line is never hungry. The floor is capped at the structure's
/// own max so a rampart whose max is somehow under it can still be whole.
///
/// **The decaying kinds are judged by two numbers and the held fact picks
/// between them**: a road or a container nobody holds is hungry below
/// `Tuning.RepairTrigger`, and one a creep is already repairing stays hungry up
/// to `Tuning.RepairWholeLine`. A single line puts the entry and the exit on
/// one number, and one repair tick steps across it, so the repair is over the
/// tick it starts and the holder is released `task-gone` on the next tick's
/// pool. The substitution happens **here and nowhere else** — `Floor` and
/// `Full` have no second number to make (ADR 0061 part 2), and the rule is
/// monotone: the pool with it is a superset of the pool without it.
let private isHungry (tuning: Tuning) (held: Set<string>) id kind (hits: HitsInfo) =
    match wholeLine kind with
    | Some WholeLine.Fraction ->
        let line =
            if Set.contains (taskId (Repair id)) held then
                tuning.RepairWholeLine
            else
                tuning.RepairTrigger

        float hits.Hits < line * float hits.HitsMax
    | Some WholeLine.Floor -> hits.Hits < min tuning.RampartFloor hits.HitsMax
    | Some WholeLine.Full -> hits.Hits < hits.HitsMax
    | None -> false

/// Every structure the projection carries hits for that stands below its kind's
/// line, with its kind, in id order — the Repair pool's own walk, and since ADR
/// 0061 the only reader of it. The held ids are the set the Planner was handed
/// (`heldTaskIds`); the safe-mode reflex's Keep arm, which used to share this
/// walk, asks `keepDamaged` instead.
let internal hungryStructures (view: ColonyView) (held: Set<string>) : (string * BuiltKind) list =
    let ramparts = keepsRamparts view

    SpatialInfo.structureHits view.Spatial
    |> List.choose (fun (id, kind, hits) ->
        match kind with
        // A rampart below the line the colony keeps them from is not
        // hungry: it is decaying away (#214, `keepsRamparts`).
        | BuiltKind.Rampart when not ramparts -> None
        | _ when isHungry view.Tuning held id kind hits -> Some(id, kind)
        | _ -> None)

/// Whether any [[keep]] structure of this colony stands below full hits: the
/// safe-mode reflex's own question (ADR 0034), asked of the same projected hits
/// the Repair pool walks. Its own predicate since ADR 0061 and no longer a
/// filter over `hungryStructures`: **this arm needs no held set**, the Keep's
/// line being full hits whoever is repairing it, and a question that needs no
/// answer should not be made to invent one to ask. So "hungry" and "damaged"
/// stay one fact here however the decaying kinds' two lines move. The cost is
/// a second walk over a hundred-odd structure hits, which is not a flood
/// (#171).
let internal keepDamaged (view: ColonyView) : bool =
    SpatialInfo.structureHits view.Spatial
    |> List.exists (fun (_, kind, hits) -> isKeep kind && hits.Hits < hits.HitsMax)

/// The range a hostile can hurt a creep from, or None for one that cannot (ADR
/// 0033).
let internal weaponRange (hostile: HostileInfo) : int option =
    [
        if List.contains Attack hostile.Body then
            Engine.meleeRange
        if List.contains RangedAttack hostile.Body then
            Engine.rangedRange
    ]
    |> function
        | [] -> None
        | ranges -> Some(List.max ranges)

/// Whether a hostile can hurt anything at all — `weaponRange` asked as a
/// yes/no (ADR 0033), and the one cut between a raider and a scout or a lone
/// healer. Written here because four rules turn on it and each used to own its
/// own opinion of what a weapon is: whose hits the guard beat counts, whether
/// a room stands down, whether safe mode fires, and whether a Reach is derived
/// at all.
let internal isArmed (hostile: HostileInfo) : bool = weaponRange hostile |> Option.isSome

/// Whether a projected target stands in a room this player **owns**, which is
/// how every rule of the season's ore answers "is this ours?" (#261, #311).
/// `FIND_MINERALS`, `FIND_STRUCTURES` and `FIND_DROPPED_RESOURCES` all carry
/// every owner's, and nothing in the shape of a deposit, an extractor, a
/// container or a pile says who put it there. The engine does say, once, and it
/// says it about the **room**: an extractor needs an owned RCL6 room
/// (`checkControllerAvailability` derives `rcl = 0` from a reservation), so an
/// extractor standing in a room we own is ours and one standing in a room we do
/// not is not.
///
/// For a **pile** the same join answers one step weaker and still answers
/// enough. It does not say a [[miner]] of ours dug it — any creep at all may
/// `drop()`, and a deposit that is gone leaves its pile behind — it says the
/// floor under it is ours, which is the only question a Pickup asks. Ore
/// somebody else abandoned in a room of ours is ore we may sweep, and at the
/// [[storage]]'s draw tier the body that goes for it is one with nothing better
/// to do anyway.
///
/// Written once because its readers — the deposits just below, and the piles
/// `ourThoriumPiles` takes off the projection's kind census — would otherwise
/// be two copies of one sentence free to disagree about whose a room is.
/// A room the colony cannot see owns nothing here (ADR 0004).
let internal inARoomWeOwn (view: ColonyView) (id: string) : bool =
    SpatialInfo.roomOf view.Spatial id
    |> Option.bind (fun room -> Map.tryFind room view.RoomControl)
    |> Option.exists (fun control -> control.Owner = Ownership.Ours)

/// The [[thorium]] deposits standing in a room this colony **owns** (ADR 0057
/// decision 2), in id order — the only deposits any rule of this colony may
/// answer for, and the list both the Task pool and the [[miner]] row's quota are
/// read off so the two can never disagree about which rock is ours.
///
/// **Whose the deposit is, is whose the room is** (#261) — the join written out
/// in `inARoomWeOwn` above, and read here rather than restated. What this list
/// adds is why getting it wrong is expensive: a scanned neighbour arrives in the
/// projection with its own deposit, its own extractor and its own container, and
/// `harvest` refuses a mineral whose extractor belongs to somebody else — one
/// `ERR_NOT_OWNER` a tick for the whole of a body's life.
let internal ourDeposits (view: ColonyView) : string list =
    SpatialInfo.idsOfKind view.Spatial Mineral |> List.filter (inARoomWeOwn view)

/// Whether decaying ore standing at this target is **this colony's to sweep**
/// (#354, #311): a room we own, or a room we declared an [[errand]] in. The
/// errand clause is the whole of #354's second half — the Reactor's room has no
/// controller, so "a room we own" made its floor belong to nobody while 915 T
/// bled on it at 1 T a tick with a body of ours standing two tiles away.
///
/// Written once because three readers ask it — the piles below, the tombstones
/// beside them (#359), and the breach channel that alarms on both — and two
/// copies of one sentence are free to disagree about whose a room is, which is
/// the failure mode that made the alarm silent through the incident it was
/// built for.
/// The rooms this colony has declared an [[errand]] in, as a set — the join
/// four rules of the season's ore make, written once (#378).
let internal errandRooms (view: ColonyView) : Set<string> =
    view.Errands |> List.map (fun errand -> errand.RoomName) |> Set.ofList

let private oursToSweep (view: ColonyView) (id: string) : bool =
    let errandRooms = errandRooms view

    inARoomWeOwn view id
    || SpatialInfo.roomOf view.Spatial id
       |> Option.exists (fun room ->
           Set.contains room errandRooms
           // The third clause and the third widening of this reach (#360): a
           // room we own (#311), a room we declared an errand in (#354), and a
           // room a chain of ours merely **crosses**. The delivery route is
           // three crossings, the courier is oldest on the loaded leg — that is
           // the leg `Emitter.outlivesTheLoadedLeg` guards — and ore that falls
           // in the middle of it used to land where nothing could name it, pool
           // it or alarm on it, while it bled `ceil(amount/1000)` a tick and the
           // season never made another gram.
           //
           // `view.Crossed` and not "any room in the projection", which is what
           // this clause was first written as and what the suite caught: a
           // stranger's room can be in a scan set without being on a chain, and
           // its floor is not ours to walk onto. What makes a crossed room's ore
           // ours is the pairing with `ColonyView.transiting`, which admits
           // exactly two kinds out of such a room — a pile and a tombstone — so
           // there is no third thing here for the widening to reach.
           || Set.contains room view.Crossed)

/// The [[thorium]] **on the ground** in a room this colony owns, in id order
/// (#311): the dig that landed on the floor rather than in the mineral
/// [[container]], because the container was at its 2,000 cap on the tick the
/// [[miner]] swung. Ours by the room and by nothing on the pile, which carries
/// no owner: `inARoomWeOwn` above is where that argument is written, including
/// what it does and does not buy for a pile.
///
/// Off the projection's kind census and not off the deposits, deliberately: a
/// pile is not seated on anything, and a mine whose container is destroyed
/// mid-haul goes on dropping onto a tile the deposit census would still name
/// but the container census no longer does. What the pool then does with the
/// list is the Withdraw's arithmetic — an amount over a threshold, capped by a
/// load — because a pile with no store in it is the mineral container with the
/// store taken away.
///
/// Whose the room has to be is `oursToSweep` above, shared with the tombstones
/// below.
let internal ourThoriumPiles (view: ColonyView) : string list =
    SpatialInfo.idsOfKind view.Spatial (Dropped Thorium)
    |> List.filter (oursToSweep view)

/// The [[thorium]] in a **store with a clock on it** — a tombstone or a ruin —
/// standing in a room this colony sweeps, in id order (#359). The pile above
/// one object over: a courier that dies with ore aboard leaves it in its
/// tombstone rather than on the floor, and W15S25's tombstone at (43,6) held
/// 175 T of it in the declared Reactor room.
///
/// It is the same errand as the pile's and the same clock, run faster. The
/// engine's `withdraw` takes a tombstone or a ruin for any resource
/// (`@screeps/engine` `src/game/creeps.js`, whose target test names
/// `globals.Tombstone` and `globals.Ruin` beside a structure), and a tombstone
/// that decays drops its **whole** store as piles
/// (`processor/intents/tombstones/tick.js`) — so ore left in one is ore that
/// becomes ore on the floor, which is where `ourThoriumPiles` above picks the
/// story up, minus whatever the decay cost in between.
///
/// The kind carries no resource, so the holding is what selects: a tombstone
/// with an entry in the Thorium map and none for the energy-only one beside it,
/// absence being no holding (ADR 0004).
let internal ourThoriumTombstones (view: ColonyView) : string list =
    SpatialInfo.idsOfKind view.Spatial Tombstone
    |> List.filter (fun id -> SpatialInfo.heldIn view.Spatial Thorium id > 0 && oursToSweep view id)

/// Whether a Reactor this colony has declared has room for a **whole load**
/// (#354). What gates a new draw at the Storage, and the only regulator of
/// supply the programme has: the Reactor burns exactly 1 T a tick against a
/// 1,000-unit store (`docs/research/thorium-reactor.md` §2), so a cadence
/// cannot meter a delivery — a fixed interval either outruns the burn and
/// strands the surplus, or falls behind it and breaks the streak, and 636
/// ticks per 999-T load outran it by 57%.
///
/// Read at the **draw**, on the store as it stands **plus every unit of ore
/// already aboard a body of ours** (#362). The store alone was the first
/// version's reading, and its claim — that a load admitted here has strictly
/// more room when it lands, the store draining for the whole loaded walk — is
/// true of *one* carrier and false of two. Live at t501,501 a hauler drew 204 T
/// against a low store and set off on the three-crossing walk; the gate stayed
/// open behind it, a courier drew a whole 500 and landed first, and the hauler
/// arrived at a store of 999 with 196 T it could not put down. It stood on the
/// Reactor's tile for 152 ticks and 419 T were already on the floor beside it
/// from the same shape.
///
/// Counting **all** ore afloat is deliberately conservative, and it is the
/// reading that cannot be gamed by who is carrying: a mine [[hauler]] walking
/// its own deposit's ore home to the Storage is counted too, though it is not
/// inbound to the Reactor, so a colony that still mines *and* delivers will
/// sometimes defer a draw by one haul cycle. That costs ticks of cadence; the
/// alternative costs ore on a hot tile, and this programme has now paid that
/// price three times (#354, #356, this). Naming which body is inbound would
/// mean reading the `Assignments` map the Planner is deliberately blind to
/// (ADR 0025), and the number it would buy is a delivery's worth of latency.
///
/// A Reactor we cannot see answers `false`: no row, no room (ADR 0004). That
/// closes the draw and leaves the load banked at home, which is where a load
/// with nowhere to go belongs.
///
/// Read off `view.Reactors` and **not** off `SpatialInfo.Thorium`, which
/// carries every store a Task can name and deliberately not this one
/// (`RoomFacts.Thorium`'s own comment). The first version of this gate asked
/// the wrong map: it answered 0 for a Reactor holding 999, so the gate never
/// closed once in flight, and ore went on arriving at a full store and ending
/// up on its floor. The unit test agreed with it because the fixture wrote the
/// store where the gate looked — a projection shape `World` has never built.
let internal reactorTakesALoad (view: ColonyView) (load: int) : bool =
    let afloat = view.Creeps |> List.sumBy (fun creep -> creep.Thorium)

    view.Errands
    |> List.exists (fun errand ->
        let reactorId = fst errand.Target

        view.Reactors
        |> List.exists (fun reactor ->
            reactor.Id = reactorId
            && reactor.Thorium + afloat + load <= Engine.reactorCapacity))

/// The **mineral [[container]]s** of this colony's own deposits, in deposit
/// order (ADR 0057 decision 3): the built container standing on a deposit's
/// [[seat]] — the mine [[post]] the store-less [[miner]] digs from, whose drop
/// the engine lands *in* the container it is standing on. The one Thorium store
/// this colony ever draws, and the term the [[hauler unit]]'s demand sum grows
/// by, read here once so the Task pool and the quota cannot come to disagree
/// about which container is a mine's.
///
/// Range 1 of the deposit **in the deposit's own room**, which is the mine Post
/// said without the [[atlas]]: a Seat is a walkable neighbour and a container
/// stands only on one, so the two censuses name the same tile. The room join is
/// load-bearing for the reason ADR 0041 gives — a `Pos` names no room, and a
/// container on the same coordinate of an [[outpost]] is not this deposit's. A
/// **built** container alone and never a site: a site holds nothing, and the
/// Post does not exist until the container stands (#261).
///
/// Off `ourDeposits` and never off the container census, for that list's own
/// reason: `FIND_STRUCTURES` carries every owner's containers, and a scanned
/// neighbour's mine is exactly as visible as ours.
///
/// The **deposit beside the container**, because the two readers ask different
/// questions of the same join (#262): the Task pool wants the store, and the
/// [[hauler unit]]'s demand term wants to know whether the rock behind it can be
/// dug at all. `ourMineralContainers` is this list with the deposit dropped.
let internal ourMineralContainerPairs (view: ColonyView) : (string * string) list =
    let containers =
        SpatialInfo.idsOfKind view.Spatial (Structure BuiltKind.Container)
        |> List.choose (fun id ->
            SpatialInfo.placementOf view.Spatial id |> Option.map (fun tile -> id, tile))

    ourDeposits view
    |> List.choose (fun depositId ->
        match SpatialInfo.placementOf view.Spatial depositId with
        | None -> None
        | Some deposit ->
            containers
            |> List.filter (fun (_, tile) ->
                tile.Room = deposit.Room && range (RoomPos.pos tile) (RoomPos.pos deposit) <= 1)
            // Ties by id, the way every other tie in this colony falls: a
            // deposit the Layout ever seated two containers beside answers with
            // one of them and not with both.
            |> List.map fst
            |> List.sort
            |> List.tryHead
            |> Option.map (fun containerId -> depositId, containerId))

let internal ourMineralContainers (view: ColonyView) : string list =
    ourMineralContainerPairs view |> List.map snd

/// Whether a deposit of ours is one the colony can **actually dig this tick**
/// (ADR 0057 decision 2): it still holds Thorium, and its extractor **stands**.
/// The two conditions the [[miner]] row's own quota puts a body at 0 for. It was
/// written once so the row that hires the digger and the term that hires the
/// carrier could not disagree about whether there is a mine (#262) — they are
/// now **meant** to disagree, and only the digger reads this (#361): the
/// carrier's load comes out of a Storage, which outlives the deposit that
/// filled it. The row adds the third,
/// the mine [[post]], which the container census the haul term reads has already
/// answered. A site is not an extractor: `harvest.js` refuses a mineral with
/// none on it, so a deposit under one produces nothing and the haul priced
/// against it is a hauler hired for a trip nobody ever makes.
let internal depositIsDiggable (view: ColonyView) atlas (depositId: string) =
    Map.tryFind depositId view.Spatial.Thorium |> Option.defaultValue 0 > 0
    && (Atlas.extractorOn atlas depositId).IsSome

/// Whether the season's delivery programme has all of its current ground
/// facts (#319, narrowed by #361): a Storage holding one whole load, and a
/// resident CLAIM body keeping vision and ownership at a declared Reactor. No
/// remembered switch — each fact closes the row when it disappears.
///
/// What is **not** a condition, and cost a live streak to learn: a diggable
/// mine. The row used to require one, on the reading that a delivery programme
/// is the far end of a mining programme. It is not. Thorium never regenerates
/// (`docs/research/thorium-reactor.md`), so every deposit ends mined out with
/// its ore sitting in a Storage — and the tick the mine ran dry this row
/// closed, the courier was not replaced, and the Reactor burned down its
/// 1,000-unit store with **7,226 T banked at home and a 7,989-tick streak
/// standing** (#361). Ore in the bank scores exactly what ore in the ground
/// scores; the mine decides only whether the bank is *refilled*.
///
/// `hasLoad` is what makes that safe: the programme wants ore in a Storage
/// before it hires anybody, so a closing programme means an empty bank rather
/// than an empty mine. Since #378 that is a whole `ReactorLoad` while more ore
/// is coming and the **remainder** once it is not (`deliveryLoad`), which
/// leaves the safety argument above intact and adds the case it could not
/// express: a bank holding less than one load is still a bank holding ore.
/// The energy standing in this colony's Storages — its **stock**, as against
/// `ColonyView.Bank`'s spawn account (ADR 0023). Read by the worker row's
/// backlog term (#364), which is paid out of the stock and not out of income:
/// a 100,000-energy terminal is bought with what is banked, and the row that
/// builds it has to be hired against the same number.
///
/// Folded over every Storage the projection holds rather than the home room's
/// alone, for `mineRefills`' reason: one is all there has ever been, and a rule
/// that assumed it would be silently wrong in the colony that first has two.
let internal stockedEnergy (view: ColonyView) : int =
    view.Spatial.TargetKinds
    |> Map.fold
        (fun total id kind ->
            if kind = Structure BuiltKind.Storage then
                total + SpatialInfo.storedIn view.Spatial id
            else
                total)
        0

/// Whether more ore is still on its way to the [[storage]] (#378): a deposit
/// of ours with an extractor over it and something left to dig, or ore already
/// standing in a mineral [[container]] waiting for its haul. The question the
/// last load turns on — while this is true the remainder in the Storage is the
/// front of a queue, and while it is false the remainder is all there will ever
/// be.
let internal oreStillComing (view: ColonyView) atlas : bool =
    ourDeposits view |> List.exists (depositIsDiggable view atlas)
    || ourMineralContainers view
       |> List.exists (fun id -> SpatialInfo.heldIn view.Spatial Thorium id > 0)

/// The most ore any one [[storage]] of this colony banks — a **maximum** and
/// not `stockedEnergy`'s sum beside it, because the draw this feeds is one
/// body at one store and what it can take is what that store holds.
let internal mostOreInAStorage (view: ColonyView) : int =
    view.Spatial.TargetKinds
    |> Map.fold
        (fun most id kind ->
            if kind = Structure BuiltKind.Storage then
                max most (SpatialInfo.heldIn view.Spatial Thorium id)
            else
                most)
        0

/// **What the next delivery draw takes, and 0 when there is none to take**
/// (#378). A whole `Tuning.ReactorLoad` while the mine still feeds the bank,
/// and the **remainder** once it does not: the gate used to read
/// `stock >= ReactorLoad` on both the programme and the draw, so the last
/// partial load was unreachable by construction — live at t559,469 the Reactor
/// ran down toward zero with 376 T standing in the Storage ten tiles from the
/// courier, because 376 is not 500 and nothing in the programme could say
/// "this is the last of it". The full-load gate is kept while ore is still
/// coming, so a trickle passing through the Storage mid-mining never hires a
/// courier for forty units; what opens the partial draw is precisely the fact
/// that there will be no fuller one (`oreStillComing`).
let internal deliveryLoad (view: ColonyView) atlas : int =
    let banked = mostOreInAStorage view

    if banked >= view.Tuning.ReactorLoad then
        view.Tuning.ReactorLoad
    elif banked > 0 && not (oreStillComing view atlas) then
        banked
    else
        0

/// Whether the ore a body holds is a **delivery's** — drawn for the Reactor
/// and not hauled out of a mine (#378). The Planner pools the Reactor's sink
/// off this and the Emitter refuses the Storage's off it, which is one
/// question asked from both ends: what a body took is where it must put it
/// down.
///
/// Three ways to be one, and the third is what the partial load needed. The
/// **exact `ReactorLoad`** is the historical marker (ADR 0067) and still the
/// common one: a body holding precisely the programme's load did not get it
/// from a mineral container by accident. **This tick's own load** carries the
/// same argument for a remainder. And in the terminal state — a colony that
/// declared the errand, with nothing left to dig and nothing standing in a
/// mineral container — **any** ore aboard is a delivery's: the draw empties
/// the bank, so a remainder that was 376 on the tick it was taken is measured
/// against a load of 0 on the tick after, and without this clause the
/// Reactor's sink would stop being pooled for the body carrying it.
///
/// Read by the **Planner**, which pools a sink, and by nothing that refuses
/// one: a permissive answer here costs an entry in the pool, where the same
/// answer in the Emitter refused the Storage to the last mine haul, to a body
/// sweeping a crossed room's pile and to the carrier walking an arriving
/// consignment in from the terminal (#349) — #262's stranded body again.
let internal carryingADelivery
    (view: ColonyView)
    (load: int)
    (stillComing: bool)
    (creep: CreepInfo)
    =
    creep.Thorium > 0
    && (creep.Thorium = view.Tuning.ReactorLoad
        || creep.Thorium = load
        || (not stillComing && not (List.isEmpty view.Errands)))

/// Whether ore is lying in a declared [[errand]] room — a pile or a tombstone
/// beside the Reactor itself (#378). Season score at the far end of the
/// delivery's own walk: a courier that dies loaded leaves 500 T five tiles from
/// the store it was carrying it to, and until this fact existed the only sink
/// such ore had was the Storage three crossings back the way it came.
let internal oreBesideTheReactor (view: ColonyView) : bool =
    ourThoriumTombstones view @ ourThoriumPiles view
    |> List.exists (fun id ->
        SpatialInfo.roomOf view.Spatial id
        |> Option.exists (fun room -> Set.contains room (errandRooms view)))

let internal courierProgrammeOpen (view: ColonyView) atlas =
    let hasLoad = deliveryLoad view atlas > 0
    let errandRooms = errandRooms view

    let hasResidentReclaimer =
        view.Creeps
        |> List.exists (fun creep ->
            partCount creep.Body BodyPart.Claim > 0
            && (Atlas.creepTile atlas creep.Name
                |> Option.exists (fun tile -> Set.contains tile.Room errandRooms)))

    view.Bank.Capacity >= bodyCost courierPattern.Block
    && hasLoad
    && hasResidentReclaimer
