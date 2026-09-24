/// The tick's raw facts, per room and per creep: hostiles and invader cores,
/// construction sites, our own bodies, and the `World` that holds every room
/// seen this tick beside the sighting recalled from the last.
[<AutoOpen>]
module Fabot.Core.Types.Sightings

/// What the decision layer knows about one construction site this tick.
type ConstructionSiteInfo =
    {
        Id: string
        /// The energy still owed before this site becomes a structure —
        /// `progressTotal - progress`, because nothing decides on how far
        /// along a site is, only on what is left to pay (#364). It is what
        /// tells a road's 300 from a terminal's 100,000.
        Left: int
    }

/// What the decision layer knows about one hostile creep in a room the colony
/// is looking into this tick.
type HostileInfo =
    {
        Id: string
        /// Whose creep this is, as the engine spells the username ("Invader"
        /// for the NPCs). Attribution for the Raid log; no reflex reads it.
        Owner: string
        /// Where it stands, room and tile in one: a bare `Pos` would read one
        /// of ours on the same coordinate of another room as range 0.
        Pos: RoomPos
        Body: BodyPart list
        /// Ticks this hostile has left. An Invader standing in a room nobody
        /// owns never suicides — the engine's suicide branch wants a
        /// controller owner and an [[outpost]] has none — so what it has left
        /// is exactly what it will spend, and this is the one deadline a raid
        /// with no core in it offers the [[stand-down]] (#257).
        TicksToLive: int
    }

/// An NPC invader core standing in a room the colony works this tick. A
/// **structure**, not a creep, so it reaches the projection through neither
/// `Hostiles` nor the fire reflex, whose sweep is `FIND_HOSTILE_CREEPS`. It is
/// the threat an [[outpost]] is stood down from — 100,000 hits, no creeps at
/// level 0, and it never leaves.
type InvaderCoreInfo =
    {
        /// The room it stands in: the colony works rooms, not tiles.
        RoomName: string
        /// The **absolute** tick the core's collapse timer runs out at, or None
        /// where it carries none — an expanded level-0 core has no stronghold
        /// to collapse, so the deadline is read off the reservation it took
        /// (`ReservationHolder.Invader`). The common case on the frontier.
        CollapseTick: int option
        /// The core's level, and what tells a **stronghold** from the level-0
        /// expansion core beside it (#382): a level-1-and-up core is a bunker
        /// nothing of ours crosses alive, a level-0 core a body walks past.
        /// Live W15S26 carried a `bunker4` and W15S27 the level-0 core it
        /// expanded into, both on the same collapse clock, so the clock cannot
        /// tell them apart and this can.
        Level: int
    }

/// Whose flag a visible sector Reactor carries. Unlike `Ownership`, the rival
/// case keeps the engine's username: operator attribution, not a decision's
/// ours-or-not question (#320).
[<RequireQualifiedAccess>]
type ReactorOwner =
    | Ours
    | Rival of username: string
    | Unowned

/// The changing facts read from one visible sector Reactor. A room without
/// vision carries no row, so no zero here can be mistaken for a stale sample.
type ReactorInfo =
    {
        Id: string
        Owner: ReactorOwner
        Thorium: int
        ContinuousWork: int
    }

/// Which deadline an [[outpost]]'s [[stand-down]] runs to — the provenance of
/// the tick, carried beside it because it cannot be recovered afterwards.
/// The first three are the fallback order for a threat, best first; the
/// fourth is not a threat at all (#165). It crosses the wire, so it is spelt
/// once in `standDownBasisName` and round-tripped by `Core.Tests`. ADR-0043
[<RequireQualifiedAccess>]
type StandDownBasis =
    /// The core's own `EFFECT_COLLAPSE_TIMER`, the first answer wherever it
    /// can be read. **Which clock the expiry came off, and nothing about the
    /// room** — whether a bunker stands in it is `OutpostEpisode.Stronghold`'s
    /// to say (#382): `sight` overwrites the basis whenever a later deadline
    /// arrives, so one field for both would send a body through a room whose
    /// raid outlived its core with four towers standing in it.
    | CollapseTimer
    /// The end of the reservation the core took with `attackController` —
    /// what a level-0 core answers with. The measured case on this frontier
    /// (docs/research/remote-mining.md §8.4).
    | Reservation
    /// Neither deadline was readable, so the clock is the 2,500-tick
    /// stronghold expansion period. It errs long deliberately: the gate may
    /// be wrong only in the direction that costs an outpost's income.
    | Fallback
    /// The end of the [[reservation]] **another player** holds on the room
    /// (#165): a clock where ownership is a latch, because a hold that decays
    /// is reversible where ownership is not. No floor under it, unlike
    /// `Reservation`: a player's claimer that stops coming leaves nothing
    /// behind, where the Invader's core re-takes the hold it lets lapse.
    | RivalReservation
    /// A raid of plain [[invader]] creeps, clocked off the longest life among
    /// them (#257; `HostileInfo.TicksToLive` says why that is a deadline).
    /// Read only once the colony has stopped fighting for the room: while a
    /// [[guard]] stands there, or the episode has casts left, the room is a
    /// fight and not a withdrawal.
    | InvaderRaid

/// What the decision layer knows about one owned creep this tick.
type CreepInfo =
    {
        Name: string
        /// Ticks the creep still has to live; a creep still spawning is
        /// outside the projection, so this is always a real count. The fact,
        /// not the judgement of whether it is expiring.
        TicksToLive: int
        /// Fatigue points still to pay off; a creep with any cannot step
        /// this tick — the engine's move answers ERR_TIRED.
        Fatigue: int
        /// Current and maximum life; self-healing reads damage independently
        /// of the active body counts (partially damaged parts still work).
        Hits: HitsInfo
        /// Energy currently carried.
        Energy: int
        /// Thorium currently carried: a **second field beside `Energy`** and
        /// never a resource key inside it, for the reason `SpatialInfo.Thorium`
        /// gives. A decision reads it because **a body carries one resource
        /// at a time**: the Thorium [[withdraw]] wants an *empty* store, the
        /// Thorium [[refill]] a body holding some, and the energy intakes are
        /// shut to a body holding any. Not derived: `FreeCapacity` is the
        /// whole store's, so `capacity - free - energy` holds only where the
        /// three agree, and a hand-built fixture is free to state a body no
        /// engine would hand back.
        Thorium: int
        /// Carry capacity still free (0 = full) — the **whole** store's: a
        /// body holding 999 Thorium of 1,200 reports 201 free and no energy.
        FreeCapacity: int
        /// Active part counts (body entries with hits > 0). Absent or zero
        /// means the creep has no usable part of that kind.
        Body: Map<BodyPart, int>
        /// Whether the creep stands on a different tile from last tick — the
        /// shell's reading of Memory's last positions, false for a body born
        /// this tick and in every hand-built fixture. The one reader is the
        /// occupancy surcharge (#225).
        Moved: bool
    }

/// A creep already paid for and still in a spawn's oven. Its generated name is
/// retained because two rows may buy the same part counts (#319).
type CastingInfo = { Name: string; Body: BodyPart list }

/// What one room holds this tick, to everybody: filed under the room's own
/// name and saying nothing about who is looking at it. Every list here is
/// *this room's*: the shell scopes what the engine does not. Absence stays
/// per entry and reaches down two levels — a room the world holds nothing
/// for reads `RoomFacts.empty`, and one it holds terrain for but has no
/// vision in reads that terrain beside empty everything else.
type RoomFacts =
    {
        /// This room's geometry: terrain, the targets standing on it, and
        /// **every** creep of ours in the room, not one colony's.
        Layer: RoomLayer
        /// The room's border ring, the Seam's terrain and never ground.
        Border: Map<Pos, Terrain>
        /// The kind of each target standing in this room, under the engine's
        /// own id — unique across the world, so the layer that holds it *is*
        /// the room it stands in.
        TargetKinds: Map<string, TargetKind>
        /// Current/max hits of the repairable kinds standing here.
        Hits: Map<string, HitsInfo>
        /// Energy currently stored, per store standing here: containers, the
        /// Storage, piles, tombstones and ruins.
        Stores: Map<string, int>
        /// Thorium currently held, per store standing here, and the remaining
        /// amount of the Thorium mineral itself. A second map beside `Stores`
        /// for the reason `SpatialInfo.Thorium` gives.
        Thorium: Map<string, int>
        /// Ticks before a structure standing here may act again — the
        /// extractor's cooldown, the one clock a decision reads off a
        /// structure.
        Cooldowns: Map<string, int>
        /// Whose each **object** standing here is (#318) — the per-object twin
        /// of `Control`'s room ownership, and the fact the sector Reactor's
        /// room has no controller to answer with. Filled for the reactors
        /// alone today.
        Owners: Map<string, Ownership>
        /// The sector Reactors visible in this room, with the richer facts
        /// used only by the global Reactor observation channel (#320).
        Reactors: ReactorInfo list
        /// Who holds the room, and whether its safe mode is running — `None`
        /// for a room nothing looked into this tick, which is not the same
        /// fact as a room nobody holds.
        Control: RoomControlInfo option
        /// The controller of this room **while it is ours**. `None` for a
        /// room we do not own, whose ownership and reservation are
        /// `Control`'s.
        Controller: ControllerInfo option
        /// The room's shared spawn-energy account. Zero for a room with no
        /// spawn or extension in it, which a colony reads as an empty bank.
        Energy: RoomEnergy
        /// Our spawns standing in this room. The world's, so a spawn standing
        /// in a [[nursery]] is a fact about that room and not about its
        /// mother — which is exactly what ends the nursery.
        Spawns: SpawnInfo list
        /// The names and bodies still gestating in this room's spawns: energy
        /// the colony has **already spent**. Filed under the room because a
        /// colony banks in one room and every row's gap is a colony number.
        Casting: CastingInfo list
        /// Our energy-hungry structures standing here (spawn, extension,
        /// tower), whether or not they currently have room.
        Refillables: RefillableInfo list
        /// The sources in this room, as vision answered for them. What a
        /// *declaration* answers for is laid over a colony's projection
        /// instead (`Outpost.place`, `Outpost.pooledSources`).
        Sources: SourceInfo list
        /// Our construction sites standing here (#150).
        ConstructionSites: ConstructionSiteInfo list
        /// Hostile creeps standing here this tick.
        Hostiles: HostileInfo list
        /// The invader cores standing here.
        InvaderCores: InvaderCoreInfo list
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RoomFacts =
    /// A room the world holds nothing for — every entry absent, which is
    /// what a room outside the scan set and a room with no vision both
    /// read as.
    let empty: RoomFacts =
        {
            Layer = RoomLayer.empty
            Border = Map.empty
            TargetKinds = Map.empty
            Hits = Map.empty
            Stores = Map.empty
            Thorium = Map.empty
            Cooldowns = Map.empty
            Owners = Map.empty
            Reactors = []
            Control = None
            Controller = None
            Energy = { Available = 0; Capacity = 0 }
            Spawns = []
            Casting = []
            Refillables = []
            Sources = []
            ConstructionSites = []
            Hostiles = []
            InvaderCores = []
        }

/// One creep of ours, in the world's reading, before any colony has claimed
/// it.
type WorldCreep =
    {
        /// The room the engine says the creep stands in
        /// (`World.creepColonies` decides whose it then is).
        Room: string
        /// What the decision layer knows about the body itself.
        Info: CreepInfo
    }

/// What the world last saw standing in one room, and when (#151): the one
/// thing carried **across** ticks about a room, for one question only.
/// Per-entry absence cannot say *why* an id left the pool, and the two
/// answers are opposite work: a container destroyed is a Task gone, a room
/// gone dark is a Task waiting. So this is read by the vision grace and by
/// nothing else, and what the grace hands on is a **room name**: the Matcher
/// keeps the assignment and the mover walks its holder at that room's Seam.
/// Nothing is placed or priced off a sighting. Which of these reach a colony
/// is the **view**'s cut (`ColonyView.ofWorld`, #271).
type RoomSighting =
    {
        /// The tick vision last answered for the room. Equal to the world's
        /// own `Time` for a room seen this tick.
        Tick: int
        /// The ids that stood in the room that tick, and nothing about them:
        /// set membership is the only question the grace asks, and there is
        /// no stale kind here for the next rule to read one out of.
        ///
        /// Deferred, because the tick a room is **seen** nobody reads this:
        /// both readers ask only about a sighting older than the tick they
        /// run in (`Facts.errandRoomOf`, and the errand narrowing in
        /// `ColonyView.ofWorld`). `Lazy` keeps it built once the room goes
        /// dark, so the world stops paying a set of several hundred ids a
        /// room a tick for an answer nobody wants yet (#371). The closure
        /// captures the projection's own census, built this tick anyway.
        Targets: Lazy<Set<string>>
    }

/// `World.linkedRecalling`'s memo: `(keeper margin, from, to)` to whether a
/// crossing joins the pair. Mutable and heap-only, like `WalkTable`, and the
/// **shell's**: one table for the life of the process, because every answer
/// in it is the terrain's. A test lays a table of its own per call
/// (`linkedBy`, `scanOf`, `creepColonies`, `ColonyView.ofWorld`); what is
/// forbidden is a **static** holding one, shared across Expecto's parallel
/// lists (#310, `ParallelSafetyTests`).
type JoinTable = System.Collections.Generic.Dictionary<int * string * string, bool>

/// Everything this tick was seen to hold, once. The shell builds one
/// (`World.ofGame`, the only code that touches `Game`) and
/// `ColonyView.ofWorld` cuts one colony's share of it; `decide` is written
/// against a view and never against the world.
type World =
    {
        Time: int
        /// Every room the world holds anything for this tick: the declared
        /// rooms, whose terrain and furniture need no vision, and every room
        /// the engine answered `Game.rooms` with. A room absent here reads
        /// `RoomFacts.empty`.
        Rooms: Map<string, RoomFacts>
        /// Every creep we own that is not still gestating, in the engine's
        /// own order. Whose each one is this tick is `World.creepColonies`'
        /// answer: the rule needs the [[stand-down]] gate, read out of
        /// Memory.
        Creeps: WorldCreep list
        /// What each room was last seen to carry (#151): this tick's census
        /// for every room vision answered for, and the last one taken for a
        /// room it did not. Heap state in the shell and deliberately not a
        /// Memory leaf, so a global reset empties it. A room never seen has
        /// no entry.
        Sightings: Map<string, RoomSighting>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module World =
    /// A world holding nothing: no room, no creep.
    let empty: World =
        {
            Time = 0
            Rooms = Map.empty
            Creeps = []
            Sightings = Map.empty
        }

    /// This tick's world with what it saw **before** laid under it (#151):
    /// every room vision answered for keeps this tick's sighting, and a room
    /// it did not answer for keeps the last one taken. One pure function a
    /// test can drive. Bounded by the rooms this tick's world holds, so a room
    /// that leaves the world leaves the memory with it.
    let recalling (previous: Map<string, RoomSighting>) (world: World) : World =
        { world with
            Sightings =
                (previous |> Map.filter (fun room _ -> Map.containsKey room world.Rooms),
                 world.Sightings)
                ||> Map.fold (fun carried room sighting -> Map.add room sighting carried)
        }

    /// One room's facts: a room the world carries nothing for reads as every
    /// entry absent, never as a lookup that throws.
    let roomOf (world: World) (room: string) : RoomFacts =
        Map.tryFind room world.Rooms |> Option.defaultValue RoomFacts.empty

    /// The rooms we own this tick, off the control entry vision paid for: one
    /// of the two facts a colony has to pass to be **living**.
    let ownedRooms (world: World) : Set<string> =
        world.Rooms
        |> Map.toList
        |> List.filter (fun (_, facts) ->
            facts.Control |> Option.exists (fun control -> control.Owner = Ownership.Ours))
        |> List.map fst
        |> Set.ofList

    /// The rooms one of our spawns stands in, in room-name order: the other
    /// fact `Colony.living` asks for.
    let spawnRooms (world: World) : string list =
        world.Rooms
        |> Map.toList
        |> List.filter (fun (_, facts) -> not (List.isEmpty facts.Spawns))
        |> List.map fst

    /// The [[stage]] of every declared colony that is one this tick, derived
    /// off the world because a stage decides whether a mother scans her
    /// child's room at all. Asked of the **declared** homes and of every
    /// **living** colony's home — not every owned room, or a room we claimed
    /// by hand and never declared would arrive in some colony's scan set as a
    /// [[nursery]] to raise.
    let rec stages
        (tuning: Tuning)
        (colonies: Colony list)
        (world: World)
        : Map<string, ColonyStage> =
        Colony.homes colonies
        @ (living colonies world |> List.map (fun colony -> colony.Home))
        |> List.distinct
        |> List.choose (fun name ->
            let facts = roomOf world name

            let owned =
                facts.Control |> Option.exists (fun control -> control.Owner = Ownership.Ours)

            Colony.stageOf
                tuning
                owned
                (not (List.isEmpty facts.Spawns))
                (facts.Controller |> Option.map (fun c -> c.Level))
            |> Option.map (fun stage -> name, stage))
        |> Map.ofList

    /// The colonies that run this tick (`Colony.living`).
    and living (colonies: Colony list) (world: World) : Colony list =
        Colony.living (ownedRooms world) (spawnRooms world) colonies

    /// One room's border ring read as a [[seam]]'s near or far side: a tile
    /// whose terrain is not wall and which no declared keeper rock masks
    /// (`Keepers`). Written here rather than inside `linked` because a second
    /// caller builds the same predicate off a committed capture and a hand
    /// copy has had to move in lockstep twice (`RoomOutpostTests`, #317). A
    /// **closure over one room**: resolved once, read forty-eight times.
    let ringWalkable (keeperMargin: int) (room: string) (border: Map<Pos, Terrain>) : Pos -> bool =
        // The mask's declaration is a lookup and the band is a loop, so
        // `maskIn` takes the room here rather than on every tile.
        let masked = Keepers.maskIn keeperMargin room

        fun tile ->
            match Map.tryFind tile border with
            | Some terrain -> terrain <> Wall && not (masked tile)
            | None -> false

    /// The same reading one layer in: a tile of a room's own **ground** a body
    /// could stand on. The [[world]]'s half of the landing test `Atlas.seams`
    /// answers off its raw terrain grid — terrain alone, because a band is
    /// geometry and a structure raised this tick must not move it.
    /// `World.ofGame` already reads terrain for every declared and transit
    /// room, so this costs no plumbing.
    let groundWalkable (keeperMargin: int) (room: string) (terrain: TerrainGrid) : Pos -> bool =
        let masked = Keepers.maskIn keeperMargin room

        fun tile ->
            match TerrainGrid.tryFind tile terrain with
            | Some ground -> ground <> Wall && not (masked tile)
            | None -> false

    /// Whether a creep could step from one room into the other: the
    /// [[world]]'s own reading of a [[seam]] band, before any [[atlas]] grid
    /// exists. The Atlas answers the same question off its ring grids
    /// (`Atlas.seams`); both go through `Seam.joinedBy`, so the scan set and
    /// the price cannot disagree. A room the world holds no facts for is
    /// joined to nothing. ADR-0058
    ///
    /// The [[keeper margin]] is an argument because a mask near a border can
    /// take exit tiles out of a band, and the routable question has to be
    /// asked over the **same** masked layer every price will use.
    ///
    /// The far side is asked **twice**: its ring, for whether the engine
    /// lands a body there at all, and its ground, for whether the body can
    /// then step off the landing. ADR-0062
    let linked (keeperMargin: int) (world: World) (fromRoom: string) (toRoom: string) : bool =
        let walkableIn room =
            ringWalkable keeperMargin room (roomOf world room).Border

        Seam.joinedBy
            (walkableIn fromRoom)
            (walkableIn toRoom)
            (groundWalkable keeperMargin toRoom (roomOf world toRoom).Layer.Terrain)
            fromRoom
            toRoom

    /// `linked` over a table the caller holds **across ticks**: three readers
    /// ask it per colony per tick (`scanOf`, `ColonyView.ofWorld`,
    /// `creepColonies`), re-asking per hop — 93 calls over 26 distinct pairs
    /// in one colony's tick before any table stood, and 5.7% of a `reactor
    /// --level 7` tick building and dropping three tables a colony once one
    /// did (`docs/profiling.md` § History, 2026-09-18).
    ///
    /// The table can outlive the tick because an answer reads only terrain —
    /// which the engine never changes — under a keeper margin that is in the
    /// key. What *can* move between ticks is which rooms the world holds, so
    /// that is read off `Rooms` ahead of the table on every ask.
    /// Asymmetric by construction, like `linked`: the key is the ordered pair
    /// and an answer is never reused backwards.
    let linkedRecalling
        (joins: JoinTable)
        (keeperMargin: int)
        (world: World)
        : string -> string -> bool =
        fun fromRoom toRoom ->
            if not (Map.containsKey fromRoom world.Rooms && Map.containsKey toRoom world.Rooms) then
                false
            else
                let key = (keeperMargin, fromRoom, toRoom)

                // `ContainsKey` then the indexer, never `TryGetValue` in a
                // match: Fable compiles the out-parameter pattern into four
                // allocations per read (`Atlas.memoised`'s note).
                if joins.ContainsKey key then
                    joins.[key]
                else
                    let joined = linked keeperMargin world fromRoom toRoom
                    joins.[key] <- joined
                    joined

    /// **A room a stronghold holds is not a link** (#382), asked of **both**
    /// ends of every hop. A declaration whose every chain crossed it stops
    /// being `routable` and is refused as a unit, and the tick the core's
    /// collapse timer runs out the room re-links. ADR-0074
    let linkedAvoiding (impassable: Set<string>) (joined: string -> string -> bool) =
        fun (fromRoom: string) (toRoom: string) ->
            not (Set.contains fromRoom impassable)
            && not (Set.contains toRoom impassable)
            && joined fromRoom toRoom

    /// **The predicate every production reader of a chain is built on**
    /// (#382): the memoised join with the stronghold rooms taken out. One
    /// combinator and not two spellings, so the scan set and the refusal
    /// report agree. `linkedBy` below is the bare join a test asks for and
    /// deliberately avoids nothing.
    let reachesUnder (gate: StandDown) (joins: JoinTable) (tuning: Tuning) (world: World) =
        linkedRecalling joins (Tuning.keeperMargin tuning) world
        |> linkedAvoiding gate.Impassable

    /// `linkedRecalling` over a table of this call's own — the shape a test
    /// asks in. The shell never calls this.
    let linkedBy (keeperMargin: int) (world: World) : string -> string -> bool =
        linkedRecalling (JoinTable()) keeperMargin world

    /// What one colony's declaration narrows to this tick, and the union of
    /// it, in four named halves rather than a positional four (two are
    /// `string list`s, so a swapped pair would compile in silence). Declared
    /// inside `World` and never auto-opened, so the names cannot be picked up
    /// by a record literal that meant `Colony`.
    type ScanSet =
        {
            /// The outposts left after both narrowings — the gate's and the
            /// chain's (#259).
            Outposts: Outpost list
            /// The errands left after the one narrowing there is.
            Errands: Errand list
            /// The rooms this colony projects for a child of its own, raised
            /// or re-claimed (#221).
            Borrowed: string list
            /// The scan set: this colony's home and all three of those, the
            /// one place that union is spelled.
            Scanned: string list
        }

    /// The declaration's narrowings and the union they make, for one colony.
    /// The two outpost narrowings answer different questions: the
    /// [[stand-down]] gate is this tick's and reopens, the chain is the
    /// declaration's and never does — a refused room leaves the scan set for
    /// good, and `ColonyView.Refused` says so.
    ///
    /// An errand is narrowed by the chain and by a stand-down on its **own
    /// target room** (#348). A shut transit room deliberately does not reach
    /// this clause: the gate withdraws work in the room it names and is not
    /// a route lock; the body crossing that room reads its live Threat and
    /// Flee instead. ADR-0066
    ///
    /// The whole `Tuning` and not the hop budget alone: the chain is searched
    /// over the **masked** border rings (`Tuning.keeperMargin`).
    let scanRecalling
        (joins: JoinTable)
        (tuning: Tuning)
        (stages: Map<string, ColonyStage>)
        (unowned: Set<string>)
        (colonies: Colony list)
        // The gate whole and not its `Shut` set alone (#382): the scan set
        // asks which rooms are withheld from work, and which of those cannot
        // be crossed either.
        (gate: StandDown)
        (world: World)
        (colony: Colony)
        : ScanSet =
        // One table for both narrowings and every hop inside each: the two
        // filters ask about overlapping chains out of the same home.
        let reaches = reachesUnder gate joins tuning world

        let outposts =
            Outpost.worked gate.Shut colony.Outposts
            |> List.filter (Outpost.routable reaches tuning.MaxHops colony.Home)

        let errands =
            Errand.worked gate.Shut colony.Errands
            |> List.filter (Errand.routable reaches tuning.MaxHops colony.Home)

        // The two halves of what a mother projects for a child of hers,
        // disjoint by construction: a room she is raising is one we own, and
        // a room she may take back is one we do not (#221).
        let borrowed =
            Colony.bootstrapping stages colonies colony
            @ Colony.reclaiming unowned colonies colony

        {
            Outposts = outposts
            Errands = errands
            Borrowed = borrowed
            Scanned = Colony.roomsProjected outposts errands borrowed colony.Home
        }

    /// `scanRecalling` over a table of this call's own (`linkedBy`).
    let scanOf
        (tuning: Tuning)
        (stages: Map<string, ColonyStage>)
        (unowned: Set<string>)
        (colonies: Colony list)
        // The gate whole and not its `Shut` set alone (#382): see
        // `scanRecalling`.
        (gate: StandDown)
        (world: World)
        (colony: Colony)
        : ScanSet =
        scanRecalling (JoinTable()) tuning stages unowned colonies gate world colony

    /// The declared homes that stand empty this tick: ours to take back if
    /// they ever were ours, and the candidates a human means to take. A room
    /// nothing looked into answers no, which is absence and not a claim that
    /// somebody holds it.
    let unownedHomes (colonies: Colony list) (world: World) : Set<string> =
        Colony.homes colonies
        |> List.filter (fun name ->
            (roomOf world name).Control
            |> Option.exists (fun control -> control.Owner = Ownership.Unowned))
        |> Set.ofList

    /// The rooms one colony projects this tick, off the world:
    /// `scanRecalling`'s union with the stages and the ownership it needs
    /// read for it, over the caller's join table.
    let roomsProjectedRecalling
        (joins: JoinTable)
        (tuning: Tuning)
        (colonies: Colony list)
        (gate: StandDown)
        (world: World)
        (colony: Colony)
        : string list =
        let scan =
            scanRecalling
                joins
                tuning
                (stages tuning colonies world)
                (unownedHomes colonies world)
                colonies
                gate
                world
                colony

        scan.Scanned

    /// Which colony holds each creep this tick (`Colony.creepColonies`),
    /// decided over every living colony's scan set at once, or two decisions
    /// would move one body twice. Over the caller's join table: the pairs the
    /// views' own walk answered are read, not re-derived.
    let creepColoniesRecalling
        (joins: JoinTable)
        (tuning: Tuning)
        (colonies: Colony list)
        (running: Colony list)
        (shut: Map<string, Set<string>>)
        (world: World)
        : Map<string, string> =
        let projections =
            running
            |> List.map (fun colony ->
                colony.Home,
                roomsProjectedRecalling
                    joins
                    tuning
                    colonies
                    { StandDown.none with
                        // **Passability is deliberately not asked here** (#382):
                        // a body already standing inside a bunker's room is
                        // exactly the one that must still be attributed to
                        // somebody — two of them were, on the tick this rule
                        // was written for. `Impassable` stays empty.
                        Shut = Map.tryFind colony.Home shut |> Option.defaultValue Set.empty
                    }
                    world
                    colony)

        let spawnHomes =
            world.Rooms
            |> Map.toList
            |> List.collect (fun (name, facts) ->
                facts.Spawns |> List.map (fun spawn -> spawn.Name, name))

        Colony.creepColonies
            projections
            spawnHomes
            (world.Creeps |> List.map (fun creep -> creep.Info.Name, Some creep.Room))

    /// `creepColoniesRecalling` over a table of this call's own (`linkedBy`).
    let creepColonies
        (tuning: Tuning)
        (colonies: Colony list)
        (running: Colony list)
        (shut: Map<string, Set<string>>)
        (world: World)
        : Map<string, string> =
        creepColoniesRecalling (JoinTable()) tuning colonies running shut world
