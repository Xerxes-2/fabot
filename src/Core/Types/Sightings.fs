/// The tick's raw facts, per room and per creep: hostiles and invader cores,
/// construction sites, our own bodies, and the `World` that holds every room
/// seen this tick beside the sighting recalled from the last (ADR 0004).
[<AutoOpen>]
module Fabot.Core.Types.Sightings

/// What the decision layer knows about one construction site this tick.
type ConstructionSiteInfo = { Id: string }

/// What the decision layer knows about one hostile creep in a room the colony
/// is looking into this tick: its id and tile (what the fire reflex aims at,
/// ADR 0014), its body parts verbatim because what a hostile can do is decided
/// from what it is made of, its owner for the Raid log's roster (ADR 0028), and
/// the room it stands in for that log's closest approach (ADR 0041).
type HostileInfo =
    {
        Id: string
        /// Whose creep this is, as the engine spells the username ("Invader"
        /// for the NPCs). The Raid log's roster is attribution, and
        /// attribution is a name (ADR 0028); no reflex reads it.
        Owner: string
        /// Where it stands, room and tile in one (ADR 0052 decision 2). A bare
        /// `Pos` carries no room, so the Raid log's closest approach would read
        /// one of ours on the same coordinate of another room as range 0;
        /// `RoomPos.range` answers None across the border instead.
        Pos: RoomPos
        Body: BodyPart list
        /// Ticks this hostile has left. An Invader standing in a room nobody
        /// owns never suicides — the engine's own suicide branch wants a
        /// controller owner and an [[outpost]] has none — so what it has left
        /// is exactly what it will spend, and this is the one deadline a raid
        /// with no core in it offers ADR 0043's [[stand-down]] (#257).
        TicksToLive: int
    }

/// An NPC invader core standing in a room the colony works this tick (ADR
/// 0043). A **structure**, not a creep, so it reaches the projection through
/// neither `Hostiles` nor the fire reflex, whose sweep is
/// `FIND_HOSTILE_CREEPS`. It is the threat an [[outpost]] is stood down from —
/// 100,000 hits, no creeps at level 0, and it never leaves — and the clock the
/// stand-down runs to is read off it while there is still vision. A room the
/// colony cannot see contributes no entry (ADR 0004).
type InvaderCoreInfo =
    {
        /// The room it stands in. The colony works rooms, not tiles, so
        /// this is the whole of where.
        RoomName: string
        /// The **absolute** tick the core's collapse timer runs out at, or None
        /// where it carries none — an expanded level-0 core has no stronghold
        /// to collapse, so the deadline is read off the reservation it took
        /// instead (ADR 0043's fallback order), which is why
        /// `ReservationHolder.Invader` is a case of its own. That is the common
        /// case on the frontier.
        CollapseTick: int option
    }

/// Which deadline an [[outpost]]'s [[stand-down]] runs to — the provenance of
/// the tick, carried beside it because it cannot be recovered from the tick
/// afterwards, and an operator asking why an outpost is shut is asking exactly
/// that. The first three are ADR 0043's own fallback order for a threat, best
/// first; the fourth is not a threat at all and joined them with #165. A closed
/// vocabulary and not a string, for the reason `Ownership` gives, and it crosses
/// the wire, so it is spelt once in `standDownBasisName` and round-tripped
/// against the union itself by `Core.Tests`.
[<RequireQualifiedAccess>]
type StandDownBasis =
    /// The core's own `EFFECT_COLLAPSE_TIMER`: the tick the engine put on
    /// the stronghold that expanded here, and the first answer wherever
    /// it can be read.
    | CollapseTimer
    /// The end of the reservation the core took with `attackController` —
    /// what a level-0 core answers with, having no stronghold to collapse
    /// and so no timer. The measured case on this colony's frontier, not
    /// the rare one (docs/research/remote-mining.md §8.4).
    | Reservation
    /// Neither deadline was readable, so the clock is the 2,500-tick
    /// stronghold expansion period. The one answer the colony chose rather than
    /// read, and it errs long deliberately: ADR 0043's gate may be wrong only
    /// in the direction that costs an outpost's income.
    | Fallback
    /// The end of the [[reservation]] **another player** holds on the room
    /// (#165). Not a threat with a clock but a room somebody else is working,
    /// and the engine's own countdown says when it stops being one — which is
    /// why this withdrawal is a clock where ADR 0043 wrote a latch: a passing
    /// reserver is a routine event where an owner is not, and a hold that
    /// decays is reversible where ownership is not. It takes no floor under
    /// it, unlike `Reservation` above: the Invader's core re-takes the hold it
    /// lets lapse, so the hold is never the end of the core, while a player's
    /// claimer that stops coming leaves nothing behind it at all.
    | RivalReservation
    /// A raid of plain [[invader]] creeps, clocked off the longest life among
    /// them (#257). An Invader in a room nobody owns never suicides — the
    /// engine's `findAttack` suicide branch wants a controller owner and an
    /// [[outpost]] has none — so what it has left is exactly what it will
    /// spend, and this is the one deadline a raid with no core in it offers.
    /// Read only once the colony has stopped fighting for the room: while a
    /// [[guard]] of ours stands there, or the episode has not yet spent the
    /// casts ADR 0056 bounds it at, the room is a fight and not a withdrawal.
    | InvaderRaid

/// What the decision layer knows about one owned creep this tick.
type CreepInfo =
    {
        Name: string
        /// Ticks the creep still has to live. A creep still spawning is
        /// outside the projection, so a projected creep always carries a real
        /// count. The fact, not the judgement: whether it is expiring is this
        /// count measured against the lead its replacement needs (ADR 0026).
        TicksToLive: int
        /// Fatigue points still to pay off; a creep with any cannot step
        /// this tick — the engine's move answers ERR_TIRED.
        Fatigue: int
        /// Current and maximum life; self-healing reads damage independently
        /// of the active body counts (partially damaged parts still work).
        Hits: HitsInfo
        /// Energy currently carried.
        Energy: int
        /// Thorium currently carried (ADR 0057 decision 3): a **second field
        /// beside `Energy`** and never a resource key inside it, for the reason
        /// `SpatialInfo.Thorium` gives — every existing reader asks about energy
        /// and would otherwise grow a question it never asks.
        ///
        /// It is here because a decision reads it (ADR 0007): **a body carries
        /// one resource at a time**. The Thorium arm of [[withdraw]] is
        /// applicable to an *empty* store and not #232's half-empty one, the
        /// Thorium arm of [[refill]] to a body holding some, and the three
        /// energy intakes are shut to a body holding any — a mixed load pours
        /// energy into a reactor that refuses it and arrives at the decade cliff
        /// with the wrong count in its store. Derived arithmetic would not do:
        /// `FreeCapacity` is the engine's whole-store answer, so the Thorium a
        /// body holds is `capacity - free - energy` only where the three agree,
        /// and a hand-built fixture is free to state a body that no engine would
        /// hand back.
        Thorium: int
        /// Carry capacity still free (0 = full) — the **whole** store's, a
        /// creep's store being general: a body holding 999 Thorium of 1,200
        /// reports 201 free and no energy at all.
        FreeCapacity: int
        /// Active part counts (body entries with hits > 0). Absent or zero
        /// means the creep has no usable part of that kind.
        Body: Map<BodyPart, int>
        /// Whether the creep stands on a different tile from last tick — the
        /// shell's reading of Memory's last positions, false for a body born
        /// this tick and in every hand-built fixture. The one reader is the
        /// occupancy surcharge (ADR 0008 as #225 amends it): two bodies each
        /// detouring around the other's last tile never pass.
        Moved: bool
    }

/// What one room holds this tick, to everybody: the half a declaration carries
/// and the half vision pays for, filed under the room's own name and saying
/// nothing about who is looking at it (ADR 0052 decision 1). The unit the
/// `World` is a map of and a [[colony view]] takes its share of — a room two
/// colonies both work contributes one of these and never two. Every list here
/// is *this room's*: the shell scopes what the engine does not scope itself.
/// Absence stays per entry (ADR 0004) and reaches down two levels — a room the
/// world holds nothing for reads `RoomFacts.empty`, and one it holds terrain
/// for but has no vision in reads that terrain beside empty everything else.
type RoomFacts =
    {
        /// This room's geometry (ADR 0041): terrain, the targets standing
        /// on it, the tiles our creeps stand on — **every** creep of ours
        /// in the room and not one colony's, which is what makes this the
        /// world's answer and lets a view file the rest under `Foreign`.
        Layer: RoomLayer
        /// The room's border ring, the Seam's terrain and never ground
        /// (ADR 0036, ADR 0041).
        Border: Map<Pos, Terrain>
        /// The kind of each target standing in this room, under the engine's
        /// own id. Id-keyed within the room and merged unlayered into a
        /// view's `SpatialInfo` (ADR 0041): an object id is unique across the
        /// world, so the layer that holds it *is* the room it stands in.
        TargetKinds: Map<string, TargetKind>
        /// Current/max hits of the repairable kinds standing here (ADR
        /// 0010, ADR 0012, ADR 0034).
        Hits: Map<string, HitsInfo>
        /// Energy currently stored, per store standing here: the
        /// containers and the Storage, the piles, the tombstones and the
        /// ruins.
        Stores: Map<string, int>
        /// Thorium currently held, per store standing here, and the remaining
        /// amount of the Thorium mineral itself (ADR 0057 decision 3). A
        /// second map beside `Stores` and never a resource key inside it, for
        /// the reason `SpatialInfo.Thorium` gives.
        Thorium: Map<string, int>
        /// Ticks before a structure standing here may act again — the
        /// extractor's cooldown, which is the one clock a decision of ours
        /// reads off a structure (ADR 0057 decision 2).
        Cooldowns: Map<string, int>
        /// Whose each **object** standing here is (#318) — the per-object twin
        /// of `Control`'s room ownership, and the fact the sector Reactor's room
        /// has no controller to answer with. Filled for the reactors alone
        /// today, for the reason `SpatialInfo.Owners` gives, and merged into a
        /// view's projection id-keyed and unlayered like the stores beside it.
        Owners: Map<string, Ownership>
        /// Who holds the room, and whether its safe mode is running (ADR
        /// 0042) — `None` for a room nothing looked into this tick, which
        /// is not the same fact as a room nobody holds (ADR 0004).
        Control: RoomControlInfo option
        /// The controller of this room **while it is ours**: the fact a
        /// colony's own Upgrade, its downgrade clock and its safe-mode reflex
        /// are read off, and the level a [[stage]] is derived from. `None` for
        /// a room we do not own, whose ownership and reservation are
        /// `Control`'s and are what price a source there (ADR 0042).
        Controller: ControllerInfo option
        /// The room's shared spawn-energy account. Zero for a room with no
        /// spawn or extension in it, which is every room we do not own —
        /// the engine's own answer, and the one a colony reads as an empty
        /// bank.
        Energy: RoomEnergy
        /// Our spawns standing in this room. The world's, so a spawn standing
        /// in a [[nursery]] a mother is raising is a fact about that room and
        /// not about her — which is exactly what ends the nursery.
        Spawns: SpawnInfo list
        /// The bodies still gestating in this room's spawns: energy the colony
        /// has **already spent** on a creep that is not alive yet. Bodies and
        /// not creeps, and filed under the room rather than under the spawn
        /// building them, because a colony banks in one room (ADR 0052 decision
        /// 1) and every row's gap is a colony number. Empty wherever no spawn
        /// of ours is mid-cast, and empty is the whole of "nothing is being
        /// cast here".
        Casting: BodyPart list list
        /// Our energy-hungry structures standing here (spawn, extension,
        /// tower), whether or not they currently have room.
        Refillables: RefillableInfo list
        /// The sources in this room, as vision answered for them. What a
        /// *declaration* answers for is laid over a colony's projection
        /// instead (`Outpost.place`, `Outpost.pooledSources`), because which
        /// rooms are worked is a colony's question and not the world's.
        Sources: SourceInfo list
        /// Our construction sites standing here (#150).
        ConstructionSites: ConstructionSiteInfo list
        /// Hostile creeps standing here this tick (ADR 0033, #201).
        Hostiles: HostileInfo list
        /// The invader cores standing here (ADR 0043, #201).
        InvaderCores: InvaderCoreInfo list
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RoomFacts =
    /// A room the world holds nothing for — every entry absent, which is
    /// what a room outside the scan set and a room with no vision both
    /// read as (ADR 0004).
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

/// One creep of ours, in the world's reading: what it is made of and where it
/// stands, before any colony has claimed it (ADR 0052 decision 1).
type WorldCreep =
    {
        /// The room the engine says the creep stands in — its own answer, so
        /// a creep in a room no colony works still carries the name of the
        /// room it is in (`World.creepColonies` decides whose it then is).
        Room: string
        /// What the decision layer knows about the body itself.
        Info: CreepInfo
    }

/// What the world last saw standing in one room, and when (#151): the tick
/// vision last answered for that room, and the bare ids it answered with. The
/// one thing carried **across** ticks about a room, and it is carried for one
/// question only. ADR 0004's absence is per-entry and per-tick, and it is the
/// right answer for everything a rule prices — geometry that cannot be priced
/// counts against no Task and blocks no action — but it cannot say *why* an id
/// left the pool, and the two answers are opposite work: a container destroyed
/// is a Task gone, a room gone dark is a Task waiting. So this is read by the
/// vision grace and by nothing else, and what the grace hands on is a **room
/// name**: the Matcher keeps the assignment and the mover walks its holder at
/// that room's Seam, which is border layer and terrain and needs no vision.
/// Nothing is placed or priced off a sighting, and no stale fact reaches a
/// decision through it.
///
/// The world holds one of these per room it can see and keeps it for the rooms
/// it cannot; which of them reaches a colony is the **view**'s cut, and it is
/// the whole of what keeps the promise above at the colony's altitude
/// (`ColonyView.ofWorld`, #271): a room this colony does not work carries no
/// memory here, and neither does a room it only **crosses** on the way to one
/// it does — an id taken out of the projection is not one the grace may hold a
/// body to.
type RoomSighting =
    {
        /// The tick vision last answered for the room. Equal to the world's
        /// own `Time` for a room seen this tick, which is how a reader tells
        /// a room it can see from one it is remembering.
        Tick: int
        /// The ids that stood in the room that tick, and nothing about them:
        /// the keys of its `RoomFacts.TargetKinds`, with the kinds dropped on
        /// the way in. Set membership is the only question the grace asks, so
        /// carrying the kinds would be a field no decision reads, which is the
        /// growth ADR 0007's rule refuses — and it is the narrowing that puts
        /// the promise above into the *type*: there is no stale kind here for
        /// the next rule to read one out of. The view's cut is written off the
        /// **room**, never off what stands in it, so it needs none either.
        Targets: Set<string>
    }

/// Everything this tick was seen to hold, once (ADR 0052 decision 1). The shell
/// builds one (`World.ofGame`, the only code that touches `Game`) and
/// `ColonyView.ofWorld` cuts one colony's share of it; `decide` is written
/// against a view and never against the world, so no rule can reach a room its
/// colony does not work.
type World =
    {
        Time: int
        /// Every room the world holds anything for this tick, under its own
        /// name: the declared rooms, whose terrain and furniture need no vision
        /// (ADR 0041), and every room the engine answered `Game.rooms` with. A
        /// room absent here reads `RoomFacts.empty` (ADR 0004).
        Rooms: Map<string, RoomFacts>
        /// Every creep we own that is not still gestating, in the engine's
        /// own order. Whose each one is this tick is `World.creepColonies`'
        /// answer and is not stored here: the rule needs the [[stand-down]]
        /// gate, read out of Memory rather than off the world (ADR 0043).
        Creeps: WorldCreep list
        /// What each room was last seen to carry, under its own name (#151):
        /// this tick's census for every room vision answered for, and the last
        /// one taken for a room it did not. Heap state in the shell — beside
        /// the plan memos and the terrain memo, and deliberately not a Memory
        /// leaf — so a global reset empties it and the colony decides exactly
        /// as it did before the grace existed. A room never seen has no entry:
        /// the absence is per-entry here too (ADR 0004).
        Sightings: Map<string, RoomSighting>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module World =
    /// A world holding nothing: no room, no creep. What a test builds up
    /// from, and what a tick before any vision would read as.
    let empty: World =
        {
            Time = 0
            Rooms = Map.empty
            Creeps = []
            Sightings = Map.empty
        }

    /// This tick's world with what it saw **before** laid under it (#151):
    /// every room vision answered for this tick keeps this tick's sighting,
    /// and a room it did not answer for keeps the last one taken. The shell
    /// carries the previous map on the heap and hands it back here, so the
    /// merge is one pure function a test can drive rather than a line buried
    /// in the only code that reads `Game`. This tick wins every entry it
    /// holds, exactly as vision wins over a declaration (ADR 0041): a
    /// remembered census is what is left where there was nothing to read.
    /// What is remembered is bounded by the rooms this tick's world holds —
    /// the declared ones and the seen ones — so a room that leaves the world
    /// leaves the memory with it rather than riding a global's whole life in
    /// a map nobody can ask about: every reader of a sighting narrows it to a
    /// colony's scan set, and a scan set is drawn from these rooms.
    let recalling (previous: Map<string, RoomSighting>) (world: World) : World =
        { world with
            Sightings =
                (previous |> Map.filter (fun room _ -> Map.containsKey room world.Rooms),
                 world.Sightings)
                ||> Map.fold (fun carried room sighting -> Map.add room sighting carried)
        }

    /// One room's facts, as ADR 0004 has every other absence: a room the
    /// world carries nothing for reads as a room whose every entry is
    /// absent, never as a lookup that throws.
    let roomOf (world: World) (room: string) : RoomFacts =
        Map.tryFind room world.Rooms |> Option.defaultValue RoomFacts.empty

    /// The rooms we own this tick, off the control entry vision paid for
    /// (ADR 0042). One of the two facts a colony has to pass to be
    /// **living** (`Colony.living`), and the fact a [[stage]] starts from.
    let ownedRooms (world: World) : Set<string> =
        world.Rooms
        |> Map.toList
        |> List.filter (fun (_, facts) ->
            facts.Control |> Option.exists (fun control -> control.Owner = Ownership.Ours))
        |> List.map fst
        |> Set.ofList

    /// The rooms one of our spawns stands in, in room-name order. The other
    /// fact `Colony.living` asks for, and the one a [[stage]] reads as "this
    /// colony can cast for itself".
    let spawnRooms (world: World) : string list =
        world.Rooms
        |> Map.toList
        |> List.filter (fun (_, facts) -> not (List.isEmpty facts.Spawns))
        |> List.map fst

    /// The [[stage]] of every declared colony that is one this tick (ADR 0052
    /// decision 3), derived off the world because a stage decides whether a
    /// mother scans her child's room at all (`Colony.bootstrapping`) — it
    /// cannot be read off a projection the scan set does not exist yet. Asked
    /// of the **declared** homes, which is what the raising rule asks about,
    /// and of every **living** colony's home, because a spawn room no
    /// declaration names is a colony of its own. Not every owned room the world
    /// holds: swept wider, a room we claimed by hand and never declared would
    /// arrive in some colony's scan set as a [[nursery]] to raise.
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

    /// The colonies that run this tick (`Colony.living`, ADR 0047 decision 1),
    /// read off the world's two facts.
    and living (colonies: Colony list) (world: World) : Colony list =
        Colony.living (ownedRooms world) (spawnRooms world) colonies

    /// One room's border ring read as a [[seam]]'s near or far side: a tile the
    /// ring carries whose terrain is not wall and which no declared keeper rock
    /// masks (`Keepers`, ADR 0060 decision 2). The [[world]]'s reading of the
    /// answer `terrainWeight` gives the Atlas's ring grid — -1 for a wall and
    /// for a masked tile, positive for everything else — and written here rather
    /// than inside `linked` because a second caller has to build the same
    /// predicate off a committed capture and a hand copy has now had to move in
    /// lockstep twice (`RoomOutpostTests`, #317). The room rides beside the ring
    /// because the mask is keyed by room name and the ring is not, and the
    /// answer comes back as a **closure over one room** because that is how
    /// both callers use it: the ring is resolved once and read forty-eight
    /// times, and the declaration's own lookup is resolved with it.
    let ringWalkable (keeperMargin: int) (room: string) (border: Map<Pos, Terrain>) : Pos -> bool =
        // Resolved once and asked forty-eight times, the mask included: the
        // room's declaration is a lookup and the band is a loop, so `maskIn`
        // takes it here rather than on every tile.
        let masked = Keepers.maskIn keeperMargin room

        fun tile ->
            match Map.tryFind tile border with
            | Some terrain -> terrain <> Wall && not (masked tile)
            | None -> false

    /// Whether a creep could step from one room into the other: the [[world]]'s
    /// own reading of a [[seam]] band, off the border maps it holds per room
    /// and before any [[atlas]] grid exists (ADR 0058). The Atlas answers the
    /// same question off its ring grids (`Atlas.seams`); both go through
    /// `Seam.joinedBy`, so the scan set and the price cannot disagree about
    /// which rooms are joined.
    ///
    /// A room the world holds no facts for is joined to nothing, which is what
    /// keeps the route search inside the rooms `Outpost.roomsProjected` put
    /// there: `World.ofGame` reads terrain for every declared and transit room
    /// whether or not there is vision (ADR 0031, ADR 0041), so what this can
    /// see is exactly what a walk could use.
    ///
    /// The [[keeper margin]] is taken off the ring here as the Atlas takes it
    /// off its own (`Keepers`, ADR 0060 decision 2), which is why the margin is
    /// an argument: a mask near a room's border can take exit tiles out of a
    /// band, so a chain that exists over raw terrain may not exist over masked
    /// terrain — and the routable question has to be asked over the **same**
    /// masked layer every price will use, or the scan set admits a chain the
    /// flood cannot walk.
    let linked (keeperMargin: int) (world: World) (fromRoom: string) (toRoom: string) : bool =
        let walkableIn room =
            ringWalkable keeperMargin room (roomOf world room).Border

        Seam.joinedBy (walkableIn fromRoom) (walkableIn toRoom) fromRoom toRoom

    /// What one colony's declaration narrows to this tick, and the union of it:
    /// `scanOf`'s whole answer, in four named halves rather than a positional
    /// four. Declared inside `World` and never auto-opened, so the names below
    /// cannot be picked up by a record literal that meant `Colony`.
    type ScanSet =
        {
            /// The outposts left after both narrowings — the gate's and the
            /// chain's (ADR 0043, #259).
            Outposts: Outpost list
            /// The errands left after the one narrowing there is (ADR 0060
            /// decision 1).
            Errands: Errand list
            /// The rooms this colony projects for a child of its own, raised or
            /// re-claimed (ADR 0047 decision 4, #221).
            Borrowed: string list
            /// The scan set: this colony's home and all three of those, which
            /// is the one place that union is spelled.
            Scanned: string list
        }

    /// The declaration's narrowings and the union they make, for one colony:
    /// the [[outpost]]s the [[stand-down]] gate leaves it (ADR 0043) and its
    /// home reaches inside the hop budget (`Outpost.withinHopBudget`, ADR 0058),
    /// the [[errand]]s a chain reaches (`Errand.routable`, ADR 0060), the rooms
    /// it is bootstrapping for a child of its own (ADR 0047 decision 4), and its
    /// scan set — its home and all of those. The two outpost narrowings are one
    /// clause apiece and answer different questions: the gate is this tick's
    /// and reopens, the border is the declaration's and never does — so a
    /// refused room leaves the scan set for good, taking its furniture, its
    /// pooled rock, its Reserve and the reserver the row would have hired for
    /// it (ADR 0042) with it, which is the whole of "refuse it loudly" that a
    /// scan set can carry. What says so out loud is `ColonyView.Refused`.
    ///
    /// An errand is narrowed **once** where an outpost is narrowed twice: the
    /// [[stand-down]] has nothing to withhold from it (ADR 0060 decision 1 —
    /// no row hires per errand on a per-tick fact), so the gate never reaches
    /// it and the chain is the whole of its admission.
    ///
    /// A **record** and not a tuple, since ADR 0060 gave the answer a fourth
    /// member: two of the four are `string list`s standing side by side and
    /// both callers destructure positionally, so a swapped pair would compile
    /// in silence and hand the rooms borrowed for a child to the reader that
    /// asked for the whole scan set. The field names are the check the compiler
    /// can make and the tuple could not.
    ///
    /// The whole `Tuning` and not the hop budget alone since ADR 0060: the
    /// chain is searched over the **masked** border rings, so the routable
    /// question reads `Tuning.keeperMargin` beside `Tuning.MaxHops`.
    let scanOf
        (tuning: Tuning)
        (stages: Map<string, ColonyStage>)
        (unowned: Set<string>)
        (colonies: Colony list)
        (shut: Set<string>)
        (world: World)
        (colony: Colony)
        : ScanSet =
        let outposts =
            Outpost.worked shut colony.Outposts
            |> List.filter (
                Outpost.routable
                    (linked (Tuning.keeperMargin tuning) world)
                    tuning.MaxHops
                    colony.Home
            )

        let errands =
            colony.Errands
            |> List.filter (
                Errand.routable
                    (linked (Tuning.keeperMargin tuning) world)
                    tuning.MaxHops
                    colony.Home
            )

        // The two halves of what a mother projects for a child of hers, and
        // they are disjoint by construction: a room she is raising is one we
        // own, and a room she may take back is one we do not (#221).
        let borrowed =
            Colony.bootstrapping stages colonies colony
            @ Colony.reclaiming unowned colonies colony

        {
            Outposts = outposts
            Errands = errands
            Borrowed = borrowed
            Scanned = Colony.roomsProjected outposts errands borrowed colony.Home
        }

    /// The declared homes that stand empty this tick: ours to take back if
    /// they ever were ours, and the candidates a human means to take. Read off
    /// the same control entry ownership is read off everywhere (ADR 0042); a
    /// room nothing looked into answers no, which is ADR 0004's absence and not
    /// a claim that somebody holds it.
    let unownedHomes (colonies: Colony list) (world: World) : Set<string> =
        Colony.homes colonies
        |> List.filter (fun name ->
            (roomOf world name).Control
            |> Option.exists (fun control -> control.Owner = Ownership.Unowned))
        |> Set.ofList

    /// The rooms one colony projects this tick, off the world: `scanOf`'s
    /// union with the stages and the ownership it needs read for it.
    let roomsProjected
        (tuning: Tuning)
        (colonies: Colony list)
        (shut: Set<string>)
        (world: World)
        (colony: Colony)
        : string list =
        let scan =
            scanOf
                tuning
                (stages tuning colonies world)
                (unownedHomes colonies world)
                colonies
                shut
                world
                colony

        scan.Scanned

    /// Which colony holds each creep this tick (`Colony.creepColonies`, ADR
    /// 0047 decision 2), decided over every living colony's scan set at once
    /// and handed to each view: a creep is one colony's business, or two
    /// decisions would move one body twice.
    let creepColonies
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
                roomsProjected
                    tuning
                    colonies
                    (Map.tryFind colony.Home shut |> Option.defaultValue Set.empty)
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
