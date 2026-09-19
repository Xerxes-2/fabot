/// The tick's raw facts, per room and per creep: hostiles and invader cores,
/// construction sites, our own bodies, and the `World` that holds every room
/// seen this tick beside the sighting recalled from the last (ADR 0004).
[<AutoOpen>]
module Fabot.Core.Types.Sightings

/// What the decision layer knows about one construction site this tick.
type ConstructionSiteInfo =
    {
        Id: string
        /// The energy still owed before this site becomes a structure —
        /// `progressTotal - progress`, one number rather than two, because
        /// nothing decides on how far along a site is, only on what is left to
        /// pay (#364).
        ///
        /// It is what tells a road's 300 from a terminal's 100,000, and a row
        /// that could not tell them apart hired two bodies at either: W13S28's
        /// terminal sat at 3,836/100,000 for thousands of ticks with 535,748
        /// energy banked, two workers in the room and 16,464 T of score
        /// stranded behind it.
        Left: int
    }

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
        /// The core's level, and what tells a **stronghold** from the level-0
        /// expansion core beside it (#382). A level-1-and-up core is a bunker:
        /// towers under million-hit ramparts, a garrison of 25-part Invaders,
        /// and a room nothing of ours crosses alive. A level-0 core has none
        /// of that — no tower, no rampart, no garrison — and a body walks past
        /// it. Live W15S26 carried a `bunker4` and W15S27 the level-0 core the
        /// same stronghold expanded into, both on the same collapse clock, so
        /// the clock cannot tell them apart and this can.
        Level: int
    }

/// Whose flag a visible sector Reactor carries. Unlike `Ownership`, the rival
/// case keeps the engine's username: this is operator attribution, not a
/// decision's ours-or-not question (#320).
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
    ///
    /// **Which clock the expiry came off, and nothing about the room** —
    /// whether a bunker stands in it is `OutpostEpisode.Stronghold`'s to say
    /// (#382). The two were briefly one field and it was wrong: `sight`
    /// overwrites the basis whenever a later deadline arrives, so a room whose
    /// raid outlived its core would have gone back to being crossed with four
    /// towers standing in it.
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

/// A creep already paid for and still in a spawn's oven. Its generated name is
/// retained because two rows may buy the same part counts (#319).
type CastingInfo = { Name: string; Body: BodyPart list }

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
        /// The sector Reactors visible in this room, with the richer facts used
        /// only by the global Reactor observation channel (#320). Decisions
        /// continue to read the narrower `Owners` map above.
        Reactors: ReactorInfo list
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
        /// The names and bodies still gestating in this room's spawns: energy
        /// the colony has **already spent** on a creep that is not alive yet.
        /// Filed under the room rather than under the spawn building them,
        /// because a colony banks in one room (ADR 0052 decision 1) and every
        /// row's gap is a colony number. The name preserves the row that bought
        /// an otherwise identical body (#319). Empty wherever no spawn of ours
        /// is mid-cast, and empty is the whole of "nothing is being cast here".
        Casting: CastingInfo list
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
        /// Deferred, because the tick a room is **seen** nobody reads this:
        /// both readers ask only about a sighting older than the tick they run
        /// in (`Facts.errandRoomOf` tests `sighting.Tick < view.Time`, and the
        /// errand narrowing in `ColonyView.ofWorld` intersects a remembered
        /// one). The set is built the tick the room goes dark and the grace
        /// starts asking, and `Lazy` keeps it built after that — so the world
        /// stops paying a set of several hundred ids a room a tick for an
        /// answer nobody wants yet (#371, the shape #371's safe set had).
        ///
        /// The narrowing above still holds where it matters: the type a rule
        /// can read is still `Set<string>` and there is still no kind in it.
        /// What the closure captures is the projection's own census, which the
        /// shell built this tick anyway.
        Targets: Lazy<Set<string>>
    }

/// Everything this tick was seen to hold, once (ADR 0052 decision 1). The shell
/// builds one (`World.ofGame`, the only code that touches `Game`) and
/// `ColonyView.ofWorld` cuts one colony's share of it; `decide` is written
/// against a view and never against the world, so no rule can reach a room its
/// colony does not work.
/// `World.linkedRecalling`'s memo: `(keeper margin, from, to)` to whether a
/// crossing joins the pair. Mutable and heap-only, like `WalkTable`, and the
/// **shell's**: one table for the life of the process, handed to every reader
/// of every colony on every tick, because every answer in it is the terrain's
/// (`linkedRecalling` says why nothing in it goes stale). A test asks through
/// `linkedBy`, `scanOf`, `creepColonies` and `ColonyView.ofWorld`, which lay a
/// table of their own per call, or lays one inside the test body; what is
/// forbidden is a **static** holding one, which would be shared across
/// Expecto's parallel lists (#310, `ParallelSafetyTests`).
type JoinTable = System.Collections.Generic.Dictionary<int * string * string, bool>

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

    /// The same reading one layer in: a tile of a room's own **ground** a body
    /// could stand on — terrain the layer carries that is not wall and which no
    /// declared keeper rock masks (ADR 0062). The [[world]]'s half of the
    /// landing test `Atlas.seams` answers off its own raw terrain grid,
    /// and it is terrain alone for the reason that one gives: a band is
    /// geometry, so a structure raised this tick must not move it.
    ///
    /// This is where the world reaches past its border maps, which is the cost
    /// ADR 0062 accepted. It costs no new plumbing: `World.ofGame` already
    /// reads terrain for every declared and transit room whether or not there
    /// is vision (ADR 0031, ADR 0041), so the ground behind a landing tile has
    /// been in hand on every tick the ring was.
    let groundWalkable (keeperMargin: int) (room: string) (terrain: TerrainGrid) : Pos -> bool =
        let masked = Keepers.maskIn keeperMargin room

        fun tile ->
            match TerrainGrid.tryFind tile terrain with
            | Some ground -> ground <> Wall && not (masked tile)
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
    ///
    /// Since ADR 0062 the far side is asked **twice**: its ring, for whether
    /// the engine lands a body there at all, and its ground, for whether the
    /// body can then step off the landing. That is the mask's own artefact —
    /// a rock six tiles inside a border masks the row behind an exit row it
    /// does not reach, so a ring keeps crossings whose ground is gone — and
    /// before that ADR this answered **true** for W15S26 and the room across
    /// its east border, where every one of the seven surviving crossings is
    /// such an orphan (#326).
    let linked (keeperMargin: int) (world: World) (fromRoom: string) (toRoom: string) : bool =
        let walkableIn room =
            ringWalkable keeperMargin room (roomOf world room).Border

        Seam.joinedBy
            (walkableIn fromRoom)
            (walkableIn toRoom)
            (groundWalkable keeperMargin toRoom (roomOf world toRoom).Layer.Terrain)
            fromRoom
            toRoom

    /// `linked` over a table the caller holds, and holds **across ticks**: the
    /// route search asks per border and re-asks per hop, and three readers ask
    /// it per colony per tick — the scan set, the refusals and the creeps'
    /// filing (`scanOf`, `ColonyView.ofWorld`, `creepColonies`) — which came
    /// to 93 calls over 26 distinct pairs in one colony's tick before any
    /// table stood (`docs/research/cpu-headroom.md`, its candidate 2), and to
    /// three tables a colony a tick, each built and thrown away, once one did:
    /// 5.7% of a `reactor --level 7` tick, all of it outside `decide`
    /// (`docs/profiling.md` § History, 2026-09-18).
    ///
    /// The table can outlive the tick because of what an answer reads and what
    /// it does not. It reads two rooms' border rings and one room's ground,
    /// which are terrain off ADR 0031's memo — the engine never changes a
    /// room's terrain — under a keeper margin that is in the key. What *can*
    /// move between ticks is which rooms the world holds at all: a room the
    /// world carries nothing for has no ring and is joined to nothing
    /// (`roomOf`, ADR 0004), and that is this tick's fact. So it is read off
    /// `Rooms` ahead of the table on every ask, and the table is asked only
    /// for a pair the world holds both rooms of — an answer filed there is
    /// the terrain's and stands for the life of the process, and a room that
    /// left the world is joined to nothing whatever the table remembers.
    /// `JoinTable`'s doc says whose the table is.
    ///
    /// Asymmetric by construction, like `linked` itself: `A B` and `B A` are
    /// two questions (`Declaration.routable` asks both, ADR 0062), so the key
    /// is the ordered pair and an answer is never reused backwards.
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

    /// **A room a stronghold holds is not a link** (#382, ADR 0074). ADR 0066
    /// decided that a stand-down does not propagate through a route, and it is
    /// right about its own case: the gate ordinarily withholds *work in a
    /// room*, which a body crossing that room does not do, and propagating a
    /// coarse outpost clock to the route would stop the Reactor's supply for
    /// something that never touched the walk. A stronghold is the case that
    /// reasoning does not cover — four towers under million-hit ramparts reach
    /// every tile of the room, and a `[20 Carry; 10 Move]` courier carries
    /// 3,000 hits. Live, a `bunker4` in W15S26 killed two 650-energy
    /// re-claimers on the same entry tile 161 ticks apart while the gate had
    /// the room correctly shut and the relay walked through it anyway.
    ///
    /// Asked of **both** ends of every hop, so such a room is neither entered
    /// nor left. What follows needs no rule of its own: a declaration whose
    /// every chain crossed it stops being `routable`, `Errand.refused` and
    /// `Outpost.refused` withhold it as a unit and say so, and the tick the
    /// core's own collapse timer runs out the room re-links and the
    /// declaration comes back.
    let linkedAvoiding (impassable: Set<string>) (joined: string -> string -> bool) =
        fun (fromRoom: string) (toRoom: string) ->
            not (Set.contains fromRoom impassable)
            && not (Set.contains toRoom impassable)
            && joined fromRoom toRoom

    /// **The predicate every production reader of a chain is built on** (#382):
    /// the memoised join, with the rooms a stronghold holds taken out of it.
    /// One combinator and not two spellings, because the scan set and the
    /// refusal report have to agree — a report that named different refusals
    /// than the set made would be a declaration vanishing with nothing said
    /// about why — and because a third reader should inherit the rule rather
    /// than silently skip it. `linkedBy` below is the bare join a test or a
    /// one-off asks for and deliberately avoids nothing.
    let reachesUnder (gate: StandDown) (joins: JoinTable) (tuning: Tuning) (world: World) =
        linkedRecalling joins (Tuning.keeperMargin tuning) world
        |> linkedAvoiding gate.Impassable

    /// `linkedRecalling` over a table of this call's own — the shape a test
    /// or a one-off asks in, the way `Atlas.ofView` is `ofViewRecalling` over
    /// fresh tables. The shell never calls this: it holds one table for the
    /// life of the process and hands it to every reader.
    let linkedBy (keeperMargin: int) (world: World) : string -> string -> bool =
        linkedRecalling (JoinTable()) keeperMargin world

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
    /// An errand is narrowed by the chain and by a stand-down on its **own
    /// target room** (#348). The latter is the room no guard row serves: an
    /// armed player standing there is a withdrawal rather than a hypothetical
    /// fight, and withholding the declaration removes its target, its Reclaim
    /// and the reserver-row seat together. A shut transit room deliberately
    /// does not reach this clause (#325, ADR 0066): the gate withdraws work in
    /// the room it names and is not a route lock. The body crossing that room
    /// reads its live Threat and Flee instead.
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
    let scanRecalling
        (joins: JoinTable)
        (tuning: Tuning)
        (stages: Map<string, ColonyStage>)
        (unowned: Set<string>)
        (colonies: Colony list)
        // The gate whole and not its `Shut` set alone (#382): the scan set
        // asks it two different questions — which rooms are withheld from
        // work, and which of those cannot be crossed either — and a caller
        // handing over one set could only answer the first.
        (gate: StandDown)
        (world: World)
        (colony: Colony)
        : ScanSet =
        // One table for both narrowings and for every hop inside each
        // (`linkedRecalling`): the two filters ask about overlapping chains
        // out of the same home, so the pairs they share are asked once — and
        // the table is the caller's, so they are asked once across every
        // reader and every tick that hands the same one in. The outposts and
        // the errands are still two filters, because they are two declaration
        // kinds and the failure sizes differ (ADR 0060).
        let reaches = reachesUnder gate joins tuning world

        let outposts =
            Outpost.worked gate.Shut colony.Outposts
            |> List.filter (Outpost.routable reaches tuning.MaxHops colony.Home)

        let errands =
            colony.Errands
            |> List.filter (fun errand -> not (Set.contains errand.RoomName gate.Shut))
            |> List.filter (Errand.routable reaches tuning.MaxHops colony.Home)

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

    /// `scanRecalling` over a table of this call's own (`linkedBy`).
    let scanOf
        (tuning: Tuning)
        (stages: Map<string, ColonyStage>)
        (unowned: Set<string>)
        (colonies: Colony list)
        // The gate whole and not its `Shut` set alone (#382): the scan set
        // asks it two different questions — which rooms are withheld from
        // work, and which of those cannot be crossed either — and a caller
        // handing over one set could only answer the first.
        (gate: StandDown)
        (world: World)
        (colony: Colony)
        : ScanSet =
        scanRecalling (JoinTable()) tuning stages unowned colonies gate world colony

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

    /// The rooms one colony projects this tick, off the world:
    /// `scanRecalling`'s union with the stages and the ownership it needs
    /// read for it, over the caller's join table (`JoinTable`).
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

    /// Which colony holds each creep this tick (`Colony.creepColonies`, ADR
    /// 0047 decision 2), decided over every living colony's scan set at once
    /// and handed to each view: a creep is one colony's business, or two
    /// decisions would move one body twice. Over the caller's join table
    /// (`JoinTable`): every colony's scan set is walked here a second time in
    /// the tick, and the pairs the views' own walk answers are read, not
    /// re-derived.
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
                        // this reader answers which colony each body belongs
                        // to, and a body already standing inside a bunker's
                        // room is exactly the one that must still be
                        // attributed to somebody — two of them were, on the
                        // tick this rule was written for. `Impassable` stays
                        // empty, and the map this reader is handed carries
                        // only `Shut` in any case.
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
