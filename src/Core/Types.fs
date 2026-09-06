module Fabot.Core.Types

/// A creep body part, the engine's full vocabulary. Our own bodies use
/// only Work/Carry/Move today; the rest arrive on hostile creeps, whose
/// parts the Snapshot projects verbatim.
///
/// `Claim` is spelled `BodyPart.Claim` wherever it means a part, because
/// `Task.Claim` (ADR 0047) shares the name and is declared later, so the
/// bare word resolves to the Task. Both names are the engine's own — the
/// part is CLAIM and the act is `claimController` — so neither was renamed
/// to dodge the other, and the qualification is where the two are told
/// apart. The whole union is not `RequireQualifiedAccess` for the reason
/// the qualification is bearable: `Work`, `Carry` and `Move` are written
/// in every body literal in the codebase and collide with nothing.
type BodyPart =
    | Work
    | Carry
    | Move
    | Attack
    | RangedAttack
    | Heal
    | Claim
    | Tough

/// What the decision layer knows about one spawn this tick.
type SpawnInfo =
    {
        Name: string
        /// Game-object id of the spawn structure — the key that locates
        /// this spawn in the spatial projection's target maps.
        Id: string
        /// Name of the room the spawn stands in — the key into the
        /// Snapshot's RoomEnergy banks.
        RoomName: string
        IsSpawning: bool
    }

/// One room's shared spawn-energy account this tick. Colony state, not
/// spawn state: every spawn in the room draws from the same bank.
type RoomEnergy =
    {
        /// Energy banked for spawning right now (spawn + extensions).
        Available: int
        /// Energy the room banks when every feeder is full (spawn + built extensions).
        Capacity: int
    }

/// What a built structure is — or what a construction site will become
/// once built. Projection vocabulary, distinct from the Intent vocabulary
/// of placeable kinds (StructureKind): every placeable kind widens into
/// one of these (`builtKindOfPlaceable`), never the other way.
[<RequireQualifiedAccess>]
type BuiltKind =
    | Spawn
    | Extension
    | Tower
    | Road
    | Container
    | Storage
    /// A link. Projection-only: no counterpart in the placeable kinds,
    /// because the Layout holds a footing for one but never places it
    /// (ADR 0022).
    | Link
    /// A rampart, the walkable defence over the Keep and the Posts (ADR
    /// 0034). Walkability answers for it before anything else does: a
    /// creep may stand on a rampart, and folding it into Other would make
    /// every kind the decision layer does not model walkable with it.
    | Rampart
    /// Any structure kind the decision layer has no rules for yet.
    | Other

/// What the decision layer knows about one energy-hungry structure
/// (spawn, extension, or tower) this tick.
type RefillableInfo =
    {
        Id: string
        /// Energy the structure's store can still take (0 = full).
        FreeCapacity: int
        /// What kind of structure this is — the Refill rank layer's key
        /// (ADR 0010): spawn-feeding kinds are feeding-tier work, towers
        /// surplus-tier. To a creep both are the same transfer.
        Kind: BuiltKind
    }

/// What the decision layer knows about one energy source this tick.
type SourceInfo =
    {
        Id: string
        /// Ticks until the source holds energy again — its restock
        /// (ADR 0013, widened by ADR 0025); 0 while it holds energy now.
        /// Not the amount: the one time fact a decision reads about a
        /// source, so that a drained source's Harvest can be judged at
        /// the creep's arrival rather than at the current tick. Stocked
        /// is a restock of zero, never a field of its own.
        TicksToRestock: int
    }

/// What the decision layer knows about the room controller this tick.
type ControllerInfo =
    {
        Id: string
        /// Controller level (RCL); gates how many extensions may exist.
        Level: int
        /// Ticks left on the downgrade timer. A downgrade costs a level
        /// AND zeroes the safe-mode stock, so this is a hard deadline.
        TicksToDowngrade: int
        /// Safe-mode activations banked (one is granted per level-up;
        /// the stock is zeroed by any downgrade).
        SafeModeAvailable: int
        /// True while safe mode is running in the room.
        SafeModeActive: bool
    }

/// Whose CLAIM parts hold one room's reservation, as the colony reads it:
/// three answers and not a username, the same closed shape and for the
/// same reason as `Ownership` below.
///
/// The third answer is load-bearing and is not a refinement of the second.
/// ADR 0043 gives an NPC invader's reservation and another *player's*
/// opposite meanings: the Invader's is the **clock** a [[stand-down]] runs
/// to where the core carries no collapse timer ("the end of the
/// reservation it has taken"), and a player's is the **clockless**
/// withdrawal, a room that has stopped being ours to work and is never
/// re-entered on a timer. Read through one "not ours" flag the two are the
/// same value, and no correct answer exists for either: an Invader's
/// reservation outliving its core would shut an outpost forever, and a
/// player's credited to a core would reopen a room somebody else holds.
/// So the shell separates them where it holds the username, and Core still
/// never sees one.
[<RequireQualifiedAccess>]
type ReservationHolder =
    /// This colony's own CLAIM parts. The one answer that doubles the
    /// room's sources and the one the reserver row sizes itself from.
    | Ours
    /// The NPC Invader — the user an invader core belongs to, and the
    /// holder of the reservation a level-0 core takes with
    /// `attackController` in a room it expanded into (ADR 0043,
    /// docs/research/remote-mining.md §8.4). Worth the neutral rate like
    /// any hold that is not ours, and, unlike a rival's, an expiry: this
    /// one lapses.
    | Invader
    /// Another player. Worth the neutral rate, and the clockless
    /// withdrawal of ADR 0043 — the one abandonment trigger every mature
    /// bot implements.
    | Rival

/// The reservation standing on one room's controller this tick (ADR
/// 0042): a neutral controller held by CLAIM parts, which doubles every
/// source in that room, decays by one a tick and caps at 5,000.
type ReservationInfo =
    {
        /// Whose CLAIM parts hold it. Whose it is, rather than whose name
        /// it carries: the engine answers holding with a username, the
        /// colony's own name is the shell's to know (the owner of the room
        /// its spawns stand in), the NPC's is a name the shell knows too,
        /// and every rule reading this asks which of the three rather than
        /// which string.
        ///
        /// A reservation somebody else holds reads for *pricing* exactly
        /// as no reservation at all does, and that is a colony decision,
        /// not the engine's arithmetic: `sources/tick.js` switches a
        /// source to 3,000 a cycle on
        /// `roomController.user || roomController.reservation` — **any**
        /// owner, **any** reservation — so a creep of ours digging in a
        /// room a rival holds really would draw ten a tick
        /// (docs/research/remote-mining.md §1.1). The colony prices it at
        /// five deliberately and conservatively: a room somebody else
        /// owns or reserves has stopped being ours to work, it is the one
        /// withdrawal trigger every mature bot implements, and the
        /// [[stand-down]] (ADR 0043) is where the withdrawal itself
        /// lands. Nothing should size a fleet against energy the colony
        /// is about to walk away from. For *withdrawing* the two are not
        /// one answer — see `ReservationHolder`.
        Holder: ReservationHolder
        /// Ticks left on the reservation — what the reserver row's one
        /// rule sizes and quotas from, `ceil((5000 - this) / 600)` CLAIM
        /// parts (ADR 0042, `Decide.reserverClaimsOf`). Read as the
        /// colony's own hold only where `Holder` is `Ours`: a reservation
        /// somebody else holds leaves this colony's own hold at zero,
        /// exactly as it leaves the room's sources at the neutral rate.
        /// Under `Invader` it is the other thing this field is: the
        /// deadline ADR 0043 falls back to when a core carries no collapse
        /// timer.
        ///
        /// The holder and the ticks left are a single engine fact off a
        /// single binding — the reservation object arrives whole or not at
        /// all — so the pair is projected together and the exception the
        /// sentence under `Reservation` below does not cover.
        TicksToEnd: int
    }

/// Whose a room's controller is, as the colony reads it: three answers and
/// not a username. Two of them are what ADR 0042 prices a source from —
/// ours is the held rate, nobody's is half — and the third is what ADR
/// 0043's clockless withdrawal is judged on: a room another player has
/// taken has stopped being ours to work, whatever it yields.
///
/// A closed vocabulary rather than a pair of booleans, because "ours" and
/// "somebody else's" are answers to one question and two flags could carry
/// both at once. It is not the fourth answer, "we cannot see": that one is
/// the absence of the whole entry (ADR 0004), because the question is only
/// asked of a room vision answered for.
[<RequireQualifiedAccess>]
type Ownership =
    /// Nobody owns the controller — the shape every neutral room and every
    /// outpost the colony works arrives in, and the shape a room with no
    /// controller at all is projected as. Reservable, and worth half until
    /// it is reserved.
    | Unowned
    /// This colony owns it: the spawn room, and nothing else while there
    /// is one colony. Worth the held ten a tick, and never reserved — the
    /// engine refuses `reserveController` on a room anybody owns.
    | Ours
    /// Another player owns it. The engine yields ten a tick in a rival's
    /// room exactly as in ours, and the colony prices it at five all the
    /// same, for the reason `ReservationInfo.Holder` gives: a room
    /// somebody else holds is one the colony is withdrawing from (ADR
    /// 0043). No NPC case here beside `ReservationHolder`'s: an invader
    /// core *reserves* and never owns — `expandStronghold` tests
    /// `!controller.user` and `attackController` leaves the owner
    /// untouched — so the NPC is a holder the colony can meet and never an
    /// owner.
    | Rival

/// Who holds one room the colony can see this tick — the fact a source's
/// output is read from (ADR 0042), because ten energy a tick is the
/// *held* rate and a neutral room's source yields five.
///
/// One entry per room vision answered for, home included. A room the
/// colony cannot see has no entry at all, and that absence is not "half":
/// who holds a room we cannot look into is not a fact this tick, so its
/// sources are unpriceable and enter no quota (ADR 0004).
type RoomControlInfo =
    {
        /// Whose the room's controller is (the engine's `controller.my`
        /// and `controller.owner`). Read *beside* the reservation and
        /// never instead of it: the engine gives a room with an owner the
        /// same 3,000 a cycle it gives a reserved one, so a rule spelled
        /// "reserved, or half" would price the colony's own two sources at
        /// five and halve its hauler quota and its income base together.
        Owner: Ownership
        /// The reservation standing on the room's controller; None where
        /// nothing reserves it. *Which* rival holds it is still
        /// deliberately not carried, and that is now the whole of what is
        /// left out: naming one rival apart from another is a name no rule
        /// reads. What the pair above and here do carry is every
        /// *question* ADR 0043 asks of a controller — whether somebody
        /// else holds this room, as `Ownership.Rival` or as a
        /// `ReservationHolder.Rival` reservation, and whether the holder
        /// is instead the NPC whose reservation is a clock rather than an
        /// exit. #133 is the tick both widenings arrived on, and they
        /// arrived as closed three-state answers rather than as usernames
        /// for the reason `ReservationHolder` gives.
        Reservation: ReservationInfo option
    }

/// A tile coordinate inside a room.
type Pos = { X: int; Y: int }

/// Screeps range: Chebyshev distance between two tiles. The one
/// definition — the Atlas's geometry, the two hostile reflexes and the
/// Raid log's closest approach all measure with it.
let range (a: Pos) (b: Pos) = max (abs (a.X - b.X)) (abs (a.Y - b.Y))

/// Current and maximum hit points of a repairable structure — what a
/// kind's whole line is judged against (ADR 0010, ADR 0034).
type HitsInfo = { Hits: int; HitsMax: int }

/// Three-state terrain of one room tile.
type Terrain =
    | Plain
    | Swamp
    | Wall

/// What kind of thing a projected target is.
type TargetKind =
    | Source
    | Controller
    | Structure of BuiltKind
    | Site of BuiltKind
    /// A dropped energy pile. Two readers now: the [[pickup reflex]],
    /// which takes what is already at a creep's feet and reads no amount
    /// at all, and the Pickup Task (#167), which walks a hauler to a pile
    /// big enough to be worth the trip and reads the amount out of
    /// `SpatialInfo.Stores` like any other store. The amount is the one
    /// field this kind grew for the Task; the reflex is unchanged and
    /// still asks only where a pile is.
    | Dropped
    /// A tombstone or a ruin: a store with a clock on it. One kind for
    /// both engine objects (#167), because the only thing any reader
    /// decides on is that it holds energy and will be gone — a tombstone
    /// in a hundred ticks, a ruin on its own decay — and `Withdraw` is
    /// the verb for either. Its energy rides `SpatialInfo.Stores` beside
    /// the containers', so the Withdraw pool, its stock cap (#161) and
    /// its tier read it through the rules they already had.
    ///
    /// Never an obstacle: a tombstone stands on the tile a creep died on,
    /// which may be a Seat or a Post, and the engine lets a creep walk
    /// over both objects. Transient like a pile, and kept off the
    /// Layout's ground for the same reason (`isTransient`).
    | Tombstone

/// Whether a projected target is one of the two transient kinds — a pile
/// or a tombstone/ruin — that stand on a tile without holding it. Both
/// vanish on their own within a few hundred ticks, so a census that let
/// one keep a construction site off its tile would make the Layout's
/// ordering depend on where a creep happened to die (ADR 0011's
/// determinism). Read by `Atlas.buildableTiles`, which is the one census
/// that walks every placed target rather than picking a kind.
let isTransient =
    function
    | Dropped
    | Tombstone -> true
    | Source
    | Controller
    | Structure _
    | Site _ -> false

/// One room's geometry, filed under that room's name (ADR 0041): every
/// container the projection keys by `Pos` or fills with `Pos`es, gathered
/// into one record rather than five maps side by side, so reading a
/// room's geometry is one lookup and not five. The id-keyed containers
/// (target kinds, hits, stores) stay outside it, because an object id is
/// already unique across the world and layering it would key a unique
/// thing twice.
///
/// Absence stays per entry (ADR 0004) and now says one more thing: a room
/// missing entry by entry inside its layer and a room with no layer at all
/// are the same answer, so a room's geometry is read as
/// `Map.tryFind name spatial.Rooms |> Option.defaultValue RoomLayer.empty`
/// and never as `.[name]`, which throws on a room the projection names but
/// has no geometry for. Neither absence is a state of its own — an outpost
/// the colony cannot currently see is unpriceable geometry and nothing
/// more.
type RoomLayer =
    {
        /// Terrain per tile over this room's ground (x,y in 1..48); a tile
        /// absent from the map is impassable. The border ring is not here
        /// and is not ground: it rides in `SpatialInfo.Borders`, which the
        /// Seam query alone is priced off (ADR 0036, ADR 0041).
        Terrain: Map<Pos, Terrain>
        /// Target id -> that target's tile in this room: the Task targets
        /// (source, refillable structure, construction site, controller,
        /// and since #167 the piles and tombstones a hauler is sent to)
        /// and the dropped piles the pickup reflex reads. The two
        /// transient kinds are filtered out by kind where standing on a
        /// tile is not the same as holding it (`isTransient`,
        /// `Atlas.buildableTiles`).
        TargetPositions: Map<string, Pos>
        /// Creep name -> the tile the creep stands on in this room.
        CreepPositions: Map<string, Pos>
        /// Tiles blocked by obstacle structures (spawn, extension,
        /// controller, ...) and by their construction sites — the engine
        /// refuses to move a creep onto its own obstacle-type site;
        /// impassable regardless of terrain.
        Obstacles: Set<Pos>
        /// Tiles holding a built road — built structures only, a road
        /// construction site is not yet a road (ADR 0010).
        Roads: Set<Pos>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RoomLayer =
    /// A room with nothing in it — every entry absent. What a `tryFind` on
    /// `SpatialInfo.Rooms` defaults to, so a room the projection holds no
    /// geometry for reads the same as one whose every container is empty
    /// (ADR 0004).
    let empty: RoomLayer =
        {
            Terrain = Map.empty
            TargetPositions = Map.empty
            CreepPositions = Map.empty
            Obstacles = Set.empty
            Roads = Set.empty
        }

/// The Snapshot's spatial projection: the terrain of the rooms the colony
/// works plus positions of the entities decisions need to place on them.
type SpatialInfo =
    {
        /// Which entry of `Rooms` is the home room — the room the colony
        /// plans for, which is the room its spawn happens to stand in and
        /// is never defined by that (ADR 0041) — and still the room name
        /// the census signature and the Layout read (ADR 0017). None for a projection that does not say which
        /// room it is, whose geometry is filed under the empty name,
        /// exactly as `Decide.censusSignature` spells that room.
        RoomName: string option
        /// Room name -> that room's geometry: every container the
        /// projection keys by `Pos` or fills with `Pos`es, and since ADR
        /// 0041's contract step the *only* place any of them lives. There
        /// is one projection and one shape of it (ADR 0005) — the flat
        /// copies of these five that carried the home room through the
        /// migration are gone, and with them the bridge that filled them.
        /// `RoomName` says which entry is home; every other entry is an
        /// outpost — so a projection carrying a `Borders` entry has to name
        /// its home room too, or `SpatialInfo.homeName` is the empty name
        /// and every home query reads `RoomLayer.empty` however the
        /// geometry here is filed. Read an entry with `Map.tryFind`,
        /// defaulting to `RoomLayer.empty`: a room with no geometry has no
        /// entry here at all, and that is the same answer (ADR 0004).
        Rooms: Map<string, RoomLayer>
        /// Room name -> the terrain of that room's border ring: the exit
        /// rows and columns (x or y of 0 or 49) a layer's `Terrain`
        /// deliberately leaves out. A layer of its own and never ground
        /// (ADR 0041): a creep that ends its tick on an exit tile is moved
        /// into the neighbouring room by the engine, so admitting one as
        /// walkable would let a Seat, a Work Area or a standing candidate
        /// teleport the creep out from under its Task — which is what ADR
        /// 0036's 1..48 trim prevents and this layer must not undo. It
        /// enters no weight grid, no walkable or buildable set and no Work
        /// Area; the Atlas lays it a grid of its own (`Atlas.Rings`, #173)
        /// beside those and never inside them, and the Seam query and the
        /// crossing's price are all that read it. Keyed by room
        /// name because a Seam joins two rooms: a room the projection does
        /// not cover is simply absent here, and answers no Seam at all
        /// (ADR 0004).
        Borders: Map<string, Map<Pos, Terrain>>
        /// Task-target id -> what kind of thing stands (or will stand)
        /// there. Id-keyed and so unlayered (ADR 0041): an object id is
        /// already unique across the world, and the layer that places the
        /// id *is* the room it stands in (`SpatialInfo.placementOf`), so a
        /// room dimension here would key a unique thing twice.
        TargetKinds: Map<string, TargetKind>
        /// Target id -> current/max hits, repairable kinds only — the
        /// decaying roads and containers (ADR 0010, ADR 0012), the Keep
        /// and our own ramparts (ADR 0034); fields nobody decides on stay
        /// out. Each kind is judged against its own whole line
        /// (`wholeLine`), and three readers now share these hits: the
        /// Repair pool, the safe-mode reflex and the Raid log's damage.
        Hits: Map<string, HitsInfo>
        /// Target id -> energy currently stored: the stock the logistics
        /// Tasks judge a store by. The containers (ADR 0012) and the
        /// Storage (ADR 0023) are the standing stores; since #167 the two
        /// transient ones are here on the same key — a tombstone's or a
        /// ruin's energy, which `Withdraw` draws exactly as it draws a
        /// container's, and a dropped pile's amount, which is what tells
        /// the Pickup Task whether the pile is worth a walk. One table
        /// and no second reading: a store is a store whatever will
        /// become of the thing holding it.
        Stores: Map<string, int>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SpatialInfo =
    /// The empty projection: no room, no tiles, no entities — every entry absent.
    let empty =
        {
            RoomName = None
            Rooms = Map.empty
            Borders = Map.empty
            TargetKinds = Map.empty
            Hits = Map.empty
            Stores = Map.empty
        }

    /// The name the projection's own room is filed under: `RoomName`, and
    /// the empty name when it names none — the name the census signature
    /// has always spelled that way (`Decide.censusSignature`). Decided
    /// here, once, so the convention has one implementation rather than a
    /// copy at every reader that has to resolve the home layer; a site
    /// that spelled it differently would file the home room under one name
    /// and read it under another, and ADR 0004 would answer every home
    /// query with the empty set rather than throwing.
    let homeName (spatial: SpatialInfo) : string =
        spatial.RoomName |> Option.defaultValue ""

    /// One room's geometry, as ADR 0004 has every other absence: a room
    /// the projection carries no layer for reads as a room whose every
    /// entry is absent, never as a lookup that throws. The one spelling of
    /// the read the `RoomLayer` doc prescribes, so no reader has to
    /// remember the default.
    let layerOf (spatial: SpatialInfo) (room: string) : RoomLayer =
        Map.tryFind room spatial.Rooms |> Option.defaultValue RoomLayer.empty

    /// The room the projection files a target id under, with its tile
    /// there. The id-to-room join on the projection itself, beside the one
    /// the Atlas precomputes (`TargetAt`): a target id is unique across
    /// the world, so the layer holding it *is* the room it stands in, and
    /// the two answer alike because the Atlas fills `TargetAt` by walking
    /// these same layers. Which one a reader spells is therefore about
    /// what it is answerable to, not about what it holds. A reader handed
    /// no Atlas — the Planner, and `censusSignature` — has only this one.
    /// A reader *guarded* by that signature spells it this way too, even
    /// holding an Atlas: the hauler quota resolves its containers here so
    /// that the join the memo signs and the join the memo's value reads
    /// are the same line, rather than two spellings kept in step by hand.
    /// Everything else — a Task priced against the Atlas's own tables —
    /// takes the precomputed join. None for a target the projection does
    /// not place, which classifies nothing and blocks nothing (ADR 0004).
    ///
    /// Deterministic under a collision that cannot happen: `Map.tryPick`
    /// walks the rooms in name order, and one id stands in one room.
    let placementOf (spatial: SpatialInfo) (id: string) : (string * Pos) option =
        spatial.Rooms
        |> Map.tryPick (fun room (layer: RoomLayer) ->
            Map.tryFind id layer.TargetPositions |> Option.map (fun pos -> room, pos))

/// One outpost: a neighbouring room this colony mines and does not own.
/// Declared, never discovered (ADR 0041) — a constant a human moves in a
/// commit, exactly as the Layout's horizon is (ADR 0039) — because every
/// "the first creep to walk in writes it down" scheme has to answer what
/// sent the first creep, and answering it means inventing scouting,
/// persistent room intel and staleness discounting for a colony with two
/// candidate neighbours already committed as fixtures.
///
/// What is declared is exactly what vision cannot be waited for: the
/// room's name, and the id and tile of each source and of the controller.
/// Everything that actually changes — the reservation remaining, container
/// and road hits, stores, hostiles — is read off the projection where
/// there is vision and is absent entry by entry where there is none (ADR
/// 0004). That is the whole of what "we cannot see it this tick" means
/// here: no second state, and nothing to discount.
///
/// The ids are the engine's own, and this is the decision the rest of the
/// outpost work is built on. Every id in the projection is the server's —
/// `TargetKinds`, `Hits` and `Stores` are keyed by it and `Snapshot.Sources`
/// carries it — so a declaration written in the room captures' readable
/// short names (`RoomFixtures` renames `6a8c…4a6` to `src-0` for a person
/// to read) would match nothing on a live server, and would do it in
/// silence: an id the projection does not place is unpriceable geometry,
/// so the outpost would simply never enter a Task rather than fail (ADR
/// 0004). The captures keep the server's ids beside the readable ones
/// (`RoomCapture.RealSources`) so a test can build the Snapshot a
/// declaration matches.
///
/// No adjacency field, deliberately: which border an outpost shares with
/// home is already a fact about the two room names, and the Seam query
/// reads it out of them (`Atlas.seams`). A room name and an edge are two
/// facts that can disagree, and the disagreement would build a band out of
/// two rooms' opposite walls.
type Outpost =
    {
        RoomName: string
        /// The room's sources, each under the id the engine knows it by.
        Sources: (string * Pos) list
        /// The room's controller, whose reservation is what doubles those
        /// sources (ADR 0042). Not optional: an unreserved source is worth
        /// half, so a room with no controller to reserve — a sector centre
        /// or a Source Keeper room — is not a candidate outpost at all.
        Controller: string * Pos
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Outpost =
    /// The declarations the colony works this tick: the declared list,
    /// less every room a [[stand-down]] is withholding (ADR 0043). The
    /// gate, and the one place the set is narrowed.
    ///
    /// It narrows the *declarations* and never the scan set directly,
    /// because the three readings below — the rooms projected, the
    /// furniture laid in and the rocks pooled — are three readings of one
    /// list, and a gate applied to one of them would be a room whose
    /// terrain nobody read carrying furniture, or a rock in the pool with
    /// no layer to price it against (ADR 0004's escape prices an unplaced
    /// target at 0, so it would *win* its tier). Narrowed here, the three
    /// narrow together or not at all, and everything downstream — the
    /// projection, the Task pool, the four quota rows, the Atlas — sees
    /// exactly what it sees for a room nobody declared. That is the whole
    /// of "withdraw" in an architecture that keeps no state and recomputes
    /// every tick, and it is semantics ADR 0004 has already paid for.
    ///
    /// The shut set is the previous tick's conclusion (`Observe.standDown`
    /// over the [[raid log]]), because the deadline it holds was read on
    /// the last tick that had vision to read one with — and the creeps
    /// that paid for that vision are the ones this gate withdraws.
    let worked (shut: Set<string>) (outposts: Outpost list) : Outpost list =
        outposts
        |> List.filter (fun outpost -> not (Set.contains outpost.RoomName shut))

    /// The rooms the shell projects this tick: the home room, and every
    /// declared outpost beside it (ADR 0041). One projection covering
    /// several rooms, never a second one (ADR 0005) — the union is taken
    /// here so the rule has one statement rather than a copy in the shell.
    ///
    /// The outposts are handed in rather than read from `declared`
    /// straight, for two reasons: the union rule is then checkable against
    /// any declaration — the empty one #124 shipped, the two rooms #126
    /// filled in, a third a human adds — rather than only against the one
    /// the colony happens to ship, and the stand-down gate (ADR 0043) has
    /// exactly one place to narrow the set
    /// — a room withdrawn from does not enter the projection at all, which
    /// is the whole of "retreat" in an architecture that keeps no state.
    ///
    /// Home first, then the declarations in their own order, each room
    /// once: a declaration naming the home room is a human's slip, and
    /// projecting that room twice would file one room's geometry under one
    /// name twice over rather than say so.
    let roomsProjected (outposts: Outpost list) (home: string) : string list =
        home :: (outposts |> List.map (fun outpost -> outpost.RoomName))
        |> List.distinct

    /// One declaration as projection entries: the controller and then the
    /// sources in their declared order, each id paired with the tile the
    /// declaration names and the kind it is. Position and kind are read off
    /// one list rather than two, so the two folds below cannot place an id
    /// the kind census then misses or classify one nothing places.
    let private furnitureOf (outpost: Outpost) : (string * Pos * TargetKind) list =
        (fst outpost.Controller, snd outpost.Controller, Controller)
        :: (outpost.Sources |> List.map (fun (id, pos) -> id, pos, Source))

    /// The declared furniture, laid into the projection: for every scanned
    /// outpost, its sources and its controller at the tiles and under the
    /// ids the declaration names — whether or not the colony has vision in
    /// that room this tick.
    ///
    /// This is the half of ADR 0041 that vision may not gate, and the
    /// deadlock the ADR spends a paragraph breaking: *"A source's position
    /// needs vision; vision needs a creep there; a creep goes there because
    /// a Task exists; the Task exists because the source is in the
    /// projection."* A declared fact — a source's id and tile, the
    /// controller's — is in the projection because a human wrote it down;
    /// only what actually changes (reservation remaining, container and
    /// road hits, stores, creeps, hostiles) waits for vision, and that is
    /// what is absent entry by entry where there is none (ADR 0004). #124
    /// read that absence onto the declaration as well, which left the whole
    /// ADR 0042 chain without its first step: no Harvest could name an
    /// outpost, so nothing walked there, so vision never came (#148).
    ///
    /// Vision wins every entry it holds: the declaration is laid *under*
    /// what the room's `find` families answered and never over it. The two
    /// agree by construction — the ids are the engine's own and a rock does
    /// not move — so this decides which truth is authoritative rather than
    /// resolving a conflict that can arise.
    ///
    /// The controller's tile joins `Obstacles`, exactly as the seen half
    /// files it (`Snapshot.projectVisible`): a controller is an obstacle
    /// structure, so a reserver stands beside it and never on it, and a
    /// Work Area built over ground that ignored it would offer a tile the
    /// engine refuses to move onto.
    ///
    /// Only rooms the projection already carries a layer for. The scan set
    /// is the one gate on which rooms the colony works (`roomsProjected`,
    /// narrowed by the stand-down of ADR 0043), and a declaration able to
    /// conjure a room the scan left out would be a second gate free to
    /// disagree with the first — furniture standing on terrain nobody read.
    let place (outposts: Outpost list) (spatial: SpatialInfo) : SpatialInfo =
        (spatial, outposts)
        ||> List.fold (fun spatial outpost ->
            match Map.tryFind outpost.RoomName spatial.Rooms with
            | None -> spatial
            | Some layer ->
                let furniture = furnitureOf outpost

                { spatial with
                    Rooms =
                        Map.add
                            outpost.RoomName
                            { layer with
                                TargetPositions =
                                    (layer.TargetPositions, furniture)
                                    ||> List.fold (fun placed (id, pos, _) ->
                                        if Map.containsKey id placed then
                                            placed
                                        else
                                            Map.add id pos placed)
                                Obstacles = Set.add (snd outpost.Controller) layer.Obstacles
                            }
                            spatial.Rooms
                    TargetKinds =
                        (spatial.TargetKinds, furniture)
                        ||> List.fold (fun kinds (id, _, kind) ->
                            if Map.containsKey id kinds then
                                kinds
                            else
                                Map.add id kind kinds)
                })

    /// The sources the Harvest pool is built from: the ones vision answered
    /// with, and every declared outpost rock beside them. One pool ranked
    /// in one order (ADR 0041), so a rock the colony cannot see this tick
    /// is a Task all the same — the declaration is what breaks the vision
    /// deadlock, and a pool that waited for vision would never see one.
    /// Deduplicated by id with the seen list first, because a declared rock
    /// in a room we *can* see arrives twice under one engine id and the
    /// engine's answer is the one carrying this tick's restock.
    ///
    /// An unseen rock restocks in 0 ticks: ADR 0025's "holds energy"
    /// default, the same one the shell gives a source whose regeneration
    /// timer the engine has not started. A restock is a *time* fact, and
    /// the unknown one is not "for ever" — priced at 0 the source is judged
    /// at arrival like any other (ADR 0025), and the Emitter's own gate is
    /// what withholds the dig from a rock that turns out to be empty when
    /// the creep gets there. Priced at anything else it would be a source
    /// no walk could cover, which is the same deadlock in a second place.
    ///
    /// Scanned rooms only, which is the gate `place` reads off the
    /// projection it is handed: the scan set is the one gate on which rooms
    /// the colony works (`roomsProjected`, narrowed by the stand-down of
    /// ADR 0043), and the pool has to pass through it too. A pool that took
    /// the declaration straight would name rocks nothing places — and an
    /// unplaced target is not inert to the Matcher: it prices at 0 (ADR
    /// 0004's escape), so it *wins* its tier, and the Emitter then aims a
    /// Harvest at an object `Game.getObjectById` cannot answer for while
    /// anti-thrash holds the creep on it. So the rocks are pooled exactly
    /// where the furniture is laid, and a stand-down narrows both at once.
    let pooledSources
        (rooms: string list)
        (outposts: Outpost list)
        (seen: SourceInfo list)
        : SourceInfo list =
        seen
        @ [
            for outpost in outposts do
                if List.contains outpost.RoomName rooms then
                    for id, _ in outpost.Sources -> { Id = id; TicksToRestock = 0 }
        ]
        |> List.distinctBy (fun source -> source.Id)

/// One colony: a [[home room]] and the [[outpost]]s worked from it (ADR
/// 0047). The unit the whole decision layer is written in — one Atlas, one
/// Layout, one set of quotas, one Task pool — and so the unit a
/// declaration is written in too, replacing the bare outpost list that
/// said the same thing while there was only ever one home.
///
/// The home is a room *name* and never a spawn: a colony outlives every
/// spawn standing in it, and the room is what the projection files
/// everything under (ADR 0041). Which colonies actually run is a fact
/// about the world and not about this constant — a declared home the
/// colony has not claimed yet is a **candidate colony**, and one with no
/// spawn of its own is not independent — so nothing here is a promise that
/// a colony exists, only that a human means it to.
type Colony =
    {
        /// The room the colony is run from: the room its spawns stand in,
        /// its Layout is planned in, and its quotas are banked in.
        Home: string
        /// The rooms it mines but does not own. A **candidate colony**'s
        /// home appears here as well, in its *mother* colony's list, until
        /// the day it is independent: the room is projected and worked as
        /// an outpost while it is being claimed and built up, and one room
        /// projected by two colonies at once is exactly what the mother's
        /// outpost declaration already means (ADR 0047).
        Outposts: Outpost list
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Colony =
    /// The colonies a human has declared (ADR 0047): today the one home
    /// room this bot has ever had, W12S28, with ADR 0042's two outposts —
    /// W12S27 across the north edge and W13S28 across the west, three
    /// sources and two controllers between them.
    ///
    /// Chosen by a human in an ADR and moved by a human in a commit,
    /// exactly as the Layout's horizon is (ADR 0039) — the types above
    /// carry why there is no discovery and why the ids are the engine's.
    /// That is why claiming a second room begins here and not in the bot:
    /// a second entry `{ Home = "W13S28"; Outposts = [] }` beside this one
    /// — W13S28 staying in W12S28's outposts until it stands on its own —
    /// is the whole of "I mean to take that room", and it is a human's
    /// sentence to write (ADR 0047's user story 1). The bot never edits
    /// it: independence is an event a person can see, not a constant a
    /// program rewrites.
    ///
    /// The outposts are still ADR 0042's, and the reasons they are those
    /// rooms have not moved. Filling that list was half of ADR 0042's
    /// first step and never the whole of it: the other half is in
    /// `Decide.workforceTarget`, which counts an unposted source's Seats
    /// into the target on the grounds that its output is spoken for by the
    /// crews that walk it — a rule that, taken across these three sources'
    /// six Seats, five of them swamp, hires six generalists to commute
    /// forty-seven to fifty-six tiles.
    ///
    /// W13S28's sources are paired to their tiles and never to their
    /// order: they are written `16,7` before `18,4`, the reverse of the
    /// order ADR 0042's prose reads them in, because that is the order the
    /// server answered the room in and the order the committed capture
    /// keeps (`RoomFixtures.RealSources`, pinned in `RoomInvariantTests`).
    /// `16,7` is the single-Seat far source, not the two-Seat one.
    let declared: Colony list =
        [
            {
                Home = "W12S28"
                Outposts =
                    [
                        {
                            RoomName = "W12S27"
                            Sources = [ "6a8caabadd4872bccd3194a6", { X = 16; Y = 45 } ]
                            Controller = "6a8caabadd4872bccd3194a5", { X = 37; Y = 43 }
                        }
                        {
                            RoomName = "W13S28"
                            Sources =
                                [
                                    "6a8caaaddd4872bccd319362", { X = 16; Y = 7 }
                                    "6a8caaaddd4872bccd319361", { X = 18; Y = 4 }
                                ]
                            Controller = "6a8caaaddd4872bccd319363", { X = 24; Y = 17 }
                        }
                    ]
            }
        ]

    /// The outposts one home room works: its own declaration's, and none
    /// at all for a room nobody declared. That last answer is the one that
    /// matters — a home the constant does not name projects the room it
    /// stands in and nothing else, which is exactly the behaviour the
    /// empty declaration shipped with (#124), so a slip in the constant
    /// costs the colony its outposts rather than putting it in a state
    /// nothing downstream has a rule for.
    ///
    /// The list is handed in rather than read off `declared` straight, for
    /// the reason `Outpost.roomsProjected` is: the rule is then checkable
    /// against any declaration a human might write rather than only
    /// against the one this colony happens to ship.
    let outpostsOf (colonies: Colony list) (home: string) : Outpost list =
        colonies
        |> List.tryFind (fun colony -> colony.Home = home)
        |> Option.map (fun colony -> colony.Outposts)
        |> Option.defaultValue []

    /// Every declared colony's home room, in declaration order. What the
    /// shell hands the decision layer (`Snapshot.ColonyHomes`), because
    /// which rooms a human means to own is not a thing vision can answer
    /// and not a thing the projection carries — the ownership half of
    /// "candidate colony" is read off `RoomControl` in Core, and this is
    /// the half that can only be declared.
    let homes (colonies: Colony list) : string list =
        colonies |> List.map (fun colony -> colony.Home)

/// What the decision layer knows about one construction site this tick.
type ConstructionSiteInfo = { Id: string }

/// What the decision layer knows about one hostile creep in a spawn room
/// this tick: its id and tile — what the fire reflex aims at (ADR 0014) —
/// its body parts, verbatim, because what a hostile can do is decided
/// from what it is made of, its owner, which the Raid log's roster reads
/// (ADR 0028), and the room it stands in, which the Raid log's closest
/// approach reads (ADR 0041). Hostiles stay out of the spatial projection:
/// they block no tiles, price no paths, gate no tasks.
type HostileInfo =
    {
        Id: string
        /// Whose creep this is, as the engine spells the username
        /// ("Invader" for the NPCs). The field the projection grew the
        /// tick a reader for it existed (ADR 0007's rule, ADR 0028): the
        /// Raid log's roster is attribution, and attribution is a name.
        /// No reflex reads it.
        Owner: string
        /// The room this hostile stands in. ADR 0028 left this field out
        /// in as many words — "a room name on `HostileInfo` is a field no
        /// decision reads, and there is one spawn" — and ADR 0041 is what
        /// gives it a reader: a `Pos` carries no room, so the Raid log's
        /// closest approach measures a hostile against the tiles of *its*
        /// room, and one of ours standing on the same coordinate of
        /// another room is not at range 0. The sweep behind it is
        /// unchanged (`Snapshot.Hostiles` is still the spawn rooms
        /// alone), so today every hostile names the colony's own room; the
        /// reflexes still read none of this, and their Reach stays the
        /// home room's until #117.
        RoomName: string
        Pos: Pos
        Body: BodyPart list
    }

/// An NPC invader core standing in a room the colony works this tick (ADR
/// 0043). A **structure**, not a creep, which is why it reaches the
/// projection through neither `Hostiles` nor the fire reflex: the sweep
/// behind those is `FIND_HOSTILE_CREEPS` and a core has never been in it.
/// It is the threat an [[outpost]] is stood down from — 100,000 hits, no
/// creeps at level 0, and it never leaves — and the clock the stand-down
/// runs to is read off it while there is still vision to read it with.
///
/// One room per entry and no tile: the gate ADR 0043 describes admits or
/// withholds a whole room, so where in the room the core stands is a fact
/// nothing asks for, and the projection grows a field the tick a reader
/// exists and not before (ADR 0007's rule). A room the colony cannot see
/// contributes no entry — a core standing unwatched is absent here rather
/// than "no core" (ADR 0004), which is exactly why the expiry below is
/// sampled while the room is still in sight.
type InvaderCoreInfo =
    {
        /// The room it stands in. The colony works rooms, not tiles, so
        /// this is the whole of where.
        RoomName: string
        /// The **absolute** tick the core's collapse timer runs out at, or
        /// None where it carries none — an expanded level-0 core has no
        /// stronghold to collapse, so the deadline has to be read off the
        /// reservation it took instead (ADR 0043's fallback order), which
        /// is the room's `RoomControlInfo.Reservation` under
        /// `ReservationHolder.Invader` and is why that holder is a case of
        /// its own. This is the common case on the frontier, not the rare
        /// one: the measured core two rooms from W12S27 is level 0 and
        /// carries no timer (docs/research/remote-mining.md §8.4).
        ///
        /// Absolute because the shell adds the current tick to what the
        /// engine hands back, and the engine hands back a **relative**
        /// count: `RoomObject.effects[].ticksRemaining` is "how many ticks
        /// will the effect last" (docs.screeps.com, confirmed for #133).
        /// The `endTime` in `docs/research/remote-mining.md` — 170,283 for
        /// W15S24's stronghold — is a field of the read-only HTTP API's
        /// raw database documents and is *not* what the runtime answers
        /// with; stored as read it would be a deadline a hundred thousand
        /// ticks wrong, and the gate that reads it would hold an outpost
        /// shut for the life of the colony.
        CollapseTick: int option
    }

/// Which of ADR 0043's deadlines an [[outpost]]'s [[stand-down]] runs to
/// — the provenance of the tick, carried beside it because it cannot be
/// recovered from the tick afterwards. The fold that picked it is the only
/// place that still knows whether 2,600 was a collapse timer, a
/// reservation or the fallback, and an operator asking why an outpost is
/// shut is asking exactly that (#117).
///
/// The three answers are the ADR's own fallback order, best first: the
/// core's collapse timer, the end of the reservation the core took, and
/// the stronghold expansion period. A closed vocabulary and not a string,
/// for the reason `Ownership` gives — these are answers to one question —
/// and it crosses the wire, so it is spelt once in `standDownBasisName`
/// and round-tripped against the union itself by `Core.Tests` (#80).
///
/// The other withdrawal — a room another player owns or reserves — is
/// deliberately not a fourth case. ADR 0043 makes it the clockless
/// trigger, "not a threat episode": it opens no episode and carries no
/// expiry for a basis to explain. It is read off the Snapshot's
/// `RoomControlInfo` on every tick with vision and *remembered* between
/// them (`RaidState.RivalHeld`, #136), because the gate's own effect is to
/// take away the vision that judged it. What it remembers beside the room
/// is a tick, and not one anything compares against: the trace the gate's
/// closing leaves in the observe channel, where a basis would have nothing
/// to explain.
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
    /// stronghold expansion period. The one answer the colony chose
    /// rather than read, and it errs long deliberately: ADR 0043's gate
    /// may be wrong only in the direction that costs an outpost's income
    /// rather than a creep a cycle.
    | Fallback

/// What the decision layer knows about one owned creep this tick.
type CreepInfo =
    {
        Name: string
        /// Ticks the creep still has to live — the engine counts down from
        /// CREEP_LIFE_TIME. A creep still spawning is outside the
        /// projection, so a projected creep always carries a real count.
        /// The fact, not the judgement: whether it is expiring is this
        /// count measured against the lead its replacement needs
        /// (ADR 0026).
        TicksToLive: int
        /// Fatigue points still to pay off; a creep with any cannot step
        /// this tick — the engine's move answers ERR_TIRED.
        Fatigue: int
        /// Energy currently carried.
        Energy: int
        /// Carry capacity still free (0 = full).
        FreeCapacity: int
        /// Part count per body part; a part absent from the map is a part
        /// the body does not have. What a creep can do is decided from
        /// what it is made of.
        Body: Map<BodyPart, int>
    }

/// Immutable projection of the current tick's game state; only what decisions need.
type Snapshot =
    {
        Time: int
        Spawns: SpawnInfo list
        /// Room name -> that room's shared spawn-energy bank. A room absent
        /// from the map banks nothing: its spawns wait.
        RoomEnergy: Map<string, RoomEnergy>
        /// Energy-hungry structures (spawn, extension, tower), whether or
        /// not they currently have room.
        Refillables: RefillableInfo list
        Sources: SourceInfo list
        /// None when no spawn room has an owned controller (should not happen in practice).
        Controller: ControllerInfo option
        /// Who holds each room the colony has vision in this tick, under
        /// that room's name — what a source's output per tick is priced
        /// from (ADR 0042). Keyed by room and not by controller id: the
        /// question a quota asks is about the room a source stands in, and
        /// the room is what the projection files a source under (ADR
        /// 0041). Absent for a room vision did not answer for, per-entry
        /// as every other absence is (ADR 0004).
        RoomControl: Map<string, RoomControlInfo>
        /// Our construction sites in every scanned room the colony has
        /// vision in this tick, and not the spawn rooms' alone (#150): the
        /// Build pool is this list one to one (`Decide.planTasks`), so an
        /// outpost's site is a Task like the home room's. A site is a thing
        /// vision pays for and no declaration carries, so a room nothing
        /// looks into contributes none of them (ADR 0004, ADR 0042).
        ConstructionSites: ConstructionSiteInfo list
        Creeps: CreepInfo list
        /// Hostile creeps standing in the spawn rooms this tick.
        Hostiles: HostileInfo list
        /// The invader cores standing in the rooms the colony works this
        /// tick and can see (ADR 0043). Its own list and not a widening of
        /// `Hostiles`, for two independent reasons. A core is a structure,
        /// so the `FIND_HOSTILE_CREEPS` sweep behind that list can never
        /// answer with one. And that list is the spawn rooms' by
        /// definition: since #138 a Threat's Reach is filed under the room
        /// it stands in, so an outpost raider no longer carves a hole at
        /// its coordinates in the home room — but ADR 0043 leaves
        /// `Snapshot.Hostiles`'s standing in the Task pipeline at exactly
        /// zero, and opening a front fifty tiles away before a worker
        /// being shot at home runs is the wrong order (#66). Reach and
        /// flee read none of this.
        ///
        /// No reader in Core yet: the fold that opens an outpost's threat
        /// episode off it (#134) and the gate that reads that episode
        /// (#136) come after. Projected ahead of them because the fact is
        /// only readable while the colony still has vision in the room,
        /// and the whole point of the gate is that the creeps who provide
        /// that vision are about to leave.
        InvaderCores: InvaderCoreInfo list
        /// The colony's spatial projection: the home room and every
        /// declared outpost beside it, in one projection (ADR 0041).
        /// Always present, possibly empty — absence is per-entry, never
        /// per-projection (ADR 0004).
        Spatial: SpatialInfo
        /// Every home room a human has declared a colony for (`Colony.homes`,
        /// ADR 0047), this colony's own included and in declaration order.
        /// The **candidate colonies** are the ones this colony does not own
        /// yet, and that second half is read off `RoomControl` in Core
        /// (`Decide.claimTargets`) rather than decided here: which rooms a
        /// human means to own is declared, whether we own one is seen, and
        /// the shell hands over facts rather than conclusions.
        ///
        /// It reaches the decision layer through the Snapshot and never off
        /// the constant, which is the same rule the declared outpost
        /// furniture already travels under (`Outpost.place`, ADR 0041):
        /// Core derives what it decides from the projection it is handed,
        /// so a colony's decisions can be built — and tested — for any
        /// declaration rather than only for the one this bot ships.
        /// Empty is the whole of "no colony is declared": nothing is
        /// claimed, and every controller in the projection is the [[reserve]]
        /// it always was.
        ColonyHomes: string list
    }

/// A unit of work in this tick's Task pool; creeps are interchangeable
/// executors that get matched to Tasks.
type Task =
    | Harvest of sourceId: string
    /// Take stored energy out of a stocked container (ADR 0012), or out of
    /// the Storage a tier below them (ADR 0023) — the haul cycle's intake,
    /// judged over stores rather than energy's name.
    ///
    /// A tombstone and a ruin are stores too (#167), and this is the verb
    /// for them: the engine's `withdraw` takes either, so the store's kind
    /// changes nothing here — the pool reads the stock, the cap divides it
    /// (#161) and the tier is the containers' own. What is different about
    /// them is only that they end: a store that decays away mid-walk is
    /// gone from the projection and releases its holder through the
    /// task-gone path every vanished Task uses.
    | Withdraw of storeId: string
    /// Walk to a dropped energy pile and take it (#167). The Task half of
    /// what the [[pickup reflex]] does by hand: the reflex takes what is
    /// already within range 1 of a creep standing somewhere for its own
    /// reasons, and this is what sends a creep to a pile that no reflex
    /// will ever reach — a death drop in the open, an [[anchor]]'s
    /// overflow on a [[container]] no hauler is due at.
    ///
    /// Pooled on the pile's amount alone, and only from a threshold
    /// (`pickupThreshold`), because a pile under one is not worth a
    /// walk that the reflex would cover for free if anyone ever passed
    /// it. Feeding tier and hauler-shaped applicability, the same as the
    /// Withdraw beside it: which of a pile and a container an empty
    /// carrier goes for is travel cost's call and never a rule's.
    | Pickup of pileId: string
    | Refill of structureId: string
    | Build of siteId: string
    | Repair of structureId: string
    | Upgrade of controllerId: string
    /// Holding a neutral controller with CLAIM parts (ADR 0042): a
    /// reservation is what makes that room's sources worth the held ten a
    /// tick rather than the neutral five, and it decays by one a tick, so
    /// this is work that is never finished. One per projected controller
    /// that is not the colony's own — the engine refuses reserveController
    /// on a room we own, and the colony's own controller is Upgraded, not
    /// reserved.
    ///
    /// Pooled whatever the reservation has left on it: the ticks remaining
    /// size the *body* (`ceil((5000 - ticksToEnd) / 600)` CLAIM, ADR 0042,
    /// #131) and never the Task, because a Task that vanished at the
    /// 5,000 cap would release its holder there and re-match it the tick
    /// after — a flicker ADR 0013 took out of Harvest for the same reason.
    /// Which rooms the colony works at all is the one gate above this, and
    /// it is the projection's: an outpost withdrawn from is out of the
    /// scan set entirely (ADR 0043).
    | Reserve of controllerId: string
    /// Taking a **candidate colony**'s controller for our own with CLAIM
    /// parts (ADR 0047): the act that turns a declared home room into an
    /// owned one, and so the first tick of a second colony. One per
    /// candidate colony — a declared home this colony does not own yet —
    /// and never for a plain [[outpost]], whose controller is [[reserve]]d
    /// instead: claiming costs a GCL level and asks the colony to run the
    /// room, which is a human's decision written in `Colony.declared` and
    /// never a rule the projection can infer.
    ///
    /// A controller carries exactly one of the three Tasks that act on
    /// one, and this is the one that wins: our own is Upgraded, a
    /// neutral controller is Reserved, and a candidate colony's is
    /// Claimed. Pooling Reserve beside it would put two Tasks on one
    /// target for one body to be matched to either, and the reservation is
    /// the work that becomes pointless the tick the claim lands.
    ///
    /// Unlike a reservation, which decays by one a tick, this is work that
    /// is finished the moment it succeeds: the room is ours, the Task is
    /// gone from the next tick's pool because the room is no longer a
    /// candidate, and the body that did it is a `[Claim; Move]` with
    /// nothing left to do. That is the price of the row sharing the
    /// [[reserver]]'s body (ADR 0047) and it is paid once per colony.
    | Claim of controllerId: string
    /// Getting out of a Threat's Reach (ADR 0033). The one Task with no
    /// target and no action: its Work Area is the tiles no Threat can
    /// hurt, and the Emitter issues movement for it and nothing else.
    | Flee

/// What kind of structure a placement Intent asks for.
type StructureKind =
    | Extension
    | Tower
    | Road
    | Container
    | Storage
    /// A rampart, over the Keep and the Posts (ADR 0034). The one
    /// defensive kind the Layout places, and the only placeable kind that
    /// goes on a tile something already stands on.
    | Rampart

/// One step of creep movement, engine vocabulary: Top decreases Y.
type Direction =
    | Top
    | TopRight
    | Right
    | BottomRight
    | Bottom
    | BottomLeft
    | Left
    | TopLeft

/// Every BodyPart — the closed set, for building tables over the
/// vocabulary. A literal, and so not compiler-checked: a part added to
/// the union has to be added here by hand. A successor chain does not
/// close that — the compiler checks such a function for exhaustiveness,
/// never for reachability, so a dangling `| NewPart -> None` compiles
/// clean and still leaves the list short. What closes it is `Core.Tests`,
/// which enumerates the union itself and fails when this list is short.
let allBodyParts =
    [ Work; Carry; Move; Attack; RangedAttack; Heal; BodyPart.Claim; Tough ]

/// Screeps body-part strings as the engine spells them, in `spawnCreep`
/// bodies and `creep.body` entries alike — the one place the spelling
/// lives (its reverse is derived from this table, never written twice).
let partName =
    function
    | Work -> "work"
    | Carry -> "carry"
    | Move -> "move"
    | Attack -> "attack"
    | RangedAttack -> "ranged_attack"
    | Heal -> "heal"
    | BodyPart.Claim -> "claim"
    | Tough -> "tough"

/// Every BuiltKind the engine spells — the modelled set, not the engine's
/// whole structure vocabulary, for building tables over the kinds. Every
/// spelling outside it classifies to Other, which is why Other is not one
/// of them: it is the absence of a modelled kind, never a kind with a
/// spelling of its own. A literal, and so not compiler-checked: a kind
/// added to the union has to be added here by hand, and `Core.Tests`
/// closes that the same way it does for `allBodyParts`.
let allBuiltKinds =
    [
        BuiltKind.Spawn
        BuiltKind.Extension
        BuiltKind.Tower
        BuiltKind.Road
        BuiltKind.Container
        BuiltKind.Storage
        BuiltKind.Link
        BuiltKind.Rampart
    ]

/// Screeps STRUCTURE_* strings as the engine spells them, in `structureType`
/// on structures and construction sites alike and in `createConstructionSite`
/// — the one place the spelling lives (its reverse is derived from this
/// table, never written twice). Other spells to nothing: it is the absence
/// of a modelled kind, so it stays out of `allBuiltKinds` and the empty
/// string never reaches the engine.
let builtKindName =
    function
    | BuiltKind.Spawn -> "spawn"
    | BuiltKind.Extension -> "extension"
    | BuiltKind.Tower -> "tower"
    | BuiltKind.Road -> "road"
    | BuiltKind.Container -> "container"
    | BuiltKind.Storage -> "storage"
    | BuiltKind.Link -> "link"
    | BuiltKind.Rampart -> "rampart"
    | BuiltKind.Other -> ""

/// The built kind a placement Intent's kind names: the one crossing
/// between the Intent vocabulary and the projection's, stated in Core
/// beside both unions rather than respelled wherever the two meet — the
/// Executor's site placement and any projection built on the .NET side
/// read the same widening (#75). Every placeable kind is a built kind;
/// the reverse does not hold — a Link is projected but never placed (ADR
/// 0022) — so the crossing runs this way only.
let builtKindOfPlaceable =
    function
    | Extension -> BuiltKind.Extension
    | Tower -> BuiltKind.Tower
    | Road -> BuiltKind.Road
    | Container -> BuiltKind.Container
    | Storage -> BuiltKind.Storage
    | Rampart -> BuiltKind.Rampart

/// The kinds Refill keeps fed (ADR 0010): the spawn-energy feeders and the
/// towers, the structures the Snapshot projects as Refillables. The
/// controller container and the Storage are Refill targets too, but the
/// Planner pools them off the projection's stores (ADR 0012, ADR 0023), so
/// they are not one of these.
let isRefillable =
    function
    | BuiltKind.Spawn
    | BuiltKind.Extension
    | BuiltKind.Tower -> true
    | BuiltKind.Road
    | BuiltKind.Container
    | BuiltKind.Storage
    | BuiltKind.Link
    | BuiltKind.Rampart
    | BuiltKind.Other -> false

/// The Keep (ADR 0034): the structures worth defending — the spawn, the
/// tower and the Storage. One list, three rules hang off it: a rampart
/// covers each of them, Repair keeps each at full hits, and any one of
/// them below full while a hostile stands in the room fires the safe-mode
/// reflex. The Posts are ramparted with the Keep but are not of it: a
/// container's hits never spend the stock.
let isKeep =
    function
    | BuiltKind.Spawn
    | BuiltKind.Tower
    | BuiltKind.Storage -> true
    | BuiltKind.Extension
    | BuiltKind.Road
    | BuiltKind.Container
    | BuiltKind.Link
    | BuiltKind.Rampart
    | BuiltKind.Other -> false

/// The kinds a raid's damage is charged on (ADR 0034): the Keep and the
/// ramparts that cover it. Not the roads and the containers, whose hits
/// the projection also carries — a chewed road is the colony's ordinary
/// decay, and charging it would drown the number the Raid log exists for.
/// Enumerated rather than written as "the Keep or a rampart" so that a
/// kind added to the union has to answer this question too.
let isDefence =
    function
    | BuiltKind.Spawn
    | BuiltKind.Tower
    | BuiltKind.Storage
    | BuiltKind.Rampart -> true
    | BuiltKind.Extension
    | BuiltKind.Road
    | BuiltKind.Container
    | BuiltKind.Link
    | BuiltKind.Other -> false

/// The kinds whose projection has to ask the engine who owns them: every
/// ownable kind whose hits a decision reads (ADR 0034). A structure of
/// another owner left standing in a room we took is neither ours to repair
/// nor ours to charge a raid's damage on, and "it stands in our spawn
/// room" is not the same fact as "it is ours". The decaying kinds are
/// deliberately not among them: a road and a container have no owner in
/// the engine at all, so asking would drop every one of them.
let needsOwner =
    function
    | BuiltKind.Spawn
    | BuiltKind.Tower
    | BuiltKind.Storage
    | BuiltKind.Rampart -> true
    | BuiltKind.Extension
    | BuiltKind.Road
    | BuiltKind.Container
    | BuiltKind.Link
    | BuiltKind.Other -> false

/// Where a kind is whole — which of the three rules judges its hits (ADR
/// 0034), never the numbers themselves: the fraction and the floor are the
/// Repair pool's tunables, stated where the pool that reads them is.
[<RequireQualifiedAccess>]
type WholeLine =
    /// A fraction of max hits: the decaying kinds (ADR 0010) — a road and
    /// a container are hungry below half of max and whole at it.
    | Fraction
    /// A fixed floor of hits: the rampart (ADR 0034). Half of max is the
    /// wrong shape for a structure whose max is three million at RCL4 and
    /// grows to three hundred — it would be hungry forever.
    | Floor
    /// Full hits: the Keep (ADR 0034). It does not decay, so below max
    /// means it was damaged and nothing else — and the safe-mode arm
    /// reads that same fact off the same hits.
    | Full

/// The line a kind is whole at, or None for a kind Repair never touches —
/// the extensions, a link, and every kind the decision layer does not
/// model (ADR 0010, widened by ADR 0034). The repairable kinds are exactly
/// the kinds whose hits the projection carries at all: fields nobody
/// decides on stay out.
let wholeLine =
    function
    | BuiltKind.Road
    | BuiltKind.Container -> Some WholeLine.Fraction
    | BuiltKind.Rampart -> Some WholeLine.Floor
    | BuiltKind.Spawn
    | BuiltKind.Tower
    | BuiltKind.Storage -> Some WholeLine.Full
    | BuiltKind.Extension
    | BuiltKind.Link
    | BuiltKind.Other -> None

/// The kinds whose stored energy enters the projection: the containers,
/// whose stock the logistics Tasks judge (ADR 0012), and the Storage,
/// whose Withdraw and Refill tiers read the same field (ADR 0023) — a
/// standing Storage's store is read exactly like a container's.
let isStored =
    function
    | BuiltKind.Container
    | BuiltKind.Storage -> true
    | BuiltKind.Spawn
    | BuiltKind.Extension
    | BuiltKind.Tower
    | BuiltKind.Road
    | BuiltKind.Link
    | BuiltKind.Rampart
    | BuiltKind.Other -> false

/// The kinds a creep can stand on; every other kind blocks its tile
/// (Screeps OBSTACLE_OBJECT_TYPES). Other is not walkable: a kind the
/// decision layer has no rules for is the one thing that must not quietly
/// open a tile, which is why Rampart is a case of its own.
let isWalkable =
    function
    | BuiltKind.Road
    | BuiltKind.Container
    | BuiltKind.Rampart -> true
    | BuiltKind.Spawn
    | BuiltKind.Extension
    | BuiltKind.Tower
    | BuiltKind.Storage
    | BuiltKind.Link
    | BuiltKind.Other -> false

/// Screeps direction constants as `Creep.move` expects them: TOP = 1, then clockwise.
let directionCode =
    function
    | Top -> 1
    | TopRight -> 2
    | Right -> 3
    | BottomRight -> 4
    | Bottom -> 5
    | BottomLeft -> 6
    | Left -> 7
    | TopLeft -> 8

/// A single described action to perform this tick; data only, never the game API.
type Intent =
    | SpawnCreep of spawnName: string * body: BodyPart list * creepName: string
    | PlaceConstructionSite of roomName: string * pos: Pos * kind: StructureKind
    | HarvestSource of creepName: string * sourceId: string
    | TransferEnergyToStructure of creepName: string * structureId: string
    | WithdrawEnergyFromStructure of creepName: string * structureId: string
    | BuildSite of creepName: string * siteId: string
    | RepairStructure of creepName: string * structureId: string
    | UpgradeController of creepName: string * controllerId: string
    /// The reserve act (ADR 0042): a CLAIM body standing beside a neutral
    /// controller pushes its reservation up by one tick per CLAIM part,
    /// which is what doubles that room's sources. Range 1, like the
    /// engine's other three touching acts.
    | ReserveController of creepName: string * controllerId: string
    /// The claim act (ADR 0047): a CLAIM body standing beside a neutral
    /// controller takes the room for this player. Range 1, like the
    /// engine's other four touching acts. The engine's own preconditions
    /// are not restated in Core: a claim needs a GCL level to spare and
    /// answers ERR_GCL_NOT_ENOUGH without one, and that code is read by
    /// nobody but the Executor's log — a Task pooled off the declaration
    /// and a room that stays unowned is the whole of what the decision
    /// layer sees, and it re-pools the Task next tick as it would after
    /// any other failure.
    | ClaimController of creepName: string * controllerId: string
    | PickupEnergy of creepName: string * resourceId: string
    | MoveCreep of creepName: string * direction: Direction
    | SayCreep of creepName: string * message: string
    | ActivateSafeMode of controllerId: string
    | FireTower of towerId: string * hostileId: string

/// Creep name -> task id. The only state remembered between ticks (anti-thrash).
type Assignments = Map<string, string>

/// A body's fatigue factor (ADR 0006): the parts that generate fatigue
/// when moving and the Move parts that pay it off. Terrain weight scales
/// by their ratio to price travel in cost units — half-ticks under the
/// engine-native weights (ADR 0010). The Atlas's own arithmetic, spelled
/// out here because the walk table below is keyed on it and outlives the
/// Atlas that fills it (ADR 0032).
type FatigueFactor = { FatigueParts: int; MoveParts: int }

/// The spawn-origin walk table (ADR 0032): the traffic-blind walk out of
/// the tiles beside a spawner, for a body's fatigue factor, as whole-tick
/// distances per tile index of one room (ADR 0026, ADR 0029) — the half of
/// a lead paid after the cast. Filled on demand by the Atlas as leads are
/// priced, and handed to the next tick's Atlas while the census signature
/// holds: every input it reads is in the census, so it runs once per
/// census rather than once per tick. Mutable, and heap-only like the memo
/// that carries it.
///
/// **The room in the key is the room the goal stands in**, and it is what
/// lets an outpost's lead ride here too (#169). Under the home room the
/// entry is the spawner's own flood; under a neighbour's it is the whole
/// cross-Seam walk — near leg, crossing and far leg already joined
/// (`Atlas.castWalkTicks`) — so a goal beyond a border costs one array
/// read rather than a flood per creep per tick. Two rooms hold the same
/// coordinates, so without the room a spawner's tile and an outpost's
/// answer would collide on one key; with it an entry means one thing under
/// either name: the ticks a body cast at this spawner needs to stand on
/// each tile of *that* room.
type WalkTable = System.Collections.Generic.Dictionary<Pos * FatigueFactor * string, int[]>

/// What a Link footing is held beside (ADR 0022, ADR 0027): each planned
/// source container, the controller container, the Storage. The Layout
/// knows a target's kind by construction — the target list is assembled
/// from exactly those three — and carries it so a footing the fold cannot
/// serve names the guarantee that was lost, not merely a tile.
[<RequireQualifiedAccess>]
type FootingKind =
    | SourceContainer
    | ControllerContainer
    | Storage

/// A footing target the Layout could not serve (#77): every tile within
/// range 1 of it was a trunk, another target, already taken by a footing,
/// or not buildable at all, so nothing was reserved for it. Recorded
/// rather than dropped — one footing per target is a guarantee, and a
/// guarantee that can degrade in silence is not one.
type UnservedFooting = { Target: Pos; Kind: FootingKind }

/// A footing target the Layout served (#106): the tile it reserved, beside
/// the target that tile is held for and that target's kind. The served
/// counterpart of `UnservedFooting`, which names a target and a kind and
/// no tile because there was none.
///
/// The pairing rather than the bare set of tiles, because the set is a
/// one-line projection of the pairing and the reverse is a search: a
/// reservation the bot never emits can otherwise only be cross-checked by
/// a second derivation (ADR 0035), and handing back tiles alone would
/// leave that derivation to be written by hand. The fold holds the target
/// and the kind in scope at the instant it picks the tile, so carrying
/// them costs nothing.
///
/// Two records rather than one whose tile is optional: only the unserved
/// half crosses the Memory boundary, as the layout record (ADR 0035), and
/// an optional tile would make every reader of either half ask which case
/// it holds — the partition is what the two names say.
type ServedFooting =
    {
        Target: Pos
        Kind: FootingKind
        Tile: Pos
    }

/// The two ends a trunk is routed to (ADR 0011): the controller's
/// Upgrade Work Area, and each spawn's walkable ring. A type of its own
/// because the loss is per goal and not per source — the goals are
/// collected per source, so one source can lose its line to the spawn and
/// keep the one to the controller. The spawn carries its id — the spawn
/// list is a list, and RCL7 adds a second one — where the Upgrade Work
/// Area is the controller's alone (ADR 0005) and needs no name beside its
/// own.
[<RequireQualifiedAccess>]
type TrunkGoal =
    | UpgradeArea
    | Spawn of spawn: string

/// A trunk the Layout could not route (#107): the router paved nothing
/// for this goal, because no tile of it was reachable from the source
/// once the clustered reservation was marked impassable — or because the
/// goal holds no tile at all, an unprojected controller or a spawn whose
/// every neighbour is wall. The two are one answer on purpose: a line
/// that carries nothing is the loss, and which way the geometry failed is
/// not something the colony can act on differently.
///
/// Recorded rather than dropped in silence — an empty path unions into
/// the road plan contributing nothing, so a source paved to nothing is
/// indistinguishable from a trunk that was never asked for. The room is
/// not fixed by saying so: the tiles paved and the tiles reserved are
/// exactly what they were (#105 owns the fix). ADR 0035's argument for
/// the footing shortfall, on the same channel and unchanged: a trunk has
/// no creep to key a Verdict on either.
type UnroutedTrunk = { Source: string; Goal: TrunkGoal }

/// What a container is planned for (ADR 0012): a source, named by its id,
/// or the controller. The two targets the container plan judges, and it
/// judges them independently — a tile can satisfy both at once (a [[dual
/// seat]] is within range 1 of a source and inside the Upgrade Work Area),
/// and ADR 0040 names that edge and leaves it rather than merging them.
/// The source carries its id where the controller needs none, the way a
/// `TrunkGoal`'s spawn does: a room has one controller (ADR 0005) and
/// several sources.
[<RequireQualifiedAccess>]
type ContainerTarget =
    | Source of source: string
    | Controller

/// A container pick the plan did not place because its target is already
/// served by a container standing somewhere else (ADR 0040): the target,
/// the tile the plan picked, and the tile actually serving it. The pick
/// moves when the trunk moves — a commit, not a tick — and the container
/// left on the old tile keeps serving the target, so the colony carries a
/// container on a worse tile rather than two containers.
///
/// Recorded rather than dropped, on the layout record beside the unserved
/// footings (#106) and the unroutable trunks (#107): nothing in this
/// colony demolishes anything (ADR 0040 keeps the orphan and #114 owns the
/// removal), so the difference between the plan and the room is permanent,
/// and an orphan no line anywhere names is a room whose Post and hauler
/// counts are read off geometry the plan no longer wants. Not a Verdict —
/// a container has no creep to key one on (ADR 0035).
type DeferredContainer =
    {
        Target: ContainerTarget
        Pick: Pos
        Serving: Pos
    }

/// The census-keyed plan memo (ADR 0017): the census signature beside the
/// plans derived from exactly that census — the Layout's site Intents,
/// the footings it placed and the ones it could not, the hauler quota,
/// and the spawn walks behind the leads (ADR 0032). Held by the host in
/// heap across ticks, never written to Memory: a global reset discards it
/// and the next tick recomputes from scratch. Same census, same plan, so
/// reuse never changes behaviour.
type PlanMemo =
    {
        Signature: string
        SiteIntents: Intent list
        /// The footing targets this plan left unserved (#77), derived from
        /// the same census as the site Intents and recomputed with them.
        /// Empty is the healthy answer and rides here all the same: the
        /// App writes it every tick, because a channel that says nothing
        /// when nothing is lost cannot be told from one that is not there.
        UnservedFootings: UnservedFooting list
        /// The footings this plan placed (#106), each naming its target,
        /// that target's kind and the tile reserved for it — derived from
        /// the same census as the site Intents and recomputed with them.
        /// No Intent ever names a link (ADR 0022) and this never crosses
        /// the Memory boundary, so the heap is the only place the tiles
        /// the fold reserved are observable at all: the whole-room
        /// invariant that a footing is off every trunk, off every target
        /// and off every other footing reads them here (ADR 0036).
        ServedFootings: ServedFooting list
        /// The trunks this plan could not route (#107), one entry per
        /// (source, goal) the router found no path for — derived from the
        /// same census as the site Intents and recomputed with them. Empty
        /// is the healthy answer and rides here all the same, for the
        /// reason `UnservedFootings` does: the App writes it every tick.
        UnroutedTrunks: UnroutedTrunk list
        /// The container picks this plan deferred to a container already
        /// serving their targets (ADR 0040), derived from the same census
        /// as the site Intents and recomputed with them. Empty is the
        /// healthy answer and rides here all the same, for the reason
        /// `UnservedFootings` does: the App writes it every tick.
        DeferredContainers: DeferredContainer list
        HaulerQuota: int
        /// The walks flooded under this signature, filled through the tick
        /// by the Atlas the table was handed to. Dropped whole when the
        /// signature moves — the Layout's own granularity, never per entry:
        /// a moved signature may have moved the weights or the body the
        /// walk is priced for, and telling which is a dependency tracker
        /// this memo deliberately does not have.
        Walks: WalkTable
    }

/// The reverse of a wire-name table, derived from the table itself: each
/// spelling is written once, in the name table, and the decoder reads
/// back what falls out of it. A name the vocabulary does not have reads
/// as None — the caller decides what a miss costs. The one builder: the
/// vocabularies below, the serialization shell's part table and the test
/// that round-trips them all call this, so no reverse is hand-rolled a
/// second time.
let reverseOf toName cases =
    let byName = cases |> List.map (fun case -> toName case, case) |> Map.ofList
    fun name -> Map.tryFind name byName

/// The same reversal for a vocabulary whose cases carry numbers beside
/// their name (#88). The entries are the cases' own constructors rather
/// than the cases, so each spelling is still written once — the name is
/// read off the case a constructor builds from a sample payload — and the
/// numbers the wire actually carried are handed back in on the way out: a
/// bare tag ignores them, a case that needs them reads as nothing without
/// them. So a name whose numbers are missing decodes to None exactly as
/// an unknown name does, and the caller decides what that costs rather
/// than restating a number nobody wrote.
let reverseCarrying toName sample (builders: ('p option -> 'a option) list) =
    let byName =
        builders
        |> List.choose (fun build ->
            build (Some sample) |> Option.map (fun case -> toName case, build))
        |> Map.ofList

    fun payload name -> Map.tryFind name byName |> Option.bind (fun build -> build payload)

/// What decided a fresh match: the first comparison that separated the
/// winning Task from its closest rival — rank tier, then travel cost, then
/// current load — or the tie-break when none did (pool order), or the fact
/// that no rival existed at all.
[<RequireQualifiedAccess>]
type MatchFactor =
    | OnlyCandidate
    | Rank
    | TravelCost
    | Load
    | PoolOrder

/// The wire spelling of each MatchFactor, in the observe channel's Memory
/// subtree (ADR 0009) — the one place the spelling lives, beside the
/// union it spells, the way `partName` holds the engine's part spelling.
let matchFactorName =
    function
    | MatchFactor.OnlyCandidate -> "only-candidate"
    | MatchFactor.Rank -> "rank"
    | MatchFactor.TravelCost -> "travel-cost"
    | MatchFactor.Load -> "load"
    | MatchFactor.PoolOrder -> "pool-order"

/// The MatchFactor a wire name spells, or None for a name this vocabulary
/// does not have. The case list is a literal, so a case added without its
/// entry decodes to nothing; `Core.Tests` round-trips the union itself and
/// fails on exactly that.
let matchFactorOf =
    reverseOf
        matchFactorName
        [
            MatchFactor.OnlyCandidate
            MatchFactor.Rank
            MatchFactor.TravelCost
            MatchFactor.Load
            MatchFactor.PoolOrder
        ]

/// Why a remembered assignment was released: its Task left the pool, a
/// Threat's Reach has taken the whole of its Work Area (ADR 0033) — the
/// release a raid writes to the transition log, and the reason asked
/// first, because a Task with nowhere to stand is gone for this creep
/// however well its body fits — the creep can no longer usefully work it
/// (body parts or energy state), the Task's worker cap was already full,
/// its Work Area is unreachable or empty (ADR 0002), or its time has not
/// come — the creep's walk no
/// longer covers a drained source's restock wait (ADR 0025), which is how
/// a creep beside a dry rock leaves it now that the Task stays pooled.
/// That last reason carries the two numbers the gate compared, the walk
/// and the wait (#88): a creep released mid-trip owes the same
/// explanation as a candidate rejected at the gate, and since ADR 0029
/// the walk cannot be recovered by halving anything.
[<RequireQualifiedAccess>]
type ReleaseReason =
    | TaskGone
    | Inapplicable
    | OverCapacity
    | Unreachable
    | Threatened
    | TooEarly of walk: int * wait: int

/// The wire spelling of each ReleaseReason, as `matchFactorName` is
/// MatchFactor's.
let releaseReasonName =
    function
    | ReleaseReason.TaskGone -> "task-gone"
    | ReleaseReason.Inapplicable -> "inapplicable"
    | ReleaseReason.OverCapacity -> "over-capacity"
    | ReleaseReason.Unreachable -> "unreachable"
    | ReleaseReason.Threatened -> "threatened"
    | ReleaseReason.TooEarly _ -> "too-early"

/// The numbers a ReleaseReason carries beside its wire name, or None for
/// a bare tag. The encoder's half of what `releaseReasonOf` reads back,
/// beside the union the way the name table is: a case's payload is spelt
/// out in one place, not once per row shape that carries it.
let releaseReasonNumbers =
    function
    | ReleaseReason.TooEarly(walk, wait) -> Some(walk, wait)
    | ReleaseReason.TaskGone
    | ReleaseReason.Inapplicable
    | ReleaseReason.OverCapacity
    | ReleaseReason.Unreachable
    | ReleaseReason.Threatened -> None

/// The ReleaseReason a wire name spells for the numbers the wire carried
/// beside it, or None for a name this vocabulary does not have — and for
/// `too-early` with no numbers to be about.
let releaseReasonOf =
    reverseCarrying
        releaseReasonName
        (0, 0)
        [
            (fun _ -> Some ReleaseReason.TaskGone)
            (fun _ -> Some ReleaseReason.Inapplicable)
            (fun _ -> Some ReleaseReason.OverCapacity)
            (fun _ -> Some ReleaseReason.Unreachable)
            (fun _ -> Some ReleaseReason.Threatened)
            Option.map ReleaseReason.TooEarly
        ]

/// Why an unassigned creep got nothing: the pool was empty, no Task fit
/// its body or energy state, every fitting Task's worker cap was full,
/// every fitting Task with room had an unreachable Work Area, or every
/// Task it could otherwise have taken is one whose time has not come
/// (ADR 0025). Reports the deepest matching gate any Task reached, so a
/// creep waiting out a drained source's restock says exactly that rather
/// than claiming nothing fit its body.
[<RequireQualifiedAccess>]
type IdleReason =
    | NoTasks
    | NoneApplicable
    | NoneFree
    | NoneReachable
    | NoneInTime

/// The wire spelling of each IdleReason, as `matchFactorName` is
/// MatchFactor's.
let idleReasonName =
    function
    | IdleReason.NoTasks -> "no-tasks"
    | IdleReason.NoneApplicable -> "none-applicable"
    | IdleReason.NoneFree -> "none-free"
    | IdleReason.NoneReachable -> "none-reachable"
    | IdleReason.NoneInTime -> "none-in-time"

/// The IdleReason a wire name spells, or None for a name this vocabulary
/// does not have.
let idleReasonOf =
    reverseOf
        idleReasonName
        [
            IdleReason.NoTasks
            IdleReason.NoneApplicable
            IdleReason.NoneFree
            IdleReason.NoneReachable
            IdleReason.NoneInTime
        ]

/// Why a Task in the pool was rejected for a creep, in a verbose scoring:
/// a Threat's Reach has taken the whole of its Work Area (ADR 0033), it
/// did not fit the creep's body or energy state, its worker cap was
/// already full, its Work Area is unreachable, or its time has not come —
/// the matching gates, in the order they are tried. The Reach is asked
/// ahead of the body because it is not a fact about the creep at all: an
/// area nobody may stand in is no Task for anyone. The last is its own
/// reason rather than Inapplicable (ADR 0025): the body and the energy
/// state fit, only the arrival doesn't, and the transition log would lie.
/// It carries the walk and the wait the gate compared (#88) — the scored
/// row is not widened for it, because only a rejected row raises the
/// question of how long the creep still has to wait.
[<RequireQualifiedAccess>]
type RejectReason =
    | Inapplicable
    | CapacityFull
    | Unreachable
    | Threatened
    | TooEarly of walk: int * wait: int

/// The wire spelling of each RejectReason, as `matchFactorName` is
/// MatchFactor's.
let rejectReasonName =
    function
    | RejectReason.Inapplicable -> "inapplicable"
    | RejectReason.CapacityFull -> "capacity-full"
    | RejectReason.Unreachable -> "unreachable"
    | RejectReason.Threatened -> "threatened"
    | RejectReason.TooEarly _ -> "too-early"

/// The numbers a RejectReason carries, as `releaseReasonNumbers` is
/// ReleaseReason's.
let rejectReasonNumbers =
    function
    | RejectReason.TooEarly(walk, wait) -> Some(walk, wait)
    | RejectReason.Inapplicable
    | RejectReason.CapacityFull
    | RejectReason.Unreachable
    | RejectReason.Threatened -> None

/// The RejectReason a wire name spells for the numbers the wire carried
/// beside it, as `releaseReasonOf` is ReleaseReason's.
let rejectReasonOf =
    reverseCarrying
        rejectReasonName
        (0, 0)
        [
            (fun _ -> Some RejectReason.Inapplicable)
            (fun _ -> Some RejectReason.CapacityFull)
            (fun _ -> Some RejectReason.Unreachable)
            (fun _ -> Some RejectReason.Threatened)
            Option.map RejectReason.TooEarly
        ]

/// The wire spelling of each FootingKind, on the Layout channel's Memory
/// leaf (#77), as `matchFactorName` is MatchFactor's. Not a Verdict
/// vocabulary — the Layout speaks no Verdicts, which is the whole reason
/// its losses need a channel — but the same rule: one spelling, written
/// once, round-tripped against the union itself by `Core.Tests`.
let footingKindName =
    function
    | FootingKind.SourceContainer -> "source-container"
    | FootingKind.ControllerContainer -> "controller-container"
    | FootingKind.Storage -> "storage"

/// The FootingKind a wire name spells, or None for a name this vocabulary
/// does not have.
let footingKindOf =
    reverseOf
        footingKindName
        [
            FootingKind.SourceContainer
            FootingKind.ControllerContainer
            FootingKind.Storage
        ]

/// The wire spelling of each TrunkGoal, on the Layout channel's Memory
/// leaf beside `footingKindName` (#107). A carrying vocabulary, like the
/// two reason vocabularies (#88): the spawn's id rides beside the name
/// rather than inside it, so a goal is one spelling and not one per
/// spawn.
let trunkGoalName =
    function
    | TrunkGoal.UpgradeArea -> "upgrade-area"
    | TrunkGoal.Spawn _ -> "spawn"

/// The spawn a TrunkGoal names beside its wire name, or None for the goal
/// that names none. The encoder's half of what `trunkGoalOf` reads back,
/// as `releaseReasonNumbers` is ReleaseReason's.
let trunkGoalSpawn =
    function
    | TrunkGoal.Spawn spawn -> Some spawn
    | TrunkGoal.UpgradeArea -> None

/// The TrunkGoal a wire name spells for the spawn the wire carried beside
/// it, or None for a name this vocabulary does not have — and for `spawn`
/// with no id carried beside it at all, which is a row that lost its
/// spawn rather than a goal. An id that is carried but empty is a spawn
/// like any other here; the vocabulary spells names, and what counts as a
/// usable id is the caller's question.
let trunkGoalOf =
    reverseCarrying
        trunkGoalName
        ""
        [ (fun _ -> Some TrunkGoal.UpgradeArea); Option.map TrunkGoal.Spawn ]

/// The wire spelling of each ContainerTarget, on the Layout channel's
/// Memory leaf beside `trunkGoalName` (ADR 0040). A carrying vocabulary
/// like it, and for the same reason: the source's id rides beside the
/// name rather than inside it, so a target is one spelling and not one
/// per source.
let containerTargetName =
    function
    | ContainerTarget.Source _ -> "source"
    | ContainerTarget.Controller -> "controller"

/// The source a ContainerTarget names beside its wire name, or None for
/// the controller, which names none. The encoder's half of what
/// `containerTargetOf` reads back, as `trunkGoalSpawn` is TrunkGoal's.
let containerTargetSource =
    function
    | ContainerTarget.Source source -> Some source
    | ContainerTarget.Controller -> None

/// The ContainerTarget a wire name spells for the source the wire carried
/// beside it, or None for a name this vocabulary does not have — and for
/// `source` with no id carried beside it at all, which is a row that lost
/// its source rather than another target.
let containerTargetOf =
    reverseCarrying
        containerTargetName
        ""
        [
            Option.map ContainerTarget.Source
            (fun _ -> Some ContainerTarget.Controller)
        ]

/// The wire spelling of each StandDownBasis, on the Raid log's Memory
/// leaf (ADR 0043), as `footingKindName` is the Layout channel's. The
/// Raid log's own first vocabulary beside the body parts its roster
/// already carries, and under the same rule: one spelling, written once
/// here, reversed by the table below and round-tripped against the union
/// itself by `Core.Tests`, so a fourth basis added without a name is a red
/// test rather than a stand-down that decodes to nothing.
let standDownBasisName =
    function
    | StandDownBasis.CollapseTimer -> "collapse-timer"
    | StandDownBasis.Reservation -> "reservation"
    | StandDownBasis.Fallback -> "fallback"

/// The StandDownBasis a wire name spells, or None for a name this
/// vocabulary does not have — a row whose basis will not read back is a
/// stand-down that cannot say why, and the shell drops that row rather
/// than inventing a reason for it.
let standDownBasisOf =
    reverseOf
        standDownBasisName
        [
            StandDownBasis.CollapseTimer
            StandDownBasis.Reservation
            StandDownBasis.Fallback
        ]

/// One row of a verbose scoring: a Task in the pool, either scored on the
/// full matching key — rank tier, travel cost, current load — or rejected
/// at the first gate it failed. The answer to "why *not* that Task".
[<RequireQualifiedAccess>]
type Candidate =
    | Scored of task: string * rank: int * cost: int * load: int
    | Rejected of task: string * reason: RejectReason

/// The reasoned outcome a decision step returns beside its decision — data,
/// never a log line (ADR 0009). The Matcher speaks at conclusion level:
/// which Task won a creep and what decided it, a remembered assignment kept
/// (anti-thrash) as distinct from a fresh match, a release with its reason,
/// or why nothing was applicable. The Resolver speaks only when something
/// became of a creep's movement: grounded by fatigue (ADR 0008), yielded —
/// settled off its preferred tile, naming the counterpart creep that holds
/// it — or rerouted, detoured by the occupancy surcharge. A creep that
/// simply steps toward its Work Area says nothing: conclusion level means
/// events, not every step. Tasks are named by task id. A creep on the
/// verbose list additionally gets a Scoring Verdict: the whole pool as
/// Candidates, judged against the state its match was decided from.
[<RequireQualifiedAccess>]
type Verdict =
    | Matched of creep: string * task: string * factor: MatchFactor
    | Kept of creep: string * task: string
    | Released of creep: string * task: string * reason: ReleaseReason
    | Unassigned of creep: string * reason: IdleReason
    | Scoring of creep: string * candidates: Candidate list
    | Grounded of creep: string
    | Yielded of creep: string * counterpart: string
    | Rerouted of creep: string

/// What one tick of deciding returns: the Intents to execute, the
/// Assignments to remember for next tick, the plan memo to hold in heap
/// for next tick (ADR 0017), and the Verdicts explaining them (ADR 0009).
type Decision =
    {
        Intents: Intent list
        Assignments: Assignments
        Memo: PlanMemo
        Verdicts: Verdict list
    }
