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
        /// Whether any energy has been built into it yet (`progress > 0`):
        /// a begun site in a [[nursery]] is what its safe mode guards.
        Begun: bool
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
        /// Hits left (#451): what a fight's kill order reads, and with the
        /// engine taking parts from the head, which of `Body` still act.
        Hits: int
        /// Ticks this hostile has left. An Invader standing in a room nobody
        /// owns never suicides — the engine's suicide branch wants a
        /// controller owner and an [[outpost]] has none — so what it has left
        /// is exactly what it will spend, and this is the one deadline a raid
        /// with no core in it offers the [[stand-down]] (#257).
        TicksToLive: int
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HostileInfo =
    /// ADR-0033. The range a hostile can hurt a creep from, or None for one
    /// that cannot.
    let weaponRange (hostile: HostileInfo) : int option =
        [
            if List.contains Attack hostile.Body then
                Engine.meleeRange
            if List.contains RangedAttack hostile.Body then
                Engine.rangedRange
        ]
        |> function
            | [] -> None
            | ranges -> Some(List.max ranges)

    /// Whether a hostile can hurt anything at all: `weaponRange` asked as a
    /// yes/no, written once because many rules turn on it.
    let isArmed (hostile: HostileInfo) : bool = weaponRange hostile |> Option.isSome

    /// What a hostile heals a tick, its HEAL parts priced unboosted: the
    /// projection carries no boosts.
    let healing (hostile: HostileInfo) : int =
        Engine.healPower * partCountIn hostile.Body Heal

    /// How many of one part still act: the engine destroys parts from the
    /// head of the body, so the live ones are the tail its hits still cover,
    /// a part with any hit left counting whole.
    let activeCount (hostile: HostileInfo) (part: BodyPart) : int =
        let live = (hostile.Hits + Engine.partHits - 1) / Engine.partHits

        hostile.Body
        |> List.skip (max 0 (List.length hostile.Body - live))
        |> List.filter ((=) part)
        |> List.length

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
    /// A squad the ranger cannot beat, last seen standing in a [[harassment
    /// room]] (#441): that sighting plus `Tuning.HarassClearTicks`, refreshed
    /// by every later one. The room is ours to walk back into, so a squad that
    /// crosses it once shuts it briefly and not for its whole life.
    | HarassSighting

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
/// thing carried **across** ticks about a room. `Targets` answers one
/// question only, and `Rival` one other. Per-entry absence cannot say *why* an id left the pool, and the two
/// answers are opposite work: a container destroyed is a Task gone, a room
/// gone dark is a Task waiting. So `Targets` is read by the vision grace and
/// by nothing else, and what the grace hands on is a **room name**: the Matcher
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
        /// Who owned the room's controller that tick, when it was another
        /// player and not us; None for a room nobody owns, ours, or one
        /// with no controller. The second thing a sighting is read for
        /// (#444): no chain enters a room a rival owns (`World.rivalHeld`),
        /// and a room nothing of ours stands in has no vision to ask.
        Rival: string option
    }

/// The exit a room's last armed Threat left by, and how long its Guard holds
/// it (#450): attacks do not cross a border, so a body re-entering lands on
/// the edge tile beside a guard standing there.
type ExitHold =
    {
        /// The walkable exit tiles of one side, contiguous, through the one
        /// nearest where the Threat was last seen.
        Run: Pos list
        /// The first tick the hold no longer stands: `Tuning.ExitHoldTicks`
        /// after it left, or the tick its remembered `ticksToLive` runs out if
        /// sooner.
        Until: int
    }

[<RequireQualifiedAccess>]
module ExitHold =
    /// Whether the hold still stands at `time`.
    let stands (time: int) (hold: ExitHold) = hold.Until > time

/// One armed Threat as the full tick saw it: where it stood, and the tick its
/// `ticksToLive` runs out.
type SeenThreat = { At: Pos; Dies: int }

/// What the world remembers of one room's armed Threats across ticks
/// (`World.watchExits`, #450). Heap state only: a reset costs one hold.
[<RequireQualifiedAccess>]
type ExitWatch =
    /// Armed Threats stood in the room on the last full tick, which saw it.
    | Seen of SeenThreat list
    /// They are gone, and the room's Guard holds the exit they left by.
    | Held of ExitHold

/// `World.linkedRecalling`'s memo: `(keeper margin, from, to)` to whether a
/// crossing joins the pair. Mutable and heap-only, like `WalkTable`, and the
/// **shell's**: one table for the life of the process, because every answer
/// in it is the terrain's. A test lays a table of its own per call
/// (`linkedBy`, `scanOf`, `creepColonies`, `ColonyView.ofWorld`); what is
/// forbidden is a **static** holding one, shared across Expecto's parallel
/// lists (#310, `ParallelSafetyTests`).
///
/// Keyed by the three joined into one string (`margin|from|to`; a room name
/// holds no `|`) and not by the tuple: Fable hashes a string key into a
/// native JS `Map`, where a tuple key is hashed and compared field by field
/// on every read, and the scan set's chain search reads this per step.
///
/// Beside it, the hop counts the scan set and the harassment casting ask of
/// the same relation (`World.hopsUnder`): a chain search per declaration per
/// colony, twice a tick, for an answer that moves only with the rooms the
/// world holds, the rooms withheld from passage and the rooms a rival owns,
/// all of which ride in its key. Who owns a room is never a `Joins` answer:
/// it is asked ahead of the table, as which rooms the world holds is.
type JoinTable() =
    member val Joins = System.Collections.Generic.Dictionary<string, bool>()
    member val Hops = System.Collections.Generic.Dictionary<string, int>()
    /// The ordered pairs filed.
    member this.Count = this.Joins.Count

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
        /// room it did not. Heap state in the shell, and only each rival
        /// room's owner is kept in Memory (`seedRivals`): a global reset
        /// empties the rest. A room never seen has no entry.
        Sightings: Map<string, RoomSighting>
        /// The rooms of ours a tower of our own has stood full in
        /// (`World.latchTowers`, #445): the latch that keeps a child out of
        /// `Weaning` while its towers fire. Heap state the shell also keeps
        /// in Memory, so a global reset does not forget it.
        Towered: Set<string>
        /// Each room's armed Threats as last seen, or the exit they left by
        /// (`World.watchExits`, #450). Heap state in the shell, never Memory.
        ExitWatches: Map<string, ExitWatch>
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
            Towered = Set.empty
            ExitWatches = Map.empty
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

    /// Whether safe mode is running in any room of ours this tick: one per
    /// shard, so while it runs no other room of ours can raise its own.
    let safeModeRunning (world: World) : bool =
        world.Rooms
        |> Map.exists (fun _ facts ->
            facts.Control
            |> Option.exists (fun control -> control.Owner = Ownership.Ours && control.SafeMode))

    /// How many towers of ours in the room hold a shot's energy this tick.
    let loadedTowers (facts: RoomFacts) : int =
        facts.Refillables
        |> List.filter (fun r ->
            r.Kind = BuiltKind.Tower
            && Engine.towerCapacity - r.FreeCapacity >= Engine.towerEnergyCost)
        |> List.length

    /// ADR-0080
    /// Whether a colony's home cannot hold the raid standing in it this tick:
    /// the room is ours, an armed hostile that is not an ally is there, safe
    /// mode is off, and the raid heals at least what the towers holding a
    /// shot's energy land at the falloff range.
    let homeBeaten (facts: RoomFacts) : bool =
        let raid =
            facts.Hostiles |> List.filter (fun hostile -> not (Colony.isAlly hostile.Owner))

        let towers = loadedTowers facts

        facts.Control
        |> Option.exists (fun control -> control.Owner = Ownership.Ours && not control.SafeMode)
        && List.exists HostileInfo.isArmed raid
        && List.sumBy HostileInfo.healing raid
           >= towers * Engine.towerAttackAt Engine.towerFalloffRange

    /// Every spawn's name beside the room it stands in: what `Colony.castBy`
    /// reads a creep's caster off.
    let spawnHomes (world: World) : (string * string) list =
        world.Rooms
        |> Map.toList
        |> List.collect (fun (name, facts) ->
            facts.Spawns |> List.map (fun spawn -> spawn.Name, name))

    /// Whether a guard `mother` cast stands in one of these rooms. The
    /// casters are read only once a guard body is found there.
    let private guardCastIn (world: World) (mother: string) (rooms: string list) : bool =
        let standing =
            world.Creeps
            |> List.filter (fun creep ->
                isGuardParts creep.Info.Body && List.contains creep.Room rooms)

        not (List.isEmpty standing)
        && (let homes = spawnHomes world

            standing
            |> List.exists (fun creep -> Colony.castBy homes creep.Info.Name = Some mother))

    /// Whether a mother defends one child's home this tick: while it is
    /// beaten, and past that while a guard she cast still stands on the
    /// defended chain (the home and the rooms between) outside the rooms she
    /// projects anyway. The second half is the latch, read off the world and
    /// kept nowhere: a raid the towers hold again once her guard has come
    /// keeps the room hers, so the guard is not handed to a child with no
    /// Guard to give it; and a guard left on the chain when the raid ends
    /// stays placed and walks home. It lets go the tick the last one walks
    /// out.
    let defends (world: World) (covered: string list) (mother: string) (child: string) : bool =
        homeBeaten (roomOf world child)
        || child :: RoomName.transitBetween mother child
           |> List.filter (fun room -> not (List.contains room covered))
           |> guardCastIn world mother

    /// The rooms we own this tick, off the control entry vision paid for: one
    /// of the two facts a colony has to pass to be **living**.
    let ownedRooms (world: World) : Set<string> =
        world.Rooms
        |> Map.toList
        |> List.filter (fun (_, facts) ->
            facts.Control |> Option.exists (fun control -> control.Owner = Ownership.Ours))
        |> List.map fst
        |> Set.ofList

    /// Whether a tower of ours in the room holds `Tuning.IndependenceTowerEnergy`
    /// this tick.
    let towerFull (tuning: Tuning) (facts: RoomFacts) : bool =
        facts.Refillables
        |> List.exists (fun r ->
            r.Kind = BuiltKind.Tower
            && Engine.towerCapacity - r.FreeCapacity >= tuning.IndependenceTowerEnergy)

    /// This tick's world with the tower latch laid under it (#445): last
    /// tick's `Towered` kept for the rooms still ours, and every room of ours
    /// whose tower stands full now added. A room that stops being ours drops
    /// out, so one claimed again is raised again. Built by `Fresh`, the set
    /// being carried to the next tick.
    let latchTowers (tuning: Tuning) (previous: Set<string>) (world: World) : World =
        let owned = ownedRooms world

        { world with
            Towered =
                owned
                |> Seq.filter (fun room ->
                    Set.contains room previous || towerFull tuning (roomOf world room))
                |> Fresh.setOfSeq
        }

    /// The exit a body standing on `From` could have stepped out by, and the
    /// run of its side's walkable exit tiles contiguous with it.
    type private ExitRun =
        {
            From: Pos
            Through: Pos
            Run: Pos list
        }

    /// The squared straight-line distance between two tiles: a step beside one
    /// is nearer than a diagonal, which range does not say.
    let private squaredDistance (a: Pos) (b: Pos) =
        let dx = b.X - a.X
        let dy = b.Y - a.Y
        dx * dx + dy * dy

    /// How far the body stood from the exit it took.
    let private distanceOf (run: ExitRun) = squaredDistance run.From run.Through

    /// The exit run a body standing on `tile` could have stepped out by,
    /// through the walkable exit tile nearest it within a step, on whichever
    /// side. None for a tile no exit is a step from — a Threat that vanished
    /// there died, it did not leave.
    let private exitRunFrom (border: Map<Pos, Terrain>) (tile: Pos) : ExitRun option =
        let walkable (p: Pos) =
            Seam.isExit p && (Map.tryFind p border |> Option.exists (fun t -> t <> Wall))

        tilesWithin 1 tile
        |> List.filter walkable
        |> List.sortBy (fun exit -> squaredDistance tile exit, exit.X, exit.Y)
        |> List.tryHead
        |> Option.map (fun exit ->
            let vertical = exit.X = 0 || exit.X = Seam.exitEdge

            let step d =
                if vertical then
                    { exit with Y = exit.Y + d }
                else
                    { exit with X = exit.X + d }

            let along dir =
                Seq.initInfinite (fun i -> step (dir * (i + 1)))
                |> Seq.takeWhile walkable
                |> List.ofSeq

            {
                From = tile
                Through = exit
                Run = List.rev (along -1) @ [ exit ] @ along 1
            })

    /// This tick's world with the exit watch laid under it (#450): a room
    /// vision finds an armed Threat in is `Seen`; the next full tick vision
    /// finds them gone, the run the nearest of them to an exit left by is
    /// `Held` until `Tuning.ExitHoldTicks` or that one's remembered death. A
    /// room out of sight keeps a hold until its clock runs out, and forgets a
    /// sighting: where a Threat went in the dark is not known. Bounded by this
    /// tick's rooms, as `recalling` is, and built by `Fresh`, the map being
    /// carried to the next tick.
    let watchExits (tuning: Tuning) (previous: Map<string, ExitWatch>) (world: World) : World =
        let seen room =
            Map.tryFind room world.Sightings
            |> Option.exists (fun sighting -> sighting.Tick = world.Time)

        let watch (room, facts: RoomFacts) =
            let armed =
                facts.Hostiles
                |> List.filter (fun h -> HostileInfo.isArmed h && not (Colony.isAlly h.Owner))

            match seen room, armed, Map.tryFind room previous with
            | true, _ :: _, _ ->
                Some(
                    room,
                    ExitWatch.Seen(
                        armed
                        |> List.map (fun h ->
                            {
                                At = RoomPos.pos h.Pos
                                Dies = world.Time + h.TicksToLive
                            })
                    )
                )
            | true, [], Some(ExitWatch.Seen last) ->
                last
                |> List.choose (fun threat ->
                    exitRunFrom facts.Border threat.At |> Option.map (fun run -> run, threat.Dies))
                |> List.sortBy (fun (run, _) -> distanceOf run)
                |> List.tryHead
                |> Option.map (fun (run, dies) ->
                    {
                        Run = run.Run
                        Until = min dies (world.Time + tuning.ExitHoldTicks)
                    })
                |> Option.filter (ExitHold.stands world.Time)
                |> Option.map (fun hold -> room, ExitWatch.Held hold)
            | false, _, Some(ExitWatch.Held hold)
            | true, [], Some(ExitWatch.Held hold) when ExitHold.stands world.Time hold ->
                Some(room, ExitWatch.Held hold)
            | _ -> None

        { world with
            ExitWatches = world.Rooms |> Map.toSeq |> Seq.choose watch |> Fresh.mapOfSeq
        }

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
    ///
    /// The tower a child waits for (#445) is asked only of a declared
    /// child: a colony with no mother has nobody raising it to wait on. A
    /// tower stood full is the latch (`Towered`) or one full this tick.
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

            let raised =
                colonies
                |> List.exists (fun colony -> colony.Home = name && Option.isSome colony.Mother)

            let towerStood =
                not raised || Set.contains name world.Towered || towerFull tuning facts

            Colony.stageOf
                tuning
                owned
                (not (List.isEmpty facts.Spawns))
                towerStood
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

    /// Whether the world last saw another player, not an ally, own the
    /// room's controller (`RoomSighting.Rival`). Read off the memory and not
    /// this tick's vision, which a room nothing of ours stands in does not
    /// have. A reservation is not ownership.
    let rivalHeld (world: World) (room: string) : bool =
        match Map.tryFind room world.Sightings with
        | Some sighting ->
            match sighting.Rival with
            | Some owner -> not (Colony.isAlly owner)
            | None -> false
        | None -> false

    /// Every room `rivalHeld` answers yes for, in room-name order.
    let rivalRooms (world: World) : string list =
        world.Sightings |> Map.toList |> List.map fst |> List.filter (rivalHeld world)

    /// Who the world last saw owning each room another player owns, allies
    /// included: what Memory keeps across a global reset (`seedRivals`).
    /// Built by `Fresh`, being carried to the next tick.
    let rivalOwners (world: World) : Map<string, string> =
        world.Sightings
        |> Map.toList
        |> List.choose (fun (room, sighting) ->
            sighting.Rival |> Option.map (fun owner -> room, owner))
        |> Fresh.mapOfList

    /// The heap's sightings with an owner read back off Memory laid under
    /// them, for the first tick after a global reset: a room the heap holds
    /// no sighting of gets one naming its owner and nothing else. It carries
    /// no targets, so the vision grace never reads its tick.
    let seedRivals (owners: Map<string, string>) (sightings: Map<string, RoomSighting>) =
        (sightings, owners)
        ||> Map.fold (fun seeded room owner ->
            if Map.containsKey room seeded then
                seeded
            else
                Map.add
                    room
                    {
                        Tick = 0
                        Targets = lazy Set.empty
                        Rival = Some owner
                    }
                    seeded)

    /// `linked`'s terrain half: everything it asks but who owns the room.
    /// What `linkedRecalling` files, because it is the half that never moves.
    let private seamed (keeperMargin: int) (world: World) (fromRoom: string) (toRoom: string) =
        let walkableIn room =
            ringWalkable keeperMargin room (roomOf world room).Border

        Keepers.enterable toRoom
        && Seam.joinedBy
            (walkableIn fromRoom)
            (walkableIn toRoom)
            (groundWalkable keeperMargin toRoom (roomOf world toRoom).Layer.Terrain)
            fromRoom
            toRoom

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
    ///
    /// A keeper room with no declared rocks is entered by nothing
    /// (`Keepers.enterable`), as `Atlas.routes` asks it.
    ///
    /// Nor is a room another player owns (`rivalHeld`, #444) — a room a body
    /// already stands in is still left by it.
    let linked (keeperMargin: int) (world: World) (fromRoom: string) (toRoom: string) : bool =
        not (rivalHeld world toRoom) && seamed keeperMargin world fromRoom toRoom

    /// `linked` over a table the caller holds **across ticks**: three readers
    /// ask it per colony per tick (`scanOf`, `ColonyView.ofWorld`,
    /// `creepColonies`), re-asking per hop — 93 calls over 26 distinct pairs
    /// in one colony's tick before any table stood, and 5.7% of a `reactor
    /// --level 7` tick building and dropping three tables a colony once one
    /// did (`docs/profiling.md` § History, 2026-09-18).
    ///
    /// The table can outlive the tick because an answer reads only terrain —
    /// which the engine never changes — under a keeper margin that is in the
    /// key. What *can* move between ticks is which rooms the world holds and
    /// who owns them, so both are read ahead of the table on every ask.
    /// Asymmetric by construction, like `linked`: the key is the ordered pair
    /// and an answer is never reused backwards.
    let linkedRecalling
        (joins: JoinTable)
        (keeperMargin: int)
        (world: World)
        : string -> string -> bool =
        fun fromRoom toRoom ->
            if
                not (Map.containsKey fromRoom world.Rooms && Map.containsKey toRoom world.Rooms)
                || rivalHeld world toRoom
            then
                false
            else
                let key = $"{keeperMargin}|{fromRoom}|{toRoom}"

                // `ContainsKey` then the indexer, never `TryGetValue` in a
                // match: Fable compiles the out-parameter pattern into four
                // allocations per read (`Atlas.memoised`'s note).
                if joins.Joins.ContainsKey key then
                    joins.Joins.[key]
                else
                    let joined = seamed keeperMargin world fromRoom toRoom
                    joins.Joins.[key] <- joined
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
    /// avoids no stronghold.
    let reachesUnder (gate: StandDown) (joins: JoinTable) (tuning: Tuning) (world: World) =
        linkedRecalling joins (Tuning.keeperMargin tuning) world
        |> linkedAvoiding gate.Impassable

    /// `Declaration.hops` over `reachesUnder`, recalled from the join table:
    /// keyed by everything the chain search reads that is not terrain — the
    /// keeper margin, the hop budget, the rooms withheld from passage, the
    /// rooms a rival owns (`rivalRooms`) and the rooms the world holds, since
    /// `linkedRecalling` enters nothing outside them. Past 4,096 rows the table is emptied rather than grown: a key
    /// minted per change of the world's rooms has nothing else to evict it.
    let hopsUnder (gate: StandDown) (joins: JoinTable) (tuning: Tuning) (world: World) =
        let reaches = reachesUnder gate joins tuning world

        let stamp =
            String.concat
                "|"
                [
                    string (Tuning.keeperMargin tuning)
                    string tuning.MaxHops
                    String.concat "," gate.Impassable
                    String.concat "," (rivalRooms world)
                    String.concat "," (Map.keys world.Rooms)
                ]

        fun (home: string) (room: string) ->
            let key = stamp + "|" + home + "|" + room

            if joins.Hops.ContainsKey key then
                match joins.Hops.[key] with
                | hops when hops < 0 -> None
                | hops -> Some hops
            else
                if joins.Hops.Count >= 4096 then
                    joins.Hops.Clear()

                let hops = Declaration.hops reaches tuning.MaxHops home room
                joins.Hops.[key] <- Option.defaultValue -1 hops
                hops

    /// The colony whose home bank holds the most, ties by home name.
    let private largestBank (world: World) (colonies: Colony list) : string option =
        colonies
        |> List.sortBy (fun colony -> -(roomOf world colony.Home).Energy.Capacity, colony.Home)
        |> List.tryHead
        |> Option.map (fun colony -> colony.Home)

    /// ADR-0081
    /// The colony that casts each [[harassment room]] this tick: among the
    /// living colonies whose bank buys the room's floor (the blocks it is
    /// declared at, `Harass.blocks`, #457) and whose chain
    /// reaches the room inside the hop budget, the one fewest crossings from
    /// it, then the one whose bank holds the most, then by home name (#437);
    /// None while no colony is both. Stateless, and decided once for every
    /// colony, so exactly one projects the room. Asked over the open gate: a
    /// caster's own stand-down withdraws its work and hands the room to
    /// nobody else. A room we own is out of the list, cast and refused by
    /// nobody (#447): the day a Claim lands in a harassment room, its
    /// mother's garrison holds it.
    let harassCasters
        (joins: JoinTable)
        (tuning: Tuning)
        (colonies: Colony list)
        (harass: Harassment)
        (world: World)
        : HarassCasting =
        let hopsOf = hopsUnder StandDown.none joins tuning world
        let living = living colonies world
        let owned = ownedRooms world

        let rooms =
            harass.Rooms |> List.filter (fun h -> not (Set.contains h.RoomName owned))

        let floors =
            rooms |> List.map (fun h -> h.RoomName, Harass.blocks tuning h) |> Map.ofList

        {
            Casters =
                rooms
                |> List.map (fun h ->
                    let floor = floors.[h.RoomName] * harass.BlockCost

                    h,
                    living
                    |> List.filter (fun colony ->
                        (roomOf world colony.Home).Energy.Capacity >= floor)
                    |> List.choose (fun colony ->
                        hopsOf colony.Home h.RoomName
                        |> Option.map (fun hops ->
                            (hops, -(roomOf world colony.Home).Energy.Capacity, colony.Home)))
                    |> List.sort
                    |> List.tryHead
                    |> Option.map (fun (_, _, home) -> home))
            Floors = floors
        }

    /// The living colony that says a [[harassment room]] no colony casts out
    /// loud (`ColonyView.Refused`): the largest bank of all, ties by home
    /// name, so a room no colony can cast is still named once.
    let harassReporter (colonies: Colony list) (world: World) : string option =
        living colonies world |> largestBank world

    /// `linkedRecalling` over a table of this call's own — the shape a test
    /// asks in. The shell never calls this.
    let linkedBy (keeperMargin: int) (world: World) : string -> string -> bool =
        linkedRecalling (JoinTable()) keeperMargin world

    /// What one colony's declaration narrows to this tick, and the union of
    /// it, in named halves rather than a positional tuple (three are
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
            /// The [[salvage]] rooms left after the same two an errand takes.
            Salvage: string list
            /// The rooms this colony projects for a child of its own, raised
            /// or re-claimed (#221).
            Borrowed: string list
            /// The children's homes this colony defends this tick
            /// (`Colony.defending`).
            Defended: string list
            /// The [[harassment room]]s this colony casts (`harassCasters`),
            /// less the ones its [[stand-down]] shuts.
            Harass: Harass list
            /// Every harassment room this colony casts, the shut ones
            /// included.
            Cast: Harass list
            /// The harassment rooms no colony casts this tick
            /// (`harassCasters`).
            Uncast: Harass list
            /// The scan set: this colony's home and all six of those, the
            /// one place that union is spelled. A cast harassment room its
            /// stand-down shuts stays in it, as a transit room: a ranger
            /// standing there is still placed, and walks home.
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
    /// A [[salvage]] room is narrowed the same two ways.
    ///
    /// A [[harassment room]] is the one declaration read off the global list
    /// and not the colony's: kept by the colony that casts it.
    ///
    /// The whole `Tuning` and not the hop budget alone: the chain is searched
    /// over the **masked** border rings (`Tuning.keeperMargin`).
    let scanRecalling
        (joins: JoinTable)
        (tuning: Tuning)
        (stages: Map<string, ColonyStage>)
        (unowned: Set<string>)
        (colonies: Colony list)
        (casting: HarassCasting)
        // The gate whole and not its `Shut` set alone (#382): the scan set
        // asks which rooms are withheld from work, and which of those cannot
        // be crossed either.
        (gate: StandDown)
        (world: World)
        (colony: Colony)
        : ScanSet =
        // Every declaration's chain off the one recalled count (`hopsUnder`):
        // routable is a chain inside the budget both ways.
        let hopsOf = hopsUnder gate joins tuning world

        let routable room =
            hopsOf colony.Home room |> Option.isSome

        let cast =
            casting.Casters
            |> List.filter (fun (h, caster) -> caster = Some colony.Home && routable h.RoomName)
            |> List.map fst

        let outposts =
            Outpost.worked gate.Shut colony.Outposts
            |> List.filter (fun outpost -> routable outpost.RoomName)

        let errands =
            Errand.worked gate.Shut colony.Errands
            |> List.filter (fun errand -> routable errand.RoomName)

        let salvage = Salvage.worked gate.Shut colony.Salvage |> List.filter routable

        // The two halves of what a mother projects for a child of hers,
        // disjoint by construction: a room she is raising is one we own, and
        // a room she may take back is one we do not (#221).
        let borrowed =
            Colony.bootstrapping stages colonies colony
            @ Colony.reclaiming unowned colonies colony

        let covered = Colony.roomsProjected outposts errands salvage borrowed [] colony.Home

        let defended = Colony.defending (defends world covered colony.Home) colonies colony

        {
            Outposts = outposts
            Errands = errands
            Salvage = salvage
            Borrowed = borrowed
            Defended = defended
            Harass = Harass.worked gate.Shut cast
            Cast = cast
            Uncast = casting.Casters |> List.filter (snd >> Option.isNone) |> List.map fst
            Scanned =
                Colony.roomsProjected outposts errands salvage borrowed defended colony.Home
                @ Harass.roomsProjected cast colony.Home
                |> List.distinct
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
        scanRecalling
            (JoinTable())
            tuning
            stages
            unowned
            colonies
            HarassCasting.none
            gate
            world
            colony

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
        (casting: HarassCasting)
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
                casting
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
        (casting: HarassCasting)
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
                    casting
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

        Colony.creepColonies
            projections
            (spawnHomes world)
            (world.Creeps |> List.map (fun creep -> creep.Info.Name, Some creep.Room))

    /// `creepColoniesRecalling` over a table of this call's own (`linkedBy`).
    let creepColonies
        (tuning: Tuning)
        (colonies: Colony list)
        (running: Colony list)
        (shut: Map<string, Set<string>>)
        (world: World)
        : Map<string, string> =
        creepColoniesRecalling (JoinTable()) tuning colonies HarassCasting.none running shut world
