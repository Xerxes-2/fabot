/// What the decision layer knows about the furniture of one room: spawns and
/// extensions, the sources and the controller, who holds the reservation, and
/// the energy standing in each store.
[<AutoOpen>]
module Fabot.Core.Types.Rooms

/// What the decision layer knows about one spawn this tick.
type SpawnInfo =
    {
        Name: string
        /// Game-object id of the spawn structure — the key that locates
        /// this spawn in the spatial projection's target maps.
        Id: string
        /// Name of the room the spawn stands in — the key into the
        /// world's per-room banks (`RoomFacts.Energy`).
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

/// What a built structure is — or what a construction site will become once
/// built. Projection vocabulary, distinct from the Intent vocabulary of
/// placeable kinds: every placeable kind widens into one of these
/// (`builtKindOfPlaceable`), never the other way.
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
    /// 0034). Walkability answers for it before anything else does: a creep
    /// may stand on a rampart, and folding it into Other would make every kind
    /// the decision layer does not model walkable with it.
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

/// The colony's [[refill cluster]] (ADR 0054): the colony's spawn and every
/// extension of it, read as **one** Refill target. One Task is one place a body
/// walks to once, and `task-gone` fires when the whole ring is full, where one
/// Task per structure had a loaded body lose its extension to whoever filled it
/// while it walked. **The spawn is the key**: the member every cluster has, and
/// the door the [[hauler unit]] quota already prices its leg to (ADR 0052
/// decision 4).
type RefillCluster =
    {
        /// The Task's target id: the cluster's spawn, and the id every
        /// Assignment, [[verdict]] and [[transition log]] line names this
        /// Refill by.
        Spawn: string
        /// Member id -> the energy it can still take this tick. Every member,
        /// full ones included, so the map is the ring's membership and not
        /// this tick's fill level; what the [[work area]] and the
        /// [[emitter]]'s pick read is the **hungry** subset (`hungry`).
        Members: Map<string, int>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RefillCluster =
    /// The cluster this colony's Refillables make, or None where no spawn
    /// stands among them to key one (ADR 0054). Membership is by kind and not
    /// by geometry: every spawn-feeding structure is in the one cluster however
    /// far the [[layout]] put it, and the walk is still short because the Work
    /// Area is the union of the members' rings.
    let ofRefillables (refillables: RefillableInfo list) : RefillCluster option =
        let members =
            refillables
            |> List.filter (fun r -> r.Kind = BuiltKind.Spawn || r.Kind = BuiltKind.Extension)

        let spawns =
            members
            |> List.filter (fun r -> r.Kind = BuiltKind.Spawn)
            |> List.map (fun r -> r.Id)

        match spawns with
        | [] -> None
        | spawns ->
            Some
                {
                    Spawn = List.min spawns
                    Members = members |> List.map (fun r -> r.Id, r.FreeCapacity) |> Map.ofList
                }

    /// The energy the whole cluster can still take: what pools the Task at
    /// all and what its [[capacity]] divides into holders (ADR 0054).
    let free (cluster: RefillCluster) =
        cluster.Members |> Map.fold (fun total _ room -> total + room) 0

    /// The members with room left, in id order — the structures the Work Area
    /// is laid over and the only ones the Emitter may transfer into (ADR
    /// 0054). A body stopped beside a full extension has nothing to pour,
    /// which is this Task shape's own churn re-entered through the geometry.
    let hungry (cluster: RefillCluster) =
        cluster.Members
        |> Map.toList
        |> List.choose (fun (id, room) -> if room > 0 then Some id else None)

/// What the decision layer knows about one energy source this tick.
type SourceInfo =
    {
        Id: string
        /// Ticks until the source holds energy again — its restock (ADR 0013,
        /// widened by ADR 0025); 0 while it holds energy now. Not the amount:
        /// the one time fact a decision reads about a source, so a drained
        /// source's Harvest is judged at the creep's arrival.
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
/// three answers and not a username, the same closed shape as `Ownership`
/// below. The third is load-bearing and not a refinement of the second — ADR
/// 0043 gives an NPC invader's reservation and another *player's* different
/// meanings, and each is a **clock** a [[stand-down]] runs to that the other's
/// rule would read wrong: the Invader's is the core's deadline and never
/// answers earlier than the fallback, because the core re-takes the hold it
/// lets lapse (#136); a player's is the hold itself, read literally and with no
/// floor, because a claimer that stops coming re-takes nothing (#165). What
/// carries no clock at all is a player's *ownership*, which is `Ownership`'s
/// answer and not this one's.
[<RequireQualifiedAccess>]
type ReservationHolder =
    /// This colony's own CLAIM parts. The one answer that doubles the
    /// room's sources and the one the reserver row sizes itself from.
    | Ours
    /// The NPC Invader — the holder of the reservation a level-0 core takes
    /// with `attackController` in a room it expanded into (ADR 0043,
    /// docs/research/remote-mining.md §8.4). Worth the neutral rate like any
    /// hold that is not ours, and, unlike a rival's, an expiry: this lapses.
    | Invader
    /// Another player. Worth the neutral rate, and a [[stand-down]] that
    /// runs to the end of the hold itself (#165) — the room is being worked
    /// by somebody else for exactly as long as the engine says it is. The
    /// abandonment trigger every mature bot implements; its permanent half
    /// is `Ownership.Rival`.
    | Rival

/// The reservation standing on one room's controller this tick (ADR
/// 0042): a neutral controller held by CLAIM parts, which doubles every
/// source in that room, decays by one a tick and caps at 5,000.
type ReservationInfo =
    {
        /// Whose CLAIM parts hold it — which of the three, never which
        /// string: the engine answers holding with a username, and both names
        /// that would have to be compared are the shell's to know. A
        /// reservation somebody else holds reads for *pricing* exactly as no
        /// reservation does, which is a colony decision and not the engine's
        /// arithmetic: the colony prices it at five because a room somebody
        /// else holds has stopped being ours to work (ADR 0043).
        Holder: ReservationHolder
        /// Ticks left on the reservation — what the reserver row sizes and
        /// quotas from, `ceil((5000 - this) / 600)` CLAIM parts (ADR 0042).
        /// Read as the colony's own hold only where `Holder` is `Ours`; under
        /// `Invader` it is the deadline ADR 0043 falls back to, and under
        /// `Rival` the one the stand-down #165 clocks runs to.
        TicksToEnd: int
    }

/// Whose a room's controller is, as the colony reads it: three answers and not
/// a username. Two of them are what ADR 0042 prices a source from — ours is
/// the held rate, nobody's is half — and the third is what ADR 0043's
/// clockless withdrawal is judged on, the one trigger the engine gives no end
/// for (#165). A closed vocabulary rather than a pair of booleans, because
/// "ours" and "somebody else's" answer one question. "We cannot see" is the
/// absence of the whole entry (ADR 0004).
[<RequireQualifiedAccess>]
type Ownership =
    /// Nobody owns the controller — the shape every neutral room and every
    /// outpost the colony works arrives in, and the shape a room with no
    /// controller at all is projected as. Reservable, and worth half until it
    /// is reserved.
    | Unowned
    /// This colony owns it: the spawn room, and nothing else while there
    /// is one colony. Worth the held ten a tick, and never reserved — the
    /// engine refuses `reserveController` on a room anybody owns.
    | Ours
    /// Another player owns it. The engine yields ten a tick in a rival's room
    /// exactly as in ours, and the colony prices it at five all the same, for
    /// the reason `ReservationInfo.Holder` gives (ADR 0043). No NPC case: an
    /// invader core *reserves* and never owns.
    | Rival

/// Who holds one room the colony can see this tick — the fact a source's
/// output is read from (ADR 0042), ten energy a tick being the *held* rate and
/// a neutral room's source yielding five. One entry per room vision answered
/// for; a room the colony cannot see has no entry, and that absence is not
/// "half" but unpriceable (ADR 0004).
type RoomControlInfo =
    {
        /// Whose the room's controller is. Read *beside* the reservation and
        /// never instead of it: the engine gives a room with an owner the same
        /// 3,000 a cycle it gives a reserved one, so "reserved, or half" would
        /// price the colony's own sources at five.
        Owner: Ownership
        /// The reservation standing on the room's controller; None where
        /// nothing reserves it. *Which* rival holds it is deliberately not
        /// carried, no rule reading a rival's name. What the pair does carry is
        /// every question ADR 0043 asks: whether somebody else holds this room,
        /// and whether the holder is the NPC whose reservation is a clock.
        Reservation: ReservationInfo option
        /// Whether the room's controller is under safe mode this tick.
        /// Carried per room and not on the colony's own controller alone:
        /// safe mode shields the room it is in, whoever is looking, and only
        /// a room *we* own shields us. False where no controller stands.
        SafeMode: bool
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RoomControlInfo =
    /// The reservation this room carries **if it is that holder's**, and None
    /// otherwise — an unheld room and one held by somebody else read alike
    /// (ADR 0004). Four rules ask it of four different holders and each used to
    /// spell the same bind-and-filter for itself; what each does with the
    /// answer stays its own, because the deadlines they derive are genuinely
    /// different clocks.
    let heldBy (holder: ReservationHolder) (control: RoomControlInfo) : ReservationInfo option =
        control.Reservation |> Option.filter (fun held -> held.Holder = holder)
