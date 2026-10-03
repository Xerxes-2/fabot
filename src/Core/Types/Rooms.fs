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
    /// because the Layout holds a footing for one but never places it.
    | Link
    /// A rampart, the walkable defence over the Keep and the Posts.
    /// Walkability answers for it before anything else does: a creep may
    /// stand on a rampart, and folding it into Other would make every kind
    /// the decision layer does not model walkable with it.
    | Rampart
    /// The extractor over a Thorium mineral. One per room ever, standing on
    /// the mineral's own tile, and a modelled kind rather than Other for one
    /// reason: the Layout has to see the one already standing — or the site
    /// going up — before it asks for another. It feeds nothing, stores
    /// nothing, decays into nothing and is never repaired, so every predicate
    /// over the vocabulary answers no for it.
    | Extractor
    /// The terminal (#349). One per room from RCL6, and the only structure in
    /// the vocabulary whose reason for existing is a room it is **not** in: a
    /// `send` between two terminals of the same owner is unrestricted by
    /// `mod-season5/src/terminal-restriction.js`, which nulls a send only when
    /// the target terminal belongs to somebody else. Modelled rather than Other
    /// for the Extractor's reason. It holds a store, and no rule reads that
    /// store yet; a kind the Refill rules do not name is a kind no hauler
    /// fills, which is what keeps this slice inert until #349's send rule lands.
    | Terminal
    /// The observer (#484): one per room at RCL8, whose `observeRoom` lends
    /// vision of one room within `Engine.observerRange` for the next tick.
    | Observer
    /// Any structure kind the decision layer has no rules for yet.
    | Other

/// What the decision layer knows about one energy-hungry structure
/// (spawn, extension, or tower) this tick.
type RefillableInfo =
    {
        Id: string
        /// Energy the structure's store can still take (0 = full).
        FreeCapacity: int
        /// What kind of structure this is — the Refill rank layer's key. To a
        /// creep both are the same transfer.
        Kind: BuiltKind
    }

/// ADR-0054
/// The colony's [[refill cluster]]: the colony's spawn and every extension
/// of it, read as **one** Refill target. **The spawn is the key**: the
/// member every cluster has, and the door the [[hauler unit]] quota
/// already prices its leg to.
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
    /// stands among them to key one. Membership is by kind and not by
    /// geometry: every spawn-feeding structure is in the one cluster however
    /// far the [[layout]] put it.
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
    /// all and what its [[capacity]] divides into holders.
    let free (cluster: RefillCluster) =
        cluster.Members |> Map.fold (fun total _ room -> total + room) 0

    /// The members with room left, in id order — the structures the Work Area
    /// is laid over and the only ones the Emitter may transfer into.
    let hungry (cluster: RefillCluster) =
        cluster.Members
        |> Map.toList
        |> List.choose (fun (id, room) -> if room > 0 then Some id else None)

/// What the decision layer knows about one energy source this tick.
type SourceInfo =
    {
        Id: string
        /// Ticks until the source holds energy again — its restock; 0 while it
        /// holds energy now. Not the amount: the one time fact a decision reads
        /// about a source.
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
        /// The first tick `activateSafeMode` is no longer refused on the
        /// controller's `safeModeCooldown` (#474); at or before now while
        /// none runs.
        SafeModeCooldownUntil: int
    }

/// Whose CLAIM parts hold one room's reservation, as the colony reads it:
/// three answers and not a username, the same closed shape as `Ownership`
/// below. The third is load-bearing and not a refinement of the second:
/// an NPC invader's reservation and another *player's* are each a
/// **clock** a [[stand-down]] runs to that the other's rule would read
/// wrong. What carries no clock at all is a player's *ownership*, which is
/// `Ownership`'s answer and not this one's.
[<RequireQualifiedAccess>]
type ReservationHolder =
    /// This colony's own CLAIM parts. The one answer that doubles the
    /// room's sources and the one the reserver row sizes itself from.
    | Ours
    /// The NPC Invader — the holder of the reservation a level-0 core takes
    /// with `attackController` in a room it expanded into
    /// (docs/research/remote-mining.md §8.4). Worth the neutral rate like any
    /// hold that is not ours, and, unlike a rival's, an expiry: this lapses.
    | Invader
    /// Another player. Worth the neutral rate, and a [[stand-down]] that
    /// runs to the end of the hold itself. Its permanent half is
    /// `Ownership.Rival`.
    | Rival

/// The reservation standing on one room's controller this tick: a neutral
/// controller held by CLAIM parts, which doubles every source in that
/// room, decays by one a tick and caps at 5,000.
type ReservationInfo =
    {
        /// Whose CLAIM parts hold it — which of the three, never which string:
        /// the engine answers holding with a username, and both names that would
        /// have to be compared are the shell's to know.
        Holder: ReservationHolder
        /// Ticks left on the reservation — what the reserver row sizes and quotas
        /// from, `ceil((5000 - this) / 600)` CLAIM parts. Read as the colony's own
        /// hold only where `Holder` is `Ours`; under `Invader` and `Rival` it is
        /// the deadline the stand-down runs to.
        TicksToEnd: int
        /// The username the engine answers the hold with. Read by one rule,
        /// the [[harassment room]]'s (#432): a room reserved by anyone but the
        /// declared enemy yields no container to take down, and `Holder`
        /// cannot tell an ally's claimer from the enemy's.
        Username: string
    }

/// Whose a thing is, as the colony reads it: three answers and not a username.
/// A closed vocabulary rather than a pair of booleans, because "ours" and
/// "somebody else's" answer one question, and the two names that would have to
/// be compared are the shell's to know. "We cannot see" is the absence of the
/// whole entry.
///
/// **Two senses, one vocabulary.** A *room*'s, read off its controller
/// (`RoomControlInfo.Owner`), and since #318 one *object*'s
/// (`SpatialInfo.Owners`), which is what a sector centre needs, there being no
/// controller in that room to read a room answer off at all. The clauses below
/// are written in the room sense; the object sense reads them one scale down,
/// and `Ownership` is deliberately not split into two types.
[<RequireQualifiedAccess>]
type Ownership =
    /// Nobody owns the controller — the shape every neutral room and every
    /// outpost the colony works arrives in, and the shape a room with no
    /// controller at all is projected as. Reservable, and worth half until it
    /// is reserved.
    | Unowned
    /// This colony owns it. Worth the held ten a tick, and never reserved — the
    /// engine refuses `reserveController` on a room anybody owns.
    | Ours
    /// Another player owns it. The engine yields ten a tick in a rival's room
    /// exactly as in ours, and the colony prices it at five all the same. No
    /// NPC case: an invader core *reserves* and never owns.
    | Rival

/// Who holds one room the colony can see this tick — the fact a source's
/// output is read from, ten energy a tick being the *held* rate and a
/// neutral room's source yielding five. One entry per room vision answered
/// for; a room the colony cannot see has no entry, and that absence is not
/// "half" but unpriceable.
type RoomControlInfo =
    {
        /// Whose the room's controller is. Read *beside* the reservation and
        /// never instead of it: the engine gives a room with an owner the same
        /// 3,000 a cycle it gives a reserved one, so "reserved, or half" would
        /// price the colony's own sources at five.
        Owner: Ownership
        /// The reservation standing on the room's controller; None where nothing
        /// reserves it.
        Reservation: ReservationInfo option
        /// Whether the room's controller is under safe mode this tick.
        /// Carried per room and not on the colony's own controller alone:
        /// safe mode shields the room it is in, whoever is looking, and only
        /// a room *we* own shields us. False where no controller stands.
        SafeMode: bool
        /// `ControllerInfo.SafeModeCooldownUntil`, read off any controller and
        /// not ours alone: an unclaim leaves the cooldown standing on a room
        /// nobody owns, and a claim landing through it banks no safe mode
        /// before it ends (#474). 0 where no controller stands.
        SafeModeCooldownUntil: int
        /// The text standing on this controller, and **None for a controller
        /// nobody has signed**. A sign is written by any creep adjacent to the
        /// controller and lasts until somebody overwrites it, so it is the one
        /// fact about a room that outlives every body that made it — which is why
        /// four of ours carried another player's flavour text for hundreds of
        /// thousands of ticks before anybody read the field.
        ///
        /// The text alone and not who wrote it: what the rule asks is whether
        /// what stands there is what this colony means to say, and a rival who
        /// copies our words has said it for us.
        Sign: string option
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RoomControlInfo =
    /// The reservation this room carries **if it is that holder's**, and None
    /// otherwise — an unheld room and one held by somebody else read alike.
    let heldBy (holder: ReservationHolder) (control: RoomControlInfo) : ReservationInfo option =
        control.Reservation |> Option.filter (fun held -> held.Holder = holder)

    /// The reservation this room carries **if it is somebody else's** — the
    /// Invader's or another player's alike — and None where nothing reserves
    /// it or we do. The one question the *engine* asks before it answers
    /// `reserveController` or `createConstructionSite`: both are refused on a
    /// controller anybody but us holds, and neither cares which of the two it
    /// is (#333). Deliberately not `heldBy` twice over: the rules above derive
    /// **clocks**, which the two holds disagree about, and this one derives a
    /// **refusal**, which they do not. `Observe.foldRaids` records it in
    /// `RaidState.Holds`; what the reserver row reads off it is
    /// `refusesReserver`'s.
    let heldByOther (control: RoomControlInfo) : ReservationInfo option =
        control.Reservation
        |> Option.filter (fun held -> held.Holder <> ReservationHolder.Ours)

    /// Whether a hold of `heldByOther`'s refuses the reserver row its
    /// controller (#487): another player's, which is a stand-down of its own.
    /// The Invader's does not: `attackController` takes a tick per CLAIM part
    /// off it. One predicate for the vision read (`reservableControllers`) and
    /// the blind one (`StandDown.HeldOutposts`), or the two could disagree.
    let refusesReserver (holder: ReservationHolder) : bool = holder = ReservationHolder.Rival

/// Another player's controller as vision last showed it (#489): what decides
/// whether its owner can raise safe mode there. Every clock is the absolute
/// tick it runs to, so a sighting stays true in the dark; the stock and the
/// downgrade timer are as seen at `Seen`, which is why they go stale.
type RivalController =
    {
        /// The engine's username for the controller's owner.
        Owner: string
        Level: int
        /// The first tick its safe mode no longer runs; at or before `Seen`
        /// while none does.
        SafeModeUntil: int
        /// The first tick `safeModeCooldown` no longer refuses an activation.
        SafeModeCooldownUntil: int
        /// Activations banked.
        SafeModeAvailable: int
        /// The first tick `upgradeBlocked` no longer refuses one: a CLAIM
        /// tap's 1,000 ticks.
        UpgradeBlockedUntil: int
        /// The downgrade timer as seen.
        TicksToDowngrade: int
        /// The tick vision showed it.
        Seen: int
    }

/// Why `activateSafeMode` would be refused, as far as a sighting can say.
[<RequireQualifiedAccess>]
type SafeModeRefusal =
    /// Safe mode runs in this room or another of the owner's: one per
    /// player at a time (ERR_BUSY).
    | Busy of room: string
    | Cooldown
    | NoStock
    | UpgradeBlocked
    /// The downgrade timer is below `Engine.safeModeDowngradeLine`.
    | Downgrading

/// Whether a rival can raise safe mode in one room now.
[<RequireQualifiedAccess>]
type SafeModeActivation =
    | Can
    | Cannot of SafeModeRefusal
    /// The room unseen, or seen too long ago to rule it out.
    | Unknown

[<RequireQualifiedAccess>]
module RivalSafeMode =
    /// Whether `player` can activate safe mode in `room` at `now`, over every
    /// controller of theirs remembered (`World.RivalControllers`). A refusal
    /// off a clock holds however old the sighting, since none ends early; the
    /// stock and the downgrade line are trusted for `staleAfter` ticks, and
    /// past that a room no clock refuses is Unknown. A room not `player`'s as
    /// last seen is Unknown too.
    let canActivate
        (staleAfter: int)
        (known: Map<string, RivalController>)
        (player: string)
        (room: string)
        (now: int)
        : SafeModeActivation =
        match Map.tryFind room known |> Option.filter (fun c -> c.Owner = player) with
        | None -> SafeModeActivation.Unknown
        | Some c ->
            let busy =
                if c.SafeModeUntil > now then
                    Some room
                else
                    known
                    |> Map.tryFindKey (fun _ other ->
                        other.Owner = player && other.SafeModeUntil > now)

            match busy with
            | Some running -> SafeModeActivation.Cannot(SafeModeRefusal.Busy running)
            | None when c.SafeModeCooldownUntil > now ->
                SafeModeActivation.Cannot SafeModeRefusal.Cooldown
            | None when c.UpgradeBlockedUntil > now ->
                SafeModeActivation.Cannot SafeModeRefusal.UpgradeBlocked
            | None when now - c.Seen >= staleAfter -> SafeModeActivation.Unknown
            | None when c.SafeModeAvailable <= 0 ->
                SafeModeActivation.Cannot SafeModeRefusal.NoStock
            | None when c.TicksToDowngrade < Engine.safeModeDowngradeLine c.Level ->
                SafeModeActivation.Cannot SafeModeRefusal.Downgrading
            | None -> SafeModeActivation.Can
