/// The explanation channel: why a creep was matched, released, rejected or
/// left idle, the arbitrated `Movement` behind each step, and the quota row
/// the census is read against — each with its wire name.
[<AutoOpen>]
module Fabot.Core.Types.Verdicts

/// The reverse of a wire-name table, derived from the table itself: each
/// spelling is written once, in the name table, and the decoder reads back what
/// falls out of it. A name the vocabulary does not have reads as None — the
/// caller decides what a miss costs.
let reverseOf toName cases =
    let byName = cases |> List.map (fun case -> toName case, case) |> Map.ofList
    fun name -> Map.tryFind name byName

/// The same reversal for a vocabulary whose cases carry numbers beside their
/// name. The entries are the cases' own constructors rather than the cases, so
/// each spelling is still written once, and the numbers the wire carried are
/// handed back in on the way out. A name whose numbers are missing decodes to
/// None exactly as an unknown name does.
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

/// The wire spelling of each MatchFactor — the one place the spelling lives,
/// beside the union it spells.
let matchFactorName =
    function
    | MatchFactor.OnlyCandidate -> "only-candidate"
    | MatchFactor.Rank -> "rank"
    | MatchFactor.TravelCost -> "travel-cost"
    | MatchFactor.Load -> "load"
    | MatchFactor.PoolOrder -> "pool-order"

/// The MatchFactor a wire name spells, or None. The case list is a literal;
/// `Core.Tests` round-trips the union itself and fails on a missing case.
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

/// Why a Task in the pool was rejected for a creep, in a verbose scoring — the
/// matching gates, in the order they are tried.
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

/// Why a remembered assignment was released: its Task left the pool, or one of
/// the matching gates refused it for this creep (`Matcher.gate` answers one
/// cascade for both readings). `TaskGone` is answered above the cascade.
[<RequireQualifiedAccess>]
type ReleaseReason =
    | TaskGone
    | Rejected of RejectReason

/// The wire spelling of each ReleaseReason: the refusals spell what they
/// spelt as refusals, so the release and scoring channels name one failure
/// one way.
let releaseReasonName =
    function
    | ReleaseReason.TaskGone -> "task-gone"
    | ReleaseReason.Rejected reason -> rejectReasonName reason

/// The numbers a ReleaseReason carries beside its wire name, or None for a
/// bare tag — the carried reason's own, for the same reason the name is.
let releaseReasonNumbers =
    function
    | ReleaseReason.TaskGone -> None
    | ReleaseReason.Rejected reason -> rejectReasonNumbers reason

/// The ReleaseReason a wire name spells for the numbers the wire carried
/// beside it, or None — including `too-early` with no numbers.
let releaseReasonOf payload name =
    if name = releaseReasonName ReleaseReason.TaskGone then
        Some ReleaseReason.TaskGone
    else
        rejectReasonOf payload name |> Option.map ReleaseReason.Rejected

/// Why an unassigned creep got nothing: the last gate every Task failed at.
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

/// The wire spelling of each FootingKind, on the Layout channel's Memory leaf.
/// Not a Verdict vocabulary, but the same rule.
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

/// The wire spelling of each DeclarationKind, on the Layout channel's Memory
/// leaf beside `footingKindName`. The refusal's own row carries it, because
/// the operator's next act — which list to move the declaration in — turns
/// on it.
let declarationKindName =
    function
    | DeclarationKind.Outpost -> "outpost"
    | DeclarationKind.Errand -> "errand"

/// The DeclarationKind a wire name spells, or None for a name this vocabulary
/// does not have.
let declarationKindOf =
    reverseOf declarationKindName [ DeclarationKind.Outpost; DeclarationKind.Errand ]

/// The wire spelling of each TrunkGoal, on the Layout channel's Memory leaf.
/// A carrying vocabulary: the spawn's id rides beside the name.
let trunkGoalName =
    function
    | TrunkGoal.UpgradeArea -> "upgrade-area"
    | TrunkGoal.Spawn _ -> "spawn"

/// The spawn a TrunkGoal names beside its wire name, or None.
let trunkGoalSpawn =
    function
    | TrunkGoal.Spawn spawn -> Some spawn
    | TrunkGoal.UpgradeArea -> None

/// The TrunkGoal a wire name spells for the spawn the wire carried beside it,
/// or None — including `spawn` with no id. An empty id is a spawn like any
/// other here; what counts as a usable id is the caller's.
let trunkGoalOf =
    reverseCarrying
        trunkGoalName
        ""
        [ (fun _ -> Some TrunkGoal.UpgradeArea); Option.map TrunkGoal.Spawn ]

/// The wire spelling of each ContainerTarget, on the Layout channel's Memory
/// leaf beside `trunkGoalName`. A carrying vocabulary like it: the source's
/// id rides beside the name rather than inside it.
let containerTargetName =
    function
    | ContainerTarget.Source _ -> "source"
    | ContainerTarget.Controller -> "controller"
    | ContainerTarget.Mineral _ -> "mineral"

/// The source a ContainerTarget names beside its wire name, or None for the
/// controller.
let containerTargetSource =
    function
    | ContainerTarget.Source source -> Some source
    | ContainerTarget.Controller -> None
    | ContainerTarget.Mineral mineral -> Some mineral

/// The ContainerTarget a wire name spells for the source the wire carried
/// beside it, or None — including `source` with no id.
let containerTargetOf =
    reverseCarrying
        containerTargetName
        ""
        [
            Option.map ContainerTarget.Source
            (fun _ -> Some ContainerTarget.Controller)
            Option.map ContainerTarget.Mineral
        ]

/// The wire spelling of each StandDownBasis, on the Raid log's Memory leaf,
/// round-tripped against the union itself by `Core.Tests`.
let standDownBasisName =
    function
    | StandDownBasis.CollapseTimer -> "collapse-timer"
    | StandDownBasis.Reservation -> "reservation"
    | StandDownBasis.Fallback -> "fallback"
    | StandDownBasis.RivalReservation -> "rival-reservation"
    | StandDownBasis.InvaderRaid -> "invader-raid"

/// The wire spelling of each ReservationHolder, on the Raid log's Memory leaf.
/// Only two of the three are ever written — the leaf records the rooms whose
/// controller somebody else holds — but the name is spelt for all three.
let reservationHolderName =
    function
    | ReservationHolder.Ours -> "ours"
    | ReservationHolder.Invader -> "invader"
    | ReservationHolder.Rival -> "rival"

/// The ReservationHolder a wire name spells, or None: the shell drops that
/// entry rather than naming the wrong player.
let reservationHolderOf =
    reverseOf
        reservationHolderName
        [ ReservationHolder.Ours; ReservationHolder.Invader; ReservationHolder.Rival ]

/// The StandDownBasis a wire name spells, or None: the shell drops that row
/// rather than inventing a reason for it.
let standDownBasisOf =
    reverseOf
        standDownBasisName
        [
            StandDownBasis.CollapseTimer
            StandDownBasis.Reservation
            StandDownBasis.Fallback
            StandDownBasis.RivalReservation
            StandDownBasis.InvaderRaid
        ]

/// A creep's Move Intent: candidate standing tiles for next tick in preference
/// order, plus a priority (the task rank). Input to the Resolver — not an
/// Intent; the Resolver's output is what becomes one. Standing tiles but one: a
/// creep walked at a Seam is given the exit tile itself as its last step, and
/// that is a destination rather than a place to stand — the engine moves a
/// creep off a border tile at the end of the tick — which is why no Seat, Work
/// Area or standing candidate query will ever name one. The tiles carry the
/// room they were registered in: a room's arbitration folds the intents of
/// every colony working that room.
type MoveIntent =
    {
        Creep: string
        Pos: RoomPos
        Rank: int
        Candidates: RoomPos list
        /// The tiles this body counts as still working from: its Work Area
        /// less this tick's Reach when it is standing inside that area, and
        /// empty for every body that is not (#267). It tells the arbitration
        /// a shuffle (moved within the set: the chain pays nothing) from an
        /// eviction (moved out of it: the chain pays the rank's weight and the
        /// sidestep), and that a body already shuffled inside it is finished
        /// business. The candidate list holds the same tiles in preference
        /// order and cannot say which are still the Work Area.
        Area: Set<RoomPos>
    }

/// One colony's movement for the tick, before a tile of it is arbitrated. It
/// exists because a room's movement is not one colony's decision: two colonies
/// work one room whenever a mother colony is raising a child, and a `decide`
/// arbitrating its own half against the other half's tiles read as empty
/// claimed the tile the child's anchor stood on, every tick.
type Movement =
    {
        /// This colony's creeps in view order — the order its move Intents
        /// and its movement Verdicts leave in.
        Order: string list
        /// Where the projection places each of this colony's bodies
        /// (`Atlas.placedCreeps`). A creep the projection cannot place is
        /// in no room's pass.
        Placed: (string * RoomPos) list
        /// The creeps fatigue takes out of arbitration this tick. Their tiles
        /// are the pass's walls.
        Tired: Set<string>
        /// The tiles held by bodies this colony does not hold, each carrying
        /// its room.
        Foreign: Set<RoomPos>
        /// Each rested creep's Move Intent, each a tile of the room the
        /// creep stands in.
        Intents: MoveIntent list
        /// The verbose-list creeps whose priced step differs from their
        /// traffic-blind one — the one movement Verdict that needs this
        /// colony's Atlas, so it is settled here and carried.
        Rerouted: Set<string>
    }

/// One row of a verbose scoring: a Task in the pool, either scored on the
/// full matching key — rank tier, travel cost, current load — or rejected
/// at the first gate it failed. The answer to "why *not* that Task".
[<RequireQualifiedAccess>]
type Candidate =
    | Scored of task: string * rank: int * cost: int * load: int
    | Rejected of task: string * reason: RejectReason

/// ADR-0009. The reasoned outcome a decision step returns beside its decision
/// — data, never a log line.
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
    /// Rested, settled off the tile it asked for first — with nobody the pass
    /// can name holding that tile.
    | Stalled of creep: string

/// One casting row's count this tick: what the quota asks for, how many living
/// bodies the row reads back (`patternOf`), and how many are in the oven for
/// it. Observability only: what `observe.mjs quotas` prints so a human can see
/// why a spawn stands idle at a full bank.
type RowQuota =
    {
        Row: string
        Quota: int
        Living: int
        Casting: int
    }

/// The tick's workforce arithmetic as the cascade saw it: the whole
/// target, the living and casting counts it was measured against, and one
/// `RowQuota` per row. `Rows` empty and `Target` zero is the cascade's
/// silence — a spawn's doorstep inside a Reach — and the view says so.
type Quotas =
    {
        Target: int
        Living: int
        Casting: int
        Rows: RowQuota list
        /// One hauler load at this bank, the divisor of the haul sum.
        HaulerLoad: int
        /// The hauler quota's own arithmetic, per source container.
        HaulerDemand: HaulDemandRow list
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Quotas =
    let silent: Quotas =
        {
            Target = 0
            Living = 0
            Casting = 0
            Rows = []
            HaulerLoad = 0
            HaulerDemand = []
        }

/// What one tick of deciding returns: the Intents to execute, the
/// Assignments to remember for next tick, the plan memo to hold in heap
/// for next tick, the Verdicts explaining them, and this colony's move
/// intents before anybody arbitrated them.
type Decision =
    {
        Intents: Intent list
        Assignments: Assignments
        Memo: PlanMemo
        Verdicts: Verdict list
        /// This colony's unarbitrated movement. `decide` folds it through the
        /// one-colony pass itself, so `Intents` and `Verdicts` are a whole
        /// answer on their own; a shell running more than one colony hands
        /// every colony's here to `resolveRooms` instead.
        Movement: Movement
        /// The cascade's own reading of the workforce this tick, for the
        /// `quotas` view; nothing downstream reads it.
        Quotas: Quotas
    }
