/// The observe channel's pure folds: the Transition log's, keyed by creep;
/// the Raid log's, colony-level and episodic; the CPU line's, one row per
/// tick; and the breach log's, one row per live invariant violation folded
/// with its age.
module Fabot.Core.Observe

open Fabot.Core.Types
open Fabot.Core.Decide.Bodies

/// One recorded change in a creep's timeline: what happened and when.
type ObserveEntry = { Tick: int; Verdict: Verdict }

/// One creep's slice of the observe state.
type CreepLog =
    {
        /// The Transition log proper: oldest first, capped per creep.
        Entries: ObserveEntry list
        /// The task-channel Verdict the Matcher last spoke on this creep — the
        /// steady state new task Verdicts are judged against. A cursor beside
        /// the ring and not its tail: the cap can evict the entry that opened
        /// the steady state, and an unchanged Kept must stay quiet regardless.
        LastTask: Verdict option
        /// The Scoring Verdict the tick before, if any — the verbose channel's
        /// own cursor. Scorings are episodic like movement, so this resets
        /// every tick and flipping verbose back on always records a fresh
        /// scoring: the investigator's confirmation the flip took effect.
        LastScoring: Verdict option
        /// The movement Verdicts the Resolver emitted for this creep last tick.
        LastMove: Verdict list
    }

/// Creep name -> that creep's timeline. The whole persisted observe state.
type ObserveState = Map<string, CreepLog>

/// The per-creep ring cap: conclusion-level entries at this cap across a
/// small colony stay well under the 2MB Memory (spec sanity: ~20 × ~20).
let capPerCreep = 60

let private creepOf =
    function
    | Verdict.Matched(creep, _, _)
    | Verdict.Kept(creep, _)
    | Verdict.Released(creep, _, _)
    | Verdict.Unassigned(creep, _)
    | Verdict.Scoring(creep, _)
    | Verdict.Grounded creep
    | Verdict.Yielded(creep, _)
    | Verdict.Rerouted creep
    | Verdict.Stalled creep
    | Verdict.Refused(creep, _, _, _) -> creep

let private isMovement =
    function
    | Verdict.Grounded _
    | Verdict.Yielded _
    | Verdict.Rerouted _
    | Verdict.Stalled _ -> true
    | _ -> false

let private isScoring =
    function
    | Verdict.Scoring _ -> true
    | _ -> false

/// Whether two Verdicts say the same thing, so the newer appends nothing.
/// Matched and Kept holding the same Task are one substance — Kept is the
/// anti-thrash steady state of the match already on record — and everything
/// else must match exactly.
let private sameSubstance a b =
    match a, b with
    | (Verdict.Matched(_, taskA, _) | Verdict.Kept(_, taskA)),
      (Verdict.Matched(_, taskB, _) | Verdict.Kept(_, taskB)) -> taskA = taskB
    | _ -> a = b

let private trim cap entries =
    let overflow = List.length entries - cap
    if overflow > 0 then List.skip overflow entries else entries

/// A refusal's entry (#629): one a streak, its count grown in place while
/// it is the creep's newest entry off the movement and scoring channels and
/// names the same Task and code, so a body refused on every tick costs its
/// ring one row and not the ring. It moves no cursor: which Task the body
/// holds is the Matcher's to say.
let private refuse cap tick (verdict: Verdict) (log: CreepLog) : CreepLog =
    let newest =
        log.Entries
        |> List.tryFindIndexBack (fun entry ->
            not (isMovement entry.Verdict || isScoring entry.Verdict))

    let grown =
        match verdict, newest |> Option.map (fun i -> i, List.item i log.Entries) with
        | Verdict.Refused(creep, task, code, count),
          Some(at,
               ({
                    Verdict = Verdict.Refused(_, before, was, counted)
                } as entry)) when before = task && was = code ->
            Some(
                log.Entries
                |> List.mapi (fun i other ->
                    if i = at then
                        { entry with
                            Verdict = Verdict.Refused(creep, task, code, counted + count)
                        }
                    else
                        other)
            )
        | _ -> None

    { log with
        Entries =
            grown
            |> Option.defaultWith (fun () ->
                log.Entries @ [ { Tick = tick; Verdict = verdict } ] |> trim cap)
    }

/// Fold one tick of a single creep's Verdicts, in emission order, into its
/// log: append each Verdict that is a change, stamp it with the tick, keep the
/// newest cap-many entries, and advance the channel cursors. The task cursor
/// only ever moves forward, task Verdicts being total; the scoring and
/// movement cursors are episode baselines and reset on a tick that brings none.
let private step cap tick (verdicts: Verdict list) (log: CreepLog) : CreepLog =
    let appended =
        (log, verdicts)
        ||> List.fold (fun log verdict ->
            match verdict with
            | Verdict.Refused _ -> refuse cap tick verdict log
            | _ ->
                let unchanged =
                    if isMovement verdict then
                        log.LastMove |> List.exists (sameSubstance verdict)
                    elif isScoring verdict then
                        log.LastScoring |> Option.exists (sameSubstance verdict)
                    else
                        log.LastTask |> Option.exists (sameSubstance verdict)

                let log =
                    if isMovement verdict || isScoring verdict then
                        log
                    else
                        { log with LastTask = Some verdict }

                if unchanged then
                    log
                else
                    { log with
                        Entries = log.Entries @ [ { Tick = tick; Verdict = verdict } ] |> trim cap
                    })

    { appended with
        LastScoring = verdicts |> List.filter isScoring |> List.tryLast
        LastMove = verdicts |> List.filter isMovement
    }

/// The [[transition log]]'s fold: this tick's Verdicts plus the previous
/// observe state produce the new one. An entry lands only where a Verdict
/// changes a creep's story, and each per-creep timeline is a ring trimmed to
/// `cap`. Only creeps in `living` keep state — a dead creep's timeline is
/// pruned whole, and a Verdict naming a creep not alive writes nothing.
let fold
    (cap: int)
    (tick: int)
    (living: Set<string>)
    (verdicts: Verdict list)
    (prior: ObserveState)
    : ObserveState =
    let grouped = verdicts |> List.groupBy creepOf |> Map.ofList

    let names =
        Set.union
            (prior |> Map.toSeq |> Seq.map fst |> Set.ofSeq)
            (grouped |> Map.toSeq |> Seq.map fst |> Set.ofSeq)
        |> Set.intersect living

    names
    |> Seq.map (fun name ->
        let log =
            Map.tryFind name prior
            |> Option.defaultValue
                {
                    Entries = []
                    LastTask = None
                    LastScoring = None
                    LastMove = []
                }

        name, step cap tick (Map.tryFind name grouped |> Option.defaultValue []) log)
    // Built where nothing is captured (#401): a comparer made here would keep
    // `prior` — last tick's log — alive through this tick's, every tick.
    |> Fresh.mapOfSeq

/// A creep off the verbose list drops its scorings at once: each runs to
/// kilobytes, and the ring would otherwise hold them for the creep's life.
let forgetScorings (verbose: Set<string>) (state: ObserveState) : ObserveState =
    let stale (name, log: CreepLog) =
        not (verbose.Contains name)
        && log.Entries |> List.exists (fun entry -> isScoring entry.Verdict)

    if state |> Map.toSeq |> Seq.exists stale |> not then
        state
    else
        state
        |> Map.toSeq
        |> Seq.map (fun (name, log) ->
            if stale (name, log) then
                name,
                { log with
                    Entries =
                        log.Entries |> List.filter (fun entry -> not (isScoring entry.Verdict))
                }
            else
                name, log)
        |> Fresh.mapOfSeq

/// A Verdict as the room visual's overlay words it (#638): the wire's
/// spelling of its reason, without the creep it names.
let verdictText =
    function
    | Verdict.Matched(_, _, factor) -> $"matched {matchFactorName factor}"
    | Verdict.Kept _ -> "kept"
    | Verdict.Released(_, _, reason) -> $"released {releaseReasonName reason}"
    | Verdict.Unassigned(_, reason) -> $"idle {idleReasonName reason}"
    | Verdict.Scoring(_, candidates) -> $"scored {List.length candidates}"
    | Verdict.Grounded _ -> "grounded"
    | Verdict.Yielded(_, counterpart) -> $"yields to {counterpart}"
    | Verdict.Rerouted _ -> "rerouted"
    | Verdict.Stalled _ -> "stalled"
    | Verdict.Refused(_, _, code, count) -> $"refused {code} x{count}"

/// A Verdict in a character or a few, for the say bubble after a Task's
/// glyph (#638): a steady Kept and a scoring say nothing.
let private sayCode =
    function
    | Verdict.Matched(_, _, factor) ->
        "+"
        + match factor with
          | MatchFactor.OnlyCandidate -> "o"
          | MatchFactor.Rank -> "r"
          | MatchFactor.TravelCost -> "t"
          | MatchFactor.Load -> "l"
          | MatchFactor.PoolOrder -> "p"
    | Verdict.Released(_, _, reason) ->
        "-"
        + match reason with
          | ReleaseReason.TaskGone -> "gone"
          | ReleaseReason.Outranked -> "out"
          | ReleaseReason.Called -> "call"
          | ReleaseReason.Refused -> "ref"
          | ReleaseReason.Rejected RejectReason.Inapplicable -> "inap"
          | ReleaseReason.Rejected RejectReason.CapacityFull -> "full"
          | ReleaseReason.Rejected RejectReason.Unreachable -> "unr"
          | ReleaseReason.Rejected RejectReason.Threatened -> "thr"
          | ReleaseReason.Rejected(RejectReason.TooEarly _) -> "early"
    | Verdict.Unassigned _ -> "?"
    | Verdict.Refused(_, _, code, _) -> $"!{abs code}"
    | Verdict.Grounded _ -> "g"
    | Verdict.Yielded _ -> "y"
    | Verdict.Rerouted _ -> "~"
    | Verdict.Stalled _ -> "s"
    | Verdict.Kept _
    | Verdict.Scoring _ -> ""

/// Each creep's say code off one tick's Verdicts (#638): its refusal, then
/// its Task's story, then its step's, each in a character or a few.
let sayCodes (verdicts: Verdict list) : Map<string, string> =
    let rank verdict =
        match verdict with
        | Verdict.Refused _ -> 0
        | _ when isMovement verdict -> 2
        | _ -> 1

    verdicts
    |> List.groupBy creepOf
    |> List.choose (fun (creep, own) ->
        match own |> List.sortBy rank |> List.map sayCode |> String.concat "" with
        | "" -> None
        | code -> Some(creep, code))
    |> Map.ofList

/// The engine's cap on a say, in string length.
let sayCap = 10

/// The target inside a Task id: `Facts.taskId` writes it between the first
/// colon and the next, and neither an object id nor a room name holds one.
let taskTargetOf (tid: string) : string option =
    match tid.IndexOf ':' with
    | -1 -> None
    | colon ->
        let rest = tid.Substring(colon + 1)

        match rest.IndexOf ':' with
        | -1 -> Some rest
        | next -> Some(rest.Substring(0, next))

/// A Task's target as a bubble shows it (#638): a room name whole, an object
/// id by its last three characters.
let sayTarget (tid: string) : string =
    match taskTargetOf tid with
    | None -> ""
    | Some target when
        target.Length >= 4
        && (target.[0] = 'W' || target.[0] = 'E')
        && System.Char.IsDigit target.[1]
        ->
        target
    | Some target when target.Length > 3 -> target.Substring(target.Length - 3)
    | Some target -> target

/// A Task's glyph, then its creep's say code, then the Task's target, cut to
/// the engine's cap: the glyph is never cut, and the code goes before the
/// target because it is this tick's news.
let denser (glyph: string) (code: string) (target: string) =
    let room = max 0 (sayCap - glyph.Length)
    let text = code + target
    glyph + (if text.Length > room then text.Substring(0, room) else text)

/// One visible reading of the sector Reactor programme; `None` at the fold
/// boundary means the room is blind.
type ReactorReading =
    {
        Owner: ReactorOwner
        StoreT: int
        ContinuousWork: int
        BankedT: int
        /// A Thorium transfer Intent aimed at this Reactor was issued this
        /// tick. Store growth is independent evidence handled by the fold.
        DeliveryIssued: bool
    }

/// The flat, durable Reactor observation state. `Seen = None` is the empty
/// state; the remaining zeroes then carry no claim about a tick nobody saw.
type ReactorState =
    {
        Owner: ReactorOwner
        StoreT: int
        ContinuousWork: int
        Seen: int option
        BankedT: int
        LastDelivery: int option
        DryTicks: int
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ReactorState =
    let empty =
        {
            Owner = ReactorOwner.Unowned
            StoreT = 0
            ContinuousWork = 0
            Seen = None
            BankedT = 0
            LastDelivery = None
            DryTicks = 0
        }

/// Fold a visible Reactor reading into the programme record. A blind tick
/// retains the last sample exactly. A delivery is evidenced by a transfer
/// Intent or by store growth from an earlier sample; the empty state's zero is
/// deliberately not a baseline, so first sight never invents a delivery.
let foldReactor (tick: int) (reading: ReactorReading option) (prior: ReactorState) : ReactorState =
    match reading with
    | None -> prior
    | Some current ->
        let delivered =
            current.DeliveryIssued
            || (Option.isSome prior.Seen && current.StoreT > prior.StoreT)

        {
            Owner = current.Owner
            StoreT = current.StoreT
            ContinuousWork = current.ContinuousWork
            Seen = Some tick
            BankedT = current.BankedT
            LastDelivery = if delivered then Some tick else prior.LastDelivery
            DryTicks = prior.DryTicks + if current.StoreT = 0 then 1 else 0
        }

/// One hostile in one room of the world [[raid roster]] (`Roster`): who owns
/// it, what it was made of when first seen there, the ticks it was first and
/// last seen there, the tick its life runs out on, and when it fell.
type RosterRow =
    {
        Owner: string
        Body: Map<BodyPart, int>
        First: int
        Last: int
        /// Its last sighting plus the ticks it then had to live: the same
        /// tick in every room it is seen in. A tombstone before it is a death
        /// before its time, a kill (`killed`); one on it, old age.
        Dies: int
        /// The tick its tombstone was first seen, wherever our vision reached.
        Fell: int option
    }

/// Whether a fallen row died before its time: its tombstone was seen before
/// its life ran out. A body that died of age beside us is no kill.
let killed (row: RosterRow) =
    row.Fell |> Option.exists (fun fell -> fell < row.Dies)

/// The smallest range recorded between any hostile and anything of ours, with
/// the tile the hostile stood on and the tick it was measured at: the number
/// that separates a probe at the room edge from a loss.
type Approach = { Range: int; Pos: RoomPos; Tick: int }

/// One owned creep gone while a hostile stood in the room, stamped at the
/// tick it was last seen alive and the tile it last stood on. `Where` is None
/// when the projection never placed the body, and on a legacy row.
type Loss =
    {
        Creep: string
        Tick: int
        Where: RoomPos option
        /// The guards and rangers of the colony placed in `Where`'s room that
        /// tick, the body itself among them if it is one (#612): the cast
        /// that lost there. Zero where no tile was, and on a legacy row.
        Cast: int
    }

/// Whether a creep's name is the guard row's or the ranger row's.
let private isFighterName (name: string) =
    isNamedFor guardPattern name || isNamedFor rangerPattern name

/// One raid: opened on the first tick any room the colony works and can see
/// holds a hostile, kept open while hostiles keep appearing, closed by a quiet
/// gap. A room it only crosses opens nothing. Colony-level: the losses, the
/// closest approach and the damage are its side of the raid.
type RaidEpisode =
    {
        /// The tick the episode opened.
        Opened: int
        /// The last tick a hostile actually stood in one of those rooms.
        /// The episode stays open while `tick - LastSeen` is inside the
        /// quiet gap — openness is derived, never stored.
        LastSeen: int
        /// The rooms the colony works that a hostile stood in inside the
        /// window: where the world roster's rows of this raid are read. Who
        /// the hostiles were, and which of them fell, is the world's
        /// (`Roster`), so a raid two colonies see is enrolled once.
        Rooms: Set<string>
        /// None while nothing of ours could be placed, never a zero range.
        Closest: Approach option
        /// Owned creeps lost inside the window, oldest first — every one
        /// stamped at a tick between `Opened` and `LastSeen`.
        Losses: Loss list
        /// Hits lost across the Keep and the ramparts inside the window:
        /// decreases summed tick over tick, repairs ignored. Read only on the
        /// ticks a hostile stood there or the one after, so a quiet gap's
        /// decay is charged to nobody.
        Damage: int
    }

/// One outpost's stand-down episode: the Raid log's second family, carrying
/// the one field a raid never needed, the tick the stand-down expires. What
/// the family has in common is a clock, not a danger; `Basis` says which. A
/// record of its own rather than a widening of `RaidEpisode`, because nothing
/// a raid episode records can be read off a core.
type OutpostEpisode =
    {
        /// The room this stand-down shuts.
        RoomName: string
        /// The tick this stand-down opened.
        Opened: int
        /// The last tick the thing that opened this row was actually read in
        /// the room, by any vision of ours, an observer's included (#534). A
        /// record for the reader and never the openness test: silence here
        /// says nothing of ours looked and never that the room is clear.
        LastSeen: int
        /// The absolute tick the stand-down runs to.
        Expiry: int
        /// Which deadline `Expiry` came off. Carried because it cannot be
        /// recovered from the tick afterwards.
        Basis: StandDownBasis
        /// Whether a stronghold (an invader core of level 1 or more) was seen
        /// in this room while the row stood. Sticky for the row's life: the
        /// colony stops crossing the room and so stops seeing anything in it.
        /// A field and not a `Basis` case, because `sight` overwrites the basis
        /// whenever a later deadline arrives, and a room whose raid outlived
        /// its core would have gone back to being crossed.
        Stronghold: bool
        /// The raid bodies sighted here while the row stood and not gone since
        /// (`strikeGone`), by id: what ends a raid's clock early once it
        /// empties (#507). Empty on a row no raid clocks, and on one spent.
        Raiders: Set<string>
        /// The tick a raid of a player's bodies alone was first seen standing
        /// in this outpost while the row stood (#513): while it is set, each
        /// sighting cuts the row to that tick plus its priced hold
        /// (`Decide.Facts.raidHoldSpan`, #585), never short of a rival's hold.
        /// A fact beside the basis, which a later deadline overwrites. None
        /// once an Invader stands among the bodies, and on any other row.
        PlayerSince: int option
        /// Whether the raid this outpost's row runs on was, at its last
        /// sighting, one a ranger chases (`Decide.Facts.chased`, #510): the
        /// room stays projected for the chase while the row stands.
        Chased: bool
        /// Whether the row reopened inside `Tuning.RaidBounceTicks` of an
        /// earlier one in its room that the raid's going ended (`bounceFloor`):
        /// read off that row at the opening, since the world log keeps one row
        /// a room and the earlier one is gone the tick after.
        Bounced: bool
        /// Whether no look has read whose this row's reservation is since an
        /// enemy's could read as his (#604): a row written before then waits
        /// on a hold that may be an enemy's, so the gate questions it
        /// (`StandDown.RivalReserved`) until a look reads a neutral's or an
        /// ally's (`sight`) or ends it (`endEnemyHeld`).
        HolderUnread: bool
    }

/// Whether an outpost episode still holds its room shut at this tick: the one
/// place the family's openness is decided. Exempt from the quiet gap that
/// closes a raid, load-bearingly: a core's absence from a view is no evidence,
/// and the stand-down's whole effect is to withdraw the creeps whose vision
/// would see it.
let standingDown (tick: int) (episode: OutpostEpisode) = tick < episode.Expiry

/// One room latched on another player's ownership: the clockless withdrawal,
/// and the two ticks it takes to hold one honestly. Two questions: when did
/// this room stop paying, and when is the colony next willing to question the
/// conclusion. Neither tick is compared against an expiry.
type RivalLatch =
    {
        /// The tick the gate shut on, never restamped by a later look that
        /// agrees with it.
        Since: int
        /// The tick the last look fell due on, which the next stride is
        /// measured from (`lookDue`). It moves on every tick the gate hands
        /// the room out, whether or not vision answered: being blind in the
        /// room is the common case, and a stride stamped only when something
        /// was seen would re-admit a blind room to the scan on every tick from
        /// its first due one. One gap: `ColonyView.ofWorld` reads a controller
        /// only for declared rooms, so a room latched and since undeclared is
        /// stamped by a look nothing took; re-declaring it costs at most one
        /// stride.
        LastLooked: int
    }

/// One outpost whose controller somebody else's CLAIM parts hold, as the
/// colony last read it (#333): whose, and the tick the hold runs out on.
/// Neither a stand-down nor a latch. Not a stand-down: what this costs is the
/// reservation and nothing else (`reserveController` is refused, so the
/// reserver row hires nobody for it), and of the Invader's leftover hold that
/// is the whole story, the room going on being mined; a rival's reservation
/// also opens a clocked stand-down. Not a latch: the engine counts the hold
/// down, so the entry ends itself with no look. `standDown` acts on it: the
/// reserver is the only body most of these rooms hold, and a rule read off
/// vision alone would hire one more reserver every time the last one died.
type OutpostHold =
    {
        /// Whose CLAIM parts hold it. `Ours` never reaches this map.
        Holder: ReservationHolder
        /// The absolute tick the hold runs out on; stored as read it would
        /// date the hold to a tick long past.
        Until: int
    }

/// One declared outpost an armed threat was seen standing in, against the
/// tick that memory runs out on (#366): a conclusion held across the blind
/// ticks, because the bodies whose vision hired the guard are what the raid
/// kills.
type ThreatLatch =
    {
        /// The absolute tick the memory runs out on: last seen plus
        /// `Tuning.ThreatMemory`.
        Until: int
    }

/// How an [[assault]]'s probe ended (#493).
type ProbeFate =
    /// Still in it: no safe mode yet, or walking out of one.
    | Probing
    /// Out of the room alive once safe mode was up, on this tick.
    | Out of tick: int
    /// Gone on this tick with life left: killed.
    | Died of tick: int
    /// Gone on this tick with its life run out.
    | Expired of tick: int

/// What one probe learnt of a rival room's safe-mode trigger (#493): from
/// its first dismantle there to its fate.
type ProbeLog =
    {
        /// The probe's name.
        Probe: string
        /// The tick of its first dismantle in the room.
        FirstHit: int
        /// Its dismantles before safe mode was seen, a light tick's replay of
        /// one counted.
        Hits: int
        /// The tick of the last decided tick it dismantled on, while it still
        /// does: the light ticks after it replay that dismantle.
        HitAt: int option
        /// The tick safe mode was first seen in the room.
        SafeModeAt: int option
        Fate: ProbeFate
        /// The tick its life runs out on, as last seen.
        Expires: int
        /// The home of the colony whose probe it is: the one colony that moves
        /// the log while it probes. Empty on a log before the world held it,
        /// which any colony moves.
        Home: string
    }

/// The whole persisted Raid log.
type RaidState =
    {
        /// The episode ring: oldest first, trimmed from the front — the
        /// Transition log's own convention.
        Episodes: RaidEpisode list
        /// The outpost family's own ring, oldest first. Its own ring because
        /// a shared list is a shared depth: twenty spawn-room raids would evict
        /// the episode holding an outpost shut. So `cap` is a depth per family,
        /// and this ring trims by one further rule (`trimOutposts`).
        Outposts: OutpostEpisode list
        /// The rooms whose controller, on the last tick the colony could see
        /// it, belonged to another player: the withdrawal with no clock. A
        /// remembered conclusion, persisted because the gate's own effect is
        /// to withdraw the creeps whose vision would re-decide it; the room is
        /// re-admitted to the scan one stride at a time (`lookDue`). The only
        /// per-room state the colony keeps in Memory.
        RivalHeld: Map<string, RivalLatch>
        /// The rooms whose controller, on the last tick the colony could see
        /// it, was reserved by somebody else, each against the tick that hold
        /// runs out on. Withdraws no room: what it takes away is the
        /// reservation, read by the reserver row on a blind tick
        /// (`ColonyView.HeldOutposts` through `standDown`). Expires by itself
        /// when `Until` passes. No ring and no cap: bounded by the rooms the
        /// colony scans.
        Holds: Map<string, OutpostHold>
        /// The declared outposts an armed threat was standing in on the last
        /// tick the colony could see them, each against the tick that memory
        /// runs out on: `Holds`' shape applied to the guard row. It withdraws
        /// nothing; it keeps the guard hired and walked at the room on the
        /// ticks the raid has killed everything of ours that could see it
        /// (`Planner.guardedOutposts`). Ends by a clock of our own
        /// (`Tuning.ThreatMemory`), since nothing in the engine counts a raid
        /// down. Bounded by the declared outposts.
        Threatened: Map<string, ThreatLatch>
        /// The resident rooms a raid their residents lose was seen in
        /// (`foldFights`), each against the last tick it was: what keeps a
        /// home's rampart doors private through the ticks its raid steps out.
        /// Bounded by the resident rooms. This and the two below,
        /// like the four above, are the world danger log's, laid in
        /// (`Danger.into`): every colony reads one record a room.
        Fought: Map<string, FightRecord>
        /// Each [[assault]] room's last probe (`foldProbes`, #493): what
        /// `observe.mjs raids` prints of what raised its safe mode. Bounded
        /// by the declared assaults.
        Probes: Map<string, ProbeLog>
        /// Each [[assault]] room whose declared targets were all down at the
        /// last look, against the tick they were first seen so (#496): the
        /// work done, cast for no more until one stands again. Bounded by the
        /// declared assaults.
        Taken: Map<string, int>
        /// Whether this tick read the stock under its [[emergency reserve]]
        /// (`Facts.belowReserve`, #505): next tick's latch.
        ReserveShort: bool
        /// The [[ore]]s whose dig this tick left open (`Facts.oresDigOpen`):
        /// next tick's latch. None for a latch never written (a leaf from
        /// before it), read as every dig open.
        DigOpen: Set<Ore> option
        /// The haul overflow reading the last decided tick wrote
        /// (`Decision.HaulOverflow`, #508): next tick's base to smooth on
        /// from. Set by the loop off the decision; the fold leaves it alone.
        HaulOverflow: HaulOverflow
        /// The last tick a raider stood in the home (`Facts.raided`, #526),
        /// None for never: what holds the full rampart floors over the build
        /// floor between a raid's visits.
        HomeRaided: int option
        /// The owned creep names the previous tick projected, less the ones
        /// whose life ran out on it: the baseline this tick's losses are read
        /// against. Carried only while an episode is open. This colony's names,
        /// differenced against the world's living names: a creep another
        /// colony adopted leaves the baseline without dying.
        Living: Set<string>
        /// The tile each of those bodies last stood on, kept on the same
        /// condition: what a loss is stamped with, read the tick after the
        /// body is gone. Some forty bytes a body, so a sixty-body colony
        /// writes ~2.5KB a tick while a raid is open. Worked rooms only: a
        /// body that dies in a transit room is stamped with no tile.
        Placed: Map<string, RoomPos>
        /// The previous tick's hits per structure id across the Keep and the
        /// ramparts, carried as `Living` is and only on a tick a hostile stood
        /// in the home room: the Keep stands in one room and the episode spans
        /// every room the colony works.
        Hits: Map<string, int>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RaidState =
    /// The empty Raid log: no episodes, neither baseline — what an
    /// absent, malformed or foreign-shaped subtree reads as.
    let empty =
        {
            Episodes = []
            Outposts = []
            RivalHeld = Map.empty
            Holds = Map.empty
            Threatened = Map.empty
            Fought = Map.empty
            Probes = Map.empty
            Taken = Map.empty
            ReserveShort = false
            DigOpen = None
            HaulOverflow = HaulOverflow.none
            HomeRaided = None
            Living = Set.empty
            Placed = Map.empty
            Hits = Map.empty
        }

/// The episode ring cap: twenty episodes of a few hundred bytes each sit
/// around 10KB against the 2MB Memory.
let capEpisodes = 20

/// The tiles of everything of ours a hostile can close on: our creeps, the
/// Refillables and the controller. The Storage is left out because it stands
/// inside the ring the Refillables cover; the ramparts because one over a
/// Post would pull the number out to the sources.
let private ourTilesIn (view: ColonyView) (room: string) : RoomPos list =
    let layer = SpatialInfo.layerOf view.Spatial room

    let structures =
        (view.Refillables |> List.map (fun r -> r.Id))
        @ (view.Controller |> Option.toList |> List.map (fun c -> c.Id))
        |> List.choose (fun id -> Map.tryFind id layer.TargetPositions)

    (layer.CreepPositions |> Map.toList |> List.map snd) @ structures
    |> List.map (RoomPos.at room)

/// This tick's closest approach, if there is both a hostile and something of
/// ours to measure it against. The hostile-free tick is the common one — every
/// tick of an open episode's quiet gap is one — so it answers before the owned
/// set is built.
let private approachAt (view: ColonyView) (hostiles: HostileInfo list) : Approach option =
    // Armed hostiles alone: a `1 MOVE` scout at range 1 is neither a probe
    // nor a loss, and one was once read as the raid (#376). The scouts stay
    // on the roster.
    //
    // Nor anything in a harassment room this colony casts, shut or not
    // (#432), or in a room it assaults (#490): the raid there is ours.
    let assaulted =
        view.Assaults |> List.map (fun facts -> facts.Assault.RoomName) |> Set.ofList

    let armed =
        hostiles
        |> List.filter (fun hostile ->
            HostileInfo.armed view.Hostiles hostile
            && not (Set.contains hostile.Pos.Room view.HarassCast)
            && not (Set.contains hostile.Pos.Room assaulted))

    if List.isEmpty armed then
        None
    else
        let ours =
            armed
            |> List.map (fun hostile -> hostile.Pos.Room)
            |> List.distinct
            |> List.map (fun room -> room, ourTilesIn view room)
            |> Map.ofList

        let measured =
            armed
            |> List.collect (fun hostile ->
                Map.tryFind hostile.Pos.Room ours
                |> Option.defaultValue []
                |> List.choose (fun tile ->
                    RoomPos.range hostile.Pos tile |> Option.map (fun r -> r, hostile.Pos)))

        if List.isEmpty measured then
            None
        else
            let closest, pos = measured |> List.minBy fst

            Some
                {
                    Range = closest
                    Pos = pos
                    Tick = view.Time
                }

/// Keep the nearer of the two, and on a tie the one already recorded: the
/// tick a raid first reached its closest is the one worth carrying.
let private nearer (stored: Approach option) (fresh: Approach option) =
    match stored, fresh with
    | Some stored, Some fresh -> Some(if fresh.Range < stored.Range then fresh else stored)
    | Some stored, None -> Some stored
    | None, fresh -> fresh

/// The rooms a colony's raid episodes read: its home, the rooms it declares
/// or prices (`Facts.pricedRooms`) and its resident rooms. A room it only
/// crosses is the world roster's alone, so a siege three colonies cross is
/// enrolled once and opens no episode in the two that only pass by.
let private workedRooms (view: ColonyView) (declared: string list) : Set<string> =
    Set.unionMany
        [
            Set.singleton (SpatialInfo.homeName view.Spatial)
            Decide.Facts.pricedRooms (Set.ofList declared) view
            Decide.Facts.residentRooms view
        ]

/// The tick one stronghold core's stand-down runs to, and which deadline it
/// was read off: its collapse timer, or the fallback while it carries none —
/// a deploying stronghold, which gains its timer only once it deploys, some
/// 5,000 ticks after it lands (`backend/lib/strongholds.js`). No reservation
/// is read: a stronghold lands in a keeper room, which has no controller, and
/// one a map gives a controller owns it rather than reserving it.
let private deadlineOf (view: ColonyView) (core: InvaderCoreInfo) =
    match core.CollapseTick with
    | Some tick -> tick, StandDownBasis.CollapseTimer
    | None -> view.Time + view.Tuning.StandDownFallback, StandDownBasis.Fallback

/// The tick a room another player has reserved stands down to: the end of
/// that reservation, off the engine's own countdown. A neutral's or an
/// ally's alone: an enemy's hold is contested, never waited out (#604), so
/// the room stays worked and its armed bodies are the verdict's
/// (`raidDeadlines`). No floor under it: a player's claimer that stops coming
/// re-takes nothing. Ownership is not here: the engine gives it no end, so it
/// stays the latch `RivalHeld` remembers.
///
/// Never in a harassment room this colony casts, shut or not (#432): the
/// enemy's reservation is the room's whole reason for being declared, and
/// read in a shut one it would stretch the squad's stand-down to its own.
let private rivalDeadlines (view: ColonyView) =
    view.RoomControl
    |> Map.toList
    |> List.filter (fun (room, _) -> not (Set.contains room view.HarassCast))
    |> List.choose (fun (room, control) ->
        control
        |> RoomControlInfo.heldBy ReservationHolder.Rival
        |> Option.map (fun held ->
            room, (view.Time + held.TicksToEnd, StandDownBasis.RivalReservation)))

/// The tick a room stands down to when the raid in it is one the colony has
/// decided not to fight: the raid's own life, since an Invader in a room
/// nobody owns never suicides (the engine's suicide branch wants a controller
/// owner). Only a raid nothing of ours wins (`Facts.answer` is `Lose`: neither
/// the row's biggest body the bank buys, #417, the ranger's in an errand room,
/// #411, nor a ranger kiting it, #510, nor the rangers standing in it with
/// those called from beside it): a withdrawal
/// and a garrison are the same room's two answers, told apart here — for an
/// outpost and, since the ranger row fights for it (#414, #411), for an
/// errand room, whose Source Keepers are terrain and neither open nor extend
/// the clock. A transit
/// room is neither answer. A harassment room is read as an errand room is
/// (#432): its ranger is weighed against the armed squad standing there, in
/// every room the colony casts, shut or not, and it is clocked off the
/// sighting and not the squad's life (#441). A raised child's home is no
/// room to withdraw from (#447), as a defended one is not (#589), whatever
/// the raid: one no ranger wins keeps its garrison, on safe ground, and has
/// nobody cast into it (its Brief's `Asks`, #451); it is the child's safe
/// mode's.
let private raidDeadlines (view: ColonyView) (declared: string list) =
    let rangerRooms =
        Set.difference
            (Set.union (Decide.Facts.rangerRooms view) view.HarassCast)
            (Decide.Facts.raisedHomes view)

    let outpostRooms = Set.ofList declared

    // A held errand's room, dug for its [[ore]] and garrisoned by nobody:
    // any raid stands it down, since no row of ours is cast to answer one.
    let unguarded = Set.difference (ColonyView.oreRooms view) rangerRooms

    // The raid is the bodies that still hurt something, the one predicate
    // `strikeGone` enrols on (#513): a body whose weapons are shot off, and
    // no heal reaches, neither opens a row nor holds one.
    view.Hostiles
    |> List.filter (fun h ->
        h.Pos.Room <> SpatialInfo.homeName view.Spatial
        && HostileInfo.hurts view.Hostiles h
        && not (HostileInfo.isKeeper h)
        // A body that can barely move opens nothing by itself (#510): it
        // harms only what stands about it, and its tiles are the Reach's.
        && not (HostileInfo.stationary view.Hostiles h))
    |> List.groupBy (fun h -> h.Pos.Room)
    |> List.filter (fun (room, _) ->
        Set.contains room unguarded
        || (Set.contains room rangerRooms || Set.contains room outpostRooms)
           && Decide.Facts.answer view room = Answer.Lose)
    |> List.map (fun (room, raid) ->
        if Set.contains room view.HarassCast then
            room, (view.Time + view.Tuning.HarassClearTicks, StandDownBasis.HarassSighting)
        else
            let life = raid |> List.map (fun h -> h.TicksToLive) |> List.max

            room, (view.Time + life, StandDownBasis.InvaderRaid))

/// `looking` is the view the raid rows are re-read off (`foldRaids`), the
/// world's vision of their shut rooms laid in; cores and reservations are read
/// off the colony's own.
let private deadlines (view: ColonyView) (looking: ColonyView) (declared: string list) =
    // The stronghold bit is or-ed over a room's sightings where the clock is
    // maxed: a room seen once with a bunker and once with a raider is a room
    // with a bunker in it, whichever deadline is longer. A level-0 core opens
    // nothing: it only reserves the controller, and a guard kills it (#487).
    (view.InvaderCores
     |> List.filter InvaderCoreInfo.isStronghold
     |> List.map (fun core ->
         let expiry, basis = deadlineOf view core
         core.RoomName, (expiry, basis, true)))
    @ (rivalDeadlines view
       |> List.map (fun (room, (expiry, basis)) -> room, (expiry, basis, false)))
    @ (raidDeadlines looking declared
       |> List.map (fun (room, (expiry, basis)) -> room, (expiry, basis, false)))
    |> List.groupBy fst
    |> List.map (fun (room, seen) ->
        let found = seen |> List.map snd
        let expiry, basis, _ = found |> List.maxBy (fun (expiry, _, _) -> expiry)
        room, (expiry, basis, found |> List.exists (fun (_, _, bunker) -> bunker)))

/// Whether a harassment sighting replaces this clock rather than racing it
/// (#441): a squad's life or a rival's reservation is no clock of a room the
/// colony harasses, and one read before the room was (or before #441) would
/// outlive the sighting that is. A core's clock is the core's.
let private harassOverrides (basis: StandDownBasis) =
    match basis with
    | StandDownBasis.InvaderRaid
    | StandDownBasis.PlayerRaidHold
    | StandDownBasis.RivalReservation -> true
    | StandDownBasis.CollapseTimer
    | StandDownBasis.Reservation
    | StandDownBasis.Fallback
    | StandDownBasis.HarassSighting
    | StandDownBasis.RaidGone -> false

/// Fold one room's sighting into the outpost ring: the room's standing
/// episode takes it, or the sighting opens one. The re-read only ever moves a
/// running clock outward, and the basis with it, so reading in a worse
/// deadline cannot cut a stand-down short, except a harassment sighting over a
/// clock `harassOverrides` names, which it replaces.
let private sight
    (bounceTicks: int)
    tick
    (room, (expiry, basis, stronghold))
    (episodes: OutpostEpisode list)
    =
    let holds (episode: OutpostEpisode) =
        episode.RoomName = room && standingDown tick episode

    let takes (episode: OutpostEpisode) =
        expiry > episode.Expiry
        || basis = StandDownBasis.HarassSighting && harassOverrides episode.Basis

    if episodes |> List.exists holds then
        episodes
        |> List.map (fun episode ->
            if holds episode then
                { episode with
                    LastSeen = tick
                    Expiry = if takes episode then expiry else episode.Expiry
                    Basis = if takes episode then basis else episode.Basis
                    // Sticky, where the basis is not: a bunker seen once is a
                    // bunker.
                    Stronghold = episode.Stronghold || stronghold
                    // A rival-reservation sighting is a look that read the
                    // hold a neutral's or an ally's.
                    HolderUnread = episode.HolderUnread && basis <> StandDownBasis.RivalReservation
                }
            else
                episode)
    else
        episodes
        @ [
            {
                RoomName = room
                Opened = tick
                LastSeen = tick
                Expiry = expiry
                Basis = basis
                Stronghold = stronghold
                // `strikeGone`'s to fill, on this same tick.
                Raiders = Fresh.setOfSeq Seq.empty
                PlayerSince = None
                Chased = false
                Bounced =
                    episodes
                    |> List.exists (fun earlier ->
                        earlier.RoomName = room
                        && earlier.Basis = StandDownBasis.RaidGone
                        && tick - earlier.Expiry <= bounceTicks)
                HolderUnread = false
            }
        ]

/// End each standing row clocked off another player's reservation whose room
/// vision answers for this tick with no neutral's or ally's hold on it
/// (#604): an enemy's hold is contested and never waited out, and a hold gone
/// or ours is no clock either. A row written before an enemy's hold read as
/// his ends on the first look (`StandDown.RivalReserved` buys it).
let private endEnemyHeld (view: ColonyView) (episodes: OutpostEpisode list) =
    let unclocked room =
        Map.tryFind room view.RoomControl
        |> Option.exists (RoomControlInfo.heldBy ReservationHolder.Rival >> Option.isNone)

    episodes
    |> List.map (fun episode ->
        if
            standingDown view.Time episode
            && episode.Basis = StandDownBasis.RivalReservation
            && unclocked episode.RoomName
        then
            { episode with Expiry = view.Time }
        else
            episode)

/// Whether a row runs on a raid's clock, and so on its bodies (#507): a core's
/// or a reservation's clock is not a raid's, and a bunker is a bunker.
let private raidClocked (episode: OutpostEpisode) =
    not episode.Stronghold
    && (episode.Basis = StandDownBasis.InvaderRaid
        || episode.Basis = StandDownBasis.PlayerRaidHold
        || episode.Basis = StandDownBasis.RaidGone)

/// The rooms of a log's standing raid rows (`raidClocked`) among `prices`:
/// the stand-down rows a colony prices out of sight with the world's look
/// laid in, what its raid log's fold re-reads (`foldRaids`).
let pricedRaidRows (prices: string -> bool) (tick: int) (outposts: OutpostEpisode list) =
    outposts
    |> List.filter (fun episode ->
        standingDown tick episode && raidClocked episode && prices episode.RoomName)
    |> List.map (fun episode -> episode.RoomName)

/// The least tick a raid's row may end on once its bodies are gone: now, or,
/// where the row reopened inside `Tuning.RaidBounceTicks` of an earlier one
/// the raid's going ended (`Bounced`), that many ticks past its opening, so a
/// squad bouncing on the border flips the projection once a bounce and not
/// once a tick.
let private bounceFloor (view: ColonyView) (episode: OutpostEpisode) =
    if episode.Bounced then
        max view.Time (episode.Opened + view.Tuning.RaidBounceTicks)
    else
        view.Time

/// Keep every standing raid row's bodies, and end its clock on the tick the
/// last one is gone (#507). A raider standing in the room, and a body that can
/// hurt something (`HostileInfo.hurts`, a dismantler) while a raider stands
/// beside it, is (re-)enrolled, beaten or not; a scout never holds a row, nor
/// a WORK body left alone with no weapon, which no Guard answers. One is
/// struck when seen as neither, or absent from the room and seen dead
/// anywhere the colony sees or, a player's, standing in another room, or
/// absent from the room on a tick vision answers for it — a look, an
/// observer's included, is the evidence it went (#585) — and from every room
/// beyond a border it was last seen within a dance of (`Dance`); one standing
/// that close across the border is not gone. Never on a blind absence, so a
/// raid nobody sees go keeps its life; an Invader seen elsewhere is not gone,
/// its clock being its life. A player's raid in a declared outpost is cut, at
/// each sighting, to the next tick plus its priced hold
/// (`Facts.raidHoldSpan`, #585, `PlayerSince`). Neither cut runs past
/// the clock, nor short of a rival's hold on the room (`rivalHolds`).
let private strikeGone
    (view: ColonyView)
    (declared: string list)
    (rivalHolds: Map<string, int>)
    (episodes: OutpostEpisode list)
    =
    let fallen = Fresh.setOfSeq view.Fallen

    let whereNow =
        view.Hostiles |> List.map (fun hostile -> hostile.Id, hostile) |> Fresh.mapOfSeq

    let raided =
        view.Hostiles
        |> List.filter (HostileInfo.raider view.Hostiles)
        |> List.map (fun hostile -> hostile.Pos.Room)
        |> Set.ofList

    // Still the raid: a raider, or a body that hurts beside one.
    let raiding (hostile: HostileInfo) =
        HostileInfo.raider view.Hostiles hostile
        || HostileInfo.hurts view.Hostiles hostile && Set.contains hostile.Pos.Room raided

    // Vision of a room this tick, the colony's or the world's, an observer's
    // look included.
    let sees room =
        Map.containsKey room view.RoomControl || WorldView.sees view.World room

    let gone room id =
        match Map.tryFind id whereNow with
        // A body that is no raid any more is gone, wherever it stands; a heal
        // that brings a weapon back is a fresh sighting. One across a border
        // within a dance of it (`Dance.besideOf`) has stepped out, not left.
        | Some hostile ->
            not (raiding hostile)
            || hostile.Pos.Room <> room
               && HostileInfo.crossesExits hostile
               && not (Dance.besideOf room hostile.Pos)
        // Out of this view: seen dead, or its room seen this tick without it —
        // a look that answers for the room is the evidence it went (#585) —
        // unless the world sees it this tick lingering across the border, or
        // it was last seen within a dance of a border or lingering across one
        // (`WorldView.LastSteps`, kept the bounce window) and a room it may
        // have stepped into is not seen too.
        | None ->
            Set.contains id fallen
            || sees room
               && (match Map.tryFind id view.World.LastSteps with
                   | Some(at, seen) when seen >= view.Time ->
                       at.Room <> room && not (Dance.besideOf room at)
                   | Some(at, _) when at.Room = room -> Dance.beyond at |> List.forall sees
                   | Some(at, _) when Dance.besideOf room at -> sees at.Room
                   | _ -> true)

    episodes
    |> List.map (fun episode ->
        if not (standingDown view.Time episode) then
            if
                Set.isEmpty episode.Raiders
                && Option.isNone episode.PlayerSince
                && not episode.Chased
            then
                episode
            else
                { episode with
                    Raiders = Fresh.setOfSeq Seq.empty
                    PlayerSince = None
                    Chased = false
                }
        elif not (raidClocked episode) then
            episode
        else
            let room = episode.RoomName

            let standing =
                view.Hostiles
                |> List.filter (fun hostile ->
                    hostile.Pos.Room = room
                    && not (HostileInfo.isKeeper hostile)
                    && raiding hostile)

            let raiders =
                Seq.append episode.Raiders (standing |> Seq.map (fun hostile -> hostile.Id))
                |> Seq.filter (gone room >> not)
                |> Fresh.setOfSeq

            let holdPast tick =
                Map.tryFind room rivalHolds |> Option.filter (fun until -> until > tick)

            // Set on the first tick a player's bodies alone stand in a declared
            // outpost (never an errand room, whose raid stays to hold the
            // room's prize); an Invader among them puts the row back on the
            // raid's life.
            let playerSince =
                match episode.PlayerSince with
                | _ when standing |> List.exists (HostileInfo.crossesExits >> not) -> None
                | Some since -> Some since
                | None when not (List.isEmpty standing) && List.contains room declared ->
                    Some view.Time
                | None -> None

            // While a player's raid alone stands there, each sighting reads its
            // hold afresh past the next tick (`Facts.raidHoldSpan`, #585), the
            // first the gate reads this tick's log on, so a raid in sight
            // always shuts its room; a blind tick keeps the last one's.
            let windowed =
                match playerSince, standing with
                | Some _, _ :: _ ->
                    let ends = view.Time + 1 + Decide.Facts.raidHoldSpan view room standing

                    let until = holdPast ends

                    match until with
                    | Some until when until < episode.Expiry ->
                        { episode with
                            PlayerSince = playerSince
                            Expiry = until
                            Basis = StandDownBasis.RivalReservation
                        }
                    | None when ends < episode.Expiry ->
                        { episode with
                            PlayerSince = playerSince
                            Expiry = ends
                            Basis = StandDownBasis.PlayerRaidHold
                        }
                    | _ ->
                        { episode with
                            PlayerSince = playerSince
                        }
                | _ ->
                    { episode with
                        PlayerSince = playerSince
                    }

            let floor = bounceFloor view episode

            let hold = holdPast floor

            let cut = hold |> Option.defaultValue floor

            // Read again at each sighting of the raid, and kept through the
            // ticks nothing of ours sees it (#510). A sighting is the row's
            // last, whatever the verdict (#534); one that finds the room
            // empty of the raid ends the chase in the row, which is what the
            // chase out of sight is gated on, so a look every other tick
            // cannot toggle it.
            let windowed =
                match standing with
                | [] when Map.containsKey room view.RoomControl -> { windowed with Chased = false }
                | [] -> windowed
                | _ ->
                    { windowed with
                        LastSeen = view.Time
                        Chased =
                            List.contains room declared
                            && Decide.Facts.chased
                                view
                                (Decide.Facts.answered view room)
                                false
                                room
                    }

            if
                not (Set.isEmpty raiders)
                || Set.isEmpty episode.Raiders
                || cut >= windowed.Expiry
            then
                { windowed with Raiders = raiders }
            else
                { windowed with
                    Raiders = raiders
                    Expiry = cut
                    Basis =
                        if Option.isSome hold then
                            StandDownBasis.RivalReservation
                        else
                            StandDownBasis.RaidGone
                })

/// Trim the outpost ring to the cap, oldest first, and never over an episode
/// that is still standing down: evicting that reopens the gate mid-stand-down.
let private trimOutposts cap tick (episodes: OutpostEpisode list) =
    let overflow = List.length episodes - cap

    if overflow <= 0 then
        episodes
    else
        ((overflow, []), episodes)
        ||> List.fold (fun (left, kept) episode ->
            if left > 0 && not (standingDown tick episode) then
                left - 1, kept
            else
                left, episode :: kept)
        |> snd
        |> List.rev

/// Whether the room this control entry answers for is another player's for
/// good. Owned and not reserved: the engine gives ownership no end, while a
/// reservation ends on a tick it is already counting down (`rivalDeadlines`).
let private rivalOwned (control: RoomControlInfo) = control.Owner = Ownership.Rival

/// Whether a latched room's next look falls due on this tick: the one place
/// the stride is spelled, read by the gate that hands the look out and by the
/// fold that records it taken. The gate takes its `Tuning` as an argument and
/// the fold reads the view's, so the agreement is the caller's to keep.
///
/// An elapsed test and not an exact multiple: a tick's evaluation is not
/// guaranteed (a throw, a CPU cut, a deploy), and every tick lost to a
/// multiple cost a whole further stride of income. A stamp ahead of the clock
/// (a hand edit, a rolled-back server) is a look owed now, or the latch is
/// unfalsifiable for as long as the stamp leads. A `RivalRecheck` of zero or
/// less is "never look again".
let private lookDue (tuning: Tuning) (tick: int) (latch: RivalLatch) =
    tuning.RivalRecheck > 0
    && (latch.LastLooked > tick || tick - latch.LastLooked >= tuning.RivalRecheck)

/// The rooms a row clocked off another player's reservation shuts this tick
/// whose holder no look has read yet (`HolderUnread`): looked into every tick
/// until one does (#604), since the hold it waits on may read as an enemy's,
/// which `endEnemyHeld` ends on sight.
let private rivalReserved (tick: int) (state: RaidState) : Set<string> =
    state.Outposts
    |> List.filter (fun episode ->
        standingDown tick episode
        && episode.Basis = StandDownBasis.RivalReservation
        && episode.HolderUnread)
    |> List.map (fun episode -> episode.RoomName)
    |> Set.ofList

/// The standing rows on a clock read off a raid's bodies, not a core's, a
/// reservation's or the fallback's: the gate's `RaidedOutposts`.
let private raidedRows (tick: int) (state: RaidState) : OutpostEpisode list =
    state.Outposts
    |> List.filter (fun episode ->
        standingDown tick episode
        && (match episode.Basis with
            | StandDownBasis.InvaderRaid
            | StandDownBasis.PlayerRaidHold
            | StandDownBasis.RaidGone
            | StandDownBasis.HarassSighting -> true
            | _ -> false))

/// The stand-down gate's answer for this colony this tick, read off the
/// previous tick's log: the one reader that acts on the Raid log. `Shut` is
/// dropped from the declarations the shell works; `Rechecked` has its
/// controllers read and nothing else; `HeldOutposts` and `ThreatenedOutposts`
/// withdraw nothing and ride the view for the reserver row and the guard row
/// to read on blind ticks. All derived here and once, so no second answer is
/// free to disagree.
let standDown (tuning: Tuning) (tick: int) (state: RaidState) : StandDown =
    {
        Shut =
            state.Outposts
            |> List.filter (standingDown tick)
            |> List.map (fun episode -> episode.RoomName)
            |> Set.ofList
            |> Set.union (state.RivalHeld |> Map.toList |> List.map fst |> Set.ofList)
        // The rooms of that set a body cannot walk through either. Off the
        // row and not this tick's vision: a rule keyed on vision would re-link
        // the room, walk a body in, lose it, and shut it again, for ever.
        Impassable =
            state.Outposts
            |> List.filter (fun episode -> standingDown tick episode && episode.Stronghold)
            |> List.map (fun episode -> episode.RoomName)
            |> Set.ofList
        Rechecked =
            state.RivalHeld
            |> Map.toList
            |> List.filter (snd >> lookDue tuning tick)
            |> List.map fst
            |> Set.ofList
            |> Set.union (rivalReserved tick state)
        RivalReserved = rivalReserved tick state
        // The standing holds, on the same `tick < Until` test the fold keeps
        // them by. Filtered here as well so the gate cannot withhold a
        // reservation on a leaf the fold has not caught up with.
        HeldOutposts =
            state.Holds
            |> Map.toList
            |> List.filter (fun (_, hold) ->
                tick < hold.Until && RoomControlInfo.refusesReserver hold.Holder)
            |> List.map fst
            |> Set.ofList
        // The standing threat memories, filtered here as well for the same
        // reason: a leaf the fold has not caught up with cannot go on hiring
        // a guard.
        ThreatenedOutposts =
            state.Threatened
            |> Map.toList
            |> List.filter (fun (_, latch) -> tick < latch.Until)
            |> List.map fst
            |> Set.ofList
        RaidedOutposts =
            raidedRows tick state
            |> List.map (fun episode -> episode.RoomName)
            |> Set.ofList
        RaidedUntil =
            raidedRows tick state
            |> List.map (fun episode -> episode.RoomName, episode.Expiry)
            |> Map.ofList
        // A standing raid row a ranger chases (#510).
        ChasedOutposts =
            state.Outposts
            |> List.filter (fun episode -> standingDown tick episode && episode.Chased)
            |> List.map (fun episode -> episode.RoomName)
            |> Set.ofList
        Fought = state.Fought
        Taken = state.Taken |> Map.keys |> Set.ofSeq
        ReserveShort = state.ReserveShort
        DigOpen = state.DigOpen
        HaulOverflow = state.HaulOverflow
        HomeRaided = state.HomeRaided
        KeepHitsBefore = state.Hits
        // A fighter by its row's name. One the projection never placed names
        // no room to hold; a legacy row, no cast, lost at least its own body.
        FighterLost =
            state.Episodes
            |> List.collect _.Losses
            |> List.choose (fun loss ->
                match loss.Where with
                | Some where when isFighterName loss.Creep ->
                    Some(
                        where.Room,
                        {
                            Tick = loss.Tick
                            Cast = max 1 loss.Cast
                        }
                    )
                | _ -> None)
            |> List.groupBy fst
            |> List.map (fun (room, lost) ->
                room, lost |> List.map snd |> List.maxBy (fun l -> l.Tick, l.Cast))
            |> Map.ofList
    }

/// The fight half of the Raid-log fold: a resident room's record is opened on
/// a tick a raid its residents lose stands there, moved on by every tick a
/// raid stands there after, and dropped once its raid was last seen more than
/// `Tuning.FightHoldTicks` ago. A record whose room holds no raider, and
/// every one of whose last-seen raiders lies dead, is dropped at once: the
/// raid was killed there, and the hold is for one that left. Rooms the colony
/// cannot see this tick keep what they had. The records are the world's: a
/// room this colony is not resident in is moved by the clock alone, never by
/// this colony's vision.
let foldFights (view: ColonyView) (prior: Map<string, FightRecord>) : Map<string, FightRecord> =
    let hold = view.Tuning.FightHoldTicks

    let raiders =
        view.Hostiles
        |> List.filter (HostileInfo.raider view.Hostiles)
        |> List.groupBy (fun hostile -> hostile.Pos.Room)
        |> Map.ofList

    let fallen = Set.ofList view.Fallen

    let killed room (record: FightRecord) =
        not (Map.containsKey room raiders)
        && not (Set.isEmpty record.Raiders)
        && Set.isSubset record.Raiders fallen

    let resident = Decide.Facts.residentRooms view

    // The clock runs out for every record, whoever's room it is; only a room
    // this colony is resident in is judged killed off its vision.
    let standing =
        prior
        |> Map.filter (fun room record ->
            not (resident.Contains room && killed room record)
            && view.Time - record.Seen <= hold)

    raiders
    |> Map.filter (fun room _ -> Set.contains room resident)
    |> Map.toList
    |> List.fold
        (fun rooms (room, raid) ->
            if Map.containsKey room rooms || Decide.Facts.residentsLose view room then
                Map.add
                    room
                    {
                        Seen = view.Time
                        Raiders = raid |> Seq.map (fun hostile -> hostile.Id) |> Fresh.setOfSeq
                    }
                    rooms
            else
                rooms)
        standing

/// ADR-0028
/// The Raid-log fold: this tick's view plus the previous Raid log produce the
/// new one. The two families are disjoint (one reads `Hostiles`, the other
/// `InvaderCores` and the controllers), so `cap` is a depth per family.
/// The taken half of the Raid-log fold (#496): an assault room seen with its
/// declared targets all down is recorded on the first tick it is, and dropped
/// on a tick one is seen standing; a dark room keeps what it had.
let foldTaken (view: ColonyView) (prior: Map<string, int>) : Map<string, int> =
    // The record is the world's, one a room: only an assault that declares
    // targets says whether they are down, so a second colony's assault of the
    // room with none (a probe's) leaves it be.
    match
        view.Assaults
        |> List.filter (fun facts ->
            Option.isSome facts.Targets && not (List.isEmpty facts.Assault.Targets))
    with
    | [] -> prior
    | seen ->
        seen
        |> List.fold
            (fun (taken: Map<string, int>) facts ->
                let room = facts.Assault.RoomName

                match facts.Taken, Map.containsKey room taken with
                | true, false -> Map.add room view.Time taken
                | true, true -> taken
                | false, _ -> Map.remove room taken)
            prior
        |> Map.toSeq
        |> Fresh.mapOfSeq

let foldRaids
    (cap: int)
    (alive: Set<string>)
    (view: ColonyView)
    (declared: string list)
    (prior: RaidState)
    : RaidState =
    // A sighting is folded, never a raid recalled through blind ticks (#569).
    let view = ColonyView.sightedOnly view

    // The rooms this log stands down from on a raid, read off the world's
    // vision, an observer's included, where the view no longer projects them
    // (#534): shut is not forgotten. The raid rows' re-read alone takes it —
    // their clock, last sighting and chase. The episode roster, the losses,
    // the cores and the reservations stay the colony's own vision's, or a
    // shut room's loiterer would grow the log every look. An outpost shut on
    // a raid is priced as the outpost it is.
    let raidRows = pricedRaidRows (fun _ -> true) view.Time prior.Outposts

    let looking = ColonyView.lookingInto raidRows view

    let priced =
        declared @ (raidRows |> List.filter view.MinedRooms.Contains) |> List.distinct

    let gap = view.Tuning.QuietGap

    // The baseline the next tick reads its losses against: this tick's names,
    // less the creeps whose clock runs out on it. A name gone tomorrow because
    // CREEP_LIFE_TIME ran down is old age, and TicksToLive says which is which
    // before the fact, so the difference never has to guess after it.
    let surviving =
        ColonyView.bodies view
        |> List.filter (fun creep -> creep.TicksToLive > 1)
        |> List.map (fun creep -> creep.Name)
        |> Set.ofList

    // Where each of ours stands this tick: the tile a loss is stamped with
    // next tick.
    let placedNow =
        view.Spatial.Rooms
        |> Map.toList
        |> List.collect (fun (room, layer) ->
            layer.CreepPositions
            |> Map.toList
            |> List.map (fun (name, pos) -> name, RoomPos.at room pos))
        |> Map.ofList

    // The hostiles standing in the room the defences are in: a window opened
    // by a raider a border away would charge this room's ordinary decay to a
    // raid that never touched it.
    let atHome =
        let home = SpatialInfo.homeName view.Spatial
        view.Hostiles |> List.filter (fun hostile -> hostile.Pos.Room = home)

    // This tick's hits across the Keep and the ramparts, the next tick's
    // baseline. The kinds are the rule's, never a list of ids: a rampart
    // raised mid-episode joins it the tick it stands. A child's rampart is
    // the child's to log (`SpatialInfo.placedAway`).
    let defended =
        SpatialInfo.structureHits view.Spatial
        |> List.choose (fun (id, kind, hits) ->
            if isDefence kind && not (SpatialInfo.placedAway view.Spatial id) then
                Some(id, hits.Hits)
            else
                None)
        |> Map.ofList

    // Hits lost since the previous tick's baseline: decreases summed,
    // increases ignored — a repair is not negative damage. A structure the
    // baseline does not carry costs nothing, which keeps a rampart raised
    // mid-raid from reading as damage on the tick it stands.
    let lostHits =
        defended
        |> Map.toList
        |> List.sumBy (fun (id, hits) ->
            match Map.tryFind id prior.Hits with
            | Some before when before > hits -> before - hits
            | _ -> 0)

    // Only the ring's last episode can still be open, and openness is the
    // quiet gap measured from its last sighting — never a stored flag.
    let earlier, current =
        match List.rev prior.Episodes with
        | last :: rest when view.Time - last.LastSeen <= gap -> List.rev rest, Some last
        | _ -> prior.Episodes, None

    // The tick's losses: names the previous tick projected that the world no
    // longer holds, stamped at the tick last seen alive. Differenced against
    // `alive` and never this colony's own names: an adopted body is still
    // standing.
    let lostSince (episode: RaidEpisode) =
        if view.Time - episode.LastSeen > 1 then
            []
        else
            let castIn room =
                prior.Placed
                |> Map.filter (fun name tile -> tile.Room = room && isFighterName name)
                |> Map.count

            Set.difference prior.Living alive
            |> Set.toList
            |> List.map (fun name ->
                let where = Map.tryFind name prior.Placed

                {
                    Creep = name
                    Tick = episode.LastSeen
                    Where = where
                    Cast =
                        where |> Option.map (fun tile -> castIn tile.Room) |> Option.defaultValue 0
                })

    // The hostiles in the rooms this colony works: a raid in a room it only
    // crosses is enrolled by the world (`Roster`) and opens nothing here.
    let worked =
        let rooms = workedRooms view declared
        view.Hostiles |> List.filter (fun hostile -> rooms.Contains hostile.Pos.Room)

    let episode =
        match current, worked with
        | None, [] -> None
        | current, hostiles ->
            let lost = current |> Option.map lostSince |> Option.defaultValue []

            // Damage is charged against the previous tick's baseline, and only
            // an episode already open has one, so nothing crosses the seam
            // between two episodes. Which ticks are inside the window is the
            // baseline's question, answered where it is carried.
            let damage = if Option.isSome current then lostHits else 0

            let episode =
                current
                |> Option.defaultValue
                    {
                        Opened = view.Time
                        LastSeen = view.Time
                        Rooms = Fresh.setOfSeq Seq.empty
                        Closest = None
                        Losses = []
                        Damage = 0
                    }

            Some
                { episode with
                    LastSeen =
                        if List.isEmpty hostiles then
                            episode.LastSeen
                        else
                            view.Time
                    Rooms =
                        if List.isEmpty hostiles then
                            episode.Rooms
                        else
                            Seq.append
                                episode.Rooms
                                (hostiles |> Seq.map (fun hostile -> hostile.Pos.Room))
                            |> Fresh.setOfSeq
                    Closest = nearer episode.Closest (approachAt view hostiles)
                    Losses = episode.Losses @ lost
                    Damage = episode.Damage + damage
                }

    // The holds, on the latch's two rules (a tick with vision decides a
    // room either way, a tick without leaves the last conclusion), ending
    // themselves on the tick they named. An expired entry is dropped here
    // rather than filtered by every reader, so the leaf cannot accumulate
    // holds that ended hours ago.
    let holds =
        let standing = prior.Holds |> Map.filter (fun _ hold -> view.Time < hold.Until)

        (standing, view.RoomControl)
        ||> Map.fold (fun rooms room control ->
            match RoomControlInfo.heldByOther control with
            | Some held ->
                Map.add
                    room
                    {
                        Holder = held.Holder
                        Until = view.Time + held.TicksToEnd
                    }
                    rooms
            | None -> Map.remove room rooms)

    // A rival's hold is a clock of its own (`rivalDeadlines`), and a raid's
    // going cuts only the raid's.
    let rivalHolds =
        holds
        |> Map.filter (fun room hold ->
            hold.Holder = ReservationHolder.Rival && not (Set.contains room view.HarassCast))
        |> Map.map (fun _ hold -> hold.Until)

    {
        Episodes =
            match episode with
            | Some episode -> earlier @ [ episode ] |> trim cap
            | None -> earlier
        // A tick that sees nothing (no vision, or a clear room) leaves every
        // stand-down exactly as it found it, clock included.
        Outposts =
            (endEnemyHeld view prior.Outposts, deadlines view looking priced)
            ||> List.fold (fun episodes seen ->
                sight view.Tuning.RaidBounceTicks view.Time seen episodes)
            |> strikeGone looking priced rivalHolds
            |> trimOutposts cap view.Time
        // The clockless withdrawal's memory, moved by the ticks with vision
        // alone: a room with no `RoomControl` entry keeps whatever the last
        // look concluded. A look the gate handed out this tick is a look
        // taken, so `LastLooked` moves whether or not vision answered; which
        // ticks those are is `lookDue`'s to say, the same function the gate
        // asks, so the record cannot disagree with the look it records.
        RivalHeld =
            let looked =
                prior.RivalHeld
                |> Map.map (fun _ latch ->
                    if lookDue view.Tuning view.Time latch then
                        { latch with LastLooked = view.Time }
                    else
                        latch)

            (looked, view.RoomControl)
            ||> Map.fold (fun rooms room control ->
                if rivalOwned control then
                    if Map.containsKey room rooms then
                        rooms
                    else
                        Map.add
                            room
                            {
                                Since = view.Time
                                LastLooked = view.Time
                            }
                            rooms
                else
                    Map.remove room rooms)
        Holds = holds
        // The guard row's memory of a raid, on `Holds`' two rules; a
        // `RoomControl` entry is exactly "the colony looked into this room
        // this tick" (`World.ofGame`). The clock is a backstop, not a
        // schedule: while the room is dark an expiry can only mean "we stopped
        // remembering", and a row that expired early read a room full of
        // invader as clear (#369). Declared outposts alone, off the guard
        // row's own derivation: no reader could act on a memory for any other
        // room.
        Threatened =
            let standing =
                prior.Threatened |> Map.filter (fun _ latch -> view.Time < latch.Until)

            let declared = Set.ofList declared

            let armedIn room =
                view.Hostiles
                |> List.exists (fun hostile ->
                    hostile.Pos.Room = room && HostileInfo.armed view.Hostiles hostile)

            (standing, view.RoomControl)
            ||> Map.fold (fun rooms room _ ->
                if not (Set.contains room declared) then
                    rooms
                elif armedIn room then
                    Map.add
                        room
                        {
                            Until = view.Time + view.Tuning.ThreatMemory
                        }
                        rooms
                else
                    Map.remove room rooms)
        Fought = foldFights view prior.Fought
        // `foldProbes`', which reads the tick's intents as well.
        Probes = prior.Probes
        Taken = foldTaken view prior.Taken
        ReserveShort = Decide.Facts.belowReserve view
        DigOpen = Some(Decide.Facts.oresDigOpen view)
        HaulOverflow = prior.HaulOverflow
        HomeRaided =
            if Decide.Facts.raided view (SpatialInfo.homeName view.Spatial) then
                Some view.Time
            else
                prior.HomeRaided
        Living = if Option.isSome episode then surviving else Set.empty
        Placed = if Option.isSome episode then placedNow else Map.empty
        // The damage baseline, carried on the condition the damage is charged
        // on: an open episode and a hostile in the room the Keep stands in.
        Hits =
            if Option.isSome episode && not (List.isEmpty atHome) then
                defended
            else
                Map.empty
    }

/// The probe log's fold (#493), off this tick's view and the intents decided
/// on it: a probe's first dismantle on one of its assault room's targets
/// opens the room's log; each decided tick after counts its dismantles, and
/// the light ticks' replays of one, until safe mode is seen in the room, and
/// settles its fate — out of the room alive once safe mode is up, or gone,
/// killed or of age. A settled log stands until the next probe's first
/// dismantle there. A log still probing is moved only by the colony whose
/// probe it is (`ProbeLog.Home`). Built by `Fresh`: the map is carried to the
/// next tick.
let foldProbes (view: ColonyView) (intents: Intent list) (state: RaidState) : RaidState =
    let alive =
        ColonyView.bodies view
        |> List.map (fun creep -> creep.Name, creep)
        |> Map.ofList

    let expiresOf (creep: CreepInfo) = view.Time + creep.TicksToLive
    let home = SpatialInfo.homeName view.Spatial

    let foldRoom (probes: Map<string, ProbeLog>) (facts: AssaultFacts) =
        let room = facts.Assault.RoomName

        let targets = facts.Targets |> Option.defaultValue [] |> List.map fst |> Set.ofList

        // The probes that dismantled one of the room's targets this tick.
        let hitters =
            intents
            |> List.choose (function
                | DismantleStructure(name, id) when
                    Set.contains id targets && squadRoleByName name = Some Probe
                    ->
                    Some name
                | _ -> None)

        match Map.tryFind room probes with
        // Another colony's probe, still probing: its log is that colony's to
        // move, and this colony, not holding the body, would read it dead.
        | Some({ Fate = Probing } as log) when log.Home <> "" && log.Home <> home -> probes
        | Some({ Fate = Probing } as log) ->
            let next =
                match Map.tryFind log.Probe alive with
                | None ->
                    { log with
                        HitAt = None
                        Fate =
                            if view.Time < log.Expires then
                                Died view.Time
                            else
                                Expired view.Time
                    }
                | Some creep ->
                    let safeModeAt =
                        log.SafeModeAt
                        |> Option.orElse (if facts.SafeMode then Some view.Time else None)

                    let hit = Option.isNone safeModeAt && List.contains log.Probe hitters

                    // Every tick since the last decided one was light, and
                    // replayed that tick's dismantle.
                    let replayed =
                        log.HitAt
                        |> Option.map (fun at -> view.Time - at - 1)
                        |> Option.defaultValue 0

                    let out =
                        Option.isSome safeModeAt
                        && SpatialInfo.creepPlacementOf view.Spatial log.Probe
                           |> Option.map (fun tile -> tile.Room)
                           <> Some room

                    { log with
                        Hits = log.Hits + replayed + (if hit then 1 else 0)
                        HitAt = if hit then Some view.Time else None
                        SafeModeAt = safeModeAt
                        Fate = if out then Out view.Time else Probing
                        Expires = expiresOf creep
                    }

            Map.add room next probes
        | _ ->
            match hitters with
            | name :: _ ->
                Map.add
                    room
                    {
                        Probe = name
                        FirstHit = view.Time
                        Hits = 1
                        HitAt = Some view.Time
                        SafeModeAt = None
                        Fate = Probing
                        Expires =
                            Map.tryFind name alive
                            |> Option.map expiresOf
                            |> Option.defaultValue view.Time
                        Home = home
                    }
                    probes
            | [] -> probes

    match view.Assaults with
    | [] -> state
    | assaults ->
        { state with
            Probes = List.fold foldRoom state.Probes assaults |> Map.toSeq |> Fresh.mapOfSeq
        }

/// One room's entry in the world [[danger log]]: its stand-down row, somebody
/// else's hold on its controller, the memory of an armed threat in it, its
/// exit watch, its fight record, and, an assault room, whether it was taken
/// and its last probe. One record a room, whichever colony concluded each
/// part; a part is None while nothing holds it: the world layer's
/// (`WorldView`) memory.
type DangerRoom =
    {
        Down: OutpostEpisode option
        Hold: OutpostHold option
        Threat: ThreatLatch option
        /// The exit watch (`World.watchExits`, #450), written beside the rest
        /// so a reset keeps a held exit (#542).
        Watch: ExitWatch option
        /// The resident room's fight record (`foldFights`): a raised child's
        /// home's is the mother's to fold and the child's to read.
        Fight: FightRecord option
        /// The tick an assault room's targets were first seen all down
        /// (`foldTaken`).
        Taken: int option
        /// An assault room's last probe (`foldProbes`).
        Probe: ProbeLog option
    }

/// The world danger log: one `DangerRoom` a room, and the rival latches
/// beside it, which the shell files under `observe.rooms.rivals`.
type Danger =
    {
        Rooms: Map<string, DangerRoom>
        Rivals: Map<string, RivalLatch>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Danger =
    let empty =
        {
            Rooms = Fresh.mapOfSeq Seq.empty
            Rivals = Fresh.mapOfSeq Seq.empty
        }

    /// A room the log holds nothing for.
    let quiet =
        {
            Down = None
            Hold = None
            Threat = None
            Watch = None
            Fight = None
            Taken = None
            Probe = None
        }

    /// Whether the log holds anything for a room: a room holding nothing is
    /// dropped.
    let held (entry: DangerRoom) =
        Option.isSome entry.Down
        || Option.isSome entry.Hold
        || Option.isSome entry.Threat
        || Option.isSome entry.Watch
        || Option.isSome entry.Fight
        || Option.isSome entry.Taken
        || Option.isSome entry.Probe

    let private entryOf (danger: Danger) (room: string) =
        Map.tryFind room danger.Rooms |> Option.defaultValue quiet

    let private partsOf (pick: DangerRoom -> 'a option) (danger: Danger) : Map<string, 'a> =
        danger.Rooms
        |> Map.toSeq
        |> Seq.choose (fun (room, entry) -> pick entry |> Option.map (fun part -> room, part))
        |> Fresh.mapOfSeq

    /// A colony's own log with the world's danger laid in: every row, hold,
    /// threat, latch, fight record, taken room and probe, whoever concluded
    /// it. What its stand-down gate is read off (`standDown`): one log, so
    /// every colony's gate agrees on a room.
    let into (danger: Danger) (own: RaidState) : RaidState =
        { own with
            Outposts =
                danger.Rooms
                |> Map.toList
                |> List.choose (fun (_, entry) -> entry.Down)
                |> List.sortBy (fun row -> row.Opened, row.RoomName)
            RivalHeld = danger.Rivals
            Holds = partsOf _.Hold danger
            Threatened = partsOf _.Threat danger
            Fought = partsOf _.Fight danger
            Probes = partsOf _.Probe danger
            Taken = partsOf _.Taken danger
        }

    /// The rooms with a folded log's holds, threats, fight records, taken
    /// rooms and probes laid back in, each part replaced by the log's: the
    /// world facts a colony moves only for the rooms it saw, so the order the
    /// colonies fold in does not matter for them.
    let private relay (folded: RaidState) (rooms: Map<string, DangerRoom>) =
        Seq.concat
            [
                Map.keys rooms
                Map.keys folded.Holds
                Map.keys folded.Threatened
                Map.keys folded.Fought
                Map.keys folded.Taken
                Map.keys folded.Probes
            ]
        |> Seq.distinct
        |> Seq.choose (fun room ->
            let entry =
                { (Map.tryFind room rooms |> Option.defaultValue quiet) with
                    Hold = Map.tryFind room folded.Holds
                    Threat = Map.tryFind room folded.Threatened
                    Fight = Map.tryFind room folded.Fought
                    Taken = Map.tryFind room folded.Taken
                    Probe = Map.tryFind room folded.Probes
                }

            if held entry then Some(room, entry) else None)
        |> Fresh.mapOfSeq

    /// Two standing rows of one room as one: the later expiry and its basis, a
    /// bunker either saw (a wrong merge reopens a stronghold room), the earlier
    /// opening and the later sighting, every raid body either waits on, the
    /// earlier player's raid, a chase or a bounce either read.
    let private mergeDown (a: OutpostEpisode) (b: OutpostEpisode) =
        let later = if b.Expiry > a.Expiry then b else a

        { later with
            Opened = min a.Opened b.Opened
            LastSeen = max a.LastSeen b.LastSeen
            Stronghold = a.Stronghold || b.Stronghold
            Raiders = Seq.append a.Raiders b.Raiders |> Fresh.setOfSeq
            PlayerSince =
                match a.PlayerSince, b.PlayerSince with
                | Some x, Some y -> Some(min x y)
                | x, None -> x
                | None, y -> y
            Chased = a.Chased || b.Chased
            Bounced = a.Bounced || b.Bounced
            HolderUnread = a.HolderUnread || b.HolderUnread
        }

    /// Several logs' rival latches as one: every room any latched, on its
    /// earliest shut and its earliest look, so the merged stride is never
    /// further off than any colony's.
    let mergeRivals (latches: Map<string, RivalLatch> list) : Map<string, RivalLatch> =
        latches
        |> List.collect Map.toList
        |> List.groupBy fst
        |> List.map (fun (room, latches) ->
            let latches = latches |> List.map snd

            room,
            {
                Since = latches |> List.map _.Since |> List.min
                LastLooked = latches |> List.map _.LastLooked |> List.min
            })
        |> Fresh.mapOfList

    /// The one row a room keeps of several: its standing rows merged, else the
    /// last to expire.
    let private oneRow tick (rows: OutpostEpisode list) =
        match rows |> List.filter (standingDown tick) with
        | [] -> rows |> List.maxBy (fun row -> row.Expiry, row.Opened)
        | standing -> standing |> List.reduce mergeDown

    /// Several logs' maps of one part as one: each room's entry with the
    /// greatest `by`.
    let private latest (by: 'a -> int) (maps: Map<string, 'a> list) =
        maps
        |> List.collect Map.toList
        |> List.groupBy fst
        |> List.map (fun (room, parts) -> room, parts |> List.map snd |> List.maxBy by)

    /// Several colony logs merged into one world log — how an arena's first
    /// tick reads the danger it seeds in a colony's log: each room's standing
    /// rows merged (`mergeDown`: the max expiry, the OR of the stronghold bit),
    /// else its latest; the later hold and threat; every latch, on its earliest
    /// shut and earliest look; each room's latest fight record, earliest taken
    /// tick and latest probe. A ring's bounce is read off it before its rows
    /// merge.
    let ofColonies (tuning: Tuning) (tick: int) (logs: RaidState list) : Danger =
        let bounced (ring: OutpostEpisode list) (row: OutpostEpisode) =
            { row with
                Bounced =
                    row.Bounced
                    || ring
                       |> List.exists (fun earlier ->
                           earlier.RoomName = row.RoomName
                           && earlier.Basis = StandDownBasis.RaidGone
                           && earlier.Opened < row.Opened
                           && row.Opened - earlier.Expiry <= tuning.RaidBounceTicks)
            }

        let downs =
            logs
            |> List.collect (fun log -> log.Outposts |> List.map (bounced log.Outposts))
            |> List.groupBy _.RoomName
            |> List.map (fun (room, rows) -> room, oneRow tick rows)

        let holds = logs |> List.map _.Holds |> latest _.Until
        let threats = logs |> List.map _.Threatened |> latest _.Until

        let rooms =
            let set (room: string) (fill: DangerRoom -> DangerRoom) rooms =
                Map.add room (fill (Map.tryFind room rooms |> Option.defaultValue quiet)) rooms

            let rooms =
                (Map.empty, downs)
                ||> List.fold (fun rooms (room, row) ->
                    set room (fun entry -> { entry with Down = Some row }) rooms)

            let rooms =
                (rooms, holds)
                ||> List.fold (fun rooms (room, hold) ->
                    set room (fun entry -> { entry with Hold = Some hold }) rooms)

            let lay (parts: (string * 'a) list) (fill: 'a -> DangerRoom -> DangerRoom) rooms =
                (rooms, parts)
                ||> List.fold (fun rooms (room, part) -> set room (fill part) rooms)

            (rooms, threats)
            ||> List.fold (fun rooms (room, threat) ->
                set room (fun entry -> { entry with Threat = Some threat }) rooms)
            |> lay (logs |> List.map _.Fought |> latest _.Seen) (fun fight entry ->
                { entry with Fight = Some fight })
            |> lay (logs |> List.map _.Taken |> latest (fun tick -> -tick)) (fun tick entry ->
                { entry with Taken = Some tick })
            |> lay (logs |> List.map _.Probes |> latest _.FirstHit) (fun probe entry ->
                { entry with Probe = Some probe })

        {
            Rooms = rooms |> Map.toSeq |> Fresh.mapOfSeq
            Rivals = logs |> List.map _.RivalHeld |> mergeRivals
        }

    /// The rooms of the standing raid rows a colony prices (`Facts.pricedRooms`
    /// over the rooms it declares, `owned`), off its own log with the world's
    /// laid in (`into`): the rows its fold re-reads off the world's look
    /// (`foldColony`). The priced set is read only where a raid row stands.
    let outOfSight (owned: Set<string>) (view: ColonyView) (laid: RaidState) =
        match pricedRaidRows (fun _ -> true) view.Time laid.Outposts with
        | [] -> []
        | rows ->
            let prices = Decide.Facts.pricedRooms owned view
            rows |> List.filter prices.Contains

    /// One colony's Raid-log fold over the world log: its own log with the
    /// world's laid in, less the standing raid rows of rooms it does not price
    /// (`Facts.pricedRooms` over the rooms it declares, `owned`), folded as its own (`foldRaids`)
    /// and written back. A row it opens in a room whose row it was not handed
    /// (a core or a reservation it saw there) merges with the world's
    /// (`mergeDown`); the holds, threats, latches, fight records and taken
    /// rooms come back whole, each colony moving only the rooms it saw (the
    /// probes are `probing`'s). Threaded through the colonies in
    /// order, so each folds what the one before wrote. The colony's log comes
    /// back with the rows laid in as it folded them.
    let foldColony
        (cap: int)
        (alive: Set<string>)
        (owned: Set<string>)
        (view: ColonyView)
        (declared: string list)
        (own: RaidState)
        (danger: Danger)
        : RaidState * Danger =
        let laid = into danger own
        let priced = outOfSight owned view laid |> Set.ofList

        let prior =
            { laid with
                Outposts =
                    laid.Outposts
                    |> List.filter (fun row ->
                        priced.Contains row.RoomName
                        || not (standingDown view.Time row && raidClocked row))
            }

        let handed = prior.Outposts |> List.map _.RoomName |> Set.ofList
        let folded = foldRaids cap alive view declared prior

        // The ring's last row of each room: one it opened is appended after
        // the spent one it replaces.
        let downs =
            (Map.empty, folded.Outposts)
            ||> List.fold (fun downs row -> Map.add row.RoomName row downs)

        let rooms =
            (danger.Rooms, downs)
            ||> Map.fold (fun rooms room row ->
                let entry = entryOf danger room

                let row =
                    match entry.Down with
                    | Some standing when
                        not (handed.Contains room) && standingDown view.Time standing
                        ->
                        mergeDown standing row
                    | _ -> row

                Map.add room { entry with Down = Some row } rooms)

        folded,
        {
            Rooms = relay folded rooms
            Rivals = folded.RivalHeld |> Map.toSeq |> Fresh.mapOfSeq
        }

    /// The probe log's fold (`foldProbes`) over a colony's log as
    /// `foldColony` handed it back, its probes laid back into the world's.
    let probing
        (view: ColonyView)
        (intents: Intent list)
        (state: RaidState, danger: Danger)
        : RaidState * Danger =
        let probed = foldProbes view intents state

        probed,
        { danger with
            Rooms = relay probed danger.Rooms
        }

    /// The log with this tick's exit watches laid in, every room's replaced.
    let watching (watches: Map<string, ExitWatch>) (danger: Danger) : Danger =
        { danger with
            Rooms =
                Seq.append (Map.keys danger.Rooms) (Map.keys watches)
                |> Seq.distinct
                |> Seq.choose (fun room ->
                    let entry =
                        { entryOf danger room with
                            Watch = Map.tryFind room watches
                        }

                    if held entry then Some(room, entry) else None)
                |> Fresh.mapOfSeq
        }

    /// Every room's exit watch: what a cold heap starts from.
    let watches (danger: Danger) : Map<string, ExitWatch> = partsOf _.Watch danger

    /// The log's rows past the cap, the oldest spent first and never one
    /// still standing (`trimOutposts`' rule, over the one world ring).
    let trim (cap: int) (tick: int) (danger: Danger) : Danger =
        let rows =
            danger.Rooms
            |> Map.toList
            |> List.choose (fun (_, entry) -> entry.Down)
            |> List.sortBy (fun row -> row.Opened, row.RoomName)

        let kept = trimOutposts cap tick rows |> List.map _.RoomName |> Set.ofList

        if List.length rows = Set.count kept then
            danger
        else
            { danger with
                Rooms =
                    danger.Rooms
                    |> Map.toSeq
                    |> Seq.choose (fun (room, entry) ->
                        let entry =
                            if kept.Contains room then
                                entry
                            else
                                { entry with Down = None }

                        if held entry then Some(room, entry) else None)
                    |> Fresh.mapOfSeq
            }

    /// The rooms the log holds danger in at this tick: a standing row, a
    /// threat not yet run out, or an exit held.
    let standing (tick: int) (danger: Danger) : Set<string> =
        danger.Rooms
        |> Map.toSeq
        |> Seq.filter (fun (_, entry) ->
            entry.Down |> Option.exists (standingDown tick)
            || entry.Threat |> Option.exists (fun threat -> tick < threat.Until)
            || entry.Watch
               |> Option.exists (function
                   | ExitWatch.Held hold -> ExitHold.stands tick hold
                   | ExitWatch.Seen threats ->
                       threats |> List.exists (fun threat -> threat.Dies > tick)))
        |> Seq.map fst
        |> Set.ofSeq

/// The world [[raid roster]]: every hostile our vision saw, by room and then
/// by id, whichever colony's look it was. A Source Keeper is the room's
/// terrain and an ally no raider, so neither is enrolled. A colony's episode
/// names the rooms it works that its raid stood in (`RaidEpisode.Rooms`),
/// and its roster is these rows in those rooms inside its window.
type Roster = Map<string, Map<string, RosterRow>>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Roster =
    let empty: Roster = Fresh.mapOfSeq Seq.empty

    /// This tick's sightings and tombstones folded in: a hostile's first
    /// sighting in a room writes its row there (the body that entered, not
    /// what the tower left of it), every later one moves its last tick and
    /// the tick its life runs out on; a tombstone stamps every row of its id
    /// once, on the tick first seen.
    let fold (tick: int) (watch: WorldView) (prior: Roster) : Roster =
        let sighted =
            watch.Rooms
            |> Map.toList
            |> List.collect (fun (room, seen) ->
                seen.Hostiles
                |> List.filter (fun hostile ->
                    not (Colony.isAlly hostile.Owner) && not (HostileInfo.isKeeper hostile))
                |> List.map (fun hostile -> room, hostile))

        let fallen =
            watch.Rooms
            |> Map.toSeq
            |> Seq.collect (fun (_, seen) -> seen.Fallen)
            |> Fresh.setOfSeq

        if List.isEmpty sighted && Set.isEmpty fallen then
            prior
        else
            let enrolled =
                (prior, sighted)
                ||> List.fold (fun roster (room, hostile) ->
                    let rows = Map.tryFind room roster |> Option.defaultValue Map.empty
                    let dies = tick + hostile.TicksToLive

                    let row =
                        match Map.tryFind hostile.Id rows with
                        | Some row -> { row with Last = tick; Dies = dies }
                        | None ->
                            {
                                Owner = hostile.Owner
                                Body = partsOf hostile.Body
                                First = tick
                                Last = tick
                                Dies = dies
                                Fell = None
                            }

                    Map.add room (Map.add hostile.Id row rows) roster)

            enrolled
            |> Map.toSeq
            |> Seq.map (fun (room, rows) ->
                room,
                rows
                |> Map.toSeq
                |> Seq.map (fun (id, row) ->
                    id,
                    if Option.isNone row.Fell && fallen.Contains id then
                        { row with Fell = Some tick }
                    else
                        row)
                |> Fresh.mapOfSeq)
            |> Fresh.mapOfSeq

    /// The most rows a room keeps past the quiet gap (`prune`): an episode
    /// open for eleven thousand ticks beside a neighbour's hauler lane would
    /// otherwise keep every hauler that ever passed.
    let roomCap = 50

    /// Whether a row is worth keeping once its raid has gone: a body that
    /// could hurt or heal, or one that fell.
    let private worthKeeping (row: RosterRow) =
        Option.isSome row.Fell
        || [ Attack; RangedAttack; Heal ]
           |> List.exists (fun part -> Map.tryFind part row.Body |> Option.defaultValue 0 > 0)

    /// The rows seen inside the quiet gap, which an episode may yet open on,
    /// all kept; and of the rest, the ones some colony's episode still reads
    /// (a room it names, seen inside its window) that could hurt, heal or
    /// fell, the latest `roomCap` a room. Every other row is dropped. So the
    /// roster is bounded by the colonies' episode rings and the cap, and a
    /// transit room's raid leaves no row once it has gone.
    let prune (tick: int) (gap: int) (episodes: RaidEpisode list) (roster: Roster) : Roster =
        let windows =
            episodes
            |> List.collect (fun episode ->
                episode.Rooms
                |> Set.toList
                |> List.map (fun room -> room, (episode.Opened, episode.LastSeen)))
            |> List.groupBy fst
            |> List.map (fun (room, spans) -> room, List.map snd spans)
            |> Map.ofList

        roster
        |> Map.toSeq
        |> Seq.choose (fun (room, rows) ->
            let spans = Map.tryFind room windows |> Option.defaultValue []

            let read (row: RosterRow) =
                spans
                |> List.exists (fun (opened, last) -> row.First <= last && row.Last >= opened)

            let recent, older =
                rows |> Map.toList |> List.partition (fun (_, row) -> tick - row.Last <= gap)

            let kept =
                older
                |> List.filter (fun (_, row) -> worthKeeping row && read row)
                |> List.sortByDescending (fun (id, row) -> row.Last, id)
                |> List.truncate roomCap
                |> List.append recent
                |> Fresh.mapOfList

            if Map.isEmpty kept then None else Some(room, kept))
        |> Fresh.mapOfSeq

/// One reading of `Grid.Counters`: floods built, how many of them
/// free-origin, and heap pops. Cumulative where the shell reads it,
/// differenced per colony by `foldCpu`: what a `decide` spike with no replan
/// in it was doing.
type FloodCounts = { Floods: int; Free: int; Pops: int }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FloodCounts =
    let zero = { Floods = 0; Free = 0; Pops = 0 }

    let private less (a: FloodCounts) (b: FloodCounts) =
        {
            Floods = a.Floods - b.Floods
            Free = a.Free - b.Free
            Pops = a.Pops - b.Pops
        }

    /// The tick's pops over its colonies — the one sum a reader takes, for
    /// the span's worst tick.
    let totalPops (rows: (string * FloodCounts) list) =
        rows |> List.sumBy (fun (_, c) -> c.Pops)

    /// Cumulative readings per colony, in decision order, differenced
    /// against the one before — the first against zero, because the shell
    /// resets the counters before the first colony decides.
    let differenced (readings: (string * FloodCounts) list) =
        readings
        |> List.fold
            (fun (spent, at) (home, reading) -> (home, less reading at) :: spent, reading)
            ([], zero)
        |> fst
        |> List.rev

/// Why a would-be light tick ran full, and how much of it (#549): the first
/// fact as `LightTick.tag` spells it, how many facts forced it, and how many
/// colonies replayed their plans instead of deciding — a partial tick's mean
/// is not a whole one's — and how many decided their fight alone and
/// replayed their economy, a fight-only tick (`LightTick.fightOnly`). And
/// the ms of its `decide` phase the replay and the one movement pass spent
/// after the deciding colonies decided (#581), zero where nothing replayed:
/// the rest of the phase is the decides, a fight-only tick's the fight lane.
type Forcing =
    {
        Reason: string
        Facts: int
        Replayed: int
        FightOnly: int
        ReplayMs: float
    }

/// What `Game.cpu.getUsed()` answered at each of the loop's phase boundaries,
/// plus the intents the engine accepted. Cumulative, every one of them; the
/// differencing happens once, here. `AtEntry` is what the engine had already
/// spent before `loop` was entered.
type CpuReadings =
    {
        AtEntry: float
        AtSnapshot: float
        AtDecide: float
        AtSave: float
        AtExecute: float
        Intents: int
        /// `Game.cpu.bucket` as the tick ended: the margin. The ceiling does
        /// not slide down with the bucket (`docs/research/fable-screeps.md`),
        /// so this is a countdown to when the ceiling moves at all.
        Bucket: int
        /// How many colonies threw their plan memo away this tick. Carried
        /// rather than inferred, because a `decide` six times its mean is
        /// either a replan or a pricing storm.
        Replans: int
        /// What each colony spent inside `decide`, in decision order. The
        /// `Decide` phase cannot say which colony a spike came out of, and
        /// every CPU refusal this bot has recorded was measured on the wrong
        /// shape (#332's keeper mask read 0.00% on the one harness world with
        /// no keeper room). Cumulative, differenced by `foldCpu`.
        ColonyDecides: (string * float) list
        /// The counter as each room's facts finished, in sweep order. The
        /// harness's stub rooms cannot price a millisecond of `snapshot`, so
        /// this split is the only reading of that phase the two compare on.
        RoomSnapshots: (string * float) list
        /// The counter as the sweep began. The rooms sum to less than the
        /// phase, and the difference is readable the way `decide`'s remainder
        /// is.
        AtRooms: float
        /// The boundary before the first colony's projection, and the readings
        /// after each: on the first window that could see it the projections
        /// were the larger part of `snapshot` (7.63 ms of 14.9, #370).
        AtProjects: float
        ColonyProjects: (string * float) list
        /// `Grid.Counters` as each colony finished deciding, cumulative from
        /// the tick's reset.
        ColonyFloods: (string * FloodCounts) list
        /// The isolate's used V8 heap as the tick ended, in MB, and the rows
        /// standing in every plan memo's three tables (#391): the two
        /// readings that tell an hour-long climb a reset cures — heap state
        /// accumulating — from a slow host, which the phase split cannot.
        HeapMb: float
        MemoRows: int
        /// `externally_allocated_size` in MB (#393): where a flood field's
        /// `Int32Array` is counted, and not in `HeapMb`.
        ExternalMb: float
        /// Whether this was a light tick, one that replayed the last full
        /// tick's decision. Its phases that did not run read zero-width.
        Light: bool
        /// Why a tick that could have been light was full (`Forcing`); None
        /// for a light tick, and for a full tick the cadence made full (after
        /// a light tick, or a reset).
        Forced: Forcing option
        /// Each colony that re-planned this tick and what moved its signature
        /// (#463, `signatureMoves`): rooms by name, the head's parts as
        /// `home`, `level` or `stages`, and `reset` for a plan paid with no
        /// prior memo.
        Moved: (string * string list) list
    }

/// One tick's cost, split at the loop's phase boundaries, and the count of
/// intents the engine accepted. The split attributes a gap the ruler cannot
/// see: the harness measured 10.45 ms/tick where the live line read a 49.4 ms
/// mean (#170), and the harness has no engine (no 0.2 CPU per intent, no
/// prelude, no Memory parse).
type CpuPhases =
    {
        Entry: float
        Snapshot: float
        Decide: float
        Save: float
        Execute: float
        Intents: int
        Bucket: int
        Replans: int
    }

/// One tick's cost as the engine measured it. `Phases` is `None` only for a
/// legacy row; `foldCpu` never writes one. The tick number rides the row
/// because a tick the loop never finished writes no row, and the gap is the
/// one thing this line can say that a bare list cannot.
type CpuSample =
    {
        Tick: int
        Ms: float
        Phases: CpuPhases option
        /// What each colony spent inside `decide`. A list and not an option:
        /// the empty list is the honest answer for a reading nobody took and
        /// for a tick no colony decided in. Kept off `CpuPhases`, which decodes
        /// all-or-none, so growing it would unmeasure every row the previous
        /// bundle wrote.
        Colonies: (string * float) list
        /// Kept off `CpuPhases` for `Colonies`' reason.
        Rooms: (string * float) list
        /// What the sweep spent before its first room. Its own number because
        /// the first live window put it at 1.9 ms, more than any single room
        /// (#370).
        SweepHead: float
        /// Each colony's projection, differenced the way its decision is.
        Projects: (string * float) list
        /// What each colony's decision flooded, differenced the way `Colonies`
        /// is; empty for a legacy row.
        Floods: (string * FloodCounts) list
        /// The tick's pops over every colony: what the row keeps of `Floods`
        /// once it ages out of `capCpuDetail`.
        Pops: int
        /// Used heap in MB and memo table rows as the tick ended (#391); zero
        /// for a legacy row.
        HeapMb: float
        MemoRows: int
        /// The off-heap size in MB (#393); zero for a legacy row.
        ExternalMb: float
        /// A light tick's row; `false` for a full tick and a legacy row. Kept
        /// off `CpuPhases` for `Colonies`' reason.
        Light: bool
        /// Why a would-be light tick ran full (`CpuReadings.Forced`).
        Forced: Forcing option
        /// `CpuReadings.Moved`; empty on every row but a replan's.
        Moved: (string * string list) list
    }

/// One span of ticks, summarised: the coarse record beside the fine one,
/// cheap enough to keep for hours, for the question "what happened at three
/// o'clock" that the five-minute fine ring cannot answer (#386). `Max` is the
/// field this exists for: the engine's 500 ms ceiling is a wall, so a
/// post-mortem needs when a single tick got large before what the mean was.
type CpuSpan =
    {
        /// The first tick the span covers, and the last: a gap between two
        /// spans is ticks this bot did not run, which is itself the record.
        From: int
        To: int
        /// How many ticks were folded in — not `To - From + 1`, which counts
        /// the ticks the bot missed as though it had measured them.
        Ticks: int
        /// The worst single tick of the span, in milliseconds.
        Max: float
        /// Their sum, so a reader divides by `Ticks` for the mean and no
        /// rounding is carried across spans.
        Sum: float
        /// The lowest bucket seen. A span whose floor is 10,000 never spent
        /// more than the allowance; one whose floor is near zero is the shape
        /// an outage leaves behind.
        Bucket: int
        /// How many colonies re-planned across the span.
        Replans: int
        /// The most heap pops any one tick of the span ran, beside `Max` for
        /// its reason: an incident hours old has to say whether its worst tick
        /// was flooding.
        MaxPops: int
        /// The largest heap and memo table the span saw (#391): a climb over
        /// hours that a reset cures shows here as both rising span by span.
        MaxHeapMb: float
        MaxMemoRows: int
        /// The smallest heap the span saw (#393): the post-GC floor, which is
        /// the live set, where `MaxHeapMb` is the pre-GC peak V8 lets grow
        /// under allocation pressure. Zero until a measured tick folds in.
        MinHeapMb: float
        /// The largest off-heap size the span saw (#393).
        MaxExternalMb: float
        /// The phase sums, so a reader divides by `Ticks` for each mean: the
        /// fine ring's hundred rows had aged out of the 2026-09-21 storm hour
        /// before anyone read it, and nothing said which phase the hour went to.
        SnapshotSum: float
        DecideSum: float
        SaveSum: float
        ExecuteSum: float
    }

/// The whole persisted CPU line: oldest first, capped. A record so the leaf
/// can grow a second key without moving the one that is there.
type CpuState =
    {
        Ticks: CpuSample list
        /// Closed spans, oldest first, and the one still filling at the end.
        Spans: CpuSpan list
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CpuState =
    /// The empty CPU line — what an absent, malformed or foreign-shaped
    /// leaf reads as, the way `RaidState.empty` is.
    let empty = { Ticks = []; Spans = [] }

/// The CPU line's ring cap: long enough that the mean is not one tick's
/// opinion. A replan makes one tick several times its neighbours; at twenty
/// rows that is a twentieth of the mean, at a hundred it is a percent.
let capCpuTicks = 100

/// How many of the newest rows keep the per-name splits (colonies, rooms,
/// projections, floods): the rows `observe cpu` prints by default. The
/// per-room map on every row of the ring was most of the leaf (#538).
let capCpuDetail = 20

/// How many ticks one coarse span covers, and how many spans are kept: 20,000
/// ticks, which at the live rate of about 1,120 ticks an hour is five hours.
let spanTicks = 100

let capCpuSpans = 200

/// The measured cost, kept to the microsecond. The engine hands back a float
/// with more digits than anyone reads and Memory pays for every one of them; a
/// microsecond is finer than the profiler's own 100µs sampling interval, so
/// nothing a reader could act on is rounded away.
let private toMicrosecond (ms: float) = floor (ms * 1000.0 + 0.5) / 1000.0

/// Whether a row carries none of the per-name splits `capCpuDetail` keeps.
let private isBare (sample: CpuSample) =
    List.isEmpty sample.Colonies
    && List.isEmpty sample.Rooms
    && List.isEmpty sample.Projects
    && List.isEmpty sample.Floods
    && sample.SweepHead = 0.0

/// The ring with the row that just left the newest `capCpuDetail` made bare.
/// One row leaves a tick, and only it is rebuilt; a line loaded whole from an
/// older leaf keeps its older rows' splits until the ring shifts them off.
let private ageOutDetail (rows: CpuSample list) =
    let leaving = List.length rows - capCpuDetail - 1

    if leaving < 0 then
        rows
    else
        match List.splitAt leaving rows with
        | before, sample :: after when not (isBare sample) ->
            let bare =
                { sample with
                    Colonies = []
                    Rooms = []
                    SweepHead = 0.0
                    Projects = []
                    Floods = []
                }

            before @ (bare :: after)
        | _ -> rows

/// The CPU line's fold: this tick's cost joins the ring and the newest `cap`
/// rows survive. No change detection: a quiet tick still writes, the point
/// being the shape of the distribution. The thresholds live with the readers
/// (`scripts/cpu-trigger.mjs`), never here. The readings arrive cumulative
/// and are differenced here, once.
let foldCpu (cap: int) (tick: int) (readings: CpuReadings) (prior: CpuState) : CpuState =
    let phases =
        {
            // Not a difference: nothing of the bot's ran before it.
            Entry = toMicrosecond readings.AtEntry
            Snapshot = toMicrosecond (readings.AtSnapshot - readings.AtEntry)
            Decide = toMicrosecond (readings.AtDecide - readings.AtSnapshot)
            Save = toMicrosecond (readings.AtSave - readings.AtDecide)
            Execute = toMicrosecond (readings.AtExecute - readings.AtSave)
            Intents = readings.Intents
            Bucket = readings.Bucket
            Replans = readings.Replans
        }

    // Differenced against the boundary before each colony, the first against
    // the phase's own start. The sum is the `Decide` phase less what the tick
    // spent between colonies, which is why neither number is derived from the
    // other.
    let colonies =
        readings.ColonyDecides
        |> List.fold
            (fun (spent, at) (home, reading) ->
                (home, toMicrosecond (reading - at)) :: spent, reading)
            ([], readings.AtSnapshot)
        |> fst
        |> List.rev

    // The snapshot's rooms, differenced against the phase's own start — which
    // is the prelude's reading, `AtEntry`, because nothing runs between them.
    let swept =
        readings.RoomSnapshots
        |> List.fold
            (fun (spent, at) (room, reading) ->
                (room, toMicrosecond (reading - at)) :: spent, reading)
            ([], readings.AtRooms)
        |> fst
        |> List.rev

    let projects =
        readings.ColonyProjects
        |> List.fold
            (fun (spent, at) (home, reading) ->
                (home, toMicrosecond (reading - at)) :: spent, reading)
            ([], readings.AtProjects)
        |> fst
        |> List.rev

    let ms = toMicrosecond readings.AtExecute

    let floods = FloodCounts.differenced readings.ColonyFloods
    let pops = FloodCounts.totalPops floods
    // A tenth of a megabyte: finer than the climb this reads, and Memory
    // pays for every digit two hundred times.
    let heap = floor (readings.HeapMb * 10.0 + 0.5) / 10.0
    let external = floor (readings.ExternalMb * 10.0 + 0.5) / 10.0

    // The floor ignores an unmeasured tick, and a span an older bundle
    // opened starts its floor at the first measured tick.
    let floorOf (a: float) (b: float) =
        if a = 0.0 then b
        elif b = 0.0 then a
        else min a b

    // The span still filling is the last of the list; it closes at
    // `spanTicks`, and a tick behind the open span's (a stale leaf) opens a
    // fresh span rather than widening the old one over a window it did not
    // measure.
    let spans =
        match List.tryLast prior.Spans with
        | Some open' when open'.Ticks < spanTicks && tick >= open'.To ->
            List.truncate (List.length prior.Spans - 1) prior.Spans
            @ [
                { open' with
                    To = tick
                    Ticks = open'.Ticks + 1
                    Max = max open'.Max ms
                    Sum = toMicrosecond (open'.Sum + ms)
                    Bucket = min open'.Bucket readings.Bucket
                    Replans = open'.Replans + readings.Replans
                    MaxPops = max open'.MaxPops pops
                    MaxHeapMb = max open'.MaxHeapMb heap
                    MaxMemoRows = max open'.MaxMemoRows readings.MemoRows
                    MinHeapMb = floorOf open'.MinHeapMb heap
                    MaxExternalMb = max open'.MaxExternalMb external
                    SnapshotSum = toMicrosecond (open'.SnapshotSum + phases.Snapshot)
                    DecideSum = toMicrosecond (open'.DecideSum + phases.Decide)
                    SaveSum = toMicrosecond (open'.SaveSum + phases.Save)
                    ExecuteSum = toMicrosecond (open'.ExecuteSum + phases.Execute)
                }
            ]
        | _ ->
            prior.Spans
            @ [
                {
                    From = tick
                    To = tick
                    Ticks = 1
                    Max = ms
                    Sum = ms
                    Bucket = readings.Bucket
                    Replans = readings.Replans
                    MaxPops = pops
                    MaxHeapMb = heap
                    MaxMemoRows = readings.MemoRows
                    MinHeapMb = heap
                    MaxExternalMb = external
                    SnapshotSum = phases.Snapshot
                    DecideSum = phases.Decide
                    SaveSum = phases.Save
                    ExecuteSum = phases.Execute
                }
            ]
            |> trim capCpuSpans

    {
        Ticks =
            prior.Ticks
            @ [
                {
                    Tick = tick
                    Ms = ms
                    Phases = Some phases
                    Colonies = colonies
                    Rooms = swept
                    SweepHead = toMicrosecond (readings.AtRooms - readings.AtEntry)
                    Projects = projects
                    Floods = floods
                    Pops = pops
                    HeapMb = heap
                    MemoRows = readings.MemoRows
                    ExternalMb = external
                    Light = readings.Light
                    Forced =
                        readings.Forced
                        |> Option.map (fun forcing ->
                            { forcing with
                                ReplayMs = toMicrosecond forcing.ReplayMs
                            })
                    Moved = readings.Moved
                }
            ]
            |> trim cap
            |> ageOutDetail
        Spans = spans
    }

/// What one live invariant check found broken this tick. The channel exists
/// because the suite runs on fixtures this repo authors and `src/App/World.fs`
/// has no tests by construction: three of four Thorium incidents shipped green
/// through the untested half (#355). A closed vocabulary rather than a message
/// string: the kind crosses the wire and a kind added without its wire name
/// fails the build.
[<RequireQualifiedAccess>]
type BreachKind =
    /// Every tick of the CPU line's last few cadences ran full and a forcing
    /// condition forced them (`CpuSample.Forced`), and at their draw over the
    /// CPU limit the bucket stands empty within `Tuning.BucketHorizon`: no
    /// light tick ran, and the bucket cannot pay for it. World-wide; `Subject` is the forcing most of those
    /// ticks named, `Amount` how many it forced. The cause `BucketFalling` is
    /// the symptom of (#630), and ahead of it.
    | LightStarved
    /// The bucket, at its slope over the CPU line's ring, stands empty within
    /// `Tuning.BucketHorizon`. World-wide; `Amount` is the ticks until empty.
    | BucketFalling
    /// The fault ring holds a throw from this bundle's load (#628) no older
    /// than `faultOpenTicks`. `Room` is the colony it cost, empty for a tick
    /// or an intent; `Subject` the step that threw, `Amount` how many times
    /// since the load.
    | Fault
    /// A spawn free beside a full bank, casting nothing, while the spawn plan
    /// left seats a full bank cannot pay though their row's block fits it
    /// (`Quotas.Unfilled`): a pending row until it has stood the longest cast
    /// (`Window`). `Subject` is the spawn, `Amount` the seats.
    | SpawnIdle
    /// A body with no Task and not fleeing (a Flee is a Task) for a share of
    /// its own life (`Tuning.BodyIdlePercent`), read off the Transition log,
    /// while nothing holds it on purpose (a fighter no open ask wants is a
    /// reserve). `Subject` is the creep, `Amount` the ticks idle.
    | BodyIdle
    /// An actor of this colony's — a body, a spawn or another structure —
    /// the engine refused since the colony last decided (#629): a pending
    /// row until it has been refused on every decide for
    /// `Tuning.RefusalHalfLife` (`Window`), the price the Matcher and the
    /// spawn plan put on a refusal having moved nothing in that time.
    /// `Subject` is the actor, `Amount` its refusals' weight standing.
    | RefusalStorm
    /// Ore decaying in a room this colony projects and may sweep, with the
    /// object it is decaying in as the `Subject`: a pile, or a tombstone or
    /// ruin holding Thorium. One kind for both because a decaying tombstone
    /// drops its store as piles, and both ask the operator for one response.
    | OreOnTheFloor
    /// One of our creeps is standing at the declared Reactor holding ore the
    /// Reactor has no room for. The tick before the ore hits the floor: a
    /// transfer into a full store moves what fits and leaves the rest aboard,
    /// and a courier that cannot put its load down drops it.
    | OreUnplaceable
    /// A declared Reactor of ours whose store is empty. Not a catastrophe and
    /// not nothing: the continuous-work streak resets, and the programme falls
    /// back to 1 point per T.
    | ReactorStarved
    /// A declared Reactor of ours that still burns, with no courier able to
    /// reach it before it stops: `ReactorStarved` read while it can still be
    /// answered. The threshold is the lead time (cast plus walk), so it fires
    /// on the last tick an answer still works: a tick later saves nothing,
    /// and an earlier alarm hires a courier that stands at the Reactor
    /// burning its life for nothing (#361).
    | ReactorRunningDry
    /// A declared Reactor whose own row says it is not ours — somebody walked a
    /// CLAIM body in, or the re-claimer died before its relief arrived. Every
    /// tonne delivered while that stands scores for whoever holds the flag.
    | ReactorLost
    /// A nuke in flight at a room of ours (a home or a declared outpost),
    /// from first sight to landing (#657): `Room` is its target, `Subject`
    /// the nuke, `Amount` the ticks until it lands, and `Impact` what of ours
    /// stands in it. World-wide, being read for every room of ours on every
    /// full tick whichever colony decided; a target out of sight keeps its
    /// row until the landing tick it was seen with. Observation only.
    | IncomingNuke

/// One structure standing within a nuke's impact area, as the shell read it.
///
/// Field names here are unique in the codebase on purpose: a record declared
/// later with `Id`, `X` or `Hits` would take over those labels' inference
/// from every record declared before it.
type NukeTarget =
    {
        /// The structure's id.
        Structure: string
        /// The engine's STRUCTURE_* spelling.
        Built: string
        Square: Pos
        HitsLeft: int
    }

/// A nuke in flight seen in a room of ours (`FIND_NUKES`), with what of ours
/// stands to take it.
type NukeSighting =
    {
        /// The nuke's id.
        Nuke: string
        /// The room it is aimed at.
        Aimed: string
        Centre: Pos
        /// The tick it lands on: this tick plus its `timeToLand`.
        LandsAt: int
        /// The room it was launched from, empty when the engine names none.
        Launch: string
        /// Every structure within `Engine.nukeRadius` of the centre that no
        /// rival owns, ramparts included.
        Inside: NukeTarget list
        /// Our creeps standing anywhere in the room: the landing kills every
        /// creep in the room, not only those inside the area.
        Doomed: string list
    }

/// The nukes in flight at the rooms of ours, and which of those rooms were in
/// sight to be read.
type NukeWatch =
    {
        /// Every home and declared outpost.
        Covered: Set<string>
        /// The rooms of `Covered` in vision this tick.
        Sighted: Set<string>
        Nukes: NukeSighting list
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NukeWatch =
    let none =
        {
            Covered = Set.empty
            Sighted = Set.empty
            Nukes = []
        }

/// One structure inside a nuke's impact area, priced: the damage its tile
/// takes (`Blast`), the rampart hits standing on that tile before it
/// (`Rampart`), and the hits a rampart there still lacks to keep the
/// structure standing (`Shortfall`, 0: it stands). A rampart is its own
/// shield: its row reads `Rampart` 0 and lacks what its own hits fall short
/// of the blast.
type NukeExposed =
    {
        Exposed: NukeTarget
        Blast: int
        Rampart: int
        Shortfall: int
    }

/// What an incoming nuke will do: its centre, landing tick and launch room,
/// each structure of ours inside the 5×5 priced, and the creeps of ours in
/// the room it will kill if they stand there when it lands.
type NukeImpact =
    {
        GroundZero: Pos
        Lands: int
        Launcher: string
        Priced: NukeExposed list
        Killed: string list
    }

/// One violation as this tick reads it: what broke, where, on which engine
/// object, and the one number that makes it actionable — T on the floor, T
/// that cannot be placed, and 0 for the two kinds whose whole content is the
/// fact itself.
///
/// `Subject` is the **engine id** (a pile, a creep name, a Reactor) and never a
/// description, because it is the fold's identity: the same pile seen on two
/// ticks has to be one row growing older rather than two rows.
type Breach =
    {
        Kind: BreachKind
        Room: string
        Subject: string
        Amount: int
        /// The ticks the condition stands before the row is open (`isOpen`):
        /// 0 for a kind whose fact already carries its own duration, and the
        /// row before that is pending, folded for its age alone.
        Window: int
        /// An `IncomingNuke` row's impact; None on every other kind.
        Impact: NukeImpact option
    }

/// One breach as the log carries it. The age is the whole reason this channel
/// folds: whether a pile is bleeding or a courier is three ticks from it.
/// `LastSeen` is always the current tick (a breach that stops appearing drops
/// out), kept because `observe.mjs breaches` reads the leaf hours later with
/// no game clock of its own.
type StandingBreach =
    {
        FirstSeen: int
        LastSeen: int
        Breach: Breach
    }

/// The whole persisted breach log: one row per (kind, subject) standing right
/// now. A map and not a ring: this channel answers "what is broken now" and
/// nothing else, and the Raid log is where episodic history lives.
type BreachState =
    {
        Standing: Map<BreachKind * string, StandingBreach>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BreachState =
    /// The empty breach log, which is also the healthy steady state.
    let empty = { Standing = Map.empty }

/// The breach log's cap: a mining accident can leave dozens of piles, and
/// twenty rows is more than an operator acts on in one sitting.
let capBreaches = 20

/// The declared Reactors this colony can actually see, each with the room its
/// declaration names. A Reactor we cannot see yields no breach of any kind:
/// a channel that read absence as a violation would alarm on every tick the
/// errand's resident body is between lives.
let private declaredReactors (view: ColonyView) : (string * ReactorInfo) list =
    view.Errands
    |> List.choose (fun errand ->
        let reactorId = fst errand.Target

        view.Reactors
        |> List.tryFind (fun reactor -> reactor.Id = reactorId)
        |> Option.map (fun reactor -> errand.RoomName, reactor))

/// This tick's violations, in kind order. Every check is O(creeps + declared
/// targets + piles) with no flood and no priced walk: a channel that priced a
/// walk would spend the budget the projection is being judged on.
let private breachesIn (view: ColonyView) : Breach list =
    // The ore on the floor, read through `Facts` rather than restated so the
    // alarm cannot disagree with the Pickup that answers it. A check and the
    // projection it reads are pinned together: the errand half fired on
    // nothing when first wired because the view's `erranding` cut emptied the
    // room's census (#356), which is why each ore case has a `ViewTests`
    // counterpart.
    let decayingOre =
        (Decide.Facts.ourThoriumPiles view @ Decide.Facts.ourThoriumTombstones view)
        |> List.choose (fun id ->
            SpatialInfo.roomOf view.Spatial id
            |> Option.map (fun room ->
                {
                    Kind = BreachKind.OreOnTheFloor
                    Room = room
                    Subject = id
                    Amount = SpatialInfo.heldIn view.Spatial Thorium id
                    Window = 0
                    Impact = None
                }))

    let reactors = declaredReactors view

    // The ore that has nowhere to go: a body standing in the declared
    // Reactor's room holding more than the Reactor's free space (`transfer`
    // into a full store leaves the rest aboard, to be dropped). The free space
    // is read off `view.Reactors` and never off `SpatialInfo.Thorium`, which
    // deliberately does not carry this store: #354's draw gate read that map,
    // answered 0 for a store holding 999, and its unit test agreed because the
    // fixture wrote the store where the gate looked. Narrowed to the bodies in
    // that room: "any creep holding Thorium" fires on every miner at home.
    let unplaceable =
        reactors
        |> List.collect (fun (room, reactor) ->
            let free = Engine.reactorCapacity - reactor.Thorium
            let standingThere = (SpatialInfo.layerOf view.Spatial room).CreepPositions

            view.Creeps
            |> List.filter (fun creep -> Map.containsKey creep.Name standingThere)
            |> List.choose (fun creep ->
                // The ore that has nowhere to go, and not the load: a courier
                // holding 500 against 300 of free space is 200 in trouble. At
                // or below zero there is room for all of it and there is no
                // breach, which is the clamp written as the gate it is.
                let stranded = creep.Thorium - free

                if stranded >= 1 then
                    Some
                        {
                            Kind = BreachKind.OreUnplaceable
                            Room = room
                            Subject = creep.Name
                            Amount = stranded
                            Window = 0
                            Impact = None
                        }
                else
                    None))

    // The Reactor that will stand dry before anybody can reach it. `leadTicks`
    // is the cast (30 parts at 3 ticks a part) plus a walk floor of 50 ticks a
    // room crossing, one tile a tick on roads; live measured 159 over three
    // crossings against the floor's 150, so it fires slightly late rather than
    // early. `None` when the names do not join: the alarm stays silent rather
    // than guessing a distance.
    let walkFloor hops = hops * 50

    let leadTicks (room: string) =
        let cast = List.length courierPattern.Block * 3

        view.Spatial.RoomName
        |> Option.bind (fun home -> RoomName.hopsBetween home room)
        |> Option.map (fun hops -> cast + walkFloor hops)

    // A body is a courier by its shape, matched against the row's own pattern
    // rather than a name this channel would have to parse.
    let couriers =
        view.Creeps
        |> List.filter (fun creep ->
            partCount creep.Body Carry = partCountIn courierPattern.Block Carry
            && partCount creep.Body Move = partCountIn courierPattern.Block Move)

    // The store is the clock: the Reactor burns exactly 1 T a tick
    // (`docs/research/thorium-reactor.md`). An answer is ore actually moving,
    // a courier-shaped body holding Thorium, and never a body alive (one
    // hauled energy for 465 ticks while the store ran dry, #367) nor an
    // assignment. And only if it can get there in time (#377): timed against
    // the same floor `leadTicks` uses, with the body's room off the
    // projection. The cost is a false alarm between cast and first load, the
    // right direction for an alarm whose value is arriving early.
    let answered (room: string, reactor: ReactorInfo) =
        couriers
        |> List.exists (fun courier ->
            courier.Thorium > 0
            && SpatialInfo.creepRoomOf view.Spatial courier.Name
               |> Option.bind (fun at -> RoomName.hopsBetween at room)
               |> Option.exists (fun hops -> walkFloor hops <= reactor.Thorium))

    let runningDry =
        reactors
        |> List.filter (fun (room, reactor) ->
            reactor.Owner = ReactorOwner.Ours
            && reactor.Thorium > 0
            && not (answered (room, reactor))
            && leadTicks room |> Option.exists (fun lead -> reactor.Thorium <= lead))
        |> List.map (fun (room, reactor) ->
            {
                Kind = BreachKind.ReactorRunningDry
                Room = room
                Subject = reactor.Id
                // The ticks of burn left, which is what the operator acts on:
                // it counts down every tick the row stands, and the row's age
                // says how long nobody has answered.
                Amount = reactor.Thorium
                Window = 0
                Impact = None
            })

    // A Reactor of ours standing dry: the one kind that is routinely
    // transient, which is what the age column is for.
    let starved =
        reactors
        |> List.filter (fun (_, reactor) ->
            reactor.Owner = ReactorOwner.Ours && reactor.Thorium = 0)
        |> List.map (fun (room, reactor) ->
            {
                Kind = BreachKind.ReactorStarved
                Room = room
                Subject = reactor.Id
                Amount = 0
                Window = 0
                Impact = None
            })

    // A declared Reactor whose row says the flag on it is not ours, the
    // rival's and the unowned alike: nothing delivered there scores for us.
    let lost =
        reactors
        |> List.filter (fun (_, reactor) -> reactor.Owner <> ReactorOwner.Ours)
        |> List.map (fun (room, reactor) ->
            {
                Kind = BreachKind.ReactorLost
                Room = room
                Subject = reactor.Id
                Amount = 0
                Window = 0
                Impact = None
            })

    decayingOre @ unplaceable @ runningDry @ starved @ lost

/// Whether a standing row is open at `tick`: it has stood its `Window`.
let isOpen (tick: int) (row: StandingBreach) =
    tick - row.FirstSeen >= row.Breach.Window

/// The facts a colony's breaches read beside its view, each one already
/// computed this tick: nothing here walks, floods or parses Memory.
type ColonyBreachFacts =
    {
        /// This tick's quota arithmetic (`Decision.Quotas`), or None where the
        /// economy did not decide whole (fight-only, replayed, faulted):
        /// the seats owed and the holds are then unknown, a `SpawnIdle` row
        /// stands as it was while its spawn stays free at a full bank, and
        /// no body reads as idle.
        Quotas: Quotas option
        /// The spawns a `SpawnCreep` of this tick's names: busy from now,
        /// unless the engine refused the last one (`casting`).
        Casting: Set<string>
        /// The spawns whose last cast the engine refused (#629): the cast
        /// before this tick's, whichever full tick made it.
        CastRefused: Set<string>
        /// The outposts of this colony's a stand-down shuts: the bodies whose
        /// last Task worked one wait by design.
        StoodDown: Set<string>
        /// The Transition log as this tick left it.
        Log: ObserveState
        /// The fault ring's rows in this colony since the bundle's load
        /// (`observe.version.reset`), as (step, tick).
        Faults: (string * int) list
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ColonyBreachFacts =
    /// A colony read off its view alone.
    let none =
        {
            Quotas = None
            Casting = Set.empty
            CastRefused = Set.empty
            StoodDown = Set.empty
            Log = Map.empty
            Faults = []
        }

/// The facts the world's breaches read: the CPU line, the cadence it was
/// run at and the allowance it is measured against, and the throws no
/// colony owns.
type WorldBreachFacts =
    {
        Time: int
        Line: CpuState
        /// The cadence in effect, or None with light ticks off: no light tick
        /// is owed then.
        Cadence: int option
        /// `Game.cpu.limit`: what a tick may spend without drawing the bucket.
        Limit: float
        /// The tick the bundle loaded on (`observe.version.reset`): a global
        /// reset, whose tick and first cadence neither window reads.
        LoadedAt: int
        /// The fault ring's rows since the bundle's load that cost no one
        /// colony (a tick's, an intent's, a segment's), as (step, tick).
        Faults: (string * int) list
        /// The nukes in flight at the rooms of ours (#657).
        Nukes: NukeWatch
    }

/// How long a step's last throw keeps its `Fault` row open: a creep's life,
/// over which every body the throw cost a tick of intents has lived out what
/// that tick did; the ring keeps listing it for `observe health`.
let faultOpenTicks = Engine.creepLifetime

/// One `Fault` row per step whose newest throw since the load is recent:
/// the amount is its throws since the load.
let private faultBreaches (tick: int) (room: string) (faults: (string * int) list) : Breach list =
    faults
    |> List.groupBy fst
    |> List.filter (fun (_, rows) ->
        rows |> List.exists (fun (_, at) -> tick - at < faultOpenTicks))
    |> List.map (fun (phase, rows) ->
        {
            Kind = BreachKind.Fault
            Room = room
            Subject = phase
            Amount = List.length rows
            Window = 0
            Impact = None
        })

/// The longest cast a spawn makes: a full-size body at three ticks a part,
/// and an engine fact: a spawn free past it has lost a whole body's cast,
/// where one free a few ticks has lost a part of one.
let spawnIdleWindow = Engine.maxBodyParts * Engine.spawnTicksPerPart

/// Whether a spawn casts this tick: a `SpawnCreep` of this tick's names it
/// and the engine did not refuse its last cast (#629). The view's
/// `IsSpawning` already reads a refused cast as no body; this tick's cast is
/// not yet answered, and asks the same of the same spawn as the refused one.
let private casting (facts: ColonyBreachFacts) (spawn: string) =
    Set.contains spawn facts.Casting && not (Set.contains spawn facts.CastRefused)

/// One `RefusalStorm` row per actor of this colony's the engine refused
/// since the colony last decided, its weight summed over the Tasks it was
/// refused holding.
let private refusalStorm (view: ColonyView) : Breach list =
    let halfLife = view.Tuning.RefusalHalfLife

    view.Refusals
    |> List.filter (fun row -> row.Fresh > 0)
    |> List.groupBy _.Actor
    |> List.map (fun (actor, rows) ->
        {
            Kind = BreachKind.RefusalStorm
            Room = SpatialInfo.homeName view.Spatial
            Subject = actor
            Amount =
                rows
                |> List.sumBy (Refusal.weightAt halfLife view.Time)
                |> System.Math.Round
                |> int
            Window = halfLife
            Impact = None
        })

/// Each spawn free beside a full bank that cast nothing this tick, while the
/// spawn plan left seats a full bank cannot pay though their row's block fits
/// it (`Quotas.Unfilled`: a body sized over capacity, a defect). The plan's
/// own answer, never re-derived from the rows; a seat a named rule holds
/// (`Quotas.Held`) is not one. A bank still filling is income, not this.
let private spawnIdle
    (facts: ColonyBreachFacts)
    (view: ColonyView)
    (prior: BreachState)
    : Breach list =
    let bankFull = view.Bank.Capacity > 0 && view.Bank.Available >= view.Bank.Capacity

    if not bankFull then
        []
    else
        view.Spawns
        |> List.filter (fun spawn -> not spawn.IsSpawning && not (casting facts spawn.Name))
        |> List.choose (fun spawn ->
            match facts.Quotas |> Option.map _.Unfilled with
            | Some seats when seats > 0 -> Some seats
            | Some _ -> None
            | None ->
                prior.Standing
                |> Map.tryFind (BreachKind.SpawnIdle, spawn.Name)
                |> Option.map _.Breach.Amount
            |> Option.map (fun seats ->
                {
                    Kind = BreachKind.SpawnIdle
                    Room = spawn.RoomName
                    Subject = spawn.Name
                    Amount = seats
                    Window = spawnIdleWindow
                    Impact = None
                }))

/// The ticks a body lives: a CLAIM part's shorter life, else the creep's.
let private lifetimeOf (body: Map<BodyPart, int>) =
    if partCount body BodyPart.Claim > 0 then
        Engine.claimLifetime
    else
        Engine.creepLifetime

/// Whether a busy reason starves the upgrader row by design: a raid, a child
/// fed and the reserve under its floor each spend the energy the upgrade
/// would take, so the controller's buffer yields and the row stands at one
/// (`Pool.planPool`, `Quota.quotaRowsOf`). The two that are work (a broken
/// line, sites owed) starve nobody.
let private starvesUpgrade (quotas: Quotas) =
    quotas.Busy
    |> List.exists (function
        | Raided
        | FeedingChild
        | UnderReserve -> true
        | LineBroken
        | SitesOwed -> false)

/// Each body of this colony's whose task cursor says unassigned (a Flee is a
/// Task, so a fleeing body is not here) for its share of its own life, while
/// nothing holds that body on purpose: a fighter with no Task and no ask
/// open for its row is a reserve walking to its idle home; an upgrader is
/// held while a busy reason starves its row (`starvesUpgrade`); a body whose
/// last Task worked an outpost a stand-down shuts waits for it. Unknown
/// quotas hold every economy body. The idle opens on the first unassigned
/// entry after the last Task the ring holds; a ring that evicted it dates the
/// idle from its oldest entry, which understates it and never overstates it.
let private bodyIdle (facts: ColonyBreachFacts) (view: ColonyView) : Breach list =
    let tick = view.Time
    let log = facts.Log

    let isTask =
        function
        | Verdict.Matched _
        | Verdict.Kept _
        | Verdict.Released _ -> true
        | _ -> false

    let isIdle =
        function
        | Verdict.Unassigned _ -> true
        | _ -> false

    // The room the body's last Task worked: its target's, or the target
    // itself where the Task names a room.
    let lastWorked (record: CreepLog) =
        record.Entries
        |> List.tryFindBack (fun entry -> isTask entry.Verdict)
        |> Option.bind (fun entry ->
            match entry.Verdict with
            | Verdict.Matched(_, tid, _)
            | Verdict.Kept(_, tid)
            | Verdict.Released(_, tid, _) -> taskTargetOf tid
            | _ -> None)
        |> Option.map (fun target ->
            SpatialInfo.roomOf view.Spatial target |> Option.defaultValue target)

    let economyHeld (creep: CreepInfo) (record: CreepLog) =
        match facts.Quotas with
        | None -> true
        | Some quotas ->
            (isNamedFor upgraderPattern creep.Name && starvesUpgrade quotas)
            || lastWorked record
               |> Option.exists (fun room -> Set.contains room facts.StoodDown)

    // A fighter is a reserve only while nothing asks for it: an ask this
    // colony casts for whose row cuts the body (`Quotas.Casts`) and that it
    // stands unseated for is idleness, not reserve (#647).
    let asked (creep: CreepInfo) =
        facts.Quotas
        |> Option.exists (fun quotas ->
            quotas.Casts
            |> List.exists (fun cast ->
                Decide.Threat.CastRow.cut cast.Ask.Row creep.Name creep.Body))

    let held (creep: CreepInfo) record =
        if Decide.Quota.isFighterBody creep then
            not (asked creep)
        else
            economyHeld creep record

    ColonyView.bodies view
    |> List.choose (fun creep ->
        match Map.tryFind creep.Name log with
        | Some record when not (held creep record) && record.LastTask |> Option.exists isIdle ->
            let sinceTask =
                record.Entries
                |> List.rev
                |> List.takeWhile (fun entry -> not (isTask entry.Verdict))

            sinceTask
            |> List.filter (fun entry -> isIdle entry.Verdict)
            |> List.tryLast
            |> Option.orElse (List.tryHead record.Entries)
            |> Option.map (fun entry -> tick - entry.Tick)
            |> Option.filter (fun idle ->
                idle * 100 >= lifetimeOf creep.Body * view.Tuning.BodyIdlePercent)
            |> Option.map (fun idle ->
                {
                    Kind = BreachKind.BodyIdle
                    Room =
                        SpatialInfo.creepRoomOf view.Spatial creep.Name
                        |> Option.defaultValue (SpatialInfo.homeName view.Spatial)
                    Subject = creep.Name
                    Amount = idle
                    Window = 0
                    Impact = None
                })
        | _ -> None)

/// This tick's violations in one colony, off its view and the facts beside
/// it; `prior` is the log they fold onto, read for what an unknown fact
/// leaves standing.
let colonyBreaches
    (facts: ColonyBreachFacts)
    (view: ColonyView)
    (prior: BreachState)
    : Breach list =
    faultBreaches view.Time (SpatialInfo.homeName view.Spatial) facts.Faults
    @ spawnIdle facts view prior
    @ bodyIdle facts view
    @ refusalStorm view
    @ breachesIn view

/// Every tick of the line's last `LightStarvedCadences` cadences ran full,
/// forcing made at least one of them so, and at the window's draw (its mean
/// over `limit`) the bucket stands empty within `Tuning.BucketHorizon`, the
/// margin `BucketFalling` prices by. Forced full ticks the bucket can pay
/// for (a siege at a full bucket, a defence under the limit) starve
/// nothing. The line must reach
/// back over the whole window: a ring younger than it has not seen a cadence
/// fail.
let private lightStarved
    (tuning: Tuning)
    (limit: float)
    (cadence: int option)
    (line: CpuState)
    : Breach list =
    match cadence, List.tryHead line.Ticks, List.tryLast line.Ticks with
    | Some cadence, Some oldest, Some newest ->
        let from = newest.Tick - cadence * tuning.LightStarvedCadences + 1
        let window = line.Ticks |> List.filter (fun sample -> sample.Tick >= from)
        let forced = window |> List.choose _.Forced

        if
            oldest.Tick <= from
            && not (List.isEmpty forced)
            && window |> List.forall (fun sample -> not sample.Light)
            && (let over = (window |> List.averageBy _.Ms) - limit

                let bucket = newest.Phases |> Option.map (fun phases -> float phases.Bucket)

                over > 0.0
                && bucket
                   |> Option.exists (fun bucket -> bucket / over <= float tuning.BucketHorizon))
        then
            // The forcing most of the window named, so the row's identity
            // holds while one forcing hands over to another for a tick.
            let reason =
                forced
                |> List.countBy _.Reason
                |> List.sortBy (fun (reason, count) -> -count, reason)
                |> List.head
                |> fst

            [
                {
                    Kind = BreachKind.LightStarved
                    Room = ""
                    Subject = reason
                    Amount = List.length forced
                    Window = 0
                    Impact = None
                }
            ]
        else
            []
    | _ -> []

/// The bucket's least-squares slope over the whole ring (`capCpuTicks`
/// rows), priced as the ticks until it stands empty; a ring not yet full is
/// too short to tell a replan's dip from a fall.
let private bucketFalling (tuning: Tuning) (line: CpuState) : Breach list =
    let samples =
        line.Ticks
        |> List.choose (fun sample ->
            sample.Phases
            |> Option.map (fun phases -> float sample.Tick, float phases.Bucket))

    if List.length samples < capCpuTicks then
        []
    else
        let meanX = samples |> List.averageBy fst
        let meanY = samples |> List.averageBy snd
        let sxx = samples |> List.sumBy (fun (x, _) -> (x - meanX) * (x - meanX))
        let sxy = samples |> List.sumBy (fun (x, y) -> (x - meanX) * (y - meanY))
        let slope = if sxx = 0.0 then 0.0 else sxy / sxx
        let toEmpty = snd (List.last samples) / -slope

        if slope < 0.0 && toEmpty <= float tuning.BucketHorizon then
            [
                {
                    Kind = BreachKind.BucketFalling
                    Room = ""
                    Subject = "bucket"
                    Amount = int toEmpty
                    Window = 0
                    Impact = None
                }
            ]
        else
            []

/// The damage a nuke deals a tile at Chebyshev `range` from its centre.
let nukeBlastAt (range: int) =
    if range = 0 then Engine.nukeDamageCentre
    elif range <= Engine.nukeRadius then Engine.nukeDamageRing
    else 0

/// A sighting priced as the engine lands it (`processor/intents/nukes/tick.js`):
/// each tile's rampart takes the blast first and passes on what its hits did
/// not stop, a structure on that tile standing while its hits exceed the
/// rest; every creep in the room dies whatever tile it stands on.
let nukeImpact (sighting: NukeSighting) : NukeImpact =
    let rampart = builtKindName BuiltKind.Rampart

    let rampartOn =
        sighting.Inside
        |> List.filter (fun target -> target.Built = rampart)
        |> List.map (fun target -> target.Square, target.HitsLeft)
        |> Map.ofList

    let priced =
        sighting.Inside
        |> List.choose (fun target ->
            let range =
                max
                    (abs (target.Square.X - sighting.Centre.X))
                    (abs (target.Square.Y - sighting.Centre.Y))

            let blast = nukeBlastAt range

            if blast = 0 then
                None
            else
                let shield =
                    if target.Built = rampart then
                        0
                    else
                        Map.tryFind target.Square rampartOn |> Option.defaultValue 0

                Some
                    {
                        Exposed = target
                        Blast = blast
                        Rampart = shield
                        Shortfall = max 0 (blast + 1 - shield - target.HitsLeft)
                    })
        |> List.sortBy (fun row -> -row.Shortfall, row.Exposed.Built, row.Exposed.Structure)

    {
        GroundZero = sighting.Centre
        Lands = sighting.LandsAt
        Launcher = sighting.Launch
        Priced = priced
        Killed = List.sort sighting.Doomed
    }

/// One `IncomingNuke` row per nuke in flight at a room of ours: each one in
/// sight priced afresh, and one the prior log holds whose target is still
/// ours but out of sight standing until the landing tick it was seen with.
/// A target in sight that no longer shows it means it landed: the row closes.
let private incomingNukes (time: int) (watch: NukeWatch) (prior: BreachState) : Breach list =
    let seen =
        watch.Nukes
        |> List.map (fun sighting ->
            {
                Kind = BreachKind.IncomingNuke
                Room = sighting.Aimed
                Subject = sighting.Nuke
                Amount = sighting.LandsAt - time
                Window = 0
                Impact = Some(nukeImpact sighting)
            })

    let blind =
        prior.Standing
        |> Map.toList
        |> List.choose (fun (_, row) ->
            match row.Breach.Kind, row.Breach.Impact with
            | BreachKind.IncomingNuke, Some impact when
                Set.contains row.Breach.Room watch.Covered
                && not (Set.contains row.Breach.Room watch.Sighted)
                && impact.Lands > time
                ->
                Some
                    { row.Breach with
                        Amount = impact.Lands - time
                    }
            | _ -> None)

    seen @ blind

/// This tick's violations that belong to no one colony: the CPU line's two,
/// cause before symptom, the throws no colony owns, and the nukes in flight
/// at any room of ours, read for all of them on every full tick whichever
/// colony decided; `prior` is the log they fold onto, read for a nuke whose
/// target is out of sight.
/// Both CPU windows read the line from after a global reset's first cadence:
/// the reset tick runs several limits (470 ms live) and the colonies it
/// defers decide on the ticks after, so a window holding them reads a deploy
/// as a starved or falling bucket while it refills (#627). A window the
/// settled line does not reach back over has not seen a cadence fail.
let worldBreaches (tuning: Tuning) (facts: WorldBreachFacts) (prior: BreachState) : Breach list =
    let settled =
        { facts.Line with
            Ticks =
                facts.Line.Ticks
                |> List.filter (fun sample ->
                    sample.Tick > facts.LoadedAt + Option.defaultValue 1 facts.Cadence)
        }

    lightStarved tuning facts.Limit facts.Cadence settled
    @ bucketFalling tuning settled
    @ faultBreaches facts.Time "" facts.Faults
    @ incomingNukes facts.Time facts.Nukes prior

/// Trim the log to the cap: a pending row before an open one, a `BodyIdle`
/// row before any other kind (a colony's idle bodies can number dozens, and
/// must never evict the Reactor or the ore), an `IncomingNuke` row after
/// every other kind, then newest `FirstSeen` dropped
/// first, since the row that has stood longest is the one bleeding longest
/// (every surviving row ties on `LastSeen`). The remaining tie is broken on
/// kind and subject so the eviction is a function of the state and not of
/// map ordering.
let private capStanding (cap: int) (rows: Map<BreachKind * string, StandingBreach>) =
    let overflow = Map.count rows - cap

    if overflow <= 0 then
        rows
    else
        rows
        |> Map.toList
        |> List.sortBy (fun ((kind, subject), row) ->
            isOpen row.LastSeen row,
            kind <> BreachKind.BodyIdle,
            kind = BreachKind.IncomingNuke,
            row.LastSeen,
            -row.FirstSeen,
            kind,
            subject)
        |> List.skip overflow
        |> Map.ofList

/// The breach log's fold over this tick's violations (`colonyBreaches`,
/// `worldBreaches`): a violation keeps the tick it was first seen on; one
/// this tick did not read is gone. Dropping out is the decision the channel
/// is built on (a lingering row would make the log a history), and a breach
/// that flickers off for one tick loses its age. A blind tick contributes no
/// breach, so the channel says "nothing is broken that I can see" and never
/// "nothing is broken".
let foldBreaches (cap: int) (tick: int) (found: Breach list) (prior: BreachState) : BreachState =
    {
        Standing =
            found
            |> List.map (fun breach ->
                let key = breach.Kind, breach.Subject

                let firstSeen =
                    prior.Standing
                    |> Map.tryFind key
                    |> Option.map (fun row -> row.FirstSeen)
                    |> Option.defaultValue tick

                key,
                {
                    FirstSeen = firstSeen
                    LastSeen = tick
                    // The latest reading and not the first: a pile that has
                    // grown from 90 T to 915 must say 915. The opposite rule to
                    // the raid roster's first-sighting-wins.
                    Breach = breach
                })
            |> Map.ofList
            |> capStanding cap
    }

/// The standing rows, oldest first, computed once so neither the Memory writer
/// nor `observe.mjs` has an ordering of its own. Ties are broken on kind and
/// subject.
let breachRows (state: BreachState) : StandingBreach list =
    state.Standing
    |> Map.toList
    |> List.sortBy (fun ((kind, subject), row) -> row.FirstSeen, kind, subject)
    |> List.map snd

/// The live rows against a clock: each breach with how many ticks it has been
/// standing, oldest first.
let standing (tick: int) (state: BreachState) : (Breach * int) list =
    breachRows state |> List.map (fun row -> row.Breach, tick - row.FirstSeen)

/// The rows open at `tick` (`isOpen`), oldest first: what is broken, the
/// pending rows left out.
let openRows (tick: int) (state: BreachState) : StandingBreach list =
    breachRows state |> List.filter (isOpen tick)

// ---- the rival dossier (#653) ---------------------------------------------
//
// What each rival's code was seen to do, folded on a full tick off what the
// world already holds of the rooms in sight, and the few facts of those rooms
// the shell reads beside it (`RivalSight`). No decision reads it.

/// How often one fact was seen of a rival, and the tick it last was.
type Tally = { Count: int; Last: int }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Tally =
    let zero: Tally = { Count = 0; Last = 0 }

    let addMany (tick: int) (count: int) (tally: Tally) : Tally =
        if count <= 0 then
            tally
        else
            {
                Count = tally.Count + count
                Last = tick
            }

    let add (tick: int) (tally: Tally) : Tally = addMany tick 1 tally

/// A duration measured over episodes: how many, their sum, the shortest and
/// the longest, and the tick the last one ended on.
type Span =
    {
        Episodes: int
        Sum: int
        Shortest: int
        Longest: int
        Ended: int
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Span =
    let zero: Span =
        {
            Episodes = 0
            Sum = 0
            Shortest = 0
            Longest = 0
            Ended = 0
        }

    let add (tick: int) (ticks: int) (span: Span) : Span =
        {
            Episodes = span.Episodes + 1
            Sum = span.Sum + ticks
            Shortest = (if span.Episodes = 0 then ticks else min span.Shortest ticks)
            Longest = max span.Longest ticks
            Ended = tick
        }

/// One question a choice answers about the policy behind it: the choices
/// whose candidates differed on it (`Open`), and of those, the ones whose
/// pick had it (`Hit`). A pick among candidates alike on it says nothing.
type Aim = { Open: int; Hit: int }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Aim =
    let zero: Aim = { Open = 0; Hit = 0 }

    /// One pick read: whether the pick had it, and whether each candidate,
    /// the pick among them, did.
    let read (picked: bool) (candidates: bool list) (aim: Aim) : Aim =
        if List.contains true candidates && List.contains false candidates then
            {
                Open = aim.Open + 1
                Hit = aim.Hit + (if picked then 1 else 0)
            }
        else
            aim

/// Which of our bodies a rival's towers shot, read against the bodies of
/// ours standing in the room: the nearest to the tower, the lowest hits
/// before the shot, a healer, an attacker, one on the edge.
type TowerAim =
    {
        Shots: Tally
        /// Shots at the one body of ours in the room: no pick to read.
        Alone: int
        Nearest: Aim
        Weakest: Aim
        Healer: Aim
        Attacker: Aim
        Edge: Aim
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TowerAim =
    let zero: TowerAim =
        {
            Shots = Tally.zero
            Alone = 0
            Nearest = Aim.zero
            Weakest = Aim.zero
            Healer = Aim.zero
            Attacker = Aim.zero
            Edge = Aim.zero
        }

/// One room a rival holds, owned or reserved, as its dossier keeps it.
type RivalRoom =
    {
        /// The tick it was last seen held by the rival.
        Seen: int
        /// Its reservation stood here at the last look.
        Reserving: bool
        /// Its towers' energy: readings taken, the lowest one tower held, the
        /// sum the last reading found, and the readings that found a tower
        /// fuller than the one before.
        TowerReads: int
        TowerLow: int
        TowerLast: int
        Refills: Tally
        /// Rampart hits gained, summed over its ramparts, and the
        /// rampart-ticks they were gained over: under fire (an armed body of
        /// ours in the room at either reading) and at peace. A gain is the
        /// rise between two readings, so a rampart we struck meanwhile
        /// reads less than was repaired.
        FireGain: float
        FireTicks: float
        PeaceGain: float
        PeaceTicks: float
        /// Whether the rampart gaining the most had the lowest hits before.
        LowestFirst: Aim
        /// Ticks from our first armed body in the room to its first armed
        /// body there; the visits it never answered; the visits it stood
        /// armed when we arrived.
        Response: Span
        Unanswered: Tally
        Standing: Tally
        /// The tick a worker of its fell here before its time, no worker of
        /// its left at work; 0 while none is pending.
        KilledAt: int
        /// Ticks from that fall to a worker of its seen here again.
        Unworked: Span
        /// The tick its reservation was first seen gone; 0 while none is.
        LapsedAt: int
        /// Ticks from that lapse to its reservation seen again.
        Rereserved: Span
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RivalRoom =
    let empty: RivalRoom =
        {
            Seen = 0
            Reserving = false
            TowerReads = 0
            TowerLow = 0
            TowerLast = 0
            Refills = Tally.zero
            FireGain = 0.0
            FireTicks = 0.0
            PeaceGain = 0.0
            PeaceTicks = 0.0
            LowestFirst = Aim.zero
            Response = Span.zero
            Unanswered = Tally.zero
            Standing = Tally.zero
            KilledAt = 0
            Unworked = Span.zero
            LapsedAt = 0
            Rereserved = Span.zero
        }

/// One safe mode a rival raised, and what stood around it: the room, the
/// tick it was raised (its end less the engine's duration), the tick we saw
/// it, the controller's level, our bodies and armed bodies in the room then,
/// how long our armed bodies had stood there before it (None for none), and
/// each kind of its structures our bodies struck there by the ticks before
/// it they last did.
type SafeModeSeen =
    {
        Room: string
        At: int
        Noticed: int
        Level: int
        Ours: int
        OursArmed: int
        Since: int option
        Struck: (string * int) list
    }

/// One rival's dossier.
type Dossier =
    {
        /// The first and the last tick anything of it was seen.
        Since: int
        Latest: int
        Aim: TowerAim
        /// Its safe modes, the tally's `Last` the latest raised; the newest
        /// few with their surroundings; the latest cooldown end seen.
        SafeModes: Tally
        Activations: SafeModeSeen list
        CooldownUntil: int
        /// Its armed bodies, each counted once, and their shapes by count.
        Armed: Tally
        Shapes: Map<string, int>
        /// An armed body seen in another room than the one it was last
        /// seen in, and of those the ones that left a room it holds for one
        /// it does not.
        Crossings: Tally
        Sorties: Tally
        /// Its workers standing in a room it holds when our armed body
        /// arrived: still there when we left, walked off, fell.
        Stayed: Tally
        Fled: Tally
        Fell: Tally
        /// Its armed bodies first seen in a home of ours, by that home's
        /// level, and in a room we reserve.
        HomeRaiders: Map<int, Tally>
        OutpostRaiders: Tally
        Rooms: Map<string, RivalRoom>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Dossier =
    let opened (tick: int) : Dossier =
        {
            Since = tick
            Latest = tick
            Aim = TowerAim.zero
            SafeModes = Tally.zero
            Activations = []
            CooldownUntil = 0
            Armed = Tally.zero
            Shapes = Fresh.mapOfSeq Seq.empty
            Crossings = Tally.zero
            Sorties = Tally.zero
            Stayed = Tally.zero
            Fled = Tally.zero
            Fell = Tally.zero
            HomeRaiders = Fresh.mapOfSeq Seq.empty
            OutpostRaiders = Tally.zero
            Rooms = Fresh.mapOfSeq Seq.empty
        }

/// Our armed body's visit to a room a rival holds, open while one stands
/// there: whose room, the tick it began, whether an armed body of the
/// rival's has stood there since, and the rival's workers standing there
/// when it began that have not yet left or fallen.
type RivalVisit =
    {
        Holder: string
        Began: int
        Answered: bool
        Workers: Set<string>
    }

/// One rival body as last seen: its owner, its room, the tick its life runs
/// out on, and whether it is armed or works.
type RivalBody =
    {
        Holder: string
        Room: string
        Ends: int
        Armed: bool
        Works: bool
    }

/// What the dossier's fold carries across full ticks beside the dossiers,
/// on the heap and never written: a reset starts each from nothing.
type RivalGlance =
    {
        /// Each rival room's towers' energy at its last reading, and its tick.
        Towers: Map<string, int * Map<string, int>>
        /// Each rival room's ramparts at its last reading: the tick, whether
        /// an armed body of ours stood there, and the hits by tile.
        Ramparts: Map<string, int * bool * Map<Pos, int>>
        Visits: Map<string, RivalVisit>
        Bodies: Map<string, RivalBody>
        /// Each rival room's structure kinds our bodies struck, by the tick
        /// they last did.
        Struck: Map<string, Map<string, int>>
    }

/// The dossiers and the fold's heap state.
type Rivals =
    {
        Dossiers: Map<string, Dossier>
        Glance: RivalGlance
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Rivals =
    let glanceEmpty: RivalGlance =
        {
            Towers = Fresh.mapOfSeq Seq.empty
            Ramparts = Fresh.mapOfSeq Seq.empty
            Visits = Fresh.mapOfSeq Seq.empty
            Bodies = Fresh.mapOfSeq Seq.empty
            Struck = Fresh.mapOfSeq Seq.empty
        }

    /// The dossiers as read back, the heap state from nothing.
    let ofDossiers (dossiers: Map<string, Dossier>) : Rivals =
        {
            Dossiers = dossiers
            Glance = glanceEmpty
        }

    let empty: Rivals = ofDossiers (Fresh.mapOfSeq Seq.empty)

    /// Each rival room's defence response (`WorldView.Responses`, #658): the
    /// fewest ticks from our first armed body there to its owner's first
    /// armed body, over every visit it answered; `Int32.MaxValue` where it
    /// was visited and never answered; no row where it was never visited.
    let responses (rivals: Rivals) : Map<string, int> =
        rivals.Dossiers
        |> Map.toSeq
        |> Seq.collect (fun (_, dossier) -> dossier.Rooms |> Map.toSeq)
        |> Seq.choose (fun (room, held) ->
            if held.Response.Episodes > 0 then
                Some(room, held.Response.Shortest)
            elif held.Unanswered.Count > 0 then
                Some(room, System.Int32.MaxValue)
            else
                None)
        |> Seq.groupBy fst
        |> Seq.map (fun (room, rows) -> room, rows |> Seq.map snd |> Seq.min)
        |> Fresh.mapOfSeq

/// One shot of a rival tower at a body of ours, off the room's event log:
/// the tower's id and our body's name.
type TowerShot = { Tower: string; Target: string }

/// What the dossier reads on one full tick: the world's rooms in sight and
/// our bodies in them, our homes' levels, every rival controller as
/// remembered, and what the shell reads beside the world in the rival rooms
/// in sight — each tower's energy by id, the towers' shots at our bodies,
/// the damage every attack in those logs dealt each body of ours by name,
/// and the kinds of the rival's structures our bodies struck.
type RivalSight =
    {
        Tick: int
        Rooms: Map<string, WorldRoom>
        Bodies: Map<string, (CreepInfo * Pos) list>
        Homes: Map<string, int>
        Controllers: Map<string, RivalController>
        TowerEnergy: Map<string, int>
        Shots: Map<string, TowerShot list>
        Hurt: Map<string, int>
        Struck: Map<string, string list>
    }

/// The most rivals the dossier keeps, the latest seen; the rooms a rival's
/// row keeps, the latest seen; its body shapes, the most seen; and its safe
/// modes kept whole, the newest. Together they keep the whole dossier inside
/// its memory segments (`ObserveMemory.rivalSegments`; a wire-check case
/// writes it full).
let capRivals = 24
let capRivalRooms = 8
let capShapes = 8
let capActivations = 5

/// SAFE_MODE_DURATION: a safe mode's end less this is the tick it was raised.
let private safeModeDuration = 20_000

/// A player's body or room the dossier reads: not the NPCs, not an ally.
let private isRivalPlayer (owner: string) =
    owner <> "Invader"
    && not (HostileInfo.isKeeperOwner owner)
    && not (Colony.isAlly owner)

/// How long the fold's heap keeps a room's last tower, rampart or strike
/// reading unrefreshed: a body's lifetime, past which the defence it read is
/// another one, and the reading a baseline for nothing.
let private glanceMargin = Engine.creepLifetime

/// Whether a count read off the engine is a number: a body still spawning
/// answers `undefined` for the ticks it has to live.
[<Fable.Core.Emit("Number.isFinite($0)")>]
let private finite (ticks: int) : bool = ticks = ticks

/// A rival player's body standing in the world, not one still spawning: a
/// body in the oven is counted once it stands.
let private rivalBody (hostile: HostileInfo) =
    isRivalPlayer hostile.Owner && finite hostile.TicksToLive

let private hasPart (part: BodyPart) (body: BodyPart list) = body |> List.exists (samePart part)

let private armedBody (body: BodyPart list) =
    hasPart Attack body || hasPart RangedAttack body

/// An unarmed body at a room's economy: a digger, a carrier, a reserver.
let private workerBody (body: BodyPart list) =
    not (armedBody body)
    && (hasPart Work body || hasPart Carry body || hasPart BodyPart.Claim body)

let private armedCreep (creep: CreepInfo) =
    partCount creep.Body Attack + partCount creep.Body RangedAttack > 0

/// A body's shape, its part counts in a fixed order: "2T10A10M".
let shapeOf (body: BodyPart list) : string =
    let parts = partsOf body

    [
        Tough, "T"
        Attack, "A"
        RangedAttack, "R"
        Heal, "H"
        Work, "W"
        Carry, "C"
        BodyPart.Claim, "L"
        Move, "M"
    ]
    |> List.choose (fun (part, letter) ->
        match partCount parts part with
        | 0 -> None
        | count -> Some(string count + letter))
    |> String.concat ""

/// On the room's border ring or the tile in from it: a step from leaving.
let private onEdge (tile: Pos) = Dance.toEdge tile <= 1

/// One tower shot read against our bodies standing in the room.
let private aimShot
    (tick: int)
    (from: Pos)
    (target: CreepInfo)
    (at: Pos)
    (hurt: Map<string, int>)
    (ours: (CreepInfo * Pos) list)
    (aim: TowerAim)
    : TowerAim =
    let aim =
        { aim with
            Shots = Tally.add tick aim.Shots
        }

    match ours with
    | [ _ ] -> { aim with Alone = aim.Alone + 1 }
    | _ ->
        let nearest = ours |> List.map (fun (_, tile) -> range from tile) |> List.min

        // The hits each stood at before the tick's attacks: every body's own
        // damage back, nothing healed since counted.
        let before (creep: CreepInfo) =
            creep.Hits.Hits + (Map.tryFind creep.Name hurt |> Option.defaultValue 0)

        let weakest = ours |> List.map (fun (creep, _) -> before creep) |> List.min
        let healer (creep: CreepInfo) = partCount creep.Body Heal > 0

        { aim with
            Nearest =
                Aim.read
                    (range from at = nearest)
                    (ours |> List.map (fun (_, tile) -> range from tile = nearest))
                    aim.Nearest
            Weakest =
                Aim.read
                    (before target = weakest)
                    (ours |> List.map (fun (creep, _) -> before creep = weakest))
                    aim.Weakest
            Healer = Aim.read (healer target) (ours |> List.map (fst >> healer)) aim.Healer
            Attacker =
                Aim.read (armedCreep target) (ours |> List.map (fst >> armedCreep)) aim.Attacker
            Edge = Aim.read (onEdge at) (ours |> List.map (snd >> onEdge)) aim.Edge
        }

/// One interval between two rampart readings: the hits gained over the
/// ramparts standing at both, the rampart-ticks, and whether the rampart
/// gaining the most stood lowest before.
let private rampartGain
    (ticks: int)
    (before: Map<Pos, int>)
    (now: Map<Pos, int>)
    : (float * float * (bool * bool list) option) option =
    let common =
        before
        |> Map.toList
        |> List.choose (fun (tile, was) ->
            Map.tryFind tile now |> Option.map (fun hits -> was, max 0 (hits - was)))

    if List.isEmpty common then
        None
    else
        let gained = common |> List.sumBy snd
        let lowest = common |> List.map fst |> List.min
        let most = common |> List.map snd |> List.max

        let order =
            if gained > 0 then
                Some(
                    common |> List.exists (fun (was, gain) -> gain = most && was = lowest),
                    common |> List.map (fun (was, _) -> was = lowest)
                )
            else
                None

        Some(float gained, float (List.length common * ticks), order)

let private capDossier (dossier: Dossier) : Dossier =
    let rooms =
        if Map.count dossier.Rooms <= capRivalRooms then
            dossier.Rooms
        else
            dossier.Rooms
            |> Map.toList
            |> List.sortByDescending (fun (room, row) -> row.Seen, room)
            |> List.truncate capRivalRooms
            |> Fresh.mapOfList

    let shapes =
        if Map.count dossier.Shapes <= capShapes then
            dossier.Shapes
        else
            dossier.Shapes
            |> Map.toList
            |> List.sortByDescending (fun (shape, count) -> count, shape)
            |> List.truncate capShapes
            |> Fresh.mapOfList

    { dossier with
        Rooms = rooms
        Shapes = shapes
        Activations = List.truncate capActivations dossier.Activations
    }

/// One full tick folded into the dossiers. Each room in sight a rival holds
/// — owns, by its controller seen this tick, or reserves — moves that
/// rival's row for it: towers, ramparts, our visit and its answer, its
/// workers' flight and falls, its reservation's lapse. Every rival body in
/// sight moves its owner's body counts; every rival controller in sight its
/// safe modes.
let foldRivals (sight: RivalSight) (prior: Rivals) : Rivals =
    let tick = sight.Tick
    let dossiers = ref prior.Dossiers
    let glance = ref prior.Glance

    let touch (owner: string) (change: Dossier -> Dossier) =
        let dossier =
            Map.tryFind owner dossiers.Value
            |> Option.defaultWith (fun () -> Dossier.opened tick)

        dossiers.Value <- Map.add owner { change dossier with Latest = tick } dossiers.Value

    let rowOf (owner: string) (room: string) =
        Map.tryFind owner dossiers.Value
        |> Option.bind (fun dossier -> Map.tryFind room dossier.Rooms)

    let touchRoom (owner: string) (room: string) (change: RivalRoom -> RivalRoom) =
        touch owner (fun dossier ->
            let row = Map.tryFind room dossier.Rooms |> Option.defaultValue RivalRoom.empty

            { dossier with
                Rooms = Map.add room (change row) dossier.Rooms
            })

    // A room the rival holds: its controller as remembered, or its
    // reservation at its row's last look.
    let holds (owner: string) (room: string) =
        Map.tryFind room sight.Controllers
        |> Option.exists (fun controller -> controller.Owner = owner)
        || rowOf owner room |> Option.exists (fun row -> row.Reserving)

    for KeyValue(room, seen) in sight.Rooms do
        let ownedBy =
            Map.tryFind room sight.Controllers
            |> Option.filter (fun controller ->
                controller.Seen = tick && isRivalPlayer controller.Owner)
            |> Option.map (fun controller -> controller.Owner)

        let reservation = seen.Control.Reservation

        let reservedBy =
            reservation
            |> Option.filter (fun held ->
                (held.Holder = ReservationHolder.Rival || held.Holder = ReservationHolder.Enemy)
                && isRivalPlayer held.Username)
            |> Option.map (fun held -> held.Username)

        let ourReserve =
            reservation |> Option.exists (fun held -> held.Holder = ReservationHolder.Ours)

        let holder = Option.orElse reservedBy ownedBy
        let ours = Map.tryFind room sight.Bodies |> Option.defaultValue []
        let oursArmed = ours |> List.filter (fst >> armedCreep) |> List.length

        let rivals = seen.Hostiles |> List.filter rivalBody

        // A reservation its row saw that stands no longer.
        for KeyValue(owner, dossier) in dossiers.Value do
            match Map.tryFind room dossier.Rooms with
            | Some row when row.Reserving && reservedBy <> Some owner ->
                touchRoom owner room (fun row ->
                    { row with
                        Reserving = false
                        LapsedAt = tick
                    })
            | _ -> ()

        holder
        |> Option.iter (fun owner ->
            touchRoom owner room (fun row ->
                if reservedBy = Some owner then
                    { row with
                        Seen = tick
                        Reserving = true
                        LapsedAt = 0
                        Rereserved =
                            if row.LapsedAt > 0 then
                                Span.add tick (tick - row.LapsedAt) row.Rereserved
                            else
                                row.Rereserved
                    }
                else
                    { row with Seen = tick }))

        // Each kind of the rival's structures our bodies struck here.
        match Map.tryFind room sight.Struck with
        | Some kinds when not (List.isEmpty kinds) ->
            let held =
                Map.tryFind room glance.Value.Struck
                |> Option.defaultWith (fun () -> Fresh.mapOfSeq Seq.empty)

            glance.Value <-
                { glance.Value with
                    Struck =
                        Map.add
                            room
                            (kinds |> List.fold (fun held kind -> Map.add kind tick held) held)
                            glance.Value.Struck
                }
        | _ -> ()

        ownedBy
        |> Option.iter (fun owner ->
            // Its towers' energy, and their shots at our bodies.
            let energies =
                seen.Towers
                |> List.choose (fun (id, _) ->
                    Map.tryFind id sight.TowerEnergy |> Option.map (fun energy -> id, energy))

            if not (List.isEmpty energies) then
                let was = Map.tryFind room glance.Value.Towers |> Option.map snd

                let refilled =
                    energies
                    |> List.exists (fun (id, energy) ->
                        was
                        |> Option.bind (Map.tryFind id)
                        |> Option.exists (fun before -> energy > before))

                let low = energies |> List.map snd |> List.min

                touchRoom owner room (fun row ->
                    { row with
                        TowerReads = row.TowerReads + 1
                        TowerLow = (if row.TowerReads = 0 then low else min row.TowerLow low)
                        TowerLast = energies |> List.sumBy snd
                        Refills = if refilled then Tally.add tick row.Refills else row.Refills
                    })

                glance.Value <-
                    { glance.Value with
                        Towers = Map.add room (tick, Fresh.mapOfList energies) glance.Value.Towers
                    }

            for shot in Map.tryFind room sight.Shots |> Option.defaultValue [] do
                match
                    seen.Towers |> List.tryFind (fun (id, _) -> id = shot.Tower),
                    ours |> List.tryFind (fun (creep, _) -> creep.Name = shot.Target)
                with
                | Some(_, from), Some(target, at) ->
                    touch owner (fun dossier ->
                        { dossier with
                            Aim = aimShot tick from target at sight.Hurt ours dossier.Aim
                        })
                | _ -> ()

            // Its ramparts' gain since the last reading.
            let now = seen.RivalRamparts
            let underFire = oursArmed > 0

            match Map.tryFind room glance.Value.Ramparts with
            | Some(was, fired, before) when tick > was ->
                match rampartGain (tick - was) before now with
                | Some(gained, rampartTicks, order) ->
                    touchRoom owner room (fun row ->
                        let row =
                            if fired || underFire then
                                { row with
                                    FireGain = row.FireGain + gained
                                    FireTicks = row.FireTicks + rampartTicks
                                }
                            else
                                { row with
                                    PeaceGain = row.PeaceGain + gained
                                    PeaceTicks = row.PeaceTicks + rampartTicks
                                }

                        match order with
                        | Some(picked, candidates) ->
                            { row with
                                LowestFirst = Aim.read picked candidates row.LowestFirst
                            }
                        | None -> row)
                | None -> ()
            | _ -> ()

            glance.Value <-
                { glance.Value with
                    Ramparts =
                        if Map.isEmpty now then
                            Map.remove room glance.Value.Ramparts
                        else
                            Map.add
                                room
                                (tick, underFire, Fresh.mapOfSeq (Map.toSeq now))
                                glance.Value.Ramparts
                })

        // Our armed body's visit, the rival's answer and its workers' flight.
        match Map.tryFind room glance.Value.Visits, holder with
        | None, Some owner when oursArmed > 0 ->
            let answered =
                rivals
                |> List.exists (fun hostile -> hostile.Owner = owner && armedBody hostile.Body)

            if answered then
                touchRoom owner room (fun row ->
                    { row with
                        Standing = Tally.add tick row.Standing
                    })

            glance.Value <-
                { glance.Value with
                    Visits =
                        Map.add
                            room
                            ({
                                Holder = owner
                                Began = tick
                                Answered = answered
                                Workers =
                                    rivals
                                    |> List.filter (fun hostile ->
                                        hostile.Owner = owner && workerBody hostile.Body)
                                    |> List.map _.Id
                                    |> Fresh.setOfSeq
                            }
                            : RivalVisit)
                            glance.Value.Visits
                }
        | Some visit, _ ->
            let present = rivals |> List.map _.Id |> Set.ofList
            let fallen = Set.ofList seen.Fallen

            // Fallen before its life ran out; one that died of age left the
            // count without fleeing or falling to anyone.
            let fell =
                visit.Workers
                |> Set.filter (fun id ->
                    fallen.Contains id
                    && Map.tryFind id glance.Value.Bodies
                       |> Option.exists (fun body -> tick < body.Ends))

            let fled =
                visit.Workers
                |> Set.filter (fun id -> not (present.Contains id || fallen.Contains id))

            let left = visit.Workers |> Set.filter present.Contains

            let answered =
                rivals
                |> List.exists (fun hostile ->
                    hostile.Owner = visit.Holder && armedBody hostile.Body)

            let answers = not visit.Answered && answered
            let ends = oursArmed = 0

            if not (Set.isEmpty fell && Set.isEmpty fled) || answers || ends then
                touch visit.Holder (fun dossier ->
                    { dossier with
                        Fell = Tally.addMany tick fell.Count dossier.Fell
                        Fled = Tally.addMany tick fled.Count dossier.Fled
                        Stayed =
                            if ends then
                                Tally.addMany tick left.Count dossier.Stayed
                            else
                                dossier.Stayed
                    })

            if answers then
                touchRoom visit.Holder room (fun row ->
                    { row with
                        Response = Span.add tick (tick - visit.Began) row.Response
                    })
            elif ends && not visit.Answered then
                touchRoom visit.Holder room (fun row ->
                    { row with
                        Unanswered = Tally.add tick row.Unanswered
                    })

            glance.Value <-
                { glance.Value with
                    Visits =
                        if ends then
                            Map.remove room glance.Value.Visits
                        else
                            Map.add
                                room
                                { visit with
                                    Answered = visit.Answered || answered
                                    Workers =
                                        if Set.count left = Set.count visit.Workers then
                                            visit.Workers
                                        else
                                            Fresh.setOfSeq left
                                }
                                glance.Value.Visits
                }
        | _ -> ()

        // Every rival body here: its first sighting, its crossing, its work.
        for hostile in rivals do
            let armed = armedBody hostile.Body
            let works = hasPart Work hostile.Body

            match Map.tryFind hostile.Id glance.Value.Bodies with
            | None when armed ->
                touch hostile.Owner (fun dossier ->
                    let shape = shapeOf hostile.Body

                    let dossier =
                        { dossier with
                            Armed = Tally.add tick dossier.Armed
                            Shapes =
                                Map.add
                                    shape
                                    ((Map.tryFind shape dossier.Shapes |> Option.defaultValue 0)
                                     + 1)
                                    dossier.Shapes
                        }

                    match Map.tryFind room sight.Homes with
                    | Some level ->
                        { dossier with
                            HomeRaiders =
                                Map.add
                                    level
                                    (Map.tryFind level dossier.HomeRaiders
                                     |> Option.defaultValue Tally.zero
                                     |> Tally.add tick)
                                    dossier.HomeRaiders
                        }
                    | None when ourReserve ->
                        { dossier with
                            OutpostRaiders = Tally.add tick dossier.OutpostRaiders
                        }
                    | None -> dossier)
            | Some body when armed && body.Room <> room ->
                let sortie = holds hostile.Owner body.Room && not (holds hostile.Owner room)

                touch hostile.Owner (fun dossier ->
                    { dossier with
                        Crossings = Tally.add tick dossier.Crossings
                        Sorties =
                            if sortie then
                                Tally.add tick dossier.Sorties
                            else
                                dossier.Sorties
                    })
            | _ -> ()

            glance.Value <-
                { glance.Value with
                    Bodies =
                        Map.add
                            hostile.Id
                            ({
                                Holder = hostile.Owner
                                Room = room
                                Ends = tick + hostile.TicksToLive
                                Armed = armed
                                Works = works
                            }
                            : RivalBody)
                            glance.Value.Bodies
                }

            if works then
                match rowOf hostile.Owner room with
                | Some row when row.KilledAt > 0 && tick > row.KilledAt ->
                    touchRoom hostile.Owner room (fun row ->
                        { row with
                            Unworked = Span.add tick (tick - row.KilledAt) row.Unworked
                            KilledAt = 0
                        })
                | _ -> ()

        // A worker fallen before its time in a room its owner's row holds,
        // none of its owner's left at work: the room's work stops until one
        // is seen again. The row and not this tick's holder, as the kill may
        // take the reservation with it.
        for id in seen.Fallen do
            match Map.tryFind id glance.Value.Bodies with
            | Some body ->
                glance.Value <-
                    { glance.Value with
                        Bodies = Map.remove id glance.Value.Bodies
                    }

                let working =
                    rivals
                    |> List.exists (fun hostile ->
                        hostile.Owner = body.Holder && hasPart Work hostile.Body)

                if
                    body.Works
                    && tick < body.Ends
                    && Option.isSome (rowOf body.Holder room)
                    && not working
                then
                    touchRoom body.Holder room (fun row ->
                        if row.KilledAt = 0 then
                            { row with KilledAt = tick }
                        else
                            row)
            | None -> ()

        // Its safe mode, and what stood around it when it was raised.
        match Map.tryFind room sight.Controllers with
        | Some controller when controller.Seen = tick && isRivalPlayer controller.Owner ->
            let owner = controller.Owner
            let dossier = Map.tryFind owner dossiers.Value

            let raised =
                if controller.SafeModeUntil > tick then
                    let at = controller.SafeModeUntil - safeModeDuration

                    if dossier |> Option.forall (fun dossier -> at > dossier.SafeModes.Last) then
                        Some at
                    else
                        None
                else
                    None

            let cooler =
                dossier
                |> Option.forall (fun dossier ->
                    controller.SafeModeCooldownUntil > dossier.CooldownUntil)

            if Option.isSome raised || cooler then
                touch owner (fun dossier ->
                    let dossier =
                        { dossier with
                            CooldownUntil =
                                max dossier.CooldownUntil controller.SafeModeCooldownUntil
                        }

                    match raised with
                    | Some at ->
                        let raisedSeen: SafeModeSeen =
                            {
                                Room = room
                                At = at
                                Noticed = tick
                                Level = controller.Level
                                Ours = List.length ours
                                OursArmed = oursArmed
                                Since =
                                    Map.tryFind room glance.Value.Visits
                                    |> Option.filter (fun visit -> visit.Holder = owner)
                                    |> Option.map (fun visit -> at - visit.Began)
                                Struck =
                                    Map.tryFind room glance.Value.Struck
                                    |> Option.map (fun kinds ->
                                        kinds
                                        |> Map.toList
                                        |> List.filter (fun (_, last) -> last <= at)
                                        |> List.map (fun (kind, last) -> kind, at - last))
                                    |> Option.defaultValue []
                            }

                        { dossier with
                            SafeModes = Tally.add at dossier.SafeModes
                            Activations = raisedSeen :: dossier.Activations
                        }
                    | None -> dossier)
        | _ -> ()

    // Every rival seen this tick, its last-seen tick.
    for owner in
        sight.Rooms
        |> Map.toSeq
        |> Seq.collect (fun (_, seen) -> seen.Hostiles |> Seq.filter rivalBody |> Seq.map _.Owner)
        |> Seq.distinct do
        touch owner id

    let dossiers = dossiers.Value

    let dossiers =
        if Map.count dossiers <= capRivals then
            dossiers |> Map.map (fun _ dossier -> capDossier dossier)
        else
            dossiers
            |> Map.toList
            |> List.sortByDescending (fun (owner, dossier) -> dossier.Latest, owner)
            |> List.truncate capRivals
            |> List.map (fun (owner, dossier) -> owner, capDossier dossier)
            |> Fresh.mapOfList

    let fresh (read: int) = tick - read <= glanceMargin

    {
        Dossiers = dossiers
        Glance =
            { glance.Value with
                Bodies = glance.Value.Bodies |> Map.filter (fun _ body -> body.Ends >= tick)
                Visits =
                    glance.Value.Visits
                    |> Map.filter (fun room _ -> Map.containsKey room sight.Rooms)
                Towers = glance.Value.Towers |> Map.filter (fun _ (read, _) -> fresh read)
                Ramparts = glance.Value.Ramparts |> Map.filter (fun _ (read, _, _) -> fresh read)
                Struck =
                    glance.Value.Struck
                    |> Map.filter (fun _ kinds -> kinds |> Map.exists (fun _ last -> fresh last))
            }
    }
