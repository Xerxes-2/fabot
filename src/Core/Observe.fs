/// The observe channel's pure folds: the Transition log's, keyed by creep
/// (ADR 0009); the Raid log's, colony-level and episodic (ADR 0028); and
/// the CPU line's, one row per tick (ADR 0041). Change detection, the ring
/// caps, timeline interleaving, dead-creep pruning and the episode's quiet
/// gap all live here; the App shell only serializes the state to and from
/// `Memory.fabot.observe`.
module Fabot.Core.Observe

open Fabot.Core.Types

/// One recorded change in a creep's timeline: what happened and when.
type ObserveEntry = { Tick: int; Verdict: Verdict }

/// One creep's slice of the observe state.
type CreepLog =
    {
        /// The Transition log proper: oldest first, capped per creep.
        Entries: ObserveEntry list
        /// The task-channel Verdict the Matcher last spoke on this creep —
        /// the steady state new task Verdicts are judged against. A cursor
        /// beside the ring, not the ring's own tail: the cap can evict the
        /// entry that opened the steady state, and an unchanged Kept must
        /// stay quiet regardless of what the ring retains.
        LastTask: Verdict option
        /// The Scoring Verdict the tick before, if any — the verbose
        /// channel's own cursor. Scorings are episodic like movement: a
        /// tick without one means the creep is off the verbose list, so
        /// this cursor resets every tick and flipping verbose back on
        /// always records a fresh scoring, even an unchanged one — the
        /// investigator's confirmation the flip took effect.
        LastScoring: Verdict option
        /// The movement Verdicts the Resolver emitted for this creep last
        /// tick. Movement Verdicts are episodic — a tick without one means
        /// the creep moved freely — so continuation is judged against the
        /// previous tick, and this cursor resets every tick: a grounding
        /// held across ticks appends once, while a fresh grounding after
        /// free movement is a new event even when the last movement entry
        /// looks the same.
        LastMove: Verdict list
    }

/// Creep name -> that creep's timeline. The whole persisted observe state.
type ObserveState = Map<string, CreepLog>

/// The per-creep ring cap: conclusion-level entries at this cap across a
/// small colony stay well under the 2MB Memory (spec sanity: ~20 × ~20).
let capPerCreep = 20

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
    | Verdict.Stalled creep -> creep

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
/// anti-thrash steady state of the match already on record, so a creep
/// that keeps its Task writes nothing; everything else must match exactly.
let private sameSubstance a b =
    match a, b with
    | (Verdict.Matched(_, taskA, _) | Verdict.Kept(_, taskA)),
      (Verdict.Matched(_, taskB, _) | Verdict.Kept(_, taskB)) -> taskA = taskB
    | _ -> a = b

let private trim cap entries =
    let overflow = List.length entries - cap
    if overflow > 0 then List.skip overflow entries else entries

/// Fold one tick of a single creep's Verdicts, in emission order, into its
/// log: append each Verdict that is a change, stamp it with the tick, keep
/// the newest cap-many entries, and advance the channel cursors — the task
/// cursor to the tick's last task Verdict (task Verdicts are total, so it
/// only ever moves forward), the scoring and movement cursors to this
/// tick's Scoring and movement Verdicts as the episode baselines for the
/// next tick — both channels are episodic, so each resets on a tick that
/// brings none.
let private step cap tick (verdicts: Verdict list) (log: CreepLog) : CreepLog =
    let appended =
        (log, verdicts)
        ||> List.fold (fun log verdict ->
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

/// The Transition-log fold: this tick's Verdicts plus the previous observe
/// state produce the new one. An entry lands only when a Verdict changes a
/// creep's story; task and movement events interleave in one per-creep
/// timeline in tick order; each timeline is a ring capped at `cap`; and
/// only living creeps keep state — a dead creep's timeline is pruned, and
/// a Verdict naming a creep not alive (never emitted in practice) writes
/// nothing.
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
    |> Map.ofSeq

/// One row of an episode's roster (ADR 0028): one hostile, who owns it
/// and what it is made of, counted from the view's verbatim part
/// list. A row, not the roster — the roster is the map of these.
type RosterRow =
    {
        Owner: string
        Body: Map<BodyPart, int>
    }

/// The smallest range recorded between any hostile and anything of ours,
/// with the tile the hostile stood on and the tick it was measured at —
/// the number that separates a probe at the room edge from a loss.
///
/// The tile carries its room since #216 R3 (#204). The episode is
/// colony-level and names no room of its own (ADR 0028), so before this
/// the one coordinate it did record was unjoined: an operator reading
/// `observe.mjs raids` could not tell an [[outpost]]'s raid from a home
/// one, and the fold that picked the minimum measured every hostile
/// against every tile of ours whatever room either stood in. Both are the
/// same missing join, and `RoomPos` is it (ADR 0052 decision 2).
type Approach = { Range: int; Pos: RoomPos; Tick: int }

/// One owned creep gone while a hostile stood in the room, stamped at the
/// tick it was last seen alive. Recorded here precisely because the
/// Transition log's fold has already pruned it.
type Loss = { Creep: string; Tick: int }

/// One raid: opened on the first tick a room the colony works and can see
/// held a hostile — the spawn rooms alone until #201 — kept open while
/// hostiles keep appearing, closed by a quiet gap.
///
/// Colony-level as ADR 0028 made it, and so it names no room: a raid that
/// crosses a border is one episode. That much is the decision; what used
/// to make it a gap as well was `Closest` recording a tile with no room to
/// read it in, so an operator could not tell an [[outpost]] raid from a
/// home one. That closed with #204 — the approach is a `RoomPos` and
/// carries the room it was measured in (ADR 0052 decision 2).
type RaidEpisode =
    {
        /// The tick the episode opened.
        Opened: int
        /// The last tick a hostile actually stood in one of those rooms.
        /// The episode stays open while `tick - LastSeen` is inside the
        /// quiet gap — openness is derived, never stored.
        LastSeen: int
        /// Hostile id -> its row, unioned over the whole window: a squad
        /// reads as five rows however often it steps back in.
        Roster: Map<string, RosterRow>
        /// None while nothing of ours could be placed — absence is
        /// per-entry (ADR 0004), never a zero range.
        Closest: Approach option
        /// Owned creeps lost inside the window, oldest first — every one
        /// stamped at a tick between `Opened` and `LastSeen`.
        Losses: Loss list
        /// Hits lost across the Keep and the ramparts inside the window
        /// (ADR 0034): decreases summed tick over tick, repairs ignored.
        /// Read on the ticks the window covers and no others — a hostile
        /// standing there, or the tick straight after a sighting, exactly
        /// as a Loss is — so the decay of a long quiet gap is charged to
        /// nobody. Decay inside the raid's own ticks does ride along, at
        /// 3 hits a tick per rampart against the hundreds a raid takes.
        ///
        /// The one field of the episode that is **one room's** (#201).
        /// Every other reads whatever room the hostile stood in, but the
        /// Keep and its ramparts stand at home and nowhere else, so the
        /// ticks charged are the ticks a hostile stood *there* — a window
        /// held open from an outpost adds nothing here. Without that the
        /// ride-along above would be the whole number rather than a
        /// rounding error: an outpost-only episode takes zero hits at home
        /// and would still record this room's decay as its cost.
        Damage: int
    }

/// One [[outpost]]'s threat episode: the Raid log's second family (ADR
/// 0043), opened by an invader core standing in a room the colony works
/// rather than by a hostile creep, and carrying the one field a raid never
/// needed — the tick the [[stand-down]] it drives expires. Since #201 the
/// two families cover the same rooms and the difference is only what opens
/// them: a creep the engine's creep sweep answers with, or a structure it
/// never can.
///
/// A record of its own beside `RaidEpisode` rather than a widening of it,
/// for two reasons pointing the same way. The four things a raid episode
/// records cannot be read off a core at all: `InvaderCoreInfo` carries a
/// room and a deadline and nothing else (#133) — no id, no tile, no body,
/// no owner — because the projection grows a field the tick a reader
/// exists (ADR 0007) and the gate reads none of those, so a shared shape
/// would be four fields structurally empty for every row of this family.
/// And the spawn family's behaviour has to come through this change
/// byte-identical (#117): two rings and two shapes give that by
/// construction, where one shared list has to argue for it step by step —
/// the losses, the hits baseline and the reassembly are all written around
/// there being exactly one open episode, and two families are two.
type OutpostEpisode =
    {
        /// The room the core stands in, which is the whole of where: the
        /// gate admits or withholds a room, so the tile a core stands on
        /// is a fact nothing asks for, and W12S27 standing down says
        /// nothing about W13S28 (ADR 0043's independent gates).
        RoomName: string
        /// The tick this stand-down opened.
        Opened: int
        /// The last tick a core was actually seen in the room. A record
        /// for the reader and never the openness test — see
        /// `standingDown`: the colony stops looking the moment it
        /// withdraws, so silence here says nobody is there to look and
        /// never that the room is clear.
        LastSeen: int
        /// The **absolute** tick the stand-down runs to, read off the
        /// threat and not chosen (ADR 0043). Sampled only on the ticks a
        /// core is actually seen, because the creeps that pay for the
        /// vision are the ones the gate withdraws: after they leave this
        /// is the last number anybody read, and holding it is the whole
        /// mechanism.
        Expiry: int
        /// Which of ADR 0043's three deadlines `Expiry` came off. Carried
        /// because it cannot be recovered from the tick afterwards, and
        /// because "shut until 2,600" and "shut until 2,600 because
        /// nothing could be read" are different answers to an operator
        /// (#117).
        Basis: StandDownBasis
    }

/// Whether an outpost episode still holds its room shut at this tick: ADR
/// 0043's "re-entry is a clock running out, not a look", written as the
/// one place the family's openness is decided. The expiry tick is the
/// first tick the room may be re-entered.
///
/// This family is exempt from the quiet gap that closes a raid, and the
/// exemption is the load-bearing part rather than a shortcut. The gap
/// answers "has the squad left?" — a question about creeps that move, can
/// be watched leaving, and are watched by a colony sitting in the room
/// they came to. Neither half holds here: an invader core has 100,000
/// hits, spawns nothing at level 0 and never leaves, so its absence from a
/// view is never evidence that it is gone; and the stand-down's whole
/// effect is to withdraw the creeps whose vision would see it. Under the
/// gap a stood-down outpost goes quiet because nobody is looking, the
/// episode closes fifty ticks later, the gate opens, and the creeps walk
/// forty-seven tiles back into the same core — the ~150-tick oscillation
/// ADR 0043 exists to forbid, arriving through the episode's lifecycle
/// instead of through the gate's test. So silence closes nothing here, and
/// only the clock does.
let standingDown (tick: int) (episode: OutpostEpisode) = tick < episode.Expiry

/// The whole persisted Raid log.
type RaidState =
    {
        /// The episode ring: oldest first, trimmed from the front — the
        /// Transition log's own convention.
        Episodes: RaidEpisode list
        /// The outpost family's own ring beside it, oldest first as that
        /// one is (ADR 0043). Its own ring and not rows mixed into it,
        /// because a shared list is a shared depth: twenty spawn-room
        /// raids would evict the very episode holding an outpost shut and
        /// the gate would reopen in the middle of a stand-down — the
        /// failure ADR 0043 was written for, reached through the ring
        /// rather than through the clock (#117). So the `cap` the fold
        /// takes is a depth per family and never a total, and this ring
        /// trims by one further rule of its own (`trimOutposts`).
        Outposts: OutpostEpisode list
        /// The rooms whose controller, on the last tick the colony could
        /// see it, belonged to or was reserved by another player, each
        /// against the tick that look was taken on: ADR 0043's *other*
        /// withdrawal, the one that needs no clock, because a room
        /// somebody else holds has not been made dangerous — it has
        /// stopped being ours to work.
        ///
        /// The tick is not a clock and nothing compares against it. It is
        /// the trace the gate's closing leaves in the observe channel
        /// (#117's US-20): the clocked family dates itself with `Opened`,
        /// and without one of its own this half could not answer the
        /// question that channel exists for — which tick the income of a
        /// room stopped arriving on. The first look's tick and not the
        /// last, because that is the tick the gate shut; a room already
        /// in the map keeps the tick it entered on.
        ///
        /// A remembered conclusion and not a per-tick reading, which is
        /// the whole reason it is persisted at all. The judgement itself
        /// needs vision (ADR 0004: who holds a room nobody is looking into
        /// is not a fact this tick), and the gate's own effect is to
        /// withdraw the creeps that pay for the vision — so a gate that
        /// re-read this off the view alone would reopen the room on
        /// the tick after it shut it, and the creeps would walk back into
        /// a room somebody else owns, for ever. That is the same
        /// oscillation `standingDown` refuses for the clocked family,
        /// arriving through the other trigger, and the answer is the same
        /// shape: hold the last conclusion until a tick with vision
        /// replaces it. This leaf is the only state the colony keeps per
        /// room (#117), which is why the conclusion lives here beside the
        /// episodes rather than in a channel of its own.
        ///
        /// Not a `StandDownBasis` and not an episode: there is no expiry
        /// for a basis to explain and no window to close, and an episode
        /// with an unreachable clock would be a stand-down whose "until"
        /// is a lie.
        ///
        /// Named for the vocabulary the rest of the colony already uses
        /// for another player — `Ownership.Rival`, `ReservationHolder.Rival`
        /// — rather than a second word for it (`docs/agents/domain.md`).
        RivalHeld: Map<string, int>
        /// The owned creep names the previous tick projected, less the
        /// ones whose life ran out on it: the baseline this tick's losses
        /// are read against. Carried only while an episode is open, so a
        /// creep that dies in peacetime is read against an empty baseline
        /// and recorded nowhere.
        ///
        /// This colony's names since #191 and no longer the world's, since
        /// a view carries one colony's creeps (ADR 0047) — which is
        /// why the difference `foldRaids` takes is against the world's
        /// living names and not against this list's successor: a creep
        /// another colony adopted leaves the baseline without dying.
        Living: Set<string>
        /// The previous tick's hits per structure id across the Keep and
        /// the ramparts: the baseline this tick's damage is read against,
        /// carried as `Living` is — only while an episode is open, so hits
        /// lost in peacetime are charged to nobody — and, since #201, only
        /// on a tick a hostile stood in the [[home room]]. The Keep and its
        /// ramparts stand in one room (ADR 0034) and the episode above them
        /// now spans every room the colony works, so the baseline is what
        /// keeps the two the same room's: a tick with the raid a border
        /// away leaves none, and the next tick differences against nothing.
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
            Living = Set.empty
            Hits = Map.empty
        }

/// The episode ring cap: one ring for the whole colony rather than one
/// per creep, so twenty episodes the size of #66's raid — a five-row
/// roster, one approach, a handful of losses, a few hundred bytes each —
/// sit around 10KB against the 2MB Memory. Twenty raids is more history
/// than stays actionable, and it is the number the sibling channel
/// already keeps.
let capEpisodes = 20

/// The tiles of everything of ours a hostile can close on, as far as a
/// view can approximate the owned set: our creeps, and the owned
/// structures it already places — the Refillables (spawn, extensions,
/// tower) and the controller. Roads and containers cannot be owned in the
/// engine; the Storage can, and is left out, because ADR 0022 puts it on
/// the cluster's first pick — a tile or two from the spawn, inside the
/// ring the Refillables already cover — so it moves no measured minimum.
/// The ramparts stand now (ADR 0034) and are deliberately still out: they
/// cover the Keep and the Posts, tiles this measure already reaches
/// through the Refillables, and a rampart over a Post would pull the
/// number out to the sources and answer a different question. What the
/// Keep is losing is watched through its hits instead — the damage below.
/// An id the projection cannot place contributes nothing (ADR 0004).
///
/// Of one room, named by the caller (ADR 0041), and handed back joined to
/// it (ADR 0052 decision 2): a set unioned across the projection would put
/// one of ours in the outpost on the same coordinate as a raider at home
/// and record a raid that reached range 0 without a creep of theirs ever
/// standing beside a creep of ours. Since #216 R3 the join is the tiles'
/// own type and `RoomPos.range` refuses the measure across a border, where
/// before it was this function's caller remembering to key by room. A room
/// the projection holds no layer for places nothing of ours, which is the
/// same answer as a room holding nothing (ADR 0004).
let private ourTilesIn (view: ColonyView) (room: string) : RoomPos list =
    let layer = SpatialInfo.layerOf view.Spatial room

    let structures =
        (view.Refillables |> List.map (fun r -> r.Id))
        @ (view.Controller |> Option.toList |> List.map (fun c -> c.Id))
        |> List.choose (fun id -> Map.tryFind id layer.TargetPositions)

    (layer.CreepPositions |> Map.toList |> List.map snd) @ structures
    |> List.map (RoomPos.at room)

/// This tick's closest approach, if there is both a hostile and something
/// of ours to measure it against. The hostile-free tick is the common one
/// — every tick of an open episode's quiet gap is one — so it answers
/// before the owned set is built.
///
/// Each hostile is measured against the room it stands in and no other,
/// and the owned set is built once per room a hostile is in rather than
/// once per hostile: one room in the single-spawn colony ADR 0005 assumes,
/// and the right shape already for the tick a hostile stands somewhere
/// else (#117).
let private approachAt (view: ColonyView) : Approach option =
    if List.isEmpty view.Hostiles then
        None
    else
        let ours =
            view.Hostiles
            |> List.map (fun hostile -> hostile.Pos.Room)
            |> List.distinct
            |> List.map (fun room -> room, ourTilesIn view room)
            |> Map.ofList

        let measured =
            view.Hostiles
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

/// Union one sighting into the roster: the first sighting of an id wins,
/// so a row records the body that entered the room rather than what the
/// tower left of it, and a creep that steps back in is one row still.
let private enrol roster (hostile: HostileInfo) =
    if Map.containsKey hostile.Id roster then
        roster
    else
        Map.add
            hostile.Id
            {
                Owner = hostile.Owner
                Body = hostile.Body |> List.countBy id |> Map.ofList
            }
            roster

/// The tick one core's stand-down runs to, and which of ADR 0043's three
/// deadlines it was read off, in the ADR's own order of availability.
///
/// The reservation branch takes the core's own hold and nobody else's. A
/// rival's is the *clockless* withdrawal and never a clock, and the
/// colony's own says nothing about the core at all — which is why the
/// holder arrived as three answers rather than as one "not ours" flag
/// (#133). `TicksToEnd` is the engine's relative count, so the tick it
/// names is this one plus it: the same addition the shell already made on
/// the collapse-timer branch, made here instead because that field is
/// carried relative for the reserver row that sizes itself off "how much
/// is left" (#133). Storing it as read would shut an outpost until a tick
/// a hundred thousand in the past.
///
/// And that branch may never answer *earlier* than the fallback — the
/// amendment ADR 0043 took in #136, where the gate that reads this was
/// built and a short clock first became observable. A core outlives the
/// hold it takes: it re-reserves the controller the tick the hold lapses,
/// so the end of a reservation is never the end of the core — where a
/// collapse timer's end is exactly that, the tick the engine takes the
/// stronghold and its core away, which is why the branch above needs no
/// such floor. A hold with a handful of ticks left says only what the
/// core did last tick, and the engine hands out exactly that: a core that
/// has *just*
/// `attackController`'d a controller nobody reserved holds it for three
/// ticks (`invader-core/reserveController.js`: `endTime = gameTime + 1`
/// plus `INVADER_CORE_CONTROLLER_POWER × CONTROLLER_RESERVE`, #117), so
/// read literally this branch would answer a three-tick stand-down — the
/// "immediately" ADR 0043's own user story says no path may reach. The
/// 5,000-tick hold of a settled core still reads through unchanged; below
/// the fallback the read is not a deadline at all and the clock is the one
/// the colony chose, which the basis then says out loud. Errs long, the
/// one direction the gate may be wrong in.
let private deadlineOf (view: ColonyView) (core: InvaderCoreInfo) =
    match core.CollapseTick with
    | Some tick -> tick, StandDownBasis.CollapseTimer
    | None ->
        view.RoomControl
        |> Map.tryFind core.RoomName
        |> Option.bind (fun control -> control.Reservation)
        |> Option.filter (fun held -> held.Holder = ReservationHolder.Invader)
        |> Option.filter (fun held -> held.TicksToEnd >= view.Tuning.StandDownFallback)
        |> Option.map (fun held -> view.Time + held.TicksToEnd, StandDownBasis.Reservation)
        |> Option.defaultValue (view.Time + view.Tuning.StandDownFallback, StandDownBasis.Fallback)

/// This tick's deadline for each room a core was seen in — the sighting
/// the fold below folds, and the only tick on which a stand-down's clock
/// moves at all. A room with no entry here is a room the colony cannot
/// see or one nothing stands in, and those two read alike on purpose:
/// neither is evidence, so neither touches an episode (ADR 0004,
/// `standingDown`).
///
/// One entry per room and never per core: the gate withholds a room, so
/// two cores in one room are one stand-down, and the later of their
/// deadlines is the one kept — the direction ADR 0043 allows the gate to
/// be wrong in.
let private deadlines (view: ColonyView) =
    view.InvaderCores
    |> List.map (fun core -> core.RoomName, deadlineOf view core)
    |> List.groupBy fst
    |> List.map (fun (room, seen) -> room, seen |> List.map snd |> List.maxBy fst)

/// Fold one room's sighting into the outpost ring: the room's standing
/// episode takes it — its window extends and its clock is re-read, since
/// this is a tick with vision — and where the room has none standing, the
/// sighting opens one. At most one episode per room can be standing, so
/// the re-read lands on one row.
///
/// The re-read only ever moves a running clock outward: the later of the
/// recorded deadline and this tick's is the one kept, and the basis with
/// it, so the record still names the read that is actually holding the
/// room. That is `deadlines`' rule above applied across ticks rather than
/// within one, and for the same reason — the sighting that lands on a
/// worse deadline is real (the core drains our hold and takes its own,
/// freshly at a handful of ticks; or our reserver takes it back and the
/// read falls through to the fallback), and reading it in would cut a
/// stand-down short, which is the direction ADR 0043's Consequences
/// forbid: a stale stand-down costs an outpost's income until its clock
/// runs out, and the failure it prevents costs a creep a cycle for the
/// life of the core.
let private sight tick (room, (expiry, basis)) (episodes: OutpostEpisode list) =
    let holds (episode: OutpostEpisode) =
        episode.RoomName = room && standingDown tick episode

    if episodes |> List.exists holds then
        episodes
        |> List.map (fun episode ->
            if holds episode then
                { episode with
                    LastSeen = tick
                    Expiry = max episode.Expiry expiry
                    Basis = if expiry > episode.Expiry then basis else episode.Basis
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
            }
        ]

/// Trim the outpost ring to the cap, oldest first — and never over an
/// episode that is still standing down.
///
/// The ring is there to bound Memory, and the row it would drop first is
/// the one holding creeps out of a room: evicting that reopens the gate
/// mid-stand-down, which is exactly the failure ADR 0043 is written
/// against, reached through the ring rather than through the clock. So a
/// standing episode is never a candidate and the ring may run past its cap
/// — by at most one row per room a core stands in, and the rooms are the
/// scan set's, a declaration a human moves (ADR 0041), so the overrun is
/// bounded by a constant in the source. It is paid back out of the
/// finished stand-downs behind it the moment there are any.
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

/// Whether the room this control entry answers for is another player's:
/// ADR 0043's clockless withdrawal, and the community's one unanimous
/// abandonment rule. Owned *or* reserved, because the two are one fact to
/// this rule — the room is being worked by somebody else — where they are
/// two facts to the economics beside it (ADR 0042 prices a rival's hold
/// at the neutral five and an owner's the same, for this reason).
///
/// The NPC's hold is deliberately not here. It is the *clock* of the
/// family above, and a core reserving the room it stands in would
/// otherwise shut that room for the life of the colony under a rule that
/// never re-opens — which is exactly the input `ReservationHolder`'s three
/// states exist to keep separable (#133).
let private rivalHeld (control: RoomControlInfo) =
    control.Owner = Ownership.Rival
    || control.Reservation
       |> Option.exists (fun held -> held.Holder = ReservationHolder.Rival)

/// The rooms the [[stand-down]] gate withholds from the scan set this
/// tick (ADR 0043), read off the previous tick's log: every room a
/// stand-down's clock is still running in, and every room the colony last
/// saw in another player's hands.
///
/// This is the one reader that *acts* on the Raid log, and ADR 0028's "a
/// record to be read, never a signal sent" is narrowed here and exactly
/// once. Everything else still reads the view.
///
/// A set of room names and not a decision: what the shell does with it is
/// drop those rooms from the declarations it works (`Outpost.worked`), so
/// they never enter the [[spatial projection]] at all. No Task pools
/// there, no quota counts them, nothing walks toward them — the whole of
/// "withdraw" in an architecture that keeps no state and recomputes every
/// tick (ADR 0004). A room named here that is not a declared outpost
/// narrows nothing, because the declarations are the only thing narrowed:
/// the home room can never be gated out by a rule about who holds it.
let standDown (tick: int) (state: RaidState) : Set<string> =
    state.Outposts
    |> List.filter (standingDown tick)
    |> List.map (fun episode -> episode.RoomName)
    |> Set.ofList
    |> Set.union (state.RivalHeld |> Map.toList |> List.map fst |> Set.ofList)

/// The Raid-log fold (ADR 0028): this tick's view plus the previous
/// Raid log produce the new one. An episode opens on the first tick a room
/// the colony works and can see holds a hostile — the spawn rooms alone
/// until #201, which is what left an outpost's raiders unrecorded — stays
/// open while hostiles keep appearing, and closes after
/// `Tuning.QuietGap` quiet ticks;
/// the ring keeps the newest `cap` episodes. A tick with no hostile and no
/// open episode records nothing. Two baselines are carried across ticks
/// while an episode is open and dropped with it: the names this tick's
/// losses are read against, and the hits this tick's damage is (ADR 0034)
/// — and that second one is carried only on the ticks a hostile stands in
/// the [[home room]], because the Keep it measures stands there alone.
///
/// Beside all of that, and sharing nothing with it but the leaf they are
/// written to, the outpost family (ADR 0043): an invader core seen in a
/// room the colony works opens or extends that room's stand-down and sets
/// the tick it runs to, in its own ring, on its own clock, with no quiet
/// gap. The two halves are deliberately disjoint — the family above reads
/// `Hostiles` and this one reads `InvaderCores`, and the engine's own
/// sweeps guarantee no object is in both, a core being a structure — so a
/// core changes nothing a raid records and a raid changes nothing a
/// stand-down does. `cap` is read as a depth per family and not a total,
/// for the reason `RaidState.Outposts` gives.
///
/// And beside both, one remembered conclusion rather than an episode:
/// which rooms the colony last saw in another player's hands
/// (`RaidState.RivalHeld`), ADR 0043's withdrawal that carries no clock —
/// which still dates itself, because the tick a gate shut on is the trace
/// #117's US-20 asks this channel for.
///
/// `alive` is every creep name the *world* still holds — `Game.creeps`,
/// the same set `fold` prunes timelines against — and not this colony's
/// fleet, because since #191 a view carries one colony's creeps and a
/// name can leave that list without anything dying: a creep standing in a
/// room only another colony projects is **adopted** by it for the tick
/// (ADR 0047 decision 2). Read against the colony's own list, adoption
/// would be written into its caster's log as a casualty, which is the one
/// confident falsehood this channel is built never to print. The baseline
/// stays the colony's — it is what the next tick's difference is taken
/// *from* — and only the subtraction is the world's.
let foldRaids (cap: int) (alive: Set<string>) (view: ColonyView) (prior: RaidState) : RaidState =
    // The silence that closes an episode is the colony's own tunable and
    // arrives on its view (ADR 0052 decision 5), where the ring's depth is
    // still the caller's: one is a judgement about how long a squad's
    // absence has to run before it is a departure, and the other is how
    // much history a Memory leaf may hold.
    let gap = view.Tuning.QuietGap

    // The baseline the next tick reads its losses against: this tick's
    // names, less the creeps whose clock runs out on it. A name gone
    // tomorrow because CREEP_LIFE_TIME ran down is old age, and this
    // record answers what a raid cost — TicksToLive says which is which
    // before the fact, so the difference never has to guess after it.
    let surviving =
        view.Creeps
        |> List.filter (fun creep -> creep.TicksToLive > 1)
        |> List.map (fun creep -> creep.Name)
        |> Set.ofList

    // The hostiles standing in the room the defences are in. Since #201
    // the sweep behind `ColonyView.Hostiles` covers every room the colony
    // works and can see, so "a hostile" and "a hostile where the Keep is"
    // are two different questions, and the damage below asks the second —
    // through the baseline it differences against, which is carried on
    // this list and not on the episode's. A rampart cannot stand in an
    // outpost and the Keep is home's by
    // definition (ADR 0034), so a window opened by a raider a border away
    // would charge this room's ordinary decay — 3 hits a tick per rampart
    // — to a raid that never touched it. The episode itself is still the
    // colony's and opens on any of them, which is the whole point of the
    // widening; it is the *measure* that is one room's.
    let atHome =
        let home = SpatialInfo.homeName view.Spatial
        view.Hostiles |> List.filter (fun hostile -> hostile.Pos.Room = home)

    // This tick's hits across the Keep and the ramparts, the next tick's
    // baseline. The kinds are the rule's, never a list of ids: a rampart
    // raised mid-episode joins it the tick it stands.
    let defended =
        view.Spatial.Hits
        |> Map.toList
        |> List.choose (fun (id, hits) ->
            match Map.tryFind id view.Spatial.TargetKinds with
            | Some(Structure kind) when isDefence kind -> Some(id, hits.Hits)
            | _ -> None)
        |> Map.ofList

    // Hits lost since the previous tick's baseline: decreases summed,
    // increases ignored — a repair is not negative damage. A structure the
    // baseline does not carry costs nothing, which is what keeps a rampart
    // raised mid-raid from reading as damage on the tick it stands, and
    // one destroyed outright charged only what it lost while it stood.
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

    // The tick's losses: names the previous tick projected that the world
    // no longer holds. A name is missing the tick *after* its creep died, so the
    // loss is stamped at the tick it was last seen alive — which is this
    // episode's last sighting, and only when that sighting was the
    // previous tick. So every Loss falls inside the window the episode
    // records, and a name that vanishes deeper into the quiet gap is
    // attrition the raid is not charged with. A freshly opened episode has
    // no baseline of its own: a creep it has not seen yet is not its loss.
    //
    // Differenced against `alive` and never against this colony's own
    // names: a body that left this view because another colony adopted
    // it is still standing (ADR 0047 decision 2), and a raid is not charged
    // for a creep that merely changed hands.
    let lostSince (episode: RaidEpisode) =
        if view.Time - episode.LastSeen > 1 then
            []
        else
            Set.difference prior.Living alive
            |> Set.toList
            |> List.map (fun name ->
                {
                    Creep = name
                    Tick = episode.LastSeen
                })

    let episode =
        match current, view.Hostiles with
        | None, [] -> None
        | current, hostiles ->
            let lost = current |> Option.map lostSince |> Option.defaultValue []

            // Damage is charged against the previous tick's baseline, and
            // only an episode that was already open has one: a freshly
            // opened episode is charged nothing on its opening tick, so
            // nothing crosses the seam between two episodes.
            //
            // Which ticks are inside the window is the *baseline's*
            // question and is answered once, where it is carried (#201): a
            // tick that carries none leaves this difference nothing to
            // subtract, so the window is exactly the ticks a hostile stood
            // in the room the Keep is in, plus the one after — the reading
            // that lags its cause by a tick, the shape a Loss is read over
            // too. That is what keeps the decay of a fifty-tick quiet gap,
            // and the decay under a raid a border away, charged to no
            // raid.
            let damage = if Option.isSome current then lostHits else 0

            let episode =
                current
                |> Option.defaultValue
                    {
                        Opened = view.Time
                        LastSeen = view.Time
                        Roster = Map.empty
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
                    Roster = (episode.Roster, hostiles) ||> List.fold enrol
                    Closest = nearer episode.Closest (approachAt view)
                    Losses = episode.Losses @ lost
                    Damage = episode.Damage + damage
                }

    {
        Episodes =
            match episode with
            | Some episode -> earlier @ [ episode ] |> trim cap
            | None -> earlier
        // The outpost family's whole tick: the rooms a core was seen in,
        // each folded into the ring. A tick that sees none — no vision, or
        // vision and a room that is clear — leaves every stand-down
        // exactly as it found it, clock included.
        Outposts =
            (prior.Outposts, deadlines view)
            ||> List.fold (fun episodes seen -> sight view.Time seen episodes)
            |> trimOutposts cap view.Time
        // The clockless withdrawal's memory, moved by the ticks with
        // vision alone: a room the colony can see this tick answers for
        // itself either way — it joins the set or it leaves it — and a
        // room with no `RoomControl` entry keeps whatever the last look
        // concluded. Absence is not evidence in either direction (ADR
        // 0004), and here it is the load-bearing half: the room this holds
        // shut is one nothing is looking into, and re-reading it as "free"
        // would walk the colony straight back into somebody else's room.
        //
        // No ring and no cap. The map is bounded by the rooms the colony
        // scans — the declaration a human moves (ADR 0041) plus the home
        // room — because only a room with a `RoomControl` entry can ever
        // enter it, and a scanned room is the only kind that gets one.
        //
        // The tick each room carries is the one it *entered* on and is
        // never refreshed: it dates the closing of the gate for the
        // observe channel (#117's US-20), and a room already held keeps
        // the answer it came in with. Nothing compares against it — this
        // withdrawal has no clock — so a stale one costs nothing and a
        // moved one would cost the only date there is.
        RivalHeld =
            (prior.RivalHeld, view.RoomControl)
            ||> Map.fold (fun rooms room control ->
                if rivalHeld control then
                    if Map.containsKey room rooms then
                        rooms
                    else
                        Map.add room view.Time rooms
                else
                    Map.remove room rooms)
        Living = if Option.isSome episode then surviving else Set.empty
        // The damage baseline, carried on the same condition the damage is
        // charged on (#201): an open episode *and* a hostile in the room
        // the Keep stands in. A tick that carries none leaves the next one
        // nothing to difference against, so an episode held open from an
        // outpost charges zero rather than this room's decay — and the one
        // tick after a raider leaves *this* room still finds the baseline
        // it needs, which is the lag reading the damage arm above owes the
        // last blow. Dropped with the episode either way, as `Living` is.
        Hits =
            if Option.isSome episode && not (List.isEmpty atHome) then
                defended
            else
                Map.empty
    }

/// What `Game.cpu.getUsed()` answered at each of the loop's phase
/// boundaries, in the order the tick ran them, plus the intents the engine
/// accepted (#170). Cumulative, every one of them, because that is what the
/// engine's counter is: the differencing is this module's job and happens
/// once, here, rather than in each of the readers.
///
/// `AtEntry` is the odd one out and the reason the split was built. It is
/// read on the loop's first line, so it is not a phase of the bot's at all
/// — it is what the engine had already spent before `loop` was entered,
/// and it stands on its own in every readout.
///
/// The order is the loop's own rather than the tick's four nouns in the
/// obvious sequence: the Memory writes land *before* `Executor.run`,
/// deliberately — a throw inside the Executor must not discard the tick's
/// anti-thrash state — so `AtSave` is read between `decide` and the
/// intents rather than after them.
type CpuReadings =
    {
        AtEntry: float
        AtSnapshot: float
        AtDecide: float
        AtSave: float
        AtExecute: float
        Intents: int
    }

/// One tick's cost, split at the loop's phase boundaries: the engine's
/// prelude and then four differences, each in milliseconds, and the count
/// of intents the engine accepted that tick (#170).
///
/// The split exists to attribute a gap the ruler cannot see. `npm run
/// profile` measures the same scenario at 10.45 ms/tick while the live
/// colony's line reads a 49.4 ms mean — 4.7×, where #97's era was 3× — and
/// the harness has no engine: no 0.2 CPU per intent, no prelude, no Memory
/// parse. A single total cannot say which of those the missing 1.6× is, so
/// every row carries the phases and the intent count, and the arithmetic is
/// left to whoever reads it (ADR 0041: measured, not budgeted).
///
/// Each phase is the ground between two of the loop's boundaries and not a
/// noun's price, which is what a reader attributing the gap has to hold on
/// to. `Snapshot` carries the Memory parse — `Memory` deserializes on the
/// loop's first touch of it, which is the Raid log's read inside that phase
/// — so the parse is fused with the shell's `find` sweep and not with the
/// prelude. `Save` spans the observe folds as well as the writes they feed,
/// because the boundary sits where the writes end, not where they begin.
type CpuPhases =
    {
        Entry: float
        Snapshot: float
        Decide: float
        Save: float
        Execute: float
        Intents: int
    }

/// One tick's cost, as the engine measured it: the tick it was measured
/// on, the milliseconds the bot had spent by the time it stopped looking
/// (ADR 0041), and — for a row this bundle wrote — where those went.
/// The tick number rides the row rather than being implied by its place in
/// the ring, because a tick the loop never finished writes no row at all —
/// a gap in the numbers is the one thing this line can say that a bare list
/// of costs cannot.
///
/// The phases are an option and not a record of zeros: a row written before
/// #170 was never split, and a zero would say the phase cost nothing rather
/// than that nobody measured it. Nothing this fold writes is ever `None` —
/// the absence arrives only off the wire, and the ring replaces itself with
/// split rows within one window of a deploy.
type CpuSample =
    {
        Tick: int
        Ms: float
        Phases: CpuPhases option
    }

/// The whole persisted CPU line: oldest first, capped, exactly as the
/// other two rings are. A record rather than a bare list so the leaf can
/// grow a second key without moving the one that is there.
type CpuState = { Ticks: CpuSample list }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CpuState =
    /// The empty CPU line — what an absent, malformed or foreign-shaped
    /// leaf reads as, the way `RaidState.empty` is.
    let empty = { Ticks = [] }

/// The CPU line's ring cap. ADR 0041 states the condition to revisit the
/// layered projection as two numbers — a mean tick above 50 ms, or any
/// single tick above 80 — and both are read off this window, so the
/// window has to be long enough that the mean is not one tick's opinion.
/// The census-keyed memos (ADR 0017, ADR 0032) make one tick in a while
/// several times the cost of its neighbours; at the sibling channels'
/// twenty that recompute is a twentieth of the mean, and at a hundred it
/// is a percent. A hundred rows of `{ t, ms }` is about 2.5KB against the
/// 2MB Memory, which is what buys the longer window. The phases (#170)
/// take a row from 22 bytes of JSON to 109 — about 11KB for the window,
/// against a whole observe subtree that measured 26.6KB the day they were
/// added — and the window is what buys the attribution, so the ring keeps
/// its hundred.
let capCpuTicks = 100

/// The measured cost, kept to the microsecond. The engine hands back a
/// float with more digits than anyone reads and Memory pays for every one
/// of them; a microsecond is finer than the profiler's own 100µs sampling
/// interval, so nothing a reader could act on is rounded away. Written as
/// arithmetic rather than as `Math.Round`, whose .NET and JS answers part
/// company on a half.
let private toMicrosecond (ms: float) = floor (ms * 1000.0 + 0.5) / 1000.0

/// The CPU line's fold (ADR 0041): this tick's cost joins the ring, oldest
/// first, and the newest `cap` rows survive. No change detection and no
/// episode — every tick costs something and the whole point is the shape
/// of the distribution, so unlike the Transition log a quiet tick still
/// writes a row.
///
/// The judgement over the ring is deliberately not here. ADR 0041 decided
/// CPU is *measured, not budgeted* — a budget exists to size a territory
/// and this colony's territory is a constant — so the two thresholds are
/// the reader's and live with the readers (`scripts/cpu-trigger.mjs`).
/// A threshold in Core is a thing the bot could act on, and the bot must
/// not: skipping a tick collides with the safe-mode reflex (ADR 0007,
/// ADR 0015), which has to be able to fire on the very tick a guard would
/// skip.
///
/// The readings arrive cumulative and are differenced here (#170), which is
/// the one place they can be: the shell reads a counter at each boundary
/// and knows nothing else, and a reader handed cumulative numbers would
/// have to subtract them again — every reader, the same way, off a shape
/// nothing pins. The tick's total stays `AtExecute`, the last reading, so
/// the row the trigger judges is the number it always was.
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
        }

    {
        Ticks =
            prior.Ticks
            @ [
                {
                    Tick = tick
                    Ms = toMicrosecond readings.AtExecute
                    Phases = Some phases
                }
            ]
            |> trim cap
    }
