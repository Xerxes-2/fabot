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
    | Verdict.Rerouted creep -> creep

let private isMovement =
    function
    | Verdict.Grounded _
    | Verdict.Yielded _
    | Verdict.Rerouted _ -> true
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
/// and what it is made of, counted from the Snapshot's verbatim part
/// list. A row, not the roster — the roster is the map of these.
type RosterRow =
    {
        Owner: string
        Body: Map<BodyPart, int>
    }

/// The smallest range recorded between any hostile and anything of ours,
/// with the tile the hostile stood on and the tick it was measured at —
/// the number that separates a probe at the room edge from a loss.
type Approach = { Range: int; Pos: Pos; Tick: int }

/// One owned creep gone while a hostile stood in the room, stamped at the
/// tick it was last seen alive. Recorded here precisely because the
/// Transition log's fold has already pruned it.
type Loss = { Creep: string; Tick: int }

/// One raid: opened on the first tick a spawn room held a hostile, kept
/// open while hostiles keep appearing, closed by a quiet gap.
type RaidEpisode =
    {
        /// The tick the episode opened.
        Opened: int
        /// The last tick a hostile actually stood in a spawn room. The
        /// episode stays open while `tick - LastSeen` is inside the quiet
        /// gap — openness is derived, never stored.
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
        Damage: int
    }

/// One [[outpost]]'s threat episode: the Raid log's second family (ADR
/// 0043), opened by an invader core standing in a room the colony works
/// rather than by a hostile creep in a spawn room, and carrying the one
/// field a raid never needed — the tick the [[stand-down]] it drives
/// expires.
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
/// Snapshot is never evidence that it is gone; and the stand-down's whole
/// effect is to withdraw the creeps whose vision would see it. Under the
/// gap a stood-down outpost goes quiet because nobody is looking, the
/// episode closes fifty ticks later, the gate opens, and the creeps walk
/// forty-seven tiles back into the same core — the ~150-tick oscillation
/// ADR 0043 exists to forbid, arriving through the episode's lifecycle
/// instead of through the gate's test. So silence closes nothing here, and
/// only the clock does.
let standingDown (tick: int) (episode: OutpostEpisode) = tick < episode.Expiry

/// The stand-down a threat gave no readable deadline for: 2,500 ticks, the
/// stronghold expansion period (ADR 0043). The last of the three answers
/// and the only one the colony chose rather than read, so it is the one
/// number here that had to be justified: it is the cadence on which the
/// thing that put the core there puts another one somewhere, and it errs
/// long by construction, which is the only direction the gate is allowed
/// to be wrong in — a stale stand-down costs an outpost's income until the
/// clock runs out, and the failure it prevents costs a creep a cycle for
/// the life of the core.
let standDownFallback = 2500

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
        /// The owned creep names the previous tick projected, less the
        /// ones whose life ran out on it: the baseline this tick's losses
        /// are read against. Carried only while an episode is open, so a
        /// creep that dies in peacetime is read against an empty baseline
        /// and recorded nowhere.
        Living: Set<string>
        /// The previous tick's hits per structure id across the Keep and
        /// the ramparts: the baseline this tick's damage is read against,
        /// carried exactly as `Living` is — only while an episode is open,
        /// so hits lost in peacetime are charged to nobody.
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

/// Ticks of silence that close an episode (ADR 0028). It has to outlast a
/// poke-and-heal cycle: giaco's squad in #66 stepped in for a tick or two
/// at the tower's minimum damage and back out to heal, over and over
/// across ~220 ticks, and that is one raid, not forty. Fifty ticks is
/// also about the round trip a squad retreating off-room makes before it
/// can be back — a shorter absence is the same squad still working the
/// room, a longer one is a decision to leave.
let quietGap = 50

/// The tiles of everything of ours a hostile can close on, as the
/// Snapshot can approximate the owned set: our creeps, and the owned
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
/// Of one room, named by the caller (ADR 0041). Range is Chebyshev over a
/// bare `Pos`, which carries no room, so a set unioned across the
/// projection would put one of ours in the outpost on the same coordinate
/// as a raider at home and record a raid that reached range 0 without a
/// creep of theirs ever standing beside a creep of ours. A room the
/// projection holds no layer for places nothing of ours, which is the same
/// answer as a room holding nothing (ADR 0004).
let private ourTilesIn (snapshot: Snapshot) (room: string) =
    let layer = SpatialInfo.layerOf snapshot.Spatial room

    let structures =
        (snapshot.Refillables |> List.map (fun r -> r.Id))
        @ (snapshot.Controller |> Option.toList |> List.map (fun c -> c.Id))
        |> List.choose (fun id -> Map.tryFind id layer.TargetPositions)

    (layer.CreepPositions |> Map.toList |> List.map snd) @ structures

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
let private approachAt (snapshot: Snapshot) : Approach option =
    if List.isEmpty snapshot.Hostiles then
        None
    else
        let ours =
            snapshot.Hostiles
            |> List.map (fun hostile -> hostile.RoomName)
            |> List.distinct
            |> List.map (fun room -> room, ourTilesIn snapshot room)
            |> Map.ofList

        let measured =
            snapshot.Hostiles
            |> List.collect (fun hostile ->
                Map.tryFind hostile.RoomName ours
                |> Option.defaultValue []
                |> List.map (fun tile -> range hostile.Pos tile, hostile.Pos))

        if List.isEmpty measured then
            None
        else
            let closest, pos = measured |> List.minBy fst

            Some
                {
                    Range = closest
                    Pos = pos
                    Tick = snapshot.Time
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
let private deadlineOf (snapshot: Snapshot) (core: InvaderCoreInfo) =
    match core.CollapseTick with
    | Some tick -> tick, StandDownBasis.CollapseTimer
    | None ->
        snapshot.RoomControl
        |> Map.tryFind core.RoomName
        |> Option.bind (fun control -> control.Reservation)
        |> Option.filter (fun held -> held.Holder = ReservationHolder.Invader)
        |> Option.map (fun held -> snapshot.Time + held.TicksToEnd, StandDownBasis.Reservation)
        |> Option.defaultValue (snapshot.Time + standDownFallback, StandDownBasis.Fallback)

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
let private deadlines (snapshot: Snapshot) =
    snapshot.InvaderCores
    |> List.map (fun core -> core.RoomName, deadlineOf snapshot core)
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

/// The Raid-log fold (ADR 0028): this tick's Snapshot plus the previous
/// Raid log produce the new one. An episode opens on the first tick a
/// spawn room holds a hostile, stays open while hostiles keep appearing,
/// and closes after `gap` quiet ticks; the ring keeps the newest `cap`
/// episodes. A tick with no hostile and no open episode records nothing.
/// Two baselines are carried across ticks while an episode is open and
/// dropped with it: the names this tick's losses are read against, and the
/// hits this tick's damage is (ADR 0034).
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
let foldRaids (cap: int) (gap: int) (snapshot: Snapshot) (prior: RaidState) : RaidState =
    // The baseline the next tick reads its losses against: this tick's
    // names, less the creeps whose clock runs out on it. A name gone
    // tomorrow because CREEP_LIFE_TIME ran down is old age, and this
    // record answers what a raid cost — TicksToLive says which is which
    // before the fact, so the difference never has to guess after it.
    let surviving =
        snapshot.Creeps
        |> List.filter (fun creep -> creep.TicksToLive > 1)
        |> List.map (fun creep -> creep.Name)
        |> Set.ofList

    // This tick's hits across the Keep and the ramparts, the next tick's
    // baseline. The kinds are the rule's, never a list of ids: a rampart
    // raised mid-episode joins it the tick it stands.
    let defended =
        snapshot.Spatial.Hits
        |> Map.toList
        |> List.choose (fun (id, hits) ->
            match Map.tryFind id snapshot.Spatial.TargetKinds with
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
        | last :: rest when snapshot.Time - last.LastSeen <= gap -> List.rev rest, Some last
        | _ -> prior.Episodes, None

    // The tick's losses: names the previous tick projected and this one
    // does not. A name is missing the tick *after* its creep died, so the
    // loss is stamped at the tick it was last seen alive — which is this
    // episode's last sighting, and only when that sighting was the
    // previous tick. So every Loss falls inside the window the episode
    // records, and a name that vanishes deeper into the quiet gap is
    // attrition the raid is not charged with. A freshly opened episode has
    // no baseline of its own: a creep it has not seen yet is not its loss.
    let lostSince (episode: RaidEpisode) =
        if snapshot.Time - episode.LastSeen > 1 then
            []
        else
            let names = snapshot.Creeps |> List.map (fun creep -> creep.Name) |> Set.ofList

            Set.difference prior.Living names
            |> Set.toList
            |> List.map (fun name ->
                {
                    Creep = name
                    Tick = episode.LastSeen
                })

    let episode =
        match current, snapshot.Hostiles with
        | None, [] -> None
        | current, hostiles ->
            let lost = current |> Option.map lostSince |> Option.defaultValue []

            // Damage is charged against the previous tick's baseline, and
            // only an episode that was already open has one: a freshly
            // opened episode is charged nothing on its opening tick, so
            // nothing crosses the seam between two episodes. The window is
            // the same one the losses are read over — a hostile standing
            // there now, or a sighting on the previous tick, the reading
            // that lags its cause by a tick — so the decay ticking away
            // through a fifty-tick quiet gap is charged to no raid.
            let damage =
                match current with
                | Some episode when
                    not (List.isEmpty hostiles) || snapshot.Time - episode.LastSeen <= 1
                    ->
                    lostHits
                | _ -> 0

            let episode =
                current
                |> Option.defaultValue
                    {
                        Opened = snapshot.Time
                        LastSeen = snapshot.Time
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
                            snapshot.Time
                    Roster = (episode.Roster, hostiles) ||> List.fold enrol
                    Closest = nearer episode.Closest (approachAt snapshot)
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
            (prior.Outposts, deadlines snapshot)
            ||> List.fold (fun episodes seen -> sight snapshot.Time seen episodes)
            |> trimOutposts cap snapshot.Time
        Living = if Option.isSome episode then surviving else Set.empty
        Hits = if Option.isSome episode then defended else Map.empty
    }

/// One tick's cost, as the engine measured it: the tick it was measured
/// on, and the milliseconds the bot had spent by the time it stopped
/// looking (ADR 0041). The tick number rides the row rather than being
/// implied by its place in the ring, because a tick the loop never
/// finished writes no row at all — a gap in the numbers is the one thing
/// this line can say that a bare list of costs cannot.
type CpuSample = { Tick: int; Ms: float }

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
/// 2MB Memory, which is what buys the longer window.
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
let foldCpu (cap: int) (tick: int) (ms: float) (prior: CpuState) : CpuState =
    {
        Ticks = prior.Ticks @ [ { Tick = tick; Ms = toMicrosecond ms } ] |> trim cap
    }
