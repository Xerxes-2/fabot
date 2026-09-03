/// The observe channel's pure folds: the Transition log's, keyed by creep
/// (ADR 0009), and the Raid log's, colony-level and episodic (ADR 0028).
/// Change detection, the ring caps, timeline interleaving, dead-creep
/// pruning and the episode's quiet gap all live here; the App shell only
/// serializes the state to and from `Memory.fabot.observe`.
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
    }

/// The whole persisted Raid log.
type RaidState =
    {
        /// The episode ring: oldest first, trimmed from the front — the
        /// Transition log's own convention.
        Episodes: RaidEpisode list
        /// The owned creep names the previous tick projected, less the
        /// ones whose life ran out on it: the baseline this tick's losses
        /// are read against. Carried only while an episode is open, so a
        /// creep that dies in peacetime is read against an empty baseline
        /// and recorded nowhere.
        Living: Set<string>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RaidState =
    /// The empty Raid log: no episodes, no baseline — what an absent,
    /// malformed or foreign-shaped subtree reads as.
    let empty = { Episodes = []; Living = Set.empty }

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
/// Ramparts (#66) would, and this derivation is the thing that would then
/// have to change. An id the projection cannot place contributes nothing
/// (ADR 0004).
let private ourTiles (snapshot: Snapshot) =
    let structures =
        (snapshot.Refillables |> List.map (fun r -> r.Id))
        @ (snapshot.Controller |> Option.toList |> List.map (fun c -> c.Id))
        |> List.choose (fun id -> Map.tryFind id snapshot.Spatial.TargetPositions)

    (snapshot.Spatial.CreepPositions |> Map.toList |> List.map snd) @ structures

/// This tick's closest approach, if there is both a hostile and something
/// of ours to measure it against. The hostile-free tick is the common one
/// — every tick of an open episode's quiet gap is one — so it answers
/// before the owned set is built.
let private approachAt (snapshot: Snapshot) : Approach option =
    if List.isEmpty snapshot.Hostiles then
        None
    else
        let ours = ourTiles snapshot

        let measured =
            snapshot.Hostiles
            |> List.collect (fun hostile ->
                ours |> List.map (fun tile -> range hostile.Pos tile, hostile.Pos))

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

/// The Raid-log fold (ADR 0028): this tick's Snapshot plus the previous
/// Raid log produce the new one. An episode opens on the first tick a
/// spawn room holds a hostile, stays open while hostiles keep appearing,
/// and closes after `gap` quiet ticks; the ring keeps the newest `cap`
/// episodes. A tick with no hostile and no open episode records nothing.
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

            let episode =
                current
                |> Option.defaultValue
                    {
                        Opened = snapshot.Time
                        LastSeen = snapshot.Time
                        Roster = Map.empty
                        Closest = None
                        Losses = []
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
                }

    {
        Episodes =
            match episode with
            | Some episode -> earlier @ [ episode ] |> trim cap
            | None -> earlier
        Living = if Option.isSome episode then surviving else Set.empty
    }
