/// The Transition log's pure fold (ADR 0009): this tick's Verdicts plus the
/// previous observe state produce the new one. Change detection, the
/// per-creep ring cap, timeline interleaving, and dead-creep pruning all
/// live here; the App shell only serializes the state to and from
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
