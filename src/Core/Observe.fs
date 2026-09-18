/// The observe channel's pure folds: the Transition log's, keyed by creep (ADR
/// 0009); the Raid log's, colony-level and episodic (ADR 0028); the CPU line's,
/// one row per tick (ADR 0041); and the [[breach log]]'s, one row per live
/// invariant violation folded with its age (#355).
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

/// Fold one tick of a single creep's Verdicts, in emission order, into its
/// log: append each Verdict that is a change, stamp it with the tick, keep the
/// newest cap-many entries, and advance the channel cursors. The task cursor
/// only ever moves forward, task Verdicts being total; the scoring and
/// movement cursors are episode baselines and reset on a tick that brings none.
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
    |> Map.ofSeq

/// One visible reading of the sector Reactor programme. The shell derives it
/// from the one declared Reactor and the Storage of the colony feeding it;
/// `None` at the fold boundary means the room is blind (#320).
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

/// One row of an episode's roster (ADR 0028): one hostile, who owns it
/// and what it is made of, counted from the view's verbatim part
/// list. A row, not the roster — the roster is the map of these.
type RosterRow =
    {
        Owner: string
        Body: Map<BodyPart, int>
    }

/// The smallest range recorded between any hostile and anything of ours, with
/// the tile the hostile stood on and the tick it was measured at — the number
/// that separates a probe at the room edge from a loss. The tile carries its
/// room (#204, ADR 0052 decision 2). The episode is colony-level and names no
/// room of its own (ADR 0028), so unjoined this coordinate could not tell an
/// [[outpost]]'s raid from a home one, and the fold measured every hostile
/// against every tile of ours whatever room either stood in.
type Approach = { Range: int; Pos: RoomPos; Tick: int }

/// One owned creep gone while a hostile stood in the room, stamped at the
/// tick it was last seen alive. Recorded here precisely because the
/// Transition log's fold has already pruned it. **With the tile it last stood
/// on** since #376: the episode is the colony's and names no room (ADR 0028),
/// so a loss with no room of its own could not say which of the colony's
/// rooms the body died in — live, 33 bodies killed in W15S27 read as an
/// errand raid at the Reactor ring three rooms away, and an ADR that had fired
/// correctly was read as broken. None when the projection never placed the
/// body (ADR 0004), and for a row written before the tile was kept.
type Loss =
    {
        Creep: string
        Tick: int
        Where: RoomPos option
    }

/// One raid: opened on the first tick any room the colony works and can see
/// holds a hostile, kept open while hostiles keep appearing, closed by a quiet
/// gap. Colony-level as ADR 0028 made it, and so it names no room: a raid that
/// crosses a border is one episode, its coordinates carrying their own rooms
/// (ADR 0052 decision 2).
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
        /// Hits lost across the Keep and the ramparts inside the window (ADR
        /// 0034): decreases summed tick over tick, repairs ignored. Read on the
        /// ticks the window covers and no others — a hostile standing there, or
        /// the tick straight after a sighting — so the decay of a long quiet
        /// gap is charged to nobody; decay inside the raid's own ticks rides
        /// along at 3 hits a tick per rampart against the hundreds a raid
        /// takes.
        Damage: int
    }

/// One [[outpost]]'s stand-down episode: the Raid log's second family (ADR
/// 0043), opened by an invader core standing in a room the colony works, or
/// since #165 by another player's [[reservation]] read off one of these
/// controllers, rather than by a hostile creep — and carrying the one field a
/// raid never needed, the tick the [[stand-down]] it drives expires. Not every
/// row is a threat, then, and the `Basis` is what says which: what this family
/// has in common is a clock, not a danger. Since #201 the two families cover the
/// same rooms and the difference is only what opens them: a creep the engine's
/// creep sweep answers with, or a structure it never can. A record of its own
/// rather than a widening of `RaidEpisode`, for two reasons pointing the same
/// way. The four things a raid episode records cannot be read off a core at all
/// — `InvaderCoreInfo` carries a room and a deadline and nothing else (#133),
/// the projection growing a field the tick a reader exists (ADR 0007) — so a
/// shared shape would be four structurally empty fields per row.
type OutpostEpisode =
    {
        /// The room this stand-down shuts, which is the whole of where: the
        /// gate admits or withholds a room, so the tile a core stands on is a
        /// fact nothing asks for, and one room standing down says nothing about
        /// another (ADR 0043's independent gates).
        RoomName: string
        /// The tick this stand-down opened.
        Opened: int
        /// The last tick the thing that opened this row was actually read in
        /// the room — a core seen standing there, or the rival's hold seen on
        /// the controller (#165). A record for the reader and never the
        /// openness test — see `standingDown`: the colony stops looking the
        /// moment it withdraws, so silence here says nobody is there to look
        /// and never that the room is clear.
        LastSeen: int
        /// The **absolute** tick the stand-down runs to, read off the threat
        /// and not chosen (ADR 0043).
        Expiry: int
        /// Which deadline `Expiry` came off — ADR 0043's three for an invader
        /// core, and #165's fourth for another player's reservation. Carried
        /// because it cannot be recovered from the tick afterwards, and
        /// because "shut until 2,600" and "shut until 2,600 because nothing
        /// could be read" are different answers to an operator (#117).
        Basis: StandDownBasis
    }

/// Whether an outpost episode still holds its room shut at this tick: ADR
/// 0043's "re-entry is a clock running out, not a look", written as the one
/// place the family's openness is decided. The expiry tick is the first tick
/// the room may be re-entered. This family is exempt from the quiet gap that
/// closes a raid, and the exemption is load-bearing. The gap answers "has the
/// squad left?" — a question about creeps that move and are watched by a colony
/// sitting in the room. Neither half holds here: a core has 100,000 hits,
/// spawns nothing at level 0 and never leaves, so its absence from a view is no
/// evidence; and the stand-down's whole effect is to withdraw the creeps whose
/// vision would see it.
let standingDown (tick: int) (episode: OutpostEpisode) = tick < episode.Expiry

/// One room latched on another player's **ownership**: ADR 0043's clockless
/// withdrawal, and the two ticks it takes to hold one honestly (#275). Two and
/// not one, because the record answers two questions that used to be asked of
/// the same number and are not the same question: *when did this room stop
/// paying* — the date an operator lines an income drop up against (#117's
/// US-20) — and *when is the colony next willing to question the conclusion*.
/// A record and no clock in either field: neither tick is compared against an
/// expiry, because ownership is the one trigger the engine gives no end for.
type RivalLatch =
    {
        /// The tick the gate shut on: the first look that found the room in
        /// another player's hands, and never restamped by a later look that
        /// agrees with it. The *current* withdrawal's date — a room that came
        /// back and was taken again dates from the taking that holds it now.
        Since: int
        /// The tick the last look **fell due** on, and the tick the stride to
        /// the next one is measured from (`lookDue`, #275). It starts equal to
        /// `Since` — the look that shut the gate is a look — and moves on every
        /// tick the gate hands the room out, whether or not vision answered on
        /// it: being blind in the room is the common case rather than a look
        /// that did not happen, the withdrawal itself having taken the vision
        /// away, and a stride stamped only when something was seen would leave a
        /// blind room re-admitted to the scan on every tick from its first due
        /// one onwards.
        ///
        /// The gate's tick and not the shell's, which is a real gap in one
        /// corner: `ColonyView.ofWorld` reads a controller only for rooms this
        /// colony **declares**, deliberately, so a room latched here and since
        /// undeclared is stamped by a look nothing ever took. The record over-
        /// claims there and the gate does not: re-declaring the room costs at
        /// most one stride before the look that can free it, and until then
        /// nothing is spent on it.
        LastLooked: int
    }

/// One [[outpost]] whose controller **somebody else's CLAIM parts hold**, as
/// the colony last read it (#333): whose, and the tick the hold runs out on.
/// Neither a stand-down nor a latch, and deliberately neither.
///
/// Not a **stand-down**: a row of the ring above withdraws its room from the
/// scan set, and what this one costs is the *reservation* and nothing else —
/// `reserveController` is refused on the controller and `createConstructionSite`
/// in the room, so the reserver row hires nobody for it
/// (`Planner.reservableControllers`) and the room raises no [[container]].
/// Whether a room in that state should stay declared at all is a question for a
/// human, and a row in the ring would have answered it by accident.
///
/// **Of the Invader's hold that is the whole story; of a rival's it is half.**
/// The two holders are one refusal to the engine and this record folds them
/// alike, but a *rival*'s reservation is also #165's clocked stand-down, opened
/// off this very control entry, so that room is withheld as well and is not one
/// the colony goes on mining. The Invader's is the case with no other record in
/// it: ADR 0043's ring is clocked off cores, the core is gone, and the hold it
/// left behind belongs to nobody else's clause. So "the room is still mined, its
/// rock pooled and its bodies standing in it" is said of the Invader's hold and
/// must not be generalised to "somebody else's" — it is the sentence #333 was
/// filed on and it is false one holder over.
///
/// Not a **latch**: the engine is counting this hold down at one a tick, so
/// `Until` is an absolute tick and the record ends itself — ADR 0043's "re-entry
/// is a clock running out, never a look", which is what lets the entry outlive
/// the vision that read it. `RivalHeld` beside it is the one withdrawal with no
/// such tick.
///
/// A record to be read and never a signal sent — with ADR 0028's one standing
/// exception widened by one rule: `Observe.standDown` already *acts* on this
/// leaf, and since #333 the set it derives from here rides the view beside
/// `RoomControl` and narrows the Reserve pool on the ticks vision answers for
/// nothing (`ColonyView.HeldOutposts`). It has to: `RoomControl` is this tick's
/// vision, the reserver is the only body most of these rooms ever hold, and a
/// rule read off vision alone would hire one more reserver every time the last
/// one died. This is also what `observe.mjs outposts` prints, so that a room the
/// colony is not reserving stops being announced as `open`.
type OutpostHold =
    {
        /// Whose CLAIM parts hold it — the Invader's, or another player's.
        /// `Ours` never reaches this map: a hold of our own is the steady
        /// state every outpost is supposed to be in.
        Holder: ReservationHolder
        /// The **absolute** tick the hold runs out on: this colony's clock
        /// plus the engine's own countdown on the tick the look was taken.
        /// Absolute like an episode's `Expiry` and for its reason — stored
        /// as read it would date the hold to a tick long past.
        Until: int
    }

/// One declared [[outpost]] an armed [[threat]] was seen standing in, against
/// the tick that memory runs out on (#366). The guard row's half of #333's
/// shape: a conclusion held across the blind ticks, because the bodies whose
/// vision hired the guard are what the raid kills.
type ThreatLatch =
    {
        /// The **absolute** tick the memory runs out on: the tick the threat
        /// was last seen plus `Tuning.ThreatMemory`. Absolute like an
        /// `OutpostHold.Until` beside it and for its reason — stored as a
        /// duration it would date the memory to whenever it was last read.
        Until: int
    }

/// The whole persisted Raid log.
type RaidState =
    {
        /// The episode ring: oldest first, trimmed from the front — the
        /// Transition log's own convention.
        Episodes: RaidEpisode list
        /// The outpost family's own ring beside it, oldest first as that one
        /// is (ADR 0043). Its own ring and not rows mixed into it, because a
        /// shared list is a shared depth: twenty spawn-room raids would evict
        /// the very episode holding an outpost shut and the gate would reopen
        /// in the middle of a stand-down (#117). So the `cap` the fold takes
        /// is a depth per family and never a total, and this ring trims by one
        /// further rule of its own (`trimOutposts`).
        Outposts: OutpostEpisode list
        /// The rooms whose controller, on the last tick the colony could see
        /// it, **belonged to** another player, each against the two ticks a
        /// `RivalLatch` keeps: ADR 0043's *other* withdrawal, the one that needs
        /// no clock, because a room somebody else holds has not been made
        /// dangerous — it has stopped being ours to work. Ownership alone since
        /// #165: a rival's *reservation* stood here too until the cost of
        /// latching a room for a hold that decays in at most 5,000 ticks was
        /// priced, and it is now a clocked episode in the ring above, where the
        /// engine's own countdown says when the room comes back. Neither tick is
        /// a clock and nothing compares against `Since` at all: it is the trace
        /// the gate's closing leaves in the observe channel (#117's US-20),
        /// which is how that channel answers which tick a room's income stopped
        /// arriving on. What is compared against is `LastLooked`, one stride at
        /// a time (`lookDue`, #165 as #275 measures it): the room is re-admitted
        /// to the scan once a whole `Tuning.RivalRecheck` has passed since the
        /// last look, so a latch the rival has walked away from can be cleared
        /// by the only thing that ever could. A remembered conclusion and not a per-tick
        /// reading, which is the whole reason it is persisted: the judgement needs vision (ADR
        /// 0004), and the gate's own effect is to withdraw the creeps that pay
        /// for it, so a gate that re-read this off the view would reopen the
        /// room on the tick after it shut it, for ever. That is
        /// `standingDown`'s oscillation reached through the other trigger, and
        /// the answer is the same shape: hold the last conclusion until a tick
        /// with vision replaces it. This leaf is the only per-room state the
        /// colony keeps **in Memory** (#117); the [[sighting]] the vision grace
        /// reads is per-room and carried across ticks too, and is heap-only
        /// (#151).
        RivalHeld: Map<string, RivalLatch>
        /// The rooms whose controller, on the last tick the colony could see
        /// it, was **reserved by somebody else** — the Invader's leftover
        /// hold or another player's — each against the tick that hold runs
        /// out on (#333). The third shape in this leaf and the third thing
        /// it means: a row of `Outposts` withdraws its room, an entry of
        /// `RivalHeld` withdraws it for good, and an entry here withdraws
        /// **no room of its own**: what it takes away is the *reservation*.
        /// For the Invader's leftover hold that is the whole effect, the room
        /// going on being mined; for a rival's the same control entry has
        /// already opened a #165 stand-down, so that room is withheld by the
        /// ring and this entry only says why the hold on it is not ours.
        ///
        /// It records the fact the reserver row and the Reserve pool read
        /// before they hire on a blind tick (`ColonyView.HeldOutposts` through
        /// `standDown`) — the engine refuses `reserveController` on a
        /// controller anybody else holds — and it is what lets the channel say
        /// why a declared outpost is being mined and not reserved, instead of
        /// printing it `open`.
        ///
        /// Carried between ticks for `RivalHeld`'s reason and ended for the
        /// ring's: a tick with vision decides the room either way, a tick
        /// without leaves the last conclusion standing, and the conclusion
        /// expires by itself when `Until` passes, no look being needed to
        /// know a countdown has run out (ADR 0043). No ring and no cap — the
        /// map is bounded by the rooms the colony scans, as the latch map
        /// beside it is.
        Holds: Map<string, OutpostHold>
        /// The declared [[outpost]]s an **armed** [[threat]] was standing in on
        /// the last tick the colony could see them, each against the tick that
        /// memory runs out on (#366). The fourth shape in this leaf, and
        /// `Holds`' shape applied to the guard row rather than the reserver
        /// one: written on the ticks with vision, **removed** on a tick with
        /// vision that shows the room clear, and expiring by its own clock in
        /// between.
        ///
        /// It withdraws nothing. What it buys is that the body the guard row
        /// already paid for is still hired, still pooled a Guard and still
        /// walked at the room on the ticks the raid has killed everything of
        /// ours that could see it (`ColonyView.ThreatenedOutposts` through
        /// `standDown`, read by `Planner.guardedOutposts`). ADR 0056 made
        /// vision in a guarded outpost the guard's own, which is circular the
        /// moment the guard has not arrived yet: W11S28 lost its anchor and its
        /// reserver to a 2-ATTACK-part invader, the room went dark, and the
        /// 15-ATTACK-part guard bought for that raid stood idle at home.
        ///
        /// Ends by a clock of its own and not by a look, for `Holds`' reason
        /// turned around: nothing in the engine counts a raid down, so the
        /// number is ours (`Tuning.ThreatMemory`) and is sized at the cast plus
        /// the walk. No ring and no cap — bounded by the declared outposts, as
        /// the two maps beside it are.
        Threatened: Map<string, ThreatLatch>
        /// The owned creep names the previous tick projected, less the ones
        /// whose life ran out on it: the baseline this tick's losses are read
        /// against. Carried only while an episode is open, so a creep that
        /// dies in peacetime is read against an empty baseline and recorded
        /// nowhere. This colony's names and not the world's (ADR 0047), which is why the difference `foldRaids` takes is
        /// against the world's living names: a creep another colony adopted
        /// leaves the baseline without dying.
        Living: Set<string>
        /// The tile each of those bodies last stood on, kept on the same
        /// condition — an open episode, and empty otherwise (#376): what a
        /// loss is stamped with, read the tick after the body is gone, when
        /// the projection no longer places it. Some forty bytes a body against
        /// `Living`'s ten, so a sixty-body colony writes ~2.5KB a tick while
        /// a raid is open and nothing while none is. Worked rooms only, as the
        /// projection is (ADR 0041): a body that dies on a walk through a
        /// transit room is stamped with no tile.
        Placed: Map<string, RoomPos>
        /// The previous tick's hits per structure id across the Keep and the
        /// ramparts: the baseline this tick's damage is read against, carried
        /// as `Living` is — only while an episode is open — and only on a tick a
        /// hostile stood in the [[home room]]. The Keep and
        /// its ramparts stand in one room (ADR 0034) and the episode above
        /// them spans every room the colony works, so the baseline is what
        /// keeps the two the same room's.
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
            Living = Set.empty
            Placed = Map.empty
            Hits = Map.empty
        }

/// The episode ring cap: one ring for the whole colony rather than one per
/// creep, so twenty episodes the size of #66's raid — a few hundred bytes each
/// — sit around 10KB against the 2MB Memory. Twenty raids is more history than
/// stays actionable, and it is the number the sibling channel already keeps.
let capEpisodes = 20

/// The tiles of everything of ours a hostile can close on, as far as a view can
/// approximate the owned set: our creeps, and the owned structures it already
/// places — the Refillables and the controller. Roads and containers cannot be
/// owned in the engine; the Storage can, and is left out because ADR 0022 puts
/// it a tile or two from the spawn, inside the ring the Refillables already
/// cover. The ramparts are deliberately out too: they cover the Keep and the
/// Posts, and a rampart over a Post would pull the number out to the sources
/// and answer a different question — what the Keep is losing is watched through
/// its hits instead. An id the projection cannot place contributes nothing (ADR
/// 0004).
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
let private approachAt (view: ColonyView) : Approach option =
    // **Armed hostiles alone** since #376: the approach is the number that
    // separates a probe from a loss, and a `1 MOVE` scout standing on the
    // Reactor's ring beside our re-claimer is neither. Live it was that scout,
    // at range 1 in W15S25, that an episode named as its closest approach
    // while an invader three rooms away killed 33 bodies, and the reading
    // "an armed raid at the ring" followed. The scouts stay on the roster.
    let armed = view.Hostiles |> List.filter Decide.Facts.isArmed

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

/// Union one sighting into the roster: the first sighting of an id wins, so a
/// row records the body that entered the room rather than what the tower left
/// of it, and a creep that steps back in is one row still.
let private enrol roster (hostile: HostileInfo) =
    if Map.containsKey hostile.Id roster then
        roster
    else
        Map.add
            hostile.Id
            {
                Owner = hostile.Owner
                Body = partsOf hostile.Body
            }
            roster

/// The tick one core's stand-down runs to, and which of ADR 0043's three
/// deadlines it was read off, in the ADR's own order of availability. The
/// reservation branch takes the core's own hold and nobody else's: a rival's is
/// a withdrawal of its own with its own clock (`rivalDeadlines`, #165) and the
/// colony's own says nothing about the core, which is why the holder arrived as
/// three answers rather than one "not ours" flag (#133). `TicksToEnd` is the
/// engine's relative count, so the tick it names is this one plus it — stored as
/// read it would shut an outpost until a tick a hundred thousand in the past.
/// And that branch may never answer *earlier* than the fallback, the amendment
/// ADR 0043 took in #136. A core outlives the hold it takes — it re-reserves the
/// controller the tick the hold lapses — where a collapse timer's end is the
/// tick the engine takes the stronghold away, which is why the branch above
/// needs no such floor. A core that has just `attackController`'d an unreserved
/// controller holds it for three ticks (`invader-core/reserveController.js`,
/// #117), so read literally this branch would answer a three-tick stand-down:
/// the "immediately" ADR 0043's own user story says no path may reach.
let private deadlineOf (view: ColonyView) (core: InvaderCoreInfo) =
    match core.CollapseTick with
    | Some tick -> tick, StandDownBasis.CollapseTimer
    | None ->
        view.RoomControl
        |> Map.tryFind core.RoomName
        |> Option.bind (RoomControlInfo.heldBy ReservationHolder.Invader)
        |> Option.filter (fun held -> held.TicksToEnd >= view.Tuning.StandDownFallback)
        |> Option.map (fun held -> view.Time + held.TicksToEnd, StandDownBasis.Reservation)
        |> Option.defaultValue (view.Time + view.Tuning.StandDownFallback, StandDownBasis.Fallback)

/// The tick a room another player has **reserved** stands down to (#165): the
/// end of that reservation, read off the engine's own countdown on the tick the
/// look was taken. ADR 0043 wrote this trigger as the clockless one, together
/// with a rival's ownership, on the grounds that it is not a threat but a room
/// that stopped being ours to work. The half of that which is wrong is the
/// *duration*: a reservation decays at one a tick and caps at 5,000, so a
/// passing claimer takes a room for a bounded window and a permanent latch pays
/// an outpost's whole income for a hold that was over hours ago (#165). The
/// other half stands, and is why this is a stand-down rather than a contest: the
/// colony withdraws for exactly as long as somebody else is working the room and
/// re-enters when the engine says nobody is (ADR 0043's re-entry is a clock
/// running out, never a look, and no vision is needed to know a countdown has
/// run out). No floor under it, unlike `deadlineOf`'s reservation branch: that
/// floor is there because an Invader core re-takes the hold the tick it lapses,
/// and a player's claimer that stops coming re-takes nothing — read literally is
/// exactly right here. Ownership is **not** here: it carries no countdown at
/// all, and it is the one trigger the engine gives no end for, which is why it
/// stays the latch `RivalHeld` remembers.
let private rivalDeadlines (view: ColonyView) =
    view.RoomControl
    |> Map.toList
    |> List.choose (fun (room, control) ->
        control
        |> RoomControlInfo.heldBy ReservationHolder.Rival
        |> Option.map (fun held ->
            room, (view.Time + held.TicksToEnd, StandDownBasis.RivalReservation)))

/// This tick's deadline for each room a stand-down can be read off — the
/// sightings the fold below folds, and the only tick on which a stand-down's
/// clock moves at all. Two threats answer here and they differ in kind: an
/// invader core standing in a room the colony works, and another player's
/// reservation on one of these controllers (#165). A room with no entry here is
/// a room the colony cannot see or one nothing stands in, and those two read
/// alike on purpose: neither is evidence, so neither touches an episode (ADR
/// 0004, `standingDown`). A room both answer for keeps the **later** of the two,
/// which is `sight`'s rule applied inside one tick and for its reason: a
/// stand-down may be wrong only in the direction that costs an outpost's income.
/// The tick a room stands down to when the raid in it is one the colony has
/// already decided not to fight (#257, ADR 0056 decision 6). ADR 0043 clocks a
/// [[stand-down]] off an invader **core**, and a raid of plain creeps offered
/// it no deadline at all — so W13S29 stayed open through a two-creep raid, the
/// reserver row went on hiring one body per declared [[outpost]] and sending it
/// into the fight, and in three hundred ticks the room took two reservers and a
/// guard while the invaders stayed at full health.
///
/// **The clock is the raid's own life.** An Invader in a room nobody owns never
/// suicides — the engine's suicide branch wants a controller owner — so what it
/// has left is exactly what it will spend, and the longest of them is when the
/// room is ours again. No floor and no fallback: this deadline is read, not
/// chosen.
///
/// **And it is read only for a raid the guard row's cap cannot beat**, where
/// the room is one the guard row actually serves. That last clause matters for
/// an [[errand]] (#348): its target room can hold a raid two guards would beat,
/// but no guard is ever hired there because the row is per declared outpost.
/// Such a room is a withdrawal regardless of the hypothetical exchange. The
/// expected Source Keepers are not that raid.
///
/// For an outpost, the old answer stands and keeps this from cancelling ADR
/// 0056 before it fights. Standing a room down withdraws it from the scan set,
/// so a raid that shut the room the tick it appeared would hide its own
/// hostiles and no guard would ever be hired: a withdrawal and a garrison are
/// the same room's two answers, and this is where they are told apart. The
/// arithmetic is `guardsWanted`'s own, at the cap: two blocks' damage against
/// the raid's armed hits, and two blocks' hits against the raid's full damage:
/// melee attacks exclude self-healing. A raid two guards beat is a fight; a
/// raid two guards lose is a room to leave, and it is left for exactly as long
/// as the raid has to live.
///
/// A [[transit room]] is neither answer (#324, ADR 0065). Its hostiles remain
/// on the view so a walker can Flee, but the colony pools no work and hires no
/// guard there, so the stand-down has nothing to withhold. This boundary reads
/// the same declared-outpost derivation as the guard row rather than matching
/// the hostile's owner: an Invader or player in a transit-only room is no more
/// actionable by this gate than a Source Keeper is.
let private raidDeadlines (view: ColonyView) =
    let errandRooms =
        view.Errands |> List.map (fun errand -> errand.RoomName) |> Set.ofList

    let outpostRooms = Fabot.Core.Decide.Planner.declaredOutposts view |> Set.ofList

    view.Hostiles
    |> List.filter (fun h -> h.Pos.Room <> SpatialInfo.homeName view.Spatial)
    |> List.groupBy (fun h -> h.Pos.Room)
    |> List.filter (fun (room, hostiles) ->
        let armed = hostiles |> List.filter Decide.Facts.isArmed

        if Set.contains room errandRooms then
            armed |> List.exists (fun hostile -> hostile.Owner <> "Source Keeper")
        else if Set.contains room outpostRooms then
            not (List.isEmpty armed)
            && not (Decide.Quota.guardBlocksBeat view room Engine.guardCap)
        else
            false)
    |> List.map (fun (room, hostiles) ->
        let raid =
            if Set.contains room errandRooms then
                hostiles |> List.filter (fun hostile -> hostile.Owner <> "Source Keeper")
            else
                hostiles

        let life = raid |> List.map (fun h -> h.TicksToLive) |> List.max

        room, (view.Time + life, StandDownBasis.InvaderRaid))

let private deadlines (view: ColonyView) =
    (view.InvaderCores |> List.map (fun core -> core.RoomName, deadlineOf view core))
    @ rivalDeadlines view
    @ raidDeadlines view
    |> List.groupBy fst
    |> List.map (fun (room, seen) -> room, seen |> List.map snd |> List.maxBy fst)

/// Fold one room's sighting into the outpost ring: the room's standing episode
/// takes it — its window extends and its clock is re-read, this being a tick
/// with vision — and where the room has none standing, the sighting opens one.
/// At most one episode per room can be standing, so the re-read lands on one
/// row. The re-read only ever moves a running clock outward: the later of the
/// recorded deadline and this tick's is kept, and the basis with it, so the
/// record still names the read that is holding the room. That is `deadlines`'
/// rule applied across ticks, and for the same reason — a sighting that lands
/// on a worse deadline is real, and reading it in would cut a stand-down short,
/// the direction ADR 0043's Consequences forbid.
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

/// Trim the outpost ring to the cap, oldest first — and never over an episode
/// that is still standing down. The ring is there to bound Memory, and the row
/// it would drop first is the one holding creeps out of a room: evicting that
/// reopens the gate mid-stand-down, the failure ADR 0043 is written against,
/// reached through the ring rather than the clock.
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

/// Whether the room this control entry answers for is another player's **for
/// good**: the community's one unanimous abandonment rule, and after #165 the
/// only half of ADR 0043's clockless withdrawal that is still clockless. Owned
/// and no longer *or reserved*, because the two are one fact only to the
/// economics (ADR 0042) and two facts to a gate that has to say when the room
/// comes back: ownership is irreversible as far as anything this colony does is
/// concerned and the engine gives no end for it, while a reservation decays at
/// one a tick and ends on a tick the engine is already counting down
/// (`rivalDeadlines`). A room read as latched here is withdrawn from until a
/// look with vision says otherwise, and the looks are at least
/// `Tuning.RivalRecheck` apart (`lookDue`).
let private rivalOwned (control: RoomControlInfo) = control.Owner = Ownership.Rival

/// Whether a latched room's next look falls due on this tick: a whole
/// `Tuning.RivalRecheck` has passed since the last look was taken (#275). The
/// one place the stride is spelled, read by the gate that hands the look out
/// and by the fold that records it having been taken, over the same log and the
/// same tick, so neither can be looking at a stride the other is not. They read
/// the knob off different places — the gate takes its `Tuning` as an argument
/// and the fold reads the view's — so the agreement is the caller's to keep,
/// and every caller hands the gate the tuning it built the view with.
///
/// An **elapsed** test and not the exact multiple #165 shipped. A multiple is a
/// gate that has to be asked on precisely one tick in five thousand or not at
/// all, and a tick's evaluation is not guaranteed: the loop can throw before
/// the fold writes the log, the engine cuts a tick short when the bot is out of
/// CPU and the bucket is empty, and a deploy lands in the middle of one. Every
/// tick lost that way silently cost the outpost a whole further stride of
/// income. Owed is owed: the first tick this is asked on from the stride
/// onwards pays it, and taking the look is what starts the next stride
/// (`foldRaids`).
///
/// A stamp **ahead of** the clock is a look owed now. No look can be taken on a
/// tick that has not happened, so such a stamp is not a record but a wrong
/// number — a mistyped hand edit of the leaf, which is the documented way out
/// of a stuck latch, or a private server rolled back behind the tick the log
/// was written on. Without this clause the elapsed test absorbs it silently and
/// the latch is unfalsifiable again for as long as the stamp leads the clock:
/// the very thing #165 bought and #275 must not sell back. The exact multiple
/// it replaces self-corrected within one tick of the stamp; this corrects
/// within one, too, and `foldRaids` stamps the real tick over it, so the leaf
/// heals as well as the gate.
///
/// A `RivalRecheck` of zero or less is "never look again", which is this rule
/// switched off rather than a stride no tick can reach.
let private lookDue (tuning: Tuning) (tick: int) (latch: RivalLatch) =
    tuning.RivalRecheck > 0
    && (latch.LastLooked > tick || tick - latch.LastLooked >= tuning.RivalRecheck)

/// The [[stand-down]] gate's answer for this colony this tick (ADR 0043 as #165
/// narrows it), read off the previous tick's log. This is the one reader that
/// *acts* on the Raid log, and ADR 0028's "a record to be read, never a signal
/// sent" is narrowed here and exactly once. Two sets and not a decision: what
/// the shell does with `Shut` is drop those rooms from the declarations it works
/// (`Outpost.worked`), so they never enter the [[spatial projection]] at all —
/// the whole of "withdraw" in an architecture that recomputes every tick (ADR
/// 0004) — and what it does with `Rechecked` is read those rooms' controllers
/// and nothing else of them (`ColonyView.ofWorld`).
///
/// Since #366 a fourth rides beside that third and is not a withdrawal either:
/// `ThreatenedOutposts`, the declared outposts an armed [[threat]] was standing
/// in at the last look and whose memory this tick is still short of. What the
/// shell does with it is the same thing: hand it to the view, where the guard
/// row and the Guard Task read it on the ticks the raid has blinded the colony
/// in the room (`Planner.guardedOutposts`) — the room goes on being worked
/// either way.
///
/// Since #333 a third set rides beside them and it is **not** a withdrawal:
/// `HeldOutposts`, the rooms whose controller somebody else's CLAIM parts were
/// standing on at the last look and whose hold this tick is still short of.
/// What the shell does with it is hand it to the same view, where the Reserve
/// pool and the reserver row read it on the ticks vision does not answer
/// (`Planner.reservableControllers`) — the room goes on being mined either way.
/// Read off the same log and the same tick as the two above for their reason: a
/// second derivation is a second answer free to disagree.
///
/// `Shut` is every room a stand-down's clock is still running in, and every room
/// the colony last saw in another player's ownership. `Rechecked` is the second of
/// those families alone, and only once a whole `Tuning.RivalRecheck` has passed
/// since the last look into that room (`lookDue`): the latch's own effect is to
/// stop scanning the room, so left alone it can never meet the tick with vision
/// that would clear it, and the room stays abandoned long after the rival has
/// gone (#165). The look stays owed until it is taken, and `foldRaids` records
/// the taking, so a look that finds the rival still there leaves the next one a
/// full `RivalRecheck` away rather than one that never falls due at all.
let standDown (tuning: Tuning) (tick: int) (state: RaidState) : StandDown =
    {
        Shut =
            state.Outposts
            |> List.filter (standingDown tick)
            |> List.map (fun episode -> episode.RoomName)
            |> Set.ofList
            |> Set.union (state.RivalHeld |> Map.toList |> List.map fst |> Set.ofList)
        Rechecked =
            state.RivalHeld
            |> Map.toList
            |> List.filter (snd >> lookDue tuning tick)
            |> List.map fst
            |> Set.ofList
        // The standing holds, on the same test `observe.mjs` prints them under
        // and `foldRaids` keeps them by: `tick < Until` and nothing else. An
        // entry whose tick has passed is a hold the engine has already counted
        // out — the room is ours to reserve again with nobody having gone to
        // look, which is ADR 0043's re-entry rule reached through a record —
        // and the fold drops it on the next tick with vision. Filtered here as
        // well so the gate cannot withhold a reservation on a leaf the fold has
        // not caught up with.
        HeldOutposts =
            state.Holds
            |> Map.toList
            |> List.filter (fun (_, hold) -> tick < hold.Until)
            |> List.map fst
            |> Set.ofList
        // The standing threat memories, on the same `tick < Until` test the
        // holds above are filtered by and the fold retires an entry on (#366).
        // Filtered here as well for that clause's reason: a leaf the fold has
        // not caught up with — no tick with vision since the memory ran out —
        // cannot go on hiring a guard for a room nothing has seen in longer
        // than `Tuning.ThreatMemory`.
        ThreatenedOutposts =
            state.Threatened
            |> Map.toList
            |> List.filter (fun (_, latch) -> tick < latch.Until)
            |> List.map fst
            |> Set.ofList
    }

/// The Raid-log fold (ADR 0028): this tick's view plus the previous Raid log
/// produce the new one. An episode opens on the first tick a room the colony
/// works and can see holds a hostile, stays open while hostiles keep
/// appearing, and closes after `Tuning.QuietGap` quiet
/// ticks; the ring keeps the newest `cap` episodes. Two baselines are carried
/// across ticks while an episode is open and dropped with it: the names this
/// tick's losses are read against, and the hits this tick's damage is (ADR
/// 0034) — the second only on the ticks a hostile stands in the [[home room]],
/// because the Keep it measures stands there alone. Beside it, sharing nothing
/// but the leaf they are written to, the outpost family (ADR 0043): an invader
/// core seen in a room the colony works opens or extends that room's
/// stand-down, in its own ring, on its own clock, with no quiet gap. The two
/// halves are deliberately disjoint — one reads `Hostiles` and the other
/// `InvaderCores` and the controllers beside them, and the engine's own sweeps
/// guarantee no object is in both — so `cap` is a depth per family and never a
/// total. Another player's **reservation** joins that second family (#165): it
/// is no threat, but it is a withdrawal the engine hands an end for, and an
/// episode whose basis names it is what tells an operator which of the two shut
/// the room. And beside both, one remembered conclusion rather than an episode:
/// which rooms the colony last saw in another player's **ownership**
/// (`RaidState.RivalHeld`), the withdrawal that carries no clock because nothing
/// in the engine ends it, which still dates itself because the tick a gate shut
/// on is the trace #117's US-20 asks for.
let foldRaids (cap: int) (alive: Set<string>) (view: ColonyView) (prior: RaidState) : RaidState =
    // The silence that closes an episode is the colony's own tunable and
    // arrives on its view (ADR 0052 decision 5), where the ring's depth is
    // still the caller's: one is a judgement about how long an absence has to
    // run before it is a departure, the other how much history a leaf may hold.
    let gap = view.Tuning.QuietGap

    // The baseline the next tick reads its losses against: this tick's names,
    // less the creeps whose clock runs out on it. A name gone tomorrow because
    // CREEP_LIFE_TIME ran down is old age, and TicksToLive says which is which
    // before the fact, so the difference never has to guess after it.
    let surviving =
        view.Creeps
        |> List.filter (fun creep -> creep.TicksToLive > 1)
        |> List.map (fun creep -> creep.Name)
        |> Set.ofList

    // Where each of ours stands this tick, over every room the projection
    // places a body in (#376): the tile a loss is stamped with next tick.
    let placedNow =
        view.Spatial.Rooms
        |> Map.toList
        |> List.collect (fun (room, layer) ->
            layer.CreepPositions
            |> Map.toList
            |> List.map (fun (name, pos) -> name, RoomPos.at room pos))
        |> Map.ofList

    // The hostiles standing in the room the defences are in. Since #201 the
    // sweep behind `ColonyView.Hostiles` covers every room the colony works, so
    // "a hostile" and "a hostile where the Keep is" are two questions, and the
    // damage below asks the second — through the baseline it differences
    // against. A rampart cannot stand in an outpost and the Keep is home's by
    // definition (ADR 0034), so a window opened by a raider a border away would
    // charge this room's ordinary decay to a raid that never touched it.
    let atHome =
        let home = SpatialInfo.homeName view.Spatial
        view.Hostiles |> List.filter (fun hostile -> hostile.Pos.Room = home)

    // This tick's hits across the Keep and the ramparts, the next tick's
    // baseline. The kinds are the rule's, never a list of ids: a rampart
    // raised mid-episode joins it the tick it stands.
    let defended =
        SpatialInfo.structureHits view.Spatial
        |> List.choose (fun (id, kind, hits) ->
            if isDefence kind then Some(id, hits.Hits) else None)
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
    // longer holds. A name is missing the tick *after* its creep died, so the
    // loss is stamped at the tick it was last seen alive — this episode's last
    // sighting, and only when that sighting was the previous tick. Differenced
    // against `alive` and never this colony's own names: a body another colony
    // adopted is still standing (ADR 0047 decision 2).
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
                    Where = Map.tryFind name prior.Placed
                })

    let episode =
        match current, view.Hostiles with
        | None, [] -> None
        | current, hostiles ->
            let lost = current |> Option.map lostSince |> Option.defaultValue []

            // Damage is charged against the previous tick's baseline, and only
            // an episode that was already open has one, so nothing crosses the
            // seam between two episodes. Which ticks are inside the window is
            // the *baseline's* question and is answered where it is carried
            // (#201): a tick that carries none leaves this difference nothing
            // to subtract, so the window is exactly the ticks a hostile stood
            // in the room the Keep is in, plus the one after — which keeps the
            // decay of a quiet gap, and of a raid a border away, charged to no
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
        // The outpost family's whole tick: the rooms a core was seen in and
        // the rooms another player's reservation was read on (#165), each
        // folded into the ring. A tick that sees none — no vision, or vision
        // and a room that is clear — leaves every stand-down exactly as it
        // found it, clock included.
        Outposts =
            (prior.Outposts, deadlines view)
            ||> List.fold (fun episodes seen -> sight view.Time seen episodes)
            |> trimOutposts cap view.Time
        // The clockless withdrawal's memory, moved by the ticks with vision
        // alone: a room the colony can see answers for itself either way, and a
        // room with no `RoomControl` entry keeps whatever the last look
        // concluded. Absence is not evidence in either direction (ADR 0004),
        // and here it is the load-bearing half — the room this holds shut is
        // one nothing is looking into, and re-reading it as "free" would walk
        // the colony straight back into somebody else's room. Since #165 that
        // blindness is broken on purpose once every `Tuning.RivalRecheck`
        // ticks: the gate re-admits a latched room to the scan alone, so a
        // `RoomControl` entry can arrive for a room the colony is still
        // withdrawn from, and this fold clears the latch off it like any other
        // look. No ring and no cap: the map is bounded by the rooms the colony
        // scans (ADR 0041), only a room with a `RoomControl` entry ever
        // entering it.
        //
        // The stride between those looks is stamped here and nowhere else
        // (#275): a look the gate handed out this tick is a look taken, so its
        // room's `LastLooked` moves to this tick whether or not vision answered
        // — being blind in the room is the common case, the withdrawal having
        // taken the vision away, and a stride only stamped when something was
        // seen would leave a blind room re-admitted to the scan on every tick
        // from its first due one onwards. Which ticks those are is `lookDue`'s
        // to say, the same function the gate asks, over the same log and the
        // same tick, so the record can never disagree with the look it records.
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
        // The rooms somebody else's reservation stands on (#333), read off the
        // same control entries the latch above reads and kept on the same two
        // rules — a tick with vision decides a room either way, and a tick
        // without leaves the last conclusion standing, because the colony is
        // blind in most of these rooms most of the time.
        //
        // Where this parts from the latch is how it ends: the engine is
        // counting the hold down at one a tick, so the entry ends itself on
        // the tick it named and no look is needed to retire it (ADR 0043's
        // re-entry rule, reached through a record rather than a gate). An
        // expired entry is dropped here rather than filtered by every reader,
        // so the leaf cannot accumulate holds that ended hours ago and the
        // channel cannot print one as if it were still standing.
        Holds =
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
        // The guard row's memory of a raid (#366), on the two rules `Holds`
        // above is kept by: a tick with vision decides a declared outpost
        // either way — an armed Threat standing in it writes the memory
        // forward, a room seen clear removes it — and a tick with no vision in
        // that room leaves the last conclusion standing. `RoomControl` is the
        // vision test for the same reason it is `Holds`': a seen room gets a
        // truthful control entry whoever holds it (`World.ofGame`), so an entry
        // is exactly "the colony looked into this room this tick".
        //
        // Where it parts from `Holds` is the clock. The engine counts a
        // reservation down and ends that record for us; nothing counts a raid
        // down, so the end is ours to choose — and it is a **backstop** rather
        // than a schedule (`Tuning.ThreatMemory`, now the raider's own longest
        // possible remaining life). What ends a memory in ordinary running is
        // the look above, because while the room is dark an expiry cannot mean
        // "the raid ended"; it can only mean "we stopped remembering", and the
        // row then reads a room full of invader as a room that is clear. Four
        // of W15S28's bodies were spent proving that (#369). An expired entry
        // is still dropped here rather than filtered by every reader, so the
        // leaf cannot accumulate raids no raider could have survived.
        //
        // Declared outposts alone, off the guard row's own derivation
        // (`Planner.declaredOutposts`, the list `raidDeadlines` above reads):
        // the [[home room]]'s raid is the [[keep]]'s business (ADR 0034), an
        // [[errand]]'s is a withdrawal (#348), and a [[transit room]] hires
        // nobody (#324) — a memory written for any of those would be one no
        // reader could ever act on.
        Threatened =
            let standing =
                prior.Threatened |> Map.filter (fun _ latch -> view.Time < latch.Until)

            let declared = Fabot.Core.Decide.Planner.declaredOutposts view |> Set.ofList

            let armedIn room =
                view.Hostiles
                |> List.exists (fun hostile ->
                    hostile.Pos.Room = room && Decide.Facts.isArmed hostile)

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
        Living = if Option.isSome episode then surviving else Set.empty
        Placed = if Option.isSome episode then placedNow else Map.empty
        // The damage baseline, carried on the same condition the damage is
        // charged on (#201): an open episode *and* a hostile in the room the
        // Keep stands in. A tick that carries none leaves the next one nothing
        // to difference against, so an episode held open from an outpost
        // charges zero rather than this room's decay — and the one tick after
        // a raider leaves *this* room still finds the baseline it needs.
        Hits =
            if Option.isSome episode && not (List.isEmpty atHome) then
                defended
            else
                Map.empty
    }

/// What `Game.cpu.getUsed()` answered at each of the loop's phase boundaries,
/// in the order the tick ran them, plus the intents the engine accepted (#170).
/// Cumulative, every one of them, because that is what the engine's counter is:
/// the differencing is this module's job and happens once, here. `AtEntry` is
/// the odd one out and the reason the split was built: it is read on the loop's
/// first line, so it is what the engine had already spent before `loop` was
/// entered, and it stands on its own in every readout.
type CpuReadings =
    {
        AtEntry: float
        AtSnapshot: float
        AtDecide: float
        AtSave: float
        AtExecute: float
        Intents: int
        /// `Game.cpu.bucket` as the tick ended: the margin, which is the one
        /// number that says whether a spike matters (#357). Read live rather
        /// than derived, because the engine's own arithmetic — a tick may spend
        /// up to `limit + bucket` capped at 500 ms, and what it does not spend
        /// it banks — is the thing being checked, not restated.
        Bucket: int
        /// How many of this bot's colonies threw their plan memo away this tick
        /// (ADR 0033). Carried on the CPU line rather than inferred from it
        /// because a `decide` six times its own mean is either a replan or a
        /// pricing storm, and a reader cannot tell those apart from a total.
        Replans: int
        /// What each colony spent inside `decide`, home room and milliseconds,
        /// in the order they decided.
        ///
        /// The `Decide` phase is the tick's, deliberately (ADR 0047: the column
        /// is what the tick spent deciding, not what one colony did), and that
        /// is the column ADR 0041's trigger is read off. But it cannot say
        /// *which* colony a spike came out of, and every CPU refusal this bot
        /// has recorded was a measurement taken on the wrong shape — #332's
        /// keeper mask read 0.00% because it was measured on `pair`, the one
        /// harness world with no keeper room. Four colonies with four
        /// projections is four shapes, and this is the reading that says which
        /// of them to take a profile of.
        ///
        /// Cumulative like the rest and differenced by `foldCpu`: the shell
        /// reads the counter at each colony's boundary and knows nothing else.
        ColonyDecides: (string * float) list
        /// The same trick one phase earlier: the counter as each room's facts
        /// finished, in sweep order, so `snapshot` can be attributed to the
        /// rooms it swept. `snapshot` is a fifth of the live tick and the
        /// harness cannot price one millisecond of it — its rooms are stubs
        /// whose `find` hands back a ready array — so the split is the only
        /// reading about that phase the two can be compared on (#370).
        RoomSnapshots: (string * float) list
        /// The counter as the sweep began. The rooms are differenced against
        /// this and not against `AtEntry`, because the phase does work before
        /// the first room and after the last — so the rooms sum to **less**
        /// than the phase, and the difference is readable the way `decide`'s
        /// remainder is (ADR 0041: measured, not budgeted).
        AtRooms: float
        /// The boundary before the first colony's projection, and the readings
        /// after each of them. `snapshot` is not the engine's sweep alone: the
        /// four projections stand inside that column too, and on the first
        /// window that could see it they were the larger part — 7.63 ms of a
        /// 14.9 ms phase stood after the last room was swept (#370).
        AtProjects: float
        ColonyProjects: (string * float) list
    }

/// One tick's cost, split at the loop's phase boundaries: the engine's prelude
/// and then four differences, each in milliseconds, and the count of intents
/// the engine accepted that tick (#170). The split exists to attribute a gap
/// the ruler cannot see. `npm run profile` measures the same scenario at 10.45
/// ms/tick while the live colony's line reads a 49.4 ms mean, and the harness
/// has no engine: no 0.2 CPU per intent, no prelude, no Memory parse. A single
/// total cannot say which of those the missing factor is, so every row carries
/// the phases and the intent count, and the arithmetic is left to whoever reads
/// it (ADR 0041: measured, not budgeted).
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

/// One tick's cost, as the engine measured it: the tick it was measured on, the
/// milliseconds the bot had spent by the time it stopped looking (ADR 0041),
/// and — for a row this bundle wrote — where those went. `Phases` is `None`
/// only for a row off the wire that a bundle wrote before the split, a zero
/// record saying the phase cost nothing rather than that nobody measured it;
/// `foldCpu` never writes one. The tick number rides
/// the row rather than being implied by its place in the ring, because a tick
/// the loop never finished writes no row at all and a gap in the numbers is the
/// one thing this line can say that a bare list cannot.
type CpuSample =
    {
        Tick: int
        Ms: float
        Phases: CpuPhases option
        /// What each colony spent inside `decide`, home room and milliseconds.
        ///
        /// A list and not an `option`, unlike `Phases`: a row from a bundle
        /// that did not measure this reads as the empty list, which is the
        /// honest answer for a reading nobody took and also the correct answer
        /// for a tick in which no colony decided at all. The distinction
        /// `Phases` needs — a zero that was measured against a zero nobody
        /// looked at — does not arise here, because the split is only ever read
        /// against `Phases.Decide`, which says whether there was anything to
        /// attribute.
        ///
        /// Kept off `CpuPhases` on purpose. That group decodes all-six-or-none,
        /// so growing it would make every row the previous bundle wrote read as
        /// unmeasured, and the window this is meant to compare against is the
        /// hundred rows standing when the change lands.
        Colonies: (string * float) list
        /// Kept off `CpuPhases` for `Colonies`' reason, and decoded apart from
        /// it for the same one.
        Rooms: (string * float) list
        /// What the sweep spent **before** its first room: the visible rooms
        /// enumerated, every creep grouped by where it stands, the
        /// declarations read. Carried as its own number because the first live
        /// window put it at 1.9 ms — more than any single room — and a reader
        /// that could only subtract `head + tail` together could not tell
        /// which of the two to go after (#370).
        SweepHead: float
        /// Each colony's projection, differenced the way its decision is.
        Projects: (string * float) list
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
/// layered projection as two numbers — a mean tick above 50 ms, or any single
/// tick above 80 — and both are read off this window, so the window has to be
/// long enough that the mean is not one tick's opinion. The census-keyed memos
/// (ADR 0017, ADR 0032) make one tick in a while several times the cost of its
/// neighbours; at twenty rows that recompute is a twentieth of the mean, and at
/// a hundred it is a percent.
let capCpuTicks = 100

/// The measured cost, kept to the microsecond. The engine hands back a float
/// with more digits than anyone reads and Memory pays for every one of them; a
/// microsecond is finer than the profiler's own 100µs sampling interval, so
/// nothing a reader could act on is rounded away.
let private toMicrosecond (ms: float) = floor (ms * 1000.0 + 0.5) / 1000.0

/// The CPU line's fold (ADR 0041): this tick's cost joins the ring, oldest
/// first, and the newest `cap` rows survive. No change detection and no episode
/// — every tick costs something and the whole point is the shape of the
/// distribution, so unlike the Transition log a quiet tick still writes. The
/// judgement over the ring is deliberately not here. ADR 0041 decided CPU is
/// *measured, not budgeted*, so the two thresholds are the reader's and live
/// with the readers (`scripts/cpu-trigger.mjs`): a threshold in Core is a thing
/// the bot could act on, and skipping a tick collides with the safe-mode reflex
/// (ADR 0007, ADR 0015), which has to fire on the very tick a guard would skip.
/// The readings arrive cumulative and are differenced here (#170), which is the
/// one place they can be: the shell reads a counter at each boundary and knows
/// nothing else, and every reader handed cumulative numbers would have to
/// subtract them again off a shape nothing pins. The tick's total stays
/// `AtExecute`, the last reading.
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
    // the phase's own start: the shell reads one counter per colony and the
    // subtraction belongs wherever the other four already are. The sum is the
    // `Decide` phase less what the tick spent between colonies — the movement
    // arbitration and the two Memory reads `decide` is handed — so a reader
    // comparing the two is reading that remainder, which is why neither number
    // is derived from the other.
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

    {
        Ticks =
            prior.Ticks
            @ [
                {
                    Tick = tick
                    Ms = toMicrosecond readings.AtExecute
                    Phases = Some phases
                    Colonies = colonies
                    Rooms = swept
                    SweepHead = toMicrosecond (readings.AtRooms - readings.AtEntry)
                    Projects = projects
                }
            ]
            |> trim cap
    }

/// What one live invariant check found broken this tick (#355). The fourth
/// observe channel's vocabulary, and the reason it exists at all: the test
/// suite runs on fixtures this repo authors, so it confirms the code's belief
/// about the projection rather than the engine's behaviour — 63 of the 65 test
/// files are hand-written fixtures, and `src/App/World.fs`, where the
/// projection is actually built, has no tests by construction. Four live
/// incidents in the Thorium delivery programme shipped green through 1,389 of
/// them, and three happened in exactly that untested half. What the layout
/// channel already does for the plan — assert on the *real* projection every
/// tick, and report a footing target with no footing — this does for the four
/// facts those incidents broke.
///
/// A closed vocabulary rather than a message string, for `Ownership`'s reason:
/// the kind crosses the wire, it is what the reader groups and sorts by, and a
/// kind added without its wire name fails the build rather than printing as a
/// blank — `breachKindName` matches the union exhaustively and an incomplete
/// match is an error here (`Directory.Build.props`).
[<RequireQualifiedAccess>]
type BreachKind =
    /// Ore decaying in a room this colony projects and may sweep, with the
    /// object it is decaying in as the `Subject`. 915 T of it — about 4,500
    /// season points — sat on the Reactor room's floor for hours because no
    /// rule could name it (#354's second half): `Facts.ourThoriumPiles`
    /// filtered "a room we own", and a Reactor room has no controller, so its
    /// floor belonged to nobody.
    ///
    /// **Two objects and one kind** (#359): a Dropped Thorium pile, and a
    /// tombstone or a ruin holding Thorium — a courier that died loaded, 175 T
    /// at W15S25's (43,6). The name is the floor's because that is where the
    /// second ends up: a tombstone drops its whole store as piles when it
    /// decays, so the two are one incident a few hundred ticks apart, and they
    /// ask the operator for one response — send a body to draw it. The
    /// `Subject` says which object it stood in.
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
    /// A declared Reactor of ours that still burns, with **no courier alive to
    /// reach it before it stops** (#361). `ReactorStarved` above fires on the
    /// tick the streak is already gone; this is the same incident read while it
    /// can still be answered, and it is the one kind here whose entire value is
    /// arriving early.
    ///
    /// **The threshold is the lead time, and that is the whole design.** It
    /// fires when the store holds fewer ticks of burn than it takes to put a
    /// load under the flag from a standing start — casting the fixed body plus
    /// walking it out — so it fires on the *last* tick an answer still works
    /// and never a tick before. Both errors cost: a tick later saves nothing,
    /// and an alarm that fired on a comfortable store would be answered by a
    /// courier hired early, which then stands at the Reactor burning down its
    /// 1,500-tick life for nothing.
    ///
    /// Why it was filed, live at W15S28 t499742: store 315 and falling 1 a
    /// tick, last delivery 685 ticks earlier, courier quota 0 — with 7,226 T
    /// banked at home and a 7,989-tick streak standing. Read 165 ticks too late
    /// to save it. Nothing in this channel said a word, because the store was
    /// not yet zero.
    | ReactorRunningDry
    /// A declared Reactor whose own row says it is not ours — somebody walked a
    /// CLAIM body in, or the re-claimer died before its relief arrived. Every
    /// tonne delivered while that stands scores for whoever holds the flag.
    | ReactorLost

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
    }

/// One breach as the log carries it: the breach itself, the tick it was first
/// seen, and the tick it was last confirmed.
///
/// **The age is the whole reason this channel folds at all.** A snapshot of
/// this tick's violations is a list live already answers; what it cannot answer
/// is whether a pile on the floor is bleeding or whether a courier is three
/// ticks from picking it up, and `tick - FirstSeen` is exactly that
/// distinction.
///
/// `LastSeen` is the tick the fold last confirmed this row, which is the tick
/// the fold last ran: a breach that stops appearing **drops out** (`foldBreaches`),
/// so every standing row carries the current tick here. It is kept all the same
/// because the leaf is read hours later out of Memory by `observe.mjs breaches`
/// with no game clock of its own, and `LastSeen - FirstSeen` is the age as of
/// the last tick the bot wrote — a self-dating record, and the one thing that
/// tells a reader whether the bundle that wrote it is still running.
type StandingBreach =
    {
        FirstSeen: int
        LastSeen: int
        Breach: Breach
    }

/// The whole persisted breach log: one row per (kind, subject) standing right
/// now. A map and not a ring, because this channel answers **"what is broken
/// now"** and nothing else — a breach that clears leaves no trace here, and the
/// Raid log beside it is where episodic history lives (ADR 0028). That is a
/// decision and not an oversight: a channel an operator has to date-filter
/// before it means anything is one more thing to read, and the two questions
/// have two answers already.
type BreachState =
    {
        Standing: Map<BreachKind * string, StandingBreach>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BreachState =
    /// The empty breach log — what an absent, malformed or foreign-shaped leaf
    /// reads as, the way `RaidState.empty` and `CpuState.empty` do. Empty is
    /// also the healthy steady state here, unlike either of those.
    let empty = { Standing = Map.empty }

/// The breach log's cap, and the sibling `capEpisodes` number for its reason: a
/// few hundred bytes a row against a 2MB Memory, and twenty rows is already
/// more than an operator acts on in one sitting. It bounds a shape that is
/// otherwise unbounded from the engine's side — a mining accident can leave
/// dozens of piles — and the cap is what keeps one bad tick from writing a leaf
/// nobody can parse.
let capBreaches = 20

/// The declared Reactors this colony can actually see, each with the room its
/// declaration names (#355). The join is the errand's: `ColonyView.Reactors`
/// carries the rows and no room, and `ColonyView.Errands` carries the room and
/// the engine id, so a row is answered for only where a declaration names it.
///
/// **A Reactor we cannot see yields nothing here, and therefore no breach of
/// any kind** (ADR 0004): no row, no reading. That is the same rule
/// `Facts.reactorTakesALoad` closes its draw on, and it has to be stated on
/// this side too — an invariant channel that read absence as a violation would
/// alarm on every tick the errand's resident body is between lives.
let private declaredReactors (view: ColonyView) : (string * ReactorInfo) list =
    view.Errands
    |> List.choose (fun errand ->
        let reactorId = fst errand.Target

        view.Reactors
        |> List.tryFind (fun reactor -> reactor.Id = reactorId)
        |> Option.map (fun reactor -> errand.RoomName, reactor))

/// This tick's violations, in kind order. Every check is O(creeps + declared
/// targets + piles) with no flood, no `Atlas.routes` and no `walkTicks`: ADR
/// 0041's revisit trigger is a mean tick above 50 ms and the live line already
/// reads 49.4, so a channel that priced a walk would spend the budget the
/// projection is being judged on.
let private breachesIn (view: ColonyView) : Breach list =
    // The ore on the floor, over exactly the reach `Facts.ourThoriumPiles` has
    // since #354: a room we own, **or** a room we declared an errand in. That
    // second clause is the whole of the incident — the Reactor's room has no
    // controller, so "a room we own" made its floor invisible while a body of
    // ours stood two tiles away and the pile decayed at 1 T a tick.
    //
    // Read through `Facts` rather than restated: `Observe` compiles after
    // `Decide/Facts.fs` (`Core.fsproj`), so the one sentence about whose a pile
    // is has one home, and the alarm can never come to disagree with the
    // Pickup that is supposed to answer it.
    //
    // **That errand half fired on nothing when this channel was wired**, and
    // the note is kept because it is the whole lesson: `ColonyView.ofWorld`'s
    // `erranding` cut emptied an errand room's kind census, so a pile on the
    // Reactor's floor reached the view with no kind and no amount — reproduced
    // against `ViewTests`' own errand world while wiring this channel (#356),
    // where a 915 T pile read back as `TargetKinds: None, Thorium: None`. The
    // narrowing was widened to admit it in #356 and the silence is over; what
    // that leaves is the rule the pair was filed under — a check and the
    // projection it reads are pinned together or not at all, which is why each
    // ore case here has a `ViewTests` counterpart written with it.
    //
    // **And the ore that is not on a floor at all** (#359), swept in the same
    // breath and under the same kind. A courier that dies loaded leaves its ore
    // in its tombstone — 175 T at W15S25's (43,6) — and a tombstone drops its
    // whole store as piles when it decays
    // (`processor/intents/tombstones/tick.js`), so the pile check above catches
    // this ore one step late and minus whatever the decay took. The ticks in
    // between are the point of covering it directly.
    //
    // `OreOnTheFloor` and not a kind of its own, deliberately: a breach kind is
    // what the reader groups and sorts by, and these two rows say one thing —
    // ore this colony may sweep is decaying somewhere it is not being swept —
    // and ask for one response, a body sent to draw it. What differs is the
    // clock, which is faster here, and the `Subject` already says which object
    // it is. A second kind would make an operator learn a vocabulary to take
    // the same action twice.
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
                }))

    let reactors = declaredReactors view

    // The ore that has nowhere to go: a body of ours standing **in the declared
    // Reactor's room** holding more than the Reactor's free space. `transfer`
    // into a full store moves what fits and answers `ERR_FULL` for the rest, so
    // the surplus stays aboard and is dropped when the body dies or is
    // re-tasked — which is how 915 T reached that floor.
    //
    // The free space is read off `view.Reactors` — the Reactor's own row — and
    // **never** off `SpatialInfo.Thorium`, which carries every store a Task can
    // name and deliberately not this one (`RoomFacts.Thorium`'s own comment).
    // #354's draw gate read the wrong map: it answered 0 for a store holding
    // 999, the gate never closed once in flight, and the ore went on arriving
    // at a full Reactor. Its unit test agreed with it, because the fixture
    // wrote the store where the gate looked — a projection shape `World` has
    // never built. An alarm written against that same map would have been
    // silent through the whole incident it exists to catch, which is why this
    // check is pinned from both sides in `ObserveTests`.
    //
    // **Narrowed to the bodies standing in that room**, and the narrowing is
    // the difference between an alarm and a noise generator: "any creep of ours
    // holding Thorium" fires on every [[miner]] at home whenever the Reactor is
    // near full, and that ore is on its way to a container, not to the Reactor.
    // A body standing in the errand room is there for one reason — the errand
    // declares one object and pools nothing else (ADR 0060 decision 1) — so its
    // load has exactly one destination. The cost of the narrowing is that a
    // surplus is named when it arrives rather than when it is drawn; the draw
    // gate is the rule that prevents it, and this is the channel that says the
    // gate failed.
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
                        }
                else
                    None))

    // The Reactor that will stand dry before anybody can reach it (#361). Three
    // terms, all off rows this view already carries — no flood and no priced
    // walk, for the reason this function's own docstring gives.
    //
    // `leadTicks` is what the alarm is timed against: 90 ticks to cast the
    // fixed courier body (30 parts at the engine's 3 ticks a part,
    // `CREEP_SPAWN_TIME`) plus the walk out. The walk is **a floor and not a
    // price** — 50 ticks a room crossing, which is a full room width at the
    // one-tile-a-tick this body does on roads (20 loaded Carry against 10 Move
    // is 10 fatigue a tile on road, under the 20 the Move parts clear). Live
    // measured 159 ticks over W15S28's three crossings against the 150 this
    // floor gives, so it under-reads by about 6% and therefore fires slightly
    // *late* rather than early. A priced walk would be honest to the tile and
    // would also cost the `Atlas.routes` this channel refuses to spend.
    ///
    /// `None` when the names do not join, which is a declaration this colony
    /// should not be holding at all (`Errand.routable`): the alarm stays silent
    /// rather than guessing a distance, because a lead time guessed too long is
    /// a row that cries on every tick forever.
    let leadTicks (room: string) =
        let cast = List.length courierPattern.Block * 3

        view.Spatial.RoomName
        |> Option.bind (fun home -> RoomName.hopsBetween home room)
        |> Option.map (fun hops -> cast + hops * 50)

    // A body is a courier by its **shape**, matched against the row's own
    // pattern rather than its name: the name is a spawn-time string this
    // channel would have to parse, while `courierPattern.Block` is the fact the
    // quota hires against, so the two cannot come to disagree about what a
    // courier is.
    let couriers =
        view.Creeps
        |> List.filter (fun creep ->
            partCount creep.Body Carry = partCountIn courierPattern.Block Carry
            && partCount creep.Body Move = partCountIn courierPattern.Block Move)

    // One store tick is one T: the Reactor burns exactly 1 a tick
    // (`docs/research/thorium-reactor.md`), so the store *is* the clock and no
    // rate has to be estimated.
    // **A body of the right shape is not a delivery in progress** (#367). The
    // first version of this alarm fell silent whenever a courier lived, on the
    // premise that a courier alive means a load is coming. Live it was alive for
    // 465 ticks hauling *energy*, because the delivery draw ranked below
    // ordinary hauling: the store fell 500 -> 0, a 15,582-tick streak broke, and
    // this channel said `no breaches` throughout. The rank is fixed; the premise
    // was wrong on its own terms and is fixed here.
    //
    // What counts as an answer is **ore actually moving**: a courier-shaped body
    // holding Thorium. That is a fact of the view (`CreepInfo.Thorium`) and not
    // an assignment, so this channel stays out of the Matcher's business (ADR
    // 0025) and cannot be told a delivery is under way by a body that is doing
    // something else.
    //
    // The cost is a false alarm for the ticks between a courier being cast and
    // its first load: it is walking to the Storage with an empty store, and this
    // reads that as nobody answering. That direction is the correct one for an
    // alarm whose whole value is arriving early (#361), and the row clears
    // itself the tick the load is aboard.
    let laden = couriers |> List.exists (fun courier -> courier.Thorium > 0)

    let runningDry =
        if laden then
            []
        else
            reactors
            |> List.filter (fun (room, reactor) ->
                reactor.Owner = ReactorOwner.Ours
                && reactor.Thorium > 0
                && leadTicks room |> Option.exists (fun lead -> reactor.Thorium <= lead))
            |> List.map (fun (room, reactor) ->
                {
                    Kind = BreachKind.ReactorRunningDry
                    Room = room
                    Subject = reactor.Id
                    // The ticks of burn left, which is what the operator acts
                    // on: it counts down every tick the row stands, and the
                    // row's age says how long nobody has answered.
                    Amount = reactor.Thorium
                })

    // A Reactor of ours standing dry. The programme pays 1 point per T at a
    // broken streak against the multiplier a continuous one earns
    // (`docs/research/thorium-reactor.md`), so an empty store is income lost
    // rather than damage taken — and it is the one of the four kinds that is
    // routinely *transient*, which is exactly what the age column is for: a
    // store empty for one tick is a delivery landing, and one empty for four
    // hundred is the supply chain broken.
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
            })

    // A declared Reactor whose row says the flag on it is not ours. Any owner
    // but ours, the rival's and the unowned alike: what is actionable is that
    // nothing delivered there scores for us, and that is equally true of a
    // Reactor somebody claimed and of one whose ownership lapsed when the
    // re-claimer died. The two are one `ReactorOwner` case apart on the wire
    // and one act apart on the ground — walk a CLAIM body back in (ADR 0060
    // decision 3).
    let lost =
        reactors
        |> List.filter (fun (_, reactor) -> reactor.Owner <> ReactorOwner.Ours)
        |> List.map (fun (room, reactor) ->
            {
                Kind = BreachKind.ReactorLost
                Room = room
                Subject = reactor.Id
                Amount = 0
            })

    decayingOre @ unplaceable @ runningDry @ starved @ lost

/// Trim the log to the cap. Oldest **last-seen** first, the way `capEpisodes`
/// trims its ring — and with the tie-break stated, because here the tie is the
/// normal case rather than the exotic one: a row that stops appearing drops out
/// altogether, so every surviving row was last seen on this very tick and the
/// primary key is tied across all of them. What decides it then is `FirstSeen`,
/// **newest dropped first**: the row that has stood longest is the one bleeding
/// longest, and a cap that evicted it would silence exactly the breach worth
/// reading. The remaining tie — two rows opened on one tick — is broken on kind
/// and subject so the eviction is a function of the state and not of map
/// ordering.
let private capStanding (cap: int) (rows: Map<BreachKind * string, StandingBreach>) =
    let overflow = Map.count rows - cap

    if overflow <= 0 then
        rows
    else
        rows
        |> Map.toList
        |> List.sortBy (fun ((kind, subject), row) -> row.LastSeen, -row.FirstSeen, kind, subject)
        |> List.skip overflow
        |> Map.ofList

/// The breach log's fold (#355): this tick's view plus the previous log produce
/// the new one. A violation this tick keeps the tick it was **first** seen on,
/// so the row ages; a violation this tick did not read is gone, whatever it
/// said last tick.
///
/// **Dropping out is the decision this channel is built on.** A row that
/// lingered would make the log a history, and a history needs a reader who
/// knows which rows are current — the failure mode of every stale dashboard.
/// The consequence is deliberate and worth naming: a breach that flickers off
/// for one tick loses its age and reads as new. That is the right trade for
/// the four kinds here, all of which are *conditions* the projection re-reads
/// every tick rather than *events*; an episodic reading of the same ground is
/// `foldRaids`' job.
///
/// A blind tick is not a quiet one, and nothing here pretends otherwise: a
/// Reactor with no row and a room with no vision contribute no breach, so the
/// log empties while the colony cannot look. That is ADR 0004 taken to its
/// conclusion — absence is never evidence — and it is why this channel says
/// "nothing is broken that I can see" and never "nothing is broken".
let foldBreaches (cap: int) (tick: int) (view: ColonyView) (prior: BreachState) : BreachState =
    {
        Standing =
            breachesIn view
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
                    // The latest reading and not the first: the amount is what
                    // makes the row actionable, and a pile that has grown from
                    // 90 T to 915 must say 915. The opposite rule to the raid
                    // roster's first-sighting-wins, and for the opposite reason
                    // — that record answers what came, this one answers what is
                    // standing there now.
                    Breach = breach
                })
            |> Map.ofList
            |> capStanding cap
    }

/// The standing rows, **oldest first**: the order every reader of this channel
/// wants, computed once here so neither the Memory writer nor `observe.mjs`
/// has an ordering of its own to drift away from. Ties on the opening tick are
/// broken on kind and subject, so the same log always reads out in the same
/// order.
let breachRows (state: BreachState) : StandingBreach list =
    state.Standing
    |> Map.toList
    |> List.sortBy (fun ((kind, subject), row) -> row.FirstSeen, kind, subject)
    |> List.map snd

/// The live rows against a clock: each breach with how many ticks it has been
/// standing, oldest first. The whole of what a consumer needs — no filtering,
/// because a row that is in the log is standing by construction, and no
/// sorting, because `breachRows` has done it.
let standing (tick: int) (state: BreachState) : (Breach * int) list =
    breachRows state |> List.map (fun row -> row.Breach, tick - row.FirstSeen)
