/// The observe channel's pure folds: the Transition log's, keyed by creep (ADR
/// 0009); the Raid log's, colony-level and episodic (ADR 0028); and the CPU
/// line's, one row per tick (ADR 0041).
module Fabot.Core.Observe

open Fabot.Core.Types

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
/// Transition log's fold has already pruned it.
type Loss = { Creep: string; Tick: int }

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
        /// it, **belonged to** another player, each against the tick that look
        /// was taken on: ADR 0043's *other* withdrawal, the one that needs no
        /// clock, because a room somebody else holds has not been made
        /// dangerous — it has stopped being ours to work. Ownership alone since
        /// #165: a rival's *reservation* stood here too until the cost of
        /// latching a room for a hold that decays in at most 5,000 ticks was
        /// priced, and it is now a clocked episode in the ring above, where the
        /// engine's own countdown says when the room comes back. The tick is
        /// still no clock and nothing compares against it — what compares
        /// against it is the **stride** between looks (`standDown`, #165), which
        /// re-admits the room to the scan once every `Tuning.RivalRecheck` ticks
        /// so a latch the rival has walked away from can be cleared by the only
        /// thing that ever could. It is also the trace the gate's closing leaves
        /// in the observe channel (#117's US-20), which is how that channel
        /// answers which tick a room's income stopped arriving on.
        /// The first look's tick and not the last, because that is the tick the
        /// gate shut — and because the stride is measured from it, a look that
        /// finds the rival still there must leave it where it stands or the next
        /// look would never fall due. A remembered conclusion and not a per-tick
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
        RivalHeld: Map<string, int>
        /// The owned creep names the previous tick projected, less the ones
        /// whose life ran out on it: the baseline this tick's losses are read
        /// against. Carried only while an episode is open, so a creep that
        /// dies in peacetime is read against an empty baseline and recorded
        /// nowhere. This colony's names and not the world's (ADR 0047), which is why the difference `foldRaids` takes is
        /// against the world's living names: a creep another colony adopted
        /// leaves the baseline without dying.
        Living: Set<string>
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
            Living = Set.empty
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
                Body = hostile.Body |> List.countBy id |> Map.ofList
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
        |> Option.bind (fun control -> control.Reservation)
        |> Option.filter (fun held -> held.Holder = ReservationHolder.Invader)
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
        control.Reservation
        |> Option.filter (fun held -> held.Holder = ReservationHolder.Rival)
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
/// **And it is read only for a raid the guard row's cap cannot beat**, which is
/// what keeps this from cancelling ADR 0056 before it fights. Standing a room
/// down withdraws it from the scan set, so a raid that shut the room the tick
/// it appeared would hide its own hostiles and no guard would ever be hired: a
/// withdrawal and a garrison are the same room's two answers, and this is where
/// they are told apart. The arithmetic is `guardsWanted`'s own, at the cap:
/// two blocks' damage against the raid's armed hits, and two blocks' hits
/// against the raid's full damage: melee attacks exclude self-healing. A raid two
/// guards beat is a fight; a raid two guards lose is a room to leave, and it is
/// left for exactly as long as the raid has to live.
let private raidDeadlines (view: ColonyView) =
    let armed (h: HostileInfo) =
        h.Body |> List.exists (fun p -> p = Attack || p = RangedAttack)

    view.Hostiles
    |> List.filter armed
    |> List.filter (fun h -> h.Pos.Room <> SpatialInfo.homeName view.Spatial)
    |> List.map (fun h -> h.Pos.Room)
    |> List.distinct
    |> List.filter (fun room -> not (Decide.Quota.guardBlocksBeat view room Engine.guardCap))
    |> List.map (fun room ->
        let life =
            view.Hostiles
            |> List.filter (fun h -> h.Pos.Room = room)
            |> List.map (fun h -> h.TicksToLive)
            |> List.max

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
/// look with vision says otherwise, and the looks are `Tuning.RivalRecheck`
/// apart (`standDown`).
let private rivalOwned (control: RoomControlInfo) = control.Owner = Ownership.Rival

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
/// `Shut` is every room a stand-down's clock is still running in, and every room
/// the colony last saw in another player's ownership. `Rechecked` is the second of
/// those families alone, and only on the ticks a whole `Tuning.RivalRecheck`
/// after the look that shut the room: the latch's own effect is to stop
/// scanning the room, so left alone it can never meet the tick with vision that
/// would clear it, and the room stays abandoned long after the rival has gone
/// (#165). The stride is measured from the recorded tick and never restamped, so
/// a look that finds the rival still there leaves the next one a full
/// `RivalRecheck` away rather than starting the count again. A `RivalRecheck` of
/// zero or less is "never look again", which is this rule switched off rather
/// than a division by zero.
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
            |> List.filter (fun (_, since) ->
                tuning.RivalRecheck > 0
                && tick > since
                && (tick - since) % tuning.RivalRecheck = 0)
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
        view.Spatial.Hits
        |> Map.toList
        |> List.choose (fun (id, hits) ->
            match Map.tryFind id view.Spatial.TargetKinds with
            | Some(Structure kind) when isDefence kind -> Some(id, hits.Hits)
            | _ -> None)
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
        RivalHeld =
            (prior.RivalHeld, view.RoomControl)
            ||> Map.fold (fun rooms room control ->
                if rivalOwned control then
                    if Map.containsKey room rooms then
                        rooms
                    else
                        Map.add room view.Time rooms
                else
                    Map.remove room rooms)
        Living = if Option.isSome episode then surviving else Set.empty
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
