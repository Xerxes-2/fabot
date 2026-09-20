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

/// One row of an episode's roster: one hostile, who owns it and what it is
/// made of.
type RosterRow =
    {
        Owner: string
        Body: Map<BodyPart, int>
    }

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
    }

/// One raid: opened on the first tick any room the colony works and can see
/// holds a hostile, kept open while hostiles keep appearing, closed by a quiet
/// gap. Colony-level, so it names no room; its coordinates carry their own.
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
        /// the room. A record for the reader and never the openness test: the
        /// colony stops looking the moment it withdraws, so silence here says
        /// nobody is there to look and never that the room is clear.
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
let private approachAt (view: ColonyView) : Approach option =
    // Armed hostiles alone: a `1 MOVE` scout at range 1 is neither a probe
    // nor a loss, and one was once read as the raid (#376). The scouts stay
    // on the roster.
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

/// The tick one core's stand-down runs to, and which deadline it was read
/// off, in order of availability. `TicksToEnd` is the engine's relative count,
/// so the tick is this one plus it. The reservation branch may never answer
/// earlier than the fallback: a core re-reserves the controller the tick the
/// hold lapses, and one that has just `attackController`'d holds it for three
/// ticks (`invader-core/reserveController.js`), which read literally would be
/// a three-tick stand-down.
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

/// The tick a room another player has reserved stands down to: the end of
/// that reservation, off the engine's own countdown. No floor, unlike
/// `deadlineOf`'s reservation branch: a player's claimer that stops coming
/// re-takes nothing. Ownership is not here: the engine gives it no end, so it
/// stays the latch `RivalHeld` remembers.
let private rivalDeadlines (view: ColonyView) =
    view.RoomControl
    |> Map.toList
    |> List.choose (fun (room, control) ->
        control
        |> RoomControlInfo.heldBy ReservationHolder.Rival
        |> Option.map (fun held ->
            room, (view.Time + held.TicksToEnd, StandDownBasis.RivalReservation)))

/// The tick a room stands down to when the raid in it is one the colony has
/// decided not to fight: the raid's own life, since an Invader in a room
/// nobody owns never suicides (the engine's suicide branch wants a controller
/// owner). For an outpost, only a raid the guard row's cap cannot beat: a
/// withdrawal and a garrison are the same room's two answers, told apart
/// here. For an errand room, any armed raid but the Source Keepers, since no
/// guard is hired there. A transit room is neither answer.
let private raidDeadlines (view: ColonyView) (outposts: Fabot.Core.Decide.Planner.OutpostFacts) =
    let errandRooms =
        view.Errands |> List.map (fun errand -> errand.RoomName) |> Set.ofList

    let outpostRooms = Set.ofList outposts.Declared

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

let private deadlines (view: ColonyView) (outposts: Fabot.Core.Decide.Planner.OutpostFacts) =
    // The stronghold bit is or-ed over a room's sightings where the clock is
    // maxed: a room seen once with a bunker and once with a raider is a room
    // with a bunker in it, whichever deadline is longer.
    (view.InvaderCores
     |> List.map (fun core ->
         let expiry, basis = deadlineOf view core
         core.RoomName, (expiry, basis, core.Level >= 1)))
    @ (rivalDeadlines view
       |> List.map (fun (room, (expiry, basis)) -> room, (expiry, basis, false)))
    @ (raidDeadlines view outposts
       |> List.map (fun (room, (expiry, basis)) -> room, (expiry, basis, false)))
    |> List.groupBy fst
    |> List.map (fun (room, seen) ->
        let found = seen |> List.map snd
        let expiry, basis, _ = found |> List.maxBy (fun (expiry, _, _) -> expiry)
        room, (expiry, basis, found |> List.exists (fun (_, _, bunker) -> bunker)))

/// Fold one room's sighting into the outpost ring: the room's standing
/// episode takes it, or the sighting opens one. The re-read only ever moves a
/// running clock outward, and the basis with it, so reading in a worse
/// deadline cannot cut a stand-down short.
let private sight tick (room, (expiry, basis, stronghold)) (episodes: OutpostEpisode list) =
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
                    // Sticky, where the basis is not: a bunker seen once is a
                    // bunker.
                    Stronghold = episode.Stronghold || stronghold
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
            }
        ]

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
        // The standing holds, on the same `tick < Until` test the fold keeps
        // them by. Filtered here as well so the gate cannot withhold a
        // reservation on a leaf the fold has not caught up with.
        HeldOutposts =
            state.Holds
            |> Map.toList
            |> List.filter (fun (_, hold) -> tick < hold.Until)
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
    }

/// ADR-0028
/// The Raid-log fold: this tick's view plus the previous Raid log produce the
/// new one. The two families are disjoint (one reads `Hostiles`, the other
/// `InvaderCores` and the controllers), so `cap` is a depth per family.
let foldRaids
    (cap: int)
    (alive: Set<string>)
    (view: ColonyView)
    (outposts: Fabot.Core.Decide.Planner.OutpostFacts)
    (prior: RaidState)
    : RaidState =
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
    // longer holds, stamped at the tick last seen alive. Differenced against
    // `alive` and never this colony's own names: an adopted body is still
    // standing.
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
        // A tick that sees nothing (no vision, or a clear room) leaves every
        // stand-down exactly as it found it, clock included.
        Outposts =
            (prior.Outposts, deadlines view outposts)
            ||> List.fold (fun episodes seen -> sight view.Time seen episodes)
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
        // The holds, on the latch's two rules (a tick with vision decides a
        // room either way, a tick without leaves the last conclusion), ending
        // themselves on the tick they named. An expired entry is dropped here
        // rather than filtered by every reader, so the leaf cannot accumulate
        // holds that ended hours ago.
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

            let declared = Set.ofList outposts.Declared

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
        // The damage baseline, carried on the condition the damage is charged
        // on: an open episode and a hostile in the room the Keep stands in.
        Hits =
            if Option.isSome episode && not (List.isEmpty atHome) then
                defended
            else
                Map.empty
    }

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

/// How many ticks one coarse span covers, and how many spans are kept: 20,000
/// ticks, which at the live rate of about 1,120 ticks an hour is five hours.
let spanTicks = 100

let capCpuSpans = 200

/// The measured cost, kept to the microsecond. The engine hands back a float
/// with more digits than anyone reads and Memory pays for every one of them; a
/// microsecond is finer than the profiler's own 100µs sampling interval, so
/// nothing a reader could act on is rounded away.
let private toMicrosecond (ms: float) = floor (ms * 1000.0 + 0.5) / 1000.0

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
                    Sum = open'.Sum + ms
                    Bucket = min open'.Bucket readings.Bucket
                    Replans = open'.Replans + readings.Replans
                    MaxPops = max open'.MaxPops pops
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
                }
            ]
            |> trim cap
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
            })

    decayingOre @ unplaceable @ runningDry @ starved @ lost

/// Trim the log to the cap: newest `FirstSeen` dropped first, since the row
/// that has stood longest is the one bleeding longest (every surviving row
/// ties on `LastSeen`). The remaining tie is broken on kind and subject so
/// the eviction is a function of the state and not of map ordering.
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

/// The breach log's fold: a violation this tick keeps the tick it was first
/// seen on; one this tick did not read is gone. Dropping out is the decision
/// the channel is built on (a lingering row would make the log a history),
/// and a breach that flickers off for one tick loses its age. A blind tick
/// contributes no breach, so the channel says "nothing is broken that I can
/// see" and never "nothing is broken".
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
