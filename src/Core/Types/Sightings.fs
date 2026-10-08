/// The tick's raw facts, per room and per creep: hostiles and invader cores,
/// construction sites, our own bodies, and the `World` that holds every room
/// seen this tick beside the sighting recalled from the last.
[<AutoOpen>]
module Fabot.Core.Types.Sightings

/// What the decision layer knows about one construction site this tick.
type ConstructionSiteInfo =
    {
        Id: string
        /// The energy still owed before this site becomes a structure —
        /// `progressTotal - progress`, because nothing decides on how far
        /// along a site is, only on what is left to pay (#364). It is what
        /// tells a road's 300 from a terminal's 100,000.
        Left: int
        /// Whether any energy has been built into it yet (`progress > 0`):
        /// a begun site in a [[nursery]] is what its safe mode guards.
        Begun: bool
    }

/// A body's weight and its live MOVE: what its pace over a tile is read off
/// (#510), one rule for a hostile and for ours.
type Gait = { Weight: int; Moves: int }

[<RequireQualifiedAccess>]
module Gait =
    /// The fatigue a weighted part pays stepping onto a road, and onto plain.
    let road = 1
    let plain = 2

    /// The weight of a body by part counts: every part but MOVE, and CARRY
    /// read empty, which over-states its pace, the safe direction.
    let weightOf (parts: BodyPart -> int) : int =
        parts Work
        + parts Attack
        + parts RangedAttack
        + parts Heal
        + parts BodyPart.Claim
        + parts Tough

    /// The gait of a body by its live part counts.
    let ofParts (parts: BodyPart -> int) : Gait =
        {
            Weight = weightOf parts
            Moves = parts Move
        }

    /// The gait of a body by its parts and the ones still acting: every part
    /// but MOVE weighs, destroyed ones too, since the engine charges fatigue
    /// on them, against its live MOVE.
    let ofBody (body: BodyPart list) (live: BodyPart list) : Gait =
        {
            Weight = weightOf (partCountIn body)
            Moves = partCountIn live Move
        }

    /// A body of ours' gait as the engine walks it (`movement.js`): every
    /// part but MOVE and CARRY weighs (`weighed`, destroyed ones too — the
    /// glance counts them, a CreepInfo reader adds `ShotOff`), and a
    /// live CARRY only while loaded, one a 50 aboard (`load`), against its live
    /// MOVE. The one reading of a body's walk, off `Game` or off a CreepInfo.
    let walked (weighed: int) (liveCarry: int) (liveMoves: int) (load: int) : Gait =
        {
            Weight = weighed + min liveCarry ((load + 49) / 50)
            Moves = liveMoves
        }

    /// The whole ticks a tile of this fatigue takes, each live MOVE paying
    /// two a tick; None for no MOVE acting, which never steps of itself.
    let ticks (fatigue: int) (gait: Gait) : int option =
        if gait.Moves <= 0 then
            None
        else
            Some(max 1 ((fatigue * gait.Weight + 2 * gait.Moves - 1) / (2 * gait.Moves)))

    /// Whether one gait falls behind another: strictly slower on plain, the
    /// ground a kite runs on, and so, its weight a MOVE heavier, quicker on
    /// none. A road where the two tie is the kite walk's to keep off, tile
    /// by tile on the arrival flood (#613): read here, it ruled every melee
    /// with half a MOVE a part as quick as a ranger, a 15-MOVE 17-ATTACK body
    /// that steps every second tick on plain among them. A body with no MOVE
    /// acting steps nowhere of itself: a puller walks it at the puller's
    /// pace, which is the puller's to answer for.
    let fallsBehind (ours: Gait) (theirs: Gait) : bool =
        match ticks plain theirs, ticks plain ours with
        | None, Some _ -> true
        | Some t, Some o -> t > o
        | _, None -> false

    /// Whether a quarry's gait outruns a chaser's (#531): strictly quicker on
    /// every ground the chase crosses, road, plain and swamp. Equal pace, or
    /// slower on any of them, is worth the chase: a real opponent runs into
    /// dead ends, swamp and exit bounces (the user, 2026-10-05). A named
    /// stand-in for the movement epic's progress checks and give-up budgets.
    let outruns (quarry: Gait) (chaser: Gait) : bool =
        [ road; plain; Engine.swampWeight ]
        |> List.forall (fun fatigue ->
            match ticks fatigue quarry, ticks fatigue chaser with
            | Some q, Some c -> q < c
            | Some _, None -> true
            | None, _ -> false)

    /// Five ticks a plain tile: a swamp tile's pace for a body at a MOVE a
    /// part, so whatever walks past it is gone before its next step.
    let private stationaryTicks = 5

    /// Whether a body is as good as rooted (#510): it threatens the tiles in
    /// its weapon's range and a step past them, not the room. Never one with
    /// no MOVE acting, which another body may pull.
    let stationary (gait: Gait) : bool =
        ticks plain gait |> Option.exists (fun ticks -> ticks >= stationaryTicks)

/// The reaction window (#531): the ticks our answering body needs, which is
/// what a hostile is read over — the parts its healing restores before our
/// strike lands, and whether it reaches a home a lent body has left. Read at
/// a tile a tick, the pace every fighting row walks plain at (a MOVE a part).
[<RequireQualifiedAccess>]
module Reaction =
    /// To close and strike: a garrison holds its room's middle, so the raid
    /// it answers, at its border or just past it, is half a room's side off.
    let strike = Engine.roomSide / 2

    /// To walk to a kill and back: a room's side each way, the walk a kill's
    /// call allows a lent body (`Brief.killWalk`).
    let lend = 2 * Engine.roomSide

/// What the decision layer knows about one hostile creep in a room the colony
/// is looking into this tick.
type HostileInfo =
    {
        Id: string
        /// Whose creep this is, as the engine spells the username ("Invader"
        /// for the NPCs). Attribution for the Raid log; no reflex reads it.
        Owner: string
        /// Where it stands, room and tile in one: a bare `Pos` would read one
        /// of ours on the same coordinate of another room as range 0.
        Pos: RoomPos
        Body: BodyPart list
        /// Each part's boost, head first as `Body`, or empty for a body with
        /// none (the common case). On the creep and not in an id-keyed map
        /// beside it: every question below asks it of the hostile alone.
        Boosts: Boost list
        /// Hits left (#451): what a fight's kill order reads, and with the
        /// engine taking parts from the head, which of `Body` still act.
        Hits: int
        /// Ticks this hostile has left. An Invader standing in a room nobody
        /// owns never suicides — the engine's suicide branch wants a
        /// controller owner and an [[outpost]] has none — so what it has left
        /// is exactly what it will spend, and this is the one deadline a raid
        /// with no core in it offers the [[stand-down]] (#257).
        TicksToLive: int
    }

/// The one set of questions asked of a hostile, every rule reading
/// these and spelling none of its own. Each part-question is asked of the
/// hostile **in effect**, beside the side it stands among
/// (`effectiveParts`): its parts still acting (`Engine.liveParts`) and the
/// ones the healing reaching it restores inside the reaction window.
/// The `...Of` questions read a part list the caller hands them; the
/// owner-questions (`isKeeper`, `crossesExits`) read no parts.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HostileInfo =
    /// A Source Keeper's owner, as the engine spells it.
    let private keeperOwner = "Source Keeper"

    /// Whether an owner is the Source Keeper's: the room's own NPC, never a raid.
    let isKeeperOwner (owner: string) : bool = owner = keeperOwner

    /// Whether a hostile is a Source Keeper (`isKeeperOwner`).
    let isKeeper (hostile: HostileInfo) : bool = isKeeperOwner hostile.Owner

    /// Whether the engine carries this hostile across an exit it ends a tick
    /// on: a player's creep, never an Invader's or a Source Keeper's
    /// (`creeps/tick.js` skips users '2' and '3').
    let crossesExits (hostile: HostileInfo) : bool =
        hostile.Owner <> "Invader" && not (isKeeper hostile)

    /// The range a list of parts can hurt a creep from, or None for one that
    /// cannot: paper or live as the caller's list is.
    let weaponRangeOf (body: BodyPart list) : int option =
        let mutable melee = false
        let mutable ranged = false

        for part in body do
            match part with
            | Attack -> melee <- true
            | RangedAttack -> ranged <- true
            | _ -> ()

        match melee, ranged with
        | false, false -> None
        | true, false -> Some Engine.meleeRange
        | false, true -> Some Engine.rangedRange
        | true, true -> Some(max Engine.meleeRange Engine.rangedRange)

    /// Whether a list of parts can take down a structure or a controller:
    /// WORK or CLAIM.
    let breaksOf (parts: BodyPart list) : bool =
        parts
        |> List.exists (fun part ->
            match part with
            | Work
            | BodyPart.Claim -> true
            | _ -> false)

    /// Whether every part of a hostile still acts: what it is in effect
    /// whatever reaches it, the common case asked first.
    let private whole (hostile: HostileInfo) : bool =
        hostile.Hits > Engine.partHits * (List.length hostile.Body - 1)

    /// Its HEAL still acting, each at its boost's multiple.
    let private liveHeals (hostile: HostileInfo) : int =
        Engine.powerAt hostile.Body hostile.Boosts hostile.Hits Heal

    /// The hits a hostile is short of whole.
    let private missing (hostile: HostileInfo) : int =
        Engine.partHits * List.length hostile.Body - hostile.Hits

    /// Whom one healer among `side` heals a tick: a heal acts on one target, so
    /// its side's budget is spent once — on the body of its owner's within
    /// ranged heal, itself included, short of the most hits (the least id
    /// on a tie). None while every one is whole.
    let healTargetOf (side: HostileInfo list) (healer: HostileInfo) : string option =
        // One pass for the least (-missing, id), the first of a tie kept: a
        // stable sort's head.
        let mutable best: HostileInfo option = None
        let mutable most = 0

        for ally in side do
            if ally.Owner = healer.Owner then
                let short = missing ally

                if
                    short > 0
                    && (ally.Id = healer.Id
                        || (match RoomPos.range healer.Pos ally.Pos with
                            | Some r -> r <= Engine.rangedRange
                            | None -> false))
                then
                    match best with
                    | Some kept when short < most || short = most && compare ally.Id kept.Id >= 0 ->
                        ()
                    | _ ->
                        best <- Some ally
                        most <- short

        best |> Option.map (fun ally -> ally.Id)

    /// Every healer among `side` under the id it heals (`healTargetOf`),
    /// beside its live HEAL, and every id the side holds a key, healed or not.
    let private healersBy
        (side: HostileInfo list)
        : System.Collections.Generic.Dictionary<string, (HostileInfo * int) list> =
        let healers =
            System.Collections.Generic.Dictionary<string, (HostileInfo * int) list>()

        for hostile in side do
            healers.[hostile.Id] <- []

        for healer in side do
            let heals = liveHeals healer

            if heals > 0 then
                match healTargetOf side healer with
                | Some id -> healers.[id] <- (healer, heals) :: healers.[id]
                | None -> ()

        healers

    /// The sides `healersBy` read this tick, latest first: a list never
    /// changes, so the same object is the same side. A hostile's every
    /// part-question reads its side's, once a side and not once a question.
    /// Bounded, and emptied each tick (`forgetSides`).
    let mutable private sides
        : (HostileInfo list *
          System.Collections.Generic.Dictionary<string, (HostileInfo * int) list>) list =
        []

    /// A tick's sides, of every colony and question, kept at most.
    let private sidesKept = 8

    /// Empties the sides read (`sides`): the shell's, once a tick, so none
    /// outlives it.
    let forgetSides () = sides <- []

    let private healersOf (side: HostileInfo list) =
        match sides |> List.tryFind (fun (read, _) -> obj.ReferenceEquals(read, side)) with
        | Some(_, healers) -> healers
        | None ->
            let healers = healersBy side
            sides <- (side, healers) :: List.truncate (sidesKept - 1) sides
            healers

    /// The hits a tick the healing reaching a hostile among `side` restores:
    /// every live HEAL whose one target it is (`healTargetOf`), its own or its
    /// owner's within ranged heal, at the heal's power beside it and the
    /// ranged heal's beyond.
    let private healedATick (side: HostileInfo list) (hostile: HostileInfo) : int =
        let healers =
            match (healersOf side).TryGetValue hostile.Id with
            | true, healers -> healers
            | _ -> (healersBy (hostile :: side)).[hostile.Id]

        healers
        |> List.sumBy (fun (healer, heals) ->
            if healer.Owner <> hostile.Owner then
                0
            else
                match RoomPos.range healer.Pos hostile.Pos with
                | Some r when r <= Engine.meleeRange -> Engine.healPower * heals
                | _ -> Engine.rangedHealPower * heals)

    /// Its hits `window` ticks on, its healing restoring them meanwhile
    /// (`healedATick`) up to the whole body.
    let private hitsWithin (window: int) (side: HostileInfo list) (hostile: HostileInfo) : int =
        min
            (Engine.partHits * List.length hostile.Body)
            (hostile.Hits + window * healedATick side hostile)

    /// The parts a hostile among `side` fights with `window` ticks on: its
    /// live parts and the ones its healing restores meanwhile, the engine's
    /// back-to-front order (`Engine.liveParts`). A dying healer barely
    /// restores; a whole one brings back the paper.
    let partsWithin (window: int) (side: HostileInfo list) (hostile: HostileInfo) : BodyPart list =
        if whole hostile then
            hostile.Body
        else
            Engine.liveParts hostile.Body (hitsWithin window side hostile)

    /// The parts a hostile fights with among `side`: `partsWithin` the
    /// window our answering body needs to close and strike (`Reaction.strike`).
    /// Every part-question below reads it, and so at that window; a lend's
    /// quiet reads `partsWithin Reaction.lend` (`Facts.homeHoldReleases`). Its hits stay
    /// what it has: a reader pricing an exchange prices the healing as a rate.
    let effectiveParts (side: HostileInfo list) (hostile: HostileInfo) : BodyPart list =
        partsWithin Reaction.strike side hostile

    /// `partsWithin` counted at each part's boost (`Engine.powerAt`): what a
    /// reader pricing what it deals, dismantles or walks multiplies by a
    /// part's power. A T3 RANGED_ATTACK counts as four.
    let powerWithin
        (window: int)
        (side: HostileInfo list)
        (hostile: HostileInfo)
        : BodyPart -> int =
        if List.isEmpty hostile.Boosts then
            partCountIn (partsWithin window side hostile)
        else
            let hits =
                if whole hostile then
                    hostile.Hits
                else
                    hitsWithin window side hostile

            Engine.powerAt hostile.Body hostile.Boosts hits

    /// `powerWithin` the parts it fights with (`effectiveParts`).
    let effectivePower (side: HostileInfo list) (hostile: HostileInfo) : BodyPart -> int =
        powerWithin Reaction.strike side hostile

    /// The hits a strike of `damage` takes off it now, through its boosted
    /// TOUGH (`Engine.damageTaken`).
    let damageTaken (hostile: HostileInfo) (damage: int) : int =
        Engine.damageTaken hostile.Body hostile.Boosts hostile.Hits damage

    /// The damage a strike must deal to take `hits` off it now, through its
    /// boosted TOUGH (`Engine.damageFor`): what a heal of `hits` on it costs
    /// our damage to undo.
    let damageFor (hostile: HostileInfo) (hits: int) : int =
        Engine.damageFor hostile.Body hostile.Boosts hostile.Hits hits

    /// The damage that kills it from the hits it has (`damageFor`).
    let effectiveHits (hostile: HostileInfo) : int = damageFor hostile hostile.Hits

    /// The hits it holds in its boosted TOUGH still standing
    /// (`Engine.toughHits`): the share of `Hits` a strike pays more for.
    let toughHits (hostile: HostileInfo) : int =
        Engine.toughHits hostile.Body hostile.Boosts hostile.Hits

    /// Whether `damage t` a tick from tick 1 to `ticks`, against `heal` a tick
    /// put back, kills it, tick by tick as the engine settles it: the damage
    /// through its boosted TOUGH (`Engine.damageTaken`), so the TOUGH is paid
    /// for once as it breaks and again only as far as the heal restores it,
    /// the heal capped at the whole body.
    let killedWithin (hostile: HostileInfo) (damage: int -> int) (heal: int) (ticks: int) : bool =
        let full = Engine.partHits * List.length hostile.Body

        let rec settle tick hits =
            if tick > ticks then
                false
            else
                let left =
                    hits - Engine.damageTaken hostile.Body hostile.Boosts hits (damage tick) + heal

                left <= 0 || settle (tick + 1) (min full left)

        settle 1 hostile.Hits

    /// How many of one part it fights with (`effectiveParts`).
    let private count (side: HostileInfo list) (hostile: HostileInfo) (part: BodyPart) : int =
        partCountIn (effectiveParts side hostile) part

    /// `weaponRangeOf` the parts it fights with.
    let weaponRange (side: HostileInfo list) (hostile: HostileInfo) : int option =
        weaponRangeOf (effectiveParts side hostile)

    /// ADR-0033. Whether it fights with an ATTACK or RANGED_ATTACK: a
    /// [[threat]] to a creep (`weaponRange` asked as a yes/no). A WORK body
    /// is not armed — it hurts no creep — but it razes what we built: what
    /// stands in a room of ours is `endangers`'.
    let armed (side: HostileInfo list) (hostile: HostileInfo) : bool =
        weaponRange side hostile |> Option.isSome

    /// Whether it raids: `armed`, and not a Source Keeper.
    let raider (side: HostileInfo list) (hostile: HostileInfo) : bool =
        armed side hostile && not (isKeeper hostile)

    /// Whether it fights with an ATTACK: a melee body, what a step and a
    /// swing is kept from.
    let melee (side: HostileInfo list) (hostile: HostileInfo) : bool = count side hostile Attack > 0

    /// Whether it fights with a RANGED_ATTACK.
    let shoots (side: HostileInfo list) (hostile: HostileInfo) : bool =
        count side hostile RangedAttack > 0

    /// Whether it works with a WORK.
    let works (side: HostileInfo list) (hostile: HostileInfo) : bool = count side hostile Work > 0

    /// Whether it works with a CLAIM.
    let claims (side: HostileInfo list) (hostile: HostileInfo) : bool =
        count side hostile BodyPart.Claim > 0

    /// Whether it works with a WORK or a CLAIM (`breaksOf`).
    let breaks (side: HostileInfo list) (hostile: HostileInfo) : bool =
        breaksOf (effectiveParts side hostile)

    /// Whether a part it fights or works with damages a creep or a structure:
    /// ATTACK, RANGED_ATTACK or WORK. A scout, a claimer, a hauler and a body
    /// whose weapons are shot off with no heal reaching it have none.
    let hurts (side: HostileInfo list) (hostile: HostileInfo) : bool =
        effectiveParts side hostile
        |> List.exists (fun part ->
            match part with
            | Attack
            | RangedAttack
            | Work -> true
            | _ -> false)

    /// Whether it endangers a room of ours it stands in, whatever it is
    /// doing there (#429): armed, or working with a WORK for an owner we
    /// strike anywhere (`Colony.strikesAnywhere`: an enemy's, an Invader's).
    /// A neutral's WORK body endangers only while it dismantles ours, which
    /// the body alone does not say (`Facts.endangersOurs`).
    let endangers (side: HostileInfo list) (hostile: HostileInfo) : bool =
        armed side hostile || Colony.strikesAnywhere hostile.Owner && works side hostile

    /// What it heals a tick, its live HEAL at their boosts: a rate, read now,
    /// as the window's restoring is.
    let healing (_side: HostileInfo list) (hostile: HostileInfo) : int =
        Engine.healPower * liveHeals hostile

    /// Whether it heals with a HEAL: the live parts `healing` reads.
    let heals (_side: HostileInfo list) (hostile: HostileInfo) : bool = liveHeals hostile > 0

    /// A hostile's gait `window` ticks on: its weight on paper, every part
    /// but MOVE weighing, destroyed ones too, against the MOVE it walks with
    /// then at their boosts (`powerWithin`): a T3 MOVE pays off eight fatigue
    /// a tick.
    let gaitWithin (window: int) (side: HostileInfo list) (hostile: HostileInfo) : Gait =
        {
            Weight = Gait.weightOf (partCountIn hostile.Body)
            Moves = powerWithin window side hostile Move
        }

    /// A hostile's gait (#510) with the parts it fights with (`gaitWithin`).
    let gait (side: HostileInfo list) (hostile: HostileInfo) : Gait =
        gaitWithin Reaction.strike side hostile

    /// Whether a hostile is as good as rooted (`Gait.stationary`).
    let stationary (side: HostileInfo list) (hostile: HostileInfo) : bool =
        Gait.stationary (gait side hostile)

/// An NPC invader core standing in a room the colony works this tick. A
/// **structure**, not a creep, so it reaches the projection through neither
/// `Hostiles` nor the fire reflex, whose sweep is `FIND_HOSTILE_CREEPS`. It is
/// what a [[stronghold]]'s room is stood down from; a level-0 core — 100,000
/// hits, no creeps, no towers — opens nothing and is a guard's to kill (#487).
type InvaderCoreInfo =
    {
        /// The engine's id: what a guard's swing names.
        Id: string
        /// The room it stands in: the colony works rooms, not tiles.
        RoomName: string
        /// Where in that room: the tile a guard stands beside.
        Tile: Pos
        /// The **absolute** tick the core's collapse timer runs out at, or None
        /// where it carries none, as a stronghold still deploying does.
        CollapseTick: int option
        /// The core's level, and what tells a **stronghold** from the level-0
        /// expansion core beside it (#382): a level-1-and-up core is a bunker
        /// nothing of ours crosses alive, a level-0 core a body walks past.
        /// Live W15S26 carried a `bunker4` and W15S27 the level-0 core it
        /// expanded into, both on the same collapse clock, so the clock cannot
        /// tell them apart and this can.
        Level: int
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module InvaderCoreInfo =
    /// Whether the core is a [[stronghold]]'s (level 1 and up), which the
    /// stand-down answers, rather than a level-0 expansion core, which a
    /// guard does (#487).
    let isStronghold (core: InvaderCoreInfo) : bool = core.Level >= 1

/// Whose flag a visible sector Reactor carries. Unlike `Ownership`, the rival
/// case keeps the engine's username: operator attribution, not a decision's
/// ours-or-not question (#320).
[<RequireQualifiedAccess>]
type ReactorOwner =
    | Ours
    | Rival of username: string
    | Unowned

/// The changing facts read from one visible sector Reactor. A room without
/// vision carries no row, so no zero here can be mistaken for a stale sample.
type ReactorInfo =
    {
        Id: string
        Owner: ReactorOwner
        Thorium: int
        ContinuousWork: int
    }

/// Which deadline an [[outpost]]'s [[stand-down]] runs to — the provenance of
/// the tick, carried beside it because it cannot be recovered afterwards.
/// The first three are the fallback order for a threat, best first; the
/// fourth is not a threat at all (#165). It crosses the wire, so it is spelt
/// once in `standDownBasisName` and round-tripped by `Core.Tests`. ADR-0043
[<RequireQualifiedAccess>]
type StandDownBasis =
    /// The core's own `EFFECT_COLLAPSE_TIMER`, the first answer wherever it
    /// can be read. **Which clock the expiry came off, and nothing about the
    /// room** — whether a bunker stands in it is `OutpostEpisode.Stronghold`'s
    /// to say (#382): `sight` overwrites the basis whenever a later deadline
    /// arrives, so one field for both would send a body through a room whose
    /// raid outlived its core with four towers standing in it.
    | CollapseTimer
    /// The end of the reservation a core took with `attackController`. No
    /// longer read (a level-0 core opens no stand-down, #487, and a
    /// stronghold reserves nothing): kept so a row a past bundle wrote still
    /// decodes.
    | Reservation
    /// No collapse timer was readable (a stronghold still deploying), so the
    /// clock is the 2,500-tick stronghold expansion period. It errs long
    /// deliberately: the gate may be wrong only in the direction that costs an
    /// outpost's income.
    | Fallback
    /// The end of the [[reservation]] **another player** holds on the room
    /// (#165): a clock where ownership is a latch, because a hold that decays
    /// is reversible where ownership is not. No floor under it: a player's
    /// claimer that stops coming leaves nothing behind.
    | RivalReservation
    /// A raid of plain [[invader]] creeps, clocked off the longest life among
    /// them (#257; `HostileInfo.TicksToLive` says why that is a deadline).
    /// Read only once the colony has stopped fighting for the room: while a
    /// [[guard]] stands there, or the episode has casts left, the room is a
    /// fight and not a withdrawal.
    | InvaderRaid
    /// A squad the ranger cannot beat, last seen standing in a [[harassment
    /// room]] (#441): that sighting plus `Tuning.HarassClearTicks`, refreshed
    /// by every later one. The room is ours to walk back into, so a squad that
    /// crosses it once shuts it briefly and not for its whole life.
    | HarassSighting
    /// A raid every body of which is gone from the room (#507, #585): seen
    /// dead, or, a player's, seen in another room after its last sighting
    /// here, or not seen in the room on a tick vision answers for it. The
    /// clock ends on the tick the last one went. An Invader seen elsewhere is
    /// not gone, so a live NPC raid keeps its life.
    | RaidGone
    /// A raid of a player's bodies alone, cut to its last sighting plus its
    /// priced hold (`Facts.raidHoldSpan`, #585, `OutpostEpisode.PlayerSince`):
    /// a squad that walks off unseen is no evidence it stays, and a return
    /// shuts the room again on sight. The printed reason; the fact is the
    /// row's.
    | PlayerRaidHold

/// Where a body's energy load was drawn from, and the tick it drew there
/// (#530).
type DrawRecord = { Store: string; Tick: int }

/// What the decision layer knows about one owned creep this tick.
type CreepInfo =
    {
        Name: string
        /// Ticks the creep still has to live; a creep still spawning is
        /// outside the projection, so this is always a real count. The fact,
        /// not the judgement of whether it is expiring.
        TicksToLive: int
        /// Fatigue points still to pay off; a creep with any cannot step
        /// this tick — the engine's move answers ERR_TIRED.
        Fatigue: int
        /// Current and maximum life; self-healing reads damage independently
        /// of the active body counts (partially damaged parts still work).
        Hits: HitsInfo
        /// Energy currently carried.
        Energy: int
        /// Thorium currently carried: a **second field beside `Energy`** and
        /// never a resource key inside it, for the reason `SpatialInfo.Thorium`
        /// gives. A decision reads it because **a body carries one resource
        /// at a time**: the Thorium [[withdraw]] wants an *empty* store, the
        /// Thorium [[refill]] a body holding some, and the energy intakes are
        /// shut to a body holding any. Not derived: `FreeCapacity` is the
        /// whole store's, so `capacity - free - energy` holds only where the
        /// three agree, and a hand-built fixture is free to state a body no
        /// engine would hand back.
        Thorium: int
        /// The minerals currently carried (the [[ore]] and the [[compound]]s),
        /// by resource, and empty for a body holding none — beside `Thorium`
        /// for its reason: a body carries one resource at a time, and a body
        /// holding a mineral takes no other intake.
        Minerals: Map<Resource, int>
        /// Carry capacity still free (0 = full) — the **whole** store's: a
        /// body holding 999 Thorium of 1,200 reports 201 free and no energy.
        FreeCapacity: int
        /// Active part counts (body entries with hits > 0). Absent or zero
        /// means the creep has no usable part of that kind.
        Body: Map<BodyPart, int>
        /// The parts shot off that the engine still weighs (`movement.js`:
        /// every part but MOVE and CARRY, hits or none). A MOVE shot off only
        /// stops paying its two a tick. 0 in every hand-built fixture that
        /// does not say otherwise.
        ShotOff: int
        /// Whether the body is on the move (#649): it stands on a different
        /// tile from last tick (Memory's last positions), or the last decide
        /// tracked a quarry for it and it stands off the tile it stood on
        /// then (`LightTick.walkers`), a light tick that stepped it nowhere
        /// (a chaser's kept step) not stopping it. False for a body born this
        /// tick and in every hand-built fixture that does not say otherwise.
        /// The one reader is who leaves a tile this tick
        /// (`Entry.leavingTiles`).
        Walking: bool
        /// The store the energy aboard was drawn from, and when (#530): the
        /// shell's reading of Memory's draw record, None in every hand-built
        /// fixture that does not say otherwise. The one reader is the Refill
        /// gate, so a load does not go straight back where it came from.
        DrawnFrom: DrawRecord option
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module CreepInfo =
    /// Every unit of mineral aboard, whatever the ore or compound.
    let mineralsAboard (creep: CreepInfo) : int =
        creep.Minerals |> Map.fold (fun total _ held -> total + held) 0

    /// What the body holds of one resource.
    let holding (resource: Resource) (creep: CreepInfo) : int =
        match resource with
        | Energy -> creep.Energy
        | Thorium -> creep.Thorium
        | Ore _
        | Compound _ -> Map.tryFind resource creep.Minerals |> Option.defaultValue 0

    /// Whether the body carries anything but energy: Thorium or a mineral.
    /// What shuts every energy intake, a mixed load being one no sink takes
    /// whole.
    let carriesMineral (creep: CreepInfo) : bool =
        creep.Thorium > 0 || mineralsAboard creep > 0

/// The names of the rows the spawn cascade casts, which it names every body
/// by (`{row}-{tick}-{spawn}`): `Bodies`' patterns are named from these, and a
/// reader before them (the Atlas's heavy set) reads a body's row off its name.
[<RequireQualifiedAccess>]
module RowName =
    let worker = "worker"
    let anchor = "anchor"
    let hauler = "hauler"
    let courier = "courier"
    let reserver = "reserver"
    let upgrader = "upgrader"
    let guard = "guard"
    let ranger = "ranger"
    let defender = "defender"
    let sapper = "sapper"
    let probe = "probe"
    let medic = "medic"
    let miner = "miner"
    let dismantler = "dismantler"

    let private all =
        [
            worker
            anchor
            hauler
            courier
            reserver
            upgrader
            guard
            ranger
            defender
            sapper
            probe
            medic
            miner
            dismantler
        ]

    /// Whether a name says its body was cast by this row.
    let names (row: string) (name: string) = name.StartsWith(row + "-")

    /// The row a body was cast for, off its name: fixed at the cast for its
    /// whole life, whatever damage does to its standing parts (#664). None
    /// for a name the cascade did not give.
    let ofName (name: string) : string option =
        all |> List.tryFind (fun row -> names row name)

    /// Whether a body was cast heavy-Work (Work > Move): its name's row is the
    /// anchor's or the miner's, the two rows that cast one; a body with no
    /// row's name answers off its standing parts.
    let castHeavy (creep: CreepInfo) : bool =
        match ofName creep.Name with
        | Some row -> row = anchor || row = miner
        | None -> partCount creep.Body Work > partCount creep.Body Move

/// A creep already paid for and still in a spawn's oven. Its generated name is
/// retained because two rows may buy the same part counts (#319).
type CastingInfo = { Name: string; Body: BodyPart list }

/// What one room holds this tick, to everybody: filed under the room's own
/// name and saying nothing about who is looking at it. Every list here is
/// *this room's*: the shell scopes what the engine does not. Absence stays
/// per entry and reaches down two levels — a room the world holds nothing
/// for reads `RoomFacts.empty`, and one it holds terrain for but has no
/// vision in reads that terrain beside empty everything else.
type RoomFacts =
    {
        /// This room's geometry: terrain, the targets standing on it, and
        /// **every** creep of ours in the room, not one colony's.
        Layer: RoomLayer
        /// The room's border ring, the Seam's terrain and never ground.
        Border: Map<Pos, Terrain>
        /// The kind of each target standing in this room, under the engine's
        /// own id — unique across the world, so the layer that holds it *is*
        /// the room it stands in.
        TargetKinds: Map<string, TargetKind>
        /// Current/max hits of the repairable kinds standing here.
        Hits: Map<string, HitsInfo>
        /// Energy currently stored, per store standing here: containers, the
        /// Storage, piles, tombstones and ruins.
        Stores: Map<string, int>
        /// Thorium currently held, per store standing here, and the remaining
        /// amount of the Thorium mineral itself. A second map beside `Stores`
        /// for the reason `SpatialInfo.Thorium` gives.
        Thorium: Map<string, int>
        /// The minerals held per store standing here, and an ore deposit's own
        /// remaining amount (`SpatialInfo.Minerals`).
        Minerals: Map<string, Map<Resource, int>>
        /// Ticks before a structure standing here may act again — the
        /// extractor's cooldown, the one clock a decision reads off a
        /// structure.
        Cooldowns: Map<string, int>
        /// Our ramparts standing here that are public (`isPublic`, #482): any
        /// creep may step onto one. Absent is private.
        PublicRamparts: Set<string>
        /// Whose each **object** standing here is (#318) — the per-object twin
        /// of `Control`'s room ownership, and the fact the sector Reactor's
        /// room has no controller to answer with. Filled for the reactors
        /// alone today.
        Owners: Map<string, Ownership>
        /// The sector Reactors visible in this room, with the richer facts
        /// used only by the global Reactor observation channel (#320).
        Reactors: ReactorInfo list
        /// Who holds the room, and whether its safe mode is running — `None`
        /// for a room nothing looked into this tick, which is not the same
        /// fact as a room nobody holds.
        Control: RoomControlInfo option
        /// The controller of this room **while it is ours**. `None` for a
        /// room we do not own, whose ownership and reservation are
        /// `Control`'s.
        Controller: ControllerInfo option
        /// The controller of this room **while another player owns it**, as
        /// this tick shows it (#489); `World.recallRivalControllers` keeps it
        /// past the tick.
        RivalController: RivalController option
        /// The room's shared spawn-energy account. Zero for a room with no
        /// spawn or extension in it, which a colony reads as an empty bank.
        Energy: RoomEnergy
        /// Our spawns standing in this room. The world's, so a spawn standing
        /// in a [[nursery]] is a fact about that room and not about its
        /// mother — which is exactly what ends the nursery.
        Spawns: SpawnInfo list
        /// The names and bodies still gestating in this room's spawns: energy
        /// the colony has **already spent**. Filed under the room because a
        /// colony banks in one room and every row's gap is a colony number.
        Casting: CastingInfo list
        /// Our energy-hungry structures standing here (spawn, extension,
        /// tower), whether or not they currently have room.
        Refillables: RefillableInfo list
        /// The sources in this room, as vision answered for them. What a
        /// *declaration* answers for is laid over a colony's projection
        /// instead (`Outpost.place`, `Outpost.pooledSources`).
        Sources: SourceInfo list
        /// Our construction sites standing here (#150).
        ConstructionSites: ConstructionSiteInfo list
        /// Hostile creeps standing here this tick.
        Hostiles: HostileInfo list
        /// Each of those hostiles' fatigue by id, where it has any.
        HostileFatigue: Map<string, int>
        /// What each of those hostiles carries by id, every resource
        /// counted, where it carries any (#621: a kill's cargo).
        HostileCargo: Map<string, int>
        /// The hits of each standing rampart here that is not ours, by tile
        /// (#651): what a body standing on one takes before the body itself.
        /// `Hits` carries ours alone.
        RivalRamparts: Map<Pos, int>
        /// The invader cores standing here.
        InvaderCores: InvaderCoreInfo list
        /// The ids of the creeps not ours whose tombstones lie here.
        Fallen: string list
        /// The tombstones here of an [[ally]]'s creeps, by tombstone id: an
        /// ally's loot, never ours to take.
        AllyGraves: Set<string>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RoomFacts =
    /// A room the world holds nothing for — every entry absent, which is
    /// what a room outside the scan set and a room with no vision both
    /// read as.
    let empty: RoomFacts =
        {
            Layer = RoomLayer.empty
            Border = Map.empty
            TargetKinds = Map.empty
            Hits = Map.empty
            Stores = Map.empty
            Thorium = Map.empty
            Minerals = Map.empty
            Cooldowns = Map.empty
            PublicRamparts = Set.empty
            Owners = Map.empty
            Reactors = []
            Control = None
            Controller = None
            RivalController = None
            Energy = { Available = 0; Capacity = 0 }
            Spawns = []
            Casting = []
            Refillables = []
            Sources = []
            ConstructionSites = []
            Hostiles = []
            HostileFatigue = Map.empty
            HostileCargo = Map.empty
            RivalRamparts = Map.empty
            InvaderCores = []
            Fallen = []
            AllyGraves = Set.empty
        }

/// One creep of ours, in the world's reading, before any colony has claimed
/// it.
type WorldCreep =
    {
        /// The room the engine says the creep stands in
        /// (`World.creepColonies` decides whose it then is).
        Room: string
        /// What the decision layer knows about the body itself.
        Info: CreepInfo
    }

/// What the world last saw standing in one room, and when (#151): the one
/// thing carried **across** ticks about a room. `Targets` answers one
/// question only, and `Rival` one other. Per-entry absence cannot say *why* an id left the pool, and the two
/// answers are opposite work: a container destroyed is a Task gone, a room
/// gone dark is a Task waiting. So `Targets` is read by the vision grace and
/// by nothing else, and what the grace hands on is a **room name**: the Matcher
/// keeps the assignment and the mover walks its holder at that room's Seam.
/// Nothing is placed or priced off a sighting. Which of these reach a colony
/// is the **view**'s cut (`ColonyView.ofWorld`, #271).
type RoomSighting =
    {
        /// The tick vision last answered for the room. Equal to the world's
        /// own `Time` for a room seen this tick.
        Tick: int
        /// The ids that stood in the room that tick, and nothing about them:
        /// set membership is the only question the grace asks, and there is
        /// no stale kind here for the next rule to read one out of.
        ///
        /// Deferred, because the tick a room is **seen** nobody reads this:
        /// both readers ask only about a sighting older than the tick they
        /// run in (`Facts.errandRoomOf`, and the errand narrowing in
        /// `ColonyView.ofWorld`). `Lazy` keeps it built once the room goes
        /// dark, so the world stops paying a set of several hundred ids a
        /// room a tick for an answer nobody wants yet (#371). The closure
        /// captures the projection's own census, built this tick anyway.
        Targets: Lazy<Set<string>>
        /// Who owned the room's controller that tick, when it was another
        /// player and not us; None for a room nobody owns, ours, or one
        /// with no controller. The second thing a sighting is read for
        /// (#444): no chain enters a room a rival owns (`World.rivalHeld`),
        /// and a room nothing of ours stands in has no vision to ask.
        Rival: string option
    }

/// The exit a room's last armed Threat left by, and how long its Guard holds
/// it (#450): attacks do not cross a border, so a body re-entering lands on
/// the edge tile beside a guard standing there.
type ExitHold =
    {
        /// The walkable exit tiles of one side, contiguous, through the one
        /// nearest where the Threat was last seen.
        Run: Pos list
        /// The one exit tile of `Run` it stepped out by (#510): a ranged body
        /// keeps three from it, beyond the step and swing of a body camping
        /// across.
        Exit: Pos
        /// The first tick the hold no longer stands: `Tuning.ExitHoldTicks`
        /// after it left, or the tick its remembered `ticksToLive` runs out if
        /// sooner.
        Until: int
    }

[<RequireQualifiedAccess>]
module ExitHold =
    /// Whether the hold still stands at `time`.
    let stands (time: int) (hold: ExitHold) = hold.Until > time

/// One armed Threat as the full tick saw it: which creep, where it stood, and
/// the tick its `ticksToLive` runs out. `Id` is the creep's id, the one its
/// tombstone names (`RoomFacts.Fallen`); empty for a row a bundle before #561
/// wrote, which no tombstone matches.
type SeenThreat = { Id: string; At: Pos; Dies: int }

/// What the world remembers of one room's armed Threats across ticks
/// (`World.watchExits`, #450). Heap state only: a reset costs one hold.
[<RequireQualifiedAccess>]
type ExitWatch =
    /// Armed Threats stood in the room on the last full tick, which saw it.
    | Seen of SeenThreat list
    /// They are gone, and the room's Guard holds the exit they left by.
    | Held of ExitHold

/// A raid healer as last seen in a room, and the tick it was seen
/// (`World.recallHealers`, #480).
type HealerSeen = { Healer: HostileInfo; Tick: int }

/// A room's raid as last seen there, and the tick it was seen
/// (`World.recallRaiders`, #569): every hostile standing in it beside a
/// raider, at its tile, its hits and its life as read then.
type RaidSeen = { Raid: HostileInfo list; Tick: int }

/// Where a hostile stands, and the tile of its room it stood on the full tick
/// before when it has stepped off it since (`Quarry.step`, #551),
/// and the first full tick it was seen on `At` (#552): one seen there on two
/// is parked. `Back`: the tile of that room it left on the step before
/// `Left`, which tells a body walking off from one milling in place.
/// `Seen`: the last full tick it was seen. `Crossed`: its trajectory across
/// a border within the bounce window (#658), a body that walks a few steps
/// out and back counted as surely as one bouncing on the exit tile, the
/// record kept through the window while it is out of sight.
type HostileStep =
    {
        At: RoomPos
        Left: RoomPos option
        Back: RoomPos option
        Since: int
        Seen: int
        Crossed: Crossing option
    }

/// A hostile's crossings of a border (#658): the tile it was last seen on
/// before its latest crossing, in the room it left, and the full ticks it
/// was seen in another room than the one it was last seen in, newest first;
/// and the tiles it crossed between, each crossing's tile it came onto and
/// the tile it came from, newest first: the tiles it steps in and out on.
and Crossing =
    {
        Came: RoomPos
        Ticks: int list
        Tiles: RoomPos list
    }

/// A body's dance on a border (#585, #658): how deep into a room a body
/// that steps out and back turns at, read off the reach it fights from.
[<RequireQualifiedAccess>]
module Dance =
    /// The tiles in from a border a dance turns back within: a ranged
    /// body's reach and the step to keep it. The one border depth (#663).
    let depth = Engine.rangedRange + 1

    /// A tile's steps to its room's nearest edge: the one measure every
    /// border depth is read in (#663).
    let toEdge (tile: Pos) : int =
        let edge = Engine.roomSide - 1
        min (min tile.X tile.Y) (min (edge - tile.X) (edge - tile.Y))

    /// Whether a tile stands within the dance depth of any border of its
    /// room.
    let byBorder (tile: RoomPos) : bool = toEdge (RoomPos.pos tile) <= depth

    /// The rooms across the borders a tile stands within the dance depth of:
    /// where a body last seen on it may have stepped out to.
    let beyond (tile: RoomPos) : string list =
        let edge = Engine.roomSide - 1

        RoomName.adjacent tile.Room
        |> List.filter (fun next ->
            match RoomName.offsetOf tile.Room next with
            | Some(-1, 0) -> tile.X <= depth
            | Some(1, 0) -> tile.X >= edge - depth
            | Some(0, -1) -> tile.Y <= depth
            | Some(0, 1) -> tile.Y >= edge - depth
            | _ -> false)

    /// Whether a body standing outside `room` lingers on its border: in a
    /// room beside it, within the dance depth of the border they share.
    let besideOf (room: string) (tile: RoomPos) : bool =
        tile.Room <> room && List.contains room (beyond tile)

/// How a [[quarry]] out of sight is known gone (#663): its tombstone was
/// seen, its life ran out, or a look at the room it was last seen in found
/// it standing nowhere in sight with no trip of it across a border left.
[<RequireQualifiedAccess>]
type Gone =
    | Fallen
    | Aged
    | SeenGone

/// One foreign body as the world remembers it, by id (`World.Quarries`,
/// #663): the one record every memory of a hostile reads its per-body facts
/// off. Folded and read by the `Quarry` module; heap state only.
type Track =
    {
        /// The body as last read: its tile, parts, hits and life then.
        Body: HostileInfo
        /// The last full tick it was seen.
        Seen: int
        /// What it carried then (`RoomFacts.HostileCargo`): the load its
        /// walk home is weighed by (`Quarry.roundTrip`).
        Cargo: int
        /// Its steps, newest first: each tile it stood on, by the first and
        /// the last full tick of each run of sightings on it (one entry for
        /// a run of one), kept over the bounce window and never fewer than
        /// the three newest: the cut may leave an older run only its last
        /// entry. Empty once it is out of sight and no trip of it
        /// is left to keep (`Quarry.recall`): seen again, it starts afresh.
        Trail: (int * RoomPos) list
        /// Its crossings of a border within the bounce window.
        Crossings: Crossing option
        /// None while it is in sight, or out of it and not known gone.
        Gone: Gone option
    }

/// `World.linkedRecalling`'s memo: `(keeper margin, from, to)` to whether a
/// crossing joins the pair. Mutable and heap-only, like `WalkTable`, and the
/// **shell's**: one table for the life of the process, because every answer
/// in it is the terrain's. A test lays a table of its own per call
/// (`linkedBy`, `scanOf`, `creepColonies`, `ColonyView.ofWorld`); what is
/// forbidden is a **static** holding one, shared across Expecto's parallel
/// lists (#310, `ParallelSafetyTests`).
///
/// Keyed by the three joined into one string (`margin|from|to`; a room name
/// holds no `|`) and not by the tuple: Fable hashes a string key into a
/// native JS `Map`, where a tuple key is hashed and compared field by field
/// on every read, and the scan set's chain search reads this per step.
///
/// Beside it, the hop counts the scan set and the harassment casting ask of
/// the same relation (`World.hopsUnder`): a chain search per declaration per
/// colony, twice a tick, for an answer that moves only with the rooms the
/// world holds, the rooms withheld from passage and the rooms a rival owns,
/// all of which ride in its key. Who owns a room is never a `Joins` answer:
/// it is asked ahead of the table, as which rooms the world holds is.
type JoinTable() =
    member val Joins = System.Collections.Generic.Dictionary<string, bool>()
    member val Hops = System.Collections.Generic.Dictionary<string, int>()
    /// The ordered pairs filed.
    member this.Count = this.Joins.Count

/// The engine's refusal of an act it was told to make (#629): its answer,
/// read by the next decide, so no act is decided identically for ever with
/// a console line the only place the refusal lands. The shell folds the
/// Executor's outcomes into these, light ticks and full (`Refusal.record`),
/// one row per actor and the Task it held.
type Refusal =
    {
        /// The body refused, by name, or the structure: a spawn by name,
        /// a site by its tile, any other by id.
        Actor: string
        /// Whether the actor is a body of ours: cut into the view of the
        /// colony that holds it, where a structure is cut into the view of
        /// the colony that issued its act (`Home`).
        Body: bool
        /// The home of the colony whose plan issued a structure's act, a
        /// site in an outpost's room included; empty for a body, or an act
        /// no plan owns.
        Home: string
        /// The Task the body held when refused; empty for a structure, or a
        /// body holding none.
        Task: string
        /// The engine's code for the newest refusal, carried for the
        /// operator: no rule reads an engine code.
        Code: int
        /// The refusals, each counted one and halved every
        /// `Tuning.RefusalHalfLife` ticks since, as of `Last`.
        Weight: float
        /// The newest refusal's tick.
        Last: int
        /// The refusals since the actor's colony last decided: the news the
        /// next decide reads.
        Fresh: int
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Refusal =
    /// One refusal at `tick`, before any fold.
    let one (actor: string) (body: bool) (home: string) (task: string) (code: int) (tick: int) =
        {
            Actor = actor
            Body = body
            Home = home
            Task = task
            Code = code
            Weight = 1.0
            Last = tick
            Fresh = 1
        }

    /// The weight standing at `tick`, halved every `halfLife` ticks since
    /// the newest refusal.
    let weightAt (halfLife: int) (tick: int) (row: Refusal) : float =
        row.Weight * 2.0 ** (-(float (tick - row.Last)) / float (max 1 halfLife))

    /// The price a refusal puts on its (body, Task) pair at `tick`, in the
    /// travel cost's half-ticks: each refusal cost the body a tick its Task
    /// did not advance, which is what a tick of walk costs it.
    let costAt (halfLife: int) (tick: int) (row: Refusal) : int =
        int (System.Math.Round(2.0 * weightAt halfLife tick row))

    /// This tick's refusals (`one`) folded into the rows: a pair refused
    /// before adds one to the weight standing now and to its news, and a
    /// pair not refused stands as it was.
    let record
        (halfLife: int)
        (tick: int)
        (refused: Refusal list)
        (rows: Refusal list)
        : Refusal list =
        (rows, refused)
        ||> List.fold (fun rows fresh ->
            let pair (row: Refusal) =
                row.Actor = fresh.Actor && row.Task = fresh.Task

            match rows |> List.tryFind pair with
            | Some row ->
                { fresh with
                    Weight = weightAt halfLife tick row + fresh.Weight
                    Fresh = row.Fresh + fresh.Fresh
                }
                :: (rows |> List.filter (pair >> not))
            | None -> fresh :: rows)

    /// The rows past their price, news unread or not, and a body's once it
    /// is dead: a weight whose price rounds to no half-tick moves no
    /// decision, and is a refusal no decide is left to read (two half-lives
    /// and more since the last), so no row outlives its price.
    let forget
        (halfLife: int)
        (tick: int)
        (living: Set<string>)
        (rows: Refusal list)
        : Refusal list =
        rows
        |> List.filter (fun row ->
            costAt halfLife tick row > 0 && (not row.Body || Set.contains row.Actor living))

    /// The news read: each row `decided` says its colony decided on has
    /// none left, its weight standing.
    let settle (decided: Refusal -> bool) (rows: Refusal list) : Refusal list =
        rows
        |> List.map (fun row ->
            if row.Fresh > 0 && decided row then
                { row with Fresh = 0 }
            else
                row)

/// Everything this tick was seen to hold, once. The shell builds one
/// (`World.ofGame`, the only code that touches `Game`) and
/// `ColonyView.ofWorld` cuts one colony's share of it; `decide` is written
/// against a view and never against the world.
type World =
    {
        Time: int
        /// Every room the world holds anything for this tick: the declared
        /// rooms, whose terrain and furniture need no vision, and every room
        /// the engine answered `Game.rooms` with. A room absent here reads
        /// `RoomFacts.empty`.
        Rooms: Map<string, RoomFacts>
        /// How many construction sites the player holds across every room,
        /// dark ones included (`Game.constructionSites`): the engine's cap
        /// (`Engine.maxConstructionSites`) is the player's, not a room's.
        Sites: int
        /// Every creep we own that is not still gestating, in the engine's
        /// own order. Whose each one is this tick is `World.creepColonies`'
        /// answer: the rule needs the [[stand-down]] gate, read out of
        /// Memory.
        Creeps: WorldCreep list
        /// What each room was last seen to carry (#151): this tick's census
        /// for every room vision answered for, and the last one taken for a
        /// room it did not. Heap state in the shell, and only each rival
        /// room's owner is kept in Memory (`seedRivals`): a global reset
        /// empties the rest. A room never seen has no entry.
        Sightings: Map<string, RoomSighting>
        /// The rooms of ours a tower of our own has stood full in
        /// (`World.latchTowers`, #445): the latch that keeps a child out of
        /// `Weaning` while its towers fire. Heap state the shell also keeps
        /// in Memory, so a global reset does not forget it.
        Towered: Set<string>
        /// Each room's armed Threats as last seen, or the exit they left by
        /// (`World.watchExits`, #450). Heap state in the shell, never Memory.
        ExitWatches: Map<string, ExitWatch>
        /// Each room's raid healers seen there within the last
        /// `Tuning.HealMemoryTicks` ticks (`World.recallHealers`, #480): a
        /// healer bouncing across the exit heals on every other tick. Heap
        /// state in the shell, never Memory.
        Healers: Map<string, HealerSeen list>
        /// Every foreign body in sight, and each out of sight still kept, by
        /// id (`Quarry.recall`, #663). Heap state in the shell, never Memory.
        Quarries: Map<string, Track>
        /// Every hostile in sight by id, and the tile it left since the full
        /// tick before (#551): `Quarries`' steps (`Quarry.step`), laid by
        /// `Quarry.recall`.
        HostileSteps: Map<string, HostileStep>
        /// Each room's raid as last seen, while no sighting since has found
        /// it gone (`World.recallRaiders`, #569): what a blind tick reads the
        /// raid off. Heap state in the shell, never Memory.
        Raiders: Map<string, RaidSeen>
        /// The last tick an armed rival — not an ally, not a Source Keeper —
        /// stood in each room, kept for `Tuning.HostileRoomMemory` ticks
        /// (`World.recallArmed`, #485): the rooms a non-fighter's walk goes
        /// round. Heap state the shell also keeps in Memory, so the deploy
        /// before a claim does not forget the room it must walk round.
        ArmedSeen: Map<string, int>
        /// Every rival controller as last seen (`World.recallRivalControllers`,
        /// #489): what `RivalSafeMode.canActivate` reads. Heap state the shell
        /// also keeps in Memory, so a reset does not forget a safe mode it saw.
        RivalControllers: Map<string, RivalController>
        /// The ground of a room `Rooms` does not hold, asked by name (#570):
        /// terrain and border alone, read by a view for the rooms a stray
        /// body of its own may walk out over and never by a chain, so no
        /// declaration's routing reads it. The shell's terrain memo; None
        /// where nothing can be read.
        StrayGround: string -> RoomFacts option
        /// The engine's refusals of our acts, folded off every tick's
        /// Executor outcomes since (`Refusal.record`, #629). Heap state in
        /// the shell, never Memory: a reset forgets a refusal the next
        /// refused act teaches again.
        Refusals: Refusal list
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module World =
    /// A world holding nothing: no room, no creep.
    let empty: World =
        {
            Time = 0
            Rooms = Map.empty
            Sites = 0
            Creeps = []
            Sightings = Map.empty
            Towered = Set.empty
            ExitWatches = Map.empty
            Healers = Map.empty
            Quarries = Map.empty
            HostileSteps = Map.empty
            Raiders = Map.empty
            ArmedSeen = Map.empty
            RivalControllers = Map.empty
            StrayGround = fun _ -> None
            Refusals = []
        }

    /// This tick's world with what it saw **before** laid under it (#151):
    /// every room vision answered for keeps this tick's sighting, and a room
    /// it did not answer for keeps the last one taken. One pure function a
    /// test can drive. Bounded by the rooms this tick's world holds, so a room
    /// that leaves the world leaves the memory with it — but for the declared
    /// rivals' (#484): an observer's look at one is what its next look waits
    /// on, and six sightings are no growth.
    let recalling (previous: Map<string, RoomSighting>) (world: World) : World =
        let kept room =
            Map.containsKey room world.Rooms
            || Colony.rivals |> List.exists (fun (rival, _) -> rival = room)

        { world with
            Sightings =
                (previous |> Map.filter (fun room _ -> kept room), world.Sightings)
                ||> Map.fold (fun carried room sighting -> Map.add room sighting carried)
        }

    /// One room's facts: a room the world carries nothing for reads as every
    /// entry absent, never as a lookup that throws.
    let roomOf (world: World) (room: string) : RoomFacts =
        Map.tryFind room world.Rooms |> Option.defaultValue RoomFacts.empty

    /// A room's facts, or where `Rooms` holds none the ground a stray's walk
    /// out reads (`StrayGround`, #570); None where the world can read
    /// neither. Asked by a view's stranded rooms alone.
    let groundOf (world: World) (room: string) : RoomFacts option =
        match Map.tryFind room world.Rooms with
        | Some facts -> Some facts
        | None -> world.StrayGround room

    /// Whether safe mode is running in any room of ours this tick: one per
    /// shard, so while it runs no other room of ours can raise its own.
    let safeModeRunning (world: World) : bool =
        world.Rooms
        |> Map.exists (fun _ facts ->
            facts.Control
            |> Option.exists (fun control -> control.Owner = Ownership.Ours && control.SafeMode))

    /// How many towers of ours stand in the room this tick, loaded or dry.
    let towersStanding (facts: RoomFacts) : int =
        facts.Refillables
        |> List.filter (fun r -> r.Kind = BuiltKind.Tower)
        |> List.length

    /// The towers of ours in the room holding a shot's energy this tick.
    let private loaded (facts: RoomFacts) : RefillableInfo list =
        facts.Refillables
        |> List.filter (fun r ->
            r.Kind = BuiltKind.Tower
            && Engine.towerCapacity - r.FreeCapacity >= Engine.towerEnergyCost)

    /// How many towers of ours in the room hold a shot's energy this tick.
    let loadedTowers (facts: RoomFacts) : int = List.length (loaded facts)

    /// The tiles of the towers of ours in the room holding a shot over
    /// `reserve` this tick: what every fight priced there counts beside our
    /// bodies. Under the reserve `Layout.planFire` takes only an urgent shot.
    let firingTowerTiles (reserve: int) (facts: RoomFacts) : Pos list =
        loaded facts
        |> List.filter (fun r ->
            Engine.towerCapacity - r.FreeCapacity >= reserve + Engine.towerEnergyCost)
        |> List.choose (fun r -> Map.tryFind r.Id facts.Layer.TargetPositions)

    /// Whether a colony's home cannot hold the raid standing in it this tick:
    /// the room is ours, a hostile that is not an ally endangers it there
    /// (`HostileInfo.endangers`, an enemy's or an Invader's dismantler too:
    /// a neutral's is never seen dismantling off one tick's facts), safe
    /// mode is off, and the raid heals at least what the towers holding a
    /// shot's energy land at the falloff range, through the TOUGH of the
    /// body of the raid they land most on.
    let homeBeaten (facts: RoomFacts) : bool =
        let raid =
            facts.Hostiles |> List.filter (fun hostile -> not (Colony.isAlly hostile.Owner))

        let towers = loadedTowers facts
        let shot = towers * Engine.towerAttackAt Engine.towerFalloffRange

        facts.Control
        |> Option.exists (fun control -> control.Owner = Ownership.Ours && not control.SafeMode)
        && List.exists (HostileInfo.endangers facts.Hostiles) raid
        && List.sumBy (HostileInfo.healing facts.Hostiles) raid
           >= (raid |> List.map (fun h -> HostileInfo.damageTaken h shot) |> List.max)

    /// Every spawn's name beside the room it stands in: what `Colony.castBy`
    /// reads a creep's caster off.
    let spawnHomes (world: World) : (string * string) list =
        world.Rooms
        |> Map.toList
        |> List.collect (fun (name, facts) ->
            facts.Spawns |> List.map (fun spawn -> spawn.Name, name))

    /// Whether a guard `caster` cast stands in one of these rooms. The
    /// casters are read only once a guard body is found there.
    let private guardCastIn (world: World) (caster: string) (rooms: string list) : bool =
        let standing =
            world.Creeps
            |> List.filter (fun creep ->
                isGuardParts creep.Info.Body && List.contains creep.Room rooms)

        not (List.isEmpty standing)
        && (let homes = spawnHomes world

            standing
            |> List.exists (fun creep -> Colony.castBy homes creep.Info.Name = Some caster))

    /// Whether a colony may cast another's home defence (#589): a spawn of
    /// its own and a bank at `Tuning.CasterFloor` or above. Kinship plays
    /// no part. Which of them casts is the Caster's (`Caster.assign`).
    let private mayDefend (tuning: Tuning) (world: World) (defender: string) : bool =
        let ours = roomOf world defender

        not (List.isEmpty ours.Spawns) && ours.Energy.Capacity >= tuning.CasterFloor

    /// Whether a colony projects another's home for its defence this tick,
    /// as a [[transit room]] with the rooms between, the home inside the
    /// hop budget off the names (`Tuning.MaxHops`, `RoomName.hopsBetween`):
    /// while the home is beaten and the colony may defend it (`mayDefend`),
    /// and past that while a guard it cast still stands on the defended
    /// chain outside the rooms it projects anyway. The second half is the
    /// latch, read off the world and kept nowhere: a raid the towers hold
    /// again once its guard has come keeps the room its, so the guard is not
    /// handed to a colony with no Guard to give it; and a guard left on the
    /// chain when the raid ends stays placed and walks home. It lets go the
    /// tick the last one walks out.
    let defends
        (tuning: Tuning)
        (world: World)
        (covered: string list)
        (defender: string)
        (home: string)
        : bool =
        defender <> home
        && RoomName.hopsBetween defender home
           |> Option.exists (fun hops -> hops <= tuning.MaxHops)
        && (homeBeaten (roomOf world home) && mayDefend tuning world defender
            || home :: RoomName.transitBetween defender home
               |> List.filter (fun room -> not (List.contains room covered))
               |> guardCastIn world defender)

    /// The rooms we own this tick, off the control entry vision paid for: one
    /// of the two facts a colony has to pass to be **living**.
    let ownedRooms (world: World) : Set<string> =
        world.Rooms
        |> Map.toList
        |> List.filter (fun (_, facts) ->
            facts.Control |> Option.exists (fun control -> control.Owner = Ownership.Ours))
        |> List.map fst
        |> Set.ofList

    /// Whether a tower of ours in the room holds `Tuning.IndependenceTowerEnergy`
    /// this tick.
    let towerFull (tuning: Tuning) (facts: RoomFacts) : bool =
        facts.Refillables
        |> List.exists (fun r ->
            r.Kind = BuiltKind.Tower
            && Engine.towerCapacity - r.FreeCapacity >= tuning.IndependenceTowerEnergy)

    /// This tick's world with the tower latch laid under it (#445): last
    /// tick's `Towered` kept for the rooms still ours, and every room of ours
    /// whose tower stands full now added. A room that stops being ours drops
    /// out, so one claimed again is raised again. Built by `Fresh`, the set
    /// being carried to the next tick.
    let latchTowers (tuning: Tuning) (previous: Set<string>) (world: World) : World =
        let owned = ownedRooms world

        { world with
            Towered =
                owned
                |> Seq.filter (fun room ->
                    Set.contains room previous || towerFull tuning (roomOf world room))
                |> Fresh.setOfSeq
        }

    /// The exit a body standing on `From` could have stepped out by, and the
    /// run of its side's walkable exit tiles contiguous with it.
    type private ExitRun =
        {
            From: Pos
            Through: Pos
            Run: Pos list
        }

    /// The squared straight-line distance between two tiles: a step beside one
    /// is nearer than a diagonal, which range does not say.
    let private squaredDistance (a: Pos) (b: Pos) =
        let dx = b.X - a.X
        let dy = b.Y - a.Y
        dx * dx + dy * dy

    /// How far the body stood from the exit it took.
    let private distanceOf (run: ExitRun) = squaredDistance run.From run.Through

    let private walkableExit (border: Map<Pos, Terrain>) (p: Pos) =
        Seam.isExit p && (Map.tryFind p border |> Option.exists (fun t -> t <> Wall))

    /// The walkable exit tiles a step from `tile`: the engine carries a body
    /// across in the tick it steps onto one (`processor/intents/creeps/tick.js`
    /// moves, then sends a body at the edge on), so these are every way out a
    /// body last seen there had. A body seen on an exit tile has just arrived
    /// and, unless it steps off, is carried straight back across that tick.
    let private exitsBeside (border: Map<Pos, Terrain>) (tile: Pos) : Pos list =
        tilesWithin 1 tile |> List.filter (walkableExit border)

    /// The exit run a body standing on `tile` could have stepped out by,
    /// through the walkable exit tile nearest it within a step, on whichever
    /// side. None for a tile no exit is a step from — a Threat that vanished
    /// there died, it did not leave.
    let private exitRunFrom (border: Map<Pos, Terrain>) (tile: Pos) : ExitRun option =
        let walkable = walkableExit border

        exitsBeside border tile
        |> List.sortBy (fun exit -> squaredDistance tile exit, exit.X, exit.Y)
        |> List.tryHead
        |> Option.map (fun exit ->
            let vertical = exit.X = 0 || exit.X = Seam.exitEdge

            let step d =
                if vertical then
                    { exit with Y = exit.Y + d }
                else
                    { exit with X = exit.X + d }

            let along dir =
                Seq.initInfinite (fun i -> step (dir * (i + 1)))
                |> Seq.takeWhile walkable
                |> List.ofSeq

            {
                From = tile
                Through = exit
                Run = List.rev (along -1) @ [ exit ] @ along 1
            })

    /// Whether vision answered for a room this tick: its sighting is this
    /// tick's, not one recalled.
    let seenNow (world: World) (room: string) : bool =
        Map.tryFind room world.Sightings
        |> Option.exists (fun sighting -> sighting.Tick = world.Time)

    /// This tick's world with the exit watch laid under it (#450): a room
    /// vision finds an armed Threat in is `Seen`; the next full tick vision
    /// finds them gone, the run the nearest of them to an exit left by is
    /// `Held` until `Tuning.ExitHoldTicks` or that one's remembered death. A
    /// room out of sight keeps a hold until its clock runs out, and forgets a
    /// sighting: where a Threat went in the dark is not known. Bounded by this
    /// tick's rooms, as `recalling` is, and built by `Fresh`, the map being
    /// carried to the next tick.
    let watchExits (tuning: Tuning) (previous: Map<string, ExitWatch>) (world: World) : World =
        let seen = seenNow world

        // Only a Threat that left holds the run, never one that died (#561):
        // its tombstone lies here, or every room it could have crossed into
        // is seen this tick and it stands in none. A row with no id (written
        // before #561) is matched by neither. Accepted staleness: the watch
        // compares full ticks, so a body that crossed and walked on out of the
        // seen neighbour during the light ticks between is ruled dead.
        let died room (facts: RoomFacts) (threat: SeenThreat) =
            let across () =
                let exits = exitsBeside facts.Border threat.At

                let into =
                    RoomName.adjacent room
                    |> List.filter (fun next ->
                        RoomName.offsetOf room next
                        |> Option.exists (fun offset ->
                            exits
                            |> List.exists (fun exit ->
                                Seam.landingOf offset exit |> Option.isSome)))

                not (List.isEmpty into)
                && into
                   |> List.forall (fun next ->
                       seen next
                       && not (
                           roomOf world next
                           |> _.Hostiles
                           |> List.exists (fun h -> h.Id = threat.Id)
                       ))

            threat.Id <> "" && (List.contains threat.Id facts.Fallen || across ())

        let watch (room, facts: RoomFacts) =
            let armed =
                facts.Hostiles
                |> List.filter (fun h ->
                    HostileInfo.armed facts.Hostiles h && not (Colony.isAlly h.Owner))

            match seen room, armed, Map.tryFind room previous with
            | true, _ :: _, _ ->
                Some(
                    room,
                    ExitWatch.Seen(
                        armed
                        |> List.map (fun h ->
                            {
                                Id = h.Id
                                At = RoomPos.pos h.Pos
                                Dies = world.Time + h.TicksToLive
                            })
                    )
                )
            | true, [], Some(ExitWatch.Seen last) ->
                last
                |> List.filter (fun threat -> not (died room facts threat))
                |> List.choose (fun threat ->
                    exitRunFrom facts.Border threat.At |> Option.map (fun run -> run, threat.Dies))
                |> List.sortBy (fun (run, _) -> distanceOf run)
                |> List.tryHead
                |> Option.map (fun (run, dies) ->
                    {
                        Run = run.Run
                        Exit = run.Through
                        Until = min dies (world.Time + tuning.ExitHoldTicks)
                    })
                |> Option.filter (ExitHold.stands world.Time)
                |> Option.map (fun hold -> room, ExitWatch.Held hold)
            | false, _, Some(ExitWatch.Held hold)
            | true, [], Some(ExitWatch.Held hold) when ExitHold.stands world.Time hold ->
                Some(room, ExitWatch.Held hold)
            | _ -> None

        { world with
            ExitWatches = world.Rooms |> Map.toSeq |> Seq.choose watch |> Fresh.mapOfSeq
        }

    /// This tick's world with the healer memory laid under it (#480): every
    /// raid healer standing in a room this tick, and each one remembered from
    /// the last `Tuning.HealMemoryTicks` ticks that is not, at its last tile.
    /// Bounded by this tick's rooms, as `watchExits` is, and built by `Fresh`,
    /// the map being carried to the next tick.
    let recallHealers
        (tuning: Tuning)
        (previous: Map<string, HealerSeen list>)
        (world: World)
        : World =
        let remember (room, facts: RoomFacts) =
            let now =
                facts.Hostiles
                |> List.filter (fun h ->
                    HostileInfo.heals facts.Hostiles h && not (Colony.isAlly h.Owner))

            let standing = now |> List.map (fun h -> h.Id) |> Set.ofList

            let kept =
                Map.tryFind room previous
                |> Option.defaultValue []
                |> List.filter (fun seen ->
                    world.Time - seen.Tick < tuning.HealMemoryTicks
                    && not (Set.contains seen.Healer.Id standing))

            match (now |> List.map (fun h -> { Healer = h; Tick = world.Time })) @ kept with
            | [] -> None
            | healers -> Some(room, healers)

        { world with
            Healers = world.Rooms |> Map.toSeq |> Seq.choose remember |> Fresh.mapOfSeq
        }

    /// This tick's world with the raid memory laid under it (#569): a raider
    /// seen in a room stays its raid until a sighting finds it gone. A room
    /// seen this tick holds the hostiles standing in it beside a raider, or
    /// nothing; a room not seen keeps its last raid, less each body seen
    /// fallen or standing in another room this tick and each whose life has
    /// run out, for as long as a raider is left of it. Bounded by this
    /// tick's rooms, as `recallHealers` is, and built by `Fresh`, the map
    /// being carried to the next tick.
    let recallRaiders (previous: Map<string, RaidSeen>) (world: World) : World =
        let seen = seenNow world

        let fallen =
            world.Rooms
            |> Map.toSeq
            |> Seq.collect (fun (_, facts) -> facts.Fallen)
            |> Set.ofSeq

        let standing =
            world.Rooms
            |> Map.toSeq
            |> Seq.collect (fun (room, facts) -> facts.Hostiles |> Seq.map (fun h -> h.Id, room))
            |> Map.ofSeq

        let remember (room, facts: RoomFacts) =
            let raid =
                if seen room then
                    let now =
                        facts.Hostiles
                        |> List.filter (fun h ->
                            not (Colony.isAlly h.Owner || HostileInfo.isKeeper h))

                    if now |> List.exists (HostileInfo.raider now) then
                        Some { Raid = now; Tick = world.Time }
                    else
                        None
                else
                    Map.tryFind room previous
                    |> Option.map (fun last ->
                        { last with
                            Raid =
                                last.Raid
                                |> List.filter (fun h ->
                                    last.Tick + h.TicksToLive > world.Time
                                    && not (Set.contains h.Id fallen)
                                    && Map.tryFind h.Id standing |> Option.forall ((=) room))
                        })
                    |> Option.filter (fun last ->
                        last.Raid |> List.exists (HostileInfo.raider last.Raid))

            raid |> Option.map (fun raid -> room, raid)

        { world with
            Raiders = world.Rooms |> Map.toSeq |> Seq.choose remember |> Fresh.mapOfSeq
        }

    /// This tick's world with the armed-room memory laid under it (#485):
    /// every room an armed rival stands in this tick at this tick, and each
    /// one remembered from the last `Tuning.HostileRoomMemory` ticks that is
    /// not. Not bounded by this tick's rooms — a room drops out of the world
    /// with the declaration that projected it, and the memory is a clock —
    /// and built by `Fresh`, the map being carried to the next tick.
    let recallArmed (tuning: Tuning) (previous: Map<string, int>) (world: World) : World =
        let now =
            world.Rooms
            |> Map.toSeq
            |> Seq.filter (fun (_, facts) ->
                facts.Hostiles
                |> List.exists (fun h ->
                    HostileInfo.raider facts.Hostiles h && not (Colony.isAlly h.Owner)))
            |> Seq.map (fun (room, _) -> room, world.Time)

        let kept =
            previous
            |> Map.toSeq
            |> Seq.filter (fun (_, tick) -> world.Time - tick < tuning.HostileRoomMemory)

        { world with
            ArmedSeen = Seq.append kept now |> Fresh.mapOfSeq
        }

    /// This tick's world with the rival controllers laid under it (#489): a
    /// room vision answers for this tick takes its own controller, or drops
    /// out where no rival owns it now; a dark room keeps the last one seen.
    /// Not bounded by this tick's rooms, as `recallArmed` is not: a rival's
    /// other room is what refuses this one. Built by `Fresh`, the map being
    /// carried to the next tick.
    let recallRivalControllers (previous: Map<string, RivalController>) (world: World) : World =
        let seen = world.Rooms |> Map.filter (fun _ facts -> Option.isSome facts.Control)

        let kept =
            previous
            |> Map.toSeq
            |> Seq.filter (fun (room, _) -> not (Map.containsKey room seen))

        let now =
            seen
            |> Map.toSeq
            |> Seq.choose (fun (room, facts) ->
                facts.RivalController |> Option.map (fun c -> room, c))

        { world with
            RivalControllers = Seq.append kept now |> Fresh.mapOfSeq
        }

    /// The rooms an armed rival was seen in within `Tuning.HostileRoomMemory`
    /// ticks: what a non-fighter's chain goes round where it can
    /// (`RoomName.routesAvoiding`).
    let avoidedRooms (tuning: Tuning) (world: World) : Set<string> =
        world.ArmedSeen
        |> Map.toSeq
        |> Seq.filter (fun (_, tick) -> world.Time - tick < tuning.HostileRoomMemory)
        |> Seq.map fst
        |> Fresh.setOfSeq

    /// The rooms one of our spawns stands in, in room-name order: the other
    /// fact `Colony.living` asks for.
    let spawnRooms (world: World) : string list =
        world.Rooms
        |> Map.toList
        |> List.filter (fun (_, facts) -> not (List.isEmpty facts.Spawns))
        |> List.map fst

    /// The [[stage]] of every declared colony that is one this tick, derived
    /// off the world because a stage decides whether a mother scans her
    /// child's room at all. Asked of the **declared** homes and of every
    /// **living** colony's home — not every owned room, or a room we claimed
    /// by hand and never declared would arrive in some colony's scan set as a
    /// [[nursery]] to raise.
    ///
    /// The tower a child waits for (#445) is asked only of a declared
    /// child: a colony with no mother has nobody raising it to wait on. A
    /// tower stood full is the latch (`Towered`) or one full this tick, and
    /// is not asked while a raid stands in the child's home or its Guard
    /// holds the exit the raid left by (#468): `Independent` drops the
    /// mother's resident garrison (#447), so a child at the line reads
    /// `Weaning` until the raid has gone, latch or not.
    let rec stages
        (tuning: Tuning)
        (colonies: Colony list)
        (world: World)
        : Map<string, ColonyStage> =
        Colony.homes colonies
        @ (living colonies world |> List.map (fun colony -> colony.Home))
        |> List.distinct
        |> List.choose (fun name ->
            let facts = roomOf world name

            let owned =
                facts.Control |> Option.exists (fun control -> control.Owner = Ownership.Ours)

            let raised =
                colonies
                |> List.exists (fun colony -> colony.Home = name && Option.isSome colony.Mother)

            let raided () =
                facts.Hostiles
                |> List.exists (fun h ->
                    HostileInfo.endangers facts.Hostiles h && not (Colony.isAlly h.Owner))
                || match Map.tryFind name world.ExitWatches with
                   | Some(ExitWatch.Seen _) -> true
                   | Some(ExitWatch.Held hold) -> ExitHold.stands world.Time hold
                   | None -> false

            let towerStood =
                not raised
                || (Set.contains name world.Towered || towerFull tuning facts) && not (raided ())

            Colony.stageOf
                tuning
                owned
                (not (List.isEmpty facts.Spawns))
                towerStood
                (facts.Controller |> Option.map (fun c -> c.Level))
            |> Option.map (fun stage -> name, stage))
        |> Map.ofList

    /// The colonies that run this tick (`Colony.living`).
    and living (colonies: Colony list) (world: World) : Colony list =
        Colony.living (ownedRooms world) (spawnRooms world) colonies

    /// One room's border ring read as a [[seam]]'s near or far side: a tile
    /// whose terrain is not wall and which no declared keeper rock masks
    /// (`Keepers`). Written here rather than inside `linked` so the outpost
    /// and errand chain tests read the one predicate, through `World.linked`
    /// (#317). A **closure over one room**: resolved once, read forty-eight
    /// times.
    let ringWalkable (keeperMargin: int) (room: string) (border: Map<Pos, Terrain>) : Pos -> bool =
        // The mask's declaration is a lookup and the band is a loop, so
        // `maskIn` takes the room here rather than on every tile.
        let masked = Keepers.maskIn keeperMargin room

        fun tile ->
            match Map.tryFind tile border with
            | Some terrain -> terrain <> Wall && not (masked tile)
            | None -> false

    /// The same reading one layer in: a tile of a room's own **ground** a body
    /// could stand on. The [[world]]'s half of the landing test `Atlas.seams`
    /// answers off its raw terrain grid — terrain alone, because a band is
    /// geometry and a structure raised this tick must not move it.
    /// `World.ofGame` already reads terrain for every declared and transit
    /// room, so this costs no plumbing.
    let groundWalkable (keeperMargin: int) (room: string) (terrain: TerrainGrid) : Pos -> bool =
        let masked = Keepers.maskIn keeperMargin room

        fun tile ->
            match TerrainGrid.tryFind tile terrain with
            | Some ground -> ground <> Wall && not (masked tile)
            | None -> false

    /// `Colony.rivals` by room, built once.
    let private declaredRivals: Set<string> =
        Colony.rivals |> List.map fst |> Fresh.setOfSeq

    /// Whether the world last saw another player, not an ally, own the
    /// room's controller (`RoomSighting.Rival`). Read off the memory and not
    /// this tick's vision, which a room nothing of ours stands in does not
    /// have. A reservation is not ownership. A room never seen is rival when
    /// `Colony.rivals` declares it (#459); any sighting overrides that.
    let rivalHeld (world: World) (room: string) : bool =
        match Map.tryFind room world.Sightings with
        | Some sighting ->
            match sighting.Rival with
            | Some owner -> not (Colony.isAlly owner)
            | None -> false
        | None -> Set.contains room declaredRivals

    /// Every room `rivalHeld` answers yes for, in room-name order.
    let rivalRooms (world: World) : string list =
        Set.union (Set.ofSeq (Map.keys world.Sightings)) declaredRivals
        |> Set.toList
        |> List.filter (rivalHeld world)

    /// Who the world last saw owning each room another player owns, allies
    /// included: what Memory keeps across a global reset (`seedRivals`).
    /// Built by `Fresh`, being carried to the next tick.
    let rivalOwners (world: World) : Map<string, string> =
        world.Sightings
        |> Map.toList
        |> List.choose (fun (room, sighting) ->
            sighting.Rival |> Option.map (fun owner -> room, owner))
        |> Fresh.mapOfList

    /// The heap's sightings with an owner read back off Memory laid under
    /// them, for the first tick after a global reset: a room the heap holds
    /// no sighting of gets one naming its owner and nothing else. It carries
    /// no targets, so the vision grace never reads its tick.
    let seedRivals (owners: Map<string, string>) (sightings: Map<string, RoomSighting>) =
        (sightings, owners)
        ||> Map.fold (fun seeded room owner ->
            if Map.containsKey room seeded then
                seeded
            else
                Map.add
                    room
                    {
                        Tick = 0
                        Targets = lazy Set.empty
                        Rival = Some owner
                    }
                    seeded)

    /// `linked`'s terrain half: everything it asks but who owns the room,
    /// over the two rooms' facts as `factsOf` reads them. What
    /// `linkedRecalling` and `reachedRecalling` file, because it is the half
    /// that never moves.
    let private seamedOver
        (keeperMargin: int)
        (factsOf: string -> RoomFacts)
        (fromRoom: string)
        (toRoom: string)
        =
        let walkableIn room =
            ringWalkable keeperMargin room (factsOf room).Border

        Keepers.enterable toRoom
        && Seam.joinedBy
            (walkableIn fromRoom)
            (walkableIn toRoom)
            (groundWalkable keeperMargin toRoom (factsOf toRoom).Layer.Terrain)
            fromRoom
            toRoom

    let private seamed (keeperMargin: int) (world: World) = seamedOver keeperMargin (roomOf world)

    /// Whether a creep could step from one room into the other: the
    /// [[world]]'s own reading of a [[seam]] band, before any [[atlas]] grid
    /// exists. The Atlas answers the same question off its ring grids
    /// (`Atlas.seams`); both go through `Seam.joinedBy`, so the scan set and
    /// the price cannot disagree. A room the world holds no facts for is
    /// joined to nothing. ADR-0058
    ///
    /// The [[keeper margin]] is an argument because a mask near a border can
    /// take exit tiles out of a band, and the routable question has to be
    /// asked over the **same** masked layer every price will use.
    ///
    /// The far side is asked **twice**: its ring, for whether the engine
    /// lands a body there at all, and its ground, for whether the body can
    /// then step off the landing. ADR-0062
    ///
    /// A keeper room with no declared rocks is entered by nothing
    /// (`Keepers.enterable`), as `Atlas.routes` asks it.
    ///
    /// Nor is a room another player owns (`rivalHeld`, #444) — a room a body
    /// already stands in is still left by it.
    let linked (keeperMargin: int) (world: World) (fromRoom: string) (toRoom: string) : bool =
        not (rivalHeld world toRoom) && seamed keeperMargin world fromRoom toRoom

    /// `linked` over a table the caller holds **across ticks**: three readers
    /// ask it per colony per tick (`scanOf`, `ColonyView.ofWorld`,
    /// `creepColonies`), re-asking per hop — 93 calls over 26 distinct pairs
    /// in one colony's tick before any table stood, and 5.7% of a `reactor
    /// --level 7` tick building and dropping three tables a colony once one
    /// did (`docs/profiling.md` § History, 2026-09-18).
    ///
    /// The table can outlive the tick because an answer reads only terrain —
    /// which the engine never changes — under a keeper margin that is in the
    /// key. What *can* move between ticks is which rooms the world holds and
    /// who owns them, so both are read ahead of the table on every ask.
    /// Asymmetric by construction, like `linked`: the key is the ordered pair
    /// and an answer is never reused backwards.
    let linkedRecalling
        (joins: JoinTable)
        (keeperMargin: int)
        (world: World)
        : string -> string -> bool =
        fun fromRoom toRoom ->
            if
                not (Map.containsKey fromRoom world.Rooms && Map.containsKey toRoom world.Rooms)
                || rivalHeld world toRoom
            then
                false
            else
                let key = $"{keeperMargin}|{fromRoom}|{toRoom}"

                // `ContainsKey` then the indexer, never `TryGetValue` in a
                // match: Fable compiles the out-parameter pattern into four
                // allocations per read (`Atlas.memoised`'s note).
                if joins.Joins.ContainsKey key then
                    joins.Joins.[key]
                else
                    let joined = seamed keeperMargin world fromRoom toRoom
                    joins.Joins.[key] <- joined
                    joined

    /// **A room a stronghold holds is not a link** (#382), asked of **both**
    /// ends of every hop. A declaration whose every chain crossed it stops
    /// being `routable` and is refused as a unit, and the tick the core's
    /// collapse timer runs out the room re-links. ADR-0074
    let linkedAvoiding (impassable: Set<string>) (joined: string -> string -> bool) =
        fun (fromRoom: string) (toRoom: string) ->
            not (Set.contains fromRoom impassable)
            && not (Set.contains toRoom impassable)
            && joined fromRoom toRoom

    /// **The predicate every production reader of a chain is built on**
    /// (#382): the memoised join with the stronghold rooms taken out. One
    /// combinator and not two spellings, so the scan set and the refusal
    /// report agree. `linkedBy` below is the bare join a test asks for and
    /// avoids no stronghold.
    let reachesUnder (gate: StandDown) (joins: JoinTable) (tuning: Tuning) (world: World) =
        linkedRecalling joins (Tuning.keeperMargin tuning) world
        |> linkedAvoiding gate.Impassable

    /// `Declaration.hops` over `reachesUnder`, recalled from the join table:
    /// keyed by everything the chain search reads that is not terrain — the
    /// keeper margin, the hop budget, the rooms withheld from passage, the
    /// rooms a rival owns (`rivalRooms`) and the rooms the world holds, since
    /// `linkedRecalling` enters nothing outside them. Past 4,096 rows the table is emptied rather than grown: a key
    /// minted per change of the world's rooms has nothing else to evict it.
    let hopsUnder (gate: StandDown) (joins: JoinTable) (tuning: Tuning) (world: World) =
        let reaches = reachesUnder gate joins tuning world

        let stamp =
            String.concat
                "|"
                [
                    string (Tuning.keeperMargin tuning)
                    string tuning.MaxHops
                    String.concat "," gate.Impassable
                    String.concat "," (rivalRooms world)
                    String.concat "," (Map.keys world.Rooms)
                ]

        fun (home: string) (room: string) ->
            let key = stamp + "|" + home + "|" + room

            if joins.Hops.ContainsKey key then
                match joins.Hops.[key] with
                | hops when hops < 0 -> None
                | hops -> Some hops
            else
                if joins.Hops.Count >= 4096 then
                    joins.Hops.Clear()

                let hops = Declaration.hops reaches tuning.MaxHops home room
                joins.Hops.[key] <- Option.defaultValue -1 hops
                hops

    /// `linkedRecalling` over every room's ground (`groundOf`) and not over
    /// the rooms the world holds (#617): a room's exits are the engine's,
    /// and vision decides only what stands in it. Who owns the far room is
    /// still asked first. A pair either of whose ground cannot be read is
    /// joined by nothing. Filed in the same table under the same key: where
    /// both rooms are held, their ground is their facts' terrain.
    let reachedRecalling
        (joins: JoinTable)
        (keeperMargin: int)
        (world: World)
        : string -> string -> bool =
        fun fromRoom toRoom ->
            if rivalHeld world toRoom then
                false
            else
                let key = $"{keeperMargin}|{fromRoom}|{toRoom}"

                if joins.Joins.ContainsKey key then
                    joins.Joins.[key]
                else
                    match groundOf world fromRoom, groundOf world toRoom with
                    | Some fromFacts, Some toFacts ->
                        let joined =
                            seamedOver
                                keeperMargin
                                (fun room -> if room = fromRoom then fromFacts else toFacts)
                                fromRoom
                                toRoom

                        joins.Joins.[key] <- joined
                        joined
                    | _ -> false

    /// `reachesUnder` over every room's ground (`reachedRecalling`, #617):
    /// the predicate a fighter's reach and walk home are searched over.
    let reachesOver (gate: StandDown) (joins: JoinTable) (tuning: Tuning) (world: World) =
        reachedRecalling joins (Tuning.keeperMargin tuning) world
        |> linkedAvoiding gate.Impassable

    /// The crossings between two rooms inside `budget`, as `hopsUnder` counts
    /// them but over every room's ground (`reachedRecalling`, #617): what the
    /// fight's reach reads — the lane a colony projects another's contested
    /// room over, a harassment room's caster, a cast's walk — so a room going
    /// dark moves none of them. A declaration's chain stays `hopsUnder`'s:
    /// it must be one the projection's Atlas prices.
    let reachHops
        (gate: StandDown)
        (joins: JoinTable)
        (tuning: Tuning)
        (world: World)
        (budget: int)
        =
        let reaches = reachesOver gate joins tuning world

        let stamp =
            String.concat
                "|"
                [
                    "reach"
                    string (Tuning.keeperMargin tuning)
                    string budget
                    String.concat "," gate.Impassable
                    String.concat "," (rivalRooms world)
                ]

        fun (home: string) (room: string) ->
            let key = stamp + "|" + home + "|" + room

            if joins.Hops.ContainsKey key then
                match joins.Hops.[key] with
                | hops when hops < 0 -> None
                | hops -> Some hops
            else
                if joins.Hops.Count >= 4096 then
                    joins.Hops.Clear()

                let hops = Declaration.hops reaches budget home room
                joins.Hops.[key] <- Option.defaultValue -1 hops
                hops

    /// The colony whose home bank holds the most, ties by home name.
    let private largestBank (world: World) (colonies: Colony list) : string option =
        colonies
        |> List.sortBy (fun colony -> -(roomOf world colony.Home).Energy.Capacity, colony.Home)
        |> List.tryHead
        |> Option.map (fun colony -> colony.Home)

    /// The energy in a room's Storage: the stock `CastBid.paysOthers` reads,
    /// as `Facts.stockedEnergy` reads a view's.
    let private stockIn (facts: RoomFacts) : int =
        facts.TargetKinds
        |> Map.fold
            (fun total id kind ->
                if kind = Structure BuiltKind.Storage then
                    total + (Map.tryFind id facts.Stores |> Option.defaultValue 0)
                else
                    total)
            0

    /// The colonies that may cast a body of `floor` energy for a room no
    /// colony asks for its own, before the views are cut, each with its bid
    /// (`casterOf` chooses among them): the living colonies but `except`,
    /// with a spawn, a bank at `Tuning.CasterFloor` that buys the body, a
    /// stock past the floor, and `hopsOf` crossings to the room inside the
    /// hop budget, bidding the walk, the body being one size whoever casts
    /// it. The projection follows the choice, so it holds still across the
    /// band a cast dips the stock through, read off the world and last
    /// tick's Tasks, kept nowhere: the **incumbent**, a colony that cast a
    /// ranger standing in the room or a fighter holding its Guard (`held`: on
    /// the walk out too), keeps it down to `Tuning.CasterReserveHold` and
    /// bids its walk `Tuning.CasterSwitchMargin` short; a colony with a
    /// ranger in its oven is read at the hold too, the body not yet holding
    /// the Guard it was cast for. Any other comes in at
    /// `Tuning.CasterReserve`. The spawns' queue is not read.
    let private castBids
        (hopsOf: string -> string -> int option)
        (tuning: Tuning)
        (living: Colony list)
        (world: World)
        (held: Map<string, string>)
        (except: string)
        (floor: int)
        (room: string)
        : CastBid list =
        // The Guard's id as `Tasks.taskId` spells it.
        let guard = "guard:" + room

        let incumbents =
            match
                world.Creeps
                |> List.filter (fun creep ->
                    creep.Room = room && Map.containsKey RangedAttack creep.Info.Body
                    || Map.tryFind creep.Info.Name held = Some guard)
            with
            | [] -> Set.empty
            | standing ->
                let homes = spawnHomes world

                standing
                |> List.choose (fun creep -> Colony.castBy homes creep.Info.Name)
                |> Set.ofList

        let casting (home: RoomFacts) =
            home.Casting |> List.exists (fun cast -> List.contains RangedAttack cast.Body)

        let bids =
            living
            |> List.filter (fun colony ->
                let home = roomOf world colony.Home

                let reserve =
                    if incumbents.Contains colony.Home || casting home then
                        tuning.CasterReserveHold
                    else
                        tuning.CasterReserve

                colony.Home <> except
                && not (List.isEmpty home.Spawns)
                && home.Energy.Capacity >= max floor tuning.CasterFloor
                && stockIn home >= reserve)
            |> List.choose (fun colony ->
                hopsOf colony.Home room
                |> Option.filter (fun hops -> hops <= tuning.MaxHops)
                |> Option.map (fun hops ->
                    {
                        Home = colony.Home
                        Cost = CastBid.walk tuning (Some hops)
                        Asks = false
                    }))

        // The one casting it now bids its walk less the margin.
        bids
        |> List.map (fun bid ->
            if incumbents.Contains bid.Home then
                { bid with
                    Cost = bid.Cost - tuning.CasterSwitchMargin
                }
            else
                bid)

    /// The Caster's choice of one colony among `castBids`' (`CastBid.choose`):
    /// the least cost, then by home.
    let private casterOf
        (hopsOf: string -> string -> int option)
        (tuning: Tuning)
        (living: Colony list)
        (world: World)
        (held: Map<string, string>)
        (except: string)
        (floor: int)
        (room: string)
        : string option =
        castBids hopsOf tuning living world held except floor room |> CastBid.choose

    /// The colonies that project each [[sweep]] candidate this tick (#658):
    /// every one past the Caster's floors that reaches it (`castBids`, over
    /// every room's ground, `reachHops`, #617), the room being no colony's
    /// own. The sweep is one operation, but no colony runs it: where it goes
    /// and how big its group is are its price's (`Facts.sweep`), and each
    /// body is the Caster's to hand the colony landing it soonest
    /// (`Caster.assign`), so a candidate one colony cannot reach is worked
    /// by one that can. Empty while no colony may. Stateless, and decided
    /// once for every colony. Asked over the open gate: a colony's own
    /// stand-down withdraws a candidate from its work alone. A room we own
    /// is out of the list, cast and refused by nobody (#447): the day a
    /// Claim lands in a candidate, its garrison holds it.
    let harassCasters
        (joins: JoinTable)
        (tuning: Tuning)
        (colonies: Colony list)
        (harass: Harassment)
        // Last tick's Tasks (`HarassCasting.Holding`).
        (held: Map<string, string>)
        (world: World)
        : HarassCasting =
        let hopsOf = reachHops StandDown.none joins tuning world tuning.MaxHops
        let living = living colonies world
        let owned = ownedRooms world

        let rooms =
            harass.Rooms |> List.filter (fun h -> not (Set.contains h.RoomName owned))

        {
            Casters =
                rooms
                |> List.map (fun h ->
                    h,
                    castBids hopsOf tuning living world held "" 0 h.RoomName
                    |> List.map _.Home
                    |> List.sort)
            GarrisonFloor = tuning.RangerResidentBlocks * harass.BlockCost
            Holding = held
        }

    /// The colony that keeps each contested home's resident garrison
    /// (#479), by the Caster's rule (`casterOf`): a home of ours declaring a
    /// perimeter, `Independent`, short of `Tuning.GarrisonTowers` towers. Its
    /// garrison is no ask of its own but its residents', so its caster is
    /// another colony past both floors, its bank buying the resident body
    /// (`HarassCasting.GarrisonFloor`); none while no colony may. Kinship
    /// plays no part. Over the open gate and stateless, so every colony reads
    /// one caster.
    let garrisonCasters
        (joins: JoinTable)
        (tuning: Tuning)
        (stages: Map<string, ColonyStage>)
        (casting: HarassCasting)
        (colonies: Colony list)
        (world: World)
        : Map<string, string> =
        let contested =
            colonies
            |> List.filter (fun home ->
                not (List.isEmpty home.Perimeter)
                && Map.tryFind home.Home stages = Some Independent
                && towersStanding (roomOf world home.Home) < tuning.GarrisonTowers)

        match contested with
        | [] -> Map.empty
        | _ ->
            let hopsOf = reachHops StandDown.none joins tuning world tuning.MaxHops
            let living = living colonies world

            // Off the names where no route prices it, as a defended home's
            // walk is (`defends`).
            let crossings from room =
                hopsOf from room |> Option.orElse (RoomName.hopsBetween from room)

            contested
            |> List.choose (fun home ->
                casterOf
                    crossings
                    tuning
                    living
                    world
                    casting.Holding
                    home.Home
                    casting.GarrisonFloor
                    home.Home
                |> Option.map (fun caster -> home.Home, caster))
            |> Map.ofList

    /// The living colony that says a [[harassment room]] no colony casts out
    /// loud (`ColonyView.Refused`): the largest bank of all, ties by home
    /// name, so a room no colony can cast is still named once.
    let harassReporter (colonies: Colony list) (world: World) : string option =
        living colonies world |> largestBank world

    /// `linkedRecalling` over a table of this call's own — the shape a test
    /// asks in. The shell never calls this.
    let linkedBy (keeperMargin: int) (world: World) : string -> string -> bool =
        linkedRecalling (JoinTable()) keeperMargin world

    /// What one colony's declaration narrows to this tick, and the union of
    /// it, in named halves rather than a positional tuple (three are
    /// `string list`s, so a swapped pair would compile in silence). Declared
    /// inside `World` and never auto-opened, so the names cannot be picked up
    /// by a record literal that meant `Colony`.
    type ScanSet =
        {
            /// The outposts left after both narrowings — the gate's and the
            /// chain's (#259).
            Outposts: Outpost list
            /// The errands left after the one narrowing there is.
            Errands: Errand list
            /// The [[salvage]] rooms left after the same two an errand takes.
            Salvage: string list
            /// The rooms this colony projects for a child of its own, raised
            /// or re-claimed (#221).
            Borrowed: string list
            /// The other colonies' homes this colony projects for their
            /// defence this tick (`defends`).
            Defended: string list
            /// The contested homes this colony garrisons
            /// (`garrisonCasters`, #479).
            Garrisoned: string list
            /// The [[harassment room]]s this colony casts (`harassCasters`),
            /// less the ones its [[stand-down]] shuts.
            Harass: Harass list
            /// Every harassment room this colony casts, the shut ones
            /// included.
            Cast: Harass list
            /// The harassment rooms no colony casts this tick
            /// (`harassCasters`).
            Uncast: Harass list
            /// The [[assault]]s this colony runs (`Assault.worked`): no
            /// stand-down or chain narrows them, the room being a rival's.
            Assaults: Assault list
            /// The scan set: this colony's home and all six of those, the
            /// one place that union is spelled. A cast harassment room its
            /// stand-down shuts stays in it, as a transit room: a ranger
            /// standing there is still placed, and walks home.
            Scanned: string list
            /// The rooms of `Scanned` this colony works — home, its worked
            /// declarations, its borrowed and garrisoned homes, the harassment
            /// rooms and assaults it runs. Every other scanned room is a
            /// [[transit room]].
            Worked: Set<string>
            /// The [[evacuating room]]s, in `Scanned` (#512): each outpost,
            /// errand or salvage room its stand-down shuts, with the transit
            /// rooms of the walk home from it, while a body this colony holds
            /// stands on that walk; and the `Stranded` rooms (#570).
            Evacuating: string list
            /// The outposts of the children whose homes this colony keeps
            /// residents in — `Borrowed`'s raised half and `Garrisoned` —
            /// that border those homes and that the child's chain reaches
            /// (#517), shut by her stand-down or not (#534), in `Scanned` as
            /// transit rooms.
            ChildOutposts: string list
            /// The rooms of `Evacuating` held for a stray body alone (#570):
            /// each room a body this colony holds stands in that nothing else
            /// here projects, with the rooms it may walk out over. No chain
            /// but one that starts in one enters them
            /// (`SpatialInfo.StrayRooms`).
            Stranded: string list
        }

    /// Each outpost, errand or salvage room of this colony's its stand-down
    /// shuts, with the rooms a walk home from it crosses — the rectangle the
    /// room was projected through while open (`Outpost.roomsProjected`),
    /// and the only rooms outside the rest of the scan set a chain home can
    /// cross, since `Atlas.routes` joins projected rooms alone (#512).
    let private shutWalks (stages: Map<string, ColonyStage>) (shut: Set<string>) (colony: Colony) =
        if Set.isEmpty shut then
            []
        else
            (Colony.outpostsWorked stages colony
             |> List.map (fun outpost -> outpost.RoomName))
            @ (colony.Errands |> List.map (fun errand -> errand.RoomName))
            @ colony.Salvage
            |> List.filter shut.Contains
            |> List.map (fun room -> room, RoomName.transitBetween colony.Home room)

    /// The declaration's narrowings and the union they make, for one colony.
    /// The two outpost narrowings answer different questions: the
    /// [[stand-down]] gate is this tick's and reopens, the chain is the
    /// declaration's and never does — a refused room leaves the scan set for
    /// good, and `ColonyView.Refused` says so.
    ///
    /// An errand is narrowed by the chain and by a stand-down on its **own
    /// target room** (#348). A shut transit room deliberately does not reach
    /// this clause: the gate withdraws work in the room it names and is not
    /// a route lock; the body crossing that room reads its live Threat and
    /// Flee instead. ADR-0066
    ///
    /// A [[salvage]] room is narrowed the same two ways.
    ///
    /// A [[harassment room]] is the one declaration read off the global list
    /// and not the colony's: kept by the colony that casts it.
    ///
    /// The whole `Tuning` and not the hop budget alone: the chain is searched
    /// over the **masked** border rings (`Tuning.keeperMargin`).
    let scanRecalling
        (joins: JoinTable)
        (tuning: Tuning)
        (stages: Map<string, ColonyStage>)
        (unowned: Set<string>)
        (colonies: Colony list)
        (casting: HarassCasting)
        // The gate whole and not its `Shut` set alone (#382): the scan set
        // asks which rooms are withheld from work, and which of those cannot
        // be crossed either.
        (gate: StandDown)
        // The rooms a body this colony holds stands in (`creepColoniesRecalling`'s
        // answer). Empty for the adoption itself, which reads the set without
        // the evacuating rooms: a body is adopted off the declaration's rooms.
        (standing: Set<string>)
        (world: World)
        (colony: Colony)
        : ScanSet =
        // Every declaration's chain off the one recalled count (`hopsUnder`):
        // routable is a chain inside the budget both ways.
        let hopsOf = hopsUnder gate joins tuning world

        let routable room =
            hopsOf colony.Home room |> Option.isSome

        let cast =
            casting.Casters
            |> List.filter (fun (h, casters) ->
                List.contains colony.Home casters && routable h.RoomName)
            |> List.map fst

        let outposts =
            Outpost.worked gate.Shut (Colony.outpostsWorked stages colony)
            |> List.filter (fun outpost -> routable outpost.RoomName)

        let errands =
            Errand.worked gate.Shut colony.Errands
            |> List.filter (fun errand -> routable errand.RoomName)

        let salvage = Salvage.worked gate.Shut colony.Salvage |> List.filter routable

        // The two halves of what a mother projects for a child of hers,
        // disjoint by construction: a room she is raising is one we own, and
        // a room she may take back is one we do not (#221).
        let raising = Colony.bootstrapping stages colonies colony

        let borrowed = raising @ Colony.reclaiming stages unowned colonies colony

        // The contested homes the Caster hands this colony's garrison, less a
        // room it works as an outpost.
        let garrisoned =
            garrisonCasters joins tuning stages casting colonies world
            |> Map.filter (fun home caster ->
                caster = colony.Home
                && not (
                    Colony.outpostsWorked stages colony
                    |> List.exists (fun outpost -> outpost.RoomName = home)
                ))
            |> Map.keys
            |> List.ofSeq

        // A garrisoned home's chain walked as a defended one's is.
        let covered =
            Colony.roomsProjected outposts errands salvage borrowed garrisoned colony.Home

        // Every declared home of ours this one may answer, kin or not and
        // spawn or not (#589).
        let defended =
            Colony.homes colonies |> List.filter (defends tuning world covered colony.Home)

        let assaults = Assault.worked colony.Assaults

        let scanned =
            Colony.roomsProjected
                outposts
                errands
                salvage
                borrowed
                (defended @ garrisoned)
                colony.Home
            @ Harass.roomsProjected cast colony.Home
            @ Assault.roomsProjected assaults colony.Home
            |> List.distinct

        let harass = Harass.worked gate.Shut cast

        let worked =
            colony.Home :: (outposts |> List.map (fun outpost -> outpost.RoomName))
            @ (errands |> List.map (fun errand -> errand.RoomName))
            @ salvage
            @ borrowed
            @ garrisoned
            @ (harass |> List.map (fun h -> h.RoomName))
            @ (assaults |> List.map (fun assault -> assault.RoomName))
            |> Set.ofList

        // A shut room with a body of ours on its walk home, the room itself or
        // a transit room of the walk, already scanned or not (#512): kept while
        // one stands there, so the body is placed, Reach and Flee answer for
        // it, and it walks home. No chain filter: the body is there whatever
        // the budget says.
        let evacuating =
            if Set.isEmpty standing then
                []
            else
                shutWalks stages gate.Shut colony
                |> List.collect (fun (room, crossed) ->
                    let walk = room :: (crossed |> List.filter (worked.Contains >> not))

                    if List.exists standing.Contains walk then walk else [])
                |> List.distinct

        // A shut outpost whose raid a ranger chases (#510), with the walk to
        // it: projected as a transit room, so the chase is pooled there and
        // nothing else is.
        let chasing =
            shutWalks stages gate.ChasedOutposts colony
            |> List.collect (fun (room, crossed) ->
                room :: (crossed |> List.filter (worked.Contains >> not)))
            |> List.distinct

        // The outposts of a child its residents hold, beside its home (#517):
        // projected as transit rooms, so a resident sees the trespasser or
        // the chase next door. Narrowed by the child's chain, and **not** by
        // her stand-down (#534): shut withholds her economy and never the
        // residents' watch, so a raid there turned Kite is still answered.
        // Every body there stays its caster's, her resident hers and the
        // child's own the child's: a body on the walk home from a room its
        // colony shuts stays that colony's (`creepColoniesRecalling`). A room
        // a stronghold is seen in is left out, as the child's own chain
        // leaves it.
        let childOutposts =
            match raising @ garrisoned with
            | [] -> []
            | homes ->
                colonies
                |> List.filter (fun child -> List.contains child.Home homes)
                |> List.collect (fun child ->
                    let hops = hopsUnder StandDown.none joins tuning world

                    Colony.outpostsWorked stages child
                    |> List.map (fun outpost -> outpost.RoomName)
                    |> List.filter (fun room ->
                        RoomName.neighbouring child.Home room
                        && not (
                            (roomOf world room).InvaderCores
                            |> List.exists InvaderCoreInfo.isStronghold
                        )
                        && Option.isSome (hops child.Home room)))
                |> List.filter (worked.Contains >> not)
                |> List.distinct

        // A room a body this colony holds stands in that nothing above
        // projects (#570) — a stronghold's room no chain enters, a room a
        // hauler strayed into — with the rooms it may walk out over
        // (`RoomName.walkOut`) toward this colony's home whose ground the world
        // reads (`World.groundOf`): so the body is placed,
        // Reach and Flee answer for it, and it walks home. No chain or gate
        // filter, as for `evacuating`: the body is there whatever they say.
        let stranded =
            if Set.isEmpty standing then
                []
            else
                let held = Set.ofList (scanned @ evacuating @ chasing @ childOutposts)

                standing
                |> Seq.filter (held.Contains >> not)
                |> Seq.collect (RoomName.walkOut tuning.MaxHops colony.Home)
                |> Seq.distinct
                |> Seq.filter (fun room ->
                    not (held.Contains room) && Option.isSome (groundOf world room))
                |> List.ofSeq

        {
            Outposts = outposts
            Errands = errands
            Salvage = salvage
            Borrowed = borrowed
            Defended = defended
            Garrisoned = garrisoned
            Harass = harass
            Cast = cast
            Uncast = casting.Casters |> List.filter (snd >> List.isEmpty) |> List.map fst
            Assaults = assaults
            Scanned = scanned @ evacuating @ chasing @ childOutposts @ stranded |> List.distinct
            Worked = worked
            Evacuating = evacuating @ stranded |> List.distinct
            ChildOutposts = childOutposts
            Stranded = stranded
        }

    /// `scanRecalling` over a table of this call's own (`linkedBy`).
    let scanOf
        (tuning: Tuning)
        (stages: Map<string, ColonyStage>)
        (unowned: Set<string>)
        (colonies: Colony list)
        // The gate whole and not its `Shut` set alone (#382): see
        // `scanRecalling`.
        (gate: StandDown)
        (world: World)
        (colony: Colony)
        : ScanSet =
        scanRecalling
            (JoinTable())
            tuning
            stages
            unowned
            colonies
            HarassCasting.none
            gate
            Set.empty
            world
            colony

    /// The declared homes that stand empty this tick: ours to take back if
    /// they ever were ours, and the candidates a human means to take. A room
    /// nothing looked into answers no, which is absence and not a claim that
    /// somebody holds it.
    let unownedHomes (colonies: Colony list) (world: World) : Set<string> =
        Colony.homes colonies
        |> List.filter (fun name ->
            (roomOf world name).Control
            |> Option.exists (fun control -> control.Owner = Ownership.Unowned))
        |> Set.ofList

    /// The rooms one colony projects this tick, off the world:
    /// `scanRecalling`'s union with the stages and the ownership it needs
    /// read for it, over the caller's join table.
    let roomsProjectedRecalling
        (joins: JoinTable)
        (tuning: Tuning)
        (colonies: Colony list)
        (casting: HarassCasting)
        (gate: StandDown)
        (world: World)
        (colony: Colony)
        : string list =
        let scan =
            scanRecalling
                joins
                tuning
                (stages tuning colonies world)
                (unownedHomes colonies world)
                colonies
                casting
                gate
                Set.empty
                world
                colony

        scan.Scanned

    /// Which colony holds each creep this tick (`Colony.creepColonies`),
    /// decided over every living colony's scan set at once, or two decisions
    /// would move one body twice. Over the caller's join table: the pairs the
    /// views' own walk answered are read, not re-derived.
    let creepColoniesRecalling
        (joins: JoinTable)
        (tuning: Tuning)
        (colonies: Colony list)
        (casting: HarassCasting)
        (running: Colony list)
        (shut: Map<string, Set<string>>)
        (world: World)
        : Map<string, string> =
        let projections =
            running
            |> List.map (fun colony ->
                colony.Home,
                roomsProjectedRecalling
                    joins
                    tuning
                    colonies
                    casting
                    { StandDown.none with
                        // **Passability is deliberately not asked here** (#382):
                        // a body already standing inside a bunker's room is
                        // exactly the one that must still be attributed to
                        // somebody — two of them were, on the tick this rule
                        // was written for. `Impassable` stays empty.
                        Shut = Map.tryFind colony.Home shut |> Option.defaultValue Set.empty
                    }
                    world
                    colony)

        let spawns = spawnHomes world

        let adopted =
            Colony.creepColonies
                projections
                spawns
                (world.Creeps |> List.map (fun creep -> creep.Info.Name, Some creep.Room))

        // A body standing on the walk home from a room its own colony's
        // stand-down shuts stays that colony's, whoever else projects the
        // room: the caster is the one that walks it home (#512).
        let walks =
            if Map.isEmpty shut then
                Map.empty
            else
                let stages = stages tuning colonies world

                running
                |> List.map (fun colony ->
                    colony.Home,
                    shutWalks
                        stages
                        (Map.tryFind colony.Home shut |> Option.defaultValue Set.empty)
                        colony
                    |> List.collect (fun (room, crossed) -> room :: crossed)
                    |> Set.ofList)
                |> Map.ofList

        if Map.isEmpty walks then
            adopted
        else
            (adopted, world.Creeps)
            ||> List.fold (fun held creep ->
                match Colony.castBy spawns creep.Info.Name with
                | Some home when Map.tryFind home walks |> Option.exists (Set.contains creep.Room) ->
                    Map.add creep.Info.Name home held
                | _ -> held)

    /// `creepColoniesRecalling` over a table of this call's own (`linkedBy`).
    let creepColonies
        (tuning: Tuning)
        (colonies: Colony list)
        (running: Colony list)
        (shut: Map<string, Set<string>>)
        (world: World)
        : Map<string, string> =
        creepColoniesRecalling (JoinTable()) tuning colonies HarassCasting.none running shut world
