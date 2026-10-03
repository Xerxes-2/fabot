/// The tick's threat facts: the tiles a hostile can reach, layered by room
/// name. Colony facts, never a change to the spatial projection.
[<AutoOpen>]
module Fabot.Core.Decide.Threat

open Fabot.Core
open Fabot.Core.Types

/// One `Fight`'s ground this tick (#453), off the squad's holders: the rally
/// ground until the squad launches, the fighting ground after.
type FightGround =
    {
        /// The squad the room's Fight is fought with (`HeldTaskFacts.Fights`),
        /// latched by its fight record.
        Squad: Squad
        /// Derived each tick from where the casts stand, never stored: every
        /// slot filled by a living body, a cast holding the Fight or a
        /// resident fitting it, and each cast holder on the rally ground or
        /// with the squad — within two of the leader, the leader within two
        /// of another; or, once in, every cast holder standing in the room or
        /// at its crossing. Never without a rally ground, nor with a slot
        /// empty.
        Launched: bool
        /// The rally ground: tiles beside the crossing toward the room, in
        /// the last room on the chain from home short of it that is neither a
        /// Source Keeper's nor a rival's, clear of the border and of that
        /// room's Reach. Empty where no chain reaches the room.
        Rally: Set<RoomPos>
        /// A brawler's ground: the rally ground, or, launched, the kill
        /// order's head's ring, and the room's resident ring while no target
        /// stands there.
        Front: Set<RoomPos>
        /// A medic's: the rally ground, or, launched, the tiles beside the
        /// leader no melee body stands beside — behind the brawler it
        /// pre-heals, where the raid's melee strike the brawler and take its
        /// strike-back — else the tiles beside the leader; the rally ground
        /// with no leader to walk behind, never the front.
        Behind: Set<RoomPos>
        /// A kiter's: the rally ground, or, launched, the kite ground.
        Ranged: Set<RoomPos>
        /// The bodies the Fight admits, by name and role: every squad cast,
        /// and once launched each resident that fits a slot.
        Roles: Map<string, SquadRole>
        /// The room's residents — holders of its Guard or its Fight that are
        /// no squad cast — whose parts fit one of its slots, by role.
        Residents: Map<string, SquadRole>
    }

/// One `Assault`'s ground this tick (#490), off its squad's holders: the
/// rally ground until the squad launches, beside its target after.
type AssaultGround =
    {
        /// The roles the declared squad casts (`Assault.Squad`).
        Slots: SquadRole list
        /// A `Fight`'s launch, over its casts alone, and never while one of
        /// them is hurt — under half its hits once in, under four fifths
        /// before — or while safe mode runs in the room.
        Launched: bool
        /// The rally ground, as a `Fight`'s, on the chain that ends in the
        /// rival's room.
        Rally: Set<RoomPos>
        /// What the sappers take down this tick (`AssaultFacts.Targets`'
        /// head), None while the room is dark or nothing is left.
        Target: (string * RoomPos) option
        /// A sapper's ground: the rally ground, or, launched, the tiles beside
        /// the target — beside the first declared breach tile while the room is
        /// dark, and beside the last once a `Provoke`'s breach has fallen.
        Front: Set<RoomPos>
        /// A medic's: the rally ground, or, launched, the tiles beside the
        /// leading sapper that are not the front.
        Behind: Set<RoomPos>
        /// Its casts holding it, by role.
        Roles: Map<string, SquadRole>
    }

/// ADR-0033. Derived once a tick and shared by the applicability gate, Flee's
/// Work Area and the spawn hold. Keyed by the room the hostile stands in: a
/// `Set<Pos>` cannot say which room's tiles it holds, so the room rides on the
/// outer key. A room with no entry answers the empty set.
type Threats =
    {
        /// Per room, the tiles a Threat standing in it can hurt. Never an
        /// empty set under a room: a room whose whole Reach our ramparts
        /// took back has no entry, so `Map.isEmpty` is "no Reach
        /// anywhere" — the one question the pool asks of it.
        Reach: Map<string, Set<Pos>>
        /// Per room, the walkable tiles no Threat reaches — Flee's Work Area
        /// for a creep standing there. Derived only for the rooms with a Reach:
        /// absence means "not derived", not "nowhere is safe".
        /// `Lazy` because it is 2,000 tiles a room and its only reader is
        /// Flee's Work Area, which most ticks has no creep to offer it to (#371).
        Safe: Map<string, Lazy<Set<RoomPos>>>
        /// ADR-0056. Per room, the walkable tiles within range 1 of a Threat
        /// standing in it, less the tiles the Threats stand on — the guard's
        /// Work Area. In a room whose last Threat has gone, the ground
        /// beside the exit it left by while that is held (#450).
        Ring: Map<string, Set<RoomPos>>
        /// Per resident room (`Facts.residentRooms`), the garrison's Work Area
        /// there, which it holds and shoots from: in a declared errand room
        /// the Reactor's own ring (#414, #411), in a raised child's home its
        /// controller's while no armed Threat stands there (#447).
        ResidentRing: Map<string, Set<RoomPos>>
        /// Per [[harassment room]] (#432), the ranger's Work Area there: an
        /// ambush (#439) — the walkable tiles within ranged reach of every
        /// enemy creep on a work spot, beside every armed target, and the
        /// declared `Stand`'s seats while neither stands there.
        HarassRing: Map<string, Set<RoomPos>>
        /// Per ranger room (`Facts.rangerRooms`) under a raid or a rival's
        /// claimer (#451), the ranger's ground there ahead of every other:
        /// under an armed raid, our ramparts within three of the kill order's
        /// head (#467); else in a resident room whose raid no ranger wins
        /// (`Facts.outmatched`), the room's safe set, laid only while a ranger of ours stands there or
        /// holds its Guard (`threatsOfHeld`);
        /// else, while a melee body stands there or the kill order's head is
        /// a claimer, the kite ground — the tiles within three of that head,
        /// less every tile a melee body reaches in a step and a swing, and
        /// less swamp near one. With a melee body there and that empty, the
        /// tiles farthest from it, never the threats' ring beside it. A room
        /// whose squad has launched holds the squad's kite ground
        /// (`threatsOfHeld`).
        Kite: Map<string, Set<RoomPos>>
        /// The rooms whose `Ring` is a held exit's ground (#450): where the
        /// Planner keeps a living guard's Guard pooled, and hires for none.
        Held: Set<string>
        /// Per room a Fight is pooled for (`Facts.fights`), its ground
        /// (`threatsOfHeld`); empty off `threatsOf`.
        Fight: Map<string, FightGround>
        /// Per room an Assault is pooled for (`ColonyView.Assaults`), its
        /// ground (`threatsOfHeld`); empty off `threatsOf`.
        Assault: Map<string, AssaultGround>
    }

/// The tick with nothing to run from: what the pipeline is handed for a quiet
/// colony.
let noThreats =
    {
        Reach = Map.empty
        Safe = Map.empty
        Ring = Map.empty
        ResidentRing = Map.empty
        HarassRing = Map.empty
        Kite = Map.empty
        Held = Set.empty
        Fight = Map.empty
        Assault = Map.empty
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Threats =
    /// One room's Reach; empty for a room no Threat stands in.
    let reachIn (threats: Threats) (room: string) : Set<Pos> =
        Map.tryFind room threats.Reach |> Option.defaultValue Set.empty

    /// One room's safe set, already joined to that room; empty for a room
    /// no Reach was derived in.
    let safeIn (threats: Threats) (room: string) : Set<RoomPos> =
        Map.tryFind room threats.Safe
        |> Option.map (fun ground -> ground.Force())
        |> Option.defaultValue Set.empty

    /// One room's range-1 ring, already joined to that room; empty for a room
    /// no Threat stands in, which leaves that room's Guard applicable to nobody.
    let ringIn (threats: Threats) (room: string) : Set<RoomPos> =
        Map.tryFind room threats.Ring |> Option.defaultValue Set.empty

    /// The garrison's Work Area in one resident room, or None for any other
    /// room (#414, #447).
    let residentRingIn (threats: Threats) (room: string) : Set<RoomPos> option =
        Map.tryFind room threats.ResidentRing

    /// The ranger's Work Area in one harassment room, or None for any other
    /// room.
    let harassRingIn (threats: Threats) (room: string) : Set<RoomPos> option =
        Map.tryFind room threats.HarassRing

    /// A Guard's ground in one room, or None where the room has none and the
    /// pool falls back to the source ring (#366): the ranger's kite ground
    /// (#451), else the resident or harassment ground, else the threats' ring.
    /// A ranger room's Guard is a ranger's alone, so its kite ground is never
    /// handed to a melee guard.
    let guardGroundIn (threats: Threats) (room: string) : Set<RoomPos> option =
        Map.tryFind room threats.Kite
        |> Option.orElse (residentRingIn threats room)
        |> Option.orElse (harassRingIn threats room)
        |> Option.orElse (Some(ringIn threats room) |> Option.filter (Set.isEmpty >> not))

    /// The role a body holds a room's Fight in, or None where the Fight does
    /// not admit it.
    let fightRoleOf (threats: Threats) (room: string) (creep: string) : SquadRole option =
        Map.tryFind room threats.Fight
        |> Option.bind (fun ground -> Map.tryFind creep ground.Roles)

    /// One member's ground in a Fight's room, by its role; empty for a room
    /// with no Fight ground or a body it does not admit.
    let fightGroundIn (threats: Threats) (room: string) (creep: string) : Set<RoomPos> =
        match Map.tryFind room threats.Fight, fightRoleOf threats room creep with
        | Some ground, Some Medic -> ground.Behind
        | Some ground, Some Kiter -> ground.Ranged
        | Some ground, Some Brawler -> ground.Front
        | _ -> Set.empty

    /// Whether a room's squad has launched.
    let fightLaunchedIn (threats: Threats) (room: string) : bool =
        Map.tryFind room threats.Fight |> Option.exists (fun ground -> ground.Launched)

    /// The rooms a Fight is pooled for.
    let fightRooms (threats: Threats) : Set<string> = threats.Fight |> Map.keys |> Set.ofSeq

    /// The role a cast holds a room's Assault in, or None where the Assault
    /// does not admit it.
    let assaultRoleOf (threats: Threats) (room: string) (creep: string) : SquadRole option =
        Map.tryFind room threats.Assault
        |> Option.bind (fun ground -> Map.tryFind creep ground.Roles)

    /// One member's ground in an Assault's room, by its role; empty for a
    /// room with no Assault ground or a body it does not admit.
    let assaultGroundIn (threats: Threats) (room: string) (creep: string) : Set<RoomPos> =
        match Map.tryFind room threats.Assault, assaultRoleOf threats room creep with
        | Some ground, Some Medic -> ground.Behind
        | Some ground, Some _ -> ground.Front
        | _ -> Set.empty

/// The kill order's head among one room's targets (`Facts.killRank`), ties by
/// id; each target's rank priced once, not once a comparison.
let private killHead (view: ColonyView) (room: string) (targets: HostileInfo list) =
    match targets with
    | [] -> None
    | _ ->
        let rank = killRank view room

        targets
        |> List.map (fun hostile -> (rank hostile, hostile.Id), hostile)
        |> List.minBy fst
        |> snd
        |> Some

// The ranger's ground under a raid (#451). Engine actions resolve on the
// tick's starting tiles and a melee body must start adjacent to swing, so a
// ranger ending each tick two clear of every melee body is never hit while it
// has a free step; a swamp tile costs it that step.
//
// A melee body hits what ends a tick within a step and a swing of it, and on
// swamp within a few tiles of one the ranger loses the step it would have
// stepped away with.
let private stepAndSwing = Engine.meleeRange + 1
let private swampReach = Engine.meleeRange + 3

/// The tiles of one room within three of the kill order's head that no melee
/// body reaches in a step and a swing, less swamp near one: the kite ground.
let private kiteGroundIn
    atlas
    (room: string)
    (inRoom: HostileInfo list)
    (head: HostileInfo option)
    =
    let melee =
        inRoom
        |> List.filter (fun hostile -> HostileInfo.activeCount hostile Attack > 0)
        |> List.map (fun hostile -> RoomPos.pos hostile.Pos)

    let standing =
        inRoom |> List.map (fun hostile -> RoomPos.pos hostile.Pos) |> Set.ofList

    let clearOfMelee reach (tile: Pos) =
        melee |> List.forall (fun m -> range m tile > reach)

    match head with
    | None -> Set.empty
    | Some target ->
        Atlas.walkableWithinIn atlas room Engine.rangedRange (RoomPos.pos target.Pos)
        |> List.filter (fun tile ->
            not (Set.contains tile standing)
            && clearOfMelee stepAndSwing tile
            && not (Atlas.isSwampIn atlas room tile && not (clearOfMelee swampReach tile)))
        |> Set.ofList
        |> RoomPos.setAt room

/// This tick's Threats, off the view's hostiles and the rampart census, room by
/// room: weapon range plus the margin in Chebyshev tiles, less every tile under
/// one of our standing ramparts in that room. The held facts say which rangers
/// hold each room's Guard, which an outmatched room's safe ground is laid for.
let private threatsHeldBy (view: ColonyView) atlas (held: HeldTaskFacts) : Threats =
    // Under safe mode a hostile in a room of ours can hurt nothing, so it is no
    // Threat and has no Reach.
    let shielded room =
        match Map.tryFind room view.RoomControl with
        | Some control -> control.Owner = Ownership.Ours && control.SafeMode
        | None -> false

    // The hostiles are asked first, so a quiet colony walks nothing for its
    // Threats: neither the rampart census nor any room's own tiles are read on
    // a tick with nothing in it to run from. Its errand rooms' ground, below,
    // is a ring of eight tiles a Reactor.
    let armed =
        match
            view.Hostiles
            |> List.filter (fun hostile -> not (shielded hostile.Pos.Room))
            |> List.choose (fun hostile ->
                weaponRange hostile
                |> Option.map (fun r -> hostile.Pos.Room, RoomPos.pos hostile.Pos, r))
        with
        | [] -> noThreats
        | threats ->
            // The tick's Threats partitioned by the room they stand in, once: all
            // three maps below are keyed on this one room set, and derived from
            // three separate regroupings nothing said so.
            let byRoom = threats |> List.groupBy (fun (room, _, _) -> room)

            let reach =
                byRoom
                |> List.choose (fun (room, inRoom) ->
                    let ramparts = Atlas.ourRampartTilesIn atlas room

                    let tiles =
                        inRoom
                        |> List.collect (fun (_, pos, weapon) ->
                            let r = weapon + view.Tuning.ReachMargin

                            pos
                            |> tilesWithin r
                            |> List.filter (fun tile -> not (Set.contains tile ramparts)))
                        |> Set.ofList

                    // Nothing left to run from once our own ramparts have taken
                    // the whole Reach back: no Reach, no entry, no safe set to
                    // derive.
                    if Set.isEmpty tiles then None else Some(room, tiles))
                |> Map.ofList

            // Less the tiles the Threats stand on: with two of them adjacent each
            // is a tile of the other's ring. Off the Threat list and not off the
            // Reach above, because the Reach has our ramparts subtracted and a
            // rampart is the best tile a guard can fight from.
            let ring =
                byRoom
                |> List.map (fun (room, inRoom) ->
                    let standing = inRoom |> List.map (fun (_, pos, _) -> pos) |> Set.ofList

                    // Asked of the grid a tile at a time rather than against the
                    // room's materialised walkable ground (#371): a ring is nine
                    // tiles a Threat and the ground is two thousand.
                    let tiles =
                        inRoom
                        |> List.collect (fun (_, pos, _) ->
                            Atlas.adjacentWalkableIn atlas room pos
                            |> List.filter (fun tile -> not (Set.contains tile standing)))
                        |> Set.ofList

                    room, RoomPos.setAt room tiles)
                |> Map.ofList

            {
                Reach = reach
                Safe =
                    reach
                    |> Map.map (fun room tiles -> lazy (Atlas.walkableTilesExcept atlas room tiles))
                Ring = ring
                ResidentRing = Map.empty
                HarassRing = Map.empty
                Kite = Map.empty
                Held = Set.empty
                Fight = Map.empty
                Assault = Map.empty
            }

    // The errand rooms' ranger ground (#414, #411): the Reactor's own ring,
    // raid or none. The ranger holding it shoots out to three, which covers a
    // claimer at the Reactor and any body close enough to shoot back.
    let ringAround room pos =
        room, Atlas.adjacentWalkableIn atlas room pos |> Set.ofList |> RoomPos.setAt room

    let errandRing =
        view.Errands
        |> List.map (fun errand -> ringAround errand.RoomName (RoomPos.pos (snd errand.Target)))

    // A raised home's (#447): its controller's ring, which a declared
    // perimeter seals inside (#446) — the one tile the claim lives or dies by,
    // and what a rival's tapper walks to. In peace only: with an armed Threat
    // in the home there is no resident ground, and the garrison takes the
    // Threats' ring as #428's guard did, or its `Kite` ground (#451).
    let raised =
        raisedHomes view
        |> Set.filter (fun room -> not (Map.containsKey room armed.Ring))

    let homeRing =
        if Set.isEmpty raised then
            []
        else
            Atlas.idsOfKind atlas Controller
            |> List.choose (Atlas.positionOf atlas)
            |> List.filter (fun tile -> Set.contains tile.Room raised)
            |> List.map (fun tile -> ringAround tile.Room (RoomPos.pos tile))

    let residentRing = errandRing @ homeRing |> Map.ofList

    let resident = residentRooms view

    // A work spot is where the enemy's bodies must stop: within two of the
    // source, where a hauler draws from the miner's pile, and around the
    // controller, within three because a reserver shuffles about it. The
    // source's come first, and while an enemy miner is anywhere in the room
    // the ranger holds the Stand's seats rather than turn to the controller:
    // the miner steps just out of reach and back, and ground that followed
    // it off the source turned the ranger between it and the reserver, out
    // of reach of both (W18S27, t840,6xx-8xx). The seats are within reach of
    // every work spot of the source.
    let harassRing =
        view.Harass
        |> List.map (fun h ->
            let room = h.RoomName

            let inRoom = view.Hostiles |> List.filter (fun hostile -> hostile.Pos.Room = room)

            let within (spot: RoomPos) reach (hostile: HostileInfo) =
                RoomPos.range spot hostile.Pos |> Option.exists (fun r -> r <= reach)

            let armedTargets, unarmedTargets =
                inRoom
                |> List.filter (guardShoots view resident Set.empty)
                |> List.partition isArmed

            let seats = [ 1, RoomPos.pos h.Stand ]

            let working =
                match unarmedTargets |> List.filter (within h.Stand 2) with
                | [] when
                    unarmedTargets |> List.exists (fun hostile -> List.contains Work hostile.Body)
                    ->
                    seats
                | [] ->
                    unarmedTargets
                    |> List.filter (within h.Controller 3)
                    |> List.map (fun hostile -> Engine.rangedRange, RoomPos.pos hostile.Pos)
                | atSource ->
                    atSource
                    |> List.map (fun hostile -> Engine.rangedRange, RoomPos.pos hostile.Pos)

            let ambushed =
                (armedTargets |> List.map (fun hostile -> 1, RoomPos.pos hostile.Pos)) @ working

            let around = if List.isEmpty ambushed then seats else ambushed

            let standing =
                inRoom |> List.map (fun hostile -> RoomPos.pos hostile.Pos) |> Set.ofList

            room,
            around
            |> List.collect (fun (radius, pos) -> Atlas.walkableWithinIn atlas room radius pos)
            |> List.filter (fun tile -> not (Set.contains tile standing))
            |> Set.ofList
            |> RoomPos.setAt room)
        |> Map.ofList

    let rangers = rangerRooms view

    // The ranger rooms an armed raid, or a rival's claimer, stands in.
    let contested =
        view.Hostiles
        |> List.filter (fun hostile ->
            Set.contains hostile.Pos.Room rangers
            && (Map.containsKey hostile.Pos.Room armed.Ring || claimsAFlag resident hostile))
        |> List.map (fun hostile -> hostile.Pos.Room)
        |> List.distinct

    let kite =
        contested
        |> List.choose (fun room ->
            let inRoom = view.Hostiles |> List.filter (fun hostile -> hostile.Pos.Room = room)

            let melee =
                inRoom
                |> List.filter (fun hostile -> HostileInfo.activeCount hostile Attack > 0)
                |> List.map (fun hostile -> RoomPos.pos hostile.Pos)

            let standing =
                inRoom |> List.map (fun hostile -> RoomPos.pos hostile.Pos) |> Set.ofList

            let gap (tile: Pos) =
                melee |> List.map (fun m -> range m tile) |> List.min

            let head =
                inRoom
                |> List.filter (guardShoots view resident Set.empty)
                |> killHead view room

            let aroundHead () = kiteGroundIn atlas room inRoom head

            // Under an armed raid, our standing ramparts within three of the
            // head (#467): a creep on its own rampart takes no damage, so
            // they are safe ground whatever the melee reaches.
            let onRamparts () =
                match head with
                | Some target when Map.containsKey room armed.Ring ->
                    Atlas.ourRampartTilesIn atlas room
                    |> Set.filter (fun tile ->
                        range tile (RoomPos.pos target.Pos) <= Engine.rangedRange
                        && not (Set.contains tile standing))
                    |> RoomPos.setAt room
                | _ -> Set.empty

            // The safe set, two thousand tiles (#371), is the outmatched
            // room's answer alone, and only for a ranger of ours to hold it:
            // one standing there, or one holding its Guard on the way in.
            let rangerHere =
                lazy
                    (let guarding =
                        HeldTaskFacts.holdersOf held (taskId (Guard room)) |> Set.ofList

                     view.Creeps
                     |> List.exists (fun creep ->
                         partCount creep.Body RangedAttack > 0
                         && (Set.contains creep.Name guarding
                             || SpatialInfo.creepPlacementOf view.Spatial creep.Name
                                |> Option.exists (fun tile -> tile.Room = room))))

            let safe () = Threats.safeIn armed room

            // Nowhere to kite and nowhere safe: the room's tiles farthest from
            // the melee, or, where every one is within a step and a swing,
            // the tiles our rangers already hold.
            let farthest () =
                let ground =
                    Atlas.walkableTilesIn atlas room
                    |> Set.filter (fun tile -> not (Set.contains tile standing))

                let held =
                    view.Creeps
                    |> List.filter (fun creep -> partCount creep.Body RangedAttack > 0)
                    |> List.choose (fun creep ->
                        SpatialInfo.creepPlacementOf view.Spatial creep.Name)
                    |> List.filter (fun tile -> tile.Room = room)
                    |> Set.ofList

                if Set.isEmpty ground then
                    held
                else
                    let widest = ground |> Seq.map gap |> Seq.max

                    if widest <= stepAndSwing && not (Set.isEmpty held) then
                        held
                    else
                        ground |> Set.filter (fun tile -> gap tile = widest) |> RoomPos.setAt room

            let claimerLeads = head |> Option.exists (claimsAFlag resident)

            let outmatchedHere = Set.contains room resident && outmatched view room

            [
                onRamparts

                if outmatchedHere then
                    if rangerHere.Value then
                        safe
                elif not (List.isEmpty melee) || claimerLeads then
                    aroundHead
                if not (List.isEmpty melee) && (not outmatchedHere || rangerHere.Value) then
                    farthest
            ]
            |> List.tryPick (fun ground ->
                let tiles = ground ()
                if Set.isEmpty tiles then None else Some(room, tiles)))
        |> Map.ofList

    // The held exits' ground (#450): the tiles within a melee guard's reach of
    // the run its room's last armed Threat left by, less the ring, whose exit
    // tiles the engine carries a body standing on across the border. Only in a
    // room whose Guard has no declared ground: a resident room keeps the ring a
    // CLAIM tapper is stopped at, and a harassment room its ambush. A hold
    // with no ground is no hold.
    let holds =
        view.ExitHolds
        |> Map.toList
        |> List.filter (fun (room, _) ->
            not (
                Map.containsKey room armed.Ring
                || Set.contains room resident
                || Map.containsKey room harassRing
            ))
        |> List.choose (fun (room, hold) ->
            let ground =
                hold.Run
                |> List.collect (Atlas.walkableWithinIn atlas room Engine.meleeRange)
                |> List.filter (Seam.onRing >> not)
                |> Set.ofList

            if Set.isEmpty ground then
                None
            else
                Some(room, RoomPos.setAt room ground))

    // A level-0 core's ring (#487), the tiles a guard swings at it from, in a
    // room no Threat, held exit or declared ground answers for first: the
    // raid is fought before the core.
    let cores =
        Facts.expansionCores view
        |> List.filter (fun core ->
            not (
                Map.containsKey core.RoomName armed.Ring
                || Set.contains core.RoomName resident
                || Map.containsKey core.RoomName harassRing
                || List.exists (fun (room, _) -> room = core.RoomName) holds
            ))
        |> List.map (fun core -> ringAround core.RoomName core.Tile)

    { armed with
        Ring =
            (armed.Ring, holds @ cores)
            ||> List.fold (fun ring (room, ground) ->
                Map.change
                    room
                    (fun prior -> Some(Set.union ground (Option.defaultValue Set.empty prior)))
                    ring)
        ResidentRing = residentRing
        HarassRing = harassRing
        Kite = kite
        Held = holds |> List.map fst |> Set.ofList
    }

/// This tick's Threats as a colony holding nothing would see them: no Fight,
/// and an outmatched room's safe ground only for a ranger standing in it.
let threatsOf (view: ColonyView) atlas : Threats =
    threatsHeldBy view atlas HeldTaskFacts.empty

/// How near the crossing it waits beside the rally ground lies, in tiles:
/// room for a squad on the near side of the exit.
let private rallyReach = 3

/// How far from the leader a member may stand and still be with it.
let private squadSpread = 2

/// How close to a room's border a body in the room beside it stands at its
/// crossing: the exit tile and the one inside it. The rally ground keeps
/// clear of both, so a body there has left it.
let private crossingGap = 1

/// Tiles from a tile to the nearest edge of its room.
let private edgeGap (tile: Pos) =
    min (min tile.X (Engine.roomSide - 1 - tile.X)) (min tile.Y (Engine.roomSide - 1 - tile.Y))

/// The range between two tiles across a border too: a member a step behind
/// its leader over a crossing is beside it. The exit tiles either side of a
/// border are one tile, the engine carrying a body from one to the other, so
/// the rooms overlap by a row. None between rooms that share no border.
let internal rangeAcross (a: RoomPos) (b: RoomPos) : int option =
    if a.Room = b.Room then
        RoomPos.range a b
    else
        let span = Engine.roomSide - 1

        RoomName.offsetOf a.Room b.Room
        |> Option.filter (fun (dx, dy) -> abs dx + abs dy = 1)
        |> Option.map (fun (dx, dy) ->
            max (abs (b.X + span * dx - a.X)) (abs (b.Y + span * dy - a.Y)))

/// Whether a tile stands at the crossing into a room from the room beside it:
/// the room's own nearest tile, across the border, within a step of the exit.
let private atCrossingInto (room: string) (tile: RoomPos) =
    RoomName.offsetOf tile.Room room
    |> Option.filter (fun (dx, dy) -> abs dx + abs dy = 1)
    |> Option.exists (fun (dx, dy) ->
        let across value offset =
            let low = Engine.roomSide * offset
            max 0 (max (low - value) (value - (low + Engine.roomSide - 1)))

        max (across tile.X dx) (across tile.Y dy) <= crossingGap + 1)

/// The crossing a squad musters beside on a chain from home to its target:
/// out of the last room short of the target that is neither a Source Keeper's
/// nor a rival's — home, which is neither, at the last — toward the next room
/// on the chain. None for a chain of one room.
let rallyHop (rivals: Set<string>) (chain: string list) : (string * string) option =
    chain
    |> List.pairwise
    |> List.rev
    |> List.tryFind (fun (from, _) ->
        not (Keepers.isKeeperRoom from) && not (Set.contains from rivals))

/// The ground beside the middle of the crossing from one room into the next,
/// clear of the border and of the room's Reach.
let private besideCrossing atlas (threats: Threats) (from: string) (into: string) =
    match Atlas.seams atlas from into |> List.map fst with
    | [] -> Set.empty
    | exits ->
        let middle = List.item (List.length exits / 2) exits
        let reach = Threats.reachIn threats from

        Atlas.walkableWithinIn atlas from rallyReach middle
        |> List.filter (fun tile -> edgeGap tile > crossingGap && not (Set.contains tile reach))
        |> Set.ofList
        |> RoomPos.setAt from

/// A squad's rally ground on its chain from home (`rallyHop`); empty where no
/// chain reaches its room.
let private rallyOn (view: ColonyView) atlas (threats: Threats) (chain: string list option) =
    chain
    |> Option.bind (rallyHop view.Spatial.RivalRooms)
    |> Option.map (fun (from, into) -> besideCrossing atlas threats from into)
    |> Option.defaultValue Set.empty

/// A squad's casts, the front leading: a brawler, else a sapper, else a
/// kiter, else whoever holds.
let private frontFirst (members: (string * SquadRole) list) =
    members
    |> List.sortBy (fun (name, role) ->
        (match role with
         | Brawler -> 0
         | Sapper -> 1
         | Kiter -> 2
         | Medic -> 3),
        name)

/// Whether each cast stands on the rally ground or with the squad: the
/// others within two of the leader, and the leader within two of one of
/// them. A leader is never with the squad by standing on its own tile, so a
/// recast front far off the rally ground is not.
let private together (view: ColonyView) (rally: Set<RoomPos>) (members: (string * SquadRole) list) =
    let tileOf name =
        SpatialInfo.creepPlacementOf view.Spatial name

    let leader = frontFirst members |> List.tryHead |> Option.map fst

    members
    |> List.forall (fun (name, _) ->
        tileOf name
        |> Option.exists (fun tile ->
            Set.contains tile rally
            || members
               |> List.exists (fun (other, _) ->
                   other <> name
                   && (Some name = leader || Some other = leader)
                   && tileOf other
                      |> Option.bind (rangeAcross tile)
                      |> Option.exists (fun r -> r <= squadSpread))))

/// Whether every cast stands in the room or at its crossing: a front chasing
/// its target, or carried over the border off an exit tile, is not called
/// back to the rally ground.
let private entered (view: ColonyView) (room: string) (members: (string * SquadRole) list) =
    members
    |> List.forall (fun (name, _) ->
        SpatialInfo.creepPlacementOf view.Spatial name
        |> Option.exists (fun tile -> tile.Room = room || atCrossingInto room tile))

/// Every squad cast of this colony, by the role its name carries.
let private squadCasts (view: ColonyView) : Map<string, SquadRole> =
    view.Creeps
    |> List.choose (fun creep ->
        squadRoleByName creep.Name |> Option.map (fun role -> creep.Name, role))
    |> Map.ofList

/// The tick's Threats with each pooled Fight's ground on it
/// (`HeldTaskFacts.Fights`), its squad whoever holds the room's Fight. A
/// launched squad's room keeps its residents on the kite ground.
let private withFights (view: ColonyView) atlas (held: HeldTaskFacts) (threats: Threats) : Threats =
    if Map.isEmpty held.Fights then
        threats
    else
        let home = SpatialInfo.homeName view.Spatial
        let resident = residentRooms view
        let fighting = held.Fights |> Map.keys |> Set.ofSeq

        let tileOf name =
            SpatialInfo.creepPlacementOf view.Spatial name

        let bodies =
            view.Creeps |> List.map (fun creep -> creep.Name, creep.Body) |> Map.ofList

        let casts = squadCasts view

        let rallyFor room =
            rallyOn view atlas threats (Atlas.route atlas home room)

        let groundOf room (squad: Squad) =
            let slots = squadRoles squad
            let tid = taskId (Fight room)

            // The casts holding the room's Fight, by role.
            let members =
                HeldTaskFacts.holdersOf held tid
                |> List.choose (fun name ->
                    Map.tryFind name casts |> Option.map (fun role -> name, role))
                |> List.sort

            // The room's residents whose parts fit one of its slots.
            let residents =
                HeldTaskFacts.holdersOf held (taskId (Guard room))
                @ HeldTaskFacts.holdersOf held tid
                |> List.filter (fun name -> not (Map.containsKey name casts))
                |> List.choose (fun name ->
                    Map.tryFind name bodies
                    |> Option.bind squadRoleOfParts
                    |> Option.filter (fun role -> List.contains role slots)
                    |> Option.map (fun role -> name, role))
                |> Map.ofList

            let inRoom = view.Hostiles |> List.filter (fun h -> h.Pos.Room = room)
            let targets = inRoom |> List.filter (guardShoots view resident fighting)
            let head = killHead view room targets
            let rally = rallyFor room

            // Every slot filled, by a cast holding the Fight or a resident
            // that fits it.
            let filled =
                SquadRole.all
                |> List.forall (fun role ->
                    SquadRole.slots role (List.map snd members)
                    + (residents |> Map.filter (fun _ fits -> fits = role) |> Map.count)
                    >= SquadRole.slots role slots)

            let ranked = frontFirst members

            // Never with a slot empty: a squad that loses a member, or whose
            // replacement is on its way, waits on the rally ground until it is
            // whole again. Gone in, it is held there while every cast stands
            // in the room or at its crossing.
            let launched =
                not (Set.isEmpty rally)
                && not (List.isEmpty members)
                && filled
                && (together view rally members || entered view room members)

            // The tiles beside a tile no hostile stands on, in its room.
            let besideIn (tile: RoomPos) =
                let taken =
                    view.Hostiles
                    |> List.filter (fun h -> h.Pos.Room = tile.Room)
                    |> List.map (fun h -> RoomPos.pos h.Pos)
                    |> Set.ofList

                Atlas.adjacentWalkableIn atlas tile.Room (RoomPos.pos tile)
                |> List.filter (fun pos -> not (Set.contains pos taken))
                |> List.map (RoomPos.at tile.Room)

            // The head's ring, and the tile of every brawler of ours already
            // swinging at a target or lamed: a body that can no longer walk
            // is not walked off the fight, where its medic heals its legs back.
            let swinging =
                members
                |> List.filter (snd >> (=) Brawler)
                |> List.choose (fun (name, _) ->
                    tileOf name |> Option.map (fun tile -> tile, Map.tryFind name bodies))
                |> List.filter (fun (tile, body) ->
                    body |> Option.exists (fun parts -> partCount parts Move = 0)
                    || targets
                       |> List.exists (fun h ->
                           RoomPos.range tile h.Pos
                           |> Option.exists (fun r -> r <= Engine.meleeRange)))
                |> List.map fst
                |> Set.ofList

            // With no target in the room, its resident ring — the
            // controller's of a raised home, the Reactor's of an errand room
            // — which the squad holds while the Fight stays pooled.
            let front =
                match head with
                | Some target -> besideIn target.Pos |> Set.ofList |> Set.union swinging
                | None ->
                    Threats.residentRingIn threats room
                    |> Option.defaultWith (fun () -> Threats.ringIn threats room)

            // Behind the leader, out of every melee body's swing where the
            // ground allows: the raid then strikes the brawler, whose ATTACK
            // strikes back, and not the medic.
            let behind =
                // The member a medic walks behind: the leading cast that is no
                // medic.
                match ranked |> List.tryFind (snd >> (<>) Medic) |> Option.bind (fst >> tileOf) with
                | Some tile ->
                    // Clear of every melee body by `reach`: past a step and a
                    // swing first, so one that closes this tick cannot strike
                    // the next, then past a swing.
                    let clearBy reach (pos: RoomPos) =
                        view.Hostiles
                        |> List.forall (fun h ->
                            HostileInfo.activeCount h Attack = 0
                            || RoomPos.range pos h.Pos |> Option.forall (fun r -> r > reach))

                    let around = besideIn tile

                    [ Engine.meleeRange + 1; Engine.meleeRange ]
                    |> List.tryPick (fun reach ->
                        match around |> List.filter (clearBy reach) with
                        | [] -> None
                        | clear -> Some(Set.ofList clear))
                    |> Option.defaultValue (Set.ofList around)
                // No one to walk behind: never the front.
                | None -> rally

            let kite =
                match kiteGroundIn atlas room inRoom head with
                | tiles when Set.isEmpty tiles ->
                    Map.tryFind room threats.Kite |> Option.defaultValue front
                | tiles -> tiles

            let launchedOr ground = if launched then ground else rally

            {
                Squad = squad
                Launched = launched
                Rally = rally
                Front = launchedOr front
                Behind = launchedOr behind
                Ranged = launchedOr kite
                Roles =
                    if launched then
                        Map.fold (fun roles name role -> Map.add name role roles) casts residents
                    else
                        casts
                Residents = residents
            }

        let grounds = held.Fights |> Map.map groundOf

        // A launched squad's residents that fit no slot hold the kite ground
        // beside it, as its kiters do, and never the safe set.
        let kite =
            (threats.Kite, grounds)
            ||> Map.fold (fun kite room ground ->
                if ground.Launched && not (Set.isEmpty ground.Ranged) then
                    Map.add room ground.Ranged kite
                else
                    kite)

        { threats with
            Fight = grounds
            Kite = kite
        }

/// The share of its hits under which a launched assault's cast sends its
/// squad back to rally, and the share every cast must hold to launch from
/// there: the gap is what its medics heal at rally, so a squad that turned
/// back at one does not walk straight back in.
let private assaultRetreatShare = 0.5

let private assaultLaunchShare = 0.8

/// The tick's Threats with each Assault's ground on it (`ColonyView.Assaults`):
/// a Fight's rally and launch over the casts holding it, kept whole on the
/// walk in.
let private withAssaults
    (view: ColonyView)
    atlas
    (held: HeldTaskFacts)
    (threats: Threats)
    : Threats =
    match view.Assaults with
    | [] -> threats
    | assaults ->
        let home = SpatialInfo.homeName view.Spatial
        let casts = squadCasts view

        let tileOf name =
            SpatialInfo.creepPlacementOf view.Spatial name

        let share =
            view.Creeps
            |> List.map (fun creep ->
                creep.Name, float creep.Hits.Hits / float (max 1 creep.Hits.HitsMax))
            |> Map.ofList

        // The walkable tiles beside a tile, in its room.
        let besideIn (tile: RoomPos) =
            Atlas.adjacentWalkableIn atlas tile.Room (RoomPos.pos tile)
            |> List.map (RoomPos.at tile.Room)
            |> Set.ofList

        let groundOf (facts: AssaultFacts) =
            let room = facts.Assault.RoomName
            let slots = facts.Assault.Squad |> List.choose squadRoleOfRow

            // The casts holding the room's Assault, by role.
            let members =
                HeldTaskFacts.holdersOf held (taskId (Assault room))
                |> List.choose (fun name ->
                    Map.tryFind name casts |> Option.map (fun role -> name, role))
                |> List.sort

            let rally = rallyOn view atlas threats (Atlas.siegeRoute atlas home room)

            let filled =
                SquadRole.all
                |> List.forall (fun role ->
                    SquadRole.slots role (List.map snd members) >= SquadRole.slots role slots)

            let inside = entered view room members

            // Hurt: under half once in, so the squad turns back; under four
            // fifths at rally, so it waits for its medics.
            let fit =
                let least = if inside then assaultRetreatShare else assaultLaunchShare

                members
                |> List.forall (fun (name, _) ->
                    Map.tryFind name share |> Option.exists (fun s -> s >= least))

            let launched =
                not (Set.isEmpty rally)
                && not (List.isEmpty members)
                && filled
                && fit
                && not facts.SafeMode
                && (together view rally members || inside)

            let breach = facts.Assault.Breach |> List.map (RoomPos.at room)

            // The target: vision's head, else the first declared breach tile
            // while the room is dark.
            let target =
                match facts.Targets with
                | Some targets ->
                    targets
                    |> List.tryHead
                    |> Option.map (fun (id, tile) -> id, RoomPos.at room tile)
                | None -> None

            let front =
                match target, facts.Targets with
                | Some(_, tile), _ -> besideIn tile
                | None, None ->
                    breach |> List.tryHead |> Option.map besideIn |> Option.defaultValue Set.empty
                // Nothing left standing: a `Provoke` holds the breach it made,
                // and a `Strike` the last one too.
                | None, Some _ ->
                    breach
                    |> List.tryLast
                    |> Option.map (fun tile -> Set.add tile (besideIn tile))
                    |> Option.defaultValue Set.empty

            let leader =
                frontFirst members
                |> List.tryFind (snd >> (<>) Medic)
                |> Option.bind (fun (name, _) -> tileOf name |> Option.map (fun tile -> name, tile))

            // Strung out under fire, the squad's medics cannot reach the body
            // the towers pick: every cast within two of the leader, or the
            // leader holds its tile and the rest close on it.
            let strung =
                match leader with
                | Some(name, at) ->
                    members
                    |> List.exists (fun (other, _) ->
                        other <> name
                        && tileOf other
                           |> Option.bind (rangeAcross at)
                           |> Option.forall (fun r -> r > squadSpread))
                | None -> false

            let front =
                match leader with
                | Some(_, at) when strung -> Set.add at (besideIn at)
                | _ -> front

            // Beside the leading sapper, off the front it works from.
            let behind =
                match leader with
                | Some(_, tile) ->
                    match Set.difference (besideIn tile) front with
                    | clear when Set.isEmpty clear -> besideIn tile
                    | clear -> clear
                | None -> rally

            let launchedOr ground = if launched then ground else rally

            {
                Slots = slots
                Launched = launched
                Rally = rally
                Target = target
                Front = launchedOr front
                Behind = launchedOr behind
                Roles = casts |> Map.filter (fun _ role -> List.contains role slots)
            }

        { threats with
            Assault =
                assaults
                |> List.map (fun facts -> facts.Assault.RoomName, groundOf facts)
                |> Map.ofList
        }

/// The tick's Threats off what the colony holds (`heldTaskFacts`): an
/// outmatched room's safe ground for every ranger holding its Guard, wherever
/// it stands, each pooled Fight's ground and each Assault's.
let threatsOfHeld (view: ColonyView) atlas (held: HeldTaskFacts) : Threats =
    threatsHeldBy view atlas held
    |> withFights view atlas held
    |> withAssaults view atlas held
