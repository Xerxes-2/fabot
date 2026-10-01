/// The tick's threat facts: the tiles a hostile can reach, layered by room
/// name. Colony facts, never a change to the spatial projection.
[<AutoOpen>]
module Fabot.Core.Decide.Threat

open Fabot.Core
open Fabot.Core.Types

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
        /// The rooms whose `Ring` is a held exit's ground (#450): where the
        /// Planner keeps a living guard's Guard pooled, and hires for none.
        Held: Set<string>
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
        Held = Set.empty
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

/// This tick's Threats, off the view's hostiles and the rampart census, room by
/// room: weapon range plus the margin in Chebyshev tiles, less every tile under
/// one of our standing ramparts in that room.
let threatsOf (view: ColonyView) atlas : Threats =
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
                Held = Set.empty
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
    // Threats' ring as #428's guard did.
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
                inRoom |> List.filter (guardShoots view resident) |> List.partition isArmed

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

    { armed with
        Ring =
            (armed.Ring, holds)
            ||> List.fold (fun ring (room, ground) -> Map.add room ground ring)
        ResidentRing = residentRing
        HarassRing = harassRing
        Held = holds |> List.map fst |> Set.ofList
    }
