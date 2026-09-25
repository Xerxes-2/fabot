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
        /// Work Area.
        Ring: Map<string, Set<RoomPos>>
        /// Per declared errand room (#414), the ranger's Work Area there: the
        /// Reactor's own ring, which it holds and shoots from (#411).
        ErrandRing: Map<string, Set<RoomPos>>
    }

/// The tick with nothing to run from: what the pipeline is handed for a quiet
/// colony.
let noThreats =
    {
        Reach = Map.empty
        Safe = Map.empty
        Ring = Map.empty
        ErrandRing = Map.empty
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

    /// The ranger's Work Area in one declared errand room, or None for any
    /// other room (#414).
    let errandRingIn (threats: Threats) (room: string) : Set<RoomPos> option =
        Map.tryFind room threats.ErrandRing

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
                    |> Map.map (fun room tiles ->
                        lazy
                            (Set.difference (Atlas.walkableTilesIn atlas room) tiles
                             |> RoomPos.setAt room))
                Ring = ring
                ErrandRing = Map.empty
            }

    // The errand rooms' ranger ground (#414, #411): the Reactor's own ring,
    // raid or none. The ranger holding it shoots out to three, which covers a
    // claimer at the Reactor and any body close enough to shoot back.
    let errandRing =
        view.Errands
        |> List.map (fun errand ->
            let room = errand.RoomName

            room,
            Atlas.adjacentWalkableIn atlas room (RoomPos.pos (snd errand.Target))
            |> Set.ofList
            |> RoomPos.setAt room)
        |> Map.ofList

    { armed with ErrandRing = errandRing }
