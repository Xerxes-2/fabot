/// The tick's threat facts (ADR 0033): the tiles a hostile can reach, layered by
/// room name (ADR 0041). Colony facts and never a change to the spatial
/// projection — the three readers below take their Reach from here.
[<AutoOpen>]
module Fabot.Core.Decide.Threat

open Fabot.Core
open Fabot.Core.Types

/// The tick's colony-level threat facts (ADR 0033): the tiles a Threat can
/// hurt, and the walkable tiles no Threat can. Derived once a tick and shared
/// by the three readers — the applicability gate that takes the Reach out of
/// every Work Area, Flee's own Work Area, and the spawn hold. Colony facts,
/// never a change to the spatial projection: hostiles still block no tiles and
/// price no paths. Layered by room name, as the projection they are derived
/// from is (ADR 0041): a Reach is a set of one room's tiles and a `Set<Pos>`
/// cannot say which room's, so the room rides on the outer key — the room the
/// hostile stands in, which `HostileInfo` carries for exactly this join. Each
/// reader picks its own room's share, and a room with no entry answers the
/// empty set (ADR 0004): it blocks no action and pools no Flee.
type Threats =
    {
        /// Per room, the tiles a Threat standing in it can hurt. Never an
        /// empty set under a room: a room whose whole Reach our ramparts
        /// took back has no entry, so `Map.isEmpty` is "no Reach
        /// anywhere" — the one question the pool asks of it.
        Reach: Map<string, Set<Pos>>
        /// Per room, the walkable tiles no Threat reaches — Flee's Work Area
        /// for a creep standing there. Derived only for the rooms with a Reach,
        /// where every other room's absence stands for "not derived" rather
        /// than "nowhere is safe": a creep with no Reach around it is matched
        /// to no Flee.
        Safe: Map<string, Set<RoomPos>>
        /// Per room, the walkable tiles within range 1 of a Threat standing in
        /// it, less the tiles the Threats themselves stand on — the [[guard]]'s
        /// Work Area (ADR 0056), and the safe set's exact opposite: Flee walks
        /// a body to the tiles nothing reaches and a Guard walks it to the
        /// tiles that reach *back*. Range 1 and not two, because 30 a part is
        /// paid there and nothing is paid at range 2 and the row carries no
        /// RANGED_ATTACK by decision. Derived here beside the Reach and
        /// touching no [[atlas]] memo, which is ADR 0033's precedent for the
        /// safe set pointed the other way.
        Ring: Map<string, Set<RoomPos>>
    }

/// The tick with nothing to run from: every Work Area stands whole and no
/// creep flees. What the pipeline is handed for a quiet colony.
let noThreats =
    {
        Reach = Map.empty
        Safe = Map.empty
        Ring = Map.empty
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Threats =
    /// One room's Reach; empty for a room no Threat stands in (ADR 0004).
    let reachIn (threats: Threats) (room: string) : Set<Pos> =
        Map.tryFind room threats.Reach |> Option.defaultValue Set.empty

    /// One room's safe set, already joined to that room; empty for a room
    /// no Reach was derived in.
    let safeIn (threats: Threats) (room: string) : Set<RoomPos> =
        Map.tryFind room threats.Safe |> Option.defaultValue Set.empty

    /// One room's range-1 ring, already joined to that room; empty for a room
    /// no Threat stands in — which leaves that room's Guard, if one was ever
    /// pooled for it, applicable to nobody (ADR 0004).
    let ringIn (threats: Threats) (room: string) : Set<RoomPos> =
        Map.tryFind room threats.Ring |> Option.defaultValue Set.empty

/// This tick's Threats, off the view's hostiles and the rampart census, room by
/// room. Each Threat reaches its weapon range plus the margin, in Chebyshev
/// tiles — less every tile under one of our standing ramparts in that same
/// room, which is in no Reach at all: a creep on its own rampart cannot be
/// attacked, and that exemption is what lets an Anchor keep digging on a
/// ramparted Post (ADR 0034).
let threatsOf (view: ColonyView) atlas : Threats =
    // Under safe mode a hostile in a room of ours can hurt nothing — the engine
    // refuses every harmful act there for the whole window — so it is no Threat
    // and has no Reach.
    let shielded room =
        match Map.tryFind room view.RoomControl with
        | Some control -> control.Owner = Ownership.Ours && control.SafeMode
        | None -> false

    // The hostiles are asked first, so a quiet colony walks nothing:
    // neither the rampart census nor any room's own tiles are read on a
    // tick with nothing in it to run from.
    match
        view.Hostiles
        |> List.filter (fun hostile -> not (shielded hostile.Pos.Room))
        |> List.choose (fun hostile ->
            weaponRange hostile
            |> Option.map (fun r -> hostile.Pos.Room, RoomPos.pos hostile.Pos, r))
    with
    | [] -> noThreats
    | threats ->
        let reach =
            threats
            |> List.groupBy (fun (room, _, _) -> room)
            |> List.choose (fun (room, inRoom) ->
                let ramparts = Atlas.ourRampartTilesIn atlas room

                let tiles =
                    inRoom
                    |> List.collect (fun (_, pos, weapon) ->
                        let r = weapon + view.Tuning.ReachMargin

                        [
                            for x in pos.X - r .. pos.X + r do
                                for y in pos.Y - r .. pos.Y + r do
                                    let tile = { X = x; Y = y }

                                    if not (Set.contains tile ramparts) then
                                        tile
                        ])
                    |> Set.ofList

                // Nothing left to run from once our own ramparts have taken
                // the whole Reach back: no Reach, no entry, no safe set to
                // derive.
                if Set.isEmpty tiles then None else Some(room, tiles))
            |> Map.ofList

        // The walkable ground of each room a Threat stands in, walked **once**
        // for the two sets derived from it: the safe set is that ground less
        // the Reach and the ring is the part of it beside a Threat, and
        // `Atlas.walkableTilesIn` builds a 2,500-tile set per call and
        // memoises nothing.
        let walkable =
            threats
            |> List.map (fun (room, _, _) -> room)
            |> List.distinct
            |> List.map (fun room -> room, Atlas.walkableTilesIn atlas room)
            |> Map.ofList

        let walkableIn room =
            Map.tryFind room walkable |> Option.defaultValue Set.empty

        // The range-1 ring of every Threat in a room, walkable and less the
        // tiles the Threats stand on — a body cannot stand where one of them
        // already does, and with two of them adjacent each is a tile of the
        // other's ring. Off the Threat list and not off the Reach above,
        // because the Reach is what our own ramparts subtract from and a
        // rampart is standing room like any other: the tile a guard fights
        // from is the best tile it has, not one it has to flee.
        let ring =
            threats
            |> List.groupBy (fun (room, _, _) -> room)
            |> List.map (fun (room, inRoom) ->
                let standing = inRoom |> List.map (fun (_, pos, _) -> pos) |> Set.ofList
                let walkable = walkableIn room

                let tiles =
                    inRoom
                    |> List.collect (fun (_, pos, _) ->
                        [
                            for x in pos.X - 1 .. pos.X + 1 do
                                for y in pos.Y - 1 .. pos.Y + 1 do
                                    let tile = { X = x; Y = y }

                                    if
                                        Set.contains tile walkable
                                        && not (Set.contains tile standing)
                                    then
                                        tile
                        ])
                    |> Set.ofList

                room, RoomPos.setAt room tiles)
            |> Map.ofList

        {
            Reach = reach
            Safe =
                reach
                |> Map.map (fun room tiles ->
                    Set.difference (walkableIn room) tiles |> RoomPos.setAt room)
            Ring = ring
        }
