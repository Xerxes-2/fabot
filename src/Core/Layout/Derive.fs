/// The Layout's facts, derived once a tick (`ARCHITECTURE.md` §1): for each
/// room whose controller is ours, its level, its standing plan (redrawn only
/// when its key moves), its census, and what its towers see. The decisions
/// (`Run`) read these and never `World`.
module Fabot.Core.Layout.Derive

open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Vocabulary
open Fabot.Core.Layout.Shape

/// One colony room as the Layout decides over it.
[<NoEquality; NoComparison>]
type Colony =
    {
        Room: string
        Level: int
        /// None for a room that offers no anchor.
        Plan: Plan option
        Census: Sites.Census
        /// Our towers holding the energy for an act, by id.
        Towers: Structure[]
        /// Every creep not ours standing in the room.
        Hostiles: Creep[]
        /// Our creeps standing in the room.
        Ours: Creep[]
    }

/// The tiles of our spawns standing in a room, ascending.
let private spawnsIn (world: World) (room: string) : int[] =
    world.Spawns
    |> Array.filter (fun s -> s.Pos.Room = room)
    |> Array.map (fun s -> Tiles.ofPos s.Pos)
    |> Array.sort

/// Everything a room's plan reads that can change: our spawns' tiles (they
/// choose the anchor and order its spawn slots) and the obstacles not ours
/// (`Ground.obstacles`). Terrain, sources, minerals and the controller never
/// move.
let keyOf (room: Room) (spawns: int[]) : string =
    System.String.Join(",", spawns)
    + "|"
    + System.String.Join(",", Ground.obstacles room)

/// The colonies this tick and the standing plans carried on, each room's
/// plan kept while its key holds and redrawn when it moves; rooms no longer
/// ours are dropped.
let colonies (world: World) (standing: Map<string, Standing>) : Colony[] * Map<string, Standing> =
    let mutable next = Map.empty
    let found = ResizeArray<Colony>()

    for room in world.Rooms |> Array.sortBy (fun r -> r.Name) do
        match room.Controller with
        | Some c when c.Owner = Ours ->
            let spawns = spawnsIn world room.Name
            let key = keyOf room spawns

            let kept =
                match Map.tryFind room.Name standing with
                | Some s when s.Key = key -> s
                | _ ->
                    {
                        Key = key
                        Plan =
                            Ground.ofRoom room
                            |> Option.bind (fun ground -> Plan.ofGround ground spawns)
                    }

            next <- Map.add room.Name kept next

            let ours = world.Creeps |> Array.filter (fun creep -> creep.Pos.Room = room.Name)

            found.Add
                {
                    Room = room.Name
                    Level = c.Level
                    Plan = kept.Plan
                    Census = Sites.censusOf room ours
                    Towers =
                        room.Structures
                        |> Array.filter (fun s ->
                            s.Kind = Kind.Tower
                            && s.Owner = Ours
                            && Store.amountOf Energy s.Store >= Engine.towerEnergyCost)
                        |> Array.sortBy (fun t -> t.Id)
                    Hostiles = room.Hostiles
                    Ours = ours
                }
        | _ -> ()

    found.ToArray(), next
