module Fabot.Core.Layout.Derive

open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Vocabulary
open Fabot.Core.Layout.Shape

[<NoEquality; NoComparison>]
type Colony =
    {
        Room: string
        Level: int
        Plan: Plan option
        Census: Sites.Census
        Towers: Structure[]
        Hostiles: Creep[]
        Ours: Creep[]
    }

let private spawnsIn (world: World) (room: string) : int[] =
    world.Spawns
    |> Array.filter (fun s -> s.Pos.Room = room)
    |> Array.map (fun s -> Tiles.ofPos s.Pos)
    |> Array.sort

/// Terrain, sources, minerals and the controller never move, so the key
/// leaves them out.
let keyOf (room: Room) (spawns: int[]) : string =
    System.String.Join(",", spawns)
    + "|"
    + System.String.Join(",", Ground.obstacles room)

let colonies (world: World) (standing: Map<string, Standing>) : Colony[] * Map<string, Standing> =
    let mutable next = Map.empty
    let found = ResizeArray<Colony>()

    for room in world.Rooms |> Array.sortBy (fun r -> r.Name) do
        match room.Controller with
        | Some c when c.Owner.IsOurs ->
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
                            s.Kind.IsTower
                            && s.Owner.IsOurs
                            && Store.amountOf Energy s.Store >= Engine.towerEnergyCost)
                        |> Array.sortBy (fun t -> t.Id)
                    Hostiles = room.Hostiles
                    Ours = ours
                }
        | _ -> ()

    found.ToArray(), next
