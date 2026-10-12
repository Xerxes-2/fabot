module Fabot.Core.Layout.Run

open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Intents
open Fabot.Core.State
open Fabot.Core.Tick
open Fabot.Core.Layout.Derive

let decideOver (colonies: Colony[]) (budget: int) : Intent list =
    let mutable budget = budget
    let intents = ResizeArray<Intent>()

    for colony in colonies do
        match colony.Plan with
        | Some plan when
            budget > 0
            && colony.Census.Pending < Sites.sitesInFlight
            && Sites.mayPlace colony.Census colony.Level
            ->
            for p in Sites.place colony.Census colony.Level plan budget do
                budget <- budget - 1
                intents.Add(Room(colony.Room, CreateSite(p.X, p.Y, p.Kind)))
        | _ -> ()

        intents.AddRange(Towers.fire colony.Towers colony.Hostiles colony.Ours)

    List.ofSeq intents

let decide (context: Context) (state: State) : Intent list * State =
    let world = context.World
    let colonies, standing = Derive.colonies world state.Layout

    let budget = Engine.maxConstructionSites - world.OurSites
    let home = decideOver colonies budget

    let placed =
        home
        |> List.sumBy (fun i ->
            match i with
            | Room(_, CreateSite _) -> 1
            | _ -> 0)

    home
    @ Remote.sites context.Atlas world (Tick.worked context) home (budget - placed),
    { state with Layout = standing }

let layout: Module = { Name = "layout"; Run = decide }
