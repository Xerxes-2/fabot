/// The layout as one module of the tick: derive the colonies (`Derive`),
/// then for each the sites its plan still wants and its towers' reflex.
module Fabot.Core.Layout.Run

open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Intents
open Fabot.Core.State
open Fabot.Core.Tick
open Fabot.Core.Layout.Derive

/// The intents for the derived colonies, with `budget` sites left of the
/// world's MAX_CONSTRUCTION_SITES. A room already holding its sites in
/// flight, or whose census leaves nothing to place, places none.
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

    decideOver colonies (Engine.maxConstructionSites - world.OurSites),
    { state with Layout = standing }

/// The layout as the tick runs it.
let layout: Module = { Name = "layout"; Run = decide }
