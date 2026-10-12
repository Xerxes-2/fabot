module Fabot.Core.Fight.Run

open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Intents
open Fabot.Core.State
open Fabot.Core.Tick

let decide (context: Context) (state: State) : Intent list * State =
    let world = context.World
    let hostiles = world.Rooms |> Array.collect (fun r -> r.Hostiles)

    let groups, _ =
        Tracker.step
            Tracker.lostGroupTicks
            0
            (Hostiles.owned world.Rooms)
            state.Groups
            hostiles
            world.Time

    [], { state with Groups = groups }

let fight: Module = { Name = "fight"; Run = decide }
