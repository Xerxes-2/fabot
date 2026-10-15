/// Repair is economy work and not here.
module Fabot.Core.Layout.Towers

open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Intents

let private dealt (from: Pos) (creep: Creep) =
    let range = Pos.range from creep.Pos |> Option.defaultValue 0
    let body = creep.Body |> Array.map (fun p -> p.Kind) |> List.ofArray
    let boosts = creep.Body |> Array.map (fun p -> p.Boost) |> List.ofArray
    Engine.damageTaken body boosts creep.Hits (Engine.towerAttackAt range)

/// `towers` each hold the energy for one act.
let fire (towers: Structure[]) (hostiles: Creep[]) (ours: Creep[]) : Intent list =
    let wounded =
        ours
        |> Array.filter (fun c -> c.Hits < c.HitsMax)
        |> Array.sortBy (fun c -> c.Hits - c.HitsMax, c.Name)
        |> Array.tryHead

    let acts = ResizeArray<Intent>()

    for tower in towers do
        if hostiles.Length > 0 then
            let target =
                hostiles
                |> Array.sortBy (fun h -> -(dealt tower.Pos h), h.Hits, h.Id)
                |> Array.head

            acts.Add(Tower(tower.Id, Attack target.Id))
        else
            match wounded with
            | Some creep -> acts.Add(Tower(tower.Id, Heal creep.Id))
            | None -> ()

    List.ofSeq acts
