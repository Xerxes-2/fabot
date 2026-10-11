/// The tower reflex: each tower of ours with the energy for an act shoots
/// the hostile in its room it deals the most damage to, else heals our most
/// damaged creep there. Repair is economy work and not here.
module Fabot.Core.Layout.Towers

open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Intents

// What a tower's shot from `from` takes off a creep: the falloff at its
// range, through its boosted TOUGH.
let private dealt (from: Pos) (creep: Creep) =
    let range = max (abs (from.X - creep.Pos.X)) (abs (from.Y - creep.Pos.Y))
    let body = creep.Body |> Array.map (fun p -> p.Kind) |> List.ofArray
    let boosts = creep.Body |> Array.map (fun p -> p.Boost) |> List.ofArray
    Engine.damageTaken body boosts creep.Hits (Engine.towerAttackAt range)

/// The acts of `towers` (ours, each holding the energy for one) in a room
/// with `hostiles` and our creeps `ours`: each on the hostile it deals the
/// most damage, then the fewest hits left, then the lowest id; with none, a
/// heal on the creep missing the most hits, then the lowest name.
let fire (towers: Structure[]) (hostiles: Creep[]) (ours: Creep[]) : Intent list =
    let wounded =
        ours
        |> Array.filter (fun c -> c.Hits < c.HitsMax)
        |> Array.sortBy (fun c -> c.Hits - c.HitsMax, c.Name)
        |> Array.tryHead

    [
        for tower in towers do
            if hostiles.Length > 0 then
                let target =
                    hostiles
                    |> Array.sortBy (fun h -> -(dealt tower.Pos h), h.Hits, h.Id)
                    |> Array.head

                yield Tower(tower.Id, Attack target.Id)
            else
                match wounded with
                | Some creep -> yield Tower(tower.Id, Heal creep.Id)
                | None -> ()
    ]
