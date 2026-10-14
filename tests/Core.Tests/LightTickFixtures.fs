/// The light tick suites' shared surface: the quiet world, the full tick
/// that saw it, the world a step on, and the hostile and plan readers more
/// than one light tick file reads. A helper one file alone reads stays
/// private there.
module Fabot.Core.Tests.LightTickFixtures

open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.LightTick

let internal room = "W1N1"
let internal tile x y = RoomPos.at room { X = x; Y = y }

let internal creepAt x y hits : GlanceCreep =
    Fabot.Core.Tests.Decide.Fixtures.glanceCreep (tile x y) hits

/// The room our controller stands in: not `room`, whose forcing rules are the
/// reach rules (a raider in a home forces every tick, `homeTests`).
let internal homeRoom = "W9N9"

/// A quiet world: two creeps of ours, a spawn, one controller at level 5, and
/// nobody else. Every object an act below names still stands, as the glance
/// looks it up (`LightTick.lookups`).
let internal quiet: Glance =
    {
        Creeps = Map.ofList [ "digger", creepAt 10 10 1000; "hauler", creepAt 20 20 500 ]
        Hostiles = []
        Structures = [ tile 30 30 ]
        Controllers = Map.ofList [ homeRoom, (5, false) ]
        Time = 0
        PowerCreeps = []
        Objects =
            Set.ofList
                [
                    "src-a"
                    "link-1"
                    "ext-9"
                    "ctrl"
                    "site"
                    "road"
                    "wall"
                    "store"
                    "can-buf"
                    "pile"
                    "reactor"
                    "core"
                    "obs-1"
                    "spawn"
                ]
        Covered = Set.empty
    }

/// The full tick that saw `quiet`: the digger harvesting where it stands and
/// handing its load on, the hauler one step into a walk.
let internal last: LastFull =
    LightTick.lastFull
        quiet
        (Map.ofList [ "hauler", [ tile 21 20; tile 22 20 ] ])
        [
            HarvestSource("digger", "src-a")
            TransferEnergyToStructure("digger", "link-1", Energy)
            MoveCreep("hauler", Right)
            SpawnCreep("Spawn1", [ Work; Carry; Move ], "next")
        ]

/// A one-colony record's plan.
let internal solo (record: LastFull) : Plan =
    record.Plans |> Map.values |> Seq.exactlyOne

/// The record with its plan's aims and volleys, as a fighter's full tick
/// lays them.
let internal aiming
    (aims: Intent list)
    (volleys: Map<string, Volley>)
    (record: LastFull)
    : LastFull =
    { record with
        Plans =
            record.Plans
            |> Map.map (fun _ plan ->
                { plan with
                    Aims = aims
                    Volleys = volleys
                })
    }

/// A hostile at a MOVE a part: never stationary.
let internal mobile: Gait = { Weight = 1; Moves = 1 }

let internal hostile x y owner armed : GlanceHostile =
    {
        Id = "h-1"
        Tile = tile x y
        Owner = owner
        Armed = armed
        Heals = false
        Weapon = if armed then Some Engine.meleeRange else None
        Gait = mobile
        Breaks = false
        Hits = 1000
    }

/// The world one tick on: the hauler took its step and everything else held.
let internal stepped =
    { quiet with
        Creeps = quiet.Creeps |> Map.add "hauler" (creepAt 21 20 500)
    }
