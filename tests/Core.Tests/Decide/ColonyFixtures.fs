/// The colony suites' shared surface: the north room as a candidate, a
/// nursery, a bootstrapped child and a room gone from the scan set, the
/// child run as a colony of its own. A helper one file alone reads stays
/// private there.
module Fabot.Core.Tests.Decide.ColonyFixtures

open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

/// The north room with a **spawn** construction site of ours standing in
/// it: the site a human places in a nursery by hand. Built beside
/// `withOutpostSite` and not out of it, because an outpost's *container*
/// site is already feeding-tier work (#157) and a case built on one could
/// not tell the nursery rule apart from that one.
let internal withNorthSpawnSite (site: Pos) (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        ConstructionSites =
            colony.ConstructionSites
            @ [
                {
                    Id = "site-spawn"
                    Left = siteOwes
                    Begun = false
                }
            ]
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add "site-spawn" (Site BuiltKind.Spawn) colony.Spatial.TargetKinds
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    TargetPositions = Map.add "site-spawn" site outpost.TargetPositions
                }
    }

/// The same colony with the north room declared a home of ours and still
/// unowned: a candidate colony, the tick before the claim lands. Holds
/// the human's half of the declaration fixed and leaves ownership to move.
let internal asCandidate (colony: ColonyView) =
    { colony with
        RoomControl = Map.add "W1N2" neutralRoom colony.RoomControl
        Declared = [ SpatialInfo.homeName colony.Spatial; "W1N2" ]
    }

/// The same colony with a spawn of ours standing in the north room: the
/// end of the nursery. A spawn *structure* in that room's layer, which is
/// what the tick a spawn is finished changes; the human's edit splitting
/// `Colony.declared` follows that tick rather than causing it.
///
/// Placed in the projection *and* in the room's stage: the shell derives
/// the pair off the world together, and the spawn structure stays in the
/// layer because that is what the mother's pioneers walk up to. Not in
/// `ColonyView.Spawns`, the colony's own spawn list since #191.
let internal withNorthSpawn (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Stages = Map.add "W1N2" Bootstrapping colony.Stages
        Spatial =
            { colony.Spatial with
                TargetKinds =
                    Map.add "spawn-2" (Structure BuiltKind.Spawn) colony.Spatial.TargetKinds
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    TargetPositions = Map.add "spawn-2" { X = 10; Y = 44 } outpost.TargetPositions
                }
    }

/// The same colony with the north room out of its scan set altogether:
/// what the tick a bootstrapped child reaches `Tuning.BootstrapLevel`
/// does to its mother's ColonyView. The level is read off the world once
/// by `Colony.bootstrapping`, so what RCL3 *is* at this seam is the whole
/// room leaving: its layer, its border ring, the ids that layer placed,
/// the sites in it and the `RoomControl` entry.
///
/// Subtracted whole rather than one entry at a time, because the scan
/// set is the single gate and a fixture that removed only the control
/// entry would pin a state the projection cannot be in.
let internal withoutNorthRoom (colony: ColonyView) =
    let placed = (SpatialInfo.layerOf colony.Spatial "W1N2").TargetPositions

    { colony with
        RoomControl = Map.remove "W1N2" colony.RoomControl
        // `Stages` is the world's map and reaches every colony whatever it
        // projects, so RCL3 turns one entry `Independent`, never removes it.
        Stages = Map.add "W1N2" Independent colony.Stages
        ConstructionSites =
            colony.ConstructionSites
            |> List.filter (fun site -> not (Map.containsKey site.Id placed))
        Spatial =
            { colony.Spatial with
                Rooms = Map.remove "W1N2" colony.Spatial.Rooms
                Borders = Map.remove "W1N2" colony.Spatial.Borders
                TargetKinds =
                    colony.Spatial.TargetKinds
                    |> Map.filter (fun id _ -> not (Map.containsKey id placed))
            }
    }

/// A creep of each colony's casting, named the way `planSpawns` names one
/// (`{pattern}-{tick}-{spawn}`), because the caster is read out of the name.
let internal motherCast = "worker-100-Spawn1"
let internal childCast = "worker-100-Spawn2"

/// The child: the north room run as a home of its own. Built beside
/// `northBorderColony` rather than out of it, because a shared projection
/// would prove nothing about which colony a Task came from.
let internal childColony (creeps: (string * Pos) list) =
    { bareRespawn with
        Spawns = []
        Controller = None
        Refillables = []
        Sources = [ source "src-child" ]
        Creeps = creeps |> List.map (fun (name, _) -> worker name 0 50)
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N2"
                Borders = Map.ofList [ "W1N2", plainRing ]
                TargetKinds = Map.ofList [ "src-child", Source ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain = TerrainGrid.ofList (corridor 10 40 48)
                    TargetPositions = Map.ofList [ "src-child", { X = 10; Y = 46 } ]
                    CreepPositions = Map.ofList creeps
                })
    }

/// A controller of *ours* in the north room under its own id: the child
/// colony's, laid into the layer the mother projects the room under. Her
/// own controller is `ColonyView.Controller`.
let internal withNorthController (pos: Pos) (colony: ColonyView) =
    let north = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add "ctrl-child" TargetKind.Controller colony.Spatial.TargetKinds
            }
            |> withNeighbour
                "W1N2"
                { north with
                    TargetPositions = Map.add "ctrl-child" pos north.TargetPositions
                }
    }

/// The mother's ColonyView while she raises a child that has already stood
/// its own spawn, with the human's spawn site still standing in it. What
/// makes this a **bootstrapped child** rather than the nursery fixture is
/// the spawn (`withNorthSpawn`).
///
/// The mother's own controller stands at home beside it, because the
/// question below is which of two Upgrades a loaded body takes.
let internal claimedChild =
    northBorderColony { X = 10; Y = 38 }
    |> withNorthOutpost None
    |> withNorthController { X = 10; Y = 45 }
    |> withNorthSpawnSite { X = 10; Y = 43 }
    |> withHomeController { X = 10; Y = 5 }
    |> asNursery

let internal raisingMother = withNorthSpawn claimedChild
