/// The fixtures the observe channels share: the rooms the raid and
/// stand-down families are told apart by, a colony nobody is raiding, the
/// hostiles that raid it, and the folds' own readers. Shared because three
/// channels read the same colony, and a second spelling of "a quiet colony"
/// is free to drift from the one the others assert against.
[<AutoOpen>]
module Fabot.Core.Tests.ObserveFixtures

open Expecto
open Fabot.Core.Types
open Fabot.Core.Observe

/// The colony's own room. Named rather than left to `SpatialInfo.homeName`'s
/// empty string: the closest approach joins a hostile's room to the
/// projection's layer, and two halves that agreed by both being blank would
/// prove nothing.
let raidRoom = "W12S28"

/// A room of the scan set that is not the colony's own: where the
/// stand-down family's cores stand, and where a raider can stand too. The
/// two families are told apart by the room a record names.
let outpostRoom = "W12S27"

/// A colony nobody is raiding: the Raid fold reads hostiles, our creeps
/// and the tiles of what is ours, so everything else stays empty.
let quiet: ColonyView =
    {
        Time = 100
        Spawns = []
        Bank = { Available = 0; Capacity = 0 }
        Refillables = []
        Sources = []
        Controller = None
        RoomControl = Map.empty
        HeldOutposts = Set.empty
        ThreatenedOutposts = Set.empty
        ConstructionSites = []
        Creeps = []
        Hostiles = []
        // An invader core opens the log's other family; empty here, so the
        // spawn room's raid measurements regress against that family
        // arriving.
        InvaderCores = []
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some raidRoom
            }
        // The log records what happened in a room, not what is planned for
        // one, so the declaration, stage, borrowing and errands stay empty.
        Declared = []
        Stages = Map.empty
        Foreign = Set.empty
        Borrowed = { Rooms = [] }
        Refused = []
        Errands = []
        Consignee = None
        Crossed = Set.empty
        Reactors = []
        Sightings = Map.empty
        // A test that is about a tunable moves the one field it is about.
        Tuning = Tuning.defaults
        Casting = []
    }

/// A hostile creep of the given owner and body standing on a tile of the
/// colony's own room.
let raider id owner pos body : HostileInfo =
    {
        Id = id
        Owner = owner
        Pos = RoomPos.at raidRoom pos
        Body = body
        TicksToLive = Engine.creepLifetime
    }

/// One of ours with a full life ahead of it; immaterial but for its name.
/// Whatever becomes of it, its own clock is not what did it.
let ours name : CreepInfo =
    {
        Name = name
        TicksToLive = 500
        Hits = { Hits = 300; HitsMax = 300 }
        Fatigue = 0
        Energy = 0
        Thorium = 0
        FreeCapacity = 50
        Moved = false
        Body = Map.ofList [ Work, 1; Carry, 1; Move, 1 ]
    }

/// One of ours on the last tick of its life: the engine's counter runs out
/// on it, so it is gone next tick whatever the raiders do.
let spent name = { ours name with TicksToLive = 1 }

/// The squad of #66, cut to the one creep the lifecycle tests need.
let squad = [ raider "TWX" "giaco" { X = 38; Y = 47 } [ Tough; Attack; Move ] ]

/// The same body on the same tile, a room away: the room is the only
/// difference from `squad`, so the pair asks which of the fold's answers
/// are the colony's and which are one room's.
let outpostSquad =
    squad
    |> List.map (fun hostile ->
        { hostile with
            Pos = RoomPos.at outpostRoom (RoomPos.pos hostile.Pos)
        })

/// A colony holding just these hostiles.
let raid hostiles = { quiet with Hostiles = hostiles }

/// #66's room in miniature, with something of ours to measure against:
/// the tower's spawn at (10,40) and one of our creeps out at (9,44).
let placed =
    { quiet with
        Refillables =
            [
                {
                    Id = "spawn-1"
                    FreeCapacity = 0
                    Kind = BuiltKind.Spawn
                }
            ]
        Creeps = [ ours "w1" ]
        Spatial =
            { quiet.Spatial with
                // Under the name the hostiles above stand in: the closest
                // approach joins the two before it measures anything.
                Rooms =
                    Map.ofList
                        [
                            raidRoom,
                            { RoomLayer.empty with
                                TargetPositions = Map.ofList [ "spawn-1", { X = 10; Y = 40 } ]
                                CreepPositions = Map.ofList [ "w1", { X = 9; Y = 44 } ]
                            }
                        ]
            }
    }

/// The colony at the given tick under a five-tick quiet gap, so an
/// episode's close is exercised in a few ticks rather than in fifty.
let atShortGap t (colony: ColonyView) =
    { colony with
        Time = t
        Tuning = { colony.Tuning with QuietGap = 5 }
    }

/// Fold one Raid-log tick over a colony at the given tick, with a small
/// ring cap and a short quiet gap so both are exercised in a few ticks
/// rather than a few hundred.
///
/// The world holds exactly this colony's creeps, which is the one-colony
/// world every test but `adoptionTests` below is written in: there is
/// nobody else to have adopted a name that left the ColonyView, so a name
/// that leaves it left the world.
let raidTick t (colony: ColonyView) state =
    let alive = colony.Creeps |> List.map (fun creep -> creep.Name) |> Set.ofList

    foldRaids
        3
        alive
        (atShortGap t colony)
        (Fabot.Core.Decide.Planner.outpostFactsOf (atShortGap t colony))
        state

/// The same fold with the world said separately from the colony: a
/// ColonyView carries one colony's fleet and `Game.creeps` everyone's.
let raidTickIn alive t (colony: ColonyView) state =
    foldRaids
        3
        (Set.ofList alive)
        (atShortGap t colony)
        (Fabot.Core.Decide.Planner.outpostFactsOf (atShortGap t colony))
        state

/// The recorded episodes as (opened, last-seen) windows, oldest first.
let windows (state: RaidState) =
    state.Episodes |> List.map (fun e -> e.Opened, e.LastSeen)

/// Every episode's roster rows, oldest episode first.
let rosters (state: RaidState) =
    state.Episodes |> List.collect (fun e -> Map.toList e.Roster)

/// Every episode's recorded losses, oldest episode first.
let losses (state: RaidState) =
    state.Episodes |> List.collect (fun e -> e.Losses)

/// A hostile of the raid standing in a room other than the colony's own,
/// with a full Invader life. Each carries an id of its own, a raid being a
/// roster.
let raiderIn room i body : HostileInfo =
    {
        Id = $"raid-{i}"
        Owner = "Invader"
        Pos = RoomPos.at room { X = 25; Y = 25 }
        Body = body
        TicksToLive = Engine.creepLifetime
    }

/// An invader core standing in a room, with or without a collapse timer to
/// read a deadline off. A level-0 core — the measured case on this
/// colony's frontier — carries none.
let core room collapse : InvaderCoreInfo =
    {
        RoomName = room
        CollapseTick = collapse
        Level = 0
    }

/// The same core as a stronghold: level 1 or more, which is to say towers
/// under million-hit ramparts and a garrison.
let bunker room collapse level : InvaderCoreInfo =
    { core room collapse with
        Level = level
    }

/// A colony that can see these cores, and nothing else going on.
let seen cores = { quiet with InvaderCores = cores }

/// The room as vision answers for it: nobody owns it, and this is what
/// stands on its controller. `RoomControl` carries an entry only for a
/// room the colony can see, so putting one there is how a fixture says the
/// colony is looking.
let visible room reservation (colony: ColonyView) =
    { colony with
        RoomControl =
            colony.RoomControl
            |> Map.add
                room
                {
                    Owner = Ownership.Unowned
                    Reservation = reservation
                    SafeMode = false
                    Sign = None
                }
    }

/// A reservation on that controller, carrying the engine's own *relative*
/// count of what is left to run on it.
let heldBy holder ticks =
    Some { Holder = holder; TicksToEnd = ticks }

/// The room as vision answers for it when another player owns the
/// controller outright: the clockless withdrawal, a rival's reservation
/// being a clock.
let ownedByRival room (colony: ColonyView) =
    { colony with
        RoomControl =
            colony.RoomControl
            |> Map.add
                room
                {
                    Owner = Ownership.Rival
                    Reservation = None
                    SafeMode = false
                    Sign = None
                }
    }

/// The outpost room as a declared one: its controller is placed and classified
/// in the projection, which is the fact both the guard row and the raid
/// stand-down use to distinguish work from a room the colony merely crosses.
let withDeclaredOutpost room (colony: ColonyView) =
    let controller = $"ctrl-{room}"
    let layer = SpatialInfo.layerOf colony.Spatial room

    { colony with
        Spatial =
            { colony.Spatial with
                Rooms =
                    Map.add
                        room
                        { layer with
                            TargetPositions =
                                Map.add controller { X = 25; Y = 25 } layer.TargetPositions
                        }
                        colony.Spatial.Rooms
                TargetKinds = Map.add controller Controller colony.Spatial.TargetKinds
            }
    }

/// Whether each recorded stand-down remembers a stronghold, oldest first —
/// the fact that decides whether the room may be crossed.
let strongholds (state: RaidState) =
    state.Outposts |> List.map (fun e -> e.Stronghold)

/// The recorded stand-downs as (room, opened, last seen, expiry, basis),
/// oldest first — the whole of what the outpost family records.
let standDowns (state: RaidState) =
    state.Outposts
    |> List.map (fun e -> e.RoomName, e.Opened, e.LastSeen, e.Expiry, e.Basis)

/// The rooms the gate withholds from the work at a tick, under the numbers
/// the bot ships.
let shutAt tick state =
    (standDown Tuning.defaults tick state).Shut

/// The rooms the gate also withholds from walking at a tick: the subset of
/// `shutAt` a stronghold holds.
let impassableAt tick state =
    (standDown Tuning.defaults tick state).Impassable

/// The latched rooms this tick takes one look into — a subset of `shutAt`'s
/// answer and never a room leaving it.
let recheckedAt tick state =
    (standDown Tuning.defaults tick state).Rechecked

/// The outposts somebody else's reservation was standing on at the last
/// look and whose hold this tick is still short of; withholds no room.
let heldAt tick state =
    (standDown Tuning.defaults tick state).HeldOutposts

/// The declared outposts an armed threat was standing in at the last look
/// and whose memory this tick is still short of; withholds no room.
let threatenedAt tick state =
    (standDown Tuning.defaults tick state).ThreatenedOutposts

/// An armed raider in a room: a Threat.
let armedIn room = raiderIn room 1 [ Move; Attack ]

/// The same raid with no weapon on it: a healer is shot and recorded, and
/// no reason to buy a guard.
let healerIn room = raiderIn room 2 [ Move; Heal ]

/// A declared outpost the colony is looking into this tick, holding
/// whatever hostiles the case names: either fact alone writes nothing.
let lookingAt room hostiles (colony: ColonyView) =
    { (colony |> withDeclaredOutpost room |> visible room None) with
        Hostiles = hostiles
    }

/// The colony with one structure of the given kind standing at the given
/// hits — what the damage fold reads: the kind decides whether it is
/// charged, the number is what moves tick over tick.
let withHits id kind hits (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add id (Structure kind) colony.Spatial.TargetKinds
                Hits = Map.add id { Hits = hits; HitsMax = 3_000_000 } colony.Spatial.Hits
            }
    }
