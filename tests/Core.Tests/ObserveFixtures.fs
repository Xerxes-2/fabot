/// The fixtures the observe channels share (#335): the rooms the raid and
/// stand-down families are told apart by, a colony nobody is raiding, the
/// hostiles that raid it, and the folds' own readers. They live here rather
/// than in one channel's file because three of them read the same colony —
/// the raid episodes, the outpost family and the breach log — and a second
/// spelling of "a quiet colony" is a fixture free to drift from the one the
/// other two are asserting against.
[<AutoOpen>]
module Fabot.Core.Tests.ObserveFixtures

open Expecto
open Fabot.Core.Types
open Fabot.Core.Observe

/// The room the raid fixtures project — the colony's own, and the only
/// room `ColonyView.Hostiles` swept until #201 widened it to every room the
/// colony works and can see. Named rather than left to
/// `SpatialInfo.homeName`'s empty string, because the closest approach
/// joins a hostile's room to the projection's layer (ADR 0041) and a
/// fixture whose two halves agreed by both being blank would prove nothing.
let raidRoom = "W12S28"

/// A room of the scan set that is not the colony's own: where the
/// stand-down family's cores stand, and — since #201 — where a raider the
/// sweep now reaches can stand too. The two families are told apart by the
/// room a record names and never by both being blank.
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
        // The Raid fold prices no source, so who holds the room is nothing
        // it reads (ADR 0042).
        RoomControl = Map.empty
        HeldOutposts = Set.empty
        ThreatenedOutposts = Set.empty
        ConstructionSites = []
        Creeps = []
        Hostiles = []
        // The raid fold reads hostile *creeps* here; an invader core is a
        // structure, opens the log's other family and is folded from this
        // list (ADR 0043) — empty for every fixture the spawn room's raid
        // is measured through, which is what makes those measurements a
        // regression against the second family arriving.
        InvaderCores = []
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some raidRoom
            }
        // The Raid fold reads no declaration: which rooms a human means to
        // own decides Tasks and quotas (ADR 0047), and the log records what
        // happened in a room rather than what is planned for one.
        Declared = []
        // Nor a [[stage]], for the same reason: what a colony is old
        // enough to do (ADR 0052 decision 3) is not what happened to it.
        Stages = Map.empty
        // Another colony's bodies are in no raid of this one's: the log
        // counts the creeps this colony lost (ADR 0028), and a body it
        // does not hold is not one of them (ADR 0052 decision 1).
        Foreign = Set.empty
        // And nothing is borrowed: what one colony may take of a child's
        // room decides Tasks, and the log records what happened.
        Borrowed = { Rooms = [] }
        // And nothing refused: the log records what happened, and a room
        // no Seam reaches has nobody in it for anything to happen to (#243).
        Refused = []
        // And no [[errand]]: an errand is a room a human declared because
        // one named object out there has to be acted on (ADR 0060 decision
        // 1), and this fixture declares none — so no Reclaim is pooled and
        // no seat of the reserver row is the re-claimer's (#318).
        Errands = []
        Consignee = None
        Crossed = Set.empty
        Reactors = []
        // And nothing remembered of a room it cannot see: the log records
        // what happened, and a vision grace changes which assignment a tick
        // holds and never what a tick did (#151).
        Sightings = Map.empty
        // The numbers this bot ships with (ADR 0052 decision 5): a
        // fixture starts from them and the tests that are *about* a
        // tunable move the one field they are about.
        Tuning = Tuning.defaults
        // Nothing in the oven: a fixture's rows count what is alive, and
        // the casting cascade's own tests are the ones that put a body
        // here (#156).
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

/// The same body on the same tile, a room away: the raider #201's widened
/// sweep reaches (`ColonyView.Hostiles` covers every room the colony works
/// and can see). The room is the only difference from `squad`, which is
/// what makes the pair able to ask which of the fold's answers are the
/// colony's and which are one room's.
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
                // Under the room's own name, which is the only place tiles
                // live since ADR 0041 — and the name the hostiles above
                // stand in, because the closest approach joins the two
                // before it measures anything.
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

/// The colony at the given tick under a five-tick [[quiet gap]], so an
/// episode's close is exercised in a few ticks rather than in fifty. The
/// gap is the colony's own tunable since #216 R4 (ADR 0052 decision 5), so
/// a fixture that wants a short one says so on the view.
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
    foldRaids 3 alive (atShortGap t colony) state

/// The same fold with the world said separately from the colony: what the
/// shell hands in since #191, where a ColonyView carries one colony's fleet
/// and `Game.creeps` carries everyone's (ADR 0047).
let raidTickIn alive t (colony: ColonyView) state =
    foldRaids 3 (Set.ofList alive) (atShortGap t colony) state

/// The recorded episodes as (opened, last-seen) windows, oldest first.
let windows (state: RaidState) =
    state.Episodes |> List.map (fun e -> e.Opened, e.LastSeen)

/// Every episode's roster rows, oldest episode first.
let rosters (state: RaidState) =
    state.Episodes |> List.collect (fun e -> Map.toList e.Roster)

/// Every episode's recorded losses, oldest episode first.
let losses (state: RaidState) =
    state.Episodes |> List.collect (fun e -> e.Losses)

/// A hostile of the raid standing in a room other than the colony's own, with
/// a full Invader life: what ADR 0043's clock reads for a raid with no core in
/// it (#257). Each carries an id of its own, a raid being a roster.
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

/// The same core as a **stronghold** — level 1 or more, which is to say towers
/// under million-hit ramparts and a garrison (#382).
let bunker room collapse level : InvaderCoreInfo =
    { core room collapse with
        Level = level
    }

/// A colony that can see these cores, and nothing else going on.
let seen cores = { quiet with InvaderCores = cores }

/// The room as vision answers for it: nobody owns it, and this is what
/// stands on its controller. `RoomControl` carries an entry only for a
/// room the colony can see (ADR 0004), so putting one there is how a
/// fixture says the colony is looking.
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
/// controller outright — ADR 0043's clockless withdrawal, and since #165 the
/// whole of it: a rival's reservation beside this one is a clock.
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

/// The recorded stand-downs as (room, opened, last seen, expiry, basis),
/// oldest first — the whole of what the outpost family records.
/// Whether each recorded stand-down remembers a stronghold (#382), oldest
/// first — the fact that decides whether the room may be crossed.
let strongholds (state: RaidState) =
    state.Outposts |> List.map (fun e -> e.Stronghold)

let standDowns (state: RaidState) =
    state.Outposts
    |> List.map (fun e -> e.RoomName, e.Opened, e.LastSeen, e.Expiry, e.Basis)

/// The rooms the gate withholds from the work at a tick, under the numbers the
/// bot ships. Since #165 the gate answers with two sets and this is the one ADR
/// 0043 wrote every pin below against: which rooms the colony does not work.
let shutAt tick state =
    (standDown Tuning.defaults tick state).Shut

/// The rooms the gate also withholds from **walking** at a tick (#382): the
/// subset of `shutAt` a stronghold holds.
let impassableAt tick state =
    (standDown Tuning.defaults tick state).Impassable

/// The gate's other half (#165): the latched rooms this tick takes one look
/// into — a subset of `shutAt`'s answer and never a room leaving it.
let recheckedAt tick state =
    (standDown Tuning.defaults tick state).Rechecked

/// The gate's third set (#333), which withholds no room at all: the outposts
/// somebody else's reservation was standing on at the last look and whose hold
/// this tick is still short of. What the view hands to
/// `Planner.reservableControllers` on the ticks vision answers for nothing.
let heldAt tick state =
    (standDown Tuning.defaults tick state).HeldOutposts

/// The gate's fourth set (#366), which withholds no room either: the declared
/// outposts an armed [[threat]] was standing in at the last look and whose
/// memory this tick is still short of. What the view hands to
/// `Planner.guardedOutposts` and `Quota.guardsWanted` on the ticks the raid has
/// killed everything of ours that could see the room.
let threatenedAt tick state =
    (standDown Tuning.defaults tick state).ThreatenedOutposts

/// An armed raider in a room: ADR 0033's Threat, the only hostile #366's
/// memory is written for.
let armedIn room = raiderIn room 1 [ Move; Attack ]

/// The same raid with no weapon on it: a healer is a hostile the [[fire
/// reflex]] shoots and the [[raid log]] records, and no reason to buy a guard.
let healerIn room = raiderIn room 2 [ Move; Heal ]

/// A declared outpost the colony is **looking into** this tick, holding
/// whatever hostiles the case names — the two facts #366's memory is written
/// from, said together because either alone writes nothing.
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
