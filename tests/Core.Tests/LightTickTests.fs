/// The light tick: what a quiet tick replays of the last full one, what it
/// walks, and every fact that hands it over to a full tick instead.
module Fabot.Core.Tests.LightTickTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.LightTick

let private room = "W1N1"
let private tile x y = RoomPos.at room { X = x; Y = y }

let private creepAt x y hits : GlanceCreep =
    {
        Tile = tile x y
        Hits = hits
        Inward = None
    }

/// A quiet world: two creeps of ours, a spawn, one controller at level 5, and
/// nobody else.
let private quiet: Glance =
    {
        Creeps = Map.ofList [ "digger", creepAt 10 10 1000; "hauler", creepAt 20 20 500 ]
        Hostiles = []
        Structures = [ tile 30 30 ]
        Controllers = Map.ofList [ room, (5, false) ]
    }

/// The full tick that saw `quiet`: the digger harvesting where it stands and
/// handing its load on, the hauler one step into a walk.
let private last: LastFull =
    LightTick.lastFull
        quiet
        (Map.ofList [ "hauler", (tile 21 20, tile 22 20) ])
        [
            HarvestSource("digger", "src-a")
            TransferEnergyToStructure("digger", "link-1", Energy)
            MoveCreep("hauler", Right)
            SpawnCreep("Spawn1", [ Work; Carry; Move ], "next")
        ]

let private hostile x y owner armed : GlanceHostile =
    {
        Tile = tile x y
        Owner = owner
        Armed = armed
        MoveOnly = false
    }

/// The world one tick on: the hauler took its step and everything else held.
let private stepped =
    { quiet with
        Creeps = quiet.Creeps |> Map.add "hauler" (creepAt 21 20 500)
    }

[<Tests>]
let replayTests =
    testList
        "light tick replay"
        [
            test "a creep on the tile it worked from re-issues its repeatable work and nothing else" {
                Expect.equal
                    (LightTick.intents last stepped)
                    [ HarvestSource("digger", "src-a"); MoveCreep("hauler", Right) ]
                    "the harvest again and the next step; never the transfer or the spawn"
            }

            test "every repeatable work intent is kept and every one-shot one dropped" {
                let work =
                    [
                        HarvestSource("digger", "src-a")
                        UpgradeController("digger", "ctrl")
                        ReserveController("digger", "ctrl")
                        BuildSite("digger", "site")
                        RepairStructure("digger", "road")
                        DismantleStructure("digger", "wall")
                    ]

                let oneShot =
                    [
                        TransferEnergyToStructure("digger", "spawn", Energy)
                        // The hauler's: one beside an upgrade is replayed.
                        WithdrawFromStore("hauler", "store", Energy, None)
                        PickupPile("digger", "pile")
                        SpawnCreep("Spawn1", [ Move ], "x")
                        PlaceConstructionSite(tile 5 5, StructureKind.Road)
                        ClaimController("digger", "ctrl")
                        ClaimReactor("digger", "reactor")
                        SignController("digger", "ctrl", "hi")
                        SendFromTerminal("term", Energy, 100, "W2N2")
                        SayCreep("digger", "o/")
                    ]

                let full = LightTick.lastFull quiet Map.empty (work @ oneShot)

                Expect.equal full.Work work "the six repeatable verbs, in the full tick's order"
                Expect.isFalse full.Fought "none of them shoots or heals"

                Expect.equal
                    (LightTick.intents full quiet)
                    work
                    "and a light tick replays exactly those"
            }

            test "a standing upgrader's withdraw beside its upgrade is replayed with it" {
                // The Emitter pairs the two for a standing body beside its
                // buffer and for no one else; a lone withdraw is one-shot.
                let paired =
                    LightTick.lastFull
                        quiet
                        Map.empty
                        [
                            UpgradeController("digger", "ctrl")
                            WithdrawFromStore("digger", "can-buf", Energy, None)
                            WithdrawFromStore("hauler", "store", Energy, None)
                        ]

                Expect.equal
                    (LightTick.intents paired quiet)
                    [
                        UpgradeController("digger", "ctrl")
                        WithdrawFromStore("digger", "can-buf", Energy, None)
                    ]
                    "both acts for the upgrader, nothing for the hauler"

                let pushed =
                    { quiet with
                        Creeps = quiet.Creeps |> Map.add "digger" (creepAt 11 10 1000)
                    }

                Expect.equal
                    (LightTick.intents paired pushed)
                    []
                    "moved, it may stand out of the buffer's reach: neither"
            }

            test "a creep off the tile it worked from re-issues nothing" {
                let pushed =
                    { stepped with
                        Creeps = stepped.Creeps |> Map.add "digger" (creepAt 11 10 1000)
                    }

                Expect.equal
                    (LightTick.intents last pushed)
                    [ MoveCreep("hauler", Right) ]
                    "the digger waits for the next full tick"
            }

            test "a walker steps toward its plan's second tile only while it stands on the first" {
                Expect.contains
                    (LightTick.intents last stepped)
                    (MoveCreep("hauler", Right))
                    "on (21,20), toward (22,20)"

                let diagonal =
                    LightTick.lastFull quiet (Map.ofList [ "hauler", (tile 21 20, tile 22 21) ]) []

                Expect.equal
                    (LightTick.intents diagonal stepped)
                    [ MoveCreep("hauler", BottomRight) ]
                    "the direction is the plan's, not last tick's"

                // Blocked, fatigued or pushed: it never reached the first tile.
                Expect.equal
                    (LightTick.intents last quiet)
                    [ HarvestSource("digger", "src-a") ]
                    "a blocked walker waits"
            }

            test "a light tick issues in creep-name order" {
                let many =
                    LightTick.lastFull
                        quiet
                        Map.empty
                        [ UpgradeController("hauler", "ctrl"); HarvestSource("digger", "src-a") ]

                Expect.equal
                    (LightTick.intents many quiet)
                    [ HarvestSource("digger", "src-a"); UpgradeController("hauler", "ctrl") ]
                    "digger before hauler, whatever the full tick's order"
            }
        ]

[<Tests>]
let forcedTests =
    testList
        "light tick forced full"
        [
            test "a quiet world one step on forces nothing" {
                Expect.equal (LightTick.forced last stepped) None "the light tick runs"
            }

            test "an armed hostile in a home room, far from everything, forces a full tick" {
                let raided =
                    { stepped with
                        Hostiles = [ hostile 45 45 "Invader" true ]
                    }

                Expect.equal
                    (LightTick.forced last raided)
                    (Some(LightForce.ArmedHostile(room, "Invader")))
                    "combat at home is decided"
            }

            test "an armed hostile beside ours in a room under our safe mode forces nothing" {
                // Live t931,466: Trepidimous' pair in W17S25 under its safe mode
                // held every tick full at 95 ms.
                let safe =
                    { stepped with
                        Hostiles = [ hostile 21 21 "Invader" true ]
                        Controllers = Map.ofList [ room, (5, true) ]
                    }

                Expect.equal
                    (LightTick.forced
                        { last with
                            Controllers = safe.Controllers
                        }
                        safe)
                    None
                    "it can act on nothing of ours"
            }

            test "an armed ally beside ours forces nothing" {
                let ally = Colony.allies |> Seq.head

                let guarded =
                    { stepped with
                        Hostiles = [ hostile 21 21 ally true ]
                    }

                Expect.equal (LightTick.forced last guarded) None "an ally is no threat"
            }

            test
                "an armed hostile in a harassment room, out of reach of every creep of ours, forces nothing" {
                // #461: a defender kept standing in a room we only harass took
                // the whole shard's light ticks away.
                let harassed = "W3N1"

                let withRanger =
                    { stepped with
                        Creeps =
                            stepped.Creeps
                            |> Map.add
                                "ranger"
                                {
                                    Tile = RoomPos.at harassed { X = 10; Y = 10 }
                                    Hits = 500
                                    Inward = None
                                }
                    }

                let full = LightTick.lastFull withRanger Map.empty []

                let defenderAt x =
                    { withRanger with
                        Hostiles =
                            [
                                {
                                    Tile = RoomPos.at harassed { X = x; Y = 10 }
                                    Owner = "Trepidimous"
                                    Armed = true
                                    MoveOnly = false
                                }
                            ]
                    }

                Expect.equal
                    (LightTick.forced full (defenderAt (10 + Engine.rangedRange + 3)))
                    None
                    "standing off: the tick may be light"

                Expect.equal
                    (LightTick.forced full (defenderAt (10 + Engine.rangedRange)))
                    (Some(LightForce.HostileNear(harassed, "Trepidimous")))
                    "trading shots with our ranger at range 3: the near rule"

                Expect.equal
                    (LightTick.forced full (defenderAt (10 + Engine.rangedRange + 2)))
                    (Some(LightForce.HostileNear(harassed, "Trepidimous")))
                    "at the near rule's edge: still full"
            }

            test "an armed Source Keeper far from us forces nothing" {
                let keeper =
                    { stepped with
                        Hostiles = [ hostile 45 45 "Source Keeper" true ]
                    }

                Expect.equal (LightTick.forced last keeper) None "a keeper on its rock is terrain"
            }

            test "an unarmed hostile off in the room forces nothing" {
                let miner =
                    { stepped with
                        Hostiles = [ hostile 45 45 "Rival" false ]
                    }

                Expect.equal
                    (LightTick.forced last miner)
                    None
                    "the harassment rooms hold one nearly every tick"
            }

            test "any hostile within ranged range plus two of a creep of ours forces a full tick" {
                let edge = Engine.rangedRange + 2

                let near =
                    { stepped with
                        Hostiles = [ hostile (10 + edge) 10 "Source Keeper" true ]
                    }

                let far =
                    { stepped with
                        Hostiles = [ hostile (10 + edge + 1) 10 "Rival" false ]
                    }

                Expect.equal
                    (LightTick.forced last near)
                    (Some(LightForce.HostileNear(room, "Source Keeper")))
                    "a keeper that comes near"

                Expect.equal (LightTick.forced last far) None "one tile further is not near"
            }

            test "any hostile near a structure of ours forces a full tick" {
                let atTheSpawn =
                    { stepped with
                        Hostiles = [ hostile 33 33 "Rival" false ]
                    }

                Expect.equal
                    (LightTick.forced last atTheSpawn)
                    (Some(LightForce.HostileNear(room, "Rival")))
                    "a body at the gate"
            }

            test "a MOVE-only hostile within range of our creep forces nothing" {
                let scout =
                    { stepped with
                        Hostiles =
                            [
                                { hostile 11 10 "Mirroar" false with
                                    MoveOnly = true
                                }
                            ]
                    }

                Expect.equal (LightTick.forced last scout) None "a scout changes no decision"
            }

            test
                "a hostile with one WORK, CARRY, CLAIM or ATTACK part within range forces a full tick" {
                // No structure of ours in the room, so an armed body is the
                // near rule's and not the armed rule's.
                for part, armed in
                    [ Work, false; Carry, false; BodyPart.Claim, false; Attack, true ] do
                    let body =
                        { stepped with
                            Structures = []
                            Hostiles = [ hostile 11 10 "Mirroar" armed ]
                        }

                    Expect.equal
                        (LightTick.forced last body)
                        (Some(LightForce.HostileNear(room, "Mirroar")))
                        $"one %A{part} part is a decision"
            }

            test "a hostile at the same coordinates in another room is not near" {
                let elsewhere =
                    { stepped with
                        Hostiles =
                            [
                                {
                                    Tile = RoomPos.at "W2N1" { X = 10; Y = 10 }
                                    Owner = "Rival"
                                    Armed = false
                                    MoveOnly = false
                                }
                            ]
                    }

                Expect.equal (LightTick.forced last elsewhere) None "range is within one room"
            }

            test "a full tick on which a structure fought forces the next one full" {
                for fight in
                    [
                        FireTower("tower", "h")
                        HealWithTower("tower", "hauler")
                        RepairWithTower("tower", "rampart")
                        ActivateSafeMode "ctrl"
                    ] do
                    let fought = LightTick.lastFull quiet Map.empty [ fight ]

                    Expect.isTrue fought.Fought $"%A{fight} is a fight"
                    Expect.isEmpty (LightTick.intents fought quiet) $"%A{fight} is never replayed"

                    Expect.equal
                        (LightTick.forced fought quiet)
                        (Some LightForce.Fought)
                        $"%A{fight} forces the next tick full"
            }

            test "a creep's shot or heal forces nothing on its own, and is never replayed" {
                // A target still in reach forces the tick on the near rule;
                // one dead or gone leaves nothing to decide.
                for shot in
                    [
                        AttackCreep("digger", "h")
                        RangedAttackCreep("digger", "h")
                        HealCreep("digger", "hauler")
                        RangedHealCreep("digger", "hauler")
                    ] do
                    let full = LightTick.lastFull quiet Map.empty [ shot ]

                    Expect.isFalse full.Fought $"%A{shot} is no structure's fight"
                    Expect.equal (LightTick.forced full quiet) None $"%A{shot} forces nothing"
                    Expect.isEmpty (LightTick.intents full quiet) $"%A{shot} is not replayed"

                // And a hostile still within reach forces the tick all the same.
                let shooting =
                    LightTick.lastFull quiet Map.empty [ RangedAttackCreep("digger", "h") ]

                let stillThere =
                    { quiet with
                        Hostiles = [ hostile 12 12 "Enemy" false ]
                    }

                Expect.equal
                    (LightTick.forced shooting stillThere)
                    (Some(LightForce.HostileNear(room, "Enemy")))
                    "the target still near is the near rule's"
            }

            test "a creep born or gone since the full tick forces a full tick" {
                let born =
                    { stepped with
                        Creeps = stepped.Creeps |> Map.add "next" (creepAt 30 31 100)
                    }

                let gone =
                    { stepped with
                        Creeps = stepped.Creeps |> Map.remove "digger"
                    }

                Expect.equal (LightTick.forced last born) (Some LightForce.CreepsChanged) "born"
                Expect.equal (LightTick.forced last gone) (Some LightForce.CreepsChanged) "gone"
            }

            test
                "a creep on the border ring with no ground inward forces a full tick, on every edge" {
                // It crossed on the full tick and stands on the neighbour's
                // ring; standing still there, the engine carries it straight
                // back, so a light tick with no step for it may not run.
                for x, y in [ 0, 20; 49, 20; 20, 0; 20, 49 ] do
                    let crossed =
                        { stepped with
                            Creeps =
                                stepped.Creeps
                                |> Map.add
                                    "hauler"
                                    {
                                        Tile = RoomPos.at "W2N1" { X = x; Y = y }
                                        Hits = 500
                                        Inward = None
                                    }
                        }

                    Expect.equal
                        (LightTick.forced last crossed)
                        (Some(LightForce.OnBorder "hauler"))
                        $"on ({x},{y})"

                Expect.equal (LightTick.forced last stepped) None "and one tile in, nothing"
            }

            test "a creep on the border ring with ground inward steps onto it, and forces nothing" {
                let crossed =
                    { stepped with
                        Creeps =
                            stepped.Creeps
                            |> Map.add
                                "hauler"
                                {
                                    Tile = RoomPos.at "W2N1" { X = 0; Y = 20 }
                                    Hits = 500
                                    Inward = Some Right
                                }
                    }

                Expect.equal
                    (LightTick.forced last crossed)
                    None
                    "the step off the ring is the light tick's"

                Expect.contains
                    (LightTick.intents last crossed)
                    (MoveCreep("hauler", Right))
                    "and it takes it"
            }

            test "inward is straight in, else an inward diagonal, onto ground, never onto the ring" {
                let open' _ = true
                let wall (walled: Pos list) (tile: Pos) = not (List.contains tile walled)

                Expect.equal (LightTick.inward open' { X = 0; Y = 20 }) (Some Right) "west edge"
                Expect.equal (LightTick.inward open' { X = 49; Y = 20 }) (Some Left) "east edge"
                Expect.equal (LightTick.inward open' { X = 20; Y = 0 }) (Some Bottom) "north edge"
                Expect.equal (LightTick.inward open' { X = 20; Y = 49 }) (Some Top) "south edge"

                Expect.equal
                    (LightTick.inward (wall [ { X = 1; Y = 20 } ]) { X = 0; Y = 20 })
                    (Some TopRight)
                    "straight in walled: the first diagonal"

                Expect.equal
                    (LightTick.inward
                        (wall [ { X = 1; Y = 20 }; { X = 1; Y = 19 } ])
                        { X = 0; Y = 20 })
                    (Some BottomRight)
                    "and the other"

                Expect.isNone
                    (LightTick.inward
                        (wall [ { X = 1; Y = 19 }; { X = 1; Y = 20 }; { X = 1; Y = 21 } ])
                        { X = 0; Y = 20 })
                    "all three walled: none"

                Expect.equal
                    (LightTick.inward open' { X = 0; Y = 1 })
                    (Some Right)
                    "beside a corner, the diagonal onto the ring is never offered"

                Expect.isNone
                    (LightTick.inward open' { X = 20; Y = 20 })
                    "off the ring there is no inward step"
            }

            test "the CPU line's reason names the rule first, then where it fired" {
                Expect.equal
                    (LightTick.tag (LightForce.ArmedHostile("W17S26", "Trepidimous")))
                    "armed W17S26 Trepidimous"
                    "an armed hostile: the room and its owner"

                Expect.equal
                    (LightTick.tag (LightForce.HostileNear("W1N1", "Invader")))
                    "near W1N1 Invader"
                    "near"

                Expect.equal
                    (LightTick.tag (LightForce.OnBorder "hauler"))
                    "border hauler"
                    "the creep on the ring"

                Expect.equal
                    (LightTick.tag (LightForce.HitsLost "hauler"))
                    "hurt hauler"
                    "the creep hurt"

                Expect.equal
                    (LightTick.tag (LightForce.ControllerChanged "W1N1"))
                    "controller W1N1"
                    "the room"

                Expect.equal (LightTick.tag LightForce.Fought) "fought" "a fight names nobody"

                Expect.equal
                    (LightTick.tag LightForce.CreepsChanged)
                    "creeps"
                    "nor does the creep census"
            }

            test "a creep that lost hits forces a full tick, and one healed does not" {
                let hurt =
                    { stepped with
                        Creeps = stepped.Creeps |> Map.add "hauler" (creepAt 21 20 499)
                    }

                let healed =
                    { stepped with
                        Creeps = stepped.Creeps |> Map.add "hauler" (creepAt 21 20 600)
                    }

                Expect.equal
                    (LightTick.forced last hurt)
                    (Some(LightForce.HitsLost "hauler"))
                    "hurt"

                Expect.equal (LightTick.forced last healed) None "healed"
            }

            test "a controller whose level or safe mode changed forces a full tick" {
                let levelled =
                    { stepped with
                        Controllers = Map.ofList [ room, (6, false) ]
                    }

                let safe =
                    { stepped with
                        Controllers = Map.ofList [ room, (5, true) ]
                    }

                let lost = { stepped with Controllers = Map.empty }

                Expect.equal
                    (LightTick.forced last levelled)
                    (Some(LightForce.ControllerChanged room))
                    "level"

                Expect.equal
                    (LightTick.forced last safe)
                    (Some(LightForce.ControllerChanged room))
                    "safe mode"

                Expect.equal
                    (LightTick.forced last lost)
                    (Some(LightForce.ControllerChanged room))
                    "lost"
            }

            test "the reasons are read in the ADR's order" {
                let everything =
                    { stepped with
                        Hostiles = [ hostile 11 10 "Invader" true ]
                        Creeps = Map.ofList [ "digger", creepAt 10 10 1 ]
                        Controllers = Map.empty
                    }

                Expect.equal
                    (LightTick.forced last everything)
                    (Some(LightForce.ArmedHostile(room, "Invader")))
                    "the armed hostile comes first"
            }

            test "a guard holding the exit a raid left by, with no hostile in reach, stays light" {
                // #450: the hold's ground is a step inside the border ring, never
                // on it, so the guard standing there trips neither `OnBorder` nor
                // anything a hostile would.
                let holding =
                    { quiet with
                        Creeps = Map.ofList [ "guard", creepAt 1 22 1000 ]
                    }

                let held = LightTick.lastFull holding Map.empty []

                Expect.isNone (LightTick.forced held holding) "nothing forces the tick full"
            }
        ]

/// Four colonies, each projecting its home and one outpost.
let private fourColonies =
    [
        "W1N1", Set.ofList [ "W1N1"; "W2N1" ]
        "W3N1", Set.ofList [ "W3N1"; "W4N1" ]
        "W5N1", Set.ofList [ "W5N1"; "W6N1" ]
        "W7N1", Set.ofList [ "W7N1"; "W8N1" ]
    ]

let private hostileIn roomName x y owner armed : GlanceHostile =
    {
        Tile = RoomPos.at roomName { X = x; Y = y }
        Owner = owner
        Armed = armed
        MoveOnly = false
    }

[<Tests>]
let resetSplitTests =
    testList
        "reset split"
        [
            test
                "a reset with one threatened colony decides it first, then the rest in order up to half" {
                let seen =
                    { quiet with
                        Hostiles = [ hostileIn "W8N1" 25 25 "Trepidimous" true ]
                    }

                let first, rest = LightTick.resetSplit 0.5 seen fourColonies

                Expect.equal
                    first
                    [ "W7N1"; "W1N1" ]
                    "the threatened colony, then the first in order"

                Expect.equal
                    rest
                    [ "W3N1"; "W5N1" ]
                    "the remaining colonies decide on the next tick"
            }

            test "an ally's or a keeper's armed body threatens nobody, and the split stays in order" {
                let seen =
                    { quiet with
                        Hostiles =
                            [
                                hostileIn "W8N1" 25 25 "Odiodin" true
                                hostileIn "W6N1" 25 25 "Source Keeper" true
                            ]
                    }

                let first, rest = LightTick.resetSplit 0.5 seen fourColonies

                Expect.equal first [ "W1N1"; "W3N1" ] "the first half in order"
                Expect.equal rest [ "W5N1"; "W7N1" ] "the second half"
            }

            test "an unarmed hostile within reach of ours threatens the colony projecting its room" {
                let seen =
                    { quiet with
                        Creeps =
                            Map.ofList
                                [
                                    "miner",
                                    { creepAt 10 10 1000 with
                                        Tile = RoomPos.at "W6N1" { X = 10; Y = 10 }
                                    }
                                ]
                        Hostiles = [ hostileIn "W6N1" 12 10 "Trepidimous" false ]
                    }

                let first, _ = LightTick.resetSplit 0.5 seen fourColonies

                Expect.equal first [ "W5N1"; "W1N1" ] "the near rule picks the colony"
            }

            test "more threatened colonies than half all decide first" {
                let seen =
                    { quiet with
                        Hostiles =
                            [
                                hostileIn "W2N1" 25 25 "Invader" true
                                hostileIn "W4N1" 25 25 "Invader" true
                                hostileIn "W8N1" 25 25 "Invader" true
                            ]
                    }

                let first, rest = LightTick.resetSplit 0.5 seen fourColonies

                Expect.equal first [ "W1N1"; "W3N1"; "W7N1" ] "every threatened colony"
                Expect.equal rest [ "W5N1" ] "only the quiet one waits"
            }

            test "a lone colony decides on the reset tick" {
                let first, rest = LightTick.resetSplit 0.5 quiet [ List.head fourColonies ]

                Expect.equal first [ "W1N1" ] "one colony is its own half"
                Expect.isEmpty rest "nothing waits"
            }

            test "the tick after a reset that deferred colonies is full" {
                let deferring =
                    { LightTick.lastFull quiet Map.empty [] with
                        Deferred = Set.ofList [ "W5N1" ]
                    }

                Expect.equal
                    (LightTick.forced deferring quiet)
                    (Some LightForce.Deferred)
                    "a quiet world still decides the deferred colonies"
            }
        ]
