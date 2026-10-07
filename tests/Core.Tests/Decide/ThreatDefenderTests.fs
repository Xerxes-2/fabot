/// The home's rampart defender: who stands the perimeter against a raid, and
/// a Threat that reaches the home across its border.
module Fabot.Core.Tests.Decide.ThreatDefenderTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.ThreatFixtures

/// MeowKittyWow's 4M2R1H shooter (2026-10-06), at a tile of the home: as
/// quick as a defender, it keeps its range and heals itself.
let private shooterAt (pos: Pos) =
    { hostileIn "W1N1" pos (List.replicate 4 Move @ List.replicate 2 RangedAttack @ [ Heal ]) with
        Id = "h-1"
        Owner = "MeowKittyWow"
    }

/// A one-block ranger of the row's, at home.
let private homeRanger = creepWith "ranger-1-Spawn1" 0 0 rangerPattern.Block

let private defenderCasts colony =
    (decideOn colony).Intents
    |> spawnIntents
    |> List.filter (fun (_, _, name) -> name.StartsWith "defender-")

/// The home's Defend's asks, each row with the bodies it wants.
let private defendAsks colony =
    let threats = threatsOf colony (Atlas.ofView colony)

    match Threats.briefFor threats (Defend "W1N1") with
    | Some brief -> brief.Asks |> List.map (fun ask -> ask.Row, ask.Wanted threats)
    | None -> []

let private defendGround colony =
    Threats.defendGroundIn (threatsOf colony (Atlas.ofView colony)) "W1N1"
    |> Set.map RoomPos.pos

[<Tests>]
let rampartDefenderTests =
    testList
        "the rampart defender"
        [
            test
                "a melee outside the line casts one defender, MOVE and ATTACK at the bank, ahead of every other row" {
                let colony = defenderHome defendedLine [ raidMeleeAt "h-1" { X = 10; Y = 34 } ] []

                Expect.equal
                    (defenderCasts colony |> List.map (fun (_, body, _) -> body))
                    [ List.replicate 6 Move @ List.replicate 6 Attack ]
                    "six blocks on 800: 780"

                Expect.equal
                    ((decideOn colony).Quotas.Rows |> List.map (fun row -> row.Row) |> List.head)
                    "defender"
                    "the first row the cascade casts"
            }

            test
                "no defender for a scout, a miner off the line, a raid inside it, no perimeter rampart, or a home that keeps none" {
                let outside = { X = 10; Y = 34 }

                let scout =
                    { raidMeleeAt "h-1" outside with
                        Body = [ Move ]
                    }

                Expect.isEmpty
                    (defenderCasts (defenderHome defendedLine [ scout ] []))
                    "a scout hurts nothing"

                let miner =
                    { raidMeleeAt "h-1" outside with
                        Body = [ Work; Carry; Move ]
                    }

                Expect.isEmpty
                    (defenderCasts (defenderHome defendedLine [ miner ] []))
                    "a WORK body walking past is no raid"

                Expect.equal
                    (defenderCasts (
                        defenderHome
                            defendedLine
                            [
                                { miner with
                                    Pos = RoomPos.at "W1N1" { X = 19; Y = 34 }
                                }
                            ]
                            []
                     )
                     |> List.length)
                    1
                    "beside a perimeter rampart it is one"

                Expect.isEmpty
                    (defenderCasts (
                        defenderHome defendedLine [ raidMeleeAt "h-1" { X = 30; Y = 34 } ] []
                    ))
                    "inside the line, broken through: the towers' and the relief's"

                Expect.isEmpty
                    (defenderCasts (defenderHome [] [ raidMeleeAt "h-1" outside ] []))
                    "nothing to stand on"

                Expect.isEmpty
                    (defenderCasts (
                        defenderHome defendedLine [ raidMeleeAt "h-1" outside ] [] |> withLevel 2
                    ))
                    "below Independent the ramparts decay away (#214)"
            }

            test "a defender owed holds the economic rows, and never a fighting one" {
                Expect.isTrue
                    ([
                        workerPattern
                        anchorPattern
                        haulerPattern
                        courierPattern
                        reserverPattern
                        upgraderPattern
                        minerPattern
                        dismantlerPattern
                     ]
                     |> List.forall heldForDefender)
                    "the economy waits"

                Expect.isFalse
                    ([
                        guardPattern
                        rangerPattern
                        defenderPattern
                        medicPattern
                        sapperPattern
                        probePattern
                     ]
                     |> List.exists heldForDefender)
                    "the fighting rows do not"

                // 300 banked, short of the defender's 780: the worker the
                // fleet is short of is not bought from it.
                let short =
                    { defenderHome defendedLine [ raidMeleeAt "h-1" { X = 10; Y = 34 } ] [] with
                        Bank = bank 300 800
                    }

                Expect.isEmpty
                    (spawnIntents (decideOn short).Intents)
                    "the bank waits for the defender"
            }

            test
                "a shooter keeping range 3 of the line calls the ranger row, not the defender, onto the ramparts its gun reaches" {
                let colony = defenderHome defendedLine [ shooterAt { X = 17; Y = 34 } ] []

                Expect.isEmpty (defenderCasts colony) "a swing that never reaches it brings nothing"

                Expect.equal
                    (defendAsks colony)
                    [ CastRow.Defender, 0; CastRow.Ranger, 1 ]
                    "the ranger's ask alone"

                Expect.equal
                    (spawnIntents (decideOn colony).Intents
                     |> List.filter (fun (_, _, name) -> name.StartsWith "ranger-")
                     |> List.map (fun (_, body, _) -> body))
                    [ rangerPattern.Block ]
                    "one ranger, one block on 800"

                Expect.equal
                    (defendGround colony)
                    (Set.ofList lineTiles)
                    "every perimeter rampart within three of it"

                Expect.isEmpty
                    (spawnIntents (decideOn { colony with Bank = bank 300 800 }).Intents)
                    "the bank waits for the ranger as for the defender"
            }

            test "a raider on the exit tile, or a healer alone outside the line, is at the line" {
                // Live t981,1xx, W17S25: Trepidimous' pair swapped on the exit
                // tile 0,15, then the healer tanked the tower there alone; the
                // flood of the outside left out the ring and a healer was no
                // raider, so no Defend was pooled and the defender stood idle.
                let onExit = defenderHome defendedLine [ raidMeleeAt "h-1" { X = 0; Y = 34 } ] []

                // A brief with no want still lists both rows at zero, so the
                // whole list is the claim.
                Expect.equal
                    (defendAsks onExit)
                    [ CastRow.Defender, 1; CastRow.Ranger, 1 ]
                    "the melee on the exit tile is answered, by both rows"

                let medic =
                    { raidMeleeAt "h-1" { X = 0; Y = 34 } with
                        Body = List.replicate 7 Heal @ List.replicate 7 Move
                    }

                Expect.equal
                    (defendAsks (defenderHome defendedLine [ medic ] []))
                    [ CastRow.Defender, 1; CastRow.Ranger, 1 ]
                    "so is the healer alone there"
            }

            test
                "a raid bouncing on the exit stands at the line on the tick it is across: on the far exit tile, or a healer remembered off ours" {
                // #552, live t981,106: the pair swapped on W17S25's 0,15 every
                // tick, so the Defend held only every other tick.
                let across =
                    { raidMeleeAt "h-1" { X = 0; Y = 0 } with
                        Pos = RoomPos.at "W2N1" { X = 49; Y = 34 }
                    }

                Expect.equal
                    (defendAsks (defenderHome defendedLine [ across ] []))
                    [ CastRow.Defender, 1; CastRow.Ranger, 1 ]
                    "the melee on W2N1's exit tile, carried onto ours next tick, as on ours"

                let remembered =
                    { defenderHome defendedLine [] [] with
                        RecalledHealers =
                            [
                                { raidMeleeAt "h-1" { X = 0; Y = 34 } with
                                    Body = List.replicate 7 Heal @ List.replicate 7 Move
                                }
                            ]
                    }

                Expect.equal
                    (defendAsks remembered)
                    [ CastRow.Defender, 1; CastRow.Ranger, 1 ]
                    "the healer remembered off the exit tile"

                Expect.isEmpty
                    (defendAsks
                        { remembered with
                            RecalledHealers =
                                remembered.RecalledHealers
                                |> List.map (fun h ->
                                    { h with
                                        Pos = RoomPos.at "W1N1" { X = 10; Y = 34 }
                                    })
                        })
                    "one remembered off another tile walked away"
            }

            test
                "a melee parked two from the ramparts, out of every swing: the defender waits on its rampart, a ranger shoots it from its own" {
                // #552: from W17S25's 2,15 a swing does not reach the exit
                // tile 0,15, a shot does.
                let colony = defenderHome defendedLine [ raidMeleeAt "h-1" { X = 18; Y = 34 } ] []

                Expect.equal
                    (defendAsks colony)
                    [ CastRow.Defender, 1; CastRow.Ranger, 0 ]
                    "walking in, it is the defender's alone: no ranger holds the economy for it"

                Expect.equal
                    (defendAsks
                        { colony with
                            Parked = Set.singleton "h-1"
                        })
                    [ CastRow.Defender, 1; CastRow.Ranger, 1 ]
                    "parked there, both rows: the melee may come to the defender, the ranger reaches it now"

                Expect.equal
                    (defendAsks (
                        defenderHome defendedLine [ raidMeleeAt "h-1" { X = 19; Y = 34 } ] []
                        |> fun c -> { c with Parked = Set.singleton "h-1" }
                    ))
                    [ CastRow.Defender, 1; CastRow.Ranger, 0 ]
                    "parked in the defender's swing, the ranger is not asked"
            }

            test "a healer alone at the line: the defender holds its rampart, a ranger shoots it" {
                // #552: the healer passed the defender's row test (no gun to
                // keep its range with), so the ranger row stayed empty.
                let medic =
                    { raidMeleeAt "h-1" { X = 17; Y = 34 } with
                        Body = List.replicate 7 Heal @ List.replicate 7 Move
                    }

                Expect.equal
                    (defendAsks (defenderHome defendedLine [ medic ] []))
                    [ CastRow.Defender, 1; CastRow.Ranger, 1 ]
                    "both rows"
            }

            test
                "the mother's resident holding the home's Guard answers the ranger row: the home casts no ranger for it, whatever its bank" {
                // #552: W15S28's residents stood in W17S25 while the child
                // was asked for a ranger of her own.
                let lent (colony: ColonyView) =
                    { colony with
                        World =
                            { colony.World with
                                Bodies =
                                    lazy (Map.ofList [ "W1N1", [ homeRanger, { X = 25; Y = 30 } ] ])
                            }
                    }

                let asksHeld (holders: string list) colony =
                    let held =
                        { HeldTaskFacts.empty with
                            Holders = Map.ofList [ taskId (Guard "W1N1"), holders ]
                        }

                    let threats = threatsOfHeld colony (Atlas.ofView colony) held

                    match Threats.briefFor threats (Defend "W1N1") with
                    | Some brief -> brief.Asks |> List.map (fun ask -> ask.Row, ask.Wanted threats)
                    | None -> []

                let shooter = defenderHome defendedLine [ shooterAt { X = 17; Y = 34 } ] []
                let thin = { shooter with Bank = bank 550 550 }

                Expect.equal
                    (asksHeld [ homeRanger.Name ] (lent shooter))
                    [ CastRow.Defender, 0; CastRow.Ranger, 0 ]
                    "the kiter is the resident's"

                Expect.equal
                    (asksHeld [ homeRanger.Name ] (lent thin))
                    [ CastRow.Defender, 0; CastRow.Ranger, 0 ]
                    "and stays the ranger row's where the bank casts no ranger"

                Expect.equal
                    (asksHeld [] (lent shooter))
                    [ CastRow.Defender, 0; CastRow.Ranger, 1 ]
                    "one passing through, holding nothing there, suppresses no cast"

                Expect.equal
                    (asksHeld [] (lent thin))
                    [ CastRow.Defender, 1; CastRow.Ranger, 0 ]
                    "nor keeps a kiter no ranger answers off the defender"
            }

            test "a ranger at home takes the Defend and shoots the shooter from its rampart" {
                let colony =
                    defenderHome
                        defendedLine
                        [ shooterAt { X = 17; Y = 34 } ]
                        [ homeRanger, { X = 20; Y = 34 } ]

                let decision = decideOn colony

                Expect.equal
                    (Map.tryFind homeRanger.Name decision.Assignments)
                    (Some(taskId (Defend "W1N1")))
                    "the ranger row's body holds it"

                Expect.contains
                    decision.Intents
                    (RangedAttackCreep(homeRanger.Name, "h-1"))
                    "its shot from cover"

                Expect.equal
                    (defendGround colony)
                    (Set.ofList lineTiles)
                    "and it keeps its own tile in the ground"
            }

            // #625: a body that keeps a raider off kites it whatever its
            // Guard's kind (`Threats.standsOffSome`), a resident holding its
            // home's Guard too; the kite walk counts our rampart unstruck,
            // so a resident standing on one against a melee at its pace it
            // keeps off stays there, as its rampart walk left it.
            test
                "a resident holding its home's Guard stands its rampart against a melee at its pace" {
                let colony =
                    defenderHome
                        defendedLine
                        [ raidMeleeAt "h-1" { X = 17; Y = 34 } ]
                        [ homeRanger, { X = 20; Y = 34 } ]

                let held = Map.ofList [ homeRanger.Name, taskId (Guard "W1N1") ]
                let decision = decide colony held Set.empty None

                Expect.equal
                    (Map.tryFind homeRanger.Name decision.Assignments)
                    (Some(taskId (Guard "W1N1")))
                    "the premise: it keeps the home's Guard"

                Expect.isFalse
                    (decision.Intents
                     |> List.exists (function
                         | MoveCreep(name, _) -> name = homeRanger.Name
                         | _ -> false))
                    "and stands on its rampart"
            }

            test "a melee raider at the line is still the defender's, and calls no ranger" {
                let colony = defenderHome defendedLine [ raidMeleeAt "h-1" { X = 19; Y = 34 } ] []

                Expect.equal (defenderCasts colony |> List.length) 1 "the melee defender"

                Expect.isFalse
                    (spawnIntents (decideOn colony).Intents
                     |> List.exists (fun (_, _, name) -> name.StartsWith "ranger-"))
                    "no ranger"

                Expect.notEqual
                    (Map.tryFind
                        homeRanger.Name
                        (decideOn (
                            defenderHome
                                defendedLine
                                [ raidMeleeAt "h-1" { X = 19; Y = 34 } ]
                                [ homeRanger, { X = 20; Y = 34 } ]
                        ))
                            .Assignments)
                    (Some(taskId (Defend "W1N1")))
                    "nor does a ranger at home take its Defend"

                Expect.equal
                    (defendAsks colony)
                    [ CastRow.Defender, 1; CastRow.Ranger, 0 ]
                    "the defender's ask alone"
            }

            // Characterisation (movement S0, `docs/adr/0088`): the Defend's line
            // ground (`defendGroundOf`), each row's ramparts within its swing or
            // its shot of the raid. Flips at S4, the one stand-tile picker.
            test
                "a melee at the rampart beside a kiter: the defender answers the melee, a ranger the kiter, each on its own ground" {
                let raid = [ raidMeleeAt "h-2" { X = 19; Y = 34 }; shooterAt { X = 17; Y = 33 } ]

                Expect.equal
                    (defendAsks (defenderHome defendedLine raid []))
                    [ CastRow.Defender, 1; CastRow.Ranger, 1 ]
                    "one ask a row, each for the raider it can strike"

                Expect.equal
                    (defenderCasts (defenderHome defendedLine raid []) |> List.length)
                    1
                    "the defender first, the row ahead of every other"

                let colony =
                    defenderHome
                        defendedLine
                        raid
                        [
                            defender "defender-1-Spawn1", { X = 20; Y = 34 }
                            homeRanger, { X = 22; Y = 30 }
                        ]

                let decision = decideOn colony

                Expect.equal
                    (Map.tryFind "defender-1-Spawn1" decision.Assignments)
                    (Some(taskId (Defend "W1N1")))
                    "the living defender keeps the Defend while the melee stands at the line"

                Expect.equal
                    (Map.tryFind homeRanger.Name decision.Assignments)
                    (Some(taskId (Defend "W1N1")))
                    "and the ranger takes it beside it"

                let threats = threatsOf colony (Atlas.ofView colony)

                Expect.equal
                    (Threats.defendGroundFor threats "W1N1" "defender-1-Spawn1"
                     |> Set.map RoomPos.pos)
                    (Set.ofList lineTiles)
                    "the defender's, beside the melee"

                Expect.equal
                    (Threats.defendGroundFor threats "W1N1" homeRanger.Name |> Set.map RoomPos.pos)
                    (Set.ofList [ { X = 20; Y = 33 }; { X = 20; Y = 35 } ])
                    "the ranger's, within three of the kiter, less the defender's tile"
            }

            test
                "a bank that casts no ranger block hands the kiter to the defender, and the economy is not held for ever" {
                let colony =
                    { defenderHome defendedLine [ shooterAt { X = 17; Y = 34 } ] [] with
                        Bank = bank 550 550
                    }

                Expect.equal
                    (defendAsks colony)
                    [ CastRow.Defender, 1; CastRow.Ranger, 0 ]
                    "a ranger's 700 is out of a 550 bank"

                Expect.equal
                    (defenderCasts colony |> List.map (fun (_, body, _) -> body))
                    [ List.replicate 4 Move @ List.replicate 4 Attack ]
                    "four blocks on 550: the defender is cast"
            }

            test
                "one defender a raid body at the line, never past the cap, and none while one lives" {
                let three =
                    [
                        raidMeleeAt "h-1" { X = 19; Y = 33 }
                        raidMeleeAt "h-2" { X = 19; Y = 34 }
                        raidMeleeAt "h-3" { X = 19; Y = 35 }
                    ]

                Expect.equal
                    (rowOf "defender" (defenderHome defendedLine three [])
                     |> Option.map (fun row -> row.Quota))
                    (Some Tuning.defaults.DefenderCap)
                    "three melee, the cap"

                Expect.isEmpty
                    (defenderCasts (
                        defenderHome
                            defendedLine
                            [ raidMeleeAt "h-1" { X = 19; Y = 34 } ]
                            [ defender "defender-1-Spawn1", { X = 30; Y = 30 } ]
                    ))
                    "the living one is the row's"
            }

            test
                "its ground is every free perimeter rampart beside the raid, less a resident's or a lent body's tile, never a Keep's" {
                let melee = [ raidMeleeAt "h-1" { X = 19; Y = 34 } ]

                // A rampart over the spawn is a Keep's, not the line's.
                let withKeepRampart = ("ram-spawn", { X = 25; Y = 25 }) :: defendedLine

                Expect.equal
                    (defendGround (defenderHome withKeepRampart melee []))
                    (Set.ofList lineTiles)
                    "all three beside the melee"

                let resident =
                    creepWith
                        "ranger-1-Spawn1"
                        0
                        0
                        (List.replicate 3 RangedAttack @ List.replicate 3 Move)

                Expect.equal
                    (defendGround (defenderHome defendedLine melee [ resident, { X = 20; Y = 33 } ]))
                    (Set.ofList [ { X = 20; Y = 34 }; { X = 20; Y = 35 } ])
                    "less the resident's"

                let lent =
                    { defenderHome defendedLine melee [] with
                        Foreign = Set.singleton (RoomPos.at "W1N1" { X = 20; Y = 35 })
                    }

                Expect.equal
                    (defendGround lent)
                    (Set.ofList [ { X = 20; Y = 33 }; { X = 20; Y = 34 } ])
                    "less the mother's lent body's"

                Expect.equal
                    (defendGround (
                        defenderHome
                            defendedLine
                            melee
                            [ defender "defender-1-Spawn1", { X = 20; Y = 34 } ]
                    ))
                    (Set.ofList lineTiles)
                    "a defender keeps its own tile in the ground"
            }

            test
                "with the raid outside but not yet at the line, its ground is the perimeter rampart nearest it" {
                Expect.equal
                    (defendGround (
                        defenderHome defendedLine [ raidMeleeAt "h-1" { X = 15; Y = 33 } ] []
                    ))
                    (Set.ofList lineTiles)
                    "all three at range 5 from (15,33), none beside it"
            }

            test "a defender swings at the melee beside it from its rampart, and never off one" {
                let onRampart =
                    defenderHome
                        defendedLine
                        [ raidMeleeAt "h-1" { X = 19; Y = 34 } ]
                        [ defender "defender-1-Spawn1", { X = 20; Y = 34 } ]

                Expect.contains
                    (decideOn onRampart).Intents
                    (AttackCreep("defender-1-Spawn1", "h-1"))
                    "on its rampart"

                // One melee at the line pools the Defend; one inside stands
                // beside the defender walking to it.
                let offRampart =
                    defenderHome
                        defendedLine
                        [
                            raidMeleeAt "h-1" { X = 19; Y = 34 }
                            raidMeleeAt "h-2" { X = 23; Y = 30 }
                        ]
                        [ defender "defender-1-Spawn1", { X = 22; Y = 30 } ]

                Expect.isFalse
                    ((decideOn offRampart).Intents
                     |> List.exists (function
                         | AttackCreep("defender-1-Spawn1", _) -> true
                         | _ -> false))
                    "off a rampart it does not swing"
            }

            // Characterisation (the Brief's S0): the Defend's pool entry and its
            // takers; its row, ground and act are pinned above.
            test
                "the home's Defend ranks on the tier, one a raid body at the line, and admits the defender row's cast by name" {
                let colony =
                    defenderHome
                        defendedLine
                        [
                            raidMeleeAt "h-1" { X = 19; Y = 33 }
                            raidMeleeAt "h-2" { X = 19; Y = 35 }
                        ]
                        [
                            defender "defender-1-Spawn1", { X = 20; Y = 34 }
                            defender "guard-1-Spawn1", { X = 22; Y = 34 }
                        ]

                Expect.equal
                    (pooledHolding [] (Defend "W1N1") colony)
                    (Some(
                        safetyRank OnTheTier,
                        Capacity.fighters 2
                        |> Capacity.capping (CapScope.Row CastRow.Defender) 2
                        |> Capacity.capping (CapScope.Row CastRow.Ranger) 0
                    ))
                    "one Defend for the home, on the Safety tier, capped at the raid at its line, row by row (#546)"

                let decision = decideOn colony

                Expect.equal
                    (Map.tryFind "defender-1-Spawn1" decision.Assignments)
                    (Some(taskId (Defend "W1N1")))
                    "the defender takes it"

                Expect.notEqual
                    (Map.tryFind "guard-1-Spawn1" decision.Assignments)
                    (Some(taskId (Defend "W1N1")))
                    "and the same body under another row's name is refused it"
            }
        ]

/// `defenderHome` with the room west of it, W2N1, joined to the home's
/// corridor across x = 0, y 33..35: its own ground x 40..48 on those rows
/// and the matching exits on its east column, `hostiles` standing wherever
/// the caller puts them, in either room. `perimeter` false is the same home
/// with no line declared.
let private borderHome perimeter ramparts hostiles creeps =
    let colony = defenderHome ramparts hostiles creeps

    let westRing = Map.ofList [ for y in 33..35 -> { X = 49; Y = y }, Plain ]

    { colony with
        Perimeter = if perimeter then colony.Perimeter else []
        Spatial =
            { colony.Spatial with
                Borders = Map.add "W2N1" westRing colony.Spatial.Borders
            }
            |> withNeighbour
                "W2N1"
                { RoomLayer.empty with
                    Terrain =
                        TerrainGrid.ofList
                            [
                                for x in 40..48 do
                                    for y in 33..35 -> { X = x; Y = y }, Plain
                            ]
                }
    }

/// Trepidimous' melee standing in W2N1.
let private meleeWestAt (pos: Pos) =
    { raidMeleeAt "h-west" pos with
        Pos = RoomPos.at "W2N1" pos
    }

let private homeReach colony =
    Threats.reachIn (threatsOf colony (Atlas.ofView colony)) "W1N1"

let private reserverAt (pos: Pos) =
    creepWith "reserver-1" 0 0 [ BodyPart.Claim; Move ], pos

/// The same colony with the home under our own safe mode.
let private shieldedHome (colony: ColonyView) =
    { colony with
        RoomControl =
            Map.add
                "W1N1"
                { ownedRoom with
                    Owner = Ownership.Ours
                    SafeMode = true
                }
                colony.RoomControl
    }

/// The one body's tiles over `ticks` decided ticks, each tick's move applied
/// as the engine would, its own tile first.
let private walkOf name (start: Pos) ticks (colonyAt: Pos -> ColonyView) =
    (start, [ 1..ticks ])
    ||> List.scan (fun at _ ->
        match moveIntentsFor name (decideOn (colonyAt at)).Intents with
        | [ MoveCreep(_, direction) ] -> stepFrom at direction
        | _ -> at)

[<Tests>]
let acrossTheBorderTests =
    testList
        "a Threat across the border"
        [
            // Movement S2 (`docs/adr/0088`) flipped the S0 pin here, which read
            // the cross-border Reach as the radius less the walk to the exit and
            // the carry (`reachAcross`): the melee's flood now carries across
            // the exit, landing it on x = 0 at tick 1 and on x = 1 at tick 2,
            // whose swing is x = 2.
            test
                "a melee beside an exit reaches across as far as it walks and swings within the horizon" {
                // #514, live t958,965: the 18A melee on W18S25's east edge put no
                // Reach on W17S25's x = 0..2, and the reserver idling beside the
                // exit there died the tick the melee stepped across. Here the
                // corridor is rows 33..35.
                let reach = homeReach (borderHome true [] [ meleeWestAt { X = 48; Y = 34 } ] [])

                for x in 0..2 do
                    for y in 33..35 do
                        Expect.contains
                            reach
                            { X = x; Y = y }
                            $"({x},{y}) is struck within two ticks across"

                Expect.isTrue
                    (reach |> Set.forall (fun tile -> tile.X <= 2))
                    $"nothing past what it reaches in two ticks: {reach}"

                Expect.isEmpty
                    (homeReach (borderHome true [] [ meleeWestAt { X = 45; Y = 34 } ] []))
                    "a melee four columns short of the exit reaches nothing across it"
            }

            test "a ranged one reaches across by its range from where it walks" {
                // Landed on x = 0 at tick 1 and on x = 1 at tick 2 (movement S2).
                let ranged =
                    { meleeWestAt { X = 48; Y = 34 } with
                        Body = [ RangedAttack; Move ]
                    }

                let reach = homeReach (borderHome true [] [ ranged ] [])

                Expect.contains reach { X = 4; Y = 34 } "four columns in"

                Expect.isTrue
                    (reach |> Set.forall (fun tile -> tile.X <= 4))
                    $"and no further: {reach}"
            }

            test "a rampart of ours across the border still takes its tile back" {
                let reach =
                    homeReach (
                        borderHome
                            true
                            [ "ram-x", { X = 1; Y = 34 } ]
                            [ meleeWestAt { X = 48; Y = 34 } ]
                            []
                    )

                Expect.isFalse
                    (Set.contains { X = 1; Y = 34 } reach)
                    "the ramparted tile is in no Reach"

                Expect.contains reach { X = 1; Y = 33 } "the bare one beside it is"
            }

            // #566, live W16S26 t980,640-980,650: two rangers stood healing 84
            // under a stronghold's 480 a tick, three tiles from the exit, until
            // they died. A room's hostile towers are one arrival class striking
            // every tile of it now (movement S2): the room has no safe tile, so
            // Flee's ground is across the nearest border whose landing no Reach
            // covers, and a body its towers out-damage walks out over it.
            test "a ranger under a stronghold's towers it cannot out-heal walks out over the exit" {
                let towered heal =
                    let colony = borderHome true [] [] []
                    let west = SpatialInfo.layerOf colony.Spatial "W2N1"

                    let ranger =
                        creepWith
                            "r1"
                            0
                            0
                            (List.replicate 21 Move
                             @ List.replicate 14 RangedAttack
                             @ List.replicate heal Heal)

                    { colony with
                        Creeps = colony.Creeps @ [ ranger ]
                        Spatial =
                            colony.Spatial
                            |> withNeighbour
                                "W2N1"
                                { west with
                                    CreepPositions = Map.ofList [ "r1", { X = 44; Y = 34 } ]
                                }
                        World =
                            { colony.World with
                                Rooms =
                                    Map.add
                                        "W2N1"
                                        {
                                            Hostiles = []
                                            HostileFatigue = Map.empty
                                            HostileCargo = Map.empty
                                            InvaderCores = []
                                            // Past the falloff: 150 a tick on every tile we stand on.
                                            Towers = [ "tower-1", { X = 10; Y = 34 } ]
                                            OurTowers = []
                                            Fallen = []
                                            Control =
                                                {
                                                    Owner = Ownership.Unowned
                                                    Reservation = None
                                                    SafeMode = false
                                                    SafeModeCooldownUntil = 0
                                                    Sign = None
                                                }
                                        }
                                        colony.World.Rooms
                            }
                    }

                let outOf heal =
                    let decision = decideOn (towered heal)

                    Map.tryFind "r1" decision.Assignments,
                    moveIntentsFor "r1" decision.Intents
                    |> List.map (function
                        | MoveCreep(_, direction) -> stepFrom { X = 44; Y = 34 } direction
                        | _ -> { X = 44; Y = 34 })

                let task, steps = outOf 7

                Expect.equal task (Some(taskId Flee)) "seven HEAL, 84 a tick under 150: it runs"

                Expect.isTrue
                    (steps |> List.forall (fun step -> step.X = 45))
                    $"east, toward the exit into W1N1: {steps}"

                Expect.equal
                    (fst (outOf 13))
                    None
                    "thirteen HEAL, 156 a tick over 150: it out-heals the towers and stays"

                // The one border open here leads into a room no route enters
                // (a rival's, a stronghold's): no ground across it is Flee's.
                let barred =
                    let colony = towered 7

                    { colony with
                        Spatial =
                            { colony.Spatial with
                                RivalRooms = Set.singleton "W1N1"
                            }
                    }

                let threats = threatsOf barred (Atlas.ofView barred)
                let ranger = barred.Creeps |> List.find (fun c -> c.Name = "r1")

                Expect.isEmpty
                    (Threats.fleeAreaFor threats (Atlas.ofView barred) ranger)
                    "no way out across a border into a room no route enters"
            }

            test "a body beside the exit inside the cross-border Reach takes Flee" {
                let decision =
                    decideOn (
                        borderHome
                            true
                            []
                            [ meleeWestAt { X = 48; Y = 34 } ]
                            [ reserverAt { X = 1; Y = 34 } ]
                    )

                Expect.equal
                    (Map.tryFind "reserver-1" decision.Assignments)
                    (Some(taskId Flee))
                    "it runs from the melee next door"

                let threats, atlas, reserver =
                    let colony =
                        borderHome
                            true
                            []
                            [ meleeWestAt { X = 48; Y = 34 } ]
                            [ reserverAt { X = 1; Y = 34 } ]

                    let atlas = Atlas.ofView colony

                    threatsOf colony atlas,
                    atlas,
                    colony.Creeps |> List.find (fun c -> c.Name = "reserver-1")

                Expect.isTrue
                    (Threats.fleeAreaFor threats atlas reserver
                     |> Set.forall (fun tile -> tile.Room = "W1N1"))
                    "at home it flees in the room"
            }

            // #511 (movement S4): live t947,504, worker-946920-Spawn8 fled
            // inside W15S29 from two Invaders and was stripped to 16 MOVE: an
            // outpost's safe set is small and moves with the raid. An unarmed
            // body caught away from home flees home across the exits it
            // reaches before any hostile, and in the room where every way home
            // crosses the raid.
            test "an unarmed body caught in the outpost flees home, unless the raid holds the way" {
                let caught (melee: Pos) (at: Pos) =
                    let colony = borderHome true [] [ meleeWestAt melee ] []
                    let west = SpatialInfo.layerOf colony.Spatial "W2N1"
                    let reserver, _ = reserverAt at

                    { colony with
                        Creeps = colony.Creeps @ [ reserver ]
                        Spatial =
                            colony.Spatial
                            |> withNeighbour
                                "W2N1"
                                { west with
                                    CreepPositions = Map.ofList [ reserver.Name, at ]
                                }
                    }

                let areaOf colony =
                    let atlas = Atlas.ofView colony
                    let reserver = colony.Creeps |> List.find (fun c -> c.Name = "reserver-1")
                    Threats.fleeAreaFor (threatsOf colony atlas) atlas reserver

                // The melee at the corridor's west end, the reserver between it
                // and the exits home.
                let homeward = caught { X = 40; Y = 34 } { X = 43; Y = 34 }
                let decision = decideOn homeward

                Expect.equal
                    (Map.tryFind "reserver-1" decision.Assignments)
                    (Some(taskId Flee))
                    "the premise: it flees"

                let area = areaOf homeward

                Expect.isTrue
                    (not (Set.isEmpty area) && area |> Set.forall (fun tile -> tile.Room = "W1N1"))
                    $"its ground is home, beside the landings: {area}"

                let step = moveIntentsFor "reserver-1" decision.Intents

                Expect.isTrue
                    (not (List.isEmpty step)
                     && step
                        |> List.forall (function
                            | MoveCreep(_, (Right | TopRight | BottomRight)) -> true
                            | _ -> false))
                    $"and it steps east, toward the exits home: {step}"

                // The melee beside the exits home: it is on every one before
                // the reserver, which flees west in the outpost instead.
                let cut = areaOf (caught { X = 47; Y = 34 } { X = 44; Y = 34 })

                Expect.isTrue
                    (not (Set.isEmpty cut) && cut |> Set.forall (fun tile -> tile.Room = "W2N1"))
                    $"the raid holds the way home: it flees in the room: {cut}"
            }

            // #602: an armed body flees only with no fight to hold (#533), and
            // then home as any other. Live W19S26 t1,007,230-417, ranger-1006821
            // traded with a shooter at its own pace until it died: struck by a
            // chaser at its pace, a body outruns nothing in the room, and any
            // exit home it walks to is its way out.
            test
                "an armed body caught in the outpost flees home too, and so does one a chaser at its pace strikes" {
                let block = Bodies.rangerPattern.Block

                let shooterWestAt (pos: Pos) =
                    { hostileIn
                          "W2N1"
                          pos
                          (List.replicate 3 RangedAttack
                           @ List.replicate 7 Move
                           @ List.replicate 4 Heal) with
                        Id = "h-west"
                    }

                let groundAmong (wide: bool) (hostiles: HostileInfo list) (at: Pos) =
                    let colony = borderHome true [] hostiles []
                    let west = SpatialInfo.layerOf colony.Spatial "W2N1"
                    let ranger = creepWith "ranger-1" 0 0 (block @ block)

                    // Wide, the outpost is plain to its borders: ground out
                    // of the Reach to flee to in the room.
                    let terrain =
                        if wide then
                            TerrainGrid.ofList
                                [
                                    for x in 1..48 do
                                        for y in 1..48 -> { X = x; Y = y }, Plain
                                ]
                        else
                            west.Terrain

                    let caught =
                        { colony with
                            Creeps = colony.Creeps @ [ ranger ]
                            Spatial =
                                colony.Spatial
                                |> withNeighbour
                                    "W2N1"
                                    { west with
                                        Terrain = terrain
                                        CreepPositions = Map.ofList [ ranger.Name, at ]
                                    }
                        }

                    let atlas = Atlas.ofView caught
                    Threats.fleeAreaFor (threatsOf caught atlas) atlas ranger

                let groundIn wide (hostile: HostileInfo) at = groundAmong wide [ hostile ] at
                let groundOf = groundIn false

                let home (area: Set<RoomPos>) =
                    not (Set.isEmpty area) && area |> Set.forall (fun tile -> tile.Room = "W1N1")

                let ahead = groundOf (meleeWestAt { X = 40; Y = 34 }) { X = 43; Y = 34 }

                Expect.isTrue
                    (home ahead)
                    $"a melee three behind it: home, as the unarmed body's: {ahead}"

                let chased = groundOf (shooterWestAt { X = 42; Y = 34 }) { X = 43; Y = 34 }

                Expect.isTrue
                    (home chased)
                    $"a shooter at its pace beside it, first on every tile: home all the same: {chased}"

                // #608 review: a raider camped across the border, five off the
                // landings, strikes them before the chased body is on one: no
                // landing is its way out, however soon its own tile is struck.
                let camper =
                    { hostileIn
                          "W1N1"
                          { X = 5; Y = 34 }
                          (List.replicate 3 RangedAttack @ List.replicate 3 Move) with
                        Id = "h-home"
                    }

                let camped =
                    groundAmong
                        false
                        [ shooterWestAt { X = 42; Y = 34 }; camper ]
                        { X = 43; Y = 34 }

                Expect.isFalse
                    (camped |> Set.exists (fun tile -> tile.Room = "W1N1"))
                    $"a raider camped across: never home onto its strike: {camped}"

                // The shooter between it and the exits home: never past it.
                let cut = groundIn true (shooterWestAt { X = 44; Y = 34 }) { X = 43; Y = 34 }

                Expect.isTrue
                    (not (Set.isEmpty cut) && cut |> Set.forall (fun tile -> tile.Room = "W2N1"))
                    $"the shooter holds the way home: it flees in the room: {cut}"
            }

            // Movement S4 review: an exit reached first is no way home when
            // every walk to it crosses a strike. A shooter behind a wall
            // (y 35-36) covers the road along y = 34 to the exits home; it
            // never reaches the exits, so they alone read as reached first.
            test
                "a body flees home only by a walk it is on first, and never steps into the strike on the road" {
                let shooter = { X = 36; Y = 37 }

                let flanked (northOpen: bool) (fatigue: int) =
                    let raid =
                        { hostileIn "W2N1" shooter [ RangedAttack; RangedAttack; Move ] with
                            Id = "h-west"
                            Owner = "Trepidimous"
                        }

                    let colony = borderHome true [] [ raid ] []
                    let west = SpatialInfo.layerOf colony.Spatial "W2N1"

                    let reserver =
                        { creepWith "reserver-1" 0 0 [ BodyPart.Claim; Move ] with
                            Fatigue = fatigue
                        }

                    let wall (tile: Pos) =
                        tile.Y = 35
                        || tile.Y = 36
                        || (not northOpen && tile.X >= 35 && tile.Y <= 33)

                    { colony with
                        Creeps = colony.Creeps @ [ reserver ]
                        Spatial =
                            colony.Spatial
                            |> withNeighbour
                                "W2N1"
                                { west with
                                    Terrain =
                                        TerrainGrid.ofList
                                            [
                                                for x in 20..48 do
                                                    for y in 25..45 ->
                                                        { X = x; Y = y },
                                                        (if wall { X = x; Y = y } then
                                                             Wall
                                                         else
                                                             Plain)
                                            ]
                                    Roads = Set.ofList [ for x in 34..48 -> { X = x; Y = 34 } ]
                                    CreepPositions =
                                        Map.ofList [ reserver.Name, { X = 34; Y = 34 } ]
                                }
                    }

                let areaOf colony =
                    let atlas = Atlas.ofView colony
                    let body = colony.Creeps |> List.find (fun c -> c.Name = "reserver-1")
                    Threats.fleeAreaFor (threatsOf colony atlas) atlas body

                let struck (tile: Pos) =
                    range tile shooter <= Engine.rangedRange

                let open' = flanked true 0

                Expect.isTrue
                    (areaOf open' |> Set.forall (fun tile -> tile.Room = "W1N1"))
                    "the walk north of the road reaches the exits first: home"

                let decision = decideOn open'

                Expect.equal
                    (Map.tryFind "reserver-1" decision.Assignments)
                    (Some(taskId Flee))
                    "the premise"

                match moveIntentsFor "reserver-1" decision.Intents with
                | [ MoveCreep(_, direction) ] ->
                    let step = stepFrom { X = 34; Y = 34 } direction
                    Expect.isFalse (struck step) $"off the road the shooter covers: {step}"
                | other -> failtest $"one step: {other}"

                // Held by its fatigue, the step its light tick replays is the
                // same walk's, not the road's.
                let held = decideOn (flanked true 2)

                match Map.tryFind "reserver-1" held.Steps with
                | Some(_ :: next :: _) ->
                    Expect.isFalse
                        (struck (RoomPos.pos next))
                        $"its planned step is off the road: {next}"
                | other -> failtest $"a planned step: {other}"

                // With the north walled, the road is the only way to the exits:
                // it flees in the outpost, away from the shooter.
                let cut = areaOf (flanked false 0)

                Expect.isTrue
                    (not (Set.isEmpty cut) && cut |> Set.forall (fun tile -> tile.Room = "W2N1"))
                    $"no walk home it is on first: it flees in the room: {cut}"
            }

            test "an idle body beside a home exit walks in, raid across it or none" {
                // Three columns short: the melee's Reach stops short of the
                // exit, so the reserver on (1,34) is in none and holds nothing.
                // Moved 2026-10-07: with no raid it used to stand there; the
                // tiles beside an exit are idle ground now (a doorway blocks).
                let raid = [ meleeWestAt { X = 46; Y = 34 } ]
                let placed = [ reserverAt { X = 1; Y = 34 } ]

                let quiet = decideOn (borderHome true [] [] placed)
                let raided = decideOn (borderHome true [] raid placed)

                Expect.isNonEmpty
                    (moveIntentsFor "reserver-1" quiet.Intents)
                    "with nothing next door it steps off the doorway too"

                Expect.isNone
                    (Map.tryFind "reserver-1" raided.Assignments)
                    "the premise: in no Reach, it holds nothing"

                let step = moveIntentsFor "reserver-1" raided.Intents

                Expect.isTrue
                    (step
                     |> List.forall (function
                         | MoveCreep(_, (Right | TopRight | BottomRight)) -> true
                         | _ -> false)
                     && not (List.isEmpty step))
                    $"it walks in along the corridor, away from the exit: {step}"
            }

            // Characterisation (movement S0, `docs/adr/0088`): the shelter and
            // its exit band (`shelterOf`, `shelterDepth` 5). Flips at S4, where
            // the band is a map reading.
            test
                "the ground it walks to is behind the declared line, else five in from the exits, never beside a spawn" {
                let raid = [ meleeWestAt { X = 46; Y = 34 } ]

                let shelter colony =
                    Threats.shelterIn (threatsOf colony (Atlas.ofView colony)) "W1N1"
                    |> Set.map RoomPos.pos

                let behind = shelter (borderHome true [] raid [])

                Expect.contains behind { X = 21; Y = 34 } "the first tile behind the line"

                Expect.isTrue
                    (behind |> Set.forall (fun tile -> tile.X > 20))
                    "and nothing on the line or outside it"

                let spawnPos = { X = 25; Y = 25 }

                Expect.isTrue
                    (behind |> Set.forall (fun tile -> range tile spawnPos > 1))
                    "and no tile beside the spawn, which a cast steps out onto"

                let inland = shelter (borderHome false [] raid [])

                Expect.contains
                    inland
                    { X = 5; Y = 34 }
                    "with no line, the first tile five in from the exits"

                Expect.isFalse (Set.contains { X = 4; Y = 34 } inland) "not one nearer"

                Expect.isTrue
                    (inland |> Set.forall (fun tile -> range tile spawnPos > 1))
                    "nor beside the spawn"

                Expect.isEmpty
                    (shelter (borderHome true [] [] []))
                    "and none at all with nothing next door"
            }

            test "a body walks from beside the exit all the way onto the shelter, and stays on it" {
                let raid = [ meleeWestAt { X = 46; Y = 34 } ]

                for perimeter, onShelter, ticks in
                    [
                        true, (fun (tile: Pos) -> tile.X > 20), 30
                        false, (fun tile -> tile.X >= 5), 10
                    ] do
                    let walk =
                        walkOf "reserver-1" { X = 1; Y = 34 } ticks (fun at ->
                            borderHome perimeter [] raid [ reserverAt at ])

                    let arrived = walk |> List.skipWhile (onShelter >> not)

                    Expect.isNonEmpty arrived $"it arrives (line {perimeter}): {walk}"

                    Expect.isTrue
                        (List.forall onShelter arrived)
                        $"and never steps off it again (line {perimeter}): {arrived}"

                    Expect.isTrue
                        (walk
                         |> List.takeWhile (onShelter >> not)
                         |> List.pairwise
                         |> List.forall (fun (a, b) -> b <> a && b.X >= a.X))
                        $"stepping in every tick until it does (line {perimeter}): {walk}"

                    if not perimeter then
                        Expect.equal
                            (List.last walk).X
                            5
                            $"with no line it stops five in, nowhere near the spawn: {walk}"
            }

            test "an idle body already in the interior does not move for a raid next door" {
                let raid = [ meleeWestAt { X = 46; Y = 34 } ]

                for perimeter in [ true; false ] do
                    let decision =
                        decideOn (borderHome perimeter [] raid [ reserverAt { X = 32; Y = 30 } ])

                    Expect.isEmpty
                        (moveIntentsFor "reserver-1" decision.Intents)
                        $"it stands where it is (line {perimeter})"
            }

            test "a Work-heavy body off its Post in the Reach across the border flees inward" {
                // Off its Post an anchor runs like any other body (#515):
                // standing in the spilled Reach it takes Flee, whose way out is
                // inward. No rock at home, so it has no dig to walk to either.
                let decision =
                    decideOn
                        { borderHome
                              true
                              []
                              [ meleeWestAt { X = 48; Y = 34 } ]
                              [ anchor "a-1" 0 50, { X = 1; Y = 34 } ] with
                            Sources = []
                        }

                Expect.equal
                    (Map.tryFind "a-1" decision.Assignments)
                    (Some(taskId Flee))
                    "the premise: Flee for it, and no work"

                let step = moveIntentsFor "a-1" decision.Intents

                Expect.isTrue
                    (step
                     |> List.forall (function
                         | MoveCreep(_, (Right | TopRight | BottomRight)) -> true
                         | _ -> false)
                     && not (List.isEmpty step))
                    $"it steps inward: {step}"
            }

            test
                "a Work-heavy body where it digs in the Reach across the border takes the shelter's inward step" {
                // On a bare home Seat it digs from, the anchor has no Flee;
                // its rock's Seats are all hot, so it holds nothing, and the
                // shelter walk takes the plain step inward through the Reach
                // rather than leave it standing in it. The idle rule alone
                // would leave it there: with the spawn taken out its rock's
                // Seats are the room's whole idle ground, and every tile off
                // them is hot.
                let border =
                    borderHome
                        true
                        []
                        [
                            { meleeWestAt { X = 48; Y = 34 } with
                                Body = [ RangedAttack; Move ]
                            }
                        ]
                        [ anchor "a-1" 0 50, { X = 1; Y = 34 } ]

                let colony =
                    { border with
                        Sources = [ source "src-x" ]
                        Spatial =
                            { border.Spatial with
                                TargetKinds = border.Spatial.TargetKinds |> Map.remove "spawn-1"
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    Terrain =
                                        layer.Terrain |> TerrainGrid.add { X = 1; Y = 33 } Wall
                                    TargetPositions = layer.TargetPositions |> Map.remove "spawn-1"
                                    Obstacles = Set.empty
                                })
                    }
                    |> withTarget "src-x" { X = 1; Y = 33 } Source

                Expect.isTrue
                    ([ 1, 34; 1, 35; 2, 33; 2, 34; 2, 35; 3, 33; 3, 34; 3, 35 ]
                     |> List.forall (fun (x, y) -> Set.contains { X = x; Y = y } (homeReach colony)))
                    "the premise: the rock's Seats and the ground off them are in the archer's spilled Reach"

                let decision = decideOn colony

                Expect.isNone
                    (Map.tryFind "a-1" decision.Assignments)
                    "the premise: no Flee for it, and no work"

                let step = moveIntentsFor "a-1" decision.Intents

                Expect.isTrue
                    (step
                     |> List.forall (function
                         | MoveCreep(_, (Right | TopRight)) -> true
                         | _ -> false)
                     && not (List.isEmpty step))
                    $"it steps inward: {step}"
            }

            test
                "under the home's own safe mode nothing reaches across into it and no shelter is laid" {
                let colony = shieldedHome (borderHome true [] [ meleeWestAt { X = 48; Y = 34 } ] [])
                let threats = threatsOf colony (Atlas.ofView colony)

                Expect.isEmpty (Threats.reachIn threats "W1N1") "no Reach lands in a shielded room"
                Expect.isEmpty (Threats.shelterIn threats "W1N1") "and nothing is sheltered from"

                Expect.isNonEmpty
                    (Threats.reachIn threats "W2N1")
                    "the premise: the melee keeps its Reach where it stands"
            }

            test "a raid stood down from beside the home lays the shelter with nothing in sight" {
                // The last body out of the outpost took our vision with it.
                let blind raided =
                    { borderHome true [] [] [ reserverAt { X = 1; Y = 34 } ] with
                        RaidedOutposts = raided
                    }

                let shelter colony =
                    Threats.shelterIn (threatsOf colony (Atlas.ofView colony)) "W1N1"

                Expect.isNonEmpty (shelter (blind (Set.singleton "W2N1"))) "the room beside home"

                Expect.isEmpty
                    (shelter (blind (Set.singleton "W5N5")))
                    "not a raided room that borders it nowhere"

                let step =
                    moveIntentsFor "reserver-1" (decideOn (blind (Set.singleton "W2N1"))).Intents

                Expect.isTrue
                    (step
                     |> List.forall (function
                         | MoveCreep(_, (Right | TopRight | BottomRight)) -> true
                         | _ -> false)
                     && not (List.isEmpty step))
                    $"and the body beside the exit walks in: {step}"
            }

            test
                "two ticks under one census share one line and one shelter ground; a moved census or perimeter lays them again" {
                // #519, live 2026-10-05: the line's flood and the shelter's ground
                // were rebuilt off ~2,000 tiles on every raid tick.
                let colony = borderHome true defendedLine [ meleeWestAt { X = 48; Y = 34 } ] []

                let held = FarFieldMemo.empty ()

                let tick () =
                    Atlas.ofViewRecalling (FarFieldMemo.walks ()) held colony

                let first = tick ()
                let second = tick ()
                let outside = Atlas.outsideLine first colony.Perimeter
                let ground = Atlas.shelterGround first colony.Perimeter 5

                Expect.isTrue
                    (obj.ReferenceEquals(outside, Atlas.outsideLine second colony.Perimeter))
                    "one flood across the two ticks"

                Expect.isTrue
                    (obj.ReferenceEquals(ground, Atlas.shelterGround second colony.Perimeter 5))
                    "one ground across them"

                Expect.equal
                    (Threats.shelterIn (threatsOf colony second) "W1N1")
                    (Threats.shelterIn (threatsOf colony (Atlas.ofView colony)) "W1N1")
                    "the recalled ground lays the shelter a fresh one does"

                let moved = tick ()
                Atlas.evictRooms moved Set.empty (Set.singleton "W1N1")
                let relaid = Atlas.outsideLine moved colony.Perimeter

                Expect.isFalse
                    (obj.ReferenceEquals(outside, relaid))
                    "the home's census moved: laid again"

                Expect.equal relaid outside "to the same tiles"

                let shorter = List.tail colony.Perimeter

                Expect.equal
                    (Atlas.outsideLine moved shorter)
                    (Atlas.outsideLine (Atlas.ofView colony) shorter)
                    "a perimeter other than the one it was laid for is laid for its own"
            }

            test
                "an obstacle site placed behind the line lays the shelter ground again through the decide seam" {
                // #519: the site moves the home's census signature, which evicts the
                // home's line, so the next tick's ground is laid off the new grid.
                let site = { X = 30; Y = 30 }

                let colony =
                    borderHome
                        true
                        defendedLine
                        [ meleeWestAt { X = 48; Y = 34 } ]
                        [ reserverAt { X = 35; Y = 30 } ]

                let sited =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withTargets [ "site-1", site, Site BuiltKind.Extension ]
                            |> withHome (fun layer ->
                                { layer with
                                    Obstacles = Set.add site layer.Obstacles
                                })
                    }

                // Read off each tick as it lands: the tables are one `Dictionary`
                // handed on, which the next tick writes in place.
                let groundOf (decision: Decision) =
                    decision.Memo.Lines.["W1N1"].Ground |> Option.map snd

                let first = decide colony Map.empty Set.empty None
                let firstGround = groundOf first
                let held = decide colony Map.empty Set.empty (Some first.Memo)
                let heldGround = groundOf held
                let next = decide sited Map.empty Set.empty (Some held.Memo)

                Expect.isTrue
                    (firstGround |> Option.exists (Set.contains (RoomPos.at "W1N1" site)))
                    "the premise: the reserver idling at home forced a ground holding the tile"

                Expect.isTrue
                    (match firstGround, heldGround with
                     | Some a, Some b -> obj.ReferenceEquals(a, b)
                     | _ -> false)
                    "an unchanged census recalls the ground"

                Expect.equal
                    (groundOf next)
                    (Some(Atlas.shelterGround (Atlas.ofView sited) sited.Perimeter 5))
                    "the moved census lays it off the new grid, the site's tile gone"
            }
        ]
