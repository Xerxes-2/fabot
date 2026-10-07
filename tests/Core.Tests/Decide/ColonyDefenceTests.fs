/// One colony defending another's home: the mother's Guard over a beaten
/// home, and the resident garrison she keeps in a child's home she raises,
/// its ground, its targets and its rows.
module Fabot.Core.Tests.Decide.ColonyDefenceTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.ColonyFixtures

/// W17S29's live raid (#428), filed in the pair's north room: one
/// `18M17A` and two `11M7H`, 168 heal a tick against the 150 one tower lands
/// at the falloff range.
let private homeRaid =
    [
        yield
            { hostileIn
                  "W1N2"
                  { X = 10; Y = 44 }
                  (List.replicate 17 Attack @ List.replicate 18 Move) with
                Id = "atk"
            }
        for i in 1..2 ->
            { hostileIn "W1N2" { X = 10; Y = 45 } (List.replicate 7 Heal @ List.replicate 11 Move) with
                Id = $"med-{i}"
            }
    ]

/// The mother while the child's home is beaten: W1N2 is on her view as a
/// defended home (`BorrowedWork.Defended`), projected as the ground her
/// guard crosses and nothing more, with a W15S28-sized bank to cast from.
let private defendingMother raid =
    let colony = northBorderColony { X = 10; Y = 38 } |> withNorthOutpost None

    { colony with
        Spawns = [ spawn ]
        Bank = bank 5_600 5_600
        Hostiles = raid
        Borrowed =
            { BorrowedWork.empty with
                Defended = [ "W1N2" ]
            }
    }

/// The child in the same tick: its own home under the same raid, and a bank
/// that buys one guard block.
let private raidedChildColony raid =
    { childColony [ childCast, { X = 10; Y = 44 } ] with
        Spawns = [ spawn ]
        Bank = bank 1_300 1_300
        Hostiles = raid
    }

let private guardsIn tasks =
    tasks
    |> List.filter (function
        | Guard _ -> true
        | _ -> false)

[<Tests>]
let defendedHomeTests =
    testList
        "another colony defends a home"
        [
            test "the defender pools the Guard of a beaten home, and its guard row casts for it" {
                // #589: no one body of hers wins the squad, the two she
                // casts do, and a home fights on whatever the count.
                let raided = defendingMother homeRaid
                let quiet = defendingMother []

                Expect.isEmpty
                    (guardsIn (planTasksOn quiet (noThreats ())))
                    "the premise: a defended home with nobody in it is no fight"

                Expect.equal
                    (guardsIn (planTasksOn raided (noThreats ())))
                    [ Guard "W1N2" ]
                    "raided, the home is one Guard in her pool, under the room's name"

                let overwhelming =
                    homeRaid
                    @ [
                        for i in 3..8 ->
                            { hostileIn
                                  "W1N2"
                                  { X = 10; Y = 45 }
                                  (List.replicate 7 Heal @ List.replicate 11 Move) with
                                Id = $"med-{i}"
                            }
                    ]

                Expect.equal
                    (guardsIn (planTasksOn (defendingMother overwhelming) (noThreats ())))
                    [ Guard "W1N2" ]
                    "and a raid even her two lose is still hers to fight"

                Expect.equal
                    (guardQuotaOf raided)
                    (Some 2)
                    "one guard loses the raid alone, so the row wants two"

                Expect.equal
                    (guardCasts (decideOn raided).Intents |> List.map List.length)
                    [ Engine.maxBodyParts ]
                    "and her spawn casts one, the biggest a body carries"
            }

            test "her melee guard's ground under a melee raid is still the threats' ring" {
                // #451's kite ground is a ranger's: a melee guard closes.
                let raided = defendingMother homeRaid
                let threats = threatsOf raided (Atlas.ofView raided)

                Expect.isNonEmpty (Threats.ringIn threats "W1N2") "the premise: a ring stands"

                Expect.equal
                    (Threats.guardGroundIn threats "W1N2")
                    (Some(Threats.ringIn threats "W1N2"))
                    "and the Guard takes it"
            }

            test "the child's own guard row, which loses the raid, casts nothing for its home" {
                let raided = raidedChildColony homeRaid

                Expect.isEmpty
                    (guardsIn (planTasksOn raided (noThreats ())))
                    "no Guard in its own home"

                Expect.isEmpty (guardCasts (decideOn raided).Intents) "and no guard cast for it"
            }
        ]

let private rangerQuotaOf colony =
    rowOf "ranger" colony |> Option.map (fun row -> row.Quota)

/// A mother raising the child W1N2 at this stage, with a W15S28-sized bank.
let private garrisonMother stage =
    { ferryMother stage with
        Bank = bank 5_600 5_600
    }

/// The Bootstrapping mother with the child's home opened to a floor wide
/// enough to kite on — plain but for these swamp tiles — her rangers standing
/// in it, and this raid.
let private openGarrison (swamps: Pos list) (rangers: (CreepInfo * Pos) list) raid =
    let colony = garrisonMother Bootstrapping
    let layer = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Hostiles = raid
        Creeps = (rangers |> List.map fst) @ colony.Creeps
        Spatial =
            colony.Spatial
            |> withNeighbour
                "W1N2"
                { layer with
                    Terrain =
                        TerrainGrid.ofList
                            [
                                for x in 2..47 do
                                    for y in 2..47 ->
                                        let tile = { X = x; Y = y }
                                        tile, (if List.contains tile swamps then Swamp else Plain)
                            ]
                    CreepPositions =
                        rangers |> List.map (fun (creep, pos) -> creep.Name, pos) |> Map.ofList
                }
    }

/// The child's home with ramparts of ours standing on these tiles: ours by
/// their hits, as the projection carries them.
let private withOurRamparts (tiles: Pos list) (colony: ColonyView) =
    let layer = SpatialInfo.layerOf colony.Spatial "W1N2"
    let ramparts = tiles |> List.map (fun tile -> $"ram-{tile.X}-{tile.Y}", tile)

    { colony with
        Spatial =
            { (colony.Spatial
               |> withNeighbour
                   "W1N2"
                   { layer with
                       TargetPositions =
                           (layer.TargetPositions, ramparts)
                           ||> List.fold (fun acc (id, tile) -> Map.add id tile acc)
                   }) with
                TargetKinds =
                    (colony.Spatial.TargetKinds, ramparts)
                    ||> List.fold (fun acc (id, _) -> Map.add id (Structure BuiltKind.Rampart) acc)
                Hits =
                    (colony.Spatial.Hits, ramparts)
                    ||> List.fold (fun acc (id, _) ->
                        Map.add id { Hits = 50_000; HitsMax = 300_000 } acc)
            }
    }

[<Tests>]
let raisedHomeGarrisonTests =
    testList
        "a mother keeps a resident garrison in a child's home she raises"
        [
            test
                "a Nursery, Bootstrapping or Weaning child's home is a Guard of the mother's ranger row in peace" {
                for stage in [ Nursery; Bootstrapping; Weaning ] do
                    let mother = garrisonMother stage

                    Expect.equal
                        (guardsIn (planTasksOn mother (noThreats ())))
                        [ Guard "W1N2" ]
                        $"{stage}: the child's home is one Guard in her pool, under its name"

                    Expect.equal
                        (rangerQuotaOf mother)
                        (Some Tuning.defaults.RangerResidents)
                        $"{stage}: and her ranger row keeps the errand room's garrison for it"

                    Expect.equal
                        (guardQuotaOf mother)
                        (Some 0)
                        $"{stage}: and her guard row nothing"
            }

            test "an Independent child's home draws no garrison" {
                let mother = garrisonMother Independent

                Expect.isEmpty (guardsIn (planTasksOn mother (noThreats ()))) "no Guard in its home"

                Expect.equal (rangerQuotaOf mother) (Some 0) "and no ranger for it"
            }

            test
                "an Independent contested child's home short of its towers keeps the garrison (#479)" {
                let independent = garrisonMother Independent

                let mother =
                    { independent with
                        Borrowed =
                            { independent.Borrowed with
                                Rooms = []
                                Garrisoned = [ "W1N2" ]
                            }
                    }

                Expect.equal
                    (guardsIn (planTasksOn mother (noThreats ())))
                    [ Guard "W1N2" ]
                    "one Guard in its home"

                Expect.equal
                    (rangerQuotaOf mother)
                    (Some Tuning.defaults.RangerResidents)
                    "and her ranger row keeps the garrison for it"
            }

            test "the garrison holds the child's controller ring" {
                let mother = garrisonMother Bootstrapping
                let atlas = Atlas.ofView mother

                Expect.equal
                    (Threats.residentRingIn (threatsOf mother atlas) "W1N2")
                    (Some(
                        Atlas.adjacentWalkableIn atlas "W1N2" { X = 10; Y = 46 }
                        |> Set.ofList
                        |> RoomPos.setAt "W1N2"
                    ))
                    "the ground is the controller's walkable ring, inside any perimeter"
            }

            // Characterisation (the Brief's S0): the garrison's Guard in peace, its
            // rank, capacity, takers and act; its row and ground are pinned
            // around it, its raid grounds below.
            test
                "the garrison's Guard ranks on the tier for the residents and a relief each, admits a ranger alone, and shoots the tapper" {
                let resident =
                    creepWith
                        "ranger-1"
                        0
                        0
                        (List.replicate 7 Bodies.rangerPattern.Block |> List.concat)

                let melee = creepWith "guard-1" 0 0 Bodies.guardPattern.Block

                let tapper =
                    { hostileIn "W1N2" { X = 11; Y = 47 } [ BodyPart.Claim; BodyPart.Claim; Move ] with
                        Id = "tapper"
                    }

                let colony =
                    openGarrison
                        []
                        [ resident, { X = 12; Y = 46 }; melee, { X = 8; Y = 44 } ]
                        [ tapper ]

                Expect.equal
                    (pooledHolding [] (Guard "W1N2") colony)
                    (Some(
                        safetyRank OnTheTier,
                        Capacity.fighters (2 * Tuning.defaults.RangerResidents)
                    ))
                    "on the Safety tier, for the garrison and a relief each"

                let decision = decideOn colony

                Expect.equal
                    (Map.tryFind "ranger-1" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the ranger takes it"

                Expect.notEqual
                    (Map.tryFind "guard-1" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "a melee guard is refused a resident room's Guard"

                Expect.contains
                    decision.Intents
                    (RangedAttackCreep("ranger-1", "tapper"))
                    "and the ranger shoots the CLAIM body at the controller"
            }

            test
                "under an armed raid the garrison's ground is the threats' ring, as #428's guard's was" {
                // A Guard takes the resident ring where there is one and the
                // threats' otherwise (`Threats.guardGroundIn`): a raised home's
                // resident ring is its peace-time ground only, so the garrison
                // chases. A lone longbow: no melee body to kite (#451), and
                // a raid the garrison wins.
                let raided =
                    { garrisonMother Bootstrapping with
                        Hostiles = [ hostileIn "W1N2" { X = 10; Y = 44 } [ RangedAttack; Move ] ]
                    }

                let atlas = Atlas.ofView raided
                let threats = threatsOf raided atlas

                Expect.isNone
                    (Threats.residentRingIn threats "W1N2")
                    "no resident ground while an armed hostile stands in the home"

                Expect.isNonEmpty (Threats.ringIn threats "W1N2") "the threats' ring stands"

                Expect.equal
                    (Threats.guardGroundIn threats "W1N2")
                    (Some(Threats.ringIn threats "W1N2"))
                    "and the Guard takes it"
            }

            test
                "under a melee raid the garrison kites: nothing within two of a melee body, nor a ranger's step onto swamp near one" {
                // #451: on the threats' ring a ranger stood beside both melee
                // bodies at W17S25 and took 1,020 a tick. The 10A2H raid is one
                // an eight-block ranger wins, so the room is a fight, not a
                // withdrawal to safe ground.
                let brawler = { X = 20; Y = 30 }
                let swamp = { X = 23; Y = 30 }
                let plain = { X = 23; Y = 31 }

                let ranger =
                    creepWith
                        "ranger-1"
                        0
                        0
                        (List.replicate 7 Bodies.rangerPattern.Block |> List.concat)

                let raided =
                    openGarrison
                        [ swamp ]
                        [ ranger, { X = 24; Y = 31 } ]
                        [
                            hostileIn
                                "W1N2"
                                brawler
                                (List.replicate 10 Attack
                                 @ List.replicate 2 Heal
                                 @ List.replicate 12 Move)
                        ]

                let atlas = Atlas.ofView raided
                let threats = threatsOf raided atlas

                let ground =
                    Threats.guardGroundIn threats "W1N2"
                    |> Option.defaultValue Set.empty
                    |> Set.map RoomPos.pos

                Expect.isNonEmpty ground "the ranger has ground to stand on"

                Expect.all
                    ground
                    (fun tile -> range brawler tile = Engine.rangedRange)
                    "only the tiles it shoots the brawler from and the brawler cannot reach in a step and a swing"

                // Movement S3: swamp near a melee is the ranger's own walk's
                // to keep off (`Threats.kiteUnsafeFor`), by the ticks the tile
                // holds this body, no longer the room's ground's.
                let index (tile: Pos) = tile.X * Engine.roomSide + tile.Y

                match Threats.kiteUnsafeFor threats atlas ranger None with
                | Some(unsafe, _) ->
                    Expect.isTrue
                        (unsafe (index swamp))
                        "a step onto the swamp three from the brawler holds it five ticks: struck"

                    Expect.isFalse
                        (unsafe (index plain))
                        "a step onto the plain tile beside it is off again first"
                | None -> failtest "the premise: the ranger's walk reads the brawler's map"
            }

            test
                "under a raid the garrison stands on our ramparts within three of its target, beside a melee body too" {
                // #467: a creep on its own rampart takes no damage, so a
                // rampart tile is safe ground whatever the melee reaches. At
                // W17S25 the garrison held the controller ring while the
                // raid broke the perimeter unshot.
                let brawler = { X = 20; Y = 30 }
                let beside = { X = 21; Y = 30 }
                let twoOff = { X = 22; Y = 32 }
                let farOff = { X = 30; Y = 30 }

                let ground raid =
                    let colony =
                        openGarrison [] [] raid |> withOurRamparts [ beside; twoOff; farOff ]

                    Threats.guardGroundIn (threatsOf colony (Atlas.ofView colony)) "W1N2"
                    |> Option.map (Set.map RoomPos.pos)

                let melee body =
                    [
                        { hostileIn "W1N2" brawler body with
                            Id = "atk"
                        }
                    ]

                Expect.equal
                    (ground (
                        melee (
                            List.replicate 10 Attack
                            @ List.replicate 2 Heal
                            @ List.replicate 12 Move
                        )
                    ))
                    (Some(Set.ofList [ beside; twoOff ]))
                    "a raid the garrison wins: the ramparts it shoots the brawler from, the one beside it too"

                let outmatching =
                    melee (List.replicate 17 Attack @ List.replicate 18 Move)
                    @ [
                        for i in 1..2 ->
                            { hostileIn
                                  "W1N2"
                                  { X = 19; Y = 30 }
                                  (List.replicate 7 Heal @ List.replicate 11 Move) with
                                Id = $"med-{i}"
                            }
                    ]

                Expect.equal
                    (ground outmatching)
                    (Some(Set.ofList [ beside; twoOff ]))
                    "a raid no ranger wins: the same ramparts, not the safe set"
            }

            test
                "a healer alone on the home's exit, there or remembered off it, keeps the garrison on our ramparts within three of it" {
                // #552, live t981,106: Trepidimous' healer bounced on W17S25's
                // exit tile, its melee across the border; with no armed
                // Threat in the room the garrison went back to the
                // controller's ring every other tick, and never reached the
                // line.
                let medic =
                    { hostileIn
                          "W1N2"
                          { X = 0; Y = 30 }
                          (List.replicate 7 Heal @ List.replicate 11 Move) with
                        Id = "med-1"
                        Owner = "Trepidimous"
                    }

                let ramparts = [ { X = 2; Y = 30 }; { X = 3; Y = 32 }; { X = 4; Y = 30 } ]

                let ground colony =
                    Threats.guardGroundIn (threatsOf colony (Atlas.ofView colony)) "W1N2"
                    |> Option.map (Set.map RoomPos.pos)

                let within = Some(Set.ofList [ { X = 2; Y = 30 }; { X = 3; Y = 32 } ])

                Expect.equal
                    (ground (openGarrison [] [] [ medic ] |> withOurRamparts ramparts))
                    within
                    "standing on the exit tile"

                let quiet = openGarrison [] [] [] |> withOurRamparts ramparts

                Expect.equal
                    (ground
                        { quiet with
                            RecalledHealers = [ medic ]
                        })
                    within
                    "carried across the border this tick, remembered on it"
            }

            test
                "the garrison shoots the raid's healer before a full-hits melee, and a tapper at the controller first" {
                // #451: `guardShoots` counted only armed bodies, so the 11M7H
                // healers keeping W17S25's raid alive were never targets.
                let ranger =
                    creepWith
                        "ranger-1"
                        0
                        0
                        (List.replicate 7 Bodies.rangerPattern.Block |> List.concat)

                let shot raid =
                    let colony = openGarrison [] [ ranger, { X = 12; Y = 46 } ] raid
                    let atlas = Atlas.ofView colony

                    emit
                        colony
                        atlas
                        (Threats.quietOn colony atlas)
                        (Map.ofList [ ranger.Name, Guard "W1N2" ])
                    |> List.choose (function
                        | RangedAttackCreep(name, target) when name = ranger.Name -> Some target
                        | _ -> None)

                Expect.equal
                    (shot homeRaid)
                    [ "med-1" ]
                    "the healer, whose hits are fewer, before the melee it heals"

                // Forty TOUGH: more hits than any of the raid, and still first.
                let tapper =
                    { hostileIn
                          "W1N2"
                          { X = 11; Y = 47 }
                          (List.replicate 40 Tough
                           @ List.replicate 3 BodyPart.Claim
                           @ List.replicate 3 Move) with
                        Id = "tapper"
                    }

                Expect.equal
                    (shot (tapper :: homeRaid))
                    [ "tapper" ]
                    "a CLAIM body beside our controller first"

                // #429: an Invader's pure dismantler hurts no creep, and
                // razes the home it stands in. A neutral's WORK body there is
                // a target only caught dismantling, which this colony reads
                // in its own home alone.
                let breacher owner =
                    { hostileIn
                          "W1N2"
                          { X = 11; Y = 45 }
                          (List.replicate 10 Work @ List.replicate 10 Move) with
                        Id = "breacher"
                        Owner = owner
                    }

                Expect.equal
                    (shot [ breacher "Invader" ])
                    [ "breacher" ]
                    "an Invader's dismantler in a home of ours is a target"

                Expect.isEmpty (shot [ breacher "raider" ]) "a neutral's passing is not"
            }

            test "a healer our damage breaks is shot before a melee body with fewer hits" {
                // #451: the healer leads because the shots out-pace its heal,
                // not because it is the weaker: 1,100 hits against 300.
                let ranger =
                    creepWith
                        "ranger-1"
                        0
                        0
                        (List.replicate 7 Bodies.rangerPattern.Block |> List.concat)

                let raid =
                    [
                        { hostileIn "W1N2" { X = 10; Y = 44 } [ Attack; Move; Move ] with
                            Id = "brawler"
                        }
                        { hostileIn "W1N2" { X = 10; Y = 45 } (Heal :: List.replicate 10 Move) with
                            Id = "medic"
                        }
                    ]

                let colony = openGarrison [] [ ranger, { X = 12; Y = 46 } ] raid
                let atlas = Atlas.ofView colony

                Expect.equal
                    (emit
                        colony
                        atlas
                        (Threats.quietOn colony atlas)
                        (Map.ofList [ ranger.Name, Guard "W1N2" ])
                     |> List.choose (function
                         | RangedAttackCreep(name, target) when name = ranger.Name -> Some target
                         | _ -> None))
                    [ "medic" ]
                    "the healer, whose one heal our 140 out-paces"
            }

            test "a towered home whose towers turn the raid is no withdrawal for its garrison" {
                // #451: W17S29's raid out-heals the largest ranger alone, but
                // three loaded towers land 450 more at the falloff range, so
                // the home is a fight and its garrison keeps fighting ground.
                let ranger =
                    creepWith
                        "ranger-1"
                        0
                        0
                        (List.replicate 7 Bodies.rangerPattern.Block |> List.concat)

                let raided towers =
                    openGarrison [] [ ranger, { X = 30; Y = 10 } ] homeRaid
                    |> withOurTowers "W1N2" (List.replicate towers { X = 40; Y = 10 })

                Expect.equal
                    (rangerQuotaOf (raided 0))
                    (Some 0)
                    "the premise: no ranger alone wins it"

                let towered = raided 3

                Expect.equal
                    (rangerQuotaOf towered)
                    (Some Tuning.defaults.RangerResidents)
                    "with the towers, the garrison is kept and cast for"

                let threats = threatsOf towered (Atlas.ofView towered)

                Expect.notEqual
                    (Threats.guardGroundIn threats "W1N2")
                    (Some(Threats.safeIn threats "W1N2"))
                    "and it holds fighting ground, not the safe set"
            }

            test
                "a beaten raised home is one Guard, the ranger row's, with the relief on top of the garrison" {
                // A 10A2H raid: 300 a tick, which the seven-block resident
                // loses alone and an eight-block ranger wins. Its own HEAL
                // drops its swing (#651), so the 24 a tick that turns the
                // seven blocks' trade is a healer's beside it.
                let winnable =
                    [
                        hostileIn
                            "W1N2"
                            { X = 10; Y = 44 }
                            (List.replicate 10 Attack
                             @ List.replicate 2 Heal
                             @ List.replicate 12 Move)
                        { hostileIn "W1N2" { X = 11; Y = 44 } [ Heal; Heal; Move; Move ] with
                            Id = "h-2"
                        }
                    ]

                let beaten raid =
                    { garrisonMother Bootstrapping with
                        Hostiles = raid
                        Borrowed =
                            { BorrowedWork.empty with
                                Rooms = [ "W1N2" ]
                                Defended = [ "W1N2" ]
                            }
                    }

                Expect.equal
                    (guardsIn (planTasksOn (beaten winnable) (noThreats ())))
                    [ Guard "W1N2" ]
                    "raised and defended, the home is still one Guard"

                Expect.equal
                    (guardQuotaOf (beaten winnable))
                    (Some 0)
                    "the melee row casts nothing for it"

                Expect.equal
                    (rangerQuotaOf (beaten winnable))
                    (Some(Tuning.defaults.RangerResidents + Engine.guardCap))
                    "and the ranger row adds #428's relief to the garrison"
            }

            test
                "a raid no ranger size wins casts no resident, and the residents there hold safe ground" {
                // #447: W17S29's squad heals 168 a tick against an eight-block
                // ranger's 160, so a body of any size only feeds it; the home
                // is its own safe mode's (#448), and no stand-down withdraws
                // the garrison from it. #451: nor is a resident cast into it,
                // and the ones standing keep out of its reach.
                let ranger =
                    creepWith
                        "ranger-1"
                        0
                        0
                        (List.replicate 7 Bodies.rangerPattern.Block |> List.concat)

                let beaten =
                    { openGarrison [] [ ranger, { X = 30; Y = 10 } ] homeRaid with
                        Borrowed =
                            { BorrowedWork.empty with
                                Rooms = [ "W1N2" ]
                                Defended = [ "W1N2" ]
                            }
                    }

                Expect.equal (rangerQuotaOf beaten) (Some 0) "no ranger cast for it"

                Expect.equal
                    (guardsIn (planTasksOn beaten (noThreats ())))
                    [ Guard "W1N2" ]
                    "the Guard stands for the one already there"

                let threats = threatsOf beaten (Atlas.ofView beaten)

                Expect.isNonEmpty
                    (Threats.safeIn threats "W1N2")
                    "the premise: the room has safe ground"

                Expect.equal
                    (Threats.guardGroundIn threats "W1N2")
                    (Some(Threats.safeIn threats "W1N2"))
                    "and it is the resident's ground"

                let unheld = { beaten with Creeps = [] }

                Expect.isFalse
                    (Option.isSome
                        (Threats.briefOf (threatsOf unheld (Atlas.ofView unheld)) "W1N2").Laid.Kite)
                    "with no ranger of ours standing there, its two thousand tiles are never walked"
            }

            test
                "a ranger holding the Guard of a room no ranger size wins has its safe ground there before it walks in" {
                let ranger =
                    creepWith
                        "ranger-1"
                        0
                        0
                        (List.replicate 7 Bodies.rangerPattern.Block |> List.concat)

                let garrison = openGarrison [] [] homeRaid

                // Standing nowhere in the room: on its way back in.
                let away =
                    { garrison with
                        Creeps = ranger :: garrison.Creeps
                        Borrowed =
                            { BorrowedWork.empty with
                                Rooms = [ "W1N2" ]
                                Defended = [ "W1N2" ]
                            }
                    }

                let threatsHeld (held: HeldTaskFacts) =
                    let atlas = Atlas.ofView away
                    threatsOfHeld away atlas held

                let guarding = heldTaskFacts away (Map.ofList [ "ranger-1", taskId (Guard "W1N2") ])

                Expect.isFalse
                    (Option.isSome
                        (Threats.briefOf (threatsHeld HeldTaskFacts.empty) "W1N2").Laid.Kite)
                    "the premise: holding nothing, it is handed nothing"

                let threats = threatsHeld guarding

                Expect.isNonEmpty
                    (Threats.safeIn threats "W1N2")
                    "the premise: the room has safe ground"

                Expect.equal
                    (Threats.guardGroundIn threats "W1N2")
                    (Some(Threats.safeIn threats "W1N2"))
                    "holding the Guard, its ground is the room's safe ground, not the threats' ring"
            }
        ]
