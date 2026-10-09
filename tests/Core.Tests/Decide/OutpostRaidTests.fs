/// An outpost with a hostile standing in it: when the economy walks back in,
/// the container that switches the room back, and the raid's memory and the
/// exit it left by. The Guard that answers is `OutpostGuardTests`'.
module Fabot.Core.Tests.Decide.OutpostRaidTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.OutpostFixtures

[<Tests>]
let raidedOutpostTests =
    testList
        "an outpost with an armed hostile standing in it"
        [
            test "the crew flees and the garrison, which cannot, is matched to nothing at all" {
                // What a raid costs an outpost before the guard row answers it: the
                // haulers run, the anchor cannot and stays.
                //
                // Pairwise against the same fixture with no hostile in it.
                let assignmentsOf hostiles =
                    (decide (raidedOutpost hostiles) Map.empty Set.empty None).Assignments

                let quiet = assignmentsOf []
                let raided = assignmentsOf raiders

                Expect.equal
                    (Map.tryFind "a-out" quiet)
                    (Some(taskId (Harvest "src-out")))
                    "the premise: on a quiet tick the garrison digs the rock it stands on"

                Expect.isEmpty
                    (quiet |> Map.filter (fun _ tid -> tid = taskId Flee))
                    "and nobody runs from a room with nothing in it (ADR 0033)"

                Expect.equal
                    (Map.tryFind "h-out1" quiet, Map.tryFind "h-out2" quiet)
                    (Some(taskId (Withdraw("can-out", Energy))),
                     Some(taskId (Withdraw("can-out", Energy))))
                    "and the crew empties the container that Post feeds"

                Expect.equal
                    (Map.tryFind "h-out1" raided, Map.tryFind "h-out2" raided)
                    (Some(taskId Flee), Some(taskId Flee))
                    "both haulers drop that haul for Flee: the Safety tier outranks every other"

                // Neither Flee nor work: Flee is inapplicable to a work-heavy body and
                // every Seat of its rock (the row of field under it) lies inside the
                // Reach. This is the hole the guard row is cast into.
                Expect.equal
                    (Map.tryFind "a-out" raided)
                    None
                    "and the Anchor is matched to nothing at all: it neither runs nor digs"
            }

            test "the garrison's Harvest is released Threatened rather than simply lost" {
                // Read off the Verdicts with the quiet tick's assignment already held.
                // Every Seat of `src-out` is inside this raid's Reach, and the
                // container the haul reads is on one of them.
                let releasesOf hostiles =
                    let held =
                        Map.ofList
                            [
                                "a-out", taskId (Harvest "src-out")
                                "h-out1", taskId (Withdraw("can-out", Energy))
                            ]

                    (decideFrom held (raidedOutpost hostiles)).Verdicts
                    |> List.choose (function
                        | Verdict.Released(creep, task, reason) -> Some(creep, task, reason)
                        | _ -> None)

                Expect.isEmpty (releasesOf []) "the premise: a quiet tick releases nobody"

                Expect.equal
                    (releasesOf raiders)
                    [
                        "a-out",
                        taskId (Harvest "src-out"),
                        ReleaseReason.Rejected RejectReason.Threatened
                        "h-out1",
                        taskId (Withdraw("can-out", Energy)),
                        ReleaseReason.Rejected RejectReason.Threatened
                    ]
                    "the raid takes the rock's Seats and the container's ground, and says so"
            }

            test
                "a fight-only decide of a raided outpost: an idle spawn the guard ask owes hands it back whole; the spawn at work, the digging anchor and the fleeing haulers keep their Tasks; the anchor shot to Work <= Move runs, and hands it back (#669)" {
                // The forced tick of a hurt anchor decides its colony's fight
                // alone (`LightTick.fightOnly`): its flight is no answer, the
                // haulers' Flee replays, and the guard's cast stays asked.
                let raided = declaredRaid raiders
                let first = decideUnarbitrated raided Map.empty Set.empty None ReplanTurn.Now

                Expect.isFalse
                    (List.isEmpty first.Economy.Asks)
                    "premise: the raid asks the guard row a body"

                Expect.isNone
                    (decideFight raided first.Assignments Set.empty first.Memo first.Economy)
                    "the guard owed an idle spawn: spawning is the economy's"

                let casting =
                    { raided with
                        Spawns =
                            raided.Spawns
                            |> List.map (fun spawn ->
                                { spawn with
                                    IsSpawning = true
                                    Remaining = 10
                                })
                    }

                let carried = decideUnarbitrated casting Map.empty Set.empty None ReplanTurn.Now

                match
                    decideFight casting carried.Assignments Set.empty carried.Memo carried.Economy
                with
                | None -> failtest "the spawn at work: nothing owed, the fight decides alone"
                | Some fight ->
                    Expect.equal
                        fight.Assignments
                        carried.Assignments
                        "the anchor holds nothing to run to and the haulers keep their Flee"

                // The live case: damage strips the anchor's Work to its Move,
                // so it runs (`runsFromThreats`, #664), which the carry, laid
                // while it dug, never priced.
                let crippled =
                    { casting with
                        Creeps =
                            casting.Creeps
                            |> List.map (fun creep ->
                                if creep.Name = "a-out" then
                                    { creep with
                                        Body = Map.add Work 1 creep.Body
                                    }
                                else
                                    creep)
                    }

                Expect.isFalse
                    (Set.contains "a-out" carried.Economy.Unfled)
                    "premise: the digging anchor was no runner the whole decide left off Flee"

                Expect.isNone
                    (decideFight crippled carried.Assignments Set.empty carried.Memo carried.Economy)
                    "the crippled anchor's flight is a whole decide's"
            }
        ]

/// The far corner of `raidedOutpost`'s field, six tiles and more off every
/// tile the container is drawn from and nine off the controller's ring:
/// whatever stands here takes no Work Area of either into its Reach.
let private farCorner = { X = 20; Y = 48 }

/// Trepidimous' 18M17A guard of W17S26 (#604, #605), standing at `pos`.
let private standingMelee pos =
    { hostileIn "W1N2" pos (List.replicate 18 Move @ List.replicate 17 Attack) with
        Id = "melee-1"
        Owner = "Trepidimous"
    }

/// One body at home, decided against the outpost and the hostiles given.
let private homeBodyUnder (creep: CreepInfo) hostiles (change: ColonyView -> ColonyView) =
    decide
        (declaredRaid hostiles |> withBodyAtHome creep atSpawn |> change)
        Map.empty
        Set.empty
        None

/// An Invader's `smallMelee` at `pos`: a raid we strike anywhere.
let private invaderAt pos =
    { hostileIn "W1N2" pos smallMelee with
        Id = "inv-1"
        Owner = "Invader"
    }

let private homeWorker = worker "w-home" 0 50

/// A bank whose guard wins `invaderAt`'s raid alone.
let private rich (colony: ColonyView) =
    { colony with
        Bank = { Available = 1_300; Capacity = 1_300 }
    }

/// The outpost Task the home worker crosses for with the room quiet.
let private quietDraw () =
    Map.tryFind "w-home" (homeBodyUnder homeWorker [] rich).Assignments

[<Tests>]
let economyEntryTests =
    testList
        "an economy body walks into a raided outpost only once it is safe there (#605)"
        [
            test
                "a raid the verdict wins keeps the home worker out until a fighter of ours is at the fight" {
                let raid = [ invaderAt farCorner ]
                let drawn = quietDraw ()

                Expect.contains
                    [ Some(taskId (Harvest "src-out")); Some(taskId (Withdraw("can-out", Energy))) ]
                    drawn
                    "the premise: with the room quiet the body crosses for the outpost's work"

                // Pairwise on the guard alone: at home it is a room off the
                // raid, beside it it is at the fight.
                let guardAtHome =
                    homeBodyUnder
                        homeWorker
                        raid
                        (rich >> withBodyAtHome (guard "g-1") { X = 25; Y = 30 })

                let guardAtFight =
                    homeBodyUnder
                        homeWorker
                        raid
                        (rich >> withGuards [ guard "g-1", { X = 21; Y = 47 } ])

                Expect.equal
                    (Facts.answer (rich (declaredRaid raid)) "W1N2")
                    Answer.Win
                    "the premise: the verdict wins it, so no stand-down shuts the room"

                Expect.notEqual
                    (Map.tryFind "w-home" guardAtHome.Assignments)
                    drawn
                    "the verdict says whether to fight, not whether the economy walks in"

                Expect.equal
                    (Map.tryFind "g-1" guardAtFight.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the premise: the guard beside the raid holds the room's Guard"

                Expect.equal
                    (Map.tryFind "w-home" guardAtFight.Assignments)
                    drawn
                    "once the guard holds the fight and wins it the room is the worker's again"
            }

            test
                "a body already holding the outpost's work keeps it while the Guard's holder stays in the room" {
                // The guard holds W1N2's Guard from the room's far corner,
                // out of its swing: seated nowhere, but in the room.
                let guardAway (colony: ColonyView) =
                    rich colony |> withGuards [ guard "g-1", { X = 30; Y = 41 } ]

                let held =
                    Map.ofList [ "w-home", Option.get (quietDraw ()); "g-1", taskId (Guard "W1N2") ]

                let fresh =
                    decide
                        (declaredRaid [ invaderAt farCorner ]
                         |> withBodyAtHome homeWorker atSpawn
                         |> guardAway)
                        (Map.ofList [ "g-1", taskId (Guard "W1N2") ])
                        Set.empty
                        None

                let kept =
                    decide
                        (declaredRaid [ invaderAt farCorner ]
                         |> withBodyAtHome homeWorker atSpawn
                         |> guardAway)
                        held
                        Set.empty
                        None

                Expect.notEqual
                    (Map.tryFind "w-home" fresh.Assignments)
                    (quietDraw ())
                    "a fresh candidate waits for the holder to be seated at the fight"

                Expect.equal
                    (Map.tryFind "w-home" kept.Assignments)
                    (quietDraw ())
                    "a holder of the work keeps it while the Guard's holder stands in the room"
            }

            test "the anchor and reserver rows hire nobody for a room the raid shuts to the economy" {
                let raided = rich (declaredRaid [ invaderAt farCorner ])

                Expect.equal
                    (Quota.shutToEconomy raided)
                    (Set.singleton "W1N2")
                    "with no fighter of ours at the fight the room is shut to the rows"

                Expect.isEmpty
                    (Quota.shutToEconomy (raided |> withGuards [ guard "g-1", { X = 21; Y = 47 } ]))
                    "a guard at the fight that wins it opens it again"
            }

            test "a neutral's armed body in the outpost shuts nothing" {
                let neutral =
                    { hostileIn "W1N2" farCorner smallMelee with
                        Id = "giaco-1"
                        Owner = "giaco"
                    }

                Expect.equal
                    (Map.tryFind "w-home" (homeBodyUnder homeWorker [ neutral ] rich).Assignments)
                    (quietDraw ())
                    "we may strike its weapon in a trespass room, but its owner is none we strike anywhere, so it is no raid the economy waits out"
            }

            // Live t1,006,924, W17S26: the room's row cut to its first sighting
            // plus `Tuning.PlayerRaidHold`, the reserver walked back to the
            // controller the melee still stood beside, and died.
            test
                "a raid nothing of ours wins keeps the room shut past the player's hold while it is seen or recalled" {
                let reserving = Some(taskId (Reserve "ctl-out"))
                let reserver = reserver "r-home"

                let contested (colony: ColonyView) =
                    { colony with
                        RoomControl = Map.add "W1N2" (enemyReservedRoom 3_000) colony.RoomControl
                    }

                Expect.equal
                    (Map.tryFind "r-home" (homeBodyUnder reserver [] contested).Assignments)
                    reserving
                    "the premise: with the melee seen gone the reserver walks to attack his hold (#604)"

                let melee = standingMelee farCorner

                Expect.equal
                    (Facts.answer (declaredRaid [ melee ]) "W1N2")
                    Answer.Lose
                    "the premise: nothing of ours wins it"

                Expect.notEqual
                    (Map.tryFind "r-home" (homeBodyUnder reserver [ melee ] contested).Assignments)
                    reserving
                    "seen, the melee keeps the reserver out whatever the stand-down's clock says"

                let recalled (colony: ColonyView) =
                    { contested colony with
                        RecalledRaiders = [ melee ]
                    }

                Expect.notEqual
                    (Map.tryFind "r-home" (homeBodyUnder reserver [ melee ] recalled).Assignments)
                    reserving
                    "and recalled through the blind ticks, until a sighting finds it gone"

                // A player's raid nobody sees go holds the room no longer than
                // its priced hold past its last sighting (#585, was #513's
                // fixed `Tuning.PlayerRaidHold`): W1N2 is declared by no
                // colony, its income nothing, so the whole round trip.
                let lastSeen ago (colony: ColonyView) =
                    { recalled colony with
                        World =
                            { colony.World with
                                RaidSeen = Map.ofList [ "W1N2", colony.Time - ago ]
                            }
                    }

                let hold = WorldView.raidTrip [ melee ]

                Expect.notEqual
                    (Map.tryFind
                        "r-home"
                        (homeBodyUnder reserver [ melee ] (lastSeen (hold - 1))).Assignments)
                    reserving
                    "seen inside the hold, still shut"

                Expect.equal
                    (Map.tryFind
                        "r-home"
                        (homeBodyUnder reserver [ melee ] (lastSeen hold)).Assignments)
                    reserving
                    "unseen for the whole hold, the room is the reserver's again"
            }
        ]

[<Tests>]
let containerSwitchTests =
    testList
        "the container is the switch"
        [
            // Read against the fleet, one body at a time: a colony exactly at its
            // target casts nothing and the same colony one body short casts one,
            // so a target that moved by n cannot hide inside a spawn's
            // one-cast-a-tick limit.
            let casts colony fleet =
                spawnIntents
                    (decide { colony with Creeps = fleet } Map.empty Set.empty None).Intents

            let short fleet =
                List.truncate (List.length fleet - 1) fleet

            test "an outpost rock with nothing built on it moves no row of the target" {
                // The room is projected, held by us and its rock is pooled for Harvest,
                // and still the colony hires exactly the fleet it hired without it.
                //
                // Pairwise: the two colonies differ in the outpost rock and nothing else.
                Expect.isEmpty
                    (casts switchHome switchHomeFleet)
                    "the premise: six is the home room's whole target"

                Expect.hasLength
                    (casts switchHome (short switchHomeFleet))
                    1
                    "the premise is tight: one body short and the colony casts"

                Expect.equal
                    (quotaOf switchUnposted)
                    (quotaOf switchHome)
                    "the unposted rock hires no haul"

                Expect.isEmpty
                    (casts switchUnposted switchHomeFleet)
                    "and no Anchor and no worker either: the same six are the whole target"
            }

            test "the container standing is one Anchor, its own haul and its income share" {
                // One tick's difference, a container on the rock's one Seat, and the
                // colony hires six more bodies: the Anchor, the one hauler its round
                // trip adds to the rounded-once pool, and the four workers the rock's
                // output feeds once those rows are amortized.
                Expect.isEmpty
                    (casts switchPosted (switchHomeFleet @ switchOutpostRows))
                    "posted, the target is the home fleet plus the outpost's own rows"

                Expect.hasLength
                    (casts switchPosted (short (switchHomeFleet @ switchOutpostRows)))
                    1
                    "and it is tight: one body short and the colony casts"

                Expect.equal
                    (quotaOf switchPosted - quotaOf switchUnposted)
                    1
                    "one of the six is what the container's own haul adds to the pool"
            }

            test "the Anchor the container hires is one, from the row the home Posts hire from" {
                // The outpost's Anchor is on the *same* row as the home room's, so the
                // proof is a swap at a fixed headcount: one body short the colony casts
                // a worker while both Anchors stand, and the same eleven bodies with
                // the outpost's Anchor spelled as a worker cast an Anchor instead.
                let shortFleet = short (switchHomeFleet @ switchOutpostRows)

                let swapped =
                    shortFleet
                    |> List.map (fun creep ->
                        if creep.Name = "a-out" then worker "w9" 0 50 else creep)

                match casts switchPosted shortFleet with
                | [ (_, _, name) ] ->
                    Expect.stringStarts
                        name
                        "worker-"
                        "the premise: with both Anchors it is a worker"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                match casts switchPosted swapped with
                | [ (_, _, name) ] -> Expect.stringStarts name "anchor-" "the gap is an Anchor gap"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a standing outpost container adds no Task; stocked it adds its own Withdraw" {
                // A rock is pooled for Harvest whether or not it is posted, so the
                // container standing adds no Task by standing. Stocked it adds a
                // Withdraw on itself and never a Refill target: an outpost's container
                // is no upgrade buffer of a controller a room away (the join is pinned
                // by `roomLayerTests`, on a fixture with a controller to be wrong about).
                //
                // A container is a repairable kind, so under half its max
                // `hungryStructures` pools a cross-room `Repair` beside this Withdraw;
                // `isHungry` judges every structure against its own kind's whole line.
                Expect.equal
                    (planTasksOn switchPosted (noThreats ()))
                    (planTasksOn switchUnposted (noThreats ()))
                    "an empty container standing changes no Task in the pool"

                let stocked =
                    { switchPosted with
                        Spatial =
                            { switchPosted.Spatial with
                                Stores = Map.add "can-out" 500 switchPosted.Spatial.Stores
                            }
                    }

                Expect.equal
                    (List.except
                        (planTasksOn switchPosted (noThreats ()))
                        (planTasksOn stocked (noThreats ())))
                    [ Withdraw("can-out", Energy) ]
                    "and stocked it adds exactly one Task, the Withdraw of its own store"

                let decayed =
                    { switchPosted with
                        Spatial =
                            { switchPosted.Spatial with
                                Hits =
                                    Map.add
                                        "can-out"
                                        { Hits = 1000; HitsMax = 2500 }
                                        switchPosted.Spatial.Hits
                            }
                    }

                Expect.equal
                    (List.except
                        (planTasksOn switchPosted (noThreats ()))
                        (planTasksOn decayed (noThreats ())))
                    [ Repair "can-out" ]
                    "and decayed it adds exactly one more, its own Repair across the Seam"
            }
        ]

/// One room's ring, W1N2's border walled but for a west exit run of
/// y 20..24, as the world reads it off the terrain.
let private westBorder =
    plainRing
    |> Map.map (fun tile terrain ->
        if tile.X = 0 && tile.Y >= 20 && tile.Y <= 24 then
            terrain
        else
            Wall)

/// W17S26's west border as #561 met it: an exit run down x = 0 past y 6,
/// walled elsewhere.
let private w17s26Border =
    plainRing
    |> Map.map (fun tile terrain ->
        if tile.X = 0 && tile.Y >= 2 && tile.Y <= 9 then
            terrain
        else
            Wall)

/// The corner the north and west borders meet at, walled but for a west run
/// of y 2..6 and a north run of x 1..5: (0,1) is wall, so from (1,1) the
/// nearest exit is (1,0) on the north side, one step, and the west run's
/// nearest is (0,2), a diagonal.
let private cornerBorder =
    plainRing
    |> Map.map (fun tile terrain ->
        if
            (tile.X = 0 && tile.Y >= 2 && tile.Y <= 6)
            || (tile.Y = 0 && tile.X >= 1 && tile.X <= 5)
        then
            terrain
        else
            Wall)

/// W1N2 seen at `time` with these hostiles in it, over this border.
let private watchedOn border time hostiles : World =
    { World.empty with
        Time = time
        Rooms =
            Map.ofList
                [
                    "W1N2",
                    { RoomFacts.empty with
                        Border = border
                        Hostiles = hostiles
                    }
                ]
        Sightings =
            Map.ofList
                [
                    "W1N2",
                    {
                        Tick = time
                        Targets = lazy Set.empty
                        Rival = None
                    }
                ]
    }

/// W1N2 seen at `time` over the west border.
let private watched time hostiles = watchedOn westBorder time hostiles

/// The same room at `time` with nothing of ours to see it by.
let private dark time : World =
    { watched time [] with
        Sightings = Map.empty
    }

let private kiterAt pos ttl =
    { hostileIn "W1N2" pos [ RangedAttack; Move ] with
        TicksToLive = ttl
    }

/// The same world with these creeps' tombstones lying in W1N2.
let private withFallen (fallen: string list) (world: World) : World =
    { world with
        Rooms =
            world.Rooms
            |> Map.change "W1N2" (Option.map (fun facts -> { facts with Fallen = fallen }))
    }

/// The same world with W2N2, the room across W1N2's west border, seen too,
/// these hostiles standing in it.
let private withWestSeen hostiles (world: World) : World =
    { world with
        Rooms =
            world.Rooms
            |> Map.add
                "W2N2"
                { RoomFacts.empty with
                    // Standing there, whatever room the fixture built them in.
                    Hostiles =
                        hostiles
                        |> List.map (fun h ->
                            { h with
                                Pos = RoomPos.at "W2N2" (RoomPos.pos h.Pos)
                            })
                }
        Sightings =
            world.Sightings
            |> Map.add
                "W2N2"
                {
                    Tick = world.Time
                    Targets = lazy Set.empty
                    Rival = None
                }
    }

let private watchOf (world: World) = Map.tryFind "W1N2" world.ExitWatches

/// The watch carried through a run of ticks, from none.
let private watchThrough (worlds: World list) : World =
    worlds
    |> List.fold
        (fun (previous: World) world -> World.watchExits previous.ExitWatches world)
        World.empty

/// The hold the watch has standing, if any.
let private holdOf (world: World) =
    match watchOf world with
    | Some(ExitWatch.Held hold) -> Some hold
    | _ -> None

/// The raid memory carried through a run of ticks, from none.
let private recallThrough (worlds: World list) : World =
    worlds
    |> List.fold
        (fun (previous: World) world -> World.recallRaiders previous.Raiders world)
        World.empty

/// The ids W1N2's raid memory holds, and the tick it was seen.
let private recalledIn (world: World) =
    Map.tryFind "W1N2" world.Raiders
    |> Option.map (fun seen -> seen.Raid |> List.map (fun h -> h.Id), seen.Tick)

[<Tests>]
let raidMemoryTests =
    testList
        "a raider stays its room's raid until seen gone (#569)"
        [
            test "a raider seen is remembered through the blind ticks after" {
                let raider = kiterAt { X = 25; Y = 25 } 1500

                Expect.equal
                    (recallThrough [ watched 100 [ raider ]; dark 101; dark 108 ] |> recalledIn)
                    (Some([ "h-1" ], 100))
                    "dark, the room holds the raid as last seen"
            }

            test "a look that finds the room empty of raiders forgets it" {
                let scout =
                    { hostileIn "W1N2" { X = 20; Y = 20 } [ Move ] with
                        Id = "s-1"
                    }

                Expect.isNone
                    (recallThrough
                        [ watched 100 [ kiterAt { X = 25; Y = 25 } 1500 ]; watched 108 [ scout ] ]
                     |> recalledIn)
                    "seen gone, gone: the scout left there is no raid"

                Expect.isNone
                    (recallThrough [ watched 100 [ scout ] ] |> recalledIn)
                    "and a room no raider was seen in holds nothing"
            }

            test "a raider seen fallen, or standing in another room, is gone from it" {
                let raider = kiterAt { X = 1; Y = 22 } 1500

                Expect.isNone
                    (recallThrough [ watched 100 [ raider ]; dark 101 |> withFallen [ "h-1" ] ]
                     |> recalledIn)
                    "its tombstone lies there"

                Expect.isNone
                    (recallThrough
                        [
                            watched 100 [ raider ]
                            dark 101
                            |> withWestSeen
                                [
                                    { raider with
                                        Pos = RoomPos.at "W2N2" { X = 48; Y = 22 }
                                    }
                                ]
                        ]
                     |> recalledIn)
                    "it stands across the exit"
            }

            test "the memory ends with the raider's life" {
                let raider = kiterAt { X = 25; Y = 25 } 30

                Expect.isSome
                    (recallThrough [ watched 100 [ raider ]; dark 129 ] |> recalledIn)
                    "a tick of life left, it is remembered"

                Expect.isNone
                    (recallThrough [ watched 100 [ raider ]; dark 130 ] |> recalledIn)
                    "its thirty ticks run out, it is not"
            }
        ]

[<Tests>]
let exitHoldTests =
    testList
        "the exit a raid left by"
        [
            test "a threat leaving by the west exit is held at the run it left by" {
                let seen =
                    World.watchExits Map.empty (watched 100 [ kiterAt { X = 1; Y = 22 } 1500 ])

                // A tile a tick on plain out and back, across a room's side:
                // a hundred ticks.
                Expect.equal
                    (watchOf seen)
                    (Some(
                        ExitWatch.Seen
                            [
                                {
                                    Id = "h-1"
                                    At = { X = 1; Y = 22 }
                                    Dies = 1600
                                    Trip = 100
                                }
                            ]
                    ))
                    "while it stands there the world remembers where, when it dies, and its round trip"

                let gone = World.watchExits seen.ExitWatches (watched 101 [])

                Expect.equal
                    (watchOf gone)
                    (Some(
                        ExitWatch.Held
                            {
                                Run = [ for y in 20..24 -> { X = 0; Y = y } ]
                                Exit = { X = 0; Y = 22 }
                                Left = 101
                                Leavers =
                                    [
                                        {
                                            Id = "h-1"
                                            At = { X = 1; Y = 22 }
                                            Dies = 1600
                                            Trip = 100
                                        }
                                    ]
                                Until = 101 + 100
                            }
                    ))
                    "the tick vision finds it gone, the whole west run beside it is held for its round trip"
            }

            // Moved by #663 S5 (#585): the hold's clock is the leaver's own
            // round trip, the odds it is back falling to nothing over it, not
            // the fixed `ExitHoldTicks` (gone); a body a tile a tick reads the
            // hundred it was.
            test "the hold ends at the threat's round trip, and at its remembered death if sooner" {
                let holdAfter ttl =
                    watchThrough [ watched 100 [ kiterAt { X = 1; Y = 22 } ttl ]; watched 101 [] ]

                let long = holdAfter 1500
                let until = 101 + 100

                Expect.isSome
                    (World.watchExits long.ExitWatches (watched (until - 1) []) |> watchOf)
                    "the hold stands to its last tick"

                Expect.isNone
                    (World.watchExits long.ExitWatches (watched until []) |> watchOf)
                    "and ends with the round trip"

                Expect.isSome
                    (World.watchExits long.ExitWatches (dark 150) |> watchOf)
                    "out of sight it keeps its clock"

                Expect.equal
                    (holdAfter 30 |> holdOf |> Option.map (fun hold -> hold.Until))
                    (Some 130)
                    "a threat with thirty ticks to live is held thirty ticks from its last sighting"
            }

            test
                "the deadline is the death of the threat whose exit is held, not the longest-lived's" {
                // Two vanish together: the one beside the west run left by it with
                // thirty ticks to live; the one in mid-room, with 1,500, died there.
                let held =
                    watchThrough
                        [
                            watched
                                100
                                [ kiterAt { X = 1; Y = 22 } 30; kiterAt { X = 25; Y = 25 } 1500 ]
                            watched 101 []
                        ]

                Expect.equal
                    (holdOf held |> Option.map (fun hold -> hold.Until))
                    (Some 130)
                    "held until the leaver's own death"
            }

            test "a sighting carried through darkness starts no hold when vision returns" {
                Expect.isNone
                    (watchThrough
                        [ watched 100 [ kiterAt { X = 1; Y = 22 } 1500 ]; dark 101; watched 102 [] ]
                     |> watchOf)
                    "the room was dark on the full tick before: where the threat went is not known"
            }

            test "a threat leaving by the north exit near the west corner is held at the north run" {
                let held =
                    watchThrough
                        [
                            watchedOn cornerBorder 100 [ kiterAt { X = 1; Y = 1 } 1500 ]
                            watchedOn cornerBorder 101 []
                        ]

                Expect.equal
                    (holdOf held |> Option.map (fun hold -> hold.Run))
                    (Some [ for x in 1..5 -> { X = x; Y = 0 } ])
                    "(1,0) is a step from (1,1) and (0,2) a diagonal: the north run is the nearer"
            }

            test "a threat that vanishes away from every exit died, and nothing is held" {
                Expect.isNone
                    (watchThrough
                        [ watched 100 [ kiterAt { X = 25; Y = 22 } 1500 ]; watched 101 [] ]
                     |> watchOf)
                    "no exit is a step from (25,22)"

                Expect.isNone
                    (watchThrough [ watched 100 [ kiterAt { X = 1; Y = 30 } 1500 ]; watched 101 [] ]
                     |> watchOf)
                    "nor from (1,30), whose border is wall"
            }

            test "a threat that died beside the exit, its tombstone lying there, holds nothing" {
                let lastSeen = watched 100 [ kiterAt { X = 1; Y = 22 } 1500 ]

                Expect.isSome
                    (watchThrough [ lastSeen; watched 101 [] ] |> holdOf)
                    "the premise: gone from beside the run with no tombstone, it left"

                Expect.isNone
                    (watchThrough [ lastSeen; watched 101 [] |> withFallen [ "h-1" ] ] |> watchOf)
                    "with its tombstone in the room it died a step from the exit"

                Expect.isSome
                    (watchThrough [ lastSeen; watched 101 [] |> withFallen [ "h-other" ] ] |> holdOf)
                    "another creep's tombstone says nothing of this one"
            }

            test
                "a threat that vanished beside the exit is held only if the room across could hold it" {
                let lastSeen = watched 100 [ kiterAt { X = 1; Y = 22 } 1500 ]

                Expect.isSome
                    (watchThrough
                        [
                            lastSeen
                            watched 101 [] |> withWestSeen [ kiterAt { X = 49; Y = 22 } 1400 ]
                        ]
                     |> holdOf)
                    "seen across the border, it stepped onto the exit and was carried over"

                Expect.isNone
                    (watchThrough [ lastSeen; watched 101 [] |> withWestSeen [] ] |> watchOf)
                    "the room across seen and it in none: it died, it did not cross"

                let onRing = watched 100 [ kiterAt { X = 0; Y = 22 } 1500 ]

                Expect.isSome
                    (watchThrough [ onRing; watched 101 [] ] |> holdOf)
                    "the premise: last seen on the exit tile itself and gone, it crossed"

                Expect.isNone
                    (watchThrough [ onRing; watched 101 [] |> withWestSeen [] ] |> watchOf)
                    "last seen on the ring, the room across seen and it not there: it died"
            }

            // #585 (#663 S5): a hold ends on evidence, not a clock. A look
            // into the room across the exit finding the leaver walked off, or
            // not there at all, ends it; finding it lingering by the border
            // keeps it; out of sight it keeps its clock.
            test
                "a look across the exit ends the hold where the leaver walked off, and keeps it where it lingers" {
                let held =
                    watchThrough [ watched 100 [ kiterAt { X = 1; Y = 22 } 1500 ]; watched 101 [] ]

                Expect.isSome (holdOf held) "the premise: held, the room across dark"

                let lookAcross across =
                    World.watchExits held.ExitWatches (watched 110 [] |> withWestSeen across)
                    |> holdOf

                Expect.isSome
                    (lookAcross [ kiterAt { X = 47; Y = 22 } 1400 ])
                    "seen two tiles over the border: it may step back in, held"

                Expect.isNone
                    (lookAcross [ kiterAt { X = 30; Y = 22 } 1400 ])
                    "seen walked off into the room across: the hold ends"

                Expect.isNone (lookAcross []) "the room across seen without it: the hold ends"

                Expect.isSome
                    (World.watchExits held.ExitWatches (watched 110 []) |> holdOf)
                    "the room across dark: it keeps its clock"

                // Seen lingering past its round trip, it may still step back
                // in: the look keeps the hold, its clock restarted from it.
                Expect.equal
                    (World.watchExits
                        held.ExitWatches
                        (watched 250 [] |> withWestSeen [ kiterAt { X = 47; Y = 22 } 1400 ])
                     |> holdOf
                     |> Option.map _.Until)
                    (Some(250 + 100))
                    "lingering at t250, past its round trip: held another round trip"

                // The review of #663 S5: every leaver, not the one nearest
                // the exit. Two leave; one walks off, the other lingers.
                let pair =
                    watchThrough
                        [
                            watched
                                100
                                [
                                    kiterAt { X = 1; Y = 22 } 1500
                                    { kiterAt { X = 1; Y = 23 } 1500 with
                                        Id = "h-2"
                                    }
                                ]
                            watched 101 []
                        ]

                Expect.equal
                    (holdOf pair |> Option.map (fun hold -> hold.Leavers |> List.map _.Id))
                    (Some [ "h-1"; "h-2" ])
                    "the premise: both are the hold's leavers"

                Expect.isSome
                    (World.watchExits
                        pair.ExitWatches
                        (watched 110 []
                         |> withWestSeen
                             [
                                 kiterAt { X = 30; Y = 22 } 1400
                                 { kiterAt { X = 48; Y = 23 } 1400 with
                                     Id = "h-2"
                                 }
                             ])
                     |> holdOf)
                    "the leader walked off, the second lingers: held"

                Expect.isNone
                    (World.watchExits
                        pair.ExitWatches
                        (watched 110 []
                         |> withWestSeen
                             [
                                 kiterAt { X = 30; Y = 22 } 1400
                                 { kiterAt { X = 25; Y = 23 } 1400 with
                                     Id = "h-2"
                                 }
                             ])
                     |> holdOf)
                    "both walked off: the hold ends"
            }

            // #585 (#663 S5): the held exit's job follows its leaver. Seen
            // lingering across the border, the hold intercepts the tile it
            // steps back in by; seen walked off, or unseen, it holds the run.
            test "a leaver seen lingering across the exit is intercepted where it steps back in" {
                let across (at: Pos) =
                    { kiterAt at 1400 with
                        Id = "h-x"
                        Pos = RoomPos.at "W2N2" at
                    }

                let colony leaver =
                    let held = declaredRaid [] |> westField |> heldWest

                    { held with
                        Hostiles = leaver |> Option.toList
                        ExitHolds =
                            held.ExitHolds
                            |> Map.map (fun _ hold ->
                                { hold with
                                    Leavers =
                                        [
                                            {
                                                Id = "h-x"
                                                At = { X = 1; Y = 44 }
                                                Dies = held.Time + 1400
                                                Trip = 100
                                            }
                                        ]
                                })
                    }

                let jobOf leaver =
                    let view = colony leaver

                    Threats.jobsIn
                        (threatsOfHeld
                            view
                            (Atlas.ofView view)
                            (heldTaskFacts view (Map.ofList [ "g-1", taskId (Guard "W1N2") ])))
                        "W1N2"
                    |> List.head

                let lingering = jobOf (Some(across { X = 47; Y = 47 }))

                Expect.equal
                    (lingering.Kind, lingering.Quarry)
                    (JobKind.Intercept, [ "h-x" ])
                    "lingering two tiles over: an intercept on the leaver"

                Expect.isTrue
                    (not lingering.Tiles.IsEmpty
                     && lingering.Tiles
                        |> Set.forall (fun tile ->
                            range (RoomPos.pos tile) { X = 0; Y = 47 } <= Engine.rangedRange))
                    $"within a shot of (0,47), where it steps back in: {lingering.Tiles}"

                // Its price is the kill the hold's holder lands on it where
                // it stands, across the border (the review of #663 S5: read
                // in the hold's own room, it was nothing).
                Expect.isGreaterThan lingering.Value.Value 0.0 "priced at its kill score"

                // Lingering beside another border of the room beside it is
                // no way back in by this exit.
                let elsewhere =
                    { across { X = 47; Y = 47 } with
                        Pos = RoomPos.at "W2N2" { X = 20; Y = 48 }
                    }

                Expect.equal
                    (jobOf (Some elsewhere)).Kind
                    JobKind.HoldExit
                    "by W2N2's south border, not ours: the run held"

                Expect.equal
                    (jobOf (Some(across { X = 30; Y = 47 }))).Kind
                    JobKind.HoldExit
                    "walked off past the border's depth: the run held"

                Expect.equal (jobOf None).Kind JobKind.HoldExit "unseen: the run held"
            }

            test "W17S26 (#561): Beam715 dead at 1,6 beside the west run holds no exit" {
                let beam =
                    { kiterAt { X = 1; Y = 6 } 800 with
                        Id = "beam715"
                    }

                let lastSeen = watchedOn w17s26Border 986951 [ beam ]
                let next = watchedOn w17s26Border 986952 []

                Expect.equal
                    (watchThrough [ lastSeen; next ] |> holdOf |> Option.map _.Exit)
                    (Some { X = 0; Y = 6 })
                    "the premise: the watch read the vanished body as leaving by 0,6"

                Expect.isNone
                    (watchThrough [ lastSeen; next |> withFallen [ "beam715" ] ] |> watchOf)
                    "its tombstone at 1,6 says it died: no hold, no guard sent to the corpse"
            }

            test "the threat re-entering puts the ground back on its ring" {
                let held =
                    watchThrough [ watched 100 [ kiterAt { X = 1; Y = 22 } 1500 ]; watched 101 [] ]

                Expect.equal
                    (World.watchExits
                        held.ExitWatches
                        (watched 102 [ kiterAt { X = 0; Y = 22 } 1400 ])
                     |> watchOf)
                    (Some(
                        ExitWatch.Seen
                            [
                                {
                                    Id = "h-1"
                                    At = { X = 0; Y = 22 }
                                    Dies = 1502
                                    Trip = 100
                                }
                            ]
                    ))
                    "seen again, the world remembers the threat and holds nothing"

                let ringOf colony =
                    Threats.ringIn (threatsOf colony (Atlas.ofView colony)) "W1N2"

                let back = declaredRaid raiders |> westField

                Expect.equal
                    (ringOf (heldWest back))
                    (ringOf back)
                    "and a Threat standing in the room is the Guard's ground whatever is held"

                Expect.isFalse
                    (ringOf back |> Set.exists (fun tile -> tile.X = 1))
                    "which is its own ring, nowhere near the run"
            }

            test
                "the raid gone by the west exit leaves the Guard on the tiles beside that run, and the guard kept" {
                let quiet =
                    declaredRaid [] |> westField |> withGuards [ guard "g-1", { X = 1; Y = 44 } ]

                let held = Map.ofList [ "g-1", taskId (Guard "W1N2") ]

                let releasesOf colony =
                    (decide colony held Set.empty None).Verdicts
                    |> List.choose (function
                        | Verdict.Released(creep, task, reason) -> Some(creep, task, reason)
                        | _ -> None)

                Expect.equal
                    (releasesOf quiet)
                    [ "g-1", taskId (Guard "W1N2"), ReleaseReason.TaskGone ]
                    "the premise: with nothing held the Guard goes with the raid"

                let holding = heldWest quiet

                Expect.isEmpty (releasesOf holding) "with the exit held the guard keeps its Guard"

                Expect.equal
                    ((decide holding held Set.empty None).Assignments |> Map.tryFind "g-1")
                    (Some(taskId (Guard "W1N2")))
                    "and holds it"

                Expect.equal
                    (Threats.ringIn (threatsOf holding (Atlas.ofView holding)) "W1N2")
                    ([ for y in 41..48 -> { X = 1; Y = y } ]
                     |> List.map (RoomPos.at "W1N2")
                     |> Set.ofList)
                    "and its ground is the tiles a step inside the run"
            }

            test "a hold with no guard alive pools no Guard" {
                // The hold keeps a living guard; it is no reason to send one.
                Expect.isNone
                    (entryFor (Guard "W1N2") (pooledOf (declaredRaid [] |> westField |> heldWest)))
                    "nobody holds the room's Guard, so none is pooled"
            }

            test "a hold whose ground comes out empty is no hold, and the guard goes" {
                // Without the field the terrain lays nothing walkable beside the
                // run: the Guard is not kept on an empty ring, which would send
                // the guard to the room's sources instead.
                let bare =
                    declaredRaid [] |> withGuards [ guard "g-1", { X = 1; Y = 44 } ] |> heldWest

                let held = Map.ofList [ "g-1", taskId (Guard "W1N2") ]

                Expect.isEmpty
                    (Threats.ringIn (threatsOf bare (Atlas.ofView bare)) "W1N2")
                    "the premise: nothing walkable beside the run"

                Expect.equal
                    ((decide bare held Set.empty None).Verdicts
                     |> List.choose (function
                         | Verdict.Released(creep, task, reason) -> Some(creep, task, reason)
                         | _ -> None))
                    [ "g-1", taskId (Guard "W1N2"), ReleaseReason.TaskGone ]
                    "the Guard goes as with nothing held"
            }

            test "exit tiles themselves are never in the ground" {
                let holding = declaredRaid [] |> westField |> heldWest
                let ground = Threats.ringIn (threatsOf holding (Atlas.ofView holding)) "W1N2"

                Expect.isNonEmpty ground "the premise: the hold has ground"

                Expect.isFalse
                    (ground |> Set.exists (fun tile -> tile.X = 0))
                    "and none of it is an exit tile the terrain lays, which would carry the guard across"
            }
        ]

/// The pair world's two homes as two colonies of their own (#663): the
/// mother's RCL5 home with its 1,800 bank and one spawn, its outpost beside
/// it, and the child grown into an RCL8 colony with a 12,900 bank and a
/// Storage past the Caster's reserve, a room off the outpost.
let private lenders =
    ViewFixtures.declared |> List.map (fun colony -> { colony with Mother = None })

/// Two of Trepidimous's 7M13R standing in `room` (W17S26, t1060231): 130 a
/// tick each, 2,000 hits, no heal, two ticks a plain tile.
let private harassers (room: string) : HostileInfo list =
    [ 0; 1 ]
    |> List.map (fun i ->
        {
            Id = $"trep-{room}-{i}"
            Owner = "Trepidimous"
            Pos = { Room = room; X = 5 + i; Y = 6 }
            Body = List.replicate 7 Move @ List.replicate 13 RangedAttack
            Boosts = []
            Hits = 20 * Engine.partHits
            TicksToLive = 1400
        })

/// The pair world with the child an RCL8 colony, these hostiles in the
/// mother's outpost and these in the child's home.
let private lendingWorld (inOutpost: HostileInfo list) (atChild: HostileInfo list) : World =
    let rooms = ViewFixtures.pairWorld.Rooms

    { ViewFixtures.pairWorld with
        Rooms =
            rooms
            |> Map.change
                ViewFixtures.outpost
                (Option.map (fun (facts: RoomFacts) -> { facts with Hostiles = inOutpost }))
            |> Map.change
                ViewFixtures.child
                (Option.map (fun (facts: RoomFacts) ->
                    { ViewFixtures.withStock 100_000 facts with
                        Controller =
                            facts.Controller |> Option.map (fun ctrl -> { ctrl with Level = 8 })
                        Energy =
                            {
                                Available = 12_900
                                Capacity = 12_900
                            }
                        Hostiles = atChild
                    }))
    }

/// A third colony, W11S27, an RCL8 home as rich as the child, a room off
/// the outpost on its other side.
let private third = "W11S27"

let private trio: Colony list =
    lenders
    @ [
        { (List.last lenders) with
            Home = third
            Mother = None
        }
    ]

/// `lendingWorld` with the third colony's home in it at this level and
/// bank, seen this tick.
let private trioWorldAt (level: int) (bank: int) (inOutpost: HostileInfo list) : World =
    let world = lendingWorld inOutpost []

    let name, facts =
        ViewFixtures.roomOf third Ownership.Ours [ $"ctrl-{third}", { X = 2; Y = 2 }, Controller ]
        |> ViewFixtures.ourColony "Spawn3" level bank

    let facts = ViewFixtures.withStock 100_000 facts

    { world with
        Rooms = Map.add name facts world.Rooms
        Sightings =
            Map.add
                name
                {
                    Tick = world.Time
                    Targets = lazy (facts.TargetKinds |> Map.keys |> Set.ofSeq)
                    Rival = None
                }
                world.Sightings
    }

/// `trioWorldAt` with the third colony an RCL8 home as rich as the child.
let private trioWorld (inOutpost: HostileInfo list) : World = trioWorldAt 8 12_900 inOutpost

/// Each colony's view with the world's verdicts priced over all of them
/// (`Facts.judge`), these Tasks held, a crossing off the room names.
let private judgedHolding (held: Assignments) (colonies: Colony list) (world: World) =
    colonies
    |> List.map (fun colony ->
        ({
            Cut = ViewFixtures.viewUnder colonies world colony.Home
            Owns = Colony.declaredRooms colony
            Decides = true
        }
        : Facts.Judged))
    |> Facts.judge (fun _ _ -> None) held

/// `judgedHolding` with nothing held.
let private judgedAll (colonies: Colony list) (world: World) : ColonyView list =
    judgedHolding Map.empty colonies world

/// The same raid aged to `life` ticks.
let private aged (life: int) (raid: HostileInfo list) =
    raid |> List.map (fun hostile -> { hostile with TicksToLive = life })

/// A guard of `blocks` guard blocks named `name` standing in `room`.
let private guardAt (room: string) (name: string) (blocks: int) (world: World) : World =
    let parts =
        guardPattern.Block
        |> List.countBy id
        |> List.map (fun (part, count) -> part, count * blocks)

    let guard = ViewFixtures.creepWithBody name room parts
    let hits = Engine.partHits * blocks * List.length guardPattern.Block

    { world with
        Creeps =
            { guard with
                Info =
                    { guard.Info with
                        Hits = { Hits = hits; HitsMax = hits }
                    }
            }
            :: world.Creeps
        Rooms =
            world.Rooms
            |> Map.change
                room
                (Option.map (fun (facts: RoomFacts) ->
                    { facts with
                        Layer =
                            { facts.Layer with
                                CreepPositions =
                                    Map.add name { X = 8; Y = 8 } facts.Layer.CreepPositions
                            }
                    }))
    }

/// A guard of `blocks` guard blocks named `name` standing in the outpost,
/// which the outpost's projection adopts into the owner's cut.
let private guardIn (name: string) (blocks: int) (world: World) : World =
    guardAt ViewFixtures.outpost name blocks world

/// The lender's five-block guard standing in the outpost.
let private lentGuard (world: World) : World = guardIn "guard-990-Spawn2" 5 world

/// The bodies each colony casts for a Task (`Caster.assign`), each judged
/// view asking what it asks (`Quota.castsOf`); a colony casting none left
/// out.
let private castHeld (held: Assignments) (task: Task) (views: ColonyView list) =
    let facts: HeldTaskFacts =
        {
            All = held |> Map.values |> Set.ofSeq
            WithThorium = Set.empty
            Holders =
                held
                |> Map.toList
                |> List.groupBy snd
                |> List.map (fun (tid, holders) -> tid, List.map fst holders)
                |> Map.ofList
        }

    views
    |> List.map (fun view ->
        let atlas = Atlas.ofView view

        ({
            Home = SpatialInfo.homeName view.Spatial
            View = view
            Asks =
                Quota.castsOf
                    view
                    (Threat.threatsOfHeld view atlas facts)
                    (Planner.outpostFactsOf view atlas).Guarded
                    (fun _ -> false)
        }
        : Caster.Bidder))
    |> Caster.assign (fun _ _ -> None) held Map.empty
    |> Map.toList
    |> List.choose (fun (home, casts) ->
        match
            casts
            |> List.filter (fun cast -> cast.For = task)
            |> List.sumBy (fun cast -> cast.Ask.Wanted)
        with
        | 0 -> None
        | bodies -> Some(home, bodies))

/// The bodies and their blocks each colony casts for a Task, nothing held;
/// a colony casting none left out.
let private castsFor (task: Task) (views: ColonyView list) : (string * int * int) list =
    views
    |> List.map (fun view ->
        let atlas = Atlas.ofView view

        ({
            Home = SpatialInfo.homeName view.Spatial
            View = view
            Asks =
                Quota.castsOf
                    view
                    (threatsOf view atlas)
                    (Planner.outpostFactsOf view atlas).Guarded
                    (fun _ -> false)
        }
        : Caster.Bidder))
    |> Caster.assign (fun _ _ -> None) Map.empty Map.empty
    |> Map.toList
    |> List.collect (fun (home, casts) ->
        casts
        |> List.filter (fun cast -> cast.For = task && cast.Ask.Wanted > 0)
        |> List.map (fun cast -> home, cast.Ask.Wanted, cast.Ask.Blocks))

/// What one judged view asks for a Task (`Quota.castsOf`), nothing held:
/// row, bodies and blocks.
let private askedFor (task: Task) (view: ColonyView) : (CastRow * int * int) list =
    let atlas = Atlas.ofView view

    Quota.castsOf view (threatsOf view atlas) (Planner.outpostFactsOf view atlas).Guarded (fun _ ->
        false)
    |> List.filter (fun cast -> cast.For = task && cast.Ask.Wanted > 0)
    |> List.map (fun cast -> cast.Ask.Row, cast.Ask.Wanted, cast.Ask.Blocks)

/// `castsFor` with each cast's row.
let private castRowsFor
    (task: Task)
    (views: ColonyView list)
    : (string * CastRow * int * int) list =
    views
    |> List.map (fun view ->
        let atlas = Atlas.ofView view

        ({
            Home = SpatialInfo.homeName view.Spatial
            View = view
            Asks =
                Quota.castsOf
                    view
                    (threatsOf view atlas)
                    (Planner.outpostFactsOf view atlas).Guarded
                    (fun _ -> false)
        }
        : Caster.Bidder))
    |> Caster.assign (fun _ _ -> None) Map.empty Map.empty
    |> Map.toList
    |> List.collect (fun (home, casts) ->
        casts
        |> List.filter (fun cast -> cast.For = task && cast.Ask.Wanted > 0)
        |> List.map (fun cast -> home, cast.Ask.Row, cast.Ask.Wanted, cast.Ask.Blocks))

/// `castHeld` with nothing held.
let private castFor (task: Task) (views: ColonyView list) : (string * int) list =
    views
    |> List.map (fun view ->
        let atlas = Atlas.ofView view

        ({
            Home = SpatialInfo.homeName view.Spatial
            View = view
            Asks =
                Quota.castsOf
                    view
                    (threatsOf view atlas)
                    (Planner.outpostFactsOf view atlas).Guarded
                    (fun _ -> false)
        }
        : Caster.Bidder))
    |> Caster.assign (fun _ _ -> None) Map.empty Map.empty
    |> Map.toList
    |> List.choose (fun (home, casts) ->
        match
            casts
            |> List.filter (fun cast -> cast.For = task)
            |> List.sumBy (fun cast -> cast.Ask.Wanted)
        with
        | 0 -> None
        | bodies -> Some(home, bodies))

/// The rooms a colony's stand-down shuts off this tick's sightings.
let private shutBy (colony: ColonyView) =
    Observe.RaidState.empty
    |> Observe.foldRaids
        Observe.capEpisodes
        Set.empty
        colony
        (Planner.outpostFactsOf colony (Atlas.ofView colony)).Declared
    |> Observe.standDown Tuning.defaults (colony.Time + 1)
    |> fun gate -> gate.Shut

/// The whole tick over these colonies (`Entry.decideColonies`), these Tasks
/// held: the Task each body is matched to, by name, whichever colony
/// matches it.
let private matchedAll (held: Assignments) (colonies: Colony list) (world: World) =
    let tuning = Tuning.defaults

    let gates =
        colonies |> List.map (fun colony -> colony.Home, StandDown.none) |> Map.ofList

    let joins = JoinTable()

    let holders =
        World.creepColonies
            tuning
            colonies
            (World.living colonies world)
            ViewFixtures.noneShut
            world

    let watch = WorldView.ofWorld colonies (gates |> Map.toList |> List.map snd) world

    let views =
        colonies
        |> List.map (fun colony ->
            colony,
            ColonyView.ofWorldRecalling
                joins
                tuning
                colonies
                HarassCasting.none
                gates.[colony.Home]
                holders
                isFighterBody
                watch
                world
                colony)

    (decideColonies
        {
            Hops = fun _ _ -> None
            Holding = held
            Verbose = Set.empty
            PlanMemos = Map.empty
            Carries = Map.empty
            Idle = Set.empty
            FightAlone = Set.empty
            TurnIndex = None
            Operations = []
            Declared = []
        }
        ignore
        views)
        .Decided
    |> List.collect (fun (_, _, decision, ()) ->
        decision.Verdicts
        |> List.choose (function
            | Verdict.Matched(creep, task, _) -> Some(creep, task)
            | _ -> None))
    |> Map.ofList

[<Tests>]
let lentOutpostTests =
    testList
        "an outpost raid its own colony loses is priced by what any colony may lend it (#663)"
        [
            // The W17S26 shape: the RCL5 owner's 1,800 bank loses two 7M13R,
            // and an RCL8 colony a room off wins them. That colony casts for
            // the outpost as its owner does: its row is weighed, the verdict
            // is its Win, the owner keeps the outpost open, and the owner's
            // ask, sized to the winning row, is cast by it at its own blocks.
            test "the W17S26 shape: an RCL8 colony's cast wins the RCL5 owner's outpost" {
                let world = lendingWorld (harassers ViewFixtures.outpost) []

                let alone =
                    judgedAll
                        (lenders |> List.filter (fun colony -> colony.Home = ViewFixtures.mother))
                        world

                Expect.equal
                    (Facts.answer (List.head alone) ViewFixtures.outpost)
                    Answer.Lose
                    "the premise: the owner's own bank loses it"

                let views = judgedAll lenders world
                let owner = List.head views
                let verdict = Facts.verdictOf owner ViewFixtures.outpost

                Expect.equal verdict.Answer Answer.Win "the lender's cast wins it"

                Expect.equal
                    verdict.Casting
                    (Set.singleton ViewFixtures.child)
                    "and the Win casts the lender"

                Expect.equal
                    (verdict.Rows
                     |> Map.map (fun _ row -> row.Wins, row.Bodies, row.Blocks)
                     |> Map.tryFind ViewFixtures.child)
                    (Some(true, 1, 5))
                    "its row: one five-block guard"

                Expect.isTrue
                    (fst (
                        Facts.castPrice
                            (List.item 1 views)
                            (CastRow.Fighter BodyTemplate.Melee)
                            ViewFixtures.outpost
                    ))
                    "the Caster's price of the lender's cast wins it too"

                Expect.isFalse
                    (Set.contains ViewFixtures.outpost (shutBy owner))
                    "the owner does not stand the outpost down"

                Expect.equal
                    (castsFor (Guard ViewFixtures.outpost) views)
                    [ ViewFixtures.child, 1, 5 ]
                    "and the outpost's Guard is cast by the lender alone: one body, at its own five blocks"
            }

            // The lender's stock dipping under the reserve as it pays for its
            // body hands the room to no one: a colony whose body holds the
            // room's fight, or whose oven holds a fighter, casts down to
            // `Tuning.CasterReserveHold`, as `World.casterOf` holds a
            // harassment room's caster. Under the hold it casts nothing.
            test "a lender whose body holds the fight keeps casting down to the hold" {
                let lent = "guard-990-Spawn2"
                let held = Map.ofList [ lent, taskId (Guard ViewFixtures.outpost) ]

                let between =
                    (Tuning.defaults.CasterReserveHold + Tuning.defaults.CasterReserve) / 2

                let stocked (stock: int) (holding: bool) =
                    let world = lendingWorld (harassers ViewFixtures.outpost) []
                    let world = if holding then guardIn lent 3 world else world

                    let world =
                        { world with
                            Rooms =
                                world.Rooms
                                |> Map.change
                                    ViewFixtures.child
                                    (Option.map (ViewFixtures.withStock stock))
                        }

                    Facts.answer
                        (List.head (
                            judgedHolding (if holding then held else Map.empty) lenders world
                        ))
                        ViewFixtures.outpost

                Expect.equal
                    (stocked between false)
                    Answer.Lose
                    "under the reserve, with no body there, it casts nothing"

                Expect.equal
                    (stocked between true)
                    Answer.Win
                    "its body holding the Guard, it casts down to the hold"

                Expect.equal
                    (stocked (Tuning.defaults.CasterReserveHold - 1) true)
                    Answer.Lose
                    "under the hold its own economy comes first"
            }

            // A held body of any size is no lent cast: the owner's two-block
            // guard holding the Guard loses the raid, so the lender's guard
            // is still cast beside it.
            test "the owner's weak guard holding the outpost: the lender still casts" {
                let weak = "guard-990-Spawn1"
                let held = Map.ofList [ weak, taskId (Guard ViewFixtures.outpost) ]

                let views =
                    judgedHolding
                        held
                        lenders
                        (lendingWorld (harassers ViewFixtures.outpost) [] |> guardIn weak 2)

                let verdict = Facts.verdictOf (List.head views) ViewFixtures.outpost

                Expect.equal
                    (verdict.Answer, verdict.Casting)
                    (Answer.Win, Set.singleton ViewFixtures.child)
                    "the premise: the lender's row is the Win"

                Expect.contains
                    (castHeld held (Guard ViewFixtures.outpost) views)
                    (ViewFixtures.child, 1)
                    "the lender casts its guard beside the weak one"
            }

            // A free body of a caster whose cast loses where another's wins
            // covers nothing: the fleet's count reads no body's size, and the
            // owner's two-block guard standing by is no body the winning cast
            // would cast. The lender casts its own.
            test "the owner's free weak guard covers nothing the lender's cast wins" {
                let views =
                    judgedAll
                        lenders
                        (lendingWorld (harassers ViewFixtures.outpost) []
                         |> guardIn "guard-990-Spawn1" 2)

                Expect.equal
                    (castFor (Guard ViewFixtures.outpost) views)
                    [ ViewFixtures.child, 1 ]
                    "the lender casts the body, the weak guard counted for none"
            }

            // Two colonies could cast for it: both rows are weighed, as two
            // owners' would be, and the room is lent to the one whose cast
            // lands first — the third, a room nearer — once: the Win's one
            // caster, handed the one body.
            test "two colonies could lend: both are weighed, one is handed the body" {
                let views = judgedAll trio (trioWorld (harassers ViewFixtures.outpost))
                let verdict = Facts.verdictOf (List.head views) ViewFixtures.outpost

                Expect.equal
                    (verdict.Casting, verdict.Lent |> Option.map fst)
                    (Set.singleton third, Some third)
                    "the room is lent to the one landing first, the Win's one caster"

                Expect.equal
                    (castFor (Guard ViewFixtures.outpost) views)
                    [ third, 1 ]
                    "and it is handed the one body"
            }

            // Lenders of different sizes (#663): the RCL7 third wins with
            // one four-block guard holding its post, the RCL8 child with a
            // five-block one, and the third lands first. What is cast is the
            // winning cast of the colony casting it: the verdict's row, the
            // owner's ask and the Caster's cast read one row, bodies and
            // blocks together, never one colony's count at another's size.
            test "lenders of different sizes: the one handed the body casts its own winning cast" {
                let alone = judgedAll lenders (lendingWorld (harassers ViewFixtures.outpost) [])

                Expect.equal
                    ((Facts.verdictOf (List.head alone) ViewFixtures.outpost).Lent
                     |> Option.map (fun (home, row) -> home, row.Bodies, row.Blocks))
                    (Some(ViewFixtures.child, 1, 5))
                    "the premise: the child alone wins it with one five-block guard"

                let views = judgedAll trio (trioWorldAt 7 3_300 (harassers ViewFixtures.outpost))
                let owner = List.head views
                let verdict = Facts.verdictOf owner ViewFixtures.outpost

                Expect.equal
                    (verdict.Lent |> Option.map (fun (home, row) -> home, row.Bodies, row.Blocks))
                    (Some(third, 1, 4))
                    "the third, landing first, is lent it by its own row: one four-block guard"

                Expect.equal
                    (askedFor (Guard ViewFixtures.outpost) owner)
                    [ CastRow.Fighter BodyTemplate.Melee, 1, 4 ]
                    "the owner asks that row's cast"

                Expect.equal
                    (castRowsFor (Guard ViewFixtures.outpost) views)
                    [ third, CastRow.Fighter BodyTemplate.Melee, 1, 4 ]
                    "and the third casts it at its own count and blocks"
            }

            // A Ranged lender row (#663): three 7M13R, which the owner's
            // melee loses and the child's two eight-block rangers win. The
            // owner's ask follows the row the room is lent by — its template,
            // bodies and blocks — so the lender casts rangers, never a melee
            // ask sized to the owner's losing cast, which no lender's melee
            // wins and the owner would cast itself.
            test
                "a Ranged lender row winning where the owner's melee loses: a Ranged ask, cast by the lender" {
                let raid =
                    [ 0..2 ]
                    |> List.map (fun i ->
                        { List.head (harassers ViewFixtures.outpost) with
                            Id = $"trep-three-{i}"
                            Pos =
                                {
                                    Room = ViewFixtures.outpost
                                    X = 5 + i
                                    Y = 6
                                }
                        })

                let views = judgedAll lenders (lendingWorld raid [])
                let owner = List.head views
                let verdict = Facts.verdictOf owner ViewFixtures.outpost

                Expect.equal
                    (verdict.Rows
                     |> Map.tryFind ViewFixtures.mother
                     |> Option.map (fun row -> row.Ranger, row.Wins))
                    (Some(false, false))
                    "the premise: the owner's own row is a melee one that loses"

                Expect.equal
                    (verdict.Answer,
                     verdict.Lent
                     |> Option.map (fun (home, row) -> home, row.Ranger, row.Bodies, row.Blocks))
                    (Answer.Win, Some(ViewFixtures.child, true, 2, 8))
                    "the room is lent to the child's row: two eight-block rangers"

                Expect.equal
                    (askedFor (Guard ViewFixtures.outpost) owner)
                    [ CastRow.Fighter BodyTemplate.Ranged, 2, 8 ]
                    "the owner asks the lender's row: rangers, its count and blocks"

                Expect.equal
                    (castRowsFor (Guard ViewFixtures.outpost) views)
                    [ ViewFixtures.child, CastRow.Fighter BodyTemplate.Ranged, 2, 8 ]
                    "and the child casts them, nothing of the owner's"

                Expect.isFalse
                    (Set.contains ViewFixtures.outpost (shutBy owner))
                    "the owner keeps the outpost open"
            }

            test "a colony whose own home is raided lends nothing" {
                let world =
                    lendingWorld (harassers ViewFixtures.outpost) (harassers ViewFixtures.child)

                let views = judgedAll lenders world

                Expect.equal
                    (Facts.answer (List.head views) ViewFixtures.outpost)
                    Answer.Lose
                    "the outpost is the stand-down's: the only lender defends its home"

                Expect.isFalse
                    (castFor (Guard ViewFixtures.outpost) views
                     |> List.exists (fun (home, _) -> home = ViewFixtures.child))
                    "and its home casts no body for the outpost"
            }

            test "the owner's own row answers a raid it wins, whoever could lend" {
                let raider =
                    {
                        Id = "scout"
                        Owner = "Trepidimous"
                        Pos =
                            {
                                Room = ViewFixtures.outpost
                                X = 5
                                Y = 6
                            }
                        Body = [ Attack; Move ]
                        Boosts = []
                        Hits = 2 * Engine.partHits
                        TicksToLive = 1400
                    }

                let views = judgedAll lenders (lendingWorld [ raider ] [])
                let verdict = Facts.verdictOf (List.head views) ViewFixtures.outpost

                Expect.equal verdict.Answer Answer.Win "the owner's row wins it"

                Expect.contains verdict.Casting ViewFixtures.mother "and casts it"

                Expect.equal
                    (castFor (Guard ViewFixtures.outpost) views |> List.map fst)
                    [ ViewFixtures.mother ]
                    "and the owner casts its Guard"
            }

            // While the lender's body spawns, its stock dipped under the
            // reserve as it paid: the body in its oven holds it at the hold
            // band, the verdict keeps its Win, and the body in the oven is
            // the ask's (the fleet read first): no second colony casts
            // another.
            test "a lender's body in its oven keeps the Win and is the only body cast" {
                let between =
                    (Tuning.defaults.CasterReserveHold + Tuning.defaults.CasterReserve) / 2

                let spawning =
                    let world = trioWorld (harassers ViewFixtures.outpost)

                    { world with
                        Rooms =
                            world.Rooms
                            |> Map.change
                                ViewFixtures.child
                                (Option.map (fun (facts: RoomFacts) ->
                                    { ViewFixtures.withStock between facts with
                                        Spawns =
                                            facts.Spawns
                                            |> List.map (fun spawner ->
                                                { spawner with
                                                    IsSpawning = true
                                                    Remaining = 20
                                                })
                                        Casting =
                                            [
                                                {
                                                    Name = "guard-991-Spawn2"
                                                    Body =
                                                        List.replicate 5 guardPattern.Block
                                                        |> List.concat
                                                }
                                            ]
                                    }))
                    }

                let views = judgedAll trio spawning
                let verdict = Facts.verdictOf (List.head views) ViewFixtures.outpost

                Expect.equal verdict.Answer Answer.Win "the Win holds while the body spawns"

                Expect.contains
                    verdict.Casting
                    ViewFixtures.child
                    "the child still casts for it, at the hold"

                Expect.equal
                    (castFor (Guard ViewFixtures.outpost) views)
                    [ ViewFixtures.child, 1 ]
                    "the body in its oven is the ask's one body: the third colony casts none"
            }

            // A dismantler with no weapon raids a home of ours as surely as a
            // harasser: the world prices the home's raid over every colony, so
            // a colony that would answer it is weighed, and its raid lasts
            // while the dismantler does. (An outpost's unarmed digger keeps
            // the old pricing.)
            test "a healed dismantler with no weapon at a home is priced over every colony" {
                let train =
                    [
                        [ Work; Work; Work; Move; Move; Move ]
                        List.replicate 20 Heal @ List.replicate 10 Move
                    ]
                    |> List.mapi (fun i parts ->
                        {
                            Id = $"train-{i}"
                            Owner = "Trepidimous"
                            Pos =
                                {
                                    Room = ViewFixtures.mother
                                    X = 5 + i
                                    Y = 6
                                }
                            Body = parts
                            Boosts = []
                            Hits = Engine.partHits * List.length parts
                            TicksToLive = 1400
                        })

                let world =
                    let world = lendingWorld [] []

                    { world with
                        Rooms =
                            world.Rooms
                            |> Map.change
                                ViewFixtures.mother
                                (Option.map (fun (facts: RoomFacts) ->
                                    { facts with
                                        Hostiles = train
                                        Refillables = []
                                    }))
                    }

                let views = judgedAll lenders world

                Expect.isTrue
                    (Map.containsKey ViewFixtures.mother (List.head views).World.Answers)
                    "the world prices the home's raid"

                Expect.notEqual
                    (Facts.verdictOf (List.head views) ViewFixtures.mother).Answer
                    Answer.Lose
                    "and some colony answers it"
            }

            // The lender's five-block guard holding the outpost: the Guard
            // counts it, the Caster casts nothing fresh beside it.
            test "the lender's body holding the outpost is the ask's body, nothing fresh beside it" {
                let lent = "guard-990-Spawn2"
                let held = Map.ofList [ lent, taskId (Guard ViewFixtures.outpost) ]

                let views =
                    judgedHolding
                        held
                        lenders
                        (lendingWorld (harassers ViewFixtures.outpost) [] |> lentGuard)

                Expect.equal
                    (Facts.answer (List.head views) ViewFixtures.outpost)
                    Answer.Win
                    "its row wins the outpost"

                Expect.equal
                    (castHeld held (Guard ViewFixtures.outpost) views)
                    [ ViewFixtures.child, 1 ]
                    "the Guard counts the holding body alone"
            }

            // The tick the lender's body is born (#663): it has left the
            // oven, and holds no Task yet (`held` is last tick's), while the
            // stock it paid from sits between the hold and the reserve. A
            // fighter holding no Task is the incumbent's, read at the hold:
            // the Win stands, and the newborn is matched to the outpost.
            test
                "the birth tick, the stock in the hold band: the Win stands and the newborn is matched" {
                let newborn = "guard-991-Spawn2"

                let between =
                    (Tuning.defaults.CasterReserveHold + Tuning.defaults.CasterReserve) / 2

                let world =
                    let world = lendingWorld (harassers ViewFixtures.outpost) []

                    { world with
                        Rooms =
                            world.Rooms
                            |> Map.change
                                ViewFixtures.child
                                (Option.map (ViewFixtures.withStock between))
                    }

                Expect.equal
                    (Facts.answer (List.head (judgedAll lenders world)) ViewFixtures.outpost)
                    Answer.Lose
                    "the premise: in the band, with no body of its, the child lends nothing"

                let born = guardAt ViewFixtures.child newborn 5 world

                let verdict =
                    Facts.verdictOf (List.head (judgedAll lenders born)) ViewFixtures.outpost

                Expect.equal
                    (verdict.Answer, verdict.Lent |> Option.map fst)
                    (Answer.Win, Some ViewFixtures.child)
                    "its newborn holding no Task, the room stays lent to it"

                Expect.equal
                    (Map.tryFind newborn (matchedAll Map.empty lenders born))
                    (Some(taskId (Guard ViewFixtures.outpost)))
                    "and the newborn is matched to the outpost's Guard"
            }
        ]
