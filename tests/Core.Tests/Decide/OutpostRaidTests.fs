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
                    Hostiles = hostiles
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
        (fun (previous: World) world -> World.watchExits Tuning.defaults previous.ExitWatches world)
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
                    World.watchExits
                        Tuning.defaults
                        Map.empty
                        (watched 100 [ kiterAt { X = 1; Y = 22 } 1500 ])

                Expect.equal
                    (watchOf seen)
                    (Some(
                        ExitWatch.Seen
                            [
                                {
                                    Id = "h-1"
                                    At = { X = 1; Y = 22 }
                                    Dies = 1600
                                }
                            ]
                    ))
                    "while it stands there the world remembers where, and when it dies"

                let gone = World.watchExits Tuning.defaults seen.ExitWatches (watched 101 [])

                Expect.equal
                    (watchOf gone)
                    (Some(
                        ExitWatch.Held
                            {
                                Run = [ for y in 20..24 -> { X = 0; Y = y } ]
                                Exit = { X = 0; Y = 22 }
                                Until = 101 + Tuning.defaults.ExitHoldTicks
                            }
                    ))
                    "the tick vision finds it gone, the whole west run beside it is held"
            }

            test "the hold ends at ExitHoldTicks, and at the threat's remembered death if sooner" {
                let holdAfter ttl =
                    watchThrough [ watched 100 [ kiterAt { X = 1; Y = 22 } ttl ]; watched 101 [] ]

                let long = holdAfter 1500
                let until = 101 + Tuning.defaults.ExitHoldTicks

                Expect.isSome
                    (World.watchExits Tuning.defaults long.ExitWatches (watched (until - 1) [])
                     |> watchOf)
                    "the hold stands to its last tick"

                Expect.isNone
                    (World.watchExits Tuning.defaults long.ExitWatches (watched until []) |> watchOf)
                    "and ends at ExitHoldTicks"

                Expect.isSome
                    (World.watchExits Tuning.defaults long.ExitWatches (dark 150) |> watchOf)
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
                        Tuning.defaults
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
