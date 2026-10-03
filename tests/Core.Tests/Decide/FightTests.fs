/// The fight: a [[squad]] holding one resident room's `Fight` — when the
/// Fight is pooled and how long it stays, the rally ground and the launch,
/// which bodies the Fight admits and what the residents do meanwhile, and the
/// squad's targets.
module Fabot.Core.Tests.Decide.FightTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

let private brawler name = creepWith name 0 0 brawlerPattern.Block

let private medic name = creepWith name 0 0 medicPattern.Block

/// A resident ranger of these blocks; eight is the kiter's guns.
let private ranger name blocks =
    creepWith name 0 0 (List.replicate blocks rangerPattern.Block |> List.concat)

let private home x y = RoomPos.at "W1N1" { X = x; Y = y }
let private child x y = RoomPos.at "W1N2" { X = x; Y = y }

/// One lone melee body of this many ATTACK, a MOVE each, in the child's home.
/// Twelve: the residents' seven blocks lose it, an eight-block ranger and the
/// duo win it.
let private meleeIn n =
    [
        { hostileIn "W1N2" { X = 26; Y = 40 } (List.replicate n Move @ List.replicate n Attack) with
            Id = "melee-1"
            Owner = "Trepidimous"
        }
    ]

let private fightsIn tasks =
    tasks
    |> List.filter (function
        | Fight _ -> true
        | _ -> false)

let private guardsIn tasks =
    tasks
    |> List.filter (function
        | Guard _ -> true
        | _ -> false)

/// The held-task facts of a colony whose living creeps hold these Tasks.
let private heldOn (colony: ColonyView) (holding: (string * Task) list) : HeldTaskFacts =
    heldTaskFacts colony (holding |> List.map (fun (name, task) -> name, taskId task) |> Map.ofList)

/// The tick's Threats with each Fight's ground on it, the squad whoever holds
/// it in `holding`.
let private threatsHolding (colony: ColonyView) (held: HeldTaskFacts) =
    threatsOfHeld colony (Atlas.ofView colony) held

/// The same, every name in `holding` holding the child's home's Fight.
let private fightThreats (colony: ColonyView) (holding: string list) =
    threatsHolding colony (heldOn colony (holding |> List.map (fun name -> name, Fight "W1N2")))

/// The pool the Planner lays for a colony whose creeps hold these Tasks.
let private planHolding (colony: ColonyView) (holding: (string * Task) list) =
    let held = heldOn colony holding
    let atlas = Atlas.ofView colony

    Planner.planTasks colony atlas (threatsHolding colony held) held (Planner.outpostFactsOf colony)

let private groundOf (threats: Threats) = Map.find "W1N2" threats.Fight

/// One row's quota as the colony decides it with these Tasks held.
let private quotaHolding (colony: ColonyView) (holding: (string * Task) list) (row: string) =
    let assignments =
        holding |> List.map (fun (name, task) -> name, taskId task) |> Map.ofList

    (decide colony assignments Set.empty None).Quotas.Rows
    |> List.tryFind (fun r -> r.Row = row)
    |> Option.map (fun r -> r.Quota)

/// `fightingMother` at a tick far enough on to carry a raid record, its raid
/// still seen the tick before.
let private fightingMotherAt time raid bodies =
    let colony = fightingMother raid bodies

    { colony with
        Time = time
        Fought = colony.Fought |> Map.map (fun _ latch -> { latch with Seen = time - 1 })
    }

/// The child's home's fight record: its raid last seen on this tick, and the
/// squad it latched, if any.
let private recordAt squad seen =
    Map.ofList [ "W1N2", { Seen = seen; Squad = squad } ]

/// The same, the duo latched.
let private foughtAt seen = recordAt (Some "duo") seen

/// Each role's cap on the child's home's Fight, in `SquadRole.all`'s order,
/// as the pool lays it for a colony whose creeps hold these Tasks.
let private fightCaps (colony: ColonyView) (holding: (string * Task) list) =
    let held = heldOn colony holding
    let atlas = Atlas.ofView colony
    let threats = threatsHolding colony held

    planPool colony atlas threats (planHolding colony holding)
    |> List.tryFind (fun entry -> entry.Task = Fight "W1N2")
    |> Option.map (fun entry ->
        SquadRole.all
        |> List.map (fun role -> Capacity.capOf (CapScope.Role role) entry.Capacity))

/// The three squad rows' quotas, brawler, medic and kiter.
let private squadQuotas (colony: ColonyView) =
    [ "brawler"; "medic"; "kiter" ]
    |> List.map (quotaHolding colony [])
    |> List.map (Option.defaultValue -1)

[<Tests>]
let pooledTests =
    testList
        "when a Fight is pooled"
        [
            test "a raided home no resident wins and the duo does pools a Fight beside its Guard" {
                let mother = fightingMother (w17s25RaidIn "W1N2") []
                let tasks = planHolding mother []

                Expect.equal (fightsIn tasks) [ Fight "W1N2" ] "one Fight, under the room's name"
                Expect.equal (guardsIn tasks) [ Guard "W1N2" ] "and the residents' Guard beside it"

                let atlas = Atlas.ofView mother
                let threats = threatsHolding mother (heldOn mother [])

                let capacity =
                    planPool mother atlas threats tasks
                    |> List.find (fun entry -> entry.Task = Fight "W1N2")
                    |> fun entry -> entry.Capacity

                Expect.equal
                    (SquadRole.all
                     |> List.map (fun role -> Capacity.capOf (CapScope.Role role) capacity))
                    [ Some 1; Some 0; Some 0; Some 1; Some 0 ]
                    "the cheapest winning squad is the duo: a brawler's slot and a medic's"
            }

            test "no catalogue squad wins, and no Fight is pooled" {
                // Nine melee bodies: a field no squad of ours prices inside
                // its ten bodies, so every entry loses.
                let horde =
                    [
                        for i in 1..9 ->
                            { hostileIn
                                  "W1N2"
                                  { X = 20 + i; Y = 30 }
                                  (List.replicate 17 Move @ List.replicate 17 Attack) with
                                Id = $"melee-{i}"
                            }
                    ]

                let tasks = planHolding (fightingMother horde []) []

                Expect.isEmpty (fightsIn tasks) "no Fight"
                Expect.equal (guardsIn tasks) [ Guard "W1N2" ] "and the Guard stands as it did"
            }

            test "a bank that holds no squad's body pools no Fight" {
                let poorer =
                    { fightingMother (w17s25RaidIn "W1N2") [] with
                        Bank = bank 5_000 5_000
                    }

                Expect.isEmpty (fightsIn (planHolding poorer [])) "the medic is 5,400"
            }

            test "the Planner pools the Fights the held facts carry, and reads no raid of its own" {
                let quiet = fightingMother [] []

                let held =
                    { HeldTaskFacts.empty with
                        Fights = Map.ofList [ "W1N2", List.head squadCatalogue ]
                    }

                Expect.equal
                    (Planner.planTasks
                        quiet
                        (Atlas.ofView quiet)
                        noThreats
                        held
                        (Planner.outpostFactsOf quiet)
                     |> fightsIn)
                    [ Fight "W1N2" ]
                    "one Fight, priced once in the held facts"
            }

            test
                "a raid seen on one tick pools nothing: the Fight waits for a second inside FightConfirmTicks" {
                let confirm = Tuning.defaults.FightConfirmTicks
                let time = 10_000

                let raided record =
                    { fightingMotherAt time (w17s25RaidIn "W1N2") [] with
                        Fought = record
                    }

                Expect.isEmpty (fightsIn (planHolding (raided Map.empty) [])) "a drive-by: no Fight"

                Expect.equal
                    (fightsIn (planHolding (raided (recordAt None (time - 1))) []))
                    [ Fight "W1N2" ]
                    "seen the tick before as well, the Fight"

                Expect.equal
                    (fightsIn (planHolding (raided (recordAt None (time - confirm))) []))
                    [ Fight "W1N2" ]
                    "and seen as far back as FightConfirmTicks"

                Expect.isEmpty
                    (fightsIn (planHolding (raided (recordAt None (time - confirm - 1))) []))
                    "but not a tick further"

                Expect.isEmpty
                    (fightsIn (
                        planHolding
                            { raided (recordAt None (time - 1)) with
                                Hostiles = []
                            }
                            []
                    ))
                    "and a record that latched no squad holds nothing once the raid is gone"
            }

            test
                "the latched squad is the Fight's whatever the raid becomes: a shrunk raid keeps three kiters" {
                let time = 10_000

                // Twelve ATTACK: priced afresh, the duo wins it.
                let shrunk squad =
                    { fightingMotherAt time (meleeIn 12) [] with
                        Fought = recordAt squad (time - 1)
                    }

                Expect.equal
                    (fightCaps (shrunk None) [])
                    (Some [ Some 1; Some 0; Some 0; Some 1; Some 0 ])
                    "the premise: priced afresh, the duo"

                Expect.equal
                    (fightCaps (shrunk (Some "3×kiter")) [])
                    (Some [ Some 0; Some 0; Some 0; Some 0; Some 3 ])
                    "latched, the three kiters' caps"

                Expect.equal
                    (squadQuotas (shrunk (Some "3×kiter")))
                    [ 0; 0; 3 ]
                    "and the rows cast the three kiters"

                Expect.equal
                    (fightCaps
                        { shrunk (Some "3×kiter") with
                            Hostiles = []
                        }
                        [])
                    (Some [ Some 0; Some 0; Some 0; Some 0; Some 3 ])
                    "and keep their caps while the raid steps out"
            }
        ]

[<Tests>]
let holdTests =
    testList
        "how long a Fight stays pooled"
        [
            test "a squad holding the Fight keeps it down to the raid's tapper" {
                let tapper =
                    w17s25RaidIn "W1N2" |> List.filter (fun hostile -> hostile.Id = "Rune908")

                let mother = fightingMother tapper [ brawler "brawler-1", child 25 41 ]

                Expect.isEmpty
                    (fightsIn (planHolding mother []))
                    "a tapper alone is the residents' to shoot"

                Expect.equal
                    (fightsIn (planHolding mother [ "brawler-1", Fight "W1N2" ]))
                    [ Fight "W1N2" ]
                    "but not mid-fight: the brawler holding it finishes it"
            }

            test "the raid record holds the Fight for FightHoldTicks after the raid steps out" {
                let tuning = Tuning.defaults
                let time = 10_000

                let seen ago =
                    { fightingMotherAt time [] [] with
                        Fought = foughtAt (time - ago)
                    }

                Expect.equal
                    (fightsIn (planHolding (seen tuning.FightHoldTicks) []))
                    [ Fight "W1N2" ]
                    "the raid gone, the Fight is held while the record is fresh"

                Expect.isEmpty
                    (fightsIn (planHolding (seen (tuning.FightHoldTicks + 1)) []))
                    "and dropped a tick after"

                Expect.isEmpty
                    (fightsIn (
                        planHolding
                            (fightingMotherAt time [] [ brawler "brawler-1", child 25 41 ])
                            [ "brawler-1", Fight "W1N2" ]
                    ))
                    "a holder alone, the room empty and no record, holds nothing"
            }

            test "a room whose Fight dropped inside FightHoldTicks casts no second squad" {
                let tuning = Tuning.defaults
                let time = 10_000
                let raided = fightingMotherAt time (w17s25RaidIn "W1N2") []

                let cooling =
                    { raided with
                        Fought = foughtAt (time - tuning.FightHoldTicks - 50)
                    }

                let quotas colony =
                    [ "brawler"; "medic"; "kiter" ]
                    |> List.map (quotaHolding colony [])
                    |> List.map (Option.defaultValue -1)

                Expect.equal (quotas raided) [ 1; 1; 0 ] "the premise: a fresh raid casts the duo"

                Expect.equal
                    (fightsIn (planHolding cooling []))
                    [ Fight "W1N2" ]
                    "the raid back, the Fight is pooled"

                Expect.equal
                    (quotas cooling)
                    [ 0; 0; 0 ]
                    "but nothing is cast for it until the bar runs out"

                Expect.equal
                    (quotas
                        { cooling with
                            Fought = foughtAt (time - 2 * tuning.FightHoldTicks)
                        })
                    [ 1; 1; 0 ]
                    "and once it has, the duo again"
            }

            test "a brawler stranded after its Fight drops walks home, there being no recycle path" {
                let colony = fightingMother [] [ brawler "brawler-1", child 25 41 ]

                let moves = (decideOn colony).Intents |> moveIntentsFor "brawler-1"

                Expect.isTrue
                    (moves
                     |> List.exists (function
                         | MoveCreep(_, (Bottom | BottomLeft | BottomRight)) -> true
                         | _ -> false))
                    $"it steps toward the border home lies behind: {moves}"
            }
        ]

[<Tests>]
let rallyTests =
    testList
        "the rally ground"
        [
            test
                "an incomplete squad's ground is the rally ground, beside the crossing one room short" {
                let mother = fightingMother (w17s25RaidIn "W1N2") [ brawler "brawler-1", home 10 8 ]

                let ground =
                    Threats.fightGroundIn (fightThreats mother [ "brawler-1" ]) "W1N2" "brawler-1"

                Expect.isNonEmpty ground "the premise: somewhere to muster"

                Expect.isTrue
                    (ground |> Set.forall (fun tile -> tile.Room = "W1N1"))
                    "all of it in the last room before the raid, the mother's own here"

                Expect.isTrue
                    (ground |> Set.forall (fun tile -> tile.Y >= 2 && tile.Y <= 3))
                    "clear of the exit and the tile inside it, within three of the crossing"
            }

            test
                "the rally room steps back past a Source Keeper's room and a rival's, to home at the last" {
                Expect.equal
                    (rallyHop Set.empty [ "W15S28"; "W15S27"; "W15S26"; "W15S25" ])
                    (Some("W15S27", "W15S26"))
                    "W15S26 is a keeper room: the squad waits short of it"

                Expect.equal
                    (rallyHop (Set.singleton "W15S27") [ "W15S28"; "W15S27"; "W15S26"; "W15S25" ])
                    (Some("W15S28", "W15S27"))
                    "and short of a rival's, at home, by the exit toward the target"

                Expect.equal
                    (rallyHop Set.empty [ "W15S28"; "W15S27" ])
                    (Some("W15S28", "W15S27"))
                    "beside the raid's own room where nothing stands between"

                Expect.isNone
                    (rallyHop Set.empty [ "W15S28" ])
                    "and a chain of one room has no crossing"
            }

            test "a squad with no rally ground never launches" {
                let mother =
                    fightingMother
                        (w17s25RaidIn "W1N2")
                        [ brawler "brawler-1", home 10 8; medic "medic-1", home 11 8 ]

                let unjoined =
                    { mother with
                        Spatial =
                            { mother.Spatial with
                                Borders = Map.empty
                            }
                    }

                let ground = groundOf (fightThreats unjoined [ "brawler-1"; "medic-1" ])

                Expect.isEmpty ground.Rally "the premise: no chain reaches the room"
                Expect.isFalse ground.Launched "complete and together, and still mustering"
            }

            test "a Fight with no rally ground casts nothing" {
                let raided = fightingMother (w17s25RaidIn "W1N2") []

                let unjoined =
                    { raided with
                        Spatial =
                            { raided.Spatial with
                                Borders = Map.empty
                            }
                    }

                Expect.equal (squadQuotas raided) [ 1; 1; 0 ] "the premise: joined, the duo"

                Expect.equal
                    (fightsIn (planHolding unjoined []))
                    [ Fight "W1N2" ]
                    "the premise: the Fight is pooled all the same"

                Expect.equal
                    (squadQuotas unjoined)
                    [ 0; 0; 0 ]
                    "but no squad is cast to wait nowhere"
            }
        ]

[<Tests>]
let launchTests =
    testList
        "the launch"
        [
            test
                "a complete squad's ground is the fight: the brawler at the head, the medic behind it" {
                let mother =
                    fightingMother
                        (w17s25RaidIn "W1N2")
                        [ brawler "brawler-1", home 10 8; medic "medic-1", home 11 8 ]

                let threats = fightThreats mother [ "brawler-1"; "medic-1" ]
                let front = Threats.fightGroundIn threats "W1N2" "brawler-1"
                let behind = Threats.fightGroundIn threats "W1N2" "medic-1"

                Expect.isNonEmpty front "the premise: ground at the head"

                // The tapper at 26,42 heads the kill order in a resident room:
                // the claimer first.
                Expect.isTrue
                    (front
                     |> Set.forall (fun tile ->
                         tile.Room = "W1N2" && RoomPos.range tile (child 26 42) = Some 1))
                    "the brawler's: every tile beside the head"

                Expect.isTrue
                    (not (Set.isEmpty behind)
                     && behind |> Set.forall (fun tile -> RoomPos.range tile (home 10 8) = Some 1))
                    "the medic's: beside the brawler, wherever it stands"
            }

            test "two members standing apart off the rally ground are no complete squad, and muster" {
                let mother =
                    fightingMother
                        (w17s25RaidIn "W1N2")
                        [ brawler "brawler-1", home 4 8; medic "medic-1", home 40 8 ]

                let ground =
                    Threats.fightGroundIn
                        (fightThreats mother [ "brawler-1"; "medic-1" ])
                        "W1N2"
                        "medic-1"

                Expect.isTrue
                    (not (Set.isEmpty ground)
                     && ground |> Set.forall (fun tile -> tile.Room = "W1N1"))
                    "both slots held, but the medic is not with the brawler: rally"
            }

            test
                "a recast brawler does not launch alone: the medic on the rally ground, the brawler off it and apart" {
                let raid = w17s25RaidIn "W1N2"

                let rally =
                    (groundOf (
                        fightThreats
                            (fightingMother raid [ brawler "brawler-1", home 10 8 ])
                            [ "brawler-1" ]
                    ))
                        .Rally

                let tile = Set.minElement rally

                let launched brawlerAt =
                    let mother =
                        fightingMother
                            raid
                            [ brawler "brawler-2", brawlerAt; medic "medic-1", tile ]

                    (groundOf (fightThreats mother [ "brawler-2"; "medic-1" ])).Launched

                Expect.isFalse
                    (launched (home (tile.X + 8) 8))
                    "the survivor waits on the rally ground for its new brawler to join it"

                Expect.isTrue
                    (launched (home (tile.X + 1) (tile.Y + 2)))
                    "and goes once it has: off the rally ground, but within two of the medic on it"
            }

            test
                "a launched squad stays launched while every cast stands in the room or at its crossing" {
                let launched at =
                    let mother =
                        fightingMother
                            (w17s25RaidIn "W1N2")
                            [ brawler "brawler-1", at; medic "medic-1", child 40 20 ]

                    (groundOf (fightThreats mother [ "brawler-1"; "medic-1" ])).Launched

                Expect.isFalse
                    (launched (home 10 8))
                    "the premise: the brawler at home and apart from its medic, they muster"

                Expect.isTrue
                    (launched (child 25 39))
                    "both in the room, apart, they are not recalled"

                Expect.isTrue
                    (launched (child 25 47))
                    "nor with the brawler chasing to the room's edge"

                Expect.isTrue
                    (launched (home 25 1))
                    "nor carried back over the border off an exit tile"
            }

            test "a brawler's death sends its medic to the rally ground, never into the melee" {
                let mother = fightingMother (w17s25RaidIn "W1N2") [ medic "medic-1", child 25 38 ]

                let ground = groundOf (fightThreats mother [ "medic-1" ])

                Expect.isFalse ground.Launched "a slot empty, the squad is no longer launched"

                Expect.isTrue
                    (not (Set.isEmpty ground.Behind)
                     && ground.Behind |> Set.forall (fun tile -> tile.Room = "W1N1"))
                    "and the survivor's ground is the rally ground"
            }

            test
                "a launched duo in a room its raid has left holds the room's resident ring, and is not released" {
                let time = 10_000

                let left =
                    { fightingMotherAt
                          time
                          []
                          [ brawler "brawler-1", child 25 30; medic "medic-1", child 25 31 ] with
                        Fought = foughtAt (time - 5)
                    }

                let threats = fightThreats left [ "brawler-1"; "medic-1" ]
                let ground = groundOf threats

                Expect.isTrue ground.Launched "the premise: the squad stays launched"

                let ring = Threats.residentRingIn threats "W1N2"

                Expect.isTrue
                    (ring |> Option.exists (Set.isEmpty >> not))
                    "the premise: the raised home's ring stands"

                Expect.equal (Some ground.Front) ring "the brawler holds the ring"
                Expect.equal (Some ground.Ranged) ring "and a kiter would too"

                let assignments =
                    Map.ofList
                        [ "brawler-1", taskId (Fight "W1N2"); "medic-1", taskId (Fight "W1N2") ]

                Expect.equal
                    ((decide left assignments Set.empty None).Assignments
                     |> Map.filter (fun _ tid -> tid = taskId (Fight "W1N2"))
                     |> Map.keys
                     |> Set.ofSeq)
                    (Set.ofList [ "brawler-1"; "medic-1" ])
                    "and both keep the Fight"
            }
        ]

[<Tests>]
let residentTests =
    testList
        "the residents while a squad fights"
        [
            test
                "before the launch the residents keep their Guard, and the Fight admits the casts alone" {
                let mother =
                    { fightingMother
                          (meleeIn 12)
                          [ ranger "ranger-1" 8, child 20 30; brawler "brawler-1", home 10 8 ] with
                        Borrowed =
                            {
                                Rooms = [ "W1N2" ]
                                Defended = [ "W1N2" ]
                                Garrisoned = []
                            }
                    }

                let holding = [ "ranger-1", Guard "W1N2"; "brawler-1", Fight "W1N2" ]
                let tasks = planHolding mother holding
                let threats = threatsHolding mother (heldOn mother holding)

                Expect.equal
                    (fightsIn tasks)
                    [ Fight "W1N2" ]
                    "the premise: the residents lose, the duo wins"

                Expect.equal (guardsIn tasks) [ Guard "W1N2" ] "the residents' Guard stays pooled"

                Expect.isNone
                    (Threats.fightRoleOf threats "W1N2" "ranger-1")
                    "no resident is pulled into the Fight"

                Expect.equal
                    (Threats.fightRoleOf threats "W1N2" "brawler-1")
                    (Some Brawler)
                    "the cast is"

                Expect.isTrue
                    (Threats.guardGroundIn threats "W1N2"
                     |> Option.exists (fun ground ->
                         not (Set.isEmpty ground)
                         && ground |> Set.forall (fun t -> t.Room = "W1N2")))
                    "and the residents hold ground in the room, never home"

                Expect.equal
                    (quotaHolding mother holding "ranger")
                    (Some(mother.Tuning.RangerResidents + Engine.guardCap))
                    "and the #447 relief is still cast while the squad musters"
            }

            test
                "once it launches the relief is the squad's, and a resident no slot fits holds the kite ground" {
                let mother =
                    { fightingMother
                          (meleeIn 12)
                          [
                              ranger "ranger-1" 7, child 20 30
                              brawler "brawler-1", home 10 8
                              medic "medic-1", home 11 8
                          ] with
                        Borrowed =
                            {
                                Rooms = [ "W1N2" ]
                                Defended = [ "W1N2" ]
                                Garrisoned = []
                            }
                    }

                let holding =
                    [ "ranger-1", Guard "W1N2"; "brawler-1", Fight "W1N2"; "medic-1", Fight "W1N2" ]

                let threats = threatsHolding mother (heldOn mother holding)

                Expect.isTrue (groundOf threats).Launched "the premise: the duo stands together"

                Expect.equal
                    (quotaHolding mother holding "ranger")
                    (Some mother.Tuning.RangerResidents)
                    "no relief beside it"

                Expect.isNone
                    (Threats.fightRoleOf threats "W1N2" "ranger-1")
                    "a seven-block ranger fits no slot"

                Expect.equal
                    (Threats.guardGroundIn threats "W1N2")
                    (Some (groundOf threats).Ranged)
                    "and keeps its Guard on the kite ground"
            }

            test
                "a ranger fills a kiter slot only at the kiter's guns, and only once the squad launches" {
                let duoAndKiter =
                    squadCatalogue |> List.find (fun squad -> squad.Name = "duo+kiter")

                let mother bodies = fightingMother (meleeIn 12) bodies

                let ground bodies holding =
                    let colony = mother bodies

                    let held: HeldTaskFacts =
                        { heldOn colony holding with
                            Fights = Map.ofList [ "W1N2", duoAndKiter ]
                        }

                    groundOf (threatsHolding colony held)

                let residents =
                    [ ranger "ranger-8" 8, child 20 30; ranger "ranger-7" 7, child 21 30 ]

                let guarding = [ "ranger-8", Guard "W1N2"; "ranger-7", Guard "W1N2" ]

                let mustering = ground residents guarding

                Expect.equal
                    mustering.Residents
                    (Map.ofList [ "ranger-8", Kiter ])
                    "the eight-block ranger fits the kiter's slot, the seven-block one none"

                Expect.isFalse
                    (Map.containsKey "ranger-8" mustering.Roles)
                    "but holds its Guard while the squad musters"

                let launched =
                    ground
                        (residents @ [ brawler "brawler-1", home 10 8; medic "medic-1", home 11 8 ])
                        (guarding @ [ "brawler-1", Fight "W1N2"; "medic-1", Fight "W1N2" ])

                Expect.isTrue
                    launched.Launched
                    "the premise: the resident fills the kiter's slot, so the duo goes"

                Expect.equal
                    (Map.tryFind "ranger-8" launched.Roles)
                    (Some Kiter)
                    "and then it is the squad's kiter"

                Expect.isFalse (Map.containsKey "ranger-7" launched.Roles) "the smaller one never"
            }

            test "a ranger elsewhere is never pulled into the Fight" {
                let mother =
                    fightingMother
                        (meleeIn 12)
                        [
                            ranger "ranger-9" 8, home 30 8
                            brawler "brawler-1", child 25 39
                            medic "medic-1", child 25 40
                        ]

                let threats = fightThreats mother [ "brawler-1"; "medic-1" ]

                Expect.isTrue (groundOf threats).Launched "the premise: the squad has launched"

                Expect.isNone
                    (Threats.fightRoleOf threats "W1N2" "ranger-9")
                    "a ranger holding nothing there is no resident"
            }
        ]

[<Tests>]
let roleTests =
    testList
        "a squad member's role"
        [
            test "a cast carries its role by name, and a body without one by its parts" {
                let r8 = (ranger "x" 8).Body

                Expect.equal (squadRoleOf "kiter-1-Spawn1" r8) (Some Kiter) "a kiter cast"

                Expect.equal
                    (squadRoleOf "ranger-1-Spawn1" r8)
                    (Some Kiter)
                    "an eight-block resident fits the slot"

                Expect.isNone
                    (squadRoleOf "ranger-1-Spawn1" (ranger "x" 7).Body)
                    "a smaller one does not"

                Expect.equal
                    (squadRoleOf "medic-1-Spawn1" (partsOf medicPattern.Block))
                    (Some Medic)
                    "a medic cast"

                Expect.isNone
                    (squadRoleOf "guard-1-Spawn1" (partsOf guardPattern.Block))
                    "a guard's HEAL makes it no brawler"
            }
        ]

[<Tests>]
let targetTests =
    testList
        "what a squad shoots"
        [
            test "a raid's healer left alone is a target while the room's Fight is pooled" {
                let time = 10_000

                let healer =
                    w17s25RaidIn "W1N2" |> List.filter (fun hostile -> hostile.Id = "Prism305")

                let colony held =
                    { fightingMotherAt time healer [ brawler "brawler-1", child 26 41 ] with
                        Fought = if held then foughtAt (time - 10) else Map.empty
                    }

                let swings colony =
                    (decide
                        colony
                        (Map.ofList [ "brawler-1", taskId (Fight "W1N2") ])
                        Set.empty
                        None)
                        .Intents
                    |> List.contains (AttackCreep("brawler-1", "Prism305"))

                Expect.isTrue (swings (colony true)) "the brawler beside it swings"

                Expect.isFalse
                    (swings (colony false))
                    "the premise: with no Fight it is no raid's healer"
            }
        ]

// ---- the assault ------------------------------------------------------------

let private sapper name = creepWith name 0 0 sapperPattern.Block

/// The rampart an assault on the child's room breaks, and its tile.
let private breachTile = { X = 20; Y = 30 }

/// `fightingMother` with no raid, sending this squad against the child's
/// room as a rival's (#490), a window open in it (#491): the room a rival's,
/// its rampart on the breach tile a wall to our walk
/// (`ColonyView.assaulting`), these targets standing, and these bodies of
/// hers each on its tile.
let private assaultingBy squad mode (targets: (string * Pos) list option) bodies =
    let colony = fightingMother [] bodies
    let child = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Assaults =
            [
                {
                    Assault =
                        {
                            RoomName = "W1N2"
                            Enemy = "Trepidimous"
                            Breach = [ breachTile ]
                            Squad = squad
                            Mode = mode
                            Targets = []
                            Entry = None
                            Active = true
                        }
                    Targets = targets
                    Taken = false
                    Towers = []
                    SafeMode = false
                    // A provoked safe mode's whole 20,000 ticks ahead.
                    BarredUntil = Some(colony.Time + 20_000)
                }
            ]
        Spatial =
            { colony.Spatial with
                RivalRooms = Set.ofList [ "W1N2" ]
            }
            |> withNeighbour
                "W1N2"
                { child with
                    Obstacles =
                        match targets with
                        | Some((_, tile) :: _) when tile = breachTile ->
                            Set.add breachTile child.Obstacles
                        | _ -> child.Obstacles
                }
    }

/// The breach's rampart still standing.
let private breachStanding = Some [ "rampart-1", breachTile ]

/// The default squad's.
let private assaultingWith mode targets bodies =
    assaultingBy Assault.breachers mode targets bodies

let private assaulting bodies =
    assaultingWith Provoke breachStanding bodies

let private probe name = creepWith name 0 0 probePattern.Block

/// A probe Provoke (#493) on the breach, this one probe on its tile.
let private probing (tile: RoomPos) =
    assaultingBy Assault.probe Provoke breachStanding [ probe "probe-1", tile ]

/// The default squad, each body on one of these tiles.
let private squadAt (tiles: RoomPos list) =
    List.zip [ sapper "sapper-1"; sapper "sapper-2"; medic "medic-1"; medic "medic-2" ] tiles

/// The tick's Threats, every name holding the child's room's Assault.
let private assaultThreats (colony: ColonyView) =
    threatsHolding
        colony
        (heldOn colony (colony.Creeps |> List.map (fun creep -> creep.Name, Assault "W1N2")))

let private assaultGroundOf (threats: Threats) = Map.find "W1N2" threats.Assault

/// Four tiles together, two short of the home's north edge.
let private mustered = [ home 25 5; home 26 5; home 25 6; home 26 6 ]

/// The tiles beside the breach on the near side, where the sappers stand.
let private atBreach = [ child 20 31; child 21 31; child 19 32; child 20 32 ]

let private holdingAssault (colony: ColonyView) =
    colony.Creeps
    |> List.map (fun creep -> creep.Name, taskId (Assault "W1N2"))
    |> Map.ofList

/// The squad on these tiles, safe mode raised in the rival's room and its
/// towers standing on these tiles.
let private provokedWith (towers: Pos list) tiles =
    let colony = assaulting (squadAt tiles)

    { colony with
        Assaults =
            colony.Assaults
            |> List.map (fun facts ->
                { facts with
                    SafeMode = true
                    Towers = towers
                })
    }

/// Whether every tile is the home's ground beside its border with the
/// rival's room: one step out of it.
let private outOfTheRoom (tiles: Set<RoomPos>) =
    not (Set.isEmpty tiles)
    && tiles |> Set.forall (fun tile -> tile.Room = "W1N1" && tile.Y = 1)

[<Tests>]
let assaultTests =
    testList
        "an Assault on a rival's room"
        [
            test
                "a switched-on assault pools one Assault, capped and cast at two sappers and two medics" {
                let colony = assaulting []

                Expect.contains
                    (planHolding colony [])
                    (Assault "W1N2")
                    "one Assault, under the room's name"

                let atlas = Atlas.ofView colony
                let threats = assaultThreats colony

                let capacity =
                    planPool colony atlas threats (planHolding colony [])
                    |> List.find (fun entry -> entry.Task = Assault "W1N2")
                    |> fun entry -> entry.Capacity

                Expect.equal
                    (SquadRole.all
                     |> List.map (fun role -> Capacity.capOf (CapScope.Role role) capacity))
                    [ Some 0; Some 2; Some 0; Some 2; Some 0 ]
                    "two sapper slots and two medic slots"

                Expect.equal
                    ([ "sapper"; "medic"; "brawler"; "kiter" ] |> List.map (quotaHolding colony []))
                    [ Some 2; Some 2; Some 0; Some 0 ]
                    "and the rows cast them"
            }

            test
                "its rally ground is short of the rival's room, on the one chain that may end in it" {
                let colony = assaulting []
                let atlas = Atlas.ofView colony
                let ground = assaultGroundOf (assaultThreats colony)

                Expect.isNone
                    (Atlas.route atlas "W1N1" "W1N2")
                    "no fighter's chain enters a rival's room"

                Expect.equal
                    (Atlas.siegeRoute atlas "W1N1" "W1N2")
                    (Some [ "W1N1"; "W1N2" ])
                    "a siege's ends in it"

                Expect.isNonEmpty ground.Rally "a rally ground"

                Expect.isTrue
                    (ground.Rally |> Set.forall (fun tile -> tile.Room = "W1N1"))
                    "in the last room short of it"
            }

            test
                "the squad waits on the rally ground until it is whole, then launches beside the breach" {
                let three = assaulting (squadAt mustered |> List.take 3)
                let ground = assaultGroundOf (assaultThreats three)

                Expect.isFalse ground.Launched "a medic short: not launched"
                Expect.equal ground.Front ground.Rally "and the sappers wait at rally"

                let whole = assaultGroundOf (assaultThreats (assaulting (squadAt mustered)))

                Expect.isTrue whole.Launched "whole and together: launched"

                Expect.equal whole.Target (Some("rampart-1", child 20 30)) "on the breach's rampart"

                Expect.isTrue
                    (Set.contains (child 20 31) whole.Front
                     && whole.Front
                        |> Set.forall (fun tile -> RoomPos.range tile (child 20 30) = Some 1))
                    $"the sappers' ground beside it: {whole.Front}"
            }

            test
                "a squad walking in single file off the rally ground stays launched; one cast left behind halts it (#491)" {
                let file = [ home 25 20; home 26 20; home 27 20; home 28 20 ]

                Expect.isTrue
                    (assaultGroundOf (assaultThreats (assaulting (squadAt file)))).Launched
                    "each within two of the next: one squad on its way"

                let behind = [ home 25 20; home 26 20; home 27 20; home 33 20 ]

                Expect.isFalse
                    (assaultGroundOf (assaultThreats (assaulting (squadAt behind)))).Launched
                    "a medic five tiles back: not launched"
            }

            test
                "a launched sapper beside the breach dismantles it, and a medic leaves its act to the heal reflex" {
                let colony = assaulting (squadAt atBreach)
                let intents = (decide colony (holdingAssault colony) Set.empty None).Intents

                for name in [ "sapper-1"; "sapper-2" ] do
                    Expect.contains
                        intents
                        (DismantleStructure(name, "rampart-1"))
                        $"{name} dismantles"

                Expect.isFalse
                    (intents
                     |> List.exists (function
                         | DismantleStructure(name, _) -> name.StartsWith "medic"
                         | _ -> false))
                    "no medic does"
            }

            test "the breach down, a Provoke holds it and a Strike walks on to the tower" {
                let fallen mode targets =
                    assaultGroundOf (
                        assaultThreats (assaultingWith mode targets (squadAt atBreach))
                    )

                let provoke = fallen Provoke (Some [])

                Expect.isNone provoke.Target "nothing left for a Provoke"

                Expect.isTrue
                    (Set.contains (child 20 30) provoke.Front)
                    "it holds the breach it made"

                let strike = fallen Strike (Some [ "tower-1", { X = 30; Y = 10 } ])

                Expect.equal
                    strike.Target
                    (Some("tower-1", child 30 10))
                    "a Strike's next is the tower"

                Expect.isTrue
                    (strike.Front
                     |> Set.forall (fun tile -> RoomPos.range tile (child 30 10) = Some 1))
                    "and its sappers' ground beside it"
            }

            test
                "a member under half its hits turns the squad back to rally, which goes again at four fifths" {
                let hurt share =
                    let squad = squadAt atBreach

                    let wounded =
                        squad
                        |> List.map (fun (creep, tile) ->
                            if creep.Name = "sapper-1" then
                                { creep with
                                    Hits =
                                        { creep.Hits with
                                            Hits = creep.Hits.HitsMax * share / 100
                                        }
                                },
                                tile
                            else
                                creep, tile)

                    assaulting wounded

                Expect.isTrue
                    (assaultGroundOf (assaultThreats (hurt 51))).Launched
                    "at half and over, it fights on"

                let back = assaultGroundOf (assaultThreats (hurt 49))

                Expect.isFalse back.Launched "under half, it turns back"

                Expect.isTrue
                    (outOfTheRoom back.Front && back.Behind = back.Front)
                    $"every member out of the room first: {back.Front}"

                let atRally share =
                    let colony = hurt share

                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        List.zip
                                            [ "sapper-1"; "sapper-2"; "medic-1"; "medic-2" ]
                                            (mustered |> List.map RoomPos.pos)
                                        |> Map.ofList
                                })
                            |> withNeighbour
                                "W1N2"
                                { SpatialInfo.layerOf colony.Spatial "W1N2" with
                                    CreepPositions = Map.empty
                                }
                    }

                Expect.isFalse
                    (assaultGroundOf (assaultThreats (atRally 79))).Launched
                    "at rally under four fifths it waits on its medics"

                Expect.isTrue
                    (assaultGroundOf (assaultThreats (atRally 80))).Launched
                    "and goes at four fifths"
            }

            test "safe mode in the rival's room turns the squad back: the bait has worked" {
                let colony = assaulting (squadAt atBreach)

                let moded =
                    { colony with
                        Assaults =
                            colony.Assaults
                            |> List.map (fun facts -> { facts with SafeMode = true })
                    }

                Expect.isTrue
                    (assaultGroundOf (assaultThreats colony)).Launched
                    "the premise: in and fighting"

                let ground = assaultGroundOf (assaultThreats moded)
                Expect.isFalse ground.Launched "safe mode: not launched"
                Expect.isTrue (outOfTheRoom ground.Front) "and out of the room"
            }

            test
                "a squad falling back from inside the rival's room leaves it by the nearest exit, then walks to rally (#492)" {
                let ground = assaultGroundOf (assaultThreats (provokedWith [] atBreach))

                Expect.isTrue
                    (outOfTheRoom ground.Front && ground.Behind = ground.Front)
                    $"sappers and medics one step out of the room: {ground.Front}"

                Expect.isTrue
                    (ground.Front |> Set.forall (fun tile -> abs (tile.X - 20) <= 2))
                    $"straight out, the towerless room priced by the step: {ground.Front}"

                let out =
                    assaultGroundOf (
                        assaultThreats (
                            provokedWith [] [ home 20 1; home 21 1; home 19 2; home 20 2 ]
                        )
                    )

                Expect.equal out.Front out.Rally "every member out: to rally"
                Expect.equal out.Behind out.Rally "medics too"
            }

            test
                "its way out is priced by the towers' fire on every step of the walk: it leaves on the side away from them (#492)" {
                let ground =
                    assaultGroundOf (assaultThreats (provokedWith [ { X = 10; Y = 44 } ] atBreach))

                Expect.isTrue (outOfTheRoom ground.Front) $"out of the room: {ground.Front}"

                Expect.isTrue
                    (ground.Front |> Set.forall (fun tile -> tile.X > 25))
                    $"east, away from the tower in the south-west: {ground.Front}"
            }

            test "an assault projects every room beside its target: the ways out (#492)" {
                let projected = Assault.roomsProjected [ Assault.w18s26 ] "W17S26"

                Expect.equal
                    (Set.ofList projected)
                    (Set.ofList [ "W18S26"; "W18S25"; "W19S26"; "W18S27" ])
                    "the room and the three beside it that are not home"
            }

            test
                "a Provoke whose room is in safe mode stays pooled and out of it, and casts no squad for it (#491)" {
                let colony = assaulting []

                let moded =
                    { colony with
                        Assaults =
                            colony.Assaults
                            |> List.map (fun facts -> { facts with SafeMode = true })
                    }

                Expect.contains (planHolding moded []) (Assault "W1N2") "pooled whenever active"

                Expect.equal
                    ([ "sapper"; "medic" ] |> List.map (quotaHolding moded []))
                    [ Some 0; Some 0 ]
                    "no body bought to wait out the safe mode"
            }

            test
                "a Strike is pooled only while its room cannot raise safe mode for longer than the squad's casts and walk (#491)" {
                let barred until =
                    let colony = assaultingWith Strike breachStanding []

                    { colony with
                        Assaults =
                            colony.Assaults
                            |> List.map (fun facts -> { facts with BarredUntil = until })
                    }

                let pooled colony =
                    planHolding colony [] |> List.contains (Assault "W1N2")

                // Two sappers and two medics in one oven, then the one crossing.
                let lead =
                    Engine.spawnTicksPerPart
                    * (2 * List.length sapperPattern.Block + 2 * List.length medicPattern.Block)
                    + Engine.roomSide

                let now = (barred None).Time

                Expect.isFalse (pooled (barred None)) "it may raise one, or nobody knows: no Strike"

                Expect.isFalse
                    (pooled (barred (Some(now + lead))))
                    "a window that closes before the squad is cast and there: no Strike"

                Expect.equal
                    ([ "sapper"; "medic" ] |> List.map (quotaHolding (barred (Some(now + lead))) []))
                    [ Some 0; Some 0 ]
                    "and nothing cast for it"

                let inside =
                    let colony = assaultingWith Strike breachStanding (squadAt atBreach)

                    { colony with
                        Assaults =
                            colony.Assaults
                            |> List.map (fun facts ->
                                { facts with
                                    BarredUntil = Some(colony.Time + 100)
                                })
                    }

                let launched = assaultThreats inside |> fun threats -> threats.Assault

                Expect.isTrue
                    (launched |> Map.tryFind "W1N2" |> Option.exists (fun ground -> ground.Launched))
                    "launched, with 100 ticks of window left: pooled until it closes"

                let open' = barred (Some(now + lead + 1))
                Expect.isTrue (pooled open') "a window that outlasts them: pooled"

                Expect.equal
                    ([ "sapper"; "medic" ] |> List.map (quotaHolding open' []))
                    [ Some 2; Some 2 ]
                    "and its squad cast"
            }

            test
                "an assault naming the room it enters from is walked in from that room alone (#493)" {
                let entering entry =
                    let colony = assaulting []

                    let colony =
                        { colony with
                            Assaults =
                                colony.Assaults
                                |> List.map (fun facts ->
                                    { facts with
                                        Assault = { facts.Assault with Entry = entry }
                                    })
                        }

                    Atlas.siegeRoute (Atlas.ofView colony) "W1N1" "W1N2"

                Expect.equal (entering (Some "W1N1")) (Some [ "W1N1"; "W1N2" ]) "in from home"

                Expect.isNone
                    (entering (Some "W2N2"))
                    "in from a room the projection holds no walk through: no chain"
            }

            test "a probe Provoke (#493) is capped and cast at one cheap probe and nothing else" {
                let colony = assaultingBy Assault.probe Provoke breachStanding []
                let atlas = Atlas.ofView colony
                let threats = assaultThreats colony

                let capacity =
                    planPool colony atlas threats (planHolding colony [])
                    |> List.find (fun entry -> entry.Task = Assault "W1N2")
                    |> fun entry -> entry.Capacity

                Expect.equal
                    (SquadRole.all
                     |> List.map (fun role -> Capacity.capOf (CapScope.Role role) capacity))
                    [ Some 0; Some 0; Some 1; Some 0; Some 0 ]
                    "one probe slot"

                Expect.equal
                    ([ "probe"; "sapper"; "medic" ] |> List.map (quotaHolding colony []))
                    [ Some 1; Some 0; Some 0 ]
                    "and the probe row casts it"

                Expect.isLessThanOrEqual
                    (bodyCost probePattern.Block)
                    1_000
                    "a body cheap enough to lose"
            }

            test "a lone probe launches off the rally ground with no medic to wait for" {
                Expect.isTrue
                    (assaultGroundOf (assaultThreats (probing (home 25 5)))).Launched
                    "a squad of one, whole"
            }

            test "a launched probe beside the breach dismantles it, and goes on under half its hits" {
                let dismantles (colony: ColonyView) =
                    (decide colony (holdingAssault colony) Set.empty None).Intents
                    |> List.contains (DismantleStructure("probe-1", "rampart-1"))

                let colony = probing (child 20 31)

                Expect.isTrue (dismantles colony) "it dismantles the breach"

                let hurt =
                    { colony with
                        Creeps =
                            colony.Creeps
                            |> List.map (fun creep ->
                                { creep with
                                    Hits =
                                        { creep.Hits with
                                            Hits = creep.Hits.HitsMax / 10
                                        }
                                })
                    }

                Expect.isTrue
                    (assaultGroundOf (assaultThreats hurt)).Launched
                    "a tenth of its hits: no medic to fall back on, so it stays"

                Expect.isTrue (dismantles hurt) "and dismantles until it dies"
            }

            test
                "past the breach a squad naming targets goes for them, and with them down it leaves by its way out and casts no more (#496)" {
                let naming taken targets squad =
                    let colony = assaultingWith Provoke targets squad

                    { colony with
                        Assaults =
                            colony.Assaults
                            |> List.map (fun facts ->
                                { facts with
                                    Assault =
                                        { facts.Assault with
                                            Targets = [ { X = 25; Y = 41 } ]
                                        }
                                    Taken = taken
                                })
                    }

                let going =
                    assaultGroundOf (
                        assaultThreats (
                            naming false (Some [ "link-1", { X = 25; Y = 41 } ]) (squadAt atBreach)
                        )
                    )

                Expect.isTrue going.Launched "the breach down and the link standing: in"
                Expect.equal going.Target (Some("link-1", child 25 41)) "on the link"

                let done' = naming true (Some []) (squadAt atBreach)
                let ground = assaultGroundOf (assaultThreats done')

                Expect.isFalse ground.Launched "the link down: not launched"

                Expect.isTrue
                    (outOfTheRoom ground.Front && ground.Behind = ground.Front)
                    $"every member out of the room: {ground.Front}"

                Expect.isFalse
                    ((decide done' (holdingAssault done') Set.empty None).Intents
                     |> List.exists (function
                         | DismantleStructure _ -> true
                         | _ -> false))
                    "and no dismantle"

                Expect.equal
                    ([ "sapper"; "medic" ] |> List.map (quotaHolding done' []))
                    [ Some 0; Some 0 ]
                    "nothing cast to replace a body lost on the way out"

                let out = naming true None (squadAt mustered)

                Expect.isFalse
                    (planHolding out [] |> List.contains (Assault "W1N2"))
                    "every member out, the room dark: its work is done and it is pooled no more"
            }

            test "safe mode up, a probe in the room takes the way out at once" {
                let colony = probing (child 20 31)

                let moded =
                    { colony with
                        Assaults =
                            colony.Assaults
                            |> List.map (fun facts -> { facts with SafeMode = true })
                    }

                let ground = assaultGroundOf (assaultThreats moded)

                Expect.isFalse ground.Launched "not launched"

                Expect.isTrue
                    (outOfTheRoom ground.Front)
                    $"one step out of the room: {ground.Front}"

                Expect.isFalse
                    ((decide moded (holdingAssault moded) Set.empty None).Intents
                     |> List.exists (function
                         | DismantleStructure _ -> true
                         | _ -> false))
                    "and no dismantle"
            }
        ]
