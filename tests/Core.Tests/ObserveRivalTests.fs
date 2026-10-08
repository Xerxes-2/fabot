/// The rival dossier's fold (`Observe.foldRivals`): what each rival's code
/// was seen to do, off the rooms in sight on a full tick.
module Fabot.Core.Tests.ObserveRivalTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Observe

/// A rival's home, its remote, and a room nobody holds.
let private keep = "W20S20"
let private remote = "W21S20"
let private open' = "W22S20"
let private rival = "Trepidimous"

let private control (reservedBy: string option) : RoomControlInfo =
    {
        Owner = Ownership.Unowned
        Reservation =
            reservedBy
            |> Option.map (fun username ->
                {
                    Holder = ReservationHolder.Enemy
                    TicksToEnd = 3000
                    Username = username
                })
        SafeMode = false
        SafeModeCooldownUntil = 0
        Sign = None
    }

/// A room in sight: its hostiles, the rival's towers by id and tile, its
/// ramparts by tile, its tombstones and its reservation.
let private room hostiles towers ramparts fallen reservedBy : WorldRoom =
    {
        Hostiles = hostiles
        HostileFatigue = Map.empty
        HostileCargo = Map.empty
        RivalRamparts = ramparts
        InvaderCores = []
        Towers = towers
        OurTowers = []
        Fallen = fallen
        Control = control reservedBy
    }

let private quietRoom = room [] [] Map.empty [] None

/// The rival's controller in its keep as seen on `tick`.
let private keepController (tick: int) : RivalController =
    {
        Owner = rival
        Level = 6
        SafeModeUntil = 0
        SafeModeCooldownUntil = 0
        SafeModeAvailable = 1
        UpgradeBlockedUntil = 0
        TicksToDowngrade = 100000
        Seen = tick
    }

let private hostileIn (roomName: string) id owner (body: BodyPart list) : HostileInfo =
    { raider id owner { X = 25; Y = 25 } body with
        Pos = RoomPos.at roomName { X = 25; Y = 25 }
    }

let private ranger name hits : CreepInfo =
    { ours name with
        Hits = { Hits = hits; HitsMax = 1000 }
        Body = Map.ofList [ RangedAttack, 2; Move, 2 ]
    }

let private healer name hits : CreepInfo =
    { ours name with
        Hits = { Hits = hits; HitsMax = 1000 }
        Body = Map.ofList [ Heal, 4; Move, 4 ]
    }

let private sightAt (tick: int) : RivalSight =
    {
        Tick = tick
        Rooms = Map.empty
        Bodies = Map.empty
        Homes = Map.empty
        Controllers = Map.ofList [ keep, keepController tick ]
        TowerEnergy = Map.empty
        Shots = Map.empty
        Hurt = Map.empty
        Struck = Map.empty
    }

let private foldAll (sights: RivalSight list) =
    (Rivals.empty, sights)
    ||> List.fold (fun rivals sight -> foldRivals sight rivals)

let private dossierOf (rivals: Rivals) = Map.find rival rivals.Dossiers

let private keepRow (rivals: Rivals) = Map.find keep (dossierOf rivals).Rooms

[<Tests>]
let towerTests =
    testList
        "rival dossier: towers"
        [
            test "a shot is read against our bodies in the room, on each question they differ on" {
                // The healer stands nearer the tower, 900 now after a creep's
                // 100; the ranger stands far off, 400 after the shot's 300 —
                // the weaker before the tick's attacks too, 700 to 1,000.
                let sight =
                    { sightAt 100 with
                        Hurt = Map.ofList [ "r1", 300; "h1", 100 ]
                        Rooms =
                            Map.ofList
                                [
                                    keep,
                                    room [] [ "tower-1", { X = 25; Y = 25 } ] Map.empty [] None
                                ]
                        Bodies =
                            Map.ofList
                                [
                                    keep,
                                    [
                                        healer "h1" 900, { X = 24; Y = 24 }
                                        ranger "r1" 400, { X = 40; Y = 40 }
                                    ]
                                ]
                        Shots = Map.ofList [ keep, [ { Tower = "tower-1"; Target = "r1" } ] ]
                    }

                let aim = (dossierOf (foldAll [ sight ])).Aim

                Expect.equal aim.Shots { Count = 1; Last = 100 } "one shot"
                Expect.equal aim.Nearest { Open = 1; Hit = 0 } "not at the nearest"

                Expect.equal
                    aim.Weakest
                    { Open = 1; Hit = 1 }
                    "at the weakest, its hits before the shot"

                Expect.equal aim.Healer { Open = 1; Hit = 0 } "not at the healer"
                Expect.equal aim.Attacker { Open = 1; Hit = 1 } "at the attacker"
                Expect.equal aim.Edge Aim.zero "neither stood on the edge: no pick to read"
                Expect.equal aim.Alone 0 "a pick between two"
            }

            test "a shot at the one body of ours there is counted alone and read on nothing" {
                let sight =
                    { sightAt 100 with
                        Rooms =
                            Map.ofList
                                [
                                    keep,
                                    room [] [ "tower-1", { X = 25; Y = 25 } ] Map.empty [] None
                                ]
                        Bodies = Map.ofList [ keep, [ ranger "r1" 400, { X = 40; Y = 1 } ] ]
                        Shots = Map.ofList [ keep, [ { Tower = "tower-1"; Target = "r1" } ] ]
                    }

                let aim = (dossierOf (foldAll [ sight ])).Aim
                Expect.equal (aim.Shots.Count, aim.Alone) (1, 1) "one shot, alone"
                Expect.equal aim.Edge Aim.zero "and nothing read off it"
            }

            test "the weakest is read before every attack of the tick, not before the shot alone" {
                // A creep's 600 took the healer from 1,100 to 500; the tower's
                // 300 took the ranger from 900 to 600. Read after the healer's
                // damage, the healer would be the weaker.
                let sight =
                    { sightAt 100 with
                        Hurt = Map.ofList [ "r1", 300; "h1", 600 ]
                        Rooms =
                            Map.ofList
                                [
                                    keep,
                                    room [] [ "tower-1", { X = 25; Y = 25 } ] Map.empty [] None
                                ]
                        Bodies =
                            Map.ofList
                                [
                                    keep,
                                    [
                                        { healer "h1" 500 with
                                            Hits = { Hits = 500; HitsMax = 1100 }
                                        },
                                        { X = 24; Y = 24 }
                                        ranger "r1" 600, { X = 40; Y = 40 }
                                    ]
                                ]
                        Shots = Map.ofList [ keep, [ { Tower = "tower-1"; Target = "r1" } ] ]
                    }

                Expect.equal
                    (dossierOf (foldAll [ sight ])).Aim.Weakest
                    { Open = 1; Hit = 1 }
                    "the ranger was the weaker before the tick"
            }

            test "a tower reading older than a body's life is no baseline for a refill" {
                let reading tick energy =
                    { sightAt tick with
                        Rooms =
                            Map.ofList
                                [
                                    keep,
                                    room [] [ "tower-1", { X = 25; Y = 25 } ] Map.empty [] None
                                ]
                        TowerEnergy = Map.ofList [ "tower-1", energy ]
                    }

                let elsewhere tick =
                    { sightAt tick with
                        Rooms = Map.ofList [ open', quietRoom ]
                    }

                let rivals =
                    foldAll
                        [
                            reading 10 100
                            elsewhere (12 + Engine.creepLifetime)
                            reading (20 + Engine.creepLifetime) 900
                        ]

                Expect.equal (keepRow rivals).Refills Tally.zero "no refill read across the gap"
            }

            test
                "tower energy: the lowest one tower held, the last sum, and the readings that found it fuller" {
                let reading tick energy =
                    { sightAt tick with
                        Rooms =
                            Map.ofList
                                [
                                    keep,
                                    room
                                        []
                                        [
                                            "tower-1", { X = 25; Y = 25 }
                                            "tower-2", { X = 26; Y = 25 }
                                        ]
                                        Map.empty
                                        []
                                        None
                                ]
                        TowerEnergy = Map.ofList [ "tower-1", energy; "tower-2", 1000 ]
                    }

                let row = keepRow (foldAll [ reading 10 500; reading 20 300; reading 30 900 ])

                Expect.equal
                    (row.TowerReads, row.TowerLow, row.TowerLast)
                    (3, 300, 1900)
                    "three readings, 300 the lowest, 1,900 the last sum"

                Expect.equal
                    row.Refills
                    { Count = 1; Last = 30 }
                    "one refill, the reading that rose"
            }
        ]

[<Tests>]
let rampartTests =
    testList
        "rival dossier: ramparts"
        [
            test
                "hits gained per rampart-tick, at peace and under fire, and whether the lowest gained first" {
                let a = { X = 10; Y = 10 }
                let b = { X = 11; Y = 10 }

                let reading tick (hits: int * int) armed =
                    { sightAt tick with
                        Rooms =
                            Map.ofList
                                [
                                    keep,
                                    room [] [] (Map.ofList [ a, fst hits; b, snd hits ]) [] None
                                ]
                        Bodies =
                            if armed then
                                Map.ofList [ keep, [ ranger "r1" 1000, { X = 30; Y = 30 } ] ]
                            else
                                Map.empty
                    }

                let row =
                    keepRow (
                        foldAll
                            [
                                reading 10 (1000, 5000) false
                                reading 20 (2000, 5000) false
                                reading 25 (2000, 6500) true
                            ]
                    )

                Expect.equal
                    (row.PeaceGain, row.PeaceTicks)
                    (1000.0, 20.0)
                    "1,000 over two ramparts' ten ticks"

                Expect.equal
                    (row.FireGain, row.FireTicks)
                    (1500.0, 10.0)
                    "1,500 over two ramparts' five ticks"

                Expect.equal
                    row.LowestFirst
                    { Open = 2; Hit = 1 }
                    "the lowest first at peace, the highest under fire"
            }
        ]

[<Tests>]
let safeModeTests =
    testList
        "rival dossier: safe mode"
        [
            test
                "a safe mode is recorded once, at the tick it was raised, beside our visit and what we struck" {
                let armedThere tick =
                    { sightAt tick with
                        Rooms = Map.ofList [ keep, quietRoom ]
                        Bodies = Map.ofList [ keep, [ ranger "r1" 1000, { X = 30; Y = 30 } ] ]
                    }

                let struck =
                    { armedThere 95 with
                        Struck = Map.ofList [ keep, [ "spawn" ] ]
                    }

                // Raised on 100 (its end less 20,000), noticed on 110.
                let raised tick =
                    { armedThere tick with
                        Controllers =
                            Map.ofList
                                [
                                    keep,
                                    { keepController tick with
                                        SafeModeUntil = 20_100
                                        SafeModeCooldownUntil = 70_100
                                    }
                                ]
                    }

                let dossier = dossierOf (foldAll [ armedThere 80; struck; raised 110; raised 111 ])

                Expect.equal
                    dossier.SafeModes
                    { Count = 1; Last = 100 }
                    "once, on the tick it was raised"

                Expect.equal dossier.CooldownUntil 70_100 "its cooldown seen"

                Expect.equal
                    dossier.Activations
                    [
                        {
                            Room = keep
                            At = 100
                            Noticed = 110
                            Level = 6
                            Ours = 1
                            OursArmed = 1
                            Since = Some 20
                            Struck = [ "spawn", 5 ]
                        }
                    ]
                    "our armed visit began 20 ticks before it, the spawn struck 5 before"
            }
        ]

[<Tests>]
let responseTests =
    testList
        "rival dossier: defence and remotes"
        [
            test "the ticks to its first armed body after ours, and its worker walking off" {
                let miner = hostileIn remote "m1" rival [ Work; Work; Carry; Move ]
                let guard = hostileIn remote "g1" rival [ Attack; Attack; Move; Move ]

                let at tick hostiles armed =
                    { sightAt tick with
                        Rooms = Map.ofList [ remote, room hostiles [] Map.empty [] (Some rival) ]
                        Bodies =
                            if armed then
                                Map.ofList [ remote, [ ranger "r1" 1000, { X = 30; Y = 30 } ] ]
                            else
                                Map.empty
                    }

                let rivals =
                    foldAll
                        [
                            at 90 [ miner ] false
                            at 100 [ miner ] true
                            at 112 [ guard ] true
                            at 120 [] false
                        ]

                let dossier = dossierOf rivals
                let row = Map.find remote dossier.Rooms

                Expect.equal row.Response.Episodes 1 "one answer"
                Expect.equal row.Response.Sum 12 "twelve ticks after our arrival"
                Expect.equal row.Unanswered Tally.zero "answered"
                Expect.equal dossier.Fled { Count = 1; Last = 112 } "the miner walked off"

                Expect.equal
                    (dossier.Stayed, dossier.Fell)
                    (Tally.zero, Tally.zero)
                    "and neither stayed nor fell"

                Expect.equal dossier.Shapes (Map.ofList [ "2A2M", 1 ]) "the guard's shape"
            }

            test "a visit it never answers, its worker staying put" {
                let miner = hostileIn remote "m1" rival [ Work; Work; Carry; Move ]

                let at tick armed =
                    { sightAt tick with
                        Rooms = Map.ofList [ remote, room [ miner ] [] Map.empty [] (Some rival) ]
                        Bodies =
                            if armed then
                                Map.ofList [ remote, [ ranger "r1" 1000, { X = 30; Y = 30 } ] ]
                            else
                                Map.empty
                    }

                let dossier = dossierOf (foldAll [ at 100 true; at 150 true; at 160 false ])

                Expect.equal
                    (Map.find remote dossier.Rooms).Unanswered
                    { Count = 1; Last = 160 }
                    "unanswered"

                Expect.equal dossier.Stayed { Count = 1; Last = 160 } "the miner stayed"
            }

            test "a worker dying of age during our visit neither fell nor fled" {
                let miner =
                    { hostileIn remote "m1" rival [ Work; Carry; Move ] with
                        TicksToLive = 10
                    }

                let at tick hostiles fallen =
                    { sightAt tick with
                        Rooms =
                            Map.ofList [ remote, room hostiles [] Map.empty fallen (Some rival) ]
                        Bodies = Map.ofList [ remote, [ ranger "r1" 1000, { X = 30; Y = 30 } ] ]
                    }

                let dossier = dossierOf (foldAll [ at 100 [ miner ] []; at 111 [] [ "m1" ] ])

                Expect.equal
                    (dossier.Fell, dossier.Fled, dossier.Stayed)
                    (Tally.zero, Tally.zero, Tally.zero)
                    "its life ran out on 110"
            }

            test
                "a remote unworked after its worker falls, and its reservation re-taken after it lapses" {
                let miner =
                    { hostileIn remote "m1" rival [ Work; Carry; Move ] with
                        TicksToLive = 1000
                    }

                let next = hostileIn remote "m2" rival [ Work; Carry; Move ]

                let rivals =
                    foldAll
                        [
                            { sightAt 1 with
                                Rooms =
                                    Map.ofList
                                        [ remote, room [ miner ] [] Map.empty [] (Some rival) ]
                            }
                            { sightAt 5 with
                                Rooms = Map.ofList [ remote, room [] [] Map.empty [ "m1" ] None ]
                            }
                            { sightAt 50 with
                                Rooms =
                                    Map.ofList
                                        [ remote, room [ next ] [] Map.empty [] (Some rival) ]
                            }
                        ]

                let row = Map.find remote (dossierOf rivals).Rooms

                Expect.equal (row.Unworked.Episodes, row.Unworked.Sum) (1, 45) "45 ticks unworked"

                Expect.equal
                    (row.Rereserved.Episodes, row.Rereserved.Sum)
                    (1, 45)
                    "45 ticks unreserved"

                Expect.equal
                    (row.KilledAt, row.LapsedAt, row.Reserving)
                    (0, 0, true)
                    "and both closed"
            }

            test "an armed body leaving the room it holds for one it does not is a sortie" {
                let guard id roomName =
                    hostileIn roomName id rival [ RangedAttack; Heal; Move; Move ]

                let dossier =
                    dossierOf (
                        foldAll
                            [
                                { sightAt 1 with
                                    Rooms =
                                        Map.ofList
                                            [ keep, room [ guard "g1" keep ] [] Map.empty [] None ]
                                }
                                { sightAt 2 with
                                    Rooms =
                                        Map.ofList
                                            [
                                                open',
                                                room [ guard "g1" open' ] [] Map.empty [] None
                                            ]
                                }
                            ]
                    )

                Expect.equal dossier.Armed { Count = 1; Last = 1 } "counted once"
                Expect.equal dossier.Crossings { Count = 1; Last = 2 } "a crossing"
                Expect.equal dossier.Sorties { Count = 1; Last = 2 } "out of its keep"
            }

            test
                "its armed bodies in our homes by their level and in our outposts; an ally's and the NPCs' are no rival's" {
                let home = "W1N1"
                let outpost = "W1N2"

                let ourRoom =
                    { quietRoom with
                        Control =
                            { quietRoom.Control with
                                Reservation =
                                    Some
                                        {
                                            Holder = ReservationHolder.Ours
                                            TicksToEnd = 4000
                                            Username = "us"
                                        }
                            }
                    }

                let rivals =
                    foldAll
                        [
                            { sightAt 1 with
                                Rooms =
                                    Map.ofList
                                        [
                                            home,
                                            { quietRoom with
                                                Hostiles =
                                                    [
                                                        hostileIn home "a1" rival [ Attack; Move ]
                                                        hostileIn
                                                            home
                                                            "i1"
                                                            "Invader"
                                                            [ Attack; Move ]
                                                    ]
                                            }
                                            outpost,
                                            { ourRoom with
                                                Hostiles =
                                                    [
                                                        hostileIn
                                                            outpost
                                                            "a2"
                                                            rival
                                                            [ RangedAttack; Move ]
                                                    ]
                                            }
                                        ]
                                Homes = Map.ofList [ home, 4 ]
                            }
                        ]

                let dossier = dossierOf rivals

                Expect.equal
                    dossier.HomeRaiders
                    (Map.ofList [ 4, { Count = 1; Last = 1 } ])
                    "one at an RCL4 home"

                Expect.equal dossier.OutpostRaiders { Count = 1; Last = 1 } "one in an outpost"

                Expect.equal
                    (rivals.Dossiers |> Map.keys |> List.ofSeq)
                    [ rival ]
                    "the Invader has no dossier"
            }

            test "the dossier keeps the latest rivals and each rival's latest rooms" {
                let rivals =
                    foldAll
                        [
                            for tick in 1 .. capRivals + 6 ->
                                { sightAt tick with
                                    Controllers = Map.empty
                                    Rooms =
                                        Map.ofList
                                            [
                                                $"W{tick}N9",
                                                { quietRoom with
                                                    Hostiles =
                                                        [
                                                            hostileIn
                                                                $"W{tick}N9"
                                                                $"m{tick}"
                                                                $"p{tick}"
                                                                [ Work; Move ]
                                                        ]
                                                }
                                            ]
                                }
                        ]

                Expect.equal (Map.count rivals.Dossiers) capRivals "capped"
                Expect.isFalse (Map.containsKey "p1" rivals.Dossiers) "the oldest dropped"

                Expect.isTrue
                    (Map.containsKey $"p{capRivals + 6}" rivals.Dossiers)
                    "the latest kept"
            }
        ]
