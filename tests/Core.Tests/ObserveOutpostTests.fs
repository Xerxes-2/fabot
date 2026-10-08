/// The Raid log's second family: the stand-downs, the ring they trim by a
/// rule of their own, the gate that reads them, the withdrawal with no
/// clock, the reservation somebody else is standing on, and the guard's
/// memory of a room it has gone blind in.
module Fabot.Core.Tests.ObserveOutpostTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Observe

/// Five attackers two guard blocks cannot beat, all one owner's, standing in
/// `room` while the colony has `outpostRoom` declared. Four ATTACK each:
/// three a body lose to the guard's swing struck back on every one of them
/// (#624, `_damage.js`).
let private squadIn room owner =
    { (quiet |> withDeclaredOutpost outpostRoom) with
        Hostiles =
            List.replicate 5 [ Attack; Attack; Attack; Attack; Move; Move; Move; Move ]
            |> List.mapi (raiderIn room)
            |> List.map (fun hostile -> { hostile with Owner = owner })
        Bank = { Available = 0; Capacity = 1_500 }
    }

/// The same five Trepidimous bodies, `raid-i` standing in `rooms[i]`; a
/// room named "" holds nobody that tick.
let private spread (rooms: string list) =
    let squad = squadIn outpostRoom "Trepidimous"

    { squad with
        Hostiles =
            List.zip squad.Hostiles rooms
            |> List.filter (fun (_, room) -> room <> "")
            |> List.map (fun (hostile, room) ->
                { hostile with
                    Pos = { hostile.Pos with Room = room }
                })
    }

[<Tests>]
let outpostTests =
    testList
        "raid fold: outpost episodes"
        [
            // #382. The two cores of one stronghold carry the **same** collapse
            // timer (live, W15S26's `bunker4` and the level-0 core it expanded into
            // W15S27 were both clocked to t655,973), so the level tells them apart.
            test "a stronghold's room is shut and impassable; a level-0 core's is neither" {
                let expiry = 400

                let atLevel level =
                    RaidState.empty |> raidTick 10 (seen [ bunker outpostRoom (Some expiry) level ])

                // #487: live t925,486, five rooms stood down for 47,240 ticks
                // behind level-0 cores that only reserve the controller.
                Expect.isEmpty
                    (standDowns (atLevel 0))
                    "a level-0 expansion core opens no stand-down: a guard kills it instead"

                Expect.equal
                    (shutAt 20 (atLevel 1), impassableAt 20 (atLevel 1))
                    (Set.singleton outpostRoom, Set.singleton outpostRoom)
                    "a level-1 core is a stronghold's, as it was"

                Expect.equal
                    (shutAt 20 (atLevel 4), impassableAt 20 (atLevel 4))
                    (Set.singleton outpostRoom, Set.singleton outpostRoom)
                    "a bunker withholds both, which is the one thing a stand-down does not otherwise say"

                Expect.equal
                    (strongholds (atLevel 4))
                    [ true ]
                    "the row remembers the bunker, so the fact outlives the vision that read it"

                // The defect the first build had: `Basis` says which clock the expiry
                // came off and `sight` overwrites it whenever a later deadline arrives,
                // so keying passability on the basis put four towers back on a
                // crossable route. The flag is sticky instead.
                let thenRaided =
                    atLevel 4 |> raidTick 20 (raid [ raiderIn outpostRoom 1 [ Move; Attack ] ])

                Expect.equal
                    (strongholds thenRaided, impassableAt 30 thenRaided)
                    ([ true ], Set.singleton outpostRoom)
                    "a later deadline moves the basis and never the bunker"

                Expect.isEmpty
                    (impassableAt (expiry + 1) (atLevel 4))
                    "the core's own timer ends it: the room re-links with no rule of its own"
            }

            test "a level-0 core stands down no harassment room, outpost or errand room" {
                // Live t925,486 (#487): W14S26's level-4 stronghold expanded into
                // W17S26 (harassed), W15S27 (an outpost) and W16S27, all on its
                // collapse clock, 47,240 ticks out.
                let harassed = "W17S26"
                let errandRoom = "W16S27"

                let colony cores =
                    { seen cores with
                        HarassCast = Set.singleton harassed
                    }
                    |> withDeclaredOutpost outpostRoom

                let rooms = [ harassed; outpostRoom; errandRoom ]

                let atLevel level =
                    RaidState.empty
                    |> raidTick
                        10
                        (colony (rooms |> List.map (fun room -> bunker room (Some 47_250) level)))

                Expect.isEmpty (shutAt 20 (atLevel 0)) "level 0: every room is worked"

                Expect.equal
                    (shutAt 20 (atLevel 1))
                    (Set.ofList rooms)
                    "level 1: every room stands down to the collapse, as before"
            }

            test
                "any raid stands down a held errand's room, dug for its ore and garrisoned by nobody" {
                // The hold pauses the Reactor (user, 2026-10-07): no row of
                // ours answers a raid there, so the miner walks home.
                let oreRoom = "W16S27"

                let raided =
                    { quiet with
                        OreRooms = Set.singleton oreRoom
                        Hostiles = [ raiderIn oreRoom 1 [ Move; Attack ] ]
                        Bank = { Available = 0; Capacity = 1_500 }
                    }

                Expect.equal
                    (shutAt 101 (RaidState.empty |> raidTick 100 raided))
                    (Set.singleton oreRoom)
                    "one melee body shuts it"

                Expect.isEmpty
                    (shutAt
                        101
                        (RaidState.empty |> raidTick 100 { raided with OreRooms = Set.empty }))
                    "the premise: a room we dig nothing in is not ours to shut"
            }

            test "a raid two guards cannot beat stands the room down to its own life" {
                // #257: a stand-down clocked off an invader *core* alone gave a raid of
                // plain creeps no deadline; W13S29 stayed open through one and in three
                // hundred ticks lost two reservers and a guard. An Invader in a room
                // nobody owns never suicides, so what it has left is the clock.
                //
                // Read only for a raid the guard row's reach cannot beat: shutting a room
                // takes it out of the scan set, so a raid that shut it on sight would
                // hide its own hostiles and buy no guard. Pairwise on the raid's size.
                // A bank that buys a two-block body: the row's reach (#417) is the
                // two blocks this case is priced at.
                let raidIn hostiles =
                    { (quiet |> withDeclaredOutpost outpostRoom) with
                        Hostiles = hostiles |> List.mapi (raiderIn outpostRoom)
                        Bank = { Available = 0; Capacity = 1_500 }
                    }

                let overwhelming =
                    List.replicate 5 [ Attack; Attack; Attack; Attack; Move; Move; Move; Move ]

                Expect.equal
                    (standDowns (RaidState.empty |> raidTick 100 (raidIn overwhelming)))
                    [ outpostRoom, 100, 100, 1600, StandDownBasis.InvaderRaid ]
                    "five attackers beat two blocks, so the room is left for the fifteen hundred they have"

                Expect.isEmpty
                    (standDowns (
                        RaidState.empty
                        |> raidTick
                            100
                            (raidIn
                                [
                                    [
                                        Tough
                                        Tough
                                        Move
                                        Move
                                        Move
                                        Move
                                        Move
                                        RangedAttack
                                        Work
                                        Attack
                                    ]
                                ])
                    ))
                    "and the lone smallMelee two blocks beat opens no stand-down: that room is a fight"
            }

            // #507. Live t951,362: a Trepidimous squad crossed W17S26 on its way to
            // W17S25's ramparts and the outpost stood down for its whole ~1,450-tick
            // life. A player's squad walks on, so a sighting of every body elsewhere
            // reopens the room; absence alone never does.
            test "a player's raid seen in another room after the outpost reopens it that tick" {
                let state =
                    RaidState.empty
                    |> raidTick 100 (squadIn outpostRoom "Trepidimous")
                    |> raidTick 110 (squadIn raidRoom "Trepidimous")

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 100, 110, StandDownBasis.RaidGone ]
                    "the clock ends on the tick the last body is seen at home, well before its window would (#507)"

                Expect.isEmpty (shutAt 111 state) "and the outpost is worked again"
            }

            // Live t951,384-417: the four Trepidimous bodies died at W17S25's
            // ramparts and W17S26 stayed shut on their lives. A tombstone is the
            // body gone, for any owner: an Invader's clock only stood for its life.
            test "a raid seen dead ends its stand-down that tick, an Invader's as a player's" {
                for owner in [ "Trepidimous"; "Invader" ] do
                    let raid = squadIn outpostRoom owner

                    let state =
                        RaidState.empty
                        |> raidTick 100 raid
                        |> raidTick
                            110
                            { raid with
                                Hostiles = []
                                Fallen = raid.Hostiles |> List.map (fun hostile -> hostile.Id)
                            }

                    Expect.equal
                        (standDowns state)
                        [ outpostRoom, 100, 100, 110, StandDownBasis.RaidGone ]
                        $"{owner}: five tombstones are five bodies gone"
            }

            test "a player's raid partly dead and partly seen elsewhere is gone" {
                let state =
                    RaidState.empty
                    |> raidTick 100 (squadIn outpostRoom "Trepidimous")
                    |> raidTick
                        110
                        { squadIn raidRoom "Trepidimous" with
                            Hostiles = (squadIn raidRoom "Trepidimous").Hostiles |> List.take 3
                            Fallen = [ "raid-3"; "raid-4" ]
                        }

                Expect.isEmpty (shutAt 111 state) "three at home and two dead are all five"

                let invaders =
                    RaidState.empty
                    |> raidTick 100 (squadIn outpostRoom "Invader")
                    |> raidTick
                        110
                        { squadIn raidRoom "Invader" with
                            Hostiles = (squadIn raidRoom "Invader").Hostiles |> List.take 1
                            Fallen = [ "raid-1"; "raid-2"; "raid-3"; "raid-4" ]
                        }

                Expect.equal
                    (shutAt 111 invaders)
                    (Set.singleton outpostRoom)
                    "an Invader alive elsewhere is not gone"
            }

            test "an Invader raid seen elsewhere keeps its clock" {
                let state =
                    RaidState.empty
                    |> raidTick 100 (squadIn outpostRoom "Invader")
                    |> raidTick 110 (squadIn raidRoom "Invader")

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 100, 1600, StandDownBasis.InvaderRaid ]
                    "an NPC raid is clocked off its life, wherever it is seen"
            }

            test "a player's raid not seen elsewhere keeps its clock" {
                let trep = squadIn outpostRoom "Trepidimous"

                let quietly =
                    RaidState.empty |> raidTick 100 trep |> raidTick 110 { trep with Hostiles = [] }

                Expect.equal
                    (shutAt 111 quietly)
                    (Set.singleton outpostRoom)
                    "not seeing the raid is no evidence it left"

                let partly =
                    RaidState.empty
                    |> raidTick 100 trep
                    |> raidTick
                        110
                        { trep with
                            Hostiles = (squadIn raidRoom "Trepidimous").Hostiles |> List.take 4
                        }

                Expect.equal
                    (shutAt 111 partly)
                    (Set.singleton outpostRoom)
                    "one body unaccounted for holds it shut"
            }

            test "a player's raid that comes back shuts the outpost again" {
                let state =
                    RaidState.empty
                    |> raidTick 100 (squadIn outpostRoom "Trepidimous")
                    |> raidTick 110 (squadIn raidRoom "Trepidimous")
                    |> raidTick 300 (squadIn outpostRoom "Trepidimous")

                Expect.equal
                    (shutAt 301 state)
                    (Set.singleton outpostRoom)
                    "a return is a fresh sighting"

                Expect.equal
                    (state |> raidTick 310 (squadIn raidRoom "Trepidimous") |> shutAt 311)
                    Set.empty
                    "and leaving again reopens it again"
            }

            test "a squad bouncing on the border flips the outpost once a bounce, not once a tick" {
                let bouncing =
                    [ 100..140 ]
                    |> List.scan
                        (fun state t ->
                            let room = if t % 2 = 0 then outpostRoom else raidRoom
                            state |> raidTick t (squadIn room "Trepidimous"))
                        RaidState.empty
                    |> List.tail

                let shut =
                    List.zip [ 100..140 ] bouncing
                    |> List.map (fun (t, state) ->
                        t, Set.contains outpostRoom (shutAt (t + 1) state))

                Expect.equal
                    (shut |> List.filter (fun (t, _) -> t >= 102) |> List.filter (snd >> not))
                    []
                    "the first return shuts it for RaidBounceTicks, however often the squad steps out"

                Expect.isEmpty
                    (shutAt
                        153
                        (List.last bouncing |> raidTick 141 (squadIn raidRoom "Trepidimous")))
                    "and it reopens once the bounce hold runs out with the squad gone"
            }

            // The review's leapfrog: a lone body that steps back in is beaten by
            // the guard row, so no deadline enrols it, while the body the row was
            // waiting on goes home.
            test "a raid body standing in the room is waited on, beaten or not" {
                let state =
                    RaidState.empty
                    |> raidTick 100 (spread (List.replicate 5 outpostRoom))
                    |> raidTick 110 (spread [ raidRoom; raidRoom; raidRoom; raidRoom; outpostRoom ])
                    |> raidTick 115 (spread [ outpostRoom; raidRoom; raidRoom; raidRoom; raidRoom ])

                Expect.equal
                    (shutAt 116 state)
                    (Set.singleton outpostRoom)
                    "raid-0 stands in the outpost, so the raid is not gone"

                Expect.isEmpty
                    (state |> raidTick 120 (spread (List.replicate 5 raidRoom)) |> shutAt 121)
                    "and once it too is seen at home the room reopens"
            }

            test "a raid's going never cuts a rival's hold on the room short" {
                let held =
                    squadIn outpostRoom "Trepidimous"
                    |> visible outpostRoom (heldBy ReservationHolder.Rival 500)

                let state =
                    RaidState.empty
                    |> raidTick 100 held
                    |> raidTick 110 (squadIn raidRoom "Trepidimous")

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 100, 600, StandDownBasis.RivalReservation ]
                    "the raid's clock is cut to the hold's end, which is the hold's"

                Expect.equal
                    (shutAt 599 state, shutAt 600 state)
                    (Set.singleton outpostRoom, Set.empty)
                    "shut until the hold ends"
            }

            // #513. Live t958,562: W17S26 waited on Mirroar's 1-MOVE scout after
            // all four Trepidimous raid bodies died at the ramparts.
            test "a body that can hurt nothing never holds a raid's row" {
                let trep = squadIn outpostRoom "Trepidimous"

                let scout =
                    { raiderIn outpostRoom 9 [ Move ] with
                        Owner = "Mirroar"
                    }

                let state =
                    RaidState.empty
                    |> raidTick
                        100
                        { trep with
                            Hostiles = trep.Hostiles @ [ scout ]
                        }
                    |> raidTick
                        110
                        { trep with
                            Hostiles = [ scout ]
                            Fallen = trep.Hostiles |> List.map (fun hostile -> hostile.Id)
                        }

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 100, 110, StandDownBasis.RaidGone ]
                    "the armed bodies are dead; the scout standing there is no raid"
            }

            // #513. Live t954,065-958,574: W18S25 held three player raids 735-1,168
            // ticks each because one body retreated into a room we cannot see.
            // Moved by #585: held its priced span past the tick after its last
            // sighting — with no observer in reach, its whole round trip, 100
            // (`Facts.holdSpan`) — not 200 past its opening.
            test
                "a player's raid nobody sees leave holds the outpost its round trip past its last sighting" {
                let trep = squadIn outpostRoom "Trepidimous"

                let blind =
                    { trep with
                        Hostiles = []
                        RoomControl = Map.remove outpostRoom trep.RoomControl
                    }

                let state =
                    RaidState.empty |> raidTick 100 trep |> raidTick 150 trep |> raidTick 160 blind

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 150, 251, StandDownBasis.PlayerRaidHold ]
                    "the tick after its last sighting plus its round trip, read afresh at each sighting"

                Expect.equal
                    (shutAt 250 state, shutAt 251 state)
                    (Set.singleton outpostRoom, Set.empty)
                    "and the room is worked again on the window's last tick"
            }

            test "an Invader raid nobody sees leave keeps its life" {
                let invaders = squadIn outpostRoom "Invader"

                let state =
                    RaidState.empty
                    |> raidTick 100 invaders
                    |> raidTick 110 { invaders with Hostiles = [] }

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 100, 1600, StandDownBasis.InvaderRaid ]
                    "an NPC never crosses an exit, so its life is its clock"

                let joined =
                    RaidState.empty
                    |> raidTick 100 (squadIn outpostRoom "Trepidimous")
                    |> raidTick
                        150
                        { invaders with
                            Hostiles =
                                (squadIn outpostRoom "Trepidimous").Hostiles
                                @ [
                                    { raiderIn outpostRoom 9 [ Attack; Move ] with
                                        TicksToLive = 1_000
                                    }
                                ]
                        }
                    |> raidTick 160 { invaders with Hostiles = [] }

                Expect.equal
                    (standDowns joined)
                    [ outpostRoom, 100, 150, 1650, StandDownBasis.InvaderRaid ]
                    "an Invader joining a player's raid puts the room back on the raid's life"
            }

            test "a player's raid over a longer rival hold stands down to the hold" {
                let held =
                    squadIn outpostRoom "Trepidimous"
                    |> visible outpostRoom (heldBy ReservationHolder.Rival 500)

                let state =
                    RaidState.empty
                    |> raidTick 100 held
                    |> raidTick
                        110
                        { squadIn outpostRoom "Trepidimous" with
                            Hostiles = []
                        }

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 100, 600, StandDownBasis.RivalReservation ]
                    "the later of the two"

                // The review's HIGH: the hold's basis is no raid clock, so a
                // later sighting put the row back on the squad's life.
                let seenAgain =
                    RaidState.empty
                    |> raidTick 100 held
                    |> raidTick 150 (squadIn outpostRoom "Trepidimous")
                    |> raidTick
                        160
                        { squadIn outpostRoom "Trepidimous" with
                            Hostiles = []
                        }

                Expect.equal
                    (standDowns seenAgain)
                    [ outpostRoom, 100, 150, 600, StandDownBasis.RivalReservation ]
                    "a later sighting of the squad still ends on the hold"
            }

            // Moved by #585: the window is the last sighting's plus the round
            // trip (100), not the first's plus 200.
            test "a short-lived first squad's reinforcements stay inside its window" {
                let trep = squadIn outpostRoom "Trepidimous"

                let state =
                    RaidState.empty
                    |> raidTick
                        100
                        { trep with
                            Hostiles =
                                trep.Hostiles
                                |> List.map (fun hostile -> { hostile with TicksToLive = 150 })
                        }
                    |> raidTick 180 trep
                    |> raidTick 190 { trep with Hostiles = [] }

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 180, 281, StandDownBasis.PlayerRaidHold ]
                    "last sighting plus its round trip, not the reinforcements' life"
            }

            // Moved by #585 as above: 150 plus 100, not plus 200.
            test "a player's raid joining a row another clock opened gets its own window" {
                let reserved =
                    quiet
                    |> withDeclaredOutpost outpostRoom
                    |> visible outpostRoom (heldBy ReservationHolder.Rival 100)

                let state =
                    RaidState.empty
                    |> raidTick 100 reserved
                    |> raidTick 150 (squadIn outpostRoom "Trepidimous")
                    |> raidTick
                        160
                        { squadIn outpostRoom "Trepidimous" with
                            Hostiles = []
                        }

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 150, 251, StandDownBasis.PlayerRaidHold ]
                    "the tick after the squad's last sighting plus its round trip"
            }

            test
                "a body whose weapons are shot off neither opens nor holds a row, unless a heal reaches it" {
                // Five 3A3M bodies at three parts' hits: the head (every ATTACK) is gone.
                let disarmed (colony: ColonyView) =
                    { colony with
                        Hostiles =
                            colony.Hostiles
                            |> List.map (fun hostile ->
                                { hostile with
                                    Hits = 3 * Engine.partHits
                                })
                    }

                Expect.isEmpty
                    (standDowns (
                        RaidState.empty
                        |> raidTick 100 (disarmed (squadIn outpostRoom "Trepidimous"))
                    ))
                    "nothing standing there can hurt anything"

                let trep = squadIn outpostRoom "Trepidimous"

                let state =
                    RaidState.empty
                    |> raidTick 100 trep
                    |> raidTick
                        110
                        { disarmed trep with
                            Hostiles = (disarmed trep).Hostiles |> List.take 1
                            Fallen =
                                trep.Hostiles |> List.skip 1 |> List.map (fun hostile -> hostile.Id)
                        }

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 100, 110, StandDownBasis.RaidGone ]
                    "four dead and the fifth disarmed in the room is a raid gone"

                // The Brief's S7, rule 1: beside a healer of its owner's its parts
                // come back, so it is still the raid, and the row holds.
                let medic =
                    { raiderIn outpostRoom 9 [ Heal; Move ] with
                        Owner = "Trepidimous"
                    }

                let healed =
                    RaidState.empty
                    |> raidTick 100 trep
                    |> raidTick
                        110
                        { disarmed trep with
                            Hostiles = ((disarmed trep).Hostiles |> List.take 1) @ [ medic ]
                            Fallen =
                                trep.Hostiles |> List.skip 1 |> List.map (fun hostile -> hostile.Id)
                        }

                Expect.equal
                    (shutAt 111 healed)
                    (Set.singleton outpostRoom)
                    "the fifth disarmed beside a healer holds the row"

                // The Brief's S7, rule 1: a WORK left and its weapons shot off,
                // alone in the room, is no raid a Guard answers, so it holds no
                // row (a dismantler beside raiders still does, below).
                let worker =
                    { raiderIn outpostRoom 0 [ Attack; Attack; Work; Move ] with
                        Owner = "Trepidimous"
                        Hits = 2 * Engine.partHits
                    }

                let workerLeft =
                    RaidState.empty
                    |> raidTick 100 trep
                    |> raidTick
                        110
                        { trep with
                            Hostiles = [ worker ]
                            Fallen =
                                trep.Hostiles |> List.skip 1 |> List.map (fun hostile -> hostile.Id)
                        }

                Expect.equal
                    (standDowns workerLeft)
                    [ outpostRoom, 100, 100, 110, StandDownBasis.RaidGone ]
                    "a disarmed WORK body alone is a raid gone"
            }

            test "a dismantler beside the armed bodies is waited on" {
                let trep = squadIn outpostRoom "Trepidimous"

                let dismantler =
                    { raiderIn outpostRoom 9 [ Work; Work; Move ] with
                        Owner = "Trepidimous"
                    }

                let state =
                    RaidState.empty
                    |> raidTick
                        100
                        { trep with
                            Hostiles = trep.Hostiles @ [ dismantler ]
                        }
                    |> raidTick 110 (squadIn raidRoom "Trepidimous")

                Expect.equal
                    (shutAt 111 state)
                    (Set.singleton outpostRoom)
                    "the WORK body nobody saw leave holds the row"
            }

            test "a player's raid's hold is priced: evidence, trip, crew and income (#585)" {
                let span = Fabot.Core.Decide.Facts.holdSpan

                Expect.equal
                    (span 10.0 0.0 100 5)
                    5
                    "nothing to send back: reopened on the first look"

                Expect.equal
                    (span 100.0 400.0 100 5)
                    5
                    "a crew worth less than one wait's income: reopened on the first look, never before"

                Expect.equal (span 10.0 5_000.0 100 100) 100 "no observer in reach: the whole trip"

                Expect.isLessThan
                    (span 10.0 5_000.0 100 5)
                    (span 10.0 5_000.0 200 5)
                    "a slower raid is held longer"

                Expect.isLessThan
                    (span 10.0 1_000.0 100 5)
                    (span 10.0 5_000.0 100 5)
                    "a dearer crew is held longer"

                Expect.isLessThan
                    (span 40.0 5_000.0 100 5)
                    (span 10.0 5_000.0 100 5)
                    "a richer room reopens sooner"
            }

            test
                "a player's raid in sight shuts its declared outpost past the next tick, an observer in reach or not (#585)" {
                let trep = squadIn outpostRoom "Trepidimous"

                let observed =
                    { trep with
                        World =
                            { trep.World with
                                Observers = [ "W12S28" ]
                            }
                    }

                for colony, name in [ trep, "unobserved"; observed, "observed" ] do
                    let state = RaidState.empty |> raidTick 100 colony

                    Expect.isTrue
                        (state.Outposts
                         |> List.exists (fun row ->
                             row.RoomName = outpostRoom
                             && row.Expiry >= 100 + 1 + Fabot.Core.Decide.Facts.lookStride))
                        $"{name}: shut at least past the next tick and the first look"

                    Expect.contains (shutAt 101 state) outpostRoom $"{name}: the gate reads it shut"
            }

            test "a player's raid back after its window shuts the outpost again" {
                let trep = squadIn outpostRoom "Trepidimous"

                let state =
                    RaidState.empty
                    |> raidTick 100 trep
                    |> raidTick 110 { trep with Hostiles = [] }
                    |> raidTick 350 trep

                Expect.equal
                    (standDowns state |> List.last)
                    (outpostRoom, 350, 350, 451, StandDownBasis.PlayerRaidHold)
                    "a fresh row with a window of its own (#585: the next tick plus its round trip)"

                Expect.equal (shutAt 351 state) (Set.singleton outpostRoom) "shut on sight"
            }

            // #510. Live t951,926: Torque404 left in W18S25 at 1,454 of 3,500 hits,
            // 14 ATTACK and 1 MOVE alive. The destroyed parts still weigh, so it
            // steps a plain tile every seventeen ticks and harms nothing not beside
            // it; read on its live ATTACK alone it shut the outpost for its life.
            test
                "a melee that can barely move opens no stand-down; the same body with its legs shuts the room" {
                let torque =
                    List.replicate 17 Move
                    @ List.replicate 3 Attack
                    @ [ Move ]
                    @ List.replicate 14 Attack

                let raidOf hits moves =
                    { (quiet |> withDeclaredOutpost outpostRoom) with
                        Hostiles =
                            [
                                { raiderIn outpostRoom 1 (moves @ torque) with
                                    Hits = hits
                                }
                            ]
                        Bank = { Available = 0; Capacity = 800 }
                    }

                Expect.isEmpty
                    (standDowns (RaidState.empty |> raidTick 100 (raidOf 1_454 [])))
                    "fourteen ATTACK on one MOVE under seventeen parts' weight is stationary: the outpost works round it"

                Expect.equal
                    (standDowns (RaidState.empty |> raidTick 100 (raidOf 3_500 [])))
                    [ outpostRoom, 100, 100, 1600, StandDownBasis.InvaderRaid ]
                    "the whole body walks, and the one block the bank buys loses to it, as before"

                // #510 review: a body with no MOVE acting may yet be pulled, so
                // it is weighed as a raid. The Brief's S7, rule 2 (shut on Lose
                // only): a ranger kites it down, so it opens no stand-down.
                Expect.isEmpty
                    (standDowns (RaidState.empty |> raidTick 100 (raidOf 1_400 [])))
                    "fourteen ATTACK and no MOVE is no stationary body, and a ranger kites it: no stand-down"
            }

            // #510, changed by the Brief's S7, rule 2 (shut on Lose only): a raid
            // a ranger kites down is answered, so the room stays open.
            test
                "a raid a ranger kites down opens no stand-down; one that closes on it shuts the outpost" {
                let raidOf body =
                    { (quiet |> withDeclaredOutpost outpostRoom) with
                        Hostiles = [ raiderIn outpostRoom 1 body ]
                        Bank = { Available = 0; Capacity = 800 }
                    }

                let plodder = List.replicate 5 Move @ List.replicate 12 Attack
                let runner = List.replicate 10 Move @ List.replicate 10 Attack

                let gate body =
                    standDown Tuning.defaults 101 (RaidState.empty |> raidTick 100 (raidOf body))

                Expect.equal
                    ((gate plodder).Shut, (gate plodder).ChasedOutposts)
                    (Set.empty, Set.empty)
                    "twelve ATTACK on five MOVE is kited: the room stays open"

                Expect.equal
                    ((gate runner).Shut, (gate runner).ChasedOutposts)
                    (Set.singleton outpostRoom, Set.empty)
                    "the same blows on a MOVE a part shut it and are no chase"

                // A room shut on a raid that is then seen kitable is the
                // ranger's chase, through the ticks nothing sees it.
                let thenDark =
                    RaidState.empty
                    |> raidTick 100 (raidOf runner)
                    |> raidTick 101 (raidOf plodder)
                    |> raidTick 102 { (raidOf plodder) with Hostiles = [] }

                Expect.equal
                    (standDown Tuning.defaults 103 thenDark).ChasedOutposts
                    (Set.singleton outpostRoom)
                    "and stays a chase through the ticks nothing sees it"
            }

            test "two melee blocks cannot use self-heal to win an equal exchange" {
                // A two-block body, as above: a reach of two blocks.
                let raid attacks =
                    { (quiet |> withDeclaredOutpost outpostRoom) with
                        Hostiles =
                            List.replicate
                                2
                                (List.replicate attacks Attack @ List.replicate (12 - attacks) Move)
                            |> List.mapi (raiderIn outpostRoom)
                        Bank = { Available = 0; Capacity = 1_500 }
                    }

                // Each side's swing is struck back (#624): the raid deals its
                // swings and our swing's 30 an ATTACK back, ours its 180 and
                // the 180 each raider takes back off its swing. Moved on
                // purpose (#647): the body is priced at the hits it keeps as
                // the gate prices a fresh one, struck for the horizon, so the
                // edge is a raid of 3 ATTACK a body, not 4.
                Expect.isEmpty
                    (standDowns (RaidState.empty |> raidTick 100 (raid 3)))
                    "two blocks kill three-ATTACK raiders before they fall"

                Expect.equal
                    (standDowns (RaidState.empty |> raidTick 100 (raid 4)))
                    [ outpostRoom, 100, 100, 1600, StandDownBasis.InvaderRaid ]
                    "2,400 at 540 against 2,000 less two ticks of 360 is not a win the gate would seat; fictitious self-heal must not keep the room open"
            }

            test "a transit-room hostile opens no stand-down, regardless of owner" {
                // #324. W15S26 is projected only because the Errand's chain crosses
                // it. The ordinary Invader is the asymmetric case: filtering only the
                // expected Source Keeper would make this half fail.
                let transitRoom = "W15S26"

                let overwhelming =
                    List.replicate 5 [ Attack; Attack; Attack; Attack; Move; Move; Move; Move ]

                let raidBy owner =
                    { quiet with
                        Hostiles =
                            overwhelming
                            |> List.mapi (raiderIn transitRoom)
                            |> List.map (fun hostile -> { hostile with Owner = owner })
                    }

                for owner in [ "Source Keeper"; "Invader"; "Shibdib" ] do
                    Expect.isEmpty
                        (standDowns (RaidState.empty |> raidTick 100 (raidBy owner)))
                        $"{owner} in a room the colony only crosses is no stand-down"
            }

            test "a home this colony defends opens no stand-down" {
                // #428: the raid that stands a declared outpost down, in the
                // same room read as a defended home instead.
                let defended = "W13S28"

                let overwhelming =
                    List.replicate 5 [ Attack; Attack; Attack; Attack; Move; Move; Move; Move ]

                let raidedAs (colony: ColonyView) =
                    { colony with
                        Hostiles = overwhelming |> List.mapi (raiderIn defended)
                        Bank = { Available = 0; Capacity = 1_500 }
                    }

                Expect.isNonEmpty
                    (standDowns (
                        RaidState.empty
                        |> raidTick 100 (raidedAs (quiet |> withDeclaredOutpost defended))
                    ))
                    "the premise: declared as an outpost, the room is stood down from"

                Expect.isEmpty
                    (standDowns (
                        RaidState.empty
                        |> raidTick
                            100
                            (raidedAs
                                { quiet with
                                    Borrowed =
                                        { BorrowedWork.empty with
                                            Defended = [ defended ]
                                        }
                                })
                    ))
                    "as a defended home it is not"

                // #447: raised, its Guard is the ranger row's, and still no
                // stand-down.
                Expect.isEmpty
                    (standDowns (
                        RaidState.empty
                        |> raidTick
                            100
                            (raidedAs
                                { quiet with
                                    Borrowed =
                                        { BorrowedWork.empty with
                                            Rooms = [ defended ]
                                            Defended = [ defended ]
                                        }
                                    Stages = Map.add defended Bootstrapping quiet.Stages
                                })
                    ))
                    "nor as a home it raises as well as defends"
            }

            test "an invader core opens a stand-down that runs to its collapse timer" {
                // The only deadline the engine hands over already absolute: the shell
                // added this tick to `ticksRemaining` on the way in (#133).
                let state = RaidState.empty |> raidTick 100 (seen [ core outpostRoom (Some 900) ])

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 100, 900, StandDownBasis.CollapseTimer ]
                    "the room, the tick it opened on, and the tick read off the threat itself"
            }

            test "with neither deadline readable the clock is the expansion period" {
                // A stronghold still deploying carries no collapse timer for its
                // first ~5,000 ticks, the shape the fallback exists for.
                let state = RaidState.empty |> raidTick 100 (seen [ core outpostRoom None ])

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "2,500 ticks on from the sighting, and the record says it was chosen and not read"
            }

            test "only the invader's own hold is a clock" {
                // A deploying stronghold, no collapse timer on it, beside one holder
                // at a time: no hold is the core's clock, so neither moves its
                // fallback.
                //
                // Since #165 a rival's hold is a deadline of its own (300 ticks here):
                // the ring keeps the **later** of the reads a tick produces for one
                // room, so the rival's short clock never cuts a core's stand-down short.
                let held holder =
                    RaidState.empty
                    |> raidTick
                        100
                        (seen [ core outpostRoom None ] |> visible outpostRoom (heldBy holder 300))
                    |> standDowns

                Expect.equal
                    (held ReservationHolder.Rival)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "a player's 300-tick hold is no deadline for the core, and never shortens its clock"

                Expect.equal
                    (held ReservationHolder.Ours)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "and the colony's own hold says nothing about the core standing in it"
            }

            test "a core still standing there extends the stand-down and re-reads its clock" {
                let state =
                    RaidState.empty
                    |> raidTick 100 (seen [ core outpostRoom None ])
                    |> raidTick 110 (seen [ core outpostRoom None ])

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 110, 2610, StandDownBasis.Fallback ]
                    "one episode, its window carried to the last sighting and its clock read at it"
            }

            test "a re-read never shortens a stand-down that is already running" {
                // A later sighting can land on a worse deadline (the read falls
                // through to the fallback), and reading that in would cut the
                // stand-down short. The same rule `deadlines` applies to two cores in
                // one tick, applied across ticks.
                let shortened =
                    RaidState.empty
                    |> raidTick 100 (seen [ core outpostRoom (Some 4100) ])
                    |> raidTick 110 (seen [ core outpostRoom None ])

                Expect.equal
                    (standDowns shortened)
                    [ outpostRoom, 100, 110, 4100, StandDownBasis.CollapseTimer ]
                    "the window still extends to the sighting, and the clock and the reason it was read off both stand"

                let lengthened =
                    RaidState.empty
                    |> raidTick 100 (seen [ core outpostRoom None ])
                    |> raidTick 110 (seen [ core outpostRoom (Some 4110) ])

                Expect.equal
                    (standDowns lengthened)
                    [ outpostRoom, 100, 110, 4110, StandDownBasis.CollapseTimer ]
                    "and a longer deadline is taken, with the basis of the tick that won"
            }

            test "a tick without vision moves no clock and closes no stand-down" {
                // #117: losing vision reads exactly like peace, and the quiet gap here
                // is five ticks, so the spawn family would have closed this episode
                // six times over.
                let standing = RaidState.empty |> raidTick 100 (seen [ core outpostRoom None ])

                let blind =
                    (standing, [ 101..130 ])
                    ||> List.fold (fun state t -> state |> raidTick t quiet)

                Expect.equal
                    (standDowns blind)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "thirty blind ticks leave the record exactly as the last tick with vision left it"
            }

            test "a room seen clear stands down all the same, until its clock runs out" {
                // A tick with vision and no core is what a creep passing the wrong
                // tile sees, and even a true one does not open the gate early.
                let state =
                    RaidState.empty
                    |> raidTick 100 (seen [ core outpostRoom None ])
                    |> raidTick 101 (quiet |> visible outpostRoom None)

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "the room looking clear neither closed the episode nor moved its expiry"
            }

            test "the clock runs out, and the next core seen opens a second stand-down" {
                let state =
                    RaidState.empty
                    |> raidTick 100 (seen [ core outpostRoom (Some 105) ])
                    |> raidTick 105 (seen [ core outpostRoom (Some 130) ])

                Expect.equal
                    (standDowns state)
                    [
                        (outpostRoom, 100, 100, 105, StandDownBasis.CollapseTimer)
                        (outpostRoom, 105, 105, 130, StandDownBasis.CollapseTimer)
                    ]
                    "the expiry is the first tick the room may be re-entered, so a sighting on it is a new stand-down and the spent one stays in the ring"
            }

            test "each outpost's clock is its own" {
                let other = "W13S28"

                let state =
                    RaidState.empty
                    |> raidTick 100 (seen [ core outpostRoom (Some 500); core other (Some 700) ])
                    |> raidTick 110 (seen [ core other (Some 700) ])

                Expect.equal
                    (standDowns state)
                    [
                        (outpostRoom, 100, 100, 500, StandDownBasis.CollapseTimer)
                        (other, 100, 110, 700, StandDownBasis.CollapseTimer)
                    ]
                    "the tick that saw one room and not the other moved that room's episode alone"
            }

            test "a ring full of raids evicts no stand-down that is running" {
                // One ring shared between the families would drop the episode driving
                // the gate mid-stand-down (#117). The cap is three and four raids
                // overflow it.
                let state =
                    (RaidState.empty |> raidTick 10 (seen [ core outpostRoom (Some 500) ]),
                     [ 20; 30; 40; 50 ])
                    ||> List.fold (fun state t -> state |> raidTick t (raid squad))

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 10, 10, 500, StandDownBasis.CollapseTimer ]
                    "the stand-down is still there with its clock untouched"

                Expect.equal
                    (windows state)
                    [ (30, 30); (40, 40); (50, 50) ]
                    "while the raid ring trims to the cap exactly as it did before"
            }

            test "a stand-down still running survives a ring overflowing past it" {
                // Six spent stand-downs in one room around one long-running one
                // elsewhere, against a cap of three: the overflow is paid out of the
                // finished rows.
                let other = "W13S28"

                let state =
                    RaidState.empty
                    |> raidTick 10 (seen [ core outpostRoom (Some 11) ])
                    |> raidTick 12 (seen [ core outpostRoom (Some 13) ])
                    |> raidTick 14 (seen [ core outpostRoom (Some 15) ])
                    |> raidTick 16 (seen [ core other (Some 5000) ])
                    |> raidTick 18 (seen [ core outpostRoom (Some 19) ])
                    |> raidTick 20 (seen [ core outpostRoom (Some 21) ])
                    |> raidTick 22 (seen [ core outpostRoom (Some 23) ])

                Expect.equal
                    (standDowns state)
                    [
                        (other, 16, 16, 5000, StandDownBasis.CollapseTimer)
                        (outpostRoom, 20, 20, 21, StandDownBasis.CollapseTimer)
                        (outpostRoom, 22, 22, 23, StandDownBasis.CollapseTimer)
                    ]
                    "the oldest row is the one still standing down, and it is the one row the trim would not take"
            }

            test "a core in an outpost leaves the spawn room's raid exactly as it was" {
                // #117's regression: the two families share a Memory leaf and nothing
                // else. Compared as whole states, so a field no list here reads is
                // covered too.
                let sequence (colony: ColonyView) =
                    (RaidState.empty, [ 10..14 ])
                    ||> List.fold (fun state t ->
                        { colony with
                            Creeps = if t < 12 then [ ours "w1"; ours "w2" ] else [ ours "w1" ]
                        }
                        |> withHits "ram-1" BuiltKind.Rampart (100_000 - 200 * t)
                        |> fun tick -> state |> raidTick t tick)

                let alone = sequence { placed with Hostiles = squad }

                let beside =
                    sequence
                        { placed with
                            Hostiles = squad
                            InvaderCores = [ core outpostRoom None ]
                        }

                Expect.equal
                    { beside with Outposts = [] }
                    alone
                    "the raid reads byte for byte the same, roster, approach, losses, damage and baselines alike"

                Expect.equal
                    (standDowns beside)
                    [ outpostRoom, 10, 14, 2514, StandDownBasis.Fallback ]
                    "while the core standing next door recorded a stand-down of its own"
            }
        ]

[<Tests>]
let clocklessTests =
    testList
        "raid fold: the withdrawal with no clock"
        [
            test "a room another player holds is remembered, and no episode opens for it" {
                // Not a threat, so no deadline and nothing for a basis to explain: the
                // record is the room's name.
                //
                // **Ownership alone since #165.** The engine ends a reservation and
                // ends nothing about an owner, so the reservation moved to the ring;
                // the pairwise contrast is one field of the control entry apart.
                let owned = RaidState.empty |> raidTick 100 (quiet |> ownedByRival outpostRoom)

                let reserved =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Rival 4000))

                Expect.equal
                    owned.RivalHeld
                    (Map.ofList [ outpostRoom, { Since = 100; LastLooked = 100 } ])
                    "an owner that is not us, against the tick the look was taken on"

                Expect.isEmpty
                    reserved.RivalHeld
                    "and a reservation that is not ours latches nothing: it runs out on its own"

                Expect.isEmpty owned.Outposts "no episode opened: there is no clock to run"

                Expect.equal
                    (standDowns reserved)
                    [ outpostRoom, 100, 100, 4100, StandDownBasis.RivalReservation ]
                    "while the reservation is an episode of the ring, clocked to the hold's own end"
            }

            test "the NPC's hold is a clock and never an exit" {
                // Pairwise, one holder at a time, the reason `ReservationHolder` is
                // three states and not a "not ours" flag (#133). Since #165 no holder
                // latches: the NPC's clock is read off the core, the rival's off the
                // hold, and ours is no clock at all.
                let folded holder =
                    RaidState.empty
                    |> raidTick 100 (quiet |> visible outpostRoom (heldBy holder 4000))

                let held holder = (folded holder).RivalHeld

                Expect.isEmpty
                    (held ReservationHolder.Invader)
                    "the Invader's hold withdraws nothing on its own"

                Expect.isEmpty (held ReservationHolder.Ours) "and neither does our own"

                Expect.isEmpty
                    (held ReservationHolder.Rival)
                    "nor does the third: since #165 the latch is ownership's alone"

                // The NPC's hold is no clock: a stronghold's is its core's
                // (`deadlineOf`), so a hold with no core standing in the room opens
                // nothing, where a rival's opens an episode on the spot.
                Expect.isEmpty
                    (standDowns (folded ReservationHolder.Invader))
                    "no core is standing there, so the NPC's hold is nobody's deadline this tick"

                Expect.equal
                    (standDowns (folded ReservationHolder.Rival))
                    [ outpostRoom, 100, 100, 4100, StandDownBasis.RivalReservation ]
                    "while the rival's hold is a stand-down running to the end of that hold"

                Expect.isEmpty
                    (standDowns (folded ReservationHolder.Ours))
                    "and our own hold is the steady state of every outpost, not a withdrawal"
            }

            test "the conclusion is held through every tick nobody is looking" {
                // The reason this is persisted rather than read off each tick's
                // ColonyView: the gate's own effect is to withdraw the creeps that paid
                // for the vision that judged it. Re-read from nothing it would reopen
                // the room the tick after it shut it.
                let taken = RaidState.empty |> raidTick 100 (quiet |> ownedByRival outpostRoom)

                let blind =
                    (taken, [ 101..130 ]) ||> List.fold (fun state t -> state |> raidTick t quiet)

                Expect.equal
                    blind.RivalHeld
                    (Map.ofList [ outpostRoom, { Since = 100; LastLooked = 100 } ])
                    "thirty blind ticks leave the last look's conclusion, and its tick, exactly where they stood"
            }

            test "the tick recorded is the look that shut the gate, not the last look" {
                // #117's US-20: the number beside the room is the tick the withdrawal
                // began, so an operator can line an income drop up against it. A
                // second look that agrees must not restamp it.
                let twice =
                    RaidState.empty
                    |> raidTick 100 (quiet |> ownedByRival outpostRoom)
                    |> raidTick 140 (quiet |> ownedByRival outpostRoom)

                Expect.equal
                    twice.RivalHeld
                    (Map.ofList [ outpostRoom, { Since = 100; LastLooked = 100 } ])
                    "the tick the gate shut on, forty ticks after a second look agreed with it — and no look fell due in between (#275)"
            }

            test "a room taken again after it was freed is dated by the second withdrawal" {
                // The tick is the *current* withdrawal's, not the room's first ever.
                let again =
                    RaidState.empty
                    |> raidTick 100 (quiet |> ownedByRival outpostRoom)
                    |> raidTick 120 (quiet |> visible outpostRoom None)
                    |> raidTick 300 (quiet |> ownedByRival outpostRoom)

                Expect.equal
                    again.RivalHeld
                    (Map.ofList [ outpostRoom, { Since = 300; LastLooked = 300 } ])
                    "the look that shut it this time, and not the one whose gate has been cleared"
            }

            test "only a tick with vision takes a room back out" {
                // Written on vision in both directions: a look that finds the room
                // free is as good evidence as the look that found it taken.
                //
                // Live, that second look used to be something the bot could not
                // arrange, which made the withdrawal permanent until a human
                // hand-edited the leaf. #165 re-admits a latched room to the **scan**
                // every `Tuning.RivalRecheck` ticks, and this fold is where such a
                // look lands.
                let freed =
                    RaidState.empty
                    |> raidTick 100 (quiet |> ownedByRival outpostRoom)
                    |> raidTick 101 (quiet |> visible outpostRoom None)

                Expect.isEmpty freed.RivalHeld "the room the colony can see is nobody else's again"
            }

            test "the look that falls due moves the stride, and never the date the gate shut on" {
                // #275. The stride is measured between *looks*, carried beside the
                // shutting tick rather than over it: one dates an income drop, the
                // other says when the colony next questions its own conclusion.
                //
                // The look is stamped whether or not vision answered: the colony is
                // usually blind in a withdrawn room, and a look stamped only when it
                // saw something would leave the room re-admitted on every tick from
                // the stride onwards.
                let recheck = Tuning.defaults.RivalRecheck
                let state = RaidState.empty |> raidTick 100 (quiet |> ownedByRival outpostRoom)

                Expect.equal
                    state.RivalHeld
                    (Map.ofList [ outpostRoom, { Since = 100; LastLooked = 100 } ])
                    "the look that shut the gate is the last look taken, so far"

                // Forty ticks late: a tick the gate was not evaluated on delays the
                // look rather than cancelling it.
                let looked = state |> raidTick (100 + recheck + 40) quiet

                Expect.equal
                    looked.RivalHeld
                    (Map.ofList
                        [
                            outpostRoom,
                            {
                                Since = 100
                                LastLooked = 100 + recheck + 40
                            }
                        ])
                    "the blind look is taken all the same, and the date of the withdrawal stands"

                Expect.isEmpty
                    (recheckedAt (100 + recheck + 41) looked)
                    "so the tick after the look, nothing is looked into"

                Expect.equal
                    (recheckedAt (100 + 2 * recheck + 40) looked)
                    (Set.singleton outpostRoom)
                    "and the next look falls a stride after the look, not a stride after the shutting"
            }

            test "a look with vision that agrees moves the stride and clears nothing" {
                // The look that finds the rival still there: the latch and its date
                // stand, and the stride runs again from this look.
                let recheck = Tuning.defaults.RivalRecheck

                let agreed =
                    RaidState.empty
                    |> raidTick 100 (quiet |> ownedByRival outpostRoom)
                    |> raidTick (100 + recheck) (quiet |> ownedByRival outpostRoom)

                Expect.equal
                    agreed.RivalHeld
                    (Map.ofList
                        [
                            outpostRoom,
                            {
                                Since = 100
                                LastLooked = 100 + recheck
                            }
                        ])
                    "still theirs, shut since the same tick, and looked into on the stride"
            }

            test "a colony nobody has taken anything from remembers nothing" {
                // The home room is in `RoomControl` on every tick with vision, so a
                // rule reading "somebody holds this" the wrong way round would
                // withdraw the colony from itself.
                let home =
                    RaidState.empty
                    |> raidTick
                        100
                        { quiet with
                            RoomControl =
                                Map.ofList
                                    [
                                        raidRoom,
                                        {
                                            Owner = Ownership.Ours
                                            Reservation = None
                                            SafeMode = false
                                            SafeModeCooldownUntil = 0
                                            Sign = None
                                        }
                                    ]
                        }

                Expect.isEmpty home.RivalHeld "the room we own is not a room somebody took"
            }

            // The world layer's S0 pin, flipped at S2 (one world danger log).
            // Live t973,5xx: W18S26 was in W15S28's and W17S29's `rivalHeld`,
            // looked into at 970,261 and 972,434.
            test "two colonies that see one rival room hold one latch, on one look stride" {
                let recheck = Tuning.defaults.RivalRecheck

                let other =
                    { quiet with
                        Spatial =
                            { quiet.Spatial with
                                RoomName = Some "W13S28"
                            }
                    }

                let _, danger =
                    (RaidState.empty, Danger.empty)
                    |> dangerTick [] 100 (quiet |> ownedByRival outpostRoom)
                    |> dangerTick [] (100 + recheck) (quiet |> ownedByRival outpostRoom)
                    |> fun (_, danger) -> RaidState.empty, danger
                    |> dangerTick [] (100 + recheck + 10) (other |> ownedByRival outpostRoom)

                Expect.equal
                    danger.Rivals
                    (Map.ofList
                        [
                            outpostRoom,
                            {
                                Since = 100
                                LastLooked = 100 + recheck
                            }
                        ])
                    "one latch, dated by the first look, strided by the last look due"

                Expect.equal
                    (recheckedAt (100 + 2 * recheck) (Danger.into danger RaidState.empty))
                    (Set.singleton outpostRoom)
                    "and every colony's gate looks again on the one stride"
            }
        ]

[<Tests>]
let holdTests =
    testList
        "raid fold: the reservation somebody else is standing on"
        [
            test "a hold that is not ours is recorded against the tick it runs out on" {
                // #333's record: the engine answers ERR_INVALID_TARGET on a controller
                // anybody but us holds, so the channel can say *why* a declared outpost
                // is being mined and not reserved.
                //
                // The tick is absolute, the engine's countdown being relative: 100 +
                // 4,000. Stored as read it would date a hold to the start of the world.
                let recorded holder =
                    (RaidState.empty
                     |> raidTick 100 (quiet |> visible outpostRoom (heldBy holder 4000)))
                        .Holds

                Expect.equal
                    (recorded ReservationHolder.Invader)
                    (Map.ofList
                        [
                            outpostRoom,
                            {
                                Holder = ReservationHolder.Invader
                                Until = 4100
                            }
                        ])
                    "the Invader's hold — W12S27's own case, the core long since collapsed"

                Expect.equal
                    (recorded ReservationHolder.Rival)
                    (Map.ofList
                        [
                            outpostRoom,
                            {
                                Holder = ReservationHolder.Rival
                                Until = 4100
                            }
                        ])
                    "and another player's, which the engine refuses us in exactly the same words"

                Expect.isEmpty
                    (recorded ReservationHolder.Ours)
                    "our own hold is the steady state of every outpost and is recorded nowhere"

                Expect.isEmpty
                    (RaidState.empty |> raidTick 100 (quiet |> visible outpostRoom None)).Holds
                    "and an unreserved controller is the room the row hires for: no entry either"
            }

            test "the hold outlives the vision that read it, and ends itself" {
                // A tick without vision leaves the conclusion standing (the colony is
                // blind in most of these rooms most of the time), and the entry ends
                // on the tick it named, no look being needed.
                let taken =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Invader 4000))

                let blind =
                    (taken, [ 101..130 ]) ||> List.fold (fun state t -> state |> raidTick t quiet)

                Expect.equal
                    blind.Holds
                    taken.Holds
                    "thirty blind ticks leave the last look's conclusion exactly where it stood"

                let atEnd =
                    (taken, [ 4099; 4100 ]) ||> List.fold (fun state t -> state |> raidTick t quiet)

                Expect.isEmpty
                    atEnd.Holds
                    "and on the tick the engine's countdown reaches, the record retires itself"

                let short = taken |> raidTick 4099 quiet

                Expect.equal
                    short.Holds
                    taken.Holds
                    "one tick short of it the hold is still standing, blind or not"
            }

            test "a look that finds the controller free takes the room back out" {
                // A tick with vision decides the room either way. Pinned *before* the
                // clock runs out, or the expiry above would be what cleared it.
                let freed =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Invader 4000))
                    |> raidTick 101 (quiet |> visible outpostRoom None)

                Expect.isEmpty
                    freed.Holds
                    "the controller the colony can see is nobody else's again"

                // A hold re-read is re-clocked: the engine counts down at one a tick,
                // so the same hold read 40 ticks later names the same absolute tick.
                let reread =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Invader 4000))
                    |> raidTick
                        140
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Invader 3960))

                Expect.equal
                    reread.Holds
                    (Map.ofList
                        [
                            outpostRoom,
                            {
                                Holder = ReservationHolder.Invader
                                Until = 4100
                            }
                        ])
                    "the same hold, forty ticks of it spent, still ends on the tick it always did"
            }

            test "the Invader's hold withdraws nothing, and a rival's withdraws the room as well" {
                // The line is the **Invader's** alone; said of "somebody else" it would
                // be false of the other half of `heldByOther`.
                //
                // The Invader's leftover hold: no core stands over it, the ring is
                // clocked off cores, and #165's rival clause does not answer for the
                // NPC. So the room is mined, and since #487 the reservation is
                // attacked off rather than refused; written as a stand-down it would
                // have withdrawn the room, which #333 leaves for a human.
                //
                // A rival's identical hold is a clocked stand-down already (#165), so
                // for that holder the record says *why* a room withheld anyway is
                // also unreservable. Pairwise, one holder apart.
                let heldByWhom holder =
                    RaidState.empty
                    |> raidTick 100 (quiet |> visible outpostRoom (heldBy holder 4000))

                let invader = heldByWhom ReservationHolder.Invader
                let rival = heldByWhom ReservationHolder.Rival

                Expect.isEmpty invader.Outposts "the Invader's leftover hold opens no episode"

                Expect.isEmpty invader.RivalHeld "and latches nothing: the engine ends this hold"

                Expect.isEmpty (shutAt 101 invader) "so the gate withholds that room from nothing"

                Expect.isEmpty
                    (heldAt 101 invader)
                    "nor the reservation: the reserver attacks it off (#487)"

                Expect.isNonEmpty
                    rival.Outposts
                    "a rival's identical hold is #165's episode: the same entry opens a stand-down"

                Expect.equal
                    (shutAt 101 rival)
                    (Set.singleton outpostRoom)
                    "so that room is withheld from the work, and is not one the colony goes on mining"

                Expect.equal
                    (heldAt 101 rival)
                    (Set.singleton outpostRoom)
                    "and it is in this set too: the two families name one room and say different things"
            }

            // #604. Live t1,006,924: Trepidimous killed W17S25's reserver in
            // W17S26 and reserved it; the room stood down to his hold's end, so
            // nothing of ours went back, no vision, no guard, his diggers mined.
            test
                "a declared enemy's hold withdraws nothing and refuses no reserver; a neutral's does both" {
                let heldByWhom holder username =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet
                         |> withDeclaredOutpost outpostRoom
                         |> visible
                             outpostRoom
                             (Some
                                 {
                                     Holder = holder
                                     TicksToEnd = 4000
                                     Username = username
                                 }))

                let enemy = heldByWhom ReservationHolder.Enemy "Trepidimous"
                let neutral = heldByWhom ReservationHolder.Rival "giaco"

                Expect.isEmpty
                    enemy.Outposts
                    "the enemy's hold opens no stand-down: it is contested"

                Expect.isEmpty (shutAt 101 enemy) "so the room stays worked, and seen"

                Expect.isEmpty
                    (heldAt 101 enemy)
                    "and the reserver row hires for it, to attack the hold"

                Expect.equal
                    (standDowns neutral)
                    [ outpostRoom, 100, 100, 4100, StandDownBasis.RivalReservation ]
                    "a neutral's identical hold is #165's stand-down, as ever"

                Expect.equal (heldAt 101 neutral) (Set.singleton outpostRoom) "and #333's refusal"
            }

            // #604's deploy: the live W17S26 row was written as a rival's
            // reservation before an enemy's could read as his.
            test
                "a standing rival-reservation row ends on the first look that reads the hold an enemy's" {
                let reading holder username =
                    quiet
                    |> withDeclaredOutpost outpostRoom
                    |> visible
                        outpostRoom
                        (Some
                            {
                                Holder = holder
                                TicksToEnd = 3900
                                Username = username
                            })

                let fresh =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Rival 4000))

                Expect.isEmpty
                    (recheckedAt 150 fresh)
                    "a row this bundle opened read its holder a neutral's: nothing to question"

                // The row as the bundle before #604 left it on the wire: no
                // look of this bundle has read whose the hold is.
                let written =
                    { fresh with
                        Outposts =
                            fresh.Outposts |> List.map (fun row -> { row with HolderUnread = true })
                    }

                Expect.equal
                    (shutAt 150 written)
                    (Set.singleton outpostRoom)
                    "the premise: the row shuts the room"

                Expect.equal
                    (recheckedAt 150 written)
                    (Set.singleton outpostRoom)
                    "and the gate looks into it every tick, to read whose the hold is"

                Expect.isEmpty
                    (written
                     |> raidTick 200 (reading ReservationHolder.Rival "giaco")
                     |> recheckedAt 201)
                    "until one look reads it: a neutral's long hold is questioned once, not for weeks"

                Expect.isEmpty
                    (written
                     |> raidTick 200 (reading ReservationHolder.Enemy "Trepidimous")
                     |> shutAt 201)
                    "the hold read as the enemy's, the row ends: no stand-down"

                Expect.equal
                    (written |> raidTick 200 quiet |> shutAt 201)
                    (Set.singleton outpostRoom)
                    "a blind tick ends nothing"

                Expect.equal
                    (written |> raidTick 200 (reading ReservationHolder.Rival "giaco") |> shutAt 201)
                    (Set.singleton outpostRoom)
                    "a neutral's hold read again keeps it"
            }

            test "an enemy's hold puts no floor under the raid's clock in the room" {
                let held =
                    squadIn outpostRoom "Trepidimous"
                    |> visible
                        outpostRoom
                        (Some
                            {
                                Holder = ReservationHolder.Enemy
                                TicksToEnd = 500
                                Username = "Trepidimous"
                            })

                let state =
                    RaidState.empty
                    |> raidTick 100 held
                    |> raidTick 110 (squadIn raidRoom "Trepidimous")

                Expect.isEmpty
                    (shutAt 111 state)
                    "the squad gone, the room reopens: his reservation is no reason to stay out"
            }

            test "the gate stops holding a reservation on the tick the engine's countdown reaches" {
                // What the gate does with the record (`Planner.reservableControllers`
                // through `ColonyView.HeldOutposts`): `tick < Until`, the same test the
                // fold retires an entry on and `observe.mjs` prints one under, so a
                // leaf the fold has not caught up with cannot withhold a reservation
                // the engine would now accept.
                let taken =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Rival 4000))

                Expect.equal
                    (heldAt 4099 taken)
                    (Set.singleton outpostRoom)
                    "one tick short of the end the hold stands, blind ever since it was read"

                Expect.isEmpty
                    (heldAt 4100 taken)
                    "and on the tick the countdown reaches, the room is ours to reserve again"

                Expect.isEmpty
                    (heldAt 101 RaidState.empty)
                    "a colony with no log at all holds nothing out of its own pool"
            }
        ]

[<Tests>]
let threatMemoryTests =
    testList
        "raid fold: the guard's memory of a raided outpost"
        [
            test "an armed threat seen in a declared outpost is remembered through the blind ticks" {
                // #366: the bodies the vision comes from are exactly what the raid
                // kills, so the room goes dark and the row that bought a 15-ATTACK-part
                // body stops asking for it. The conclusion is written on the tick with
                // vision and read on the ticks without one.
                let looked =
                    RaidState.empty
                    |> raidTick 100 (quiet |> lookingAt outpostRoom [ armedIn outpostRoom ])

                Expect.equal
                    (looked.Threatened |> Map.tryFind outpostRoom)
                    (Some
                        {
                            Until = 100 + Tuning.defaults.ThreatMemory
                        })
                    "the look writes the room down against its own clock: this tick plus ThreatMemory"

                // The blind tick: no control entry and no hostile on the view, and the
                // record has to survive that.
                let blind = looked |> raidTick 140 quiet

                Expect.equal
                    (blind.Threatened |> Map.tryFind outpostRoom)
                    (Some { Until = 100 + Engine.creepLifetime })
                    "a tick with no vision in the room leaves the conclusion exactly as it found it"

                // The clock is a backstop and not a schedule (#369). It was 300, the
                // guard's cast plus its walk, the right size for *sending* a guard and
                // the wrong size for *remembering*: while the room is dark an expiry
                // cannot mean the raid ended. W15S29 killed four of W15S28's bodies
                // that way in one day, one unarmed body at a time.
                Expect.equal
                    (threatenedAt 999 blind)
                    (Set.singleton outpostRoom)
                    "700 ticks past the old memory's end, with nobody looking, the room is still remembered"

                Expect.equal
                    (threatenedAt (99 + Engine.creepLifetime) blind)
                    (Set.singleton outpostRoom)
                    "one tick short of the backstop it still answers"

                Expect.isEmpty
                    (threatenedAt (100 + Engine.creepLifetime) blind)
                    "and on the tick no raider seen then could still be alive it is forgotten, with no look taken at all"

                Expect.isEmpty
                    (threatenedAt 101 RaidState.empty)
                    "a colony with no log remembers no raid: absence classifies nothing (ADR 0004)"
            }

            test "a look that finds the outpost clear forgets the raid on the tick it takes" {
                // A tick with vision decides the room either way, which keeps this a
                // memory and not a second stand-down. The guard's own arrival usually
                // takes this look, so the cost of a long memory is one body's walk.
                let remembered =
                    RaidState.empty
                    |> raidTick 100 (quiet |> lookingAt outpostRoom [ armedIn outpostRoom ])

                let cleared = remembered |> raidTick 160 (quiet |> lookingAt outpostRoom [])

                Expect.isEmpty
                    cleared.Threatened
                    "the room is seen clear 240 ticks before the memory would have run out, and the entry goes with the look"

                Expect.isEmpty (threatenedAt 161 cleared) "so the gate answers for nothing"

                // Vision and no Threat is a clearing; vision and a Threat is a fresh
                // write, so a raid that outlives the memory is not forgotten while watched.
                let stillThere =
                    remembered
                    |> raidTick 160 (quiet |> lookingAt outpostRoom [ armedIn outpostRoom ])

                Expect.equal
                    (stillThere.Threatened |> Map.tryFind outpostRoom)
                    (Some { Until = 160 + Engine.creepLifetime })
                    "and a look that finds it still standing there moves the clock to this tick's"
            }

            test
                "a healer alone is remembered nowhere, and neither is a room the colony merely crosses" {
                // Two narrowings: a hostile with no ATTACK or RANGED_ATTACK part reaches
                // nothing and writes no memory a guard row could act on; and the guard
                // row is per **declared outpost**, so a raid at home is the keep's and
                // one in an undeclared room hires nobody (#324).
                let healerSeen =
                    RaidState.empty
                    |> raidTick 100 (quiet |> lookingAt outpostRoom [ healerIn outpostRoom ])

                Expect.isEmpty
                    healerSeen.Threatened
                    "a healer standing in the outpost is a hostile with no reach and buys no guard"

                let atHome =
                    RaidState.empty
                    |> raidTick
                        100
                        { (quiet |> visible raidRoom None) with
                            Hostiles = [ armedIn raidRoom ]
                        }

                Expect.isEmpty
                    atHome.Threatened
                    "and an armed raid in the colony's own room is no outpost's memory"

                let undeclared =
                    RaidState.empty
                    |> raidTick
                        100
                        { (quiet |> visible outpostRoom None) with
                            Hostiles = [ armedIn outpostRoom ]
                        }

                Expect.isEmpty
                    undeclared.Threatened
                    "nor is a room with no controller of ours projected in it: no declaration, no guard row"
            }
        ]

[<Tests>]
let gateTests =
    testList
        "the stand-down gate"
        [
            test "a running clock shuts its room, and the tick it runs out opens it" {
                // The gate reads the log the way `observe.mjs outposts` reads it: the
                // expiry is the first tick the room may be re-entered.
                let state = RaidState.empty |> raidTick 100 (seen [ core outpostRoom (Some 900) ])

                Expect.equal
                    (shutAt 899 state)
                    (Set.singleton outpostRoom)
                    "the tick before the clock runs out the room is still withheld"

                Expect.isEmpty
                    (shutAt 900 state)
                    "on the expiry itself the room is back in the set the shell scans"

                Expect.isEmpty (shutAt 5000 state) "and stays there"
            }

            test "each outpost's gate is its own" {
                // Two rooms, one core each, two clocks that run out at different ticks.
                let other = "W13S28"

                let state =
                    RaidState.empty
                    |> raidTick 100 (seen [ core outpostRoom (Some 200); core other (Some 900) ])

                Expect.equal
                    (shutAt 150 state)
                    (Set.ofList [ outpostRoom; other ])
                    "both clocks running, both rooms withheld"

                Expect.equal
                    (shutAt 500 state)
                    (Set.singleton other)
                    "the room whose clock ran out is back on its own, and the other is still shut"
            }

            test "a room in another player's hands is shut by no clock at all" {
                // The two triggers meet in one set: the clocked family carries an
                // expiry, the clockless one nothing to compare.
                let state = RaidState.empty |> raidTick 100 (quiet |> ownedByRival outpostRoom)

                Expect.equal
                    (shutAt 101 state)
                    (Set.singleton outpostRoom)
                    "shut the tick after it was seen taken"

                Expect.equal
                    (shutAt 1_000_000 state)
                    (Set.singleton outpostRoom)
                    "and shut a million ticks later: there is no clock for this one to run out"
            }

            test "a room another player reserved comes back when the reservation runs out" {
                // #165's first half: a passing claimer is routine where an owner is
                // not, and the engine is already counting its hold down, so the room
                // re-enters with **no vision needed**.
                //
                // Pairwise against the owner above: 200 ticks of a rival's reservation
                // seen at t100.
                let reserved =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Rival 200))

                Expect.equal
                    (shutAt 299 reserved)
                    (Set.singleton outpostRoom)
                    "the tick before the hold ends the room is still withheld"

                // t300 and not t301: `TicksToEnd` counts down to the tick the
                // reservation is *gone*, and `Expiry` is the first tick the room may
                // be re-entered (`rivalDeadlines`).
                Expect.isEmpty
                    (shutAt 300 reserved)
                    "on the tick the hold ends the room is back in the pool, nobody having looked"

                Expect.isEmpty (recheckedAt 300 reserved) "and no latch was ever taken to re-check"
            }

            test "a latched room is looked into again every RivalRecheck ticks" {
                // #165's second half: the latch stays ownership's, but the gate hands
                // the shell one room to look into a whole `RivalRecheck` after the
                // look that shut it, so the tick with vision that clears a latch can
                // arrive at all.
                let state = RaidState.empty |> raidTick 100 (quiet |> ownedByRival outpostRoom)
                let recheck = Tuning.defaults.RivalRecheck

                Expect.equal
                    (shutAt (100 + recheck) state)
                    (Set.singleton outpostRoom)
                    "the recheck tick withholds the room from the work exactly as every other tick does"

                Expect.isEmpty
                    (recheckedAt (100 + recheck - 1) state)
                    "one tick short of the stride, nothing is looked into"

                Expect.equal
                    (recheckedAt (100 + recheck) state)
                    (Set.singleton outpostRoom)
                    "on the stride itself the room is re-admitted to the scan"

                // The look is one tick long because taking it stamps the stride, so
                // this has to fold the tick the look was taken on; the gate alone,
                // asked twice, is being asked about a look still owed (#275).
                let looked = state |> raidTick (100 + recheck) (quiet |> ownedByRival outpostRoom)

                Expect.isEmpty
                    (recheckedAt (100 + recheck + 1) looked)
                    "and the look is one tick long"

                Expect.equal
                    (recheckedAt (100 + 2 * recheck) looked)
                    (Set.singleton outpostRoom)
                    "a look that changed nothing leaves the next one a whole stride away"

                Expect.isEmpty
                    (recheckedAt 100 state)
                    "the look that shut the gate is not itself a recheck"

                // The knob at a second value. The exact-multiple rule had its own trap
                // here (5,000 is a multiple of 1,000) and the elapsed rule (#275) has
                // none: 3,100 is past a stride of 3,000 and short of one of 5,000, so
                // the second assertion fails outright if the constant is read in
                // place of the knob.
                let sooner =
                    { Tuning.defaults with
                        RivalRecheck = 3000
                    }

                Expect.equal
                    (standDown sooner 3_100 state).Rechecked
                    (Set.singleton outpostRoom)
                    "a colony tuned to look oftener looks on its own stride"

                Expect.isEmpty
                    (recheckedAt 3_100 state)
                    "and on that tick the shipped stride is not due yet: the knob is read, not a constant"

                Expect.isEmpty
                    (standDown
                        { Tuning.defaults with
                            RivalRecheck = 0
                        }
                        (100 + recheck)
                        state)
                        .Rechecked
                    "a stride of zero is this rule switched off, not a tick divided by nothing"
            }

            test "a tick the gate was never evaluated on delays the look, never forfeits it" {
                // #275. The stride used to be `(tick - since) % RivalRecheck = 0`, a
                // gate asked on precisely the right tick or not at all; the loop can
                // throw before the log is written, the engine cuts a tick short on an
                // empty bucket, and a deploy lands mid-tick. Every tick lost that way
                // cost 5,000 ticks of an outpost's income, silently.
                let recheck = Tuning.defaults.RivalRecheck
                let state = RaidState.empty |> raidTick 100 (quiet |> ownedByRival outpostRoom)

                Expect.isEmpty
                    (recheckedAt (100 + recheck - 1) state)
                    "one tick short of the stride the look is not owed yet"

                Expect.equal
                    (recheckedAt (100 + recheck + 1) state)
                    (Set.singleton outpostRoom)
                    "the tick after the stride, with nothing having looked, the look is still owed"

                Expect.equal
                    (recheckedAt (100 + recheck + 3_000) state)
                    (Set.singleton outpostRoom)
                    "and three thousand ticks after it, nowhere near a multiple of the stride"

                Expect.equal
                    (recheckedAt (100 + 3 * recheck - 1) state)
                    (Set.singleton outpostRoom)
                    "two whole strides of missed ticks are a late look and not a lost one"
            }

            test "a look stamped ahead of the clock is owed now, and the fold stamps the real tick" {
                // #275's own hazard: an elapsed test has no upper bound, so a
                // `LastLooked` in the future silences the latch until the clock
                // catches it up. Two ways in: a mistyped hand edit of this leaf, the
                // documented way out of a stuck latch, and a private server rolled
                // back behind the tick the log was written on.
                let ahead =
                    { RaidState.empty with
                        RivalHeld =
                            Map.ofList [ outpostRoom, { Since = 100; LastLooked = 9_000_000 } ]
                    }

                Expect.equal
                    (recheckedAt 100_000 ahead)
                    (Set.singleton outpostRoom)
                    "the look is owed on the first tick the gate is asked, not nine million ticks out"

                Expect.equal
                    (shutAt 100_000 ahead)
                    (Set.singleton outpostRoom)
                    "and the room is withheld from the work through it, as on every other tick"

                // The leaf corrects with the gate: the fold stamps the tick the look
                // actually fell due on over the impossible one.
                let healed = ahead |> raidTick 100_000 (quiet |> ownedByRival outpostRoom)

                Expect.equal
                    healed.RivalHeld
                    (Map.ofList [ outpostRoom, { Since = 100; LastLooked = 100_000 } ])
                    "the impossible stamp is written over by the look that was taken, the date standing"

                Expect.isEmpty
                    (recheckedAt 100_001 healed)
                    "so the stride runs from the real look and the room is not re-read every tick"
            }

            test "a clocked stand-down is never re-checked, and an empty log never looks" {
                // The recheck belongs to the latch alone: a clocked stand-down's own
                // clock takes the room back, and scanning it early would cost a room
                // read for an answer nothing acts on.
                let clocked = RaidState.empty |> raidTick 100 (seen [ core outpostRoom (Some 900) ])

                Expect.equal
                    (shutAt 800 clocked)
                    (Set.singleton outpostRoom)
                    "the core's clock is still running"

                Expect.isEmpty
                    (recheckedAt (100 + Tuning.defaults.RivalRecheck) clocked)
                    "and the stride falls due on nothing: there is no latch here to question"

                Expect.isEmpty
                    (recheckedAt 5_100 RaidState.empty)
                    "an empty log looks into nothing on any tick"
            }

            test "an empty log withholds nothing" {
                // The colony's ordinary state: no outpost has ever held a core, so the
                // shell scans every room a human declared.
                Expect.isEmpty (shutAt 100 RaidState.empty) "nothing is recorded, nothing is shut"
            }
        ]

/// A colony that has withdrawn from `outpostRoom` (no layer of it), which the
/// world sees holding these hostiles: an observer's look, nothing of ours
/// standing there.
let private observedShut (hostiles: HostileInfo list) =
    { quiet with
        MinedRooms = Set.singleton outpostRoom
        Bank = { Available = 0; Capacity = 1_500 }
        World =
            { WorldView.empty with
                Rooms =
                    Map.ofList
                        [
                            outpostRoom,
                            {
                                Hostiles = hostiles
                                HostileFatigue = Map.empty
                                HostileCargo = Map.empty
                                RivalRamparts = Map.empty
                                InvaderCores = []
                                Towers = []
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
                        ]
            }
    }

/// The row a raid shut `outpostRoom` on at tick 100, running to 600.
let private shutOnRaid: RaidState =
    { RaidState.empty with
        Outposts =
            [
                {
                    RoomName = outpostRoom
                    Opened = 100
                    LastSeen = 100
                    Expiry = 600
                    Basis = StandDownBasis.InvaderRaid
                    Stronghold = false
                    Raiders = set [ "raid-0" ]
                    PlayerSince = None
                    Chased = false
                    Bounced = false
                    HolderUnread = false
                }
            ]
    }

[<Tests>]
let watchedShutTests =
    testList
        "a shut outpost is re-read off any vision of ours (#534)"
        [
            test
                "an observer's look at a raid nothing wins is a sighting: last seen now, the clock re-read" {
                let raid =
                    List.replicate 5 [ Attack; Attack; Attack; Attack; Move; Move; Move; Move ]
                    |> List.mapi (raiderIn outpostRoom)

                Expect.equal
                    (standDowns (raidTick 150 (observedShut raid) shutOnRaid))
                    [
                        outpostRoom,
                        100,
                        150,
                        150 + Engine.creepLifetime,
                        StandDownBasis.InvaderRaid
                    ]
                    "seen at 150, held to the raid's life"
            }

            test
                "a look that finds the raid one we win is a sighting too, and the stand-down stands" {
                let lone = [ raiderIn outpostRoom 0 [ Attack; Move ] ]

                Expect.equal
                    (standDowns (raidTick 150 (observedShut lone) shutOnRaid))
                    [ outpostRoom, 100, 150, 600, StandDownBasis.InvaderRaid ]
                    "last seen at 150, the clock its own"
            }

            test
                "a look that finds the raid one our own row wins and a ranger kites chases it: the row is shut out of the room, its kill is not (#569)" {
                // Four ATTACK on one MOVE: a ranger holds its range on it, on
                // road too.
                let lone = [ raiderIn outpostRoom 0 [ Attack; Attack; Attack; Attack; Move ] ]

                let shut =
                    { observedShut lone with
                        RaidedOutposts = Set.singleton outpostRoom
                    }

                Expect.equal
                    (standDown Tuning.defaults 151 (raidTick 150 shut shutOnRaid)).ChasedOutposts
                    (Set.singleton outpostRoom)
                    "the stand-down stands, and its raid is a ranger's chase"
            }

            test
                "a raid recalled through a blind tick is no sighting: the row is left as it stood (#569)" {
                let raider = raiderIn outpostRoom 0 [ Attack; Move ]

                let recalled =
                    { quiet with
                        Hostiles = [ raider ]
                        RecalledRaiders = [ raider ]
                    }

                Expect.equal
                    (standDowns (raidTick 150 recalled shutOnRaid))
                    [ outpostRoom, 100, 100, 600, StandDownBasis.InvaderRaid ]
                    "last seen at 100 still"
            }

            test "with no vision of the room at all, the row is left as it stood" {
                Expect.equal
                    (standDowns (raidTick 150 quiet shutOnRaid))
                    [ outpostRoom, 100, 100, 600, StandDownBasis.InvaderRaid ]
                    "nothing seen, nothing moved"
            }

            test
                "an observer's look at a loitering raider moves the row's last sighting and enrols nothing in the episode" {
                let loiterer = [ raiderIn outpostRoom 0 [ Attack; Move ] ]
                let state = raidTick 150 (observedShut loiterer) shutOnRaid

                Expect.equal
                    (state.Outposts |> List.map (fun row -> row.LastSeen))
                    [ 150 ]
                    "the row is last seen at 150"

                Expect.isEmpty
                    state.Episodes
                    "and the colony's raid log grows nothing off a room it shut"
            }

            test
                "a look that finds the room empty ends the chase in the row, and a blind tick after leaves it ended" {
                let chasing =
                    { shutOnRaid with
                        Outposts =
                            shutOnRaid.Outposts |> List.map (fun row -> { row with Chased = true })
                    }

                let chasedAt tick state =
                    (standDown Tuning.defaults tick state).ChasedOutposts

                let blind = raidTick 150 quiet chasing

                Expect.equal
                    (chasedAt 151 blind)
                    (Set.singleton outpostRoom)
                    "blind, the chase stands"

                let seenEmpty = raidTick 151 (observedShut []) blind
                Expect.isEmpty (chasedAt 152 seenEmpty) "seen empty, it ends"

                let blindAgain = raidTick 152 quiet seenEmpty

                Expect.isEmpty
                    (chasedAt 153 blindAgain)
                    "and an observer looking every other tick cannot bring it back"
            }
        ]

/// One stand-down row of `room`, standing from `opened` to `expiry`.
let private rowOf room opened expiry basis stronghold : OutpostEpisode =
    {
        RoomName = room
        Opened = opened
        LastSeen = opened
        Expiry = expiry
        Basis = basis
        Stronghold = stronghold
        Raiders = Set.empty
        PlayerSince = None
        Chased = false
        Bounced = false
        HolderUnread = false
    }

/// A colony beside the fixture's, home `W13S28`, declaring nothing.
let private neighbour =
    { quiet with
        Spatial =
            { quiet.Spatial with
                RoomName = Some "W13S28"
            }
    }

[<Tests>]
let worldDangerTests =
    testList
        "the world danger log: one record a room"
        [
            test "the merge keeps a stronghold row a stronghold, whichever colony saw the bunker" {
                // W16S26: a bunker under W15S28's row, a later raid under
                // another's. A merge that kept the longer row's bit alone would
                // let every chain walk back through the towers.
                let mother =
                    { RaidState.empty with
                        Outposts = [ rowOf "W16S26" 100 5_000 StandDownBasis.CollapseTimer true ]
                        RivalHeld = Map.ofList [ "W18S26", { Since = 900; LastLooked = 970 } ]
                        Holds =
                            Map.ofList
                                [
                                    "W16S27",
                                    {
                                        Holder = ReservationHolder.Rival
                                        Until = 4_000
                                    }
                                ]
                    }

                let child =
                    { RaidState.empty with
                        Outposts =
                            [
                                rowOf "W16S26" 10 50 StandDownBasis.RaidGone false
                                rowOf "W16S26" 900 6_000 StandDownBasis.InvaderRaid false
                            ]
                        RivalHeld = Map.ofList [ "W18S26", { Since = 950; LastLooked = 960 } ]
                        Holds =
                            Map.ofList
                                [
                                    "W16S27",
                                    {
                                        Holder = ReservationHolder.Rival
                                        Until = 4_500
                                    }
                                ]
                    }

                let merged = Danger.ofColonies Tuning.defaults 1_000 [ mother; child ]

                Expect.equal
                    ((gateAt 1_000 merged).Impassable, (gateAt 5_500 merged).Impassable)
                    (Set.singleton "W16S26", Set.singleton "W16S26")
                    "impassable to every colony, and past the bunker's own clock"

                Expect.equal
                    (merged.Rooms
                     |> Map.tryFind "W16S26"
                     |> Option.bind _.Down
                     |> Option.map (fun row -> row.Opened, row.Expiry, row.Basis, row.Stronghold))
                    (Some(100, 6_000, StandDownBasis.InvaderRaid, true))
                    "one row: the earliest opening, the max expiry and its basis, the OR of the bunker; the spent row is gone"

                Expect.equal
                    merged.Rivals
                    (Map.ofList [ "W18S26", { Since = 900; LastLooked = 960 } ])
                    "one latch: the earliest shut, the earliest look"

                Expect.equal
                    (merged.Rooms
                     |> Map.tryFind "W16S27"
                     |> Option.bind _.Hold
                     |> Option.map _.Until)
                    (Some 4_500)
                    "the later hold"
            }

            // The fight records, taken rooms and probes an arena seats in each
            // colony's log, merged into the world log.
            test
                "the merge lays each room's latest fight record, earliest taken tick and latest probe into the world log" {
                let fight seen = { Seen = seen; Raiders = Set.empty }

                let probe first =
                    {
                        Probe = $"probe-{first}"
                        FirstHit = first
                        Hits = 1
                        HitAt = None
                        SafeModeAt = None
                        Fate = Died(first + 4)
                        Expires = first + 1_000
                        Home = ""
                    }

                let mother =
                    { RaidState.empty with
                        Fought = Map.ofList [ "W17S25", fight 900 ]
                        Taken = Map.ofList [ "W18S26", 800 ]
                        Probes = Map.ofList [ "W18S26", probe 700 ]
                    }

                let child =
                    { RaidState.empty with
                        Fought = Map.ofList [ "W17S25", fight 950 ]
                        Taken = Map.ofList [ "W18S26", 850 ]
                        Probes = Map.ofList [ "W18S26", probe 750 ]
                    }

                let laid =
                    Danger.into
                        (Danger.ofColonies Tuning.defaults 1_000 [ mother; child ])
                        RaidState.empty

                Expect.equal
                    (laid.Fought, laid.Taken, laid.Probes |> Map.map (fun _ log -> log.FirstHit))
                    (Map.ofList [ "W17S25", fight 950 ],
                     Map.ofList [ "W18S26", 800 ],
                     Map.ofList [ "W18S26", 750 ])
                    "the latest record, the earliest taken tick, the latest probe"
            }

            test
                "a raid row one colony opens shuts the room for every colony, and one that does not price it leaves it be" {
                let squad = squadIn outpostRoom "Trepidimous"

                let _, opened =
                    (RaidState.empty, Danger.empty) |> dangerTick [ outpostRoom ] 100 squad

                let row = opened.Rooms |> Map.tryFind outpostRoom |> Option.bind _.Down

                Expect.isSome row "the declaring colony opens it"

                Expect.equal
                    ((gateAt 101 opened).Shut)
                    (Set.singleton outpostRoom)
                    "and any colony's gate reads it shut"

                let _, after =
                    (RaidState.empty, opened)
                    |> dangerTick
                        []
                        101
                        { neighbour with
                            Hostiles = squad.Hostiles
                            Bank = squad.Bank
                        }

                Expect.equal
                    (after.Rooms |> Map.tryFind outpostRoom |> Option.bind _.Down)
                    row
                    "a colony that sees the raid and does not price the room neither re-prices nor strikes it"
            }

            test
                "a stronghold sighted in a room whose raid row a colony was not handed merges into that row" {
                let squad = squadIn outpostRoom "Trepidimous"

                let _, opened =
                    (RaidState.empty, Danger.empty) |> dangerTick [ outpostRoom ] 100 squad

                let _, after =
                    (RaidState.empty, opened)
                    |> dangerTick
                        []
                        101
                        { neighbour with
                            InvaderCores = [ bunker outpostRoom (Some 90_000) 1 ]
                        }

                Expect.equal
                    (after.Rooms
                     |> Map.tryFind outpostRoom
                     |> Option.bind _.Down
                     |> Option.map (fun row -> row.Opened, row.Expiry, row.Stronghold))
                    (Some(100, 90_000, true))
                    "the raid's opening, the bunker's clock and its bit, in one row"

                Expect.equal
                    ((gateAt 102 after).Impassable)
                    (Set.singleton outpostRoom)
                    "impassable to every colony"
            }

            test
                "a squad bouncing on the border flips the outpost once a bounce in the world log too" {
                // The world keeps one row a room, so the earlier row the raid's
                // going ended is gone once the next opens: its bounce is read at
                // the opening.
                let bouncing =
                    [ 100..140 ]
                    |> List.scan
                        (fun state t ->
                            let room = if t % 2 = 0 then outpostRoom else raidRoom
                            state |> dangerTick [] t (squadIn room "Trepidimous"))
                        (RaidState.empty, Danger.empty)
                    |> List.tail
                    |> List.map snd

                Expect.equal
                    (List.zip [ 100..140 ] bouncing
                     |> List.filter (fun (t, danger) ->
                         t >= 102 && not ((gateAt (t + 1) danger).Shut.Contains outpostRoom)))
                    []
                    "the first return shuts it for RaidBounceTicks, however often the squad steps out"

                Expect.isTrue
                    (List.last bouncing
                     |> fun danger -> danger.Rooms.[outpostRoom].Down
                     |> Option.exists _.Bounced)
                    "one row, marked bounced"
            }

            test "the exit watch rides the log, and a room nothing holds leaves it" {
                let held =
                    ExitWatch.Held
                        {
                            Run = [ { X = 0; Y = 20 }; { X = 0; Y = 21 } ]
                            Exit = { X = 0; Y = 20 }
                            Left = 50
                            Leavers = []
                            Until = 150
                        }

                let watched = Danger.empty |> Danger.watching (Map.ofList [ outpostRoom, held ])

                Expect.equal
                    (Danger.watches watched)
                    (Map.ofList [ outpostRoom, held ])
                    "what a cold heap starts from"

                Expect.equal
                    (Danger.standing 120 watched, Danger.standing 150 watched)
                    (Set.singleton outpostRoom, Set.empty)
                    "danger while the exit is held"

                Expect.isEmpty
                    (watched |> Danger.watching Map.empty).Rooms
                    "the hold gone, the room leaves the log"

                let seen =
                    Danger.empty
                    |> Danger.watching (
                        Map.ofList
                            [
                                outpostRoom,
                                ExitWatch.Seen
                                    [
                                        {
                                            Id = "h-1"
                                            At = { X = 10; Y = 10 }
                                            Dies = 130
                                            Trip = 100
                                        }
                                    ]
                            ]
                    )

                Expect.equal
                    (Danger.standing 120 seen, Danger.standing 130 seen)
                    (Set.singleton outpostRoom, Set.empty)
                    "a sighting is danger while its body lives, as the watch reads it"
            }
        ]
