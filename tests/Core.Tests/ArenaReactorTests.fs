/// The sector Reactor in the arena (#469): its rules against
/// `screeps/mod-season5` `da59118`, and whether our live code holds W15S25's
/// against Shibdib's steal squad (`docs/research/shibdib-reactor-steal.md`).
module Fabot.Core.Tests.ArenaReactorTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests.Arena

let private reactorId = fst Errand.w15s25.Target
let private reactorAt = snd Errand.w15s25.Target
let private reactorTile = RoomPos.pos reactorAt
let private w15s25 (x: int) (y: int) = RoomPos.at "W15S25" { X = x; Y = y }
let private shibdib = Side.Player "Shibdib"
let private start = 900_000

/// W15S25 alone with its Reactor, nothing of ours deciding.
let private reactorRoom (owner: Side option) (thorium: int) (bodies: Body list) =
    arena start [ room "W15S25" |> withReactor reactorId reactorTile owner thorium ] [] bodies

/// The Reactor at the end of a run.
let private reactorOf (a: Arena) =
    match a.Rooms["W15S25"].Reactor with
    | Some reactor -> reactor
    | None -> failtest "W15S25 holds no Reactor"

/// What `n` ticks of one unbroken streak score (`reactor.roomObject.js`).
let private streakScore (n: int) =
    List.sumBy (fun work -> 1 + int (floor (log10 (float (1 + work))))) [ 0 .. n - 1 ]

/// SlothBot's `2M/1C` reactorClaimer.
let private claimerParts = parts [ Move, 2; BodyPart.Claim, 1 ]

/// SlothBot's longbow under its 2,300 cap, `10M/8RA/2H` (§3); the part
/// order is not on record, and MOVE first is assumed.
let private longbowParts = parts [ Move, 10; RangedAttack, 8; Heal, 2 ]

/// One resident R7 as the ranger row casts it: `21M 14R 7H`, MOVE first.
let private r7 = parts [ Move, 21; RangedAttack, 14; Heal, 7 ]

let private w15s24 (x: int) (y: int) = RoomPos.at "W15S24" { X = x; Y = y }
let private w14s25 (x: int) (y: int) = RoomPos.at "W14S25" { X = x; Y = y }
let private w15s28 (x: int) (y: int) = RoomPos.at "W15S28" { X = x; Y = y }

/// The rows the arena casts for W15S28: the fighting rows and the
/// re-claimer. Its economy is staffed live, and none of it is here.
let private fightingRows =
    [ "guard"; "ranger"; "brawler"; "medic"; "kiter"; "reserver" ]

/// W15S28 as it stands live at t898,728: RCL7, a 5,600 bank, Spawn3 at
/// 18,30 and Spawn8 at 17,35, the Storage at 17,29 holding the 21,885 T the
/// burn would spend, and one hauler standing for the economy so no supply
/// floor jumps the queue. Its errand worked (`Held = false`) in this world
/// only, and none of its three outposts declared: they are not on the way.
/// The chain north to the Reactor (W15S27, keeper room W15S26), keeper room
/// W15S24 the squad comes through, and keeper room W14S25 east. **No Source
/// Keeper stands in any keeper room**: our route is laid around their
/// lairs by the Atlas whether or not one stands, and SlothBot's squad is
/// scripted on a straight walk, which a keeper would only slow.
let private reactorWorld (reactor: Side option) (thorium: int) (bodies: Body list) =
    let home =
        room "W15S28"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5600
        |> withStructures
            [
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Thorium = 21_885
                }
            ]

    let hauler =
        body "hauler-898000-Spawn3" Side.Ours (parts [ Carry, 10; Move, 10 ]) (w15s28 20 30) None

    let w15s28 =
        { colony "W15S28" with
            Errands = [ { Errand.w15s25 with Held = false } ]
        }

    arena
        start
        [
            home
            room "W15S27"
            room "W15S26"
            room "W15S25" |> withReactor reactorId reactorTile reactor thorium
            room "W15S24"
            room "W14S25"
        ]
        [ w15s28 ]
        (hauler :: bodies)
    |> withCasts fightingRows

/// The #419 garrison: two resident R7s standing on the Reactor's ring.
let private residents =
    [
        body "ranger-897000-Spawn3" Side.Ours r7 (w15s25 43 7) None
        body "ranger-897001-Spawn8" Side.Ours r7 (w15s25 45 7) None
    ]

let private residentIds = residents |> List.map (fun b -> b.Id)

/// A reactorClaimer of SlothBot's starting on this tile.
let private claimer (id: string) (at: RoomPos) =
    body id shibdib claimerParts at (Some(TakeReactor reactorAt))

/// The steal squad (§3) in W15S24's south-east pocket, whose exits open
/// onto the Reactor's room six tiles from it: `longbows` longbows walking for
/// the Reactor and kiting whatever they meet, and `claimers` reactorClaimers
/// three to four tiles behind them, escorted.
let private stealSquad (longbows: int) (claimers: int) =
    [
        for i in 1..longbows ->
            body
                $"Longbow{i}"
                shibdib
                longbowParts
                (w15s24 (44 + i) 46)
                (Some(Sweep(w15s25 44 9, Nearest, 3)))
        for i in 1..claimers -> claimer $"Claimer{i}" (w15s24 48 (44 - i))
    ]

let private squadIds (longbows: int) (claimers: int) =
    stealSquad longbows claimers |> List.map (fun b -> b.Id)

/// The ticks a Reactor claim landed on, with who claimed.
let private claimsIn (trace: TickTrace list) =
    trace
    |> List.collect (fun t ->
        t.Events
        |> List.choose (function
            | ReactorClaimed(_, by) -> Some(t.Tick, by)
            | _ -> None))

/// The T the Reactor burnt over a run.
let private burnedIn (trace: TickTrace list) =
    trace
    |> List.sumBy (fun t ->
        t.Events
        |> List.filter (function
            | Burned _ -> true
            | _ -> false)
        |> List.length)

/// Whether one of these bodies of ours shot this target on some tick.
let private shotBy (shooters: string list) (target: string) (trace: TickTrace list) =
    trace
    |> List.exists (fun t ->
        t.Ours
        |> List.exists (function
            | RangedAttackCreep(n, h) -> List.contains n shooters && h = target
            | _ -> false))

/// The squad rows' casts over a run.
let private squadCasts (trace: TickTrace list) =
    trace
    |> List.collect (fun t ->
        t.Ours
        |> List.choose (function
            | SpawnCreep(_, _, name) when
                [ "brawler-"; "medic-"; "kiter-" ]
                |> List.exists (fun row -> name.StartsWith row)
                ->
                Some name
            | _ -> None))

[<Tests>]
let reactorPhysicsTests =
    testList
        "arena Reactor physics"
        [
            test
                "an owned Reactor burns 1 T a tick and scores 1 + floor(log10(1 + continuous work)) to its owner" {
                let final, trace = reactorRoom (Some Side.Ours) 100 [] |> run 12
                let reactor = reactorOf final

                Expect.equal reactor.Thorium 88 "twelve ticks, twelve T"
                Expect.equal reactor.LaunchTime (Some start) "launched on the first tick"
                // Work 0..8 scores 1, 9..11 scores 2.
                Expect.equal (Map.tryFind "fabot" final.Scores) (Some(9 + 3 * 2)) (describe trace)
                Expect.equal (streakScore 12) (9 + 3 * 2) "the helper agrees"
            }

            test "an unowned Reactor keeps its store and scores nobody" {
                let final, _ = reactorRoom None 50 [] |> run 5
                Expect.equal (reactorOf final).Thorium 50 "nothing burnt"
                Expect.equal (reactorOf final).LaunchTime None "never launched"
                Expect.isEmpty final.Scores "nobody scored"
            }

            test "the tick the store is empty clears the streak" {
                let final, _ = reactorRoom (Some Side.Ours) 3 [] |> run 5
                Expect.equal (reactorOf final).Thorium 0 "all three burnt"
                Expect.equal (reactorOf final).LaunchTime None "the dry tick cleared it"
                Expect.equal (Map.tryFind "fabot" final.Scores) (Some 3) "three ticks at 1"
            }

            test
                "a CLAIM body beside it takes the flag the tick it asks, the streak untouched, and scores from that tick" {
                let claimer =
                    body
                        "Claimer1"
                        shibdib
                        claimerParts
                        (w15s25 45 7)
                        (Some(Phases [ 5, Hold; 10_000, Do [ Act.ClaimReactor reactorId ] ]))

                let final, trace = reactorRoom (Some Side.Ours) 100 [ claimer ] |> run 10
                let reactor = reactorOf final

                Expect.equal reactor.Owner (Some shibdib) $"theirs\n{describe trace}"
                Expect.equal reactor.LaunchTime (Some start) "the streak survives the steal"

                // Ours ticks 0..4 at 1; theirs 5..8 at 1 and tick 9 at 2.
                Expect.equal
                    final.Scores
                    (Map.ofList [ "fabot", 5; "Shibdib", 6 ])
                    "the tick's burn is the new owner's"

                Expect.equal
                    (claimsIn trace |> List.tryHead)
                    (Some(5, "Claimer1"))
                    "the first claim is in the trace"
            }

            test "a claim from two tiles off, or with its CLAIM part dead, takes nothing" {
                let asks = Some(Do [ Act.ClaimReactor reactorId ])
                let far = body "Far1" shibdib claimerParts (w15s25 46 8) asks

                // CLAIM head first and 200 hits left: the CLAIM part is the one lost.
                let lamed =
                    { body
                          "Lamed1"
                          shibdib
                          (parts [ BodyPart.Claim, 1; Move, 2 ])
                          (w15s25 45 7)
                          asks with
                        Hits = 200
                    }

                let final, _ = reactorRoom (Some Side.Ours) 100 [ far; lamed ] |> run 3
                Expect.equal (reactorOf final).Owner (Some Side.Ours) "still ours"
            }

            test "Thorium poured in from beside it fills it to its 1,000 cap, the rest kept" {
                let courier =
                    { body
                          "Courier1"
                          shibdib
                          (parts [ Carry, 10; Move, 10 ])
                          (w15s25 45 7)
                          (Some(Do [ Act.TransferThorium reactorId ])) with
                        Thorium = 500
                    }

                let final, _ = reactorRoom None 800 [ courier ] |> run 1
                Expect.equal (reactorOf final).Thorium Engine.reactorCapacity "full"

                Expect.equal
                    (final.Bodies |> List.find (fun b -> b.Id = "Courier1")).Thorium
                    300
                    "the rest kept"
            }

            test
                "the view carries the Reactor's owner, store and continuous work as the shell files them" {
                let ten, _ = reactorWorld (Some Side.Ours) 500 residents |> run 10
                let view = viewOf ten "W15S28"

                Expect.equal
                    view.Reactors
                    [
                        {
                            Id = reactorId
                            Owner = ReactorOwner.Ours
                            Thorium = 490
                            ContinuousWork = 10
                        }
                    ]
                    "the Reactor's row"

                Expect.equal
                    (Map.tryFind reactorId view.Spatial.Owners)
                    (Some Ownership.Ours)
                    "the ownership the re-claim reads"

                Expect.equal (List.length view.Errands) 1 "the errand is worked"
            }

            test
                "a spawn of ours casts the rows the arena names, 3 ticks a part; the others never stand" {
                // No resident: the ranger row casts the garrison, 42 parts.
                let _, trace = reactorWorld (Some Side.Ours) 500 [] |> run 130

                let born =
                    trace
                    |> List.collect (fun t ->
                        t.Events
                        |> List.choose (function
                            | Born id -> Some(t.Tick, id)
                            | _ -> None))

                Expect.contains born (125, "ranger-900000-Spawn3") "42 parts, 126 ticks"

                Expect.all
                    born
                    (fun (_, id) -> not (id.StartsWith "worker-"))
                    "no economy row stands"

                match
                    standingAt 125 trace |> List.tryFind (fun s -> s.Id = "ranger-900000-Spawn3")
                with
                | Some s ->
                    Expect.equal (RoomPos.range s.At (w15s28 18 30)) (Some 1) "beside Spawn3"
                | None -> failtest $"the ranger stands\n{describe trace}"
            }
        ]

[<Tests>]
let reactorHoldTests =
    testList
        "arena W15S25 Reactor hold against Shibdib"
        [
            test
                "scenario 1: two R7 residents on the ring kill the escorted claimer before it claims, then both longbows, losing nothing" {
                let final, trace =
                    reactorWorld (Some Side.Ours) 0 (residents @ stealSquad 2 1) |> run 300

                let failure = describe trace

                // Measured: the claimer dies on t9 three off the Reactor, the
                // longbows on t16 and t24; the residents never drop below 4,048.
                for id in squadIds 2 1 do
                    Expect.isSome (diedOn id trace) $"{id} dies\n{failure}"

                Expect.isLessThan
                    (diedOn "Claimer1" trace).Value
                    (diedOn "Longbow1" trace).Value
                    "the claimer first"

                Expect.isEmpty (claimsIn trace) "no claim lands"
                Expect.equal (reactorOf final).Owner (Some Side.Ours) "the Reactor stays ours"

                for id in residentIds do
                    Expect.isNone (diedOn id trace) $"{id} lives\n{failure}"
            }

            test
                "scenario 2: the same with 500 T burning: all 300 ticks' score is ours; undefended, the claim lands on t11 and the rest is theirs" {
                let held, heldTrace =
                    reactorWorld (Some Side.Ours) 500 (residents @ stealSquad 2 1) |> run 300

                Expect.equal (burnedIn heldTrace) 300 "a T a tick"
                Expect.equal held.Scores (Map.ofList [ "fabot", streakScore 300 ]) "792, all ours"

                // The counterfactual: no garrison standing. The re-claimer the
                // row casts walks round the longbows' Reach (#472) and retakes
                // the flag on t212, trading it with Claimer1 three times until
                // it dies on t239; the two R7s it casts are born on t125 and
                // t131, still walking.
                let lost, lostTrace = reactorWorld (Some Side.Ours) 500 (stealSquad 2 1) |> run 300

                match claimsIn lostTrace with
                | (tick, "Claimer1") :: (retaken, by) :: _ ->
                    Expect.equal tick 11 "the claimer walks straight in"

                    Expect.equal
                        (retaken, by)
                        (212, "reserver-900000-Spawn8")
                        "and ours walks back in"

                    // Ticks 0–10 at 13, and six more of ours at 3 a tick.
                    Expect.equal
                        lost.Scores
                        (Map.ofList [ "fabot", 31; "Shibdib", streakScore 300 - 31 ])
                        "31 ours, 761 theirs: the streak is one, its owner changed"
                | claims -> failtest $"the claimer claims: {claims}\n{describe lostTrace}"

                Expect.equal (reactorOf lost).Owner (Some shibdib) "theirs at the end"
            }

            test
                "scenario 3, today: three longbows outmatch the largest ranger, the stand-down shuts the errand room, and the idle garrison dies in it" {
                let final, trace =
                    reactorWorld (Some Side.Ours) 500 (residents @ stealSquad 3 2) |> run 600

                let failure = describe trace

                // The gate: the raid one 8-block ranger loses, on the tick the
                // third longbow is seen, clocked to the raid's life.
                Expect.contains
                    (Observe.standDown Tuning.defaults final.Time final.Carried.Raids["W15S28"])
                        .Shut
                    "W15S25"
                    "the room is shut"

                // A shut room is no resident room, so no Fight is pooled and
                // no squad is cast for it.
                Expect.isEmpty (squadCasts trace) "no squad"

                // Unassigned (`NoneApplicable`) from the shut on: no shot, no
                // step, only the heal reflex. Measured: both die, t39 and t65.
                let afterShut = trace |> List.filter (fun t -> t.Tick >= 6)

                for id in residentIds do
                    Expect.isFalse
                        (shotBy [ id ] "Longbow1" afterShut
                         || shotBy [ id ] "Longbow2" afterShut
                         || shotBy [ id ] "Longbow3" afterShut)
                        $"{id} never shoots after the shut\n{failure}"

                    Expect.isSome (diedOn id trace) $"{id} dies in the room\n{failure}"

                // The claim lands on t11 and every T after it is theirs.
                match claimsIn trace with
                | (tick, _) :: _ ->
                    Expect.equal
                        final.Scores
                        (Map.ofList
                            [
                                "fabot", streakScore tick
                                "Shibdib", streakScore 500 - streakScore tick
                            ])
                        "13 ours, 1,379 theirs"
                | [] -> failtest $"a claim lands\n{failure}"

                Expect.equal (reactorOf final).Owner (Some shibdib) "theirs at the end"
            }

            // #469's defect, live: `Observe.raidDeadlines` weighs an errand
            // room's raid against the largest single ranger the bank buys
            // (`Quota.rangerBlocksReach`, 8 blocks), never the two-body
            // garrison standing there (14 blocks). Three longbows lose to the
            // pair (280 a tick and 168 heal against 240 and 72) and beat the
            // one body, so the gate shuts the room on t5, the residents fall
            // to `NoneApplicable` and stand under fire unshooting, and the
            // claimer takes the Reactor on t11 (the test above). With the
            // gate held open (Raid log cleared every tick, measured once) the
            // residents still price the raid one body at a time
            // (`Facts.outmatched`), kite off the ring to safe ground, let the
            // claimer land on t11, then kill both claimers (t32, t35); the
            // re-claimer retakes on t222: 847 ours, 545 theirs.
            ptest
                "scenario 3, as it should be: the garrison that wins three longbows together keeps the room and the Reactor" {
                let final, trace =
                    reactorWorld (Some Side.Ours) 500 (residents @ stealSquad 3 2) |> run 600

                let failure = describe trace
                Expect.isEmpty (claimsIn trace) $"no claim lands\n{failure}"
                Expect.equal (reactorOf final).Owner (Some Side.Ours) "ours"

                for id in residentIds do
                    Expect.isNone (diedOn id trace) $"{id} lives\n{failure}"
            }

            test
                "scenario 3, the squad it would have priced, scripted on both sides — our decide does not run: the duo catches three kiting longbows in the Reactor's corner" {
                // The catalogue's pick on the tick the raid stood whole: the
                // cheapest squad that wins standing (`Facts.pricedSquad`).
                let seen, _ =
                    reactorWorld (Some Side.Ours) 500 (residents @ stealSquad 3 2) |> run 5

                let view = viewOf seen "W15S28"

                let duo = squadCatalogue |> List.find (fun squad -> squad.Name = "duo")

                Expect.isTrue
                    (Facts.squadWins view "W15S25" duo.Members false)
                    "the duo wins standing"

                let fabot = Side.Player "fabot"

                let bodies =
                    [
                        yield
                            body
                                "brawler-1"
                                fabot
                                brawlerPattern.Block
                                (w15s25 42 12)
                                (Some(Chase(Nearest, None)))
                        yield
                            body
                                "medic-1"
                                fabot
                                medicPattern.Block
                                (w15s25 42 13)
                                (Some(Follow "brawler-1"))
                        for i in 1..3 ->
                            body
                                $"Longbow{i}"
                                shibdib
                                longbowParts
                                (w15s25 (43 + i) 4)
                                (Some(Kite(Nearest, 3)))
                    ]

                let _, trace = arena start [ room "W15S25"; room "W15S24" ] [] bodies |> run 300

                let failure = describe trace

                // Measured: t7, t20, t32. The Reactor stands in the room's
                // north-east corner, walled east, and the arena's kiter never
                // leaves by an exit: whether SlothBot's does is not on record.
                for i in 1..3 do
                    Expect.isSome (diedOn $"Longbow{i}" trace) $"Longbow{i} is caught\n{failure}"

                for id in [ "brawler-1"; "medic-1" ] do
                    Expect.isNone (diedOn id trace) $"{id} lives\n{failure}"
            }

            test
                "scenario 4: a claimer slipping in by the east exit draws both residents off the ring, who shoot it 13 tiles short of the Reactor" {
                let final, trace =
                    reactorWorld
                        (Some Side.Ours)
                        500
                        (residents @ stealSquad 2 0 @ [ claimer "Claimer1" (w14s25 3 35) ])
                    |> run 300

                let failure = describe trace

                Expect.isTrue
                    (shotBy residentIds "Claimer1" trace)
                    $"the residents shoot it\n{failure}"

                // Measured: dead on t19 at 45,19; the longbows on t51 and t69.
                match pathOf "Claimer1" trace |> List.tryLast with
                | Some(_, last) ->
                    Expect.isGreaterThan
                        (RoomPos.range last.At reactorAt |> Option.defaultValue 99)
                        10
                        "far short of the ring"
                | None -> failtest "the claimer stood"

                for id in [ "Claimer1"; "Longbow1"; "Longbow2" ] do
                    Expect.isSome (diedOn id trace) $"{id} dies\n{failure}"

                Expect.isEmpty (claimsIn trace) "no claim lands"
                Expect.equal final.Scores (Map.ofList [ "fabot", streakScore 300 ]) "every T ours"

                for id in residentIds do
                    Expect.isNone (diedOn id trace) $"{id} lives\n{failure}"
            }
        ]
