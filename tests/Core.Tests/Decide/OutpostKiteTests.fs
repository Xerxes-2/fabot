/// A ranger kiting a melee down in an outpost: the seats a kite takes, its
/// step, the chase it is called to, and the outpost shut on the raid.
module Fabot.Core.Tests.Decide.OutpostKiteTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.OutpostFixtures

/// The same blows on a MOVE a part: as quick as the ranger.
let private runnerAt pos =
    hostileIn "W1N2" pos (List.replicate 10 Move @ List.replicate 10 Attack)

/// Isolate093 beside it: eighteen parts at 180 hits, the two HEAL at its
/// tail all that still acts.
let private dyingHealerAt pos =
    { hostileIn "W1N2" pos (List.replicate 9 Move @ List.replicate 9 Heal) with
        Id = "Isolate093"
        Owner = "Trepidimous"
        Hits = 180
    }

/// A whole healer of the same side: twelve HEAL on twelve MOVE.
let private strongHealerAt pos =
    { hostileIn "W1N2" pos (List.replicate 12 Move @ List.replicate 12 Heal) with
        Id = "medic"
        Owner = "Trepidimous"
    }

/// The raided field with a one-wide dead end off its east side, (31..36, 44):
/// a pocket a ranger kited into is pinned in.
let private withDeadEnd (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Spatial =
            colony.Spatial
            |> withNeighbour
                "W1N2"
                { outpost with
                    Terrain =
                        TerrainGrid.ofList
                            [
                                for x in 20..30 do
                                    for y in 41..48 -> { X = x; Y = y }, Plain
                                for x in 31..36 -> { X = x; Y = 44 }, Plain
                            ]
                }
    }

/// The raided outpost on a wide field, x 5..44 and y 5..48 (its south row a
/// step off the exits), plain but for `swamp`: ground to run off a melee at
/// its pace for many ticks (#625).
let private withOpenField (swamp: Pos list) (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"
    let swamps = Set.ofList swamp

    { colony with
        Spatial =
            colony.Spatial
            |> withNeighbour
                "W1N2"
                { outpost with
                    Terrain =
                        TerrainGrid.ofList
                            [
                                for x in 5..44 do
                                    for y in 5..48 ->
                                        let tile = { X = x; Y = y }
                                        tile, (if swamps.Contains tile then Swamp else Plain)
                            ]
                }
    }

/// Whether the outpost's verdict is a Kite: the rangers standing in it kite
/// its raid down (`Facts.kitedDown`).
let private kiteVerdict (colony: ColonyView) =
    (Threats.briefOf (threatsOf colony (Atlas.ofView colony)) "W1N2").Verdict.Value = Answer.Kite

/// A melee a tile a tick on plain as a ranger is, and quicker on swamp:
/// twenty MOVE, its legs first, under ten ATTACK.
let private swiftAt pos =
    hostileIn "W1N2" pos (List.replicate 20 Move @ List.replicate 10 Attack)

/// The kite ground's approach band (#595), and what the first cut closed on
/// in its place: every walkable tile within three of the ground, off it.
let private approachOf colony =
    let atlas = Atlas.ofView colony
    let ground = groundOf colony

    let walkable =
        ground
        |> Seq.collect (fun tile -> Atlas.walkableWithinIn atlas "W1N2" 3 (RoomPos.pos tile))
        |> Seq.map (RoomPos.at "W1N2")
        |> Set.ofSeq

    (Threats.briefOf (threatsOf colony atlas) "W1N2").Laid.Approach.Value,
    Set.difference walkable ground

/// A ranger of `shots` RANGED_ATTACK on as many MOVE.
let private rangerOf name shots =
    creepWith name 0 0 (List.replicate shots RangedAttack @ List.replicate shots Move)

[<Tests>]
let seatTests =
    testList
        "who ends a raid soonest, and how many a kite seats (#596)"
        [
            test
                "a body's end is its walk to its range and its kill alone, its TOUGH and its life read" {
                let cripple = crippledAt { X = 25; Y = 44 }
                let ranger = rangerOf "r-1" 4

                let ends at body raid =
                    Facts.endsIn (kitedBy raid [ body, at ]) "W1N2" body

                Expect.equal
                    (ends { X = 22; Y = 44 } ranger [ cripple ])
                    (Some(0, 37))
                    "in range: 1,454 hits at 40 a tick"

                Expect.equal
                    (ends { X = 16; Y = 44 } ranger [ cripple ])
                    (Some(6, 37))
                    "nine tiles off: six to its range"

                Expect.equal
                    (ends { X = 22; Y = 44 } { ranger with TicksToLive = 30 } [ cripple ])
                    (Some(0, Engine.creepLifetime))
                    "a body that ages out before the kill never ends it"

                Expect.equal
                    (ends { X = 22; Y = 44 } ranger [ cripple; strongHealerAt { X = 26; Y = 44 } ])
                    (Some(0, Engine.creepLifetime))
                    "nor one the raid's heal out-heals"

                Expect.isNone (ends { X = 22; Y = 44 } ranger []) "no raider, nothing to end"

                let brute =
                    hostileIn
                        "W1N2"
                        { X = 25; Y = 44 }
                        (List.replicate 5 Tough @ List.replicate 10 Attack @ List.replicate 5 Move)

                let killOf raider =
                    ends { X = 22; Y = 44 } (rangerOf "r-1" 10) [ raider ]
                    |> Option.map snd
                    |> Option.defaultValue 0

                Expect.isGreaterThan
                    (killOf (boostedAt Tier3 brute))
                    (killOf brute)
                    "a T3 TOUGH takes longer to shoot through"
            }

            test
                "a kite seats the fewest that kill before the raider ages out, and those at the fight" {
                // 1,454 hits; each 10-RANGED ranger 100 a tick, four ticks off.
                let cripple =
                    { crippledAt { X = 25; Y = 44 } with
                        TicksToLive = 14
                    }

                let away name y = rangerOf name 10, { X = 18; Y = y }

                let seats raid rangers holding =
                    Facts.kiteSeats (kitedBy raid rangers) "W1N2" holding

                Expect.equal
                    (seats
                        [ { cripple with TicksToLive = 1500 } ]
                        [ away "r-1" 44; away "r-2" 43 ]
                        Set.empty)
                    1
                    "one kills it long before it ages out"

                Expect.equal
                    (seats [ cripple ] [ away "r-1" 44; away "r-2" 43; away "r-3" 45 ] Set.empty)
                    2
                    "alone it would age out first, two kill it in time, the third is not needed"

                Expect.equal
                    (seats
                        [ cripple ]
                        [ away "r-1" 44; away "r-2" 43; rangerOf "r-3" 2, { X = 22; Y = 44 } ]
                        Set.empty)
                    3
                    "a small one standing in range, last to end it alone, shoots at no walk's cost"

                Expect.equal
                    (seats
                        [ cripple ]
                        [ away "r-1" 44; away "r-2" 43; away "r-3" 45 ]
                        (set [ "r-3" ]))
                    3
                    "one holding the kite keeps its seat while it dances out past its range"

                Expect.equal
                    (seats
                        [ cripple ]
                        [ away "r-1" 44; away "r-2" 43; rangerOf "r-3" 10, { X = 14; Y = 44 } ]
                        (set [ "r-3" ]))
                    2
                    "but not once it is far off"

                Expect.equal (seats [] [ away "r-1" 44 ] Set.empty) 1 "no raider: one"
            }

            // The kill orders one Task's bodies, never one body's Tasks: in the
            // key a raid no body kills alone would price every candidate a
            // lifetime's kill and send it to any other Task of its rank.
            test "the kill seats a Task's bodies and leaves each body's key its walk" {
                let raid = [ crippledAt { X = 25; Y = 44 } ]

                let rangers =
                    [
                        rangerOf "r-weak" 2, { X = 22; Y = 44 }
                        rangerOf "r-strong" 10, { X = 20; Y = 44 }
                    ]

                let world = kitedBy raid rangers

                Expect.equal
                    (rangers |> List.map (fun (r, _) -> Facts.endsIn world "W1N2" r))
                    [ Some(0, 73); Some(2, 15) ]
                    "the weak one in range ends it at t73, the strong one two tiles off at t17"

                let guard = taskId (Guard "W1N2")
                let verbose = set [ "r-weak"; "r-strong" ]

                let result = decide world Map.empty verbose None

                let worldCosts =
                    result.Verdicts
                    |> List.choose (function
                        | Verdict.Scoring(name, rows) ->
                            rows
                            |> List.tryPick (function
                                | Candidate.Scored(tid, _, cost, _) when tid = guard ->
                                    Some(name, cost)
                                | _ -> None)
                        | _ -> None)

                let assigned = result.Assignments

                Expect.equal
                    worldCosts
                    [ "r-weak", 0; "r-strong", 4 ]
                    "each key's cost is its walk alone, in half-ticks, no kill in it"

                for name in verbose do
                    Expect.equal (Map.tryFind name assigned) (Some guard) $"{name} takes the kite"
            }
        ]

[<Tests>]
let kiteTests =
    testList
        "a melee that cannot close on a ranged body (#510)"
        [
            test
                "a crippled melee threatens a step and a swing about it, and the same body on its legs the room's margin" {
                let at = { X = 25; Y = 44 }

                let reachOf colony =
                    Threats.reachIn (threatsOf colony (Atlas.ofView colony)) "W1N2"

                Expect.equal
                    (reachOf (declaredRaid [ crippledAt at ]))
                    (set
                        [
                            for dx in -2 .. 2 do
                                for dy in -2 .. 2 -> { X = 25 + dx; Y = 44 + dy }
                        ])
                    "with its fatigue spent it steps once and swings, then waits seventeen ticks for the next step"

                Expect.isTrue
                    (reachOf (declaredRaid [ { crippledAt at with Hits = 3_500 } ])
                     |> Set.contains { X = 25; Y = 41 })
                    "the whole body walks, so its Reach keeps the margin"
            }

            // A hostile's strength is its live parts and the parts the healing
            // reaching it restores in the reaction window (#531), replacing the
            // Brief's S7 rule 1 (any healer within three read it on paper).
            test
                "a cripple beside a dying healer is read near its live parts, beside a whole one near its paper" {
                let at = { X = 25; Y = 44 }
                let crunchy = crunchyAt at

                let reachOf colony =
                    Threats.reachIn (threatsOf colony (Atlas.ofView colony)) "W1N2"

                let weak = [ crunchy; dyingHealerAt { X = 26; Y = 44 } ]
                let strong = [ crunchy; strongHealerAt { X = 26; Y = 44 } ]

                Expect.isTrue
                    (HostileInfo.stationary weak crunchy)
                    "two HEAL restore too little in the window: it still cannot walk"

                Expect.equal
                    (reachOf (declaredRaid weak) |> Set.filter (fun tile -> range tile at <= 2))
                    (reachOf (declaredRaid [ crunchy ]))
                    "so it threatens a step and a swing about it, as alone"

                Expect.equal
                    (HostileInfo.effectiveParts strong crunchy)
                    crunchy.Body
                    "twelve HEAL restore every part within the window: its paper"

                Expect.isFalse (HostileInfo.stationary strong crunchy) "and it walks"
            }

            test "beside a dying healer a ranger out-shooting its two HEAL kites the cripple down" {
                let crunchy = crunchyAt { X = 25; Y = 44 }
                let healer = dyingHealerAt { X = 26; Y = 44 }

                // Two ranger blocks, 40 a tick against the healer's 24.
                let colony =
                    { declaredRaid [ crunchy; healer ] with
                        Bank = bank 1400 1400
                    }
                    |> withGuards
                        [
                            creepWith "r-1" 0 0 (bodyFor Bodies.rangerPattern 1400),
                            { X = 22; Y = 44 }
                        ]

                Expect.equal (Facts.answer colony "W1N2") Answer.Kite "a ranger kites it down"

                let result = decide colony Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "and the ranger takes the room"

                // The recast cooldown (#576) holds a Win alone: a Kite after a
                // loss casts and holds as it would.
                let lost = decide (lostIn "W1N2" (colony.Time - 1) colony) Map.empty Set.empty None

                Expect.equal
                    (lost.Quotas, spawnIntents lost.Intents, Map.tryFind "r-1" lost.Assignments)
                    (result.Quotas,
                     spawnIntents result.Intents,
                     Map.tryFind "r-1" result.Assignments)
                    "a ranger just lost there moves nothing of a Kite"

                Expect.isTrue
                    (groundOf colony
                     |> Set.exists (fun tile -> rangeTo (RoomPos.pos crunchy.Pos) tile <= 3))
                    "on ground in range of the cripple, not kited off the room"
            }

            // #615 U2: the exchange's Kites is a win for every set of bodies,
            // priced on their damage together.
            test "two rangers together kite down a slow melee whose healer out-heals each alone" {
                let melee =
                    { hostileIn "W1N2" { X = 25; Y = 44 } [ Move; Attack; Attack; Attack ] with
                        Id = "melee"
                        Owner = "Trepidimous"
                    }

                // Six HEAL, 72 a tick: one 2-block ranger's 40 never kills.
                let healer =
                    { hostileIn
                          "W1N2"
                          { X = 26; Y = 44 }
                          (List.replicate 6 Move @ List.replicate 6 Heal) with
                        Id = "healer"
                        Owner = "Trepidimous"
                    }

                let colony =
                    { declaredRaid [ melee; healer ] with
                        Bank = bank 1400 1400
                    }
                    |> withGuards
                        [
                            creepWith "r-1" 0 0 (bodyFor Bodies.rangerPattern 1400),
                            { X = 22; Y = 44 }
                            creepWith "r-2" 0 0 (bodyFor Bodies.rangerPattern 1400),
                            { X = 22; Y = 45 }
                        ]

                Expect.equal
                    (Facts.answer colony "W1N2", (Facts.verdictOf colony "W1N2").Rangers.Value)
                    (Answer.Win, Some 2)
                    "the two rangers' 80 outruns the 72: won by both, not stood down"
            }

            test
                "one ranger that kites a raid down alone is asked once, at the fewest blocks that kite it" {
                let melee =
                    { hostileIn "W1N2" { X = 25; Y = 44 } [ Move; Attack; Attack; Attack ] with
                        Id = "melee"
                        Owner = "Trepidimous"
                    }

                let healer =
                    { hostileIn
                          "W1N2"
                          { X = 26; Y = 44 }
                          (List.replicate 6 Move @ List.replicate 6 Heal) with
                        Id = "healer"
                        Owner = "Trepidimous"
                    }

                let colony =
                    { declaredRaid [ melee; healer ] with
                        Bank = bank 5600 5600
                    }

                // What `rangersWanted` and `rangerBlocksFor` read: the biggest
                // body the bank buys kites it alone, so one is asked, sized at
                // the fewest blocks that do.
                let kites = blocksBeat rangerPattern Stance.Standoff colony "W1N2"

                Expect.equal
                    (kites (Facts.rangerBlocksReach colony), kites 3, kites 4)
                    (true, false, true)
                    "four blocks' 80 outruns the healer's 72, three blocks' 60 does not"
            }

            // #615 U2: a hybrid as quick as a ranger picks the range, and
            // against a body that deals as much from three it closes to swing
            // beside its shot. #651: its heal drops its swing (`intents.js`),
            // so it is priced at the act worth more, never both: before, its
            // 70 a tick beside its 36 heal outlasted the pair.
            test
                "a quick hybrid closes on rangers to swing beside its shot, and two 2-block rangers win it: it heals or swings, never both" {
                let hybrid =
                    { hostileIn
                          "W1N2"
                          { X = 25; Y = 44 }
                          (List.replicate 8 Move
                           @ List.replicate 4 RangedAttack
                           @ [ Attack ]
                           @ List.replicate 3 Heal) with
                        Id = "hybrid"
                        Owner = "Trepidimous"
                    }

                // A 1,400 bank buys 2-block rangers, 40 a tick and 24 heal each.
                let colony =
                    { declaredRaid [ hybrid ] with
                        Bank = bank 1400 1400
                    }

                Expect.isTrue
                    (Facts.rowAnswer colony "W1N2").Ranger
                    "its 36 heal beside its 40 shot (or its 70 unhealed) loses to the pair's 80: the ranger row"
            }

            // A part's boost multiplies what it does, as the
            // engine's `calcBodyEffectiveness` and `_applyDamage` do.
            test
                "a T3 6T22R10M12H is priced at its boosts: ×4 its shot, heal and MOVE, its TOUGH taking 0.3" {
                let plain = hostileIn "W1N2" { X = 25; Y = 25 } kalgenBody
                let t3 = boostedAt Tier3 plain

                let shot h =
                    HostileInfo.effectivePower [ h ] h RangedAttack

                Expect.equal (shot plain, shot t3) (22, 88) "22 RANGED_ATTACK shoot as 88"

                Expect.equal
                    (HostileInfo.healing [ plain ] plain, HostileInfo.healing [ t3 ] t3)
                    (144, 576)
                    "12 HEAL heal 576 a tick, not 144"

                let plainPace h =
                    Gait.ticks Gait.plain (HostileInfo.gait [ h ] h)

                Expect.equal
                    (plainPace plain, plainPace t3)
                    (Some 4, Some 1)
                    "10 MOVE under 40 weighted parts: a tile in four ticks on plain, boosted in one"

                Expect.equal
                    (HostileInfo.effectiveHits plain, HostileInfo.effectiveHits t3)
                    (5_000, 6_400)
                    "its 600 hits of TOUGH cost 2,000 damage"

                Expect.equal
                    (HostileInfo.damageTaken t3 600, HostileInfo.damageTaken t3 3_000)
                    (180, 1_600)
                    "a 600 tower shot takes 180; 3,000 takes the TOUGH's 600 and 1,000 behind it"

                Expect.equal
                    (HostileInfo.damageFor t3 576)
                    1_920
                    "so a tick of its heal costs 1,920 of our damage to undo"

                // 3,600 a tick: 2,200 through the TOUGH and 576 back leave
                // 3,376; then 3,600 in full leaves 352; the third kills.
                Expect.equal
                    ([ 2; 3 ] |> List.map (HostileInfo.killedWithin t3 (fun _ -> 3_600) 576))
                    [ false; true ]
                    "under 3,600 a tick its TOUGH is paid for once: dead on the third tick"

                let worn = { t3 with Hits = 4_400 }

                Expect.equal
                    (HostileInfo.damageTaken worn 600, HostileInfo.effectiveHits worn)
                    (600, 4_400)
                    "with its TOUGH shot off, a shot lands in full"
            }

            test "a body whose gun is shot off and heals itself is unarmed within a short window" {
                let wraith = wraithAt "W1N2" { X = 25; Y = 44 }

                Expect.equal
                    (Engine.liveParts wraith.Body wraith.Hits)
                    [ Move; Heal; Move ]
                    "the premise: MOVE, HEAL, MOVE acting"

                Expect.isFalse
                    (HostileInfo.partsWithin 2 [ wraith ] wraith |> List.contains RangedAttack)
                    "two ticks of its own 12 a tick leave its gun off"

                Expect.isTrue
                    (HostileInfo.partsWithin 3 [ wraith ] wraith |> List.contains RangedAttack)
                    "the third brings it back"
            }

            // A heal acts on one target a tick (#531): a side's healing is
            // spent once, on the body short of the most hits in its reach.
            test "one healer's heal restores the most hurt of two cripples, never both" {
                let crunchy = crunchyAt { X = 25; Y = 44 }

                let lighter =
                    { crunchyAt { X = 27; Y = 44 } with
                        Id = "lighter"
                        Hits = 2_500
                    }

                let medic = strongHealerAt { X = 26; Y = 44 }
                let side = [ crunchy; lighter; medic ]

                Expect.equal
                    (HostileInfo.effectiveParts side crunchy)
                    crunchy.Body
                    "the most hurt takes the whole heal: its paper"

                Expect.equal
                    (HostileInfo.effectiveParts side lighter)
                    (Engine.liveParts lighter.Body lighter.Hits)
                    "the other none: its live parts"
            }

            test
                "a healer whose HEAL is shot off heals nothing and is no healer, whatever comes back" {
                // HEAL at the head: 100 hits leave the MOVE at the tail alone.
                let worn =
                    { hostileIn "W1N2" { X = 25; Y = 44 } [ Heal; Heal; Move ] with
                        Id = "worn"
                        Owner = "Trepidimous"
                        Hits = 100
                    }

                // A whole healer beside it restores its HEAL inside the window.
                let side = [ worn; strongHealerAt { X = 26; Y = 44 } ]

                Expect.isTrue
                    (HostileInfo.effectiveParts side worn |> List.contains Heal)
                    "the premise: its HEAL comes back in the window"

                Expect.equal (HostileInfo.healing side worn) 0 "it heals nothing now"

                Expect.isFalse
                    (HostileInfo.heals side worn)
                    "and is read as no healer by the same parts"
            }

            // The kite ground off the maps (movement S3, which retired its
            // step-and-swing cut and `holdReach`; `stepAndSwing` stays for the
            // defender's line, S4's): the cripple, rested, takes one
            // step before its seventeen-tick wait, so a tile at two is struck
            // the tick after and the ring at three is what is left.
            test "a ranger is matched to kill it from range three, and the melee guard is not sent" {
                let at = { X = 25; Y = 44 }

                let colony =
                    declaredRaid [ crippledAt at ]
                    |> withGuards
                        [ ranger "r-1", { X = 22; Y = 47 }; guard "g-1", { X = 20; Y = 41 } ]

                let result = decide colony Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the ranger takes the room's Guard"

                Expect.notEqual
                    (Map.tryFind "g-1" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the guard it out-swings is never walked into its reach"

                Expect.contains
                    result.Intents
                    (RangedAttackCreep("r-1", "h-1"))
                    "and shoots from where it stands"

                Expect.equal
                    (guardQuotaOf colony)
                    (Some 0)
                    "nor does the guard row cast a melee body for it"

                let ground = groundOf colony

                Expect.isNonEmpty ground "the premise: the Guard has ground"

                Expect.isTrue
                    (ground |> Set.forall (fun tile -> rangeTo at tile >= 3 && rangeTo at tile <= 3))
                    "its ground is in range of the melee and out of its step and swing"
            }

            // Movement S3 flipped the S0 pin here ("the kite ground is the same
            // for a lamed, tired ranger as for a quick one"): the room's ground
            // is still one set, but which of its tiles a ranger may step onto
            // is its own walk's (`Threats.kiteUnsafeFor`, #524): a tile the
            // melee strikes before this ranger, its legs, its parts shot off
            // and its fatigue counted, could be on it and gone.
            test "a lamed, tired ranger may not step onto a kite tile a quick one may" {
                let at = { X = 25; Y = 44 }
                let from = { X = 21; Y = 47 }
                let ring = { X = 22; Y = 47 }
                let quick = ranger "r-1"

                let lamed =
                    { quick with
                        Body = Map.add Move 1 quick.Body
                        Fatigue = 20
                    }

                let unsafeWith body =
                    let colony = declaredRaid [ plodderAt at ] |> withGuards [ body, from ]

                    let atlas = Atlas.ofView colony

                    match Threats.kiteUnsafeFor colony (threatsOf colony atlas) atlas body None with
                    | Some(unsafe, _) -> unsafe (ring.X * Engine.roomSide + ring.Y)
                    | None -> failtest "the premise: its walk reads the plodder's map"

                Expect.isFalse
                    (unsafeWith quick)
                    "the quick ranger is on the ring at three and off it before the plodder's step and swing"

                Expect.isTrue
                    (unsafeWith lamed)
                    "one MOVE acting under its shot-off weight and twenty fatigue to pay: struck first"
            }

            // #615 U1: a raid quicker than the body picks the standoff, so the
            // body closes to its reach, but only where it wins the fight
            // (`Resolver.closes`). A scratched ranger (MOVE first, so its
            // first wound lames it) the admission let in by a Lose's call or
            // a `rangers > 1` Win keeps its kite walk through the ticks its
            // heal takes to stand its MOVE back up.
            test "a lamed ranger closes on a quicker melee only where it wins the fight" {
                let at = { X = 25; Y = 44 }
                let from = { X = 21; Y = 47 }
                let quick = ranger "r-1"

                let lamed =
                    { quick with
                        Body = Map.add Move 1 quick.Body
                        Hits =
                            { quick.Hits with
                                Hits = quick.Hits.HitsMax - 200
                            }
                    }

                let judgedAt capacity hostile partners =
                    let colony =
                        { declaredRaid [ hostile ] with
                            Bank = bank capacity capacity
                        }
                        |> withGuards ((lamed, from) :: partners)

                    let atlas = Atlas.ofView colony
                    let threats = threatsOf colony atlas
                    let assigned = Map.ofList [ "r-1", Guard "W1N2" ]

                    (Threats.briefOf threats "W1N2").Verdict.Value,
                    Threats.standsOffNothing colony threats atlas lamed None,
                    Resolver.closes colony threats assigned lamed

                // The haul home's 700 bank.
                let judged = judgedAt 700

                let verdict, nothing, closes = judged (runnerAt at) []
                Expect.equal verdict Answer.Lose "the premise: nothing of ours wins the runner"
                Expect.isTrue nothing "the premise: the runner is the quicker on plain"
                Expect.isFalse closes "under a Lose it keeps its kite walk"

                // A bank that buys no fighter, so the room's answer is the
                // bodies standing in it: since #654 the 700 buys a pair of
                // rangers that trades the 2A melee down (a Win; the pair was
                // weighed only where no swing caught a raider).
                let verdict, nothing, closes =
                    judgedAt
                        300
                        (hostileIn "W1N2" at (List.replicate 2 Move @ List.replicate 2 Attack))
                        [
                            creepWith "r-2" 0 0 (bodyFor Bodies.rangerPattern 1950),
                            { X = 20; Y = 36 }
                        ]

                // Moved by #625: the 2A melee is at a ranger's pace and its
                // first MOVE shot off lames it, so the room's answer is the
                // Kite where it was a Win.
                Expect.equal verdict Answer.Kite "the premise: the 2A melee's room is a kite"

                Expect.isTrue nothing "the premise: still the quicker"
                Expect.isFalse closes "a partner holding no Guard beside it is no reason to close"

                let verdict, nothing, closes =
                    judged (hostileIn "W1N2" at [ Move; Move; Attack ]) []

                Expect.notEqual verdict Answer.Lose "the premise: the light melee is no Lose"
                Expect.isTrue nothing "the premise: it is the quicker on plain"
                Expect.isTrue closes "one it wins alone it closes on"
            }

            test "the speed and not the paper picks the target" {
                let withRanger hostile =
                    declaredRaid [ hostile ] |> withGuards [ ranger "r-1", { X = 22; Y = 48 } ]

                let assigned colony =
                    (decide colony Map.empty Set.empty None).Assignments |> Map.tryFind "r-1"

                Expect.equal
                    (assigned (withRanger (plodderAt { X = 25; Y = 44 })))
                    (Some(taskId (Guard "W1N2")))
                    "360 a swing, behind the ranger on road and plain, is kited"

                Expect.notEqual
                    (assigned (withRanger (runnerAt { X = 25; Y = 44 })))
                    (Some(taskId (Guard "W1N2")))
                    "the same blows on legs as quick as the ranger's are no chase"

                // #613 flipped this from "keeps the ranger's pace on a road":
                // a road it ties on is the kite walk's to keep off, tile by
                // tile, and read here it ruled Zero3956 (15 MOVE live under 17
                // ATTACK, a step every second tick on plain) as quick as a
                // ranger, which then never took the raid it out-paces.
                Expect.equal
                    (assigned (
                        withRanger (
                            hostileIn
                                "W1N2"
                                { X = 25; Y = 44 }
                                (List.replicate 5 Move @ List.replicate 10 Attack)
                        )
                    ))
                    (Some(taskId (Guard "W1N2")))
                    "ten ATTACK on five MOVE, slower on plain and quicker nowhere, is kited"
            }

            // #625 (W17S26 t1,021,484, Wolf0965 18M 17A at our rangers'
            // pace): stepping at once, a pursuer at the kiter's pace never
            // closes while the kiter has a tile to step onto that it reaches
            // first, and it needs that ground only until its shots lame the
            // pursuer (`Facts.keepsOff`): eleven ticks of one hundred a tick
            // take the eleventh MOVE off the swift melee, which then falls
            // behind on plain.
            test
                "an equal-pace melee is kited down where the ranger has run-off ground until it is lamed" {
                let shooter = rangerOf "r-1" 10

                let kitedDownFrom ground (melee: HostileInfo) (at: Pos) =
                    kitedBy [ melee ] [ shooter, at ] |> ground |> kiteVerdict

                let open' = swiftAt { X = 25; Y = 25 }

                Expect.isFalse
                    (Gait.fallsBehind
                        (Fighter.ofCreep shooter).Gait
                        (HostileInfo.gait [ open' ] open'))
                    "the premise: a tile a tick on plain, as the ranger"

                Expect.isTrue
                    (kitedDownFrom (withOpenField []) open' { X = 25; Y = 28 })
                    "in open ground it is kept off and kited down"

                let colony = kitedBy [ open' ] [ shooter, { X = 25; Y = 28 } ] |> withOpenField []

                Expect.equal
                    ((decide colony Map.empty Set.empty None).Assignments |> Map.tryFind "r-1")
                    (Some(taskId (Guard "W1N2")))
                    "and the ranger takes the Guard"

                Expect.isFalse
                    (kitedDownFrom withDeadEnd (swiftAt { X = 31; Y = 44 }) { X = 34; Y = 44 })
                    "its back to the dead end's wall, three steps of ground short of the lame: not kited"

                Expect.isFalse
                    (kitedDownFrom
                        (withOpenField [])
                        (swiftAt { X = 25; Y = 45 })
                        { X = 25; Y = 48 })
                    "its back to the room's edge: not kited"

                let swamp =
                    [
                        for x in 5..44 do
                            for y in 29..48 -> { X = x; Y = y }
                    ]

                Expect.isFalse
                    (kitedDownFrom (withOpenField swamp) open' { X = 25; Y = 28 })
                    "its back to swamp the melee crosses quicker: not kited"
            }

            // #654: Trepidimous' 18M 17A (MOVE first, 3,500 hits, no heal) in
            // an outpost of an 1,800 bank. The cast is the cheapest set that
            // ends it by the exchange, and every body of it is one the gate
            // seats; in open ground the ranger kites it down, its back to a
            // dead end it does not.
            test
                "an equal-pace melee is cast for by the cheapest set that ends it, which fits and kites it" {
                let trep (at: Pos) =
                    { hostileIn "W1N2" at (List.replicate 18 Move @ List.replicate 17 Attack) with
                        Owner = "Trepidimous"
                    }

                let castIn ground (at: Pos) =
                    let colony =
                        { declaredRaid [ trep at ] with
                            Bank = bank 1800 1800
                        }
                        |> ground

                    colony, Facts.fighterCast colony "W1N2" bodyTemplates Engine.guardCap

                let colony, cast = castIn (withOpenField []) { X = 25; Y = 25 }

                // A ranger lames the MOVE-first body before the field's
                // run-off runs out (#625), and kites it down: no set trades
                // it, so the verdict is the Kite (`rowAnswer`), answered by
                // the rangers standing by. Moved by #656: the fight's ticks
                // cost the outpost's held source (`Contest.Kept`), which the
                // chooser read as none on a room's first asks a tick, so two
                // blocks killing in half the ticks are the cheaper set (it
                // was one block).
                Expect.equal
                    (cast.Template, cast.Bodies, cast.Blocks, cast.Ends, cast.Trades)
                    (BodyTemplate.Ranged, 1, 2, true, false)
                    "in open ground the cheapest set is one two-block ranger, which kites it down"

                let bodies =
                    [
                        for i in 1 .. cast.Bodies ->
                            creepWith
                                $"ranger-{i}"
                                0
                                0
                                (bodyFor
                                    (patternOfTemplate cast.Template)
                                    (cast.Blocks * bodyCost (patternOfTemplate cast.Template).Block))
                    ]

                for body in bodies do
                    let beside =
                        bodies
                        |> List.filter (fun other -> other.Name <> body.Name)
                        |> List.map (fun other -> other, true, Some 0)

                    Expect.isTrue
                        (Facts.fits
                            colony
                            Stance.Standoff
                            "W1N2"
                            colony.Tuning.ReachHorizon
                            false
                            body
                            beside)
                        $"{body.Name} is a body the gate seats"

                let kitedFrom ground (at: Pos) (from: Pos) =
                    { kitedBy [ trep at ] (bodies |> List.map (fun body -> body, from)) with
                        Bank = bank 1800 1800
                    }
                    |> ground
                    |> kiteVerdict

                Expect.isTrue
                    (kitedFrom (withOpenField []) { X = 25; Y = 25 } { X = 25; Y = 28 })
                    "and standing three off it, it kites it down"

                // Its back to the dead end's wall, no set the bank buys ends
                // it: the ranger has no ground to be lamed on, and the melee
                // holding its post loses it.
                let _, walled = castIn withDeadEnd { X = 31; Y = 44 }

                Expect.isFalse walled.Ends "with no run-off ground nothing the bank buys ends it"
            }

            // #655 (live t~1,035,974: Tension5531, 18M17A, held W17S26 while
            // W17S25 asked no fight, no ranger alive, its guards idle): the
            // Kite verdict counted a ranger the colony did not have, and a
            // kite asked a cast only of a room shut on its raid. A verdict
            // that counts a body casts it: the row's cheapest ranged set.
            test
                "an equal-pace melee with no ranger standing by casts the ranger its Kite counts, which takes the Guard" {
                let trep =
                    { hostileIn
                          "W1N2"
                          { X = 25; Y = 25 }
                          (List.replicate 18 Move @ List.replicate 17 Attack) with
                        Owner = "Trepidimous"
                    }

                let colony =
                    { declaredRaid [ trep ] with
                        Bank = bank 1800 1800
                    }
                    |> withOpenField []

                Expect.equal
                    (Facts.answer colony "W1N2")
                    Answer.Kite
                    "the premise: the verdict is the Kite"

                let rangers (decision: Decision) =
                    spawnIntents decision.Intents
                    |> List.map (fun (_, body, _) -> body)
                    |> List.filter (List.contains RangedAttack)

                let decision = decideOn colony

                Expect.equal
                    (rangers decision |> List.length)
                    1
                    $"one ranger is cast for the kite\n{spawnIntents decision.Intents}"

                // The body cast, landed at home: it fits and takes the Guard.
                let cast = creepWith "ranger-cast" 0 0 (rangers decision |> List.head)

                let landed = colony |> withGuards [ cast, { X = 25; Y = 47 } ]

                Expect.equal
                    (Map.tryFind "ranger-cast" (decideOn landed).Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "and the ranger takes the room's Guard"
            }

            // #625: the lame window is read off the live body head first, as
            // the engine destroys it. The same twenty parts with the MOVE
            // first fall behind on plain once one MOVE is shot off; with the
            // ATTACK first they press the ranger until all ten are, a hundred
            // ticks at ten a tick, longer than the field's run-off.
            test "a melee whose MOVE parts come first falls behind after the lame window" {
                let shooter = rangerOf "r-1" 1
                let movesFirst = runnerAt { X = 25; Y = 25 }

                let attacksFirst =
                    hostileIn
                        "W1N2"
                        { X = 25; Y = 25 }
                        (List.replicate 10 Attack @ List.replicate 10 Move)

                let kited (melee: HostileInfo) =
                    kitedBy [ melee ] [ shooter, { X = 25; Y = 28 } ]
                    |> withOpenField []
                    |> kiteVerdict

                let lamed =
                    { movesFirst with
                        Hits = movesFirst.Hits - Engine.partHits
                    }

                Expect.isTrue
                    (Gait.fallsBehind
                        (Fighter.ofCreep shooter).Gait
                        (HostileInfo.gait [ lamed ] lamed))
                    "one MOVE shot off its head, it falls behind on plain"

                Expect.isTrue (kited movesFirst) "so ten ticks of ground keep it off"

                Expect.isFalse
                    (kited attacksFirst)
                    "its ATTACK first, it presses the ranger far longer"
            }

            // #625: run-off is ground no class that strikes the ranger
            // before it strikes back reaches first. A second melee of
            // another pace south of the ranger shuts the ground the swift
            // one north of it leaves, though each is lamed in a tick or so.
            test
                "an equal-pace melee is not kited with a second melee class across the ranger's run-off" {
                let shooter = rangerOf "r-1" 10
                let north = swiftAt { X = 25; Y = 25 }

                // Eleven MOVE under ten ATTACK: a tile a tick on plain, three
                // on swamp where the swift melee takes two.
                let south =
                    { hostileIn
                          "W1N2"
                          { X = 25; Y = 31 }
                          (List.replicate 11 Move @ List.replicate 10 Attack) with
                        Id = "south"
                    }

                let kited raid =
                    kitedBy raid [ shooter, { X = 25; Y = 28 } ] |> withOpenField [] |> kiteVerdict

                Expect.isTrue (kited [ north ]) "the premise: the swift melee alone is kited"
                Expect.isTrue (kited [ south ]) "the premise: so is the other alone"
                Expect.isFalse (kited [ north; south ]) "the two from either side are not"
            }

            // #625: a pursuer quicker than the ranger is kept off only where
            // the ranger lames it before it closes the two tiles between
            // their reaches: at three ticks a tile against its two, twelve.
            test "a quicker melee is kited where the ranger lames it before it closes" {
                // Nine RANGED_ATTACK on three MOVE: three ticks a tile, 90 a tick.
                let slow =
                    creepWith "r-1" 0 0 (List.replicate 9 RangedAttack @ List.replicate 3 Move)

                // Its own fit (`Facts.fits`): the row's rangers out-pace the
                // melee on plain and would kite it whatever this one does.
                let kited parts =
                    let colony =
                        kitedBy
                            [ hostileIn "W1N2" { X = 25; Y = 25 } parts ]
                            [ slow, { X = 25; Y = 28 } ]
                        |> withOpenField []

                    Facts.fits colony Stance.Standoff "W1N2" 1 true slow []

                Expect.isTrue
                    (kited (List.replicate 6 Move @ List.replicate 10 Attack))
                    "its MOVE first, three shot off in four ticks make it the slower"

                Expect.isFalse
                    (kited (List.replicate 10 Attack @ List.replicate 6 Move))
                    "its ATTACK first, it closes before the twelve ticks its swing takes to fall"
            }

            // #625: one ranger shoots one target a tick, so pursuers at its
            // pace are lamed in turn: the second presses it for both turns.
            test "two melees at the ranger's pace press it for both their lame windows" {
                let shooter = rangerOf "r-1" 10

                let quick id pos =
                    { hostileIn "W1N2" pos (List.replicate 11 Move @ List.replicate 10 Attack) with
                        Id = id
                    }

                let kited raid =
                    kitedBy raid [ shooter, { X = 34; Y = 44 } ] |> withDeadEnd |> kiteVerdict

                Expect.isTrue
                    (kited [ quick "a" { X = 31; Y = 44 } ])
                    "one lamed in two ticks, inside the dead end's three"

                Expect.isFalse
                    (kited [ quick "a" { X = 31; Y = 44 }; quick "b" { X = 30; Y = 44 } ])
                    "the second waits its turn: four ticks, past the three"
            }

            // #613: one fitness answer takes and keeps the Guard. The brief
            // admits a ranger that kites the raid down; #602's release read
            // the exchange alone, so a kiter the melee once caught lost the
            // Guard it was taken for, walked off, and was admitted again.
            test "a ranger kiting the raid down keeps its Guard the tick the melee strikes it" {
                let colony =
                    declaredRaid [ plodderAt { X = 25; Y = 44 } ]
                    |> withGuards [ ranger "r-1", { X = 24; Y = 44 } ]

                let held = Map.ofList [ "r-1", taskId (Guard "W1N2") ]

                Expect.equal
                    ((decide colony held Set.empty None).Assignments |> Map.tryFind "r-1")
                    (Some(taskId (Guard "W1N2")))
                    "struck beside the melee, it loses the exchange on paper and still kites it down"
            }

            test "the kite ground keeps out of a dead end the melee can pin the ranger in" {
                let at = { X = 28; Y = 44 }
                let colony = declaredRaid [ plodderAt at ] |> withDeadEnd
                let ground = groundOf colony

                Expect.isNonEmpty ground "the premise: there is open ground to kite on"

                Expect.isFalse
                    (ground |> Set.exists (fun tile -> tile.X > 30))
                    "the corridor's mouth is three from the melee, and a step down it is a step it cannot come back up"

                Expect.isTrue
                    (ground |> Set.forall (fun tile -> rangeTo at tile >= 3))
                    "and nothing in it is in the melee's step and swing"
            }

            // #595 review: the band a kiter with no safe walk onto its ground
            // closes on is the ground's own tests one to three past it. The
            // first cut read it as the walkable tiles near the ground, which
            // held the tiles within three of the melee the ground drops for
            // want of escape room, the north row's corner and the corridor's
            // mouth: a pocket the melee pins the kiter in.
            test "the approach band keeps out of the pockets the kite ground keeps out of" {
                let at = { X = 28; Y = 44 }
                let band, near = approachOf (declaredRaid [ plodderAt at ] |> withDeadEnd)

                let pocket (tile: RoomPos) =
                    rangeTo at tile <= Engine.rangedRange || tile.X > 30

                Expect.exists
                    near
                    pocket
                    "the premise: the walkable tiles near the ground hold a pocket"

                Expect.isNonEmpty band "there is a band to close by"

                Expect.isFalse
                    (band |> Set.exists pocket)
                    "and none of it is a pocket the ground drops"
            }

            test "the approach band holds no exit tile" {
                let at = { X = 25; Y = 3 }
                let colony = declaredRaid [ plodderAt at ]
                let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

                let colony =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withNeighbour
                                "W1N2"
                                { outpost with
                                    Terrain =
                                        TerrainGrid.ofList
                                            [
                                                for x in 15..35 do
                                                    for y in 0..14 -> { X = x; Y = y }, Plain
                                            ]
                                }
                    }

                let band, near = approachOf colony
                let exit (tile: RoomPos) = Seam.onRing (RoomPos.pos tile)

                Expect.exists
                    near
                    exit
                    "the premise: the walkable tiles near the ground hold exit tiles"

                Expect.isNonEmpty band "there is a band to close by"

                Expect.isFalse
                    (band |> Set.exists exit)
                    "and none of it is an exit tile, which carries the body out of the room"
            }

            // A kiter's exit camp, read by its own timing since movement S3
            // (`Threats.kiteUnsafeFor`): a step and a swing about the held exit,
            // a step more for each tick past the first the tile holds the body.
            // A rested ranger on plain keeps the S0 pin's two.
            test
                "the chase crosses the border at an exit three or more from the one the target left by" {
                // The raid left the home by (25,0) and waits across it; the ranger
                // follows it into W1N2 by another exit tile.
                let home =
                    declaredRaid [ plodderAt { X = 25; Y = 44 } ]
                    |> withBodyAtHome (ranger "r-1") { X = 25; Y = 3 }

                let wide =
                    { home with
                        Spatial =
                            home.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Terrain =
                                        TerrainGrid.ofList
                                            [
                                                for x in 15..35 do
                                                    for y in 1..9 -> { X = x; Y = y }, Plain
                                            ]
                                })
                        ExitHolds =
                            Map.ofList
                                [
                                    "W1N1",
                                    {
                                        Run = [ for x in 1..48 -> { X = x; Y = 0 } ]
                                        Exit = { X = 25; Y = 0 }
                                        Until = home.Time + 50
                                    }
                                ]
                    }

                let result = decide wide Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the premise: the ranger chases the target into the outpost"

                let step =
                    moveIntents result.Intents |> List.tryFind (fst >> (=) "r-1") |> Option.map snd

                Expect.isSome step "it walks"

                Expect.isFalse
                    (List.contains step.Value [ TopLeft; Top; TopRight ])
                    "and never onto a tile within two of the exit the target left by"
            }
        ]

/// The raided outpost floored out to its borders, and the colony harassing
/// W1N3 beyond it, its ranger standing quiet there on the enemy's source.
/// Moved by #658: the sweep works a candidate only where it has something to
/// kill, so W1N3 holds the enemy's miner in the sweep's memory
/// (`WorldView.PreySeen`), none in sight: its Guard is the ranger's quiet one.
let private harassingBeyond hostiles =
    let colony = declaredRaid hostiles
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    let remembered =
        { hostileIn "W1N3" { X = 25; Y = 44 } [ Work; Work; Move ] with
            Id = "miner-w1n3"
            Owner = "Trepidimous"
        }

    let colony =
        { colony with
            World =
                { colony.World with
                    PreySeen =
                        Map.ofList
                            [
                                "W1N3",
                                {
                                    Bodies = [ remembered ]
                                    Tick = colony.Time
                                }
                            ]
                }
        }

    let floor =
        TerrainGrid.ofList
            [
                for x in 1..48 do
                    for y in 1..48 -> { X = x; Y = y }, Plain
            ]

    { colony with
        Creeps = colony.Creeps @ [ ranger "r-1" ]
        Spatial =
            { colony.Spatial with
                Borders = colony.Spatial.Borders |> Map.add "W1N3" plainRing
            }
            |> withNeighbour "W1N2" { outpost with Terrain = floor }
            |> withNeighbour
                "W1N3"
                { RoomLayer.empty with
                    Terrain = floor
                    CreepPositions = Map.ofList [ "r-1", { X = 25; Y = 45 } ]
                }
    }
    |> casting
        {
            RoomName = "W1N3"
            Enemy = "Trepidimous"
            Stand = RoomPos.at "W1N3" { X = 25; Y = 44 }
            Controller = RoomPos.at "W1N3" { X = 30; Y = 30 }
            Via = []
            Blocks = None
        }

[<Tests>]
let chaseCallTests =
    testList
        "a quiet ranger called to a chase (#510)"
        [
            // Moved by #658: the sweep's room is never quiet; its remembered
            // miner is laid in the view (one model for its price, its fight
            // and its cast), so r-1 holds a kill there, which a chase does not
            // call it off (it was a quiet harassment room's Guard, called).
            test "a ranger holding the sweep's kill keeps it against a chase within two rooms" {
                let held = Map.ofList [ "r-1", taskId (Guard "W1N3") ]

                let quiet = decide (harassingBeyond []) held Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" quiet.Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "the premise: with nothing to chase it keeps its own Guard"

                let called =
                    decide (harassingBeyond [ crippledAt { X = 25; Y = 10 } ]) held Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" called.Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "a crippled melee a walk away calls it off no kill"
            }

            test "a chase already held calls nobody, whichever order the holders are walked in" {
                // r-1 is walked before r-9, which holds the chase: read against the
                // ones kept so far, the slot looked free to r-1 every tick, and it
                // was let go and took its own Guard back, tick after tick.
                let colony =
                    harassingBeyond [ crippledAt { X = 25; Y = 10 } ]
                    |> standingIn "W1N2" ("r-9", { X = 25; Y = 14 })

                let colony =
                    { colony with
                        Creeps = colony.Creeps @ [ ranger "r-9" ]
                    }

                let held = Map.ofList [ "r-1", taskId (Guard "W1N3"); "r-9", taskId (Guard "W1N2") ]

                let result = decide colony held Set.empty None

                Expect.isFalse
                    (result.Verdicts
                     |> List.exists (function
                         | Verdict.Released _ -> true
                         | _ -> false))
                    "nobody is released"

                Expect.equal
                    (Map.tryFind "r-1" result.Assignments, Map.tryFind "r-9" result.Assignments)
                    (Some(taskId (Guard "W1N3")), Some(taskId (Guard "W1N2")))
                    "and each keeps what it held"
            }
        ]

[<Tests>]
let transitStrikeTests =
    testList
        "a Guard's holder walking through another room (#563)"
        [
            // Live t986,014 (W17S26): a ranger holding W17S25's Guard walked
            // past Trepidimous' melee three off and struck it nothing but a
            // mass attack's 2, its kill order standing in the Guard's room.
            test "strikes by the weapon reflex what it may strike and reaches there" {
                // W1N3, a room that is none of ours: the body stands at
                // (25,25), a raider of the declared enemy at `pos`.
                let held raid body pos =
                    let colony = declaredRaid raid
                    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

                    let floor =
                        TerrainGrid.ofList
                            [
                                for x in 1..48 do
                                    for y in 1..48 -> { X = x; Y = y }, Plain
                            ]

                    let colony =
                        { colony with
                            Creeps = colony.Creeps @ [ body ]
                            Hostiles =
                                raid
                                @ [
                                    { hostileIn "W1N3" pos [ Move; Carry ] with
                                        Id = "passer"
                                        Owner = "Trepidimous"
                                    }
                                ]
                            Spatial =
                                { colony.Spatial with
                                    Borders = colony.Spatial.Borders |> Map.add "W1N3" plainRing
                                }
                                |> withNeighbour "W1N2" { outpost with Terrain = floor }
                                |> withNeighbour
                                    "W1N3"
                                    { RoomLayer.empty with
                                        Terrain = floor
                                        CreepPositions =
                                            Map.ofList [ body.Name, { X = 25; Y = 25 } ]
                                    }
                        }

                    decide colony (Map.ofList [ body.Name, taskId (Guard "W1N2") ]) Set.empty None

                // The ranger answers a plodder's raid, the melee guard a
                // smallMelee's.
                let shot = held [ plodderAt { X = 25; Y = 44 } ] (ranger "r-1") { X = 25; Y = 27 }

                Expect.equal
                    (Map.tryFind "r-1" shot.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the premise: the ranger holds the outpost's Guard on its way"

                Expect.contains
                    shot.Intents
                    (RangedAttackCreep("r-1", "passer"))
                    "and shoots the declared enemy two off on its way"

                let swung = held raiders (guard "g-1") { X = 25; Y = 26 }

                Expect.equal
                    (Map.tryFind "g-1" swung.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the premise: the melee guard holds it too"

                Expect.contains
                    swung.Intents
                    (AttackCreep("g-1", "passer"))
                    "and swings at the one beside it"
            }
        ]

/// The raided field widened to x 15..35, y 35..48, room for a ranger to come
/// at the melee from the north.
let private withField (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Spatial =
            colony.Spatial
            |> withNeighbour
                "W1N2"
                { outpost with
                    Terrain =
                        TerrainGrid.ofList
                            [
                                for x in 15..35 do
                                    for y in 35..48 -> { X = x; Y = y }, Plain
                            ]
                }
    }

/// Where one step in a direction lands.
let private stepped (pos: Pos) (direction: Direction) =
    let dx, dy =
        match direction with
        | Top -> 0, -1
        | TopRight -> 1, -1
        | Right -> 1, 0
        | BottomRight -> 1, 1
        | Bottom -> 0, 1
        | BottomLeft -> -1, 1
        | Left -> -1, 0
        | TopLeft -> -1, -1

    { X = pos.X + dx; Y = pos.Y + dy }

[<Tests>]
let kiteStepTests =
    testList
        "a kiting ranger's step (#510)"
        [
            // A kiter walks round what a melee strikes before it could be on it
            // and gone (movement S3, `Threats.kiteUnsafeFor`), not its Reach:
            // against the plodder that is the S0 pin's step and swing.
            test
                "it walks onto the ring three from the melee, inside its Reach, and shoots from there" {
                let melee = { X = 25; Y = 44 }
                let from = { X = 24; Y = 40 }

                let at pos =
                    declaredRaid [ plodderAt melee ]
                    |> withField
                    |> withGuards [ ranger "r-1", pos ]

                let first = decide (at from) Map.empty Set.empty None

                let step =
                    moveIntents first.Intents
                    |> List.tryFind (fst >> (=) "r-1")
                    |> Option.map (snd >> stepped from)

                Expect.equal
                    (step |> Option.map (range melee))
                    (Some 3)
                    "the Reach reaches the ring; only a step and a swing about the melee is kept off"

                let held = Map.ofList [ "r-1", taskId (Guard "W1N2") ]
                let second = decide (at step.Value) held Set.empty None

                Expect.contains
                    second.Intents
                    (RangedAttackCreep("r-1", "h-1"))
                    "and from the ring it shoots"
            }

            // Movement S3 review: the arbitration shoves a body only onto its own
            // candidates, so a kiter on its ring at three offers none a melee
            // strikes before it is off again: a higher-rank chain never pushes
            // it to two.
            test "a kiter on its ring offers the arbitration no tile in the melee's step and swing" {
                let melee = { X = 25; Y = 44 }
                let from = { X = 25; Y = 41 }

                let colony =
                    declaredRaid [ plodderAt melee ]
                    |> withField
                    |> withGuards [ ranger "r-1", from ]

                let atlas = Atlas.ofView colony
                let threats = threatsOf colony atlas

                let movement =
                    movementOf
                        colony
                        atlas
                        threats
                        []
                        (Map.ofList [ "r-1", Guard "W1N2" ])
                        Map.empty
                        Set.empty

                match movement.Intents |> List.tryFind (fun intent -> intent.Creep = "r-1") with
                | Some intent ->
                    Expect.equal
                        (List.tryHead intent.Candidates)
                        (Some(RoomPos.at "W1N2" from))
                        "the premise: it stays"

                    Expect.all
                        intent.Candidates
                        (fun tile -> range melee (RoomPos.pos tile) >= 3)
                        "and is offered nothing within two of the melee"
                | None -> failtest "it has an intent"
            }

            // Movement S3 (#524): a ring tile is one this ranger is off again
            // before the melee strikes it. Swamp holds it five ticks, past the
            // plodder's fourth: the swamp in front is walked round to a plain
            // ring tile it is on in two ticks.
            test "it walks round the swamp on the ring to a plain ring tile" {
                let melee = { X = 25; Y = 44 }
                let from = { X = 25; Y = 40 }
                let swamp = [ for x in 24..26 -> { X = x; Y = 41 } ]
                let colony = declaredRaid [ plodderAt melee ] |> withField
                let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

                let colony =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withNeighbour
                                "W1N2"
                                { outpost with
                                    Terrain =
                                        TerrainGrid.ofList
                                            [
                                                for x in 15..35 do
                                                    for y in 35..48 ->
                                                        let tile = { X = x; Y = y }

                                                        tile,
                                                        (if List.contains tile swamp then
                                                             Swamp
                                                         else
                                                             Plain)
                                            ]
                                }
                    }
                    |> withGuards [ ranger "r-1", from ]

                let step =
                    moveIntents (decideOn colony).Intents
                    |> List.tryFind (fst >> (=) "r-1")
                    |> Option.map (snd >> stepped from)

                Expect.isSome step "it walks"

                Expect.isFalse
                    (List.contains step.Value swamp)
                    "never onto the swamp three off the melee"

                Expect.isGreaterThan
                    (range melee step.Value)
                    (Engine.meleeRange + 1)
                    "nor into its step and swing"
            }

            // The user's chase (live 2026-10-07): a ranger that steps onto
            // swamp is held there five ticks, and its target walks out of
            // reach. Its step is weighed by when it strikes the target next,
            // its own gait and the target's steps counted (`chaseStep`): across
            // a two-row swamp band from a shooter, it shoots from the plain
            // where it stands, steps along the plain back into reach, and
            // crosses the band only where no plain tile reaches it.
            test
                "a ranger chasing across a swamp band keeps to the plain tiles that keep its target in reach" {
                let band =
                    set
                        [
                            for x in 15..35 do
                                for y in 41..42 -> { X = x; Y = y }
                        ]

                let from = { X = 25; Y = 43 }

                let decided (target: Pos) =
                    let colony = declaredRaid [ hostileIn "W1N2" target smallRanged ] |> withField
                    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

                    let colony =
                        { colony with
                            Spatial =
                                colony.Spatial
                                |> withNeighbour
                                    "W1N2"
                                    { outpost with
                                        Terrain =
                                            TerrainGrid.ofList
                                                [
                                                    for x in 15..35 do
                                                        for y in 35..48 ->
                                                            let tile = { X = x; Y = y }

                                                            tile,
                                                            (if Set.contains tile band then
                                                                 Swamp
                                                             else
                                                                 Plain)
                                                ]
                                    }
                        }
                        |> withGuards
                            [
                                creepWith
                                    "r-1"
                                    0
                                    0
                                    (List.concat (List.replicate 5 Bodies.rangerPattern.Block)),
                                from
                            ]

                    let decision =
                        decide colony (Map.ofList [ "r-1", taskId (Guard "W1N2") ]) Set.empty None

                    decision.Intents,
                    moveIntents decision.Intents
                    |> List.tryFind (fst >> (=) "r-1")
                    |> Option.map (snd >> stepped from)

                let intents, step = decided { X = 28; Y = 40 }
                Expect.contains intents (RangedAttackCreep("r-1", "h-1")) "in reach it shoots"

                Expect.isNone
                    step
                    "and stands on the plain, where main stepped onto the band at (26,42)"

                let _, step = decided { X = 29; Y = 40 }

                Expect.equal
                    step
                    (Some { X = 26; Y = 43 })
                    "a step out of reach, it steps along the plain back into it, not onto the band"

                let _, step = decided { X = 25; Y = 39 }

                Expect.isTrue
                    (step |> Option.exists (fun tile -> Set.contains tile band))
                    "four off, where no plain tile reaches it, it crosses the band"
            }

            // #607: the swamp check weighed only the kill order's head, but
            // the light tick shoots whatever it reaches first (`pickStrike`).
            // Head four off across the band, a second two off on its edge: a
            // step onto the band beside the second held the body five ticks,
            // from which the second, walking off, is out of its reach. It
            // weighs every target it strikes next, and keeps the second in
            // reach from the plain.
            test
                "a ranger shooting a second target keeps it in reach rather than step onto swamp toward the head" {
                let band =
                    set
                        [
                            for x in 15..35 do
                                for y in 41..42 -> { X = x; Y = y }
                        ]

                let from = { X = 25; Y = 43 }

                let decided (headAt: Pos) (secondAt: Pos) =
                    // The head is the hurt one, first of the kill order, its
                    // MOVE still whole.
                    let head =
                        { hostileIn "W1N2" headAt smallRanged with
                            Hits = 800
                        }

                    let second =
                        { hostileIn "W1N2" secondAt smallRanged with
                            Id = "h-2"
                        }

                    let colony = declaredRaid [ head; second ] |> withField
                    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

                    let colony =
                        { colony with
                            Spatial =
                                colony.Spatial
                                |> withNeighbour
                                    "W1N2"
                                    { outpost with
                                        Terrain =
                                            TerrainGrid.ofList
                                                [
                                                    for x in 15..35 do
                                                        for y in 35..48 ->
                                                            let tile = { X = x; Y = y }

                                                            tile,
                                                            (if Set.contains tile band then
                                                                 Swamp
                                                             else
                                                                 Plain)
                                                ]
                                    }
                        }
                        |> withGuards
                            [
                                creepWith
                                    "r-1"
                                    0
                                    0
                                    (List.concat (List.replicate 5 Bodies.rangerPattern.Block)),
                                from
                            ]

                    let decision =
                        decide colony (Map.ofList [ "r-1", taskId (Guard "W1N2") ]) Set.empty None

                    let step =
                        moveIntents decision.Intents
                        |> List.tryFind (fst >> (=) "r-1")
                        |> Option.map (snd >> stepped from)

                    step, decision.Intents

                let step, intents = decided { X = 25; Y = 39 } { X = 23; Y = 41 }

                Expect.contains
                    intents
                    (RangedAttackCreep("r-1", "h-2"))
                    "it shoots the second, in reach"

                Expect.isFalse
                    (step |> Option.exists (fun tile -> Set.contains tile band))
                    $"and steps onto no swamp, which would lose it: {step}"
            }

            // Movement S3 retired `glueStep`'s `struck` (a second melee beside
            // the target's tile): the full tick reads the maps
            // (`Emitter.glueKeeps`), the tile kept where it is struck no harder
            // by the tick after than where the guard stands.
            test "a melee guard keeps to its target's tile unless the maps strike it harder there" {
                let target = { X = 25; Y = 44 }
                let from = { X = 24; Y = 44 }
                let body = guard "g-1"

                let keeps second =
                    let colony =
                        declaredRaid (plodderAt target :: second)
                        |> withField
                        |> withGuards [ body, from ]

                    let atlas = Atlas.ofView colony

                    Emitter.glueKeeps
                        (threatsOf colony atlas)
                        body
                        (RoomPos.at "W1N2" from)
                        (RoomPos.at "W1N2" target)

                Expect.isTrue (keeps []) "alone, its target strikes both tiles alike"

                let beyond =
                    { plodderAt { X = 27; Y = 44 } with
                        Id = "h-2"
                    }

                Expect.isFalse
                    (keeps [ beyond ])
                    "a second melee a step from the target's tile and two from the guard's strikes it there first"
            }

            // #615 U1 flipped the U0 quirk pin ("the glue counts a guard's
            // HEAL beside its swing"): the glue reads `Fighter.HealAt`,
            // nothing in a tick it swings. U3 took `Matcher`'s `struckNow`
            // (`healPower × HEAL`) out with the struck gate it served.
            test "the glue, the exchange and the engine give a swinging guard no heal" {
                let target = { X = 25; Y = 44 }
                let from = { X = 24; Y = 44 }

                // A light melee two tiles past the target strikes its tile
                // and not the guard's: 30 more there than the target's 360.
                let beyond =
                    { hostileIn "W1N2" { X = 27; Y = 44 } [ Move; Attack ] with
                        Id = "h-2"
                    }

                let healerGuard heals =
                    creepWith "g-1" 0 0 ([ Tough; Move; Attack ] @ List.replicate heals Heal)

                let keeps body =
                    let colony =
                        declaredRaid [ plodderAt target; beyond ]
                        |> withField
                        |> withGuards [ body, from ]

                    let atlas = Atlas.ofView colony

                    Emitter.glueKeeps
                        (threatsOf colony atlas)
                        body
                        (RoomPos.at "W1N2" from)
                        (RoomPos.at "W1N2" target)

                Expect.isFalse (keeps (healerGuard 1)) "one HEAL's 12 does not cover the 390 there"

                Expect.isFalse
                    (keeps (healerGuard 33))
                    "nor does 33 HEAL's 396: a heal would drop its swing"

                let fighter = Fighter.ofCreep (healerGuard 33)

                Expect.equal
                    (fighter.HealAt(Fighter.bestRange fighter))
                    0
                    "the exchange heals a melee body nothing where it fights: its heal would drop its attack"
            }

            // #614: the glue was the one chaser's step no chase weighed. A
            // guard of five MOVE to five other parts is held five ticks on
            // swamp, from which a target walking off is out of its swing.
            test
                "a melee guard never glues onto its target's swamp tile it would be held on while the target walks off" {
                let target = { X = 25; Y = 44 }
                let from = { X = 24; Y = 44 }
                let body = guard "g-1"

                let glued (ground: Terrain) =
                    let colony = declaredRaid [ hostileIn "W1N2" target smallRanged ] |> withField
                    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

                    let colony =
                        { colony with
                            Spatial =
                                colony.Spatial
                                |> withNeighbour
                                    "W1N2"
                                    { outpost with
                                        Terrain =
                                            TerrainGrid.ofList
                                                [
                                                    for x in 15..35 do
                                                        for y in 35..48 ->
                                                            let tile = { X = x; Y = y }

                                                            tile,
                                                            (if tile = target then ground else Plain)
                                                ]
                                    }
                        }
                        |> withGuards [ body, from ]

                    let atlas = Atlas.ofView colony
                    let threats = threatsOf colony atlas
                    let assigned = Map.ofList [ "g-1", Guard "W1N2" ]

                    Emitter.glue
                        colony
                        atlas
                        threats
                        assigned
                        (Resolver.glueChases colony atlas threats assigned)

                Expect.equal
                    (glued Plain)
                    [ MoveCreep("g-1", Right) ]
                    "the premise: onto its target's plain tile"

                Expect.isEmpty (glued Swamp) "never onto its swamp tile: it stands and swings"
            }

            // #615 U1: the glue is the standoff of a body whose reach is a
            // swing (`Fighter.glues`), read off its live parts. A hybrid was
            // the guard's cut and glued; it reaches three with its gun, so it
            // holds there and shoots.
            test "only a body whose reach is a swing glues onto its target; a hybrid shoots it" {
                let target = { X = 25; Y = 44 }
                let from = { X = 24; Y = 44 }

                let glued body =
                    let colony =
                        declaredRaid [ hostileIn "W1N2" target smallRanged ]
                        |> withField
                        |> withGuards [ body, from ]

                    let atlas = Atlas.ofView colony
                    let threats = threatsOf colony atlas
                    let assigned = Map.ofList [ "g-1", Guard "W1N2" ]

                    Emitter.glue
                        colony
                        atlas
                        threats
                        assigned
                        (Resolver.glueChases colony atlas threats assigned),
                    Emitter.aims colony atlas threats assigned

                let hybrid = creepWith "g-1" 0 0 [ Move; Move; Move; Attack; RangedAttack; Heal ]

                Expect.equal
                    (fst (glued (guard "g-1")))
                    [ MoveCreep("g-1", Right) ]
                    "the premise: the guard steps onto its target's tile"

                let steps, strikes = glued hybrid
                Expect.isEmpty steps "the hybrid never steps onto it"

                Expect.allEqual
                    (strikes
                     |> List.map (function
                         | RangedAttackCreep _ -> true
                         | _ -> false))
                    true
                    "and aims its gun, the strike it holds its standoff for"

                Expect.isNonEmpty strikes "the premise: it aims"
            }

            // #614 review: the light ticks' plan is cut only where the full
            // tick would trade the step for another. A guard far off a
            // target standing past a swamp band that spans the field has no
            // plain way round: its plan crosses the band, as the full tick
            // would, and no light tick stalls at the edge.
            test "a far melee chaser's plan across a swamp band with no way round is kept whole" {
                let band =
                    set
                        [
                            for x in 15..35 do
                                for y in 41..42 -> { X = x; Y = y }
                        ]

                let from = { X = 25; Y = 38 }

                let colony =
                    declaredRaid [ hostileIn "W1N2" { X = 25; Y = 46 } smallRanged ] |> withField

                let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

                let colony =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withNeighbour
                                "W1N2"
                                { outpost with
                                    Terrain =
                                        TerrainGrid.ofList
                                            [
                                                for x in 15..35 do
                                                    for y in 35..48 ->
                                                        let tile = { X = x; Y = y }

                                                        tile,
                                                        (if Set.contains tile band then
                                                             Swamp
                                                         else
                                                             Plain)
                                            ]
                                }
                    }
                    |> withGuards [ guard "g-1", from ]

                let decision =
                    decide colony (Map.ofList [ "g-1", taskId (Guard "W1N2") ]) Set.empty None

                let plan = Map.tryFind "g-1" decision.Steps |> Option.defaultValue []

                Expect.isTrue
                    (plan |> List.exists (fun tile -> Set.contains (RoomPos.pos tile) band))
                    $"its plan walks onto the band: {plan}"
            }
        ]

[<Tests>]
let shutChaseTests =
    testList
        "an outpost shut on a raid a ranger kites down (#510)"
        [
            test "pools the chase for a ranger and nothing else, dark or seen" {
                let shut hostiles =
                    { raidedOutpost hostiles with
                        ChasedOutposts = Set.singleton "W1N2"
                    }
                    |> withGuards
                        [ ranger "r-1", { X = 22; Y = 47 }; guard "g-1", { X = 20; Y = 41 } ]

                let dark = decide (shut []) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" dark.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "out of sight the ranger takes the chase"

                Expect.isNone
                    (Map.tryFind "g-1" dark.Assignments)
                    "and the melee guard is sent nowhere"

                let seen = decide (shut [ plodderAt { X = 25; Y = 44 } ]) Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" seen.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "in sight it holds it"

                Expect.contains seen.Intents (RangedAttackCreep("r-1", "h-1")) "and shoots"
            }

            test
                "a raid the stand-down shuts that the colony's own row would win is the ranger's chase all the same: it shuts the economy, not the kill (#569)" {
                // Four ATTACK on one MOVE: the guard row wins it, and a ranger
                // holds its range on it, on road too.
                let shut =
                    { raidedOutpost
                          [
                              hostileIn
                                  "W1N2"
                                  { X = 25; Y = 42 }
                                  [ Attack; Attack; Attack; Attack; Move ]
                          ] with
                        RaidedOutposts = Set.singleton "W1N2"
                        Bank = { Available = 1_300; Capacity = 1_300 }
                    }
                    |> withGuards [ ranger "r-1", { X = 22; Y = 47 } ]

                Expect.isTrue
                    (Facts.rowAnswer shut "W1N2").Wins
                    "the guard row the bank buys would win it"

                // The stand-down withholds no row from the verdict: the room
                // is the row's Win, and its kill the chase.
                Expect.equal
                    (Facts.answer shut "W1N2")
                    Answer.Win
                    "and the room's verdict is the row's Win"

                let decision = decide shut Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "but the room is shut to the row's economy, so its kill is the chase, and the ranger takes its Guard"
            }

            test
                "a raid the stand-down shuts that the colony's own guard row wins, with no ranger to chase it, is the guard's kill all the same (#571)" {
                let shut =
                    { raidedOutpost
                          [
                              hostileIn
                                  "W1N2"
                                  { X = 25; Y = 42 }
                                  [ Attack; Attack; Attack; Attack; Move ]
                          ] with
                        RaidedOutposts = Set.singleton "W1N2"
                        Bank = { Available = 1_300; Capacity = 1_300 }
                    }
                    |> withGuards [ guard "g-1", { X = 22; Y = 47 } ]

                Expect.isTrue
                    (Facts.rowAnswer shut "W1N2").Wins
                    "the premise: the guard row the bank buys wins it"

                let decision = decide shut Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "g-1" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the stand-down shuts the economy, never the kill: the guard takes the room's Guard"

                // Each body on its own fitness, not the row's (#615 U3: the
                // Matcher's `Facts.fits`, no longer the Brief's admission): a
                // two-ATTACK guard that loses this raid alone.
                let weak = creepWith "g-weak" 0 0 [ Attack; Attack; Move; Move ]

                let alone =
                    decide
                        ({ shut with
                            Creeps = shut.Creeps |> List.filter (fun creep -> creep.Name <> "g-1")
                         }
                         |> withGuards [ weak, { X = 20; Y = 47 } ])
                        Map.empty
                        Set.empty
                        None

                Expect.notEqual
                    (Map.tryFind "g-weak" alone.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "one that loses the raid alone is not sent, though its row wins"
            }

            test
                "a raid the stand-down shuts that the colony's guard row wins, with no fighter of ours anywhere, casts the guard that hunts it (#571)" {
                let shut =
                    { raidedOutpost
                          [
                              hostileIn
                                  "W1N2"
                                  { X = 25; Y = 42 }
                                  [ Attack; Attack; Attack; Attack; Move ]
                          ] with
                        RaidedOutposts = Set.singleton "W1N2"
                        Bank = { Available = 5_600; Capacity = 5_600 }
                    }

                Expect.equal (Facts.answer shut "W1N2") Answer.Win "the premise: the row wins it"

                let brief = Threats.briefOf (threatsOf shut (Atlas.ofView shut)) "W1N2"

                // Moved by #654: the chase asked the guard row; the cheapest
                // body that ends a one-MOVE melee is a ranger, which shoots it
                // from out of its swing.
                Expect.equal
                    (brief.Asks |> List.map (fun ask -> ask.Row))
                    [ CastRow.Fighter BodyTemplate.Ranged ]
                    "the stand-down withholds no cast: the chase asks its row's cheapest body"

                Expect.exists
                    (spawnIntents (decide shut Map.empty Set.empty None).Intents)
                    (fun (_, _, name) -> name.StartsWith "ranger-")
                    "and the colony casts it"
            }

            test
                "a chase whose guard was just lost to the raid it wins casts none until RecastCooldown runs out (#576)" {
                // The chase passes no Guarded gate, so the recast loop the
                // cooldown breaks runs through it too.
                let shut =
                    { raidedOutpost
                          [
                              hostileIn
                                  "W1N2"
                                  { X = 25; Y = 42 }
                                  [ Attack; Attack; Attack; Attack; Move ]
                          ] with
                        RaidedOutposts = Set.singleton "W1N2"
                        Bank = { Available = 5_600; Capacity = 5_600 }
                    }

                let cooldown = shut.Tuning.RecastCooldown

                // The row's cast is a ranger since #654: the cheapest body that
                // ends a one-MOVE melee shoots it from out of its swing.
                let guardsCast colony =
                    spawnIntents (decide colony Map.empty Set.empty None).Intents
                    |> List.filter (fun (_, _, name) -> name.StartsWith "ranger-")
                    |> List.length

                Expect.equal (Facts.answer shut "W1N2") Answer.Win "the premise: the row wins it"

                Expect.equal
                    (guardsCast (lostIn "W1N2" (shut.Time - cooldown + 1) shut))
                    0
                    "a guard lost there inside the cooldown: the chase casts none"

                Expect.equal
                    (guardsCast (lostIn "W1N2" (shut.Time - cooldown) shut))
                    1
                    "and the cooldown run out, it casts the guard again"
            }

            test
                "the chase out of sight is the row's: this tick's look elsewhere in the world does not drop it (#534)" {
                let shut =
                    { raidedOutpost [] with
                        ChasedOutposts = Set.singleton "W1N2"
                    }
                    |> withGuards [ ranger "r-1", { X = 22; Y = 47 } ]

                let observed =
                    { shut with
                        World =
                            { WorldView.empty with
                                Rooms =
                                    Map.ofList
                                        [
                                            "W1N2",
                                            {
                                                Hostiles = []
                                                HostileFatigue = Map.empty
                                                HostileCargo = Map.empty
                                                RivalRamparts = Map.empty
                                                InvaderCores = []
                                                Towers = []
                                                OurTowers = []
                                                Fallen = []
                                                Control = neutralRoom
                                            }
                                        ]
                            }
                    }

                Expect.equal
                    (Map.tryFind "r-1" (decide shut Map.empty Set.empty None).Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the premise: out of sight the ranger takes the chase"

                // An observer every other tick would otherwise toggle the Task:
                // the look that finds it empty ends the chase in the row
                // (`ObserveOutpostTests`), and the gate reads only the row.
                Expect.equal
                    (Map.tryFind "r-1" (decide observed Map.empty Set.empty None).Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "with the world looking in, the row still says chase, and the ranger keeps it"
            }
        ]
