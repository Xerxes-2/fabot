/// The combat arena (#458) under test: its physics against the engine's rules,
/// and the scenarios our live fight code is played through.
module Fabot.Core.Tests.ArenaTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Tests.Arena

/// Open plain ground in W16S25, the swamp patch west of it, and the west
/// exit at y 28 that joins W17S25's east exit.
let private plainRoom = "W16S25"

let private at (x: int) (y: int) = RoomPos.at plainRoom { X = x; Y = y }

let private red = Side.Player "red"
let private blue = Side.Player "blue"

/// A body doing exactly these acts every tick.
let private doing id side bodyParts pos acts =
    body id side bodyParts pos (Some(Do acts))

/// An arena over the plain room and its west neighbour, no colony of ours.
let private physics (bodies: Body list) =
    arena 1000 [ room plainRoom; room "W17S25" ] [] bodies

let private snapshotOf id (trace: TickTrace list) =
    match List.tryLast trace with
    | Some last ->
        match last.Bodies |> List.tryFind (fun b -> b.Id = id) with
        | Some b -> b
        | None -> failtestf "%s is not standing at the end:\n%s" id (describe trace)
    | None -> failtest "no tick was run"

[<Tests>]
let arenaPhysicsTests =
    testList
        "arena physics"
        [
            test "the fixture's ground is what the tests below assume" {
                let r = room plainRoom
                Expect.equal (terrainAt r { X = 20; Y = 12 }) Plain "open plain at 20,12"
                Expect.equal (terrainAt r { X = 6; Y = 14 }) Swamp "swamp at 6,14"
                Expect.equal (terrainAt r { X = 0; Y = 28 }) Plain "an open west exit at 0,28"

                Expect.equal
                    (landingOf (at 0 28))
                    (Some("W17S25", { X = Seam.exitEdge; Y = 28 }))
                    "the west exit lands on W17S25's east edge"
            }

            test "a one-to-one body walks plain every tick, a heavier one is tired the tick after" {
                let light =
                    doing "light" red (parts [ Move, 1; Attack, 1 ]) (at 20 12) [ Act.Move Right ]

                let heavy =
                    doing "heavy" blue (parts [ Move, 1; Attack, 2 ]) (at 20 13) [ Act.Move Right ]

                let _, trace = physics [ light; heavy ] |> run 3

                Expect.equal
                    (snapshotOf "light" trace).At
                    (at 23 12)
                    "1:1 moves every tick on plain"
                // 2 heavy parts × 2 = 4 fatigue, 2 paid off a tick: moves on
                // ticks 0 and 2, rests on tick 1.
                Expect.equal (snapshotOf "heavy" trace).At (at 22 13) "1:2 moves every other tick"

                Expect.equal
                    (pathOf "heavy" trace |> List.map (fun (_, s) -> s.Fatigue))
                    [ 2; 0; 2 ]
                    "fatigue after each tick"
            }

            test "swamp costs five times plain, and a dead part still weighs" {
                // One heavy part on swamp: 10 fatigue, 2 paid a tick.
                let walker =
                    doing "walker" red (parts [ Move, 1; Attack, 1 ]) (at 6 15) [ Act.Move Top ]

                // An ATTACK at the head, destroyed: 100 hits of 200 keep the
                // tail's MOVE alone, and the dead part still weighs.
                let lamed =
                    { doing
                          "lamed"
                          blue
                          (parts [ Attack, 1; Move, 1 ])
                          (at 20 12)
                          [ Act.Move Right ] with
                        Hits = 100
                    }

                let _, trace = physics [ walker; lamed ] |> run 1
                Expect.equal (snapshotOf "walker" trace).Fatigue 8 "10 on swamp less 2 paid"

                Expect.equal
                    (snapshotOf "lamed" trace).Fatigue
                    0
                    "the dead ATTACK weighs 2, paid by the MOVE"

                Expect.equal (snapshotOf "lamed" trace).At (at 21 12) "and it stepped"
            }

            test "parts are lost from the head: a body whose MOVE is gone cannot step" {
                let lamed =
                    { doing "lamed" red (parts [ Move, 1; Attack, 1 ]) (at 20 12) [ Act.Move Right ] with
                        Hits = 100
                    }

                let _, trace = physics [ lamed ] |> run 2
                Expect.equal (snapshotOf "lamed" trace).At (at 20 12) "no live MOVE, no step"
                Expect.equal (live lamed) [ Attack ] "the tail still acts"
            }

            test "a melee hit on a body with ATTACK strikes back, a ranged one does not" {
                let hitter =
                    doing
                        "hitter"
                        red
                        (parts [ Move, 2; Attack, 2 ])
                        (at 20 12)
                        [ Act.Attack "brawler" ]

                let brawler = doing "brawler" blue (parts [ Move, 3; Attack, 3 ]) (at 21 12) []

                let shooter =
                    doing
                        "shooter"
                        red
                        (parts [ Move, 1; RangedAttack, 1 ])
                        (at 18 12)
                        [ Act.RangedAttack "brawler" ]

                let _, trace = physics [ hitter; brawler; shooter ] |> run 1

                Expect.equal
                    (snapshotOf "brawler" trace).Hits
                    (600 - 60 - 10)
                    "two swings and one shot"

                Expect.equal (snapshotOf "hitter" trace).Hits (400 - 90) "three ATTACK struck back"
                Expect.equal (snapshotOf "shooter" trace).Hits 200 "no strike-back at range"
            }

            test "a boosted part acts at its multiple and a boosted TOUGH takes its share of a hit" {
                // `calcBodyEffectiveness` with `BOOSTS`, and
                // `_applyDamage` through the TOUGH from the head.
                let boost tiers (b: Body) = { b with Boosts = tiers }

                let tank =
                    doing "tank" blue (parts [ Tough, 2; Move, 1; Heal, 1 ]) (at 20 12) []
                    |> boost [ Tier3; Tier3; Unboosted; Unboosted ]

                let hitter =
                    doing
                        "hitter"
                        red
                        (parts [ Move, 1; Attack, 2 ])
                        (at 21 12)
                        [ Act.Attack "tank" ]
                    |> boost [ Unboosted; Tier1; Tier1 ]

                let _, trace = physics [ tank; hitter ] |> run 1

                Expect.equal
                    (snapshotOf "tank" trace).Hits
                    (400 - 36)
                    "two UH ATTACK swing 120; the XGHO2 TOUGH takes 0.3 of it"

                let medic =
                    doing "medic" blue (parts [ Move, 1; Heal, 1 ]) (at 19 12) [ Act.Heal "tank" ]
                    |> boost [ Unboosted; Tier3 ]

                let _, trace = physics [ { tank with Hits = 300 }; medic ] |> run 1
                Expect.equal (snapshotOf "tank" trace).Hits 348 "one XLHO2 HEAL heals 48"

                let runner tiers =
                    doing "runner" red (parts [ Move, 1; Attack, 3 ]) (at 20 12) [ Act.Move Right ]
                    |> boost tiers

                let after tiers =
                    let _, trace = physics [ runner tiers ] |> run 2
                    (snapshotOf "runner" trace).At

                Expect.equal
                    (after [ Unboosted; Unboosted; Unboosted; Unboosted ],
                     after [ Tier3; Unboosted; Unboosted; Unboosted ])
                    (at 21 12, at 22 12)
                    "one MOVE under three parts tires on plain; an XZHO2 MOVE pays its 6 off at once"
            }

            test "damage and heal settle together before the death check" {
                let target =
                    { doing "target" red (parts [ Move, 1; Heal, 1 ]) (at 20 12) [] with
                        Hits = 100
                    }

                let medic =
                    doing "medic" red (parts [ Move, 1; Heal, 1 ]) (at 21 12) [ Act.Heal "target" ]

                let hitter =
                    doing
                        "hitter"
                        blue
                        (parts [ Move, 4; Attack, 4 ])
                        (at 19 12)
                        [ Act.Attack "target" ]

                // 100 - 120 + 12 is below zero whichever order they land in.
                let _, trace = physics [ target; medic; hitter ] |> run 1
                Expect.equal (diedOn "target" trace) (Some 0) "120 against 100 + 12 is death"

                // 110 - 120 is death alone, and the heal on the same tick
                // lands before the check.
                let _, trace = physics [ { target with Hits = 110 }; medic; hitter ] |> run 1
                Expect.equal (snapshotOf "target" trace).Hits 2 "110 - 120 + 12 lives on 2"
            }

            test "heal suppresses attack on the same body, a ranged attack beside heal stands" {
                let both =
                    doing
                        "both"
                        red
                        (parts [ Move, 2; Attack, 1; RangedAttack, 1; Heal, 1 ])
                        (at 20 12)
                        [ Act.Attack "dummy"; Act.RangedAttack "dummy"; Act.Heal "both" ]

                let dummy = doing "dummy" blue (parts [ Move, 5 ]) (at 21 12) []
                let _, trace = physics [ both; dummy ] |> run 1

                Expect.equal
                    (snapshotOf "dummy" trace).Hits
                    (500 - 10)
                    "the shot landed, the swing did not"
            }

            test "two bodies into one tile: one wins, and a standing body blocks" {
                // Equal bodies from equally wanted tiles: the first by id wins.
                let left = doing "a-left" red (parts [ Move, 1 ]) (at 19 12) [ Act.Move Right ]
                let right = doing "b-right" blue (parts [ Move, 1 ]) (at 21 12) [ Act.Move Left ]
                let _, trace = physics [ left; right ] |> run 1
                Expect.equal (snapshotOf "a-left" trace).At (at 20 12) "the first wins the tile"
                Expect.equal (snapshotOf "b-right" trace).At (at 21 12) "the other holds"

                let rock = doing "rock" blue (parts [ Move, 1 ]) (at 20 3) []
                let pusher = doing "pusher" red (parts [ Move, 1 ]) (at 19 3) [ Act.Move Right ]
                let _, trace = physics [ rock; pusher ] |> run 1
                Expect.equal (snapshotOf "pusher" trace).At (at 19 3) "a standing body blocks"
            }

            test "two bodies swap tiles, and a chain moves into tiles vacated the same tick" {
                let east = doing "east" red (parts [ Move, 1 ]) (at 20 12) [ Act.Move Right ]
                let west = doing "west" blue (parts [ Move, 1 ]) (at 21 12) [ Act.Move Left ]
                let _, trace = physics [ east; west ] |> run 1
                Expect.equal (snapshotOf "east" trace).At (at 21 12) "east took west's tile"
                Expect.equal (snapshotOf "west" trace).At (at 20 12) "and west east's"

                let head = doing "head" red (parts [ Move, 1 ]) (at 21 4) [ Act.Move Right ]
                let tail = doing "tail" red (parts [ Move, 1 ]) (at 20 4) [ Act.Move Right ]
                let _, trace = physics [ head; tail ] |> run 1

                Expect.equal
                    (snapshotOf "tail" trace).At
                    (at 21 4)
                    "the tail took the head's old tile"

                // And a chain whose head is blocked stops whole.
                let wall = doing "wall" blue (parts [ Move, 1 ]) (at 22 5) []
                let head = doing "head" red (parts [ Move, 1 ]) (at 21 5) [ Act.Move Right ]
                let tail = doing "tail" red (parts [ Move, 1 ]) (at 20 5) [ Act.Move Right ]
                let _, trace = physics [ wall; head; tail ] |> run 1
                Expect.equal (snapshotOf "head" trace).At (at 21 5) "the head is blocked"
                Expect.equal (snapshotOf "tail" trace).At (at 20 5) "the tail stops with its head"
            }

            test
                "a body ending a tick on an exit tile lands in the neighbour room, and bounces back if it stays" {
                let runner =
                    doing "runner" red (parts [ Move, 1; Attack, 1 ]) (at 1 28) [ Act.Move Left ]

                let _, trace = physics [ runner ] |> run 1

                Expect.equal
                    (snapshotOf "runner" trace).At
                    (RoomPos.at "W17S25" { X = Seam.exitEdge; Y = 28 })
                    "on W17S25's east edge"

                // Standing still on the landing: carried back the next tick.
                let arrived =
                    doing
                        "runner"
                        red
                        (parts [ Move, 1 ])
                        (RoomPos.at "W17S25" { X = Seam.exitEdge; Y = 28 })
                        []

                let _, trace = physics [ arrived ] |> run 1

                Expect.equal
                    (snapshotOf "runner" trace).At
                    (at 0 28)
                    "bounced back onto W16S25's west edge"

                // An NPC is never carried.
                let invader =
                    body
                        "invader"
                        (Side.Npc "Invader")
                        (parts [ Move, 1 ])
                        (at 1 29)
                        (Some(Do [ Act.Move Left ]))

                let _, trace = physics [ invader ] |> run 2
                Expect.equal (snapshotOf "invader" trace).At (at 0 29) "an NPC stays on the edge"
            }

            test "the step onto an exit tile costs no fatigue" {
                let heavy =
                    doing "heavy" red (parts [ Move, 1; Attack, 3 ]) (at 1 28) [ Act.Move Left ]

                let _, trace = physics [ heavy ] |> run 1
                Expect.equal (snapshotOf "heavy" trace).Fatigue 0 "fatigue zeroed on the exit step"
            }

            test "no attack or heal reaches across a border, whatever the coordinates say" {
                // One tile apart by coordinates, in two rooms.
                let here =
                    { doing
                          "here"
                          red
                          (parts [ Move, 1; Attack, 1; RangedAttack, 1 ])
                          (at 2 28)
                          [ Act.Attack "there"; Act.RangedAttack "there" ] with
                        Hits = 200
                    }

                let there =
                    doing
                        "there"
                        blue
                        (parts [ Move, 1; Heal, 1 ])
                        (RoomPos.at "W17S25" { X = 2; Y = 27 })
                        [ Act.Heal "here" ]

                let _, trace = physics [ here; there ] |> run 1
                Expect.equal (snapshotOf "there" trace).Hits 200 "nothing landed across the seam"
                Expect.equal (snapshotOf "here" trace).Hits 200 "nor was anything healed across it"
            }
        ]

/// The W11S28 invasion's shooter and healer (#535).
let private invaderShooter = parts [ Tough, 2; Move, 4; RangedAttack, 3; Move, 1 ]
let private invaderHealer = parts [ Move, 4; Heal, 5; Move, 1 ]
let private npc = Side.Npc "Invader"

/// The engine's invader AI (#541, `invaders/pretick.js`), as the #540
/// xxscreeps spike watched it.
[<Tests>]
let arenaInvaderTests =
    testList
        "arena NPC invaders"
        [
            test
                "an invader shooter flees a melee nearer than 3, keeping it off and shooting it as it chases" {
                let shooter = body "shooter" npc invaderShooter (at 20 13) (Some Invader)

                let chaser =
                    body
                        "chaser"
                        red
                        (parts [ Move, 5; Attack, 5 ])
                        (at 18 13)
                        (Some(Chase(Nearest, None)))

                let _, trace = physics [ shooter; chaser ] |> run 12
                let failure = describe trace

                for tick, s in pathOf "shooter" trace do
                    let c = pathOf "chaser" trace |> List.find (fst >> (=) tick) |> snd

                    Expect.isGreaterThan
                        (RoomPos.range s.At c.At |> Option.defaultValue 99)
                        1
                        $"t{tick}: never beside it\n{failure}"

                Expect.equal (snapshotOf "shooter" trace).Hits 1000 $"never struck\n{failure}"

                Expect.isLessThan
                    (snapshotOf "chaser" trace).Hits
                    1000
                    $"and shooting it\n{failure}"
            }

            test "an invader shooter shoots the lowest-hits hostile in reach, not the nearest" {
                let shooter = body "shooter" npc invaderShooter (at 20 13) (Some Invader)

                let near = doing "near" red (parts [ Move, 2 ]) (at 22 13) []

                let hurt =
                    { doing "hurt" red (parts [ Move, 2 ]) (at 17 13) [] with
                        Hits = 150
                    }

                let _, trace = physics [ shooter; near; hurt ] |> run 1

                Expect.contains
                    trace.Head.Theirs
                    ("shooter", Act.RangedAttack "hurt")
                    $"the hurt one, at 3, over the one at 2\n{describe trace}"
            }

            test
                "an invader healer walks to the hurt invader, beside a healthy one it was standing by" {
                let healer = body "healer" npc invaderHealer (at 20 13) (Some Invader)
                let healthy = body "healthy" npc invaderShooter (at 19 13) (Some Invader)

                let hurt =
                    { body "hurt" npc invaderShooter (at 26 13) (Some Invader) with
                        Hits = 500
                    }

                let _, trace = physics [ healer; healthy; hurt ] |> run 4
                let failure = describe trace

                Expect.equal
                    (RoomPos.range (snapshotOf "healer" trace).At (at 26 13))
                    (Some 2)
                    $"four steps toward it\n{failure}"

                Expect.isGreaterThan
                    (snapshotOf "hurt" trace).Hits
                    500
                    $"healing it in reach\n{failure}"
            }

            test "an unhurt invader healer with no invader but healers beside it suicides" {
                let healer = body "healer" npc invaderHealer (at 20 13) (Some Invader)
                let _, trace = physics [ healer ] |> run 1
                Expect.equal (diedOn "healer" trace) (Some 0) $"it suicides\n{describe trace}"
            }
        ]

/// The plain room with these structures standing in it.
let private built (structures: ArenaStructure list) (bodies: Body list) =
    arena 1000 [ room plainRoom |> withStructures structures; room "W17S25" ] [] bodies

let private tile (x: int) (y: int) : Pos = { X = x; Y = y }

/// A structure's hits at the end of the run, or None once it is gone.
let private hitsLeft (id: string) (trace: TickTrace list) : int option =
    trace |> List.tryLast |> Option.bind (fun t -> Map.tryFind id t.Structures)

[<Tests>]
let arenaStructureTests =
    testList
        "arena structures"
        [
            test
                "a body on its owner's rampart takes nothing: the rampart takes the swing and the shot, and strikes nobody back" {
                let cover = rampart red 10_000 (tile 21 12)

                let standing = doing "standing" red (parts [ Move, 3; Attack, 3 ]) (at 21 12) []

                let hitter =
                    doing
                        "hitter"
                        blue
                        (parts [ Move, 2; Attack, 2 ])
                        (at 20 12)
                        [ Act.Attack "standing" ]

                let shooter =
                    doing
                        "shooter"
                        blue
                        (parts [ Move, 1; RangedAttack, 1 ])
                        (at 18 12)
                        [ Act.RangedAttack "standing" ]

                let _, trace = built [ cover ] [ standing; hitter; shooter ] |> run 1
                Expect.equal (snapshotOf "standing" trace).Hits 600 "the body is untouched"

                Expect.equal
                    (hitsLeft cover.Id trace)
                    (Some(10_000 - 60 - 10))
                    "the rampart took both"

                Expect.equal
                    (snapshotOf "hitter" trace).Hits
                    400
                    "a swing that lands on a rampart is struck back by nobody"
            }

            test
                "a scripted tower shoots the nearest enemy, and the rampart under it takes the shot at the falloff" {
                // Range 10 from 21,12: 600 less 600 × 0.75 × 5 / 15.
                let tower = towerOf blue (tile 21 2) 1000
                let cover = rampart red 10_000 (tile 21 12)
                let standing = doing "standing" red (parts [ Move, 1 ]) (at 21 12) []
                let bare = doing "bare" red (parts [ Move, 5 ]) (at 21 13) []

                let _, trace =
                    arena
                        1000
                        [
                            room plainRoom
                            |> withStructures [ tower; cover ]
                            |> withTowerDuties [ Shoot Nearest ]
                        ]
                        []
                        [ standing; bare ]
                    |> run 1

                Expect.equal (hitsLeft cover.Id trace) (Some(10_000 - 450)) "450 at range 10"
                Expect.equal (snapshotOf "standing" trace).Hits 100 "nothing on the body"
            }

            test
                "a tower heals its side's hurt at the falloff and mends a rampart, ten energy an act, and a dry tower does nothing" {
                // Heal before repair before attack (`towers/intents.js`).
                let tower = towerOf blue (tile 21 2) 1000
                let worn = rampart blue 1_000 (tile 21 12)

                let hurt =
                    { doing "hurt" blue (parts [ Move, 10 ]) (at 21 22) [] with
                        Hits = 500
                    }

                let duties = [ HealHurt; Mend 1_000_000; Shoot Nearest ]

                let world tower bodies =
                    arena
                        1000
                        [
                            room plainRoom
                            |> withStructures [ tower; worn ]
                            |> withTowerDuties duties
                        ]
                        []
                        bodies

                // Range 20: 400 × 0.25.
                let final, trace = world tower [ hurt ] |> run 1
                Expect.equal (snapshotOf "hurt" trace).Hits 600 "100 healed at range 20"
                Expect.equal (hitsLeft worn.Id trace) (Some 1_000) "the heal took the tick"

                let spent =
                    final.Rooms[plainRoom].Structures |> List.find (fun s -> s.Id = tower.Id)

                Expect.equal spent.Energy 990 "ten energy spent"

                // Nobody hurt: the rampart at range 10, 800 less 800 × 0.25.
                let _, trace = world tower [] |> run 1
                Expect.equal (hitsLeft worn.Id trace) (Some(1_000 + 600)) "600 mended at range 10"

                let _, trace = world { tower with Energy = 9 } [] |> run 1
                Expect.equal (hitsLeft worn.Id trace) (Some 1_000) "nine energy is no act"
            }

            test
                "rangedMassAttack hits the other side's structures in three at the distance rate, never a wall nor a body under a rampart" {
                let mass =
                    doing
                        "mass"
                        blue
                        (parts [ Move, 2; RangedAttack, 2 ])
                        (at 20 20)
                        [ Act.RangedMassAttack ]

                let cover = rampart red 10_000 (tile 21 20)
                let sheltered = doing "sheltered" red (parts [ Move, 1 ]) (at 21 20) []
                let store = structureOf "extension" (Some red) 1_000 1_000 (tile 22 20)
                let wall = structureOf "constructedWall" None 10_000 10_000 (tile 20 22)
                let far = doing "far" red (parts [ Move, 1 ]) (at 23 21) []

                let _, trace = built [ cover; store; wall ] [ mass; sheltered; far ] |> run 1
                Expect.equal (hitsLeft cover.Id trace) (Some(10_000 - 20)) "range 1: the full 20"
                Expect.equal (hitsLeft store.Id trace) (Some(1_000 - 8)) "range 2: 40%"
                Expect.equal (hitsLeft wall.Id trace) (Some 10_000) "a wall has no owner to hit"
                Expect.equal (snapshotOf "sheltered" trace).Hits 100 "the rampart covers its body"
                Expect.equal (snapshotOf "far" trace).Hits 98 "range 3: 10%"
            }

            test "dismantle takes 50 a WORK, off the rampart over a structure first, and off a wall" {
                let spawn = structureOf "spawn" (Some red) 5_000 5_000 (tile 21 12)
                let cover = rampart red 10_000 (tile 21 12)
                let wall = structureOf "constructedWall" None 10_000 10_000 (tile 19 12)

                let breaker =
                    doing
                        "breaker"
                        blue
                        (parts [ Move, 2; Work, 2 ])
                        (at 20 12)
                        [ Act.Dismantle spawn.Id ]

                let waller =
                    doing
                        "waller"
                        blue
                        (parts [ Move, 1; Work, 1 ])
                        (at 19 13)
                        [ Act.Dismantle wall.Id ]

                let _, trace = built [ spawn; cover; wall ] [ breaker; waller ] |> run 1
                Expect.equal (hitsLeft cover.Id trace) (Some(10_000 - 100)) "the rampart first"
                Expect.equal (hitsLeft spawn.Id trace) (Some 5_000) "the spawn under it untouched"
                Expect.equal (hitsLeft wall.Id trace) (Some(10_000 - 50)) "the wall"
            }

            test
                "a body repairs 100 a WORK at range 3, an energy per 100 hits, capped by what it carries and what is missing" {
                let worn = rampart red 1_000 (tile 23 12)

                let mender energy =
                    { doing
                          "mender"
                          red
                          (parts [ Move, 2; Work, 2; Carry, 1 ])
                          (at 20 12)
                          [ Act.Repair worn.Id ] with
                        Energy = energy
                    }

                let final, trace = built [ worn ] [ mender 50 ] |> run 1
                Expect.equal (hitsLeft worn.Id trace) (Some 1_200) "two WORK, 200"

                Expect.equal
                    (final.Bodies |> List.find (fun b -> b.Id = "mender")).Energy
                    48
                    "two energy spent"

                let _, trace = built [ worn ] [ mender 1 ] |> run 1
                Expect.equal (hitsLeft worn.Id trace) (Some 1_100) "one energy buys 100"

                let _, trace = built [ { worn with HitsMax = 1_150 } ] [ mender 50 ] |> run 1

                Expect.equal (hitsLeft worn.Id trace) (Some 1_150) "no more than is missing"
            }

            test
                "a transfer puts what the body carries into a structure beside it, no more than there is room for" {
                let tower = towerOf red (tile 21 12) 900

                let carrier =
                    { doing
                          "carrier"
                          red
                          (parts [ Move, 4; Carry, 4 ])
                          (at 20 12)
                          [ Act.Transfer tower.Id ] with
                        Energy = 200
                    }

                let final, _ = built [ tower ] [ carrier ] |> run 1

                let filled =
                    final.Rooms[plainRoom].Structures |> List.find (fun s -> s.Id = tower.Id)

                Expect.equal filled.Energy 1000 "filled to TOWER_CAPACITY"

                Expect.equal
                    (final.Bodies |> List.find (fun b -> b.Id = "carrier")).Energy
                    100
                    "the rest still carried"
            }

            test
                "a rampart decays 300 every 100 ticks from its next decay tick, and one decayed to nothing is gone" {
                // Placed with no decay tick, it has just decayed; then it
                // decays at `gameTime >= nextDecayTime - 1` (`ramparts/tick.js`).
                let worn = rampart red 700 (tile 21 12)
                let _, trace = built [ worn ] [] |> run 300

                let at tick =
                    trace[tick].Structures |> Map.tryFind worn.Id

                Expect.equal (at 98) (Some 700) "whole until its decay tick"
                Expect.equal (at 99) (Some 400) "300 off at time 1099, nextDecayTime 1100 less one"
                Expect.equal (at 197) (Some 400) "held until the next"
                Expect.equal (at 198) (Some 100) "300 off again at 1198"
                Expect.equal (at 297) None "gone"

                Expect.isTrue
                    (trace[297].Events |> List.contains (Destroyed worn.Id))
                    "and its passing is an event"
            }

            test
                "a wall and the other side's rampart bar a step; the owner's rampart and a public one do not" {
                let mine = rampart red 10_000 (tile 21 12)

                let shared =
                    { rampart red 10_000 (tile 21 14) with
                        IsPublic = true
                    }

                let wall = structureOf "constructedWall" None 10_000 10_000 (tile 21 16)

                let step id side y =
                    doing id side (parts [ Move, 1 ]) (at 20 y) [ Act.Move Right ]

                let _, trace =
                    built
                        [ mine; shared; wall ]
                        [ step "owner" red 12; step "walker" blue 14; step "waller" red 16 ]
                    |> run 1

                Expect.equal (snapshotOf "owner" trace).At (at 21 12) "onto its own rampart"
                Expect.equal (snapshotOf "walker" trace).At (at 21 14) "onto a public one"
                Expect.equal (snapshotOf "waller" trace).At (at 20 16) "never onto a wall"

                let _, trace = built [ mine ] [ step "stranger" blue 12 ] |> run 1

                Expect.equal
                    (snapshotOf "stranger" trace).At
                    (at 20 12)
                    "never onto another's rampart"
            }

            test
                "a breacher walks to the weakest barrier between it and its goal, breaks it, and walks in" {
                // A goal ringed by eight ramparts, one of them a tenth of the rest.
                let goal = at 30 12

                let ring =
                    [
                        for x in 29..31 do
                            for y in 11..13 do
                                if (x, y) <> (30, 12) then
                                    rampart
                                        red
                                        (if (x, y) = (29, 12) then 3_000 else 30_000)
                                        (tile x y)
                    ]

                let breacher =
                    body
                        "breacher"
                        blue
                        (parts [ Move, 10; Attack, 10 ])
                        (at 20 12)
                        (Some(Breach goal))

                let _, trace = built ring [ breacher ] |> run 40
                let weak = ring |> List.find (fun s -> s.At = tile 29 12)

                Expect.isTrue
                    (trace |> List.exists (fun t -> t.Events |> List.contains (Destroyed weak.Id)))
                    $"the weak rampart broken\n{describe trace}"

                Expect.equal (snapshotOf "breacher" trace).At goal "and the goal reached"

                Expect.equal
                    (ring
                     |> List.filter (fun s -> s.Id <> weak.Id)
                     |> List.choose (fun s -> hitsLeft s.Id trace))
                    (List.replicate 7 30_000)
                    "no other rampart struck"
            }
        ]

/// The outpost a melee guard defends: W12S28 at RCL7 with its declared
/// W12S27, a hauler of ours standing by W12S27's source for vision.
let private outpostRaid (invader: Body) =
    let home =
        room "W12S28"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn1" { X = 12; Y = 38 } 5600

    let outpost = room "W12S27"

    let declared =
        { colony "W12S28" with
            Outposts = [ outpostOf outpost.Capture ]
        }

    let guard =
        body
            "guard-1-Spawn1"
            Side.Ours
            (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
            (RoomPos.at "W12S28" { X = 20; Y = 5 })
            None

    let hauler =
        body
            "hauler-1-Spawn1"
            Side.Ours
            (parts [ Carry, 2; Move, 2 ])
            (RoomPos.at "W12S27" { X = 13; Y = 46 })
            None

    arena 900_000 [ home; outpost ] [ declared ] [ guard; hauler; invader ]

/// Trepidimous' bodies (`docs/research/squads.md` §1.2, the t878,470 JSON):
/// MOVE first, the weapon, one MOVE last; the healer MOVE then HEAL.
let private trepMelee = parts [ Move, 17; Attack, 17; Move, 1 ]
let private trepHealer = parts [ Move, 11; Heal, 7 ]
let private trepTapper = parts [ Move, 3; BodyPart.Claim, 3 ]
let private trep = Side.Player "Trepidimous"

/// One resident R7 as the ranger row casts it: `21M 14R 7H`, MOVE first.
let private r7 = parts [ Move, 21; RangedAttack, 14; Heal, 7 ]

let private w17s25 (x: int) (y: int) = RoomPos.at "W17S25" { X = x; Y = y }

/// The W17S25 siege at t880,341 (§1.1): the parked 2×2 block beside the
/// controller at 15,36 with the tapper on it, against what we field. W17S25
/// is a nursery raised by W17S26 here — live its mother was W15S28, five
/// crossings off; the arena's mother is the neighbour so one seam joins them,
/// which moves nothing inside W17S25.
let private besieged (raid: Body list) (ours: Body list) =
    let mother =
        room "W17S26"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn8" { X = 20; Y = 26 } 5600

    let child = room "W17S25" |> withController Ownership.Ours None 1 0

    let colonies =
        [
            colony "W17S26"
            { colony "W17S25" with
                Mother = Some "W17S26"
            }
        ]

    arena 880_341 [ mother; child ] colonies (ours @ raid)

let private siege (ours: Body list) =
    let controller = w17s25 15 36

    let raid =
        [
            body
                "Eternity536"
                trep
                trepMelee
                (w17s25 16 36)
                (Some(Chase(Nearest, Some(w17s25 16 36, 4))))
            body
                "Prime803"
                trep
                trepMelee
                (w17s25 17 37)
                (Some(Chase(Nearest, Some(w17s25 17 37, 4))))
            body "Prism305" trep trepHealer (w17s25 16 35) (Some(Follow "Eternity536"))
            body "Paragon722" trep trepHealer (w17s25 17 36) (Some(Follow "Prime803"))
            body "Rune908" trep trepTapper (w17s25 16 37) (Some(Tap controller))
        ]

    besieged raid ours

/// Odiodin's garrison (§1.4): `20M16R4H`, MOVE first.
let private garrison = parts [ Move, 20; RangedAttack, 16; Heal, 4 ]
let private odiodin = Side.Player "Odiodin"
let private w17s22 (x: int) (y: int) = RoomPos.at "W17S22" { X = x; Y = y }

/// W17S22 alone, Odiodin's room at RCL2 (§1.5), and no colony of ours: both
/// sides scripted, the replay's own numbers checked against the physics.
let private replay (bodies: Body list) =
    let r = room "W17S22" |> withController Ownership.Rival (Some "Odiodin") 2 0

    arena 878_959 [ r ] [] bodies

/// The hits one body ends each tick on.
let private hitsOf (id: string) (trace: TickTrace list) =
    pathOf id trace |> List.map (fun (_, s) -> s.Hits)

/// The ticks a body of ours ended within one of any of these melee bodies,
/// each with the melee beside it.
let private caughtBeside (melee: string list) (ours: string) (trace: TickTrace list) =
    trace
    |> List.choose (fun t ->
        match t.Bodies |> List.tryFind (fun b -> b.Id = ours) with
        | Some me ->
            let beside =
                t.Bodies
                |> List.filter (fun b ->
                    List.contains b.Id melee
                    && RoomPos.range me.At b.At |> Option.exists (fun r -> r <= 1))
                |> List.map (fun b -> b.Id)

            if List.isEmpty beside then None else Some(t.Tick, beside)
        | None -> None)

/// The raid's bodies, by id.
let private raidIds =
    [ "Eternity536"; "Prime803"; "Prism305"; "Paragon722"; "Rune908" ]

/// `npm run capture -- W12S28 972170 --name storageFlip`: W12S28 at t972,170,
/// in the middle of #530's flip (hauler-970894/970938-Spawn6 drew the Storage
/// and poured it back eight times over t972,169-185). Roads (130) are not
/// placed; every body starts idle, the Transition log holding nothing that
/// early.
let private storageFlip () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w12s28 =
        room "W12S28"
        |> withController Ownership.Ours None 7 6
        |> withControllerClocks 0 0 149_982 10_696_734
        |> withSpawn "Spawn1" { X = 12; Y = 40 } 5_600
        |> withSpawn "Spawn6" { X = 18; Y = 34 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        7, 37
                        8, 34
                        8, 36
                        9, 33
                        9, 35
                        9, 37
                        9, 39
                        9, 41
                        10, 34
                        10, 38
                        10, 42
                        11, 33
                        11, 37
                        11, 39
                        11, 41
                        12, 34
                        12, 38
                        12, 42
                        12, 44
                        13, 33
                        13, 35
                        13, 37
                        13, 39
                        13, 41
                        13, 43
                        14, 34
                        14, 38
                        14, 40
                        14, 44
                        15, 33
                        15, 35
                        15, 37
                        15, 39
                        15, 43
                        15, 45
                        16, 34
                        16, 38
                        17, 33
                        17, 35
                        17, 37
                        17, 43
                        17, 45
                        18, 42
                        18, 44
                        18, 46
                        19, 33
                        19, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 10, 36; 10, 40; 16, 46 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                structureOf "container" None 133_100 250_000 { X = 7; Y = 39 }
                { structureOf "container" None 136_500 250_000 { X = 10; Y = 43 } with
                    Energy = 576
                }
                { structureOf "container" None 158_200 250_000 { X = 16; Y = 39 } with
                    Energy = 348
                }
                structureOf "container" None 180_300 250_000 { X = 25; Y = 6 }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 14; Y = 36 } with
                    Energy = 88
                }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 16; Y = 36 }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 16; Y = 44 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 26; Y = 5 }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 14; Y = 42 } with
                    Energy = 30_569
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 11; Y = 43 } with
                    Energy = 4_000
                }
            ]

    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W12S28" ])

    let bodies: Body list =
        [
            { body
                  "worker-970554-Spawn1"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W12S28" 12 43)
                  None with
                TicksToLive = 32
                Energy = 450
            }
            { body
                  "worker-970746-Spawn1"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W12S28" 13 44)
                  None with
                TicksToLive = 224
                Energy = 236
            }
            { body
                  "hauler-970894-Spawn6"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W12S28" 13 42)
                  None with
                TicksToLive = 366
            }
            { body
                  "hauler-970938-Spawn1"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W12S28" 43 25)
                  None with
                TicksToLive = 410
            }
            { body
                  "upgrader-971328-Spawn1"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W12S28" 5 37)
                  None with
                TicksToLive = 803
            }
            { body
                  "anchor-971374-Spawn6"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W12S28" 10 43)
                  None with
                TicksToLive = 732
                Energy = 50
            }
            { body
                  "anchor-971427-Spawn6"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W12S28" 16 39)
                  None with
                TicksToLive = 785
                Energy = 50
            }
            { body
                  "hauler-970592-Spawn6"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W12S28" 14 41)
                  None with
                TicksToLive = 64
            }
        ]

    arena 972_171 [ w12s28 ] colonies bodies

/// Each body's draws from a store and pours back into it with nothing poured
/// anywhere else between: the flip #530 is about, as (body, store, tick) rows.
let private pouredBack (trace: TickTrace list) =
    trace
    |> List.collect (fun t ->
        t.Ours
        |> List.choose (function
            | WithdrawFromStore(creep, store, Energy, _) -> Some(creep, (t.Tick, Choice1Of2 store))
            | TransferEnergyToStructure(creep, target, Energy) ->
                Some(creep, (t.Tick, Choice2Of2 target))
            | _ -> None))
    |> List.groupBy fst
    |> List.collect (fun (creep, acts) ->
        acts
        |> List.map snd
        |> List.pairwise
        |> List.choose (function
            | (_, Choice1Of2 drawn), (tick, Choice2Of2 poured) when drawn = poured ->
                Some(creep, drawn, tick)
            | _ -> None))

// ---- arena capture w11s28-invasion (#535) -----------------------------------
// `npm run capture -- W11S28 973670 --rooms W12S28 --name w11s28Invasion`.
// The room-history replay at t973,670, the state after that tick ran: arena
// tick 0 runs as t973,671.
//   W11S28  https://screeps.com/room-history/shardSeason/W11S28/973600.json
//   W12S28  https://screeps.com/room-history/shardSeason/W12S28/973600.json
// Left out, as the capture says: Healers, ExitWatches and Sightings start
// empty; W11S28's reservation, roads and drops are not placed; two invaders'
// boosts are not placed. W12S28 is cut to what the fight reads: its
// spawns, towers and storage. The two guards hold no verdict that early in
// the Transition log, so they start unassigned.

/// W11S28 at t973,670 (#535's "Why"): an NPC invasion — two 2T5M3R shooters,
/// each followed by a 5M5H healer, and a small mixed body — answered by two
/// 30-part melee guards cast at W12S28, one at the east exit and one by the
/// spawns. Live, the first fought alone, had its MOVE shot off and sat rooted.
/// All five run the engine's invader AI (`Invader`, #541): the shooters flee
/// a guard nearer than 3 and shoot the lowest hits, the healers go to the
/// hurt, the small one walks in.
let private w11s28Invasion () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w11s28 =
        room "W11S28"
        |> withStructures [ structureOf "container" None 120_100 250_000 { X = 32; Y = 14 } ]

    let w12s28 =
        room "W12S28"
        |> withController Ownership.Ours None 7 6
        |> withControllerClocks 0 0 149_922 10_730_254
        |> withSpawn "Spawn1" { X = 12; Y = 40 } 5_600
        |> withSpawn "Spawn6" { X = 18; Y = 34 } 5_600
        |> withStructures
            [
                for x, y in [ 10, 36; 10, 40; 16, 46 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 14; Y = 42 } with
                    Energy = 29_037
                }
            ]

    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W12S28" ])

    let invader = Side.Npc "Invader"
    let shooter = parts [ Tough, 2; Move, 4; RangedAttack, 3; Move, 1 ]
    let healer = parts [ Move, 4; Heal, 5; Move, 1 ]
    let guard = parts [ Tough, 3; Move, 15; Attack, 9; Heal, 3 ]

    let bodies: Body list =
        [
            // live: healed→12,19; then closes and dies to the first guard.
            { body
                  "invader_W11S28_735"
                  invader
                  (parts [ Tough, 2; Move, 4; RangedAttack, 1; Work, 1; Attack, 1; Move, 1 ])
                  (at "W11S28" 10 22)
                  (Some Invader) with
                TicksToLive = 1_373
            }
            { body "invader_W11S28_768" invader shooter (at "W11S28" 11 19) (Some Invader) with
                TicksToLive = 1_373
            }
            { body "invader_W11S28_779" invader shooter (at "W11S28" 10 19) (Some Invader) with
                TicksToLive = 1_373
            }
            { body "invader_W11S28_495" invader healer (at "W11S28" 12 18) (Some Invader) with
                TicksToLive = 1_373
            }
            { body "invader_W11S28_657" invader healer (at "W11S28" 12 19) (Some Invader) with
                TicksToLive = 1_373
            }
            { body "guard-973545-Spawn1" Side.Ours guard (at "W12S28" 47 33) None with
                TicksToLive = 1_463
            }
            { body "guard-973545-Spawn6" Side.Ours guard (at "W12S28" 19 39) None with
                TicksToLive = 1_463
            }
            // The economy is staffed live; one hauler of the capture's stands
            // for it, so no supply floor jumps the fighting rows' queue.
            { body
                  "hauler-972391-Spawn1"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W12S28" 17 34)
                  None with
                TicksToLive = 363
            }
        ]

    let a = arena 973_671 [ w11s28; w12s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                Towered = Set.ofList [ "W12S28" ]
                // Nothing of ours stands in W11S28 at the tick: live, the
                // raid was remembered (the threatened latch the guards were
                // cast on at t973,545), not seen.
                Raids =
                    Map.ofList
                        [
                            "W12S28",
                            { Fabot.Core.Observe.RaidState.empty with
                                Threatened =
                                    Map.ofList
                                        [
                                            "W11S28",
                                            {
                                                Until = 973_545 + Engine.creepLifetime
                                            }
                                        ]
                            }
                        ]
            }
    }

[<Tests>]
let arenaScenarioTests =
    testList
        "arena scenarios"
        [
            // #530 capture t972,170: live, W12S28's haulers drew the Storage for
            // sinks other loads already covered and poured the energy straight
            // back, every tick or two.
            test "#530 capture t972,170: no hauler pours a Storage draw back into the Storage" {
                let _, trace = storageFlip () |> run 40

                Expect.isEmpty
                    (pouredBack trace)
                    $"a load goes on to a sink, never back where it came from\n{describe trace}"
            }

            // Reproduced live before #535 (main feaee027, no casts): both guards
            // took the Guard and the first went in alone at t6, swung the
            // small mixed body down and stood kept off by the shooters, the
            // second in at t32. #535 read penned Invaders as in a melee's
            // reach and the two guards ran the shooters into the walls. #568:
            // an Invader shooter flees every hostile inside three, so penned
            // is no reach (spar's lone guard kited to death by one), and the
            // room is the ranger row's as a player's shooters are
            // (`engagementTests`). The guards kill the mixed body at t9 and
            // are sent no further; the ranger cast at t1 clears the rest by
            // t198 (768, 779, 495, 657 at t184/189/198/198).
            test
                "#535 capture t973,670: the W11S28 invasion's shooters keep off a melee guard, so a ranger is cast for them" {
                let start = w11s28Invasion () |> withCasts [ "guard"; "ranger" ]
                let _, trace = start |> run 300
                crossValidate "w11s28-invasion" start trace
                let failure = describe trace

                let born =
                    trace
                    |> List.collect (fun t ->
                        t.Events
                        |> List.choose (function
                            | Born id -> Some id
                            | _ -> None))

                Expect.isNonEmpty born $"a ranger is cast\n{failure}"

                Expect.all
                    born
                    (fun id -> id.StartsWith "ranger-")
                    $"and nothing but rangers\n{failure}"

                for invader in
                    [
                        "invader_W11S28_735"
                        "invader_W11S28_768"
                        "invader_W11S28_779"
                        "invader_W11S28_495"
                        "invader_W11S28_657"
                    ] do
                    Expect.isSome (diedOn invader trace) $"{invader} dies\n{failure}"

                for guard in [ "guard-973545-Spawn1"; "guard-973545-Spawn6" ] do
                    Expect.isNone (diedOn guard trace) $"{guard} lives\n{failure}"
            }

            // #535 invariant 3, the melee's rule: attacks resolve before moves,
            // so a guard swinging at the body beside it steps onto that body's
            // tile too. The move lands when the body steps away and fails
            // harmlessly when it stays.
            test
                "a melee guard beside a raider that steps away ends each tick beside it, and beside one that stays it stands and swings" {
                let world (behaviour: Behaviour) =
                    let home =
                        room "W12S28"
                        |> withController Ownership.Ours None 7 0
                        |> withSpawn "Spawn1" { X = 12; Y = 38 } 5600

                    let outpost = room "W12S27"

                    let declared =
                        { colony "W12S28" with
                            Outposts = [ outpostOf outpost.Capture ]
                        }

                    let guard =
                        body
                            "guard-1-Spawn1"
                            Side.Ours
                            (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                            (RoomPos.at "W12S27" { X = 19; Y = 25 })
                            None

                    let raider =
                        body
                            "invader-1"
                            (Side.Npc "Invader")
                            Fabot.Core.Tests.Decide.Fixtures.smallMelee
                            (RoomPos.at "W12S27" { X = 20; Y = 25 })
                            (Some behaviour)

                    arena 900_000 [ home; outpost ] [ declared ] [ guard; raider ]

                let _, fleeing = world (Do [ Act.Move Right ]) |> run 6
                let failure = describe fleeing

                for (tick, raider) in pathOf "invader-1" fleeing do
                    let guard =
                        pathOf "guard-1-Spawn1" fleeing |> List.find (fst >> (=) tick) |> snd

                    Expect.equal
                        (RoomPos.range guard.At raider.At)
                        (Some 1)
                        $"t{tick}: beside it again\n{failure}"

                Expect.isLessThan
                    (pathOf "invader-1" fleeing |> List.last |> snd |> (fun s -> s.Hits))
                    (Engine.partHits * List.length Fabot.Core.Tests.Decide.Fixtures.smallMelee)
                    $"and struck as it goes\n{failure}"

                let _, staying = world Hold |> run 4
                let failure = describe staying

                Expect.equal
                    (pathOf "guard-1-Spawn1" staying
                     |> List.map (fun (_, s) -> s.At)
                     |> List.distinct)
                    [ RoomPos.at "W12S27" { X = 19; Y = 25 } ]
                    $"beside one that stays the step fails and it stands\n{failure}"

                // Its swing and the strike-back its ATTACK lands on the
                // raider's own swing, every tick.
                Expect.equal
                    (pathOf "invader-1" staying |> List.map (fun (_, s) -> s.Hits))
                    [ 820; 640; 460; 280 ]
                    $"and swings every tick\n{failure}"
            }
            // The Brief's S7, rule 1 (a hostile no heal reaches is read live): the
            // guard swings the invader down to its last part, a MOVE, unarmed,
            // and its room's Guard as a raid's goes. #535 flips what S7 left:
            // the fight it began is fought to the death, so it kills it.
            test "an outpost guard walks in, disarms a smallMelee invader and fights on to kill it" {
                let invader =
                    body
                        "invader-1"
                        (Side.Npc "Invader")
                        Fabot.Core.Tests.Decide.Fixtures.smallMelee
                        (RoomPos.at "W12S27" { X = 8; Y = 48 })
                        (Some Invader)

                let _, trace = outpostRaid invader |> run 40

                Expect.isSome (diedOn "invader-1" trace) $"the invader dies\n{describe trace}"

                Expect.isNone (diedOn "guard-1-Spawn1" trace) "the guard lives"
            }

            // #510, the W18S25 shape: Torque404 left at 1,454 hits, 14 ATTACK on
            // one MOVE under the whole body's weight, against an 800 bank whose
            // one guard block it out-swings, and one ranger of ours at home.
            test "a ranger walks out and kills a crippled melee from range, untouched" {
                let home =
                    room "W12S28"
                    |> withController Ownership.Ours None 7 0
                    |> withSpawn "Spawn1" { X = 12; Y = 38 } 800

                let outpost = room "W12S27"

                let declared =
                    { colony "W12S28" with
                        Outposts = [ outpostOf outpost.Capture ]
                    }

                let ranger =
                    body
                        "ranger-1-Spawn1"
                        Side.Ours
                        Fabot.Core.Decide.Bodies.rangerPattern.Block
                        (RoomPos.at "W12S28" { X = 20; Y = 5 })
                        None

                let hauler =
                    body
                        "hauler-1-Spawn1"
                        Side.Ours
                        (parts [ Carry, 2; Move, 2 ])
                        (RoomPos.at "W12S27" { X = 13; Y = 46 })
                        None

                let torque =
                    { body
                          "Torque404"
                          trep
                          (parts [ Move, 17; Attack, 3; Move, 1; Attack, 14 ])
                          (RoomPos.at "W12S27" { X = 8; Y = 48 })
                          (Some(Chase(Nearest, None))) with
                        Hits = 1_454
                    }

                let _, trace =
                    arena 900_000 [ home; outpost ] [ declared ] [ ranger; hauler; torque ]
                    |> run 150

                Expect.isSome
                    (diedOn "Torque404" trace)
                    $"the crippled melee dies\n{describe trace}"

                Expect.isTrue
                    (hitsOf "ranger-1-Spawn1" trace
                     |> List.forall (
                         (=) (
                             Engine.partHits
                             * List.length Fabot.Core.Decide.Bodies.rangerPattern.Block
                         )
                     ))
                    $"and the ranger never takes a swing\n{describe trace}"

                Expect.isEmpty
                    (caughtBeside [ "Torque404" ] "ranger-1-Spawn1" trace)
                    "nor ends a tick beside it"
            }

            // The user's chase (live 2026-10-07): a ranger chasing stepped onto
            // swamp, its fatigue held it, and two ticks later its target had
            // walked out of reach. Here a shooter walks east along a lane with
            // a two-row swamp band between it and the ranger, three rows off:
            // the plain lane beside the band keeps it in reach.
            test
                "a ranger chasing a shooter along a swamp band keeps to the plain lane and keeps it in reach" {
                let home =
                    room "W12S28"
                    |> withController Ownership.Ours None 7 0
                    |> withSpawn "Spawn1" { X = 12; Y = 38 } 5_600

                // A field laid on W12S27's south-west floor: plain, but for
                // the swamp band on rows 29 and 30.
                let outpost =
                    let r = room "W12S27"

                    { r with
                        Capture =
                            { r.Capture with
                                Terrain =
                                    (r.Capture.Terrain,
                                     [
                                         for x in 5..30 do
                                             for y in 26..34 -> { X = x; Y = y }
                                     ])
                                    ||> List.fold (fun grid tile ->
                                        TerrainGrid.add
                                            tile
                                            (if tile.Y = 29 || tile.Y = 30 then Swamp else Plain)
                                            grid)
                            }
                    }

                let declared =
                    { colony "W12S28" with
                        Outposts = [ outpostOf outpost.Capture ]
                    }

                let ranger =
                    body
                        "ranger-1-Spawn1"
                        Side.Ours
                        (List.concat (List.replicate 5 Fabot.Core.Decide.Bodies.rangerPattern.Block))
                        (RoomPos.at "W12S27" { X = 8; Y = 31 })
                        None

                let shooter =
                    body
                        "invader-1"
                        (Side.Npc "Invader")
                        Fabot.Core.Tests.Decide.Fixtures.smallRanged
                        (RoomPos.at "W12S27" { X = 8; Y = 28 })
                        (Some(Do [ Act.Move Right ]))

                let _, trace =
                    arena 900_000 [ home; outpost ] [ declared ] [ ranger; shooter ] |> run 20

                let failure = describe trace

                // Main stepped onto the band at t0 and again at t5, each time
                // held five ticks while the shooter walked off: it was shot
                // twice and lived.
                Expect.isEmpty
                    (pathOf "invader-1" trace
                     |> List.choose (fun (tick, target) ->
                         pathOf "ranger-1-Spawn1" trace
                         |> List.tryFind (fun (at, ranger) ->
                             at = tick
                             && ranger.Fatigue > 0
                             && RoomPos.range ranger.At target.At
                                |> Option.exists (fun r -> r > Engine.rangedRange))
                         |> Option.map fst))
                    $"its fatigue never holds it while the shooter stands out of its reach\n{failure}"

                Expect.equal
                    (diedOn "invader-1" trace)
                    (Some 12)
                    $"and it shoots it dead\n{failure}"
            }

            // #571, the user's cripple nobody chased: an Invader shot down to
            // its ATTACK and one HEAL in an outpost the stand-down shuts, no
            // body of ours there, no ranger anywhere, an observer's look on
            // the room. The owner's guard row wins it, so that row casts the
            // guard while the room is shut, and the guard hunts it down.
            test
                "#571: a cripple in a shut outpost its owner's guard row wins is hunted by the guard that row casts" {
                let home =
                    room "W12S28"
                    |> withController Ownership.Ours None 7 0
                    |> withSpawn "Spawn1" { X = 12; Y = 38 } 5_600

                let outpost = room "W12S27"

                let declared =
                    { colony "W12S28" with
                        Outposts = [ outpostOf outpost.Capture ]
                    }

                let lair = RoomPos.at "W12S27" { X = 20; Y = 30 }

                // 2 TOUGH, 4 MOVE, 4 ATTACK, 1 HEAL at 700 of 1,100 hits: the
                // TOUGH and two MOVE are gone.
                let cripple =
                    { body
                          "invader-cripple"
                          (Side.Npc "Invader")
                          (parts [ Tough, 2; Move, 4; Attack, 4; Heal, 1 ])
                          lair
                          (Some(Chase(Nearest, Some(lair, 4)))) with
                        Hits = 700
                        TicksToLive = 1_000
                    }

                // The economy is staffed: one hauler stands for it, so no
                // supply floor jumps the fighting row's queue.
                let hauler =
                    body
                        "hauler-1-Spawn1"
                        Side.Ours
                        (parts [ Carry, 32; Move, 16 ])
                        (RoomPos.at "W12S28" { X = 17; Y = 34 })
                        None

                let start =
                    let a =
                        arena 900_000 [ home; outpost ] [ declared ] [ cripple; hauler ]
                        |> withCasts [ "guard" ]

                    { a with
                        Lookout = Set.singleton "W12S27"
                        Carried =
                            { a.Carried with
                                Raids =
                                    Map.ofList
                                        [
                                            "W12S28",
                                            { Fabot.Core.Observe.RaidState.empty with
                                                Outposts =
                                                    [
                                                        {
                                                            Fabot.Core.Observe.OutpostEpisode.RoomName =
                                                                "W12S27"
                                                            Opened = 899_900
                                                            LastSeen = 899_999
                                                            Expiry = 901_000
                                                            Basis = StandDownBasis.InvaderRaid
                                                            Stronghold = false
                                                            Bounced = false
                                                            HolderUnread = false
                                                            Raiders =
                                                                Set.singleton "invader-cripple"
                                                            PlayerSince = None
                                                            Chased = false
                                                        }
                                                    ]
                                            }
                                        ]
                            }
                    }

                let _, trace = start |> run 300
                let failure = describe trace

                let born =
                    trace
                    |> List.collect (fun t ->
                        t.Events
                        |> List.choose (function
                            | Born id -> Some id
                            | _ -> None))

                Expect.exists born (fun id -> id.StartsWith "guard-") $"a guard is cast\n{failure}"

                Expect.isSome (diedOn "invader-cripple" trace) $"and the cripple dies\n{failure}"
            }

            test
                "W17S22 replay: two garrisons on Genie669 beside Vibe199 take 188 a tick, the replay's own hits" {
                // t878,960: Genie669 (6M4H) at 672, healing itself, Vibe199
                // (11M7H) beside it healing it, two garrisons at range 2-3.
                let shoot target = Some(Do [ Act.RangedAttack target ])

                let bodies =
                    [
                        body "g1" odiodin garrison (w17s22 24 12) (shoot "Genie669")
                        body "g2" odiodin garrison (w17s22 25 12) (shoot "Genie669")
                        { body
                              "Genie669"
                              trep
                              (parts [ Move, 6; Heal, 4 ])
                              (w17s22 27 13)
                              (Some(Do [ Act.Heal "Genie669" ])) with
                            Hits = 672
                        }
                        body
                            "Vibe199"
                            trep
                            trepHealer
                            (w17s22 28 13)
                            (Some(Do [ Act.Heal "Genie669" ]))
                    ]

                let _, trace = replay bodies |> run 2
                Expect.equal (hitsOf "Genie669" trace) [ 484; 296 ] "672→484→296, t878,960–962"
            }

            test "W17S22 replay: Vibe199 alone under two garrisons loses 236 a tick" {
                let bodies =
                    [
                        body
                            "g1"
                            odiodin
                            garrison
                            (w17s22 24 12)
                            (Some(Do [ Act.RangedAttack "Vibe199" ]))
                        body
                            "g2"
                            odiodin
                            garrison
                            (w17s22 25 12)
                            (Some(Do [ Act.RangedAttack "Vibe199" ]))
                        { body
                              "Vibe199"
                              trep
                              trepHealer
                              (w17s22 27 13)
                              (Some(Do [ Act.Heal "Vibe199" ])) with
                            Hits = 1640
                        }
                    ]

                let _, trace = replay bodies |> run 3

                Expect.equal
                    (hitsOf "Vibe199" trace)
                    [ 1404; 1168; 932 ]
                    "320 − 84 a tick, t878,966 on"
            }

            test "W17S23 replay: three garrisons on Failsafe204 take 396 a tick" {
                let shoot = Some(Do [ Act.RangedAttack "Failsafe204" ])

                let bodies =
                    [
                        body "g1" odiodin garrison (w17s22 24 12) shoot
                        body "g2" odiodin garrison (w17s22 25 12) shoot
                        body "g3" odiodin garrison (w17s22 26 11) shoot
                        { body
                              "Failsafe204"
                              trep
                              trepHealer
                              (w17s22 27 13)
                              (Some(Do [ Act.Heal "Failsafe204" ])) with
                            Hits = 1564
                        }
                    ]

                let _, trace = replay bodies |> run 3

                Expect.equal
                    (hitsOf "Failsafe204" trace)
                    [ 1168; 772; 376 ]
                    "1564→1168→772→376, t878,466–469"
            }

            test "W17S22 hold: three kiting garrisons, healers first, beat the melee squad" {
                let kite = Some(Kite(HealersFirst, 3))
                let controller = w17s22 30 14

                let bodies =
                    [
                        body "g1" odiodin garrison (w17s22 24 12) kite
                        body "g2" odiodin garrison (w17s22 25 12) kite
                        body "g3" odiodin garrison (w17s22 26 10) kite
                        body "Torque592" trep trepMelee (w17s22 30 20) (Some(Chase(Nearest, None)))
                        body "Vibe199" trep trepHealer (w17s22 30 21) (Some(Follow "Torque592"))
                        body
                            "Genie669"
                            trep
                            (parts [ Move, 6; Heal, 4 ])
                            (w17s22 30 22)
                            (Some(Follow "Vibe199"))
                        body
                            "Quake773"
                            trep
                            (parts [ Move, 2; BodyPart.Claim, 2 ])
                            (w17s22 28 28)
                            (Some(Tap controller))
                    ]

                let _, trace = replay bodies |> run 40

                let died id =
                    diedOn id trace |> Option.defaultValue System.Int32.MaxValue

                Expect.isLessThan
                    (died "Vibe199")
                    (died "Torque592")
                    $"Vibe199 before Torque592\n{describe trace}"

                Expect.isLessThan (died "Genie669") (died "Torque592") "Genie669 before Torque592"
                Expect.isSome (diedOn "Torque592" trace) "the melee dies"

                for g in [ "g1"; "g2"; "g3" ] do
                    Expect.isNone (diedOn g trace) $"{g} holds"
            }

            test
                "the W17S25 siege: one resident R7 cannot break the raid, and is never caught beside a melee" {
                let ranger = "ranger-879853-Spawn8"

                // Two starts: walking in from the mother, as the live body did,
                // and standing on the controller's ring when the block parks.
                for start in [ RoomPos.at "W17S26" { X = 10; Y = 2 }; w17s25 20 38 ] do
                    let _, trace = siege [ body ranger Side.Ours r7 start None ] |> run 60

                    // (a) The live outcome: nothing of the raid dies, the
                    // tapper lands on the controller. Since S0 (#451) the
                    // resident holds safe ground rather than dying.
                    for id in raidIds do
                        Expect.isNone
                            (diedOn id trace)
                            $"{id} stands, from {start}\n{describe trace}"

                    Expect.isTrue
                        (trace
                         |> List.exists (fun t ->
                             t.Events |> List.contains (ControllerAttacked("W17S25", "Rune908"))))
                        "the tapper lands on the controller"

                    // (b) S0's kite ground: never a tick ended beside a melee.
                    Expect.isEmpty
                        (caughtBeside [ "Eternity536"; "Prime803" ] ranger trace)
                        $"caught beside a melee, from {start}\n{describe trace}"

                    Expect.isNone (diedOn ranger trace) "the resident lives"
            }

        ]

let private w18s25 (x: int) (y: int) = RoomPos.at "W18S25" { X = x; Y = y }

/// W17S25 as W17S26's declared outpost, with W18S25 beyond its west exits:
/// two runs of five tiles at x 0, y 15–19 and y 24–28 (#450).
let private westBorder (bodies: Body list) =
    let mother =
        room "W17S26"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn8" { X = 20; Y = 26 } 5600

    let outpost = room "W17S25"

    let declared =
        { colony "W17S26" with
            Outposts = [ outpostOf outpost.Capture ]
        }

    arena 881_000 [ mother; outpost; room "W18S25" ] [ declared ] bodies

/// The siege world's nursery with W18S25 beside it, for a resident ranger.
let private nurseryWest (bodies: Body list) =
    let mother =
        room "W17S26"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn8" { X = 20; Y = 26 } 5600

    let child = room "W17S25" |> withController Ownership.Ours None 1 0

    let colonies =
        [
            colony "W17S26"
            { colony "W17S25" with
                Mother = Some "W17S26"
            }
        ]

    arena 881_000 [ mother; child; room "W18S25" ] colonies bodies

/// The rooms one body stood in, tick by tick.
let private roomsOf (id: string) (trace: TickTrace list) =
    pathOf id trace |> List.map (fun (_, s) -> s.At.Room) |> List.distinct

/// The ticks a body crossed into a room on.
let private crossingsInto (id: string) (roomName: string) (trace: TickTrace list) =
    trace
    |> List.filter (fun t ->
        t.Events
        |> List.exists (function
            | Crossed(who, _, landing) -> who = id && landing.Room = roomName
            | _ -> false))
    |> List.map (fun t -> t.Tick)

// ---- arena capture two-raids-beside-w17s25 (#532) ---------------------------
// `npm run capture -- W17S25 972617 --rooms W18S25,W17S26,W15S28 --name two-raids-beside-w17s25`.
// The room-history replay at t972,617, the state after that tick ran: arena
// tick 0 runs as t972,618.
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/972600.json
//   W18S25  https://screeps.com/room-history/shardSeason/W18S25/972600.json
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/972600.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/972600.json
// Left out, as the capture says: Healers, ExitWatches and Sightings start
// empty, and the Raid log too: its live ring opened no stand-down after
// t970,910, and its threat and fight latches for the tick were since
// overwritten. W18S25's reservation, roads, drops and ruins are not placed.
// Assignments off the Transition log, read t972,742.

/// W17S25 at t972,617 (#531's "Why"): Crunchy047, 14 live ATTACK on one live
/// MOVE, in the west outpost W18S25; Wraith403, its RANGED_ATTACK shot off and
/// healing itself, in the south outpost W17S26 beside our melee guard; two
/// resident rangers by the controller, cast by the mother W15S28. Their
/// bodies stand still (`Hold`).
let private twoRaidsBesideW17S25 () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 38_961 70_919
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in
                    [
                        10, 24
                        10, 26
                        10, 28
                        10, 30
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 28
                        16, 30
                        17, 29
                        17, 31
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 76_200 250_000 { X = 16; Y = 34 } with
                    Energy = 45
                }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 288
                }
                { structureOf "rampart" (Some Side.Ours) 92_821 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 972_630
                }
                { structureOf "rampart" (Some Side.Ours) 92_761 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 972_710
                }
                { structureOf "rampart" (Some Side.Ours) 94_601 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 972_713
                }
                { structureOf "rampart" (Some Side.Ours) 95_431 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 972_619
                }
                { structureOf "rampart" (Some Side.Ours) 95_071 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 972_690
                }
                { structureOf "rampart" (Some Side.Ours) 94_831 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 972_692
                }
                { structureOf "rampart" (Some Side.Ours) 91_121 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 972_717
                }
                { structureOf "rampart" (Some Side.Ours) 95_301 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 972_679
                }
                { structureOf "rampart" (Some Side.Ours) 91_901 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 972_696
                }
                { structureOf "rampart" (Some Side.Ours) 92_661 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 972_711
                }
                { structureOf "rampart" (Some Side.Ours) 93_161 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 972_660
                }
                { structureOf "rampart" (Some Side.Ours) 10_601 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 972_695
                }
                { structureOf "rampart" (Some Side.Ours) 901 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 972_671
                }
                { structureOf "rampart" (Some Side.Ours) 94_521 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 972_659
                }
                { structureOf "rampart" (Some Side.Ours) 34_401 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 972_716
                }
                { structureOf "rampart" (Some Side.Ours) 94_501 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 972_661
                }
                { structureOf "rampart" (Some Side.Ours) 98_501 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 972_657
                }
                { structureOf "rampart" (Some Side.Ours) 93_271 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 972_670
                }
                { structureOf "rampart" (Some Side.Ours) 91_881 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 972_688
                }
                towerOf Side.Ours { X = 13; Y = 29 } 840
            ]
        |> withSite Side.Ours "storage" None 2_670 { X = 13; Y = 27 }

    let w18s25 =
        room "W18S25"
        |> withStructures [ structureOf "container" None 120_100 250_000 { X = 31; Y = 5 } ]

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 27; Y = 9 } with
                    Energy = 510
                }
            ]

    // The mother: the residents are Spawn3's, so hers and not the child's.
    // Without her the child adopts them and lends them at once.
    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_739_967
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 30
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 27
                        13, 29
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        15, 35
                        16, 26
                        16, 32
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        18, 34
                        19, 27
                        19, 33
                        20, 26
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        21, 35
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 6; Y = 29 } with
                    Energy = 324
                }
                { structureOf "container" None 125_100 250_000 { X = 11; Y = 18 } with
                    Energy = 624
                }
                structureOf "container" None 153_100 250_000 { X = 23; Y = 29 }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 19; Y = 31 } with
                    Energy = 50
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 100_901 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 972_641
                }
                { structureOf "rampart" (Some Side.Ours) 99_401 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 972_700
                }
                { structureOf "rampart" (Some Side.Ours) 100_601 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 972_659
                }
                { structureOf "rampart" (Some Side.Ours) 100_701 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 972_626
                }
                { structureOf "rampart" (Some Side.Ours) 101_301 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 972_701
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 972_655
                }
                { structureOf "rampart" (Some Side.Ours) 100_301 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 972_676
                }
                { structureOf "rampart" (Some Side.Ours) 101_601 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 972_625
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 972_717
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 31_076
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]

    // Today's declarations, not the tick's.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            { body
                  "ranger-971130-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 37)
                  None with
                TicksToLive = 137
            }
            { body
                  "ranger-971410-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 417
            }
            { body
                  "worker-972000-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 7; Move, 6 ])
                  (at "W17S25" 5 20)
                  None with
                TicksToLive = 935
                Energy = 184
            }
            { body
                  "worker-972372-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 18 37)
                  None with
                TicksToLive = 1_313
                Energy = 15
            }
            { body
                  "anchor-972458-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 1_369
                Energy = 50
            }
            { body
                  "anchor-972558-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 42)
                  None with
                TicksToLive = 1_469
            }
            { body
                  "reserver-972604-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S25" 12 25)
                  None with
                TicksToLive = 597
            }
            { body
                  "worker-971946-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 7; Move, 6 ])
                  (at "W17S25" 17 45)
                  None with
                TicksToLive = 881
                Energy = 350
            }
            // live: 14 ATTACK, 1 MOVE
            { body
                  "Crunchy047"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W18S25" 40 13)
                  (Some Hold) with
                Hits = 1_454
                TicksToLive = 1_033
                Fatigue = 15
            }
            { body
                  "worker-971252-Spawn11"
                  Side.Ours
                  (parts [ Work, 4; Carry, 6; Move, 5 ])
                  (at "W18S25" 38 14)
                  None with
                TicksToLive = 178
                Energy = 300
            }
            { body
                  "hauler-971381-Spawn11"
                  Side.Ours
                  (parts [ Carry, 12; Move, 6 ])
                  (at "W18S25" 38 15)
                  None with
                TicksToLive = 316
                Energy = 364
            }
            { body
                  "hauler-971755-Spawn11"
                  Side.Ours
                  (parts [ Carry, 14; Move, 7 ])
                  (at "W18S25" 38 10)
                  None with
                TicksToLive = 699
                Energy = 700
            }
            { body
                  "reserver-972084-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W18S25" 17 8)
                  None with
                TicksToLive = 71
            }
            { body
                  "anchor-972488-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W18S25" 28 9)
                  None with
                TicksToLive = 1_399
            }
            { body
                  "Synapse250"
                  trepidimous
                  (List.replicate 23 [ Move; Carry ] |> List.concat)
                  (at "W17S26" 26 9)
                  (Some Hold) with
                TicksToLive = 1_269
            }
            // live: 1 HEAL, 1 MOVE
            { body
                  "Wraith403"
                  trepidimous
                  (parts [ Move, 1; RangedAttack, 1; Move, 1; Heal, 1; Move, 1 ])
                  (at "W17S26" 13 19)
                  (Some Hold) with
                Hits = 188
                TicksToLive = 1_299
                Fatigue = 12
            }
            // live: 2 MOVE, 5 WORK
            { body
                  "Torpedo487"
                  trepidimous
                  (parts [ Move, 3; Work, 5 ])
                  (at "W17S26" 27 9)
                  (Some Hold) with
                Hits = 620
                TicksToLive = 1_392
            }
            { body
                  "anchor-972054-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 24 5)
                  None with
                TicksToLive = 965
                Energy = 50
            }
            // live: 1 TOUGH, 5 MOVE, 3 ATTACK, 1 HEAL
            { body
                  "guard-972528-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S26" 12 19)
                  None with
                Hits = 970
                TicksToLive = 1_439
            }
            { body
                  "hauler-971818-Spawn11"
                  Side.Ours
                  (parts [ Carry, 14; Move, 7 ])
                  (at "W17S26" 1 15)
                  None with
                TicksToLive = 762
            }
            { body
                  "worker-971258-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 289
                Energy = 534
            }
            { body
                  "anchor-971904-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 809
                Energy = 50
            }
            { body
                  "upgrader-972317-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 1_345
            }
            { body
                  "hauler-972400-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 14 27)
                  None with
                TicksToLive = 1_425
            }
            { body
                  "ranger-972464-Spawn8"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W15S28" 6 7)
                  None with
                TicksToLive = 1_471
            }
            { body
                  "anchor-972544-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 12 17)
                  None with
                TicksToLive = 1_449
                Fatigue = 2
            }
            { body
                  "worker-972250-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 4 5)
                  None with
                TicksToLive = 1_281
                Energy = 807
            }
        ]

    let a = arena 972_618 [ w17s25; w18s25; w17s26; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before
                // t972,617; ranger-971130's off its timeline read at t972,742
                // (matched guard:W17S25 at t972,500).
                Assignments =
                    Map.ofList
                        [
                            "ranger-971130-Spawn3", "guard:W17S25"
                            "ranger-971410-Spawn3", "guard:W17S25"
                            "worker-972000-Spawn11", "repair:rampart-2-20"
                            "worker-972372-Spawn11", "withdraw:container-27-9"
                            "anchor-972458-Spawn11", "harvest:6a8caa7cdd4872bccd318c75"
                            "anchor-972558-Spawn11", "harvest:6a8caa7ddd4872bccd318c78"
                            "hauler-971755-Spawn11", "refill:spawn-Spawn11"
                            "anchor-972488-Spawn11", "harvest:6a8caa71dd4872bccd318b28"
                            "guard-972528-Spawn11", "guard:W17S26"
                        ]
            }
    }

// ---- arena capture shut-outpost-cripple (#534, the world layer's S0) -------
// `npm run capture -- W18S25 973320 --rooms W17S25,W15S28,W17S26 --name shutOutpostCripple`.
// The room-history replay at t973,320, the state after that tick ran: arena
// tick 0 runs as t973,321.
//   W18S25  https://screeps.com/room-history/shardSeason/W18S25/973300.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/973300.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/973300.json
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/973300.json
// Read at live t974,751, too late for live Memory: W17S25's stand-down of
// W18S25 is seated by hand off #534's "Why" (opened t973,265 on a Lose while
// Shade461 healed Idol768, held to t973,465). Healers, ExitWatches and
// Sightings start empty. W18S25's and W17S26's reservations, roads and
// tombstones are not placed; spawning bodies (a defender and a ranger due
// t973,355, a hauler t973,409) are not placed. The Transition log held no
// verdict that far back: the resident's and W17S26's guard's Tasks are
// seated off the t972,617 capture, every other body starts idle.

/// W18S25 at t973,320 (#534's "Why"): the healer gone, Idol768 stands alone
/// at 818 hits with 8 live ATTACK and 1 live MOVE in the child W17S25's
/// outpost, which W17S25's stand-down holds shut; the mother W15S28's
/// resident ranger-972663-Spawn3 stands in W17S25 next door.
let private shutOutpostCripple () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"
    let odiodin = Side.Player "Odiodin"

    let w18s25 =
        room "W18S25"
        |> withStructures
            [
                { structureOf "container" None 120_100 250_000 { X = 31; Y = 5 } with
                    Energy = 796
                }
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 38_258 70_919
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in [ 10, 24; 12, 30; 13, 31; 14, 30 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        10, 26
                        10, 28
                        10, 30
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        13, 25
                        14, 26
                        15, 27
                        15, 29
                        15, 31
                        16, 28
                        16, 30
                        17, 29
                        17, 31
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 66_200 250_000 { X = 16; Y = 34 } with
                    Energy = 100
                }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 348
                }
                { structureOf "rampart" (Some Side.Ours) 90_721 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 973_323
                }
                { structureOf "rampart" (Some Side.Ours) 90_661 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 973_403
                }
                { structureOf "rampart" (Some Side.Ours) 99_711 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 973_406
                }
                { structureOf "rampart" (Some Side.Ours) 93_031 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 973_411
                }
                { structureOf "rampart" (Some Side.Ours) 92_971 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 973_383
                }
                { structureOf "rampart" (Some Side.Ours) 98_231 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 973_385
                }
                { structureOf "rampart" (Some Side.Ours) 99_221 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 973_410
                }
                { structureOf "rampart" (Some Side.Ours) 93_201 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 973_372
                }
                { structureOf "rampart" (Some Side.Ours) 89_801 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 973_389
                }
                { structureOf "rampart" (Some Side.Ours) 90_561 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 973_404
                }
                { structureOf "rampart" (Some Side.Ours) 91_061 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 973_353
                }
                { structureOf "rampart" (Some Side.Ours) 8_501 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 973_388
                }
                { structureOf "rampart" (Some Side.Ours) 1_201 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 973_364
                }
                { structureOf "rampart" (Some Side.Ours) 92_421 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 973_352
                }
                { structureOf "rampart" (Some Side.Ours) 32_301 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 973_409
                }
                { structureOf "rampart" (Some Side.Ours) 92_401 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 973_354
                }
                { structureOf "rampart" (Some Side.Ours) 96_401 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 973_350
                }
                { structureOf "rampart" (Some Side.Ours) 91_171 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 973_363
                }
                { structureOf "rampart" (Some Side.Ours) 91_781 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 973_381
                }
                towerOf Side.Ours { X = 13; Y = 29 } 940
            ]
        |> withSite Side.Ours "storage" None 13_863 { X = 13; Y = 27 }

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_753_992
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 30
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 27
                        13, 29
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        15, 35
                        16, 26
                        16, 32
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        18, 34
                        19, 27
                        19, 31
                        19, 33
                        20, 26
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        21, 35
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 6; Y = 29 } with
                    Energy = 792
                }
                { structureOf "container" None 125_100 250_000 { X = 11; Y = 18 } with
                    Energy = 756
                }
                structureOf "container" None 148_100 250_000 { X = 23; Y = 29 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 102_001 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 973_334
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 973_393
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 973_352
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 973_384
                }
                { structureOf "rampart" (Some Side.Ours) 102_401 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 973_394
                }
                { structureOf "rampart" (Some Side.Ours) 101_901 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 973_348
                }
                { structureOf "rampart" (Some Side.Ours) 101_401 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 973_369
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 973_417
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 973_410
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 28_361
                    Thorium = 41_977
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 } with
                    Energy = 346
                }
            ]

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 27; Y = 9 } with
                    Energy = 480
                }
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            // Idol768: live: 8 ATTACK, 1 MOVE; it wanders W18S25 until a
            // resident kills it after t973,430, so it holds here.
            { body
                  "Idol768"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W18S25" 44 13)
                  (Some Hold) with
                Hits = 818
                TicksToLive = 1_051
                Fatigue = 13
            }
            { body
                  "hauler-971818-Spawn11"
                  Side.Ours
                  (parts [ Carry, 14; Move, 7 ])
                  (at "W17S25" 18 30)
                  None with
                TicksToLive = 59
            }
            { body
                  "worker-971946-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 7; Move, 6 ])
                  (at "W17S25" 29 34)
                  None with
                TicksToLive = 178
                Energy = 325
            }
            { body
                  "worker-972000-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 7; Move, 6 ])
                  (at "W17S25" 29 33)
                  None with
                TicksToLive = 232
                Fatigue = 86
                Energy = 285
            }
            { body
                  "anchor-972054-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 16 42)
                  None with
                TicksToLive = 262
                Energy = 50
            }
            { body
                  "worker-972372-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 24 35)
                  None with
                TicksToLive = 610
                Energy = 314
            }
            { body
                  "anchor-972458-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 666
                Energy = 50
            }
            { body
                  "guard-972528-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 17 45)
                  None with
                TicksToLive = 736
            }
            { body
                  "ranger-972663-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 967
            }
            { body
                  "hauler-972861-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 9 24)
                  None with
                TicksToLive = 1_111
                Energy = 552
            }
            { body
                  "guard-973227-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 19 30)
                  None with
                TicksToLive = 1_435
            }
            // scout-W17S22-972936: live: no act
            { body "scout-W17S22-972936" odiodin (parts [ Move, 1 ]) (at "W17S25" 13 24) (Some Hold) with
                TicksToLive = 1_117
            }
            { body
                  "anchor-972488-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 3 18)
                  None with
                TicksToLive = 696
                Energy = 50
            }
            { body
                  "hauler-971764-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 18 29)
                  None with
                TicksToLive = 86
            }
            { body
                  "anchor-971904-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 106
                Energy = 50
            }
            { body
                  "worker-972250-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 578
                Energy = 594
            }
            { body
                  "upgrader-972317-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 642
            }
            { body
                  "hauler-972400-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 37 16)
                  None with
                TicksToLive = 722
            }
            { body
                  "anchor-972544-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 746
                Energy = 50
            }
            { body
                  "hauler-972938-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 11 28)
                  None with
                TicksToLive = 1_260
            }
            { body
                  "anchor-972558-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 766
                Energy = 50
            }
            { body
                  "reserver-973148-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 9 22)
                  None with
                TicksToLive = 438
            }
        ]

    let a = arena 973_321 [ w18s25; w17s25; w15s28; w17s26 ] colonies bodies

    { a with
        // W13S28's observer, which the arena does not hold, on W18S25 as live.
        Lookout = set [ "W18S25" ]
        Carried =
            { a.Carried with
                Assignments =
                    Map.ofList
                        [
                            "ranger-972663-Spawn3", "guard:W17S25"
                            "guard-972528-Spawn11", "guard:W17S26"
                        ]
                Raids =
                    Map.ofList
                        [
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            RoomName = "W18S25"
                                            Opened = 973_265
                                            LastSeen = 973_265
                                            Expiry = 973_465
                                            Basis = StandDownBasis.PlayerRaidHold
                                            Stronghold = false
                                            Raiders = set [ "Idol768"; "Shade461" ]
                                            PlayerSince = Some 973_265
                                            Chased = false
                                            Bounced = false
                                            HolderUnread = false
                                        }
                                    ]
                            }
                        ]
            }
    }

// ---- arena capture w15s27-t974800 (#536) ------------------------------------
// `npm run capture -- W15S27 974800 --rooms W15S28`, read at live t975,006.
// The room-history replay at t974,800, the state after that tick ran: arena
// tick 0 runs as t974,801.
//   W15S27  https://screeps.com/room-history/shardSeason/W15S27/974800.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/974800.json
// Healers, ExitWatches and Sightings start empty; the Raid log, ArmedSeen and
// Towered are live Memory's at t975,006, less what postdates the tick.
// W15S27's reservation, 178 roads and a tombstone are not placed; spawning
// bodies (a hauler due t974,875, a ranger t974,888) are not placed.

/// W15S27 at t974,800 (#536's "Why"): invader_W15S27_250, 10 hits left and
/// one live MOVE, stands rooted on (6,36), the first diagonal step of
/// anchor-974661-Spawn3's line from (5,37) to its seat on the container at
/// (13,29). Live, the anchor stepped into it for ~230 ticks.
let private rootedInvaderOnAnchorStep () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w15s27 =
        room "W15S27"
        |> withStructures [ structureOf "container" None 130_800 250_000 { X = 13; Y = 29 } ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_779_864
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 26
                        12, 30
                        12, 32
                        12, 34
                        12, 36
                        13, 25
                        13, 27
                        13, 29
                        13, 31
                        13, 33
                        13, 35
                        14, 26
                        14, 28
                        14, 30
                        14, 32
                        14, 34
                        15, 25
                        15, 27
                        15, 29
                        15, 31
                        15, 33
                        15, 35
                        16, 26
                        16, 30
                        16, 32
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        19, 27
                        19, 31
                        20, 26
                        20, 28
                        20, 30
                        21, 27
                        21, 29
                        22, 26
                        23, 25
                        23, 27
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 18, 34; 19, 33; 20, 32; 21, 31; 21, 33; 21, 35; 23, 35 ] ->
                    structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 6; Y = 29 } with
                    Energy = 492
                }
                { structureOf "container" None 195_100 250_000 { X = 11; Y = 18 } with
                    Energy = 864
                }
                structureOf "container" None 133_100 250_000 { X = 23; Y = 29 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 100_701 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 974_819
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 974_878
                }
                { structureOf "rampart" (Some Side.Ours) 102_001 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 974_837
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 974_814
                }
                { structureOf "rampart" (Some Side.Ours) 101_101 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 974_879
                }
                { structureOf "rampart" (Some Side.Ours) 100_601 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 974_833
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 974_854
                }
                { structureOf "rampart" (Some Side.Ours) 101_401 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 974_803
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 974_895
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 29_322
                    Thorium = 41_977
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 } with
                    Energy = 701
                }
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W15S28" ])

    let bodies: Body list =
        [
            // Live: 1 MOVE; it swung at the anchor on (5,37), which the arena's
            // hits-ordered parts leave it no ATTACK to do. The invader AI
            // walks it no nearer than 3 to a hostile without one (#541).
            { body
                  "invader_W15S27_250"
                  (Side.Npc "Invader")
                  (parts [ Tough, 2; Move, 4; RangedAttack, 1; Work, 1; Attack, 1; Move, 1 ])
                  (at "W15S27" 6 36)
                  (Some Invader) with
                Hits = 10
                TicksToLive = 1_287
            }
            { body
                  "reserver-974641-Spawn3"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W15S27" 5 9)
                  None with
                TicksToLive = 445
            }
            { body
                  "anchor-974661-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S27" 5 37)
                  None with
                TicksToLive = 1_389
            }
            { body
                  "hauler-973265-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 33 16)
                  None with
                TicksToLive = 107
            }
            { body
                  "anchor-973374-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 96
                Energy = 50
            }
            { body
                  "anchor-973647-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 5 1)
                  None with
                TicksToLive = 375
                Energy = 50
            }
            { body
                  "worker-973746-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 23 28)
                  None with
                TicksToLive = 594
                Energy = 338
            }
            { body
                  "upgrader-973875-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 21 24)
                  None with
                TicksToLive = 720
            }
            { body
                  "hauler-973896-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 16 35)
                  None with
                TicksToLive = 738
                Energy = 640
            }
            { body
                  "anchor-974040-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 762
                Energy = 50
            }
            { body
                  "brawler-974217-Spawn3"
                  Side.Ours
                  (parts [ Move, 25; Attack, 25 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 1_065
            }
            { body
                  "hauler-974425-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 14 27)
                  None with
                TicksToLive = 1_267
            }
            { body
                  "worker-974514-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 15 34)
                  None with
                TicksToLive = 1_362
                Energy = 680
            }
            { body
                  "guard-974611-Spawn3"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W15S28" 7 0)
                  None with
                TicksToLive = 1_339
            }
        ]

    let a = arena 974_801 [ w15s27; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before t974,800,
                // read at live t975,006; it held none that early for
                // hauler-973265, anchor-973374 and brawler-974217.
                Assignments =
                    Map.ofList
                        [
                            "reserver-974641-Spawn3", "reserve:6a8caa95dd4872bccd319010"
                            "anchor-974661-Spawn3", "harvest:6a8caa95dd4872bccd319011"
                            "worker-973746-Spawn3", "upgrade:6a8caa95dd4872bccd319015"
                            "hauler-973896-Spawn3", "refill:spawn-Spawn3"
                            "anchor-974040-Spawn3", "harvest:6a8caa95dd4872bccd319013"
                            "hauler-974425-Spawn3", "withdraw:container-11-18"
                            "worker-974514-Spawn8", "refill:spawn-Spawn3"
                            "guard-974611-Spawn3", "guard:W18S25"
                        ]
                ArmedSeen = Map.ofList [ "W15S27", 974_694 ]
                Towered = Set.ofList [ "W15S28" ]
                Raids =
                    Map.ofList
                        [
                            "W15S28",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 974_800
                                            Expiry = 977_506
                                            Basis = StandDownBasis.Fallback
                                            Stronghold = true
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            Bounced = false
                                            HolderUnread = false
                                        }
                                    ]
                            }
                        ]
            }
    }

// ---- arena capture w17s26-t979950 (#551) ------------------------------------
// `npm run capture -- W17S26 979950 --rooms W17S25,W15S28`, read at live t980,066.
// The room-history replay at t979,950, the state after that tick ran: arena
// tick 0 runs as t979,951.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/979900.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/979900.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/979900.json
// Healers, ExitWatches and Sightings start empty; the Raid log, ArmedSeen and
// Towered are live Memory's at t980,066, less what postdates the tick.
// W17S26's reservation, 262 roads and 2 tombstones are not placed; a ranger
// spawning at Spawn3 until t980,040 is not placed.

/// A body that steps one way on even arena ticks and back on odd ones.
let private swaying (out: Direction) (back: Direction) =
    Phases [ for t in 0..99 -> t + 1, Do [ Act.Move(if t % 2 = 0 then out else back) ] ]

/// W17S26 at t979,950 (#551's "Why"): Trepidimous' Onyx632 sways between
/// (22,3) and (21,2), and Morgana682 between (21,4) and (22,4), every tick
/// (live t979,951-979,970), round hauler-979756-Spawn11 at (21,3) on its way
/// to the container at (27,9). Live, the hauler stepped every tick onto the
/// tile one of them was entering, the engine refused it, and it stood.
let private swayingTrespassers () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let rampart hits hitsMax (x, y, decay) =
        { structureOf "rampart" (Some Side.Ours) hits hitsMax { X = x; Y = y } with
            NextDecay = decay
        }

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 27; Y = 9 } with
                    Energy = 1_088
                }
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 39_940 121_695
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in
                    [
                        10, 24
                        10, 26
                        10, 28
                        10, 30
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 28
                        16, 30
                        17, 29
                        17, 31
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                structureOf "container" None 166_600 250_000 { X = 16; Y = 34 }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 378
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 8_339
                }
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]
        |> withStructures
            [
                for hits, tile in
                    [
                        91_021, (2, 15, 979_956)
                        91_861, (2, 16, 980_036)
                        99_561, (2, 17, 980_039)
                        99_631, (2, 18, 980_044)
                        90_271, (2, 19, 980_016)
                        99_731, (2, 20, 980_018)
                        99_521, (2, 23, 980_043)
                        93_501, (2, 24, 980_005)
                        92_401, (2, 25, 980_022)
                        92_061, (2, 26, 980_037)
                        92_561, (2, 27, 979_986)
                        82_401, (13, 27, 980_036)
                        62_201, (13, 29, 980_021)
                        13_501, (14, 28, 979_997)
                        93_921, (16, 44, 979_985)
                        12_201, (17, 20, 980_042)
                        93_901, (17, 44, 979_987)
                        97_301, (18, 44, 979_983)
                        91_531, (32, 31, 979_996)
                        90_481, (32, 32, 980_014)
                    ] -> rampart hits 3_000_000 tile
            ]
        |> withSite Side.Ours "road" None 682 { X = 33; Y = 30 }

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 149_217 5_857_785
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [ 12, 30; 13, 25; 13, 31; 14, 26; 14, 30; 15, 27; 15, 29; 15, 31; 16, 30 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 36
                        13, 27
                        13, 29
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        15, 35
                        16, 26
                        16, 32
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        18, 34
                        19, 27
                        19, 31
                        19, 33
                        20, 26
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        21, 35
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 177_100 250_000 { X = 6; Y = 29 } with
                    Energy = 1_578
                }
                { structureOf "container" None 145_100 250_000 { X = 11; Y = 18 } with
                    Energy = 718
                }
                structureOf "container" None 142_300 250_000 { X = 23; Y = 29 }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 24 } with
                    Energy = 100
                    Id = "extension-12-24-W15S28"
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 26 } with
                    Energy = 100
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 32 } with
                    Energy = 100
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 34 } with
                    Energy = 1
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 14_513
                    Thorium = 41_977
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 } with
                    Energy = 1_688
                }
            ]
        |> withStructures
            [
                for hits, tile in
                    [
                        99_501, (6, 29, 979_967)
                        101_201, (11, 18, 980_026)
                        97_601, (16, 28, 979_985)
                        88_501, (17, 29, 980_044)
                        98_301, (17, 31, 980_027)
                        97_801, (17, 35, 979_981)
                        98_901, (18, 30, 980_002)
                        98_301, (19, 29, 980_050)
                        100_301, (20, 34, 980_043)
                    ] -> rampart hits 100_000_000 tile
            ]

    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let hauler (a, b) = parts [ Move, a; Carry, b ]

    let bodies: Body list =
        [
            // Live: sways (23,12)-(24,11), no act.
            { body
                  "Gambit229"
                  trepidimous
                  (hauler (23, 23))
                  (at "W17S26" 23 12)
                  (Some(swaying TopRight BottomLeft)) with
                Hits = 4_160
                TicksToLive = 915
            }
            // Live: no act.
            { body
                  "Infinity425"
                  trepidimous
                  (parts [ Move, 3; BodyPart.Claim, 3 ])
                  (at "W17S26" 10 23)
                  (Some Hold) with
                TicksToLive = 91
                Fatigue = 18
            }
            // Live 3 MOVE, 5 WORK, swaying on a road the arena does not lay:
            // two MOVE more keep its step every tick on plain.
            { body
                  "Onyx632"
                  trepidimous
                  (parts [ Move, 5; Work, 5 ])
                  (at "W17S26" 22 3)
                  (Some(swaying TopLeft BottomRight)) with
                TicksToLive = 1_204
            }
            // Live: sways (21,4)-(22,4), no act.
            { body
                  "Morgana682"
                  trepidimous
                  (hauler (23, 23))
                  (at "W17S26" 21 4)
                  (Some(swaying Right Left)) with
                TicksToLive = 1_368
            }
            { body
                  "anchor-978575-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 153
                Energy = 50
            }
            { body
                  "guard-978780-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S26" 26 7)
                  None with
                TicksToLive = 358
            }
            { body
                  "reserver-979607-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 9 22)
                  None with
                TicksToLive = 267
            }
            { body
                  "hauler-979756-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S26" 21 3)
                  None with
                TicksToLive = 1_376
            }
            { body
                  "anchor-978551-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 123
                Energy = 50
            }
            { body
                  "defender-978831-Spawn11"
                  Side.Ours
                  (parts [ Move, 10; Attack, 10 ])
                  (at "W17S25" 4 19)
                  None with
                TicksToLive = 439
            }
            { body
                  "guard-979625-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 1 15)
                  None with
                TicksToLive = 1_203
            }
            { body
                  "upgrader-979829-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 19 30)
                  None with
                TicksToLive = 1_428
            }
            { body
                  "hauler-978947-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 12 29)
                  None with
                TicksToLive = 567
            }
            { body
                  "guard-979116-Spawn8"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W15S28" 48 20)
                  None with
                TicksToLive = 694
            }
            { body
                  "anchor-979625-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_203
                Energy = 50
            }
            { body
                  "ranger-979811-Spawn8"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W15S28" 20 20)
                  None with
                TicksToLive = 1_485
            }
            { body
                  "anchor-979882-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 14 15)
                  None with
                TicksToLive = 1_454
            }
            { body
                  "hauler-979425-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 35 14)
                  None with
                TicksToLive = 1_117
                Energy = 1_600
            }
            { body
                  "hauler-979281-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 36 13)
                  None with
                TicksToLive = 973
                Energy = 536
            }
            { body
                  "worker-979732-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 9 40)
                  None with
                TicksToLive = 1_430
                Energy = 850
            }
        ]

    let a = arena 979_951 [ w17s26; w17s25; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before t979,950,
                // read at live t980,066; it held none that early for
                // ranger-979811-Spawn8.
                Assignments =
                    Map.ofList
                        [
                            "anchor-978575-Spawn11", "harvest:6a8caa7ddd4872bccd318c78"
                            "guard-978780-Spawn11", "guard:W17S26"
                            "reserver-979607-Spawn11", "reserve:6a8caa7ddd4872bccd318c79"
                            "hauler-979756-Spawn11", "withdraw:container-27-9"
                            "anchor-978551-Spawn11", "harvest:6a8caa7cdd4872bccd318c75"
                            "hauler-978947-Spawn11", "withdraw:container-27-9"
                            "anchor-979625-Spawn8", "harvest:6a8caa95dd4872bccd319014"
                            "anchor-979882-Spawn3", "harvest:6a8caa95dd4872bccd319013"
                            "hauler-979425-Spawn3", "refill:spawn-Spawn3"
                            "hauler-979281-Spawn3", "refill:spawn-Spawn3"
                            "worker-979732-Spawn3", "refill:spawn-Spawn3"
                        ]
                ArmedSeen = Map.ofList [ "W17S25", 979_698; "W17S26", 979_497 ]
                Towered = Set.ofList [ "W15S28"; "W17S25" ]
                Raids =
                    Map.ofList
                        [
                            "W15S28",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 979_950
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            Bounced = false
                                            HolderUnread = false
                                        }
                                    ]
                                Fought =
                                    Map.ofList
                                        [
                                            "W17S25",
                                            ({
                                                Seen = 979_698
                                                Raiders = Set.ofList [ "6ac3dbeec7926d7481bf9b87" ]
                                            }
                                            : FightRecord)
                                        ]
                            }
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                HomeRaided = Some 979_698
                            }
                        ]
            }
    }

// ---- arena capture t976,955 (#542) --------------------------------------------
// `npm run capture -- W17S26 976955 --rooms W17S25,W15S28`, read at live t977,069.
// The room-history replay at t976,955, the state after that tick ran: arena
// tick 0 runs as t976,956 and decides what our code decided live then.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/976900.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/976900.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/976900.json
// Specter836 Holds: live it dug on its seat. Healers and Sightings start empty
// (heap only); the ExitWatch is seeded below. The Raid log, ArmedSeen and
// Towered are live Memory's at t977,069, less what postdates the tick.
// W17S26 is reserved by us to t980242: the arena has no reservations.
// Spawn3 is spawning hauler-976850-Spawn3 until t976994: not placed.
// Spawn8 is spawning anchor-976950-Spawn8 until t976974: not placed.
// not placed: 257 road, 5 tombstone (the arena has no roads, drops or ruins).

/// Trepidimous' Specter836, a 5W3M digger, on W17S26's source beside our
/// anchor, while W17S26's north exit is still held for Paragon731, an armed
/// body seen leaving by it at t976,871, and nobody holds the Guard.
let private diggerBesideHeldExit () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 27; Y = 9 } with
                    Energy = 1_140
                }
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 40_000 90_767
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in
                    [
                        10, 24
                        10, 26
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        16, 30
                        17, 29
                        17, 31
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in [ 10, 28; 10, 30; 15, 27; 15, 31; 16, 28 ] ->
                    structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 91_600 250_000 { X = 16; Y = 34 } with
                    Energy = 1_984
                }
                { structureOf "container" None 125_200 250_000 { X = 17; Y = 20 } with
                    Energy = 408
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 15; Y = 29 } with
                    Energy = 26
                }
                { structureOf "rampart" (Some Side.Ours) 89_821 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 976_986
                }
                { structureOf "rampart" (Some Side.Ours) 99_261 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 976_967
                }
                { structureOf "rampart" (Some Side.Ours) 98_541 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 976_970
                }
                { structureOf "rampart" (Some Side.Ours) 99_031 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 976_975
                }
                { structureOf "rampart" (Some Side.Ours) 99_271 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 977_046
                }
                { structureOf "rampart" (Some Side.Ours) 97_331 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 977_048
                }
                { structureOf "rampart" (Some Side.Ours) 98_621 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 976_974
                }
                { structureOf "rampart" (Some Side.Ours) 92_301 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 977_035
                }
                { structureOf "rampart" (Some Side.Ours) 90_601 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 977_052
                }
                { structureOf "rampart" (Some Side.Ours) 90_561 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 976_968
                }
                { structureOf "rampart" (Some Side.Ours) 90_761 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 977_016
                }
                { structureOf "rampart" (Some Side.Ours) 71_701 3_000_000 { X = 13; Y = 27 } with
                    NextDecay = 976_967
                }
                { structureOf "rampart" (Some Side.Ours) 601 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 977_051
                }
                { structureOf "rampart" (Some Side.Ours) 22_501 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 977_027
                }
                { structureOf "rampart" (Some Side.Ours) 92_121 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 977_015
                }
                { structureOf "rampart" (Some Side.Ours) 21_501 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 976_973
                }
                { structureOf "rampart" (Some Side.Ours) 92_101 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 977_017
                }
                { structureOf "rampart" (Some Side.Ours) 95_501 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 977_013
                }
                { structureOf "rampart" (Some Side.Ours) 100_531 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 977_026
                }
                { structureOf "rampart" (Some Side.Ours) 99_481 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 977_044
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 9_820
                }
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_818_322
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 30
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 27
                        13, 29
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        15, 35
                        16, 26
                        16, 32
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        19, 27
                        19, 31
                        19, 33
                        20, 26
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        21, 35
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 125_500 250_000 { X = 6; Y = 29 } with
                    Energy = 516
                }
                { structureOf "container" None 175_100 250_000 { X = 11; Y = 18 } with
                    Energy = 810
                }
                structureOf "container" None 172_300 250_000 { X = 23; Y = 29 }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 18; Y = 34 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 102_101 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 976_997
                }
                { structureOf "rampart" (Some Side.Ours) 100_901 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 976_957
                }
                { structureOf "rampart" (Some Side.Ours) 101_801 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 977_015
                }
                { structureOf "rampart" (Some Side.Ours) 101_001 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 977_009
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 976_958
                }
                { structureOf "rampart" (Some Side.Ours) 102_001 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 977_011
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 977_032
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 976_981
                }
                { structureOf "rampart" (Some Side.Ours) 100_001 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 976_974
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 32_010
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            // Specter836: live, it dug its seat beside the anchor.
            { body
                  "Specter836"
                  trepidimous
                  (parts [ Move, 3; Work, 5 ])
                  (at "W17S26" 26 8)
                  (Some Hold) with
                TicksToLive = 1_403
            }
            { body
                  "anchor-975456-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 23
                Energy = 50
            }
            { body
                  "ranger-975348-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 37)
                  None with
                TicksToLive = 17
            }
            { body
                  "upgrader-975489-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 16 34)
                  None with
                TicksToLive = 83
                Energy = 42
            }
            { body
                  "anchor-975540-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 113
                Energy = 50
            }
            { body
                  "anchor-975570-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 16 44)
                  None with
                TicksToLive = 143
                Energy = 6
            }
            { body
                  "guard-975682-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 15 45)
                  None with
                TicksToLive = 255
            }
            { body
                  "guard-975730-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 7 20)
                  None with
                TicksToLive = 303
            }
            { body
                  "hauler-975863-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 14 27)
                  None with
                TicksToLive = 478
                Energy = 308
            }
            { body
                  "ranger-976065-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 734
            }
            { body
                  "hauler-976395-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 16 33)
                  None with
                TicksToLive = 1_010
                Energy = 247
            }
            { body
                  "hauler-976467-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 19 34)
                  None with
                TicksToLive = 1_082
            }
            { body
                  "upgrader-976611-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 17 33)
                  None with
                TicksToLive = 1_205
                Energy = 42
            }
            { body
                  "defender-976863-Spawn11"
                  Side.Ours
                  (parts [ Move, 10; Attack, 10 ])
                  (at "W17S25" 5 20)
                  None with
                TicksToLive = 1_466
            }
            { body
                  "reserver-976923-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S25" 17 43)
                  None with
                TicksToLive = 578
                Fatigue = 12
            }
            { body
                  "anchor-975498-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 71
                Energy = 50
            }
            { body
                  "upgrader-975527-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 217
            }
            { body
                  "worker-976003-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 696
                Energy = 258
            }
            { body
                  "worker-976518-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 29)
                  None with
                TicksToLive = 1_211
                Energy = 176
            }
            { body
                  "anchor-976603-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_176
                Energy = 50
            }
            { body
                  "hauler-975921-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 25 24)
                  None with
                TicksToLive = 608
                Energy = 1_600
            }
        ]

    let a = arena 976_956 [ w17s26; w17s25; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before t976,955,
                // read from Memory at live t977,069. No Guard of W17S26's: both
                // guards released it task-gone at t976,867. The log held none
                // for anchor-975456, ranger-975348, upgrader-975489,
                // defender-976863 and anchor-975498 that early, and
                // hauler-976467's withdraw names a tombstone the arena does not place.
                Assignments =
                    Map.ofList
                        [
                            "anchor-975540-Spawn11", "harvest:6a8caa7cdd4872bccd318c75"
                            "anchor-975570-Spawn11", "harvest:6a8caa7ddd4872bccd318c78"
                            "hauler-975863-Spawn11", "refill:spawn-Spawn11"
                            "ranger-976065-Spawn3", "guard:W17S25"
                            "hauler-976395-Spawn11", "refill:container-16-34"
                            "upgrader-976611-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "reserver-976923-Spawn11", "reserve:6a8caa7ddd4872bccd318c79"
                            "worker-976003-Spawn8", "upgrade:6a8caa95dd4872bccd319015"
                            "worker-976518-Spawn3", "upgrade:6a8caa95dd4872bccd319015"
                            "anchor-976603-Spawn8", "harvest:6a8caa95dd4872bccd319014"
                            "hauler-975921-Spawn3", "refill:storage-17-29"
                        ]
                // Heap only, so seeded: Paragon731 (18M 17A), seen on W17S26's
                // north exit at 6,0 at t976,871 and gone, held the run it left
                // by until t976,972, as live.
                ExitWatches =
                    Map.ofList
                        [
                            "W17S26",
                            ExitWatch.Held
                                {
                                    Run = [ for x in 3..21 -> { X = x; Y = 0 } ]
                                    Exit = { X = 6; Y = 0 }
                                    Until = 976_972
                                }
                        ]
                // From live Memory at t977,069, less what is stamped after the
                // tick: W15S28's stand-down of W16S26, seen t977,027, is seated
                // with its LastSeen clamped to the tick.
                ArmedSeen = Map.ofList [ "W17S25", 976_918; "W17S26", 976_871 ]
                Towered = Set.ofList [ "W15S28"; "W17S25" ]
                Raids =
                    Map.ofList
                        [
                            "W15S28",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 976_955
                                            Expiry = 979_527
                                            Basis = StandDownBasis.Fallback
                                            Stronghold = true
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            Bounced = false
                                            HolderUnread = false
                                        }
                                    ]
                            }
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                HomeRaided = Some 976_918
                            }
                        ]
            }
    }

// ---- arena capture t985,660 (#558) --------------------------------------------
// `npm run capture -- W17S26 985660 --rooms W17S25,W15S28`, read at live t985,731.
// The room-history replay at t985,660, the state after that tick ran: arena
// tick 0 runs as t985,661 and decides what our code decided live then.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/985600.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/985600.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/985600.json
// Obsidian522 Holds: live it stood parked. Healers, ExitWatches and Sightings
// start empty (heap only). The Raid log, ArmedSeen and Towered are live
// Memory's at t985,731, less what postdates the tick.
// W17S26 is reserved by us to t989217: the arena has no reservations.
// Spawn3 is spawning ranger-985616-Spawn3 until t985742: not placed.
// Spawn8 is spawning hauler-985545-Spawn8 until t985689: not placed.
// not placed: 263 road, 1 energy, 4 tombstone (the arena has no roads, drops or ruins).
// No task verdict that early for guard-985559-Spawn11, brawler-985442-Spawn3
// and hauler-984023-Spawn8; hauler-985043-Spawn11's withdraw named a tombstone.

/// Trepidimous' Obsidian522, a 23C22M hauler, parked in W17S26 eight tiles
/// from a level-0 invader core at (12,25), while two rangers hold
/// guard:W17S26: live they stood beside the core for 100+ ticks, shooting
/// nothing.
let private haulerBesideCore () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 120_000 250_000 { X = 27; Y = 9 } with
                    Energy = 180
                }
                structureOf
                    "invaderCore"
                    (Some(Side.Npc "Invader"))
                    66_670
                    100_000
                    { X = 12; Y = 25 }
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withControllerClocks 0 0 40_000 199_288
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in
                    [
                        10, 24
                        10, 26
                        10, 28
                        10, 30
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 28
                        16, 30
                        17, 29
                        17, 31
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                { structureOf "container" None 144_200 250_000 { X = 16; Y = 34 } with
                    Energy = 1_466
                }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 384
                }
                { structureOf "rampart" (Some Side.Ours) 100_021 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 985_698
                }
                { structureOf "rampart" (Some Side.Ours) 99_961 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 985_679
                }
                { structureOf "rampart" (Some Side.Ours) 100_461 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 985_682
                }
                { structureOf "rampart" (Some Side.Ours) 99_331 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 985_687
                }
                { structureOf "rampart" (Some Side.Ours) 99_541 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 985_758
                }
                { structureOf "rampart" (Some Side.Ours) 99_731 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 985_760
                }
                { structureOf "rampart" (Some Side.Ours) 99_821 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 985_686
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 985_747
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 985_665
                }
                { structureOf "rampart" (Some Side.Ours) 100_161 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 985_680
                }
                { structureOf "rampart" (Some Side.Ours) 99_161 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 985_728
                }
                { structureOf "rampart" (Some Side.Ours) 99_501 3_000_000 { X = 13; Y = 27 } with
                    NextDecay = 985_679
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 985_664
                }
                { structureOf "rampart" (Some Side.Ours) 99_801 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 985_739
                }
                { structureOf "rampart" (Some Side.Ours) 95_121 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 985_727
                }
                { structureOf "rampart" (Some Side.Ours) 99_301 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 985_685
                }
                { structureOf "rampart" (Some Side.Ours) 95_101 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 985_729
                }
                { structureOf "rampart" (Some Side.Ours) 94_901 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 985_725
                }
                { structureOf "rampart" (Some Side.Ours) 93_701 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 985_738
                }
                { structureOf "rampart" (Some Side.Ours) 93_191 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 985_756
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 9_102
                }
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 149_993 5_931_169
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 30
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 27
                        13, 29
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        15, 35
                        16, 26
                        16, 32
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 34
                        19, 27
                        21, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        18, 32
                        19, 31
                        19, 33
                        20, 26
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                structureOf "container" None 126_100 250_000 { X = 6; Y = 29 }
                { structureOf "container" None 165_300 250_000 { X = 11; Y = 18 } with
                    Energy = 1_154
                }
                structureOf "container" None 164_500 250_000 { X = 23; Y = 29 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 101_301 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 985_709
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 985_669
                }
                { structureOf "rampart" (Some Side.Ours) 101_001 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 985_727
                }
                { structureOf "rampart" (Some Side.Ours) 98_101 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 985_760
                }
                { structureOf "rampart" (Some Side.Ours) 98_801 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 985_670
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 985_723
                }
                { structureOf "rampart" (Some Side.Ours) 100_701 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 985_744
                }
                { structureOf "rampart" (Some Side.Ours) 102_001 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 985_693
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 985_686
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 27_983
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]

    // Today's declarations, not the tick's.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let r7 = parts [ Move, 21; RangedAttack, 14; Heal, 7 ]
    let guard = parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ]
    let hauler16 = parts [ Carry, 16; Move, 8 ]
    let hauler32 = parts [ Carry, 32; Move, 16 ]

    let bodies: Body list =
        [
            // Obsidian522: live 23 CARRY, 22 MOVE, parked.
            { body
                  "Obsidian522"
                  trepidimous
                  (List.replicate 23 [ Move; Carry ] |> List.concat)
                  (at "W17S26" 19 26)
                  (Some Hold) with
                Hits = 4_420
                TicksToLive = 541
            }
            { body "ranger-984393-Spawn3" Side.Ours r7 (at "W17S26" 12 24) None with
                TicksToLive = 357
            }
            { body
                  "ranger-985441-Spawn11"
                  Side.Ours
                  (parts [ Move, 3; RangedAttack, 2; Heal, 1 ])
                  (at "W17S26" 16 20)
                  None with
                TicksToLive = 1_297
            }
            { body
                  "reserver-985414-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 22 14)
                  None with
                TicksToLive = 364
            }
            { body "hauler-985043-Spawn11" Side.Ours hauler16 (at "W17S26" 19 13) None with
                TicksToLive = 953
            }
            { body
                  "worker-984557-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 16 34)
                  None with
                TicksToLive = 455
                Energy = 20
            }
            { body
                  "anchor-984621-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 483
                Energy = 50
            }
            { body "ranger-984683-Spawn3" Side.Ours r7 (at "W17S25" 16 36) None with
                TicksToLive = 647
            }
            { body "hauler-984971-Spawn11" Side.Ours hauler16 (at "W17S25" 18 29) None with
                TicksToLive = 881
            }
            { body "guard-985115-Spawn11" Side.Ours guard (at "W17S25" 27 46) None with
                TicksToLive = 983
            }
            { body
                  "upgrader-985149-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 17 34)
                  None with
                TicksToLive = 1_038
                Energy = 42
            }
            { body "hauler-985200-Spawn11" Side.Ours hauler16 (at "W17S25" 9 22) None with
                TicksToLive = 1_110
            }
            { body
                  "upgrader-985272-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 1_161
                Energy = 42
            }
            { body "guard-985559-Spawn11" Side.Ours guard (at "W17S25" 14 23) None with
                TicksToLive = 1_427
            }
            { body
                  "anchor-985629-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 14 29)
                  None with
                TicksToLive = 1_497
            }
            { body
                  "worker-984223-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 16 16)
                  None with
                TicksToLive = 211
            }
            { body
                  "anchor-984253-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 121
                Energy = 50
            }
            { body "guard-984525-Spawn3" Side.Ours guard (at "W15S28" 6 1) None with
                TicksToLive = 393
            }
            { body
                  "upgrader-984881-Spawn3"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 21 24)
                  None with
                TicksToLive = 866
            }
            { body "hauler-985401-Spawn8" Side.Ours hauler32 (at "W15S28" 17 32) None with
                TicksToLive = 1_383
                Energy = 100
            }
            { body
                  "brawler-985442-Spawn3"
                  Side.Ours
                  (parts [ Move, 25; Attack, 25 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 1_430
            }
            { body
                  "anchor-985592-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_454
                Energy = 12
            }
            { body "hauler-984023-Spawn8" Side.Ours hauler32 (at "W15S28" 17 34) None with
                TicksToLive = 5
            }
            { body "hauler-985259-Spawn3" Side.Ours hauler32 (at "W15S28" 7 28) None with
                TicksToLive = 1_241
                Energy = 866
            }
            { body "hauler-984785-Spawn8" Side.Ours hauler32 (at "W15S28" 19 26) None with
                TicksToLive = 767
                Energy = 1_296
            }
        ]

    let a = arena 985_661 [ w17s26; w17s25; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                Assignments =
                    Map.ofList
                        [
                            "ranger-984393-Spawn3", "guard:W17S26"
                            "ranger-985441-Spawn11", "guard:W17S26"
                            "reserver-985414-Spawn11", "reserve:6a8caa7ddd4872bccd318c79"
                            "worker-984557-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "anchor-984621-Spawn11", "harvest:6a8caa7cdd4872bccd318c75"
                            "ranger-984683-Spawn3", "guard:W17S25"
                            "upgrader-985149-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "upgrader-985272-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "anchor-985629-Spawn11", "harvest:6a8caa7ddd4872bccd318c78"
                            "worker-984223-Spawn8", "withdraw:container-11-18"
                            "anchor-984253-Spawn3", "harvest:6a8caa95dd4872bccd319013"
                            "hauler-985401-Spawn8", "refill:spawn-Spawn3"
                            "anchor-985592-Spawn3", "harvest:6a8caa95dd4872bccd319014"
                            "hauler-985259-Spawn3", "withdraw:container-6-29"
                            "hauler-984785-Spawn8", "refill:spawn-Spawn3"
                        ]
                ArmedSeen = Map.ofList [ "W17S25", 985_452; "W17S26", 985_625 ]
                Towered = Set.ofList [ "W15S28"; "W17S25" ]
                Raids =
                    Map.ofList
                        [
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                HomeRaided = Some 985_452
                            }
                        ]
            }
    }

// ---- arena capture ranger-into-melee (movement S0) -------------------------
// `npm run capture -- W17S26 986005 --rooms W17S25,W15S28 --name rangerIntoMelee`, read at live t986,233.
// The room-history replay at t986,005, the state after that tick ran: arena
// tick 0 runs as t986,006.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/986000.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/986000.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/986000.json
// Healers, ExitWatches and Sightings start empty; ArmedSeen, Towered and the
// Raid log are live Memory's at t986,233, less what postdates the tick.
// The 263 roads are laid (#560); W17S26's reservation and a tombstone are not
// placed, nor the ranger Spawn8 was casting; the invader core stands as a structure only (the
// arena lays no `InvaderCores`, #558). The Transition log no longer held the
// two W17S26 rangers' Tasks, nor the anchor's or the guards'.

// ---- arena capture: W17S26 t1,006,904 (movement S4, #583) -------------------
// `npm run capture -- W17S26 1006904 --rooms W17S25`, read at live t1,007,728.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/1006900.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/1006900.json
// W17S26's reservation, 6 ruins and a tombstone are not placed, nor the
// anchor Spawn11 was casting. The Transition log no longer held any Task of
// the tick's. W17S25's ramparts are laid without their decay clocks.

/// W17S26 at t1,006,904: Trepidimous' Coefficient6755 (17 MOVE + 1, 17
/// ATTACK, a step a tick) walks in along y = 1 from (14,1). Live,
/// anchor-1005773 stepped from (22,3) to the exit road's (20,1), toward the
/// melee, then back to (22,3), where it was struck dead (t1,006,911-912);
/// reserver-1006558 (2 CLAIM 2 MOVE, the melee's pace) walked north-east from
/// (16,20) at the melee coming down the road, turned at (26,9) and was caught
/// at (22,11) (t1,006,924).
let private trepEntersW17S26 (melee: Behaviour) : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 169_900 250_000 { X = 27; Y = 9 } with
                    Energy = 420
                }
                structureOf
                    "invaderCore"
                    (Some(Side.Npc "Invader"))
                    100_000
                    100_000
                    { X = 12; Y = 25 }
            ]
        |> withRoads
            [
                20, 1, 3_600
                21, 2, 2_680
                22, 3, 3_200
                23, 4, 3_700
                24, 5, 1_700
                25, 6, 3_700
                26, 7, 2_300
                27, 8, 3_500
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 5 3
        |> withControllerClocks 0 0 79_811 7_093
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_800
        |> withStructures
            [
                for x, y in
                    [
                        9, 23
                        9, 25
                        9, 27
                        9, 29
                        10, 24
                        10, 26
                        10, 28
                        10, 30
                        11, 23
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 23
                        13, 25
                        13, 31
                        14, 24
                        14, 26
                        14, 30
                        15, 31
                        16, 32
                        17, 29
                        17, 31
                        18, 30
                        19, 31
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in [ 15, 27; 15, 29; 16, 28; 16, 30 ] ->
                    structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 142_000 250_000 { X = 16; Y = 34 } with
                    Energy = 1_000
                }
                { structureOf "container" None 125_200 250_000 { X = 17; Y = 20 } with
                    Energy = 518
                }
                yield!
                    [
                        for x, y, hits in
                            [
                                2, 15, 99_721
                                2, 16, 99_661
                                2, 17, 90_931
                                2, 18, 99_231
                                2, 19, 98_811
                                2, 20, 98_731
                                2, 23, 93_321
                                2, 24, 93_901
                                2, 25, 93_401
                                2, 26, 93_661
                                2, 27, 94_261
                                11, 29, 25_001
                                13, 27, 87_201
                                13, 29, 87_201
                                14, 28, 86_601
                                16, 44, 100_321
                                17, 20, 87_001
                                17, 44, 100_301
                                18, 44, 100_101
                                32, 31, 99_911
                                32, 32, 93_201
                            ] ->
                            structureOf "rampart" (Some Side.Ours) hits 10_000_000 { X = x; Y = y }
                    ]
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 5_982
                }
                towerOf Side.Ours { X = 11; Y = 29 } 1_000
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]
        |> withRoads
            [
                1, 16, 2_300
                2, 17, 1_800
                3, 18, 2_200
                4, 19, 3_400
                5, 20, 3_300
                6, 21, 3_800
                7, 22, 12_900
                8, 23, 10_700
                9, 24, 11_000
                9, 26, 24_500
                9, 28, 24_000
                10, 23, 24_000
                10, 25, 12_600
                10, 27, 13_300
                10, 29, 14_600
                11, 24, 15_600
                11, 26, 7_600
                11, 28, 8_300
                11, 30, 3_400
                12, 23, 13_300
                12, 25, 9_700
                12, 27, 10_500
                12, 29, 1_800
                12, 31, 4_600
                13, 24, 1_500
                13, 26, 1_900
                13, 28, 2_800
                13, 30, 1_900
                14, 23, 1_800
                14, 25, 2_300
                14, 27, 1_900
                14, 29, 2_100
                14, 31, 4_900
                15, 22, 2_900
                15, 28, 3_100
                15, 30, 3_900
                15, 32, 4_800
                16, 21, 3_200
                16, 29, 4_000
                16, 31, 4_900
                16, 33, 3_700
                16, 35, 2_600
                16, 37, 4_000
                16, 38, 8_600
                16, 39, 9_100
                16, 41, 4_800
                16, 43, 4_600
                16, 44, 3_800
                17, 20, 3_700
                17, 30, 1_800
                17, 32, 2_600
                17, 34, 5_000
                17, 36, 3_200
                17, 38, 2_800
                17, 39, 14_000
                17, 40, 19_100
                17, 42, 5_000
                17, 45, 5_000
                18, 29, 2_900
                18, 31, 3_300
                18, 35, 3_900
                18, 37, 3_000
                18, 39, 3_700
                18, 41, 1_900
                18, 46, 3_700
                19, 30, 2_600
                19, 34, 3_200
                19, 36, 2_500
                19, 38, 3_800
                19, 40, 3_700
                19, 47, 3_600
                20, 31, 4_400
                20, 33, 3_000
                20, 35, 3_700
                20, 37, 3_100
                20, 39, 3_800
                20, 48, 3_000
                21, 32, 2_800
                21, 34, 2_600
                21, 36, 3_800
                21, 38, 2_600
                22, 33, 3_100
                22, 35, 3_800
                22, 37, 3_200
                23, 34, 3_100
                23, 36, 3_600
                24, 35, 3_700
                25, 34, 2_100
                26, 33, 2_500
                27, 32, 2_200
                28, 31, 7_000
                28, 33, 11_600
                29, 32, 15_900
                29, 34, 14_100
                30, 31, 8_000
                30, 33, 13_400
                31, 32, 11_400
                32, 31, 20_800
                33, 30, 15_140
                33, 32, 7_380
            ]

    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W17S25" ])

    let bodies: Body list =
        [
            // Ocean6422 (23 MOVE 23 CARRY): live, no act.
            { body
                  "Ocean6422"
                  trepidimous
                  (parts [ Move, 23; Carry, 23 ])
                  (at "W17S26" 28 8)
                  (Some Hold) with
                TicksToLive = 1_154
            }
            { body
                  "Coefficient6755"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W17S26" 14 1)
                  (Some melee) with
                TicksToLive = 1_454
            }
            { body
                  "anchor-1005773-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 22 3)
                  None with
                TicksToLive = 397
                Energy = 50
            }
            { body
                  "reserver-1006558-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 16 20)
                  None with
                TicksToLive = 264
            }
            { body
                  "hauler-1005435-Spawn11"
                  Side.Ours
                  (parts [ Carry, 20; Move, 10 ])
                  (at "W17S25" 12 25)
                  None with
                TicksToLive = 119
            }
            { body
                  "guard-1005661-Spawn11"
                  Side.Ours
                  (parts [ Tough, 2; Move, 10; Attack, 6; Heal, 2 ])
                  (at "W17S25" 17 47)
                  None with
                TicksToLive = 315
            }
            { body
                  "worker-1006267-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 10; Move, 9 ])
                  (at "W17S25" 16 31)
                  None with
                TicksToLive = 942
                Energy = 201
            }
            { body
                  "hauler-1006605-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W17S25" 15 22)
                  None with
                TicksToLive = 1_307
            }
            { body
                  "hauler-1006713-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W17S25" 14 29)
                  None with
                TicksToLive = 1_415
                Energy = 50
            }
            { body
                  "ranger-1006821-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; RangedAttack, 4; Heal, 2 ])
                  (at "W17S25" 18 47)
                  None with
                TicksToLive = 1_451
            }
        ]

    arena 1_006_905 [ w17s26; w17s25 ] colonies bodies

/// W17S26 at t986,005 (the movement ADR's W17S26 symptom): Trepidimous' HalfLife847
/// (17 MOVE + 1, 17 ATTACK, a step a tick) walks in from (20,1) on its way to
/// the container at (27,8); our ranger-984393-Spawn3 (21M 14R 7H) at (25,7)
/// walked toward it (24,6), (24,5) to shoot at range 2 (t986,008), the melee
/// stepped beside it and struck it four times, 4,200 to 2,496 hits
/// (t986,009-986,012).
let private rangerIntoMelee () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 125_800 250_000 { X = 27; Y = 9 } with
                    Energy = 586
                }
                structureOf
                    "invaderCore"
                    (Some(Side.Npc "Invader"))
                    57_550
                    100_000
                    { X = 12; Y = 25 }
            ]
        |> withRoads
            [
                20, 1, 4_600
                21, 2, 4_200
                22, 3, 4_200
                23, 4, 4_700
                24, 5, 2_700
                25, 6, 4_700
                26, 7, 3_200
                27, 8, 3_600
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withControllerClocks 0 0 40_000 205_038
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in
                    [
                        10, 24
                        10, 26
                        10, 28
                        10, 30
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 28
                        16, 30
                        17, 29
                        17, 31
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                { structureOf "container" None 139_200 250_000 { X = 16; Y = 34 } with
                    Energy = 366
                }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 360
                }
                { structureOf "rampart" (Some Side.Ours) 98_821 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 986_094
                }
                { structureOf "rampart" (Some Side.Ours) 98_761 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 986_075
                }
                { structureOf "rampart" (Some Side.Ours) 99_261 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 986_078
                }
                { structureOf "rampart" (Some Side.Ours) 98_131 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 986_083
                }
                { structureOf "rampart" (Some Side.Ours) 98_641 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 986_055
                }
                { structureOf "rampart" (Some Side.Ours) 98_831 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 986_057
                }
                { structureOf "rampart" (Some Side.Ours) 98_621 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 986_082
                }
                { structureOf "rampart" (Some Side.Ours) 99_201 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 986_044
                }
                { structureOf "rampart" (Some Side.Ours) 98_701 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 986_061
                }
                { structureOf "rampart" (Some Side.Ours) 98_961 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 986_076
                }
                { structureOf "rampart" (Some Side.Ours) 98_261 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 986_025
                }
                { structureOf "rampart" (Some Side.Ours) 100_701 3_000_000 { X = 13; Y = 27 } with
                    NextDecay = 986_075
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 986_060
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 986_036
                }
                { structureOf "rampart" (Some Side.Ours) 99_621 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 986_024
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 986_081
                }
                { structureOf "rampart" (Some Side.Ours) 99_601 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 986_026
                }
                { structureOf "rampart" (Some Side.Ours) 94_001 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 986_022
                }
                { structureOf "rampart" (Some Side.Ours) 92_801 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 986_035
                }
                { structureOf "rampart" (Some Side.Ours) 92_291 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 986_053
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 9_277
                }
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]
        |> withRoads
            [
                1, 16, 3_400
                2, 17, 3_500
                3, 18, 3_400
                4, 19, 4_800
                5, 20, 4_800
                6, 21, 3_000
                7, 22, 14_900
                8, 23, 13_900
                9, 24, 13_500
                10, 25, 18_800
                10, 27, 13_500
                10, 29, 17_000
                11, 24, 17_300
                11, 26, 15_100
                11, 28, 19_400
                11, 30, 3_400
                12, 23, 18_400
                12, 25, 18_900
                12, 27, 13_500
                12, 29, 3_800
                13, 24, 3_200
                13, 26, 3_500
                13, 28, 4_500
                13, 30, 4_000
                14, 23, 3_100
                14, 25, 2_800
                14, 27, 3_600
                14, 29, 3_700
                14, 31, 3_800
                15, 22, 4_100
                15, 28, 4_400
                15, 30, 2_900
                15, 32, 3_700
                16, 21, 3_700
                16, 29, 2_700
                16, 31, 3_000
                16, 33, 3_000
                16, 35, 3_100
                16, 37, 2_500
                16, 38, 16_500
                16, 39, 17_800
                16, 41, 3_000
                16, 43, 2_900
                16, 44, 2_900
                17, 20, 3_400
                17, 30, 2_700
                17, 32, 3_400
                17, 34, 2_700
                17, 36, 4_400
                17, 38, 2_900
                17, 39, 14_900
                17, 40, 12_000
                17, 42, 2_900
                17, 45, 3_800
                18, 29, 3_100
                18, 31, 3_500
                18, 35, 2_500
                18, 37, 3_000
                18, 39, 3_400
                18, 41, 4_000
                18, 46, 4_300
                19, 30, 2_900
                19, 34, 3_700
                19, 36, 4_700
                19, 38, 3_500
                19, 40, 3_400
                19, 47, 3_900
                20, 31, 4_400
                20, 33, 3_900
                20, 35, 3_400
                20, 37, 2_800
                20, 39, 3_500
                20, 48, 4_500
                21, 32, 3_000
                21, 34, 4_700
                21, 36, 3_500
                21, 38, 2_300
                22, 33, 3_000
                22, 35, 3_500
                22, 37, 3_000
                23, 34, 2_900
                23, 36, 3_400
                24, 35, 3_600
                25, 34, 4_400
                26, 33, 2_400
                27, 32, 4_600
                28, 31, 20_500
                28, 33, 12_500
                29, 32, 20_500
                29, 34, 15_000
                30, 31, 21_000
                30, 33, 16_000
                31, 32, 16_000
                32, 31, 21_000
                33, 30, 22_500
                33, 32, 18_810
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_935_955
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 30
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 27
                        13, 29
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        16, 26
                        16, 32
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        18, 34
                        19, 27
                        19, 31
                        19, 33
                        20, 26
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        21, 35
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 126_100 250_000 { X = 6; Y = 29 } with
                    Energy = 96
                }
                { structureOf "container" None 165_300 250_000 { X = 11; Y = 18 } with
                    Energy = 312
                }
                { structureOf "container" None 159_500 250_000 { X = 23; Y = 29 } with
                    Energy = 52
                }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 15; Y = 35 }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 16; Y = 34 } with
                    Energy = 50
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 100_101 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 986_105
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 986_065
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 986_024
                }
                { structureOf "rampart" (Some Side.Ours) 99_701 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 986_064
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 986_066
                }
                { structureOf "rampart" (Some Side.Ours) 100_301 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 986_020
                }
                { structureOf "rampart" (Some Side.Ours) 99_801 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 986_041
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 986_089
                }
                { structureOf "rampart" (Some Side.Ours) 99_601 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 986_082
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 28_321
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]
        |> withRoads
            [
                4, 5, 3_100
                5, 4, 4_500
                5, 6, 3_100
                5, 44, 4_900
                6, 3, 3_500
                6, 7, 5_000
                6, 29, 4_000
                6, 43, 4_500
                6, 45, 2_800
                7, 2, 2_500
                7, 8, 3_100
                7, 28, 2_800
                7, 42, 4_300
                7, 46, 4_700
                8, 1, 3_300
                8, 9, 3_900
                8, 28, 4_300
                8, 41, 4_600
                8, 47, 3_900
                9, 10, 2_400
                9, 27, 5_000
                9, 28, 2_800
                9, 40, 4_300
                9, 48, 2_900
                10, 11, 4_200
                10, 26, 2_800
                10, 29, 5_000
                10, 39, 4_600
                11, 12, 3_700
                11, 18, 3_500
                11, 27, 4_000
                11, 30, 2_600
                11, 38, 2_500
                12, 13, 4_600
                12, 17, 2_400
                12, 25, 13_000
                12, 27, 17_900
                12, 28, 2_600
                12, 29, 2_500
                12, 31, 4_000
                12, 33, 20_700
                12, 35, 14_900
                12, 37, 4_500
                13, 14, 4_700
                13, 16, 4_800
                13, 26, 13_700
                13, 28, 3_900
                13, 30, 2_900
                13, 32, 3_500
                13, 34, 4_600
                13, 36, 4_800
                14, 15, 3_400
                14, 25, 15_100
                14, 27, 3_800
                14, 29, 4_500
                14, 31, 4_900
                14, 33, 3_100
                14, 35, 4_900
                15, 15, 3_100
                15, 26, 3_500
                15, 28, 4_800
                15, 30, 3_500
                15, 32, 3_400
                15, 34, 4_700
                16, 16, 3_100
                16, 25, 18_500
                16, 27, 3_800
                16, 29, 3_800
                16, 31, 4_900
                16, 33, 4_400
                16, 35, 4_000
                17, 17, 4_100
                17, 26, 19_800
                17, 28, 4_000
                17, 30, 3_300
                17, 32, 2_900
                17, 34, 3_400
                18, 18, 2_600
                18, 25, 20_100
                18, 27, 18_400
                18, 29, 2_500
                18, 31, 4_700
                18, 33, 5_000
                19, 19, 16_100
                19, 22, 18_000
                19, 24, 14_400
                19, 26, 13_900
                19, 28, 4_000
                19, 30, 4_800
                19, 32, 3_100
                20, 20, 16_400
                20, 21, 20_000
                20, 23, 19_700
                20, 25, 19_300
                20, 27, 16_100
                20, 29, 3_500
                20, 31, 4_000
                20, 33, 17_100
                21, 21, 4_900
                21, 22, 3_200
                21, 23, 3_200
                21, 24, 17_000
                21, 25, 20_600
                21, 26, 12_800
                21, 28, 20_000
                21, 30, 2_700
                21, 32, 20_500
                21, 34, 17_200
                22, 24, 2_600
                22, 25, 12_700
                22, 27, 18_800
                22, 28, 2_600
                22, 29, 4_600
                22, 30, 17_000
                22, 31, 21_300
                22, 32, 12_500
                22, 33, 12_700
                22, 35, 4_800
                23, 26, 15_200
                23, 28, 21_000
                23, 29, 12_600
                23, 30, 13_900
                23, 31, 14_500
                23, 32, 12_300
                23, 33, 16_200
                23, 34, 2_800
                24, 25, 13_000
                24, 28, 13_400
                24, 29, 14_500
                24, 30, 12_900
                24, 33, 17_500
                25, 24, 2_800
                25, 28, 13_600
                25, 29, 13_700
                25, 34, 14_800
                26, 23, 15_400
                27, 22, 12_600
                28, 21, 12_300
                28, 34, 13_600
                29, 20, 13_600
                30, 19, 12_000
                31, 18, 2_000
                32, 17, 2_500
                33, 16, 3_800
                34, 15, 4_400
                35, 14, 2_600
                36, 13, 16_500
                37, 12, 2_700
                38, 11, 4_100
                39, 12, 2_100
                40, 13, 13_800
                41, 14, 12_100
                42, 15, 3_600
                43, 16, 3_600
                44, 17, 2_000
                45, 18, 2_000
                46, 19, 2_000
                47, 20, 3_800
                48, 21, 4_400
            ]

    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            // Obsidian522: live 23 CARRY, 22 MOVE; walked north out over
            // (20,0) and bounced on the W17S25 border under its tower.
            { body
                  "Obsidian522"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                      ])
                  (at "W17S26" 21 2)
                  (Some(
                      Waypoints
                          [
                              at "W17S26" 21 1
                              at "W17S26" 20 0
                              at "W17S25" 20 48
                              at "W17S25" 21 49
                          ]
                  )) with
                Hits = 4_420
                TicksToLive = 196
            }
            // HalfLife847: live, down the road a step a tick, swinging at
            // the ranger, then off toward the core: its live tiles to
            // t986,030.
            { body
                  "HalfLife847"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W17S26" 20 1)
                  (Some(
                      Waypoints
                          [
                              for x, y in
                                  [
                                      21, 2
                                      22, 3
                                      23, 4
                                      24, 5
                                      25, 6
                                      26, 7
                                      27, 8
                                      26, 9
                                      25, 10
                                      24, 11
                                      23, 12
                                      22, 13
                                      22, 14
                                      21, 15
                                      20, 16
                                      19, 17
                                  ] -> at "W17S26" x y
                          ]
                  )) with
                TicksToLive = 1_446
            }
            { body
                  "ranger-984393-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S26" 25 7)
                  None with
                TicksToLive = 12
            }
            { body
                  "reserver-985414-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 9 22)
                  None with
                TicksToLive = 19
            }
            { body
                  "ranger-985441-Spawn11"
                  Side.Ours
                  (parts [ Move, 3; RangedAttack, 2; Heal, 1 ])
                  (at "W17S26" 25 10)
                  None with
                TicksToLive = 952
            }
            { body
                  "anchor-985849-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 1_372
                Energy = 14
            }
            { body
                  "reserver-985957-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 27 8)
                  None with
                TicksToLive = 562
            }
            { body
                  "worker-984557-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 16 33)
                  None with
                TicksToLive = 110
                Energy = 320
            }
            { body
                  "anchor-984621-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 138
                Energy = 50
            }
            { body
                  "hauler-984663-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 19 47)
                  None with
                TicksToLive = 228
                Energy = 250
            }
            { body
                  "ranger-984683-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 37)
                  None with
                TicksToLive = 302
            }
            { body
                  "guard-985115-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 28 47)
                  None with
                TicksToLive = 638
            }
            { body
                  "upgrader-985149-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 693
                Energy = 42
            }
            { body
                  "hauler-985200-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 8 23)
                  None with
                TicksToLive = 765
            }
            { body
                  "upgrader-985272-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 17 34)
                  None with
                TicksToLive = 816
                Energy = 42
            }
            { body
                  "guard-985559-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 14 23)
                  None with
                TicksToLive = 1_082
            }
            { body
                  "ranger-985616-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 17 36)
                  None with
                TicksToLive = 1_235
            }
            { body
                  "guard-984525-Spawn3"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W15S28" 6 1)
                  None with
                TicksToLive = 48
            }
            { body
                  "hauler-984785-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 16 25)
                  None with
                TicksToLive = 422
            }
            { body
                  "upgrader-984881-Spawn3"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 521
                Energy = 26
            }
            { body
                  "hauler-985259-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 14 27)
                  None with
                TicksToLive = 896
                Energy = 1_164
            }
            { body
                  "hauler-985401-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 18 33)
                  None with
                TicksToLive = 1_038
                Energy = 470
            }
            { body
                  "brawler-985442-Spawn3"
                  Side.Ours
                  (parts [ Move, 25; Attack, 25 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 1_085
            }
            { body
                  "hauler-985545-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 8 41)
                  None with
                TicksToLive = 1_182
                Energy = 58
            }
            { body
                  "anchor-985592-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_109
                Energy = 50
            }
            { body
                  "anchor-985719-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 1_242
                Energy = 50
            }
            { body
                  "worker-985819-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 5 44)
                  None with
                TicksToLive = 1_462
            }
            { body
                  "reserver-985969-Spawn3"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W15S28" 45 18)
                  None with
                TicksToLive = 568
            }
        ]

    let a = arena 986_006 [ w17s26; w17s25; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                Assignments =
                    Map.ofList
                        [
                            "reserver-985957-Spawn11", "reserve:6a8caa7ddd4872bccd318c79"
                            "hauler-984663-Spawn11", "flee"
                            "ranger-984683-Spawn3", "guard:W17S25"
                            "upgrader-985149-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "hauler-985200-Spawn11", "withdraw:container-27-9"
                            "upgrader-985272-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "ranger-985616-Spawn3", "guard:W17S25"
                            "hauler-984785-Spawn8", "withdraw:6ac0e2f1e169cce45673df61"
                            "upgrader-984881-Spawn3", "upgrade:6a8caa95dd4872bccd319015"
                            "hauler-985259-Spawn3", "refill:storage-17-29"
                            "hauler-985401-Spawn8", "refill:spawn-Spawn3"
                            "hauler-985545-Spawn8", "withdraw:6ab55d5a936dc16f644ebc58"
                            "anchor-985592-Spawn3", "harvest:6a8caa95dd4872bccd319014"
                            "anchor-985719-Spawn8", "harvest:6a8caa95dd4872bccd319013"
                            "worker-985819-Spawn3", "withdraw:6ab55d5a936dc16f644ebc58"
                            "reserver-985969-Spawn3", "reserve:6a8caaa1dd4872bccd3191fa"
                        ]
                ArmedSeen = Map.ofList [ "W17S25", 985_452 ]
                Towered = Set.ofList [ "W15S28"; "W17S25" ]
                Raids =
                    Map.ofList
                        [
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                HomeRaided = Some 985_452
                            }
                        ]
            }
    }

// ---- arena capture slow-melee-haulers (#527, movement S0) ------------------
// `npm run capture -- W18S25 971045 --rooms W17S25,W15S28 --name slowMeleeHaulers`, read at live t986,316.
// The room-history replay at t971,045, the state after that tick ran: arena
// tick 0 runs as t971,046.
//   W18S25  https://screeps.com/room-history/shardSeason/W18S25/971000.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/971000.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/971000.json
// Read too late for live Memory: the Raid log, ArmedSeen, Healers, ExitWatches
// and Sightings start empty, and every body of ours starts idle. The 264
// roads are laid (#560); W18S25's reservation and 2 tombstones are not
// placed, nor the hauler Spawn3 was casting.

/// W18S25 at t971,045 (#527's "Why"): Trepidimous' Virtual458 at (39,14),
/// 14 live ATTACK and 1 live MOVE, steps once every ~9 ticks toward the
/// south-west; W17S25's loaded haulers hauler-969730-Spawn11 (36,13) and
/// hauler-970335-Spawn11 (36,17), walking home east past it, stand at its
/// Reach's edge and step in and out of it (live t971,030-971,060).
let private slowMeleeHaulers () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"
    let kazkel = Side.Player "Kazkel"

    let w18s25 =
        room "W18S25"
        |> withStructures [ structureOf "container" None 120_000 250_000 { X = 31; Y = 5 } ]
        |> withRoads
            [
                28, 9, 3_100
                29, 8, 3_300
                29, 10, 3_000
                30, 7, 3_300
                30, 11, 3_300
                31, 6, 3_500
                31, 12, 2_900
                32, 13, 3_000
                33, 14, 2_600
                34, 15, 3_400
                35, 16, 2_600
                36, 17, 2_600
                37, 16, 2_600
                38, 15, 2_600
                39, 14, 2_500
                40, 13, 2_300
                41, 12, 2_500
                42, 13, 2_300
                43, 13, 2_000
                44, 13, 2_000
                45, 13, 2_200
                46, 14, 2_100
                47, 15, 2_100
                48, 16, 2_200
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 39_973 69_540
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 850
        |> withStructures
            [
                for x, y in
                    [ 11, 25; 11, 27; 12, 28; 14, 26; 14, 30; 15, 29; 16, 28; 16, 30; 17, 31 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                structureOf "container" None 71_200 250_000 { X = 16; Y = 34 }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 108
                }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 30 }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 15; Y = 27 }
                { structureOf "rampart" (Some Side.Ours) 97_321 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 971_145
                }
                { structureOf "rampart" (Some Side.Ours) 97_561 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 971_126
                }
                { structureOf "rampart" (Some Side.Ours) 93_021 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 971_129
                }
                { structureOf "rampart" (Some Side.Ours) 99_931 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 971_134
                }
                { structureOf "rampart" (Some Side.Ours) 99_871 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 971_106
                }
                { structureOf "rampart" (Some Side.Ours) 94_631 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 971_108
                }
                { structureOf "rampart" (Some Side.Ours) 95_921 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 971_133
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 971_095
                }
                { structureOf "rampart" (Some Side.Ours) 96_701 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 971_112
                }
                { structureOf "rampart" (Some Side.Ours) 97_461 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 971_127
                }
                { structureOf "rampart" (Some Side.Ours) 97_961 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 971_076
                }
                { structureOf "rampart" (Some Side.Ours) 15_401 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 971_111
                }
                { structureOf "rampart" (Some Side.Ours) 5_701 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 971_087
                }
                { structureOf "rampart" (Some Side.Ours) 99_321 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 971_075
                }
                { structureOf "rampart" (Some Side.Ours) 39_201 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 971_132
                }
                { structureOf "rampart" (Some Side.Ours) 99_301 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 971_077
                }
                { structureOf "rampart" (Some Side.Ours) 92_101 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 971_073
                }
                { structureOf "rampart" (Some Side.Ours) 98_071 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 971_086
                }
                { structureOf "rampart" (Some Side.Ours) 96_681 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 971_104
                }
                towerOf Side.Ours { X = 13; Y = 29 } 980
            ]
        |> withSite Side.Ours "road" None 0 { X = 21; Y = 32 }
        |> withSite Side.Ours "road" None 0 { X = 22; Y = 33 }
        |> withSite Side.Ours "road" None 156 { X = 23; Y = 34 }
        |> withSite Side.Ours "road" None 182 { X = 22; Y = 35 }
        |> withSite Side.Ours "storage" None 0 { X = 13; Y = 27 }
        |> withSite Side.Ours "extension" None 753 { X = 13; Y = 31 }
        |> withSite Side.Ours "extension" None 2_742 { X = 15; Y = 31 }
        |> withSite Side.Ours "extension" None 2_346 { X = 10; Y = 24 }
        |> withSite Side.Ours "extension" None 0 { X = 10; Y = 26 }
        |> withSite Side.Ours "extension" None 0 { X = 10; Y = 28 }
        |> withSite Side.Ours "extension" None 0 { X = 10; Y = 30 }
        |> withSite Side.Ours "extension" None 1_765 { X = 12; Y = 24 }
        |> withSite Side.Ours "road" None 0 { X = 10; Y = 29 }
        |> withSite Side.Ours "road" None 0 { X = 11; Y = 30 }
        |> withSite Side.Ours "road" None 0 { X = 10; Y = 27 }
        |> withSite Side.Ours "extension" None 107 { X = 17; Y = 29 }
        |> withSite Side.Ours "extension" None 1_800 { X = 13; Y = 25 }
        |> withSite Side.Ours "road" None 271 { X = 18; Y = 29 }
        |> withRoads
            [
                2, 17, 3_500
                3, 18, 3_500
                4, 19, 3_100
                5, 20, 3_100
                6, 21, 2_800
                7, 22, 14_000
                8, 23, 14_000
                9, 24, 13_000
                10, 25, 12_000
                11, 24, 18_000
                11, 26, 10_300
                11, 28, 18_500
                12, 23, 18_500
                12, 25, 12_600
                12, 27, 13_000
                12, 29, 3_400
                13, 24, 2_500
                13, 26, 3_200
                13, 28, 3_800
                13, 30, 2_100
                14, 23, 2_600
                14, 25, 1_400
                14, 27, 3_700
                14, 29, 1_300
                14, 31, 1_900
                15, 22, 2_200
                15, 28, 1_300
                15, 30, 3_800
                15, 32, 2_000
                16, 21, 3_300
                16, 29, 1_600
                16, 31, 3_100
                16, 33, 1_600
                16, 35, 3_500
                16, 37, 4_900
                16, 38, 20_000
                16, 39, 9_000
                16, 41, 4_000
                16, 43, 2_700
                16, 44, 2_700
                17, 20, 1_900
                17, 30, 5_000
                17, 32, 3_300
                17, 34, 3_100
                17, 36, 5_000
                17, 38, 4_900
                17, 39, 8_500
                17, 40, 24_500
                17, 42, 2_900
                17, 45, 3_100
                18, 31, 3_600
                18, 35, 2_800
                18, 37, 5_000
                18, 39, 5_000
                18, 41, 2_600
                18, 46, 3_100
                19, 30, 5_000
                19, 34, 3_400
                19, 36, 3_900
                19, 38, 5_000
                19, 40, 2_600
                19, 47, 3_100
                20, 31, 3_900
                20, 33, 3_400
                20, 35, 5_000
                20, 37, 4_400
                20, 39, 2_100
                20, 48, 2_800
                21, 34, 3_900
                21, 36, 5_000
                21, 38, 3_800
                22, 37, 1_500
                23, 36, 1_400
                24, 35, 3_300
                25, 34, 4_000
                26, 33, 2_400
                27, 32, 1_300
                28, 33, 7_500
                29, 34, 8_500
                30, 33, 9_500
                31, 32, 9_500
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_710_197
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 25
                        13, 27
                        13, 29
                        13, 31
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 31
                        15, 33
                        15, 35
                        16, 26
                        16, 32
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        18, 34
                        19, 27
                        19, 31
                        19, 33
                        20, 26
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        21, 35
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 12, 30; 14, 26; 14, 30; 15, 27; 15, 29; 16, 30 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 6; Y = 29 } with
                    Energy = 12
                }
                { structureOf "container" None 125_100 250_000 { X = 11; Y = 18 } with
                    Energy = 360
                }
                structureOf "container" None 168_100 250_000 { X = 23; Y = 29 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 100_901 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 971_057
                }
                { structureOf "rampart" (Some Side.Ours) 101_001 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 971_116
                }
                { structureOf "rampart" (Some Side.Ours) 100_601 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 971_075
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 971_144
                }
                { structureOf "rampart" (Some Side.Ours) 99_701 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 971_117
                }
                { structureOf "rampart" (Some Side.Ours) 102_401 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 971_071
                }
                { structureOf "rampart" (Some Side.Ours) 101_901 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 971_092
                }
                { structureOf "rampart" (Some Side.Ours) 99_701 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 971_140
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 971_133
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 29_952
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]
        |> withRoads
            [
                4, 5, 3_900
                5, 4, 4_300
                5, 6, 3_900
                5, 44, 4_700
                6, 3, 3_300
                6, 7, 4_800
                6, 29, 2_900
                6, 43, 4_300
                6, 45, 2_300
                7, 2, 3_200
                7, 8, 3_900
                7, 28, 2_500
                7, 42, 3_900
                7, 46, 2_800
                8, 1, 3_800
                8, 9, 3_700
                8, 28, 4_500
                8, 41, 4_300
                8, 47, 3_700
                9, 10, 4_800
                9, 27, 4_900
                9, 28, 4_700
                9, 40, 3_900
                9, 48, 4_800
                10, 11, 3_800
                10, 26, 2_700
                10, 29, 2_800
                10, 39, 4_300
                11, 12, 3_500
                11, 18, 3_400
                11, 27, 3_900
                11, 30, 4_600
                11, 38, 4_900
                12, 13, 2_800
                12, 17, 4_600
                12, 25, 21_000
                12, 27, 14_200
                12, 28, 2_700
                12, 29, 4_500
                12, 31, 3_400
                12, 33, 20_600
                12, 35, 17_900
                12, 37, 4_300
                13, 14, 2_900
                13, 16, 2_500
                13, 26, 14_200
                13, 28, 3_300
                13, 30, 2_600
                13, 32, 3_200
                13, 34, 3_000
                13, 36, 4_600
                14, 15, 4_500
                14, 25, 17_500
                14, 27, 4_300
                14, 29, 4_000
                14, 31, 4_800
                14, 33, 3_000
                14, 35, 4_600
                15, 15, 4_700
                15, 26, 3_900
                15, 28, 3_900
                15, 30, 4_300
                15, 32, 3_800
                15, 34, 3_100
                16, 16, 4_700
                16, 25, 14_400
                16, 27, 4_400
                16, 29, 4_400
                16, 31, 2_500
                16, 33, 4_900
                16, 35, 3_400
                17, 17, 2_500
                17, 26, 19_200
                17, 28, 3_300
                17, 30, 3_600
                17, 32, 3_000
                17, 34, 3_300
                18, 18, 3_700
                18, 25, 18_500
                18, 27, 16_700
                18, 29, 2_800
                18, 31, 5_000
                18, 33, 4_700
                19, 19, 18_400
                19, 22, 14_800
                19, 24, 20_400
                19, 26, 15_800
                19, 28, 2_900
                19, 30, 4_900
                19, 32, 4_100
                20, 20, 19_200
                20, 21, 17_800
                20, 23, 16_500
                20, 25, 17_100
                20, 27, 17_000
                20, 29, 4_600
                20, 31, 4_700
                20, 33, 17_500
                21, 21, 3_800
                21, 22, 2_600
                21, 23, 5_000
                21, 24, 20_400
                21, 25, 18_900
                21, 26, 21_300
                21, 28, 19_400
                21, 30, 2_500
                21, 32, 18_800
                21, 34, 17_100
                22, 24, 4_400
                22, 25, 13_600
                22, 27, 13_000
                22, 28, 5_000
                22, 29, 4_100
                22, 30, 16_400
                22, 31, 19_700
                22, 32, 20_500
                22, 33, 20_700
                22, 35, 3_800
                23, 26, 17_600
                23, 28, 18_200
                23, 29, 20_600
                23, 30, 21_400
                23, 31, 14_000
                23, 32, 20_300
                23, 33, 15_700
                23, 34, 4_300
                24, 25, 14_900
                24, 28, 21_400
                24, 29, 22_000
                24, 30, 20_400
                24, 33, 15_900
                25, 24, 2_500
                25, 28, 21_600
                25, 29, 21_200
                25, 34, 12_700
                26, 23, 16_800
                27, 22, 14_000
                28, 21, 13_700
                28, 34, 21_100
                29, 20, 15_500
                30, 19, 13_900
                31, 18, 4_500
                32, 17, 5_000
                33, 16, 3_600
                34, 15, 2_600
                35, 14, 3_500
                36, 13, 19_400
                37, 12, 2_400
                38, 11, 3_700
                39, 12, 4_400
                40, 13, 13_600
                41, 14, 13_500
                42, 15, 3_100
                43, 16, 3_100
                44, 17, 2_600
                45, 18, 2_600
                46, 19, 4_200
                47, 20, 4_400
                48, 21, 3_400
            ]

    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            // Virtual458: live 14 ATTACK, 1 MOVE; a road step each time its
            // fatigue runs out, its live tiles to t971,136.
            { body
                  "Virtual458"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W18S25" 39 14)
                  (Some(
                      Waypoints
                          [
                              for x, y in
                                  [
                                      38, 15
                                      37, 16
                                      36, 17
                                      35, 16
                                      34, 15
                                      33, 14
                                      32, 13
                                      31, 12
                                      30, 11
                                      29, 10
                                      28, 9
                                  ] -> at "W18S25" x y
                          ]
                  )) with
                Hits = 1_454
                TicksToLive = 1_016
            }
            { body
                  "hauler-969730-Spawn11"
                  Side.Ours
                  (parts [ Carry, 10; Move, 5 ])
                  (at "W18S25" 36 13)
                  None with
                TicksToLive = 228
                Energy = 457
            }
            { body
                  "hauler-970335-Spawn11"
                  Side.Ours
                  (parts [ Carry, 10; Move, 5 ])
                  (at "W18S25" 36 17)
                  None with
                TicksToLive = 833
                Energy = 500
            }
            { body
                  "reserver-970973-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W18S25" 17 8)
                  None with
                TicksToLive = 532
            }
            { body
                  "anchor-970998-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W18S25" 46 14)
                  None with
                TicksToLive = 1_481
                Fatigue = 1
                Energy = 15
            }
            // scout-W17S18-969668: live 1 MOVE, no act.
            { body "scout-W17S18-969668" kazkel (parts [ Move, 1 ]) (at "W18S25" 25 1) (Some Hold) with
                Hits = 80
                TicksToLive = 124
            }
            { body
                  "worker-969637-Spawn11"
                  Side.Ours
                  (parts [ Work, 4; Carry, 4; Move, 4 ])
                  (at "W17S25" 15 32)
                  None with
                TicksToLive = 126
                Energy = 148
            }
            { body
                  "ranger-969847-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 36)
                  None with
                TicksToLive = 426
            }
            { body
                  "upgrader-970047-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 1; Move, 5 ])
                  (at "W17S25" 19 30)
                  None with
                TicksToLive = 533
            }
            { body
                  "ranger-970126-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 705
            }
            { body
                  "hauler-970276-Spawn11"
                  Side.Ours
                  (parts [ Carry, 10; Move, 5 ])
                  (at "W17S25" 16 43)
                  None with
                TicksToLive = 774
            }
            { body
                  "worker-970391-Spawn11"
                  Side.Ours
                  (parts [ Work, 4; Carry, 4; Move, 4 ])
                  (at "W17S25" 16 33)
                  None with
                TicksToLive = 880
            }
            { body
                  "worker-970447-Spawn11"
                  Side.Ours
                  (parts [ Work, 4; Carry, 4; Move, 4 ])
                  (at "W17S25" 13 22)
                  None with
                TicksToLive = 936
                Energy = 32
            }
            { body
                  "worker-970795-Spawn11"
                  Side.Ours
                  (parts [ Work, 4; Carry, 4; Move, 5 ])
                  (at "W17S25" 13 26)
                  None with
                TicksToLive = 1_287
                Energy = 64
            }
            { body
                  "defender-970852-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; Attack, 6 ])
                  (at "W17S25" 20 31)
                  None with
                TicksToLive = 1_341
            }
            { body
                  "guard-970892-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 13 24)
                  None with
                TicksToLive = 1_375
            }
            { body
                  "anchor-970943-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 1_426
                Energy = 50
            }
            { body
                  "worker-970483-Spawn11"
                  Side.Ours
                  (parts [ Work, 4; Carry, 4; Move, 4 ])
                  (at "W17S25" 23 36)
                  None with
                TicksToLive = 972
                Energy = 188
            }
            { body
                  "anchor-969618-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 95
                Energy = 50
            }
            { body
                  "worker-969759-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 7 28)
                  None with
                TicksToLive = 362
                Energy = 638
            }
            { body
                  "hauler-969982-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 17 32)
                  None with
                TicksToLive = 579
                Energy = 890
            }
            { body
                  "hauler-970270-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 5 5)
                  None with
                TicksToLive = 867
            }
            { body
                  "anchor-970434-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 911
                Energy = 50
            }
            { body
                  "upgrader-970821-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 14 37)
                  None with
                TicksToLive = 1_421
            }
            { body
                  "worker-970069-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 672
                Energy = 650
            }
            { body
                  "reserver-971002-Spawn8"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W15S28" 6 7)
                  None with
                TicksToLive = 567
            }
            { body
                  "hauler-969423-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 16 27)
                  None with
                TicksToLive = 20
                Energy = 1_464
            }
        ]

    arena 971_046 [ w18s25; w17s25; w15s28 ] colonies bodies

// ---- arena capture doorway-jam (movement S0) -------------------------------
// `npm run capture -- W17S25 972061 --rooms W18S25,W15S28 --name doorwayJam`, read at live t986,357.
// The room-history replay at t972,061, the state after that tick ran: arena
// tick 0 runs as t972,062.
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/972000.json
//   W18S25  https://screeps.com/room-history/shardSeason/W18S25/972000.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/972000.json
// Read too late for live Memory: the Raid log, ArmedSeen, Healers, ExitWatches
// and Sightings start empty, and every body of ours starts idle. The 272
// roads are laid (#560); W18S25's reservation and 5 tombstones are not
// placed, nor the anchor Spawn11 was casting.

/// W17S25 at t972,061 (the movement ADR's doorway jam): no hostile in sight;
/// guard-970892-Spawn11 stands idle on (1,15), the tile inside the west exit
/// and outside the rampart line at x = 2; hauler-971381-Spawn11, carrying 600
/// home from W18S25, lands on the exit tile (0,15) every other tick, cannot
/// step onto the guard's tile and is carried back to W18S25 (49,15) (live
/// t972,055-972,085).
let private doorwayJam () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 39_517 70_919
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_150
        |> withStructures
            [
                for x, y in [ 10, 24; 12, 24; 13, 25; 13, 31; 15, 31; 17, 29; 17, 31 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        11, 25
                        11, 27
                        12, 28
                        12, 30
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        16, 28
                        16, 30
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 81_200 250_000 { X = 16; Y = 34 } with
                    Energy = 386
                }
                structureOf "container" None 123_600 250_000 { X = 17; Y = 20 }
                { structureOf "rampart" (Some Side.Ours) 94_321 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 972_135
                }
                { structureOf "rampart" (Some Side.Ours) 94_561 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 972_116
                }
                { structureOf "rampart" (Some Side.Ours) 90_021 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 972_119
                }
                { structureOf "rampart" (Some Side.Ours) 96_931 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 972_124
                }
                { structureOf "rampart" (Some Side.Ours) 96_871 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 972_096
                }
                { structureOf "rampart" (Some Side.Ours) 91_631 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 972_098
                }
                { structureOf "rampart" (Some Side.Ours) 92_921 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 972_123
                }
                { structureOf "rampart" (Some Side.Ours) 97_101 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 972_085
                }
                { structureOf "rampart" (Some Side.Ours) 93_701 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 972_102
                }
                { structureOf "rampart" (Some Side.Ours) 94_461 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 972_117
                }
                { structureOf "rampart" (Some Side.Ours) 94_961 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 972_066
                }
                { structureOf "rampart" (Some Side.Ours) 12_401 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 972_101
                }
                { structureOf "rampart" (Some Side.Ours) 2_701 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 972_077
                }
                { structureOf "rampart" (Some Side.Ours) 96_321 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 972_065
                }
                { structureOf "rampart" (Some Side.Ours) 36_201 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 972_122
                }
                { structureOf "rampart" (Some Side.Ours) 96_301 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 972_067
                }
                { structureOf "rampart" (Some Side.Ours) 100_301 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 972_063
                }
                { structureOf "rampart" (Some Side.Ours) 95_071 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 972_076
                }
                { structureOf "rampart" (Some Side.Ours) 93_681 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 972_094
                }
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]
        |> withSite Side.Ours "storage" None 0 { X = 13; Y = 27 }
        |> withSite Side.Ours "extension" None 2_841 { X = 10; Y = 26 }
        |> withSite Side.Ours "extension" None 200 { X = 10; Y = 28 }
        |> withSite Side.Ours "extension" None 1_080 { X = 10; Y = 30 }
        |> withRoads
            [
                2, 17, 3_300
                3, 18, 3_300
                4, 19, 2_900
                5, 20, 2_900
                6, 21, 2_600
                7, 22, 13_000
                8, 23, 13_000
                9, 24, 12_000
                10, 25, 11_500
                10, 27, 25_000
                10, 29, 25_000
                11, 24, 17_500
                11, 26, 9_800
                11, 28, 18_000
                11, 30, 5_000
                12, 23, 18_000
                12, 25, 11_600
                12, 27, 12_000
                12, 29, 3_200
                13, 24, 2_300
                13, 26, 3_000
                13, 28, 3_700
                13, 30, 1_900
                14, 23, 2_400
                14, 25, 1_300
                14, 27, 3_600
                14, 29, 3_900
                14, 31, 1_700
                15, 22, 2_000
                15, 28, 4_400
                15, 30, 3_600
                15, 32, 1_700
                16, 21, 3_200
                16, 29, 1_500
                16, 31, 2_900
                16, 33, 1_300
                16, 35, 3_300
                16, 37, 4_700
                16, 38, 19_000
                16, 39, 8_000
                16, 41, 3_800
                16, 43, 2_500
                16, 44, 2_500
                17, 20, 1_800
                17, 30, 4_900
                17, 32, 3_200
                17, 34, 2_900
                17, 36, 4_800
                17, 38, 4_800
                17, 39, 8_000
                17, 40, 23_500
                17, 42, 2_800
                17, 45, 2_900
                18, 29, 4_900
                18, 31, 3_500
                18, 35, 2_600
                18, 37, 4_800
                18, 39, 4_900
                18, 41, 2_500
                18, 46, 3_000
                19, 30, 4_800
                19, 34, 3_300
                19, 36, 3_800
                19, 38, 4_900
                19, 40, 2_500
                19, 47, 3_000
                20, 31, 3_800
                20, 33, 3_300
                20, 35, 4_900
                20, 37, 4_300
                20, 39, 2_000
                20, 48, 2_700
                21, 32, 5_000
                21, 34, 3_800
                21, 36, 4_900
                21, 38, 3_700
                22, 33, 5_000
                22, 35, 4_900
                22, 37, 1_400
                23, 34, 4_900
                23, 36, 1_300
                24, 35, 3_200
                25, 34, 3_900
                26, 33, 4_300
                27, 32, 4_000
                28, 33, 7_000
                29, 34, 8_000
                30, 33, 8_500
                31, 32, 9_000
            ]

    let w18s25 =
        room "W18S25"
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 31; Y = 5 } with
                    Energy = 369
                }
            ]
        |> withRoads
            [
                28, 9, 2_900
                29, 8, 3_200
                29, 10, 2_800
                30, 7, 3_200
                30, 11, 3_100
                31, 6, 3_400
                31, 12, 2_700
                32, 13, 2_900
                33, 14, 2_500
                34, 15, 3_300
                35, 16, 2_500
                36, 17, 2_500
                37, 16, 2_400
                38, 15, 2_400
                39, 14, 2_300
                40, 13, 2_100
                41, 12, 2_300
                42, 13, 2_100
                43, 13, 1_900
                44, 13, 1_800
                45, 13, 2_000
                46, 14, 2_000
                47, 15, 1_900
                48, 16, 2_000
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_727_681
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 30
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 27
                        13, 29
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        15, 35
                        16, 26
                        16, 32
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        18, 34
                        19, 27
                        19, 31
                        19, 33
                        20, 26
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        21, 35
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 6; Y = 29 } with
                    Energy = 240
                }
                { structureOf "container" None 125_100 250_000 { X = 11; Y = 18 } with
                    Energy = 732
                }
                { structureOf "container" None 158_100 250_000 { X = 23; Y = 29 } with
                    Energy = 824
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 102_401 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 972_146
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 972_106
                }
                { structureOf "rampart" (Some Side.Ours) 102_401 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 972_065
                }
                { structureOf "rampart" (Some Side.Ours) 98_901 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 972_120
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 972_107
                }
                { structureOf "rampart" (Some Side.Ours) 102_301 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 972_160
                }
                { structureOf "rampart" (Some Side.Ours) 102_101 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 972_082
                }
                { structureOf "rampart" (Some Side.Ours) 103_101 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 972_130
                }
                { structureOf "rampart" (Some Side.Ours) 100_301 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 972_123
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 33_074
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]
        |> withRoads
            [
                4, 5, 3_700
                5, 4, 4_100
                5, 6, 3_700
                5, 44, 4_500
                6, 3, 3_200
                6, 7, 4_600
                6, 29, 2_800
                6, 43, 4_100
                6, 45, 4_900
                7, 2, 3_100
                7, 8, 3_700
                7, 28, 5_000
                7, 42, 3_700
                7, 46, 2_600
                8, 1, 3_700
                8, 9, 3_600
                8, 28, 4_300
                8, 41, 4_200
                8, 47, 3_500
                9, 10, 4_600
                9, 27, 4_700
                9, 28, 4_600
                9, 40, 3_700
                9, 48, 4_600
                10, 11, 3_700
                10, 26, 2_500
                10, 29, 2_600
                10, 39, 4_200
                11, 12, 3_300
                11, 18, 3_300
                11, 27, 3_700
                11, 30, 4_500
                11, 38, 4_800
                12, 13, 2_700
                12, 17, 4_500
                12, 25, 20_500
                12, 27, 13_200
                12, 28, 2_600
                12, 29, 4_400
                12, 31, 3_300
                12, 33, 20_100
                12, 35, 17_400
                12, 37, 4_200
                13, 14, 2_700
                13, 16, 5_000
                13, 26, 13_200
                13, 28, 3_200
                13, 30, 5_000
                13, 32, 3_000
                13, 34, 2_900
                13, 36, 4_500
                14, 15, 4_200
                14, 25, 16_500
                14, 27, 4_100
                14, 29, 3_900
                14, 31, 4_600
                14, 33, 2_900
                14, 35, 4_400
                15, 15, 4_400
                15, 26, 3_700
                15, 28, 3_700
                15, 30, 4_200
                15, 32, 3_600
                15, 34, 2_900
                16, 16, 4_400
                16, 25, 13_400
                16, 27, 4_200
                16, 29, 4_200
                16, 31, 4_900
                16, 33, 4_700
                16, 35, 3_300
                17, 17, 3_800
                17, 26, 17_700
                17, 28, 3_100
                17, 30, 3_400
                17, 32, 2_800
                17, 34, 3_100
                18, 18, 3_500
                18, 25, 17_500
                18, 27, 15_200
                18, 29, 2_500
                18, 31, 4_800
                18, 33, 4_600
                19, 19, 16_900
                19, 22, 13_800
                19, 24, 19_400
                19, 26, 14_300
                19, 28, 2_700
                19, 30, 4_700
                19, 32, 4_000
                20, 20, 17_700
                20, 21, 16_800
                20, 23, 15_500
                20, 25, 16_100
                20, 27, 15_500
                20, 29, 4_400
                20, 31, 4_600
                20, 33, 17_000
                21, 21, 3_700
                21, 22, 2_400
                21, 23, 4_900
                21, 24, 19_400
                21, 25, 17_900
                21, 26, 20_300
                21, 28, 18_400
                21, 30, 4_900
                21, 32, 18_300
                21, 34, 16_600
                22, 24, 4_300
                22, 25, 13_100
                22, 27, 21_600
                22, 28, 4_800
                22, 29, 4_000
                22, 30, 15_900
                22, 31, 19_200
                22, 32, 19_500
                22, 33, 20_200
                22, 35, 3_700
                23, 26, 16_600
                23, 28, 17_700
                23, 29, 20_100
                23, 30, 20_900
                23, 31, 13_500
                23, 32, 19_300
                23, 33, 15_200
                23, 34, 4_200
                24, 25, 14_400
                24, 28, 20_900
                24, 29, 21_500
                24, 30, 19_900
                24, 33, 15_400
                25, 24, 5_000
                25, 28, 21_100
                25, 29, 20_700
                25, 34, 21_800
                26, 23, 16_300
                27, 22, 13_500
                28, 21, 13_200
                28, 34, 20_600
                29, 20, 15_000
                30, 19, 13_400
                31, 18, 4_300
                32, 17, 4_800
                33, 16, 3_400
                34, 15, 2_500
                35, 14, 3_300
                36, 13, 18_400
                37, 12, 2_300
                38, 11, 3_600
                39, 12, 4_300
                40, 13, 13_100
                41, 14, 13_000
                42, 15, 3_000
                43, 16, 3_000
                44, 17, 2_400
                45, 18, 2_400
                46, 19, 4_000
                47, 20, 4_300
                48, 21, 3_300
            ]

    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            { body
                  "worker-970795-Spawn11"
                  Side.Ours
                  (parts [ Work, 4; Carry, 4; Move, 5 ])
                  (at "W17S25" 15 32)
                  None with
                TicksToLive = 271
            }
            { body
                  "defender-970852-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; Attack, 6 ])
                  (at "W17S25" 32 31)
                  None with
                TicksToLive = 325
            }
            { body
                  "guard-970892-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 1 15)
                  None with
                TicksToLive = 359
            }
            { body
                  "anchor-970943-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 410
                Energy = 14
            }
            { body
                  "ranger-971410-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 37)
                  None with
                TicksToLive = 973
            }
            { body
                  "hauler-971755-Spawn11"
                  Side.Ours
                  (parts [ Carry, 14; Move, 7 ])
                  (at "W17S25" 40 27)
                  None with
                TicksToLive = 1_255
                Energy = 46
            }
            { body
                  "worker-971946-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 7; Move, 6 ])
                  (at "W17S25" 13 28)
                  None with
                TicksToLive = 1_437
            }
            { body
                  "ranger-971130-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 693
            }
            { body
                  "worker-972000-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 7; Move, 6 ])
                  (at "W17S25" 16 21)
                  None with
                TicksToLive = 1_491
                Energy = 289
            }
            { body
                  "hauler-971818-Spawn11"
                  Side.Ours
                  (parts [ Carry, 14; Move, 7 ])
                  (at "W17S25" 20 30)
                  None with
                TicksToLive = 1_318
            }
            { body
                  "hauler-971381-Spawn11"
                  Side.Ours
                  (parts [ Carry, 12; Move, 6 ])
                  (at "W17S25" 0 15)
                  None with
                TicksToLive = 872
                Energy = 600
            }
            { body
                  "anchor-970998-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W18S25" 31 5)
                  None with
                TicksToLive = 465
                Energy = 50
            }
            { body
                  "reserver-971527-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W18S25" 17 8)
                  None with
                TicksToLive = 70
            }
            { body
                  "worker-971252-Spawn11"
                  Side.Ours
                  (parts [ Work, 4; Carry, 6; Move, 5 ])
                  (at "W18S25" 31 6)
                  None with
                TicksToLive = 734
                Energy = 300
            }
            { body
                  "guard-970714-Spawn3"
                  Side.Ours
                  (parts [ Tough, 5; Move, 25; Attack, 15; Heal, 5 ])
                  (at "W15S28" 7 1)
                  None with
                TicksToLive = 301
            }
            { body
                  "upgrader-970821-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 405
                Energy = 26
            }
            { body
                  "anchor-971063-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 524
                Energy = 50
            }
            { body
                  "worker-971258-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 29)
                  None with
                TicksToLive = 845
                Energy = 658
            }
            { body
                  "hauler-971479-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 22 30)
                  None with
                TicksToLive = 1_060
                Energy = 1_600
            }
            { body
                  "anchor-971904-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_365
                Energy = 50
            }
            { body
                  "hauler-970917-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 27 18)
                  None with
                TicksToLive = 498
            }
            { body
                  "hauler-971764-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 8 47)
                  None with
                TicksToLive = 1_345
                Energy = 1_412
            }
        ]

    arena 972_062 [ w17s25; w18s25; w15s28 ] colonies bodies

// ---- arena capture idle-ranger-in-reach (#533, movement S0) ----------------
// `npm run capture -- W18S25 973267 --rooms W17S25,W15S28,W17S26 --name idleRangerInReach`, read at live t986,392.
// The room-history replay at t973,267, the state after that tick ran: arena
// tick 0 runs as t973,268.
//   W18S25  https://screeps.com/room-history/shardSeason/W18S25/973200.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/973200.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/973200.json
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/973200.json
// Read too late for live Memory: W17S25's stand-down of W18S25 is seated as
// `shutOutpostCripple` seats it (#534's "Why": opened t973,265 on a Lose while
// Shade461 healed Idol768); the rest of the Raid log, ArmedSeen, Healers,
// ExitWatches and Sightings start empty, and every body of ours starts idle.
// The reservations, roads and tombstones are not placed, nor the ranger and
// the hauler W15S28 was casting.

/// W18S25 at t973,267 (#533's "Why"): the stand-down took
/// ranger-972464-Spawn8's Guard (`task-gone`) on (46,14), two tiles from
/// Idol768 (7 live ATTACK, 1 live MOVE) whom Shade461 heals from the exit
/// tile; the ranger itself is down to 2 live MOVE. Live it stood idle there,
/// the melee stepped beside it at t973,273 and struck it to death by
/// t973,294.
let private idleRangerInReach () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"
    let odiodin = Side.Player "Odiodin"

    let w18s25 =
        room "W18S25"
        |> withStructures
            [
                { structureOf "container" None 120_100 250_000 { X = 31; Y = 5 } with
                    Energy = 796
                }
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 38_311 70_919
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in
                    [
                        10, 24
                        10, 26
                        10, 28
                        10, 30
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 25
                        13, 31
                        14, 30
                        15, 29
                        15, 31
                        16, 28
                        16, 30
                        17, 29
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in [ 14, 26; 15, 27; 17, 31 ] ->
                    structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 66_200 250_000 { X = 16; Y = 34 } with
                    Energy = 100
                }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 216
                }
                { structureOf "rampart" (Some Side.Ours) 90_721 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 973_323
                }
                { structureOf "rampart" (Some Side.Ours) 90_961 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 973_304
                }
                { structureOf "rampart" (Some Side.Ours) 89_411 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 973_307
                }
                { structureOf "rampart" (Some Side.Ours) 93_331 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 973_312
                }
                { structureOf "rampart" (Some Side.Ours) 93_271 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 973_284
                }
                { structureOf "rampart" (Some Side.Ours) 98_531 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 973_286
                }
                { structureOf "rampart" (Some Side.Ours) 99_521 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 973_311
                }
                { structureOf "rampart" (Some Side.Ours) 93_501 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 973_273
                }
                { structureOf "rampart" (Some Side.Ours) 90_101 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 973_290
                }
                { structureOf "rampart" (Some Side.Ours) 90_861 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 973_305
                }
                { structureOf "rampart" (Some Side.Ours) 91_061 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 973_353
                }
                { structureOf "rampart" (Some Side.Ours) 8_801 3_000_000 { X = 13; Y = 29 } with
                    IsPublic = true
                    NextDecay = 973_289
                }
                { structureOf "rampart" (Some Side.Ours) 1_201 3_000_000 { X = 14; Y = 28 } with
                    IsPublic = true
                    NextDecay = 973_364
                }
                { structureOf "rampart" (Some Side.Ours) 92_421 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 973_352
                }
                { structureOf "rampart" (Some Side.Ours) 32_601 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 973_310
                }
                { structureOf "rampart" (Some Side.Ours) 92_401 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 973_354
                }
                { structureOf "rampart" (Some Side.Ours) 96_401 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 973_350
                }
                { structureOf "rampart" (Some Side.Ours) 91_171 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 973_363
                }
                { structureOf "rampart" (Some Side.Ours) 90_081 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 973_282
                }
                towerOf Side.Ours { X = 13; Y = 29 } 890
            ]
        |> withSite Side.Ours "storage" None 13_863 { X = 13; Y = 27 }

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_753_712
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 27
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        15, 35
                        16, 26
                        16, 32
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 28
                        18, 32
                        18, 34
                        19, 31
                        19, 33
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        21, 35
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                for x, y in [ 13, 25; 13, 31; 14, 26; 14, 30; 15, 27; 15, 29; 15, 31; 16, 30 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in [ 13, 29; 18, 26; 19, 27; 20, 26 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 6; Y = 29 } with
                    Energy = 156
                }
                { structureOf "container" None 125_100 250_000 { X = 11; Y = 18 } with
                    Energy = 120
                }
                structureOf "container" None 148_100 250_000 { X = 23; Y = 29 }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 24 } with
                    Energy = 50
                    Id = "extension-12-24-W15S28"
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 30 } with
                    Energy = 100
                    Id = "extension-12-30-W15S28"
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 102_001 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 973_334
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 973_294
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 973_352
                }
                { structureOf "rampart" (Some Side.Ours) 99_401 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 973_281
                }
                { structureOf "rampart" (Some Side.Ours) 102_701 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 973_295
                }
                { structureOf "rampart" (Some Side.Ours) 101_901 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 973_348
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 973_270
                }
                { structureOf "rampart" (Some Side.Ours) 101_101 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 973_318
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 973_311
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 31_183
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 27; Y = 9 } with
                    Energy = 535
                }
            ]

    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            { body
                  "anchor-972488-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W18S25" 38 15)
                  None with
                TicksToLive = 749
                Energy = 50
            }
            { body
                  "reserver-972681-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W18S25" 24 5)
                  None with
                TicksToLive = 24
            }
            // ranger-972464-Spawn8: live 2 MOVE, 14 RANGED_ATTACK, 7 HEAL.
            { body
                  "ranger-972464-Spawn8"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W18S25" 46 14)
                  None with
                Hits = 2_286
                TicksToLive = 821
                Fatigue = 11
            }
            // Idol768: live 7 ATTACK, 1 MOVE; it stepped on the ranger as
            // soon as its fatigue ran out.
            { body
                  "Idol768"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W18S25" 48 16)
                  (Some(Chase(Nearest, None))) with
                Hits = 734
                TicksToLive = 1_104
                Fatigue = 9
            }
            // Shade461: live 6 MOVE, 7 HEAL; it healed Idol768 from the exit
            // tile, crossing to W17S25 and back.
            { body
                  "Shade461"
                  trepidimous
                  (parts [ Move, 11; Heal, 7 ])
                  (at "W18S25" 49 15)
                  (Some(Do [ Act.Heal "Idol768" ])) with
                Hits = 1_278
                TicksToLive = 746
            }
            { body
                  "hauler-971755-Spawn11"
                  Side.Ours
                  (parts [ Carry, 14; Move, 7 ])
                  (at "W17S25" 16 44)
                  None with
                TicksToLive = 49
            }
            { body
                  "worker-972000-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 7; Move, 6 ])
                  (at "W17S25" 12 29)
                  None with
                TicksToLive = 285
                Energy = 350
            }
            { body
                  "anchor-972458-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 719
                Energy = 50
            }
            { body
                  "guard-972528-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 17 45)
                  None with
                TicksToLive = 789
            }
            { body
                  "ranger-972663-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 33)
                  None with
                TicksToLive = 1_020
            }
            { body "scout-W17S22-972936" odiodin (parts [ Move, 1 ]) (at "W17S25" 13 26) (Some Hold) with
                TicksToLive = 1_170
            }
            { body
                  "hauler-971818-Spawn11"
                  Side.Ours
                  (parts [ Carry, 14; Move, 7 ])
                  (at "W17S25" 10 25)
                  None with
                TicksToLive = 112
            }
            { body
                  "worker-971946-Spawn11"
                  Side.Ours
                  (parts [ Work, 5; Carry, 7; Move, 6 ])
                  (at "W17S25" 14 31)
                  None with
                TicksToLive = 231
                Energy = 350
            }
            { body
                  "guard-973227-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 19 30)
                  None with
                TicksToLive = 1_488
            }
            { body
                  "hauler-972861-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 12 25)
                  None with
                TicksToLive = 1_164
                Energy = 611
            }
            { body
                  "worker-972372-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 13 30)
                  None with
                TicksToLive = 663
                Energy = 350
            }
            { body
                  "anchor-972054-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 20 48)
                  None with
                TicksToLive = 315
                Energy = 50
            }
            { body
                  "anchor-971904-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 159
                Energy = 50
            }
            { body
                  "worker-972250-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 11 28)
                  None with
                TicksToLive = 631
                Fatigue = 12
                Energy = 350
            }
            { body
                  "upgrader-972317-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 695
            }
            { body
                  "hauler-972400-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 19 26)
                  None with
                TicksToLive = 775
                Energy = 96
            }
            { body
                  "anchor-972544-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 799
                Energy = 50
            }
            { body
                  "worker-972857-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 29)
                  None with
                TicksToLive = 1_238
                Energy = 72
            }
            { body
                  "hauler-972938-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 12 28)
                  None with
                TicksToLive = 1_313
                Energy = 100
            }
            { body
                  "reserver-973249-Spawn8"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W15S28" 6 45)
                  None with
                TicksToLive = 586
            }
            { body
                  "hauler-971764-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 8 1)
                  None with
                TicksToLive = 139
                Energy = 1_280
            }
            { body
                  "anchor-972558-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 819
                Energy = 50
            }
            { body
                  "reserver-973148-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 9 22)
                  None with
                TicksToLive = 491
            }
        ]

    let a = arena 973_268 [ w18s25; w17s25; w15s28; w17s26 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                Raids =
                    Map.ofList
                        [
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            RoomName = "W18S25"
                                            Opened = 973_265
                                            LastSeen = 973_265
                                            Expiry = 973_465
                                            Basis = StandDownBasis.PlayerRaidHold
                                            Stronghold = false
                                            Raiders = set [ "Idol768"; "Shade461" ]
                                            PlayerSince = Some 973_265
                                            Chased = false
                                            Bounced = false
                                            HolderUnread = false
                                        }
                                    ]
                            }
                        ]
            }
    }

// ---- arena capture w17s26-t1014100 -------------------------------------------
// `npm run capture -- W17S26 1014100 --rooms W17S25`, read at live t1,014,280.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/1014100.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/1014100.json
// W17S26 is reserved by 2 to t1014110: the arena has no reservations.
// Spawn11 is spawning guard-1014093-Spawn11 until t1014153: not placed.
// Zero3956 walks its live tiles t1,014,101-149 (`observe history`), slowed to a
// step every second tick from t1,014,117, when the guard's swings took its
// three head MOVE, and swings at the core beside (13,23) from t1,014,149.

/// #613: Trepidimous' Zero3956 (18M 17A, no heal) against the lone 6M 4RA 2H
/// ranger W17S26's stand-down left idle at t1,014,093.
let private zeroAtTheCore (zero: Behaviour) () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"
    let odiodin = Side.Player "Odiodin"

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 133_700 250_000 { X = 27; Y = 9 } with
                    Energy = 163
                }
                structureOf
                    "invaderCore"
                    (Some(Side.Npc "Invader"))
                    92_880
                    100_000
                    { X = 12; Y = 24 }
            ]
        |> withRoads
            [
                20, 1, 4_200
                21, 2, 4_300
                22, 3, 4_800
                23, 4, 2_500
                24, 5, 4_200
                25, 6, 2_600
                26, 7, 4_300
                27, 8, 2_500
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 5 3
        |> withControllerClocks 0 0 79_938 52_681
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_800
        |> withStructures
            [
                for x, y in [ 9, 23; 9, 25; 9, 27; 9, 29; 11, 23; 13, 23 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        10, 24
                        10, 26
                        10, 28
                        10, 30
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 25
                        13, 31
                        14, 24
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 28
                        16, 30
                        16, 32
                        17, 29
                        17, 31
                        18, 30
                        19, 31
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                structureOf "container" None 145_800 250_000 { X = 16; Y = 34 }
                { structureOf "container" None 145_000 250_000 { X = 17; Y = 20 } with
                    Energy = 108
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 6_069
                }
                towerOf Side.Ours { X = 11; Y = 29 } 1_000
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]
        |> withStructures
            [
                for x, y in
                    [
                        2, 15
                        2, 16
                        2, 17
                        2, 18
                        2, 19
                        2, 20
                        2, 23
                        2, 24
                        2, 25
                        2, 26
                        2, 27
                        11, 29
                        13, 27
                        13, 29
                        14, 28
                        16, 44
                        17, 20
                        17, 44
                        18, 44
                        32, 31
                        32, 32
                    ] -> structureOf "rampart" (Some Side.Ours) 100_000 10_000_000 { X = x; Y = y }
            ]

    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W17S25" ])

    let bodies: Body list =
        [
            { body
                  "Zero3956"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W17S26" 9 1)
                  (Some zero) with
                TicksToLive = 1_459
            }
            { body
                  "ranger-1013698-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; RangedAttack, 4; Heal, 2 ])
                  (at "W17S26" 15 19)
                  None with
                TicksToLive = 1_132
                Fatigue = 36
            }
            { body
                  "ranger-1013734-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; RangedAttack, 4; Heal, 2 ])
                  (at "W17S26" 22 14)
                  None with
                TicksToLive = 1_168
            }
            { body
                  "reserver-1013856-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 10 22)
                  None with
                TicksToLive = 366
            }
            { body
                  "anchor-1013881-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 22 3)
                  None with
                TicksToLive = 1_309
            }
            { body
                  "guard-1013931-Spawn11"
                  Side.Ours
                  (parts [ Tough, 2; Move, 10; Attack, 6; Heal, 2 ])
                  (at "W17S26" 17 20)
                  None with
                TicksToLive = 1_389
            }
            { body
                  "worker-1012533-Spawn11"
                  Side.Ours
                  (parts [ Work, 9; Carry, 9; Move, 9 ])
                  (at "W17S25" 2 17)
                  None with
                TicksToLive = 12
                Energy = 450
            }
            { body
                  "hauler-1012841-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W17S25" 12 26)
                  None with
                TicksToLive = 347
            }
            { body
                  "anchor-1012949-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 371
                Energy = 50
            }
            { body
                  "hauler-1012989-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W17S25" 13 26)
                  None with
                TicksToLive = 495
                Energy = 1_200
            }
            { body
                  "upgrader-1013787-Spawn11"
                  Side.Ours
                  (parts [ Work, 11; Carry, 1; Move, 11 ])
                  (at "W17S25" 19 34)
                  None with
                TicksToLive = 1_254
            }
            { body
                  "scout-W17S22-1012929"
                  odiodin
                  (parts [ Move, 1 ])
                  (at "W17S25" 14 14)
                  (Some Hold) with
                TicksToLive = 330
            }
        ]

    let a = arena 1_014_101 [ w17s26; w17s25 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                Assignments =
                    Map.ofList
                        [
                            "ranger-1013734-Spawn11", "guard:W19S25"
                            "hauler-1012841-Spawn11", "withdraw:storage-13-27"
                            "anchor-1012949-Spawn11", "harvest:6a8caa7cdd4872bccd318c75"
                            "hauler-1012989-Spawn11", "withdraw:storage-13-27"
                        ]
                Towered = Set.ofList [ "W17S25" ]
                Raids =
                    Map.ofList
                        [
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 1_014_100
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Bounced = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            HolderUnread = false
                                        }
                                        {
                                            RoomName = "W17S26"
                                            Opened = 1_014_093
                                            LastSeen = 1_014_100
                                            Expiry = 1_014_293
                                            Basis = StandDownBasis.PlayerRaidHold
                                            Stronghold = false
                                            Bounced = false
                                            Raiders = Set.ofList [ "Zero3956" ]
                                            PlayerSince = Some 1_014_093
                                            Chased = false
                                            HolderUnread = false
                                        }
                                        {
                                            RoomName = "W18S29"
                                            Opened = 986_839
                                            LastSeen = 1_014_100
                                            Expiry = 1_016_242
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            HolderUnread = false
                                        }
                                    ]
                                Threatened =
                                    Map.ofList
                                        [
                                            "W17S26",
                                            ({ Until = 1_015_593 }: Fabot.Core.Observe.ThreatLatch)
                                        ]
                                HomeRaided = Some 1_013_957
                            }
                        ]
            }
    }

/// Zero3956's live tiles, t1,014,101-149, to the core it swings at.
let private zeroLiveTiles =
    [
        for x in 10..20 -> RoomPos.at "W17S26" { X = x; Y = 1 }
        for x, y in
            [
                21, 2
                22, 3
                23, 4
                24, 5
                25, 6
                26, 7
                27, 8
                26, 9
                25, 10
                24, 11
                23, 12
                22, 13
                22, 14
                21, 15
                20, 16
                19, 17
                18, 18
                18, 19
                17, 20
                16, 21
                15, 21
                14, 22
                13, 23
            ] -> RoomPos.at "W17S26" { X = x; Y = y }
    ]

/// An arena run a tick at a time: the trace, and one creep's Task at the end
/// of every tick ("" for none).
let private taskEachTick (creep: string) (ticks: int) (start: Arena) =
    let rec go n (a: Arena) traces tasks =
        if n = 0 then
            List.rev traces |> List.concat, List.rev tasks
        else
            let next, trace = a |> run 1
            let task = Map.tryFind creep next.Carried.Assignments |> Option.defaultValue ""
            go (n - 1) next (trace :: traces) (task :: tasks)

    go ticks start [] []

[<Tests>]
let arenaBorderTests =
    testList
        "arena border play"
        [
            // #613 (live t1,014,093-291): the idle ranger walked in toward
            // Zero3956 and off again for 200 ticks, and the one fit ranger
            // went elsewhere: `Gait.fallsBehind` asked the melee to fall
            // behind on road too, where 15 MOVE under 17 ATTACK ties a
            // ranger, so no ranger kited the body that steps every second
            // tick on plain, and #602's release kept no kiter it once caught.
            // The live Zero3956 stood at the core it broke; here it walks its
            // live tiles and then chases, as our pricing must assume it does.
            // The capture parts from live at t16: the live guard stalled a
            // tile short and traded 360 with the melee, slowing it from
            // t1,014,117; here the guard flees untouched, the melee keeps its
            // pace, and the idle ranger-1013698 is caught at t23 (live never).
            test
                "a lone ranger takes the Guard of a melee it out-paces on plain, keeps it, and kites it dead" {
                let trace, tasks =
                    zeroAtTheCore
                        (Phases [ 50, Waypoints zeroLiveTiles; 0, Chase(Nearest, None) ])
                        ()
                    |> taskEachTick "ranger-1013734-Spawn11" 160

                let guard = "guard:W17S26"
                let died = diedOn "Zero3956" trace

                Expect.isSome died $"Zero3956 dies to the kite\n{describe trace}"

                let taken = tasks |> List.findIndex ((=) guard)

                Expect.isTrue
                    (tasks.[taken .. died.Value] |> List.forall ((=) guard))
                    $"ranger-1013734 keeps the Guard from t{taken} until the melee dies: {tasks}"

                Expect.isNone (diedOn "ranger-1013734-Spawn11" trace) "the kiter lives"
                Expect.isNone (diedOn "ranger-1013698-Spawn11" trace) "and so does the idle one"
            }
            // Live (t985,560-985,681) both rangers stood beside the core for
            // 100+ ticks shooting nothing: the core claimed the room ahead of
            // the hauler's kill. On main this capture reproduces that (both on
            // the core's ring, no shot, the core untouched over 40 ticks).
            // Characterisation (movement S0): #558's capture for this stage; the
            // range-3 core ring is a stand tile S4's picker takes over, keeping
            // the outcome.
            test
                "#558 capture t985,660: both rangers shoot Obsidian522 down, then one shoots the core from three, never beside it" {
                let _, trace = haulerBesideCore () |> run 80
                let failure = describe trace
                let rangers = [ "ranger-984393-Spawn3"; "ranger-985441-Spawn11" ]
                let core = RoomPos.at "W17S26" { X = 12; Y = 25 }

                let shotAt target (t: TickTrace) ranger =
                    t.Ours |> List.contains (RangedAttackCreep(ranger, target))

                let dead =
                    match diedOn "Obsidian522" trace with
                    | Some tick -> tick
                    | None -> failtest $"Obsidian522 dies\n{failure}"

                Expect.isLessThanOrEqual dead 35 $"at 160 a tick, by t35\n{failure}"

                // The one beside the core shoots it at t0, on its way to the
                // creep, which stands out of its reach.
                Expect.isTrue
                    (trace
                     |> List.filter (fun t -> t.Tick >= 5 && t.Tick < dead)
                     |> List.forall (fun t -> rangers |> List.forall (shotAt "Obsidian522" t)))
                    $"both shoot it every tick from t5 until it dies\n{failure}"

                let coreShots =
                    trace
                    |> List.collect (fun t ->
                        t.Ours
                        |> List.choose (function
                            | RangedAttackStructure(ranger, "invaderCore-12-25") ->
                                Some(t.Tick, ranger)
                            | _ -> None))

                let afterward = coreShots |> List.filter (fun (tick, _) -> tick > dead)
                Expect.isNonEmpty afterward $"a ranger then shoots the core\n{failure}"

                for tick, ranger in afterward do
                    let at =
                        pathOf ranger trace
                        |> List.tryFind (fun (t, _) -> t = tick - 1)
                        |> Option.bind (fun (_, s) -> RoomPos.range s.At core)

                    Expect.equal
                        at
                        (Some Engine.rangedRange)
                        $"{ranger} shoots from three at t{tick}\n{failure}"

                for ranger in rangers do
                    Expect.isFalse
                        (pathOf ranger trace
                         |> List.exists (fun (tick, s) ->
                             tick > dead
                             && RoomPos.range s.At core |> Option.exists (fun r -> r <= 1)))
                        $"{ranger} never stands beside the core once the creep is dead\n{failure}"

                Expect.isLessThan
                    (trace |> List.last |> (fun t -> Map.find "invaderCore-12-25" t.Structures))
                    66_670
                    $"the core is struck\n{failure}"
            }
            test "a guard that crosses in steps off the border ring rather than being carried back" {
                let invader =
                    body
                        "invader-1"
                        (Side.Npc "Invader")
                        Fabot.Core.Tests.Decide.Fixtures.smallMelee
                        (RoomPos.at "W12S27" { X = 8; Y = 48 })
                        (Some Invader)

                let _, trace = outpostRaid invader |> run 30
                let guard = "guard-1-Spawn1"
                let path = pathOf guard trace |> Map.ofList

                let crossings =
                    trace
                    |> List.choose (fun t ->
                        t.Events
                        |> List.tryPick (function
                            | Crossed(who, _, _) when who = guard -> Some t.Tick
                            | _ -> None))

                Expect.isNonEmpty crossings "the guard crossed at least once"

                for tick in crossings do
                    match Map.tryFind (tick + 1) path with
                    | Some next ->
                        Expect.isFalse
                            (Seam.onRing (RoomPos.pos next.At))
                            $"t{tick + 1}: off the ring after crossing\n{describe trace}"
                    | None -> ()
            }

            test "#450: a guard holds the exit a kiter left by and hits it on re-entry" {
                // Seven HEAL out-heal the kiter's 80 a tick: a guard the kiter
                // out-damages, and cannot strike back at range, flees (#533,
                // #535) instead of holding.
                let guard =
                    body
                        "guard-1-Spawn8"
                        Side.Ours
                        (parts [ Tough, 3; Move, 25; Attack, 15; Heal, 7 ])
                        (RoomPos.at "W17S25" { X = 14; Y = 17 })
                        None

                let kiter =
                    body
                        "Kiter1"
                        trep
                        (parts [ Move, 10; RangedAttack, 8; Heal, 2 ])
                        (RoomPos.at "W17S25" { X = 5; Y = 17 })
                        (Some(
                            Phases
                                [
                                    6, Kite(Nearest, 3)
                                    30, GoTo(w18s25 44 17)
                                    200, GoTo(RoomPos.at "W17S25" { X = 3; Y = 17 })
                                ]
                        ))

                // The guard already holds the outpost's Guard (#535): a kiter as
                // quick as it is the ranger row's to answer, but the fight the
                // guard holds is its own while it can strike the kiter.
                let start =
                    let a = westBorder [ guard; kiter ]

                    { a with
                        Carried =
                            { a.Carried with
                                Assignments = Map.ofList [ "guard-1-Spawn8", "guard:W17S25" ]
                            }
                    }

                let _, trace = start |> run 45
                let back = crossingsInto "Kiter1" "W17S25" trace
                Expect.isNonEmpty back $"the kiter came back\n{describe trace}"

                // The guard holds the run (#450): it stands within one of the
                // exit tiles, off the ring, while the kiter is away.
                let away = crossingsInto "Kiter1" "W18S25" trace |> List.head
                let landed = List.head back

                let held =
                    pathOf "guard-1-Spawn8" trace
                    |> List.filter (fun (tick, _) -> tick > away + 3 && tick <= landed)

                for tick, s in held do
                    Expect.equal
                        s.At.X
                        1
                        $"t{tick}: holding beside the west exits\n{describe trace}"

                // Five exit tiles are more than one melee covers: the kiter
                // lands on the run's end, two from the guard, and is hit the
                // tick after it steps in.
                let struck =
                    trace
                    |> List.filter (fun t -> t.Tick > landed && t.Tick <= landed + 2)
                    |> List.exists (fun t ->
                        t.Ours |> List.contains (AttackCreep("guard-1-Spawn8", "Kiter1")))

                Expect.isTrue
                    struck
                    $"the guard hit it within two ticks of t{landed}\n{describe trace}"

                Expect.equal
                    (roomsOf "guard-1-Spawn8" trace)
                    [ "W17S25" ]
                    "and never crossed after it"
            }

            test "a hurt pair retreats across the border and our ranger does not follow" {
                let ranger =
                    body
                        "ranger-880900-Spawn8"
                        Side.Ours
                        (parts [ Move, 24; RangedAttack, 16; Heal, 8 ])
                        (w17s25 16 17)
                        None

                let anchor = w17s25 6 17

                // Legs at the tail, so a hurt body still walks out; each falls
                // back at its first wound and comes back healed.
                let refuge = w18s25 44 17

                let melee =
                    body
                        "Brute1"
                        trep
                        (parts [ Tough, 4; Attack, 6; Move, 10 ])
                        (w17s25 3 16)
                        (Some(Retreat(1900, refuge, 2000, Chase(Nearest, Some(anchor, 49)))))

                let healer =
                    body
                        "Medic1"
                        trep
                        (parts [ Heal, 4; Move, 4 ])
                        (w17s25 2 17)
                        (Some(Retreat(760, refuge, 800, Follow "Brute1")))

                let _, trace = nurseryWest [ ranger; melee; healer ] |> run 30
                Expect.isNonEmpty (crossingsInto "Brute1" "W18S25" trace) "the brute left"

                // Moved by #598: the medic, its HEAL shot off beside its
                // brute, is no longer nobody's and dies short of the exit
                // (it left at 380 hits).
                Expect.isSome (diedOn "Medic1" trace) $"the medic dies\n{describe trace}"

                Expect.isEmpty (crossingsInto "Medic1" "W18S25" trace) "rather than leaving"

                Expect.isEmpty
                    (crossingsInto "ranger-880900-Spawn8" "W18S25" trace)
                    $"the ranger did not follow\n{describe trace}"

                // #598: half the raid going ends nothing for the half
                // still in our room; the ranger fires every tick a raider
                // stands in its reach there.
                let shots =
                    trace
                    |> List.map (fun t ->
                        t.Tick,
                        t.Ours
                        |> List.exists (function
                            | RangedAttackCreep(who, _)
                            | RangedMassAttack who -> who = ranger.Id
                            | _ -> false))
                    |> Map.ofList

                let inReach =
                    trace
                    |> List.choose (fun t ->
                        let at id =
                            t.Bodies |> List.tryFind (fun b -> b.Id = id) |> Option.map _.At

                        let reached raider =
                            match at raider, at ranger.Id with
                            | Some them, Some us ->
                                them.Room = "W17S25"
                                && RoomPos.range them us |> Option.exists (fun r -> r <= 3)
                            | _ -> false

                        if reached "Brute1" || reached "Medic1" then
                            Some(t.Tick + 1)
                        else
                            None)

                for tick in inReach do
                    if Map.containsKey tick shots then
                        Expect.isTrue
                            shots[tick]
                            $"t{tick}: the ranger fires while a raider stands in reach in W17S25\n{describe trace}"
            }

            // As live (t972,617-972,661): the guard kills Wraith403 on its post
            // by t972,620; while Wraith403 stands in W17S26 no resident is lent
            // to Crunchy047 next door (HomeHold, `Facts.homeHoldReleases`);
            // once it is dead one is, and
            // kills Crunchy047 by t972,661. Wraith403 has no weapon left and
            // Crunchy047 steps once in ~17 ticks, so neither reaches the home
            // inside the lend's window (#531).
            test
                "#532 capture t972,617: while Wraith403 stands in W17S26 the residents hold home, then one kills Crunchy047, as live" {
                let _, trace = twoRaidsBesideW17S25 () |> run 80
                let failure = describe trace

                let wraithDead =
                    match diedOn "Wraith403" trace with
                    | Some tick -> tick
                    | None -> failtest $"the guard kills Wraith403\n{failure}"

                Expect.isLessThanOrEqual wraithDead 3 $"on its post, by t972,620\n{failure}"

                Expect.isTrue
                    (trace
                     |> List.exists (fun t ->
                         t.Ours |> List.contains (AttackCreep("guard-972528-Spawn11", "Wraith403"))))
                    "the guard swings at it"

                for ranger in [ "ranger-971130-Spawn3"; "ranger-971410-Spawn3" ] do
                    Expect.equal
                        (pathOf ranger trace
                         |> List.filter (fun (tick, _) -> tick <= wraithDead)
                         |> List.map (fun (_, s) -> s.At.Room)
                         |> List.distinct)
                        [ "W17S25" ]
                        $"{ranger} holds home while Wraith403 lives\n{failure}"

                // Moved 2026-10-06 (#550's ranger row, live t981,500): the
                // other resident is called to W17S26's 23C23M hauler the guard
                // there cannot catch, where it used to stay home because the
                // guard of W17S25's held the kill.
                Expect.equal
                    (roomsOf "ranger-971130-Spawn3" trace)
                    [ "W17S25"; "W17S26" ]
                    $"the other resident is called to W17S26's hauler\n{failure}"

                Expect.isNonEmpty
                    (crossingsInto "ranger-971410-Spawn3" "W18S25" trace)
                    $"the other is lent to W18S25\n{failure}"

                Expect.isSome (diedOn "Crunchy047" trace) $"and kills Crunchy047\n{failure}"
            }

            // The world layer's S1 (#534) flipped the S0 pin here, which read
            // as live (t973,321-973,405): nobody answered the cripple, and the
            // log's last sighting stayed the one that shut it. Now the
            // observer's look reaches the mother's watch of the shut room and
            // W17S25's log: the resident next door is called while the room is
            // still shut, and kills it.
            test
                "#534 capture t973,320: a cripple alone in the child's shut outpost is answered by the mother's resident while the room is shut" {
                let resident = "ranger-972663-Spawn3"

                let rowOf (a: Arena) =
                    Map.tryFind "W17S25" a.Carried.Raids
                    |> Option.bind (fun raids ->
                        raids.Outposts |> List.tryFind (fun row -> row.RoomName = "W18S25"))

                let inside (a: Arena) =
                    a.Bodies |> List.exists (fun b -> b.Id = resident && b.At.Room = "W18S25")

                let entered, toEntry = shutOutpostCripple () |> runUntil inside 85
                let failure = describe toEntry

                Expect.isTrue (inside entered) $"the resident enters W18S25\n{failure}"

                Expect.isTrue
                    (rowOf entered
                     |> Option.exists (fun row -> Fabot.Core.Observe.standingDown entered.Time row))
                    $"while W17S25's stand-down still shuts it\n{failure}"

                Expect.isTrue
                    (rowOf entered |> Option.exists (fun row -> row.LastSeen > 973_265))
                    $"and the log has read the observer's look\n{failure}"

                let _, trace = shutOutpostCripple () |> run 85

                Expect.isSome (diedOn "Idol768" trace) $"Idol768 dies\n{describe trace}"
            }

            // Live (t976,867-976,972) Specter836 dug W17S26 for ~100 ticks while
            // both W17S25 guards stood idle: the exit held for Paragon731, which
            // nobody held, claimed the room ahead of the trespass and pooled no
            // Guard until it ran out at t976,972 (#542). A claim that pools
            // nothing shadows nothing: the kill is pooled at t0.
            test
                "#542 capture t976,955: a held exit nobody holds leaves Specter836 to the trespass's kill at once" {
                let first, opening = diggerBesideHeldExit () |> step

                let holders =
                    first.Carried.Assignments
                    |> Map.filter (fun _ task -> task = "guard:W17S26")
                    |> Map.keys
                    |> List.ofSeq

                Expect.isNonEmpty
                    holders
                    $"a body takes W17S26's Guard at t0\n{describe [ opening ]}"

                let _, trace = diggerBesideHeldExit () |> run 60

                Expect.isSome (diedOn "Specter836" trace) $"and Specter836 dies\n{describe trace}"
            }

            // Live (t979,593-980,050) guard-978780 stood on Gambit229's store
            // ring at (26,7), counted itself arrived, and struck nothing for
            // ~450 ticks (#550). Beside Onyx632, slower than the guard, the
            // kill's ground is beside each trespasser. No ranger holds the
            // kill, so W17S25's guards answer all four, two at most; the mother's
            // resident is lent to her own kill there, its ranger row asked for
            // the haulers, but this arena holds no room between W15S28 and
            // W17S26 for it to walk.
            test
                "#550 capture t979,950: the guard walks off the hauler's store ring and kills the trespassers" {
                // #551's sway kept Onyx632's step every tick with two MOVE more,
                // on a road the arena does not lay; the guard's catch reads
                // plain, where its live 3M5W falls behind.
                let start =
                    let a = swayingTrespassers ()

                    { a with
                        Bodies =
                            a.Bodies
                            |> List.map (fun b ->
                                if b.Id = "Onyx632" then
                                    { b with
                                        Parts = parts [ Move, 3; Work, 5 ]
                                    }
                                else
                                    b)
                    }

                let guard = "guard-978780-Spawn11"

                let briefIn home =
                    let view = viewOf start home
                    let atlas = Fabot.Core.Atlas.ofView view
                    let threats = Fabot.Core.Decide.Threat.threatsOf view atlas
                    view, threats, Fabot.Core.Decide.Threat.Threats.briefOf threats "W17S26"

                let _, threats, brief = briefIn "W17S25"

                Expect.equal
                    (brief.Asks |> List.map (fun ask -> ask.Row, ask.Wanted threats))
                    [ CastRow.Guard, Engine.guardCap; CastRow.Ranger, Engine.guardCap ]
                    "its guards answer all four until a ranger holds the kill, and the two haulers ask the ranger row"

                let mother, threats, brief = briefIn "W15S28"

                Expect.contains
                    (mother.Fighters |> List.filter brief.Admits |> List.map (fun c -> c.Name))
                    "ranger-979811-Spawn8"
                    "the mother's resident may be lent to her kill there"

                Expect.isTrue
                    (brief.Asks
                     |> List.exists (fun ask -> ask.Row = CastRow.Ranger && ask.Wanted threats > 0))
                    "and her ranger row is asked for the haulers"

                let _, trace = start |> run 90
                let failure = describe trace

                Expect.isTrue
                    (trace
                     |> List.take 10
                     |> List.exists (fun t ->
                         t.Ours
                         |> List.exists (function
                             | AttackCreep(name, _) -> name = guard
                             | _ -> false)))
                    $"the guard walks to the trespassers and strikes within ten ticks\n{failure}"

                for id in [ "Onyx632"; "Morgana682" ] do
                    Expect.isSome (diedOn id trace) $"{id} dies\n{failure}"

                // Left with Gambit229 at its pace and Infinity425 crossing, the
                // guard waits on the store ring, where it swings at every draw.
                let container = RoomPos.at "W17S26" { X = 27; Y = 9 }

                Expect.isTrue
                    (pathOf guard trace
                     |> List.last
                     |> fun (_, s) ->
                         RoomPos.range s.At container |> Option.exists (fun r -> r <= 2))
                    $"and then waits for Gambit229 at the container\n{failure}"
            }

            // Live (t974,722-974,950) the anchor stepped onto the invader's
            // tile every tick and only walked once it died. A body we cannot
            // move is ground nobody of ours can take, an ally's as much as an
            // enemy's: Odiodin's creep on the same tile is walked round too.
            test
                "#536 capture t974,800: the anchor walks round a body rooted on its step to its seat, invader or ally" {
                let anchor = "anchor-974661-Spawn3"
                let seat = RoomPos.at "W15S27" { X = 13; Y = 29 }

                let asAlly (a: Arena) =
                    { a with
                        Bodies =
                            a.Bodies
                            |> List.map (fun b ->
                                if b.Id = "invader_W15S27_250" then
                                    { b with
                                        Side = Side.Player "Odiodin"
                                        Script = Some Hold
                                    }
                                else
                                    b)
                    }

                for label, edit in [ "invader", id; "ally", asAlly ] do
                    let _, trace = rootedInvaderOnAnchorStep () |> edit |> run 30
                    let failure = describe trace

                    Expect.isTrue
                        (pathOf anchor trace |> List.exists (fun (_, s) -> s.At = seat))
                        $"beside the {label}, the anchor reaches its seat\n{failure}"
            }

            // Live (t979,951-979,970) the hauler stepped every tick onto the
            // tile a swaying trespasser was entering, and stood. A hostile
            // that stepped since the last tick also stands on the tile it
            // left, its likeliest next one. Characterisation (movement S0):
            // the capture for #551's row (moving blockers); S5 swaps the
            // mechanism (`Left` plus an `A <= 1` surcharge, Decided 4) and
            // must keep this outcome.
            test
                "#551 capture t979,950: the hauler walks round two swaying trespassers to its container" {
                let hauler = "hauler-979756-Spawn11"
                let container = RoomPos.at "W17S26" { X = 27; Y = 9 }
                let _, trace = swayingTrespassers () |> run 20

                Expect.isTrue
                    (pathOf hauler trace
                     |> List.exists (fun (_, s) -> RoomPos.range s.At container = Some 1))
                    $"the hauler reaches its container\n{describe trace}"
            }

            // Movement S3 (`docs/adr/0088`) flipped the S0 pin here. Live
            // (t986,006-986,012) the ranger stepped to range 2 of HalfLife847
            // to shoot, and was struck four times, t3-t6 (4,200 to 2,496);
            // S0 pinned one strike at t6, S2 (whole ticks) at t4. The melee
            // walks its live road tiles (#560). Now the ranger's walk keeps off
            // every tile the melee's map strikes before the ranger, by its own
            // timing, could be on it and gone (`Threats.kiteUnsafeFor`): it
            // holds at (25,7) while the melee is far, then steps and shoots
            // each tick a step ahead of it, never struck, until its life runs
            // out at t11 (as live). The melee dies to ranger-984683 at t36
            // (#595 below).
            test
                "movement S3 capture t986,005: a ranger holding the Guard kites the melee down untouched" {
                let ranger = "ranger-984393-Spawn3"
                let start = rangerIntoMelee ()
                let first, _ = step start

                Expect.equal
                    (Map.tryFind ranger first.Carried.Assignments)
                    (Some "guard:W17S26")
                    "the ranger holds W17S26's Guard"

                let gone (a: Arena) =
                    a.Bodies |> List.forall (fun b -> b.Id <> "HalfLife847")

                let _, trace = start |> runUntil gone 200
                let failure = describe trace
                let lived = pathOf ranger trace

                Expect.isTrue
                    (lived |> List.forall (fun (_, s) -> s.Hits = 4_200))
                    $"the melee never strikes the ranger\n{failure}"

                let melee = pathOf "HalfLife847" trace |> Map.ofList

                Expect.isTrue
                    (lived
                     |> List.filter (fun (tick, _) -> tick >= 3)
                     |> List.forall (fun (tick, _) ->
                         match Map.tryFind (tick - 1) melee, Map.tryFind tick melee with
                         | Some before, Some now -> now.Hits < before.Hits
                         | _ -> false))
                    $"it shoots the melee every tick from t3 while it lives\n{failure}"

                // #595: the mother's ranger-984683 (14 RANGED_ATTACK) enters at
                // t13 by (18,0). Every tile of its kite ground is one the
                // melee strikes before the ranger, twelve tiles off, could be
                // there and gone, so S3's walk found none and it parked on
                // (17,1) until t67 while the melee stood killable at 2,030
                // hits (main: first shot t68, kill t74). Now it closes on the
                // ground by the safe tiles nearest it, waits a step off it for
                // the melee's fatigue, steps in at t23 and shoots from t24.
                let mother = pathOf "ranger-984683-Spawn3" trace

                Expect.isTrue
                    (mother |> List.forall (fun (_, s) -> s.Hits = 4_200))
                    $"the melee never strikes the mother's ranger\n{failure}"

                let firstShot =
                    trace
                    |> List.tryFind (fun t ->
                        t.Ours
                        |> List.contains (RangedAttackCreep("ranger-984683-Spawn3", "HalfLife847")))
                    |> Option.map (fun t -> t.Tick)

                Expect.equal
                    firstShot
                    (Some 24)
                    $"it shoots the melee from t24 (main t68)\n{failure}"

                Expect.equal
                    (diedOn "HalfLife847" trace)
                    (Some 36)
                    $"the melee dies at t36 (S3 t74)\n{failure}"

                // Characterisation (for S4's Flee band, #583). S3 kept
                // anchor-985849 and lost reserver-985957 at t10: the 1-block
                // ranger held (25,10), out of reach of its ground, and the
                // reserver's walk priced round it took the swamp at (25,9).
                // Since #595 that ranger closes on its ground by (26,8), the
                // reserver walks (26,9) on plain and lives (unchanged by #600,
                // which prices a loose body's tile at its step: the ranger
                // swaps onto (26,8) as the reserver steps); the anchor steps to
                // (26,8) at t6 as the melee steps beside it, and dies at t8
                // (as before S3).
                let reserver = pathOf "reserver-985957-Spawn11" trace

                Expect.equal
                    (reserver
                     |> List.tryFind (fun (tick, _) -> tick = 1)
                     |> Option.map (fun (_, s) -> s.At, s.Fatigue))
                    (Some(RoomPos.at "W17S26" { X = 26; Y = 9 }, 0))
                    $"the reserver steps onto the plain round the ranger\n{failure}"

                Expect.isNone (diedOn "reserver-985957-Spawn11" trace) $"and lives\n{failure}"

                Expect.equal
                    (diedOn "anchor-985849-Spawn11" trace)
                    (Some 8)
                    $"the anchor dies at t8\n{failure}"
            }

            // Characterisation (movement S4, #583). The capture does not
            // reproduce live: the Transition log no longer held either body's
            // Task, and live ran an older bundle. Here the reserver holds its
            // Reserve, walks toward the controller, turns at (23,12) the tick
            // the melee's map reaches it and flees south-west, untouched,
            // against the melee on its live tiles and against one chasing the
            // nearest body, on main as under S4. The anchor (6 WORK 1 CARRY 3
            // MOVE: two ticks a road tile, three a plain one) walks to its
            // Post down the road the melee takes, flees ahead of it from t5
            // and is struck at t10, dead by t12: a melee at a tile a tick reaches every
            // tile before it, the exit home (20,0) included, so it has no
            // escape to be given.
            test
                "S4 capture t1,006,904: the reserver flees the melee untouched; the anchor it outpaces has none" {
                let live =
                    Waypoints
                        [
                            for x, y in
                                [
                                    15, 1
                                    16, 1
                                    17, 1
                                    18, 1
                                    19, 1
                                    20, 1
                                    21, 2
                                    22, 3
                                    23, 4
                                    24, 5
                                    25, 6
                                    26, 7
                                    27, 8
                                    26, 9
                                    25, 10
                                    24, 11
                                    23, 12
                                    22, 13
                                    22, 14
                                    21, 15
                                    20, 16
                                ] -> RoomPos.at "W17S26" { X = x; Y = y }
                        ]

                for label, melee in [ "live tiles", live; "chasing", Chase(Nearest, None) ] do
                    let _, trace = trepEntersW17S26 melee |> run 40
                    let failure = describe trace

                    Expect.isTrue
                        (pathOf "reserver-1006558-Spawn11" trace
                         |> List.forall (fun (_, s) -> s.Hits = 400))
                        $"{label}: the melee never strikes the reserver\n{failure}"

                    Expect.isTrue
                        (diedOn "anchor-1005773-Spawn11" trace |> Option.exists (fun t -> t <= 12))
                        $"{label}: the anchor dies by t12\n{failure}"
            }

            // Movement S2 (`docs/adr/0088`) flipped the S0 pin here, which read
            // #527's loop live (t971,046-971,081): hauler-970335-Spawn11 fled a
            // tile back each time the slow melee stepped and rematched the same
            // Refill, standing still between; hauler-969730-Spawn11 fled once
            // and crossed home at t25. Now Virtual458's map waits out its
            // fatigue, so its Reach is its swing alone between steps, and a
            // walk to work across a border goes round what the melee strikes
            // before the walker is gone (`Threats.unsafeFor`): both haulers
            // walk north round it with the Refill held, never flee, and cross
            // home (the first standing still no more: its Work Area across the
            // border was empty, and the walk round asked it for its way).
            test
                "#527 capture t971,045: both haulers walk round the slow melee to the tower and keep its Refill" {
                let hauler = "hauler-970335-Spawn11"
                let walker = "hauler-969730-Spawn11"

                let rows, trace =
                    (slowMeleeHaulers (), [ 0..35 ])
                    ||> List.mapFold (fun a tick ->
                        let next, t = step a
                        let taskOf id = Map.tryFind id next.Carried.Assignments
                        ((tick, taskOf hauler, taskOf walker), t), next)
                    |> fst
                    |> List.unzip

                let failure = describe trace

                for id, held in
                    [
                        hauler, rows |> List.map (fun (_, task, _) -> task)
                        walker, rows |> List.map (fun (_, _, task) -> task)
                    ] do
                    Expect.equal
                        (List.distinct held)
                        [ Some "refill:tower-13-29" ]
                        $"{id} holds the tower's Refill every tick and never flees"

                    Expect.isTrue
                        (pathOf id trace
                         |> List.pairwise
                         |> List.forall (fun ((_, a), (_, b)) -> b.Hits >= a.Hits))
                        $"{id} is never struck\n{failure}"

                let home id =
                    pathOf id trace
                    |> List.tryFind (fun (_, s) -> s.At.Room = "W17S25")
                    |> Option.map fst

                Expect.equal
                    (home hauler)
                    (Some 23)
                    $"the first crosses home round the melee\n{failure}"

                Expect.equal (home walker) (Some 20) $"and so does the other\n{failure}"
            }

            // Movement S0's doorway pin, flipped early (2026-10-07, live
            // t987,159-987,529: guard-986309 parked on the same (1,15)): the
            // tiles beside an exit are idle ground, so an idle body steps off
            // them. Live (t972,062-972,082) the guard stood idle on (1,15) and
            // the hauler behind it was carried back to W18S25 every other
            // tick. S4's stand-tile picker keeps the outcome. The hauler's
            // bounce does not reproduce, and not for want of roads: with them
            // laid (#560) and the guard back on (1,15), it steps in by (1,16)
            // at t0.
            test
                "movement S0 capture t972,061: an idle guard steps off the doorway inside the west exit" {
                let guard = "guard-970892-Spawn11"
                let doorway = RoomPos.at "W17S25" { X = 1; Y = 15 }

                let tasks, trace =
                    (doorwayJam (), [ 0..19 ])
                    ||> List.mapFold (fun a _ ->
                        let next, t = step a
                        (Map.tryFind guard next.Carried.Assignments, t), next)
                    |> fst
                    |> List.unzip

                Expect.isTrue
                    (tasks |> List.forall Option.isNone)
                    $"the guard holds no Task\n{tasks}"

                Expect.isTrue
                    (pathOf guard trace |> List.exists (fun (_, s) -> s.At <> doorway))
                    $"and leaves (1,15)\n{describe trace}"
            }

            // Characterisation (movement S0): pinned how a body
            // released inside a Reach walks out of it, which read no time:
            // its way home to W17S25 runs through (48,14), beside Idol768.
            // #563 made it the #533 acceptance before S2 did, and S2 keeps
            // it: a fighter walking to another room's fight goes round what
            // out-damages it where it stands (movement S2). Live
            // (t973,268-973,294) the ranger stood idle on (46,14) and was
            // struck to death: that was before
            // #533, which gave a released fighter a Task the same tick, so
            // the live outcome no longer reproduces by design. Here it takes
            // the home's Guard at t0 and, standing outside the Guard's room,
            // shoots Idol768 by the weapon reflex from t0 (#563): it dies at
            // t7, before the ranger passes it (struck t8-t10 before #563,
            // when the ranger struck nothing on its way and came back to
            // kill it); the towers kill Shade461 on the exit tile.
            test
                "#533 capture t973,267: a ranger released beside a melee shoots it dead on its way home, and is never struck" {
                let ranger = "ranger-972464-Spawn8"

                let tasks, trace =
                    (idleRangerInReach (), [ 0..29 ])
                    ||> List.mapFold (fun a _ ->
                        let next, t = step a
                        (Map.tryFind ranger next.Carried.Assignments, t), next)
                    |> fst
                    |> List.unzip

                let failure = describe trace

                Expect.isTrue
                    (tasks |> List.forall Option.isSome)
                    $"the ranger is never idle\n{tasks}"

                Expect.equal tasks.Head (Some "guard:W17S25") "its first Task is the home's Guard"

                let struck =
                    pathOf ranger trace
                    |> List.pairwise
                    |> List.choose (fun ((_, before), (tick, now)) ->
                        if now.Hits < before.Hits then Some tick else None)
                    |> List.filter (fun tick -> tick < 12)

                Expect.equal struck [] $"the melee never strikes it on its way out\n{failure}"

                Expect.equal
                    (diedOn "Idol768" trace)
                    (Some 7)
                    $"Idol768 dies of its shots on the way\n{failure}"

                Expect.isNone (diedOn ranger trace) $"and the ranger lives\n{failure}"
            }
        ]

// ---- arena capture synapse-beside-ranger (#535) ----------------------------
// `npm run capture -- W17S26 965400 --rooms W17S25,W15S28 --name synapse`.
// The room-history replay at t965,400: arena tick 0 runs as t965,401.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/965400.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/965400.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/965400.json
// Left out: the Raid log, ArmedSeen, Healers and Sightings start empty; roads,
// a ruin and tombstones are not placed.

/// W17S26 at t965,400 (#535, today's combat stats): Synapse673, 13 RANGED_ATTACK on
/// 7 MOVE, standing still and shooting ranger-965017 beside it, which had its
/// MOVE shot off, 4 RANGED_ATTACK and 7 HEAL left, healed itself and never shot
/// until it died at t965,417; Synapse673 never lost a hit.
let private synapseBeside () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s26 =
        room "W17S26"
        |> withStructures [ structureOf "container" None 90_000 250_000 { X = 27; Y = 9 } ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 39_876 32_600
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 800
        |> withStructures
            [
                for x, y in [ 11, 25; 11, 27; 12, 28; 14, 26; 15, 29; 16, 28; 16, 30 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                structureOf "container" None 66_200 250_000 { X = 16; Y = 34 }
                { structureOf "container" None 125_200 250_000 { X = 17; Y = 20 } with
                    Energy = 532
                }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 12; Y = 30 }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 14; Y = 30 }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 15; Y = 27 } with
                    Energy = 42
                }
                { structureOf "rampart" (Some Side.Ours) 781 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 965_403
                }
                { structureOf "rampart" (Some Side.Ours) 96_901 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 965_486
                }
                { structureOf "rampart" (Some Side.Ours) 35_031 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 965_491
                }
                { structureOf "rampart" (Some Side.Ours) 54_971 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 965_463
                }
                { structureOf "rampart" (Some Side.Ours) 7_731 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 965_465
                }
                { structureOf "rampart" (Some Side.Ours) 14_021 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 965_490
                }
                { structureOf "rampart" (Some Side.Ours) 3_141 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 965_452
                }
                { structureOf "rampart" (Some Side.Ours) 14_601 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 965_469
                }
                { structureOf "rampart" (Some Side.Ours) 1_041 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 965_484
                }
                { structureOf "rampart" (Some Side.Ours) 2_501 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 965_433
                }
                { structureOf "rampart" (Some Side.Ours) 11_701 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 965_468
                }
                { structureOf "rampart" (Some Side.Ours) 6_001 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 965_444
                }
                { structureOf "rampart" (Some Side.Ours) 66_711 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 965_432
                }
                { structureOf "rampart" (Some Side.Ours) 56_301 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 965_489
                }
                { structureOf "rampart" (Some Side.Ours) 99_201 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 965_434
                }
                { structureOf "rampart" (Some Side.Ours) 3_601 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 965_430
                }
                { structureOf "rampart" (Some Side.Ours) 19_161 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 965_443
                }
                { structureOf "rampart" (Some Side.Ours) 1_461 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 965_461
                }
                towerOf Side.Ours { X = 13; Y = 29 } 780
            ]
        |> withSite Side.Ours "road" None 0 { X = 21; Y = 32 }
        |> withSite Side.Ours "road" None 0 { X = 22; Y = 33 }
        |> withSite Side.Ours "road" None 0 { X = 23; Y = 34 }
        |> withSite Side.Ours "road" None 0 { X = 22; Y = 35 }
        |> withSite Side.Ours "road" None 0 { X = 21; Y = 36 }
        |> withSite Side.Ours "road" None 200 { X = 20; Y = 35 }
        |> withSite Side.Ours "road" None 200 { X = 17; Y = 36 }
        |> withSite Side.Ours "road" None 0 { X = 18; Y = 37 }
        |> withSite Side.Ours "road" None 0 { X = 19; Y = 38 }
        |> withSite Side.Ours "road" None 0 { X = 18; Y = 39 }
        |> withSite Side.Ours "road" None 0 { X = 17; Y = 38 }
        |> withSite Side.Ours "road" None 180 { X = 16; Y = 37 }
        |> withSite Side.Ours "road" None 988 { X = 17; Y = 40 }
        |> withSite Side.Ours "road" None 0 { X = 19; Y = 30 }
        |> withSite Side.Ours "road" None 0 { X = 17; Y = 30 }
        |> withSite Side.Ours "road" None 0 { X = 18; Y = 29 }
        |> withSite Side.Ours "storage" None 0 { X = 13; Y = 27 }
        |> withSite Side.Ours "extension" None 0 { X = 13; Y = 25 }
        |> withSite Side.Ours "extension" None 0 { X = 13; Y = 31 }
        |> withSite Side.Ours "extension" None 0 { X = 15; Y = 31 }
        |> withSite Side.Ours "extension" None 0 { X = 17; Y = 29 }
        |> withSite Side.Ours "extension" None 750 { X = 17; Y = 31 }
        |> withSite Side.Ours "extension" None 1_346 { X = 10; Y = 24 }
        |> withSite Side.Ours "extension" None 0 { X = 10; Y = 26 }
        |> withSite Side.Ours "extension" None 0 { X = 10; Y = 28 }
        |> withSite Side.Ours "extension" None 0 { X = 10; Y = 30 }
        |> withSite Side.Ours "extension" None 1_765 { X = 12; Y = 24 }
        |> withSite Side.Ours "rampart" None 0 { X = 2; Y = 16 }
        |> withSite Side.Ours "road" None 0 { X = 10; Y = 29 }
        |> withSite Side.Ours "road" None 0 { X = 11; Y = 30 }
        |> withSite Side.Ours "road" None 0 { X = 10; Y = 27 }

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_645_583
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 25
                        13, 27
                        13, 29
                        13, 31
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 31
                        15, 33
                        15, 35
                        16, 26
                        16, 32
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        18, 34
                        19, 27
                        19, 31
                        19, 33
                        20, 26
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        21, 35
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 12, 30; 14, 26; 14, 30; 15, 27; 15, 29; 16, 30 ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 141_100 250_000 { X = 6; Y = 29 } with
                    Energy = 444
                }
                { structureOf "container" None 142_900 250_000 { X = 11; Y = 18 } with
                    Energy = 228
                }
                structureOf "container" None 148_100 250_000 { X = 23; Y = 29 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 100_401 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 965_414
                }
                { structureOf "rampart" (Some Side.Ours) 102_101 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 965_473
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 965_432
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 965_479
                }
                { structureOf "rampart" (Some Side.Ours) 100_801 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 965_474
                }
                { structureOf "rampart" (Some Side.Ours) 100_301 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 965_428
                }
                { structureOf "rampart" (Some Side.Ours) 101_401 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 965_449
                }
                { structureOf "rampart" (Some Side.Ours) 102_401 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 965_497
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 965_490
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 20_281
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            // live: rangedAttack→37,4 every tick, standing still.
            { body
                  "Synapse673"
                  trepidimous
                  (parts [ Move, 7; RangedAttack, 13 ])
                  (at "W17S26" 36 3)
                  (Some(Do [ Act.RangedAttack "ranger-965017-Spawn3" ])) with
                TicksToLive = 831
            }
            // ranger-965017-Spawn3: live: 4 RANGED_ATTACK, 7 HEAL
            { body
                  "ranger-965017-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S26" 37 4)
                  None with
                Hits = 1_018
                TicksToLive = 1_241
            }
            // The other resident, at W17S25's controller; the economy of both
            // homes is left out: it decides nothing of this fight.
            { body
                  "ranger-964741-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 36)
                  None with
                TicksToLive = 965
            }
        ]

    let a = arena 965_401 [ w17s26; w17s25; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log holds no verdict that early for any body:
                // they start unassigned.
                Assignments = Map.empty
            }
    }

[<Tests>]
let arenaRangerReflexTests =
    testList
        "arena capture synapse-beside-ranger"
        [
            // #535 (combat stats, 2026-10-06): four 4,900-energy rangers died in
            // reach of their killer, healing themselves, never shooting.
            // On main b6c986a1 it never shoots: unassigned (its Task gone) it
            // strikes nothing, the heal reflex alone acting. Now the weapon
            // reflex shoots every tick a RANGED_ATTACK still acts; its last
            // one goes under 700 hits (the seven HEAL at its tail).
            test
                "#535 capture t965,400: a rooted ranger beside Synapse673 shoots it every tick a gun of its still acts" {
                let _, trace = synapseBeside () |> run 12
                let failure = describe trace
                let ranger = "ranger-965017-Spawn3"
                let healTail = 7 * Engine.partHits

                // Each tick with the ranger's hits at its start, the capture's first.
                let armedTicks =
                    trace
                    |> List.scan
                        (fun _ t ->
                            Some t,
                            t.Bodies
                            |> List.tryFind (fun b -> b.Id = ranger)
                            |> Option.map (fun b -> b.Hits)
                            |> Option.defaultValue 0)
                        (None, 1_018)
                    |> List.pairwise
                    |> List.choose (fun ((_, before), (tick, _)) ->
                        tick |> Option.filter (fun _ -> before > healTail))

                Expect.isNonEmpty armedTicks $"the premise: it is armed a while\n{failure}"

                for t in armedTicks do
                    Expect.contains
                        t.Ours
                        (RangedAttackCreep(ranger, "Synapse673"))
                        $"t{t.Tick}: it shoots\n{failure}"
            }
        ]

// `npm run capture -- W17S25 981106 --rooms W18S25,W15S28 --name ranger-at-the-line`, read at live t981,917.
// The room-history replay at t981,106, the state after that tick ran: arena
// tick 0 runs as t981,107 and decides what our code decided live then.
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/981100.json
//   W18S25  https://screeps.com/room-history/shardSeason/W18S25/981100.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/981100.json
// W18S25 is reserved by us to t985515: the arena has no reservations.
// Spawn3 is spawning hauler-980982-Spawn3 until t981126: not placed.
// Spawn8 is spawning brawler-981080-Spawn8 until t981230: not placed.
// not placed: 279 road, 4 tombstone, 1 energy (the arena has no roads, drops or ruins).

/// #552, live t981,106: Trepidimous' 18M17A melee and 11M7H healer bounce on
/// W17S25's west exit tile 0,15, two from the 2,15–2,17 ramparts, draining the
/// tower; W15S28's resident ranger stands by the controller at 18,35 and the
/// defender at 14,23. Both stand on their exit tile, so the border carries
/// them across every tick: the bounce. The tower holds `towerEnergy` (990
/// live).
let private rangerAtTheLine (towerEnergy: int) : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 981_420
        |> withControllerClocks 0 0 40_000 134_101
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in
                    [
                        10, 24
                        10, 26
                        10, 28
                        10, 30
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 28
                        16, 30
                        17, 29
                        17, 31
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                { structureOf "container" None 156_600 250_000 { X = 16; Y = 34 } with
                    Energy = 1_984
                }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 718
                }
                { structureOf "rampart" (Some Side.Ours) 98_221 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 981_144
                }
                { structureOf "rampart" (Some Side.Ours) 100_561 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 981_125
                }
                { structureOf "rampart" (Some Side.Ours) 98_271 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 981_128
                }
                { structureOf "rampart" (Some Side.Ours) 96_331 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 981_133
                }
                { structureOf "rampart" (Some Side.Ours) 97_471 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 981_204
                }
                { structureOf "rampart" (Some Side.Ours) 98_531 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 981_206
                }
                { structureOf "rampart" (Some Side.Ours) 99_821 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 981_132
                }
                { structureOf "rampart" (Some Side.Ours) 99_501 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 981_193
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 981_111
                }
                { structureOf "rampart" (Some Side.Ours) 99_561 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 981_126
                }
                { structureOf "rampart" (Some Side.Ours) 99_761 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 981_174
                }
                { structureOf "rampart" (Some Side.Ours) 97_701 3_000_000 { X = 13; Y = 27 } with
                    NextDecay = 981_125
                }
                { structureOf "rampart" (Some Side.Ours) 58_901 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 981_110
                }
                { structureOf "rampart" (Some Side.Ours) 9_901 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 981_185
                }
                { structureOf "rampart" (Some Side.Ours) 90_321 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 981_173
                }
                { structureOf "rampart" (Some Side.Ours) 76_501 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 981_131
                }
                { structureOf "rampart" (Some Side.Ours) 90_301 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 981_175
                }
                { structureOf "rampart" (Some Side.Ours) 93_701 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 981_171
                }
                { structureOf "rampart" (Some Side.Ours) 97_901 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 981_184
                }
                { structureOf "rampart" (Some Side.Ours) 96_191 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 981_202
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 10_094
                }
                towerOf Side.Ours { X = 13; Y = 29 } towerEnergy
            ]

    let w18s25 =
        room "W18S25"
        |> withStructures
            [
                { structureOf "container" None 120_100 250_000 { X = 31; Y = 5 } with
                    Energy = 347
                }
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 5_859_035
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 30
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 26
                        12, 32
                        12, 34
                        13, 27
                        13, 29
                        13, 33
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        16, 26
                        16, 32
                        17, 25
                        17, 27
                        18, 26
                        18, 28
                        19, 27
                        20, 26
                        20, 28
                        21, 27
                        21, 29
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 36
                        13, 35
                        15, 35
                        16, 34
                        17, 33
                        18, 32
                        18, 34
                        19, 31
                        19, 33
                        20, 30
                        20, 32
                        21, 31
                        21, 33
                        21, 35
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 167_100 250_000 { X = 6; Y = 29 } with
                    Energy = 168
                }
                { structureOf "container" None 130_100 250_000 { X = 11; Y = 18 } with
                    Energy = 672
                }
                structureOf "container" None 132_300 250_000 { X = 23; Y = 29 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 99_101 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 981_155
                }
                { structureOf "rampart" (Some Side.Ours) 101_101 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 981_115
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 981_173
                }
                { structureOf "rampart" (Some Side.Ours) 99_201 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 981_195
                }
                { structureOf "rampart" (Some Side.Ours) 103_001 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 981_116
                }
                { structureOf "rampart" (Some Side.Ours) 94_201 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 981_169
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 981_190
                }
                { structureOf "rampart" (Some Side.Ours) 99_801 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 981_139
                }
                { structureOf "rampart" (Some Side.Ours) 100_201 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 981_132
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 13_905
                    Thorium = 41_977
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 } with
                    Energy = 487
                }
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            { body
                  "hauler-979756-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 10 25)
                  None with
                TicksToLive = 220
                Energy = 248
            }
            { body
                  "upgrader-979829-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 16 34)
                  None with
                TicksToLive = 272
                Energy = 42
            }
            { body
                  "anchor-980086-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 508
                Energy = 50
            }
            { body
                  "hauler-980131-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 17 33)
                  None with
                TicksToLive = 595
                Energy = 550
            }
            { body
                  "worker-980320-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 20 48)
                  None with
                TicksToLive = 772
                Energy = 350
            }
            { body
                  "hauler-980420-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 16 33)
                  None with
                TicksToLive = 884
                Energy = 209
            }
            { body
                  "upgrader-980563-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 1_006
                Energy = 42
            }
            { body
                  "ranger-980660-Spawn8"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 18 35)
                  None with
                TicksToLive = 1_178
            }
            { body
                  "reserver-980840-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W17S25" 2 17)
                  None with
                TicksToLive = 338
            }
            { body
                  "defender-981038-Spawn11"
                  Side.Ours
                  (parts [ Move, 10; Attack, 10 ])
                  (at "W17S25" 14 23)
                  None with
                TicksToLive = 1_490
            }
            // Splinter546: live: 4 MOVE, 17 ATTACK; attacked by the tower.
            { body
                  "Splinter546"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W17S25" 0 15)
                  (Some Hold) with
                Hits = 2_084
                TicksToLive = 1_043
            }
            // Oblivion822: live: crossing to W17S25 0,15; at t981,111 and
            // after, healing itself there on the exit.
            { body
                  "Oblivion822"
                  trepidimous
                  (parts [ Move, 11; Heal, 7 ])
                  (at "W18S25" 49 15)
                  (Some(Do [ Act.Heal "Oblivion822" ])) with
                TicksToLive = 268
            }
            { body
                  "anchor-979625-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 47
                Energy = 50
            }
            { body
                  "anchor-979882-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 298
                Energy = 50
            }
            { body
                  "hauler-980337-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 11 32)
                  None with
                TicksToLive = 873
                Energy = 850
            }
            { body
                  "worker-980588-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 1_130
                Energy = 450
            }
            { body
                  "hauler-980792-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 8 6)
                  None with
                TicksToLive = 1_328
            }
            { body
                  "hauler-980936-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 12 33)
                  None with
                TicksToLive = 1_472
                Energy = 800
            }
        ]

    let a = arena 981_107 [ w17s25; w18s25; w15s28 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before
                // t981,106; it holds none that early for the other bodies.
                Assignments =
                    Map.ofList
                        [
                            "upgrader-980563-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "hauler-980792-Spawn8", "withdraw:6aba1ca3293c6f833d8b43f5"
                        ]
            }
    }

[<Tests>]
let arenaRangerAtTheLineTests =
    testList
        "arena capture ranger-at-the-line"
        [
            // #552, with the live tower (990). Live it held fire after
            // t981,111 and the pair survived: with only the healer in the room
            // no full tick was forced and the light ticks' towers stayed
            // silent, the regression f6875e52 fixed. The arena runs every
            // tick full, so on main 6d2a37d4 as now the tower kills the melee
            // at t12 and the healer at t19, before the resident's 23-tick
            // walk to the line ends.
            test
                "#552 capture t981,106, the live tower: it kills the pair before the resident reaches the line" {
                let _, trace = rangerAtTheLine 990 |> run 30
                let failure = describe trace

                let dies id =
                    trace
                    |> List.tryFind (fun t -> t.Bodies |> List.forall (fun b -> b.Id <> id))
                    |> Option.map (fun t -> t.Tick)

                let reached =
                    trace
                    |> List.tryFind (fun t ->
                        t.Bodies
                        |> List.exists (fun b ->
                            b.Id = "ranger-980660-Spawn8"
                            && b.At.Room = "W17S25"
                            && b.At.X = 2
                            && b.At.Y >= 15
                            && b.At.Y <= 18))
                    |> Option.map (fun t -> t.Tick)

                Expect.isSome (dies "Splinter546") $"the melee dies\n{failure}"
                Expect.isSome (dies "Oblivion822") $"the healer dies\n{failure}"

                Expect.isTrue
                    (match dies "Oblivion822", reached with
                     | Some died, Some came -> died < came
                     | Some _, None -> true
                     | None, _ -> false)
                    $"before the resident stands on the line\n{failure}"
            }

            // #552, the tower dry at start so the pair outlives the walk, as it
            // did live; the haulers' refill brings the tower back.
            //
            // On main 6d2a37d4 the resident thrashed between the line and the
            // controller's ring with the bounce (its kite ground laid only on
            // the ticks the melee stood in the room) and never shot.
            test
                "#552 capture t981,106: the mother's resident takes a rampart within three of the exit tile and shoots the bouncing pair, the defender holding 2,15" {
                let _, trace = rangerAtTheLine 0 |> run 30
                let failure = describe trace
                let ranger = "ranger-980660-Spawn8"
                let defender = "defender-981038-Spawn11"
                let at x y = RoomPos.at "W17S25" (tile x y)
                let exitTile = at 0 15

                // The west line's ramparts, 2,15–2,20 and 2,23–2,27.
                let atLine =
                    [ for y in [ 15..20 ] @ [ 23..27 ] -> at 2 y ]
                    |> List.filter (fun rampart ->
                        RoomPos.range rampart exitTile
                        |> Option.exists (fun r -> r <= Engine.rangedRange))
                    |> Set.ofList

                let standing id (t: TickTrace) =
                    t.Bodies |> List.tryFind (fun b -> b.Id = id) |> Option.map (fun b -> b.At)

                let shots =
                    trace
                    |> List.filter (fun t ->
                        t.Ours
                        |> List.exists (function
                            | RangedAttackCreep(name, target) ->
                                name = ranger && (target = "Splinter546" || target = "Oblivion822")
                            | _ -> false))

                Expect.isNonEmpty shots $"the ranger shoots the pair\n{failure}"

                for t in shots do
                    Expect.isTrue
                        (standing ranger t |> Option.exists (fun at -> Set.contains at atLine))
                        $"t{t.Tick}: from a rampart within three of the exit tile\n{failure}"

                let holds = trace |> List.skipWhile (fun t -> standing defender t <> Some(at 2 15))

                Expect.isNonEmpty holds $"the defender takes 2,15\n{failure}"

                Expect.isTrue
                    (holds |> List.forall (fun t -> standing defender t = Some(at 2 15)))
                    $"and holds it\n{failure}"
            }
        ]

/// W17S25's declared perimeter (#446): sixteen ramparts sealing the room at
/// its exits' chokes — eleven west, three south, two east.
let private perimeter =
    [ for y in 15..20 -> tile 2 y ]
    @ [ for y in 23..27 -> tile 2 y ]
    @ [ tile 16 44; tile 17 44; tile 18 44; tile 47 27; tile 47 28 ]

let private childSpawn = tile 22 34
let private childTower = tile 21 32

/// W17S25 raised by W17S26 and weaning at RCL3: its own spawn, one tower
/// holding `towerEnergy` — never full, so the child stays raised (#445) —
/// the perimeter's ramparts at `hits`, and W18S25 beyond the west exits.
/// Nothing refills the tower.
let private sealedChild (hits: int) (towerEnergy: int) (safeModes: int) (bodies: Body list) =
    let mother =
        room "W17S26"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn8" { X = 20; Y = 26 } 5600

    let child =
        room "W17S25"
        |> withController Ownership.Ours None 3 safeModes
        |> withSpawn "Spawn10" childSpawn 800
        |> withStructures [ towerOf Side.Ours childTower towerEnergy ]
        |> withRamparts Side.Ours hits perimeter

    let colonies =
        [
            colony "W17S26"
            { colony "W17S25" with
                Mother = Some "W17S26"
                Perimeter = perimeter
            }
        ]

    arena 880_341 [ mother; child; room "W18S25" ] colonies bodies

/// W17S25 raised by W17S26 and bootstrapping at RCL2: its own spawn, no
/// tower, no perimeter yet, `safeModes` banked.
let private bootstrappingChild (safeModes: int) (bodies: Body list) =
    let mother =
        room "W17S26"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn8" { X = 20; Y = 26 } 5600

    let child =
        room "W17S25"
        |> withController Ownership.Ours None 2 safeModes
        |> withSpawn "Spawn10" childSpawn 550

    let colonies =
        [
            colony "W17S26"
            { colony "W17S25" with
                Mother = Some "W17S26"
                Perimeter = perimeter
            }
        ]

    arena 880_341 [ mother; child; room "W18S25" ] colonies bodies

/// The t880,341 raid (§1.1) come back from W18S25, beyond the west exits:
/// the melee breaking in for the controller's ring, each healer behind its
/// melee, the tapper walking for the controller once a way is open.
let private westRaid =
    [
        body "Eternity536" trep trepMelee (w18s25 44 22) (Some(Breach(w17s25 16 36)))
        body "Prime803" trep trepMelee (w18s25 44 24) (Some(Breach(w17s25 16 36)))
        body "Prism305" trep trepHealer (w18s25 43 22) (Some(Follow "Eternity536"))
        body "Paragon722" trep trepHealer (w18s25 43 24) (Some(Follow "Prime803"))
        body "Rune908" trep trepTapper (w18s25 45 23) (Some(Tap(w17s25 15 36)))
    ]

/// The perimeter ramparts' ids.
let private lineIds =
    perimeter |> List.map (fun p -> $"rampart-{p.X}-{p.Y}") |> Set.ofList

/// The tick the first of these structures fell on.
let private firstFallen (ids: Set<string>) (trace: TickTrace list) : int option =
    trace
    |> List.tryPick (fun t ->
        t.Events
        |> List.tryPick (function
            | Destroyed id when Set.contains id ids -> Some t.Tick
            | _ -> None))

/// The tick safe mode fired on, if it did.
let private safeModeOn (trace: TickTrace list) : int option =
    trace
    |> List.tryPick (fun t ->
        t.Events
        |> List.tryPick (function
            | SafeModeActivated _ -> Some t.Tick
            | _ -> None))

/// The t880,341 raid already inside the line and beside the child's spawn,
/// the melee breaking it: no tapper.
let private insideRaid =
    let spawn = RoomPos.at "W17S25" childSpawn

    [
        body "Eternity536" trep trepMelee (w17s25 23 35) (Some(Breach spawn))
        body "Prime803" trep trepMelee (w17s25 22 35) (Some(Breach spawn))
        body "Prism305" trep trepHealer (w17s25 23 36) (Some(Follow "Eternity536"))
        body "Paragon722" trep trepHealer (w17s25 22 36) (Some(Follow "Prime803"))
    ]

/// W18S26 as captured at t889,849 (#465): Trepidimous' RCL6, its two towers
/// full, healing their own first, then shooting the nearest, then mending
/// their ramparts. Nothing refills them from the 86,308 in its Storage.
let private trepBase () =
    room "W18S26" |> withBase [ HealHurt; Shoot Nearest; Mend 2_000_000 ]

/// W18S26's outer line on the side facing W17S26: the 45 ramparts at x 30.
let private outerLine =
    trepBase().Structures
    |> List.filter (fun s -> s.Kind = "rampart" && s.At.X = 30)
    |> List.map (fun s -> s.Id)
    |> Set.ofList

/// A scripted siege from W17S26, our decide off: two strikers breaking for
/// the base's south-east corner (25,44), so the cheapest way in crosses the
/// outer line where both towers are past their falloff range, each with a
/// 16 HEAL healer behind it. Run until the line falls, every body is dead,
/// or a life (1,500 ticks) is spent.
let private offenceProbe (striker: BodyPart list) =
    let w17s26 x y = RoomPos.at "W17S26" { X = x; Y = y }
    let goal = RoomPos.at "W18S26" { X = 25; Y = 44 }
    let us = Side.Player "fabot"
    let healer = parts [ Move, 16; Heal, 16 ]

    let start =
        arena
            889_849
            [ room "W17S26"; trepBase () ]
            []
            [
                body "striker-1" us striker (w17s26 4 8) (Some(Breach goal))
                body "striker-2" us striker (w17s26 4 10) (Some(Breach goal))
                body "healer-1" us healer (w17s26 3 8) (Some(Follow "striker-1"))
                body "healer-2" us healer (w17s26 3 10) (Some(Follow "striker-2"))
            ]

    let over (a: Arena) =
        let standing =
            a.Rooms["W18S26"].Structures
            |> List.filter (fun s -> Set.contains s.Id outerLine)
            |> List.length

        List.isEmpty a.Bodies || standing < outerLine.Count

    let final, trace = start |> runUntil over Engine.creepLifetime
    start, final, trace

/// The rampart on W18S26's far line at x30 y44 (boosts.md §4.2).
let private farBreach =
    trepBase().Structures
    |> List.find (fun s -> s.Kind = "rampart" && s.At = { X = 30; Y = 44 })

/// #490's declaration: the default squad's Provoke on the far line.
let private farLine: Assault =
    { Assault.w18s26 with
        Breach = [ farBreach.At ]
        Squad = Assault.breachers
        Entry = None
        Active = true
    }

/// Our decide this time (#490): W17S26 an RCL7 colony of ours sending the
/// default squad against W18S26's far line, a Provoke, its four casts
/// standing apart in W17S26 as the probe's do. Run until the breach's rampart
/// falls, every body of ours is dead, or `ticks` are spent.
let private assaultProbe (ticks: int) =
    let w17s26 x y = RoomPos.at "W17S26" { X = x; Y = y }

    let mother =
        room "W17S26"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn8" { X = 20; Y = 26 } 5600

    let colonies =
        [
            { colony "W17S26" with
                Assaults = [ farLine ]
            }
        ]

    let cast row (block: BodyPart list) n at =
        body $"{row}-889000{n}-Spawn8" Side.Ours block at None

    let squad =
        [
            cast "sapper" Fabot.Core.Decide.Bodies.sapperPattern.Block 1 (w17s26 4 8)
            cast "sapper" Fabot.Core.Decide.Bodies.sapperPattern.Block 2 (w17s26 4 10)
            cast "medic" Fabot.Core.Decide.Bodies.medicPattern.Block 3 (w17s26 3 8)
            cast "medic" Fabot.Core.Decide.Bodies.medicPattern.Block 4 (w17s26 3 10)
        ]

    let start = arena 889_849 [ mother; trepBase () ] colonies squad

    let over (a: Arena) =
        let ours = a.Bodies |> List.filter (fun b -> b.Side = Side.Ours)

        List.isEmpty ours
        || not (a.Rooms["W18S26"].Structures |> List.exists (fun s -> s.Id = farBreach.Id))

    let final, trace = start |> runUntil over ticks
    start, final, trace

/// W17S24 as captured at t926,538 (RCL4, 3 safe modes): its west line at x2
/// y18–21, one tower at 17,33, one spawn at 23,33.
let private w17s24Base () =
    room "W17S24" |> withBase [ HealHurt; Shoot Nearest; Mend 2_000_000 ]

let private w17s24Tower = { X = 17; Y = 33 }
let private w17s24Spawn = { X = 23; Y = 33 }

/// The bait (#491): Trepidimous raises safe mode in a struck room whenever
/// the engine lets them. #490's Provoke squad from W17S26 strikes W18S26's
/// far line; W18S25, an RCL7 colony of ours with the same squad standing at
/// home, declares a Strike on W17S24's west line, its controller remembered
/// as seen at the start; W18S27 stands south of W18S26, a way out the
/// Provoke's fall-back may weigh (#492). Run until W17S24's tower and spawn
/// are down, every body of ours is dead, or `ticks` are spent.
let private baitProbe (ticks: int) =
    let time = 889_849
    let w17s26 x y = RoomPos.at "W17S26" { X = x; Y = y }

    let provoker =
        room "W17S26"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn8" { X = 20; Y = 26 } 5600

    let striker =
        room "W18S25"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn9" { X = 16; Y = 20 } 5600

    let colonies =
        [
            { colony "W17S26" with
                Assaults = [ farLine ]
            }
            // The shipped declaration, switched on.
            { colony "W18S25" with
                Assaults = [ { Assault.w17s24 with Active = true } ]
            }
        ]

    let w18s25 x y = RoomPos.at "W18S25" { X = x; Y = y }

    let cast spawn row (block: BodyPart list) n at =
        body $"{row}-889000{n}-{spawn}" Side.Ours block at None

    // Each colony's squad standing at home: the arena's spawns cast no
    // economy, and a colony's cascade buys that first.
    let squadOf spawn (at: int -> int -> RoomPos) (tiles: (int * int) list) =
        List.zip3
            [ "sapper"; "sapper"; "medic"; "medic" ]
            [
                Fabot.Core.Decide.Bodies.sapperPattern.Block
                Fabot.Core.Decide.Bodies.sapperPattern.Block
                Fabot.Core.Decide.Bodies.medicPattern.Block
                Fabot.Core.Decide.Bodies.medicPattern.Block
            ]
            tiles
        |> List.mapi (fun i (row, block, (x, y)) -> cast spawn row block (i + 1) (at x y))

    let squad =
        squadOf "Spawn8" w17s26 [ 4, 8; 4, 10; 3, 8; 3, 10 ]
        @ squadOf "Spawn9" w18s25 [ 12, 27; 12, 28; 11, 27; 11, 28 ]

    let target = w17s24Base () |> panicking

    let seen =
        let c = target.Controller.Value

        {
            Owner = "Trepidimous"
            Level = c.Level
            SafeModeUntil = 0
            SafeModeCooldownUntil = 0
            SafeModeAvailable = c.SafeModeAvailable
            UpgradeBlockedUntil = 0
            TicksToDowngrade = c.TicksToDowngrade
            Seen = time
        }

    let start =
        arena
            time
            [
                provoker
                trepBase () |> panicking
                room "W17S25"
                striker
                room "W18S24"
                target
                room "W18S27"
            ]
            colonies
            squad
        |> fun a ->
            { a with
                Carried =
                    { a.Carried with
                        RivalControllers = Map.ofList [ "W17S24", seen ]
                    }
            }

    let standing (a: Arena) at =
        a.Rooms["W17S24"].Structures
        |> List.exists (fun s -> s.At = at && (s.Kind = "tower" || s.Kind = "spawn"))

    let over (a: Arena) =
        let ours = a.Bodies |> List.filter (fun b -> b.Side = Side.Ours)

        List.isEmpty ours || not (standing a w17s24Tower || standing a w17s24Spawn)

    let final, trace = start |> runUntil over ticks
    start, final, trace

/// The rampart on W18S26's west line the shipped probe breaks (#493).
let private westBreach =
    trepBase().Structures
    |> List.find (fun s -> s.Kind = "rampart" && s.At = List.head Assault.w18s26.Breach)

/// The default squad on the west line (#493): W19S26 an RCL7 colony of ours
/// beside it for the arena, its four casts standing at home — the arena's
/// spawns cast no economy — on the shipped Provoke with the default squad,
/// Trepidimous raising safe mode wherever struck or not. Run until the
/// breach falls, every body of ours is dead, or `ticks` are spent.
let private westProbe (panics: bool) (ticks: int) =
    let w19s26 x y = RoomPos.at "W19S26" { X = x; Y = y }

    let mother =
        room "W19S26"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn8" { X = 38; Y = 3 } 5600

    let colonies =
        [
            { colony "W19S26" with
                Assaults =
                    [
                        { Assault.w18s26 with
                            Squad = Assault.breachers
                            Active = true
                        }
                    ]
            }
        ]

    let squad =
        [
            "sapper", Fabot.Core.Decide.Bodies.sapperPattern.Block, (43, 11)
            "sapper", Fabot.Core.Decide.Bodies.sapperPattern.Block, (44, 11)
            "medic", Fabot.Core.Decide.Bodies.medicPattern.Block, (43, 12)
            "medic", Fabot.Core.Decide.Bodies.medicPattern.Block, (44, 12)
        ]
        |> List.mapi (fun i (row, block, (x, y)) ->
            body $"{row}-889000{i + 1}-Spawn8" Side.Ours block (w19s26 x y) None)

    let target = if panics then trepBase () |> panicking else trepBase ()

    let start =
        arena 889_849 [ mother; target; room "W18S25"; room "W18S27" ] colonies squad

    let over (a: Arena) =
        let ours = a.Bodies |> List.filter (fun b -> b.Side = Side.Ours)

        List.isEmpty ours
        || not (a.Rooms["W18S26"].Structures |> List.exists (fun s -> s.Id = westBreach.Id))

    let final, trace = start |> runUntil over ticks
    start, final, trace

/// The shipped probe (#493): W17S29 an RCL6 colony as it is live, declaring
/// `Assault.w18s26` as shipped, its probe standing at home; every room of the
/// walk loaded, and Trepidimous raising safe mode wherever struck. Run until
/// the probe is dead or `ticks` are spent.
let private probeProbe (ticks: int) =
    let mother =
        room "W17S29"
        |> withController Ownership.Ours None 6 0
        |> withSpawn "Spawn9" { X = 24; Y = 40 } 2300

    let colonies =
        [
            { colony "W17S29" with
                Assaults = [ { Assault.w18s26 with Active = true } ]
            }
        ]

    let probe =
        body
            "probe-8890001-Spawn9"
            Side.Ours
            Fabot.Core.Decide.Bodies.probePattern.Block
            (RoomPos.at "W17S29" { X = 25; Y = 38 })
            None

    // The rectangle from home to the entry, and the rooms beside the target.
    let between =
        [
            for x in 17..19 do
                for y in 26..29 -> $"W{x}S{y}"
        ]
        |> List.filter (fun name -> name <> "W17S29" && name <> "W18S26")
        |> List.map room

    let start =
        arena
            889_849
            ([ mother; trepBase () |> panicking; room "W18S25" ] @ between)
            colonies
            [ probe ]

    let over (a: Arena) =
        a.Bodies |> List.exists (fun b -> b.Side = Side.Ours) |> not

    let final, trace = start |> runUntil over ticks
    start, final, trace

/// The link inside W18S26's far line (#496), fed by the source at (23,43).
let private farLink =
    trepBase().Structures
    |> List.find (fun s -> s.Kind = "link" && s.At = List.head Assault.w18s26Link.Targets)

/// The rampart on W18S26's south line the link raid breaks (#494).
let private southBreach =
    trepBase().Structures
    |> List.find (fun s -> s.Kind = "rampart" && s.At = List.head Assault.w18s26Link.Breach)

/// W15S28 as it stands live at t929,344: RCL7, Spawn3 and Spawn8, the
/// storage, terminal, three towers and fifty extensions on their tiles.
let private w15s28Home () =
    let ours kind (x, y) =
        structureOf kind (Some Side.Ours) 1000 1000 { X = x; Y = y }

    let extensions =
        [
            19, 31
            18, 28
            16, 32
            16, 30
            18, 32
            15, 29
            15, 27
            20, 32
            20, 28
            20, 30
            21, 27
            15, 31
            19, 27
            21, 29
            15, 33
            17, 27
            17, 33
            21, 31
            19, 33
            21, 33
            14, 30
            20, 26
            18, 26
            14, 28
            16, 26
            14, 26
            14, 32
            14, 34
            18, 34
            16, 34
            22, 26
            17, 25
            15, 35
            15, 25
            13, 29
            13, 33
            13, 35
            13, 31
            13, 27
            13, 25
            23, 27
            12, 30
            23, 25
            23, 35
            21, 35
            12, 36
            12, 34
            12, 32
            12, 26
            12, 24
        ]

    room "W15S28"
    |> withController Ownership.Ours None 7 0
    |> withSpawn "Spawn3" { X = 18; Y = 30 } 12_900
    |> withSpawn "Spawn8" { X = 17; Y = 35 } 12_900
    |> withStructures (
        List.map (ours "extension") extensions
        @ [ ours "storage" (17, 29); ours "terminal" (16, 28) ]
        @ ([ 17, 31; 19, 29; 20, 34 ]
           |> List.map (fun (x, y) -> towerOf Side.Ours { X = x; Y = y } 1000))
    )

/// Every room the link raid's walk from W15S28 may cross, and the rooms
/// beside W18S26 a fall-back may leave by: W15–W18 by S25–S29, W19S26.
let private linkWalk () =
    [
        for x in 15..18 do
            for y in 25..29 -> $"W{x}S{y}"
        yield "W19S26"
    ]
    |> List.filter (fun name -> name <> "W15S28" && name <> "W18S26")
    |> List.map room

/// The shipped link assault (#496, from the south #494), from its caster:
/// W15S28 as it stands live, its four casts — sapper, sapper, medic, medic —
/// standing on `tiles` — `holding` the Assault already, as a squad on its
/// walk does — every room of the walk loaded; Trepidimous raising safe mode
/// wherever struck or not. Run until the link is down and no body of ours is
/// in W18S26, every body of ours is dead, or `ticks` are spent.
let private linkProbeFrom (tiles: RoomPos list) (holding: bool) (panics: bool) (ticks: int) =
    let colonies =
        [
            { colony "W15S28" with
                Assaults =
                    [
                        { Assault.w18s26Link with
                            Active = true
                        }
                    ]
            }
        ]

    let squad =
        [
            "sapper", Fabot.Core.Decide.Bodies.sapperPattern.Block
            "sapper", Fabot.Core.Decide.Bodies.sapperPattern.Block
            "medic", Fabot.Core.Decide.Bodies.medicPattern.Block
            "medic", Fabot.Core.Decide.Bodies.medicPattern.Block
        ]
        |> List.zip tiles
        |> List.mapi (fun i (at, (row, block)) ->
            body $"{row}-889000{i + 1}-Spawn3" Side.Ours block at None)

    // Its towers mend nothing: nothing refills them here, and mending
    // through the walk from home would empty them before the squad came.
    let target =
        trepBase ()
        |> withTowerDuties [ HealHurt; Shoot Nearest ]
        |> fun r -> if panics then panicking r else r

    let start =
        arena 889_849 ([ w15s28Home (); target ] @ linkWalk ()) colonies squad
        |> fun a ->
            if holding then
                { a with
                    Carried =
                        { a.Carried with
                            Assignments =
                                squad
                                |> List.map (fun b ->
                                    b.Id, Fabot.Core.Decide.Facts.taskId (Assault "W18S26"))
                                |> Map.ofList
                        }
                }
            else
                a

    let over (a: Arena) =
        let ours = a.Bodies |> List.filter (fun b -> b.Side = Side.Ours)

        let ended =
            not (a.Rooms["W18S26"].Structures |> List.exists (fun s -> s.Id = farLink.Id))
            || (a.Rooms["W18S26"].Controller
                |> Option.exists (fun c -> c.SafeModeUntil > a.Time))

        List.isEmpty ours
        || ended && ours |> List.forall (fun b -> b.At.Room <> "W18S26")

    let final, trace = start |> runUntil over ticks
    start, final, trace

/// The link raid from its four casts' tiles south-east of W15S28's spawns.
let private linkProbe (panics: bool) (ticks: int) =
    let tiles =
        [ 30, 38; 31, 38; 30, 39; 31, 39 ]
        |> List.map (fun (x, y) -> RoomPos.at "W15S28" { X = x; Y = y })

    linkProbeFrom tiles false panics ticks

/// The link raid's casts on W18S26's south corridor, the one row between its
/// south line and the exit, sappers first, at these x, holding the Assault.
let private inCorridor (xs: int list) (panics: bool) (ticks: int) =
    let tiles = xs |> List.map (fun x -> RoomPos.at "W18S26" { X = x; Y = 48 })
    linkProbeFrom tiles true panics ticks

/// Odiodin's garrison outside W17S25's west line, walking in for the room's
/// west end (#482).
let private passingGarrison =
    [
        body "Odio1" odiodin garrison (w17s25 1 17) (Some(GoTo(w17s25 10 17)))
        body "Odio2" odiodin garrison (w17s25 1 18) (Some(GoTo(w17s25 10 18)))
    ]

/// Every `setPublic` that landed, by tick, rampart and the state it set.
let private flips (trace: TickTrace list) =
    trace
    |> List.collect (fun t ->
        t.Events
        |> List.choose (function
            | MadePublic(id, isPublic) -> Some(t.Tick, id, isPublic)
            | _ -> None))

/// The child's one worker, carrying a full 200.
let private worker =
    { body
          "worker-880300-Spawn10"
          Side.Ours
          (parts [ Work, 4; Carry, 4; Move, 4 ])
          (w17s25 20 30)
          None with
        Energy = 200
    }

/// The #447 garrison: the mother's two resident R7s by the controller.
let private residents =
    [
        body "ranger-879853-Spawn8" Side.Ours r7 (w17s25 20 38) None
        body "ranger-879854-Spawn8" Side.Ours r7 (w17s25 19 39) None
    ]

/// W17S25 as an RCL4 home of its own (W12S26's profile, the Meow doctrine
/// §A): one tower, dry, the perimeter's ramparts at 300,000, a 1,300 bank,
/// and MeowKittyWow's 4M2R1H shooter holding at range three of the south
/// line, where no swing reaches it (#546).
let private kitedHome () =
    let home =
        room "W17S25"
        |> withController Ownership.Ours None 4 0
        |> withSpawn "Spawn10" childSpawn 1300
        |> withStructures [ towerOf Side.Ours childTower 0 ]
        |> withRamparts Side.Ours 300_000 perimeter

    let shooter =
        body
            "Meow"
            (Side.Player "MeowKittyWow")
            (parts [ Move, 4; RangedAttack, 2; Heal, 1 ])
            (w17s25 17 47)
            (Some Hold)

    arena
        880_341
        [ home; room "W18S25" ]
        [
            { colony "W17S25" with
                Perimeter = perimeter
            }
        ]
        [ worker; shooter ]
    |> withCasts [ "ranger"; "defender" ]

[<Tests>]
let arenaDefenceTests =
    testList
        "arena siege defence"
        [
            test
                "our structures reach the views as the shell projects them: the Keep, our ramparts, the loaded tower" {
                let start = sealedChild 50_000 500 1 residents
                let child = viewOf start "W17S25"
                let mother = viewOf start "W17S26"
                let atlas = Fabot.Core.Atlas.ofView child

                Expect.equal
                    (Fabot.Core.Atlas.ourRampartTilesIn atlas "W17S25")
                    (Set.ofList perimeter)
                    "the sixteen ramparts are ours, by their hits"

                Expect.equal
                    (Fabot.Core.Atlas.keepTilesIn atlas "W17S25")
                    (Set.ofList [ childSpawn; childTower ])
                    "the spawn and the tower are the Keep"

                Expect.equal
                    (Map.tryFind "W17S25" mother.World.Rooms
                     |> Option.map (fun seen -> seen.OurTowers))
                    (Some [ childTower ])
                    "the mother sees the loaded tower"

                Expect.equal
                    (Map.tryFind "W17S25" child.Stages)
                    (Some Weaning)
                    "the child is weaning"
            }

            test
                "a worker of the child by its perimeter repairs a rampart under the floor through our decide, 400 hits for 4 energy a tick" {
                // The tower full, so nothing outranks the Repair: the child
                // stands on its own (#445) and keeps its ramparts at
                // `RampartFloor`.
                let start = sealedChild 10_000 1000 0 [ { worker with At = w17s25 3 18 } ]
                let final, trace = start |> run 10
                let failure = describe trace

                let mended =
                    trace
                    |> List.choose (fun t ->
                        t.Ours
                        |> List.tryPick (function
                            | RepairStructure(n, id) when n = worker.Id -> Some id
                            | _ -> None))
                    |> List.distinct

                match mended with
                | [ id ] ->
                    Expect.isTrue (Set.contains id lineIds) $"a perimeter rampart\n{failure}"

                    Expect.equal
                        (hitsLeft id trace)
                        (Some(10_000 + 10 * 400))
                        "four WORK, ten ticks"
                | ids -> failtest $"one rampart repaired every tick: {ids}\n{failure}"

                Expect.equal
                    (final.Bodies |> List.find (fun b -> b.Id = worker.Id)).Energy
                    (200 - 10 * 4)
                    "four energy a tick"
            }

            test
                "a worker of the child repairs the rampart a melee is breaking, over its floor, before the empty tower" {
                // #467: in scenario 1 the worker poured its 200 into the
                // tower; Repair was surplus work below a tower's Refill, and
                // a weaning child keeps no rampart floor at all.
                let melee =
                    body "Eternity536" trep trepMelee (w17s25 1 24) (Some(Breach(w17s25 16 36)))

                let start = sealedChild 300_000 0 0 [ worker; melee ]
                let final, trace = start |> run 40
                let failure = describe trace

                let spent =
                    trace
                    |> List.collect (fun t ->
                        t.Ours
                        |> List.choose (function
                            | RepairStructure(n, id) when n = worker.Id -> Some(Choice1Of2 id)
                            | TransferEnergyToStructure(n, id, _) when n = worker.Id ->
                                Some(Choice2Of2 id)
                            | _ -> None))

                Expect.isNonEmpty spent $"the worker spends its load\n{failure}"

                Expect.all
                    spent
                    (function
                    | Choice1Of2 id -> Set.contains id lineIds
                    | Choice2Of2 _ -> false)
                    $"on the struck line and never the tower: {spent}\n{failure}"

                Expect.isLessThan
                    (final.Bodies |> List.find (fun b -> b.Id = worker.Id)).Energy
                    worker.Energy
                    "and the load goes into it"
            }

            for hits, holds in [ 10_000, false; 50_000, true; 300_000, true ] do
                test
                    $"scenario 1 (#446, #467): the perimeter at {hits} hits against the t880,341 raid, the garrison shooting from its ramparts untouched" {
                    let start = sealedChild hits 500 0 (worker :: residents @ westRaid)
                    let raid = [ "Eternity536"; "Prime803"; "Prism305"; "Paragon722" ]

                    let standing (a: Arena) =
                        a.Rooms["W17S25"].Structures
                        |> List.filter (fun s -> Set.contains s.Id lineIds)

                    let broken (a: Arena) =
                        raid
                        |> List.forall (fun id -> a.Bodies |> List.forall (fun b -> b.Id <> id))

                    let final, trace =
                        start |> runUntil (fun a -> List.length (standing a) < 16 || broken a) 700

                    let failure = describe trace
                    let swing = Engine.attackPower * 17

                    let shots =
                        trace
                        |> List.sumBy (fun t ->
                            t.Ours
                            |> List.filter (function
                                | FireTower _ -> true
                                | _ -> false)
                            |> List.length)

                    let towerLeft =
                        final.Rooms["W17S25"].Structures
                        |> List.filter (fun s -> s.Kind = "tower")
                        |> List.sumBy (fun s -> s.Energy)

                    // #466, measured: 6 shots over the 38 ticks to the
                    // 10,000 line's breach and 7 over the 77 the thicker
                    // lines' raid lives, every one at a body the energy over
                    // the reserve can kill through the heal on it; on the
                    // ticks between the tower repairs the struck rampart
                    // (#477), and holds 380 and 300. Taking every shot that
                    // merely out-healed (21 and 40) broke the 10,000 line at
                    // t25 instead of t37.
                    Expect.isLessThanOrEqual
                        shots
                        (List.length trace * 3 / 4)
                        $"a shot only where it out-damages the heal\n{failure}"

                    Expect.isGreaterThan towerLeft 0 $"the tower holds energy at the end\n{failure}"

                    match firstFallen lineIds trace, holds with
                    | Some fell, false ->
                        // Measured: t37, one melee on one rampart from the
                        // walk in, before the garrison has walked to the
                        // line from the controller (#446 measured t105 and
                        // t593 at the two thicker lines, unshot).
                        Expect.isGreaterThanOrEqual
                            fell
                            (hits / (2 * swing))
                            "no faster than both melee on one rampart"

                        Expect.isLessThanOrEqual
                            fell
                            (hits / swing + 30)
                            $"one melee on it from the walk in\n{failure}"
                    | None, true ->
                        // Measured: the raid dead by t76 at both thicker lines.
                        for id in raid do
                            Expect.isSome (diedOn id trace) $"{id} dies at the line\n{failure}"

                        let onRamparts (r: Body) =
                            pathOf r.Id trace
                            |> List.exists (fun (_, s) ->
                                List.contains (RoomPos.pos s.At) perimeter)

                        Expect.all
                            residents
                            onRamparts
                            $"the garrison shot from the ramparts\n{failure}"
                    | outcome -> failtest $"the line holds: {holds}, fell: {outcome}\n{failure}"

                    for r in residents do
                        for tick, s in pathOf r.Id trace do
                            Expect.equal s.Hits 4200 $"t{tick}: {r.Id} untouched\n{failure}"

                            Expect.isGreaterThanOrEqual
                                s.At.X
                                2
                                $"t{tick}: {r.Id} on the line or inside it"
                }

            test
                "scenario 2 (#448): the raid inside a towered home at its bare spawn fires safe mode on the first tick, and nothing lands after" {
                // The tower takes the undefended arm away, and no tapper
                // comes. Two 17-ATTACK melee beside the bare spawn, the
                // home's only one, raze it in five ticks; the tower and the
                // residents kill the first only at t10. The spawn is lost
                // before the defence stops the raid, so the economy arm
                // (#545) fires on the first tick, before the Keep arm's dent
                // (the Keep arm's own case is the safe-mode reflex's "a dented
                // Keep with a hostile in the room fires").
                let _, trace = sealedChild 50_000 500 1 (residents @ insideRaid) |> run 80
                let failure = describe trace

                let keep (t: TickTrace) =
                    [ "spawn-Spawn10"; $"tower-{childTower.X}-{childTower.Y}" ]
                    |> List.sumBy (fun id -> Map.tryFind id t.Structures |> Option.defaultValue 0)

                match safeModeOn trace with
                | Some fired ->
                    Expect.equal fired 0 $"fired on the first tick\n{failure}"

                    let after = trace |> List.filter (fun t -> t.Tick >= fired)

                    Expect.isTrue
                        (after |> List.forall (fun t -> keep t = keep (List.head after)))
                        $"nothing landed on the Keep after\n{failure}"
                | None -> failtest $"no safe mode\n{failure}"
            }

            test
                "scenario 2 (#448): a skirmisher the garrison wins fires no safe mode in a towerless home" {
                let skirmisher =
                    body
                        "Skirmish1"
                        trep
                        (parts [ Move, 5; RangedAttack, 2; Heal, 3 ])
                        (w17s25 8 18)
                        (Some(Kite(Nearest, 3)))

                let _, trace = bootstrappingChild 1 (skirmisher :: residents) |> run 60
                let failure = describe trace
                Expect.isNone (safeModeOn trace) $"no safe mode\n{failure}"
                Expect.isSome (diedOn "Skirmish1" trace) $"the garrison kills it\n{failure}"
            }

            test
                "scenario 2 (#448): the full raid in a towerless home fires safe mode on sight, the garrison losing it" {
                let _, trace = bootstrappingChild 1 (residents @ insideRaid) |> run 3
                Expect.equal (safeModeOn trace) (Some 0) $"the undefended arm\n{describe trace}"
            }

            test
                "an Odiodin garrison walks through W17S25's perimeter in peace, the line shut again behind it (#482)" {
                let final, trace = sealedChild 300_000 500 0 passingGarrison |> run 30
                let failure = describe trace

                for b in passingGarrison do
                    let now = final.Bodies |> List.find (fun o -> o.Id = b.Id)
                    Expect.isGreaterThan now.At.X 2 $"{b.Id} through the line\n{failure}"
                    Expect.equal now.Hits b.Hits $"{b.Id} untouched\n{failure}"

                let landed = flips trace

                Expect.isTrue
                    (landed
                     |> List.exists (fun (_, id, isPublic) -> isPublic && Set.contains id lineIds))
                    $"the line opened: {landed}\n{failure}"

                Expect.isEmpty
                    (final.Rooms["W17S25"].Structures
                     |> List.filter (fun s -> s.Kind = "rampart" && s.IsPublic))
                    $"and shut once the garrison was through: {landed}\n{failure}"
            }

            test
                "with a Trepidimous melee 6 tiles off, W17S25's line stays shut to the Odiodin garrison beside it (#482)" {
                // The tower dry, so the melee stands where it was put.
                let melee = body "Eternity536" trep trepMelee (w17s25 8 17) (Some Hold)
                let final, trace = sealedChild 300_000 0 0 (melee :: passingGarrison) |> run 20
                let failure = describe trace

                Expect.isEmpty (flips trace) $"never opened\n{failure}"

                for b in passingGarrison do
                    let now = final.Bodies |> List.find (fun o -> o.Id = b.Id)
                    Expect.isLessThan now.At.X 2 $"{b.Id} held outside\n{failure}"
            }

            for name, striker in
                [
                    "25 WORK dismantlers", parts [ Move, 25; Work, 25 ]
                    "20 ATTACK melee", parts [ Move, 20; Attack, 20 ]
                    "20 RANGED_ATTACK rangers", parts [ Move, 20; RangedAttack, 20 ]
                ] do
                test
                    $"scenario 3, scripted on both sides — our decide does not run: two {name} and two 16 HEAL healers on W18S26's outer line, far from its towers" {
                    let start, final, trace = offenceProbe striker
                    let failure = describe trace
                    let squadIds = start.Bodies |> List.map (fun b -> b.Id)
                    let fell = firstFallen outerLine trace

                    let towers =
                        final.Rooms["W18S26"].Structures
                        |> List.filter (fun s -> s.Kind = "tower")
                        |> List.sumBy (fun s -> s.Energy)

                    // Both towers at their falloff floor land 300 on one
                    // body, under the 384 the two healers put back.
                    Expect.isLessThan
                        (2 * Engine.towerAttackAt Engine.towerFalloffRange)
                        (2 * 16 * Engine.healPower)
                        "at the far line the heal outpaces the towers"

                    Expect.equal towers 0 $"the towers' 2,000 energy all spent\n{failure}"

                    match name with
                    | "25 WORK dismantlers" ->
                        // Measured: the line falls at t351, nobody lost,
                        // 17,100 energy of bodies.
                        Expect.isSome fell $"breached\n{failure}"
                        Expect.isLessThan fell.Value 400 "inside 400 ticks"

                        for id in squadIds do
                            Expect.isNone (diedOn id trace) $"{id} lives\n{failure}"
                    | "20 ATTACK melee" ->
                        // Measured: t691; the towers' nearest-first fire
                        // kills both healers on the walk in.
                        Expect.isSome fell $"breached\n{failure}"
                        Expect.isLessThan fell.Value 750 "inside 750 ticks"
                    | _ ->
                        // 400 a tick against ~780,000: not inside a life.
                        Expect.isNone fell $"never breached inside a life\n{failure}"
                }

            test
                "scenario 4, our decide (#490): the default assault squad musters, launches whole and breaks W18S26's far line with no loss" {
                let start, _, trace = assaultProbe 450
                let failure = describe trace
                let fell = firstFallen (Set.singleton farBreach.Id) trace

                // Measured: in at t3, the first dismantle at t62 after the
                // walk, the line down at t366 (2 × 1,250 a tick on 758,201),
                // no body under 72% of its hits. boosts.md §4.2 prices it
                // at t322 with the walk shorter.
                Expect.isSome fell $"breached\n{failure}"
                Expect.isLessThan fell.Value 400 "inside 400 ticks"

                for b in start.Bodies do
                    Expect.isNone (diedOn b.Id trace) $"{b.Id} lives\n{failure}"

                let sappers =
                    start.Bodies
                    |> List.filter (fun b -> b.Id.StartsWith "sapper")
                    |> List.map (fun b -> b.Id)

                let firstIn id =
                    pathOf id trace
                    |> List.tryFind (fun (_, s) -> s.At.Room = "W18S26")
                    |> Option.map fst

                let entries = start.Bodies |> List.map (fun b -> firstIn b.Id)

                Expect.isTrue
                    (entries |> List.forall Option.isSome)
                    $"every cast went in: {entries}\n{failure}"

                Expect.isLessThanOrEqual
                    ((entries |> List.choose id |> List.max)
                     - (entries |> List.choose id |> List.min))
                    4
                    $"together, not one by one: {entries}"

                Expect.isTrue
                    (trace
                     |> List.exists (fun t ->
                         t.Ours
                         |> List.exists (function
                             | DismantleStructure(name, id) ->
                                 id = farBreach.Id && List.contains name sappers
                             | _ -> false)))
                    "the sappers took it down"
            }

            test
                "scenario 5, the bait (#491): the Provoke raises W18S26's safe mode, and only then the Strike breaks W17S24's west line and kills its tower and spawn" {
                let start, final, trace = baitProbe 2_000
                let failure = describe trace

                let raisedIn room =
                    trace
                    |> List.tryPick (fun t ->
                        t.Events
                        |> List.tryPick (function
                            | SafeModeActivated r when r = room -> Some t.Tick
                            | _ -> None))

                let provoked = raisedIn "W18S26"

                Expect.isSome provoked $"W18S26 raised safe mode\n{failure}"
                Expect.isNone (raisedIn "W17S24") "W17S24 never could"

                // The Strike's casts are W18S25's: Spawn9's.
                let strikers =
                    start.Bodies
                    |> List.filter (fun b -> b.Id.EndsWith "Spawn9")
                    |> List.map (fun b -> b.Id)

                let stepped (t: TickTrace) =
                    t.Ours
                    |> List.exists (function
                        | MoveCreep(name, _) -> List.contains name strikers
                        | _ -> false)

                let firstStep = trace |> List.tryFind stepped |> Option.map (fun t -> t.Tick)

                Expect.isSome firstStep $"the Strike went\n{failure}"

                Expect.isGreaterThan
                    firstStep.Value
                    provoked.Value
                    "not one step before the window opened"

                let left =
                    strikers
                    |> List.choose (fun id ->
                        pathOf id trace
                        |> List.tryFind (fun (_, s) -> s.At.Room <> "W18S25")
                        |> Option.map fst)

                Expect.hasLength left 4 "every cast went"

                let w17s24 = start.Rooms["W17S24"].Structures

                let idsWhere pick =
                    w17s24 |> List.filter pick |> List.map (fun s -> s.Id) |> Set.ofList

                let line = idsWhere (fun s -> s.Kind = "rampart" && s.At.X = 2)
                let tower = idsWhere (fun s -> s.At = w17s24Tower)
                let spawn = idsWhere (fun s -> s.At = w17s24Spawn)

                let breached = firstFallen line trace
                let towerDown = firstFallen tower trace
                let spawnDown = firstFallen spawn trace

                Expect.isSome breached $"the west line breached\n{failure}"
                Expect.isSome towerDown $"the tower killed\n{failure}"
                Expect.isSome spawnDown $"the spawn killed\n{failure}"

                // Measured: safe mode at t62 off the Provoke's first
                // dismantle; the Strike's first step t63, out of W18S25, where
                // it musters, at t95–101, the west line down at t384, the
                // tower at t423, the spawn at t442. The Strike lost nobody.
                Expect.isLessThan spawnDown.Value 600 $"inside 600 ticks, ended t{final.Tick}"

                for id in strikers do
                    Expect.isNone (diedOn id trace) $"{id} lives\n{failure}"

                // The Provoke's way out (#492): every exit it can reach is
                // W17S26's, 18 tiles east of the far line — W18S27's lie
                // behind the line, W18S25's 35 tiles north — and all of it
                // past the towers' falloff, 300 a tick with our heal refused.
                // A body takes MOVE damage first, so the first 100 hits halve
                // its pace and the one hit straggles. Measured: a medic dead
                // at t84 and a sapper at t93, the other two out of the room;
                // before #492, both medics.
                let lost =
                    start.Bodies
                    |> List.filter (fun b -> b.Id.EndsWith "Spawn8")
                    |> List.filter (fun b -> Option.isSome (diedOn b.Id trace))

                Expect.isLessThanOrEqual lost.Length 2 $"half the Provoke at most\n{failure}"
            }

            test
                "scenario 6 (#493): with no safe mode, the default squad cannot hold W18S26's west line: the pocket outside it fits two sappers and one medic" {
                let start, _, trace = westProbe false 1_500
                let failure = describe trace

                // Measured: the first dismantle at t8; the second medic finds
                // no tile in the pocket (x1 y5–9) and is carried back and forth
                // over the border; the one inside, the towers' pick, dies at
                // t34 under 390 a tick with its own heal alone; the squad falls
                // back after 37 dismantles and is not whole again.
                Expect.isNone
                    (firstFallen (Set.singleton westBreach.Id) trace)
                    $"never breached inside a life\n{failure}"

                let lost =
                    start.Bodies
                    |> List.choose (fun b -> diedOn b.Id trace)
                    |> List.filter (fun tick -> tick < Engine.creepLifetime - 1)

                Expect.equal lost.Length 1 $"one medic lost\n{failure}"
            }

            test
                "scenario 6b (#493): Trepidimous safe-modes the default squad's first dismantle on the west line, and it walks out losing nobody" {
                let start, _, trace = westProbe true 200
                let failure = describe trace
                let raised = safeModeOn trace

                // Measured: the first dismantle and the safe mode at t8, every
                // cast out of W18S26 at t10, a sapper's lowest 3,470.
                Expect.isSome raised $"safe mode raised\n{failure}"

                for b in start.Bodies do
                    Expect.isNone (diedOn b.Id trace) $"{b.Id} lives\n{failure}"

                    Expect.isTrue
                        (pathOf b.Id trace
                         |> List.forall (fun (tick, s) ->
                             tick <= raised.Value + 3 || s.At.Room <> "W18S26"))
                        $"{b.Id} out within three ticks of it\n{failure}"
            }

            test
                "scenario 7 (#493): the shipped probe walks from W17S29 by W19S26, raises a scripted safe mode on its first dismantle, and walks out alive, the probe log saying so" {
                let start, final, trace = probeProbe 500
                let failure = describe trace
                let probe = start.Bodies |> List.exactlyOne

                Expect.isNone (diedOn probe.Id trace) $"the probe lives\n{failure}"

                let rooms =
                    pathOf probe.Id trace |> List.map (fun (_, s) -> s.At.Room) |> List.distinct

                Expect.equal
                    (rooms |> List.skipWhile ((<>) "W19S26") |> List.truncate 2)
                    [ "W19S26"; "W18S26" ]
                    $"in from W19S26: {rooms}"

                // Measured: in W18S26 at t258, the first dismantle at t260,
                // safe mode seen at t261, out at t262 on 660 of its 1,800.
                let logs =
                    Map.tryFind "W17S29" final.Carried.Raids
                    |> Option.map (fun raids -> raids.Probes)
                    |> Option.defaultValue Map.empty

                match Map.tryFind "W18S26" logs with
                | Some log ->
                    Expect.equal log.Hits 1 "one dismantle raised it"

                    Expect.equal log.SafeModeAt (Some(log.FirstHit + 1)) "seen the tick after"

                    Expect.isTrue
                        (match log.Fate with
                         | Fabot.Core.Observe.Out tick -> tick <= log.FirstHit + 3
                         | _ -> false)
                        $"out alive at once: {log.Fate}"
                | None -> failtest $"no probe log\n{failure}"
            }

            test
                "the link raid's squad musters at W15S28 by the exit its walk leaves by, off the spawns and extensions, its walk entering W18S26 from W18S27 in seven crossings and crossing no rival's room" {
                let start, _, _ = linkProbe false 0
                let view = viewOf start "W15S28"
                let atlas = Fabot.Core.Atlas.ofView view
                let chain = Fabot.Core.Atlas.siegeRoute atlas "W15S28" "W18S26"

                Expect.isSome chain "a siege chain inside its own budget"

                Expect.equal
                    (List.length chain.Value - 1,
                     chain.Value |> List.truncate 2,
                     chain.Value |> List.rev |> List.truncate 2)
                    (7, [ "W15S28"; "W15S29" ], [ "W18S26"; "W18S27" ])
                    $"seven crossings, out by W15S29 and in from W18S27: {chain}"

                // The tuning's six find none (#494).
                let defaulted =
                    { start with
                        Colonies =
                            [
                                { colony "W15S28" with
                                    Assaults =
                                        [
                                            { Assault.w18s26Link with
                                                MaxHops = None
                                                Active = true
                                            }
                                        ]
                                }
                            ]
                    }

                Expect.isNone
                    (Fabot.Core.Atlas.siegeRoute
                        (Fabot.Core.Atlas.ofView (viewOf defaulted "W15S28"))
                        "W15S28"
                        "W18S26")
                    "no chain inside the tuning's budget"

                Expect.isTrue
                    (chain.Value
                     |> List.forall (fun name ->
                         name = "W18S26" || not (Set.contains name view.Spatial.RivalRooms)))
                    $"no rival's room on the way: {chain}"

                let rally =
                    Fabot.Core.Decide.Threat.Threats.formationOf
                        (Fabot.Core.Decide.Threat.threatsOfHeld
                            view
                            atlas
                            Fabot.Core.Decide.Facts.HeldTaskFacts.empty)
                        (Assault "W18S26")
                    |> Option.get
                    |> fun ground -> ground.Rally

                Expect.isNonEmpty rally "a rally ground"

                Expect.isTrue
                    (rally |> Set.forall (fun tile -> tile.Room = "W15S28"))
                    $"at home: {rally}"

                let next = List.item 1 chain.Value

                let exits = Fabot.Core.Atlas.seams atlas "W15S28" next |> List.map fst

                Expect.isTrue
                    (rally
                     |> Set.forall (fun tile ->
                         exits |> List.exists (fun exit -> range (RoomPos.pos tile) exit <= 3)))
                    $"within three of the exit toward {next}: {rally}"

                let idle = Fabot.Core.Atlas.idleGroundIn atlas "W15S28"

                Expect.isTrue
                    (rally |> Set.forall (fun tile -> not (Set.contains (RoomPos.pos tile) idle)))
                    $"off the ground beside the spawns, extensions and stores: {rally}"
            }

            test
                "scenario 8 (#496, #494): the shipped link assault musters at W15S28, walks out whole by the south, breaks W18S26's south line from W18S27, dismantles the link behind it and walks out, the raid log saying it is taken" {
                let start, final, trace = linkProbe false 1_500
                let failure = describe trace
                let fell = firstFallen (Set.singleton southBreach.Id) trace
                let linkDown = firstFallen (Set.singleton farLink.Id) trace

                let out =
                    trace
                    |> List.tryFind (fun t ->
                        linkDown |> Option.exists (fun down -> t.Tick > down)
                        && start.Bodies
                           |> List.forall (fun b ->
                               pathOf b.Id trace
                               |> List.tryFind (fun (tick, _) -> tick = t.Tick)
                               |> Option.forall (fun (_, s) -> s.At.Room <> "W18S26")))
                    |> Option.map (fun t -> t.Tick)

                // Measured: mustered by W15S28's south exit, out of home at
                // t63–66 by W15S29, W16S29, W16S28, W17S28, W17S27 and
                // W18S27, the line down at t563, the link at t583 round the
                // walls west of it, every cast out of the room by W18S27 at
                // t590, nobody lost.
                Expect.isSome fell $"breached\n{failure}"
                Expect.isSome linkDown $"the link down\n{failure}"
                Expect.isLessThan linkDown.Value (fell.Value + 30) "a few steps past the breach"
                Expect.isSome out $"every cast out of the room\n{failure}"
                Expect.isLessThan out.Value (linkDown.Value + 40) "straight out once it is down"

                for b in start.Bodies do
                    Expect.isNone (diedOn b.Id trace) $"{b.Id} lives\n{failure}"

                // Launched from home whole: every cast leaves W15S28 for good
                // within a dozen ticks of the rest — the tail shuffles at the
                // crossing while the leader holds for it; a step onto the exit
                // at rally, carried over and straight back, is no launch — and
                // goes in by W18S27.
                let leftHome =
                    start.Bodies
                    |> List.map (fun b ->
                        pathOf b.Id trace
                        |> List.windowed 5
                        |> List.tryFind (List.forall (fun (_, s) -> s.At.Room <> "W15S28"))
                        |> Option.map (List.head >> fst))

                Expect.isTrue
                    (leftHome |> List.forall Option.isSome)
                    $"every cast left home: {leftHome}"

                Expect.isLessThanOrEqual
                    ((leftHome |> List.choose id |> List.max)
                     - (leftHome |> List.choose id |> List.min))
                    12
                    $"together, not one by one: {leftHome}\n{failure}"

                for b in start.Bodies do
                    let rooms =
                        pathOf b.Id trace |> List.map (fun (_, s) -> s.At.Room) |> List.distinct

                    Expect.equal
                        (rooms |> List.skipWhile ((<>) "W18S26") |> List.truncate 1)
                        [ "W18S26" ]
                        $"{b.Id} went in: {rooms}"

                    Expect.equal
                        (rooms |> List.takeWhile ((<>) "W18S26") |> List.last)
                        "W18S27"
                        $"{b.Id} in from W18S27: {rooms}"

                Expect.isTrue
                    (Map.tryFind "W15S28" final.Carried.Raids
                     |> Option.exists (fun raids -> Map.containsKey "W18S26" raids.Taken))
                    "the raid log has W18S26 taken"

                let _, after = final |> run 30

                Expect.isTrue
                    (after
                     |> List.forall (fun t ->
                         t.Bodies
                         |> List.forall (fun s ->
                             s.At.Room <> "W18S26" || not (s.Id.EndsWith "Spawn3"))))
                    $"its work done, nobody goes back in\n{describe after}"
            }

            test
                "scenario 8b (#496, #494): Trepidimous safe-modes the link assault's first dismantle on the south line; the link stands, and the squad steps out losing nobody" {
                let start, final, trace = linkProbe true 1_000
                let failure = describe trace
                let raised = safeModeOn trace
                let linkDown = firstFallen (Set.singleton farLink.Id) trace

                let stranded =
                    final.Bodies
                    |> List.filter (fun b -> b.Side = Side.Ours && b.At.Room = "W18S26")

                // Measured: safe mode at t249 off the first dismantle, from
                // (24,48) outside the line, the rest of the file still in
                // W18S27; one step to W18S27's exit, out at t249, where the
                // far line's 18 cost a sapper and a medic.
                Expect.isSome raised $"safe mode raised\n{failure}"
                Expect.isNone linkDown "the link stands"
                Expect.isEmpty stranded $"nobody left in the room\n{failure}"

                for b in start.Bodies do
                    Expect.isNone (diedOn b.Id trace) $"{b.Id} lives\n{failure}"

                    Expect.isTrue
                        (pathOf b.Id trace
                         |> List.forall (fun (tick, s) ->
                             tick <= raised.Value + 3 || s.At.Room <> "W18S26"))
                        $"{b.Id} out within three ticks of it\n{failure}"
            }

            // Live t930,260: the leader beside the breach, a medic four back
            // down the one-tile corridor; the file shoved the leader east off
            // the breach to the corridor's end at x29 and held there for good.
            for name, xs in
                [
                    "the leader at the breach, a medic four back (live t930,260)",
                    [ 24; 23; 22; 20 ]
                    "the file shoved east to the corridor's end (live t930,290)", [ 29; 28; 27; 26 ]
                ] do
                test $"scenario 8c: in W18S26's south corridor, {name}: the line falls" {
                    let start, _, trace = inCorridor xs false 600
                    let failure = describe trace
                    let fell = firstFallen (Set.singleton southBreach.Id) trace

                    // Measured: down at t311 and t314, both sappers on it —
                    // the line's hits over two sappers' dismantle.
                    Expect.isSome fell $"breached\n{failure}"
                    Expect.isLessThan fell.Value 330 $"both sappers at it\n{failure}"

                    for b in start.Bodies do
                        Expect.isNone (diedOn b.Id trace) $"{b.Id} lives\n{failure}"
                }

            test
                "a shooter at range three of the line: the home casts a ranger, not a defender, and it shoots from a rampart (#546)" {
                let _, trace = kitedHome () |> run 80
                let failure = describe trace

                Expect.isFalse
                    (trace
                     |> List.exists (fun t ->
                         t.Bodies |> List.exists (fun b -> b.Id.StartsWith "defender-")))
                    $"no melee defender\n{failure}"

                // A shot decided on a tick, from where the shooter stood when
                // the tick before ended.
                let fromCover =
                    trace
                    |> List.pairwise
                    |> List.tryPick (fun (before, t) ->
                        t.Ours
                        |> List.tryPick (function
                            | RangedAttackCreep(name, "Meow") when name.StartsWith "ranger-" ->
                                before.Bodies
                                |> List.tryFind (fun b ->
                                    b.Id = name && List.contains (RoomPos.pos b.At) perimeter)
                                |> Option.map (fun b -> t.Tick, b.At)
                            | _ -> None))

                // Measured: cast at t0, shooting from the south line's 16,44
                // at t29.
                match fromCover with
                | Some(tick, at) ->
                    Expect.isLessThanOrEqual tick 40 $"cast and walked there\n{failure}"

                    Expect.isLessThanOrEqual
                        (RoomPos.range at (w17s25 17 47) |> Option.defaultValue 99)
                        Engine.rangedRange
                        $"its rampart within three of the shooter\n{failure}"
                | None -> failtest $"a ranger shoots from a perimeter rampart\n{failure}"
            }
        ]

/// W17S25 as captured at t974,040 (`npm run capture -- W17S25 974040`, #545's
/// re-review): RCL4, its tower at 13,29 full, Spawn11 at 14,28 under a
/// 701-hit rampart, twenty extensions, two 16C haulers, and no hostile. The
/// safe-mode cooldown is cleared so the arms alone decide, and the mother
/// W15S28 is left out: the safe-mode reflex reads this room alone.
let private w17s25At974040 () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withSafeModeCooldown 0
        |> withControllerClocks 0 0 40_000 74_238
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in
                    [
                        10, 24
                        10, 28
                        10, 30
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 28
                        16, 30
                        17, 29
                        17, 31
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                yield
                    { structureOf "container" None 96_200 250_000 { X = 16; Y = 34 } with
                        Energy = 1_781
                    }
                yield
                    { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                        Energy = 46
                    }
                yield
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 10; Y = 26 } with
                        Energy = 22
                    }
                for (x, y), hits in
                    [
                        (2, 15), 98_521
                        (2, 16), 99_361
                        (2, 17), 97_611
                        (2, 18), 90_931
                        (2, 19), 90_871
                        (2, 20), 96_131
                        (2, 23), 97_121
                        (2, 24), 91_101
                        (2, 25), 99_601
                        (2, 26), 99_261
                        (2, 27), 99_761
                        (13, 29), 6_401
                        (14, 28), 701
                        (16, 44), 90_321
                        (17, 20), 30_201
                        (17, 44), 90_301
                        (18, 44), 94_301
                        (32, 31), 99_871
                        (32, 32), 98_881
                    ] -> structureOf "rampart" (Some Side.Ours) hits 3_000_000 { X = x; Y = y }
                yield towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]
        |> withSite Side.Ours "storage" None 15_711 { X = 13; Y = 27 }

    let colonies = Colony.declared |> List.filter (fun c -> c.Home = "W17S25")

    let bodies: Body list =
        [
            { body
                  "guard-972528-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 17 40)
                  None with
                TicksToLive = 16
            }
            { body
                  "ranger-972663-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 37)
                  None with
                TicksToLive = 247
            }
            { body
                  "hauler-972861-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 11 24)
                  None with
                TicksToLive = 391
            }
            { body
                  "guard-973227-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 19 30)
                  None with
                TicksToLive = 715
            }
            { body
                  "ranger-973229-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 5 20)
                  None with
                TicksToLive = 813
            }
            { body
                  "defender-973295-Spawn11"
                  Side.Ours
                  (parts [ Move, 10; Attack, 10 ])
                  (at "W17S25" 26 31)
                  None with
                TicksToLive = 813
            }
            { body
                  "upgrader-973563-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 16 34)
                  None with
                TicksToLive = 1_072
                Energy = 42
            }
            { body
                  "anchor-973966-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 1_454
                Energy = 50
            }
            { body
                  "hauler-973355-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 17 34)
                  None with
                TicksToLive = 885
                Energy = 800
            }
        ]

    arena 974_041 [ w17s25 ] colonies bodies

[<Tests>]
let arenaEconomyArmTests =
    let trep = Side.Player "Trepidimous"
    let w (x: int) (y: int) = RoomPos.at "W17S25" { X = x; Y = y }

    let melee id x y =
        body id trep trepMelee (w x y) (Some Hold)

    let healer id x y =
        body id trep trepHealer (w x y) (Some Hold)

    let mapRoom (f: ArenaRoom -> ArenaRoom) (a: Arena) =
        { a with
            Rooms = a.Rooms |> Map.change "W17S25" (Option.map f)
        }

    let towerAt (energy: int) (a: Arena) =
        a
        |> mapRoom (fun r ->
            { r with
                Structures =
                    r.Structures
                    |> List.map (fun s ->
                        if s.Kind = "tower" then { s with Energy = energy } else s)
            })

    let withBodies bodies (a: Arena) = { a with Bodies = a.Bodies @ bodies }

    let carriersDead (a: Arena) =
        { a with
            Bodies =
                a.Bodies
                |> List.filter (fun b -> not (b.Side = Side.Ours && List.contains Carry b.Parts))
        }

    let fires (a: Arena) =
        let _, trace = a |> run 1
        safeModeOn trace

    testList
        "the economy arm on W17S25's t974,040 layout (#545)"
        [
            test
                "two melee pairs beside the worn spawn, the tower full, fire: the spawn falls in six ticks" {
                let a =
                    w17s25At974040 ()
                    |> withBodies
                        [ melee "m1" 15 28; healer "h1" 16 29; melee "m2" 13 28; healer "h2" 12 29 ]

                Expect.equal (fires a) (Some 0) "B"
            }

            test "a pair beside the spawn, the tower dry and the carriers dead, fires" {
                let a =
                    w17s25At974040 ()
                    |> towerAt 0
                    |> carriersDead
                    |> withBodies [ melee "m1" 15 28; healer "h1" 16 29 ]

                Expect.equal (fires a) (Some 0) "C"
            }

            test
                "the same with the upgrader and the anchor alive: their crumbs refill nothing, and it fires" {
                // A Meow drain with the upgraders standing: a body holding no
                // Refill of this home buys a dry tower no shot.
                let a =
                    w17s25At974040 ()
                    |> towerAt 0
                    |> fun a ->
                        { a with
                            Bodies =
                                a.Bodies |> List.filter (fun b -> not (b.Id.StartsWith "hauler-"))
                        }
                    |> withBodies [ melee "m1" 15 28; healer "h1" 16 29 ]

                Expect.equal (fires a) (Some 0) "C with upgraders"
            }

            for name, energy, raid in
                [
                    "C2: a 1-RANGED cripple at range 3 of the spawn, the tower at 0",
                    0,
                    [ body "c1" trep (parts [ Move, 1; RangedAttack, 1 ]) (w 17 26) (Some Hold) ]
                    "C3: the same cripple, the tower at 5",
                    5,
                    [ body "c1" trep (parts [ Move, 1; RangedAttack, 1 ]) (w 17 26) (Some Hold) ]
                    "C4: a lone melee beside the spawn, the tower at 50", 50, [ melee "m1" 15 28 ]
                    "C5: a pair beside the spawn, the tower at 60",
                    60,
                    [ melee "m1" 15 28; healer "h1" 16 29 ]
                ] do
                test $"{name}, its 800-energy hauler holding the tower's Refill, spends nothing" {
                    let a = w17s25At974040 () |> towerAt energy |> withBodies raid

                    let a =
                        { a with
                            Carried =
                                { a.Carried with
                                    Assignments =
                                        Map.ofList
                                            [
                                                "hauler-973355-Spawn11",
                                                Fabot.Core.Decide.Facts.taskId (
                                                    Refill("tower-13-29", Energy)
                                                )
                                            ]
                                }
                        }

                    Expect.isNone (fires a) name
                }

            test
                "D4: two 10-CARRY haulers refilling beside a pair at the west gate, the tower full, spend nothing" {
                let cheap (id: string) (x: int) (y: int) (a: Arena) =
                    { a with
                        Bodies =
                            a.Bodies
                            |> List.map (fun b ->
                                if b.Id = id then
                                    { b with
                                        Parts = parts [ Carry, 10; Move, 5 ]
                                        Hits = 1_500
                                        At = w x y
                                    }
                                else
                                    b)
                    }

                let refill = Fabot.Core.Decide.Facts.taskId (Refill("spawn-Spawn11", Energy))

                let a =
                    w17s25At974040 ()
                    |> cheap "hauler-972861-Spawn11" 4 18
                    |> cheap "hauler-973355-Spawn11" 4 17
                    |> withBodies [ melee "m1" 3 18; healer "h1" 2 17 ]

                let a =
                    { a with
                        Carried =
                            { a.Carried with
                                Assignments =
                                    Map.ofList
                                        [
                                            "hauler-972861-Spawn11", refill
                                            "hauler-973355-Spawn11", refill
                                        ]
                            }
                    }

                Expect.isNone (fires a) "two cheap bodies are under the refilling floor"
            }
        ]

/// W13S28 at RCL8 with its observer, and W14S27 declared a colony of ours
/// nobody has claimed: diagonal to the home, so in no scan set and blind.
let private observing () =
    let home =
        room "W13S28"
        |> withController Ownership.Ours None 8 0
        |> withSpawn "Spawn3" { X = 36; Y = 42 } 12_900
        |> withStructures [ structureOf "observer" (Some Side.Ours) 500 500 { X = 30; Y = 30 } ]

    arena 922_600 [ home; room "W14S27" ] [ colony "W13S28"; colony "W14S27" ] []

[<Tests>]
let arenaVisionTests =
    testList
        "arena vision"
        [
            test
                "an observed room's sighting reaches the next tick's World, and is kept after (#484)" {
                let observer = "observer-30-30"

                let sightingOf (a: Arena) =
                    Map.tryFind "W14S27" (worldOf a).Sightings

                let start = observing ()

                Expect.isNone (sightingOf start) "blind before the look"

                let looked, trace = step start

                Expect.contains
                    trace.Ours
                    (ObserveRoom(observer, "W14S27"))
                    "the observer looks at the declared room nobody of ours sees"

                Expect.equal
                    (sightingOf looked |> Option.map (fun s -> s.Tick))
                    (Some looked.Time)
                    "seen the next tick, through the World's own path"

                let after, _ = step looked

                Expect.isFalse (Set.contains "W14S27" after.Observed) "seen, so not looked at again"

                Expect.equal
                    (sightingOf after |> Option.map (fun s -> s.Tick))
                    (Some looked.Time)
                    "and the sighting is remembered once the vision lapses"
            }
        ]

// ---- arena capture intercept850 (#569) -------------------------------------
// `npm run capture -- W18S25 990400 --rooms W17S25,W15S28,W17S26 --name intercept850`,
// read at live t990,553. The room-history replay at t990,400, the state after
// that tick ran: arena tick 0 runs as t990,401.
//   W18S25  https://screeps.com/room-history/shardSeason/W18S25/990400.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/990400.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/990400.json
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/990400.json
// Healers, ExitWatches, Sightings and the raid memory start empty (heap only);
// the Raid log, ArmedSeen and Towered are live Memory's at t990,553, less what
// postdates the tick (ArmedSeen W18S25 t990,548, Threatened W18S25 t990,548).
// W18S25's and W17S26's reservations, 1 energy, 3 tombstones and 2 ruins are
// not placed.

/// W18S25 at t990,400 (#569): Intercept850, Trepidimous's 17 MOVE 17 ATTACK
/// 1 MOVE crippled to 14 ATTACK and 1 MOVE, plods down the road from (42,13)
/// toward (22,10), one tile every few ticks, while W17S25 stands W18S25 down
/// on its raid (PlayerRaidHold, to t990,545). Nothing of ours stands in the
/// room: it is seen only on W13S28's observer's looks. Live it lived to
/// t990,631, killed once the stand-down ran out and the rooms' work resumed.
let private intercept850 () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w18s25 =
        room "W18S25"
        |> withStructures
            [
                { structureOf "container" None 120_000 250_000 { X = 31; Y = 5 } with
                    Energy = 410
                }
            ]
        |> withRoads
            [
                28, 9, 4_000
                29, 8, 3_000
                29, 10, 4_400
                30, 7, 3_000
                30, 11, 2_400
                31, 6, 3_800
                31, 12, 2_600
                32, 13, 2_500
                33, 14, 2_700
                34, 15, 3_600
                35, 16, 3_300
                36, 17, 3_300
                37, 16, 2_600
                38, 15, 3_200
                39, 14, 3_100
                40, 13, 4_600
                41, 12, 4_200
                42, 13, 2_900
                43, 13, 4_000
                44, 13, 4_500
                45, 13, 2_600
                46, 14, 2_500
                47, 15, 4_100
                48, 16, 4_400
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withControllerClocks 0 0 40_000 263_454
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in
                    [
                        10, 24
                        10, 26
                        10, 28
                        10, 30
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 28
                        16, 30
                        17, 29
                        17, 31
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                structureOf "container" None 172_000 250_000 { X = 16; Y = 34 }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 262
                }
                { structureOf "rampart" (Some Side.Ours) 100_021 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 990_450
                }
                { structureOf "rampart" (Some Side.Ours) 100_561 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 990_431
                }
                { structureOf "rampart" (Some Side.Ours) 100_191 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 990_434
                }
                { structureOf "rampart" (Some Side.Ours) 100_531 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 990_439
                }
                { structureOf "rampart" (Some Side.Ours) 100_111 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 990_411
                }
                { structureOf "rampart" (Some Side.Ours) 100_031 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 990_413
                }
                { structureOf "rampart" (Some Side.Ours) 100_421 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 990_438
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 990_499
                }
                { structureOf "rampart" (Some Side.Ours) 101_101 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 990_417
                }
                { structureOf "rampart" (Some Side.Ours) 100_761 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 990_432
                }
                { structureOf "rampart" (Some Side.Ours) 100_361 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 990_480
                }
                { structureOf "rampart" (Some Side.Ours) 100_701 3_000_000 { X = 13; Y = 27 } with
                    NextDecay = 990_431
                }
                { structureOf "rampart" (Some Side.Ours) 100_701 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 990_416
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 990_491
                }
                { structureOf "rampart" (Some Side.Ours) 100_521 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 990_479
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 990_437
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 990_481
                }
                { structureOf "rampart" (Some Side.Ours) 99_701 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 990_477
                }
                { structureOf "rampart" (Some Side.Ours) 90_101 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 990_490
                }
                { structureOf "rampart" (Some Side.Ours) 95_891 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 990_409
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 9_804
                }
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]
        |> withRoads
            [
                1, 16, 2_600
                2, 17, 2_700
                3, 18, 2_600
                4, 19, 3_900
                5, 20, 3_900
                6, 21, 4_500
                7, 22, 19_500
                8, 23, 17_800
                9, 24, 16_900
                10, 25, 13_800
                10, 27, 18_400
                10, 29, 14_500
                11, 24, 13_300
                11, 26, 18_000
                11, 28, 15_400
                11, 30, 2_900
                12, 23, 15_400
                12, 25, 13_400
                12, 27, 16_900
                12, 29, 2_900
                13, 24, 4_800
                13, 26, 2_600
                13, 28, 3_600
                13, 30, 3_100
                14, 23, 4_600
                14, 25, 4_100
                14, 27, 2_900
                14, 29, 2_800
                14, 31, 2_800
                15, 22, 3_300
                15, 28, 3_600
                15, 30, 4_400
                15, 32, 2_800
                16, 21, 3_000
                16, 29, 4_400
                16, 31, 4_600
                16, 33, 4_600
                16, 35, 2_500
                16, 37, 4_300
                16, 38, 13_000
                16, 39, 13_300
                16, 41, 4_600
                16, 43, 4_400
                16, 44, 4_400
                17, 20, 2_900
                17, 30, 4_400
                17, 32, 2_600
                17, 34, 4_500
                17, 36, 3_600
                17, 38, 4_700
                17, 39, 12_900
                17, 40, 18_100
                17, 42, 4_700
                17, 45, 3_100
                18, 29, 2_600
                18, 31, 3_000
                18, 35, 4_100
                18, 37, 2_400
                18, 39, 3_000
                18, 41, 3_600
                18, 46, 3_600
                19, 30, 4_800
                19, 34, 3_100
                19, 36, 4_200
                19, 38, 3_000
                19, 40, 3_000
                19, 47, 3_300
                20, 31, 3_900
                20, 33, 3_300
                20, 35, 2_900
                20, 37, 4_800
                20, 39, 3_100
                20, 48, 4_000
                21, 32, 2_500
                21, 34, 4_300
                21, 36, 3_000
                21, 38, 4_200
                22, 33, 2_500
                22, 35, 3_100
                22, 37, 2_500
                23, 34, 2_500
                23, 36, 2_900
                24, 35, 3_100
                25, 34, 3_900
                26, 33, 4_300
                27, 32, 4_100
                28, 31, 18_000
                28, 33, 20_100
                29, 32, 18_000
                29, 34, 13_000
                30, 31, 18_500
                30, 33, 13_500
                31, 32, 14_000
                32, 31, 19_000
                33, 30, 20_000
                33, 32, 16_310
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 6_019_484
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 30
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 27
                        13, 29
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        15, 35
                        16, 26
                        16, 32
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        19, 27
                        19, 31
                        19, 33
                        20, 26
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        21, 35
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                structureOf "container" None 170_700 250_000 { X = 6; Y = 29 }
                { structureOf "container" None 125_100 250_000 { X = 11; Y = 18 } with
                    Energy = 726
                }
                { structureOf "container" None 197_500 250_000 { X = 23; Y = 29 } with
                    Energy = 1_950
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 18; Y = 34 } with
                    Energy = 50
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 99_701 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 990_461
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 990_421
                }
                { structureOf "rampart" (Some Side.Ours) 102_601 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 990_479
                }
                { structureOf "rampart" (Some Side.Ours) 101_001 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 990_429
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 990_422
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 990_475
                }
                { structureOf "rampart" (Some Side.Ours) 102_301 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 990_496
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 990_445
                }
                { structureOf "rampart" (Some Side.Ours) 102_401 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 990_438
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 30_493
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]
        |> withRoads
            [
                4, 5, 2_500
                5, 4, 3_800
                5, 6, 5_000
                5, 44, 4_200
                6, 3, 2_900
                6, 7, 4_300
                6, 29, 3_500
                6, 43, 3_800
                6, 45, 4_700
                7, 2, 4_500
                7, 8, 2_500
                7, 28, 4_700
                7, 42, 3_600
                7, 46, 4_000
                8, 1, 2_800
                8, 9, 3_300
                8, 28, 3_500
                8, 41, 3_900
                8, 47, 3_300
                9, 10, 4_300
                9, 27, 4_300
                9, 28, 4_900
                9, 40, 3_600
                9, 48, 4_000
                10, 11, 3_500
                10, 26, 4_700
                10, 29, 4_400
                10, 39, 3_900
                11, 12, 3_000
                11, 18, 3_100
                11, 27, 3_300
                11, 30, 4_600
                11, 38, 4_400
                12, 13, 3_900
                12, 17, 4_300
                12, 25, 20_100
                12, 27, 13_400
                12, 28, 4_800
                12, 29, 4_500
                12, 31, 3_400
                12, 33, 17_700
                12, 35, 21_500
                12, 37, 3_800
                13, 14, 4_000
                13, 16, 3_900
                13, 26, 18_300
                13, 28, 3_300
                13, 30, 4_900
                13, 32, 2_800
                13, 34, 3_900
                13, 36, 4_000
                14, 15, 4_900
                14, 25, 21_200
                14, 27, 2_900
                14, 29, 3_900
                14, 31, 4_200
                14, 33, 5_000
                14, 35, 4_300
                15, 15, 4_500
                15, 26, 2_700
                15, 28, 3_800
                15, 30, 2_800
                15, 32, 2_500
                15, 34, 4_000
                16, 16, 4_500
                16, 25, 15_000
                16, 27, 2_800
                16, 29, 2_900
                16, 31, 4_000
                16, 33, 3_500
                16, 35, 3_400
                17, 17, 2_900
                17, 26, 14_300
                17, 28, 2_800
                17, 30, 5_000
                17, 32, 4_700
                17, 34, 2_600
                18, 18, 4_000
                18, 25, 15_100
                18, 27, 12_900
                18, 29, 4_200
                18, 31, 3_800
                18, 33, 4_300
                19, 19, 19_200
                19, 22, 13_500
                19, 24, 19_500
                19, 26, 16_500
                19, 28, 3_000
                19, 30, 4_000
                19, 32, 5_000
                20, 20, 20_000
                20, 21, 14_500
                20, 23, 14_700
                20, 25, 13_800
                20, 27, 19_700
                20, 29, 2_700
                20, 31, 3_300
                20, 33, 13_600
                21, 21, 4_400
                21, 22, 2_700
                21, 23, 2_700
                21, 24, 13_000
                21, 25, 18_100
                21, 26, 15_800
                21, 28, 14_000
                21, 30, 4_700
                21, 32, 18_000
                21, 34, 14_700
                22, 24, 4_700
                22, 25, 19_300
                22, 27, 13_800
                22, 28, 4_500
                22, 29, 3_900
                22, 30, 14_500
                22, 31, 18_800
                22, 32, 20_100
                22, 33, 18_200
                22, 35, 4_300
                23, 26, 21_300
                23, 28, 18_000
                23, 29, 19_700
                23, 30, 19_400
                23, 31, 12_500
                23, 32, 19_900
                23, 33, 13_700
                23, 34, 4_900
                24, 25, 18_600
                24, 28, 20_500
                24, 29, 21_600
                24, 30, 18_900
                24, 33, 15_500
                25, 24, 4_700
                25, 28, 21_200
                25, 29, 20_800
                25, 34, 21_900
                26, 23, 21_500
                27, 22, 18_700
                28, 21, 18_400
                28, 34, 11_600
                29, 20, 19_700
                30, 19, 18_100
                31, 18, 4_300
                32, 17, 4_300
                33, 16, 3_000
                34, 15, 3_600
                35, 14, 4_400
                36, 13, 13_000
                37, 12, 4_600
                38, 11, 3_400
                39, 12, 4_300
                40, 13, 19_900
                41, 14, 18_200
                42, 15, 2_900
                43, 16, 2_900
                44, 17, 4_400
                45, 18, 4_400
                46, 19, 4_400
                47, 20, 3_100
                48, 21, 3_900
            ]

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 143_600 250_000 { X = 27; Y = 9 } with
                    Energy = 1_128
                }
            ]
        |> withRoads
            [
                20, 1, 3_900
                21, 2, 3_500
                22, 3, 3_500
                23, 4, 4_000
                24, 5, 4_400
                25, 6, 4_000
                26, 7, 2_500
                27, 8, 3_000
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    // Intercept850's live tiles, t990,400-990,620 (`observe history`).
    let plod =
        [
            42, 13
            41, 12
            40, 13
            39, 14
            38, 15
            37, 16
            36, 17
            35, 16
            34, 15
            33, 14
            32, 13
            31, 12
            30, 11
            29, 10
            28, 9
            27, 10
            26, 11
            25, 11
            24, 10
            23, 10
            22, 10
        ]
        |> List.map (fun (x, y) -> at "W18S25" x y)

    let bodies: Body list =
        [
            // Live: 14 ATTACK and 1 MOVE acting; it walks its live tiles.
            { body
                  "Intercept850"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W18S25" 42 13)
                  (Some(Waypoints plod)) with
                Hits = 1_474
                TicksToLive = 1_053
                Fatigue = 11
            }
            { body
                  "worker-988870-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 17 33)
                  None with
                TicksToLive = 28
                Energy = 188
            }
            { body
                  "anchor-989072-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 194
                Energy = 50
            }
            { body
                  "worker-989418-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 4 19)
                  None with
                TicksToLive = 576
                Energy = 344
            }
            { body
                  "hauler-989600-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 14 23)
                  None with
                TicksToLive = 770
            }
            { body
                  "ranger-989686-Spawn8"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 35)
                  None with
                TicksToLive = 910
            }
            { body
                  "hauler-989738-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 18 46)
                  None with
                TicksToLive = 908
                Energy = 300
            }
            { body
                  "upgrader-989914-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 19 34)
                  None with
                TicksToLive = 1_063
            }
            { body
                  "upgrader-990062-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 19 30)
                  None with
                TicksToLive = 1_211
            }
            { body
                  "defender-990336-Spawn11"
                  Side.Ours
                  (parts [ Move, 10; Attack, 10 ])
                  (at "W17S25" 9 24)
                  None with
                TicksToLive = 1_494
            }
            { body
                  "worker-989078-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 31)
                  None with
                TicksToLive = 326
                Energy = 258
            }
            { body
                  "worker-989272-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 520
                Energy = 136
            }
            { body
                  "guard-989780-Spawn3"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W15S28" 47 20)
                  None with
                TicksToLive = 908
            }
            { body
                  "hauler-989812-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 13 36)
                  None with
                TicksToLive = 1_054
                Energy = 922
            }
            { body
                  "hauler-989962-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 37 12)
                  None with
                TicksToLive = 1_204
                Energy = 1_114
            }
            { body
                  "hauler-990022-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 18 33)
                  None with
                TicksToLive = 1_264
                Energy = 1_300
            }
            { body
                  "anchor-990126-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 1_248
                Energy = 50
            }
            { body
                  "anchor-990150-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_272
                Energy = 50
            }
            { body
                  "upgrader-990248-Spawn3"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 22 29)
                  None with
                TicksToLive = 1_493
                Energy = 26
            }
            { body
                  "reserver-990388-Spawn8"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W15S28" 22 27)
                  None with
                TicksToLive = 592
            }
            // Live: 13 MOVE and 13 CARRY, a hauler of Trepidimous's, standing.
            { body
                  "Enigma370"
                  trepidimous
                  (parts (List.replicate 23 [ Move, 1; Carry, 1 ] |> List.concat))
                  (at "W17S26" 19 27)
                  (Some Hold) with
                Hits = 2_590
                TicksToLive = 606
            }
            { body
                  "ranger-989396-Spawn8"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S26" 17 23)
                  None with
                TicksToLive = 620
            }
            { body
                  "hauler-989528-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S26" 21 3)
                  None with
                TicksToLive = 698
                Energy = 50
            }
            { body
                  "reserver-989858-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 9 22)
                  None with
                TicksToLive = 68
            }
            { body
                  "anchor-990128-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 1_256
                Energy = 50
            }
            { body
                  "guard-990286-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S26" 19 17)
                  None with
                TicksToLive = 1_414
            }
        ]

    let a = arena 990_401 [ w18s25; w17s25; w15s28; w17s26 ] colonies bodies

    // A home's stand-down rows as live Memory held them, LastSeen clamped to
    // the tick: W16S26's stronghold, W18S25's raid, and the two rival
    // reservations.
    let row room opened expiry basis stronghold : Fabot.Core.Observe.OutpostEpisode =
        {
            RoomName = room
            Opened = opened
            LastSeen = 990_400
            Expiry = expiry
            Basis = basis
            Stronghold = stronghold
            Raiders = Set.empty
            PlayerSince = None
            Chased = false
            Bounced = false
            HolderUnread = false
        }

    let rows =
        [
            row "W16S26" 972_732 1_045_681 StandDownBasis.CollapseTimer true
            row "W18S25" 990_345 990_545 StandDownBasis.PlayerRaidHold false
            row "W18S29" 986_839 991_717 StandDownBasis.RivalReservation false
            row "W19S28" 986_841 991_510 StandDownBasis.RivalReservation false
        ]

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before t990,400,
                // read from Memory at live t990,553; it held none that early
                // for worker-988870, defender-990336 and reserver-989858.
                Assignments =
                    Map.ofList
                        [
                            "anchor-989072-Spawn11", "harvest:6a8caa7cdd4872bccd318c75"
                            "worker-989418-Spawn11", "repair:rampart-2-16"
                            "ranger-989686-Spawn8", "guard:W17S25"
                            "hauler-989738-Spawn11", "withdraw:container-27-9"
                            "worker-989078-Spawn3", "upgrade:6a8caa95dd4872bccd319015"
                            "worker-989272-Spawn3", "upgrade:6a8caa95dd4872bccd319015"
                            "hauler-989812-Spawn8", "refill:storage-17-29"
                            "hauler-989962-Spawn8", "refill:storage-17-29"
                            "hauler-990022-Spawn3", "refill:spawn-Spawn3"
                            "anchor-990126-Spawn8", "harvest:6a8caa95dd4872bccd319013"
                            "anchor-990150-Spawn8", "harvest:6a8caa95dd4872bccd319014"
                            "upgrader-990248-Spawn3", "upgrade:6a8caa95dd4872bccd319015"
                            "reserver-990388-Spawn8", "reserve:6a8caaa1dd4872bccd3191fa"
                            "ranger-989396-Spawn8", "guard:W17S26"
                            "hauler-989528-Spawn11", "withdraw:container-27-9"
                            "anchor-990128-Spawn11", "harvest:6a8caa7ddd4872bccd318c78"
                            "guard-990286-Spawn11", "guard:W17S26"
                        ]
                ArmedSeen = Map.ofList [ "W17S25", 990_343; "W17S26", 990_124 ]
                Towered = Set.ofList [ "W15S28"; "W17S25" ]
                Raids =
                    Map.ofList
                        [
                            "W15S28",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts = rows
                            }
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts = rows
                                HomeRaided = Some 990_343
                            }
                        ]
            }
    }

/// The same tick with W18S25's stand-down spent, as it was from t990,545:
/// the room worked again, the raid seen only on the observer's looks.
let private intercept850Spent () : Arena =
    let a = intercept850 ()

    { a with
        Carried =
            { a.Carried with
                Raids =
                    a.Carried.Raids
                    |> Map.map (fun _ log ->
                        { log with
                            Outposts =
                                log.Outposts |> List.filter (fun row -> row.RoomName <> "W18S25")
                        })
            }
    }

/// Run `ticks` ticks with W18S25 in sight only on W13S28's observer's looks,
/// which the arena does not hold: every eighth tick from tick 4, as live
/// (`dangerStride`). Each tick's trace, and the assignments it ended on.
let private underTheObserver (ticks: int) (start: Arena) : (TickTrace * Assignments) list =
    ((start, []), List.replicate ticks ())
    ||> List.fold (fun (a, acc) () ->
        let looked =
            { a with
                Lookout = (if (a.Tick + 4) % 8 = 0 then set [ "W18S25" ] else Set.empty)
            }

        let next, trace = step looked
        next, (trace, next.Carried.Assignments) :: acc)
    |> snd
    |> List.rev

/// The ticks from `first` until Intercept850 dies on which the ranger did not
/// end holding W18S25's Guard, and the tick it died on.
let private guardGaps (first: int) (run: (TickTrace * Assignments) list) =
    let trace = run |> List.map fst
    let died = diedOn "Intercept850" trace

    let gaps =
        run
        |> List.filter (fun (t, assignments) ->
            t.Tick >= first
            && died |> Option.forall (fun d -> t.Tick < d)
            && Map.tryFind "ranger-989686-Spawn8" assignments <> Some "guard:W18S25")
        |> List.map (fun (t, _) -> t.Tick)

    gaps, died

[<Tests>]
let intercept850Tests =
    testList
        "arena capture intercept850"
        [
            // Not pinned, a finding about the arena: with the stand-down
            // seated the arena chases and kills the cripple inside it on main
            // too (its verdict at the first look is Kite, which sets the row's
            // `Chased`), while live the room went unanswered to t990,545. The
            // Win half of that gap is pinned in `OutpostRaidTests` and
            // `ObserveOutpostTests` (#569).
            //
            // Live after t990,545 the ranger matched the Guard on each look and
            // dropped it task-gone the next tick (#569); main dropped it on
            // ticks 5 and 13 and the guard killed it at t67.
            test
                "#569 capture t990,400, its stand-down spent: the ranger holds W18S25's Guard through the ticks between the looks and kills the cripple" {
                let run = intercept850Spent () |> underTheObserver 60
                let gaps, died = guardGaps 4 run

                Expect.isEmpty gaps "never task-gone between two looks"
                Expect.isSome died "and its shots kill the cripple inside the run (t42 at writing)"
            }
        ]

// ---- arena capture w18s25-crippled (#573) -------------------------------------
// `npm run capture -- W18S25 994917 --rooms W17S25,W15S28,W19S25,W17S29,W17S26,W19S26,W13S28,W12S28,W12S26,W18S27,W18S28,W18S29,W19S28,W19S29,W16S25,W16S27,W16S29,W15S25,W15S27,W15S29,W14S28,W18S26,W17S24 --name w18s25-crippled --live-memory`, read at live t995,234.
// The room-history replay at t994,917: arena tick 0 runs as t994,918.
// Faraday387 (15 ATTACK on one MOVE, no healer) crawls west through W18S25 on
// its live tiles; its healer Intercept171 died at t994,882, a tick after
// its Lose opened the stand-down (PlayerRaidHold). Photon713 and Pendulum654
// raid W17S25's east side on their live tiles from t994,920. The W18S25 row
// is seated as live Memory held it at t995,011 (its raid bodies, its player
// clock and `Chased`): the replay read at t995,234 holds the row spent.

/// Live t994,917 (#573): a crippled melee crawling through W17S25's outpost
/// W18S25, shut on a player's raid its healer no longer heals, while a second
/// raid strikes W17S25's home next door.
let private w18s25Crippled () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"
    let giaco = Side.Player "giaco"
    let kazkel = Side.Player "Kazkel"
    let odiodin = Side.Player "Odiodin"

    let w18s25 =
        room "W18S25"
        |> withStructures
            [
                { structureOf "container" None 120_000 250_000 { X = 31; Y = 5 } with
                    Energy = 1_274
                }
            ]
        |> withRoads
            [
                28, 9, 3_300
                29, 8, 2_400
                29, 10, 3_700
                30, 7, 2_500
                30, 11, 4_100
                31, 6, 3_300
                31, 12, 4_200
                32, 13, 4_300
                33, 14, 4_400
                34, 15, 3_000
                35, 16, 2_600
                36, 17, 2_700
                37, 16, 4_400
                38, 15, 2_500
                39, 14, 2_500
                40, 13, 4_000
                41, 12, 3_600
                42, 13, 4_600
                43, 13, 3_200
                44, 13, 3_700
                45, 13, 4_300
                46, 14, 4_200
                47, 15, 3_400
                48, 16, 3_700
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 4 2
        |> withControllerClocks 0 0 40_000 316_662
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_300
        |> withStructures
            [
                for x, y in
                    [
                        10, 24
                        10, 26
                        10, 28
                        10, 30
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 28
                        16, 30
                        17, 29
                        17, 31
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                structureOf "container" None 127_000 250_000 { X = 16; Y = 34 }
                { structureOf "container" None 125_400 250_000 { X = 17; Y = 20 } with
                    Energy = 324
                }
                { structureOf "rampart" (Some Side.Ours) 99_421 3_000_000 { X = 2; Y = 15 } with
                    NextDecay = 995_004
                }
                { structureOf "rampart" (Some Side.Ours) 98_761 3_000_000 { X = 2; Y = 16 } with
                    NextDecay = 994_985
                }
                { structureOf "rampart" (Some Side.Ours) 95_511 3_000_000 { X = 2; Y = 17 } with
                    NextDecay = 994_988
                }
                { structureOf "rampart" (Some Side.Ours) 98_731 3_000_000 { X = 2; Y = 18 } with
                    NextDecay = 994_993
                }
                { structureOf "rampart" (Some Side.Ours) 99_511 3_000_000 { X = 2; Y = 19 } with
                    NextDecay = 994_965
                }
                { structureOf "rampart" (Some Side.Ours) 99_431 3_000_000 { X = 2; Y = 20 } with
                    NextDecay = 994_967
                }
                { structureOf "rampart" (Some Side.Ours) 99_221 3_000_000 { X = 2; Y = 23 } with
                    NextDecay = 994_992
                }
                { structureOf "rampart" (Some Side.Ours) 98_601 3_000_000 { X = 2; Y = 24 } with
                    NextDecay = 994_954
                }
                { structureOf "rampart" (Some Side.Ours) 96_901 3_000_000 { X = 2; Y = 25 } with
                    NextDecay = 994_971
                }
                { structureOf "rampart" (Some Side.Ours) 97_761 3_000_000 { X = 2; Y = 26 } with
                    NextDecay = 994_986
                }
                { structureOf "rampart" (Some Side.Ours) 90_461 3_000_000 { X = 2; Y = 27 } with
                    NextDecay = 994_935
                }
                { structureOf "rampart" (Some Side.Ours) 99_501 3_000_000 { X = 13; Y = 27 } with
                    NextDecay = 994_985
                }
                { structureOf "rampart" (Some Side.Ours) 97_701 3_000_000 { X = 13; Y = 29 } with
                    NextDecay = 994_970
                }
                { structureOf "rampart" (Some Side.Ours) 98_301 3_000_000 { X = 14; Y = 28 } with
                    NextDecay = 994_946
                }
                { structureOf "rampart" (Some Side.Ours) 100_221 3_000_000 { X = 16; Y = 44 } with
                    NextDecay = 994_934
                }
                { structureOf "rampart" (Some Side.Ours) 98_101 3_000_000 { X = 17; Y = 20 } with
                    NextDecay = 994_991
                }
                { structureOf "rampart" (Some Side.Ours) 100_201 3_000_000 { X = 17; Y = 44 } with
                    NextDecay = 994_936
                }
                { structureOf "rampart" (Some Side.Ours) 100_001 3_000_000 { X = 18; Y = 44 } with
                    NextDecay = 994_932
                }
                { structureOf "rampart" (Some Side.Ours) 96_101 3_000_000 { X = 32; Y = 31 } with
                    NextDecay = 994_945
                }
                { structureOf "rampart" (Some Side.Ours) 96_971 3_000_000 { X = 32; Y = 32 } with
                    NextDecay = 994_963
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 10_575
                }
                towerOf Side.Ours { X = 13; Y = 29 } 980
            ]
        |> withRoads
            [
                1, 16, 4_300
                2, 17, 3_800
                3, 18, 4_300
                4, 19, 3_100
                5, 20, 3_000
                6, 21, 3_700
                7, 22, 15_500
                8, 23, 13_800
                9, 24, 12_900
                10, 25, 17_200
                10, 27, 14_900
                10, 29, 12_000
                11, 24, 18_200
                11, 26, 12_500
                11, 28, 19_300
                11, 30, 2_400
                12, 23, 12_900
                12, 25, 15_800
                12, 27, 12_400
                12, 29, 4_300
                13, 24, 3_900
                13, 26, 4_100
                13, 28, 2_700
                13, 30, 4_500
                14, 23, 3_900
                14, 25, 3_600
                14, 27, 3_900
                14, 29, 4_400
                14, 31, 4_300
                15, 22, 2_500
                15, 28, 2_800
                15, 30, 3_600
                15, 32, 4_200
                16, 21, 2_400
                16, 29, 3_600
                16, 31, 3_800
                16, 33, 3_700
                16, 35, 4_300
                16, 37, 3_600
                16, 38, 19_100
                16, 39, 20_100
                16, 41, 3_800
                16, 43, 3_600
                16, 44, 3_600
                17, 20, 2_500
                17, 30, 3_700
                17, 32, 4_400
                17, 34, 3_800
                17, 36, 2_800
                17, 38, 4_200
                17, 39, 20_000
                17, 40, 14_600
                17, 42, 4_000
                17, 45, 2_500
                18, 29, 4_400
                18, 31, 4_800
                18, 35, 3_300
                18, 37, 4_300
                18, 39, 2_500
                18, 41, 3_100
                18, 46, 3_000
                19, 30, 4_200
                19, 34, 2_400
                19, 36, 3_700
                19, 38, 2_600
                19, 40, 2_500
                19, 47, 2_600
                20, 31, 3_300
                20, 33, 2_700
                20, 35, 2_500
                20, 37, 4_300
                20, 39, 2_600
                20, 48, 3_500
                21, 32, 4_300
                21, 34, 3_800
                21, 36, 2_600
                21, 38, 3_800
                22, 33, 4_400
                22, 35, 2_600
                22, 37, 4_500
                23, 34, 4_400
                23, 36, 2_500
                24, 35, 2_600
                25, 34, 3_400
                26, 33, 3_800
                27, 32, 3_500
                28, 31, 15_000
                28, 33, 17_600
                29, 32, 15_000
                29, 34, 20_100
                30, 31, 15_500
                30, 33, 19_900
                31, 32, 19_400
                32, 31, 16_000
                33, 30, 15_840
                33, 32, 13_810
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 6_126_063
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 30
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 27
                        13, 29
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        15, 35
                        16, 26
                        16, 32
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        19, 27
                        19, 31
                        19, 33
                        20, 26
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        21, 35
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 125_700 250_000 { X = 6; Y = 29 } with
                    Energy = 648
                }
                { structureOf "container" None 196_100 250_000 { X = 11; Y = 18 } with
                    Energy = 612
                }
                { structureOf "container" None 147_500 250_000 { X = 23; Y = 29 } with
                    Energy = 358
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 18; Y = 34 } with
                    Energy = 50
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 98_701 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 995_015
                }
                { structureOf "rampart" (Some Side.Ours) 102_301 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 994_975
                }
                { structureOf "rampart" (Some Side.Ours) 101_901 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 994_934
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 994_936
                }
                { structureOf "rampart" (Some Side.Ours) 101_001 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 994_976
                }
                { structureOf "rampart" (Some Side.Ours) 102_101 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 994_930
                }
                { structureOf "rampart" (Some Side.Ours) 100_001 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 994_951
                }
                { structureOf "rampart" (Some Side.Ours) 101_001 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 994_999
                }
                { structureOf "rampart" (Some Side.Ours) 101_401 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 994_992
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 30_074
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]
        |> withRoads
            [
                4, 5, 4_400
                5, 4, 3_200
                5, 6, 4_400
                5, 44, 3_500
                6, 3, 4_900
                6, 7, 3_700
                6, 29, 3_000
                6, 43, 3_100
                6, 45, 4_000
                7, 2, 3_900
                7, 8, 4_400
                7, 28, 4_000
                7, 42, 2_900
                7, 46, 3_400
                8, 1, 4_900
                8, 9, 2_600
                8, 28, 2_700
                8, 41, 3_100
                8, 47, 2_600
                9, 10, 3_700
                9, 27, 3_500
                9, 28, 4_400
                9, 40, 2_900
                9, 48, 3_400
                10, 11, 2_800
                10, 26, 4_000
                10, 29, 3_900
                10, 39, 3_200
                11, 12, 4_900
                11, 18, 2_600
                11, 27, 2_600
                11, 30, 4_100
                11, 38, 3_700
                12, 13, 3_300
                12, 17, 3_700
                12, 25, 17_600
                12, 27, 18_500
                12, 28, 4_400
                12, 29, 3_900
                12, 31, 2_800
                12, 33, 14_700
                12, 35, 18_000
                12, 37, 3_100
                13, 14, 3_400
                13, 16, 3_000
                13, 26, 14_300
                13, 28, 2_700
                13, 30, 4_200
                13, 32, 4_800
                13, 34, 3_100
                13, 36, 3_300
                14, 15, 3_800
                14, 25, 17_700
                14, 27, 4_800
                14, 29, 3_300
                14, 31, 3_500
                14, 33, 4_200
                14, 35, 3_600
                15, 15, 3_200
                15, 26, 4_400
                15, 28, 2_900
                15, 30, 4_900
                15, 32, 3_300
                15, 34, 3_200
                16, 16, 3_200
                16, 25, 20_600
                16, 27, 4_200
                16, 29, 4_900
                16, 31, 3_300
                16, 33, 2_700
                16, 35, 2_800
                17, 17, 4_200
                17, 26, 18_400
                17, 28, 4_300
                17, 30, 4_200
                17, 32, 3_900
                17, 34, 3_400
                18, 18, 2_700
                18, 25, 18_700
                18, 27, 18_000
                18, 29, 3_300
                18, 31, 3_000
                18, 33, 3_500
                19, 19, 13_200
                19, 22, 18_100
                19, 24, 14_000
                19, 26, 19_500
                19, 28, 4_600
                19, 30, 3_200
                19, 32, 4_100
                20, 20, 13_500
                20, 21, 19_100
                20, 23, 19_300
                20, 25, 18_900
                20, 27, 14_200
                20, 29, 3_400
                20, 31, 2_600
                20, 33, 20_200
                21, 21, 4_000
                21, 22, 4_700
                21, 23, 4_900
                21, 24, 19_600
                21, 25, 15_600
                21, 26, 20_900
                21, 28, 16_500
                21, 30, 3_800
                21, 32, 15_000
                21, 34, 21_300
                22, 24, 4_200
                22, 25, 15_800
                22, 27, 18_400
                22, 28, 3_800
                22, 29, 3_200
                22, 30, 21_100
                22, 31, 16_300
                22, 32, 18_100
                22, 33, 15_700
                22, 35, 3_800
                23, 26, 17_800
                23, 28, 14_500
                23, 29, 16_700
                23, 30, 17_400
                23, 31, 19_600
                23, 32, 17_400
                23, 33, 21_300
                23, 34, 4_500
                24, 25, 14_600
                24, 28, 18_000
                24, 29, 19_100
                24, 30, 16_900
                24, 33, 13_000
                25, 24, 4_100
                25, 28, 18_700
                25, 29, 18_800
                25, 34, 19_900
                26, 23, 18_000
                27, 22, 15_200
                28, 21, 14_900
                28, 34, 9_100
                29, 20, 16_700
                30, 19, 14_600
                31, 18, 3_600
                32, 17, 3_500
                33, 16, 5_000
                34, 15, 2_900
                35, 14, 3_700
                36, 13, 18_600
                37, 12, 3_800
                38, 11, 2_600
                39, 12, 3_600
                40, 13, 16_400
                41, 14, 14_700
                42, 15, 4_900
                43, 16, 5_000
                44, 17, 3_700
                45, 18, 3_700
                46, 19, 3_700
                47, 20, 2_500
                48, 21, 3_400
            ]

    let w19s25 =
        room "W19S25"
        |> withSite trepidimous "road" None 0 { X = 9; Y = 37 }
        |> withSite trepidimous "road" None 0 { X = 10; Y = 37 }
        |> withSite trepidimous "road" None 0 { X = 11; Y = 37 }
        |> withSite trepidimous "road" None 0 { X = 12; Y = 37 }

    let w17s29 =
        room "W17S29"
        |> withController Ownership.Ours None 6 4
        |> withControllerClocks 0 0 120_000 1_455_751
        |> withSpawn "Spawn9" { X = 24; Y = 40 } 2_300
        |> withStructures
            [
                for x, y in
                    [
                        19, 35
                        19, 41
                        19, 43
                        20, 36
                        20, 38
                        20, 40
                        20, 42
                        20, 44
                        21, 37
                        21, 41
                        21, 43
                        21, 45
                        22, 36
                        22, 44
                        23, 37
                        23, 43
                        23, 45
                        24, 36
                        24, 42
                        24, 44
                        25, 35
                        25, 37
                        25, 43
                        25, 45
                        26, 36
                        26, 38
                        26, 44
                        27, 35
                        27, 43
                        27, 45
                        28, 44
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in [ 22, 38; 22, 40; 22, 42; 24, 38; 25, 39; 25, 41; 26, 40 ] ->
                    structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 150_200 250_000 { X = 13; Y = 29 } with
                    Energy = 732
                }
                structureOf "container" None 150_500 250_000 { X = 15; Y = 43 }
                { structureOf "container" None 120_600 250_000 { X = 27; Y = 42 } with
                    Energy = 1_950
                }
                { structureOf "container" None 125_000 250_000 { X = 44; Y = 38 } with
                    Energy = 704
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 21; Y = 35 } with
                    Energy = 50
                    Id = "extension-21-35-W17S29"
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 23; Y = 35 } with
                    Energy = 50
                    Id = "extension-23-35-W17S29"
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 16; Y = 44 }
                { structureOf "rampart" (Some Side.Ours) 99_001 30_000_000 { X = 13; Y = 29 } with
                    Id = "rampart-13-29-W17S29"
                    NextDecay = 994_981
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 30_000_000 { X = 21; Y = 39 } with
                    NextDecay = 994_992
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 30_000_000 { X = 23; Y = 39 } with
                    NextDecay = 994_928
                }
                { structureOf "rampart" (Some Side.Ours) 101_201 30_000_000 { X = 23; Y = 41 } with
                    NextDecay = 994_971
                }
                { structureOf "rampart" (Some Side.Ours) 101_601 30_000_000 { X = 24; Y = 40 } with
                    NextDecay = 994_970
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 30_000_000 { X = 26; Y = 42 } with
                    NextDecay = 994_976
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 30_000_000 { X = 44; Y = 38 } with
                    NextDecay = 994_925
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 23; Y = 39 } with
                    Energy = 24_518
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 26; Y = 42 } with
                    Energy = 4_000
                }
                towerOf Side.Ours { X = 21; Y = 39 } 1_000
                towerOf Side.Ours { X = 23; Y = 41 } 1_000
            ]
        |> withRoads
            [
                13, 29, 4_600
                14, 29, 3_200
                15, 30, 3_100
                16, 31, 3_200
                16, 42, 4_600
                17, 32, 3_400
                17, 42, 15_900
                18, 33, 4_300
                18, 41, 12_800
                19, 34, 19_700
                19, 40, 3_400
                19, 42, 4_500
                20, 35, 15_800
                20, 37, 13_200
                20, 39, 3_300
                20, 41, 3_400
                20, 43, 4_100
                21, 36, 4_300
                21, 38, 2_600
                21, 40, 3_800
                21, 42, 4_300
                21, 44, 4_600
                22, 35, 4_000
                22, 37, 3_200
                22, 39, 2_700
                22, 41, 3_700
                22, 43, 3_770
                22, 45, 2_560
                23, 36, 2_600
                23, 38, 4_200
                23, 42, 3_300
                23, 44, 4_070
                24, 35, 13_200
                24, 37, 3_100
                24, 39, 3_300
                24, 41, 3_300
                24, 43, 2_700
                24, 45, 3_070
                25, 36, 17_000
                25, 38, 3_300
                25, 40, 2_600
                25, 42, 3_000
                25, 44, 2_800
                26, 35, 18_200
                26, 37, 16_100
                26, 39, 12_700
                26, 41, 3_700
                26, 43, 3_700
                26, 45, 3_800
                27, 36, 12_500
                27, 37, 15_000
                27, 38, 13_600
                27, 39, 16_800
                27, 40, 13_900
                27, 41, 3_900
                27, 42, 3_000
                27, 44, 2_500
                28, 36, 12_600
                28, 37, 14_400
                28, 38, 13_600
                28, 39, 13_900
                28, 40, 18_500
                28, 41, 3_900
                28, 43, 2_900
                29, 35, 3_100
                29, 36, 14_900
                29, 37, 12_900
                29, 38, 19_100
                29, 39, 12_800
                29, 40, 15_000
                29, 41, 15_700
                29, 42, 3_700
                30, 34, 2_700
                30, 36, 14_900
                30, 37, 18_200
                30, 40, 13_800
                30, 41, 13_200
                30, 42, 14_800
                31, 33, 2_400
                31, 36, 16_600
                31, 41, 14_300
                31, 42, 17_100
                32, 32, 3_700
                32, 36, 14_300
                32, 41, 4_300
                33, 31, 2_700
                33, 36, 18_900
                33, 37, 20_300
                33, 40, 4_000
                34, 30, 3_300
                34, 39, 4_200
                35, 29, 4_100
                35, 38, 3_200
                36, 28, 2_600
                36, 37, 4_300
                37, 27, 3_900
                37, 38, 4_000
                38, 26, 4_000
                38, 39, 3_800
                39, 25, 3_500
                39, 40, 2_700
                40, 24, 2_500
                40, 41, 4_100
                41, 23, 3_900
                41, 40, 4_200
                42, 24, 4_300
                42, 39, 4_300
                43, 25, 2_600
                43, 38, 2_300
                44, 25, 4_300
                44, 37, 3_400
                44, 38, 2_400
                45, 26, 2_900
                45, 36, 3_000
                46, 26, 4_400
                46, 35, 3_400
                47, 25, 4_500
                47, 36, 2_500
                48, 20, 4_600
                48, 21, 4_400
                48, 22, 4_400
                48, 23, 4_540
                48, 24, 3_300
                48, 37, 3_200
            ]

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 139_900 250_000 { X = 27; Y = 9 } with
                    Energy = 482
                }
            ]
        |> withRoads
            [
                20, 1, 3_000
                21, 2, 4_480
                22, 3, 2_600
                23, 4, 3_100
                24, 5, 3_500
                25, 6, 3_100
                26, 7, 4_100
                27, 8, 2_500
            ]

    let w19s26 = room "W19S26" |> withRoads [ 9, 43, 700; 27, 27, 300 ]

    let w13s28 =
        room "W13S28"
        |> withController Ownership.Ours None 8 6
        |> withControllerClocks 0 0 200_000 0
        |> withSpawn "Spawn2" { X = 16; Y = 12 } 12_900
        |> withSpawn "Spawn7" { X = 10; Y = 6 } 12_900
        |> withSpawn "Spawn4" { X = 10; Y = 8 } 12_900
        |> withStructures
            [
                for x, y in
                    [
                        9, 5
                        9, 11
                        9, 13
                        9, 15
                        9, 17
                        9, 19
                        10, 12
                        10, 14
                        10, 16
                        10, 18
                        11, 7
                        11, 9
                        11, 11
                        11, 13
                        11, 15
                        11, 17
                        11, 19
                        12, 8
                        12, 10
                        12, 12
                        12, 14
                        12, 16
                        12, 18
                        13, 9
                        13, 11
                        13, 13
                        13, 15
                        13, 17
                        13, 19
                        14, 12
                        14, 14
                        14, 16
                        14, 18
                        15, 15
                        15, 17
                        15, 19
                        16, 10
                        16, 14
                        16, 16
                        16, 18
                        17, 9
                        17, 13
                        17, 15
                        17, 19
                        18, 10
                        18, 12
                        18, 14
                        19, 9
                        19, 11
                        19, 13
                        19, 15
                        20, 8
                        20, 10
                        20, 12
                        20, 14
                        21, 5
                        21, 9
                        21, 11
                        22, 10
                        22, 12
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 200
                    }
            ]
        |> withStructures
            [
                for x, y in [ 9, 7; 9, 9; 10, 10; 15, 13; 17, 11; 21, 13 ] ->
                    towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                structureOf "container" None 205_000 250_000 { X = 6; Y = 25 }
                structureOf "container" None 127_500 250_000 { X = 15; Y = 8 }
                { structureOf "container" None 155_000 250_000 { X = 18; Y = 3 } with
                    Energy = 180
                }
                structureOf "container" None 141_700 250_000 { X = 22; Y = 15 }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 7; Y = 25 }
                structureOf "observer" (Some Side.Ours) 500 500 { X = 23; Y = 5 }
                { structureOf "rampart" (Some Side.Ours) 106_061 300_000_000 { X = 9; Y = 7 } with
                    NextDecay = 994_953
                }
                { structureOf "rampart" (Some Side.Ours) 101_561 300_000_000 { X = 9; Y = 9 } with
                    NextDecay = 994_986
                }
                { structureOf "rampart" (Some Side.Ours) 101_401 300_000_000 { X = 10; Y = 6 } with
                    NextDecay = 994_962
                }
                { structureOf "rampart" (Some Side.Ours) 101_381 300_000_000 { X = 10; Y = 8 } with
                    NextDecay = 994_977
                }
                { structureOf "rampart" (Some Side.Ours) 102_381 300_000_000 { X = 10; Y = 10 } with
                    NextDecay = 994_999
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 300_000_000 { X = 14; Y = 10 } with
                    NextDecay = 994_943
                }
                { structureOf "rampart" (Some Side.Ours) 101_871 300_000_000 { X = 15; Y = 8 } with
                    NextDecay = 994_934
                }
                { structureOf "rampart" (Some Side.Ours) 101_601 300_000_000 { X = 15; Y = 11 } with
                    NextDecay = 994_963
                }
                { structureOf "rampart" (Some Side.Ours) 102_901 300_000_000 { X = 15; Y = 13 } with
                    NextDecay = 994_972
                }
                { structureOf "rampart" (Some Side.Ours) 101_501 300_000_000 { X = 16; Y = 12 } with
                    NextDecay = 994_990
                }
                { structureOf "rampart" (Some Side.Ours) 101_101 300_000_000 { X = 17; Y = 11 } with
                    NextDecay = 994_971
                }
                { structureOf "rampart" (Some Side.Ours) 100_381 300_000_000 { X = 18; Y = 3 } with
                    NextDecay = 995_006
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 300_000_000 { X = 21; Y = 13 } with
                    NextDecay = 994_923
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 15; Y = 11 } with
                    Energy = 69_085
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 14; Y = 10 } with
                    Energy = 5_602
                }
            ]
        |> withRoads
            [
                8, 5, 4_000
                9, 4, 4_400
                9, 6, 18_900
                9, 8, 13_100
                9, 10, 14_100
                9, 12, 14_100
                9, 14, 19_600
                9, 16, 3_500
                9, 18, 3_500
                10, 3, 3_700
                10, 4, 3_400
                10, 5, 2_800
                10, 7, 21_200
                10, 9, 19_700
                10, 11, 17_800
                10, 13, 16_400
                10, 15, 4_000
                10, 17, 4_000
                10, 19, 3_900
                11, 2, 4_200
                11, 3, 4_600
                11, 6, 19_700
                11, 8, 20_900
                11, 10, 14_500
                11, 12, 19_800
                11, 14, 15_900
                11, 16, 4_800
                11, 18, 3_700
                12, 1, 3_000
                12, 2, 4_800
                12, 7, 21_100
                12, 9, 15_600
                12, 11, 18_900
                12, 13, 20_100
                12, 15, 4_200
                12, 17, 4_900
                12, 19, 3_900
                13, 1, 4_200
                13, 7, 12_600
                13, 8, 14_200
                13, 10, 14_300
                13, 12, 13_800
                13, 14, 4_200
                13, 16, 3_700
                13, 18, 3_100
                14, 1, 3_100
                14, 7, 19_400
                14, 8, 12_400
                14, 9, 17_900
                14, 11, 20_100
                14, 13, 3_000
                14, 15, 2_500
                14, 17, 2_800
                14, 19, 2_800
                15, 1, 5_000
                15, 8, 21_300
                15, 9, 17_900
                15, 10, 20_600
                15, 12, 5_000
                15, 14, 2_900
                15, 16, 4_100
                15, 18, 3_700
                15, 20, 2_600
                15, 22, 2_500
                15, 24, 4_400
                15, 26, 3_200
                15, 28, 2_500
                15, 30, 2_600
                15, 32, 3_200
                15, 34, 2_600
                16, 1, 2_600
                16, 9, 4_500
                16, 11, 4_800
                16, 13, 4_100
                16, 15, 4_600
                16, 17, 3_100
                16, 19, 2_500
                16, 21, 2_500
                16, 23, 4_200
                16, 25, 2_500
                16, 27, 4_000
                16, 29, 4_800
                16, 31, 2_600
                16, 33, 3_700
                16, 35, 4_900
                17, 2, 3_700
                17, 10, 3_300
                17, 12, 4_200
                17, 14, 4_100
                17, 16, 2_700
                17, 36, 3_300
                17, 48, 3_500
                18, 1, 3_400
                18, 2, 3_400
                18, 3, 4_200
                18, 9, 2_900
                18, 11, 3_300
                18, 13, 3_200
                18, 15, 4_700
                18, 37, 2_700
                18, 39, 2_900
                18, 41, 4_000
                18, 43, 4_400
                18, 45, 3_500
                18, 47, 3_700
                19, 2, 4_400
                19, 3, 17_900
                19, 8, 3_900
                19, 10, 4_700
                19, 12, 5_000
                19, 14, 3_600
                19, 38, 2_600
                19, 40, 2_600
                19, 42, 2_700
                19, 44, 2_900
                19, 46, 4_800
                20, 1, 3_900
                20, 4, 21_600
                20, 9, 4_100
                20, 11, 5_000
                20, 13, 4_000
                20, 15, 4_500
                21, 10, 18_200
                21, 12, 2_900
                21, 14, 4_200
                22, 11, 3_900
                22, 13, 3_500
                22, 15, 3_700
                22, 18, 15_900
                23, 14, 4_500
                23, 18, 15_700
                23, 19, 12_900
                24, 18, 14_500
                24, 19, 20_200
            ]

    let w12s28 =
        room "W12S28"
        |> withController Ownership.Ours None 8 7
        |> withControllerClocks 0 0 200_000 0
        |> withSpawn "Spawn1" { X = 12; Y = 40 } 12_900
        |> withSpawn "Spawn6" { X = 18; Y = 34 } 12_900
        |> withSpawn "Spawn5" { X = 18; Y = 36 } 12_900
        |> withStructures
            [
                for x, y in
                    [
                        4, 36
                        7, 37
                        8, 32
                        8, 34
                        8, 36
                        9, 33
                        9, 35
                        9, 37
                        9, 39
                        9, 41
                        10, 32
                        10, 34
                        10, 38
                        10, 42
                        11, 33
                        11, 37
                        11, 39
                        11, 41
                        12, 38
                        12, 42
                        12, 44
                        13, 37
                        13, 39
                        13, 41
                        13, 43
                        14, 36
                        14, 38
                        14, 40
                        14, 44
                        15, 37
                        15, 39
                        15, 43
                        15, 45
                        16, 36
                        16, 38
                        16, 44
                        17, 35
                        17, 37
                        17, 43
                        17, 45
                        18, 42
                        18, 44
                        18, 46
                        19, 39
                        19, 45
                        19, 47
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 200
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 32
                        12, 34
                        13, 33
                        13, 35
                        14, 32
                        14, 34
                        15, 33
                        15, 35
                        16, 34
                        17, 33
                        19, 33
                        19, 35
                        19, 41
                        19, 43
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 200
                        Id = $"extension-{x}-{y}-W12S28"
                    }
            ]
        |> withStructures
            [
                for x, y in [ 5, 37; 10, 36; 10, 40; 16, 46; 18, 38; 19, 37 ] ->
                    towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 203_900 250_000 { X = 7; Y = 39 } with
                    Energy = 1_985
                }
                structureOf "container" None 205_000 250_000 { X = 10; Y = 8 }
                { structureOf "container" None 136_900 250_000 { X = 10; Y = 43 } with
                    Energy = 1_308
                }
                { structureOf "container" None 130_400 250_000 { X = 16; Y = 39 } with
                    Energy = 1_308
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 11; Y = 7 }
                structureOf "observer" (Some Side.Ours) 500 500 { X = 16; Y = 32 }
                { structureOf "rampart" (Some Side.Ours) 100_661 300_000_000 { X = 5; Y = 37 } with
                    NextDecay = 994_924
                }
                { structureOf "rampart" (Some Side.Ours) 103_801 300_000_000 { X = 10; Y = 36 } with
                    NextDecay = 994_920
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 300_000_000 { X = 10; Y = 40 } with
                    NextDecay = 994_946
                }
                { structureOf "rampart" (Some Side.Ours) 101_101 300_000_000 { X = 10; Y = 43 } with
                    NextDecay = 995_001
                }
                { structureOf "rampart" (Some Side.Ours) 99_801 300_000_000 { X = 11; Y = 43 } with
                    NextDecay = 994_995
                }
                { structureOf "rampart" (Some Side.Ours) 101_301 300_000_000 { X = 12; Y = 40 } with
                    NextDecay = 994_999
                }
                { structureOf "rampart" (Some Side.Ours) 102_701 300_000_000 { X = 14; Y = 42 } with
                    NextDecay = 994_995
                }
                { structureOf "rampart" (Some Side.Ours) 102_211 300_000_000 { X = 16; Y = 39 } with
                    NextDecay = 994_943
                }
                { structureOf "rampart" (Some Side.Ours) 103_001 300_000_000 { X = 16; Y = 46 } with
                    NextDecay = 994_976
                }
                { structureOf "rampart" (Some Side.Ours) 101_301 300_000_000 { X = 18; Y = 34 } with
                    NextDecay = 995_008
                }
                { structureOf "rampart" (Some Side.Ours) 102_841 300_000_000 { X = 18; Y = 36 } with
                    NextDecay = 994_942
                }
                { structureOf "rampart" (Some Side.Ours) 103_021 300_000_000 { X = 18; Y = 38 } with
                    NextDecay = 994_963
                }
                { structureOf "rampart" (Some Side.Ours) 99_861 300_000_000 { X = 19; Y = 37 } with
                    NextDecay = 995_001
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 14; Y = 42 } with
                    Energy = 70_935
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 11; Y = 43 } with
                    Energy = 4_001
                }
            ]
        |> withRoads
            [
                1, 21, 3_900
                1, 22, 3_300
                2, 20, 3_900
                2, 23, 4_500
                2, 39, 17_000
                3, 19, 3_700
                3, 24, 2_900
                3, 39, 15_800
                4, 18, 4_000
                4, 25, 3_800
                4, 39, 15_000
                4, 40, 12_800
                5, 17, 4_100
                5, 26, 3_700
                5, 40, 17_200
                6, 6, 3_000
                6, 7, 3_800
                6, 8, 3_800
                6, 9, 4_000
                6, 10, 3_800
                6, 11, 3_800
                6, 12, 2_600
                6, 13, 3_100
                6, 14, 2_900
                6, 15, 3_000
                6, 16, 2_900
                6, 27, 18_700
                6, 40, 18_500
                6, 41, 16_300
                7, 5, 3_800
                7, 28, 2_500
                7, 39, 3_100
                7, 40, 15_800
                7, 41, 16_700
                8, 4, 2_800
                8, 29, 4_100
                8, 33, 4_800
                8, 35, 16_700
                8, 37, 3_300
                8, 38, 4_300
                8, 39, 3_600
                8, 41, 4_000
                9, 3, 4_700
                9, 30, 2_900
                9, 32, 3_100
                9, 34, 3_300
                9, 36, 4_300
                9, 38, 4_600
                9, 40, 4_400
                9, 42, 5_000
                10, 2, 3_800
                10, 31, 4_100
                10, 33, 4_200
                10, 35, 3_000
                10, 37, 3_200
                10, 39, 3_300
                10, 41, 4_300
                10, 43, 5_000
                11, 1, 4_100
                11, 32, 19_500
                11, 34, 4_100
                11, 38, 2_900
                11, 40, 3_300
                11, 42, 3_900
                12, 33, 4_500
                12, 37, 3_400
                12, 39, 3_200
                12, 41, 4_900
                12, 43, 21_300
                13, 32, 19_500
                13, 34, 2_500
                13, 36, 4_500
                13, 38, 4_900
                13, 40, 2_500
                13, 42, 13_200
                13, 44, 16_800
                14, 33, 4_600
                14, 35, 2_600
                14, 37, 4_100
                14, 39, 4_900
                14, 41, 4_500
                14, 43, 4_200
                14, 45, 3_100
                15, 32, 4_000
                15, 34, 4_000
                15, 36, 4_700
                15, 38, 3_300
                15, 40, 22_000
                15, 44, 3_600
                16, 33, 4_100
                16, 35, 4_900
                16, 37, 3_100
                16, 39, 3_100
                16, 43, 2_600
                16, 45, 3_400
                17, 34, 4_200
                17, 36, 4_800
                17, 38, 4_300
                17, 42, 16_600
                17, 44, 19_500
                17, 46, 4_300
                18, 33, 4_300
                18, 35, 2_800
                18, 37, 4_000
                18, 39, 2_400
                18, 41, 4_000
                18, 43, 2_700
                18, 45, 4_400
                19, 34, 3_400
                19, 36, 4_100
                19, 38, 4_200
                19, 40, 3_300
                19, 42, 21_000
                19, 44, 21_000
                19, 46, 4_200
                20, 40, 3_500
                21, 41, 3_700
                22, 42, 2_700
                23, 43, 3_600
                24, 44, 4_500
                25, 45, 4_700
                26, 46, 3_600
                27, 45, 4_600
                28, 44, 2_500
                29, 45, 4_700
                30, 46, 4_600
                31, 47, 3_800
                32, 48, 3_900
                33, 47, 2_800
                34, 46, 3_800
                35, 45, 4_800
                36, 44, 4_100
                37, 43, 4_200
                38, 42, 3_600
                39, 41, 3_800
                40, 40, 3_700
                41, 39, 5_000
                42, 38, 3_600
                43, 37, 4_900
                44, 36, 3_700
                45, 35, 4_700
                46, 34, 2_900
                47, 33, 3_800
                48, 32, 4_700
            ]

    let w12s26 =
        room "W12S26"
        |> withController Ownership.Ours None 5 4
        |> withControllerClocks 0 0 80_000 34_582
        |> withSpawn "Spawn10" { X = 37; Y = 34 } 1_800
        |> withStructures
            [
                for x, y in
                    [
                        30, 33
                        31, 34
                        31, 36
                        31, 38
                        31, 40
                        32, 33
                        32, 35
                        32, 37
                        33, 34
                        33, 36
                        34, 33
                        34, 35
                        35, 28
                        35, 30
                        35, 34
                        35, 36
                        36, 29
                        36, 31
                        37, 28
                        37, 30
                        37, 32
                        37, 36
                        38, 29
                        38, 31
                        38, 35
                        39, 30
                        39, 36
                        42, 37
                        43, 38
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                { structureOf "container" None 152_200 250_000 { X = 6; Y = 34 } with
                    Energy = 419
                }
                { structureOf "container" None 125_000 250_000 { X = 42; Y = 39 } with
                    Energy = 960
                }
                structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 43; Y = 40 }
                { structureOf "rampart" (Some Side.Ours) 101_401 10_000_000 { X = 36; Y = 33 } with
                    NextDecay = 994_957
                }
                { structureOf "rampart" (Some Side.Ours) 85_801 10_000_000 { X = 36; Y = 35 } with
                    NextDecay = 994_996
                }
                { structureOf "rampart" (Some Side.Ours) 84_901 10_000_000 { X = 37; Y = 34 } with
                    NextDecay = 994_952
                }
                { structureOf "rampart" (Some Side.Ours) 67_701 10_000_000 { X = 40; Y = 37 } with
                    NextDecay = 994_920
                }
                { structureOf "rampart" (Some Side.Ours) 85_501 10_000_000 { X = 42; Y = 39 } with
                    NextDecay = 994_929
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 36; Y = 33 } with
                    Energy = 9_942
                }
                towerOf Side.Ours { X = 36; Y = 35 } 1_000
                towerOf Side.Ours { X = 40; Y = 37 } 1_000
            ]
        |> withRoads
            [
                1, 36, 3_000
                2, 37, 3_000
                3, 38, 3_000
                4, 38, 3_700
                4, 39, 4_900
                5, 37, 3_100
                5, 40, 3_000
                5, 41, 2_500
                6, 35, 2_900
                6, 36, 20_600
                6, 42, 2_500
                7, 43, 3_100
                8, 44, 2_500
                9, 45, 4_400
                10, 46, 2_500
                11, 46, 4_900
                12, 47, 4_800
                13, 48, 3_800
                14, 47, 4_800
                15, 46, 4_800
                16, 45, 3_300
                17, 45, 3_800
                18, 44, 3_500
                19, 4, 3_200
                19, 5, 3_300
                19, 43, 4_900
                20, 2, 3_200
                20, 3, 3_200
                20, 6, 3_300
                20, 42, 2_400
                21, 1, 3_200
                21, 7, 3_300
                21, 41, 1_700
                22, 8, 3_200
                22, 42, 2_000
                23, 9, 3_200
                23, 13, 3_200
                23, 43, 2_100
                24, 10, 3_200
                24, 11, 3_200
                24, 12, 3_200
                24, 14, 3_200
                24, 43, 2_600
                25, 15, 3_200
                25, 43, 1_600
                26, 16, 3_200
                26, 42, 2_000
                27, 17, 3_100
                27, 43, 2_400
                28, 18, 3_100
                28, 42, 2_500
                29, 19, 3_100
                29, 41, 2_700
                30, 20, 3_100
                30, 40, 2_500
                31, 21, 3_100
                31, 33, 4_800
                31, 35, 24_000
                31, 37, 4_600
                31, 39, 2_500
                32, 22, 3_300
                32, 34, 4_700
                32, 36, 4_600
                32, 38, 2_300
                33, 23, 3_300
                33, 33, 4_700
                33, 35, 4_700
                33, 37, 3_100
                34, 24, 3_300
                34, 32, 4_700
                34, 34, 4_700
                34, 36, 3_100
                35, 25, 3_300
                35, 27, 2_900
                35, 29, 2_800
                35, 31, 4_200
                35, 33, 4_600
                35, 35, 2_100
                36, 26, 3_300
                36, 28, 3_000
                36, 30, 2_500
                36, 32, 4_000
                36, 34, 3_300
                36, 36, 4_800
                37, 29, 4_800
                37, 31, 4_700
                37, 33, 3_200
                37, 35, 4_700
                38, 30, 4_800
                38, 34, 2_400
                38, 36, 4_800
                39, 35, 3_700
                40, 36, 3_800
                41, 37, 3_600
                42, 38, 2_000
                42, 39, 2_700
                43, 39, 4_800
            ]

    let w18s27 = room "W18S27"

    let w18s28 =
        room "W18S28"
        |> withSite giaco "road" None 69 { X = 24; Y = 46 }
        |> withSite giaco "road" None 0 { X = 17; Y = 43 }
        |> withSite giaco "road" None 295 { X = 15; Y = 43 }

    let w18s29 =
        room "W18S29"
        |> withStructures
            [
                { structureOf "container" None 201_100 250_000 { X = 10; Y = 6 } with
                    Energy = 1_074
                }
                structureOf "container" None 190_700 250_000 { X = 37; Y = 37 }
            ]
        |> withRoads
            [
                2, 18, 3_050
                2, 19, 3_050
                2, 20, 3_650
                2, 22, 3_870
                2, 24, 3_600
                2, 25, 4_220
                2, 26, 3_900
                3, 15, 3_650
                3, 17, 2_920
                3, 21, 3_440
                3, 27, 4_000
                3, 28, 4_600
                4, 14, 3_670
                4, 16, 3_290
                5, 13, 3_700
                6, 12, 4_000
                7, 11, 4_150
                7, 13, 3_790
                8, 8, 4_200
                8, 10, 3_650
                8, 14, 4_250
                9, 7, 4_200
                9, 9, 3_900
                9, 15, 4_090
                10, 16, 4_000
                11, 17, 4_500
                12, 18, 4_500
                13, 19, 4_500
                13, 37, 4_900
                13, 39, 4_900
                14, 20, 4_500
                14, 36, 4_800
                14, 38, 4_900
                14, 40, 4_900
                15, 21, 4_500
                15, 35, 4_900
                15, 41, 4_900
                16, 22, 4_500
                16, 34, 4_900
                16, 42, 4_900
                17, 23, 4_500
                17, 33, 4_900
                17, 43, 4_900
                17, 47, 5_000
                18, 24, 4_900
                18, 32, 4_900
                18, 44, 5_000
                18, 46, 4_900
                18, 48, 5_000
                19, 25, 4_900
                19, 29, 4_900
                19, 31, 4_900
                19, 45, 4_900
                19, 47, 4_900
                20, 26, 4_900
                20, 28, 4_900
                20, 30, 4_600
                20, 44, 4_900
                20, 46, 4_900
                21, 27, 4_900
                21, 45, 4_900
                22, 26, 4_900
                22, 44, 5_000
                23, 25, 4_900
                23, 45, 4_900
                24, 24, 4_900
                24, 44, 4_900
                25, 17, 4_900
                25, 18, 4_900
                25, 19, 4_900
                25, 20, 4_900
                25, 21, 4_900
                25, 22, 4_900
                25, 23, 4_900
                25, 43, 4_900
                26, 14, 4_900
                26, 15, 5_000
                26, 16, 5_000
                26, 42, 4_900
                27, 13, 5_000
                27, 41, 4_600
                28, 1, 4_900
                28, 12, 5_000
                28, 40, 4_600
                29, 1, 4_900
                29, 11, 4_900
                29, 39, 4_600
                30, 1, 4_900
                30, 10, 4_900
                30, 40, 4_600
                31, 2, 5_000
                31, 9, 4_900
                31, 41, 4_600
                32, 3, 4_900
                32, 8, 4_900
                32, 40, 4_600
                33, 4, 5_000
                33, 5, 4_600
                33, 7, 4_900
                33, 41, 4_900
                34, 6, 5_000
                34, 40, 4_600
                35, 39, 4_600
                36, 38, 5_000
            ]

    let w19s28 =
        room "W19S28"
        |> withStructures
            [
                { structureOf "container" None 162_000 250_000 { X = 20; Y = 41 } with
                    Energy = 1_316
                }
                { structureOf "container" None 164_600 250_000 { X = 43; Y = 23 } with
                    Energy = 932
                }
            ]
        |> withRoads
            [
                1, 41, 4_900
                2, 40, 4_800
                3, 41, 4_800
                4, 42, 4_900
                5, 41, 4_900
                6, 42, 4_800
                7, 43, 4_800
                8, 44, 4_800
                9, 45, 4_900
                10, 44, 4_800
                11, 45, 4_800
                12, 45, 4_800
                13, 45, 4_800
                14, 45, 4_800
                15, 45, 4_900
                16, 44, 4_900
                17, 43, 4_900
                18, 42, 4_900
                19, 42, 5_000
                20, 42, 5_000
                21, 42, 5_000
                22, 41, 5_000
                23, 40, 5_000
                24, 39, 5_000
                25, 38, 5_000
                26, 37, 5_000
                27, 36, 5_000
                28, 35, 5_000
                29, 34, 5_000
                30, 33, 5_000
                31, 32, 5_000
                32, 31, 5_000
                33, 30, 4_900
                34, 29, 5_000
                35, 28, 5_000
                36, 27, 5_000
                37, 26, 23_600
                38, 25, 5_000
                39, 24, 5_000
                40, 23, 5_000
                40, 26, 5_000
                41, 24, 4_900
                41, 25, 5_000
                42, 23, 4_900
            ]

    let w19s29 =
        room "W19S29"
        |> withController Ownership.Rival (Some "giaco") 5 4
        |> withControllerClocks 0 0 80_000 588_342
        |> withStructures
            [
                for x, y in
                    [
                        36, 17
                        37, 13
                        37, 14
                        37, 15
                        37, 16
                        37, 18
                        38, 15
                        38, 17
                        38, 19
                        39, 18
                        39, 20
                        40, 15
                        40, 17
                        41, 13
                        41, 14
                        41, 15
                        41, 16
                        41, 18
                        42, 15
                        42, 17
                        42, 19
                        43, 12
                        43, 14
                        43, 16
                        43, 18
                        44, 12
                        44, 15
                        44, 17
                        45, 14
                        45, 16
                    ] ->
                    { structureOf "extension" (Some giaco) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                { structureOf "container" None 120_400 250_000 { X = 36; Y = 47 } with
                    Energy = 1_146
                }
                structureOf "container" None 250_000 250_000 { X = 38; Y = 14 }
                { structureOf "container" None 250_000 250_000 { X = 40; Y = 14 } with
                    Energy = 100
                }
                { structureOf "container" None 250_000 250_000 { X = 44; Y = 13 } with
                    Energy = 379
                }
                { structureOf "link" (Some giaco) 1_000 1_000 { X = 37; Y = 47 } with
                    Energy = 800
                }
                structureOf "link" (Some giaco) 1_000 1_000 { X = 43; Y = 21 }
                { structureOf "spawn" (Some giaco) 5_000 5_000 { X = 41; Y = 22 } with
                    Energy = 300
                }
                { structureOf "storage" (Some giaco) 10_000 10_000 { X = 41; Y = 21 } with
                    Energy = 76_505
                }
                towerOf giaco { X = 43; Y = 22 } 879
                towerOf giaco { X = 43; Y = 23 } 842
            ]
        |> withRoads
            [
                1, 25, 5_000
                2, 26, 4_900
                3, 25, 5_000
                4, 24, 4_900
                5, 23, 5_000
                6, 22, 4_900
                7, 21, 4_900
                8, 22, 4_900
                9, 23, 4_900
                10, 22, 4_900
                11, 21, 4_900
                12, 20, 5_000
                13, 21, 5_000
                14, 20, 4_900
                15, 21, 5_000
                16, 20, 4_900
                17, 19, 4_900
                18, 18, 4_900
                19, 19, 5_000
                20, 18, 5_000
                21, 17, 4_900
                22, 16, 5_000
                23, 15, 4_900
                24, 14, 4_900
                25, 13, 4_900
                26, 12, 5_000
                27, 11, 5_000
                28, 10, 5_000
                29, 11, 4_900
                30, 12, 5_000
                31, 13, 5_000
                32, 14, 5_000
                33, 15, 4_800
                34, 16, 4_900
                35, 17, 5_000
                35, 31, 4_900
                36, 18, 4_900
                36, 30, 5_000
                36, 32, 4_900
                36, 34, 4_900
                36, 38, 4_900
                36, 40, 4_900
                36, 42, 4_900
                36, 44, 5_000
                37, 17, 4_900
                37, 19, 4_900
                37, 20, 4_900
                37, 21, 4_900
                37, 22, 5_000
                37, 23, 4_900
                37, 24, 4_900
                37, 25, 25_000
                37, 26, 4_900
                37, 27, 4_900
                37, 29, 5_000
                37, 33, 5_000
                37, 35, 4_900
                37, 37, 4_900
                37, 39, 4_900
                37, 41, 4_900
                37, 43, 5_000
                37, 45, 4_900
                38, 16, 4_800
                38, 18, 4_800
                38, 20, 4_900
                38, 25, 4_900
                38, 27, 4_900
                38, 28, 4_900
                38, 36, 4_900
                38, 46, 4_900
                38, 48, 4_700
                39, 15, 4_900
                39, 17, 4_900
                39, 19, 4_700
                39, 47, 4_900
                40, 18, 4_800
                40, 19, 4_700
                40, 20, 4_900
                40, 48, 4_900
                41, 17, 4_600
                41, 19, 4_900
                42, 16, 4_500
                42, 18, 4_700
                42, 20, 4_900
                42, 22, 4_800
                43, 15, 3_800
                43, 17, 4_500
                43, 19, 4_900
                44, 14, 3_900
                44, 16, 3_100
            ]

    let w16s25 = room "W16S25"

    let w16s27 =
        room "W16S27"
        |> withStructures
            [
                structureOf
                    "invaderCore"
                    (Some(Side.Npc "Invader"))
                    100_000
                    100_000
                    { X = 26; Y = 34 }
            ]

    let w16s29 =
        room "W16S29"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 16; Y = 42 } with
                    Energy = 1_114
                }
                { structureOf "container" None 171_500 250_000 { X = 22; Y = 21 } with
                    Energy = 1_570
                }
            ]
        |> withRoads
            [
                1, 20, 4_410
                1, 37, 4_310
                2, 19, 4_430
                2, 38, 3_130
                3, 18, 4_010
                3, 39, 3_400
                4, 18, 2_650
                4, 40, 3_100
                5, 18, 4_310
                5, 41, 2_800
                6, 18, 3_310
                6, 42, 2_800
                7, 18, 3_750
                7, 41, 3_500
                8, 18, 3_050
                8, 40, 3_800
                9, 18, 3_450
                9, 39, 4_400
                10, 18, 3_750
                10, 40, 4_400
                11, 18, 4_430
                11, 40, 3_000
                12, 18, 3_130
                12, 40, 4_000
                13, 18, 4_530
                13, 41, 2_600
                14, 18, 4_530
                14, 41, 3_180
                15, 18, 2_780
                15, 41, 3_400
                16, 18, 3_680
                17, 18, 4_600
                18, 18, 4_600
                19, 18, 4_300
                20, 19, 3_700
                21, 20, 4_400
            ]

    let w15s25 =
        room "W15S25"
        |> withReactor "6a901a3bb8684d0008337ed2" { X = 44; Y = 6 } (Some odiodin) 0

    let w15s27 =
        room "W15S27"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 13; Y = 29 } with
                    Energy = 24
                    Id = "container-13-29-W15S27"
                }
            ]
        |> withRoads
            [
                1, 41, 4_900
                2, 40, 3_800
                2, 42, 3_800
                3, 39, 3_700
                3, 43, 2_600
                4, 38, 3_800
                4, 44, 4_900
                5, 37, 3_800
                5, 45, 2_500
                6, 36, 2_800
                6, 46, 3_740
                7, 35, 4_000
                7, 47, 2_800
                8, 34, 4_400
                8, 48, 2_600
                9, 33, 4_400
                10, 32, 4_000
                11, 31, 3_600
                12, 30, 4_000
            ]

    let w15s29 =
        room "W15S29"
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 17; Y = 19 } with
                    Energy = 1_020
                }
            ]
        |> withRoads
            [
                4, 6, 2_480
                5, 5, 3_880
                5, 7, 4_500
                6, 4, 2_800
                6, 8, 4_800
                7, 3, 3_300
                7, 9, 2_600
                8, 2, 3_500
                8, 10, 4_800
                9, 1, 4_100
                9, 11, 4_300
                10, 12, 4_300
                11, 13, 3_200
                12, 14, 3_800
                13, 15, 3_200
                14, 16, 3_500
                15, 17, 2_500
                16, 18, 2_400
            ]

    let w14s28 =
        room "W14S28"
        |> withStructures
            [
                { structureOf "container" None 125_000 250_000 { X = 6; Y = 9 } with
                    Energy = 1_150
                }
            ]
        |> withRoads
            [
                1, 21, 2_800
                2, 20, 2_700
                3, 19, 4_600
                4, 18, 3_500
                5, 17, 3_600
                6, 10, 3_700
                6, 16, 4_180
                7, 11, 3_600
                7, 15, 4_200
                8, 12, 2_700
                8, 14, 4_500
                9, 13, 2_900
            ]

    let w18s26 =
        room "W18S26"
        |> withController Ownership.Rival (Some "Trepidimous") 6 4
        |> withControllerClocks 0 0 120_000 1_975_077
        |> withStructures
            [
                for x, y in
                    [
                        15, 11
                        15, 13
                        16, 12
                        17, 11
                        17, 17
                        18, 16
                        18, 18
                        19, 17
                        20, 18
                        21, 17
                        22, 18
                        23, 9
                        23, 11
                        23, 13
                        23, 15
                        23, 17
                        24, 9
                        24, 10
                        24, 12
                        24, 14
                        24, 16
                        24, 18
                    ] ->
                    { structureOf "extension" (Some trepidimous) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        16, 10
                        16, 14
                        16, 16
                        16, 18
                        17, 9
                        17, 13
                        17, 15
                        18, 10
                        19, 9
                        19, 13
                        19, 15
                        20, 10
                        21, 9
                        22, 10
                    ] ->
                    { structureOf "extension" (Some trepidimous) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                        Id = $"extension-{x}-{y}-W18S26"
                    }
            ]
        |> withStructures
            [
                for x, y in [ 18, 12; 19, 11; 21, 11 ] ->
                    { structureOf "extension" (Some trepidimous) 1_000 1_000 { X = x; Y = y } with
                        Id = $"extension-{x}-{y}-W18S26"
                    }
            ]
        |> withStructures
            [
                for x, y in [ 25, 21; 26, 20; 27, 21 ] ->
                    { structureOf "lab" (Some trepidimous) 500 500 { X = x; Y = y } with
                        Energy = 2_000
                    }
            ]
        |> withStructures
            [
                { structureOf "container" None 250_000 250_000 { X = 13; Y = 19 } with
                    Energy = 2_000
                }
                structureOf "container" None 250_000 250_000 { X = 15; Y = 42 }
                { structureOf "extension" (Some trepidimous) 1_000 1_000 { X = 18; Y = 14 } with
                    Energy = 10
                    Id = "extension-18-14-W18S26"
                }
                structureOf "extractor" (Some trepidimous) 500 500 { X = 14; Y = 41 }
                structureOf "link" (Some trepidimous) 1_000 1_000 { X = 20; Y = 16 }
                { structureOf "link" (Some trepidimous) 1_000 1_000 { X = 25; Y = 41 } with
                    Energy = 180
                }
                { structureOf "link" (Some trepidimous) 1_000 1_000 { X = 30; Y = 4 } with
                    Energy = 90
                }
                { structureOf "rampart" (Some trepidimous) 953_601 30_000_000 { X = 1; Y = 10 } with
                    NextDecay = 994_939
                }
                { structureOf "rampart" (Some trepidimous) 962_401 30_000_000 { X = 2; Y = 4 } with
                    NextDecay = 994_949
                }
                { structureOf "rampart" (Some trepidimous) 964_001 30_000_000 { X = 2; Y = 5 } with
                    NextDecay = 994_940
                }
                { structureOf "rampart" (Some trepidimous) 971_801 30_000_000 { X = 2; Y = 6 } with
                    NextDecay = 994_967
                }
                { structureOf "rampart" (Some trepidimous) 966_941 30_000_000 { X = 2; Y = 7 } with
                    NextDecay = 994_998
                }
                { structureOf "rampart" (Some trepidimous) 971_331 30_000_000 { X = 2; Y = 8 } with
                    NextDecay = 994_930
                }
                { structureOf "rampart" (Some trepidimous) 971_801 30_000_000 { X = 2; Y = 9 } with
                    NextDecay = 995_017
                }
                { structureOf "rampart" (Some trepidimous) 980_721 30_000_000 { X = 2; Y = 10 } with
                    NextDecay = 995_016
                }
                { structureOf "rampart" (Some trepidimous) 987_101 30_000_000 { X = 10; Y = 22 } with
                    NextDecay = 994_998
                }
                { structureOf "rampart" (Some trepidimous) 973_201 30_000_000 { X = 11; Y = 22 } with
                    NextDecay = 994_938
                }
                { structureOf "rampart" (Some trepidimous) 950_801 30_000_000 { X = 12; Y = 21 } with
                    NextDecay = 994_969
                }
                { structureOf "rampart" (Some trepidimous) 969_201 30_000_000 { X = 12; Y = 22 } with
                    NextDecay = 994_935
                }
                { structureOf "rampart" (Some trepidimous) 967_901 30_000_000 { X = 15; Y = 47 } with
                    NextDecay = 995_005
                }
                { structureOf "rampart" (Some trepidimous) 969_501 30_000_000 { X = 15; Y = 48 } with
                    NextDecay = 994_974
                }
                { structureOf "rampart" (Some trepidimous) 951_801 30_000_000 { X = 16; Y = 47 } with
                    NextDecay = 994_975
                }
                { structureOf "rampart" (Some trepidimous) 981_501 30_000_000 { X = 17; Y = 47 } with
                    NextDecay = 994_923
                }
                { structureOf "rampart" (Some trepidimous) 943_001 30_000_000 { X = 18; Y = 47 } with
                    NextDecay = 995_005
                }
                { structureOf "rampart" (Some trepidimous) 958_101 30_000_000 { X = 19; Y = 47 } with
                    NextDecay = 994_978
                }
                { structureOf "rampart" (Some trepidimous) 945_081 30_000_000 { X = 20; Y = 47 } with
                    NextDecay = 994_921
                }
                { structureOf "rampart" (Some trepidimous) 953_321 30_000_000 { X = 21; Y = 47 } with
                    NextDecay = 994_920
                }
                { structureOf "rampart" (Some trepidimous) 966_971 30_000_000 { X = 22; Y = 47 } with
                    NextDecay = 994_960
                }
                { structureOf "rampart" (Some trepidimous) 967_881 30_000_000 { X = 23; Y = 47 } with
                    NextDecay = 994_997
                }
                { structureOf "rampart" (Some trepidimous) 974_711 30_000_000 { X = 24; Y = 47 } with
                    NextDecay = 994_931
                }
                { structureOf "rampart" (Some trepidimous) 980_931 30_000_000 { X = 25; Y = 47 } with
                    NextDecay = 994_925
                }
                { structureOf "rampart" (Some trepidimous) 984_481 30_000_000 { X = 26; Y = 47 } with
                    NextDecay = 994_956
                }
                { structureOf "rampart" (Some trepidimous) 965_671 30_000_000 { X = 27; Y = 47 } with
                    NextDecay = 994_948
                }
                { structureOf "rampart" (Some trepidimous) 957_431 30_000_000 { X = 28; Y = 47 } with
                    NextDecay = 994_952
                }
                { structureOf "rampart" (Some trepidimous) 944_591 30_000_000 { X = 29; Y = 47 } with
                    NextDecay = 994_942
                }
                { structureOf "rampart" (Some trepidimous) 952_201 30_000_000 { X = 30; Y = 3 } with
                    NextDecay = 994_956
                }
                { structureOf "rampart" (Some trepidimous) 949_301 30_000_000 { X = 30; Y = 4 } with
                    NextDecay = 995_011
                }
                { structureOf "rampart" (Some trepidimous) 963_301 30_000_000 { X = 30; Y = 5 } with
                    NextDecay = 994_976
                }
                { structureOf "rampart" (Some trepidimous) 958_501 30_000_000 { X = 30; Y = 6 } with
                    NextDecay = 994_941
                }
                { structureOf "rampart" (Some trepidimous) 947_301 30_000_000 { X = 30; Y = 7 } with
                    NextDecay = 994_977
                }
                { structureOf "rampart" (Some trepidimous) 962_201 30_000_000 { X = 30; Y = 8 } with
                    NextDecay = 995_002
                }
                { structureOf "rampart" (Some trepidimous) 971_301 30_000_000 { X = 30; Y = 9 } with
                    NextDecay = 994_934
                }
                { structureOf "rampart" (Some trepidimous) 972_201 30_000_000 { X = 30; Y = 10 } with
                    NextDecay = 995_001
                }
                { structureOf "rampart" (Some trepidimous) 960_801 30_000_000 { X = 30; Y = 11 } with
                    NextDecay = 994_936
                }
                { structureOf "rampart" (Some trepidimous) 957_101 30_000_000 { X = 30; Y = 12 } with
                    NextDecay = 994_980
                }
                { structureOf "rampart" (Some trepidimous) 962_801 30_000_000 { X = 30; Y = 13 } with
                    NextDecay = 994_932
                }
                { structureOf "rampart" (Some trepidimous) 944_701 30_000_000 { X = 30; Y = 14 } with
                    NextDecay = 994_979
                }
                { structureOf "rampart" (Some trepidimous) 969_701 30_000_000 { X = 30; Y = 15 } with
                    NextDecay = 994_934
                }
                { structureOf "rampart" (Some trepidimous) 946_101 30_000_000 { X = 30; Y = 16 } with
                    NextDecay = 994_961
                }
                { structureOf "rampart" (Some trepidimous) 967_201 30_000_000 { X = 30; Y = 17 } with
                    NextDecay = 994_991
                }
                { structureOf "rampart" (Some trepidimous) 973_401 30_000_000 { X = 30; Y = 18 } with
                    NextDecay = 994_984
                }
                { structureOf "rampart" (Some trepidimous) 972_101 30_000_000 { X = 30; Y = 19 } with
                    NextDecay = 994_983
                }
                { structureOf "rampart" (Some trepidimous) 951_301 30_000_000 { X = 30; Y = 20 } with
                    NextDecay = 994_962
                }
                { structureOf "rampart" (Some trepidimous) 970_401 30_000_000 { X = 30; Y = 21 } with
                    NextDecay = 994_988
                }
                { structureOf "rampart" (Some trepidimous) 976_201 30_000_000 { X = 30; Y = 22 } with
                    NextDecay = 994_955
                }
                { structureOf "rampart" (Some trepidimous) 946_101 30_000_000 { X = 30; Y = 23 } with
                    NextDecay = 995_013
                }
                { structureOf "rampart" (Some trepidimous) 959_701 30_000_000 { X = 30; Y = 24 } with
                    NextDecay = 994_971
                }
                { structureOf "rampart" (Some trepidimous) 977_501 30_000_000 { X = 30; Y = 25 } with
                    NextDecay = 995_004
                }
                { structureOf "rampart" (Some trepidimous) 943_801 30_000_000 { X = 30; Y = 26 } with
                    NextDecay = 995_005
                }
                { structureOf "rampart" (Some trepidimous) 950_081 30_000_000 { X = 30; Y = 27 } with
                    NextDecay = 994_972
                }
                { structureOf "rampart" (Some trepidimous) 973_381 30_000_000 { X = 30; Y = 28 } with
                    NextDecay = 994_972
                }
                { structureOf "rampart" (Some trepidimous) 948_281 30_000_000 { X = 30; Y = 29 } with
                    NextDecay = 994_980
                }
                { structureOf "rampart" (Some trepidimous) 966_281 30_000_000 { X = 30; Y = 30 } with
                    NextDecay = 994_941
                }
                { structureOf "rampart" (Some trepidimous) 956_901 30_000_000 { X = 30; Y = 31 } with
                    NextDecay = 994_984
                }
                { structureOf "rampart" (Some trepidimous) 962_001 30_000_000 { X = 30; Y = 32 } with
                    NextDecay = 994_943
                }
                { structureOf "rampart" (Some trepidimous) 984_831 30_000_000 { X = 30; Y = 33 } with
                    NextDecay = 994_920
                }
                { structureOf "rampart" (Some trepidimous) 951_081 30_000_000 { X = 30; Y = 34 } with
                    NextDecay = 994_981
                }
                { structureOf "rampart" (Some trepidimous) 948_901 30_000_000 { X = 30; Y = 35 } with
                    NextDecay = 995_001
                }
                { structureOf "rampart" (Some trepidimous) 979_181 30_000_000 { X = 30; Y = 36 } with
                    NextDecay = 995_002
                }
                { structureOf "rampart" (Some trepidimous) 959_041 30_000_000 { X = 30; Y = 37 } with
                    NextDecay = 994_963
                }
                { structureOf "rampart" (Some trepidimous) 980_401 30_000_000 { X = 30; Y = 38 } with
                    NextDecay = 994_969
                }
                { structureOf "rampart" (Some trepidimous) 959_471 30_000_000 { X = 30; Y = 39 } with
                    NextDecay = 994_927
                }
                { structureOf "rampart" (Some trepidimous) 984_401 30_000_000 { X = 30; Y = 40 } with
                    NextDecay = 994_961
                }
                { structureOf "rampart" (Some trepidimous) 977_501 30_000_000 { X = 30; Y = 41 } with
                    NextDecay = 995_009
                }
                { structureOf "rampart" (Some trepidimous) 971_661 30_000_000 { X = 30; Y = 42 } with
                    NextDecay = 994_974
                }
                { structureOf "rampart" (Some trepidimous) 985_601 30_000_000 { X = 30; Y = 43 } with
                    NextDecay = 994_979
                }
                { structureOf "rampart" (Some trepidimous) 964_101 30_000_000 { X = 30; Y = 44 } with
                    NextDecay = 994_930
                }
                { structureOf "rampart" (Some trepidimous) 973_301 30_000_000 { X = 30; Y = 45 } with
                    NextDecay = 994_990
                }
                { structureOf "rampart" (Some trepidimous) 963_201 30_000_000 { X = 30; Y = 46 } with
                    NextDecay = 994_927
                }
                { structureOf "rampart" (Some trepidimous) 947_601 30_000_000 { X = 30; Y = 47 } with
                    NextDecay = 994_993
                }
                { structureOf "spawn" (Some trepidimous) 5_000 5_000 { X = 20; Y = 12 } with
                    Energy = 150
                }
                { structureOf "storage" (Some trepidimous) 10_000 10_000 { X = 20; Y = 14 } with
                    Energy = 37_355
                }
                { structureOf "terminal" (Some trepidimous) 3_000 3_000 { X = 22; Y = 16 } with
                    Energy = 40_284
                }
                towerOf trepidimous { X = 18; Y = 8 } 991
                towerOf trepidimous { X = 22; Y = 8 } 1_000
            ]
        |> withRoads
            [
                1, 8, 4_800
                2, 8, 4_700
                3, 8, 4_700
                4, 8, 4_400
                5, 8, 4_300
                6, 8, 4_300
                7, 8, 4_000
                8, 9, 4_300
                9, 9, 5_000
                10, 9, 4_300
                11, 9, 4_700
                12, 9, 4_000
                13, 9, 4_100
                13, 23, 4_500
                14, 9, 4_200
                14, 22, 4_500
                15, 10, 4_300
                15, 22, 22_500
                16, 11, 5_000
                16, 21, 4_000
                17, 12, 3_900
                17, 18, 4_900
                17, 20, 4_600
                18, 13, 4_800
                18, 15, 4_200
                18, 17, 4_400
                18, 19, 5_000
                18, 24, 4_500
                18, 25, 4_700
                18, 26, 4_300
                18, 27, 4_100
                18, 28, 4_400
                18, 29, 4_700
                18, 30, 4_700
                18, 31, 4_800
                18, 32, 4_000
                18, 33, 4_200
                18, 39, 4_200
                18, 40, 4_800
                18, 41, 4_200
                18, 42, 4_100
                18, 43, 4_000
                18, 44, 4_400
                18, 48, 4_900
                19, 12, 4_900
                19, 14, 4_200
                19, 16, 4_800
                19, 18, 4_500
                19, 20, 4_100
                19, 21, 4_800
                19, 22, 4_100
                19, 23, 4_000
                19, 34, 4_200
                19, 35, 5_000
                19, 36, 4_100
                19, 37, 4_400
                19, 38, 4_600
                19, 45, 4_900
                19, 47, 4_900
                20, 11, 4_700
                20, 13, 4_000
                20, 15, 4_400
                20, 17, 4_600
                20, 19, 4_700
                20, 21, 4_500
                20, 46, 4_700
                21, 10, 4_000
                21, 12, 5_000
                21, 13, 4_400
                21, 14, 4_700
                21, 16, 4_900
                21, 22, 4_000
                21, 47, 4_900
                22, 9, 4_200
                22, 11, 4_800
                22, 15, 5_000
                22, 23, 4_800
                22, 48, 4_400
                23, 8, 4_700
                23, 16, 4_400
                23, 20, 5_000
                23, 21, 4_900
                23, 24, 4_300
                24, 7, 4_100
                24, 17, 5_000
                24, 19, 4_300
                24, 22, 4_900
                24, 25, 4_600
                25, 7, 4_100
                25, 18, 4_200
                25, 20, 23_500
                25, 23, 4_800
                25, 26, 4_400
                25, 35, 4_400
                25, 41, 4_200
                25, 42, 4_300
                26, 7, 4_800
                26, 17, 4_800
                26, 18, 4_200
                26, 21, 4_800
                26, 23, 4_600
                26, 27, 4_700
                26, 28, 4_100
                26, 29, 4_500
                26, 30, 5_000
                26, 31, 4_500
                26, 32, 4_900
                26, 33, 4_800
                26, 34, 4_900
                26, 35, 4_600
                26, 36, 4_100
                26, 37, 4_700
                26, 38, 4_100
                26, 39, 4_700
                26, 40, 4_800
                26, 41, 4_200
                27, 6, 4_300
                27, 16, 5_000
                27, 19, 4_400
                27, 22, 4_500
                28, 5, 4_500
                28, 15, 4_100
                28, 20, 4_800
                28, 21, 5_000
                29, 4, 4_700
                29, 6, 4_700
                29, 14, 4_200
                29, 21, 4_100
                30, 3, 4_800
                30, 5, 4_000
                30, 7, 4_900
                30, 13, 4_300
                30, 22, 4_500
                31, 8, 4_900
                31, 12, 4_300
                31, 23, 3_900
                32, 9, 4_000
                32, 11, 4_400
                32, 24, 4_300
                33, 10, 4_700
                33, 25, 4_700
                34, 11, 4_900
                34, 26, 5_000
                35, 11, 4_600
                35, 27, 4_300
                36, 11, 4_300
                36, 28, 4_700
                37, 10, 4_600
                37, 29, 5_000
                38, 9, 4_000
                38, 30, 4_200
                39, 8, 4_900
                39, 31, 5_000
                40, 7, 4_700
                40, 31, 5_000
                41, 6, 4_300
                41, 7, 4_500
                41, 31, 5_000
                42, 6, 4_700
                42, 7, 5_000
                42, 31, 4_000
                43, 6, 5_000
                43, 7, 4_200
                43, 31, 4_400
                44, 6, 4_000
                44, 7, 4_800
                44, 31, 4_400
                45, 6, 4_100
                45, 7, 4_100
                45, 31, 4_900
                46, 6, 4_700
                46, 7, 4_100
                46, 31, 5_000
                47, 6, 5_000
                47, 7, 5_000
                47, 31, 3_900
                48, 6, 4_300
                48, 7, 4_100
                48, 31, 4_500
            ]

    let w17s24 =
        room "W17S24"
        |> withController Ownership.Rival (Some "Trepidimous") 5 4
        |> withControllerClocks 0 0 80_000 7_950
        |> withStructures
            [
                for x, y in
                    [
                        19, 31
                        19, 33
                        20, 32
                        21, 31
                        21, 33
                        21, 35
                        21, 37
                        22, 36
                        22, 38
                        24, 38
                        25, 39
                        26, 36
                        26, 38
                    ] ->
                    { structureOf "extension" (Some trepidimous) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                        Id = $"extension-{x}-{y}-W17S24"
                    }
            ]
        |> withStructures
            [
                for x, y in [ 19, 35; 19, 39; 20, 36; 20, 38 ] ->
                    { structureOf "extension" (Some trepidimous) 1_000 1_000 { X = x; Y = y } with
                        Id = $"extension-{x}-{y}-W17S24"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        20, 34
                        22, 32
                        22, 34
                        23, 31
                        23, 39
                        24, 32
                        25, 31
                        26, 32
                        26, 34
                        27, 31
                        27, 33
                    ] ->
                    { structureOf "extension" (Some trepidimous) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                { structureOf "container" None 250_000 250_000 { X = 17; Y = 19 } with
                    Energy = 110
                    Id = "container-17-19-W17S24"
                }
                structureOf "extension" (Some trepidimous) 1_000 1_000 { X = 19; Y = 37 }
                structureOf "extension" (Some trepidimous) 1_000 1_000 { X = 21; Y = 39 }
                { structureOf "link" (Some trepidimous) 1_000 1_000 { X = 11; Y = 12 } with
                    Energy = 187
                }
                structureOf "link" (Some trepidimous) 1_000 1_000 { X = 23; Y = 37 }
                { structureOf "rampart" (Some trepidimous) 685_141 10_000_000 { X = 2; Y = 18 } with
                    Id = "rampart-2-18-W17S24"
                    NextDecay = 995_009
                }
                { structureOf "rampart" (Some trepidimous) 691_631 10_000_000 { X = 2; Y = 19 } with
                    Id = "rampart-2-19-W17S24"
                    NextDecay = 995_014
                }
                { structureOf "rampart" (Some trepidimous) 696_561 10_000_000 { X = 2; Y = 20 } with
                    Id = "rampart-2-20-W17S24"
                    NextDecay = 994_992
                }
                { structureOf "rampart" (Some trepidimous) 684_881 10_000_000 { X = 2; Y = 21 } with
                    NextDecay = 995_017
                }
                { structureOf "rampart" (Some trepidimous) 697_011 10_000_000 { X = 6; Y = 2 } with
                    NextDecay = 994_977
                }
                { structureOf "rampart" (Some trepidimous) 685_101 10_000_000 { X = 6; Y = 3 } with
                    NextDecay = 995_002
                }
                { structureOf "rampart" (Some trepidimous) 683_901 10_000_000 { X = 9; Y = 9 } with
                    Id = "rampart-9-9-W17S24"
                    NextDecay = 995_009
                }
                { structureOf "rampart" (Some trepidimous) 695_501 10_000_000 { X = 10; Y = 8 } with
                    Id = "rampart-10-8-W17S24"
                    NextDecay = 994_983
                }
                { structureOf "rampart" (Some trepidimous) 687_401 10_000_000 { X = 10; Y = 9 } with
                    NextDecay = 994_949
                }
                { structureOf "rampart" (Some trepidimous) 696_301 10_000_000 { X = 24; Y = 3 } with
                    NextDecay = 994_943
                }
                { structureOf "rampart" (Some trepidimous) 697_301 10_000_000 { X = 24; Y = 4 } with
                    NextDecay = 994_975
                }
                { structureOf "rampart" (Some trepidimous) 688_701 10_000_000 { X = 24; Y = 5 } with
                    NextDecay = 994_942
                }
                { structureOf "rampart" (Some trepidimous) 693_501 10_000_000 { X = 24; Y = 6 } with
                    NextDecay = 994_954
                }
                { structureOf "rampart" (Some trepidimous) 685_201 10_000_000 { X = 30; Y = 10 } with
                    Id = "rampart-30-10-W17S24"
                    NextDecay = 994_921
                }
                { structureOf "rampart" (Some trepidimous) 683_851 10_000_000 { X = 31; Y = 10 } with
                    NextDecay = 995_015
                }
                { structureOf "rampart" (Some trepidimous) 691_271 10_000_000 { X = 32; Y = 10 } with
                    NextDecay = 995_012
                }
                { structureOf "rampart" (Some trepidimous) 691_201 10_000_000 { X = 33; Y = 10 } with
                    NextDecay = 994_922
                }
                { structureOf "rampart" (Some trepidimous) 688_801 10_000_000 { X = 34; Y = 10 } with
                    NextDecay = 994_973
                }
                { structureOf "rampart" (Some trepidimous) 688_851 10_000_000 { X = 43; Y = 33 } with
                    NextDecay = 994_961
                }
                { structureOf "rampart" (Some trepidimous) 684_141 10_000_000 { X = 44; Y = 33 } with
                    NextDecay = 994_979
                }
                { structureOf "rampart" (Some trepidimous) 682_501 10_000_000 { X = 45; Y = 33 } with
                    NextDecay = 994_993
                }
                { structureOf "rampart" (Some trepidimous) 689_801 10_000_000 { X = 46; Y = 33 } with
                    NextDecay = 995_017
                }
                { structureOf "spawn" (Some trepidimous) 5_000 5_000 { X = 23; Y = 33 } with
                    Energy = 300
                }
                { structureOf "storage" (Some trepidimous) 10_000 10_000 { X = 23; Y = 35 } with
                    Energy = 40_437
                }
                towerOf trepidimous { X = 17; Y = 33 } 1_000
                towerOf trepidimous { X = 17; Y = 37 } 1_000
            ]
        |> withRoads
            [
                8, 10, 4_700
                8, 11, 4_700
                8, 12, 4_800
                8, 13, 4_200
                8, 14, 4_500
                8, 15, 4_500
                8, 16, 4_000
                8, 17, 5_000
                8, 18, 4_100
                8, 19, 4_800
                8, 20, 4_800
                8, 21, 4_700
                9, 14, 4_500
                9, 22, 4_400
                9, 30, 4_700
                9, 31, 4_800
                10, 15, 4_500
                10, 23, 4_300
                10, 24, 4_000
                10, 25, 4_900
                10, 26, 4_300
                10, 27, 4_000
                10, 28, 4_300
                10, 29, 4_400
                10, 32, 5_000
                11, 12, 4_800
                11, 16, 4_200
                11, 24, 4_800
                11, 25, 4_200
                11, 26, 4_200
                11, 27, 4_900
                11, 28, 4_900
                11, 32, 4_900
                12, 12, 4_200
                12, 13, 4_100
                12, 14, 4_300
                12, 15, 4_400
                12, 16, 5_000
                12, 17, 4_300
                12, 18, 5_000
                12, 19, 4_600
                12, 20, 4_500
                12, 21, 4_200
                12, 22, 4_200
                12, 23, 4_700
                12, 32, 4_100
                13, 20, 4_500
                13, 22, 4_400
                13, 33, 4_400
                14, 21, 4_800
                14, 34, 4_900
                15, 19, 4_700
                15, 20, 4_200
                15, 34, 4_700
                16, 18, 5_000
                16, 19, 4_800
                16, 34, 4_200
                17, 34, 4_000
                18, 33, 4_600
                19, 32, 4_000
                20, 33, 4_500
                21, 32, 4_800
                21, 34, 4_300
                22, 21, 4_500
                22, 22, 21_500
                22, 23, 20_000
                22, 31, 4_900
                22, 33, 5_000
                22, 35, 4_200
                23, 20, 4_900
                23, 24, 20_500
                23, 30, 5_000
                23, 34, 4_300
                24, 16, 4_400
                24, 17, 4_100
                24, 18, 5_000
                24, 19, 4_900
                24, 25, 4_200
                24, 26, 4_000
                24, 27, 4_100
                24, 28, 4_400
                24, 29, 4_700
                25, 15, 4_500
                26, 14, 4_700
                27, 13, 4_800
                28, 12, 4_700
                29, 11, 4_000
                30, 11, 4_700
                31, 11, 4_500
                32, 10, 23_500
                33, 1, 4_200
                33, 9, 22_400
                34, 2, 4_100
                34, 8, 4_100
                35, 2, 4_500
                35, 3, 4_000
                35, 4, 4_100
                35, 5, 4_800
                35, 6, 5_000
                35, 7, 4_900
                36, 1, 4_900
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c ->
            List.contains c.Home [ "W17S25"; "W15S28"; "W17S29"; "W13S28"; "W12S28"; "W12S26" ])

    let bodies: Body list =
        [
            // Faraday387: live: 15 ATTACK, 1 MOVE
            // Faraday387: TODO script; live: no act
            { body
                  "Faraday387"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W18S25" 44 13)
                  (Some(
                      Waypoints
                          [
                              at "W18S25" 43 13
                              at "W18S25" 42 13
                              at "W18S25" 41 13
                              at "W18S25" 40 13
                              at "W18S25" 39 14
                              at "W18S25" 38 15
                              at "W18S25" 37 16
                              at "W18S25" 36 17
                              at "W18S25" 35 16
                              at "W18S25" 34 15
                              at "W18S25" 33 14
                          ]
                  )) with
                Hits = 1_544
                TicksToLive = 1_073
                Fatigue = 13
            }
            { body
                  "ranger-993409-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 34)
                  None with
                TicksToLive = 116
            }
            { body
                  "worker-993432-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 16 39)
                  None with
                TicksToLive = 73
                Energy = 248
            }
            { body
                  "anchor-993520-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 125
                Energy = 50
            }
            { body
                  "hauler-993574-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 10 23)
                  None with
                TicksToLive = 227
            }
            { body
                  "upgrader-993726-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 18 35)
                  None with
                TicksToLive = 358
            }
            { body
                  "guard-994071-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 13 47)
                  None with
                TicksToLive = 682
            }
            { body
                  "hauler-994306-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 19 30)
                  None with
                TicksToLive = 959
            }
            { body
                  "hauler-994378-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 18 34)
                  None with
                TicksToLive = 1_031
                Energy = 800
            }
            { body
                  "ranger-994540-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 26 31)
                  None with
                TicksToLive = 1_247
            }
            { body
                  "upgrader-994568-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 17 32)
                  None with
                TicksToLive = 1_200
            }
            { body
                  "defender-994619-Spawn11"
                  Side.Ours
                  (parts [ Move, 10; Attack, 10 ])
                  (at "W17S25" 3 17)
                  None with
                TicksToLive = 1_260
            }
            { body
                  "hauler-994803-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 12 27)
                  None with
                TicksToLive = 1_456
                Energy = 256
            }
            { body
                  "ranger-994875-Spawn11"
                  Side.Ours
                  (parts [ Move, 3; RangedAttack, 2; Heal, 1 ])
                  (at "W17S25" 12 25)
                  None with
                TicksToLive = 1_474
            }
            { body
                  "reserver-994679-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S25" 3 18)
                  None with
                TicksToLive = 372
            }
            // Photon713: live: 15 MOVE, 13 ATTACK
            // Photon713: TODO script; live: healed→49,28
            { body
                  "Photon713"
                  trepidimous
                  (parts [ Move, 14; Attack, 13; Move, 1 ])
                  (at "W17S25" 47 28)
                  (Some(
                      Waypoints
                          [
                              at "W17S25" 46 28
                              at "W17S25" 45 28
                              at "W17S25" 44 29
                              at "W17S25" 43 30
                              at "W17S25" 42 30
                              at "W17S25" 41 30
                              at "W17S25" 40 30
                              at "W17S25" 39 30
                              at "W17S25" 38 30
                          ]
                  )) with
                Hits = 2_722
                TicksToLive = 1_378
            }
            // Pendulum654: live: 5 MOVE, 6 HEAL
            // Pendulum654: TODO script; live: attacked→13,29 heal→48,28
            { body
                  "Pendulum654"
                  trepidimous
                  (parts [ Move, 6; Heal, 6 ])
                  (at "W17S25" 48 28)
                  (Some(Follow "Photon713")) with
                Hits = 1_050
                TicksToLive = 1_271
            }
            { body
                  "upgrader-993505-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 233
                Energy = 26
            }
            { body
                  "worker-994186-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 30)
                  None with
                TicksToLive = 917
                Energy = 594
            }
            { body
                  "anchor-994580-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 1_185
                Energy = 50
            }
            { body
                  "anchor-994748-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_353
                Energy = 50
            }
            { body
                  "hauler-994772-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 19 32)
                  None with
                TicksToLive = 1_497
            }
            { body
                  "ranger-994078-Spawn9"
                  Side.Ours
                  (parts [ Move, 9; RangedAttack, 6; Heal, 3 ])
                  (at "W19S25" 8 48)
                  None with
                TicksToLive = 713
            }
            { body
                  "worker-993452-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 27 37)
                  None with
                TicksToLive = 138
                Energy = 138
            }
            { body
                  "worker-993584-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 27 36)
                  None with
                TicksToLive = 270
                Energy = 6
            }
            { body
                  "guard-993724-Spawn9"
                  Side.Ours
                  (parts [ Tough, 2; Move, 10; Attack, 6; Heal, 2 ])
                  (at "W17S29" 47 9)
                  None with
                TicksToLive = 365
            }
            { body
                  "guard-993784-Spawn9"
                  Side.Ours
                  (parts [ Tough, 2; Move, 10; Attack, 6; Heal, 2 ])
                  (at "W17S29" 29 35)
                  None with
                TicksToLive = 425
            }
            { body
                  "worker-993966-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 31 33)
                  None with
                TicksToLive = 652
                Energy = 534
            }
            { body
                  "hauler-994452-Spawn9"
                  Side.Ours
                  (parts [ Carry, 30; Move, 15 ])
                  (at "W17S29" 22 30)
                  None with
                TicksToLive = 1_168
            }
            { body
                  "anchor-994587-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 13 29)
                  None with
                TicksToLive = 1_192
                Energy = 50
            }
            { body
                  "anchor-994611-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 44 38)
                  None with
                TicksToLive = 1_216
                Energy = 50
            }
            { body
                  "upgrader-994821-Spawn9"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W17S29" 26 41)
                  None with
                TicksToLive = 1_495
                Energy = 50
            }
            // Quantum825: TODO script; live: no act
            { body
                  "Quantum825"
                  trepidimous
                  (parts [ Move, 3; Work, 5 ])
                  (at "W17S26" 21 2)
                  (Some Hold) with
                TicksToLive = 1_430
            }
            { body
                  "anchor-993654-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 265
                Energy = 50
            }
            { body
                  "reserver-994462-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 9 22)
                  None with
                TicksToLive = 155
            }
            { body
                  "ranger-994398-Spawn9"
                  Side.Ours
                  (parts [ Move, 9; RangedAttack, 6; Heal, 3 ])
                  (at "W19S26" 24 35)
                  None with
                TicksToLive = 1_033
            }
            { body
                  "worker-993508-Spawn2"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W13S28" 19 12)
                  None with
                TicksToLive = 239
                Energy = 703
            }
            { body
                  "hauler-993704-Spawn2"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W13S28" 15 10)
                  None with
                TicksToLive = 429
                Energy = 1_058
            }
            { body
                  "upgrader-993898-Spawn2"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W13S28" 23 14)
                  None with
                TicksToLive = 572
                Energy = 5
            }
            { body
                  "anchor-994184-Spawn2"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W13S28" 15 8)
                  None with
                TicksToLive = 789
                Energy = 50
            }
            { body
                  "anchor-994396-Spawn2"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W13S28" 18 3)
                  None with
                TicksToLive = 1_001
                Energy = 50
            }
            { body
                  "hauler-993532-Spawn6"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W12S28" 8 38)
                  None with
                TicksToLive = 257
                Energy = 230
            }
            { body
                  "anchor-993806-Spawn1"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W12S28" 10 43)
                  None with
                TicksToLive = 411
                Energy = 50
            }
            { body
                  "anchor-993828-Spawn6"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W12S28" 16 39)
                  None with
                TicksToLive = 433
                Energy = 50
            }
            { body
                  "worker-993828-Spawn5"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W12S28" 6 14)
                  None with
                TicksToLive = 559
                Energy = 786
            }
            { body
                  "upgrader-994294-Spawn1"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W12S28" 6 40)
                  None with
                TicksToLive = 968
                Energy = 35
            }
            { body
                  "hauler-993650-Spawn1"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W12S28" 48 32)
                  None with
                TicksToLive = 375
                Energy = 860
            }
            { body
                  "worker-993918-Spawn1"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W12S28" 48 30)
                  None with
                TicksToLive = 649
                Fatigue = 32
                Energy = 850
            }
            { body
                  "hauler-993624-Spawn10"
                  Side.Ours
                  (parts [ Carry, 22; Move, 11 ])
                  (at "W12S26" 31 35)
                  None with
                TicksToLive = 304
                Energy = 1_020
            }
            { body
                  "hauler-993812-Spawn10"
                  Side.Ours
                  (parts [ Carry, 22; Move, 11 ])
                  (at "W12S26" 38 30)
                  None with
                TicksToLive = 492
                Energy = 600
            }
            { body
                  "hauler-993992-Spawn10"
                  Side.Ours
                  (parts [ Carry, 22; Move, 11 ])
                  (at "W12S26" 13 48)
                  None with
                TicksToLive = 672
            }
            { body
                  "upgrader-994266-Spawn10"
                  Side.Ours
                  (parts [ Work, 11; Carry, 1; Move, 11 ])
                  (at "W12S26" 7 35)
                  None with
                TicksToLive = 916
                Energy = 39
            }
            { body
                  "worker-994608-Spawn10"
                  Side.Ours
                  (parts [ Work, 9; Carry, 9; Move, 9 ])
                  (at "W12S26" 42 38)
                  None with
                TicksToLive = 1_270
                Energy = 314
            }
            { body
                  "anchor-994712-Spawn10"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W12S26" 42 39)
                  None with
                TicksToLive = 1_317
                Energy = 50
            }
            // HalfLife418: TODO script; live: no act
            { body
                  "HalfLife418"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                      ])
                  (at "W18S27" 20 2)
                  (Some Hold) with
                TicksToLive = 1_113
            }
            // Force618: TODO script; live: no act
            { body "Force618" trepidimous (parts [ Move, 1 ]) (at "W18S27" 23 1) (Some Hold) with
                TicksToLive = 1_202
            }
            { body
                  "ranger-994340-Spawn9"
                  Side.Ours
                  (parts [ Move, 9; RangedAttack, 6; Heal, 3 ])
                  (at "W18S27" 26 8)
                  None with
                TicksToLive = 975
            }
            // RRP: TODO script; live: harvest→11,5
            { body "RRP" giaco (parts [ Work, 6; Carry, 1; Move, 6 ]) (at "W18S29" 10 6) (Some Hold) with
                TicksToLive = 71
                Energy = 50
            }
            // cS5: TODO script; live: no act
            { body
                  "cS5"
                  giaco
                  (parts
                      [
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                      ])
                  (at "W18S29" 2 22)
                  (Some Hold) with
                TicksToLive = 182
            }
            // odc: TODO script; live: no act
            { body "odc" giaco (parts [ Move, 1 ]) (at "W18S29" 2 16) (Some Hold) with
                TicksToLive = 792
            }
            // bdf: TODO script; live: no act
            { body
                  "bdf"
                  giaco
                  (parts
                      [
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                      ])
                  (at "W18S29" 17 43)
                  (Some Hold) with
                TicksToLive = 846
                Energy = 238
            }
            // viq: TODO script; live: reserveController→38,43
            { body
                  "viq"
                  giaco
                  (parts [ Move, 2; BodyPart.Claim, 2 ])
                  (at "W18S29" 37 42)
                  (Some Hold) with
                TicksToLive = 225
            }
            // oS2: TODO script; live: no act
            { body "oS2" giaco (parts [ Move, 1 ]) (at "W19S28" 2 2) (Some Hold) with
                TicksToLive = 74
            }
            // RTp: TODO script; live: harvest→44,24
            { body
                  "RTp"
                  giaco
                  (parts [ Work, 6; Carry, 1; Move, 6 ])
                  (at "W19S28" 43 23)
                  (Some Hold) with
                TicksToLive = 221
                Energy = 50
            }
            // cUS: TODO script; live: no act
            { body
                  "cUS"
                  giaco
                  (parts
                      [
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                      ])
                  (at "W19S28" 14 45)
                  (Some Hold) with
                TicksToLive = 329
                Energy = 1_200
            }
            // RYR: TODO script; live: harvest→19,40
            { body
                  "RYR"
                  giaco
                  (parts [ Work, 6; Carry, 1; Move, 6 ])
                  (at "W19S28" 20 41)
                  (Some Hold) with
                TicksToLive = 507
                Energy = 50
            }
            // vj2: TODO script; live: reserveController→16,28
            { body
                  "vj2"
                  giaco
                  (parts [ Move, 2; BodyPart.Claim, 2 ])
                  (at "W19S28" 15 29)
                  (Some Hold) with
                TicksToLive = 237
            }
            // uWC: TODO script; live: upgradeController→41,23
            { body "uWC" giaco (parts [ Carry, 6; Work, 15 ]) (at "W19S29" 40 21) (Some Hold) with
                TicksToLive = 392
                Energy = 180
            }
            // cXr: TODO script; live: no act
            { body
                  "cXr"
                  giaco
                  (parts
                      [ Carry, 2; Move, 1; Carry, 2; Move, 1; Carry, 2; Move, 1; Carry, 2; Move, 1 ])
                  (at "W19S29" 27 37)
                  (Some Hold) with
                TicksToLive = 468
            }
            // gZG: TODO script; live: no act
            { body "gZG" giaco (parts [ Carry, 8; Move, 1 ]) (at "W19S29" 42 22) (Some Hold) with
                TicksToLive = 546
                Energy = 400
            }
            // gZh: TODO script; live: no act
            { body "gZh" giaco (parts [ Carry, 4; Move, 1 ]) (at "W19S29" 36 47) (Some Hold) with
                TicksToLive = 561
                Energy = 200
            }
            // dbg: TODO script; live: no act
            { body
                  "dbg"
                  giaco
                  (parts
                      [
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                      ])
                  (at "W19S29" 36 16)
                  (Some Hold) with
                TicksToLive = 729
                Energy = 500
            }
            // dce: TODO script; live: no act
            { body
                  "dce"
                  giaco
                  (parts
                      [
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                      ])
                  (at "W19S29" 38 22)
                  (Some Hold) with
                TicksToLive = 789
                Energy = 500
            }
            // ufW: TODO script; live: upgradeController→41,23
            { body "ufW" giaco (parts [ Carry, 6; Work, 15 ]) (at "W19S29" 40 22) (Some Hold) with
                TicksToLive = 970
                Energy = 225
            }
            // chF: TODO script; live: no act
            { body
                  "chF"
                  giaco
                  (parts
                      [
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                      ])
                  (at "W19S29" 29 39)
                  (Some Hold) with
                TicksToLive = 1_113
            }
            // skO: TODO script; live: harvest→39,13
            { body
                  "skO"
                  giaco
                  (parts [ Carry, 1; Work, 3; Move, 3 ])
                  (at "W19S29" 40 14)
                  (Some Hold) with
                TicksToLive = 1_230
                Energy = 50
            }
            // cZw: TODO script; live: no act
            { body
                  "cZw"
                  giaco
                  (parts
                      [
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                      ])
                  (at "W19S29" 37 46)
                  (Some Hold) with
                TicksToLive = 669
            }
            // Keeper6a8caa89dd4872bccd318e34: TODO script; live: no act
            { body
                  "Keeper6a8caa89dd4872bccd318e34"
                  (Side.Npc "Source Keeper")
                  (parts
                      [
                          Tough, 17
                          Move, 13
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                      ])
                  (at "W16S25" 7 8)
                  (Some Hold) with
                TicksToLive = 526
            }
            // Keeper6a8caa89dd4872bccd318e35: TODO script; live: no act
            { body
                  "Keeper6a8caa89dd4872bccd318e35"
                  (Side.Npc "Source Keeper")
                  (parts
                      [
                          Tough, 17
                          Move, 13
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                      ])
                  (at "W16S25" 41 18)
                  (Some Hold) with
                TicksToLive = 960
            }
            // Keeper6a8caa89dd4872bccd318e38: TODO script; live: no act
            { body
                  "Keeper6a8caa89dd4872bccd318e38"
                  (Side.Npc "Source Keeper")
                  (parts
                      [
                          Tough, 17
                          Move, 13
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                      ])
                  (at "W16S25" 11 38)
                  (Some Hold) with
                TicksToLive = 960
            }
            // Keeper6a8caa89dd4872bccd318e39: TODO script; live: no act
            { body
                  "Keeper6a8caa89dd4872bccd318e39"
                  (Side.Npc "Source Keeper")
                  (parts
                      [
                          Tough, 17
                          Move, 13
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                      ])
                  (at "W16S25" 45 44)
                  (Some Hold) with
                TicksToLive = 960
            }
            // Pendulum654: crossing to W17S25 49,28
            // Pendulum654: TODO script; live: healed→1,27 heal→1,27
            { body
                  "Pendulum654"
                  trepidimous
                  (parts [ Move, 6; Heal, 6 ])
                  (at "W16S25" 0 28)
                  (Some Hold) with
                TicksToLive = 1_271
            }
            // scout-W19S11-993775: TODO script; live: no act
            { body "scout-W19S11-993775" kazkel (parts [ Move, 1 ]) (at "W16S27" 42 2) (Some Hold) with
                TicksToLive = 359
            }
            { body
                  "anchor-993844-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W16S29" 16 42)
                  None with
                TicksToLive = 455
                Energy = 50
            }
            { body
                  "anchor-994292-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W16S29" 22 21)
                  None with
                TicksToLive = 903
                Energy = 50
            }
            { body
                  "hauler-994686-Spawn9"
                  Side.Ours
                  (parts [ Carry, 30; Move, 15 ])
                  (at "W16S29" 6 15)
                  None with
                TicksToLive = 1_402
                Energy = 36
            }
            { body
                  "hauler-994145-Spawn9"
                  Side.Ours
                  (parts [ Carry, 30; Move, 15 ])
                  (at "W16S29" 2 31)
                  None with
                TicksToLive = 861
            }
            // garrison-W14S22-993494: TODO script; live: no act
            { body
                  "garrison-W14S22-993494"
                  odiodin
                  (parts [ Move, 20; RangedAttack, 16; Heal, 4 ])
                  (at "W15S25" 45 8)
                  (Some Hold) with
                TicksToLive = 195
            }
            // garrison-W14S22-993614: TODO script; live: no act
            { body
                  "garrison-W14S22-993614"
                  odiodin
                  (parts [ Move, 20; RangedAttack, 16; Heal, 4 ])
                  (at "W15S25" 44 8)
                  (Some Hold) with
                TicksToLive = 315
            }
            // reactor_guard-W12S23-993895: TODO script; live: no act
            { body
                  "reactor_guard-W12S23-993895"
                  odiodin
                  (parts [ Move, 25; RangedAttack, 20; Heal, 5 ])
                  (at "W15S25" 41 3)
                  (Some Hold) with
                TicksToLive = 626
            }
            // reactor_claimer-W12S23-994532: TODO script; live: no act
            { body
                  "reactor_claimer-W12S23-994532"
                  odiodin
                  (parts [ BodyPart.Claim, 1; Move, 2 ])
                  (at "W15S25" 43 5)
                  (Some Hold) with
                TicksToLive = 222
            }
            { body
                  "anchor-994204-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S27" 13 29)
                  None with
                TicksToLive = 815
                Energy = 50
            }
            { body
                  "reserver-994408-Spawn8"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W15S27" 5 9)
                  None with
                TicksToLive = 95
            }
            { body
                  "hauler-994604-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S27" 11 31)
                  None with
                TicksToLive = 1_329
                Energy = 1_376
            }
            { body
                  "reserver-994574-Spawn8"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W15S29" 13 35)
                  None with
                TicksToLive = 261
            }
            { body
                  "hauler-994430-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S29" 7 9)
                  None with
                TicksToLive = 1_155
                Energy = 536
            }
            { body
                  "anchor-994174-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W14S28" 6 9)
                  None with
                TicksToLive = 785
                Energy = 50
            }
            { body
                  "reserver-994328-Spawn3"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W14S28" 21 14)
                  None with
                TicksToLive = 15
            }
            { body
                  "hauler-994396-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W14S28" 7 12)
                  None with
                TicksToLive = 1_121
            }
            // Blade579: TODO script; live: healed→19,12 heal→19,12
            { body
                  "Blade579"
                  trepidimous
                  (parts [ Move, 11; Heal, 7 ])
                  (at "W18S26" 19 12)
                  (Some Hold) with
                TicksToLive = 214
            }
            // Enigma633: TODO script; live: no act
            { body
                  "Enigma633"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                      ])
                  (at "W18S26" 28 15)
                  (Some Hold) with
                TicksToLive = 295
                Energy = 262
            }
            // Ice714: TODO script; live: harvest→32,4
            { body
                  "Ice714"
                  trepidimous
                  (parts [ Move, 3; Work, 5; Carry, 2 ])
                  (at "W18S26" 31 4)
                  (Some Hold) with
                TicksToLive = 336
                Energy = 70
            }
            // Darkness755: TODO script; live: no act
            { body
                  "Darkness755"
                  trepidimous
                  (parts [ Move, 1; Carry, 16 ])
                  (at "W18S26" 21 15)
                  (Some Hold) with
                TicksToLive = 464
            }
            // Livewire883: TODO script; live: upgradeController→11,21
            { body
                  "Livewire883"
                  trepidimous
                  (parts [ Move, 1; Work, 1; Carry, 1 ])
                  (at "W18S26" 13 19)
                  (Some Hold) with
                TicksToLive = 473
                Energy = 17
            }
            // Exponential046: TODO script; live: no act
            { body
                  "Exponential046"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                      ])
                  (at "W18S26" 23 19)
                  (Some Hold) with
                TicksToLive = 717
            }
            // Plane136: TODO script; live: no act
            { body
                  "Plane136"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                      ])
                  (at "W18S26" 22 14)
                  (Some Hold) with
                TicksToLive = 807
                Energy = 950
            }
            // Amplify226: TODO script; live: harvest→23,43
            { body
                  "Amplify226"
                  trepidimous
                  (parts [ Move, 3; Work, 5; Carry, 2 ])
                  (at "W18S26" 24 42)
                  (Some Hold) with
                TicksToLive = 837
                Energy = 50
            }
            // Bang256: TODO script; live: no act
            { body
                  "Bang256"
                  trepidimous
                  (parts [ Move, 1; Carry, 2; Move, 1; Carry, 1 ])
                  (at "W18S26" 19 14)
                  (Some Hold) with
                TicksToLive = 852
            }
            // Coefficient271: TODO script; live: no act
            { body
                  "Coefficient271"
                  trepidimous
                  (parts [ Move, 1; Carry, 2; Move, 1; Carry, 1 ])
                  (at "W18S26" 21 13)
                  (Some Hold) with
                TicksToLive = 867
                Energy = 150
            }
            // Entropy286: TODO script; live: no act
            { body
                  "Entropy286"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W18S26" 20 11)
                  (Some Hold) with
                TicksToLive = 972
            }
            // Theory720: TODO script; live: healed→21,12 heal→21,12
            { body
                  "Theory720"
                  trepidimous
                  (parts [ Move, 11; Heal, 7 ])
                  (at "W18S26" 21 12)
                  (Some Hold) with
                TicksToLive = 1_355
            }
            // Redshift849: TODO script; live: healed→18,13 heal→18,13
            { body
                  "Redshift849"
                  trepidimous
                  (parts [ Move, 11; Heal, 7 ])
                  (at "W18S26" 17 12)
                  (Some Hold) with
                TicksToLive = 1_496
            }
            // Dread409: TODO script; live: no act
            { body
                  "Dread409"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                      ])
                  (at "W17S24" 22 37)
                  (Some Hold) with
                TicksToLive = 80
                Fatigue = 160
                Energy = 974
            }
            // Specter499: TODO script; live: harvest→13,10
            { body
                  "Specter499"
                  trepidimous
                  (parts [ Move, 3; Work, 5 ])
                  (at "W17S24" 12 11)
                  (Some Hold) with
                TicksToLive = 104
            }
            // Blazer631: TODO script; live: no act
            { body
                  "Blazer631"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                      ])
                  (at "W17S24" 23 30)
                  (Some Hold) with
                TicksToLive = 320
            }
            // Night739: TODO script; live: harvest→18,18
            { body
                  "Night739"
                  trepidimous
                  (parts [ Move, 3; Work, 5 ])
                  (at "W17S24" 17 19)
                  (Some Hold) with
                TicksToLive = 344
            }
            // Reflection150: TODO script; live: no act
            { body
                  "Reflection150"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                      ])
                  (at "W17S24" 10 26)
                  (Some Hold) with
                TicksToLive = 821
                Energy = 750
            }
            // Antimatter240: TODO script; live: upgradeController→9,8
            { body
                  "Antimatter240"
                  trepidimous
                  (parts [ Move, 1; Work, 1; Carry, 1 ])
                  (at "W17S24" 11 11)
                  (Some Hold) with
                TicksToLive = 830
                Energy = 10
            }
            // Convex273: TODO script; live: no act
            { body
                  "Convex273"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                      ])
                  (at "W17S24" 9 30)
                  (Some Hold) with
                TicksToLive = 926
                Energy = 600
            }
            // Exothermic345: TODO script; live: no act
            { body
                  "Exothermic345"
                  trepidimous
                  (parts [ Move, 1; Carry, 16 ])
                  (at "W17S24" 24 36)
                  (Some Hold) with
                TicksToLive = 977
            }
            // Optic627: TODO script; live: no act
            { body
                  "Optic627"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                      ])
                  (at "W17S24" 40 35)
                  (Some Hold) with
                TicksToLive = 1_235
                Energy = 62
            }
        ]

    let a =
        arena
            994_918
            [
                w18s25
                w17s25
                w15s28
                w19s25
                w17s29
                w17s26
                w19s26
                w13s28
                w12s28
                w12s26
                w18s27
                w18s28
                w18s29
                w19s28
                w19s29
                w16s25
                w16s27
                w16s29
                w15s25
                w15s27
                w15s29
                w14s28
                w18s26
                w17s24
            ]
            colonies
            bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before t994,917,
                // read from Memory at live t995,234.
                // TODO ranger-993409-Spawn3: the log holds no task verdict for it that early (or it has died since).
                // TODO worker-993432-Spawn11: the log holds no task verdict for it that early (or it has died since).
                // TODO anchor-993520-Spawn11: the log holds no task verdict for it that early (or it has died since).
                // TODO hauler-993574-Spawn11: the log holds no task verdict for it that early (or it has died since).
                // TODO guard-994071-Spawn11: the log holds no task verdict for it that early (or it has died since).
                // TODO ranger-994875-Spawn11: the log holds no task verdict for it that early (or it has died since).
                // TODO upgrader-993505-Spawn8: the log holds no task verdict for it that early (or it has died since).
                // TODO worker-993452-Spawn9: the log holds no task verdict for it that early (or it has died since).
                // TODO worker-993584-Spawn9: the log holds no task verdict for it that early (or it has died since).
                // TODO guard-993784-Spawn9: the log holds no task verdict for it that early (or it has died since).
                // TODO anchor-993654-Spawn11: the log holds no task verdict for it that early (or it has died since).
                // TODO reserver-994462-Spawn11: the log holds no task verdict for it that early (or it has died since).
                // TODO worker-993508-Spawn2: the log holds no task verdict for it that early (or it has died since).
                // TODO hauler-993532-Spawn6: the log holds no task verdict for it that early (or it has died since).
                // TODO hauler-993624-Spawn10: the log holds no task verdict for it that early (or it has died since).
                // TODO reserver-994408-Spawn8: the log holds no task verdict for it that early (or it has died since).
                // TODO reserver-994574-Spawn8: the log holds no task verdict for it that early (or it has died since).
                // TODO reserver-994328-Spawn3: the log holds no task verdict for it that early (or it has died since).
                // TODO hauler-994772-Spawn8: withdraw:6ac4c16fad3e038630a2ebe1 names a tombstone, which the arena does not place.
                // TODO hauler-993992-Spawn10: withdraw:6ac4c134293c6f62e090bdd2 names a tombstone, which the arena does not place.
                Assignments =
                    Map.ofList
                        [
                            "hauler-994378-Spawn11", "refill:storage-13-27"
                            "ranger-994540-Spawn3", "guard:W17S25"
                            "defender-994619-Spawn11", "defend:W17S25"
                            "hauler-994803-Spawn11", "withdraw:container-27-9"
                            "worker-994186-Spawn8", "upgrade:6a8caa95dd4872bccd319015"
                            "anchor-994580-Spawn8", "harvest:6a8caa95dd4872bccd319013"
                            "anchor-994748-Spawn8", "harvest:6a8caa95dd4872bccd319014"
                            "ranger-994078-Spawn9", "guard:W19S25"
                            "worker-993966-Spawn9", "repair:road-32-36-W17S29"
                            "hauler-994452-Spawn9", "withdraw:container-22-21"
                            "anchor-994587-Spawn9", "harvest:6a8caa7ddd4872bccd318c81"
                            "anchor-994611-Spawn9", "harvest:6a8caa7ddd4872bccd318c83"
                            "upgrader-994821-Spawn9", "withdraw:container-27-42"
                            "ranger-994398-Spawn9", "guard:W19S26"
                            "hauler-993704-Spawn2", "refill:container-22-15"
                            "upgrader-993898-Spawn2", "upgrade:6a8caaaddd4872bccd319363"
                            "anchor-994184-Spawn2", "harvest:6a8caaaddd4872bccd319362"
                            "anchor-994396-Spawn2", "harvest:6a8caaaddd4872bccd319361"
                            "anchor-993806-Spawn1", "harvest:6a8caabadd4872bccd3194aa"
                            "anchor-993828-Spawn6", "harvest:6a8caabadd4872bccd3194a8"
                            "worker-993828-Spawn5", "repair:6abf060f03c73029905f9d4a"
                            "upgrader-994294-Spawn1", "upgrade:6a8caabadd4872bccd3194a9"
                            "hauler-993650-Spawn1", "refill:storage-14-42"
                            "worker-993918-Spawn1", "repair:6abf060f03c73029905f9d4a"
                            "hauler-993812-Spawn10", "refill:spawn-Spawn10"
                            "upgrader-994266-Spawn10", "upgrade:6a8caab9dd4872bccd3194a2"
                            "worker-994608-Spawn10", "repair:rampart-40-37"
                            "anchor-994712-Spawn10", "harvest:6a8caab9dd4872bccd3194a3"
                            "ranger-994340-Spawn9", "guard:W18S27"
                            "anchor-993844-Spawn9", "harvest:6a8caa89dd4872bccd318e4d"
                            "anchor-994292-Spawn9", "harvest:6a8caa89dd4872bccd318e4b"
                            "hauler-994686-Spawn9", "withdraw:container-22-21"
                            "hauler-994145-Spawn9", "withdraw:container-16-42"
                            "anchor-994204-Spawn3", "harvest:6a8caa95dd4872bccd319011"
                            "hauler-994604-Spawn8", "refill:spawn-Spawn3"
                            "hauler-994430-Spawn8", "withdraw:container-17-19"
                            "anchor-994174-Spawn3", "harvest:6a8caaa1dd4872bccd3191f9"
                            "hauler-994396-Spawn3", "withdraw:container-6-9"
                        ]
                // From live Memory at t995,234, 317 ticks after the capture:
                // what it held then, less what is stamped after the tick.
                // Heap only, never in Memory: Healers, ExitWatches and Sightings start empty.
                // Stamped after the tick, not seated unless said: ArmedSeen W17S25 t994,943, ArmedSeen W18S25 t995,032, ArmedSeen W18S26 t995,204, ArmedSeen W19S26 t995,018, ArmedSeen W19S28 t995,200, stand-down W12S28/W16S26 seen t995,192 (LastSeen clamped to the tick, Raiders as read), stand-down W12S28/W18S25 seen t995,032 (LastSeen clamped to the tick, Raiders as read), stand-down W12S28/W18S29 seen t995,198 (LastSeen clamped to the tick, Raiders as read), stand-down W12S28/W19S28 seen t995,200 (LastSeen clamped to the tick, Raiders as read), Threatened W12S28/W11S28 seen t995,234, stand-down W13S28/W16S26 seen t995,192 (LastSeen clamped to the tick, Raiders as read), stand-down W13S28/W18S25 seen t995,032 (LastSeen clamped to the tick, Raiders as read), stand-down W13S28/W18S29 seen t995,198 (LastSeen clamped to the tick, Raiders as read), stand-down W13S28/W19S28 seen t995,200 (LastSeen clamped to the tick, Raiders as read), Threatened W13S28/W11S28 seen t995,234, stand-down W15S28/W16S26 seen t995,192 (LastSeen clamped to the tick, Raiders as read), stand-down W15S28/W18S25 seen t995,032 (LastSeen clamped to the tick, Raiders as read), stand-down W15S28/W18S29 seen t995,198 (LastSeen clamped to the tick, Raiders as read), stand-down W15S28/W19S28 seen t995,200 (LastSeen clamped to the tick, Raiders as read), Threatened W15S28/W11S28 seen t995,234, stand-down W17S29/W16S26 seen t995,192 (LastSeen clamped to the tick, Raiders as read), stand-down W17S29/W18S25 seen t995,032 (LastSeen clamped to the tick, Raiders as read), stand-down W17S29/W18S29 seen t995,198 (LastSeen clamped to the tick, Raiders as read), stand-down W17S29/W19S28 seen t995,200 (LastSeen clamped to the tick, Raiders as read), Threatened W17S29/W11S28 seen t995,234, stand-down W12S26/W16S26 seen t995,192 (LastSeen clamped to the tick, Raiders as read), stand-down W12S26/W18S25 seen t995,032 (LastSeen clamped to the tick, Raiders as read), stand-down W12S26/W18S29 seen t995,198 (LastSeen clamped to the tick, Raiders as read), stand-down W12S26/W19S28 seen t995,200 (LastSeen clamped to the tick, Raiders as read), Threatened W12S26/W11S28 seen t995,234, stand-down W17S25/W16S26 seen t995,192 (LastSeen clamped to the tick, Raiders as read), stand-down W17S25/W18S25 seen t995,032 (LastSeen clamped to the tick, Raiders as read), stand-down W17S25/W18S29 seen t995,198 (LastSeen clamped to the tick, Raiders as read), stand-down W17S25/W19S28 seen t995,200 (LastSeen clamped to the tick, Raiders as read), Threatened W17S25/W11S28 seen t995,234, HomeRaided W17S25 t994,943.
                ArmedSeen = Map.ofList [ "W16S29", 993_826; "W17S24", 994_824; "W19S25", 994_816 ]
                Towered = Set.ofList [ "W12S26"; "W12S28"; "W13S28"; "W15S28"; "W17S25"; "W17S29" ]
                Raids =
                    Map.ofList
                        [
                            "W12S28",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 994_917
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S25"
                                            Opened = 994_881
                                            LastSeen = 994_917
                                            Expiry = 995_081
                                            Basis = StandDownBasis.PlayerRaidHold
                                            Stronghold = false
                                            Bounced = true
                                            HolderUnread = false
                                            Raiders =
                                                Set.ofList
                                                    [
                                                        "6ac4b9d0a78afb3f38184866"
                                                        "6ac4bfc916149cc8bc43ae95"
                                                    ]
                                            PlayerSince = Some 994_881
                                            Chased = true
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S29"
                                            Opened = 986_839
                                            LastSeen = 994_917
                                            Expiry = 997_040
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W19S28"
                                            Opened = 986_841
                                            LastSeen = 994_917
                                            Expiry = 997_160
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                    ]
                                HomeRaided = Some 992_834
                            }
                            "W13S28",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 994_917
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S25"
                                            Opened = 994_881
                                            LastSeen = 994_917
                                            Expiry = 995_081
                                            Basis = StandDownBasis.PlayerRaidHold
                                            Stronghold = false
                                            Bounced = true
                                            HolderUnread = false
                                            Raiders =
                                                Set.ofList
                                                    [
                                                        "6ac4b9d0a78afb3f38184866"
                                                        "6ac4bfc916149cc8bc43ae95"
                                                    ]
                                            PlayerSince = Some 994_881
                                            Chased = true
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S29"
                                            Opened = 986_839
                                            LastSeen = 994_917
                                            Expiry = 997_040
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W19S28"
                                            Opened = 986_841
                                            LastSeen = 994_917
                                            Expiry = 997_160
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                    ]
                                HomeRaided = Some 991_605
                            }
                            "W15S28",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 994_917
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S25"
                                            Opened = 994_881
                                            LastSeen = 994_917
                                            Expiry = 995_081
                                            Basis = StandDownBasis.PlayerRaidHold
                                            Stronghold = false
                                            Bounced = true
                                            HolderUnread = false
                                            Raiders =
                                                Set.ofList
                                                    [
                                                        "6ac4b9d0a78afb3f38184866"
                                                        "6ac4bfc916149cc8bc43ae95"
                                                    ]
                                            PlayerSince = Some 994_881
                                            Chased = true
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S29"
                                            Opened = 986_839
                                            LastSeen = 994_917
                                            Expiry = 997_040
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W19S28"
                                            Opened = 986_841
                                            LastSeen = 994_917
                                            Expiry = 997_160
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                    ]
                            }
                            "W17S29",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 994_917
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S25"
                                            Opened = 994_881
                                            LastSeen = 994_917
                                            Expiry = 995_081
                                            Basis = StandDownBasis.PlayerRaidHold
                                            Stronghold = false
                                            Bounced = true
                                            HolderUnread = false
                                            Raiders =
                                                Set.ofList
                                                    [
                                                        "6ac4b9d0a78afb3f38184866"
                                                        "6ac4bfc916149cc8bc43ae95"
                                                    ]
                                            PlayerSince = Some 994_881
                                            Chased = true
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S29"
                                            Opened = 986_839
                                            LastSeen = 994_917
                                            Expiry = 997_040
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W19S28"
                                            Opened = 986_841
                                            LastSeen = 994_917
                                            Expiry = 997_160
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                    ]
                                HomeRaided = Some 991_430
                            }
                            "W12S26",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 994_917
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S25"
                                            Opened = 994_881
                                            LastSeen = 994_917
                                            Expiry = 995_081
                                            Basis = StandDownBasis.PlayerRaidHold
                                            Stronghold = false
                                            Bounced = true
                                            HolderUnread = false
                                            Raiders =
                                                Set.ofList
                                                    [
                                                        "6ac4b9d0a78afb3f38184866"
                                                        "6ac4bfc916149cc8bc43ae95"
                                                    ]
                                            PlayerSince = Some 994_881
                                            Chased = true
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S29"
                                            Opened = 986_839
                                            LastSeen = 994_917
                                            Expiry = 997_040
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W19S28"
                                            Opened = 986_841
                                            LastSeen = 994_917
                                            Expiry = 997_160
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                    ]
                                HomeRaided = Some 978_444
                            }
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 994_917
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S25"
                                            Opened = 994_881
                                            LastSeen = 994_917
                                            Expiry = 995_081
                                            Basis = StandDownBasis.PlayerRaidHold
                                            Stronghold = false
                                            Bounced = true
                                            HolderUnread = false
                                            Raiders =
                                                Set.ofList
                                                    [
                                                        "6ac4b9d0a78afb3f38184866"
                                                        "6ac4bfc916149cc8bc43ae95"
                                                    ]
                                            PlayerSince = Some 994_881
                                            Chased = true
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S29"
                                            Opened = 986_839
                                            LastSeen = 994_917
                                            Expiry = 997_040
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W19S28"
                                            Opened = 986_841
                                            LastSeen = 994_917
                                            Expiry = 997_160
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                    ]
                            }
                        ]
            }
    }


/// Each tick's world verdict on W18S25 as W17S25 reads it, its answerer, and
/// the assignments after the tick.
let private crippledRun (ticks: int) (start: Arena) =
    ((start, []), List.replicate ticks ())
    ||> List.fold (fun (a, acc) () ->
        let verdict =
            Map.tryFind "W18S25" (viewOf a "W17S25").World.Answers
            |> Option.map (fun v -> v.Value.Answer, v.Value.Answerer)

        let next, _ = step a
        next, (a.Tick, verdict, next.Carried.Assignments) :: acc)
    |> snd
    |> List.rev

[<Tests>]
let w18s25CrippledTests =
    testList
        "arena capture w18s25-crippled"
        [
            // Live, W17S29's guard row won the room on paper, so the verdict
            // bound its Guard to W17S29: its melee guard was offered the
            // chase and could not take it, its ranger walked round from
            // W19S25, and W17S25's residents next door were never counted
            // (t994,918-995,026). The stand-down had withheld W17S25's own
            // row: now its row is priced, shut or not, and
            // the room is the kite its biggest ranger answers. The kill time is the small
            // rangers' either way in the arena (t93 at writing); what this
            // pins is the binding.
            test
                "#573 capture t994,917: the stand-down withholds no row, so the owner's ranger prices the cripple a kite every seeing colony pools" {
                let run = w18s25Crippled () |> crippledRun 50

                for tick, verdict, _ in run do
                    match verdict with
                    | Some(Answer.Win, Some answerer) ->
                        failtest $"t{tick}: a Win bound to {answerer}"
                    | _ -> ()

                Expect.exists
                    run
                    (fun (_, _, assignments) ->
                        Map.tryFind "ranger-993409-Spawn3" assignments = Some "guard:W18S25")
                    "once its home raid is down, W17S25's resident ranger next door takes the chase"
            }

            // The world lane (one match over the fleet): W17S25's own ranger
            // at the fight (W17S25 11,24, by the west exit) holds the chase
            // throughout, its seat no far holder takes (#596); the mother's
            // bigger rangers, which would end it sooner on paper, take the
            // seat the kite adds for spells (t35-t52, t56-t69) and never reach
            // it, so it alone kills at t102. W17S26's trespass, a digger, is
            // W17S25's guard's beside it (#601): W17S25 home's Guard is the
            // mother's resident kind, which the child's own Outpost claim of
            // it gives way to, so the guard is not pulled home and W17S29's
            // far ranger does not take W17S26 from W19S25 and walk there for
            // ever; the digger dies at t19.
            test
                "#573 capture t994,917, the world lane: the fleet body at the fight holds the cripple's chase throughout" {
                let run = w18s25Crippled () |> crippledRun 50

                for tick, verdict, _ in run do
                    match verdict with
                    | Some(Answer.Win, Some answerer) ->
                        failtest $"t{tick}: a Win bound to {answerer}"
                    | _ -> ()

                for tick, _, assignments in run do
                    Expect.equal
                        (Map.tryFind "ranger-994875-Spawn11" assignments)
                        (Some "guard:W18S25")
                        $"t{tick}: the body at the fight holds the chase"

                    Expect.isFalse
                        (Map.exists
                            (fun (name: string) tid ->
                                tid = "guard:W18S25" && name.EndsWith "-Spawn9")
                            assignments)
                        $"t{tick}: no W17S29 body joins it"

                let _, _, first = List.head run

                Expect.equal
                    (Map.tryFind "guard-994071-Spawn11" first)
                    (Some "guard:W17S26")
                    "W17S25's guard beside W17S26 takes its trespass"

                let _, trace = w18s25Crippled () |> Arena.run 30

                Expect.equal
                    (diedOn "Quantum825" trace)
                    (Some 19)
                    $"the digger dies at t19\n{describe trace}"
            }

            test
                "#573 capture t994,917, the world lane: the one ranger on the kite kills the cripple" {
                let _, trace = w18s25Crippled () |> run 120

                // t102.
                Expect.isLessThanOrEqual
                    (diedOn "Faraday387" trace |> Option.defaultValue System.Int32.MaxValue)
                    110
                    $"Faraday387 dies\n{describe trace}"
            }
        ]

// ---- arena capture w17s29-tower-backed (#590) --------------------------------
// `npm run capture -- W17S29 996675 --name w17s29-tower-backed`, read at live
// t999,336. The room-history replay at t996,675, the state after that tick
// ran: arena tick 0 runs as t996,676.
//   W17S29  https://screeps.com/room-history/shardSeason/W17S29/996600.json
// Left out, as the capture says: the Raid log, ArmedSeen, Healers and
// Sightings start empty; no body holds a Task (the Transition log holds none
// that early); the worker Spawn9 casts until t996,741 is not placed; nor are
// two tombstones, nor the extensions' energy beyond what the capture read.

/// W17S29 at t996,675: an Invader pair in the home, its 25-HEAL healer glued
/// to a dismantler whose TOUGH is shot off (900 hits), both ~20 tiles from
/// the two towers, which land 2 × 150 against the healer's 300. Live, the
/// resident ranger-996535 (6 RANGED_ATTACK, 60 a tick) stood in the room ~100
/// ticks and never shot (#590): 60 beside the towers' 300 out-damages the heal.
let private w17s29TowerBacked () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w17s29 =
        room "W17S29"
        |> withController Ownership.Ours None 6 4
        |> withControllerClocks 0 0 119_992 1_503_130
        |> withSpawn "Spawn9" { X = 24; Y = 40 } 2_300
        |> withStructures
            [
                for x, y in
                    [
                        19, 35
                        19, 41
                        19, 43
                        20, 36
                        20, 38
                        20, 40
                        20, 42
                        21, 35
                        21, 37
                        21, 41
                        21, 43
                        22, 36
                        22, 38
                        22, 40
                        23, 35
                        23, 37
                        24, 36
                        26, 44
                        27, 43
                        27, 45
                        28, 44
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        20, 44
                        21, 45
                        22, 42
                        22, 44
                        23, 43
                        23, 45
                        24, 38
                        24, 42
                        24, 44
                        25, 35
                        25, 37
                        25, 39
                        25, 43
                        25, 45
                        26, 36
                        26, 38
                        26, 40
                        27, 35
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 135_200 250_000 { X = 13; Y = 29 } with
                    Energy = 432
                }
                structureOf "container" None 135_500 250_000 { X = 15; Y = 43 }
                structureOf "container" None 191_400 250_000 { X = 27; Y = 42 }
                { structureOf "container" None 190_200 250_000 { X = 44; Y = 38 } with
                    Energy = 264
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 25; Y = 41 } with
                    Energy = 13
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 16; Y = 44 }
                { structureOf "rampart" (Some Side.Ours) 100_201 30_000_000 { X = 13; Y = 29 } with
                    NextDecay = 996_763
                }
                { structureOf "rampart" (Some Side.Ours) 101_601 30_000_000 { X = 21; Y = 39 } with
                    NextDecay = 996_774
                }
                { structureOf "rampart" (Some Side.Ours) 100_201 30_000_000 { X = 23; Y = 39 } with
                    NextDecay = 996_710
                }
                { structureOf "rampart" (Some Side.Ours) 100_201 30_000_000 { X = 23; Y = 41 } with
                    NextDecay = 996_753
                }
                { structureOf "rampart" (Some Side.Ours) 100_601 30_000_000 { X = 24; Y = 40 } with
                    NextDecay = 996_752
                }
                { structureOf "rampart" (Some Side.Ours) 100_701 30_000_000 { X = 26; Y = 42 } with
                    NextDecay = 996_758
                }
                { structureOf "rampart" (Some Side.Ours) 101_601 30_000_000 { X = 44; Y = 38 } with
                    NextDecay = 996_707
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 23; Y = 39 } with
                    Energy = 19_740
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 26; Y = 42 } with
                    Energy = 4_000
                }
                towerOf Side.Ours { X = 21; Y = 39 } 640
                towerOf Side.Ours { X = 23; Y = 41 } 850
            ]

    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W17S29" ])

    let worker name x y ttl =
        { body name Side.Ours (parts [ Work, 11; Carry, 12; Move, 12 ]) (at "W17S29" x y) None with
            TicksToLive = ttl
        }

    let bodies: Body list =
        [
            worker "worker-995080-Spawn9" 42 39 8
            { worker "worker-995234-Spawn9" 26 43 162 with
                Energy = 244
            }
            worker "worker-995430-Spawn9" 20 35 358
            worker "worker-996318-Spawn9" 27 36 1_246
            { body
                  "upgrader-995634-Spawn9"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W17S29" 29 35)
                  None with
                TicksToLive = 550
            }
            { body
                  "upgrader-996423-Spawn9"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W17S29" 28 33)
                  None with
                TicksToLive = 1_339
            }
            { body
                  "anchor-996061-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 44 38)
                  None with
                TicksToLive = 908
                Energy = 50
            }
            { body
                  "anchor-996085-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 13 29)
                  None with
                TicksToLive = 932
                Energy = 50
            }
            { body
                  "hauler-996183-Spawn9"
                  Side.Ours
                  (parts [ Carry, 30; Move, 15 ])
                  (at "W17S29" 20 43)
                  None with
                TicksToLive = 1_141
                Energy = 870
            }
            { body
                  "hauler-995926-Spawn9"
                  Side.Ours
                  (parts [ Carry, 30; Move, 15 ])
                  (at "W17S29" 32 32)
                  None with
                TicksToLive = 884
                Energy = 992
            }
            { body
                  "ranger-996535-Spawn9"
                  Side.Ours
                  (parts [ Move, 9; RangedAttack, 6; Heal, 3 ])
                  (at "W17S29" 11 12)
                  None with
                TicksToLive = 1_412
                Fatigue = 36
            }
            // live: 2 RANGED_ATTACK, 4 WORK, 2 ATTACK, 1 MOVE; attack→43,25.
            // Its live tiles to t996,714, where it struck the anchor's
            // rampart at 44,38 until t996,800 (`observe history`).
            { body
                  "invader_W17S29_832"
                  (Side.Npc "Invader")
                  (parts [ Tough, 16; Move, 24; RangedAttack, 3; Work, 4; Attack, 2; Move, 1 ])
                  (at "W17S29" 44 25)
                  (Some(
                      Waypoints
                          [
                              for x, y in
                                  [
                                      45, 26
                                      46, 27
                                      45, 28
                                      45, 29
                                      45, 30
                                      45, 31
                                      45, 32
                                      45, 33
                                      44, 34
                                      44, 35
                                      43, 36
                                      43, 37
                                      43, 38
                                  ] -> at "W17S29" x y
                          ]
                  )) with
                Hits = 900
                TicksToLive = 1_432
            }
            // live: heal→44,25, then beside the dismantler throughout.
            { body
                  "invader_W17S29_976"
                  (Side.Npc "Invader")
                  (parts [ Move, 24; Heal, 25; Move, 1 ])
                  (at "W17S29" 45 26)
                  (Some(Follow "invader_W17S29_832")) with
                TicksToLive = 1_432
            }
        ]

    arena 996_676 [ w17s29 ] colonies bodies

[<Tests>]
let w17s29TowerBackedTests =
    testList
        "arena capture w17s29-tower-backed"
        [
            // The pricing half of #590, red before it: the home's two loaded
            // towers reach the world's read of the room, and the ranger
            // standing there wins the 900-hit dismantler and its healer beside
            // them (60 + 300 against 300 healed), which it loses alone.
            test
                "#590 capture t996,675: the ranger in the home wins the pair beside the home's towers, and loses it alone" {
                let view = viewOf (w17s29TowerBacked ()) "W17S29"
                let ranger = view.Fighters |> List.filter (fun c -> c.Name = "ranger-996535-Spawn9")

                Expect.equal
                    (Map.tryFind "W17S29" view.World.Rooms
                     |> Option.map (fun seen -> seen.OurTowers))
                    (Some [ { X = 21; Y = 39 }; { X = 23; Y = 41 } ])
                    "the home's own loaded towers, by tile"

                Expect.isTrue
                    (Fabot.Core.Decide.Facts.bodiesWin view "W17S29" ranger)
                    "beside the towers it wins"

                let towerless =
                    { view with
                        World =
                            { view.World with
                                Rooms =
                                    view.World.Rooms
                                    |> Map.change
                                        "W17S29"
                                        (Option.map (fun seen -> { seen with OurTowers = [] }))
                            }
                    }

                Expect.isFalse
                    (Fabot.Core.Decide.Facts.bodiesWin towerless "W17S29" ranger)
                    "alone it loses"
            }

            // Pending: priced right, the ranger still idles, as live. The
            // home's raid outside the line is its Defend's, so the home pools
            // no Guard a ranger takes (`homeUnheld`) and is no chase
            // (`Facts.chased`); and by the time a ranger 40 tiles off walks
            // in, the healer has the dismantler back at 5,000 hits, which
            // the exchange prices a loss (its two ATTACK counted against a
            // ranger at range 3). The invaders keep their live tiles.
            ptest "#590 capture t996,675: the ranger engages beside the towers and the pair dies" {
                let _, trace = w17s29TowerBacked () |> run 150
                let failure = describe trace

                Expect.isTrue
                    (trace
                     |> List.exists (fun t ->
                         t.Ours
                         |> List.exists (function
                             | RangedAttackCreep("ranger-996535-Spawn9", _) -> true
                             | _ -> false)))
                    $"the ranger shoots\n{failure}"

                Expect.isSome (diedOn "invader_W17S29_832" trace) $"the dismantler dies\n{failure}"
                Expect.isSome (diedOn "invader_W17S29_976" trace) $"the healer dies\n{failure}"
            }
        ]

// ---- arena capture w17s29-starved-towers (#591) ---------------------------
// `npm run capture -- W17S29 996765 --rooms W15S28 --name w17s29-starved --no-live-memory`, read at live t999,322.
// The room-history replay at t996,765, the state after that tick ran: arena
// tick 0 runs as t996,766 and decides what our code decided live then.
//   W17S29  https://screeps.com/room-history/shardSeason/W17S29/996700.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/996700.json
// The two Invaders run the engine's NPC script; the Raid log, ArmedSeen,
// Healers and Sightings start empty (the tick is past `--live-memory`).
// Spawn3 is spawning guard-996644-Spawn3 until t996794: not placed.
// Spawn8 is spawning worker-996759-Spawn8 until t996909: not placed.
// not placed: 2 tombstone, 1 ruin (the arena has no drops, tombstones or ruins).

/// W17S29 at t996,765: a dismantler and its healer on the anchor's rampart
/// (44,38, 116k), both towers drained to 300 and 340, three loaded workers
/// and 19k in the Storage.
let private w17s29StarvedTowers () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w17s29 =
        room "W17S29"
        |> withController Ownership.Ours None 6 4
        |> withControllerClocks 0 0 119_902 1_503_130
        |> withSpawn "Spawn9" { X = 24; Y = 40 } 2_300
        |> withStructures
            [
                for x, y in
                    [
                        19, 35
                        19, 41
                        19, 43
                        20, 36
                        20, 38
                        20, 40
                        20, 42
                        20, 44
                        21, 35
                        21, 37
                        21, 41
                        21, 43
                        21, 45
                        22, 36
                        22, 44
                        23, 35
                        23, 37
                        23, 43
                        23, 45
                        24, 36
                        24, 42
                        24, 44
                        25, 35
                        25, 37
                        25, 43
                        25, 45
                        26, 36
                        26, 38
                        26, 44
                        27, 35
                        27, 43
                        27, 45
                        28, 44
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in [ 22, 38; 22, 40; 22, 42; 24, 38; 25, 39; 25, 41; 26, 40 ] ->
                    structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 135_200 250_000 { X = 13; Y = 29 } with
                    Energy = 408
                }
                structureOf "container" None 135_500 250_000 { X = 15; Y = 43 }
                structureOf "container" None 191_400 250_000 { X = 27; Y = 42 }
                { structureOf "container" None 190_200 250_000 { X = 44; Y = 38 } with
                    Energy = 1_188
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 16; Y = 44 }
                { structureOf "rampart" (Some Side.Ours) 99_901 30_000_000 { X = 13; Y = 29 } with
                    NextDecay = 996_862
                }
                { structureOf "rampart" (Some Side.Ours) 101_601 30_000_000 { X = 21; Y = 39 } with
                    NextDecay = 996_774
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 30_000_000 { X = 23; Y = 39 } with
                    NextDecay = 996_809
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 30_000_000 { X = 23; Y = 41 } with
                    NextDecay = 996_852
                }
                { structureOf "rampart" (Some Side.Ours) 100_301 30_000_000 { X = 24; Y = 40 } with
                    NextDecay = 996_851
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 30_000_000 { X = 26; Y = 42 } with
                    NextDecay = 996_857
                }
                { structureOf "rampart" (Some Side.Ours) 116_311 30_000_000 { X = 44; Y = 38 } with
                    NextDecay = 996_806
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 23; Y = 39 } with
                    Energy = 19_140
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 26; Y = 42 } with
                    Energy = 4_000
                }
                towerOf Side.Ours { X = 21; Y = 39 } 340
                towerOf Side.Ours { X = 23; Y = 41 } 300
            ]
        |> withRoads
            [
                13, 29, 4_400
                14, 29, 2_900
                15, 30, 2_800
                16, 31, 2_800
                16, 42, 4_500
                17, 32, 3_100
                17, 42, 15_400
                18, 33, 3_900
                18, 41, 20_600
                19, 34, 17_700
                19, 40, 3_200
                19, 42, 4_300
                20, 35, 13_800
                20, 37, 20_500
                20, 39, 3_000
                20, 41, 3_100
                20, 43, 3_900
                21, 36, 3_800
                21, 38, 4_400
                21, 40, 3_600
                21, 42, 4_000
                21, 44, 4_400
                22, 35, 3_700
                22, 37, 2_800
                22, 39, 4_600
                22, 41, 3_400
                22, 43, 3_570
                22, 45, 4_560
                23, 36, 4_500
                23, 38, 3_700
                23, 42, 3_000
                23, 44, 3_770
                24, 35, 20_500
                24, 37, 2_700
                24, 39, 2_800
                24, 41, 2_900
                24, 43, 2_400
                24, 45, 2_870
                25, 36, 15_500
                25, 38, 2_800
                25, 40, 4_200
                25, 42, 2_700
                25, 44, 2_600
                26, 35, 16_700
                26, 37, 14_100
                26, 39, 19_500
                26, 41, 3_200
                26, 43, 3_400
                26, 45, 3_600
                27, 36, 19_300
                27, 37, 13_500
                27, 38, 12_100
                27, 39, 15_300
                27, 40, 20_100
                27, 41, 3_600
                27, 42, 2_800
                27, 44, 4_400
                28, 36, 19_900
                28, 37, 13_400
                28, 38, 12_100
                28, 39, 12_900
                28, 40, 17_500
                28, 41, 3_700
                28, 43, 2_700
                29, 35, 2_800
                29, 36, 13_400
                29, 37, 20_700
                29, 38, 18_100
                29, 39, 20_100
                29, 40, 14_000
                29, 41, 14_200
                29, 42, 3_400
                30, 34, 4_600
                30, 36, 13_400
                30, 37, 17_200
                30, 40, 21_100
                30, 41, 20_500
                30, 42, 13_800
                31, 33, 4_300
                31, 36, 15_600
                31, 41, 21_100
                31, 42, 15_600
                32, 32, 3_500
                32, 36, 19_400
                32, 41, 3_900
                33, 31, 2_400
                33, 36, 17_400
                33, 37, 18_800
                33, 40, 3_700
                34, 30, 3_000
                34, 39, 3_800
                35, 29, 3_700
                35, 38, 2_700
                36, 28, 4_500
                36, 37, 3_900
                37, 27, 3_600
                37, 38, 3_500
                38, 26, 3_700
                38, 39, 3_300
                39, 25, 3_200
                39, 40, 4_800
                40, 24, 4_400
                40, 41, 3_500
                41, 23, 3_600
                41, 40, 3_600
                42, 24, 4_000
                42, 39, 3_800
                43, 25, 3_460
                44, 25, 3_080
                44, 37, 3_000
                44, 38, 4_400
                45, 26, 1_060
                45, 36, 2_600
                46, 26, 3_770
                46, 35, 3_000
                47, 25, 3_870
                47, 36, 4_400
                48, 20, 4_170
                48, 21, 3_970
                48, 22, 3_770
                48, 23, 4_010
                48, 24, 2_670
                48, 37, 3_000
            ]
        |> withSite Side.Ours "road" None 0 { X = 43; Y = 38 }

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 149_990 6_183_616
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 26
                        13, 25
                        13, 27
                        13, 29
                        14, 26
                        14, 28
                        15, 25
                        15, 27
                        15, 29
                        16, 26
                        16, 30
                        17, 25
                        17, 27
                        18, 26
                        18, 28
                        19, 27
                        20, 26
                        20, 28
                        21, 27
                        21, 29
                        22, 26
                        23, 25
                        23, 27
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 30
                        12, 32
                        12, 34
                        12, 36
                        13, 31
                        13, 33
                        13, 35
                        14, 30
                        14, 32
                        14, 34
                        15, 31
                        15, 33
                        15, 35
                        16, 32
                        16, 34
                        17, 33
                        18, 32
                        18, 34
                        19, 31
                        19, 33
                        20, 30
                        20, 32
                        21, 31
                        21, 33
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 6; Y = 29 } with
                    Energy = 708
                }
                { structureOf "container" None 176_100 250_000 { X = 11; Y = 18 } with
                    Energy = 324
                }
                structureOf "container" None 132_500 250_000 { X = 23; Y = 29 }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 21; Y = 35 } with
                    Id = "extension-21-35-W15S28"
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 23; Y = 35 } with
                    Energy = 100
                    Id = "extension-23-35-W15S28"
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 102_901 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 996_797
                }
                { structureOf "rampart" (Some Side.Ours) 99_801 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 996_856
                }
                { structureOf "rampart" (Some Side.Ours) 102_601 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 996_815
                }
                { structureOf "rampart" (Some Side.Ours) 99_701 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 996_784
                }
                { structureOf "rampart" (Some Side.Ours) 101_701 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 996_857
                }
                { structureOf "rampart" (Some Side.Ours) 102_801 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 996_811
                }
                { structureOf "rampart" (Some Side.Ours) 100_701 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 996_832
                }
                { structureOf "rampart" (Some Side.Ours) 102_001 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 996_781
                }
                { structureOf "rampart" (Some Side.Ours) 102_401 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 996_774
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 24_211
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]
        |> withRoads
            [
                4, 5, 4_200
                5, 4, 2_900
                5, 6, 4_100
                5, 44, 3_200
                6, 3, 4_700
                6, 7, 3_400
                6, 29, 2_900
                6, 43, 2_800
                6, 45, 3_700
                7, 2, 3_600
                7, 8, 4_200
                7, 28, 3_700
                7, 42, 2_600
                7, 46, 3_100
                8, 1, 4_700
                8, 9, 5_000
                8, 28, 5_000
                8, 41, 2_900
                8, 47, 4_900
                9, 10, 3_400
                9, 27, 3_200
                9, 28, 4_100
                9, 40, 2_600
                9, 48, 3_200
                10, 11, 2_600
                10, 26, 3_700
                10, 29, 3_600
                10, 39, 2_900
                11, 12, 4_700
                11, 18, 2_500
                11, 27, 4_900
                11, 30, 3_800
                11, 38, 3_400
                12, 13, 3_000
                12, 17, 3_400
                12, 25, 16_600
                12, 27, 16_500
                12, 28, 4_200
                12, 29, 3_700
                12, 31, 2_600
                12, 33, 13_700
                12, 35, 17_000
                12, 37, 2_800
                13, 14, 3_100
                13, 16, 2_700
                13, 26, 21_900
                13, 28, 2_500
                13, 30, 4_000
                13, 32, 4_600
                13, 34, 2_900
                13, 36, 3_000
                14, 15, 3_400
                14, 25, 16_700
                14, 27, 4_500
                14, 29, 3_100
                14, 31, 3_200
                14, 33, 3_900
                14, 35, 3_300
                15, 15, 2_700
                15, 26, 4_100
                15, 28, 2_500
                15, 30, 4_600
                15, 32, 2_900
                15, 34, 2_900
                16, 16, 2_700
                16, 25, 19_100
                16, 27, 3_700
                16, 29, 4_500
                16, 31, 2_900
                16, 33, 4_900
                16, 35, 2_500
                17, 17, 3_700
                17, 26, 15_400
                17, 28, 3_700
                17, 30, 3_800
                17, 32, 3_600
                17, 34, 3_100
                18, 18, 4_900
                18, 25, 15_700
                18, 27, 15_000
                18, 29, 2_900
                18, 31, 2_700
                18, 33, 3_200
                19, 19, 20_300
                19, 22, 16_600
                19, 24, 21_100
                19, 26, 16_000
                19, 28, 4_200
                19, 30, 2_900
                19, 32, 3_800
                20, 20, 20_600
                20, 21, 17_100
                20, 23, 17_300
                20, 25, 16_400
                20, 27, 20_800
                20, 29, 3_100
                20, 31, 4_900
                20, 33, 19_200
                21, 21, 3_800
                21, 22, 4_500
                21, 23, 4_700
                21, 24, 18_100
                21, 25, 14_600
                21, 26, 18_400
                21, 28, 13_500
                21, 30, 3_500
                21, 32, 14_000
                21, 34, 20_300
                22, 24, 4_000
                22, 25, 14_800
                22, 27, 16_400
                22, 28, 3_400
                22, 29, 2_900
                22, 30, 20_100
                22, 31, 15_300
                22, 32, 17_100
                22, 33, 14_700
                22, 35, 3_600
                23, 26, 16_300
                23, 28, 13_000
                23, 29, 15_700
                23, 30, 16_400
                23, 31, 18_600
                23, 32, 16_900
                23, 33, 20_300
                23, 34, 4_300
                24, 25, 13_100
                24, 28, 17_000
                24, 29, 18_600
                24, 30, 15_900
                24, 33, 12_500
                25, 24, 3_800
                25, 28, 18_200
                25, 29, 17_800
                25, 34, 18_900
                26, 23, 16_500
                27, 22, 13_700
                28, 21, 13_400
                28, 34, 8_100
                29, 20, 15_200
                30, 19, 13_100
                31, 18, 3_300
                32, 17, 3_200
                33, 16, 4_700
                34, 15, 2_500
                35, 14, 3_400
                36, 13, 17_100
                37, 12, 3_500
                38, 11, 2_300
                39, 12, 3_300
                40, 13, 14_900
                41, 14, 13_200
                42, 15, 4_600
                43, 16, 4_700
                44, 17, 3_400
                45, 18, 3_400
                46, 19, 3_400
                47, 20, 4_900
                48, 21, 3_100
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S29"; "W15S28" ])

    let bodies: Body list =
        [
            // invader_W17S29_832: live: attack→44,38 rangedAttack→44,38 healed→41,40
            { body
                  "invader_W17S29_832"
                  (Side.Npc "Invader")
                  (parts [ Tough, 16; Move, 24; RangedAttack, 3; Work, 4; Attack, 2; Move, 1 ])
                  (at "W17S29" 43 39)
                  (Some Invader) with
                TicksToLive = 1_342
            }
            // invader_W17S29_976: live: rangedHeal→43,39
            { body
                  "invader_W17S29_976"
                  (Side.Npc "Invader")
                  (parts [ Move, 24; Heal, 25; Move, 1 ])
                  (at "W17S29" 40 41)
                  (Some Invader) with
                TicksToLive = 1_342
            }
            { body
                  "worker-995234-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 37 38)
                  None with
                TicksToLive = 72
                Energy = 600
            }
            { body
                  "worker-995430-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 37 39)
                  None with
                TicksToLive = 268
                Energy = 504
            }
            { body
                  "upgrader-995634-Spawn9"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W17S29" 29 35)
                  None with
                TicksToLive = 460
            }
            { body
                  "anchor-996061-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 44 38)
                  None with
                TicksToLive = 818
                Energy = 50
            }
            { body
                  "anchor-996085-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 13 29)
                  None with
                TicksToLive = 842
                Energy = 50
            }
            { body
                  "worker-996318-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 44 33)
                  None with
                TicksToLive = 1_156
                Energy = 600
            }
            { body
                  "upgrader-996423-Spawn9"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W17S29" 28 33)
                  None with
                TicksToLive = 1_249
            }
            { body
                  "worker-996636-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 48 23)
                  None with
                TicksToLive = 1_474
            }
            { body
                  "reserver-996741-Spawn9"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W17S29" 43 25)
                  None with
                TicksToLive = 580
            }
            { body
                  "hauler-995926-Spawn9"
                  Side.Ours
                  (parts [ Carry, 30; Move, 15 ])
                  (at "W17S29" 48 35)
                  None with
                TicksToLive = 794
                Energy = 1_500
            }
            { body
                  "worker-995684-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 13 26)
                  None with
                TicksToLive = 567
            }
            { body
                  "anchor-996028-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 785
                Energy = 50
            }
            { body
                  "hauler-996036-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 17 28)
                  None with
                TicksToLive = 913
                Energy = 1_600
            }
            { body
                  "anchor-996217-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 974
                Energy = 50
            }
            { body
                  "hauler-996263-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 19 26)
                  None with
                TicksToLive = 1_140
            }
            { body
                  "upgrader-996497-Spawn3"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 19 24)
                  None with
                TicksToLive = 1_377
            }
            { body
                  "guard-996609-Spawn8"
                  Side.Ours
                  (parts [ Tough, 5; Move, 25; Attack, 15; Heal, 5 ])
                  (at "W15S28" 10 39)
                  None with
                TicksToLive = 1_492
            }
            { body
                  "hauler-996091-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 15 30)
                  None with
                TicksToLive = 968
                Energy = 1_400
            }
            { body
                  "hauler-995872-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 12 13)
                  None with
                TicksToLive = 749
                Energy = 1_544
            }
        ]

    // No Transition-log verdict reaches back to t996,765: every body starts
    // unassigned and the first tick hands out what live held (the workers
    // on rampart-44-38's Repair, the hauler on tower-23-41's Refill).
    arena 996_766 [ w17s29; w15s28 ] colonies bodies

[<Tests>]
let w17s29StarvedTowersTests =
    testList
        "arena capture w17s29-starved-towers"
        [
            // Live from t996,770 the towers sat at their 300 reserve for a
            // hundred ticks with 19k in the Storage: three loaded workers
            // held the struck rampart's Repair parked at the Reach's edge,
            // no walk round to it (the arena reproduced it unchanged), and
            // the towers had spent ~2,000 energy repairing a 112k rampart.
            test
                "#591 capture t996,765: the raided home keeps both towers fed from the Storage, and no tower repairs a rampart that outlasts the raid" {
                let final, trace = w17s29StarvedTowers () |> run 30
                let failure = describe trace

                Expect.isEmpty
                    (trace
                     |> List.collect (fun t -> t.Ours)
                     |> List.filter (function
                         | RepairWithTower _ -> true
                         | _ -> false))
                    $"116k against the dismantler's 200 a tick outlasts the horizon: the energy held\n{failure}"

                for id in [ "tower-21-39"; "tower-23-41" ] do
                    let energy =
                        final.Rooms["W17S29"].Structures
                        |> List.find (fun s -> s.Id = id)
                        |> fun s -> s.Energy

                    Expect.isGreaterThan
                        energy
                        600
                        $"{id} fed (live: 300 for a hundred ticks)\n{failure}"
            }
        ]

// ---- arena capture w17s29-dismantler (#589) ----------------------------------
// `npm run capture -- W17S29 996620 --rooms W15S28 --name w17s29-dismantler`,
// read at live t999,213. The room-history replay at t996,620, the state after
// that tick ran: arena tick 0 runs as t996,621.
//   W17S29  https://screeps.com/room-history/shardSeason/W17S29/996600.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/996600.json
// Left out, as the capture says: the Raid log, ArmedSeen, Healers and
// Sightings start empty; no body holds a Task (the Transition log holds none
// that early); every body not ours Holds; Spawn3's upgrader (until t996,644)
// and Spawn8's guard-996609 (until t996,759) are not placed, so both spawns
// stand idle; nor are three tombstones.

/// W17S29 (RCL6, a 2,300 bank) at t996,620: an Invader dismantler, its
/// 25-HEAL healer and an 18-RANGED_ATTACK shooter in the north of the home,
/// two towers at 910. Live, W17S29 cast workers, a reserver and a hauler for
/// 230 ticks while its mother W15S28, two rooms off, cast the only guard
/// (t996,609, in the home at t996,838): `homeUnheld` handed a child's home to
/// its mother whatever the child could cast (#589).
let private w17s29Dismantler () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w17s29 =
        room "W17S29"
        |> withController Ownership.Ours None 6 4
        |> withControllerClocks 0 0 120_000 1_501_935
        |> withSpawn "Spawn9" { X = 24; Y = 40 } 2_300
        |> withStructures
            [
                for x, y in
                    [
                        19, 35
                        19, 41
                        19, 43
                        20, 36
                        20, 38
                        20, 40
                        20, 42
                        21, 35
                        21, 37
                        21, 41
                        21, 43
                        21, 45
                        22, 36
                        22, 38
                        22, 40
                        22, 42
                        22, 44
                        23, 35
                        23, 37
                        23, 43
                        23, 45
                        24, 36
                        24, 38
                        24, 42
                        25, 35
                        25, 37
                        25, 39
                        25, 41
                        26, 36
                        26, 38
                        26, 40
                        27, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in [ 20, 44; 24, 44; 25, 43; 25, 45; 26, 44; 27, 43; 27, 45; 28, 44 ] ->
                    structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 135_200 250_000 { X = 13; Y = 29 } with
                    Energy = 372
                }
                structureOf "container" None 135_500 250_000 { X = 15; Y = 43 }
                structureOf "container" None 191_400 250_000 { X = 27; Y = 42 }
                { structureOf "container" None 190_200 250_000 { X = 44; Y = 38 } with
                    Energy = 48
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 16; Y = 44 }
                { structureOf "rampart" (Some Side.Ours) 100_501 30_000_000 { X = 13; Y = 29 } with
                    NextDecay = 996_664
                }
                { structureOf "rampart" (Some Side.Ours) 101_901 30_000_000 { X = 21; Y = 39 } with
                    NextDecay = 996_675
                }
                { structureOf "rampart" (Some Side.Ours) 100_201 30_000_000 { X = 23; Y = 39 } with
                    NextDecay = 996_710
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 30_000_000 { X = 23; Y = 41 } with
                    NextDecay = 996_654
                }
                { structureOf "rampart" (Some Side.Ours) 100_901 30_000_000 { X = 24; Y = 40 } with
                    NextDecay = 996_653
                }
                { structureOf "rampart" (Some Side.Ours) 101_001 30_000_000 { X = 26; Y = 42 } with
                    NextDecay = 996_659
                }
                { structureOf "rampart" (Some Side.Ours) 101_601 30_000_000 { X = 44; Y = 38 } with
                    NextDecay = 996_707
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 23; Y = 39 } with
                    Energy = 21_240
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 26; Y = 42 } with
                    Energy = 4_000
                }
                towerOf Side.Ours { X = 21; Y = 39 } 910
                towerOf Side.Ours { X = 23; Y = 41 } 910
            ]
        |> withRoads
            [
                13, 29, 4_400
                14, 29, 2_900
                15, 30, 2_800
                16, 31, 2_800
                16, 42, 4_500
                17, 32, 3_100
                17, 42, 15_400
                18, 33, 3_900
                18, 41, 20_600
                19, 34, 17_700
                19, 40, 3_200
                19, 42, 4_300
                20, 35, 13_800
                20, 37, 20_500
                20, 39, 3_100
                20, 41, 3_100
                20, 43, 3_900
                21, 36, 3_900
                21, 38, 4_500
                21, 40, 3_600
                21, 42, 4_100
                21, 44, 4_400
                22, 35, 3_800
                22, 37, 2_800
                22, 39, 4_600
                22, 41, 3_400
                22, 43, 3_570
                22, 45, 4_560
                23, 36, 4_500
                23, 38, 3_800
                23, 42, 3_100
                23, 44, 3_770
                24, 35, 20_500
                24, 37, 2_800
                24, 39, 2_900
                24, 41, 2_900
                24, 43, 2_500
                24, 45, 2_870
                25, 36, 15_500
                25, 38, 2_800
                25, 40, 4_300
                25, 42, 2_700
                25, 44, 2_600
                26, 35, 16_700
                26, 37, 14_100
                26, 39, 20_000
                26, 41, 3_200
                26, 43, 3_400
                26, 45, 3_600
                27, 36, 19_300
                27, 37, 13_500
                27, 38, 12_600
                27, 39, 15_800
                27, 40, 20_100
                27, 41, 3_600
                27, 42, 2_800
                27, 44, 4_500
                28, 36, 19_900
                28, 37, 13_400
                28, 38, 12_600
                28, 39, 12_900
                28, 40, 17_500
                28, 41, 3_700
                28, 43, 2_700
                29, 35, 2_900
                29, 36, 13_400
                29, 37, 20_700
                29, 38, 18_100
                29, 39, 20_100
                29, 40, 14_000
                29, 41, 14_700
                29, 42, 3_400
                30, 34, 4_600
                30, 36, 13_900
                30, 37, 17_200
                30, 40, 21_100
                30, 41, 20_500
                30, 42, 13_800
                31, 33, 4_400
                31, 36, 15_600
                31, 41, 21_100
                31, 42, 16_100
                32, 32, 3_500
                32, 36, 19_400
                32, 41, 4_000
                33, 31, 2_400
                33, 36, 17_900
                33, 37, 19_300
                33, 40, 3_700
                34, 30, 3_000
                34, 39, 3_800
                35, 29, 3_800
                35, 38, 2_800
                36, 28, 4_500
                36, 37, 3_900
                37, 27, 3_700
                37, 38, 3_600
                38, 26, 3_700
                38, 39, 3_400
                39, 25, 3_200
                39, 40, 4_800
                40, 24, 4_400
                40, 41, 3_600
                41, 23, 3_600
                41, 40, 3_700
                42, 24, 4_000
                42, 39, 3_800
                43, 25, 4_500
                43, 38, 4_100
                44, 25, 4_000
                44, 37, 3_100
                44, 38, 4_400
                45, 26, 2_500
                45, 36, 2_700
                46, 26, 4_100
                46, 35, 3_100
                47, 25, 4_200
                47, 36, 4_400
                48, 20, 4_400
                48, 21, 4_200
                48, 22, 4_100
                48, 23, 4_240
                48, 24, 3_000
                48, 37, 3_000
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 6_180_606
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 26
                        12, 36
                        13, 25
                        13, 27
                        13, 35
                        14, 26
                        14, 28
                        15, 25
                        15, 27
                        15, 29
                        16, 26
                        16, 30
                        16, 32
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        19, 27
                        20, 26
                        21, 27
                        22, 26
                        23, 25
                        23, 27
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 30
                        12, 32
                        12, 34
                        13, 29
                        13, 31
                        13, 33
                        14, 30
                        14, 32
                        14, 34
                        15, 31
                        15, 33
                        15, 35
                        16, 34
                        18, 34
                        19, 31
                        19, 33
                        20, 30
                        20, 32
                        21, 29
                        21, 31
                        21, 33
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 6; Y = 29 } with
                    Energy = 36
                }
                { structureOf "container" None 176_100 250_000 { X = 11; Y = 18 } with
                    Energy = 876
                }
                { structureOf "container" None 132_500 250_000 { X = 23; Y = 29 } with
                    Energy = 742
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 20; Y = 28 } with
                    Energy = 50
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 21; Y = 35 } with
                    Id = "extension-21-35-W15S28"
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 23; Y = 35 } with
                    Id = "extension-23-35-W15S28"
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 100_001 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 996_698
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 996_658
                }
                { structureOf "rampart" (Some Side.Ours) 99_701 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 996_716
                }
                { structureOf "rampart" (Some Side.Ours) 101_801 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 996_629
                }
                { structureOf "rampart" (Some Side.Ours) 102_301 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 996_659
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 996_712
                }
                { structureOf "rampart" (Some Side.Ours) 101_301 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 996_634
                }
                { structureOf "rampart" (Some Side.Ours) 102_301 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 996_682
                }
                { structureOf "rampart" (Some Side.Ours) 102_701 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 996_675
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 24_317
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]
        |> withRoads
            [
                4, 5, 4_200
                5, 4, 2_900
                5, 6, 4_100
                5, 44, 3_200
                6, 3, 4_700
                6, 7, 3_400
                6, 29, 2_900
                6, 43, 2_800
                6, 45, 3_700
                7, 2, 3_600
                7, 8, 4_200
                7, 28, 3_800
                7, 42, 2_600
                7, 46, 3_100
                8, 1, 4_800
                8, 9, 5_000
                8, 28, 5_000
                8, 41, 2_900
                8, 47, 4_900
                9, 10, 3_500
                9, 27, 3_300
                9, 28, 4_200
                9, 40, 2_600
                9, 48, 3_200
                10, 11, 2_600
                10, 26, 3_800
                10, 29, 3_700
                10, 39, 2_900
                11, 12, 4_700
                11, 18, 2_500
                11, 27, 5_000
                11, 30, 3_800
                11, 38, 3_400
                12, 13, 3_100
                12, 17, 3_400
                12, 25, 16_600
                12, 27, 17_000
                12, 28, 4_200
                12, 29, 3_700
                12, 31, 2_600
                12, 33, 13_700
                12, 35, 17_000
                12, 37, 2_800
                13, 14, 3_100
                13, 16, 2_700
                13, 26, 21_900
                13, 28, 2_500
                13, 30, 4_000
                13, 32, 4_600
                13, 34, 2_900
                13, 36, 3_000
                14, 15, 3_400
                14, 25, 16_700
                14, 27, 4_500
                14, 29, 3_100
                14, 31, 3_200
                14, 33, 3_900
                14, 35, 3_400
                15, 15, 2_800
                15, 26, 4_100
                15, 28, 2_500
                15, 30, 4_600
                15, 32, 3_000
                15, 34, 2_900
                16, 16, 2_800
                16, 25, 19_100
                16, 27, 3_800
                16, 29, 4_500
                16, 31, 3_000
                16, 33, 4_900
                16, 35, 2_600
                17, 17, 3_800
                17, 26, 15_900
                17, 28, 3_800
                17, 30, 3_900
                17, 32, 3_600
                17, 34, 3_100
                18, 18, 4_900
                18, 25, 16_200
                18, 27, 15_500
                18, 29, 2_900
                18, 31, 2_700
                18, 33, 3_200
                19, 19, 20_800
                19, 22, 16_600
                19, 24, 21_600
                19, 26, 16_500
                19, 28, 4_200
                19, 30, 2_900
                19, 32, 3_800
                20, 20, 21_100
                20, 21, 17_600
                20, 23, 17_800
                20, 25, 16_900
                20, 27, 20_800
                20, 29, 3_100
                20, 31, 4_900
                20, 33, 19_200
                21, 21, 3_800
                21, 22, 4_500
                21, 23, 4_700
                21, 24, 18_100
                21, 25, 14_600
                21, 26, 18_900
                21, 28, 14_000
                21, 30, 3_500
                21, 32, 14_000
                21, 34, 20_300
                22, 24, 4_000
                22, 25, 14_800
                22, 27, 16_400
                22, 28, 3_400
                22, 29, 2_900
                22, 30, 20_100
                22, 31, 15_300
                22, 32, 17_100
                22, 33, 14_700
                22, 35, 3_600
                23, 26, 16_300
                23, 28, 13_000
                23, 29, 15_700
                23, 30, 16_400
                23, 31, 18_600
                23, 32, 16_900
                23, 33, 20_300
                23, 34, 4_300
                24, 25, 13_100
                24, 28, 17_000
                24, 29, 18_600
                24, 30, 15_900
                24, 33, 12_500
                25, 24, 3_800
                25, 28, 18_200
                25, 29, 17_800
                25, 34, 18_900
                26, 23, 16_500
                27, 22, 13_700
                28, 21, 13_400
                28, 34, 8_100
                29, 20, 15_200
                30, 19, 13_600
                31, 18, 3_300
                32, 17, 3_300
                33, 16, 4_700
                34, 15, 2_600
                35, 14, 3_400
                36, 13, 17_100
                37, 12, 3_600
                38, 11, 2_400
                39, 12, 3_400
                40, 13, 14_900
                41, 14, 13_700
                42, 15, 4_700
                43, 16, 4_700
                44, 17, 3_500
                45, 18, 3_500
                46, 19, 3_500
                47, 20, 4_900
                48, 21, 3_200
            ]

    // Today's declarations, not the tick's: the two homes' are unchanged.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S29"; "W15S28" ])

    let bodies: Body list =
        [
            { body
                  "worker-995080-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 27 36)
                  None with
                TicksToLive = 63
                Energy = 435
            }
            { body
                  "worker-995234-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 33 40)
                  None with
                TicksToLive = 217
                Energy = 248
            }
            { body
                  "worker-995430-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 40 41)
                  None with
                TicksToLive = 413
                Energy = 516
            }
            { body
                  "upgrader-995634-Spawn9"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W17S29" 29 35)
                  None with
                TicksToLive = 605
            }
            { body
                  "anchor-996061-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 44 38)
                  None with
                TicksToLive = 963
                Energy = 50
            }
            { body
                  "anchor-996085-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 13 29)
                  None with
                TicksToLive = 987
                Energy = 50
            }
            { body
                  "hauler-996183-Spawn9"
                  Side.Ours
                  (parts [ Carry, 30; Move, 15 ])
                  (at "W17S29" 22 43)
                  None with
                TicksToLive = 1_196
                Energy = 423
            }
            { body
                  "worker-996318-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 33 36)
                  None with
                TicksToLive = 1_301
                Energy = 512
            }
            { body
                  "upgrader-996423-Spawn9"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W17S29" 28 33)
                  None with
                TicksToLive = 1_394
            }
            { body
                  "ranger-996535-Spawn9"
                  Side.Ours
                  (parts [ Move, 9; RangedAttack, 6; Heal, 3 ])
                  (at "W17S29" 42 23)
                  None with
                TicksToLive = 1_467
            }
            { body
                  "invader_W17S29_832"
                  (Side.Npc "Invader")
                  (parts [ Tough, 16; Move, 24; RangedAttack, 3; Work, 4; Attack, 2; Move, 1 ])
                  (at "W17S29" 41 8)
                  (Some Invader) with
                TicksToLive = 1_487
            }
            // live: rangedHeal→42,9
            { body
                  "invader_W17S29_976"
                  (Side.Npc "Invader")
                  (parts [ Move, 24; Heal, 25; Move, 1 ])
                  (at "W17S29" 41 9)
                  (Some Invader) with
                TicksToLive = 1_487
            }
            // live: attacked→21,39 healed→40,8
            { body
                  "invader_W17S29_855"
                  (Side.Npc "Invader")
                  (parts [ Tough, 6; Move, 24; RangedAttack, 18; Work, 1; Move, 1 ])
                  (at "W17S29" 42 9)
                  (Some Invader) with
                Hits = 3_100
                TicksToLive = 1_487
            }
            { body
                  "upgrader-995000-Spawn3"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 25
                Energy = 26
            }
            { body
                  "worker-995684-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 16 31)
                  None with
                TicksToLive = 712
                Energy = 402
            }
            { body
                  "anchor-996028-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 930
                Energy = 50
            }
            { body
                  "hauler-996036-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 17 30)
                  None with
                TicksToLive = 1_058
                Energy = 1_300
            }
            { body
                  "hauler-996091-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 16 29)
                  None with
                TicksToLive = 1_113
                Energy = 1_300
            }
            { body
                  "anchor-996217-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_119
                Energy = 48
            }
            { body
                  "hauler-996263-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 20 27)
                  None with
                TicksToLive = 1_285
                Energy = 1_600
            }
            { body
                  "worker-996313-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 12 35)
                  None with
                TicksToLive = 1_341
                Energy = 650
            }
            { body
                  "hauler-995872-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 11 12)
                  None with
                TicksToLive = 894
                Energy = 1_402
            }
        ]

    arena 996_621 [ w17s29; w15s28 ] colonies bodies

/// The rows our spawns ask to cast on a run's first tick, by spawn.
let private firstCasts (trace: TickTrace list) =
    trace
    |> List.head
    |> fun t ->
        t.Ours
        |> List.choose (function
            | SpawnCreep(spawn, _, name) -> Some(spawn, name.Split('-')[0])
            | _ -> None)

let private isFighterRow (_, row) = row = "guard" || row = "ranger"

[<Tests>]
let w17s29DismantlerTests =
    testList
        "arena capture w17s29-dismantler"
        [
            // The decision side reproduces live: W15S28 casts the guard,
            // Spawn9 a hauler. Now by cost, not kinship: the trio's 270 a
            // tick, focused, kills a 3-block guard in ~13 ticks, so neither
            // one body a 2,300 bank buys nor the two its row casts win, and
            // W15S28's do. The arena's fight does not reproduce live
            // (#592); only the cast is pinned.
            test
                "#589 capture t996,620: a raid the home's own cast loses is cast by the colony whose cast wins" {
                let start = w17s29Dismantler ()
                let _, trace = start |> run 1
                let casts = firstCasts trace

                Expect.equal
                    (Fabot.Core.Decide.Facts.verdictOf (viewOf start "W17S29") "W17S29").Defender
                    (Some "W15S28")
                    $"the verdict names W15S28\n{casts}"

                Expect.exists
                    casts
                    (fun (spawn, row) -> (spawn = "Spawn3" || spawn = "Spawn8") && row = "guard")
                    $"W15S28 casts the guard\n{casts}"

                Expect.isFalse
                    (casts
                     |> List.exists (fun (spawn, row) ->
                         spawn = "Spawn9" && isFighterRow (spawn, row)))
                    $"and W17S29 none\n{casts}"
            }

            // What the pricing above says, fought: two 3-block guards, the
            // pair W17S29's row would cast, standing beside the trio and
            // holding its Guard (named W15S28's, whose pool holds it), lose.
            test "#589 capture t996,620: two of the home's 3-block guards lose the trio" {
                let start = w17s29Dismantler ()

                let block =
                    List.replicate 3 Fabot.Core.Decide.Bodies.guardPattern.Block |> List.concat

                let pair =
                    [ "guard-996600-Spawn3", 40, 11; "guard-996601-Spawn3", 42, 11 ]
                    |> List.map (fun (name, x, y) ->
                        body name Side.Ours block (RoomPos.at "W17S29" { X = x; Y = y }) None)

                let fought =
                    { start with
                        Bodies = start.Bodies @ pair
                        Carried =
                            { start.Carried with
                                Assignments =
                                    pair |> List.map (fun b -> b.Id, "guard:W17S29") |> Map.ofList
                            }
                    }

                let _, trace = fought |> run 150
                let failure = describe trace

                Expect.isTrue
                    (pair |> List.forall (fun b -> Option.isSome (diedOn b.Id trace)))
                    $"both guards die\n{failure}"
            }

            // The flip: live the shooter fell to the towers at t996,658,
            // leaving the dismantler and its healer, which a body the home
            // buys wins beside its towers (#590's pricing); and the home
            // still built workers, its mother's guard 180 ticks away, as
            // `homeUnheld` refused a child's home its own row. Red before.
            test
                "#589 capture t996,620, the shooter dead: the home casts its own defence, and its mother none" {
                let start = w17s29Dismantler ()

                let shrunk =
                    { start with
                        Bodies = start.Bodies |> List.filter (fun b -> b.Id <> "invader_W17S29_855")
                    }

                let _, trace = shrunk |> run 1
                let casts = firstCasts trace

                Expect.exists
                    casts
                    (fun (spawn, row) -> spawn = "Spawn9" && isFighterRow (spawn, row))
                    $"Spawn9 casts a fighter for its home\n{casts}"

                Expect.isFalse
                    (casts
                     |> List.exists (fun (spawn, row) ->
                         spawn <> "Spawn9" && isFighterRow (spawn, row)))
                    $"and W15S28, which lands later, none\n{casts}"
            }
        ]

// ---- arena capture w17s29-glued-healer (#592) -------------------------------
// `npm run capture -- W17S29 996620 --rooms W15S28,W15S29,W16S29 --name w17s29-glued-healer`, read at live t1,000,473.
// The room-history replay at t996,620, the state after that tick ran: arena
// tick 0 runs as t996,621 and decides what our code decided live then.
//   W17S29  https://screeps.com/room-history/shardSeason/W17S29/996600.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/996600.json
//   W15S29  https://screeps.com/room-history/shardSeason/W15S29/996600.json
//   W16S29  https://screeps.com/room-history/shardSeason/W16S29/996600.json
// The three Invaders run the engine's NPC script; the Raid log, ArmedSeen,
// Healers and Sightings start empty (the tick is past `--live-memory`).
// W15S29 and W16S29 are reserved by us: the arena has no reservations.
// not placed: 3 tombstone (the arena has no drops, tombstones or ruins).
// Left out: ranger-996535-Spawn9 (42,23), which live walked off west at
// t996,621 and out of the north exit at t996,714 on a task the Transition
// log no longer holds, shooting nothing; the arena's idle one shoots the
// dismantler from 3.

/// W17S29 at t996,620: an Invader dismantler, its 25-HEAL healer and a hurt
/// shooter on the north edge, two towers at 910, and W15S28's guard in its
/// spawn (live it reached W17S29 at t996,838 by W15S29 and W16S29).
let private w17s29GluedHealer () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }

    let w17s29 =
        room "W17S29"
        |> withController Ownership.Ours None 6 4
        |> withControllerClocks 0 0 120_000 1_501_935
        |> withSpawn "Spawn9" { X = 24; Y = 40 } 2_300
        |> withStructures
            [
                for x, y in
                    [
                        19, 35
                        19, 41
                        19, 43
                        20, 36
                        20, 38
                        20, 40
                        20, 42
                        21, 35
                        21, 37
                        21, 41
                        21, 43
                        21, 45
                        22, 36
                        22, 38
                        22, 40
                        22, 42
                        22, 44
                        23, 35
                        23, 37
                        23, 43
                        23, 45
                        24, 36
                        24, 38
                        24, 42
                        25, 35
                        25, 37
                        25, 39
                        25, 41
                        26, 36
                        26, 38
                        26, 40
                        27, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in [ 20, 44; 24, 44; 25, 43; 25, 45; 26, 44; 27, 43; 27, 45; 28, 44 ] ->
                    structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 135_200 250_000 { X = 13; Y = 29 } with
                    Energy = 372
                }
                structureOf "container" None 135_500 250_000 { X = 15; Y = 43 }
                structureOf "container" None 191_400 250_000 { X = 27; Y = 42 }
                { structureOf "container" None 190_200 250_000 { X = 44; Y = 38 } with
                    Energy = 48
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 16; Y = 44 }
                { structureOf "rampart" (Some Side.Ours) 100_501 30_000_000 { X = 13; Y = 29 } with
                    NextDecay = 996_664
                }
                { structureOf "rampart" (Some Side.Ours) 101_901 30_000_000 { X = 21; Y = 39 } with
                    NextDecay = 996_675
                }
                { structureOf "rampart" (Some Side.Ours) 100_201 30_000_000 { X = 23; Y = 39 } with
                    NextDecay = 996_710
                }
                { structureOf "rampart" (Some Side.Ours) 100_501 30_000_000 { X = 23; Y = 41 } with
                    NextDecay = 996_654
                }
                { structureOf "rampart" (Some Side.Ours) 100_901 30_000_000 { X = 24; Y = 40 } with
                    NextDecay = 996_653
                }
                { structureOf "rampart" (Some Side.Ours) 101_001 30_000_000 { X = 26; Y = 42 } with
                    NextDecay = 996_659
                }
                { structureOf "rampart" (Some Side.Ours) 101_601 30_000_000 { X = 44; Y = 38 } with
                    NextDecay = 996_707
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 23; Y = 39 } with
                    Energy = 21_240
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 26; Y = 42 } with
                    Energy = 4_000
                }
                towerOf Side.Ours { X = 21; Y = 39 } 910
                towerOf Side.Ours { X = 23; Y = 41 } 910
            ]
        |> withRoads
            [
                13, 29, 4_400
                14, 29, 2_900
                15, 30, 2_800
                16, 31, 2_800
                16, 42, 4_500
                17, 32, 3_100
                17, 42, 15_400
                18, 33, 3_900
                18, 41, 20_600
                19, 34, 17_700
                19, 40, 3_200
                19, 42, 4_300
                20, 35, 13_800
                20, 37, 20_500
                20, 39, 3_100
                20, 41, 3_100
                20, 43, 3_900
                21, 36, 3_900
                21, 38, 4_500
                21, 40, 3_600
                21, 42, 4_100
                21, 44, 4_400
                22, 35, 3_800
                22, 37, 2_800
                22, 39, 4_600
                22, 41, 3_400
                22, 43, 3_570
                22, 45, 4_560
                23, 36, 4_500
                23, 38, 3_800
                23, 42, 3_100
                23, 44, 3_770
                24, 35, 20_500
                24, 37, 2_800
                24, 39, 2_900
                24, 41, 2_900
                24, 43, 2_500
                24, 45, 2_870
                25, 36, 15_500
                25, 38, 2_800
                25, 40, 4_300
                25, 42, 2_700
                25, 44, 2_600
                26, 35, 16_700
                26, 37, 14_100
                26, 39, 20_000
                26, 41, 3_200
                26, 43, 3_400
                26, 45, 3_600
                27, 36, 19_300
                27, 37, 13_500
                27, 38, 12_600
                27, 39, 15_800
                27, 40, 20_100
                27, 41, 3_600
                27, 42, 2_800
                27, 44, 4_500
                28, 36, 19_900
                28, 37, 13_400
                28, 38, 12_600
                28, 39, 12_900
                28, 40, 17_500
                28, 41, 3_700
                28, 43, 2_700
                29, 35, 2_900
                29, 36, 13_400
                29, 37, 20_700
                29, 38, 18_100
                29, 39, 20_100
                29, 40, 14_000
                29, 41, 14_700
                29, 42, 3_400
                30, 34, 4_600
                30, 36, 13_900
                30, 37, 17_200
                30, 40, 21_100
                30, 41, 20_500
                30, 42, 13_800
                31, 33, 4_400
                31, 36, 15_600
                31, 41, 21_100
                31, 42, 16_100
                32, 32, 3_500
                32, 36, 19_400
                32, 41, 4_000
                33, 31, 2_400
                33, 36, 17_900
                33, 37, 19_300
                33, 40, 3_700
                34, 30, 3_000
                34, 39, 3_800
                35, 29, 3_800
                35, 38, 2_800
                36, 28, 4_500
                36, 37, 3_900
                37, 27, 3_700
                37, 38, 3_600
                38, 26, 3_700
                38, 39, 3_400
                39, 25, 3_200
                39, 40, 4_800
                40, 24, 4_400
                40, 41, 3_600
                41, 23, 3_600
                41, 40, 3_700
                42, 24, 4_000
                42, 39, 3_800
                43, 25, 4_500
                43, 38, 4_100
                44, 25, 4_000
                44, 37, 3_100
                44, 38, 4_400
                45, 26, 2_500
                45, 36, 2_700
                46, 26, 4_100
                46, 35, 3_100
                47, 25, 4_200
                47, 36, 4_400
                48, 20, 4_400
                48, 21, 4_200
                48, 22, 4_100
                48, 23, 4_240
                48, 24, 3_000
                48, 37, 3_000
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 6_180_606
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 26
                        12, 36
                        13, 25
                        13, 27
                        13, 35
                        14, 26
                        14, 28
                        15, 25
                        15, 27
                        15, 29
                        16, 26
                        16, 30
                        16, 32
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        19, 27
                        20, 26
                        21, 27
                        22, 26
                        23, 25
                        23, 27
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 30
                        12, 32
                        12, 34
                        13, 29
                        13, 31
                        13, 33
                        14, 30
                        14, 32
                        14, 34
                        15, 31
                        15, 33
                        15, 35
                        16, 34
                        18, 34
                        19, 31
                        19, 33
                        20, 30
                        20, 32
                        21, 29
                        21, 31
                        21, 33
                    ] -> structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 6; Y = 29 } with
                    Energy = 36
                }
                { structureOf "container" None 176_100 250_000 { X = 11; Y = 18 } with
                    Energy = 876
                }
                { structureOf "container" None 132_500 250_000 { X = 23; Y = 29 } with
                    Energy = 742
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 20; Y = 28 } with
                    Energy = 50
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 21; Y = 35 } with
                    Id = "extension-21-35-W15S28"
                }
                { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = 23; Y = 35 } with
                    Id = "extension-23-35-W15S28"
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 100_001 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 996_698
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 996_658
                }
                { structureOf "rampart" (Some Side.Ours) 99_701 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 996_716
                }
                { structureOf "rampart" (Some Side.Ours) 101_801 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 996_629
                }
                { structureOf "rampart" (Some Side.Ours) 102_301 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 996_659
                }
                { structureOf "rampart" (Some Side.Ours) 99_901 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 996_712
                }
                { structureOf "rampart" (Some Side.Ours) 101_301 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 996_634
                }
                { structureOf "rampart" (Some Side.Ours) 102_301 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 996_682
                }
                { structureOf "rampart" (Some Side.Ours) 102_701 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 996_675
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 24_317
                    Thorium = 41_977
                }
                structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 }
            ]
        |> withRoads
            [
                4, 5, 4_200
                5, 4, 2_900
                5, 6, 4_100
                5, 44, 3_200
                6, 3, 4_700
                6, 7, 3_400
                6, 29, 2_900
                6, 43, 2_800
                6, 45, 3_700
                7, 2, 3_600
                7, 8, 4_200
                7, 28, 3_800
                7, 42, 2_600
                7, 46, 3_100
                8, 1, 4_800
                8, 9, 5_000
                8, 28, 5_000
                8, 41, 2_900
                8, 47, 4_900
                9, 10, 3_500
                9, 27, 3_300
                9, 28, 4_200
                9, 40, 2_600
                9, 48, 3_200
                10, 11, 2_600
                10, 26, 3_800
                10, 29, 3_700
                10, 39, 2_900
                11, 12, 4_700
                11, 18, 2_500
                11, 27, 5_000
                11, 30, 3_800
                11, 38, 3_400
                12, 13, 3_100
                12, 17, 3_400
                12, 25, 16_600
                12, 27, 17_000
                12, 28, 4_200
                12, 29, 3_700
                12, 31, 2_600
                12, 33, 13_700
                12, 35, 17_000
                12, 37, 2_800
                13, 14, 3_100
                13, 16, 2_700
                13, 26, 21_900
                13, 28, 2_500
                13, 30, 4_000
                13, 32, 4_600
                13, 34, 2_900
                13, 36, 3_000
                14, 15, 3_400
                14, 25, 16_700
                14, 27, 4_500
                14, 29, 3_100
                14, 31, 3_200
                14, 33, 3_900
                14, 35, 3_400
                15, 15, 2_800
                15, 26, 4_100
                15, 28, 2_500
                15, 30, 4_600
                15, 32, 3_000
                15, 34, 2_900
                16, 16, 2_800
                16, 25, 19_100
                16, 27, 3_800
                16, 29, 4_500
                16, 31, 3_000
                16, 33, 4_900
                16, 35, 2_600
                17, 17, 3_800
                17, 26, 15_900
                17, 28, 3_800
                17, 30, 3_900
                17, 32, 3_600
                17, 34, 3_100
                18, 18, 4_900
                18, 25, 16_200
                18, 27, 15_500
                18, 29, 2_900
                18, 31, 2_700
                18, 33, 3_200
                19, 19, 20_800
                19, 22, 16_600
                19, 24, 21_600
                19, 26, 16_500
                19, 28, 4_200
                19, 30, 2_900
                19, 32, 3_800
                20, 20, 21_100
                20, 21, 17_600
                20, 23, 17_800
                20, 25, 16_900
                20, 27, 20_800
                20, 29, 3_100
                20, 31, 4_900
                20, 33, 19_200
                21, 21, 3_800
                21, 22, 4_500
                21, 23, 4_700
                21, 24, 18_100
                21, 25, 14_600
                21, 26, 18_900
                21, 28, 14_000
                21, 30, 3_500
                21, 32, 14_000
                21, 34, 20_300
                22, 24, 4_000
                22, 25, 14_800
                22, 27, 16_400
                22, 28, 3_400
                22, 29, 2_900
                22, 30, 20_100
                22, 31, 15_300
                22, 32, 17_100
                22, 33, 14_700
                22, 35, 3_600
                23, 26, 16_300
                23, 28, 13_000
                23, 29, 15_700
                23, 30, 16_400
                23, 31, 18_600
                23, 32, 16_900
                23, 33, 20_300
                23, 34, 4_300
                24, 25, 13_100
                24, 28, 17_000
                24, 29, 18_600
                24, 30, 15_900
                24, 33, 12_500
                25, 24, 3_800
                25, 28, 18_200
                25, 29, 17_800
                25, 34, 18_900
                26, 23, 16_500
                27, 22, 13_700
                28, 21, 13_400
                28, 34, 8_100
                29, 20, 15_200
                30, 19, 13_600
                31, 18, 3_300
                32, 17, 3_300
                33, 16, 4_700
                34, 15, 2_600
                35, 14, 3_400
                36, 13, 17_100
                37, 12, 3_600
                38, 11, 2_400
                39, 12, 3_400
                40, 13, 14_900
                41, 14, 13_700
                42, 15, 4_700
                43, 16, 4_700
                44, 17, 3_500
                45, 18, 3_500
                46, 19, 3_500
                47, 20, 4_900
                48, 21, 3_200
            ]

    let w15s29 =
        room "W15S29"
        |> withStructures
            [
                { structureOf "container" None 121_300 250_000 { X = 17; Y = 19 } with
                    Energy = 552
                }
            ]
        |> withRoads
            [
                4, 6, 4_700
                5, 5, 3_580
                5, 7, 4_200
                6, 4, 2_500
                6, 8, 4_500
                7, 3, 3_000
                7, 9, 4_900
                8, 2, 3_300
                8, 10, 4_500
                9, 1, 3_800
                9, 11, 4_000
                10, 12, 4_000
                11, 13, 2_800
                12, 14, 3_500
                13, 15, 2_900
                14, 16, 3_200
                15, 17, 4_800
                16, 18, 4_800
            ]

    let w16s29 =
        room "W16S29"
        |> withStructures
            [
                { structureOf "container" None 141_400 250_000 { X = 16; Y = 42 } with
                    Energy = 618
                }
                { structureOf "container" None 125_100 250_000 { X = 22; Y = 21 } with
                    Energy = 872
                }
            ]
        |> withRoads
            [
                1, 20, 4_110
                1, 37, 4_010
                2, 19, 4_130
                2, 38, 2_830
                3, 18, 3_710
                3, 39, 3_100
                4, 18, 4_900
                4, 40, 2_800
                5, 18, 4_110
                5, 41, 2_500
                6, 18, 3_110
                6, 42, 2_500
                7, 18, 3_450
                7, 41, 3_200
                8, 18, 2_750
                8, 40, 3_500
                9, 18, 3_150
                9, 39, 4_100
                10, 18, 3_550
                10, 40, 4_100
                11, 18, 4_130
                11, 40, 2_700
                12, 18, 2_930
                12, 40, 3_700
                13, 18, 4_230
                13, 41, 4_500
                14, 18, 4_230
                14, 41, 2_880
                15, 18, 4_680
                15, 41, 3_200
                16, 18, 3_380
                17, 18, 4_300
                18, 18, 4_400
                19, 18, 4_100
                20, 19, 3_400
                21, 20, 4_200
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S29"; "W15S28" ])

    let bodies: Body list =
        [
            { body
                  "worker-995080-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 27 36)
                  None with
                TicksToLive = 63
                Energy = 435
            }
            { body
                  "worker-995234-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 33 40)
                  None with
                TicksToLive = 217
                Energy = 248
            }
            { body
                  "worker-995430-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 40 41)
                  None with
                TicksToLive = 413
                Energy = 516
            }
            { body
                  "upgrader-995634-Spawn9"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W17S29" 29 35)
                  None with
                TicksToLive = 605
            }
            { body
                  "anchor-996061-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 44 38)
                  None with
                TicksToLive = 963
                Energy = 50
            }
            { body
                  "anchor-996085-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 13 29)
                  None with
                TicksToLive = 987
                Energy = 50
            }
            { body
                  "hauler-996183-Spawn9"
                  Side.Ours
                  (parts [ Carry, 30; Move, 15 ])
                  (at "W17S29" 22 43)
                  None with
                TicksToLive = 1_196
                Energy = 423
            }
            { body
                  "worker-996318-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 33 36)
                  None with
                TicksToLive = 1_301
                Energy = 512
            }
            { body
                  "upgrader-996423-Spawn9"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W17S29" 28 33)
                  None with
                TicksToLive = 1_394
            }
            { body
                  "invader_W17S29_832"
                  (Side.Npc "Invader")
                  (parts [ Tough, 16; Move, 24; RangedAttack, 3; Work, 4; Attack, 2; Move, 1 ])
                  (at "W17S29" 41 8)
                  (Some Invader) with
                TicksToLive = 1_487
            }
            // invader_W17S29_976: live: rangedHeal→42,9
            { body
                  "invader_W17S29_976"
                  (Side.Npc "Invader")
                  (parts [ Move, 24; Heal, 25; Move, 1 ])
                  (at "W17S29" 41 9)
                  (Some Invader) with
                TicksToLive = 1_487
            }
            // invader_W17S29_855: live: 12 MOVE, 18 RANGED_ATTACK, 1 WORK
            // invader_W17S29_855: live: attacked→21,39 healed→40,8
            { body
                  "invader_W17S29_855"
                  (Side.Npc "Invader")
                  (parts [ Tough, 6; Move, 24; RangedAttack, 18; Work, 1; Move, 1 ])
                  (at "W17S29" 42 9)
                  (Some Invader) with
                Hits = 3_100
                TicksToLive = 1_487
            }
            { body
                  "upgrader-995000-Spawn3"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 25
                Energy = 26
            }
            { body
                  "worker-995684-Spawn3"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 16 31)
                  None with
                TicksToLive = 712
                Energy = 402
            }
            { body
                  "anchor-996028-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 930
                Energy = 50
            }
            { body
                  "hauler-996036-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 17 30)
                  None with
                TicksToLive = 1_058
                Energy = 1_300
            }
            { body
                  "hauler-996091-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 16 29)
                  None with
                TicksToLive = 1_113
                Energy = 1_300
            }
            { body
                  "anchor-996217-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_119
                Energy = 48
            }
            { body
                  "hauler-996263-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 20 27)
                  None with
                TicksToLive = 1_285
                Energy = 1_600
            }
            { body
                  "worker-996313-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 12 35)
                  None with
                TicksToLive = 1_341
                Energy = 650
            }
            { body
                  "hauler-995872-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 11 12)
                  None with
                TicksToLive = 894
                Energy = 1_402
            }
            { body
                  "reserver-996235-Spawn3"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W15S29" 13 35)
                  None with
                TicksToLive = 219
            }
            { body
                  "anchor-996407-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S29" 17 19)
                  None with
                TicksToLive = 1_315
                Energy = 38
            }
            { body
                  "anchor-995393-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W16S29" 16 42)
                  None with
                TicksToLive = 301
                Energy = 50
            }
            { body
                  "anchor-995835-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W16S29" 22 21)
                  None with
                TicksToLive = 743
                Energy = 50
            }
            { body
                  "reserver-996109-Spawn9"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W16S29" 22 33)
                  None with
                TicksToLive = 93
            }
            { body
                  "hauler-995926-Spawn9"
                  Side.Ours
                  (parts [ Carry, 30; Move, 15 ])
                  (at "W16S29" 7 14)
                  None with
                TicksToLive = 939
            }
        ]

    let a = arena 996_621 [ w17s29; w15s28; w15s29; w16s29 ] colonies bodies

    // Both W15S28 spawns were busy at the tick (the skeleton places neither):
    // the bodies and finishing ticks from the replay after it.
    { a with
        Ovens =
            [
                {
                    Spawn = "spawn-Spawn3"
                    Room = "W15S28"
                    Name = "upgrader-996497-Spawn3"
                    Parts = parts [ Work, 24; Carry, 1; Move, 24 ]
                    Done = 996_644
                }
                {
                    Spawn = "spawn-Spawn8"
                    Room = "W15S28"
                    Name = "guard-996609-Spawn8"
                    Parts = parts [ Tough, 5; Move, 25; Attack, 15; Heal, 5 ]
                    Done = 996_759
                }
            ]
    }

[<Tests>]
let w17s29GluedHealerTests =
    testList
        "arena capture w17s29-glued-healer"
        [
            // Live (t996,621-679) the shooter died at t996,657 under both
            // towers; the dismantler then walked the x=48 swamp gap, the towers
            // took it to 600, and from t996,675 its healer stood beside it:
            // 300 a tick against 2×150, the towers held their fire and it
            // healed to full. The pin ends at t58: then live the dismantler
            // turned east for the anchor while here it turns west for
            // upgrader-995634, and the rest (it lived to the guard at
            // t996,838) is not reproduced: our economy bodies stand where a
            // capture without the tick's Memory puts them, and the guard
            // stays home.
            test
                "#592 capture t996,620: the Invader healer glues to its hurt dismantler and out-heals two towers, which hold their fire, as live" {
                let _, trace = w17s29GluedHealer () |> run 59
                let failure = describe trace
                let dismantler = "invader_W17S29_832"
                let healer = "invader_W17S29_976"
                let path = pathOf dismantler trace |> Map.ofList

                Expect.equal
                    (diedOn "invader_W17S29_855" trace)
                    (Some 36)
                    $"the shooter dies at t996,657\n{failure}"

                for tick, (x, y), hits in
                    [
                        8, (46, 13), 5_000
                        24, (48, 15), 4_000
                        50, (44, 25), 1_300
                        53, (44, 25), 600
                        58, (43, 25), 2_100
                    ] do
                    let at = path[tick]

                    Expect.equal
                        (at.At.X, at.At.Y, at.Hits)
                        (x, y, hits)
                        $"the dismantler at t{tick} as live\n{failure}"

                for t in trace |> List.filter (fun t -> t.Tick >= 54) do
                    let near =
                        t.Bodies
                        |> List.filter (fun b -> b.Id = dismantler || b.Id = healer)
                        |> List.map (fun b -> b.At)

                    Expect.equal
                        (near |> List.length,
                         near
                         |> List.pairwise
                         |> List.forall (fun (p, q) -> max (abs (p.X - q.X)) (abs (p.Y - q.Y)) <= 1))
                        (2, true)
                        $"t{t.Tick}: the healer beside the dismantler\n{failure}"

                    Expect.contains
                        t.Theirs
                        (healer, Act.Heal dismantler)
                        $"t{t.Tick}: healing it\n{failure}"

                    Expect.isEmpty
                        (t.Ours
                         |> List.filter (function
                             | FireTower _ -> true
                             | _ -> false))
                        $"t{t.Tick}: 300 healed against 2×150, the towers hold\n{failure}"
            }
        ]

// ---- the world lane ---------------------------------------------------------
// The captures the world lane's one match over the fleet first moved or
// reproduced (ADR 0091 W5), each pinned on its outcome and why.

[<Tests>]
let worldLaneCaptureTests =
    testList
        "arena captures on the world lane"
        [
            // At t10 W17S26's chase turns Kite. Before #596 it seated one body
            // world-wide, the one at the fight soonest: the child's 2-RANGED
            // ranger-985441, the mother's 14-RANGED ranger-984683 let go, and
            // the melee died at t114. Now the kite seats the
            // body that ends the raid soonest (the mother's, walk and kill)
            // and beside it every ranger already at the fight (the child's).
            test
                "movement S3 capture t986,005, the world lane: the mother's 14-RANGED ranger takes a seat beside the child's, and the melee dies at t36" {
                let first, _ = rangerIntoMelee () |> run 20

                Expect.equal
                    (first.Carried.Assignments
                     |> Map.filter (fun _ tid -> tid = "guard:W17S26")
                     |> Map.keys
                     |> List.ofSeq)
                    [ "ranger-984683-Spawn3"; "ranger-985441-Spawn11" ]
                    "at t20 the mother's ranger and the child's at the fight hold the kite"

                let _, trace = rangerIntoMelee () |> run 120
                let failure = describe trace

                Expect.isTrue
                    (trace
                     |> List.exists (fun t ->
                         t.Ours
                         |> List.contains (
                             RangedAttackCreep("ranger-984683-Spawn3", "HalfLife847")
                         )))
                    $"the mother's ranger shoots it\n{failure}"

                Expect.equal
                    (diedOn "HalfLife847" trace)
                    (Some 36)
                    $"HalfLife847 dies at t36\n{failure}"
            }

            // The child's 600-hit ranger-985441 keeps its seat on W17S26's
            // trespass (#601). Before, the world lane's hold
            // kept only the seats the caps still had, and the trespass's cap
            // was 1 while the child's own ranger held it (its guard row then
            // answers nothing) and 2 once it let go: the ranger stepped
            // (17,20) <-> (16,21) between this Guard and its home's and never
            // shot, and the hauler died to the other alone at t35.
            test "#558 capture t985,660, the world lane: both rangers shoot Obsidian522 down by t31" {
                let _, trace = haulerBesideCore () |> run 80
                let failure = describe trace

                let dead =
                    match diedOn "Obsidian522" trace with
                    | Some tick -> tick
                    | None -> failtest $"Obsidian522 dies\n{failure}"

                Expect.isTrue
                    (trace
                     |> List.filter (fun t -> t.Tick >= 6 && t.Tick < dead)
                     |> List.forall (fun t ->
                         [ "ranger-984393-Spawn3"; "ranger-985441-Spawn11" ]
                         |> List.forall (fun ranger ->
                             t.Ours |> List.contains (RangedAttackCreep(ranger, "Obsidian522")))))
                    $"both shoot it every tick from t6 until it dies\n{failure}"

                Expect.isLessThanOrEqual dead 31 $"by t31\n{failure}"
            }
        ]

// ---- a fight lost where it is struck ----------------------------------------
// `npm run capture -- W19S26 1007219 --rooms W19S25,W18S26,W17S25,W18S25,W17S26 --name w19s26-losing-trade`, read at live t1,008,800.
//   W19S26  https://screeps.com/room-history/shardSeason/W19S26/1007219.json
// W17S25's ranger-1006821 (6M 4RA 2H) harassing Trepidimous' remote W19S26
// as Wave7157 (7M 3RA 4H) crosses in from his home: its 40 a tick under the
// other's 48 heal, the other's 30 over its own 24. Live it closed to range 1
// by t1,007,230 and traded there, its MOVE shot off first, until it died at
// t1,007,417. Wave7157, crossing on the tick, stands on its landing and
// chases the nearest of ours (live it kept range 1 throughout); every other
// body not ours holds but the hauler Ion6927, which walks to 43,10 as live.
// Trepidimous' home W18S26 is its controller, towers, spawn and stores only
// (the fight never enters it); the reservations, the drops and the replay's
// ruins and tombstones are not placed. The Task log no longer held either
// ranger's Task.
//
// Before #602 the capture reproduced the live trade tick for tick: range 1
// at 40,17 from t10, 1,122 hits at t20 and 822 at t70 (live t1,007,240 and
// t1,007,290). It parts from live at t75, where the other ranger,
// ranger-1006965, arrives: live it stood at W19S25 27,20 until past
// t1,007,350, whatever held it there gone from the log, while here W19S26's
// Guard takes it at t0. The pair then killed Wave7157 at t109, the first
// ranger down to 818.
let private w19s26LosingTrade () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w19s26 =
        room "W19S26"
        |> withSite trepidimous "container" None 2_710 { X = 5; Y = 47 }
        |> withSite trepidimous "container" None 0 { X = 22; Y = 35 }

    let w19s25 = room "W19S25"

    let w18s26 =
        room "W18S26"
        |> withController Ownership.Rival (Some "Trepidimous") 6 4
        |> withControllerClocks 0 0 120_000 1_987_328
        |> withStructures
            [
                { structureOf "spawn" (Some trepidimous) 5_000 5_000 { X = 20; Y = 12 } with
                    Energy = 300
                }
                { structureOf "storage" (Some trepidimous) 10_000 10_000 { X = 20; Y = 14 } with
                    Energy = 20_989
                }
                { structureOf "terminal" (Some trepidimous) 3_000 3_000 { X = 22; Y = 16 } with
                    Energy = 40_674
                }
                towerOf trepidimous { X = 18; Y = 8 } 1_000
                towerOf trepidimous { X = 22; Y = 8 } 990
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 5 3
        |> withControllerClocks 0 0 79_954 8_043
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_800
        |> withStructures
            [
                for x, y in
                    [
                        9, 23
                        9, 25
                        9, 27
                        9, 29
                        10, 24
                        10, 26
                        10, 28
                        10, 30
                        11, 23
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 23
                        13, 25
                        13, 31
                        14, 24
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 28
                        16, 30
                        16, 32
                        17, 29
                        17, 31
                        18, 30
                        19, 31
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withRamparts
            Side.Ours
            100_000
            [
                for y in [ 15..20 ] @ [ 23..27 ] -> { X = 2; Y = y }
                for x, y in
                    [
                        11, 29
                        13, 27
                        13, 29
                        14, 28
                        16, 44
                        17, 20
                        17, 44
                        18, 44
                        32, 31
                        32, 32
                    ] -> { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 137_000 250_000 { X = 16; Y = 34 } with
                    Energy = 2
                }
                { structureOf "container" None 125_000 250_000 { X = 17; Y = 20 } with
                    Energy = 198
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 6_991
                }
                towerOf Side.Ours { X = 11; Y = 29 } 1_000
                towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]
        |> withRoads
            [
                1, 16, 4_900
                2, 17, 5_000
                3, 18, 4_900
                4, 19, 3_400
                5, 20, 3_200
                6, 21, 3_800
                7, 22, 12_900
                8, 23, 19_800
                9, 24, 20_100
                9, 26, 24_000
                9, 28, 24_000
                10, 23, 23_500
                10, 25, 20_100
                10, 27, 13_300
                10, 29, 14_100
                11, 24, 15_100
                11, 26, 19_900
                11, 28, 21_100
                11, 30, 3_300
                12, 23, 13_300
                12, 25, 20_400
                12, 27, 21_200
                12, 29, 1_800
                12, 31, 4_600
                13, 24, 4_600
                13, 26, 5_000
                13, 28, 2_700
                13, 30, 1_800
                14, 23, 5_000
                14, 25, 5_000
                14, 27, 5_000
                14, 29, 2_100
                14, 31, 4_800
                15, 22, 2_800
                15, 28, 3_000
                15, 30, 3_800
                15, 32, 4_700
                16, 21, 3_100
                16, 29, 3_900
                16, 31, 4_900
                16, 33, 3_600
                16, 35, 2_500
                16, 37, 3_900
                16, 38, 8_100
                16, 39, 9_100
                16, 41, 4_700
                16, 43, 4_500
                16, 44, 3_700
                17, 20, 3_600
                17, 30, 1_800
                17, 32, 2_600
                17, 34, 4_900
                17, 36, 3_200
                17, 38, 2_800
                17, 39, 13_500
                17, 40, 18_600
                17, 42, 4_900
                17, 45, 4_900
                18, 29, 2_900
                18, 31, 3_300
                18, 35, 3_800
                18, 37, 2_900
                18, 39, 3_700
                18, 41, 1_800
                18, 46, 3_600
                19, 30, 2_600
                19, 34, 3_100
                19, 36, 2_400
                19, 38, 3_700
                19, 40, 3_600
                19, 47, 3_600
                20, 31, 4_300
                20, 33, 3_000
                20, 35, 3_600
                20, 37, 3_100
                20, 39, 3_700
                20, 48, 3_000
                21, 32, 2_800
                21, 34, 2_600
                21, 36, 3_700
                21, 38, 2_500
                22, 33, 3_100
                22, 35, 3_800
                22, 37, 3_200
                23, 34, 3_000
                23, 36, 3_600
                24, 35, 3_700
                25, 34, 2_000
                26, 33, 2_500
                27, 32, 2_200
                28, 31, 6_500
                28, 33, 11_100
                29, 32, 15_400
                29, 34, 13_600
                30, 31, 7_500
                30, 33, 12_900
                31, 32, 10_900
                32, 31, 20_800
                33, 30, 15_140
                33, 32, 7_380
            ]

    let w18s25 =
        room "W18S25"
        |> withStructures
            [
                { structureOf "container" None 146_100 250_000 { X = 31; Y = 5 } with
                    Energy = 424
                }
            ]
        |> withRoads
            [
                28, 9, 3_400
                29, 8, 2_850
                29, 10, 4_580
                30, 7, 3_000
                30, 11, 4_200
                31, 6, 3_500
                31, 12, 5_000
                32, 13, 4_000
                33, 14, 2_500
                34, 15, 2_800
                35, 16, 2_500
                36, 17, 3_100
                37, 16, 4_000
                38, 15, 3_000
                39, 14, 2_900
                40, 13, 4_900
                41, 12, 4_800
                42, 13, 2_700
                43, 13, 3_870
                44, 13, 3_800
                45, 13, 2_400
                46, 14, 5_000
                47, 15, 3_900
                48, 16, 4_900
            ]

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 154_900 250_000 { X = 27; Y = 9 } with
                    Energy = 420
                }
            ]
        |> withRoads
            [
                20, 1, 3_600
                21, 2, 2_580
                22, 3, 3_100
                23, 4, 3_700
                24, 5, 1_700
                25, 6, 3_700
                26, 7, 2_300
                27, 8, 3_400
            ]

    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W17S25" ])

    let bodies: Body list =
        [
            { body
                  "Wave7157"
                  trepidimous
                  (parts [ RangedAttack, 3; Move, 6; Heal, 4; Move, 1 ])
                  (at "W19S26" 49 7)
                  (Some(Chase(Nearest, None))) with
                TicksToLive = 1_478
            }
            { body
                  "Ion6927"
                  trepidimous
                  (parts
                      [
                          for _ in 1..22 do
                              yield! [ Move, 1; Carry, 1 ]
                      ])
                  (at "W19S26" 39 18)
                  (Some(GoTo(at "W19S26" 43 10))) with
                Hits = 4_360
                TicksToLive = 1_338
            }
            { body
                  "Ocean6422"
                  trepidimous
                  (parts
                      [
                          for _ in 1..23 do
                              yield! [ Move, 1; Carry, 1 ]
                      ])
                  (at "W17S26" 28 8)
                  (Some Hold) with
                TicksToLive = 839
            }
            { body
                  "Coefficient6755"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W17S26" 12 24)
                  (Some Hold) with
                TicksToLive = 1_139
            }
            { body
                  "Constant7133"
                  trepidimous
                  (parts [ Move, 3; Work, 5 ])
                  (at "W17S26" 17 1)
                  (Some Hold) with
                TicksToLive = 1_436
                Fatigue = 4
            }
            { body
                  "dismantler-1006415-Spawn11"
                  Side.Ours
                  (parts [ Work, 12; Move, 12 ])
                  (at "W19S26" 21 35)
                  None with
                TicksToLive = 766
            }
            { body
                  "ranger-1006821-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; RangedAttack, 4; Heal, 2 ])
                  (at "W19S26" 32 20)
                  None with
                TicksToLive = 1_136
            }
            { body
                  "ranger-1006965-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; RangedAttack, 4; Heal, 2 ])
                  (at "W19S25" 27 20)
                  None with
                TicksToLive = 1_280
            }
            { body
                  "worker-1005881-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 9; Move, 9 ])
                  (at "W17S25" 12 23)
                  None with
                TicksToLive = 238
                Energy = 378
            }
            { body
                  "worker-1006267-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 10; Move, 9 ])
                  (at "W18S25" 32 13)
                  None with
                TicksToLive = 627
                Energy = 500
            }
            { body
                  "hauler-1006605-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W17S25" 7 22)
                  None with
                TicksToLive = 992
            }
            { body
                  "hauler-1006713-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W17S25" 14 21)
                  None with
                TicksToLive = 1_100
            }
            { body
                  "anchor-1006887-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 1_196
                Energy = 50
            }
            { body
                  "guard-1007097-Spawn11"
                  Side.Ours
                  (parts [ Tough, 2; Move, 10; Attack, 6; Heal, 2 ])
                  (at "W17S25" 19 35)
                  None with
                TicksToLive = 1_436
            }
            { body
                  "anchor-1006857-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W18S25" 31 5)
                  None with
                TicksToLive = 1_166
                Energy = 50
            }
            { body
                  "reserver-1007073-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W18S25" 17 8)
                  None with
                TicksToLive = 458
            }
        ]

    // W19S26 and W19S25 are harassment rooms W17S25 casts (#609): without
    // them the capture never sat ranger-1006965 on W19S25's Stand as live.
    arena 1_007_220 [ w19s26; w19s25; w18s26; w17s25; w18s25; w17s26 ] colonies bodies
    |> withHarass Colony.harass

[<Tests>]
let foughtWhereStruckTests =
    testList
        "arena a fight lost where it is struck"
        [
            // #602: a Guard is priced again on every tick its body is struck.
            // #608 had ranger-1006821, losing alone, run for the exit and cross
            // home at t28; home, nothing of ours saw W19S26, its Guard went,
            // and Wave7157 was left at the border.
            // #609: with the harassment rooms cast as live, ranger-1006965
            // held W19S25's quiet Guard on its Stand (live: 27,20 past
            // t1,007,350) while W19S26's harassment Guard seated one ranger.
            // Now the raided room seats the pair that wins it, two rungs up,
            // so ranger-1006965 is sent at t1; and ranger-1006821, which
            // outlasts Wave7157 by two hundred ticks (6 a tick net), holds
            // the fight until it lands. The pair kills Wave7157 at t109, the
            // first ranger down to 824, neither leaving W19S26 once in it.
            test
                "#602 capture t1,007,219: a ranger losing alone holds while its partner walks in, and the pair kills Wave7157" {
                let _, trace = w19s26LosingTrade () |> run 120
                let failure = describe trace

                let least ranger =
                    trace
                    |> List.collect (fun t -> t.Bodies |> List.filter (fun b -> b.Id = ranger))
                    |> List.map _.Hits
                    |> List.min

                Expect.isGreaterThanOrEqual
                    (least "ranger-1006821-Spawn11")
                    800
                    $"it holds at the cost of the wait, never to its death\n{failure}"

                Expect.isGreaterThanOrEqual
                    (least "ranger-1006965-Spawn11")
                    1_150
                    $"the other walks in to a won fight\n{failure}"

                Expect.isFalse
                    (pathOf "ranger-1006821-Spawn11" trace
                     |> List.exists (fun (_, s) -> s.At.Room <> "W19S26"))
                    $"it never leaves W19S26 for the exit row\n{failure}"

                Expect.isLessThanOrEqual
                    (diedOn "Wave7157" trace |> Option.defaultValue System.Int32.MaxValue)
                    115
                    $"the pair kills Wave7157 by t115\n{failure}"
            }
        ]

// ---- arena capture: W17S26 t1,012,444 (#608) ---------------------------------
// `npm run capture -- W17S26 1012444 --rooms W17S25`, read at live t1,012,655.
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/1012400.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/1012400.json
// W17S26's reservation, 8 ruins and 2 tombstones are not placed. W17S25's
// ramparts are laid without their decay clocks. The Transition log no longer
// held the reserver's, the anchor's or the guard's Task.

/// The live tiles of Trepidimous' Shadow2363 (3 RANGED_ATTACK 4 HEAL 7 MOVE)
/// from t1,012,445, in runs that never revisit a tile (`Waypoints` takes the
/// latest match): in from the west, round the swamp at (10,22), after the
/// reserver to (18,15), then after the anchor up the road and west along
/// y = 1.
let private shadowLive: Behaviour =
    let run tiles =
        Waypoints [ for x, y in tiles -> RoomPos.at "W17S26" { X = x; Y = y } ]

    Phases
        [
            30,
            run
                [
                    1, 32
                    2, 33
                    3, 33
                    4, 33
                    5, 32
                    6, 31
                    7, 30
                    8, 30
                    9, 30
                    10, 29
                    11, 28
                    12, 27
                    11, 26
                    11, 25
                    11, 24
                    11, 23
                    10, 22
                ]
            46,
            run
                [
                    11, 23
                    12, 23
                    13, 23
                    14, 22
                    15, 21
                    16, 20
                    17, 20
                    18, 19
                    18, 18
                    18, 17
                    19, 16
                    19, 15
                    18, 15
                ]
            88,
            run
                [
                    19, 16
                    20, 16
                    21, 15
                    22, 14
                    23, 13
                    24, 12
                    24, 11
                    25, 10
                    26, 9
                    27, 8
                    26, 7
                    25, 6
                    24, 5
                    23, 4
                    22, 3
                    21, 2
                    20, 1
                    19, 1
                    18, 1
                    17, 1
                    16, 1
                    15, 1
                    14, 1
                ]
            120, run [ 15, 1; 14, 0 ]
        ]

/// W17S26 at t1,012,444 (#608): Shadow2363 walks in from the west. Live,
/// reserver-1012111 (2 CLAIM 2 MOVE) reserved from the swamp at (9,22) until
/// the shooter stood five off, stepped onto the swamp at (10,22) (t1,012,465)
/// and was shot there four ticks; it ran east and north and stepped from
/// (18,15) onto the swamp at (19,14) with the shooter three off (t1,012,484),
/// where it died held (t1,012,490). anchor-1012148 (6 WORK 1 CARRY 3 MOVE)
/// left its container at t1,012,495, walked the road to the exit road's
/// (20,1), then west along y = 1 beside the exits home, the shooter on its
/// heels, and died at (13,1) (t1,012,532) without crossing.
let private w17s26ShotHome (shadow: Behaviour) : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"
    let odiodin = Side.Player "Odiodin"

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 120_700 250_000 { X = 27; Y = 9 } with
                    Energy = 648
                }
            ]
        |> withRoads
            [
                20, 1, 2_700
                21, 2, 4_500
                22, 3, 2_200
                23, 4, 2_700
                24, 5, 4_400
                25, 6, 2_900
                26, 7, 4_600
                27, 8, 2_700
            ]

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 5 3
        |> withControllerClocks 0 0 80_000 41_999
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_800
        |> withStructures
            [
                for x, y in
                    [
                        9, 23
                        9, 25
                        9, 27
                        9, 29
                        10, 24
                        10, 26
                        10, 28
                        10, 30
                        11, 23
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 23
                        13, 25
                        13, 31
                        14, 24
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 28
                        16, 30
                        16, 32
                        17, 29
                        17, 31
                        18, 30
                        19, 31
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                yield
                    { structureOf "container" None 160_800 250_000 { X = 16; Y = 34 } with
                        Energy = 1_493
                    }
                yield
                    { structureOf "container" None 165_000 250_000 { X = 17; Y = 20 } with
                        Energy = 36
                    }
                for x, y in
                    [
                        2, 15
                        2, 16
                        2, 17
                        2, 18
                        2, 19
                        2, 20
                        2, 23
                        2, 24
                        2, 25
                        2, 26
                        2, 27
                        11, 29
                        13, 27
                        13, 29
                        14, 28
                        16, 44
                        17, 20
                        17, 44
                        18, 44
                        32, 31
                        32, 32
                    ] -> structureOf "rampart" (Some Side.Ours) 99_000 10_000_000 { X = x; Y = y }
                yield
                    { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                        Energy = 10_170
                    }
                yield towerOf Side.Ours { X = 11; Y = 29 } 1_000
                yield towerOf Side.Ours { X = 13; Y = 29 } 1_000
            ]
        |> withRoads
            [
                1, 16, 4_100
                2, 17, 4_100
                3, 18, 4_100
                4, 19, 2_500
                5, 20, 4_100
                6, 21, 2_900
                7, 22, 15_900
                8, 23, 15_300
                9, 24, 14_600
                9, 26, 20_500
                9, 28, 21_000
                10, 23, 20_000
                10, 25, 15_100
                10, 27, 17_800
                10, 29, 19_200
                11, 24, 20_100
                11, 26, 14_400
                11, 28, 16_600
                11, 30, 2_700
                12, 23, 18_800
                12, 25, 13_900
                12, 27, 16_700
                12, 29, 4_100
                12, 31, 4_000
                13, 24, 3_300
                13, 26, 4_000
                13, 28, 3_600
                13, 30, 4_000
                14, 23, 3_800
                14, 25, 4_400
                14, 27, 4_300
                14, 29, 4_100
                14, 31, 3_900
                15, 22, 3_500
                15, 28, 4_000
                15, 30, 2_900
                15, 32, 3_900
                16, 21, 4_200
                16, 29, 3_200
                16, 31, 4_100
                16, 33, 2_800
                16, 35, 3_700
                16, 37, 3_200
                16, 38, 19_000
                16, 39, 21_800
                16, 41, 4_100
                16, 43, 3_800
                16, 44, 3_000
                17, 20, 3_100
                17, 30, 4_300
                17, 32, 3_600
                17, 34, 4_300
                17, 36, 2_500
                17, 38, 2_300
                17, 39, 20_000
                17, 40, 15_600
                17, 42, 4_300
                17, 45, 4_300
                18, 29, 4_100
                18, 31, 2_700
                18, 35, 3_200
                18, 37, 2_400
                18, 39, 3_200
                18, 41, 4_800
                18, 46, 3_000
                19, 30, 3_800
                19, 34, 4_200
                19, 36, 3_700
                19, 38, 3_200
                19, 40, 3_100
                19, 47, 2_900
                20, 31, 3_800
                20, 33, 4_200
                20, 35, 3_100
                20, 37, 2_600
                20, 39, 3_200
                20, 48, 2_400
                21, 32, 4_100
                21, 34, 3_800
                21, 36, 3_200
                21, 38, 3_800
                22, 33, 2_600
                22, 35, 3_300
                22, 37, 2_700
                23, 34, 2_500
                23, 36, 3_100
                24, 35, 3_100
                25, 34, 1_500
                26, 33, 1_900
                27, 32, 1_600
                28, 31, 18_400
                28, 33, 8_600
                29, 32, 12_900
                29, 34, 20_100
                30, 31, 19_400
                30, 33, 19_400
                31, 32, 19_200
                32, 31, 18_300
                33, 30, 12_640
                33, 32, 19_280
            ]

    let colonies =
        Colony.declared |> List.filter (fun c -> List.contains c.Home [ "W17S25" ])

    let bodies: Body list =
        [
            { body
                  "reserver-1012111-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 9 22)
                  None with
                TicksToLive = 277
            }
            { body
                  "anchor-1012148-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 1_232
                Energy = 50
            }
            { body
                  "Shadow2363"
                  trepidimous
                  (parts [ RangedAttack, 3; Move, 6; Heal, 4; Move, 1 ])
                  (at "W17S26" 1 31)
                  (Some shadow) with
                TicksToLive = 1_459
            }
            { body
                  "guard-1011187-Spawn11"
                  Side.Ours
                  (parts [ Tough, 2; Move, 10; Attack, 6; Heal, 2 ])
                  (at "W17S25" 17 47)
                  None with
                TicksToLive = 301
            }
            { body
                  "hauler-1011295-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W17S25" 14 23)
                  None with
                TicksToLive = 457
                Energy = 840
            }
            { body
                  "anchor-1011403-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 487
                Energy = 50
            }
            { body
                  "worker-1011790-Spawn11"
                  Side.Ours
                  (parts [ Work, 9; Carry, 9; Move, 9 ])
                  (at "W17S25" 16 33)
                  None with
                TicksToLive = 925
                Energy = 306
            }
            { body
                  "upgrader-1012042-Spawn11"
                  Side.Ours
                  (parts [ Work, 11; Carry, 1; Move, 11 ])
                  (at "W17S25" 17 34)
                  None with
                TicksToLive = 1_165
                Energy = 39
            }
            { body
                  "scout-W17S22-1011026"
                  odiodin
                  (parts [ Move, 1 ])
                  (at "W17S25" 13 14)
                  (Some Hold) with
                TicksToLive = 83
            }
        ]

    let a = arena 1_012_445 [ w17s26; w17s25 ] colonies bodies

    { a with
        Carried =
            { a.Carried with
                Assignments =
                    Map.ofList
                        [
                            "hauler-1011295-Spawn11", "refill:container-16-34"
                            "anchor-1011403-Spawn11", "harvest:6a8caa7cdd4872bccd318c75"
                            "worker-1011790-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "upgrader-1012042-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                        ]
                Towered = Set.ofList [ "W17S25" ]
                Raids =
                    Map.ofList
                        [
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 1_004_791
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Bounced = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            HolderUnread = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S29"
                                            Opened = 986_839
                                            LastSeen = 1_012_444
                                            Expiry = 1_014_298
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            HolderUnread = false
                                        }
                                    ]
                            }
                        ]
            }
    }

[<Tests>]
let shotHomeTests =
    testList
        "arena a body shot on its way home"
        [
            // #608. Before the fix the capture reproduced live tile for tile:
            // the reserver left (9,22) by the swamp (10,22) at t20 (live
            // t1,012,465), ran east and north into the pocket under the wall
            // at (17,15), its walk home planned through the swamps (19,14)
            // and (20,13) though only its first step was weighed, and from
            // (18,15) stepped onto (19,14) with the shooter three off, struck
            // a tick later there than where it stood but held four ticks:
            // dead at t45 (live t1,012,490). The anchor walked the road to
            // (20,1) by t64 and west along y = 1 beside the exits, the
            // landings ahead of it in the Reach the shooter at its heels
            // carried across, and died at (13,1) at t86 (live t1,012,532).
            // Now the reserver's walk home keeps to the tiles it is on first,
            // east of the wall on the plain, and it crosses home; the anchor
            // crosses at (20,0) the tick it reaches the exit road. Against a
            // shooter that chases the nearest body from t30 the reserver
            // crosses home as well, and the anchor, cut off from the exits,
            // runs south on the plain, alive past its live death.
            test
                "#608 capture t1,012,444: the reserver's walk home keeps off the swamp it is struck on and the anchor crosses home under fire" {
                let home id (trace: TickTrace list) =
                    pathOf id trace
                    |> List.tryFind (fun (_, s) -> s.At.Room = "W17S25")
                    |> Option.map fst

                let reserver = "reserver-1012111-Spawn11"
                let anchor = "anchor-1012148-Spawn11"

                let _, trace = w17s26ShotHome shadowLive |> run 80
                let failure = describe trace

                Expect.isTrue
                    (home reserver trace |> Option.exists (fun t -> t <= 70))
                    $"live tiles: the reserver crosses home by t70\n{failure}"

                Expect.isTrue
                    (home anchor trace |> Option.exists (fun t -> t <= 70))
                    $"live tiles: so does the anchor\n{failure}"

                Expect.isNone (diedOn reserver trace) $"live tiles: the reserver lives\n{failure}"
                Expect.isNone (diedOn anchor trace) $"live tiles: so does the anchor\n{failure}"

                let chasing =
                    match shadowLive with
                    | Phases(first :: _) -> Phases [ first; 999, Kite(Nearest, Engine.rangedRange) ]
                    | other -> other

                let _, chased = w17s26ShotHome chasing |> run 90
                let failure = describe chased

                Expect.isTrue
                    (home reserver chased |> Option.exists (fun t -> t <= 75))
                    $"chasing: the reserver crosses home by t75\n{failure}"

                Expect.isNone (diedOn reserver chased) $"chasing: the reserver lives\n{failure}"

                // Its fallback once ranked the tiles beside it by when they
                // are struck alone: the swamp (31,4), struck later than the
                // plain beside it, held it a dozen ticks under fire.
                Expect.isFalse
                    (pathOf anchor chased
                     |> List.exists (fun (_, s) -> s.At = RoomPos.at "W17S26" { X = 31; Y = 4 }))
                    $"chasing: the anchor never steps onto the swamp (31,4) under fire\n{failure}"

                Expect.isNone (diedOn anchor chased) $"chasing: the anchor outlives t87\n{failure}"
            }
        ]

// ---- arena capture w19s25-t1012579 -------------------------------------------
// `npm run capture -- W19S25 1012579 --rooms W17S25,W15S28,W17S26,W18S25`, read at live t1,012,645.
// The room-history replay at t1,012,579, the state after that tick ran: arena
// tick 0 runs as t1,012,580 and decides what our code decided live then.
//   W19S25  https://screeps.com/room-history/shardSeason/W19S25/1012500.json
//   W17S25  https://screeps.com/room-history/shardSeason/W17S25/1012500.json
//   W15S28  https://screeps.com/room-history/shardSeason/W15S28/1012500.json
//   W17S26  https://screeps.com/room-history/shardSeason/W17S26/1012500.json
//   W18S25  https://screeps.com/room-history/shardSeason/W18S25/1012500.json
// Spawn11 is spawning worker-1012533-Spawn11 until t1012614: not placed.
// Spawn3 is spawning worker-1012447-Spawn3 until t1012597: not placed.
// Spawn8 is spawning hauler-1012517-Spawn8 until t1012661: not placed.
// W17S26 is reserved by us to t1012751: the arena has no reservations.
// W18S25 is reserved by us to t1017579: the arena has no reservations.
// not placed: 8 ruin, 1 energy, 1 tombstone (the arena has no drops, tombstones or ruins).
/// Two Trepidimous 3RA4H7M kiters, Shadow2363 in W17S26 and Deepening1722
/// looping W19S25's west exit, against W17S25's two 4RA2H rangers and melee
/// guards (#609).
let private trepKiters () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"

    let w19s25 =
        room "W19S25"
        |> withSite trepidimous "road" None 0 { X = 9; Y = 37 }
        |> withSite trepidimous "road" None 0 { X = 10; Y = 37 }
        |> withSite trepidimous "road" None 0 { X = 11; Y = 37 }
        |> withSite trepidimous "road" None 0 { X = 12; Y = 37 }

    let w17s25 =
        room "W17S25"
        |> withController Ownership.Ours None 5 3
        |> withControllerClocks 0 0 80_000 44_240
        |> withSpawn "Spawn11" { X = 14; Y = 28 } 1_800
        |> withStructures
            [
                for x, y in
                    [
                        9, 23
                        9, 25
                        9, 27
                        9, 29
                        10, 24
                        10, 26
                        10, 28
                        10, 30
                        11, 23
                        11, 25
                        11, 27
                        12, 24
                        12, 28
                        12, 30
                        13, 23
                        13, 25
                        13, 31
                        14, 24
                        14, 26
                        14, 30
                        15, 29
                        15, 31
                        16, 30
                        16, 32
                        17, 31
                        19, 31
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 50
                    }
            ]
        |> withStructures
            [
                for x, y in [ 15, 27; 16, 28; 17, 29; 18, 30 ] ->
                    structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y }
            ]
        |> withStructures
            [
                { structureOf "container" None 160_800 250_000 { X = 16; Y = 34 } with
                    Energy = 348
                }
                { structureOf "container" None 160_000 250_000 { X = 17; Y = 20 } with
                    Energy = 468
                }
                { structureOf "rampart" (Some Side.Ours) 99_221 10_000_000 { X = 2; Y = 15 } with
                    NextDecay = 1_012_626
                }
                { structureOf "rampart" (Some Side.Ours) 99_161 10_000_000 { X = 2; Y = 16 } with
                    NextDecay = 1_012_607
                }
                { structureOf "rampart" (Some Side.Ours) 99_231 10_000_000 { X = 2; Y = 17 } with
                    NextDecay = 1_012_610
                }
                { structureOf "rampart" (Some Side.Ours) 99_731 10_000_000 { X = 2; Y = 18 } with
                    NextDecay = 1_012_615
                }
                { structureOf "rampart" (Some Side.Ours) 99_111 10_000_000 { X = 2; Y = 19 } with
                    NextDecay = 1_012_587
                }
                { structureOf "rampart" (Some Side.Ours) 99_031 10_000_000 { X = 2; Y = 20 } with
                    NextDecay = 1_012_589
                }
                { structureOf "rampart" (Some Side.Ours) 99_221 10_000_000 { X = 2; Y = 23 } with
                    NextDecay = 1_012_614
                }
                { structureOf "rampart" (Some Side.Ours) 99_701 10_000_000 { X = 2; Y = 24 } with
                    NextDecay = 1_012_675
                }
                { structureOf "rampart" (Some Side.Ours) 99_301 10_000_000 { X = 2; Y = 25 } with
                    NextDecay = 1_012_593
                }
                { structureOf "rampart" (Some Side.Ours) 98_761 10_000_000 { X = 2; Y = 26 } with
                    NextDecay = 1_012_608
                }
                { structureOf "rampart" (Some Side.Ours) 99_961 10_000_000 { X = 2; Y = 27 } with
                    NextDecay = 1_012_656
                }
                { structureOf "rampart" (Some Side.Ours) 100_001 10_000_000 { X = 11; Y = 29 } with
                    NextDecay = 1_012_590
                }
                { structureOf "rampart" (Some Side.Ours) 99_701 10_000_000 { X = 13; Y = 27 } with
                    NextDecay = 1_012_607
                }
                { structureOf "rampart" (Some Side.Ours) 99_501 10_000_000 { X = 13; Y = 29 } with
                    NextDecay = 1_012_592
                }
                { structureOf "rampart" (Some Side.Ours) 99_401 10_000_000 { X = 14; Y = 28 } with
                    NextDecay = 1_012_667
                }
                { structureOf "rampart" (Some Side.Ours) 93_721 10_000_000 { X = 16; Y = 44 } with
                    NextDecay = 1_012_655
                }
                { structureOf "rampart" (Some Side.Ours) 100_301 10_000_000 { X = 17; Y = 20 } with
                    NextDecay = 1_012_613
                }
                { structureOf "rampart" (Some Side.Ours) 93_701 10_000_000 { X = 17; Y = 44 } with
                    NextDecay = 1_012_657
                }
                { structureOf "rampart" (Some Side.Ours) 93_501 10_000_000 { X = 18; Y = 44 } with
                    NextDecay = 1_012_653
                }
                { structureOf "rampart" (Some Side.Ours) 93_311 10_000_000 { X = 32; Y = 31 } with
                    NextDecay = 1_012_666
                }
                { structureOf "rampart" (Some Side.Ours) 94_101 10_000_000 { X = 32; Y = 32 } with
                    NextDecay = 1_012_585
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 13; Y = 27 } with
                    Energy = 8_348
                }
                towerOf Side.Ours { X = 11; Y = 29 } 980
                towerOf Side.Ours { X = 13; Y = 29 } 935
            ]
        |> withRoads
            [
                1, 16, 4_100
                2, 17, 4_100
                3, 18, 4_100
                4, 19, 2_500
                5, 20, 4_100
                6, 21, 2_900
                7, 22, 15_900
                8, 23, 15_300
                9, 24, 14_600
                9, 26, 20_500
                9, 28, 21_000
                10, 23, 20_000
                10, 25, 15_100
                10, 27, 17_800
                10, 29, 19_200
                11, 24, 20_100
                11, 26, 14_400
                11, 28, 16_600
                11, 30, 2_700
                12, 23, 18_800
                12, 25, 13_400
                12, 27, 16_200
                12, 29, 4_000
                12, 31, 4_000
                13, 24, 3_300
                13, 26, 4_000
                13, 28, 3_600
                13, 30, 4_000
                14, 23, 3_800
                14, 25, 4_400
                14, 27, 4_200
                14, 29, 4_000
                14, 31, 3_900
                15, 22, 3_400
                15, 28, 4_000
                15, 30, 2_900
                15, 32, 3_800
                16, 21, 4_100
                16, 29, 3_100
                16, 31, 4_000
                16, 33, 2_800
                16, 35, 3_600
                16, 37, 3_200
                16, 38, 19_000
                16, 39, 21_300
                16, 41, 4_000
                16, 43, 3_800
                16, 44, 3_000
                17, 20, 3_100
                17, 30, 4_200
                17, 32, 3_600
                17, 34, 4_300
                17, 36, 2_500
                17, 38, 2_300
                17, 39, 20_000
                17, 40, 15_600
                17, 42, 4_300
                17, 45, 4_300
                18, 29, 4_100
                18, 31, 2_600
                18, 35, 3_200
                18, 37, 2_400
                18, 39, 3_100
                18, 41, 4_800
                18, 46, 3_000
                19, 30, 3_800
                19, 34, 4_200
                19, 36, 3_700
                19, 38, 3_200
                19, 40, 3_100
                19, 47, 2_900
                20, 31, 3_800
                20, 33, 4_200
                20, 35, 3_100
                20, 37, 2_600
                20, 39, 3_200
                20, 48, 2_400
                21, 32, 4_000
                21, 34, 3_800
                21, 36, 3_200
                21, 38, 3_800
                22, 33, 2_600
                22, 35, 3_200
                22, 37, 2_700
                23, 34, 2_500
                23, 36, 3_100
                24, 35, 3_100
                25, 34, 1_500
                26, 33, 1_900
                27, 32, 1_600
                28, 31, 18_400
                28, 33, 8_600
                29, 32, 12_900
                29, 34, 20_100
                30, 31, 19_400
                30, 33, 19_400
                31, 32, 19_200
                32, 31, 17_800
                33, 30, 12_140
                33, 32, 19_280
            ]

    let w15s28 =
        room "W15S28"
        |> withController Ownership.Ours None 7 5
        |> withControllerClocks 0 0 150_000 6_696_895
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5_600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5_600
        |> withStructures
            [
                for x, y in
                    [
                        12, 24
                        12, 30
                        13, 25
                        13, 31
                        14, 26
                        14, 30
                        15, 27
                        15, 29
                        15, 31
                        16, 30
                        16, 32
                        19, 31
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                        Id = $"extension-{x}-{y}-W15S28"
                    }
            ]
        |> withStructures
            [
                for x, y in
                    [
                        12, 26
                        12, 32
                        12, 34
                        12, 36
                        13, 27
                        13, 29
                        13, 33
                        13, 35
                        14, 28
                        14, 32
                        14, 34
                        15, 25
                        15, 33
                        15, 35
                        16, 26
                        16, 34
                        17, 25
                        17, 27
                        17, 33
                        18, 26
                        18, 28
                        18, 32
                        18, 34
                        19, 27
                        19, 33
                        20, 26
                        20, 28
                        20, 30
                        20, 32
                        21, 27
                        21, 29
                        21, 31
                        21, 33
                        21, 35
                        22, 26
                        23, 25
                        23, 27
                        23, 35
                    ] ->
                    { structureOf "extension" (Some Side.Ours) 1_000 1_000 { X = x; Y = y } with
                        Energy = 100
                    }
            ]
        |> withStructures
            [
                for x, y in [ 17, 31; 19, 29; 20, 34 ] -> towerOf Side.Ours { X = x; Y = y } 1_000
            ]
        |> withStructures
            [
                { structureOf "container" None 162_700 250_000 { X = 6; Y = 29 } with
                    Energy = 12
                }
                { structureOf "container" None 125_100 250_000 { X = 11; Y = 18 } with
                    Energy = 264
                }
                { structureOf "container" None 135_700 250_000 { X = 23; Y = 29 } with
                    Energy = 1_976
                }
                structureOf "extractor" (Some Side.Ours) 500 500 { X = 29; Y = 12 }
                { structureOf "rampart" (Some Side.Ours) 99_701 100_000_000 { X = 6; Y = 29 } with
                    NextDecay = 1_012_637
                }
                { structureOf "rampart" (Some Side.Ours) 100_101 100_000_000 { X = 11; Y = 18 } with
                    NextDecay = 1_012_597
                }
                { structureOf "rampart" (Some Side.Ours) 99_401 100_000_000 { X = 16; Y = 28 } with
                    NextDecay = 1_012_655
                }
                { structureOf "rampart" (Some Side.Ours) 97_801 100_000_000 { X = 17; Y = 29 } with
                    NextDecay = 1_012_593
                }
                { structureOf "rampart" (Some Side.Ours) 100_401 100_000_000 { X = 17; Y = 31 } with
                    NextDecay = 1_012_598
                }
                { structureOf "rampart" (Some Side.Ours) 99_601 100_000_000 { X = 17; Y = 35 } with
                    NextDecay = 1_012_651
                }
                { structureOf "rampart" (Some Side.Ours) 99_101 100_000_000 { X = 18; Y = 30 } with
                    NextDecay = 1_012_672
                }
                { structureOf "rampart" (Some Side.Ours) 102_001 100_000_000 { X = 19; Y = 29 } with
                    NextDecay = 1_012_621
                }
                { structureOf "rampart" (Some Side.Ours) 102_401 100_000_000 { X = 20; Y = 34 } with
                    NextDecay = 1_012_614
                }
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 34_135
                    Thorium = 41_977
                }
                { structureOf "terminal" (Some Side.Ours) 3_000 3_000 { X = 16; Y = 28 } with
                    Energy = 319
                }
            ]
        |> withRoads
            [
                4, 5, 4_500
                5, 4, 3_300
                5, 6, 4_500
                5, 44, 3_500
                6, 3, 2_500
                6, 7, 3_800
                6, 29, 3_800
                6, 43, 3_000
                6, 45, 3_900
                7, 2, 4_300
                7, 8, 4_500
                7, 28, 4_200
                7, 42, 2_700
                7, 46, 3_300
                8, 1, 3_000
                8, 9, 2_700
                8, 28, 3_800
                8, 41, 4_600
                8, 47, 2_600
                9, 10, 3_800
                9, 27, 3_300
                9, 28, 4_800
                9, 40, 2_700
                9, 48, 4_000
                10, 11, 3_000
                10, 26, 3_800
                10, 29, 4_300
                10, 39, 4_600
                11, 12, 2_500
                11, 18, 3_500
                11, 27, 2_500
                11, 30, 4_400
                11, 38, 3_500
                12, 13, 3_400
                12, 17, 3_600
                12, 25, 17_700
                12, 27, 19_100
                12, 28, 2_500
                12, 29, 4_300
                12, 31, 3_100
                12, 33, 21_800
                12, 35, 15_100
                12, 37, 2_900
                13, 14, 3_500
                13, 16, 5_000
                13, 26, 16_000
                13, 28, 3_100
                13, 30, 4_400
                13, 32, 4_900
                13, 34, 4_600
                13, 36, 3_000
                14, 15, 5_000
                14, 25, 14_800
                14, 27, 4_200
                14, 29, 3_500
                14, 31, 3_300
                14, 33, 4_200
                14, 35, 3_800
                15, 15, 3_700
                15, 26, 4_000
                15, 28, 4_600
                15, 30, 4_800
                15, 32, 2_700
                15, 34, 4_600
                16, 16, 3_900
                16, 25, 15_700
                16, 27, 2_500
                16, 29, 4_100
                16, 31, 2_800
                16, 33, 4_400
                16, 35, 3_200
                17, 17, 4_900
                17, 26, 14_600
                17, 28, 2_500
                17, 30, 3_700
                17, 32, 3_400
                17, 34, 3_000
                18, 18, 3_600
                18, 25, 13_900
                18, 27, 16_700
                18, 29, 4_800
                18, 31, 4_900
                18, 33, 3_300
                19, 19, 18_000
                19, 22, 18_800
                19, 24, 13_900
                19, 26, 14_600
                19, 28, 2_800
                19, 30, 4_400
                19, 32, 3_700
                20, 20, 18_300
                20, 21, 18_800
                20, 23, 20_000
                20, 25, 16_600
                20, 27, 17_500
                20, 29, 2_700
                20, 31, 4_900
                20, 33, 18_300
                21, 21, 4_800
                21, 22, 2_600
                21, 23, 3_000
                21, 24, 15_200
                21, 25, 15_200
                21, 26, 16_000
                21, 28, 21_800
                21, 30, 3_200
                21, 32, 13_600
                21, 34, 20_400
                22, 24, 4_900
                22, 25, 13_400
                22, 27, 15_100
                22, 28, 3_300
                22, 29, 2_900
                22, 30, 19_200
                22, 31, 15_400
                22, 32, 18_700
                22, 33, 16_300
                22, 35, 4_500
                23, 26, 13_400
                23, 28, 18_700
                23, 29, 15_300
                23, 30, 17_000
                23, 31, 20_200
                23, 32, 18_500
                23, 33, 21_900
                23, 34, 2_700
                24, 25, 19_300
                24, 28, 17_100
                24, 29, 20_200
                24, 30, 17_500
                24, 33, 14_100
                25, 24, 4_100
                25, 28, 19_800
                25, 29, 17_800
                25, 34, 20_500
                26, 23, 14_100
                27, 22, 11_800
                28, 21, 19_000
                28, 34, 16_100
                29, 20, 12_800
                30, 19, 11_200
                31, 18, 3_400
                32, 17, 3_300
                33, 16, 2_100
                34, 15, 4_400
                35, 14, 3_400
                36, 13, 13_700
                37, 12, 4_100
                38, 11, 2_500
                39, 12, 3_500
                40, 13, 13_000
                41, 14, 11_300
                42, 15, 2_300
                43, 16, 2_400
                44, 17, 4_200
                45, 18, 4_200
                46, 19, 4_200
                47, 20, 2_600
                48, 21, 4_000
            ]

    let w17s26 =
        room "W17S26"
        |> withStructures
            [
                { structureOf "container" None 120_700 250_000 { X = 27; Y = 9 } with
                    Energy = 814
                }
            ]
        |> withRoads
            [
                20, 1, 2_700
                21, 2, 4_500
                22, 3, 2_100
                23, 4, 2_700
                24, 5, 4_400
                25, 6, 2_900
                26, 7, 4_600
                27, 8, 2_700
            ]

    let w18s25 =
        room "W18S25"
        |> withStructures
            [
                { structureOf "container" None 125_100 250_000 { X = 31; Y = 5 } with
                    Energy = 312
                }
            ]
        |> withRoads
            [
                28, 9, 2_500
                29, 8, 3_850
                29, 10, 3_580
                30, 7, 4_000
                30, 11, 3_300
                31, 6, 2_900
                31, 12, 4_000
                32, 13, 3_200
                33, 14, 3_600
                34, 15, 3_900
                35, 16, 3_600
                36, 17, 2_400
                37, 16, 3_300
                38, 15, 4_000
                39, 14, 4_000
                40, 13, 4_200
                41, 12, 4_100
                42, 13, 3_700
                43, 13, 2_970
                44, 13, 2_800
                45, 13, 3_200
                46, 14, 4_200
                47, 15, 3_100
                48, 16, 4_200
            ]

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c -> List.contains c.Home [ "W17S25"; "W15S28" ])

    let bodies: Body list =
        [
            // Fourier1584: live: no act
            { body
                  "Fourier1584"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                      ])
                  (at "W19S25" 27 26)
                  (Some Hold) with
                TicksToLive = 641
            }
            // Reptile2176: live: no act
            { body
                  "Reptile2176"
                  trepidimous
                  (parts [ Move, 1; Work, 1; Carry, 1 ])
                  (at "W19S25" 34 26)
                  (Some Hold) with
                TicksToLive = 1_104
            }
            { body
                  "ranger-1011255-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; RangedAttack, 4; Heal, 2 ])
                  (at "W19S25" 27 20)
                  None with
                TicksToLive = 210
            }
            { body
                  "ranger-1011720-Spawn11"
                  Side.Ours
                  (parts [ Move, 6; RangedAttack, 4; Heal, 2 ])
                  (at "W19S25" 7 12)
                  None with
                TicksToLive = 675
            }
            // Deepening1722: live: healed→2,17 heal→2,17. Live it loops the
            // west exit: in at 0,19, up the diagonal to 3,16, out at 0,13,
            // about eight ticks in W20S25, and in again (t1,012,505-530).
            { body
                  "Deepening1722"
                  trepidimous
                  (parts [ RangedAttack, 3; Move, 6; Heal, 4; Move, 1 ])
                  (at "W19S25" 3 16)
                  (Some(
                      Phases
                          [
                              for k in 0..39 do
                                  16 * k + 4,
                                  Waypoints
                                      [ at "W19S25" 2 15; at "W19S25" 1 14; at "W20S25" 48 13 ]

                                  16 * k + 12, GoTo(at "W20S25" 47 17)

                                  16 * k + 16,
                                  Waypoints [ at "W19S25" 1 18; at "W19S25" 2 17; at "W19S25" 3 16 ]
                          ]
                  )) with
                TicksToLive = 683
            }
            { body
                  "anchor-1011403-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 352
                Energy = 50
            }
            { body
                  "hauler-1011503-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W17S25" 14 29)
                  None with
                TicksToLive = 530
            }
            { body
                  "worker-1011790-Spawn11"
                  Side.Ours
                  (parts [ Work, 9; Carry, 9; Move, 9 ])
                  (at "W17S25" 18 31)
                  None with
                TicksToLive = 790
                Energy = 400
            }
            { body
                  "upgrader-1012042-Spawn11"
                  Side.Ours
                  (parts [ Work, 11; Carry, 1; Move, 11 ])
                  (at "W17S25" 17 34)
                  None with
                TicksToLive = 1_030
                Energy = 39
            }
            // guard-1012445-Spawn11: crossing to W17S26 3,0
            { body
                  "guard-1012445-Spawn11"
                  Side.Ours
                  (parts [ Tough, 2; Move, 10; Attack, 6; Heal, 2 ])
                  (at "W17S25" 3 49)
                  None with
                TicksToLive = 1_424
            }
            { body
                  "ranger-1011014-Spawn3"
                  Side.Ours
                  (parts [ Move, 12; RangedAttack, 8; Heal, 4 ])
                  (at "W15S28" 4 45)
                  None with
                TicksToLive = 5
            }
            { body
                  "hauler-1011018-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 81
                Energy = 12
            }
            { body
                  "hauler-1011086-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 2 1)
                  None with
                TicksToLive = 149
            }
            { body
                  "anchor-1011168-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 117
                Energy = 50
            }
            { body
                  "upgrader-1011653-Spawn3"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 23 28)
                  None with
                TicksToLive = 719
                Energy = 26
            }
            { body
                  "anchor-1011928-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 871
                Energy = 50
            }
            { body
                  "hauler-1012086-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 18 27)
                  None with
                TicksToLive = 1_149
            }
            { body
                  "worker-1012144-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 29)
                  None with
                TicksToLive = 1_213
                Energy = 386
            }
            { body
                  "hauler-1011688-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 7 28)
                  None with
                TicksToLive = 751
                Energy = 1_092
            }
            // Shadow2363: live: healed→5,3 heal→5,3 rangedAttack→7,0
            { body
                  "Shadow2363"
                  trepidimous
                  (parts [ RangedAttack, 3; Move, 6; Heal, 4; Move, 1 ])
                  (at "W17S26" 5 3)
                  (Some(Waypoints [ at "W17S26" 5 3 ])) with
                TicksToLive = 1_324
            }
            // guard-1011187-Spawn11: live: 2 TOUGH, 10 MOVE, 6 ATTACK, 2 HEAL
            // guard-1011187-Spawn11: crossing to W17S25 7,49
            { body
                  "guard-1011187-Spawn11"
                  Side.Ours
                  (parts [ Tough, 2; Move, 10; Attack, 6; Heal, 2 ])
                  (at "W17S26" 7 0)
                  None with
                Hits = 1_994
                TicksToLive = 166
            }
            { body
                  "anchor-1011647-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W18S25" 31 5)
                  None with
                TicksToLive = 596
                Energy = 50
            }
            { body
                  "reserver-1012178-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W18S25" 17 8)
                  None with
                TicksToLive = 209
            }
            // hauler-1011295-Spawn11: crossing to W17S25 0,15
            { body
                  "hauler-1011295-Spawn11"
                  Side.Ours
                  (parts [ Carry, 24; Move, 12 ])
                  (at "W18S25" 49 15)
                  None with
                TicksToLive = 322
                Energy = 1_008
            }
        ]

    // W19S25 is a harassment room W17S25 casts (live `outposts`).
    let a =
        arena 1_012_580 [ w19s25; w17s25; w15s28; w17s26; w18s25; room "W20S25" ] colonies bodies
        |> withHarass Colony.harass

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before t1,012,579,
                // read from Memory at live t1,012,645; the log held none for
                // guard-1012445, ranger-1011014 or guard-1011187.
                Assignments =
                    Map.ofList
                        [
                            "ranger-1011255-Spawn11", "guard:W19S25"
                            "ranger-1011720-Spawn11", "guard:W19S25"
                            "anchor-1011403-Spawn11", "harvest:6a8caa7cdd4872bccd318c75"
                            "hauler-1011503-Spawn11", "refill:tower-13-29"
                            "worker-1011790-Spawn11", "refill:spawn-Spawn11"
                            "upgrader-1012042-Spawn11", "upgrade:6a8caa7cdd4872bccd318c76"
                            "hauler-1011018-Spawn8", "refill:container-23-29"
                            "hauler-1011086-Spawn3", "withdraw:6ac0e2f1e169cce45673df61"
                            "anchor-1011168-Spawn8", "harvest:6a8caa95dd4872bccd319014"
                            "upgrader-1011653-Spawn3", "upgrade:6a8caa95dd4872bccd319015"
                            "anchor-1011928-Spawn3", "harvest:6a8caa95dd4872bccd319013"
                            "hauler-1012086-Spawn3", "withdraw:6aba1ca3293c6f833d8b43f5"
                            "worker-1012144-Spawn8", "upgrade:6a8caa95dd4872bccd319015"
                            "hauler-1011688-Spawn8", "withdraw:container-6-29"
                            "anchor-1011647-Spawn11", "harvest:6a8caa71dd4872bccd318b28"
                            "reserver-1012178-Spawn11", "reserve:6a8caa71dd4872bccd318b29"
                            "hauler-1011295-Spawn11", "refill:tower-11-29"
                        ]
                // From live Memory at t1,012,645, 66 ticks after the capture,
                // less what is stamped after the tick. Healers, ExitWatches
                // and Sightings start empty.
                ArmedSeen = Map.ofList [ "W17S25", 1_012_536 ]
                Towered = Set.ofList [ "W15S28"; "W17S25" ]
                Raids =
                    Map.ofList
                        [
                            "W15S28",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 1_004_791
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Bounced = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            HolderUnread = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S29"
                                            Opened = 986_839
                                            LastSeen = 1_012_579
                                            Expiry = 1_014_298
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            HolderUnread = false
                                        }
                                    ]
                            }
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 1_004_791
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Bounced = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            HolderUnread = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S29"
                                            Opened = 986_839
                                            LastSeen = 1_012_579
                                            Expiry = 1_014_298
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                            HolderUnread = false
                                        }
                                    ]
                                HomeRaided = Some 1_012_536
                            }
                        ]
            }
    }

[<Tests>]
let rangerPairTests =
    testList
        "arena a raid the ranger row's pair beats"
        [
            // #609: Shadow2363, 3 RANGED_ATTACK 4 HEAL 7 MOVE, out-heals the
            // 4RA2H ranger W17S25's 1,800 bank buys (40 < 48) and loses to two
            // (80). Before, the room was the melee guard's, which never
            // catches a 7M kiter: as live, guard-1011187 bounced on the
            // W17S25/W17S26 border and Shadow2363 stood at full hits; and in
            // W19S25 ranger-1011255 sat on the Stand's seat while
            // ranger-1011720 hovered four off Deepening1722, neither ever
            // shooting (live t1,012,580-605). Now the room is the ranger
            // row's at two bodies: ranger-1011720 is sent, the cast waits for
            // it at W17S25's south exit, the pair goes in together and kills
            // Shadow2363 at t175, neither below 1,186.
            test
                "#609 capture t1,012,579: a kiter one ranger loses to and two beat is answered by two rangers, which kill it" {
                let _, trace = trepKiters () |> withCasts [ "ranger" ] |> run 200
                let failure = describe trace

                Expect.isLessThanOrEqual
                    (diedOn "Shadow2363" trace |> Option.defaultValue System.Int32.MaxValue)
                    190
                    $"the pair kills Shadow2363 by t190\n{failure}"

                let least =
                    trace
                    |> List.collect (fun t ->
                        t.Bodies
                        |> List.filter (fun b -> b.At.Room = "W17S26" && b.Id.StartsWith "ranger-"))
                    |> List.map _.Hits

                Expect.isNonEmpty least $"rangers fight in W17S26\n{failure}"

                Expect.isGreaterThanOrEqual
                    (List.min least)
                    1_100
                    $"they go in together: none trades its hits away alone\n{failure}"
            }
        ]
