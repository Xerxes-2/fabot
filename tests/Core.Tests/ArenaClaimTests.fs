/// Taking W17S25 and holding it against Trepidimous (#470): the controller's
/// and the construction site's rules against the engine, and the claim played
/// through our live decide — the claim party (G1, #471), residents before
/// the claim (G2), and the perimeter once safe mode ends.
module Fabot.Core.Tests.ArenaClaimTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Tests.Arena

let private w17s25 (x: int) (y: int) = RoomPos.at "W17S25" { X = x; Y = y }
let private tile (x: int) (y: int) : Pos = { X = x; Y = y }
let private controllerAt = w17s25 15 36

let private trep = Side.Player "Trepidimous"

/// Trepidimous' bodies (`docs/research/squads.md` §1.2): MOVE first.
let private trepMelee = parts [ Move, 17; Attack, 17; Move, 1 ]
let private trepHealer = parts [ Move, 11; Heal, 7 ]
let private trepTapper = parts [ Move, 3; BodyPart.Claim, 3 ]

/// W15S28's worker as it stands live at t921,063: `16W 17C 17M`.
let private pioneerParts = parts [ Work, 16; Carry, 17; Move, 17 ]

/// W17S25's `safeModeCooldown` as live: unclaimed at t880,418, plus
/// SAFE_MODE_COOLDOWN (50,000); read 9,355 ticks off at t921,063.
let private liveCooldown = 930_418

/// W17S25 alone, its controller held as given, nothing of ours deciding.
let private physics (owner: Ownership) (level: int) (bodies: Body list) =
    let r =
        room "W17S25"
        |> withController
            owner
            (if owner = Ownership.Rival then Some "Trepidimous" else None)
            level
            0

    arena 921_000 [ r ] [] bodies

/// The room's controller at the end of a run.
let private controllerOf (a: Arena) =
    match a.Rooms["W17S25"].Controller with
    | Some c -> c
    | None -> failtest "W17S25 has no controller"

/// The tick an event first happened on.
let private firstTick (event: ArenaEvent -> bool) (trace: TickTrace list) : int option =
    trace
    |> List.tryFind (fun t -> t.Events |> List.exists event)
    |> Option.map (fun t -> t.Tick)

let private doing id side bodyParts at acts =
    body id side bodyParts at (Some(Do acts))

/// The ticks our side asked for safe mode on.
let private safeModeAsksOf (trace: TickTrace list) =
    trace
    |> List.filter (fun t ->
        t.Ours
        |> List.exists (function
            | ActivateSafeMode _ -> true
            | _ -> false))
    |> List.map (fun t -> t.Tick)

[<Tests>]
let arenaControllerPhysicsTests =
    testList
        "arena controller and site physics"
        [
            test
                "claimController: beside a controller nobody holds, a CLAIM part takes it at level 1, the cooldown standing" {
                let claimer =
                    doing
                        "claimer"
                        Side.Ours
                        (parts [ BodyPart.Claim, 1; Move, 1 ])
                        (w17s25 16 37)
                        [ Act.ClaimController "W17S25" ]

                let start =
                    physics Ownership.Unowned 0 [ claimer ]
                    |> fun a ->
                        { a with
                            Rooms =
                                a.Rooms
                                |> Map.add
                                    "W17S25"
                                    (withSafeModeCooldown liveCooldown a.Rooms["W17S25"])
                        }

                let final, trace = start |> run 2
                let c = controllerOf final
                Expect.equal c.Owner Ownership.Ours "ours"
                Expect.equal c.Level 1 "level 1"
                Expect.equal c.SafeModeAvailable 0 "no safe mode with the claim"
                Expect.equal c.SafeModeCooldown liveCooldown "the unclaim's cooldown stands"
                Expect.equal c.TicksToDowngrade 19_999 "full on the claim, one tick run down"

                Expect.equal
                    (firstTick (fun e -> e = ControllerClaimed("W17S25", "claimer")) trace)
                    (Some 0)
                    "landed at once"
            }

            test "claimController: no CLAIM part, or two tiles off, takes nothing" {
                let bodies =
                    [
                        doing
                            "mover"
                            Side.Ours
                            (parts [ Move, 1; Carry, 1 ])
                            (w17s25 16 37)
                            [ Act.ClaimController "W17S25" ]
                        doing
                            "far"
                            Side.Ours
                            (parts [ BodyPart.Claim, 1; Move, 1 ])
                            (w17s25 17 38)
                            [ Act.ClaimController "W17S25" ]
                    ]

                let final, _ = physics Ownership.Unowned 0 bodies |> run 3
                Expect.equal (controllerOf final).Owner Ownership.Unowned "still nobody's"
            }

            test
                "upgradeController: sixteen WORK take RCL1 to RCL2 on the 13th tick, banking a safe mode" {
                let pioneer =
                    { doing
                          "pioneer"
                          Side.Ours
                          pioneerParts
                          (w17s25 17 38)
                          [ Act.UpgradeController "W17S25" ] with
                        Energy = 850
                    }

                let final, trace = physics Ownership.Ours 1 [ pioneer ] |> run 13
                let c = controllerOf final

                Expect.equal
                    (firstTick (fun e -> e = LeveledUp("W17S25", 2)) trace)
                    (Some 12)
                    "16 × 13 = 208"

                Expect.equal c.Level 2 "RCL2"
                Expect.equal c.Progress 8 "the overflow carried"
                Expect.equal c.SafeModeAvailable 1 "one safe mode banked"
                Expect.equal c.TicksToDowngrade 5_100 "half of RCL2's 10,000, then one restore"
                Expect.equal (final.Bodies |> List.head).Energy (850 - 208) "one energy a point"
            }

            test
                "attackController: 3 CLAIM cut 900 off the timer and block upgrades from the next tick for 1,000; a second tap the same tick lands too" {
                let pioneer =
                    { doing
                          "pioneer"
                          Side.Ours
                          pioneerParts
                          (w17s25 17 38)
                          [ Act.UpgradeController "W17S25" ] with
                        Energy = 850
                    }

                let tap id at =
                    doing id trep trepTapper at [ Act.AttackController "W17S25" ]

                let final, trace =
                    physics
                        Ownership.Ours
                        1
                        [ pioneer; tap "Rune908" (w17s25 16 37); tap "Rune909" (w17s25 16 35) ]
                    |> run 5

                let c = controllerOf final

                let taps =
                    trace
                    |> List.collect (fun t ->
                        t.Events
                        |> List.choose (function
                            | ControllerAttacked(_, by) -> Some(t.Tick, by)
                            | _ -> None))

                Expect.equal taps [ 0, "Rune908"; 0, "Rune909" ] "both land on tick 0, none after"
                Expect.equal c.Progress 16 "the tap tick's own upgrade lands; none after"
                Expect.equal c.UpgradeBlockedUntil (921_000 + 1000) "blocked 1,000 ticks"
                // 20,000 − 1,800 on tick 0 (no restore under the new block),
                // then one a tick for four ticks.
                Expect.equal
                    c.TicksToDowngrade
                    (20_000 - 1_800 - 5)
                    "the cut, and the timer running"
            }

            test
                "upgradeController: past 200 points the level waits for the downgrade timer back within 100 of full" {
                let pioneer =
                    { doing
                          "pioneer"
                          Side.Ours
                          pioneerParts
                          (w17s25 17 38)
                          [ Act.UpgradeController "W17S25" ] with
                        Energy = 850
                    }

                // A tap's 900 off, its block run out: 19,000 left.
                let start =
                    physics Ownership.Ours 1 [ pioneer ]
                    |> fun a ->
                        let r = a.Rooms["W17S25"]

                        { a with
                            Rooms =
                                a.Rooms
                                |> Map.add
                                    "W17S25"
                                    { r with
                                        Controller =
                                            r.Controller
                                            |> Option.map (fun c ->
                                                { c with
                                                    TicksToDowngrade = 19_000
                                                    Progress = 190
                                                })
                                    }
                        }

                let _, trace = start |> run 20
                // +100 a tick: 19,000 → 19,900 after nine ticks, so the tenth
                // upgrade (tick 9) levels.
                Expect.equal
                    (firstTick (fun e -> e = LeveledUp("W17S25", 2)) trace)
                    (Some 9)
                    "nine ticks of restore first"
            }

            test
                "activateSafeMode: refused on cooldown, under an upgrade block, and against a tap landing the same tick" {
                // W17S25 a living colony at RCL2, one safe mode banked, no
                // tower, a melee inside: our reflex asks on tick 0.
                let home (edit: ArenaRoom -> ArenaRoom) (bodies: Body list) =
                    let r =
                        room "W17S25"
                        |> withController Ownership.Ours None 2 1
                        |> withSpawn "Spawn10" (tile 22 34) 550
                        |> edit

                    arena 921_000 [ r ] [ colony "W17S25" ] bodies

                let melee = body "Eternity536" trep trepMelee (w17s25 23 37) None

                let blocked (until: int) (r: ArenaRoom) =
                    { r with
                        Controller =
                            r.Controller
                            |> Option.map (fun c -> { c with UpgradeBlockedUntil = until })
                    }

                let fired (trace: TickTrace list) =
                    firstTick
                        (function
                        | SafeModeActivated _ -> true
                        | _ -> false)
                        trace

                let tapper =
                    doing "Rune908" trep trepTapper (w17s25 16 37) [ Act.AttackController "W17S25" ]

                // On cooldown our side does not ask (#474): the reflex reads it.
                for name, edit, bodies, asks, expected in
                    [
                        "free", id, [ melee ], Some 0, Some 0
                        "on cooldown", withSafeModeCooldown liveCooldown, [ melee ], None, None
                        "under a block", blocked 921_500, [ melee ], Some 0, None
                        "against a tap the same tick", id, [ melee; tapper ], Some 0, None
                    ] do
                    let final, trace = home edit bodies |> run 3
                    let asked = safeModeAsksOf trace
                    Expect.equal (List.tryHead asked) asks $"{name}: our side's first ask"
                    Expect.equal (fired trace) expected $"{name}\n{describe trace}"

                    if expected.IsSome then
                        Expect.equal
                            (controllerOf final).SafeModeCooldown
                            (921_000 + 50_000)
                            "SAFE_MODE_COOLDOWN on"
            }

            test
                "a hostile stepping onto our spawn site removes it; under our safe mode it walks over it" {
                let walker = doing "Eternity536" trep trepMelee (w17s25 12 28) [ Act.Move Right ]

                for safe in [ false; true ] do
                    let r =
                        room "W17S25"
                        |> withController Ownership.Ours None 2 0
                        |> withSite Side.Ours "spawn" (Some "Spawn10") 3_000 (tile 14 28)

                    let r =
                        if safe then
                            { r with
                                Controller =
                                    r.Controller
                                    |> Option.map (fun c -> { c with SafeModeUntil = 950_000 })
                            }
                        else
                            r

                    let final, trace = arena 921_000 [ r ] [] [ walker ] |> run 6

                    let stomped =
                        firstTick
                            (fun e -> e = SiteStomped("site-spawn-14-28", "Eternity536"))
                            trace

                    if safe then
                        Expect.isNone stomped $"safe mode shields it\n{describe trace}"
                        Expect.equal final.Rooms["W17S25"].Sites.Length 1 "the site stands"
                    else
                        Expect.equal
                            stomped
                            (Some 1)
                            $"the second step lands on it\n{describe trace}"

                        Expect.isEmpty final.Rooms["W17S25"].Sites "gone, 3,000 built into it"
            }

            test
                "build: sixteen WORK put 80 a tick into a spawn site, never while a body stands on it, and finish it named" {
                let builder at =
                    { doing "pioneer" Side.Ours pioneerParts at [ Act.Build "site-spawn-14-28" ] with
                        Energy = 850
                    }

                let site progress =
                    room "W17S25"
                    |> withController Ownership.Ours None 2 0
                    |> withSite Side.Ours "spawn" (Some "Spawn10") progress (tile 14 28)

                let final, _ = arena 921_000 [ site 0 ] [] [ builder (w17s25 15 29) ] |> run 3
                Expect.equal final.Rooms["W17S25"].Sites.Head.Progress 240 "5 a WORK"

                let squatter = body "Eternity536" trep trepMelee (w17s25 14 28) None

                let final, _ =
                    arena 921_000 [ site 0 ] [] [ builder (w17s25 15 29); squatter ] |> run 3

                Expect.equal
                    final.Rooms["W17S25"].Sites.Head.Progress
                    0
                    "a body on the tile bars it"

                let final, trace =
                    arena 921_000 [ site 14_960 ] [] [ builder (w17s25 15 29) ] |> run 1

                Expect.isEmpty final.Rooms["W17S25"].Sites "finished"

                match
                    final.Rooms["W17S25"].Structures |> List.tryFind (fun s -> s.Kind = "spawn")
                with
                | Some spawn ->
                    Expect.equal spawn.Name (Some "Spawn10") "named"
                    Expect.equal spawn.Hits 5000 "SPAWN_HITS"
                    Expect.equal spawn.Energy 0 "empty"
                | None -> failtest $"a spawn stands\n{describe trace}"

                Expect.equal (final.Bodies |> List.head).Energy (850 - 40) "only what was left"
            }
        ]

let private w15s28 (x: int) (y: int) = RoomPos.at "W15S28" { X = x; Y = y }
let private w18s25 (x: int) (y: int) = RoomPos.at "W18S25" { X = x; Y = y }

/// W17S25's declared perimeter (#446).
let private perimeter =
    [ for y in 15..20 -> tile 2 y ]
    @ [ for y in 23..27 -> tile 2 y ]
    @ [ tile 16 44; tile 17 44; tile 18 44; tile 47 27; tile 47 28 ]

let private w17s25Capture = (room "W17S25").Capture

/// The declarations before the claim lands: W17S25 a Claim among W15S28's
/// outposts and a colony of its own mothered by W15S28 (#404's shape).
let private beforeClaim =
    [
        { colony "W15S28" with
            Outposts = [ outpostOf w17s25Capture ]
        }
        { colony "W17S25" with
            Mother = Some "W15S28"
            Perimeter = perimeter
        }
    ]

/// The declarations after the #404 edit: the Claim out of W15S28's outposts,
/// so W17S25 is its nursery.
let private afterClaim =
    [
        colony "W15S28"
        { colony "W17S25" with
            Mother = Some "W15S28"
            Perimeter = perimeter
        }
    ]

/// The rows the arena casts for W15S28: the pioneers' and the fighters'.
let private claimRows = [ "worker"; "ranger"; "brawler"; "medic" ]

/// W15S28 as it stands live at t921,063 — RCL7, a 5,600 bank, Spawn3 at
/// 18,30 and Spawn8 at 17,35, 137,274 energy in the Storage at 17,29 — the
/// chain to W17S25 by keeper room W16S26 (no keeper standing: our route is
/// laid round the lairs either way), and W18S25 west of W17S25, where the
/// raid waits. W17S25 is nobody's, under the live `safeModeCooldown`
/// unless `cooldown` says otherwise.
let private claimWorld (cooldown: int) (colonies: Colony list) (bodies: Body list) =
    let mother =
        room "W15S28"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5600
        |> withStructures
            [
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 17; Y = 29 } with
                    Energy = 137_274
                }
            ]

    let child =
        room "W17S25"
        |> withController Ownership.Unowned None 0 0
        |> withSafeModeCooldown cooldown

    arena
        921_000
        [
            mother
            room "W15S27"
            room "W16S28"
            room "W16S27"
            room "W16S26"
            room "W17S26"
            child
            room "W18S25"
        ]
        colonies
        bodies
    |> withCasts claimRows

/// W15S28's standing economy, as live at t921,063 so far as the arena runs
/// it: an anchor on each home rock and a hauler standing for the haul (no
/// supply floor jumps the queue), and the two workers beside the Storage,
/// loaded: the arena digs nothing and a body it casts is born empty, so
/// these two are the pioneers that can reach the nursery with energy.
let private motherStaff =
    [
        body
            "anchor-920000-Spawn3"
            Side.Ours
            (parts [ Work, 6; Carry, 1; Move, 1 ])
            (w15s28 5 30)
            None
        body
            "anchor-920001-Spawn8"
            Side.Ours
            (parts [ Work, 6; Carry, 1; Move, 1 ])
            (w15s28 10 18)
            None
        body "hauler-920000-Spawn3" Side.Ours (parts [ Carry, 32; Move, 16 ]) (w15s28 20 30) None
        { body "worker-920100-Spawn3" Side.Ours pioneerParts (w15s28 18 28) None with
            Energy = 850
        }
        { body "worker-920101-Spawn8" Side.Ours pioneerParts (w15s28 16 28) None with
            Energy = 850
        }
    ]

/// The claimer of W15S28's reserver row, on the controller's ring.
let private claimer =
    body
        "reserver-920400-Spawn3"
        Side.Ours
        (parts [ BodyPart.Claim, 1; Move, 1 ])
        (w17s25 16 35)
        None

/// The live cooldown laid on at the claim's tick. Our side withholds the Claim
/// while one runs (#474), so a scenario of a room claimed under one starts it
/// once the claim has landed: its physics, and not how the claim got there.
let private cooled (a: Arena) =
    { a with
        Rooms =
            a.Rooms
            |> Map.add "W17S25" (withSafeModeCooldown liveCooldown a.Rooms["W17S25"])
    }

/// The nursery controller's Upgrade, as a Task id.
let private nurseryUpgrade =
    match w17s25Capture.RealController with
    | Some(id, _) -> $"upgrade:{id}"
    | None -> failwith "W17S25 has no controller"

/// These bodies holding these Tasks, as the Matcher left them last tick.
let private holding (tasks: (string * string) list) (a: Arena) =
    { a with
        Carried =
            { a.Carried with
                Assignments =
                    (a.Carried.Assignments, tasks) ||> List.fold (fun m (k, v) -> Map.add k v m)
            }
    }

/// The t880,341 raid (§1.1) waiting in W18S25 beyond W17S25's west exits
/// until arena tick `arrival`, then coming for the controller: each melee
/// chasing what stands within four of the controller's ring (the parked
/// block), each healer behind its melee, the tapper — CREEP_CLAIM_LIFE_TIME
/// — walking for the controller.
let private raidFrom (arrival: int) =
    let after inner =
        Some(Phases [ arrival, Hold; System.Int32.MaxValue, inner ])

    [
        body
            "Eternity536"
            trep
            trepMelee
            (w18s25 44 22)
            (after (Chase(Nearest, Some(w17s25 16 36, 4))))
        body
            "Prime803"
            trep
            trepMelee
            (w18s25 44 24)
            (after (Chase(Nearest, Some(w17s25 17 37, 4))))
        body "Prism305" trep trepHealer (w18s25 43 22) (after (Follow "Eternity536"))
        body "Paragon722" trep trepHealer (w18s25 43 24) (after (Follow "Prime803"))
        { body "Rune908" trep trepTapper (w18s25 45 23) (after (Tap controllerAt)) with
            TicksToLive = Engine.claimLifetime
        }
    ]

let private raidIds =
    [ "Eternity536"; "Prime803"; "Prism305"; "Paragon722"; "Rune908" ]

/// The ticks of the run's milestones, for a report and the assertions.
type private Milestones =
    {
        Taps: int list
        Rcl2: int option
        SafeMode: int option
        /// The tick each named body of ours first stood in W17S25.
        Entered: Map<string, int>
        Died: Map<string, int>
    }

let private milestonesOf (trace: TickTrace list) =
    let ticksOf event =
        trace
        |> List.filter (fun t -> t.Events |> List.exists event)
        |> List.map (fun t -> t.Tick)

    {
        Taps =
            ticksOf (function
                | ControllerAttacked("W17S25", _) -> true
                | _ -> false)
        Rcl2 = firstTick (fun e -> e = LeveledUp("W17S25", 2)) trace
        SafeMode =
            firstTick
                (function
                | SafeModeActivated "W17S25" -> true
                | _ -> false)
                trace
        Entered =
            trace
            |> List.collect (fun t ->
                t.Bodies
                |> List.filter (fun b -> b.Side = Side.Ours && b.At.Room = "W17S25")
                |> List.map (fun b -> b.Id, t.Tick))
            |> List.groupBy fst
            |> List.map (fun (id, ticks) -> id, ticks |> List.map snd |> List.min)
            |> Map.ofList
        Died =
            trace
            |> List.collect (fun t ->
                t.Events
                |> List.choose (function
                    | Died id -> Some(id, t.Tick)
                    | _ -> None))
            |> Map.ofList
    }

/// Run on live code until the claim lands — no declaration edited, the
/// Claim's outpost entry standing (#471) — apply `atLanding` on the landing
/// tick, and run `after` ticks more. The claim's tick beside the run.
let private claimThen (atLanding: Arena -> Arena) (after: int) (start: Arena) =
    let claimedYet (a: Arena) =
        a.Rooms["W17S25"].Controller
        |> Option.exists (fun c -> c.Owner = Ownership.Ours)

    let landed, first = start |> runUntil claimedYet 900

    match
        firstTick
            (function
            | ControllerClaimed("W17S25", _) -> true
            | _ -> false)
            first
    with
    | Some claim ->
        let rest, second = landed |> atLanding |> run after
        claim, rest, first @ second
    | None -> failtest $"the claim lands: {milestonesOf first}"

/// Trepidimous' answer to a claim (§1.1): the raid sets out from W18S25
/// `arrival` ticks after the landing.
let private raidAfter (arrival: int) (a: Arena) =
    { a with
        Bodies = a.Bodies @ raidFrom (a.Tick + arrival)
    }

/// G1, the claim party: two of W15S28's workers, loaded, on the
/// controller's ring when the claim lands.
let private party =
    [
        { body "worker-920800-Spawn3" Side.Ours pioneerParts (w17s25 17 37) None with
            Energy = 850
        }
        { body "worker-920801-Spawn8" Side.Ours pioneerParts (w17s25 18 38) None with
            Energy = 850
        }
    ]

let private partyIds = party |> List.map (fun b -> b.Id)

/// The party as its walk leaves it (#471): holding the controller's Upgrade,
/// one Task id before the claim and after it.
let private partyHolds (a: Arena) =
    holding (partyIds |> List.map (fun id -> id, nurseryUpgrade)) a

/// G2: two #447 resident R7s of W15S28's in W17S25 before the claim, on
/// the controller's south ring, holding its Guard from the claim.
let private residents =
    let r7 = parts [ Move, 21; RangedAttack, 14; Heal, 7 ]

    [
        body "ranger-920500-Spawn3" Side.Ours r7 (w17s25 20 38) None
        body "ranger-920501-Spawn8" Side.Ours r7 (w17s25 19 39) None
    ]

let private residentsHold (a: Arena) =
    holding (residents |> List.map (fun b -> b.Id, "guard:W17S25")) a

/// The live spawn site, placed by hand at 14,28 as on t879,239, unbuilt.
let private withSpawnSite (a: Arena) =
    { a with
        Rooms =
            a.Rooms
            |> Map.add
                "W17S25"
                (a.Rooms["W17S25"] |> withSite Side.Ours "spawn" (Some "Spawn10") 0 (tile 14 28))
    }

/// The window after safe mode (the #465 scenario, mothered by W15S28): W17S25
/// at RCL3 with its own spawn and one tower holding 500, never full so the
/// child stays raised (#445); the sixteen perimeter ramparts at 50,000; the
/// safe mode RCL3 banked standing, under the 30,000 ticks of cooldown the
/// spent one left; and the child's one worker, carrying 200.
let private heldWorld (bodies: Body list) =
    let mother =
        room "W15S28"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn3" { X = 18; Y = 30 } 5600
        |> withSpawn "Spawn8" { X = 17; Y = 35 } 5600

    let child =
        room "W17S25"
        |> withController Ownership.Ours None 3 1
        |> withSafeModeCooldown (921_000 + 30_000)
        |> withSpawn "Spawn10" (tile 22 34) 800
        |> withStructures [ towerOf Side.Ours (tile 21 32) 500 ]
        |> withRamparts Side.Ours 50_000 perimeter

    let worker =
        { body
              "worker-920300-Spawn10"
              Side.Ours
              (parts [ Work, 4; Carry, 4; Move, 4 ])
              (w17s25 20 30)
              None with
            Energy = 200
        }

    arena
        921_000
        [
            mother
            room "W15S27"
            room "W16S28"
            room "W16S27"
            room "W16S26"
            room "W17S26"
            child
            room "W18S25"
        ]
        afterClaim
        (worker :: motherStaff @ bodies)
    |> withCasts claimRows

/// The t880,341 raid come back from W18S25 beyond the west exits, as #465
/// plays it: each melee breaking in for the controller's ring, each healer
/// behind its melee, the tapper walking for the controller once a way is
/// open.
let private westRaid =
    [
        body "Eternity536" trep trepMelee (w18s25 44 22) (Some(Breach(w17s25 16 36)))
        body "Prime803" trep trepMelee (w18s25 44 24) (Some(Breach(w17s25 16 36)))
        body "Prism305" trep trepHealer (w18s25 43 22) (Some(Follow "Eternity536"))
        body "Paragon722" trep trepHealer (w18s25 43 24) (Some(Follow "Prime803"))
        { body "Rune908" trep trepTapper (w18s25 45 23) (Some(Tap controllerAt)) with
            TicksToLive = Engine.claimLifetime
        }
    ]


[<Tests>]
let arenaClaimTests =
    testList
        "arena W17S25 claim"
        [
            test
                "scenario 2, live: the mother's loaded workers are the party, the claim waits for them at the controller, and RCL2 comes 7 ticks after it; under a cooldown our side never asks and the tap lands" {
                // #471 on live code: no party placed, no hold scripted, no
                // declaration edited. W15S28's two loaded workers take the
                // party's Upgrade at the Storage, the claimer on the ring
                // waits for them, and Trepidimous answers the claim.
                for arrival in [ 20; 100; 300 ] do
                    let claim, final, trace =
                        claimWorld 0 beforeClaim (claimer :: motherStaff)
                        |> claimThen (cooled >> withSpawnSite >> raidAfter arrival) 400

                    let m = milestonesOf trace
                    let failure = $"arrival {arrival}, claim t{claim}: {m}"
                    // Measured: the claim at t492, the tick both workers
                    // stand in reach (a 16W 17C body walks loaded at half
                    // speed), RCL2 at t499, the tap at claim + arrival + 50.
                    Expect.isGreaterThan
                        claim
                        400
                        $"the claimer waits out five loaded crossings\n{failure}"

                    Expect.equal
                        (m.Rcl2 |> Option.map (fun t -> t - claim))
                        (Some 7)
                        $"two 16-WORK pioneers: 32 a tick\n{failure}"

                    Expect.equal (controllerOf final).SafeModeAvailable 1 "banked, and never spent"

                    // The begun site arms the undefended arm (#449), and the
                    // cooldown silences it (#474): `activateSafeMode.js`
                    // would refuse every ask.
                    Expect.isEmpty (safeModeAsksOf trace) $"no ask\n{failure}"
                    Expect.isNone m.SafeMode failure
                    Expect.isNonEmpty m.Taps $"the tap lands\n{failure}"
            }

            test
                "scenario 2, live, the cooldown run out: safe mode fires as the raid enters and nothing taps; with no begun site the claimer arm fires at its deadline" {
                for arrival in [ 20; 100; 300 ] do
                    let claim, final, trace =
                        claimWorld 0 beforeClaim (claimer :: motherStaff)
                        |> claimThen (withSpawnSite >> raidAfter arrival) 400

                    let m = milestonesOf trace
                    let failure = $"arrival {arrival}, claim t{claim}: {m}"
                    Expect.equal (m.Rcl2 |> Option.map (fun t -> t - claim)) (Some 7) failure

                    match m.SafeMode with
                    | Some fired ->
                        Expect.isLessThanOrEqual fired (claim + arrival + 10) $"on entry\n{failure}"
                    | None -> failtest $"safe mode fires\n{failure}"

                    Expect.isEmpty m.Taps failure

                    Expect.isFalse
                        (trace
                         |> List.exists (fun t ->
                             t.Events
                             |> List.exists (function
                                 | SiteStomped _ -> true
                                 | _ -> false)))
                        $"the site stands under it\n{failure}"

                    Expect.isNonEmpty final.Rooms["W17S25"].Sites "still building"

                // No site begun: only the claimer arm can fire, at
                // `SafeModeDeadline` (3) of the controller.
                let _, _, trace =
                    claimWorld 0 beforeClaim (claimer :: motherStaff)
                    |> claimThen (raidAfter 20) 200

                let m = milestonesOf trace

                match m.SafeMode with
                | Some fired ->
                    let tapper =
                        pathOf "Rune908" trace
                        |> List.tryFind (fun (t, _) -> t = fired - 1)
                        |> Option.bind (fun (_, s) -> RoomPos.range s.At controllerAt)

                    Expect.isTrue
                        (tapper |> Option.exists (fun r -> r <= 3))
                        $"the tapper in reach: {tapper}\n{m}"
                | None -> failtest $"the claimer arm fires\n{m}"

                Expect.isEmpty m.Taps $"no tap\n{m}"
            }

            // Was a live-code defect (#471): on the claim's own tick W17S25
            // was still W15S28's Claim outpost and no nursery, so the party
            // was handed W15S28's own Upgrade and walked home with its 1,700
            // energy. The party now holds the candidate's Upgrade as its walk
            // left it, the Claim's outpost entry standing throughout.
            test
                "G1: a loaded party standing at the controller when the claim lands upgrades it to RCL2 inside 20 ticks" {
                let _, trace =
                    claimWorld 0 beforeClaim (claimer :: motherStaff @ party)
                    |> partyHolds
                    |> run 100

                let m = milestonesOf trace
                Expect.isTrue (m.Rcl2 |> Option.exists (fun t -> t <= 20)) $"RCL2: {m.Rcl2}"
            }

            test
                "scenario 3, G2 with G1 under the live cooldown: the residents kill the tapper before it taps, at least one resident dies, and the duo's Fight pools" {
                for arrival in [ 20; 100; 300 ] do
                    let _, final, trace =
                        claimWorld
                            0
                            beforeClaim
                            (claimer :: motherStaff @ party @ residents @ raidFrom arrival)
                        |> partyHolds
                        |> claimThen (cooled >> residentsHold) 699

                    let m = milestonesOf trace
                    let failure = $"arrival {arrival}: {m}"
                    Expect.equal m.Rcl2 (Some 7) failure
                    Expect.isEmpty m.Taps $"never taps\n{failure}"

                    // Measured: dead at arrival + 45, three ticks under both
                    // residents' fire; one resident dead at +63 or +63/+176.
                    match Map.tryFind "Rune908" m.Died with
                    | Some died -> Expect.isLessThanOrEqual died (arrival + 50) failure
                    | None -> failtest $"the tapper dies\n{failure}"

                    Expect.isTrue
                        (residents |> List.exists (fun r -> m.Died.ContainsKey r.Id))
                        $"the melee catch a resident\n{failure}"

                    // The duo's Fight pools on sight: its brawler cast within
                    // ten ticks of the raid's arrival, its record latched.
                    let squadCast =
                        trace
                        |> List.tryFind (fun t ->
                            t.Ours
                            |> List.exists (function
                                | SpawnCreep(_, _, n) ->
                                    n.StartsWith "brawler-" || n.StartsWith "medic-"
                                | _ -> false))
                        |> Option.map (fun t -> t.Tick)

                    Expect.isTrue
                        (squadCast |> Option.exists (fun t -> t <= arrival + 10))
                        $"cast: {squadCast}\n{failure}"

                    Expect.equal
                        (final.Carried.Raids
                         |> Map.tryFind "W15S28"
                         |> Option.bind (fun r -> Map.tryFind "W17S25" r.Fought)
                         |> Option.bind (fun latch -> latch.Squad))
                        (Some "duo")
                        "the duo latched"

                    // Measured: the duo in the room ~490 ticks after the cast
                    // and the raid dead by +530 for the two early arrivals.
                    if arrival < 300 then
                        for id in raidIds do
                            Expect.isTrue (m.Died.ContainsKey id) $"{id} dies\n{failure}"
            }

            // #472: an unarmed pioneer of the mother's, sent to the nursery's
            // spawn site once RCL2 is banked, walks in by the south corridor
            // toward the raid parked on the controller's ring (16–18,35–38).
            // It used to keep its Build inside the Reach — the site's ground
            // being clear — and die at t489 with no Flee; now it flees the
            // tick the Reach takes it and waits at the Reach's edge.
            test "a pioneer bound for the nursery's site is not walked into a parked raid's reach" {
                // The two at the Storage sent to the site the tick it is
                // placed: with the party holding the claim's Upgrade (#471)
                // they spend their load at home first, and the arena digs
                // nothing to send them out with afterwards.
                let sentToSite =
                    holding
                        [
                            "worker-920100-Spawn3", "build:site-spawn-14-28"
                            "worker-920101-Spawn8", "build:site-spawn-14-28"
                        ]

                let _, _, trace =
                    claimWorld 0 beforeClaim (claimer :: motherStaff @ party @ raidFrom 300)
                    |> partyHolds
                    |> claimThen (cooled >> withSpawnSite >> sentToSite) 519

                let m = milestonesOf trace

                Expect.isTrue
                    (m.Entered.ContainsKey "worker-920100-Spawn3")
                    $"the premise: it walks in: {m}"

                for id in [ "worker-920100-Spawn3"; "worker-920101-Spawn8" ] do
                    Expect.isFalse (m.Died.ContainsKey id) $"{id} lives: {m}"
            }

            test
                "the declared first spawn's site is placed at 14,28 the tick after the claim lands, and once (#476)" {
                let swept =
                    beforeClaim
                    |> List.map (fun colony ->
                        if colony.Home = "W17S25" then
                            { colony with
                                FirstSpawn = Some { X = 14; Y = 28 }
                            }
                        else
                            colony)

                let claim, final, trace =
                    claimWorld 0 swept (claimer :: motherStaff) |> claimThen cooled 20

                let placed =
                    trace
                    |> List.filter (fun t ->
                        t.Ours
                        |> List.exists (function
                            | PlaceSpawnSite _ -> true
                            | _ -> false))
                    |> List.map (fun t ->
                        t.Tick,
                        t.Ours
                        |> List.filter (function
                            | PlaceSpawnSite _ -> true
                            | _ -> false))

                // Spawn3 and Spawn8 stand in the mother: the next is Spawn9.
                Expect.equal
                    placed
                    [ claim + 1, [ PlaceSpawnSite(w17s25 14 28, "Spawn9") ] ]
                    $"one placement, the tick after the claim at t{claim}"

                Expect.equal
                    (final.Rooms["W17S25"].Sites |> List.map (fun s -> s.Kind, s.At, s.Name))
                    [ "spawn", tile 14 28, Some "Spawn9" ]
                    "the site stands"
            }

            test
                "under the live cooldown the Claim waits: the claimer on the ring claims the tick the cooldown ends (#474)" {
                // The claimer alone: with a loaded worker at home the claim
                // would wait out its party's walk as well (#471).
                let _, trace = claimWorld (921_000 + 30) beforeClaim [ claimer ] |> run 40

                Expect.equal
                    (firstTick
                        (function
                        | ControllerClaimed("W17S25", _) -> true
                        | _ -> false)
                        trace)
                    (Some 30)
                    "nothing claims through the cooldown"
            }

            test
                "scenario 4: after safe mode, the 50k perimeter, one tower and the residents on their ramparts hold the full raid with no safe mode to fall back on" {
                let start = heldWorld (residents @ westRaid)
                let lineIds = perimeter |> List.map (fun p -> $"rampart-{p.X}-{p.Y}") |> Set.ofList

                let standing (a: Arena) =
                    a.Rooms["W17S25"].Structures |> List.filter (fun s -> Set.contains s.Id lineIds)

                let final, trace =
                    start
                    |> runUntil
                        (fun a ->
                            List.length (standing a) < lineIds.Count
                            || raidIds
                               |> List.forall (fun id ->
                                   a.Bodies |> List.forall (fun b -> b.Id <> id)))
                        700

                let m = milestonesOf trace
                let failure = $"{m}\n{describe trace}"
                // Measured: the raid dead by t60, the most struck rampart
                // (2,24) at 25,010 of its 50,000.
                Expect.equal
                    (List.length (standing final))
                    lineIds.Count
                    $"the line holds\n{failure}"

                Expect.isEmpty m.Taps "the tapper never reaches the controller"
                Expect.isNone m.SafeMode "no safe mode: the cooldown runs 30,000 more ticks"

                for id in raidIds do
                    Expect.isTrue (m.Died.ContainsKey id) $"{id} dies\n{failure}"

                for r in residents do
                    Expect.isFalse (m.Died.ContainsKey r.Id) $"{r.Id} lives\n{failure}"
            }
        ]
