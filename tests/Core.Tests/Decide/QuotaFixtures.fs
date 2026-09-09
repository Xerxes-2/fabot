/// The rows a colony casts from — hauler, worker, upgrader, Reserver — the
/// workforce target and per-source output they are sized from (ADR 0012, ADR
/// 0042), the body patterns they are cast with (ADR 0006), the supply floor in
/// front of them (ADR 0050), and the Tuning knobs that price them.
/// The quota suite's fixtures: the colonies, banks and casts the rows below
/// are sized against.
module Fabot.Core.Tests.Decide.QuotaFixtures

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

/// The hauler quota fixture: a 3-wide field y = 9..11 from x = 8 to one
/// tile past the spawn, the source embedded in wall at (10,10) with its
/// eight Seats open — a seat-based target roomy enough to leave the
/// hauler row slots — the built source container "can-src" on the Seat
/// (11,10) (a Post), and the spawn structure standing at (spawnX,10).
let quotaRoom spawnX =
    { spatial
          [
              "src-a", { X = 10; Y = 10 }
              "can-src", { X = 11; Y = 10 }
              "spawn-1", { X = spawnX; Y = 10 }
          ]
          [
              for x in 8 .. spawnX + 1 do
                  for y in 9..11 -> { X = x; Y = y }, (if x = 10 && y = 10 then Wall else Plain)
          ] with
        TargetKinds =
            Map.ofList
                [
                    "src-a", Source
                    "can-src", Structure BuiltKind.Container
                    "spawn-1", Structure BuiltKind.Spawn
                ]
    }
    |> withObstacles [ { X = spawnX; Y = 10 } ]

/// The quota fixture's colony: `spawnCount` idle spawns drawing on the one
/// 300-capacity bank holding `available` energy.
let quotaColony spawnX spawnCount available =
    { bareRespawn with
        Spawns =
            [
                for i in 1..spawnCount ->
                    { spawn with
                        Name = $"Spawn{i}"
                        Id = (if i = 1 then "spawn-1" else $"spawn-{i}")
                    }
            ]
        Bank = bank available 300
        Sources = [ source "src-a" ]
        Spatial = quotaRoom spawnX
    }

let haulerCasts intents =
    spawnIntents intents
    |> List.filter (fun (_, _, name: string) -> name.StartsWith "hauler-")
    |> List.length

/// A room whose source containers are a two- and a three-step paved haul
/// from the spawn — the live W12S28 geometry the round trip's repricing
/// was measured against (ADR 0029), flattened onto one paved lane. Sources embedded in
/// wall at (8,10) and (17,10), their built containers on the Seats (9,10)
/// and (16,10) — two Posts — and the spawn structure standing at (12,10),
/// whose free neighbours are (11,10) and (13,10): two steps from the first
/// container, three from the second.
let shortHaulRoom =
    { spatial
          [
              "src-a", { X = 8; Y = 10 }
              "src-b", { X = 17; Y = 10 }
              "can-a", { X = 9; Y = 10 }
              "can-b", { X = 16; Y = 10 }
              "spawn-1", { X = 12; Y = 10 }
          ]
          [
              for x in 8..17 -> { X = x; Y = 10 }, (if x = 8 || x = 17 then Wall else Plain)
          ] with
        TargetKinds =
            Map.ofList
                [
                    "src-a", Source
                    "src-b", Source
                    "can-a", Structure BuiltKind.Container
                    "can-b", Structure BuiltKind.Container
                    "spawn-1", Structure BuiltKind.Spawn
                ]
    }
    |> withHome (fun layer ->
        { layer with
            Roads = Set.ofList [ for x in 9..16 -> { X = x; Y = 10 } ]
            Obstacles = Set.singleton { X = 12; Y = 10 }
        })

/// The arms of the paved cross below, each an id, its container's tile and
/// the rock that container serves — the rock a tile off the lane, range 1
/// from its own container and out of reach of every other. Four arms are
/// fixed and the north one is the caller's, which is the single tile the
/// pairwise case moves.
///
/// The distances are chosen to make the demands read off the page. At a
/// 300 bank the hauler row casts `[4Carry;2Move]` — 200 of carry — the
/// Anchor row casts `2W/1C/1M` and so a Post ships the four a tick that
/// body digs (#208), and a paved step is a tick on either leg: a container
/// `n` steps from the tile a transfer reaches the spawn from is a round
/// trip of `2n` ticks and a demand of `2n × 4 / 200 = n / 25` of a body,
/// 0.52 apiece thirteen steps out and 0.2 apiece five steps out.
let internal haulRoundingArms north =
    [
        "e13", { X = 39; Y = 25 }, { X = 39; Y = 24 }
        "e5", { X = 31; Y = 25 }, { X = 31; Y = 24 }
        "w13", { X = 11; Y = 25 }, { X = 11; Y = 24 }
        "w5", { X = 19; Y = 25 }, { X = 19; Y = 24 }
        "n", north, { north with X = 24 }
    ]

/// The colony those arms make: a paved lane down row 25 and column 25 of
/// the colony's own room with the spawn standing at their crossing, and
/// one built source container on each arm. No controller and no
/// refillable, so the hauler quota is the only thing this fixture answers
/// — three outpost containers and two home ones are the live shape the
/// rounding was measured on (#194), and one room's arms are that shape's
/// arithmetic without a Seam to price it through.
let internal haulRoundingColony arms =
    let spawnPos = { X = 25; Y = 25 }

    let lane =
        [ for x in 4..46 -> { X = x; Y = 25 } ]
        @ [ for y in 4..24 -> { X = 25; Y = y } ]

    let targets =
        arms
        |> List.collect (fun (name, tile, rock) ->
            [
                $"can-{name}", tile, Structure BuiltKind.Container
                $"src-{name}", rock, Source
            ])

    { bareRespawn with
        Controller = None
        Refillables = []
        Sources = arms |> List.map (fun (name, _, _) -> source $"src-{name}")
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                TargetKinds = Map.ofList [ "spawn-1", Structure BuiltKind.Spawn ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        Map.ofList (
                            [ for tile in lane -> tile, Plain ]
                            @ [ for (_, _, rock) in arms -> rock, Wall ]
                        )
                    TargetPositions = Map.ofList [ "spawn-1", spawnPos ]
                    Obstacles = Set.singleton spawnPos
                    Roads = Set.ofList lane
                })
            |> withTargets targets
    }

/// The hauler body this fixture's bank casts, read from the row's own
/// sizing rule rather than restated as a literal: the premise assertions
/// below price their round trips off the Atlas, and a body spelled by hand
/// here would go on passing them against a body the colony no longer
/// casts while the quota beside them moved.
let internal haulRoundingBody = bodyFor haulerPattern 300

/// The same colony at a bank the real W12S28 banks: the geometry is
/// untouched — same sources, same containers, same four idle spawns — and
/// only the bank moves, to 1300 against 1300, so every row's body grows
/// with it. Anchor 6W/1C/1M = 700, hauler 16C/8M = 1200 (16 Carry is 800
/// capacity, so the pair's 480 energy-ticks of demand is 0.6 of a body and
/// the row is one), worker 6W/7C/7M — a Work drain of 6. That drain is the
/// granularity the worker row's rounding is paid in, and it grows with
/// RCL: the fixture the row was pinned at banks 300, where the drain is 1
/// and a lost fraction is worth 0.8 e/tick.
///
/// It is also the rich half of #208's pair. The Anchor row's cast here is
/// 6 Work — twelve a tick, over the ten the owned room pays — so the cap
/// is not binding and each Post is worth the room's rate exactly as it was
/// before that ticket: 20 a tick of income, where the same geometry at the
/// 300 bank above earns 8. Every number in this fixture's cases is
/// therefore unmoved by #208, which is the half of the pair that says the
/// rule is a cap and not a discount.
let richIncomeColony =
    { incomeColony with
        Bank = bank 1300 1300
    }

/// The rich bank's fleet at a given worker count: the rows its quotas
/// pin — one Anchor per Post (2) beside the one hauler this bank's body
/// clears both containers with (ADR 0049) — and as many workers as the
/// case under it is pinning.
let richIncomeFleet workers =
    [ anchor "a1" 0 50; anchor "a2" 0 50; hauler "h1" 0 100 ]
    @ [ for i in 1..workers -> worker $"w{i}" 0 50 ]

/// The whole fleet the switch hires: the home rows and the outpost's,
/// twelve bodies standing exactly at `switchPosted`'s target and six over
/// `switchUnposted`'s.
let internal switchFleet = switchHomeFleet @ switchOutpostRows

/// The same fleet with the named bodies respelled as generalists — the
/// headcount never moves, so a case reading against it reads a *row gap*
/// and nothing else, the deficit being the same number whichever row the
/// twelve bodies were cast from. The spare bodies are named off a
/// prefix of this helper's own, so growing `switchOutpostRows` can never
/// mint a name twice into one fleet.
let internal respelled names fleet =
    fleet
    |> List.mapFold
        (fun n (creep: CreepInfo) ->
            if List.contains creep.Name names then
                worker $"gen{n}" 0 50, n + 1
            else
                creep, n)
        1
    |> fst

/// The fleet with both Anchors respelled: twelve bodies alive and every
/// Post in the colony standing empty.
let internal unmannedPosts = respelled [ "a-home"; "a-out" ] switchFleet

/// The fleet with both haulers respelled: twelve bodies alive and no
/// shipping at all.
let internal unshippedFleet = respelled [ "h-home1"; "h-out1" ] switchFleet

/// A colony standing exactly at its Workforce target with one reserver in
/// it: no Post, no source container and no placed rock, so the target is
/// the floor of two and the two living creeps meet it. One body leaving
/// the count is therefore one cast, which is what makes a lead readable
/// (ADR 0026). The bank is 1,300 — ADR 0042's own reserver body at
/// capacity — and the reserver stands at (25,29), three plain steps from
/// the tile a replacement is born on.
let internal leadColony life =
    let room = atLevel 2 (openRoom 6)

    { room with
        Bank = bank 1300 1300
        Creeps = [ worker "w1" 0 50; reserver "r1" |> withLife life ]
        Spatial =
            room.Spatial
            |> withCreepsAt [ "w1", { X = 25; Y = 27 }; "r1", { X = 25; Y = 29 } ]
    }

/// ADR 0042's own reserver body, which a deficit of one to 1,200 ticks
/// buys: 1,300 energy, 2.17 a tick over a CLAIM part's 600-tick life.
let internal twoBlocks = [ BodyPart.Claim; BodyPart.Claim; Move; Move ]

/// Files our own bodies into one named room's layer of the projection (ADR
/// 0041): a creep the projection places nowhere stands in no room at all, and
/// the row's `Living` and the cases that stand a guard beside its raid both
/// want it standing somewhere real — a guard that stands in the raided room is
/// what the row's `Living`, the Task's holders and the Matcher all read, even
/// though since #272 the count itself reads no body of ours at all.
let internal standingIn room (ours: (CreepInfo * Pos) list) (colony: ColonyView) =
    let layer = SpatialInfo.layerOf colony.Spatial room

    { colony with
        Spatial =
            colony.Spatial
            |> withNeighbour
                room
                { layer with
                    CreepPositions =
                        ours |> List.map (fun (creep, pos) -> creep.Name, pos) |> Map.ofList
                }
    }

/// The guard row's colony (ADR 0056): `reserverColony`'s W12S28 shape with its
/// north outpost declared, posted and held at the reservation cap — so the
/// reserver row wants exactly one block and the Anchor row is at quota — plus
/// the hostiles the case names standing in that outpost and our own bodies
/// standing there beside them.
///
/// The hostiles are a parameter and not a field of the fixture, so a case reads
/// the quiet tick and the raided one **pairwise** off one geometry: what moves
/// between two calls is the raid and can be nothing else.
let internal guardColony hostiles (ours: (CreepInfo * Pos) list) =
    let colony =
        reserverColony
            [ northOutpost true ]
            (surplusFleet 3 @ List.map fst ours)
            [ "W1N2", reservedRoom true 5000 ]

    { colony with Hostiles = hostiles } |> standingIn "W1N2" ours

/// One rock of the north outpost's three-Seat field, which is the whole of the
/// walkable ground `northOutpost` lays: the guard stands on one Seat and the
/// raid on the tile below the rock. The second Seat is for the cases that stand
/// two guards up, the engine putting no two bodies on one tile.
let internal outpostSeat = { X = 41; Y = 40 }

let internal secondSeat = { X = 40; Y = 39 }

let internal raidTile = { X = 40; Y = 41 }

/// The same tile of the *west* outpost's field, for the one case that asks
/// which room a body of the raid is standing in.
let internal westSeat = { X = 21; Y = 40 }

/// A raid of one `smallMelee` and the healers the case names, all in the north
/// outpost: the [[threat]] that makes the room guarded at all (ADR 0033's own
/// test, which a healer fails), and beside it the HEAL parts the count rule
/// prices. Each healer carries an id of its own, a raid being a roster and not
/// one creep.
let internal raidOf healers =
    hostileIn "W1N2" raidTile smallMelee
    :: [
        for i in 1..healers ->
            { hostileIn "W1N2" raidTile smallHealer with
                Id = $"heal-{i}"
            }
    ]

/// The same colony at a named spawn capacity, for the one case that asks what
/// the bank does to the count: 800 and 1,300 buy one guard block, 1,800 — the
/// live RCL5 capacity `guardColony` itself banks — two, and 2,300 three (ADR
/// 0056 decision 1's own table). The capacity moves and the 8,000 banked does
/// not, keeping `reserverColony`'s own property: restraint in these cases comes
/// from the rows, never from the bank running dry between two casts of one
/// tick.
let internal banked capacity (colony: ColonyView) =
    { colony with
        Bank = bank 8000 capacity
    }

/// The `guard` row of the tick's `Quotas`, which is where the cascade writes its
/// own arithmetic down (ADR 0009) — the quota being observability and never a
/// number anything downstream reads.
let internal rowOf name colony =
    (decideOn colony).Quotas.Rows |> List.tryFind (fun row -> row.Row = name)

let internal guardQuotaOf colony =
    rowOf "guard" colony |> Option.map (fun row -> row.Quota)

/// This tick's guard casts, by the row name every creep name carries.
let internal guardCasts intents =
    spawnIntents intents
    |> List.filter (fun (_, _, name: string) -> name.StartsWith "guard-")
    |> List.map (fun (_, body, _) -> body)

/// The Anchor #203 met, spelled as the colony really held it: `6W/1C/1M`,
/// standing full on a full container. One Carry and one Move, and yet
/// nothing that can put a single energy into an extension — a standing
/// body by ADR 0046's ratio (`1 × 4 < 6`) and a Work-heavy one by ADR
/// 0016's (`6 > 1`), so Refill, Withdraw, Build and Repair are all shut to
/// it and Harvest at its Post is the whole of its working life.
let internal liveAnchor name =
    creepWith name 50 0 [ Work; Work; Work; Work; Work; Work; Carry; Move ]

/// #203's colony at the tick the user found it, in the reserver row's own
/// RCL5 shape: both outposts declared, neither posted and neither held, so
/// the row's demand is two bodies at the bank's `[2Claim;2Move]`; the home
/// room's two Posts garrisoned, so the Anchor row is at quota; and a fleet
/// of exactly those two Anchors.
///
/// The bank is the live reading — 361 against a capacity of 1,800 — and it
/// is a **fixed point**, not a slope: nothing alive here can refill an
/// extension, and the engine's spawn regeneration only ticks while the
/// room holds under 300. Every row but the supply floor prices its body at
/// that 1,800 capacity, so the colony stood 1,235 ticks without casting
/// anything at all while 246,818 energy sat in the storage beside it.
let internal deadlockColony =
    { reserverColony
          [ northOutpost false; westOutpost false ]
          [ liveAnchor "a1"; liveAnchor "a2" ]
          [] with
        Bank = bank 361 1800
    }

/// A colony standing exactly at its Workforce target with one body of the
/// given shape in it: the shape `leadColony` above has, at the live RCL5
/// bank of 1,800 instead of 1,300 — no Post, no source container and no
/// placed rock, so the target is the floor of two and the two living
/// creeps meet it. One body leaving the count is therefore one cast, which
/// is what makes a lead readable (ADR 0026). The body under test stands at
/// (25,29), three plain steps from the tile a replacement is born on.
let internal upgraderLeadColony body life =
    let room = atLevel 2 (openRoom 6)

    { room with
        Bank = bank 1800 1800
        Creeps = [ worker "w1" 0 50; creepWith "u1" 0 50 body |> withLife life ]
        Spatial =
            room.Spatial
            |> withCreepsAt [ "w1", { X = 25; Y = 27 }; "u1", { X = 25; Y = 29 } ]
    }

let internal leadCasts body life =
    spawnIntents (decide (upgraderLeadColony body life) Map.empty Set.empty None).Intents

/// The buffer colony at the live RCL5 bank (ADR 0046): the W12S28
/// corridor — a 3-wide plain field y = 9..11 from x = 8 to 32, the two
/// sources embedded in wall at (10,10) and (30,10) with their built
/// containers standing on the Seats (11,10) and (29,10), so two Posts and
/// no Dual Seat — with the controller at (20,11) and the spawn at (20,10)
/// beside it. One spawn and not four, so a tick casts at most one body and
/// the list a case reads names the row whose gap was answered first.
///
/// The bank is 1,800 against 1,800: the row under test is the one whose
/// whole argument is what that bank buys (`11W/1C/11M` against the
/// generalist's `9W/9C/9M`), so a poorer fixture would pin the cascade and
/// not the row.
let internal upgraderRoom =
    { SpatialInfo.empty with
        RoomName = Some "W1N1"
        TargetKinds =
            Map.ofList
                [
                    "spawn-1", Structure BuiltKind.Spawn
                    "src-a", Source
                    "src-b", Source
                    "can-a", Structure BuiltKind.Container
                    "can-b", Structure BuiltKind.Container
                    "ctrl-1", Controller
                ]
    }
    |> withHome (fun layer ->
        { layer with
            Terrain =
                Map.ofList
                    [
                        for x in 8..32 do
                            for y in 9..11 ->
                                { X = x; Y = y },
                                (if (x = 10 || x = 30) && y = 10 then Wall else Plain)
                    ]
            TargetPositions =
                Map.ofList
                    [
                        "spawn-1", { X = 20; Y = 10 }
                        "src-a", { X = 10; Y = 10 }
                        "src-b", { X = 30; Y = 10 }
                        "can-a", { X = 11; Y = 10 }
                        "can-b", { X = 29; Y = 10 }
                        "ctrl-1", { X = 20; Y = 11 }
                    ]
            Obstacles = Set.ofList [ { X = 20; Y = 10 }; { X = 20; Y = 11 } ]
        })

/// The same room with the upgrade buffer at (18,11) — two tiles inside the
/// controller's Upgrade Work Area, on no source's Seat and within range 1
/// of neither rock, so it is the controller's container and not a source's
/// (ADR 0012, ADR 0019). Built or pending is the whole of what the
/// pairwise below varies.
let internal withBuffer kind =
    upgraderRoom |> withTargets [ "can-buf", { X = 18; Y = 11 }, kind ]

let internal upgraderColony room =
    { bareRespawn with
        Bank = bank 1800 1800
        Refillables = []
        Sources = [ source "src-a"; source "src-b" ]
        Controller = Some(controllerAt 5)
        Stages = homeStages room 5
        Spatial = room
    }

/// The two rows the ground hires, and as many of the two surplus rows as
/// the case wants: one Anchor per Post, the one hauler every round trip
/// comes to together at this bank (ADR 0049), then upgraders and
/// generalists. The bank is a parameter and not the literal 1,800 because
/// the standing row's stand-in has to be the body *this* bank's sizing
/// rule casts — a fixture at a poorer bank whose fleet still held
/// `11W/1C/11M` would have `patternOf` read a row the colony could not
/// have cast, and the row's living count is what every reading below is.
let internal upgraderFleetAt capacity anchors upgraders workers =
    [ for i in 1..anchors -> anchor $"a{i}" 0 50 ]
    @ [ hauler "h1" 0 100 ]
    @ [
        for i in 1..upgraders -> creepWith $"u{i}" 0 50 (bodyFor upgraderPattern capacity)
    ]
    @ [ for i in 1..workers -> worker $"w{i}" 0 50 ]

/// The fleet at the live RCL5 bank and this room's two Posts, which is
/// what most of the readings below are read against.
let internal upgraderFleet upgraders workers =
    upgraderFleetAt 1800 2 upgraders workers

/// A construction site standing on the corridor's top row, out of the way
/// of the trunk the haulers walk: what puts a Build in the pool, which is
/// the only thing the worker row's floor reads (ADR 0046).
let internal withBuildSite (colony: ColonyView) =
    { colony with
        ConstructionSites = [ { Id = "site-1" } ]
        Spatial =
            colony.Spatial
            |> withTargets [ "site-1", { X = 16; Y = 9 }, Site BuiltKind.Extension ]
    }

/// A declared outpost one room north: a controller in a room the colony
/// neither owns nor holds, which is the whole of what the reserver row's
/// quota is derived from (ADR 0042). No source and no container, so it
/// adds a reserver place and nothing else to the target.
let internal withDeclaredOutpost (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                Borders = Map.ofList [ "W1N1", plainRing; "W1N2", plainRing ]
                TargetKinds = colony.Spatial.TargetKinds |> Map.add "ctrl-out" Controller
            }
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = Map.ofList (corridor 25 41 48)
                    TargetPositions = Map.ofList [ "ctrl-out", { X = 25; Y = 45 } ]
                }
    }

/// The same colony with a third source embedded in the wall at (14,10)
/// and its built container standing on the Seat at (15,10) — a third Post,
/// and with it a third ten a tick of income. The one knob #195's pairwise
/// turns: two posted sources are a surplus of one and a half standing
/// bodies, three are two and a half.
let internal thirdSource (colony: ColonyView) =
    { colony with
        Sources = colony.Sources @ [ source "src-c" ]
        Spatial =
            colony.Spatial
            |> withTargets
                [
                    "src-c", { X = 14; Y = 10 }, Source
                    "can-c", { X = 15; Y = 10 }, Structure BuiltKind.Container
                ]
            |> withHome (fun layer ->
                { layer with
                    Terrain = layer.Terrain |> Map.add { X = 14; Y = 10 } Wall
                })
    }

/// The same colony with one posted source instead of two — `src-b` and its
/// container out of the projection and out of the Sources beside it. The
/// other side of the same pairwise: half the income is a surplus that does
/// not reach one whole standing body.
let internal oneSource (colony: ColonyView) =
    { colony with
        Sources = colony.Sources |> List.filter (fun s -> s.Id <> "src-b")
        Spatial =
            { colony.Spatial with
                TargetKinds = colony.Spatial.TargetKinds |> Map.remove "src-b" |> Map.remove "can-b"
            }
    }

/// One tick's casts off the one idle spawn: empty, or the one row whose
/// gap came first.
let internal buffered upgraders workers =
    spawnIntents
        (decide
            { upgraderColony (withBuffer (Structure BuiltKind.Container)) with
                Creeps = upgraderFleet upgraders workers
            }
            Map.empty
            Set.empty
            None)
            .Intents

let internal castName casts =
    match casts with
    | [ (_, _, name: string) ] -> name
    | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

/// The same colony with one tunable moved — the whole of what a pairwise
/// case on `Tuning` does, and the reason a rule reads its numbers off the
/// [[colony view]] rather than off a module constant (ADR 0052 decision 5).
let internal tunedBy (change: Tuning -> Tuning) (colony: ColonyView) =
    { colony with
        Tuning = change colony.Tuning
    }

/// A one-room colony whose whole haul is one source container's, with the
/// colony's controller — and the upgrade [[buffer]] standing one tile off
/// it — at the given x on the same three-wide lane. The spawn stands at
/// (25,25) and the source's container at (21,25), so moving the controller
/// moves the buffer's leg and nothing else about the room.
///
/// The controller and its buffer move **together**, which is the one place
/// this departs from the ticket's wording ("the buffer beside the
/// controller against the buffer hugging the spawn"). A container is a
/// buffer because it stands in the controller's own Upgrade Work Area
/// (`Atlas.controllerContainers`, ADR 0019); a container parked by the
/// spawn with the controller left across the room is no buffer at all and
/// would leave the colony with one sink again, which is the state before
/// the case rather than the other half of it. W13S28's own geometry is the
/// pair as written: a controller thirty tiles from the Post that feeds it.
let internal sinkLaneColony available controllerX =
    let spawnPos = { X = 25; Y = 25 }

    { bareRespawn with
        Controller = Some(controllerAt 4)
        Refillables = []
        Sources = [ source "src-a" ]
        Bank = bank available available
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                TargetKinds =
                    Map.ofList
                        [
                            "spawn-1", Structure BuiltKind.Spawn
                            "src-a", Source
                            "can-src", Structure BuiltKind.Container
                            "ctrl-1", Controller
                            "can-buf", Structure BuiltKind.Container
                        ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        Map.ofList
                            [
                                for x in 18..47 do
                                    for y in 24..26 ->
                                        { X = x; Y = y },
                                        (if x = 21 && y = 25 then Wall else Plain)
                            ]
                    TargetPositions =
                        Map.ofList
                            [
                                "spawn-1", spawnPos
                                "src-a", { X = 21; Y = 25 }
                                "can-src", { X = 22; Y = 25 }
                                "ctrl-1", { X = controllerX; Y = 25 }
                                "can-buf", { X = controllerX - 1; Y = 25 }
                            ]
                    Obstacles = Set.ofList [ spawnPos; { X = controllerX; Y = 25 } ]
                })
    }

/// A colony whose one [[post]] is an outpost's, so the Anchor row's ceiling
/// is that room's rate and moves with the reservation on it — which is
/// what makes the row's cast, and therefore its lead, readable at this
/// seam. One Anchor alive with `life` ticks left standing two tiles from
/// the spawn, and one hauler beside it so the supply floor is quiet.
let internal outpostPostColony held life =
    { bareRespawn with
        Controller = None
        Refillables = []
        Sources = [ source "src-out" ]
        Bank = bank 1800 1800
        RoomControl =
            Map.ofList
                [
                    "W1N1", ownedRoom
                    "W1N2", (if held then reservedRoom true 5000 else neutralRoom)
                ]
        Creeps = [ anchor "a1" 0 50 |> withLife life; hauler "h1" 0 100 ]
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                Borders = Map.ofList [ "W1N1", plainRing; "W1N2", plainRing ]
                TargetKinds =
                    Map.ofList
                        [
                            "spawn-1", Structure BuiltKind.Spawn
                            "src-out", Source
                            "can-out", Structure BuiltKind.Container
                        ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        Map.ofList
                            [
                                for x in 9..11 do
                                    for y in 1..10 -> { X = x; Y = y }, Plain
                            ]
                    TargetPositions = Map.ofList [ "spawn-1", { X = 10; Y = 3 } ]
                    CreepPositions =
                        Map.ofList [ "a1", { X = 10; Y = 5 }; "h1", { X = 10; Y = 6 } ]
                    Obstacles = Set.singleton { X = 10; Y = 3 }
                })
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain =
                        Map.ofList
                            [
                                for x in 9..11 do
                                    for y in 44..48 ->
                                        { X = x; Y = y }, (if x = 10 && y = 45 then Wall else Plain)
                            ]
                    TargetPositions =
                        Map.ofList [ "src-out", { X = 10; Y = 45 }; "can-out", { X = 10; Y = 44 } ]
                }
    }

/// **One [[post]] on each side of a border, over two rocks the colony
/// prices differently**: its own room's source at (10,9) with its
/// container standing on the Seat (10,8), and `outpostPostColony`'s
/// outpost rock at (10,45) with its container on (10,44). An Anchor
/// garrisons each of them, standing on the Post itself, and a hauler keeps
/// the [[supply floor]] quiet.
///
/// The shape ADR 0053 is about and the one no fixture could reach before
/// it: while the home Post is in the projection an owned room prices at
/// the held rate, so the old colony-wide `List.max` answered six Work for
/// *both* rocks however the outpost's controller stood — which is why the
/// only fixture that could move the number was one that deleted the home
/// room's Posts (`withoutHomePosts`).
///
/// Three dials and no others: who holds W1N2, and how long each of the two
/// garrisons has left. The one with the shorter life is the one that goes
/// [[expiring]] and so the one whose Post the row is casting into (ADR
/// 0026), which is the whole of what pairs a body to a rock here.
let internal pairedPostColony held homeLife outLife =
    { bareRespawn with
        Controller = None
        Refillables = []
        Sources = [ source "src-home"; source "src-out" ]
        Bank = bank 1800 1800
        RoomControl =
            Map.ofList
                [
                    "W1N1", ownedRoom
                    "W1N2", (if held then reservedRoom true 5000 else neutralRoom)
                ]
        Creeps =
            [
                anchor "a-home" 0 50 |> withLife homeLife
                anchor "a-out" 0 50 |> withLife outLife
                hauler "h1" 0 100
            ]
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                Borders = Map.ofList [ "W1N1", plainRing; "W1N2", plainRing ]
                TargetKinds =
                    Map.ofList
                        [
                            "spawn-1", Structure BuiltKind.Spawn
                            "src-home", Source
                            "can-home", Structure BuiltKind.Container
                            "src-out", Source
                            "can-out", Structure BuiltKind.Container
                        ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        Map.ofList
                            [
                                for x in 9..11 do
                                    for y in 1..10 ->
                                        { X = x; Y = y }, (if x = 10 && y = 9 then Wall else Plain)
                            ]
                    TargetPositions =
                        Map.ofList
                            [
                                "spawn-1", { X = 10; Y = 3 }
                                "src-home", { X = 10; Y = 9 }
                                "can-home", { X = 10; Y = 8 }
                            ]
                    CreepPositions =
                        Map.ofList [ "a-home", { X = 10; Y = 8 }; "h1", { X = 10; Y = 6 } ]
                    Obstacles = Set.singleton { X = 10; Y = 3 }
                })
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain =
                        Map.ofList
                            [
                                for x in 9..11 do
                                    for y in 44..48 ->
                                        { X = x; Y = y }, (if x = 10 && y = 45 then Wall else Plain)
                            ]
                    TargetPositions =
                        Map.ofList [ "src-out", { X = 10; Y = 45 }; "can-out", { X = 10; Y = 44 } ]
                    CreepPositions = Map.ofList [ "a-out", { X = 10; Y = 44 } ]
                }
    }

/// `pairedPostColony` with the outpost's garrison never hired, so its Post
/// stands **genuinely** empty beside a home Post whose incumbent is still
/// standing on it.
///
/// Which is what makes the arrival reading discriminate at all (ADR 0026,
/// ADR 0053 trap (i)): with both Posts garrisoned a rule that judged a
/// vacancy by who is standing *now* finds no free Post anywhere and falls
/// back to the richest ceiling — the same six Work arrival gives, for the
/// wrong reason. Leave the outpost's Post empty and the two readings part:
/// arrival counts the expiring home incumbent out and buys for its held
/// rock, where a standing read sees only the neutral hole and buys three
/// Work for a rock giving ten.
let internal withoutOutpostGarrison (colony: ColonyView) =
    { colony with
        Creeps = colony.Creeps |> List.filter (fun creep -> creep.Name <> "a-out")
        Spatial =
            colony.Spatial
            |> withNeighbour
                "W1N2"
                { Map.find "W1N2" colony.Spatial.Rooms with
                    CreepPositions = Map.empty
                }
    }

/// A colony whose surplus lands in the band #200 is about: five Posts of
/// its own paying ten a tick each, no haul priceable at all — the rocks
/// and the controller sit in two regions of one room with no ground
/// between them — and an 1,800 bank. Income 50 a tick over a lifetime is
/// 75,000; the Anchor row's five 700-energy bodies are the only
/// amortization, so the surplus is exactly **71,500**.
///
/// At that surplus the upgrader row's two divisors part: 71,500 over one
/// body's lifetime drink (16,500) is four, and over the drink plus the
/// body it is spent on (18,200) is three. The fleet is the readout.
let internal upgraderBandColony upgraders =
    let rocks =
        [
            { X = 7; Y = 7 }
            { X = 7; Y = 10 }
            { X = 7; Y = 13 }
            { X = 12; Y = 7 }
            { X = 12; Y = 10 }
        ]

    let cans = rocks |> List.map (fun rock -> { rock with X = rock.X + 1 })

    { bareRespawn with
        Controller = Some(controllerAt 5)
        Refillables = []
        Bank = bank 1800 1800
        Sources = [ for i in 1 .. List.length rocks -> source $"src-{i}" ]
        Creeps =
            [ for i in 1..5 -> anchor $"a{i}" 0 50 ]
            @ [ hauler "h1" 0 100 ]
            @ [
                for i in 1..upgraders ->
                    creepWith
                        $"u{i}"
                        0
                        50
                        (List.replicate 11 Work @ [ Carry ] @ List.replicate 11 Move)
            ]
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                TargetKinds =
                    Map.ofList (
                        [
                            "spawn-1", Structure BuiltKind.Spawn
                            "ctrl-1", Controller
                            "can-buf", Structure BuiltKind.Container
                        ]
                        @ [ for i in 1 .. List.length rocks -> $"src-{i}", Source ]
                        @ [
                            for i in 1 .. List.length cans ->
                                $"can-{i}", Structure BuiltKind.Container
                        ]
                    )
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        Map.ofList (
                            [
                                for x in 5..15 do
                                    for y in 5..15 ->
                                        { X = x; Y = y },
                                        (if List.contains { X = x; Y = y } rocks then
                                             Wall
                                         else
                                             Plain)
                            ]
                            @ [
                                for x in 30..40 do
                                    for y in 30..40 -> { X = x; Y = y }, Plain
                            ]
                        )
                    TargetPositions =
                        Map.ofList (
                            [
                                "spawn-1", { X = 34; Y = 34 }
                                "ctrl-1", { X = 35; Y = 35 }
                                "can-buf", { X = 34; Y = 35 }
                            ]
                            @ [ for i, rock in List.indexed rocks -> $"src-{i + 1}", rock ]
                            @ [ for i, can in List.indexed cans -> $"can-{i + 1}", can ]
                        )
                    Obstacles = Set.ofList [ { X = 34; Y = 34 }; { X = 35; Y = 35 } ]
                })
    }
