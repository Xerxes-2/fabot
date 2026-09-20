/// The outpost's source container, what it adds to the hauler quota, and
/// the Anchor's lead over it.
module Fabot.Core.Tests.Decide.OutpostHaulTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.OutpostFixtures

[<Tests>]
let outpostContainerTests =
    testList
        "the outpost's source container"
        [
            test "the site lands on the Seat whose walk out to the Seam is shortest" {
                // An outpost has no spawn for a trunk to anchor on, so the pick is
                // anchored on the Seam, measured as a walk and never a range: the Seat
                // the range would pick is the one three swamp tiles from the border.
                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]

                Expect.equal
                    (Atlas.seatTilesOf (Atlas.ofView colony) "src-out" |> RoomPos.inRoom "W1N2")
                    (Set.ofList [ { X = 10; Y = 45 }; { X = 11; Y = 43 } ])
                    "the premise: the rock has two Seats, and the nearer one to the border is (10,45)"

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "the farther Seat wins, because the ground between it and the Seam is cheaper"
            }

            test "the Intent carries the outpost's own room, never the colony's" {
                // `planLayout` stamps the one room it plans onto every site it emits,
                // so a pick routed through it would land on the home room's tile of the
                // same coordinates. (11,43) is a real coordinate in both rooms.
                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]

                // The whole list, never `Expect.all`, which is vacuously true of an
                // empty one.
                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "the one site this rule places names the room its source stands in"
            }

            test "a room the colony cannot see this tick is planned nothing" {
                // With no vision the container census is empty because nobody looked.
                // `Game.rooms` holds the seen rooms alone, so the Intent could only be
                // reported as `ActorMissing`, once a tick per rock.
                let seen =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]

                Expect.isNonEmpty
                    (containerSites seen)
                    "the premise: seen, this rock is planned a container"

                Expect.isEmpty
                    (containerSites
                        { seen with
                            RoomControl = Map.remove "W1N2" seen.RoomControl
                        })
                    "and the same tick with the room unseen plans nothing at all"
            }

            test "a source with one Seat is the same rule with one candidate" {
                // W13S28's `16,7` is a single-Seat rock, and "the shortest"
                // has to answer where there is nothing to be shorter than.
                let ground =
                    [
                        { X = 10; Y = 45 }, Swamp
                        for y in 46..48 do
                            { X = 10; Y = y }, Plain
                    ]

                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" ground [ "src-out", outpostSource, Source ]

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 10; Y = 45 } ]
                    "the one Seat there is, priced and picked like any other"
            }

            test "Seats that price alike fall to the lowest (X, Y), as every tie here does" {
                // W12S27's `16,45` has three swamp Seats over one plain apron, so the
                // three walks are equal by construction and only the tie-break
                // separates them. It also pins the subtraction: the Seat's own swamp
                // step is charged to whatever walks *in* to it.
                let ground =
                    [
                        { X = 9; Y = 45 }, Swamp
                        { X = 10; Y = 45 }, Swamp
                        { X = 11; Y = 45 }, Swamp
                        for x in 8..12 do
                            for y in 46..48 do
                                { X = x; Y = y }, Plain
                    ]

                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" ground [ "src-out", outpostSource, Source ]

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 9; Y = 45 } ]
                    "the three Seats tie, and the lowest X answers"
            }

            test "a Seat's own terrain is not charged to it: the walk is the ground beyond it" {
                // A walk charges the tiles a creep steps onto, never the tile it stands
                // on. Two Seats over one symmetric apron differ only in their own
                // terrain; charging a Seat for standing on it would pick the plain one.
                // Whoever hauls from that container starts on it and never pays to arrive.
                let ground =
                    [
                        { X = 9; Y = 45 }, Swamp
                        { X = 11; Y = 45 }, Plain
                        for x in 8..12 do
                            for y in 46..48 do
                                { X = x; Y = y }, Plain
                    ]

                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" ground [ "src-out", outpostSource, Source ]

                Expect.equal
                    (Atlas.seatTilesOf (Atlas.ofView colony) "src-out" |> RoomPos.inRoom "W1N2")
                    (Set.ofList [ { X = 9; Y = 45 }; { X = 11; Y = 45 } ])
                    "the premise: two Seats, one swamp and one plain, over the same apron"

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 9; Y = 45 } ]
                    "the swamp Seat is no dearer than the plain one, so the tie-break answers"
            }

            test "a container already serving the source is planned for no second one" {
                // By target and not by tile: the thing serving the rock is on (10,45),
                // not the tile the plan picked. Standing and pending both.
                let served kind =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround
                        "W1N2"
                        detourGround
                        [ "src-out", outpostSource, Source; "con-out", { X = 10; Y = 45 }, kind ]

                Expect.isEmpty
                    (containerSites (served (Structure BuiltKind.Container)))
                    "a container standing within range 1 of the rock, on a Seat the plan did not pick"

                Expect.isEmpty
                    (containerSites (served (Site BuiltKind.Container)))
                    "and a site pending there, which is a container already being built"
            }

            test "a Seat another kind's site already holds is no candidate at all" {
                // #244, live in W13S29: hand-placed road sites landed on the two Seats
                // this rule had picked, (28,6) and (15,28). The engine takes one
                // construction site per tile, so the container was refused
                // ERR_INVALID_TARGET once a tick, for ever, and the rock was no Post.
                let siteOn tile =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround
                        "W1N2"
                        detourGround
                        [ "src-out", outpostSource, Source; "road-out", tile, Site BuiltKind.Road ]

                Expect.equal
                    (containerSites (siteOn { X = 11; Y = 43 }))
                    [ "W1N2", { X = 10; Y = 45 } ]
                    "the Seat the walk picked is taken, so the dearer Seat takes the container"

                Expect.equal
                    (containerSites (siteOn { X = 10; Y = 45 }))
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "and a site on the Seat that lost moves nothing: one tile is subtracted, not a source"
            }

            test "a road that already stands is no obstruction, and is the best tile there is" {
                // A *finished* structure holds no site, and a container on a paved Seat
                // is the tile the hauler arrives over anyway. A built road prices the
                // tile too, so it is laid in both pieces the shell lays one in (`paved`).
                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround
                        "W1N2"
                        detourGround
                        [
                            "src-out", outpostSource, Source
                            "road-out", { X = 11; Y = 43 }, Structure BuiltKind.Road
                        ]
                    |> paved "W1N2" [ { X = 11; Y = 43 } ]

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "the pick is unmoved by a road that has finished going up on it"
            }

            test "a source whose every Seat is taken plans nothing and waits" {
                // Waiting is not self-clearing: a road site in an outpost is a plain
                // Surplus Build outside the builders' budget, so the human's site is
                // the only thing that ends this. No Intent, this tick or any other.
                let bothTaken =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround
                        "W1N2"
                        detourGround
                        [
                            "src-out", outpostSource, Source
                            "road-a", { X = 11; Y = 43 }, Site BuiltKind.Road
                            "road-b", { X = 10; Y = 45 }, Site BuiltKind.Road
                        ]

                Expect.isEmpty
                    (containerSites bothTaken)
                    "both Seats hold a site, so this rock is planned no container this tick"
            }

            test "a Seat a rival's site holds is no candidate either, whatever it is building" {
                // #248: the site census was read off `FIND_MY_CONSTRUCTION_SITES`, and
                // out here another player's site is the one it most needs to see. The
                // engine takes one site per tile whoever placed it.
                let rivalOn tile =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]
                    |> rivalSites "W1N2" [ tile ]

                Expect.equal
                    (containerSites (rivalOn { X = 11; Y = 43 }))
                    [ "W1N2", { X = 10; Y = 45 } ]
                    "the Seat the walk picked is a rival's, so the dearer Seat takes the container"

                // The projection carries a rival's site as a tile and no kind, so it
                // never answers "this rock is served": a rival's container never will
                // be ours.
                Expect.equal
                    (containerSites (rivalOn { X = 10; Y = 45 }))
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "the Seat that lost is a rival's, and the rock is still unserved"
            }

            test "a home container on the pick's coordinates defers nothing" {
                // A `Pos` carries no room, so a census unioning both rooms' container
                // tiles would read the home container as serving a rock fifty tiles
                // away and defer the outpost's container forever.
                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]
                    |> withTarget "con-home" { X = 11; Y = 43 } (Structure BuiltKind.Container)

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "the outpost's rock is unserved: what stands on those coordinates stands at home"
            }

            test "the home room's Layout is not moved by an outpost joining the projection" {
                // This rule runs beside the Layout and never inside it. The trunk
                // fixture's ground stops fourteen tiles short of its own north border,
                // and the container's Seat is picked on the walk out to that border, so
                // the ground is laid on **both** colonies to keep the comparison like
                // for like.
                let reachingItsBorder (colony: ColonyView) =
                    colony
                    |> withOutpostLayer "W1N1" (fun layer ->
                        { layer with
                            Terrain =
                                (layer.Terrain, [ for y in 1..14 -> { X = 10; Y = y } ])
                                ||> List.fold (fun terrain tile ->
                                    TerrainGrid.add tile Plain terrain)
                        })

                let alone = reachingItsBorder (trunkColony 4)

                let withOutpost =
                    alone
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]

                let atHome colony =
                    let { Intents = intents } = decideOn colony

                    placementIntents intents |> List.filter (fun (room, _, _) -> room = "W1N1")

                Expect.isNonEmpty (atHome alone) "the premise: this room has a Layout to move"

                Expect.equal
                    (atHome withOutpost)
                    (atHome alone)
                    "every home site the Layout placed, unmoved and in its own order"

                Expect.equal
                    (containerSites withOutpost |> List.filter (fun (room, _) -> room <> "W1N1"))
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "and the one site the outpost gained is the container, in the outpost"
            }

            test "a room home shares no border with is planned nothing" {
                // Two rooms four sectors apart share no band, so the walk that anchors
                // the pick has no anchor. W5N5 is not W1N1's neighbour.
                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W5N5" detourGround [ "src-out", outpostSource, Source ]

                Expect.isEmpty
                    (containerSites colony)
                    "no band to price a Seat against, so no Seat is picked"
            }
        ]

[<Tests>]
let outpostHaulTests =
    testList
        "the outpost's container in the hauler quota"
        [
            test "an outpost container hires haul capacity, priced at its own room's rate" {
                // The round trip is the Seam join (`Atlas.haulRoundTripTicks`), 51
                // ticks over this corridor. At the 600 bank the hauler carries 400, so
                // held the rock ships ten a tick and hires ceil(51 x 10 / 400) = 2.
                //
                // Pairwise: the two colonies differ in who holds W1N2 and nothing else.
                let held control =
                    quotaOf (haulHome |> withHaulOutpost (Some control))

                Expect.equal
                    (quotaOf haulHome)
                    0
                    "the premise: without the outpost there is no haul"

                // Read off the demand, not the quota: since #279 a haul across a Seam
                // is floored at two bodies, so the quota cannot see which rock ships
                // ten a tick.
                let shipped control =
                    haulDemandOf (haulHome |> withHaulOutpost (Some control))

                Expect.equal
                    (shipped (reservedRoom true 4000))
                    510
                    "reserved, the rock ships ten a tick"

                Expect.equal
                    (shipped neutralRoom)
                    255
                    "held by nobody it ships five, and the sum the quota divides is halved with it"

                Expect.equal
                    (held (reservedRoom true 4000))
                    2
                    "and either way the crossing is never one body's to lose (#279)"

                Expect.equal
                    (held neutralRoom)
                    2
                    "including the neutral rock, whose overflow decays the same"

                // #279's premise: a full container out here drops the Anchor's next
                // fifty on the floor to decay. Live it was 1,170 energy-ticks against
                // a 1,200 load, one body at 97.5% of itself.
                Expect.isTrue
                    (shipped (reservedRoom true 4000) * 2 >= 400)
                    "the premise: the crossing is worth half a 400 load or more"

                Expect.equal
                    (held ownedRoom)
                    (held (reservedRoom true 4000))
                    "owned or reserved by us is one rate, as the engine pays it"

                Expect.equal
                    (quotaOf (haulHome |> withHaulOutpost None))
                    0
                    "and a room the colony cannot see prices no rock at all (ADR 0004)"
            }

            test "two containers across the same Seam are one sum, rounded once" {
                // #194: `haulRoundingTests` has no Seam in it, so nothing before this
                // pooled *two* cross-room terms. Both rocks ship ten a tick, a 400
                // carry is a body per 40 ticks of round trip, and 51 + 63 comes to
                // 2.85 bodies and hires three; a ceiling apiece would hire four.
                let colony =
                    haulHome
                    |> withHaulOutpost (Some(reservedRoom true 4000))
                    |> withSecondHaulContainer { X = 33; Y = 44 }

                let atlas = Atlas.ofView colony

                // The body the quota itself divides by: this fixture's 600 bank, not
                // `haulRoundingBody`'s 300 (#208). Road parity holds at every size.
                let roundTrip from =
                    Atlas.haulRoundTripTicks
                        atlas
                        (bodyFor haulerPattern 600)
                        (RoomPos.at "W1N2" from)
                        (RoomPos.at "W1N1" { X = 25; Y = 10 })

                Expect.equal
                    (roundTrip { X = 25; Y = 41 })
                    (Some 51)
                    "the premise: the near container's Seam crossing"

                Expect.equal
                    (roundTrip { X = 33; Y = 44 })
                    (Some 63)
                    "the premise: and the branch container's, eight steps out along the branch"

                Expect.equal (quotaOf colony) 3 "the two crossings are summed and rounded once"
            }

            test "one crossing container's longer haul moves the pair by a body" {
                // The branch container's round trip goes 63 to 72 ticks, 1.575 of a
                // body to 1.8: its own ceiling is two either way, so only the pooled
                // sum, 2.85 to 3.075, sees the move.
                let atTile tile =
                    haulHome
                    |> withHaulOutpost (Some(reservedRoom true 4000))
                    |> withSecondHaulContainer tile

                Expect.equal
                    (quotaOf (atTile { X = 33; Y = 44 }))
                    3
                    "the premise: eight steps down the branch hires three"

                Expect.equal
                    (quotaOf (atTile { X = 36; Y = 44 }))
                    4
                    "three steps further out is one body more"
            }

            test "a container in a room the projection does not carry hires nobody" {
                // The container is in the census and the colony holds the room, but
                // the projection places neither it nor its rock: no tile to flood from.
                let seen = haulHome |> withHaulOutpost (Some(reservedRoom true 4000))

                let unprojected =
                    { seen with
                        Spatial =
                            { seen.Spatial with
                                Rooms = Map.remove "W1N2" seen.Spatial.Rooms
                            }
                    }

                Expect.equal (quotaOf seen) 2 "the premise: projected, the container hires two"
                Expect.equal (quotaOf unprojected) 0 "unprojected, the same census hires none"
            }

            test "a quota memoised while the outpost was held is not handed back when it lapses" {
                // #127's memo case: the quota reads a second room's held rate, so the
                // census signature signs every projected room's. Every other census
                // input is byte-identical between the two views.
                let lapsed = haulHome |> withHaulOutpost (Some neutralRoom)

                let previous =
                    (decide
                        (haulHome |> withHaulOutpost (Some(reservedRoom true 4000)))
                        Map.empty
                        Set.empty
                        None)
                        .Memo

                // Read off the demand, not the quota: since #279 both rates hire two
                // across a Seam, and only the sum the quota divides halves with the rate.
                let shipped (memo: PlanMemo) =
                    memo.HaulerDemand |> List.sumBy (fun row -> row.Demand)

                Expect.equal
                    (shipped previous)
                    510
                    "the premise: held, the container ships ten a tick"

                let recalled = decide lapsed Map.empty Set.empty (Some previous)
                let fresh = decideOn lapsed

                Expect.equal (shipped fresh.Memo) 255 "the premise: lapsed, it ships five"

                Expect.equal
                    (shipped recalled.Memo)
                    (shipped fresh.Memo)
                    "the stale memo recomputes rather than handing back the held rate's haul"

                Expect.equal
                    (spawnIntents recalled.Intents)
                    (spawnIntents fresh.Intents)
                    "so the fleet the colony casts is the one the halved haul asked for"
            }

            test
                "the quota memoised before an outpost container stood is not handed back once it does" {
                // The census entry itself: this moves nothing but whether `can-out`
                // stands in W1N2, which only a standing census spanning every projected
                // room can see. Signed against the home layer alone the two views sign
                // one string.
                let standing = haulHome |> withHaulOutpost (Some(reservedRoom true 4000))
                let before = standing |> beforeHaulContainer

                let previous = (decideOn before).Memo

                Expect.equal
                    previous.HaulerQuota
                    0
                    "the premise: with no container on the Seat there is no haul to hire for"

                let recalled = decide standing Map.empty Set.empty (Some previous)
                let fresh = decideOn standing

                Expect.equal fresh.Memo.HaulerQuota 2 "the premise: standing, it hires two"

                Expect.equal
                    recalled.Memo.HaulerQuota
                    fresh.Memo.HaulerQuota
                    "the container standing up moves the signature, so the quota is recomputed"

                Expect.equal
                    (spawnIntents recalled.Intents)
                    (spawnIntents fresh.Intents)
                    "and the fleet the colony casts is the one the new haul asked for"
            }
        ]

[<Tests>]
let outpostSuccessionTests =
    testList
        "an outpost's Anchor and its lead"
        [
            test "an Anchor a room away is expiring, and its replacement is cast before it dies" {
                // #153: a lead priced off the home room's flood alone answered 0 for a
                // creep the home room did not place, so an outpost's successor was cast
                // only once the garrison was dead.
                //
                // The Anchor row at this 300 bank is two Work over a Carry and a Move
                // (`anchorBodyFor`): twelve ticks in the spawner and two ticks a plain
                // step. Born on (25,9), eight tiles up to (25,1), the exit at (25,0),
                // moved to (25,49) for nothing, off onto (25,48) and seven more down to
                // the container at (25,41): 16 tiles at two ticks plus the exit's two,
                // 34 of walking and a lead of 46.
                Expect.equal
                    (castNames (outpostSuccession 1500))
                    (castNames (outpostSuccession 47))
                    "one tick outside its lead the garrison still counts, exactly as a fresh one does"

                match castNames (outpostSuccession 47) with
                | [ name ] ->
                    Expect.stringStarts name "hauler-" "the premise: the Anchor row is filled"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                match castNames (outpostSuccession 46) with
                | [ name ] ->
                    Expect.stringStarts
                        name
                        "anchor-"
                        "at its lead the outpost's row is short and the successor is cast"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                match castNames (outpostSuccession 1) with
                | [ name ] ->
                    Expect.stringStarts
                        name
                        "anchor-"
                        "and a tick from death it is being replaced, not mourned"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "an outpost no crossing reaches leads nobody, however little life is left" {
                // With no ring in the projection the two rooms share no Seam band, so
                // there is no walk to price: a lead of 0 leaves the garrison counted
                // living to its last tick.
                let unbordered life =
                    let colony = outpostSuccession life

                    { colony with
                        Spatial =
                            { colony.Spatial with
                                Borders = Map.empty
                            }
                    }

                Expect.equal
                    (castNames (unbordered 1))
                    (castNames (unbordered 1500))
                    "the same colony casts the same body whether the garrison is dying or fresh"

                // A worker and not a hauler, because the same missing band leaves the
                // container's haul unhired. What this case reads is the row it is
                // *not*: the Anchor row is filled.
                match castNames (unbordered 1) with
                | [ name ] ->
                    Expect.stringStarts name "worker-" "and the Anchor row reads as filled"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }
        ]
