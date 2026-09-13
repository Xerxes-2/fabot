/// The Layout's invariants on real terrain, the losses the sweep found,
/// and the clustered horizon at RCL6 (ADR 0055).
module Fabot.Core.Tests.RoomLayoutInvariantTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas
open Fabot.Core.Decide
open Fabot.Core.Tests.RoomFixtures
open Fabot.Core.Tests.Decide
open Fabot.Core.Tests.RoomInvariantFixtures

[<Tests>]
let invariantTests =
    testList
        "layout invariants on real terrain"
        [
            test "the sweep covers every room from many spawns" {
                // The guard on the rest of this list: an invariant asserted
                // over an empty sweep is green and says nothing.
                let cases = sweep.Value

                Expect.isGreaterThan (List.length cases) 100 "a sweep worth the name"

                Expect.equal
                    (cases |> List.map (fun case -> case.Room.Name) |> List.distinct |> List.sort)
                    (rooms |> List.map (fun room -> room.Name) |> List.sort)
                    "every captured room is planned"

                Expect.isTrue
                    (cases
                     |> List.exists (fun case ->
                         case.Room.Name = "W12S28" && case.Spawn = { X = 12; Y = 40 }))
                    "W12S28's own spawn tile is swept"
            }

            test "every Link footing target the Layout names is served" {
                // The #77 detector, and the invariant ADR 0035 made cheap:
                // an unserved target is a guarantee the colony no longer
                // has. Across every room and every spawn there is not one,
                // so this is the strong form with no ceiling to weaken it.
                Expect.isEmpty
                    (violations (fun case -> not (List.isEmpty case.Unserved)))
                    "a room whose plan is short a footing"
            }

            test "the footing target count is the rule's, never a constant" {
                // The targets are the container picks plus the Storage
                // (ADR 0022, ADR 0027), so sources + 2 is arithmetic the
                // room does rather than a number written down — one
                // container per source, one for the controller, one
                // Storage. A three-source room holds five.
                let miscounts (case: Case) =
                    let containers =
                        if plansBuffer case.Room.Buffer case.Spawn then
                            case.SourceCount + 1
                        else
                            case.SourceCount

                    List.length case.Containers <> containers
                    || List.length (tilesOfKind Storage case.Placed) <> 1

                Expect.isEmpty
                    (violations miscounts)
                    "a room whose footing targets its sources do not explain"
            }

            test "every reserved footing is off the trunks, the targets and the others" {
                // ADR 0036's fourth invariant, unassertable when that ADR
                // was written and assertable now that the Layout records
                // the tiles it reserved (#106). The search rule in full:
                // range 1 of its target, off every tile the Layout paves,
                // off the footings' own targets, off every other footing
                // (ADR 0022, ADR 0027). Real terrain is what makes this
                // worth asserting — a footing is chosen from whatever
                // handful of tiles a target's ring leaves, and hand-built
                // fixtures can only pose the collisions their author
                // imagined.
                let breaksTheRule (case: Case) =
                    // The road plan the fold filtered on, read off the
                    // sites: a swept colony starts with no road standing
                    // and no road pending, so the gap the first tick asks
                    // for is the whole plan rather than the remainder of
                    // one.
                    let paved = tilesOfKind Road case.Placed |> Set.ofList

                    let targets =
                        (case.Served |> List.map (fun footing -> RoomPos.pos footing.Target))
                        @ (case.Unserved |> List.map (fun footing -> RoomPos.pos footing.Target))
                        |> Set.ofList

                    let tiles = case.Served |> List.map (fun footing -> RoomPos.pos footing.Tile)

                    List.length (List.distinct tiles) <> List.length tiles
                    || case.Served
                       |> List.exists (fun footing ->
                           RoomPos.range footing.Tile footing.Target <> Some 1
                           || Set.contains (RoomPos.pos footing.Tile) paved
                           || Set.contains (RoomPos.pos footing.Tile) targets)

                Expect.isEmpty
                    (violations breaksTheRule)
                    "a room reserving a footing on a road, on a target or on another footing"
            }

            test "served and unserved footings partition the room's targets" {
                // The two records are one record read two ways, so their
                // counts sum to the targets the room actually constructs —
                // one per planned container plus the Storage (ADR 0022,
                // ADR 0027) — and no target is in both. Counted off the
                // room's own arithmetic rather than off sources + 2, which
                // #104's swamp pocket is a standing counterexample to. The
                // containers are counted a tick on, where the road plan no
                // longer defers them, and the Storage off this tick's
                // sites: the two plans name the same tiles, and no
                // container pick is ever a Storage pick — one is working
                // ground and the ordering never offers the other (ADR
                // 0022) — so nothing collapses between the counts.
                let miscounts (case: Case) =
                    let targets =
                        (case.Served |> List.map (fun footing -> footing.Target))
                        @ (case.Unserved |> List.map (fun footing -> footing.Target))

                    List.length targets
                    <> List.length case.Containers + List.length (tilesOfKind Storage case.Placed)
                    || List.length (List.distinct targets) <> List.length targets

                Expect.isEmpty
                    (violations miscounts)
                    "a room whose two footing records do not partition its targets"
            }

            test "the clustered ordering never takes working ground" {
                // ADR 0022: a tower, extension or Storage on a Seat or an
                // Upgrade tile eats a tile an Anchor or an upgrader stands
                // on, and nothing they do is worth that.
                let onWorkingGround (case: Case) =
                    let working = workingGroundIn case.Atlas case.Room.Name
                    clusteredTiles case.Placed |> Set.exists (fun tile -> Set.contains tile working)

                Expect.isEmpty
                    (violations onWorkingGround)
                    "a room clustering onto ground the colony works from"
            }

            test "the trunks carry every source to the spawn and the controller" {
                Expect.isEmpty
                    (violations (fun case ->
                        not (List.contains case.Spawn case.Room.SealedDoorsteps)
                        && not (trunksCarryEverySource case)))
                    "a source with no paved line home"
            }

            test "every tile the Layout paves is one a creep can stand on" {
                // A trunk is a paved line, so every tile of it has to be
                // walkable ground: a road on a wall is not a road.
                let pavesTheImpassable (case: Case) =
                    let walkable = walkableTilesIn case.Atlas case.Room.Name

                    tilesOfKind Road case.Placed
                    |> List.exists (fun tile -> not (Set.contains tile walkable))

                Expect.isEmpty
                    (violations pavesTheImpassable)
                    "a room paving ground nothing can walk"
            }

            test "no tile is asked for two structures in one tick" {
                // Ramparts are excluded by construction: one goes over
                // every Keep structure and every Post a container stands
                // on, so sharing a tile is what a rampart is for (ADR
                // 0034).
                let doubleBooked (case: Case) =
                    let footprints =
                        case.Placed
                        |> List.filter (fun (_, kind) -> kind <> Rampart)
                        |> List.map fst

                    List.length (List.distinct footprints) <> List.length footprints

                Expect.isEmpty
                    (violations doubleBooked)
                    "a room asking two structures onto one tile"
            }

            test "a level never takes a clustered tile back" {
                // What climbing the ladder changes is only which reserved
                // tiles the placement filter lets through, so nothing a
                // colony was already building should move because it
                // levelled up. The whole ladder — 1 to 2 to 3 to 4 to 5 to
                // 8, 680 level pairs across this sweep — was checked once
                // by hand and holds everywhere; the 2-to-4 pair is the one
                // pinned here, because it is the one that costs a plan.
                let shrinks (case: Case) =
                    not (Set.isSubset case.ClusterAtRcl2 (clusteredTiles case.Placed))

                Expect.isEmpty (violations shrinks) "a room dropping a clustered tile as it levels"
            }

            test "the dropped trunks the Layout records are the ones its roads show" {
                // #107's record, pinned against an independent derivation
                // rather than against itself. The Layout says which
                // (source, goal) pairs it could not route; the road plan
                // says which sources its paved tiles do not carry to which
                // goal, and the two must name the same pairs in every case
                // — the sealed doorstep included, which is the whole point
                // of recording the loss instead of dropping it (#105).
                // Compared as sets, so the invariant is about the pairs and
                // not about the order the fold happens to accumulate them.
                let disagrees (case: Case) =
                    Set.ofList (unroutedByRoads case) <> Set.ofList case.Unrouted

                Expect.isEmpty
                    (violations disagrees)
                    "a room whose dropped trunks and whose road plan tell different stories"
            }

            test "a plan recalled from its memo is the plan that was computed" {
                // ADR 0017's guarantee, stated over rooms big enough for it
                // to be worth something: under an unchanged census the memo
                // hands back the same Intents and the same shortfall, tile
                // for tile, rather than a plan that merely resembles them.
                Expect.isEmpty
                    (violations (fun case -> not case.RecallsIdentically))
                    "a room whose recalled plan differs from the computed one"
            }
        ]

[<Tests>]
let knownLossTests =
    testList
        "layout losses the sweep found"
        [
            test "a controller in an all-swamp pocket gets no container (#104)" {
                // W12S27's controller sits in a pocket whose 7x7 Upgrade
                // Work Area holds no plain tile at all. The controller
                // container must be an Upgrade tile off the road plan, and
                // every swamp in that area is paved, so there is no
                // candidate: the room plans no buffer, holds one fewer
                // footing target than sources + 2, and records neither.
                let cases = sweep.Value |> List.filter (fun case -> case.Room.Name = "W12S27")

                Expect.isNonEmpty cases "W12S27 is swept"

                Expect.all
                    cases
                    (fun case -> List.length case.Containers = case.SourceCount)
                    "every spawn in W12S27 plans source containers and no controller container"

                Expect.all
                    cases
                    (fun case -> List.isEmpty case.Unserved)
                    "and the loss is invisible: a target that never existed is never unserved"
            }

            test "a sealed spawn doorstep drops a source's trunk whole (#105)" {
                // W12S27 from 6,18: the spawn has exactly two walkable
                // neighbours and the clustered reservation takes one of
                // them, so the source-to-spawn trunk cannot be routed and
                // is dropped in silence. The working-ground exclusion
                // guards Seats and the Upgrade area; nothing guards the
                // spawn's own doorstep. 32,2 is the same mechanism reached
                // from the other side: that tile routed everything until
                // the horizon moved to RCL6 (ADR 0055) and the reservation
                // widened by ten tiles onto its corridor out. The pin is
                // per tile so that whichever of them a fix reaches first
                // says so.
                let sealed' =
                    sweep.Value
                    |> List.filter (fun case -> List.contains case.Spawn case.Room.SealedDoorsteps)

                Expect.isNonEmpty sealed' "the sealed-doorstep case is still in the sweep"

                Expect.all
                    sealed'
                    (trunksCarryEverySource >> not)
                    "the trunk is still dropped; delete the pin and the exclusion when #105 lands"

                // And the drop is no longer silent (#107). The loss is per
                // (source, goal), which this room is the live counterexample
                // for: the source→spawn trunk is dropped and the
                // source→controller trunk is routed and paved, so the record
                // names the spawn alone. A record keyed on the source would
                // be false here, and one that named both goals would claim a
                // haul the colony does in fact have.
                Expect.all
                    sealed'
                    (fun case -> not (List.isEmpty case.Unrouted))
                    "the room says so on the layout record rather than only in its road plan"

                Expect.all
                    sealed'
                    (fun case ->
                        case.Unrouted
                        |> List.forall (fun trunk -> trunk.Goal = TrunkGoal.Spawn case.SpawnId))
                    "and names the spawn alone: the controller's trunk is routed and paved"
            }

            test "W15S28 loses its buffer to the same paved pocket, and RCL is not why (#331)" {
                // #104's mechanism, reached by a room we own and live in.
                // W15S28 stands at RCL5 with no [[upgrade buffer]] and no
                // site for one, so `upgraderQuota` hires nobody (ADR 0046
                // point 3) and the colony climbs on what the worker row
                // carries past the controller.
                //
                // The first thing this pins is what the cause is *not*. ADR
                // 0046 point 3 says in as many words that "the container
                // plan places the buffer under no level gate of its own, so
                // 'no buffer yet' is a fact about the room and not about
                // RCL" — and the room is planned here at the level it stands
                // at and at the level it is trying to reach, which is the
                // experiment that says so rather than the sentence.
                //
                // What the cause *is* comes out of the room's shape and the
                // spawn's together. The controller container must be an
                // Upgrade Work Area tile off the road plan and beside a
                // trunk (ADR 0012), and this area is three quarters swamp.
                // Every swamp in it is paved, which takes those tiles; the
                // trunk stops at the first area tile it reaches, which takes
                // the ones it enters by; and the plain tiles left over stand
                // at the far end of the pocket, beside paving but beside no
                // trunk. So the fold returns `None` — and the two clauses
                // that empty it are asserted apart below, because relaxing
                // either one alone would fill it and they are two different
                // decisions (#104's own "judgement this needs", still
                // unmade).
                //
                // "And the spawn's together" is the sweep's finding and the
                // reason this room joined it: from the other thirty-three
                // tiles swept here the cluster grows elsewhere, the trunk
                // enters the area from another side, and the buffer is
                // planned. The tile below is the one the live colony stands
                // on — read off the live room, as the extractor table's are
                // further down this file, because a capture records a room's
                // fixed furniture and never a base (ADR 0036).
                let capture = load "W15S28"
                let loaded = project capture { X = 18; Y = 30 } None
                let controllerId = Option.get loaded.ControllerId

                let planAt level =
                    let colony = colonyOf loaded level
                    let decision = decide colony Map.empty Set.empty None
                    ofView colony, placementsOf decision.Intents, decision.Memo

                let atLevel = [ 5; 6 ] |> List.map (fun level -> level, planAt level)

                for level, (atlas, placed, memo) in atLevel do
                    let area = workArea atlas (Upgrade controllerId) |> RoomPos.inRoom "W15S28"

                    // The plan's own paving, read off the sites it asks for:
                    // a swept colony starts with no road standing and no road
                    // pending, so this tick's gap is the whole plan. The
                    // trunks are what is left of it once the Work Area's own
                    // swamps are taken out — the only two things the Layout
                    // paves (ADR 0011).
                    let roadPlan = tilesOfKind Road placed |> Set.ofList
                    let swamps = area |> Set.filter (isSwampIn atlas "W15S28")
                    let trunks = Set.difference roadPlan swamps

                    let besideATrunk tile =
                        trunks |> Set.exists (fun t -> range tile t = 1)

                    // The premise, not the expectation: this is a pocket, and
                    // the capture is what says so.
                    Expect.isTrue
                        (Set.count swamps * 2 > Set.count area)
                        $"RCL{level}: the Upgrade Work Area is mostly swamp"

                    // The loss. Both halves of it — no buffer, and one fewer
                    // footing target than sources + 2 (ADR 0022, ADR 0027) —
                    // and the silence, which is the part #104 said should not
                    // survive: a target never constructed is never unserved.
                    Expect.isEmpty
                        (tilesOfKind Container placed
                         |> List.filter (fun tile -> Set.contains tile area))
                        $"RCL{level}: no container site anywhere in the Upgrade Work Area"

                    Expect.isEmpty
                        ((memo.ServedFootings |> List.map (fun footing -> footing.Kind))
                         @ (memo.UnservedFootings |> List.map (fun footing -> footing.Kind))
                         |> List.filter (fun kind -> kind = FootingKind.ControllerContainer))
                        $"RCL{level}: and no controller-container footing, served or recorded"

                    // The cause, stated as the rule the Layout applies: an
                    // area tile off the road plan and beside a trunk.
                    Expect.isEmpty
                        (area
                         |> Set.filter (fun tile ->
                             not (Set.contains tile roadPlan) && besideATrunk tile))
                        $"RCL{level}: the rule's candidate set is empty"

                    // And the two clauses that empty it, each on its own.
                    // Delete this test the day either one is decided — and
                    // decide it, because the room has candidates under both.
                    Expect.isNonEmpty
                        (area
                         |> Set.filter (fun tile ->
                             not (Set.contains tile trunks) && besideATrunk tile))
                        $"RCL{level}: paved swamps beside the trunk, refused for being paved"

                    Expect.isNonEmpty
                        (area
                         |> Set.filter (fun tile ->
                             not (Set.contains tile roadPlan)
                             && roadPlan |> Set.exists (fun t -> range tile t = 1)))
                        $"RCL{level}: plain tiles beside the paving, refused for being off the trunk"

                // And the two levels plan the same room: the level the
                // colony stands at and the level it cannot reach without
                // this buffer differ in the deposit's own container and in
                // nothing about the controller's (ADR 0057 decision 1).
                let record (_, (_, placed, memo)) =
                    tilesOfKind Road placed |> Set.ofList, memo.ServedFootings

                Expect.equal
                    (atLevel |> List.map record |> List.distinct |> List.length)
                    1
                    "the trunks and the footings are the same at RCL5 and at RCL6"

                // And the sweep's half of the same finding, which is what
                // keeps the sentence above honest: the loss is this spawn's
                // and not the room's, so the fixture records it per tile and
                // the day it lands here it lands for the tile that matters.
                let cases = sweep.Value |> List.filter (fun case -> case.Room.Name = "W15S28")

                Expect.isNonEmpty cases "W15S28 is swept"

                Expect.equal
                    (cases
                     |> List.filter (fun case -> List.length case.Containers = case.SourceCount)
                     |> List.map (fun case -> case.Spawn))
                    [ { X = 18; Y = 30 } ]
                    "and the live colony's own tile is the only spawn in the room that loses it"
            }
        ]

// ---- the horizon, re-derived on the room that is about to reach it ------

/// ADR 0055's re-derivation, kept as a test rather than only as prose: the
/// horizon moves to RCL6 **before** W12S28 gets there, and what has to hold
/// on the far side of that move is that the room plans the ten extensions
/// RCL6 unlocks without moving one of the thirty it already stands on.
/// Planned from `12,40`, the tile the live spawn occupies, because a
/// horizon is re-derived on the room it is being moved for (ADR 0039) —
/// which is also why this list sits outside the sweep: the sweep is the
/// general rule over every spawn, and this is the one room's arithmetic.
[<Tests>]
let horizonTests =
    testList
        "the clustered horizon at RCL6"
        [
            test "W12S28 at RCL6 plans forty extensions: the thirty standing, and ten more" {
                let loaded = project (load "W12S28") { X = 12; Y = 40 } None
                let colony = colonyOf loaded 6

                let extensionsOf (view: ColonyView) =
                    decide view Map.empty Set.empty None
                    |> fun decision -> placementsOf decision.Intents |> tilesOfKind Extension

                // The room as ADR 0039's horizon left it: thirty extensions,
                // which is what stands in W12S28 the tick RCL6 lands.
                let thirty = extensionsOf (colony |> atHorizon 5)
                Expect.hasLength thirty 30 "the horizon of five sizes RCL5's whole allowance"

                let forty = extensionsOf colony

                Expect.hasLength
                    forty
                    40
                    "the shipped horizon sizes RCL6's, so an empty room plans all forty at once"

                // The ten the level adds, asked for by a room that has
                // already built the thirty. This is the acceptance criterion
                // and the failure the move exists to prevent: under a horizon
                // of five this same room computes a gap of zero and asks for
                // nothing at all, and the bank stays at 1,800.
                let standing = colony |> withExtensions thirty
                let ten = extensionsOf standing

                Expect.hasLength ten 10 "the ten RCL6 unlocks, and only those"

                Expect.isEmpty
                    (extensionsOf (standing |> atHorizon 5))
                    "the horizon left behind plans none of them: a room that stops growing in silence"

                // And the thirty do not move. A standing structure is a
                // target, so its tile is out of the ordering and its slot off
                // the plan; the ten are picks the ordering had never reached.
                Expect.equal
                    (Set.union (Set.ofList thirty) (Set.ofList ten))
                    (Set.ofList forty)
                    "the thirty standing plus the ten asked for are the forty the horizon planned"
            }

            test "the ten new picks take no working ground and move no trunk" {
                // The question ADR 0039 left for this level: W12S28's north
                // band (rows 35–37) is spoken for by the RCL5 cluster, so does
                // the room still have cluster space for ten more, and does the
                // overflow tread on a Seat or on the Upgrade Work Area (ADR
                // 0022)? It grows a ring out — north to row 34 and east to
                // column 17 — and the working-ground exclusion is what keeps
                // it off the ground the colony stands on. The trunks are the
                // other half of the price: a wider reservation is a router
                // with more tiles to dodge, and on this room it dodges none.
                let loaded = project (load "W12S28") { X = 12; Y = 40 } None
                let colony = colonyOf loaded 6
                let atlas = ofView colony

                let planOf (view: ColonyView) =
                    decide view Map.empty Set.empty None
                    |> fun decision -> placementsOf decision.Intents, decision.Memo

                let placedFive, memoFive = planOf (colony |> atHorizon 5)
                let placedSix, memoSix = planOf colony

                let added =
                    Set.difference
                        (tilesOfKind Extension placedSix |> Set.ofList)
                        (tilesOfKind Extension placedFive |> Set.ofList)

                Expect.hasLength
                    added
                    10
                    "ten tiles the wider horizon reaches and the narrower does not"

                Expect.isEmpty
                    (Set.intersect added (workingGroundIn atlas "W12S28"))
                    "no new pick on a Seat or in the Upgrade Work Area"

                Expect.equal
                    (tilesOfKind Road placedSix |> Set.ofList)
                    (tilesOfKind Road placedFive |> Set.ofList)
                    "the trunks pave the same tiles under both horizons — the same set, not the same length"

                Expect.equal
                    memoSix.ServedFootings
                    memoFive.ServedFootings
                    "and the Link footings sit on the tiles the narrower horizon gave them"

                Expect.isEmpty
                    memoSix.UnservedFootings
                    "no footing target goes unserved at the wider horizon"
            }
        ]

// ---- the extractor and its container, on the rooms that hold a deposit ---

/// ADR 0057 decision 1 on the three rooms it is for. Outside the sweep for
/// the reason the horizon list is: the sweep is the general rule over every
/// spawn at RCL4, and this is three rooms' arithmetic at the level the engine
/// unlocks the extractor at, each planned from the tile its live spawn stands
/// on. The deposit's tile is the capture's own and nothing here invented it;
/// the spawn tiles are **not** in any committed artifact — a capture records a
/// room's fixed furniture and never a base (ADR 0036) — so they are read off
/// the live rooms and are as good as the tick they were read at. What they
/// decide is only where the cluster grows from, which is the same thing the
/// sweep varies on purpose.
[<Tests>]
let extractorTests =
    testList
        "the extractor at RCL6"
        [
            // The three rooms we own that hold a Thorium deposit, each beside
            // the tile its live spawn stands on and the deposit's own
            // coordinates as the capture records them. Read as a table because
            // the rule is one rule: what differs between the three is terrain.
            for room, spawn, deposit in
                [
                    "W12S28", { X = 12; Y = 40 }, { X = 26; Y = 5 }
                    "W13S28", { X = 16; Y = 12 }, { X = 42; Y = 30 }
                    "W15S28", { X = 18; Y = 30 }, { X = 29; Y = 12 }
                ] do
                test $"{room} at RCL6 plans the extractor on its deposit, and at RCL5 plans neither" {
                    let capture = load room
                    let loaded = project capture spawn None

                    let planOf level =
                        decide (colonyOf loaded level) Map.empty Set.empty None
                        |> fun decision -> placementsOf decision.Intents

                    // The capture is the premise, not the expectation: the
                    // room holds exactly one Thorium deposit and it is where
                    // the season mod put it.
                    Expect.equal
                        (capture.Minerals |> List.map snd)
                        [ deposit ]
                        "the capture holds one Thorium deposit, on the tile the live room has it on"

                    let atSix = planOf 6

                    Expect.equal
                        (tilesOfKind Extractor atSix)
                        [ deposit ]
                        "one extractor, on the mineral's own tile"

                    // The container is judged on what a whole room can say
                    // about it: exactly one, on a Seat of the deposit, and on a
                    // tile nothing else in the plan wants. **Which** Seat is
                    // ADR 0057's own sentence — "nearest the Storage's trunk" —
                    // and it is pinned where it can be pinned honestly, in
                    // `LayoutPlanTests` on a fixture built where that reading
                    // and "nearest any trunk" disagree. Re-deriving it here off
                    // `tilesOfKind Road` would only restate whatever metric the
                    // Layout used, and would pass under either.
                    let atlas = ofView (colonyOf loaded 6)
                    let seats = seatTilesOf atlas (List.exactlyOne loaded.MineralIds)

                    let containers =
                        tilesOfKind Container atSix
                        |> List.filter (fun tile -> Set.contains (RoomPos.at room tile) seats)

                    Expect.hasLength containers 1 "one container for the deposit"

                    Expect.isFalse
                        (List.contains (List.exactlyOne containers) (tilesOfKind Road atSix))
                        "and the plan does not pave the tile it seats it on"

                    // ADR 0022's two whole-room invariants, at the level the
                    // sweep does not reach and with the two kinds it has never
                    // seen in the plan. The sweep runs at RCL4 and RCL2 (and
                    // its own spawn stride), so without this the extractor and
                    // the mineral container are in no double-booking check at
                    // all — which is where a footing on the container's tile
                    // and a second container on one tile would both have hidden.
                    let footprints =
                        placementsOf (decide (colonyOf loaded 6) Map.empty Set.empty None).Intents
                        |> List.filter (fun (_, kind) -> kind <> Rampart)
                        |> List.map fst

                    Expect.equal
                        (List.length (List.distinct footprints))
                        (List.length footprints)
                        "no tile is asked for two structures in one tick, the extractor among them"

                    Expect.isEmpty
                        (decide (colonyOf loaded 6) Map.empty Set.empty None).Memo.UnservedFootings
                        "and every Link footing still finds a tile with the deposit's container placed"

                    // The level below, where `CONTROLLER_STRUCTURES.extractor`
                    // is still 0. Not the horizon's business: the deposit sits
                    // on a wall tile off the clustered checkerboard, so there
                    // is no window an extension can take and nothing to hold
                    // open a level early (ADR 0022 against ADR 0057).
                    let atFive = planOf 5

                    Expect.isEmpty
                        (tilesOfKind Extractor atFive)
                        "RCL5 unlocks no extractor, so none is asked for"

                    Expect.isEmpty
                        (tilesOfKind Container atFive
                         |> List.filter (fun tile -> Set.contains (RoomPos.at room tile) seats))
                        "and no container for a deposit no body can dig"
                }

            test "the deposit's ground is off the clustered ordering at every level" {
                // ADR 0057 decision 1's working-ground clause, which is ADR
                // 0022's and is not gated on the level the extractor is: an
                // extension landing on the one accessible tile at a wall mouth
                // would cost the room its whole deposit, and it would land
                // there long before RCL6.
                let capture = load "W12S28"
                let loaded = project capture { X = 12; Y = 40 } None

                for level in [ 4; 6 ] do
                    let colony = colonyOf loaded level
                    let atlas = ofView colony

                    let placed =
                        decide colony Map.empty Set.empty None |> fun d -> placementsOf d.Intents

                    let mineralId = List.exactlyOne loaded.MineralIds

                    let ground =
                        seatTilesOf atlas mineralId
                        |> Set.add (
                            RoomPos.at "W12S28" (List.exactlyOne (List.map snd capture.Minerals))
                        )
                        |> Set.map RoomPos.pos

                    Expect.isTrue
                        (Set.isSubset ground (workingGroundIn atlas "W12S28"))
                        $"the deposit and its Seats are working ground at RCL{level}"

                    Expect.isEmpty
                        (Set.intersect ground (clusteredTiles placed))
                        $"and no clustered pick takes one of them at RCL{level}"
            }
        ]

// ---- the Seam bands the captured rooms hold -----------------------------
