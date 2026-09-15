/// The Layout's invariants on real terrain, the losses the sweep found, and
/// the derived clustered horizon, `controller.Level + 1` (ADR 0063) — which
/// sizes the *placement* alone since ADR 0064, the reservation the trunks
/// route around being sized at `allowanceOf`'s ceiling and reading no level,
/// so the road a level-up used to abandon is an invariant here rather than a
/// loss.
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

            test "only paved-buffer fallbacks leave their controller Link footing unserved" {
                // The #77 detector, and the invariant ADR 0035 made cheap:
                // an unserved target is a guarantee the colony no longer
                // has. #331 accepts exactly one kind: a controller buffer
                // kept in a fully paved pocket at the cost of its Link.
                let cases = sweep.Value

                let accepted (case: Case) =
                    usesPavedBuffer case.Room.PavedBuffer case.Spawn

                let isControllerShortfall (case: Case) =
                    match case.Unserved with
                    | [ footing ] -> footing.Kind = FootingKind.ControllerContainer
                    | _ -> false

                Expect.isEmpty
                    (cases
                     |> List.filter (fun case ->
                         not (List.isEmpty case.Unserved) && not (accepted case))
                     |> List.map describe)
                    "no other room or spawn trades a footing for its buffer"

                Expect.equal
                    (cases |> List.filter accepted |> List.forall isControllerShortfall)
                    true
                    "every accepted case records exactly its controller-container footing"
            }

            test "the footing target count is the rule's, never a constant" {
                // The targets are the container picks plus the Storage
                // (ADR 0022, ADR 0027), so sources + 2 is arithmetic the
                // room does rather than a number written down — one
                // container per source, one for the controller, one
                // Storage. A three-source room holds five.
                let miscounts (case: Case) =
                    let containers = case.SourceCount + 1

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
                    clusteredTiles case.Placed
                    |> Set.exists (fun tile -> Set.contains tile case.WorkingGround)

                Expect.isEmpty
                    (violations onWorkingGround)
                    "a room clustering onto ground the colony works from"
            }

            test "a maxed room's whole cluster is inside the reservation its trunks dodged" {
                // The rule ADR 0064 rests on and nothing pinned: the
                // reservation is never narrower than the placement, so the
                // tiles the cluster draws from at any level are tiles the
                // trunk router already routed around. A reservation narrower
                // than the placement plants a tower or an extension on a tile
                // the same plan paves — which is the *decisive* argument
                // against the rejected constant-6 rule (164 such collisions
                // across 118 cases at RCL7 and RCL8), and which nothing in
                // this suite would have caught it doing.
                //
                // Read at RCL8, because that is where the placement is widest
                // and the slack thinnest: the reservation carries one spare
                // tile per Link footing (ADR 0027), `sources + 2` of them,
                // and that slack hides a narrowing of up to three tower slots
                // on every case here. What it does not hide is four — and
                // these two spawns are the counterexamples a probe over all
                // 171 found, which is what real terrain is for (ADR 0036).
                // Pinned as two named cases rather than swept: the sweep's
                // budget is plans per case and this would add a sixth level
                // to all 171 for two answers.
                let collisionsAt roomName spawn =
                    let room = rooms |> List.find (fun room -> room.Name = roomName)
                    let loaded = project (load roomName) spawn room.FallbackController

                    let placed =
                        decide (colonyOf loaded 8) Map.empty Set.empty None
                        |> fun decision -> placementsOf decision.Intents

                    Set.intersect (tilesOfKind Road placed |> Set.ofList) (clusteredTiles placed)

                Expect.isEmpty
                    (collisionsAt "W13S28" { X = 18; Y = 12 })
                    "W13S28 beside its live spawn: no clustered tile on a tile the same plan paves"

                Expect.isEmpty
                    (collisionsAt "W15S25" { X = 24; Y = 6 })
                    "and the other tile a narrowed reservation plants a structure on the road at"
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
                    tilesOfKind Road case.Placed
                    |> List.exists (fun tile -> not (Set.contains tile case.Walkable))

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

            test "a level-up asks for exactly what the level unlocks, less what stands" {
                // #341's own invariant, generalised off the three rooms the
                // ticket named and onto every spawn of every capture. A room
                // built out at the sweep's level and then levelled asks, per
                // kind, for the next level's whole allowance minus the census
                // already standing — which is what `gapAt` computes and what a
                // horizon left behind the room destroys: sized at 6 and
                // filtered at 7, W12S28's extension gap was `40 − 40 = 0` and
                // the room asked for none of the ten RCL7 unlocked.
                //
                // This replaces `a level never takes a clustered tile back`,
                // which asserted that a level-up plans nothing onto a standing
                // structure's tile. That was true and said nothing: it did not
                // red when the horizon was reverted to a constant, nor with
                // the rampart exclusion it carried removed, because
                // `withBuilt` makes a standing tile an obstacle, the ordering
                // drops occupied tiles and the router routes around obstacles
                // — three mechanisms pinned elsewhere, and between them
                // nothing in this sweep can ever plan onto one. The claim
                // below is not implied by them: it fails the moment the
                // horizon falls below the room's own level, at any spawn.
                //
                // Read against the **engine's** table and not against a second
                // plan of the same room: a horizon that is wrong in the same
                // direction at both levels satisfies "the levelled plan is the
                // bare one less what stands" perfectly, and a room that asks
                // for ten of the twenty extensions RCL5 allows is still a room
                // that has stopped growing. The numbers below are
                // `allowanceOf`'s RCL4 and RCL5 rows — 20 and 30 extensions,
                // 1 and 2 towers — which is the engine's arithmetic and not
                // terrain's, so ADR 0036's ban on expected values off real
                // captures is not touched: no tile is named.
                let entitled = [ Tower, 1, 2 - 1; StructureKind.Extension, 20, 30 - 20 ]

                let shortchanged (case: Case) = case.LevelUpAsks <> entitled

                Expect.isEmpty
                    (violations shortchanged)
                    "a room that levels and asks for less than the level unlocked it"
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
            test "an all-swamp pocket keeps its buffer and records the footing it trades (#331)" {
                // W12S27's controller sits in a pocket whose 7x7 Upgrade
                // Work Area holds no plain tile at all. The paved fallback
                // preserves the controller buffer from every spawn; since
                // its neighbours are paving, its Link footing is the
                // deliberately recorded remainder of #104's loss.
                let cases = sweep.Value |> List.filter (fun case -> case.Room.Name = "W12S27")

                Expect.isNonEmpty cases "W12S27 is swept"

                Expect.all
                    cases
                    (fun case -> List.length case.Containers = case.SourceCount + 1)
                    "every spawn in W12S27 plans its source and controller containers"

                Expect.all
                    cases
                    (fun case ->
                        match case.Unserved with
                        | [ footing ] -> footing.Kind = FootingKind.ControllerContainer
                        | _ -> false)
                    "and records exactly the controller Link footing it cannot reserve"
            }

            test "a sealed spawn doorstep drops a source's trunk whole (#105)" {
                // W12S27 from 6,18: the spawn has exactly two walkable
                // neighbours and the clustered reservation takes one of
                // them, so the source-to-spawn trunk cannot be routed and
                // is dropped in silence. The working-ground exclusion
                // guards Seats and the Upgrade area; nothing guards the
                // spawn's own doorstep. 32,2 is the same mechanism reached
                // from the other side, and since ADR 0064 it is reached at
                // every level again: the reservation is sized at
                // `allowanceOf`'s ceiling and reads no level, so the tile is
                // back in `SealedDoorsteps` beside 6,18 and the sweep's own
                // RCL4 sees it. The test below this one holds it level by
                // level from 3 to 8.
                //
                // This pins **accepted behaviour** and not a pending fix.
                // #105 is closed (2026-09-08) and so is the recording half it
                // was split into (#107, shipped): triage measured the doorstep
                // exclusion, found it moves the live colony's Storage and five
                // hand-built fixtures and amends ADR 0011 and ADR 0022, and
                // judged it a decision rather than a repair — then closed the
                // ticket without taking it. So there is no landing to wait
                // for. The pins stay per tile because the loss is per tile:
                // whichever of them a future rule reaches first says so by
                // going red.
                let sealed' =
                    sweep.Value
                    |> List.filter (fun case -> List.contains case.Spawn case.Room.SealedDoorsteps)

                Expect.isNonEmpty sealed' "the sealed-doorstep case is still in the sweep"

                Expect.all
                    sealed'
                    (trunksCarryEverySource >> not)
                    "the trunk is still dropped — the exclusion records that, and comes out with the rule that fixes it"

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

            test "the doorstep 32,2 is sealed at every level, and no level-up pays for it (#105)" {
                // The same loss as above, reached from the other side, and a
                // function of the terrain alone again. ADR 0055 widened every
                // room's reservation to RCL6's forty tiles whatever level it
                // stood at, which closed this spawn's corridor out and put the
                // tile in `SealedDoorsteps` beside 6,18. ADR 0063 derived the
                // horizon and the corridor started opening at the low levels:
                // the room paved its way out at RCL4 and walked away from the
                // pavement on the tick it reached RCL5 — 95 tiles down to 35,
                // 60 orphaned, the worst churn the sweep found anywhere.
                // ADR 0064 sizes the reservation at `allowanceOf`'s ceiling
                // and stops it reading the level at all, so the corridor is
                // closed at RCL3 exactly as at RCL8 and the 95 is never laid.
                //
                // This is the ticket's one measured regression stated as a
                // test: the tile seals two levels earlier than it did
                // yesterday, and what it buys is that nothing is bought and
                // abandoned. No colony stands on `32,2`, and the set of spawns
                // that ever drop a trunk is unchanged under all three rules.
                //
                // Pinned rather than deleted because the mechanism stands
                // even though the ticket does not: the working-ground
                // exclusion guards Seats and the Upgrade area, and nothing
                // guards the spawn's own doorstep. #105 was closed on
                // 2026-09-08 having measured the exclusion and judged it a
                // decision rather than a fix, and #107 — the recording half it
                // was split into — shipped, which is why the loss below is
                // read off `UnroutedTrunks` rather than off a hole in the road
                // plan. Nothing is going to land here; this is the record.
                let loaded = project (load "W12S27") { X = 32; Y = 2 } None

                let unroutedAt level =
                    decide (colonyOf loaded level) Map.empty Set.empty None
                    |> fun decision -> decision.Memo.UnroutedTrunks

                for level in 3..8 do
                    Expect.all
                        (unroutedAt level)
                        (fun trunk -> trunk.Goal = TrunkGoal.Spawn "spawn-1")
                        $"at RCL{level} the loss is the spawn trunk's alone, as it is from 6,18"

                    Expect.isNonEmpty
                        (unroutedAt level)
                        $"at RCL{level} the reservation seals the doorstep, and the room routes out of it nowhere"

                // And what it costs in pavement, which is the number this
                // pin carried while the loss was level-dependent and is kept
                // here restated rather than dropped: the 95 tiles RCL4 used
                // to lay and the 60 the level-up used to orphan are both
                // gone, because the plan is the sealed one from the start.
                let pavedAt level =
                    decide (colonyOf loaded level) Map.empty Set.empty None
                    |> fun decision ->
                        placementsOf decision.Intents |> tilesOfKind Road |> Set.ofList

                for level in 3..8 do
                    Expect.equal
                        (Set.count (pavedAt level))
                        35
                        $"RCL{level} plans the sealed room's 35 tiles: the 95 is never laid"

                Expect.isEmpty
                    (Set.difference (pavedAt 4) (pavedAt 5))
                    "and the RCL4 → RCL5 level-up that orphaned 60 tiles orphans none"
            }

            test
                "W15S28 falls back to a paved buffer, and records the Link footing it trades (#331)" {
                // #104's mechanism, reached by the live spawn in W15S28.
                // Its strict set is empty at both levels: plain ground lies
                // beside paving but no trunk, while the usable swamp is
                // paved. #331 keeps the growth-enabling buffer by falling
                // back to that swamp, then records the Link it cannot fit.
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

                    let afterRoads =
                        colonyOf loaded level
                        |> withRoadsStanding "W15S28" roadPlan
                        |> fun colony -> decide colony Map.empty Set.empty None
                        |> fun decision -> placementsOf decision.Intents

                    let besideATrunk tile =
                        trunks |> Set.exists (fun t -> range tile t = 1)

                    // The premise, not the expectation: this is a pocket, and
                    // the capture is what says so.
                    Expect.isTrue
                        (Set.count swamps * 2 > Set.count area)
                        $"RCL{level}: the Upgrade Work Area is mostly swamp"

                    // The road site wins the tile on the first tick; once that
                    // road stands the container plan below asks for the buffer.
                    Expect.isEmpty
                        (tilesOfKind Container placed
                         |> List.filter (fun tile -> Set.contains tile area))
                        $"RCL{level}: the fallback container defers to this tick's road site"

                    Expect.equal
                        (tilesOfKind Container afterRoads
                         |> List.filter (fun tile -> Set.contains tile area))
                        [ { X = 23; Y = 29 } ]
                        $"RCL{level}: once the road stands the fallback buffer is requested"

                    Expect.equal
                        (memo.UnservedFootings
                         |> List.filter (fun footing ->
                             footing.Kind = FootingKind.ControllerContainer)
                         |> List.map (fun footing -> RoomPos.pos footing.Target))
                        [ { X = 23; Y = 29 } ]
                        $"RCL{level}: the fallback target records the Link footing it cannot reserve"

                    // The strict set is empty: an area tile off the whole
                    // road plan and beside a trunk.
                    Expect.isEmpty
                        (area
                         |> Set.filter (fun tile ->
                             not (Set.contains tile roadPlan) && besideATrunk tile))
                        $"RCL{level}: the rule's candidate set is empty"

                    // The fallback set differs only by allowing paved swamp.
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

                // The decision is independent of RCL: the footings and road
                // reservation are identical at the standing and target levels.
                let footings (_, (_, _, memo)) = memo.ServedFootings

                Expect.equal
                    (atLevel |> List.map footings |> List.distinct |> List.length)
                    1
                    "the footings are the same at RCL5 and at RCL6"

                let paved (_, (_, placed, _)) = tilesOfKind Road placed |> Set.ofList

                Expect.equal
                    (atLevel |> List.map (paved >> Set.count) |> List.distinct |> List.length)
                    1
                    "and the wider reservation costs the trunks a detour, not a longer road"

                // The general fallback gives every swept spawn a buffer.
                let cases = sweep.Value |> List.filter (fun case -> case.Room.Name = "W15S28")

                Expect.isNonEmpty cases "W15S28 is swept"

                Expect.isEmpty
                    (cases
                     |> List.filter (fun case -> List.length case.Containers = case.SourceCount)
                     |> List.map (fun case -> case.Spawn))
                    "the paved fallback gives every swept spawn its controller buffer"
            }

            test "a level-up abandons no paved road, and moves no container's pick (#344)" {
                // The invariant ADR 0064 restores, standing where ADR 0063's
                // recorded loss stood. A reservation that is a function of
                // the level is a road plan that is a function of the level:
                // the window widens on the tick the room levels, the router
                // re-routes around it, and the tiles the old route paved are
                // tiles nothing plans any more — 589 of them over this sweep,
                // some 176,700 energy, plus ten source-container picks moved
                // out from under a standing container. `Facts.hungryStructures`
                // walks every standing structure with no reference to the road
                // plan, so an orphan is not written off once: it stays in the
                // [[repair]] pool and draws upkeep for as long as it stands
                // (#342).
                //
                // Sized at `allowanceOf`'s ceiling the reservation reads no
                // level, so a bare room's road plan is identical at every
                // level **by construction** and a level-up cannot orphan a
                // road. That is ADR 0027's determinism invariant, which
                // ADR 0039 raised against a derived horizon and ADR 0063
                // priced rather than met, holding for the roads again.
                //
                // Stated as a rule and not as a ratchet, which is the whole
                // difference: a bound of "no more than 152 tiles" is green on
                // a change that churns 151, and by construction the number
                // here is zero. Measured on **real terrain** and at the
                // levels the live colonies stand at, because the openRoom
                // ladder in `LayoutPlacementTests` runs on featureless ground
                // where a trunk barely exists.
                let plannedFrom roomName spawn level =
                    let room = rooms |> List.find (fun room -> room.Name = roomName)
                    let loaded = project (load roomName) spawn room.FallbackController
                    loaded, decide (colonyOf loaded level) Map.empty Set.empty None

                let paved placed = tilesOfKind Road placed |> Set.ofList

                // A source container is planned onto the [[seat]] nearest its
                // trunk, and on the first tick it defers to the road site it
                // shares ground with (ADR 0040) — so the picks are read off
                // each plan with its own roads already standing, which is the
                // state the level-up actually finds the room in.
                let picksOf roomName colony placed =
                    decide
                        (colony |> withRoadsStanding roomName (paved placed))
                        Map.empty
                        Set.empty
                        None
                    |> fun decision ->
                        placementsOf decision.Intents |> tilesOfKind Container |> Set.ofList

                let levelUp roomName spawn level =
                    let loaded, before = plannedFrom roomName spawn level
                    let placed = placementsOf before.Intents
                    let levelled = colonyOf loaded (level + 1) |> withBuilt (standingCluster placed)
                    let after = decide levelled Map.empty Set.empty None
                    let placedAfter = placementsOf after.Intents

                    (placed, picksOf roomName (colonyOf loaded level) placed),
                    (placedAfter, picksOf roomName levelled placedAfter)

                // W13S28 from `36,42`, RCL6 to RCL7 — the transition W12S28
                // made on the day #341 was filed, in a room the colony owns,
                // and the largest churn ADR 0063 found in one: 111 paved
                // tiles became 92, 34 abandoned, some 10,200 energy, and a
                // source container's pick moved with the trunk that chose it.
                // The room now plans the 92 from the start and the level-up
                // is free.
                //
                // `36,42` is one of the sweep's **stride** tiles and not the
                // spawn W13S28 stands on: this room carries no `AlsoSweep`
                // entry, unlike W12S28 and W15S28, so no test here plans it
                // from its live tile and no claim about the live colony rests
                // on this number (#345).
                let (before, picksBefore), (after, picksAfter) =
                    levelUp "W13S28" { X = 36; Y = 42 } 6

                Expect.equal
                    (Set.count (paved before))
                    92
                    "RCL6 paves from 36,42 the 92 tiles RCL7 wants, not the 111 it used to"

                Expect.equal
                    (paved before)
                    (paved after)
                    "and the levelled room plans the same set: not a superset, the same tiles"

                Expect.equal
                    picksBefore
                    picksAfter
                    "no source container's pick moves out from under the container that stands on it"

                // W15S28 from `18,30`, the live spawn, RCL5 to RCL6: the
                // colony's own next level-up, which ADR 0063 priced at a
                // four-tile detour and which is now nothing at all.
                let (before, _), (after, _) = levelUp "W15S28" { X = 18; Y = 30 } 5

                Expect.equal
                    (paved before)
                    (paved after)
                    "W15S28's own next level-up abandons nothing and lays nothing"

                // Two more transitions, chosen because the sweep cannot reach
                // them. The sweep plans RCL4 → RCL5 and one transition is all
                // it can afford (ADR 0036: the honest lever is fewer plans per
                // case, not more levels), so a reservation re-coupled to the
                // level at a band the sweep never crosses would leave every
                // assertion above green. The tower half is exactly that hole:
                // `allowanceOf` moves the tower allowance at 5 and again at 7
                // and 8, so a tower reservation read off the horizon churns at
                // RCL6 → 7 and not at RCL4 → 5. These are the two worst cases
                // it produces — twenty tiles and six, and the six carry a
                // source container's pick off `16,44` with them (#344 review).
                let (before, picksBefore), (after, picksAfter) =
                    levelUp "W15S28" { X = 18; Y = 12 } 6

                Expect.equal
                    (paved before)
                    (paved after)
                    "an RCL6 → 7 level-up abandons nothing either: the tower allowance moves there, the reservation does not"

                Expect.equal picksBefore picksAfter "and no pick moves across it"

                let (before, picksBefore), (after, picksAfter) =
                    levelUp "W12S27" { X = 18; Y = 30 } 6

                Expect.equal
                    (paved before)
                    (paved after)
                    "nor in the room whose corridors are narrow enough to re-route on one reserved tile"

                Expect.equal picksBefore picksAfter "and `16,44` keeps the container standing on it"

                // And the sweep's half, over every (room, spawn) the suite
                // plans, built out at RCL4 and levelled to RCL5. An
                // invariant and not a ratchet: zero, stated as zero.
                Expect.isEmpty
                    (sweep.Value |> List.collect (fun case -> case.LevelUpAbandons))
                    "no spawn of any capture abandons a paved tile when its room levels"

                Expect.isEmpty
                    (violations (fun case -> not (List.isEmpty case.LevelUpAbandons)))
                    "and the claim is the whole sweep's, named case by case when it breaks"
            }
        ]

// ---- the horizon, re-derived on the room that is about to reach it ------

/// ADR 0063's re-derivation, kept as a test rather than only as prose. The
/// horizon is no longer a constant a human moves before the room arrives —
/// it is `controller.Level + 1`, so it arrives *with* the room — and what
/// has to hold on the far side of that move is what had to hold on the far
/// side of ADR 0039's and ADR 0055's: the room asks for everything the new
/// level unlocks, and moves nothing it already stands on.
///
/// Re-derived on W12S28 at RCL7, which is the live room #341 was found on
/// and the widest **placement** window this change opens anywhere —
/// `allowanceOf` answers anything above 7 with sixty extensions and six
/// towers, so an RCL7 room's horizon of 8 draws from sixty and six where it
/// drew from forty and two. The *reservation* is that same sixty and six at
/// every level and was never the thing widening here (ADR 0064). Planned from `12,40`, the tile the live spawn occupies, because a
/// horizon is re-derived on the room it is being moved for (ADR 0039) —
/// which is also why this list sits outside the sweep: the sweep is the
/// general rule over every spawn, and this is the one room's arithmetic.
[<Tests>]
let horizonTests =
    testList
        "the derived clustered horizon"
        [
            test "W12S28 at RCL7 asks for the ten extensions and the third tower the level unlocks" {
                let loaded = project (load "W12S28") { X = 12; Y = 40 } None

                let planOf (view: ColonyView) =
                    decide view Map.empty Set.empty None
                    |> fun decision -> placementsOf decision.Intents

                // The room as the shipped-yesterday constant left it: RCL6
                // under a horizon of 6, which is a lookahead of none — forty
                // extensions and two towers, the census #341 measured standing
                // in W12S28 live at t427,931.
                let shipped = planOf (colonyOf loaded 6 |> atLookahead 0)
                let forty = tilesOfKind Extension shipped
                let two = tilesOfKind Tower shipped

                Expect.hasLength forty 40 "RCL6 under no lookahead is the forty the live room built"
                Expect.hasLength two 2 "and the two towers standing beside them"

                // That room, levelled. Nothing about the colony moves but the
                // controller's own level, which is the whole of what the
                // derived horizon reads.
                let standing =
                    (forty |> List.map (fun tile -> tile, BuiltKind.Extension))
                    @ (two |> List.map (fun tile -> tile, BuiltKind.Tower))

                let levelled = colonyOf loaded 7 |> withBuilt standing
                let placed = planOf levelled

                Expect.hasLength
                    (tilesOfKind Extension placed)
                    10
                    "the ten RCL7 unlocks, asked for on the tick the level lands"

                Expect.hasLength (tilesOfKind Tower placed) 1 "and the third tower RCL7 unlocks"

                // #341 itself, reproduced by the only setting that can still
                // produce it: a lookahead of −1 is the stale absolute horizon
                // of 6 met by an RCL7 room, and it plans nothing at all.
                let stale = planOf (levelled |> atLookahead -1)

                Expect.isEmpty
                    (tilesOfKind Extension stale)
                    "sized a level behind, the room asks for none of the ten — #341, on the room it was found on"

                Expect.isEmpty (tilesOfKind Tower stale) "and none of the third tower either"

                // And nothing standing moves. A standing structure is a
                // target, so its tile is out of the ordering entirely; this is
                // the claim the ticket asked to be measured rather than
                // assumed, and it is measured against every kind the Layout
                // places and not the clustered ones alone.
                // A **rampart** is the one kind that may share a standing
                // structure's tile, and is meant to: ADR 0034 covers every
                // Keep structure with one. Every other kind sharing a tile
                // would be the plan eating the colony's own buildings.
                let standingTiles = standing |> List.map fst |> Set.ofList

                Expect.isEmpty
                    (placed
                     |> List.filter (fun (tile, kind) ->
                         kind <> Rampart && Set.contains tile standingTiles))
                    "no tile a structure already stands on is planned for anything else"
            }

            test "the ten new picks take no working ground and move no trunk" {
                // ADR 0055 asked this of the ten RCL6 added and ADR 0063 asks
                // it again of RCL7's, on the widest window the derivation ever
                // opens: does the overflow tread on a Seat or on the Upgrade
                // Work Area (ADR 0022), and what does a router with sixty
                // reserved tiles to dodge instead of forty pave? It grows a
                // ring further out — row 33 and column 18 — the
                // working-ground exclusion keeps it off the ground the colony
                // stands on, and the trunks do not move at all.
                let loaded = project (load "W12S28") { X = 12; Y = 40 } None
                let colony = colonyOf loaded 7
                let atlas = ofView colony

                let planOf (view: ColonyView) =
                    decide view Map.empty Set.empty None
                    |> fun decision -> placementsOf decision.Intents, decision.Memo

                let stalePlaced, staleMemo = planOf (colony |> atLookahead -1)
                let placed, memo = planOf colony

                let added =
                    Set.difference
                        (tilesOfKind Extension placed |> Set.ofList)
                        (tilesOfKind Extension stalePlaced |> Set.ofList)

                Expect.isEmpty
                    (Set.intersect added (workingGroundIn atlas "W12S28"))
                    "no new pick on a Seat or in the Upgrade Work Area"

                Expect.equal
                    (tilesOfKind Road placed |> Set.ofList)
                    (tilesOfKind Road stalePlaced |> Set.ofList)
                    "the trunks pave the same tiles under both horizons — the same set, not the same length"

                Expect.equal
                    memo.ServedFootings
                    staleMemo.ServedFootings
                    "and the Link footings sit on the tiles the narrower horizon gave them"

                Expect.isEmpty
                    memo.UnservedFootings
                    "no footing target goes unserved at the wider horizon"
            }

            // No captured room asks for less than its own level allows, at
            // any level any of them can stand at. This is #341 stated as a
            // property rather than as one room's arithmetic, and it is the
            // one an absolute constant could never satisfy: at RCL7 under a
            // horizon of 6 W12S28 asked for nothing, and at RCL8 under a
            // horizon of 7 it would have asked for nothing again. Every level
            // from the first that allows an extension to the last, over all
            // three rooms the colony stands in.
            for room, spawn in
                [
                    "W12S28", { X = 12; Y = 40 }
                    "W13S28", { X = 16; Y = 12 }
                    "W15S28", { X = 18; Y = 30 }
                ] do
                test $"{room} asks for its whole allowance at every level it can stand at" {
                    let loaded = project (load room) spawn None

                    let placedAt level =
                        decide (colonyOf loaded level) Map.empty Set.empty None
                        |> fun decision -> placementsOf decision.Intents

                    // The engine's own table, from RCL2 where the first
                    // extension is unlocked to RCL8 where the last is. Written
                    // out rather than read off `allowanceOf`, which is the
                    // private function under test: a second derivation, not
                    // the same one twice (ADR 0035).
                    for level, extensions, towers in
                        [ 2, 5, 0; 3, 10, 1; 4, 20, 1; 5, 30, 2; 6, 40, 2; 7, 50, 3; 8, 60, 6 ] do
                        let placed = placedAt level

                        Expect.hasLength
                            (tilesOfKind Extension placed)
                            extensions
                            $"RCL{level} allows {extensions} extensions and the room asks for all of them"

                        Expect.hasLength
                            (tilesOfKind Tower placed)
                            towers
                            $"RCL{level} allows {towers} towers and the room asks for all of them"
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

                    let unserved =
                        (decide (colonyOf loaded 6) Map.empty Set.empty None).Memo.UnservedFootings

                    let fixture = rooms |> List.find (fun fixture -> fixture.Name = room)

                    if usesPavedBuffer fixture.PavedBuffer spawn then
                        Expect.all
                            unserved
                            (fun footing -> footing.Kind = FootingKind.ControllerContainer)
                            "the live paved buffer trades only its own Link footing"

                        Expect.hasLength
                            unserved
                            1
                            "the accepted paved-buffer shortfall is recorded once"
                    else
                        Expect.isEmpty unserved "every Link footing finds a tile"

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
