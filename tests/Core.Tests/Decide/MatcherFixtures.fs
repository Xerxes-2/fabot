/// The Matcher and the Resolver: which Tasks a body may hold, the travel cost
/// that ranks them (ADR 0002), the yield arbitration that settles a contested
/// tile (ADR 0001), the Verdicts a match and a release are returned under (ADR
/// 0009), the verbose list an operator reads (ADR 0018), and the Intents the
/// whole decision emits.
/// The Matcher suite's fixtures: the corridors, crowds and casts the
/// matching and arbitration below are run over.
module Fabot.Core.Tests.Decide.MatcherFixtures

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

/// The live `9W/9C/9M` generalist of #235: neither Work-heavy (ADR 0016's
/// ratio is strict) nor a standing body (ADR 0046's is too), so it answers
/// every one of the Harvest gate's light-body clauses and no exemption. Its
/// store is the caller's, because the whole of the first clause is what the
/// store holds. Named for the [[body class]] and not "commuter", which this
/// file already spends on `Capacity.Commuters` — the crowd that is merely not
/// Heavy, the Standing row included.
let lightWorker name energy freeCapacity =
    creepWith
        name
        energy
        freeCapacity
        ([ for _ in 1..9 -> Work ]
         @ [ for _ in 1..9 -> Carry ]
         @ [ for _ in 1..9 -> Move ])

/// A three-row field y = 9..11, x = 8..15, with one source walled into the
/// middle of it at (10,10), and nothing else in the world to do: the Harvest
/// is the whole pool, so an unmatched body here was refused by the gate and
/// not outranked. Three rows so the rock is walked *around* — a body on one
/// side of a one-wide lane can reach no Seat on the other, and a Seat the
/// light body cannot reach would answer this suite's questions with ADR 0002's
/// reachability instead of the applicability it is asking about. The source
/// arrives through `withTargets` and so carries its **kind**, which the Seat
/// union is read through (ADR 0041): a projection that placed the rock and
/// did not say what it was answers no Seats, and so no Post, whatever stands
/// on them.
let loneSourceRoom =
    spatial
        []
        [
            for x in 8..15 do
                for y in 9..11 -> { X = x; Y = y }, (if (x, y) = (10, 10) then Wall else Plain)
        ]
    |> withTargets [ "src-a", { X = 10; Y = 10 }, Source ]

/// The same field with a container standing on the Seat at (11,10), which
/// makes that Seat a [[post]] (ADR 0042) and leaves the rock's seven other
/// Seats as the light body's Work Area (ADR 0051). Stocked with nothing, so
/// the container pools no Withdraw of its own and the pool stays the one
/// Harvest — what refuses a body here is the gate and never a rival.
let postedSourceRoom =
    loneSourceRoom
    |> withTargets [ "can-a", { X = 11; Y = 10 }, Structure BuiltKind.Container ]

/// The same field one step earlier: the container on (11,10) is still a
/// construction site. #205 makes that Seat a [[post]] all the same — the
/// Anchor hired for it is the body that raises it — but ADR 0042's split
/// keeps it out of the economy until the structure stands, which is the
/// half `Decide.isPosted` reads and the half #235's spare-rate clause reads
/// with it.
let siteSourceRoom =
    loneSourceRoom
    |> withTargets [ "can-a", { X = 11; Y = 10 }, Site BuiltKind.Container ]

/// The colony over one of those rooms: the named bodies on the named tiles,
/// an owned home room — so the rock is priced at the held ten a tick
/// (ADR 0042) — and no controller, refillable with room or store to pool a
/// second Task.
let sourceColony room (bodies: (CreepInfo * Pos) list) =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Controller = None
        Refillables = []
        Creeps = bodies |> List.map fst
        Spatial =
            room
            |> withHome (fun layer ->
                { layer with
                    CreepPositions =
                        bodies |> List.map (fun (body, pos) -> body.Name, pos) |> Map.ofList
                })
    }

/// Spatial projection of a plain corridor x = 10, y = 9..21 with a source
/// at each end (source tiles are walls): "src-far" at (10, 10), "src-near"
/// at (10, 20).
let nearFarCorridor creepPositions =
    spatial
        [ "src-far", { X = 10; Y = 10 }; "src-near", { X = 10; Y = 20 } ]
        [
            for y in 9..21 -> { X = 10; Y = y }, (if y = 10 || y = 20 then Wall else Plain)
        ]
    |> withHome (fun layer ->
        { layer with
            CreepPositions = Map.ofList creepPositions
        })

/// The Resolver's movement Verdicts at the same seam, with the named
/// creeps on the verbose list (ADR 0018).
let resolveVerdictsVerboseOn snapshot assigned verbose =
    resolve
        snapshot
        (Atlas.ofView snapshot)
        noThreats
        (poolOn snapshot)
        (Map.ofList assigned)
        Map.empty
        (Set.ofList verbose)
    |> snd

/// The same for a quiet colony: nobody on the verbose list.
let resolveVerdictsOn snapshot assigned =
    resolveVerdictsVerboseOn snapshot assigned []

/// Run the Emitter at its own seam, over the same tick-start Atlas.
let emitOn snapshot assigned =
    emit snapshot (Atlas.ofView snapshot) noThreats (Map.ofList assigned)

/// Two single-Seat sources at the ends of a two-tile corridor; each creep
/// stands on the other's Seat.
let headOnSwap =
    let terrain =
        [
            { X = 10; Y = 10 }, Wall
            { X = 10; Y = 11 }, Plain
            { X = 10; Y = 12 }, Plain
            { X = 10; Y = 13 }, Wall
        ]

    { bareRespawn with
        Sources = [ source "src-a"; source "src-b" ]
        Creeps = [ worker "wa" 0 50; worker "wb" 0 50 ]
        Spatial =

            spatial [ "src-a", { X = 10; Y = 10 }; "src-b", { X = 10; Y = 13 } ] terrain
            |> withHome (fun layer ->
                { layer with
                    CreepPositions =
                        Map.ofList [ "wa", { X = 10; Y = 12 }; "wb", { X = 10; Y = 11 } ]
                })
    }

/// The lane with an east-bound body of ours on (11,12) and whatever holds
/// (12,12) in front of it — a body of ours, or a body of another colony's,
/// which is the pair #220 turns on.
let laneWith pocket ours foreign =
    { bareRespawn with
        Sources = [ source "src-w"; source "src-e" ]
        Controller = None
        Creeps = worker "eb" 0 50 :: ours
        Spatial =
            lane pocket
            |> withHome (fun layer ->
                { layer with
                    CreepPositions =
                        Map.ofList (
                            ("eb", { X = 11; Y = 12 })
                            :: (ours |> List.map (fun c -> c.Name, { X = 12; Y = 12 }))
                        )
                })
        Foreign =
            if foreign then
                Set.singleton (RoomPos.at "W1N1" { X = 12; Y = 12 })
            else
                Set.empty
    }

/// W13S28's north Upgrade pocket (#241), the geometry the jam is made of,
/// narrowed to six tiles and written either way round. The controller stands
/// at (24,17) with the whole row y = 16 walled, so the only ground inside its
/// Upgrade Work Area is the pocket north of it — (21..23,14) and (21..23,15) —
/// reached down one corridor along y = 14. The live pocket is the eight tiles
/// the ticket lists, (21..25,14) and (21..23,15); the two east tiles are left
/// out here so the corridor is ordinary ground and the pocket's mouth is one
/// tile.
///
/// `mirror` is which way that corridor runs, and it is not decoration: every
/// tie in this bot falls to the lowest x then y, so the two orientations put
/// the working ground on opposite sides of the order `arbitrate` re-houses a
/// displaced body in. One of them can be right by accident, which is why both
/// are pinned below.
///
/// The [[buffer]] container stands at (22,15), *inside* the pocket, so its
/// five standing tiles are Upgrade [[working ground]] to the last one: nowhere
/// here is a tile a body can park on without taking it from the row that works
/// there or from the hauler that feeds them. The buffer is empty, which is
/// what leaves the upgraders with no Task at all.
let internal pocketFacing (mirror: int -> int) =
    let controller = { X = mirror 24; Y = 17 }
    let buffer = { X = mirror 22; Y = 15 }

    { spatial
          []
          ([ for x in 16..23 -> { X = mirror x; Y = 14 }, Plain ]
           @ [ for x in 21..23 -> { X = mirror x; Y = 15 }, Plain ]
           @ [ controller, Wall ]) with
        Stores = Map.ofList [ "can-buf", 0 ]
    }
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.singleton controller
        })
    |> withTargets
        [
            "ctrl-1", controller, Controller
            "can-buf", buffer, Structure BuiltKind.Container
        ]

/// The live room's orientation: the corridor runs west out of the mouth.
let pocketRoom = pocketFacing id

/// And the same pocket reflected about x = 20 — mouth at (20,14), corridor
/// running east.
let mirroredPocketRoom = pocketFacing (fun x -> 40 - x)

/// The pocket colony: the bodies the test puts in it, standing where it puts
/// them. No source, so the only work in the room is the controller's and its
/// buffer's.
let pocketColonyIn room creeps positions =
    { bareRespawn with
        Sources = []
        Creeps = creeps
        Spatial =
            room
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList positions
                })
    }

let pocketColony creeps positions =
    pocketColonyIn pocketRoom creeps positions

/// The pairs that exchanged tiles between two ticks. A swap is a legal answer
/// to a head-on meeting; a swap repeated is the livelock #241 forbids, and
/// what `livelock-scan` counted 54 of in 190 ticks.
let internal swaps (before: Map<string, Pos>) (after: Map<string, Pos>) =
    Set.ofList
        [
            for KeyValue(a, _) in before do
                for KeyValue(b, _) in before do
                    if a < b && before[a] = after[b] && before[b] = after[a] then
                        a, b
        ]

/// The pairs that exchanged tiles on two consecutive tick boundaries of a run.
let repeatedSwaps (ticks: Map<string, Pos> list) =
    ticks
    |> List.pairwise
    |> List.map (fun (before, after) -> swaps before after)
    |> List.pairwise
    |> List.collect (fun (first, second) -> Set.intersect first second |> Set.toList)

/// The positions a colony's bodies settle into, tick by tick, each tick's move
/// Intents folded onto the tick before it.
let walkedTicks colony assigned count (start: Map<string, Pos>) =
    let step (positions: Map<string, Pos>) =
        (positions, resolveOn (colony (Map.toList positions)) assigned |> moveIntents)
        ||> List.fold (fun acc (name, direction) -> Map.add name (stepFrom acc[name] direction) acc)

    List.scan (fun positions _ -> step positions) start [ 1..count ]

/// An upgrader-shaped body: the row that stands beside the buffer, and the one
/// idling in the pocket in #241.
let upgrader name =
    creepWith name 0 50 [ Work; Work; Carry; Move ]

/// The [[storage]] tucked against a wall (#268): the stock at (10,10) with
/// wall on every side but two — (10,11) and (11,11) — and a corridor running
/// east from them along y = 11. Those two tiles are the whole of the ground
/// its [[refill]] can be made from, and they are outside ADR 0022's [[working
/// ground]] to the last one: the room holds no source and no controller, so
/// #241's set is empty here and whatever vacates them is the mover's own rule.
/// The stock's own tile is an obstacle, exactly as the engine has it, so it is
/// no third standing tile.
let internal wallStorageRoom =
    { spatial
          []
          ([ { X = 10; Y = 10 }, Plain; { X = 10; Y = 11 }, Plain ]
           @ [ for x in 11..18 -> { X = x; Y = 11 }, Plain ]) with
        Stores = Map.ofList [ "sto-1", 0 ]
    }
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.singleton { X = 10; Y = 10 }
        })
    |> withTargets [ "sto-1", { X = 10; Y = 10 }, Structure BuiltKind.Storage ]

/// The colony standing on it: no source, no controller and no placed spawn, so
/// the only Task the room offers is the stock's own Refill.
let wallStorageColony creeps positions =
    { bareRespawn with
        Sources = []
        Controller = None
        Creeps = creeps
        Spatial =
            wallStorageRoom
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList positions
                })
    }

/// The tier colony with the given hunger: one loaded Carry-only body
/// standing on the buffer, so the deepest tier costs it nothing to reach
/// and every shallower one costs more. Whatever wins, wins against travel
/// cost, and only rank can do that.
let tierColony refillables =
    { bareRespawn with
        Sources = []
        Refillables = refillables
        Creeps = [ creepWith "h1" 100 0 [ Carry; Carry; Move ] ]
        Spatial =
            tierRoom
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList [ "h1", { X = 18; Y = 10 } ]
                })
    }

/// The surplus fixture: one loaded generalist and a hungry tower, in a
/// colony the projection places nothing in — unpriceable geometry never
/// counts against a Task (ADR 0004), so every candidate ties on travel
/// cost and load, and rank is the only thing left that can separate a
/// pair. Each caller adds exactly one rival, so the Verdict's factor is
/// evidence about that rival alone.
let surplusColony =
    { bareRespawn with
        Sources = []
        Controller = None
        Refillables = [ refillable "tower-1" 500 BuiltKind.Tower ]
        Creeps = [ worker "w1" 50 0 ]
    }
