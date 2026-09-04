/// Loads the committed room captures (ADR 0036). Real terrain, taken off
/// the season server once by `scripts/capture-room.mjs`, reviewed as text
/// and committed — so the suite can test whole-room geometry without ever
/// touching the API. Real terrain is a counterexample generator here, not
/// a source of expected values: nothing in this module knows a tile the
/// Layout is supposed to pick.
module Fabot.Core.Tests.RoomFixtures

open System
open System.IO
open Fabot.Core.Types

/// One captured room, as its committed fixture spells it.
type RoomCapture =
    {
        RoomName: string
        /// The shard and tick the capture was taken at — provenance, so a
        /// fixture can be re-captured later with confidence. Terrain never
        /// changes; the furniture is what the tick pins.
        Shard: string
        Tick: int
        /// Terrain per tile over x,y in 1..48 — the same window
        /// `Snapshot.terrainOf` projects as ground, with the exit rows
        /// dropped.
        Terrain: Map<Pos, Terrain>
        /// Terrain on the border ring, x or y of 0 or 49 — the rows the
        /// window above drops, delivered beside it and never inside it,
        /// exactly as the shell delivers them (ADR 0041): the Seam's own
        /// terrain, the engine's verbatim, and never a tile to stand on.
        Border: Map<Pos, Terrain>
        /// The room's sources in the capture's order, each under the
        /// readable id the projection knows it by.
        Sources: (string * Pos) list
        /// The controller, when the room has one. A three-source room —
        /// sector centre or Source Keeper room — has none, and cannot be
        /// owned.
        Controller: (string * Pos) option
        /// The same sources, in the same order, under the ids the *engine*
        /// gave them — what the capture actually recorded, before the
        /// rename above made it readable. Beside the readable ids rather
        /// than instead of them, because the two answer different
        /// questions: a test that names a tile reads better with `src-0`,
        /// and a test that has to match an outpost declaration has no
        /// choice at all. An outpost is declared in the engine's ids
        /// (`Outpost`), because that is what a live projection keys every
        /// target by, so these are the ids a Snapshot built to meet one
        /// has to carry (ADR 0041, ADR 0042).
        RealSources: (string * Pos) list
        /// The controller under the engine's own id, as `RealSources` is.
        RealController: (string * Pos) option
    }

/// A captured room projected as a `SpatialInfo`, beside the ids the
/// loader placed on it: a test that hand-wrote its own source list would
/// be green and wrong the day it met a room with a different count.
type LoadedRoom =
    {
        Spatial: SpatialInfo
        SourceIds: string list
        ControllerId: string option
    }

let private roomSide = 50

/// Fixtures are copied beside the test assembly, so they are found the
/// same way whatever the runner's working directory happens to be.
let private roomsDirectory = Path.Combine(AppContext.BaseDirectory, "rooms")

/// The engine's own terrain mask, classified into the Core's three states
/// exactly as `Snapshot.terrainAt` classifies it — wall bit first, then
/// swamp. These two must agree or the fixture describes a room the bot
/// never sees, which is #75's argument one level up.
let private terrainOfMask mask =
    if mask &&& 1 <> 0 then Wall
    elif mask &&& 2 <> 0 then Swamp
    else Plain

/// The committed capture of one room, by name.
let load (roomName: string) : RoomCapture =
    let path = Path.Combine(roomsDirectory, roomName + ".room")

    if not (File.Exists path) then
        failwithf "no room capture at %s — run `npm run capture-room -- %s`" path roomName

    let lines = File.ReadAllLines path

    let sectionAt marker =
        match Array.tryFindIndex ((=) marker) lines with
        | Some index -> index
        | None -> failwithf "%s has no %s section" path marker

    let terrainSection = sectionAt "[terrain]"
    let objectSection = sectionAt "[objects]"

    let header =
        lines.[.. terrainSection - 1]
        |> Array.filter (fun line -> not (line.StartsWith "#") && line.Trim() <> "")
        |> Array.map (fun line ->
            match line.Split '\t' with
            | [| key; value |] -> key, value
            | _ -> failwithf "%s: header line %s is not key<TAB>value" path line)
        |> Map.ofArray

    let field name =
        match Map.tryFind name header with
        | Some value -> value
        | None -> failwithf "%s: header carries no %s" path name

    let rows = lines.[terrainSection + 1 .. terrainSection + roomSide]

    if
        rows.Length <> roomSide
        || rows |> Array.exists (fun row -> row.Length <> roomSide)
    then
        failwithf "%s: terrain is not %d rows of %d characters" path roomSide roomSide

    // The file holds the room verbatim so a re-capture diffs cleanly, and
    // these are the lines that split it the way the shell splits it: the
    // window the projection stands on, and the border ring beside it that
    // only the Seam reads (ADR 0036, ADR 0041). Rows are the capture's own
    // row-major order — `rows.[y].[x]`.
    let terrainAt x y =
        terrainOfMask (int rows.[y].[x] - int '0')

    let terrain =
        Map.ofList
            [
                for y in 1 .. roomSide - 2 do
                    for x in 1 .. roomSide - 2 -> { X = x; Y = y }, terrainAt x y
            ]

    let border =
        Map.ofList
            [
                for y in 0 .. roomSide - 1 do
                    for x in 0 .. roomSide - 1 do
                        if x = 0 || x = roomSide - 1 || y = 0 || y = roomSide - 1 then
                            { X = x; Y = y }, terrainAt x y
            ]

    let objectRows =
        lines.[objectSection + 1 ..] |> Array.filter (fun line -> line.Trim() <> "")

    match Array.tryHead objectRows with
    | Some "id\ttype\tx\ty" -> ()
    | _ -> failwithf "%s: the objects section has no id/type/x/y column header" path

    // The capture keeps the API's real ids, which is what makes it
    // traceable; the projection's keys are the test's own vocabulary — and
    // since an outpost is declared in the engine's ids, both are carried
    // out rather than one being thrown away here.
    // A mineral row is read and ignored: a mineral has no TargetKind to
    // enter the projection through.
    let objects =
        objectRows
        |> Array.skip 1
        |> Array.map (fun line ->
            match line.Split '\t' with
            | [| objectId; kind; x; y |] -> kind, (objectId, { X = int x; Y = int y })
            | _ -> failwithf "%s: object row %s is not id/type/x/y" path line)

    let ofKind kind =
        objects |> Array.filter (fst >> (=) kind) |> Array.map snd |> List.ofArray

    let sources = ofKind "source"
    let controller = ofKind "controller" |> List.tryHead

    {
        RoomName = field "room"
        Shard = field "shard"
        Tick = int (field "tick")
        Terrain = terrain
        Border = border
        Sources = sources |> List.mapi (fun index (_, pos) -> $"src-{index}", pos)
        Controller = controller |> Option.map (fun (_, pos) -> "ctrl", pos)
        RealSources = sources
        RealController = controller
    }

/// The captured room with a spawn standing on it. The spawn is always the
/// test's — a real room's spawn is somebody else's base — and
/// `fallbackController` supplies a tile only for a room the capture found
/// none in, which is the only way a three-source room plans anything:
/// without a projected controller position the Upgrade Work Area is empty,
/// the trunks route to the spawn alone, and the footing count is wrong for
/// reasons that are not the rule's.
let project (capture: RoomCapture) (spawn: Pos) (fallbackController: Pos option) : LoadedRoom =
    let controllerTarget =
        capture.Controller
        |> Option.orElse (fallbackController |> Option.map (fun pos -> "ctrl", pos))

    let targets =
        [
            yield "spawn-1", spawn, Structure BuiltKind.Spawn

            for id, pos in capture.Sources do
                yield id, pos, Source

            for id, pos in Option.toList controllerTarget do
                yield id, pos, Controller
        ]

    {
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some capture.RoomName
                // The captured room's geometry under the captured room's
                // name, which is the shape `buildSpatial` produces and the
                // only shape there is (ADR 0041).
                Rooms =
                    Map.ofList
                        [
                            capture.RoomName,
                            { RoomLayer.empty with
                                Terrain = capture.Terrain
                                TargetPositions =
                                    targets |> List.map (fun (id, pos, _) -> id, pos) |> Map.ofList
                                // The obstacle rule is the shell's, read off
                                // the Core's own table rather than restated:
                                // a structure a creep cannot stand on blocks
                                // its tile, and so does the controller.
                                Obstacles =
                                    targets
                                    |> List.choose (fun (_, pos, kind) ->
                                        match kind with
                                        | Structure built when not (isWalkable built) -> Some pos
                                        | Controller -> Some pos
                                        | _ -> None)
                                    |> Set.ofList
                            }
                        ]
                // Beside the ground, never inside it, exactly as the shell
                // delivers it (ADR 0041): one room's ring under its own
                // name, which is the shape `buildSpatial` produces. One
                // capture is one room, so this projection answers no band
                // by itself — a Seam joins two rooms, and a projection that
                // can answer one is composed by merging two captures'
                // rings, as `RoomInvariantTests.acrossFrom` does.
                Borders = Map.ofList [ capture.RoomName, capture.Border ]
                TargetKinds = targets |> List.map (fun (id, _, kind) -> id, kind) |> Map.ofList
            }
        SourceIds = capture.Sources |> List.map fst
        ControllerId = controllerTarget |> Option.map fst
    }
