/// The [[quarry]] memory (#663): one record per foreign body by id
/// (`World.Quarries`, a `Track`), folded on every full tick, out of which the
/// world's per-body memories are read. The step memory is its projection
/// (`step`, `World.HostileSteps`).
[<RequireQualifiedAccess>]
module Fabot.Core.Types.Quarry

/// The rooms a body out of sight is kept for while it is not known gone:
/// every home and outpost, each [[sweep]] candidate, and every room whose
/// raid or exit the world remembers this tick (`World.Raiders`,
/// `World.ExitWatches`). Anywhere else, only a trip keeps it.
let watched (colonies: Colony list) (candidates: string list) (world: World) : Set<string> =
    World.preyRooms colonies candidates
    |> Set.union (Colony.homes colonies |> Set.ofList)
    |> Set.union (Map.keys world.Raiders |> Set.ofSeq)
    |> Set.union (Map.keys world.ExitWatches |> Set.ofSeq)

/// The life a body had left when read: a spawning body's reads no number
/// (the engine leaves `ticksToLive` undefined), and it has a whole lifetime
/// ahead of it.
let private lifeOf (body: HostileInfo) : int =
    if body.TicksToLive >= 1 then
        body.TicksToLive
    else
        Engine.creepLifetime

/// A track's step memory (#551): where it stands, the tile of its room it
/// stood on the sighting before when it has stepped off it since, the one
/// before that, the first full tick of its run on its tile, the last full
/// tick it was seen, and its crossings. None once its trip is forgotten.
let step (track: Track) : HostileStep option =
    match track.Trail with
    | [] -> None
    | (seen, at) :: older ->
        let left, back =
            match older with
            | (_, left) :: earlier when left <> at && left.Room = at.Room ->
                Some left,
                (match earlier with
                 | (_, back) :: _ when back <> left && back.Room = left.Room -> Some back
                 | _ -> None)
            | _ -> None, None

        Some
            {
                At = at
                Left = left
                Back = back
                Since =
                    match older with
                    | (first, tile) :: _ when tile = at -> first
                    | _ -> seen
                Seen = seen
                Crossed = track.Crossings
            }

/// A sighting on `at` laid onto a trajectory: a run on one tile keeps its
/// first and its newest sighting.
let private stepOn (tick: int) (at: RoomPos) (steps: (int * RoomPos) list) =
    match steps with
    | (_, last) :: ((_, first) :: _ as earlier) when last = at && first = at ->
        (tick, at) :: earlier
    | _ -> (tick, at) :: steps

/// A trajectory cut to the bounce window, never below the three newest
/// entries `step` reads.
let private ring (window: int) (now: int) (steps: (int * RoomPos) list) =
    match steps with
    | a :: b :: c :: older ->
        a :: b :: c :: List.takeWhile (fun (tick, _) -> now - tick <= window) older
    | _ -> steps

/// This tick's world with every foreign body's track laid under it (#663),
/// and the step memory read off them (`World.HostileSteps`, #551): every
/// body standing in a room this tick, at its tile and as read now; and each
/// out of sight, kept through `window` ticks while last seen within a dance
/// of a border (`Dance.byBorder`) or crossing one lately — a trip, whose
/// steps are kept — or, its trip forgotten, while it was last seen in a
/// `watched` room and was not known gone on the full tick before. Bounded by
/// this tick's bodies, the window and the watched rooms' living bodies, and
/// built by `Fresh`, the map being carried to the next tick.
///
/// Each one's border crossings within `window` ticks are kept beside it
/// (`Track.Crossings`, #658), so a body that steps out over the border into a
/// room nothing sees and comes back is known by the trip.
let recall
    (window: int)
    (watched: Set<string>)
    (previous: Map<string, Track>)
    (world: World)
    : World =
    let recent (crossed: Crossing option) =
        crossed
        |> Option.map (fun c ->
            let ticks = c.Ticks |> List.filter (fun tick -> world.Time - tick <= window)

            { c with
                Ticks = ticks
                Tiles = List.truncate (2 * List.length ticks) c.Tiles
            })
        |> Option.filter (fun c -> not (List.isEmpty c.Ticks))

    // Where a body last seen on an exit tile stood across it: the tile it
    // lands on in the room beyond (`Seam.landingOf`). An exit tile carries
    // a body across, so one seen on it and next in the same room went
    // out and came back, whether anything saw the room beyond or not
    // (the research's rule for a dark border: missing is out).
    let landing (tile: RoomPos) =
        RoomName.adjacent tile.Room
        |> List.tryPick (fun room ->
            RoomName.offsetOf tile.Room room
            |> Option.bind (fun offset -> Seam.landingOf offset (RoomPos.pos tile))
            |> Option.map (RoomPos.at room))

    let present =
        world.Rooms
        |> Map.toSeq
        |> Seq.collect (fun (_, facts) -> facts.Hostiles)
        |> Seq.map (fun h ->
            let track = Map.tryFind h.Id previous
            let before = track |> Option.bind step

            let missedAcross =
                match before with
                // A body the exits carry (never an Invader, nor a
                // keeper), back on the tile it left or beside it.
                | Some step when
                    step.At.Room = h.Pos.Room
                    && step.Seen < world.Time
                    && HostileInfo.crossesExits h
                    && RoomPos.range step.At h.Pos |> Option.exists (fun r -> r <= 1)
                    ->
                    landing step.At |> Option.map (fun far -> step, far)
                | _ -> None

            h.Id,
            {
                Body = h
                Seen = world.Time
                Trail =
                    match track with
                    | Some track when not (List.isEmpty track.Trail) ->
                        stepOn world.Time h.Pos track.Trail |> ring window world.Time
                    | _ -> [ world.Time, h.Pos ]
                Crossings =
                    match before, missedAcross with
                    | _, Some(step, far) ->
                        let kept = recent step.Crossed

                        Some
                            {
                                Came = far
                                Ticks =
                                    world.Time
                                    :: step.Seen + 1
                                    :: (kept |> Option.map _.Ticks |> Option.defaultValue [])
                                Tiles =
                                    h.Pos
                                    :: far
                                    :: far
                                    :: step.At
                                    :: (kept |> Option.map _.Tiles |> Option.defaultValue [])
                            }
                    | Some step, _ when step.At.Room <> h.Pos.Room ->
                        let kept = recent step.Crossed

                        Some
                            {
                                Came = step.At
                                Ticks =
                                    world.Time
                                    :: (kept |> Option.map _.Ticks |> Option.defaultValue [])
                                Tiles =
                                    h.Pos
                                    :: step.At
                                    :: (kept |> Option.map _.Tiles |> Option.defaultValue [])
                            }
                    | Some step, _ -> recent step.Crossed
                    | None, _ -> None
                Gone = None
            })
        |> List.ofSeq

    let seen = present |> List.map fst |> Set.ofList

    let fallen =
        world.Rooms
        |> Map.toSeq
        |> Seq.collect (fun (_, facts) -> facts.Fallen)
        |> Set.ofSeq

    let absent =
        previous
        |> Map.toSeq
        |> Seq.filter (fun (id, _) -> not (seen.Contains id))
        |> Seq.choose (fun (id, track) ->
            let crossed = recent track.Crossings

            // Only a body last seen within a dance of a border, or one that
            // crossed one lately: the rest walked off inland and are no
            // trip (#585: where a stand-down's look finds it gone from).
            let trip =
                not (List.isEmpty track.Trail)
                && world.Time - track.Seen <= window
                && (Option.isSome crossed || Dance.byBorder track.Body.Pos)

            if trip || (track.Gone.IsNone && Set.contains track.Body.Pos.Room watched) then
                Some(
                    id,
                    { track with
                        Trail = if trip then track.Trail else []
                        Crossings = if trip then crossed else None
                        Gone =
                            if track.Gone = Some Gone.Fallen || Set.contains id fallen then
                                Some Gone.Fallen
                            elif track.Seen + lifeOf track.Body <= world.Time then
                                Some Gone.Aged
                            // A body on a trip may only have stepped across:
                            // its room in sight finds it nowhere, not gone.
                            elif
                                not trip
                                && (track.Gone.IsSome || World.seenNow world track.Body.Pos.Room)
                            then
                                Some Gone.SeenGone
                            else
                                None
                    }
                )
            else
                None)
        |> List.ofSeq

    let quarries = present @ absent |> Fresh.mapOfSeq

    { world with
        Quarries = quarries
        HostileSteps =
            quarries
            |> Map.toSeq
            |> Seq.choose (fun (id, track) -> step track |> Option.map (fun step -> id, step))
            |> Fresh.mapOfSeq
    }
