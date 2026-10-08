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
    colonies
    |> List.collect (fun colony -> colony.Outposts |> List.map (fun outpost -> outpost.RoomName))
    |> List.append candidates
    |> List.append (Colony.homes colonies)
    |> Set.ofList
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

/// The rooms a body out of sight may stand in (#663 M1, #585): the room it
/// was last seen in and, while its trip is kept (`Track.Trail`), every room
/// across a border it stood within the dance depth of that the border opens
/// onto (`Dance.beyond`, `opens`), and the room its latest crossing in the
/// window came from (`Track.Crossings`) — less every room seen since it was
/// (`lastSeen`: a room's last sighting), which found it nowhere. Nothing for
/// a body in sight this tick or known gone; nothing once every room it may
/// stand in has been seen without it (`recall` knows it gone then).
let region
    (opens: string -> string -> bool)
    (lastSeen: string -> int option)
    (now: int)
    (track: Track)
    : string list =
    if track.Seen >= now || track.Gone.IsSome then
        []
    else
        let at = track.Body.Pos

        let trip =
            if List.isEmpty track.Trail then
                []
            else
                (Dance.beyond at |> List.filter (opens at.Room))
                @ (track.Crossings |> Option.map _.Came.Room |> Option.toList)

        at.Room :: trip
        |> List.distinct
        |> List.filter (fun room -> lastSeen room |> Option.forall (fun tick -> tick <= track.Seen))

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
/// room nothing sees and comes back is known by the trip. A trip whose every
/// room has been seen since without it is known gone (`region`).
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
        |> Seq.collect (fun (_, facts) ->
            facts.Hostiles |> Seq.map (fun h -> h, facts.HostileCargo))
        |> Seq.map (fun (h, cargo) ->
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
                Cargo = Map.tryFind h.Id cargo |> Option.defaultValue 0
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

            // A trip every room of which vision has answered for since the
            // body was seen, finding it nowhere (`region`): it walked on, out
            // of every room it may stand in.
            let faded =
                trip
                && track.Gone.IsNone
                && List.isEmpty (
                    region
                        (fun room next ->
                            Dance.crosses
                                (World.groundOf world room |> Option.map _.Border)
                                room
                                next)
                        (fun room -> Map.tryFind room world.Sightings |> Option.map _.Tick)
                        world.Time
                        { track with Crossings = crossed }
                )

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
                            elif faded then
                                Some Gone.SeenGone
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

// ---- the queries every reader of a remembered body asks (#663 S2) ---------

/// The tile a body is a room's quarry on (#658, #663): standing in the room,
/// where it stands; standing across its border, out by its latest crossing
/// (`Crossing.Came`) from a tile within the depth a dance turns back at
/// (`Dance.byBorder`) and lingering beside it (`Dance.besideOf`), the tile it
/// crossed out from, so the room keeps it through the ticks it is out; else
/// none. A dancer is the quarry of both rooms it dances between.
let placed (room: string) (at: RoomPos) (crossed: Crossing option) : RoomPos option =
    if at.Room = room then
        Some at
    elif Dance.besideOf room at then
        crossed
        |> Option.map _.Came
        |> Option.filter (fun came -> came.Room = room && Dance.byBorder came)
    else
        None

/// `placed` off a track: its body as last seen and its crossings.
let placedIn (room: string) (track: Track) : RoomPos option =
    placed room track.Body.Pos track.Crossings

/// The index `lastIn` reads, built once a tick per quarry map.
let private byRoom: TickMemo.Memo<Map<string, Track>, Map<string, (Track * RoomPos) list>> =
    TickMemo.create 4 TickMemo.byReference

/// Every track not known gone that is a room's quarry (`placedIn`), with the
/// tile it is that on: the room it was last seen in, and the room it danced
/// out of while it lingers across that border. By id; derived once a tick
/// for every colony that asks (`TickMemo`).
let lastIn (room: string) (quarries: Map<string, Track>) : (Track * RoomPos) list =
    let index =
        TickMemo.recall byRoom quarries (fun quarries ->
            quarries
            |> Map.toList
            |> List.collect (fun (_, track) ->
                if track.Gone.IsSome then
                    []
                else
                    let at = track.Body.Pos

                    (at.Room, (track, at))
                    :: (track.Crossings
                        |> Option.bind (fun crossed -> placedIn crossed.Came.Room track)
                        |> Option.filter (fun tile -> tile.Room <> at.Room)
                        |> Option.map (fun tile -> tile.Room, (track, tile))
                        |> Option.toList))
            |> List.groupBy fst
            |> List.map (fun (room, placings) -> room, List.map snd placings)
            |> Map.ofList)

    Map.tryFind room index |> Option.defaultValue []

/// Whether a body's last step took it nearer its room's way out and the one
/// before not farther (#619): it walks off, off its tile now, the tile it
/// left (`HostileStep.Left`) and the one before (`HostileStep.Back`), each
/// tile's distance out read off `out` — the steps through the room's ground
/// to its nearest walkable exit (`Facts.exitSteps`, #663 S5), so a body
/// walking for the real exit along a walled edge is leaving and one stepping
/// toward a walled edge is not.
let leaving
    (out: RoomPos -> int)
    (at: RoomPos)
    (left: RoomPos option)
    (back: RoomPos option)
    : bool =
    left
    |> Option.exists (fun last ->
        last.Room = at.Room
        && out at < out last
        && back |> Option.forall (fun back -> out last <= out back))

/// Whether a body stepped in its room since the full tick before, and not
/// back onto the tile it stood on before that (a dance in place is its work).
let walking (at: RoomPos) (left: RoomPos option) (back: RoomPos option) : bool =
    left
    |> Option.exists (fun last -> last.Room = at.Room && back |> Option.forall ((<>) at))

/// Whether a body mills in place (#658): it stands on the tile it left, or
/// steps back onto the one it stood on two steps ago.
let inPlace (at: RoomPos) (left: RoomPos option) (back: RoomPos option) : bool =
    left = Some at || back = Some at

/// The share of the ticks a dancer was watched that it stood in `room`
/// (#658), off its crossings: between each crossing and the next it stood on
/// one side, the side it stands on now after the latest (out, where it is
/// placed back on the tile it crossed out from, `placed`). A body watched
/// dancing in place stands in all its time, but on the exit tile itself
/// (`at`), which carries it across, half; one never watched long enough,
/// half.
let inShare (room: string) (now: int) (at: Pos) (crossed: Crossing option) : float =
    match crossed with
    | None when Dance.toEdge at = 0 -> 0.5
    | None -> 1.0
    | Some crossed ->
        let inNow = crossed.Came.Room <> room
        let bounds = now :: crossed.Ticks

        let spans =
            List.pairwise bounds
            |> List.mapi (fun i (later, earlier) -> (i % 2 = 0) = inNow, later - earlier)

        let watched = spans |> List.sumBy snd

        if watched <= 0 then
            0.5
        else
            float (spans |> List.filter fst |> List.sumBy snd) / float watched

/// How often a dancer comes back (#658): its returns over the ticks it was
/// watched in, off its crossings, a return each out and back.
let returnRate (now: int) (crossed: Crossing) : float =
    let returns = float ((List.length crossed.Ticks + 1) / 2)
    let watched = float (max 1 (now - List.last crossed.Ticks + 1))
    returns / watched

/// Whether a tile stands within a step of a target of a kind, off a room's
/// target tiles and the kinds by id: what `rooted` asks its `beside` of.
let besideTarget
    (targets: Map<string, Pos>)
    (kinds: Map<string, TargetKind>)
    (kind: TargetKind)
    (at: Pos)
    : bool =
    targets
    |> Map.exists (fun id tile ->
        Map.tryFind id kinds = Some kind
        && max (abs (tile.X - at.X)) (abs (tile.Y - at.Y)) <= 1)

/// Whether a body is rooted at its work (#550): a digger on its seat (WORK in
/// effect, no weapon, beside a source of its room), or a CLAIM body beside
/// its room's controller; `beside` answers whether it stands within a step
/// of a target of a kind.
let rooted (beside: TargetKind -> bool) (side: HostileInfo list) (body: HostileInfo) : bool =
    HostileInfo.works side body
    && not (HostileInfo.armed side body)
    && beside Source
    || HostileInfo.claims side body && beside Controller

/// How a remembered body's odds are read (#663): what a cast is spent on, or
/// what it is worth where it stands.
[<RequireQualifiedAccess>]
type Reading =
    /// A cast is spent on the body: one not certain counts nothing where a
    /// look that confirms it is coming (a tick or a few of its take, where
    /// a cast spent on a body gone is the whole body).
    | Cast
    /// What the body is worth to a fight already standing: at its odds.
    | Value

/// The odds a body is still in the room it was last seen in, or back in it
/// (#663), the one still-there rule: nothing once known gone; certain while
/// in sight, rooted at its work (`rooted`, asked of it where it was last
/// seen), unable to walk, or on a trip `recall` keeps (a dance across the
/// border: how much of its time it stands in is its catch's, `inShare`);
/// any other falls in a line from certain when last seen to nothing over its
/// round trip (`HostileInfo.roundTrip`). Its age is the body's own, from the last tick it
/// was seen, never a look at its room that found it gone.
let odds (side: HostileInfo list) (rooted: bool) (now: int) (track: Track) : float =
    match track.Gone with
    | Some _ -> 0.0
    | None ->
        let age = now - track.Seen

        if age <= 0 || rooted || not (List.isEmpty track.Trail) then
            1.0
        else
            match HostileInfo.roundTrip side track.Cargo track.Body with
            | None -> 1.0
            | Some trip -> max 0.0 (1.0 - float age / float trip)

/// The bodies a colony keeps in `rooms` that this tick's glance of them does
/// not hold (#663, #618): each track not known gone that is the quarry of one
/// of them (`lastIn`, the room it was last seen in first) — out of sight, or
/// standing in sight across the border of the room it crossed out of — while
/// the odds it is still there (`odds`) are above nothing: laid there at its
/// tile, with the life left it now. A room dark or seen, the room across
/// dark or seen, alike: a body that crossed out of a room of ours stays the
/// room's while its trip lasts, so a Task on it holds until it is known gone
/// or its odds run out. A body standing in one of `rooms` is that room's own
/// sighting, never laid. Never an ally's nor a Source Keeper, nor an armed
/// body, which the raid's own memories keep (`World.Raiders` in a dark room,
/// the exit hold a seen one keeps where it left), nor a healer in a room one
/// of them holds (`raidHeld`): a lone healer is a trespasser as any body is.
/// `rooted` is asked of a body at its tile there; `side` is the bodies its
/// gait is read beside. In id order.
let ghosts
    (rooms: Set<string>)
    (raidHeld: string -> bool)
    (rooted: HostileInfo -> bool)
    (side: HostileInfo list)
    (now: int)
    (quarries: Map<string, Track>)
    : HostileInfo list =
    rooms
    |> Seq.collect (fun room -> lastIn room quarries)
    |> Seq.filter (fun (track, tile) ->
        (track.Seen < now || not (rooms.Contains track.Body.Pos.Room))
        && not (Colony.isAlly track.Body.Owner)
        && not (HostileInfo.isKeeper track.Body)
        && not (HostileInfo.armed [ track.Body ] track.Body)
        && not (HostileInfo.heals [ track.Body ] track.Body && raidHeld tile.Room))
    |> Seq.groupBy (fun (track, _) -> track.Body.Id)
    |> Seq.choose (fun (_, placings) ->
        let track, tile =
            placings |> Seq.sortBy (fun (track, tile) -> tile <> track.Body.Pos) |> Seq.head

        let life = track.Body.TicksToLive - (now - track.Seen)

        if life > 0 && odds side (rooted { track.Body with Pos = tile }) now track > 0.0 then
            Some
                { track.Body with
                    Pos = tile
                    TicksToLive = life
                }
        else
            None)
    |> Seq.sortBy _.Id
    |> List.ofSeq
