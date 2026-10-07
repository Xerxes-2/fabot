/// The Resolver: arbitrated movement (ADR-0001). Prices each mover's step,
/// matches steps to tiles so two bodies never claim one, and returns the move
/// Intents with the Verdicts explaining them.
[<AutoOpen>]
module Fabot.Core.Decide.Resolver

open Fabot.Core
open Fabot.Core.Types

/// Creeps with no Task rank below every task in arbitration.
let private idleRank = System.Int32.MaxValue

/// A kiting body's walk (#510, movement S3, #615 U1).
[<RequireQualifiedAccess>]
type private Kiting =
    /// No class it stands off nor camp in the room it stands in: the plain
    /// walk, closing to its reach.
    | Clear
    /// By tile index, what its walk keeps off (`Threats.kiteUnsafeFor`), and
    /// the soonest a class shorter than its reach strikes each tile, which a
    /// tie breaks toward the later.
    /// With the kite ground's approach band (`Laid.Approach`), lazily.
    | Keeps of unsafe: (int -> bool) * later: (int -> int) * approach: Lazy<Set<RoomPos>>

/// One target a fighter strikes next, as its chase reads it.
type private Aimed =
    {
        /// The target's tile, in the body's room.
        Target: Pos
        /// The steps the target can take by a tick, its fatigue waited out, at
        /// its quickest pace off swamp: its likely next steps, away.
        Steps: int -> int
        /// The tile of the room it left since the full tick before
        /// (`ColonyView.Stepped`): which way it walks.
        Left: Pos option
        /// Whether it bears no weapon (`HostileInfo.armed`): a quarry, not
        /// the kite's.
        Unarmed: bool
    }

/// What a fighter strikes next (`Emitter.aimOf`), as its chase reads it.
type private Aim =
    {
        /// Its kill order's head, then every other target in its reach.
        Targets: Aimed list
        /// How far the body strikes.
        Reach: int
    }

/// How far from a tile the nearest of the targets stands.
let private gapOf (aim: Aim) (tile: Pos) =
    aim.Targets
    |> List.fold (fun gap aimed -> min gap (range tile aimed.Target)) System.Int32.MaxValue

/// A fighter's chase: what its step is weighed by (`chaseStep`), each part
/// read only where the one before it asks.
type private Chase =
    {
        /// Whether a step onto a tile holds the body there past the tick
        /// after, by its gait and the tile's weight.
        Holds: Pos -> bool
        /// What it aims at in its room; None for nothing.
        Aim: Lazy<Aim option>
        /// The body's own arrival over its room (`Arrival.ofBody`: its legs,
        /// load, parts shot off and fatigue).
        Walk: Lazy<Arrival.BodyArrival option>
    }

/// `chaseStep`'s reading, off the body's own arrival: by tile, the ticks the
/// body is held on it past the tick it stands on it, and how far from it the
/// nearest target can stand by the tick the body could step off it.
let private readingOf (aim: Aim) (walk: Arrival.BodyArrival) =
    let leaves tile = Grid.at (indexOf tile) walk.Leaves

    let held tile =
        leaves tile - Grid.at (indexOf tile) walk.Ticks

    let apart tile =
        let leaves = leaves tile

        if leaves = System.Int32.MaxValue then
            System.Int32.MaxValue
        else
            aim.Targets
            |> List.fold
                (fun apart aimed -> min apart (range tile aimed.Target + aimed.Steps leaves))
                System.Int32.MaxValue

    held, apart

/// Whether a chaser's step from one tile onto the next loses its targets
/// (`readingOf`): it holds the body past the tick after, and every target is
/// past the bar (its reach, or the nearest gap from where it steps) by the
/// tick the body could step off it.
let private loses (aim: Aim) (walk: Arrival.BodyArrival) (from: Pos) (wanted: Pos) =
    let held, apart = readingOf aim walk
    held wanted > 0 && apart wanted > max aim.Reach (gapOf aim from)

/// `chaseStep`'s weighing: the step wanted is kept unless it `loses` the
/// targets; else the tile that keeps one within the bar, held less and
/// struck no sooner. None to keep it; Some None to stand.
let private weighed
    (aim: Aim)
    (walk: Arrival.BodyArrival)
    (pos: Pos)
    (wanted: Pos)
    (beside: Pos list)
    (later: (int -> int) option)
    : Pos option option =
    let gap = gapOf aim
    let held, apart = readingOf aim walk
    let bar = max aim.Reach (gap pos)

    let struck tile =
        later |> Option.map (fun soonest -> soonest (indexOf tile))

    if loses aim walk pos wanted then
        (if gap pos <= aim.Reach then pos :: beside else beside)
        |> List.filter (fun tile ->
            held tile < held wanted && apart tile <= bar && struck tile >= struck wanted)
        |> List.sortBy (fun tile -> apart tile, gap tile, (if tile = pos then 0 else 1), tile)
        |> List.tryHead
        |> Option.map (fun tile -> if tile = pos then None else Some tile)
    else
        None

/// A chaser's step, its own gait and its targets' next steps counted: a body
/// aiming at a target in its room (its kill order's head, or any other in its
/// reach), however far off, never takes a step that holds it on a tile
/// (swamp, or a lamed body's plain) from which every such target, walking
/// away, is farther than its reach, or than the nearest stands now, by the
/// tick it could step again (#614: a melee three off stepped onto swamp),
/// where it can step instead onto a tile held less that keeps one within that
/// by then, and no sooner struck than the step it was given, or stand where
/// it strikes one now. By the tick a body can next step off a tile it stands
/// that far from where the nearest target can have walked; ties to the
/// nearer, then to standing. None to keep the step it was given; Some None to
/// stand. One rule at any reach (#615 U1): a body closes to its reach, and
/// at a reach of one its glue onto the target's tile is this step
/// (`glueChases`); a body that stands off its target kites instead
/// (`kiteStep`), and its step is weighed here too.
let private chaseStep
    (chase: Chase)
    (at: RoomPos)
    (beside: Pos list)
    (later: (int -> int) option)
    (step: RoomPos option)
    : RoomPos option option =
    match step with
    | Some next when next.Room = at.Room && chase.Holds(RoomPos.pos next) ->
        match chase.Aim.Value, chase.Walk.Value with
        | Some aim, Some walk when walk.Room = at.Room ->
            weighed aim walk (RoomPos.pos at) (RoomPos.pos next) beside later
            |> Option.map (Option.map (RoomPos.at at.Room))
        | _ -> None
    | _ -> None

/// How much of a chaser's step plan (its step, then the light ticks' tiles)
/// the light ticks walk: up to the first tile past its step that holds the
/// body and that the full tick, standing on the tile before it, would trade
/// for another (`weighed`), so a light tick replays no step the full tick
/// would refuse (#614); a losing step with nothing better beside it is kept,
/// as the full tick keeps it. None to keep it whole.
let private chasePlan
    atlas
    (chase: Chase)
    (at: RoomPos)
    (later: (int -> int) option)
    (plan: RoomPos list)
    : int option =
    match plan with
    | step :: ahead when
        ahead
        |> List.exists (fun tile -> tile.Room = at.Room && chase.Holds(RoomPos.pos tile))
        ->
        match chase.Aim.Value, chase.Walk.Value with
        | Some aim, Some walk when walk.Room = at.Room ->
            (step :: ahead)
            |> List.pairwise
            |> List.tryFindIndex (fun (from, tile) ->
                let from = RoomPos.pos from

                tile.Room = at.Room
                && chase.Holds(RoomPos.pos tile)
                && weighed
                    aim
                    walk
                    from
                    (RoomPos.pos tile)
                    (Atlas.adjacentWalkableIn atlas at.Room from)
                    later
                   |> Option.isSome)
            |> Option.map ((+) 1)
        | _ -> None
    | _ -> None

/// An armed body's chase (`chaseStep`) where it stands: each target's steps
/// by its gait off swamp, its fatigue waited out, and the body's own arrival.
/// None for a body with no weapon.
let private chaseFor
    (view: ColonyView)
    atlas
    (threats: Threats)
    (assigned: Map<string, Task>)
    (creep: CreepInfo)
    (at: RoomPos)
    : Chase option =
    if partCount creep.Body Attack > 0 || partCount creep.Body RangedAttack > 0 then
        let gait = Arrival.gaitOf creep

        let aimed (side: HostileInfo list) (target: HostileInfo) =
            let gait = HostileInfo.gait side target

            let wait =
                Map.tryFind at.Room view.World.Rooms
                |> Option.bind (fun seen -> Map.tryFind target.Id seen.HostileFatigue)
                |> Option.defaultValue 0
                |> fun fatigue -> Arrival.waitOf fatigue gait

            {
                Target = RoomPos.pos target.Pos
                Steps =
                    match Arrival.paceOf gait with
                    | Some pace ->
                        let quickest = min pace.Road pace.Plain

                        fun ticks ->
                            if ticks <= wait then
                                0
                            else
                                (ticks - wait + quickest - 1) / quickest
                    | None -> fun _ -> 0
                Left =
                    Map.tryFind target.Id view.Stepped
                    |> Option.filter (fun left -> left.Room = at.Room)
                    |> Option.map RoomPos.pos
                Unarmed = not (HostileInfo.armed side target)
            }

        let aim () =
            Emitter.aimOf view atlas threats assigned creep
            |> Option.map (fun (targets, reach) ->
                let side = view.Hostiles |> List.filter (fun h -> h.Pos.Room = at.Room)

                {
                    Targets = targets |> List.map (aimed side)
                    Reach = reach
                })

        Some
            {
                Holds = Emitter.holds atlas at.Room gait
                Aim = lazy (aim ())
                Walk = lazy (Threats.bodyWalkOf threats atlas creep)
            }
    else
        None

/// Whether a melee body's glue from its tile onto its target's
/// (`Emitter.glue`) is a step its chase keeps (`chaseStep`, #614): one that
/// holds it while its target walks off is refused, and it stands.
let glueChases (view: ColonyView) atlas (threats: Threats) (assigned: Map<string, Task>) =
    fun (creep: CreepInfo) (from: RoomPos) (target: RoomPos) ->
        match chaseFor view atlas threats assigned creep from with
        | Some chase ->
            let beside = Atlas.adjacentWalkableIn atlas from.Room (RoomPos.pos from)

            chaseStep chase from beside (Threats.soonestIn threats from.Room) (Some target)
            |> Option.isNone
        | None -> true

/// A chase's walk over the cadence window and the quarry's track it assumes
/// (#626), so a light tick walks it on while the quarry walks as assumed and
/// re-decides it only off that. `quarry`: where the quarry stands by a tick
/// of `horizon` (tick 0 now), walking on in its heading. `decided`: the
/// decide's own walk from `from` (its step, then its step plan's tiles; none
/// to stand), timed by `held` (the ticks a tile holds the body past its
/// own). Laid only where the decide pursued the quarry — its step no farther
/// from where the quarry stands, or a stand within `reach` of it — else
/// nothing. With `extend` (a walk no chase weighing cut short), the walk goes
/// on past its end, a tile a tick its gait allows, onto the `opens` tile
/// beside it nearest where the quarry stands on arrival, never nearer than
/// the decide's own walk left it (`keep`, the gap at its end) and only while
/// that closes the gap the quarry opened; within the window's last light
/// tick. The track: the quarry's tile for each tick from the first, while a
/// stand keeps it in `reach`, or a walk within the gap its first tick
/// leaves. The walk is the decide's then the extension; the track the
/// quarry's tiles, tick 1 first.
let chainOf
    (horizon: int)
    (reach: int)
    (around: Pos -> Pos list)
    (opens: Pos -> bool)
    (held: Pos -> int)
    (quarry: int -> Pos)
    (from: Pos)
    (decided: Pos list)
    (extend: bool)
    : Pos list * Pos list =
    let start = quarry 0

    let pursued =
        match decided with
        | [] -> range from start <= reach
        | first :: _ -> range first start <= range from start

    if not pursued then
        [], []
    else
        let path = ResizeArray<Pos>()
        let times = ResizeArray<int>()
        let mutable tick = held from + 1

        for tile in decided do
            path.Add tile
            times.Add tick
            tick <- tick + held tile + 1

        let last () =
            if path.Count = 0 then from else path.[path.Count - 1]

        let keep = range (last ()) start
        let mutable walking = extend && path.Count > 0

        while walking do
            let here = last ()
            let arrives = times.[times.Count - 1] + held here + 1

            if arrives > horizon - 1 then
                walking <- false
            else
                let toward = quarry arrives
                let gap = range here toward

                let next =
                    if gap <= keep then
                        None
                    else
                        around here
                        |> List.filter (fun tile ->
                            opens tile && range tile toward >= keep && range tile toward < gap)
                        |> List.sortBy (fun tile ->
                            let dx, dy = tile.X - toward.X, tile.Y - toward.Y
                            range tile toward, held tile, dx * dx + dy * dy, tile.X, tile.Y)
                        |> List.tryHead

                match next with
                | Some tile ->
                    path.Add tile
                    times.Add arrives
                | None -> walking <- false

        let standing t =
            let mutable at = from

            for i in 0 .. path.Count - 1 do
                if times.[i] <= t then
                    at <- path.[i]

            at

        // A stand holds while the quarry stays in reach; a walk while it
        // keeps the gap its first tick leaves (a tick behind a quarry at its
        // pace), which is the gap a decide each tick would keep.
        let limit =
            if path.Count = 0 then
                reach
            else
                max reach (range (standing 1) (quarry 1))

        let track =
            Seq.initInfinite ((+) 1)
            |> Seq.takeWhile (fun t -> t <= horizon - 1 && range (standing t) (quarry t) <= limit)
            |> Seq.map quarry
            |> List.ofSeq

        List.ofSeq path, track

/// A chaser's walk and track (`chainOf`) for a body whose chase aims its kill
/// order's head at an unarmed quarry that walked since the full tick before
/// (`Aimed.Left`): the walk `step` (or a stand) began, priced as the full
/// tick prices it — never a tile a step onto holds the body on
/// (`Chase.Holds`), any a hostile strikes within the cadence window
/// (`later`), off its arrival or on the border ring — and timed by its own
/// arrival (`Chase.Walk`).
let private layChain
    atlas
    (creep: string)
    (chase: Chase)
    (later: (int -> int) option)
    (at: RoomPos)
    (step: RoomPos option)
    (extend: bool)
    =
    match chase.Aim.Value, chase.Walk.Value with
    | Some({ Targets = head :: _ } as aim), Some walk when head.Unarmed && walk.Room = at.Room ->
        match head.Left with
        | Some left when left <> head.Target ->
            let dx, dy = sign (head.Target.X - left.X), sign (head.Target.Y - left.Y)
            let clamp v = max 0 (min (Engine.roomSide - 1) v)

            let quarry ticks =
                let steps = head.Steps ticks

                {
                    X = clamp (head.Target.X + steps * dx)
                    Y = clamp (head.Target.Y + steps * dy)
                }

            let leaves tile = Grid.at (indexOf tile) walk.Leaves
            let held, _ = readingOf aim walk
            let from = RoomPos.pos at

            let decided =
                match step with
                | Some next when next.Room = at.Room && RoomPos.pos next <> from ->
                    match Atlas.stepPlanOf atlas creep with
                    | Some(first :: _ as plan) when
                        first = next && plan |> List.forall (fun tile -> tile.Room = at.Room)
                        ->
                        plan |> List.map RoomPos.pos
                    | _ -> [ RoomPos.pos next ]
                | _ -> []

            let opens tile =
                not (Seam.onRing tile)
                && leaves tile <> System.Int32.MaxValue
                && not (chase.Holds tile)
                && later
                   |> Option.forall (fun soonest -> soonest (indexOf tile) > Tuning.maxCadence)

            let path, track =
                chainOf
                    Tuning.maxCadence
                    aim.Reach
                    (Atlas.adjacentWalkableIn atlas at.Room)
                    opens
                    (fun tile -> if leaves tile = System.Int32.MaxValue then 0 else held tile)
                    quarry
                    from
                    decided
                    extend

            if not (List.isEmpty track) then
                Atlas.recordChain
                    atlas
                    creep
                    (if List.length path > List.length decided then
                         Some(path |> List.map (RoomPos.at at.Room))
                     else
                         None)
                    (track |> List.map (RoomPos.at at.Room))
        | _ -> ()
    | _ -> ()

/// A kiter's step toward its ground (#510, #524, movement S3): the first step
/// of a walk that keeps off what it keeps off, across the border too, a tie
/// going to the tile a melee strikes later. Where no such walk reaches the
/// ground, the same walk to the band's safe tiles nearest it (#595). Where none
/// closes on it: none, standing where it stands while that is safe; else the step
/// beside it a melee strikes last, where that is later than here.
let private kiteStep
    atlas
    (creep: string)
    (at: RoomPos)
    (unsafe: int -> bool)
    (later: int -> int)
    (approach: Lazy<Set<RoomPos>>)
    (area: Set<RoomPos>)
    : RoomPos option =
    let room = at.Room
    let pos = RoomPos.pos at
    let goals = Set.remove at area

    // Where no safe walk reaches the ground, it closes on it all the same
    // (#595): to the band's safe tiles a tile off it, then two, then three,
    // while those lie nearer than it stands, so it waits a step from the
    // ground for the melee's window and never parks at the edge it came in by.
    // The band is the ground's own tests one to three past it: never an exit,
    // never a pocket the ground was denied for want of escape room.
    let closer () =
        let ground = goals |> Seq.map RoomPos.pos |> List.ofSeq

        let gap =
            ground
            |> List.map (fun tile -> range tile pos)
            |> List.fold min System.Int32.MaxValue

        let band =
            approach.Value
            |> Set.filter (fun tile ->
                tile.Room = room && not (unsafe (indexOf (RoomPos.pos tile))))

        let gapOf (tile: RoomPos) =
            ground
            |> List.map (range (RoomPos.pos tile))
            |> List.fold min System.Int32.MaxValue

        [ 1 .. min 3 (gap - 1) ]
        |> List.tryPick (fun radius ->
            let near = band |> Set.filter (fun tile -> gapOf tile <= radius)

            if Set.isEmpty near then
                None
            else
                Atlas.firstStepAvoidingWhere atlas creep Set.empty unsafe (Some later) near)

    let walk =
        match goals |> Seq.tryHead with
        | Some tile when tile.Room <> room ->
            Atlas.stepTowardRoomWhere atlas creep Set.empty (Some unsafe) (Some later) tile.Room
        | _ ->
            Atlas.firstStepAvoidingWhere atlas creep Set.empty unsafe (Some later) goals
            |> Option.orElseWith closer

    match walk with
    | Some step -> Some step
    | None ->
        Atlas.dropStep atlas creep

        if unsafe (indexOf pos) then
            Atlas.adjacentWalkableIn atlas room pos
            |> List.sortBy (fun tile -> unsafe (indexOf tile), -(later (indexOf tile)), tile)
            |> List.tryHead
            |> Option.filter (fun tile ->
                not (unsafe (indexOf tile)) || later (indexOf tile) > later (indexOf pos))
            |> Option.map (RoomPos.at room)
        else
            None

/// What a fleeing body's walk keeps off (movement S4): the tiles a hostile
/// strikes before it could be on them and gone (`Threats.unsafeFor`, every
/// class counted); None where no map is near it.
let private fleeUnsafe (threats: Threats) atlas (creep: string) : (int -> bool) option =
    Threats.bodyOf threats creep
    |> Option.bind (fun body -> Threats.unsafeFor threats atlas body false)

/// A fleeing body's step toward its ground (movement S4), the full tick's
/// and the light tick's plan alike. Near a map, the walk through the tiles it
/// is on and gone first, all the way and not its first step alone (#608: the
/// plain walk home led a reserver through a swamp the shooter struck), toward
/// the ground's room where that lies across a border (#511); with no such
/// walk, the plain step (`stepToward`) where no hostile strikes that tile
/// before the body is gone from it. Else never a step onto a tile struck
/// sooner than its own: the tile beside it struck latest after the body could
/// be gone from it (a swamp holding it counted), where struck later than its
/// own, else the plain step where that is struck no sooner, else none.
let private fleeStep
    (threats: Threats)
    atlas
    (creep: string)
    (at: RoomPos)
    (area: Set<RoomPos>)
    (later: (int -> int) option)
    : RoomPos option =
    let room = at.Room
    // Priced only where the walk round finds nothing: the walk is the step.
    let direct = lazy (stepToward atlas creep Flee area)

    match fleeUnsafe threats atlas creep with
    | None -> direct.Value
    | Some unsafe ->
        let walked =
            match area |> Seq.tryHead with
            | Some tile when tile.Room <> room ->
                Atlas.stepTowardRoomWhere atlas creep Set.empty (Some unsafe) later tile.Room
            | _ -> Atlas.firstStepAvoidingWhere atlas creep Set.empty unsafe later area

        let direct = direct.Value

        match walked with
        | Some step -> Some step
        | None when direct |> Option.forall (fun step -> step.Room <> room) -> direct
        | None when direct |> Option.exists (fun step -> not (unsafe (indexOf (RoomPos.pos step)))) ->
            direct
        | None ->
            Atlas.dropStep atlas creep

            match Threats.soonestIn threats room with
            | Some soonest ->
                let pos = RoomPos.pos at

                // The ticks its gait holds it on a tile past the tick it is
                // on it (a swamp, its load): the tiles beside it rank by the
                // strike less that hold, so the plain outranks a swamp
                // struck a tick later that holds it four.
                let held =
                    match
                        Threats.bodyOf threats creep |> Option.bind (Threats.walkOf threats atlas)
                    with
                    | Some walk when walk.Room = room ->
                        fun index ->
                            let on = Grid.at index walk.Ticks

                            if on = System.Int32.MaxValue then
                                0
                            else
                                Grid.at index walk.Leaves - on
                    | _ -> fun _ -> 0

                let struck index = soonest index - held index
                // Standing is no hold that ends: a body that stays is struck
                // every tick it stays, so a step is weighed against the
                // strike on its own tile alone. Weighed by its hold too, a
                // reserver on a swamp whose only ways off are swamps (#608,
                // (9,22)) stood and was shot dead.
                let here = soonest (indexOf pos)

                Atlas.adjacentWalkableIn atlas room pos
                |> List.sortBy (fun tile -> unsafe (indexOf tile), -(struck (indexOf tile)), tile)
                |> List.tryHead
                |> Option.filter (fun tile ->
                    not (unsafe (indexOf tile)) || soonest (indexOf tile) > here)
                |> Option.map (RoomPos.at room)
                // Where every tile about it is struck as soon as its own (a
                // room its towers cover whole, #566), the plain step, which
                // is struck no sooner.
                |> Option.orElse (
                    direct
                    |> Option.filter (fun step -> soonest (indexOf (RoomPos.pos step)) >= here)
                )
            | None -> None

/// Register one creep's Move Intent — every creep gets one: a traveller heads
/// its next step and tails a sidestep, an arrived body heads its own tile and
/// tails its area's neighbours then the ground outside, an idle body heads the
/// first step off the idle ground and tails its own tile, and a body with a
/// Task it cannot reach is parked on the Task's rank. The order is the whole
/// of the preference. One tile is never a candidate: the creep's own when it
/// is a Seam (#142), since a creep ending its tick on the ring is moved out of
/// the room again — unless across is where its step is taking it (#436). The
/// Task goes to `stepToward` beside the area, which is what gives a creep
/// matched across a border somewhere to walk. A body
/// crossing for a room it cannot see keeps crossing (#151): the vision grace
/// hands the mover the room its target was last seen in and nothing else,
/// which is enough for `Atlas.stepTowardRoom`, and it is its own arrival that
/// ends the darkness.
let private moveIntentFor
    (rankOf: Task -> int)
    (idleGround: string -> Set<Pos> * Set<RoomPos>)
    (threats: Threats)
    atlas
    (creep: string)
    (runs: bool)
    // A body walking home out of an evacuating room, round its Reach (#512).
    (walksRound: bool)
    // What it walks round where it walks round a Reach, first choice first
    // (`movementOf.roundsOf`), asked only where it does.
    (rounds: unit -> (Set<Pos> * (int -> bool) option) list)
    // For a fighter, the rank an equal-tick step of its walk round is broken
    // by, toward the tile a hostile strikes later (movement S3); None for
    // every other body.
    (later: (int -> int) option)
    // The home's shelter for an idle body at home that walks in to it and
    // stays on it (#514); empty for every other.
    (shelter: Set<RoomPos>)
    // For a kiting body, its walk in place of the Reach's (#510); None for
    // every other body.
    (kite: Kiting option)
    // For a fighter aiming at a target in its room, its chase (`chaseStep`);
    // None for every other body.
    (chase: Chase option)
    // For a trespass's killer, the Reach it walks round (#516); None for
    // every other body.
    (reachRound: Set<Pos> option)
    (at: RoomPos)
    (task: Task option)
    (crossing: string option)
    : MoveIntent =
    // The room the creep stands in and the only room its candidates are tiles
    // of (#145).
    let room = at.Room
    let pos = RoomPos.pos at
    let here = RoomPos.at room
    let around = Atlas.adjacentWalkableIn atlas room pos
    let onSeam = Atlas.standsOnSeam atlas creep && not (List.isEmpty around)

    // The ground beside it it may be stepped or shoved onto: for a kiter,
    // never a tile a melee strikes before it could be off it again
    // (movement S3), whoever's chain asks.
    let beside =
        match kite with
        | Some(Kiting.Keeps(unsafe, _, _)) ->
            around |> List.filter (fun tile -> not (unsafe (indexOf tile)))
        | _ -> around

    // Where this creep may stay: its own tile, unless that tile is a Seam
    // with ground beside it to walk onto.
    let staying = if onSeam then [] else [ pos ]

    // Every branch below decides three things and no more: the rank it pushes
    // at, the tiles it will accept in the order it wants them tried, and the
    // Work Area the arbitration charges a push out of. The rest is stated here
    // once, so a branch cannot forget the room stamp.
    let intent rank (candidates: Pos list) area =
        {
            Creep = creep
            Pos = at
            Rank = rank
            Candidates = candidates |> List.map here
            Area = area
            // Read after the branch priced its step, which filed the plan.
            Onward =
                match candidates with
                | head :: _ when head <> pos ->
                    match Atlas.stepPlanOf atlas creep with
                    | Some(first :: second :: _) when first = here head -> Some second
                    | _ -> None
                | _ -> None
        }

    // A parked body has a Task it cannot reach, so it is standing where its
    // Work Area is not.
    let parked rank =
        intent rank (staying @ beside) Set.empty

    // The detours behind a step: the ground beside this creep that also lies
    // beside the step it asked for — a way *around* the tile it wanted and
    // never a way back down the lane it came up. Without the tail a creep
    // whose one candidate is held by a body that cannot move stands still for
    // as long as that body does; with the whole neighbourhood in it, a
    // traveller queued behind a merely fatigued creep would back away and
    // return every other tick.
    let detour step =
        if onSeam then
            beside |> List.filter ((<>) step)
        else
            let around = Atlas.adjacentWalkableIn atlas room step |> Set.ofList
            beside |> List.filter (fun tile -> Set.contains tile around)

    // A body that has not arrived: the step it asked for, the ways around it,
    // and no Work Area at all — it is standing outside the one it is walking
    // to, so nothing it is pushed off is work. A step onto its own tile is the
    // crossing it stands on, and it holds that and nothing else.
    let travelling rank step =
        if step = pos then
            intent rank [ pos ] Set.empty
        else
            intent rank (step :: detour step) Set.empty

    // A body standing outside its room's `reach`, the tiles of the last of
    // `avoids`, walks round it (#472): the step that never enters the first
    // of `avoids` a walk keeps out of — its tiles and, where given, those its
    // test answers for by index —, else the plain one where that stays out
    // of `reach`, else none: it waits at the edge, planning no step for the
    // light tick to replay. Inside it, or with none, the plain step.
    let round
        (avoids: (Set<Pos> * (int -> bool) option) list)
        (avoiding: Set<Pos> -> (int -> bool) option -> RoomPos option)
        (plain: unit -> RoomPos option)
        =
        let reach = fst (List.last avoids)

        if Set.isEmpty reach || Set.contains pos reach then
            plain ()
        else
            match avoids |> List.tryPick (fun (avoid, also) -> avoiding avoid also) with
            | Some step -> Some step
            | None ->
                let step =
                    plain ()
                    |> Option.filter (fun tile ->
                        not (tile.Room = room && Set.contains (RoomPos.pos tile) reach))

                if step.IsNone then
                    Atlas.dropStep atlas creep

                step

    // The graced holder's crossing (#151), asked before the Task branches
    // because it is the one body with neither. No Seam to that room — it is
    // not next door — and it falls through to the idle rule below. It crosses
    // on the cheapest chain, as the price does (#297, #485). A body walking
    // home out of an evacuating room walks round its Reach, a Work-heavy one
    // too (#512).
    let crossingStep =
        match task, crossing with
        | None, Some target ->
            let avoids = if walksRound then rounds () else [ Set.empty, None ]

            round
                avoids
                (fun avoid also -> Atlas.stepTowardRoomWhere atlas creep avoid also later target)
                (fun () -> Atlas.stepTowardRoom atlas creep Set.empty target)
            |> Option.map RoomPos.pos
        | _ -> None

    // The walk in to the shelter (#514), round the Reach as a runner's is,
    // every tick until the body stands on it. One standing in a Reach — a
    // Work-heavy body on its footing, which has no Flee — takes the plain
    // step inward, even into the Reach, rather than stand still in it
    // (`round`).
    let shelterStep () =
        if task.IsNone && not (Set.isEmpty shelter) && not (Set.contains at shelter) then
            let avoids =
                if Set.isEmpty (Threats.reachIn threats room) then
                    [ Set.empty, None ]
                else
                    rounds ()

            round
                avoids
                (fun avoid also ->
                    Atlas.firstStepAvoidingWhere
                        atlas
                        creep
                        avoid
                        (Option.defaultValue (fun _ -> false) also)
                        None
                        shelter)
                (fun () -> Atlas.firstStepWithin atlas creep shelter)
            |> Option.map RoomPos.pos
        else
            None

    match crossingStep |> Option.orElseWith shelterStep, task with
    | Some step, _ ->
        // Walking toward a room it cannot even see, or in to the shelter with
        // no Task, so it pushes at the idle rank.
        travelling idleRank step
    | None, None ->
        // The room's idle ground and the ground just off it: any way off runs
        // through one of those tiles, so the goal set stays the perimeter
        // rather than the whole room.
        let ground, offGround = idleGround room

        // A body on the shelter steps nowhere off it (#514), neither off the
        // idle ground nor when shoved.
        let sheltered (tile: Pos) =
            Set.isEmpty shelter || Set.contains (here tile) shelter

        // The tail, ordered off the idle ground first: `arbitrate` re-houses a
        // displaced body on the first free tile of its list, so an unordered
        // tail puts a body shoved off the ground straight back onto it, and
        // two idle bodies trade the one tile off the ground every tick.
        let off, on =
            beside
            |> List.filter sheltered
            |> List.partition (fun tile -> not (Set.contains tile ground))

        let tail = staying @ off @ on

        // The way off, less this tick's Reach — the subtraction `areaFor`
        // makes below, made here too because a work-heavy body on its footing
        // has no Flee, so a step out of a safe pocket into an attacker is one
        // nothing walks it back from. Nowhere safe off the ground is nowhere
        // to go: it parks.
        let stepOff =
            if Set.contains pos ground then
                let reach = Threats.reachIn threats room

                offGround
                |> Set.filter (fun tile ->
                    not (Set.contains (RoomPos.pos tile) reach) && sheltered (RoomPos.pos tile))
                |> Atlas.firstStepWithin atlas creep
                |> Option.map RoomPos.pos
            else
                None

        // A body with no Task is working from nowhere: shoving it off costs
        // the chain nothing.
        intent
            idleRank
            (match stepOff with
             | Some step -> step :: (tail |> List.filter ((<>) step))
             | None -> tail)
            Set.empty
    | None, Some task ->
        // The area less this tick's Reach: a creep works from the safe half of
        // its Work Area rather than abandoning the Task because one corner is
        // hot. Read against the set the Atlas already holds rather than a
        // narrowed copy of it.
        let area = areaFor threats atlas creep task

        // A kiter stands on its ground only while a melee strikes it there no
        // sooner than it could step off again (movement S3).
        let standsSafe () =
            match kite with
            | Some(Kiting.Keeps(unsafe, _, _)) -> not (unsafe (indexOf pos))
            | _ -> true

        if Set.contains (here pos) area && standsSafe () then
            let inside, outside =
                beside |> List.partition (fun tile -> Set.contains (here tile) area)

            // A chaser standing on its ground: the ticks its quarry stays in
            // its reach, walking on (`layChain`).
            match chase, kite with
            | _, Some(Kiting.Keeps _)
            | None, _ -> ()
            | Some chase, _ -> layChain atlas creep chase later at None false

            // The one body that has arrived: the tiles it may be shuffled
            // between for nothing, and the border the arbitration charges for
            // pushing it over — the area less this tick's Reach, the same set
            // the candidates were partitioned on.
            intent (rankOf task) (pos :: (inside @ outside)) area
        else
            // A body that runs from a Threat, standing outside its room's
            // Reach, walks round it (#472), or waits at its edge where no way
            // round reaches the work. Flee's own walk is the way out of one.
            // A Defend's holder walks round the tiles outside its line a raid melee
            // reaches (#504), and waits inside it where no way round reaches
            // its rampart. A kiting body keeps off what a class it stands off
            // strikes before it could be on it and gone (`kiteStep`), across the
            // border too; a trespass's killer walks round the Reach.
            let plain () = stepToward atlas creep task area

            let step =
                match kite, reachRound with
                | Some(Kiting.Keeps(unsafe, soonest, approach)), _ ->
                    kiteStep atlas creep at unsafe soonest approach area
                | Some Kiting.Clear, _ -> plain ()
                | None, Some avoid ->
                    round
                        [ avoid, None ]
                        (fun avoid _ ->
                            match area |> Seq.tryHead with
                            | Some tile when tile.Room <> room ->
                                Atlas.stepTowardRoom atlas creep avoid tile.Room
                            | _ -> Atlas.firstStepAvoiding atlas creep avoid area)
                        plain
                | None, None when task = Flee -> fleeStep threats atlas creep at area later
                | None, None when runs ->
                    let avoids =
                        match task with
                        | Defend _ -> [ Threats.defendAvoidIn threats room, None ]
                        | _ when Set.isEmpty (Threats.reachIn threats room) -> [ Set.empty, None ]
                        | _ -> rounds ()

                    // A walk to work in another room crosses toward it
                    // (`stepTowardRoom`), round what it avoids here (#527):
                    // its Work Area is empty from across a border, so asked
                    // of the room the work is in.
                    let workRoom =
                        area
                        |> Seq.tryHead
                        |> Option.map (fun tile -> tile.Room)
                        |> Option.orElseWith (fun () -> roomOfWork atlas task)

                    round
                        avoids
                        (fun avoid also ->
                            match workRoom with
                            | Some other when other <> room ->
                                Atlas.stepTowardRoomWhere atlas creep avoid also later other
                            | _ ->
                                Atlas.firstStepAvoidingWhere
                                    atlas
                                    creep
                                    avoid
                                    (Option.defaultValue (fun _ -> false) also)
                                    later
                                    area)
                        plain
                | None, None -> plain ()

            // A chaser's step weighed by when it strikes its target next
            // (`chaseStep`), whichever walk gave it; the light tick replays
            // what it keeps, and of a step kept, the plan its chase keeps
            // (`chasePlan`).
            // Off the kite, its walk goes on after an unarmed quarry walking
            // on over the cadence window (`layChain`), where no weighing cut
            // it short.
            let step =
                match chase with
                | Some chase ->
                    let chained step extend =
                        match kite with
                        | Some(Kiting.Keeps _) -> ()
                        | _ -> layChain atlas creep chase later at step extend

                    match chaseStep chase at beside later step with
                    | Some kept ->
                        match kept with
                        | Some tile -> Atlas.recordStep atlas creep (at, tile)
                        | None -> Atlas.dropStep atlas creep

                        chained kept false
                        kept
                    | None ->
                        match
                            Atlas.stepPlanOf atlas creep
                            |> Option.filter (fun plan -> step.IsSome && List.tryHead plan = step)
                            |> Option.bind (chasePlan atlas chase at later)
                        with
                        | Some count ->
                            Atlas.keepSteps atlas creep count
                            chained step false
                        | None -> chained step true

                        step
                | None -> step

            match step |> Option.map RoomPos.pos with
            | Some step -> travelling (rankOf task) step
            | None -> parked (rankOf task)

/// The push a rank carries into the arbitration's arithmetic: a *weight*, so a
/// chain seating two bodies on the steps they asked for can outweigh one body
/// pushed off its own. Positive and never rising with rank, and `idleRank`
/// lands on the smallest weight rather than none, or a crowd of idle bodies
/// would be a wall no traveller could walk into. The ladder's rungs are divided
/// back out, rounding to the **nearest** tier (#237): a rung is a claim about
/// which of two Tasks a creep should take, never about how hard it pushes, and
/// a two-rung step rounded past its own tier when the window was one rung
/// wide. A tier owns the ranks from `tierRungs / 2` above it to
/// `tierRungs / 2 - 1` below, a tie going to the deeper tier, so a rung may be
/// half a tier at the most (`Pool.Rung`). Exported for the tests that walk it.
let weightOfRank (rank: int) : int =
    max 1 (ceilDiv (priorityOfTier Stock - rank - tierRungs / 2) tierRungs + 2)

/// The room's matching while the arbitration runs: a tile's holder and a
/// holder's tile, one relation written both ways because the search reads it
/// both ways — the tile to find whom to displace, the creep to find what to
/// vacate. Injective in both directions by construction: a creep's own entry is
/// rewritten by the assignment that moves it, and the chain's initiator has its
/// tile emptied before the search starts.
type private Matching =
    {
        Holder: Map<RoomPos, string>
        Tile: Map<string, RoomPos>
    }

/// Resolver core: the room's Move Intents matched onto its tiles by a weighted
/// augmenting search — sy-harabi's traffic manager, read and rewritten rather
/// than linked (unlicensed, and it issues the engine's moves itself), without
/// its hash-shuffled candidate order (every tie here falls to the lowest x then
/// y), its cost-matrix threshold (a crowd is priced, never impassable) or its
/// free-tiles-first order (incompatible with the head-and-tail list). A room's
/// pass is O(creeps x 8). The intents are offered in a deterministic order:
/// travellers before stayers (a stayer settled first walls off a traveller's
/// only path), then by rank, then by name. Only a strictly positive chain is
/// taken, and dropping the Map the search returned is the whole rollback.
let private arbitrate
    (occupants: Map<RoomPos, string>)
    (blocked: Set<RoomPos>)
    (moveIntents: MoveIntent list)
    : Map<string, RoomPos> =
    let byCreep = moveIntents |> List.map (fun i -> i.Creep, i) |> Map.ofList

    let headOf (intent: MoveIntent) = List.tryHead intent.Candidates

    // A creep whose first candidate is the tile it stands on: it asked to
    // stay, and it is displaceable by anybody.
    let staying (intent: MoveIntent) = headOf intent = Some intent.Pos

    let place creep tile (m: Matching) =
        {
            Holder = Map.add tile creep m.Holder
            Tile = Map.add creep tile m.Tile
        }

    let vacate creep (m: Matching) =
        match Map.tryFind creep m.Tile with
        | Some tile ->
            {
                Holder = Map.remove tile m.Holder
                Tile = Map.remove creep m.Tile
            }
        | None -> m

    // What a creep landing on `tile` is worth to the chain. The eviction
    // charge is levied here, on the landing, because that is where the search
    // knows where the body ended up; `cost` below is charged on the push. It
    // is the weight **and one**: a chain that ends with its initiator stepping
    // aside onto a tail scores exactly 1, so at the bare weight an eviction
    // would be worth the same as walking round as soon as the arriving body is
    // one tier up.
    let gain initiator (intent: MoveIntent) tile =
        if headOf intent = Some tile then
            weightOfRank intent.Rank
        elif not (Set.isEmpty intent.Area) && not (Set.contains tile intent.Area) then
            -(weightOfRank intent.Rank + 1)
        elif initiator then
            1
        else
            0

    // What taking `tile` off its occupant costs the chain: the weight of a
    // step the occupant had asked for and is being denied. A stayer is denied
    // nothing here — it asked for the tile it is standing on, and what its
    // displacement costs depends on where it lands, which `gain` above prices.
    let cost (occupant: MoveIntent) tile =
        if headOf occupant = Some tile && not (staying occupant) then
            weightOfRank occupant.Rank
        else
            0

    // A body with no Task standing where it is: its tile is anybody's (#562).
    // Not a parked body, which has a Task and chose its tile — a Defend
    // holder waiting inside its line.
    let idle (intent: MoveIntent) =
        staying intent && intent.Rank = idleRank

    // Where an idle body is tried once a mover shoves it off its tile:
    // anywhere else first, then the mover's own tile (a swap), and the
    // mover's way on last (#562) — up the lane ahead of it is where the mover
    // shoves it again the next tick, and the tick after. Every other body
    // keeps its own order.
    let rehouse (mover: MoveIntent) (occupant: MoveIntent) (tiles: RoomPos list) =
        if idle occupant then
            let last = List.choose id [ Some mover.Pos; mover.Onward ]

            (tiles |> List.filter (fun tile -> not (List.contains tile last)))
            @ (last |> List.filter (fun tile -> List.contains tile tiles))
        else
            tiles

    // `visited` is threaded through and never rolled back, the one thing the
    // search borrows from its mutable original: a creep the chain has already
    // tried to rehouse is not tried again inside the same outer search, which
    // bounds the work at one expansion per creep per creep.
    let rec augment
        (initiator: bool)
        (visited: Set<string>)
        (score: int)
        (intent: MoveIntent)
        (candidates: RoomPos list)
        (m: Matching)
        : Set<string> * (int * Matching) option =
        let visited = Set.add intent.Creep visited

        let rec walk visited tiles =
            match tiles with
            | [] -> visited, None
            | tile :: rest ->
                if Set.contains tile blocked then
                    walk visited rest
                else
                    let score = score + gain initiator intent tile

                    match Map.tryFind tile m.Holder with
                    | None ->
                        if score > 0 then
                            visited, Some(score, place intent.Creep tile m)
                        else
                            walk visited rest
                    | Some held when Set.contains held visited -> walk visited rest
                    | Some held ->
                        match Map.tryFind held byCreep with
                        | None -> walk visited rest
                        | Some occupant ->
                            // The occupant keeps its tile filed under its name
                            // while its own search runs, and `place` below
                            // overwrites that entry once the chain comes back.
                            let visited, outcome =
                                augment
                                    false
                                    visited
                                    (score - cost occupant tile)
                                    occupant
                                    (occupant.Candidates
                                     |> List.filter ((<>) tile)
                                     |> rehouse intent occupant)
                                    m

                            match outcome with
                            | Some(total, settled) when total > 0 ->
                                visited, Some(total, place intent.Creep tile settled)
                            | _ -> walk visited rest

        walk visited candidates

    // The identity matching: everybody standing in the room, whether or not
    // it registered an intent, so an occupant with none is a wall the search
    // finds by looking rather than a tile somebody has to remember to block.
    let start =
        {
            Holder = occupants
            Tile =
                occupants
                |> Map.toList
                |> List.map (fun (tile, creep) -> creep, tile)
                |> Map.ofList
        }

    // Whether an intent has nothing left to ask for: it holds the tile it
    // asked for, or it is an arrived body an earlier chain shuffled within its
    // own area, or an idle body an earlier chain shoved anywhere. Re-initiating the second would collect its rank's whole weight
    // for being put back on a tile it never chose to leave, and three such
    // phantom gains in one chain buy the very eviction `gain` prices; the
    // third would walk off the tile `rehouse` chose onto the mover's way on
    // (#562).
    let asked (intent: MoveIntent) (m: Matching) =
        let held = Map.tryFind intent.Creep m.Tile

        held = headOf intent
        || (staying intent
            && (match held with
                | Some tile -> idle intent || Set.contains tile intent.Area
                | None -> false))

    let settled =
        (start, moveIntents |> List.sortBy (fun i -> staying i, i.Rank, i.Creep))
        ||> List.fold (fun m intent ->
            if asked intent m then
                m
            else
                match
                    augment true Set.empty 0 intent intent.Candidates (vacate intent.Creep m)
                with
                | _, Some(_, settled) -> settled
                | _, None -> m)

    // The creeps that registered an intent and nobody else: what the pass
    // above reads back is each *rested* creep's settled tile, and a fatigued
    // occupant is answered for out of `Blocked` and `Occupants` instead.
    settled.Tile |> Map.filter (fun creep _ -> Map.containsKey creep byCreep)

/// One room's arbitration, settled: what each of its rested creeps was
/// settled on and what it asked for first, and the fatigued creeps' tiles
/// and the occupants the settlement was made against — the four things
/// the Verdicts read back, all keyed on that room's tiles alone (#145).
type private RoomPass =
    {
        /// Each rested creep's settled standing tile.
        Standing: Map<string, RoomPos>
        /// Each rested creep's preferred standing tile: the head of its
        /// candidate list — a Move Intent's candidates are never empty.
        Preferences: Map<string, RoomPos>
        /// The tiles no intent in this pass may be settled onto and no
        /// chain may run through: the fatigued creeps' and the foreign
        /// bodies' that nobody in the fold holds (#220).
        Blocked: Set<RoomPos>
        /// Who stands where at tick start.
        Occupants: Map<RoomPos, string>
    }

/// Whether a Guard's holder may close on its room's raid (#615 U1): it fits
/// the fight (`Facts.fits`, #615 U3) beside the other fighters of ours
/// holding that Guard as they stand, the answer it was matched by read
/// where it stands, as its holder.
let closes
    (view: ColonyView)
    (threats: Threats)
    (assigned: Map<string, Task>)
    (creep: CreepInfo)
    : bool =
    match Map.tryFind creep.Name assigned with
    | Some(Guard room as task) ->
        let brief = Threats.briefOf threats room

        Facts.fits
            view
            (Brief.stanceOf view room brief creep)
            room
            threats.Horizon
            true
            creep
            (view.Creeps
             |> List.filter (fun other -> other.Name <> creep.Name && isFighterBody other)
             |> List.map (fun other -> other, Map.tryFind other.Name assigned = Some task, None))
    | _ -> false

/// Resolver, first half: one colony's Move Intents, unarbitrated. Every rested
/// creep the Atlas places registers one; a fatigued creep registers none — the
/// engine would answer its move with ERR_TIRED — and its tile is a wall for the
/// tick (ADR-0008). Takes the tick's assigned Task per creep as data; a creep
/// absent from the map is idle, unless it is in `crossings` — the vision
/// grace's holders and the room each is still walking toward (#151). Rerouted
/// is settled here rather than in the pass, because it is the one movement
/// Verdict the arbitration does not answer: it compares this creep's priced
/// first step against the step the same body would take were no tile occupied,
/// which is a second flood on this colony's Atlas.
///
/// `moves`: the bodies it registers and places, each of the rest a wall where
/// the Atlas stands it; a fight-only decide (`decideFight`) moves its
/// fighters alone, the rest stepping on their replayed plans.
let movementAmong
    (moves: string -> bool)
    (view: ColonyView)
    atlas
    (threats: Threats)
    (pool: PooledTask list)
    (assigned: Map<string, Task>)
    (crossings: Map<string, string>)
    (verbose: Set<string>)
    : Movement =
    // The push each assigned Task carries into arbitration is its own pooled
    // priority, read off the pool rather than re-derived: the mover and the
    // Matcher order the colony's work by one number or they order it by two.
    let priorities = pool |> List.map (fun p -> taskId p.Task, p.Priority) |> Map.ofList

    let priorityOf task =
        Map.tryFind (taskId task) priorities |> Option.defaultValue idleRank

    let tired =
        view.Creeps
        |> List.choose (fun c -> if c.Fatigue > 0 then Some c.Name else None)
        |> Set.ofList

    let placed = Atlas.placedCreeps atlas |> List.filter (fst >> moves)

    // Asked only on a tick with a Reach: the mover's one reader of it.
    let runners =
        if Map.isEmpty threats.Reach then
            Set.empty
        else
            view.Creeps
            // And a [[defender]], to its rampart (#504), and any holder of the
            // home's Defend (#546).
            |> List.filter (fun c ->
                runsFromThreats view atlas c
                || isNamedFor defenderPattern c.Name
                || (match Map.tryFind c.Name assigned with
                    | Some(Defend _) -> true
                    | _ -> false))
            |> List.map (fun c -> c.Name)
            |> Set.ofList

    // A fighter walking to a fight in another room than the one it stands in
    // walks round what out-damages it here (movement S2, #533): the fight it
    // walks to is not this room's.
    let passers =
        if Map.isEmpty threats.Reach then
            Set.empty
        else
            placed
            |> List.filter (fun (name, at) ->
                match Map.tryFind name assigned with
                | Some(Guard room)
                | Some(Assault room) -> room <> at.Room
                | _ -> false)
            |> List.map fst
            |> Set.ofList
            |> Set.filter (fun name ->
                view.Creeps |> List.exists (fun c -> c.Name = name && isFighterBody c))

    // What a body walks round in the room it stands in, first choice first
    // (movement S2): the room's Reach — for a fighter, the tiles of it where
    // what strikes within the horizon out-damages its heal —, where the body
    // would stand at its next decision, with the tiles a hostile strikes
    // before it could be on them and gone (`Threats.unsafeFor`); then, where
    // no walk keeps out of those, the Reach alone. The last is the one its
    // own tile and its plain step are read against. A fighter that wins the
    // exchange against what strikes the room walks nothing round (#568,
    // movement S3): the gate weighs the fight, not a tile's damage alone;
    // and a holder of the room's own Guard that stands off its raid and fits
    // the fight beside the holders there (`closes`, #611), whose partner's
    // shots are its win; a swing holding a post walks to it round the Reach.
    // `closes` priced once a body this pass.
    let closed = System.Collections.Generic.Dictionary<string, bool>()

    let closesNow (creep: CreepInfo) =
        match closed.TryGetValue creep.Name with
        | true, answer -> answer
        | _ ->
            let answer = closes view threats assigned creep
            closed.[creep.Name] <- answer
            answer

    let roundsOf name room =
        let reach = Threats.reachIn threats room

        match view.Creeps |> List.tryFind (fun c -> c.Name = name) with
        | None -> [ reach, None ]
        | Some creep when
            isFighterBody creep
            && (Threats.winsIn view threats room creep
                || Map.tryFind name assigned = Some(Guard room)
                   && Facts.raided view room
                   && Brief.stanceOf view room (Threats.briefOf threats room) creep = Stance.Standoff
                   && closesNow creep)
            ->
            [ Set.empty, None ]
        | Some creep ->
            let fighter = isFighterBody creep
            let heal = Engine.healPower * partCount creep.Body Heal

            let standing =
                if fighter && heal > 0 then
                    reach
                    |> Set.filter (fun tile ->
                        Threats.damageAt threats room view.Tuning.ReachHorizon tile > heal)
                else
                    reach

            match Threats.unsafeFor threats atlas creep fighter with
            | Some timed -> [ standing, Some timed; standing, None ]
            | None -> [ standing, None ]

    // The bodies walking home out of an evacuating room, which walk round its
    // Reach (#512), and every idle fighter walking home (#533), or it walks
    // back into the Reach Flee walked it out of: asked, as the runners are,
    // only on a tick with one.
    let home = SpatialInfo.homeName view.Spatial

    let roundWalkers =
        if Map.isEmpty threats.Reach then
            Set.empty
        else
            let fighters =
                view.Creeps
                |> List.filter isFighterBody
                |> List.map (fun c -> c.Name)
                |> Set.ofList

            placed
            |> List.filter (fun (name, at) ->
                match Map.tryFind name crossings with
                | Some target ->
                    target = home && Set.contains at.Room view.Evacuating
                    || Set.contains name fighters && target = ColonyView.idleHome view name
                | None -> false)
            |> List.map fst
            |> Set.ofList

    // The idle bodies at home while it has a shelter (#514): any but a
    // Fighter, each walking in to it until it stands on it and staying on it
    // after. Read off where the body stands and nothing remembered, so a walk
    // begun beside an exit is kept every tick until it arrives. Asked only on
    // a tick with a shelter; a tired body issues no step at all.
    let sheltering =
        if Map.containsKey home threats.Shelter then
            view.Creeps
            |> List.filter (fun creep ->
                not (isFighterBody creep)
                && not (Map.containsKey creep.Name assigned)
                && not (Map.containsKey creep.Name crossings)
                && not (Set.contains creep.Name tired)
                && Atlas.creepRoom atlas creep.Name = Some home)
            |> List.map (fun creep -> creep.Name)
            |> Set.ofList
        else
            Set.empty

    // What a Guard's holder walks round is its Brief's (`Brief.Avoid`).
    let avoidFor name =
        match Map.tryFind name assigned with
        | Some(Guard room) -> (Threats.briefOf threats room).Avoid
        | _ -> Avoid.Runner

    // A Chase's holder that outranges a swing kites (#510, #615 U1); a
    // resident's walk onto its ramparts under a raid crosses the Reach, and
    // is no kite. Its walk keeps off what a class it stands off strikes
    // before it could be on it and gone, its own timing counted, and the camp
    // about the exit a raid left by (`Threats.kiteUnsafeFor`). It closes to
    // its reach (`Kiting.Clear`) only where it stands off nothing there
    // (`Threats.standsOffNothing`: the raid picks the standoff) and wins the
    // fight (`closes`); a body that can neither keep off nor win keeps its
    // kite walk, never walking into a quicker melee's ring.
    let kitingOf name (at: RoomPos) =
        match view.Creeps |> List.tryFind (fun c -> c.Name = name) with
        | Some creep when avoidFor name = Avoid.Kite && Fighter.outranges (Fighter.ofCreep creep) ->
            let camp = Map.tryFind at.Room view.ExitHolds |> Option.map (fun hold -> hold.Exit)

            let approach =
                match Map.tryFind name assigned with
                | Some(Guard room) -> (Threats.briefOf threats room).Laid.Approach
                | _ -> lazy Set.empty

            match Threats.kiteUnsafeFor threats atlas creep camp with
            | Some(unsafe, soonest) when
                not (Threats.standsOffNothing threats atlas creep camp && closesNow creep)
                ->
                Some(Kiting.Keeps(unsafe, soonest, approach))
            | _ -> Some Kiting.Clear
        | _ -> None

    // The bodies by name, for the chase's lookup of each placed body.
    let named = lazy (view.Creeps |> List.map (fun c -> c.Name, c) |> Map.ofList)

    // A fighter aiming at a target in its room weighs its step by when it
    // strikes next (`chaseFor`).
    let chaseOf name (at: RoomPos) =
        if List.isEmpty view.Hostiles then
            None
        else
            Map.tryFind name named.Value
            |> Option.bind (fun creep -> chaseFor view atlas threats assigned creep at)

    // A fighter's walk round breaks an equal-tick tie toward the tile a
    // hostile strikes later (movement S3).
    let laterOf name (at: RoomPos) =
        if view.Creeps |> List.exists (fun c -> c.Name = name && isFighterBody c) then
            Threats.soonestIn threats at.Room
        else
            None

    // The ground an idle body steps off, and the ring of ground just outside
    // it that any step off has to land on — one pair per room some body of
    // ours idles in, and none at all for a tick where every body has a Task,
    // which is the tick that must pay nothing for this rule. `idleGroundIn`
    // and not `workingGroundIn`: the working ground is the Layout's question
    // and this is the mover's, and the two sets part company at the Storage's
    // and the refill cluster's standing tiles (#268).
    let idleGrounds =
        placed
        |> List.filter (fun (name, _) ->
            not (Map.containsKey name assigned)
            // A graced holder is walking a crossing, not idling (#151).
            && not (Map.containsKey name crossings)
            && not (Set.contains name tired))
        |> List.map (fun (_, at) -> at.Room)
        |> List.distinct
        |> List.map (fun room -> room, Atlas.idleGroundAndRingIn atlas room)
        |> Map.ofList

    let idleGround room =
        Map.tryFind room idleGrounds |> Option.defaultValue (Set.empty, Set.empty)

    // A fatigued walker issues no step this tick, so the light tick after it
    // had none to replay and it stood still the tick its fatigue cleared
    // (user, 2026-10-04): its plan is its own tile, then the step it would
    // take. The light tick re-issues it until the engine lets it move. A
    // kiter's is its kite step, the ticks it still stands held counted
    // (#524); a fleeing body's its flee step (movement S4).
    for name, at in placed do
        match Map.tryFind name assigned with
        | Some task when Set.contains name tired ->
            let area = areaFor threats atlas name task

            let kiting =
                match kitingOf name at with
                | Some(Kiting.Keeps(unsafe, soonest, approach)) -> Some(unsafe, soonest, approach)
                | _ -> None

            let standsSafe =
                match kiting with
                | Some(unsafe, _, _) -> not (unsafe (indexOf (RoomPos.pos at)))
                | None -> true

            let walk () =
                match kiting with
                | Some(unsafe, soonest, approach) ->
                    kiteStep atlas name at unsafe soonest approach area
                | None when task = Flee -> fleeStep threats atlas name at area (laterOf name at)
                | None -> stepToward atlas name task area

            // A chaser's, weighed as the full tick weighs it (`chaseStep`).
            let chased step =
                match chaseOf name at with
                | Some chase ->
                    let beside =
                        Atlas.adjacentWalkableIn atlas at.Room (RoomPos.pos at)
                        |> List.filter (fun tile ->
                            match kiting with
                            | Some(unsafe, _, _) -> not (unsafe (indexOf tile))
                            | None -> true)

                    chaseStep chase at beside (laterOf name at) step |> Option.defaultValue step
                | None -> step

            if not (Set.contains at area && standsSafe) then
                match Atlas.withoutRecording atlas name walk |> chased with
                | Some step when step.Room = at.Room && step <> at ->
                    Atlas.recordStep atlas name (at, step)
                | _ -> ()
        | _ -> ()

    // A reading and not a move, so it leaves the step plan the mover left for
    // the light tick as it was (`Atlas.withoutRecording`): it prices the
    // Task's own crossing where a Guard's mover prices the named room's.
    let rerouted name task =
        let area = areaFor threats atlas name task

        Atlas.withoutRecording atlas name (fun () ->
            match
                Atlas.firstStep atlas name task area,
                Atlas.firstStepIgnoringTraffic atlas name task area
            with
            | Some priced, Some blind -> priced <> blind
            | _ -> false)

    {
        Order = view.Creeps |> List.map (fun c -> c.Name) |> List.filter moves
        Placed = placed
        Tired = Set.filter moves tired
        // A body it does not move stands where it is, as another colony's.
        Foreign =
            Atlas.placedCreeps atlas
            |> List.filter (fst >> moves >> not)
            |> List.map snd
            |> Set.ofList
            |> Set.union view.Foreign
        Intents =
            placed
            |> List.filter (fun (name, _) -> not (Set.contains name tired))
            |> List.map (fun (name, at) ->
                moveIntentFor
                    priorityOf
                    idleGround
                    threats
                    atlas
                    name
                    (Set.contains name runners || Set.contains name passers)
                    (Set.contains name roundWalkers)
                    (fun () -> roundsOf name at.Room)
                    (laterOf name at)
                    (match Map.tryFind home threats.Shelter with
                     | Some ground when Set.contains name sheltering -> ground.Force()
                     | _ -> Set.empty)
                    (kitingOf name at)
                    (chaseOf name at)
                    (if avoidFor name = Avoid.Reach then
                         Some(Threats.reachIn threats at.Room)
                     else
                         None)
                    at
                    (Map.tryFind name assigned)
                    (Map.tryFind name crossings))
        Rerouted =
            placed
            |> List.choose (fun (name, _) ->
                if Set.contains name verbose then
                    match Map.tryFind name assigned with
                    | Some task when rerouted name task -> Some name
                    | _ -> None
                else
                    None)
            |> Set.ofList
    }

/// `movementAmong` over every body of the colony.
let movementOf
    (view: ColonyView)
    atlas
    (threats: Threats)
    (pool: PooledTask list)
    (assigned: Map<string, Task>)
    (crossings: Map<string, string>)
    (verbose: Set<string>)
    : Movement =
    movementAmong (fun _ -> true) view atlas threats pool assigned crossings verbose

/// Resolver, second half: the room passes, and the move Intents and movement
/// Verdicts they settle. One pass per room over **every** creep of ours
/// standing in it, whichever colony registered its intent (#220) — a room two
/// colonies work is one room, and half its traffic arbitrated against the other
/// half read as empty is how a body ends up claiming a tile the engine will
/// never let it into. Once per room, and never across two (#145): geometry
/// crosses the Seam and arbitration does not, so the tiles keying each room's
/// occupants, walls and intents carry the room they are in, or two creeps on
/// one coordinate of two rooms would collapse into one occupant. What is *not*
/// arbitrated is the border tile: two creeps aiming at one exit from its two
/// sides are never checked against each other. What crosses the Seam is the
/// *destination* (#142): a creep matched to an outpost's Task is arbitrated at
/// home over a home tile, and the next tick that room's pass walks it off the
/// ring onto its own floor.
///
/// `claims` are the tiles the bodies of colonies that do not decide this tick
/// step onto (a forced tick's replay, #549): their steps are issued whatever
/// this pass settles, so a claimed tile is a wall to every intent here, and an
/// idle body of ours standing on one steps off it where it can (#562) — a step
/// it is denied is a step the replayed body is.
let resolveRooms (claims: Set<RoomPos>) (movements: Movement list) : Intent list * Verdict list =
    let tired =
        (Set.empty, movements) ||> List.fold (fun acc m -> Set.union acc m.Tired)

    let everywhere = movements |> List.collect (fun m -> m.Placed)

    let passOf =
        everywhere
        |> List.map (fun (_, at) -> at.Room)
        |> List.distinct
        |> List.map (fun room ->
            let here = everywhere |> List.filter (fun (_, at) -> at.Room = room)
            let occupants = here |> List.map (fun (name, at) -> at, name) |> Map.ofList

            // The bodies no intent in this fold can move: the fatigued, and
            // the foreign tiles nobody standing here answered for.
            let foreign =
                (Set.empty, movements)
                ||> List.fold (fun acc m ->
                    Set.union acc (m.Foreign |> Set.filter (fun tile -> tile.Room = room)))
                |> Set.filter (fun tile -> not (Map.containsKey tile occupants))

            let moveIntents =
                movements
                |> List.collect (fun m -> m.Intents |> List.filter (fun i -> i.Pos.Room = room))

            // A claim moves only an idle body: a body with a Task on a
            // claimed tile — a Defend holder on its rampart — keeps it, the
            // claim is void, and the replayed step into it fails.
            let claimed =
                let inRoom = claims |> Set.filter (fun tile -> tile.Room = room)

                if Set.isEmpty inRoom then
                    inRoom
                else
                    moveIntents
                    |> List.filter (fun i -> i.Rank <> idleRank)
                    |> List.fold (fun kept i -> Set.remove i.Pos kept) inRoom

            let blocked =
                here
                |> List.choose (fun (name, at) ->
                    if Set.contains name tired then Some at else None)
                |> Set.ofList
                |> Set.union foreign
                |> Set.union claimed

            // An idle body standing on a claimed tile asks for its other
            // tiles instead: its own is not a tile it may keep. With none it
            // stays, and the replayed step into it fails.
            let moveIntents =
                if Set.isEmpty claimed then
                    moveIntents
                else
                    moveIntents
                    |> List.map (fun i ->
                        if Set.contains i.Pos claimed then
                            match
                                i.Candidates
                                |> List.filter (fun tile -> not (Set.contains tile claimed))
                            with
                            | [] -> i
                            | rest -> { i with Candidates = rest }
                        else
                            i)

            room,
            {
                Standing = arbitrate occupants blocked moveIntents
                Preferences =
                    moveIntents
                    |> List.map (fun i -> i.Creep, List.head i.Candidates)
                    |> Map.ofList
                Blocked = blocked
                Occupants = occupants
            })
        |> Map.ofList

    // Every placed creep beside its room's pass, colony by colony and in
    // each colony's own view creep order: the order the Intents and
    // Verdicts leave in.
    let rows =
        movements
        |> List.collect (fun m ->
            let placed = m.Placed |> Map.ofList

            m.Order
            |> List.choose (fun name ->
                Map.tryFind name placed
                |> Option.map (fun at -> m, name, at, Map.find at.Room passOf)))

    let intents =
        rows
        |> List.choose (fun (_, name, at, pass) ->
            Map.tryFind name pass.Standing
            |> Option.bind (fun settled -> directionTo (RoomPos.pos at) (RoomPos.pos settled))
            |> Option.map (fun direction -> MoveCreep(name, direction)))

    // Who holds a tile this creep did not get, in this creep's room: the
    // creep settled on it, or the fatigued occupant whose blocked tile
    // pre-claimed it. A foreign body's tile is blocked and has no occupant
    // here, which is what leaves a creep stalled rather than yielded.
    let counterpartAt (pass: RoomPass) tile self =
        pass.Standing
        |> Map.tryPick (fun name settled ->
            if settled = tile && name <> self then Some name else None)
        |> Option.orElse (
            if Set.contains tile pass.Blocked then
                Map.tryFind tile pass.Occupants
            else
                None
        )

    let verdicts =
        rows
        |> List.collect (fun (movement, name, _, pass) ->
            if Set.contains name tired then
                [ Verdict.Grounded name ]
            else
                let reroute =
                    if Set.contains name movement.Rerouted then
                        [ Verdict.Rerouted name ]
                    else
                        []

                let yielded =
                    match Map.tryFind name pass.Preferences, Map.tryFind name pass.Standing with
                    | Some preferred, Some settled when settled <> preferred ->
                        match counterpartAt pass preferred name with
                        | Some other -> [ Verdict.Yielded(name, other) ]
                        | None -> [ Verdict.Stalled name ]
                    | _ -> []

                reroute @ yielded)

    intents, verdicts

/// Resolver, single colony: this colony's movement, arbitrated against nobody
/// else's.
let resolve
    (view: ColonyView)
    atlas
    (threats: Threats)
    (pool: PooledTask list)
    (assigned: Map<string, Task>)
    (crossings: Map<string, string>)
    (verbose: Set<string>)
    : Intent list * Verdict list =
    resolveRooms Set.empty [ movementOf view atlas threats pool assigned crossings verbose ]
