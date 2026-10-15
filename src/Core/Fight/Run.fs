module Fabot.Core.Fight.Run

open System.Collections.Generic
open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Intents
open Fabot.Core.State
open Fabot.Core.Tick
open Fabot.Core.Fight.Shape
open Fabot.Core.Fight.Kinds

/// A merge keeps the older group's id; two engagements on one group are one,
/// the older surviving with both's bodies, losses and budget. Also each
/// merged id's survivor.
let private merged
    (events: Tracker.Event list)
    (engagements: Engagement list)
    : Engagement list * Map<int, int> =
    let retarget (e: Engagement) =
        events
        |> List.fold
            (fun (e: Engagement) event ->
                match event with
                | Tracker.Event.Merged(into, from) when e.Group = from -> { e with Group = into }
                | _ -> e)
            e

    engagements
    |> List.map retarget
    |> List.sortBy (fun e -> e.Id)
    |> List.fold
        (fun (kept: Engagement list, into: Map<int, int>) e ->
            match
                kept
                |> List.tryFind (fun k ->
                    k.Objective.IsDefend && e.Objective.IsDefend && k.Group = e.Group)
            with
            | Some older ->
                kept
                |> List.map (fun k ->
                    if k.Id = older.Id then
                        { k with
                            Ours = Set.union k.Ours e.Ours
                            Lost = k.Lost + e.Lost
                            Budget = k.Budget + e.Budget
                        }
                    else
                        k),
                Map.add e.Id older.Id into
            | None -> kept @ [ e ], into)
        ([], Map.empty)

let decide (context: Context) (state: State) : Intent list * State =
    let world = context.World
    let foes = Dictionary<string, Creep[]>()
    let cores = HashSet<string>()

    for r in world.Rooms do
        let razed =
            r.Structures
            |> Array.filter (fun s -> s.Kind.IsInvaderCore && s.Level = 0)
            |> Array.map Casting.coreAsHostile

        for c in razed do
            cores.Add c.Id |> ignore

        foes.[r.Name] <- Array.append r.Hostiles razed

    let hostiles = world.Rooms |> Array.collect (fun r -> foes.[r.Name])
    let owned = Hostiles.owned world.Rooms
    let held = state.Engagements |> List.fold (fun most e -> max most e.Group) 0

    let groups, events =
        Tracker.step Tracker.lostGroupTicks held owned state.Groups hostiles world.Time

    let homes = Tick.colonies world |> Set.ofArray

    let worked =
        context.Declarations.Outposts |> List.map (fun o -> o.Room) |> Set.ofList

    let byId = Dictionary<int, Group>()

    for g in groups do
        byId.[g.Id] <- g

    let hostileAt = Dictionary<string, Pos>()

    for h in hostiles do
        hostileAt.[h.Id] <- h.Pos

    let ours = Dictionary<string, Creep>()

    for c in world.Creeps do
        ours.[c.Name] <- c

    let standing = world.Creeps |> Array.map (fun c -> c.Name) |> Set.ofArray

    let agingOut =
        world.Creeps
        |> Array.filter (fun c -> c.TicksToLive <= 1)
        |> Array.map (fun c -> c.Name)
        |> Set.ofArray

    let engagements, into = merged events state.Engagements

    let armed =
        let set = HashSet<string>()

        for h in hostiles do
            if cores.Contains h.Id || Hostiles.armed owned hostiles h then
                set.Add h.Id |> ignore

        set

    // A Defend left open holds its room: a newcomer inside it is its target
    // now, with a fresh budget, where its own group is not there (a home's,
    // so a second raid is not left to towers) or was lost from sight.
    let rehomed (e: Engagement) =
        let inRoom (id: int) =
            byId.ContainsKey id
            && byId.[id].Room = e.Room
            && byId.[id].LastSeen = world.Time

        let gone =
            if homes.Contains e.Room then
                not (inRoom e.Group)
            else
                not (byId.ContainsKey e.Group)

        match
            events
            |> List.tryPick (fun event ->
                match event with
                | Tracker.Event.Newcomer id when
                    gone && inRoom id && byId.[id].Members |> Set.exists armed.Contains
                    ->
                    Some id
                | _ -> None)
        with
        | Some id ->
            { e with
                Group = id
                Opened = world.Time
                Budget = e.Lost
                Unanswered = true
                Predicted = None
            }
        | None -> e

    let engagements =
        engagements |> List.map (fun e -> if e.Objective.IsDefend then rehomed e else e)

    let alive = engagements |> List.map (fun e -> e.Id) |> Set.ofList

    let landed, waiting =
        state.Casts
        |> List.map (fun c ->
            match Map.tryFind c.Engagement into with
            | Some survivor -> { c with Engagement = survivor }
            | None -> c)
        |> List.filter (fun c ->
            alive.Contains c.Engagement
            && world.Rooms
               |> Array.exists (fun r ->
                   r.Name = c.Room && r.EnergyCapacity >= Engine.bodyCost c.Body))
        |> List.partition (fun c -> standing.Contains c.Name)

    let engagements =
        engagements
        |> List.map (fun e ->
            { e with
                Ours =
                    landed
                    |> List.fold
                        (fun o c -> if c.Engagement = e.Id then Set.add c.Name o else o)
                        e.Ours
            })

    let factsOf (e: Engagement) : Defend.Facts =
        let group =
            match byId.TryGetValue e.Group with
            | true, g -> Some g
            | _ -> None

        let near (name: string) =
            ours.ContainsKey name
            && group
               |> Option.exists (fun g ->
                   g.Members
                   |> Set.exists (fun m ->
                       hostileAt.ContainsKey m
                       && Pos.within Engine.rangedRange ours.[name].Pos hostileAt.[m]))

        {
            Time = world.Time
            Group = group
            Standing = standing
            AgingOut = agingOut
            InReach = e.Ours |> Set.exists near
            Home =
                e.Ours
                |> Set.forall (fun n -> ours.ContainsKey n && homes.Contains ours.[n].Pos.Room)
            Held = homes.Contains e.Room
            Asked = waiting |> List.exists (fun c -> c.Engagement = e.Id)
            Threatens =
                group
                |> Option.exists (fun g -> homes.Contains g.Room || worked.Contains g.Room)
            // A core in sight stands in a group of its own, so a room in sight
            // holds open only the engagement on that group.
            Clear =
                Sightings.clear context.Sightings world.Time e.Room
                || foes.ContainsKey e.Room && Sightings.unarmed context.Sightings world.Time e.Room
        }

    let rival (s: Structure) =
        not s.Owner.IsOurs && (s.Kind.IsRampart || s.Kind.IsWall)

    let inSight (room: string) =
        world.Rooms |> Array.tryFind (fun r -> r.Name = room)

    let barrierAt (room: Room) =
        room.Structures
        |> Array.filter rival
        |> Array.map (fun s -> s.Id, s)
        |> Map.ofArray

    // A planned barrier fallen advances the plan, never re-prices it.
    let engagements =
        engagements
        |> List.map (fun e ->
            match e.Objective, inSight e.Room with
            | Objective.Siege _, Some room ->
                let standing = barrierAt room

                { e with
                    Plan =
                        { e.Plan with
                            Order = e.Plan.Order |> List.filter standing.ContainsKey
                        }
                }
            | _ -> e)

    let nextBarrier (e: Engagement) =
        inSight e.Room
        |> Option.bind (fun room ->
            e.Plan.Order
            |> List.tryHead
            |> Option.bind (fun id -> Map.tryFind id (barrierAt room)))

    let siegeFacts (e: Engagement) (target: string) : Siege.Facts =
        {
            Time = world.Time
            Standing = standing
            AgingOut = agingOut
            Asked = waiting |> List.exists (fun c -> c.Engagement = e.Id)
            Home =
                e.Ours
                |> Set.forall (fun n -> ours.ContainsKey n && homes.Contains ours.[n].Pos.Room)
            UnderFire =
                e.Ours |> Set.exists (fun n -> ours.ContainsKey n && ours.[n].Pos.Room = e.Room)
            Open =
                inSight e.Room
                |> Option.exists (fun room ->
                    not e.Unanswered && e.Plan.Order.IsEmpty
                    || room.Structures |> Array.forall (fun s -> s.Id <> target))
        }

    let stepped =
        engagements
        |> List.choose (fun e ->
            match e.Objective with
            | Objective.Defend -> Defend.step (factsOf e) e
            | Objective.Siege target -> Siege.step (siegeFacts e target) e
            |> Option.map (fun next -> next, next.Ours.Count < e.Ours.Count))

    let targeted =
        stepped
        |> List.collect (fun (e, _) ->
            e.Room
            :: (match byId.TryGetValue e.Group with
                | true, g -> [ g.Room ]
                | _ -> []))
        |> Set.ofList

    // Past every id held last tick, so none closed this tick is reused by one opened.
    let firstId = 1 + (state.Engagements |> List.fold (fun most e -> max most e.Id) 0)

    let opened =
        Defend.opens homes worked armed.Contains world.Time targeted groups
        |> List.mapi (fun i (g, priority) ->
            {
                Id = firstId + i
                Objective = Objective.Defend
                Group = g.Id
                Room = g.Room
                Ours = Set.empty
                Plan = { Tactic = Tactic.Close; Order = [] }
                Phase = Phase.Forming
                Opened = world.Time
                Since = world.Time
                Lost = 0
                Budget = 0
                Priority = priority
                Unanswered = true
                Capacity = 0
                Predicted = None
            },
            true)

    let changed (e: Engagement) =
        events
        |> List.exists (fun event ->
            match event with
            | Tracker.Event.Split(from, into) -> from = e.Group || into = e.Group
            | Tracker.Event.Merged(into, _) -> into = e.Group
            | Tracker.Event.Left id -> id = e.Group
            | Tracker.Event.Newcomer id -> byId.ContainsKey id && byId.[id].Room = e.Room
            | _ -> false)

    let free =
        let committed =
            stepped |> List.fold (fun all (e, _) -> Set.union all e.Ours) Set.empty

        world.Creeps
        |> Array.filter (fun c ->
            Tick.fights c.Name
            && not (committed.Contains c.Name || agingOut.Contains c.Name))
        |> Array.map (fun c -> c.Name)
        |> ResizeArray

    let rooms = Dictionary<string, Room>()

    for r in world.Rooms do
        rooms.[r.Name] <- r

    let casts = ResizeArray<Cast>(waiting)
    let queued = Dictionary<string, int>()

    let queue (room: string) (body: Exchange.Exchange.Body) =
        queued.[room] <-
            (if queued.ContainsKey room then queued.[room] else 0)
            + Engine.spawnTicksPerPart * List.length body

    for c in waiting do
        queue c.Room c.Body

    let walkTo (room: string) (from: string) =
        Atlas.route context.Atlas Set.empty Tick.reachHops from room
        |> Option.map (fun rooms -> Engine.roomSide * (List.length rooms - 1))

    // A home's Defend is the home's alone.
    let coloniesOf (e: Engagement) =
        world.Rooms
        |> Array.filter (fun r ->
            homes.Contains r.Name
            && (if homes.Contains e.Room then
                    r.Name = e.Room
                else
                    Option.isSome (walkTo e.Room r.Name)))

    let capacityOf (e: Engagement) =
        coloniesOf e |> Array.fold (fun most r -> max most r.EnergyCapacity) 0

    let spent (e: Engagement) =
        homes.Contains e.Room && e.Lost > e.Budget

    let ask (e: Engagement) (set: Exchange.Exchange.Body list) =
        set
        |> List.iteri (fun i body ->
            match Casting.colonyFor (coloniesOf e) world.Spawns queued (walkTo e.Room) body with
            | Some colony ->
                queue colony body

                casts.Add
                    {
                        Engagement = e.Id
                        Name = $"{Tick.fightPrefix}{world.Time}-{e.Id}-{i}"
                        Room = colony
                        Body = body
                    }
            | None -> ())

    let membersOf (e: Engagement) =
        e.Ours
        |> Seq.filter ours.ContainsKey
        |> Seq.map (fun n -> ours.[n])
        |> Array.ofSeq

    let towersIn (room: Room) =
        room.Structures |> Array.filter (fun s -> s.Kind.IsTower && not s.Owner.IsOurs)

    let breachIn
        (room: Room)
        (target: Structure)
        (colony: string)
        (set: Exchange.Fighter.Fighter list)
        =
        let barriers = barrierAt room |> Map.values |> Array.ofSeq
        let line = barriers |> Array.map _.Pos

        let entry =
            Atlas.route context.Atlas Set.empty Tick.reachHops colony room.Name
            |> Option.bind (fun rooms ->
                [ Tiles.top; Tiles.right; Tiles.bottom; Tiles.left ]
                |> List.tryFind (fun exit ->
                    rooms.Length >= 2
                    && Tiles.roomToward room.Name exit = Some rooms.[rooms.Length - 2]))

        let gait =
            set
            |> List.map _.Gait
            |> List.filter (fun g -> g.Moves > 0)
            |> List.sortBy (fun g -> -(Grid.stepTicks g Engine.plainWeight))
            |> List.tryHead

        match Atlas.weights context.Atlas room.Name, entry, gait with
        | Some weights, Some exit, Some gait ->
            for b in barriers do
                Tiles.ofPos b.Pos
                |> fun t ->
                    if
                        room.Structures
                        |> Array.forall (fun s -> s.Pos <> b.Pos || s.Walkable || rival s)
                    then
                        weights.[t] <-
                            if Terrain.isSwamp room.Terrain b.Pos.X b.Pos.Y then
                                Engine.swampWeight
                            else
                                Engine.plainWeight

            let ground: Exchange.Exchange.Ground =
                {
                    Room = room.Name
                    Weights = weights
                    Barriers =
                        barriers
                        |> Array.groupBy (fun b -> Tiles.ofPos b.Pos)
                        |> Array.map (fun (t, on) ->
                            t, Array.sumBy (fun (b: Structure) -> b.Hits) on)
                        |> Map.ofArray
                }

            let repair =
                Engine.repairPower
                * Array.sumBy (fun h -> (Casting.fighterOf h).Work) foes.[room.Name]

            let rate =
                Engine.dismantlePower
                * List.sumBy (fun (f: Exchange.Fighter.Fighter) -> f.Work) set
                - repair

            let towers = towersIn room |> Array.map _.Pos |> List.ofArray

            let attempt seated =
                Exchange.Exchange.breachPlan
                    ground
                    gait
                    towers
                    (Exchange.Exchange.pooledHeal seated set)
                    rate
                    (Tiles.exitTiles exit |> Array.filter (fun t -> weights.[t] >= 0))
                    (Tiles.around (Tiles.ofPos target.Pos))

            let seatsOf (plan: (int * int) list, _) =
                match plan with
                | (_, work) :: _ -> Tactics.seatsAt context.Atlas line room.Name work
                | [] -> set.Length

            // Every barrier on a tile, its rampart first: the tile stands
            // in the plan until it is walkable.
            let idsAt (tile: int) =
                barriers
                |> Array.filter (fun b -> Tiles.ofPos b.Pos = tile)
                |> Array.sortBy (fun b -> not b.Kind.IsRampart, b.Id)
                |> Array.map _.Id
                |> List.ofArray

            // Seats follow the planned barrier: priced with every medic beside
            // the struck body, then with those its seats hold.
            attempt set.Length
            |> Option.bind (fun first ->
                let seats = seatsOf first
                attempt seats |> Option.filter (fun plan -> seatsOf plan >= seats))
            |> Option.map (fun (plan, ticks) ->
                List.collect (fst >> idsAt) plan,
                Exchange.Exchange.Win(ticks, { Dead = 0; Hits = 0 }))
        | _ -> None

    // Whole, as cast: a part struck off is healed back, and only a body lost
    // re-prices the set.
    let fielded (e: Engagement) =
        (e.Ours
         |> Set.toList
         |> List.filter ours.ContainsKey
         |> List.map (fun n -> ours.[n].Body |> Array.map _.Kind |> List.ofArray))
        @ (waiting |> List.filter (fun c -> c.Engagement = e.Id) |> List.map _.Body)
        |> List.map Exchange.Fighter.cast

    // Priced at the first sight of its room, where an offensive kind no set
    // the bank buys staffs closes (§6.2); after, only on an event.
    let besieged (e: Engagement) (target: string) (left: bool) : Engagement option =
        let colony = coloniesOf e |> Array.map _.Name |> Array.sort |> Array.tryHead

        match inSight e.Room, colony with
        | Some room, Some colony ->
            match room.Structures |> Array.tryFind (fun s -> s.Id = target) with
            | None -> Some e
            | Some target when e.Unanswered ->
                let capacity = capacityOf e

                match
                    Casting.siege
                        capacity
                        (List.map Exchange.Fighter.cast >> breachIn room target colony)
                with
                // A way in with nothing to break is no siege: closed before it casts.
                | Some(set, (order, outcome)) when not order.IsEmpty ->
                    ask e set

                    Some
                        { e with
                            Plan =
                                {
                                    Tactic = Tactic.Blob Tactic.Close
                                    Order = order
                                }
                            Budget = e.Lost
                            Unanswered = false
                            Capacity = capacity
                            Predicted = Some outcome
                        }
                | _ -> None
            | Some _ when e.Phase.IsDisengaging -> Some e
            | Some target ->
                let set = fielded e
                let inRoom = membersOf e |> Array.filter (fun m -> m.Pos.Room = room.Name)

                let struck (b: Structure) =
                    Exchange.Exchange.towerStrike
                        (towersIn room |> Array.map _.Pos |> List.ofArray)
                        b.Pos

                let unreached (b: Structure) =
                    inRoom.Length > 0
                    && inRoom
                       |> Array.forall (fun m ->
                           Atlas.field
                               context.Atlas
                               {
                                   Footprint = Grid.One
                                   Factor = Grid.factorOf m
                               }
                               {
                                   Room = room.Name
                                   Tiles = [| Tiles.ofPos b.Pos |]
                                   Range = Engine.meleeRange
                                   Avoid = Set.empty
                                   MaxHops = Tick.reachHops
                               }
                               room.Name
                           |> Option.bind (fun field -> Field.ticks field (Tiles.ofPos m.Pos))
                           |> Option.isNone)

                let late =
                    match e.Predicted with
                    | Some(Exchange.Exchange.Win(ticks, _)) ->
                        e.Phase.IsFighting && world.Time > e.Since + ticks
                    | _ -> false

                let event =
                    left
                    || landed |> List.exists (fun c -> c.Engagement = e.Id)
                    || late
                    || nextBarrier e
                       |> Option.exists (fun b ->
                           struck b > Exchange.Exchange.pooledHeal set.Length set || unreached b)

                if not event then
                    Some e
                else
                    // Once: a new plan whose first barrier the set still cannot
                    // reach is no plan, or the event would fire every tick.
                    let reached (order: string list) =
                        match order with
                        | first :: _ ->
                            Map.tryFind first (barrierAt room) |> Option.exists (unreached >> not)
                        | [] -> true

                    match breachIn room target colony set with
                    | Some(order, outcome) ->
                        { Siege.replanned world.Time (reached order) e with
                            Plan = { e.Plan with Order = order }
                            Predicted = Some outcome
                        }
                        |> Some
                    | None -> Some(Siege.replanned world.Time false e)
        | _ -> Some e

    let priced (e: Engagement) : Engagement =
        match byId.TryGetValue e.Group with
        | _ when spent e -> e
        | true, g when rooms.ContainsKey g.Room && not e.Phase.IsDisengaging ->
            let room = rooms.[g.Room]

            let raid = foes.[room.Name] |> Array.filter (fun h -> armed.Contains h.Id)

            let theirs: Exchange.Exchange.Side =
                {
                    Bodies =
                        raid
                        |> Array.filter (fun h -> not (cores.Contains h.Id))
                        |> Array.map Casting.fighterOf
                        |> List.ofArray
                    Ramparts = Map.empty
                    Towers = []
                }

            let joining =
                free
                |> Seq.filter (fun n -> (walkTo e.Room ours.[n].Pos.Room).IsSome)
                |> List.ofSeq

            let fielding =
                (Set.toList e.Ours @ joining)
                |> List.filter ours.ContainsKey
                |> List.map (fun n -> ours.[n])

            let bodies = fielding |> List.map Casting.fighterOf

            let askedBodies =
                casts
                |> Seq.filter (fun c -> c.Engagement = e.Id)
                |> Seq.map _.Body
                |> List.ofSeq

            let asked = askedBodies |> List.map Exchange.Fighter.cast

            let coreHits =
                raid |> Array.filter (fun h -> cores.Contains h.Id) |> Array.sumBy _.Hits

            let colonies = coloniesOf e
            let capacity = capacityOf e
            let terrain = Casting.terrainAt room g.Centre

            // A home is a post the raid comes to where our ramparts stand on it;
            // with none, a Hold stands still while the raid stands off.
            let stance =
                if
                    homes.Contains e.Room
                    && g.Room = e.Room
                    && room.Structures |> Array.exists (fun s -> s.Kind.IsRampart && s.Owner.IsOurs)
                then
                    Exchange.Exchange.Stance.Hold
                else
                    Exchange.Exchange.Stance.Standoff

            // An outpost is worth the yield the raid's life would stand it down
            // for; a core is the razer's and holds nothing at the wave's cost.
            let copiesOf, tickWorth =
                match context.Declarations.Outposts |> List.tryFind (fun o -> o.Room = e.Room) with
                | Some outpost when not (homes.Contains e.Room) ->
                    let tickWorth = outpost.Sources.Length * Engine.heldOutputPerTick

                    let ticks =
                        raid
                        |> Array.filter (fun h -> not (cores.Contains h.Id))
                        |> Array.fold (fun most h -> max most h.TicksToLive) 0

                    let spawns =
                        world.Spawns
                        |> Array.filter (fun s ->
                            colonies |> Array.exists (fun r -> r.Name = s.Pos.Room))
                        |> Array.length

                    let razing = Casting.razer capacity coreHits []

                    let fielded =
                        (fielding |> List.map (fun c -> c.Body |> Array.map _.Kind |> List.ofArray))
                        @ askedBodies
                        |> List.filter (fun b -> not (List.contains b razing))
                        |> List.sumBy Engine.bodyCost

                    Casting.copiesWithin
                        (tickWorth * ticks - fielded)
                        (spawns * ticks
                         - Engine.spawnTicksPerPart * List.sumBy List.length askedBodies),
                    tickWorth
                | _ -> Casting.homeCopies, 0

            match
                Casting.price
                    stance
                    (Casting.sideIn room (bodies @ asked))
                    theirs
                    terrain
                    capacity
                    copiesOf
                    tickWorth
            with
            | Some(set, outcome, tactic) ->
                for n in joining do
                    free.Remove n |> ignore

                let set =
                    set
                    @ Casting.razer
                        capacity
                        coreHits
                        (bodies @ asked @ List.map Exchange.Fighter.cast set)

                ask e set

                let e =
                    { e with
                        Ours = Set.union e.Ours (Set.ofList joining)
                        Plan =
                            {
                                Tactic = tactic
                                Order = Casting.killOrder raid
                            }
                        Budget =
                            e.Lost
                            + (match outcome with
                               | Exchange.Exchange.Win(_, losses) -> losses.Dead
                               | _ -> 0)
                        Unanswered = false
                        Capacity = capacity
                        // A core alone is no fight Exchange predicts (§10.1).
                        Predicted =
                            if not e.Unanswered then e.Predicted
                            elif theirs.Bodies.IsEmpty then None
                            else Some outcome
                    }

                let standing =
                    e.Ours
                    |> Set.toList
                    |> List.filter ours.ContainsKey
                    |> List.map (fun n -> Casting.fighterOf ours.[n])

                if Casting.wins stance (Casting.sideIn room standing) theirs terrain then
                    Defend.standsAlone world.Time e
                else
                    e
            | None when e.Unanswered || e.Ours.IsEmpty && asked.IsEmpty ->
                { e with
                    Unanswered = true
                    Capacity = capacity
                }
            | None -> Defend.giveUp (homes.Contains e.Room) world.Time e
        | _ -> e

    let engagements =
        stepped @ opened
        |> List.sortBy (fun (e, _) -> -e.Priority, e.Id)
        |> List.choose (fun (e, left) ->
            let grew = e.Unanswered && capacityOf e > e.Capacity

            match e.Objective with
            | Objective.Siege target -> besieged e target left
            | Objective.Defend when
                left || grew || changed e || e.Since = world.Time && e.Phase.IsForming
                ->
                Some(priced e)
            | Objective.Defend -> Some e)

    let kept =
        engagements
        |> List.filter (fun e -> not (e.Phase.IsDisengaging || spent e))
        |> List.map (fun e -> e.Id)
        |> Set.ofList

    let casts = casts |> Seq.filter (fun c -> kept.Contains c.Engagement) |> List.ofSeq

    let ovens =
        world.Spawns
        |> Array.choose (fun s -> s.Spawning |> Option.map (fun o -> o.Name))
        |> Set.ofArray

    let sight: Tactics.Sight =
        {
            Atlas = context.Atlas
            Hostiles = Dictionary()
            Ramparts = Dictionary()
            Homes =
                world.Spawns
                |> Array.filter (fun s -> homes.Contains s.Pos.Room)
                |> Array.map (fun s -> s.Pos)
        }

    for r in world.Rooms do
        sight.Hostiles.[r.Name] <- foes.[r.Name]

        sight.Ramparts.[r.Name] <-
            r.Structures
            |> Array.filter (fun s -> s.Kind.IsRampart && s.Owner.IsOurs)
            |> Array.map (fun s -> s.Pos)

    let engagedIn = Dictionary<string, Engagement>()

    for e in engagements do
        for n in e.Ours do
            engagedIn.[n] <- e

    let fighters = world.Creeps |> Array.filter (fun c -> Tick.fights c.Name)

    let swings (c: Creep) =
        c.Body |> Array.exists (fun p -> p.Kind.IsAttack)

    // A group lost from sight is sought in its room, which brings it back in sight.
    let centreOf (e: Engagement) =
        match e.Objective, byId.TryGetValue e.Group, nextBarrier e with
        | Objective.Siege _, _, Some b -> b.Pos
        | Objective.Defend, (true, g), _ -> g.Centre
        | _ ->
            {
                Room = e.Room
                X = Engine.roomSide / 2
                Y = Engine.roomSide / 2
            }

    let walking = Dictionary<string, Move.Wish>()

    for e in engagements do
        if e.Phase.IsApproaching && e.Plan.Tactic.IsBlob && not e.Unanswered then
            let members = membersOf e |> Array.filter (swings >> not)

            if members.Length > 0 then
                for wish in Tactics.blob sight (centreOf e) members do
                    walking.[wish.Creep] <- wish

    let fronts = Dictionary<int, Tactics.Front>()

    for e in engagements do
        match e.Objective, inSight e.Room, nextBarrier e with
        | Objective.Siege _, Some room, Some wall ->
            let towers =
                towersIn room
                |> Array.filter (fun s -> Store.amountOf Energy s.Store >= Engine.towerEnergyCost)
                |> Array.map _.Pos

            fronts.[e.Id] <-
                {
                    Breach = wall.Pos
                    Wall = wall.Id
                    Line = room.Structures |> Array.filter rival |> Array.map _.Pos
                    Towers = towers
                    Strike =
                        towers
                        |> Array.sumBy (fun t ->
                            Pos.range t wall.Pos
                            |> Option.map Engine.towerAttackAt
                            |> Option.defaultValue 0)
                }
        | _ -> ()

    let act (c: Creep) =
        match engagedIn.TryGetValue c.Name with
        | true, e when e.Phase.IsFighting && fronts.ContainsKey e.Id ->
            Tactics.siege sight e.Plan.Order fronts.[e.Id] (membersOf e) c
        | true, e ->
            let centre = centreOf e
            let order = e.Plan.Order

            match e.Phase with
            | _ when e.Unanswered -> Tactics.rampart sight order c
            | Phase.Forming -> Tactics.holdFast sight order c
            | Phase.Pursuing -> Tactics.pursue sight order centre c
            | Phase.Disengaging -> Tactics.retreat sight c
            | Phase.Approaching
            | Phase.Fighting ->
                match e.Plan.Tactic with
                | _ when walking.ContainsKey c.Name ->
                    Tactics.strike sight order c, walking.[c.Name]
                | Tactic.Blob Tactic.Close when Tactics.exposed sight centre.Room (membersOf e) c ->
                    Tactics.strike sight order c, Tactics.stepBack sight centre.Room (membersOf e) c
                | Tactic.Kite
                | Tactic.Blob Tactic.Kite when not (swings c) -> Tactics.kite sight order centre c
                | Tactic.Hold -> Tactics.rampart sight order c
                | _ -> Tactics.close sight order centre c
        | _ -> Tactics.retreat sight c

    let lastResort =
        homes
        |> Seq.choose (fun name ->
            let room = rooms.[name]
            let ours = room.Structures |> Array.filter (fun s -> s.Owner.IsOurs)

            let raiding =
                engagements
                |> List.filter (fun e -> e.Unanswered && e.Room = name && byId.ContainsKey e.Group)

            let raid =
                raiding
                |> List.collect (fun e -> Set.toList byId.[e.Group].Members)
                |> Set.ofList

            room.Controller
            |> Option.map (fun c ->
                ({
                    Controller = c
                    Towered = ours |> Array.exists (fun s -> s.Kind.IsTower)
                    Raid = room.Hostiles |> Array.filter (fun h -> raid.Contains h.Id)
                    Hostiles = room.Hostiles
                    Ours = ours
                    Bodies = world.Creeps |> Array.filter (fun c -> c.Pos.Room = name)
                    Time = world.Time
                    Opened =
                        raiding
                        |> List.filter (fun e ->
                            room.Hostiles
                            |> Array.exists (fun h ->
                                h.Owner <> Theirs Engine.invaderUser
                                && byId.[e.Group].Members.Contains h.Id))
                        |> List.fold (fun first e -> min first e.Opened) world.Time
                    Fallen = room.Remains |> Array.choose (fun r -> r.Fallen)
                }
                : Defend.Home)))
        |> List.ofSeq

    let running =
        world.Rooms
        |> Array.exists (fun r ->
            r.Controller |> Option.exists (fun c -> c.Owner.IsOurs && c.SafeMode > 0))

    let acts = ResizeArray<Intent>()

    match Defend.safeMode running lastResort with
    | Some id -> acts.Add(Controller(id, ActivateSafeMode))
    | None -> ()

    let incoming =
        engagements
        |> List.choose (fun e ->
            match fronts.TryGetValue e.Id with
            | true, front ->
                Tactics.focusOf
                    front
                    (membersOf e |> Array.filter (fun m -> m.Pos.Room = front.Breach.Room))
                |> Option.map (fun focus -> focus.Name, front.Strike)
            | _ -> None)
        |> Map.ofList

    let swinging = HashSet<string>()
    let shooting = HashSet<string>()

    for c in fighters do
        let strike, wish = act c

        match strike with
        | Some s ->
            if s.IsAttackCreep then
                swinging.Add c.Name |> ignore
            else
                shooting.Add c.Name |> ignore

            acts.Add(Creep(c.Name, s))
        | None -> ()

        acts.Add(Intent.Wish(c.Pos.Room, wish))

    (casts
     |> List.filter (fun c -> not (ovens.Contains c.Name))
     |> List.map (fun c ->
         Queue(
             c.Room,
             {
                 Body = c.Body
                 Name = c.Name
                 Rank = Rank.Fight
             }
         )))
    @ List.ofSeq acts
    @ Tactics.heals fighters world.Creeps incoming swinging shooting,
    { state with
        Groups = groups
        Engagements = engagements |> List.sortBy (fun e -> e.Id)
        Casts = casts
    }

let fight: Module = { Name = "fight"; Run = decide }
