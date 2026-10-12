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
            match kept |> List.tryFind (fun k -> k.Group = e.Group) with
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
    let hostiles = world.Rooms |> Array.collect (fun r -> r.Hostiles)
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

    let engagements, into = merged events state.Engagements
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
            InReach = e.Ours |> Set.exists near
            Home =
                e.Ours
                |> Set.forall (fun n -> ours.ContainsKey n && homes.Contains ours.[n].Pos.Room)
            Held = homes.Contains e.Room
            Asked = waiting |> List.exists (fun c -> c.Engagement = e.Id)
            Threatens =
                group
                |> Option.exists (fun g -> homes.Contains g.Room || worked.Contains g.Room)
        }

    let stepped =
        engagements
        |> List.choose (fun e ->
            Defend.step (factsOf e) e |> Option.map (fun next -> next, next.Lost > e.Lost))

    let targeted =
        stepped
        |> List.collect (fun (e, _) ->
            e.Room
            :: (match byId.TryGetValue e.Group with
                | true, g -> [ g.Room ]
                | _ -> []))
        |> Set.ofList

    let armed =
        let set = HashSet<string>()

        for h in hostiles do
            if Hostiles.armed owned hostiles h then
                set.Add h.Id |> ignore

        set

    // Past every id held last tick, so none closed this tick is reused by one opened.
    let firstId = 1 + (state.Engagements |> List.fold (fun most e -> max most e.Id) 0)

    let opened =
        Defend.opens homes worked armed.Contains world.Time targeted groups
        |> List.mapi (fun i (g, priority) ->
            {
                Id = firstId + i
                Group = g.Id
                Room = g.Room
                Ours = Set.empty
                Plan = { Tactic = Tactic.Close; Order = [] }
                Phase = Phase.Forming
                Since = world.Time
                Lost = 0
                Budget = 0
                Priority = priority
                Unanswered = true
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
        |> Array.filter (fun c -> Tick.fights c.Name && not (committed.Contains c.Name))
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

    let spent (e: Engagement) =
        homes.Contains e.Room && e.Lost > e.Budget

    let priced (e: Engagement) : Engagement =
        match byId.TryGetValue e.Group with
        | _ when spent e -> e
        | true, g when rooms.ContainsKey g.Room && not e.Phase.IsDisengaging ->
            let room = rooms.[g.Room]

            let raid = room.Hostiles |> Array.filter (fun h -> armed.Contains h.Id)

            let theirs: Exchange.Exchange.Side =
                {
                    Bodies = raid |> Array.map Casting.fighterOf |> List.ofArray
                    Ramparts = Map.empty
                    Towers = []
                }

            let joining = List.ofSeq free

            let bodies =
                (Set.toList e.Ours @ joining)
                |> List.filter ours.ContainsKey
                |> List.map (fun n -> Casting.fighterOf ours.[n])

            let asked =
                casts
                |> Seq.filter (fun c -> c.Engagement = e.Id)
                |> Seq.map (fun c -> Exchange.Fighter.cast c.Body)
                |> List.ofSeq

            // A home's Defend is the home's alone.
            let colonies =
                world.Rooms
                |> Array.filter (fun r ->
                    homes.Contains r.Name
                    && (if homes.Contains e.Room then
                            r.Name = e.Room
                        else
                            Option.isSome (walkTo e.Room r.Name)))

            let capacity = colonies |> Array.fold (fun most r -> max most r.EnergyCapacity) 0
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

            match
                Casting.price stance (Casting.sideIn room (bodies @ asked)) theirs terrain capacity
            with
            | Some(set, dead, tactic) ->
                free.Clear()

                set
                |> List.iteri (fun i body ->
                    match Casting.colonyFor colonies world.Spawns queued (walkTo e.Room) body with
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

                let e =
                    { e with
                        Ours = Set.union e.Ours (Set.ofList joining)
                        Plan =
                            {
                                Tactic = tactic
                                Order = Casting.killOrder raid
                            }
                        Budget = e.Lost + dead
                        Unanswered = false
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
                { e with Unanswered = true }
            | None -> Defend.giveUp (homes.Contains e.Room) world.Time e
        | _ -> e

    // A home's Defend left open holds the home: a newcomer inside it is its
    // target now, with a fresh budget, so a second raid is not left to towers.
    let rehomed (e: Engagement) =
        let inHome (id: int) =
            byId.ContainsKey id
            && byId.[id].Room = e.Room
            && byId.[id].LastSeen = world.Time

        match
            events
            |> List.tryPick (fun event ->
                match event with
                | Tracker.Event.Newcomer id when
                    homes.Contains e.Room && inHome id && not (inHome e.Group)
                    ->
                    Some id
                | _ -> None)
        with
        | Some id -> { e with Group = id; Budget = e.Lost }
        | None -> e

    let engagements =
        stepped @ opened
        |> List.map (fun (e, lost) -> rehomed e, lost)
        |> List.sortBy (fun (e, _) -> -e.Priority, e.Id)
        |> List.map (fun (e, lost) ->
            if lost || changed e || e.Since = world.Time && e.Phase.IsForming then
                priced e
            else
                e)

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
        sight.Hostiles.[r.Name] <- r.Hostiles

        sight.Ramparts.[r.Name] <-
            r.Structures
            |> Array.filter (fun s -> s.Kind.IsRampart && s.Owner.IsOurs)
            |> Array.map (fun s -> s.Pos)

    let engagedIn = Dictionary<string, Engagement>()

    for e in engagements do
        for n in e.Ours do
            engagedIn.[n] <- e

    let fighters = world.Creeps |> Array.filter (fun c -> Tick.fights c.Name)

    let act (c: Creep) =
        match engagedIn.TryGetValue c.Name with
        | true, e ->
            let centre =
                match byId.TryGetValue e.Group with
                | true, g -> g.Centre
                | _ -> c.Pos

            let order = e.Plan.Order

            match e.Phase with
            | _ when e.Unanswered -> Tactics.rampart sight order c
            | Phase.Forming when homes.Contains e.Room -> Tactics.holdFast sight order c
            | Phase.Forming -> Tactics.muster sight order centre c
            | Phase.Pursuing -> Tactics.pursue sight order centre c
            | Phase.Disengaging -> Tactics.retreat sight c
            | Phase.Approaching
            | Phase.Fighting ->
                match e.Plan.Tactic with
                | Tactic.Kite when not (c.Body |> Array.exists (fun p -> p.Kind.IsAttack)) ->
                    Tactics.kite sight order centre c
                | Tactic.Hold -> Tactics.rampart sight order c
                | _ -> Tactics.close sight order centre c
        | _ -> Tactics.retreat sight c

    let acts = ResizeArray<Intent>()
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
    @ Tactics.heals fighters world.Creeps swinging shooting,
    { state with
        Groups = groups
        Engagements = engagements |> List.sortBy (fun e -> e.Id)
        Casts = casts
    }

let fight: Module = { Name = "fight"; Run = decide }
