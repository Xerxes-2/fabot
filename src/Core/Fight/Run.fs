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
            Asked = false
            Threatens =
                group
                |> Option.exists (fun g -> homes.Contains g.Room || worked.Contains g.Room)
        }

    let stepped =
        fst (merged events state.Engagements)
        |> List.choose (fun e -> Defend.step (factsOf e) e)

    let targeted =
        stepped
        |> List.collect (fun e ->
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

        set.Contains

    // Past every id held last tick, so none closed this tick is reused by one opened.
    let firstId = 1 + (state.Engagements |> List.fold (fun most e -> max most e.Id) 0)

    let opened =
        Defend.opens homes worked armed world.Time targeted groups
        |> List.mapi (fun i (g, priority) ->
            {
                Id = firstId + i
                Group = g.Id
                Room = g.Room
                Ours = Set.empty
                Phase = Phase.Forming
                Since = world.Time
                Lost = 0
                Budget = 0
                Priority = priority
                Unanswered = true
            })

    [],
    { state with
        Groups = groups
        Engagements = stepped @ opened
    }

let fight: Module = { Name = "fight"; Run = decide }
