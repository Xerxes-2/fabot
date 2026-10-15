/// A throw in one module costs that module this tick and nothing else
/// (`ARCHITECTURE.md` §2). The clock is passed in to keep `decide` deterministic.
module Fabot.Core.Tick

open Fabot.Core.World
open Fabot.Core.Intents
open Fabot.Core.State

type Declared = { Id: string; Pos: Pos }

/// What is known of a room before it is seen.
type Outpost =
    {
        Room: string
        Controller: Declared
        Sources: Declared list
    }

type Declarations =
    {
        /// The creep lifetimes of tower fire a colony's reserve holds.
        ReserveLifetimes: float
        Outposts: Outpost list
    }

let undeclared =
    {
        ReserveLifetimes = 2.0
        Outposts = []
    }

[<NoEquality; NoComparison>]
type Context =
    {
        World: World
        Atlas: Atlas
        Declarations: Declarations
        Sightings: Sightings
    }

/// A body cast under this name is Fight's for its life, so no creep is in
/// both Fight and Economy (`ARCHITECTURE.md` §2).
let fightPrefix = "fight-"

let fights (name: string) = name.StartsWith fightPrefix

/// The route search needs a bound; nothing is worked further than this.
let reachHops = 3

let targetAt (pos: Pos) (range: int) : Target =
    {
        Room = pos.Room
        Tiles = [| Tiles.ofPos pos |]
        Range = range
        Avoid = Set.empty
        MaxHops = reachHops
    }

/// Ticks and crossings are not one unit.
let nearest
    (atlas: Atlas)
    (route: Target -> string -> string list option)
    (homes: Pos[])
    (can: int -> bool)
    (at: Pos)
    : int option =
    let walker =
        {
            Footprint = Grid.One
            Factor = Grid.loadedFactor [ BodyPart.Move ]
        }

    let least (measure: Pos -> int option) =
        Array.init homes.Length id
        |> Array.filter can
        |> Array.choose (fun i -> measure homes.[i] |> Option.map (fun d -> d, i))
        |> Array.sort
        |> Array.tryHead
        |> Option.map snd

    least (fun home -> Atlas.ticksTo atlas route walker (targetAt home 1) at)
    |> Option.orElseWith (fun () ->
        least (fun home ->
            route (targetAt at 0) home.Room
            |> Option.map (fun rooms -> Atlas.crossingTicks * (List.length rooms - 1))))

let colonies (world: World) : string[] =
    world.Rooms
    |> Array.filter (fun r ->
        r.Controller |> Option.exists (fun c -> c.Owner.IsOurs)
        && world.Spawns |> Array.exists (fun s -> s.Pos.Room = r.Name))
    |> Array.map (fun r -> r.Name)

/// The declared outposts a colony's spawn reaches and no hostile seen there
/// outlives (`ARCHITECTURE.md` §13, M1b): a stood-down room is no ground.
let worked (context: Context) : Outpost list =
    let trees =
        colonies context.World
        |> Array.map (fun home -> home, Atlas.tree context.Atlas reachHops home)

    context.Declarations.Outposts
    |> List.filter (fun o ->
        Sightings.unarmed context.Sightings context.World.Time o.Room
        && trees
           |> Array.exists (fun (home, tree) -> home = o.Room || tree.ContainsKey o.Room))

type Module =
    {
        Name: string
        Run: Context -> State -> Intent list * State
    }

type Outcome =
    | Ran
    | Off
    | Faulted of message: string

type Report =
    {
        Module: string
        Cpu: float
        Outcome: Outcome
    }

type Result =
    {
        Intents: Intent list
        State: State
        Reports: Report list
    }

let private byRoom (pairs: (string * 'T) list) : (string * 'T[])[] =
    let pairs = Array.ofList pairs
    let groups = System.Collections.Generic.Dictionary<string, ResizeArray<'T>>()
    let rooms = ResizeArray<string>()

    for i in 0 .. pairs.Length - 1 do
        let room, value = Tiles.at i pairs

        if not (groups.ContainsKey room) then
            groups.[room] <- ResizeArray()
            rooms.Add room

        groups.[room].Add value

    rooms.ToArray() |> Array.map (fun room -> room, groups.[room].ToArray())

/// Each room's spawn queue (§6.5): floors, then the fight's asks, then the
/// economy's, on its idle spawns while the bank pays; a fight ask the bank
/// cannot pay yet holds the economy's behind it, one its capacity never pays
/// holds nothing.
let queue (world: World) (asks: (string * Ask) list) : Intent list =
    let rank (a: Ask) =
        match a.Rank with
        | Rank.Floor -> 0
        | Rank.Fight -> 1
        | Rank.Economy -> 2

    let rec serve (idle: Spawn list) (capacity: int) (bank: int) (held: bool) (asks: Ask list) =
        match idle, asks with
        | [], _
        | _, [] -> []
        | _, a :: _ when held && a.Rank.IsEconomy -> []
        | spawn :: rest, a :: more when Engine.bodyCost a.Body <= bank ->
            Spawn(spawn.Id, SpawnCreep(a.Body, a.Name))
            :: serve rest capacity (bank - Engine.bodyCost a.Body) held more
        | _, a :: more ->
            serve
                idle
                capacity
                bank
                (held || a.Rank.IsFight && Engine.bodyCost a.Body <= capacity)
                more

    byRoom asks
    |> List.ofArray
    |> List.collect (fun (room, asks) ->
        match world.Rooms |> Array.tryFind (fun r -> r.Name = room) with
        | Some r ->
            let idle =
                world.Spawns
                |> Array.filter (fun s -> s.Pos.Room = room && s.Spawning.IsNone)
                |> List.ofArray

            serve
                idle
                r.EnergyCapacity
                r.EnergyAvailable
                false
                (List.ofArray asks |> List.sortBy rank)
        | None -> [])

/// One arbitration per room over every module's wishes (§3's `Move`); a creep
/// of ours that wishes nothing holds its tile.
let arbitrate (context: Context) (wishes: (string * Move.Wish) list) : Intent list =
    let wished = System.Collections.Generic.HashSet<string>()
    let wishes = byRoom wishes

    for _, ws in wishes do
        for w in ws do
            wished.Add w.Creep |> ignore

    wishes
    |> List.ofArray
    |> List.collect (fun (room, ws) ->
        let held =
            context.World.Creeps
            |> Array.filter (fun c -> c.Pos.Room = room && not (wished.Contains c.Name))
            |> Array.map (fun c -> Tiles.ofPos c.Pos)

        Move.Arbitrate.arbitrate (Atlas.passable context.Atlas room) held (List.ofArray ws)
        |> List.map (fun (name, direction) -> Creep(name, CreepAct.Move direction)))

let private settle (context: Context) (intents: Intent list) : Intent list =
    let asks =
        intents
        |> List.choose (fun i ->
            match i with
            | Queue(room, ask) -> Some(room, ask)
            | _ -> None)

    let wishes =
        intents
        |> List.choose (fun i ->
            match i with
            | Wish(room, wish) -> Some(room, wish)
            | _ -> None)

    queue context.World asks
    @ (intents |> List.filter (fun i -> not (i.IsQueue || i.IsWish)))
    @ arbitrate context wishes

let run (clock: unit -> float) (modules: Module list) (context: Context) (state: State) : Result =
    let step (intents, state, reports) (m: Module) =
        if Set.contains m.Name state.Off then
            intents,
            state,
            {
                Module = m.Name
                Cpu = 0.0
                Outcome = Off
            }
            :: reports
        else
            let start = clock ()

            let produced, next, outcome =
                try
                    let produced, next = m.Run context state
                    produced, next, Ran
                with error ->
                    [], state, Faulted error.Message

            let report =
                {
                    Module = m.Name
                    Cpu = clock () - start
                    Outcome = outcome
                }

            List.rev produced @ intents, next, report :: reports

    let intents, state, reports = List.fold step ([], state, []) modules
    let intents = List.rev intents
    let start = clock ()

    // A fault settling the fight's asks and wishes costs only the fight's.
    let fights (i: Intent) =
        match i with
        | Queue(_, ask) -> ask.Rank.IsFight
        | Wish(_, wish) -> fights wish.Creep
        | _ -> false

    let settled, outcome =
        try
            settle context intents, Ran
        with error ->
            try
                settle context (intents |> List.filter (fights >> not)), Faulted error.Message
            with error ->
                intents |> List.filter (fun i -> not (i.IsQueue || i.IsWish)), Faulted error.Message

    {
        Intents = settled
        State = state
        Reports =
            List.rev (
                {
                    Module = "settle"
                    Cpu = clock () - start
                    Outcome = outcome
                }
                :: reports
            )
    }
