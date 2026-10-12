/// A soak node's structures (`ARCHITECTURE.md` §10.2): the Layout's plan for
/// a room, built at a level the way the bot's sites would build it with every
/// site finished at once. Reads the room on stdin as JSON
/// `{ room, terrain, sources: [[x, y]], minerals: [[x, y]], controller: [x, y], level }`
/// (terrain: 2500 digits, the raw buffer's), writes `[{ kind, x, y }]` in
/// build order, the first spawn first.
module Fabot.Plan.Program

open System
open System.Text.Json
open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Layout

[<EntryPoint>]
let main _ =
    let input = JsonDocument.Parse(Console.In.ReadToEnd()).RootElement
    let name = input.GetProperty("room").GetString()
    let level = input.GetProperty("level").GetInt32()

    let pos (e: JsonElement) : Pos =
        {
            Room = name
            X = e.[0].GetInt32()
            Y = e.[1].GetInt32()
        }

    let each (key: string) =
        input.GetProperty(key).EnumerateArray() |> Seq.map pos |> Array.ofSeq

    let room: Room =
        {
            Name = name
            Terrain =
                input.GetProperty("terrain").GetString()
                |> Seq.map (fun c -> byte c - byte '0')
                |> Array.ofSeq
            Controller =
                Some
                    {
                        Id = "controller"
                        Pos = pos (input.GetProperty "controller")
                        Owner = Ours
                        Level = level
                        Progress = 0
                        ProgressTotal = 0
                        TicksToDowngrade = 0
                        Reservation = None
                        SafeMode = 0
                        SafeModeAvailable = 0
                        SafeModeCooldown = 0
                        UpgradeBlocked = 0
                    }
            EnergyAvailable = 0
            EnergyCapacity = 0
            Sources =
                each "sources"
                |> Array.mapi (fun i p ->
                    {
                        Id = $"source{i}"
                        Pos = p
                        Energy = 0
                        EnergyCapacity = 0
                        TicksToRegeneration = 0
                    })
            Minerals =
                each "minerals"
                |> Array.mapi (fun i p ->
                    {
                        Id = $"mineral{i}"
                        Pos = p
                        Resource = Energy
                        Amount = 0
                    })
            Structures = [||]
            Sites = [||]
            Piles = [||]
            Remains = [||]
            Hostiles = [||]
        }

    let ground = Ground.ofRoom room |> Option.get

    let firstSpawn =
        (Plan.ofGround ground [||] |> Option.get).Items
        |> Array.find (fun i -> i.Kind = Kind.Spawn)

    // The plan the bot draws once its first spawn stands.
    let plan = Plan.ofGround ground [| firstSpawn.Tile |] |> Option.get

    let built = ResizeArray<Structure>()

    let build (kind: Kind) (tile: int) =
        built.Add
            {
                Id = $"s{built.Count}"
                Kind = kind
                Pos = Tiles.toPos name tile
                Owner = Ours
                Hits = 1
                HitsMax = 1
                Store = Store.empty
                Cooldown = 0
                Collapse = 0
                Walkable = Kind.walkable kind
            }

    build Kind.Spawn firstSpawn.Tile
    let mutable placing = true

    while placing do
        let census =
            Sites.censusOf
                { room with
                    Structures = built.ToArray()
                }
                [||]

        let placed = Sites.place census level plan Int32.MaxValue
        placing <- not placed.IsEmpty

        for p in placed do
            build p.Kind (Tiles.index p.X p.Y)

    built
    |> Seq.map (fun s ->
        {|
            kind = Kind.name s.Kind
            x = s.Pos.X
            y = s.Pos.Y
        |})
    |> JsonSerializer.Serialize
    |> Console.WriteLine

    0
