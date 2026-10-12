/// The generated set's predictions (`ARCHITECTURE.md` §10.1, `npm run exchange`).
/// Reads samples on stdin as JSON
/// `[{ id, room, terrain: { weight, runOff }, ours: side, theirs: side }]`, a
/// side `{ bodies: [{ parts, boosts, hits, x, y }], towers: [{ x, y, energy }],
/// ramparts: [{ x, y, hits }] }` (parts head first by engine name, boosts by
/// compound or ""), and writes `[{ id, kind, ticks, dead, hits }]`.
module Fabot.Exchange.Program

open System
open System.Text.Json
open Fabot.Core.Vocabulary
open Fabot.Core.World
open Fabot.Core.Exchange
open Fabot.Core.Exchange.Exchange

[<EntryPoint>]
let main _ =
    let samples = JsonDocument.Parse(Console.In.ReadToEnd()).RootElement

    let predict (sample: JsonElement) =
        let room = sample.GetProperty("room").GetString()
        let int (e: JsonElement) (key: string) = e.GetProperty(key).GetInt32()

        let pos (e: JsonElement) : Pos =
            {
                Room = room
                X = int e "x"
                Y = int e "y"
            }

        let each (e: JsonElement) (key: string) =
            e.GetProperty(key).EnumerateArray() |> List.ofSeq

        let side (e: JsonElement) : Side =
            {
                Bodies =
                    each e "bodies"
                    |> List.map (fun b ->
                        let parts =
                            each b "parts"
                            |> List.map (fun p -> p.GetString() |> partOfName |> Option.get)

                        let boosts =
                            List.map2
                                (fun part (c: JsonElement) ->
                                    Boost.ofCompound part (c.GetString()))
                                parts
                                (each b "boosts")

                        Fighter.ofParts parts boosts (int b "hits") (Some(pos b)))
                Ramparts =
                    each e "ramparts" |> List.map (fun r -> pos r, int r "hits") |> Map.ofList
                Towers =
                    each e "towers" |> List.map (fun t -> { Pos = pos t; Energy = int t "energy" })
            }

        let terrain = sample.GetProperty "terrain"

        let kind, ticks, losses =
            match
                outcome
                    (side (sample.GetProperty "ours"))
                    (side (sample.GetProperty "theirs"))
                    {
                        Weight = int terrain "weight"
                        RunOff = int terrain "runOff"
                    }
            with
            | Win(ticks, losses) -> "win", ticks, losses
            | Kite ticks -> "kite", ticks, { Dead = 0; Hits = 0 }
            | Lose -> "lose", 0, { Dead = 0; Hits = 0 }

        {|
            id = sample.GetProperty("id").GetString()
            kind = kind
            ticks = ticks
            dead = losses.Dead
            hits = losses.Hits
        |}

    samples.EnumerateArray()
    |> Seq.map predict
    |> Array.ofSeq
    |> JsonSerializer.Serialize
    |> Console.WriteLine

    0
