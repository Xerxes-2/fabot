/// Invariants of the tick runner (`ARCHITECTURE.md` §2): a module's fault
/// costs that module alone, a switched-off module does nothing, and the
/// accounting covers every module.
module Fabot.Core.Tests.TickTests

open Expecto
open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Intents
open Fabot.Core.State

let private world: Tick.Context =
    let world =
        {
            Time = 1
            Me = ""
            CpuLimit = 20.0
            Bucket = 10_000
            Rooms = [||]
            Spawns = [||]
            Creeps = [||]
            OurSites = 0
        }

    {
        World = world
        Atlas = Atlas.ofWorld world
        Declarations = Tick.undeclared
        Sightings = Map.empty
    }

/// A module named `i` that says its name once, or throws.
let private moduleOf (i: int) (throws: bool) : Tick.Module =
    {
        Name = $"m{i}"
        Run =
            fun _ state ->
                if throws then
                    failwith $"m{i} broke"
                else
                    [ Creep($"c{i}", Say $"m{i}") ], state
    }

let private modulesOf (throws: bool list) = throws |> List.mapi moduleOf

/// What a module said: an `Intent` holds a wish and has no equality.
let private said (intents: Intent list) =
    intents
    |> List.choose (fun i ->
        match i with
        | Creep(name, Say message) -> Some(name, message)
        | _ -> None)

/// A clock that advances one unit a reading.
let private ticking () =
    let mutable t = 0.0

    fun () ->
        t <- t + 1.0
        t

[<Tests>]
let tests =
    testList
        "Tick.run"
        [
            testProperty "a throw costs its own module's intents and no other's"
            <| fun (throws: bool list) ->
                let result = Tick.run (ticking ()) (modulesOf throws) world State.empty

                let expected =
                    throws
                    |> List.indexed
                    |> List.filter (fun (_, t) -> not t)
                    |> List.map (fun (i, _) -> $"c{i}", $"m{i}")

                said result.Intents = expected

            testProperty "every module is reported once, in order, faults named, then the settling"
            <| fun (throws: bool list) ->
                let result = Tick.run (ticking ()) (modulesOf throws) world State.empty

                List.length result.Reports = List.length throws + 1
                && (List.last result.Reports).Module = "settle"
                && List.forall2
                    (fun (r: Tick.Report) (i, t) ->
                        r.Module = $"m{i}"
                        && (match r.Outcome with
                            | Tick.Faulted message -> t && message = $"m{i} broke"
                            | Tick.Ran -> not t
                            | Tick.Off -> false))
                    (List.truncate (List.length throws) result.Reports)
                    (List.indexed throws)

            testProperty "a switched-off module runs nothing and spends nothing"
            <| fun (throws: bool list) (offIndex: int) ->
                let throws = false :: throws
                let off = abs (offIndex % List.length throws)

                let state =
                    { State.empty with
                        Off = Set.singleton $"m{off}"
                    }

                let result = Tick.run (ticking ()) (modulesOf throws) world state
                let report = result.Reports.[off]

                report.Outcome = Tick.Off
                && report.Cpu = 0.0
                && not (List.contains ($"c{off}", $"m{off}") (said result.Intents))
        ]
