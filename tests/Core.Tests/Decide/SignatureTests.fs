/// The colony's signature: the one line it writes onto a controller it
/// stands beside, and the reflex that writes it.
module Fabot.Core.Tests.Decide.SignatureTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

/// The signatures a tick emits, as (creep, controller, text).
let private signatures intents =
    intents
    |> List.choose (function
        | SignController(creep, controller, text) -> Some(creep, controller, text)
        | _ -> None)

/// `reflexColony`'s ground with a controller on it, the colony owning the
/// room, and whatever line is standing on that controller.
/// The line the fixtures' home room is signed with: `reflexColony`'s room.
let private homeLine = Colony.signatureFor ""

let private signColony sign creeps positions =
    { reflexColony "ctrl-1" Controller creeps positions with
        RoomControl = Map.ofList [ "", { ownedRoom with Sign = sign } ]
    }

[<Tests>]
let signatureTests =
    testList
        "the signature"
        [
            test "a creep beside an unsigned controller writes the colony's line" {
                let colony = signColony None [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 11 } ]

                Expect.equal
                    (signatures (decideOn colony).Intents)
                    [ "w1", "ctrl-1", homeLine ]
                    "a controller nobody has signed is the case this exists for (ADR 0004: absence is not a match)"
            }

            test "a controller already carrying our line is left alone" {
                let colony =
                    signColony (Some homeLine) [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 11 } ]

                Expect.isEmpty
                    (signatures (decideOn colony).Intents)
                    "the words standing there are the words we mean, so there is nothing to say"
            }

            test "a stranger's line is overwritten" {
                let colony =
                    signColony
                        (Some "🐝 You found this room. That's already an achievement.")
                        [ worker "w1" 0 50 ]
                        [ "w1", { X = 10; Y = 11 } ]

                Expect.equal
                    (signatures (decideOn colony).Intents |> List.map (fun (_, _, text) -> text))
                    [ homeLine ]
                    "four of our rooms carried a stranger's flavour text for hundreds of thousands of ticks"
            }

            test "a body two tiles off writes nothing, and never walks over to" {
                let colony = signColony None [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 12 } ]

                let { Intents = intents } = decideOn colony

                Expect.isEmpty
                    (signatures intents)
                    "the act needs range 1, and this reflex is opportunistic: it sends nobody anywhere"

                Expect.isEmpty
                    (intents
                     |> List.choose (function
                         | MoveCreep(name, _) when name = "w1" -> Some name
                         | _ -> None))
                    "and it buys no walk either — a sign is worth one intent and never a body's tick"
            }

            test "one of the bodies standing there writes it, not both" {
                let colony =
                    signColony
                        None
                        [ worker "w1" 0 50; worker "w2" 0 50 ]
                        [ "w1", { X = 10; Y = 11 }; "w2", { X = 11; Y = 10 } ]

                Expect.equal
                    (signatures (decideOn colony).Intents |> List.map (fun (creep, _, _) -> creep))
                    [ "w1" ]
                    "two bodies writing the same words is one wasted intent; the lowest name takes it"
            }

            // A creep at (10,11) of one room is not beside a controller at (10,10) of
            // another: `RoomPos.range` answers `None` across a border. Nothing else
            // in this file stands two rooms.
            test
                "a second room's controller is signed by a body in that room, and by nobody at home" {
                let outpost = "W1N2"

                let withOutpostController creeps positions (colony: ColonyView) =
                    { colony with
                        Creeps = colony.Creeps @ creeps
                        Spatial =
                            { colony.Spatial with
                                Rooms =
                                    Map.add
                                        outpost
                                        { RoomLayer.empty with
                                            Terrain =
                                                TerrainGrid.ofList
                                                    [
                                                        for x in 8..12 do
                                                            for y in 8..12 ->
                                                                { X = x; Y = y }, Plain
                                                    ]
                                            TargetPositions =
                                                Map.ofList [ "ctrl-out", { X = 10; Y = 10 } ]
                                            CreepPositions = Map.ofList positions
                                        }
                                        colony.Spatial.Rooms
                                TargetKinds =
                                    Map.add "ctrl-out" Controller colony.Spatial.TargetKinds
                            }
                        RoomControl = Map.add outpost ownedRoom colony.RoomControl
                    }

                // The home body stands at (10,11) of *home*, which is the same
                // pair of numbers as a tile beside the outpost's controller.
                let athome =
                    signColony (Some homeLine) [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 11 } ]
                    |> withOutpostController [] []

                Expect.isEmpty
                    (signatures (decideOn athome).Intents)
                    "a body one room over is not beside anything, whatever its coordinates say"

                let outThere =
                    athome
                    |> withOutpostController [ worker "w2" 0 50 ] [ "w2", { X = 10; Y = 11 } ]

                Expect.equal
                    (signatures (decideOn outThere).Intents)
                    [ "w2", "ctrl-out", Colony.signatureFor outpost ]
                    "and the body standing in the outpost signs the outpost's controller"
            }

            test "every line is under the engine's hundred characters" {
                // The engine truncates past 100, so a line that overflows is a
                // line nobody ever reads the end of.
                for line in Colony.signatures do
                    Expect.isLessThanOrEqual
                        (String.length line)
                        100
                        $"the sign the engine keeps is at most a hundred characters: {line}"
            }

            test "each room keeps one line of its own, and the rooms do not all share one" {
                // Picked off the room's name, so a room signed once is never
                // signed again with another line.
                let rooms = [ "W12S28"; "W13S28"; "W15S28"; "W11S29"; "W11S27" ]

                Expect.equal
                    (rooms |> List.map Colony.signatureFor)
                    (rooms |> List.map Colony.signatureFor)
                    "the same room, the same line, every tick"

                Expect.isGreaterThan
                    (rooms |> List.map Colony.signatureFor |> List.distinct |> List.length)
                    1
                    "and the colonies' rooms are not all signed alike"

                for room in rooms do
                    Expect.contains
                        Colony.signatures
                        (Colony.signatureFor room)
                        "a line off the list"
            }
        ]
