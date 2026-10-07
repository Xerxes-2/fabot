/// The colony's signature: the one line it writes onto a controller it
/// stands beside, and the reflex that writes it; and the weapon reflex (#535),
/// which strikes beside any Task.
module Fabot.Core.Tests.Decide.SignatureTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.OutpostFixtures

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
                    "a controller nobody has signed is the case this exists for: absence is not a match"
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

/// A ranger body as the ranger row casts it at an 800 bank.
let private ranger name =
    creepWith name 0 0 (bodyFor Bodies.rangerPattern 800)

/// The raided outpost field with one hostile of this owner beside the body of
/// ours standing at `beside`; `transit` takes the room's source away, so it is
/// a room we neither mine nor hold.
let private reflexField (transit: bool) (owner: string) (ours: CreepInfo) =
    let hostile =
        { hostileIn "W1N2" { X = 25; Y = 42 } smallMelee with
            Owner = owner
        }

    let colony = raidedOutpost [ hostile ] |> withGuards [ ours, beside ]

    if transit then { colony with Sources = [] } else colony

[<Tests>]
let weaponReflexTests =
    testList
        "the weapon reflex: every armed body strikes beside any Task, by one diplomacy rule (#535)"
        [
            // Live t974,080 (W18S25): ranger-973229 on Flee stood beside Sky320
            // and only healed itself.
            test
                "a fleeing ranger beside a hostile in our outpost shoots it as it steps away; an idle guard swings" {
                let result =
                    decideFrom
                        (Map.ofList [ "r-1", taskId Flee ])
                        (reflexField false "raider" (ranger "r-1"))

                Expect.equal
                    (Map.tryFind "r-1" result.Assignments)
                    (Some(taskId Flee))
                    "the premise: it flees"

                Expect.contains
                    result.Intents
                    (RangedAttackCreep("r-1", "h-1"))
                    "it shoots the hostile beside it"

                Expect.isNonEmpty
                    (moveIntents result.Intents |> List.filter (fst >> (=) "r-1"))
                    "while it steps away"

                let swung = decideOn (reflexField false "raider" (guard "g-1"))

                Expect.contains
                    swung.Intents
                    (AttackCreep("g-1", "h-1"))
                    "and an idle guard beside one swings at it"
            }

            test
                "beside a neutral's armed creep in a transit room it does not shoot; beside an Invader anywhere it does" {
                let shoots owner transit =
                    (decideFrom
                        (Map.ofList [ "r-1", taskId Flee ])
                        (reflexField transit owner (ranger "r-1")))
                        .Intents
                    |> List.contains (RangedAttackCreep("r-1", "h-1"))

                Expect.isFalse
                    (shoots "raider" true)
                    "a neutral's, in a room we neither mine nor hold: never"

                Expect.isTrue (shoots "raider" false) "the same in our outpost: struck"
                Expect.isTrue (shoots "Invader" true) "an Invader's anywhere"
                Expect.isTrue (shoots "Trepidimous" true) "a declared enemy's anywhere"
                Expect.isFalse (shoots "Source Keeper" false) "never a keeper, not provoked"
                Expect.isFalse (shoots "Odiodin" false) "never an ally"
            }
        ]

/// A raider's smallMelee in our outpost at this tile, under this id.
let private raiderAt id (pos: Pos) =
    { hostileIn "W1N2" pos smallMelee with
        Id = id
    }

/// What the ranger "r-1" (two guns, on Flee at `beside`, (24,43)) strikes this
/// tick among the bodies of this outpost: its shots and its mass attack.
let private strikesAmong (colony: ColonyView) =
    (decideFrom (Map.ofList [ "r-1", taskId Flee ]) (colony |> withGuards [ ranger "r-1", beside ]))
        .Intents
    |> List.filter (fun intent ->
        match intent with
        | RangedAttackCreep("r-1", _)
        | RangedMassAttack "r-1" -> true
        | _ -> false)

/// Two raiders beside the ranger.
let private pair () =
    raidedOutpost [ raiderAt "h-1" { X = 25; Y = 42 }; raiderAt "h-2" { X = 23; Y = 42 } ]

/// The outpost with an object of this owner's at this tile, as
/// `World.factsOf` projects every object besides creeps a mass attack hits: a
/// structure somebody else owns, of whatever kind, or a power creep.
let private withForeign (owner: string) (pos: Pos) (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Spatial =
            colony.Spatial
            |> withNeighbour
                "W1N2"
                { outpost with
                    Foreign = Map.add pos owner outpost.Foreign
                }
    }

[<Tests>]
let strikeChoiceTests =
    testList
        "the strike choice: the strike dealing the most, never a mass attack beside a body we may not strike (#537)"
        [
            test "two hostiles beside a ranger take a mass attack; one takes a shot" {
                Expect.equal
                    (strikesAmong (pair ()))
                    [ RangedMassAttack "r-1" ]
                    "two at range 1: 40 against 20"

                Expect.equal
                    (strikesAmong (raidedOutpost [ raiderAt "h-1" { X = 25; Y = 42 } ]))
                    [ RangedAttackCreep("r-1", "h-1") ]
                    "one: the shot, which deals the same"
            }

            test "three hostiles at range 2 take a mass attack" {
                Expect.equal
                    (strikesAmong (
                        raidedOutpost
                            [
                                raiderAt "h-1" { X = 26; Y = 43 }
                                raiderAt "h-2" { X = 22; Y = 43 }
                                raiderAt "h-3" { X = 24; Y = 45 }
                            ]
                    ))
                    [ RangedMassAttack "r-1" ]
                    "24 against 20"
            }

            test
                "a raider at range 2 and a hauler beside it take the shot at the raider: the mass counts only the raid's band (#563)" {
                let hauler =
                    { hostileIn "W1N2" { X = 25; Y = 42 } [ Move; Carry ] with
                        Id = "hauler"
                        Owner = "Trepidimous"
                    }

                Expect.equal
                    (strikesAmong (raidedOutpost [ raiderAt "h-1" { X = 26; Y = 43 }; hauler ]))
                    [ RangedAttackCreep("r-1", "h-1") ]
                    "the mass deals the raider 8 and the hauler 20, the shot the raider 20"
            }

            // Live t986,014 (W17S26): a ranger whose kill order stood in
            // another room mass-attacked the melee three off, for 2.
            test "with no target of its kill order in reach: never a mass attack (#563)" {
                Expect.equal
                    (Emitter.pickStrike
                        (fun _ -> false)
                        (fun _ -> 20)
                        (fun _ -> 0)
                        (Some(fun _ -> 2))
                        [ "far" ])
                    (Some(Emitter.At("far", false)))
                    "the head out of reach, whatever else stands within three"
            }

            test
                "a broken healer and its melee both beside it take a mass attack: the healer loses nothing, the melee gains a blow (#563)" {
                // The order's bands: the broken healer 1, the melee 2; two
                // guns, both at range 1.
                let band =
                    function
                    | "healer" -> 1
                    | _ -> 2

                Expect.equal
                    (Emitter.pickStrike
                        (fun _ -> true)
                        (fun _ -> 20)
                        band
                        (Some(fun _ -> 20))
                        [ "healer"; "melee" ])
                    (Some Emitter.Mass)
                    "20 on the healer as the shot, and 20 on the melee besides"

                Expect.equal
                    (Emitter.pickStrike
                        (fun _ -> true)
                        (fun _ -> 20)
                        band
                        (Some(fun t -> if t = "healer" then 8 else 20))
                        [ "healer"; "melee" ])
                    (Some(Emitter.At("healer", true)))
                    "the healer at range 2: 8 on it against the shot's 20, the melee's 20 notwithstanding"
            }

            test
                "a hostile beside it with 5 hits left and one at range 3 take a shot: damage is capped by hits" {
                let struck =
                    strikesAmong (
                        raidedOutpost
                            [
                                { raiderAt "h-1" { X = 25; Y = 42 } with
                                    Hits = 5
                                }
                                raiderAt "h-2" { X = 27; Y = 43 }
                            ]
                    )

                Expect.isFalse
                    (List.contains (RangedMassAttack "r-1") struck)
                    "the mass deals 5 + 2"

                Expect.hasLength struck 1 "a shot, up to 20"
            }

            test "an ally's creep within three: never a mass attack, whose blast would hit it" {
                let odiodin =
                    { raiderAt "ally" { X = 24; Y = 46 } with
                        Owner = "Odiodin"
                    }

                let filed = { pair () with Allies = [ odiodin ] }

                Expect.isFalse
                    (strikesAmong filed |> List.contains (RangedMassAttack "r-1"))
                    "an Odiodin creep at range 3, filed as an ally"

                Expect.isFalse
                    (strikesAmong
                        { pair () with
                            Hostiles = (pair ()).Hostiles @ [ odiodin ]
                        }
                     |> List.contains (RangedMassAttack "r-1"))
                    "nor filed among the hostiles"

                Expect.hasLength (strikesAmong filed) 1 "a single shot instead"
            }

            test
                "an ally's or a neutral's structure within three: never a mass attack; an enemy's: still" {
                let massed owner pos =
                    strikesAmong (withForeign owner pos (pair ()))
                    |> List.contains (RangedMassAttack "r-1")

                Expect.isFalse
                    (massed "passer" { X = 24; Y = 46 })
                    "a neutral's extension at range 3"

                Expect.isFalse
                    (massed "Odiodin" { X = 24; Y = 46 })
                    "an Odiodin structure at range 3"

                Expect.isFalse (massed "" { X = 24; Y = 46 }) "a power bank at range 3"

                Expect.isTrue
                    (massed "passer" { X = 24; Y = 47 })
                    "at range 4 it is out of the blast"

                Expect.isTrue
                    (massed "Trepidimous" { X = 24; Y = 46 })
                    "a Trepidimous rampart at range 3: a target too"
            }

            test "a keeper or an unarmed neutral within three: never a mass attack" {
                let beside owner body =
                    { pair () with
                        Hostiles =
                            (pair ()).Hostiles
                            @ [
                                { hostileIn "W1N2" { X = 24; Y = 46 } body with
                                    Id = "other"
                                    Owner = owner
                                }
                            ]
                    }

                Expect.isFalse
                    (strikesAmong (beside "Source Keeper" smallMelee)
                     |> List.contains (RangedMassAttack "r-1"))
                    "a Source Keeper at range 3, not provoked"

                Expect.isFalse
                    (strikesAmong (beside "passer" [ Move; Carry ])
                     |> List.contains (RangedMassAttack "r-1"))
                    "a neutral's hauler at range 3, which we may not strike"

                Expect.hasLength
                    (strikesAmong (beside "passer" [ Move; Carry ]))
                    1
                    "a single shot at a raider instead"
            }

            test "in a room the view only crosses: never a mass attack, its objects unseen" {
                let crossed =
                    { pair () with
                        Hostiles =
                            (pair ()).Hostiles |> List.map (fun h -> { h with Owner = "Invader" })
                        Crossed = Set.ofList [ "W1N2" ]
                    }

                Expect.isFalse
                    (strikesAmong crossed |> List.contains (RangedMassAttack "r-1"))
                    "a crossed room carries no shields to read"
            }
        ]
