/// A player's unarmed body in a room we mine, and the resident lent to a
/// chase beside its home.
module Fabot.Core.Tests.Decide.OutpostTrespassTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.OutpostFixtures

/// Gambit229 as W17S26 saw it (#550): an empty 23C23M hauler, at the pace of
/// every fighting row of ours.
let private haulerOf id pos =
    { hostileIn "W1N2" pos (List.replicate 23 Carry @ List.replicate 23 Move) with
        Id = id
        Owner = "Trepidimous"
    }

/// Infinity425 as W17S26 saw it (#550): a 3M3C claimer reserving against us.
let private claimerOf id pos =
    { hostileIn "W1N2" pos (List.replicate 3 Move @ List.replicate 3 BodyPart.Claim) with
        Id = id
        Owner = "Trepidimous"
    }

/// The outpost floored out to its borders, and beyond it W1N3, a second
/// room this colony mines, its source at (25,25): outpost A and outpost B
/// (#528), the guards of ours standing in A on the tiles the case names.
let private secondOutpost hostiles (ours: (CreepInfo * Pos) list) =
    let colony = declaredRaid hostiles |> withGuards ours
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    let floor =
        TerrainGrid.ofList
            [
                for x in 1..48 do
                    for y in 1..48 -> { X = x; Y = y }, Plain
            ]

    { colony with
        Sources = source "src-b" :: colony.Sources
        Spatial =
            { colony.Spatial with
                Borders = colony.Spatial.Borders |> Map.add "W1N3" plainRing
                TargetKinds = colony.Spatial.TargetKinds |> Map.add "src-b" Source
            }
            |> withNeighbour "W1N2" { outpost with Terrain = floor }
            |> withNeighbour
                "W1N3"
                { RoomLayer.empty with
                    Terrain = floor
                    TargetPositions = Map.ofList [ "src-b", { X = 25; Y = 25 } ]
                    Obstacles = Set.singleton { X = 25; Y = 25 }
                }
    }

/// The colony at its bank's floor (`Tuning.CasterFloor`), no stock: what a
/// trespass's priced cast into its own room is paid out of (#652).
let private paying (colony: ColonyView) =
    { colony with
        Bank = bank Tuning.defaults.CasterFloor Tuning.defaults.CasterFloor
    }

/// `raisingBeyond` with W2N3 beside the child's home W1N3 mined too, its
/// source at (25,25): two outposts the child's resident borders (#652).
let private raisingBesideTwo hostiles =
    let colony = raisingBeyond hostiles

    let floor =
        TerrainGrid.ofList
            [
                for x in 1..48 do
                    for y in 1..48 -> { X = x; Y = y }, Plain
            ]

    { colony with
        Sources = source "src-c" :: colony.Sources
        Spatial =
            { colony.Spatial with
                Borders = colony.Spatial.Borders |> Map.add "W2N3" plainRing
                TargetKinds = colony.Spatial.TargetKinds |> Map.add "src-c" Source
            }
            |> withNeighbour
                "W2N3"
                { RoomLayer.empty with
                    Terrain = floor
                    TargetPositions = Map.ofList [ "src-c", { X = 25; Y = 25 } ]
                    Obstacles = Set.singleton { X = 25; Y = 25 }
                }
    }

/// The pair world decided whole (#652, the #647 shape): Raptor865's 5W3M
/// seated beside the mother's outpost rock, the mother's bank at 1,300,
/// under the floor her own cast is paid past, and the child at RCL6 past
/// both floors, the bar for casting into another's room; `bodies` stand at
/// the child's home.
let private trespassedPairDecided (bodies: (string * BodyPart list) list) =
    let outpost, child = ViewFixtures.outpost, ViewFixtures.child
    let tuning = Tuning.defaults
    let colonies = ViewFixtures.declared

    let raptor: HostileInfo =
        {
            Id = "raptor-out"
            Owner = "Trepidimous"
            Pos = { Room = outpost; X = 5; Y = 6 }
            Body = List.replicate 5 Work @ List.replicate 3 Move
            Boosts = []
            Hits = 8 * Engine.partHits
            TicksToLive = 1000
        }

    let rooms =
        ViewFixtures.pairRooms
        |> Map.change outpost (Option.map (fun facts -> { facts with Hostiles = [ raptor ] }))
        |> Map.change
            ViewFixtures.mother
            (Option.map (fun facts ->
                { facts with
                    Energy = { Available = 1_300; Capacity = 1_300 }
                }))
        |> Map.change
            child
            (Option.map (fun facts ->
                { ViewFixtures.withStock tuning.CasterReserve facts with
                    Controller =
                        facts.Controller |> Option.map (fun ctrl -> { ctrl with Level = 6 })
                    Energy =
                        {
                            Available = tuning.CasterFloor
                            Capacity = tuning.CasterFloor
                        }
                    Layer =
                        { facts.Layer with
                            CreepPositions =
                                (facts.Layer.CreepPositions, bodies)
                                ||> List.fold (fun tiles (name, _) ->
                                    Map.add name ViewFixtures.pioneerTile tiles)
                        }
                }))

    let world =
        { ViewFixtures.pairWorld with
            Rooms = rooms
            Sightings =
                rooms
                |> Map.map (fun _ facts ->
                    {
                        Tick = ViewFixtures.pairWorld.Time
                        Targets = lazy (facts.TargetKinds |> Map.keys |> Set.ofSeq)
                        Rival = None
                    })
            Creeps =
                ViewFixtures.pairWorld.Creeps
                @ (bodies
                   |> List.map (fun (name, parts) ->
                       let body = ViewFixtures.creepWithBody name child (List.countBy id parts)
                       let hits = Engine.partHits * List.length parts

                       { body with
                           Info =
                               { body.Info with
                                   Hits = { Hits = hits; HitsMax = hits }
                               }
                       }))
        }

    let gates =
        colonies |> List.map (fun colony -> colony.Home, StandDown.none) |> Map.ofList

    let joins = JoinTable()

    let holders =
        World.creepColonies
            tuning
            colonies
            (World.living colonies world)
            ViewFixtures.noneShut
            world

    let watch = WorldView.ofWorld colonies (gates |> Map.toList |> List.map snd) world

    let views =
        colonies
        |> List.map (fun colony ->
            colony,
            ColonyView.ofWorldRecalling
                joins
                tuning
                colonies
                HarassCasting.none
                gates.[colony.Home]
                holders
                isFighterBody
                watch
                world
                colony)

    let decided =
        decideColonies
            {
                Hops = World.reachHops StandDown.none joins tuning world tuning.MaxHops
                Holding = Map.empty
                Verbose = Set.empty
                PlanMemos = Map.empty
                Carries = Map.empty
                Idle = Set.empty
                FightAlone = Set.empty
                TurnIndex = None
            }
            ignore
            views

    decided,
    fun home ->
        decided.Decided
        |> List.pick (fun (colony, _, decision, ()) ->
            if colony.Home = home then Some decision else None)

/// This tick's casts for a room's kill: row, bodies and prey by id.
let private killCasts (room: string) (decided: Decision) =
    decided.Quotas.Casts
    |> List.filter (fun cast -> cast.For = Guard room)
    |> List.map (fun cast -> cast.Ask.Row, cast.Ask.Wanted, cast.Prey |> Option.map (List.map fst))

[<Tests>]
let trespassTests =
    testList
        "a player's unarmed worker in a room we mine (#516)"
        [
            test "an unarmed 5W3M player body in our outpost pools one kill Task, one rung up" {
                let pool = pooledOf (declaredRaid [ diggerAt "W1N2" { X = 25; Y = 44 } ])

                let entry = entryFor (Guard "W1N2") pool

                Expect.isSome entry "the outpost's Guard is pooled for it"

                Expect.equal
                    (entry |> Option.map (fun e -> e.Capacity))
                    (Some(Capacity.fighters 1))
                    "for one body"

                let raidGuard =
                    pooledOf (declaredRaid [ hostileIn "W1N2" { X = 25; Y = 42 } smallMelee ])
                    |> entryFor (Guard "W1N2")

                Expect.equal
                    (entry |> Option.map (fun e -> e.Priority))
                    (Some(safetyRank OneRungUp))
                    "a rung over the tier, a quiet holder's Guard"

                Expect.isLessThan
                    (raidGuard |> Option.map (fun e -> e.Priority) |> Option.defaultValue 0)
                    (entry |> Option.map (fun e -> e.Priority) |> Option.defaultValue 0)
                    "and under a raided outpost's Guard: defence first (#528)"

                Expect.isSome
                    (pooledOf (declaredRaid [ diggerAt "W1N1" { X = 25; Y = 5 } ])
                     |> entryFor (Guard "W1N1"))
                    "and in the home"
            }

            test "a 23C23M player hauler in our outpost pools it too: it carries our energy off" {
                let hauler =
                    { hostileIn
                          "W1N2"
                          { X = 25; Y = 44 }
                          (List.replicate 23 Carry @ List.replicate 23 Move) with
                        Owner = "Trepidimous"
                    }

                Expect.isSome
                    (pooledOf (declaredRaid [ hauler ]) |> entryFor (Guard "W1N2"))
                    "the outpost's Guard is pooled for it"
            }

            test
                "a hauler two from our container is waited for at the container, shot in range, and never followed out" {
                let container = { X = 25; Y = 41 }

                let haulerIn room pos =
                    { hostileIn room pos (List.replicate 23 Carry @ List.replicate 23 Move) with
                        Owner = "Trepidimous"
                    }

                let waiting =
                    declaredRaid [ haulerIn "W1N2" { X = 27; Y = 43 } ]
                    |> withGuards [ ranger "r-9", { X = 25; Y = 47 } ]

                let ground = groundOf waiting

                Expect.isNonEmpty ground "the premise: the kill has ground"

                Expect.isTrue
                    (ground
                     |> Set.forall (fun tile ->
                         rangeTo container tile <= 2 && rangeTo container tile > 0))
                    "its ground is about the container, off it, and not on the hauler's heels"

                Expect.equal
                    (Map.tryFind "r-9" (decideOn waiting).Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the ranger takes it"

                let inRange =
                    declaredRaid [ haulerIn "W1N2" { X = 27; Y = 43 } ]
                    |> withGuards [ ranger "r-9", { X = 26; Y = 45 } ]

                Expect.contains
                    (decideOn inRange).Intents
                    (RangedAttackCreep("r-9", "h-1"))
                    "and shoots it within three"

                let gone =
                    decide
                        (declaredRaid [ haulerIn "W1N3" { X = 25; Y = 48 } ]
                         |> withGuards [ ranger "r-9", { X = 25; Y = 47 } ])
                        (Map.ofList [ "r-9", taskId (Guard "W1N2") ])
                        Set.empty
                        None

                Expect.notEqual
                    (Map.tryFind "r-9" gone.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "out of the room it is no kill of ours"
            }

            test
                "a hauler is waited for at the store nearest it, and beside itself away from every store" {
                let haulerAt pos =
                    { hostileIn "W1N2" pos (List.replicate 23 Carry @ List.replicate 23 Move) with
                        Owner = "Trepidimous"
                    }

                let withSecond colony =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                TargetKinds =
                                    colony.Spatial.TargetKinds
                                    |> Map.add "can-two" (Structure BuiltKind.Container)
                            }
                    }
                    |> withOutpostLayer "W1N2" (fun layer ->
                        { layer with
                            TargetPositions =
                                Map.add "can-two" { X = 29; Y = 47 } layer.TargetPositions
                        })

                let near = groundOf (declaredRaid [ haulerAt { X = 27; Y = 43 } ] |> withSecond)

                Expect.isNonEmpty near "the premise: the kill has ground"

                Expect.isTrue
                    (near |> Set.forall (fun tile -> rangeTo { X = 25; Y = 41 } tile <= 2))
                    "about the container two from it, and not the one four off"

                let away = { X = 21; Y = 48 }
                let far = groundOf (declaredRaid [ haulerAt away ])

                Expect.isNonEmpty far "the premise: the kill has ground"

                Expect.isTrue
                    (far |> Set.forall (fun tile -> rangeTo away tile = 1))
                    "seven from the one store it draws from none, and is the ground's own"
            }

            // Live W17S26 t1,017,292 (#619): an empty hauler walked past the
            // container and out while two rangers stood on its ring.
            test
                "a hauler stepping away from its store is chased, one stepping toward it waited for at the store, one four off chased (#619)" {
                let container = { X = 25; Y = 41 }

                let haulerAt pos =
                    { hostileIn "W1N2" pos (List.replicate 23 Carry @ List.replicate 23 Move) with
                        Owner = "Trepidimous"
                    }

                let stepped (from: Pos) (colony: ColonyView) =
                    { colony with
                        Stepped = Map.ofList [ "h-1", RoomPos.at "W1N2" from ]
                    }

                let at = { X = 27; Y = 43 }

                let leaving = groundOf (declaredRaid [ haulerAt at ] |> stepped { X = 26; Y = 42 })

                Expect.isNonEmpty leaving "the premise: the kill has ground"

                Expect.isTrue
                    (leaving |> Set.forall (fun tile -> rangeTo at tile = 1))
                    "two off and stepping away, it is beside itself"

                let coming = groundOf (declaredRaid [ haulerAt at ] |> stepped { X = 28; Y = 44 })

                Expect.isTrue
                    (not (Set.isEmpty coming)
                     && coming |> Set.forall (fun tile -> rangeTo container tile <= 2))
                    "two off and stepping toward it, it is waited for at the container"

                let four = { X = 29; Y = 44 }
                let far = groundOf (declaredRaid [ haulerAt four ] |> stepped { X = 30; Y = 45 })

                Expect.isTrue
                    (not (Set.isEmpty far) && far |> Set.forall (fun tile -> rangeTo four tile = 1))
                    "four off and coming in, it is beside itself"

                let three = { X = 28; Y = 44 }
                let ring = groundOf (declaredRaid [ haulerAt three ])

                let chased =
                    groundOf (declaredRaid [ haulerAt three ] |> stepped { X = 27; Y = 43 })

                Expect.isNonEmpty
                    (Set.intersect ring chased)
                    "three off, where it is waited for at the farthest, the ring and the tiles beside it share ground: a sway there moves no body standing on both"
            }

            // Live W17S26 t979,593-980,050 (#550): a hauler parked four from the
            // container laid its store ring under the whole kill, and the guard
            // standing on it waited ~450 ticks beside a digger and a claimer.
            // Three off here: four off, a hauler is beside itself since #619.
            test
                "beside a trespasser the guard catches a hauler is not waited for at its store (#550)" {
                let hauler = haulerOf "a-haul" { X = 28; Y = 44 }
                let beside (pos: Pos) tile = rangeTo pos tile = 1

                let besideAny bodies tile =
                    bodies |> List.exists (fun h -> beside (RoomPos.pos h.Pos) tile)

                for label, rooted in
                    [
                        // The outpost's controller stands at (30,48).
                        "a claimer at the controller", claimerOf "y-claim" { X = 29; Y = 47 }
                        "a digger on its seat", seatedDigger
                        "a digger slower than the guard",
                        { diggerAt "W1N2" { X = 22; Y = 46 } with
                            Id = "d-2"
                        }
                    ] do
                    let ground = groundOf (declaredRaid [ hauler; rooted ])

                    Expect.isNonEmpty ground $"the premise: beside {label} the kill has ground"

                    Expect.isTrue
                        (ground |> Set.forall (besideAny [ hauler; rooted ]))
                        $"beside {label}, every tile of it is beside a trespasser"

                    Expect.isTrue
                        (ground |> Set.exists (beside (RoomPos.pos hauler.Pos)))
                        $"the hauler's own among them, beside {label}"

                let crossing = claimerOf "y-claim" { X = 22; Y = 46 }

                Expect.isTrue
                    (groundOf (declaredRaid [ hauler; crossing ])
                     |> Set.exists (besideAny [ hauler; crossing ] >> not))
                    "a claimer crossing at the guard's pace roots nothing: the hauler's store ring stands"
            }

            test "a claimer heads the kill order among trespassers, behind the raid (#550)" {
                let hauler = haulerOf "a-haul" { X = 27; Y = 46 }
                let claimer = claimerOf "y-claim" { X = 29; Y = 47 }

                let raider =
                    { hostileIn "W1N2" { X = 20; Y = 44 } smallMelee with
                        Id = "z-raid"
                    }

                let colony =
                    declaredRaid [ hauler; claimer ]
                    |> withGuards [ guard "g-1", { X = 28; Y = 46 } ]

                let result = decideOn colony

                Expect.equal
                    (Map.tryFind "g-1" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the premise: the guard holds the kill"

                Expect.contains
                    result.Intents
                    (AttackCreep("g-1", "y-claim"))
                    "beside both, it swings at the claimer, the hauler nearer the Post"

                let raided = declaredRaid [ hauler; claimer; raider ]
                let threats = threatsOf raided (Atlas.ofView raided)

                Expect.equal
                    ((Threats.briefOf threats "W1N2").Targets.Order
                        "W1N2"
                        None
                        (fun _ -> 0)
                        [ hauler; claimer; raider ]
                     |> List.map (fun h -> h.Id))
                    [ "z-raid"; "y-claim"; "a-haul" ]
                    "the raid first, then the claimer, then the rest"
            }

            // #621: the digger on its seat (650 body, our source's 10 a tick
            // until it is replaced, 800 hits) is worth more a tick of one
            // ranger block's effort (20 a tick) than the empty hauler (4,600
            // hits, a 230-tick kill), so the holder is seated beside the
            // digger alone and shoots it first, where before every
            // trespasser's tiles were its ground.
            test
                "a ranger holding the kill is seated about the trespasser worth most a tick of its effort, and shoots it first (#621)" {
                let hauler = haulerOf "a-haul" { X = 30; Y = 46 }

                let colony =
                    declaredRaid [ hauler; seatedDigger ]
                    |> withGuards [ ranger "r-9", { X = 27; Y = 45 } ]

                let held = Map.ofList [ "r-9", taskId (Guard "W1N2") ]
                let atlas = Atlas.ofView colony
                let threats = threatsOfHeld colony atlas (Facts.heldTaskFacts colony held)
                let brief = Threats.briefOf threats "W1N2"

                Expect.isTrue
                    (brief.Ground
                     |> Option.exists (
                         Set.exists (fun tile -> rangeTo (RoomPos.pos hauler.Pos) tile = 1)
                     ))
                    "the premise: the hauler's tiles are the kill's ground too"

                let seat = Map.tryFind "r-9" brief.Seats.Value |> Option.defaultValue Set.empty

                Expect.isNonEmpty seat "it has a seat"

                Expect.isTrue
                    (seat
                     |> Set.forall (fun tile ->
                         rangeTo (RoomPos.pos seatedDigger.Pos) tile <= Engine.rangedRange))
                    "every tile of it within its reach of the digger"

                Expect.equal
                    (brief.Targets.Order
                        "W1N2"
                        (Some(ranger "r-9"))
                        (fun _ -> 0)
                        [ hauler; seatedDigger ]
                     |> List.map (fun h -> h.Id))
                    [ "d-1"; "a-haul" ]
                    "the digger leads its kill order"
            }

            // #621 (concentrate force for kills, user 2026-10-08): the digger's
            // medic heals it 48 a tick, more than one 40-a-tick ranger deals and
            // less than two. Each kill is priced at the damage of every holder
            // of the fight together, so the pair focuses the digger, where one
            // alone goes for the small carrier it kills by itself.
            test
                "two rangers holding the kill both shoot the healed digger only the two of them kill; one alone leads with the carrier it kills (#621)" {
                let twoBlocks name =
                    creepWith name 0 0 (List.replicate 2 Bodies.rangerPattern.Block |> List.concat)

                let medic =
                    { hostileIn
                          "W1N2"
                          { X = 25; Y = 42 }
                          (List.replicate 4 Heal @ List.replicate 4 Move) with
                        Id = "m-1"
                        Owner = "Trepidimous"
                    }

                let carrier =
                    { hostileIn "W1N2" { X = 27; Y = 43 } [ Carry; Move ] with
                        Id = "c-1"
                        Owner = "Trepidimous"
                    }

                // Each holder three from the digger, and out of a mass
                // attack's reach of the other two.
                let pair =
                    [ twoBlocks "r-1", { X = 21; Y = 44 }; twoBlocks "r-2", { X = 24; Y = 44 } ]

                let colonyOf holders =
                    declaredRaid [ seatedDigger; medic; carrier ] |> withGuards holders

                let heldBy (holders: (CreepInfo * Pos) list) =
                    holders
                    |> List.map (fun (creep, _) -> creep.Name, taskId (Guard "W1N2"))
                    |> Map.ofList

                let headFor holders (creep: CreepInfo) =
                    let colony = colonyOf holders
                    let atlas = Atlas.ofView colony

                    let threats =
                        threatsOfHeld colony atlas (Facts.heldTaskFacts colony (heldBy holders))

                    (Threats.briefOf threats "W1N2").Targets.Order
                        "W1N2"
                        (Some creep)
                        (fun _ -> 0)
                        [ seatedDigger; medic; carrier ]
                    |> List.tryHead
                    |> Option.map (fun h -> h.Id)

                for creep, _ in pair do
                    Expect.equal
                        (headFor pair creep)
                        (Some "d-1")
                        $"{creep.Name} beside its partner leads with the digger"

                Expect.equal
                    ((decide (colonyOf pair) (heldBy pair) Set.empty None).Intents
                     |> List.choose (function
                         | RangedAttackCreep(name, id) -> Some(name, id)
                         | _ -> None)
                     |> List.sort)
                    [ "r-1", "d-1"; "r-2", "d-1" ]
                    "the pair both shoot the digger"

                let alone = [ List.head pair ]

                Expect.equal
                    (headFor alone (fst (List.head pair)))
                    (Some "c-1")
                    "one alone leads with the carrier it kills by itself"
            }

            test
                "each trespasser is answered by a row that can strike it: the guard the rooted and the slow, the ranger the rest (#550)" {
                let hauler = haulerOf "a-haul" { X = 29; Y = 44 }

                // The asks and the guards' cap, as a colony whose creeps hold
                // these Tasks reads them.
                let readHolding (holding: string list) colony =
                    let threats =
                        threatsOfHeld
                            colony
                            (Atlas.ofView colony)
                            (heldTaskFacts
                                colony
                                (holding
                                 |> List.map (fun name -> name, taskId (Guard "W1N2"))
                                 |> Map.ofList))

                    let brief = Threats.briefOf threats "W1N2"

                    brief.Asks
                    |> List.map (fun ask -> ask.Row, ask.Wanted threats)
                    |> List.filter (fun (_, wanted) -> wanted > 0),
                    Map.tryFind
                        (CapScope.Row(CastRow.Fighter BodyTemplate.Melee))
                        (brief.Capacity threats).Caps

                let asksOf colony = fst (readHolding [] colony)

                let lending colony =
                    colony |> withGuards [ ranger "r-9", { X = 28; Y = 47 } ]

                // Moved by #654: the split asked each row for its own
                // trespassers (two guards and a ranger here); the one cast is
                // the cheapest set that kills the most of them, two rangers,
                // which reach the hauler a swing never catches. The guards'
                // seats are still the split's (the cap below).
                Expect.equal
                    (asksOf (declaredRaid [ hauler; seatedDigger ]))
                    [ CastRow.Fighter BodyTemplate.Ranged, 2 ]
                    "one cast for both, of the template that reaches both"

                Expect.equal
                    (asksOf
                        { declaredRaid [ hauler ] with
                            Bank = bank 550 550
                        })
                    [ CastRow.Fighter BodyTemplate.Melee, 1 ]
                    "with no block castable, one melee block, as the split asked"

                Expect.equal
                    (snd (readHolding [ "r-9" ] (declaredRaid [ hauler; seatedDigger ] |> lending)))
                    (Some 1)
                    "a ranger holding the kill takes the hauler off the guards' seats"

                Expect.equal
                    (snd (
                        readHolding
                            []
                            (declaredRaid [ hauler ] |> withBodyAtHome (ranger "r-9") atSpawn)
                    ))
                    (Some 1)
                    "a lendable ranger elsewhere takes nothing off the guards' seats"

                // The ranger's lend reads its home's quiet and where it stands,
                // which may flip tick to tick; the guard's seat reads neither.
                Expect.equal
                    (snd (readHolding [] (declaredRaid [ hauler ] |> lending)))
                    (snd (readHolding [] (declaredRaid [ hauler ])))
                    "the guard's cap is one with a ranger to lend or none"

                let haulers = [ for i in 1..4 -> haulerOf $"haul-{i}" { X = 20 + i; Y = 44 } ]

                Expect.equal
                    (asksOf (declaredRaid haulers))
                    [ CastRow.Fighter BodyTemplate.Ranged, Engine.guardCap ]
                    "four haulers buy no more than a raid's room of bodies (#654: a row each)"

                let holders held colony =
                    (decide colony held Set.empty None).Assignments
                    |> Map.filter (fun _ task -> task = taskId (Guard "W1N2"))
                    |> Map.keys
                    |> Set.ofSeq

                Expect.equal
                    (holders
                        Map.empty
                        (declaredRaid [ hauler; seatedDigger ]
                         |> withGuards
                             [ guard "g-1", { X = 27; Y = 47 }; ranger "r-9", { X = 28; Y = 47 } ]))
                    (Set.ofList [ "g-1"; "r-9" ])
                    "a guard and a ranger hold the kill together"

                Expect.equal
                    (holders
                        (Map.ofList [ "r-9", taskId (Guard "W1N2") ])
                        (declaredRaid [ hauler; seatedDigger ]
                         |> withGuards
                             [
                                 guard "g-1", { X = 27; Y = 47 }
                                 guard "g-2", { X = 26; Y = 47 }
                                 ranger "r-9", { X = 28; Y = 47 }
                             ])
                     |> Set.count)
                    2
                    "beside the ranger holding it, a second guard is no answer to the hauler"
            }

            test "a body on the exit row of the room beyond keeps the kill across the bounce" {
                let colony = raisingBeyond [ diggerAt "W1N3" { X = 25; Y = 49 } ]
                let ground = groundOf colony

                Expect.isSome
                    (pooledOf colony |> entryFor (Guard "W1N2"))
                    "the outpost's Guard is pooled for the body the exit carries in"

                Expect.isTrue
                    (not (Set.isEmpty ground)
                     && ground |> Set.forall (fun tile -> tile.Y = 1 && abs (tile.X - 25) <= 1))
                    "beside where it lands, off the exit"
            }

            test
                "a raid's healer on the exit row beyond is the kill an Invader's is, never an ally's or a neutral's (#597)" {
                let healerOf owner =
                    { hostileIn "W1N3" { X = 25; Y = 49 } smallHealer with
                        Owner = owner
                    }

                let pooled owner =
                    pooledOf (raisingBeyond [ healerOf owner ])
                    |> entryFor (Guard "W1N2")
                    |> Option.isSome

                Expect.isTrue (pooled "Invader") "the premise: an Invader's is carried in, a kill"
                Expect.isFalse (pooled "Odiodin") "an ally's passing healer calls no guard out"
                Expect.isFalse (pooled "Kalgen") "nor a neutral's, which carried no weapon"
            }

            test "a healer whose raider stands just across the exit is still that raid's (#597)" {
                let healer =
                    { hostileIn "W1N2" { X = 25; Y = 1 } smallHealer with
                        Owner = "Invader"
                    }

                let raider =
                    { hostileIn "W1N3" { X = 25; Y = 47 } smallRanged with
                        Id = "h-2"
                        Owner = "Invader"
                    }

                let dutyIn hostiles =
                    let colony = raisingBeyond hostiles
                    let brief = Threats.briefOf (threatsOf colony (Atlas.ofView colony)) "W1N2"
                    brief.Duty, Brief.isKill brief

                Expect.equal
                    (dutyIn [ healer ])
                    (Duty.Guard BriefKind.Contested, true)
                    "the premise: alone, it is the contested room's prey, a kill"

                Expect.notEqual
                    (dutyIn [ healer; raider ])
                    (Duty.Guard BriefKind.Contested, true)
                    "with a ranged raider next door no trespass kill sends a guard into its reach"
            }

            test
                "defence first: a fresh ranger takes the raided child's Guard, not a trespass at home" {
                let raid =
                    { hostileIn "W1N3" { X = 40; Y = 40 } smallMelee with
                        Id = "h-2"
                    }

                let colony =
                    raisingBeyond [ diggerAt "W1N1" { X = 25; Y = 5 }; raid ]
                    |> withBodyAtHome (ranger "r-5") atSpawn

                let result = decide colony residentHeld Set.empty None

                Expect.equal
                    (Map.tryFind "r-5" result.Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "the raid is answered first"

                let quiet =
                    decide
                        (raisingBeyond [ diggerAt "W1N1" { X = 25; Y = 5 } ]
                         |> withBodyAtHome (ranger "r-5") atSpawn)
                        residentHeld
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "r-5" quiet.Assignments)
                    (Some(taskId (Guard "W1N1")))
                    "the premise: with no raid it takes the trespass"
            }

            test "a neutral player's hauler in our outpost pools nothing" {
                let giaco =
                    { hostileIn
                          "W1N2"
                          { X = 25; Y = 44 }
                          (List.replicate 10 Carry @ List.replicate 10 Move) with
                        Owner = "giaco"
                    }

                Expect.isNone
                    (pooledOf (declaredRaid [ giaco ]) |> entryFor (Guard "W1N2"))
                    "only a declared enemy trespasses"
            }

            test "an ally's body and an Invader's digger pool none" {
                let guardIn colony =
                    pooledOf colony
                    |> List.exists (fun e ->
                        match e.Task with
                        | Guard _ -> true
                        | _ -> false)

                Expect.isFalse
                    (guardIn
                        { declaredRaid [] with
                            Allies = [ diggerAt "W1N2" { X = 25; Y = 44 } ]
                        })
                    "an ally's digger is no hostile"

                Expect.isFalse
                    (guardIn (
                        declaredRaid
                            [
                                { diggerAt "W1N2" { X = 25; Y = 44 } with
                                    Owner = "Invader"
                                }
                            ]
                    ))
                    "an Invader's body is the NPC's, no player's"
            }

            test "a melee guard may take it and swings from beside it, and no row casts for it" {
                let at = { X = 25; Y = 44 }

                let colony =
                    declaredRaid [ diggerAt "W1N2" at ]
                    |> withGuards [ guard "g-1", { X = 24; Y = 45 } ]

                let result = decideOn colony

                Expect.equal
                    (Map.tryFind "g-1" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the guard takes the kill"

                Expect.contains (attacksOf result.Intents) ("g-1", "h-1") "and swings"

                Expect.equal (guardQuotaOf colony) (Some 0) "no guard is cast for it"

                Expect.equal
                    (rowOf "ranger" colony |> Option.map (fun row -> row.Quota))
                    (Some 0)
                    "nor a ranger"
            }

            test "nobody qualifying, nothing takes it" {
                let colony = declaredRaid [ diggerAt "W1N2" { X = 25; Y = 44 } ]

                Expect.isFalse
                    ((decideOn colony).Assignments
                     |> Map.exists (fun _ tid -> tid = taskId (Guard "W1N2")))
                    "the anchor and the haulers are no fighters"
            }

            // #652, live W17S26 t~1,030,262: Trepidimous' 5W3M miner on its
            // seat and 23C23M hauler worked W17S25's outpost unanswered, no
            // row casting for a trespass and no holder within the kill's
            // walk.
            test
                "a miner and a hauler worth their casts in an outpost no body reaches: one ranger is cast for both, and a body of either template takes the kill" {
                let trespassers = [ seatedDigger; haulerOf "hauler-1" { X = 27; Y = 44 } ]
                let colony = paying (declaredRaid trespassers)

                // The kill is shown by `Facts.castBodiesPaid`'s own
                // `killTicksOf` (the body alone at its standoff); this pins
                // the casts and the seat, the arena the kill's ticks.
                // Moved by #654 (the issue's own case): the split cast a guard
                // for the miner and a ranger for the hauler; one ranger kills
                // both, the cheapest set that kills the most.
                Expect.equal
                    (killCasts "W1N2" (decideOn colony))
                    [ CastRow.Fighter BodyTemplate.Ranged, 1, Some [ "d-1"; "hauler-1" ] ]
                    "one ranger for the miner and the hauler at its pace"

                // The bodies those casts are, standing at the spawn: each
                // is matched to the kill's seat (#647: the cast and the seat
                // come from the same Brief).
                for body in
                    [
                        creepWith "guard-1-Spawn1" 0 0 guardPattern.Block
                        creepWith "ranger-1-Spawn1" 0 0 rangerPattern.Block
                    ] do
                    let cast = colony |> withBodyAtHome body atSpawn

                    Expect.equal
                        (Map.tryFind body.Name (decideOn cast).Assignments)
                        (Some(taskId (Guard "W1N2")))
                        $"the cast {body.Name} takes the outpost's kill"
            }

            // #622 (b): a trespass's kill is a seat priced by what it lands
            // on its cost (`Facts.castReturn`, the Caster's own price), cast
            // after the economy's seats that return more — where the fighting
            // rows once cast ahead of every economy row, so two trespass
            // guards jumped a home Anchor's refill and the home lost income.
            test
                "a trespass's kill is cast after the seats that return more: the empty Post, the reservation" {
                let colony = paying (declaredRaid [ seatedDigger ])

                let first colony =
                    spawnIntents (decideOn colony).Intents
                    |> List.map (fun (_, _, name: string) -> name.Split('-').[0])

                Expect.equal
                    (killCasts "W1N2" (decideOn colony))
                    [ CastRow.Fighter BodyTemplate.Melee, 1, Some [ "d-1" ] ]
                    "the premise: the miner on its seat is a kill worth its guard"

                Expect.equal
                    (first
                        { colony with
                            Creeps = colony.Creeps |> List.filter (fun c -> c.Name <> "a-out")
                        })
                    [ "anchor" ]
                    "the outpost's Post standing empty, its Anchor is cast before the guard"

                Expect.equal (first colony) [ "reserver" ] "and with it manned, the reservation is"
            }

            test
                "a body worth less than its cast, a bare scout or a body shot disarmed is cast for by no row" {
                let cheap =
                    { hostileIn "W1N2" { X = 24; Y = 41 } [ Work; Move ] with
                        Id = "d-2"
                        Owner = "Trepidimous"
                    }

                let scout =
                    { hostileIn "W1N2" { X = 27; Y = 44 } [ Move ] with
                        Owner = "Trepidimous"
                    }

                // 1,300 of body, its ten ATTACK shot off: a trespasser, but
                // no economy of the enemy's (spar's squads: their cripples
                // bought casts until the prey was the enemy's economy alone).
                let disarmed =
                    { hostileIn
                          "W1N2"
                          { X = 27; Y = 44 }
                          (List.replicate 10 Attack @ List.replicate 10 Move) with
                        Owner = "Trepidimous"
                        Hits = 10 * Engine.partHits
                    }

                for hostiles, case in
                    [
                        [ cheap ], "a 1W1M digger on its seat: 262 energy against a 750 guard"
                        [ scout ], "a lone scout: no trespasser at all"
                        [ disarmed ], "a body shot disarmed: the fight's to finish, not a cast's"
                    ] do
                    Expect.isEmpty
                        (killCasts "W1N2" (decideOn (paying (declaredRaid hostiles))))
                        case
            }

            // #652 review S1, R3: the cast is spent out of the economy it
            // buys back, so a colony's own kill is cast only past its bank's
            // floor; the stock's reserve is the bar for lending to another.
            test
                "a colony under its bank's floor casts nothing for its own trespasser, at it with no stock it does" {
                let raided = declaredRaid [ seatedDigger; haulerOf "hauler-1" { X = 27; Y = 44 } ]
                let floor = Tuning.defaults.CasterFloor

                Expect.isEmpty
                    (killCasts
                        "W1N2"
                        (decideOn
                            { raided with
                                Bank = bank (floor - 1) (floor - 1)
                            }))
                    "a bank under CasterFloor"

                Expect.isNonEmpty
                    (killCasts "W1N2" (decideOn (paying raided)))
                    "a bank at CasterFloor, no Storage at all"
            }

            test "a holder called to the kill within its walk is the answer: no row casts for it" {
                let colony = paying (raisingBeyond [ seatedDigger ])

                let called = decide colony residentHeld Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" called.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the premise: the quiet resident is called to the miner"

                Expect.isEmpty (killCasts "W1N2" called) "and nothing is cast for it"

                Expect.equal
                    (killCasts "W1N2" (decideOn colony) |> List.map (fun (row, _, _) -> row))
                    [ CastRow.Fighter BodyTemplate.Melee ]
                    "the same miner with no holder to call is cast for"
            }

            // #652 review A2: the cast reads the Matcher's own call, one
            // holder to one kill, so a resident in walk of two kills answers
            // one and the other casts.
            test "one resident in walk of two trespassed outposts is called to one: the other casts" {
                let second =
                    { seatedDigger with
                        Id = "d-3"
                        Pos = RoomPos.at "W2N3" { X = 24; Y = 25 }
                    }

                let called =
                    decide
                        (paying (raisingBesideTwo [ seatedDigger; second ]))
                        residentHeld
                        Set.empty
                        None

                // Both border its home; W2N3's miner is the nearer walk,
                // about 50 tiles to W1N2's 64.
                Expect.equal
                    (Map.tryFind "r-1" called.Assignments)
                    (Some(taskId (Guard "W2N3")))
                    "the premise: the resident is called to the nearer kill"

                Expect.isEmpty (killCasts "W2N3" called) "the kill it answers casts nothing"

                Expect.equal
                    (killCasts "W1N2" called)
                    [ CastRow.Fighter BodyTemplate.Melee, 1, Some [ "d-1" ] ]
                    "and the other outpost's miner is cast for"
            }

            // #652 re-review R1: a chase is always raided, which only the
            // match prices; its call takes the resident from the trespass
            // all the same, so the trespass casts.
            test "a resident called to a nearer chase leaves the trespass beside it to a cast" {
                let chased =
                    { crippledAt { X = 25; Y = 10 } with
                        Id = "h-chase"
                        Pos = RoomPos.at "W2N3" { X = 45; Y = 25 }
                    }

                let called =
                    decide
                        (paying (raisingBesideTwo [ seatedDigger; chased ]))
                        residentHeld
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "r-1" called.Assignments)
                    (Some(taskId (Guard "W2N3")))
                    "the premise: the resident takes the chase"

                Expect.equal
                    (killCasts "W1N2" called)
                    [ CastRow.Fighter BodyTemplate.Melee, 1, Some [ "d-1" ] ]
                    "and the outpost's miner is cast for"
            }

            // #652 review A5, the #647 invariant across colonies: the cast
            // the Caster hands another colony is matched to the kill's seat.
            test
                "a trespass in the mother's outpost the child lands is cast by the child, and its guard takes the kill" {
                let outpost, mother, child =
                    ViewFixtures.outpost, ViewFixtures.mother, ViewFixtures.child

                let decided, decisionOf = trespassedPairDecided []

                Expect.isEmpty decided.Faulted "the premise: both colonies decide"

                Expect.equal
                    (killCasts outpost (decisionOf child))
                    [ CastRow.Fighter BodyTemplate.Melee, 1, Some [ "raptor-out" ] ]
                    "the child, past both floors, is handed the guard for the miner"

                Expect.isEmpty
                    (killCasts outpost (decisionOf mother))
                    "and the mother, under her bank's floor, casts none"

                let decided, decisionOf =
                    trespassedPairDecided [ "guard-960-Spawn2", guardPattern.Block ]

                Expect.isEmpty decided.Faulted "the premise: both colonies decide, the guard cast"

                Expect.equal
                    (Map.tryFind "guard-960-Spawn2" (decisionOf child).Assignments)
                    (Some(taskId (Guard outpost)))
                    "the child's cast guard takes the mother's kill"
            }

            test "a ranger shoots it from three" {
                let colony =
                    declaredRaid [ diggerAt "W1N2" { X = 25; Y = 44 } ]
                    |> withGuards [ ranger "r-9", { X = 25; Y = 47 } ]

                let result = decideOn colony

                Expect.equal
                    (Map.tryFind "r-9" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the ranger takes the kill"

                Expect.contains result.Intents (RangedAttackCreep("r-9", "h-1")) "and shoots"
            }

            test
                "a resident of the bordering home takes it while home is quiet, and walks back once it dies" {
                let called =
                    decide
                        (raisingBeyond [ diggerAt "W1N2" { X = 25; Y = 10 } ])
                        residentHeld
                        Set.empty
                        None

                Expect.contains
                    called.Verdicts
                    (Verdict.Released("r-1", taskId (Guard "W1N3"), ReleaseReason.Called))
                    "the resident is let go of its quiet ring"

                Expect.equal
                    (Map.tryFind "r-1" called.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "and takes the kill"

                let after =
                    raisingBeyond []
                    |> standingIn "W1N2" ("r-1", { X = 25; Y = 12 })
                    |> withOutpostLayer "W1N3" (fun layer ->
                        { layer with
                            CreepPositions = Map.empty
                        })

                let back = decide after (Map.ofList [ "r-1", taskId (Guard "W1N2") ]) Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" back.Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "with the trespasser dead it holds its home's ring again"
            }

            test "a resident is not sent to a room beside our home and not its own" {
                let result =
                    decide
                        (raisingBeyond [ diggerAt "W1N1" { X = 25; Y = 5 } ])
                        residentHeld
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "r-1" result.Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "W1N1 does not border W1N3"
            }

            test
                "an armed body at home keeps the resident home, and recalls it from the kill at once" {
                let raid =
                    { hostileIn "W1N3" { X = 40; Y = 40 } smallMelee with
                        Id = "h-2"
                    }

                let kept =
                    decide
                        (raisingBeyond [ diggerAt "W1N2" { X = 25; Y = 10 }; raid ])
                        residentHeld
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "r-1" kept.Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "a raid in its home keeps it there"

                let recalled =
                    decide
                        (raisingBeyond [ diggerAt "W1N2" { X = 25; Y = 10 }; raid ]
                         |> standingIn "W1N2" ("r-1", { X = 25; Y = 12 })
                         |> withOutpostLayer "W1N3" (fun layer ->
                             { layer with
                                 CreepPositions = Map.empty
                             }))
                        (Map.ofList [ "r-1", taskId (Guard "W1N2") ])
                        Set.empty
                        None

                Expect.contains
                    recalled.Verdicts
                    (Verdict.Released(
                        "r-1",
                        taskId (Guard "W1N2"),
                        ReleaseReason.Rejected RejectReason.Inapplicable
                    ))
                    "a raid arriving there lets it go of the kill, still pooled"

                Expect.equal
                    (Map.tryFind "r-1" recalled.Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "and it holds its home again"
            }

            test
                "a raid in outpost A withholds no trespass kill in B from a guard A leaves spare (#528)" {
                let raid = hostileIn "W1N2" { X = 25; Y = 42 } smallMelee

                let digger =
                    { diggerAt "W1N3" { X = 25; Y = 20 } with
                        Id = "h-2"
                    }

                let colony =
                    secondOutpost
                        [ raid; digger ]
                        [ guard "g-1", beside; guard "g-2", { X = 25; Y = 30 } ]

                let result =
                    decide colony (Map.ofList [ "g-1", taskId (Guard "W1N2") ]) Set.empty None

                Expect.equal
                    (Map.tryFind "g-1" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the raid's guard holds the raid"

                Expect.equal
                    (Map.tryFind "g-2" result.Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "and the spare guard takes B's trespasser"
            }

            test "defence first: a lone guard takes outpost A's raid over B's trespasser (#528)" {
                let raid = hostileIn "W1N2" { X = 25; Y = 42 } smallMelee

                let digger =
                    { diggerAt "W1N3" { X = 25; Y = 20 } with
                        Id = "h-2"
                    }

                let colony = secondOutpost [ raid; digger ] [ guard "g-1", { X = 25; Y = 30 } ]

                Expect.equal
                    (Map.tryFind "g-1" (decideOn colony).Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the raid's Guard ranks over the trespass's kill"
            }

            test "a declared enemy's body shot disarmed in our outpost is a trespasser (#528)" {
                let colony =
                    declaredRaid [ disarmedAt "Trepidimous" "W1N2" { X = 25; Y = 44 } ]
                    |> withGuards [ guard "g-1", { X = 25; Y = 47 } ]

                let atlas = Atlas.ofView colony
                let brief = Threats.briefOf (threatsOf colony atlas) "W1N2"

                Expect.equal
                    (brief.Duty, Brief.isKill brief)
                    (Duty.Guard BriefKind.Contested, true)
                    "its Guard is the contested room's prey, a kill"

                Expect.isSome (pooledOf colony |> entryFor (Guard "W1N2")) "pooled"

                Expect.equal
                    (Map.tryFind "g-1" (decideOn colony).Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "and taken"
            }

            test "a guard three off its trespasser aims at it, for the light ticks to swing (#528)" {
                let colony =
                    declaredRaid [ diggerAt "W1N2" { X = 25; Y = 44 } ]
                    |> withGuards [ guard "g-1", { X = 25; Y = 47 } ]

                let result = decideOn colony

                Expect.equal
                    (Map.tryFind "g-1" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the premise: it holds the kill"

                Expect.isFalse
                    (List.contains (AttackCreep("g-1", "h-1")) result.Intents)
                    "out of reach, no swing this tick"

                Expect.equal result.Aims [ AttackCreep("g-1", "h-1") ] "but its aim is recorded"

                Expect.isEmpty (decideOn (declaredRaid [])).Aims "a quiet colony aims at nothing"
            }

            // #535 made the disarmed clause owner-agnostic (any non-ally's): a
            // cripple left in a room we mine is a fight's to finish, whoever's.
            test
                "a neutral's disarmed body is a trespasser too, an ally's and a bare enemy scout are not (#528, #535)" {
                Expect.isSome
                    (pooledOf (declaredRaid [ disarmedAt "giaco" "W1N2" { X = 25; Y = 44 } ])
                     |> entryFor (Guard "W1N2"))
                    "a neutral's cripple in our outpost"

                Expect.isNone
                    (pooledOf (declaredRaid [ disarmedAt "Odiodin" "W1N2" { X = 25; Y = 44 } ])
                     |> entryFor (Guard "W1N2"))
                    "never an ally's"

                Expect.isNone
                    (pooledOf (
                        declaredRaid
                            [
                                { hostileIn "W1N2" { X = 25; Y = 44 } [ Move ] with
                                    Owner = "Trepidimous"
                                }
                            ]
                     )
                     |> entryFor (Guard "W1N2"))
                    "a scout carried no weapon to lose"
            }

            // #597: spar outpost-squad's last healer, every HEAL shot off,
            // its 1 MOVE left at 100 hits, outlived the armed raiders and
            // nothing hunted it.
            test
                "a raid's healer left in our outpost is a trespasser, its HEAL live or shot off; a neutral's unarmed body is not (#597)" {
                let remnant owner =
                    { hostileIn
                          "W1N2"
                          { X = 25; Y = 44 }
                          (List.replicate 4 Move @ List.replicate 5 Heal @ [ Move ]) with
                        Owner = owner
                        Hits = 100
                    }

                let colony =
                    declaredRaid [ remnant "Invader" ]
                    |> withGuards [ guard "g-1", { X = 25; Y = 47 } ]

                let brief = Threats.briefOf (threatsOf colony (Atlas.ofView colony)) "W1N2"

                Expect.equal
                    (brief.Duty, Brief.isKill brief)
                    (Duty.Guard BriefKind.Contested, true)
                    "an Invader's healer with no live HEAL is the contested room's prey, a kill"

                Expect.equal
                    (Map.tryFind "g-1" (decideOn colony).Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "and the guard takes it"

                let pooled hostile =
                    pooledOf (declaredRaid [ hostile ]) |> entryFor (Guard "W1N2") |> Option.isSome

                Expect.isTrue (pooled (remnant "Trepidimous")) "a declared enemy's remnant"

                Expect.isTrue
                    (pooled
                        { hostileIn "W1N2" { X = 25; Y = 44 } smallHealer with
                            Owner = "Invader"
                        })
                    "an Invader's healer whose HEAL still acts, its raid dead"

                Expect.isFalse (pooled (remnant "Odiodin")) "never an ally's"

                Expect.isFalse
                    (pooled (remnant "giaco"))
                    "nor a neutral's healer, which carried no weapon (`mayStrike`)"

                // A neutral's hauler is "a neutral player's hauler in our
                // outpost pools nothing".
                Expect.isFalse
                    (pooled
                        { hostileIn "W1N2" { X = 25; Y = 44 } [ Move ] with
                            Owner = "giaco"
                        })
                    "nor a neutral's scout"
            }

            // #598: arena "a hurt pair retreats", Medic1's HEAL shot off
            // while Brute1 still walked our room; the ranger in reach of it
            // held its fire.
            test
                "a healer shot bare beside its armed raider is struck while the raider lives (#598)" {
                let bare =
                    { hostileIn
                          "W1N2"
                          { X = 25; Y = 46 }
                          (List.replicate 4 Move @ List.replicate 5 Heal @ [ Move ]) with
                        Owner = "Invader"
                        Hits = 100
                    }

                let raider =
                    { hostileIn "W1N2" { X = 25; Y = 40 } smallMelee with
                        Id = "h-2"
                        Owner = "Invader"
                    }

                let result =
                    declaredRaid [ bare; raider ]
                    |> withGuards [ guard "g-1", { X = 25; Y = 47 } ]
                    |> decideOn

                Expect.equal
                    (Map.tryFind "g-1" result.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the premise: the guard holds the raid's Guard"

                Expect.contains
                    result.Intents
                    (AttackCreep("g-1", "h-1"))
                    "and swings at the bare healer beside it, the raider out of reach"
            }
        ]

[<Tests>]
let contestTests =
    testList
        "an outpost as the economic war over it (#656)"
        [
            // The W17S26 pair (#550, #652) in our reserved outpost: its delta
            // is our yield kept and what the pair takes a tick, denied.
            test "a digger and a hauler in our outpost: our yield kept, their take denied" {
                let colony = declaredRaid [ seatedDigger; haulerOf "h-2" { X = 27; Y = 43 } ]

                let contest = Facts.contestOf colony "W1N2"

                // Seated, the digger takes its five WORK's dig up to the held
                // rock's output; the empty hauler a load a round trip of a
                // room's side each way.
                let dug = float (min (5 * Engine.harvestPerWork) Engine.heldOutputPerTick)

                let hauled = float (23 * Engine.carryPartCapacity) / float (2 * Engine.roomSide)

                Expect.equal
                    contest.Kept
                    (float Engine.heldOutputPerTick)
                    "its one held source's yield, kept"

                Expect.equal
                    contest.Takes
                    (Map.ofList [ "d-1", dug; "h-2", hauled ])
                    "each economy body's take"

                Expect.equal
                    (Contest.delta contest)
                    (contest.Kept + dug + hauled)
                    "the delta: ours kept and theirs denied"

                Expect.equal
                    (contest.Values |> Map.keys |> List.ofSeq)
                    [ "d-1"; "h-2" ]
                    "each priced at its kill value too"
            }

            test "a raid alone, or a neutral's hauler, denies nothing" {
                let raid = Facts.contestOf (declaredRaid raiders) "W1N2"

                let neutral =
                    Facts.contestOf
                        (declaredRaid
                            [
                                { haulerOf "h-2" { X = 27; Y = 43 } with
                                    Owner = "Somebody"
                                }
                            ])
                        "W1N2"

                Expect.isEmpty raid.Takes "an Invader's raid takes none of our economy itself"
                Expect.equal (Contest.delta raid) raid.Kept "the raid's delta is our yield alone"
                Expect.isEmpty neutral.Takes "a neutral's body is nobody's to deny"
            }
        ]

[<Tests>]
let residentChaseTests =
    testList
        "a resident lent to a chase beside its home (#516)"
        [
            test
                "the W18S25 shape: a crippled melee next door calls the quiet resident, and a raid at home recalls it" {
                let called =
                    decide
                        (raisingBeyond [ crippledAt { X = 25; Y = 10 } ])
                        residentHeld
                        Set.empty
                        None

                Expect.contains
                    called.Verdicts
                    (Verdict.Released("r-1", taskId (Guard "W1N3"), ReleaseReason.Called))
                    "the resident is let go of its quiet ring"

                Expect.equal
                    (Map.tryFind "r-1" called.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "and takes the chase"

                let raid =
                    { hostileIn "W1N3" { X = 40; Y = 40 } smallMelee with
                        Id = "h-2"
                    }

                let recalled =
                    decide
                        (raisingBeyond [ crippledAt { X = 25; Y = 10 }; raid ]
                         |> standingIn "W1N2" ("r-1", { X = 25; Y = 14 })
                         |> withOutpostLayer "W1N3" (fun layer ->
                             { layer with
                                 CreepPositions = Map.empty
                             }))
                        (Map.ofList [ "r-1", taskId (Guard "W1N2") ])
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "r-1" recalled.Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "a raid at home calls it back off the chase"
            }

            test
                "a healer bouncing on the home's exit keeps the resident home on the tick it is across" {
                // #552, live t981,3xx: the resident was called to W18S25 each
                // tick the pair stood across the border from W17S25, and
                // released `task-gone` the next.
                let bouncing =
                    { raisingBeyond [ crippledAt { X = 25; Y = 10 } ] with
                        RecalledHealers =
                            [
                                { hostileIn "W1N3" { X = 25; Y = 49 } [ Heal; Move ] with
                                    Id = "med-1"
                                    Owner = "Trepidimous"
                                }
                            ]
                    }

                let held = decide bouncing residentHeld Set.empty None

                Expect.equal
                    (Map.tryFind "r-1" held.Assignments)
                    (Some(taskId (Guard "W1N3")))
                    "the home is not quiet: no lend"
            }

            // #648, #621: a body shot disarmed in the raised home is worth its
            // kill, and the garrison's ground follows it off the controller's
            // ring; never while an armed raider bounces on the home's exit,
            // across the border this tick, whose line the ring holds first.
            test
                "a disarmed body in the raised home draws the garrison off its ring, but not while a raider bounces on its exit" {
                let brute = disarmedAt "Trepidimous" "W1N3" { X = 40; Y = 40 }

                let raider =
                    { hostileIn "W1N2" { X = 25; Y = 0 } [ RangedAttack; RangedAttack; Move; Move ] with
                        Id = "raider"
                        Owner = "Trepidimous"
                    }

                let groundIn colony =
                    let atlas = Atlas.ofView colony

                    let threats =
                        threatsOfHeld colony atlas (Facts.heldTaskFacts colony residentHeld)

                    (Threats.briefOf threats "W1N3").Ground |> Option.defaultValue Set.empty

                let besideBrute (ground: Set<RoomPos>) =
                    not (Set.isEmpty ground)
                    && ground
                       |> Set.forall (fun tile ->
                           rangeTo (RoomPos.pos brute.Pos) tile <= Engine.rangedRange)

                Expect.isTrue
                    (besideBrute (groundIn (raisingBeyond [ brute ])))
                    "alone in the home, the ground is within three of the brute"

                Expect.isFalse
                    (besideBrute (groundIn (raisingBeyond [ brute; raider ])))
                    "with a raider on the exit, the garrison holds its ring"
            }
        ]
