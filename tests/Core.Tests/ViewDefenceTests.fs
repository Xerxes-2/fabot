/// What the colony view carries of a defended home: the guards a home pools,
/// the garrison, the child's outposts under a raid, the world's Seam and the
/// safe-mode facts.
module Fabot.Core.Tests.ViewDefenceTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests.ViewFixtures

/// The pair world a few levels on: the child independent at RCL4, so its
/// mother raises it no more and projects nothing of it on a quiet tick, with
/// one tower per entry of `towers`, holding that much energy.
let private independentChild (towers: int list) : World =
    { pairWorld with
        Rooms =
            pairWorld.Rooms
            |> Map.change
                child
                (Option.map (fun (facts: RoomFacts) ->
                    { facts with
                        Controller =
                            facts.Controller |> Option.map (fun ctrl -> { ctrl with Level = 4 })
                        Refillables =
                            towers
                            |> List.mapi (fun i energy ->
                                {
                                    Id = $"tower-{i}"
                                    FreeCapacity = Engine.towerCapacity - energy
                                    Kind = BuiltKind.Tower
                                })
                    }))
    }

/// One change to one room.
let private inRoom (room: string) (change: RoomFacts -> RoomFacts) (world: World) : World =
    { world with
        Rooms = world.Rooms |> Map.change room (Option.map change)
    }

/// One change to the child's room.
let private inChild = inRoom child

/// A room's bank, full at `energy`.
let private banked (energy: int) (facts: RoomFacts) : RoomFacts =
    { facts with
        Energy =
            {
                Available = energy
                Capacity = energy
            }
    }

/// A squad in the child's home: one attacker and two healers of `heals`
/// HEAL parts each, W17S29's live raid being two of seven.
let private raidedBy owner (heals: int) =
    inChild (fun facts ->
        { facts with
            Hostiles =
                [
                    "atk", [ Attack; Move ]
                    "med-1", List.replicate heals Heal @ [ Move ]
                    "med-2", List.replicate heals Heal @ [ Move ]
                ]
                |> List.mapi (fun i (id, parts) ->
                    {
                        Id = id
                        Owner = owner
                        Pos = { Room = child; X = 5 + i; Y = 5 }
                        Body = parts
                        Boosts = []
                        Hits = Engine.partHits * List.length parts
                        TicksToLive = 1000
                    })
        })

/// The one-tower child under a Trepidimous squad of `heals` a healer.
let private raided heals =
    independentChild [ 1000 ] |> raidedBy "Trepidimous" heals

/// The same squad with a pure dismantler (WORK and MOVE, no weapon) in the
/// melee's place.
let private breached heals =
    raided heals
    |> inChild (fun facts ->
        { facts with
            Hostiles =
                facts.Hostiles
                |> List.map (fun h ->
                    if h.Id = "atk" then
                        { h with
                            Body = [ Work; Work; Move; Move ]
                            Hits = 4 * Engine.partHits
                        }
                    else
                        h)
        })

let private underSafeMode =
    inChild (fun facts ->
        { facts with
            Control = facts.Control |> Option.map (fun ctrl -> { ctrl with SafeMode = true })
        })

let private heldBy owner =
    inChild (fun facts ->
        { facts with
            Control = Some(control owner)
        })

/// A guard of ours standing in `room`; whose it is, is the spawn in its name.
let private withGuard name room (world: World) =
    let guard = creep name room

    { world with
        Creeps =
            { guard with
                Info =
                    { guard.Info with
                        Body = Map.ofList [ Attack, 3; Move, 5; Heal, 1 ]
                    }
            }
            :: world.Creeps
        Rooms =
            world.Rooms
            |> Map.change
                room
                (Option.map (fun facts ->
                    { facts with
                        Layer =
                            { facts.Layer with
                                CreepPositions =
                                    Map.add name { X = 5; Y = 7 } facts.Layer.CreepPositions
                            }
                    }))
    }

/// The Guards a colony pools off the view the shell builds for it.
let private guardsPooled (view: ColonyView) =
    let atlas = Atlas.ofView view

    Planner.planTasks
        view
        atlas
        (threatsOf view atlas)
        HeldTaskFacts.empty
        (Planner.outpostFactsOf view (Fabot.Core.Atlas.ofView view))
    |> List.filter (function
        | Guard _ -> true
        | _ -> false)

/// The mother's guard, and one of the child's: `Spawn1` is hers, `Spawn2`
/// the child's.
let private hers = "guard-990-Spawn1"
let private childs = "guard-990-Spawn2"

/// A child two hops from the mother, W13S28 between them as a room nobody
/// declares, for the chain a guard walks home along.
let private farChild = "W14S28"

let private farDeclared: Colony list =
    declared
    |> List.map (fun colony ->
        if colony.Home = child then
            { colony with Home = farChild }
        else
            colony)

let private farWorld: World =
    let rooms =
        pairWorld.Rooms
        |> Map.remove child
        |> unseen child
        |> Map.add
            farChild
            (roomOf farChild Ownership.Ours [ $"ctrl-{farChild}", { X = 8; Y = 8 }, Controller ]
             |> ourColony "Spawn2" 4 1300
             |> snd)

    { pairWorld with
        Rooms = rooms
        Creeps = [ creep "worker-900-Spawn1" mother ]
    }

[<Tests>]
let defendedHomeTests =
    testList
        "another colony defends a home its towers cannot hold"
        [
            test
                "the mother projects a raided child's home, as a room she crosses and works nothing in" {
                // W17S29's live raid (#428): 14 HEAL parts put back 168 a tick
                // against the 150 its one tower lands at the falloff range.
                let raided = viewOf (raided 7) mother
                let quiet = viewOf (independentChild [ 1000 ]) mother

                Expect.isEmpty quiet.Borrowed.Defended "the premise: a quiet child is none of hers"

                Expect.isFalse
                    (Map.containsKey child quiet.Spatial.Rooms)
                    "and she projects nothing of an independent child on a quiet tick"

                Expect.equal
                    raided.Borrowed.Defended
                    [ child ]
                    "raided, the child's home is hers to defend"

                Expect.isTrue
                    (Map.containsKey child raided.Spatial.Rooms)
                    "so its ground is in her projection"

                Expect.containsAll
                    (raided.Hostiles |> List.map (fun h -> h.Id))
                    [ "atk"; "med-1"; "med-2" ]
                    "and the raid stands in her view"

                Expect.isFalse
                    ([ "ctrl-W13S28"; "spawn-child"; "site-child"; "can-child" ]
                     |> List.exists (fun id -> Map.containsKey id raided.Spatial.TargetKinds))
                    "but nothing of the child's is work of hers: the room narrows as a transit room"

                Expect.isFalse
                    (List.contains child raided.Borrowed.Rooms)
                    "and it is no room she raises"
            }

            test
                "a dismantler the healers keep alive under the towers beats the home as a melee does" {
                // #429: the breacher deals a creep nothing and razes the home.
                Expect.equal
                    (viewOf (breached 7) mother).Borrowed.Defended
                    [ child ]
                    "the child's home is hers to defend"

                Expect.isEmpty
                    (viewOf (breached 0) mother).Borrowed.Defended
                    "and one the towers kill is none of hers"
            }

            test
                "one world, both views: the mother pools the Guard of the beaten home, the child none" {
                let world = raided 7

                Expect.equal
                    (guardsPooled (viewOf world mother))
                    [ Guard child ]
                    "the mother's pool holds the Guard, under the child's name"

                Expect.isEmpty (guardsPooled (viewOf world child)) "and the child's holds none"
            }

            test "a child under safe mode is not defended" {
                Expect.isEmpty
                    (viewOf (raided 7 |> underSafeMode) mother).Borrowed.Defended
                    "safe mode holds the room without her"
            }

            test "a child whose towers out-damage the raid's heal is not defended" {
                let defendedWith towers =
                    (viewOf (independentChild towers |> raidedBy "Trepidimous" 7) mother)
                        .Borrowed.Defended

                Expect.isEmpty
                    (defendedWith [ 1000; 1000 ])
                    "two towers land 300 against 168 of heal"

                Expect.equal
                    (defendedWith [ 1000; 5 ])
                    [ child ]
                    "and a tower too dry to fire is no tower"

                Expect.isEmpty
                    (viewOf (raided 6) mother).Borrowed.Defended
                    "12 HEAL parts put back 144, which one tower out-damages"
            }

            // A contested room another colony declares, within
            // `Tuning.LaneHops` of a fighter of hers, is one her Atlas must
            // price, so a world Brief there may seat that fighter; in peace
            // nothing is added.
            test "a fighter's colony projects a contested room next door, and nothing in peace" {
                let viewIn world =
                    let colony = declared |> List.find (fun colony -> colony.Home = mother)

                    let holders =
                        World.creepColonies
                            Tuning.defaults
                            declared
                            (World.living declared world)
                            noneShut
                            world

                    ColonyView.ofWorld Tuning.defaults declared StandDown.none holders world colony

                let held = raided 6 |> withGuard hers mother
                let quiet = independentChild [ 1000 ] |> withGuard hers mother

                Expect.isEmpty
                    (viewIn held).Borrowed.Defended
                    "the premise: the child's tower holds this raid, so she defends nothing"

                Expect.isTrue
                    (Map.containsKey child (viewIn held).Spatial.Rooms)
                    "her guard next door has the raided home projected"

                Expect.isFalse
                    (Map.containsKey child (viewIn quiet).Spatial.Rooms)
                    "in peace she projects nothing of the child"
            }

            test "an ally's squad is no raid" {
                Expect.isEmpty
                    (viewOf (independentChild [ 1000 ] |> raidedBy "Odiodin" 7) mother)
                        .Borrowed.Defended
                    "an ally's creep is never hostile"
            }

            test "a child's home a rival holds is not hers to defend" {
                Expect.isEmpty
                    (viewOf (raided 7 |> heldBy Ownership.Rival) mother).Borrowed.Defended
                    "a lost home is the stand-down's business, not the guard's"
            }

            test
                "her guard in the home keeps it hers while the raid stands, though the towers now hold it" {
                // One healer down mid-fight: 144 heal, which the tower beats.
                let healerDown = raided 6
                let guarded = healerDown |> withGuard hers child

                Expect.isEmpty
                    (viewOf healerDown mother).Borrowed.Defended
                    "the premise: the home is not beaten"

                Expect.equal
                    (viewOf guarded mother).Borrowed.Defended
                    [ child ]
                    "with her guard standing in it, the home stays hers"

                Expect.equal
                    (Map.tryFind hers (holdersOf guarded))
                    (Some mother)
                    "so the guard is not handed to a child with no Guard to give it"

                Expect.equal
                    (guardsPooled (viewOf guarded mother))
                    [ Guard child ]
                    "and her pool still holds the Guard it is fighting"

                Expect.isEmpty
                    (viewOf (healerDown |> withGuard childs child) mother).Borrowed.Defended
                    "a guard of the child's own standing at home holds nothing for her"
            }

            test
                "with the raid gone, her guard in the home stays hers and fights nothing, until it walks out" {
                let left = independentChild [ 1000 ] |> withGuard hers child

                Expect.equal
                    (viewOf left mother).Borrowed.Defended
                    [ child ]
                    "the home stays in her scan"

                Expect.equal
                    (Map.tryFind hers (holdersOf left))
                    (Some mother)
                    "the guard stays hers"

                Expect.isEmpty (guardsPooled (viewOf left mother)) "with nothing to fight in it"

                Expect.isEmpty
                    (viewOf (independentChild [ 1000 ] |> withGuard hers mother) mother)
                        .Borrowed.Defended
                    "and once it is home, the child's home is none of hers"
            }

            test "a guard of hers left between the two homes when the raid ends is still placed" {
                let stranded = farWorld |> withGuard hers child
                let view = viewUnder farDeclared stranded mother

                Expect.equal
                    view.Borrowed.Defended
                    [ farChild ]
                    "the chain stays in her scan while her guard stands on it"

                Expect.equal
                    (Atlas.creepRoom (Atlas.ofView view) hers)
                    (Some child)
                    "so the guard is placed in the room between, where it can be walked home"

                let home = viewUnder farDeclared (farWorld |> withGuard hers mother) mother

                Expect.isEmpty home.Borrowed.Defended "and once it is home, the chain lets go"

                Expect.isFalse
                    (Map.containsKey child home.Spatial.Rooms)
                    "and the room between leaves her projection"
            }

            test "the child's own view is unchanged by its mother's defence" {
                let raided = viewOf (raided 7) child
                let quiet = viewOf (independentChild [ 1000 ]) child

                Expect.isEmpty raided.Borrowed.Defended "a colony defends no home of its own"

                Expect.equal
                    (raided.Spatial.Rooms |> Map.keys |> List.ofSeq)
                    (quiet.Spatial.Rooms |> Map.keys |> List.ofSeq)
                    "and projects the rooms it would on a quiet tick"

                Expect.equal
                    raided.Spatial.TargetKinds
                    quiet.Spatial.TargetKinds
                    "with every target of its own"
            }

            // #589: who defends a home is its bank and spawn's question,
            // never kinship's.
            test "a colony that is nobody's mother defends a beaten home its bank outbuys" {
                let unrelated = declared |> List.map (fun colony -> { colony with Mother = None })

                Expect.equal
                    (viewUnder unrelated (raided 7) mother).Borrowed.Defended
                    [ child ]
                    "kinship plays no part"

                Expect.isEmpty
                    (viewUnder unrelated (raided 7) child).Borrowed.Defended
                    "and the smaller bank defends nothing of the larger's"
            }

            test "a bank under the floor defends no home but its own" {
                let poor = raided 7 |> inRoom mother (banked (Tuning.defaults.CasterFloor - 1))

                Expect.isEmpty (viewOf poor mother).Borrowed.Defended "its economy is its own"
            }

            test "the home's own bank keeps no defender out: the verdict weighs both" {
                let rich = raided 7 |> inChild (banked 1800)

                Expect.equal
                    (viewOf rich mother).Borrowed.Defended
                    [ child ]
                    "a bank no bigger than the home's still projects it"

                Expect.equal
                    (viewOf (rich |> inChild (fun facts -> { facts with Spawns = [] })) mother)
                        .Borrowed.Defended
                    [ child ]
                    "and so does one beside a home with no spawn"
            }

            // The verdict weighs the rows the Caster may cast (`castsInto`):
            // another's home a colony only defends, past both its floors.
            test "a home the mother only defends weighs her row past the Caster's stock floor alone" {
                let stocked energy (facts: RoomFacts) =
                    { facts with
                        TargetKinds =
                            Map.add "sto-mother" (Structure BuiltKind.Storage) facts.TargetKinds
                        Stores = Map.add "sto-mother" energy facts.Stores
                    }

                let rowsOver world =
                    let deciding view : Facts.Judged =
                        {
                            Cut = view
                            Owns = Set.empty
                            Decides = true
                        }

                    let judged =
                        Facts.judge
                            (fun _ _ -> None)
                            Map.empty
                            [ deciding (viewOf world mother); deciding (viewOf world child) ]

                    (Facts.verdictOf (List.head judged) child).Rows

                let short = raided 7 |> inRoom mother (stocked (Tuning.defaults.CasterReserve - 1))

                let reserved = raided 7 |> inRoom mother (stocked Tuning.defaults.CasterReserve)

                Expect.equal
                    (viewOf short mother).Borrowed.Defended
                    [ child ]
                    "the premise: she defends the home either way"

                Expect.isFalse
                    (Map.containsKey mother (rowsOver short))
                    "one short of the reserve, her row is not weighed: she would cast nothing there"

                Expect.isTrue (Map.containsKey mother (rowsOver reserved)) "at the reserve it is"
            }

            // #663: a colony lends unless its own home's fight needs it. A
            // body that hurts nothing gates nothing; a raid her towers and
            // standing defence lose does.
            test
                "a scout in the mother's home leaves her defending the child; a raid she loses there does not" {
                let rowsOver world =
                    let deciding view : Facts.Judged =
                        {
                            Cut = view
                            Owns = Set.empty
                            Decides = true
                        }

                    let judged =
                        Facts.judge
                            (fun _ _ -> None)
                            Map.empty
                            [ deciding (viewOf world mother); deciding (viewOf world child) ]

                    (Facts.verdictOf (List.head judged) child).Rows

                let atMother (parts: BodyPart list) =
                    inRoom mother (fun facts ->
                        { facts with
                            Hostiles =
                                [
                                    {
                                        Id = "at-mother"
                                        Owner = "Trepidimous"
                                        Pos = { Room = mother; X = 5; Y = 5 }
                                        Body = parts
                                        Boosts = []
                                        Hits = Engine.partHits * List.length parts
                                        TicksToLive = 1000
                                    }
                                ]
                        })

                let reserved = raided 7 |> inRoom mother (withStock Tuning.defaults.CasterReserve)

                Expect.isTrue
                    (Map.containsKey mother (rowsOver reserved))
                    "the premise: she defends the child's home"

                Expect.isTrue
                    (Map.containsKey mother (rowsOver (reserved |> atMother [ Move; Move ])))
                    "a scout at her home leaves her row weighed"

                let towered =
                    inRoom mother (fun facts ->
                        { facts with
                            Refillables =
                                [
                                    {
                                        Id = "tower-mother"
                                        FreeCapacity = 0
                                        Kind = BuiltKind.Tower
                                    }
                                ]
                        })

                Expect.isTrue
                    (Map.containsKey
                        mother
                        (rowsOver (reserved |> towered |> atMother [ RangedAttack; Move ])))
                    "and so does a harasser her tower kills"

                Expect.isFalse
                    (Map.containsKey
                        mother
                        (rowsOver (
                            reserved
                            |> atMother (List.replicate 7 Move @ List.replicate 13 RangedAttack)
                        )))
                    "a 7M13R her home has no tower to win is her own fight first"
            }

            // #663: her home's raid is priced with its healers beside it, as
            // `homeUnheld` prices it: a dismantler her tower would kill alone
            // is healed past its damage, and her own fight. An ally's armed
            // body is no raid of hers.
            test
                "a healed dismantler her tower cannot out-damage is her own fight; an ally's body is not" {
                let towered =
                    inRoom mother (fun facts ->
                        { facts with
                            Refillables =
                                [
                                    {
                                        Id = "tower-mother"
                                        FreeCapacity = 0
                                        Kind = BuiltKind.Tower
                                    }
                                ]
                        })

                let atMother (owner: string) (bodies: BodyPart list list) =
                    inRoom mother (fun facts ->
                        { facts with
                            Hostiles =
                                bodies
                                |> List.mapi (fun i parts ->
                                    {
                                        Id = $"at-mother-{i}"
                                        Owner = owner
                                        Pos = { Room = mother; X = 5 + i; Y = 5 }
                                        Body = parts
                                        Boosts = []
                                        Hits = Engine.partHits * List.length parts
                                        TicksToLive = 1000
                                    })
                        })

                let towerless = raided 7 |> inRoom mother (withStock Tuning.defaults.CasterReserve)
                let reserved = towerless |> towered

                let threatened world =
                    Facts.homeThreatened (viewOf world mother)

                let dismantler = [ Work; Work; Move; Move ]
                let healer = List.replicate 20 Heal @ List.replicate 10 Move
                let harasser = List.replicate 7 Move @ List.replicate 13 RangedAttack

                Expect.isTrue
                    (threatened (towerless |> atMother "Trepidimous" [ harasser ]))
                    "the premise: a 7M13R her home has no tower for is her own fight"

                Expect.isFalse
                    (threatened (reserved |> atMother "Trepidimous" [ dismantler ]))
                    "the premise: her tower kills the dismantler alone"

                Expect.isTrue
                    (threatened (reserved |> atMother "Trepidimous" [ dismantler; healer ]))
                    "healed past her tower's damage, it is her own fight"

                Expect.isFalse
                    (threatened (towerless |> atMother "Odiodin" [ harasser ]))
                    "an ally's is no raid"
            }

            test "a raised home its mother defends too is still her residents', never its own row's" {
                // The pair world's child is Bootstrapping at RCL2 with a
                // spawn of its own; no tower, so any armed raid beats it.
                let world = pairWorld |> raidedBy "Trepidimous" 7
                let motherView = viewOf world mother
                let childView = viewOf world child

                Expect.equal motherView.Borrowed.Defended [ child ] "the premise: she defends it"
                Expect.contains motherView.Borrowed.Rooms child "and raises it"

                let deciding view : Facts.Judged =
                    {
                        Cut = view
                        Owns = Set.empty
                        Decides = true
                    }

                let judged =
                    Facts.judge
                        (fun _ _ -> None)
                        Map.empty
                        [ deciding motherView; deciding childView ]

                let verdict = Facts.verdictOf (List.head judged) child

                Expect.isFalse
                    (Map.containsKey child verdict.Rows)
                    "the child's own row is not weighed"

                Expect.isEmpty
                    (guardsPooled (List.item 1 judged))
                    "so the child pools no Guard in its own home"
            }
        ]

/// The declaration with the child contested: a perimeter declared (#446).
let private contestedDeclared: Colony list =
    declared
    |> List.map (fun colony ->
        if colony.Home = child then
            { colony with
                Perimeter = [ { X = 2; Y = 8 } ]
            }
        else
            colony)

/// A rampart of ours standing in the child's home.
let private withChildRampart =
    inChild (fun facts ->
        { facts with
            TargetKinds = Map.add "ram-child" (Structure BuiltKind.Rampart) facts.TargetKinds
            Hits = Map.add "ram-child" { Hits = 50_000; HitsMax = 300_000 } facts.Hits
            Layer =
                { facts.Layer with
                    TargetPositions =
                        Map.add "ram-child" { X = 2; Y = 8 } facts.Layer.TargetPositions
                }
        })

[<Tests>]
let garrisonedHomeTests =
    testList
        "a colony at the caster floor garrisons a contested home until it stands two towers"
        [
            test
                "an Independent child with a perimeter and one tower is garrisoned, as ground alone" {
                let view =
                    viewUnder
                        contestedDeclared
                        (independentChild [ 1000 ]
                         |> withChildRampart
                         |> inRoom mother (withStock Tuning.defaults.CasterReserve))
                        mother

                Expect.equal view.Borrowed.Garrisoned [ child ] "the home is hers to garrison"
                Expect.isEmpty view.Borrowed.Rooms "and no room she raises: its stage is its own"

                Expect.equal
                    (guardsPooled view)
                    [ Guard child ]
                    "her ranger row's Guard, under its name"

                Expect.equal
                    (view.Spatial.TargetKinds
                     |> Map.filter (fun id _ -> SpatialInfo.roomOf view.Spatial id = Some child)
                     |> Map.keys
                     |> Set.ofSeq)
                    (set [ "ctrl-W13S28"; "ram-child" ])
                    "the controller its ring is read off and the rampart it fights from, and nothing else"

                let atlas = Atlas.ofView view

                Expect.isFalse
                    (Planner.planTasks
                        view
                        atlas
                        (threatsOf view atlas)
                        HeldTaskFacts.empty
                        (Planner.outpostFactsOf view (Fabot.Core.Atlas.ofView view))
                     |> List.contains (Upgrade "ctrl-W13S28"))
                    "and the child's Upgrade is not hers"
            }

            test "two towers send the garrison home" {
                let view = viewUnder contestedDeclared (independentChild [ 1000; 0 ]) mother

                Expect.isEmpty view.Borrowed.Garrisoned "a dry tower still stands"
                Expect.isEmpty (guardsPooled view) "and no Guard is pooled there"
            }

            test
                "a child that declares no perimeter turns the garrison off at Independent, as before" {
                Expect.isEmpty
                    (viewOf (independentChild [ 1000 ]) mother).Borrowed.Garrisoned
                    "an uncontested child is none of hers"
            }

            test "the garrison is the Caster's, not kinship's: any colony at the floor keeps it" {
                // The child names no mother: the mother still garrisons it, the
                // one colony at `Tuning.CasterFloor` within the hop budget.
                let orphaned =
                    contestedDeclared
                    |> List.map (fun colony ->
                        if colony.Home = child then
                            { colony with Mother = None }
                        else
                            colony)

                let stockedAt energy =
                    independentChild [ 1000 ]
                    |> withChildRampart
                    |> inRoom mother (withStock energy)

                let world = stockedAt Tuning.defaults.CasterReserve

                Expect.equal
                    (viewUnder orphaned world mother).Borrowed.Garrisoned
                    [ child ]
                    "the nearest colony at the floor garrisons it, kin or not"

                // The stock's band (`Tuning.CasterReserveHold`): a newcomer
                // comes in at the reserve, the one whose ranger stands there
                // keeps the home down to the hold.
                let dipped = stockedAt (Tuning.defaults.CasterReserve - 1)

                Expect.isEmpty
                    (viewUnder orphaned dipped mother).Borrowed.Garrisoned
                    "one short of the reserve, and no ranger of hers there: she takes nothing up"

                let held =
                    { dipped with
                        Creeps =
                            creepWithBody "ranger-950-Spawn1" child [ RangedAttack, 1 ]
                            :: dipped.Creeps
                    }

                Expect.equal
                    (viewUnder orphaned held mother).Borrowed.Garrisoned
                    [ child ]
                    "with her ranger standing there, she keeps it under the reserve"

                let drained =
                    { stockedAt (Tuning.defaults.CasterReserveHold - 1) with
                        Creeps = held.Creeps
                    }

                Expect.isEmpty
                    (viewUnder orphaned drained mother).Borrowed.Garrisoned
                    "and lets it go under the hold"

                let poor =
                    world
                    |> inRoom mother (fun facts ->
                        { facts with
                            Energy =
                                { facts.Energy with
                                    Capacity = Tuning.defaults.CasterFloor - 1
                                }
                        })

                Expect.isEmpty
                    (viewUnder contestedDeclared poor mother).Borrowed.Garrisoned
                    "a mother short of the floor garrisons nothing"
            }

            test "the child's own view carries no garrison" {
                Expect.isEmpty
                    (viewUnder contestedDeclared (independentChild [ 1000 ]) child)
                        .Borrowed.Garrisoned
                    "a colony garrisons no home of its own"
            }
        ]

/// The child's outpost beside its home, and one two rooms out (#517).
let private childOutpost = "W14S28"
let private childFar = "W15S28"

let private outpostAt room : Outpost =
    {
        RoomName = room
        Sources = [ $"src-{room}", { Room = room; X = 5; Y = 5 } ]
        Controller = $"ctrl-{room}", { Room = room; X = 7; Y = 7 }
    }

/// The pair with the child mining both rooms.
let private childMining: Colony list =
    declared
    |> List.map (fun colony ->
        if colony.Home = child then
            { colony with
                Outposts = [ outpostAt childOutpost; outpostAt childFar ]
            }
        else
            colony)

/// Raptor865's shape (#516): a declared enemy's 5W3M digging the room.
let private raptorIn room : HostileInfo =
    {
        Id = $"raptor-{room}"
        Owner = "Trepidimous"
        Pos = { Room = room; X = 5; Y = 6 }
        Body = List.replicate 5 Work @ List.replicate 3 Move
        Boosts = []
        Hits = 8 * Engine.partHits
        TicksToLive = 1000
    }

/// A world with both of the child's outposts standing, a trespasser in each.
let private withChildOutposts (world: World) : World =
    let mined room =
        let name, facts =
            roomOf room Ownership.Unowned [ $"src-{room}", { X = 5; Y = 5 }, Source ]
            |> withSources [ $"src-{room}" ]

        name,
        { facts with
            Hostiles = [ raptorIn room ]
        }

    { world with
        Rooms =
            [ mined childOutpost; mined childFar ]
            |> List.fold (fun rooms (k, v) -> Map.add k v rooms) world.Rooms
    }

[<Tests>]
let childOutpostTests =
    testList
        "a mother projects her raised child's outposts for her residents (#517)"
        [
            test
                "the outpost beside the child's home is a transit room of hers, its trespasser in sight" {
                let view = viewUnder childMining (withChildOutposts pairWorld) mother

                Expect.equal
                    view.Borrowed.ChildOutposts
                    [ childOutpost ]
                    "the bordering outpost alone"

                Expect.isTrue
                    (Map.containsKey childOutpost view.Spatial.Rooms
                     && Set.contains childOutpost view.Crossed)
                    "projected as a transit room"

                Expect.isFalse
                    (Map.containsKey childFar view.Spatial.Rooms)
                    "and not the one two rooms out, which no resident borders"

                Expect.isTrue
                    (view.Hostiles |> List.exists (fun h -> h.Id = $"raptor-{childOutpost}"))
                    "its trespasser is in her sight"

                Expect.isFalse
                    (view.Spatial.TargetKinds
                     |> Map.exists (fun id _ ->
                         SpatialInfo.roomOf view.Spatial id = Some childOutpost))
                    "and nothing of the room is hers to work"

                Expect.isFalse
                    (view.Sources |> List.exists (fun source -> source.Id = $"src-{childOutpost}"))
                    "not even its rock"
            }

            test
                "a fighter the mother cast that the child holds walks to the mother's home, the walk projected for it" {
                // A fighter's caster governs its walk home. The child is Independent, so only she projects her outpost,
                // and she holds the mother's ranger standing in it.
                let world =
                    { withChildOutposts (independentChild [ 1000 ]) with
                        Creeps = creep "ranger-960-Spawn1" childOutpost :: pairWorld.Creeps
                    }

                let view = viewHolding childMining world child

                Expect.equal
                    (view.Fighters |> List.map _.Name)
                    [ "ranger-960-Spawn1" ]
                    "the premise: the child holds the mother's ranger"

                Expect.isTrue
                    (Map.containsKey mother view.Spatial.Rooms && Set.contains mother view.Crossed)
                    "the mother's home is a transit room of the child's"

                Expect.equal
                    (ColonyView.idleHome view "ranger-960-Spawn1")
                    mother
                    "the ranger's idle walk is to its caster's home"

                Expect.isNonEmpty
                    (Fabot.Core.Atlas.routes (Fabot.Core.Atlas.ofView view) childOutpost mother)
                    "and the child's Atlas prices the walk"

                Expect.equal
                    (ColonyView.idleHome view "hauler-950-Spawn2")
                    child
                    "a body she cast walks to her own"

                Expect.isFalse
                    (Map.containsKey
                        mother
                        (viewHolding childMining (independentChild [ 1000 ]) child).Spatial.Rooms)
                    "with no fighter of the mother's held, she projects no walk to her home"
            }

            test "the child's own view works its outpost as before" {
                let view = viewUnder childMining (withChildOutposts pairWorld) child

                Expect.isEmpty view.Borrowed.ChildOutposts "a colony is no child of its own"
                Expect.isFalse (Set.contains childOutpost view.Crossed) "it is worked, not crossed"
            }

            test "an Independent child she keeps no residents in is none of hers to project" {
                let view =
                    viewUnder childMining (withChildOutposts (independentChild [ 1000 ])) mother

                Expect.isEmpty view.Borrowed.ChildOutposts "no resident, no outpost of the child's"

                Expect.isFalse
                    (Map.containsKey childOutpost view.Spatial.Rooms)
                    "and the room is not projected"
            }

            test
                "a body standing in the child's outpost stays its caster's, the child's own as hers" {
                let world =
                    { withChildOutposts pairWorld with
                        Creeps =
                            pairWorld.Creeps
                            @ [
                                creep "anchor-960-Spawn2" childOutpost
                                creep "ranger-960-Spawn1" childOutpost
                            ]
                    }

                let holdersUnder shut =
                    World.creepColonies
                        Tuning.defaults
                        childMining
                        (World.living childMining world)
                        shut
                        world

                let open' = holdersUnder noneShut

                Expect.equal
                    (Map.tryFind "anchor-960-Spawn2" open')
                    (Some child)
                    "the child's anchor is the child's"

                Expect.equal
                    (Map.tryFind "ranger-960-Spawn1" open')
                    (Some mother)
                    "and her resident is hers, never adopted by the child mid-kill"

                Expect.equal
                    (Map.tryFind
                        "anchor-960-Spawn2"
                        (holdersUnder (Map.ofList [ child, Set.singleton childOutpost ])))
                    (Some child)
                    "with the child's stand-down shutting the room, its anchor stays the child's to walk home"
            }

            test "decaying ore in the child's outpost is no sweep of hers" {
                let pile = "ore-child-out"

                let world =
                    let world = withChildOutposts pairWorld
                    let facts = World.roomOf world childOutpost

                    { world with
                        Rooms =
                            world.Rooms
                            |> Map.add
                                childOutpost
                                { facts with
                                    TargetKinds = Map.add pile (Dropped Thorium) facts.TargetKinds
                                    Thorium = Map.add pile 900 facts.Thorium
                                    Layer =
                                        { facts.Layer with
                                            TargetPositions =
                                                Map.add
                                                    pile
                                                    { X = 6; Y = 6 }
                                                    facts.Layer.TargetPositions
                                        }
                                }
                    }

                let view = viewUnder childMining world mother
                let atlas = Atlas.ofView view

                Expect.isFalse
                    (Planner.planTasks
                        view
                        atlas
                        (threatsOf view atlas)
                        HeldTaskFacts.empty
                        (Planner.outpostFactsOf view (Fabot.Core.Atlas.ofView view))
                     |> List.exists (fun task ->
                         match task with
                         | Pickup(id, _)
                         | Withdraw(id, _) -> id = pile
                         | _ -> false))
                    "the child's floor is the child's"
            }
        ]

/// One colony's view under every colony's shut rooms, its own gate the shut
/// set it holds: the cut the shell makes (`ColonyView.ofWorldRecalling`).
let private viewShutUnder colonies (shut: Map<string, Set<string>>) world home =
    let colony = colonies |> List.find (fun colony -> colony.Home = home)

    let holders =
        World.creepColonies Tuning.defaults colonies (World.living colonies world) shut world

    ColonyView.ofWorldRecalling
        (JoinTable())
        Tuning.defaults
        colonies
        HarassCasting.none
        { StandDown.none with
            Shut = Map.tryFind home shut |> Option.defaultValue Set.empty
        }
        holders
        (fun _ -> false)
        (WorldView.ofWorld colonies [] world)
        world
        colony

/// A Trepidimous ranged harasser, Synapse673's body (13 RANGED_ATTACK,
/// 7 MOVE, no heal), standing in a room.
let private harasserIn room : HostileInfo =
    {
        Id = $"harasser-{room}"
        Owner = "Trepidimous"
        Pos = { Room = room; X = 25; Y = 20 }
        Body = List.replicate 7 Move @ List.replicate 13 RangedAttack
        Boosts = []
        Hits = 20 * Engine.partHits
        TicksToLive = 1000
    }

/// A body of ours with the given parts, standing in a room.
let private bodied name room (parts: (BodyPart * int) list) : WorldCreep =
    let built = creep name room

    { built with
        Info =
            { built.Info with
                Body = Map.ofList parts
                Hits =
                    {
                        Hits = Engine.partHits * List.sumBy snd parts
                        HitsMax = Engine.partHits * List.sumBy snd parts
                    }
            }
    }

/// The world with these bodies added, each on its own tile of its room.
let private standing (bodies: (WorldCreep * Pos) list) (world: World) : World =
    { world with
        Creeps = world.Creeps @ (bodies |> List.map fst)
        Rooms =
            (world.Rooms, bodies)
            ||> List.fold (fun rooms (body, at) ->
                rooms
                |> Map.change
                    body.Room
                    (Option.map (fun facts ->
                        { facts with
                            Layer =
                                { facts.Layer with
                                    CreepPositions =
                                        Map.add body.Info.Name at facts.Layer.CreepPositions
                                }
                        })))
    }

/// Each case pins a world fact once held or concluded per colony and now
/// read once for every colony; its comment names the world-layer stage that
/// flipped it.
[<Tests>]
let worldSeamTests =
    testList
        "world facts read once for every colony"
        [
            // Flipped at the world layer's S1 (#534). Live t973,5xx: W13S28's
            // observer looked into W18S25 while W17S25's stand-down held it
            // shut; the look reached `World.Rooms` and no view, and the log
            // still read "last seen t973,265". The view still does not
            // project the room; the log reads the world's look.
            test
                "a stood-down outpost's hostiles, seen with no body of ours there, reach the owner's raid log through the world" {
                let raider =
                    { harasserIn outpost with
                        Id = "seen-by-observer"
                    }

                let world =
                    { pairWorld with
                        Rooms =
                            pairWorld.Rooms
                            |> Map.change
                                outpost
                                (Option.map (fun facts -> { facts with Hostiles = [ raider ] }))
                    }

                let shut = Map.ofList [ mother, Set.singleton outpost ]
                let motherView = viewShutUnder declared shut world mother

                Expect.isFalse
                    (world.Creeps |> List.exists (fun body -> body.Room = outpost))
                    "the premise: no body of ours stands there, the look is the observer's"

                Expect.contains
                    (World.roomOf world outpost).Hostiles
                    raider
                    "the world holds the look"

                Expect.isFalse
                    ([ mother; child ]
                     |> List.exists (fun home ->
                         (viewShutUnder declared shut world home).Hostiles
                         |> List.exists (fun h -> h.Id = raider.Id)))
                    "and no colony's view carries it"

                // The row that shut it, opened ten ticks back on this raid.
                let shutOn: Observe.OutpostEpisode =
                    {
                        RoomName = outpost
                        Opened = world.Time - 10
                        LastSeen = world.Time - 10
                        Expiry = world.Time + 500
                        Basis = StandDownBasis.InvaderRaid
                        Stronghold = false
                        Raiders = set [ raider.Id ]
                        PlayerSince = None
                        Chased = false
                        Bounced = false
                        HolderUnread = false
                    }

                let log =
                    { Observe.RaidState.empty with
                        Outposts = [ shutOn ]
                    }
                    |> Observe.foldRaids
                        Observe.capEpisodes
                        Set.empty
                        motherView
                        (Planner.outpostFactsOf motherView (Fabot.Core.Atlas.ofView motherView))
                            .Declared

                Expect.equal
                    (log.Outposts |> List.map (fun row -> row.RoomName, row.LastSeen))
                    [ outpost, world.Time ]
                    "the owner's stand-down is last seen this tick"

                Expect.isEmpty
                    log.Episodes
                    "while its episode roster, the colony's own vision's, grows nothing"
            }

            // Flipped at the world layer's S1 (#534). Live: W17S25's
            // stand-down shut W18S25, and the mother W15S28's `ChildOutposts`
            // dropped it, so the trespasser there stood in no view at all and
            // the raid there turned Kite was answered by nobody. Shut withholds
            // the child's economy, never the residents' watch.
            test
                "a child's shut outpost stays in the mother's ChildOutposts, and its hostile in her view" {
                let world = withChildOutposts pairWorld
                let trespasser = $"raptor-{childOutpost}"

                // The child mining the bordering room alone, so no walk of hers
                // to a farther outpost crosses it.
                let colonies =
                    childMining
                    |> List.map (fun colony ->
                        if colony.Home = child then
                            { colony with
                                Outposts = [ outpostAt childOutpost ]
                            }
                        else
                            colony)

                let seenBy shut home =
                    (viewShutUnder colonies shut world home).Hostiles
                    |> List.exists (fun h -> h.Id = trespasser)

                Expect.equal
                    (viewShutUnder colonies noneShut world mother).Borrowed.ChildOutposts
                    [ childOutpost ]
                    "the premise: open, the mother projects it for her residents"

                let shut = Map.ofList [ child, Set.singleton childOutpost ]

                Expect.equal
                    (viewShutUnder colonies shut world mother).Borrowed.ChildOutposts
                    [ childOutpost ]
                    "shut by the child, it stays in the mother's ChildOutposts"

                Expect.isTrue (seenBy shut mother) "so the mother's view carries the trespasser"

                Expect.isFalse (seenBy shut child) "and the child, who shut it, sees it no more"

                let childWorks gate =
                    World.scanOf
                        Tuning.defaults
                        (World.stages Tuning.defaults colonies world)
                        (World.unownedHomes colonies world)
                        colonies
                        gate
                        world
                        (colonies |> List.find (fun colony -> colony.Home = child))
                    |> fun scan -> Set.contains childOutpost scan.Worked

                Expect.isTrue (childWorks StandDown.none) "the premise: open, the child works it"

                Expect.isFalse
                    (childWorks
                        { StandDown.none with
                            Shut = Set.singleton childOutpost
                        })
                    "shut, she works it no more"
            }

            // Flipped at the world layer's S3. The live shape: W18S25, priced
            // by W17S25 and by its mother W15S28 apart, the child a Win (her
            // rangers and the mother's beside them) and the mother a Lose
            // (hers alone).
            test
                "one world, one room, one verdict: the rangers at the child's home win it, and both colonies read the Win" {
                let ranger name room =
                    bodied name room [ Move, 21; RangedAttack, 14; Heal, 7 ]

                let world =
                    { withChildOutposts pairWorld with
                        Rooms =
                            (withChildOutposts pairWorld).Rooms
                            |> Map.change
                                childOutpost
                                (Option.map (fun facts ->
                                    { facts with
                                        Hostiles = [ harasserIn childOutpost ]
                                    }))
                    }
                    |> standing
                        [
                            bodied "ranger-960-Spawn1" child [ Move, 10; RangedAttack, 7; Heal, 3 ],
                            { X = 5; Y = 2 }
                            ranger "ranger-961-Spawn2" child, { X = 5; Y = 3 }
                            ranger "ranger-962-Spawn2" child, { X = 6; Y = 3 }
                        ]

                let motherView = viewUnder childMining world mother
                let childView = viewUnder childMining world child

                Expect.contains
                    motherView.Borrowed.ChildOutposts
                    childOutpost
                    "the premise: the mother projects the child's outpost"

                Expect.isTrue
                    (motherView.Hostiles |> List.exists (fun h -> h.Pos.Room = childOutpost))
                    "and sees its harasser"

                Expect.equal
                    (Facts.answer motherView childOutpost)
                    Answer.Lose
                    "the premise: alone, the mother's bodies lose it"

                let deciding view : Facts.Judged =
                    {
                        Cut = view
                        Owns = Set.empty
                        Decides = true
                    }

                let judged =
                    Facts.judge
                        (fun _ _ -> None)
                        Map.empty
                        [ deciding motherView; deciding childView ]

                Expect.equal
                    (judged |> List.map (fun view -> Facts.answer view childOutpost))
                    [ Answer.Win; Answer.Win ]
                    "priced once over both colonies' bodies, both read one Win"

                Expect.equal
                    (judged
                     |> List.map (fun view -> (Facts.verdictOf view childOutpost).Rangers.Value))
                    [ Some 2; Some 2 ]
                    "by two rangers: the mother's at the child's home and one of the child's"

                Expect.equal
                    (judged
                     |> List.map (fun view ->
                         (Facts.verdictOf view childOutpost).Bodies |> List.map snd |> Set.ofList))
                    [ Set.ofList [ mother; child ]; Set.ofList [ mother; child ] ]
                    "each of the two colonies' own: who casts for the room is the Caster's (W6 S2)"
            }
        ]

let private defenderNames (view: ColonyView) room =
    Map.tryFind room view.Defenders
    |> Option.defaultValue []
    |> List.map (fun creep -> creep.Name)

[<Tests>]
let safeModeFactTests =
    testList
        "what a safe-mode reflex reads off the world"
        [
            test "a child's home counts the mother's armed body standing in it as a defender" {
                // #448: the resident is held by the mother (`Spawn1`), so the
                // child's `Creeps` never carries it, and its home is defended
                // by it all the same.
                let world = pairWorld |> withGuard hers child
                let view = viewOf world child

                Expect.equal
                    (defenderNames view child)
                    [ hers ]
                    "her guard, and not the unarmed pioneer"

                Expect.isFalse (names view |> List.contains hers) "while the body stays hers"

                Expect.isEmpty
                    (defenderNames (viewOf world mother) child)
                    "a bootstrapping child's home is its own reflex's, not its mother's"
            }

            test "a mother reads her nursery's controller and the defenders in it" {
                // #449: a Nursery runs no tick of its own, so its mother fires
                // its safe mode and pools its Upgrade off these.
                let world = spawnlessWorld |> withGuard hers child
                let view = viewOf world mother

                Expect.equal
                    (Map.tryFind child view.NurseryControllers
                     |> Option.map (fun c -> c.Id, c.Level))
                    (Some("ctrl-W13S28", 2))
                    "the child's controller, at its level"

                Expect.equal (defenderNames view child) [ hers ] "and the armed body standing in it"

                Expect.isEmpty
                    (viewOf pairWorld mother).NurseryControllers
                    "a child with its spawn standing is no nursery"
            }

            test
                "a mother reads her nursery's declared first spawn, named past every spawn the world holds" {
                // #476: the next free `SpawnN` is past the highest number
                // standing, so a name a lost room once held is never reused.
                let firstSpawning =
                    declared
                    |> List.map (fun colony ->
                        if colony.Home = child then
                            { colony with
                                FirstSpawn = Some { X = 10; Y = 10 }
                            }
                        else
                            colony)

                let withSpawn7 =
                    { spawnlessWorld with
                        Rooms =
                            spawnlessWorld.Rooms
                            |> Map.add
                                mother
                                (snd (
                                    ourColony
                                        "Spawn7"
                                        5
                                        1800
                                        (mother, World.roomOf spawnlessWorld mother)
                                ))
                    }

                Expect.equal
                    ((viewUnder firstSpawning withSpawn7 mother).FirstSpawns |> Map.toList)
                    [
                        child,
                        {
                            Tile = { X = 10; Y = 10 }
                            Name = "Spawn8"
                        }
                    ]
                    "the declared tile, named after Spawn7"

                Expect.isEmpty
                    (viewUnder firstSpawning pairWorld mother).FirstSpawns
                    "a child with its spawn standing has nothing to place"

                Expect.isEmpty
                    (viewOf spawnlessWorld mother).FirstSpawns
                    "a nursery nobody declared a tile for has nothing to place"
            }
        ]
