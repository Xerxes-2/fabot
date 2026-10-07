/// The fighter: a body read off its parts (reach, damage and heal by range,
/// gait, hits), the weapon reflex every armed body strikes by beside its Task
/// (#535), and the strike it picks (#537).
module Fabot.Core.Tests.Decide.FighterTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.OutpostFixtures

/// A Fighter's numbers, its two by-range readings out to range 4.
let private numbersOf (fighter: Fighter) =
    {|
        Reach = fighter.Reach
        Damage = [ 1..4 ] |> List.map fighter.DamageAt
        Heal = [ 1..4 ] |> List.map fighter.HealAt
        RangedHeal = fighter.RangedHeal
        Gait = fighter.Gait
        Hits = fighter.Hits
        Tough = fighter.Tough
    |}

/// A whole body's Fighter, unboosted.
let private wholeFighter (body: BodyPart list) =
    Fighter.ofParts body [] (Engine.partHits * List.length body)

[<Tests>]
let fighterTests =
    testList
        "a fighter read off its parts (#615)"
        [
            test "one block of each fighting row: reach, damage and heal by range, gait, hits" {
                Expect.equal
                    (numbersOf (wholeFighter guardPattern.Block))
                    {|
                        Reach = 1
                        Damage = [ 90; 0; 0; 0 ]
                        Heal = [ 0; 12; 12; 12 ]
                        RangedHeal = 4
                        Gait = { Weight = 5; Moves = 5 }
                        Hits = 1000
                        Tough = 0
                    |}
                    "the guard swings 3 × 30 at range 1, where its swing leaves its HEAL idle"

                Expect.equal
                    (numbersOf (wholeFighter rangerPattern.Block))
                    {|
                        Reach = 3
                        Damage = [ 20; 20; 20; 0 ]
                        Heal = [ 12; 12; 12; 12 ]
                        RangedHeal = 4
                        Gait = { Weight = 3; Moves = 3 }
                        Hits = 600
                        Tough = 0
                    |}
                    "the ranger shoots 2 × 10 out to 3 and heals beside every shot"

                Expect.equal
                    (numbersOf (wholeFighter defenderPattern.Block))
                    {|
                        Reach = 1
                        Damage = [ 30; 0; 0; 0 ]
                        Heal = [ 0; 0; 0; 0 ]
                        RangedHeal = 0
                        Gait = { Weight = 1; Moves = 1 }
                        Hits = 200
                        Tough = 0
                    |}
                    "the defender swings 30 and heals nothing"

                Expect.equal
                    (numbersOf (wholeFighter medicPattern.Block))
                    {|
                        Reach = 0
                        Damage = [ 0; 0; 0; 0 ]
                        Heal = [ 216; 216; 216; 216 ]
                        RangedHeal = 72
                        Gait = { Weight = 18; Moves = 18 }
                        Hits = 3600
                        Tough = 0
                    |}
                    "the medic strikes nothing and heals 18 × 12, or 18 × 4 at range"
            }

            test "a hybrid swings and shoots at range 1, heals only where it does not swing" {
                Expect.equal
                    (numbersOf (wholeFighter [ Move; Move; Move; Attack; RangedAttack; Heal ]))
                    {|
                        Reach = 3
                        Damage = [ 40; 10; 10; 0 ]
                        Heal = [ 0; 12; 12; 12 ]
                        RangedHeal = 4
                        Gait = { Weight = 3; Moves = 3 }
                        Hits = 600
                        Tough = 0
                    |}
                    "attack and rangedAttack are in separate chains; heal drops the attack"
            }

            test "a hurt body acts with the tail its hits cover and still weighs what is shot off" {
                let at hits =
                    numbersOf (Fighter.ofParts guardPattern.Block [] hits)

                Expect.equal
                    (at 450)
                    {| at 1000 with
                        Gait = { Weight = 5; Moves = 1 }
                        Hits = 450
                    |}
                    "TOUGH and four MOVE gone: it still swings and heals, on one MOVE under the whole weight"

                Expect.equal
                    (at 350)
                    {| at 1000 with
                        Gait = { Weight = 5; Moves = 0 }
                        Hits = 350
                    |}
                    "the last MOVE gone: rooted, still armed"

                Expect.equal
                    (at 150)
                    {| at 1000 with
                        Damage = [ 30; 0; 0; 0 ]
                        Gait = { Weight = 5; Moves = 0 }
                        Hits = 150
                    |}
                    "one ATTACK and the HEAL left: one swing, the heal idle beside it"
            }

            test
                "a living creep's fight weighs what is shot off as the engine does (Arrival.gaitOf)" {
                let body = guardPattern.Block

                // #615 U3 (`movement.js`): the TOUGH shot off still weighs,
                // the four MOVE shot off do not; it weighed all five.
                let hurt =
                    { creepWith "g-1" 0 0 body with
                        Body = partsOf (Engine.liveParts body 450)
                        Hits = { Hits = 450; HitsMax = 1000 }
                        ShotOff = 1
                    }

                Expect.equal
                    (numbersOf (Fighter.ofCreep hurt))
                    (numbersOf (Fighter.ofParts body [] 450))
                    "the same weapons, heal and gait as the body read whole at its hits"

                Expect.equal
                    (numbersOf (Fighter.ofCreep (creepWith "g-2" 0 0 body)))
                    (numbersOf (wholeFighter body))
                    "whole, the two readings agree"
            }

            // #615 U3: every Fighter field off the live parts, head first as
            // our rows cast them. Before, a creep's damage and heal read its
            // part counts as given, and a worn body (`Facts.winsBeside`) lost
            // each kind in the share of its hits, its shots with its MOVE.
            test
                "a creep worn to fewer hits loses its head first, and a count past its hits prices nothing" {
                let ranger = creepWith "r-1" 0 0 (rangerPattern.Block @ rangerPattern.Block)

                Expect.equal
                    (numbersOf (Fighter.atHits ranger 650))
                    {| numbersOf (wholeFighter (rangerPattern.Block @ rangerPattern.Block)) with
                        Gait = { Weight = 6; Moves = 1 }
                        Hits = 650
                    |}
                    "five MOVE gone off the head: four shots and two HEAL act on one MOVE, under the same weight (a MOVE weighs nothing, live or dead)"

                Expect.equal
                    (numbersOf (Fighter.atHits ranger 150))
                    {| numbersOf (wholeFighter (rangerPattern.Block @ rangerPattern.Block)) with
                        Damage = [ 0; 0; 0; 0 ]
                        Heal = [ 24; 24; 24; 24 ]
                        Reach = 0
                        Gait = { Weight = 6; Moves = 0 }
                        Hits = 150
                    |}
                    "the shots gone, the two HEAL left"

                let stated =
                    { creepWith "g-1" 0 0 guardPattern.Block with
                        Hits = { Hits = 150; HitsMax = 1000 }
                    }

                Expect.equal
                    (numbersOf (Fighter.ofCreep stated))
                    (numbersOf (Fighter.atHits (creepWith "g-1" 0 0 guardPattern.Block) 150))
                    "a body counting parts its hits do not cover is priced on the tail they do"
            }

            test "boosts multiply the parts they act on, and a boosted TOUGH is counted" {
                let boosts =
                    guardPattern.Block
                    |> List.map (function
                        | Tough -> Tier1
                        | Move -> Tier1
                        | Attack -> Tier2
                        | Heal -> Tier3
                        | _ -> Unboosted)

                Expect.equal
                    (numbersOf (Fighter.ofParts guardPattern.Block boosts 1000))
                    {|
                        Reach = 1
                        Damage = [ 270; 0; 0; 0 ]
                        Heal = [ 0; 48; 48; 48 ]
                        RangedHeal = 16
                        Gait = { Weight = 5; Moves = 10 }
                        Hits = 1000
                        Tough = 100
                    |}
                    "UH2O swings ×3, XLHO2 heals ×4, ZO walks ×2, GO's 100 hits take a share"

                let ranger = Fighter.ofParts rangerPattern.Block (List.replicate 6 Tier3) 600

                Expect.equal (ranger.DamageAt 3) 80 "XKHO2 shoots ×4"
            }

            // #624, `_damage.js`: a swing on a creep is struck back by the
            // creep's live ATTACK at its boosts (`calcBodyEffectiveness`).
            test
                "a body strikes back a swing at it by its live ATTACK at its boosts, and nothing else" {
                let boosted =
                    guardPattern.Block
                    |> List.map (function
                        | Attack -> Tier2
                        | _ -> Unboosted)

                Expect.equal
                    ([ guardPattern; rangerPattern; defenderPattern; medicPattern ]
                     |> List.map (fun pattern -> (wholeFighter pattern.Block).StrikesBack))
                    [ 90; 0; 30; 0 ]
                    "three ATTACK strike back 90; a shot or a heal strikes back nothing"

                Expect.equal
                    (Fighter.ofParts guardPattern.Block boosted 1000).StrikesBack
                    270
                    "UH2O strikes back ×3 too"

                Expect.equal
                    ([ 250; 100 ]
                     |> List.map (fun hits ->
                         (Fighter.ofParts guardPattern.Block [] hits).StrikesBack))
                    [ 60; 0 ]
                    "an ATTACK shot off strikes nothing back: at 250 hits two still stand, at 100 only the HEAL"
            }

            test "a body stands off a class it outranges and out-paces on plain (U1)" {
                let melee plain =
                    {
                        Strike = Engine.meleeRange
                        Pace = Some { Road = 1; Plain = plain; Swamp = 5 }
                    }

                let shooter =
                    { melee 1 with
                        Strike = Engine.rangedRange
                    }

                let ranger = wholeFighter rangerPattern.Block
                let guard = wholeFighter guardPattern.Block
                let hybrid = wholeFighter [ Move; Move; Move; Attack; RangedAttack; Heal ]

                Expect.isTrue
                    (Fighter.standsOff ranger (melee 2))
                    "a ranger kites a melee slower on plain"

                Expect.isFalse
                    (Fighter.standsOff ranger (melee 1))
                    "one at its pace can close, as the exchange prices it (`Facts.standoff`)"

                Expect.isFalse
                    (Fighter.standsOff (Fighter.ofParts rangerPattern.Block [] 350) (melee 2))
                    "a ranger on one MOVE under its whole weight is the slower: the melee picks the standoff"

                Expect.isFalse
                    (Fighter.standsOff ranger shooter)
                    "a shooter strikes as far as it reaches: it closes"

                Expect.isFalse
                    (Fighter.standsOff guard (melee 5))
                    "a melee stands off nothing, however slow the other: its standoff is the target's tile"

                Expect.isTrue
                    (Fighter.standsOff hybrid (melee 2))
                    "a hybrid holds at its gun's reach"

                Expect.isTrue
                    (Fighter.standsOff ranger { melee 1 with Pace = None })
                    "a melee with no MOVE acting never closes"

                Expect.isTrue (Fighter.glues guard) "the guard's standoff is its target's tile"

                Expect.isFalse
                    (Fighter.glues ranger || Fighter.glues hybrid)
                    "a body with a gun never glues"

                Expect.isFalse
                    (Fighter.glues (wholeFighter medicPattern.Block))
                    "nor one with no weapon"
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

/// Blows priced at their damage alone, as `pickStrike` read them before #651.
let private raw (blows: (string * int) list) = blows |> List.sumBy (snd >> float)

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
                        raw
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
                        raw
                        band
                        (Some(fun _ -> 20))
                        [ "healer"; "melee" ])
                    (Some Emitter.Mass)
                    "20 on the healer as the shot, and 20 on the melee besides"

                Expect.equal
                    (Emitter.pickStrike
                        (fun _ -> true)
                        (fun _ -> 20)
                        raw
                        band
                        (Some(fun t -> if t = "healer" then 8 else 20))
                        [ "healer"; "melee" ])
                    (Some(Emitter.At("healer", true)))
                    "the healer at range 2: 8 on it against the shot's 20, the melee's 20 notwithstanding"
            }

            test
                "beside a partner, two in the raid's band take the focused shot where the heal undoes the mass's spread (#651)" {
                // Priced by `Facts.blowsWorth`: a partner deals the head 30
                // and no heal reaches it, so the shot's 20 on it stands whole;
                // a healer reaching only the other undoes the mass's 20 on it.
                let terms = Map.ofList [ "head", (1.0, 30); "other", (1.0, 0) ]
                let heals = [ Map.ofList [ "other", 30 ] ]

                Expect.equal
                    (Emitter.pickStrike
                        (fun _ -> true)
                        (fun _ -> 20)
                        (Facts.blowsWorth terms heals)
                        (fun _ -> 2)
                        (Some(fun _ -> 20))
                        [ "head"; "other" ])
                    (Some(Emitter.At("head", true)))
                    "20 kept on the head either way, nothing on the other"

                Expect.equal
                    (Emitter.pickStrike
                        (fun _ -> true)
                        (fun _ -> 20)
                        raw
                        (fun _ -> 2)
                        (Some(fun _ -> 20))
                        [ "head"; "other" ])
                    (Some Emitter.Mass)
                    "priced on raw damage it spread the blow: 40 against 20"
            }

            test
                "a heal acts on one target a tick: two healers undo a focused 60 and only part of a spread (#651)" {
                // #623's pair: each heals itself or the other 36; nothing of
                // ours beside the striker.
                let terms = Map.ofList [ "near", (1.0, 0); "far", (1.0, 0) ]
                let both = Map.ofList [ "near", 36; "far", 36 ]
                let worth = Facts.blowsWorth terms [ both; both ]

                Expect.equal
                    (worth [ "near", 60; "far", 0 ])
                    0.0
                    "the shot: 72 of heal on it undo all 60"

                Expect.equal
                    (worth [ "near", 60; "far", 24 ])
                    24.0
                    "the mass: the heals spent where they undo most still leave 24 standing"
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

            // Live t1,019,113–159 (W19S25): through a harassment room's
            // stand-down a fleeing ranger shot the harasser beside it 45
            // ticks running, its partner two off, and massed the tick the
            // stand-down ended. The body standing there is the room's sight.
            test
                "in a room the view only crosses or holds for a stray alone, two beside the ranger take a mass attack; a neutral's structure within three still forbids it" {
                let invaders (colony: ColonyView) =
                    { colony with
                        Hostiles =
                            colony.Hostiles |> List.map (fun h -> { h with Owner = "Invader" })
                    }

                let crossed (colony: ColonyView) =
                    { invaders colony with
                        Crossed = Set.ofList [ "W1N2" ]
                    }

                let stray (colony: ColonyView) =
                    { invaders colony with
                        Spatial =
                            { colony.Spatial with
                                StrayRooms = Set.ofList [ "W1N2" ]
                            }
                    }

                Expect.equal
                    (strikesAmong (crossed (pair ())))
                    [ RangedMassAttack "r-1" ]
                    "a crossed room: 40 against 20"

                Expect.equal
                    (strikesAmong (stray (pair ())))
                    [ RangedMassAttack "r-1" ]
                    "a stray's room: 40 against 20"

                Expect.isFalse
                    (strikesAmong (crossed (withForeign "passer" { X = 24; Y = 46 } (pair ())))
                     |> List.contains (RangedMassAttack "r-1"))
                    "a neutral's extension at range 3 in a crossed room"
            }
        ]
