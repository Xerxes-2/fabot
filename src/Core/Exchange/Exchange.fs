/// Who wins a fight between two sides standing in one room: two clocks, each
/// side's kill of the other, at the ranges the faster side picks.
module Fabot.Core.Exchange.Exchange

open Fabot.Core
open Fabot.Core.Vocabulary
open Fabot.Core.World
open Fabot.Core.Grid
open Fabot.Core.Exchange.Fighter

type Tower = { Pos: Pos; Energy: int }

[<NoEquality; NoComparison>]
type Side =
    {
        Bodies: Fighter list
        Ramparts: Map<Pos, int>
        Towers: Tower list
    }

/// `Weight` is the step weight of the ground fought on; `RunOff` the tiles a
/// kiter backs away before a wall or an exit corners it.
type Terrain = { Weight: int; RunOff: int }

type Losses = { Dead: int; Hits: int }

type Outcome =
    | Win of ticks: int * Losses
    | Kite of ticks: int
    | Lose

type Body = BodyPart list

/// Holding a post the raid must come to, every pair stands at a swing's
/// range; else the faster side picks the range.
[<RequireQualifiedAccess>]
type Stance =
    | Hold
    | Standoff

/// A cast body lands no sooner than half a room's walk; no fight is read past
/// two rooms' walk.
let private castWalk = Engine.roomSide / 2

let private longest = 2 * Engine.roomSide

/// A raider by its place in its side, so a repeated body counts as many.
[<NoEquality; NoComparison>]
type private Foe = { At: int; F: Fighter }

let private rangeTo (tile: Pos) (f: Fighter) = f.Pos |> Option.bind (Pos.range tile)

let private between (a: Foe) (b: Foe) : int option =
    if a.At = b.At then
        Some 0
    else
        match a.F.Pos with
        | Some p -> rangeTo p b.F
        | None -> None

type private Healer =
    {
        Mender: Foe
        CastsHeal: bool
        CastsRangedHeal: bool
    }

/// Read at the range it closes to within `horizon` ticks. A `rangedHeal`
/// drops the shot beside it, and either heal drops the swing.
let private healOnto (weight: int) (horizon: int) (healer: Healer) (target: Foe) =
    match between healer.Mender target with
    | None -> 0
    | Some range ->
        let closes =
            if horizon <= 0 || range <= Engine.meleeRange then
                0
            else
                match pace weight healer.Mender.F.Gait with
                | Some ticks -> horizon / ticks
                | None -> 0

        let range = range - closes

        if range <= Engine.meleeRange && healer.CastsHeal then
            healing healer.Mender.F
        elif range <= Engine.rangedRange && healer.CastsRangedHeal then
            healing healer.Mender.F * Engine.rangedHealPower / Engine.healPower
        else
            0

let private healersOf (raid: Foe list) =
    raid
    |> List.filter (fun h -> h.F.Heal > 0)
    |> List.map (fun h ->
        {
            Mender = h
            CastsHeal = true
            CastsRangedHeal = true
        })

let private healFrom weight horizon (healers: Healer list) (target: Foe) =
    healers |> List.sumBy (fun healer -> healOnto weight horizon healer target)

let private rampartOver (side: Side) (f: Fighter) =
    match f.Pos with
    | Some at -> Map.tryFind at side.Ramparts |> Option.defaultValue 0
    | None -> 0

/// A raid's price at a horizon: its striking bodies' hits in three phases (the
/// ramparts under them, which no heal puts back; the damage that breaks their
/// boosted TOUGH; the rest at `Healing`), against the damage it deals.
type private Price =
    {
        Damage: int
        Healing: int
        /// What a hit healed on its TOUGH costs our damage.
        ToughHealing: int
        Shield: int
        Tough: int
        Hits: int
        /// It dismantles: a raid dealing our bodies nothing is won only by outhealing it.
        Breaches: bool
        Dismantlers: (Fighter * int) list
        /// Its dismantlers' hits that strike nothing: the kill where no striker stands.
        Razing: int
    }

/// Its healers heal the body they heal most. Where `heals`, a healer that
/// fights heals wherever its heal reaches and drops the weapon it heals over.
let private priceAs
    (terrain: Terrain)
    (theirs: Side)
    (raid: Foe list)
    horizon
    heals
    (rangeOf: Foe -> int)
    : Price =
    let striking = raid |> List.filter (fun h -> strikes h.F)

    let weapons (h: Foe) =
        let range = rangeOf h

        (if range <= Engine.meleeRange then
             Engine.attackPower * h.F.Attack
         else
             0),
        (if range <= Engine.rangedRange then
             Engine.rangedAttackPower * h.F.Ranged
         else
             0)

    let healers =
        healersOf raid
        |> List.map (fun healer ->
            let swing, shot = weapons healer.Mender

            { healer with
                CastsHeal = heals || swing = 0
                CastsRangedHeal = heals || swing + shot = 0
            })

    let healOn = healFrom terrain.Weight horizon healers
    let breachers = raid |> List.filter (fun h -> h.F.Work > 0)

    let focus =
        match
            (if not striking.IsEmpty then striking
             elif not breachers.IsEmpty then breachers
             else raid)
        with
        | [] -> None
        | patients -> Some(List.maxBy healOn patients)

    let dropped =
        match focus with
        | None -> 0
        | Some focus ->
            healers
            |> List.sumBy (fun healer ->
                let swing, shot = weapons healer.Mender

                if
                    healOnto terrain.Weight horizon { healer with CastsRangedHeal = false } focus > 0
                then
                    swing
                elif healOnto terrain.Weight horizon healer focus > 0 then
                    swing + shot
                else
                    0)

    let healed = focus |> Option.map healOn |> Option.defaultValue 0
    let tough = striking |> List.filter (fun h -> toughHits h.F > 0)

    {
        Breaches = not breachers.IsEmpty
        Damage = (striking |> List.sumBy (fun h -> weapons h ||> (+))) - dropped
        Healing = healed
        ToughHealing =
            match tough with
            | [] -> healed
            | _ -> tough |> List.map (fun h -> damageFor h.F (healOn h)) |> List.max
        Shield = striking |> List.sumBy (fun h -> rampartOver theirs h.F)
        Tough = tough |> List.sumBy (fun h -> damageFor h.F (toughHits h.F))
        Hits = striking |> List.sumBy (fun h -> h.F.Hits - toughHits h.F)
        Dismantlers =
            raid
            |> List.choose (fun h ->
                if h.F.Work > 0 then
                    Some(h.F, h.F.Work * Engine.dismantlePower)
                else
                    None)
        Razing =
            breachers
            |> List.filter (fun h -> not (strikes h.F))
            |> List.sumBy (fun h -> effectiveHits h.F)
    }

let private prices terrain theirs raid horizon rangeOf : Price list =
    let weapons = priceAs terrain theirs raid horizon false rangeOf

    if raid |> List.exists (fun h -> h.F.Heal > 0 && strikes h.F) then
        [ weapons; priceAs terrain theirs raid horizon true rangeOf ]
    else
        [ weapons ]

let private outHeals (price: Price) (ourDamage: int) =
    ourDamage > price.Healing && (price.Tough = 0 || ourDamage > price.ToughHealing)

type private Focused =
    {
        Deals: int
        Heals: int
        Hits: int
        /// What its own swing is struck back a tick, focused or not.
        Back: int
    }

type private Fight =
    {
        Won: bool
        /// None where neither side ends the other.
        Ticks: float option
        Losses: Losses
    }

/// The raid's layers (ramparts, TOUGH, hits) struck at `dealt` for `span`
/// ticks: Ok the tick it falls, else what is left of them.
let private spend (price: Price) dealt span (layers: float list) =
    let rec go (layers: float list) rates span spent =
        match layers, rates with
        | layer :: rest, rate :: rates when layer <= 0.0 || rate > 0.0 && layer / rate < span ->
            let took = if layer <= 0.0 then 0.0 else layer / rate

            go rest rates (span - took) (spent + took)
            |> Result.mapError (fun left -> 0.0 :: left)
        | layer :: rest, rate :: _ ->
            Error((if rate <= 0.0 then layer else layer - span * rate) :: rest)
        | _ -> Ok spent

    go layers (List.map float [ dealt; dealt - price.ToughHealing; dealt - price.Healing ]) span 0.0

/// Past our ramparts (`broken` is what its dismantlers take a tick) the raid
/// kills ours one at a time, fewest hits first; ours fight with those standing.
let private focusedAgainst
    (price: Price)
    beside
    (bodies: Focused list)
    (ourShield, broken)
    (towers, approach)
    : Fight =
    let dealt (alive: (Focused * float) list) =
        beside + List.sumBy (fst >> _.Deals) alive

    let whole = bodies |> List.map (fun b -> b, float b.Hits)
    let total = price.Damage + List.sumBy _.Back bodies

    let price =
        if price.Shield + price.Tough + price.Hits = 0 then
            { price with Hits = price.Razing }
        else
            price

    let worn span (alive: (Focused * float) list) =
        alive |> List.map (fun (b, left) -> b, max 0.0 (left - float b.Back * span))

    let ends won ticks dead (standing: (Focused * float) list) =
        {
            Won = won
            Ticks = ticks
            Losses =
                {
                    Dead = dead
                    Hits = List.sumBy _.Hits bodies - int (List.sumBy snd standing)
                }
        }

    let rec phase layers (alive: (Focused * float) list) elapsed dead =
        match alive with
        | [] -> ends false (Some elapsed) dead []
        | (focus, left) :: rest ->
            let rate = float (price.Damage + focus.Back - List.sumBy (fst >> _.Heals) alive)
            let span = if rate <= 0.0 then infinity else left / rate

            match spend price (dealt alive) span layers with
            | Ok t ->
                ends true (Some(elapsed + t)) dead ((focus, left - t * max 0.0 rate) :: worn t rest)
            | Error _ when span = infinity -> ends false None dead alive
            | Error layers -> phase layers (worn span rest) (elapsed + span) (dead + 1)

    let order = List.sortBy (fun (b: Focused, _) -> b.Hits) whole

    let fight =
        let breaks = total + broken

        match
            spend price towers approach (List.map float [ price.Shield; price.Tough; price.Hits ])
        with
        | Ok t -> ends true (Some t) 0 whole
        | Error layers ->

            match ourShield with
            | 0 -> phase layers order 0.0 0
            | _ ->
                let span =
                    if breaks <= 0 then
                        infinity
                    else
                        float ourShield / float breaks

                match spend price (dealt whole) span layers with
                | Ok t -> ends true (Some t) 0 whole
                | Error _ when span = infinity -> ends false None 0 whole
                | Error layers -> phase layers (worn span order) span 0

    if total = 0 then
        { fight with
            Won = not price.Breaches || outHeals price (dealt whole)
        }
    else
        fight

/// The ticks `hostile` takes, struck alone at `damage`, net of its side's
/// heal, to fall behind `ours` on this ground, lose its last ATTACK, or die.
let private lamedIn
    (terrain: Terrain)
    (raid: Foe list)
    (ours: FatigueFactor)
    (damage: int)
    (hostile: Foe)
    =
    let healers = healersOf raid
    let f = hostile.F

    let netWithin ticks =
        damage - damageFor f (healFrom terrain.Weight ticks healers hostile)

    let needed =
        let rec lamedFrom live =
            let hits = Engine.partHits * live
            let power = Engine.powerAt f.Body f.Boosts hits

            if
                live <= 0
                || power Attack <= 0
                || fallsBehind
                    terrain.Weight
                    ours
                    {
                        Weight = f.Gait.Weight
                        Moves = power Move
                    }
            then
                hits
            else
                lamedFrom (live - 1)

        let live =
            min (List.length f.Body) ((f.Hits + Engine.partHits - 1) / Engine.partHits)

        damageFor f (max 0 (f.Hits - lamedFrom (live - 1)))

    // The window and the heal within it read each other: widened until it holds.
    let rec within ticks tries =
        let net = netWithin ticks

        if net <= 0 then
            None
        else
            let lamed = (needed + net - 1) / net

            if lamed <= ticks || tries = 0 then
                Some lamed
            else
                within lamed (tries - 1)

    within 0 4

/// One body shoots one target a tick: every raider pressing it alike is lamed
/// in turn, the soonest first.
let private pressesFor terrain (raid: Foe list) (fighter: Fighter) (hostile: Foe) : int option =
    let damage = damageAt fighter fighter.Reach

    lamedIn terrain raid fighter.Gait damage hostile
    |> Option.map (fun mine ->
        raid
        |> List.sumBy (fun h ->
            if
                h.At = hostile.At
                || not (strikes h.F)
                || h.F.Reach >= fighter.Reach
                || fallsBehind terrain.Weight fighter.Gait h.F.Gait
            then
                0
            else
                match lamedIn terrain raid fighter.Gait damage h with
                | Some ticks when ticks < mine || ticks = mine && h.At < hostile.At -> ticks
                | _ -> 0)
        |> (+) mine)

/// With both stepping at once, a pursuer at its pace never closes while the
/// kiter has ground to step onto; one quicker closes the ground between reaches.
let private keepsOff (terrain: Terrain) raid (fighter: Fighter) (hostile: Foe) =
    let strike = hostile.F.Reach

    strike < fighter.Reach
    && (fallsBehind terrain.Weight fighter.Gait hostile.F.Gait
        || strike > 0
           && (match pace terrain.Weight fighter.Gait, pace terrain.Weight hostile.F.Gait with
               | Some ours, Some them ->
                   pressesFor terrain raid fighter hostile
                   |> Option.exists (fun presses ->
                       (them = ours
                        || presses < (fighter.Reach - strike) * ours * them / (ours - them))
                       && presses <= terrain.RunOff * ours)
               | _ -> false))

/// Whether a hostile closes onto a body where the range is its own pick: it
/// swings and closing costs it nothing its shot keeps off, or its swing is its weapon.
let private closesIn (fighter: Fighter) (hostile: Fighter) =
    hostile.Attack > 0
    && (hostile.Invader && hostile.Heal = 0
        || damageAt fighter Engine.rangedRange >= damageAt fighter Engine.meleeRange
        || Engine.attackPower * hostile.Attack >= Engine.rangedAttackPower * hostile.Ranged)

[<RequireQualifiedAccess>]
type private Picker =
    | Kites
    | Closes
    | Theirs

/// The faster side picks the range.
let private standoffPick terrain raid (fighter: Fighter) (hostile: Foe) =
    if keepsOff terrain raid fighter hostile then
        fighter.Reach, Picker.Kites
    elif fallsBehind terrain.Weight fighter.Gait hostile.F.Gait || stationary hostile.F then
        bestRange fighter, Picker.Closes
    elif closesIn fighter hostile.F then
        Engine.meleeRange, Picker.Theirs
    else
        Engine.rangedRange, Picker.Theirs

let private standoff terrain raid fighter hostile =
    fst (standoffPick terrain raid fighter hostile)

let private rangesBetween terrain (raid: Foe list) (fighters: Fighter list) =
    let rows =
        raid
        |> List.filter (fun h -> strikes h.F)
        |> List.map (fun h -> h, fighters |> List.map (fun f -> standoff terrain raid f h))

    let ours =
        fighters
        |> List.mapi (fun i _ ->
            match rows |> List.map (fun (_, ranges) -> List.item i ranges) with
            | [] -> Engine.meleeRange, Engine.meleeRange
            | ranges -> max Engine.meleeRange (List.max ranges), List.min ranges)

    let rangeOf (hostile: Foe) =
        match rows |> List.tryFind (fun (h, _) -> h.At = hostile.At) with
        | Some(_, (_ :: _ as ranges)) -> List.min ranges
        | Some _ -> Engine.meleeRange
        | None ->
            match fighters with
            | [] -> Engine.meleeRange
            | _ -> fighters |> List.map (fun f -> standoff terrain raid f hostile) |> List.min

    ours, rangeOf, rows

type private HitBack =
    {
        Taken: int list
        Struck: int
        Swingers: int
        /// The hits of the raiders that strike back a swing of ours.
        Strikers: int
    }

/// A swing on a body with a live ATTACK is struck back by it; a shot never.
/// Who swings at whom is either side's pick, so neither is priced on its best.
let private hitBack
    theirs
    (fighters: Fighter list)
    (deals: int list)
    (rows: (Foe * int list) list)
    : HitBack =
    let swings =
        rows
        |> List.filter (fun (h, _) -> rampartOver theirs h.F = 0)
        |> List.map (fun (h, ranges) -> h.F, Engine.attackPower * h.F.Attack, ranges)
        |> List.filter (fun (_, swing, _) -> swing > 0)

    let swinging =
        List.map2
            (fun (f: Fighter) range -> f.StrikesBack > 0 && range <= Engine.meleeRange)
            fighters
            deals

    let taken =
        swinging
        |> List.mapi (fun i swings' ->
            if not swings' then
                0
            else
                swings
                |> List.fold
                    (fun most (_, swing, ranges) ->
                        if List.item i ranges <= Engine.meleeRange then
                            max most swing
                        else
                            most)
                    0)

    let strikers =
        swings
        |> List.filter (fun (_, _, ranges) ->
            List.exists2
                (fun swings' range -> swings' && range <= Engine.meleeRange)
                swinging
                ranges)
        |> List.sumBy (fun (h, _, _) -> effectiveHits h)

    let struck =
        swings
        |> List.choose (fun (h, _, ranges) ->
            List.zip fighters ranges
            |> List.fold
                (fun least (f: Fighter, range) ->
                    if range <= Engine.meleeRange then
                        Some(min f.StrikesBack (Option.defaultValue f.StrikesBack least))
                    else
                        least)
                None
            |> Option.filter (fun back -> back > 0)
            |> Option.map (fun back -> h, back))

    {
        Taken = taken
        Struck = List.sumBy snd struck
        Swingers = struck |> List.sumBy (fst >> effectiveHits)
        Strikers = strikers
    }

/// Our swings' hit back is taken only while its strikers stand, and they fall first.
let private takenWhile (price: Price) (damage: int) (taken: int) (strikers: int) : int =
    let net = damage - price.Healing

    if taken = 0 || net <= 0 || strikers >= price.Tough + price.Hits then
        taken
    else
        let kill =
            float price.Shield * float net / float damage + float (price.Tough + price.Hits)

        int (ceil (float taken * float strikers / kill))

let private struckBack (price: Price) (damage: int) (struck: int) (swingers: int) : int =
    let hits = int64 (price.Shield + price.Tough + price.Hits)
    let net = int64 (damage - price.Healing)
    let all = net + int64 struck

    if struck = 0 || all <= 0L then
        0
    elif hits <= int64 swingers || int64 struck * hits <= int64 swingers * all then
        struck
    elif net <= 0L then
        0
    else
        int (hits * net / (hits - int64 swingers) - net)

/// A body that kites walks off its rampart.
let private staysOn terrain stance (raid: Foe list) (fighter: Fighter) (tile: Pos) =
    stance = Stance.Hold
    || raid
       |> List.forall (fun h ->
           not (strikes h.F)
           || (match standoffPick terrain raid fighter h with
               | _, Picker.Theirs -> true
               | range, Picker.Closes -> rangeTo tile h.F |> Option.exists (fun r -> r <= range)
               | _, Picker.Kites -> false))

let private onRamparts terrain stance (ours: Side) raid =
    ours.Bodies
    |> List.mapFold
        (fun held (fighter: Fighter) ->
            match
                fighter.Pos
                |> Option.bind (fun at ->
                    Map.tryFind at ours.Ramparts |> Option.map (fun hits -> at, hits))
            with
            | Some(tile, hits) when staysOn terrain stance raid fighter tile ->
                { fighter with StrikesBack = 0 }, (tile, hits) :: held
            | _ -> fighter, held)
        []

let private brokenBy (terrain: Terrain) horizon (price: Price) (ramparts: (Pos * int) list) =
    match ramparts with
    | [] -> 0
    | _ ->
        price.Dismantlers
        |> List.sumBy (fun (h, rate) ->
            let ticks = pace terrain.Weight h.Gait |> Option.defaultValue System.Int32.MaxValue

            let reaches (tile: Pos) =
                rangeTo tile h
                |> Option.exists (fun r ->
                    r <= Engine.meleeRange || int64 (r - 1) * int64 ticks <= int64 horizon)

            if ramparts |> List.exists (fst >> reaches) then rate else 0)

/// The slowest of our walks to within reach of the raid; every heal that
/// reaches the raid before we land counts.
let private arrivalOf (terrain: Terrain) (fighters: Fighter list) (raid: Fighter list) =
    fighters
    |> List.fold
        (fun most (f: Fighter) ->
            let walk =
                match f.Pos with
                | None -> castWalk
                | Some at ->
                    match raid |> List.choose (rangeTo at) with
                    | [] -> 0
                    | tiles ->
                        max 0 (List.min tiles - f.Reach)
                        * (pace terrain.Weight f.Gait |> Option.defaultValue castWalk)

            max most walk)
        0

/// Our towers strike the raid until a weapon of either side reaches: the
/// nearest pair's gap, both walking in, a cast body landing at `castWalk`.
let private approachOf (terrain: Terrain) stance (fighters: Fighter list) (raid: Fighter list) =
    let closing (f: Fighter) (h: Fighter) =
        match f.Pos, h.Pos |> Option.bind (fun at -> rangeTo at f) with
        | None, _ -> Some(float castWalk)
        | Some _, None -> None
        | Some _, Some range ->
            let gap = float (max 0 (range - max f.Reach h.Reach))

            let walks =
                if stance = Stance.Hold then
                    None
                else
                    pace terrain.Weight f.Gait

            match walks, pace terrain.Weight h.Gait with
            | Some ours, Some theirs -> Some(gap * float (ours * theirs) / float (ours + theirs))
            | Some ticks, None
            | None, Some ticks -> Some(gap * float ticks)
            | None, None -> None

    match List.allPairs fighters raid |> List.choose (fun (f, h) -> closing f h) with
    | [] -> 0.0
    | ticks -> List.min ticks

/// A raider that walks in strikes from its range of the body it strikes, the
/// one farthest from the tower.
let private towersOn
    (towers: Tower list)
    (strikesFrom: Foe -> (Fighter * int) list)
    (raid: Foe list)
    =
    let loaded = towers |> List.filter (fun t -> t.Energy >= Engine.towerEnergyCost)

    let on (h: Foe) =
        loaded
        |> List.sumBy (fun t ->
            let here = rangeTo t.Pos h.F |> Option.defaultValue Engine.towerFalloffRange

            match
                strikesFrom h
                |> List.choose (fun (f, range) -> rangeTo t.Pos f |> Option.map ((+) range))
            with
            | [] -> here
            | struck -> List.max struck
            |> Engine.towerAttackAt)

    match loaded, raid |> List.filter (fun h -> strikes h.F) with
    | [], _ -> 0
    | _, [] -> List.length loaded * Engine.towerAttackAt Engine.towerFalloffRange
    | _, striking -> striking |> List.map on |> List.min

let private exchange stance (ours: Side) (theirs: Side) (terrain: Terrain) : Outcome =
    let raid = theirs.Bodies |> List.mapi (fun i f -> { At = i; F = f })
    let fighters, ramparts = onRamparts terrain stance ours raid

    let mine, rangeOf, rows =
        match stance with
        | Stance.Hold ->
            let swing = List.map (fun _ -> Engine.meleeRange) fighters

            List.zip swing swing,
            (fun _ -> Engine.meleeRange),
            raid |> List.filter (fun h -> strikes h.F) |> List.map (fun h -> h, swing)
        | Stance.Standoff -> rangesBetween terrain raid fighters

    let back = hitBack theirs fighters (List.map fst mine) rows

    let kited =
        match stance with
        | Stance.Hold -> []
        | Stance.Standoff -> theirs.Bodies |> List.filter strikes

    let pairs = List.zip fighters mine

    let strikesFrom (h: Foe) =
        match stance, rows |> List.tryFind (fun (r, _) -> r.At = h.At) with
        | _, Some _ when stationary h.F -> []
        | Stance.Hold, Some _ -> fighters |> List.map (fun f -> f, h.F.Reach)
        | Stance.Standoff, Some(_, ranges) -> List.zip fighters ranges
        | _, None -> []

    let towers = towersOn ours.Towers strikesFrom raid
    let walkingIn = towersOn ours.Towers (fun _ -> []) raid

    let bodies =
        pairs
        |> List.map (fun (f, (range, healRange)) ->
            {
                Deals = damageAt f range
                Heals = healAt f healRange
                Hits = f.Hits
                Back = 0
            })

    let dealt = towers + List.sumBy _.Deals bodies
    let approach = approachOf terrain stance fighters theirs.Bodies

    let settle horizon (price: Price) =
        let struck = struckBack price dealt back.Struck back.Swingers
        let damage = dealt + struck

        let bodies =
            List.map2
                (fun (b: Focused) taken ->
                    { b with
                        Back = takenWhile price damage taken back.Strikers
                    })
                bodies
                back.Taken

        let priced =
            { price with
                Damage = price.Damage + List.sumBy _.Back bodies
            }

        let shield = List.sumBy snd ramparts, brokenBy terrain horizon price ramparts

        let fight =
            focusedAgainst price (towers + struck) bodies shield (walkingIn, approach)

        let ticks = fight.Ticks |> Option.map (ceil >> int) |> Option.defaultValue longest

        let outcome =
            if priced.Damage = 0 && not kited.IsEmpty then
                if
                    damage > (kited |> List.map (fun h -> damageFor h priced.Healing) |> List.min)
                then
                    Kite ticks
                else
                    Lose
            elif fight.Won then
                Win(ticks, fight.Losses)
            else
                Lose

        outcome, ticks

    // Every heal and dismantle reaching the fight before it ends counts: the
    // horizon widens to the fight's length, read off both pricings.
    let rec widen horizon rounds =
        let settled =
            prices terrain theirs raid horizon rangeOf |> List.map (settle horizon)

        let lasts = settled |> List.map snd |> List.max

        if lasts <= horizon || rounds = 0 then
            List.map fst settled
        else
            widen (min longest lasts) (rounds - 1)

    match widen (arrivalOf terrain fighters theirs.Bodies) 3 with
    | outcomes when outcomes |> List.exists _.IsLose -> Lose
    | first :: _ -> first
    | [] -> Lose

/// A raid comes to our post only while it must: one that strikes from past
/// the reach of any striker of ours and outlasts our towers picks its range,
/// and the fight is a standoff (owner's call).
let stanceAgainst (stance: Stance) (ours: Side) (theirs: Side) (terrain: Terrain) : Stance =
    let reach =
        match ours.Bodies |> List.filter strikes with
        | [] -> 0
        | striking -> striking |> List.map _.Reach |> List.min

    if
        stance = Stance.Hold
        && theirs.Bodies |> List.exists (fun h -> h.Reach > reach)
        && not (exchange Stance.Hold { ours with Bodies = [] } theirs terrain).IsWin
    then
        Stance.Standoff
    else
        stance

/// Towers not ours strike the kite ground and the ring alike, refilled, and
/// nothing prices them: no side of ours wins under them.
let outcome (stance: Stance) (ours: Side) (theirs: Side) (terrain: Terrain) : Outcome =
    if theirs.Bodies.IsEmpty then
        Win(0, { Dead = 0; Hits = 0 })
    elif not theirs.Towers.IsEmpty then
        Lose
    else
        exchange (stanceAgainst stance ours theirs terrain) ours theirs terrain

/// Sets of one body repeated up to its count in `affordable`, cheapest first:
/// the simple rank a priced one (spawn time, walk, kill) would replace.
let private rank (affordable: Body list) : Body list list =
    affordable
    |> List.groupBy (fun body -> body |> List.map partLetter |> System.String.Concat)
    |> List.collect (fun (_, copies) ->
        let body = List.head copies
        [ 1 .. List.length copies ] |> List.map (fun n -> List.replicate n body))
    |> List.sortBy (fun set ->
        List.sumBy Engine.bodyCost set, List.sumBy List.length set, List.length set)

let cheapestWin
    (stance: Stance)
    (ours: Side)
    (affordable: Body list)
    (theirs: Side)
    (terrain: Terrain)
    : Body list option =
    rank affordable
    |> List.tryFind (fun set ->
        match
            outcome
                stance
                { ours with
                    Bodies = ours.Bodies @ List.map cast set
                }
                theirs
                terrain
        with
        | Win _
        | Kite _ -> true
        | Lose -> false)
