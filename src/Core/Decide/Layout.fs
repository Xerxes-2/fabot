/// The colony reflexes that stand beside the pipeline — safe mode and tower
/// fire — and the Layout: the RCL-gated allowances and the declarative
/// structure plan, plus outpost containers and pickups.
[<AutoOpen>]
module Fabot.Core.Decide.Layout

open Fabot.Core
open Fabot.Core.Types

/// The Layout's one pick rule: cheapest by the caller's price, ties to the
/// lowest (X, Y), and None where there is nothing to pick from. Every tile the
/// Layout settles on goes through it, because a pick that left the coordinates
/// out of its key would be decided by set ordering instead — and set ordering
/// is not the same under Fable as under .NET, so the plan would differ between
/// the bot and its own tests.
let private cheapest (price: 'a -> int) (tileOf: 'a -> Pos) (candidates: 'a list) : 'a option =
    match candidates with
    | [] -> None
    | _ ->
        candidates
        |> List.minBy (fun candidate ->
            let tile = tileOf candidate
            price candidate, tile.X, tile.Y)
        |> Some

/// The hostiles standing in the colony's own room, which is the whole of what
/// the tower reflexes below may read: a tower's shot is a range act inside its
/// own room. The home name and not a tower's room, because the answer is
/// needed on a tick the projection places none.
let private hostilesAtHome (view: ColonyView) : HostileInfo list =
    let home = SpatialInfo.homeName view.Spatial
    view.Hostiles |> List.filter (fun hostile -> hostile.Pos.Room = home)

/// A tower's energy this tick; one the projection holds no store for is read
/// as empty.
let private towerEnergy (view: ColonyView) (towerId: string) =
    view.Refillables
    |> List.tryFind (fun r -> r.Id = towerId)
    |> Option.map (fun r -> Engine.towerCapacity - r.FreeCapacity)
    |> Option.defaultValue 0

/// A hostile's parts acting at these hits, each at its boost's multiple
/// (`Engine.powerAt`), counted once for the many questions asked of them.
let private livePower (h: HostileInfo) (hits: int) : BodyPart -> int =
    if List.isEmpty h.Boosts then
        let counts = Engine.liveParts h.Body hits |> List.countBy id |> Map.ofList
        fun part -> Map.tryFind part counts |> Option.defaultValue 0
    else
        Engine.powerAt h.Body h.Boosts hits

/// Each hostile's live parts by id (#544), the ones acting this tick (the
/// engine destroys parts from the head), each at its boost's multiple.
let private liveCounts (hostiles: HostileInfo list) : Map<string, BodyPart -> int> =
    hostiles |> List.map (fun h -> h.Id, livePower h h.Hits) |> Map.ofList

/// What a hostile of these live parts deals one target of ours at this range
/// a tick: its shot at its reach, and beside it its swing or, on a
/// structure, its dismantle, whichever deals more — a dismantle and an
/// attack share the engine's one melee act, and a ranged attack lands beside
/// either (#591).
let private strikeAt (parts: BodyPart -> int) (structure: bool) (r: int) =
    let shot =
        if r <= Engine.rangedRange then
            Engine.rangedAttackPower * parts RangedAttack
        else
            0

    let melee =
        if r <= Engine.meleeRange then
            max
                (Engine.attackPower * parts Attack)
                (if structure then Engine.dismantlePower * parts Work else 0)
        else
            0

    shot + melee

/// `strikeAt` on one tile of ours in `home`, from where the hostile stands.
let private strikeOn
    (home: string)
    (parts: BodyPart -> int)
    (h: HostileInfo)
    (structure: bool)
    (tile: Pos)
    =
    match RoomPos.range (RoomPos.at home tile) h.Pos with
    | None -> 0
    | Some r -> strikeAt parts structure r

/// One part of the home's means to recover (#545): a spawn, an extension, or
/// the CARRY of a body that refills them.
type private Holding =
    | Spawning
    | Banking
    | Refilling

/// The fewest refilling CARRY whose loss is the economy's (#545): 32, two
/// 16-CARRY haulers, the 1,600 energy a trip that refills an RCL5 bank once.
/// Two cheap bodies under it are two casts, which the home makes itself.
let private refillingFloor = 32

/// How far ahead the economy arm reckons (#545): 50 ticks, a hauler's trip
/// to the store and back or the cast of a refiller, inside which the home
/// cannot answer a loss itself. A loss further off is asked again every tick
/// as it nears (an armed raider at home forces every tick full), so the cap
/// costs no warning, and it bounds the reckoning's CPU.
let private economyHorizon = 50

/// The share of one part of the economy whose loss before the defence stops
/// the raid fires the economy arm (#545): half, and never a single extension
/// or body. Half the extensions is half the bank every body is cast from, and
/// half the CARRY half the hands refilling the spawn and the towers; less, the
/// home rebuilds, and a stock of three with a 50k cooldown is not for that.
/// Half the spawns is the only one of one or two.
let private economyShareLost (part: Holding) (lost: int) (bodies: int) (whole: int) =
    whole > 0
    && 2 * lost >= whole
    && match part with
       | Spawning -> true
       | Banking -> lost >= 2
       | Refilling -> bodies >= 2 && lost >= refillingFloor

/// Whether the home's means to recover is being razed and its defence will
/// not stop it in time (#545): the safe-mode reflex's economy arm, the
/// economy priced as a whole. A tick-by-tick reckoning, every body where it
/// stands: each raider strikes the softest part of the economy its live
/// parts reach (a refiller, then a spawn, then an extension; a rampart of
/// ours over one is hit first), while the towers and our armed creeps, each
/// striking one raider, shoot the striker with the fewest hits; every healer
/// heals the one body `HostileInfo.healTargetOf` gives it. A tower out of
/// energy refills from what a refiller of this home carries, once it has
/// walked to the tower, while it stands. It fires when `economyShareLost` falls within
/// `economyHorizon` and before every striker is dead, aged out or out of
/// anything to strike. A refiller is a body holding a Refill of a spawn, an
/// extension or a tower here this tick, inside the line and off the exit
/// ring: a hauler in transit is its walk's to save, not the stock's. A
/// player's raid only: an Invader recurs, and 20k ticks of safe mode for
/// what one raid takes is a bad trade. A home with no tower is the
/// undefended arm's.
let private economyRazed (view: ColonyView) atlas (held: HeldTaskFacts) : bool =
    let home = SpatialInfo.homeName view.Spatial

    let raiders =
        hostilesAtHome view
        |> List.filter (fun h -> h.Owner <> "Invader" && not (HostileInfo.isKeeper h))
        |> Array.ofList

    match Atlas.placedTowers atlas with
    | [] -> false
    | _ when Array.isEmpty raiders -> false
    | placed ->
        let tiles = (SpatialInfo.layerOf view.Spatial home).TargetPositions
        let living = ColonyView.bodies view |> List.map (fun c -> c.Name, c) |> Map.ofList

        let towers =
            placed
            |> List.map (fun (id, tile) -> tile, towerEnergy view id / Engine.towerEnergyCost)
            |> Array.ofList

        let rampartHits =
            SpatialInfo.structureHits view.Spatial
            |> List.choose (fun (id, kind, hits) ->
                match kind, Map.tryFind id tiles with
                | BuiltKind.Rampart, Some tile -> Some(tile, hits.Hits)
                | _ -> None)
            |> Map.ofList

        let hitsOf =
            SpatialInfo.structureHits view.Spatial
            |> List.map (fun (id, _, hits) -> id, hits.Hits)
            |> Map.ofList

        // (part, tile, structure, hits, carry): a rampart's hits stand in
        // front of what it covers.
        let structures =
            view.Spatial.TargetKinds
            |> Map.toList
            |> List.choose (fun (id, kind) ->
                match kind, Map.tryFind id tiles with
                | Structure(BuiltKind.Extension | BuiltKind.Spawn as built), Some tile ->
                    let own =
                        if built = BuiltKind.Extension then
                            Engine.extensionHits
                        else
                            Map.tryFind id hitsOf |> Option.defaultValue 0

                    let covering = Map.tryFind tile rampartHits |> Option.defaultValue 0
                    let part = if built = BuiltKind.Spawn then Spawning else Banking
                    Some(part, tile, true, own + covering, 0)
                | _ -> None)

        let outside = Atlas.outsideLine atlas view.Perimeter

        // The bodies of ours carrying inside the line, off the exit ring:
        // what refills a tower, and of them the refillers.
        let carriers =
            Atlas.placedCreeps atlas
            |> List.choose (fun (name, at) ->
                let tile = RoomPos.pos at

                match Map.tryFind name living with
                | Some creep when
                    at.Room = home
                    && partCount creep.Body Carry > 0
                    && not (Seam.onRing tile)
                    && not (Set.contains tile outside)
                    ->
                    Some(name, tile, creep)
                | _ -> None)

        let refilling =
            view.Spatial.TargetKinds
            |> Map.toList
            |> List.choose (fun (id, kind) ->
                match kind with
                | Structure(BuiltKind.Extension | BuiltKind.Spawn | BuiltKind.Tower) when
                    Map.containsKey id tiles
                    ->
                    Some(taskId (Refill(id, Energy)))
                | _ -> None)
            |> List.collect (HeldTaskFacts.holdersOf held)
            |> Set.ofList

        let hands =
            carriers
            |> List.choose (fun (name, tile, creep) ->
                if Set.contains name refilling && not (Map.containsKey tile rampartHits) then
                    Some(Refilling, tile, false, creep.Hits.Hits, partCount creep.Body Carry)
                else
                    None)

        let holdings = structures @ hands |> Array.ofList
        let standing = holdings |> Array.map (fun (_, _, _, hits, _) -> hits)

        // The energy each carrier holds for a dry tower, and the holding it
        // dies with when it is a refiller.
        // A dry tower refills from a refiller of this home only, and only
        // once it has walked there: its range to the tower less the transfer's
        // one tile, at its loaded gait off road (a tile a tick per MOVE
        // matching every other part, else slower). Bodies that refill nothing
        // here, an upgrader's crumbs or a hauler passing through, buy no shot.
        let pool =
            carriers
            |> List.choose (fun (name, tile, creep) ->
                if Set.contains name refilling then
                    let moves = max 1 (partCount creep.Body Move)

                    let laden =
                        creep.Body
                        |> Map.toList
                        |> List.sumBy (fun (part, n) -> if part = Move then 0 else n)

                    let gait = max 1 ((laden + moves - 1) / moves)

                    let owner =
                        holdings
                        |> Array.tryFindIndex (fun (part, at, _, _, _) ->
                            part = Refilling && at = tile)

                    Some(owner, tile, gait, creep.Energy)
                else
                    None)
            |> Array.ofList

        // What each raider reaches, at what range: every body holds where it
        // stands, so the reach is found once.
        let reach =
            raiders
            |> Array.map (fun h ->
                holdings
                |> Array.indexed
                |> Array.choose (fun (j, (_, tile, _, _, _)) ->
                    RoomPos.range (RoomPos.at home tile) h.Pos
                    |> Option.filter (fun r -> r <= Engine.rangedRange)
                    |> Option.map (fun r -> j, r)))

        if reach |> Array.forall Array.isEmpty then
            false
        else
            let whole part =
                holdings
                |> Array.sumBy (fun (p, _, _, _, carry) ->
                    if p <> part then 0
                    elif part = Refilling then carry
                    else 1)

            let wholes =
                [ Spawning, whole Spawning; Banking, whole Banking; Refilling, whole Refilling ]

            let hp = raiders |> Array.map (fun h -> h.Hits)
            let ttl = raiders |> Array.map (fun h -> h.TicksToLive)
            let shots = towers |> Array.map snd
            let alive i = hp[i] > 0 && ttl[i] > 0

            let partsAt i = livePower raiders[i] hp[i]

            let fighters =
                Atlas.placedCreeps atlas
                |> List.choose (fun (name, at) ->
                    match Map.tryFind name living with
                    | Some creep when
                        at.Room = home
                        && (partCount creep.Body Attack > 0
                            || partCount creep.Body RangedAttack > 0)
                        ->
                        Some(at, partCount creep.Body)
                    | _ -> None)

            // A raider strikes a refiller first, then a spawn, then an
            // extension, each the softest standing: Meow's order.
            let order (part, _, _, _, _) =
                match part with
                | Refilling -> 0
                | Spawning -> 1
                | Banking -> 2

            let fuel = pool |> Array.map (fun (_, _, _, energy) -> energy)

            // Tower t's next shot on this tick, out of what a standing
            // refiller arrived beside it holds.
            let refuel t tick =
                let at = fst towers[t]

                pool
                |> Array.indexed
                |> Array.tryFind (fun (k, (owner, tile, gait, _)) ->
                    fuel[k] >= Engine.towerEnergyCost
                    && owner |> Option.forall (fun j -> standing[j] > 0)
                    && RoomPos.range at (RoomPos.at home tile)
                       |> Option.exists (fun r -> tick > max 0 (r - 1) * gait))
                |> Option.map (fun (k, _) -> fuel[k] <- fuel[k] - Engine.towerEnergyCost)
                |> Option.isSome

            let rec reckon tick =
                // Each raider's live parts, once a tick.
                let partsOf =
                    Array.init raiders.Length (fun i -> if alive i then partsAt i else fun _ -> 0)

                // What raider i's live parts deal holding j at range r.
                let strike i (j, r) =
                    let parts = partsOf[i]
                    let _, _, structure, _, _ = holdings[j]

                    let dismantle =
                        if structure && r <= Engine.meleeRange then
                            Engine.dismantlePower * parts Work
                        else
                            0

                    max (damageAt parts r) dismantle

                let strikes =
                    [ 0 .. raiders.Length - 1 ]
                    |> List.choose (fun i ->
                        if alive i then
                            reach[i]
                            |> Array.filter (fun (j, r) -> standing[j] > 0 && strike i (j, r) > 0)
                            |> function
                                | [||] -> None
                                | struck ->
                                    let j, r =
                                        struck
                                        |> Array.minBy (fun (j, _) ->
                                            order holdings[j], standing[j], j)

                                    Some(i, j, strike i (j, r))
                        else
                            None)

                let strikers = strikes |> List.map (fun (i, _, _) -> i)

                if List.isEmpty strikes || tick > economyHorizon then
                    false
                else
                    // The raid's strikes on the economy.
                    for _, j, damage in strikes do
                        standing[j] <- standing[j] - damage

                    let razed =
                        wholes
                        |> List.exists (fun (part, whole) ->
                            let lost =
                                holdings
                                |> Array.indexed
                                |> Array.filter (fun (j, (p, _, _, _, _)) ->
                                    p = part && standing[j] <= 0)

                            let share =
                                lost
                                |> Array.sumBy (fun (_, (_, _, _, _, carry)) ->
                                    if part = Refilling then carry else 1)

                            economyShareLost part share lost.Length whole)

                    if razed then
                        true
                    else
                        // The defence: the towers on the striker with the
                        // fewest hits, each armed body of ours on one raider.
                        let dealt = Array.zeroCreate raiders.Length
                        let focus = strikers |> List.minBy (fun i -> hp[i], raiders[i].Id)

                        for t in 0 .. towers.Length - 1 do
                            if shots[t] > 0 || refuel t tick then
                                shots[t] <- max 0 (shots[t] - 1)

                                match RoomPos.range (fst towers[t]) raiders[focus].Pos with
                                | Some r -> dealt[focus] <- dealt[focus] + Engine.towerAttackAt r
                                | None -> ()

                        for at, parts in fighters do
                            let inReach =
                                [ 0 .. raiders.Length - 1 ]
                                |> List.choose (fun i ->
                                    if alive i then
                                        RoomPos.range at raiders[i].Pos
                                        |> Option.filter (fun r -> damageAt parts r > 0)
                                        |> Option.map (fun r -> i, r)
                                    else
                                        None)

                            match inReach |> List.tryFind (fun (i, _) -> i = focus) with
                            | Some(i, r) -> dealt[i] <- dealt[i] + damageAt parts r
                            | None ->
                                match inReach with
                                | [] -> ()
                                | _ ->
                                    let i, r =
                                        inReach |> List.minBy (fun (i, r) -> r, raiders[i].Id)

                                    dealt[i] <- dealt[i] + damageAt parts r

                        // Each healer heals once, the body its heal picks.
                        let side =
                            [ 0 .. raiders.Length - 1 ]
                            |> List.filter alive
                            |> List.map (fun i -> { raiders[i] with Hits = hp[i] })

                        let healed = Array.zeroCreate raiders.Length

                        for i in 0 .. raiders.Length - 1 do
                            let heals = if alive i then partsOf[i]Heal else 0

                            if heals > 0 then
                                match
                                    HostileInfo.healTargetOf side { raiders[i] with Hits = hp[i] }
                                with
                                | Some target ->
                                    match
                                        raiders |> Array.tryFindIndex (fun h -> h.Id = target)
                                    with
                                    | Some k ->
                                        let power =
                                            match RoomPos.range raiders[i].Pos raiders[k].Pos with
                                            | Some r when r <= Engine.meleeRange -> Engine.healPower
                                            | _ -> Engine.rangedHealPower

                                        healed[k] <- healed[k] + power * heals
                                    | None -> ()
                                | None -> ()

                        for i in 0 .. raiders.Length - 1 do
                            if alive i then
                                let top = Engine.partHits * List.length raiders[i].Body

                                let taken =
                                    Engine.damageTaken
                                        raiders[i].Body
                                        raiders[i].Boosts
                                        hp[i]
                                        dealt[i]

                                hp[i] <- min top (hp[i] - taken + healed[i])
                                ttl[i] <- ttl[i] - 1

                        reckon (tick + 1)

            reckon 1

/// The safe-mode reflex over one room of ours and its controller, four arms
/// and one pair of gates — stock remaining, safe mode not already running.
/// The CLAIM arm holds until a claimer stands within reach of the range-1
/// tap; an unplaceable controller falls back to firing on sight. The Keep
/// arm: `keepDented` while any hostile stands in the room — any hostile and
/// not only a Threat, a WORK-only dismantler hurting a structure without ever
/// qualifying as one. The economy arm: `razed` (`economyRazed`, #545), a
/// Keep behind its ramparts says nothing while the extensions and the
/// refillers die. The undefended arm (#217): no tower standing and a
/// hostile in the room that endangers it (`Facts.endangersOurs`, #429: a
/// neutral's WORK body only caught dismantling ours, so one crossing a
/// Nursery never spends the activation), unless one armed body of ours there wins the
/// exchange alone (`Quota.homeHolds`, #448), and only while `guarded` — a
/// Nursery's is a begun site, so a squad crossing an empty claimed room does
/// not spend the shard's one activation. The gates are the stock, no cooldown
/// running on the controller (#474), and no safe mode running here or in any
/// other room of ours: the engine runs one per shard.
/// Stateless on purpose: one tick's hits, never a comparison against the
/// last tick's.
let private safeModeIn
    (view: ColonyView)
    atlas
    (room: string)
    (keepDented: bool)
    (razed: Lazy<bool>)
    (guarded: bool)
    (controller: ControllerInfo)
    : Intent list =
    if
        controller.SafeModeAvailable > 0
        && not controller.SafeModeActive
        && controller.SafeModeCooldownUntil <= view.Time
        && not view.SafeModeRunning
    then
        // That room and no other (#201): a claimer in an outpost is tapping a
        // controller safe mode does not cover.
        let here = view.Hostiles |> List.filter (fun hostile -> hostile.Pos.Room = room)

        let withinReach (h: HostileInfo) =
            HostileInfo.claims here h
            && match Atlas.positionOf atlas controller.Id with
               // Across a border there is no range to measure, so None here is
               // "not in reach", where an unplaced controller below is still
               // "fire on sight".
               | Some tile ->
                   RoomPos.range h.Pos tile
                   |> Option.exists (fun r -> r <= view.Tuning.SafeModeDeadline)
               | None -> true

        let claimerInReach = here |> List.exists withinReach

        let dentedKeepUnderHostiles = keepDented && not (List.isEmpty here)

        // A garrison buys time only while the Keep is untouched, so only this
        // arm asks it (#448).
        let undefended =
            guarded
            && Atlas.builtIn atlas room BuiltKind.Tower = 0
            && here |> List.exists (endangersOurs view)
            && not (Quota.homeHolds view room)

        // Last: the economy's reckoning is the dearest question, asked only
        // past the gates and when no cheaper arm already fires.
        if claimerInReach || dentedKeepUnderHostiles || undefended || razed.Value then
            [ ActivateSafeMode controller.Id ]
        else
            []
    else
        []

/// Colony reflex beside the pipeline (ADR-0007, ADR-0015): `safeModeIn` over
/// the colony's own home, and over each [[nursery]] it raises (#449), which
/// runs no tick of its own. The home's Keep half is `keepDamaged`'s, off the
/// same projected hits the Repair pool walks; the Posts and the ramparts are
/// hungry on their own lines and are not of the Keep; its economy half is
/// `economyRazed`'s. A Nursery has no Keep or economy
/// standing — a spawn is what ends it — and its mother projects no hits there;
/// what it guards is a site of ours with energy already built into it. One
/// activation at most, the home's first: the engine takes one per shard.
let internal planSafeMode (view: ColonyView) atlas (held: HeldTaskFacts) : Intent list =
    let home =
        view.Controller
        |> Option.toList
        |> List.collect (
            safeModeIn
                view
                atlas
                (SpatialInfo.homeName view.Spatial)
                (keepDamaged view)
                (lazy (economyRazed view atlas held))
                true
        )

    let begunIn room =
        view.ConstructionSites
        |> List.exists (fun site ->
            site.Begun
            && Atlas.positionOf atlas site.Id |> Option.exists (fun at -> at.Room = room))

    let nurseries =
        view.NurseryControllers
        |> Map.toList
        |> List.collect (fun (room, controller) ->
            safeModeIn view atlas room false (lazy false) (begunIn room) controller)

    List.truncate 1 (home @ nurseries)

/// The tick's intents, every colony's together, with every `ActivateSafeMode`
/// after the first dropped: the engine takes one per shard and refuses the
/// rest with ERR_BUSY. Colony order decides which, so the answer is the same
/// every time.
let firstActivationOnly (intents: Intent list) : Intent list =
    let rec keep fired kept rest =
        match rest with
        | [] -> List.rev kept
        | ActivateSafeMode _ as intent :: rest ->
            if fired then
                keep fired kept rest
            else
                keep true (intent :: kept) rest
        | intent :: rest -> keep fired (intent :: kept) rest

    keep false [] intents


/// The most of `held` a terminal ships over `range` rooms with `energy` to
/// pay the fee, or None under `Engine.terminalMinSend`. The largest amount the
/// energy pays for, found by the fee rather than by an inverse of it: the
/// engine's formula is an exponential and the amount is an integer, so the
/// affordable amount is read off the fee of what is there.
let private affordableSend (range: int) (held: int) (energy: int) : int option =
    if held < Engine.terminalMinSend then
        None
    else
        let affordable =
            if Engine.sendFee range held <= energy then
                held
            else
                // Scale down by the ratio the fee overshoots by. One correction
                // is enough because the fee is linear in the amount — the
                // exponential is in the range alone.
                let ratio = float energy / float (Engine.sendFee range held)
                float held * ratio |> floor |> int

        if
            affordable >= Engine.terminalMinSend
            && Engine.sendFee range affordable <= energy
        then
            Some affordable
        else
            None

/// The consignment's one intent (#349): a terminal shipping this colony's
/// banked Thorium to the consignee's terminal — the only place the ore crosses
/// the map without a body; the hauling either side is the `Planner`'s. Four
/// facts and no memory: a declared consignee, the terminal's Thorium, its
/// energy, and the engine's fee. Not read: the cooldown (`RoomFacts` carries
/// none, so about a tenth of the asks are refused `ERR_TIRED`, a logged failure
/// and a tick's call), and anything about the far end, which is outside every
/// scan set this colony holds — the engine moves the ore or refuses the intent.
/// The amount is the smaller of what the terminal holds and what its energy
/// pays the fee on, floored at `Engine.terminalMinSend`.
let internal planConsignment (view: ColonyView) : Intent list =
    match view.Consignee, view.Spatial.RoomName with
    | Some destination, Some home ->
        let ship terminalId =
            let banked = SpatialInfo.heldIn view.Spatial Thorium terminalId
            let energy = SpatialInfo.storedIn view.Spatial terminalId

            sendRange home destination
            |> Option.bind (fun range -> affordableSend range banked energy)
            |> Option.map (fun amount -> SendFromTerminal(terminalId, Thorium, amount, destination))

        view.Spatial.TargetKinds
        |> Map.toList
        |> List.filter (fun (_, kind) -> kind = Structure BuiltKind.Terminal)
        |> List.choose (fst >> ship)
    | _ -> []

/// The [[lab cluster]]'s inputs by terminal: a colony whose terminal holds an
/// ore the cluster lacks (`Facts.oreForCluster`) ships it to the cluster's
/// home, no more than the cluster lacks, the fee out of the same terminal's
/// energy (`affordableSend`). Sends between two terminals of ours are the
/// consignment's own, which the season leaves open. A terminal already
/// sending this tick (`sending`) is skipped, and of the ores it holds the one
/// the cluster lacks most goes first: one send a terminal a tick.
let internal planOreSend (view: ColonyView) (sending: Set<string>) : Intent list =
    match Facts.oreForCluster view, view.Spatial.RoomName with
    | Some(destination, short), Some home ->
        match sendRange home destination with
        | None -> []
        | Some range ->
            Facts.ourTerminals view
            |> List.filter (fun id -> not (Set.contains id sending))
            |> List.choose (fun terminalId ->
                let energy = SpatialInfo.storedIn view.Spatial terminalId

                short
                |> Map.toList
                |> List.sortBy (fun (ore, wanted) -> -wanted, ore)
                |> List.tryPick (fun (ore, wanted) ->
                    let held = SpatialInfo.heldIn view.Spatial (Ore ore) terminalId

                    affordableSend range (min held wanted) energy
                    |> Option.map (fun amount ->
                        SendFromTerminal(terminalId, Ore ore, amount, destination))))
    | _ -> []

/// The [[lab cluster]]'s reactions this tick (`Labs.readyRuns`): each output
/// lab ready now, or the tick after — the light tick that follows replays
/// the ask, and the engine refuses the one the cooldown holds back.
let internal planReaction (view: ColonyView) : Intent list =
    let cooldown lab =
        Map.tryFind lab view.Spatial.Cooldowns |> Option.defaultValue 0

    Labs.readyRuns (Facts.labPlan view) (Facts.labHolds view) cooldown 1
    |> List.map RunReaction

/// Energy aid by terminal (user, 2026-10-04): a colony that gives
/// (`Facts.givesAid`) ships its terminal's energy above the consignment's
/// fee reserve to the colony furthest short in `ColonyView.AidRooms`,
/// the send's own fee paid out of the same energy. A terminal already
/// shipping this tick (`sending`) is skipped: one send a terminal a tick.
let internal planAid (view: ColonyView) (sending: Set<string>) : Intent list =
    match view.AidRooms, view.Spatial.RoomName with
    | destination :: _, Some home when Facts.givesAid view ->
        let reserve = Facts.feeReserve view

        // Chebyshev, the engine's metric for the fee (`planConsignment`).
        let range =
            RoomName.offsetOf home destination
            |> Option.map (fun (dx, dy) -> max (abs dx) (abs dy))

        let ship terminalId =
            let spare = SpatialInfo.storedIn view.Spatial terminalId - reserve

            match range with
            | Some range when spare >= Engine.terminalMinSend ->
                // The load and its fee out of the one spare: the fee is linear
                // in the amount, so one scaling and a step back cover it.
                let fits amount =
                    amount + Engine.sendFee range amount <= spare

                let amount =
                    let scaled =
                        float spare / (1.0 + float (Engine.sendFee range 1000) / 1000.0)
                        |> floor
                        |> int

                    if fits scaled then scaled else scaled - 1

                if amount >= Engine.terminalMinSend && fits amount then
                    Some(SendFromTerminal(terminalId, Energy, amount, destination))
                else
                    None
            | _ -> None

        SpatialInfo.idsOfKindIn view.Spatial.TargetKinds (Structure BuiltKind.Terminal)
        |> List.filter (fun id -> not (Set.contains id sending))
        |> List.choose ship
    | _ -> []

/// The fire reflex's held hand (#477): with no hostile worth a shot, each tower
/// still holding `Tuning.TowerRepairReserve` after the act repairs the rampart
/// of ours under attack (`rampartsUnderAttack`, #467) with the fewest hits,
/// then the nearest, then by id — one that would fall inside
/// `Tuning.RampartRepairHorizon` at the strike the bodies reaching it deal
/// (#591). A repair lands 800 near the tower and 200 from range 20, against
/// the 510 one 17-ATTACK melee strikes.
let private planTowerRepair (view: ColonyView) (towers: (string * RoomPos) list) : Intent list =
    let home = SpatialInfo.homeName view.Spatial
    let tiles = (SpatialInfo.layerOf view.Spatial home).TargetPositions

    // The struck ramparts (#477), else with none struck the dying ones
    // (`Tuning.RampartDying`): a fresh rampart decays away at 1 hit before a
    // body comes to it (live 2026-10-04, W17S25). A struck one is the
    // towers' only under its line (#591): the horizon's worth of the strike
    // the raid at home deals it once every body of it stands beside it —
    // the raid recalled between two looks counted, and never less than what
    // strikes it now —, and never under `Tuning.RampartTowerFloor`, so a
    // plinker that strikes it alone holds it no lower than a heavy body
    // walking in breaks. Over the line it is the workers', and the towers
    // hold for kills and heals (W17S29 t996,693: ~2,000 energy into a 112k
    // rampart losing 90).
    let wanted =
        match rampartsUnderAttack view with
        | struck when Set.isEmpty struck ->
            fun _ kind (hits: HitsInfo) ->
                kind = BuiltKind.Rampart && hits.Hits < view.Tuning.RampartDying
        | struck ->
            let hostiles = hostilesAtHome view
            let strikers = liveCounts hostiles

            let line =
                let converged =
                    hostiles |> List.sumBy (fun h -> strikeAt strikers[h.Id] true Engine.meleeRange)

                max
                    (int64 view.Tuning.RampartTowerFloor)
                    (int64 view.Tuning.RampartRepairHorizon * int64 converged)

            fun id _ (hits: HitsInfo) ->
                Set.contains id struck && hits.Hits < hits.HitsMax && int64 hits.Hits < line

    match
        SpatialInfo.structureHits view.Spatial
        |> List.choose (fun (id, kind, hits) ->
            match Map.tryFind id tiles with
            | Some tile when wanted id kind hits -> Some(id, RoomPos.at home tile, hits.Hits)
            | _ -> None)
    with
    | [] -> []
    | ramparts ->

        towers
        |> List.filter (fun (towerId, _) ->
            towerEnergy view towerId - Engine.towerEnergyCost
            >= view.Tuning.TowerRepairReserve)
        |> List.choose (fun (towerId, from) ->
            ramparts
            |> List.choose (fun (id, tile, hits) ->
                RoomPos.range from tile |> Option.map (fun r -> hits, r, id))
            |> function
                | [] -> None
                | reachable ->
                    let _, _, target = List.min reachable
                    Some(RepairWithTower(towerId, target)))

/// The fire reflex's one threat order (#544), first first: a claimer, whose
/// whole approach is the window; then what it deals a creep or structure of
/// ours this tick (`dealt`); then what it heals, sustaining the raid; then
/// what its weapons could deal. Bait, no weapon and no heal, comes last;
/// range only breaks a tie. A healer our damage breaks, within heal range of
/// a body it keeps alive, goes straight after a claimer, as in the kill order
/// (`Facts.KillTier.BrokenHealer`): shooting the body it sustains only hands
/// it back healed (live t981,040, W17S25).
let private threatRank
    (claimer: bool)
    (brokenHealer: bool)
    (dealt: int)
    (healing: int)
    (weapons: int)
    =
    (if claimer then 0
     elif brokenHealer then 1
     else 2),
    -dealt,
    -healing,
    -weapons

/// Colony reflex beside the pipeline (ADR-0014): each tower in turn shoots
/// the hostile worth a shot first in the threat order (`threatRank`), then
/// nearest, then by id, while the towers' shots this tick already on it fall
/// short of its hits and the heal reaching it (#544: no overkill). Worth one
/// (#466): a hostile our damage reaching it this tick — every tower's over
/// its reserve at its range, as if all fired on it, and our creeps' —
/// out-damages the heal reaching it (`healReaching`, the last few ticks'
/// healers counted, #480) by enough that the energy over the reserve kills
/// it before it runs out; or an urgent one — whose weapon reaches our Keep
/// or creeps, or a dismantler beside the Keep, hurting something now; or a
/// claimer (`claimsAFlag`, #451). Not a rampart: it is there to absorb the
/// hits, and a shot healed back is waste. A tower under
/// `Tuning.TowerRepairReserve` shoots only an urgent claimer, or an urgent
/// one the towers and our creeps kill this tick (#480, #544): a strike lands
/// at the end of the tick, so only a kill saves what it strikes. A tower
/// short of one shot's energy fires nothing. With none
/// worth one, the towers repair (`planTowerRepair`). `placedTowers` has
/// always answered home alone, and the hostiles are narrowed to match;
/// `RoomPos.range` answers None across a border.
let internal planFire (view: ColonyView) atlas : Intent list =
    match hostilesAtHome view with
    // In peace the towers still keep a dying rampart (`planTowerRepair`).
    | [] -> planTowerRepair view (Atlas.placedTowers atlas)
    | hostiles ->
        let home = SpatialInfo.homeName view.Spatial
        let towers = Atlas.placedTowers atlas
        let healOn = healReaching view home
        let damageOn = damageReaching view

        let keep = Atlas.keepTilesIn atlas home

        // A creep of ours on our own rampart is not hurt: the swing lands on
        // the rampart (#504). Nor is a [[defender]] ever the reserve's to
        // save: the expendable body, and a shot does not save a lamed one. A
        // raider beside either alone is the kill check's.
        let ramparts = Atlas.ourRampartTilesIn atlas home

        let creepTiles =
            Atlas.placedCreeps atlas
            |> List.choose (fun (name, at) ->
                if
                    at.Room = home
                    && not (isNamedFor defenderPattern name)
                    && not (Set.contains (RoomPos.pos at) ramparts)
                then
                    Some(RoomPos.pos at)
                else
                    None)
            |> Set.ofList

        // Every structure of ours at home a strike takes hits off (#544): the
        // perimeter as much as the Keep. Not a road or a container, which
        // anything walking by stands beside.
        let structureTiles =
            let tiles = (SpatialInfo.layerOf view.Spatial home).TargetPositions

            SpatialInfo.structureHits view.Spatial
            |> List.choose (fun (id, kind, _) ->
                match kind with
                | BuiltKind.Road
                | BuiltKind.Container -> None
                | _ -> Map.tryFind id tiles)
            |> Set.ofList
            |> Set.union keep

        let strikers = liveCounts hostiles

        let hardest (h: HostileInfo) structure (tiles: Set<Pos>) =
            tiles
            |> Seq.fold
                (fun most tile -> max most (strikeOn home strikers[h.Id] h structure tile))
                0

        // What it deals the one creep or structure of ours it strikes
        // hardest (a creep strikes one target a tick), which the threat
        // order reads; and whether it strikes a creep or the Keep, which is
        // urgent.
        let dealt =
            hostiles
            |> List.map (fun h ->
                h.Id, max (hardest h false creepTiles) (hardest h true structureTiles))
            |> Map.ofList

        let hurting (h: HostileInfo) =
            hardest h false creepTiles > 0 || hardest h true keep > 0

        // A tower over its reserve (#480): only these take a shot that is
        // not urgent, so only these are counted in the damage it must beat.
        // A tower short of one shot fires nothing, and counts in no kill.
        let able =
            towers
            |> List.filter (fun (towerId, _) -> towerEnergy view towerId >= Engine.towerEnergyCost)

        let spare =
            able
            |> List.filter (fun (towerId, _) ->
                towerEnergy view towerId - Engine.towerEnergyCost
                >= view.Tuning.TowerRepairReserve)

        // The kill check (2026-10-04 arena, W17S25): a shot that only dents a
        // healed body before the energy over the reserve runs out was a
        // repair of the struck rampart thrown away. Each spare tower pays for
        // its own shots over its reserve; the heal our creeps leave unbeaten
        // runs for as long as the longest of them fires. Where our creeps
        // already out-damage the heal, every shot sticks: the heal priced in
        // the damage that undoes it through its TOUGH (`HostileInfo.damageFor`).
        // A boosted body is settled tick by tick (`HostileInfo.killedWithin`),
        // its TOUGH paid for once as it breaks.
        let killable (h: HostileInfo) =
            let creepNet = int64 (damageOn h - HostileInfo.damageFor h (healOn h))

            let budgets =
                spare
                |> List.choose (fun (towerId, tile) ->
                    RoomPos.range tile h.Pos
                    |> Option.map (fun r ->
                        int64 (Engine.towerAttackAt r),
                        int64 (
                            (towerEnergy view towerId - view.Tuning.TowerRepairReserve)
                            / Engine.towerEnergyCost
                        )))

            match budgets with
            | _ when creepNet > 0L -> true
            | [] -> false
            | _ when List.isEmpty h.Boosts ->
                let towersDeal = budgets |> List.sumBy (fun (damage, shots) -> damage * shots)
                let longest = budgets |> List.map snd |> List.max
                towersDeal + creepNet * longest >= int64 h.Hits
            | _ ->
                let onIt tick =
                    damageOn h
                    + (budgets
                       |> List.sumBy (fun (damage, shots) ->
                           if int64 tick <= shots then int damage else 0))

                HostileInfo.killedWithin
                    h
                    onIt
                    (healOn h)
                    (budgets |> List.map snd |> List.max |> int)

        let claimer h =
            claimsAFlag hostiles (Set.singleton home) h

        let urgent = hostiles |> List.filter (fun h -> claimer h || hurting h)

        let worth =
            urgent
            @ (hostiles |> List.filter (fun h -> not (List.contains h urgent) && killable h))

        // The urgent a tower under its reserve shoots (#544): a claimer, or
        // one every able tower and our creeps kill this tick.
        let belowFloor =
            lazy
                (let killedNow (h: HostileInfo) =
                    let towersDeal =
                        able
                        |> List.sumBy (fun (_, tile) ->
                            RoomPos.range tile h.Pos
                            |> Option.map Engine.towerAttackAt
                            |> Option.defaultValue 0)

                    towersDeal + damageOn h >= HostileInfo.damageFor h (h.Hits + healOn h)

                 urgent
                 |> List.filter (fun h -> claimer h || killedNow h)
                 |> List.map (fun h -> h.Id)
                 |> Set.ofList)

        let ranked =
            worth
            |> List.map (fun h ->
                h,
                threatRank
                    (claimer h)
                    (HostileInfo.healing hostiles h > 0
                     && killable h
                     && hostiles
                        |> List.exists (fun other ->
                            other.Id <> h.Id
                            && RoomPos.range other.Pos h.Pos
                               |> Option.exists (fun r -> r <= Engine.rangedRange)))
                    dealt[h.Id]
                    (HostileInfo.healing hostiles h)
                    (damageAt strikers[h.Id] Engine.meleeRange))

        match worth with
        | [] -> planTowerRepair view towers
        | _ ->
            able
            |> List.mapFold
                (fun (landed: Map<string, int>) (towerId, tile) ->
                    let spareTower = List.contains (towerId, tile) spare

                    let pick =
                        ranked
                        |> List.choose (fun (h, rank) ->
                            let onIt = Map.tryFind h.Id landed |> Option.defaultValue 0

                            if
                                onIt >= HostileInfo.damageFor h (h.Hits + healOn h)
                                || not (spareTower || Set.contains h.Id belowFloor.Value)
                            then
                                None
                            else
                                RoomPos.range tile h.Pos
                                |> Option.map (fun r ->
                                    (rank, r, h.Id), h, onIt + Engine.towerAttackAt r))
                        |> function
                            | [] -> None
                            | reachable -> Some(reachable |> List.minBy (fun (key, _, _) -> key))

                    match pick with
                    | None -> None, landed
                    | Some(_, target, onIt) ->
                        Some(FireTower(towerId, target.Id)), Map.add target.Id onIt landed)
                Map.empty
            |> fst
            |> List.choose id

/// The rampart door (#482): each rampart of ours at home is public while an
/// ally stands within `Tuning.AllyPassRange` of it, no other hostile but a
/// keeper within `Tuning.AllyPassGuard`, and no raid is on the home's record
/// within `Tuning.FightHoldTicks`; private otherwise. Ranges reach across a
/// border (`rangeAcross`): the perimeter stands at the exits. An intent only
/// where the rampart's `isPublic` differs.
let internal planRampartDoors (view: ColonyView) : Intent list =
    let home = SpatialInfo.homeName view.Spatial
    let tiles = (SpatialInfo.layerOf view.Spatial home).TargetPositions

    let raided =
        Map.tryFind home view.Fought
        |> Option.exists (fun latch -> view.Time - latch.Seen <= view.Tuning.FightHoldTicks)

    let guards = view.Hostiles |> List.filter (HostileInfo.isKeeper >> not)

    let within reach (tile: RoomPos) (creeps: HostileInfo list) =
        creeps
        |> List.exists (fun h ->
            Formation.rangeAcross tile h.Pos |> Option.exists (fun r -> r <= reach))

    SpatialInfo.structureHits view.Spatial
    |> List.choose (fun (id, kind, _) ->
        match kind, Map.tryFind id tiles with
        | BuiltKind.Rampart, Some tile ->
            let at = RoomPos.at home tile

            let wanted =
                not raided
                && within view.Tuning.AllyPassRange at view.Allies
                && not (within view.Tuning.AllyPassGuard at guards)

            if wanted <> Set.contains id view.Spatial.PublicRamparts then
                Some(SetRampartPublic(id, wanted))
            else
                None
        | _ -> None)

/// The readers a look is asked by, highest first (#484, #660 S1): the
/// observer's tiers, folded into the requests as their bands. A request is
/// served by its band, then by its value, so a reader that prices its look
/// orders its band's rooms and a reader that prices none keeps its tier's
/// turn; the bands stand until every reader prices its look in one measure.
[<RequireQualifiedAccess>]
type LookBand =
    /// A room that wants eyes: one some colony stands down from on a raid, or
    /// remembers an armed threat in, or shuts on a reservation whose holder
    /// is to be read (`WorldView.Danger`, `WorldView.Questioned`): what a look
    /// there protects is ours, so no kill a look elsewhere may unlock
    /// outranks it while the two are priced on different horizons.
    | Danger
    /// Every room a body out of sight a room's worth prices may stand in, a
    /// dark outpost's or a [[sweep]] candidate's: the kill a look may unlock.
    | Pursuit
    /// A declared colony room not yet ours, or a nursery.
    | Claim
    /// A rival's room or remote (the rival dossier's watch, the live
    /// [[sweep]] candidates among them), or a room on our chains.
    | Watched
    /// A colony's scan set, its transit rectangles included.
    | Box

/// One reader's ask for eyes on a room (#660 S1). Read-is-request: a reader
/// whose last sighting of the room is older than it tolerates asks, and any
/// vision of ours answers it — a body of ours standing or passing there is a
/// sighting as much as an observer's look, and resets the clock. Stateless
/// like every decision: a reader asks while its question stands, so a look
/// that ends the question (a stand-down's raid seen gone) ends the asking,
/// and a continuous watch is a question that never ends.
type LookRequest =
    {
        Room: string
        Band: LookBand
        /// The energy the decision the look unlocks is worth; 0 from a reader
        /// that prices none yet.
        Value: float
        /// How old a sighting of the room the reader still acts on: it asks
        /// once the last one is this many ticks old (1: any tick no vision
        /// of ours answers for the room).
        Tolerates: int
        /// The last tick the look still unlocks the decision (a stand-down's
        /// expiry); None for a continuous watch.
        Until: int option
    }

/// How old a room that wants eyes' last sighting is before the observer
/// looks again (`Facts.lookStride`, #534).
let private dangerStride = Facts.lookStride

/// A request of a reader that prices no look (value 0) and watches on.
let lookRequest (band: LookBand) (tolerates: int) (room: string) : LookRequest =
    {
        Room = room
        Band = band
        Value = 0.0
        Tolerates = tolerates
        Until = None
    }

/// Where every observer looks this tick, one room each and no room twice
/// (#484, #559, #660), given every request and the observers' rooms. Every
/// request still open (`Until` not passed) whose room's last sighting is as
/// old as it tolerates (`lastSeen` None: never seen) and an observer reaches
/// (`Facts.observerReach`) is ranked by its band, then its
/// value, highest first, then longest unseen, then the fewest free observers
/// reaching it, then by name, and each in turn goes to the nearest observer
/// still free, ties by room name. Longest unseen is the round robin: the look
/// itself is the next tick's sighting. An observer's own room is never its
/// look. Which observer serves a room is the ranking's alone: any in reach.
let assignLooks
    (time: int)
    (lastSeen: string -> int option)
    (observers: string list)
    (requests: LookRequest list)
    : Map<string, string> =
    let reach from room =
        if room = from then None else Facts.observerReach from room

    let wants (request: LookRequest) =
        request.Until |> Option.forall (fun until -> time <= until)
        && lastSeen request.Room
           |> Option.forall (fun tick -> time - tick >= request.Tolerates)

    let wanted =
        requests
        |> List.filter wants
        |> List.collect (fun request ->
            observers
            |> List.choose (fun from ->
                reach from request.Room
                |> Option.map (fun distance ->
                    (request.Band, -request.Value, lastSeen request.Room, request.Room),
                    (distance, from))))
        |> List.groupBy fst
        |> List.map (fun (key, holders) -> key, holders |> List.map snd |> List.distinct)

    // Each pick re-reads who is still free: of the rooms ranked alike, the one
    // the fewest free observers reach goes first, so the only observer that
    // reaches a room is not spent on one another observer reaches too.
    let rec pick (looks: Map<string, string>) wanted =
        let servable =
            wanted
            |> List.choose (fun ((band, value, seen, room), holders) ->
                match
                    holders |> List.filter (fun (_, from) -> not (Map.containsKey from looks))
                with
                | [] -> None
                | free -> Some((band, value, seen, List.length free, room), free))

        match servable with
        | [] -> looks
        | _ ->
            let (_, _, _, _, room), free = List.minBy fst servable
            let _, from = List.min free

            servable
            |> List.filter (fun ((_, _, _, _, other), _) -> other <> room)
            |> List.map (fun ((band, value, seen, _, other), holders) ->
                (band, value, seen, other), holders)
            |> pick (Map.add from room looks)

    pick Map.empty wanted

/// Where an observer in `from` looks this tick (#484): `assignLooks` with no
/// other observer.
let aimObserver
    (time: int)
    (from: string)
    (lastSeen: string -> int option)
    (requests: LookRequest list)
    : string option =
    assignLooks time lastSeen [ from ] requests |> Map.tryFind from

/// The observer's watched tier: `Colony.rivals`, every other room a sighting
/// names a rival's, not an ally's (#489: their controllers' safe-mode clocks),
/// and the rooms this colony crosses.
let watchedRooms (seen: Map<string, RoomSighting>) (crossed: Set<string>) : string list =
    let sighted =
        seen
        |> Map.toList
        |> List.filter (fun (_, sighting) ->
            sighting.Rival |> Option.exists (fun owner -> not (Colony.isAlly owner)))
        |> List.map fst

    (Colony.rivals |> List.map fst) @ sighted @ Set.toList crossed |> List.distinct

/// The observer standing at a colony's home, if one does.
let private observerOf (view: ColonyView) : string option =
    let home = SpatialInfo.homeName view.Spatial

    SpatialInfo.idsOfKind view.Spatial (Structure BuiltKind.Observer)
    |> List.tryFind (fun id -> SpatialInfo.roomOf view.Spatial id = Some home)

/// The requests of the readers that price no look, asked where a colony holds
/// an observer (#484's tiers): the declared rooms not yet colonies of ours or
/// still nurseries while no vision of ours answers for them, the `watched`
/// rooms once their sighting is `Tuning.ObserveStaleTicks` old, and the
/// colony's scan set, the rest of the observer's ticks.
let private tierRequests (view: ColonyView) (watched: string list) : LookRequest list =
    (view.Declared
     |> List.filter (fun room ->
         match Map.tryFind room view.Stages with
         | None
         | Some Nursery -> true
         | Some _ -> false)
     |> List.map (lookRequest LookBand.Claim 1))
    @ (watched |> List.map (lookRequest LookBand.Watched view.Tuning.ObserveStaleTicks))
    @ (view.Spatial.Rooms
       |> Map.keys
       |> List.ofSeq
       |> List.map (lookRequest LookBand.Box 1))

/// The [[sweep]] candidates one colony casts (`ColonyView.HarassCast`, the
/// shut ones included, beside the one the sweep narrowed `Harass` to): every
/// room whose worth the world's table prices as a candidate.
let private candidatesOf (view: ColonyView) : string list =
    view.Harass
    |> List.map _.RoomName
    |> Set.ofList
    |> Set.union view.HarassCast
    |> Set.toList

/// The requests one colony prices (#660, #585, #663 M5), each a room that
/// wants eyes, one look in `dangerStride`:
/// - every raid's stand-down, worth the income a fight there keeps a tick
///   (`RoomWorth.Kept`) over the ticks it has left, which a look finding the
///   raid gone ends (`Observe.strikeGone`), open until its expiry — and as
///   much every room a body out of sight that may stand in it may stand in
///   instead (`Quarry.region`), since the look that strikes it needs that
///   room seen too;
/// - in its own band below them (`LookBand.Pursuit`), every body out of
///   sight the worth of a dark outpost holding remembered trespassers or of
///   a live [[sweep]] candidate prices (`RoomWorth.unseenBodies`: its kill
///   and its take over the stride, at the odds it still stands there), once
///   whichever rooms price it, asked of every room it may stand in
///   (`Quarry.region`, which a look at a room finding it nowhere shrinks),
///   each room's request the sum of the bodies that may stand there — the
///   pursuit of a remembered body and the candidate's evidence in one price.
let private pricedRequests (view: ColonyView) : LookRequest list =
    let looking (band: LookBand) (until: int option) (room: string, value: float) =
        {
            Room = room
            Band = band
            Value = value
            Tolerates = dangerStride
            Until = until
        }

    let region =
        Quarry.region
            (fun room next -> Dance.crosses (Map.tryFind room view.Spatial.Borders) room next)
            (fun room -> Map.tryFind room view.Seen |> Option.map _.Tick)
            view.Time

    // Every room a body out of sight that may stand in `room` may stand in.
    let lingering room =
        view.World.Quarries
        |> Map.toList
        |> List.collect (fun (_, track) ->
            match region track with
            | rooms when List.contains room rooms -> rooms
            | _ -> [])

    let standDowns =
        view.RaidedUntil
        |> Map.toList
        |> List.collect (fun (room, until) ->
            let worth = (Facts.roomWorth view room).Kept * float (until - view.Time)

            if worth > 0.0 then
                room :: lingering room
                |> List.distinct
                |> List.map (fun looked -> looking LookBand.Danger (Some until) (looked, worth))
            else
                [])

    let darkOutposts =
        view.RecalledRaiders
        |> List.map _.Pos.Room
        |> List.distinct
        |> List.filter (fun room ->
            not (WorldView.sees view.World room) && Facts.trespassRooms view room)

    // One body is one look's worth wherever it is priced: a dancer that is
    // the quarry of two rooms counts once in each room it may stand in.
    let pursued =
        darkOutposts @ candidatesOf view
        |> List.distinct
        |> List.collect (fun room ->
            RoomWorth.unseenBodies dangerStride (Facts.roomWorth view room)
            |> Map.toList
            |> List.map (fun (id, value) -> id, (room, value)))
        |> List.groupBy fst
        |> List.collect (fun (id, pricings) ->
            let value = pricings |> List.map (snd >> snd) |> List.max

            let rooms =
                match Map.tryFind id view.World.Quarries with
                | Some track -> region track
                | None -> pricings |> List.map (snd >> fst) |> List.distinct

            rooms |> List.map (fun looked -> looked, value))
        |> List.groupBy fst
        |> List.map (fun (room, values) -> room, List.sumBy snd values)
        |> List.filter (fun (_, value) -> value > 0.0)
        |> List.sort
        |> List.map (looking LookBand.Pursuit None)

    standDowns @ pursued

/// The rival dossier's watch on the rival remotes we know (#663 M5): every
/// declared [[sweep]] candidate the live switch has on (`Colony.harass`),
/// whether or not a colony can cast it this tick, worth no decision priced
/// yet, so its band (`Watched`) serves it only on a tick no priced request
/// nor claim takes the observer, once its sighting is `dangerStride` old:
/// the sweep's evidence of a room with nothing remembered in it.
let private dossierRequests: LookRequest list =
    Colony.harass
    |> List.map (fun h -> lookRequest LookBand.Watched dangerStride h.RoomName)

/// Every look the colonies' readers ask this tick, off the first view's
/// world, which every view shares: the rooms that want eyes world-wide
/// (`WorldView.Danger`, `Questioned`), every colony's priced requests
/// (`pricedRequests`), the dossier's watch (`dossierRequests`), and the tiers
/// of each colony holding an observer (`tierRequests`).
let lookRequests (views: ColonyView list) (watched: string list) : LookRequest list =
    match views with
    | [] -> []
    | first :: _ ->
        let danger =
            Set.union first.World.Danger first.World.Questioned
            |> Set.toList
            |> List.map (lookRequest LookBand.Danger dangerStride)

        danger
        @ (views |> List.collect pricedRequests)
        @ dossierRequests
        @ (views
           |> List.filter (fun view -> Option.isSome (observerOf view))
           |> List.collect (fun view -> tierRequests view watched))
        |> List.distinct

/// The looks of the colonies' observers, aimed together (`assignLooks`) off
/// the first view's clock and sightings, which every view shares: the rooms
/// that want eyes world-wide (`WorldView.Danger`, `Questioned`), every
/// colony's priced requests, the dossier's watch, and the tiers of each
/// colony holding an observer (`lookRequests`).
let private looksOf (views: ColonyView list) (watched: string list) : Map<string, string> =
    let observing = views |> List.filter (fun view -> Option.isSome (observerOf view))

    match views, observing with
    | [], _
    | _, [] -> Map.empty
    | first :: _, _ ->
        let lastSeen room =
            Map.tryFind room first.Seen |> Option.map (fun sighting -> sighting.Tick)

        lookRequests views watched
        |> assignLooks
            first.Time
            lastSeen
            (observing |> List.map (fun view -> SpatialInfo.homeName view.Spatial))

/// The world aims every observer once a tick (#559, #660): one ranking over
/// every colony's requests, the watched rooms the union of every colony's
/// crossings beside the rivals, laid in every
/// view as `WorldView.Looks`. The colonies of `idle` (homes that replay or
/// wait this tick, and look at nothing of the world's choosing) hold no look,
/// so a room they would take goes to the next observer free. The observer
/// half of S6.
let aimObservers (idle: Set<string>) (views: ColonyView list) : ColonyView list =
    match views with
    | [] -> []
    | first :: _ ->
        let crossed = views |> List.map (fun view -> view.Crossed) |> Set.unionMany

        let looks =
            looksOf
                (views
                 |> List.filter (fun view ->
                     not (Set.contains (SpatialInfo.homeName view.Spatial) idle)))
                (watchedRooms first.Seen crossed)

        views
        |> List.map (fun view ->
            { view with
                World = { view.World with Looks = Some looks }
            })

/// The observer at home spends its tick (#484): the look the world assigned
/// it, or in a world that assigned none, the look its own view alone aims.
let internal planObserver (view: ColonyView) : Intent list =
    let home = SpatialInfo.homeName view.Spatial

    match observerOf view with
    | None -> []
    | Some observer ->
        view.World.Looks
        |> Option.defaultWith (fun () -> looksOf [ view ] (watchedRooms view.Seen view.Crossed))
        |> Map.tryFind home
        |> Option.map (fun room -> ObserveRoom(observer, room))
        |> Option.toList

/// The fire reflex's quiet twin (#410): with no hostile at home to shoot, each
/// tower heals the creep of ours at home with the most hits still owed, the
/// heal it will land (`Engine.towerHealAt` its range) counted off as it is
/// planned so two towers do not both pour into a wound the first one closes.
/// Never on a tick `planFire` fires: the engine runs a tower's heal before its
/// attack and drops the attack. Ties by name. No energy gate, as `planFire`
/// has none: a dry tower's heal fails harmlessly.
let internal planTowerHeal (view: ColonyView) atlas : Intent list =
    let home = SpatialInfo.homeName view.Spatial

    if not (List.isEmpty (hostilesAtHome view)) then
        []
    else
        let wounded =
            ColonyView.bodies view
            |> List.filter (fun creep -> creep.Hits.Hits < creep.Hits.HitsMax)
            |> List.choose (fun creep ->
                SpatialInfo.creepPlacementOf view.Spatial creep.Name
                |> Option.filter (fun tile -> tile.Room = home)
                |> Option.map (fun tile ->
                    creep.Name, (tile, creep.Hits.HitsMax - creep.Hits.Hits)))
            |> Map.ofList

        ((wounded, []), Atlas.placedTowers atlas)
        ||> List.fold (fun (owed, heals) (towerId, towerTile) ->
            owed
            |> Map.toList
            |> List.filter (fun (_, (_, left)) -> left > 0)
            |> List.sortBy (fun (name, (_, left)) -> -left, name)
            |> List.tryHead
            |> function
                | None -> owed, heals
                | Some(name, (tile, left)) ->
                    let landed =
                        RoomPos.range towerTile tile
                        |> Option.map Engine.towerHealAt
                        |> Option.defaultValue 0

                    Map.add name (tile, left - landed) owed, HealWithTower(towerId, name) :: heals)
        |> snd
        |> List.rev

/// What a controller level allows the room, by kind (Screeps
/// CONTROLLER_STRUCTURES). One table over the kind and not one per kind,
/// because the gap rule below subtracts a census keyed by that same kind: a
/// pair carried separately is a pair that can be handed to each other's
/// allowance. Only the sized kinds are in it; every other kind the Layout
/// places is sized by its own rule.
let private allowanceOf kind level =
    match kind, level with
    | BuiltKind.Extension, (0 | 1) -> 0
    | BuiltKind.Extension, 2 -> 5
    | BuiltKind.Extension, 3 -> 10
    | BuiltKind.Extension, 4 -> 20
    | BuiltKind.Extension, 5 -> 30
    | BuiltKind.Extension, 6 -> 40
    | BuiltKind.Extension, 7 -> 50
    | BuiltKind.Extension, _ -> 60
    | BuiltKind.Tower, (0 | 1 | 2) -> 0
    | BuiltKind.Tower, (3 | 4) -> 1
    | BuiltKind.Tower, (5 | 6) -> 2
    | BuiltKind.Tower, 7 -> 3
    | BuiltKind.Tower, _ -> 6
    | BuiltKind.Storage, (0 | 1 | 2 | 3) -> 0
    | BuiltKind.Storage, _ -> 1
    | BuiltKind.Terminal, (0 | 1 | 2 | 3 | 4 | 5) -> 0
    | BuiltKind.Terminal, _ -> 1
    | BuiltKind.Spawn, 0 -> 0
    | BuiltKind.Spawn, (1 | 2 | 3 | 4 | 5 | 6) -> 1
    | BuiltKind.Spawn, 7 -> 2
    | BuiltKind.Spawn, _ -> 3
    | BuiltKind.Observer, (0 | 1 | 2 | 3 | 4 | 5 | 6 | 7) -> 0
    | BuiltKind.Observer, _ -> 1
    | BuiltKind.Lab, (0 | 1 | 2 | 3 | 4 | 5) -> 0
    | BuiltKind.Lab, 6 -> 3
    | BuiltKind.Lab, 7 -> 6
    | BuiltKind.Lab, _ -> 10
    | _ -> 0

/// The kinds the clustered horizon sizes, and the ones the ceiling below is
/// read over. The Storage is not one of them: it reads no horizon at all and
/// holds its whole allowance from level 0.
let private clusteredKinds =
    [
        BuiltKind.Extension
        BuiltKind.Tower
        BuiltKind.Terminal
        BuiltKind.Spawn
        BuiltKind.Observer
    ]

/// The level past which `allowanceOf` stops growing (ADR-0064): the smallest
/// level at which every clustered kind already answers its catch-all row,
/// jointly, because it is the joint window the reservation is sized at. Read
/// off the table and never written down, so the table is what moves the day
/// the engine adds a level.
///
/// Two premises the table has to keep, unchecked here because a check that
/// fails at module load is a bot that never boots: every clustered kind has a
/// wildcard row of its own (the trailing `| _ -> 0` does not count, and an
/// unbounded row makes the climb diverge), and every clustered kind's row is
/// non-decreasing in the level, which is what makes the reservation never
/// narrower than the placement.
///
/// `terminal` asks for `Int32.MaxValue` and not an arithmetic expression on
/// it: a `when` guard doing arithmetic on `level` is the one place .NET's
/// wrapping int and Fable's JS number would answer differently.
let private allowanceCeiling =
    let terminal kind = allowanceOf kind System.Int32.MaxValue

    let rec climb level =
        if
            clusteredKinds
            |> List.forall (fun kind -> allowanceOf kind level = terminal kind)
        then
            level
        else
            climb (level + 1)

    climb 0

/// Whether the Layout places road sites at all this tick (#209): only for an
/// `Independent` colony. Not an engine unlock — the engine allows a road at
/// RCL1 — but the stage below which a road is the wrong spend: a bootstrapping
/// room's trunk set is thousands of energy of income placed in one tick, on the
/// same surplus tier as the Upgrade and nearer to hand, so every worker builds
/// roads and nobody upgrades.
let private placesRoads (view: ColonyView) = isIndependent view

/// What one outpost road tile is worth over `Tuning.RoadPayback`, in energy:
/// the loaded hauler ticks it saves at the source's rate, each worth the
/// hauler block's cost over its life per unit it carries, less the tile's
/// upkeep and, unbuilt, its build. Plain saves the `[C, C, M]` block one tick
/// loaded and swamp nine. The upkeep is the decay timer running down by
/// itself and the haul's wear on it: two passes a trip (loaded and back)
/// at the rate over one body's load, each `Engine.roadWearout` timer ticks a
/// part. Per block the parts and the load scale together, so the wear is the
/// same whatever size of hauler is cast.
let private roadTileWorth (tuning: Tuning) (rate: int) (swamp: bool) (built: bool) : float =
    let block = haulerPattern.Block
    let moves = partCountIn block Move

    let loaded: Gait =
        {
            Weight = List.length block - moves
            Moves = moves
        }

    let ground = if swamp then Engine.swampWeight else Gait.plain

    let saved =
        match Gait.ticks ground loaded, Gait.ticks Gait.road loaded with
        | Some bare, Some paved -> bare - paved
        | _ -> 0

    let tickValue =
        float (bodyCost block) / float (Engine.creepLifetime * carryCapacityOf block)

    let ratio = if swamp then Engine.roadSwampRatio else 1

    // Timer ticks the tile loses a tick: its own one, and the haul's.
    let wear =
        2.0 * float rate / float (carryCapacityOf block)
        * float (List.length block * Engine.roadWearout)

    let decay =
        (1.0 + wear) * float (Engine.roadDecayAmount * ratio)
        / float (Engine.roadDecayTime * Engine.repairHitsPerEnergy)

    let build = if built then 0 else Engine.roadBuildCost * ratio

    (float rate * float saved * tickValue - decay) * float tuning.RoadPayback
    - float build

/// ADR-0087
/// Each outpost source's road: from `drops` to a tile beside the source's
/// container, the cheapest of the fighter chains (`Atlas.roadAlong`) priced
/// around `avoid` with every road already standing, pending or planned at a
/// road's 1, so a hand road is followed and two outposts share. Kept where it
/// pays (`roadTileWorth` summed over the tiles in rooms the colony works, at
/// the rate the reserver row holds), placed there from home outward up to
/// `Tuning.OutpostRoadSites` pending while the home is not `building`, never
/// on a tile `homeRoads` (the home's own rule) or `homeTaken` holds.
let private planOutpostRoads
    (view: ColonyView)
    atlas
    (home: string)
    (avoid: Set<RoomPos>)
    (homeRoads: Set<Pos>)
    (homeTaken: Set<Pos>)
    (building: bool)
    (drops: RoomPos list)
    : Intent list * OutpostRoad list * Set<RoomPos> =
    let outposts =
        view.Sources
        |> List.sortBy (fun s -> s.Id)
        |> List.choose (fun s ->
            match Atlas.positionOf atlas s.Id with
            | Some tile when
                tile.Room <> home
                && not (List.contains tile.Room view.Borrowed.Rooms)
                && not (Set.contains tile.Room view.Crossed)
                ->
                Some(s.Id, tile)
            | _ -> None)

    let mined = outposts |> List.map (fun (_, tile) -> tile.Room) |> Set.ofList

    // A room whose sites and hits the colony carries this tick, and whose
    // controller nobody else holds: the engine refuses a site under another's
    // reservation (#334).
    let worked room =
        room = home
        || Set.contains room mined
           && Map.tryFind room view.RoomControl
              |> Option.exists (RoomControlInfo.heldByOther >> Option.isNone)

    // The rate the reserver row holds a room at (#515), not this tick's.
    let reservable = reservableRoomsOf view

    // A census of one room read once, however many tiles ask it.
    let perRoom (census: string -> Set<Pos>) =
        let byRoom = System.Collections.Generic.Dictionary<string, Set<Pos>>()

        fun room ->
            match byRoom.TryGetValue room with
            | true, tiles -> tiles
            | _ ->
                let tiles = census room
                byRoom.[room] <- tiles
                tiles

    let pendingIn = perRoom (Atlas.pendingRoadTilesIn atlas)

    // A site of another kind, ours or a rival's, or a container's.
    let collidingIn =
        perRoom (fun room ->
            Set.union
                (Atlas.collidingSiteTilesIn atlas room)
                (Atlas.pendingContainerTilesIn atlas room))

    let standing (tile: RoomPos) =
        Set.contains (RoomPos.pos tile) (Atlas.roadTilesIn atlas tile.Room)

    let pending (tile: RoomPos) =
        Set.contains (RoomPos.pos tile) (pendingIn tile.Room)

    let record sourceId room state (path: RoomPos list) =
        {
            Source = sourceId
            Room = room
            State = state
            Tiles = List.length path
            Standing = path |> List.filter standing |> List.length
            Pending = path |> List.filter pending |> List.length
            Skipped = path |> List.filter (fun tile -> not (worked tile.Room)) |> List.length
        }

    // Every road already standing or pending on the chains' rooms prices as
    // paved; what each planned road adds is paved for the next.
    let alreadyPaved =
        outposts
        |> List.collect (fun (_, tile) -> Atlas.routesFor atlas Atlas.Walker.Bold home tile.Room)
        |> List.concat
        |> List.distinct
        |> List.collect (fun room ->
            Set.union (Atlas.roadTilesIn atlas room) (pendingIn room)
            |> Set.toList
            |> List.map (RoomPos.at room))
        |> Set.ofList
        |> Set.union (RoomPos.setAt home homeRoads)

    let _, records, planned =
        ((alreadyPaved, [], []), outposts)
        ||> List.fold (fun (paved, records, planned) (sourceId, source) ->
            let room = source.Room

            let container =
                Atlas.containerCensusIn atlas room
                |> Set.filter (servesSource (RoomPos.pos source))
                |> Set.toList
                |> cheapest (fun _ -> 0) id

            match container, sourceRateOf view atlas sourceId with
            | None, _ ->
                paved, record sourceId room OutpostRoadState.NoContainer [] :: records, planned
            | Some _, None ->
                paved, record sourceId room OutpostRoadState.Dark [] :: records, planned
            | Some container, Some seen ->
                let rate =
                    reservableRateOf view atlas reservable sourceId |> Option.defaultValue seen

                let goals =
                    Atlas.adjacentWalkableIn atlas room container
                    |> List.map (RoomPos.at room)
                    |> Set.ofList

                // The cheapest chain, the first on a tie.
                let best =
                    Atlas.routesFor atlas Atlas.Walker.Bold home room
                    |> List.choose (fun chain ->
                        Atlas.roadAlong atlas avoid paved drops chain goals)
                    |> List.fold
                        (fun best (cost, path) ->
                            match best with
                            | Some(least, _) when least <= cost -> best
                            | _ -> Some(cost, path))
                        None

                match best with
                | None ->
                    paved, record sourceId room OutpostRoadState.Unrouted [] :: records, planned
                | Some(_, path) ->
                    let worth =
                        path
                        |> List.filter (fun tile -> worked tile.Room)
                        |> List.sumBy (fun tile ->
                            roadTileWorth
                                view.Tuning
                                rate
                                (Atlas.isSwampIn atlas tile.Room (RoomPos.pos tile))
                                (standing tile))

                    if worth >= 0.0 then
                        Set.union paved (Set.ofList path),
                        record sourceId room OutpostRoadState.Planned path :: records,
                        path :: planned
                    else
                        paved,
                        record sourceId room OutpostRoadState.Unpaid path :: records,
                        planned)

    let records = List.rev records
    let tiles = planned |> List.rev |> List.concat |> List.distinct

    // The tiles a site of this rule would go down on, in walking order from
    // home outward.
    let ours =
        tiles
        |> List.filter (fun tile ->
            worked tile.Room
            && not (
                tile.Room = home
                && (Set.contains (RoomPos.pos tile) homeRoads
                    || Set.contains (RoomPos.pos tile) homeTaken)
            ))

    // Every flood the plan asked for is in the table now; the rest go.
    Atlas.sweepRoadFloods atlas

    // The home's build first (#526): no new outpost road site while an
    // extension, the Storage or a tower stands as a site at home or goes down
    // this tick. And never past the player's ceiling: every colony's sites
    // share the engine's one cap, and a level-up asks for a dozen at once.
    let sites =
        if placesRoads view && not building then
            // Off `tiles` and not `ours`: a pending road site is in
            // `homeTaken`, so the home stretch's own would drop out of `ours`.
            let held =
                tiles
                |> List.filter (fun tile ->
                    worked tile.Room
                    && pending tile
                    && not (tile.Room = home && Set.contains (RoomPos.pos tile) homeRoads))
                |> List.length

            let room =
                min
                    (view.Tuning.OutpostRoadSites - held)
                    (view.Tuning.OutpostRoadSiteCeiling - view.PlayerSites)

            ours
            |> List.filter (fun tile ->
                not (standing tile)
                && not (pending tile)
                && not (Set.contains (RoomPos.pos tile) (collidingIn tile.Room)))
            |> List.truncate (max 0 room)
            |> List.map (fun tile -> PlaceConstructionSite(tile, Road))
        else
            []

    sites, records, Set.ofList tiles

/// The extractors, standing or pending, that stand off every mineral the room
/// digs (`minerals`, `Atlas.mineralsIn`), in id order: the engine allows a
/// room one, and the one a mined-out Thorium tile still holds keeps the room's
/// ore from its own. None while the room digs nothing, so a room with no rock
/// to move onto keeps what stands.
let private strayExtractorsIn (atlas: Atlas.Atlas) (room: string) (minerals: (string * Pos) list) =
    if List.isEmpty minerals then
        []
    else
        let rocks = minerals |> List.map snd |> Set.ofList

        [ Structure BuiltKind.Extractor; Site BuiltKind.Extractor ]
        |> List.collect (Atlas.idsOfKind atlas)
        |> List.filter (fun id ->
            Atlas.positionOf atlas id
            |> Option.exists (fun at ->
                at.Room = room && not (Set.contains (RoomPos.pos at) rocks)))
        |> List.sort

/// The extractor moved onto the room's ore (ADR-0089): a stray one
/// (`strayExtractorsIn`) standing in our home is destroyed, and the Layout
/// asks for the one on the ore once the census no longer holds it. Only once
/// the mine it served is empty — its container holds no Thorium — because the
/// container is that mine's only while the extractor marks it
/// (`Facts.ourMineralContainerPairs`). Beside the memoised Layout and never
/// inside it: the container's stock is no census fact.
///
/// Only while the room's dig is wanted (`Facts.digWanted`): a room that will
/// not dig keeps what stands and spends no rebuild. Then a stray site is
/// removed, and the old mine's container goes down with its extractor when
/// it holds nothing and serves nothing else — no source, the controller, nor
/// the rock now dug: a container left standing is repaired for ever and holds
/// one of the room's five.
let internal planExtractorMove (view: ColonyView) atlas : Intent list =
    match view.Controller, view.Spatial.RoomName with
    | Some controller, Some room when controller.Level >= view.Tuning.ExtractorLevel ->
        let minerals = Atlas.mineralsIn atlas room

        if minerals |> List.exists (fst >> Facts.digWanted view) |> not then
            []
        else
            let unemptied =
                Facts.ourMineralContainerPairs view
                |> List.filter (fun (_, containerId) ->
                    SpatialInfo.heldIn view.Spatial Thorium containerId > 0)
                |> List.map fst
                |> Set.ofList

            let tileOf id =
                Atlas.positionOf atlas id
                |> Option.filter (fun at -> at.Room = room)
                |> Option.map RoomPos.pos

            // What a container may still serve: a source, the rock now dug.
            let served =
                (view.Sources |> List.choose (fun s -> tileOf s.Id))
                @ (minerals |> List.map snd)

            let buffers = Atlas.controllerContainers atlas

            let spentContainersBeside tile =
                Atlas.idsOfKind atlas (Structure BuiltKind.Container)
                |> List.filter (fun id ->
                    match tileOf id with
                    | Some at ->
                        range at tile <= 1
                        && SpatialInfo.heldAllIn view.Spatial id = 0
                        && not (Set.contains id buffers)
                        && served |> List.forall (fun rock -> range at rock > 1)
                    | None -> false)

            strayExtractorsIn atlas room minerals
            |> List.collect (fun id ->
                match Map.tryFind id view.Spatial.TargetKinds with
                | Some(Structure BuiltKind.Extractor) when not (Set.contains id unemptied) ->
                    DestroyStructure id
                    :: (tileOf id
                        |> Option.map spentContainersBeside
                        |> Option.defaultValue []
                        |> List.map DestroyStructure)
                | Some(Site BuiltKind.Extractor) -> [ RemoveSite id ]
                | _ -> [])
    | _ -> []

/// How many free tiles nearest the terminal the lab stamp is searched over:
/// the search is every pair of them, and the stamp is wanted beside the
/// terminal anyway.
let private labSearch = 40

/// The [[lab cluster]]'s stamp: `count` tiles, every lab already `placed`
/// among them, two of them reagent labs and every other within
/// `Engine.labReactionRange` of both (`Labs.rolesOf`'s roles), the stamp
/// nearest `anchor` — its tiles' ranges summed, ties to the least tiles in
/// (X, Y) order. Out of `candidates` (free tiles) and the placed labs; empty
/// when no stamp fits, or for fewer than three labs, which react nothing.
let private labStamp
    (count: int)
    (anchor: Pos)
    (placed: Pos list)
    (candidates: Pos list)
    : Pos list =
    let nearest tiles =
        tiles |> List.sortBy (fun (tile: Pos) -> range tile anchor, tile.X, tile.Y)

    let pool =
        placed
        @ (candidates
           |> List.filter (fun tile -> not (List.contains tile placed))
           |> nearest
           |> List.truncate labSearch)

    let reaches a b = range a b <= Engine.labReactionRange

    if count < 3 then
        []
    else
        [
            for i, reagent in List.indexed pool do
                for other in List.skip (i + 1) pool do
                    let outputs =
                        pool
                        |> List.filter (fun tile ->
                            tile <> reagent
                            && tile <> other
                            && reaches tile reagent
                            && reaches tile other)

                    let kept, free =
                        outputs |> List.partition (fun tile -> List.contains tile placed)

                    let stamp = reagent :: other :: List.truncate (count - 2) (kept @ nearest free)

                    if
                        List.length stamp = count
                        && placed |> List.forall (fun tile -> List.contains tile stamp)
                    then
                        yield stamp
        ]
        |> List.sortBy (fun stamp ->
            stamp |> List.sumBy (range anchor),
            stamp |> List.map (fun tile -> tile.X, tile.Y) |> List.sort)
        |> List.tryHead
        |> Option.defaultValue []

/// Colony-level planning step beside the Planner/Matcher pipeline: the
/// deterministic Layout (ADR-0011), computed whole from the Atlas every tick
/// and placed all at once — no persisted plan, no pacing. One ordering rule
/// seats every clustered structure, the working ground excluded (ADR-0022);
/// trunk roads route around the reservation, which the Link footing count
/// widens (ADR-0027); a rampart covers every standing Keep structure and Post
/// container (ADR-0034), a rampart being no footprint at all. The last item,
/// the outpost roads' planned tiles, is the tests' alone: the memo keeps none.
let planLayout
    (view: ColonyView)
    atlas
    : Intent list *
      ServedFooting list *
      UnservedFooting list *
      UnroutedTrunk list *
      DeferredContainer list *
      OutpostRoad list *
      Set<RoomPos>
    =
    let home = Atlas.homeRoom atlas

    // The tile the whole plan is oriented on, and it is a tile of the room
    // being planned.
    let inHome (tile: RoomPos) = Some tile.Room = home

    // The oldest spawn, which the cluster was grown around (#408): an engine
    // id leads with its creation time, so the least id is the first spawn and
    // one the Layout adds later never takes the plan's origin from it. The
    // spawn list's own order is the engine's, and not a rule.
    let anchor =
        view.Spawns
        |> List.sortBy (fun s -> s.Id)
        |> List.tryPick (fun s -> Atlas.positionOf atlas s.Id |> Option.filter inHome)

    match home, anchor, view.Controller with
    | Some room, Some anchorTile, Some controller ->
        let spawnPos = RoomPos.pos anchorTile
        // Same checkerboard colour as the spawn: clustered structures sit on
        // the spawn's colour, leaving the other colour free for movement.
        let parity = (spawnPos.X + spawnPos.Y) % 2

        // The sources this plan is for: the home room's alone.
        let homeSources =
            view.Sources |> List.filter (fun s -> Atlas.targetRoom atlas s.Id = Some room)

        // The working ground — every source's Seats and the controller's
        // Upgrade Work Area — is off-limits to the cluster.
        let working = Atlas.workingGroundIn atlas room

        let buildable = Atlas.buildableTilesIn atlas room

        let ordering =
            buildable
            |> List.filter (fun tile ->
                (tile.X + tile.Y) % 2 = parity && not (Set.contains tile working))
            |> List.sortBy (fun tile -> range tile spawnPos, tile.X, tile.Y)

        // A kind's still-open gap at a level: its allowance there minus the
        // projection's censuses of standing and pending structures of that
        // kind. Judged at the level the kind is reserved for it sizes the
        // reservation; at the current level it sizes the placement.
        //
        // The room being planned, and no other (#140): the allowance is this
        // controller's, so what is subtracted from it is this room's census —
        // a neighbour's site counted here is a site this room never places.
        let gapAt kind level =
            allowanceOf kind level
            - Atlas.builtIn atlas room kind
            - Atlas.pendingIn atlas room kind
            |> max 0

        // The still-unclaimed slots, Storage first and tower next: a built or
        // pending structure keeps its tile out of the ordering (it is a target)
        // and its slot off the plan. The clustered kinds are placed at the
        // horizon; the Storage reads none — its whole allowance is held from
        // level 0, because once an extension takes that tile it never comes
        // back.
        //
        // The horizon is this room's own level plus the lookahead (ADR-0063),
        // read here beside the level the placement filters at rather than off
        // a constant. It sizes the placement alone; the reservation below reads
        // the ceiling instead and no level at all.
        let horizon = Tuning.horizonOf view.Tuning controller.Level

        let storageSlots = gapAt BuiltKind.Storage view.Tuning.StorageLevel

        // The terminal's slot, sized at the horizon and not from level 0
        // (#349): holding a tile from level 0 for a kind unlocked at RCL6 cost
        // a cramped room its fifth extension at RCL2. What the horizon risks
        // is one tile of distance.
        let terminalSlots = gapAt BuiltKind.Terminal horizon

        // ... and at the ceiling for the reservation the trunks dodge: a road
        // planned across the terminal's tile at RCL2 is a road orphaned at
        // RCL6.
        let reservedTerminalSlots = gapAt BuiltKind.Terminal allowanceCeiling
        let spawnSlots = gapAt BuiltKind.Spawn horizon
        let towerSlots = gapAt BuiltKind.Tower horizon
        let extensionSlots = gapAt BuiltKind.Extension horizon
        let observerSlots = gapAt BuiltKind.Observer horizon

        // The same two kinds sized at the ceiling instead, which is what the
        // trunk router dodges. Sized there the reservation is a function of
        // the terrain and of this room's own census, and of no level at all,
        // so a bare room's road plan is identical at every level by
        // construction. It is never narrower than the placement's:
        // `allowanceOf` never decreases and the ceiling is where it stops.
        let reservedSpawnSlots = gapAt BuiltKind.Spawn allowanceCeiling
        let reservedTowerSlots = gapAt BuiltKind.Tower allowanceCeiling
        let reservedExtensionSlots = gapAt BuiltKind.Extension allowanceCeiling
        let reservedObserverSlots = gapAt BuiltKind.Observer allowanceCeiling

        // The Link footings cannot be named here — their targets are the
        // container picks, which are derived from the trunks the reservation is
        // for — but their count can: one per source, one for the controller
        // container, one for the Storage.
        let footingSlots = List.length homeSources + 2

        let clustered =
            ordering
            |> List.truncate (
                storageSlots
                + reservedTerminalSlots
                + reservedSpawnSlots
                + reservedTowerSlots
                + reservedExtensionSlots
                + reservedObserverSlots
                + footingSlots
            )

        let storagePick = ordering |> List.truncate storageSlots

        // The terminal behind the Storage in the same ordering (#349): the
        // ordering is the cluster sorted by range from the spawn, so the tile
        // after the Storage's is the nearest tile to it the cluster has, and
        // the ore's walk from the one store to the other is a hauler's
        // shortest leg.
        let terminalPick =
            ordering
            |> List.skip (min storageSlots (List.length ordering))
            |> List.truncate terminalSlots

        // Reserved before trunks: a trunk never crosses a tile a reserved
        // structure will claim, and the widened window holds the footings as
        // well. The footings' own tiles are settled below.
        let reserved = Set.ofList clustered

        // The reservation as the router reads it, joined once: the trunks ask
        // for it per source per goal, and a room name added to every tile of
        // it at each ask is a census tick's worth of rebuilding for an answer
        // that does not move (#216 R3).
        let reservedTiles = RoomPos.setAt room reserved

        // This room's share of the controller's Upgrade Work Area: a
        // controller the projection files under another room contributes
        // nothing here, rather than its coordinates.
        let upgradeArea =
            Atlas.workArea atlas (Upgrade controller.Id) |> RoomPos.inRoom room

        // Each goal beside the name it is recorded under when a source cannot
        // reach it (#107). The Upgrade Work Area first and the spawns after,
        // which is the order the routes are collected in and therefore the
        // order a loss reads in.
        let trunkGoals =
            (TrunkGoal.UpgradeArea, RoomPos.setAt room upgradeArea)
            :: (view.Spawns
                |> List.choose (fun s ->
                    Atlas.positionOf atlas s.Id
                    |> Option.filter inHome
                    |> Option.map (fun spawn ->
                        TrunkGoal.Spawn s.Id,
                        Atlas.adjacentWalkableIn atlas room (RoomPos.pos spawn)
                        |> List.map (RoomPos.at room)
                        |> Set.ofList)))

        // Every route the Layout asks for, kept per source and per goal: the
        // union paves the roads and each source's own trunk anchors its
        // container, while the goals stay apart because the loss below is per
        // goal.
        let sourceRoutes =
            homeSources
            |> List.sortBy (fun s -> s.Id)
            |> List.choose (fun s ->
                Atlas.positionOf atlas s.Id
                |> Option.filter inHome
                |> Option.map (fun sourcePos ->
                    s.Id,
                    trunkGoals
                    |> List.map (fun (goal, area) ->
                        goal,
                        Atlas.trunkPath atlas reservedTiles sourcePos area
                        |> List.map RoomPos.pos)))

        let sourceTrunks =
            sourceRoutes
            |> List.map (fun (id, routes) -> id, routes |> List.collect snd |> Set.ofList)

        // The empty path is the router's answer for a goal it paved nothing
        // for, and it unions into the road plan contributing nothing. Recorded
        // here, where the source and the goal are both still in scope:
        // downstream there is only a set of tiles, and a trunk that was dropped
        // whole looks exactly like one that was never asked for.
        let unroutedTrunks =
            sourceRoutes
            |> List.collect (fun (id, routes) ->
                routes
                |> List.choose (fun (goal, path) ->
                    if List.isEmpty path then
                        Some { Source = id; Goal = goal }
                    else
                        None))

        let trunkTiles = sourceTrunks |> List.map snd |> List.fold Set.union Set.empty

        // The spawn-bound half of those routes, kept apart from the union: the
        // mineral container is seated against the Storage's trunk, and the
        // Storage stands on the cluster's first pick beside the spawn, so the
        // paved line a deposit's load is carried down is the `Spawn` half. The
        // `UpgradeArea` half leads the other way, and a Seat priced against the
        // union would, at a deposit beyond the spawn, be the Seat furthest from
        // the haul.
        let spawnTrunkTiles =
            sourceRoutes
            |> List.collect (fun (_, routes) ->
                routes
                |> List.collect (fun (goal, path) ->
                    match goal with
                    | TrunkGoal.Spawn _ -> path
                    | TrunkGoal.UpgradeArea -> []))
            |> Set.ofList

        // The controller's Work Area paves its swamps and only its swamps —
        // upgraders shuttle within it, so the dear ground gets a road and the
        // plain ground does not. No reservation can stand here: the Work Area
        // is working ground, which the ordering never offered.
        let workAreaSwamps = upgradeArea |> Set.filter (Atlas.isSwampIn atlas room)

        // Every tile the Layout paves: the trunks plus the Work Area's
        // swamps. The road gap measures this against the projection's road
        // census, and a Link footing is chosen off it.
        let roadPlan = Set.union trunkTiles workAreaSwamps

        // A built road or a pending road site already claims its tile.
        let roadGap =
            Set.difference roadPlan (Atlas.roadTilesIn atlas room)
            |> fun wanted -> Set.difference wanted (Atlas.pendingRoadTilesIn atlas room)

        // The road sites this tick: the whole gap once the colony is
        // `Independent`, none before it (#209). The stage gate is a filter on
        // the placement and not on the plan — `roadPlan` and `roadGap` are
        // computed at every stage, so the trunks still route around the
        // reservation — and it is a gate rather than pacing.
        let placedRoads =
            if placesRoads view then
                Set.difference roadGap (Atlas.pendingContainerTilesIn atlas room)
            else
                Set.empty

        // Containers, computed whole like everything else and RCL-gated by
        // nothing — the engine allows them from level 0. Each source's
        // container sits on the Seat nearest that source's trunk; the trunk's
        // first tile is itself a Seat, so in practice the container lands where
        // the trunk leaves the source and harvest overflow falls straight in.
        // Seats are terrain geometry and trunks avoid only the reservations, so
        // the pick never shifts as the container gets built.
        let sourceContainerPicks =
            sourceTrunks
            |> List.choose (fun (sourceId, trunk) ->
                let seats = Atlas.seatTilesOf atlas sourceId |> RoomPos.inRoom room

                // The trunk guard is not the empty-list one `cheapest` makes:
                // the price below is a `List.min` over the trunk's own tiles.
                if Set.isEmpty trunk then
                    None
                else
                    seats
                    |> Set.toList
                    |> cheapest
                        (fun seat -> trunk |> Set.toList |> List.map (range seat) |> List.min)
                        id
                    |> Option.map (fun seat -> sourceId, seat))

        let sourceContainerTiles = sourceContainerPicks |> List.map snd

        // The controller container: preferably an Upgrade-Work-Area tile beside
        // a trunk and off the road itself; only when that strict set is empty
        // may the buffer share the paving (ADR-0068). The footing fold below
        // records the resulting shortfall rather than deleting the buffer.
        let controllerContainerTile =
            Atlas.positionOf atlas controller.Id
            |> Option.filter inHome
            |> Option.map RoomPos.pos
            |> Option.bind (fun controllerPos ->
                let besideTrunk tile =
                    trunkTiles |> Set.exists (fun t -> range tile t = 1)

                let candidates =
                    upgradeArea
                    |> Set.filter (fun tile ->
                        not (Set.contains tile trunkTiles) && besideTrunk tile)

                let pick tiles =
                    tiles |> Set.toList |> cheapest (fun tile -> range tile controllerPos) id

                candidates
                |> Set.filter (fun tile -> not (Set.contains tile workAreaSwamps))
                |> pick
                |> Option.orElseWith (fun () -> pick candidates))

        // The room's Thorium deposits and the level that unlocks them
        // (ADR-0057 decision 1). `CONTROLLER_STRUCTURES.extractor` is 1 at
        // RCL6, 7 and 8 and 0 below. The current level and not the horizon:
        // the deposit sits on a wall tile off the clustered checkerboard, so
        // there is no window an extension can take and nothing to hold open.
        // The container is gated with it: a container beside a deposit no body
        // can dig is a site the surplus tier builds for nothing.
        let minerals =
            if controller.Level >= view.Tuning.ExtractorLevel then
                Atlas.mineralsIn atlas room
            else
                []

        // The ones built for: an ore only while its dig is wanted, so a room
        // that will not dig spends nothing on its extractor or its container
        // (the census signature signs the gate, `Facts.oreDug`).
        let wanted = minerals |> List.filter (fst >> Facts.digWanted view)

        // The extractor, on the mineral's own tile. One per room ever, so the
        // census is the whole of the gap, and none is asked for while a stray
        // one still holds the room's allowance (`planExtractorMove` takes it
        // down). The tile clause is still owed below (`extractorGap`): a road
        // site is the one kind Screeps allows on a natural wall, and the
        // extractor is the one kind whose tile can never move to dodge one.
        let extractorTiles =
            let census = Atlas.extractorCensusIn atlas room

            if List.isEmpty (strayExtractorsIn atlas room minerals) then
                wanted
                |> List.map snd
                |> List.filter (fun tile -> not (Set.contains tile census))
            else
                []

        // The mineral container, on the deposit's Seat nearest the Storage's
        // trunk: the source container's rule with the source swapped out. A
        // mineral has no trunk of its own — nothing paves one to a deposit — so
        // it is seated against the line the load is carried down to the
        // Storage, which is `spawnTrunkTiles` and not the whole network. At a
        // wall mouth this is a choice between one and three tiles and never a
        // search.
        let mineralContainerPicks =
            wanted
            |> List.choose (fun (mineralId, _) ->
                let seats = Atlas.seatTilesOf atlas mineralId |> RoomPos.inRoom room

                // The same trunk guard the source picks carry: the price below
                // is a `List.min` over the trunk's own tiles, which has no
                // answer for a room that paved none — and a room that reached
                // no spawn has no haul to the Storage to price against either.
                if Set.isEmpty spawnTrunkTiles then
                    None
                else
                    seats
                    |> Set.toList
                    |> cheapest
                        (fun seat ->
                            spawnTrunkTiles |> Set.toList |> List.map (range seat) |> List.min)
                        id
                    |> Option.map (fun seat -> mineralId, seat))

        // The Link footings: one tile held beside every target a link will
        // ever serve — each planned source container, the controller container,
        // and the Storage. Not the mineral container: a link carries energy and
        // nothing else. Planned, not built: a Post needs a standing container,
        // so a Post-anchored rule would reserve nothing at level 0. The tiles
        // are settled here rather than with the reservation because a
        // footing's targets are the container picks, derived from the trunks
        // the reservation is for; re-flooding the trunks to name the tiles
        // first would pay the tick's dearest step twice.
        let footingTargets =
            [
                for tile in sourceContainerTiles -> tile, FootingKind.SourceContainer
                for tile in Option.toList controllerContainerTile ->
                    tile, FootingKind.ControllerContainer
                for tile in storagePick -> tile, FootingKind.Storage
                for tile in
                    Set.union
                        (Atlas.storageTilesIn atlas room)
                        (Atlas.pendingStorageTilesIn atlas room)
                    |> Set.toList -> tile, FootingKind.Storage
            ]
            |> List.distinctBy fst

        // The tiles no footing may be reserved on: every footing target, since
        // a link beside one container may not stand on another's tile, and the
        // mineral container's and the terminal's picks with them (#349), which
        // are the targets of no footing and reach this list through no other
        // route. A link and a container cannot share a tile, and the loss is
        // silent in both directions: the footing fold would record the tile as
        // served while the container site took it.
        let footingBlockedTiles =
            (footingTargets |> List.map fst)
            @ (mineralContainerPicks |> List.map snd)
            @ terminalPick

        // A standing link is a target, so its own footing has stopped being
        // buildable: added back, or the footing would jump the tick the link
        // went up. The working ground is deliberately not subtracted — a
        // footing is the one structure footing allowed there, because a link
        // on a Seat or an Upgrade tile is exactly what buys the Anchor and the
        // upgraders a transfer without leaving their tile.
        let footingCandidates =
            Set.union (Set.ofList buildable) (Atlas.linkTilesIn atlas room)

        // A target with no candidate at all leaves the tiles alone and is
        // recorded: the fold reserves what it can, and the shortfall rides out
        // beside the plan instead of falling through in silence. What it does
        // reserve rides out too, each tile beside the target and the kind it
        // was reserved for — both in scope here and nowhere else, since no
        // Intent ever names a link.
        let footingTiles, servedFootings, unservedFootings =
            ((Set.empty, [], []), footingTargets)
            ||> List.fold (fun (taken, served, unserved) (target, kind) ->
                footingCandidates
                |> Set.filter (fun tile ->
                    range tile target = 1
                    && not (Set.contains tile roadPlan)
                    && not (List.contains tile footingBlockedTiles)
                    && not (Set.contains tile taken))
                |> Set.toList
                |> cheapest (fun tile -> range tile spawnPos) id
                |> function
                    | None ->
                        taken,
                        served,
                        {
                            Target = RoomPos.at room target
                            Kind = kind
                        }
                        :: unserved
                    | Some tile ->
                        Set.add tile taken,
                        {
                            Target = RoomPos.at room target
                            Kind = kind
                            Tile = RoomPos.at room tile
                        }
                        :: served,
                        unserved)

        // The spawns, the tower and the extensions take the ordering again
        // with the footings held out — a footing outranks all three — and the
        // Storage's and the terminal's picks held out with them (#349). The
        // spawns first (#408): the nearest free tile to the first spawn is
        // where a second one shares its refill round. The `…Slots` and not the
        // `reserved…` counts: this is the placement, sized at the horizon,
        // inside the reservation the trunks already dodged. The observer last
        // (#484): it is never refilled, so the furthest tile is its.
        let clusterPicks =
            ordering
            |> List.filter (fun tile ->
                not (List.contains tile storagePick)
                && not (List.contains tile terminalPick)
                && not (Set.contains tile footingTiles))
            |> List.truncate (spawnSlots + towerSlots + extensionSlots + observerSlots)

        let spawnTiles, rest =
            clusterPicks |> List.splitAt (min spawnSlots clusterPicks.Length)

        let towerTiles, rest = rest |> List.splitAt (min towerSlots rest.Length)

        let extensionTiles, observerTiles =
            rest |> List.splitAt (min extensionSlots rest.Length)

        // The container census the target clause is judged against: a
        // container standing, or a site already going up.
        let containerCensus = Atlas.containerCensusIn atlas room

        // The target clause (ADR-0040): a source is served when a container
        // stands or is pending within range 1 of it, the controller when one
        // stands or is pending in its Upgrade Work Area. A pick the clause
        // defers because something else serves its target is a loss the room
        // keeps, so it rides out beside the footings and the trunks. Named for
        // the geometry and not for the source: two kinds of rock are judged by
        // it and the rule is one rule.
        let servingRock rockId =
            Atlas.positionOf atlas rockId
            |> Option.filter inHome
            |> Option.map (fun rockPos ->
                Set.filter (servesSource (RoomPos.pos rockPos)) containerCensus)
            |> Option.defaultValue Set.empty

        // Every target beside its pick and the containers already serving it.
        // Both answers below are read off this one list, so each target is
        // judged once and the same judgement decides whether it is planned for
        // and whether it lost its pick.
        let targets =
            [
                for sourceId, pick in sourceContainerPicks ->
                    ContainerTarget.Source sourceId, pick, servingRock sourceId
                for pick in Option.toList controllerContainerTile ->
                    ContainerTarget.Controller, pick, Set.intersect containerCensus upgradeArea
                for mineralId, pick in mineralContainerPicks ->
                    ContainerTarget.Mineral mineralId, pick, servingRock mineralId
            ]

        let unservedPicks =
            targets
            |> List.choose (fun (_, pick, serving) ->
                if Set.isEmpty serving then Some pick else None)

        let deferredContainers =
            targets
            |> List.choose (fun (target, pick, serving) ->
                if Set.isEmpty serving || Set.contains pick serving then
                    None
                else
                    Some
                        {
                            Target = target
                            Pick = RoomPos.at room pick
                            Serving = RoomPos.at room (Set.minElement serving)
                        })

        // The tile clause, and only it: a pick whose tile another site already
        // holds waits, because the engine takes one construction site per tile.
        // `Atlas.collidingSiteTilesIn` is every pending site in the room but a
        // container's, ours (#246) or a rival's (#248: a site placed while the
        // room was neutral survives into the room we claim). Beside it, the
        // roads placed this tick, which no census carries yet — the placed
        // roads and not the whole gap, since below the road gate none is
        // placed at all.
        let takenTiles = Set.union placedRoads (Atlas.collidingSiteTilesIn atlas room)

        // Distinct, because the picks are made per target and the targets are
        // judged independently: a deposit two tiles from a rock, or from the
        // controller's Work Area, can be seated on the very tile that rock's
        // container was picked for. Two `PlaceConstructionSite(tile,
        // Container)` in one tick is one site and one `ERR_INVALID_TARGET`,
        // and one container within range 1 of both serves both.
        let containerGap =
            unservedPicks
            |> List.filter (fun tile -> not (Set.contains tile takenTiles))
            |> List.distinct

        // The extractor's tile clause, subtracted off the same census the
        // containers are: the one kind that can stand on the wall the deposit
        // occupies is a road, which the engine allows there as a tunnel, so a
        // site on that tile is a placement the engine refuses and the plan
        // cannot route around.
        let extractorGap =
            extractorTiles |> List.filter (fun tile -> not (Set.contains tile takenTiles))

        // The [[lab cluster]]'s stamp (`labStamp`), as near the terminal as
        // the plan leaves room and around every lab already standing or
        // going up. Only at the
        // cluster's home over its Storage floor (`Facts.labsFunded`), and one
        // site at a time, so the floor is asked again before each 50k goes
        // in. Off every tile the plan holds for something else: the
        // reservation, the footings, the roads, the container picks, the
        // perimeter and every site's tile.
        let labTiles, labSites =
            if Facts.labsFunded view then
                let tileIn kind =
                    Atlas.idsOfKind atlas kind
                    |> List.choose (fun id ->
                        Atlas.positionOf atlas id |> Option.filter inHome |> Option.map RoomPos.pos)

                let placed = tileIn (Structure BuiltKind.Lab) @ tileIn (Site BuiltKind.Lab)

                let anchor =
                    tileIn (Structure BuiltKind.Terminal)
                    @ terminalPick
                    @ (Atlas.storageTilesIn atlas room |> Set.toList)
                    @ [ spawnPos ]
                    |> List.head

                let held =
                    Set.unionMany
                        [
                            reserved
                            footingTiles
                            roadPlan
                            takenTiles
                            Set.ofList view.Perimeter
                            Set.ofList (
                                sourceContainerTiles
                                @ Option.toList controllerContainerTile
                                @ (mineralContainerPicks |> List.map snd)
                            )
                        ]

                let stamp =
                    labStamp
                        (min view.Tuning.Labs (allowanceOf BuiltKind.Lab controller.Level))
                        anchor
                        placed
                        (ordering |> List.filter (fun tile -> not (Set.contains tile held)))

                let fresh = stamp |> List.filter (fun tile -> not (List.contains tile placed))

                stamp,
                (if Atlas.pendingIn atlas room BuiltKind.Lab > 0 then
                     []
                 else
                     List.truncate 1 fresh)
            else
                [], []

        // The ramparts: one over every standing Keep structure and every
        // standing Post container, the tick the thing it covers stands — a site
        // is not covered until it is a structure — and one on every tile of the
        // declared perimeter (#446). No allowance to size against: the gap is
        // the covering census alone, standing ramparts subtracted the way the
        // roads' are, and every pending site's tile — a rampart's needs no
        // second, and a road site on a choke holds its tile until the road
        // stands. The one gate is the colony's stage (`keepsRamparts`), and the
        // perimeter's second: a tower standing in the room, so sixteen
        // decaying sites never race the first tower or pause a raised child's
        // borrowed Upgrade (`sitesPendingBeside`).
        let perimeter =
            if Atlas.builtIn atlas room BuiltKind.Tower > 0 then
                Set.ofList view.Perimeter
            else
                Set.empty

        let covered =
            if keepsRamparts view then
                Set.unionMany
                    [
                        Atlas.keepTilesIn atlas room
                        Atlas.postContainerTilesIn atlas room
                        perimeter
                    ]
            else
                Set.empty

        let rampartGap =
            Set.difference
                covered
                (Set.union (Atlas.rampartTilesIn atlas room) (Atlas.collidingSiteTilesIn atlas room))

        // The outpost roads leave from the Storage, standing, pending or
        // picked (held from level 0), or the oldest spawn beside it, whichever
        // the line leaves more cheaply: a Storage boxed in by extensions would
        // leave none. They dodge the reservation and the footings, and give
        // way to every other home site this tick.
        let drops =
            (Set.union (Atlas.storageTilesIn atlas room) (Atlas.pendingStorageTilesIn atlas room)
             |> Set.toList
             |> List.truncate 1)
            @ storagePick
            @ [ spawnPos ]
            |> List.truncate 2
            |> List.map (RoomPos.at room)

        let storageSites =
            storagePick |> List.truncate (gapAt BuiltKind.Storage controller.Level)

        let towerSites =
            towerTiles |> List.truncate (gapAt BuiltKind.Tower controller.Level)

        let extensionSites =
            extensionTiles |> List.truncate (gapAt BuiltKind.Extension controller.Level)

        let outpostRoadSites, outpostRoads, plannedRoads =
            planOutpostRoads
                view
                atlas
                room
                (Set.unionMany
                    [
                        reservedTiles
                        RoomPos.setAt room footingTiles
                        RoomPos.setAt room (Set.ofList labTiles)
                    ])
                roadPlan
                (Set.unionMany
                    [ takenTiles; Set.ofList containerGap; Set.ofList extractorGap; rampartGap ])
                (Facts.homeBuilding view
                 || not (
                     List.isEmpty storageSites
                     && List.isEmpty towerSites
                     && List.isEmpty extensionSites
                 ))
                drops

        let place kind tiles =
            tiles
            |> List.map (fun tile -> PlaceConstructionSite(RoomPos.at room tile, kind))

        place Storage storageSites
        @ place Terminal (terminalPick |> List.truncate (gapAt BuiltKind.Terminal controller.Level))
        @ place Spawn (spawnTiles |> List.truncate (gapAt BuiltKind.Spawn controller.Level))
        @ place Tower towerSites
        @ place Extension extensionSites
        @ place
            Observer
            (observerTiles |> List.truncate (gapAt BuiltKind.Observer controller.Level))
        @ place Lab labSites
        @ place Road (Set.toList placedRoads)
        @ place Container containerGap
        @ place Extractor extractorGap
        @ place Rampart (Set.toList rampartGap)
        @ outpostRoadSites,
        List.rev servedFootings,
        List.rev unservedFootings,
        unroutedTrunks,
        deferredContainers,
        outpostRoads,
        plannedRoads
    // A room the Layout cannot even orient itself in plans nothing and loses
    // nothing: there are no targets to serve and no trunk was ever asked for,
    // so every record is empty rather than any of them being a shortfall.
    | _ -> [], [], [], [], [], [], Set.empty

/// The outpost's source containers — the colony's one placement rule that is
/// not the Layout's: one container per outpost source, and per [[ore]] deposit
/// the colony digs past home (the Reactor's room's), on the Seat whose walk
/// out to the Seam toward home is shortest, the Seam being the only fixed
/// thing in a room with no spawn to pave a trunk to. The room is the outpost's
/// own: the Layout stamps the single room it plans onto every site it emits,
/// so a pick routed through it would land on the home room's coordinates.
/// Served by target, with the census read in the source's own room.
///
/// And a tile clause (#244): a human lays road sites across Seats, and a
/// container asked for on an occupied tile is `ERR_INVALID_TARGET` once a
/// tick for ever — no container, no Post, no income. A Seat holding another
/// site, ours or anyone's (`Atlas.collidingSiteTilesIn`), is no candidate, and
/// a source whose every Seat is taken waits; our own sites clear once the
/// builders' queue reaches them (#266), a rival's never, and the second list
/// records whose sites each wait is on (#247). A standing road is not
/// subtracted: a container site goes down on a built road.
///
/// Only into a room the colony can see: planning off a blind room's empty
/// census would hand the Executor an `ActorMissing` a tick per rock. Recomputed
/// every tick and not ridden on the plan memo: signing the pending census it
/// reads would throw the whole Layout away the tick an outpost site appears,
/// and what the memo would buy is one flood a room a tick — 4.84 ms without
/// this rule against 5.35 with it.
let internal planOutpostContainers (view: ColonyView) atlas : Intent list * WaitingContainer list =
    let home = SpatialInfo.homeName view.Spatial

    // And the ore rooms' deposits the colony digs (`Facts.digWanted`) under
    // the extractor already standing on them: the miner stands on its
    // container as on a home mine's, and no spawn there paves a trunk to
    // seat it against.
    let mines =
        Facts.ourDeposits view
        |> List.filter (fun id ->
            Atlas.targetRoom atlas id |> Option.exists (fun room -> room <> home)
            && Facts.digWanted view id
            && (Atlas.extractorOn atlas id).IsSome)

    // Every rock the projection places in a room that is not home and that
    // the colony is looking into this tick — `RoomControl` carries one
    // entry per seen room, and vision is what both the census below and
    // the Executor's own `Game.rooms` lookup are paid for with.
    (view.Sources |> List.map (fun s -> s.Id)) @ mines
    |> List.choose (fun rockId ->
        match Atlas.positionOf atlas rockId with
        // Not a nursery's rock its pioneers dig (#473): a container is a
        // third of a spawn, and the child plans its own once it casts.
        // Nor into a room anybody but us reserves (#334): the engine refuses
        // the site there, the reserver row's refusal one act over.
        | Some tile when
            tile.Room <> home
            && Map.tryFind tile.Room view.RoomControl
               |> Option.exists (RoomControlInfo.heldByOther >> Option.isNone)
            && not (List.contains tile.Room view.Borrowed.Rooms)
            ->
            Some(rockId, tile)
        | _ -> None)
    |> List.choose (fun (sourceId, source) ->
        let room = source.Room

        let served =
            Atlas.containerCensusIn atlas room
            |> Set.exists (servesSource (RoomPos.pos source))

        if served then
            None
        else
            let seats = Atlas.seatTilesOf atlas sourceId |> RoomPos.inRoom room
            let taken = Set.intersect seats (Atlas.collidingSiteTilesIn atlas room)

            if not (Set.isEmpty seats) && taken = seats then
                let rival = Set.intersect taken (Atlas.rivalSiteTilesIn atlas room)

                Some(
                    Choice2Of2
                        {
                            Source = sourceId
                            Room = room
                            Ours = Set.difference taken rival |> Set.toList
                            Rival = Set.toList rival
                        }
                )
            else
                // Three Seats all of swamp can price identically, so the lowest
                // (X, Y) answers. Over the Seats a site of another kind has not
                // already taken (#244).
                Set.difference seats taken
                |> Set.toList
                |> List.choose (fun seat ->
                    Atlas.seamWalkTicks atlas room home seat
                    |> Option.map (fun walk -> walk, seat))
                |> cheapest fst snd
                |> Option.map (fun (_, seat) ->
                    Choice1Of2(PlaceConstructionSite(RoomPos.at room seat, Container))))
    |> fun picks ->
        picks
        |> List.choose (function
            | Choice1Of2 site -> Some site
            | Choice2Of2 _ -> None),
        picks
        |> List.choose (function
            | Choice2Of2 waiting -> Some waiting
            | Choice1Of2 _ -> None)

/// Each [[nursery]]'s declared first spawn (#476), placed by its mother: a
/// nursery runs no decide, and before this a human placed it. Only while the
/// room is a nursery this colony projects and can see, and no spawn site of
/// ours stands in it — the pending site is the stop, so it goes down once.
/// The spawn ends the nursery, and the child's own Layout plans around it.
let internal planFirstSpawns (view: ColonyView) atlas : Intent list =
    let spawnSiteIn room =
        view.ConstructionSites
        |> List.exists (fun site ->
            Map.tryFind site.Id view.Spatial.TargetKinds = Some(Site BuiltKind.Spawn)
            && Atlas.positionOf atlas site.Id |> Option.exists (fun at -> at.Room = room))

    view.FirstSpawns
    |> Map.toList
    |> List.filter (fun (room, _) ->
        isNurseryRoom view room
        && Map.containsKey room view.RoomControl
        && not (spawnSiteIn room))
    |> List.map (fun (room, first) -> PlaceSpawnSite(RoomPos.at room first.Tile, first.Name))

/// The colony's signature, written by whoever is standing there (#381). A
/// reflex and not a Task: it sends nobody anywhere, and the engine keeps a sign
/// until it is overwritten. It reaches what somebody already has business
/// beside — an outpost's controller by its reserver, a claimed room by its
/// claimer — and not a home controller, which the upgrader row works from
/// range 3 (four were signed by hand once). Silent when the text standing
/// there is ours; absence is not a match, which is the case this exists for.
/// One creep per controller per tick, the lowest name beside it. Geometry
/// through the Atlas and not off the projection, so the keeper margin's mask
/// reaches this rule too.
let internal planSignatures (view: ColonyView) atlas (textFor: string -> string) : Intent list =
    let signed room =
        Map.tryFind room view.RoomControl
        |> Option.bind (fun control -> control.Sign)
        |> Option.contains (textFor room)

    let placed = Atlas.placedCreeps atlas

    SpatialInfo.idsOfKind view.Spatial Controller
    |> List.choose (fun id ->
        match Atlas.positionOf atlas id with
        | Some at when not (signed at.Room) ->
            placed
            |> List.filter (fun (_, tile) ->
                RoomPos.range tile at |> Option.exists (fun range -> range <= 1))
            |> List.map fst
            |> List.sort
            |> List.tryHead
            |> Option.map (fun name -> SignController(name, id, textFor at.Room))
        | _ -> None)

/// Colony reflex beside the pipeline: every creep with free carry capacity
/// standing within pickup range of a dropped energy pile asks to pick it up —
/// beside its assigned Task's action, since the engine's pickup conflicts with
/// no other action. No movement, no matching, no threshold. One target per
/// creep, the last reachable pile in Atlas order. Paired once per room and
/// never across two: a pile and a creep on the same coordinate of two rooms
/// would draw a pickup the engine answers ERR_NOT_IN_RANGE. A body already
/// carrying Thorium is not hungry (#262): the reflex asks nothing of
/// `applicable`, so free capacity alone let it scoop energy into a mixed load.
/// Nor is an empty body holding an ore-bound Task (#314): one scoop and its
/// ore Withdraw or Pickup is inapplicable, and the walk was spent for nothing.
let internal planPickups (view: ColonyView) atlas (assigned: Map<string, Task>) : Intent list =
    let oreBound name =
        match Map.tryFind name assigned with
        | Some(Withdraw(_, (Thorium | Ore _ | Compound _)))
        | Some(Pickup(_, (Thorium | Ore _ | Compound _))) -> true
        | _ -> false

    let hungry =
        view.Creeps
        |> List.filter (fun c ->
            c.FreeCapacity > 0 && not (CreepInfo.carriesMineral c) && not (oreBound c.Name))
        |> List.map (fun c -> c.Name)
        |> Set.ofList

    // A nursery's floor is the ferry's drop (#473): only a Work body, which
    // builds with it, takes it up, or the hauler takes its load back.
    let builds =
        view.Creeps
        |> List.filter (fun c -> partCount c.Body Work > 0)
        |> List.map (fun c -> c.Name)
        |> Set.ofList

    Atlas.placedCreeps atlas
    |> List.groupBy (fun (_, tile) -> tile.Room)
    |> List.collect (fun (room, placed) ->
        let placed =
            if isNurseryRoom view room then
                placed |> List.filter (fun (name, _) -> Set.contains name builds)
            else
                placed

        match Atlas.droppedEnergyIn atlas room with
        | [] -> []
        | piles ->
            placed
            |> List.collect (fun (name, pos) ->
                if Set.contains name hungry then
                    piles
                    |> List.choose (fun (pile, tile) ->
                        if RoomPos.range pos tile |> Option.exists (fun r -> r <= 1) then
                            Some(PickupPile(name, pile))
                        else
                            None)
                    |> List.tryLast
                    |> Option.toList
                else
                    []))
