/// ADR-0091
/// The Caster: which colony casts each of the world's fight asks. Every
/// colony asks for its own rooms (`Quota.castsOf`); a combat Task asked by
/// several is asked once, and each body it still wants goes to the colony
/// that lands it at least cost. Answers "who casts", never "who fights":
/// the world's match seats whichever body stands.
module Fabot.Core.Decide.Caster

open Fabot.Core
open Fabot.Core.Types

/// One colony as the Caster weighs it. Lives within the tick.
[<NoEquality; NoComparison>]
type Bidder =
    {
        Home: string
        /// Every body's view (`ColonyView.fought`): its bank, spawns, oven,
        /// stock and census, and what it prices a cast in.
        View: ColonyView
        /// What it asks for this tick (`Quota.castsOf`); none for a colony
        /// that decides nothing.
        Asks: Cast list
    }

/// What the Caster has handed out so far: each colony's bodies of a row
/// not yet read against an ask, the ticks its spawns are queued for, and its
/// share, bodies and blocks by Task and row.
type private Ledger =
    {
        Supply: Map<string * CastRow, int>
        HeldFor: Map<string * string * CastRow, int>
        Queued: Map<string, int>
        Handed: Map<string * Task * CastRow, int * int>
    }

/// The combat Task's room, for the Tasks the Caster weighs.
let private roomOf (task: Task) =
    match task with
    | Guard room
    | Defend room
    | Operate(_, room) -> Some room
    | _ -> None

/// The fight asks each colony casts for this tick, by home: every
/// colony's own squad asks (an Assault stays its declarer's), and its share
/// of the world's guard, ranger and defender asks. A Task asked by several
/// colonies is asked once, as its owner asks it (`owners`), else as the
/// first colony asking it does. Each body an ask wants is read off the
/// fleet first, so a caster flip never casts twice: a living body of the
/// row or one in an oven, whichever colony cast it — one of a colony asking
/// the Task first, else of the nearest that reaches the room; never one of
/// a caster whose cast loses where another's wins —, a body
/// holding another home's Defend (`held`) left out, as that Defend's ask
/// counts it; only the remainder is cast. A body still to cast goes to a
/// colony whose cast wins the room (`Facts.castPrice`), whether it decides
/// this tick or not (it casts when it next decides whole), else, where none
/// wins, to any — but a home's own row casts no losing fight for it —, at
/// least cost: its spawn time, the ticks its spawns are already queued for
/// (`SpawnInfo.Remaining`, and the bodies handed it this tick), and the
/// walk, a room's side a crossing (`hops`, the route's over every room's
/// ground, `World.reachHops`, else off the names; past the hop budget where
/// neither prices it); then a colony
/// asking it, then by home. A trespass's kill (`Cast.Prey`, #652) goes only
/// to a colony past its bank's floor for its own room (`CastBid.paysOwn`),
/// past both floors for another's, whose cast landed at
/// that cost pays for the body and every one before it in the ask
/// (`Facts.castBodiesPaid`), and to nobody where none does. A colony casts
/// for any ask but its own — one it asks for itself, for a room that is no other colony's home it defends —
/// only where it lends to it (`Facts.lendsAt`, the membership the verdict
/// weighs its row by, #663: a spawn, a bank at `Tuning.CasterFloor`, a
/// stock at `Tuning.CasterReserve` or the incumbent's at
/// `Tuning.CasterReserveHold`, its home's fight not needing it), within
/// `Tuning.MaxHops` of the room and projecting it or within
/// `Tuning.LaneHops`, so its body is handed the room's fight; and casts it
/// at its own cast's blocks where it did not ask it (`Facts.castOf`). A
/// raided Guard room lent to one colony (`RoomVerdict.Lent`) is cast by that
/// colony alone, at its lent row's blocks, the ask sized to that row. A
/// home's Defend (its rampart defender) is the home's alone. One colony may
/// serve several asks, one ask be split among colonies. A body nobody may
/// cast stays its first asker whose own room it is.
let assign
    (hops: string -> string -> int option)
    (held: Assignments)
    (owners: Map<Task, string>)
    (bidders: Bidder list)
    : Map<string, Cast list> =
    let tuning =
        match bidders with
        | first :: _ -> first.View.Tuning
        | [] -> Tuning.defaults

    let crossings (bidder: Bidder) (room: string) =
        hops bidder.Home room |> Option.orElse (RoomName.hopsBetween bidder.Home room)

    let walk bidder room =
        CastBid.walk tuning (crossings bidder room)

    let reaches (bidder: Bidder) (room: string) =
        Facts.castReaches
            tuning
            (crossings bidder room)
            (Map.containsKey room bidder.View.Spatial.Rooms)

    let spawnsOf (bidder: Bidder) = max 1 (List.length bidder.View.Spawns)

    let weighed (cast: Cast) =
        match cast.Ask.Row with
        | CastRow.Squad _ -> false
        | CastRow.Fighter BodyTemplate.Melee
        | CastRow.Fighter BodyTemplate.Ranged
        | CastRow.Defender -> Option.isSome (roomOf cast.For)

    // Each Task's ask, once: its owner's where the owner asks it, else the
    // first asker's; every home's Defend first, then by Task.
    let asks =
        bidders
        |> List.collect (fun bidder ->
            bidder.Asks |> List.filter weighed |> List.map (fun cast -> bidder, cast))
        |> List.groupBy (fun (_, cast) -> cast.For)
        |> List.map (fun (task, asked) ->
            let askers = asked |> List.map fst |> List.distinctBy _.Home

            let source =
                Map.tryFind task owners
                |> Option.bind (fun owner -> askers |> List.tryFind (fun b -> b.Home = owner))
                |> Option.defaultValue (List.head askers)

            task,
            askers,
            asked
            |> List.filter (fun (bidder, _) -> bidder.Home = source.Home)
            |> List.map snd)
        |> List.sortBy (fun (task, _, _) ->
            (match task with
             | Defend _ -> 0
             | _ -> 1),
            taskId task)

    // The combat Task a body holds, if any: it is sent to that fight, and
    // to no other.
    let fightOf (creep: CreepInfo) =
        Map.tryFind creep.Name held
        |> Option.filter (fun tid ->
            tid.StartsWith "guard:"
            || tid.StartsWith "defend:"
            || tid.StartsWith "assault:"
            || tid.StartsWith "op:")

    // The fleet each colony brings to a row: its living bodies of it, cast
    // wherever held (`Quota.fightersCast`), and its oven's. A body holding a
    // combat Task is supply for that Task alone (`HeldFor`): a ranger held
    // to another room's Guard is no partner for this fight, and a body
    // holding another home's Defend is counted by that Defend's ask
    // (`Brief.lentRangers`).
    let cast =
        bidders
        |> List.map (fun bidder -> bidder.Home, fightersCast bidder.View)
        |> Map.ofList

    let fleetOf (bidder: Bidder) (row: CastRow) =
        let cut = CastRow.cut row

        (cast.[bidder.Home]
         |> List.filter (fun creep -> cut creep.Name creep.Body && Option.isNone (fightOf creep))
         |> List.length)
        + (bidder.View.Casting
           |> List.filter (fun cast -> cut cast.Name (partsOf cast.Body))
           |> List.length)

    let ledger =
        {
            Supply =
                bidders
                |> List.collect (fun bidder ->
                    [
                        CastRow.Fighter BodyTemplate.Melee
                        CastRow.Fighter BodyTemplate.Ranged
                        CastRow.Defender
                    ]
                    |> List.map (fun row -> (bidder.Home, row), fleetOf bidder row))
                |> Map.ofList
            HeldFor =
                bidders
                |> List.collect (fun bidder ->
                    cast.[bidder.Home]
                    |> List.choose (fun creep ->
                        fightOf creep
                        |> Option.bind (fun tid ->
                            [
                                CastRow.Fighter BodyTemplate.Melee
                                CastRow.Fighter BodyTemplate.Ranged
                                CastRow.Defender
                            ]
                            |> List.tryFind (fun row -> CastRow.cut row creep.Name creep.Body)
                            |> Option.map (fun row -> bidder.Home, tid, row))))
                |> List.countBy id
                |> Map.ofList
            Queued =
                bidders
                |> List.map (fun bidder ->
                    bidder.Home,
                    (bidder.View.Spawns |> List.sumBy (fun spawn -> spawn.Remaining))
                    / spawnsOf bidder)
                |> Map.ofList
            Handed = Map.empty
        }

    let hand (ledger: Ledger) (bidder: Bidder) (task: Task) (row: CastRow) (blocks: int) =
        let units, most =
            Map.tryFind (bidder.Home, task, row) ledger.Handed |> Option.defaultValue (0, 0)

        { ledger with
            Handed = Map.add (bidder.Home, task, row) (units + 1, max most blocks) ledger.Handed
        }

    let weigh (ledger: Ledger) (task: Task, askers: Bidder list, source: Cast list) =
        let room = (roomOf task).Value

        let fixedTo =
            match task with
            | Defend home -> Some home
            | _ -> None

        let isAsker (bidder: Bidder) =
            askers |> List.exists (fun b -> b.Home = bidder.Home)

        let homeOfOurs = Set.contains room (Facts.homesOfOurs (List.head askers).View)

        // A home's own row casts no losing fight for it.
        let losesHome (bidder: Bidder) =
            homeOfOurs && Option.isNone fixedTo && bidder.Home = room

        (ledger, source)
        ||> List.fold (fun ledger ask ->
            let row = ask.Ask.Row

            // Its own ask a colony casts for past no floor: one it asks for,
            // its home or a room that is no other colony's home it defends
            // (`Facts.defendsOthers`); any other where it lends, as the
            // verdict weighs its row (`Facts.lendsAt`). An
            // operation's room is no colony's own, whoever asks for it: each
            // body past both floors, as for another's room.
            let own (bidder: Bidder) =
                match task with
                | Operate _ -> false
                | _ ->
                    isAsker bidder
                    && not (Facts.defendsOthers bidder.View room)
                    && Facts.contestOurs bidder.View room

            // Any ask but its own is bid for by no colony whose own home is
            // threatened (`Facts.homeThreatened`): home defence first.
            let eligible =
                match fixedTo with
                | Some home -> bidders |> List.filter (fun b -> b.Home = home)
                | None ->
                    bidders
                    |> List.filter (fun b ->
                        own b
                        || (isAsker b || reaches b room) && not (Facts.homeThreatened b.View))

            // A trespass's kill is cast into a colony's own room only past
            // its bank's floor, the body paid for (#652): it is spent out of
            // the economy it buys back.
            let casters =
                eligible
                |> List.filter (fun b ->
                    not (List.isEmpty b.View.Spawns)
                    && (Facts.lendsAt held b.View room
                        || (match ask.Prey with
                            | Some _ -> own b && CastBid.paysOwn tuning b.View.Bank.Capacity
                            | None -> Option.isSome fixedTo || own b)))

            let priced =
                casters
                |> List.map (fun b -> b.Home, lazy (Facts.castOf b.View row room))
                |> Map.ofList

            let wins (bidder: Bidder) =
                Map.tryFind bidder.Home priced
                |> Option.exists (fun cast ->
                    let won, _, _ = cast.Value
                    won)

            // The caster a raided Guard room nothing of its own answers is
            // lent to, with the row the verdict won it by
            // (`RoomVerdict.Lent`, #663), where that row is this ask's: the
            // ask was sized to that row, so that colony alone casts it, at
            // that row's blocks.
            let lent =
                match task, row with
                | Guard _, CastRow.Fighter template ->
                    (Facts.verdictOf (List.head askers).View room).Lent
                    |> Option.filter (fun (_, answer) ->
                        answer.Ranger = (template = BodyTemplate.Ranged))
                    |> Option.bind (fun (home, answer) ->
                        casters
                        |> List.tryFind (fun b -> b.Home = home)
                        |> Option.map (fun lender -> lender, template, answer))
                | _ -> None

            let lentTo (bidder: Bidder) =
                lent |> Option.filter (fun (lender, _, _) -> lender.Home = bidder.Home)

            let spawnOf (bidder: Bidder) =
                match lentTo bidder with
                | Some(_, template, answer) -> Facts.castSpawnTicks template answer.Blocks
                | None ->
                    let _, spawn, _ = priced.[bidder.Home].Value
                    spawn

            // The blocks a colony casts for it: its own ask's, else the lent
            // row's, else its own cast's (`Facts.castOf`, #663: a colony
            // casting an ask it did not make casts the body its bank buys,
            // never the asker's smaller one), else the source's.
            let blocksOf (bidder: Bidder) =
                bidder.Asks
                |> List.tryFind (fun cast -> cast.For = task && cast.Ask.Row = row)
                |> Option.map _.Ask.Blocks
                |> Option.orElse (
                    lentTo bidder |> Option.map (fun (_, _, answer) -> answer.Blocks)
                )
                |> Option.orElse (
                    Map.tryFind bidder.Home priced
                    |> Option.bind (fun cast ->
                        let _, _, blocks = cast.Value
                        blocks)
                )
                |> Option.defaultValue ask.Ask.Blocks

            // Weighed over every colony that may cast, deciding this tick
            // or not: a winner that does not decide is handed the body all
            // the same, and casts it when it next decides whole. A lent
            // room's one winner is its lender.
            let winners =
                match lent with
                | Some(lender, _, _) -> [ lender ]
                | None -> casters |> List.filter wins

            let pool =
                match casters with
                | [ one ] when not homeOfOurs -> [ one ]
                | _ ->
                    match winners with
                    | [] -> casters |> List.filter (losesHome >> not)
                    | winners -> winners

            // A caster whose cast loses where another's wins covers nothing
            // with a free body (#663): the fleet's count reads no body's
            // size, and its body is one the winning cast would not cast.
            let covers (bidder: Bidder) =
                List.isEmpty winners
                || not (Map.containsKey bidder.Home priced)
                || winners |> List.exists (fun b -> b.Home = bidder.Home)

            let left (ledger: Ledger) (bidder: Bidder) =
                Map.tryFind (bidder.Home, row) ledger.Supply |> Option.defaultValue 0

            let tid = taskId task

            let holding (ledger: Ledger) (bidder: Bidder) =
                Map.tryFind (bidder.Home, tid, row) ledger.HeldFor |> Option.defaultValue 0

            // A body free of every fight, else one cast; `unit` the body's
            // place in the ask, so the kth of a priced kill pays for k.
            let fromFleet (ledger: Ledger) (unit: int) =
                let covering =
                    match
                        eligible
                        |> List.filter (fun b -> isAsker b && covers b)
                        |> List.tryFind (fun b -> left ledger b > 0)
                    with
                    | Some own -> Some own
                    | None ->
                        eligible
                        |> List.filter (fun b -> covers b && left ledger b > 0)
                        |> List.sortBy (fun b -> walk b room, b.Home)
                        |> List.tryHead

                match covering with
                | Some bidder ->
                    let ledger =
                        { ledger with
                            Supply =
                                Map.add (bidder.Home, row) (left ledger bidder - 1) ledger.Supply
                        }

                    hand ledger bidder task row (blocksOf bidder)
                | None ->
                    let queue (bidder: Bidder) =
                        Map.tryFind bidder.Home ledger.Queued |> Option.defaultValue 0

                    let landing (bidder: Bidder) =
                        spawnOf bidder + queue bidder + walk bidder room

                    // A trespass's kill is cast only by a colony whose cast,
                    // landed where it bids, pays for this body and every one
                    // before it in the ask (#652).
                    let pays (bidder: Bidder) =
                        match ask.Prey with
                        | Some prey ->
                            Facts.castBodiesPaid
                                bidder.View
                                row
                                (blocksOf bidder)
                                room
                                (landing bidder)
                                prey
                            >= unit
                        | None -> true

                    match
                        pool
                        |> List.filter pays
                        |> List.map (fun b ->
                            ({
                                Home = b.Home
                                Cost = landing b
                                Asks = isAsker b
                            }
                            : CastBid))
                        |> CastBid.choose
                        |> Option.map (fun home -> pool |> List.find (fun b -> b.Home = home))
                    with
                    | Some bidder ->
                        let ledger =
                            { ledger with
                                Queued =
                                    Map.add
                                        bidder.Home
                                        (queue bidder + spawnOf bidder / spawnsOf bidder)
                                        ledger.Queued
                            }

                        hand ledger bidder task row (blocksOf bidder)
                    | None when Option.isSome ask.Prey -> ledger
                    | None ->
                        match askers |> List.tryFind (fun b -> own b && not (losesHome b)) with
                        | Some asker -> hand ledger asker task row (blocksOf asker)
                        | None -> ledger

            (ledger, [ 1 .. ask.Ask.Wanted ])
            ||> List.fold (fun ledger unit ->
                // A body already sent to this fight first, whoever cast it.
                match bidders |> List.tryFind (fun b -> holding ledger b > 0) with
                | Some bidder ->
                    let ledger =
                        { ledger with
                            HeldFor =
                                Map.add
                                    (bidder.Home, tid, row)
                                    (holding ledger bidder - 1)
                                    ledger.HeldFor
                        }

                    hand ledger bidder task row (blocksOf bidder)
                | None -> fromFleet ledger unit))

    let handed = (List.fold weigh ledger asks).Handed

    bidders
    |> List.map (fun bidder ->
        let shares =
            asks
            |> List.collect (fun (task, _, source) ->
                source
                |> List.choose (fun ask ->
                    Map.tryFind (bidder.Home, task, ask.Ask.Row) handed
                    |> Option.map (fun (units, blocks) ->
                        {
                            For = task
                            Ask =
                                {
                                    Row = ask.Ask.Row
                                    Wanted = units
                                    Blocks = blocks
                                }
                            Prey = ask.Prey
                        })))

        bidder.Home, shares @ (bidder.Asks |> List.filter (weighed >> not)))
    |> Map.ofList
