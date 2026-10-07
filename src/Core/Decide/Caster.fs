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
    | Defend room -> Some room
    | _ -> None

/// The fight asks each colony casts for this tick, by home: every
/// colony's own squad asks (an Assault stays its declarer's), and its share
/// of the world's guard, ranger and defender asks. A Task asked by several
/// colonies is asked once, as its owner asks it (`owners`), else as the
/// first colony asking it does. Each body an ask wants is read off the
/// fleet first, so a caster flip never casts twice: a living body of the
/// row or one in an oven, whichever colony cast it — one of a colony asking
/// the Task first, else of the nearest that reaches the room —, a body
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
/// asking it, then by home. A colony casts for any ask but its own — one it
/// asks for itself, for a room that is no other colony's home it defends —
/// only with a spawn, a bank at `Tuning.CasterFloor` and a stock at
/// `Tuning.CasterReserve`, within `Tuning.MaxHops` of the room and
/// projecting it or within `Tuning.LaneHops`, so its body is handed the
/// room's fight. A home's Defend (its rampart defender) is the home's
/// alone. One colony may serve several asks, one ask be split among
/// colonies. A body nobody may cast stays its first asker whose own room it
/// is.
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
        let projects = Map.containsKey room bidder.View.Spatial.Rooms

        match crossings bidder room with
        | Some hops -> hops <= tuning.MaxHops && (projects || hops <= tuning.LaneHops)
        | None -> projects

    let spawnsOf (bidder: Bidder) = max 1 (List.length bidder.View.Spawns)

    let weighed (cast: Cast) =
        match cast.Ask.Row with
        | CastRow.Squad _ -> false
        | CastRow.Guard
        | CastRow.Ranger
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
            tid.StartsWith "guard:" || tid.StartsWith "defend:" || tid.StartsWith "assault:")

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
                    [ CastRow.Guard; CastRow.Ranger; CastRow.Defender ]
                    |> List.map (fun row -> (bidder.Home, row), fleetOf bidder row))
                |> Map.ofList
            HeldFor =
                bidders
                |> List.collect (fun bidder ->
                    cast.[bidder.Home]
                    |> List.choose (fun creep ->
                        fightOf creep
                        |> Option.bind (fun tid ->
                            [ CastRow.Guard; CastRow.Ranger; CastRow.Defender ]
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

            // The blocks a colony casts for it: its own ask's, else the
            // source's.
            let blocksOf (bidder: Bidder) =
                bidder.Asks
                |> List.tryFind (fun cast -> cast.For = task && cast.Ask.Row = row)
                |> Option.map _.Ask.Blocks
                |> Option.defaultValue ask.Ask.Blocks

            let eligible =
                match fixedTo with
                | Some home -> bidders |> List.filter (fun b -> b.Home = home)
                | None -> bidders |> List.filter (fun b -> isAsker b || reaches b room)

            // Its own ask a colony casts for past no floor: one it asks for,
            // its home or a room that is no other colony's home it defends
            // (`Facts.defendsOthers`); any other past the floor, as the
            // verdict weighs its row (`Facts.castsForOthers`).
            let own (bidder: Bidder) =
                isAsker bidder && not (Facts.defendsOthers bidder.View room)

            let casters =
                eligible
                |> List.filter (fun b ->
                    not (List.isEmpty b.View.Spawns)
                    && (Option.isSome fixedTo || own b || Facts.castsForOthers b.View))

            let priced =
                casters
                |> List.map (fun b -> b.Home, lazy (Facts.castPrice b.View row room))
                |> Map.ofList

            // Weighed over every colony that may cast, deciding this tick
            // or not: a winner that does not decide is handed the body all
            // the same, and casts it when it next decides whole.
            let pool =
                match casters with
                | [ one ] when not homeOfOurs -> [ one ]
                | _ ->
                    match casters |> List.filter (fun b -> fst priced.[b.Home].Value) with
                    | [] -> casters |> List.filter (losesHome >> not)
                    | winners -> winners

            let left (ledger: Ledger) (bidder: Bidder) =
                Map.tryFind (bidder.Home, row) ledger.Supply |> Option.defaultValue 0

            let tid = taskId task

            let holding (ledger: Ledger) (bidder: Bidder) =
                Map.tryFind (bidder.Home, tid, row) ledger.HeldFor |> Option.defaultValue 0

            // A body free of every fight, else one cast.
            let fromFleet (ledger: Ledger) =
                let covering =
                    match
                        eligible
                        |> List.filter isAsker
                        |> List.tryFind (fun b -> left ledger b > 0)
                    with
                    | Some own -> Some own
                    | None ->
                        eligible
                        |> List.filter (fun b -> left ledger b > 0)
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

                    match
                        pool
                        |> List.map (fun b ->
                            ({
                                Home = b.Home
                                Cost = snd priced.[b.Home].Value + queue b + walk b room
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
                                        (queue bidder
                                         + snd priced.[bidder.Home].Value / spawnsOf bidder)
                                        ledger.Queued
                            }

                        hand ledger bidder task row (blocksOf bidder)
                    | None ->
                        match askers |> List.tryFind (fun b -> own b && not (losesHome b)) with
                        | Some asker -> hand ledger asker task row (blocksOf asker)
                        | None -> ledger

            (ledger, [ 1 .. ask.Ask.Wanted ])
            ||> List.fold (fun ledger _ ->
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
                | None -> fromFleet ledger))

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
                        })))

        bidder.Home, shares @ (bidder.Asks |> List.filter (weighed >> not)))
    |> Map.ofList
