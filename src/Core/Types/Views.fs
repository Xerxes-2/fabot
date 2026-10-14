/// The `ColonyView`: one colony's cut of the World — the rooms it works, the
/// bodies that are its own, its bank, and the cross-colony work it may
/// borrow. The only thing `decide` is ever handed.
[<AutoOpen>]
module Fabot.Core.Types.Views

/// The cross-colony work one colony may take this tick, named and bounded:
/// an explicit exception and never a narrowed layer.
type BorrowedWork =
    {
        /// The home rooms of the children this colony carries in its
        /// projection for a reason that is not mining them: the children it
        /// is **raising**, whose Upgrade and Build its bodies may cross for,
        /// and the children it has **lost**, whose controller is a [[claim]]
        /// to make. Both narrow to the same three kinds, because a Claim asks
        /// for exactly what an Upgrade does.
        Rooms: string list
        /// The other colonies' homes this colony projects for their
        /// **defence** this tick (`World.defends`), kin or not: transit
        /// rooms, so nothing of them is work but the Guard the planner pools
        /// there where the verdict has this colony cast it.
        Defended: string list
        /// The contested `Independent` homes of other colonies this colony
        /// keeps its resident garrison in, the Caster's choice
        /// (`World.garrisonCasters`, #479): projected for
        /// the garrison's ground alone — the controller its ring is read
        /// off and the ramparts it fights from — and no Upgrade or Build.
        Garrisoned: string list
        /// The outposts of the children it keeps residents in that border
        /// those homes (`World.ScanSet.ChildOutposts`, #517): projected as
        /// transit rooms, where a [[trespasser]]'s kill is pooled for the
        /// residents next door, as a [[chase]]'s is, and nothing else.
        ChildOutposts: string list
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BorrowedWork =
    /// Nothing borrowed: no child raised, lost, defended or garrisoned.
    let empty =
        {
            Rooms = []
            Defended = []
            Garrisoned = []
            ChildOutposts = []
        }

/// A [[nursery]]'s declared first spawn (`Colony.FirstSpawn`, #476) and the
/// name the site goes down under.
type FirstSpawnSite = { Tile: Pos; Name: string }

/// One [[assault]] this colony runs, as this tick reads its room (#490).
type AssaultFacts =
    {
        Assault: Assault
        /// What its sappers take down, by id and tile, in order: the rampart
        /// on each declared breach tile still standing, then its declared
        /// targets (`Assault.Targets`), else, for a `Strike`, the room's
        /// towers and then its spawns. None while the room is dark.
        Targets: (string * Pos) list option
        /// Whether its declared targets are all down (#496): seen so this
        /// tick, or, the room dark, at the last look (`StandDown.Taken`).
        /// Never for an assault that declares none.
        Taken: bool
        /// The room's towers' tiles, off this tick's vision: what prices a
        /// fall-back's walk out (#492). Empty while the room is dark.
        Towers: Pos list
        /// Every rampart of the rival's standing in the room, by tile, off
        /// this tick's vision: a body standing on one takes no hit, so a tile
        /// beside one is open to a melee no shot of ours reaches. Empty while
        /// the room is dark.
        Ramparts: Pos list
        /// Every construction site of another player's in the room that would
        /// restore its economy or defence and that somebody is building, by
        /// tile and kind, off this tick's vision: a body of ours stepping onto
        /// one removes it (`movement.js`), unless a rampart stands over it or
        /// safe mode runs. A road, container, rampart or wall site, or one
        /// nobody has begun, is free to lay as a decoy, and is left.
        Sites: (Pos * BuiltKind) list
        /// The room's controller, by id and tile, off this tick's vision.
        Controller: (string * Pos) option
        /// A `Declaim`'s next tap: the first tick its controller's block
        /// lets one land, while that controller is its enemy's as seen or
        /// remembered (`World.RivalControllers`). None for any other mode,
        /// and once the controller is seen anybody else's or nobody's.
        TapAt: int option
        /// Whether safe mode runs in the room: vision this tick, or the
        /// controller remembered (`World.RivalControllers`, #491).
        SafeMode: bool
        /// The window (`RivalSafeMode.barredUntil`): the first tick its owner
        /// may raise safe mode in the room again, as remembered; None while
        /// they can or nobody knows.
        BarredUntil: int option
    }

/// How the raid standing in a room is answered: one verdict a room a tick,
/// which the stand-down, the rows, the chase and the call all read.
[<RequireQualifiedAccess>]
type Answer =
    /// The bodies that could answer it win: a row's biggest cast, or the
    /// rangers standing in it or called to it (`Facts.rangersWin`). A room
    /// with no raider in it.
    | Win
    /// A ranger kites it down (`Facts.kitedDown`, #510): it cannot close.
    | Kite
    /// Nothing of ours wins it: the [[stand-down]]'s.
    | Lose

/// A room's own row in one colony (#535): whether it is the ranger's,
/// whether its biggest cast wins the room alone (`Facts.rowAnswer`), and
/// that cast's bodies of `Blocks` blocks, `Held` where it is a guard holding
/// its post (#663: the room's ask is sized to the row that wins it,
/// whichever colony's); none where no raid is priced.
type RowAnswer =
    {
        Ranger: bool
        Wins: bool
        Blocks: int
        Bodies: int
        Held: bool
    }

/// One raided room's verdict, priced once a tick over our fleet and the
/// world's vision (`Facts.judge`) and read by every colony alike.
type RoomVerdict =
    {
        Answer: Answer
        /// The row answer of each colony whose row the verdict weighed, by
        /// home (`Own`'s casters), priced once for the verdict and the
        /// colony's Brief alike.
        Rows: Map<string, RowAnswer>
        /// The colonies whose own row gives it: whose row's biggest cast wins
        /// the room alone, among the colonies whose row casts for it, or,
        /// where none does and it is no home of ours, every colony that sees
        /// it (`Facts.rowAnswer`).
        Own: Set<string>
        /// Those of `Own` whose row casts for the room, not only binds it
        /// through a fighter of theirs that takes the raid; under a `Kite`,
        /// the casters whose biggest ranger the verdict counted (#655).
        Casting: Set<string>
        /// The fewest rangers that win the raid together, of every colony
        /// that sees it and would send them, standing in the room or lent to
        /// it by their own colony's rule; None where all of them lose.
        Rangers: Lazy<int option>
        /// Those rangers by name, each with its colony's home, where they
        /// are the answer (no row wins and none kites); empty otherwise.
        Bodies: (string * string) list
        /// The colony a raided Guard room nothing of its own answers is lent
        /// to, with the row it wins it by (#663): its owner's ask is that
        /// row's template, bodies and blocks, and the Caster casts it there
        /// alone. None where the room's own rows, kite or rangers answer it.
        Lent: (string * RowAnswer) option
    }

/// The whole ticks a tile of road, plain and swamp takes one body
/// (`Gait.ticks`): exact, the engine carrying no fatigue over a fraction.
type Pace = { Road: int; Plain: int; Swamp: int }

/// One hostile class (`Arrival`): the range its weapon strikes from and its
/// pace; no pace for a body with no MOVE acting, which never arrives.
type HostileClass = { Strike: int; Pace: Pace option }

/// One class's hostile map over a room: by tile index, the ticks until one
/// of `Members` (the hostiles' ids, the room's own and those carried across
/// its borders) can strike the tile; `Int32.MaxValue` where none ever can.
type ClassArrival =
    {
        Class: HostileClass
        Members: string list
        Ticks: int[]
    }

/// A room's hostile maps: one a class, and the risk an unseen neighbour's
/// remembered danger lands on its exits, timed for the quickest unknown body
/// (`Arrival`).
type ArrivalMaps =
    {
        Classes: ClassArrival list
        Unseen: int[] option
    }

/// One class's flood over a room from the room's own hostiles that cross
/// exits, undilated: by tile index, the tick a member can stand on it. What a
/// neighbour carries over.
type ArrivalFlood =
    {
        Class: HostileClass
        Members: string list
        Arrive: int[]
    }

/// One room's arrival floods (`Arrival`), each built on first read: its own
/// crossing hostiles' (`ArrivalFlood`) and its maps, borders carried in.
type RoomArrival =
    {
        Own: Lazy<ArrivalFlood list>
        Maps: Lazy<ArrivalMaps>
    }

/// One member of one class of a room's maps, priced (`Arrival.strikersOf`):
/// its class's ticks, where it stands, how far its weapon strikes and what it
/// lands a tick on a tile.
type Striker =
    {
        Ticks: int[]
        At: RoomPos
        Strike: int
        Power: Pos -> int
    }

/// One contested room's danger this tick (`WorldDanger`), the one object
/// every colony reading the room reads: its hostile maps, their members
/// priced on first read, the tiles the maps strike within the reach horizon
/// that a body of ours may stand on or cross from, less our standing
/// ramparts there (empty where nothing is left to run from), those ramparts'
/// tiles, which every reader of the room's danger counts as unstruck, and
/// the room's walkable tiles less that Reach, laid on first read; and the
/// walking grid that is read off (by tile index, a step's weight, -1 where
/// no body of ours stands), what a kite's run-off is walked on
/// (`Arrival.runOff`).
type RoomDanger =
    {
        HostileMaps: ArrivalMaps
        Strikers: Lazy<Striker list>
        Reach: Set<Pos>
        Ramparts: Set<Pos>
        Safe: Lazy<Set<RoomPos>>
        Walking: int[]
    }

/// One room as this tick's vision answered for it, to every colony: a creep's
/// look, a structure's or an observer's.
type WorldRoom =
    {
        /// Every creep not ours standing there, allies included.
        Hostiles: HostileInfo list
        /// Each of those creeps' fatigue by id, as glanced this tick
        /// (`Arrival`: a hostile's arrival waits on it); absent is none.
        HostileFatigue: Map<string, int>
        /// What each of those creeps carries by id (`RoomFacts.HostileCargo`);
        /// absent is nothing.
        HostileCargo: Map<string, int>
        /// Each standing rampart not ours there by tile, at its hits
        /// (`RoomFacts.RivalRamparts`); absent is none.
        RivalRamparts: Map<Pos, int>
        InvaderCores: InvaderCoreInfo list
        /// The towers not ours that fire there, each by id and tile
        /// (`WorldView.hostileTowers`): every tile of the room is under them.
        Towers: (string * Pos) list
        /// The tiles of our towers there holding a shot's energy
        /// (`World.firingTowerTiles`), whichever colony's room it is.
        OurTowers: Pos list
        /// The ids of the creeps not ours whose tombstones lie there.
        Fallen: string list
        Control: RoomControlInfo
    }

/// The body patterns a fight's cast is chosen among (#654): the melee guard's
/// block and the ranger's (#411), each a `Bodies` pattern.
[<RequireQualifiedAccess>]
type BodyTemplate =
    | Melee
    | Ranged

/// The camp an [[operation]] lays on a rival home's exit (#663 S7d), in the
/// room beside the home its bodies land in crossing out: what the room's
/// fight reads of it.
type ExitCamp =
    {
        /// The rival home whose exit it camps; the camp never enters it.
        Home: string
        /// The tiles of the room a body crossing out of the home lands on,
        /// each with its weight: the arrivals seen on it, one more each so a
        /// tile no arrival was seen on yet still counts. What the camp's
        /// ground covers (`Facts.campGround`).
        Entries: (Pos * int) list
        /// The armed bodies that may come out (`Facts.exitLanders`): what a
        /// tile's depth risks (`Facts.campRisk`).
        Landers: HostileInfo list
        /// The body the camp casts, its template and its blocks: the
        /// chooser's (`Facts.exitCampPrice`); with its group there, the
        /// group's template.
        Template: BodyTemplate
        Blocks: int
        /// The tile its body stands on, priced for that body
        /// (`Facts.exitCampPrice`); None where no ground covers an entry.
        Tile: Pos option
    }

/// An [[operation]] working a room this tick (#663 S7c), as the interpreter
/// laid it for every colony (`Operations.lay`): what the room's fight reads
/// of it.
type OperationWork =
    {
        /// Its id, which its Task carries (`Operate`, `op:<id>`).
        Id: string
        /// Whose bodies it is about: its quarry in its room.
        Enemy: string
        /// The room its record is committed to (`OpRecord.Room`).
        Room: string
        /// Its group, every colony's body of it, with where it stands
        /// (`Operations.groupIn`): what its seats and its muster count.
        Group: (CreepInfo * RoomPos) list
        /// The rate its group earns in its room a tick (`Operations.ownFacts`):
        /// what the operation ledger reads as its price.
        Rate: float
        /// The most bodies it seats (`OpLimits.MaxBodies`): its asks' cap.
        MaxBodies: int
        /// The exit it camps (#663 S7d); None for the [[sweep]], which
        /// works its room whole.
        Camp: ExitCamp option
    }

/// The world layer's read of this tick, built once off the `World` and handed
/// to every colony.
type WorldView =
    {
        /// Every room vision answered for this tick, by name.
        Rooms: Map<string, WorldRoom>
        /// Every room some colony declares, raises or garrisons, and every
        /// harassment room, shut or not: a stand-down withholds a colony's
        /// economy from a room and never takes it out of here.
        Watched: Set<string>
        /// The watched rooms some colony's raid log holds a raid on or
        /// remembers an armed threat in (`StandDown.RaidedOutposts`,
        /// `ChasedOutposts`, `ThreatenedOutposts`): the rooms that want eyes.
        Danger: Set<string>
        /// The watched rooms shut on another player's reservation
        /// whose holder no look has read (`StandDown.RivalReserved`, #604):
        /// eyes ask whose the hold is, and an enemy's ends the row. The observer's, beside `Danger`; no walk
        /// reads it.
        Questioned: Set<string>
        /// The rooms of ours an observer stands in: what a dark room's
        /// remembered body waits on a look from (`Facts.odds`).
        Observers: string list
        /// Each observer's room and the one room it looks at this tick, aimed
        /// once over every colony (`Layout.aimObservers`). None until the
        /// views are cut, and in every fixture of one colony, whose observer
        /// is aimed over its own view alone.
        Looks: Map<string, string> option
        /// Every body of ours by the room it stands in, and its tile there,
        /// whoever holds it. Read on first use: by a home's Defend, and by the
        /// world's verdicts as the fleet (`Facts.judge`).
        Bodies: Lazy<Map<string, (CreepInfo * Pos) list>>
        /// Every body of ours whose name a living colony's spawn is in, by
        /// name, with that colony's home (`Colony.castBy`), whoever holds it:
        /// the census by caster (`Quota.fightersCast`). Read on first use;
        /// empty in a fixture with no world, whose bodies count at their
        /// holder.
        CastBy: Lazy<Map<string, string * CreepInfo>>
        /// Each room a raider stands in, its one verdict, priced on first
        /// read over the fleet and this vision (`Facts.judge`). Empty until the
        /// views are cut, and in every fixture of one colony, which prices
        /// a room over its own view alone.
        Answers: Map<string, Lazy<RoomVerdict>>
        /// Every room's facts the world holds this tick (`World.Rooms`),
        /// whole, whatever any colony's cut keeps of them: the ground each
        /// contested room's danger is read over (`WorldDanger`). Empty in a
        /// fixture with no world, whose danger reads its views' projections.
        Facts: Map<string, RoomFacts>
        /// Each contested room's danger (`WorldDanger.lay`), built on first
        /// read, once for every colony. None until the views are cut, and in
        /// every fixture that decides one view alone, which reads the danger
        /// off its own view (`WorldDanger.read`).
        Dangers: Map<string, Lazy<RoomDanger option>> option
        /// Each watched room whose raid nothing sees this tick and the world
        /// recalls (`World.Raiders`, #569), against the tick it was last seen:
        /// how old the economy gate's memory of it is (`Facts.openToEconomy`,
        /// #605).
        RaidSeen: Map<string, int>
        /// Every foreign body the world remembers, by id (`World.Quarries`,
        /// #663): what a body out of sight is priced by (`Quarry.odds`), and
        /// whether one a look finds gone may only have stepped across
        /// (`Quarry.step`, `Observe.strikeGone`, #585). Empty in a fixture.
        Quarries: Map<string, Track>
        /// The rooms whose raid the world remembers, which keep their
        /// healers themselves (`Quarry.raidHeld`): a healer remembered there
        /// is the raid's, never a body of the room.
        RaidHeld: Set<string>
        /// Each rival room's defence response as its dossier saw it (#653):
        /// the fewest ticks from our first armed body in the room to its
        /// owner's first armed body there; `Int32.MaxValue` where it was
        /// visited and never answered. What the [[sweep]] prices its muster
        /// by (`RoomWorth.Response`, #658); a room absent has no reading.
        /// Laid by the shell off last tick's dossier; empty in a fixture.
        Responses: Map<string, int>
        /// Each rival's share of its workers that walked off a room it holds
        /// while our armed body stood there (#663, `Observe.Rivals.flees`):
        /// the odds a body of its stops working while we hold a room, which
        /// its take denied is priced by (`Facts.roomRate`). A rival absent
        /// has no reading. Laid by the shell off last tick's dossier; empty
        /// in a fixture.
        Flees: Map<string, float>
        /// Each room's worth this tick (`RoomWorth`, #663), built on first
        /// read off the colony whose economy it is, once for every colony
        /// (`Facts.layWorths`). None until the operations are laid
        /// (`Operations.lay`), and in a fixture that prices a room off its
        /// own view (`Facts.roomWorth`).
        Worths: Map<string, Lazy<RoomWorth>> option
        /// Each [[operation]] working a room this tick (`Operations.lay`):
        /// its room, its group and its price, the one answer every colony's
        /// fight reads. None until the operations are laid: a candidate is
        /// then read as worked (`Facts.operationRooms`), as pricing it for
        /// the operation asks, and in every fixture of one view that lays
        /// none.
        Operations: OperationWork list option
        /// Each room an [[operation]] is committed to, against the tick it
        /// committed (its record's room and phase entry, #663 S7c): where a
        /// body seen rooted at its work there since is priced on that tile
        /// while it stays in the room (`Facts.sweepPricing`). Empty with no
        /// operation working a room.
        Committed: Map<string, int>
        /// Each watched exit of a rival home (`World.Traffic`, #663 S7d), by
        /// `ExitTraffic.key`: the arrivals seen on its landing tiles and the
        /// full ticks it was watched, what an exit camp's rate is read off.
        Traffic: Map<string, ExitLog>
        /// Each room an [[operation]] asks eyes on (#663 S7d), at the energy
        /// the decision a look there unlocks is worth (`Operations.lay`):
        /// an exit camp's rooms, its value over a look's stride, and the
        /// sweep's other rooms, the latch move a look may enable
        /// (`Operations.latchLooks`), summed where both ask.
        OperationLooks: Map<string, float>
        /// Each room a guard or ranger of ours was last lost in, the latest of
        /// every colony's raid log (`StandDown.FighterLost`), the larger cast
        /// on one tick: a room's recast cooldown is the room's, whichever
        /// colony would cast.
        FighterLost: Map<string, FighterLoss>
        /// Each body of ours' kill head as its last decide seated it
        /// (`World.Heads`).
        Heads: Map<string, HeadLatch>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module WorldView =
    /// A world nothing was seen in.
    let empty =
        {
            Rooms = Map.empty
            Watched = Set.empty
            Danger = Set.empty
            RaidSeen = Map.empty
            Quarries = Map.empty
            RaidHeld = Set.empty
            Responses = Map.empty
            Flees = Map.empty
            Worths = None
            Operations = None
            Committed = Map.empty
            Traffic = Map.empty
            OperationLooks = Map.empty
            FighterLost = Map.empty
            Heads = Map.empty
            Questioned = Set.empty
            Observers = []
            Looks = None
            Bodies = lazy Map.empty
            CastBy = lazy Map.empty
            Answers = Map.empty
            Facts = Map.empty
            Dangers = None
        }

    /// Whether vision answers for a room this tick, whoever's.
    let sees (watch: WorldView) (room: string) : bool = Map.containsKey room watch.Rooms

    /// The towers not ours that fire in a room (#566), by id and tile: an
    /// Invader's beside a [[stronghold]]'s core, or the tower of the player
    /// owning the room, never an ally's. Read off the census of every owner's
    /// structures (`TargetKinds`, `RoomLayer.Foreign`); a tower left in a room
    /// its owner lost fires at nothing. Its energy is not carried, so it is
    /// read as loaded, the direction a body runs in.
    let hostileTowers (facts: RoomFacts) : (string * Pos) list =
        let stronghold = facts.InvaderCores |> List.exists InvaderCoreInfo.isStronghold
        let owner = facts.RivalController |> Option.map (fun rival -> rival.Owner)

        if Map.isEmpty facts.Layer.Foreign || not stronghold && Option.isNone owner then
            []
        else
            facts.TargetKinds
            |> Map.toList
            |> List.choose (fun (id, kind) ->
                match kind with
                | Structure BuiltKind.Tower ->
                    Map.tryFind id facts.Layer.TargetPositions
                    |> Option.filter (fun tile ->
                        match Map.tryFind tile facts.Layer.Foreign with
                        | Some by ->
                            not (Colony.isAlly by)
                            && (by = "Invader" && stronghold || Some by = owner)
                        | None -> false)
                    |> Option.map (fun tile -> id, tile)
                | _ -> None)

    /// The ticks a raid that walked off unseen may still stand in, or come
    /// back to, the room it was last seen in (#585): its slowest body's round
    /// trip empty (`HostileInfo.roundTrip`: a raid carries nothing), over which the odds it is still there fall
    /// to nothing (`Quarry.odds`) — a body that cannot walk for the life it
    /// has left. A room's round trip where nothing is known of the raid.
    let raidTrip (raid: HostileInfo list) : int =
        match raid with
        | [] -> 2 * Engine.roomSide
        | _ ->
            raid
            |> List.map (fun hostile ->
                HostileInfo.roundTrip raid 0 hostile
                |> Option.defaultValue (max 1 hostile.TicksToLive))
            |> List.max

    /// Whether a raid body recalled through blind ticks, last seen on `seen`,
    /// still shuts its room to the economy (#605): an Invader for its life,
    /// which never leaves the room; a player's body for `span` ticks, the
    /// hold its stand-down is priced at (`Facts.raidHoldSpan`, #585) — a look
    /// finding it gone ends the recall before. Only a body we strike
    /// wherever it stands (`Colony.strikesAnywhere`) shuts a room at all.
    let recallShuts (span: int) (time: int) (seen: int) (hostile: HostileInfo) =
        Colony.strikesAnywhere hostile.Owner
        && (not (HostileInfo.crossesExits hostile) || time - seen < span)

    /// The world's read off this tick's `World` and every colony's
    /// stand-down gate. A room with a `Control` entry is one something of ours
    /// looked into (`World.ofGame`). No danger yet: it is laid once the views
    /// are cut (`WorldDanger.lay`).
    let ofWorld (colonies: Colony list) (gates: StandDown list) (world: World) : WorldView =
        let watched =
            (colonies
             |> List.collect (fun colony ->
                 colony.Home :: (colony.Outposts |> List.map (fun outpost -> outpost.RoomName))
                 @ (colony.Errands |> List.map (fun errand -> errand.RoomName))
                 @ colony.Salvage
                 @ (colony.Assaults |> List.map (fun assault -> assault.RoomName))))
            // The declared harassment rooms stay watched while harassment is
            // paused (#615 U4 E): the sweep reads their raids.
            @ (Colony.harassDeclared |> List.map (fun h -> h.RoomName))
            |> Set.ofList

        let raidSeen =
            world.Raiders
            |> Map.filter (fun room _ -> watched.Contains room && not (World.seenNow world room))
            |> Map.map (fun _ last -> last.Tick)

        {
            Rooms =
                world.Rooms
                |> Map.toList
                |> List.choose (fun (room, facts) ->
                    facts.Control
                    |> Option.map (fun control ->
                        room,
                        {
                            Hostiles = facts.Hostiles
                            HostileFatigue = facts.HostileFatigue
                            HostileCargo = facts.HostileCargo
                            RivalRamparts = facts.RivalRamparts
                            InvaderCores = facts.InvaderCores
                            Towers = hostileTowers facts
                            OurTowers =
                                World.firingTowerTiles Tuning.defaults.TowerRepairReserve facts
                            Fallen = facts.Fallen
                            Control = control
                        }))
                |> Map.ofList
            Watched = watched
            // And each watched room a recalled raid may still stand in
            // (`recallShuts` over its round trip, #605, #585): nothing else of
            // ours walks in to look.
            Danger =
                gates
                |> List.map (fun gate ->
                    Set.unionMany
                        [ gate.RaidedOutposts; gate.ChasedOutposts; gate.ThreatenedOutposts ])
                |> Set.unionMany
                |> Set.union (
                    raidSeen
                    |> Map.filter (fun room seen ->
                        let raid = world.Raiders.[room].Raid
                        raid |> List.exists (recallShuts (raidTrip raid) world.Time seen))
                    |> Map.keys
                    |> Set.ofSeq
                )
                |> Set.intersect watched
            RaidSeen = raidSeen
            Quarries = world.Quarries
            RaidHeld = Quarry.raidHeld world
            Responses = Map.empty
            Flees = Map.empty
            Worths = None
            Operations = None
            Committed = Map.empty
            Traffic = world.Traffic
            OperationLooks = Map.empty
            Heads = world.Heads
            FighterLost =
                (Map.empty, gates)
                ||> List.fold (fun lost gate ->
                    (lost, gate.FighterLost)
                    ||> Map.fold (fun lost room loss ->
                        match Map.tryFind room lost with
                        | Some later when (later.Tick, later.Cast) >= (loss.Tick, loss.Cast) ->
                            lost
                        | _ -> Map.add room loss lost))
            Questioned =
                gates
                |> List.map (fun gate -> gate.RivalReserved)
                |> Set.unionMany
                |> Set.intersect watched
            Observers =
                world.Rooms
                |> Map.toList
                |> List.filter (fun (_, facts) ->
                    Option.isSome facts.Controller
                    && facts.TargetKinds
                       |> Map.exists (fun _ kind -> kind = Structure BuiltKind.Observer))
                |> List.map fst
            Looks = None
            Bodies =
                lazy
                    (world.Creeps
                     |> List.choose (fun creep ->
                         Map.tryFind creep.Room world.Rooms
                         |> Option.bind (fun facts ->
                             Map.tryFind creep.Info.Name facts.Layer.CreepPositions)
                         |> Option.map (fun tile -> creep.Room, (creep.Info, tile)))
                     |> List.groupBy fst
                     |> List.map (fun (room, placed) -> room, List.map snd placed)
                     |> Map.ofList)
            CastBy =
                lazy
                    (let spawns = World.spawnHomes world
                     let homes = colonies |> List.map (fun colony -> colony.Home) |> Set.ofList

                     world.Creeps
                     |> List.choose (fun creep ->
                         Colony.castBy spawns creep.Info.Name
                         |> Option.filter homes.Contains
                         |> Option.map (fun home -> creep.Info.Name, (home, creep.Info)))
                     |> Map.ofList)
            Answers = Map.empty
            Facts = world.Rooms
            Dangers = None
        }

/// The [[lab cluster]] as one colony reads it, whichever colony's home it
/// stands in: the declaration, and what that home holds, read off the world —
/// the cluster's home is one of ours and always in sight.
type LabFacts =
    {
        Cluster: LabCluster
        /// Mineral -> what the cluster home's terminals and labs hold of it
        /// together, a mineral held nowhere missing.
        Stock: Map<Resource, int>
    }

/// One colony's whole reading of this tick: its home room's projection, the
/// rooms it works beside it, the bodies it holds, the bank it casts from and
/// the explicit little it may take of its neighbours'. ADR-0052
type ColonyView =
    {
        Time: int
        /// This colony's spawns: the ones it casts from and anchors its
        /// Layout on.
        Spawns: SpawnInfo list
        /// The named bodies this colony has in its ovens this tick: its **home
        /// room's** `RoomFacts.Casting` alone, for the reason `Bank` is one
        /// account. Read by the casting cascade and by nothing else.
        Casting: CastingInfo list
        /// The **tunables** this colony decides under, so a rule reads its
        /// colony's own and a test moves one field instead of editing the
        /// rule.
        Tuning: Tuning
        /// The colony's bank: its **home room's** shared spawn-energy account,
        /// and no other room's — not a fold over the projected rooms, which
        /// would have read a child's 300 beside a mother's 1,800.
        Bank: RoomEnergy
        /// Energy-hungry structures in the home room (spawn, extension,
        /// tower), whether or not they currently have room.
        Refillables: RefillableInfo list
        /// The sources this colony **mines**: every room it works but the ones
        /// it merely [[bootstrap]]s, with every declared outpost rock beside
        /// them whether or not there is vision (`Outpost.pooledSources`).
        Sources: SourceInfo list
        /// The sources of the children this colony bootstraps past their
        /// nursery: dug by its pioneers standing there, and out of every
        /// quota, which `Sources` feeds and the child's own rows count (#192).
        /// And an errand room's, dug on the same terms by the bodies building
        /// or repairing its [[ore]] mine.
        BorrowedSources: SourceInfo list
        /// Every other colony home with a terminal whose Storage holds under
        /// its own [[emergency reserve]], the furthest short of it first: where
        /// this colony's terminal sends while its own Storage can spare it.
        AidRooms: string list
        /// `StandDown.ReserveShort`: whether the last decided tick read this
        /// colony's stock under its [[emergency reserve]].
        ReserveShort: bool
        /// `StandDown.DigOpen`: the [[ore]]s whose dig the last decided tick
        /// left open.
        DigOpen: Set<Ore> option
        /// `StandDown.HaulOverflow`: the haul overflow reading the last
        /// decided tick left (#508).
        HaulOverflow: HaulOverflow
        /// Every room any colony of ours declares as an outpost: a harassment
        /// room among them keeps its source's seats for our miners, never
        /// the ranger's (W17S26, 2026-10-04).
        MinedRooms: Set<string>
        /// This colony's own controller. Never a child's, which reaches the
        /// pool as a target in a layer she projects.
        Controller: ControllerInfo option
        /// Who holds each room this colony works and has vision in this tick,
        /// under that room's name — what a source's output is priced from.
        /// One entry can be a room the colony does **not** work: the
        /// [[stand-down]] gate re-admits a latched room to the scan for one
        /// tick every `Tuning.RivalRecheck` (#165, #275), and this is the
        /// whole of what such a look reads — no layer, no rock, no Task — so
        /// the look moves no decision and only the next [[raid log]] is any
        /// wiser for it.
        RoomControl: Map<string, RoomControlInfo>
        /// The rooms this colony last saw **somebody else's reservation** on,
        /// whose hold has not run out yet (`StandDown.HeldOutposts`, #333):
        /// what the last look concluded, where `RoomControl` is this tick's
        /// vision. The one place a view carries a conclusion, because the
        /// refusal's own effect is to withdraw the body whose vision read it,
        /// so a rule reading vision alone would hire the body back the tick
        /// after it died, for ever. The engine counts a reservation down at
        /// one a tick, so the record dates itself. **Vision overrules it**: a
        /// room with a `RoomControl` entry is decided by that entry.
        HeldOutposts: Set<string>
        /// The declared [[outpost]]s this colony last saw an **armed**
        /// [[threat]] standing in, whose memory has not run out
        /// (`StandDown.ThreatenedOutposts`, #366). `Hostiles`' memory as
        /// `HeldOutposts` is `RoomControl`'s. What a reader may conclude: that
        /// something with an ATTACK or a RANGED_ATTACK part stood there on the
        /// last tick anything of ours could see the room, and that fewer than
        /// `Tuning.ThreatMemory` ticks have passed — enough to hire **one**
        /// guard and pool its Guard. What it may not: what it is made of or
        /// how many there are (`Facts.blocksBeat` reads bodies, and there
        /// are none here). Not a withdrawal: the room is worked, which tells
        /// it apart from `StandDown.Shut`. **Vision overrules it**
        /// (`Planner.guardedOutposts` consults it only where there is none).
        ThreatenedOutposts: Set<string>
        /// The rooms this colony stands down from on a raid
        /// (`StandDown.RaidedOutposts`, #514): what lays the home's shelter
        /// while a raid beside it is out of sight.
        RaidedOutposts: Set<string>
        /// Each of those rooms with the tick its row runs to
        /// (`StandDown.RaidedUntil`): what the observer prices its look by.
        RaidedUntil: Map<string, int>
        /// The shut outposts a ranger chases a raid in
        /// (`StandDown.ChasedOutposts`, #510).
        ChasedOutposts: Set<string>
        /// Each resident room's [[fight]] record (`StandDown.Fought`).
        Fought: Map<string, FightRecord>
        /// The last tick a raider stood in the home (`StandDown.HomeRaided`).
        HomeRaided: int option
        /// The home's Keep and rampart hits the tick before
        /// (`StandDown.KeepHitsBefore`).
        KeepHitsBefore: Map<string, int>
        /// Our construction sites in every room this colony works and has
        /// vision in: the Build pool is this list one to one.
        ConstructionSites: ConstructionSiteInfo list
        /// Every construction site the player holds, every colony's and in
        /// every room (`World.Sites`): what the engine's cap counts.
        PlayerSites: int
        /// The economy bodies this colony holds this tick
        /// (`World.creepColonies`), in the world's own order, so who holds a
        /// body does not move the Matcher's order. No fighter: the world's
        /// fight commands those (`Fighters`). A view whose `Fighters` is
        /// empty may hold its fighters here too — a fixture's, or the fight's
        /// own (`ColonyView.fought`).
        Creeps: CreepInfo list
        /// The fighters (`isFighterBody`) this colony holds this tick, in the
        /// world's own order: the bodies the world's fight prices in this
        /// colony's Atlas (`ColonyView.bodies`). Its fighting rows count the
        /// fighters it cast, wherever held (`Quota.fightersCast`).
        Fighters: CreepInfo list
        /// The names of its economy bodies standing in a room this colony
        /// does not project, and so in no layer: the bodies the projection does not
        /// place because the shell does not, told apart from a fixture that
        /// simply placed nobody (#506).
        Unprojected: Set<string>
        /// Hostile creeps standing in any room this colony works and has
        /// vision in, each under its own room's name.
        Hostiles: HostileInfo list
        /// Our allies' creeps (`Colony.allies`) standing where `Hostiles` would
        /// have filed them: what a rampart of ours opens to (#482).
        Allies: HostileInfo list
        /// The invader cores standing in the rooms this colony works and can
        /// see. Its own list and not a widening of `Hostiles`: a raider is
        /// something a creep runs from this tick, a core is something a whole
        /// room is withheld from for thousands, and `FIND_HOSTILE_CREEPS` can
        /// never answer with a structure.
        InvaderCores: InvaderCoreInfo list
        /// The ids of the creeps not ours whose tombstones lie in the rooms
        /// this colony works and can see: a raid body seen dead (#507).
        Fallen: string list
        /// This colony's spatial projection: the home room and every room it
        /// works beside it. `RoomName` is the home room and the `Rooms` keys
        /// are the scan set, so neither is stored a second time beside it.
        Spatial: SpatialInfo
        /// Every home room a human has declared a colony for (`Colony.homes`),
        /// in declaration order. The **candidate colonies** are the ones nobody
        /// owns yet, read off `RoomControl` in Core: a view carries facts
        /// rather than conclusions.
        Declared: string list
        /// The [[errand]]s this colony runs this tick, **refused ones removed**
        /// (`Errand.routable`), so a rule that reads it can neither pool a
        /// Task nor hire a body for a room no chain of [[seam]]s reaches. Two
        /// rules read it — the [[reclaim]] Task's pool and the re-claimer's
        /// seat on the reserver row. A held one is not here: the hold pauses
        /// the Reactor's work.
        Errands: Errand list
        /// The rooms past home whose [[ore]] this colony digs into its home
        /// terminal: every errand room the scan works, held or not
        /// (`ColonyView.oreRooms`).
        OreRooms: Set<string>
        /// The structures standing in this colony's [[salvage]] rooms that its
        /// dismantler takes down (`Salvage.isTarget`), by id: placed in the
        /// projection and classified by nothing (`salvaging`), so a rule names
        /// them from here. Empty once the last one falls.
        Dismantles: string list
        /// The [[harassment room]]s this colony casts this tick
        /// (`World.harassCasters`), less those its [[stand-down]] shuts: the
        /// [[sweep]]'s candidates it works, of which the operation commits
        /// to one (`WorldView.Operations`, `Facts.operationRooms`).
        Harass: Harass list
        /// Every harassment room this colony casts, the shut ones included:
        /// the rooms the [[raid log]] reads as ours to attack, never as a
        /// raid on us.
        HarassCast: Set<string>
        /// The [[assault]]s this colony runs this tick (`Assault.worked`):
        /// what the Assault Tasks, their ground and the squad rows read.
        Assaults: AssaultFacts list
        /// The home room of the colony this one **ships its banked Thorium to**
        /// (`Colony.Consignee`, #349), a declaration and not a sighting: the
        /// far end is outside every scan set this colony holds. A rule may
        /// read a **room name to send to** and never a fact about that room;
        /// the `send` is issued into that blindness on purpose, and a refusal
        /// costs the tick's call and nothing else.
        Consignee: string option
        /// The [[lab cluster]] (`Colony.labCluster`) and what its home holds:
        /// the cluster's own colony reacts, every other ships it the ore it
        /// lacks. None while no declared colony's home is the cluster's.
        Labs: LabFacts option
        /// The home room's declared perimeter (`Colony.Perimeter`, #446): the
        /// chokes the Layout ramparts beside the Keep.
        Perimeter: Pos list
        /// The hits a rampart on each of these home tiles is whole at, in
        /// place of `Tuning.RampartFloor` (`Colony.RampartFloors`).
        RampartFloors: Map<Pos, int>
        /// The armed bodies of ours (an ATTACK or a RANGED_ATTACK part)
        /// standing in each room whose safe mode this colony fires — its home,
        /// and each [[nursery]] it raises (#449) — under that room's name,
        /// **whichever colony holds them** (#448): a mother's resident in her
        /// child's home is in `Creeps` of neither the child nor the reflex
        /// that weighs it, and is the home's defence all the same.
        Defenders: Map<string, CreepInfo list>
        /// The controller of each [[nursery]] this colony raises, under its
        /// room's name (#449): the level its Upgrade waits on, and the stock
        /// its mother fires. A Nursery runs no tick of its own to read it.
        NurseryControllers: Map<string, ControllerInfo>
        /// The declared first spawn of each [[nursery]] this colony raises,
        /// under its room's name (#476), named past every spawn the world
        /// holds (`ColonyView.firstSpawns`). No entry for a nursery nobody
        /// swept a tile for.
        FirstSpawns: Map<string, FirstSpawnSite>
        /// The raid healers seen in the home room within the last
        /// `Tuning.HealMemoryTicks` ticks and not standing there now, at the
        /// tile last seen (`World.Healers`, #480): one that stepped out across
        /// the exit is back next tick. And in each child's home this colony
        /// raises (#552), whose line its garrison holds.
        RecalledHealers: HostileInfo list
        /// The raid last seen in each room this colony works, or chases a
        /// raid in, that nothing sees this tick (`World.Raiders`, #569), at
        /// its last tiles, its life counted down since, and each body out of
        /// sight the world remembers in an outpost of its own or a [[sweep]]
        /// candidate, dark or seen (`Quarry.ghosts`, #618): laid into
        /// `Hostiles` too, so a raider stays the room's raid and a trespasser
        /// its kill — its Guard, verdict and targets — through the ticks
        /// between two looks. What the [[raid
        /// log]] folds is a sighting, so it reads `Hostiles` less these
        /// (`sightedOnly`).
        RecalledRaiders: HostileInfo list
        /// Whether safe mode is running in any room of ours this tick
        /// (`World.safeModeRunning`), the same answer handed to every colony:
        /// the engine runs one per shard and refuses a second with ERR_BUSY.
        SafeModeRunning: bool
        /// The rooms in this colony's scan set that it merely **crosses**
        /// (`transiting`). Carried because decaying ore in such a room is
        /// this colony's to sweep (#360), and ore is the only thing
        /// `transiting` lets through. **Not** "rooms we may act in": a rival's
        /// room can sit on a chain.
        Crossed: Set<string>
        /// The [[evacuating room]]s (`World.ScanSet.Evacuating`, #512): rooms
        /// of `Crossed` every idle body of this colony standing in walks home
        /// from, and which pool nothing, not even ore to sweep.
        Evacuating: Set<string>
        /// The declared sector Reactors this colony can see (#354), narrowed
        /// as the errand's other facts are. The one fact about a Reactor a
        /// *decision* reads: its store is deliberately absent from
        /// `SpatialInfo.Thorium`, and the first draw gate read it there anyway
        /// — the projection answered 0 for a store holding 999, the gate never
        /// closed, and ore reached a full Reactor's floor. The fixture agreed
        /// because it wrote the store where the gate looked, a shape `World`
        /// has never produced.
        Reactors: ReactorInfo list
        /// The [[stage]] of every room that is a colony of ours this tick
        /// (`World.stages`), the same map handed to every colony.
        Stages: Map<string, ColonyStage>
        /// Where every creep this colony does not hold stands in the rooms it
        /// works — other colonies' of ours, hostiles' and allies': bodies it
        /// cannot move. A hostile or ally that stepped since the full tick
        /// before stands on the tile it left as well (`World.HostileSteps`).
        Foreign: Set<RoomPos>
        /// The hostiles in sight standing on the tile they stood on the full
        /// tick before (`HostileStep.Since`, #552): parked, not walking in.
        Parked: Set<string>
        /// The hostiles in sight that stepped since the full tick before, each
        /// with the tile of its room it left (`HostileStep.Left`): which way
        /// it walks.
        Stepped: Map<string, RoomPos>
        /// Of those, each with the tile it left on the step before
        /// (`HostileStep.Back`): whether it walks on or mills in place.
        SteppedBack: Map<string, RoomPos>
        /// The hostiles that crossed a border within the bounce window, each
        /// with its crossings (`HostileStep.Crossed`, #658): the bodies
        /// dancing on a border, by their trajectory.
        Bounces: Map<string, Crossing>
        /// The tiles, world-wide, of our bodies that leave them this tick
        /// (`Entry.leavingTiles`, #649): on the border ring, or rested and on
        /// the move (`CreepInfo.Walking`) or shoved for free by the
        /// arbitration (#600).
        /// Empty off the shell and in every hand-built fixture, which price
        /// every body as occupied; the decide settles it before the Atlas is
        /// built. The one reader is the Atlas's occupancy grid.
        Leaving: Set<RoomPos>
        /// The engine's refusals of this colony's acts (`World.Refusals`,
        /// #629): its bodies', wherever they stand, and the structures' and
        /// sites' whose acts its plan issued, wherever they stand. What the
        /// Matcher prices a refused (body, Task) pair by, what the spawn
        /// plan orders its spawns by, and what the breach log counts.
        Refusals: Refusal list
        /// What this colony may take of a neighbour's, explicitly and
        /// bounded.
        Borrowed: BorrowedWork
        /// The declarations this colony's constant names that no chain of
        /// [[seam]]s joins to its home (`Outpost.refused`, `Errand.refused`,
        /// `Salvage.refused`, #243), and the harassment rooms no colony casts,
        /// said by the largest bank alone: out of the scan set rather than in it
        /// unworkable. Carried because the refusal has to be *said* on the
        /// [[layout record]]; empty is the healthy answer and rides here all
        /// the same. Each entry carries the **kind**, which
        /// `RefusedDeclaration` sizes once.
        Refused: RefusedDeclaration list
        /// What each room this colony **works** was last seen to carry (#151):
        /// the world's sightings, narrowed to the scan set the [[stand-down]]
        /// has already cut, so the withdrawal spelled through `task-gone`
        /// keeps working. Narrowed once more (#271): a room this colony only
        /// **crosses** carries no memory either, or a memory of work the
        /// projection has just refused would reach the grace anyway — which
        /// is how a stand-down on a room a chain runs *through* used to hold
        /// a hauler to a Withdraw it had already withdrawn from.
        Sightings: Map<string, RoomSighting>
        /// Every sighting the world holds, `Sightings` before the narrowing:
        /// which rooms nobody of ours sees, the observer's aim (#484). A room
        /// seen this tick carries `Time`.
        Seen: Map<string, RoomSighting>
        /// The exit each room this colony works is held at (#450): the run its
        /// last armed Threat left by, while the hold stands
        /// (`World.watchExits`). What `Threat.threatsOf` lays the Guard's
        /// ground on while the room is empty of them.
        ExitHolds: Map<string, ExitHold>
        /// The world layer's read of this tick, the same one handed to every
        /// colony: what the [[raid log]] reads a room it stands down from off
        /// (`lookingInto`).
        World: WorldView
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ColonyView =
    /// The rooms past home whose [[ore]] this colony digs into its home
    /// terminal: its errand rooms, held or not, less a stood-down one.
    let oreRooms (view: ColonyView) : Set<string> = view.OreRooms

    /// Whether vision answered for a room before and not this tick (#666):
    /// the world's last sighting of it is older than the tick. A room never
    /// seen is not dark by this reading: it projects the same every tick.
    let dark (view: ColonyView) (room: string) : bool =
        match Map.tryFind room view.Seen with
        | Some sighting -> sighting.Tick < view.Time
        | None -> false

    /// Every body this colony holds, its economy's and the fighters the
    /// world's fight prices in its Atlas: what a fight rule reads of ours.
    let bodies (view: ColonyView) : CreepInfo list =
        match view.Fighters with
        | [] -> view.Creeps
        | fighters -> view.Creeps @ fighters

    /// The home an idle fighter this colony holds walks to: the colony's
    /// that cast it (`WorldView.CastBy`), which this colony projects the walk
    /// to, else its own. A fighter's caster governs its walk home, its holder
    /// only its tick.
    let idleHome (view: ColonyView) (creep: string) : string =
        match Map.tryFind creep view.World.CastBy.Value with
        | Some(caster, _) when Map.containsKey caster view.Spatial.Rooms -> caster
        | _ -> SpatialInfo.homeName view.Spatial

    /// The view the world's fight reads for this colony: every body it holds
    /// in `Creeps`, as the Emitter, the reflexes and the mover walk them.
    let fought (view: ColonyView) : ColonyView =
        match view.Fighters with
        | [] -> view
        | _ ->
            { view with
                Creeps = bodies view
                Fighters = []
            }

    /// The view with the world's vision of these rooms laid in, for each one
    /// it does not project (#534): their hostiles, allies, cores, tombstones
    /// and control, as the cut would have filed them. What the [[raid log]]
    /// folds a room it stands down from off: vision is vision, whoever's.
    let lookingInto (rooms: string list) (view: ColonyView) : ColonyView =
        let seen =
            rooms
            |> List.distinct
            |> List.filter (fun room -> not (Map.containsKey room view.Spatial.Rooms))
            |> List.choose (fun room ->
                Map.tryFind room view.World.Rooms |> Option.map (fun r -> room, r))

        match seen with
        | [] -> view
        | seen ->
            let bodies = seen |> List.collect (fun (_, r) -> r.Hostiles)

            { view with
                Hostiles =
                    view.Hostiles
                    @ (bodies |> List.filter (fun hostile -> not (Colony.isAlly hostile.Owner)))
                Allies =
                    view.Allies
                    @ (bodies |> List.filter (fun hostile -> Colony.isAlly hostile.Owner))
                InvaderCores =
                    view.InvaderCores @ (seen |> List.collect (fun (_, r) -> r.InvaderCores))
                Fallen = view.Fallen @ (seen |> List.collect (fun (_, r) -> r.Fallen))
                RoomControl =
                    (view.RoomControl, seen)
                    ||> List.fold (fun control (room, r) -> Map.add room r.Control control)
            }

    /// The hostiles this tick's glance holds (#663): `Hostiles` less each
    /// body laid out of memory (`RecalledRaiders`), matched by id and tile,
    /// so one in sight across a border and laid back in the room it crossed
    /// out of stays where it stands. The only bodies an act — a strike, a
    /// heal, an aim — is aimed at: a remembered body shapes Tasks and prices,
    /// never an act.
    let inGlance (view: ColonyView) : HostileInfo list =
        match view.RecalledRaiders with
        | [] -> view.Hostiles
        | recalled ->
            let laid = recalled |> List.map (fun h -> h.Id, h.Pos) |> Set.ofList
            view.Hostiles |> List.filter (fun h -> not (laid.Contains(h.Id, h.Pos)))

    /// The view with only what was seen this tick in `Hostiles`, the raids
    /// recalled from earlier sightings (`RecalledRaiders`, #569) taken out:
    /// what the [[raid log]] folds, since a memory is no sighting.
    let sightedOnly (view: ColonyView) : ColonyView =
        match view.RecalledRaiders with
        | [] -> view
        | _ ->
            { view with
                Hostiles = inGlance view
                RecalledRaiders = []
            }

    /// What a colony may see of a room it carries for a child of its own: the
    /// controller its workers upgrade — or, where the child has been lost,
    /// [[claim]] — the sites they build, and the spawn, the tile her
    /// [[pioneer]]s walk up to. Beside the kinds, at most one store: the
    /// [[ferry]]'s sink.
    let private borrowable (kind: TargetKind) =
        match kind with
        | Controller
        | Site _
        | Structure BuiltKind.Spawn -> true
        | Source
        // A pile of either resource: a child's floor is the child's.
        | Dropped _
        | Tombstone
        // A child's deposit is the child's: the mother's [[pioneer]]s mine
        // nothing out there.
        | Mineral _
        | Structure _ -> false

    /// The one store of a bootstrapped child's the mother may carry: **the
    /// child's upgrade buffer** — a built container inside the child's own
    /// controller's Upgrade area and on none of its Seats. That is what a
    /// [[ferry]] fills, so the mother has to see how much room is left in it;
    /// a source container carried here would be a Withdraw in her pool and
    /// the child's income hauled across the Seam.
    let private ferrySink (stage: ColonyStage option) (facts: RoomFacts) : Set<string> =
        let placed = facts.Layer.TargetPositions
        let tileOf id = Map.tryFind id placed

        let idsOfKind = SpatialInfo.idsOfKindIn facts.TargetKinds

        match stage, idsOfKind Controller |> List.tryPick tileOf with
        | Some Bootstrapping, Some controller
        | Some Weaning, Some controller ->
            let sources = idsOfKind Source |> List.choose tileOf

            idsOfKind (Structure BuiltKind.Container)
            |> List.filter (fun id ->
                match tileOf id with
                | Some pos ->
                    range pos controller <= 3
                    && not (sources |> List.exists (fun s -> range pos s <= 1))
                | None -> false)
            |> Set.ofList
        | _ -> Set.empty

    /// A child's ramparts, ours by their hits as `Atlas.ourRampartTilesIn`
    /// reads them: the ground her garrison fights from (#467). Never hers to
    /// repair (`Facts.hungryStructures`).
    let private ourRamparts (facts: RoomFacts) =
        facts.Hits
        |> Map.filter (fun id _ ->
            Map.tryFind id facts.TargetKinds = Some(Structure BuiltKind.Rampart))

    /// One bootstrapped room's facts, cut down to the borrowed work: what
    /// this colony may **carry**, not what to read. What is left is the
    /// per-entry absence a room with no vision arrives in, so every rule
    /// downstream already answers correctly for it. The geometry is kept
    /// whole, being what the mother's workers walk over; the hits and the
    /// sources go. The room's **memory** needs no cutting (#271): a room
    /// reaches this arm only through a control entry, which is vision's, so
    /// its sighting is this tick's and the grace never reads it.
    let private borrowed (stage: ColonyStage option) (facts: RoomFacts) : RoomFacts =
        let sink = ferrySink stage facts
        let ramparts = ourRamparts facts

        // A [[nursery]]'s pioneers are the only bodies in it (#473): its rock
        // is theirs to dig, and the energy on its floor — the ferry's drop —
        // theirs to pick up.
        let nursery = stage = Some Nursery

        // Past the nursery the pioneers still dig the child's rock: its own
        // first bodies are too small to (user, 2026-10-04, W17S25).
        let digs = nursery || stage = Some Bootstrapping || stage = Some Weaning

        let nurseryWork kind =
            match kind with
            | Source
            | Dropped Energy -> digs
            | _ -> false

        let kinds =
            facts.TargetKinds
            |> Map.filter (fun id kind ->
                borrowable kind
                || nurseryWork kind
                || Set.contains id sink
                || Map.containsKey id ramparts)

        let piles =
            if digs then
                kinds |> Map.filter (fun _ kind -> kind = Dropped Energy)
            else
                Map.empty

        { facts with
            Layer =
                { facts.Layer with
                    TargetPositions =
                        facts.Layer.TargetPositions
                        |> Map.filter (fun id _ -> Map.containsKey id kinds)
                }
            TargetKinds = kinds
            Hits = ramparts
            Stores =
                facts.Stores
                |> Map.filter (fun id _ -> Set.contains id sink || Map.containsKey id piles)
            // A ferry carries energy, so no Thorium of the child's is a fact
            // the mother may act on; the extractor cooldown goes for the same
            // reason, and so do the owners (#318): what a mother may act on
            // is the explicit list `borrowable` holds.
            Thorium = Map.empty
            Minerals = Map.empty
            Cooldowns = Map.empty
            Owners = Map.empty
            Sources = if digs then facts.Sources else []
        }

    /// A room's facts without an [[ally]]'s loot: the tombstone of an ally's
    /// creep, and in a room an ally owns or reserves every pile and
    /// tombstone. The ally rule of what we may strike, applied to what we
    /// may take: no Task names it, so no body walks for it.
    let private withoutAllyLoot (facts: RoomFacts) : RoomFacts =
        let allyHolds =
            facts.RivalController |> Option.exists (fun rival -> Colony.isAlly rival.Owner)
            || facts.Control
               |> Option.bind (fun control -> control.Reservation)
               |> Option.exists (fun held -> Colony.isAlly held.Username)

        let loot =
            facts.TargetKinds
            |> Map.filter (fun id kind ->
                match kind with
                | Tombstone -> allyHolds || Set.contains id facts.AllyGraves
                | Dropped _ -> allyHolds
                | _ -> false)

        if Map.isEmpty loot then
            facts
        else
            let kept map =
                map |> Map.filter (fun id _ -> not (Map.containsKey id loot))

            { facts with
                Layer =
                    { facts.Layer with
                        TargetPositions = kept facts.Layer.TargetPositions
                    }
                TargetKinds = kept facts.TargetKinds
                Stores = kept facts.Stores
                Thorium = kept facts.Thorium
            }

    /// A **transit** room's facts: the ground a chain of [[seam]]s crosses and
    /// the bodies standing on it, and not one thing a rule could work.
    /// `furnitureOf` lays nothing in a transit room, but the tick one of our
    /// bodies walks through it `World.seenFacts` files its real sources and
    /// controller, and from there nothing tells it apart from a declared
    /// outpost. ADR-0058
    ///
    /// Live on 2026-09-10 that cost a colony its child (#286): W14S28, between
    /// W13S28 and the nursery it was raising, was reserved against a
    /// controller **no declaration names**, anchored and hauled from — 2,020
    /// of a 2,540-energy haul demand on a single-spawn colony whose worker
    /// row stood at zero of five. Killing the bodies did not stop it: the
    /// pioneers crossing the room are themselves the vision.
    ///
    /// So the promise is kept here, **per colony**: the same room is a
    /// transit room for the mother and an ordinary neighbour for whoever
    /// declares it. What survives is what is not *work* — terrain, occupants,
    /// the border ring, the hostiles Reach and Flee owe an answer about, and
    /// the room's ownership. What goes is every id a Task could name, and
    /// (#248) the tiles a rival's construction sites hold. The memory goes
    /// with the work, one level up in `ofWorld` (#271).
    let private transiting (facts: RoomFacts) : RoomFacts =
        // The one exception (#360): **decaying ore is not furniture**. Ore on
        // the floor bleeds `ceil(amount/1000)` a tick, the season never makes
        // another gram of it, and a transit room is where a crossing courier
        // dies — until this clause its tombstone landed where nothing could
        // name, pool or alarm on it. Admitted in **every** transit room, not
        // only those on an errand chain: ore reaches a room we have no other
        // business in only by falling out of a body of ours. What rides along
        // is the minimum a sweep needs — that it is there, where, and how
        // much ore — so nothing here can be repaired, refilled or withdrawn
        // from as furniture.
        let decaying =
            facts.TargetKinds
            |> Map.filter (fun _ kind ->
                match kind with
                | Dropped Thorium
                | Tombstone -> true
                | _ -> false)

        let oreOf table =
            table |> Map.filter (fun id _ -> Map.containsKey id decaying)

        { facts with
            Layer =
                { facts.Layer with
                    TargetPositions = oreOf facts.Layer.TargetPositions
                    // A rival's site is a placement fact (#248) and nothing is
                    // ever placed in a transit room; carried here the census
                    // signature would sign it and throw the plan memo away.
                    RivalSites = Map.empty
                }
            TargetKinds = decaying
            Hits = Map.empty
            Stores = Map.empty
            // Thorium, cooldowns and owners are work facts — what a quota
            // reads and what an Emitter gates on — so they go out with the
            // stores. What stays is the ore in the two decaying kinds (#360).
            // The ownership that survives a crossing is the **room**'s, on
            // `Control`.
            Thorium = oreOf facts.Thorium
            // The room's own [[ore]] is no score: it decays on the floor as
            // anything does, and goes out with the work.
            Minerals = Map.empty
            Cooldowns = Map.empty
            Owners = Map.empty
            Reactors = []
            Controller = None
            Refillables = []
            Sources = []
            ConstructionSites = []
        }

    /// A garrisoned child's home's facts (#479): a [[transit room]]'s, and
    /// beside them the controller the garrison's ring is read off and our
    /// ramparts it fights from — ground, and not one Task of the child's.
    let private garrisoning (facts: RoomFacts) : RoomFacts =
        let crossed = transiting facts
        let ramparts = ourRamparts facts

        let kinds =
            facts.TargetKinds
            |> Map.filter (fun id kind -> kind = Controller || Map.containsKey id ramparts)

        { crossed with
            Layer =
                { crossed.Layer with
                    TargetPositions =
                        facts.Layer.TargetPositions
                        |> Map.filter (fun id _ -> Map.containsKey id kinds)
                }
            TargetKinds = kinds
            Hits = ramparts
            Thorium = Map.empty
        }

    /// An **errand** room's facts: a [[transit room]]'s ground and bodies, and
    /// beside them the one thing a declaration out there names. **More** than
    /// a transit room: whatever the shell filed under the declared id — store,
    /// Thorium, cooldown — rides on the view. **Less** than an outpost:
    /// nothing else in that room is work, however much vision we pay for.
    ///
    /// The shell files two views of the one Reactor: `Owners` carries the
    /// narrow ours-or-not fact the ClaimReactor act needs, `Reactors` the
    /// richer row the observation channel reads. A relay that gaps drops
    /// both; the act treats missing ownership as *not ours*, the observation
    /// fold keeps its dated sample as stale.
    ///
    /// The kind census is **the decaying ore and the ore mine** (#356, #359):
    /// every pool sweeps `TargetKinds`, so a target classified by nothing is
    /// priceable by the errand's own Tasks and enumerable by no pool. What is
    /// classified is what decays and is gone in a few hundred ticks if nothing
    /// names it, and the room's [[ore]] deposit with what digs it (below).
    /// The target's hits stay out: an errand's target is not a thing this
    /// colony repairs.
    let private erranding (targets: Set<string>) (facts: RoomFacts) : RoomFacts =
        let crossed = transiting facts

        // The declaration's own targets, and beside them any Thorium lying on
        // this room's floor (#354, #356): nobody owns the room, we are the only
        // colony that walks a body to it, and the ore is score bleeding at
        // 1 T a tick. `Facts.ourThoriumPiles` was widened to reach an errand
        // room's floor and reached nothing, because this narrowing had already
        // taken the pile's kind out — a rule and its projection are read
        // together or not at all. **And the same ore one object over** (#359):
        // a tombstone or ruin holding Thorium (a courier that dies loaded
        // leaves 175 T in its tombstone, W15S25 live). Recognised by the kind
        // **and** an entry in the Thorium map, because `Tombstone` does not
        // name a resource: a tombstone holding only energy stays out.
        let decayingOre =
            facts.TargetKinds
            |> Map.filter (fun id kind ->
                kind = Dropped Thorium || (kind = Tombstone && Map.containsKey id facts.Thorium))

        // **And the room's ore mine**: an ore deposit, the
        // extractor on its tile, and the containers and our container sites
        // on its Seats — what the miner digs, the Layout seats and the haul
        // draws from. Whether it is dug is the colony's (`Facts.digWanted`);
        // what is carried is only ever the mine.
        let tileOf id =
            Map.tryFind id facts.Layer.TargetPositions

        let oreTiles =
            facts.TargetKinds
            |> Map.toList
            |> List.choose (fun (id, kind) ->
                match kind with
                | Mineral(Ore _) -> tileOf id
                | _ -> None)

        let mine =
            facts.TargetKinds
            |> Map.filter (fun id kind ->
                let at reach =
                    tileOf id
                    |> Option.exists (fun tile ->
                        oreTiles |> List.exists (fun rock -> range tile rock <= reach))

                match kind with
                | Mineral(Ore _) -> true
                // The room's sources, for the energy the bodies building and
                // repairing the mine dig where they stand (`ofWorld` files
                // them as `BorrowedSources`): no Post, haul or row is hung
                // off them.
                | Source -> true
                | Structure BuiltKind.Extractor -> at 0
                | Structure BuiltKind.Container
                | Site BuiltKind.Container -> at 1
                | _ -> false)

        let admitted =
            Set.unionMany
                [ targets; decayingOre |> Map.keys |> Set.ofSeq; mine |> Map.keys |> Set.ofSeq ]

        let named map =
            map |> Map.filter (fun id _ -> Set.contains id admitted)

        let mined map =
            map |> Map.filter (fun id _ -> Map.containsKey id mine)

        { crossed with
            Layer =
                { crossed.Layer with
                    TargetPositions = named facts.Layer.TargetPositions
                }
            TargetKinds = Map.fold (fun kinds id kind -> Map.add id kind kinds) decayingOre mine
            // The mine's container decays like an outpost's, and is repaired
            // like one.
            Hits = mined facts.Hits
            Minerals = mined facts.Minerals
            ConstructionSites =
                facts.ConstructionSites
                |> List.filter (fun site -> Map.containsKey site.Id mine)
            Sources = facts.Sources
            // **The energy column is the declaration's alone** (#359): a
            // tombstone that also holds energy would otherwise pool an energy
            // Withdraw three crossings from home off a census entry granted
            // for the ore. An ore pile's amount is filed in `Thorium`, so it
            // has no entry here to lose.
            Stores = facts.Stores |> Map.filter (fun id _ -> Set.contains id targets)
            Thorium = named facts.Thorium
            Cooldowns = named facts.Cooldowns
            Owners = named facts.Owners
            Reactors =
                facts.Reactors |> List.filter (fun reactor -> Set.contains reactor.Id targets)
        }

    /// The structures one salvage room's dismantler takes down: the ids vision
    /// classifies as a salvage target, and has a tile for — and none while
    /// anybody but us owns or reserves the room, whatever the [[stand-down]]
    /// makes of that a tick later.
    let private salvageTargets (facts: RoomFacts) : Set<string> =
        let nobodyElseHolds =
            facts.Control
            |> Option.exists (fun control ->
                control.Owner = Ownership.Unowned
                && Option.isNone (RoomControlInfo.heldByOther control))

        if not nobodyElseHolds then
            Set.empty
        else
            facts.TargetKinds
            |> Map.filter (fun id kind ->
                Salvage.isTarget kind && Map.containsKey id facts.Layer.TargetPositions)
            |> Map.keys
            |> Set.ofSeq

    /// A [[salvage]] room's facts: a [[transit room]]'s ground and bodies, and
    /// beside them the tile of each structure its dismantler takes down, with
    /// no kind, hits or store for any of them. ADR-0079
    let private salvaging (facts: RoomFacts) : RoomFacts =
        let crossed = transiting facts
        let targets = salvageTargets facts

        { crossed with
            Layer =
                { crossed.Layer with
                    TargetPositions =
                        (crossed.Layer.TargetPositions, facts.Layer.TargetPositions)
                        ||> Map.fold (fun placed id pos ->
                            if Set.contains id targets then
                                Map.add id pos placed
                            else
                                placed)
                }
        }

    /// The containers one harassment room's dismantler takes down: every
    /// one vision places there, and none in a room anybody owns or anybody
    /// but the declared enemy reserves — an ally's hold or ours is never
    /// the enemy's remote. A room nobody reserves counts only when it is not
    /// also one of our outposts: there a lapsed reservation means our own
    /// reserver stood down, and the containers are ours.
    let private harassTargets (ourOutpost: bool) (enemy: string) (facts: RoomFacts) : Set<string> =
        let enemyHolds (held: ReservationInfo) = held.Username = enemy

        let enemyRemote =
            facts.Control
            |> Option.exists (fun control ->
                control.Owner = Ownership.Unowned
                && (if ourOutpost then
                        Option.exists enemyHolds control.Reservation
                    else
                        Option.forall enemyHolds control.Reservation))

        if not enemyRemote then
            Set.empty
        else
            facts.TargetKinds
            |> Map.filter (fun id kind ->
                kind = Structure BuiltKind.Container
                && Map.containsKey id facts.Layer.TargetPositions)
            |> Map.keys
            |> Set.ofSeq

    /// A [[harassment room]]'s facts: a [[transit room]]'s ground and bodies,
    /// hostiles included, and the tile of each container its dismantler takes
    /// down, with no kind, hits or store — so nothing Refills, Withdraws from
    /// or Repairs it.
    let private harassing (enemy: string) (facts: RoomFacts) : RoomFacts =
        let crossed = transiting facts
        let targets = harassTargets false enemy facts

        { crossed with
            Layer =
                { crossed.Layer with
                    TargetPositions =
                        (crossed.Layer.TargetPositions, facts.Layer.TargetPositions)
                        ||> Map.fold (fun placed id pos ->
                            if Set.contains id targets then
                                Map.add id pos placed
                            else
                                placed)
                }
        }

    /// Every rampart standing in a room that is not ours, by tile: the rival's
    /// own, which the engine lets no body of ours step onto.
    let private rivalRamparts (facts: RoomFacts) : Map<Pos, string> =
        facts.TargetKinds
        |> Map.filter (fun id kind ->
            kind = Structure BuiltKind.Rampart && not (Map.containsKey id facts.Hits))
        |> Map.toList
        |> List.choose (fun (id, _) ->
            Map.tryFind id facts.Layer.TargetPositions |> Option.map (fun tile -> tile, id))
        |> Map.ofList

    /// An [[assault]] room's facts (#490): a [[transit room]]'s, with the
    /// rival's ramparts laid in as obstacles — the projection calls a rampart
    /// walkable whoever owns it, and a squad that walks for a tower must go
    /// round the line it has not broken.
    let private assaulting (facts: RoomFacts) : RoomFacts =
        let crossed = transiting facts

        { crossed with
            Layer =
                { crossed.Layer with
                    Obstacles =
                        rivalRamparts facts
                        |> Map.keys
                        |> Seq.fold
                            (fun blocked tile -> Set.add tile blocked)
                            crossed.Layer.Obstacles
                }
        }

    /// One assault as this tick's vision of its room reads it, and its safe
    /// mode as the rival controllers remembered read it.
    let private assaultFacts
        (tuning: Tuning)
        (taken: Set<string>)
        (world: World)
        (assault: Assault)
        : AssaultFacts =
        let facts = World.roomOf world assault.RoomName

        let remembered =
            Map.tryFind assault.RoomName world.RivalControllers
            |> Option.exists (fun c -> c.SafeModeUntil > world.Time)

        let placed kind =
            facts.TargetKinds
            |> Map.filter (fun _ seen -> seen = Structure kind)
            |> Map.toList
            |> List.choose (fun (id, _) ->
                Map.tryFind id facts.Layer.TargetPositions |> Option.map (fun tile -> id, tile))
            |> List.sortBy snd

        let ramparts = rivalRamparts facts

        let rampartOn tile =
            Map.tryFind tile ramparts |> Option.map (fun id -> id, tile) |> Option.toList

        // A declaim's every tower first, wherever it stands: once built it
        // outshoots the escorts' heal on the breach and the tap tile. Then
        // each declared target's tile: its rampart, then what it covers.
        let towers =
            match assault.Mode with
            | Declaim ->
                placed BuiltKind.Tower
                |> List.collect (fun (id, tile) -> rampartOn tile @ [ id, tile ])
            | _ -> []

        let declared =
            towers
            @ (assault.Targets
               |> List.collect (fun tile ->
                   rampartOn tile
                   @ (facts.TargetKinds
                      |> Map.toList
                      |> List.filter (fun (id, kind) ->
                          match kind with
                          | Structure BuiltKind.Rampart
                          | Structure BuiltKind.Road -> false
                          | Structure _ -> Map.tryFind id facts.Layer.TargetPositions = Some tile
                          | _ -> false)
                      |> List.map (fun (id, _) -> id, tile)
                      |> List.sort)))

        // A declaim's every other rampart after its declared ones (user,
        // 2026-10-14), nearest its controller first: what its sappers take
        // down once the tap tile is clear. Never in `Taken`, which gates the
        // taps.
        let demolition =
            match assault.Mode with
            | Declaim ->
                let named = declared |> List.map fst |> Set.ofList

                let from =
                    facts.TargetKinds
                    |> Map.tryFindKey (fun _ kind -> kind = Controller)
                    |> Option.bind (fun id -> Map.tryFind id facts.Layer.TargetPositions)

                ramparts
                |> Map.toList
                |> List.filter (fun (_, id) -> not (Set.contains id named))
                |> List.sortBy (fun (tile, id) ->
                    (from |> Option.map (fun c -> range c tile) |> Option.defaultValue 0), id)
                |> List.map (fun (tile, id) -> id, tile)
            | _ -> []

        let targets =
            facts.Control
            |> Option.map (fun _ ->
                let breach = assault.Breach |> List.collect rampartOn

                match assault.Mode, assault.Targets with
                | _, _ :: _ -> breach @ declared @ demolition
                | Provoke, []
                | Declaim, [] -> breach
                | Strike, [] -> breach @ placed BuiltKind.Tower @ placed BuiltKind.Spawn)

        {
            Assault = assault
            Targets = targets
            Taken =
                not (List.isEmpty assault.Targets)
                && (match facts.Control with
                    | Some _ -> List.isEmpty declared
                    | None -> Set.contains assault.RoomName taken)
            Towers = placed BuiltKind.Tower |> List.map snd
            Ramparts = ramparts |> Map.keys |> List.ofSeq
            Sites =
                facts.Layer.RivalSites
                |> Map.toList
                |> List.choose (fun (tile, (kind, progress)) ->
                    match kind with
                    | BuiltKind.Road
                    | BuiltKind.Container
                    | BuiltKind.Rampart
                    | BuiltKind.Other -> None
                    | _ when progress > 0 -> Some(tile, kind)
                    | _ -> None)
            Controller =
                facts.TargetKinds
                |> Map.tryFindKey (fun _ kind -> kind = Controller)
                |> Option.bind (fun id ->
                    Map.tryFind id facts.Layer.TargetPositions |> Option.map (fun tile -> id, tile))
            TapAt =
                let held =
                    Map.tryFind assault.RoomName world.RivalControllers
                    |> Option.filter (fun c -> c.Owner = assault.Enemy)

                let lost =
                    facts.Control |> Option.exists (fun control -> control.Owner <> Ownership.Rival)

                match assault.Mode, held with
                | Declaim, Some c when not lost -> Some c.UpgradeBlockedUntil
                | _ -> None
            SafeMode =
                remembered || facts.Control |> Option.exists (fun control -> control.SafeMode)
            BarredUntil =
                RivalSafeMode.barredUntil
                    tuning.RivalIntelTicks
                    world.RivalControllers
                    assault.Enemy
                    assault.RoomName
                    world.Time
        }

    /// Every declared first spawn still to place (#476), world-wide: each
    /// [[nursery]] with a swept tile, in room order, named `SpawnN` from one
    /// past the highest N standing, so a name a lost room once held is never
    /// handed out again. One numbering over every colony, so two nurseries
    /// placed on one tick never share a name.
    let firstSpawns
        (stages: Map<string, ColonyStage>)
        (colonies: Colony list)
        (world: World)
        : (string * FirstSpawnSite) list =
        let numberOf (name: string) =
            if name.StartsWith "Spawn" then
                match System.Int32.TryParse(name.Substring 5) with
                | true, n -> Some n
                | _ -> None
            else
                None

        let highest =
            world.Rooms
            |> Map.toList
            |> List.collect (fun (_, facts) -> facts.Spawns)
            |> List.choose (fun spawn -> numberOf spawn.Name)
            |> List.fold max 0

        colonies
        |> List.choose (fun colony ->
            match colony.FirstSpawn with
            | Some tile when Map.tryFind colony.Home stages = Some Nursery ->
                Some(colony.Home, tile)
            | _ -> None)
        |> List.sortBy fst
        |> List.mapi (fun i (room, tile) ->
            room,
            {
                Tile = tile
                Name = $"Spawn{highest + 1 + i}"
            })

    /// One colony's cut of this tick before it is projected (`project`): the
    /// rooms its view projects (`cutRooms`, the view's `Spatial.Rooms`
    /// whole), the bodies it holds, and the facts the projection was handed.
    /// Lives within the tick.
    [<NoEquality; NoComparison>]
    type Cut =
        private
            {
                Joins: JoinTable
                Tuning: Tuning
                Colonies: Colony list
                Gate: StandDown
                Holders: Map<string, string>
                Fighter: CreepInfo -> bool
                Watch: WorldView
                World: World
                Colony: Colony
                Stages: Map<string, ColonyStage>
                Mine: WorldCreep list
                Scan: World.ScanSet
                Contested: string list
                Homeward: string list
                Rooms: string list
            }

    /// One colony's cut: the rooms it works and crosses out of the `World`,
    /// and the bodies it holds. Seven facts are handed in and none is
    /// decided here: the tunables, the declaration, the [[stand-down]] gate
    /// (Memory's answer, not the world's), the holders `World.creepColonies`
    /// cut over every living colony at once, which of them is a fighter
    /// (`isFighterBody`, the decision layer's), the world layer's read
    /// (`WorldView`, carried whole) and the world. The join table is the
    /// shell's, handed in for the life of the process (`JoinTable`).
    let cutOf
        (joins: JoinTable)
        (tuning: Tuning)
        (colonies: Colony list)
        (casting: HarassCasting)
        (gate: StandDown)
        (holders: Map<string, string>)
        (fighter: CreepInfo -> bool)
        // The world layer's read, built once a tick for every colony.
        (watch: WorldView)
        (world: World)
        (colony: Colony)
        : Cut =
        let home = colony.Home
        let stages = World.stages tuning colonies world

        // This colony's bodies, and the names to cut its geometry by, so the
        // fleet and the layers' occupants cannot disagree.
        let mine =
            world.Creeps
            |> List.filter (fun creep -> Map.tryFind creep.Info.Name holders = Some home)

        // The declaration's narrowings and their union, off the one
        // derivation the creep adoption reads too (`World.scanRecalling`),
        // and the shut rooms a body of ours still stands in (#512).
        let scan =
            World.scanRecalling
                joins
                tuning
                stages
                (World.unownedHomes colonies world)
                colonies
                casting
                gate
                (mine |> List.map (fun creep -> creep.Room) |> Set.ofList)
                world
                colony

        // Each contested room another colony declares — a raider seen or
        // recalled there, or a raid its declarer's log holds or remembers
        // (`WorldView.Danger`: the blind room #366 pools a Guard for) — within
        // `Tuning.LaneHops` crossings of a fighter of this colony over the
        // rooms' ground (`World.reachHops`, #617), with the rooms a walk of
        // those crossings may cross, projected as transit rooms off their
        // ground: the room a world Brief may hand this colony's fighter must
        // be one its Atlas prices. None in peace.
        let contested =
            let fighting =
                mine
                |> List.filter (fun creep ->
                    isGuardParts creep.Info.Body || partCount creep.Info.Body RangedAttack > 0)
                |> List.map (fun creep -> creep.Room)
                |> List.distinct

            let raided (room: string) =
                Map.containsKey room world.Raiders
                || watch.Danger.Contains room
                || (World.roomOf world room).Hostiles
                   |> List.exists (fun hostile -> not (Colony.isAlly hostile.Owner))

            match fighting with
            | [] -> []
            | _ ->
                let hops = World.reachHops gate joins tuning world tuning.LaneHops
                let held = Set.ofList scan.Scanned

                colonies
                |> List.filter (fun other -> other.Home <> home)
                |> Seq.collect Colony.declaredRooms
                |> Seq.distinct
                |> Seq.filter (fun room -> not (held.Contains room) && raided room)
                |> Seq.collect (fun room ->
                    fighting
                    |> List.collect (fun at ->
                        match hops at room with
                        | Some n -> room :: RoomName.transitWithin n at room
                        | None -> []))
                |> Seq.filter (fun room -> not (held.Contains room))
                |> Seq.distinct
                |> List.ofSeq

        // The walk home of each fighter this colony holds and another cast,
        // to its caster's home and the rooms between, inside the hop budget,
        // projected as transit rooms: the home an idle fighter walks to is
        // its caster's (`ColonyView.idleHome`), and the walk must be one this
        // colony's Atlas prices. Its first step out of the rooms this colony
        // alone projects hands it on: to its caster, or to whoever projects
        // the room it steps into.
        let homeward =
            let held = Set.ofList (scan.Scanned @ contested)

            match
                mine
                |> List.filter (fun creep -> fighter creep.Info)
                |> List.choose (fun creep ->
                    match Map.tryFind creep.Info.Name watch.CastBy.Value with
                    | Some(caster, _) when caster <> home && creep.Room <> caster ->
                        Some(creep.Room, caster)
                    | _ -> None)
            with
            | [] -> []
            | walks ->
                // Every chain the Atlas may walk it by, over the masked rings
                // of every room's ground and round the rooms a rival owns
                // (`World.reachesOver`, #617); one no chain joins inside the
                // budget projects nothing, and the body walks to its holder's
                // home.
                let linked = World.reachesOver gate joins tuning world

                walks
                |> List.distinct
                |> List.collect (fun (from, caster) ->
                    RoomName.routesBy linked tuning.MaxHops from caster |> List.concat)
                |> List.filter (held.Contains >> not)
                |> List.distinct

        {
            Joins = joins
            Tuning = tuning
            Colonies = colonies
            Gate = gate
            Holders = holders
            Fighter = fighter
            Watch = watch
            World = world
            Colony = colony
            Stages = stages
            Mine = mine
            Scan = scan
            Contested = contested
            Homeward = homeward
            Rooms = scan.Scanned @ contested @ homeward
        }

    /// The rooms a cut's view projects, in scan order: its `Spatial.Rooms`.
    let cutRooms (cut: Cut) : string list = cut.Rooms

    /// The fighters a cut's colony holds, by name: its view's `Fighters`.
    let cutFighters (cut: Cut) : string list =
        cut.Mine
        |> List.filter (fun creep -> cut.Fighter creep.Info)
        |> List.map (fun creep -> creep.Info.Name)

    // ADR-0091
    /// The lane of a tick some colonies decide nothing on: every room a
    /// deciding colony projects and each room beside one, over which a
    /// colony that decides nothing is projected (`project`).
    let lane (deciding: Cut list) : Set<string> =
        deciding
        |> List.collect (fun cut -> cut.Rooms)
        |> List.collect (fun room -> room :: RoomName.adjacent room)
        |> Set.ofList

    /// One colony's view of this tick off its cut: the rooms it works cut
    /// out of the `World`, the bodies it holds, its own bank and controller,
    /// and the explicit little it may take of a child's. **Pure, and that is
    /// the point of it**: the shell reads the engine once (`World.ofGame`)
    /// and every rule about which rooms a colony works, which creeps are its
    /// own and what it may borrow is here, where a test can hand it a
    /// two-colony world. `within`, a colony that decides nothing this tick:
    /// only the rooms of the cut in the lane (`lane`) and its home are cut
    /// out, each as the whole view cuts it; the colony's sets (its crossings,
    /// borrowed rooms, declarations) stay the whole cut's.
    let project (within: Set<string> option) (cut: Cut) : ColonyView =
        let joins, tuning, colonies = cut.Joins, cut.Tuning, cut.Colonies

        let gate, holders, fighter, watch = cut.Gate, cut.Holders, cut.Fighter, cut.Watch
        let world, colony, stages, mine = cut.World, cut.Colony, cut.Stages, cut.Mine
        let scan, contested, homeward = cut.Scan, cut.Contested, cut.Homeward
        let home = colony.Home

        // Its fighters apart from its economy: the world's fight commands them.
        let fighters, economy = mine |> List.map _.Info |> List.partition fighter

        let outposts = scan.Outposts
        let errands = scan.Errands
        let salvageRooms = Set.ofList scan.Salvage

        let harassEnemies =
            scan.Harass |> List.map (fun h -> h.RoomName, h.Enemy) |> Map.ofList

        let assaultRooms =
            scan.Assaults |> List.map (fun assault -> assault.RoomName) |> Set.ofList

        let bootstrap = scan.Borrowed

        let scanned =
            match within with
            | None -> cut.Rooms
            | Some lane -> cut.Rooms |> List.filter (fun room -> room = home || lane.Contains room)

        // The `elif` chain below reads this after the bootstrap and transit
        // branches and before the worked ones, which **presumes the two
        // declaration lists name disjoint rooms** — true by the types' own
        // definitions (an `Outpost` carries a mandatory `Controller`, an
        // `Errand` exists for the room that has none). A human who wrote one
        // room into both lists is caught red before deploy over
        // `Colony.declared` (`ViewTests`, "no room is declared as both"): not
        // refused at runtime, since `Refused` means "no chain reaches it",
        // and not unioned, which would accept the contradiction.
        let errandRooms = errands |> List.map (fun errand -> errand.RoomName) |> Set.ofList

        // The rooms in the set for the walk alone, derived by subtraction
        // because the union (`Colony.roomsProjected`) is the one place that
        // rule is spelled. Off the whole cut: a room the lane leaves out is
        // still one the colony crosses (`Crossed`).
        let transit = cut.Rooms |> List.filter (scan.Worked.Contains >> not) |> Set.ofList

        // The rooms held for a stray body alone (#570), narrowed as transit
        // rooms are.
        let stranded = Fresh.setOfSeq scan.Stranded

        // The lane's rooms and the walks home, which may lie beyond
        // `World.Rooms` (#617).
        let laned = Set.ofList (contested @ homeward)

        // Each scanned room's facts and its memory (#151), in scan order. A
        // room the colony only **crosses** keeps neither, one rule and not
        // two (#271): `transiting` takes its every id out, and a memory of
        // those ids would put them back the tick the room went dark. The live
        // case is a [[stand-down]] on a room a chain runs *through* — the
        // gate takes it out of `outposts` and the chain keeps it in
        // `scanned`, and its Withdraw went on holding a hauler for a whole
        // `Tuning.VisionGrace`.
        let narrowed =
            scanned
            |> List.map (fun room ->
                // A stranded room beyond `World.Rooms` reads a stray's ground
                // (#570), and a lane's or a walk home's room the ground its
                // reach was read off.
                let facts =
                    if stranded.Contains room || laned.Contains room then
                        World.groundOf world room |> Option.defaultValue RoomFacts.empty
                    else
                        World.roomOf world room
                    |> withoutAllyLoot

                let remembered = Map.tryFind room world.Sightings

                if List.contains room bootstrap then
                    room, borrowed (Map.tryFind room stages) facts, remembered
                elif List.contains room scan.Garrisoned then
                    room, garrisoning facts, None
                // No memory, as a transit room keeps none.
                elif Set.contains room assaultRooms then
                    room, assaulting facts, None
                elif Set.contains room transit then
                    room, transiting facts, None
                elif Set.contains room errandRooms then
                    // The errand room's memory is narrowed by the same rule
                    // its facts are (#271): an id this colony may not work is
                    // not one it may be held to while the room is dark.
                    let targets = Errand.targetsIn room errands

                    room,
                    erranding targets facts,
                    remembered
                    |> Option.map (fun sighting ->
                        { sighting with
                            Targets = lazy (Set.intersect sighting.Targets.Value targets)
                        })
                elif Set.contains room salvageRooms then
                    // No memory, as a transit room keeps none: while anything
                    // stands to take down it is vision, so the room is never
                    // dark while there is work in it.
                    room, salvaging facts, None
                else
                    match Map.tryFind room harassEnemies with
                    // A room this colony mines is its outpost whole, harassed
                    // or not (user, 2026-10-04: W17S26, W17S25's outpost and a
                    // room W15S28 harasses): its containers, sites and piles
                    // are ours to work. The harassment is the caster's.
                    | Some _ when
                        outposts
                        |> List.exists (fun outpost ->
                            outpost.RoomName = room
                            && not (List.contains room (Colony.homes colonies)))
                        ->
                        room, facts, remembered
                    // No memory, as a transit room keeps none. Ahead of the
                    // outposts: a Claim outpost harassed until its Claim lands
                    // (W17S25) works its declared controller and rock, laid in
                    // below, and nothing the cut takes out.
                    | Some enemy -> room, harassing enemy facts, None
                    | None -> room, facts, remembered)

        let worked = narrowed |> List.map (fun (room, facts, _) -> room, facts)

        let names = mine |> List.map (fun creep -> creep.Info.Name) |> Set.ofList

        // The rooms whose safe mode this colony fires (#449): its home, and
        // each child it raises that has no spawn standing yet.
        let nurseries =
            bootstrap |> List.filter (fun room -> Map.tryFind room stages = Some Nursery)

        let fired = home :: nurseries

        let collected (select: RoomFacts -> 'a list) = worked |> List.collect (snd >> select)

        // The raid last seen in each room it works, an assault's aside (its
        // own clocks read what the room was seen to hold), chases a raid in,
        // or watches for a child of its own, where nothing sees it this tick
        // (#569): a raider is the room's raid until a sighting finds it gone.
        let recalled =
            let raid =
                match world.Raiders with
                | raiders when Map.isEmpty raiders -> []
                | raiders ->
                    let childOutposts = Set.ofList scan.ChildOutposts

                    worked
                    |> List.filter (fun (room, _) ->
                        (scan.Worked.Contains room && not (assaultRooms.Contains room)
                         || gate.ChasedOutposts.Contains room
                         || childOutposts.Contains room)
                        && not (World.seenNow world room))
                    |> List.collect (fun (room, _) ->
                        match Map.tryFind room raiders with
                        | Some last ->
                            last.Raid
                            |> List.map (fun h ->
                                { h with
                                    TicksToLive = h.TicksToLive - (world.Time - last.Tick)
                                })
                        | None -> [])

            // And every body the world remembers in an outpost it works or
            // a [[sweep]] candidate that this tick's glance of the room does
            // not hold (`Quarry.ghosts`, #663, #618), the room dark or seen:
            // out of sight, or in sight across the border it crossed out
            // over. A rival's economy body taking from us stays the room's
            // kill, and a Task on it holds, until it is known gone or its
            // odds run out, as a raider stays its raid. Laid apart from the
            // glance (`RecalledRaiders`): no act is aimed at one
            // (`ColonyView.inGlance`).
            match world.Quarries with
            | quarries when Map.isEmpty quarries -> raid
            | quarries ->
                let laid = raid |> List.map _.Id |> Set.ofList

                let rooms =
                    (outposts |> List.map _.RoomName) @ (scan.Harass |> List.map _.RoomName)
                    |> Set.ofList
                    |> Set.intersect (worked |> List.map fst |> Set.ofList)

                let side = collected (fun facts -> facts.Hostiles)

                // Rooted at its work by the one rule (`Quarry.rooted`), off
                // the targets this cut projects of its room: its `Spatial`'s,
                // as `Facts.rootedAtWork` reads them.
                let rooted (body: HostileInfo) =
                    match worked |> List.tryFind (fun (room, _) -> room = body.Pos.Room) with
                    | None -> false
                    | Some(_, facts) ->
                        Quarry.rooted
                            (fun kind ->
                                Quarry.besideTarget
                                    facts.Layer.TargetPositions
                                    facts.TargetKinds
                                    kind
                                    (RoomPos.pos body.Pos))
                            side
                            body

                raid
                @ (Quarry.ghosts
                    rooms
                    (Quarry.raidHeld world).Contains
                    rooted
                    side
                    world.Time
                    quarries
                   |> List.filter (fun h -> not (laid.Contains h.Id)))

        // A child past its nursery: its rock is borrowed, never the mother's.
        let raised room =
            List.contains room bootstrap && Map.tryFind room stages <> Some Nursery

        let sourcesWhere keep =
            worked
            |> List.filter (fst >> keep)
            |> List.collect (fun (_, facts) -> facts.Sources)

        // The id-keyed tables merged flat, an object id being unique across
        // the world. Seeded with the **biggest** room's table rather than
        // `Map.empty` (#384): no key is ever written twice, so the result does
        // not depend on the seed, and the home room is 204 of a colony's ~320
        // entries, so six merges a tick stop re-inserting two thirds of what
        // they touch.
        let mergedBy (select: RoomFacts -> Map<string, 'v>) =
            match worked |> List.map (snd >> select) with
            | [] -> Map.empty
            | tables ->
                let seed = tables |> List.maxBy Map.count

                (seed, tables)
                ||> List.fold (fun acc table ->
                    // By reference, and only the seed instance is skipped: two
                    // rooms cannot hand back the same table object.
                    if obj.ReferenceEquals(table, seed) then
                        acc
                    else
                        (acc, table) ||> Map.fold (fun acc id value -> Map.add id value acc))

        let homeFacts = World.roomOf world home

        // The scan set's own control entries, and beside them the one look
        // #165 buys a latched room: whatever vision answered for it this
        // tick, and nothing else. Read off the **declaration** and never off
        // the latch's own room names: a hand-edited `rivalHeld` leaf is a room
        // name a human wrote, and the only rooms this colony may look into are
        // the ones it declared. A room vision did not answer for adds no
        // entry, which is why the latch survives every recheck the colony is
        // blind on.
        let control =
            (worked
             |> List.choose (fun (room, facts) ->
                 facts.Control |> Option.map (fun control -> room, control))
             |> Map.ofList,
             colony.Outposts
             |> List.filter (fun outpost -> Set.contains outpost.RoomName gate.Rechecked))
            ||> List.fold (fun control outpost ->
                match (World.roomOf world outpost.RoomName).Control with
                | Some seen -> Map.add outpost.RoomName seen control
                | None -> control)

        {
            Time = world.Time
            Spawns = homeFacts.Spawns
            Casting = homeFacts.Casting
            Tuning = tuning
            Bank = homeFacts.Energy
            Refillables = homeFacts.Refillables
            // Every worked room's sources but a bootstrapped child's (#192) and
            // an errand room's, with the declared outpost rocks laid in
            // beside them. Those two are dug only by a body already standing
            // there: the child's pioneers, and the bodies building or
            // repairing the errand room's ore mine.
            Sources =
                sourcesWhere (fun room -> not (raised room || errandRooms.Contains room))
                |> Outpost.pooledSources scanned outposts
            BorrowedSources = sourcesWhere (fun room -> raised room || errandRooms.Contains room)
            MinedRooms =
                colonies
                |> List.collect (fun other -> other.Outposts |> List.map (fun o -> o.RoomName))
                |> Fresh.setOfSeq
            AidRooms =
                colonies
                |> List.map (fun other -> other.Home)
                |> List.filter ((<>) home)
                |> List.choose (fun other ->
                    let facts = World.roomOf world other

                    let ids kind =
                        SpatialInfo.idsOfKindIn facts.TargetKinds (Structure kind)

                    match ids BuiltKind.Terminal, ids BuiltKind.Storage with
                    | _ :: _, storage :: _ ->
                        let energy = Map.tryFind storage facts.Stores |> Option.defaultValue 0

                        let reserve =
                            facts.Controller
                            |> Option.map (fun controller ->
                                Tuning.energyReserve tuning controller.Level)
                            |> Option.defaultValue 0

                        // Keyed on the shortfall, negated, so the furthest
                        // short sorts first.
                        if energy < reserve then
                            Some(energy - reserve, other)
                        else
                            None
                    | _ -> None)
                |> List.sort
                |> List.map snd
            ReserveShort = gate.ReserveShort
            DigOpen = gate.DigOpen
            HaulOverflow = gate.HaulOverflow
            Controller = homeFacts.Controller
            RoomControl = control
            // The gate's third and fourth sets, verbatim (#333, #366). Not
            // narrowed to the declaration the way `Rechecked` is: that one
            // *admits* a look, while these only ever take a controller out of
            // a pool or put a Guard in for a declared room, and their readers
            // intersect them with the projected outposts anyway.
            HeldOutposts = gate.HeldOutposts
            ThreatenedOutposts = gate.ThreatenedOutposts
            RaidedOutposts = gate.RaidedOutposts
            RaidedUntil = gate.RaidedUntil
            ChasedOutposts = gate.ChasedOutposts
            Fought = gate.Fought
            HomeRaided = gate.HomeRaided
            KeepHitsBefore = gate.KeepHitsBefore
            ConstructionSites = collected (fun facts -> facts.ConstructionSites)
            PlayerSites = world.Sites
            Creeps = economy
            Fighters = fighters
            Refusals =
                world.Refusals
                |> List.filter (fun row ->
                    if row.Body then
                        Map.tryFind row.Actor holders = Some home
                    else
                        row.Home = home)
            // Its economy bodies alone: a fighter is the world's fight's.
            Unprojected =
                mine
                |> List.filter (fun creep -> not (fighter creep.Info))
                |> List.filter (fun creep ->
                    not (List.exists (fun (room, _) -> room = creep.Room) worked))
                |> List.map (fun creep -> creep.Info.Name)
                |> Set.ofList
            // An ally's creep is no hostile (#412): this is the one entry every
            // rule that fires, flees, stands down or counts a rival reads.
            Hostiles =
                (collected (fun facts -> facts.Hostiles)
                 |> List.filter (fun hostile -> not (Colony.isAlly hostile.Owner)))
                @ recalled
            Allies =
                collected (fun facts -> facts.Hostiles)
                |> List.filter (fun hostile -> Colony.isAlly hostile.Owner)
            InvaderCores = collected (fun facts -> facts.InvaderCores)
            Fallen = collected (fun facts -> facts.Fallen)
            Spatial =
                {
                    RoomName = Some home
                    Rooms =
                        worked
                        |> List.map (fun (room, facts) ->
                            room,
                            { facts.Layer with
                                CreepPositions =
                                    facts.Layer.CreepPositions
                                    |> Map.filter (fun name _ -> Set.contains name names)
                            })
                        |> Map.ofList
                    Borders =
                        worked |> List.map (fun (room, facts) -> room, facts.Border) |> Map.ofList
                    TargetKinds = mergedBy (fun facts -> facts.TargetKinds)
                    Hits = mergedBy (fun facts -> facts.Hits)
                    Stores = mergedBy (fun facts -> facts.Stores)
                    Thorium = mergedBy (fun facts -> facts.Thorium)
                    Minerals = mergedBy (fun facts -> facts.Minerals)
                    Cooldowns = mergedBy (fun facts -> facts.Cooldowns)
                    PublicRamparts =
                        worked
                        |> Seq.collect (fun (_, facts) -> facts.PublicRamparts)
                        |> Fresh.setOfSeq
                    Owners = mergedBy (fun facts -> facts.Owners)
                    // A stronghold's room is entered by no chain either, a
                    // fighter's included: its towers reach every tile (live
                    // t980,650: two rangers crossing W16S26 to a Guard died).
                    // Whether or not this projection carries it.
                    RivalRooms =
                        worked
                        |> List.map fst
                        |> List.filter (World.rivalHeld world)
                        |> Seq.append gate.Impassable
                        |> Fresh.setOfSeq
                    AvoidRooms =
                        let avoided = World.avoidedRooms tuning world

                        worked |> List.map fst |> List.filter avoided.Contains |> Fresh.setOfSeq
                    StrayRooms = stranded
                    FightRooms =
                        cut.Rooms |> List.filter (scan.Economy.Contains >> not) |> Fresh.setOfSeq
                }
                // The declared furniture and the errands' targets go in last,
                // over the whole assembled projection: a declared id and tile
                // do not wait for vision.
                |> Outpost.place outposts
                |> Errand.place errands
            Declared = Colony.homes colonies
            // The scan set's own errand list and never `colony.Errands` read a
            // second time: a refused errand has left the scan set, and a rule
            // pooling off the declaration would name a target in a room the
            // projection does not hold. A held one's room is projected for
            // its ore alone.
            Errands = Errand.unheld errands
            OreRooms = errands |> List.map (fun errand -> errand.RoomName) |> Set.ofList
            Dismantles =
                (scan.Salvage
                 |> List.collect (fun room ->
                     World.roomOf world room |> salvageTargets |> Set.toList))
                @ (scan.Harass
                   |> List.collect (fun h ->
                       // A remote of ours, not a room we are claiming: a claim
                       // target's containers are the enemy's leftovers.
                       let ourOutpost =
                           colonies
                           |> List.exists (fun c ->
                               c.Outposts |> List.exists (fun o -> o.RoomName = h.RoomName))
                           && not (List.contains h.RoomName (Colony.homes colonies))

                       World.roomOf world h.RoomName
                       |> harassTargets ourOutpost h.Enemy
                       |> Set.toList))
            Harass = scan.Harass
            Assaults = scan.Assaults |> List.map (assaultFacts tuning gate.Taken world)
            HarassCast = scan.Cast |> List.map (fun h -> h.RoomName) |> Set.ofList
            // The declaration, straight through: the room it names is not one
            // this colony projects (#349).
            Consignee = colony.Consignee
            Labs =
                let cluster = Colony.labCluster

                if List.contains cluster.Home (Colony.homes colonies) then
                    let facts = World.roomOf world cluster.Home

                    let stock =
                        [ BuiltKind.Terminal; BuiltKind.Lab ]
                        |> List.collect (fun kind ->
                            SpatialInfo.idsOfKindIn facts.TargetKinds (Structure kind))
                        |> List.collect (fun id ->
                            Map.tryFind id facts.Minerals
                            |> Option.map Map.toList
                            |> Option.defaultValue [])
                        |> List.groupBy fst
                        |> List.map (fun (resource, held) -> resource, List.sumBy snd held)
                        |> Fresh.mapOfList

                    Some { Cluster = cluster; Stock = stock }
                else
                    None
            Perimeter = colony.Perimeter
            RampartFloors = colony.RampartFloors
            // Off the world's creeps and not `mine` (#448): who holds a body
            // is not whether it defends the room it stands in.
            Defenders =
                world.Creeps
                |> List.filter (fun creep ->
                    List.contains creep.Room fired
                    && (partCount creep.Info.Body Attack > 0
                        || partCount creep.Info.Body RangedAttack > 0))
                |> List.groupBy (fun creep -> creep.Room)
                |> List.map (fun (room, creeps) ->
                    room, creeps |> List.map (fun creep -> creep.Info))
                |> Fresh.mapOfList
            NurseryControllers =
                nurseries
                |> List.choose (fun room ->
                    (World.roomOf world room).Controller
                    |> Option.map (fun controller -> room, controller))
                |> Fresh.mapOfList
            FirstSpawns =
                match nurseries with
                | [] -> Map.empty
                | _ ->
                    firstSpawns stages colonies world
                    |> List.filter (fun (room, _) -> List.contains room nurseries)
                    |> Fresh.mapOfList
            RecalledHealers =
                home :: bootstrap @ scan.Garrisoned
                |> List.collect (fun room ->
                    Map.tryFind room world.Healers |> Option.defaultValue [])
                |> List.filter (fun seen -> seen.Tick < world.Time)
                |> List.map (fun seen -> seen.Healer)
            RecalledRaiders = recalled
            SafeModeRunning = World.safeModeRunning world
            // Less the rooms held for a stray alone (#570): no chain of the
            // colony's crosses them, no observer aims at them and no sweep
            // works them.
            Crossed = Set.difference transit stranded
            Evacuating = Set.ofList scan.Evacuating
            // The declared Reactors' own rows (#354): the store here is what
            // meters a delivery. A room without vision contributes no row, and
            // no row reads as "no room for a load", which keeps a load banked
            // at home rather than drawn towards a store nobody can see.
            Reactors = worked |> List.collect (fun (_, facts) -> facts.Reactors)
            Stages = stages
            Foreign =
                worked
                |> List.collect (fun (room, facts) ->
                    facts.Layer.CreepPositions
                    |> Map.toList
                    |> List.filter (fun (name, _) -> not (Set.contains name names))
                    |> List.map (snd >> RoomPos.at room))
                // Hostiles and allies alike (#536): a body rooted on a step is
                // one the engine refuses every tick, armed or not. One that
                // stepped stands on the tile it left too (#551): a body
                // swaying between two tiles enters the one it is not on.
                |> List.append (
                    collected (fun facts -> facts.Hostiles)
                    |> List.collect (fun h ->
                        match Map.tryFind h.Id world.HostileSteps with
                        | Some { Left = Some left } -> [ h.Pos; left ]
                        | _ -> [ h.Pos ])
                )
                |> Set.ofList
            Parked =
                collected (fun facts -> facts.Hostiles)
                |> List.filter (fun h ->
                    Map.tryFind h.Id world.HostileSteps
                    |> Option.exists (fun step -> step.At = h.Pos && step.Since < world.Time))
                |> List.map (fun h -> h.Id)
                |> Set.ofList
            Stepped =
                collected (fun facts -> facts.Hostiles)
                |> List.choose (fun h ->
                    Map.tryFind h.Id world.HostileSteps
                    |> Option.bind (fun step -> step.Left)
                    |> Option.map (fun left -> h.Id, left))
                |> Map.ofList
            SteppedBack =
                collected (fun facts -> facts.Hostiles)
                |> List.choose (fun h ->
                    Map.tryFind h.Id world.HostileSteps
                    |> Option.bind (fun step -> step.Back)
                    |> Option.map (fun back -> h.Id, back))
                |> Map.ofList
            // Every hostile's, in this cut or across its border: a body
            // dancing on it stands on either side.
            Bounces =
                world.HostileSteps
                |> Map.toSeq
                |> Seq.choose (fun (id, step) ->
                    step.Crossed |> Option.map (fun crossed -> id, crossed))
                |> Map.ofSeq
            Leaving = Set.empty
            Borrowed =
                {
                    Rooms = bootstrap
                    Defended = scan.Defended
                    Garrisoned = scan.Garrisoned
                    ChildOutposts = scan.ChildOutposts
                }
            // Read off the whole declaration and not off `scanned`, where these
            // rooms have just been subtracted (#243); asked over the **masked**
            // border rings the scan set's own narrowing was asked over, and
            // through the one combinator it is built on (#382), so the report
            // names the same refusals the set made.
            Refused =
                let reaches = World.reachesUnder gate joins tuning world

                Outpost.refused reaches tuning.MaxHops home colony.Outposts
                @ Errand.refused reaches tuning.MaxHops home (Errand.unheld colony.Errands)
                @ Salvage.refused reaches tuning.MaxHops home colony.Salvage
                @ (if World.harassReporter colonies world = Some home then
                       scan.Uncast
                       |> List.map (fun h ->
                           {
                               RoomName = h.RoomName
                               Kind = DeclarationKind.Harass
                           })
                   else
                       [])
            // The world's memory of these rooms and of no others (#151), read
            // off the same walk the facts are (#271).
            Sightings =
                narrowed
                |> List.choose (fun (room, _, remembered) ->
                    remembered |> Option.map (fun sighting -> room, sighting))
                |> Map.ofList
            Seen = world.Sightings
            // The world's holds in the rooms this colony works, transit
            // included: a defended child's home is projected as one.
            ExitHolds =
                worked
                |> List.choose (fun (room, _) ->
                    match Map.tryFind room world.ExitWatches with
                    | Some(ExitWatch.Held hold) when ExitHold.stands world.Time hold ->
                        Some(room, hold)
                    | _ -> None)
                |> Map.ofList
            World = watch
        }

    /// One colony's whole view of this tick (`project` of its `cutOf`).
    let ofWorldRecalling
        (joins: JoinTable)
        (tuning: Tuning)
        (colonies: Colony list)
        (casting: HarassCasting)
        (gate: StandDown)
        (holders: Map<string, string>)
        (fighter: CreepInfo -> bool)
        (watch: WorldView)
        (world: World)
        (colony: Colony)
        : ColonyView =
        cutOf joins tuning colonies casting gate holders fighter watch world colony
        |> project None

    /// `ofWorldRecalling` over a table of this call's own — the shape a test
    /// asks in, the way `Atlas.ofView` is `ofViewRecalling` over fresh tables.
    /// Every body it holds in `Creeps`, a fixture's whole view: this layer
    /// does not know a fighter's body.
    let ofWorld
        (tuning: Tuning)
        (colonies: Colony list)
        (gate: StandDown)
        (holders: Map<string, string>)
        (world: World)
        (colony: Colony)
        : ColonyView =
        ofWorldRecalling
            (JoinTable())
            tuning
            colonies
            HarassCasting.none
            gate
            holders
            (fun _ -> false)
            (WorldView.ofWorld colonies [ gate ] world)
            world
            colony
