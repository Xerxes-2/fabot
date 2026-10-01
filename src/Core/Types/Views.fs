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
        /// The children's homes this colony **defends** this tick
        /// (`Colony.defending`): projected as transit rooms, so nothing of
        /// them is work but the Guard the planner pools there.
        Defended: string list
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
        /// how many there are (`Facts.guardBlocksBeat` reads bodies, and there
        /// are none here). Not a withdrawal: the room is worked, which tells
        /// it apart from `StandDown.Shut`. **Vision overrules it**
        /// (`Planner.guardedOutposts` consults it only where there is none).
        ThreatenedOutposts: Set<string>
        /// Each resident room's [[fight]] record (`StandDown.Fought`).
        Fought: Map<string, FightLatch>
        /// Our construction sites in every room this colony works and has
        /// vision in: the Build pool is this list one to one.
        ConstructionSites: ConstructionSiteInfo list
        /// The creeps this colony holds this tick (`World.creepColonies`), in
        /// the world's own order, so who holds a body does not move the
        /// Matcher's order.
        Creeps: CreepInfo list
        /// Hostile creeps standing in any room this colony works and has
        /// vision in, each under its own room's name.
        Hostiles: HostileInfo list
        /// The invader cores standing in the rooms this colony works and can
        /// see. Its own list and not a widening of `Hostiles`: a raider is
        /// something a creep runs from this tick, a core is something a whole
        /// room is withheld from for thousands, and `FIND_HOSTILE_CREEPS` can
        /// never answer with a structure.
        InvaderCores: InvaderCoreInfo list
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
        /// seat on the reserver row.
        Errands: Errand list
        /// The structures standing in this colony's [[salvage]] rooms that its
        /// dismantler takes down (`Salvage.isTarget`), by id: placed in the
        /// projection and classified by nothing (`salvaging`), so a rule names
        /// them from here. Empty once the last one falls.
        Dismantles: string list
        /// The [[harassment room]]s this colony casts this tick
        /// (`World.harassCasters`), less those its [[stand-down]] shuts: what
        /// the ranger's Guard and its unarmed targets are read off.
        Harass: Harass list
        /// Every harassment room this colony casts, the shut ones included:
        /// the rooms the [[raid log]] reads as ours to attack, never as a
        /// raid on us.
        HarassCast: Set<string>
        /// The ranger blocks each room of `Harass` was floored at
        /// (`HarassCasting.Floors`), its ranger's least body (#457). A room
        /// missing here reads the full floor, `Tuning.HarassBlocks`.
        HarassFloors: Map<string, int>
        /// The home room of the colony this one **ships its banked Thorium to**
        /// (`Colony.Consignee`, #349), a declaration and not a sighting: the
        /// far end is outside every scan set this colony holds. A rule may
        /// read a **room name to send to** and never a fact about that room;
        /// the `send` is issued into that blindness on purpose, and a refusal
        /// costs the tick's call and nothing else.
        Consignee: string option
        /// The home room's declared perimeter (`Colony.Perimeter`, #446): the
        /// chokes the Layout ramparts beside the Keep.
        Perimeter: Pos list
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
        /// The towers of ours holding a shot's energy (`World.loadedTowers`)
        /// in each child's home this colony raises, under its room's name
        /// (#451): what its garrison fights beside. No entry for none.
        LoadedTowers: Map<string, int>
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
        /// Where **other colonies'** creeps stand in the rooms this colony
        /// works: bodies it does not hold and cannot move.
        Foreign: Set<RoomPos>
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
        /// The exit each room this colony works is held at (#450): the run its
        /// last armed Threat left by, while the hold stands
        /// (`World.watchExits`). What `Threat.threatsOf` lays the Guard's
        /// ground on while the room is empty of them.
        ExitHolds: Map<string, ExitHold>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ColonyView =
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
        | Mineral
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

        // Our ramparts there, ours by their hits as `Atlas.ourRampartTilesIn`
        // reads them: the ground her garrison fights from (#467). Never
        // hers to repair (`Facts.hungryStructures`).
        let ramparts =
            facts.Hits
            |> Map.filter (fun id _ ->
                Map.tryFind id facts.TargetKinds = Some(Structure BuiltKind.Rampart))

        let kinds =
            facts.TargetKinds
            |> Map.filter (fun id kind ->
                borrowable kind || Set.contains id sink || Map.containsKey id ramparts)

        { facts with
            Layer =
                { facts.Layer with
                    TargetPositions =
                        facts.Layer.TargetPositions
                        |> Map.filter (fun id _ -> Map.containsKey id kinds)
                }
            TargetKinds = kinds
            Hits = ramparts
            Stores = facts.Stores |> Map.filter (fun id _ -> Set.contains id sink)
            // A ferry carries energy, so no Thorium of the child's is a fact
            // the mother may act on; the extractor cooldown goes for the same
            // reason, and so do the owners (#318): what a mother may act on
            // is the explicit list `borrowable` holds.
            Thorium = Map.empty
            Cooldowns = Map.empty
            Owners = Map.empty
            Sources = []
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
                    RivalSites = Set.empty
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
            Cooldowns = Map.empty
            Owners = Map.empty
            Reactors = []
            Controller = None
            Refillables = []
            Sources = []
            ConstructionSites = []
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
    /// The kind census is **the decaying ore and nothing else** (#356, #359):
    /// every pool sweeps `TargetKinds`, so a target classified by nothing is
    /// priceable by the errand's own Tasks and enumerable by no pool. What is
    /// classified is what decays and is gone in a few hundred ticks if nothing
    /// names it. The hits stay out: an errand's target is not a thing this
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

        let admitted = Set.union targets (decayingOre |> Map.keys |> Set.ofSeq)

        let named map =
            map |> Map.filter (fun id _ -> Set.contains id admitted)

        { crossed with
            Layer =
                { crossed.Layer with
                    TargetPositions = named facts.Layer.TargetPositions
                }
            TargetKinds = decayingOre
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
    /// the enemy's remote.
    let private harassTargets (enemy: string) (facts: RoomFacts) : Set<string> =
        let enemyRemote =
            facts.Control
            |> Option.exists (fun control ->
                control.Owner = Ownership.Unowned
                && control.Reservation
                   |> Option.forall (fun held ->
                       held.Holder = ReservationHolder.Rival && held.Username = enemy))

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
        let targets = harassTargets enemy facts

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

    /// One colony's view of this tick: the rooms it works cut out of the
    /// `World`, the bodies it holds, its own bank and controller, and the
    /// explicit little it may take of a child's. **Pure, and that is the point
    /// of it**: the shell reads the engine once (`World.ofGame`) and every
    /// rule about which rooms a colony works, which creeps are its own and
    /// what it may borrow is here, where a test can hand it a two-colony
    /// world. Five facts are handed in and none is decided here: the
    /// tunables, the declaration, the [[stand-down]] gate (Memory's answer,
    /// not the world's), the holders `World.creepColonies` cut over every
    /// living colony at once, and the world. The join table is the shell's,
    /// handed in for the life of the process (`JoinTable`).
    let ofWorldRecalling
        (joins: JoinTable)
        (tuning: Tuning)
        (colonies: Colony list)
        (casting: HarassCasting)
        (gate: StandDown)
        (holders: Map<string, string>)
        (world: World)
        (colony: Colony)
        : ColonyView =
        let home = colony.Home
        let stages = World.stages tuning colonies world

        // The declaration's narrowings and their union, off the one
        // derivation the creep adoption reads too (`World.scanRecalling`).
        let scan =
            World.scanRecalling
                joins
                tuning
                stages
                (World.unownedHomes colonies world)
                colonies
                casting
                gate
                world
                colony

        let outposts = scan.Outposts
        let errands = scan.Errands
        let salvageRooms = Set.ofList scan.Salvage

        let harassEnemies =
            scan.Harass |> List.map (fun h -> h.RoomName, h.Enemy) |> Map.ofList

        let bootstrap = scan.Borrowed
        let scanned = scan.Scanned

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
        // rule is spelled.
        let transit =
            scanned
            |> List.filter (fun room ->
                room <> home
                && not (List.contains room bootstrap)
                && not (Set.contains room errandRooms)
                && not (Set.contains room salvageRooms)
                && not (Map.containsKey room harassEnemies)
                && not (outposts |> List.exists (fun outpost -> outpost.RoomName = room)))
            |> Set.ofList

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
                let facts = World.roomOf world room
                let remembered = Map.tryFind room world.Sightings

                if List.contains room bootstrap then
                    room, borrowed (Map.tryFind room stages) facts, remembered
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
                    // No memory, as a transit room keeps none. Ahead of the
                    // outposts: a Claim outpost harassed until its Claim lands
                    // (W17S25) works its declared controller and rock, laid in
                    // below, and nothing the cut takes out.
                    | Some enemy -> room, harassing enemy facts, None
                    | None -> room, facts, remembered)

        let worked = narrowed |> List.map (fun (room, facts, _) -> room, facts)

        // This colony's bodies, and the names to cut its geometry by, so the
        // fleet and the layers' occupants cannot disagree.
        let mine =
            world.Creeps
            |> List.filter (fun creep -> Map.tryFind creep.Info.Name holders = Some home)

        let names = mine |> List.map (fun creep -> creep.Info.Name) |> Set.ofList

        // The rooms whose safe mode this colony fires (#449): its home, and
        // each child it raises that has no spawn standing yet.
        let nurseries =
            bootstrap |> List.filter (fun room -> Map.tryFind room stages = Some Nursery)

        let fired = home :: nurseries

        let collected (select: RoomFacts -> 'a list) = worked |> List.collect (snd >> select)

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
            // Every worked room's sources but a bootstrapped child's (#192),
            // with the declared outpost rocks laid in beside them.
            Sources =
                collected (fun facts -> facts.Sources) |> Outpost.pooledSources scanned outposts
            Controller = homeFacts.Controller
            RoomControl = control
            // The gate's third and fourth sets, verbatim (#333, #366). Not
            // narrowed to the declaration the way `Rechecked` is: that one
            // *admits* a look, while these only ever take a controller out of
            // a pool or put a Guard in for a declared room, and their readers
            // intersect them with the projected outposts anyway.
            HeldOutposts = gate.HeldOutposts
            ThreatenedOutposts = gate.ThreatenedOutposts
            Fought = gate.Fought
            ConstructionSites = collected (fun facts -> facts.ConstructionSites)
            Creeps = mine |> List.map (fun creep -> creep.Info)
            // An ally's creep is no hostile (#412): this is the one entry every
            // rule that fires, flees, stands down or counts a rival reads.
            Hostiles =
                collected (fun facts -> facts.Hostiles)
                |> List.filter (fun hostile -> not (Colony.isAlly hostile.Owner))
            InvaderCores = collected (fun facts -> facts.InvaderCores)
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
                    Cooldowns = mergedBy (fun facts -> facts.Cooldowns)
                    Owners = mergedBy (fun facts -> facts.Owners)
                    RivalRooms =
                        worked
                        |> List.map fst
                        |> List.filter (World.rivalHeld world)
                        |> Fresh.setOfSeq
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
            // projection does not hold.
            Errands = errands
            Dismantles =
                (scan.Salvage
                 |> List.collect (fun room ->
                     World.roomOf world room |> salvageTargets |> Set.toList))
                @ (scan.Harass
                   |> List.collect (fun h ->
                       World.roomOf world h.RoomName |> harassTargets h.Enemy |> Set.toList))
            Harass = scan.Harass
            HarassCast = scan.Cast |> List.map (fun h -> h.RoomName) |> Set.ofList
            HarassFloors =
                scan.Harass
                |> List.choose (fun h ->
                    Map.tryFind h.RoomName casting.Floors
                    |> Option.map (fun blocks -> h.RoomName, blocks))
                |> Map.ofList
            // The declaration, straight through: the room it names is not one
            // this colony projects (#349).
            Consignee = colony.Consignee
            Perimeter = colony.Perimeter
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
            LoadedTowers =
                bootstrap
                |> List.choose (fun room ->
                    match World.loadedTowers (World.roomOf world room) with
                    | 0 -> None
                    | towers -> Some(room, towers))
                |> Fresh.mapOfList
            SafeModeRunning = World.safeModeRunning world
            Crossed = transit
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
                |> Set.ofList
            Borrowed =
                {
                    Rooms = bootstrap
                    Defended = scan.Defended
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
        }

    /// `ofWorldRecalling` over a table of this call's own — the shape a test
    /// asks in, the way `Atlas.ofView` is `ofViewRecalling` over fresh tables.
    let ofWorld
        (tuning: Tuning)
        (colonies: Colony list)
        (gate: StandDown)
        (holders: Map<string, string>)
        (world: World)
        (colony: Colony)
        : ColonyView =
        ofWorldRecalling (JoinTable()) tuning colonies HarassCasting.none gate holders world colony
