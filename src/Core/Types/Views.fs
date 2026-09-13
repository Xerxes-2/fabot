/// The `ColonyView`: one colony's cut of the World (ADR 0052) — the rooms it
/// works, the bodies that are its own, its bank, and the cross-colony work it
/// may borrow. The only thing `decide` is ever handed.
[<AutoOpen>]
module Fabot.Core.Types.Views

/// The cross-colony work one colony may take this tick, named and bounded
/// (ADR 0052 decision 7). Borrowing is an explicit exception and never a
/// narrowed layer: what a [[mother colony]] may do in a child's room is
/// written down here, and everything else the room holds stays the child's.
type BorrowedWork =
    {
        /// The home rooms of the children this colony carries in its projection
        /// for a reason that is not mining them, and it is two reasons: the
        /// children it is **raising**, whose Upgrade and Build its bodies may
        /// cross for (ADR 0047 decision 4), and the children it has **lost**,
        /// whose controller is a [[claim]] to make. The two are disjoint by
        /// construction and narrow to the same three kinds, because a Claim
        /// asks for exactly what an Upgrade does. The view carries only those
        /// kinds for these rooms, so the mother pools no Harvest on the child's
        /// rock and hauls none of its energy home.
        Rooms: string list
    }

/// One colony's whole reading of this tick: its home room's projection, the
/// rooms it works beside it, the bodies it holds, the bank it casts from and
/// the explicit little it may take of its neighbours' (ADR 0052 decision 1).
type ColonyView =
    {
        Time: int
        /// This colony's spawns: the ones it casts from and anchors its
        /// Layout on. A spawn standing in another colony's home is that
        /// colony's, and whether one stands in a declared home reaches this
        /// colony as that room's [[stage]].
        Spawns: SpawnInfo list
        /// The bodies this colony has in its ovens this tick: its **home
        /// room's** `RoomFacts.Casting` alone, for the reason `Bank` is one
        /// account — a colony casts from the spawns of the room it banks in.
        /// Read by the casting cascade and by nothing else: a body in an oven
        /// stands on no tile, holds no Task and answers no Verdict (ADR 0026).
        Casting: BodyPart list list
        /// The **tunables** this colony decides under (ADR 0052 decision 5),
        /// arriving on the view like every other fact so that a rule reads its
        /// colony's own and a test moves one field instead of editing the rule.
        Tuning: Tuning
        /// The colony's bank: its **home room's** shared spawn-energy account,
        /// and no other room's (ADR 0052 decision 1). Every spawn it casts from
        /// stands in that room, so one account is the whole of what it can
        /// spend — and not a fold over the projected rooms, which would have
        /// read a child's 300 beside a mother's 1,800.
        Bank: RoomEnergy
        /// Energy-hungry structures in the home room (spawn, extension,
        /// tower), whether or not they currently have room.
        Refillables: RefillableInfo list
        /// The sources this colony **mines**: every room it works but the ones
        /// it merely [[bootstrap]]s, whose rocks are the child's own (ADR 0047
        /// decision 4), with every declared outpost rock beside them whether or
        /// not there is vision (`Outpost.pooledSources`, ADR 0041).
        Sources: SourceInfo list
        /// This colony's own controller — the one it upgrades, whose downgrade
        /// clock it runs against and whose safe mode it fires (ADR 0047
        /// decision 1). Never a child's, which reaches the pool as a target in
        /// a layer she projects. `None` where the projection cannot place it,
        /// which is ADR 0004's absence and not a state.
        Controller: ControllerInfo option
        /// Who holds each room this colony works and has vision in this tick,
        /// under that room's name — what a source's output per tick is priced
        /// from (ADR 0042), and the fact a rule reads to say whether a room is
        /// this colony's business at all. Absent for a room vision did not
        /// answer for, per-entry as every other absence is (ADR 0004). One
        /// entry can be a room the colony does **not** work: the [[stand-down]]
        /// gate re-admits a room it has latched to the scan for one tick, once
        /// a whole `Tuning.RivalRecheck` has passed since the last look into it
        /// (#165 as #275 measures the stride), and this is the whole of what
        /// such a look reads. Nothing else of that room is here — no layer, no
        /// rock, no Task — so every reader below finds it nowhere, which is why
        /// the look moves no decision and only the next [[raid log]] is any
        /// wiser for it.
        RoomControl: Map<string, RoomControlInfo>
        /// Our construction sites in every room this colony works and has
        /// vision in: the Build pool is this list one to one, so an outpost's
        /// site is a Task like the home room's, and a bootstrapped child's site
        /// is the second half of what a [[pioneer]] crosses for.
        ConstructionSites: ConstructionSiteInfo list
        /// The creeps this colony holds this tick: the ones it cast, plus the
        /// ones it has adopted, less the ones another colony has adopted from
        /// it (`World.creepColonies`, ADR 0047 decision 2). In the world's own
        /// order, so who holds a body does not move the Matcher's order.
        Creeps: CreepInfo list
        /// Hostile creeps standing in any room this colony works and has
        /// vision in, each under its own room's name (ADR 0033, #201).
        Hostiles: HostileInfo list
        /// The invader cores standing in the rooms this colony works and can
        /// see (ADR 0043). Its own list and not a widening of `Hostiles`: a
        /// raider is something a creep runs from this tick, a core is something
        /// a whole room is withheld from for thousands, and
        /// `FIND_HOSTILE_CREEPS` can never answer with a structure.
        InvaderCores: InvaderCoreInfo list
        /// This colony's spatial projection: the home room and every room it
        /// works beside it, in one projection (ADR 0041, ADR 0005). `RoomName`
        /// is the home room and the `Rooms` keys are the scan set, so the
        /// view's home and the rooms it works are read off the projection
        /// rather than stored a second time beside it. Always present, possibly
        /// empty — absence is per-entry, never per-projection (ADR 0004).
        Spatial: SpatialInfo
        /// Every home room a human has declared a colony for (`Colony.homes`,
        /// ADR 0047), this colony's own included and in declaration order. The
        /// **candidate colonies** are the ones nobody owns yet, and that second
        /// half is read off `RoomControl` in Core: which rooms a human means to
        /// own is declared, whether we own one is seen, and a view carries
        /// facts rather than conclusions.
        Declared: string list
        /// The [[errand]]s this colony runs this tick, after the one narrowing
        /// there is (`Errand.routable`, ADR 0060 decision 1): a room name and
        /// the engine id and tile of the one object out there we act on.
        ///
        /// Carried on the view and not re-read from the constant, for
        /// `Declared`'s own reason and one more of its own: the list here is
        /// the **refused ones removed**, so a rule that reads it can neither
        /// pool a Task nor hire a body for a room no chain of [[seam]]s
        /// reaches. Two rules read it — the [[reclaim]] Task's pool and the
        /// re-claimer's seat on the reserver row — and they are exactly the
        /// "no row hires for it except the ones the errand's own Tasks belong
        /// to" that ADR 0060 decision 1 states and `Errand.place`'s kind-less
        /// target enforces from the other side: nothing here is found by
        /// sweeping a kind, so nothing else can find it at all.
        ///
        /// Empty for every colony that declares none, which is two of the three
        /// today: the reactor is five and six crossings from W12S28 and W13S28,
        /// outside `Tuning.MaxHops`, and a room they cannot price is a room they
        /// do not project.
        Errands: Errand list
        /// The [[stage]] of every room that is a colony of ours this tick
        /// (`World.stages`, ADR 0052 decision 3) — this colony's own and its
        /// children's alike, the same map handed to every colony because a
        /// stage is a fact about a room and not about who is looking.
        Stages: Map<string, ColonyStage>
        /// Where **other colonies'** creeps stand in the rooms this colony
        /// works, each tile carrying its room (ADR 0052 decisions 1 and 2): the
        /// bodies this colony does not hold and cannot move — in no `Creeps`
        /// list of hers, on no tile of her layers, and in nobody's Task pool
        /// but their own colony's.
        Foreign: Set<RoomPos>
        /// What this colony may take of a neighbour's, explicitly and
        /// bounded (ADR 0052 decision 7): today the Upgrade and the Build
        /// of a child it is still raising (ADR 0047 decision 4).
        Borrowed: BorrowedWork
        /// The declarations this colony's constant names that no chain of
        /// [[seam]]s joins to its home, and that it therefore **refuses**
        /// (`Outpost.refused`, `Errand.refused`, #243, ADR 0060 decision 1):
        /// nothing in them can be priced, walked to or worked, and they are out
        /// of the scan set rather than in it unworkable. Carried on the view
        /// because the refusal has to be *said*: it is the colony's own reading
        /// of its declaration, it reaches the operator on the [[layout record]]
        /// beside the plan's other losses, and the silence it replaces is what
        /// #243 was filed for. Empty is the healthy answer and rides here all
        /// the same, as the Layout's own loss lists do (ADR 0035).
        ///
        /// Each entry carries the **kind** it was declared as and not the room
        /// name alone, because there are two kinds now and a reader told only
        /// the room has to guess which list to go and look at — and because the
        /// two failures are not the same size, which `RefusedDeclaration` sizes
        /// once and this does not restate.
        Refused: RefusedDeclaration list
        /// What each room this colony **works** was last seen to carry (#151):
        /// the world's sightings, narrowed to the scan set. The narrowing is
        /// the rule and not housekeeping — a room a [[stand-down]] withholds
        /// leaves the scan set (ADR 0043) and leaves this map with it, so the
        /// withdrawal that ADR spells through `task-gone` keeps working
        /// unchanged. A withheld room is one the colony stops holding
        /// assignments in; a dark one is a room it is still working and
        /// cannot see this tick.
        ///
        /// Narrowed once more inside that set, by the same rule (#271): a room
        /// this colony only **crosses** carries no memory either. Its work is
        /// taken out of the projection where the facts are cut
        /// (`transiting`), and a memory of work the projection has just
        /// refused would reach the grace anyway — which is how a [[stand-down]]
        /// on a room a chain runs *through* used to hold a hauler to a Withdraw
        /// it had already withdrawn from.
        Sightings: Map<string, RoomSighting>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ColonyView =
    /// What a colony may see of a room it carries for a child of its own: the
    /// controller its workers upgrade — or, where the child has been lost,
    /// [[claim]] — the sites they build, and the spawn, not a target of hers at
    /// all but the tile her [[pioneer]]s walk up to and the structure that says
    /// a colony lives here (ADR 0047 decision 4). Both halves of
    /// `BorrowedWork.Rooms` narrow through this one filter: a Claim asks for
    /// exactly what an Upgrade does. Beside the kinds, at most one store: the
    /// [[ferry]]'s sink.
    let private borrowable (kind: TargetKind) =
        match kind with
        | Controller
        | Site _
        | Structure BuiltKind.Spawn -> true
        | Source
        // A pile of either resource: a child's floor is the child's, and the
        // season's ore lying in her room is hers as much as her deposit is.
        | Dropped _
        | Tombstone
        // A child's deposit is the child's: the mother's [[pioneer]]s build
        // and upgrade out there and mine nothing, so a mineral of the room
        // she is raising is no more borrowable than its sources are.
        | Mineral
        | Structure _ -> false

    /// One bootstrapped room's facts, cut down to the borrowed work (ADR 0052
    /// decision 7). Taken off the whole facts rather than gated when the world
    /// is read: the world reads a room once for everybody, so what this chooses
    /// is not what to *read* but what this colony may **carry**. What is left
    /// is exactly ADR 0004's per-entry absence — the shape a room with no
    /// vision arrives in — so every rule downstream already answers correctly
    /// for it. The geometry is kept whole, being what the mother's workers walk
    /// over. The room's hits go, so no Repair of the child's reaches her pool,
    /// and its sources go with them. Of its stores at most one survives: **the
    /// child's upgrade buffer, and no other store of its** — a built container
    /// inside the child's own controller's Upgrade area and on none of its
    /// Seats. That is what a [[ferry]] fills, so the mother has to see how much
    /// room is left in it; a source container of the child's carried here would
    /// be a Withdraw in her pool and the child's income hauled across the Seam.
    let private ferrySink (stage: ColonyStage option) (facts: RoomFacts) : Set<string> =
        let placed = facts.Layer.TargetPositions
        let tileOf id = Map.tryFind id placed

        let idsOfKind = SpatialInfo.idsOfKindIn facts.TargetKinds

        match stage, idsOfKind Controller |> List.tryPick tileOf with
        | Some Bootstrapping, Some controller ->
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

    /// The narrowing itself. The room's **memory** is not cut with it and needs
    /// no cutting (#271): a room reaches this arm only through `Colony.bootstrapping`
    /// or `Colony.reclaiming`, both of which read the room's control entry, and
    /// a control entry is vision's — so a bootstrapped room is a room we can
    /// see this tick, its sighting is this tick's, and the grace (which asks
    /// only about rooms gone **dark**) never reads it. A child's room that does
    /// go dark is not narrowed here at all: it loses its [[stage]] with its
    /// control entry and leaves the scan set outright, taking its sighting with
    /// it.
    let private borrowed (stage: ColonyStage option) (facts: RoomFacts) : RoomFacts =
        let sink = ferrySink stage facts

        let kinds =
            facts.TargetKinds
            |> Map.filter (fun id kind -> borrowable kind || Set.contains id sink)

        { facts with
            Layer =
                { facts.Layer with
                    TargetPositions =
                        facts.Layer.TargetPositions
                        |> Map.filter (fun id _ -> Map.containsKey id kinds)
                }
            TargetKinds = kinds
            Hits = Map.empty
            Stores = facts.Stores |> Map.filter (fun id _ -> Set.contains id sink)
            // Neither survives the cut, and the [[ferry]]'s exemption does not
            // reach them (ADR 0057): a ferry carries energy, so no Thorium of
            // the child's is a fact the mother may act on, and the deposit's own
            // amount would otherwise ride here under an id `borrowable` has just
            // dropped — a fact with no target, which is the shape ADR 0004
            // forbids. The child's extractor cooldown goes for the same reason:
            // the miner that reads it is hers.
            Thorium = Map.empty
            Cooldowns = Map.empty
            // And whose the child's objects are (#318), for the same reason
            // one rung up: what a mother may *act* on in a child's room is the
            // explicit list `borrowable` holds, and no act of hers turns on an
            // object's owner out there.
            Owners = Map.empty
            Sources = []
        }

    /// A **transit** room's facts: the ground a chain of [[seam]]s crosses and
    /// the bodies standing on it, and not one thing a rule could work (ADR
    /// 0058, #286). ADR 0058 decision 2 promised exactly this — *"a transit
    /// room enters the projection carrying terrain and a border ring and
    /// nothing else"* — and left it to `furnitureOf`, which reads a
    /// declaration that names none. That holds only while the room is
    /// **blind**: the tick one of our bodies walks through it, `World.seenFacts`
    /// files the room's real sources, its real controller and whatever stands
    /// in it, and from there nothing tells it apart from a declared outpost.
    ///
    /// Live on 2026-09-10 that cost a colony its child: W14S28, the room
    /// between W13S28 and the nursery it was raising, was reserved (a reserver
    /// hired against a controller **no declaration names**), anchored, given a
    /// container and hauled from — 2,020 of a 2,540-energy haul demand, on a
    /// single-spawn colony whose worker row stood at zero of five while the
    /// nursery's spawn site sat at 1,668 of 15,000. Withdrawing the
    /// declaration did not stop it (the room is a transit room either way) and
    /// killing the bodies did not either: the rows re-hired within seven
    /// hundred ticks, because the pioneers crossing the room are themselves the
    /// vision.
    ///
    /// So the promise is kept here, where the room's facts enter one colony's
    /// view, and it is kept **per colony**: the same room is a transit room for
    /// the mother and an ordinary neighbour for whoever declares it. What
    /// survives is what is not *work* — the layer's terrain and its occupants,
    /// the border ring a Seam is read off, the hostiles ADR 0033's Reach and
    /// Flee owe an answer about, and the room's ownership. What goes is every
    /// id a Task could name — and, since #248, the one placement fact that
    /// carries no id at all, the tiles a rival's construction sites hold: the
    /// test is whether the field is *work*, not whether something can be named
    /// off it. The room's **memory** goes with its work and for the same reason
    /// (#271) — an id this colony may not work is not one it may be held to
    /// while the room is dark — but it goes one level up, at the walk in
    /// `ofWorld`, because a sighting is the world's and not a field of the
    /// facts: this function is handed no memory to drop.
    let private transiting (facts: RoomFacts) : RoomFacts =
        { facts with
            Layer =
                { facts.Layer with
                    TargetPositions = Map.empty
                    // A rival's site is a placement fact and nothing else
                    // (#248), and nothing is ever placed in a transit room:
                    // carried here it would be work of a sort after all — the
                    // census signature would sign it, and a neighbour building
                    // in a room we merely walk through would throw the plan
                    // memo away.
                    RivalSites = Set.empty
                }
            TargetKinds = Map.empty
            Hits = Map.empty
            Stores = Map.empty
            // A deposit's remaining Thorium, a store's Thorium and an
            // extractor's cooldown are work facts by the test above — what a
            // quota reads and what an Emitter gates on (ADR 0057) — so they go
            // out with the stores they stand beside.
            Thorium = Map.empty
            Cooldowns = Map.empty
            // And whose an object standing here is (#318), by the same test:
            // it is what an Emitter gates an act on, so it is work. A room we
            // merely cross holds nothing of ours to act on, and the ownership
            // that *does* survive a crossing is the **room**'s, which rides on
            // `Control` and is deliberately kept.
            Owners = Map.empty
            Controller = None
            Refillables = []
            Sources = []
            ConstructionSites = []
        }

    /// An **errand** room's facts: a [[transit room]]'s ground and bodies, and
    /// beside them the one thing a declaration out there names (ADR 0060
    /// decision 1). More than a transit room and less than an [[outpost]], and
    /// this is where both halves of that are said.
    ///
    /// **More**, because a declared room's vision is work and the one target it
    /// names is the work: whatever the shell filed under that id — its store,
    /// its Thorium, its cooldown — rides on the view rather than being cut away
    /// with the rest of the room. What the declaration itself supplies is laid
    /// over this afterwards (`Errand.place`), so the target is placed whether or
    /// not there is vision, and everything about it that *changes* is absent
    /// entry by entry where there is none (ADR 0004).
    ///
    /// **One of those facts is live and two are not** (#318). `Owners` is: the
    /// shell sweeps `FIND_REACTORS` and files whose the reactor is, so "the body
    /// standing there is the colony's only eye on it" is now a fact and not an
    /// intention — a relay that gaps drops the entry and the act that reads it
    /// treats the absence as *not ours*. What still does not ride is the
    /// reactor's **store**: `World.seenFacts` fills `Stores` and `Thorium` off
    /// `isStored`, which asks a `BuiltKind`, and a reactor has none — the mod
    /// registers it as a **custom object**, so it reaches no `FIND_STRUCTURES`
    /// sweep at all and arrives only through `FIND_REACTORS`. Nor does
    /// `continuousWork`, which is not a word this tree knows. Both wait on the
    /// ticket whose decision reads them — the courier's, and the `reactor` leaf's
    /// — which is ADR 0007's rule and the reason this line grew one map and not
    /// three.
    ///
    /// **Less**, because nothing else in that room is work, however much vision
    /// we pay for: no source of it is pooled, no controller of it is Reserved,
    /// no site of it is built. The errand in force names a sector centre with
    /// three sources, an owner-less extractor and no controller at all, and
    /// what this narrowing prevents is #286's live failure one room further out
    /// — a row hiring against furniture no declaration names, because our own
    /// bodies walking through were the vision that filed it.
    ///
    /// The kind census stays **empty**, which is the narrowing stated in the
    /// data rather than as a rule each pool has to remember: every pool is
    /// built by sweeping `TargetKinds`, so an id that is placed and classified
    /// by nothing is priceable by a Task that names it — which the errand's own
    /// Tasks do — and enumerable by no pool at all. The room's hits go with the
    /// kinds and for the same reason: a Repair is pooled off a hit count, and
    /// an errand's target is not a thing this colony repairs.
    let private erranding (targets: Set<string>) (facts: RoomFacts) : RoomFacts =
        let crossed = transiting facts

        let named map =
            map |> Map.filter (fun id _ -> Set.contains id targets)

        { crossed with
            Layer =
                { crossed.Layer with
                    TargetPositions = named facts.Layer.TargetPositions
                }
            Stores = named facts.Stores
            Thorium = named facts.Thorium
            Cooldowns = named facts.Cooldowns
            Owners = named facts.Owners
        }

    /// One colony's view of this tick (ADR 0052 decision 1): the rooms it works
    /// cut out of the `World`, the bodies it holds cut out of the world's
    /// creeps, its own bank and controller, and the explicit little it may take
    /// of a child's. **Pure, and that is the point of it** (ADR 0052 decision
    /// 8): the shell reads the engine once (`World.ofGame`) and every rule
    /// about which rooms a colony works, which creeps are its own and what it
    /// may borrow is here, where a test can hand it a two-colony world and read
    /// the answer back. Five facts are handed in and none is decided here: the
    /// **tunables** (decision 5), the **declaration**, the **gate** the
    /// [[stand-down]] derives off the previous tick's [[raid log]] (ADR 0043 —
    /// Memory's answer, not the world's), the **holders** `World.creepColonies`
    /// cut over every living colony's scan set at once, and the **world**
    /// itself.
    let ofWorld
        (tuning: Tuning)
        (colonies: Colony list)
        (gate: StandDown)
        (holders: Map<string, string>)
        (world: World)
        (colony: Colony)
        : ColonyView =
        let home = colony.Home
        let stages = World.stages tuning colonies world

        // The declaration's narrowings and their union, off the one
        // derivation the creep adoption reads too (`World.scanOf`). Written
        // here a second time it would be a second answer free to disagree.
        let scan =
            World.scanOf
                tuning
                stages
                (World.unownedHomes colonies world)
                colonies
                gate.Shut
                world
                colony

        // Named out of the record once rather than read through `scan.` at each
        // of the dozen sites below, which is the shape this block had while it
        // was a tuple; what the record buys is that the names are now the
        // compiler's to check rather than a position's to lose.
        let outposts = scan.Outposts
        let errands = scan.Errands
        let bootstrap = scan.Borrowed
        let scanned = scan.Scanned

        // The errand rooms as a set, and the `elif` chain below reads it after
        // the bootstrap and the transit branches and before the worked ones.
        // **That ordering presumes the two declaration lists name disjoint
        // rooms**, and they are disjoint by the types' own definitions rather
        // than by luck: an `Outpost` carries a mandatory `Controller` and an
        // `Errand` exists for the room that has none. A human who wrote one
        // room into both lists would be writing a contradiction, and the branch
        // that won would narrow the room to the errand's one target — taking
        // the outpost's own container, store and site out of the projection
        // while the reserver row went on hiring for it, which is #243's and
        // #286's silence in reverse. It is caught where a contradiction in a
        // human's constant belongs: red before deploy, over `Colony.declared`
        // (`ViewTests`, "no room is declared as both"). Not refused at runtime
        // — `Refused` means "no chain of Seams reaches it" and would say the
        // wrong thing — and not unioned, which would accept the contradiction
        // and leave nothing to notice it.
        let errandRooms = errands |> List.map (fun errand -> errand.RoomName) |> Set.ofList

        // The rooms in the set for the walk alone: everything the union added
        // that is neither this colony's home, nor a room it works, nor a room
        // it runs an errand in, nor a room it raises (`Colony.roomsProjected`,
        // ADR 0058, ADR 0060). Derived by subtraction rather than returned
        // beside the set, because the union is the one place that rule is
        // spelled and a second derivation would be a second answer free to
        // disagree.
        let transit =
            scanned
            |> List.filter (fun room ->
                room <> home
                && not (List.contains room bootstrap)
                && not (Set.contains room errandRooms)
                && not (outposts |> List.exists (fun outpost -> outpost.RoomName = room)))
            |> Set.ofList

        // The scan set with everything the world holds about each room beside
        // it, in scan order: this tick's facts — a room the world holds nothing
        // for reads empty (ADR 0004), and a room this colony only bootstraps
        // reads the borrowed work alone — and the memory of the room the vision
        // grace reads (#151).
        //
        // A room the colony only **crosses** keeps neither, and that is one
        // rule and not two (#271): `transiting` takes its every id out of the
        // projection, and a memory of those ids would put them back the tick
        // the room went dark, which is the only tick the grace looks at. The
        // live case is a [[stand-down]] on a room a chain runs *through* — the
        // gate takes the room out of `outposts` and the chain keeps it in
        // `scanned`, so it lands here as a transit room with the census of the
        // outpost it was still in it, and its Withdraw went on holding a hauler
        // the withdrawal had released for a whole `Tuning.VisionGrace`.
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
                    // The errand room's memory is narrowed by the same rule its
                    // facts are (#271, ADR 0060 decision 1): the ids the
                    // declaration names are the ones the grace may hold a body
                    // to while the room is dark, and an id this colony may not
                    // work is not one it may be held to either. A room whose
                    // errands name nothing it ever saw remembers an empty set,
                    // which is the answer a transit room's `None` gives one
                    // level down.
                    let targets = Errand.targetsIn room errands

                    room,
                    erranding targets facts,
                    remembered
                    |> Option.map (fun sighting ->
                        { sighting with
                            Targets = Set.intersect sighting.Targets targets
                        })
                else
                    room, facts, remembered)

        let worked = narrowed |> List.map (fun (room, facts, _) -> room, facts)

        // This colony's bodies, and the names to cut its geometry by: a
        // colony's fleet and its layers' occupants are one set, so the two
        // cannot disagree about who is standing where.
        let mine =
            world.Creeps
            |> List.filter (fun creep -> Map.tryFind creep.Info.Name holders = Some home)

        let names = mine |> List.map (fun creep -> creep.Info.Name) |> Set.ofList

        // The three id-keyed tables, merged flat across the worked rooms,
        // because an object id is already unique across the world (ADR 0041).
        // Deterministic under a collision that cannot happen: the fold walks
        // the scan set in order, and one object stands in one room.
        // The list-valued halves of the same merge: every worked room's, in
        // the scan set's order.
        let collected (select: RoomFacts -> 'a list) = worked |> List.collect (snd >> select)

        let mergedBy (select: RoomFacts -> Map<string, 'v>) =
            (Map.empty, worked)
            ||> List.fold (fun acc (_, facts) ->
                (acc, select facts) ||> Map.fold (fun acc id value -> Map.add id value acc))

        let homeFacts = World.roomOf world home

        // The scan set's own control entries, and beside them the one look
        // #165 buys a room the gate has latched on another player's ownership:
        // whatever vision answered for that room this tick, and nothing else it
        // holds. This is the whole of "re-admitted to the scan set only" — the
        // room contributes no furniture, no rock, no hostile and no layer, so
        // nothing pools there and no quota counts it while the look happens,
        // and ADR 0043's withdrawal stands through the tick that questions it.
        // Read off the **declaration** and never off the latch's own room
        // names: a hand-edited `rivalHeld` leaf is a room name a human wrote,
        // and the only rooms this colony may look into are the ones it
        // declared. A room vision did not answer for adds no entry at all,
        // which is ADR 0004's absence and the reason the latch survives every
        // recheck the colony is blind on.
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
            // Every worked room's sources but a bootstrapped child's, whose
            // rocks are the child's to pool (ADR 0047 decision 4, #192),
            // with the declared outpost rocks laid in beside them whether
            // or not there is vision (ADR 0041).
            Sources =
                collected (fun facts -> facts.Sources) |> Outpost.pooledSources scanned outposts
            Controller = homeFacts.Controller
            RoomControl = control
            ConstructionSites = collected (fun facts -> facts.ConstructionSites)
            Creeps = mine |> List.map (fun creep -> creep.Info)
            Hostiles = collected (fun facts -> facts.Hostiles)
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
                }
                // The declared furniture goes in last, over the whole
                // assembled projection rather than room by room inside it
                // (`Outpost.place`, ADR 0041): a source's and a
                // controller's id and tile do not wait for vision.
                |> Outpost.place outposts
                // And the errands' one target apiece, by the same rule and over
                // the same assembled projection (`Errand.place`, ADR 0060
                // decision 1): a courier has to hold `Deliver of reactorId`
                // before any body of ours has stood in that room.
                |> Errand.place errands
            Declared = Colony.homes colonies
            // The scan set's own errand list and never `colony.Errands` read a
            // second time (ADR 0060 decision 1): a refused errand has left the
            // scan set, so nothing of it is projected — and a rule that pooled
            // a Task off the declaration instead would name a target in a room
            // the projection does not hold, which is #243's silence with a
            // bigger body standing beside the spawn.
            Errands = errands
            Stages = stages
            // The bodies in these rooms that are not this colony's, each
            // tile joined to the room it stands in (ADR 0052 decision 2): a
            // room with none contributes nothing (ADR 0004), an empty set.
            Foreign =
                worked
                |> List.collect (fun (room, facts) ->
                    facts.Layer.CreepPositions
                    |> Map.toList
                    |> List.filter (fun (name, _) -> not (Set.contains name names))
                    |> List.map (snd >> RoomPos.at room))
                |> Set.ofList
            Borrowed = { Rooms = bootstrap }
            // Read off the whole declaration and not off `scanned`, which is
            // where these rooms have just been subtracted: what the channel
            // must name is the room a human declared and this colony cannot
            // work, and by the time the scan set is cut the name is gone
            // (#243).
            // Both declaration kinds, each named as what it was declared as
            // (ADR 0060 decision 1): the outposts a human wrote first, then the
            // errands beside them.
            // Asked over the **masked** border rings, which is the layer the
            // scan set's own narrowing was asked over (`World.scanOf`, ADR 0060
            // decision 2): a refusal read off raw terrain would name a room the
            // chain admits, or keep quiet about one it does not.
            Refused =
                let reaches = World.linked (Tuning.keeperMargin tuning) world

                Outpost.refused reaches tuning.MaxHops home colony.Outposts
                @ Errand.refused reaches tuning.MaxHops home colony.Errands
            // The world's memory of these rooms and of no others (#151):
            // narrowed by the scan set the [[stand-down]] gate has already
            // cut, so a withheld room's remembered census cannot hold a
            // creep to a Task in a room the colony has withdrawn from. Read
            // off the same walk the facts are, because the scan set alone did
            // not deliver that (#271): a withheld room the chain to a further
            // outpost still crosses stays in the set, and the narrowing that
            // takes its work is the walk's.
            Sightings =
                narrowed
                |> List.choose (fun (room, _, remembered) ->
                    remembered |> Option.map (fun sighting -> room, sighting))
                |> Map.ofList
        }
