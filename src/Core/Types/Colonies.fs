/// A colony and what it declares: the outposts it works (ADR 0042), the
/// stand-down gate that silences one (ADR 0043), and the stage it stands at
/// (ADR 0052). The declaration, never the tick.
[<AutoOpen>]
module Fabot.Core.Types.Colonies

/// One outpost: a room this colony mines and does not own, inside the hop
/// budget its home reaches over a chain of Seams (`Tuning.MaxHops`, ADR 0058;
/// it was a *neighbouring* room until that ADR, and most of them still are).
/// Declared, never discovered (ADR 0041) — a constant a human moves in a
/// commit, exactly as the Layout's horizon is (ADR 0039) — because every "the
/// first creep to walk in writes it down" scheme has to answer what sent the
/// first creep, and answering it means inventing scouting and room intel. What
/// is declared is exactly what vision cannot be waited for: the room's name,
/// and the id and tile of each source and of the controller. Everything that
/// actually changes is read off the projection where there is vision and is
/// absent entry by entry where there is none (ADR 0004). The ids are the
/// **engine's own**: a declaration written in readable short names would match
/// nothing on a live server, and would do it in silence.
type Outpost =
    {
        RoomName: string
        /// The room's sources, each under the id the engine knows it by, and
        /// each tile joined to the room it is a tile of (ADR 0052 decision 2).
        Sources: (string * RoomPos) list
        /// The room's controller, whose reservation is what doubles those
        /// sources (ADR 0042). Not optional: an unreserved source is worth
        /// half, so a room with no controller to reserve — a sector centre
        /// or a Source Keeper room — is not a candidate outpost at all.
        Controller: string * RoomPos
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Outpost =
    /// The declarations the colony works this tick: the declared list, less
    /// every room a [[stand-down]] is withholding (ADR 0043). The gate, and the
    /// one place *that* gate narrows the set — `World.scanOf` narrows it once
    /// more beside this, on the declaration's own geometry (`withinHopBudget`,
    /// #243, ADR 0058), and the two are one clause apiece there. The gate's other half
    /// (`StandDown.Rechecked`, #165) never reaches here: a room it re-admits to
    /// the scan is still withheld from the work, so a room this drops stays
    /// dropped whatever tick the recheck falls on.
    let worked (shut: Set<string>) (outposts: Outpost list) : Outpost list =
        outposts
        |> List.filter (fun outpost -> not (Set.contains outpost.RoomName shut))

    /// Whether a declared outpost is one its home can work **at all**: the two
    /// rooms are inside the hop budget, so a chain of [[seam]]s could join them
    /// (`RoomName.hopsBetween`, ADR 0058). Not a gate that opens and shuts like
    /// the [[stand-down]]'s — it is a fact about the declaration a human wrote
    /// and the constant they wrote it under, and it answers the same on every
    /// tick of that declaration's life.
    ///
    /// Read off the **names**, as `RoomName.neighbouring` was before it and for
    /// the same reason: this is asked while the scan set is being built, which
    /// is before there is a projection to read terrain off. So it answers
    /// whether the declaration is *shaped* like one a route could join, never
    /// whether one does — a room inside the budget that every chain to is
    /// walled is priced at `None` by `Atlas.route` and is the case #259 is
    /// open on, one hop out and now three.
    let withinHopBudget (maxHops: int) (home: string) (outpost: Outpost) : bool =
        RoomName.hopsBetween home outpost.RoomName
        |> Option.exists (fun hops -> hops >= 1 && hops <= maxHops)

    /// Whether a chain actually joins the two — the same question one room
    /// further down (#259, ADR 0058). `withinHopBudget` above answers off the
    /// names and so can be asked of a declaration with no terrain read at all;
    /// this one asks `linked` per border and so answers whether a creep could
    /// really walk there. The budget is inside it: `RoomName.routeBy` searches
    /// no deeper, so a room this accepts is one `Atlas.route` will price.
    ///
    /// The two are not the same test and the difference is the whole of #259: a
    /// room three hops out whose every chain the engine walled is *shaped* like
    /// a declaration and is not one, and refusing it on the names alone would
    /// leave it projected, pooled and hired for by a row that hires per
    /// declared outpost — #243's silent failure, one budget further out.
    let routable
        (linked: string -> string -> bool)
        (maxHops: int)
        (home: string)
        (outpost: Outpost)
        : bool =
        withinHopBudget maxHops home outpost
        && RoomName.routeBy linked maxHops home outpost.RoomName |> Option.isSome

    /// The declared outposts outside the hop budget, by name (#243, ADR 0058).
    /// A walk is priced over a chain of at most `Tuning.MaxHops` Seams, each
    /// leg a flood that never leaves its room, so a room further out than that
    /// is not a badly-priced outpost but an unpriceable one: `pricedAcross` and
    /// `haulRoundTripTicks` answer `None` for every target in it, and by ADR
    /// 0004 unpriceable geometry never counts against a Task. Worked anyway,
    /// such a room is projected, pooled and hired for — ADR 0042's reserver row
    /// hires one body per declared outpost — and every body bought for it
    /// stands beside the spawn for its whole life with nothing to say why. So
    /// the declaration is **refused** here rather than accepted and never
    /// worked, and the refusal is named: `ColonyView.Refused` carries it to the
    /// colony's [[layout record]], and the test over `Colony.declared` is what
    /// makes a human's slip red before it is deployed. Read off the whole
    /// declaration and not off `worked`'s survivors: a room this refuses is
    /// wrong whatever the stand-down is doing about it this tick.
    let refused
        (linked: string -> string -> bool)
        (maxHops: int)
        (home: string)
        (outposts: Outpost list)
        : string list =
        outposts
        |> List.filter (routable linked maxHops home >> not)
        |> List.map (fun outpost -> outpost.RoomName)

    /// The rooms the shell projects this tick: the home room, and every
    /// declared outpost beside it (ADR 0041). One projection covering several
    /// rooms, never a second one (ADR 0005) — the union is taken here so the
    /// rule has one statement, and the outposts are handed in rather than read
    /// off the constant, so the stand-down gate (ADR 0043) has exactly one
    /// place to narrow the set.
    let roomsProjected (outposts: Outpost list) (home: string) : string list =
        home
        :: (outposts
            |> List.collect (fun outpost ->
                // The outpost, and every room a shortest walk to it could
                // cross (ADR 0058). A **transit** room is projected for its
                // terrain and for nothing else: no furniture is laid in it
                // (`furnitureOf` reads the declaration, which names none),
                // nothing is pooled there and no row hires for it, so what it
                // adds is a walking grid and a border ring — which is exactly
                // what `Atlas.route` needs to find a chain through it, and
                // what the chain's own flood needs to cross it. For a
                // one-hop outpost this is empty and the set is the one every
                // tick before ADR 0058 projected.
                outpost.RoomName :: RoomName.transitBetween home outpost.RoomName))
        |> List.distinct

    /// One declaration as projection entries: the controller and then the
    /// sources in their declared order, each id paired with the tile the
    /// declaration names and the kind it is. Position and kind are read off one
    /// list rather than two, so the folds below cannot place an id the kind
    /// census misses or classify one nothing places; the Harvest pool reads it
    /// too (`pooledSources`), because a rock nothing places must not be pooled.
    /// A declared tile filed under another room name is **dropped** here rather
    /// than written onto this room's coordinate (ADR 0052 decision 2).
    let private furnitureOf (outpost: Outpost) : (string * Pos * TargetKind) list =
        (fst outpost.Controller, snd outpost.Controller, Controller)
        :: (outpost.Sources |> List.map (fun (id, tile) -> id, tile, Source))
        |> List.filter (fun (_, tile, _) -> tile.Room = outpost.RoomName)
        |> List.map (fun (id, tile, kind) -> id, RoomPos.pos tile, kind)

    /// The declared furniture, laid into the projection: for every scanned
    /// outpost, its sources and its controller at the tiles and under the ids
    /// the declaration names — whether or not the colony has vision there. This
    /// is the half of ADR 0041 that vision may not gate, and the deadlock the
    /// ADR breaks: a source's position needs vision, vision needs a creep
    /// there, a creep goes there because a Task exists, and the Task exists
    /// because the source is in the projection. A declared fact is in the
    /// projection because a human wrote it down; only what actually changes
    /// waits for vision (ADR 0004). Vision wins every entry it holds: the
    /// declaration is laid *under* what the room's `find` families answered.
    /// The two agree by construction — the ids are the engine's own and a rock
    /// does not move — so this decides which truth is authoritative rather than
    /// resolving a conflict. The controller's tile joins `Obstacles`, so a
    /// reserver stands beside it and never on it.
    let place (outposts: Outpost list) (spatial: SpatialInfo) : SpatialInfo =
        (spatial, outposts)
        ||> List.fold (fun spatial outpost ->
            match Map.tryFind outpost.RoomName spatial.Rooms with
            | None -> spatial
            | Some layer ->
                let furniture = furnitureOf outpost

                { spatial with
                    Rooms =
                        Map.add
                            outpost.RoomName
                            { layer with
                                TargetPositions =
                                    (layer.TargetPositions, furniture)
                                    ||> List.fold (fun placed (id, pos, _) ->
                                        if Map.containsKey id placed then
                                            placed
                                        else
                                            Map.add id pos placed)
                                // The controller's own tile, from the
                                // furniture already checked against this
                                // room: a declaration whose controller names
                                // another room blocks no tile here.
                                Obstacles =
                                    (layer.Obstacles, furniture)
                                    ||> List.fold (fun blocked (_, pos, kind) ->
                                        if kind = Controller then Set.add pos blocked else blocked)
                            }
                            spatial.Rooms
                    TargetKinds =
                        (spatial.TargetKinds, furniture)
                        ||> List.fold (fun kinds (id, _, kind) ->
                            if Map.containsKey id kinds then
                                kinds
                            else
                                Map.add id kind kinds)
                })

    /// The sources the Harvest pool is built from: the ones vision answered
    /// with, and every declared outpost rock beside them. One pool ranked in
    /// one order (ADR 0041), so a rock the colony cannot see this tick is a
    /// Task all the same. Deduplicated by id with the seen list first, the
    /// engine's answer carrying this tick's restock. An unseen rock restocks in
    /// 0 ticks: ADR 0025's "holds energy" default. A restock is a *time* fact
    /// and the unknown one is not "for ever" — priced at 0 the source is judged
    /// at arrival like any other, and the Emitter's own gate withholds the dig
    /// from a rock that turns out empty. Scanned rooms only, and read off
    /// `furnitureOf` rather than off `outpost.Sources`, so the rocks are pooled
    /// exactly where the furniture is laid and a stand-down narrows both at
    /// once. A pool that took the declaration straight would name rocks nothing
    /// places — and an unplaced target prices at 0 (ADR 0004's escape), so it
    /// would *win* its tier.
    let pooledSources
        (rooms: string list)
        (outposts: Outpost list)
        (seen: SourceInfo list)
        : SourceInfo list =
        seen
        @ [
            for outpost in outposts do
                if List.contains outpost.RoomName rooms then
                    for id, _, kind in furnitureOf outpost do
                        if kind = Source then
                            { Id = id; TicksToRestock = 0 }
        ]
        |> List.distinctBy (fun source -> source.Id)

    /// The two rooms ADR 0042 measured, as the outposts they were declared
    /// as: the real-terrain fixtures (`RoomInvariantTests`) read the
    /// captures relative to W12S28 with both of them laid in. The ids and
    /// tiles are the engine's, pinned against the committed captures.
    let adr0042: Outpost list =
        [
            {
                RoomName = "W12S27"
                Sources = [ "6a8caabadd4872bccd3194a6", { Room = "W12S27"; X = 16; Y = 45 } ]
                Controller = "6a8caabadd4872bccd3194a5", { Room = "W12S27"; X = 37; Y = 43 }
            }
            {
                RoomName = "W13S28"
                Sources =
                    [
                        "6a8caaaddd4872bccd319362", { Room = "W13S28"; X = 16; Y = 7 }
                        "6a8caaaddd4872bccd319361", { Room = "W13S28"; X = 18; Y = 4 }
                    ]
                Controller = "6a8caaaddd4872bccd319363", { Room = "W13S28"; X = 24; Y = 17 }
            }
        ]

    /// W13S28's south outpost, declared 2026-09-07 off the remote survey
    /// (`docs/research/remote-candidates.md`): two sources, a 12-tile south
    /// Seam, the best net income of the five rooms a Seam reaches. The ids
    /// and tiles are the engine's, read the day it was declared.
    let w13s29: Outpost =
        {
            RoomName = "W13S29"
            Sources =
                [
                    "6a8caaaddd4872bccd319365", { Room = "W13S29"; X = 29; Y = 6 }
                    "6a8caaaddd4872bccd319366", { Room = "W13S29"; X = 14; Y = 29 }
                ]
            Controller = "6a8caaaddd4872bccd319367", { Room = "W13S29"; X = 15; Y = 41 }
        }

    /// W13S28's west outpost, declared 2026-09-10 off
    /// `docs/research/multihop-outposts.md`, which ranks it second of the
    /// six: one source, a 21-tile west Seam, and the lowest cost of any of
    /// them because the room was already in the projection as the transit
    /// room on the way to W15S28. It is also the room that keeps that chain
    /// warm — a declaration of its own rather than a room seen in passing,
    /// which is what it had become the day W15S28 was claimed. The ids and
    /// tiles are the engine's, read the day it was declared.
    let w14s28: Outpost =
        {
            RoomName = "W14S28"
            Sources = [ "6a8caaa1dd4872bccd3191f9", { Room = "W14S28"; X = 6; Y = 8 } ]
            Controller = "6a8caaa1dd4872bccd3191fa", { Room = "W14S28"; X = 22; Y = 15 }
        }

    /// The third colony's room, declared 2026-09-10 off
    /// `docs/research/third-colony.md`: two sources, a d3 Thorium deposit of
    /// 22,000, and — the reason it and not a nearer room — the one site left
    /// after the risk screen from which the sector Reactor is inside
    /// `Tuning.MaxHops`: three crossings by way of W15S27 and the Source
    /// Keeper room W15S26, against five from W13S28 and six from W12S28.
    /// W13S25 is nearer still at two and is **not** this room's rival on the
    /// budget: it borders W13S24, which an invader core holds and reserves, so
    /// what ruled it out is a raid a room at RCL1 cannot answer and never the
    /// arithmetic. It is **two** hops from its home, which is a
    /// declaration ADR 0058 made writable and #243 would have refused: the
    /// chain runs through W14S28, which enters the projection as a transit
    /// room carrying terrain and nothing else. Declared as W13S28's outpost
    /// and as a colony of its own on the same day, which is ADR 0047's
    /// candidate-colony arrangement — the controller the mother's pool offers
    /// is a Claim rather than a Reserve for exactly as long as the second
    /// entry stands beside the first. The ids and tiles are the engine's, read
    /// the day it was declared, and the source order is the capture's
    /// (`tests/Core.Tests/rooms/W15S28.room`).
    let w15s28: Outpost =
        {
            RoomName = "W15S28"
            Sources =
                [
                    "6a8caa95dd4872bccd319014", { Room = "W15S28"; X = 6; Y = 30 }
                    "6a8caa95dd4872bccd319013", { Room = "W15S28"; X = 10; Y = 19 }
                ]
            Controller = "6a8caa95dd4872bccd319015", { Room = "W15S28"; X = 25; Y = 31 }
        }

/// The [[stand-down]] gate's whole answer for one colony this tick (ADR 0043 as
/// #165 narrows it), derived once off that colony's [[raid log]]
/// (`Observe.standDown`) and handed to `ColonyView.ofWorld`: two sets rather
/// than one, because after #165 the gate has two strengths and not one. A room
/// is withdrawn from the work the colony does, and a room whose withdrawal
/// **latched** on another player's ownership is looked into all the same, once
/// in every `Tuning.RivalRecheck` ticks, so the conclusion that shut it can be
/// contradicted by the only thing that ever could — a tick with vision. One
/// record and not two derivations: the two answers are read off one log and one
/// tick, and split apart they would be free to disagree about which rooms the
/// colony has withdrawn from.
type StandDown =
    {
        /// Every room the gate withholds from the declaration this colony works
        /// (`Outpost.worked`): no Task pools there, no quota counts it and
        /// nothing walks toward it, because the room does not enter the
        /// [[spatial projection]] at all — the whole of "withdraw" in an
        /// architecture that recomputes every tick (ADR 0004, ADR 0043).
        Shut: Set<string>
        /// The rooms of `Shut` this tick takes one look into (#165): a
        /// **subset** of it and never a room leaving it. The look re-admits the
        /// room to the scan — the colony reads its controller, so the next
        /// [[raid log]] can drop a latch the rival has walked away from — and
        /// to nothing else: no furniture, no pooled rock, no Task and no quota
        /// row, which is what keeps ADR 0043's withdrawal in force on the very
        /// tick the gate is being questioned.
        Rechecked: Set<string>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StandDown =
    /// The open gate: nothing withheld and nothing to look into — what a colony
    /// with no [[raid log]] yet, and every colony on an ordinary tick, decides
    /// under.
    let none =
        {
            Shut = Set.empty
            Rechecked = Set.empty
        }

/// Where one colony stands in its life (ADR 0052 decision 3). Three answers to
/// one question — how much of its own economy a colony has bought yet — and the
/// one fact five rules read instead of a controller level apiece: whether it
/// places roads and keeps ramparts, whether its sites come before its
/// controller, whether a [[mother colony]] is still raising it.
type ColonyStage =
    /// Claimed, and no spawn of ours standing in it yet: a [[nursery]] — a
    /// colony by declaration and by ownership, and by nothing else it can do
    /// for itself. What ends it is a spawn (ADR 0047 decision 4).
    | Nursery
    /// Its own spawn standing and its controller still under
    /// `Tuning.BootstrapLevel`: running its own `decide`, casting its own
    /// bodies, and still being raised — the **bootstrap window**.
    | Bootstrapping
    /// At `Tuning.BootstrapLevel` or past it: the first tower and the tenth
    /// extension, a bank that casts a body which is not the 300-energy
    /// starter. The stage every rule written for the one home this bot grew
    /// up in was written at (ADR 0052).
    | Independent

/// One colony: a [[home room]] and the [[outpost]]s worked from it (ADR 0047).
/// The unit the whole decision layer is written in — one Atlas, one Layout, one
/// set of quotas, one Task pool — and so the unit a declaration is written in.
type Colony =
    {
        /// The room the colony is run from: the room its spawns stand in,
        /// its Layout is planned in, and its quotas are banked in.
        Home: string
        /// The rooms it mines but does not own. A **candidate colony**'s home
        /// appears here as well, in its *mother* colony's list, until the day
        /// it is independent: one room projected by two colonies at once is
        /// what the mother's outpost declaration already means (ADR 0047).
        Outposts: Outpost list
        /// The home room of the [[mother colony]] that raised this one, for as
        /// long as it is still being raised (ADR 0047 decision 4): the
        /// **bootstrap** window, from the day the child leaves its mother's
        /// outpost list to the tick its controller reaches
        /// `Tuning.BootstrapLevel`. `None` for a colony that was never
        /// anybody's child and for one that has outgrown its mother.
        Mother: string option
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Colony =
    /// The colonies a human has declared (ADR 0047): W12S28 with ADR 0042's
    /// north outpost W12S27, W13S28 — the colony it raised — beside it, and
    /// W15S28, which nobody owns yet. Chosen by a human in an ADR or a survey
    /// and moved by a human in a commit, exactly as the Layout's horizon is
    /// (ADR 0039). Claiming a room therefore begins here and not in the bot:
    /// an entry beside these ones is the whole of "I mean to take that room"
    /// (ADR 0047's user story 1), which is why the third entry has no spawn
    /// behind it and is not a mistake.
    ///
    /// Three declared and two **living**: `Colony.living` is the set `decide`
    /// runs over, and a home with no spawn of ours in it is not in it — its
    /// mother works that room until one stands.
    let declared: Colony list =
        [
            {
                Home = "W12S28"
                // W12S27 alone since W13S28 stood its own spawn (below); the
                // room is ADR 0042's north outpost, read off the pair above.
                Outposts = Outpost.adr0042 |> List.filter (fun o -> o.RoomName = "W12S27")
                Mother = None
            }
            // The second colony (ADR 0047). W13S28 was the first colony's
            // outpost until its own spawn stood; that tick it became a living
            // colony and left the mother's list. The same arrangement now runs
            // one generation further down: W15S28 below is in *this* colony's
            // list and declared beside it, so that room is projected by two
            // colonies for as long as it has no spawn — which is what an
            // outpost declaration naming a candidate colony already means
            // (ADR 0047), and it collapses back to one the tick it stands
            // one.
            {
                Home = "W13S28"
                // W13S29 to the south (2026-09-07): two sources across a
                // twelve-tile Seam, the survey's first pick. Withdrawn by hand
                // for three hundred ticks on 2026-09-08 while a two-creep
                // raid stood in it with nothing to shut the room — the brake
                // that should have is ADR 0043's stand-down, which clocks off
                // an invader *core* and not off creeps (#257) — and put back
                // once that raid was fifty ticks from expiring.
                //
                // W14S28 to the west (2026-09-10), the survey's second pick
                // and the room the chain to W15S28 crosses.
                //
                // W15S28 is **not** here, and the day it was claimed is why.
                // A candidate colony is its mother's outpost while nobody
                // owns it — that is what pools the Claim — but `childrenWhere`
                // gives a room in both lists to the outpost list, so leaving
                // it here after the claim classified an owned, spawn-less room
                // as a room we *mine*: no Reserve left to send a body, no
                // container to make a Post, and its spawn site ranked in the
                // outpost builders' budget behind a container one hop nearer.
                // Out of this list it is what it is — a [[nursery]] its mother
                // raises, whose every site is feeding-tier (ADR 0047 decision
                // 4). Live proof: claimed at t~305,2xx, spawn site placed by
                // hand, and not one body crossed until this line changed.
                Outposts = [ Outpost.w13s29; Outpost.w14s28 ]
                Mother = Some "W12S28"
            }
            // The third colony (2026-09-10, `docs/research/third-colony.md`).
            // The entry with no spawn behind it *is* the decision to take the
            // room (ADR 0047's user story 1): W13S28 projects it as an outpost
            // by the line above, and this line is what turns that room's
            // controller from a Reserve into a Claim. No outposts of its own
            // yet — W15S27, W15S29 and W14S29 are the rooms it will want, and
            // a room worked from a colony that does not exist is a body bought
            // for nobody.
            {
                Home = "W15S28"
                Outposts = []
                Mother = Some "W13S28"
            }
        ]

    /// The outposts one home room works: its own declaration's, and none at all
    /// for a room nobody declared. That last answer is the one that matters — a
    /// home the constant does not name projects the room it stands in and
    /// nothing else, so a slip in the constant costs the colony its outposts
    /// rather than putting it in a state nothing has a rule for.
    let outpostsOf (colonies: Colony list) (home: string) : Outpost list =
        colonies
        |> List.tryFind (fun colony -> colony.Home = home)
        |> Option.map (fun colony -> colony.Outposts)
        |> Option.defaultValue []

    /// Every declared colony's home room, in declaration order. What the
    /// shell hands the decision layer (`ColonyView.Declared`): which rooms a
    /// human means to own is not a thing vision can answer, and this is the
    /// half of "candidate colony" that can only be declared.
    let homes (colonies: Colony list) : string list =
        colonies |> List.map (fun colony -> colony.Home)

    /// The **living** colonies: the ones `Main.loop` builds a view for and runs
    /// `decide` once for — those whose home room is ours *and* holds one of our
    /// spawns (ADR 0047 decision 1). Declaration order, so the first entry is
    /// the one a creep no spawn name claims falls to. Two facts and not one,
    /// because each state a declared colony passes through on its way to
    /// running fails exactly one: a [[candidate colony]] owns nothing and
    /// spawns nothing, and a [[nursery]] is owned with no spawn of its own and
    /// is run by its [[mother colony]] (ADR 0047 decision 4). A spawn room no
    /// declaration names is a colony of its own, with no outposts and no
    /// mother, and only when nothing declared is living — the first such room
    /// in the order `spawnRooms` hands them (ADR 0047). Empty when the world
    /// holds no owned spawn room at all.
    let living
        (owned: Set<string>)
        (spawnRooms: string list)
        (colonies: Colony list)
        : Colony list =
        let declared =
            colonies
            |> List.filter (fun colony ->
                Set.contains colony.Home owned && List.contains colony.Home spawnRooms)

        match declared with
        | [] ->
            spawnRooms
            |> List.filter (fun room -> Set.contains room owned)
            |> List.tryHead
            |> Option.map (fun home ->
                {
                    Home = home
                    Outposts = []
                    // Nobody's child: a room the declaration does not
                    // describe is one no human wrote a mother for, and an
                    // invented one would hire pioneers for a constant's slip.
                    Mother = None
                })
            |> Option.toList
        | living -> living

    /// One colony's [[stage]] this tick, off the three facts that decide it
    /// (ADR 0052 decision 3). **The one place `Tuning.BootstrapLevel` is
    /// read**: no rule compares a controller level of its own — each asks for a
    /// stage instead, so the line moves in one field and cannot
    /// drift between its readers. The tunables arrive as an argument rather
    /// than off a constant (decision 5), so a test moves the line by handing
    /// another `Tuning`. `None` for a room that is not a colony at all: not
    /// owned by us is not a stage — a declared home nobody has claimed yet is a
    /// **candidate colony**, whose one rule is the Claim pool — and owned with
    /// no controller level to read is `None` too. Every reader's answer for
    /// `None` is the one it already gives that colony: no rampart kept, no road
    /// placed, nothing bootstrapped.
    let stageOf
        (tuning: Tuning)
        (owned: bool)
        (spawnStanding: bool)
        (level: int option)
        : ColonyStage option =
        if not owned then
            None
        elif not spawnStanding then
            Some Nursery
        else
            level
            |> Option.map (fun level ->
                if level >= tuning.BootstrapLevel then
                    Independent
                else
                    Bootstrapping)

    /// The declared children of this colony a further rule picks out: a colony
    /// this one is the mother of, that is not this one, and that this one does
    /// not already work as an [[outpost]] — the three clauses `bootstrapping`
    /// and `reclaiming` share whole, written once because they are one
    /// sentence ("a child of mine, not an outpost, not me") and the two rules
    /// differ only in what they then ask of the room. An outpost is worked and
    /// not raised, so a room in both lists is the outpost list's.
    let private childrenWhere (colonies: Colony list) (rule: string -> bool) (colony: Colony) =
        let worked = colony.Outposts |> List.map (fun outpost -> outpost.RoomName)

        colonies
        |> List.filter (fun child ->
            child.Mother = Some colony.Home
            && child.Home <> colony.Home
            && not (List.contains child.Home worked)
            && rule child.Home)
        |> List.map (fun child -> child.Home)

    /// The rooms one colony **bootstraps** this tick (ADR 0047 decision 4): the
    /// homes of the colonies it is the [[mother colony]] of, while those
    /// colonies are not yet `Independent`. The mother projects each of them
    /// beside her own rooms and works two Tasks there — the child's Upgrade and
    /// its Build — which is the one cross-colony borrowing rule there is. The
    /// stages are handed in, derived off the world (`World.stages`), because a
    /// colony's own view cannot answer for a room outside its scan set and this
    /// is the rule that *decides* that set. Three readers take the same answer
    /// and agree by construction; what must not be written twice is the *rule*
    /// (`World.scanOf` is the one place the union is spelled), because a second
    /// rule would be a room projected with nothing pooled in it, or pooled with
    /// nothing projecting it. **Both of the stages before independence**, and
    /// not the bootstrap window alone: a child that has left its mother's
    /// outpost list with no spawn standing is a [[nursery]] again, and the
    /// mother is the only colony that can put the spawn site back up. That is
    /// wider than the level rule it replaces, deliberately, and the price is
    /// the nursery's own paid over a grown room until the spawn stands again. A
    /// room with no stage is not bootstrapped: absence classifies nothing (ADR
    /// 0004), so a room we cannot see or do not own is left to the `Outposts`
    /// list, and a child that stops being ours is `reclaiming`'s.
    let bootstrapping
        (stages: Map<string, ColonyStage>)
        (colonies: Colony list)
        (colony: Colony)
        : string list =
        colony
        |> childrenWhere colonies (fun home ->
            Map.tryFind home stages |> Option.exists (fun stage -> stage <> Independent))

    /// The declared children of this colony that have stopped being ours, and
    /// are nobody else's either: the second half of what a mother projects for
    /// a child, and the one that has nothing to do with raising it. A [[stage]]
    /// is `None` for a room we do not own (ADR 0052 decision 3), so without
    /// this a child whose spawn was destroyed and whose controller was then
    /// lost — to a rival's claim or to the RCL1 downgrade — left every
    /// projection there was: no [[claim]] pooled anywhere, and only a human's
    /// edit could take the room back. **Unowned and never a rival's**: a room
    /// somebody else holds is ADR 0043's business, and a room with no control
    /// entry is one nothing looked into, which classifies nothing (ADR 0004).
    let reclaiming (unowned: Set<string>) (colonies: Colony list) (colony: Colony) : string list =
        colony |> childrenWhere colonies (fun home -> Set.contains home unowned)

    /// The rooms one colony projects this tick: its home and its worked
    /// [[outpost]]s (`Outpost.roomsProjected`), and beside them the rooms it
    /// bootstraps. The whole scan set in one sentence, here and not in the
    /// shell, because the projection is not the set's only reader — the entity
    /// lists the Task pool is built from are swept over it too.
    let roomsProjected
        (outposts: Outpost list)
        (bootstrap: string list)
        (home: string)
        : string list =
        Outpost.roomsProjected outposts home
        // A borrowed room carries its transit rooms exactly as an outpost does
        // (ADR 0058): the mother works two Tasks in a child of hers, and a
        // Task in a room no chain reaches is priced at `None` — so a nursery
        // two hops out would be projected, its spawn site lifted to the
        // feeding tier, and no pioneer could be sent to it. Found live on
        // 2026-09-10, when W15S28 was claimed two hops from its mother and
        // the only thing that made the walk priceable was a *separate*
        // declaration standing in the room between. `transitBetween` answers
        // off the names, so this asks nothing the union does not already know
        // (ADR 0058 decision 2), and a one-hop child adds nothing.
        @ (bootstrap
           |> List.collect (fun room -> room :: RoomName.transitBetween home room))
        |> List.distinct

    /// The colony that cast one creep, read off its own name: creep names are
    /// `{pattern}-{tick}-{spawn}`, so the room the named spawn stands in is its
    /// home (ADR 0047 decision 2). None when no known spawn's name is in it — a
    /// creep from an older naming scheme, or one a human made by hand.
    let private castBy (spawnHomes: (string * string) list) (creep: string) : string option =
        spawnHomes
        |> List.filter (fun (spawn, _) -> creep.Contains spawn)
        |> List.sortByDescending (fun (spawn, _) -> (spawn: string).Length)
        |> List.tryHead
        |> Option.map snd

    /// Which colony each creep belongs to this tick (ADR 0047 decision 2),
    /// keyed by creep name: a creep belongs to the colony that **cast** it,
    /// unless it is standing in a room only some *other* colony projects, in
    /// which case that colony **adopts** it for the tick. What a colony's
    /// `ColonyView.Creeps` and its census are cut by, so a creep is one
    /// colony's business and never two's — two colonies matching one creep
    /// would write two Tasks into one flat `assignments` leaf and move it
    /// twice. Adoption answers the creep a colony cannot place: a body outside
    /// every room its own colony projects has no tile there, and the colony
    /// that *does* project the room it stands in can price it, match it and
    /// move it. A fact about this tick, kept nowhere. **Only** another
    /// colony's, and only when exactly one projects it: a room its own colony
    /// projects too is its own colony's business, and a room two others project
    /// names no single adopter. A room *nobody* projects — a [[stand-down]]'s
    /// withheld outpost (ADR 0043) — adopts nobody either.
    let creepColonies
        (projections: (string * string list) list)
        (spawnHomes: (string * string) list)
        (creeps: (string * string option) list)
        : Map<string, string> =
        match projections with
        | [] -> Map.empty
        | (first, _) :: _ ->
            let homes = projections |> List.map fst

            creeps
            |> List.map (fun (name, standing) ->
                // A caster that is not one of the living colonies is no
                // answer at all, and falls to `first` beside the unreadable
                // names: a creep filed under a home with no view is nobody's.
                let cast =
                    castBy spawnHomes name
                    |> Option.filter (fun home -> List.contains home homes)
                    |> Option.defaultValue first

                let projecting =
                    match standing with
                    | None -> []
                    | Some room ->
                        projections
                        |> List.filter (fun (_, rooms) -> List.contains room rooms)
                        |> List.map fst

                match projecting with
                | [ adopter ] when adopter <> cast -> name, adopter
                | _ -> name, cast)
            |> Map.ofList
