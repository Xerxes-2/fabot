/// A colony and what it declares: the outposts it works (ADR 0042), the
/// stand-down gate that silences one (ADR 0043), and the stage it stands at
/// (ADR 0052). The declaration, never the tick.
[<AutoOpen>]
module Fabot.Core.Types.Colonies

/// Which kind of declaration one refusal names (ADR 0060 decision 1). There
/// are two kinds of room a human declares and one rule that refuses either, so
/// the channel that says a declaration was refused has to say *what* it
/// refused: "W15S25" printed under a heading that reads "declared outposts that
/// do not border this home" would be a second silent failure wearing the first
/// one's clothes.
[<RequireQualifiedAccess>]
type DeclarationKind =
    /// A room this colony mines and does not own (`Outpost`, ADR 0042).
    | Outpost
    /// A room this colony walks a body to because it must act on one named
    /// object standing in it (`Errand`, ADR 0060).
    | Errand

/// One declaration this colony cannot work, as the [[layout record]] carries
/// it: the room a human named, and the kind they named it as. Both halves, and
/// the kind is not decoration — a reader told only the room name has to guess
/// which of two lists to go and look at, and the two failures are not the same
/// size.
///
/// **The size of each, said here and referred to from everywhere else that
/// needs it** (ADR 0060 decision 1): an outpost with no chain wastes a
/// [[reserver]] — one body a tick, hired by a row that hires per declared
/// outpost, standing beside the spawn for its whole life. An errand with no
/// chain wastes the **whole programme**, the errand's entire content being a
/// walk. That is why carrying an unreachable errand is strictly worse than
/// carrying an unreachable outpost, and it is one sentence rather than five.
type RefusedDeclaration =
    {
        RoomName: string
        Kind: DeclarationKind
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Declaration =
    /// Whether a declared room is one its home can reach **at all**: the two
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
    /// walled is priced at `None` by `Atlas.route` and is the case #259 was
    /// open on, one hop out and now three.
    ///
    /// One rule over a room **name**, because both declaration kinds ask it and
    /// two spellings of it would be free to disagree about which rooms a colony
    /// can reach (ADR 0060 decision 1).
    let withinHopBudget (maxHops: int) (home: string) (room: string) : bool =
        RoomName.hopsBetween home room
        |> Option.exists (fun hops -> hops >= 1 && hops <= maxHops)

    /// Whether a chain actually joins the two — the same question one room
    /// further down (#259, ADR 0058). `withinHopBudget` above answers off the
    /// names and so can be asked of a declaration with no terrain read at all;
    /// this one asks `linked` per border and so answers whether a creep could
    /// really walk there. The budget is inside it: `RoomName.routesBy` searches
    /// no deeper, so a room this accepts is one `Atlas.route` will price.
    ///
    /// The two are not the same test and the difference is the whole of #259: a
    /// room three hops out whose every chain the engine walled is *shaped* like
    /// a declaration and is not one, and refusing it on the names alone would
    /// leave it projected, pooled and hired for by a row that hires per
    /// declared room — #243's silent failure, one budget further out.
    ///
    /// Asked of `routesBy` and not of `routeBy`: the search answers **every**
    /// shortest chain and the price picks one of them (ADR 0059), so "is there
    /// a chain" is an empty list and not a missing head. The two agree today
    /// and the one that keeps agreeing is this one.
    ///
    /// And asked **both ways** since ADR 0062, which is where that ADR's
    /// directed band reaches admission. `linked A B` and `linked B A` are two
    /// questions about two different rooms' ground and are free to answer
    /// differently, while `routesBy` expands away from `home` alone — so a
    /// chain out is no longer a chain back, and a room joined outbound by
    /// orphaned landings on the return would be admitted, projected, pooled
    /// and hired for while every body bought for it walked out and stayed
    /// there. What a declaration buys is a **round trip**: the reserver walks
    /// out, the hauler comes back loaded, the errand brings its ore home. So
    /// the chain home is asked for beside the chain out, and `refused` below
    /// keeps the invariant it states — `pricedAcross` and `haulRoundTripTicks`
    /// answer `None` for every target it names — which the inbound half is
    /// what makes true: `haulRoundTripTicks` prices off
    /// `Atlas.routes container.Room sink.Room`, the direction this clause and
    /// no other admits.
    ///
    /// Two searches and not one symmetric `linked`, deliberately. `linked` is
    /// the [[world]]'s reading of one band and `Atlas.seams` is the
    /// [[atlas]]'s reading of the same one; ADR 0058's invariant is that those
    /// two cannot disagree, and folding the round trip into `linked` would put
    /// the scan set's predicate one layer away from the price's. The round
    /// trip is admission's question, so it is asked where admission is
    /// decided — and asking it twice also lets the two legs take **different**
    /// chains, which is what a directed relation permits and a symmetric
    /// predicate would have forbidden.
    let routable
        (linked: string -> string -> bool)
        (maxHops: int)
        (home: string)
        (room: string)
        : bool =
        withinHopBudget maxHops home room
        && RoomName.routesBy linked maxHops home room |> List.isEmpty |> not
        && RoomName.routesBy linked maxHops room home |> List.isEmpty |> not

    /// The declared rooms of one kind that no chain joins to this home, each
    /// under the kind it was declared as (#243, ADR 0058, ADR 0060). A walk is
    /// priced over a chain of at most `Tuning.MaxHops` Seams, each leg a flood
    /// that never leaves its room, so a room further out than that is not a
    /// badly-priced declaration but an unpriceable one: `pricedAcross` and
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
        (kind: DeclarationKind)
        (rooms: string list)
        : RefusedDeclaration list =
        rooms
        |> List.filter (routable linked maxHops home >> not)
        |> List.map (fun room -> { RoomName = room; Kind = kind })

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

    /// Whether a declared outpost is one its home can work **at all**, off the
    /// names (`Declaration.withinHopBudget`, ADR 0058). The shared rule read
    /// through this declaration's own room name, so an outpost and an errand
    /// cannot disagree about what the budget is.
    let withinHopBudget (maxHops: int) (home: string) (outpost: Outpost) : bool =
        Declaration.withinHopBudget maxHops home outpost.RoomName

    /// Whether a chain actually joins the two (`Declaration.routable`, #259),
    /// asked of this declaration's room.
    let routable
        (linked: string -> string -> bool)
        (maxHops: int)
        (home: string)
        (outpost: Outpost)
        : bool =
        Declaration.routable linked maxHops home outpost.RoomName

    /// The declared outposts no chain joins to this home, each named as the
    /// outpost it was declared as (`Declaration.refused`, #243, ADR 0058).
    let refused
        (linked: string -> string -> bool)
        (maxHops: int)
        (home: string)
        (outposts: Outpost list)
        : RefusedDeclaration list =
        outposts
        |> List.map (fun outpost -> outpost.RoomName)
        |> Declaration.refused linked maxHops home DeclarationKind.Outpost

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
    ///
    /// **Withdrawn by hand 2026-09-16, re-declared 2026-09-18.** What withdrew
    /// it was never the room: it stood in the list while W11S29 was also in it,
    /// three crossings out and mined by its own colony at the same time, and
    /// the two together took this colony's haul demand from 2,780 over two
    /// haulers to 4,810 over four. #352 took W11S29 out and the demand fell
    /// back to 2,790 over two, which is the colony this declaration re-enters:
    /// nine of ten living, no row with a gap, the terminal at (14,10) built and
    /// shipping. Re-read the day it came back — one source at (6,8), no Source
    /// Keeper lair, no invader core, controller unreserved at (22,15), and the
    /// terrain layer still in the world as the transit room to W15S28, so the
    /// tick this adds is W12S29's +0.7 ms and not the +1.5..2.0 of a room
    /// nobody has walked (ADR 0041's trigger is still firing).
    ///
    /// The Thorium mineral at (2,29) is not part of this and never can be: an
    /// Extractor is an owned-room structure at RCL 6, and an outpost is by
    /// definition a room we reserve and do not own. Recorded here so the tile
    /// is not mistaken for an opportunity a later reading of this file might
    /// think was overlooked.
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

    /// The fourth colony's room, declared 2026-09-16 off
    /// `docs/research/fourth-colony.md`: **one** source, and a d4 Thorium
    /// deposit of **45,000** — the richest deposit a chain of ours reaches and
    /// twice any *reachable* rival candidate's, which is the whole of why a
    /// one-source room is here at all. A source buys RCL and the season is scored in Thorium; 45,000 T
    /// is about 225,000 score at the 5-a-tick band — more once the cumulative
    /// burn passes 99,999 ticks and the marginal T is worth 6 — and the one
    /// source only moves the
    /// tick RCL6 arrives on, which the research puts around t750,000 against a
    /// season ending near t1,966,000.
    ///
    /// **What the research found that no earlier survey had: terrain, not
    /// economics, decided this.** Counted the way ADR 0062 counts — both rings,
    /// the ground behind the landing, and the keeper margin — W9S28 carries the
    /// same d4 45,000 with *two* sources and is walled off entirely
    /// (`W11S28 ↔ W10S28` and `W10S28 ↔ W9S28` are both empty bands, so the
    /// highway column is sealed on both sides along this row); W13S25,
    /// `third-colony.md`'s runner-up and
    /// the one room that can price the Reactor at two crossings, opens onto
    /// nothing but the Source Keeper room W14S25; W11S26 opens only north; and
    /// W13S26 is W12S28's three-hop room rather than W13S28's two-hop one,
    /// because `W13S26 ↔ W13S27` is empty too. **Six** outpost declarations
    /// the names would accept and the ground refuses — W9S28 and W11S26 from
    /// W12S28, W13S25 and W13S26 from W13S28, W16S28 at *one* hop and W17S28 at
    /// two from W15S28 — and a seventh refusal of the errand kind, W12S25's
    /// reactor chain. #259's shape is the common case on this map and not the
    /// exception.
    ///
    /// **One source, one door.** `W11S28 ↔ W11S29` is empty as well, so this
    /// room is entered from W12S29 and from nowhere else a body can walk; its
    /// other two borders are the highways W10S29 and W11S30, which carry no
    /// controller and can never be outposts of it. That is the cheapest
    /// defensive frontage of any candidate and the thinnest outpost future.
    ///
    /// **Its Thorium is not deliverable by any walk, and that is deliberate.**
    /// W15S25 is eight crossings away, so this room will never declare
    /// `Errand.w15s25`. What moves the ore is a terminal — `mod-season5`'s
    /// `terminal-restriction.js` nulls only a `send` whose target terminal is
    /// somebody else's, so our own terminals should be able to send Thorium to
    /// each other for about 125 energy per 1,000 T over this range, which the
    /// research read off that file and off `calcTerminalEnergyCost` and
    /// **marks unverified**: nothing has sent a unit of Thorium on this server
    /// and `engine`'s own `terminal/tick.js` was not re-read. That capability
    /// is owed to the **36,484 T already banked in W12S28 and W13S28** (35,564
    /// in the two storages, 920 in their mineral containers), worth 182,420
    /// score, whichever room is claimed next. The
    /// research therefore chose on ore in the ground rather than on walking
    /// distance, and says plainly that if the terminal is refused the right
    /// fourth room is W13S26 instead.
    ///
    /// Declared as W13S28's outpost and as a colony of its own on the same day
    /// (ADR 0047's candidate-colony arrangement), with W13S28 as the mother
    /// rather than the two-hop W12S28 because a mother lends stock and not
    /// distance: W12S28's storage was measured at 0 rising to 3,746 over three
    /// hundred ticks with every spare unit going into its own controller, while
    /// W13S28's holds 625,402. W13S29, W12S28, W12S29 and W11S28 — the whole
    /// interior of `transitBetween`'s rectangle, W12S28's own home among them —
    /// enter the projection as transit rooms carrying terrain and a border ring
    /// (`OutpostDeclarationTests` pins the list). The ids and tiles are the
    /// engine's, read the day it was declared.
    let w11s29: Outpost =
        {
            RoomName = "W11S29"
            Sources = [ "6a8caac6dd4872bccd3195f5", { Room = "W11S29"; X = 7; Y = 33 } ]
            Controller = "6a8caac6dd4872bccd3195f4", { Room = "W11S29"; X = 30; Y = 29 }
        }

    /// W15S28's north outpost, declared 2026-09-16 off
    /// `docs/research/w15s27-outpost.md`, which executes the ticket ADR 0059
    /// left open: the room was declined for W13S28 in
    /// `multihop-outposts.md` §4 at a price taken under the old chain rule, and
    /// the recommendation was that it wait for W15S28. It now has one hop and
    /// one chain each way over a 20-tile band, one source at a 132-tick round
    /// trip, and ≈6.3 energy a tick net against a container, a reserver and one
    /// more hauler.
    ///
    /// What decided it is none of those. The room is **already** in this
    /// colony's scan set as the W15S25 errand's transit room, and `transiting`
    /// hands a transit room's facts back with `ConstructionSites = []` — so a
    /// human's hand-laid trunk out there is not merely unbuilt, it is
    /// *invisible*, and this line is what makes those sites pool as Build at
    /// all (#266 rations them, container first then nearest the crossing, at
    /// `Tuning.OutpostBuilders`).
    ///
    /// Its rock has a **single** Seat, `13,29`: a hand-laid site of any other
    /// kind on that tile plans this room no container at all, which is the one
    /// way this declaration can be live and worth nothing. Paving it saves the
    /// Reactor courier nothing either — at `Tuning.ReactorLoad` 500 the
    /// 20-Carry body is half empty, ten fatigue parts against ten Move, and one
    /// tick a tile on plain as on road (198 ticks loaded, paved or not).
    let w15s27: Outpost =
        {
            RoomName = "W15S27"
            Sources = [ "6a8caa95dd4872bccd319011", { Room = "W15S27"; X = 14; Y = 28 } ]
            Controller = "6a8caa95dd4872bccd319010", { Room = "W15S27"; X = 6; Y = 9 }
        }

    /// W11S29's first outpost, declared 2026-09-17 off
    /// `docs/research/w12s29-outpost.md`: one hop east, one source, and an
    /// **81-tick haul round trip** — less than half the cheapest number in
    /// either earlier outpost survey, because this room's one source sits at
    /// (40,43), hard against the border it shares with W11S29, whose spawn
    /// stands at (11,30). A 500-capacity hauler over 81 ticks moves 6.2 energy
    /// a tick against this colony's whole current demand of 380 over one
    /// hauler, so the declaration roughly doubles the fourth colony's income
    /// for one more hauler and change.
    ///
    /// Two liabilities, both measured and neither a refusal:
    ///
    /// - **The source has a single Seat** (one walkable neighbour), so one
    ///   anchor at a time and a dead one waits for its corpse —
    ///   `multihop-outposts.md` §4.3, and the same shape as W11S28's (32,14).
    ///   A reason to keep hands off that tile.
    /// - **The reserver this colony can afford banks nothing.** At RCL3 with
    ///   ten extensions the bank is 800 and `Bodies.bodyFor` answers
    ///   `[Claim; Move]` — one CLAIM, which holds a reservation flat (one tick
    ///   added an action against one decayed a tick) and accumulates no
    ///   buffer, so any gap in its presence drops the source to
    ///   `Engine.neutralOutputPerTick`. The room's full 10 a tick arrives with
    ///   RCL4's 1,300 bank and ADR 0042's two-CLAIM body; until then this is a
    ///   declared shortfall rather than a surprise, and positive even at the
    ///   neutral rate.
    let w12s29: Outpost =
        {
            RoomName = "W12S29"
            Sources = [ "6a8caabadd4872bccd3194ad", { Room = "W12S29"; X = 40; Y = 43 } ]
            Controller = "6a8caabadd4872bccd3194ac", { Room = "W12S29"; X = 15; Y = 36 }
        }

    /// W15S28's south outpost, declared 2026-09-17 off
    /// `docs/research/outpost-wave-2.md`, which called it *the best room of the
    /// three* and told it to wait: cheapest haul of the candidates (1,170), the
    /// most Seats (5), a 26-tile band, about 6.34 energy a tick net. What it
    /// waited on was never the room — it was #353 (landed) and this colony's own
    /// digestion of W15S27, whose container now stands at (13,29) holding 1,380
    /// with the second hauler and third anchor cast.
    ///
    /// Its one liability, and the survey names it alone among the three: the
    /// controller's Work Area is a **single tile** (12,34), so one reserver at a
    /// time and a dead one waits for its corpse before the next can stand —
    /// `multihop-outposts.md` §4.3's warning, the same shape as W11S28's single
    /// Seat. A reason to keep hands off that tile, not a reason to refuse it.
    let w15s29: Outpost =
        {
            RoomName = "W15S29"
            Sources = [ "6a8caa95dd4872bccd319017", { Room = "W15S29"; X = 18; Y = 20 } ]
            Controller = "6a8caa95dd4872bccd319018", { Room = "W15S29"; X = 12; Y = 34 }
        }

    /// W12S28's west outpost, declared 2026-09-16 off
    /// `docs/research/outpost-wave-2.md`, which is the wave-2 survey's only
    /// "declare now": one hop and one chain each way, one source at a 210-tick
    /// round trip, about 6.25 energy a tick net — and the cheapest tick of the
    /// three candidates, because this room's terrain layer is already in the
    /// world as a transit room of W13S28's W11S29 chain, so what the
    /// declaration adds is this colony's own projection and not a grid
    /// (+0.7 ms of `decide`, against +1.5..2.0 for either W15S28 candidate,
    /// while ADR 0041's revisit trigger is firing — #332, #353).
    ///
    /// Two liabilities, both geometry. Its band is **two tiles**, `49,31` and
    /// `49,32`, and it is the room's only door: W11S27, W10S28 and W11S29 all
    /// answer `false` both ways. And its rock has a **single** Seat, `32,14` —
    /// a site of any other kind on that tile plans this room no container at
    /// all (ADR 0042 as #244 amends it), and a dead anchor waits for its own
    /// corpse before the next one can stand (`multihop-outposts.md` §4.3).
    ///
    /// It does **not** starve #349's terminal site: that site is a *home* site,
    /// so it is not in `Pool.siteOrder`'s outpost queue at all, and what this
    /// declaration puts ahead of it is 5,000 progress against the terminal's
    /// 100,000 — one twentieth — while paying +6.25 a tick towards it.
    let w11s28: Outpost =
        {
            RoomName = "W11S28"
            Sources = [ "6a8caac6dd4872bccd3195f1", { Room = "W11S28"; X = 33; Y = 15 } ]
            Controller = "6a8caac6dd4872bccd3195f2", { Room = "W11S28"; X = 8; Y = 16 }
        }

/// One errand: a room a colony declares because it must walk a body there and
/// act on **one** named object in it, and for no other reason (ADR 0060
/// decision 1). A room name and that object's engine id and tile, and nothing
/// else — the second declaration kind, beside `Outpost` and not inside it,
/// because every reader of an outpost asks a question that presumes a
/// controller (the reservation, ADR 0042; the [[stand-down]] and its rival
/// latch, ADR 0043; the reserver row's per-declared-outpost quota; ADR 0056's
/// guard), and an optional `Controller` would make each of them learn to skip —
/// each a place the skip can be forgotten. It would also put a room we mine
/// nothing in on the list of rooms we mine, which is what `Outpost`'s own type
/// is for.
///
/// **More than a [[transit room]] and less than an [[outpost]].** More, because
/// the errand room's vision is work and the one target it names is the work: it
/// enters the [[spatial projection]] carrying terrain, a border ring and that
/// target laid under whatever vision answers, which is exactly the shape ADR
/// 0041 gave an outpost's sources and controller and for exactly the same
/// reason — a courier has to hold `Deliver of reactorId` before any body of
/// ours has stood in the room. Less, because *nothing else* in that room is:
/// no furniture is laid there beyond the one target, no source or controller of
/// it is ever pooled however much vision we pay for, and no row hires for it
/// except the ones the errand's own Tasks belong to. The failure #286 found
/// live in W14S28 — a reserver hired against a controller no declaration names,
/// an Anchor on a rock nobody declared — is the failure that narrowing exists
/// to prevent one room further out.
///
/// What does **not** wait on the declaration is everything that changes — the
/// target's store, its owner and its `continuousWork` — which room facts answer where there
/// is vision and is absent entry by entry where there is none (ADR 0004). The
/// narrowing below carries every such entry the shell filed under the declared
/// id and drops the rest, which is the rule. For the custom Reactor, the shell's
/// `FIND_REACTORS` sweep files a dedicated rich row beside the narrow ownership
/// map: decisions read the latter, while the global Reactor observation reads
/// the former and retains its dated sample through a blind tick.
///
/// Declared **inside the colony that runs it**, beside `Colony.Outposts`, for
/// ADR 0047's reason: a room's name in that list is what makes it that colony's
/// business. The ids are the **engine's own**, as every declaration in this
/// repo is, because a declaration written in readable names matches nothing on
/// a live server and does it in silence (ADR 0041).
type Errand =
    {
        RoomName: string
        /// The one object the errand is for, under the id the engine knows it
        /// by, and its tile joined to the room it is a tile of (ADR 0052
        /// decision 2). One and not a list: an errand is declared because there
        /// is exactly one thing out there to act on, and a second entry would
        /// be a second errand's.
        Target: string * RoomPos
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Errand =
    /// Whether a declared errand is one its home can reach at all, off the
    /// names (`Declaration.withinHopBudget`, ADR 0058).
    let withinHopBudget (maxHops: int) (home: string) (errand: Errand) : bool =
        Declaration.withinHopBudget maxHops home errand.RoomName

    /// Whether a chain of [[seam]]s actually joins the errand's room to this
    /// home (`Declaration.routable`, #259). Asked in `World.scanOf` on the same
    /// tick and against the same `linked` an outpost's is, because the two
    /// refusals are one rule — and it matters more here than there, for the
    /// reason `RefusedDeclaration` states once and this does not restate.
    let routable
        (linked: string -> string -> bool)
        (maxHops: int)
        (home: string)
        (errand: Errand)
        : bool =
        Declaration.routable linked maxHops home errand.RoomName

    /// The declared errands no chain joins to this home, each named as the
    /// errand it was declared as (`Declaration.refused`, ADR 0060 decision 1).
    let refused
        (linked: string -> string -> bool)
        (maxHops: int)
        (home: string)
        (errands: Errand list)
        : RefusedDeclaration list =
        errands
        |> List.map (fun errand -> errand.RoomName)
        |> Declaration.refused linked maxHops home DeclarationKind.Errand

    /// The rooms one colony's errands add to its scan set: each errand's room
    /// and every room a shortest walk to it could cross (ADR 0058), exactly as
    /// `Outpost.roomsProjected` unions an outpost's chain and by the same call.
    /// The rule ADR 0058 stated is unchanged and is what makes an errand
    /// declarable at all: which rooms are projected is answered off the
    /// **names**, because the route needs their terrain and their terrain needs
    /// them projected. Home itself is not in the list — `Colony.roomsProjected`
    /// puts it there once, for every declaration kind at once.
    let roomsProjected (errands: Errand list) (home: string) : string list =
        errands
        |> List.collect (fun errand ->
            errand.RoomName :: RoomName.transitBetween home errand.RoomName)

    /// One errand's declared target as a projection entry: the id paired with
    /// the tile the declaration names, and no kind at all. A declared tile filed
    /// under another room name is **dropped** here rather than written onto this
    /// room's coordinate (ADR 0052 decision 2), exactly as an outpost's is.
    ///
    /// **No kind, and that is the narrowing.** An outpost's furniture is placed
    /// *and* classified because the Harvest and Reserve pools are built by
    /// sweeping the kind census; an errand's target is placed and classified by
    /// nothing, so it is priceable and walkable by a Task that names its id —
    /// which the declaration's own Tasks do — and enumerable by no pool that
    /// sweeps a kind. That is the whole of "no row hires for it except the ones
    /// the errand's own Tasks belong to" (ADR 0060 decision 1), said in the
    /// data rather than as a rule each pool has to remember.
    let private targetOf (errand: Errand) : (string * Pos) option =
        let id, tile = errand.Target

        if tile.Room = errand.RoomName then
            Some(id, RoomPos.pos tile)
        else
            None

    /// The declared targets, laid into the projection: for every scanned
    /// errand, its one object at the tile and under the id the declaration
    /// names — whether or not the colony has vision there. The half of ADR 0041
    /// that vision may not gate, and the deadlock it breaks: a target's position
    /// needs vision, vision needs a creep there, a creep goes there because a
    /// Task exists, and the Task exists because the target is in the projection.
    /// Absence read onto the declaration as well is what left ADR 0042's chain
    /// with no first step, because no Task could name the room and so nothing
    /// ever walked there to get the vision. Vision wins every entry it holds:
    /// the declaration is laid *under* what the room's `find` families answered.
    let place (errands: Errand list) (spatial: SpatialInfo) : SpatialInfo =
        (spatial, errands)
        ||> List.fold (fun spatial errand ->
            match Map.tryFind errand.RoomName spatial.Rooms, targetOf errand with
            | Some layer, Some(id, pos) ->
                { spatial with
                    Rooms =
                        Map.add
                            errand.RoomName
                            { layer with
                                TargetPositions =
                                    if Map.containsKey id layer.TargetPositions then
                                        layer.TargetPositions
                                    else
                                        Map.add id pos layer.TargetPositions
                            }
                            spatial.Rooms
                }
            | _ -> spatial)

    /// The ids one errand room's facts may keep: the targets the declarations
    /// filed under that room name, and nothing else. What the view's own
    /// narrowing cuts an errand room's vision down to (`ColonyView.ofWorld`) —
    /// the one target a declaration names is work, and the room's sources, its
    /// sites and whatever else stands in it are not, however much vision we pay
    /// for. Empty for a room no errand names, which is the answer every other
    /// room in the scan set wants.
    let targetsIn (room: string) (errands: Errand list) : Set<string> =
        errands
        |> List.filter (fun errand -> errand.RoomName = room)
        |> List.choose (targetOf >> Option.map fst)
        |> Set.ofList

    /// W15S28's errand: the sector Reactor at W15S25 (44,6), declared
    /// 2026-09-13 off ADR 0060 decision 1 and `docs/research/thorium-season-plan.md`.
    /// The room is a **sector centre** and has no controller at all, so it can
    /// never be an `Outpost` — that is the vocabulary hole this kind fills, and
    /// the distance never was the problem: the reactor is exactly three
    /// crossings from W15S28, by W15S27 and the Source Keeper room W15S26,
    /// inside `Tuning.MaxHops`, and W15S28 was sited for that
    /// (`docs/research/third-colony.md` §4). It is five crossings from W13S28
    /// and six from W12S28, which is why neither of those colonies declares it
    /// and neither projects one room of the way there: a price into it from
    /// either is `None`, and a room in their projection would be one every rule
    /// answers nothing about.
    ///
    /// The id and the tile are the engine's, read off `/api/game/room-objects`
    /// at **tick 396,515–396,757**, the window
    /// `docs/research/thorium-season-plan.md` was taken over and the day this
    /// was declared — dated because every engine-read fact in this tree is, a
    /// declaration being a claim about a live server at one moment of it.
    ///
    /// The reactor's store, its owner and its `continuousWork` are **not** here
    /// and must not be: they change, so the visible room's dedicated Reactor row
    /// carries them and omits them when there is no vision (ADR 0004). Nothing
    /// captures that row either — `scripts/capture-room.mjs` takes a room's
    /// *fixed* furniture, its sources, controller and mineral (ADR 0036) — so
    /// W15S25's committed capture pins the ground this tile stands on and never
    /// the object standing on it.
    let w15s25: Errand =
        {
            RoomName = "W15S25"
            Target = "6a901a3bb8684d0008337ed2", { Room = "W15S25"; X = 44; Y = 6 }
        }

/// What this colony's [[raid log]] says about the rooms it declares, this tick
/// (ADR 0043 as #165 narrows it and #333 widens it), derived once off that log
/// (`Observe.standDown`) and handed to `ColonyView.ofWorld`: three sets rather
/// than one, because the log now answers three questions and not one. A room is
/// withdrawn from the work the colony does; a room whose withdrawal **latched**
/// on another player's ownership is looked into all the same, once a whole
/// `Tuning.RivalRecheck` has passed since the last look (#275), so the
/// conclusion that shut it can be contradicted by the only thing that ever
/// could — a tick with vision; and a room the colony goes on working is
/// remembered as one whose controller it may not reserve. One
/// record and not three derivations: the answers are read off one log and one
/// tick, and split apart they would be free to disagree about which rooms the
/// colony has withdrawn from.
///
/// Since #366 there is a fourth set and it is #333's shape said about the
/// guard row: the outposts an armed [[threat]] was standing in at the last
/// look. It withdraws nothing either, and it is read on the blind ticks alone.
///
/// The name is ADR 0043's and is now narrower than the record — two of the
/// four sets are a stand-down's, and `HeldOutposts` and `ThreatenedOutposts`
/// are deliberately not.
/// It is left as written rather than renamed under an accepted ADR: what the
/// field docs owe a reader is which of them withdraws a room, and they say so.
type StandDown =
    {
        /// Every room the gate withholds from the declaration this colony works
        /// (`Outpost.worked`): no Task pools there, no quota counts it and
        /// nothing walks toward it, because the room does not enter the
        /// [[spatial projection]] at all — the whole of "withdraw" in an
        /// architecture that recomputes every tick (ADR 0004, ADR 0043).
        Shut: Set<string>
        /// The rooms of `Shut` that **cannot be crossed either** (#382, ADR
        /// 0074): the ones shut on a `StandDownBasis.Stronghold` — a core of
        /// level 1 or more, which is towers under million-hit ramparts and a
        /// garrison of 25-part Invaders.
        ///
        /// A subset of `Shut` and a different question from it. ADR 0066
        /// decided that a stand-down does not propagate through a route, and
        /// it is right about its own case: what the gate ordinarily withholds
        /// is *work in a room*, which a body crossing that room does not do,
        /// and propagating a coarse outpost clock to the route would stop the
        /// Reactor's supply for something that never touched the walk. A
        /// stronghold is the case that reasoning does not cover — its danger
        /// is to the passage itself. Live, a `bunker4` in W15S26 killed two
        /// 650-energy re-claimers on the same entry tile 161 ticks apart while
        /// the gate had the room correctly shut and the relay went on walking
        /// through it.
        Impassable: Set<string>
        /// The rooms of `Shut` this tick takes one look into (#165): a
        /// **subset** of it and never a room leaving it. The look re-admits the
        /// room to the scan — the colony reads its controller, so the next
        /// [[raid log]] can drop a latch the rival has walked away from — and
        /// to nothing else: no furniture, no pooled rock, no Task and no quota
        /// row, which is what keeps ADR 0043's withdrawal in force on the very
        /// tick the gate is being questioned.
        Rechecked: Set<string>
        /// The declared [[outpost]]s whose controller **somebody else's CLAIM
        /// parts were standing on** at the last look, and whose hold has not
        /// run out on this tick (`RaidState.Holds`, #333). Held in the
        /// reservation sense `RoomControlInfo.heldByOther` carries, and not in
        /// `RaidState.RivalHeld`'s ownership sense — the two words collide in
        /// this leaf and nowhere else.
        ///
        /// The one set here that **withdraws nothing**: the room is worked, its
        /// rock is pooled and its bodies stand in it. What it narrows is the
        /// Reserve pool and the reserver row, and only on the ticks the colony
        /// is blind in the room (`Planner.reservableControllers`) — a tick with
        /// vision answers for itself and this set is not consulted at all. That
        /// is the whole of why it is carried: `RoomControl` is this tick's
        /// vision, the reserver is the only body most of these rooms ever hold,
        /// and a rule that read the refusal off vision alone would hire one
        /// more reserver every time the last one died (#333's live cadence, one
        /// body per 600 ticks).
        HeldOutposts: Set<string>
        /// The declared [[outpost]]s an armed [[threat]] was **standing in** at
        /// the last look, and whose memory has not run out on this tick
        /// (`RaidState.Threatened`, #366). #333's shape in the guard row: the
        /// second set here that withdraws nothing, and the second one read
        /// only where this tick's vision answers for nothing.
        ///
        /// What it buys is the body already paid for. The guard row hires on a
        /// threat seen in an outpost, and the bodies providing that vision —
        /// the anchor, the hauler, the reserver — are exactly what the raid
        /// kills, so the room goes dark, `view.Hostiles` empties, no Guard is
        /// pooled and a 15-ATTACK-part guard stands idle at home while a
        /// 2-ATTACK-part invader keeps the room (#366's live W11S28). ADR
        /// 0056's "vision in a guarded outpost is the guard" is circular, and
        /// this is where the circle is cut.
        ///
        /// **Vision overrules it**, exactly as it overrules `HeldOutposts`: a
        /// room with a `RoomControl` entry is decided by that tick's hostiles
        /// either way, and this set is consulted only where there is none
        /// (`Planner.guardedOutposts`). It ends by its own clock
        /// (`Tuning.ThreatMemory`) because a raid nobody can see is a raid
        /// nothing ends, and a memory with no end would hire a guard for a room
        /// an invader left hours ago.
        ThreatenedOutposts: Set<string>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StandDown =
    /// The open gate: nothing withheld, nothing to look into and nobody else's
    /// reservation remembered — what a colony with no [[raid log]] yet, and
    /// every colony on an ordinary tick, decides under.
    let none =
        {
            Shut = Set.empty
            Impassable = Set.empty
            Rechecked = Set.empty
            HeldOutposts = Set.empty
            ThreatenedOutposts = Set.empty
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
        /// The rooms it walks a body to for one named object and for nothing
        /// else (ADR 0060 decision 1). Beside `Outposts` and never inside it:
        /// an errand room is not one we mine, and the room this list exists for
        /// has no controller to be an outpost's. Declared inside the colony
        /// that runs it, for ADR 0047's reason, and projected by **that colony
        /// alone** — the errand in force names a room five and six crossings
        /// from the other two homes, so a price from either is `None` and a
        /// room in their projection would be one every rule answers nothing
        /// about. That is not an economy, it is the rule.
        Errands: Errand list
        /// The home room of the [[mother colony]] that raised this one, for as
        /// long as it is still being raised (ADR 0047 decision 4): the
        /// **bootstrap** window, from the day the child leaves its mother's
        /// outpost list to the tick its controller reaches
        /// `Tuning.BootstrapLevel`. `None` for a colony that was never
        /// anybody's child and for one that has outgrown its mother.
        Mother: string option
        /// The home room of the colony this one **ships its banked Thorium to**
        /// (#349), or `None` for a colony that ships none — either because it
        /// can walk its own ore to the Reactor or because it has none left.
        ///
        /// Declared by hand for the reason `Errands` is: it is a claim about
        /// two rooms at once, and the claim is not the distance but that the
        /// **far end can finish the job**. A send puts ore in a terminal three
        /// rooms away; what makes that worth 100,000 energy of terminal is that
        /// the room it lands in holds a declared `Errand` and a courier row
        /// that walks it the last three crossings. A colony cannot read that
        /// off its own view — `decide` runs per colony (ADR 0032) and no view
        /// carries another colony's declarations — so the pairing is stated
        /// where both ends are visible to a reader, here, rather than derived
        /// from a fact neither end holds.
        ///
        /// The energy is not the reason to hesitate: `mod-season5`'s
        /// `terminal-restriction.js` nulls only a `send` whose target terminal
        /// belongs to **another user**, and the engine's own fee is
        /// `ceil(amount · (1 − e^(−range/30)))` — about 95 energy a thousand
        /// over the three rooms between W12S28 and W15S28.
        Consignee: string option
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Colony =
    /// What this colony writes on a controller it stands beside (#381).
    ///
    /// A human's line, moved by a human in a commit, for the same reason the
    /// roster below is: what a room says to the rest of the server is not a
    /// number the bot derives. One line for every room rather than one per
    /// room — the thing being said is true of all of them, and a per-room
    /// table would be a table to keep in step for no gain.
    ///
    /// The engine caps a sign at 100 characters and keeps it until somebody
    /// overwrites it, so this is written rarely and read by strangers: it says
    /// what the bot *is* rather than who owns the room, which the map already
    /// shows.
    ///
    /// Core is F# and reaches no engine object, and the shell is Fable's
    /// JavaScript: both plainly true. "Core is pure" is the compression a
    /// hundred characters buys — `decide` is referentially transparent, and it
    /// writes the walk and far-field memo tables the caller carries across
    /// ticks (`PlanMemo`, #310), so it is pure the way a function with a cache
    /// is and not the way a textbook is. "One projection in, intents out" is
    /// the same kind of compression: it also takes the tick's assignments and
    /// memo, and returns [[verdict]]s and a [[movement]] beside the intents
    /// (ADR 0009). Flavour on a sign, not a specification — recorded here so
    /// the next reader does not take it for one.
    let signature =
        "A functional bot. F# compiled by Fable; Core is pure — one projection in, intents out"

    /// The colonies a human has declared (ADR 0047). Chosen by a human in an
    /// ADR or a survey and moved by a human in a commit, exactly as the
    /// Layout's horizon is (ADR 0039), so claiming a room begins here and not
    /// in the bot: an entry in this list is the whole of "I mean to take that
    /// room" (ADR 0047's user story 1), and an entry with no spawn behind it
    /// yet is that intent rather than a mistake.
    ///
    /// Declared is not **living**: `Colony.living` is the set `decide` runs
    /// over, and a home with no spawn of ours standing in it is not in it —
    /// its mother works that room until one stands.
    ///
    /// **No roster in this comment, deliberately** (#302). It used to name the
    /// rooms and count them — "three declared and two living", "W15S28, which
    /// nobody owns yet" — and every one of those clauses was a dated snapshot
    /// written in the present tense: W15S28 was claimed at about t305,200 and
    /// had its own spawn, thirty extensions and two towers long before anybody
    /// re-read the sentence saying nobody owned it. The list below **is** the
    /// roster and is right by construction; a reader who wants to know which
    /// of them are live asks `Colony.living`, and one who wants a room's stage
    /// asks the room. The dated notes on the individual entries stay: each one
    /// says the tick or the day it was written and cites the survey it came
    /// from, which is a record of why a room is here and not a claim about
    /// today.
    let declared: Colony list =
        [
            {
                Home = "W12S28"
                // Two rooms: ADR 0042's north outpost W12S27, read off the pair
                // above, and W11S28 to the west (2026-09-16,
                // `docs/research/outpost-wave-2.md`). That survey priced three
                // candidates at about 6.3 energy a tick each and found the
                // question is never which room is best but **which colony can
                // pay**: two of the three land on W15S28, which took W15S27
                // today and would go to 1,035 spawn ticks of 1,500 holding
                // both, and either costs it +1.5..2.0 ms of `decide` while ADR
                // 0041's revisit trigger is already firing. This room costs its
                // holder +0.7 ms, because its terrain is in the world already.
                Outposts =
                    (Outpost.adr0042 |> List.filter (fun o -> o.RoomName = "W12S27"))
                    @ [ Outpost.w11s28 ]
                // The sector Reactor is six crossings away and no room of the
                // way there is projected from here (ADR 0060 decision 1).
                Errands = []
                Mother = None
                // 19,848 T banked here with no walk to spend it on, and the
                // terminal to move it stood on 2026-09-17 at (11,43) — the
                // 100,000 energy that emptied this storage. W15S28 is the far
                // end because it is the one colony that declares the Reactor
                // errand: three rooms of `send` and then three crossings of
                // courier (#349).
                Consignee = Some "W15S28"
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
                // W14S28 to the west was declared on 2026-09-10 and
                // **withdrawn by hand the same day**, the way W13S29 was for
                // three hundred ticks on 2026-09-08. Not because the room is
                // wrong — it is the survey's second pick — but because of what
                // it costs *this* colony *now*: a single spawn cannot raise a
                // nursery two hops out and take on a second outpost at once.
                // Live at t~306,8xx the numbers said so plainly: haul demand
                // 2,790 → 4,810 the tick it was declared, the hauler row 2 →
                // 4 with two alive, so the extensions sat at 1,065 of 2,000
                // over a 162,931 storage, every body was cast at a 1,365 bank
                // instead of 2,300, the worker row stood at one of five, and
                // the nursery's spawn site had one pioneer of the three
                // `Tuning.PioneerCount` allows. Its container is built and
                // will decay; the room goes back in the list the day W15S28
                // stands its own spawn. `Outpost.w14s28` stays written for
                // that day.
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
                // W11S29 was in this list from 2026-09-16 until its Claim
                // landed, for the one tick's worth of reason ADR 0047 allows:
                // a room declared here puts its controller in this colony's
                // pool as a Claim, which is how a fourth colony is taken at
                // all. It came out on 2026-09-17 (#352), and both halves of
                // why are worth keeping.
                //
                // The half the W15S28 paragraph below already argues:
                // `childrenWhere` gives a room in both lists to the **outpost**
                // list, so an owned spawn-less room reads as a room we mine —
                // no Reserve, no Post, and its spawn site behind a container
                // one hop nearer.
                //
                // And the half this one paid: the haul. Left in for a few
                // thousand ticks past the Claim, W13S28 ran an anchor of its
                // own on W11S29's rock **three crossings out** while W11S29's
                // colony ran one on the same rock, and this colony's demand
                // went 2,780 over two haulers to 5,760 over four. That is the
                // shape that took W14S28 from 2,790 to 4,810 and had it
                // withdrawn by hand the day it was declared. W14S28 is back in
                // the list below, and what changed is that half: W11S29 left
                // this colony in #352 and the demand went back to 2,790 over
                // two haulers, so the room is being added to a colony that has
                // the seat for it rather than beside another colony's anchor on
                // the same rock.
                Outposts = [ Outpost.w13s29; Outpost.w14s28 ]
                // Five crossings to the Reactor, so this colony declares no
                // errand either — and the 22,000 Thorium it banks is ore
                // nothing here can deliver, which ADR 0060 decision 3 files as
                // its own open question and does not answer.
                Errands = []
                Mother = Some "W12S28"
                // 16,464 T banked and a terminal going up at (14,10),
                // 3,836/100,000 on 2026-09-17. Same far end and same reason as
                // W12S28's above: five crossings of walk this colony will
                // never make, three rooms of `send` it can (#349).
                Consignee = Some "W15S28"
            }
            // The third colony (2026-09-10, `docs/research/third-colony.md`).
            // The entry with no spawn behind it *is* the decision to take the
            // room (ADR 0047's user story 1): W13S28 projects it as an outpost
            // by the line above, and this line is what turns that room's
            // controller from a Reserve into a Claim. The colony it was a
            // decision about now exists — RCL6, its own spawn, its own
            // extensions — so the rooms it *would* want stop being bodies
            // bought for nobody, and W15S27 is the first of them (2026-09-16,
            // `docs/research/w15s27-outpost.md`). W15S29 and W14S29 are not
            // measured yet; W14S28 is W13S28's by the entry above and one hop
            // from here, which is a reassignment and not a new declaration.
            {
                Home = "W15S28"
                // W15S29 joins it on 2026-09-17. The wave-2 survey called it
                // the best *room* of its three candidates and told it to wait
                // on two things, neither about the room: #353, and this
                // colony's own digestion of W15S27. Both are in: the perf work
                // landed, and W15S27's container stands at (13,29) holding
                // 1,380 with the second hauler and third anchor cast.
                //
                // And the survey's own refusal was a CPU refusal — "+1.5-2.0 ms
                // of `decide`, +40-50%, on the colony whose tick is already the
                // dearest we run" — which is no longer the price. Re-measured
                // on today's code over the same scenario, three interleaved
                // rounds with a rebuild between each and the declaration moved
                // in this very list: **2.95, 2.95, 3.23 without against 3.51,
                // 3.53, 3.68 with — +0.55 ms, +18%, intervals not
                // overlapping.** What changed is #353 and #358: a room's
                // marginal cost is a third of what it was, because the far
                // field it adds is now held across ticks and shares its
                // suffix with the chains already priced.
                //
                // **And W15S29 came back out the same day**, after it ate four
                // of this colony's bodies: anchor-516370 and reserver-517630 at
                // t517,880-517,964, reserver-518659 at t518,726 and
                // reserver-519082 at t519,150 — all at range 1 in W15S29, all
                // to **one** invader of 1 ATTACK and 1 RANGED_ATTACK. W15S28
                // fell from 12 living to 3, its cluster to 1,300 of 8,300, and
                // with its reserver row empty the resident re-claimer at the
                // Reactor went with it, which shuts the courier programme and
                // stops the season scoring.
                //
                // The room is not the problem and neither is the price: #366
                // now remembers a raid through the blind ticks and sends the
                // guard in, but its memory was `Tuning.ThreatMemory` = 300
                // ticks and an invader that loiters for its whole 1,500-tick
                // life outlasted it — the memory expired, the room read clear
                // because nobody could see it, and the next unarmed body walked
                // in. Four of them: anchor-516370 and three reservers between
                // t517,880 and t519,150, to one 1-ATTACK invader.
                //
                // Both conditions that withdrawal named are met, which is why
                // W15S29 is back. The memory is now `Engine.creepLifetime` and
                // a backstop rather than a schedule (#369): nothing expires it
                // while nobody looks, and what ends it is a look that finds the
                // room clear — the guard standing in it being that look. And
                // this colony has its fleet back: ten of eleven living, the
                // reserver row at two of two, delivery restarted at t529,889
                // after nine thousand ticks dry, with the roads we laid in
                // W15S29 still standing and its controller unreserved by
                // anyone.
                Outposts = [ Outpost.w15s27; Outpost.w15s29 ]
                // And the one errand there is (ADR 0060 decision 1): the
                // sector Reactor in W15S25, three crossings out by W15S27 and
                // the Source Keeper room W15S26. This colony declares it
                // because this colony is the only one that can reach it, which
                // is the room's whole reason for being where it is.
                Errands = [ Errand.w15s25 ]
                Mother = Some "W13S28"
                // The far end of the other two colonies' consignments, and
                // so ships nothing itself: what lands in this terminal is
                // walked the last three crossings by the courier row this
                // colony already runs (#349).
                Consignee = None
            }
            // The fourth colony (2026-09-16, `docs/research/fourth-colony.md`).
            // The entry with no spawn behind it *is* the decision to take the
            // room, exactly as the third colony's was: W13S28 projects it as an
            // outpost by the line above, and this line turns that room's
            // controller from a Reserve into a Claim.
            //
            // No outposts: the only room this could ever declare is W12S29 —
            // one source, and W12S28's own candidate — because the two borders
            // open beside it land on the highways W10S29 and W11S30, which
            // carry no controller, and the fourth border is wall. And a room
            // worked from a colony that does not exist is a body bought for
            // nobody.
            // And no errand, by eight crossings — the 45,000 Thorium under this
            // room is ore no walk of ours can deliver, and the terminal that
            // can is owed to the 36,484 T already banked in the two homes
            // rather than to this room (ADR 0060 decision 3's open question,
            // one room wider).
            {
                Home = "W11S29"
                // Its first outpost, one hop east, declared the day after its
                // own spawn stood (2026-09-17,
                // `docs/research/w12s29-outpost.md`): the cheapest haul in the
                // programme at 81 ticks, on the colony with the most to gain
                // from one — 380 of demand over a single hauler today.
                Outposts = [ Outpost.w12s29 ]
                Errands = []
                Mother = Some "W13S28"
                // Its own 45,000 T — the richest deposit we can reach, and
                // after the season's banked stock ran out on 2026-09-18 the
                // **only** ore left that can score — goes the way W12S28's and
                // W13S28's went: three rooms of `send` into W15S28's terminal,
                // and the last three crossings by the courier row that colony
                // already runs (#349). Its own walk to the Reactor is four
                // crossings and four rooms wide, past `Tuning.MaxHops`, so it
                // will never make one.
                //
                // **Declared before the terminal it needs, on purpose** (#349's
                // shape, read forward): the pairing is inert without one —
                // `consignWithdraws` folds over terminals and finds none,
                // `Layout.planConsignment`'s `ship` the same — and the tick the
                // Layout stands it at RCL6 the ore has a path with nobody
                // having to remember. The alternative is a hand edit at a
                // moment no alarm watches, which is how 36,484 T sat unshipped
                // until #349 was noticed.
                Consignee = Some "W15S28"
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

    /// The [[errand]]s one home room runs, on the same rule and for the same
    /// reason (ADR 0060 decision 1): its own declaration's, and none at all for
    /// a room nobody declared. Read through this rather than off the field
    /// wherever the home is a *name*, so a slip in the constant costs the
    /// colony its errand rather than putting it in a state nothing has a rule
    /// for.
    let errandsOf (colonies: Colony list) (home: string) : Errand list =
        colonies
        |> List.tryFind (fun colony -> colony.Home = home)
        |> Option.map (fun colony -> colony.Errands)
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
                    // And no errand: an errand is a walk a human wrote down,
                    // and an invented one would send a body three rooms out
                    // for a constant's slip.
                    Errands = []
                    // Nobody's child: a room the declaration does not
                    // describe is one no human wrote a mother for, and an
                    // invented one would hire pioneers for a constant's slip.
                    Mother = None
                    // And nothing to ship: a consignment is a pairing a
                    // human wrote down for both its ends (#349), and the
                    // fallback colony knows of no second room at all.
                    Consignee = None
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
    /// [[outpost]]s (`Outpost.roomsProjected`), its [[errand]]s beside them
    /// (`Errand.roomsProjected`, ADR 0060 decision 1), and the rooms it
    /// bootstraps. The whole scan set in one sentence, here and not in the
    /// shell, because the projection is not the set's only reader — the entity
    /// lists the Task pool is built from are swept over it too.
    let roomsProjected
        (outposts: Outpost list)
        (errands: Errand list)
        (bootstrap: string list)
        (home: string)
        : string list =
        Outpost.roomsProjected outposts home
        // An errand's room and its chain, by the same union and the same rule
        // one hop wider (ADR 0060 decision 1): `World.worldRooms` picks the
        // room up because a *standing colony declares it*, which is the path an
        // outpost already takes, and only the declaring colony's set gains it.
        @ Errand.roomsProjected errands home
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
