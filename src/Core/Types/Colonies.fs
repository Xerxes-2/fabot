/// A colony and what it declares: the outposts it works, the stand-down gate
/// that silences one, and the stage it stands at. The declaration, never the
/// tick.
[<AutoOpen>]
module Fabot.Core.Types.Colonies

/// Which kind of declaration one refusal names: two kinds of room a human
/// declares, one rule that refuses either, so the channel has to say *what*
/// it refused.
[<RequireQualifiedAccess>]
type DeclarationKind =
    /// A room this colony mines and does not own (`Outpost`).
    | Outpost
    /// A room this colony walks a body to because it must act on one named
    /// object standing in it (`Errand`).
    | Errand

/// One declaration this colony cannot work, as the [[layout record]] carries
/// it: the room a human named, and the kind they named it as. The two
/// failures are not the same size: an outpost with no chain wastes a
/// [[reserver]], one body a tick standing beside the spawn; an errand with no
/// chain wastes the **whole programme**, the errand's entire content being a
/// walk.
type RefusedDeclaration =
    {
        RoomName: string
        Kind: DeclarationKind
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Declaration =
    /// Whether a declared room is one its home can reach **at all**: inside
    /// the hop budget, off the **names** (`RoomName.hopsBetween`), because this
    /// is asked while the scan set is being built, before there is a
    /// projection to read terrain off. So it answers whether the declaration
    /// is *shaped* like one a route could join, never whether one does.
    let withinHopBudget (maxHops: int) (home: string) (room: string) : bool =
        RoomName.hopsBetween home room
        |> Option.exists (fun hops -> hops >= 1 && hops <= maxHops)

    /// Whether a chain actually joins the two (#259): `linked` per border, so
    /// a creep could really walk there. The budget is inside it —
    /// `RoomName.routesBy` searches no deeper, so a room this accepts is one
    /// `Atlas.route` will price. Asked of `routesBy` and not `routeBy`, so "is
    /// there a chain" is an empty list and not a missing head. ADR-0058
    ///
    /// Asked **both ways**: `linked A B` and `linked B A` are two questions
    /// about two rooms' ground, and what a declaration buys is a round trip —
    /// `haulRoundTripTicks` prices the inbound direction. Two searches and
    /// not one symmetric `linked`, so the scan set's predicate stays the
    /// price's, and the two legs may take different chains. ADR-0062
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
    /// under the kind it was declared as (#243). Such a room is unpriceable:
    /// `pricedAcross` and `haulRoundTripTicks` answer `None` for every target
    /// in it, and worked anyway it is projected, pooled and hired for with
    /// nothing to say why. So it is **refused** and the refusal is named:
    /// `ColonyView.Refused` carries it to the [[layout record]], and the test
    /// over `Colony.declared` makes a human's slip red before deploy. Read off
    /// the whole declaration and not off `worked`'s survivors.
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
/// budget its home reaches over a chain of Seams. Declared, never discovered
/// — every "the first creep to walk in writes it down" scheme has to answer
/// what sent the first creep. What is declared is exactly what vision cannot
/// be waited for: the room's name, and the id and tile of each source and of
/// the controller. The ids are the **engine's own**: readable short names
/// would match nothing on a live server, in silence. ADR-0042
type Outpost =
    {
        RoomName: string
        /// The room's sources, each under the id the engine knows it by, and
        /// each tile joined to the room it is a tile of.
        Sources: (string * RoomPos) list
        /// The room's controller, whose reservation is what doubles those
        /// sources. Not optional: a room with no controller to reserve — a
        /// sector centre or a Source Keeper room — is not a candidate outpost.
        Controller: string * RoomPos
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Outpost =
    /// The declarations the colony works this tick: the declared list, less
    /// every room a [[stand-down]] is withholding. `StandDown.Rechecked`
    /// (#165) never reaches here: a room re-admitted to the scan is still
    /// withheld from the work.
    let worked (shut: Set<string>) (outposts: Outpost list) : Outpost list =
        outposts
        |> List.filter (fun outpost -> not (Set.contains outpost.RoomName shut))

    /// `Declaration.withinHopBudget` asked of this declaration's room.
    let withinHopBudget (maxHops: int) (home: string) (outpost: Outpost) : bool =
        Declaration.withinHopBudget maxHops home outpost.RoomName

    /// `Declaration.routable` asked of this declaration's room.
    let routable
        (linked: string -> string -> bool)
        (maxHops: int)
        (home: string)
        (outpost: Outpost)
        : bool =
        Declaration.routable linked maxHops home outpost.RoomName

    /// The declared outposts no chain joins to this home
    /// (`Declaration.refused`).
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
    /// declared outpost beside it, in one projection. The outposts are handed
    /// in rather than read off the constant, so the stand-down gate has
    /// exactly one place to narrow the set.
    let roomsProjected (outposts: Outpost list) (home: string) : string list =
        home
        :: (outposts
            |> List.collect (fun outpost ->
                // The outpost, and every room a shortest walk to it could
                // cross. A **transit** room is projected for its terrain and
                // border ring alone — what `Atlas.route` needs to find a chain
                // through it; nothing is laid, pooled or hired there.
                outpost.RoomName :: RoomName.transitBetween home outpost.RoomName))
        |> List.distinct

    /// One declaration as projection entries: the controller and then the
    /// sources in their declared order, id, tile and kind read off one list,
    /// so the folds below cannot place an id the kind census misses or
    /// classify one nothing places. A declared tile filed under another room
    /// name is **dropped** rather than written onto this room's coordinate.
    let private furnitureOf (outpost: Outpost) : (string * Pos * TargetKind) list =
        (fst outpost.Controller, snd outpost.Controller, Controller)
        :: (outpost.Sources |> List.map (fun (id, tile) -> id, tile, Source))
        |> List.filter (fun (_, tile, _) -> tile.Room = outpost.RoomName)
        |> List.map (fun (id, tile, kind) -> id, RoomPos.pos tile, kind)

    /// The declared furniture, laid into the projection whether or not the
    /// colony has vision there: the half of the projection vision may not
    /// gate, and the deadlock that breaks (a position needs vision, vision
    /// needs a creep, a creep needs a Task, a Task needs the projection).
    /// Vision wins every entry it holds: the declaration is laid *under* what
    /// the room's `find` families answered. The controller's tile joins
    /// `Obstacles`, so a reserver stands beside it and never on it.
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
    /// with, and every declared outpost rock beside them, deduplicated by id
    /// with the seen list first. An unseen rock restocks in 0 ticks: judged
    /// at arrival like any other, and the Emitter's own gate withholds the dig
    /// from a rock that turns out empty. Read off `furnitureOf` and scanned
    /// rooms only, so the rocks are pooled exactly where the furniture is
    /// laid — an unplaced target prices at 0 and would *win* its tier.
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

    /// The two rooms the outpost layer was first measured on: the
    /// real-terrain fixtures (`RoomInvariantTests`) read the captures
    /// relative to W12S28 with both laid in. Ids and tiles are the engine's,
    /// pinned against the committed captures.
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

    /// W13S28's south outpost, declared 2026-09-07 off
    /// `docs/research/remote-candidates.md`: two sources, a 12-tile south
    /// Seam, the best net income of the five rooms a Seam reaches.
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

    /// **W15S28's east outpost since 2026-09-20**, W13S28's before that
    /// (declared 2026-09-10 off `docs/research/multihop-outposts.md`, ranked
    /// second of six; withdrawn 2026-09-16 and re-declared 2026-09-18 over the
    /// haul it added beside W11S29). The rock walks 46 tiles from W15S28's
    /// Storage and 67 from W13S28's. One source at (6,8), no keeper lair, no
    /// invader core. The Thorium mineral at (2,29) is not part of this and
    /// never can be: an Extractor is an owned-room structure at RCL 6.
    let w14s28: Outpost =
        {
            RoomName = "W14S28"
            Sources = [ "6a8caaa1dd4872bccd3191f9", { Room = "W14S28"; X = 6; Y = 8 } ]
            Controller = "6a8caaa1dd4872bccd3191fa", { Room = "W14S28"; X = 22; Y = 15 }
        }

    /// The third colony's room, declared 2026-09-10 off
    /// `docs/research/third-colony.md`: two sources, a d3 Thorium deposit of
    /// 22,000, and the one site left after the risk screen from which the
    /// sector Reactor is inside `Tuning.MaxHops` (three crossings by W15S27
    /// and the Source Keeper room W15S26). Two hops from its home through
    /// W14S28; declared as W13S28's outpost and as a colony of its own on the
    /// same day, the candidate-colony arrangement. Source order is the
    /// capture's (`tests/Core.Tests/rooms/W15S28.room`).
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
    /// deposit of **45,000** — the richest deposit a chain of ours reaches,
    /// which is the whole of why a one-source room is here. Terrain decided
    /// it: W9S28 (same deposit, two sources) is walled off entirely, and six
    /// declarations the names would accept the ground refuses. **One door**:
    /// entered from W12S29 and nowhere else a body can walk; its other borders
    /// are highways. Its Thorium is eight crossings from W15S25 and moves by
    /// terminal, which the research **marks unverified** (nothing has sent a
    /// unit of Thorium on this server); if the terminal is refused the right
    /// fourth room is W13S26. Mother W13S28 rather than the two-hop W12S28,
    /// because a mother lends stock and not distance.
    let w11s29: Outpost =
        {
            RoomName = "W11S29"
            Sources = [ "6a8caac6dd4872bccd3195f5", { Room = "W11S29"; X = 7; Y = 33 } ]
            Controller = "6a8caac6dd4872bccd3195f4", { Room = "W11S29"; X = 30; Y = 29 }
        }

    /// W15S28's north outpost, declared 2026-09-16 off
    /// `docs/research/w15s27-outpost.md`: one hop over a 20-tile band, one
    /// source at a 132-tick round trip, ≈6.3 energy a tick net. What decided
    /// it: the room is already in this colony's scan set as the W15S25
    /// errand's transit room, and `transiting` returns `ConstructionSites =
    /// []`, so a hand-laid trunk out there is invisible until this line makes
    /// its sites pool as Build. Its rock has a **single** Seat, `13,29`: a
    /// site of any other kind on that tile plans this room no container.
    let w15s27: Outpost =
        {
            RoomName = "W15S27"
            Sources = [ "6a8caa95dd4872bccd319011", { Room = "W15S27"; X = 14; Y = 28 } ]
            Controller = "6a8caa95dd4872bccd319010", { Room = "W15S27"; X = 6; Y = 9 }
        }

    /// W11S29's first outpost, declared 2026-09-17 off
    /// `docs/research/w12s29-outpost.md`: one hop east, one source at (40,43)
    /// hard against the shared border, an **81-tick haul round trip** that
    /// roughly doubles the fourth colony's income. Two measured liabilities,
    /// neither a refusal: the source has a single Seat (keep hands off that
    /// tile), and at RCL3's 800 bank the reserver is `[Claim; Move]`, which
    /// holds a reservation flat and banks no buffer — the full 10 a tick
    /// arrives with RCL4's two-CLAIM body.
    let w12s29: Outpost =
        {
            RoomName = "W12S29"
            Sources = [ "6a8caabadd4872bccd3194ad", { Room = "W12S29"; X = 40; Y = 43 } ]
            Controller = "6a8caabadd4872bccd3194ac", { Room = "W12S29"; X = 15; Y = 36 }
        }

    /// W15S28's south outpost, declared 2026-09-17 off
    /// `docs/research/outpost-wave-2.md`: cheapest haul of the candidates,
    /// the most Seats (5), about 6.34 energy a tick net. Its one liability:
    /// the controller's Work Area is a **single tile** (12,34), so one
    /// reserver at a time — keep hands off that tile.
    let w15s29: Outpost =
        {
            RoomName = "W15S29"
            Sources = [ "6a8caa95dd4872bccd319017", { Room = "W15S29"; X = 18; Y = 20 } ]
            Controller = "6a8caa95dd4872bccd319018", { Room = "W15S29"; X = 12; Y = 34 }
        }

    /// W12S28's west outpost, declared 2026-09-16 off
    /// `docs/research/outpost-wave-2.md`: one hop, one source at a 210-tick
    /// round trip, about 6.25 energy a tick net, and the cheapest tick of the
    /// three candidates because its terrain layer is already in the world as
    /// a transit room (+0.7 ms of `decide`; #332, #353). Two liabilities, both
    /// geometry: its band is **two tiles**, `49,31` and `49,32`, the room's
    /// only door; and its rock has a **single** Seat, `32,14`, where a site of
    /// any other kind plans this room no container (#244).
    let w11s28: Outpost =
        {
            RoomName = "W11S28"
            Sources = [ "6a8caac6dd4872bccd3195f1", { Room = "W11S28"; X = 33; Y = 15 } ]
            Controller = "6a8caac6dd4872bccd3195f2", { Room = "W11S28"; X = 8; Y = 16 }
        }

/// One errand: a room a colony declares because it must walk a body there and
/// act on **one** named object in it — a room name and that object's engine
/// id and tile, and nothing else. The second declaration kind, beside
/// `Outpost` and not inside it, because every reader of an outpost presumes a
/// controller and an optional one would make each of them learn to skip.
/// ADR-0060
///
/// **More than a [[transit room]] and less than an [[outpost]].** More: the
/// one target is laid into the [[spatial projection]] under whatever vision
/// answers, so a courier can hold `Deliver of reactorId` before any body of
/// ours has stood in the room. Less: nothing else in that room is work — no
/// source or controller of it is ever pooled, and no row hires for it except
/// the ones the errand's own Tasks belong to (#286's live failure in W14S28,
/// a reserver hired against a controller no declaration names, is what the
/// narrowing prevents one room further out). What changes — the target's
/// store, owner, `continuousWork` — is vision's, absent where there is none.
type Errand =
    {
        RoomName: string
        /// The one object the errand is for, under the id the engine knows it
        /// by. One and not a list: a second entry would be a second errand's.
        Target: string * RoomPos
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Errand =
    /// `Declaration.withinHopBudget` asked of this errand's room.
    let withinHopBudget (maxHops: int) (home: string) (errand: Errand) : bool =
        Declaration.withinHopBudget maxHops home errand.RoomName

    /// `Declaration.routable` asked of this errand's room, on the same tick
    /// and against the same `linked` an outpost's is.
    let routable
        (linked: string -> string -> bool)
        (maxHops: int)
        (home: string)
        (errand: Errand)
        : bool =
        Declaration.routable linked maxHops home errand.RoomName

    /// The declared errands no chain joins to this home
    /// (`Declaration.refused`).
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
    /// and every room a shortest walk to it could cross, off the names, as
    /// `Outpost.roomsProjected`. Home is not in the list —
    /// `Colony.roomsProjected` puts it there once.
    let roomsProjected (errands: Errand list) (home: string) : string list =
        errands
        |> List.collect (fun errand ->
            errand.RoomName :: RoomName.transitBetween home errand.RoomName)

    /// One errand's declared target as a projection entry: id and tile, and
    /// **no kind — that is the narrowing**. Every pool is built by sweeping
    /// the kind census, so a target classified by nothing is priceable by a
    /// Task that names its id and enumerable by no pool. A tile filed under
    /// another room name is dropped, as an outpost's is.
    let private targetOf (errand: Errand) : (string * Pos) option =
        let id, tile = errand.Target

        if tile.Room = errand.RoomName then
            Some(id, RoomPos.pos tile)
        else
            None

    /// The declared targets, laid into the projection whether or not there is
    /// vision, for `Outpost.place`'s reason; vision wins every entry it holds.
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

    /// The ids one errand room's facts may keep: the declared targets filed
    /// under that room name and nothing else (`ColonyView.ofWorld`). Empty for
    /// a room no errand names.
    let targetsIn (room: string) (errands: Errand list) : Set<string> =
        errands
        |> List.filter (fun errand -> errand.RoomName = room)
        |> List.choose (targetOf >> Option.map fst)
        |> Set.ofList

    /// W15S28's errand: the sector Reactor at W15S25 (44,6), declared
    /// 2026-09-13 off `docs/research/thorium-season-plan.md`. A sector centre
    /// has no controller, so it can never be an `Outpost`. Three crossings
    /// from W15S28 by W15S27 and the Source Keeper room W15S26; five from
    /// W13S28 and six from W12S28, which is why neither declares it. The id
    /// and tile were read off `/api/game/room-objects` at tick
    /// 396,515–396,757. The reactor's store, owner and `continuousWork` are
    /// **not** here: they change, so the visible room's Reactor row carries
    /// them. `scripts/capture-room.mjs` takes fixed furniture only, so
    /// W15S25's capture pins the ground and never the object standing on it.
    let w15s25: Errand =
        {
            RoomName = "W15S25"
            Target = "6a901a3bb8684d0008337ed2", { Room = "W15S25"; X = 44; Y = 6 }
        }

/// What this colony's [[raid log]] says about the rooms it declares, this tick
/// (#165, #333, #366), derived once off that log (`Observe.standDown`) and
/// handed to `ColonyView.ofWorld`: one record and not five derivations, so
/// the sets cannot disagree about which rooms the colony has withdrawn from.
/// The name is narrower than the record — `HeldOutposts` and
/// `ThreatenedOutposts` withdraw nothing, and say so. ADR-0043
type StandDown =
    {
        /// Every room the gate withholds from the work (`Outpost.worked`): no
        /// Task pools there, no quota counts it and nothing walks toward it,
        /// because the room does not enter the [[spatial projection]] at all.
        Shut: Set<string>
        /// The rooms of `Shut` that **cannot be crossed either** (#382): shut
        /// on a `StandDownBasis.Stronghold`, a core of level 1 or more with
        /// towers under million-hit ramparts. A different question from
        /// `Shut`: the gate ordinarily withholds *work in a room*, which a
        /// body crossing it does not do; a stronghold's danger is to the
        /// passage itself. Live, a `bunker4` in W15S26 killed two re-claimers
        /// on the same entry tile 161 ticks apart while the room was shut.
        Impassable: Set<string>
        /// The rooms of `Shut` this tick takes one look into (#165): a
        /// **subset** of it and never a room leaving it. The look re-admits
        /// the room to the scan — the next [[raid log]] can drop a latch the
        /// rival has walked away from — and to nothing else, so the
        /// withdrawal stands on the very tick the gate is being questioned.
        Rechecked: Set<string>
        /// The declared [[outpost]]s whose controller **somebody else's CLAIM
        /// parts were standing on** at the last look, whose hold has not run
        /// out (`RaidState.Holds`, #333). The reservation sense
        /// `RoomControlInfo.heldByOther` carries, not `RaidState.RivalHeld`'s
        /// ownership sense.
        ///
        /// Withdraws nothing: the room is worked and its rock is pooled. What
        /// it narrows is the Reserve pool and the reserver row, on the ticks
        /// the colony is blind in the room (`Planner.reservableControllers`);
        /// a tick with vision answers for itself. Carried because the reserver
        /// is the only body most of these rooms hold, and a refusal read off
        /// vision alone would hire one more every time the last one died.
        HeldOutposts: Set<string>
        /// The declared [[outpost]]s an armed [[threat]] was **standing in** at
        /// the last look, whose memory has not run out (`RaidState.Threatened`,
        /// #366). Withdraws nothing, and is read only where this tick's vision
        /// answers for nothing (`Planner.guardedOutposts`).
        ///
        /// What it buys is the body already paid for: the bodies providing
        /// vision — anchor, hauler, reserver — are exactly what a raid kills,
        /// so the room goes dark, no Guard is pooled and a 15-ATTACK guard
        /// stands idle at home while a 2-ATTACK invader keeps the room (#366's
        /// live W11S28). It ends by its own clock (`Tuning.ThreatMemory`)
        /// because a raid nobody can see is a raid nothing ends.
        ThreatenedOutposts: Set<string>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module StandDown =
    /// The open gate: nothing withheld, nothing to look into and nobody else's
    /// reservation remembered.
    let none =
        {
            Shut = Set.empty
            Impassable = Set.empty
            Rechecked = Set.empty
            HeldOutposts = Set.empty
            ThreatenedOutposts = Set.empty
        }

/// Where one colony stands in its life: the one fact rules read instead of a
/// controller level apiece — whether it places roads and keeps ramparts,
/// whether its sites come before its controller, whether a [[mother colony]]
/// is still raising it. ADR-0052
type ColonyStage =
    /// Claimed, and no spawn of ours standing in it yet: a [[nursery]]. What
    /// ends it is a spawn.
    | Nursery
    /// Its own spawn standing and its controller still under
    /// `Tuning.BootstrapLevel`: running its own `decide`, and still being
    /// raised — the **bootstrap window**.
    | Bootstrapping
    /// At `Tuning.BootstrapLevel` or past it. The stage every rule written for
    /// the one home this bot grew up in was written at.
    | Independent

/// One colony: a [[home room]] and the [[outpost]]s worked from it. The unit
/// the whole decision layer is written in — one Atlas, one Layout, one set of
/// quotas, one Task pool — and so the unit a declaration is written in.
/// ADR-0047
type Colony =
    {
        /// The room the colony is run from: the room its spawns stand in,
        /// its Layout is planned in, and its quotas are banked in.
        Home: string
        /// The rooms it mines but does not own. A **candidate colony**'s home
        /// appears here as well, in its *mother* colony's list, until the day
        /// it is independent.
        Outposts: Outpost list
        /// The rooms it walks a body to for one named object and for nothing
        /// else. Beside `Outposts` and never inside it: an errand room is not
        /// one we mine, and has no controller to be an outpost's.
        Errands: Errand list
        /// The home room of the [[mother colony]] that raised this one, for as
        /// long as it is still being raised. `None` for a colony that was
        /// never anybody's child and for one that has outgrown its mother.
        Mother: string option
        /// The home room of the colony this one **ships its banked Thorium to**
        /// (#349), or `None` for a colony that ships none. Declared by hand
        /// because the claim is that the **far end can finish the job** — it
        /// holds a declared `Errand` and a courier row — and no view carries
        /// another colony's declarations. `mod-season5`'s
        /// `terminal-restriction.js` nulls only a `send` whose target terminal
        /// belongs to **another user**; the fee is `Engine.sendFee`.
        Consignee: string option
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Colony =
    /// What this colony writes on a controller it stands beside (#381): a
    /// human's line, one for every room. The engine caps a sign at 100
    /// characters. "Core is pure" and "one projection in, intents out" are the
    /// compression a hundred characters buys — `decide` also takes the tick's
    /// assignments and memo and writes the walk tables (`PlanMemo`, #310).
    /// Flavour on a sign, not a specification.
    let signature =
        "A functional bot. F# compiled by Fable; Core is pure — one projection in, intents out"

    /// The colonies a human has declared, moved by a human in a commit: an
    /// entry in this list is the whole of "I mean to take that room", and an
    /// entry with no spawn behind it yet is that intent rather than a mistake.
    /// Declared is not **living**: `Colony.living` is the set `decide` runs
    /// over. **No roster in this comment, deliberately** (#302): the list
    /// below is the roster, and every prose count of it went stale.
    let declared: Colony list =
        [
            {
                Home = "W12S28"
                // The north outpost W12S27, read off the pair above, and
                // W11S28 to the west (2026-09-16, `outpost-wave-2.md`): the
                // question is never which room is best but **which colony
                // can pay**, and this room's terrain is in the world already.
                Outposts =
                    (Outpost.adr0042 |> List.filter (fun o -> o.RoomName = "W12S27"))
                    @ [ Outpost.w11s28 ]
                // The sector Reactor is six crossings away.
                Errands = []
                Mother = None
                // 19,848 T banked here with no walk to spend it on; the
                // terminal stood on 2026-09-17 at (11,43). W15S28 is the one
                // colony that declares the Reactor errand (#349).
                Consignee = Some "W15S28"
            }
            // The second colony: the first colony's outpost until its own
            // spawn stood.
            {
                Home = "W13S28"
                // W13S29 to the south (2026-09-07), the survey's first pick.
                //
                // W15S28 and W11S29 are **not** here, and the day each was
                // claimed is why: `childrenWhere` gives a room in both lists
                // to the outpost list, so an owned, spawn-less room left here
                // reads as a room we *mine* — no Reserve, no Post, its spawn
                // site behind a container one hop nearer. W15S28 was claimed
                // at t~305,2xx and not one body crossed until this line
                // changed; W11S29 also paid in haul, this colony anchoring
                // its rock three crossings out while W11S29's colony did the
                // same (#352).
                //
                // W14S28 was declared 2026-09-10, withdrawn the same day (a
                // single spawn cannot raise a nursery two hops out and take a
                // second outpost: haul demand 2,790 → 4,810 the tick it was
                // declared), came back after #352, and **left again on
                // 2026-09-20 to W15S28** on a conversion rate measured over
                // 246 live ticks at t593,825-594,071:
                //
                //   this colony  5 rocks, ~50 e/t in, **15.7 e/t** into the
                //                controller, 849,766 banked and flat
                //   W12S28       4 rocks, ~40 e/t in, 30.7 e/t, Storage at 0
                //   W15S28       3 rocks, ~30 e/t in, 25.3 e/t, Storage falling
                //
                // The colony with the most rocks converts the least, by a
                // factor of two: a hauler row that always has Feeding work
                // never has a spare load for the buffer, and the controller
                // container at 22,15 held nothing while the upgrader read
                // `idle (none-applicable)`. The last step between the rate and
                // the demand is not measured and belongs in a ticket; the
                // farthest rock is the one lever this file has.
                Outposts = [ Outpost.w13s29 ]
                // Five crossings to the Reactor: the 22,000 Thorium banked
                // here is ore nothing here can deliver.
                Errands = []
                Mother = Some "W12S28"
                // 16,464 T banked and a terminal at (14,10); same far end and
                // same reason as W12S28's (#349).
                Consignee = Some "W15S28"
            }
            // The third colony (2026-09-10, `docs/research/third-colony.md`).
            // The entry with no spawn behind it *was* the decision to take
            // the room: it turned the controller W13S28 projected from a
            // Reserve into a Claim.
            {
                Home = "W15S28"
                // W15S27 first (2026-09-16, `w15s27-outpost.md`).
                //
                // W15S29 joined 2026-09-17. The wave-2 survey's CPU refusal
                // ("+1.5-2.0 ms of `decide`") is no longer the price:
                // re-measured over three interleaved rounds, **2.95, 2.95,
                // 3.23 without against 3.51, 3.53, 3.68 with — +0.55 ms**,
                // because a room's far field is now held across ticks (#353,
                // #358). It came back out the same day after **one** invader
                // of 1 ATTACK and 1 RANGED_ATTACK ate four bodies between
                // t517,880 and t519,150: #366's 300-tick memory expired while
                // the raider stayed and the next unarmed body walked in. Back
                // once the memory became `Engine.creepLifetime` (#369) and
                // the fleet was back; delivery restarted at t529,889.
                //
                // **W14S28 since 2026-09-20**, taken off W13S28: a flood over
                // real terrain measures **46 tiles from here against 67 from
                // W13S28**, and this colony had the hauler row to carry it
                // (1,480 of demand over two, against W13S28's 4,730 over
                // three), so the pair sheds haul rather than shifting it.
                // W15S27 is shut to t655,973 on its core's collapse timer and
                // the errand's road through W15S26 is out of every chain
                // while a bunker stands there, so neither draws a hauler
                // today; when W15S27 reopens this is the nearest of the three.
                Outposts = [ Outpost.w15s27; Outpost.w15s29; Outpost.w14s28 ]
                // The one errand there is: the sector Reactor in W15S25, three
                // crossings out. This colony is the only one that can reach
                // it, which is the room's whole reason for being where it is.
                Errands = [ Errand.w15s25 ]
                Mother = Some "W13S28"
                // The far end of the other two colonies' consignments (#349).
                Consignee = None
            }
            // The fourth colony (2026-09-16, `docs/research/fourth-colony.md`).
            // No errand, by eight crossings: its 45,000 Thorium moves by
            // terminal or not at all.
            {
                Home = "W11S29"
                // Its first outpost, one hop east, declared the day after its
                // own spawn stood (2026-09-17, `w12s29-outpost.md`): the
                // cheapest haul in the programme at 81 ticks.
                Outposts = [ Outpost.w12s29 ]
                Errands = []
                Mother = Some "W13S28"
                // Its own 45,000 T — after the banked stock ran out on
                // 2026-09-18 the **only** ore left that can score — goes into
                // W15S28's terminal (#349); its own walk is four crossings,
                // past `Tuning.MaxHops`. **Declared before the terminal it
                // needs, on purpose**: the pairing is inert without one, and
                // the tick the Layout stands it the ore has a path with nobody
                // having to remember — a hand edit at a moment no alarm
                // watches is how 36,484 T sat unshipped until #349.
                Consignee = Some "W15S28"
            }
        ]

    /// The outposts one home room works: its own declaration's, and none at
    /// all for a room nobody declared, so a slip in the constant costs the
    /// colony its outposts rather than putting it in a state nothing has a
    /// rule for.
    let outpostsOf (colonies: Colony list) (home: string) : Outpost list =
        colonies
        |> List.tryFind (fun colony -> colony.Home = home)
        |> Option.map (fun colony -> colony.Outposts)
        |> Option.defaultValue []

    /// The [[errand]]s one home room runs, on `outpostsOf`'s rule.
    let errandsOf (colonies: Colony list) (home: string) : Errand list =
        colonies
        |> List.tryFind (fun colony -> colony.Home = home)
        |> Option.map (fun colony -> colony.Errands)
        |> Option.defaultValue []

    /// Every declared colony's home room, in declaration order
    /// (`ColonyView.Declared`): which rooms a human means to own is not a
    /// thing vision can answer.
    let homes (colonies: Colony list) : string list =
        colonies |> List.map (fun colony -> colony.Home)

    /// The **living** colonies: those whose home room is ours *and* holds one
    /// of our spawns, in declaration order. Two facts and not one, because a
    /// [[candidate colony]] owns nothing and spawns nothing, and a [[nursery]]
    /// is owned with no spawn of its own. A spawn room no declaration names is
    /// a colony of its own, with no outposts and no mother, and only when
    /// nothing declared is living. Empty when the world holds no owned spawn
    /// room at all.
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
                    // No errand, no mother, nothing to ship: each is a thing
                    // a human wrote down, and an invented one would buy
                    // bodies for a constant's slip.
                    Errands = []
                    Mother = None
                    Consignee = None
                })
            |> Option.toList
        | living -> living

    /// One colony's [[stage]] this tick, off the three facts that decide it.
    /// **The one place `Tuning.BootstrapLevel` is read**: no rule compares a
    /// controller level of its own. `None` for a room that is not a colony at
    /// all — a declared home nobody has claimed yet is a **candidate colony**,
    /// whose one rule is the Claim pool — and every reader's answer for `None`
    /// is the one it already gives that colony.
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

    /// The declared children of this colony a further rule picks out: a child
    /// of mine, not an outpost, not me. A room in both lists is the outpost
    /// list's — worked and not raised.
    let private childrenWhere (colonies: Colony list) (rule: string -> bool) (colony: Colony) =
        let worked = colony.Outposts |> List.map (fun outpost -> outpost.RoomName)

        colonies
        |> List.filter (fun child ->
            child.Mother = Some colony.Home
            && child.Home <> colony.Home
            && not (List.contains child.Home worked)
            && rule child.Home)
        |> List.map (fun child -> child.Home)

    /// The rooms one colony **bootstraps** this tick: the homes of its
    /// children that are not yet `Independent`, which the mother projects and
    /// works two Tasks in — the child's Upgrade and its Build. The stages are
    /// handed in off the world (`World.stages`), because this is the rule that
    /// *decides* the scan set. **Both stages before independence**: a child
    /// with no spawn standing is a [[nursery]] again, and the mother is the
    /// only colony that can put the spawn site back up. A room with no stage
    /// is not bootstrapped; a child that stops being ours is `reclaiming`'s.
    let bootstrapping
        (stages: Map<string, ColonyStage>)
        (colonies: Colony list)
        (colony: Colony)
        : string list =
        colony
        |> childrenWhere colonies (fun home ->
            Map.tryFind home stages |> Option.exists (fun stage -> stage <> Independent))

    /// The declared children of this colony that have stopped being ours, and
    /// are nobody else's either. Without this a child whose spawn was
    /// destroyed and whose controller was then lost left every projection
    /// there was, and only a human's edit could take the room back.
    /// **Unowned and never a rival's**: a room somebody else holds is the
    /// [[stand-down]]'s business.
    let reclaiming (unowned: Set<string>) (colonies: Colony list) (colony: Colony) : string list =
        colony |> childrenWhere colonies (fun home -> Set.contains home unowned)

    /// The rooms one colony projects this tick: its home and its worked
    /// [[outpost]]s, its [[errand]]s, and the rooms it bootstraps. The whole
    /// scan set in one sentence, here and not in the shell, because the
    /// entity lists the Task pool is built from are swept over it too.
    let roomsProjected
        (outposts: Outpost list)
        (errands: Errand list)
        (bootstrap: string list)
        (home: string)
        : string list =
        Outpost.roomsProjected outposts home
        @ Errand.roomsProjected errands home
        // A borrowed room carries its transit rooms exactly as an outpost
        // does: a Task in a room no chain reaches is priced at `None`, so a
        // nursery two hops out would be projected and no pioneer could be
        // sent to it — found live on 2026-09-10, when W15S28 was claimed two
        // hops from its mother.
        @ (bootstrap
           |> List.collect (fun room -> room :: RoomName.transitBetween home room))
        |> List.distinct

    /// The colony that cast one creep, read off its own name: creep names are
    /// `{pattern}-{tick}-{spawn}`. None when no known spawn's name is in it.
    let private castBy (spawnHomes: (string * string) list) (creep: string) : string option =
        spawnHomes
        |> List.filter (fun (spawn, _) -> creep.Contains spawn)
        |> List.sortByDescending (fun (spawn, _) -> (spawn: string).Length)
        |> List.tryHead
        |> Option.map snd

    /// Which colony each creep belongs to this tick, keyed by creep name: the
    /// colony that **cast** it, unless it stands in a room only some *other*
    /// colony projects, in which case that colony **adopts** it for the tick
    /// — a body outside every room its own colony projects has no tile there.
    /// One colony's business and never two's, or two decisions would move one
    /// body twice. Only when exactly one other colony projects the room; a
    /// room nobody projects adopts nobody.
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
                // names.
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
