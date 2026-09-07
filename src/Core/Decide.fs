module Fabot.Core.Decide

open Fabot.Core.Types

/// A Body pattern: the repeating part block a body is generated from.
/// Which pattern a spawn casts is a colony decision; the pattern shapes
/// what a creep is good at, never what it is assigned (ADR 0006).
type BodyPattern = { Name: string; Block: BodyPart list }

/// The generalist pattern: 200 energy, full speed empty, half speed loaded.
let workerPattern =
    {
        Name = "worker"
        Block = [ Work; Carry; Move ]
    }

/// The Anchor pattern: the heavy-WORK body cast for a Dual Seat (ADR 0006).
/// The block is its minimal cast — two Work keep the body readable as an
/// Anchor (Work > Move, which fatigue parity forbids a worker body) beside the
/// single Carry and the single Move that pay the walk to the seat.
let anchorPattern =
    {
        Name = "anchor"
        Block = [ Work; Work; Carry; Move ]
    }

/// The hauler unit (ADR 0012): 150 energy, full speed loaded on roads — the
/// row carries its own road-parity declaration, because a hauler's whole life
/// is the trunk. No Work part, so it lives in the Withdraw->Refill cycle.
let haulerPattern =
    {
        Name = "hauler"
        Block = [ Carry; Carry; Move ]
    }

/// The reserver row (ADR 0042): the CLAIM body that walks to an outpost's
/// controller and holds its reservation, which is what makes that room's
/// sources worth ten a tick rather than five. `[2Claim;2Move]` pays for itself
/// twice over on a single source.
let reserverPattern =
    {
        Name = "reserver"
        Block = [ BodyPart.Claim; Move ]
    }

/// The upgrader row (ADR 0046): the body that stands beside the upgrade buffer
/// and spends the colony's surplus into the controller. Every part slot past
/// its single Carry goes to a Work/Move pair, so nothing in the body pays for a
/// commute it does not make. One Carry, because a body that stands still needs
/// exactly enough store to hold a Withdraw from the buffer at its feet (ADR
/// 0019).
let upgraderPattern =
    {
        Name = "upgrader"
        Block = [ Work; Carry; Move ]
    }

/// The guard row (ADR 0056): the melee body cast the tick a [[threat]] is seen
/// standing in a declared [[outpost]], and never before. 750 energy, ten parts,
/// 1,000 hits, 90 damage and 12 self-heal a tick — the body Overmind and bonzAI
/// converged on independently, and the one melee beats ranged on arithmetic at
/// this budget: an ATTACK part is 0.231 damage per energy counting the Move
/// that carries it, against a RANGED_ATTACK's 0.050. One Move per non-Move
/// part, so ADR 0003's fatigue parity holds with none of the [[anchor]]'s
/// exemption. The block's order is the body's: TOUGH eats damage first and HEAL
/// dies last, which is what the community's ordering is for.
let guardPattern =
    {
        Name = "guard"
        Block = [ Tough; Attack; Attack; Attack; Move; Move; Move; Move; Move; Heal ]
    }

/// The pattern table: every body the colony casts is a row here, sized by
/// energy under the row's own sizing rule. A future pattern is one more data
/// row plus its own quota rule, never a new code path (ADR 0006).
let patternTable =
    [
        workerPattern
        anchorPattern
        haulerPattern
        reserverPattern
        upgraderPattern
        guardPattern
    ]

let bodyCost body =
    body
    |> List.sumBy (function
        | Work -> 100
        | Carry -> 50
        | Move -> 50
        | Attack -> 80
        | RangedAttack -> 150
        | Heal -> 250
        | BodyPart.Claim -> 600
        | Tough -> 10)

/// The Anchor row's Work ceiling (ADR 0021): the Work that saturate one source
/// — dig its whole regeneration in the regeneration time — plus one spare. Past
/// saturation a further Work only drains the source sooner and idles; the spare
/// drains it 50 ticks early, and those ticks absorb an unmanned Post's gap at
/// no cost. A rule about one source's regeneration and never about heavy bodies
/// in general, so ADR 0042 narrows it by changing its input.
let private workCapOf output = output / Engine.harvestPerWork + 1

/// The ceiling in a room the colony holds: six Work, the number ADR 0021
/// derived.
let private heldWorkCap = workCapOf Engine.heldOutputPerTick

/// The worker row's sizing rule: the largest affordable repetition of the block
/// (never below one repeat), with the remainder spent on Carry/Move at fatigue
/// parity — the padded body is never slower than the pure-block body, empty or
/// loaded, and within that buys as much Carry as possible (ADR 0003, narrowed
/// to the worker pattern by ADR 0006). Parts are grouped Work, Carry, Move so
/// damage strips Work first and mobility last. It is the rule every row without
/// one of its own falls through to, and it can only place the three parts it
/// counts, so a *shape* it cannot size is a hard stop rather than a quiet
/// omission: a block holding a guard's Attack would be silently rebuilt out of
/// Work, Carry and Move, and an empty one divides by zero on .NET while the
/// emitted JS reads `capacity / 0` as no repeats at all and pads a Carry/Move
/// body out of a row that asked for neither — which is why the stop is
/// explicit and not left to the arithmetic.
let private parityBodyFor (pattern: BodyPattern) capacity =
    let block = pattern.Block

    if List.isEmpty block then
        failwith (
            $"body pattern '{pattern.Name}' holds no parts at all, which the generalist sizing rule "
            + "cannot size: it buys whole repeats of a block, and an empty block has no repeat to "
            + "buy. Give this row a block, or its own sizing rule beside anchor/hauler/reserver "
            + "(ADR 0006)."
        )

    let unplaceable =
        block
        |> List.filter (fun part -> part <> Work && part <> Carry && part <> Move)
        |> List.distinct

    if not (List.isEmpty unplaceable) then
        let parts = unplaceable |> List.map string |> String.concat ", "

        failwith (
            $"body pattern '{pattern.Name}' holds {parts}, which the generalist sizing rule cannot "
            + "place: it counts Work, Carry and Move out of a block and emits only those, so the "
            + $"body it would return holds no {parts} at all. Give this row its own sizing rule "
            + "beside anchor/hauler/reserver (ADR 0006)."
        )

    let blockSize = List.length block
    let carryCost = bodyCost [ Carry ]
    let moveCost = bodyCost [ Move ]

    let blockCount part =
        block |> List.filter ((=) part) |> List.length

    let repeats =
        capacity / bodyCost block |> max 1 |> min (Engine.maxBodyParts / blockSize)

    // Loaded parity is work + carry <= 2 * move: a lone Carry is added
    // only under that bound, a Carry+Move pair preserves it, and a lone
    // Move (the trailing 50) only widens it.
    let rec pad work carry move budget slots =
        if slots >= 1 && budget >= carryCost && work + carry + 1 <= 2 * move then
            pad work (carry + 1) move (budget - carryCost) (slots - 1)
        elif slots >= 2 && budget >= carryCost + moveCost then
            pad work (carry + 1) (move + 1) (budget - carryCost - moveCost) (slots - 2)
        elif slots >= 1 && budget >= moveCost then
            pad work carry (move + 1) (budget - moveCost) (slots - 1)
        else
            work, carry, move

    let work, carry, move =
        pad
            (repeats * blockCount Work)
            (repeats * blockCount Carry)
            (repeats * blockCount Move)
            (capacity - repeats * bodyCost block)
            (Engine.maxBodyParts - repeats * blockSize)

    List.replicate work Work @ List.replicate carry Carry @ List.replicate move Move

/// The anchor row's sizing rule: one Carry, one Move, and every part slot the
/// remaining energy affords on Work up to the row's ceiling — spawn energy buys
/// output rather than mobility the Post never uses, and stops where the source
/// has no more to give (ADR 0021). Exempt from fatigue parity (ADR 0006); never
/// below the row's two-Work block.
let private anchorBodyFor workCap capacity =
    let work =
        (capacity - bodyCost [ Carry; Move ]) / bodyCost [ Work ]
        |> max 2
        |> min workCap

    List.replicate work Work @ [ Carry; Move ]

/// The whole-block rows' shared arithmetic: as many whole blocks as the
/// capacity buys, never below one and never past the engine's 50-part cap, with
/// the parts grouped by kind in the block's own order — so damage strips a
/// row's output before its legs.
let private wholeBlockBodyFor (block: BodyPart list) capacity =
    let repeats =
        capacity / bodyCost block
        |> max 1
        |> min (Engine.maxBodyParts / List.length block)

    block
    |> List.distinct
    |> List.collect (fun part ->
        let perBlock = block |> List.filter ((=) part) |> List.length
        List.replicate (repeats * perBlock) part)

/// The hauler row's sizing rule (ADR 0012): as many whole [Carry; Carry; Move]
/// blocks as capacity buys (never below one), and nothing else. The row's
/// parity declaration is road parity — two loaded Carry generate two fatigue on
/// a road tile, the one Move pays off two a tick — which the whole block meets
/// and a padded lone Carry would break.
let private haulerBodyFor capacity =
    wholeBlockBodyFor haulerPattern.Block capacity

/// The reserver row's sizing rule: as many whole [Claim; Move] blocks as
/// capacity buys, never below one. The bank's truncation alone, which is half
/// the row's rule — ADR 0042 sizes the body off the reservation deficit *capped
/// by the bank*, and `reserverBodyWithin` is where the two halves meet. This
/// entry point is the one `bodyFor` exposes, so a reader holding only a
/// capacity gets the largest body the row could cast and therefore the longest
/// lead, which is the safe direction: a successor is cast early rather than
/// after its incumbent died.
let private reserverBodyFor capacity =
    wholeBlockBodyFor reserverPattern.Block capacity

/// The guard row's sizing rule (ADR 0056): as many whole
/// `[Tough; Attack×3; Move×5; Heal]` blocks as capacity buys, never below one
/// and capped at five by the engine's 50 parts — the rule the hauler and
/// reserver rows already share, chosen for this row because the block is
/// already at fatigue parity and a remainder spent at ADR 0003's parity would
/// buy Carry a guard has no use for. `List.distinct` keeps the block's order,
/// so two blocks are `[T;T; A×6; M×10; H;H]` and not a shuffle of them. What
/// the banks buy: 300 cannot afford one block at all and the row **yields**
/// (ADR 0050), 800 and 1,300 buy one, 1,800 two and 2,300 three.
let private guardBodyFor capacity =
    wholeBlockBodyFor guardPattern.Block capacity

/// The upgrader row's sizing rule (ADR 0046): one Carry, and every part slot
/// the rest of the capacity affords spent on Work/Move **pairs** — `W = M =
/// floor((capacity - 50) / 150)`, never below one pair. The gain over the
/// generalist is the parts it would spend carrying energy to work it is not
/// going to do standing still. Why the Move parts at all, for a body that
/// stands: ADR 0016's gate is `Work > Move`, and a body over that line may not
/// Withdraw — which is the buffer this row exists to drink from (ADR 0019). So
/// pairing keeps the row at `Work = Move`, inside the gate.
let private upgraderBodyFor capacity =
    let pairs =
        (capacity - bodyCost [ Carry ]) / bodyCost [ Work; Move ]
        |> max 1
        |> min ((Engine.maxBodyParts - 1) / 2)

    List.replicate pairs Work @ [ Carry ] @ List.replicate pairs Move

/// Body for a pattern at an energy capacity, under the row's own sizing rule
/// (ADR 0006): the anchor row spends on Work beside its fixed Carry/Move pair,
/// the hauler, reserver and guard rows buy whole blocks, the upgrader row buys
/// Work/Move pairs beside one Carry, and every other row pads its remainder at
/// plain fatigue parity — or, if its block holds a part that rule cannot place,
/// is refused rather than sized into some other body. A capacity is the whole
/// of what this entry point holds, so the two rows whose real rule reads a
/// second fact — the anchor's Post (ADR 0053) and the reserver's deficit — are
/// answered here at their **largest** body.
let bodyFor pattern capacity =
    if pattern.Name = anchorPattern.Name then
        anchorBodyFor heldWorkCap capacity
    elif pattern.Name = haulerPattern.Name then
        haulerBodyFor capacity
    elif pattern.Name = reserverPattern.Name then
        reserverBodyFor capacity
    elif pattern.Name = guardPattern.Name then
        guardBodyFor capacity
    elif pattern.Name = upgraderPattern.Name then
        upgraderBodyFor capacity
    else
        parityBodyFor pattern capacity

/// The generalist body: the worker row of the pattern table, sized to
/// capacity.
let workerBodyFor capacity = bodyFor workerPattern capacity

/// Stable identity of a Task across ticks; what Assignments point at.
let taskId =
    function
    | Harvest sourceId -> $"harvest:{sourceId}"
    | Withdraw storeId -> $"withdraw:{storeId}"
    | Refill structureId -> $"refill:{structureId}"
    | Build siteId -> $"build:{siteId}"
    | Repair structureId -> $"repair:{structureId}"
    | Upgrade controllerId -> $"upgrade:{controllerId}"
    | Reserve controllerId -> $"reserve:{controllerId}"
    | Claim controllerId -> $"claim:{controllerId}"
    | Pickup pileId -> $"pickup:{pileId}"
    // One Flee for the whole colony: it has no target to be identified by,
    // and every creep inside a Reach is running from the same thing.
    | Flee -> "flee"
    // One Guard per raided room, identified by the room and never by what
    // stands in it (ADR 0056): a raid that loses a creep, or moves one a tile,
    // is the same fight and must be the same identity, or anti-thrash would
    // re-match the [[guard]] mid-swing.
    | Guard room -> $"guard:{room}"

/// The **target** inside a Task id: the engine's own object id, which is
/// everything `taskId` writes after its one colon. `None` for Flee, the one id
/// naming no target. Read by the vision grace, which holds an id and no Task —
/// the Task it named has left the pool — and stays blind to Task kinds doing
/// it (ADR 0052 decision 6): what it needs is the id the room's census files
/// that target under, and every kind spells that the same way.
let private taskTarget (tid: string) : string option =
    match tid.IndexOf ':' with
    | -1 -> None
    | colon -> Some(tid.Substring(colon + 1))

/// The vision grace's one question (#151): the room a held Task's target was
/// last seen standing in, when that room has gone **dark** inside
/// `Tuning.VisionGrace` and not longer. `None` is every other answer — the room
/// is in view this tick, it was never seen, the darkness has outlasted the
/// grace, or the id names no target at all.
///
/// A Task leaves the pool for two opposite reasons and its id alone cannot tell
/// them apart: the target was destroyed, or the room carrying it went dark. The
/// pool stays gated on vision and rightly so (ADR 0004) — a blind room hands
/// over an empty site list and no rule may price what nobody can see — so what
/// this asks is about **looking** and never about the target. Two readers, and
/// they are the two halves of one rule: the Matcher keeps the assignment, and
/// the mover walks its holder at the room the answer names (`Atlas.stepTowardRoom`).
/// Written once because a second spelling is free to disagree about which of
/// them is holding a creep still.
let private lastSeenIn (view: ColonyView) (tid: string) : string option =
    match taskTarget tid with
    | None -> None
    | Some target ->
        view.Sightings
        |> Map.tryPick (fun room sighting ->
            if
                sighting.Tick < view.Time
                && view.Time - sighting.Tick <= view.Tuning.VisionGrace
                && Set.contains target sighting.Targets
            then
                Some room
            else
                None)

/// The [[stage]] of the colony whose home is the named room, off the one
/// derivation the shell ran for the tick (ADR 0052 decision 3). `None` for a
/// room no colony of ours lives in — undeclared, unclaimed, or one nothing
/// could see — which is the answer every reader here already gives such a room.
let private roomStage (view: ColonyView) room = Map.tryFind room view.Stages

/// This colony's own stage: its home room's entry. Always present for a living
/// colony (ADR 0047 decision 1), so `None` is the projection that cannot place
/// its own controller, and every reader gives that colony the answer it gives
/// one standing under the line.
let private homeStage (view: ColonyView) =
    roomStage view (SpatialInfo.homeName view.Spatial)

/// Whether this colony has outgrown its bootstrap window: `Independent`,
/// at `Tuning.BootstrapLevel` or past it (ADR 0052 decision 3). The one
/// question the Layout's two gates and the Repair pool's rampart line ask
/// — a colony below it is still buying the economy those spends serve.
let private isIndependent (view: ColonyView) = homeStage view = Some Independent

/// Whether this colony keeps ramparts this tick (ADR 0034 as #214 amends it):
/// it is `Independent`, past the bootstrap line and one stage past the engine's
/// own unlock. The covering rule's one gate and the floor's one gate, one
/// spelling for both: a colony below it places no rampart and counts none of
/// its standing ramparts hungry, so the three a child raised at RCL2 decay away
/// instead of holding four workers to a floor derived for a home with a tower
/// and a Storage behind it.
let private keepsRamparts (view: ColonyView) = isIndependent view

/// Whether a structure of this kind, carrying these hits, is hungry: its own
/// whole line, read off the kind (ADR 0034). The decaying kinds sit below a
/// fraction of max (ADR 0010), a rampart below the floor, the Keep below full —
/// it does not decay, so below max means damaged. A kind with no line is never
/// hungry. The floor is capped at the structure's own max so a rampart whose
/// max is somehow under it can still be whole.
let private isHungry (tuning: Tuning) kind (hits: HitsInfo) =
    match wholeLine kind with
    | Some WholeLine.Fraction -> float hits.Hits < tuning.RepairTrigger * float hits.HitsMax
    | Some WholeLine.Floor -> hits.Hits < min tuning.RampartFloor hits.HitsMax
    | Some WholeLine.Full -> hits.Hits < hits.HitsMax
    | None -> false

/// Every structure the projection carries hits for that stands below its kind's
/// whole line, with its kind, in id order. The one walk over the hits and the
/// kinds, shared by its two readers: the Repair pool takes all of them, the
/// safe-mode reflex's Keep arm asks only whether one is of the Keep (ADR 0034).
let private hungryStructures (view: ColonyView) : (string * BuiltKind) list =
    let ramparts = keepsRamparts view

    view.Spatial.Hits
    |> Map.toList
    |> List.choose (fun (id, hits) ->
        match Map.tryFind id view.Spatial.TargetKinds with
        // A rampart below the line the colony keeps them from is not
        // hungry: it is decaying away (#214, `keepsRamparts`).
        | Some(Structure BuiltKind.Rampart) when not ramparts -> None
        | Some(Structure kind) when isHungry view.Tuning kind hits -> Some(id, kind)
        | _ -> None)

/// The range a hostile can hurt a creep from, or None for one that cannot (ADR
/// 0033).
let private weaponRange (hostile: HostileInfo) : int option =
    [
        if List.contains Attack hostile.Body then
            Engine.meleeRange
        if List.contains RangedAttack hostile.Body then
            Engine.rangedRange
    ]
    |> function
        | [] -> None
        | ranges -> Some(List.max ranges)

/// The tick's colony-level threat facts (ADR 0033): the tiles a Threat can
/// hurt, and the walkable tiles no Threat can. Derived once a tick and shared
/// by the three readers — the applicability gate that takes the Reach out of
/// every Work Area, Flee's own Work Area, and the spawn hold. Colony facts,
/// never a change to the spatial projection: hostiles still block no tiles and
/// price no paths. Layered by room name, as the projection they are derived
/// from is (ADR 0041): a Reach is a set of one room's tiles and a `Set<Pos>`
/// cannot say which room's, so the room rides on the outer key — the room the
/// hostile stands in, which `HostileInfo` carries for exactly this join. Each
/// reader picks its own room's share, and a room with no entry answers the
/// empty set (ADR 0004): it blocks no action and pools no Flee.
type Threats =
    {
        /// Per room, the tiles a Threat standing in it can hurt. Never an
        /// empty set under a room: a room whose whole Reach our ramparts
        /// took back has no entry, so `Map.isEmpty` is "no Reach
        /// anywhere" — the one question the pool asks of it.
        Reach: Map<string, Set<Pos>>
        /// Per room, the walkable tiles no Threat reaches — Flee's Work Area
        /// for a creep standing there. Derived only for the rooms with a Reach,
        /// where every other room's absence stands for "not derived" rather
        /// than "nowhere is safe": a creep with no Reach around it is matched
        /// to no Flee.
        Safe: Map<string, Set<RoomPos>>
        /// Per room, the walkable tiles within range 1 of a Threat standing in
        /// it, less the tiles the Threats themselves stand on — the [[guard]]'s
        /// Work Area (ADR 0056), and the safe set's exact opposite: Flee walks
        /// a body to the tiles nothing reaches and a Guard walks it to the
        /// tiles that reach *back*. Range 1 and not two, because 30 a part is
        /// paid there and nothing is paid at range 2 and the row carries no
        /// RANGED_ATTACK by decision. Derived here beside the Reach and
        /// touching no [[atlas]] memo, which is ADR 0033's precedent for the
        /// safe set pointed the other way.
        Ring: Map<string, Set<RoomPos>>
    }

/// The tick with nothing to run from: every Work Area stands whole and no
/// creep flees. What the pipeline is handed for a quiet colony.
let noThreats =
    {
        Reach = Map.empty
        Safe = Map.empty
        Ring = Map.empty
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Threats =
    /// One room's Reach; empty for a room no Threat stands in (ADR 0004).
    let reachIn (threats: Threats) (room: string) : Set<Pos> =
        Map.tryFind room threats.Reach |> Option.defaultValue Set.empty

    /// One room's safe set, already joined to that room; empty for a room
    /// no Reach was derived in.
    let safeIn (threats: Threats) (room: string) : Set<RoomPos> =
        Map.tryFind room threats.Safe |> Option.defaultValue Set.empty

    /// One room's range-1 ring, already joined to that room; empty for a room
    /// no Threat stands in — which leaves that room's Guard, if one was ever
    /// pooled for it, applicable to nobody (ADR 0004).
    let ringIn (threats: Threats) (room: string) : Set<RoomPos> =
        Map.tryFind room threats.Ring |> Option.defaultValue Set.empty

/// This tick's Threats, off the view's hostiles and the rampart census, room by
/// room. Each Threat reaches its weapon range plus the margin, in Chebyshev
/// tiles — less every tile under one of our standing ramparts in that same
/// room, which is in no Reach at all: a creep on its own rampart cannot be
/// attacked, and that exemption is what lets an Anchor keep digging on a
/// ramparted Post (ADR 0034).
let threatsOf (view: ColonyView) atlas : Threats =
    // Under safe mode a hostile in a room of ours can hurt nothing — the engine
    // refuses every harmful act there for the whole window — so it is no Threat
    // and has no Reach.
    let shielded room =
        match Map.tryFind room view.RoomControl with
        | Some control -> control.Owner = Ownership.Ours && control.SafeMode
        | None -> false

    // The hostiles are asked first, so a quiet colony walks nothing:
    // neither the rampart census nor any room's own tiles are read on a
    // tick with nothing in it to run from.
    match
        view.Hostiles
        |> List.filter (fun hostile -> not (shielded hostile.Pos.Room))
        |> List.choose (fun hostile ->
            weaponRange hostile
            |> Option.map (fun r -> hostile.Pos.Room, RoomPos.pos hostile.Pos, r))
    with
    | [] -> noThreats
    | threats ->
        let reach =
            threats
            |> List.groupBy (fun (room, _, _) -> room)
            |> List.choose (fun (room, inRoom) ->
                let ramparts = Atlas.ourRampartTilesIn atlas room

                let tiles =
                    inRoom
                    |> List.collect (fun (_, pos, weapon) ->
                        let r = weapon + view.Tuning.ReachMargin

                        [
                            for x in pos.X - r .. pos.X + r do
                                for y in pos.Y - r .. pos.Y + r do
                                    let tile = { X = x; Y = y }

                                    if not (Set.contains tile ramparts) then
                                        tile
                        ])
                    |> Set.ofList

                // Nothing left to run from once our own ramparts have taken
                // the whole Reach back: no Reach, no entry, no safe set to
                // derive.
                if Set.isEmpty tiles then None else Some(room, tiles))
            |> Map.ofList

        // The walkable ground of each room a Threat stands in, walked **once**
        // for the two sets derived from it: the safe set is that ground less
        // the Reach and the ring is the part of it beside a Threat, and
        // `Atlas.walkableTilesIn` builds a 2,500-tile set per call and
        // memoises nothing.
        let walkable =
            threats
            |> List.map (fun (room, _, _) -> room)
            |> List.distinct
            |> List.map (fun room -> room, Atlas.walkableTilesIn atlas room)
            |> Map.ofList

        let walkableIn room =
            Map.tryFind room walkable |> Option.defaultValue Set.empty

        // The range-1 ring of every Threat in a room, walkable and less the
        // tiles the Threats stand on — a body cannot stand where one of them
        // already does, and with two of them adjacent each is a tile of the
        // other's ring. Off the Threat list and not off the Reach above,
        // because the Reach is what our own ramparts subtract from and a
        // rampart is standing room like any other: the tile a guard fights
        // from is the best tile it has, not one it has to flee.
        let ring =
            threats
            |> List.groupBy (fun (room, _, _) -> room)
            |> List.map (fun (room, inRoom) ->
                let standing = inRoom |> List.map (fun (_, pos, _) -> pos) |> Set.ofList
                let walkable = walkableIn room

                let tiles =
                    inRoom
                    |> List.collect (fun (_, pos, _) ->
                        [
                            for x in pos.X - 1 .. pos.X + 1 do
                                for y in pos.Y - 1 .. pos.Y + 1 do
                                    let tile = { X = x; Y = y }

                                    if
                                        Set.contains tile walkable
                                        && not (Set.contains tile standing)
                                    then
                                        tile
                        ])
                    |> Set.ofList

                room, RoomPos.setAt room tiles)
            |> Map.ofList

        {
            Reach = reach
            Safe =
                reach
                |> Map.map (fun room tiles ->
                    Set.difference (walkableIn room) tiles |> RoomPos.setAt room)
            Ring = ring
        }

/// The source container geometry (ADR 0012): a tile within range 1 of the given
/// source is that source's container tile — the Seat-standing kind the Layout
/// places, which harvest overflow fills. The one range this colony calls a
/// source container, asked of one source.
let private servesSource (sourcePos: Pos) (tile: Pos) = range tile sourcePos <= 1

/// The source a tile of the named room is a container's for: the placed source
/// **standing in that same room** within range 1 of it, or None where there is
/// none. The one geometry judgement behind both rules that care about a source
/// container — the Planner keeps them out of Refill, the hauler quota counts
/// them. Unplaced geometry classifies nothing (ADR 0004). The source's identity
/// and not merely its existence, because since ADR 0042 the hauler quota prices
/// a container at *that* source's own output: a room's reservation decides
/// whether the rock under a container is worth ten a tick or five. Of several
/// sources within range 1 the first in view order answers.
let private sourceContainerServes (view: ColonyView) (room: string) (pos: Pos) : string option =
    view.Sources
    |> List.tryFind (fun s ->
        match SpatialInfo.placementOf view.Spatial s.Id with
        | Some source -> source.Room = room && servesSource (RoomPos.pos source) pos
        | None -> false)
    |> Option.map (fun s -> s.Id)

/// Whether a tile of the named room is a source container's at all — the
/// half of the rule above that the Refill pool asks, which needs to know
/// that the tile is spoken for and never which rock spoke for it.
let private isSourceContainerTile (view: ColonyView) (room: string) (pos: Pos) =
    sourceContainerServes view room pos |> Option.isSome

/// Whether this colony owns the named room. Read by the rules that mean *ours*
/// — a [[nursery]] of this colony's, a child it is bootstrapping — where whose
/// room it is decides whose business it is (ADR 0047 decision 4). A room with no
/// control entry is one the colony cannot see, and an unseen room is not one it
/// owns (ADR 0004).
let private colonyOwns (view: ColonyView) room =
    view.RoomControl
    |> Map.tryFind room
    |> Option.exists (fun control -> control.Owner = Ownership.Ours)

/// Whether the named room carries an **owner at all** — one spelling for the
/// two rules of the reserver's that read it. `reserveController` answers
/// ERR_INVALID_TARGET on a controller with an owner, *whoever* it is, so the
/// Reserve pool must not carry its controller and the row must not hire against
/// it: one sentence rather than two gates free to disagree. A rival's counts
/// too, because ADR 0043's stand-down reads the *previous* tick's raid log and
/// the one tick between first sight and the withdrawal cast the bank's whole
/// reserver body at a room the engine refuses.
let private roomHasOwner (view: ColonyView) room =
    view.RoomControl
    |> Map.tryFind room
    |> Option.exists (fun control -> control.Owner <> Ownership.Unowned)

/// The controllers a Claim is pooled for this tick, each with the room it
/// stands in (ADR 0047) — one spelling for the two rules that read it: the Task
/// pool offers exactly these and the reserver row hires one body for each, so
/// the row and its Task cannot disagree (ADR 0006). A **candidate colony** is a
/// declared home this colony does not own yet, and both halves are needed: the
/// declaration, because claiming a room is a human's decision and no projected
/// fact distinguishes a room we mean to own from a neighbour we merely mine;
/// and the ownership, because the tick the claim lands the room stops being a
/// candidate and this pool empties itself, with no state kept. A room nothing
/// looked into is not one it can claim (ADR 0004).
let private claimTargets (view: ColonyView) : (string * string) list =
    let takeable room =
        match Map.tryFind room view.RoomControl with
        | Some control ->
            control.Owner = Ownership.Unowned
            && control.Reservation
               |> Option.forall (fun held -> held.Holder = ReservationHolder.Ours)
        | None -> false

    let candidate room =
        List.contains room view.Declared && takeable room

    view.Spatial.TargetKinds
    |> Map.toList
    |> List.choose (fun (id, kind) ->
        if kind = Controller then
            match SpatialInfo.placementOf view.Spatial id with
            | Some tile when candidate tile.Room -> Some(id, tile.Room)
            | _ -> None
        else
            None)

/// Whether the named room is this colony's **nursery**: a declared colony of
/// ours that has been claimed and has no spawn of its own yet (ADR 0047
/// decision 4). Its home goes on being projected as this colony's [[outpost]],
/// and three rules read that state — every site in it is feeding-tier work, the
/// concurrent-builder budget does not reach those sites, and the worker row
/// hires `Tuning.PioneerCount` more bodies — so it is one spelling and not
/// three gates free to disagree. Two facts: the room's **stage** is `Nursery`
/// (ADR 0052 decision 3), which carries the declaration, the ownership and the
/// missing spawn in one derivation, and **this colony projects the room**
/// (`colonyOwns`), which is what keeps a second declarer from hiring
/// [[pioneer]]s for a child it never projects.
let private isNurseryRoom (view: ColonyView) room =
    room <> SpatialInfo.homeName view.Spatial
    && colonyOwns view room
    && roomStage view room = Some Nursery

/// Whether the named room is a child colony this one is still **bootstrapping**
/// (ADR 0047 decision 4): a declared colony of ours that stands its own spawn —
/// so it runs its own `decide` and is nobody's [[nursery]] any more — and that
/// this colony is nonetheless projecting. Two rules read it: the child's
/// controller joins this colony's Upgrade pool, and the worker row keeps hiring
/// `Tuning.PioneerCount` bodies. `isNurseryRoom`'s two facts with the stage
/// inverted, so the two are complements over one room. **Both standing
/// stages**, because what closes the borrowing is the scan set and never a
/// level read here (ADR 0047's Consequences): while a human still declares the
/// child's room as one of this colony's `Outposts` the room is in the scan set
/// through the outpost reading, which asks no stage.
let private isBootstrapRoom (view: ColonyView) room =
    room <> SpatialInfo.homeName view.Spatial
    && colonyOwns view room
    && (match roomStage view room with
        | Some Bootstrapping
        | Some Independent -> true
        | Some Nursery
        | None -> false)

/// Whether an Upgrade in this pool is **borrowed**: its controller is not this
/// colony's own, so it is a bootstrapped child's, pooled by `planTasks` for the
/// pioneers (ADR 0047 decision 4).
let private isBorrowedUpgrade (view: ColonyView) controllerId =
    view.Controller |> Option.exists (fun c -> c.Id = controllerId) |> not

/// The [[ferry]]'s sinks (ADR 0052 decision 7): the upgrade buffers of the
/// children this colony is **bootstrapping**. Three readers must name the same
/// store or the colony hires a body for a Task nobody pooled — the hauler
/// quota's ferry term, `planTasks`' Refill, and `planPool`'s bound. Read off
/// what the view carries and not off the geometry again: the one store of a
/// child's a mother's view holds is this one (`ColonyView.ferrySink`), and the
/// join cannot be respelled here because it subtracts the tiles beside the
/// child's **sources**, which the same narrowing drops. Total (ADR 0004).
let private ferryBuffers (view: ColonyView) : Set<string> =
    let rooms =
        view.Borrowed.Rooms
        |> List.filter (fun room -> roomStage view room = Some Bootstrapping)
        |> Set.ofList

    if Set.isEmpty rooms then
        Set.empty
    else
        view.Spatial.Stores
        |> Map.toList
        |> List.map fst
        // A container and not merely a store: a buffer is one, and the
        // kind is the one half of the join that survives the narrowing, so
        // asking it costs nothing and keeps a tombstone or a pile lying in
        // that room out of a Refill it could never be filled through.
        |> List.filter (fun id ->
            Map.tryFind id view.Spatial.TargetKinds = Some(Structure BuiltKind.Container)
            && (SpatialInfo.placementOf view.Spatial id
                |> Option.exists (fun tile -> Set.contains tile.Room rooms)))
        |> Set.ofList

/// The declared [[outpost]]s this colony works this tick — the rooms two rows
/// are hired per, written once because a paraphrase would let the reserver row
/// and the guard row disagree about which rooms are ours to work (ADR 0042, ADR
/// 0056). A room qualifies by carrying **a controller of its own in the
/// projection** that is not this colony's home: an outpost's declaration names
/// its controller and a room with none is no candidate outpost at all. It must
/// be unowned — a room somebody holds is one this colony is withdrawing from,
/// not mining — and it must not be a [[candidate colony]]'s, whose controller
/// carries a Claim rather than a Reserve.
///
/// **Declared and not posted**, which is where #131's correction overrides ADR
/// 0042's "one reserver per posted outpost" clause: gating on a standing
/// container deadlocks the outpost chain, since the container needs vision,
/// vision needs a creep, and the reserver is the only creep with a reason to
/// go. The scan set is the gate that remains, and two things narrow it, both
/// inside `World.scanOf`: ADR 0043's stand-down, and the declaration's own
/// geometry — its `Outpost.neighbouring` filter (#243) — a room its home shares
/// no border with is joined by no [[seam]], so a body hired for it could never
/// walk there, and the room is out of the scan set before anything here counts
/// it. What *says* so is `Outpost.refused` on the [[layout record]]; what
/// narrows the set is the filter.
///
/// Off the projection itself (`SpatialInfo.placementOf`) and not off the
/// [[atlas]]'s join of it, which answers alike: the [[guard]]'s own Task is
/// pooled by `planTasks`, and the Planner's first half is handed the view and
/// no Atlas (ADR 0056 decision 2). One derivation for the two rows and the
/// pool, or the three of them could disagree about which rooms are ours.
let private declaredOutposts (view: ColonyView) : string list =
    let home = view.Controller |> Option.map (fun c -> c.Id)
    let claimed = claimTargets view |> List.map snd |> Set.ofList

    view.Spatial.TargetKinds
    |> Map.toList
    |> List.choose (fun (id, kind) ->
        if kind = Controller && Some id <> home then
            SpatialInfo.placementOf view.Spatial id |> Option.map (fun tile -> tile.Room)
        else
            None)
    |> List.distinct
    |> List.filter (roomHasOwner view >> not)
    |> List.filter (fun room -> not (Set.contains room claimed))

/// The declared [[outpost]]s a [[threat]] stands in this tick (ADR 0056): the
/// rooms the guard row hires a body for, and the rooms `planTasks` pools a
/// Guard in. One derivation for both halves, which is decision 2's "one number
/// computed once and read as both the row's quota and the Task's cap" stated
/// for the rooms that number is summed over: pooled where nothing is hired, the
/// Guard would hold a body a raided room needs elsewhere; hired where nothing
/// is pooled, the cast body would stand idle in the oven's shadow.
///
/// ADR 0033's own Threat test and never "a hostile": a scout or a healer alone
/// reaches nothing, takes no ground and is no reason to buy a body. Vision is
/// the whole of what it reads (ADR 0004) — an outpost the colony cannot see
/// carries no hostiles and asks for no guard, the same zero a quiet room
/// contributes.
let private guardedOutposts (view: ColonyView) : string list =
    if List.isEmpty view.Hostiles then
        []
    else
        declaredOutposts view
        |> List.filter (fun room ->
            view.Hostiles
            |> List.exists (fun h -> h.Pos.Room = room && (weaponRange h |> Option.isSome)))

/// Planner: rebuild this tick's full Task pool from the colony view. Pure and
/// from scratch every tick — Tasks are never persisted.
let planTasks (view: ColonyView) (threats: Threats) : Task list =
    // Flee exists while a Reach does (ADR 0033): one Task for the whole
    // colony, at the head of the pool as its Safety tier is at the head of
    // the ranking. No Reach, no Flee — a quiet tick's pool is the pool it
    // always was.
    let flees = if Map.isEmpty threats.Reach then [] else [ Flee ]

    // One Guard per declared [[outpost]] a [[threat]] stands in (ADR 0056),
    // keyed on the room: the same list the guard row hires against, so the body
    // the cascade buys has a Task waiting for it and no Task waits for a body
    // nobody bought. Beside Flee at the head of the pool — the two Tasks of the
    // Safety tier, disjoint by [[body class]], so nothing ever asks how they
    // order.
    let guards = guardedOutposts view |> List.map Guard

    // Harvest exists for every source, drained or not (ADR 0013, revised by
    // ADR 0025): the task no longer flickers with the source's stock, because
    // whether a dry rock is worth walking to depends on the walker's body and
    // position — the Matcher's knowledge, not the creep-blind Planner's.
    let harvests = view.Sources |> List.map (fun s -> Harvest s.Id)

    // The flow's sink, as **one** Task (ADR 0054): the [[refill cluster]] — the
    // colony's spawn and every extension of it — is pooled under the spawn's id
    // and stands while any member has room, so `task-gone` fires when the whole
    // ring is full instead of once per extension somebody else got to first.
    let cluster = RefillCluster.ofRefillables view.Refillables

    let clustered =
        cluster
        |> Option.map (fun c -> c.Members |> Map.toList |> List.map fst |> Set.ofList)
        |> Option.defaultValue Set.empty

    let refills =
        (cluster
         |> Option.filter (fun c -> RefillCluster.free c > 0)
         |> Option.map (fun c -> Refill c.Spawn)
         |> Option.toList)
        @ (view.Refillables
           |> List.filter (fun r -> r.FreeCapacity > 0 && not (Set.contains r.Id clustered))
           |> List.map (fun r -> Refill r.Id))

    let builds = view.ConstructionSites |> List.map (fun site -> Build site.Id)

    // A Repair per repairable structure below its kind's whole line, in id
    // order (ADR 0010, ADR 0034).
    let repairs = hungryStructures view |> List.map (fst >> Repair)

    // The ids of one projected kind, in id order. The containers, the
    // Storage and the controllers are all pooled by the projection's kind
    // — never by position, never by name — so the rule is written once.
    let idsOfKind kind =
        view.Spatial.TargetKinds
        |> Map.toList
        |> List.choose (fun (id, k) -> if k = kind then Some id else None)

    // The colony's own controller, and the controller of every child it is
    // still bootstrapping (ADR 0047 decision 4) — half of the one cross-colony
    // borrowing rule there is: a loaded worker of the mother's may cross the
    // Seam and spend into the child's controller until that controller reaches
    // `Tuning.BootstrapLevel`. Surplus tier, like the home Upgrade it stands
    // beside, so the mother's own flow is fed first and nothing but travel cost
    // separates the two — which is what leaves the child's to the bodies
    // already standing in its room.
    let upgrades =
        let own = view.Controller |> Option.toList |> List.map (fun c -> c.Id)

        let children =
            idsOfKind Controller
            |> List.filter (fun id ->
                SpatialInfo.placementOf view.Spatial id
                |> Option.map (fun tile -> tile.Room)
                |> Option.exists (isBootstrapRoom view))

        own @ children |> List.map Upgrade

    // One Claim per candidate colony's controller (ADR 0047), read off
    // the one rule that says which those are (`claimTargets`).
    let claims = claimTargets view |> List.map (fst >> Claim)

    // One Reserve per projected controller that is not the colony's own (ADR
    // 0042): a neutral controller held by CLAIM parts pays its room's sources
    // ten a tick instead of five, and the hold decays by one a tick, so the
    // Task stands whatever the reservation has left on it — the ticks remaining
    // size the body, not the pool. Read off the projection's kind census and
    // never off the declared outposts (ADR 0041), so a room a stand-down keeps
    // out of the scan set (ADR 0043) leaves this pool with it rather than
    // through a second gate free to disagree. The colony's own controller is
    // excluded by id, and every controller in a room that carries an owner by
    // the same `roomHasOwner` the reserver row drops the room with, since the
    // engine refuses reserveController on any owned room. A controller the
    // projection does not place names no room and stays pooled (ADR 0004).
    let reserves =
        let home = view.Controller |> Option.map (fun c -> c.Id)
        let claimed = claimTargets view |> List.map fst |> Set.ofList

        let inAnOwnedRoom id =
            SpatialInfo.placementOf view.Spatial id
            |> Option.map (fun tile -> tile.Room)
            |> Option.exists (roomHasOwner view)

        idsOfKind Controller
        |> List.filter (fun id ->
            Some id <> home && not (inAnOwnedRoom id) && not (Set.contains id claimed))
        |> List.map Reserve

    // The haul cycle's intake (ADR 0012), shaped over the projection's
    // stores rather than energy's name: every stocked container yields a
    // Withdraw, at feeding tier beside Harvest — whether to dig or to
    // collect is travel cost's call, never a rule's.
    let stored id =
        view.Spatial.Stores |> Map.tryFind id |> Option.defaultValue 0

    let containers = idsOfKind (Structure BuiltKind.Container)
    let storages = idsOfKind (Structure BuiltKind.Storage)

    // A tombstone and a ruin are stores the same way, so they pool through the
    // same line: a store with energy in it yields a Withdraw, and what will
    // become of the thing holding it is not this pool's question. The engine's
    // `withdraw` takes either object, and the cap and the tier are read off the
    // stock and the kind exactly as a container's are.
    let tombstones = idsOfKind Tombstone

    // The [[ferry]]'s sink is the one store in this pool a body of this
    // colony's may fill and may never draw: it is a child's, and the whole of
    // the lend is energy going one way. Left in, the mother's hauler would take
    // the load she just carried across the Seam straight back out — the ADR 0019
    // cycle over a border.
    let ferrySinks = ferryBuffers view

    // **No store of a child's is hers to draw, at any [[stage]]** (ADR 0047
    // decision 1): what one colony may take of another is the explicit list
    // `BorrowedWork` carries, and no store is on it.
    let borrowedRooms = Set.ofList view.Borrowed.Rooms

    let inABorrowedRoom id =
        SpatialInfo.placementOf view.Spatial id
        |> Option.exists (fun tile -> Set.contains tile.Room borrowedRooms)

    let withdraws =
        containers @ tombstones
        |> List.filter (fun id -> stored id > 0 && not (inABorrowedRoom id))
        |> List.map Withdraw

    // The piles worth walking to: a dropped pile at or over
    // `Tuning.PickupThreshold` is a Feeding-tier Task, and every smaller one is
    // left to the reflex that costs nothing. The amount and nothing else:
    // whether the pile is at somebody's feet already is a fact about a creep,
    // and the Planner is creep-blind by construction (ADR 0013).
    let pickups =
        idsOfKind Dropped
        |> List.filter (fun id -> stored id >= view.Tuning.PickupThreshold)
        |> List.map Pickup

    // The haul cycle's outflow: the controller container is one more Refill
    // target (ADR 0010's target layering, widened by ADR 0012). Which container
    // is the controller's is judged by geometry — it stands inside the Upgrade
    // Work Area the Layout picked it from, while a source container's tile is
    // never a Refill target.
    let containerRefills =
        view.Controller
        |> Option.bind (fun c -> SpatialInfo.placementOf view.Spatial c.Id)
        |> Option.map (fun controller ->
            let controllerRoom = controller.Room
            let controllerPos = RoomPos.pos controller
            let placed = (SpatialInfo.layerOf view.Spatial controllerRoom).TargetPositions

            containers
            |> List.filter (fun id ->
                match Map.tryFind id placed with
                | Some pos ->
                    range pos controllerPos <= 3
                    && not (isSourceContainerTile view controllerRoom pos)
                    && stored id < Engine.containerCapacity
                | None -> false)
            |> List.map Refill)
        |> Option.defaultValue []

    // The colony's stock is the outflow's last stop (ADR 0023): a standing
    // Storage with room is one more Refill target, on the deepest tier of all.
    let storageRefills =
        storages
        |> List.filter (fun id -> stored id < Engine.storageCapacity)
        |> List.map Refill

    // The [[ferry]]'s other half (ADR 0052 decision 7): a bootstrapping child's
    // upgrade buffer is a Refill target of the mother's, on the same tier her
    // own buffer sits on (ADR 0012) — the deepest but the stock's, so nothing
    // she feeds at home waits on it and the load crosses the Seam only when
    // there is nowhere nearer to put it.
    let ferryRefills =
        ferrySinks
        |> Set.toList
        |> List.filter (fun id -> stored id < Engine.containerCapacity)
        |> List.map Refill

    // The stock's other half (ADR 0023): a stocked Storage is a Withdraw source
    // too, but only while the pool holds a Refill whose target is not the stock
    // itself. Its own Refill is deliberately no such sink: counting it would
    // gate the Storage open against itself, and a hauler beside a store that is
    // both its only intake and its only sink cycles energy in and out of it
    // tick after tick. Both halves can still be pooled on one tick, and there
    // the tier gap carries the load away instead of putting it back.
    let storageWithdraws =
        if
            List.isEmpty refills
            && List.isEmpty containerRefills
            && List.isEmpty ferryRefills
        then
            []
        else
            storages |> List.filter (fun id -> stored id > 0) |> List.map Withdraw

    flees
    @ guards
    @ harvests
    // **The piles stand before the Withdraws** (#242). Pool order is the last
    // rung of the Matcher's ladder — what it falls back to once [[priority]],
    // travel cost *and* the crowding load have all three tied, the scored key
    // being `(rank, cost, load)` (`MatchFactor.PoolOrder`) — and of two intakes
    // a body could take at the same rank, for the same walk, with the same
    // crowd already on them, the decaying one is the one to take: a pile loses
    // `ceil(amount / 1000)` a tick and a container loses nothing. The order
    // reaches exact ties and nothing else, so it moves no pair the ladder or
    // the flood had already separated.
    @ pickups
    @ withdraws
    @ refills
    @ builds
    @ repairs
    @ upgrades
    @ reserves
    @ claims
    @ containerRefills
    @ ferryRefills
    @ storageRefills
    @ storageWithdraws

/// What one body of this shape hauls in a trip: its Carry parts at the engine's
/// per-part capacity. Two readers turn Carry parts into energy — the hauler
/// quota divides a source's output over a round trip by it (ADR 0012), and a
/// Withdraw's cap divides its store's stock by it — so the arithmetic is
/// written once and neither can grow a second per-part rule.
let private carryCapacityOf body =
    (body |> List.filter ((=) Carry) |> List.length) * Engine.carryPartCapacity

/// Ceiling division over the quota rows' arithmetic: a quota that came out a
/// fraction of a body hires the whole body (ADR 0012 for the hauler row, ADR
/// 0037 for the worker row), because the fraction a floor drops is demand
/// nobody is hired for. A numerator at or below zero lands at or below zero —
/// F# divides toward zero — and each row's own floor answers for it.
let private ceilDiv numerator divisor = (numerator + divisor - 1) / divisor

/// What one source of a room the colony holds this way is worth per tick (ADR
/// 0042): the held rate in a room this colony owns or reserves, half of it in a
/// room nobody holds. The whole rate rule, so the census signature can sign
/// exactly what the memoised quota reads rather than a paraphrase of it. Owned
/// **or** reserved, never reserved alone: the engine gives a room carrying
/// either the same 3,000 a cycle, and the colony's own room is owned while
/// nothing reserves it, so "reserved, or half" would price the two home sources
/// at five each.
let private heldRateOf (control: RoomControlInfo) =
    if
        control.Owner = Ownership.Ours
        || control.Reservation
           |> Option.exists (fun held -> held.Holder = ReservationHolder.Ours)
    then
        Engine.heldOutputPerTick
    else
        Engine.neutralOutputPerTick

/// One source's **rate** per tick (ADR 0042), read off the room it stands in:
/// what the rock regenerates, and so the ceiling on what anything standing over
/// it can take out. A fact read per source and not a module constant, because a
/// reservation can lapse and quotas sized for the held rate against a source
/// yielding five overbuild their rows twofold. The rate and not the output:
/// what a Post is *worth* is what the body garrisoning it digs, which is this
/// number only while the row's cast can reach it (`sourceOutputOf`). Two
/// readers want the ceiling itself — `postWorkCapsOf`, which would otherwise
/// size the body off a number the body decides, and the cap inside
/// `sourceOutputOf`. None for a source in a room the colony has no vision in,
/// and for one the projection does not place (ADR 0004): unpriceable is not
/// half, and a blind outpost must not hire against income the colony has no
/// evidence for.
let private sourceRateOf (view: ColonyView) atlas (sourceId: string) : int option =
    Atlas.targetRoom atlas sourceId
    |> Option.bind (fun room -> Map.tryFind room view.RoomControl)
    |> Option.map heldRateOf

/// Whether a source is posted: whether a container stands on one of its Seats,
/// or a Dual Seat makes one of them a Post without a structure — the switch
/// that admits a source into the quotas at all (ADR 0042). One spelling, read
/// by the anchor row's ceiling and by the income base's own split, so a rule
/// that narrows what counts as posted cannot narrow it for one of the two
/// alone. Judged in the source's own room, by `Atlas.standingPostsOf` and not
/// by testing its Seats against the home room's Posts: a `Pos` carries no room,
/// so a home Post on an outpost Seat's coordinates would read that outpost
/// source as posted with no container under it — a phantom ten a tick in the
/// income base, and a phantom Anchor place beside it.
let private isPosted atlas (s: SourceInfo) =
    Atlas.standingPostsOf atlas s.Id |> Set.isEmpty |> not

/// **Every [[post]]'s own Work ceiling** (ADR 0021 as ADR 0042 narrows it and
/// ADR 0053 pairs it): the saturation of the rock that Post seats, plus the one
/// spare Work. A source under no reservation regenerates half as much, and six
/// Work on it drain it in 125 ticks and then idle for 175. **A Post and no
/// longer the set**, which is the whole of ADR 0053: folded into one
/// colony-wide `List.max`, the answer was the held ceiling in every state a
/// colony with one posted home source can reach, and an outpost whose
/// reservation had lapsed went on being garrisoned at six Work against a rock
/// giving five for ever. What pairs a body to a rock without a role is not the
/// caster's knowledge but the **vacancy** it is casting into (`planSpawns`,
/// which walks the empty Posts richest first). Two other readers ask this map
/// for a Post they already hold — the amortization, and a [[lead]] pricing the
/// incumbent's successor. Richest first, and every fallback answers the largest
/// ceiling the rule gives, because an over-sized Anchor wastes 300 energy once
/// in 1,500 ticks where an under-sized one loses four energy a tick for its
/// whole life; a Post whose room the colony cannot price keeps the **held**
/// ceiling (ADR 0004). The **ground** census and not the income one, so the map
/// has one entry per Anchor the colony hires and the amortization can charge
/// them one for one. Keyed by the Post's own [[room position]] and never a bare
/// tile (ADR 0041): two sources whose Seats overlap share a Post tile, and the
/// richer rate keeps it.
let private postWorkCapsOf (view: ColonyView) atlas : Map<RoomPos, int> =
    view.Sources
    |> List.collect (fun s ->
        let cap =
            sourceRateOf view atlas s.Id
            |> Option.map workCapOf
            |> Option.defaultValue heldWorkCap

        Atlas.postsOf atlas s.Id |> Set.toList |> List.map (fun tile -> tile, cap))
    |> List.fold
        (fun caps (tile, cap) ->
            match Map.tryFind tile caps with
            | Some held when held >= cap -> caps
            | _ -> Map.add tile cap caps)
        Map.empty

/// What one source is **worth to the quotas that read a store** (ADR 0042 as
/// #208 amends it): what the Anchor row's cast digs there, capped at the rate
/// its room pays. A Post yields what the body garrisoning it takes out of it,
/// and a bank that cannot buy the Work to drain a source does not earn ten a
/// tick because the room would have paid ten — at a 300 bank the row casts
/// `2W/1C/1M`, which digs four, so a child with two Posts read twenty a tick of
/// income it never earned and hired eighteen workers off it. **The row's cast
/// at this bank, and emphatically not the living Anchor's body**: a quota read
/// off a living body oscillates on that body's death, where what the row
/// *casts* is a colony fact (ADR 0006). Unpriceable stays unpriceable (ADR
/// 0004).
let private sourceOutputOf (view: ColonyView) atlas (sourceId: string) : int option =
    sourceRateOf view atlas sourceId
    |> Option.map (fun rate ->
        // The Work the row would cast for this rock's own Post times
        // HARVEST_POWER — the same `anchorBodyFor` triple the amortization
        // charges that Post at, so the two readings cannot drift apart.
        let dug =
            anchorBodyFor (workCapOf rate) view.Bank.Capacity
            |> List.filter ((=) Work)
            |> List.length
            |> (*) Engine.harvestPerWork

        min rate dug)

/// The hauler row's quota rule (ADR 0012) — the row's colony fact, per ADR
/// 0006's law that a row arrives with its quota or not at all: ceil(Sigma over
/// the source containers of round-trip travel ticks to the colony's **sinks** x
/// that container's own source's output, / the cast body's carry capacity), so
/// a farther container hires proportionally more haul capacity and never
/// quietly overflows. No source containers, or unreachable geometry, hire
/// nothing. **The sinks are where this colony's energy is actually spent** (ADR
/// 0052 decision 4), and there are three: the spawn/extension cluster, the
/// controller's [[buffer]] and the [[storage]]. The cluster is one place and
/// not one per spawn — the extensions ring the spawns and a hauler filling them
/// walks to that ring once — so several spawns resolve at the cheapest. Each
/// contributes a leg while it stands and none while it does not. The spawn
/// alone is what this read before, and a child whose buffer sat thirty tiles
/// from its north Post hired **one** hauler off the spawn leg while its
/// containers overflowed: the energy really was flowing to the controller, and
/// the quota was priced as if it flowed to the spawn. **Each container's flow
/// is spread over the sinks it can price**, and that is an admission rather
/// than a measurement: this layer knows what is produced and where it is spent,
/// and nothing here knows in what proportion. It errs **both ways** — larger
/// wherever a sink stands further off than the cluster, smaller wherever one
/// stands nearer — and neither direction is free: a body too many idles, and a
/// body too few leaves a room's income on the ground. A container that can
/// price **no** sink hires nobody (ADR 0004). **One rounding, for the colony**
/// (ADR 0049, succeeding ADR 0012 and ADR 0037 on the granularity alone): the
/// demands are summed first and the ceiling taken once. Rounding each container
/// up on its own bought a body per fraction, because a hauler is not the
/// property of the container it was hired for: a Withdraw's capacity is its own
/// store's stock divided by a hauler load, so the shared integer is spent where
/// the energy actually stands. The cap is a **capacity and not an order** —
/// `tierOf` files every source container's Withdraw on the feeding tier alike
/// and travel cost ranks inside it. What ADR 0012 rejected was the *flat*
/// quota, one hauler per container regardless of distance, and this is the
/// opposite of that. The output is that source's and not the colony's (ADR
/// 0042), which is why the fold resolves each tile back to the rock it serves:
/// a container over an unreserved source ships half as much. And that output is
/// what the Post's garrison digs, capped at the rock's rate (`sourceOutputOf`).
/// Every room the projection carries, and not the colony's own alone (ADR
/// 0042): an outpost's container ships its source's energy home across a
/// border, so it hires haul capacity exactly as a home container does, against
/// `Atlas.haulRoundTripTicks` joined on the Seam band, run once per leg because
/// the loaded body and the empty one are two journeys (ADR 0029, ADR 0030). The
/// room is the container's own throughout, carried beside its tile rather than
/// assumed, because a `Pos` names none (ADR 0041). A container the projection
/// places in no room is priced by nothing and hires nobody (ADR 0004), as is a
/// Seam band the body cannot pay a crossing on.
let private haulerDemandOf (view: ColonyView) atlas : int * HaulDemandRow list * int =
    // Each source container beside the room it stands in and the output of the
    // rock it serves: the tile alone cannot be priced, so a container the
    // projection places in no room, or one the fold cannot resolve to a source
    // whose room it can price, leaves the list here rather than entering the sum
    // at some default rate.
    let sourceContainers =
        view.Spatial.TargetKinds
        |> Map.toList
        |> List.choose (fun (id, kind) ->
            if kind = Structure BuiltKind.Container then
                SpatialInfo.placementOf view.Spatial id
            else
                None)
        |> List.choose (fun container ->
            sourceContainerServes view container.Room (RoomPos.pos container)
            |> Option.bind (sourceOutputOf view atlas)
            |> Option.map (fun output -> container, output))

    // One load, for the whole colony, and the row's own body cast at the
    // richest bank: rounding once (ADR 0049) sums demands before it divides, so
    // every term has to be a fraction of the *same* body or the integer at the
    // end counts nothing.
    let body = bodyFor haulerPattern (view.Bank.Capacity)

    let capacity = carryCapacityOf body

    let home = SpatialInfo.homeName view.Spatial

    // The three sinks, each a **place** and not a structure: a sink is a list
    // of tiles that stand for one destination, and the cheapest of them is that
    // sink's leg. The spawn/extension cluster is the list with more than one
    // entry today, and taking its minimum is the old rule's "of several spawns
    // the cheapest wins" read as what it always was. Every tile is the
    // projection's and never `SpawnInfo.RoomName` (ADR 0041).
    let cluster =
        view.Spawns |> List.choose (fun s -> SpatialInfo.placementOf view.Spatial s.Id)

    // The upgrade buffer, off the one derivation the Withdraw gate and the
    // upgrader row's own quota read (`Atlas.controllerContainers`, ADR 0019):
    // built and in the home controller's Upgrade area, so a container *site*
    // beside the controller is a promise and not yet a sink.
    let buffers =
        Atlas.controllerContainers atlas
        |> Set.toList
        |> List.choose (SpatialInfo.placementOf view.Spatial)

    // The Storage while one stands, in the home room alone: it is the
    // colony's stock (ADR 0023) and a colony banks in one room (ADR 0052
    // decision 1), so a Storage standing anywhere else is somebody else's.
    let storages =
        Atlas.storageTilesIn atlas home |> Set.toList |> List.map (RoomPos.at home)

    let sinks =
        [ "cluster", cluster; "buffer", buffers; "storage", storages ]
        |> List.filter (snd >> List.isEmpty >> not)

    // Each container's own haul, priced at the **dearest** sink it can reach
    // and summed over the colony — the fraction of a hauler it asks for, never
    // that fraction rounded. The dearest and not the mean: a cluster holds a
    // few hundred energy and fills in a trip, so the flow that goes on all day
    // is the flow to the far sink, and a quota sized to the mean hired one body
    // for a room whose both containers stood full with the buffer at zero.
    let rows =
        sourceContainers
        |> List.map (fun (container, output) ->
            let priced =
                sinks
                |> List.map (fun (kind, places) ->
                    {
                        Kind = kind
                        Trip =
                            places
                            |> List.choose (Atlas.haulRoundTripTicks atlas body container)
                            |> function
                                | [] -> None
                                | trips -> Some(List.min trips)
                    })

            let trips = priced |> List.choose (fun sink -> sink.Trip)

            {
                Container = container
                Output = output
                Sinks = priced
                Demand =
                    match trips with
                    | [] -> 0
                    | trips -> output * List.max trips
            })

    let demand = rows |> List.sumBy (fun row -> row.Demand)

    // The [[ferry]] (ADR 0052 decision 7): the bodies a mother lends a
    // bootstrapping child, over and above the haul her own containers ask for.
    // Hired per child and capped at `Tuning.FerryLoads`, because what one
    // colony takes of another is written down and bounded and never derived
    // from how much the child could absorb. Priced **from her Storage**, which
    // is what makes it a lend and not a second economy: the stock is the only
    // energy a mother has that her own rows are not already hired against (ADR
    // 0023). A child whose room she cannot reach, or that has no buffer
    // standing, hires nobody (ADR 0004).
    let ferry =
        if List.isEmpty storages then
            0
        else
            ferryBuffers view
            |> Set.toList
            |> List.choose (SpatialInfo.placementOf view.Spatial)
            |> List.filter (fun tile ->
                storages
                |> List.exists (fun stock ->
                    Atlas.haulRoundTripTicks atlas body stock tile |> Option.isSome))
            // Per **child** and not per store: the lend is a sentence about
            // a colony, and a room the Layout ever planned two buffers in
            // would otherwise buy two ferries off one declaration.
            |> List.map (fun tile -> tile.Room)
            |> List.distinct
            |> List.length
            |> (*) view.Tuning.FerryLoads

    // The colony's whole haul, rounded once (ADR 0049), and the ferry's own
    // whole bodies beside it: a lend is counted in bodies rather than in
    // tick-energy, so it is added after the division rather than inside it.
    ceilDiv demand capacity + ferry, rows, capacity

/// The hauler quota alone; `haulerDemandOf` is the same arithmetic with
/// its lines kept.
let private haulerQuota (view: ColonyView) atlas : int =
    let quota, _, _ = haulerDemandOf view atlas
    quota

/// What one body of this shape drinks a tick standing at a controller: its Work
/// parts at the rate above.
let private upgradeDrainOf body =
    body
    |> List.sumBy (function
        | Work -> Engine.upgradeDrainPerWork
        | _ -> 0)

/// The reserver row's body for one outpost (ADR 0042): the deficit sizing and
/// the bank truncation, whichever asks for less, never below one block. The
/// deficit arrives as a second capacity ceiling, because "as many whole blocks
/// as capacity buys" is already `reserverBodyFor`'s rule.
let private reserverBodyWithin claims capacity =
    reserverBodyFor (min capacity (claims * bodyCost reserverPattern.Block))

/// Whether a living body was cast from the guard row: it carries an ATTACK
/// part (ADR 0056). The same part test `findAttack.js` splits the engine's own
/// invaders on, and the one cut no other row of this colony makes — every other
/// row is built out of Work, Carry, Move and CLAIM — so it is asked **first**,
/// beside `Fighter`'s place at the head of the [[body class]] ladder, and it is
/// written here rather than beside `isHaulerBody` because the guard row's own
/// quota below is its first reader. Read off
/// the parts like every other row predicate (ADR 0006), so a fighting body the
/// colony was handed rather than cast fills this row's quota exactly as one it
/// cast does.
let private isGuardBody (creep: CreepInfo) =
    creep.Body |> Map.tryFind Attack |> Option.exists (fun n -> n > 0)

/// How many guards one raided [[outpost]] wants (ADR 0056), which is **0** for
/// the whole of a colony's ordinary life because no room is raided: one guard
/// per declared outpost a [[threat]] stands in this tick, two where the raid
/// out-heals the guards already standing there, capped at two and — for the
/// row's own quota — summed over the outposts. A per-tick fact read off vision
/// and nothing remembered between ticks — vision
/// in a guarded outpost *is* the guard — so it falls to 0 the tick the room is
/// clear; it does not decay in between, and it needs none: a cast is 1,500
/// ticks of body and a raid is 1,500 ticks, so one cast covers one raid by
/// construction and the survivor goes on filling the row's `Living`.
///
/// The second guard's arithmetic is the engine's, over the parts the projection
/// already carries. The raid heals at `Engine.healPower` per HEAL part **over
/// every hostile in that room** — a `smallHealer` is no Threat itself and is
/// exactly what buys the second body — and our standing guards deal
/// `Engine.attackPower` per ATTACK plus `Engine.rangedAttackPower` per
/// RANGED_ATTACK. So one 750-energy guard's 90 stands against an unboosted
/// `smallHealer`'s 60 and the count stays at one; a second healer's 120 casts
/// the second.
///
/// The escalation is asked **only of a room a guard of ours already stands in**
/// — `damage > 0` and not `healing >= damage` alone — because the damage term
/// counts what is standing there and an empty room's is honestly zero, which
/// every raid carrying a single HEAL part out-heals by arithmetic. Read without
/// that conjunct the rule degenerates to "a healer in the room buys two guards"
/// on the tick a raid is first seen: two bodies for the raid ADR 0056 prices at
/// one, and its sentence above is written of a guard that has *arrived*. So the
/// first body is bought against the [[threat]] and the second against a fight
/// the colony can measure, the escalation arriving one oven later — which is the
/// direction decision 4's two-cast bound and the [[stand-down]] behind it exist
/// to hold. It is also what keeps a lone `smallMelee` met by an empty room at
/// one rather than at `0 >= 0`, and a raid that heals **nothing** buys no second
/// body whatever our damage is.
///
/// Vision is the whole of what this reads (ADR 0004): an outpost the colony
/// cannot see this tick carries no hostiles and asks for no guard, which is the
/// same zero a quiet room contributes. The [[home room]] is not in this list —
/// a raid at home casts no guard and is the [[keep]]'s business (ADR 0034), and
/// the spawn hold would refuse the cast anyway.
///
/// **One room's number, and the row's quota is its sum** (ADR 0056 decision 2):
/// the Guard pooled for that room is capped at exactly this, so the row hires
/// what the pool admits and the Matcher counts holders against the number the
/// Planner set, which is ADR 0052 decision 6. Asked only of a room
/// `guardedOutposts` has already answered for — a room with no Threat in it is
/// not one guard but none.
let private guardsWanted (view: ColonyView) atlas (room: string) : int =
    let parts part body =
        body |> List.filter ((=) part) |> List.length

    let hostiles = view.Hostiles |> List.filter (fun h -> h.Pos.Room = room)
    let healing = hostiles |> List.sumBy (fun h -> Engine.healPower * parts Heal h.Body)

    let damage =
        view.Creeps
        |> List.filter (fun creep ->
            isGuardBody creep
            && (Atlas.creepTile atlas creep.Name |> Option.exists (fun tile -> tile.Room = room)))
        |> List.sumBy (fun creep ->
            let count part =
                creep.Body |> Map.tryFind part |> Option.defaultValue 0

            Engine.attackPower * count Attack
            + Engine.rangedAttackPower * count RangedAttack)

    if damage > 0 && healing >= damage then 2 else 1

/// The guard row's quota: `guardsWanted` over every raided outpost, summed.
let private guardQuota (view: ColonyView) atlas : int =
    guardedOutposts view |> List.sumBy (guardsWanted view atlas)

/// The reserver row's quota and its sizing, which are one rule with two faces
/// (ADR 0042, ADR 0006's law that a row arrives with its quota): one reserver
/// per **declared** outpost, each wanting `ceil((5000 - ticks this colony
/// holds) / 600)` CLAIM parts. The list's length is the quota; each entry is
/// what that outpost's body asks for. No state is kept between ticks — the
/// deficit recomputes from the reservation itself. A **candidate colony** takes
/// one more entry, of a single block (ADR 0047), and its room leaves the
/// reservation demands, because a controller carries one Task and a candidate
/// colony's is the Claim; the body is the same `[Claim; Move]` either way,
/// which is why this is one row and not two. Which rooms count is
/// `declaredOutposts`, the derivation this row shares with the guard row. The
/// *rooms* drop out and every cast this tick is sized at the largest demand in
/// the list: the quota counts bodies, and which controller each finished body
/// holds is the Matcher's, priced by travel cost. Over-buying is the safe
/// direction (ADR 0026), and the bank truncates it anyway. **The bank must
/// afford one block**, or the row hires nobody: a colony that cannot buy a
/// reservation does not hold one, and a row hired against a body it can never
/// buy is an addend of the Workforce target no cast will pay off.
let private reserverClaimsOf (view: ColonyView) : int list =
    let heldTicks room =
        view.RoomControl
        |> Map.tryFind room
        |> Option.bind (fun control -> control.Reservation)
        |> Option.filter (fun held -> held.Holder = ReservationHolder.Ours)
        |> Option.map (fun held -> held.TicksToEnd)
        |> Option.defaultValue 0

    // The candidate colonies this tick, each asking for **one** block (ADR
    // 0047): the Claim row is this row, because both bodies are CLAIM bodies
    // and a second pattern row would be the same block under a second name (ADR
    // 0006), so `patternOf` reads a claimer back as a reserver and the casting
    // order, the gap and the amortization all count it as one. One block and
    // never the deficit's nine: a claim is one act by one CLAIM part, finished
    // the tick it succeeds. Their rooms are already out of `declaredOutposts`,
    // which is the same fact read from the other end.
    let claims = claimTargets view

    if view.Bank.Capacity < bodyCost reserverPattern.Block then
        []
    else
        let reserved =
            declaredOutposts view
            |> List.map (fun room ->
                ceilDiv (Engine.reservationCap - heldTicks room) Engine.claimLifetime |> max 1)

        reserved @ (claims |> List.map (fun _ -> 1))

/// The colony's surplus over one creep's lifetime: the income the two upgrade
/// rows are hired out of, written once because both read it and a paraphrase
/// would let them hire against different money (ADR 0012, ADR 0046). Income is
/// counted per source at that source's own output and never at a colony-wide
/// ten (ADR 0042): an unreserved source is worth half a held one, and a posted
/// source whose room the colony cannot see is worth nothing at all rather than
/// half (ADR 0004). An output is what the garrison digs, capped at the rock's
/// rate, and the row is charged its replacement at that same body, so credit
/// and charge are one cast — since ADR 0053, Post by Post. From that income the
/// reserver, anchor and hauler rows' amortization is deducted: those three are
/// hired off facts about the *ground*, so their price is settled before the
/// surplus has a number, while the two rows hired out of the surplus itself are
/// charged inside `workforceTarget`.
let private surplusOverLifetime
    (view: ColonyView)
    atlas
    reserverClaims
    (anchorPostCaps: Map<RoomPos, int>)
    haulerQuota
    =
    let capacity = view.Bank.Capacity

    // The row's own body, once, times the places it hires: every reserver cast
    // this tick carries the largest outstanding demand, so the charge is priced
    // off that same body and never off a per-room one the casting step would not
    // have cast. Scaled from a CLAIM body's own 600-tick life onto the 1,500 the
    // rest of this sum is written in (ADR 0042): a reserver is replaced two and
    // a half times over one worker's life, and charging it once would hire an
    // upgrade mouth the reservation is really paying for.
    let reserverCost =
        if List.isEmpty reserverClaims then
            0
        else
            List.length reserverClaims
            * bodyCost (reserverBodyWithin (List.max reserverClaims) capacity)

    // The anchor row charged **Post by Post**, each at the body the casting
    // step would actually buy for that Post (ADR 0053): a row whose bodies
    // shrank with a lapsed reservation while its amortization went on deducting
    // the six-Work price would hire an upgrade mouth fewer than the income
    // really feeds, and a quota times one ceiling is that same mistake wherever
    // the colony's Posts disagree.
    let amortization =
        (anchorPostCaps
         |> Map.fold (fun total _ cap -> total + bodyCost (anchorBodyFor cap capacity)) 0)
        + haulerQuota * bodyCost (bodyFor haulerPattern capacity)
        + reserverCost * Engine.creepLifetime / Engine.claimLifetime

    // Summed over the posted sources at each one's own output, never a count
    // times a constant (ADR 0042): a source the colony cannot price contributes
    // nothing, the same zero it would contribute by not being posted.
    let income =
        view.Sources
        |> List.filter (isPosted atlas)
        |> List.sumBy (fun s -> sourceOutputOf view atlas s.Id |> Option.defaultValue 0)

    income * Engine.creepLifetime - amortization

/// ADR 0046's ratio itself, over two part counts, written once because two
/// readers ask it of two different shapes: `isStandingBody` of a living creep's
/// part map, and `isStandingCast` of a body this module has just sized.
let private standingRatio (tuning: Tuning) carryParts workParts =
    carryParts * tuning.StandingCarryPerWork < workParts

/// Whether a body this module has sized is a standing body: the same ratio over
/// a part list rather than over a living creep's part map.
let private isStandingCast (tuning: Tuning) body =
    let count part =
        body |> List.filter ((=) part) |> List.length

    standingRatio tuning (count Carry) (count Work)


/// Whether a living body is a **standing body** (ADR 0046): it carries fewer
/// than one Carry part per four Work — `Carry * 4 < Work`. Part arithmetic and
/// nothing else, like every other row-reading predicate here (ADR 0006), and a
/// fact about a *body* rather than about a row: the upgrader row's `11W/1C/11M`
/// is one, and so is the anchor row's `6W/1C/1M`. The gate that reads it is
/// `applicable` below, on Build, Repair and Refill — on Pickup and on every
/// Withdraw but the buffer's, and since #235 on Harvest, where the body that
/// keeps it is the Work-heavy one (ADR 0016) and not the standing one, the
/// anchor row's `6W/1C/1M` being both. What is left exactly as its own gates
/// already had it is the working life the upgrader row was shaped for: it draws
/// from the buffer at its feet (ADR 0019, through ADR 0016's gate) and spends
/// into the controller in place. Digging is not part of it — #206 left Harvest
/// open on the reasoning that travel cost would keep the row beside its buffer,
/// and an empty buffer leaves the row nothing else applicable at all.
let private isStandingBody (tuning: Tuning) (creep: CreepInfo) =
    let count part =
        creep.Body |> Map.tryFind part |> Option.defaultValue 0

    standingRatio tuning (count Carry) (count Work)

/// What one body of the upgrader row eats per tick: every Work part of the
/// row's cast at the richest bank, at the controller's own per-Work rate
/// (ADR 0046). Never below one — the row's sizing rule floors at a pair —
/// so the quota below always has a divisor.
let private upgraderDrain capacity =
    upgradeDrainOf (bodyFor upgraderPattern capacity)

/// The upgrader row's quota (ADR 0046): the surplus divided by what one
/// standing body **costs the colony over a life** — the energy its Work drinks
/// plus the body itself — rounded **down**, with the remainder handed on to the
/// worker row, whose own division rounds up (ADR 0037). **The divisor carries
/// the row's own replacement cost.** Read as the drain alone, a surplus of
/// 33,100 over a 16,500 drain hires two bodies that cost 36,400 to run and
/// replace: the worker row's income term goes to zero and the colony has
/// promised more over a lifetime than its rocks bring in. What a row pays for
/// is the mouth *and* the body. Only one of the two rows may round up: ADR 0037
/// admits an oversell bounded by *one body's* lifetime drain, and two rows
/// rounding up against the same number sell that bound twice — the rounding
/// goes to the row whose oversold body is smaller, which is the worker row.
/// **Non-zero only while a built controller container stands in the room**: the
/// buffer is this row's working ground (ADR 0046 against ADR 0012's
/// generalization), and a site there is a promise, not a store to withdraw
/// from. A negative surplus hires none.
let private upgraderQuota (view: ColonyView) atlas surplus =
    let capacity = view.Bank.Capacity

    if
        Set.isEmpty (Atlas.controllerContainers atlas)
        || not (isStandingCast view.Tuning (bodyFor upgraderPattern capacity))
    then
        0
    else
        surplus
        / (upgraderDrain capacity * Engine.creepLifetime
           + bodyCost (bodyFor upgraderPattern capacity))
        |> max 0

/// The worker row's floor (ADR 0046): the row's income term is whatever the
/// upgrader row has not eaten, and beside a buffer that can still be nothing at
/// all — the remainder is bounded by one standing body's lifetime drink, and
/// that row's own replacement is charged against it first. A colony with no
/// generalist builds nothing and repairs nothing: a standing body is shut out
/// of all three deliveries and the hauler row carries no Work. Two while
/// anything stands in the Build or Repair pool, one otherwise. Two, because
/// since ADR 0042 a builder crosses a Seam and the home room's own sites are
/// unattended for the fifty ticks of that walk; one, because hiring the second
/// against no pool would be hiring for a job that does not exist.
let private workerFloor (tasks: Task list) =
    let building =
        tasks
        |> List.exists (function
            | Build _
            | Repair _ -> true
            | _ -> false)

    if building then 2 else 1

/// Workforce target (ADR 0012, ADR 0046, ADR 0056): six addends, each a pattern
/// row's own colony fact — reservers one per declared outpost, guards one or two
/// per raided one, Anchors one per Post, haulers the throughput quota, upgraders
/// the surplus divided by a standing body's drain, workers the income arithmetic
/// that is left and the pioneers a nursery adds to it (ADR 0047) — floored at
/// `Tuning.MinWorkforce` and derived
/// fresh each tick. A source whose Post is provided for retires its other
/// Seats: one heavy body drains it alone. An unposted source of the home room
/// still contributes its Seat count, its output being spoken for by the seat
/// crews that walk it, so only the posted sources' output is income. An
/// unposted source of an **outpost** contributes nothing at all (ADR 0042): the
/// seat-crew justification presumes the walk is cheap, and across a border it
/// is not. A standing container is the switch admitting an outpost into the
/// economy: until one stands the room is invisible to every quota but the
/// reserver's, and the tick it stands the source enters the two that read a
/// store, a hauler term at its own round trip and a share of the income base at
/// its own output. The Anchor place moved one step earlier with the container's
/// *site*. The reserver row is the quota this switch does *not* gate — it is
/// what makes the container possible — arriving as `reserverClaims`, whose
/// length is the addend and whose largest entry prices the amortization. The
/// income and the three ground-hired rows' amortization arrive together as
/// `surplus`, read here and by `upgraderQuota` alike. The guard row is an addend
/// like the rest (ADR 0056) and is charged nowhere else: it is 0 for the whole
/// of an ordinary life, and a guard left out of the target would have the
/// deficit read the body it is alive as one of the generalists the income
/// already paid for — a raid would quietly retire a worker for as long as the
/// guard stood.
let private workforceTarget
    (view: ColonyView)
    atlas
    (tasks: Task list)
    reserverClaims
    guardQuota
    anchorQuota
    haulerQuota
    upgraderQuota
    surplus
    =
    let home = SpatialInfo.homeName view.Spatial

    let unpostedSeats =
        view.Sources
        |> List.filter (isPosted atlas >> not)
        |> List.filter (fun s -> Atlas.targetRoom atlas s.Id = Some home)
        |> List.sumBy (fun s -> Atlas.seats atlas s.Id |> Option.defaultValue 0)

    let capacity = view.Bank.Capacity

    let workerDrain = upgradeDrainOf (bodyFor workerPattern capacity)

    // What the standing row takes out of the surplus before the commuting one
    // is hired against the rest (ADR 0046): the energy its Work drinks over a
    // lifetime, and the row's replacement cost over the same lifetime, priced
    // at the body the casting step would actually cast.
    let upgraderCost =
        upgraderQuota * upgraderDrain capacity * Engine.creepLifetime
        + upgraderQuota * bodyCost (bodyFor upgraderPattern capacity)

    // Rounded up through the same ceilDiv as the hauler row (ADR 0037): the
    // granularity a floor would drop is a whole worker body's Work, which grows
    // with RCL, and the income it drops leaks every tick while the body it
    // oversells is paid for out of stock.
    let incomeWorkers =
        ceilDiv (surplus - upgraderCost) (workerDrain * Engine.creepLifetime) |> max 0

    // The pioneers (ADR 0047 decision 4): while a room this colony has claimed
    // still has no spawn in it, the mother hires `Tuning.PioneerCount` more
    // generalists to go and raise one. Hired off a fact about the *world* and
    // not out of the surplus — a nursery is a room a human declared and the
    // colony has taken, exactly as the reserver row is hired off a declared
    // outpost — so it is added to the row rather than divided out of what the
    // upgrader row left. On top of the whole row and outside its floor: the
    // floor is the smallest crowd that can take a delivery at all (ADR 0046),
    // and these bodies are hired for a delivery that exists whatever else the
    // colony is doing. No term of `surplus` answers for them, which is the
    // worker row's pre-existing shape. The addend outlives the nursery and runs
    // on through the bootstrap window, flat over both [[stage]]s for the reason
    // it is flat over two nurseries.
    let pioneers =
        let raising room =
            isNurseryRoom view room || isBootstrapRoom view room

        if view.Stages |> Map.exists (fun room _ -> raising room) then
            view.Tuning.PioneerCount
        else
            0

    // The generalist row's whole share of the target, and the floor sits here
    // rather than on the income term beside it (ADR 0046): both addends hire
    // the same body from the same row, so a colony already running three seat
    // crews has three bodies that can build, and a floor read off the income
    // term alone would hire a fourth against a job that does not exist. What
    // the floor is for is the colony where this sum is *zero*.
    let workerRow =
        (unpostedSeats + incomeWorkers |> max (workerFloor tasks)) + pioneers

    List.length reserverClaims
    + guardQuota
    + anchorQuota
    + haulerQuota
    + upgraderQuota
    + workerRow
    |> max view.Tuning.MinWorkforce

/// Whether a living body was cast from the hauler row: Carry parts but no Work.
/// The worker and anchor rows both keep at least one Work, and only the hauler
/// row casts none (ADR 0012) — so, like the anchor's Work > Move, the casting
/// pattern is readable off the body itself (ADR 0006).
let private isHaulerBody (creep: CreepInfo) =
    let count part =
        creep.Body |> Map.tryFind part |> Option.defaultValue 0

    count Work = 0 && count Carry > 0

/// Whether a living body was cast from the reserver row: it carries a CLAIM
/// part. The one part no other row buys (ADR 0042), so it identifies the row on
/// its own and is asked before the other two — the comparative tests below would
/// read a `[Claim; Carry; Move]` body as a hauler.
let private isReserverBody (creep: CreepInfo) =
    creep.Body |> Map.tryFind BodyPart.Claim |> Option.exists (fun n -> n > 0)

/// Whether a living body can take energy out of a store and put it into an
/// extension — the one capability the bank's own refilling depends on, and so
/// the one every capacity-sized row depends on (the supply floor, ADR 0050).
/// Not "has a Carry part". It is the body half of `Refill`'s gate and the body
/// half of `Withdraw`'s read back together, because a body that can deliver but
/// never draw cannot reach the storage the energy is standing in: a Carry part,
/// no standing-body ratio (ADR 0046) and no more Work than Move (ADR 0016).
/// `Refill`'s third conjunct, `Energy > 0`, is deliberately *not* read: that is
/// a state a hauler passes through twice a trip.
let private canRefill (tuning: Tuning) atlas (creep: CreepInfo) =
    (creep.Body |> Map.tryFind Carry |> Option.exists (fun n -> n > 0))
    && not (isStandingBody tuning creep)
    && not (Atlas.workHeavy atlas creep.Name)

/// The pattern row a living body was cast from, read off the parts alone (ADR
/// 0006): an ATTACK part is the guard row, a CLAIM part is the reserver row,
/// more Work than Move is the anchor row, a standing body at or under that line
/// is the upgrader row, no Work beside a Carry is the hauler row, and every
/// other body is the generalist. The row is what sizes the replacement a lead
/// prices (ADR 0026), so one rule serves every row. Order matters between the
/// anchor and upgrader arms and nowhere else: `6W/1C/1M` satisfies both
/// descriptions, and it is the anchor row that casts it — a body pinned to a
/// Post by ADR 0020's Work Area is a stronger claim than standing beside the
/// buffer. The reserver arm is what keeps ADR 0026 honest for a CLAIM body:
/// `[Claim; Move]` has neither Work nor Carry, so before it existed a
/// reserver's lead was priced off a worker unit. The guard arm is the same debt
/// paid for a fighting body (ADR 0056): `[T; A×3; M×5; H]` has neither, so
/// without it a guard read back as a **worker**, and the raid that cast it
/// would go on filling the generalist row's `Living` for 1,500 ticks.
let private patternOf (tuning: Tuning) atlas (creep: CreepInfo) =
    if isGuardBody creep then guardPattern
    elif isReserverBody creep then reserverPattern
    elif Atlas.workHeavy atlas creep.Name then anchorPattern
    elif isStandingBody tuning creep then upgraderPattern
    elif isHaulerBody creep then haulerPattern
    else workerPattern

/// The row a body **still in the oven** was bought for, read off the parts
/// exactly as `patternOf` reads them off a living creep: the same six arms in
/// the same order, with `Work > Move` written out because the Atlas's own
/// `workHeavy` set is keyed by creep name and a body being cast has none.
let private patternOfCast (tuning: Tuning) (body: BodyPart list) =
    let count part =
        body |> List.filter ((=) part) |> List.length

    if count Attack > 0 then
        guardPattern
    elif count BodyPart.Claim > 0 then
        reserverPattern
    elif count Work > count Move then
        anchorPattern
    elif standingRatio tuning (count Carry) (count Work) then
        upgraderPattern
    elif count Work = 0 && count Carry > 0 then
        haulerPattern
    else
        workerPattern

/// Whether a body in the oven will be able to put energy into an extension once
/// it stands — `canRefill`'s three clauses over a body rather than over a
/// living creep, for the supply floor's one question (ADR 0050): is there
/// anything, alive or bought, that can break the deadlock?
let private castCanRefill (tuning: Tuning) (body: BodyPart list) =
    let count part =
        body |> List.filter ((=) part) |> List.length

    count Carry > 0
    && not (standingRatio tuning (count Carry) (count Work))
    && count Work <= count Move

/// The two facts the two rows whose sizing is not the bank's answer alone read,
/// derived once for the tick (ADR 0042): the anchor row's Work ceilings and the
/// reserver row's outstanding CLAIM demands. Together with the bank they say
/// what **this colony's rows will cast this tick** (ADR 0052 decision 4), which
/// is the number three readers have to agree on: the casting cascade that buys
/// the body, the amortization that charges for it, and the lead that prices its
/// succession. A record and not two arguments, and derived in
/// `decideUnarbitrated` rather than per reader, because both folds walk the
/// projection and a lead is priced once per living creep in two different steps
/// of the tick. Neither field may be derived from a creep's remaining life (ADR
/// 0053): a [[lead]] is priced off this record, so which Posts stand *empty* —
/// an arrival-time judgement (ADR 0026) — cannot be a field of it without
/// closing a circle.
type RowSizing =
    {
        /// `postWorkCapsOf`'s answer this tick — one ceiling per [[post]],
        /// keyed by the Post's own tile.
        AnchorPostCaps: Map<RoomPos, int>
        /// `reserverClaimsOf`'s answer this tick — one entry per room the
        /// row hires for, each that room's CLAIM demand.
        ReserverClaims: int list
    }

let private rowSizingOf (view: ColonyView) atlas : RowSizing =
    {
        AnchorPostCaps = postWorkCapsOf view atlas
        ReserverClaims = reserverClaimsOf view
    }

/// The largest ceiling the row's Posts ask for, and the held one where it has
/// no Post at all: the anchor row's answer wherever a reader wants a body but
/// names no Post (ADR 0053).
let private richestAnchorCap (caps: Map<RoomPos, int>) =
    caps
    |> Map.fold (fun richest _ cap -> max richest cap) 0
    |> function
        | 0 -> heldWorkCap
        | cap -> cap

/// The ceiling of the Post at a tile, and the richest one for a tile that
/// is no Post (ADR 0053): the two readings a body sized for a *place*
/// needs — the Post a garrison stands on, and the nothing-in-particular a
/// body still walking to one stands on.
let private anchorCapAt (caps: Map<RoomPos, int>) (tile: RoomPos) =
    match Map.tryFind tile caps with
    | Some cap -> cap
    | None -> richestAnchorCap caps

/// **The body a row casts to put a creep on one tile**, at this colony's bank
/// and under this tick's second fact where the row has one (ADR 0052 decision
/// 4): the anchor row under the ceiling of the [[post]] that tile is (ADR
/// 0053), the reserver row at its largest outstanding demand, and every other
/// row at `bodyFor`'s answer, which for them *is* the whole rule. A tile and
/// not a row alone, because since ADR 0053 the anchor row casts no single body:
/// its ceiling is the rock the Post seats, and the one caller here is the
/// [[lead]], which asks what will stand *where this creep stands*. So an
/// incumbent on the home room's Post is led by a six-Work successor while one
/// on an outpost's lapsed Post is led by a three-Work one, off one rule. A tile
/// that is no Post takes the richest ceiling the row has. The entry point every
/// reader that means "what will this colony buy" asks, where `bodyFor` answers
/// the narrower question a caller holding only a capacity can ask.
let private castBodyOf
    (view: ColonyView)
    (sizing: RowSizing)
    (pattern: BodyPattern)
    (tile: RoomPos)
    =
    let capacity = view.Bank.Capacity

    if pattern.Name = anchorPattern.Name then
        anchorBodyFor (anchorCapAt sizing.AnchorPostCaps tile) capacity
    elif pattern.Name = reserverPattern.Name then
        match sizing.ReserverClaims with
        | [] -> bodyFor reserverPattern capacity
        | claims -> reserverBodyWithin (List.max claims) capacity
    else
        bodyFor pattern capacity

/// A creep's lead (ADR 0026): the ticks its replacement needs to stand where it
/// stands — the successor body's cast time plus that body's walk out of the
/// spawn, priced for the successor's own fatigue factor and not the
/// incumbent's. The body is **the one this colony's row would cast to stand on
/// the incumbent's own tile** (`castBodyOf`, ADR 0053) and not the largest
/// that row could cast, so an Anchor on a lapsed outpost's [[post]]
/// earns a shorter lead than the Anchor beside it at home. The walk starts
/// beside the spawner rather than on it, where the engine actually places the
/// finished creep. Several spawns resolve at the cheapest. Geometry that prices
/// nothing leads nobody (ADR 0004), and a lead of 0 leaves every living creep
/// counted.
let private leadOf (view: ColonyView) atlas (sizing: RowSizing) (creep: CreepInfo) : int =
    let pattern = patternOf view.Tuning atlas creep

    match Atlas.creepTile atlas creep.Name with
    | None -> 0
    | Some tile ->
        // The colony's one bank, whatever room the spawn is filed under: every
        // spawn a colony casts from stands in its home room (ADR 0052 decision
        // 1), so the capacity a replacement would be cast at is the same for
        // all of them — and so is the tile it is cast to stand on (ADR 0053).
        let body = castBodyOf view sizing pattern tile

        view.Spawns
        |> List.choose (fun s ->
            match Atlas.positionOf atlas s.Id with
            | None -> None
            | Some spawnPos ->
                Atlas.castWalkTicks atlas body (RoomPos.pos spawnPos) tile
                |> Option.map (fun walk -> Engine.spawnTicksPerPart * List.length body + walk))
        |> function
            | [] -> 0
            | leads -> List.min leads

/// Whether a creep is expiring (ADR 0026): its remaining life is at or under
/// its lead, so it will be dead before a replacement cast now could stand where
/// it stands. It leaves the workforce's living count and its row's gap, which
/// is what casts the successor while it still works.
let private expiring (view: ColonyView) atlas (sizing: RowSizing) (creep: CreepInfo) =
    creep.TicksToLive <= leadOf view atlas sizing creep

/// The spawn Intents the Workforce target's rows are owed. The target is the
/// quota the *generalist* row is hired against; every other row is hired against
/// its own unfilled quota and can carry the fleet past the target. Spawning is a
/// colony-level need, not a Task creeps get matched to, so it sits beside the
/// Planner/Matcher pipeline rather than inside it. It reads the tick's Task pool
/// for one number: the worker row's floor is "two while anything stands in the
/// Build or Repair pool" (ADR 0046), and the pool is the only honest reading of
/// that. The step is derived from the pool and never feeds it.
let private planSpawns
    (view: ColonyView)
    atlas
    (sizing: RowSizing)
    (threats: Threats)
    (tasks: Task list)
    (haulerQuota: int)
    : Intent list * Quotas =
    // The spawn holds while its doorstep is hot (ADR 0033): a creep born into a
    // Reach is a kill delivered, so no spawn casts anything while any tile
    // beside any spawn lies in one — the disaster fallback included, an empty
    // colony's first creep least affording to be born under fire.
    let doorstepInReach (s: SpawnInfo) =
        match Atlas.targetRoom atlas s.Id, Atlas.positionOf atlas s.Id with
        | Some room, Some pos ->
            let reach = Threats.reachIn threats room

            [
                for x in pos.X - 1 .. pos.X + 1 do
                    for y in pos.Y - 1 .. pos.Y + 1 -> { X = x; Y = y }
            ]
            |> List.exists (fun tile -> Set.contains tile reach)
        | _ -> false

    // Asked before anything is priced, the way the reflexes ask their
    // hostiles first: a held tick derives no Workforce target and floods
    // no lead.
    if view.Spawns |> List.exists doorstepInReach then
        [], Quotas.silent
    else

        // The specialist rows' quota rules (ADR 0006, ADR 0012): one Anchor per
        // Post, haulers per the throughput arithmetic — the hauler quota
        // arriving memoised on the census signature (ADR 0017), which signs the
        // *union* of what the Layout and the quota read, neither input set
        // containing the other. Both quotas are addends of the target itself.
        // One Anchor per Post of *every* projected room (ADR 0042): an
        // outpost's Post is the same garrison tile a home Post is, so it hires
        // from the same row and travel cost pins each Anchor on the Post
        // nearest it.
        let anchorQuota = Atlas.postCount atlas

        // The reserver row's quota and its body in one value (ADR 0042): one
        // entry per declared outpost, each entry that outpost's CLAIM demand,
        // and the largest of them is what every cast this tick carries.
        let reserverClaims = sizing.ReserverClaims

        // The guard row's quota (ADR 0056): zero on every ordinary tick, and on
        // the ticks a raid stands in a declared outpost, one or two per raided
        // room. Read here beside the others because it is an addend of the
        // target below and a gap of its own in the cascade.
        let guardQuota = guardQuota view atlas

        // The anchor row's ceilings this tick, one per Post, read once beside
        // the quotas for the reason the reserver's demand list is (ADR 0042,
        // ADR 0053): the row's bodies are what the amortization is charged and
        // what the casts below buy, and each Post's two readings must be one
        // body.
        let anchorPostCaps = sizing.AnchorPostCaps

        // The income the two upgrade rows are hired out of, once (ADR
        // 0046): the standing row's quota is derived from it and the
        // commuting row's is derived from what that quota leaves, so the
        // two must read one number and not two spellings of it.
        let surplus =
            surplusOverLifetime view atlas reserverClaims anchorPostCaps haulerQuota

        // The upgrader row's quota (ADR 0046), read here beside the other
        // rows' for the same reason: it is an addend of the target below
        // and a gap of its own in the cascade, and a body hired for one
        // and not counted in the other would be an oversell every tick.
        let upgraderQuota = upgraderQuota view atlas surplus

        let target =
            workforceTarget
                view
                atlas
                tasks
                reserverClaims
                guardQuota
                anchorQuota
                haulerQuota
                upgraderQuota
                surplus

        // The deficit and every row gap count the creeps that will still be
        // alive when a replacement could arrive: an expiring creep is already
        // outside the count (ADR 0026), so its successor is cast while it still
        // works.
        let living =
            view.Creeps |> List.filter (fun creep -> not (expiring view atlas sizing creep))

        // The bodies already bought and not yet standing (#156). A creep in an
        // oven is in no `Creeps` list — it cannot act, cannot be matched and
        // holds no tile — so every row's living count read straight past it,
        // and a colony with **two** idle spawns bought the same seat twice:
        // spawn one casts an Anchor for the empty Post at tick T, and at T+1
        // the gap is still one and spawn two casts a second for the same Post.
        // ADR 0026 rejected counting a gestating body on a reason true of one
        // spawn and of no other number of them.
        let casting = view.Casting

        let castOf pattern =
            casting
            |> List.filter (fun body -> patternOfCast view.Tuning body = pattern)
            |> List.length

        let deficit = target - (List.length living + List.length casting)

        // A body is sized to the bank's capacity and cast the tick the bank
        // holds its cost (ADR 0021) — a full bank for rows priced at capacity,
        // sooner for the capped Anchor row. Disaster fallback: an empty colony
        // can never refill extensions, so a capacity-sized body would wait
        // forever — spawn a minimal worker unit from whatever is banked right
        // now, time-to-first-creep outranking specialisation (ADR 0006). The
        // row's sizing rule arrives as a function of the bank rather than being
        // looked up from the pattern, which is the choice ADR 0042's reserver
        // row forces: two rows are the bank's answer alone, but the reserver's
        // body is `min(reservation deficit, bank)` and the anchor's is capped
        // by the ceiling of the **Post** the cast is filling (ADR 0053) — a
        // fact about the room being reserved and a fact about one rock, neither
        // of them about the row.
        let castFromBank pattern (sizing: RoomEnergy -> BodyPart list) (bank: RoomEnergy) =
            if List.isEmpty view.Creeps then
                if bank.Available >= bodyCost workerPattern.Block then
                    Some(workerPattern, workerPattern.Block)
                else
                    None
            else
                let body = sizing bank

                if bank.Available >= bodyCost body then
                    Some(pattern, body)
                else
                    None

        // Reserver gaps are filled before Anchor gaps, Anchor gaps before
        // hauler gaps, hauler gaps before upgrader gaps and those before
        // generalist gaps (ADR 0046) — and the worker row's quota is whatever
        // the target has left. The reserver goes in front of all four (ADR
        // 0042): the other rows spend income, and this one decides whether the
        // income is five a tick or ten across every source of an outpost at
        // once. The guard goes in front of *it* (ADR 0056), behind the supply
        // floor alone: it is zero for the whole of a colony's ordinary life,
        // and on the ticks it is not, every tick of delay is an [[anchor]] and
        // a [[hauler unit]] dying in a room a reserver would walk into next.
        // Being first it is asked first, and it does not *hold* the cascade the
        // tick the bank cannot pay for it: a row the bank cannot afford yields
        // the tick to the rows below it (ADR 0050), which at a 300 bank is
        // exactly what the guard row does. Each specialist gap is that row's
        // own unfilled quota, answered on its own terms rather than out of the
        // deficit: an empty Post is a fact about the ground, and the row that
        // hires for it does not stop hiring because the headcount overshot some
        // other row's arithmetic.
        //
        // The guard row's own `Living` is where ADR 0056's "no decay" lands: a
        // guard that outlived its raid is still an ATTACK body in the fleet, so
        // the next raid inside its life reads a filled row and waits no thirty
        // ticks of oven for a body it already owns. Counted over the **fleet**
        // against a quota derived per room, which is the reserver row's own
        // shape one line down (ADR 0042): the row counts bodies and which room
        // each finished body works is the Matcher's, priced by travel cost.
        let guardLiving = living |> List.filter isGuardBody |> List.length

        let guardGap = guardQuota - guardLiving - castOf guardPattern |> max 0

        let reserverLiving = living |> List.filter isReserverBody |> List.length

        let reserverGap =
            List.length reserverClaims - reserverLiving - castOf reserverPattern |> max 0

        let anchorGarrison =
            living |> List.filter (fun creep -> Atlas.workHeavy atlas creep.Name)

        let anchorLiving = List.length anchorGarrison

        let anchorGap = anchorQuota - anchorLiving - castOf anchorPattern |> max 0

        // **The vacancies this row is casting into, richest ceiling first**
        // (ADR 0053): every Post with nobody standing on it who will still be
        // there when a replacement could arrive. This is what pairs a body to a
        // rock in an architecture where no caster knows which Post a finished
        // Anchor will man (ADR 0021, ADR 0006) — the row is sizing for the hole
        // it is filling, not for a posting. Judged at **arrival** and never by
        // who is standing there now (ADR 0026): an expiring incumbent is
        // already out of `living`, so the Post it is still standing on reads
        // empty and its own successor is sized off its own rock. Read off who
        // is standing instead, an ordinary home succession would find its Post
        // occupied, fall through to whatever outpost Post happened to be free,
        // and buy the home room's replacement off a neutral rock.
        let emptyPostCaps =
            let manned =
                anchorGarrison
                |> List.choose (fun creep -> Atlas.creepTile atlas creep.Name)
                |> Set.ofList

            anchorPostCaps
            |> Map.toList
            |> List.filter (fun (tile, _) -> not (Set.contains tile manned))
            |> List.map snd
            |> List.sortDescending

        // The ceiling every Anchor cast this tick is sized under: the **dearest
        // vacancy's** (ADR 0053), with the richest Post the row hires for
        // standing in to keep the expression total. One ceiling for the tick's
        // casts and not one per vacancy at its own rock, because the caster
        // cannot steer the finished body: the Matcher pairs it to a Post by
        // travel cost and knows nothing of which vacancy it was bought for (ADR
        // 0021's rejection). A tick that bought `6W` for a held vacancy and
        // `3W/1C/1M` for a neutral one beside it has bought a body that can
        // land on the held rock and dig six where the rock gives ten — the
        // "cheapest vacancy first" ADR 0053 rejects by name.
        let anchorCap =
            emptyPostCaps
            |> List.tryHead
            |> Option.defaultValue (richestAnchorCap anchorPostCaps)

        let haulerLiving = living |> List.filter isHaulerBody |> List.length

        let haulerGap = haulerQuota - haulerLiving - castOf haulerPattern |> max 0

        // Bodies and not names (ADR 0006): the row's living count is what
        // `patternOf` reads back off the parts, so a `11W/1C/11M` the colony
        // inherited or was handed fills this quota exactly as one it cast does.
        // Asking `patternOf` rather than `isStandingBody` alone is what keeps
        // the Anchor row out of it: `6W/1C/1M` answers to both descriptions and
        // it is the anchor arm that claims it, so an Anchor at its Post never
        // pays off an upgrader's gap.
        let upgraderLiving =
            living
            |> List.filter (fun creep -> patternOf view.Tuning atlas creep = upgraderPattern)
            |> List.length

        let upgraderGap = upgraderQuota - upgraderLiving - castOf upgraderPattern |> max 0

        // The tick's arithmetic, written down for the `quotas` view (ADR
        // 0009: a record returned, never logged). The worker row is what
        // the target leaves after the five specialist rows, which is how
        // the cascade below hires it.
        let quotas: Quotas =
            let row name quota living casting =
                {
                    Row = name
                    Quota = quota
                    Living = living
                    Casting = casting
                }

            let specialists =
                List.length reserverClaims
                + guardQuota
                + anchorQuota
                + haulerQuota
                + upgraderQuota

            {
                Target = target
                Living = List.length living
                Casting = List.length casting
                HaulerLoad = 0
                HaulerDemand = []
                Rows =
                    [
                        row "guard" guardQuota guardLiving (castOf guardPattern)
                        row
                            "reserver"
                            (List.length reserverClaims)
                            reserverLiving
                            (castOf reserverPattern)
                        row "anchor" anchorQuota anchorLiving (castOf anchorPattern)
                        row "hauler" haulerQuota haulerLiving (castOf haulerPattern)
                        row "upgrader" upgraderQuota upgraderLiving (castOf upgraderPattern)
                        row
                            "worker"
                            (target - specialists |> max 0)
                            (List.length living
                             - guardLiving
                             - reserverLiving
                             - anchorLiving
                             - haulerLiving
                             - upgraderLiving)
                            (castOf workerPattern)
                    ]
            }

        // The supply floor (ADR 0050), and the one row that is not a quota: a
        // colony holding no body that can put energy into an extension hires
        // one hauler in front of every row, sized from what is banked **right
        // now**. It is the disaster fallback's own argument (ADR 0006) carried
        // to the state that fallback cannot see. Every row below it prices its
        // body at `bank.Capacity`, so none is buyable until the extensions are
        // full — and the extensions are filled by creeps.
        let supplyFloor =
            if
                view.Creeps |> List.exists (canRefill view.Tuning atlas)
                // Or one already bought (#156): the floor asks whether the bank
                // can ever be filled again, and a hauler nine ticks from
                // standing answers yes — buying a second one out of the same
                // stranded bank is the oversell this row exists to make exactly
                // once.
                || casting |> List.exists (castCanRefill view.Tuning)
            then
                0
            else
                1

        // The rows expanded into the seats they are owed, in casting order: the
        // supply floor, then guard, reserver, Anchor, hauler, upgrader (ADR
        // 0042, ADR 0046, ADR 0056) and last the generalist, whose seats are
        // whatever the whole-fleet deficit has left once every row above is
        // counted. The deficit gates the *worker* row alone and stands in for
        // that row's own gap: ADR 0012 hires it against whatever the target has
        // left once the specialist rows are counted, and the whole-fleet gap
        // less the rows above is exactly that remainder while every specialist
        // row is at or under quota.
        let seats =
            List.replicate
                supplyFloor
                // The one row sized from `Available` (with the disaster
                // fallback inside `castFromBank`, for the same reason).
                (castFromBank haulerPattern (fun bank -> bodyFor haulerPattern bank.Available))
            @ List.replicate
                guardGap
                // Priced at capacity like every row but the floor (ADR 0021),
                // so a bank that cannot hold 750 casts nothing here and yields
                // to the reserver behind it (ADR 0050): a colony that small has
                // ADR 0043's [[stand-down]] and nothing else.
                (castFromBank guardPattern (fun bank -> bodyFor guardPattern bank.Capacity))
            @ List.replicate
                reserverGap
                // Every cast at the largest outstanding demand and never at the
                // one standing beside it in the list: the Matcher pairs a
                // finished body to a controller by travel cost, so a body sized
                // for the room that has slipped furthest can land on the room
                // that has not. A positive gap is a non-empty demand list, so
                // the `List.max` is total inside this sizing — and it is inside
                // it, because `List.replicate 0` still evaluates the element.
                (castFromBank reserverPattern (fun bank ->
                    reserverBodyWithin (List.max reserverClaims) bank.Capacity))
            @ List.replicate
                anchorGap
                // Sized under the dearest **vacancy**'s ceiling and never a
                // colony-wide constant (ADR 0053): which Post the finished
                // body lands on is the Matcher's, so the cast carries the
                // saturation of the richest rock this row has a hole on.
                (castFromBank anchorPattern (fun bank -> anchorBodyFor anchorCap bank.Capacity))
            @ List.replicate
                haulerGap
                (castFromBank haulerPattern (fun bank -> bodyFor haulerPattern bank.Capacity))
            @ List.replicate
                upgraderGap
                // Ahead of the generalist and behind the three rows hired off
                // the ground (ADR 0046): the upgrader spends the surplus those
                // three produce, so it is cast once they stand, and it spends
                // it at eleven Work against the generalist's nine.
                (castFromBank upgraderPattern (fun bank -> bodyFor upgraderPattern bank.Capacity))
            @ List.replicate
                (deficit
                 - (supplyFloor + guardGap + reserverGap + anchorGap + haulerGap + upgraderGap)
                 |> max 0)
                (castFromBank workerPattern (fun bank -> bodyFor workerPattern bank.Capacity))

        // Idle spawns draw from the colony's one bank in list order — each body
        // debits the budget the next spawn sees, so the same energy is never
        // committed twice. One bank and never a map keyed by the spawn's room
        // (ADR 0052 decision 1): every spawn a colony casts from stands in its
        // home room.
        let intents, _, _ =
            view.Spawns
            |> List.filter (fun s -> not s.IsSpawning)
            |> List.fold
                (fun
                    (intents,
                     bank: RoomEnergy,
                     unfilled: (RoomEnergy -> (BodyPattern * BodyPart list) option) list)
                    s ->
                    // The first seat this bank can pay for, and the rest of
                    // the list with exactly that seat taken out of it.
                    let rec take passed remaining =
                        match remaining with
                        | [] -> None
                        | cast :: rest ->
                            match cast bank with
                            | Some filled -> Some(filled, List.rev passed @ rest)
                            | None -> take (cast :: passed) rest

                    match take [] unfilled with
                    | Some((pattern, body), left) ->
                        SpawnCreep(s.Name, body, $"{pattern.Name}-{view.Time}-{s.Name}") :: intents,
                        { bank with
                            Available = bank.Available - bodyCost body
                        },
                        left
                    | None -> intents, bank, unfilled)
                ([], view.Bank, seats)

        List.rev intents, quotas

/// The hostiles standing in the colony's own room, which is the whole of what
/// the two reflexes below may read. Since `ColonyView.Hostiles` stopped being
/// the spawn rooms' alone, "a hostile" and "a hostile here" are two different
/// questions, and both reflexes ask the second: safe mode protects a controller
/// of ours and an outpost has none (ADR 0007), and a tower's shot is a range act
/// inside its own room (ADR 0014). Everything above them — Reach, Flee, the
/// spawn hold — reads the list whole and files each hostile under its own room
/// (ADR 0033). The home name and not the controller's or a tower's room, because
/// both arms need an answer on a tick the projection places neither: ADR 0004's
/// absence would otherwise widen the reflex back to every room.
let private hostilesAtHome (view: ColonyView) : HostileInfo list =
    let home = SpatialInfo.homeName view.Spatial
    view.Hostiles |> List.filter (fun hostile -> hostile.Pos.Room = home)

/// Colony reflex beside the pipeline, two arms and one pair of gates — stock
/// remaining, safe mode not already running. The CLAIM arm: a CLAIM-part
/// hostile is the one threat that can disarm safe mode itself,
/// `attackController` blocking activation for 1,000 ticks. But the tap is a
/// range-1 act, so the activation holds until a claimer stands within reach of
/// landing it (ADR 0015) — free, and it buys the towers their window. An
/// unplaceable controller falls back to firing on sight. The Keep arm (ADR
/// 0034): any Keep structure below full hits while any hostile stands in the
/// home room — the same shape, hold until the harm is certain, over the other
/// half of the exposure. Any hostile and not only a Threat, a WORK-only
/// dismantler hurting a structure without ever qualifying as one. Stateless on
/// purpose: one tick's hits, never a comparison against the last tick's.
let private planSafeMode (view: ColonyView) atlas : Intent list =
    match view.Controller with
    | Some controller when controller.SafeModeAvailable > 0 && not controller.SafeModeActive ->
        // The colony's own room and no other (`hostilesAtHome`, #201): a
        // claimer in an outpost is tapping a controller safe mode does not
        // cover, and the Keep it could be denting is not in that room.
        let here = hostilesAtHome view

        let withinReach (h: HostileInfo) =
            List.contains BodyPart.Claim h.Body
            && match Atlas.positionOf atlas controller.Id with
               // Across a border there is no range to measure (ADR 0052
               // decision 2), and a claimer in another room is tapping a
               // controller this safe mode does not cover — so None here is
               // "not in reach", where an unplaced controller below is still
               // "fire on sight".
               | Some tile ->
                   RoomPos.range h.Pos tile
                   |> Option.exists (fun r -> r <= view.Tuning.SafeModeDeadline)
               | None -> true

        let claimerInReach = here |> List.exists withinReach

        // Below full hits, off the walk the Repair pool reads: the Keep's whole
        // line is Full, so "hungry" and "damaged" are one fact and the two
        // readers cannot drift apart. The Posts and the ramparts are hungry on
        // their own lines and are not of the Keep.
        let keepDamaged =
            not (List.isEmpty here) && hungryStructures view |> List.exists (snd >> isKeep)

        // The undefended arm (ADR 0034 as #217 amends it): a colony with no
        // tower standing fires on the first armed hostile in its room.
        let undefended =
            List.isEmpty (Atlas.placedTowers atlas)
            && here
               |> List.exists (fun h ->
                   List.contains BodyPart.Attack h.Body
                   || List.contains BodyPart.RangedAttack h.Body)

        if claimerInReach || keepDamaged || undefended then
            [ ActivateSafeMode controller.Id ]
        else
            []
    | _ -> []

/// Colony reflex beside the pipeline (ADR 0014): every tower shoots the hostile
/// nearest to itself, every tick one stands in the room. Attack only — no tower
/// repair or heal — per-tower nearest with no focus fire or anti-drain gate,
/// and no energy gate: unlike safe mode there is no stock to protect, so a dry
/// tower's Intent fails harmlessly. Equal ranges tie-break by hostile id. Both
/// halves of the pairing are the colony's own room's: `placedTowers` has always
/// answered home alone — a tower stands only in a room we own — and the
/// hostiles are narrowed to match. That narrowing is the reflex's own rule and
/// not a repair for a missing join: a tower shoots inside its own room (ADR
/// 0014), and `RoomPos.range` answers None across a border.
let private planFire (view: ColonyView) atlas : Intent list =
    match hostilesAtHome view with
    | [] -> []
    | hostiles ->
        Atlas.placedTowers atlas
        |> List.choose (fun (towerId, tile) ->
            hostiles
            |> List.choose (fun h -> RoomPos.range tile h.Pos |> Option.map (fun r -> r, h))
            |> function
                | [] -> None
                | reachable ->
                    let _, target = reachable |> List.minBy (fun (r, h) -> r, h.Id)
                    Some(FireTower(towerId, target.Id)))

/// Extensions the controller level allows in the room (Screeps
/// CONTROLLER_STRUCTURES for "extension").
let private extensionAllowance level =
    match level with
    | 0
    | 1 -> 0
    | 2 -> 5
    | 3 -> 10
    | 4 -> 20
    | 5 -> 30
    | 6 -> 40
    | 7 -> 50
    | _ -> 60

/// Towers the controller level allows in the room (Screeps
/// CONTROLLER_STRUCTURES for "tower").
let private towerAllowance level =
    match level with
    | 0
    | 1
    | 2 -> 0
    | 3
    | 4 -> 1
    | 5
    | 6 -> 2
    | 7 -> 3
    | _ -> 6

/// Storages the controller level allows in the room (Screeps
/// CONTROLLER_STRUCTURES for "storage").
let private storageAllowance level =
    match level with
    | 0
    | 1
    | 2
    | 3 -> 0
    | _ -> 1

/// Whether the Layout places **road sites** at all this tick (ADR 0011 as #209
/// amends it): only for an `Independent` colony. Not an engine unlock — the
/// engine allows a road at RCL1 — but the stage below which a road is the wrong
/// spend: the trunk set a bootstrapping room plans is thousands of energy of
/// income placed in one tick, on the same surplus tier as the Upgrade and
/// nearer to hand, so every worker builds roads and nobody upgrades. One colony
/// at RCL1 planned some 19,000 energy of it against 8 a tick, ahead of the 200
/// progress that unlocks five extensions and doubles the body. This narrows ADR
/// 0010 and does not contradict it: what #209 says is that half a tick a loaded
/// step is not worth 2,400 ticks of income when the same energy buys the level
/// that doubles the body.
let private placesRoads (view: ColonyView) = isIndependent view

/// Colony-level planning step beside the Planner/Matcher pipeline: the
/// deterministic Layout (ADR 0011), computed whole from the Atlas every tick
/// and placed all at once — no persisted plan, no pacing. One ordering rule
/// eats every clustered structure: buildable tiles on the spawn's checkerboard
/// colour, nearest-to-spawn first, the working ground excluded, the Storage's
/// pick before the tower's and both before the extensions' (ADR 0022). Trunk
/// roads pave each source to the controller and to each spawn plus the swamps
/// of the controller's Work Area, priced on raw terrain and routed around every
/// reserved tile, reservations coming first. One tile beside each container
/// pick and beside the Storage is held as a Link footing (ADR 0022) and
/// outranks the tower and the extensions, the reservation being widened by the
/// footing count (ADR 0027); no link is ever placed on one (ADR 0038). Beside
/// all of that runs one rule that reads no tile of the ordering: a rampart
/// covers every standing Keep structure and every standing Post container (ADR
/// 0034), a rampart being no footprint at all.
let private planLayout
    (view: ColonyView)
    atlas
    : Intent list *
      ServedFooting list *
      UnservedFooting list *
      UnroutedTrunk list *
      DeferredContainer list
    =
    let home = Atlas.homeRoom atlas

    // The tile the whole plan is oriented on, and it is a tile of the room
    // being planned (ADR 0052 decision 2).
    let inHome (tile: RoomPos) = Some tile.Room = home

    let anchor =
        view.Spawns
        |> List.tryPick (fun s -> Atlas.positionOf atlas s.Id |> Option.filter inHome)

    match home, anchor, view.Controller with
    | Some room, Some anchorTile, Some controller ->
        let spawnPos = RoomPos.pos anchorTile
        // Same checkerboard colour as the spawn: clustered structures sit on
        // the spawn's colour, leaving the other colour free for movement.
        let parity = (spawnPos.X + spawnPos.Y) % 2

        // The sources this plan is for: the home room's alone (ADR 0041).
        let homeSources =
            view.Sources |> List.filter (fun s -> Atlas.targetRoom atlas s.Id = Some room)

        // The working ground — every source's Seats and the controller's
        // Upgrade Work Area — is off-limits (ADR 0022): a clustered structure
        // there eats a tile an Anchor or an upgrader stands on, so a colony
        // whose nearest same-colour tiles are working ground clusters one ring
        // out instead of eating them.
        let working = Atlas.workingGroundIn atlas room

        let buildable = Atlas.buildableTilesIn atlas room

        let ordering =
            buildable
            |> List.filter (fun tile ->
                (tile.X + tile.Y) % 2 = parity && not (Set.contains tile working))
            |> List.sortBy (fun tile -> range tile spawnPos, tile.X, tile.Y)

        // A kind's still-open gap at a level: its allowance there minus the
        // projection's censuses of standing and pending structures. Judged
        // at the level the kind is reserved for it sizes the reservation;
        // at the current level it sizes the placement.
        let gapAt allowanceOf built pending level =
            allowanceOf level - built - pending |> max 0

        // The room being planned, and no other (#140): the allowance is
        // this controller's, so what is subtracted from it is this room's
        // census — a neighbour's site counted here is a site this room
        // never places.
        let storageGap =
            gapAt
                storageAllowance
                (Atlas.builtStoragesIn atlas room)
                (Atlas.pendingStoragesIn atlas room)

        let towerGap =
            gapAt towerAllowance (Atlas.builtTowersIn atlas room) (Atlas.pendingTowersIn atlas room)

        let extensionGap =
            gapAt
                extensionAllowance
                (Atlas.builtExtensionsIn atlas room)
                (Atlas.pendingExtensionsIn atlas room)

        // The still-unclaimed slots, Storage first and tower next: a built or
        // pending structure keeps its tile out of the ordering (it is a target)
        // and its slot off the plan. The clustered kinds are sized at the
        // horizon; the Storage is not one of them and reads none (ADR 0022) —
        // its whole allowance is held from level 0, because once an extension
        // takes that tile it never comes back.
        let storageSlots = storageGap view.Tuning.StorageLevel
        let towerSlots = towerGap view.Tuning.HorizonLevel
        let extensionSlots = extensionGap view.Tuning.HorizonLevel

        // The Link footings cannot be named here — their targets are the
        // container picks, which are derived from the trunks the reservation is
        // for — but their count can: one per source, one for the controller
        // container, one for the Storage. The window is widened by that many, so
        // the tiles the cluster is pushed onto when a footing takes one of its
        // picks are inside the reservation too (ADR 0027).
        let footingSlots = List.length homeSources + 2

        let clustered =
            ordering
            |> List.truncate (storageSlots + towerSlots + extensionSlots + footingSlots)

        let storagePick = ordering |> List.truncate storageSlots

        // Reserved before trunks: a trunk never crosses a tile a reserved
        // structure will claim, and the widened window holds the footings as
        // well — so the precedence runs one way for every kind the Layout places
        // (ADR 0011). The footings' own tiles are settled below.
        let reserved = Set.ofList clustered

        // The reservation as the router reads it, joined once: the trunks
        // ask for it per source per goal, and a room name added to every
        // tile of it at each of those asks is a census tick's worth of
        // rebuilding for an answer that does not move (#216 R3).
        let reservedTiles = RoomPos.setAt room reserved

        // This room's share of the controller's Upgrade Work Area: a
        // controller the projection files under another room contributes
        // nothing here, rather than its coordinates (ADR 0052 decision 2).
        let upgradeArea =
            Atlas.workArea atlas (Upgrade controller.Id) |> RoomPos.inRoom room

        // Each goal beside the name it is recorded under when a source
        // cannot reach it (#107). The Upgrade Work Area first and the
        // spawns after, which is the order the routes are collected in and
        // therefore the order a loss reads in.
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

        // Every route the Layout asks for, kept per source and per goal:
        // the union paves the roads and each source's own trunk anchors
        // its container (ADR 0012), while the goals stay apart for the
        // reason `TrunkGoal` is a type — the loss below is per goal.
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

        // The controller's Work Area paves its swamps and only its swamps —
        // upgraders shuttle within it, so the dear ground gets a road and the
        // plain ground does not. No reservation can stand here: the Work Area is
        // working ground, which the ordering never offered (ADR 0022).
        let workAreaSwamps = upgradeArea |> Set.filter (Atlas.isSwampIn atlas room)

        // Every tile the Layout paves: the trunks plus the Work Area's
        // swamps. The road gap measures this against the projection's road
        // census, and a Link footing is chosen off it.
        let roadPlan = Set.union trunkTiles workAreaSwamps

        // The road gap reads the projection's road census: a built road or a
        // pending road site already claims its tile (ADR 0010).
        let roadGap =
            Set.difference roadPlan (Atlas.roadTilesIn atlas room)
            |> fun wanted -> Set.difference wanted (Atlas.pendingRoadTilesIn atlas room)

        // The road sites this tick: the whole gap once the colony is
        // `Independent`, none before it (#209). The stage gate is a filter on
        // the placement and not on the plan — `roadPlan` and `roadGap` are
        // computed at every stage, so the trunks still route around the
        // reservation — and it is a gate rather than pacing, which ADR 0011
        // rejected and still rejects. It is the same shape the clustered kinds
        // already have, one question coarser.
        let placedRoads =
            if placesRoads view then
                Set.difference roadGap (Atlas.pendingContainerTilesIn atlas room)
            else
                Set.empty

        // Containers (ADR 0012), computed whole like everything else and
        // RCL-gated by nothing — the engine allows them from level 0. Each
        // source's container sits on the Seat nearest that source's trunk; the
        // trunk's first tile is itself a Seat, so in practice the container
        // lands where the trunk leaves the source and harvest overflow falls
        // straight in. Seats are terrain geometry and trunks avoid only the
        // reservations, so the pick never shifts as the container gets built,
        // and Seats need no reservation dodge (ADR 0022).
        let sourceContainerPicks =
            sourceTrunks
            |> List.choose (fun (sourceId, trunk) ->
                let seats = Atlas.seatTilesOf atlas sourceId |> RoomPos.inRoom room

                if Set.isEmpty trunk || Set.isEmpty seats then
                    None
                else
                    seats
                    |> Set.toList
                    |> List.minBy (fun seat ->
                        trunk |> Set.toList |> List.map (range seat) |> List.min, seat.X, seat.Y)
                    |> fun seat -> Some(sourceId, seat))

        let sourceContainerTiles = sourceContainerPicks |> List.map snd

        // The controller container: an Upgrade-Work-Area tile beside a trunk
        // and off the road itself — the buffer upgraders work from standing
        // still, one tile from where the haulers drive. No reservation to dodge
        // either: the Work Area is working ground (ADR 0022).
        let controllerContainerTile =
            Atlas.positionOf atlas controller.Id
            |> Option.filter inHome
            |> Option.map RoomPos.pos
            |> Option.bind (fun controllerPos ->
                upgradeArea
                |> Set.filter (fun tile ->
                    not (Set.contains tile trunkTiles)
                    && not (Set.contains tile workAreaSwamps)
                    && trunkTiles |> Set.exists (fun t -> range tile t = 1))
                |> Set.toList
                |> function
                    | [] -> None
                    | candidates ->
                        candidates
                        |> List.minBy (fun tile -> range tile controllerPos, tile.X, tile.Y)
                        |> Some)

        // The Link footings (ADR 0022): one tile held for a link beside every
        // target a link will ever serve — each planned source container, the
        // controller container, and the Storage. Planned, not built: a Post
        // needs a standing container, so a Post-anchored rule would reserve
        // nothing at level 0 and the tiles would be gone by the time links
        // arrive. The count is the rule's, never a constant (ADR 0027). The
        // tiles are settled here rather than with the reservation because a
        // footing's targets are the container picks, derived from the trunks
        // the reservation is for; re-flooding the trunks to name the tiles
        // first would pay the tick's dearest step twice (ADR 0017).
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

        let footingTargetTiles = footingTargets |> List.map fst

        // A standing link is a target, so its own footing has stopped being
        // buildable: added back, or the footing would jump the tick the link
        // went up. The working ground is deliberately not subtracted — a footing
        // is the one structure footing allowed there (ADR 0022), because a link
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
                    && not (List.contains tile footingTargetTiles)
                    && not (Set.contains tile taken))
                |> Set.toList
                |> function
                    | [] ->
                        taken,
                        served,
                        {
                            Target = RoomPos.at room target
                            Kind = kind
                        }
                        :: unserved
                    | candidates ->
                        candidates
                        |> List.minBy (fun tile -> range tile spawnPos, tile.X, tile.Y)
                        |> fun tile ->
                            Set.add tile taken,
                            {
                                Target = RoomPos.at room target
                                Kind = kind
                                Tile = RoomPos.at room tile
                            }
                            :: served,
                            unserved)

        // The tower and the extensions take the ordering again with the
        // footings held out — a footing outranks both — and the Storage's pick
        // held out with them: it outranks the footings, which are anchored on
        // it.
        let clusterPicks =
            ordering
            |> List.filter (fun tile ->
                not (List.contains tile storagePick) && not (Set.contains tile footingTiles))
            |> List.truncate (towerSlots + extensionSlots)

        let towerTiles, extensionTiles =
            clusterPicks |> List.splitAt (min towerSlots clusterPicks.Length)

        // The container census the target clause is judged against (ADR 0040):
        // a container standing, or a site already going up.
        let containerCensus = Atlas.containerCensusIn atlas room

        // The target clause (ADR 0040): a source is served when a container
        // stands or is pending within range 1 of it, the controller when one
        // stands or is pending in its Upgrade Work Area — the geometry each
        // rule already reads a container by, not the tile this plan happens to
        // have picked. A served target is planned for no further container. A
        // pick the clause defers because something else serves its target is a
        // loss the room keeps — nothing demolishes the orphan — so it rides out
        // beside the footings and the trunks.
        let servingSource sourceId =
            Atlas.positionOf atlas sourceId
            |> Option.filter inHome
            |> Option.map (fun sourcePos ->
                Set.filter (servesSource (RoomPos.pos sourcePos)) containerCensus)
            |> Option.defaultValue Set.empty

        // Every target beside its pick and the containers already serving
        // it. Both answers below are read off this one list, so each
        // target is judged once and the same judgement decides whether it
        // is planned for and whether it lost its pick.
        let targets =
            [
                for sourceId, pick in sourceContainerPicks ->
                    ContainerTarget.Source sourceId, pick, servingSource sourceId
                for pick in Option.toList controllerContainerTile ->
                    ContainerTarget.Controller, pick, Set.intersect containerCensus upgradeArea
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

        // The tile clause (ADR 0040), and only it: a pick whose tile is still
        // owed a road waits, because the engine takes one construction site per
        // tile and the source container is planned onto the trunk's first tile.
        // This is about the tile and moves with no target. It reads the road
        // sites actually placed and not the whole gap (#209): below the gate no
        // road site is placed at all, so there is nothing to collide with and
        // nothing to wait for.
        let owedRoad = Set.union (Atlas.pendingRoadTilesIn atlas room) placedRoads

        let containerGap =
            unservedPicks |> List.filter (fun tile -> not (Set.contains tile owedRoad))

        // The ramparts (ADR 0034): one over every standing Keep structure and
        // every standing Post container, the tick the thing it covers stands —
        // a site is not covered until it is a structure. No allowance to size
        // against: the rule is the whole plan, so the gap is the covering
        // census alone, standing ramparts and pending sites subtracted the way
        // the roads' is. The one gate is the colony's [[stage]]
        // (`keepsRamparts`).
        let covered =
            if keepsRamparts view then
                Set.union (Atlas.keepTilesIn atlas room) (Atlas.postContainerTilesIn atlas room)
            else
                Set.empty

        let rampartGap =
            Set.difference
                covered
                (Set.union
                    (Atlas.rampartTilesIn atlas room)
                    (Atlas.pendingRampartTilesIn atlas room))

        let place kind tiles =
            tiles
            |> List.map (fun tile -> PlaceConstructionSite(RoomPos.at room tile, kind))

        place Storage (storagePick |> List.truncate (storageGap controller.Level))
        @ place Tower (towerTiles |> List.truncate (towerGap controller.Level))
        @ place Extension (extensionTiles |> List.truncate (extensionGap controller.Level))
        @ place Road (Set.toList placedRoads)
        @ place Container containerGap
        @ place Rampart (Set.toList rampartGap),
        List.rev servedFootings,
        List.rev unservedFootings,
        unroutedTrunks,
        deferredContainers
    // A room the Layout cannot even orient itself in plans nothing and
    // loses nothing: there are no targets to serve and no trunk was ever
    // asked for, so every record is empty rather than any of them being a
    // shortfall (#77, #106, #107).
    | _ -> [], [], [], [], []

/// The outpost's source containers (ADR 0042) — the colony's one placement rule
/// that is not the Layout's, and a rule beside it rather than a branch inside
/// it: one container per outpost source, on the Seat whose walk out to the Seam
/// toward home is shortest. **Why it is not the Layout's.** The Layout seats a
/// source container on the Seat nearest that source's trunk, and a trunk is a
/// paved line to a spawn; an outpost has no spawn, so the pick needs another
/// anchor, and the Seam is the only fixed thing in that room home lies beyond.
/// Nothing here orders a clustered pick, reserves a Link footing, paves a trunk
/// or enters the layout record's three lists: every one of those is a fact
/// about the home room's plan, which ADR 0042 leaves untouched. **The room is
/// the outpost's own**, stamped from the projection's id-to-room join (ADR
/// 0041): the Layout stamps the single room it plans onto every site it emits,
/// so an outpost pick routed through that path would drop a container site on
/// the *home* room's tile of the same coordinates. **ADR 0040 holds here, by
/// target rather than by tile**: a source with a container standing or pending
/// within range 1 is served wherever the thing serving it sits, and the census
/// is read in that source's own room, or a home container on its coordinates
/// would defer the plan forever. **And a tile clause after all** (#244). ADR
/// 0040 keeps the two questions apart and this rule was written with only the
/// target one, on the premise that nothing paves an outpost — but a *human*
/// does: live in W13S29 the user laid road sites across the Seats both picks
/// answered, and since the engine takes one construction site per tile the
/// Executor asked for a container on an occupied tile and was answered
/// ERR_INVALID_TARGET once a tick, for ever — no container, so no Post, no
/// Anchor and no income, behind a road the surplus tier gives two workers
/// hundreds of ticks to finish. So a Seat holding a site of another kind is no
/// candidate (`Atlas.nonContainerSiteTilesIn`), the pick is the shortest walk
/// over the Seats that are left, and a source whose every Seat is taken plans
/// nothing and waits: asking the engine for a refusal once a tick is not a
/// plan, and this colony has no vocabulary for cancelling a human's site.
/// **That wait clears no faster than the builders' budget reaches the site**
/// (#266), and until #266 it did not clear at all: the Seat frees when the
/// site on it is *built*, and nothing built it — a non-container site in an
/// outpost was a plain Surplus Build with no home rung (#234) and outside the
/// builders' budget (#157, which was keyed on a container site), so travel
/// cost kept every loaded worker at the home controller. The same budget now
/// queues those sites, the [[seam]] nearest first, so the Seat frees of itself
/// once its site reaches the head of that queue — which is a wait on a queue
/// and no longer a wait on the human. It is still a wait, and can be a long
/// one: the queue is `Tuning.OutpostBuilders` long against a trunk 45 sites
/// long, and a Seat is wherever in it the human happened to pave. What keeps
/// its travel cost meanwhile is everything behind the head ("an ordinary
/// outpost site keeps its travel cost"), and the outage is silent either way,
/// the `-7` line having been the only thing that ever said so. A
/// **standing** road is not subtracted and must not be: a container site goes
/// down on a built road, and out here that is the best tile there is. It is a
/// collision rule and not a target one, so ADR 0040's "by target, not by tile"
/// is untouched. **Only into a room the colony can see.**
/// Both halves of the rule are paid for by vision, and a blind room's empty
/// census is a missing entry and not a "no container" (ADR 0004): planning off
/// it would hand the Executor an Intent it can only report as `ActorMissing`,
/// once a tick per rock for ever. Nothing is lost by waiting — a Harvest names
/// an outpost rock with no vision at all. **Recomputed every tick, and
/// deliberately not ridden on the plan memo.** The signature does not sign this
/// rule's other inputs — the outpost's terrain, its declared source tiles, its
/// Seam band, and the *pending* census its own site lands in — and signing the
/// pending half would throw the whole Layout and spawn-walk table (ADR 0032)
/// away the tick an outpost site appears. What it would buy is one flood a room
/// a tick, measured at 4.84 ms a tick without this rule and 5.35 with it,
/// against ADR 0041's revisit trigger of a 50 ms mean. Total (ADR 0004): a
/// source the projection does not place, a source in a room the colony cannot
/// see, a room with no Seam band to home, and a source no Seat of which can
/// reach one all plan nothing.
let private planOutpostContainers (view: ColonyView) atlas : Intent list =
    let home = SpatialInfo.homeName view.Spatial

    // Every rock the projection places in a room that is not home and that
    // the colony is looking into this tick — `RoomControl` carries one
    // entry per seen room, and vision is what both the census below and
    // the Executor's own `Game.rooms` lookup are paid for with.
    view.Sources
    |> List.choose (fun s ->
        match Atlas.positionOf atlas s.Id with
        | Some tile when tile.Room <> home && Map.containsKey tile.Room view.RoomControl ->
            Some(s.Id, tile)
        | _ -> None)
    |> List.choose (fun (sourceId, source) ->
        let room = source.Room

        let served =
            Atlas.containerCensusIn atlas room
            |> Set.exists (servesSource (RoomPos.pos source))

        if served then
            None
        else
            // The pick, and with it the tie-break — the same trap the Layout's
            // own pick has: three Seats all of swamp can price identically, so
            // the lowest (X, Y) answers, exactly as every other tie in the
            // colony answers. Over the Seats a site of another kind has not
            // already taken (#244): one construction site per tile is the
            // engine's rule, and a pick onto a taken tile is refused every
            // tick until that site is built.
            Set.difference
                (Atlas.seatTilesOf atlas sourceId |> RoomPos.inRoom room)
                (Atlas.nonContainerSiteTilesIn atlas room)
            |> Set.toList
            |> List.choose (fun seat ->
                Atlas.seamWalkTicks atlas room home seat |> Option.map (fun walk -> walk, seat))
            |> function
                | [] -> None
                | priced ->
                    let _, seat =
                        priced |> List.minBy (fun (walk, seat: Pos) -> walk, seat.X, seat.Y)

                    Some(PlaceConstructionSite(RoomPos.at room seat, Container)))

/// Colony reflex beside the pipeline, the second after safe mode: every creep
/// with free carry capacity standing within pickup range of a dropped energy
/// pile asks to pick it up — beside its assigned Task's action, since the
/// engine's pickup conflicts with no other action. No movement, no matching, no
/// threshold: the reflex only recaptures what is already in reach, and duplicate
/// pickups on one pile are the engine's to settle.
///
/// Paired once per room the projection places a creep in, and never across two:
/// a pickup is a range-1 act inside one room, and a pile in one room and a creep
/// in another on the same coordinate would draw a pickup the engine answers
/// ERR_NOT_IN_RANGE. Both sides carry that room in the tile, so the range is
/// measured or it is not measured at all. The room that made it necessary is the
/// outpost (ADR 0042): its hauler runs one container, so the Anchor's overflow
/// lands on the container's own tile, a full container turns that overflow into
/// a pile, and the hauler then stands *on* the pile and walked away from it.
let private planPickups (view: ColonyView) atlas : Intent list =
    let hungry =
        view.Creeps
        |> List.filter (fun c -> c.FreeCapacity > 0)
        |> List.map (fun c -> c.Name)
        |> Set.ofList

    Atlas.placedCreeps atlas
    |> List.groupBy (fun (_, tile) -> tile.Room)
    |> List.collect (fun (room, placed) ->
        match Atlas.droppedEnergyIn atlas room with
        | [] -> []
        | piles ->
            placed
            |> List.collect (fun (name, pos) ->
                if Set.contains name hungry then
                    piles
                    |> List.choose (fun (pile, tile) ->
                        if RoomPos.range pos tile |> Option.exists (fun r -> r <= 1) then
                            Some(PickupEnergy(name, pile))
                        else
                            None)
                else
                    []))

/// Ticks until a source restocks (ADR 0025), 0 while it holds energy —
/// and 0 for a source the view does not carry at all, so a source
/// nothing projects never holds a decision up.
let private ticksToRestock (view: ColonyView) sourceId =
    view.Sources
    |> List.tryFind (fun s -> s.Id = sourceId)
    |> Option.map (fun s -> s.TicksToRestock)
    |> Option.defaultValue 0

/// Whether a creep garrisons a source's container Post: ADR 0024's condition —
/// a Work-heavy body standing on that source's built container.
let private garrisons atlas (creep: CreepInfo) sourceId =
    Atlas.workHeavy atlas creep.Name
    && Atlas.catchesOverflow atlas creep.Name sourceId

/// Whether a source's **rate** still outruns what the bodies garrisoning it
/// take — **whether a rock has a dig left in it worth a walk** (#235). A Post's
/// Anchor is sized to saturate its rock (ADR 0021: the Work that drain the
/// whole regeneration, plus one spare), so a manned Post ordinarily leaves
/// nothing over: a light body joining it takes energy the garrison would have
/// taken anyway, the colony earns not one point for the trip, and the seats it
/// fills are seats the garrison itself competes for (ADR 0051's cap is over the
/// source, and live a mother's two workers took the last two of an outpost's
/// three and its own Anchor read `none-free`). Live at t199,88x a worker with
/// nine free walked a Seam for one dig on a rock a six-Work Anchor was already
/// draining.
///
/// Named for the **rate** because that is the number it reads, and the file
/// spells the two apart on purpose (ADR 0042, #208): `sourceOutputOf` is the
/// quota's number — the row's *cast* capped at that rate — and reading it here
/// would have a colony too poor to cast a saturating Anchor read its own
/// half-worked rock as spent and keep its workers off the half nobody is
/// digging. The garrison side is the **living** bodies, for the same reason
/// from the other end: this gate prices one walk this tick, where a quota read
/// off a cast would go on pricing a body that has died. Zero garrison on an
/// unposted rock and on a vacant Post, so both stay open — the safety valve
/// that keeps a colony whose Anchor has just died from starving. A rate the
/// projection cannot price is no evidence of saturation (ADR 0004).
///
/// The **standing** Posts and not `Atlas.postsOf`, which is `Decide.isPosted`'s
/// own query for this same question (ADR 0042 as #205 amended it): a container
/// site is a garrison place and not yet an economy, so the twelve a tick its
/// Anchor digs goes into construction progress and reaches no store — and there
/// is no container standing beside the rock to Withdraw from either, so a rock
/// closed at the site stage leaves the whole light row with no Feeding intake
/// at all while both home Posts are still being raised. The clause starts
/// biting the tick the container stands, which is the tick the rock joins the
/// economy the clause is rationing.
///
/// Read here in `applicable` and not as a `Capacity.Commuters` of zero, where
/// ADR 0052 decision 6 otherwise keeps the per-source numbers: a capacity
/// bounds the crowd a Task admits but never evicts a body already holding it,
/// and #235's case (b) is exactly an eviction — the outpost Anchor stands up
/// and the squatting light body has to be released that tick, not merely
/// refused the next time it asks.
let private hasSpareRate (view: ColonyView) atlas (sourceId: string) =
    let posts = Atlas.standingPostsOf atlas sourceId

    let dug =
        if Set.isEmpty posts then
            0
        else
            // Read off the bodies *standing* on the Posts, the way `garrisons`
            // above reads one — a Post's garrison is a fact about where a body
            // is (ADR 0024) — and off the heavy ones alone, because ADR 0051
            // keeps the light bodies' Work Area off these tiles: one standing
            // there is squatting the Post, not working it. A body standing on
            // one of these tiles digs *some* source, and where two rocks share
            // a Seat it is charged to both — the ambiguity `Atlas.postsOf` has
            // carried since ADR 0024's cap, not one this gate introduces.
            view.Creeps
            |> List.sumBy (fun creep ->
                if
                    Atlas.workHeavy atlas creep.Name
                    && Atlas.creepTile atlas creep.Name
                       |> Option.exists (fun tile -> Set.contains tile posts)
                then
                    (creep.Body |> Map.tryFind Work |> Option.defaultValue 0)
                    * Engine.harvestPerWork
                else
                    0)

    sourceRateOf view atlas sourceId |> Option.forall (fun rate -> rate > dug)

/// Whether a source has a [[post]] with no garrison standing on it — **whether
/// the walk this body is about to make ends on a tile it can have** (#258). A
/// **heavy** body alone mans a Post, because ADR 0051 keeps every other body
/// off those tiles and one standing there is squatting the Post rather than
/// working it — `hasSpareRate`'s reading of the bodies, and the candidate never
/// counts against itself, the way the Matcher's own garrison count does not.
///
/// **Every** Post of the rock and not the standing ones alone, which is the
/// census the clause in `applicable` that reads this already asks: the question
/// here is standing room, and a Post whose container is still a site is a tile
/// a body stands on and raises (#205). `hasSpareRate` reads the standing census
/// instead because its question is the rock's *economy*, and a site is a
/// garrison place and not yet an economy. Read here, the standing census would
/// strand the body the walk-home clause exists for: a rock carrying a manned
/// container Post and a site Post beside it would read occupied for a full body
/// one step off that site, and Build asks for the exact tile (#205, #234) — so
/// that body would hold no Task at all.
///
/// Read **now** and not at arrival, which is where this parts from every other
/// count of a Post. ADR 0026 discounts a holder that will be dead when the
/// candidate gets there — the Matcher's cap and ADR 0053's `emptyPostCaps` both
/// take that reading, and say so — so a Post ninety ticks away reads free to
/// any heavy body in the colony, and live at 204,966 an Anchor a border away
/// that had just lost its own rock took the walk home on it and stood beside a
/// garrison that outlived its arrival by hundreds of ticks (user, 2026-09-08).
/// The discount is safe for the body a cast was aimed at, and this gate cannot
/// tell one of those from a released squatter, so it is refused here: is there
/// standing room, this tick, on the rock this walk ends at. The price is a full
/// body whose incumbent is genuinely [[expiring]], which waits where it stands
/// rather than timing its walk; the alternative is the live case above. A rock
/// with no Post at all stays open, the same safety valve `hasSpareRate` keeps —
/// the clause below never asks with one, having settled it a conjunct earlier.
let private hasUnmannedPost (view: ColonyView) atlas (creep: CreepInfo) (sourceId: string) =
    let posts = Atlas.postsOf atlas sourceId

    if Set.isEmpty posts then
        true
    else
        let manned =
            view.Creeps
            |> List.choose (fun c ->
                if c.Name <> creep.Name && Atlas.workHeavy atlas c.Name then
                    Atlas.creepTile atlas c.Name
                else
                    None)
            |> Set.ofList

        posts |> Set.exists (fun tile -> not (Set.contains tile manned))

/// Whether a Work-heavy body holds a source through its empty window: the
/// **empty-source** reprieve, which ADR 0048 widened off the container to the
/// source's whole digging range. Named for the reprieve and not for the Post on
/// purpose: the second option ADR 0048 rejected by name was widening this to
/// the source's Posts, and the tile the Anchor is bumped onto is not one. ADR
/// 0025 wrote the two reprieves as one judgement about one tile, and the room
/// proved them different questions — a hauler drawing the container swaps the
/// Anchor onto the Seat beside it, and on the container-only condition that one
/// step released it TooEarly. Overflow is a fact about the tile underfoot;
/// being in position to dig is a fact about the range.
let private keepsThroughEmptyWindow atlas (creep: CreepInfo) sourceId =
    garrisons atlas creep sourceId
    || (Atlas.workHeavy atlas creep.Name
        && Atlas.standsAtSource atlas creep.Name sourceId
        && not (Atlas.standsOnDualSeat atlas creep.Name))

/// The walk and the wait that hold a Task up for this creep, or None when its
/// time has come (ADR 0025, repriced by ADR 0029): a drained source's Harvest
/// is applicable only when the creep's walk covers the restock wait — walk >=
/// ticks to restock, with no slack, because the wait shrinks by one each tick
/// while the walk stays put, so a creep one tick short departs one tick later
/// and arrives as the energy does. The walk is the Atlas's own query, already
/// whole ticks and blind to today's traffic, so a bystander in the lane cannot
/// dispatch a creep this tick and recall it the next. A creep already beside a
/// dry rock has no walk to cover anything and is released (ADR 0013). One
/// exemption, ADR 0024's condition as ADR 0048 widened it: a Work-heavy body
/// already in digging range keeps its Post through the window, a bare Dual Seat
/// subtracted. **One rule for both bodies** (#258, retiring ADR 0048's heavy
/// arm): the walk covers the wait or it does not, and how many ticks a tile
/// costs this body is already in the walk. ADR 0048 read the same walk as
/// earliness for a Work-heavy body whatever its length, because "the walk
/// covers the wait, so set out now" had dispatched an Anchor across half a room
/// onto a Post another Anchor was standing on — a **capacity** question, which
/// the Post count answers (ADR 0024, ADR 0051) and `applicable` below answers
/// again, this tick and not at arrival, for a *full* body still walking. The
/// window that left — a Post whose garrison holds some *other* Task this tick,
/// a bare [[dual seat]]'s upgrading through this same empty window, counted by
/// neither gate, so an empty heavy body far enough out was dispatched onto it —
/// is closed in the cap and not here (#269): the Post census the Heavy cap
/// counts is the bodies standing on the rock's Posts unioned with the Task's
/// own holders, so a manned Post never reads vacant. Here it stays one
/// question about the walk, because a heavy body with a free store must keep
/// the walk this gate would otherwise refuse it, or no successor could ever be
/// sent to the Post its expiring incumbent is standing on (ADR 0026) — and that
/// discount is the cap's alone to give, to an incumbent that will be **dead**
/// on arrival and never to one that will still be standing there. What the
/// heavy arm had left was the release it caused: an Anchor whose outpost rock
/// was dug out from under it mid-walk was released at ninety tiles of walk
/// against fifty ticks of wait, went `none-in-time`, and re-matched a home rock
/// it had no business on — twice, the vacancy it left behind casting a second
/// Anchor (user, 2026-09-08). Every other Task is judged at the current tick. Two
/// consequences, both ADR 0004's totality.
let private tooEarly (view: ColonyView) atlas (creep: CreepInfo) task (walk: Lazy<int option>) =
    match task with
    | Harvest sourceId ->
        match walk.Value with
        // No walk at all is unreachable geometry, which is not earliness:
        // the reachability gate stands ahead of this one in both cascades
        // and names that rejection itself (ADR 0002, ADR 0029).
        | None -> None
        | Some ticks ->
            let wait = ticksToRestock view sourceId

            // A stocked source is a wait of zero, which every walk covers:
            // the arm below answers it either way, and asking it first
            // keeps the reprieve's Atlas joins off the pairs a stocked
            // pool is mostly made of.
            if wait = 0 || keepsThroughEmptyWindow atlas creep sourceId then
                None
            elif ticks < wait then
                Some(ticks, wait)
            else
                None
    | Withdraw _
    // A pile is workable the tick a creep reaches it and every tick before. It
    // moves — down by decay, up under an [[anchor]] spilling onto a full
    // [[container]] — but neither direction is a restock, so there is no tick
    // to be early *of*.
    | Pickup _
    | Refill _
    | Build _
    | Repair _
    | Upgrade _
    // A controller is always there to be reserved: a reservation has no restock
    // and no stock, so a reserver that has walked to one is never early (ADR
    // 0042).
    | Reserve _
    | Claim _
    // A [[threat]] standing in a room is there to be hit the tick a guard
    // arrives and every tick before: a fight has no restock (ADR 0056).
    | Guard _
    | Flee -> None

/// The room a Task's Work Area lies in: its target's, since the area is that
/// target's surroundings and empty across a border (ADR 0020, ADR 0041) — so the
/// Reach taken out of it is that room's share. None for Flee, whose area is the
/// creep's own room's, and for a target the projection does not place. A Guard
/// names its room outright (ADR 0056) — the Planner keyed it on one — and the
/// case is the whole answer for a Task this function is never asked about:
/// `areaFor` below is its only caller and it settles a Guard two cases earlier,
/// no Reach being taken out of a Guard's area at all. The case stands because
/// the match is exhaustive and a Task it forgot would be a build error.
let private roomOfWork atlas task =
    match task with
    | Harvest id
    | Withdraw id
    | Pickup id
    | Refill id
    | Build id
    | Repair id
    | Upgrade id
    | Reserve id
    | Claim id -> Atlas.targetRoom atlas id
    | Guard room -> Some room
    | Flee -> None

/// The tiles a creep may work a Task from this tick (ADR 0033): its Work Area
/// less its room's Reach — and for Flee, the safe set of the room the creep
/// stands in, an area of the colony's own rather than some target's
/// surroundings. Each is the share of one room: a hostile a room away on the
/// same coordinate takes no tile here.
let private areaFor (threats: Threats) atlas creep task : Set<RoomPos> =
    match task with
    | Flee ->
        Atlas.creepRoom atlas creep
        |> Option.map (Threats.safeIn threats)
        |> Option.defaultValue Set.empty
    // The [[guard]]'s ground, and the one area no Reach is taken out of (ADR
    // 0056): it is *made* of Reach tiles, one ring around every Threat in the
    // room the Planner keyed the Task on, so the subtraction below would empty
    // it on every tick the Task exists. A colony fact like the safe set beside
    // it and no target's surroundings, so the room is the Task's own and never
    // the creep's — a body a border away is offered the same ring, and it is
    // the price of walking there that decides whether it can have it.
    | Guard room -> Threats.ringIn threats room
    | _ when Map.isEmpty threats.Reach -> Atlas.workAreaFor atlas creep task
    | _ ->
        // A Reach is one room's grid (`Threats.Reach`), so the tiles it
        // takes are matched on that room's coordinates and on no other's
        // (ADR 0052 decision 2, #138).
        let room = roomOfWork atlas task

        let reach =
            room |> Option.map (Threats.reachIn threats) |> Option.defaultValue Set.empty

        Atlas.workAreaFor atlas creep task
        |> Set.filter (fun tile ->
            not (Some tile.Room = room && Set.contains (RoomPos.pos tile) reach))

/// The travel cost of a Task for a creep, priced over the tiles it may actually
/// work from this tick (ADR 0033): the safe set for Flee, and every other
/// Task's own Work Area less the Reach — so the reachability gate judges the
/// tiles that are left rather than a tile the creep may not stand on, and a
/// candidate whose cold remainder is walled off is rejected as unreachable
/// instead of being held and never worked. The pricing itself is untouched:
/// same weights, same surcharge, same flood, only the goals are this tick's. An
/// area that is empty here was never taken by the Reach — the threat gate
/// stands ahead of this one in both cascades — so it falls back to the Task's
/// own price, which carries ADR 0004's escape for an unplaceable target and the
/// Seam join for a target in another room. The Work Area a creep is handed is
/// empty across a border by construction (ADR 0041), so an outpost's Task ranks
/// in the one pool through this fallback rather than a case of its own.
///
/// The Guard is priced off its area rather than off a target, for Flee's own
/// reason (ADR 0056): that area is the colony's `Threats` and not a target's
/// surroundings, so there is no unplaceable-target escape to fall back to and an
/// empty ring is honestly nowhere to stand. It crosses a border where Flee never
/// has to, though — the safe set is the creep's own room's, while a Guard's ring
/// is the room the Planner keyed the Task on, which is an outpost and never the
/// room the guard row cast the body in. So it prices through `travelCostToward`,
/// the same Seam-band minimum every cross-room Task is ranked by, taken toward a
/// named room's tiles instead of toward a placed target's: the walk to the fight
/// is what decides whether a body standing at the oven can have it, exactly as an
/// outpost's Harvest is offered to a body standing at home.
let private travelCostOf (threats: Threats) atlas (creep: string) task =
    match task with
    | Flee -> Atlas.travelCostWithin atlas creep (areaFor threats atlas creep task)
    | Guard room -> Atlas.travelCostToward atlas creep task room (areaFor threats atlas creep task)
    | _ ->
        match areaFor threats atlas creep task with
        | area when Set.isEmpty area -> Atlas.travelCost atlas creep task
        | area -> Atlas.travelCostWithin atlas creep area

/// The mover's step toward the tiles a creep may work its Task from, and the
/// mate of `travelCostOf` above: whatever the price crossed for, the walk has to
/// cross for too, or a body is ranked onto a Task it is never carried to. A
/// Guard names its own room and is stepped toward it (`firstStepToward`); every
/// other Task derives its crossing from the target the projection places
/// (`firstStep`).
let private stepToward atlas (creep: string) task (area: Set<RoomPos>) =
    match task with
    | Guard room -> Atlas.firstStepToward atlas creep task room area
    | _ -> Atlas.firstStep atlas creep task area

/// Whether the Reach has taken a creep's whole Work Area for a Task (ADR 0033):
/// it had somewhere to stand and has nowhere left. That makes the Task
/// inapplicable to that creep — a Harvest whose only Seat is hot is no Harvest
/// — and releases a holder under a reason of its own, so the transition log
/// tells a raid's release from a Task that vanished. An area that was empty to
/// begin with is not threatened: unplaceable or blocked geometry is the
/// reachability gate's answer (ADR 0002).
let private threatened (threats: Threats) atlas (creep: CreepInfo) task =
    not (Set.isEmpty (Atlas.workAreaFor atlas creep.Name task))
    && Set.isEmpty (areaFor threats atlas creep.Name task)

/// Whether the creep itself is standing where it can be hurt: its own tile
/// inside a Reach of its own room (ADR 0033). Flee's applicability is this and
/// a body that can run, and the vision grace asks it too — the grace is the one
/// keep that answers for a Task no gate below can be asked about, so the
/// question ADR 0033 puts above all work is asked of the creep instead. Total
/// (ADR 0004): a creep the projection cannot place stands in no Reach.
let private standsInReach (threats: Threats) atlas (creep: string) =
    match Atlas.creepTile atlas creep with
    | Some tile -> Set.contains (RoomPos.pos tile) (Threats.reachIn threats tile.Room)
    | None -> false

/// Whether a construction site stands in a room this colony **mines** — an
/// [[outpost]]'s, and so a site the outpost builders' budget may ration rather
/// than a piece of the home room's surplus. One half of that queue's reading
/// and not the whole of it: `planPool` narrows it again by what no other rule
/// already feeds, because a claimed room a human still names in this colony's
/// outpost list is not `Borrowed` and answers true here (`Colony.bootstrapping`,
/// ADR 0047 decision 1). The room half alone since #266:
/// what the budget covered was the container site this colony places itself
/// (ADR 0042), and out there a human paves too (ADR 0042 as #244 amends it) —
/// live, 45 hand-laid road sites in W13S29 stood at 0/300 for as long as they
/// were surplus, because a loaded worker at home is a step from its own
/// controller and a Seam plus sixty tiles from the trunk. So the kind half is
/// gone from the *reading* and survives as the order the budget is spent in
/// (`planPool`), the container staying the switch it always was. Read off the
/// projection and not off the declaration (ADR 0041), exactly as the Reserve
/// pool's room join is, so a room a stand-down drops from the scan set (ADR
/// 0043) leaves this reading with it. Total (ADR 0004): an unplaced site names
/// no room, answers false, and is the ordinary surplus Build it has always
/// been.
let private isOutpostSite (view: ColonyView) atlas siteId =
    Atlas.targetRoom atlas siteId
    |> Option.exists (fun room ->
        room <> SpatialInfo.homeName view.Spatial
        // A borrowed room's site is the child's own and not an outpost's (user
        // decision 2026-09-07): it neither draws the outpost builders' budget
        // nor dilutes it — the nursery's and the bootstrapping child's sites
        // reach the pool by their own rules, and the budget is spread over the
        // sites of rooms the colony *mines*.
        && not (List.contains room view.Borrowed.Rooms))

/// Whether a construction site stands in a **nursery** — a room this colony has
/// claimed and not yet stood a spawn in (ADR 0047 decision 4). `isOutpostSite`'s
/// room read asked one question deeper — an outpost is a room this colony mines
/// and a nursery is one it has claimed — and answered a rank deeper with it: in
/// a nursery **every** site is feeding-tier outright, where in an outpost only
/// the builders' budget's own head is. Total the same way (ADR 0004).
let private isNurserySite (view: ColonyView) atlas siteId =
    Atlas.targetRoom atlas siteId |> Option.exists (isNurseryRoom view)

/// Whether a room is **bootstrapping** as seen from this colony's tick: a child
/// of ours running its own spawn (the mother's reading), or this colony's own
/// home standing at the `Bootstrapping` stage (the child's own reading). One
/// predicate for both ticks, because the rule that reads it is about the room
/// and not about who is looking (ADR 0052 decision 3). The home half reads the
/// stage and not a level of its own.
let private isBootstrappingRoom (view: ColonyView) room =
    isBootstrapRoom view room
    || (room = SpatialInfo.homeName view.Spatial && homeStage view = Some Bootstrapping)

/// A site standing in a bootstrapping room: feeding-tier in both pools (user,
/// 2026-09-06). What a room under RCL3 builds is its containers and its
/// extensions, and the extensions are the bank — 300 to 550 doubles the Anchor
/// body and with it the income the whole window is waiting on — so they come
/// before the controller, for the child's own workers and for the pioneers
/// alike.
let private isBootstrappingSite (view: ColonyView) atlas siteId =
    Atlas.targetRoom atlas siteId |> Option.exists (isBootstrappingRoom view)

/// Whether any site stands in the room of the named controller — the
/// borrowed Upgrade's other half: while the child has sites, its
/// controller waits.
let private sitesPendingBeside (view: ColonyView) atlas controllerId =
    match Atlas.targetRoom atlas controllerId with
    | None -> false
    | Some room ->
        view.ConstructionSites
        |> List.exists (fun site -> Atlas.targetRoom atlas site.Id = Some room)

/// Whether this Build is on the feeding tier rather than in the surplus the
/// colony's other sites are spent out of — the three rules that lift one there,
/// said once. One reader is left: `tierOf`, and nothing else. ADR 0052 decision
/// 6 had folded the body gate into this reading too, and then #234 lifted every
/// home site a rung over the Upgrade beside it, leaving no Build on the ladder
/// travel cost still thins, so that gate stopped asking about the target. The
/// outpost sites the builders' budget has picked out this tick — handed in,
/// because which they are is a fact about the whole outpost's queue and not
/// about the one site (#266) — the container among them being ADR 0042's switch
/// on whether that room is in the economy at all; and every site in a nursery,
/// the switch on whether there is going to be a second colony at all (ADR
/// 0047).
let private isFeedingSite (view: ColonyView) atlas (fed: Set<string>) siteId =
    Set.contains siteId fed
    || isNurserySite view atlas siteId
    || isBootstrappingSite view atlas siteId

/// Whether a site stands in this colony's **own home room** — the room #234's
/// surplus rung is scoped to, and the one question that separates the site a
/// colony grows by from a site it would cross a [[seam]] for. The rung lifts a
/// Build over the Upgrade it shares the surplus tier with, and a rank the whole
/// colony shares is exactly what [[travel cost]] can no longer thin. At home
/// that is the point: the sites and the controller stand a few tiles apart. The
/// room join is `isOutpostSite`'s (ADR 0041), and total (ADR 0004)
/// resolved toward home.
let private isHomeSite (view: ColonyView) atlas siteId =
    Atlas.targetRoom atlas siteId
    |> Option.forall (fun room -> room = SpatialInfo.homeName view.Spatial)

/// The full downgrade timer per controller level (Screeps
/// CONTROLLER_DOWNGRADE).
let private fullDowngradeTimer level =
    match level with
    | 1 -> 20000
    | 2 -> 10000
    | 3 -> 20000
    | 4 -> 40000
    | 5 -> 80000
    | 6 -> 120000
    | 7 -> 150000
    | _ -> 200000

/// The hard deadline on the controller's downgrade timer: half the level's full
/// timer. The engine refuses activateSafeMode once the timer sinks below half
/// minus 5,000 (its grace), so escalating at half keeps the safe-mode reflex
/// fireable with the whole grace still banked — a downgrade costs a level and
/// zeroes the stock, so neither line is ever approached (ADR 0007).
let private downgradeDeadline level = fullDowngradeTimer level / 2

/// Whether the controller stands inside its downgrade deadline (ADR 0007).
let private insideDowngradeDeadline (view: ColonyView) =
    view.Controller
    |> Option.exists (fun c -> c.TicksToDowngrade <= downgradeDeadline c.Level)

/// The tier of work a Task belongs to, once its target is taken into account
/// (ADR 0010, ADR 0012, ADR 0023) — the ladder `planPool` sets each entry's
/// [[priority]] off.
type private Tier =
    /// Getting out of a Reach (ADR 0033): the one Task in it is Flee, and
    /// it sits above every other tier and above the downgrade deadline
    /// too, because no other work matters while a creep is being killed.
    | Safety
    /// Feeding the economy: Harvest, a container's Withdraw, the Refill of a
    /// spawn or an extension, Reserve, the Build of the outpost sites the
    /// builders' budget has picked out this tick (#157, widened by #266) and
    /// every site in a **nursery** (ADR 0047) — the flow the colony's
    /// reproduction runs on, and beside it ADR 0042's two switches on a third of
    /// that flow: the Reserve that decides how fast an outpost's rock gives, and
    /// the Build that decides whether the room is in the economy at all — the
    /// container that makes the rock a Post, and the trunk the haul off it is
    /// priced on. The nursery's sites are the third switch and the deepest of
    /// them.
    | Feeding
    /// The Storage's Withdraw (ADR 0023): the colony's stock as an intake, one
    /// tier below the source containers the flow fills, so a stock standing
    /// beside the spawn never wins the travel-cost tie the containers have to
    /// win.
    | StockDraw
    /// Surplus work: a tower Refill (ADR 0010), Build, Repair and Upgrade. The
    /// colony feeds its own reproduction before its guns, and everything it
    /// merely spends energy on waits behind the flow.
    | Surplus
    /// The controller container's Refill (ADR 0012): a full creep beside the
    /// buffer sinks its load into the controller rather than dumping it back
    /// into the container it just drew from and orbiting in place, so the buffer
    /// is filled by bodies with no surplus work of their own.
    | UpgradeBuffer
    /// The Storage's Refill (ADR 0023): the colony's stock, deeper than every
    /// sink that spends. A load reaches it only when there is nowhere else at
    /// all to put it, the upgrade buffer included, so the stock never outbids
    /// the flow, however close beside the spawn it stands.
    | Stock

/// How far apart two tiers stand on the [[priority]] ladder. Ten and not one,
/// so that a Task can be ordered against another **inside** its tier
/// (`priorityStep`) without ever reaching the tier above or below it.
let private tierRungs = 10

/// The whole tier order, shallowest first — the one place the ordering lives
/// (ADR 0010, ADR 0012, ADR 0023): the flow is fed, then the stock is drawn on,
/// then surplus is spent, then whatever is left sinks into the upgrade buffer,
/// and what even the buffer cannot hold is stocked. The stock's two roles sit
/// on either side of the surplus work the colony does between them. Exhaustive
/// over Tier on purpose — a tier this match forgets is a build error. The
/// downgrade deadline (ADR 0007) is the one thing above the sequence rather
/// than in it.
let private priorityOfTier =
    function
    // One tier beneath `deadlineRank`'s, which is itself one beneath the
    // shallowest tier of work: a fleeing creep outbids even a controller
    // about to downgrade (ADR 0033).
    | Safety -> -2 * tierRungs
    | Feeding -> 0
    | StockDraw -> tierRungs
    | Surplus -> 2 * tierRungs
    | UpgradeBuffer -> 3 * tierRungs
    | Stock -> 4 * tierRungs

/// One tier above the shallowest tier of work: where the downgrade
/// deadline puts Upgrade (ADR 0007). Not a tier of its own — "never let it
/// downgrade" is an ordering imposed on the sequence, not a tier of work.
let private deadlineRank = -tierRungs

/// The step a Task is moved by when it is ordered against another inside
/// one tier. One rung of ten, so it never crosses a tier and the tier
/// order is what it always was.
let private priorityStep = 1

/// Which of the five shapes a body is, as far as a [[capacity]] is concerned
/// (ADR 0052 decision 6, ADR 0006): part arithmetic, asked in the order the
/// existing gates ask it in, because Heavy and Standing overlap on the
/// [[anchor]]'s `6W/1C/1M` and every rule that reads both reads the heavy one
/// first (ADR 0016 before ADR 0046). `Fighter` is asked before all of them (ADR
/// 0056): a guard's `[T; A×3; M×5; H]` carries no Work at all, so the four
/// classes below would answer `Carrier` — the class of the bodies that shift
/// energy, and the one a Guard's capacity must not be sharing a number with.
/// Exported for the same reason `bodyFor` and `patternTable` are (ADR 0006): the
/// ladder is a body fact a test reads directly. The head of the ladder is read
/// in one [[capacity]] scope and one only — the Guard Task's `Fighter -> the
/// room's quota, every other class 0` (`Capacity.Fighters`, ADR 0056) — so a
/// body that stopped answering `Fighter` here would be a body no Guard admits.
let bodyClassOf (tuning: Tuning) atlas (creep: CreepInfo) : BodyClass =
    let count part =
        creep.Body |> Map.tryFind part |> Option.defaultValue 0

    if isGuardBody creep then Fighter
    elif Atlas.workHeavy atlas creep.Name then Heavy
    elif isStandingBody tuning creep then Standing
    elif count Work = 0 then Carrier
    else Light

/// Planner, second half: this tick's pool with each entry's [[priority]] and
/// [[capacity]] on it (ADR 0052 decision 6). `planTasks` says **what** is
/// pooled; this says where each entry ranks and how many bodies it admits, and
/// between them they are everything the Matcher knows about a Task — which is
/// why the Matcher can be, and now is, blind to Task kinds. Every exception the
/// colony has learned about ordering and crowding lands here and nowhere else:
/// the tier ladder, the [[downgrade deadline]]'s lift (ADR 0007), a source's
/// [[seat]]s and [[post]]s (ADR 0024, ADR 0051), a store's stock over the load
/// of the row that draws it, one holder per controller (ADR 0042, ADR 0047),
/// the outpost builders' budget — which since #266 rations the feeding tier
/// out there as well as the crowd on it — the [[pioneer]]s' ceiling and the
/// garrison's own tile.
let planPool (view: ColonyView) atlas (tasks: Task list) : PooledTask list =
    let bank = view.Bank.Capacity

    // The three loads a store is divided by, each the row's own cast at the
    // richest bank and never a candidate's own carry: a capacity is a fact about
    // the Task, so one store must not answer two numbers depending on which
    // creep asked — except by [[body class]], which is the one place
    // a store answers two numbers on purpose.
    let haulerLoad = carryCapacityOf (bodyFor haulerPattern bank)
    let workerLoad = carryCapacityOf (workerBodyFor bank)
    let standingLoad = carryCapacityOf (bodyFor upgraderPattern bank)

    let buffers = Atlas.controllerContainers atlas

    // The [[refill cluster]], off the one rule its three readers share
    // (`RefillCluster.ofRefillables`, ADR 0054): `planTasks` pooled the
    // spawn, this bounds it, and the Atlas lays its Work Area.
    let cluster = RefillCluster.ofRefillables view.Refillables

    // The [[ferry]]'s sinks, named by the one rule three readers share
    // (`ferryBuffers`): what a mother lends a bootstrapping child is
    // written down and bounded, so the Refill `planTasks` pooled for the
    // child's buffer carries that bound here.
    let ferrySinks = ferryBuffers view

    // One `Tuning.FerryLoads` budget per child room, spread over that room's
    // buffers in id order (user decision 2026-09-07): the hauler row hires per
    // child, so the pool admits per child — a second buffer in one room shares
    // the lend rather than doubling it, and with a budget smaller than the
    // buffer count the last ones take none.
    let ferryShare: Map<string, int> =
        ferrySinks
        |> Set.toList
        |> List.choose (fun id -> Atlas.targetRoom atlas id |> Option.map (fun room -> room, id))
        |> List.groupBy fst
        |> List.collect (fun (_, buffers) ->
            let ids = buffers |> List.map snd |> List.sort
            let n = List.length ids
            let budget = view.Tuning.FerryLoads

            ids
            |> List.mapi (fun i id -> id, budget / n + (if i < budget % n then 1 else 0)))
        |> Map.ofList

    let stored id =
        view.Spatial.Stores |> Map.tryFind id |> Option.defaultValue 0

    // **The queue the builders' budget rations** (#266): every site the pool
    // holds in a room this colony merely mines and that no other rule already
    // feeds. The second clause is what keeps the budget's head worth having.
    // A [[nursery]]'s site and a bootstrapping child's are feeding-tier
    // outright by their own reading (ADR 0047 decision 4) and capped by
    // nothing, and neither room is always `Borrowed`: while a human still
    // names the child's room in the mother's `Outposts` list
    // `Colony.bootstrapping` drops it from `BorrowedWork.Rooms` — its own
    // docstring spells that state out, and ADR 0047 decision 1 makes it the
    // normal one before the declaration is split — so `isOutpostSite` answers
    // true for its sites. Left in the queue they take places the lift buys
    // them nothing with, and the outpost's own container, ADR 0042's switch on
    // whether that room is in the economy at all, is pushed back into the
    // surplus where travel cost answers a Seam and sixty tiles against an
    // Upgrade underfoot. Asked through `isFeedingSite` with the budget's own
    // answer held empty, so the queue and the tier read one sentence and
    // cannot drift apart.
    let outpostSites =
        tasks
        |> List.choose (function
            | Build siteId when
                isOutpostSite view atlas siteId
                && not (isFeedingSite view atlas Set.empty siteId)
                ->
                Some siteId
            | _ -> None)

    // **The order the budget is spent in** (#266): the container sites first,
    // then the nearest to the crossing. The container is ADR 0042's switch on
    // whether the room is in the economy at all, so it is never queued behind a
    // road; every other site out there is a human's [[trunk]] (ADR 0042 as #244
    // amends it), and a trunk is worth building from the [[seam]] outward,
    // because the paved tiles nearest the crossing are the ones every haul from
    // that room walks over. The order asks the *kind* only for that one
    // question and the lift asks it not at all, which is #266's whole
    // narrowing undone if a kind list were written back in: at RCL0 the engine
    // allows a road and a container out there and nothing else, so a list
    // would name what a human is allowed to want built, and out here as in a
    // [[nursery]] that is not a judgement this colony makes. `Atlas.seamWalkTicks` is the same walk ADR 0042
    // anchors its container pick on — to the border and not across it — so the
    // two rules out here measure one thing. **One queue over every outpost and
    // not one apiece**, which is what keeps the budget the colony-wide number
    // #157 made it: two rooms' sites are ordered against each other on a walk
    // that leaves the home-side leg off both, so what the comparison says is
    // "nearer its own crossing" and not "nearer the spawn" — a tie-break inside
    // a budget, never a price (ADR 0002 does the pricing, from where the body
    // stands). Ties fall to the id, the way every
    // other tie in this colony falls, and a site whose walk cannot be priced
    // sorts last rather than out of the list: unpriceable is not nearest (ADR
    // 0004).
    let siteOrder siteId =
        let container =
            if Map.tryFind siteId view.Spatial.TargetKinds = Some(Site BuiltKind.Container) then
                0
            else
                1

        let walk =
            Atlas.positionOf atlas siteId
            |> Option.bind (fun tile ->
                Atlas.seamWalkTicks
                    atlas
                    tile.Room
                    (SpatialInfo.homeName view.Spatial)
                    (RoomPos.pos tile))
            |> Option.defaultValue System.Int32.MaxValue

        container, walk, siteId

    // **The budget rations the tier, not just the crowd on it** (#266, live:
    // W13S29's 45 hand-laid road sites at 0/300 while two workers refilled and
    // upgraded at home). Lifting *every* outpost site onto the feeding tier is
    // the failure #157's budget was written against — the whole worker row over
    // the Seam at once — and leaving them all in the surplus is the failure
    // above, travel cost answering 120 against an Upgrade underfoot that costs
    // nothing. So the same number does both: the first `Tuning.OutpostBuilders`
    // sites in the order above are lifted and the rest stay surplus, and as each
    // one is finished the next one out takes its place. The head is all that is
    // wanted, so the order is asked for only when the budget cannot cover the
    // list — the walk behind it is a flood over the outpost's whole grid, and a
    // colony whose outpost holds one site pays for none of it.
    let fedOutpostSites =
        if List.length outpostSites <= view.Tuning.OutpostBuilders then
            outpostSites
        else
            outpostSites
            |> List.sortBy siteOrder
            |> List.truncate view.Tuning.OutpostBuilders

    let fedSiteIds = Set.ofList fedOutpostSites

    // **A budget and not a per-site number** (#157): `planOutpostContainers`
    // places a site for *every* unserved outpost source, all on the same tick,
    // so a per-site two is a colony-wide six — the whole worker row, and
    // exactly what the cap exists to prevent. The budget is spread over the
    // sites it has lifted, floored at one apiece, and as each site completes
    // the divisor falls and the survivors get the bodies back. The divisor is
    // the **lifted** list and never the whole pool (#266): spread over the
    // pool, W13S29's 45 sites took one builder apiece and the colony-wide two
    // was no cap at all. Under #266 the divisor and the queue are one list, so
    // a room the budget does not ration no longer dilutes it either — the two
    // used to be separable and are not any more, a place in the list now being
    // the lift itself. **Two is a tunable, and this is the reason for that
    // number**: one is the smallest crowd that builds, and two is the smallest
    // that survives losing a body — a container is 5,000 progress against a
    // generalist's 50, so a lone holder that dies or is released by a Reach
    // (ADR 0033) leaves the switch open for a whole cast-and-walk cycle. Which
    // is why the spread stays a spread rather than becoming one apiece: with a
    // single switch open the pair is what #157 asked for, and the budget is
    // spent either way.
    let builderShare =
        match fedOutpostSites with
        | [] -> 0
        | sites -> view.Tuning.OutpostBuilders / List.length sites |> max 1


    // The tier a Task sits in. Refill, Withdraw and Build are the three Tasks
    // whose tier layers by target (ADR 0010, ADR 0023, ADR 0042). Two of the
    // three read the layer off the projection's kind and nothing else — the
    // stock is recognised for what it is, never for where it stands; the third,
    // Build, asks where as well. On Refill the Storage and the container are
    // each one projected kind and exclude each other by construction, while a
    // tower is read off the Refillables census, which can overlap either — so
    // the kind is asked first, deepest answer first, and the census only of
    // what the kind leaves.
    let tierOf task =
        match task with
        | Flee -> Safety
        // Beside Flee, on Flee's own argument and with no rung of its own (ADR
        // 0056): no other work matters while a creep is being killed. The tier
        // holds two Tasks and no ordering between them, because decision 3
        // makes them disjoint by [[body class]] — Flee inapplicable to a
        // Fighter, a Guard applicable to nothing else — and until that clause
        // lands (#256) a guard standing in the ring holds this Task on travel
        // cost alone, being already inside its Work Area and one walk short of
        // any safe tile.
        | Guard _ -> Safety
        | Harvest _ -> Feeding
        // **A decision made here, because nothing else made it.** ADR 0042 and
        // #116 both fix the reserver row's *casting* order and neither says a
        // word about its *matching* order, and `priorityOfTier` is exhaustive
        // on purpose, so a tier had to be chosen. Reserve joins the feeding
        // tier on the casting order's own argument: every other row spends the
        // colony's income, and this one decides whether that income is five a
        // tick or ten.
        | Reserve _ -> Feeding
        // Beside the Reserve it replaces, and for a stronger form of the same
        // argument (ADR 0047): a reservation decides whether one room's income
        // is five a tick or ten, and a claim decides whether there is going to
        // be a second colony at all.
        | Claim _ -> Feeding
        | Withdraw storeId ->
            if Map.tryFind storeId view.Spatial.TargetKinds = Some(Structure BuiltKind.Storage) then
                StockDraw
            else
                Feeding
        // A pile is flow and not stock: it is the haul cycle's energy lying
        // where it fell — an Anchor's overflow, a death drop — so it feeds the
        // colony on the tier the containers do, and which of the two an empty
        // carrier goes for is travel cost's call — for every pile but the two
        // `priorityOf` steps up a rung, the one lying on a drawable store and
        // the one holding half a [[hauler unit]]'s load (#216 R5, #242).
        | Pickup _ -> Feeding
        | Refill structureId ->
            let isTower =
                view.Refillables
                |> List.exists (fun r -> r.Id = structureId && r.Kind = BuiltKind.Tower)

            let kind = Map.tryFind structureId view.Spatial.TargetKinds

            if kind = Some(Structure BuiltKind.Storage) then
                Stock
            elif kind = Some(Structure BuiltKind.Container) then
                UpgradeBuffer
            elif isTower then
                Surplus
            else
                // The flow, and since ADR 0054 the [[refill cluster]] arrives
                // here through the same door rather than a case of its own:
                // the cluster is keyed on a spawn, and the ring it stands for
                // is spawn-feeding to the last extension.
                Feeding
        // The switch ADR 0042 hangs a whole room on, ranked where a switch
        // belongs (#157). A standing container is what admits an outpost into
        // the economy, so building it is not surplus work done with spare
        // energy — it decides whether a third of the colony's income exists at
        // all. Read on the surplus tier, only travel cost separated it from
        // Upgrade, and the home controller is a few tiles from a loaded worker
        // while the site is a Seam and fifty tiles away: every worker upgraded,
        // every tick. **And the same is true of the road beside it** (#266):
        // the trunk a human paves out there is what the [[hauler unit]]'s round
        // trip is priced on, so the argument that lifted the container reaches
        // as far as the budget can pay for — `fedSiteIds` is the head of that
        // queue and the tail stays in the surplus, where travel cost goes on
        // keeping the row at home. The same argument one question deeper for a
        // **nursery**'s sites (ADR 0047 decision 4): the spawn a human has
        // placed in a room this colony has claimed decides whether there is
        // going to be a second colony at all.
        | Build siteId when isFeedingSite view atlas fedSiteIds siteId -> Feeding
        // A bootstrapped child's Upgrade, in the mother's pool (#213): the tier
        // the pioneers were hired for. Left in the surplus beside the home
        // Upgrade, travel cost — a Seam and fifty tiles against five — kept
        // every one of them at home, and the addend was three more home
        // upgraders.
        | Upgrade controllerId when
            isBorrowedUpgrade view controllerId
            && not (sitesPendingBeside view atlas controllerId)
            ->
            Feeding
        | Build _
        | Repair _
        | Upgrade _ -> Surplus

    // The Task's place on the ladder: its tier, with the two orderings that are
    // not tiers laid over it. **The colony's own controller and no other.** The
    // deadline is read off `ColonyView.Controller`, which is this colony's
    // alone, and since ADR 0047 decision 4 the pool can hold a second Upgrade —
    // a bootstrapped child's. Lifting that one on the mother's timer would send
    // her whole loaded fleet across the Seam on the tick her *own* controller
    // was closest to downgrading. The child escalates its own controller in its
    // own tick. Where the pool's Feeding-tier stores stand, so a [[pickup]] can
    // be asked whether one of them is under its own pile. Read off the pool and
    // not off the projection's whole container census: a store the pool holds
    // no Withdraw for is not an alternative to anything.
    let drawableTiles =
        tasks
        |> List.choose (fun task ->
            match task with
            | Withdraw storeId when tierOf task = Feeding ->
                SpatialInfo.placementOf view.Spatial storeId
            | _ -> None)
        |> Set.ofList

    // **A [[pickup]] outbids the [[withdraw]] standing on its own tile** (live:
    // a hauler beside a full container ignored the pile on it). The two share
    // the feeding tier and the tile, so travel cost is equal and, when this was
    // written, the pool's order decided and the container stood first in it;
    // what separates them is decay — a pile loses `ceil(amount / 1000)` a tick
    // and a container loses nothing, so the energy that has to be taken first
    // is the energy that is going away. Since #242 the pool's order says that
    // much on its own — the piles stand before the Withdraws, so the same-tile
    // tie falls to the pile with no rung at all — and what this clause still
    // buys is the rest of the claim: a pile lying on a drawable store outranks
    // every *other* Feeding container in the colony, however much nearer that
    // one is. Written as the **Pickup** stepping up a rung and conditioned on
    // the store under it, so that it stays a claim about that one tile: a
    // [[priority]] is a scalar the whole tier is ordered by, so whichever of
    // the pair moves moves against every other Feeding Task in the colony. Stepping the *Withdraw* down was tried
    // first and is the bug it was meant to cure, inverted — a hundred-energy
    // overflow demoted a full container behind every other store at any
    // distance, and the engine drops that overflow only once the container is
    // full. One rung is inside the tier (`priorityStep`). The second lift, one
    // rung under the Pickup's: a **full source container**, whose income is
    // going away too, and which a hauler row sized to the mean round trip let
    // overflow for hours. Source containers alone: the buffer and the Storage
    // are sinks the haulers fill. The two rungs sit the other way round from
    // the first cut: with the pile above the full container the haulers chased
    // fifty-energy piles all day and never drew the 2,000 beside them, so every
    // pickup bred the next pile. **A [[pickup]] steps up where the pile is
    // worth a trip of its own** (#242, user: "worker 和 hauler 在不满的
    // container 和地上的能量中会优先选择前者") — the same rung as the tile
    // rule, taken by either clause, because the two are one sentence about one
    // pile. Where that clause is a claim about the store *under* the pile, this
    // one is the claim to make where there is no store under it at all: half a
    // [[hauler unit]]'s load or more lying on the ground is a whole trip, and a
    // trip made for it takes the copy that is going away rather than the one
    // that is not. Half a load and read off the row's own cast at this bank,
    // never the candidate's carry — the Planner is creep-blind (ADR 0013) —
    // which makes it the creep-blind mirror of `applicable`'s `worthTheTrip`
    // (#232): half a load is what makes a store worth a body's trip there and
    // what makes a pile worth one here. **The rung reaches the whole Feeding
    // tier** and not the container Withdraws alone, a [[priority]] being one
    // colony-wide scalar: a lifted pile outbids the spawn ring's [[refill]],
    // the [[harvest]], the [[reserve]] and the [[claim]], the Withdraw of a
    // tombstone or a ruin — a store that ends is rank 0 until it holds a
    // container's worth — and the feeding-tier [[build]]s of ADR 0042 and ADR
    // 0047, at any travel cost, a rank being settled before a price is asked.
    // The full source container's two rungs are the only thing above it. That
    // reach is the mechanism's price and not an oversight: the rung has to be
    // carried by the Pickup (above), and there is no rung that separates a
    // Task from one member of its tier and not from the rest. Every smaller
    // pile stays on rank 0 and is ordered by distance alone, which is the whole
    // reason the lift is not given to piles as a class: one hundred energy
    // forty tiles off is no reason to leave the 1,500 under a body's feet.
    // **Below a bank of 450 there is no smaller pile.** The row's cast carries
    // `100 * (bank / 150)`, so at RCL1's 300 half a load is a hundred —
    // `Tuning.PickupThreshold` itself — and every pile the pool holds takes the
    // rung; the distance-only rung exists only once the cast outgrows twice the
    // threshold, from RCL2 up. The line is the one #242 pinned, and whether a
    // bootstrapping colony's one body should walk off its rock for a
    // threshold-sized pile is that question's own issue and not this one's.
    // **A site outranks the controller inside the surplus tier** (#234, live:
    // 42 sites in one colony while every loaded worker upgraded). Build, Repair and Upgrade shared one rung, so travel
    // cost alone ordered them, and a worker that fills at the [[buffer]] is
    // already standing in the controller's Work Area: Upgrade costs it nothing
    // and never goes task-gone. What ADR 0042 and ADR 0047 lifted to Feeding
    // was the site that decides whether income *exists*; this is the ordinary
    // home site, which decides how fast it grows.
    let priorityOf task =
        let step =
            match task with
            | Pickup pileId ->
                let overADrawableStore =
                    SpatialInfo.placementOf view.Spatial pileId
                    |> Option.exists (fun tile -> Set.contains tile drawableTiles)

                let worthATripOfItsOwn = stored pileId * 2 >= haulerLoad

                if overADrawableStore || worthATripOfItsOwn then
                    -priorityStep
                else
                    0
            | Withdraw storeId when
                tierOf task = Feeding && stored storeId >= Engine.containerCapacity
                ->
                -2 * priorityStep
            | Build siteId when tierOf task = Surplus && isHomeSite view atlas siteId ->
                -priorityStep
            | _ -> 0

        match task with
        | Upgrade id when
            insideDowngradeDeadline view
            && view.Controller |> Option.exists (fun c -> c.Id = id)
            ->
            deadlineRank
        | _ -> priorityOfTier (tierOf task) + step

    // How many bodies the Task admits, and of which shapes. **Harvest is three
    // numbers over one source** (ADR 0024, ADR 0051): the Seat count every
    // harvester shares, the Post count only the garrisons compete for, and the
    // Seats beyond the Posts the light bodies are left, which sum back to the
    // Seat count exactly. A source with no Post derives neither of the last
    // two, and the two rooms mean different things by that: at home nothing
    // narrows a heavy body's area (ADR 0020's pre-container fallback), so the
    // Seat cap is the only one; in an outpost that area is *empty*, so the
    // reachability gate rejects the pair for every heavy body. An unplaced
    // source derives no cap (ADR 0004). Beside the numbers, the **tiles**
    // (#205, widened by #269): every Post of the rock is held by the body
    // *standing* on it whatever Task it holds this tick, over the same census
    // the Post number is counted from. A cap that counted Harvest's own holders
    // alone read a Post as free on every tick its garrison held something else
    // — a build tick on a Post whose container is still a site, an Upgrade
    // through the empty window on a bare [[dual seat]] — and dispatched a
    // second heavy body across the room onto a tile that was never vacant
    // (#258's accepted window). **A Withdraw is capped by its store's
    // stock** (#161), **and a Pickup by its pile's**: `ceil(stored / one
    // drawer's load)`. Nothing else in the pipeline says it — the matching key
    // puts cost ahead of crowding (ADR 0002), so a container holding 400 draws
    // five haulers while a full one across the room stands unvisited. **The
    // [[buffer]] divides twice** (#196). ADR 0019 shuts every body with no Work
    // part out of the controller's container, so its drawers are the two Work
    // rows: the generalists, carrying 450, and the [[upgrader]]s, carrying
    // fifty. Divided by the generalist's load alone a 900-energy buffer admits
    // two drawers *in total*, so the row hired to stand there took at most two
    // seats; divided by the upgrader's alone it admits eighteen, which re-opens
    // the pile-on the cap is here for. So the store answers both numbers and
    // each class is counted against its own, with deliberately no `Total`.
    let isBorrowedSite siteId =
        isBootstrapRoom view (Atlas.targetRoom atlas siteId |> Option.defaultValue "")

    let capacityOf task =
        match task with
        | Harvest sourceId ->
            let seats = Atlas.seats atlas sourceId
            // One binding, read twice: the number and the tiles are the same
            // census since #269, and a second call is a second thing to narrow
            // later.
            let postTiles = Atlas.postsOf atlas sourceId
            let posts = Set.count postTiles

            { Capacity.unbounded with
                Total = seats
                Garrisons = (if posts = 0 then None else Some posts)
                Commuters =
                    seats
                    |> Option.filter (fun _ -> posts > 0)
                    |> Option.map (fun n -> max 0 (n - posts))
                Garrison = postTiles
            }
        // The [[guard]]s that room wants and nobody else at all (ADR 0056):
        // `guardsWanted` is the row's own arithmetic, read here a second time
        // rather than restated, so the number the cascade hires against and the
        // number the Matcher counts holders against are one number. One or two
        // — a second body only where a guard of ours is already standing in
        // that room and the raid out-heals it — and the class share is what
        // keeps the [[hauler unit]]s and the workers out of a Task whose whole
        // Work Area is a Reach.
        | Guard room -> Capacity.fighters (guardsWanted view atlas room)
        // One holder per controller (ADR 0042, ADR 0047). A reservation is a
        // single capped number one body's CLAIM parts are sized to hold, so a
        // second body there buys nothing while the other outpost stays at five
        // a tick; for the Claim beside it the second body buys even less, a
        // room being claimed by one touch of one CLAIM part.
        | Reserve _
        | Claim _ -> Capacity.total 1
        | Withdraw storeId ->
            let stock = stored storeId

            if Set.contains storeId buffers then
                { Capacity.unbounded with
                    Standing = Some(ceilDiv stock standingLoad)
                    Generalists = Some(ceilDiv stock workerLoad)
                }
            else
                Capacity.total (ceilDiv stock haulerLoad)
        | Pickup pileId -> Capacity.total (ceilDiv (stored pileId) haulerLoad)
        // **The [[refill cluster]] is bounded by what it can still hold** (ADR
        // 0054, amending ADR 0029 for this one Task): as many bodies as the
        // ring's free energy divides into loads, so a second one joins only
        // while what stands empty exceeds what the first is carrying. The bound
        // is what makes one Task out of ten safe: ten Tasks of capacity one
        // apiece spread the crowd by accident, at the cost of a `task-gone`
        // release per creep per tick or two, and unbounded, one Task would
        // gather every loaded body onto one ring and leave the [[buffer]] and
        // the [[storage]] unvisited. Divided by the [[hauler unit]]'s load and
        // never a candidate's own carry, and a `Total` with no per-class share.
        | Refill spawnId when cluster |> Option.exists (fun c -> c.Spawn = spawnId) ->
            let free = cluster |> Option.map RefillCluster.free |> Option.defaultValue 0

            Capacity.total (ceilDiv free haulerLoad)
        // The lend, bounded (ADR 0052 decision 7): `Tuning.FerryLoads` bodies
        // at the child's buffer and no more, the same number the hauler row was
        // raised by, so a human retuning the lend retunes the hire with it. A
        // `Total` and not the hauler class's share alone: what makes this a lend
        // rather than a second economy is that it is *bounded*, and a cap on the
        // carriers would leave every generalist free to cross for the same
        // store. The tier puts this Refill below every sink at home.
        | Refill structureId when Set.contains structureId ferrySinks ->
            Capacity.total (Map.tryFind structureId ferryShare |> Option.defaultValue 0)
        // A borrowed Upgrade takes the bodies hired for it and no more (#213):
        // `Tuning.PioneerCount`, the same constant the worker row is raised by,
        // so a human retuning the hire retunes the lift with it.
        | Upgrade controllerId when isBorrowedUpgrade view controllerId ->
            Capacity.total view.Tuning.PioneerCount
        | Build siteId ->
            // The body standing on the site is outside the builders' budget
            // (#205): every word of that number's argument is about a commute,
            // and this body costs the home room neither a walk nor a surplus
            // tick.
            let exempt = Atlas.postSiteTile atlas siteId |> Option.toList |> Set.ofList

            // A bootstrapped child's site in the mother's pool, per site: the
            // same bodies that were hired for the room, on the site that ends
            // its window sooner than its controller does. The child's own room
            // reads no cap here — its own sites are its own workers' to crowd.
            //
            // And the crowd the budget rations is the list it lifted, with
            // nothing left to subtract from it (#266): the rooms whose sites
            // the budget does not reach — a [[nursery]]'s, a bootstrapping
            // child's, a borrowed room's — are the rooms whose sites never
            // entered the queue, so what the cap covers and what the tier
            // lifts are one list read twice.
            let total =
                if isBorrowedSite siteId then Some view.Tuning.PioneerCount
                elif Set.contains siteId fedSiteIds then Some builderShare
                else None

            { Capacity.unbounded with
                Total = total
                Exempt = exempt
            }
        | _ -> Capacity.unbounded

    tasks
    |> List.map (fun task ->
        {
            Task = task
            Priority = priorityOf task
            Capacity = capacityOf task
            // Work in a room another colony of ours runs (ADR 0047 decision 4).
            // One gate reads it, and only on the Upgrade: a [[standing body]]
            // holds no commuting work (ADR 0046), and a Seam crossing is the
            // longest commute the colony has, so the lift that sends the
            // pioneers must not send the home upgraders after them.
            Borrowed =
                match task with
                | Upgrade controllerId -> isBorrowedUpgrade view controllerId
                | Build siteId -> isBorrowedSite siteId
                | _ -> false
        })

/// Whether a creep can usefully work this Task right now. The body must
/// physically be able to do it — Work-part tasks need a Work part, energy
/// delivery needs a Carry part — and the energy state must call for it: a body
/// past half full is not worth *sending* for more, by digging or by drawing
/// (#235), and an empty creep has nothing to deliver. Not all of it is a
/// judgement about the body: the gates below read the target's kind, its
/// geometry and what is standing in it. Gates read part arithmetic, never names
/// or roles (ADR 0006). One geometric widening (ADR
/// 0012), body-aware since ADR 0024: a full Work-heavy creep standing on a
/// built source container keeps Harvest, the engine dropping the overflow into
/// the container underfoot. A light body gets no such reprieve, or it would
/// hold the Post for the rest of its life. A second gate is comparative (ADR
/// 0016): a body with more Work than Move never Withdraws, so its only
/// feeding-tier candidate is Harvest. A third reads the target's kind beside
/// the body (ADR 0019): only a creep with a Work part draws from the
/// controller's upgrade buffer. A fourth reads the body alone and covers
/// **every** Build (#157, widened by #234): a Build is inapplicable to a
/// Work-heavy body. A fifth covers Build, Repair and Refill for a **standing
/// body** (ADR 0046), all three being deliveries and a delivery by a body
/// holding fifty energy against eleven Work being a commute. A sixth reads the
/// geometry beside the body (ADR 0048): Upgrade is applicable to a Work-heavy
/// body only where it may already act on it. A seventh is three clauses over
/// one Task (#235), all of them about the walk a light body makes to a rock
/// and none of them reaching the Work-heavy body ADR 0020 has pinned to a Post:
/// it must be half empty *or* already standing where it may dig, it must not be
/// a standing body — which closes #206 for the one Task that ticket spared —
/// and the rock's rate must still outrun what the bodies garrisoning it take.
let private applicable
    (view: ColonyView)
    (threats: Threats)
    atlas
    (creep: CreepInfo)
    (pooled: PooledTask)
    =
    let task = pooled.Task

    let has part =
        creep.Body |> Map.tryFind part |> Option.exists (fun n -> n > 0)

    // An intake — a Withdraw, a Pickup or, since #235, a Harvest — is for a body
    // with room to carry it: at least half its store free (live: a hauler holding
    // 1,150 of 1,200 walked forty tiles to pick fifty off a pile while the spawn
    // stood at eighteen energy). A body past half full is a delivery, and its
    // intake waits until it has delivered. A standing body's one Carry is a
    // trip's worth, so for it this is "empty". Harvest joined the other two
    // late and for a light body only (#235), and there it prices the **walk**
    // alone: the other two finish in the tick the body arrives, where a dig
    // runs for dozens, so only Harvest can be half way through when this is
    // read — and a garrison's whole working life is spent past half full.
    let halfEmpty = creep.FreeCapacity * 2 >= creep.Energy + creep.FreeCapacity

    match task with
    // ADR 0024's full-store reprieve, and beside it the clause that keeps ADR
    // 0048's own Consequence reachable ("stands where it is until it can dig
    // again"). A Work-heavy body never empties — ADR 0016 shut Withdraw and
    // Transfer, ADR 0046 shut Refill, Build and Repair, ADR 0048 shuts the walk
    // to the controller — so a store gate that reads fullness as "done here"
    // reads a garrison's ordinary condition as a reason to take its work away.
    // Which it did: a hauler drawing the container swaps the Anchor onto the
    // Seat beside it, and a full body one step off its Post had no Task at all.
    // So the gate is widened by a question and not by a tile: ADR 0024 asks
    // whether a body may keep *digging* where it stands, and a body still
    // walking is not digging.
    //
    // **And the walk has to end somewhere it can stand** (#258). ADR 0048
    // offered it wherever the source has a Post, on the argument that a Post is
    // a tile the arriving body has something to do on — true of the tile and
    // not of the tick, because the Post cap that would otherwise refuse the
    // pair is counted at arrival (ADR 0026) and a long enough walk discounts
    // any incumbent. So a full Anchor released off its own rock read every
    // garrisoned Post in the colony as somewhere to go, and the one live case
    // walked a border home to stand beside another Anchor's Post while the
    // vacancy it left cast a replacement (user, 2026-09-08). The walk is
    // offered while a Post of that source has no garrison standing on it
    // *now* — over the same census this clause's own Post test reads, so the
    // rock a full body is walking to raise the site of is one it can still
    // have (#205). The narrowing is this disjunct's alone and so is a **full**
    // body's alone: a heavy body with a free store is offered the walk by the
    // clause above, which is what lets a fresh Anchor be sent to the Post its
    // expiring incumbent is still standing on (ADR 0026).
    | Harvest sourceId ->
        has Work
        && (creep.FreeCapacity > 0
            || garrisons atlas creep sourceId
            || (Atlas.workHeavy atlas creep.Name
                && not (Set.isEmpty (Atlas.postsOf atlas sourceId))
                && not (Atlas.mayAct atlas creep.Name task (areaFor threats atlas creep.Name task))
                && hasUnmannedPost view atlas creep sourceId))
        // **Three clauses a light body answers and a garrison does not**
        // (#235), drawn at ADR 0016's ratio, which is where every other line
        // that separates the two bodies is drawn. Every one of the three is
        // about the walk digging costs a body that does not live at the rock,
        // so none of them can reach the body ADR 0020 has already pinned to a
        // Post it is standing on: a Work-heavy body's Harvest goes on being
        // decided by the gate above and by ADR 0024's two reprieves alone.
        //
        // **Half empty, like the other two intakes — while the walk is still
        // ahead of it.** The mirror the Withdraw and the Pickup have carried
        // since 75edfee never reached Harvest, so any room at all was room
        // enough: live at t199,88x a `9W/9C/9M` worker with nine free of four
        // hundred and fifty crossed a Seam, dug once, released full, and
        // crossed back — and Harvest being the Feeding tier (ADR 0023) it
        // outranked every Surplus Task the body could have done where it
        // stood. But Harvest is the one intake that does not finish in a tick,
        // and this gate is the *release* gate as well as the dispatch one, so
        // the store mirror alone evicted a body off the Seat it was digging on
        // the tick it crossed half full: half a load carried home for the whole
        // of the walk it had already paid. So it is spelled the way ADR 0048
        // spells its own widening in this same branch — the Emitter's `mayAct`,
        // false while the walk is ahead of the body and true the tick it
        // arrives. A body still walking answers the mirror; a body standing
        // where it may dig has no walk left to price and fills to the brim.
        //
        // **A [[standing body]] does not walk to a rock either**, which closes
        // #206 for the one Task it left open. That ticket shut the Pickup and
        // every non-buffer Withdraw for a body carrying fewer than one Carry per
        // four Work, and spared Harvest on the reasoning that travel cost would
        // keep the upgrader row beside its buffer and that the anchor row is a
        // standing body too. Travel cost did not: an empty buffer leaves the row
        // nothing else applicable at all, and W13S28's `11W/1C/11M` upgraders
        // walked to the sources on the ticks it ran dry, fifty energy a trip
        // against eleven Work. The anchor row's half of that reasoning is what
        // the heavy exemption above already answers.
        //
        // **And something spare in the rock to dig.** Those last two carry no
        // arrival exemption on purpose: what they refuse is a body that should
        // not be at the rock at all, and the release is the point of them —
        // #235's case (b) is a light body squatting the Seat the outpost's own
        // Anchor needs the tick it stands up.
        && (Atlas.workHeavy atlas creep.Name
            || ((halfEmpty
                 || Atlas.mayAct atlas creep.Name task (areaFor threats atlas creep.Name task))
                && not (isStandingBody view.Tuning creep)
                && hasSpareRate view atlas sourceId))
    // The body half of this gate — a Carry part and ADR 0016's comparative
    // clause — is read a second time out of line by `canRefill`, the supply
    // floor's arming condition (ADR 0050): a clause narrowing what a body may
    // draw with belongs in front of both readers, or a colony whose only carrier
    // this gate has just shut out still reads as able to refill.
    | Withdraw storeId ->
        let buffer = Set.contains storeId (Atlas.controllerContainers atlas)

        // **A Withdraw must be worth this body's trip** (#232): the store has
        // to hold at least half of what the body came with room for. It is the
        // mirror of `halfEmpty` above and the second half of the same sentence
        // — half empty is what makes a body worth sending, half a load is what
        // makes a store worth sending it to — and it is a fact about the
        // *pair*, so it belongs here and not in `capacityOf`, whose number is
        // the Task's alone. What it cures is the other end of the haul cycle: a
        // [[capacity]] of `ceil(stock / one load)` admits a drawer to any store
        // holding one energy, and the half-full rule above then keeps the
        // arriving body there until it has drained the Anchor's trickle. Live,
        // a 24C/12M hauler stood forty-two ticks on a container holding ~200 to
        // carry six hundred, while the Storage held 263,803 and the spawn stood
        // at twenty-eight. The tier gap (ADR 0023) cannot break that by itself:
        // a container's Withdraw outranks the stock's while it is applicable.
        // Read off the body's **free** capacity and not its total, so it is the
        // same sentence for a part-loaded body as for an empty one, and judged
        // every tick against a pool rebuilt from scratch, so it gates
        // persistence as well as entry. Not carried to Pickup, which keeps the
        // half-empty clause alone: a pile decays and a container does not.
        // Three stores it does not price. **A store that ends** — a tombstone
        // or a ruin — is the Pickup's exemption word for word. **The stock**
        // (ADR 0023): what this line buys is the fall to the tier below, and
        // there is none below the Storage's own Withdraw. **The [[standing
        // body]] at the buffer under its own feet**: the same exception #205
        // makes of a site on a creep's own Post — this clause prices a trip and
        // that row makes none.
        let stock = view.Spatial.Stores |> Map.tryFind storeId |> Option.defaultValue 0

        let worthTheTrip =
            stock * 2 >= creep.FreeCapacity
            || (Map.tryFind storeId view.Spatial.TargetKinds |> Option.exists isTransient)
            || pooled.Priority >= priorityOfTier StockDraw
            || (buffer && isStandingBody view.Tuning creep)

        has Carry
        && halfEmpty
        && worthTheTrip
        && not (Atlas.workHeavy atlas creep.Name)
        && (has Work || not buffer)
        // A standing body fetches from the buffer at its feet and from nowhere
        // else (#206, ADR 0046): its one Carry is one trip's worth, and a trip
        // to the Storage — or across a Seam to a pile — is the commute the row
        // was shaped to never make.
        && (buffer || not (isStandingBody view.Tuning creep))
    // The Withdraw gate without its one target-shaped clause: a Carry part,
    // room to put the energy, and ADR 0016's comparative gate — a Work-heavy
    // body's intake is digging, and picking a pile up off the ground is no more
    // its work than drawing a container is. The buffer clause has no
    // counterpart here: ADR 0019 shuts a Work-less body out of the
    // *controller's* container, and a pile is nobody's buffer.
    | Pickup _ ->
        has Carry
        && halfEmpty
        && not (Atlas.workHeavy atlas creep.Name)
        && not (isStandingBody view.Tuning creep)
    // Its two body clauses are read a second time out of line by
    // `canRefill`, beside Withdraw's (ADR 0050) — the Energy clause is not,
    // being a state and not a fact about the body.
    | Refill _ -> has Carry && creep.Energy > 0 && not (isStandingBody view.Tuning creep)
    // The body gate on Build (#157, widened to every Build by #234), here for
    // the same reason ADR 0016's Withdraw gate is: the ladder lifts a site over
    // the Task that was pinning the body, and a rank the whole colony shares is
    // exactly what travel cost can no longer thin. A full Anchor whose Post has
    // no standing container under it loses Harvest, and was then outranked off
    // its own controller and walked fifty tiles at four to seven ticks a step
    // to spend one Carry into a 5,000-progress site. A heavy body's cross-room
    // work is a Post and never a delivery (ADR 0020), so the switch is light
    // bodies' work, and what it costs the colony is one body's walk and never a
    // garrison's Post. The gate followed the *tier* and now follows the body,
    // #234 having lifted the ordinary **home** site a rung over the Upgrade
    // that was the whole of what travel cost pinned the Anchor with. And one
    // exception over both gates, which is #205's whole change: a container site
    // **under the body's own feet, on its own Post**
    // (`Atlas.standsOnPostSite`). Both prohibitions are about a walk, and
    // neither reaches a site the body is standing on.
    | Build siteId ->
        has Work
        && creep.Energy > 0
        && (Atlas.standsOnPostSite atlas creep.Name siteId
            || (not (isStandingBody view.Tuning creep) && not (Atlas.workHeavy atlas creep.Name)))
    // Repair leaves Upgrade's arm with ADR 0046's gate (a delivery, and a
    // standing body's Carry is one trip's worth), and the two stay
    // otherwise identical: a Work part and something to spend.
    | Repair _ -> has Work && creep.Energy > 0 && not (isStandingBody view.Tuning creep)
    // The one Task the whole row exists for, and so the one place the standing
    // gate must not appear (ADR 0046): a standing body spends its Work into the
    // controller from where it stands. And the sixth gate, which is that
    // sentence's other half (ADR 0048): a Work-heavy body spends its Work into
    // the controller only from where it already stands, because it is the walk
    // that is the loss. ADR 0016 accepted one commute — "a full Anchor off-post
    // matching Upgrade once empties it and converges" — but there is no *once*:
    // every release puts the same body back at this gate. The Dual Seat and the
    // buffer-side row are exactly the shapes this leaves standing (ADR 0020,
    // ADR 0046), both already inside the Work Area.
    | Upgrade _ ->
        has Work
        && creep.Energy > 0
        && (not (Atlas.workHeavy atlas creep.Name)
            || Atlas.mayAct atlas creep.Name task (areaFor threats atlas creep.Name task))
        // A standing body holds no commuting body (ADR 0046) and the borrowed
        // Upgrade is a commute across the Seam (#213): the lift that sends the
        // pioneers must not send the home upgraders after them. Their own
        // controller stays the one Task the row exists for, ungated.
        && not (pooled.Borrowed && isStandingBody view.Tuning creep)
    // Part arithmetic and nothing else (ADR 0006): a reservation is pushed up
    // by CLAIM parts, so a body without one can no more reserve than a
    // Work-less one can dig, and a body with one asks for no energy state.
    | Reserve _ -> has BodyPart.Claim
    // The same part arithmetic, for the same reason (ADR 0047): the engine's
    // `claimController` is a CLAIM part's act, and a claimer carries nothing.
    | Claim _ -> has BodyPart.Claim
    // The same part arithmetic once more (ADR 0006, ADR 0056): an ATTACK part
    // is what makes a body a Fighter and the only thing that kills an invader,
    // and a body carrying one asks for no energy state — it spends nothing. No
    // room clause beside it: the [[work area]] is that room's ring and the
    // walk to it is what travel cost prices, exactly as an outpost's Harvest is
    // offered to a body standing at home. Spelled through the row predicate the
    // [[body class]] ladder itself reads (`isGuardBody`), so "a Fighter body"
    // is one sentence here and in `bodyClassOf` and the [[capacity]] beside it
    // cannot come to disagree with the gate.
    | Guard _ -> isGuardBody creep
    // Flee asks for no part and no energy state, only for a creep that is being
    // shot at and can run (ADR 0033). A Work-heavy body is exempt: at four to
    // seven ticks a step an Anchor leaving its Post neither escapes nor digs,
    // and the answer for the Post is a rampart (ADR 0034) — which is also why
    // the tile under one is in no Reach.
    | Flee -> not (Atlas.workHeavy atlas creep.Name) && standsInReach threats atlas creep.Name

/// The action Intent a Task asks of a creep, or None for a Task with no
/// action: Flee is movement and nothing else (ADR 0033), and the Emitter
/// issues it none.
let private intentFor atlas (creep: CreepInfo) task =
    match task with
    | Harvest sourceId -> Some(HarvestSource(creep.Name, sourceId))
    // The same Intent for a tombstone or a ruin as for a container (#167):
    // the engine's `withdraw` is one method over every store, so the
    // Intent's name is the only thing that says "structure" and the
    // Executor hands it whatever `getObjectById` answers with.
    | Withdraw storeId -> Some(WithdrawEnergyFromStructure(creep.Name, storeId))
    // The reflex's own Intent, issued for a creep that walked: one act, one
    // vocabulary, whether the energy was underfoot already or was the reason the
    // creep came. Which is why an arriving picker spells it twice and `decide`
    // keeps one — this Task owns its own act, and the reflex is what gives way.
    | Pickup pileId -> Some(PickupEnergy(creep.Name, pileId))
    // One Task, one act, and — since ADR 0054 — sometimes many structures: a
    // [[refill cluster]]'s Refill names a place, and *which* member of it the
    // energy lands in is settled here, at arrival, off the tile the body
    // actually stands on (`Atlas.refillTarget`). Every other Refill resolves
    // through the same call, so the Emitter has one line and not a branch.
    | Refill structureId ->
        Atlas.refillTarget atlas creep.Name structureId
        |> Option.map (fun target -> TransferEnergyToStructure(creep.Name, target))
    | Build siteId -> Some(BuildSite(creep.Name, siteId))
    | Repair structureId -> Some(RepairStructure(creep.Name, structureId))
    | Upgrade controllerId -> Some(UpgradeController(creep.Name, controllerId))
    | Reserve controllerId -> Some(ReserveController(creep.Name, controllerId))
    | Claim controllerId -> Some(ClaimController(creep.Name, controllerId))
    | Flee -> None
    // The Guard's acts are two, and neither is this function's: it names one
    // Intent for a target the projection places, and a Guard's target is a
    // hostile creep chosen at arrival off the colony's own facts
    // (`guardIntents`).
    | Guard _ -> None

/// Chat-bubble glyph of a Task: the whole colony's current matching is
/// legible in the viewer at one glyph per creep.
let private glyphFor =
    function
    | Harvest _ -> "⛏"
    | Withdraw _ -> "📥"
    | Pickup _ -> "🧲"
    | Refill _ -> "🔋"
    | Build _ -> "🔨"
    | Repair _ -> "🔧"
    | Upgrade _ -> "⚡"
    | Reserve _ -> "🚩"
    | Claim _ -> "🏴"
    | Flee -> "🏃"
    | Guard _ -> "⚔️"

/// The [[threat]] a [[guard]] swings at, out of the ones standing in the room
/// its Task names and passing the caller's own gate (ADR 0056): **the one
/// nearest a [[post]] of that room**, ties by id. That is "between the invader
/// and the [[anchor]]" said in this colony's vocabulary and the only place the
/// Anchor enters the geometry — the engine's own `findAttack.js` chases the
/// closest hostile creep by path, so a guard standing on the ring of the invader
/// nearest the Post *is* between it and everything behind it, and a tile set of
/// ours would be a second, weaker spelling of a fact the engine already
/// guarantees. With no Post standing, the nearest Threat to the guard — a room
/// with no garrison in it has nothing to stand in front of, so the body fights
/// what is closest. A Threat and never "a hostile": the healer beside an invader
/// is what the row's count rule prices, not what its ATTACK parts are spent on.
/// None where the room holds none, or where the projection places the guard
/// nowhere (ADR 0004).
///
/// **The gate is the caller's and stands ahead of the choice**, which is where
/// ADR 0056 decision 2's one sentence reads it behind: a Threat the swing cannot
/// reach is not a Threat this answer is about. Ordered the other way round, a
/// guard standing on the ring of the *second* invader of a two-creep raid is
/// handed the one nearest the Post, finds it three tiles off, and swings at
/// nothing while the invader beside it deals 40 a tick — 90 damage a tick
/// forgone for a pick that moves nothing else, the mover aiming at the whole
/// ring either way. Where the nearest-Post Threat is in reach — the 90% raid of
/// one invader, and every case the ADR argues about — the two readings answer
/// alike, which is why this narrows decision 2 rather than overturning it.
let private guardTarget
    (view: ColonyView)
    atlas
    (creep: CreepInfo)
    (room: string)
    (among: HostileInfo -> bool)
    =
    let posts = Atlas.postsIn atlas room

    // The guard's own tile, which is only read where the room has no Post; an
    // unplaced body prices every Threat alike and the id order answers.
    let here =
        Atlas.creepTile atlas creep.Name |> Option.filter (fun t -> t.Room = room)

    let distance (hostile: HostileInfo) =
        let from = RoomPos.pos hostile.Pos

        if Set.isEmpty posts then
            here
            |> Option.map (fun tile -> range from (RoomPos.pos tile))
            |> Option.defaultValue 0
        else
            posts |> Set.toList |> List.map (range from) |> List.min

    view.Hostiles
    |> List.filter (fun h -> h.Pos.Room = room && (weaponRange h |> Option.isSome) && among h)
    |> List.sortBy (fun h -> distance h, h.Id)
    |> List.tryHead

/// The two acts of a Guard (ADR 0056), which are the Emitter's alone because
/// neither is a Task target's: `AttackCreep` at the chosen Threat, chosen out of
/// the ones **standing within range 1** since 30 a part is paid there and nothing
/// is paid at range 2; and `HealCreep` on the guard itself **every tick**, a
/// separate Intent kind so that the two do not compete for one act. No movement
/// of its own — the mover walks the body into the ring like any other Task's
/// Work Area, which is ADR 0033's whole argument for making a fight a Task
/// instead of a reflex. The range is the *gate on the candidates* and not a
/// filter on the pick, for the reason `guardTarget` above writes down.
let private guardIntents (view: ColonyView) atlas (creep: CreepInfo) (room: string) : Intent list =
    let inSwing (hostile: HostileInfo) =
        Atlas.creepTile atlas creep.Name
        |> Option.bind (fun tile -> RoomPos.range tile hostile.Pos)
        |> Option.exists (fun r -> r <= Engine.meleeRange)

    let swing =
        guardTarget view atlas creep room inSwing
        |> Option.map (fun hostile -> AttackCreep(creep.Name, hostile.Id))
        |> Option.toList

    swing @ [ HealCreep(creep.Name, creep.Name) ]

/// Action Intent for one assigned creep: emitted when the Atlas judges the
/// action reachable from the tick-start position, and — for Harvest alone —
/// only while the source holds energy (ADR 0025). Anticipatory dispatch and the
/// occupancy surcharge (ADR 0008) both price a walk high enough to land a creep
/// a tick or two early, so the gate is what keeps the engine's
/// ERR_NOT_ENOUGH_RESOURCES spam structurally impossible. The Guard is the one
/// Task judged outside that gate: its acts reach a creep the projection places
/// nothing for, so `Atlas.mayAct` — which asks where a Task's *target* stands —
/// answers false for it on every tick, and the range it is really gated on is
/// the swing `guardIntents` measures itself (ADR 0056).
let private actionIntents
    (view: ColonyView)
    atlas
    (threats: Threats)
    (creep: CreepInfo)
    (task: Task)
    : Intent list =
    let drained =
        match task with
        | Harvest sourceId -> ticksToRestock view sourceId > 0
        | Withdraw _
        | Pickup _
        | Refill _
        | Build _
        | Repair _
        | Upgrade _
        | Reserve _
        | Claim _
        | Guard _
        | Flee -> false

    match task with
    | Guard room -> guardIntents view atlas creep room
    | _ ->
        if
            Atlas.mayAct atlas creep.Name task (areaFor threats atlas creep.Name task)
            && not drained
        then
            intentFor atlas creep task |> Option.toList
        else
            []

/// Emitter: each assigned creep's action Intent, then every assigned
/// creep's chat bubble, both in view creep order. Judges actions from
/// tick-start geometry — it must run against the same Atlas the Matcher
/// used, never against resolved positions.
let emit (view: ColonyView) atlas (threats: Threats) (assigned: Map<string, Task>) : Intent list =
    let actions =
        view.Creeps
        |> List.collect (fun creep ->
            match Map.tryFind creep.Name assigned with
            | Some task -> actionIntents view atlas threats creep task
            | None -> [])

    // Every assigned creep says its Task's glyph every tick; unassigned
    // creeps say nothing.
    let says =
        view.Creeps
        |> List.choose (fun creep ->
            Map.tryFind creep.Name assigned
            |> Option.map (fun task -> SayCreep(creep.Name, glyphFor task)))

    actions @ says

/// Creeps with no Task rank below every task in arbitration.
let private idleRank = System.Int32.MaxValue

/// Register one creep's Move Intent — every creep gets one (ADR 0001). A creep
/// travelling toward its Work Area wants exactly its next path step; one
/// already inside is force-registered "stay put, displaceable within the Work
/// Area"; one with no Task, or no way to reach its area, is parked: stay put,
/// displaceable to any adjacent walkable tile. The displacement tiles are its
/// own room's, the Resolver arbitrating each projected room by itself (ADR
/// 0041's Consequences). **Every candidate list is the head it asked for and a
/// tail after it** (#219). A traveller heads its step and tails the ground that
/// lies beside both it and that step — a sidestep around the tile it wanted. A
/// creep inside its area heads its own tile, then the area's neighbours, then
/// the ground outside it. Head and tail are read differently by the arbitration
/// below — the head is what the score pays a creep's whole weight for, a tail
/// is a detour worth the least positive thing — so the order is the whole of
/// the preference. Without a tail a creep whose one candidate is held stands
/// still for as long as that body does; it is a sidestep and never a retreat,
/// which is the half ADR 0008 keeps. One tile is never a candidate: the creep's
/// own, when that tile is a Seam (#142). The ring is no room's ground (ADR
/// 0036) and a creep that ends its tick on it is moved out of the room again,
/// so "stay put" there is a bounce across the border every other tick. The Task
/// goes to `stepToward` beside the area, and that is what gives a creep matched
/// across a border somewhere to walk (#142): its Work Area is empty here by
/// construction (ADR 0041), so without the Task it would park on a Task it was
/// priced for and never move. A [[guard]]'s area is the one that is *not* empty
/// across that border — it is the raided room's ring, filed under that room (ADR
/// 0056) — and it crosses through the same seam all the same, the Task naming
/// the room the step is aimed at.
///
/// **A body with no Task parks off the [[working ground]]** (#241, widening ADR
/// 0022 from the Layout to the mover). The Seats and the Upgrade Work Area are
/// the tiles the colony works *from*, and an idle body standing on one costs
/// exactly what a clustered structure there would: the tile. W13S28's north
/// pocket is eight tiles of Upgrade Work Area with the [[buffer]] container
/// inside it, so its five refill tiles are working ground to the last one; two
/// idle upgraders parked on them for 190 ticks and the hauler carrying the
/// energy that would have un-idled them never got in — the buffer stayed empty
/// because the bodies waiting on it were standing where its feed had to stand.
/// So an idle body's head is the first step off that ground, and its own tile
/// falls into the tail behind it: where there is nowhere off the ground to go,
/// it parks exactly as before. Two things ride on that head. The goal set is
/// taken less this tick's Reach, as every tasked candidate already is (ADR
/// 0033) — a [[work-heavy body]] has no Flee, so a step off a safe pocket into
/// an attacker is one nothing walks back — and the tail is ordered off the
/// ground first, because a body shoved aside off the ground and re-housed onto
/// it is a body that steps off again next tick, which is the very swap #241
/// was opened about. The rule is the idle body's alone — a body with a Task it
/// cannot reach parks on the Task's own rank and keeps its tile, because it is
/// not the ground it is standing on that is stopping it.
///
/// **A body crossing for a room it cannot see keeps crossing** (#151). The
/// vision grace holds a creep's assignment while its target's room is dark, and
/// a held assignment whose Task left the pool with the vision reaches the mover
/// as `crossing`: the room the target was last seen in, and nothing else about
/// it. That is enough to walk — the step is toward the near side of a Seam,
/// which is border layer and memoised terrain (`Atlas.stepTowardRoom`) — and it
/// is the whole reason the grace is worth having: a body that stops walking
/// arrives no sooner than the one that turned round, and it is its own arrival
/// that ends the darkness. It pushes at `idleRank`, because the Task it is
/// walking for is in no pool to be priced against the ones that are.
let private moveIntentFor
    (rankOf: Task -> int)
    (idleGround: string -> Set<Pos> * Set<RoomPos>)
    (threats: Threats)
    atlas
    (creep: string)
    (at: RoomPos)
    (task: Task option)
    (crossing: string option)
    : MoveIntent =
    // The room the creep stands in and the only room its candidates are
    // tiles of (#145): it rides on the tile now (ADR 0052 decision 2)
    // rather than beside it as a field of its own.
    let room = at.Room
    let pos = RoomPos.pos at
    let here = RoomPos.at room
    let beside = Atlas.adjacentWalkableIn atlas room pos
    let onSeam = Atlas.standsOnSeam atlas creep && not (List.isEmpty beside)

    // Where this creep may stay: its own tile, unless that tile is a Seam
    // with ground beside it to walk onto.
    let staying = if onSeam then [] else [ pos ]

    let parked rank =
        {
            Creep = creep
            Pos = at
            Rank = rank
            Candidates = staying @ beside |> List.map here
            // A parked body has a Task it cannot reach, so it is standing
            // where its Work Area is not (#267).
            Area = Set.empty
        }

    // The detours behind a step: the ground beside this creep that also lies
    // beside the step it asked for — a way *around* the tile it wanted and
    // never a way back down the lane it came up. Both halves are load-bearing.
    // Without the tail a creep whose one candidate is held by a body that
    // cannot move stands still for as long as that body does; with the whole
    // neighbourhood in it, a traveller queued behind a merely fatigued creep
    // would back away and return every other tick, and ADR 0008's answer —
    // wait in place — is the right one.
    let detour step =
        if onSeam then
            beside |> List.filter ((<>) step)
        else
            let around = Atlas.adjacentWalkableIn atlas room step |> Set.ofList
            beside |> List.filter (fun tile -> Set.contains tile around)

    // The graced holder's crossing (#151), asked before the Task branches
    // because it is the one body with neither: its Task left the pool with its
    // target's room's vision, so there is nothing to price and nothing to act
    // on, and the room name is the whole of what the mover was handed. No Seam
    // to that room — it is not next door — and it falls through to the idle
    // rule below, which is what it is until the vision comes back.
    let crossingStep =
        match task, crossing with
        | None, Some room -> Atlas.stepTowardRoom atlas creep room |> Option.map RoomPos.pos
        | _ -> None

    match crossingStep, task with
    | Some step, _ ->
        {
            Creep = creep
            Pos = at
            Rank = idleRank
            Candidates = step :: detour step |> List.map here
            // Walking, and toward a room it cannot even see: nowhere it
            // stands this tick is work (#267).
            Area = Set.empty
        }
    | None, None ->
        // The room's working ground and the ground just off it: any way off
        // runs through one of those tiles, so the nearest of them is the
        // nearest standing room there is outside the colony's workplaces, and
        // the goal set stays the perimeter rather than the whole room.
        let working, offGround = idleGround room

        // The tail, ordered off the working ground first — the same job the
        // other two branches give their own tails. `arbitrate` re-houses a
        // displaced body on the first free tile of its list, so an unordered
        // tail puts a body shoved off the ground straight back onto it, and
        // the two idle bodies trade the one tile off the ground every tick.
        let off, on = beside |> List.partition (fun tile -> not (Set.contains tile working))

        let tail = staying @ off @ on

        // The way off, less this tick's Reach (ADR 0033) — the subtraction
        // `areaFor` makes below, made here too because this is the branch it
        // is hardest on: ADR 0033 gives a [[work-heavy body]] no Flee, so a
        // step out of a safe pocket into an attacker is one nothing walks it
        // back from. Nowhere safe off the ground is nowhere to go: it parks.
        let stepOff =
            if Set.contains pos working then
                let reach = Threats.reachIn threats room

                offGround
                |> Set.filter (fun tile -> not (Set.contains (RoomPos.pos tile) reach))
                |> Atlas.firstStepWithin atlas creep
                |> Option.map RoomPos.pos
            else
                None

        {
            Creep = creep
            Pos = at
            Rank = idleRank
            Candidates =
                (match stepOff with
                 | Some step -> step :: (tail |> List.filter ((<>) step))
                 | None -> tail)
                |> List.map here
            // A body with no Task is working from nowhere, which is the whole
            // of #241's rule: the ground it stands on is somebody else's to
            // work from, and shoving it off costs the chain nothing (#267).
            Area = Set.empty
        }
    | None, Some task ->
        // The area less this tick's Reach (ADR 0033): a creep works from the
        // safe half of its Work Area rather than abandoning the Task because
        // one corner is hot, and its steps go nowhere else. Read against the
        // set the Atlas already holds rather than a narrowed copy of it.
        let area = areaFor threats atlas creep task

        if Set.contains (here pos) area then
            let inside, outside =
                beside |> List.partition (fun tile -> Set.contains (here tile) area)

            {
                Creep = creep
                Pos = at
                Rank = rankOf task
                Candidates = pos :: (inside @ outside) |> List.map here
                // The one body that has arrived: the tiles it may be shuffled
                // between for nothing, and the border the arbitration charges
                // for pushing it over (#267). The area less this tick's Reach,
                // the same set the candidates were partitioned on — a tile the
                // Reach took is not somewhere this body is working from.
                Area = area
            }
        else
            match stepToward atlas creep task area |> Option.map RoomPos.pos with
            | Some step ->
                {
                    Creep = creep
                    Pos = at
                    Rank = rankOf task
                    Candidates = step :: detour step |> List.map here
                    // A traveller has not arrived: it is standing outside its
                    // Work Area, so nothing it is pushed off is work (#267).
                    Area = Set.empty
                }
            | None -> parked (rankOf task)

/// The push a rank carries into the arbitration's arithmetic. `Rank` stays the
/// fold's deterministic sort key below, and this is the second reading the
/// augmenting search needs: a *weight*, so a chain seating two bodies on the
/// steps they asked for can outweigh one body pushed off its own. Positive and
/// never rising with rank, so the ladder's order carries over, and `idleRank`
/// lands on the smallest weight there is rather than none: a body with no Task
/// pushes with something, or a crowd of idle bodies would be a wall no
/// traveller could walk into. The ladder's rungs are divided back out,
/// **rounding to the tier** (ADR 0052 decision 6): a [[priority]] the Planner
/// stepped one rung inside its tier is a claim about which of two Tasks a creep
/// should take and never about how hard it should push through a corridor.
let private weightOfRank (rank: int) : int =
    max 1 (ceilDiv (priorityOfTier Stock - rank - priorityStep) tierRungs + 2)

/// The room's matching while the arbitration runs: a tile's holder and a
/// holder's tile, one relation written both ways because the search reads it
/// both ways — the tile to find whom to displace, the creep to find what to
/// vacate. Injective in both directions by construction: a creep's own entry is
/// rewritten by the assignment that moves it, and the chain's initiator has its
/// tile emptied before the search starts.
type private Matching =
    {
        Holder: Map<RoomPos, string>
        Tile: Map<string, RoomPos>
    }

/// Resolver core: the room's Move Intents matched onto its tiles by a weighted
/// augmenting search (#219). The algorithm is sy-harabi's traffic manager, read
/// and rewritten rather than linked — that library is unlicensed and issues the
/// engine's moves itself, which ADR 0001 and ADR 0009 each refuse. What is
/// deliberately not taken from it: its hash-shuffled candidate order, because
/// every tie in this bot falls to the lowest x then y; its cost-matrix
/// threshold, because a crowd is priced here and never made impassable (ADR
/// 0008); and its free-tiles-first candidate order, which is incompatible with
/// the head-and-tail list beside it. Preference order costs nil: a room's pass
/// is O(creeps x 8). The state starts as the identity — every creep holds the
/// tile it stands on — and the intents are offered one at a time in a
/// deterministic order: travellers before stayers (a stayer settled first walls
/// off a traveller's only path), then by rank, then by name. A creep already
/// holding its first candidate is left where it is, and so is one an earlier
/// chain has already shuffled to another tile of the area it works from
/// (#267); any other is lifted off its tile and searched for an augmenting
/// path. A path is a chain of displacements ending on a free tile, and its
/// `score` is the chain's **net** priority: a
/// creep landing on the candidate it asked for first adds its rank's whole
/// weight, the chain's initiator adds the smallest weight there is for landing
/// on a tail instead, a creep merely shuffled out of the way adds nothing, and
/// a creep pushed off a step *it* had asked for subtracts its own weight. A
/// creep pushed off a tile it merely stands on costs nothing, which is ADR
/// 0001's essential rule as arithmetic. Only a strictly positive chain is
/// taken, and dropping the Map the search returned is the whole rollback.
///
/// **Yielding is a move inside the area, and eviction from it is priced**
/// (#267). "Merely stands on" was read off the candidate list — a body whose
/// head is its own tile asked for nothing and so was free to push anywhere —
/// and that reading gave away the one thing ADR 0001 was written to protect:
/// an arrived body could be shoved clean out of its Work Area for nothing, and
/// walked back in next tick at the same price, which in W13S28's Upgrade
/// pocket was a traveller and an upgrader trading the mouth every tick for as
/// long as the scan ran. So a displaced body's landing is read against the
/// area it arrived in (`MoveIntent.Area`): inside it the shuffle is free, as
/// this decision's rule requires, and outside it the chain pays that body's
/// rank's weight and the sidestep it is priced against. A body with no area is
/// unchanged — the free shuffle it always was. The price is only half of it:
/// the fold must also stop *reopening* a body it has already shuffled inside
/// its own area, or that body re-initiates and is paid its rank's whole weight
/// for landing back on the tile it never chose to leave, and three of those
/// phantom gains in one chain buy the very eviction this priced — the same two
/// bodies trading the same mouth for ever on a pocket whose free tile is
/// merely not adjacent.
let private arbitrate
    (occupants: Map<RoomPos, string>)
    (blocked: Set<RoomPos>)
    (moveIntents: MoveIntent list)
    : Map<string, RoomPos> =
    let byCreep = moveIntents |> List.map (fun i -> i.Creep, i) |> Map.ofList

    let headOf (intent: MoveIntent) = List.tryHead intent.Candidates

    // A creep whose first candidate is the tile it stands on: it asked to
    // stay, and it is displaceable by anybody.
    let staying (intent: MoveIntent) = headOf intent = Some intent.Pos

    let place creep tile (m: Matching) =
        {
            Holder = Map.add tile creep m.Holder
            Tile = Map.add creep tile m.Tile
        }

    let vacate creep (m: Matching) =
        match Map.tryFind creep m.Tile with
        | Some tile ->
            {
                Holder = Map.remove tile m.Holder
                Tile = Map.remove creep m.Tile
            }
        | None -> m

    // What a creep landing on `tile` is worth to the chain. A tail tile is
    // worth the smallest weight there is, and only to the creep the search
    // started from: that creep asked to move and could not have the tile it
    // asked for, and stepping aside is what empties a lane (#219). For a body
    // the chain is shuffling out of somebody's way it is worth nothing —
    // unless the tile lies outside the Work Area it had arrived in, where it
    // is worth *minus* its rank's weight and the sidestep above (#267). ADR
    // 0001's rule is that a creep with slack yields, and the slack a working
    // body has is its own area: pushed to another of its tiles it goes on
    // working and the chain owes it nothing, pushed off the area it stops
    // working and the chain pays for that. The charge is levied here, on the
    // landing, because that is where the search knows where the body ended up;
    // `cost` below is charged on the push, where it does not. A body with no
    // area — a traveller, a parked or idle one — is landing nowhere it was
    // working, so it is the free shuffle this rule leaves exactly as it was.
    //
    // The sidestep is why the charge is the weight **and one**, and it is not a
    // fudge: a chain that ends with its initiator stepping aside onto a tail
    // scores exactly 1, so an eviction priced at the occupant's bare weight
    // makes taking a body off its work worth the same as walking round it as
    // soon as the arriving body is one tier up the ladder — one tier and never
    // one rung, `weightOfRank` above dividing the ladder's rungs back out, so
    // one unit of push weight is one whole tier. Live that reads as the
    // [[buffer]]'s own hauler evicted from the tile it feeds from by the sixth
    // upgrader walking into a full pocket — the ring #241 opened on, the
    // served displacing the server, arrived at through eviction instead of
    // through parking. So a body is taken off its work only by a chain worth
    // strictly more than the sidestep that is the alternative to it.
    let gain initiator (intent: MoveIntent) tile =
        if headOf intent = Some tile then
            weightOfRank intent.Rank
        elif not (Set.isEmpty intent.Area) && not (Set.contains tile intent.Area) then
            -(weightOfRank intent.Rank + 1)
        elif initiator then
            1
        else
            0

    // What taking `tile` off its occupant costs the chain: the weight of a
    // step the occupant had asked for and is being denied. A stayer is denied
    // nothing here — it asked for the tile it is standing on, and what its
    // displacement costs depends on where it lands, which `gain` above prices.
    let cost (occupant: MoveIntent) tile =
        if headOf occupant = Some tile && not (staying occupant) then
            weightOfRank occupant.Rank
        else
            0

    // `visited` is threaded through and never rolled back, the one thing the
    // search borrows from its mutable original: a creep the chain has already
    // tried to rehouse is not tried again inside the same outer search, which
    // bounds the work at one expansion per creep per creep.
    let rec augment
        (initiator: bool)
        (visited: Set<string>)
        (score: int)
        (intent: MoveIntent)
        (candidates: RoomPos list)
        (m: Matching)
        : Set<string> * (int * Matching) option =
        let visited = Set.add intent.Creep visited

        let rec walk visited tiles =
            match tiles with
            | [] -> visited, None
            | tile :: rest ->
                if Set.contains tile blocked then
                    walk visited rest
                else
                    let score = score + gain initiator intent tile

                    match Map.tryFind tile m.Holder with
                    | None ->
                        if score > 0 then
                            visited, Some(score, place intent.Creep tile m)
                        else
                            walk visited rest
                    | Some held when Set.contains held visited -> walk visited rest
                    | Some held ->
                        match Map.tryFind held byCreep with
                        | None -> walk visited rest
                        | Some occupant ->
                            // The occupant keeps its tile filed under its name
                            // while its own search runs, and `place` below
                            // overwrites that entry once the chain comes back.
                            let visited, outcome =
                                augment
                                    false
                                    visited
                                    (score - cost occupant tile)
                                    occupant
                                    (occupant.Candidates |> List.filter ((<>) tile))
                                    m

                            match outcome with
                            | Some(total, settled) when total > 0 ->
                                visited, Some(total, place intent.Creep tile settled)
                            | _ -> walk visited rest

        walk visited candidates

    // The identity matching: everybody standing in the room, whether or not
    // it registered an intent, so an occupant with none is a wall the search
    // finds by looking rather than a tile somebody has to remember to block.
    let start =
        {
            Holder = occupants
            Tile =
                occupants
                |> Map.toList
                |> List.map (fun (tile, creep) -> creep, tile)
                |> Map.ofList
        }

    // Whether an intent has nothing left to ask for: it is holding the tile it
    // asked for, or — the arrived body an earlier chain has already shuffled —
    // it is holding another tile of the area it works from. The second half is
    // the fold's side of #267's rule. A yield inside the area is finished
    // business, and an arrived body that re-initiated after one would collect
    // its rank's whole weight for being put back on the tile it never chose to
    // leave: three such phantom gains in one chain buy the eviction this
    // ticket prices at the weight and the sidestep, which is the two-cycle
    // surviving its own fix on a pocket with a free tile that is not adjacent.
    let asked (intent: MoveIntent) (m: Matching) =
        let held = Map.tryFind intent.Creep m.Tile

        held = headOf intent
        || (staying intent
            && (match held with
                | Some tile -> Set.contains tile intent.Area
                | None -> false))

    let settled =
        (start, moveIntents |> List.sortBy (fun i -> staying i, i.Rank, i.Creep))
        ||> List.fold (fun m intent ->
            if asked intent m then
                m
            else
                match
                    augment true Set.empty 0 intent intent.Candidates (vacate intent.Creep m)
                with
                | _, Some(_, settled) -> settled
                | _, None -> m)

    // The creeps that registered an intent and nobody else: what the pass
    // above reads back is each *rested* creep's settled tile, and a fatigued
    // occupant is answered for out of `Blocked` and `Occupants` instead.
    settled.Tile |> Map.filter (fun creep _ -> Map.containsKey creep byCreep)

/// Direction of a single step between adjacent tiles.
let private directionTo (from: Pos) (dest: Pos) : Direction option =
    match sign (dest.X - from.X), sign (dest.Y - from.Y) with
    | 0, -1 -> Some Top
    | 1, -1 -> Some TopRight
    | 1, 0 -> Some Right
    | 1, 1 -> Some BottomRight
    | 0, 1 -> Some Bottom
    | -1, 1 -> Some BottomLeft
    | -1, 0 -> Some Left
    | -1, -1 -> Some TopLeft
    | _ -> None

/// One room's arbitration, settled: what each of its rested creeps was
/// settled on and what it asked for first, and the fatigued creeps' tiles
/// and the occupants the settlement was made against — the four things
/// the Verdicts read back, all keyed on that room's tiles alone (#145).
type private RoomPass =
    {
        /// Each rested creep's settled standing tile.
        Standing: Map<string, RoomPos>
        /// Each rested creep's preferred standing tile: the head of its
        /// candidate list — a Move Intent's candidates are never empty.
        Preferences: Map<string, RoomPos>
        /// The tiles no intent in this pass may be settled onto and no
        /// chain may run through: the fatigued creeps' (ADR 0008) and the
        /// [[foreign bodies]]' that nobody in the fold holds (#220).
        Blocked: Set<RoomPos>
        /// Who stands where at tick start.
        Occupants: Map<RoomPos, string>
    }

/// Resolver, first half: one colony's Move Intents, unarbitrated. Every rested
/// creep the Atlas places registers one (ADR 0001); a fatigued creep registers
/// none — the engine would answer its move with ERR_TIRED — and its tile is a
/// wall for the tick, so nobody plans a step through it (ADR 0008). Takes the
/// tick's assigned Task per creep as data; a creep absent from the map is idle,
/// unless it is in `crossings` — the vision grace's holders and the room each
/// is still walking toward (#151), which is the one assignment that reaches
/// here without a Task because the Task left the pool with its room's vision.
/// Rerouted is settled here rather than in the pass, because it is the one
/// movement Verdict the arbitration does not answer: it compares this creep's
/// priced first step against the step the same body would take were no tile
/// occupied, which is a second flood on this colony's Atlas.
let movementOf
    (view: ColonyView)
    atlas
    (threats: Threats)
    (pool: PooledTask list)
    (assigned: Map<string, Task>)
    (crossings: Map<string, string>)
    (verbose: Set<string>)
    : Movement =
    // The push each assigned Task carries into arbitration is its own pooled
    // [[priority]] (ADR 0052 decision 6), read off the pool rather than
    // re-derived: the mover and the Matcher order the colony's work by one
    // number or they order it by two.
    let priorities = pool |> List.map (fun p -> taskId p.Task, p.Priority) |> Map.ofList

    let priorityOf task =
        Map.tryFind (taskId task) priorities |> Option.defaultValue idleRank

    let tired =
        view.Creeps
        |> List.choose (fun c -> if c.Fatigue > 0 then Some c.Name else None)
        |> Set.ofList

    let placed = Atlas.placedCreeps atlas

    // The [[working ground]] an idle body steps off (#241), and the ring of
    // ground just outside it that any step off has to land on — one pair per
    // room some body of ours idles in, and none at all for a tick where every
    // body has a Task, which is the tick that must pay nothing for this rule.
    let idleGrounds =
        placed
        |> List.filter (fun (name, _) ->
            not (Map.containsKey name assigned)
            // A graced holder is walking a crossing, not idling (#151): it
            // steps off nothing and it is not the [[working ground]]'s
            // problem.
            && not (Map.containsKey name crossings)
            && not (Set.contains name tired))
        |> List.map (fun (_, at) -> at.Room)
        |> List.distinct
        |> List.map (fun room ->
            let working = Atlas.workingGroundIn atlas room

            let off =
                working
                |> Set.toList
                |> List.collect (Atlas.adjacentWalkableIn atlas room)
                |> List.filter (fun tile -> not (Set.contains tile working))
                |> Set.ofList
                |> RoomPos.setAt room

            room, (working, off))
        |> Map.ofList

    let idleGround room =
        Map.tryFind room idleGrounds |> Option.defaultValue (Set.empty, Set.empty)

    let rerouted name task =
        let area = areaFor threats atlas name task

        match
            Atlas.firstStep atlas name task area,
            Atlas.firstStepIgnoringTraffic atlas name task area
        with
        | Some priced, Some blind -> priced <> blind
        | _ -> false

    {
        Order = view.Creeps |> List.map (fun c -> c.Name)
        Placed = placed
        Tired = tired
        Foreign = view.Foreign
        Intents =
            placed
            |> List.filter (fun (name, _) -> not (Set.contains name tired))
            |> List.map (fun (name, at) ->
                moveIntentFor
                    priorityOf
                    idleGround
                    threats
                    atlas
                    name
                    at
                    (Map.tryFind name assigned)
                    (Map.tryFind name crossings))
        Rerouted =
            placed
            |> List.choose (fun (name, _) ->
                if Set.contains name verbose then
                    match Map.tryFind name assigned with
                    | Some task when rerouted name task -> Some name
                    | _ -> None
                else
                    None)
            |> Set.ofList
    }

/// Resolver, second half: the room passes, and the move Intents and movement
/// Verdicts they settle. One pass per room over **every** creep of ours
/// standing in it, whichever colony registered its intent (#220) — a room two
/// colonies work is one room, and half its traffic arbitrated against the other
/// half read as empty is how a body ends up claiming a tile the engine will
/// never let it into. Once per room, and never across two (#145): arbitrated
/// movement is a room's (ADR 0001, ADR 0008), and ADR 0041's Consequences keep
/// it so — geometry crosses the Seam and arbitration does not. The tiles keying
/// each room's occupants, walls and intents carry the room they are in (ADR
/// 0052 decision 2), or two creeps on one coordinate of two rooms would
/// collapse into one occupant. What is *not* arbitrated is the border tile: two
/// creeps aiming at one exit from its two sides are never checked against each
/// other, which ADR 0041 accepts in as many words. What crosses the Seam is the
/// *destination* (#142): a creep matched to an outpost's Task is arbitrated at
/// home over a home tile, and the next tick that room's pass walks it off the
/// ring onto its own floor.
let resolveRooms (movements: Movement list) : Intent list * Verdict list =
    let tired =
        (Set.empty, movements) ||> List.fold (fun acc m -> Set.union acc m.Tired)

    let everywhere = movements |> List.collect (fun m -> m.Placed)

    let passOf =
        everywhere
        |> List.map (fun (_, at) -> at.Room)
        |> List.distinct
        |> List.map (fun room ->
            let here = everywhere |> List.filter (fun (_, at) -> at.Room = room)
            let occupants = here |> List.map (fun (name, at) -> at, name) |> Map.ofList

            // The bodies no intent in this fold can move: the fatigued, and
            // the foreign tiles nobody standing here answered for.
            let foreign =
                (Set.empty, movements)
                ||> List.fold (fun acc m ->
                    Set.union acc (m.Foreign |> Set.filter (fun tile -> tile.Room = room)))
                |> Set.filter (fun tile -> not (Map.containsKey tile occupants))

            let blocked =
                here
                |> List.choose (fun (name, at) ->
                    if Set.contains name tired then Some at else None)
                |> Set.ofList
                |> Set.union foreign

            let moveIntents =
                movements
                |> List.collect (fun m -> m.Intents |> List.filter (fun i -> i.Pos.Room = room))

            room,
            {
                Standing = arbitrate occupants blocked moveIntents
                Preferences =
                    moveIntents
                    |> List.map (fun i -> i.Creep, List.head i.Candidates)
                    |> Map.ofList
                Blocked = blocked
                Occupants = occupants
            })
        |> Map.ofList

    // Every placed creep beside its room's pass, colony by colony and in
    // each colony's own view creep order: the order the Intents and
    // Verdicts leave in.
    let rows =
        movements
        |> List.collect (fun m ->
            let placed = m.Placed |> Map.ofList

            m.Order
            |> List.choose (fun name ->
                Map.tryFind name placed
                |> Option.map (fun at -> m, name, at, Map.find at.Room passOf)))

    let intents =
        rows
        |> List.choose (fun (_, name, at, pass) ->
            Map.tryFind name pass.Standing
            |> Option.bind (fun settled -> directionTo (RoomPos.pos at) (RoomPos.pos settled))
            |> Option.map (fun direction -> MoveCreep(name, direction)))

    // Who holds a tile this creep did not get, in this creep's room: the
    // creep settled on it, or the fatigued occupant whose blocked tile
    // pre-claimed it. A foreign body's tile is blocked and has no occupant
    // here, which is what leaves a creep stalled rather than yielded.
    let counterpartAt (pass: RoomPass) tile self =
        pass.Standing
        |> Map.tryPick (fun name settled ->
            if settled = tile && name <> self then Some name else None)
        |> Option.orElse (
            if Set.contains tile pass.Blocked then
                Map.tryFind tile pass.Occupants
            else
                None
        )

    let verdicts =
        rows
        |> List.collect (fun (movement, name, _, pass) ->
            if Set.contains name tired then
                [ Verdict.Grounded name ]
            else
                let reroute =
                    if Set.contains name movement.Rerouted then
                        [ Verdict.Rerouted name ]
                    else
                        []

                let yielded =
                    match Map.tryFind name pass.Preferences, Map.tryFind name pass.Standing with
                    | Some preferred, Some settled when settled <> preferred ->
                        match counterpartAt pass preferred name with
                        | Some other -> [ Verdict.Yielded(name, other) ]
                        | None -> [ Verdict.Stalled name ]
                    | _ -> []

                reroute @ yielded)

    intents, verdicts

/// Resolver, single colony: this colony's movement, arbitrated against nobody
/// else's.
let resolve
    (view: ColonyView)
    atlas
    (threats: Threats)
    (pool: PooledTask list)
    (assigned: Map<string, Task>)
    (crossings: Map<string, string>)
    (verbose: Set<string>)
    : Intent list * Verdict list =
    resolveRooms [ movementOf view atlas threats pool assigned crossings verbose ]

/// Matcher: keep still-valid assignments (anti-thrash) and greedily assign the
/// rest. Assignments in, Assignments and the Verdicts explaining them out (ADR
/// 0009): releases first in memory order, then one status Verdict per living
/// creep in view order — each preceded, for a creep on the verbose list, by its
/// Scoring Verdict, the whole pool judged against the same state its status was
/// decided from. Emission belongs to the Emitter, movement to the Resolver.
/// **It knows no Task kinds** (ADR 0052 decision 6).
let matchCreeps
    (view: ColonyView)
    atlas
    (sizing: RowSizing)
    (threats: Threats)
    (pool: PooledTask list)
    (assignments: Assignments)
    (verbose: Set<string>)
    : Assignments * Verdict list =
    let byId = pool |> List.map (fun p -> taskId p.Task, p) |> Map.ofList

    // Each living creep's remaining life and its [[body class]], hoisted
    // for the tick: the capacity gate asks the first once per holder per
    // judged pair and the second once per holder and once per candidate,
    // and both are view facts that cannot move inside a tick.
    let lives = view.Creeps |> List.map (fun c -> c.Name, c.TicksToLive) |> Map.ofList

    let classes =
        view.Creeps
        |> List.map (fun c -> c.Name, bodyClassOf view.Tuning atlas c)
        |> Map.ofList

    let classOf name = Map.tryFind name classes

    // The crowding component of the matching key (ADR 0002): every holder,
    // counted at this tick. Arrival discounts what a Task's cap counts (ADR
    // 0026), never what the key does — spreading creeps over Tasks is a
    // judgement about now.
    let load (loads: Map<string, int>) tid =
        Map.tryFind tid loads |> Option.defaultValue 0

    let hold (loads: Map<string, int>) tid = Map.add tid (load loads tid + 1) loads

    // The holders a candidate actually competes with, counted at arrival (ADR
    // 0026): two creeps hold the same standing room against each other only
    // while both are standing on it, so a holder counts against a candidate
    // exactly when their two stays overlap.
    let overlaps (candidate: CreepInfo) task arrival name =
        let alive =
            match arrival with
            | None -> true
            | Some ticks -> Map.tryFind name lives |> Option.forall (fun life -> life >= ticks)

        let arrived =
            match Atlas.walkTicks atlas name task with
            | None -> true
            | Some ticks -> ticks <= candidate.TicksToLive

        alive && arrived

    let holdersAt (acc: Assignments) (candidate: CreepInfo) task arrival =
        let tid = taskId task

        acc
        |> Map.toList
        |> List.choose (fun (name, assigned) ->
            if assigned = tid && overlaps candidate task arrival name then
                Some name
            else
                None)

    // Every heavy body and the tile it stands on, folded once for the tick: the
    // Post census below asks this of every (candidate, Harvest) pair, and since
    // #269 every posted rock carries tiles where only a rock mid-build did.
    let heavyStanders =
        view.Creeps
        |> List.choose (fun c ->
            if classOf c.Name = Some Heavy then
                Atlas.creepTile atlas c.Name |> Option.map (fun tile -> c.Name, tile)
            else
                None)

    // The bodies standing on the Task's `Garrison` tiles, whatever Task they
    // hold this tick (#205, widened to every Post by #269) — **unioned** with
    // the Heavy holders below rather than added to them, because on a standing
    // container the two sets are ordinarily the same body: the overflow
    // reprieve keeps a garrison's Harvest applicable through a full store, so
    // it holds the Task it is standing on and a sum would spend two of the
    // rock's Posts on one Anchor. What the tiles add is the tick the two part —
    // a build tick on a Post whose container is still a site, an Upgrade
    // through the empty window on a bare dual seat — where a cap counting
    // assignments alone reads a manned Post as free. Counted at arrival like
    // every other holder (ADR 0026), which is what keeps a succession's
    // successor admissible; the candidate never counts against itself.
    let garrisons (candidate: CreepInfo) task arrival (tiles: Set<RoomPos>) =
        if Set.isEmpty tiles then
            Set.empty
        else
            heavyStanders
            |> List.choose (fun (name, tile) ->
                if
                    name <> candidate.Name
                    && Set.contains tile tiles
                    && overlaps candidate task arrival name
                then
                    Some name
                else
                    None)
            |> Set.ofList

    // Holders against numbers, and nothing else (ADR 0052 decision 6): the
    // total the Task admits, the share each scope the candidate falls in
    // admits, and the tiles whose standing bodies hold a slot without holding
    // the Task. A candidate standing on an `Exempt` tile is outside all of it —
    // the one body a budget that prices a commute never priced (#205).
    let hasCapacity (creep: CreepInfo) acc (pooled: PooledTask) (arrival: Lazy<int option>) =
        let capacity = pooled.Capacity

        let standing =
            not (Set.isEmpty capacity.Exempt)
            && (Atlas.creepTile atlas creep.Name
                |> Option.exists (fun tile -> Set.contains tile capacity.Exempt))

        if standing || not (Capacity.isBounded capacity) then
            // Only a capped Task forces the walk: the Refills and the
            // surplus work the pool is mostly made of neither walk the
            // assignment map nor pay for an arrival (ADR 0029).
            true
        else
            let holders = holdersAt acc creep pooled.Task arrival.Value
            let cls = classOf creep.Name

            let inClass wanted =
                holders |> List.filter (fun name -> classOf name = Some wanted)

            let heavyHolders = inClass Heavy
            let heavy = List.length heavyHolders
            let standingRow = inClass Standing |> List.length
            let all = List.length holders

            // The Post cap's crowd, by **name**: the heavy bodies holding this
            // Task and the heavy bodies standing on its Posts are one crowd,
            // and the ordinary garrison is in both lists (#269). Summed, it
            // would spend two of a two-Post rock's slots on the one Anchor that
            // is both, and the second Post would read full while it stands
            // empty. Only the Heavy cap reads tiles; the class shares below
            // stay counts of holders.
            let garrisoned =
                garrisons creep pooled.Task arrival.Value capacity.Garrison
                |> Set.union (Set.ofList heavyHolders)
                |> Set.count

            // A cap the candidate's own class does not fall in is not its
            // cap: the `None` scope is how a rule says "this number is
            // about somebody else's crowd".
            let within scope cap counted =
                match cap with
                | Some limit when cls |> Option.exists scope -> counted < limit
                | _ -> true

            within (fun _ -> true) capacity.Total all
            && within ((=) Heavy) capacity.Garrisons garrisoned
            && within ((<>) Heavy) capacity.Commuters (all - heavy)
            && within ((=) Standing) capacity.Standing standingRow
            && within
                (fun c -> c <> Heavy && c <> Standing)
                capacity.Generalists
                (all - heavy - standingRow)
            // The one cap that refuses a class outright rather than counting it
            // (ADR 0056): a `Fighters` number admits that many Fighters and no
            // body of any other class, because the scopes above cannot spell
            // "not a Fighter" — `Commuters` and `Generalists` both contain it —
            // and `within`'s `None` scope means "somebody else's crowd", which
            // is the opposite of a refusal.
            && (match capacity.Fighters with
                | Some limit -> cls = Some Fighter && (inClass Fighter |> List.length) < limit
                | None -> true)

    // The vision grace (#151): a Task leaves the pool for two opposite reasons
    // and its id alone cannot tell them apart — the target was destroyed, or
    // the room carrying it went dark. The pool stays gated on vision and
    // rightly so (ADR 0004): a blind room hands over an empty site list, and
    // no rule here may price what nobody can see. What this asks instead is
    // about **looking**, never about the target — the id stood in a room this
    // colony works, that room has not been seen since, and it went dark inside
    // `Tuning.VisionGrace`. Then the assignment is kept: the [[outpost]] whose
    // [[reserver]] just died is dark for the relief's lead and no longer, and
    // a builder released here walks a full load home to start the crossing
    // again on the tick the vision returns. Past the grace the release is
    // `task-gone` exactly as it was, because a container that really was
    // destroyed must not be held for ever by a body that cannot see the tile.
    // One rule over every Task kind that vision pays for, and Harvest needs
    // none of it: a declared rock is placed and pooled without vision at all
    // (ADR 0041, #148).
    //
    // One thing the grace may not outrank, and it is the one thing nothing
    // below could have asked for it: Safety (ADR 0033). A Task in no pool has
    // no Work Area, so `threatened` — which reads the *Task's* tiles — answers
    // false for it whatever stands where; the question that keeps a body alive
    // has to be asked of the **creep**. A holder standing inside a Reach is
    // therefore denied the grace, falls through to `task-gone`, and rematches
    // to Flee in the cascade below, exactly as it did before the grace existed.
    let graced (creep: CreepInfo) tid =
        if standsInReach threats atlas creep.Name then
            None
        else
            lastSeenIn view tid

    // Capacity applies to remembered assignments too: memory can carry an
    // oversell from before a cap existed. So does reachability — a Work Area
    // the Atlas can no longer reach releases the assignment, freeing its
    // capacity for creeps that can get there, deliberately with no range-based
    // fallback (ADR 0002) — and so does the arrival gate: a drained source's
    // Harvest whose wait the holder's walk no longer covers releases it (ADR
    // 0025). Each failed gate names the release; a dead creep's assignment
    // drops silently.
    let kept, keptLoads, released =
        ((Map.empty, Map.empty, []), assignments)
        ||> Map.fold (fun (acc, loads, released) name tid ->
            let release reason =
                acc, loads, Verdict.Released(name, tid, reason) :: released

            match view.Creeps |> List.tryFind (fun c -> c.Name = name) with
            | None -> acc, loads, released
            | Some creep ->
                match Map.tryFind tid byId with
                // Kept, and by the Verdict every other steady assignment
                // answers with (ADR 0009): what the colony did this tick about
                // this creep is nothing, and there is no second word for it.
                // The Task is in no pool, so no gate below can be asked about
                // it: `graced` is the whole judgement, and the one gate it
                // carries is the one a Work Area could not have answered for
                // (Safety, ADR 0033). The holder counts against its own Task's
                // crowd the tick the vision returns, on the ordinary path.
                | None when Option.isSome (graced creep tid) ->
                    Map.add name tid acc, hold loads tid, released
                | None -> release ReleaseReason.TaskGone
                // The raid's release stands ahead of the ordinary one: a
                // Task whose whole Work Area is in a Reach is gone for this
                // creep however well its body fits (ADR 0033).
                | Some pooled when threatened threats atlas creep pooled.Task ->
                    release ReleaseReason.Threatened
                | Some pooled when not (applicable view threats atlas creep pooled) ->
                    release ReleaseReason.Inapplicable
                | Some pooled ->
                    // The walk is bound one gate before it is spent, exactly as
                    // the fresh cascade below binds it: capacity counts holders
                    // at this holder's own arrival (ADR 0026), and the arrival
                    // gate spends the very same number — priced at most once,
                    // and only if one of the two asks.
                    let cost = travelCostOf threats atlas creep.Name pooled.Task
                    let arrival = lazy (Atlas.walkTicks atlas creep.Name pooled.Task)

                    if
                        not (hasCapacity creep acc pooled arrival)
                        && not (expiring view atlas sizing creep)
                    then
                        release ReleaseReason.OverCapacity
                    else
                        match cost with
                        | None -> release ReleaseReason.Unreachable
                        | Some _ ->
                            match tooEarly view atlas creep pooled.Task arrival with
                            | Some(walk, wait) -> release (ReleaseReason.TooEarly(walk, wait))
                            | None -> Map.add name tid acc, hold loads tid, released)

    // One gate cascade judges every (creep, Task) pair — rejected at the first
    // matching gate it fails (applicable, capacity, reachable, in time) or
    // scored on the full key when none does. Two numbers are bound above the
    // capacity gate: the travel cost, which the reachability gate and the
    // scored key both read, and the walk, which capacity counts holders at (ADR
    // 0026) and the arrival gate spends after it — one number for both, priced
    // at most once.
    let judge acc loads (creep: CreepInfo) (pooled: PooledTask) =
        let tid = taskId pooled.Task

        if threatened threats atlas creep pooled.Task then
            Candidate.Rejected(tid, RejectReason.Threatened)
        elif not (applicable view threats atlas creep pooled) then
            Candidate.Rejected(tid, RejectReason.Inapplicable)
        else
            let cost = travelCostOf threats atlas creep.Name pooled.Task
            let arrival = lazy (Atlas.walkTicks atlas creep.Name pooled.Task)

            if not (hasCapacity creep acc pooled arrival) then
                Candidate.Rejected(tid, RejectReason.CapacityFull)
            else
                match cost with
                | None -> Candidate.Rejected(tid, RejectReason.Unreachable)
                | Some cost ->
                    match tooEarly view atlas creep pooled.Task arrival with
                    | Some(walk, wait) -> Candidate.Rejected(tid, RejectReason.TooEarly(walk, wait))
                    | None -> Candidate.Scored(tid, pooled.Priority, cost, load loads tid)

    let assignOne (acc, loads, verdicts) (creep: CreepInfo) =
        let verdicts =
            if Set.contains creep.Name verbose then
                // The creep's own claim is set aside for its scoring: a held
                // single-Seat Task must read as the winning row, never as
                // capacity-full against its own holder's seat. The crowding
                // table is set aside with it, the two being one fact (#95).
                let without =
                    match Map.tryFind creep.Name acc with
                    | Some tid -> Map.add tid (max 0 (load loads tid - 1)) loads
                    | None -> loads

                let rows = pool |> List.map (judge (Map.remove creep.Name acc) without creep)
                Verdict.Scoring(creep.Name, rows) :: verdicts
            else
                verdicts

        match Map.tryFind creep.Name acc with
        | Some tid -> acc, loads, Verdict.Kept(creep.Name, tid) :: verdicts
        | None ->
            let judged = pool |> List.map (fun p -> p.Task, judge acc loads creep p)

            let keyed =
                judged
                |> List.choose (function
                    | t, Candidate.Scored(_, rank, cost, load) -> Some((rank, cost, load), t)
                    | _ -> None)

            match keyed with
            | [] ->
                // How far the best Task got through the gates — applicable,
                // capacity, reachable, in time — is why the creep sits idle,
                // deepest gate first: a creep whose only rejection is the arrival
                // gate is waiting out a restock (ADR 0025), and saying nothing
                // fit its body would be the same lie the rejection reason
                // refuses.
                let rejectedWith wanted =
                    judged
                    |> List.exists (function
                        | _, Candidate.Rejected(_, reason) -> wanted reason
                        | _ -> false)

                // The arrival gate's reason carries the numbers it compared
                // (#88), so the depth question asks after the case rather
                // than after a value it would have to invent to compare to.
                let isTooEarly =
                    function
                    | RejectReason.TooEarly _ -> true
                    | _ -> false

                let reason =
                    if List.isEmpty pool then
                        IdleReason.NoTasks
                    elif rejectedWith isTooEarly then
                        IdleReason.NoneInTime
                    elif rejectedWith ((=) RejectReason.Unreachable) then
                        IdleReason.NoneReachable
                    elif rejectedWith ((=) RejectReason.CapacityFull) then
                        IdleReason.NoneFree
                    else
                        IdleReason.NoneApplicable

                acc, loads, Verdict.Unassigned(creep.Name, reason) :: verdicts
            | keyed ->
                let bestKey, task = keyed |> List.minBy fst

                // The deciding factor: the first component separating the
                // winner from its closest rival, or the pool-order tie-break
                // when the whole key ties.
                let factor =
                    match keyed |> List.filter (fun (_, t) -> t <> task) with
                    | [] -> MatchFactor.OnlyCandidate
                    | rivals ->
                        let bestRank, bestCost, bestLoad = bestKey
                        let rivalRank, rivalCost, rivalLoad = rivals |> List.map fst |> List.min

                        if rivalRank <> bestRank then MatchFactor.Rank
                        elif rivalCost <> bestCost then MatchFactor.TravelCost
                        elif rivalLoad <> bestLoad then MatchFactor.Load
                        else MatchFactor.PoolOrder

                Map.add creep.Name (taskId task) acc,
                hold loads (taskId task),
                Verdict.Matched(creep.Name, taskId task, factor) :: verdicts

    let next, _, statuses = view.Creeps |> List.fold assignOne (kept, keptLoads, [])

    next, List.rev released @ List.rev statuses

/// Join the Matcher's Assignments back onto the Planner's pool: the tick's
/// assigned Task per creep, as data for the Emitter and the Resolver.
let private assignedTasks (tasks: Task list) (assignments: Assignments) : Map<string, Task> =
    let byId = tasks |> List.map (fun t -> taskId t, t) |> Map.ofList

    assignments
    |> Map.toList
    |> List.choose (fun (name, tid) -> Map.tryFind tid byId |> Option.map (fun t -> name, t))
    |> Map.ofList

/// The census signature (ADR 0017): a string over exactly the inputs the
/// census-derived plans read — the (kind, position) census of standing
/// structures, the (kind, position) census of pending sites, the controller
/// level, the home room's name, who holds each room the projection carries, and
/// every colony's [[stage]]. Any one input moving moves the signature;
/// everything else a view carries — creeps, stores, hits, piles, hostiles,
/// banked energy, the tick — is invisible to it. The hauler quota rides the
/// same signature on two load-bearing derivations, and both are covered here
/// rather than assumed. The bank Capacity it sizes bodies from is a function of
/// the standing census and the level, both signed above. Since ADR 0042 it also
/// prices each container at its source's own output, read off `RoomControl` — a
/// **vision** fact and not a census one, so it is signed explicitly, and as the
/// *rate* rather than the reservation's `TicksToEnd`, which decays every tick.
/// **It signs every projected room**, a deliberate widening, because ADR 0042's
/// hauler quota is the entry that reads a second one. The standing census names
/// the room in each entry, two rooms holding the same coordinates, and spans
/// every *kind* rather than the containers alone, because the round trip floods
/// that room's step-weight grid. The pending census spans every projected room
/// too: the walk table's far leg floods the *goal* room's grid, and a site
/// outside home closes a tile there.
let censusSignature (view: ColonyView) : string =
    let spatial = view.Spatial
    let home = SpatialInfo.homeName spatial

    // One join for both halves: a target is read wherever the
    // projection places it, standing or pending alike, because both halves
    // move a grid the memo holds an answer off.
    let census select =
        spatial.TargetKinds
        |> Map.toList
        |> List.choose (fun (id, kind) ->
            select kind
            |> Option.bind (fun (built: BuiltKind) ->
                SpatialInfo.placementOf spatial id
                |> Option.map (fun tile -> $"{built}@{tile.Room}:{tile.X},{tile.Y}")))
        |> List.sort
        |> String.concat ";"

    let standing =
        census (function
            | Structure kind -> Some kind
            | _ -> None)

    let pending =
        census (function
            | Site kind -> Some kind
            | _ -> None)

    let level =
        view.Controller
        |> Option.map (fun c -> string c.Level)
        |> Option.defaultValue ""

    // The rate each projected room's sources are priced at this tick, in
    // room-name order, and the empty rate for a room vision answered for not at
    // all — the third answer the quota gives, and a different one from either
    // rate (ADR 0004).
    let held =
        spatial.Rooms
        |> Map.toList
        |> List.map (fun (room, _) ->
            let rate =
                Map.tryFind room view.RoomControl
                |> Option.map (heldRateOf >> string)
                |> Option.defaultValue ""

            $"{room}:{rate}")
        |> String.concat ","

    // Every colony of ours and the stage it stands at, in room-name order:
    // the map is already sorted by key, and a room leaving it moves the
    // string as surely as one changing stage does.
    let stages =
        view.Stages
        |> Map.toList
        |> List.map (fun (room, stage) -> $"{room}:{stage}")
        |> String.concat ","

    $"{home}|{level}|{held}|{stages}|{standing}|{pending}"

/// The decision seam: a colony view in — with the verbose list of creep names
/// owed the manufactured-evidence Verdicts and the previous tick's plan memo —
/// Decision out, with this colony's movement left unarbitrated on it. A room's
/// traffic is not one colony's decision, so the last step of the pipeline is
/// not taken here: what comes out is the room's Move Intents, and the caller
/// folds every colony's together and arbitrates each room once
/// (`resolveRooms`). The tick's pipeline is visible here — plan, match, emit,
/// move — beside the colony steps (spawns, sites), with geometry consulted
/// through one Atlas built up front, so every step prices from the same flood
/// (ADR 0004).
let decideUnarbitrated
    (view: ColonyView)
    (assignments: Assignments)
    (verbose: Set<string>)
    (memo: PlanMemo option)
    : Decision =
    let signature = censusSignature view
    // The signature is read before the Atlas is built, because the Atlas
    // is one of the things it decides: a memo whose census still stands
    // hands over its spawn walk table, and a memo that has gone stale —
    // or none at all — leaves the Atlas a fresh one (ADR 0032).
    let recalled = memo |> Option.filter (fun m -> m.Signature = signature)

    let walks =
        match recalled with
        | Some m -> m.Walks
        | None -> WalkTable()

    let atlas = Atlas.ofViewRecalling walks view

    let plan =
        match recalled with
        | Some m -> m
        | None ->
            let siteIntents, servedFootings, unservedFootings, unroutedTrunks, deferredContainers =
                planLayout view atlas

            let quota, demandRows, load = haulerDemandOf view atlas

            {
                Signature = signature
                SiteIntents = siteIntents
                UnservedFootings = unservedFootings
                ServedFootings = servedFootings
                UnroutedTrunks = unroutedTrunks
                DeferredContainers = deferredContainers
                HaulerQuota = quota
                HaulerDemand = demandRows
                HaulerLoad = load
                Walks = walks
            }

    // The tick's Threats, derived once off the view's hostiles and the
    // rampart census, and shared by every reader of them (ADR 0033).
    let threats = threatsOf view atlas

    // The colony's other placement step, beside the memoised Layout and
    // never inside it (ADR 0042): the outpost's source containers, derived
    // fresh every tick for the reason written on the rule itself.
    let outpostSiteIntents = planOutpostContainers view atlas

    let defenseIntents = planSafeMode view atlas @ planFire view atlas

    // The pool is derived before the spawns, and the dependency runs one way
    // only: the worker row's floor asks the pool whether anything is standing
    // in Build or Repair (ADR 0046), and nothing in the pool reads a spawn
    // Intent.
    let sizing = rowSizingOf view atlas

    let tasks = planTasks view threats

    // The pool's other half (ADR 0052 decision 6): every entry's priority
    // and capacity, set once here and read by the Matcher and the mover.
    let pool = planPool view atlas tasks

    let spawnIntents, quotas =
        planSpawns view atlas sizing threats tasks plan.HaulerQuota

    let next, verdicts = matchCreeps view atlas sizing threats pool assignments verbose
    let assigned = assignedTasks tasks next

    // The other half of the vision grace (#151): the holders the Matcher kept
    // against a Task that is in no pool, and the room each one's target was
    // last seen in. They reach the Emitter as nothing — there is no act to
    // spell on a target nobody can see — and the mover as a crossing, which is
    // the whole of what the grace buys. Read off the same rule the keep was
    // taken on (`lastSeenIn`), over the assignments that survived it: a keep
    // with no pooled Task is a graced one by construction.
    let crossings =
        next
        |> Map.toList
        |> List.filter (fun (name, _) -> not (Map.containsKey name assigned))
        |> List.choose (fun (name, tid) ->
            lastSeenIn view tid |> Option.map (fun room -> name, room))
        |> Map.ofList

    let taskIntents = emit view atlas threats assigned

    // The reflex, less what a Task already asked for: the Pickup Task's own act
    // is a strict subset of the reflex's — both want a Carry body with room in
    // it standing within range 1 of the pile in that pile's own room — so an
    // arriving picker's `PickupEnergy` was going to be spelt twice. The engine
    // executes a creep's second pickup over its first, so the duplicate cost
    // nothing on the server; what it did cost is the accepted-[[intent]] count
    // the CPU line is read off.
    let pickupIntents =
        planPickups view atlas
        |> List.filter (fun intent -> not (List.contains intent taskIntents))

    {
        Intents =
            defenseIntents
            @ spawnIntents
            @ plan.SiteIntents
            @ outpostSiteIntents
            @ pickupIntents
            @ taskIntents
        Assignments = next
        Memo = plan
        Verdicts = verdicts
        Movement = movementOf view atlas threats pool assigned crossings verbose
        Quotas =
            { quotas with
                HaulerLoad = plan.HaulerLoad
                HaulerDemand = plan.HaulerDemand
            }
    }

/// The decision seam a shell with one colony — and the whole suite — asks for:
/// `decideUnarbitrated`'s answer with this colony's movement folded back in
/// through the one-room-at-a-time pass (`resolveRooms`), so the `Intents` and
/// `Verdicts` here are the tick's whole answer for this colony. The two are the
/// same call in every world one colony works alone.
let decide
    (view: ColonyView)
    (assignments: Assignments)
    (verbose: Set<string>)
    (memo: PlanMemo option)
    : Decision =
    let decision = decideUnarbitrated view assignments verbose memo
    let moveIntents, moveVerdicts = resolveRooms [ decision.Movement ]

    { decision with
        Intents = decision.Intents @ moveIntents
        Verdicts = decision.Verdicts @ moveVerdicts
    }
