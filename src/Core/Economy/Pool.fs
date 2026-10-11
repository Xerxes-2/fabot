/// The world pool: every Task offered this tick, where it ranks (its price,
/// `Price`) and how many bodies it admits. One pool for every colony: a body
/// takes the work it fits and reaches, whoever cast it (`ARCHITECTURE.md` §5).
module Fabot.Core.Economy.Pool

open Fabot.Core
open Fabot.Core.Vocabulary
open Fabot.Core.World
open Fabot.Core.Economy.Task
open Fabot.Core.Economy.Bodies
open Fabot.Core.Economy.Derived

/// How many bodies a Task admits, counted in what its holders bring to it. A
/// candidate is admitted while what the holders before it bring falls short
/// of the number.
[<RequireQualifiedAccess>]
type Capacity =
    | Unbounded
    /// Holders, one each.
    | Bodies of int
    /// Holders' live WORK parts.
    | Work of int
    /// Holders, one to a seat, and their live WORK parts: whichever binds.
    /// Of the seats, `posts` are the garrisons' (`garrisons`) and the rest
    /// everyone else's, so a light body never holds the seat a harvester
    /// digs into a container from; with no post every seat is anyone's.
    | Seated of seats: int * posts: int * work: int
    /// Energy: an intake counts its holders' free room, a delivery what they
    /// carry of the resource.
    | Load of int

/// Which bodies a draw from a store is for.
[<RequireQualifiedAccess>]
type Draw =
    /// Any body with room: a store the flow passes through, or the stock
    /// while a sink other than itself is hungry.
    | Open
    /// A body that spends what it draws (a WORK part): the stock while no
    /// other sink is hungry, which a carrier would only pour back.
    | Spenders
    /// On an Upgrade, the controller's buffer stocked: its holder draws as
    /// it upgrades (`drawsBuffer`) and may hold it empty. The buffer is drawn
    /// nowhere else, so what it holds goes to the controller alone.
    | Buffer

/// One entry of the pool.
[<NoEquality; NoComparison>]
type Offer =
    {
        Task: Task
        /// `taskId Task`, taken once.
        Id: string
        /// Where the target stands: what travel is priced to.
        Target: Pos
        /// Where the Task ranks against every other, lower first.
        Priority: int
        Capacity: Capacity
        /// Which bodies may take it, where it is a draw; `Open` for any other
        /// Task.
        Draw: Draw
    }

/// What a body carries of a resource.
let private carried (body: Body) (resource: Resource) = Store.amountOf resource body.Store

/// Whether a body holds a seated offer's post rather than its other seats: a
/// harvester, where the offer has a post.
let garrisons (body: Body) (offer: Offer) : bool =
    match offer.Capacity with
    | Capacity.Seated(_, posts, _) -> posts > 0 && body.Pattern = Pattern.Harvester
    | _ -> false

/// Whether a body's parts let it upgrade.
let upgrades (body: Body) : bool =
    body.Pattern <> Pattern.Harvester && body.Work > 0 && body.Carry > 0

/// Whether an Upgrade's holder draws the buffer this tick: what it holds
/// falls short of a tick's upgrade. The engine withdraws before it upgrades,
/// so the draw tops up what the upgrade spends.
let drawsBuffer (body: Body) (offer: Offer) : bool =
    offer.Draw = Draw.Buffer
    && body.Free > 0
    && body.Energy < body.Work * Engine.upgradeDrainPerWork

/// Whether a body's parts and store let it act on the offer now. One rule
/// for a fresh match and a kept one: an intake while the store is at least
/// half empty (a fuller body delivers first), or a dig already in reach until
/// the store is full; a delivery while it holds the resource. A harvester
/// digs whatever its store holds, the engine dropping the overflow where it
/// stands, and takes nothing but a post: the seat it was cast for and `Quota`
/// counts it in. An upgrader upgrades and nothing else: a delivery is a
/// commute for its WORK.
let fits (body: Body) (offer: Offer) : bool =
    let halfEmpty = body.Free > 0 && body.Free * 2 >= body.Store.Capacity

    match offer.Task with
    | Upgrade _ -> upgrades body && (body.Energy > 0 || offer.Draw = Draw.Buffer)
    | _ when body.Pattern = Pattern.Upgrader -> false
    | Harvest _ ->
        body.Work > 0
        && (garrisons body offer
            || body.Pattern <> Pattern.Harvester
               && (halfEmpty
                   || body.Free > 0 && Pos.within (rangeOf offer.Task) body.Pos offer.Target))
    | _ when body.Pattern = Pattern.Harvester -> false
    | Withdraw _ when offer.Draw <> Draw.Open && body.Work = 0 -> false
    | Withdraw _
    | Pickup _ -> body.Carry > 0 && halfEmpty
    | Refill(_, resource) -> body.Carry > 0 && carried body resource > 0
    | Build _
    | Repair _ -> body.Work > 0 && body.Carry > 0 && body.Energy > 0
    | Reserve _
    | Claim _ -> body.Claim > 0

/// What one holder counts against an offer's capacity. Where a seated offer
/// has a post, only its garrison counts WORK: the other seats stay open while
/// the garrison digs short of the work.
let share (body: Body) (offer: Offer) : int =
    match offer.Capacity with
    | Capacity.Unbounded -> 0
    | Capacity.Bodies _ -> 1
    | Capacity.Seated(_, posts, _) when posts > 0 && not (garrisons body offer) -> 0
    | Capacity.Work _
    | Capacity.Seated _ -> body.Work
    | Capacity.Load _ ->
        match offer.Task with
        | Refill(_, resource) -> carried body resource
        | task when isIntake task -> body.Free
        | _ -> body.Energy

/// Whether an offer admits one more holder while its holders bring `held`
/// (`share` summed) and `seatedAlike` of them hold seats of the candidate's kind
/// (`garrisons`).
let admits (offer: Offer) (garrison: bool) (held: int) (seatedAlike: int) : bool =
    match offer.Capacity with
    | Capacity.Unbounded -> true
    | Capacity.Bodies n
    | Capacity.Work n
    | Capacity.Load n -> held < n
    | Capacity.Seated(seats, posts, work) ->
        held < work && seatedAlike < (if garrison then posts else seats - posts)

/// What a body brings a seated offer over its stay, `walk` ticks away: the
/// energy it digs or upgrades before it dies, or before its load is full (a
/// dig into its store, counting what it already dug, so a digger part-way
/// through its load is worth no less than an empty one) or empty (an
/// upgrade off no stocked buffer). 0 for any other offer.
let seatValue (body: Body) (offer: Offer) (walk: int) : int =
    match offer.Capacity with
    | Capacity.Seated _ ->
        let stay = max 0 (body.TicksToLive - walk)

        match offer.Task with
        | Harvest _ ->
            let dig = body.Work * Engine.harvestPerWork * stay

            if body.Pattern = Pattern.Harvester then
                dig
            else
                min (body.Free + body.Energy) dig
        | Upgrade _ ->
            let upgrade = body.Work * Engine.upgradeDrainPerWork * stay

            if offer.Draw = Draw.Buffer then
                upgrade
            else
                min body.Energy upgrade
        | _ -> body.Work * stay
    | _ -> 0

let private offer task (target: Pos) (value: float) capacity =
    {
        Task = task
        Id = taskId task
        Target = target
        Priority = Price.economyRank value
        Capacity = capacity
        Draw = Draw.Open
    }

/// The controller stands inside the downgrade line safe mode needs.
let private insideDeadline (c: Controller) =
    c.Level > 0 && c.TicksToDowngrade < Engine.safeModeDowngradeLine c.Level

/// Where a site of ours ranks: every site alike at the surplus band, its
/// order Layout's, which places them in plan order. The one place a site is
/// ranked.
let siteRank (_: Derived) (_: SiteFact) : int = Price.surplusRank

/// Every Task offered this tick, ground by ground in `Derived` order. A Task
/// with no energy price is anchored at a band (`Price`).
let pool (derived: Derived) : Offer[] =
    let offers = ResizeArray<Offer>()

    // Resources other than energy some body carries: what the storage is
    // offered for, so no load is stranded.
    let ores =
        derived.Bodies
        |> Array.collect (fun b -> b.Store.Held |> Array.map fst)
        |> Array.filter (fun r -> r <> Energy)
        |> Array.distinct

    derived.Grounds
    |> Array.iteri (fun i ground ->
        let colony =
            if i < derived.Colonies.Length then
                Some derived.Colonies.[i]
            else
                None

        // Offered through a regeneration gap too: its holders keep their
        // seats and wait to dig. Priced at the rate, the engine's cap on
        // what its seats dig.
        for s in ground.Sources do
            offers.Add(
                offer
                    (Harvest s.Id)
                    s.Pos
                    (float s.Rate)
                    (Capacity.Seated(
                        s.Seats,
                        (if Option.isSome s.Post then 1 else 0),
                        (s.Rate + Engine.harvestPerWork - 1) / Engine.harvestPerWork
                    ))
            )

        for m in ground.Minerals do
            offers.Add(
                offer (Harvest m.Id) m.Pos (Price.valueOfRank Price.bottomRank) (Capacity.Bodies 1)
            )

        // The refills the stock supplies: its draw is priced off the hungriest.
        let mutable hungriest = 0.0

        for r in ground.Refills do
            let value =
                if r.Kind = Kind.Tower then
                    Price.towerShort Price.homeRaidPrior r.Free
                else
                    // A tick the spawn waits is a tick of the income its
                    // bodies earn.
                    float ground.Income

            hungriest <- max hungriest value
            offers.Add(offer (Refill(r.Id, Energy)) r.Pos value (Capacity.Load r.Free))

        for s in ground.Sites do
            offers.Add(
                offer
                    (Build s.Id)
                    s.Pos
                    (Price.valueOfRank (siteRank derived s))
                    (Capacity.Load s.Left)
            )

        for r in ground.Repairs do
            let missing =
                (r.HitsMax - r.Hits + Engine.repairHitsPerEnergy - 1)
                / Engine.repairHitsPerEnergy

            offers.Add(
                offer
                    (Repair r.Id)
                    r.Pos
                    (Price.rebuildAtRisk r.Rebuild r.Hits r.Decay)
                    (Capacity.Load missing)
            )

        match ground.Controller with
        | Some c when c.Owner = Ours ->
            let capacity =
                Capacity.Seated(
                    ground.UpgradeSeats,
                    0,
                    if c.Level >= Engine.controllerMaxLevel then
                        Engine.maxUpgradePerTick
                    else
                        System.Int32.MaxValue
                )

            let upgrade =
                { offer (Upgrade c.Id) c.Pos (Price.valueOfRank Price.upgradeRank) capacity with
                    Draw =
                        if ground.Buffer |> Option.exists (fun b -> b.Energy > 0) then
                            Draw.Buffer
                        else
                            Draw.Open
                }

            offers.Add(
                if insideDeadline c then
                    { upgrade with
                        Priority = Price.deadlineRank
                    }
                else
                    upgrade
            )
        | Some c when ground.Reservable && Option.isNone colony ->
            // What the reservation adds: every source at the held rate.
            let gain =
                ground.Sources.Length * (Engine.heldOutputPerTick - Engine.neutralOutputPerTick)

            offers.Add(offer (Reserve c.Id) c.Pos (float gain) (Capacity.Bodies 1))
        | _ -> ()

        let starved = colony |> Option.exists (fun c -> c.Bank < c.BankCapacity)

        // The buffer is filled while a body that upgrades stands (or bakes)
        // in the room to drink it.
        let drinks =
            Array.append derived.Bodies derived.Ovens
            |> Array.exists (fun b -> upgrades b && b.Pos.Room = ground.Room)

        // The room the sinks other than the stock have: what a carrier's draw
        // on the stock is for.
        let room =
            (ground.Refills |> Array.sumBy (fun r -> r.Free))
            + (ground.Stores
               |> Array.sumBy (fun s -> if s.Kind = StoreKind.Buffer && drinks then s.Free else 0))

        for s in ground.Stores do
            if s.Energy > 0 && s.Kind <> StoreKind.Buffer then
                let value =
                    match s.Kind with
                    | StoreKind.Remains -> Price.flowDraw (Price.transientLoss s.Energy)
                    | StoreKind.Container when s.SourceDug > 0 ->
                        Price.flowDraw (Price.overflowLoss s.SourceDug s.Fill)
                    | StoreKind.Container
                    | StoreKind.Buffer
                    | StoreKind.Storage -> Price.stockDraw starved hungriest

                let draw, load =
                    match s.Kind with
                    | StoreKind.Storage when room > 0 -> Draw.Open, min s.Energy room
                    | StoreKind.Storage -> Draw.Spenders, s.Energy
                    | _ -> Draw.Open, s.Energy

                offers.Add
                    { offer (Withdraw(s.Id, Energy)) s.Pos value (Capacity.Load load) with
                        Draw = draw
                    }

            if s.Kind = StoreKind.Buffer && s.Free > 0 && drinks then
                offers.Add(
                    offer
                        (Refill(s.Id, Energy))
                        s.Pos
                        (Price.valueOfRank Price.bufferRank)
                        (Capacity.Load s.Free)
                )

            if s.Owned && s.Free > 0 then
                for r in Array.append [| Energy |] ores do
                    offers.Add(
                        offer
                            (Refill(s.Id, r))
                            s.Pos
                            (Price.valueOfRank Price.bottomRank)
                            (Capacity.Load s.Free)
                    )

        for p in ground.Piles do
            if p.Resource = Energy then
                offers.Add(
                    offer
                        (Pickup(p.Id, p.Resource))
                        p.Pos
                        (Price.flowDraw (Price.pileDecay p.Amount))
                        (Capacity.Load p.Amount)
                ))

    for c in derived.Claims do
        offers.Add(offer (Claim c.Id) c.Pos Price.heldRate (Capacity.Bodies 1))

    offers.ToArray()
