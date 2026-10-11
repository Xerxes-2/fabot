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
    | Seated of seats: int * work: int
    /// Energy: an intake counts its holders' free room, a delivery what they
    /// carry of the resource.
    | Load of int

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
    }

/// What a body carries of a resource.
let private carried (body: Body) (resource: Resource) = Store.amountOf resource body.Store

/// Whether a body's parts and store let it act on the Task now. One rule for
/// a fresh match and a kept one: an intake while the store has room, a
/// delivery while it holds the resource. A harvester digs whatever its store
/// holds, the engine dropping the overflow where it stands, and takes nothing
/// else: its seat is the work it was cast for, its CARRY the dig's buffer.
let fits (body: Body) (task: Task) : bool =
    match task with
    | Harvest _ -> body.Work > 0 && (body.Pattern = Pattern.Harvester || body.Free > 0)
    | _ when body.Pattern = Pattern.Harvester -> false
    | Withdraw _
    | Pickup _ -> body.Carry > 0 && body.Free > 0
    | Refill(_, resource) -> body.Carry > 0 && carried body resource > 0
    | Build _
    | Repair _
    | Upgrade _ -> body.Work > 0 && body.Carry > 0 && body.Energy > 0
    | Reserve _
    | Claim _ -> body.Claim > 0

/// What one holder counts against an offer's capacity.
let share (body: Body) (offer: Offer) : int =
    match offer.Capacity with
    | Capacity.Unbounded -> 0
    | Capacity.Bodies _ -> 1
    | Capacity.Work _
    | Capacity.Seated _ -> body.Work
    | Capacity.Load _ ->
        match offer.Task with
        | Refill(_, resource) -> carried body resource
        | task when isIntake task -> body.Free
        | _ -> body.Energy

/// Whether an offer admits one more holder while it has `holders` who bring
/// `held` (`share` summed).
let admits (offer: Offer) (held: int) (holders: int) : bool =
    match offer.Capacity with
    | Capacity.Unbounded -> true
    | Capacity.Bodies n
    | Capacity.Work n
    | Capacity.Load n -> held < n
    | Capacity.Seated(seats, work) -> holders < seats && held < work

/// What a body brings a seated offer over its stay, `walk` ticks away: the
/// energy it digs or upgrades before it dies, or before its store fills (a
/// dig into its store) or empties (an upgrade). 0 for any other offer.
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
                min body.Free dig
        | Upgrade _ -> min body.Energy (body.Work * Engine.upgradeDrainPerWork * stay)
        | _ -> body.Work * stay
    | _ -> 0

let private offer task (target: Pos) (value: float) capacity =
    {
        Task = task
        Id = taskId task
        Target = target
        Priority = Price.economyRank value
        Capacity = capacity
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
        // seats and wait to dig.
        for s in ground.Sources do
            offers.Add(
                offer
                    (Harvest s.Id)
                    s.Pos
                    (float s.Rate)
                    (Capacity.Seated(
                        s.Seats,
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
                    if c.Level >= Engine.controllerMaxLevel then
                        Engine.maxUpgradePerTick
                    else
                        System.Int32.MaxValue
                )

            let upgrade =
                offer (Upgrade c.Id) c.Pos (Price.valueOfRank Price.upgradeRank) capacity

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

        for s in ground.Stores do
            if s.Energy > 0 then
                let value =
                    match s.Kind with
                    | StoreKind.Remains -> Price.flowDraw (Price.transientLoss s.Energy)
                    | StoreKind.Container when s.SourceRate > 0 ->
                        Price.flowDraw (Price.overflowLoss s.SourceRate s.Fill)
                    | StoreKind.Container
                    | StoreKind.Storage -> Price.stockDraw starved hungriest

                offers.Add(offer (Withdraw(s.Id, Energy)) s.Pos value (Capacity.Load s.Energy))

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
