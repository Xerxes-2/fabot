/// One pool for every colony: a body takes the work it fits and reaches,
/// whoever cast it (`ARCHITECTURE.md` §5).
module Fabot.Core.Economy.Pool

open Fabot.Core
open Fabot.Core.Vocabulary
open Fabot.Core.World
open Fabot.Core.Economy.Task
open Fabot.Core.Economy.Bodies
open Fabot.Core.Economy.Derived

[<RequireQualifiedAccess>]
type Capacity =
    | Unbounded
    | Bodies of int
    | Work of int
    /// `posts` are the garrisons' seats, so a light body never holds the seat
    /// a harvester digs into a container from.
    | Seated of seats: int * posts: int * work: int
    | Load of int

[<RequireQualifiedAccess>]
type Draw =
    | Open
    /// The stock while no other sink is hungry, which a carrier would only
    /// pour back.
    | Spenders
    /// The buffer is drawn nowhere else, so what it holds goes to the
    /// controller or to construction, never back to a refill.
    | Buffer

[<NoEquality; NoComparison>]
type Offer =
    {
        Task: Task
        Id: string
        Target: Pos
        /// Lower first.
        Priority: int
        Capacity: Capacity
        Draw: Draw
        /// The tile a seated offer's garrison digs from.
        Post: Pos option
        /// Rooms a body may not take it from: another colony's, whose own
        /// stock fills it.
        Shut: Set<string>
    }

let private carried (body: Body) (resource: Resource) = Store.amountOf resource body.Store

let garrisons (body: Body) (offer: Offer) : bool =
    match offer.Capacity with
    | Capacity.Seated(_, posts, _) -> posts > 0 && body.Pattern.IsHarvester
    | _ -> false

let upgrades (body: Body) : bool =
    not body.Pattern.IsHarvester && body.Work > 0 && body.Carry > 0

/// The engine withdraws before it upgrades, so an Upgrade's draw tops up
/// what the upgrade spends.
let drawsBuffer (body: Body) (offer: Offer) : bool =
    offer.Draw.IsBuffer
    && body.Free > 0
    && match offer.Task with
       | Upgrade _ -> body.Energy < body.Work * Engine.upgradeDrainPerWork
       | _ -> body.Energy = 0

/// One rule for a fresh match and a kept one. A harvester digs whatever it
/// holds, the engine dropping the overflow; an upgrader's delivery is a
/// commute for its WORK.
let fits (body: Body) (offer: Offer) : bool =
    let halfEmpty = body.Free > 0 && body.Free * 2 >= body.Store.Capacity

    match offer.Task with
    // An upgrader is cast by, and counts for, the room it stands in, so it
    // waits out a dry buffer there rather than walk to another's controller.
    | Upgrade _ when body.Pattern.IsUpgrader -> body.Pos.Room = offer.Target.Room && upgrades body
    | Upgrade _ -> upgrades body && (body.Energy > 0 || offer.Draw.IsBuffer)
    | _ when body.Pattern.IsUpgrader -> false
    | Harvest _ ->
        body.Work > 0
        && (garrisons body offer
            || not body.Pattern.IsHarvester
               && (halfEmpty
                   || body.Free > 0 && Pos.within (rangeOf offer.Task) body.Pos offer.Target))
    | _ when body.Pattern.IsHarvester -> false
    | Withdraw _ when not offer.Draw.IsOpen && body.Work = 0 -> false
    | Withdraw _
    | Pickup _ -> body.Carry > 0 && halfEmpty
    | Refill(_, resource) ->
        body.Carry > 0
        && carried body resource > 0
        && not (offer.Shut.Contains body.Pos.Room)
    | Build _
    | Repair _ -> body.Work > 0 && body.Carry > 0 && (body.Energy > 0 || offer.Draw.IsBuffer)
    | Reserve _
    | Claim _ -> body.Claim > 0

/// The walk back is read as the walk there: a load's sink is not known when
/// it is matched.
let outlives (body: Body) (offer: Offer) (walk: int) : bool =
    let legs =
        match offer.Task with
        | Withdraw _
        | Pickup _ -> 2
        | _ -> 1

    legs * walk < body.TicksToLive

/// Only a post's garrison counts WORK, so the other seats stay open while it
/// digs short of the work.
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
        | _ when drawsBuffer body offer -> body.Free
        | _ -> body.Energy

let admits (offer: Offer) (garrison: bool) (held: int) (seatedAlike: int) : bool =
    match offer.Capacity with
    | Capacity.Unbounded -> true
    | Capacity.Bodies n
    | Capacity.Work n
    | Capacity.Load n -> held < n
    | Capacity.Seated(seats, posts, work) ->
        held < work && seatedAlike < (if garrison then posts else seats - posts)

/// A dig counts what the body already dug, so a digger part-way through its
/// load is worth no less than an empty one.
let seatValue (body: Body) (offer: Offer) (walk: int) : int =
    match offer.Capacity with
    | Capacity.Seated _ ->
        let stay = max 0 (body.TicksToLive - walk)

        match offer.Task with
        | Harvest _ ->
            let dig = body.Work * Engine.harvestPerWork * stay

            if body.Pattern.IsHarvester then
                dig
            else
                min (body.Free + body.Energy) dig
        | Upgrade _ ->
            let upgrade = body.Work * Engine.upgradeDrainPerWork * stay

            if offer.Draw.IsBuffer then
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
        Post = None
        Shut = Set.empty
    }

let private insideDeadline (c: Controller) =
    c.Level > 0 && c.TicksToDowngrade < Engine.safeModeDowngradeLine c.Level

/// Every site alike: their order is Layout's plan order.
let siteRank (_: Derived) (_: SiteFact) : int = Price.surplusRank

/// A rampart under `rampartSafe` is priced as the flow, so a fresh one is
/// not lost. A structure that outlives a creep's life loses nothing while it
/// waits for the next, so it waits behind the sites.
let repairValue (r: RepairFact) : float =
    match r.Kind with
    | Kind.Rampart when r.Hits <= rampartSafe -> Price.flowDraw 0.0
    | Kind.Rampart -> Price.valueOfRank (Price.surplusRank + 1)
    | _ when float r.Hits < r.Decay * float Engine.creepLifetime ->
        Price.rebuildAtRisk r.Rebuild r.Hits r.Decay
    | _ -> Price.valueOfRank (Price.surplusRank + 1)

let pool (derived: Derived) : Offer[] =
    let offers = ResizeArray<Offer>()

    // The storage is offered for every ore a body carries, so no load is
    // stranded.
    let ores =
        let seen = System.Collections.Generic.HashSet<string>()

        derived.Bodies
        |> Array.collect (fun b -> b.Store.Held |> Array.map fst)
        |> Array.filter (fun r -> not r.IsEnergy && seen.Add(resourceName r))

    // A tick a spawn waits is a tick of the income bodies earn, and every
    // colony's alike: travel picks the ring, so a richer room's never draws a
    // body past its own.
    let ringWorth =
        let n = derived.Colonies.Length

        float (derived.Grounds |> Array.truncate n |> Array.sumBy (fun g -> g.Income))
        / float (max 1 n)

    let colonyRooms = derived.Colonies |> Array.map (fun c -> c.Room) |> Set.ofArray

    derived.Grounds
    |> Array.iteri (fun i ground ->
        let colony =
            if i < derived.Colonies.Length then
                Some derived.Colonies.[i]
            else
                None

        // Offered through a regeneration gap too, so its holders keep their
        // seats. Priced at the rate, the engine's cap on what its seats dig.
        // Away from a colony only at a post: a worker never walks out to dig.
        for s in ground.Sources do
            if Option.isSome colony || Option.isSome s.Post then
                offers.Add(
                    { offer
                          (Harvest s.Id)
                          s.Pos
                          (float s.Rate)
                          (Capacity.Seated(
                              Option.defaultValue System.Int32.MaxValue s.Seats,
                              (if Option.isSome s.Post then 1 else 0),
                              (s.Rate + Engine.harvestPerWork - 1) / Engine.harvestPerWork
                          )) with
                        Post = s.Post
                    }
                )

        let mutable hungriest = 0.0

        let ringFree = ground.Refills |> Array.sumBy (fun r -> r.Free)

        let shutTo (free: int) =
            if
                Option.isSome colony
                && ground.Stores |> Array.exists (fun s -> s.Owned && s.Energy >= free)
            then
                Set.remove ground.Room colonyRooms
            else
                Set.empty

        for r in ground.Refills do
            let value =
                if r.Kind.IsTower then
                    Price.towerShort Price.homeRaidPrior r.Free
                else
                    ringWorth

            hungriest <- max hungriest value

            offers.Add
                { offer (Refill(r.Id, Energy)) r.Pos value (Capacity.Load r.Free) with
                    Shut = shutTo ringFree
                }

        let draw =
            if ground.Buffer |> Option.exists (fun b -> b.Energy > 0) then
                Draw.Buffer
            else
                Draw.Open

        for s in ground.Sites do
            offers.Add
                { offer
                      (Build s.Id)
                      s.Pos
                      (Price.valueOfRank (siteRank derived s))
                      (Capacity.Load s.Left) with
                    Draw = draw
                }

        for r in ground.Repairs do
            let missing =
                (r.HitsMax - r.Hits + Engine.repairHitsPerEnergy - 1)
                / Engine.repairHitsPerEnergy

            offers.Add
                { offer (Repair r.Id) r.Pos (repairValue r) (Capacity.Load missing) with
                    Draw = draw
                }

        match ground.Controller with
        | Some c when c.Owner.IsOurs ->
            let capacity =
                Capacity.Seated(
                    Option.defaultValue System.Int32.MaxValue ground.UpgradeSeats,
                    0,
                    upgradableAt c.Level
                )

            let upgrade =
                { offer (Upgrade c.Id) c.Pos (Price.valueOfRank Price.upgradeRank) capacity with
                    Draw = draw
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
            offers.Add(offer (Reserve c.Id) c.Pos (float (reserveGain ground)) (Capacity.Bodies 1))
        | _ -> ()

        let starved = colony |> Option.exists (fun c -> c.Bank < c.BankCapacity)

        let drinks =
            Array.append derived.Bodies derived.Ovens
            |> Array.exists (fun b -> upgrades b && b.Pos.Room = ground.Room)

        let feed = ground.Feed |> Option.filter (fun f -> f.Free > 0 && drinks)

        // An upgrader waiting on its buffer loses a tick of what the row hired
        // it at.
        let feedWorth =
            match feed, ground.Buffer, colony with
            | Some _, Some buffer, Some c ->
                let upgradable = upgradableAt c.Controller.Level

                let upgraders =
                    derived.Bodies
                    |> Array.sumBy (fun b ->
                        if b.Pattern.IsUpgrader && b.Pos.Room = ground.Room then
                            1
                        else
                            0)

                sized Pattern.Upgrader upgradable c.BankCapacity
                |> Option.map (fun body ->
                    upgraderHire upgradable upgraders body * (1.0 - buffer.Fill))
                |> Option.defaultValue 0.0
            | _ -> 0.0

        hungriest <- max hungriest feedWorth

        let room =
            ringFree + (feed |> Option.map (fun f -> f.Free) |> Option.defaultValue 0)

        for s in ground.Stores do
            if s.Energy > 0 && not s.Kind.IsBuffer then
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

            if s.Owned && s.Free > 0 then
                for r in Array.append [| Energy |] ores do
                    offers.Add(
                        offer
                            (Refill(s.Id, r))
                            s.Pos
                            (Price.valueOfRank Price.bottomRank)
                            (Capacity.Load s.Free)
                    )

        for f in Option.toArray feed do
            offers.Add
                { offer
                      (Refill(f.Id, Energy))
                      f.Pos
                      (max (Price.valueOfRank Price.bufferRank) feedWorth)
                      (Capacity.Load f.Free) with
                    Shut = shutTo room
                }

        for p in ground.Piles do
            if p.Resource.IsEnergy then
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
