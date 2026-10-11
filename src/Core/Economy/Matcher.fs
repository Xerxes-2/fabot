/// The matcher: keep each still-valid assignment, then match the rest of the
/// bodies greedily over the world pool. Assignments in, assignments out. It
/// knows no Task kinds: what a body may take is `Pool.fits`, what it brings
/// to a capacity `Pool.share`, where a Task ranks its `Priority`.
module Fabot.Core.Economy.Matcher

open Fabot.Core.Economy.Derived
open Fabot.Core.Economy.Pool

/// Creep name → `taskId` of the Task it holds: the `assignments` store.
type Assignments = Map<string, string>

/// `assignments` after this tick's match. `travel` prices a body's walk to an
/// offer in ticks, `None` where it cannot get there: the Atlas's answer.
///
/// - A body keeps its Task while the Task is offered, the body fits it,
///   reaches it before it dies, and the holders kept before it leave room.
///   Holders are kept in ascending share, then nearest, so a set of holders
///   the match admitted is kept whole while nothing changed; a seated
///   offer's in descending `Pool.seatValue`, so its seats hold the bodies
///   that bring them the most.
/// - The rest are matched greedily: of every unmatched body's best offer by
///   (rank, travel, holders), the smallest pair is matched first, ties to the
///   body's name then the offer's order, so a contested Task goes to the body
///   that reaches it cheapest.
/// - A body is admitted while what the holders before it bring falls short of
///   the capacity (`Pool.admits`); on a seated offer, the holders ahead of it
///   by seat value, and a holder that falls behind a newcomer is released.
///   The result does not depend on the order of `bodies`.
let matchAll
    (travel: Body -> Offer -> int option)
    (pool: Offer[])
    (bodies: Body[])
    (assignments: Assignments)
    : Assignments =
    let offers = pool.Length
    let byId = System.Collections.Generic.Dictionary<string, int>()

    for oi in 0 .. offers - 1 do
        byId.[pool.[oi].Id] <- oi

    // Travel per pair, asked at most once: -2 unasked, -1 unreachable. A
    // body never takes work it dies before reaching.
    let walks = Array.create (bodies.Length * offers) -2

    let walk (bi: int) (oi: int) =
        let at = bi * offers + oi

        if walks.[at] = -2 then
            walks.[at] <-
                match travel bodies.[bi] pool.[oi] with
                | Some t when t < bodies.[bi].TicksToLive -> max 0 t
                | _ -> -1

        walks.[at]

    let held = Array.zeroCreate<int> offers
    let holders = Array.zeroCreate<int> offers
    let garrisoned = Array.zeroCreate<int> offers
    let assigned = Array.create bodies.Length -1

    let seated (oi: int) =
        match pool.[oi].Capacity with
        | Capacity.Seated _ -> true
        | _ -> false

    // Whether body `a` goes ahead of body `b` on a seated offer: what it
    // brings the seat over its stay, then the nearer, then its name.
    let ahead (oi: int) (a: int) (b: int) =
        let va = seatValue bodies.[a] pool.[oi] (walk a oi)
        let vb = seatValue bodies.[b] pool.[oi] (walk b oi)

        va > vb
        || (va = vb
            && (walk a oi < walk b oi
                || (walk a oi = walk b oi
                    && System.String.CompareOrdinal(bodies.[a].Name, bodies.[b].Name) < 0)))

    let garrison (bi: int) (oi: int) = garrisons bodies.[bi] pool.[oi]

    // Whether an offer admits a body among all its holders.
    let admitsNow (bi: int) (oi: int) =
        let kin =
            if garrison bi oi then
                garrisoned.[oi]
            else
                holders.[oi] - garrisoned.[oi]

        admits pool.[oi] (garrison bi oi) held.[oi] kin

    // Whether a seated offer admits a body among the holders ahead of it.
    let admitsAhead (bi: int) (oi: int) =
        let mutable kin = 0
        let mutable sum = 0

        for b in 0 .. bodies.Length - 1 do
            if assigned.[b] = oi && b <> bi && ahead oi b bi then
                sum <- sum + share bodies.[b] pool.[oi]

                if garrison b oi = garrison bi oi then
                    kin <- kin + 1

        admits pool.[oi] (garrison bi oi) sum kin

    let take (bi: int) (oi: int) =
        assigned.[bi] <- oi
        held.[oi] <- held.[oi] + share bodies.[bi] pool.[oi]
        holders.[oi] <- holders.[oi] + 1

        if garrison bi oi then
            garrisoned.[oi] <- garrisoned.[oi] + 1

    let release (bi: int) =
        let oi = assigned.[bi]
        assigned.[bi] <- -1
        held.[oi] <- held.[oi] - share bodies.[bi] pool.[oi]
        holders.[oi] <- holders.[oi] - 1

        if garrison bi oi then
            garrisoned.[oi] <- garrisoned.[oi] - 1

    // The keep: a seated offer keeps its dearest holders, any other its
    // lightest first.
    let kept = ResizeArray<struct (int * int * int * int)>()

    for bi in 0 .. bodies.Length - 1 do
        match Map.tryFind bodies.[bi].Name assignments with
        | Some tid ->
            match byId.TryGetValue tid with
            | true, oi when fits bodies.[bi] pool.[oi].Task && walk bi oi >= 0 ->
                let key =
                    if seated oi then
                        -(seatValue bodies.[bi] pool.[oi] (walk bi oi))
                    else
                        share bodies.[bi] pool.[oi]

                kept.Add(struct (key, walk bi oi, bi, oi))
            | _ -> ()
        | None -> ()

    kept.Sort(fun (struct (s1, w1, b1, _)) (struct (s2, w2, b2, _)) ->
        if s1 <> s2 then
            compare s1 s2
        elif w1 <> w2 then
            compare w1 w2
        else
            System.String.CompareOrdinal(bodies.[b1].Name, bodies.[b2].Name))

    for struct (_, _, bi, oi) in kept do
        if admitsNow bi oi then
            take bi oi

    // The fresh match. Each body's best offer, recomputed only when the
    // offer it was best on gains a holder.
    let bestOffer = Array.create bodies.Length -1
    let bestRank = Array.zeroCreate<int> bodies.Length
    let bestWalk = Array.zeroCreate<int> bodies.Length
    let bestLoad = Array.zeroCreate<int> bodies.Length

    let before (r1, w1, l1) (r2, w2, l2) =
        r1 < r2 || (r1 = r2 && (w1 < w2 || (w1 = w2 && l1 < l2)))

    // Open to the body: room among all the holders, or on a seated offer
    // among those ahead of it.
    let open' (bi: int) (oi: int) =
        admitsNow bi oi || (seated oi && admitsAhead bi oi)

    let score (bi: int) =
        bestOffer.[bi] <- -1

        for oi in 0 .. offers - 1 do
            let offer = pool.[oi]

            if fits bodies.[bi] offer.Task && open' bi oi then
                let w = walk bi oi

                if
                    w >= 0
                    && (bestOffer.[bi] < 0
                        || before
                            (offer.Priority, w, holders.[oi])
                            (bestRank.[bi], bestWalk.[bi], bestLoad.[bi]))
                then
                    bestOffer.[bi] <- oi
                    bestRank.[bi] <- offer.Priority
                    bestWalk.[bi] <- w
                    bestLoad.[bi] <- holders.[oi]

    for bi in 0 .. bodies.Length - 1 do
        if assigned.[bi] < 0 then
            score bi

    // The smallest pair first: (rank, travel, holders), then the body's name.
    let first (a: int) (b: int) =
        let ka = (bestRank.[a], bestWalk.[a], bestLoad.[a])
        let kb = (bestRank.[b], bestWalk.[b], bestLoad.[b])

        before ka kb
        || (ka = kb && System.String.CompareOrdinal(bodies.[a].Name, bodies.[b].Name) < 0)

    let mutable go = true

    while go do
        let mutable pick = -1

        for bi in 0 .. bodies.Length - 1 do
            if assigned.[bi] < 0 && bestOffer.[bi] >= 0 && (pick < 0 || first bi pick) then
                pick <- bi

        if pick < 0 then
            go <- false
        else
            let oi = bestOffer.[pick]
            take pick oi

            // A seat taken by a dearer body releases the holders that no
            // longer fit behind it.
            let released = ResizeArray<int>()

            if seated oi then
                let seatedHere =
                    [| 0 .. bodies.Length - 1 |] |> Array.filter (fun b -> assigned.[b] = oi)

                let ordered =
                    seatedHere |> Array.sortWith (fun a b -> if ahead oi a b then -1 else 1)

                let mutable posted = 0
                let mutable n = 0
                let mutable sum = 0

                for b in ordered do
                    let kin = if garrison b oi then posted else n - posted

                    if admits pool.[oi] (garrison b oi) sum kin then
                        n <- n + 1
                        sum <- sum + share bodies.[b] pool.[oi]

                        if garrison b oi then
                            posted <- posted + 1
                    else
                        released.Add b

                for b in released do
                    release b

            for bi in 0 .. bodies.Length - 1 do
                if assigned.[bi] < 0 && (bestOffer.[bi] = oi || released.Contains bi) then
                    score bi

    let mutable out = Map.empty

    for bi in 0 .. bodies.Length - 1 do
        if assigned.[bi] >= 0 then
            out <- Map.add bodies.[bi].Name pool.[assigned.[bi]].Id out

    out
