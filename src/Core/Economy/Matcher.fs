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
///   the match admitted is kept whole while nothing changed.
/// - The rest are matched greedily: of every unmatched body's best offer by
///   (rank, travel, holders), the smallest pair is matched first, ties to the
///   body's then the offer's order, so a contested Task goes to the body that
///   reaches it cheapest.
/// - A body is admitted while what the holders before it bring falls short of
///   the capacity (`Pool.admits`).
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
    let assigned = Array.create bodies.Length -1

    let take (bi: int) (oi: int) =
        assigned.[bi] <- oi
        held.[oi] <- held.[oi] + share bodies.[bi] pool.[oi]
        holders.[oi] <- holders.[oi] + 1

    // The keep.
    let kept = ResizeArray<struct (int * int * int * int)>()

    for bi in 0 .. bodies.Length - 1 do
        match Map.tryFind bodies.[bi].Name assignments with
        | Some tid ->
            match byId.TryGetValue tid with
            | true, oi when fits bodies.[bi] pool.[oi].Task && walk bi oi >= 0 ->
                kept.Add(struct (share bodies.[bi] pool.[oi], walk bi oi, bi, oi))
            | _ -> ()
        | None -> ()

    kept.Sort(fun (struct (s1, w1, b1, _)) (struct (s2, w2, b2, _)) ->
        if s1 <> s2 then compare s1 s2
        elif w1 <> w2 then compare w1 w2
        else compare bodies.[b1].Name bodies.[b2].Name)

    for struct (_, _, bi, oi) in kept do
        if admits pool.[oi] held.[oi] then
            take bi oi

    // The fresh match. Each body's best offer, recomputed only when the
    // offer it was best on gains a holder.
    let bestOffer = Array.create bodies.Length -1
    let bestRank = Array.zeroCreate<int> bodies.Length
    let bestWalk = Array.zeroCreate<int> bodies.Length
    let bestLoad = Array.zeroCreate<int> bodies.Length

    let before (r1, w1, l1) (r2, w2, l2) =
        r1 < r2 || (r1 = r2 && (w1 < w2 || (w1 = w2 && l1 < l2)))

    let score (bi: int) =
        bestOffer.[bi] <- -1

        for oi in 0 .. offers - 1 do
            let offer = pool.[oi]

            if fits bodies.[bi] offer.Task && admits offer held.[oi] then
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

    let mutable go = true

    while go do
        let mutable pick = -1

        for bi in 0 .. bodies.Length - 1 do
            if
                assigned.[bi] < 0
                && bestOffer.[bi] >= 0
                && (pick < 0
                    || before
                        (bestRank.[bi], bestWalk.[bi], bestLoad.[bi])
                        (bestRank.[pick], bestWalk.[pick], bestLoad.[pick]))
            then
                pick <- bi

        if pick < 0 then
            go <- false
        else
            let oi = bestOffer.[pick]
            take pick oi

            for bi in 0 .. bodies.Length - 1 do
                if assigned.[bi] < 0 && bestOffer.[bi] = oi then
                    score bi

    let mutable out = Map.empty

    for bi in 0 .. bodies.Length - 1 do
        if assigned.[bi] >= 0 then
            out <- Map.add bodies.[bi].Name pool.[assigned.[bi]].Id out

    out
