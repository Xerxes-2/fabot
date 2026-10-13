/// Knows no Task kinds: what a body may take is `Pool.fits`, what it brings to
/// a capacity `Pool.share`, where a Task ranks its `Priority`.
module Fabot.Core.Economy.Matcher

open Fabot.Core.World
open Fabot.Core.Economy.Derived
open Fabot.Core.Economy.Pool

/// Creep name → `taskId`.
type Assignments = Map<string, string>

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

    // -2 unasked, -1 unreachable or outlived (`Pool.outlives`).
    let walks = Array.create (bodies.Length * offers) -2

    let walk (bi: int) (oi: int) =
        let at = bi * offers + oi

        if walks.[at] = -2 then
            walks.[at] <-
                match travel bodies.[bi] pool.[oi] with
                | Some t when outlives bodies.[bi] pool.[oi] t -> max 0 t
                | _ -> -1

        walks.[at]

    let held = Array.zeroCreate<int> offers
    let holders = Array.zeroCreate<int> offers
    let assigned = Array.create bodies.Length -1

    let seated (oi: int) =
        match pool.[oi].Capacity with
        | Capacity.Seated _ -> true
        | _ -> false

    let garrison (bi: int) (oi: int) = garrisons bodies.[bi] pool.[oi]

    let posted (bi: int) (oi: int) =
        garrison bi oi
        && match pool.[oi].Post with
           | Some post -> Pos.within 0 bodies.[bi].Pos post
           | None -> false

    let ahead (oi: int) (a: int) (b: int) =
        let va = seatValue bodies.[a] pool.[oi] (walk a oi)
        let vb = seatValue bodies.[b] pool.[oi] (walk b oi)
        let pa = posted a oi
        let pb = posted b oi

        (pa && not pb)
        || (pa = pb && va > vb)
        || (pa = pb
            && va = vb
            && (walk a oi < walk b oi
                || (walk a oi = walk b oi
                    && System.String.CompareOrdinal(bodies.[a].Name, bodies.[b].Name) < 0)))

    // So a successor walks out while its predecessor still works.
    let outlasts (holder: int) (bi: int) (oi: int) =
        bodies.[holder].TicksToLive > walk bi oi

    let admitsOver (bi: int) (oi: int) (counts: int -> bool) =
        let mutable seatedAlike = 0
        let mutable sum = 0

        for b in 0 .. bodies.Length - 1 do
            if assigned.[b] = oi && b <> bi && outlasts b bi oi && counts b then
                sum <- sum + share bodies.[b] pool.[oi]

                if garrison b oi = garrison bi oi then
                    seatedAlike <- seatedAlike + 1

        admits pool.[oi] (garrison bi oi) sum seatedAlike

    let admitsNow (bi: int) (oi: int) =
        if seated oi then
            admitsOver bi oi (fun _ -> true)
        else
            admits pool.[oi] false held.[oi] 0

    let admitsAhead (bi: int) (oi: int) =
        admitsOver bi oi (fun b -> ahead oi b bi)

    let take (bi: int) (oi: int) =
        assigned.[bi] <- oi
        held.[oi] <- held.[oi] + share bodies.[bi] pool.[oi]
        holders.[oi] <- holders.[oi] + 1

    let release (bi: int) =
        let oi = assigned.[bi]
        assigned.[bi] <- -1
        held.[oi] <- held.[oi] - share bodies.[bi] pool.[oi]
        holders.[oi] <- holders.[oi] - 1

    // Lightest first keeps whole a set the match admitted while nothing
    // changed; a garrison on its post first, so a newcomer never displaces it.
    let kept = ResizeArray<struct (bool * int * int * int * int)>()

    for bi in 0 .. bodies.Length - 1 do
        match Map.tryFind bodies.[bi].Name assignments with
        | Some tid ->
            match byId.TryGetValue tid with
            | true, oi when fits bodies.[bi] pool.[oi] && walk bi oi >= 0 ->
                let key =
                    if seated oi then
                        -(seatValue bodies.[bi] pool.[oi] (walk bi oi))
                    else
                        share bodies.[bi] pool.[oi]

                kept.Add(struct (not (posted bi oi), key, walk bi oi, bi, oi))
            | _ -> ()
        | None -> ()

    kept.Sort(fun (struct (p1, s1, w1, b1, _)) (struct (p2, s2, w2, b2, _)) ->
        if p1 <> p2 then
            compare p1 p2
        elif s1 <> s2 then
            compare s1 s2
        elif w1 <> w2 then
            compare w1 w2
        else
            System.String.CompareOrdinal(bodies.[b1].Name, bodies.[b2].Name))

    for struct (_, _, _, bi, oi) in kept do
        if admitsNow bi oi then
            take bi oi

    let bestOffer = Array.create bodies.Length -1
    let bestRank = Array.zeroCreate<int> bodies.Length
    let bestWalk = Array.zeroCreate<int> bodies.Length
    let bestLoad = Array.zeroCreate<int> bodies.Length

    let before (r1, w1, l1) (r2, w2, l2) =
        r1 < r2 || (r1 = r2 && (w1 < w2 || (w1 = w2 && l1 < l2)))

    let open' (bi: int) (oi: int) =
        admitsNow bi oi || (seated oi && admitsAhead bi oi)

    // Best rank first, the pool's order within one, so a body walks to no offer
    // ranked below the best it reaches.
    let ranked =
        Array.init offers id
        |> Array.sortWith (fun a b ->
            if pool.[a].Priority <> pool.[b].Priority then
                compare pool.[a].Priority pool.[b].Priority
            else
                compare a b)

    let score (bi: int) =
        bestOffer.[bi] <- -1
        let mutable k = 0

        while k < offers do
            let oi = ranked.[k]
            let offer = pool.[oi]
            let below = bestOffer.[bi] >= 0 && offer.Priority > bestRank.[bi]
            k <- if below then offers else k + 1

            if not below && fits bodies.[bi] offer && open' bi oi then
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

    // So a contested Task goes to the body that reaches it cheapest.
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

            let released = ResizeArray<int>()

            if seated oi then
                let seatedHere =
                    [| 0 .. bodies.Length - 1 |] |> Array.filter (fun b -> assigned.[b] = oi)

                let ordered =
                    seatedHere
                    |> Array.sortWith (fun a b ->
                        if ahead oi a b then -1
                        elif ahead oi b a then 1
                        else 0)

                let admitted = ResizeArray<int>()

                for b in ordered do
                    let over = admitted |> Seq.filter (fun a -> outlasts a b oi) |> Array.ofSeq
                    let sum = over |> Array.sumBy (fun a -> share bodies.[a] pool.[oi])
                    let alike = over |> Array.filter (fun a -> garrison a oi = garrison b oi)

                    if admits pool.[oi] (garrison b oi) sum alike.Length then
                        admitted.Add b
                    else
                        released.Add b

                for b in released do
                    release b

            for bi in 0 .. bodies.Length - 1 do
                if assigned.[bi] < 0 && (bestOffer.[bi] = oi || released.Count > 0) then
                    score bi

    let mutable out = Map.empty

    for bi in 0 .. bodies.Length - 1 do
        if assigned.[bi] >= 0 then
            out <- Map.add bodies.[bi].Name pool.[assigned.[bi]].Id out

    out
