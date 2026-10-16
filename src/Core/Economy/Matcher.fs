/// Knows no Task kinds: what a body may take is `Pool.fits`, what it brings to
/// a capacity `Pool.share`, where a Task ranks its `Priority` and walk.
module Fabot.Core.Economy.Matcher

open Fabot.Core
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
        byId.[(Tiles.at oi pool).Id] <- oi

    // -2 unasked, -1 unreachable or outlived (`Pool.outlives`).
    let walks = Array.create (bodies.Length * offers) -2

    let walk (bi: int) (oi: int) =
        let at = bi * offers + oi

        if Tiles.at at walks = -2 then
            Tiles.setAt
                at
                walks
                (match travel (Tiles.at bi bodies) (Tiles.at oi pool) with
                 | Some t when outlives (Tiles.at bi bodies) (Tiles.at oi pool) t -> max 0 t
                 | _ -> -1)

        Tiles.at at walks

    let held = Array.zeroCreate<int> offers
    let holders = Array.zeroCreate<int> offers
    let assigned = Array.create bodies.Length -1

    let seated (oi: int) =
        match (Tiles.at oi pool).Capacity with
        | Capacity.Seated _ -> true
        | _ -> false

    let garrison (bi: int) (oi: int) =
        garrisons (Tiles.at bi bodies) (Tiles.at oi pool)

    let posted (bi: int) (oi: int) =
        garrison bi oi
        && match (Tiles.at oi pool).Post with
           | Some post -> Pos.within 0 (Tiles.at bi bodies).Pos post
           | None -> false

    let ahead (oi: int) (a: int) (b: int) =
        let va = seatValue (Tiles.at a bodies) (Tiles.at oi pool) (walk a oi)
        let vb = seatValue (Tiles.at b bodies) (Tiles.at oi pool) (walk b oi)
        let pa = posted a oi
        let pb = posted b oi

        (pa && not pb)
        || (pa = pb && va > vb)
        || (pa = pb
            && va = vb
            && (walk a oi < walk b oi
                || (walk a oi = walk b oi
                    && System.String.CompareOrdinal(
                        (Tiles.at a bodies).Name,
                        (Tiles.at b bodies).Name
                    ) < 0)))

    // So a successor walks out while its predecessor still works.
    let outlasts (holder: int) (bi: int) (oi: int) =
        (Tiles.at holder bodies).TicksToLive > walk bi oi

    let admitsOver (bi: int) (oi: int) (counts: int -> bool) =
        let mutable seatedAlike = 0
        let mutable sum = 0

        for b in 0 .. bodies.Length - 1 do
            if Tiles.at b assigned = oi && b <> bi && outlasts b bi oi && counts b then
                sum <- sum + share (Tiles.at b bodies) (Tiles.at oi pool)

                if garrison b oi = garrison bi oi then
                    seatedAlike <- seatedAlike + 1

        admits (Tiles.at oi pool) (garrison bi oi) sum seatedAlike

    let admitsNow (bi: int) (oi: int) =
        if seated oi then
            admitsOver bi oi (fun _ -> true)
        else
            admits (Tiles.at oi pool) false (Tiles.at oi held) 0

    let admitsAhead (bi: int) (oi: int) =
        admitsOver bi oi (fun b -> ahead oi b bi)

    let take (bi: int) (oi: int) =
        Tiles.setAt bi assigned oi
        Tiles.setAt oi held (Tiles.at oi held + share (Tiles.at bi bodies) (Tiles.at oi pool))
        Tiles.setAt oi holders (Tiles.at oi holders + 1)

    let release (bi: int) =
        let oi = Tiles.at bi assigned
        Tiles.setAt bi assigned -1
        Tiles.setAt oi held (Tiles.at oi held - share (Tiles.at bi bodies) (Tiles.at oi pool))
        Tiles.setAt oi holders (Tiles.at oi holders - 1)

    // Lightest first keeps whole a set the match admitted while nothing
    // changed; a garrison on its post first, so a newcomer never displaces it.
    let kept = ResizeArray<struct (bool * int * int * int * int)>()

    for bi in 0 .. bodies.Length - 1 do
        match Map.tryFind (Tiles.at bi bodies).Name assignments with
        | Some tid ->
            match byId.TryGetValue tid with
            | true, oi when fits (Tiles.at bi bodies) (Tiles.at oi pool) && walk bi oi >= 0 ->
                let key =
                    if seated oi then
                        -(seatValue (Tiles.at bi bodies) (Tiles.at oi pool) (walk bi oi))
                    else
                        share (Tiles.at bi bodies) (Tiles.at oi pool)

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
            System.String.CompareOrdinal((Tiles.at b1 bodies).Name, (Tiles.at b2 bodies).Name))

    for struct (_, _, _, bi, oi) in kept do
        if admitsNow bi oi then
            take bi oi

    let bestOffer = Array.create bodies.Length -1
    let bestCost = Array.zeroCreate<int> bodies.Length
    let bestWalk = Array.zeroCreate<int> bodies.Length
    let bestLoad = Array.zeroCreate<int> bodies.Length

    let before (r1, w1, l1) (r2, w2, l2) =
        r1 < r2 || (r1 = r2 && (w1 < w2 || (w1 = w2 && l1 < l2)))

    let open' (bi: int) (oi: int) =
        admitsNow bi oi || (seated oi && admitsAhead bi oi)

    // Best rank first, the pool's order within one, so no offer is walked
    // whose rank alone costs more than the best reached.
    let ranked =
        Array.init offers id
        |> Array.sortWith (fun a b ->
            if (Tiles.at a pool).Priority <> (Tiles.at b pool).Priority then
                compare (Tiles.at a pool).Priority (Tiles.at b pool).Priority
            else
                compare a b)

    let score (bi: int) =
        Tiles.setAt bi bestOffer -1
        let mutable k = 0

        while k < offers do
            let oi = Tiles.at k ranked
            let offer = Tiles.at oi pool

            let below =
                Tiles.at bi bestOffer >= 0
                && Price.walked offer.Priority 0 > Tiles.at bi bestCost

            k <- if below then offers else k + 1

            if not below && fits (Tiles.at bi bodies) offer && open' bi oi then
                let w = walk bi oi

                if
                    w >= 0
                    && (Tiles.at bi bestOffer < 0
                        || before
                            (Price.walked offer.Priority w, w, Tiles.at oi holders)
                            (Tiles.at bi bestCost, Tiles.at bi bestWalk, Tiles.at bi bestLoad))
                then
                    Tiles.setAt bi bestOffer oi
                    Tiles.setAt bi bestCost (Price.walked offer.Priority w)
                    Tiles.setAt bi bestWalk w
                    Tiles.setAt bi bestLoad (Tiles.at oi holders)

    for bi in 0 .. bodies.Length - 1 do
        if Tiles.at bi assigned < 0 then
            score bi

    // So a contested Task goes to the body that reaches it cheapest.
    let first (a: int) (b: int) =
        let ka = (Tiles.at a bestCost, Tiles.at a bestWalk, Tiles.at a bestLoad)
        let kb = (Tiles.at b bestCost, Tiles.at b bestWalk, Tiles.at b bestLoad)

        before ka kb
        || (ka = kb
            && System.String.CompareOrdinal((Tiles.at a bodies).Name, (Tiles.at b bodies).Name) < 0)

    let mutable go = true

    while go do
        let mutable pick = -1

        for bi in 0 .. bodies.Length - 1 do
            if
                Tiles.at bi assigned < 0
                && Tiles.at bi bestOffer >= 0
                && (pick < 0 || first bi pick)
            then
                pick <- bi

        if pick < 0 then
            go <- false
        else
            let oi = Tiles.at pick bestOffer
            take pick oi

            let released = ResizeArray<int>()

            if seated oi then
                let seatedHere =
                    [| 0 .. bodies.Length - 1 |] |> Array.filter (fun b -> Tiles.at b assigned = oi)

                let ordered =
                    seatedHere
                    |> Array.sortWith (fun a b ->
                        if ahead oi a b then -1
                        elif ahead oi b a then 1
                        else 0)

                let admitted = ResizeArray<int>()

                for b in ordered do
                    let over = admitted |> Seq.filter (fun a -> outlasts a b oi) |> Array.ofSeq

                    let sum =
                        over |> Array.sumBy (fun a -> share (Tiles.at a bodies) (Tiles.at oi pool))

                    let alike = over |> Array.filter (fun a -> garrison a oi = garrison b oi)

                    if admits (Tiles.at oi pool) (garrison b oi) sum alike.Length then
                        admitted.Add b
                    else
                        released.Add b

                for b in released do
                    release b

            for bi in 0 .. bodies.Length - 1 do
                if
                    Tiles.at bi assigned < 0 && (Tiles.at bi bestOffer = oi || released.Count > 0)
                then
                    score bi

    let mutable out = Map.empty

    for bi in 0 .. bodies.Length - 1 do
        if Tiles.at bi assigned >= 0 then
            out <- Map.add (Tiles.at bi bodies).Name (Tiles.at (Tiles.at bi assigned) pool).Id out

    out
