/// `List.sortBy`, `sortByDescending` and `maxBy` with the key taken once an
/// element. Fable's own ask the projection of both sides at every
/// comparison, so a sort runs it O(n log n) times and a `maxBy` twice an
/// element: a cast's price (`Facts.castsByPrice`) ran a kill clock per
/// comparison. Same answers as Fable's, ties included: the sorts are stable,
/// and `maxBy` keeps the first of equal keys by `compare`, as it did.
/// Opened with the namespace, so every `List.sortBy` below resolves here.
///
/// No `minBy`: Fable's keeps the **last** of equal keys where .NET's keeps
/// the first, so the bundle and `dotnet test` already part on a tie there,
/// and one written here would move one of them.
[<AutoOpen>]
module Fabot.Core.Types.Lists

// The work, once: the wrappers below are inline only to hand it the
// comparison their key type specialises, so a call site costs one call.
let sortedByKey (projection: 'T -> 'Key) (order: 'Key -> 'Key -> int) (list: 'T list) : 'T list =
    match list with
    | []
    | [ _ ] -> list
    | _ ->
        // `List.sortWith` is stable, as the `sortBy` it stands in for was.
        list
        |> List.map (fun item -> struct (projection item, item))
        |> List.sortWith (fun (struct (a, _)) (struct (b, _)) -> order a b)
        |> List.map (fun (struct (_, item)) -> item)

let greatestByKey (projection: 'T -> 'Key) (order: 'Key -> 'Key -> int) (list: 'T list) : 'T =
    match list with
    | [] -> invalidArg "list" "The input list was empty."
    | first :: rest ->
        let mutable best = first
        let mutable bestKey = projection first

        for item in rest do
            let key = projection item

            if order key bestKey > 0 then
                best <- item
                bestKey <- key

        best

[<RequireQualifiedAccess>]
module List =
    let inline sortBy (projection: 'T -> 'Key) (list: 'T list) : 'T list =
        sortedByKey projection (fun a b -> compare a b) list

    let inline sortByDescending (projection: 'T -> 'Key) (list: 'T list) : 'T list =
        sortedByKey projection (fun a b -> compare b a) list

    let inline maxBy (projection: 'T -> 'Key) (list: 'T list) : 'T =
        greatestByKey projection (fun a b -> compare a b) list
