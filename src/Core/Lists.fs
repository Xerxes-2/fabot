/// Fable's `sortBy` and `maxBy` run the projection at every comparison, and
/// v1 measured that live. These keep Fable's answers, ties included.
///
/// No `minBy`: Fable's keeps the last of equal keys and .NET's the first, so
/// one written here would move the bundle or `dotnet test` on a tie.
[<AutoOpen>]
module Fabot.Core.Lists

// The wrappers are inline only to specialise `compare` to the key type.
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
