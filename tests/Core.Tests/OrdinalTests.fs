module Fabot.Core.Tests.OrdinalTests

open Expecto
open Microsoft.FSharp.Reflection
open Fabot.Core.Types

// The hand-written equality and order on the vocabulary unions read one int
// per value. It must ascend as the default structural order ran — case by
// declaration, then the value inside — and never give two values one int,
// or a Map over them reorders and a census merges two kinds.

// Every value of a union, in its default structural order: each case by
// declaration, a case with a field once per value of the field's type.
let rec private valuesOf (t: System.Type) : obj list =
    FSharpType.GetUnionCases t
    |> Array.toList
    |> List.collect (fun case ->
        match case.GetFields() with
        | [||] -> [ FSharpValue.MakeUnion(case, [||]) ]
        | [| field |] ->
            valuesOf field.PropertyType
            |> List.map (fun inner -> FSharpValue.MakeUnion(case, [| inner |]))
        | _ -> failwith $"{t.Name}.{case.Name}: one field at most")

let private ordered<'T when 'T: comparison> (ordinal: 'T -> int) =
    let values = valuesOf typeof<'T> |> List.map unbox<'T>
    let ordinals = values |> List.map ordinal

    Expect.equal (List.distinct ordinals) ordinals $"{typeof<'T>.Name}: one int per value"
    Expect.equal (List.sort ordinals) ordinals $"{typeof<'T>.Name}: ascending as declared"

    // Listed in structural order, so a value's place is what it compares by.
    for i, a in List.indexed values do
        for j, b in List.indexed values do
            Expect.equal (a = b) (i = j) $"{a} = {b}"
            Expect.equal (compare a b |> sign) (compare i j |> sign) $"compare {a} {b}"
            Expect.equal (a = b) (hash a = hash b) $"hash {a} {b}"

[<Tests>]
let ordinalTests =
    testList
        "vocabulary ordinals"
        [
            test "BodyPart" { ordered (fun (p: BodyPart) -> p.Ordinal) }
            test "Ore" { ordered (fun (o: Ore) -> o.Ordinal) }
            test "Compound" { ordered (fun (c: Compound) -> c.Ordinal) }
            test "Resource" { ordered (fun (r: Resource) -> r.Ordinal) }
            test "BuiltKind" { ordered (fun (k: BuiltKind) -> k.Ordinal) }
            test "TargetKind" { ordered (fun (k: TargetKind) -> k.Ordinal) }
        ]
