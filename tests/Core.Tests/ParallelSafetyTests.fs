/// What keeps `dotnet test` a gate rather than a coin toss (#310).
///
/// Expecto runs this suite's tests in parallel, and an `Atlas` is a
/// single-threaded, per-tick value by construction: it memoises onto mutable
/// `Dictionary` tables (`SeamWalks`, `Routes`, `FarFields`, `WorkAreas`,
/// `HeavyAreas`, `Walks`) and hands out resumable `Flood`s that every reader
/// pushes further out. Production never notices — the bundle is
/// single-threaded JS — but a fixture that hands *one* Atlas to two tests is
/// two threads writing one Dictionary, and what comes back is a wrong number
/// rather than a crash — plus, when the tear lands mid-resize, the
/// `NullReferenceException` out of `Atlas.memoised` the issue also records.
/// That is what made three tests of `atlas multi-hop corner` and one list of
/// `layout invariants on real terrain` red in roughly one run in ten for
/// reasons no diff could explain.
///
/// A module-level `let` binding of a value is where such sharing comes from,
/// because F# compiles it to a static field initialised once for the process.
/// So the rule is stated over the *shape* rather than over the fixtures that
/// broke: no static value in this assembly may carry an Atlas, a `WalkTable`,
/// or a mutable container a memo could be rolled out of by hand — an Atlas
/// fixture is a function, and every test builds its own. A function compiles
/// to a method and is not flagged. The rule is written where a
/// fixture author reads it, in `AGENTS.md` § Code hygiene and in
/// `docs/agents/orchestration.md` § Where a new Decide test goes; this file is
/// only where it is enforced.
///
/// Every static is judged **twice**: on the type it declares, and on the type
/// its value turns out to have. The second check is the load-bearing one,
/// because "make it a function" invites exactly the shape that erases the
/// declared type —
///
/// ```fsharp
/// let private priceFromCorner: string -> int option =
///     let atlas = cornerOfFour [ … ]
///     fun target -> travelCost atlas "w" (Harvest target)
/// ```
///
/// — where `FSharpFunc<string, int option>` names no Atlas and lives in
/// FSharp.Core, so nothing about the declared type is suspicious while every
/// caller shares one Atlas. The closure's *runtime* class is ours and holds
/// the Atlas in a field, so it is flagged. `obj`, an interface and a delegate
/// (whose `Target` is followed for the same reason) erase it the same way and
/// are caught the same way.
///
/// Reflection lives in this project alone — Core's tables stay plain data, so
/// none of this reaches the Fable bundle (`WireTests` states the rule; this is
/// the second of its two sites).
module Fabot.Core.Tests.ParallelSafetyTests

open System
open System.Reflection
open Expecto

/// The shared-mutable roots that are *named* rather than recognised by shape.
/// `Atlas` reaches its own tables and the resumable `Flood`s it hands out
/// through its own fields, so those need no entry of their own — `Flood` could
/// not have one anyway: it is `internal` to Core with no `InternalsVisibleTo`,
/// so `typeof<_>` cannot spell it from here. This list is therefore not a
/// census of what is shared, but the part of it a name can reach. `WalkTable`
/// is named beside `Atlas` because the plan memo carries one without an Atlas
/// around it, and a memo held as a fixture would share it the same way.
let private mutableRoots =
    [ typeof<Fabot.Core.Atlas.Atlas>; typeof<Fabot.Core.Types.Intents.WalkTable> ]

/// The roots recognised by shape instead: the mutable containers a fixture
/// rolls a memo out of by hand, which no list of names could anticipate. Every
/// one of them is a hazard for the same reason the Atlas's tables are — two of
/// Expecto's parallel tests writing one of these is a torn read — and a
/// fixture that wants a shared table across tests wants `Lazy` over an
/// immutable one, or a `ConcurrentDictionary` it has argued for.
let private mutableShapes =
    [
        typedefof<System.Collections.Generic.Dictionary<_, _>>
        typedefof<System.Collections.Generic.HashSet<_>>
        typedefof<System.Collections.Generic.List<_>>
        typedefof<Ref<_>>
    ]

let private ours (t: Type) =
    t.Assembly = typeof<Fabot.Core.Atlas.Atlas>.Assembly
    || t.Assembly = Assembly.GetExecutingAssembly()

/// Whether a type reaches one of the roots — through a generic argument (a
/// list, a `Lazy`, a tuple, a `Map`'s value), an element type, or a field of
/// one of our own types, closure classes included. Depth-bounded and
/// cycle-guarded: a record that holds itself would otherwise walk forever.
///
/// Three shapes it does *not* see, each needing a value rather than a type to
/// find and none of them a fixture anyone writes by accident: a union case's
/// payload (the fields sit on the case's nested class, not on the union's own
/// type); a value behind a type from neither of our assemblies that is not a
/// delegate (a `System.Collections.Generic.List<obj>` of Atlases, say); and an
/// open generic, whose `FullName` is null and which is therefore never a
/// static's runtime type in the first place.
let rec internal carries (depth: int) (seen: Set<string>) (t: Type) =
    if isNull t || depth > 12 || isNull t.FullName then
        false
    elif
        List.contains t mutableRoots
        || (t.IsGenericType && List.contains (t.GetGenericTypeDefinition()) mutableShapes)
    then
        true
    elif seen.Contains t.FullName then
        false
    else
        let seen = Set.add t.FullName seen
        let deeper = carries (depth + 1) seen

        (t.IsGenericType && t.GetGenericArguments() |> Array.exists deeper)
        || (t.HasElementType && deeper (t.GetElementType()))
        || (ours t
            && t.GetFields(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
               |> Array.exists (fun field -> deeper field.FieldType))

/// The types one static is judged on: what it declares, what its value turns
/// out to be, and — for a delegate, whose own type is FSharp.Core's or the
/// BCL's — what its `Target` closure is. Reading the value runs the module
/// initialiser, which is what every test in this suite does anyway; a static
/// that throws on read is skipped rather than failing this test, because its
/// own list is where that belongs.
let private shapesOf (declared: Type) (value: obj) =
    [
        declared
        match value with
        | null -> ()
        | :? Delegate as d ->
            value.GetType()

            match d.Target with
            | null -> ()
            | target -> target.GetType()
        | _ -> value.GetType()
    ]

[<Tests>]
let parallelSafetyTests =
    testList
        "test fixtures are parallel-safe"
        [
            test "the probe answers for the shapes a fixture is written in" {
                // A clean assembly and a broken probe are the same green
                // otherwise: the assertion below is that a list is empty.
                let reaches t = carries 0 Set.empty t

                Expect.isTrue (reaches typeof<Fabot.Core.Atlas.Atlas>) "an Atlas is the root itself"
                Expect.isTrue (reaches typeof<Fabot.Core.Atlas.Atlas list>) "a list of them"
                Expect.isTrue (reaches typeof<Lazy<Fabot.Core.Atlas.Atlas>>) "one behind a Lazy"

                Expect.isTrue
                    (reaches typeof<Map<string, Fabot.Core.Atlas.Atlas>>)
                    "one as a Map's value"

                Expect.isTrue (reaches typeof<Fabot.Core.Types.Intents.WalkTable>) "the other root"

                Expect.isTrue
                    (reaches typeof<System.Collections.Generic.HashSet<string>>)
                    "a memo rolled by hand, recognised by shape"

                Expect.isTrue (reaches typeof<int ref>) "and the smallest of those shapes"
                Expect.isFalse (reaches typeof<string>) "a string is not a fixture to fear"

                Expect.isFalse
                    (reaches typeof<Set<Fabot.Core.Types.Geometry.Pos> list>)
                    "nor a census taken off one"
            }

            test "no fixture holds an Atlas or a mutable table as a shared value" {
                // Both halves of a module-level binding are checked: the
                // static property the module exposes, and the backing field
                // the startup class holds it in. Either alone would miss a
                // shape the compiler chose differently, and both together name
                // one offender twice — `binding` is the name they agree on.
                let binding (owner: Type) (name: string) =
                    let short = name.Split('@').[0].TrimStart('$')

                    let where =
                        (owner.FullName |> String.filter (fun c -> c <> '$'))
                            .Replace("<StartupCode", "")
                            .Split('.')
                        |> Array.last

                    $"%s{where}.%s{short}"

                let flagged (name: string) (declared: Type) (value: unit -> obj) =
                    let value =
                        try
                            value ()
                        with _ ->
                            null

                    shapesOf declared value
                    |> List.tryFind (carries 0 Set.empty)
                    |> Option.map (fun t -> name, $"%s{name} : %s{t.ToString()}")

                let statics = BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic

                let offenders =
                    [
                        for owner in Assembly.GetExecutingAssembly().GetTypes() do
                            for property in owner.GetProperties(statics) do
                                if property.GetIndexParameters().Length = 0 && property.CanRead then
                                    yield!
                                        flagged
                                            (binding owner property.Name)
                                            property.PropertyType
                                            (fun () -> property.GetValue null)
                                        |> Option.toList

                            for field in owner.GetFields(statics) do
                                yield!
                                    flagged (binding owner field.Name) field.FieldType (fun () ->
                                        field.GetValue null)
                                    |> Option.toList
                    ]
                    |> List.distinctBy fst
                    |> List.map snd

                Expect.isEmpty
                    offenders
                    ("shared mutable state between Expecto's parallel tests (#310) — make the fixture a function, "
                     + "and take care that the function captures none of its own:\n"
                     + String.Join("\n", offenders))
            }
        ]
