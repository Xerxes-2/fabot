/// The closed vocabularies that cross the wire, checked against the unions
/// themselves rather than against the lists the decoders ride: a case added
/// without its wire name used to empty the observe log in silence.
/// Reflection lives in this project alone, so none of it reaches the bundle.
module Fabot.Core.Tests.WireTests

open Expecto
open System
open System.IO
open System.Text.RegularExpressions
open FSharp.Reflection
open Fabot.Core.Types
open Fabot.Core

/// Distinct numbers per field position, so swapped fields fail the round trip.
let private sampleField i = i + 1

/// The text twin of `sampleField`, distinct per position.
let private sampleText i = $"field-{i}"

/// The numbers `casesOf` builds a payload-carrying case around, in field order.
let private sampleNumbers = Some(sampleField 0, sampleField 1)

/// The same, for a payload that is a name: the TrunkGoal's spawn id.
let private sampleName = Some(sampleText 0)

/// Every case of a closed union, read off the union's own metadata. Numbers,
/// names and a `Resource` are sampled; any other field type throws, which is
/// the next author's notice to widen this. The Resource sample is `Energy`,
/// so two resources at one store is the cross-product the Task test below
/// walks by hand.
let private casesOf<'a> () =
    FSharpType.GetUnionCases typeof<'a>
    |> Array.map (fun case ->
        let fields =
            case.GetFields()
            |> Array.mapi (fun i field ->
                if field.PropertyType = typeof<int> then
                    box (sampleField i)
                elif field.PropertyType = typeof<string> then
                    box (sampleText i)
                elif field.PropertyType = typeof<Resource> then
                    box Energy
                else
                    failwith $"no sample value for a {field.PropertyType.Name} field")

        FSharpValue.MakeUnion(case, fields) :?> 'a)

/// One vocabulary's contract: every case spells a name that reads back as
/// that same case, and no two cases share a name.
let private roundTrips label cases toName ofName =
    for case in cases do
        Expect.equal
            (ofName (toName case))
            (Some case)
            $"{label}: %A{case} reads back from its wire name"

    Expect.equal
        (cases |> Array.map toName |> Array.distinct |> Array.length)
        (Array.length cases)
        $"{label}: no two cases share a wire name"

[<Tests>]
let wireVocabularyTests =
    testList
        "wire vocabularies"
        [
            test "every observe vocabulary round-trips, case by case" {
                // The seven unions that ride the observe channel's Memory subtree. The
                // encoder is exhaustive by construction; the other direction is not.
                roundTrips "MatchFactor" (casesOf<MatchFactor> ()) matchFactorName matchFactorOf

                // The two reason vocabularies carry numbers, so they are reversed for a
                // payload. A ReleaseReason is `TaskGone` or a carried RejectReason, so its
                // enumeration is built off that union's own.
                roundTrips
                    "ReleaseReason"
                    (Array.append
                        [| ReleaseReason.TaskGone |]
                        (casesOf<RejectReason> () |> Array.map ReleaseReason.Rejected))
                    releaseReasonName
                    (releaseReasonOf sampleNumbers)

                roundTrips "IdleReason" (casesOf<IdleReason> ()) idleReasonName idleReasonOf

                // The Layout channel's own vocabulary: not a Verdict, same subtree.
                roundTrips "FootingKind" (casesOf<FootingKind> ()) footingKindName footingKindOf

                // The Layout channel's fourth vocabulary: which kind of declaration.
                roundTrips
                    "DeclarationKind"
                    (casesOf<DeclarationKind> ())
                    declarationKindName
                    declarationKindOf

                // The Layout channel's second vocabulary, its first carrying one.
                roundTrips
                    "TrunkGoal"
                    (casesOf<TrunkGoal> ())
                    trunkGoalName
                    (trunkGoalOf sampleName)

                // The Layout channel's third vocabulary, its second carrying one.
                roundTrips
                    "ContainerTarget"
                    (casesOf<ContainerTarget> ())
                    containerTargetName
                    (containerTargetOf sampleName)

                roundTrips
                    "RejectReason"
                    (casesOf<RejectReason> ())
                    rejectReasonName
                    (rejectReasonOf sampleNumbers)

                // The Raid log's own vocabulary: which deadline a stand-down's expiry was
                // read off.
                roundTrips
                    "StandDownBasis"
                    (casesOf<StandDownBasis> ())
                    standDownBasisName
                    standDownBasisOf

                // The Raid log's second vocabulary. All three cases are spelt though only
                // two are ever written.
                roundTrips
                    "ReservationHolder"
                    (casesOf<ReservationHolder> ())
                    reservationHolderName
                    reservationHolderOf
            }

            test "the engine vocabularies round-trip over their own lists" {
                // `allBodyParts` and `allBuiltKinds` are hand-written literals the shells
                // reverse; `reverseOf` is the builder they call, not a look-alike.
                roundTrips
                    "BodyPart"
                    (casesOf<BodyPart> ())
                    partName
                    (reverseOf partName allBodyParts)

                // Other is deliberately not in `allBuiltKinds`: it is what an unmatched
                // engine string classifies to, and it spells the empty string.
                roundTrips
                    "BuiltKind"
                    (casesOf<BuiltKind> () |> Array.filter (fun kind -> kind <> BuiltKind.Other))
                    builtKindName
                    (reverseOf builtKindName allBuiltKinds)

                // The resources are `store` keys and the argument of every `withdraw` and
                // `transfer`. Thorium's is the season mod's one-letter "T", which is also
                // what `mineralType` reads on the deposit.
                roundTrips
                    "Resource"
                    (casesOf<Resource> ())
                    resourceName
                    (reverseOf resourceName allResources)
            }

            test "a name outside a vocabulary decodes to nothing, never to a case" {
                // An unknown name is None, so the shell can decide what it costs rather
                // than silently reading as some other case.
                Expect.isNone (matchFactorOf "pool-ordre") "a misspelt MatchFactor is no factor"

                Expect.isNone
                    (releaseReasonOf sampleNumbers "no-tasks")
                    "an IdleReason is no ReleaseReason"

                Expect.isNone (idleReasonOf "") "the empty name is no IdleReason"

                Expect.isNone (footingKindOf "container") "a near miss is no FootingKind"

                Expect.isNone (declarationKindOf "outposts") "a near miss is no DeclarationKind"

                Expect.isNone (trunkGoalOf sampleName "upgrade") "a near miss is no TrunkGoal"

                Expect.isNone
                    (rejectReasonOf sampleNumbers "task-gone")
                    "a ReleaseReason is no RejectReason"
            }

            test "a payload-carrying name without its numbers reads as nothing" {
                // The name alone is not the case: a `too-early` row that lost its numbers
                // decodes to None, and the shell drops that row.
                Expect.isNone (rejectReasonOf None "too-early") "no numbers, no reason"
                Expect.isNone (releaseReasonOf None "too-early") "and the same on the other side"

                // The Layout channel's carrying vocabulary under the same rule: a `spawn`
                // row that lost its id names no goal.
                Expect.isNone (trunkGoalOf None "spawn") "no spawn id, no goal"

                Expect.equal
                    (trunkGoalOf None "upgrade-area")
                    (Some TrunkGoal.UpgradeArea)
                    "while the goal that carries nothing needs nothing"

                Expect.equal
                    (rejectReasonOf None "unreachable")
                    (Some RejectReason.Unreachable)
                    "a bare tag needs none, and is unaffected"
            }

            test "no two Tasks spell one task id" {
                // `taskId` is one-way — there is no `taskOf` — because an assignment crosses
                // into Memory as an opaque `string -> string` map. What rides it is the
                // identity, and two cases spelling one id would make anti-thrash keep the
                // wrong assignment alive.
                let tasks = casesOf<Task> ()

                Expect.equal
                    (tasks |> Array.map Decide.Facts.taskId |> Array.distinct |> Array.length)
                    (Array.length tasks)
                    "Task: no two cases share a task id"

                // The axis `casesOf` cannot walk: one id per resource at one store, off
                // `allResources`. The energy spelling is pinned outright: widening a Task
                // id would orphan every standing assignment on the tick the bundle deploys.
                let perResource task =
                    allResources |> List.map (task >> Decide.Facts.taskId)

                for spelt in
                    [
                        perResource (fun r -> Withdraw("store", r))
                        perResource (fun r -> Refill("store", r))
                    ] do
                    Expect.equal
                        (spelt |> List.distinct |> List.length)
                        (List.length spelt)
                        $"one id per resource at one store: %A{spelt}"

                Expect.equal
                    (Decide.Facts.taskId (Withdraw("store", Energy)),
                     Decide.Facts.taskId (Refill("store", Energy)))
                    ("withdraw:store", "refill:store")
                    "and the energy spelling is the one it has always been"
            }
        ]

/// The other end of the same wire: the tables in `scripts/observe.mjs` that
/// turn these names into English. **The JavaScript reader has no union to be
/// exhaustive against**: live (#368) `StandDownBasis` grew a fifth case, the
/// observer's table kept four, and two real stand-downs were unreadable for
/// about 66,000 ticks while the reader blamed a hand-edited leaf.
///
/// So this reads the script as text and checks the keys — crude, and the
/// only check short of emitting English prose into the bundle.
module private Observer =

    open System.IO
    open System.Text.RegularExpressions

    /// The repository root, found by walking up from the test binary until a
    /// `package.json` stands in the directory: a relative hop count from
    /// `AppContext.BaseDirectory` would rot when the build layout moves.
    let root () =
        let rec climb (dir: DirectoryInfo) =
            if isNull dir then
                failwith
                    "no package.json above the test binary: this test needs the repository, not just the assembly"
            elif File.Exists(Path.Combine(dir.FullName, "package.json")) then
                dir.FullName
            else
                climb dir.Parent

        climb (DirectoryInfo AppContext.BaseDirectory)

    let script =
        lazy (File.ReadAllText(Path.Combine(root (), "scripts", "observe.mjs")))

    /// The quoted keys of one `const NAME = { ... }` table in the script, from
    /// its opening brace to the first line that closes it at the same
    /// indentation. Fails loudly when the table is not there at all.
    let keysOf (table: string) =
        let opening = Regex.Match(script.Value, $@"const {table} = \{{")

        if not opening.Success then
            failwithf
                "no `const %s = {` in scripts/observe.mjs: the table was renamed or removed, and this test can no longer see whether it is complete"
                table

        let body = script.Value.Substring(opening.Index + opening.Length)
        let closing = body.IndexOf("\n  };")

        let body = if closing >= 0 then body.Substring(0, closing) else body

        // Quoted **and** bare keys: `reservation:` and `fallback:` are legal
        // identifiers the script leaves unquoted, and a reader that saw the quoted
        // form alone reported this complete table as missing them.
        Regex.Matches(
            body,
            "^\\s{4}(?:\"([a-z0-9-]+)\"|([A-Za-z][A-Za-z0-9]*))\\s*:",
            RegexOptions.Multiline
        )
        |> Seq.map (fun m ->
            if m.Groups[1].Success then
                m.Groups[1].Value
            else
                m.Groups[2].Value)
        |> Set.ofSeq

/// The wire spelling these tables are keyed by, as a convention rather than
/// a shared function: the breach vocabulary lives in `App`
/// (`ObserveMemory`), which this project cannot reference. A vocabulary
/// that departs from it reddens this test, deliberately.
let private kebabOf (name: string) =
    name
    |> Seq.mapi (fun i c ->
        if System.Char.IsUpper c && i > 0 then
            $"-{System.Char.ToLower c}"
        else
            string (System.Char.ToLower c))
    |> String.concat ""

[<Tests>]
let observerTableTests =
    testList
        "the observer's closed tables carry every case"
        [
            test "the stand-down bases the script can name are all of them" {
                let spelt = casesOf<StandDownBasis> () |> Seq.map standDownBasisName |> Set.ofSeq

                // Both tables, because the row prints a basis twice over: as the reason
                // the outpost is shut and as the sighting the deadline was read off.
                for table in [ "BASIS"; "SIGHTING" ] do
                    Expect.isEmpty
                        (Set.difference spelt (Observer.keysOf table))
                        $"every StandDownBasis is a key of {table} in scripts/observe.mjs — a case the script cannot name is a row it drops while blaming the operator (#368)"
            }

            test "the breach kinds the script can name are all of them" {
                // Five kinds today: two about ore that cannot be spent and three about the
                // Reactor.
                let spelt =
                    casesOf<Observe.BreachKind> ()
                    |> Seq.map (fun kind -> kebabOf (string kind))
                    |> Set.ofSeq

                Expect.isEmpty
                    (Set.difference spelt (Observer.keysOf "KIND"))
                    "every BreachKind is a key of KIND in scripts/observe.mjs (#368)"
            }
        ]

/// The CPU line's per-colony key is a third vocabulary across the same
/// seam. Weaker than the two above: a colony's home room is not a closed
/// set, so what can be checked is only that the reader looks for the group
/// under the spelling the writer uses.
[<Tests>]
let observerCpuTests =
    testList
        "the observer reads the CPU line's per-colony split"
        [
            test "the reader knows the keys `saveCpu` writes the splits under" {
                let script = Observer.script.Value

                // The reader takes the key as an argument, so both spellings must reach
                // it: `colonies` for each colony's `decide`, `rooms` for each room's
                // `snapshot`.
                for key in [ "colonies"; "rooms"; "projects" ] do
                    Expect.isTrue
                        (script.Contains $"attributedBy(\"{key}\")" || script.Contains $"\"{key}\",")
                        $"`observe.mjs cpu` reads `{key}` off a row: the sub-object `saveCpu` writes that split into"
            }

            test "the split is not folded into the all-six-or-none phase group" {
                // `cpuPhaseFields` decodes all of its keys or none, so a sixth phase key
                // would make every row the previous bundle wrote read as unmeasured.
                let phases =
                    Observer.script.Value
                    |> fun text -> Regex.Match(text, @"const PHASES = \[([^\]]*)\]")

                Expect.isTrue
                    phases.Success
                    "`observe.mjs` still declares its phase columns in one list"

                for key in [ "colonies"; "rooms"; "projects" ] do
                    Expect.isFalse
                        (phases.Groups.[1].Value.Contains key)
                        $"and the per-{key} split is not one of them: it decodes on its own, so an older row keeps its phases"
            }
        ]
