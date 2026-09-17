/// The closed vocabularies that cross the wire, checked against the unions
/// themselves rather than against the lists the decoders ride (#80). A
/// case added to one of these unions without its wire name used to empty
/// the observe log in silence, every tick, with no error anywhere; here it
/// is a red test. Reflection lives in this project alone — Core's tables
/// stay plain data, so none of this reaches the Fable bundle.
module Fabot.Core.Tests.WireTests

open Expecto
open System
open System.IO
open System.Text.RegularExpressions
open FSharp.Reflection
open Fabot.Core.Types
open Fabot.Core

/// The value a case's i-th field is sampled with: distinct numbers, so a
/// vocabulary that drops a field — or rebuilds a case with two of them
/// swapped — fails the round trip rather than passing on a coincidence.
let private sampleField i = i + 1

/// The value a case's i-th text field is sampled with, as `sampleField` is
/// its numeric one: distinct per position for the same reason, and
/// recognisable in a failure message.
let private sampleText i = $"field-{i}"

/// The numbers handed to a vocabulary that decodes payload-carrying
/// cases: exactly what `casesOf` builds such a case around, in field
/// order, so the case that was spelt is the case that must read back.
let private sampleNumbers = Some(sampleField 0, sampleField 1)

/// The same, for a vocabulary whose payload is a name rather than
/// numbers: the Layout channel's TrunkGoal carries the spawn's id (#107).
let private sampleName = Some(sampleText 0)

/// Every case of a closed union, read off the union's own metadata: the
/// enumeration no hand-written list can be trusted to match. A case that
/// carries fields is built around `sampleField` or `sampleText`, so a
/// reason that is no longer a bare tag is enumerated exactly like one.
/// Numbers, names and a `Resource` are sampled; any other field type throws,
/// which is the next author's notice to widen this rather than a case quietly
/// skipped. The Resource sample is `Energy` and one case is built per union
/// case, so what this enumeration proves about the resource-carrying Tasks is
/// that their *prefixes* differ — that the two **resources** differ at one store
/// is the cross-product the Task test below walks by hand.
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
/// that same case, and no two cases share a name. A case the encoder's
/// list omits decodes to None and fails the first assertion; a spelling
/// copied onto a second case fails the second.
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
                // The seven unions that ride the observe channel's Memory
                // subtree — the four Verdict vocabularies and the Layout
                // channel's three. The encoder is exhaustive by construction;
                // what is checked here is the other direction, which the
                // compiler cannot see.
                roundTrips "MatchFactor" (casesOf<MatchFactor> ()) matchFactorName matchFactorOf

                // The two reason vocabularies carry numbers now (#88), so
                // they are reversed for a payload: the decoder is handed
                // the very pair `casesOf` spelt, and `too-early` must read
                // back as that case rather than as a bare tag around zeros.
                //
                // A ReleaseReason is `TaskGone` or a carried RejectReason, so
                // its enumeration is built off that union's own rather than
                // read from its metadata: `casesOf` samples numbers and names,
                // not unions, and a reason added to the refusals has to arrive
                // here as a release too — which is the whole point of the two
                // being one union now.
                roundTrips
                    "ReleaseReason"
                    (Array.append
                        [| ReleaseReason.TaskGone |]
                        (casesOf<RejectReason> () |> Array.map ReleaseReason.Rejected))
                    releaseReasonName
                    (releaseReasonOf sampleNumbers)

                roundTrips "IdleReason" (casesOf<IdleReason> ()) idleReasonName idleReasonOf

                // The Layout channel's own vocabulary (#77, ADR 0035).
                // Not a Verdict — the Layout speaks none — but it rides
                // the same Memory subtree under the same rule, so it is
                // enumerated here beside them.
                roundTrips "FootingKind" (casesOf<FootingKind> ()) footingKindName footingKindOf

                // The Layout channel's fourth vocabulary (ADR 0060 decision
                // 1): which kind of declaration one refusal names, which the
                // record has to say now that there are two kinds of room a
                // human declares and the fix is to move one of two lists.
                roundTrips
                    "DeclarationKind"
                    (casesOf<DeclarationKind> ())
                    declarationKindName
                    declarationKindOf

                // The Layout channel's second vocabulary (#107), and its
                // first carrying one: a trunk goal is the Upgrade Work
                // Area or a spawn, and the spawn's id rides beside the
                // name the way a reason's numbers do.
                roundTrips
                    "TrunkGoal"
                    (casesOf<TrunkGoal> ())
                    trunkGoalName
                    (trunkGoalOf sampleName)

                // The Layout channel's third vocabulary (ADR 0040), and
                // its second carrying one: a container target is a source
                // or the controller, and the source's id rides beside the
                // name as a trunk goal's spawn does.
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

                // The Raid log's own vocabulary (ADR 0043): which deadline a
                // [[stand-down]]'s expiry tick was read off — ADR 0043's
                // three for an invader core, and #165's fourth for another
                // player's reservation. It rides the same Memory subtree as
                // the rest, and a basis that will not read back costs its
                // episode the reason it is holding an outpost shut.
                roundTrips
                    "StandDownBasis"
                    (casesOf<StandDownBasis> ())
                    standDownBasisName
                    standDownBasisOf

                // The Raid log's second vocabulary (#333): whose CLAIM parts
                // stand on an [[outpost]]'s controller, carried on the leaf's
                // hold map so the channel can name the player a room is not
                // ours to reserve because of. All three cases are spelt
                // though only two are ever written — a vocabulary with a
                // hole in it is one a later reader falls through.
                roundTrips
                    "ReservationHolder"
                    (casesOf<ReservationHolder> ())
                    reservationHolderName
                    reservationHolderOf
            }

            test "the engine vocabularies round-trip over their own lists" {
                // `allBodyParts` and `allBuiltKinds` are hand-written
                // literals the shells reverse to classify engine strings;
                // a case missing from either leaves the lookup short, and
                // the round trip is what says so. `reverseOf` here is the
                // builder the shells call, not a look-alike, so what is
                // checked is the lookup they ship.
                roundTrips
                    "BodyPart"
                    (casesOf<BodyPart> ())
                    partName
                    (reverseOf partName allBodyParts)

                // Other is deliberately not in `allBuiltKinds`: it is what
                // an unmatched engine string classifies to, not a kind the
                // engine names, and it spells the empty string.
                roundTrips
                    "BuiltKind"
                    (casesOf<BuiltKind> () |> Array.filter (fun kind -> kind <> BuiltKind.Other))
                    builtKindName
                    (reverseOf builtKindName allBuiltKinds)

                // The resources the colony names (ADR 0057). The same
                // contract as the two above and for the same reason: these
                // strings are `store` keys and the argument of every
                // `withdraw` and `transfer`, so a case added to the union
                // without a spelling is a resource the shell cannot ask the
                // engine for. Thorium's is the season mod's one-letter "T",
                // which is also what `mineralType` reads on the deposit.
                roundTrips
                    "Resource"
                    (casesOf<Resource> ())
                    resourceName
                    (reverseOf resourceName allResources)
            }

            test "a name outside a vocabulary decodes to nothing, never to a case" {
                // What the decoders' misses rest on: an unknown name is
                // None, so the shell can decide what it costs rather than
                // silently reading as some other case.
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
                // The other half of a carrying vocabulary's contract: the
                // name alone is not the case. A `too-early` row that lost
                // its numbers — a bundle that predates them, a hand-edit
                // through the Memory HTTP API — decodes to None, and the
                // shell drops that row rather than restating a walk and a
                // wait nobody wrote.
                Expect.isNone (rejectReasonOf None "too-early") "no numbers, no reason"
                Expect.isNone (releaseReasonOf None "too-early") "and the same on the other side"

                // The Layout channel's carrying vocabulary under the same
                // rule: a `spawn` row that lost its id names no goal, and
                // must not read back as the goal that carries none.
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
                // The half of a Task's wire contract that exists (#167).
                // `Task` is not one of the vocabularies above and cannot
                // be: `taskId` is one-way — there is no `taskOf` anywhere
                // in Core, App or the scripts — because an assignment
                // crosses into Memory as an opaque `string -> string` map
                // that nothing ever decodes back into a case. What *does*
                // ride that map is the identity, and it is only an
                // identity while it is unique: two cases spelling one id
                // would make a creep's assignment name two different
                // pieces of work, and anti-thrash would keep the wrong one
                // alive. `casesOf` samples every field, so a case added
                // without a `taskId` arm fails the compiler and one added
                // with a copied prefix fails here.
                let tasks = casesOf<Task> ()

                Expect.equal
                    (tasks |> Array.map Decide.Facts.taskId |> Array.distinct |> Array.length)
                    (Array.length tasks)
                    "Task: no two cases share a task id"

                // And the other axis, which `casesOf` cannot walk: the two
                // Tasks that carry a [[resource]] spell one id per resource at
                // one store (ADR 0057 decision 3). Off `allResources`, so a
                // third resource is checked the day it is named rather than the
                // day it collides. The energy spelling is pinned outright
                // beside it: a Task id is a key carried across ticks, in Memory
                // and in every `observe` transition line, so widening the
                // colony's energy work's own ids would orphan every standing
                // assignment on the tick the bundle is deployed.
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

/// The other end of the same wire: the tables in `scripts/observe.mjs` that turn
/// these names into English. The round trips above keep F# honest with itself,
/// and #368 is what they do not cover — **the JavaScript reader has no union to
/// be exhaustive against.**
///
/// Live, `StandDownBasis` grew a fifth case (#165's `invader-raid`) and the
/// observer's table kept four. The F# side was green: `standDownBasisName`
/// matched exhaustively, the round trip passed, the leaf was written correctly.
/// The reader dropped every row it could not name — and, because these tables
/// are deliberately closed (a guessed row would describe a violation nobody
/// wrote), it dropped them while **printing that a human must have hand-edited
/// the leaf**. Two real stand-downs in W15S25 and W15S26 were unreadable for
/// about 66,000 ticks, and the diagnosis was pointed at the operator.
///
/// So this reads the script as text and checks the keys. Crude, and the only
/// check available: the alternative is emitting the tables from F# into the
/// bundle, which would put English prose the bot never reads into the 570 KB it
/// uploads every deploy.
module private Observer =

    open System.IO
    open System.Text.RegularExpressions

    /// The repository root, found by walking up from the test binary until a
    /// `package.json` stands in the directory. Not a relative hop count from
    /// `AppContext.BaseDirectory`: that is `bin/Debug/net10.0` today and the
    /// count would rot the next time the build layout moves.
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
    /// indentation. Fails loudly when the table is not there at all, because a
    /// renamed table that this test silently found nothing in is the same
    /// failure it exists to catch.
    let keysOf (table: string) =
        let opening = Regex.Match(script.Value, $@"const {table} = \{{")

        if not opening.Success then
            failwithf
                "no `const %s = {` in scripts/observe.mjs: the table was renamed or removed, and this test can no longer see whether it is complete"
                table

        let body = script.Value.Substring(opening.Index + opening.Length)
        let closing = body.IndexOf("\n  };")

        let body = if closing >= 0 then body.Substring(0, closing) else body

        // Quoted **and** bare keys. Two of the five stand-down bases are
        // spelt `reservation:` and `fallback:` — legal JavaScript identifiers,
        // so the script quotes only the hyphenated ones — and a reader that saw
        // the quoted form alone reported this complete table as missing them.
        // It cost the first run of this very test, which is the argument for
        // anchoring on the table's own indentation rather than on a quote.
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

/// The wire spelling these tables are keyed by, as a convention rather than a
/// shared function: the breach vocabulary lives in `App` (`ObserveMemory`),
/// which this project cannot reference, and its own docstring states the
/// convention — "hyphenated lower case, the spelling `standDownBasisName` and
/// `declarationKindName` already use". A vocabulary that departs from it reddens
/// this test, which is the right outcome: the departure needs to be deliberate.
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

                // Both tables, because the row prints a basis twice over: once
                // as the reason the outpost is shut and once as the sighting the
                // deadline was read off, and #165's case reached only one of
                // them for a while.
                for table in [ "BASIS"; "SIGHTING" ] do
                    Expect.isEmpty
                        (Set.difference spelt (Observer.keysOf table))
                        $"every StandDownBasis is a key of {table} in scripts/observe.mjs — a case the script cannot name is a row it drops while blaming the operator (#368)"
            }

            test "the breach kinds the script can name are all of them" {
                // Five kinds today: two about ore that cannot be spent and
                // three about the Reactor. `reactor-running-dry` is the one
                // added most recently (#361), which is exactly the moment this
                // check earns its keep.
                let spelt =
                    casesOf<Observe.BreachKind> ()
                    |> Seq.map (fun kind -> kebabOf (string kind))
                    |> Set.ofSeq

                Expect.isEmpty
                    (Set.difference spelt (Observer.keysOf "KIND"))
                    "every BreachKind is a key of KIND in scripts/observe.mjs (#368)"
            }
        ]

/// #370: the CPU line's per-colony split is written by `saveCpu` and read by
/// `observe.mjs cpu`, and the key it rides under is a third vocabulary across
/// the same seam #368 caught lagging.
///
/// Weaker than the two tests above, deliberately. `BASIS` and `KIND` are closed
/// tables whose keys are a union's cases, so the union can be enumerated and
/// the table checked against it. A colony's home room is not a closed set — it
/// is whatever `Colony.declared` says today — so what can be checked is only
/// that the reader looks for the group at all, under the spelling the writer
/// uses. That is still the failure #368 was about: the writer grew a key and
/// the reader never learnt it.
[<Tests>]
let observerCpuTests =
    testList
        "the observer reads the CPU line's per-colony split"
        [
            test "the reader knows the key `saveCpu` writes the split under" {
                let script = Observer.script.Value

                Expect.isTrue
                    (script.Contains "row.colonies")
                    "`observe.mjs cpu` reads `colonies` off a row: the sub-object `saveCpu` writes each colony's `decide` into, absent on a row from a bundle that did not measure it"
            }

            test "the split is not folded into the all-six-or-none phase group" {
                // `cpuPhaseFields` decodes all of its keys or none of them, so
                // a sixth phase key would make every row the previous bundle
                // wrote read as unmeasured — and those rows are the window a
                // change to this reading is compared against. The reader's own
                // phase list is the thing that must not have grown.
                let phases =
                    Observer.script.Value
                    |> fun text -> Regex.Match(text, @"const PHASES = \[([^\]]*)\]")

                Expect.isTrue
                    phases.Success
                    "`observe.mjs` still declares its phase columns in one list"

                Expect.isFalse
                    (phases.Groups.[1].Value.Contains "colonies")
                    "and the per-colony split is not one of them: it decodes on its own, so an older row keeps its phases"
            }
        ]
