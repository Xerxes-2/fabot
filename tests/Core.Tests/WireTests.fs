/// The closed vocabularies that cross the wire, checked against the unions
/// themselves rather than against the lists the decoders ride (#80). A
/// case added to one of these unions without its wire name used to empty
/// the observe log in silence, every tick, with no error anywhere; here it
/// is a red test. Reflection lives in this project alone — Core's tables
/// stay plain data, so none of this reaches the Fable bundle.
module Fabot.Core.Tests.WireTests

open Expecto
open FSharp.Reflection
open Fabot.Core.Types

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
/// Numbers and names are sampled; any other field type throws, which is
/// the next author's notice to widen this rather than a case quietly
/// skipped.
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
                roundTrips
                    "ReleaseReason"
                    (casesOf<ReleaseReason> ())
                    releaseReasonName
                    (releaseReasonOf sampleNumbers)

                roundTrips "IdleReason" (casesOf<IdleReason> ()) idleReasonName idleReasonOf

                // The Layout channel's own vocabulary (#77, ADR 0035).
                // Not a Verdict — the Layout speaks none — but it rides
                // the same Memory subtree under the same rule, so it is
                // enumerated here beside them.
                roundTrips "FootingKind" (casesOf<FootingKind> ()) footingKindName footingKindOf

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
        ]
