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

/// Every case of a closed union whose cases carry no fields, read off the
/// union's own metadata: the enumeration no hand-written list can be
/// trusted to match.
let private casesOf<'a> () =
    FSharpType.GetUnionCases typeof<'a>
    |> Array.map (fun case -> FSharpValue.MakeUnion(case, [||]) :?> 'a)

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
            test "every Verdict vocabulary round-trips, case by case" {
                // The four unions that ride the observe channel's Memory
                // subtree. The encoder is exhaustive by construction; what
                // is checked here is the other direction, which the
                // compiler cannot see.
                roundTrips "MatchFactor" (casesOf<MatchFactor> ()) matchFactorName matchFactorOf

                roundTrips
                    "ReleaseReason"
                    (casesOf<ReleaseReason> ())
                    releaseReasonName
                    releaseReasonOf

                roundTrips "IdleReason" (casesOf<IdleReason> ()) idleReasonName idleReasonOf

                roundTrips "RejectReason" (casesOf<RejectReason> ()) rejectReasonName rejectReasonOf
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
                Expect.isNone (releaseReasonOf "no-tasks") "an IdleReason is no ReleaseReason"
                Expect.isNone (idleReasonOf "") "the empty name is no IdleReason"
                Expect.isNone (rejectReasonOf "task-gone") "a ReleaseReason is no RejectReason"
            }
        ]
