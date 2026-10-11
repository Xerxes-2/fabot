/// The body patterns the economy casts and the arithmetic that sizes one to
/// a bank. A pattern shapes what a body is good at, never what it is
/// assigned: the matcher reads parts, not patterns, but for the harvester's
/// seat (`Pool.fits`).
module Fabot.Core.Economy.Bodies

open Fabot.Core
open Fabot.Core.Vocabulary

/// A body pattern: the repeating block a body is grown from.
[<RequireQualifiedAccess>]
type Pattern =
    /// Digs, carries and spends: the generalist.
    | Worker
    /// Heavy WORK that stands at a source's seat and digs, its dig falling
    /// where it stands.
    | Harvester
    /// Carries and does not dig.
    | Hauler
    /// Holds a reservation or takes a controller.
    | Claimer

/// Every pattern, in the order a quota reports them.
let allPatterns =
    [ Pattern.Worker; Pattern.Harvester; Pattern.Hauler; Pattern.Claimer ]

/// The name a cast of the pattern starts with.
let nameOf (pattern: Pattern) : string =
    match pattern with
    | Pattern.Worker -> "worker"
    | Pattern.Harvester -> "harvester"
    | Pattern.Hauler -> "hauler"
    | Pattern.Claimer -> "claimer"

/// The repeating block, and the least body the pattern casts.
let blockOf (pattern: Pattern) : BodyPart list =
    match pattern with
    | Pattern.Worker -> [ Work; Carry; Move ]
    | Pattern.Harvester -> [ Work; Work; Carry; Move ]
    | Pattern.Hauler -> [ Carry; Carry; Move ]
    | Pattern.Claimer -> [ Claim; Move ]

/// The part a pattern's quota is counted in.
let keyPartOf (pattern: Pattern) : BodyPart =
    match pattern with
    | Pattern.Worker
    | Pattern.Harvester -> Work
    | Pattern.Hauler -> Carry
    | Pattern.Claimer -> Claim

/// A cast's name: `{pattern}-{tick}-{spawn}`, unique because a spawn casts
/// one body a tick.
let castName (pattern: Pattern) (time: int) (spawn: string) : string =
    $"{nameOf pattern}-{time}-{spawn}"

/// The pattern a cast's name says (`castName`), fixed for the body's life:
/// damage strips parts head-first, so no reading of the standing parts holds
/// it. `None` for a name no cast wrote.
let patternOfName (name: string) : Pattern option =
    let prefix =
        match name.IndexOf '-' with
        | -1 -> name
        | at -> name.Substring(0, at)

    allPatterns |> List.tryFind (fun p -> nameOf p = prefix)

/// The pattern live parts read as, for a body whose name says none.
let patternOfParts (work: int) (carry: int) (move: int) (claim: int) : Pattern =
    if claim > 0 then
        Pattern.Claimer
    elif work > 0 && (carry = 0 || work > move) then
        Pattern.Harvester
    elif work = 0 && carry > 0 then
        Pattern.Hauler
    else
        Pattern.Worker

/// The last part a MOVE: the engine destroys parts from the head and charges
/// fatigue on dead parts, so a body with a MOVE last walks while any part
/// lives.
let withMoveLast (body: BodyPart list) : BodyPart list =
    match List.tryFindIndexBack (fun p -> samePart p Move) body with
    | Some at when at < List.length body - 1 -> List.removeAt at body @ [ Move ]
    | _ -> body

let private replicate (n: int) (part: BodyPart) = List.replicate (max 0 n) part

/// The worker's rule: as many whole blocks as the bank buys up to
/// `most`; where the bank and not `most` stopped them, the bank's remainder on
/// Carry and Move at loaded road parity (`work + carry <= 2 × move`, a tile a
/// tick loaded on a road). Grouped so damage strips Work first. A body held to
/// `most` is the unit body and nothing more.
let private workerBody (most: int) (capacity: int) =
    let block = blockOf Pattern.Worker

    let repeats =
        capacity / Engine.bodyCost block |> min (Engine.maxBodyParts / 3) |> min most

    let mutable work = repeats
    let mutable carry = repeats
    let mutable move = repeats

    let mutable budget =
        if repeats >= most then
            0
        else
            capacity - repeats * Engine.bodyCost block

    let mutable slots = Engine.maxBodyParts - 3 * repeats
    let carryCost = Engine.partCost Carry
    let moveCost = Engine.partCost Move
    let mutable go = true

    while go do
        if slots >= 1 && budget >= carryCost && work + carry + 1 <= 2 * move then
            carry <- carry + 1
            budget <- budget - carryCost
            slots <- slots - 1
        elif slots >= 2 && budget >= carryCost + moveCost then
            carry <- carry + 1
            move <- move + 1
            budget <- budget - carryCost - moveCost
            slots <- slots - 2
        elif slots >= 1 && budget >= moveCost then
            move <- move + 1
            budget <- budget - moveCost
            slots <- slots - 1
        else
            go <- false

    replicate work Work @ replicate carry Carry @ replicate move Move

/// The harvester's rule: one Carry, a Move per two Work so it walks to a
/// container in another room at a tile every two ticks, and every Work the
/// bank buys up to `most`.
let private harvesterBody (most: int) (capacity: int) =
    let bodyOf n =
        replicate n Work @ [ Carry ] @ replicate ((n + 1) / 2) Move

    let mutable n = min most ((Engine.maxBodyParts - 1) * 2 / 3)

    while n > 2 && Engine.bodyCost (bodyOf n) > capacity do
        n <- n - 1

    bodyOf (max 2 n)

/// Whole blocks, grouped by part in the block's order, up to `most` blocks.
let private wholeBlocks (block: BodyPart list) (most: int) (capacity: int) =
    let repeats =
        capacity / Engine.bodyCost block
        |> min (Engine.maxBodyParts / List.length block)
        |> min most

    block
    |> List.distinct
    |> List.collect (fun part -> replicate (repeats * partCountIn block part) part)

/// The body a pattern casts at a bank `capacity`, its key parts held to
/// `most`; `None` where the bank cannot buy the pattern's block. Never over
/// the engine's 50 parts or the bank.
let sized (pattern: Pattern) (most: int) (capacity: int) : BodyPart list option =
    if capacity < Engine.bodyCost (blockOf pattern) then
        None
    else
        let most = max 1 most

        let body =
            match pattern with
            | Pattern.Worker -> workerBody most capacity
            | Pattern.Harvester -> harvesterBody (max 2 most) capacity
            | Pattern.Hauler -> wholeBlocks (blockOf pattern) ((most + 1) / 2) capacity
            | Pattern.Claimer -> wholeBlocks (blockOf pattern) most capacity

        Some(withMoveLast body)
