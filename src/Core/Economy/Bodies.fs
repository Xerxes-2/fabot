/// A pattern shapes what a body is good at, never what it is assigned: the
/// matcher reads parts, not patterns, but for the harvester's seat (`Pool.fits`).
module Fabot.Core.Economy.Bodies

open Fabot.Core
open Fabot.Core.Vocabulary

[<RequireQualifiedAccess>]
type Pattern =
    | Worker
    | Harvester
    | Hauler
    | Upgrader
    | Claimer

/// In the order a quota reports them.
let allPatterns =
    [
        Pattern.Worker
        Pattern.Harvester
        Pattern.Hauler
        Pattern.Upgrader
        Pattern.Claimer
    ]

let nameOf (pattern: Pattern) : string =
    match pattern with
    | Pattern.Worker -> "worker"
    | Pattern.Harvester -> "harvester"
    | Pattern.Hauler -> "hauler"
    | Pattern.Upgrader -> "upgrader"
    | Pattern.Claimer -> "claimer"

let blockOf (pattern: Pattern) : BodyPart list =
    match pattern with
    | Pattern.Worker -> [ Work; Carry; Move ]
    | Pattern.Harvester -> [ Work; Work; Carry; Move ]
    | Pattern.Hauler -> [ Carry; Carry; Move ]
    | Pattern.Upgrader -> [ Work; Carry; Move ]
    | Pattern.Claimer -> [ Claim; Move ]

let keyPartOf (pattern: Pattern) : BodyPart =
    match pattern with
    | Pattern.Worker
    | Pattern.Harvester
    | Pattern.Upgrader -> Work
    | Pattern.Hauler -> Carry
    | Pattern.Claimer -> Claim

/// Unique because a spawn casts one body a tick.
let castName (pattern: Pattern) (time: int) (spawn: string) : string =
    $"{nameOf pattern}-{time}-{spawn}"

/// Read off the name, not the parts: damage strips parts head-first, so no
/// reading of the standing parts holds the pattern for the body's life.
let patternOfName (name: string) : Pattern option =
    let prefix =
        match name.IndexOf '-' with
        | -1 -> name
        | at -> name.Substring(0, at)

    allPatterns |> List.tryFind (fun p -> nameOf p = prefix)

/// Under one CARRY to four WORK a delivery is a commute for its WORK. The
/// upgrader row casts only a standing body, or it is a worker by another name.
let standing (work: int) (carry: int) : bool = carry * 4 < work

let patternOfParts (work: int) (carry: int) (move: int) (claim: int) : Pattern =
    if claim > 0 then
        Pattern.Claimer
    elif work > 0 && (carry = 0 || work > move) then
        Pattern.Harvester
    elif work = 0 && carry > 0 then
        Pattern.Hauler
    elif standing work carry then
        Pattern.Upgrader
    else
        Pattern.Worker

/// The engine destroys parts from the head and charges fatigue on dead parts,
/// so a body with a MOVE last walks while any part lives.
let withMoveLast (body: BodyPart list) : BodyPart list =
    match List.tryFindIndexBack (fun p -> samePart p Move) body with
    | Some at when at < List.length body - 1 -> List.removeAt at body @ [ Move ]
    | _ -> body

let private replicate (n: int) (part: BodyPart) = List.replicate (max 0 n) part

/// `work + carry <= 2 × move` is a tile a tick loaded on a road. Grouped so
/// damage strips Work first.
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

/// The dig falls into the container under it with no Carry; only a link
/// takes it from the store, and a bank that holds links has the room.
let private harvesterBody (moves: int -> int) (most: int) (capacity: int) =
    let bodyOf n =
        replicate n Work @ replicate (moves n) Move

    let mutable n = min most ((Engine.maxBodyParts - 1) * 2 / 3)

    while n > 2 && Engine.bodyCost (bodyOf n) > capacity do
        n <- n - 1

    let n = max 2 n

    let carry =
        if Engine.bodyCost (Carry :: bodyOf n) <= capacity then
            [ Carry ]
        else
            []

    replicate n Work @ carry @ replicate (moves n) Move

/// The MOVEs walk it to the buffer once at road parity; the CARRY holds one
/// draw from the buffer at its feet.
let private upgraderBody (most: int) (capacity: int) =
    let pairs =
        (capacity - Engine.partCost Carry) / Engine.bodyCost [ Work; Move ]
        |> min ((Engine.maxBodyParts - 1) / 2)
        |> min most
        |> max 1

    replicate pairs Work @ [ Carry ] @ replicate pairs Move

let private wholeBlocks (block: BodyPart list) (most: int) (capacity: int) =
    let repeats =
        capacity / Engine.bodyCost block
        |> min (Engine.maxBodyParts / List.length block)
        |> min most

    let parts = Array.ofList block
    let body = ResizeArray<BodyPart>()

    for i in 0 .. parts.Length - 1 do
        let part = Tiles.at i parts

        if Array.findIndex (samePart part) parts = i then
            for _ in 1 .. repeats * partCountIn block part do
                body.Add part

    List.ofArray (body.ToArray())

let sized (pattern: Pattern) (most: int) (capacity: int) : BodyPart list option =
    if capacity < Engine.bodyCost (blockOf pattern) then
        None
    else
        let most = max 1 most

        let body =
            match pattern with
            | Pattern.Worker -> workerBody most capacity
            | Pattern.Harvester -> harvesterBody (fun n -> (n + 1) / 2) (max 2 most) capacity
            | Pattern.Hauler -> wholeBlocks (blockOf pattern) ((most + 1) / 2) capacity
            | Pattern.Upgrader -> upgraderBody most capacity
            | Pattern.Claimer -> wholeBlocks (blockOf pattern) most capacity

        Some(withMoveLast body)

/// For a seat in the casting colony's own room, which it walks once: legs
/// never cost it a Work.
let homeHarvester (most: int) (capacity: int) : BodyPart list option =
    if capacity < Engine.bodyCost (blockOf Pattern.Harvester) then
        None
    else
        let body = harvesterBody (fun _ -> 1) (max 2 most) capacity
        let legs = (partCountIn body Work + 1) / 2 - 1
        let spare = (capacity - Engine.bodyCost body) / Engine.partCost Move
        Some(withMoveLast (body @ replicate (min legs spare) Move))
