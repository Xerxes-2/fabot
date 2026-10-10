/// The economy's work vocabulary: what a body can be assigned. Fight and
/// season work are not Tasks; a creep in an engagement is not in the
/// economy's pool at all (`ARCHITECTURE.md` §6.2).
module Fabot.Core.Economy.Task

open Fabot.Core.Vocabulary

/// One piece of economy work, named by the object it is done to. Compared
/// only through `taskId`: the id is what `assignments` keys on, and two Tasks
/// with one id are one Task.
[<NoEquality; NoComparison>]
type Task =
    /// Dig a source's energy, or a mineral's ore under our extractor.
    | Harvest of target: string
    /// Take a resource out of a store: a container, a storage, a tombstone or
    /// a ruin.
    | Withdraw of store: string * resource: Resource
    /// Pick a dropped pile up.
    | Pickup of pile: string * resource: Resource
    /// Put a resource into a structure: a spawn, an extension, a tower, the
    /// storage.
    | Refill of structure: string * resource: Resource
    /// Put energy into a construction site.
    | Build of site: string
    /// Put energy into a decaying structure's hits.
    | Repair of structure: string
    | Upgrade of controller: string
    /// Hold an unowned controller's reservation.
    | Reserve of controller: string
    /// Take an unowned controller.
    | Claim of controller: string

/// The Task's stable id: a one-letter kind, the target's id and, for the
/// Tasks that move a resource, the resource's engine name, `:`-separated.
/// Injective because an object id and a resource name hold no `:`.
let taskId (task: Task) : string =
    match task with
    | Harvest id -> "h:" + id
    | Withdraw(id, r) -> "w:" + id + ":" + resourceName r
    | Pickup(id, r) -> "p:" + id + ":" + resourceName r
    | Refill(id, r) -> "f:" + id + ":" + resourceName r
    | Build id -> "b:" + id
    | Repair id -> "r:" + id
    | Upgrade id -> "u:" + id
    | Reserve id -> "v:" + id
    | Claim id -> "c:" + id

/// The Task an id names, or `None` for a string `taskId` never writes.
let ofTaskId (id: string) : Task option =
    match id.Split ':' with
    | [| "h"; t |] -> Some(Harvest t)
    | [| "w"; t; r |] -> Some(Withdraw(t, resourceOfName r))
    | [| "p"; t; r |] -> Some(Pickup(t, resourceOfName r))
    | [| "f"; t; r |] -> Some(Refill(t, resourceOfName r))
    | [| "b"; t |] -> Some(Build t)
    | [| "r"; t |] -> Some(Repair t)
    | [| "u"; t |] -> Some(Upgrade t)
    | [| "v"; t |] -> Some(Reserve t)
    | [| "c"; t |] -> Some(Claim t)
    | _ -> None

/// Whether the Task fills the body's store (a draw) rather than empties it.
let isIntake (task: Task) : bool =
    match task with
    | Harvest _
    | Withdraw _
    | Pickup _ -> true
    | Refill _
    | Build _
    | Repair _
    | Upgrade _
    | Reserve _
    | Claim _ -> false

/// The engine's reach for the act: 3 for build, repair and upgrade, 1 for the
/// rest. Where a body must stand is the Atlas's question; this is its input.
let rangeOf (task: Task) : int =
    match task with
    | Build _
    | Repair _
    | Upgrade _ -> 3
    | Harvest _
    | Withdraw _
    | Pickup _
    | Refill _
    | Reserve _
    | Claim _ -> 1
