/// Fight and season work are not Tasks: a creep in an engagement is not in
/// the economy's pool at all (`ARCHITECTURE.md` §6.2).
module Fabot.Core.Economy.Task

open Fabot.Core.Vocabulary

/// Compared only through `taskId`, which `assignments` keys on.
[<NoEquality; NoComparison>]
type Task =
    | Harvest of target: string
    | Withdraw of store: string * resource: Resource
    | Pickup of pile: string * resource: Resource
    | Refill of structure: string * resource: Resource
    | Build of site: string
    | Repair of structure: string
    | Upgrade of controller: string
    | Reserve of controller: string
    | Claim of controller: string

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
