/// The caller reads the creep's pace once (`Grid.factorOf`) and hands it in.
module Fabot.Core.Move.Plan

open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Grid

let canMove (creep: Creep) (factor: FatigueFactor) : bool = creep.Fatigue = 0 && factor.Moves > 0

let private beside (atlas: Atlas) (creep: Creep) : int[] =
    let at = Tiles.ofPos creep.Pos

    Tiles.within 1 at
    |> Array.filter (fun tile -> tile <> at && Atlas.passable atlas creep.Pos.Room tile)

let private wish
    (creep: Creep)
    (moves: bool)
    (head: int)
    (onward: int)
    (tail: int[])
    (working: int[])
    (weight: int)
    : Wish =
    {
        Creep = creep.Name
        Footprint = One
        At = Tiles.ofPos creep.Pos
        Head = head
        Onward = onward
        Tail = tail
        Working = working
        Weight = weight
        Moves = moves
    }

let stay (atlas: Atlas) (creep: Creep) (factor: FatigueFactor) (weight: int) : Wish =
    let at = Tiles.ofPos creep.Pos
    let tail = beside atlas creep |> Array.filter (Tiles.onEdge >> not)
    wish creep (canMove creep factor) at -1 tail [||] weight

/// The tail is the way round a body the Atlas's walk does not see.
let toward (atlas: Atlas) (creep: Creep) (mover: Mover) (target: Target) (weight: int) : Wish =
    let room = creep.Pos.Room
    let at = Tiles.ofPos creep.Pos

    match
        (if canMove creep mover.Factor then
             Atlas.field atlas mover target room
         else
             None)
    with
    | None -> stay atlas creep mover.Factor weight
    | Some field ->
        let ticks (tile: int) =
            Field.ticks field tile |> Option.defaultValue unreached

        let here = ticks at

        if here = 0 && room = target.Room then
            let inside, outside =
                beside atlas creep
                |> Array.filter (Tiles.onEdge >> not)
                |> Array.partition (fun tile -> ticks tile = 0)

            wish
                creep
                true
                at
                -1
                (Array.append inside outside)
                (Array.append [| at |] inside)
                weight
        else
            match Field.step field at with
            | None -> stay atlas creep mover.Factor weight
            | Some direction ->
                let head = Tiles.neighbour at direction

                let onward =
                    match Field.step field head with
                    | Some next -> Tiles.neighbour head next
                    | None -> -1

                let tail =
                    beside atlas creep
                    |> Array.filter (fun tile ->
                        tile <> head
                        && (ticks tile < here
                            || (ticks tile < unreached && Tiles.range tile head = 1))
                        && (ticks tile = 0 || not (Tiles.onEdge tile)))
                    |> Array.sortBy (fun tile -> ticks tile, tile)

                wish creep true head onward tail [||] weight
