/// Collections that capture no scope (#401). Fable emits a `Map`'s or `Set`'s
/// comparer as an arrow at the construction site, and V8 keeps the defining
/// function's whole context alive through a closure — every variable a
/// sibling closure captured. A collection that outlives its tick must be
/// built here, where nothing is captured: measured 2026-09-22, each tick's
/// Transition log retained the previous tick's through its comparer, ~28 KB
/// a tick until a reset.
module Fabot.Core.Types.Fresh

let mapOfList (pairs: ('k * 'v) list) : Map<'k, 'v> = Map.ofList pairs

let mapOfSeq (pairs: seq<'k * 'v>) : Map<'k, 'v> = Map.ofSeq pairs

let mapOfArray (pairs: ('k * 'v)[]) : Map<'k, 'v> = Map.ofArray pairs

let setOfSeq (items: seq<'k>) : Set<'k> = Set.ofSeq items
