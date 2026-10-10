/// What crosses a tick (`ARCHITECTURE.md` §4). Each field has one owner; the
/// shell reads it from Memory and segments and writes it back once a tick.
module Fabot.Core.State

type State =
    {
        /// Modules the operator turned off (`Memory.v2.switches`): a module
        /// named here is skipped and its last output is not replayed.
        Off: Set<string>
        /// Creep name → the id of the Task it holds (`Memory.v2.assignments`).
        /// Owned by Economy.
        Assignments: Map<string, string>
    }

let empty =
    {
        Off = Set.empty
        Assignments = Map.empty
    }
