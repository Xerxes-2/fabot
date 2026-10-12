/// What crosses a tick (`ARCHITECTURE.md` §4); each field has one owner.
module Fabot.Core.State

type State =
    {
        /// A module named here is skipped and its last output not replayed.
        Off: Set<string>
        Assignments: Map<string, string>
        /// Heap only, never in Memory: empty after a reset, so the first tick redraws.
        Layout: Map<string, Layout.Shape.Standing>
        Groups: Fight.Shape.Group list
        Engagements: Fight.Shape.Engagement list
    }

let empty =
    {
        Off = Set.empty
        Assignments = Map.empty
        Layout = Map.empty
        Groups = []
        Engagements = []
    }
