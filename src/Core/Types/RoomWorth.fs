/// One room's price this tick (#663 M2): what a fight there keeps, denies,
/// kills and takes down, priced once per room per tick over the world and
/// shared by every colony (`WorldView.Worths`, `Facts.roomWorth`). Every
/// reader of a room's value reads this record: the [[sweep]]'s choice and
/// group size, its muster, a contested room's cast, the trespass casts, the
/// outposts' hold spans and the looks. The rate a group earns there is
/// `Facts.roomRate`.
[<AutoOpen>]
module Fabot.Core.Types.Worth

/// A rival structure a fight may take down (#661): what its owner pays to
/// build it again, and the hits our damage takes off it first.
type Standing = { Rebuild: int; Hits: int }

/// One room's worth (#663 M2).
type RoomWorth =
    {
        /// Our income a tick a fight there keeps: our sources' yield at the
        /// rate the room is held at, a declared outpost's at its reserved
        /// yield whichever colony projects it.
        Kept: float
        /// The income a tick each economy body we may strike there takes
        /// (`Facts.harmRate`, #621), by id, before its odds.
        Takes: Map<string, float>
        /// Each body's kill value (`Facts.killValue`, #621), by id, before
        /// its odds: the declared enemy's economy bodies, and its escort in
        /// the sweep's room.
        Values: Map<string, float>
        /// The odds each body still stands there (`Facts.odds`), by id, read
        /// for its value to a fight standing (`Quarry.Reading.Value`) and for
        /// a cast (`Quarry.Reading.Cast`).
        Odds: Map<string, float * float>
        /// The bodies out of sight among them: what a look at the room
        /// confirms (#660).
        Dark: Set<string>
        /// The rival structures standing there a fight may take down
        /// (#661), by id, at their rebuild cost: the enemy's containers the
        /// census names. Roads and sites carry no census in a rival's room.
        Structures: Map<string, Standing>
        /// The flow through a choke of the room, at its value: the
        /// interdiction term, nothing until a camp prices one (S7).
        Exits: float
        /// The armed bodies standing there (`Facts.raidersIn`): the escort a
        /// group's exchange is priced against, ticks to kill and whether it
        /// wins, per group (`Facts.roomRate`).
        Escort: HostileInfo list
        /// The ticks the room's owner answers our first armed body in (the
        /// rival dossier's fastest response, #653), else at once: the worst
        /// case.
        Response: int
    }

[<RequireQualifiedAccess>]
module RoomWorth =
    /// A room worth nothing: nothing kept, taken, killed or standing.
    let empty =
        {
            Kept = 0.0
            Takes = Map.empty
            Values = Map.empty
            Odds = Map.empty
            Dark = Set.empty
            Structures = Map.empty
            Exits = 0.0
            Escort = []
            Response = 0
        }

    /// A body's odds read as `reading`; certain for a body no odds name.
    let oddsOf (reading: Quarry.Reading) (worth: RoomWorth) (id: string) : float =
        match Map.tryFind id worth.Odds, reading with
        | Some(value, _), Quarry.Reading.Value -> value
        | Some(_, cast), Quarry.Reading.Cast -> cast
        | None, _ -> 1.0

    /// Each body's kill at its odds read as `reading`.
    let values (reading: Quarry.Reading) (worth: RoomWorth) : Map<string, float> =
        worth.Values |> Map.map (fun id value -> value * oddsOf reading worth id)

    /// Each body's take at its odds read as `reading`.
    let takes (reading: Quarry.Reading) (worth: RoomWorth) : Map<string, float> =
        worth.Takes |> Map.map (fun id take -> take * oddsOf reading worth id)

    /// The rival's income a tick a fight there denies: every take at its
    /// odds, summed.
    let denied (reading: Quarry.Reading) (worth: RoomWorth) : float =
        takes reading worth |> Map.fold (fun total _ take -> total + take) 0.0

    /// The room's economic delta a tick a fight there moves: our income
    /// kept and the rival's denied.
    let delta (reading: Quarry.Reading) (worth: RoomWorth) : float =
        worth.Kept + denied reading worth

    /// What a look at the room is worth (#660): the kills of the bodies out
    /// of sight there, each at the odds it still stands there.
    let unseen (worth: RoomWorth) : float =
        values Quarry.Reading.Value worth
        |> Map.fold
            (fun total id value -> if worth.Dark.Contains id then total + value else total)
            0.0
