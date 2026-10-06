/// The world's danger, read once a tick a contested room: its hostile maps
/// (`Arrival`), the Reach they lay less our standing ramparts there, and the
/// walkable ground left, built off the world's vision and the room's whole
/// facts, never a colony's cut of them, and laid in every view: every colony
/// that reads a room reads the one object.
module Fabot.Core.WorldDanger

open Fabot.Core.Types

/// The rooms whose danger is read: each room an armed hostile not an ally's
/// or a hostile tower stands in, and each one beside it a crossing hostile
/// there could land on within the horizon, one no farther from the border
/// between than that, a tile a tick at the quickest. A room beside it that
/// none could has no Reach, so no map.
let private contested (horizon: int) (world: WorldView) : string list =
    world.Rooms
    |> Map.toList
    |> List.collect (fun (room, seen) ->
        let armed =
            seen.Hostiles
            |> List.filter (fun hostile ->
                not (Colony.isAlly hostile.Owner)
                && Option.isSome (HostileInfo.weaponRange seen.Hostiles hostile))

        let beside =
            RoomName.adjacent room
            |> List.filter (fun next ->
                armed
                |> List.exists (fun hostile ->
                    HostileInfo.crossesExits hostile
                    && (let at = RoomPos.pos hostile.Pos

                        match RoomName.offsetOf room next with
                        | Some(1, 0) -> Seam.exitEdge - at.X <= horizon
                        | Some(-1, 0) -> at.X <= horizon
                        | Some(0, 1) -> Seam.exitEdge - at.Y <= horizon
                        | Some(0, -1) -> at.Y <= horizon
                        | _ -> false)))

        if List.isEmpty armed && List.isEmpty seen.Towers then
            beside
        else
            room :: beside)
    |> List.distinct

/// The world the danger reads: the world's vision, and each room a view's
/// hostiles stand in that the vision does not answer for, as the views hold
/// them: a raid recalled through a blind tick (#569), or a fixture's.
let private dangerWorld (views: ColonyView list) (watch: WorldView) : WorldView =
    match
        views
        |> List.collect (fun view ->
            view.Hostiles
            |> List.filter (fun hostile -> not (Map.containsKey hostile.Pos.Room watch.Rooms)))
        |> List.distinctBy (fun hostile -> hostile.Id)
    with
    | [] -> watch
    | unseen ->
        { watch with
            Rooms =
                unseen
                |> List.groupBy (fun hostile -> hostile.Pos.Room)
                |> List.fold
                    (fun rooms (room, hostiles) ->
                        Map.add
                            room
                            {
                                Hostiles = hostiles
                                HostileFatigue = Map.empty
                                InvaderCores = []
                                Towers = []
                                OurTowers = []
                                Fallen = []
                                Control =
                                    views
                                    |> List.tryPick (fun view -> Map.tryFind room view.RoomControl)
                                    |> Option.defaultValue
                                        {
                                            Owner = Ownership.Unowned
                                            Reservation = None
                                            SafeMode = false
                                            SafeModeCooldownUntil = 0
                                            Sign = None
                                        }
                            }
                            rooms)
                    watch.Rooms
        }

/// Each contested room's danger in `world`, over each room's facts
/// (`factsOf`, None for a room nothing carries ground for), built on first
/// read: no ground or no class, no danger.
let private tableOver
    (tuning: Tuning)
    (world: WorldView)
    (factsOf: string -> Atlas.GroundFacts option)
    : Map<string, Lazy<RoomDanger option>> =
    let horizon = tuning.ReachHorizon
    let margin = Tuning.keeperMargin tuning

    let floods =
        Arrival.floodsOver (fun room -> factsOf room |> Option.map Atlas.hostileGroundOf) world

    contested horizon world
    |> List.map (fun room ->
        room,
        lazy
            (match floods room, factsOf room with
             | Some arrival, Some facts ->
                 match arrival.Maps.Force() with
                 | { Classes = [] } -> None
                 | maps ->
                     let _, walking = Atlas.groundGrids margin room facts.Layer
                     let ramparts = Atlas.ourRampartTilesOf facts

                     // Where a map strikes within the horizon, less our
                     // standing ramparts there.
                     let reach =
                         Atlas.standingTilesOn
                             walking
                             (Atlas.ringGrid margin room facts.Border)
                             ramparts
                             (fun index ->
                                 Arrival.soonestAmong index maps.Classes System.Int32.MaxValue
                                 <= horizon)

                     Some
                         {
                             HostileMaps = maps
                             Strikers = lazy (Arrival.strikersOf world room maps)
                             Reach = reach
                             Ramparts = ramparts
                             Safe = lazy (Atlas.walkableTilesExceptOn walking room reach)
                         }
             | _ -> None))
    |> Map.ofList

/// Every contested room's danger read over the views together: each room's
/// facts the world's (`WorldView.Facts`), else as a view that projects it
/// cuts them (a stray body's room, or a fixture with no world).
let private ofViews (views: ColonyView list) : Map<string, Lazy<RoomDanger option>> =
    match views with
    | [] -> Map.empty
    | first :: _ ->
        let watch = first.World

        let factsOf (room: string) =
            match Map.tryFind room watch.Facts with
            | Some facts -> Some(Atlas.groundFactsOf facts)
            | None ->
                views |> List.tryPick (fun view -> Atlas.projectedGroundFacts view.Spatial room)

        tableOver first.Tuning (dangerWorld views watch) factsOf

/// The views with the world's danger laid in, one table for all of them.
let lay (views: ColonyView list) : ColonyView list =
    let table = ofViews views

    views
    |> List.map (fun view ->
        { view with
            World = { view.World with Dangers = Some table }
        })

/// A view's danger: the world's laid in it, else read off this view alone
/// (a fixture deciding one view).
let read (view: ColonyView) : Map<string, Lazy<RoomDanger option>> =
    match view.World.Dangers with
    | Some table -> table
    | None -> ofViews [ view ]
