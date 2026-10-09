/// The arena suites' shared surface: the plain ground, the raid bodies and the
/// room positions more than one arena file reads. A helper one file alone
/// reads stays private there (`docs/agents/orchestration.md`, the fixture tiers).
module Fabot.Core.Tests.ArenaFixtures

open Fabot.Core.Types
open Fabot.Core.Tests.Arena

/// Open plain ground in W16S25, the swamp patch west of it, and the west
/// exit at y 28 that joins W17S25's east exit.
let internal plainRoom = "W16S25"

let internal at (x: int) (y: int) = RoomPos.at plainRoom { X = x; Y = y }

let internal tile (x: int) (y: int) : Pos = { X = x; Y = y }

/// A structure's hits at the end of the run, or None once it is gone.
let internal hitsLeft (id: string) (trace: TickTrace list) : int option =
    trace |> List.tryLast |> Option.bind (fun t -> Map.tryFind id t.Structures)

/// The outpost a melee guard defends: W12S28 at RCL7 with its declared
/// W12S27, a hauler of ours standing by W12S27's source for vision.
let internal outpostRaid (invader: Body) =
    let home =
        room "W12S28"
        |> withController Ownership.Ours None 7 0
        |> withSpawn "Spawn1" { X = 12; Y = 38 } 5600

    let outpost = room "W12S27"

    let declared =
        { colony "W12S28" with
            Outposts = [ outpostOf outpost.Capture ]
        }

    let guard =
        body "guard-1-Spawn1" Side.Ours (ourGuard 1) (RoomPos.at "W12S28" { X = 20; Y = 5 }) None

    let hauler =
        body
            "hauler-1-Spawn1"
            Side.Ours
            (parts [ Carry, 2; Move, 2 ])
            (RoomPos.at "W12S27" { X = 13; Y = 46 })
            None

    arena 900_000 [ home; outpost ] [ declared ] [ guard; hauler; invader ]

/// Trepidimous' bodies (`docs/research/squads.md` §1.2, the t878,470 JSON):
/// MOVE first, the weapon, one MOVE last; the healer MOVE then HEAL.
let internal trepMelee = parts [ Move, 17; Attack, 17; Move, 1 ]
let internal trepHealer = parts [ Move, 11; Heal, 7 ]
let internal trepTapper = parts [ Move, 3; BodyPart.Claim, 3 ]
let internal trep = Side.Player "Trepidimous"

/// One resident R7 as the ranger row cast it before #672 (the captures'
/// live body): `21M 14R 7H`, MOVE first.
let internal r7 = parts [ Move, 21; RangedAttack, 14; Heal, 7 ]

let internal w17s25 (x: int) (y: int) = RoomPos.at "W17S25" { X = x; Y = y }

let internal w18s25 (x: int) (y: int) = RoomPos.at "W18S25" { X = x; Y = y }

/// Odiodin's garrison (§1.4): `20M16R4H`, MOVE first.
let internal garrison = parts [ Move, 20; RangedAttack, 16; Heal, 4 ]
let internal odiodin = Side.Player "Odiodin"

/// The rooms one body stood in, tick by tick.
let internal roomsOf (id: string) (trace: TickTrace list) =
    pathOf id trace |> List.map (fun (_, s) -> s.At.Room) |> List.distinct
