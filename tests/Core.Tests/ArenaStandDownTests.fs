/// An outpost's stand-down in the arena: the w18s25-crippled capture (#573), a
/// crippled melee crawling through a shut outpost while a second raid strikes
/// the home next door. Its rooms are `ArenaStandDownRooms`' and `ArenaStandDownOuterRooms`'.
module Fabot.Core.Tests.ArenaStandDownTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Tests.Arena
open Fabot.Core.Tests.ArenaStandDownRooms
open Fabot.Core.Tests.ArenaStandDownOuterRooms

// ---- arena capture w18s25-crippled (#573) -------------------------------------
// `npm run capture -- W18S25 994917 --rooms W17S25,W15S28,W19S25,W17S29,W17S26,W19S26,W13S28,W12S28,W12S26,W18S27,W18S28,W18S29,W19S28,W19S29,W16S25,W16S27,W16S29,W15S25,W15S27,W15S29,W14S28,W18S26,W17S24 --name w18s25-crippled --live-memory`, read at live t995,234.
// The room-history replay at t994,917: arena tick 0 runs as t994,918.
// Faraday387 (15 ATTACK on one MOVE, no healer) crawls west through W18S25 on
// its live tiles; its healer Intercept171 died at t994,882, a tick after
// its Lose opened the stand-down (PlayerRaidHold). Photon713 and Pendulum654
// raid W17S25's east side on their live tiles from t994,920. The W18S25 row
// is seated as live Memory held it at t995,011 (its raid bodies, its player
// clock and `Chased`): the replay read at t995,234 holds the row spent.

/// Live t994,917 (#573): a crippled melee crawling through W17S25's outpost
/// W18S25, shut on a player's raid its healer no longer heals, while a second
/// raid strikes W17S25's home next door.
let private w18s25Crippled () : Arena =
    let at roomName (x: int) (y: int) = RoomPos.at roomName { X = x; Y = y }
    let trepidimous = Side.Player "Trepidimous"
    let giaco = Side.Player "giaco"
    let kazkel = Side.Player "Kazkel"
    let odiodin = Side.Player "Odiodin"

    // Today's declarations, not the tick's: spell them out if the test must outlive a roster change.
    let colonies =
        Colony.declared
        |> List.filter (fun c ->
            List.contains c.Home [ "W17S25"; "W15S28"; "W17S29"; "W13S28"; "W12S28"; "W12S26" ])

    let bodies: Body list =
        [
            // Faraday387: live: 15 ATTACK, 1 MOVE
            // Faraday387: TODO script; live: no act
            { body
                  "Faraday387"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W18S25" 44 13)
                  (Some(
                      Waypoints
                          [
                              at "W18S25" 43 13
                              at "W18S25" 42 13
                              at "W18S25" 41 13
                              at "W18S25" 40 13
                              at "W18S25" 39 14
                              at "W18S25" 38 15
                              at "W18S25" 37 16
                              at "W18S25" 36 17
                              at "W18S25" 35 16
                              at "W18S25" 34 15
                              at "W18S25" 33 14
                          ]
                  )) with
                Hits = 1_544
                TicksToLive = 1_073
                Fatigue = 13
            }
            { body
                  "ranger-993409-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 16 34)
                  None with
                TicksToLive = 116
            }
            { body
                  "worker-993432-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 7; Move, 7 ])
                  (at "W17S25" 16 39)
                  None with
                TicksToLive = 73
                Energy = 248
            }
            { body
                  "anchor-993520-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S25" 17 20)
                  None with
                TicksToLive = 125
                Energy = 50
            }
            { body
                  "hauler-993574-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 10 23)
                  None with
                TicksToLive = 227
            }
            { body
                  "upgrader-993726-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 18 35)
                  None with
                TicksToLive = 358
            }
            { body
                  "guard-994071-Spawn11"
                  Side.Ours
                  (parts [ Tough, 1; Move, 5; Attack, 3; Heal, 1 ])
                  (at "W17S25" 13 47)
                  None with
                TicksToLive = 682
            }
            { body
                  "hauler-994306-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 19 30)
                  None with
                TicksToLive = 959
            }
            { body
                  "hauler-994378-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 18 34)
                  None with
                TicksToLive = 1_031
                Energy = 800
            }
            { body
                  "ranger-994540-Spawn3"
                  Side.Ours
                  (parts [ Move, 21; RangedAttack, 14; Heal, 7 ])
                  (at "W17S25" 26 31)
                  None with
                TicksToLive = 1_247
            }
            { body
                  "upgrader-994568-Spawn11"
                  Side.Ours
                  (parts [ Work, 8; Carry, 1; Move, 8 ])
                  (at "W17S25" 17 32)
                  None with
                TicksToLive = 1_200
            }
            { body
                  "defender-994619-Spawn11"
                  Side.Ours
                  (parts [ Move, 10; Attack, 10 ])
                  (at "W17S25" 3 17)
                  None with
                TicksToLive = 1_260
            }
            { body
                  "hauler-994803-Spawn11"
                  Side.Ours
                  (parts [ Carry, 16; Move, 8 ])
                  (at "W17S25" 12 27)
                  None with
                TicksToLive = 1_456
                Energy = 256
            }
            { body
                  "ranger-994875-Spawn11"
                  Side.Ours
                  (parts [ Move, 3; RangedAttack, 2; Heal, 1 ])
                  (at "W17S25" 12 25)
                  None with
                TicksToLive = 1_474
            }
            { body
                  "reserver-994679-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S25" 3 18)
                  None with
                TicksToLive = 372
            }
            // Photon713: live: 15 MOVE, 13 ATTACK
            // Photon713: TODO script; live: healed→49,28
            { body
                  "Photon713"
                  trepidimous
                  (parts [ Move, 14; Attack, 13; Move, 1 ])
                  (at "W17S25" 47 28)
                  (Some(
                      Waypoints
                          [
                              at "W17S25" 46 28
                              at "W17S25" 45 28
                              at "W17S25" 44 29
                              at "W17S25" 43 30
                              at "W17S25" 42 30
                              at "W17S25" 41 30
                              at "W17S25" 40 30
                              at "W17S25" 39 30
                              at "W17S25" 38 30
                          ]
                  )) with
                Hits = 2_722
                TicksToLive = 1_378
            }
            // Pendulum654: live: 5 MOVE, 6 HEAL
            // Pendulum654: TODO script; live: attacked→13,29 heal→48,28
            { body
                  "Pendulum654"
                  trepidimous
                  (parts [ Move, 6; Heal, 6 ])
                  (at "W17S25" 48 28)
                  (Some(Follow "Photon713")) with
                Hits = 1_050
                TicksToLive = 1_271
            }
            { body
                  "upgrader-993505-Spawn8"
                  Side.Ours
                  (parts [ Work, 24; Carry, 1; Move, 24 ])
                  (at "W15S28" 22 28)
                  None with
                TicksToLive = 233
                Energy = 26
            }
            { body
                  "worker-994186-Spawn8"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W15S28" 22 30)
                  None with
                TicksToLive = 917
                Energy = 594
            }
            { body
                  "anchor-994580-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 11 18)
                  None with
                TicksToLive = 1_185
                Energy = 50
            }
            { body
                  "anchor-994748-Spawn8"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W15S28" 6 29)
                  None with
                TicksToLive = 1_353
                Energy = 50
            }
            { body
                  "hauler-994772-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S28" 19 32)
                  None with
                TicksToLive = 1_497
            }
            { body
                  "ranger-994078-Spawn9"
                  Side.Ours
                  (parts [ Move, 9; RangedAttack, 6; Heal, 3 ])
                  (at "W19S25" 8 48)
                  None with
                TicksToLive = 713
            }
            { body
                  "worker-993452-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 27 37)
                  None with
                TicksToLive = 138
                Energy = 138
            }
            { body
                  "worker-993584-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 27 36)
                  None with
                TicksToLive = 270
                Energy = 6
            }
            { body
                  "guard-993724-Spawn9"
                  Side.Ours
                  (parts [ Tough, 2; Move, 10; Attack, 6; Heal, 2 ])
                  (at "W17S29" 47 9)
                  None with
                TicksToLive = 365
            }
            { body
                  "guard-993784-Spawn9"
                  Side.Ours
                  (parts [ Tough, 2; Move, 10; Attack, 6; Heal, 2 ])
                  (at "W17S29" 29 35)
                  None with
                TicksToLive = 425
            }
            { body
                  "worker-993966-Spawn9"
                  Side.Ours
                  (parts [ Work, 11; Carry, 12; Move, 12 ])
                  (at "W17S29" 31 33)
                  None with
                TicksToLive = 652
                Energy = 534
            }
            { body
                  "hauler-994452-Spawn9"
                  Side.Ours
                  (parts [ Carry, 30; Move, 15 ])
                  (at "W17S29" 22 30)
                  None with
                TicksToLive = 1_168
            }
            { body
                  "anchor-994587-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 13 29)
                  None with
                TicksToLive = 1_192
                Energy = 50
            }
            { body
                  "anchor-994611-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W17S29" 44 38)
                  None with
                TicksToLive = 1_216
                Energy = 50
            }
            { body
                  "upgrader-994821-Spawn9"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W17S29" 26 41)
                  None with
                TicksToLive = 1_495
                Energy = 50
            }
            // Quantum825: TODO script; live: no act
            { body
                  "Quantum825"
                  trepidimous
                  (parts [ Move, 3; Work, 5 ])
                  (at "W17S26" 21 2)
                  (Some Hold) with
                TicksToLive = 1_430
            }
            { body
                  "anchor-993654-Spawn11"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W17S26" 27 9)
                  None with
                TicksToLive = 265
                Energy = 50
            }
            { body
                  "reserver-994462-Spawn11"
                  Side.Ours
                  (parts [ BodyPart.Claim, 2; Move, 2 ])
                  (at "W17S26" 9 22)
                  None with
                TicksToLive = 155
            }
            { body
                  "ranger-994398-Spawn9"
                  Side.Ours
                  (parts [ Move, 9; RangedAttack, 6; Heal, 3 ])
                  (at "W19S26" 24 35)
                  None with
                TicksToLive = 1_033
            }
            { body
                  "worker-993508-Spawn2"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W13S28" 19 12)
                  None with
                TicksToLive = 239
                Energy = 703
            }
            { body
                  "hauler-993704-Spawn2"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W13S28" 15 10)
                  None with
                TicksToLive = 429
                Energy = 1_058
            }
            { body
                  "upgrader-993898-Spawn2"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W13S28" 23 14)
                  None with
                TicksToLive = 572
                Energy = 5
            }
            { body
                  "anchor-994184-Spawn2"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W13S28" 15 8)
                  None with
                TicksToLive = 789
                Energy = 50
            }
            { body
                  "anchor-994396-Spawn2"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W13S28" 18 3)
                  None with
                TicksToLive = 1_001
                Energy = 50
            }
            { body
                  "hauler-993532-Spawn6"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W12S28" 8 38)
                  None with
                TicksToLive = 257
                Energy = 230
            }
            { body
                  "anchor-993806-Spawn1"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W12S28" 10 43)
                  None with
                TicksToLive = 411
                Energy = 50
            }
            { body
                  "anchor-993828-Spawn6"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W12S28" 16 39)
                  None with
                TicksToLive = 433
                Energy = 50
            }
            { body
                  "worker-993828-Spawn5"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W12S28" 6 14)
                  None with
                TicksToLive = 559
                Energy = 786
            }
            { body
                  "upgrader-994294-Spawn1"
                  Side.Ours
                  (parts [ Work, 15; Carry, 1; Move, 15 ])
                  (at "W12S28" 6 40)
                  None with
                TicksToLive = 968
                Energy = 35
            }
            { body
                  "hauler-993650-Spawn1"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W12S28" 48 32)
                  None with
                TicksToLive = 375
                Energy = 860
            }
            { body
                  "worker-993918-Spawn1"
                  Side.Ours
                  (parts [ Work, 16; Carry, 17; Move, 17 ])
                  (at "W12S28" 48 30)
                  None with
                TicksToLive = 649
                Fatigue = 32
                Energy = 850
            }
            { body
                  "hauler-993624-Spawn10"
                  Side.Ours
                  (parts [ Carry, 22; Move, 11 ])
                  (at "W12S26" 31 35)
                  None with
                TicksToLive = 304
                Energy = 1_020
            }
            { body
                  "hauler-993812-Spawn10"
                  Side.Ours
                  (parts [ Carry, 22; Move, 11 ])
                  (at "W12S26" 38 30)
                  None with
                TicksToLive = 492
                Energy = 600
            }
            { body
                  "hauler-993992-Spawn10"
                  Side.Ours
                  (parts [ Carry, 22; Move, 11 ])
                  (at "W12S26" 13 48)
                  None with
                TicksToLive = 672
            }
            { body
                  "upgrader-994266-Spawn10"
                  Side.Ours
                  (parts [ Work, 11; Carry, 1; Move, 11 ])
                  (at "W12S26" 7 35)
                  None with
                TicksToLive = 916
                Energy = 39
            }
            { body
                  "worker-994608-Spawn10"
                  Side.Ours
                  (parts [ Work, 9; Carry, 9; Move, 9 ])
                  (at "W12S26" 42 38)
                  None with
                TicksToLive = 1_270
                Energy = 314
            }
            { body
                  "anchor-994712-Spawn10"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 1 ])
                  (at "W12S26" 42 39)
                  None with
                TicksToLive = 1_317
                Energy = 50
            }
            // HalfLife418: TODO script; live: no act
            { body
                  "HalfLife418"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                      ])
                  (at "W18S27" 20 2)
                  (Some Hold) with
                TicksToLive = 1_113
            }
            // Force618: TODO script; live: no act
            { body "Force618" trepidimous (parts [ Move, 1 ]) (at "W18S27" 23 1) (Some Hold) with
                TicksToLive = 1_202
            }
            { body
                  "ranger-994340-Spawn9"
                  Side.Ours
                  (parts [ Move, 9; RangedAttack, 6; Heal, 3 ])
                  (at "W18S27" 26 8)
                  None with
                TicksToLive = 975
            }
            // RRP: TODO script; live: harvest→11,5
            { body "RRP" giaco (parts [ Work, 6; Carry, 1; Move, 6 ]) (at "W18S29" 10 6) (Some Hold) with
                TicksToLive = 71
                Energy = 50
            }
            // cS5: TODO script; live: no act
            { body
                  "cS5"
                  giaco
                  (parts
                      [
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                      ])
                  (at "W18S29" 2 22)
                  (Some Hold) with
                TicksToLive = 182
            }
            // odc: TODO script; live: no act
            { body "odc" giaco (parts [ Move, 1 ]) (at "W18S29" 2 16) (Some Hold) with
                TicksToLive = 792
            }
            // bdf: TODO script; live: no act
            { body
                  "bdf"
                  giaco
                  (parts
                      [
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                      ])
                  (at "W18S29" 17 43)
                  (Some Hold) with
                TicksToLive = 846
                Energy = 238
            }
            // viq: TODO script; live: reserveController→38,43
            { body
                  "viq"
                  giaco
                  (parts [ Move, 2; BodyPart.Claim, 2 ])
                  (at "W18S29" 37 42)
                  (Some Hold) with
                TicksToLive = 225
            }
            // oS2: TODO script; live: no act
            { body "oS2" giaco (parts [ Move, 1 ]) (at "W19S28" 2 2) (Some Hold) with
                TicksToLive = 74
            }
            // RTp: TODO script; live: harvest→44,24
            { body
                  "RTp"
                  giaco
                  (parts [ Work, 6; Carry, 1; Move, 6 ])
                  (at "W19S28" 43 23)
                  (Some Hold) with
                TicksToLive = 221
                Energy = 50
            }
            // cUS: TODO script; live: no act
            { body
                  "cUS"
                  giaco
                  (parts
                      [
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                      ])
                  (at "W19S28" 14 45)
                  (Some Hold) with
                TicksToLive = 329
                Energy = 1_200
            }
            // RYR: TODO script; live: harvest→19,40
            { body
                  "RYR"
                  giaco
                  (parts [ Work, 6; Carry, 1; Move, 6 ])
                  (at "W19S28" 20 41)
                  (Some Hold) with
                TicksToLive = 507
                Energy = 50
            }
            // vj2: TODO script; live: reserveController→16,28
            { body
                  "vj2"
                  giaco
                  (parts [ Move, 2; BodyPart.Claim, 2 ])
                  (at "W19S28" 15 29)
                  (Some Hold) with
                TicksToLive = 237
            }
            // uWC: TODO script; live: upgradeController→41,23
            { body "uWC" giaco (parts [ Carry, 6; Work, 15 ]) (at "W19S29" 40 21) (Some Hold) with
                TicksToLive = 392
                Energy = 180
            }
            // cXr: TODO script; live: no act
            { body
                  "cXr"
                  giaco
                  (parts
                      [ Carry, 2; Move, 1; Carry, 2; Move, 1; Carry, 2; Move, 1; Carry, 2; Move, 1 ])
                  (at "W19S29" 27 37)
                  (Some Hold) with
                TicksToLive = 468
            }
            // gZG: TODO script; live: no act
            { body "gZG" giaco (parts [ Carry, 8; Move, 1 ]) (at "W19S29" 42 22) (Some Hold) with
                TicksToLive = 546
                Energy = 400
            }
            // gZh: TODO script; live: no act
            { body "gZh" giaco (parts [ Carry, 4; Move, 1 ]) (at "W19S29" 36 47) (Some Hold) with
                TicksToLive = 561
                Energy = 200
            }
            // dbg: TODO script; live: no act
            { body
                  "dbg"
                  giaco
                  (parts
                      [
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                      ])
                  (at "W19S29" 36 16)
                  (Some Hold) with
                TicksToLive = 729
                Energy = 500
            }
            // dce: TODO script; live: no act
            { body
                  "dce"
                  giaco
                  (parts
                      [
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                      ])
                  (at "W19S29" 38 22)
                  (Some Hold) with
                TicksToLive = 789
                Energy = 500
            }
            // ufW: TODO script; live: upgradeController→41,23
            { body "ufW" giaco (parts [ Carry, 6; Work, 15 ]) (at "W19S29" 40 22) (Some Hold) with
                TicksToLive = 970
                Energy = 225
            }
            // chF: TODO script; live: no act
            { body
                  "chF"
                  giaco
                  (parts
                      [
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                      ])
                  (at "W19S29" 29 39)
                  (Some Hold) with
                TicksToLive = 1_113
            }
            // skO: TODO script; live: harvest→39,13
            { body
                  "skO"
                  giaco
                  (parts [ Carry, 1; Work, 3; Move, 3 ])
                  (at "W19S29" 40 14)
                  (Some Hold) with
                TicksToLive = 1_230
                Energy = 50
            }
            // cZw: TODO script; live: no act
            { body
                  "cZw"
                  giaco
                  (parts
                      [
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                      ])
                  (at "W19S29" 37 46)
                  (Some Hold) with
                TicksToLive = 669
            }
            // Keeper6a8caa89dd4872bccd318e34: TODO script; live: no act
            { body
                  "Keeper6a8caa89dd4872bccd318e34"
                  (Side.Npc "Source Keeper")
                  (parts
                      [
                          Tough, 17
                          Move, 13
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                      ])
                  (at "W16S25" 7 8)
                  (Some Hold) with
                TicksToLive = 526
            }
            // Keeper6a8caa89dd4872bccd318e35: TODO script; live: no act
            { body
                  "Keeper6a8caa89dd4872bccd318e35"
                  (Side.Npc "Source Keeper")
                  (parts
                      [
                          Tough, 17
                          Move, 13
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                      ])
                  (at "W16S25" 41 18)
                  (Some Hold) with
                TicksToLive = 960
            }
            // Keeper6a8caa89dd4872bccd318e38: TODO script; live: no act
            { body
                  "Keeper6a8caa89dd4872bccd318e38"
                  (Side.Npc "Source Keeper")
                  (parts
                      [
                          Tough, 17
                          Move, 13
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                      ])
                  (at "W16S25" 11 38)
                  (Some Hold) with
                TicksToLive = 960
            }
            // Keeper6a8caa89dd4872bccd318e39: TODO script; live: no act
            { body
                  "Keeper6a8caa89dd4872bccd318e39"
                  (Side.Npc "Source Keeper")
                  (parts
                      [
                          Tough, 17
                          Move, 13
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                          Attack, 1
                          RangedAttack, 1
                      ])
                  (at "W16S25" 45 44)
                  (Some Hold) with
                TicksToLive = 960
            }
            // Pendulum654: crossing to W17S25 49,28
            // Pendulum654: TODO script; live: healed→1,27 heal→1,27
            { body
                  "Pendulum654"
                  trepidimous
                  (parts [ Move, 6; Heal, 6 ])
                  (at "W16S25" 0 28)
                  (Some Hold) with
                TicksToLive = 1_271
            }
            // scout-W19S11-993775: TODO script; live: no act
            { body "scout-W19S11-993775" kazkel (parts [ Move, 1 ]) (at "W16S27" 42 2) (Some Hold) with
                TicksToLive = 359
            }
            { body
                  "anchor-993844-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W16S29" 16 42)
                  None with
                TicksToLive = 455
                Energy = 50
            }
            { body
                  "anchor-994292-Spawn9"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W16S29" 22 21)
                  None with
                TicksToLive = 903
                Energy = 50
            }
            { body
                  "hauler-994686-Spawn9"
                  Side.Ours
                  (parts [ Carry, 30; Move, 15 ])
                  (at "W16S29" 6 15)
                  None with
                TicksToLive = 1_402
                Energy = 36
            }
            { body
                  "hauler-994145-Spawn9"
                  Side.Ours
                  (parts [ Carry, 30; Move, 15 ])
                  (at "W16S29" 2 31)
                  None with
                TicksToLive = 861
            }
            // garrison-W14S22-993494: TODO script; live: no act
            { body
                  "garrison-W14S22-993494"
                  odiodin
                  (parts [ Move, 20; RangedAttack, 16; Heal, 4 ])
                  (at "W15S25" 45 8)
                  (Some Hold) with
                TicksToLive = 195
            }
            // garrison-W14S22-993614: TODO script; live: no act
            { body
                  "garrison-W14S22-993614"
                  odiodin
                  (parts [ Move, 20; RangedAttack, 16; Heal, 4 ])
                  (at "W15S25" 44 8)
                  (Some Hold) with
                TicksToLive = 315
            }
            // reactor_guard-W12S23-993895: TODO script; live: no act
            { body
                  "reactor_guard-W12S23-993895"
                  odiodin
                  (parts [ Move, 25; RangedAttack, 20; Heal, 5 ])
                  (at "W15S25" 41 3)
                  (Some Hold) with
                TicksToLive = 626
            }
            // reactor_claimer-W12S23-994532: TODO script; live: no act
            { body
                  "reactor_claimer-W12S23-994532"
                  odiodin
                  (parts [ BodyPart.Claim, 1; Move, 2 ])
                  (at "W15S25" 43 5)
                  (Some Hold) with
                TicksToLive = 222
            }
            { body
                  "anchor-994204-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W15S27" 13 29)
                  None with
                TicksToLive = 815
                Energy = 50
            }
            { body
                  "reserver-994408-Spawn8"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W15S27" 5 9)
                  None with
                TicksToLive = 95
            }
            { body
                  "hauler-994604-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S27" 11 31)
                  None with
                TicksToLive = 1_329
                Energy = 1_376
            }
            { body
                  "reserver-994574-Spawn8"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W15S29" 13 35)
                  None with
                TicksToLive = 261
            }
            { body
                  "hauler-994430-Spawn8"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W15S29" 7 9)
                  None with
                TicksToLive = 1_155
                Energy = 536
            }
            { body
                  "anchor-994174-Spawn3"
                  Side.Ours
                  (parts [ Work, 6; Carry, 1; Move, 3 ])
                  (at "W14S28" 6 9)
                  None with
                TicksToLive = 785
                Energy = 50
            }
            { body
                  "reserver-994328-Spawn3"
                  Side.Ours
                  (parts [ BodyPart.Claim, 1; Move, 1 ])
                  (at "W14S28" 21 14)
                  None with
                TicksToLive = 15
            }
            { body
                  "hauler-994396-Spawn3"
                  Side.Ours
                  (parts [ Carry, 32; Move, 16 ])
                  (at "W14S28" 7 12)
                  None with
                TicksToLive = 1_121
            }
            // Blade579: TODO script; live: healed→19,12 heal→19,12
            { body
                  "Blade579"
                  trepidimous
                  (parts [ Move, 11; Heal, 7 ])
                  (at "W18S26" 19 12)
                  (Some Hold) with
                TicksToLive = 214
            }
            // Enigma633: TODO script; live: no act
            { body
                  "Enigma633"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                      ])
                  (at "W18S26" 28 15)
                  (Some Hold) with
                TicksToLive = 295
                Energy = 262
            }
            // Ice714: TODO script; live: harvest→32,4
            { body
                  "Ice714"
                  trepidimous
                  (parts [ Move, 3; Work, 5; Carry, 2 ])
                  (at "W18S26" 31 4)
                  (Some Hold) with
                TicksToLive = 336
                Energy = 70
            }
            // Darkness755: TODO script; live: no act
            { body
                  "Darkness755"
                  trepidimous
                  (parts [ Move, 1; Carry, 16 ])
                  (at "W18S26" 21 15)
                  (Some Hold) with
                TicksToLive = 464
            }
            // Livewire883: TODO script; live: upgradeController→11,21
            { body
                  "Livewire883"
                  trepidimous
                  (parts [ Move, 1; Work, 1; Carry, 1 ])
                  (at "W18S26" 13 19)
                  (Some Hold) with
                TicksToLive = 473
                Energy = 17
            }
            // Exponential046: TODO script; live: no act
            { body
                  "Exponential046"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                      ])
                  (at "W18S26" 23 19)
                  (Some Hold) with
                TicksToLive = 717
            }
            // Plane136: TODO script; live: no act
            { body
                  "Plane136"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                      ])
                  (at "W18S26" 22 14)
                  (Some Hold) with
                TicksToLive = 807
                Energy = 950
            }
            // Amplify226: TODO script; live: harvest→23,43
            { body
                  "Amplify226"
                  trepidimous
                  (parts [ Move, 3; Work, 5; Carry, 2 ])
                  (at "W18S26" 24 42)
                  (Some Hold) with
                TicksToLive = 837
                Energy = 50
            }
            // Bang256: TODO script; live: no act
            { body
                  "Bang256"
                  trepidimous
                  (parts [ Move, 1; Carry, 2; Move, 1; Carry, 1 ])
                  (at "W18S26" 19 14)
                  (Some Hold) with
                TicksToLive = 852
            }
            // Coefficient271: TODO script; live: no act
            { body
                  "Coefficient271"
                  trepidimous
                  (parts [ Move, 1; Carry, 2; Move, 1; Carry, 1 ])
                  (at "W18S26" 21 13)
                  (Some Hold) with
                TicksToLive = 867
                Energy = 150
            }
            // Entropy286: TODO script; live: no act
            { body
                  "Entropy286"
                  trepidimous
                  (parts [ Move, 17; Attack, 17; Move, 1 ])
                  (at "W18S26" 20 11)
                  (Some Hold) with
                TicksToLive = 972
            }
            // Theory720: TODO script; live: healed→21,12 heal→21,12
            { body
                  "Theory720"
                  trepidimous
                  (parts [ Move, 11; Heal, 7 ])
                  (at "W18S26" 21 12)
                  (Some Hold) with
                TicksToLive = 1_355
            }
            // Redshift849: TODO script; live: healed→18,13 heal→18,13
            { body
                  "Redshift849"
                  trepidimous
                  (parts [ Move, 11; Heal, 7 ])
                  (at "W18S26" 17 12)
                  (Some Hold) with
                TicksToLive = 1_496
            }
            // Dread409: TODO script; live: no act
            { body
                  "Dread409"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                      ])
                  (at "W17S24" 22 37)
                  (Some Hold) with
                TicksToLive = 80
                Fatigue = 160
                Energy = 974
            }
            // Specter499: TODO script; live: harvest→13,10
            { body
                  "Specter499"
                  trepidimous
                  (parts [ Move, 3; Work, 5 ])
                  (at "W17S24" 12 11)
                  (Some Hold) with
                TicksToLive = 104
            }
            // Blazer631: TODO script; live: no act
            { body
                  "Blazer631"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                          Move, 1
                          Carry, 2
                      ])
                  (at "W17S24" 23 30)
                  (Some Hold) with
                TicksToLive = 320
            }
            // Night739: TODO script; live: harvest→18,18
            { body
                  "Night739"
                  trepidimous
                  (parts [ Move, 3; Work, 5 ])
                  (at "W17S24" 17 19)
                  (Some Hold) with
                TicksToLive = 344
            }
            // Reflection150: TODO script; live: no act
            { body
                  "Reflection150"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                      ])
                  (at "W17S24" 10 26)
                  (Some Hold) with
                TicksToLive = 821
                Energy = 750
            }
            // Antimatter240: TODO script; live: upgradeController→9,8
            { body
                  "Antimatter240"
                  trepidimous
                  (parts [ Move, 1; Work, 1; Carry, 1 ])
                  (at "W17S24" 11 11)
                  (Some Hold) with
                TicksToLive = 830
                Energy = 10
            }
            // Convex273: TODO script; live: no act
            { body
                  "Convex273"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                          Move, 1
                          Carry, 1
                      ])
                  (at "W17S24" 9 30)
                  (Some Hold) with
                TicksToLive = 926
                Energy = 600
            }
            // Exothermic345: TODO script; live: no act
            { body
                  "Exothermic345"
                  trepidimous
                  (parts [ Move, 1; Carry, 16 ])
                  (at "W17S24" 24 36)
                  (Some Hold) with
                TicksToLive = 977
            }
            // Optic627: TODO script; live: no act
            { body
                  "Optic627"
                  trepidimous
                  (parts
                      [
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                          Move, 1
                          Work, 1
                          Carry, 1
                      ])
                  (at "W17S24" 40 35)
                  (Some Hold) with
                TicksToLive = 1_235
                Energy = 62
            }
        ]

    let a =
        arena 994_918 (w18s25CrippledRooms () @ w18s25CrippledOuterRooms ()) colonies bodies

    { a with
        Carried =
            { a.Carried with
                // The Transition log's last task verdict at or before t994,917,
                // read from Memory at live t995,234.
                // TODO ranger-993409-Spawn3: the log holds no task verdict for it that early (or it has died since).
                // TODO worker-993432-Spawn11: the log holds no task verdict for it that early (or it has died since).
                // TODO anchor-993520-Spawn11: the log holds no task verdict for it that early (or it has died since).
                // TODO hauler-993574-Spawn11: the log holds no task verdict for it that early (or it has died since).
                // TODO guard-994071-Spawn11: the log holds no task verdict for it that early (or it has died since).
                // TODO ranger-994875-Spawn11: the log holds no task verdict for it that early (or it has died since).
                // TODO upgrader-993505-Spawn8: the log holds no task verdict for it that early (or it has died since).
                // TODO worker-993452-Spawn9: the log holds no task verdict for it that early (or it has died since).
                // TODO worker-993584-Spawn9: the log holds no task verdict for it that early (or it has died since).
                // TODO guard-993784-Spawn9: the log holds no task verdict for it that early (or it has died since).
                // TODO anchor-993654-Spawn11: the log holds no task verdict for it that early (or it has died since).
                // TODO reserver-994462-Spawn11: the log holds no task verdict for it that early (or it has died since).
                // TODO worker-993508-Spawn2: the log holds no task verdict for it that early (or it has died since).
                // TODO hauler-993532-Spawn6: the log holds no task verdict for it that early (or it has died since).
                // TODO hauler-993624-Spawn10: the log holds no task verdict for it that early (or it has died since).
                // TODO reserver-994408-Spawn8: the log holds no task verdict for it that early (or it has died since).
                // TODO reserver-994574-Spawn8: the log holds no task verdict for it that early (or it has died since).
                // TODO reserver-994328-Spawn3: the log holds no task verdict for it that early (or it has died since).
                // TODO hauler-994772-Spawn8: withdraw:6ac4c16fad3e038630a2ebe1 names a tombstone, which the arena does not place.
                // TODO hauler-993992-Spawn10: withdraw:6ac4c134293c6f62e090bdd2 names a tombstone, which the arena does not place.
                Assignments =
                    Map.ofList
                        [
                            "hauler-994378-Spawn11", "refill:storage-13-27"
                            "ranger-994540-Spawn3", "guard:W17S25"
                            "defender-994619-Spawn11", "defend:W17S25"
                            "hauler-994803-Spawn11", "withdraw:container-27-9"
                            "worker-994186-Spawn8", "upgrade:6a8caa95dd4872bccd319015"
                            "anchor-994580-Spawn8", "harvest:6a8caa95dd4872bccd319013"
                            "anchor-994748-Spawn8", "harvest:6a8caa95dd4872bccd319014"
                            "ranger-994078-Spawn9", "guard:W19S25"
                            "worker-993966-Spawn9", "repair:road-32-36-W17S29"
                            "hauler-994452-Spawn9", "withdraw:container-22-21"
                            "anchor-994587-Spawn9", "harvest:6a8caa7ddd4872bccd318c81"
                            "anchor-994611-Spawn9", "harvest:6a8caa7ddd4872bccd318c83"
                            "upgrader-994821-Spawn9", "withdraw:container-27-42"
                            "ranger-994398-Spawn9", "guard:W19S26"
                            "hauler-993704-Spawn2", "refill:container-22-15"
                            "upgrader-993898-Spawn2", "upgrade:6a8caaaddd4872bccd319363"
                            "anchor-994184-Spawn2", "harvest:6a8caaaddd4872bccd319362"
                            "anchor-994396-Spawn2", "harvest:6a8caaaddd4872bccd319361"
                            "anchor-993806-Spawn1", "harvest:6a8caabadd4872bccd3194aa"
                            "anchor-993828-Spawn6", "harvest:6a8caabadd4872bccd3194a8"
                            "worker-993828-Spawn5", "repair:6abf060f03c73029905f9d4a"
                            "upgrader-994294-Spawn1", "upgrade:6a8caabadd4872bccd3194a9"
                            "hauler-993650-Spawn1", "refill:storage-14-42"
                            "worker-993918-Spawn1", "repair:6abf060f03c73029905f9d4a"
                            "hauler-993812-Spawn10", "refill:spawn-Spawn10"
                            "upgrader-994266-Spawn10", "upgrade:6a8caab9dd4872bccd3194a2"
                            "worker-994608-Spawn10", "repair:rampart-40-37"
                            "anchor-994712-Spawn10", "harvest:6a8caab9dd4872bccd3194a3"
                            "ranger-994340-Spawn9", "guard:W18S27"
                            "anchor-993844-Spawn9", "harvest:6a8caa89dd4872bccd318e4d"
                            "anchor-994292-Spawn9", "harvest:6a8caa89dd4872bccd318e4b"
                            "hauler-994686-Spawn9", "withdraw:container-22-21"
                            "hauler-994145-Spawn9", "withdraw:container-16-42"
                            "anchor-994204-Spawn3", "harvest:6a8caa95dd4872bccd319011"
                            "hauler-994604-Spawn8", "refill:spawn-Spawn3"
                            "hauler-994430-Spawn8", "withdraw:container-17-19"
                            "anchor-994174-Spawn3", "harvest:6a8caaa1dd4872bccd3191f9"
                            "hauler-994396-Spawn3", "withdraw:container-6-9"
                        ]
                // From live Memory at t995,234, 317 ticks after the capture:
                // what it held then, less what is stamped after the tick.
                // Heap only, never in Memory: Healers, ExitWatches and Sightings start empty.
                // Stamped after the tick, not seated unless said: ArmedSeen W17S25 t994,943, ArmedSeen W18S25 t995,032, ArmedSeen W18S26 t995,204, ArmedSeen W19S26 t995,018, ArmedSeen W19S28 t995,200, stand-down W12S28/W16S26 seen t995,192 (LastSeen clamped to the tick, Raiders as read), stand-down W12S28/W18S25 seen t995,032 (LastSeen clamped to the tick, Raiders as read), stand-down W12S28/W18S29 seen t995,198 (LastSeen clamped to the tick, Raiders as read), stand-down W12S28/W19S28 seen t995,200 (LastSeen clamped to the tick, Raiders as read), Threatened W12S28/W11S28 seen t995,234, stand-down W13S28/W16S26 seen t995,192 (LastSeen clamped to the tick, Raiders as read), stand-down W13S28/W18S25 seen t995,032 (LastSeen clamped to the tick, Raiders as read), stand-down W13S28/W18S29 seen t995,198 (LastSeen clamped to the tick, Raiders as read), stand-down W13S28/W19S28 seen t995,200 (LastSeen clamped to the tick, Raiders as read), Threatened W13S28/W11S28 seen t995,234, stand-down W15S28/W16S26 seen t995,192 (LastSeen clamped to the tick, Raiders as read), stand-down W15S28/W18S25 seen t995,032 (LastSeen clamped to the tick, Raiders as read), stand-down W15S28/W18S29 seen t995,198 (LastSeen clamped to the tick, Raiders as read), stand-down W15S28/W19S28 seen t995,200 (LastSeen clamped to the tick, Raiders as read), Threatened W15S28/W11S28 seen t995,234, stand-down W17S29/W16S26 seen t995,192 (LastSeen clamped to the tick, Raiders as read), stand-down W17S29/W18S25 seen t995,032 (LastSeen clamped to the tick, Raiders as read), stand-down W17S29/W18S29 seen t995,198 (LastSeen clamped to the tick, Raiders as read), stand-down W17S29/W19S28 seen t995,200 (LastSeen clamped to the tick, Raiders as read), Threatened W17S29/W11S28 seen t995,234, stand-down W12S26/W16S26 seen t995,192 (LastSeen clamped to the tick, Raiders as read), stand-down W12S26/W18S25 seen t995,032 (LastSeen clamped to the tick, Raiders as read), stand-down W12S26/W18S29 seen t995,198 (LastSeen clamped to the tick, Raiders as read), stand-down W12S26/W19S28 seen t995,200 (LastSeen clamped to the tick, Raiders as read), Threatened W12S26/W11S28 seen t995,234, stand-down W17S25/W16S26 seen t995,192 (LastSeen clamped to the tick, Raiders as read), stand-down W17S25/W18S25 seen t995,032 (LastSeen clamped to the tick, Raiders as read), stand-down W17S25/W18S29 seen t995,198 (LastSeen clamped to the tick, Raiders as read), stand-down W17S25/W19S28 seen t995,200 (LastSeen clamped to the tick, Raiders as read), Threatened W17S25/W11S28 seen t995,234, HomeRaided W17S25 t994,943.
                ArmedSeen = Map.ofList [ "W16S29", 993_826; "W17S24", 994_824; "W19S25", 994_816 ]
                Towered = Set.ofList [ "W12S26"; "W12S28"; "W13S28"; "W15S28"; "W17S25"; "W17S29" ]
                Raids =
                    Map.ofList
                        [
                            "W12S28",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 994_917
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S25"
                                            Opened = 994_881
                                            LastSeen = 994_917
                                            Expiry = 995_081
                                            Basis = StandDownBasis.PlayerRaidHold
                                            Stronghold = false
                                            Bounced = true
                                            HolderUnread = false
                                            Raiders =
                                                Set.ofList
                                                    [
                                                        "6ac4b9d0a78afb3f38184866"
                                                        "6ac4bfc916149cc8bc43ae95"
                                                    ]
                                            PlayerSince = Some 994_881
                                            Chased = true
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S29"
                                            Opened = 986_839
                                            LastSeen = 994_917
                                            Expiry = 997_040
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W19S28"
                                            Opened = 986_841
                                            LastSeen = 994_917
                                            Expiry = 997_160
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                    ]
                                HomeRaided = Some 992_834
                            }
                            "W13S28",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 994_917
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S25"
                                            Opened = 994_881
                                            LastSeen = 994_917
                                            Expiry = 995_081
                                            Basis = StandDownBasis.PlayerRaidHold
                                            Stronghold = false
                                            Bounced = true
                                            HolderUnread = false
                                            Raiders =
                                                Set.ofList
                                                    [
                                                        "6ac4b9d0a78afb3f38184866"
                                                        "6ac4bfc916149cc8bc43ae95"
                                                    ]
                                            PlayerSince = Some 994_881
                                            Chased = true
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S29"
                                            Opened = 986_839
                                            LastSeen = 994_917
                                            Expiry = 997_040
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W19S28"
                                            Opened = 986_841
                                            LastSeen = 994_917
                                            Expiry = 997_160
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                    ]
                                HomeRaided = Some 991_605
                            }
                            "W15S28",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 994_917
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S25"
                                            Opened = 994_881
                                            LastSeen = 994_917
                                            Expiry = 995_081
                                            Basis = StandDownBasis.PlayerRaidHold
                                            Stronghold = false
                                            Bounced = true
                                            HolderUnread = false
                                            Raiders =
                                                Set.ofList
                                                    [
                                                        "6ac4b9d0a78afb3f38184866"
                                                        "6ac4bfc916149cc8bc43ae95"
                                                    ]
                                            PlayerSince = Some 994_881
                                            Chased = true
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S29"
                                            Opened = 986_839
                                            LastSeen = 994_917
                                            Expiry = 997_040
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W19S28"
                                            Opened = 986_841
                                            LastSeen = 994_917
                                            Expiry = 997_160
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                    ]
                            }
                            "W17S29",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 994_917
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S25"
                                            Opened = 994_881
                                            LastSeen = 994_917
                                            Expiry = 995_081
                                            Basis = StandDownBasis.PlayerRaidHold
                                            Stronghold = false
                                            Bounced = true
                                            HolderUnread = false
                                            Raiders =
                                                Set.ofList
                                                    [
                                                        "6ac4b9d0a78afb3f38184866"
                                                        "6ac4bfc916149cc8bc43ae95"
                                                    ]
                                            PlayerSince = Some 994_881
                                            Chased = true
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S29"
                                            Opened = 986_839
                                            LastSeen = 994_917
                                            Expiry = 997_040
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W19S28"
                                            Opened = 986_841
                                            LastSeen = 994_917
                                            Expiry = 997_160
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                    ]
                                HomeRaided = Some 991_430
                            }
                            "W12S26",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 994_917
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S25"
                                            Opened = 994_881
                                            LastSeen = 994_917
                                            Expiry = 995_081
                                            Basis = StandDownBasis.PlayerRaidHold
                                            Stronghold = false
                                            Bounced = true
                                            HolderUnread = false
                                            Raiders =
                                                Set.ofList
                                                    [
                                                        "6ac4b9d0a78afb3f38184866"
                                                        "6ac4bfc916149cc8bc43ae95"
                                                    ]
                                            PlayerSince = Some 994_881
                                            Chased = true
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S29"
                                            Opened = 986_839
                                            LastSeen = 994_917
                                            Expiry = 997_040
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W19S28"
                                            Opened = 986_841
                                            LastSeen = 994_917
                                            Expiry = 997_160
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                    ]
                                HomeRaided = Some 978_444
                            }
                            "W17S25",
                            { Fabot.Core.Observe.RaidState.empty with
                                Outposts =
                                    [
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W16S26"
                                            Opened = 972_732
                                            LastSeen = 994_917
                                            Expiry = 1_045_681
                                            Basis = StandDownBasis.CollapseTimer
                                            Stronghold = true
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S25"
                                            Opened = 994_881
                                            LastSeen = 994_917
                                            Expiry = 995_081
                                            Basis = StandDownBasis.PlayerRaidHold
                                            Stronghold = false
                                            Bounced = true
                                            HolderUnread = false
                                            Raiders =
                                                Set.ofList
                                                    [
                                                        "6ac4b9d0a78afb3f38184866"
                                                        "6ac4bfc916149cc8bc43ae95"
                                                    ]
                                            PlayerSince = Some 994_881
                                            Chased = true
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W18S29"
                                            Opened = 986_839
                                            LastSeen = 994_917
                                            Expiry = 997_040
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                        {
                                            Fabot.Core.Observe.OutpostEpisode.RoomName = "W19S28"
                                            Opened = 986_841
                                            LastSeen = 994_917
                                            Expiry = 997_160
                                            Basis = StandDownBasis.RivalReservation
                                            Stronghold = false
                                            Bounced = false
                                            HolderUnread = false
                                            Raiders = Set.empty
                                            PlayerSince = None
                                            Chased = false
                                        }
                                    ]
                            }
                        ]
            }
    }


/// Each tick's world verdict on W18S25 as W17S25 reads it, the colonies
/// whose weighed row or rangers give a raid seen a `Win` (`Own` where a row
/// was weighed, `Bodies`), and the assignments after the tick.
let private crippledRun (ticks: int) (start: Arena) =
    ((start, []), List.replicate ticks ())
    ||> List.fold (fun (a, acc) () ->
        let verdict =
            Map.tryFind "W18S25" (viewOf a "W17S25").World.Answers
            |> Option.map (fun v ->
                let rowed = if Map.isEmpty v.Value.Rows then Set.empty else v.Value.Own
                let giving = Set.union rowed (v.Value.Bodies |> List.map snd |> Set.ofList)

                v.Value.Answer,
                (if Set.isEmpty giving || v.Value.Answer <> Answer.Win then
                     None
                 else
                     Some(String.concat "," giving)))

        let next, _ = step a
        next, (a.Tick, verdict, next.Carried.Assignments) :: acc)
    |> snd
    |> List.rev

[<Tests>]
let w18s25CrippledTests =
    testList
        "arena capture w18s25-crippled"
        [
            // Live, W17S29's guard row won the room on paper, so the verdict
            // bound its Guard to W17S29: its melee guard was offered the
            // chase and could not take it, its ranger walked round from
            // W19S25, and W17S25's residents next door were never counted
            // (t994,918-995,026). The stand-down had withheld W17S25's own
            // row: now its row is priced, shut or not, and
            // the room is the kite its biggest ranger answers. The kill time is the small
            // rangers' either way in the arena (t93 at writing); what this
            // pins is the binding.
            test
                "#573 capture t994,917: the stand-down withholds no row, so the owner's ranger prices the cripple a kite every seeing colony pools" {
                let run = w18s25Crippled () |> crippledRun 50

                for tick, verdict, _ in run do
                    match verdict with
                    | Some(Answer.Win, Some giving) ->
                        failtest $"t{tick}: a Win given by {giving}'s row or rangers"
                    | _ -> ()

                Expect.exists
                    run
                    (fun (_, _, assignments) ->
                        Map.tryFind "ranger-993409-Spawn3" assignments = Some "guard:W18S25")
                    "once its home raid is down, W17S25's resident ranger next door takes the chase"
            }

            // The world lane (one match over the fleet): W17S25's own ranger
            // at the fight (W17S25 11,24, by the west exit) holds the chase
            // throughout, its seat no far holder takes (#596); the mother's
            // bigger rangers, which would end it sooner on paper, take the
            // seat the kite adds for spells (t35-t52, t56-t69) and never reach
            // it, so it alone kills at t102. W17S26's trespass, a digger, is
            // W17S25's guard's beside it (#601): W17S25 home's Guard is the
            // mother's resident kind, which the child's own Outpost claim of
            // it gives way to, so the guard is not pulled home and W17S29's
            // far ranger does not take W17S26 from W19S25 and walk there for
            // ever; the digger dies at t19.
            test
                "#573 capture t994,917, the world lane: the fleet body at the fight holds the cripple's chase throughout" {
                let run = w18s25Crippled () |> crippledRun 50

                for tick, verdict, _ in run do
                    match verdict with
                    | Some(Answer.Win, Some giving) ->
                        failtest $"t{tick}: a Win given by {giving}'s row or rangers"
                    | _ -> ()

                for tick, _, assignments in run do
                    Expect.equal
                        (Map.tryFind "ranger-994875-Spawn11" assignments)
                        (Some "guard:W18S25")
                        $"t{tick}: the body at the fight holds the chase"

                    Expect.isFalse
                        (Map.exists
                            (fun (name: string) tid ->
                                tid = "guard:W18S25" && name.EndsWith "-Spawn9")
                            assignments)
                        $"t{tick}: no W17S29 body joins it"

                let _, _, first = List.head run

                Expect.equal
                    (Map.tryFind "guard-994071-Spawn11" first)
                    (Some "guard:W17S26")
                    "W17S25's guard beside W17S26 takes its trespass"

                let _, trace = w18s25Crippled () |> Arena.run 30

                Expect.equal
                    (diedOn "Quantum825" trace)
                    (Some 19)
                    $"the digger dies at t19\n{describe trace}"
            }

            test
                "#573 capture t994,917, the world lane: the one ranger on the kite kills the cripple" {
                let _, trace = w18s25Crippled () |> run 120

                // t102.
                Expect.isLessThanOrEqual
                    (diedOn "Faraday387" trace |> Option.defaultValue System.Int32.MaxValue)
                    110
                    $"Faraday387 dies\n{describe trace}"
            }
        ]
