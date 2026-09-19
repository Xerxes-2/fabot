/// The Raid log's second family (ADR 0043): the stand-downs, the ring they
/// trim by a rule of their own, the gate that reads them, the withdrawal with
/// no clock, the reservation somebody else is standing on, and the guard's
/// memory of a room it has gone blind in.
module Fabot.Core.Tests.ObserveOutpostTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Observe

[<Tests>]
let outpostTests =
    testList
        "raid fold: outpost episodes"
        [
            test "a raid two guards cannot beat stands the room down to its own life" {
                // #257. ADR 0043 clocked a stand-down off an invader *core*
                // and off nothing else, so a raid of plain creeps offered no
                // deadline at all: W13S29 stayed open through one, the rows
                // went on hiring into it, and in three hundred ticks it took
                // two reservers and a guard while the invaders kept full
                // health. An Invader in a room nobody owns never suicides, so
                // what it has left is exactly what it will spend and that is
                // the clock.
                //
                // Read only for a raid the guard row's cap cannot beat, which
                // is what keeps the withdrawal from cancelling ADR 0056 before
                // it fights: shutting a room takes it out of the scan set, so a
                // raid that shut it on sight would hide its own hostiles and
                // buy no guard at all. Pairwise on the raid's size alone.
                let raidIn hostiles =
                    { (quiet |> withDeclaredOutpost outpostRoom) with
                        Hostiles = hostiles |> List.mapi (raiderIn outpostRoom)
                    }

                let overwhelming = List.replicate 5 [ Attack; Attack; Attack; Move; Move; Move ]

                Expect.equal
                    (standDowns (RaidState.empty |> raidTick 100 (raidIn overwhelming)))
                    [ outpostRoom, 100, 100, 1600, StandDownBasis.InvaderRaid ]
                    "five attackers beat two blocks, so the room is left for the fifteen hundred they have"

                Expect.isEmpty
                    (standDowns (
                        RaidState.empty
                        |> raidTick
                            100
                            (raidIn
                                [
                                    [
                                        Tough
                                        Tough
                                        Move
                                        Move
                                        Move
                                        Move
                                        Move
                                        RangedAttack
                                        Work
                                        Attack
                                    ]
                                ])
                    ))
                    "and the lone smallMelee two blocks beat opens no stand-down: that room is a fight"
            }

            test "two melee blocks cannot use self-heal to win an equal exchange" {
                let raid attacks =
                    { (quiet |> withDeclaredOutpost outpostRoom) with
                        Hostiles =
                            List.replicate
                                2
                                (List.replicate attacks Attack @ List.replicate (10 - attacks) Move)
                            |> List.mapi (raiderIn outpostRoom)
                    }

                Expect.isEmpty
                    (standDowns (RaidState.empty |> raidTick 100 (raid 2)))
                    "two blocks kill 2,000 hits before 120 damage kills them"

                Expect.equal
                    (standDowns (RaidState.empty |> raidTick 100 (raid 3)))
                    [ outpostRoom, 100, 100, 1600, StandDownBasis.InvaderRaid ]
                    "equal 180 damage and 2,000 hits is not a win; fictitious self-heal must not keep the room open"
            }

            test "a transit-room hostile opens no stand-down, regardless of owner" {
                // #324. W15S26 is projected only because the Errand's chain
                // crosses it. Its hostiles stay in the view so a walker can
                // Flee, but this gate can neither garrison the room nor
                // withhold any work in it. The ordinary Invader is the
                // asymmetric case: filtering only the expected Source Keeper
                // would make this half fail.
                let transitRoom = "W15S26"
                let overwhelming = List.replicate 5 [ Attack; Attack; Attack; Move; Move; Move ]

                let raidBy owner =
                    { quiet with
                        Hostiles =
                            overwhelming
                            |> List.mapi (raiderIn transitRoom)
                            |> List.map (fun hostile -> { hostile with Owner = owner })
                    }

                for owner in [ "Source Keeper"; "Invader"; "Shibdib" ] do
                    Expect.isEmpty
                        (standDowns (RaidState.empty |> raidTick 100 (raidBy owner)))
                        $"{owner} in a room the colony only crosses is no stand-down"
            }

            test "an invader core opens a stand-down that runs to its collapse timer" {
                // The best of ADR 0043's three deadlines, and the only one
                // the engine hands over already absolute — the shell added
                // this tick to `ticksRemaining` on the way in (#133).
                let state = RaidState.empty |> raidTick 100 (seen [ core outpostRoom (Some 900) ])

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 100, 900, StandDownBasis.CollapseTimer ]
                    "the room, the tick it opened on, and the tick read off the threat itself"
            }

            test "a core with no collapse timer runs to the end of the reservation it took" {
                // A level-0 core has no stronghold to collapse and carries
                // no timer, so the only deadline it has is the hold it took
                // with `attackController`. `TicksToEnd` is relative, so the
                // tick is this one plus it: stored as read it would be a
                // deadline four thousand ticks after the epoch.
                let state =
                    RaidState.empty
                    |> raidTick
                        100
                        (seen [ core outpostRoom None ]
                         |> visible outpostRoom (heldBy ReservationHolder.Invader 4000))

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 100, 4100, StandDownBasis.Reservation ]
                    "four thousand ticks left on the hold is a deadline at tick 4,100, not at tick 4,000"
            }

            test "a hold shorter than the fallback is no deadline at all" {
                // ADR 0043's amendment, taken in #136 because this is the
                // ticket where a short clock first became observable: the
                // reservation branch may only ever answer *later* than the
                // fallback.
                //
                // A core outlives the hold it takes — it re-reserves the
                // controller the tick the hold lapses — so the end of a
                // reservation is never the end of the core, and a hold
                // with a handful of ticks left says only what the core did
                // last tick. The engine hands out exactly that: a core
                // that has just taken a controller nobody reserved holds it
                // for three ticks. Read literally that is a three-tick
                // stand-down, which is the "immediately" ADR 0043's own
                // user story says no path may reach.
                //
                // Pairwise, one number at a time, on either side of the
                // 2,500-tick fallback: only the length of the hold moves.
                let heldFor ticks =
                    RaidState.empty
                    |> raidTick
                        100
                        (seen [ core outpostRoom None ]
                         |> visible outpostRoom (heldBy ReservationHolder.Invader ticks))
                    |> standDowns

                Expect.equal
                    (heldFor 3)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "a three-tick hold is read as unreadable, and the clock is the one the colony chose"

                Expect.equal
                    (heldFor 300)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "and so is a three-hundred-tick one: below the fallback the read never shortens the gate"

                Expect.equal
                    (heldFor Tuning.defaults.StandDownFallback)
                    [
                        outpostRoom,
                        100,
                        100,
                        100 + Tuning.defaults.StandDownFallback,
                        StandDownBasis.Reservation
                    ]
                    "at the fallback's own length the hold reads through, and says so"

                // The basis is the operator's half of the amendment: the
                // number the two answers give at the boundary is the same,
                // and "shut until 2,600" and "shut until 2,600 because
                // nothing could be read" are different answers (#117). So
                // the floor is not a `max` over the tick with the reason
                // left standing — a stand-down naming a reservation names
                // the tick that reservation really ends on.
                let basisOf rows =
                    rows |> List.map (fun (_, _, _, _, basis) -> basis)

                Expect.notEqual
                    (basisOf (heldFor 300))
                    (basisOf (heldFor Tuning.defaults.StandDownFallback))
                    "the two sides of the floor are told apart by the reason, not only by the tick"
            }

            test
                "the fallback clock is the colony's tunable, and it is both the floor and the answer" {
                // `Tuning.StandDownFallback` (ADR 0052 decision 5),
                // pairwise over the one field: it is read twice in the same
                // rule — as the deadline a threat gave no readable clock
                // for, and as the floor under a hold too short to believe —
                // so moving it has to move both answers together or the
                // second reading is a literal wearing the first one's name.
                let shut fallback ticks =
                    let colony =
                        seen [ core outpostRoom None ]
                        |> visible outpostRoom (heldBy ReservationHolder.Invader ticks)

                    RaidState.empty
                    |> raidTick
                        100
                        { colony with
                            Tuning =
                                { colony.Tuning with
                                    StandDownFallback = fallback
                                }
                        }
                    |> standDowns

                Expect.equal
                    (shut 2500 300)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "at the shipped 2,500 a three-hundred-tick hold is unreadable and the colony's own clock answers"

                Expect.equal
                    (shut 200 300)
                    [ outpostRoom, 100, 100, 400, StandDownBasis.Reservation ]
                    "at a fallback of 200 the same hold clears the floor and reads through as a reservation"

                Expect.equal
                    (shut 200 100)
                    [ outpostRoom, 100, 100, 300, StandDownBasis.Fallback ]
                    "and the fallback is still the answer under its own floor: 200 ticks from now, said as the colony's choice"
            }

            test "with neither deadline readable the clock is the expansion period" {
                // Nothing is unreadable here by accident: a core with no
                // timer in a room nothing holds is the shape the fallback
                // exists for, and no path may answer "indefinitely" or
                // "now".
                let state = RaidState.empty |> raidTick 100 (seen [ core outpostRoom None ])

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "2,500 ticks on from the sighting, and the record says it was chosen and not read"
            }

            test "only the invader's own hold is a clock" {
                // Pairwise, one holder at a time: a rule reading "not ours"
                // would take a rival's reservation for the core's and shut
                // the room until a tick that says nothing about the core,
                // and ADR 0043 answers those two differently. Both fall back
                // rather than reading a deadline off a hold that is not the
                // threat's.
                //
                // Since #165 a rival's hold is a deadline of its own — 300
                // ticks here — and this is where the two families meet: the
                // ring keeps the **later** of the reads a tick produces for
                // one room, so the rival's short clock can never cut a core's
                // stand-down short.
                let held holder =
                    RaidState.empty
                    |> raidTick
                        100
                        (seen [ core outpostRoom None ] |> visible outpostRoom (heldBy holder 300))
                    |> standDowns

                Expect.equal
                    (held ReservationHolder.Rival)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "a player's 300-tick hold is no deadline for the core, and never shortens its clock"

                Expect.equal
                    (held ReservationHolder.Ours)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "and the colony's own hold says nothing about the core standing in it"
            }

            test "a core still standing there extends the stand-down and re-reads its clock" {
                let state =
                    RaidState.empty
                    |> raidTick 100 (seen [ core outpostRoom None ])
                    |> raidTick 110 (seen [ core outpostRoom None ])

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 110, 2610, StandDownBasis.Fallback ]
                    "one episode, its window carried to the last sighting and its clock read at it"
            }

            test "a re-read never shortens a stand-down that is already running" {
                // The gate may be wrong in one direction only (ADR 0043's
                // Consequences): a stale stand-down costs an outpost's
                // income until its clock runs out, and the failure it
                // prevents costs a creep a cycle. A later sighting can
                // land on a worse deadline than the one already recorded
                // — the core drains our hold and takes its own, freshly
                // at a handful of ticks, or our reserver takes it back and
                // the read falls through to the fallback — and reading
                // that in would cut the stand-down short, which is the
                // other direction. The same rule `deadlines` applies to
                // two cores in one tick, applied across ticks.
                let shortened =
                    RaidState.empty
                    |> raidTick
                        100
                        (seen [ core outpostRoom None ]
                         |> visible outpostRoom (heldBy ReservationHolder.Invader 4000))
                    |> raidTick 110 (seen [ core outpostRoom None ])

                Expect.equal
                    (standDowns shortened)
                    [ outpostRoom, 100, 110, 4100, StandDownBasis.Reservation ]
                    "the window still extends to the sighting, and the clock and the reason it was read off both stand"

                let lengthened =
                    RaidState.empty
                    |> raidTick 100 (seen [ core outpostRoom None ])
                    |> raidTick
                        110
                        (seen [ core outpostRoom None ]
                         |> visible outpostRoom (heldBy ReservationHolder.Invader 4000))

                Expect.equal
                    (standDowns lengthened)
                    [ outpostRoom, 100, 110, 4110, StandDownBasis.Reservation ]
                    "and a longer deadline is taken, with the basis of the tick that won"
            }

            test "a tick without vision moves no clock and closes no stand-down" {
                // The dangerous case (#117): losing vision reads exactly
                // like peace, and the quiet gap here is five ticks, so the
                // spawn family would have closed this episode six times
                // over. This family is exempt — the colony stops looking
                // the moment it withdraws, so silence is never evidence.
                let standing = RaidState.empty |> raidTick 100 (seen [ core outpostRoom None ])

                let blind =
                    (standing, [ 101..130 ])
                    ||> List.fold (fun state t -> state |> raidTick t quiet)

                Expect.equal
                    (standDowns blind)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "thirty blind ticks leave the record exactly as the last tick with vision left it"
            }

            test "a room seen clear stands down all the same, until its clock runs out" {
                // Re-entry is a clock running out and never a second look
                // (ADR 0043). A tick with vision and no core is not
                // evidence the core is gone — it is what a creep passing
                // the wrong tile sees — and even a true one does not open
                // the gate early.
                let state =
                    RaidState.empty
                    |> raidTick 100 (seen [ core outpostRoom None ])
                    |> raidTick 101 (quiet |> visible outpostRoom None)

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 100, 100, 2600, StandDownBasis.Fallback ]
                    "the room looking clear neither closed the episode nor moved its expiry"
            }

            test "the clock runs out, and the next core seen opens a second stand-down" {
                let state =
                    RaidState.empty
                    |> raidTick 100 (seen [ core outpostRoom (Some 105) ])
                    |> raidTick 105 (seen [ core outpostRoom (Some 130) ])

                Expect.equal
                    (standDowns state)
                    [
                        (outpostRoom, 100, 100, 105, StandDownBasis.CollapseTimer)
                        (outpostRoom, 105, 105, 130, StandDownBasis.CollapseTimer)
                    ]
                    "the expiry is the first tick the room may be re-entered, so a sighting on it is a new stand-down and the spent one stays in the ring"
            }

            test "each outpost's clock is its own" {
                let other = "W13S28"

                let state =
                    RaidState.empty
                    |> raidTick 100 (seen [ core outpostRoom (Some 500); core other (Some 700) ])
                    |> raidTick 110 (seen [ core other (Some 700) ])

                Expect.equal
                    (standDowns state)
                    [
                        (outpostRoom, 100, 100, 500, StandDownBasis.CollapseTimer)
                        (other, 100, 110, 700, StandDownBasis.CollapseTimer)
                    ]
                    "the tick that saw one room and not the other moved that room's episode alone"
            }

            test "a ring full of raids evicts no stand-down that is running" {
                // One ring shared between the families would drop the
                // episode driving the gate and reopen the room in the
                // middle of a stand-down (#117). The cap is three here and
                // four raids overflow it.
                let state =
                    (RaidState.empty |> raidTick 10 (seen [ core outpostRoom (Some 500) ]),
                     [ 20; 30; 40; 50 ])
                    ||> List.fold (fun state t -> state |> raidTick t (raid squad))

                Expect.equal
                    (standDowns state)
                    [ outpostRoom, 10, 10, 500, StandDownBasis.CollapseTimer ]
                    "the stand-down is still there with its clock untouched"

                Expect.equal
                    (windows state)
                    [ (30, 30); (40, 40); (50, 50) ]
                    "while the raid ring trims to the cap exactly as it did before"
            }

            test "a stand-down still running survives a ring overflowing past it" {
                // Six spent stand-downs in one room around one long-running
                // one somewhere else, against a cap of three. The overflow
                // is paid out of the finished rows and never out of the one
                // holding a room shut.
                let other = "W13S28"

                let state =
                    RaidState.empty
                    |> raidTick 10 (seen [ core outpostRoom (Some 11) ])
                    |> raidTick 12 (seen [ core outpostRoom (Some 13) ])
                    |> raidTick 14 (seen [ core outpostRoom (Some 15) ])
                    |> raidTick 16 (seen [ core other (Some 5000) ])
                    |> raidTick 18 (seen [ core outpostRoom (Some 19) ])
                    |> raidTick 20 (seen [ core outpostRoom (Some 21) ])
                    |> raidTick 22 (seen [ core outpostRoom (Some 23) ])

                Expect.equal
                    (standDowns state)
                    [
                        (other, 16, 16, 5000, StandDownBasis.CollapseTimer)
                        (outpostRoom, 20, 20, 21, StandDownBasis.CollapseTimer)
                        (outpostRoom, 22, 22, 23, StandDownBasis.CollapseTimer)
                    ]
                    "the oldest row is the one still standing down, and it is the one row the trim would not take"
            }

            test "a core in an outpost leaves the spawn room's raid exactly as it was" {
                // The regression #117 asks for. The two families share a
                // Memory leaf and nothing else, so every step the raid fold
                // takes — the window, the roster, the closest approach, the
                // losses and the damage, plus both baselines it carries
                // between ticks — must read the same with a core standing
                // next door as without one. Compared as whole states rather
                // than through the projections, so a field no list here
                // reads is covered too.
                let sequence (colony: ColonyView) =
                    (RaidState.empty, [ 10..14 ])
                    ||> List.fold (fun state t ->
                        { colony with
                            Creeps = if t < 12 then [ ours "w1"; ours "w2" ] else [ ours "w1" ]
                        }
                        |> withHits "ram-1" BuiltKind.Rampart (100_000 - 200 * t)
                        |> fun tick -> state |> raidTick t tick)

                let alone = sequence { placed with Hostiles = squad }

                let beside =
                    sequence
                        { placed with
                            Hostiles = squad
                            InvaderCores = [ core outpostRoom None ]
                        }

                Expect.equal
                    { beside with Outposts = [] }
                    alone
                    "the raid reads byte for byte the same, roster, approach, losses, damage and baselines alike"

                Expect.equal
                    (standDowns beside)
                    [ outpostRoom, 10, 14, 2514, StandDownBasis.Fallback ]
                    "while the core standing next door recorded a stand-down of its own"
            }
        ]

[<Tests>]
let clocklessTests =
    testList
        "raid fold: the withdrawal with no clock"
        [
            test "a room another player holds is remembered, and no episode opens for it" {
                // ADR 0043's other trigger, and the community's one
                // unanimous abandonment rule. It is not a threat, so there
                // is no threat to read a deadline off and nothing for a
                // basis to explain: the record is the room's name and
                // that is the whole of it.
                //
                // **Ownership alone since #165.** Owned and reserved are one
                // fact to the economics (ADR 0042) and two facts to a gate
                // that has to say when the room comes back: the engine ends a
                // reservation and ends nothing about an owner. So the
                // reservation moved to the ring, and the pairwise contrast is
                // one room, one look, one field of the control entry apart.
                let owned = RaidState.empty |> raidTick 100 (quiet |> ownedByRival outpostRoom)

                let reserved =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Rival 4000))

                Expect.equal
                    owned.RivalHeld
                    (Map.ofList [ outpostRoom, { Since = 100; LastLooked = 100 } ])
                    "an owner that is not us, against the tick the look was taken on"

                Expect.isEmpty
                    reserved.RivalHeld
                    "and a reservation that is not ours latches nothing: it runs out on its own"

                Expect.isEmpty owned.Outposts "no episode opened: there is no clock to run"

                Expect.equal
                    (standDowns reserved)
                    [ outpostRoom, 100, 100, 4100, StandDownBasis.RivalReservation ]
                    "while the reservation is an episode of the ring, clocked to the hold's own end"
            }

            test "the NPC's hold is a clock and never an exit" {
                // Pairwise, one holder at a time — the whole reason
                // `ReservationHolder` is three states and not a "not ours"
                // flag (#133). Since #165 no holder latches: a reservation of
                // anybody's ends on a tick the engine counts down, and the
                // three answers are three *clocks* — the NPC's read off the
                // core standing there, the rival's off the hold itself, and
                // ours no clock at all because the room is being worked by us.
                let folded holder =
                    RaidState.empty
                    |> raidTick 100 (quiet |> visible outpostRoom (heldBy holder 4000))

                let held holder = (folded holder).RivalHeld

                Expect.isEmpty
                    (held ReservationHolder.Invader)
                    "the Invader's hold withdraws nothing on its own"

                Expect.isEmpty (held ReservationHolder.Ours) "and neither does our own"

                Expect.isEmpty
                    (held ReservationHolder.Rival)
                    "nor does the third: since #165 the latch is ownership's alone"

                // The NPC's hold is read off the *core* and never off the
                // controller (`deadlineOf`), so a hold with no core standing
                // in the room opens nothing — where a rival's hold is read off
                // the controller itself and opens an episode on the spot.
                Expect.isEmpty
                    (standDowns (folded ReservationHolder.Invader))
                    "no core is standing there, so the NPC's hold is nobody's deadline this tick"

                Expect.equal
                    (standDowns (folded ReservationHolder.Rival))
                    [ outpostRoom, 100, 100, 4100, StandDownBasis.RivalReservation ]
                    "while the rival's hold is a stand-down running to the end of that hold"

                Expect.isEmpty
                    (standDowns (folded ReservationHolder.Ours))
                    "and our own hold is the steady state of every outpost, not a withdrawal"
            }

            test "the conclusion is held through every tick nobody is looking" {
                // The load-bearing half, and the reason this is persisted
                // at all rather than read off each tick's ColonyView: the
                // gate's own effect is to withdraw the creeps that paid for
                // the vision that judged it. A rule re-read from nothing
                // would reopen the room the tick after it shut it, and the
                // colony would walk back into somebody else's room for
                // ever — `standingDown`'s oscillation, arriving through
                // the other trigger.
                let taken = RaidState.empty |> raidTick 100 (quiet |> ownedByRival outpostRoom)

                let blind =
                    (taken, [ 101..130 ]) ||> List.fold (fun state t -> state |> raidTick t quiet)

                Expect.equal
                    blind.RivalHeld
                    (Map.ofList [ outpostRoom, { Since = 100; LastLooked = 100 } ])
                    "thirty blind ticks leave the last look's conclusion, and its tick, exactly where they stood"
            }

            test "the tick recorded is the look that shut the gate, not the last look" {
                // The trace half of the record (#117's US-20): the number
                // beside the room is the tick the withdrawal began, so an
                // operator can line an income drop up against it months
                // later. A second look that finds the room still taken is
                // not a second withdrawal and must not restamp it — and
                // nothing measures against the tick, so keeping the first
                // costs nothing and moving it would cost the only date
                // there is.
                let twice =
                    RaidState.empty
                    |> raidTick 100 (quiet |> ownedByRival outpostRoom)
                    |> raidTick 140 (quiet |> ownedByRival outpostRoom)

                Expect.equal
                    twice.RivalHeld
                    (Map.ofList [ outpostRoom, { Since = 100; LastLooked = 100 } ])
                    "the tick the gate shut on, forty ticks after a second look agreed with it — and no look fell due in between (#275)"
            }

            test "a room taken again after it was freed is dated by the second withdrawal" {
                // The other side of the rule above: the tick is the
                // *current* withdrawal's, not the room's first ever, so a
                // room that came back and was taken again dates from the
                // taking that is holding it now.
                let again =
                    RaidState.empty
                    |> raidTick 100 (quiet |> ownedByRival outpostRoom)
                    |> raidTick 120 (quiet |> visible outpostRoom None)
                    |> raidTick 300 (quiet |> ownedByRival outpostRoom)

                Expect.equal
                    again.RivalHeld
                    (Map.ofList [ outpostRoom, { Since = 300; LastLooked = 300 } ])
                    "the look that shut it this time, and not the one whose gate has been cleared"
            }

            test "only a tick with vision takes a room back out" {
                // "Until it is seen again" is the rule ADR 0043 gives, and
                // it is written on vision in both directions: a look that
                // finds the room free is as good evidence as the look that
                // found it taken.
                //
                // In the live colony that second look used to be something
                // the bot could not arrange — a room this holds shut is not
                // scanned, so nothing went there to see it — which made the
                // withdrawal permanent until a human hand-edited the leaf.
                // #165 arranges it: the gate re-admits a latched room to the
                // **scan** once every `Tuning.RivalRecheck` ticks, and this
                // fold is what such a look lands in. The rule here is
                // unchanged and is why that was enough — a look that finds
                // the room free has always taken it back out.
                let freed =
                    RaidState.empty
                    |> raidTick 100 (quiet |> ownedByRival outpostRoom)
                    |> raidTick 101 (quiet |> visible outpostRoom None)

                Expect.isEmpty freed.RivalHeld "the room the colony can see is nobody else's again"
            }

            test "the look that falls due moves the stride, and never the date the gate shut on" {
                // #275. The stride is measured between *looks*, so the tick a
                // look was taken on is the one the record has to carry — and
                // it is carried beside the shutting tick rather than over it,
                // because the two answer different questions: one dates an
                // income drop for an operator (#117's US-20), the other says
                // when the colony next questions its own conclusion.
                //
                // The look is stamped whether or not vision answered. The gate
                // re-admits the room to the scan for that tick and the colony
                // may well be blind in it — which is the common case, the
                // withdrawal itself having taken the vision away — and a look
                // stamped only when it saw something would leave a blind room
                // re-admitted on every tick from the stride onwards.
                let recheck = Tuning.defaults.RivalRecheck
                let state = RaidState.empty |> raidTick 100 (quiet |> ownedByRival outpostRoom)

                Expect.equal
                    state.RivalHeld
                    (Map.ofList [ outpostRoom, { Since = 100; LastLooked = 100 } ])
                    "the look that shut the gate is the last look taken, so far"

                // Forty ticks late, which is the whole of what this ticket
                // fixes: a tick the gate was not evaluated on delays the look
                // rather than cancelling it.
                let looked = state |> raidTick (100 + recheck + 40) quiet

                Expect.equal
                    looked.RivalHeld
                    (Map.ofList
                        [
                            outpostRoom,
                            {
                                Since = 100
                                LastLooked = 100 + recheck + 40
                            }
                        ])
                    "the blind look is taken all the same, and the date of the withdrawal stands"

                Expect.isEmpty
                    (recheckedAt (100 + recheck + 41) looked)
                    "so the tick after the look, nothing is looked into"

                Expect.equal
                    (recheckedAt (100 + 2 * recheck + 40) looked)
                    (Set.singleton outpostRoom)
                    "and the next look falls a stride after the look, not a stride after the shutting"
            }

            test "a look with vision that agrees moves the stride and clears nothing" {
                // The look that finds the rival still there: the latch stands,
                // its date stands, and the stride runs again from this look.
                // The one thing that separates it from the blind look above is
                // that it could have cleared the latch and did not.
                let recheck = Tuning.defaults.RivalRecheck

                let agreed =
                    RaidState.empty
                    |> raidTick 100 (quiet |> ownedByRival outpostRoom)
                    |> raidTick (100 + recheck) (quiet |> ownedByRival outpostRoom)

                Expect.equal
                    agreed.RivalHeld
                    (Map.ofList
                        [
                            outpostRoom,
                            {
                                Since = 100
                                LastLooked = 100 + recheck
                            }
                        ])
                    "still theirs, shut since the same tick, and looked into on the stride"
            }

            test "a colony nobody has taken anything from remembers nothing" {
                // The home room is in `RoomControl` on every tick with
                // vision and is the colony's own, so a rule reading
                // "somebody holds this" the wrong way round would withdraw
                // the colony from itself.
                let home =
                    RaidState.empty
                    |> raidTick
                        100
                        { quiet with
                            RoomControl =
                                Map.ofList
                                    [
                                        raidRoom,
                                        {
                                            Owner = Ownership.Ours
                                            Reservation = None
                                            SafeMode = false
                                        }
                                    ]
                        }

                Expect.isEmpty home.RivalHeld "the room we own is not a room somebody took"
            }
        ]

[<Tests>]
let holdTests =
    testList
        "raid fold: the reservation somebody else is standing on"
        [
            test "a hold that is not ours is recorded against the tick it runs out on" {
                // #333's record, and the three answers `ReservationHolder`
                // gives read one at a time. What goes in the leaf is the
                // fact the reserver row now refuses to hire against — the
                // engine answers ERR_INVALID_TARGET on a controller anybody
                // but us holds — so the channel can say *why* a declared
                // outpost is being mined and not reserved.
                //
                // The tick is absolute, the engine's countdown being
                // relative: 100 + 4,000. Stored as read it would date a hold
                // to the start of the world.
                let recorded holder =
                    (RaidState.empty
                     |> raidTick 100 (quiet |> visible outpostRoom (heldBy holder 4000)))
                        .Holds

                Expect.equal
                    (recorded ReservationHolder.Invader)
                    (Map.ofList
                        [
                            outpostRoom,
                            {
                                Holder = ReservationHolder.Invader
                                Until = 4100
                            }
                        ])
                    "the Invader's hold — W12S27's own case, the core long since collapsed"

                Expect.equal
                    (recorded ReservationHolder.Rival)
                    (Map.ofList
                        [
                            outpostRoom,
                            {
                                Holder = ReservationHolder.Rival
                                Until = 4100
                            }
                        ])
                    "and another player's, which the engine refuses us in exactly the same words"

                Expect.isEmpty
                    (recorded ReservationHolder.Ours)
                    "our own hold is the steady state of every outpost and is recorded nowhere"

                Expect.isEmpty
                    (RaidState.empty |> raidTick 100 (quiet |> visible outpostRoom None)).Holds
                    "and an unreserved controller is the room the row hires for: no entry either"
            }

            test "the hold outlives the vision that read it, and ends itself" {
                // The two rules the record is kept on, which are the latch's
                // first and the ring's second. A tick without vision leaves
                // the conclusion standing — the colony is blind in most of
                // these rooms most of the time, and re-reading the entry off
                // nothing would clear it the tick after it was written. And
                // the entry ends on the tick it named, no look being needed
                // to know a countdown has run out (ADR 0043's re-entry rule).
                let taken =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Invader 4000))

                let blind =
                    (taken, [ 101..130 ]) ||> List.fold (fun state t -> state |> raidTick t quiet)

                Expect.equal
                    blind.Holds
                    taken.Holds
                    "thirty blind ticks leave the last look's conclusion exactly where it stood"

                let atEnd =
                    (taken, [ 4099; 4100 ]) ||> List.fold (fun state t -> state |> raidTick t quiet)

                Expect.isEmpty
                    atEnd.Holds
                    "and on the tick the engine's countdown reaches, the record retires itself"

                let short = taken |> raidTick 4099 quiet

                Expect.equal
                    short.Holds
                    taken.Holds
                    "one tick short of it the hold is still standing, blind or not"
            }

            test "a look that finds the controller free takes the room back out" {
                // The other direction, on the same evidence rule: a tick
                // with vision decides the room either way, so the entry that
                // matters is the last look's and never the first. Pinned
                // *before* the clock runs out, or the expiry above would be
                // what cleared it and this would pass on the wrong reason.
                let freed =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Invader 4000))
                    |> raidTick 101 (quiet |> visible outpostRoom None)

                Expect.isEmpty
                    freed.Holds
                    "the controller the colony can see is nobody else's again"

                // And a hold re-read is re-clocked: the engine counts down at
                // one a tick, so the same hold read 40 ticks later names the
                // same absolute tick, and a *fresh* hold names a later one.
                let reread =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Invader 4000))
                    |> raidTick
                        140
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Invader 3960))

                Expect.equal
                    reread.Holds
                    (Map.ofList
                        [
                            outpostRoom,
                            {
                                Holder = ReservationHolder.Invader
                                Until = 4100
                            }
                        ])
                    "the same hold, forty ticks of it spent, still ends on the tick it always did"
            }

            test "the Invader's hold withdraws nothing, and a rival's withdraws the room as well" {
                // The line this record is drawn on, and the whole reason it
                // is a third shape rather than a row of the ring — but the
                // line is the **Invader's** alone, and saying it of "somebody
                // else" would be false of the other half of the very
                // predicate the record is folded on (`heldByOther`).
                //
                // The Invader's leftover hold is the new case: no core stands
                // over it, ADR 0043's ring is clocked off cores, and #165's
                // rival clause does not answer for the NPC. So the room is
                // mined and only the *reservation* is refused. Written as a
                // stand-down it would have withdrawn the room, which is the
                // architectural question #333 leaves for a human.
                //
                // A rival's identical hold is a clocked stand-down already
                // (#165) — the same control entry opens an episode and the
                // gate shuts the room — so for that holder the record says
                // *why* a room that is withheld anyway is also unreservable,
                // and nothing about a room the colony goes on mining. Pinned
                // pairwise, one holder apart, because the sentence in the
                // docs is written one way and is true only one way.
                let heldByWhom holder =
                    RaidState.empty
                    |> raidTick 100 (quiet |> visible outpostRoom (heldBy holder 4000))

                let invader = heldByWhom ReservationHolder.Invader
                let rival = heldByWhom ReservationHolder.Rival

                Expect.isEmpty invader.Outposts "the Invader's leftover hold opens no episode"

                Expect.isEmpty invader.RivalHeld "and latches nothing: the engine ends this hold"

                Expect.isEmpty (shutAt 101 invader) "so the gate withholds that room from nothing"

                Expect.equal
                    (heldAt 101 invader)
                    (Set.singleton outpostRoom)
                    "what it does narrow is the reservation, on every blind tick of the hold"

                Expect.isNonEmpty
                    rival.Outposts
                    "a rival's identical hold is #165's episode: the same entry opens a stand-down"

                Expect.equal
                    (shutAt 101 rival)
                    (Set.singleton outpostRoom)
                    "so that room is withheld from the work, and is not one the colony goes on mining"

                Expect.equal
                    (heldAt 101 rival)
                    (Set.singleton outpostRoom)
                    "and it is in this set too: the two families name one room and say different things"
            }

            test "the gate stops holding a reservation on the tick the engine's countdown reaches" {
                // What the gate does with the record, which is the half the
                // rule the ticket is about actually reads
                // (`Planner.reservableControllers` through
                // `ColonyView.HeldOutposts`). The set is the standing holds
                // and nothing else: `tick < Until`, the same test the fold
                // retires an entry on and `observe.mjs` prints one under, so
                // a leaf the fold has not caught up with — no tick with
                // vision since the hold ended — cannot withhold a
                // reservation the engine would now accept.
                let taken =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Invader 4000))

                Expect.equal
                    (heldAt 4099 taken)
                    (Set.singleton outpostRoom)
                    "one tick short of the end the hold stands, blind ever since it was read"

                Expect.isEmpty
                    (heldAt 4100 taken)
                    "and on the tick the countdown reaches, the room is ours to reserve again"

                Expect.isEmpty
                    (heldAt 101 RaidState.empty)
                    "a colony with no log at all holds nothing out of its own pool"
            }
        ]

[<Tests>]
let threatMemoryTests =
    testList
        "raid fold: the guard's memory of a raided outpost"
        [
            test "an armed threat seen in a declared outpost is remembered through the blind ticks" {
                // #366. The guard row hires on a threat seen in an outpost,
                // and the bodies that vision comes from — the anchor, the
                // hauler, the reserver — are exactly what the raid kills, so
                // the room goes dark and the row that bought a 15-ATTACK-part
                // body stops asking for it. This is #333's answer in the guard
                // row: the conclusion is written down on the tick with vision
                // and read on the ticks without one.
                let looked =
                    RaidState.empty
                    |> raidTick 100 (quiet |> lookingAt outpostRoom [ armedIn outpostRoom ])

                Expect.equal
                    (looked.Threatened |> Map.tryFind outpostRoom)
                    (Some
                        {
                            Until = 100 + Tuning.defaults.ThreatMemory
                        })
                    "the look writes the room down against its own clock: this tick plus ThreatMemory"

                // The blind tick is the whole point: nothing of ours stands in
                // the room any more, so there is no control entry and no
                // hostile on the view, and the record has to survive that.
                let blind = looked |> raidTick 140 quiet

                Expect.equal
                    (blind.Threatened |> Map.tryFind outpostRoom)
                    (Some { Until = 100 + Engine.creepLifetime })
                    "a tick with no vision in the room leaves the conclusion exactly as it found it"

                // The clock is a backstop and not a schedule (#369). It was 300
                // — the guard's cast plus its walk — and that is the right size
                // for *sending* a guard and the wrong size for *remembering*:
                // while the room is dark an expiry cannot mean the raid ended,
                // only that we stopped remembering, and the row then reads a
                // room full of invader as clear. W15S29 killed four of W15S28's
                // bodies that way in one day, one unarmed body at a time.
                Expect.equal
                    (threatenedAt 999 blind)
                    (Set.singleton outpostRoom)
                    "700 ticks past the old memory's end, with nobody looking, the room is still remembered"

                Expect.equal
                    (threatenedAt (99 + Engine.creepLifetime) blind)
                    (Set.singleton outpostRoom)
                    "one tick short of the backstop it still answers"

                Expect.isEmpty
                    (threatenedAt (100 + Engine.creepLifetime) blind)
                    "and on the tick no raider seen then could still be alive it is forgotten, with no look taken at all"

                Expect.isEmpty
                    (threatenedAt 101 RaidState.empty)
                    "a colony with no log remembers no raid: absence classifies nothing (ADR 0004)"
            }

            test "a look that finds the outpost clear forgets the raid on the tick it takes" {
                // The other direction of the same rule, and the one that keeps
                // this a memory rather than a second [[stand-down]]: a tick
                // with vision decides the room either way. The guard's own
                // arrival is what usually takes this look, which is why the
                // memory may be generous — the cost of it being too long is one
                // body's walk into a room that turns out to be clear.
                let remembered =
                    RaidState.empty
                    |> raidTick 100 (quiet |> lookingAt outpostRoom [ armedIn outpostRoom ])

                let cleared = remembered |> raidTick 160 (quiet |> lookingAt outpostRoom [])

                Expect.isEmpty
                    cleared.Threatened
                    "the room is seen clear 240 ticks before the memory would have run out, and the entry goes with the look"

                Expect.isEmpty (threatenedAt 161 cleared) "so the gate answers for nothing"

                // Vision and no Threat is a clearing; vision and a Threat is a
                // fresh write, which is what keeps a raid that outlives the
                // memory from being forgotten while it is being watched.
                let stillThere =
                    remembered
                    |> raidTick 160 (quiet |> lookingAt outpostRoom [ armedIn outpostRoom ])

                Expect.equal
                    (stillThere.Threatened |> Map.tryFind outpostRoom)
                    (Some { Until = 160 + Engine.creepLifetime })
                    "and a look that finds it still standing there moves the clock to this tick's"
            }

            test
                "a healer alone is remembered nowhere, and neither is a room the colony merely crosses" {
                // Two narrowings in one case, both of them the rule's own
                // spelling rather than this fixture's. ADR 0033's Threat test:
                // a hostile with no ATTACK or RANGED_ATTACK part reaches
                // nothing and is no reason to buy a body, so it writes no
                // memory a guard row could act on. And the room: the guard row
                // is per **declared outpost**, so a raid at home is the
                // [[keep]]'s (ADR 0034) and one in a room the colony does not
                // declare hires nobody (#324).
                let healerSeen =
                    RaidState.empty
                    |> raidTick 100 (quiet |> lookingAt outpostRoom [ healerIn outpostRoom ])

                Expect.isEmpty
                    healerSeen.Threatened
                    "a healer standing in the outpost is a hostile with no reach and buys no guard"

                let atHome =
                    RaidState.empty
                    |> raidTick
                        100
                        { (quiet |> visible raidRoom None) with
                            Hostiles = [ armedIn raidRoom ]
                        }

                Expect.isEmpty
                    atHome.Threatened
                    "and an armed raid in the colony's own room is no outpost's memory"

                let undeclared =
                    RaidState.empty
                    |> raidTick
                        100
                        { (quiet |> visible outpostRoom None) with
                            Hostiles = [ armedIn outpostRoom ]
                        }

                Expect.isEmpty
                    undeclared.Threatened
                    "nor is a room with no controller of ours projected in it: no declaration, no guard row"
            }
        ]

[<Tests>]
let gateTests =
    testList
        "the stand-down gate"
        [
            test "a running clock shuts its room, and the tick it runs out opens it" {
                // The gate reads the log the way `observe.mjs outposts`
                // reads it: shut while the tick is short of the expiry,
                // and the expiry is the first tick the room may be
                // re-entered.
                let state = RaidState.empty |> raidTick 100 (seen [ core outpostRoom (Some 900) ])

                Expect.equal
                    (shutAt 899 state)
                    (Set.singleton outpostRoom)
                    "the tick before the clock runs out the room is still withheld"

                Expect.isEmpty
                    (shutAt 900 state)
                    "on the expiry itself the room is back in the set the shell scans"

                Expect.isEmpty (shutAt 5000 state) "and stays there"
            }

            test "each outpost's gate is its own" {
                // ADR 0043's independent gates: W12S27 standing down says
                // nothing about W13S28. Two rooms, one core each, two
                // clocks that run out at different ticks.
                let other = "W13S28"

                let state =
                    RaidState.empty
                    |> raidTick 100 (seen [ core outpostRoom (Some 200); core other (Some 900) ])

                Expect.equal
                    (shutAt 150 state)
                    (Set.ofList [ outpostRoom; other ])
                    "both clocks running, both rooms withheld"

                Expect.equal
                    (shutAt 500 state)
                    (Set.singleton other)
                    "the room whose clock ran out is back on its own, and the other is still shut"
            }

            test "a room in another player's hands is shut by no clock at all" {
                // The two triggers meet in one set, and only here: the
                // clocked family carries an expiry the gate compares
                // against, the clockless one carries nothing to compare.
                let state = RaidState.empty |> raidTick 100 (quiet |> ownedByRival outpostRoom)

                Expect.equal
                    (shutAt 101 state)
                    (Set.singleton outpostRoom)
                    "shut the tick after it was seen taken"

                Expect.equal
                    (shutAt 1_000_000 state)
                    (Set.singleton outpostRoom)
                    "and shut a million ticks later: there is no clock for this one to run out"
            }

            test "a room another player reserved comes back when the reservation runs out" {
                // #165's first half, at the gate: a passing claimer is a
                // routine event where an owner is not, and the engine is
                // already counting its hold down. The room stands down for
                // exactly that hold and re-enters with **no vision needed** —
                // which is the whole of ADR 0043's "re-entry is a clock
                // running out, not a look", now reached through the trigger
                // that used to latch.
                //
                // Pairwise against the owner above, one field of one control
                // entry apart: 200 ticks of a rival's reservation seen at
                // t100, against the same room owned outright.
                let reserved =
                    RaidState.empty
                    |> raidTick
                        100
                        (quiet |> visible outpostRoom (heldBy ReservationHolder.Rival 200))

                Expect.equal
                    (shutAt 299 reserved)
                    (Set.singleton outpostRoom)
                    "the tick before the hold ends the room is still withheld"

                // t300 and not t301: `TicksToEnd` is the engine's own
                // countdown to the tick the reservation is *gone*, and
                // `Expiry` is the first tick the room may be re-entered —
                // the same reading `deadlineOf` gives the Invader's hold,
                // where 4,000 ticks at t100 records 4,100.
                Expect.isEmpty
                    (shutAt 300 reserved)
                    "on the tick the hold ends the room is back in the pool, nobody having looked"

                Expect.isEmpty (recheckedAt 300 reserved) "and no latch was ever taken to re-check"
            }

            test "a latched room is looked into again every RivalRecheck ticks" {
                // #165's second half. The latch is ownership's and stays
                // ownership's — the room is withheld from the work on every
                // tick below — but the gate hands the shell one room to look
                // into on the ticks a whole `RivalRecheck` after the look
                // that shut it, so the tick with vision that clears a latch
                // can arrive at all. Outside those ticks the room is
                // withdrawn and unscanned, which is ADR 0043 unchanged.
                let state = RaidState.empty |> raidTick 100 (quiet |> ownedByRival outpostRoom)
                let recheck = Tuning.defaults.RivalRecheck

                Expect.equal
                    (shutAt (100 + recheck) state)
                    (Set.singleton outpostRoom)
                    "the recheck tick withholds the room from the work exactly as every other tick does"

                Expect.isEmpty
                    (recheckedAt (100 + recheck - 1) state)
                    "one tick short of the stride, nothing is looked into"

                Expect.equal
                    (recheckedAt (100 + recheck) state)
                    (Set.singleton outpostRoom)
                    "on the stride itself the room is re-admitted to the scan"

                // The look is one tick long because taking it stamps the
                // stride, so the test for that has to fold the tick the look
                // was taken on — the gate alone, asked twice about a log
                // nothing wrote to in between, is being asked about a look
                // that is still owed (#275, and the test below).
                let looked = state |> raidTick (100 + recheck) (quiet |> ownedByRival outpostRoom)

                Expect.isEmpty
                    (recheckedAt (100 + recheck + 1) looked)
                    "and the look is one tick long"

                Expect.equal
                    (recheckedAt (100 + 2 * recheck) looked)
                    (Set.singleton outpostRoom)
                    "a look that changed nothing leaves the next one a whole stride away"

                Expect.isEmpty
                    (recheckedAt 100 state)
                    "the look that shut the gate is not itself a recheck"

                // The knob at a second value, which is what makes it a
                // tunable and not a constant in disguise (ADR 0052 decision
                // 5). The exact-multiple rule had its own trap here — 5,000 is
                // a multiple of 1,000, so a colony tuned to 1,000 would have
                // looked on the shipped stride too — and the elapsed rule
                // (#275) has none: any value below the shipped one separates
                // the two at a tick between them. 3,000 is that, and the pair
                // below asks 3,100: a stride of 3,000 has elapsed there and a
                // stride of 5,000 has not, so the second assertion fails
                // outright if the constant is read in place of the knob.
                let sooner =
                    { Tuning.defaults with
                        RivalRecheck = 3000
                    }

                Expect.equal
                    (standDown sooner 3_100 state).Rechecked
                    (Set.singleton outpostRoom)
                    "a colony tuned to look oftener looks on its own stride"

                Expect.isEmpty
                    (recheckedAt 3_100 state)
                    "and on that tick the shipped stride is not due yet: the knob is read, not a constant"

                Expect.isEmpty
                    (standDown
                        { Tuning.defaults with
                            RivalRecheck = 0
                        }
                        (100 + recheck)
                        state)
                        .Rechecked
                    "a stride of zero is this rule switched off, not a tick divided by nothing"
            }

            test "a tick the gate was never evaluated on delays the look, never forfeits it" {
                // #275. The stride used to be an exact-multiple test —
                // `(tick - since) % RivalRecheck = 0` — a gate that has to be
                // asked on precisely the right tick or not at all. A tick's
                // evaluation is not guaranteed: the loop can throw before the
                // log is written, the engine cuts a tick short when the bot is
                // out of CPU and the bucket is empty, and a deploy lands in the
                // middle of one. Under the old test every tick lost that way
                // cost a whole 5,000 ticks of an outpost's income, silently,
                // because the next tick the gate answered on was another stride
                // away. A look that is owed stays owed.
                let recheck = Tuning.defaults.RivalRecheck
                let state = RaidState.empty |> raidTick 100 (quiet |> ownedByRival outpostRoom)

                Expect.isEmpty
                    (recheckedAt (100 + recheck - 1) state)
                    "one tick short of the stride the look is not owed yet"

                Expect.equal
                    (recheckedAt (100 + recheck + 1) state)
                    (Set.singleton outpostRoom)
                    "the tick after the stride, with nothing having looked, the look is still owed"

                Expect.equal
                    (recheckedAt (100 + recheck + 3_000) state)
                    (Set.singleton outpostRoom)
                    "and three thousand ticks after it, nowhere near a multiple of the stride"

                Expect.equal
                    (recheckedAt (100 + 3 * recheck - 1) state)
                    (Set.singleton outpostRoom)
                    "two whole strides of missed ticks are a late look and not a lost one"
            }

            test "a look stamped ahead of the clock is owed now, and the fold stamps the real tick" {
                // #275's own hazard, which the exact multiple it replaces did
                // not have. An elapsed test has no upper bound, so a
                // `LastLooked` in the future absorbs every tick until the
                // clock catches it up and a stride passes on top — a latch
                // silenced for longer than the stride, which is exactly the
                // unfalsifiable gate #165 bought its way out of. Two ways in,
                // both real: a mistyped hand edit of this leaf, which is the
                // documented way out of a stuck latch, and a private server
                // rolled back behind the tick the log was written on. No look
                // is taken on a tick that has not happened, so a stamp ahead
                // of the clock is a wrong number and not a record.
                let ahead =
                    { RaidState.empty with
                        RivalHeld =
                            Map.ofList [ outpostRoom, { Since = 100; LastLooked = 9_000_000 } ]
                    }

                Expect.equal
                    (recheckedAt 100_000 ahead)
                    (Set.singleton outpostRoom)
                    "the look is owed on the first tick the gate is asked, not nine million ticks out"

                Expect.equal
                    (shutAt 100_000 ahead)
                    (Set.singleton outpostRoom)
                    "and the room is withheld from the work through it, as on every other tick"

                // The gate corrects in one tick; the leaf corrects with it,
                // because the fold stamps the tick the look actually fell due
                // on over the impossible one.
                let healed = ahead |> raidTick 100_000 (quiet |> ownedByRival outpostRoom)

                Expect.equal
                    healed.RivalHeld
                    (Map.ofList [ outpostRoom, { Since = 100; LastLooked = 100_000 } ])
                    "the impossible stamp is written over by the look that was taken, the date standing"

                Expect.isEmpty
                    (recheckedAt 100_001 healed)
                    "so the stride runs from the real look and the room is not re-read every tick"
            }

            test "a clocked stand-down is never re-checked, and an empty log never looks" {
                // The recheck belongs to the latch alone: a clocked
                // stand-down needs no look, because its own clock takes the
                // room back (ADR 0043), and scanning it early would cost a
                // room read for an answer nothing acts on.
                let clocked = RaidState.empty |> raidTick 100 (seen [ core outpostRoom (Some 900) ])

                Expect.equal
                    (shutAt 800 clocked)
                    (Set.singleton outpostRoom)
                    "the core's clock is still running"

                Expect.isEmpty
                    (recheckedAt (100 + Tuning.defaults.RivalRecheck) clocked)
                    "and the stride falls due on nothing: there is no latch here to question"

                Expect.isEmpty
                    (recheckedAt 5_100 RaidState.empty)
                    "an empty log looks into nothing on any tick"
            }

            test "an empty log withholds nothing" {
                // The colony's ordinary state, and the one it has run in
                // since ADR 0042 filled the declaration: no outpost has
                // ever held a core, so the gate is open and the shell
                // scans every room a human declared.
                Expect.isEmpty (shutAt 100 RaidState.empty) "nothing is recorded, nothing is shut"
            }
        ]
