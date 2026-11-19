use super::mv::Wish;
use super::vocabulary::{Part, Resource};
use super::world::{Id, Kind, RoomName};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CreepAct {
    /// TOP = 1, clockwise to TOP_LEFT = 8.
    Move(i32),
    Say(String),
    Harvest(Id),
    Transfer(Id, Resource),
    Withdraw(Id, Resource),
    Pickup(Id),
    Drop(Resource),
    Build(Id),
    Repair(Id),
    Upgrade(Id),
    Reserve(Id),
    Unreserve(Id),
    ClaimController(Id),
    AttackCreep(Id),
    RangedAttackCreep(Id),
    RangedMassAttack,
    HealCreep(Id),
    RangedHealCreep(Id),
    Dismantle(Id),
}

pub const MELEE: u8 = 1;
pub const RANGED: u8 = 2;

impl CreepAct {
    /// The engine's pipelines it takes and its rank in them, first highest:
    /// vanilla's `intents.js` priorities, xxscreeps' intent `type`s.
    fn pipeline(&self) -> (u8, u8) {
        match self {
            CreepAct::HealCreep(_) => (MELEE, 0),
            CreepAct::RangedHealCreep(_) => (MELEE | RANGED, 1),
            CreepAct::Dismantle(_) => (MELEE, 2),
            CreepAct::Repair(_) => (MELEE | RANGED, 3),
            CreepAct::Build(_) => (MELEE | RANGED, 4),
            CreepAct::RangedMassAttack => (RANGED, 5),
            CreepAct::RangedAttackCreep(_) => (RANGED, 6),
            CreepAct::AttackCreep(_) => (MELEE, 7),
            CreepAct::Harvest(_) => (MELEE, 8),
            _ => (0, 0),
        }
    }

    pub fn slots(&self) -> u8 {
        self.pipeline().0
    }
}

/// What the engine performs of one body's acts in a tick. Vanilla cancels an
/// act beside any higher one wanted in its pipeline, performed or not; of two
/// alike, the last call stands.
pub fn performed(acts: &[CreepAct]) -> Vec<CreepAct> {
    let beats = |(i, a): (usize, &CreepAct), (j, b): (usize, &CreepAct)| {
        let ((sa, ra), (sb, rb)) = (a.pipeline(), b.pipeline());
        sa & sb != 0 && (rb < ra || rb == ra && j > i)
    };
    acts.iter()
        .enumerate()
        .filter(|a| !acts.iter().enumerate().any(|b| beats(*a, b)))
        .map(|(_, a)| a.clone())
        .collect()
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SpawnAct {
    SpawnCreep(Vec<Part>, String, Vec<Id>),
    RecycleCreep(Id),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RoomAct {
    CreateSite(i32, i32, Kind),
    Destroy(Id),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LinkAct {
    TransferEnergy(Id, i32),
}

/// The output lab runs it off the two reagent labs, first reagent first.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LabAct {
    RunReaction(Id, Id),
    /// The lab spends its compound and energy on the parts of the body with this id.
    BoostCreep(Id),
}

/// `send`: resource, amount, the room it lands in.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TerminalAct {
    Send(Resource, i32, RoomName),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ControllerAct {
    ActivateSafeMode,
    Unclaim,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TowerAct {
    Attack(Id),
    Heal(Id),
    Repair(Id),
}

/// Where an ask stands in its room's queue: a floor is never displaced,
/// a fight's goes before the economy's and holds it while it waits on the bank.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Rank {
    Floor,
    Fight,
    Economy,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ask {
    pub body: Vec<Part>,
    pub name: String,
    pub rank: Rank,
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct Refusal {
    pub actor: String,
    pub act: String,
    pub code: i32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Intent {
    Creep(String, CreepAct),
    Spawn(Id, SpawnAct),
    Room(RoomName, RoomAct),
    Tower(Id, TowerAct),
    Controller(Id, ControllerAct),
    Link(Id, LinkAct),
    Lab(Id, LabAct),
    Terminal(Id, TerminalAct),
    /// An observer and the room it brings in sight next tick.
    Observe(Id, RoomName),
    /// Tick serves these from the room's queue; none reaches `Executor`.
    Queue(RoomName, Ask),
    /// Tick arbitrates every module's wishes in a room as one.
    Wish(RoomName, Wish),
    /// A body's scored tiles, for the harness's trace; nothing reaches the game.
    Trace(String, Vec<(crate::world::Pos, i64)>),
    /// Summed into the observe segment's counts (§9.3).
    Count(&'static str, i32),
    /// A colony's open slots of a pattern left uncast; nothing reaches the game.
    Short(RoomName, crate::economy::bodies::Pattern, i32),
    /// A movable body the arbitration left standing: read by the metrics alone, nothing reaches the game.
    Held(String),
}

impl Intent {
    pub fn actor(&self) -> String {
        match self {
            Intent::Creep(name, _) | Intent::Trace(name, _) | Intent::Held(name) => name.clone(),
            Intent::Wish(_, wish) => wish.creep.clone(),
            Intent::Spawn(id, _) | Intent::Tower(id, _) | Intent::Controller(id, _) => {
                id.to_string()
            }
            Intent::Link(id, _) | Intent::Lab(id, _) | Intent::Terminal(id, _) => id.to_string(),
            Intent::Observe(id, _) => id.to_string(),
            Intent::Room(room, _) | Intent::Queue(room, _) | Intent::Short(room, _, _) => {
                room.to_string()
            }
            Intent::Count(name, _) => name.to_string(),
        }
    }

    pub fn is_queue(&self) -> bool {
        matches!(self, Intent::Queue(_, _))
    }

    pub fn is_wish(&self) -> bool {
        matches!(self, Intent::Wish(_, _))
    }
}

#[cfg(test)]
mod tests {
    use super::{CreepAct, performed};
    use crate::world::Id;

    #[test]
    fn the_engine_performs_one_act_per_pipeline_slot_by_its_priority() {
        let id = || Id::from("x");
        let (heal, ranged_heal) = (CreepAct::HealCreep(id()), CreepAct::RangedHealCreep(id()));
        let (attack, shot) = (
            CreepAct::AttackCreep(id()),
            CreepAct::RangedAttackCreep(id()),
        );
        let (mass, dismantle) = (CreepAct::RangedMassAttack, CreepAct::Dismantle(id()));
        let table: Vec<(Vec<CreepAct>, Vec<CreepAct>)> = vec![
            (vec![attack.clone(), heal.clone()], vec![heal.clone()]),
            (
                vec![shot.clone(), ranged_heal.clone()],
                vec![ranged_heal.clone()],
            ),
            (
                vec![mass.clone(), ranged_heal.clone()],
                vec![ranged_heal.clone()],
            ),
            (
                vec![attack.clone(), ranged_heal.clone()],
                vec![ranged_heal.clone()],
            ),
            (vec![heal.clone(), ranged_heal.clone()], vec![heal.clone()]),
            (
                vec![heal.clone(), shot.clone()],
                vec![heal.clone(), shot.clone()],
            ),
            (
                vec![attack.clone(), shot.clone()],
                vec![attack.clone(), shot.clone()],
            ),
            (
                vec![attack.clone(), dismantle.clone()],
                vec![dismantle.clone()],
            ),
            (
                vec![dismantle.clone(), mass.clone()],
                vec![dismantle.clone(), mass.clone()],
            ),
            (vec![shot.clone(), mass.clone()], vec![mass]),
            (vec![heal.clone(), ranged_heal, shot], vec![heal.clone()]),
            (
                vec![CreepAct::AttackCreep("a".into()), attack.clone()],
                vec![attack],
            ),
            (
                vec![CreepAct::Move(1), heal.clone(), dismantle],
                vec![CreepAct::Move(1), heal],
            ),
        ];
        for (wanted, done) in table {
            assert_eq!(performed(&wanted), done, "{wanted:?}");
        }
    }
}
