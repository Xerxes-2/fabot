use strum::IntoStaticStr;

use super::vocabulary::{Part, ResourceType};
use super::wish::Wish;
use super::world::{Id, RoomName, StructureType};

/// Into `src/app/executor.rs`'s verb, which a refusal is counted under.
#[derive(Clone, Debug, PartialEq, Eq, IntoStaticStr)]
#[strum(serialize_all = "camelCase")]
pub enum CreepAct {
    Move(i32),
    Say(String),
    Harvest(Id),
    Transfer(Id, ResourceType),
    Withdraw(Id, ResourceType),
    Pickup(Id),
    Drop(ResourceType),
    Build(Id),
    Repair(Id),
    Upgrade(Id),
    Reserve(Id),
    Unreserve(Id),
    #[strum(serialize = "claim")]
    ClaimController(Id),
    #[strum(serialize = "attack")]
    AttackCreep(Id),
    #[strum(serialize = "rangedAttack")]
    RangedAttackCreep(Id),
    RangedMassAttack,
    #[strum(serialize = "heal")]
    HealCreep(Id),
    #[strum(serialize = "rangedHeal")]
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
    CreateSite(i32, i32, StructureType),
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TerminalAct {
    Send {
        resource: ResourceType,
        amount: i32,
        to: RoomName,
    },
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
pub enum Action {
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
}

impl Action {
    pub fn actor(&self) -> String {
        match self {
            Action::Creep(name, _) => name.clone(),
            Action::Spawn(id, _) | Action::Tower(id, _) | Action::Controller(id, _) => {
                id.to_string()
            }
            Action::Link(id, _) | Action::Lab(id, _) | Action::Terminal(id, _) => id.to_string(),
            Action::Observe(id, _) => id.to_string(),
            Action::Room(room, _) => room.to_string(),
        }
    }
}

/// What a tick tells the operator; nothing reaches the game.
#[derive(Clone, Default)]
pub struct Telemetry {
    /// Summed into the observe segment's counts (§9.3).
    pub counts: Vec<(&'static str, i32)>,
    /// A colony's open slots of a pattern, by its name, left uncast.
    pub short: Vec<(RoomName, &'static str, i32)>,
    /// A body's scored tiles, for the harness's trace.
    pub traces: Vec<(String, Vec<(crate::world::Pos, i64)>)>,
}

/// A module's tick: `Tick` settles the asks and wishes of every module as one.
#[derive(Clone, Default)]
pub struct ModuleOutput {
    pub actions: Vec<Action>,
    pub asks: Vec<(RoomName, Ask)>,
    pub wishes: Vec<(RoomName, Wish)>,
    pub telemetry: Telemetry,
}

impl ModuleOutput {
    pub fn extend(&mut self, other: ModuleOutput) {
        self.actions.extend(other.actions);
        self.asks.extend(other.asks);
        self.wishes.extend(other.wishes);
        self.telemetry.counts.extend(other.telemetry.counts);
        self.telemetry.short.extend(other.telemetry.short);
        self.telemetry.traces.extend(other.telemetry.traces);
    }
}

impl From<Vec<Action>> for ModuleOutput {
    fn from(actions: Vec<Action>) -> Self {
        ModuleOutput {
            actions,
            ..ModuleOutput::default()
        }
    }
}

/// What `Executor` runs, and the movable bodies arbitration left standing,
/// which the metrics alone read.
#[derive(Default)]
pub struct Settled {
    pub actions: Vec<Action>,
    pub held: Vec<String>,
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
