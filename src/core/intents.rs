use super::mv::Wish;
use super::vocabulary::{BodyPart, Resource};
use super::world::Kind;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CreepAct {
    /// TOP = 1, clockwise to TOP_LEFT = 8.
    Move(i32),
    Say(String),
    Harvest(String),
    Transfer(String, Resource),
    Withdraw(String, Resource),
    Pickup(String),
    Build(String),
    Repair(String),
    Upgrade(String),
    Reserve(String),
    Unreserve(String),
    ClaimController(String),
    AttackCreep(String),
    RangedAttackCreep(String),
    RangedMassAttack,
    HealCreep(String),
    RangedHealCreep(String),
    Dismantle(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SpawnAct {
    SpawnCreep(Vec<BodyPart>, String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RoomAct {
    CreateSite(i32, i32, Kind),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LinkAct {
    TransferEnergy(String, i32),
}

/// The output lab runs it off the two reagent labs, first reagent first.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LabAct {
    RunReaction(String, String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ControllerAct {
    ActivateSafeMode,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TowerAct {
    Attack(String),
    Heal(String),
}

/// Where an ask stands in its room's queue: a floor is never displaced,
/// a fight's goes before the economy's and holds it while it waits on the bank.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Rank {
    Floor,
    Fight,
    Economy,
}

impl Rank {
    pub fn is_floor(self) -> bool {
        self == Rank::Floor
    }
    pub fn is_fight(self) -> bool {
        self == Rank::Fight
    }
    pub fn is_economy(self) -> bool {
        self == Rank::Economy
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Ask {
    pub body: Vec<BodyPart>,
    pub name: String,
    pub rank: Rank,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Intent {
    Creep(String, CreepAct),
    Spawn(String, SpawnAct),
    Room(String, RoomAct),
    Tower(String, TowerAct),
    Controller(String, ControllerAct),
    Link(String, LinkAct),
    Lab(String, LabAct),
    /// Tick serves these from the room's queue; none reaches `Executor`.
    Queue(String, Ask),
    /// Tick arbitrates every module's wishes in a room as one.
    Wish(String, Wish),
}

impl Intent {
    pub fn actor(&self) -> String {
        match self {
            Intent::Creep(name, _) => name.clone(),
            Intent::Spawn(id, _) => id.clone(),
            Intent::Room(name, _) => name.clone(),
            Intent::Tower(id, _) => id.clone(),
            Intent::Controller(id, _) => id.clone(),
            Intent::Link(id, _) => id.clone(),
            Intent::Lab(id, _) => id.clone(),
            Intent::Queue(room, _) => room.clone(),
            Intent::Wish(_, wish) => wish.creep.clone(),
        }
    }

    pub fn is_queue(&self) -> bool {
        matches!(self, Intent::Queue(_, _))
    }

    pub fn is_wish(&self) -> bool {
        matches!(self, Intent::Wish(_, _))
    }
}
