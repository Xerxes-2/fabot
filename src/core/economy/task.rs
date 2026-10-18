/// What a body is hired to do; compared only through `task_id`, which
/// `assignments` keys on.
use crate::core::vocabulary::{Resource, resource_name, resource_of_name};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Task {
    Harvest(String),
    Withdraw(String, Resource),
    Pickup(String, Resource),
    Refill(String, Resource),
    Build(String),
    Repair(String),
    Upgrade(String),
    Reserve(String),
    Claim(String),
    /// A room, by name: what it holds is not seen until a body stands in it.
    Scout(String),
}

/// Injective because an object id and a resource name hold no `:`.
pub fn task_id(task: &Task) -> String {
    match task {
        Task::Harvest(id) => format!("h:{id}"),
        Task::Withdraw(id, r) => format!("w:{}:{}", id, resource_name(r)),
        Task::Pickup(id, r) => format!("p:{}:{}", id, resource_name(r)),
        Task::Refill(id, r) => format!("f:{}:{}", id, resource_name(r)),
        Task::Build(id) => format!("b:{id}"),
        Task::Repair(id) => format!("r:{id}"),
        Task::Upgrade(id) => format!("u:{id}"),
        Task::Reserve(id) => format!("v:{id}"),
        Task::Claim(id) => format!("c:{id}"),
        Task::Scout(room) => format!("s:{room}"),
    }
}

pub fn of_task_id(id: &str) -> Option<Task> {
    let parts: Vec<&str> = id.split(':').collect();
    match parts.as_slice() {
        ["h", target] => Some(Task::Harvest(target.to_string())),
        ["w", target, resource] => Some(Task::Withdraw(
            target.to_string(),
            resource_of_name(resource),
        )),
        ["p", target, resource] => {
            Some(Task::Pickup(target.to_string(), resource_of_name(resource)))
        }
        ["f", target, resource] => {
            Some(Task::Refill(target.to_string(), resource_of_name(resource)))
        }
        ["b", target] => Some(Task::Build(target.to_string())),
        ["r", target] => Some(Task::Repair(target.to_string())),
        ["u", target] => Some(Task::Upgrade(target.to_string())),
        ["v", target] => Some(Task::Reserve(target.to_string())),
        ["c", target] => Some(Task::Claim(target.to_string())),
        ["s", room] => Some(Task::Scout(room.to_string())),
        _ => None,
    }
}

/// `range_of` of a build, a repair or an upgrade: `upgradeController`'s reach.
pub const RANGE_UPGRADE: i32 = 3;

/// From the room's centre, every tile but its edges: a body in sight of the
/// room has arrived, whatever its terrain.
const RANGE_SCOUT: i32 = 23;

impl Task {
    pub fn is_intake(&self) -> bool {
        matches!(
            self,
            Task::Harvest(_) | Task::Withdraw(_, _) | Task::Pickup(_, _)
        )
    }

    pub fn is_refill(&self) -> bool {
        matches!(self, Task::Refill(_, _))
    }

    pub fn is_upgrade(&self) -> bool {
        matches!(self, Task::Upgrade(_))
    }

    pub fn is_build_or_repair(&self) -> bool {
        matches!(self, Task::Build(_) | Task::Repair(_))
    }

    pub fn range_of(&self) -> i32 {
        match self {
            Task::Build(_) | Task::Repair(_) | Task::Upgrade(_) => RANGE_UPGRADE,
            Task::Harvest(_)
            | Task::Withdraw(_, _)
            | Task::Pickup(_, _)
            | Task::Refill(_, _)
            | Task::Reserve(_)
            | Task::Claim(_) => 1,
            Task::Scout(_) => RANGE_SCOUT,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::economy::dice::{self, Dice};

    #[test]
    fn a_task_id_reads_back_as_the_same_task() {
        let mut dice = Dice::new(1);
        for _ in 0..300 {
            let target = dice::object_id(&mut dice);
            let task = dice::task(&mut dice, &target);
            assert_eq!(
                of_task_id(&task_id(&task)).map(|t| task_id(&t)),
                Some(task_id(&task))
            );
        }
    }

    #[test]
    fn two_tasks_with_one_id_are_one_task() {
        let mut dice = Dice::new(2);
        for _ in 0..300 {
            let (ta, tb) = (dice::object_id(&mut dice), dice::object_id(&mut dice));
            let a = dice::task(&mut dice, &ta);
            let b = dice::task(&mut dice, &tb);
            assert!(task_id(&a) != task_id(&b) || format!("{a:?}") == format!("{b:?}"));
        }
    }
}
