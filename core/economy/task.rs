/// What a body is hired to do; Memory holds it as its `task_id`.
use std::fmt::Write;

use crate::vocabulary::{Resource, resource_name, resource_of_name};
use crate::world::{Id, RoomName};

/// What a task acts on: an object, or a colony's spawn ring or bank pile,
/// which `run` resolves to the object it serves when the body arrives.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Target {
    Object(Id),
    Ring(RoomName),
    Pile(RoomName),
}

impl From<Id> for Target {
    fn from(id: Id) -> Target {
        Target::Object(id)
    }
}

const RING: &str = "ring-";
const PILE: &str = "pile-";

impl Target {
    pub fn object(self) -> Option<Id> {
        match self {
            Target::Object(id) => Some(id),
            Target::Ring(_) | Target::Pile(_) => None,
        }
    }

    fn write(self, out: &mut String) {
        let _ = match self {
            Target::Object(id) => out.write_str(&id.text()),
            Target::Ring(room) => write!(out, "{RING}{room}"),
            Target::Pile(room) => write!(out, "{PILE}{room}"),
        };
    }

    fn parse(text: &str) -> Option<Target> {
        let room = |tag: &str| text.strip_prefix(tag).and_then(RoomName::parse);
        (room(RING).map(Target::Ring))
            .or_else(|| room(PILE).map(Target::Pile))
            .or_else(|| Id::parse(text).map(Target::Object))
    }
}

/// A body's sticky slot and the task it did last tick.
#[derive(Clone, Debug, PartialEq)]
pub struct Assignment {
    pub slot: String,
    pub task: Option<Task>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Task {
    Harvest(Target),
    Withdraw(Target, Resource),
    Pickup(Target, Resource),
    Refill(Target, Resource),
    Build(Target),
    Repair(Target),
    Upgrade(Target),
    Reserve(Target),
    Claim(Target),
    /// A carrier bringing this body its load.
    Meet(Target),
    /// A room, by name: what it holds is not seen until a body stands in it.
    Scout(RoomName),
}

impl Task {
    pub fn target(&self) -> Option<Target> {
        match self {
            Task::Harvest(t)
            | Task::Withdraw(t, _)
            | Task::Pickup(t, _)
            | Task::Refill(t, _)
            | Task::Build(t)
            | Task::Repair(t)
            | Task::Upgrade(t)
            | Task::Reserve(t)
            | Task::Claim(t)
            | Task::Meet(t) => Some(*t),
            Task::Scout(_) => None,
        }
    }
}

/// Injective because a target and a resource name hold no `:`.
pub fn task_id(task: &Task) -> String {
    let tag = match task {
        Task::Harvest(_) => "h:",
        Task::Withdraw(..) => "w:",
        Task::Pickup(..) => "p:",
        Task::Refill(..) => "f:",
        Task::Build(_) => "b:",
        Task::Repair(_) => "r:",
        Task::Upgrade(_) => "u:",
        Task::Reserve(_) => "v:",
        Task::Claim(_) => "c:",
        Task::Meet(_) => "m:",
        Task::Scout(_) => "s:",
    };
    let mut out = String::with_capacity(40);
    out.push_str(tag);
    match (task, task.target()) {
        (Task::Scout(room), _) => {
            let _ = write!(out, "{room}");
        }
        (_, Some(target)) => target.write(&mut out),
        (_, None) => (),
    }
    if let Task::Withdraw(_, r) | Task::Pickup(_, r) | Task::Refill(_, r) = task {
        out.push(':');
        out.push_str(&resource_name(r));
    }
    out
}

pub fn of_task_id(id: &str) -> Option<Task> {
    let mut parts = id.split(':');
    let [tag, text, resource, rest] = [parts.next(), parts.next(), parts.next(), parts.next()];
    if let (Some("s"), Some(room), None) = (tag, text, resource) {
        return Some(Task::Scout(RoomName::parse(room)?));
    }
    let t = Target::parse(text?)?;
    match (tag?, resource.map(resource_of_name), rest) {
        ("h", None, _) => Some(Task::Harvest(t)),
        ("w", Some(r), None) => Some(Task::Withdraw(t, r)),
        ("p", Some(r), None) => Some(Task::Pickup(t, r)),
        ("f", Some(r), None) => Some(Task::Refill(t, r)),
        ("b", None, _) => Some(Task::Build(t)),
        ("r", None, _) => Some(Task::Repair(t)),
        ("u", None, _) => Some(Task::Upgrade(t)),
        ("v", None, _) => Some(Task::Reserve(t)),
        ("c", None, _) => Some(Task::Claim(t)),
        ("m", None, _) => Some(Task::Meet(t)),
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

    pub fn range_of(&self) -> i32 {
        match self {
            Task::Build(_) | Task::Repair(_) | Task::Upgrade(_) => RANGE_UPGRADE,
            Task::Harvest(_)
            | Task::Withdraw(_, _)
            | Task::Pickup(_, _)
            | Task::Refill(_, _)
            | Task::Reserve(_)
            | Task::Claim(_)
            | Task::Meet(_) => 1,
            Task::Scout(_) => RANGE_SCOUT,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::economy::dice::{self, Dice};

    #[test]
    fn a_task_id_reads_back_as_the_same_task() {
        let mut dice = Dice::new(1);
        for _ in 0..300 {
            let target = dice::target(&mut dice);
            let task = dice::task(&mut dice, target);
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
            let (ta, tb) = (dice::target(&mut dice), dice::target(&mut dice));
            let a = dice::task(&mut dice, ta);
            let b = dice::task(&mut dice, tb);
            assert!(task_id(&a) != task_id(&b) || format!("{a:?}") == format!("{b:?}"));
        }
    }
}
