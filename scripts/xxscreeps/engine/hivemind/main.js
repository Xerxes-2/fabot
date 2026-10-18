// Hivemind (Mirroar's, MIT) as a fixed defender (ARCHITECTURE.md §10.2): a
// bot that holds its rooms as a player's does. `fetch.json` names its bundle,
// never in this tree, as `hivemind`.
const { loop } = require("hivemind");

// Hivemind reads its rooms' ramparts and buildings off the plan it laid them
// out by; seated on a room laid out by another, its guardians find no rampart
// (a throw that stops every creep after them) and it razes what stands.
let adopted = false;
Object.defineProperty(Room.prototype, "roomPlanner", {
  configurable: true,
  set(planner) {
    if (!adopted) {
      const plan = Object.getPrototypeOf(planner);
      const planned = plan.isPlannedLocation;
      plan.isPlannedLocation = function (pos, type) {
        return planned.call(this, pos, type) || pos.lookFor(LOOK_STRUCTURES).some((s) => s.structureType === type);
      };
      adopted = true;
    }
    Object.defineProperty(this, "roomPlanner", { value: planner, writable: true, configurable: true });
  },
});

// No market offline: its trade process throws on the history it reads.
const run = hivemind.runProcess;
hivemind.runProcess = function (id, ...rest) {
  if (id !== "empire.trade") run.call(this, id, ...rest);
};

module.exports.loop = loop;
