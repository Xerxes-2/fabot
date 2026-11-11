// TooAngel as the fixed attacker (ARCHITECTURE.md §10.2). Its code is AGPL
// and never in this tree: `fetch.json` names the package soak.mjs fetches
// beside this file, its main.js as `tooangel`. The engine's own Memory save
// overflows its stack on TooAngel's deep Memory near tick 7000, so the loop
// saves Memory itself. `opponent.js` { target, first, every, siege? } sends
// its melee squad, or with `siege` its dismantler and three healers, at
// target from its first room, at first, first + every, … ticks after this
// code loaded; its config ranks the squad first in its spawn queue, where by
// default its economy starves the squad at RCL2.
const { config, pin } = require("seat");
const tooangel = require("tooangel");
global.config.priorityQueue.otherRoom.autoattackmelee = 0;
global.config.priorityQueue.otherRoom.squadheal = 0;
global.config.priorityQueue.otherRoom.squadsiege = 0;
let loaded;

module.exports.loop = function () {
  pin();
  loaded ??= Game.time;
  tooangel.loop();
  const t = Game.time - loaded;
  if (config.target && t >= config.first && (t - config.first) % config.every === 0) {
    const home = Object.values(Game.rooms).find((r) => r.controller && r.controller.my);
    if (home) {
      const squads = require("brain_squadmanager");
      (config.siege ? squads.startSquad : squads.startMeleeSquad)(home.name, config.target);
      console.log(`attack ordered from ${home.name} at ${config.target}`);
    }
  }
  RawMemory.set(JSON.stringify(Memory));
};
