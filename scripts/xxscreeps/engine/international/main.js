// The International (MIT; GPLv3 by its package.json) as a rival economy:
// `fetch.json` names its source, built in the cache, never in this tree, as
// `international`.
// Its build drops LogOps.registerStyles(): a style dump on every tick.
const { pin } = require("seat");
const { loop } = require("international");

// No market or ally segments offline, and its log would fill the console.
// An empty `allies` throws every tick, so its default stays.
Object.assign(global.settings, { marketUsage: false, allyCommunication: false, creepSay: false, logging: 0 });

module.exports.loop = () => {
  pin();
  loop();
};
