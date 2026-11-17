// A scripted rival, one of §10.2's fixed opponents (ARCHITECTURE.md):
// `opponent.js`'s { role } picks its behaviour list in `roles.js` (keep, remote,
// raid, drain, pass) and its other keys are that role's own.
let config = {};
try {
  config = require("opponent");
} catch {}
const roles = require("roles");
const role = Object.hasOwn(roles, config.role) && roles[config.role];
if (!role) throw new Error(`opponent.js: no rival role ${config.role}`);
module.exports.loop = () => role(config);
