// A server mod: before each run, sets the user's `__intents` to the intents
// the runner billed (0.2 CPU each) for that user's previous tick, or null.
module.exports = (config) => {
  if (!config.engine) return;
  const billed = {};
  let wrapped = false;
  config.engine.on("playerSandbox", (sandbox, userId) => {
    if (!wrapped) {
      wrapped = true;
      const key = Object.keys(require.cache).find((k) => k.endsWith("@screeps/engine/dist/utils.js"));
      if (!key) console.error("intents.cjs error: @screeps/engine/dist/utils.js is not loaded; no intent counts");
      const utils = key && require.cache[key].exports;
      const store = utils?.storeIntents;
      // The driver's own count (make.js `intentsCount`): say and pull are free.
      if (store)
        utils.storeIntents = (user, list, ...rest) => {
          let n = 0;
          for (const id in list)
            for (const name in list[id]) if (name !== "say" && name !== "pull") n += Array.isArray(list[id][name]) ? list[id][name].length : 1;
          billed[user] = n;
          return store(user, list, ...rest);
        };
    }
    sandbox.set("__intents", billed[userId] ?? null);
    delete billed[userId];
  });
};
