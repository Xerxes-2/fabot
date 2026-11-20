// Tags are `executor.rs`'s enums, which pack the records: a change to one is a
// change to both. Nothing is kept: the views die with the tick.
const CREEP = 0;
const ROOM = 1;
const [INT, TEXT, BY_ID, TEXTS] = [0, 1, 2, 3];
const [CODE, GONE, THREW] = [0, 1, 2];
const NOT_FOUND = -5;

// A lookup that throws finds nothing, as a missing object does.
const byId = (id) => {
  try {
    return Game.getObjectById(id) ?? undefined;
  } catch {}
};
const named = (kind, name) => {
  try {
    return (kind === CREEP ? Game.creeps : Game.rooms)[name] ?? undefined;
  } catch {}
};

globalThis.__fabotExec = (ints, text) => {
  const strs = text.split("\0");
  const refused = [];
  for (let at = 0, i = 0; at < ints.length; i++) {
    const kind = ints[at];
    const actor = strs[ints[at + 1]];
    const method = strs[ints[at + 2]];
    const argc = ints[at + 3];
    at += 4;
    const self = kind === CREEP || kind === ROOM ? named(kind, actor) : byId(actor);
    const args = [];
    let found = true;
    for (let k = 0; k < argc; k++) {
      const tag = ints[at];
      const x = ints[at + 1];
      at += 2;
      if (tag === INT) args.push(x);
      else if (tag === TEXT) args.push(strs[x]);
      else if (tag === BY_ID) {
        const target = byId(strs[x]);
        found &&= target !== undefined;
        args.push(target);
      } else {
        const words = Array.from(ints.subarray(at, (at += x)), (w) => strs[w]);
        args.push(tag === TEXTS ? words : x === 0 ? {} : { energyStructures: words.map(byId).filter((s) => s !== undefined) });
      }
    }
    if (self === undefined) refused.push(i, NOT_FOUND, GONE);
    else if (!found) refused.push(i, NOT_FOUND, CODE);
    else
      try {
        const code = +self[method](...args) | 0;
        if (code !== 0) refused.push(i, code, CODE);
      } catch {
        refused.push(i, -100, THREW);
      }
  }
  return refused;
};
