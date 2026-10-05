// Heap snapshots of a user's isolate (#543): turns the isolate inspector on
// and, when `<world>/snap.req` appears ({ user, out } as JSON), runs
// HeapProfiler.takeHeapSnapshot in that user's sandbox and writes the
// joined chunks to `out`, then `out.done` with the byte count.
import * as fs from "node:fs";
import { hooks } from "xxscreeps/driver/index.js";

hooks.register("isolateInspector", true);
const sandboxes = new Map();
hooks.register("sandboxCreated", (sandbox, userId) => sandboxes.set(userId, sandbox));

setInterval(() => {
  let request;
  try {
    request = JSON.parse(fs.readFileSync("snap.req", "utf8"));
  } catch {
    return; // none asked, or another thread took it
  }
  const { user, out } = request;
  // Each runner thread loads this mod; the one holding the user's sandbox answers.
  const sandbox = sandboxes.get(user);
  if (!sandbox) return;
  try {
    fs.unlinkSync("snap.req");
  } catch {
    return;
  }
  const t0 = Date.now();
  const session = sandbox.createInspectorSession();
  const chunks = [];
  session.onNotification = (msg) => {
    const m = JSON.parse(msg);
    if (m.method === "HeapProfiler.addHeapSnapshotChunk") chunks.push(m.params.chunk);
  };
  session.onResponse = (id) => {
    if (id !== 2) return;
    const text = chunks.join("");
    fs.writeFileSync(out, text);
    fs.writeFileSync(`${out}.done`, JSON.stringify({ bytes: text.length, ms: Date.now() - t0 }));
    session.dispose();
  };
  session.dispatchProtocolMessage(JSON.stringify({ id: 1, method: "HeapProfiler.enable" }));
  session.dispatchProtocolMessage(
    JSON.stringify({ id: 2, method: "HeapProfiler.takeHeapSnapshot", params: { reportProgress: false } }),
  );
}, 250);
