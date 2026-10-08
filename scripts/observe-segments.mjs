// The memory segments the operator-only logs live in (#639), as
// `src/App/ObserveMemory.fs` writes them: each log's JSON cut across its ids
// in order, the unused ones empty. `scripts/wire-check.mjs` holds these to
// the bundle's own constants.

/// Each log's segment ids, in order (`ObserveMemory.creepSegments`,
/// `ObserveMemory.cpuSegments`, the rival dossier's
/// `ObserveMemory.rivalSegments`, #653, and the operation ledger's
/// `ObserveMemory.ledgerSegments`, #662).
export const SEGMENTS = { creeps: [0, 1, 2, 3], cpu: [4, 5], rivals: [6, 7], ledger: [8] };

/// Ticks between two segment writes (`ObserveMemory.segmentEvery`): how
/// stale a log read off the segments may be on a healthy bot.
export const SEGMENT_EVERY = 10;

/// One log off its segments' contents, in id order: the chunks joined and
/// parsed. Undefined for none written (a chunk the server answers null for
/// is empty) and for chunks that will not parse.
export function joinSegments(chunks) {
  const text = chunks.map((chunk) => (typeof chunk === "string" ? chunk : "")).join("");
  if (text === "") return undefined;
  try {
    return JSON.parse(text);
  } catch {
    return undefined;
  }
}

/// One log off the server: the Memory leaf when it stands — an older bundle
/// writes it, as after a rollback, and the segments then hold a stale copy —
/// else its segments. Undefined when neither holds one; throws when a read
/// fails, for the caller to word.
export async function readLog(api, shard, leaf) {
  const res = await api.userMemoryGet(`fabot.observe.${leaf}`, shard);
  if (res?.ok !== 1) throw new Error(`memory read failed: ${JSON.stringify(res)}`);
  let data = res.data;
  // A leaf may come back gzip'd under a `gz:` prefix.
  if (typeof data === "string" && data.startsWith("gz:")) {
    const { gunzipSync } = await import("node:zlib");
    data = JSON.parse(gunzipSync(Buffer.from(data.slice(3), "base64")).toString());
  }
  if (data != null && typeof data === "object") return data;
  const seg = await api.userMemorySegmentGet(SEGMENTS[leaf], shard);
  if (seg?.ok !== 1) throw new Error(`segment read failed: ${JSON.stringify(seg)}`);
  return joinSegments(Array.isArray(seg.data) ? seg.data : [seg.data]);
}
