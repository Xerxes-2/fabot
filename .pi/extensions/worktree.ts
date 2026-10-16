/**
 * The WorktreeCreate and SubagentStop hooks Claude Code ran around an isolated subagent, which
 * pi has no event for; pi-claude-hooks covers the rest of .claude/settings.json. The jj rules
 * stay in .claude/hooks/*.sh, which this feeds. Pi's Agent tool cannot carry `isolation`, so a
 * spawn asks with `worktree_path: "auto"`.
 */

import { spawn } from "node:child_process";
import { randomUUID } from "node:crypto";
import { existsSync } from "node:fs";
import path from "node:path";
import { isToolCallEventType, type ExtensionAPI } from "@earendil-works/pi-coding-agent";

const AGENT = "Agent";
const AUTO = "auto";
const MARKER = "/.claude/worktrees/";
const AGENT_DIR = "agent-";
const CREATE = "worktree-create.sh";
const STOP = "subagent-stop.sh";

type AgentInput = { worktree_path?: string; run_in_background?: boolean };

/** The repo holding `from`, whether that is a directory in it or a workspace under it. */
function repoRoot(from: string): string | null {
  const at = from.indexOf(MARKER);
  let dir = at === -1 ? from : from.slice(0, at);
  for (;;) {
    if (existsSync(path.join(dir, ".claude/hooks", CREATE))) return dir;
    const up = path.dirname(dir);
    if (up === dir) return null;
    dir = up;
  }
}

/** A hook: JSON on stdin, stdout out, stderr as the rejection's message. */
function hook(root: string, script: string, payload: unknown): Promise<string> {
  return new Promise((resolve, reject) => {
    const child = spawn(path.join(root, ".claude/hooks", script), [], { cwd: root });
    let out = "";
    let err = "";
    child.stdout.on("data", (chunk) => (out += chunk));
    child.stderr.on("data", (chunk) => (err += chunk));
    child.on("error", reject);
    child.on("close", (code) =>
      code === 0 ? resolve(out.trim()) : reject(new Error(err.trim() || `${script} exited ${code}`)),
    );
    child.stdin.end(JSON.stringify(payload));
  });
}

export default function (pi: ExtensionAPI) {
  /** subagent-stop.sh, not worktree-remove.sh: a workspace still holding the agent's work stays. */
  const stop = async (workspace: string): Promise<void> => {
    const name = path.basename(workspace);
    if (!name.startsWith(AGENT_DIR)) return;
    const root = repoRoot(workspace);
    if (!root) return;
    await hook(root, STOP, { agent_id: name.slice(AGENT_DIR.length), background_tasks: [] });
  };

  pi.on("tool_call", async (event, ctx) => {
    if (!isToolCallEventType<typeof AGENT, AgentInput>(AGENT, event)) return;
    if (event.input.worktree_path !== AUTO) return;
    const root = repoRoot(ctx.cwd);
    if (!root) return { block: true, reason: `worktree_path "${AUTO}" outside a repo with .claude/hooks/${CREATE}` };
    try {
      event.input.worktree_path = await hook(root, CREATE, {
        name: AGENT_DIR + randomUUID().slice(0, 8),
        session_id: ctx.sessionManager.getSessionId(),
      });
    } catch (error) {
      // A spawn that got no workspace would edit the main tree instead of its own.
      return { block: true, reason: `worktree-create failed: ${error instanceof Error ? error.message : String(error)}` };
    }
  });

  pi.on("tool_result", async (event) => {
    if (event.toolName !== AGENT) return;
    // A foreground spawn's result is the agent's stop. A background one's came back at the
    // spawn, so its workspace waits for the session ending below.
    if (event.input.run_in_background === true) return;
    const status = (event.details as { status?: unknown } | undefined)?.status;
    if (status === "running" || status === "queued") return;
    const workspace = event.input.worktree_path;
    if (typeof workspace === "string") await stop(workspace);
  });

  pi.on("session_shutdown", async (_event, ctx) => {
    if (!ctx.cwd.includes(MARKER)) return;
    // The workspace's own session ending: a background agent stopped or cleared, or a settled
    // one the parent disposed at the end of its own session.
    await stop(ctx.cwd);
  });
}
