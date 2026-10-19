/**
 * The WorktreeCreate and SubagentStop hooks Claude Code ran around an isolated subagent, which
 * pi has no event for; pi-claude-hooks covers the rest of .claude/settings.json. The jj rules
 * stay in .claude/hooks/*.sh, which this feeds. pi-subagents' `subagent` tool isolates in a git
 * worktree of its own, which bypasses jj, so a spawn asks for a jj workspace with `cwd: "auto"`
 * and `worktree: true` is refused.
 */

import { spawn } from "node:child_process";
import { randomUUID } from "node:crypto";
import { existsSync } from "node:fs";
import path from "node:path";
import { isToolCallEventType, type ExtensionAPI } from "@earendil-works/pi-coding-agent";

const SUBAGENT = "subagent";
const AUTO = "auto";
const MARKER = "/.claude/worktrees/";
const AGENT_DIR = "agent-";
const CREATE = "worktree-create.sh";
const STOP = "subagent-stop.sh";
/** pi-subagents' completion of a background run, carrying its `runId`. */
const ASYNC_COMPLETE = "subagent:async-complete";

type SubagentInput = {
  cwd?: string;
  worktree?: boolean;
  action?: string;
  options?: { isolation?: string };
};

type SubagentDetails = { asyncId?: string; runId?: string; results?: { detached?: boolean }[] };

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
  /** The workspaces this session made, by toolCallId, until their spawn returns. */
  const made = new Map<string, string>();
  /** A background run's workspace, by runId, until the run completes. */
  const running = new Map<string, string>();

  /** subagent-stop.sh, not worktree-remove.sh: a workspace still holding the agent's work stays. */
  const stop = async (workspace: string): Promise<void> => {
    const name = path.basename(workspace);
    if (!name.startsWith(AGENT_DIR)) return;
    const root = repoRoot(workspace);
    if (!root) return;
    await hook(root, STOP, { agent_id: name.slice(AGENT_DIR.length), background_tasks: [] });
  };

  pi.on("tool_call", async (event, ctx) => {
    if (!isToolCallEventType<typeof SUBAGENT, SubagentInput>(SUBAGENT, event)) return;
    const input = event.input;
    const root = repoRoot(ctx.cwd);
    if (root && (input.worktree === true || input.options?.isolation === "worktree")) {
      return { block: true, reason: `a git worktree bypasses jj here: pass cwd "${AUTO}" for a jj workspace` };
    }
    if (input.cwd !== AUTO) return;
    if (input.action) return { block: true, reason: `cwd "${AUTO}" makes a workspace for a launch, not for action "${input.action}"` };
    if (!root) return { block: true, reason: `cwd "${AUTO}" outside a repo with .claude/hooks/${CREATE}` };
    try {
      input.cwd = await hook(root, CREATE, {
        name: AGENT_DIR + randomUUID().slice(0, 8),
        session_id: ctx.sessionManager.getSessionId(),
      });
      made.set(event.toolCallId, input.cwd);
    } catch (error) {
      // A spawn that got no workspace would edit the main tree instead of its own.
      return { block: true, reason: `worktree-create failed: ${error instanceof Error ? error.message : String(error)}` };
    }
  });

  pi.on("tool_result", async (event) => {
    if (event.toolName !== SUBAGENT) return;
    const workspace = made.get(event.toolCallId);
    if (workspace === undefined) return;
    made.delete(event.toolCallId);
    const details = event.details as SubagentDetails | undefined;
    // A background run returned at its launch; its stop is the completion event below. A
    // foreground child detached mid-run has no single completion: the session's sweep takes it.
    if (details?.asyncId) {
      running.set(details.asyncId, workspace);
      return;
    }
    if (details?.results?.some((result) => result.detached)) return;
    await stop(workspace);
  });

  pi.events.on(ASYNC_COMPLETE, (data) => {
    const runId = (data as { runId?: unknown } | undefined)?.runId;
    if (typeof runId !== "string") return;
    const workspace = running.get(runId);
    if (workspace === undefined) return;
    running.delete(runId);
    stop(workspace).catch((error) => console.error(`${STOP} failed for ${workspace}:`, error));
  });

  pi.on("session_shutdown", async (_event, ctx) => {
    if (!ctx.cwd.includes(MARKER)) return;
    // The workspace's own session ending: a pi started inside one, done with it.
    await stop(ctx.cwd);
  });
}
