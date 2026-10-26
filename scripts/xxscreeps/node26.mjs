// The Node 26 the engine runs on, resolved through nix once: a parent passes it to its
// soaks as FABOT_NODE26, so concurrent soaks do not queue on nix's flake lock.
import { spawnSync } from "node:child_process";
import { existsSync } from "node:fs";
import { dirname } from "node:path";

export function node26() {
  if (process.env.FABOT_NODE26) return process.env.FABOT_NODE26;
  const res = spawnSync("nix", ["build", "--no-link", "--print-out-paths", "nixpkgs#nodejs_26"], { encoding: "utf8", stdio: ["ignore", "pipe", "inherit"] });
  const bin = `${res.stdout?.trim().split("\n")[0]}/bin/node`;
  if (res.status !== 0 || !existsSync(bin)) throw new Error(`nix: no nodejs_26 (exit ${res.status})`);
  return (process.env.FABOT_NODE26 = bin);
}

// The environment a Node 26 child runs in: its `node` first on PATH, as under `nix shell`.
export const node26Env = (env = process.env) => ({ ...env, FABOT_NODE26: node26(), PATH: `${dirname(node26())}:${env.PATH}` });
