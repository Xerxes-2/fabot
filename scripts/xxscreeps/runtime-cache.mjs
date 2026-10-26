// Patches the installed xxscreeps so a runner reuses the sandbox runtime it bundled before.
// Upstream, every runner start runs webpack, babel and terser over the runtime (seconds of
// CPU per soak), and writes the bundle to one shared path in dist/, which concurrent soaks
// with different mod sets race on. Patched, the bundle is kept under dist/runtime-cache/,
// keyed by its inputs (Node's version and the mods' virtual modules, which name only
// xxscreeps files: our mods provide none of constants, game or schema), and a miss
// bundles into a directory of its own. Idempotent; a reinstall drops patch and cache.
import { readFileSync, renameSync, writeFileSync } from "node:fs";
import { join } from "node:path";

const MARK = "// fabot: runtime-cache";

const edits = {
  "driver/sandbox/isolated/index.js": [
    [
      "import ivm from 'isolated-vm';\n",
      `import ivm from 'isolated-vm';\n${MARK}\nimport { createHash } from 'node:crypto';\nimport * as fs from 'node:fs/promises';\n`,
    ],
    [
      "const getRuntimeSource = runOnce(async () => {\n    const runtime",
      `const getRuntimeSource = runOnce(async () => {
    const inputs = [process.version, await makeModSourceText(mods, 'constants'), await makeModSourceText(mods, 'game'), await makeModSourceText(mods, 'schema'), makePackagesModule()];
    const key = createHash('sha256').update(JSON.stringify(inputs)).digest('hex').slice(0, 16);
    const dir = new URL('../../../runtime-cache/', import.meta.url);
    const file = new URL(\`\${key}.json\`, dir);
    try {
        return JSON.parse(await fs.readFile(file, 'utf8'));
    } catch {}
    const built = await compileRuntime();
    await fs.mkdir(dir, { recursive: true });
    const tmp = new URL(\`\${key}.\${process.pid}.\${Math.random()}.tmp\`, dir);
    await fs.writeFile(tmp, JSON.stringify(built));
    await fs.rename(tmp, file);
    return built;
});
const compileRuntime = runOnce(async () => {
    const runtime`,
    ],
  ],
  "driver/webpack.js": [
    [
      "import Webpack from 'webpack';\n",
      `import Webpack from 'webpack';\n${MARK}\nimport { threadId } from 'node:worker_threads';\n`,
    ],
    [
      "    const output = new URL(`${baseName}.webpack.js`, import.meta.url);",
      "    const outDir = new URL(`webpack-${process.pid}-${threadId}/`, import.meta.url);\n    const output = new URL(`${baseName}.webpack.js`, outDir);",
    ],
    ["                path: fileURLToPath(new URL('.', import.meta.url)),", "                path: fileURLToPath(outDir),"],
    [
      "    return { source, map };\n}",
      "    await fs.rm(outDir, { recursive: true, force: true });\n    return { source, map };\n}",
    ],
  ],
};

export function patchRuntimeCache(xxscreepsDir) {
  for (const [rel, pairs] of Object.entries(edits)) {
    const path = join(xxscreepsDir, "dist", rel);
    let text = readFileSync(path, "utf8");
    if (text.includes(MARK)) continue;
    for (const [from, to] of pairs) {
      if (text.split(from).length !== 2) throw new Error(`runtime-cache: ${rel} no longer has the text the patch replaces; xxscreeps changed`);
      text = text.replace(from, () => to);
    }
    const tmp = `${path}.${process.pid}.tmp`;
    writeFileSync(tmp, text);
    renameSync(tmp, path);
  }
}
