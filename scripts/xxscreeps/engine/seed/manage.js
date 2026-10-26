// xxscreeps' `manage`, its Math.random seeded by the command: `bot add` draws
// the user's id, and its intents are applied in user-id order (`processor.js`),
// so with ids at random two seats' moves part from one run to the next.
import { draw, sequence } from "./random.js";

const s = sequence(process.argv.slice(2).join(" "));
Math.random = () => draw(s);
// The query is batch.mjs's: a second command in its process runs manage again.
await import(import.meta.resolve("xxscreeps/scripts/manage.js") + new URL(import.meta.url).search);
