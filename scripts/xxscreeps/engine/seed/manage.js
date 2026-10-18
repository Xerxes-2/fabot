// xxscreeps' `manage`, its Math.random seeded by the command: `bot add` draws
// the user's id, and its intents are applied in user-id order (`processor.js`),
// so with ids at random two seats' moves part from one run to the next.
import { draw, sequence } from "./random.js";

const s = sequence(process.argv.slice(2).join(" "));
Math.random = () => draw(s);
await import("xxscreeps/scripts/manage.js");
