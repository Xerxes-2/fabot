// The live switches in effect, one line for `observe health` and `observe
// cpu` (#575): the light-tick cadence and the fight-only switch, each with
// where it comes from — the tuned value, the bucket's rule (the cadence
// alone) or a live override in Memory. An override never expires, so a
// standing one is flagged with the command that clears it.
//
// Read off `Memory.fabot.observe.switches` (`ObserveMemory.saveSwitches`).
// A bundle before it writes none; then only the override leaves themselves
// (`observe.cadence`, `observe.fightOnly`) are named.

// Each switch's key in Memory and its `observe` subcommand.
const SWITCHES = [
  ["cadence", "cadence"],
  ["fightOnly", "fight-only"],
];

// An override leaf the bot reads as set (`ObserveMemory.loadCadence` and its
// sibling): a cleared one is null.
const SET = {
  cadence: (value) => typeof value === "number" && Number.isInteger(value),
  fightOnly: (value) => typeof value === "boolean",
};

const shown = (value) => (typeof value === "boolean" ? (value ? "on" : "off") : `${value}`);

/// The line for the `observe` subtree as read off Memory.
export function switchesLine(observe) {
  const switches = observe?.switches;
  const written = switches != null && typeof switches === "object";
  const standing = SWITCHES.filter(([key]) =>
    written ? switches[`${key}From`] === "override" : SET[key](observe?.[key]),
  );
  const line = written
    ? `switches ${SWITCHES.map(([key, name]) => `${name} ${shown(switches[key])} (${switches[`${key}From`]})`).join(" · ")}`
    : "switches not written (the deployed bundle predates them)";
  if (standing.length === 0) return line;
  const values = written ? switches : observe;
  return (
    `${line}  ! override standing: ` +
    standing.map(([key, name]) => `${name} ${shown(values[key])}`).join(", ") +
    ` (${standing.map(([, name]) => `observe ${name} clear`).join(", ")})`
  );
}
