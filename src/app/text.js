// The engine's sandbox has no TextDecoder or TextEncoder; wasm-bindgen's glue
// crosses every string with them.
if (typeof TextDecoder === "undefined") {
  globalThis.TextDecoder = function TextDecoder() {};
  globalThis.TextDecoder.prototype.decode = function (bytes) {
    if (!bytes) return "";
    let out = "";
    for (let i = 0; i < bytes.length; ) {
      let end = i;
      // An argument list stays far under the engine's stack limit at 8k.
      while (end < bytes.length && end - i < 8192 && bytes[end] < 0x80) end++;
      if (end > i) {
        out += String.fromCharCode.apply(null, bytes.subarray(i, end));
        i = end;
        continue;
      }
      const c = bytes[i++];
      if (c < 0xe0) out += String.fromCharCode(((c & 0x1f) << 6) | (bytes[i++] & 0x3f));
      else if (c < 0xf0) out += String.fromCharCode(((c & 0xf) << 12) | ((bytes[i++] & 0x3f) << 6) | (bytes[i++] & 0x3f));
      else {
        const cp = (((c & 7) << 18) | ((bytes[i++] & 0x3f) << 12) | ((bytes[i++] & 0x3f) << 6) | (bytes[i++] & 0x3f)) - 0x10000;
        out += String.fromCharCode(0xd800 + (cp >> 10), 0xdc00 + (cp & 0x3ff));
      }
    }
    return out;
  };
}
if (typeof TextEncoder === "undefined") {
  globalThis.TextEncoder = function TextEncoder() {};
  globalThis.TextEncoder.prototype.encodeInto = function (text, view) {
    let read = 0;
    let written = 0;
    while (read < text.length) {
      let c = text.charCodeAt(read);
      let width = 1;
      if (c >= 0xd800 && c <= 0xdfff) {
        const low = text.charCodeAt(read + 1);
        if (c <= 0xdbff && low >= 0xdc00 && low <= 0xdfff) {
          c = ((c - 0xd800) << 10) + (low - 0xdc00) + 0x10000;
          width = 2;
        } else c = 0xfffd; // as the native encoder does: a lone surrogate is no valid `str` for Rust
      }
      if (c < 0x80) {
        if (written >= view.length) break;
        view[written++] = c;
      } else if (c < 0x800) {
        if (written + 2 > view.length) break;
        view[written++] = 0xc0 | (c >> 6);
        view[written++] = 0x80 | (c & 0x3f);
      } else if (c < 0x10000) {
        if (written + 3 > view.length) break;
        view[written++] = 0xe0 | (c >> 12);
        view[written++] = 0x80 | ((c >> 6) & 0x3f);
        view[written++] = 0x80 | (c & 0x3f);
      } else {
        if (written + 4 > view.length) break;
        view[written++] = 0xf0 | (c >> 18);
        view[written++] = 0x80 | ((c >> 12) & 0x3f);
        view[written++] = 0x80 | ((c >> 6) & 0x3f);
        view[written++] = 0x80 | (c & 0x3f);
      }
      read += width;
    }
    return { read, written };
  };
  globalThis.TextEncoder.prototype.encode = function (text = "") {
    const out = new Uint8Array(text.length * 3);
    return out.slice(0, this.encodeInto(text, out).written);
  };
}
