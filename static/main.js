import { keys } from "./keycodes.js";

console.log(keys);

const container = document.querySelector("#canvas_container");
const canvas = document.querySelector("#ctx_canvas");
const ctx = canvas.getContext("2d");

canvas.width = canvas.clientWidth;
canvas.height = canvas.clientHeight;

const asdf = await WebAssembly.instantiateStreaming(fetch("main.wasm"), {
  env: {
    logInt: (arg) => console.log(arg),
    logFloat: (arg) => console.log(arg),

    beginPath: () => ctx.beginPath(),
    moveTo: (x, y) => ctx.moveTo(x, y),
    lineTo: (x, y) => ctx.lineTo(x, y),
    closePath: () => ctx.closePath(),
    fill: () => ctx.fill(),
    stroke: () => ctx.stroke(),
    setLineWidth: (w) => (ctx.lineWidth = w),
    setFillColor: (r, g, b) => (ctx.fillStyle = rgbToHex(r, g, b)),
    setStrokeColor: (r, g, b) => (ctx.strokeStyle = rgbToHex(r, g, b)),
    setGlobalAlpha: (a) => (ctx.globalAlpha = a),
    fillRect: (x, y, w, h) => ctx.fillRect(x, y, w, h),
    getWidth: () => canvas.width,
    getHeight: () => canvas.height,
  },
});

const wasm_exports = asdf.instance.exports;
const wasm_memory = new Uint8Array(wasm_exports.memory.buffer);

document.addEventListener("resize", (_) => {
  if (
    canvas.width !== canvas.clientWidth ||
    canvas.height !== canvas.clientHeight
  ) {
    canvas.width = canvas.clientWidth;
    canvas.height = canvas.clientHeight;
  }
});

let last_timestamp_millis = 0;
function every_frame(cur_timestamp_millis) {
  const delta_seconds = (cur_timestamp_millis - last_timestamp_millis) / 1000;
  last_timestamp_millis = cur_timestamp_millis;

  ctx.clearRect(0, 0, canvas.width, canvas.height);

  wasm_exports.frame(delta_seconds);
  wasm_exports.draw();

  requestAnimationFrame(every_frame);
}

requestAnimationFrame(every_frame);

document.addEventListener("keydown", (ev) => {
  if (ev.repeat) return;
  const key_num = keys[ev.code];
  if (key_num !== undefined) {
    wasm_exports.keydown(key_num);
  }
});

function rgbToHex(r, g, b) {
  return (
    "#" +
    [r, g, b]
      .map((num) => {
        const hex = num.toString(16);
        return hex.length === 1 ? "0" + hex : hex;
      })
      .join("")
  );
}
