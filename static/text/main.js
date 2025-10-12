const text_decoder = new TextDecoder();
const text_encoder = new TextEncoder();

const wasm_exports = (await WebAssembly.instantiateStreaming(fetch("../text.wasm"), {
  env: {
    clearOutput: () => {
      output.textContent = "";
    },
    addOutput: (ptr, len) => {
      const asdf = getString(ptr, len);
      output.textContent += asdf;
    },
  },
})).instance.exports;


function wasmMem() {
  // TODO: keep a permanent variable and only change it if '.detached' is true
  return new Uint8Array(wasm_exports.memory.buffer);
}

function getString(ptr, len) {
  return text_decoder.decode(
    wasmMem().subarray(ptr, ptr + len),
  );
}


function computeOutput(input) {
  // https://developer.mozilla.org/en-US/docs/Web/API/TextEncoder/encodeInto
  // the output space is never greater than s.length * 3 bytes
  const required_size = input.length * 3;
  const dst_ptr = wasm_exports.allocInput(required_size);
  const dst_len = text_encoder.encodeInto(input, wasmMem().subarray(dst_ptr)).written;
  // internall calls setOutput
  wasm_exports.setInput(dst_ptr, dst_len);
}

window.computeOutput = computeOutput;
runCode();