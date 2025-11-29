// Nova WASM Runtime
// Provides the runtime imports for Nova-compiled WebAssembly modules

class NovaRuntime {
  constructor() {
    this.memory = null;
    this.heapPtr = 1024;
    this.strings = new Map();  // heap ptr -> string
    this.arrays = new Map();   // heap ptr -> array
    this.closures = new Map(); // heap ptr -> {func, env}
    this.nextId = 1;
  }

  // Tag constants
  static TAG_INT = 0;
  static TAG_BOOL = 1;
  static TAG_HEAP = 2;
  static TAG_CTOR = 3;

  // Value encoding/decoding
  makeInt(n) {
    return n << 2;
  }

  unboxInt(val) {
    return val >> 2;
  }

  makeBool(b) {
    return (b ? 4 : 0) | 1;
  }

  unboxBool(val) {
    return (val >> 2) !== 0;
  }

  makeHeapPtr(addr) {
    return (addr << 2) | 2;
  }

  unboxHeapPtr(val) {
    return val >>> 2;
  }

  makeCtor(tag, arity) {
    return ((arity << 16) | (tag << 2) | 3);
  }

  getCtorTag(val) {
    return (val >>> 2) & 0x3fff;
  }

  getTag(val) {
    return val & 3;
  }

  isCtor(val) {
    return this.getTag(val) === 3;
  }

  // Runtime imports for WASM
  getImports() {
    const self = this;
    return {
      // Stub modules namespace for standalone loading
      modules: new Proxy({}, {
        get: (target, prop) => {
          // Return a stub function
          return () => 0;
        }
      }),
      runtime: {
        alloc: (size) => {
          const ptr = this.heapPtr;
          this.heapPtr += size;
          return ptr;
        },

        make_string: (ptr, len) => {
          // Read string from WASM memory
          const bytes = new Uint8Array(this.memory.buffer, ptr, len);
          const str = new TextDecoder().decode(bytes);
          const id = this.nextId++;
          this.strings.set(id, str);
          return this.makeHeapPtr(id);
        },

        string_append: (a, b) => {
          const strA = this.getString(a);
          const strB = this.getString(b);
          const result = strA + strB;
          const id = this.nextId++;
          this.strings.set(id, result);
          return this.makeHeapPtr(id);
        },

        string_eq: (a, b) => {
          const strA = this.getString(a);
          const strB = this.getString(b);
          return this.makeBool(strA === strB);
        },

        // Generic equality that handles strings properly
        generic_eq: (a, b) => {
          // If both are heap pointers and are strings, compare content
          const tagA = this.getTag(a);
          const tagB = this.getTag(b);
          if (tagA === NovaRuntime.TAG_HEAP && tagB === NovaRuntime.TAG_HEAP) {
            const idA = this.unboxHeapPtr(a);
            const idB = this.unboxHeapPtr(b);
            // Check if both are strings
            const strA = this.strings.get(idA);
            const strB = this.strings.get(idB);
            if (strA !== undefined && strB !== undefined) {
              // Both are strings - compare content
              // console.log(`  string compare: "${strA}" === "${strB}" = ${strA === strB}`);
              return this.makeBool(strA === strB);
            }
          }
          // Default to value comparison for integers, bools, ctors
          // console.log(`  value compare: ${a} === ${b} = ${a === b}`);
          return this.makeBool(a === b);
        },

        generic_ne: (a, b) => {
          const tagA = this.getTag(a);
          const tagB = this.getTag(b);
          if (tagA === NovaRuntime.TAG_HEAP && tagB === NovaRuntime.TAG_HEAP) {
            const idA = this.unboxHeapPtr(a);
            const idB = this.unboxHeapPtr(b);
            const strA = this.strings.get(idA);
            const strB = this.strings.get(idB);
            if (strA !== undefined && strB !== undefined) {
              return this.makeBool(strA !== strB);
            }
          }
          return this.makeBool(a !== b);
        },

        make_array: (capacity) => {
          const id = this.nextId++;
          this.arrays.set(id, []);
          return this.makeHeapPtr(id);
        },

        array_push: (arrVal, elem) => {
          const id = this.unboxHeapPtr(arrVal);
          const arr = this.arrays.get(id) || [];
          arr.push(elem);
          this.arrays.set(id, arr);
          return arrVal;
        },

        array_get: (arrVal, idx) => {
          const id = this.unboxHeapPtr(arrVal);
          const arr = this.arrays.get(id) || [];
          const i = this.unboxInt(idx);
          return arr[i] || 0;
        },

        array_length: (arrVal) => {
          const id = this.unboxHeapPtr(arrVal);
          const arr = this.arrays.get(id) || [];
          return this.makeInt(arr.length);
        },

        print: (val) => {
          console.log(this.valueToString(val));
        },

        make_closure: (funcIdx, envPtr) => {
          const id = this.nextId++;
          this.closures.set(id, { funcIdx, envPtr });
          return this.makeHeapPtr(id);
        },

        apply_closure: (closureVal, arg) => {
          // Check if it's a heap pointer to a closure
          const tag = this.getTag(closureVal);
          if (tag === NovaRuntime.TAG_HEAP) {
            const id = this.unboxHeapPtr(closureVal);
            const closure = this.closures.get(id);
            if (closure && closure.func) {
              // This is a JavaScript function closure from stdlib
              const result = closure.func(arg);
              return result;
            }
            if (closure && closure.funcIdx !== undefined) {
              // This is a WASM function closure - would need indirect call
              console.warn('WASM closure apply not yet supported');
              return arg;
            }
          }
          // If closureVal is itself a function (direct JS function), call it
          if (typeof closureVal === 'function') {
            return closureVal(arg);
          }
          // Fallback: return arg
          console.warn('apply_closure: unknown closure type', closureVal);
          return arg;
        },

        // Get the environment pointer from a closure
        closure_get_env: (closureVal) => {
          const tag = this.getTag(closureVal);
          if (tag === NovaRuntime.TAG_HEAP) {
            const id = this.unboxHeapPtr(closureVal);
            const closure = this.closures.get(id);
            if (closure && closure.envPtr !== undefined) {
              return closure.envPtr;
            }
          }
          return 0;
        },

        // Tuple operations
        alloc_tuple: (size) => {
          const id = this.nextId++;
          const tuple = new Array(size).fill(0);
          this.arrays.set(id, tuple);
          return this.makeHeapPtr(id);
        },

        tuple_get: (tupleVal, idx) => {
          const id = this.unboxHeapPtr(tupleVal);
          const tuple = this.arrays.get(id) || [];
          return tuple[idx] || 0;
        },

        tuple_set: (tupleVal, idx, val) => {
          const id = this.unboxHeapPtr(tupleVal);
          const tuple = this.arrays.get(id);
          if (tuple) {
            tuple[idx] = val;
          }
        }
      }
    };
  }

  getString(val) {
    const tag = this.getTag(val);
    if (tag === NovaRuntime.TAG_HEAP) {
      const id = this.unboxHeapPtr(val);
      return this.strings.get(id) || '';
    }
    return '';
  }

  valueToString(val) {
    const tag = this.getTag(val);
    switch (tag) {
      case NovaRuntime.TAG_INT:
        return String(this.unboxInt(val));
      case NovaRuntime.TAG_BOOL:
        return this.unboxBool(val) ? 'true' : 'false';
      case NovaRuntime.TAG_HEAP:
        const id = this.unboxHeapPtr(val);
        if (this.strings.has(id)) {
          return JSON.stringify(this.strings.get(id));
        }
        if (this.arrays.has(id)) {
          const arr = this.arrays.get(id);
          return '[' + arr.map(v => this.valueToString(v)).join(', ') + ']';
        }
        return `<heap:${id}>`;
      case NovaRuntime.TAG_CTOR:
        const ctorTag = this.getCtorTag(val);
        return `<ctor:${ctorTag}>`;
      default:
        return `<unknown:${val}>`;
    }
  }

  setMemory(mem) {
    this.memory = mem;
  }
}

// Load and run a WAT file
async function loadWat(watPath) {
  const fs = require('fs');
  const wabt = await require('wabt')();

  const watSource = fs.readFileSync(watPath, 'utf8');
  const wasmModule = wabt.parseWat(watPath, watSource);
  const { buffer } = wasmModule.toBinary({});

  const runtime = new NovaRuntime();
  const { instance } = await WebAssembly.instantiate(buffer, runtime.getImports());

  if (instance.exports.memory) {
    runtime.setMemory(instance.exports.memory);
  }

  return { instance, runtime };
}

// Test runner
async function runTest(watPath, testName, func, args, expected) {
  try {
    const { instance, runtime } = await loadWat(watPath);

    if (!instance.exports[func]) {
      console.log(`  SKIP: ${testName} (function '${func}' not exported)`);
      return;
    }

    const result = instance.exports[func](...args);
    const resultStr = runtime.valueToString(result);

    if (expected !== undefined) {
      if (resultStr === String(expected)) {
        console.log(`  PASS: ${testName} = ${resultStr}`);
      } else {
        console.log(`  FAIL: ${testName} = ${resultStr} (expected ${expected})`);
      }
    } else {
      console.log(`  OK: ${testName} = ${resultStr}`);
    }
  } catch (err) {
    console.log(`  ERROR: ${testName} - ${err.message}`);
  }
}

module.exports = { NovaRuntime, loadWat, runTest };

// CLI test
if (require.main === module) {
  const watPath = process.argv[2];
  if (!watPath) {
    console.log('Usage: node wasm-runtime.js <file.wat> [function] [args...]');
    process.exit(1);
  }

  const func = process.argv[3];
  const args = process.argv.slice(4).map(a => {
    if (a === 'true') return 1;
    if (a === 'false') return 0;
    return parseInt(a, 10) << 2; // Make tagged int
  });

  loadWat(watPath).then(({ instance, runtime }) => {
    if (func) {
      if (instance.exports[func]) {
        const result = instance.exports[func](...args);
        console.log(`${func}(${args.join(', ')}) = ${runtime.valueToString(result)}`);
      } else {
        console.log(`Function '${func}' not found. Available:`);
        console.log(Object.keys(instance.exports).filter(k => typeof instance.exports[k] === 'function'));
      }
    } else {
      console.log('Exported functions:');
      const funcs = Object.keys(instance.exports).filter(k => typeof instance.exports[k] === 'function');
      funcs.forEach(f => console.log(`  - ${f}`));
    }
  }).catch(err => {
    console.error('Error:', err.message);
    process.exit(1);
  });
}
