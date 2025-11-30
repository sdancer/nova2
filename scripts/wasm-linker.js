#!/usr/bin/env node
// Nova WASM Module Linker
// Links multiple WASM modules together with standard library support

const fs = require('fs');
const path = require('path');
const { NovaRuntime, loadWat } = require('./wasm-runtime.js');

class NovaLinker {
  constructor() {
    this.runtime = new NovaRuntime();
    this.modules = new Map();  // module name -> instance
    this.exports = {};         // all exported functions
    this.pendingModules = [];
    this.functionTables = new Map(); // module name -> function table
    this.moduleMemories = new Map(); // module name -> memory (for per-module data segments)
    this.allLambdas = [];      // [{ table, index }] - all lambdas indexed sequentially
  }

  // Apply a closure (either JS or WASM) - used by stdlib and runtime
  applyClosure(closureVal, arg) {
    const rt = this.runtime;
    const tag = rt.getTag(closureVal);
    if (tag === 2) { // TAG_HEAP
      const id = rt.unboxHeapPtr(closureVal);
      const closure = rt.closures.get(id);
      if (closure && closure.func) {
        // JavaScript function closure from stdlib
        return closure.func(arg);
      }
      if (closure && closure.funcIdx !== undefined) {
        // WASM closure - use the appropriate function table
        const table = closure.table || this.currentFunctionTable;
        if (table) {
          const func = table.get(closure.funcIdx);
          if (func) {
            // Lambda functions take (env, arg) parameters
            return func(closure.envPtr, arg);
          }
        }
        // Fallback: try all function tables
        for (const [name, tbl] of this.functionTables) {
          try {
            const func = tbl.get(closure.funcIdx);
            if (func) {
              return func(closure.envPtr, arg);
            }
          } catch (e) {
            // funcIdx out of bounds, try next table
          }
        }
        // If still not found, it may be a stub value
        return arg;
      }
    }
    // Tag 0 (integer) - might be an error or a partial application stub
    if (tag === 0) {
      // Just return arg - this happens when a function returns 0
      return arg;
    }
    // Direct JS function
    if (typeof closureVal === 'function') {
      return closureVal(arg);
    }
    // Constructor values (tag 3) - just return them
    if (tag === 3) {
      return closureVal;
    }
    // Fallback - don't warn to reduce noise
    return arg;
  }

  // Helper to create a curried function from a multi-arity function
  _makeCurried(func, arity) {
    const rt = this.runtime;
    const self = this;

    // Create a curried version that collects args
    const curried = (collectedArgs) => (arg) => {
      const newArgs = [...collectedArgs, arg];
      if (newArgs.length >= arity) {
        // All args collected, call the function
        return func(...newArgs);
      } else {
        // Need more args, return another closure
        const id = rt.nextId++;
        rt.closures.set(id, {
          func: curried(newArgs),
          arity: 1,
          args: []
        });
        return rt.makeHeapPtr(id);
      }
    };

    return curried([]);
  }

  // Create standard library implementations
  getStdLib() {
    const rt = this.runtime;
    const self = this;

    // Curried function helper - creates a closure-like value
    const curry = (f, arity) => {
      const id = rt.nextId++;
      rt.closures.set(id, { func: f, arity, args: [] });
      return rt.makeHeapPtr(id);
    };

    // Make a Just value (tuple [ctor(1), value])
    const makeJust = (value) => {
      const id = rt.nextId++;
      rt.arrays.set(id, [rt.makeCtor(1, 1), value]);
      return rt.makeHeapPtr(id);
    };

    return {
      // Expose curry if needed
      curry,

      // Array functions
      Array_length: () => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        return rt.makeInt(a.length);
      }, 1),

      Array_null: () => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        return rt.makeBool(a.length === 0);
      }, 1),

      Array_head: () => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        if (a.length === 0) return rt.makeCtor(0, 0); // Nothing
        return makeJust(a[0]);
      }, 1),

      Array_tail: () => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        const newId = rt.nextId++;
        rt.arrays.set(newId, a.slice(1));
        return rt.makeHeapPtr(newId);
      }, 1),

      Array_cons: () => curry((x) => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        const newId = rt.nextId++;
        rt.arrays.set(newId, [x, ...a]);
        return rt.makeHeapPtr(newId);
      }, 1), 2),

      Array_snoc: () => curry((arr) => curry((x) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        const newId = rt.nextId++;
        rt.arrays.set(newId, [...a, x]);
        return rt.makeHeapPtr(newId);
      }, 1), 2),

      Array_reverse: () => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        const newId = rt.nextId++;
        rt.arrays.set(newId, [...a].reverse());
        return rt.makeHeapPtr(newId);
      }, 1),

      Array_elem: () => curry((x) => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        // Need to compare values properly - strings need content comparison
        const xTag = rt.getTag(x);
        if (xTag === 2) { // TAG_HEAP - might be a string
          const xId = rt.unboxHeapPtr(x);
          const xStr = rt.strings.get(xId);
          if (xStr !== undefined) {
            // x is a string - compare by content
            for (const elem of a) {
              if (rt.getTag(elem) === 2) {
                const elemId = rt.unboxHeapPtr(elem);
                const elemStr = rt.strings.get(elemId);
                if (elemStr !== undefined && elemStr === xStr) {
                  return rt.makeBool(true);
                }
              }
            }
            return rt.makeBool(false);
          }
        }
        // Fallback to reference equality for non-strings
        return rt.makeBool(a.includes(x));
      }, 1), 2),

      Array_take: () => curry((n) => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        const count = rt.unboxInt(n);
        const newId = rt.nextId++;
        rt.arrays.set(newId, a.slice(0, count));
        return rt.makeHeapPtr(newId);
      }, 1), 2),

      Array_index: () => curry((arr) => curry((idx) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        const i = rt.unboxInt(idx);
        if (i >= 0 && i < a.length) {
          return makeJust(a[i]);
        }
        return rt.makeCtor(0, 0); // Nothing
      }, 1), 2),

      Array_drop: () => curry((n) => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        const count = rt.unboxInt(n);
        const newId = rt.nextId++;
        rt.arrays.set(newId, a.slice(count));
        return rt.makeHeapPtr(newId);
      }, 1), 2),

      Array_range: () => curry((start) => curry((end) => {
        const s = rt.unboxInt(start);
        const e = rt.unboxInt(end);
        const arr = [];
        for (let i = s; i <= e; i++) {
          arr.push(rt.makeInt(i));
        }
        const newId = rt.nextId++;
        rt.arrays.set(newId, arr);
        return rt.makeHeapPtr(newId);
      }, 1), 2),

      Array_replicate: () => curry((n) => curry((x) => {
        const count = rt.unboxInt(n);
        const arr = new Array(count).fill(x);
        const newId = rt.nextId++;
        rt.arrays.set(newId, arr);
        return rt.makeHeapPtr(newId);
      }, 1), 2),

      Array_find: () => curry((pred) => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        for (let i = 0; i < a.length; i++) {
          // Apply predicate to element using linker's applyClosure
          const result = self.applyClosure(pred, a[i]);
          if (rt.unboxBool(result)) {
            return makeJust(a[i]);
          }
        }
        return rt.makeCtor(0, 0); // Nothing
      }, 1), 2),

      // Map functions (use JS Map for simplicity)
      Map_empty: () => {
        const id = rt.nextId++;
        rt.arrays.set(id, new Map());  // Reuse arrays storage for maps
        return rt.makeHeapPtr(id);
      },

      Map_singleton: () => curry((k) => curry((v) => {
        const id = rt.nextId++;
        const m = new Map();
        m.set(k, v);
        rt.arrays.set(id, m);
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Map_insert: () => curry((k) => curry((v) => curry((mapVal) => {
        const id = rt.unboxHeapPtr(mapVal);
        const m = rt.arrays.get(id) || new Map();
        const newMap = new Map(m);
        newMap.set(k, v);
        const newId = rt.nextId++;
        rt.arrays.set(newId, newMap);
        return rt.makeHeapPtr(newId);
      }, 1), 1), 3),

      Map_lookup: () => curry((k) => curry((mapVal) => {
        const id = rt.unboxHeapPtr(mapVal);
        const m = rt.arrays.get(id);
        if (m && m.has && m.has(k)) {
          return makeJust(m.get(k));
        }
        return rt.makeCtor(0, 0); // Nothing
      }, 1), 2),

      Map_union: () => curry((m1) => curry((m2) => {
        const id1 = rt.unboxHeapPtr(m1);
        const id2 = rt.unboxHeapPtr(m2);
        const map1 = rt.arrays.get(id1) || new Map();
        const map2 = rt.arrays.get(id2) || new Map();
        const newMap = new Map([...map2, ...map1]); // m1 takes precedence
        const newId = rt.nextId++;
        rt.arrays.set(newId, newMap);
        return rt.makeHeapPtr(newId);
      }, 1), 2),

      // Set functions (use JS Set)
      Set_empty: () => {
        const id = rt.nextId++;
        rt.arrays.set(id, new Set());
        return rt.makeHeapPtr(id);
      },

      Set_singleton: () => curry((x) => {
        const id = rt.nextId++;
        rt.arrays.set(id, new Set([x]));
        return rt.makeHeapPtr(id);
      }, 1),

      Set_insert: () => curry((x) => curry((setVal) => {
        const id = rt.unboxHeapPtr(setVal);
        const s = rt.arrays.get(id) || new Set();
        const newSet = new Set(s);
        newSet.add(x);
        const newId = rt.nextId++;
        rt.arrays.set(newId, newSet);
        return rt.makeHeapPtr(newId);
      }, 1), 2),

      Set_member: () => curry((x) => curry((setVal) => {
        const id = rt.unboxHeapPtr(setVal);
        const s = rt.arrays.get(id);
        return rt.makeBool(s && s.has && s.has(x));
      }, 1), 2),

      Set_union: () => curry((s1) => curry((s2) => {
        const id1 = rt.unboxHeapPtr(s1);
        const id2 = rt.unboxHeapPtr(s2);
        const set1 = rt.arrays.get(id1) || new Set();
        const set2 = rt.arrays.get(id2) || new Set();
        const newSet = new Set([...set1, ...set2]);
        const newId = rt.nextId++;
        rt.arrays.set(newId, newSet);
        return rt.makeHeapPtr(newId);
      }, 1), 2),

      Set_size: () => curry((setVal) => {
        const id = rt.unboxHeapPtr(setVal);
        const s = rt.arrays.get(id);
        return rt.makeInt(s && s.size ? s.size : 0);
      }, 1),

      // String functions
      String_length: () => curry((strVal) => {
        const str = rt.getString(strVal);
        return rt.makeInt(str.length);
      }, 1),

      String_take: () => curry((n) => curry((strVal) => {
        const str = rt.getString(strVal);
        const count = rt.unboxInt(n);
        const id = rt.nextId++;
        rt.strings.set(id, str.slice(0, count));
        return rt.makeHeapPtr(id);
      }, 1), 2),

      String_drop: () => curry((n) => curry((strVal) => {
        const str = rt.getString(strVal);
        const count = rt.unboxInt(n);
        const id = rt.nextId++;
        rt.strings.set(id, str.slice(count));
        return rt.makeHeapPtr(id);
      }, 1), 2),

      String_singleton: () => curry((charVal) => {
        const code = rt.unboxInt(charVal);
        const id = rt.nextId++;
        rt.strings.set(id, String.fromCharCode(code));
        return rt.makeHeapPtr(id);
      }, 1),

      String_joinWith: () => curry((sep) => curry((arr) => {
        const sepStr = rt.getString(sep);
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        const strs = a.map(v => rt.getString(v));
        const newId = rt.nextId++;
        rt.strings.set(newId, strs.join(sepStr));
        return rt.makeHeapPtr(newId);
      }, 1), 2),

      String_split: () => curry((patVal) => curry((strVal) => {
        const pat = rt.getString(patVal);
        const str = rt.getString(strVal);
        const parts = str.split(pat);
        const arrId = rt.nextId++;
        const arr = parts.map(s => {
          const id = rt.nextId++;
          rt.strings.set(id, s);
          return rt.makeHeapPtr(id);
        });
        rt.arrays.set(arrId, arr);
        return rt.makeHeapPtr(arrId);
      }, 1), 2),

      String_contains: () => curry((pat) => curry((str) => {
        const patStr = rt.getString(pat);
        const strStr = rt.getString(str);
        return rt.makeBool(strStr.includes(patStr));
      }, 1), 2),

      String_toLower: () => curry((strVal) => {
        const str = rt.getString(strVal);
        const id = rt.nextId++;
        rt.strings.set(id, str.toLowerCase());
        return rt.makeHeapPtr(id);
      }, 1),

      String_Pattern: () => curry((x) => x, 1),  // Pattern is just identity
      String_Replacement: () => curry((x) => x, 1),

      // Character utilities
      CU_charAt: () => curry((idx) => curry((strVal) => {
        const str = rt.getString(strVal);
        const i = rt.unboxInt(idx);
        if (i >= 0 && i < str.length) {
          // Just x = tuple [ctor(1), x]
          const id = rt.nextId++;
          rt.arrays.set(id, [rt.makeCtor(1, 1), rt.makeInt(str.charCodeAt(i))]);
          return rt.makeHeapPtr(id);
        }
        return rt.makeCtor(0, 0); // Nothing
      }, 1), 2),

      CU_singleton: () => curry((charVal) => {
        const code = rt.unboxInt(charVal);
        const id = rt.nextId++;
        rt.strings.set(id, String.fromCharCode(code));
        return rt.makeHeapPtr(id);
      }, 1),

      CU_drop: () => curry((n) => curry((strVal) => {
        const str = rt.getString(strVal);
        const count = rt.unboxInt(n);
        const id = rt.nextId++;
        rt.strings.set(id, str.slice(count));
        return rt.makeHeapPtr(id);
      }, 1), 2),

      CU_toCharArray: () => curry((strVal) => {
        const str = rt.getString(strVal);
        const chars = [];
        for (let i = 0; i < str.length; i++) {
          chars.push(rt.makeInt(str.charCodeAt(i)));
        }
        const id = rt.nextId++;
        rt.arrays.set(id, chars);
        return rt.makeHeapPtr(id);
      }, 1),

      CU_fromCharArray: () => curry((arrVal) => {
        const id = rt.unboxHeapPtr(arrVal);
        const arr = rt.arrays.get(id) || [];
        let str = '';
        for (const charVal of arr) {
          str += String.fromCharCode(rt.unboxInt(charVal));
        }
        const newId = rt.nextId++;
        rt.strings.set(newId, str);
        return rt.makeHeapPtr(newId);
      }, 1),

      SCU_charAt: () => curry((idx) => curry((strVal) => {
        const str = rt.getString(strVal);
        const i = rt.unboxInt(idx);
        if (i >= 0 && i < str.length) {
          return makeJust(rt.makeInt(str.charCodeAt(i)));
        }
        return rt.makeCtor(0, 0);
      }, 1), 2),

      SCU_singleton: () => curry((charVal) => {
        const code = rt.unboxInt(charVal);
        const id = rt.nextId++;
        rt.strings.set(id, String.fromCharCode(code));
        return rt.makeHeapPtr(id);
      }, 1),

      // Number parsing
      Int_fromString: () => curry((strVal) => {
        const str = rt.getString(strVal);
        const n = parseInt(str, 10);
        if (isNaN(n)) return rt.makeCtor(0, 0); // Nothing
        return makeJust(rt.makeInt(n));
      }, 1),

      Number_fromString: () => curry((strVal) => {
        const str = rt.getString(strVal);
        const n = parseFloat(str);
        if (isNaN(n)) return rt.makeCtor(0, 0);
        return makeJust(rt.makeInt(Math.floor(n)));
      }, 1),

      // Array_uncons: returns Nothing for empty, Just (Tuple head tail) for non-empty
      Array_uncons: () => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        if (a.length === 0) return rt.makeCtor(0, 0); // Nothing
        // Just { head: a[0], tail: rest }
        const tailId = rt.nextId++;
        rt.arrays.set(tailId, a.slice(1));
        const recId = rt.nextId++;
        // { head, tail } as tuple-like structure
        rt.arrays.set(recId, [a[0], rt.makeHeapPtr(tailId)]);
        return makeJust(rt.makeHeapPtr(recId));
      }, 1),

      Array_last: () => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        if (a.length === 0) return rt.makeCtor(0, 0);
        return makeJust(a[a.length - 1]);
      }, 1),

      Array_foldl: () => curry((f) => curry((init) => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        let acc = init;
        for (let i = 0; i < a.length; i++) {
          // foldl f acc [x] = f(acc)(x)
          const partial = self.applyClosure(f, acc);
          acc = self.applyClosure(partial, a[i]);
        }
        return acc;
      }, 1), 1), 3),

      Array_foldr: () => curry((f) => curry((init) => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        let acc = init;
        for (let i = a.length - 1; i >= 0; i--) {
          // foldr f acc [x] = f(x)(acc)
          const partial = self.applyClosure(f, a[i]);
          acc = self.applyClosure(partial, acc);
        }
        return acc;
      }, 1), 1), 3),

      Array_filter: () => curry((pred) => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        const result = [];
        for (let i = 0; i < a.length; i++) {
          const keep = self.applyClosure(pred, a[i]);
          if (rt.unboxBool(keep)) {
            result.push(a[i]);
          }
        }
        const newId = rt.nextId++;
        rt.arrays.set(newId, result);
        return rt.makeHeapPtr(newId);
      }, 1), 2),

      Array_mapWithIndex: () => curry((f) => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        const result = [];
        for (let i = 0; i < a.length; i++) {
          const partial = self.applyClosure(f, rt.makeInt(i));
          result.push(self.applyClosure(partial, a[i]));
        }
        const newId = rt.nextId++;
        rt.arrays.set(newId, result);
        return rt.makeHeapPtr(newId);
      }, 1), 2),

      Array_zip: () => curry((arr1) => curry((arr2) => {
        const id1 = rt.unboxHeapPtr(arr1);
        const id2 = rt.unboxHeapPtr(arr2);
        const a1 = rt.arrays.get(id1) || [];
        const a2 = rt.arrays.get(id2) || [];
        const len = Math.min(a1.length, a2.length);
        const result = [];
        for (let i = 0; i < len; i++) {
          // Tuple as array [fst, snd]
          const tupId = rt.nextId++;
          rt.arrays.set(tupId, [a1[i], a2[i]]);
          result.push(rt.makeHeapPtr(tupId));
        }
        const newId = rt.nextId++;
        rt.arrays.set(newId, result);
        return rt.makeHeapPtr(newId);
      }, 1), 2),

      Array_span: () => curry((pred) => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        let splitIdx = 0;
        while (splitIdx < a.length) {
          const keep = self.applyClosure(pred, a[splitIdx]);
          if (!rt.unboxBool(keep)) break;
          splitIdx++;
        }
        const initId = rt.nextId++;
        const restId = rt.nextId++;
        rt.arrays.set(initId, a.slice(0, splitIdx));
        rt.arrays.set(restId, a.slice(splitIdx));
        // Return record-like { init, rest }
        const recId = rt.nextId++;
        rt.arrays.set(recId, [rt.makeHeapPtr(initId), rt.makeHeapPtr(restId)]);
        return rt.makeHeapPtr(recId);
      }, 1), 2),

      Array_mapMaybe: () => curry((f) => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        const result = [];
        for (let i = 0; i < a.length; i++) {
          const maybeVal = self.applyClosure(f, a[i]);
          // Check if Just (tag 1) or Nothing (tag 0)
          const tag = rt.getTag(maybeVal);
          if (tag === 2) { // heap ptr (Just is a tuple)
            const mid = rt.unboxHeapPtr(maybeVal);
            const tup = rt.arrays.get(mid);
            if (tup && rt.getCtorTag(tup[0]) === 1) { // Just
              result.push(tup[1]);
            }
          }
        }
        const newId = rt.nextId++;
        rt.arrays.set(newId, result);
        return rt.makeHeapPtr(newId);
      }, 1), 2),

      String_lastIndexOf: () => curry((pat) => curry((str) => {
        const patStr = rt.getString(pat);
        const strStr = rt.getString(str);
        const idx = strStr.lastIndexOf(patStr);
        if (idx < 0) return rt.makeCtor(0, 0);
        return makeJust(rt.makeInt(idx));
      }, 1), 2),

      Set_toUnfoldable: () => curry((setVal) => {
        const id = rt.unboxHeapPtr(setVal);
        const s = rt.arrays.get(id);
        const result = s ? [...s] : [];
        const newId = rt.nextId++;
        rt.arrays.set(newId, result);
        return rt.makeHeapPtr(newId);
      }, 1),

      Set_difference: () => curry((s1) => curry((s2) => {
        const id1 = rt.unboxHeapPtr(s1);
        const id2 = rt.unboxHeapPtr(s2);
        const set1 = rt.arrays.get(id1) || new Set();
        const set2 = rt.arrays.get(id2) || new Set();
        const newSet = new Set([...set1].filter(x => !set2.has(x)));
        const newId = rt.nextId++;
        rt.arrays.set(newId, newSet);
        return rt.makeHeapPtr(newId);
      }, 1), 2),

      Set_fromFoldable: () => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        const newSet = new Set(a);
        const newId = rt.nextId++;
        rt.arrays.set(newId, newSet);
        return rt.makeHeapPtr(newId);
      }, 1),

      Map_fromFoldable: () => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        const newMap = new Map();
        for (const tupVal of a) {
          const tupId = rt.unboxHeapPtr(tupVal);
          const tup = rt.arrays.get(tupId);
          if (tup && tup.length >= 2) {
            newMap.set(tup[0], tup[1]);
          }
        }
        const newId = rt.nextId++;
        rt.arrays.set(newId, newMap);
        return rt.makeHeapPtr(newId);
      }, 1),

      Map_toUnfoldable: () => curry((mapVal) => {
        const id = rt.unboxHeapPtr(mapVal);
        const m = rt.arrays.get(id) || new Map();
        const result = [];
        for (const [k, v] of m) {
          const tupId = rt.nextId++;
          rt.arrays.set(tupId, [k, v]);
          result.push(rt.makeHeapPtr(tupId));
        }
        const newId = rt.nextId++;
        rt.arrays.set(newId, result);
        return rt.makeHeapPtr(newId);
      }, 1),

      Map_keys: () => curry((mapVal) => {
        const id = rt.unboxHeapPtr(mapVal);
        const m = rt.arrays.get(id) || new Map();
        const keys = m.keys ? [...m.keys()] : [];
        const newId = rt.nextId++;
        rt.arrays.set(newId, keys);
        return rt.makeHeapPtr(newId);
      }, 1),

      // AST Constructors - these create tagged data structures
      // Pattern constructors
      Ast_PatVar: () => curry((name) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(0, 1), name]); // PatVar tag=0
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_PatWildcard: () => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(1, 0)]); // PatWildcard tag=1
        return rt.makeHeapPtr(id);
      },

      Ast_PatLit: () => curry((lit) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(2, 1), lit]); // PatLit tag=2
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_PatCtor: () => curry((name) => curry((pats) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(3, 2), name, pats]); // PatCtor tag=3
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_PatRecord: () => curry((fields) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(4, 1), fields]); // PatRecord tag=4
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_PatList: () => curry((pats) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(5, 1), pats]); // PatList tag=5
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_PatAs: () => curry((name) => curry((pat) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(6, 2), name, pat]); // PatAs tag=6
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_PatParens: () => curry((pat) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(7, 1), pat]); // PatParens tag=7
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_PatTyped: () => curry((pat) => curry((ty) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(8, 2), pat, ty]); // PatTyped tag=8
        return rt.makeHeapPtr(id);
      }, 1), 2),

      // Literal constructors
      Ast_LitInt: () => curry((n) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(0, 1), n]); // LitInt tag=0
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_LitNumber: () => curry((n) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(1, 1), n]); // LitNumber tag=1
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_LitString: () => curry((s) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(2, 1), s]); // LitString tag=2
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_LitChar: () => curry((c) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(3, 1), c]); // LitChar tag=3
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_LitBool: () => curry((b) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(4, 1), b]); // LitBool tag=4
        return rt.makeHeapPtr(id);
      }, 1),

      // Expr constructors
      Ast_ExprVar: () => curry((name) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(0, 1), name]); // ExprVar tag=0
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_ExprLit: () => curry((lit) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(1, 1), lit]); // ExprLit tag=1
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_ExprApp: () => curry((f) => curry((arg) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(2, 2), f, arg]); // ExprApp tag=2
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_ExprLambda: () => curry((pats) => curry((body) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(3, 2), pats, body]); // ExprLambda tag=3
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_ExprLet: () => curry((bindings) => curry((body) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(4, 2), bindings, body]); // ExprLet tag=4
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_ExprIf: () => curry((cond) => curry((then_) => curry((else_) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(5, 3), cond, then_, else_]); // ExprIf tag=5
        return rt.makeHeapPtr(id);
      }, 1), 1), 3),

      Ast_ExprCase: () => curry((expr) => curry((branches) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(6, 2), expr, branches]); // ExprCase tag=6
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_ExprInfix: () => curry((left) => curry((op) => curry((right) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(7, 3), left, op, right]); // ExprInfix tag=7
        return rt.makeHeapPtr(id);
      }, 1), 1), 3),

      Ast_ExprNegate: () => curry((expr) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(8, 1), expr]); // ExprNegate tag=8
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_ExprArray: () => curry((exprs) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(9, 1), exprs]); // ExprArray tag=9
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_ExprRecord: () => curry((fields) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(10, 1), fields]); // ExprRecord tag=10
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_ExprRecordAccess: () => curry((expr) => curry((field) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(11, 2), expr, field]); // ExprRecordAccess tag=11
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_ExprRecordUpdate: () => curry((expr) => curry((updates) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(12, 2), expr, updates]); // ExprRecordUpdate tag=12
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_ExprTyped: () => curry((expr) => curry((ty) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(13, 2), expr, ty]); // ExprTyped tag=13
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_ExprParens: () => curry((expr) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(14, 1), expr]); // ExprParens tag=14
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_ExprDo: () => curry((stmts) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(15, 1), stmts]); // ExprDo tag=15
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_ExprCtor: () => curry((name) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(16, 1), name]); // ExprCtor tag=16
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_ExprOpSection: () => curry((op) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(17, 1), op]); // ExprOpSection tag=17
        return rt.makeHeapPtr(id);
      }, 1),

      // Do statement constructors
      Ast_DoLet: () => curry((bindings) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(0, 1), bindings]); // DoLet tag=0
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_DoBind: () => curry((pat) => curry((expr) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(1, 2), pat, expr]); // DoBind tag=1
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_DoExpr: () => curry((expr) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(2, 1), expr]); // DoExpr tag=2
        return rt.makeHeapPtr(id);
      }, 1),

      // Declaration constructors
      Ast_DeclModule: () => curry((name) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(0, 1), name]); // DeclModule tag=0
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_DeclImport: () => curry((name) => curry((importSpec) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(1, 2), name, importSpec]); // DeclImport tag=1
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_DeclForeignImport: () => curry((name) => curry((ty) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(2, 2), name, ty]); // DeclForeignImport tag=2
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_DeclInfix: () => curry((assoc) => curry((prec) => curry((op) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(3, 3), assoc, prec, op]); // DeclInfix tag=3
        return rt.makeHeapPtr(id);
      }, 1), 1), 3),

      Ast_DeclNewtype: () => curry((name) => curry((params) => curry((ctor) => curry((ty) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(4, 4), name, params, ctor, ty]); // DeclNewtype tag=4
        return rt.makeHeapPtr(id);
      }, 1), 1), 1), 4),

      Ast_DeclDataType: () => curry((name) => curry((params) => curry((ctors) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(5, 3), name, params, ctors]); // DeclDataType tag=5
        return rt.makeHeapPtr(id);
      }, 1), 1), 3),

      Ast_DeclTypeAlias: () => curry((name) => curry((params) => curry((ty) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(6, 3), name, params, ty]); // DeclTypeAlias tag=6
        return rt.makeHeapPtr(id);
      }, 1), 1), 3),

      Ast_DeclTypeClass: () => curry((constraints) => curry((name) => curry((params) => curry((members) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(7, 4), constraints, name, params, members]); // DeclTypeClass tag=7
        return rt.makeHeapPtr(id);
      }, 1), 1), 1), 4),

      Ast_DeclTypeClassInstance: () => curry((constraints) => curry((className) => curry((types) => curry((members) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(8, 4), constraints, className, types, members]); // DeclTypeClassInstance tag=8
        return rt.makeHeapPtr(id);
      }, 1), 1), 1), 4),

      Ast_DeclFunction: () => curry((funcDecl) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(1, 1), funcDecl]); // DeclFunction tag=1, arity=1
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_DeclTypeSig: () => curry((sig) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(10, 1), sig]); // DeclTypeSig tag=10, arity=1
        return rt.makeHeapPtr(id);
      }, 1),

      // Associativity constructors
      Ast_AssocLeft: () => rt.makeCtor(0, 0),
      Ast_AssocRight: () => rt.makeCtor(1, 0),
      Ast_AssocNone: () => rt.makeCtor(2, 0),

      // Import constructors
      Ast_ImportType: () => curry((name) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(0, 1), name]); // ImportType tag=0
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_ImportValue: () => curry((name) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(1, 1), name]); // ImportValue tag=1
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_ImportAll: () => rt.makeCtor(0, 0),

      Ast_ImportSome: () => curry((items) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(1, 1), items]); // ImportSome tag=1
        return rt.makeHeapPtr(id);
      }, 1),

      // Guard constructors
      Ast_GuardExpr: () => curry((expr) => curry((body) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(0, 2), expr, body]); // GuardExpr tag=0
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_GuardPat: () => curry((expr) => curry((pat) => curry((body) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(1, 3), expr, pat, body]); // GuardPat tag=1
        return rt.makeHeapPtr(id);
      }, 1), 1), 3),

      // TypeExpr constructors
      Ast_TyExprVar: () => curry((name) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(0, 1), name]); // TyExprVar tag=0
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_TyExprCon: () => curry((name) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(1, 1), name]); // TyExprCon tag=1
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_TyExprApp: () => curry((f) => curry((arg) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(2, 2), f, arg]); // TyExprApp tag=2
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_TyExprArrow: () => curry((from) => curry((to) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(3, 2), from, to]); // TyExprArrow tag=3
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_TyExprForAll: () => curry((vars) => curry((ty) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(4, 2), vars, ty]); // TyExprForAll tag=4
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_TyExprRecord: () => curry((fields) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(5, 1), fields]); // TyExprRecord tag=5
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_TyExprTuple: () => curry((types) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(6, 1), types]); // TyExprTuple tag=6
        return rt.makeHeapPtr(id);
      }, 1),

      // Additional Pattern constructors
      Ast_PatCon: () => curry((name) => curry((pats) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(3, 2), name, pats]); // PatCon = PatCtor alias
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_PatCons: () => curry((head) => curry((tail) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(9, 2), head, tail]); // PatCons tag=9
        return rt.makeHeapPtr(id);
      }, 1), 2),

      // Additional Expr constructors
      Ast_ExprQualified: () => curry((module) => curry((name) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(18, 2), module, name]); // ExprQualified tag=18
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_ExprBinOp: () => curry((left) => curry((op) => curry((right) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(7, 3), left, op, right]); // ExprBinOp = ExprInfix
        return rt.makeHeapPtr(id);
      }, 1), 1), 3),

      Ast_ExprUnaryOp: () => curry((op) => curry((expr) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(8, 2), op, expr]); // ExprUnaryOp
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_ExprSection: () => curry((op) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(17, 1), op]); // ExprSection = ExprOpSection
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_ExprSectionLeft: () => curry((left) => curry((op) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(19, 2), left, op]); // ExprSectionLeft tag=19
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_ExprSectionRight: () => curry((op) => curry((right) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(20, 2), op, right]); // ExprSectionRight tag=20
        return rt.makeHeapPtr(id);
      }, 1), 2),

      Ast_ExprList: () => curry((exprs) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(9, 1), exprs]); // ExprList = ExprArray
        return rt.makeHeapPtr(id);
      }, 1),

      Ast_ExprTuple: () => curry((exprs) => {
        const id = rt.nextId++;
        rt.arrays.set(id, [rt.makeCtor(21, 1), exprs]); // ExprTuple tag=21
        return rt.makeHeapPtr(id);
      }, 1),

      // Additional utility functions
      Array_dropWhile: () => curry((pred) => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        let dropIdx = 0;
        while (dropIdx < a.length) {
          const keep = self.applyClosure(pred, a[dropIdx]);
          if (!rt.unboxBool(keep)) break;
          dropIdx++;
        }
        const newId = rt.nextId++;
        rt.arrays.set(newId, a.slice(dropIdx));
        return rt.makeHeapPtr(newId);
      }, 1), 2),

      Array_any: () => curry((pred) => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        for (let i = 0; i < a.length; i++) {
          const result = self.applyClosure(pred, a[i]);
          if (rt.unboxBool(result)) return rt.makeBool(true);
        }
        return rt.makeBool(false);
      }, 1), 2),

      Array_all: () => curry((pred) => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        for (let i = 0; i < a.length; i++) {
          const result = self.applyClosure(pred, a[i]);
          if (!rt.unboxBool(result)) return rt.makeBool(false);
        }
        return rt.makeBool(true);
      }, 1), 2),

      Array_concatMap: () => curry((f) => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        const result = [];
        for (let i = 0; i < a.length; i++) {
          const mapped = self.applyClosure(f, a[i]);
          const mappedId = rt.unboxHeapPtr(mapped);
          const mappedArr = rt.arrays.get(mappedId) || [];
          result.push(...mappedArr);
        }
        const newId = rt.nextId++;
        rt.arrays.set(newId, result);
        return rt.makeHeapPtr(newId);
      }, 1), 2),

      Array_partition: () => curry((pred) => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        const yes = [], no = [];
        for (let i = 0; i < a.length; i++) {
          const keep = self.applyClosure(pred, a[i]);
          if (rt.unboxBool(keep)) {
            yes.push(a[i]);
          } else {
            no.push(a[i]);
          }
        }
        const yesId = rt.nextId++;
        const noId = rt.nextId++;
        rt.arrays.set(yesId, yes);
        rt.arrays.set(noId, no);
        const recId = rt.nextId++;
        rt.arrays.set(recId, [rt.makeHeapPtr(yesId), rt.makeHeapPtr(noId)]);
        return rt.makeHeapPtr(recId);
      }, 1), 2),

      Array_init: () => curry((arr) => {
        const id = rt.unboxHeapPtr(arr);
        const a = rt.arrays.get(id) || [];
        if (a.length === 0) return rt.makeCtor(0, 0);
        const newId = rt.nextId++;
        rt.arrays.set(newId, a.slice(0, -1));
        return makeJust(rt.makeHeapPtr(newId));
      }, 1),

      String_replaceAll: () => curry((pat) => curry((rep) => curry((str) => {
        const patStr = rt.getString(pat);
        const repStr = rt.getString(rep);
        const strStr = rt.getString(str);
        const result = strStr.split(patStr).join(repStr);
        const id = rt.nextId++;
        rt.strings.set(id, result);
        return rt.makeHeapPtr(id);
      }, 1), 1), 3),

      String_stripPrefix: () => curry((prefix) => curry((str) => {
        const prefixStr = rt.getString(prefix);
        const strStr = rt.getString(str);
        if (strStr.startsWith(prefixStr)) {
          const id = rt.nextId++;
          rt.strings.set(id, strStr.slice(prefixStr.length));
          return makeJust(rt.makeHeapPtr(id));
        }
        return rt.makeCtor(0, 0);
      }, 1), 2),

      String_toCodePointArray: () => curry((str) => {
        const strStr = rt.getString(str);
        const result = [];
        for (let i = 0; i < strStr.length; i++) {
          result.push(rt.makeInt(strStr.charCodeAt(i)));
        }
        const id = rt.nextId++;
        rt.arrays.set(id, result);
        return rt.makeHeapPtr(id);
      }, 1),

      // Set functions
      Set_mapMaybe: () => curry((f) => curry((setVal) => {
        const id = rt.unboxHeapPtr(setVal);
        const s = rt.arrays.get(id) || new Set();
        const result = new Set();
        for (const x of s) {
          const maybeVal = self.applyClosure(f, x);
          const tag = rt.getTag(maybeVal);
          if (tag === 2) {
            const mid = rt.unboxHeapPtr(maybeVal);
            const tup = rt.arrays.get(mid);
            if (tup && rt.getCtorTag(tup[0]) === 1) {
              result.add(tup[1]);
            }
          }
        }
        const newId = rt.nextId++;
        rt.arrays.set(newId, result);
        return rt.makeHeapPtr(newId);
      }, 1), 2),

      Set_delete: () => curry((x) => curry((setVal) => {
        const id = rt.unboxHeapPtr(setVal);
        const s = rt.arrays.get(id) || new Set();
        const newSet = new Set(s);
        newSet.delete(x);
        const newId = rt.nextId++;
        rt.arrays.set(newId, newSet);
        return rt.makeHeapPtr(newId);
      }, 1), 2),

      Set_findMin: () => curry((setVal) => {
        const id = rt.unboxHeapPtr(setVal);
        const s = rt.arrays.get(id);
        if (!s || s.size === 0) return rt.makeCtor(0, 0);
        const min = Math.min(...[...s].map(x => rt.unboxInt(x)));
        return makeJust(rt.makeInt(min));
      }, 1),

      Set_intersection: () => curry((s1) => curry((s2) => {
        const id1 = rt.unboxHeapPtr(s1);
        const id2 = rt.unboxHeapPtr(s2);
        const set1 = rt.arrays.get(id1) || new Set();
        const set2 = rt.arrays.get(id2) || new Set();
        const newSet = new Set([...set1].filter(x => set2.has(x)));
        const newId = rt.nextId++;
        rt.arrays.set(newId, newSet);
        return rt.makeHeapPtr(newId);
      }, 1), 2),

      Map_delete: () => curry((k) => curry((mapVal) => {
        const id = rt.unboxHeapPtr(mapVal);
        const m = rt.arrays.get(id) || new Map();
        const newMap = new Map(m);
        newMap.delete(k);
        const newId = rt.nextId++;
        rt.arrays.set(newId, newMap);
        return rt.makeHeapPtr(newId);
      }, 1), 2),
    };
  }

  // Get all imports needed
  getImports(moduleName) {
    const rt = this.runtime;
    const stdlib = this.getStdLib();
    const self = this;
    const baseRuntime = rt.getImports().runtime;

    return {
      runtime: {
        ...baseRuntime,
        // Use linker's applyClosure which has access to function table
        apply_closure: (closureVal, arg) => self.applyClosure(closureVal, arg),
        // Override make_closure to track which module's table to use
        make_closure: (funcIdx, envPtr) => {
          const id = rt.nextId++;
          // Store reference to current function table at closure creation time
          rt.closures.set(id, {
            funcIdx,
            envPtr,
            table: self.functionTables.get(moduleName) || self.currentFunctionTable,
            moduleName
          });
          return rt.makeHeapPtr(id);
        },
        // Module-specific make_string that reads from this module's memory
        make_string: (offset, len) => {
          // Get this module's memory (set during loadModule after instantiation)
          // Fall back to runtime's memory if not yet set
          const memory = self.moduleMemories.get(moduleName) || rt.memory;
          if (!memory) {
            const id = rt.nextId++;
            rt.strings.set(id, "");
            return rt.makeHeapPtr(id);
          }
          const bytes = new Uint8Array(memory.buffer, offset, len);
          const str = new TextDecoder().decode(bytes);
          const id = rt.nextId++;
          rt.strings.set(id, str);
          return rt.makeHeapPtr(id);
        }
      },
      modules: new Proxy({}, {
        get: (target, prop) => {
          // Check stdlib first
          if (stdlib[prop]) {
            return stdlib[prop];
          }
          // Check other loaded modules
          for (const [name, instance] of self.modules) {
            if (instance.exports[prop]) {
              const func = instance.exports[prop];
              // Get function arity
              const arity = func.length;
              if (arity === 0) {
                // Zero-arity function - return as-is
                return func;
              } else {
                // Multi-arity function - wrap as curried closure
                // Return a zero-arity function that returns a curried closure
                return () => {
                  const id = rt.nextId++;
                  rt.closures.set(id, {
                    func: self._makeCurried(func, arity),
                    arity,
                    args: []
                  });
                  return rt.makeHeapPtr(id);
                };
              }
            }
          }
          // Return a stub that returns 0
          console.warn(`STUB: ${prop} not implemented`);
          return () => 0;
        }
      })
    };
  }

  async loadModule(name, watPath) {
    const wabt = await require('wabt')();
    const watSource = fs.readFileSync(watPath, 'utf8');
    const wasmModule = wabt.parseWat(watPath, watSource);
    const { buffer } = wasmModule.toBinary({});

    const imports = this.getImports(name);
    const { instance } = await WebAssembly.instantiate(buffer, imports);

    if (instance.exports.memory) {
      this.runtime.setMemory(instance.exports.memory);
      // Save this module's memory for its data segment access
      this.moduleMemories.set(name, instance.exports.memory);
    }

    // Capture the function table for indirect calls
    if (instance.exports.__indirect_function_table) {
      this.functionTables.set(name, instance.exports.__indirect_function_table);
      this.currentFunctionTable = instance.exports.__indirect_function_table;
    }

    this.modules.set(name, instance);
    this.currentModuleName = name;

    // Register all exports
    // Export names are already fully qualified (e.g., Nova_Compiler_Types_tInt)
    for (const [exportName, value] of Object.entries(instance.exports)) {
      if (typeof value === 'function') {
        this.exports[exportName] = value;
      }
    }

    return instance;
  }

  // Extract module dependencies from WAT source
  extractDependencies(watSource) {
    const deps = new Set();
    // Look for (import "modules" "Nova_Compiler_Foo_bar" ...) patterns
    const importRegex = /\(import\s+"modules"\s+"(Nova_Compiler_([^_]+)_[^"]+)"/g;
    let match;
    while ((match = importRegex.exec(watSource)) !== null) {
      const moduleName = match[2]; // e.g., "Types" from "Nova_Compiler_Types_tInt"
      deps.add(moduleName);
    }
    return [...deps];
  }

  // Topologically sort modules by dependencies
  sortByDependencies(watFiles) {
    // Build dependency graph
    const deps = new Map();
    const watByName = new Map();

    for (const { name, path: watPath } of watFiles) {
      watByName.set(name, watPath);
      const source = fs.readFileSync(watPath, 'utf8');
      const moduleDeps = this.extractDependencies(source);
      // Filter to only include deps that are in our module set
      const filteredDeps = moduleDeps.filter(d => watFiles.some(f => f.name === d));
      deps.set(name, filteredDeps);
    }

    // Kahn's algorithm for topological sort
    const inDegree = new Map();
    for (const name of deps.keys()) {
      inDegree.set(name, 0);
    }
    for (const [, moduleDeps] of deps) {
      for (const dep of moduleDeps) {
        inDegree.set(dep, (inDegree.get(dep) || 0) + 1);
      }
    }

    // Start with modules that have no dependencies on them
    const queue = [];
    for (const [name, degree] of inDegree) {
      if (degree === 0) {
        queue.push(name);
      }
    }

    const sorted = [];
    while (queue.length > 0) {
      const name = queue.shift();
      sorted.push(name);
      for (const dep of (deps.get(name) || [])) {
        const newDegree = inDegree.get(dep) - 1;
        inDegree.set(dep, newDegree);
        if (newDegree === 0) {
          queue.push(dep);
        }
      }
    }

    // Reverse so dependencies come first (we want Types before TypeChecker)
    sorted.reverse();

    // Return in sorted order
    return sorted.map(name => ({
      name,
      path: watByName.get(name)
    }));
  }

  async loadModules(watFiles, { sortDependencies = true } = {}) {
    let orderedFiles = watFiles;

    if (sortDependencies && watFiles.length > 1) {
      try {
        orderedFiles = this.sortByDependencies(watFiles);
        console.log('Loading order:', orderedFiles.map(f => f.name).join(' -> '));
      } catch (e) {
        console.warn('Could not sort dependencies, using provided order:', e.message);
      }
    }

    for (const { name, path } of orderedFiles) {
      console.log(`Loading ${name}...`);
      try {
        await this.loadModule(name, path);
        console.log(`  OK: ${Object.keys(this.modules.get(name).exports).length} exports`);
      } catch (err) {
        console.log(`  ERROR: ${err.message}`);
      }
    }
  }

  getModule(name) {
    return this.modules.get(name);
  }
}

// CLI
async function main() {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    console.log('Usage: node wasm-linker.js <wat-files...>');
    console.log('       node wasm-linker.js --dir <dir>');
    process.exit(1);
  }

  const linker = new NovaLinker();

  let watFiles = [];
  if (args[0] === '--dir') {
    const dir = args[1];
    const files = fs.readdirSync(dir).filter(f => f.endsWith('.wat'));
    watFiles = files.map(f => ({
      name: path.basename(f, '.wat'),
      path: path.join(dir, f)
    }));
  } else {
    watFiles = args.map(f => ({
      name: path.basename(f, '.wat'),
      path: f
    }));
  }

  console.log(`Loading ${watFiles.length} modules...`);
  await linker.loadModules(watFiles);

  console.log('\nAvailable exports:');
  for (const name of Object.keys(linker.exports).slice(0, 20)) {
    console.log(`  - ${name}`);
  }
  if (Object.keys(linker.exports).length > 20) {
    console.log(`  ... and ${Object.keys(linker.exports).length - 20} more`);
  }

  return linker;
}

if (require.main === module) {
  main().catch(err => {
    console.error('Error:', err);
    process.exit(1);
  });
}

module.exports = { NovaLinker };
