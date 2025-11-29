#!/usr/bin/env node
// Test Maybe pattern matching in WASM

const fs = require('fs');
const path = require('path');
const { NovaLinker } = require('./wasm-linker.js');

async function main() {
  const linker = new NovaLinker();
  const rt = linker.runtime;

  // Load Parser module
  const wasmDir = '/tmp/wasm_modules';
  const files = fs.readdirSync(wasmDir).filter(f => f.endsWith('.wat'));
  const watFiles = files.map(f => ({
    name: path.basename(f, '.wat'),
    path: path.join(wasmDir, f)
  }));

  console.log('Loading compiler modules...');
  await linker.loadModules(watFiles);
  console.log(`Loaded ${linker.modules.size} modules\n`);

  // Test CU_charAt directly from stdlib
  const stdlib = linker.getStdLib();
  const CU_charAt = stdlib.CU_charAt;

  // Create a test string
  const testStr = "hello";
  const strId = rt.nextId++;
  rt.strings.set(strId, testStr);
  const strVal = rt.makeHeapPtr(strId);

  console.log('Testing CU_charAt...');
  console.log('Test string:', testStr);
  console.log('String value:', strVal, '(tag:', rt.getTag(strVal), ')');

  // Apply CU_charAt to index 0 and string
  const partial = linker.applyClosure(CU_charAt(), rt.makeInt(0));
  console.log('Partial application result:', partial, '(tag:', rt.getTag(partial), ')');

  const result = linker.applyClosure(partial, strVal);
  console.log('Full application result:', result, '(tag:', rt.getTag(result), ')');

  // Decode the result
  const tag = rt.getTag(result);
  if (tag === 3) {
    console.log('Result is CTOR, tag:', rt.getCtorTag(result));
    console.log('This is Nothing');
  } else if (tag === 2) {
    const id = rt.unboxHeapPtr(result);
    const arr = rt.arrays.get(id);
    console.log('Result is heap ptr, array:', arr);
    if (arr) {
      console.log('  arr[0] tag:', rt.getTag(arr[0]), 'ctor tag:', rt.getCtorTag(arr[0]));
      console.log('  arr[1] =', arr[1], '(unboxed int:', rt.unboxInt(arr[1]), ')');
      console.log('This is Just', String.fromCharCode(rt.unboxInt(arr[1])));
    }
  }

  // Test with out of bounds index
  console.log('\nTesting with index 100 (out of bounds)...');
  const partial2 = linker.applyClosure(CU_charAt(), rt.makeInt(100));
  const result2 = linker.applyClosure(partial2, strVal);
  console.log('Result:', result2, '(tag:', rt.getTag(result2), ')');
  if (rt.getTag(result2) === 3) {
    console.log('Result is CTOR, tag:', rt.getCtorTag(result2));
    console.log('is_ctor:', (result2 & 3) === 3);
    console.log('This is Nothing');
  }

  // Now test the isLowerCase function from Parser
  console.log('\n\nTesting isLowerCase from Parser...');
  const parserMod = linker.getModule('Parser');
  if (parserMod && parserMod.exports.isLowerCase) {
    console.log('isLowerCase function found');

    // Test with "hello"
    const helloId = rt.nextId++;
    rt.strings.set(helloId, "hello");
    const helloVal = rt.makeHeapPtr(helloId);

    try {
      const isLower = parserMod.exports.isLowerCase(helloVal);
      console.log('isLowerCase("hello") =', isLower, '(bool:', rt.unboxBool(isLower), ')');
    } catch (err) {
      console.log('Error calling isLowerCase:', err.message);
    }
  } else {
    console.log('isLowerCase not found in Parser exports');
    if (parserMod) {
      const funcs = Object.keys(parserMod.exports).filter(k => typeof parserMod.exports[k] === 'function');
      console.log('Available functions:', funcs.slice(0, 20).join(', '));
    }
  }

  // Test more parser functions to find the failing one
  console.log('\n\nTesting more Parser functions...');

  // Create token array for "module Test where"
  const tokenizerMod = linker.getModule('Tokenizer');
  const tokenize = tokenizerMod.exports.tokenize;

  const testSource = "module Test where\n\nx = 1\n";
  const srcId = rt.nextId++;
  rt.strings.set(srcId, testSource);
  const srcVal = rt.makeHeapPtr(srcId);

  const tokensVal = tokenize(srcVal);
  console.log('Tokens value:', tokensVal, '(tag:', rt.getTag(tokensVal), ')');

  // List parser functions
  const parseFuncs = Object.keys(parserMod.exports).filter(k =>
    typeof parserMod.exports[k] === 'function' && k.startsWith('parse')
  );
  console.log('Parse functions:', parseFuncs.join(', '));

  // Test each parse function
  for (const funcName of parseFuncs) {
    if (funcName === 'parseModule' || funcName === 'parseDeclaration') {
      // These might fail - test them
      try {
        console.log(`Testing ${funcName}...`);
        const result = parserMod.exports[funcName](tokensVal);
        console.log(`  ${funcName} result: ${result} (tag: ${rt.getTag(result)})`);
      } catch (err) {
        console.log(`  ${funcName} ERROR: ${err.message}`);
      }
    }
  }

  // Test parseModuleHeader specifically
  if (parserMod.exports.parseModuleHeader) {
    console.log('\nTesting parseModuleHeader...');
    try {
      const result = parserMod.exports.parseModuleHeader(tokensVal);
      console.log('parseModuleHeader result:', result, '(tag:', rt.getTag(result), ')');
    } catch (err) {
      console.log('parseModuleHeader ERROR:', err.message);
    }
  }
}

main().catch(err => {
  console.error('Error:', err);
  process.exit(1);
});
