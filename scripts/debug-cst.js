const fs = require('fs');
const cstLexer = require('../output/Nova.Compiler.CstLexer/index.js');
const cstParser = require('../output/Nova.Compiler.CstParser/index.js');

// Test a simple module
const testSource = `module Test where

import Prelude

add :: Int -> Int -> Int
add x y = x + y
`;

console.log('Source:');
console.log(testSource);
console.log('='.repeat(50));

const tokens = cstLexer.lexModule(testSource);
console.log('Tokens:', tokens.length);
for (let i = 0; i < Math.min(tokens.length, 30); i++) {
  const tok = tokens[i];
  const val = tok.value;
  let tokStr = val.constructor ? val.constructor.name : JSON.stringify(val);
  if (val.value0) tokStr += ` ${val.value0}`;
  if (val.value1) tokStr += ` ${val.value1}`;
  console.log(`  ${i}: [${tok.range.start.line}:${tok.range.start.column}] ${tokStr}`);
}

console.log('='.repeat(50));
console.log('Trying to parse...');

const result = cstParser.runParser(cstParser.parseModule)(tokens);
if (result.constructor && result.constructor.name === 'Left') {
  console.log('Parse error:', result.value0);
  console.log('Remaining tokens:', JSON.stringify(result).substring(0, 500));
} else {
  console.log('Parse succeeded!');
  const mod = result.value0.value0;
  console.log('Module:', mod.header.name);
}
