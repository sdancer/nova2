const fs = require('fs');
const cstLexer = require('../output/Nova.Compiler.CstLexer/index.js');

// Test layout tokens
const source = `module Test where

makeDeclId :: String -> Int
makeDeclId s = s

foo :: Int
foo = 42
`;

console.log('Source:');
console.log(source);
console.log('='.repeat(50));

const tokens = cstLexer.lexModule(source);
console.log('Tokens:', tokens.length);
for (let i = 0; i < tokens.length; i++) {
  const tok = tokens[i];
  const val = tok.value;
  let tokStr = val.constructor ? val.constructor.name : JSON.stringify(val);
  if (val.value0 !== undefined) tokStr += ` "${val.value0}"`;
  if (val.value1 !== undefined) tokStr += ` "${val.value1}"`;
  console.log(`  ${i}: [${tok.range.start.line}:${tok.range.start.column}] ${tokStr}`);
}
