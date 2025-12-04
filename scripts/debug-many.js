const fs = require('fs');
const cstLexer = require('../output/Nova.Compiler.CstLexer/index.js');
const cstParser = require('../output/Nova.Compiler.CstParser/index.js');

// Test parsing multiple declarations
const source = `module Test where

data Status = Fresh

type Foo = Int
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

console.log('='.repeat(50));

// Try parsing just the body
const afterHeader = tokens.slice(3); // Skip module Test where
console.log('Trying to parse declaration from:', afterHeader[0]?.value);

// Try parseDeclData
const dataResult = cstParser.runParser(cstParser.parseDeclData)(afterHeader);
if (dataResult.constructor && dataResult.constructor.name === 'Left') {
  console.log('parseDeclData failed:', dataResult.value0);
} else {
  console.log('parseDeclData succeeded!');
  const remaining = dataResult.value0.value1;
  console.log('Remaining:', remaining.length);
  console.log('Next token:', remaining[0]?.value);
}

// Try parseDeclType on what remains
const afterData = dataResult.value0?.value1 || [];
console.log('\nTrying parseDeclType from:', afterData[0]?.value);
const typeResult = cstParser.runParser(cstParser.parseDeclType)(afterData);
if (typeResult.constructor && typeResult.constructor.name === 'Left') {
  console.log('parseDeclType failed:', typeResult.value0);
} else {
  console.log('parseDeclType succeeded!');
}
