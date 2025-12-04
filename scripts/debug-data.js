const fs = require('fs');
const cstLexer = require('../output/Nova.Compiler.CstLexer/index.js');
const cstParser = require('../output/Nova.Compiler.CstParser/index.js');

// Test data declaration parsing
const source = `module Test where

data Status = Fresh | Valid

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
console.log('Trying to parse...');

const result = cstParser.runParser(cstParser.parseModule)(tokens);
if (result.constructor && result.constructor.name === 'Left') {
  console.log('Parse error:', result.value0);
} else {
  console.log('Parse succeeded!');
  const cstMod = result.value0.value0;
  const remaining = result.value0.value1;
  console.log('Module:', cstMod.header.name.name);
  console.log('Declarations:', cstMod.body.decls.length);
  console.log('Remaining tokens:', remaining.length);

  for (const decl of cstMod.body.decls) {
    console.log(' -', decl.constructor ? decl.constructor.name : 'unknown');
  }
}
