const fs = require('fs');
const cstLexer = require('../output/Nova.Compiler.CstLexer/index.js');
const cstParser = require('../output/Nova.Compiler.CstParser/index.js');

// Test multi-equation function
const source = `module Test where

getDeclKind :: Int -> Int
getDeclKind 0 = 1
getDeclKind n = n
`;

console.log('Source:');
console.log(source);

const tokens = cstLexer.lexModule(source);
console.log('Tokens:');
for (let i = 0; i < tokens.length; i++) {
  const tok = tokens[i];
  const val = tok.value;
  let tokStr = val.constructor ? val.constructor.name : JSON.stringify(val);
  if (val.value0 !== undefined) tokStr += ` "${val.value0}"`;
  console.log(`  ${i}: [${tok.range.start.line}:${tok.range.start.column}] ${tokStr}`);
}

console.log('\n--- Trying parseModule ---');
const result = cstParser.runParser(cstParser.parseModule)(tokens);
if (result.constructor && result.constructor.name === 'Left') {
  console.log('parseModule failed:', result.value0);
} else {
  console.log('parseModule succeeded!');
  const mod = result.value0.value0;
  const rem = result.value0.value1;
  console.log('Declarations:', mod.body.decls.length);
  for (const decl of mod.body.decls) {
    console.log(' -', decl.constructor?.name);
  }
  console.log('Remaining:', rem.length);
  if (rem.length > 0) {
    console.log('Remaining tokens:');
    for (let i = 0; i < Math.min(rem.length, 10); i++) {
      const tok = rem[i];
      const val = tok.value;
      let tokStr = val.constructor ? val.constructor.name : JSON.stringify(val);
      if (val.value0 !== undefined) tokStr += ` "${val.value0}"`;
      console.log(`  ${i}: [${tok.range.start.line}:${tok.range.start.column}] ${tokStr}`);
    }
  }
}
