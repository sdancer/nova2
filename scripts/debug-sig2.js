const fs = require('fs');
const cstLexer = require('../output/Nova.Compiler.CstLexer/index.js');
const cstParser = require('../output/Nova.Compiler.CstParser/index.js');

// Test type signature parsing - just the first signature
const source = `module Test where

makeDeclId :: String -> Int
makeDeclId s = s
`;

const tokens = cstLexer.lexModule(source);

// Skip module header (tokens 0-2)
const afterHeader = tokens.slice(3);
console.log('Tokens after module header:');
for (let i = 0; i < afterHeader.length; i++) {
  const tok = afterHeader[i];
  const val = tok.value;
  let tokStr = val.constructor ? val.constructor.name : JSON.stringify(val);
  if (val.value0 !== undefined) tokStr += ` "${val.value0}"`;
  console.log(`  ${i}: [${tok.range.start.line}:${tok.range.start.column}] ${tokStr}`);
}

console.log('\n--- Parsing first signature ---');
const sigResult = cstParser.runParser(cstParser.parseDeclSignature)(afterHeader);
if (sigResult.constructor && sigResult.constructor.name === 'Left') {
  console.log('parseDeclSignature failed:', sigResult.value0);
} else {
  console.log('parseDeclSignature succeeded!');
  const rem = sigResult.value0.value1;
  console.log('Remaining tokens:', rem.length);
  for (let i = 0; i < rem.length; i++) {
    const tok = rem[i];
    const val = tok.value;
    let tokStr = val.constructor ? val.constructor.name : JSON.stringify(val);
    if (val.value0 !== undefined) tokStr += ` "${val.value0}"`;
    console.log(`  ${i}: [${tok.range.start.line}:${tok.range.start.column}] ${tokStr}`);
  }

  console.log('\n--- Now trying parseDeclaration on remaining ---');
  const declResult = cstParser.runParser(cstParser.parseDeclaration)(rem);
  if (declResult.constructor && declResult.constructor.name === 'Left') {
    console.log('parseDeclaration failed:', declResult.value0);
  } else {
    console.log('parseDeclaration succeeded!');
    console.log('Type:', declResult.value0.value0.constructor?.name);
    console.log('Remaining:', declResult.value0.value1.length);
  }
}
