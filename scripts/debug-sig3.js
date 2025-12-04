const fs = require('fs');
const cstLexer = require('../output/Nova.Compiler.CstLexer/index.js');
const cstParser = require('../output/Nova.Compiler.CstParser/index.js');

// Test type parsing
const source = `module Test where

makeDeclId :: String -> Int
makeDeclId s = s
`;

const tokens = cstLexer.lexModule(source);

// Start from :: token (token index 4 in full, 1 in afterHeader)
const afterColonColon = tokens.slice(5);  // Starting from "String"
console.log('Tokens after :: (should be String -> Int makeDeclId s = s):');
for (let i = 0; i < afterColonColon.length; i++) {
  const tok = afterColonColon[i];
  const val = tok.value;
  let tokStr = val.constructor ? val.constructor.name : JSON.stringify(val);
  if (val.value0 !== undefined) tokStr += ` "${val.value0}"`;
  console.log(`  ${i}: [${tok.range.start.line}:${tok.range.start.column}] ${tokStr}`);
}

console.log('\n--- Parsing type ---');
const typeResult = cstParser.runParser(cstParser.parseType)(afterColonColon);
if (typeResult.constructor && typeResult.constructor.name === 'Left') {
  console.log('parseType failed:', typeResult.value0);
} else {
  console.log('parseType succeeded!');
  const rem = typeResult.value0.value1;
  console.log('Remaining tokens:', rem.length);
  for (let i = 0; i < rem.length; i++) {
    const tok = rem[i];
    const val = tok.value;
    let tokStr = val.constructor ? val.constructor.name : JSON.stringify(val);
    if (val.value0 !== undefined) tokStr += ` "${val.value0}"`;
    console.log(`  ${i}: [${tok.range.start.line}:${tok.range.start.column}] ${tokStr}`);
  }
}
