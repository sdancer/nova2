const fs = require('fs');
const cstLexer = require('../output/Nova.Compiler.CstLexer/index.js');
const cstParser = require('../output/Nova.Compiler.CstParser/index.js');

// Test type signature parsing
const source = `module Test where

makeDeclId :: String -> Int
makeDeclId s = s
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

// Try parsing just the value declaration after skipping module header + signature
// Module header is: module Test where (3 tokens)
// Signature is: makeDeclId :: String -> Int (5 tokens)
const afterSig = tokens.slice(8); // Skip to second makeDeclId
console.log('\\nTokens after signature:');
for (let i = 0; i < afterSig.length; i++) {
  const tok = afterSig[i];
  const val = tok.value;
  let tokStr = val.constructor ? val.constructor.name : JSON.stringify(val);
  if (val.value0 !== undefined) tokStr += ` "${val.value0}"`;
  if (val.value1 !== undefined) tokStr += ` "${val.value1}"`;
  console.log(`  ${i}: [${tok.range.start.line}:${tok.range.start.column}] ${tokStr}`);
}

console.log('\\nTrying parseDeclValue on afterSig...');
const valueResult = cstParser.runParser(cstParser.parseDeclValue)(afterSig);
if (valueResult.constructor && valueResult.constructor.name === 'Left') {
  console.log('parseDeclValue failed:', valueResult.value0);
} else {
  console.log('parseDeclValue succeeded!');
  const decl = valueResult.value0.value0;
  const rem = valueResult.value0.value1;
  console.log('Remaining:', rem.length);
}

console.log('\\nTrying parseDeclSignature on afterSig...');
const sigResult = cstParser.runParser(cstParser.parseDeclSignature)(afterSig);
if (sigResult.constructor && sigResult.constructor.name === 'Left') {
  console.log('parseDeclSignature failed:', sigResult.value0);
} else {
  console.log('parseDeclSignature succeeded!');
  const decl = sigResult.value0.value0;
  const rem = sigResult.value0.value1;
  console.log('Remaining:', rem.length);
  if (rem.length > 0) {
    console.log('Remaining tokens:');
    for (let i = 0; i < rem.length; i++) {
      const tok = rem[i];
      const val = tok.value;
      let tokStr = val.constructor ? val.constructor.name : JSON.stringify(val);
      if (val.value0 !== undefined) tokStr += ` "${val.value0}"`;
      console.log(`  ${i}: [${tok.range.start.line}:${tok.range.start.column}] ${tokStr}`);
    }
  }
}

console.log('\\nTrying parseDeclaration on afterSig...');
const declResult = cstParser.runParser(cstParser.parseDeclaration)(afterSig);
if (declResult.constructor && declResult.constructor.name === 'Left') {
  console.log('parseDeclaration failed:', declResult.value0);
} else {
  console.log('parseDeclaration succeeded!');
  const decl = declResult.value0.value0;
  const rem = declResult.value0.value1;
  console.log('Declaration type:', decl.constructor?.name);
  console.log('Remaining:', rem.length);
  if (rem.length > 0) {
    console.log('Remaining tokens:');
    for (let i = 0; i < rem.length; i++) {
      const tok = rem[i];
      const val = tok.value;
      let tokStr = val.constructor ? val.constructor.name : JSON.stringify(val);
      if (val.value0 !== undefined) tokStr += ` "${val.value0}"`;
      console.log(`  ${i}: [${tok.range.start.line}:${tok.range.start.column}] ${tokStr}`);
    }
  }
}

console.log('='.repeat(50));
console.log('Trying to parse full module...');

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

  if (remaining.length > 0) {
    console.log('\nRemaining tokens:');
    for (let i = 0; i < remaining.length; i++) {
      const tok = remaining[i];
      const val = tok.value;
      let tokStr = val.constructor ? val.constructor.name : JSON.stringify(val);
      if (val.value0 !== undefined) tokStr += ` "${val.value0}"`;
      if (val.value1 !== undefined) tokStr += ` "${val.value1}"`;
      console.log(`  ${i}: [${tok.range.start.line}:${tok.range.start.column}] ${tokStr}`);
    }
  }
}
