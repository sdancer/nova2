const fs = require('fs');
const cstLexer = require('../output/Nova.Compiler.CstLexer/index.js');
const cstParser = require('../output/Nova.Compiler.CstParser/index.js');

const source = fs.readFileSync('./src/Nova/Compiler/Ast.purs', 'utf8');

console.log('Lexing Ast.purs...');
const tokens = cstLexer.lexModule(source);
console.log('Tokens:', tokens.length);

// Show first 50 tokens
console.log('\nFirst 50 tokens:');
for (let i = 0; i < Math.min(tokens.length, 50); i++) {
  const tok = tokens[i];
  const val = tok.value;
  let tokStr = val.constructor ? val.constructor.name : JSON.stringify(val);
  if (val.value0 !== undefined) tokStr += ` "${val.value0}"`;
  if (val.value1 !== undefined) tokStr += ` "${val.value1}"`;
  console.log(`  ${i}: [${tok.range.start.line}:${tok.range.start.column}] ${tokStr}`);
}

console.log('\n='.repeat(50));
console.log('Trying to parse...');

const result = cstParser.runParser(cstParser.parseModule)(tokens);
if (result.constructor && result.constructor.name === 'Left') {
  console.log('Parse error:', result.value0);
} else {
  console.log('Parse succeeded!');
  const mod = result.value0.value0;
  console.log('Module:', mod.header.name.name);
  console.log('Imports:', mod.header.imports.length);
  console.log('Declarations:', mod.body.decls.length);
}
