const fs = require('fs');
const cstLexer = require('../output/Nova.Compiler.CstLexer/index.js');
const cstParser = require('../output/Nova.Compiler.CstParser/index.js');

const source = fs.readFileSync('./src/Nova/Compiler/Ast.purs', 'utf8');

console.log('Lexing Ast.purs...');
const tokens = cstLexer.lexModule(source);
console.log('Tokens:', tokens.length);

// Find tokens around line 14-20 (first declaration area)
console.log('\nTokens around lines 14-30:');
for (let i = 0; i < tokens.length; i++) {
  const tok = tokens[i];
  if (tok.range.start.line >= 14 && tok.range.start.line <= 30) {
    const val = tok.value;
    let tokStr = val.constructor ? val.constructor.name : JSON.stringify(val);
    if (val.value0 !== undefined) tokStr += ` "${val.value0}"`;
    if (val.value1 !== undefined) tokStr += ` "${val.value1}"`;
    console.log(`  ${i}: [${tok.range.start.line}:${tok.range.start.column}] ${tokStr}`);
  }
}

console.log('\n='.repeat(50));
console.log('Trying to parse...');

const result = cstParser.runParser(cstParser.parseModule)(tokens);
if (result.constructor && result.constructor.name === 'Left') {
  console.log('Parse error:', result.value0);
} else {
  console.log('Parse succeeded!');
  const cstMod = result.value0.value0;
  const remaining = result.value0.value1;
  console.log('Module:', cstMod.header.name.name);
  console.log('Imports:', cstMod.header.imports.length);
  console.log('Declarations:', cstMod.body.decls.length);
  console.log('Remaining tokens:', remaining.length);

  if (remaining.length > 0) {
    console.log('\nFirst 10 remaining tokens:');
    for (let i = 0; i < Math.min(remaining.length, 10); i++) {
      const tok = remaining[i];
      const val = tok.value;
      let tokStr = val.constructor ? val.constructor.name : JSON.stringify(val);
      if (val.value0 !== undefined) tokStr += ` "${val.value0}"`;
      if (val.value1 !== undefined) tokStr += ` "${val.value1}"`;
      console.log(`  [${tok.range.start.line}:${tok.range.start.column}] ${tokStr}`);
    }
  }

  // Print declarations
  console.log('\nDeclarations found:');
  for (const decl of cstMod.body.decls) {
    console.log(' -', decl.constructor ? decl.constructor.name : 'unknown');
  }
}
