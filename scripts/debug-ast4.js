const fs = require('fs');
const cstLexer = require('../output/Nova.Compiler.CstLexer/index.js');
const cstParser = require('../output/Nova.Compiler.CstParser/index.js');

const source = fs.readFileSync('./src/Nova/Compiler/Ast.purs', 'utf8');

console.log('Lexing Ast.purs...');
const tokens = cstLexer.lexModule(source);
console.log('Tokens:', tokens.length);

console.log('\n--- Trying parseModule ---');
const result = cstParser.runParser(cstParser.parseModule)(tokens);
if (result.constructor && result.constructor.name === 'Left') {
  console.log('parseModule failed:', result.value0);
} else {
  console.log('parseModule succeeded!');
  const mod = result.value0.value0;
  const rem = result.value0.value1;
  console.log('Module:', mod.header.name.name);
  console.log('Imports:', mod.header.imports.length);
  console.log('Declarations:', mod.body.decls.length);
  console.log('Remaining tokens:', rem.length);

  if (mod.header.imports.length > 0) {
    console.log('\nImports:');
    for (const imp of mod.header.imports) {
      console.log(' -', imp.module?.name);
    }
  }

  if (mod.body.decls.length > 0) {
    console.log('\nFirst 10 declarations:');
    for (let i = 0; i < Math.min(mod.body.decls.length, 10); i++) {
      const decl = mod.body.decls[i];
      console.log(' -', decl.constructor?.name);
    }
  }

  if (rem.length > 0) {
    console.log('\nFirst 10 remaining tokens:');
    for (let i = 0; i < Math.min(rem.length, 10); i++) {
      const tok = rem[i];
      const val = tok.value;
      let tokStr = val.constructor ? val.constructor.name : JSON.stringify(val);
      if (val.value0 !== undefined) tokStr += ` "${val.value0}"`;
      console.log(`  ${i}: [${tok.range.start.line}:${tok.range.start.column}] ${tokStr}`);
    }
  }
}
