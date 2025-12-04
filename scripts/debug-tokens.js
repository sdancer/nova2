const fs = require('fs');
const cstLexer = require('../output/Nova.Compiler.CstLexer/index.js');

const source = fs.readFileSync('./src/Nova/Compiler/Ast.purs', 'utf8');

const tokens = cstLexer.lexModule(source);

// Find tokens around line 205-210
console.log('Tokens around lines 203-210:');
for (let i = 0; i < tokens.length; i++) {
  const tok = tokens[i];
  if (tok.range.start.line >= 203 && tok.range.start.line <= 210) {
    const val = tok.value;
    let tokStr = val.constructor ? val.constructor.name : JSON.stringify(val);
    if (val.value0 !== undefined) tokStr += ` "${val.value0}"`;
    if (val.value1 !== undefined) tokStr += ` "${val.value1}"`;
    console.log(`  ${i}: [${tok.range.start.line}:${tok.range.start.column}] ${tokStr}`);
  }
}
