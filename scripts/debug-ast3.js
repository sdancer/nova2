const fs = require('fs');
const cstLexer = require('../output/Nova.Compiler.CstLexer/index.js');
const cstParser = require('../output/Nova.Compiler.CstParser/index.js');

const source = fs.readFileSync('./src/Nova/Compiler/Ast.purs', 'utf8');

console.log('Lexing Ast.purs...');
const tokens = cstLexer.lexModule(source);
console.log('Tokens:', tokens.length);

// Try parsing just the module header
console.log('\n--- Trying parseModuleHeader ---');
const headerResult = cstParser.runParser(cstParser.parseModuleHeader)(tokens);
if (headerResult.constructor && headerResult.constructor.name === 'Left') {
  console.log('parseModuleHeader failed:', headerResult.value0);
} else {
  console.log('parseModuleHeader succeeded!');
  const header = headerResult.value0.value0;
  const rem = headerResult.value0.value1;
  console.log('Module:', header.name.name);
  console.log('Imports:', header.imports.length);
  console.log('Remaining tokens:', rem.length);

  // Show first few remaining tokens
  console.log('\nFirst 10 remaining tokens after header:');
  for (let i = 0; i < Math.min(rem.length, 10); i++) {
    const tok = rem[i];
    const val = tok.value;
    let tokStr = val.constructor ? val.constructor.name : JSON.stringify(val);
    if (val.value0 !== undefined) tokStr += ` "${val.value0}"`;
    console.log(`  ${i}: [${tok.range.start.line}:${tok.range.start.column}] ${tokStr}`);
  }

  // Try parsing module body
  console.log('\n--- Trying parseModuleBody ---');
  const bodyResult = cstParser.runParser(cstParser.parseModuleBody)(rem);
  if (bodyResult.constructor && bodyResult.constructor.name === 'Left') {
    console.log('parseModuleBody failed:', bodyResult.value0);

    // Try layoutBlock manually
    console.log('\n--- Trying layoutBlock parseDeclaration ---');

    // First check for TokLayoutStart
    if (rem.length > 0 && rem[0].value.constructor?.name === 'TokLayoutStart') {
      console.log('Found TokLayoutStart');
      const afterStart = rem.slice(1);

      // Try parsing first declaration
      console.log('\n--- Trying first parseDeclaration ---');
      const declResult = cstParser.runParser(cstParser.parseDeclaration)(afterStart);
      if (declResult.constructor && declResult.constructor.name === 'Left') {
        console.log('First parseDeclaration failed:', declResult.value0);
        console.log('First token after LayoutStart:', afterStart[0]?.value);
      } else {
        console.log('First parseDeclaration succeeded!');
        console.log('Type:', declResult.value0.value0.constructor?.name);
      }
    } else {
      console.log('No TokLayoutStart found! First token:', rem[0]?.value);
    }
  } else {
    console.log('parseModuleBody succeeded!');
    const body = bodyResult.value0.value0;
    console.log('Declarations:', body.decls.length);
  }
}
