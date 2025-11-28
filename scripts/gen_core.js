const fs = require('fs');
const { parseModule } = require('../output/Nova.Compiler.Parser/index.js');
const { tokenize } = require('../output/Nova.Compiler.Tokenizer/index.js');
const { genModule } = require('../output/Nova.Compiler.CodeGenCoreErlang/index.js');

const files = [
  'Ast', 'Types', 'Tokenizer', 'Unify', 'TypeChecker', 'CodeGen', 'Parser', 'Dependencies'
];

files.forEach(name => {
  const path = './src/Nova/Compiler/' + name + '.purs';
  const src = fs.readFileSync(path, 'utf8');
  const tokens = tokenize(src);
  const result = parseModule(tokens);

  // Parse result is Right(Tuple(AST, rest)) on success
  // In PureScript FFI: Right = { value0: undefined, value1: Tuple }
  // Where Tuple = { value0: AST, value1: rest }
  // But parseModule actually returns the raw tuple from success()

  // Debug: show the structure
  // console.log('Result keys:', Object.keys(result));

  let mod;
  // Result structure is: result.value0 = Tuple, result.value0.value0 = AST
  if (result.value0 && result.value0.value0 && result.value0.value0.name) {
    mod = result.value0.value0;
  } else if (result.value1 && result.value1.value0) {
    // Right case
    mod = result.value1.value0;
  } else if (result.value0 && result.value0.name) {
    // AST directly in value0
    mod = result.value0;
  } else {
    console.log('Parse error for ' + name + ': structure=' + JSON.stringify(Object.keys(result)));
    return;
  }

  if (!mod || !mod.name) {
    console.log('Invalid AST for ' + name + ': ' + JSON.stringify(mod ? Object.keys(mod) : null));
    return;
  }

  const code = genModule(mod);
  const outFile = '/tmp/nova_core/nova_compiler_' + name.toLowerCase() + '.core';
  fs.writeFileSync(outFile, code);
  console.log('Generated ' + outFile + ' (' + code.split('\n').length + ' lines)');
});
