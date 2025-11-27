const fs = require('fs');
const codegen = require('../output/Nova.Compiler.CodeGen/index.js');
const parser = require('../output/Nova.Compiler.Parser/index.js');
const tokenizer = require('../output/Nova.Compiler.Tokenizer/index.js');
const types = require('../output/Nova.Compiler.Types/index.js');
const tc = require('../output/Nova.Compiler.TypeChecker/index.js');

function compile(name, path, deps) {
  console.log('Compiling', name + '...');

  // Parse deps first
  let depDecls = [];
  for (const depPath of (deps || [])) {
    const src = fs.readFileSync(depPath, 'utf8');
    const tokens = tokenizer.tokenize(src);
    const result = parser.parseModule(tokens);
    if (!(result.constructor && result.constructor.name === 'Left')) {
      depDecls = depDecls.concat(result.value0.value0.declarations);
    }
  }

  const src = fs.readFileSync(path, 'utf8');
  const tokens = tokenizer.tokenize(src);
  const result = parser.parseModule(tokens);

  if (result.constructor && result.constructor.name === 'Left') {
    console.log('  Parse error:', result.value0);
    return null;
  }

  const mod = result.value0.value0;
  const allDecls = depDecls.concat(mod.declarations);
  const check = tc.checkModule(types.emptyEnv)(allDecls);

  if (check.constructor && check.constructor.name === 'Left') {
    console.log('  Type error:', JSON.stringify(check.value0));
    return null;
  }

  const code = codegen.genModule(mod);
  console.log('  Generated', code.split('\n').length, 'lines');
  return code;
}

const base = './src/Nova/Compiler/';
const modules = [
  ['Ast', base + 'Ast.purs', []],
  ['Types', base + 'Types.purs', [base + 'Ast.purs']],
  ['Tokenizer', base + 'Tokenizer.purs', []],
  ['Unify', base + 'Unify.purs', [base + 'Types.purs']],
  ['TypeChecker', base + 'TypeChecker.purs', [base + 'Ast.purs', base + 'Types.purs', base + 'Unify.purs']],
  ['CodeGen', base + 'CodeGen.purs', [base + 'Ast.purs']],
  ['Parser', base + 'Parser.purs', [base + 'Ast.purs', base + 'Tokenizer.purs']],
  ['Dependencies', base + 'Dependencies.purs', [base + 'Ast.purs']],
];

for (const [name, path, deps] of modules) {
  const code = compile(name, path, deps);
  if (code) {
    fs.writeFileSync('./output/' + name + '.ex', code);
  }
}
console.log('Done!');
