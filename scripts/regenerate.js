const fs = require('fs');
const codegen = require('../output/Nova.Compiler.CodeGen/index.js');
const cstPipeline = require('../output/Nova.Compiler.CstPipeline/index.js');
const types = require('../output/Nova.Compiler.Types/index.js');
const tc = require('../output/Nova.Compiler.TypeChecker/index.js');

const base = './src/Nova/Compiler/';

// Module dependency graph
const moduleDeps = {
  [base + 'Ast.purs']: [],
  [base + 'Types.purs']: [base + 'Ast.purs'],
  [base + 'Cst.purs']: [],
  [base + 'CstLayout.purs']: [base + 'Cst.purs'],
  [base + 'CstLexer.purs']: [base + 'Cst.purs', base + 'CstLayout.purs'],
  [base + 'CstParser.purs']: [base + 'Cst.purs'],
  [base + 'CstToAst.purs']: [base + 'Cst.purs', base + 'Ast.purs'],
  [base + 'CstPipeline.purs']: [base + 'Cst.purs', base + 'Ast.purs', base + 'CstLexer.purs', base + 'CstParser.purs', base + 'CstToAst.purs'],
  [base + 'Tokenizer.purs']: [],
  [base + 'Parser.purs']: [base + 'Ast.purs', base + 'Tokenizer.purs'],
  [base + 'Unify.purs']: [base + 'Types.purs'],
  [base + 'TypeChecker.purs']: [base + 'Ast.purs', base + 'Types.purs', base + 'Unify.purs'],
  [base + 'CodeGen.purs']: [base + 'Ast.purs'],
  [base + 'CodeGenCoreErlang.purs']: [base + 'Ast.purs'],
  [base + 'CodeGenWasmSimple.purs']: [base + 'Ast.purs'],
  [base + 'Dependencies.purs']: [base + 'Ast.purs'],
};

// Cache for parsed and type-checked modules
const moduleCache = {};

// Helper to get module name from path
function getModuleName(path) {
  const match = path.match(/\/(\w+)\.purs$/);
  return match ? 'Nova.Compiler.' + match[1] : path;
}

// Parse and type-check a module, caching the result
function parseAndCheckModule(path) {
  if (moduleCache[path]) {
    return moduleCache[path];
  }

  const deps = moduleDeps[path] || [];
  const src = fs.readFileSync(path, 'utf8');
  const result = cstPipeline.parseModuleCst(src);

  if (result.constructor && result.constructor.name === 'Left') {
    console.log('  Parse error in', path + ':', result.value0);
    return null;
  }

  const mod = result.value0;

  // Build registry from dependencies (recursively)
  let registry = types.emptyRegistry;
  let depDecls = [];
  for (const depPath of deps) {
    const depResult = parseAndCheckModule(depPath);
    if (depResult) {
      const depName = getModuleName(depPath);
      registry = types.registerModule(registry)(depName)(depResult.exports);
      depDecls = depDecls.concat(depResult.mod.declarations);
    }
  }

  // Type check with registry for imports + concatenated deps for backward compatibility
  const allDecls = depDecls.concat(mod.declarations);
  const check = tc.checkModuleWithRegistry(registry)(types.emptyEnv)(allDecls);

  if (check.constructor && check.constructor.name === 'Left') {
    console.log('  Type error in', path + ':', JSON.stringify(check.value0));
    return null;
  }

  const env = check.value0;

  // Extract exports from the checked module
  const exports = tc.extractExports(mod.declarations);
  const exportsWithValues = tc.addValuesToExports(exports)(env)(mod.declarations);

  moduleCache[path] = { mod, env, exports: exportsWithValues };
  return moduleCache[path];
}

function compile(name, path) {
  console.log('Compiling', name + '...');

  const result = parseAndCheckModule(path);
  if (!result) {
    return null;
  }

  const code = codegen.genModule(result.mod);
  console.log('  Generated', code.split('\n').length, 'lines');
  return code;
}

const modules = [
  // Core types
  ['Ast', base + 'Ast.purs'],
  ['Types', base + 'Types.purs'],

  // CST types and lexer/parser
  ['Cst', base + 'Cst.purs'],
  ['CstLayout', base + 'CstLayout.purs'],
  ['CstLexer', base + 'CstLexer.purs'],
  ['CstParser', base + 'CstParser.purs'],
  ['CstToAst', base + 'CstToAst.purs'],
  ['CstPipeline', base + 'CstPipeline.purs'],

  // Legacy parser (still needed for some things)
  ['Tokenizer', base + 'Tokenizer.purs'],
  ['Parser', base + 'Parser.purs'],

  // Type system
  ['Unify', base + 'Unify.purs'],
  ['TypeChecker', base + 'TypeChecker.purs'],

  // Code generation
  ['CodeGen', base + 'CodeGen.purs'],
  ['CodeGenCoreErlang', base + 'CodeGenCoreErlang.purs'],
  ['CodeGenWasmSimple', base + 'CodeGenWasmSimple.purs'],

  // Utilities
  ['Dependencies', base + 'Dependencies.purs'],
];

for (const [name, path] of modules) {
  const code = compile(name, path);
  if (code) {
    fs.writeFileSync('./output/' + name + '.ex', code);
    // Also copy to nova_lang for running
    fs.writeFileSync('./nova_lang/lib/nova/compiler/' + name + '.ex', code);
  }
}
console.log('Done!');
