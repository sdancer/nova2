const fs = require('fs');
const codegen = require('../output/Nova.Compiler.CodeGen/index.js');
const cstPipeline = require('../output/Nova.Compiler.CstPipeline/index.js');
const types = require('../output/Nova.Compiler.Types/index.js');
const tc = require('../output/Nova.Compiler.TypeChecker/index.js');

// Helper to convert PureScript List (Cons/Nil) to JS Array
function listToArray(list) {
  const result = [];
  let current = list;
  while (current && current.constructor && current.constructor.name === 'Cons') {
    result.push(current.value0);
    current = current.value1;
  }
  return result;
}

// Helper to convert PureScript Map to object for counting
function mapSize(map) {
  if (!map || !map.value0) return 0;
  // Simplified - just count by traversing the tree
  let count = 0;
  function traverse(node) {
    if (!node) return;
    if (node.value2) count++; // has a key-value pair
    if (node.value4) traverse(node.value4);
    if (node.value5) traverse(node.value5);
  }
  traverse(map);
  return count;
}

const base = './src/Nova/Compiler/';
const lib = './lib/';

// Library modules (compiled first, used as dependencies)
// Order matters - modules with no deps first, then those that depend on them
const libModuleDeps = {
  [lib + 'Data/Maybe.purs']: [],
  [lib + 'Data/Tuple.purs']: [],
  [lib + 'Data/Either.purs']: [],
  [lib + 'Data/Char.purs']: [],
  [lib + 'Data/List.purs']: [lib + 'Data/Maybe.purs', lib + 'Data/Tuple.purs'],
  [lib + 'Data/Array.purs']: [lib + 'Data/Maybe.purs', lib + 'Data/Tuple.purs'],
  [lib + 'Data/String.purs']: [lib + 'Data/Maybe.purs'],
  [lib + 'Data/Map.purs']: [lib + 'Data/Maybe.purs', lib + 'Data/Tuple.purs'],
  [lib + 'Data/Set.purs']: [lib + 'Data/Maybe.purs'],
  [lib + 'Data/Foldable.purs']: [lib + 'Data/Maybe.purs'],
};

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

// Registry for library modules (populated before compiling main modules)
let libRegistry = types.emptyRegistry;

// Helper to get module name from path
function getModuleName(path) {
  // Library modules use their path structure (e.g., lib/Data/List.purs -> Data.List)
  if (path.startsWith(lib)) {
    const relative = path.slice(lib.length).replace(/\.purs$/, '');
    return relative.replace(/\//g, '.');
  }
  // Compiler modules use Nova.Compiler prefix
  const match = path.match(/\/(\w+)\.purs$/);
  return match ? 'Nova.Compiler.' + match[1] : path;
}

// Parse and type-check a module, caching the result
function parseAndCheckModule(path, isLibModule = false) {
  if (moduleCache[path]) {
    return moduleCache[path];
  }

  const deps = isLibModule ? (libModuleDeps[path] || []) : (moduleDeps[path] || []);
  const src = fs.readFileSync(path, 'utf8');
  const result = cstPipeline.parseModuleCst(src);

  if (result.constructor && result.constructor.name === 'Left') {
    console.log('  Parse error in', path + ':', result.value0);
    return null;
  }

  const mod = result.value0;

  // Build registry from dependencies (recursively)
  // Start with library registry for non-lib modules
  let registry = isLibModule ? types.emptyRegistry : libRegistry;
  let depDecls = [];
  for (const depPath of deps) {
    const depResult = parseAndCheckModule(depPath, isLibModule);
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

  // Convert declarations from PureScript List to JS Array for extractExports
  const modDeclsArray = Array.isArray(mod.declarations) ? mod.declarations : listToArray(mod.declarations);

  // Extract exports from the checked module (using Array)
  const exports = tc.extractExports(modDeclsArray);
  const exportsWithValues = tc.addValuesToExports(exports)(env)(modDeclsArray);

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

// Library modules to compile first (provides exports for compiler modules)
// Order matters - base types first, then modules that depend on them
const libModules = [
  ['Data.Maybe', lib + 'Data/Maybe.purs'],
  ['Data.Tuple', lib + 'Data/Tuple.purs'],
  ['Data.Either', lib + 'Data/Either.purs'],
  ['Data.Char', lib + 'Data/Char.purs'],
  ['Data.List', lib + 'Data/List.purs'],
  ['Data.Array', lib + 'Data/Array.purs'],
  ['Data.String', lib + 'Data/String.purs'],
  ['Data.Map', lib + 'Data/Map.purs'],
  ['Data.Set', lib + 'Data/Set.purs'],
  ['Data.Foldable', lib + 'Data/Foldable.purs'],
];

// Compile library modules first and build libRegistry
console.log('=== Compiling Library Modules ===');
for (const [name, path] of libModules) {
  console.log('Compiling library', name + '...');
  const result = parseAndCheckModule(path, true);
  if (result) {
    // Register in libRegistry for use by compiler modules
    libRegistry = types.registerModule(libRegistry)(name)(result.exports);
    // Debug: count exports
    const valueCount = mapSize(result.exports.values);
    console.log('  Registered', name, 'with', valueCount, 'values');
  }
}

console.log('\n=== Compiling Compiler Modules ===');
for (const [name, path] of modules) {
  const code = compile(name, path);
  if (code) {
    fs.writeFileSync('./output/' + name + '.ex', code);
    // Also copy to nova_lang for running
    fs.writeFileSync('./nova_lang/lib/nova/compiler/' + name + '.ex', code);
  }
}
console.log('Done!');
