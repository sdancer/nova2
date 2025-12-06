const fs = require('fs');
const codegen = require('../output/Nova.Compiler.CodeGen/index.js');
const cstPipeline = require('../output/Nova.Compiler.CstPipeline/index.js');
const types = require('../output/Nova.Compiler.Types/index.js');
const tc = require('../output/Nova.Compiler.TypeChecker/index.js');
const Maybe = require('../output/Data.Maybe/index.js');

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

// ============================================================================
// Auto-discovery of module dependencies
// ============================================================================

// Parse import statements from a PureScript source file
// Returns array of imported module names
function extractImports(source) {
  const imports = [];
  const lines = source.split('\n');
  for (const line of lines) {
    // Match: import ModuleName or import ModuleName (...)
    const match = line.match(/^import\s+([\w.]+)/);
    if (match) {
      imports.push(match[1]);
    }
  }
  return imports;
}

// Convert module name to file path
// e.g., "Nova.Compiler.Ast" -> "./src/Nova/Compiler/Ast.purs"
// e.g., "Data.List" -> "./lib/Data/List.purs"
function moduleToPath(modName, isLibContext) {
  // Library modules
  if (modName.startsWith('Data.') || modName === 'Prelude') {
    const path = lib + modName.replace(/\./g, '/') + '.purs';
    if (fs.existsSync(path)) {
      return path;
    }
  }
  // Compiler modules
  if (modName.startsWith('Nova.Compiler.')) {
    const path = base + modName.replace('Nova.Compiler.', '').replace(/\./g, '/') + '.purs';
    if (fs.existsSync(path)) {
      return path;
    }
  }
  return null; // External module, not in our project
}

// Discover dependencies for a module by parsing its imports
// Only returns dependencies that are in our project (lib/ or src/)
function discoverDependencies(path, isLibModule = false) {
  if (!fs.existsSync(path)) {
    return [];
  }
  const source = fs.readFileSync(path, 'utf8');
  const imports = extractImports(source);
  const deps = [];

  for (const modName of imports) {
    const depPath = moduleToPath(modName, isLibModule);
    if (depPath && depPath !== path) {
      deps.push(depPath);
    }
  }
  return deps;
}

// Build dependency graph automatically from all modules
function buildDependencyGraph(modulePaths, isLibModules = false) {
  const graph = {};
  for (const path of modulePaths) {
    graph[path] = discoverDependencies(path, isLibModules);
  }
  return graph;
}

// Topologically sort modules based on dependencies
function topologicalSort(graph) {
  const sorted = [];
  const visited = new Set();
  const visiting = new Set();

  function visit(path) {
    if (visited.has(path)) return;
    if (visiting.has(path)) {
      console.warn('  Warning: Circular dependency detected involving', path);
      return;
    }
    visiting.add(path);
    const deps = graph[path] || [];
    for (const dep of deps) {
      if (graph[dep] !== undefined) { // Only visit deps in our graph
        visit(dep);
      }
    }
    visiting.delete(path);
    visited.add(path);
    sorted.push(path);
  }

  for (const path of Object.keys(graph)) {
    visit(path);
  }
  return sorted;
}

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

// Dependency graphs - auto-discovered at startup
let libModuleDeps = {};
let moduleDeps = {};

// Parse and type-check a module, caching the result
function parseAndCheckModule(path, isLibModule = false) {
  if (moduleCache[path]) {
    return moduleCache[path];
  }

  // Use auto-discovered dependencies
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

  // Pass the type environment for proper arity lookups (wrapped in Just)
  const maybeEnv = new Maybe.Just(result.env);
  const code = codegen.genModuleWithEnv(maybeEnv)(result.mod);
  console.log('  Generated', code.split('\n').length, 'lines');
  return code;
}

// ============================================================================
// Discover all modules and their dependencies
// ============================================================================

// Find all .purs files in a directory
function findPursFiles(dir) {
  const files = [];
  if (!fs.existsSync(dir)) return files;

  function walk(currentDir) {
    const entries = fs.readdirSync(currentDir, { withFileTypes: true });
    for (const entry of entries) {
      const path = currentDir + '/' + entry.name;
      if (entry.isDirectory()) {
        walk(path);
      } else if (entry.name.endsWith('.purs')) {
        // Normalize path to use ./ prefix
        files.push(path.replace(/^\.\//, './'));
      }
    }
  }
  walk(dir);
  return files;
}

// Helper to get short module name from path (for display and output file naming)
function getShortName(path) {
  const match = path.match(/\/(\w+)\.purs$/);
  return match ? match[1] : path;
}

// ============================================================================
// Main compilation flow
// ============================================================================

console.log('=== Discovering Module Dependencies ===');

// Find all library and compiler module files
const libFiles = findPursFiles('./lib/Data');
const compilerFiles = findPursFiles('./src/Nova/Compiler');

console.log('Found', libFiles.length, 'library modules');
console.log('Found', compilerFiles.length, 'compiler modules');

// Auto-discover dependencies
libModuleDeps = buildDependencyGraph(libFiles, true);
moduleDeps = buildDependencyGraph(compilerFiles, false);

// Topologically sort modules
const sortedLibModules = topologicalSort(libModuleDeps);
const sortedCompilerModules = topologicalSort(moduleDeps);

console.log('\n=== Compiling Library Modules (auto-sorted) ===');
for (const path of sortedLibModules) {
  const name = getModuleName(path);
  console.log('Compiling library', name + '...');
  const deps = libModuleDeps[path] || [];
  if (deps.length > 0) {
    console.log('  Dependencies:', deps.map(d => getShortName(d)).join(', '));
  }
  const result = parseAndCheckModule(path, true);
  if (result) {
    // Register in libRegistry for use by compiler modules
    libRegistry = types.registerModule(libRegistry)(name)(result.exports);
    // Debug: count exports
    const valueCount = mapSize(result.exports.values);
    console.log('  Registered', name, 'with', valueCount, 'values');
  }
}

console.log('\n=== Compiling Compiler Modules (auto-sorted) ===');
for (const path of sortedCompilerModules) {
  const name = getShortName(path);
  const deps = moduleDeps[path] || [];
  console.log('Compiling', name + '...');
  if (deps.length > 0) {
    console.log('  Dependencies:', deps.map(d => getShortName(d)).join(', '));
  }

  const result = parseAndCheckModule(path);
  if (!result) {
    continue;
  }

  // Pass the type environment for proper arity lookups (wrapped in Just)
  const maybeEnv = new Maybe.Just(result.env);
  const code = codegen.genModuleWithEnv(maybeEnv)(result.mod);
  console.log('  Generated', code.split('\n').length, 'lines');

  fs.writeFileSync('./output/' + name + '.ex', code);
  // Also copy to nova_lang for running
  fs.writeFileSync('./nova_lang/lib/nova/compiler/' + name + '.ex', code);
}
console.log('Done!');
