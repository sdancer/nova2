const fs = require('fs');
const path = require('path');
const { parseModuleCst } = require('../output/Nova.Compiler.CstPipeline/index.js');
const { genModule } = require('../output/Nova.Compiler.CodeGenCoreErlang/index.js');

// Recursively find all .purs files
function findPursFiles(dir) {
  let results = [];
  try {
    const items = fs.readdirSync(dir);
    for (const item of items) {
      const fullPath = path.join(dir, item);
      const stat = fs.statSync(fullPath);
      if (stat.isDirectory()) {
        results = results.concat(findPursFiles(fullPath));
      } else if (item.endsWith('.purs')) {
        results.push(fullPath);
      }
    }
  } catch (e) {
    // Directory doesn't exist or not readable
  }
  return results;
}

// Convert PureScript List to JS array
function listToArray(list) {
  const arr = [];
  let node = list;
  while (node && node.value0 !== undefined) {
    arr.push(node.value0);
    node = node.value1;
  }
  return arr;
}

// Extract imports from module AST
function getImports(mod) {
  const imports = [];
  if (mod && mod.declarations) {
    const decls = listToArray(mod.declarations);
    for (const decl of decls) {
      // ImportDecl is wrapped: { value0: { moduleName: "..." } }
      if (decl && decl.value0 && decl.value0.moduleName) {
        imports.push(decl.value0.moduleName);
      }
    }
  }
  return imports;
}

// Topological sort using Kahn's algorithm
function topologicalSort(modules) {
  const moduleMap = new Map(); // name -> module info
  const inDegree = new Map();  // name -> count of unprocessed dependencies
  const dependents = new Map(); // name -> list of modules that depend on it

  // Build the graph
  for (const mod of modules) {
    const name = mod.ast.name;
    moduleMap.set(name, mod);
    inDegree.set(name, 0);
    if (!dependents.has(name)) {
      dependents.set(name, []);
    }
  }

  // Calculate in-degrees and dependents
  for (const mod of modules) {
    const name = mod.ast.name;
    const imports = getImports(mod.ast);
    let degree = 0;
    for (const imp of imports) {
      if (moduleMap.has(imp)) {
        degree++;
        dependents.get(imp).push(name);
      }
      // Imports of external modules (not in our set) don't count
    }
    inDegree.set(name, degree);
  }

  // Find all modules with no dependencies (in our set)
  const queue = [];
  for (const [name, degree] of inDegree) {
    if (degree === 0) {
      queue.push(name);
    }
  }

  // Process in topological order
  const sorted = [];
  while (queue.length > 0) {
    const name = queue.shift();
    sorted.push(moduleMap.get(name));

    for (const dependent of dependents.get(name)) {
      const newDegree = inDegree.get(dependent) - 1;
      inDegree.set(dependent, newDegree);
      if (newDegree === 0) {
        queue.push(dependent);
      }
    }
  }

  // Check for cycles
  if (sorted.length !== modules.length) {
    console.warn(`Warning: Dependency cycle detected. ${modules.length - sorted.length} modules couldn't be sorted.`);
    // Add remaining modules at the end
    for (const mod of modules) {
      if (!sorted.includes(mod)) {
        sorted.push(mod);
      }
    }
  }

  return sorted;
}

// Directories to search - NO TESTS
const searchDirs = [
  './src/Nova/Compiler',
  './lib',  // PureScript stdlib (Data.Array, Data.List, etc.)
];

let allFiles = [];
for (const dir of searchDirs) {
  allFiles = allFiles.concat(findPursFiles(dir));
}

console.log(`Found ${allFiles.length} .purs files\n`);

// Ensure output directory exists
const outDir = '/tmp/nova_core';
if (!fs.existsSync(outDir)) {
  fs.mkdirSync(outDir, { recursive: true });
}

// Phase 1: Parse all files
console.log('Phase 1: Parsing...');
const parsedModules = [];
let parseErrors = 0;
let invalidAst = 0;

for (const filePath of allFiles) {
  const name = path.basename(filePath, '.purs');
  const src = fs.readFileSync(filePath, 'utf8');
  const result = parseModuleCst(src);

  // Either: Left has value0 (error string), Right has value0 (AST)
  if (result.value0 && typeof result.value0 === 'string') {
    // console.log('Parse error for ' + name + ': ' + result.value0.substring(0, 80));
    parseErrors++;
    continue;
  }

  // Right case: result.value0 is the AST module
  const mod = result.value0;
  if (!mod || !mod.name) {
    // console.log('Invalid AST for ' + name);
    invalidAst++;
    continue;
  }

  parsedModules.push({ filePath, ast: mod });
}

console.log(`Parsed ${parsedModules.length} modules (${parseErrors} parse errors, ${invalidAst} invalid)\n`);

// Phase 2: Topological sort
console.log('Phase 2: Topological sorting...');
const sortedModules = topologicalSort(parsedModules);
console.log(`Sorted ${sortedModules.length} modules\n`);

// Phase 3: Generate code in dependency order
console.log('Phase 3: Code generation...');
let success = 0, codegenErrors = 0;

for (const { filePath, ast } of sortedModules) {
  try {
    const code = genModule(ast);
    // Use module name for output file (replace . with _)
    const outName = ast.name.replace(/\./g, '_').toLowerCase();
    const outFile = path.join(outDir, outName + '.core');
    fs.writeFileSync(outFile, code);
    console.log('Generated ' + outFile + ' (' + code.split('\n').length + ' lines)');
    success++;
  } catch (e) {
    console.log('Codegen error for ' + ast.name + ': ' + e.message);
    codegenErrors++;
  }
}

console.log(`\nResults: ${success} generated, ${parseErrors} parse errors, ${codegenErrors} codegen errors, ${invalidAst} invalid AST`);
console.log(`Total .purs files: ${allFiles.length}`);
