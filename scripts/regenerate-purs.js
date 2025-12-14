#!/usr/bin/env node
/**
 * Regenerate script using PureScript modules with verbose timing
 */

const fs = require('fs');
const path = require('path');

function formatTime(ms) {
  if (ms >= 1000) return `${(ms / 1000).toFixed(2)}s`;
  return `${ms.toFixed(1)}ms`;
}

function findFiles(dir, ext, files = []) {
  if (!fs.existsSync(dir)) return files;
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const fullPath = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      findFiles(fullPath, ext, files);
    } else if (entry.name.endsWith(ext)) {
      files.push('./' + fullPath.replace(/\\/g, '/'));
    }
  }
  return files;
}

async function main() {
  console.log('=== Nova Compiler Regeneration (Verbose) ===\n');
  const totalStart = performance.now();

  // Import modules
  console.log('Loading PureScript modules...');
  let loadStart = performance.now();

  const CstPipeline = await import('../output/Nova.Compiler.CstPipeline/index.js');
  const CodeGen = await import('../output/Nova.Compiler.CodeGenCoreErlang/index.js');
  const TypeChecker = await import('../output/Nova.Compiler.TypeChecker/index.js');
  const Types = await import('../output/Nova.Compiler.Types/index.js');
  const Either = await import('../output/Data.Either/index.js');
  const DataArray = await import('../output/Data.Array/index.js');
  const DataListTypes = await import('../output/Data.List.Types/index.js');

  console.log(`Modules loaded in ${formatTime(performance.now() - loadStart)}\n`);

  const config = {
    srcBase: './src/',
    libBase: './lib/',
    outputDir: './output/',
    targetDir: './nova_lang/priv/core/'
  };

  // Find all source files
  console.log('Finding source files...');
  const libFiles = findFiles(config.libBase, '.purs');
  const srcFiles = findFiles(config.srcBase, '.purs');
  const allFiles = [...libFiles, ...srcFiles];
  console.log(`Found ${libFiles.length} lib files, ${srcFiles.length} src files\n`);

  // Extract imports from a file
  function extractImports(source) {
    const imports = [];
    for (const line of source.split('\n')) {
      const match = line.match(/^import\s+([A-Z][A-Za-z0-9.]*)/);
      if (match) imports.push(match[1]);
    }
    return imports;
  }

  // Convert module name to file path
  function moduleToPath(modName) {
    // Special case: "Prelude" maps to Nova.Prelude
    if (modName === 'Prelude') {
      return config.libBase + 'Nova/Prelude.purs';
    }
    const relPath = modName.replace(/\./g, '/') + '.purs';
    const libPath = config.libBase + relPath;
    const srcPath = config.srcBase + relPath;
    if (fs.existsSync(libPath)) return libPath;
    if (fs.existsSync(srcPath)) return srcPath;
    return null;
  }

  // Build dependency graph
  console.log('Building dependency graph...');
  let graphStart = performance.now();
  const deps = new Map();
  for (const filePath of allFiles) {
    const source = fs.readFileSync(filePath, 'utf8');
    const imports = extractImports(source);
    const depPaths = imports.map(moduleToPath).filter(p => p && p !== filePath);
    deps.set(filePath, depPaths);
  }
  console.log(`Dependency graph built in ${formatTime(performance.now() - graphStart)}\n`);

  // Topological sort
  console.log('Topological sort...');
  let sortStart = performance.now();
  const sorted = [];
  const visited = new Set();
  const visiting = new Set();

  function visit(path) {
    if (visited.has(path)) return;
    if (visiting.has(path)) return; // circular
    visiting.add(path);
    for (const dep of (deps.get(path) || [])) {
      if (deps.has(dep)) visit(dep);
    }
    visiting.delete(path);
    visited.add(path);
    sorted.push(path);
  }

  for (const path of allFiles) visit(path);
  console.log(`Sorted ${sorted.length} modules in ${formatTime(performance.now() - sortStart)}\n`);

  // Get module name from path
  function getModuleName(filePath) {
    let relative = filePath;
    if (filePath.startsWith(config.libBase)) {
      relative = filePath.slice(config.libBase.length);
    } else if (filePath.startsWith(config.srcBase)) {
      relative = filePath.slice(config.srcBase.length);
    }
    return relative.replace(/\.purs$/, '').replace(/\//g, '.');
  }

  // Compile modules
  console.log('=== Compiling Modules ===\n');
  // Start with empty registry - Nova.Prelude will be registered as "Prelude" when compiled
  // preludeExports contains only primitive operators that will be merged with Nova.Prelude
  let registry = Types.registerModule(Types.emptyRegistry)("Prelude")(Types.preludeExports);
  let compiled = 0;
  let errors = 0;

  // Helper to merge exports with primitive operators
  const DataMap = await import('../output/Data.Map.Internal/index.js');
  const DataOrd = await import('../output/Data.Ord/index.js');
  function mergeWithPrimitives(exports) {
    return {
      ...exports,
      values: DataMap.union(DataOrd.ordString)(exports.values)(Types.preludeExports.values)
    };
  }

  for (const filePath of sorted) {
    const modName = getModuleName(filePath);
    const source = fs.readFileSync(filePath, 'utf8');
    const bytes = Buffer.byteLength(source, 'utf8');

    process.stdout.write(`${modName} (${bytes} bytes)... `);
    const modStart = performance.now();

    // Phase 1: Parse
    let parseStart = performance.now();
    const parseResult = CstPipeline.parseModuleCst(source);
    const parseTime = performance.now() - parseStart;

    if (parseResult instanceof Either.Left) {
      console.log(`PARSE ERROR: ${parseResult.value0}`);
      errors++;
      continue;
    }
    const mod = parseResult.value0;

    // Phase 2: Type check
    let tcStart = performance.now();
    const declsArray = DataArray.fromFoldable(DataListTypes.foldableList)(mod.declarations);
    const tcResult = TypeChecker.checkModule(registry)(Types.emptyEnv)(declsArray);
    const tcTime = performance.now() - tcStart;

    if (tcResult instanceof Either.Left) {
      console.log(`TYPE ERROR (parse: ${formatTime(parseTime)}): ${JSON.stringify(tcResult.value0, null, 2)}`);
      errors++;
      continue;
    }
    const env = tcResult.value0;

    // Phase 3: CodeGen
    let codegenStart = performance.now();
    const codegenResult = CodeGen.genModule(mod);
    const codegenTime = performance.now() - codegenStart;

    if (codegenResult instanceof Either.Left) {
      console.log(`CODEGEN ERROR (parse: ${formatTime(parseTime)}, tc: ${formatTime(tcTime)}): ${codegenResult.value0}`);
      errors++;
      continue;
    }
    const code = codegenResult.value0;

    // Write output
    const modPath = modName.replace(/\./g, '/');
    const outputFile = config.outputDir + modPath + '.core';
    const targetFile = config.targetDir + modPath + '.core';

    const outDir = path.dirname(outputFile);
    const targetDir = path.dirname(targetFile);
    if (!fs.existsSync(outDir)) fs.mkdirSync(outDir, { recursive: true });
    if (!fs.existsSync(targetDir)) fs.mkdirSync(targetDir, { recursive: true });

    fs.writeFileSync(outputFile, code);
    fs.writeFileSync(targetFile, code);

    // Update registry
    const exports = TypeChecker.extractExports(declsArray);
    const exportsWithValues = TypeChecker.addValuesToExports(exports)(env)(declsArray);

    // When compiling Nova.Prelude, register as "Prelude" with primitive operators merged
    if (modName === 'Nova.Prelude') {
      const preludeWithPrimitives = mergeWithPrimitives(exportsWithValues);
      registry = Types.registerModule(registry)("Prelude")(preludeWithPrimitives);
      registry = Types.registerModule(registry)(modName)(exportsWithValues);
    } else {
      registry = Types.registerModule(registry)(modName)(exportsWithValues);
    }

    const totalTime = performance.now() - modStart;
    const lines = code.split('\n').length;
    console.log(`OK [parse: ${formatTime(parseTime)}, tc: ${formatTime(tcTime)}, codegen: ${formatTime(codegenTime)}, total: ${formatTime(totalTime)}] (${lines} lines)`);
    compiled++;
  }

  const totalTime = performance.now() - totalStart;
  console.log('\n=== Summary ===');
  console.log(`Compiled: ${compiled}`);
  console.log(`Errors: ${errors}`);
  console.log(`Total time: ${formatTime(totalTime)}`);
}

main().catch(err => {
  console.error('Error:', err);
  console.error(err.stack);
  process.exit(1);
});
