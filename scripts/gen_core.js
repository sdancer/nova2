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

// Directories to search
const searchDirs = [
  './src/Nova/Compiler',
  './lib',  // PureScript stdlib (Data.Array, Data.List, etc.)
  './test',
  './nova_lang/purs_tests',
  './nova_lang/test/purs'
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

let success = 0, failed = 0, parseErrors = 0, codegenErrors = 0;

allFiles.forEach(filePath => {
  const name = path.basename(filePath, '.purs');
  const src = fs.readFileSync(filePath, 'utf8');
  const result = parseModuleCst(src);

  // Either: Left has value0 (error string), Right has value0 (AST)
  if (result.value0 && typeof result.value0 === 'string') {
    // console.log('Parse error for ' + name + ': ' + result.value0.substring(0, 80));
    parseErrors++;
    return;
  }

  // Right case: result.value0 is the AST module
  const mod = result.value0;
  if (!mod || !mod.name) {
    // console.log('Invalid AST for ' + name);
    failed++;
    return;
  }

  try {
    const code = genModule(mod);
    // Use module name for output file (replace . with _)
    const outName = mod.name.replace(/\./g, '_').toLowerCase();
    const outFile = path.join(outDir, outName + '.core');
    fs.writeFileSync(outFile, code);
    console.log('Generated ' + outFile + ' (' + code.split('\n').length + ' lines)');
    success++;
  } catch (e) {
    // console.log('Codegen error for ' + name + ': ' + e.message);
    codegenErrors++;
  }
});

console.log(`\nResults: ${success} generated, ${parseErrors} parse errors, ${codegenErrors} codegen errors, ${failed} other failures`);
console.log(`Total .purs files: ${allFiles.length}`);
