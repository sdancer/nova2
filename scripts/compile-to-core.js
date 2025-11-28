const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

const coreCodegen = require('../output/Nova.Compiler.CodeGenCoreErlang/index.js');
const parser = require('../output/Nova.Compiler.Parser/index.js');
const tokenizer = require('../output/Nova.Compiler.Tokenizer/index.js');

const OUTPUT_DIR = path.join(__dirname, '..', 'output', 'core_erlang');

function compileToCore(name, sourcePath) {
  console.log(`Compiling ${name} to Core Erlang...`);

  const src = fs.readFileSync(sourcePath, 'utf8');
  const tokens = tokenizer.tokenize(src);
  const result = parser.parseModule(tokens);

  if (result.constructor && result.constructor.name === 'Left') {
    console.log(`  Parse error: ${result.value0}`);
    return null;
  }

  const mod = result.value0.value0;
  const code = coreCodegen.genModule(mod);

  // Extract module name from the generated code
  const moduleMatch = code.match(/module '([^']+)'/);
  const moduleName = moduleMatch ? moduleMatch[1] : name.toLowerCase();

  const outputPath = path.join(OUTPUT_DIR, `${moduleName}.core`);
  fs.writeFileSync(outputPath, code);
  console.log(`  Generated ${code.split('\n').length} lines -> ${outputPath}`);

  return { name, moduleName, outputPath, code };
}

function compileWithErlc(corePath) {
  const moduleName = path.basename(corePath, '.core');
  console.log(`  Compiling ${moduleName} with erlc...`);

  try {
    execSync(`erlc +from_core -o "${OUTPUT_DIR}" "${corePath}"`, {
      encoding: 'utf8',
      stdio: ['pipe', 'pipe', 'pipe']
    });
    console.log(`    ✓ Success: ${moduleName}.beam created`);
    return true;
  } catch (error) {
    console.log(`    ✗ Error: ${error.stderr || error.message}`);
    return false;
  }
}

// Ensure output directory exists
if (!fs.existsSync(OUTPUT_DIR)) {
  fs.mkdirSync(OUTPUT_DIR, { recursive: true });
}

const base = path.join(__dirname, '..', 'src', 'Nova', 'Compiler');
const modules = [
  ['Ast', path.join(base, 'Ast.purs')],
  ['Types', path.join(base, 'Types.purs')],
  ['Tokenizer', path.join(base, 'Tokenizer.purs')],
  ['Unify', path.join(base, 'Unify.purs')],
  ['TypeChecker', path.join(base, 'TypeChecker.purs')],
  ['CodeGen', path.join(base, 'CodeGen.purs')],
  ['CodeGenCoreErlang', path.join(base, 'CodeGenCoreErlang.purs')],
  ['Parser', path.join(base, 'Parser.purs')],
  ['Dependencies', path.join(base, 'Dependencies.purs')],
];

console.log('=== Generating Core Erlang ===\n');

const results = [];
for (const [name, sourcePath] of modules) {
  const result = compileToCore(name, sourcePath);
  if (result) {
    results.push(result);
  }
}

console.log('\n=== Compiling with erlc ===\n');

let successCount = 0;
let failCount = 0;

for (const result of results) {
  const success = compileWithErlc(result.outputPath);
  if (success) {
    successCount++;
  } else {
    failCount++;
  }
}

console.log(`\n=== Summary ===`);
console.log(`Success: ${successCount}, Failed: ${failCount}`);
