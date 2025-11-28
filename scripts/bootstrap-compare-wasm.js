#!/usr/bin/env node
// Bootstrap Comparison for WASM Code Generation
// Compares WASM output between JS (Stage 1) and BEAM/Elixir (Stage 2) compilers

const fs = require('fs');
const path = require('path');
const { execSync, spawnSync } = require('child_process');

const BASE_DIR = path.resolve(__dirname, '..');

// Source files to compile (a subset for initial testing)
const srcFiles = [
  'src/Nova/Compiler/Ast.purs',
  'src/Nova/Compiler/Types.purs',
  'src/Nova/Compiler/Tokenizer.purs',
  'src/Nova/Compiler/Unify.purs',
  'src/Nova/Compiler/TypeChecker.purs',
  'src/Nova/Compiler/CodeGen.purs',
  'src/Nova/Compiler/Parser.purs',
  'src/Nova/Compiler/Dependencies.purs',
  'src/Nova/Compiler/CodeGenWasm.purs'
];

// Load JS compiler modules
const CodeGenWasm = require('../output/Nova.Compiler.CodeGenWasm');
const Parser = require('../output/Nova.Compiler.Parser');
const Tokenizer = require('../output/Nova.Compiler.Tokenizer');

function parseModule(source) {
  const tokens = Tokenizer.tokenize(source);
  const result = Parser.parseModule(tokens);

  if (result.constructor && result.constructor.name === 'Left') {
    throw new Error(`Parse error: ${JSON.stringify(result.value0)}`);
  }

  return result.value0.value0;
}

function generateWasmJs(source) {
  const ast = parseModule(source);
  return CodeGenWasm.genModule(ast);
}

// Temporary directories
const stage1Dir = '/tmp/nova_wasm_stage1';
const stage2Dir = '/tmp/nova_wasm_stage2';

// Clean and create directories
[stage1Dir, stage2Dir].forEach(dir => {
  if (fs.existsSync(dir)) {
    fs.rmSync(dir, { recursive: true });
  }
  fs.mkdirSync(dir, { recursive: true });
});

console.log('WASM Bootstrap Comparison');
console.log('='.repeat(60));
console.log();

// Stage 1: Generate WASM using JS compiler
console.log('Stage 1: Generating WASM with JS compiler...');
const stage1Results = {};

srcFiles.forEach(srcFile => {
  const source = fs.readFileSync(path.join(BASE_DIR, srcFile), 'utf8');
  const baseName = path.basename(srcFile, '.purs');

  try {
    const wat = generateWasmJs(source);
    const outPath = path.join(stage1Dir, baseName + '.wat');
    fs.writeFileSync(outPath, wat);
    stage1Results[baseName] = { success: true, lines: wat.split('\n').length };
    console.log(`  [JS] ${baseName}: ${stage1Results[baseName].lines} lines`);
  } catch (err) {
    stage1Results[baseName] = { success: false, error: err.message };
    console.log(`  [JS] ${baseName}: ERROR - ${err.message}`);
  }
});

console.log();

// Stage 2: Generate WASM using BEAM compiler
console.log('Stage 2: Generating WASM with BEAM compiler...');

// First, regenerate Elixir compiler with CodeGenWasm
console.log('  Regenerating Elixir modules...');
try {
  execSync('node scripts/regenerate.js', { cwd: BASE_DIR, stdio: 'inherit' });
} catch (err) {
  console.error('Failed to regenerate Elixir modules');
  process.exit(1);
}

// Create Elixir script to generate WASM
const elixirScript = `
modules = [
  {"../src/Nova/Compiler/Ast.purs", "Ast"},
  {"../src/Nova/Compiler/Types.purs", "Types"},
  {"../src/Nova/Compiler/Tokenizer.purs", "Tokenizer"},
  {"../src/Nova/Compiler/Unify.purs", "Unify"},
  {"../src/Nova/Compiler/TypeChecker.purs", "TypeChecker"},
  {"../src/Nova/Compiler/CodeGen.purs", "CodeGen"},
  {"../src/Nova/Compiler/Parser.purs", "Parser"},
  {"../src/Nova/Compiler/Dependencies.purs", "Dependencies"},
  {"../src/Nova/Compiler/CodeGenWasm.purs", "CodeGenWasm"}
]

for {path, name} <- modules do
  source = File.read!(path)
  tokens = Nova.Compiler.Tokenizer.tokenize(source)
  case Nova.Compiler.Parser.parse_module(tokens) do
    {:left, err} ->
      IO.puts("ERROR #{name}: #{inspect(err)}")
    {:right, {:tuple, mod, _}} ->
      wat = Nova.Compiler.CodeGenWasm.gen_module(mod)
      out_path = "${stage2Dir}/#{name}.wat"
      File.write!(out_path, wat)
      lines = length(String.split(wat, "\\n"))
      IO.puts("OK #{name}: #{lines} lines")
  end
end
`;

const scriptPath = path.join(BASE_DIR, 'nova_lang', 'gen_wasm.exs');
fs.writeFileSync(scriptPath, elixirScript);

// Run Elixir script
const stage2Results = {};
try {
  const result = spawnSync('mix', ['run', 'gen_wasm.exs'], {
    cwd: path.join(BASE_DIR, 'nova_lang'),
    encoding: 'utf8',
    timeout: 120000
  });

  if (result.error) {
    console.error('Failed to run Elixir compiler:', result.error);
    process.exit(1);
  }

  console.log(result.stdout);
  if (result.stderr) {
    console.error(result.stderr);
  }

  // Parse results from output
  const lines = result.stdout.split('\n');
  lines.forEach(line => {
    const match = line.match(/^(OK|ERROR) (\w+): (.+)$/);
    if (match) {
      const [, status, name, rest] = match;
      if (status === 'OK') {
        const lineCount = parseInt(rest);
        stage2Results[name] = { success: true, lines: lineCount };
      } else {
        stage2Results[name] = { success: false, error: rest };
      }
    }
  });
} catch (err) {
  console.error('Failed to run Stage 2:', err.message);
}

console.log();

// Compare results
console.log('Comparing Stage 1 and Stage 2 outputs...');
console.log('-'.repeat(60));

let matches = 0;
let differs = 0;
let errors = 0;

srcFiles.forEach(srcFile => {
  const baseName = path.basename(srcFile, '.purs');
  const s1 = stage1Results[baseName];
  const s2 = stage2Results[baseName];

  if (!s1?.success || !s2?.success) {
    const s1err = s1?.success ? '' : ` (JS: ${s1?.error || 'missing'})`;
    const s2err = s2?.success ? '' : ` (BEAM: ${s2?.error || 'missing'})`;
    console.log(`  ✗ ${baseName}: ERROR${s1err}${s2err}`);
    errors++;
    return;
  }

  const file1 = path.join(stage1Dir, baseName + '.wat');
  const file2 = path.join(stage2Dir, baseName + '.wat');

  if (!fs.existsSync(file1) || !fs.existsSync(file2)) {
    console.log(`  ✗ ${baseName}: Missing output file`);
    errors++;
    return;
  }

  const content1 = fs.readFileSync(file1, 'utf8');
  const content2 = fs.readFileSync(file2, 'utf8');

  if (content1 === content2) {
    console.log(`  ✓ ${baseName}: MATCH (${s1.lines} lines)`);
    matches++;
  } else {
    console.log(`  ✗ ${baseName}: DIFFER (JS: ${s1.lines} lines, BEAM: ${s2.lines} lines)`);
    differs++;
  }
});

console.log('-'.repeat(60));
console.log();
console.log(`Summary: ${matches} match, ${differs} differ, ${errors} errors`);
console.log();

if (matches === srcFiles.length) {
  console.log('🎉 WASM Bootstrap verification PASSED!');
} else if (differs > 0 || errors > 0) {
  console.log('❌ WASM Bootstrap verification FAILED');
  process.exit(1);
}
