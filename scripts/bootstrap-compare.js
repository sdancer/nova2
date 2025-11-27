#!/usr/bin/env node
/**
 * Bootstrap Comparison Script
 *
 * Stage 1: Compile PureScript source using the PureScript compiler (spago build)
 *          Then use the compiled JS to generate Elixir output
 *
 * Stage 2: Use the Stage 1 Elixir output (via Mix) to compile the same PureScript source
 *          This produces Stage 2 Elixir output
 *
 * Compare: The Stage 1 and Stage 2 outputs should be identical (bootstrap verification)
 */

const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

const BASE_DIR = path.resolve(__dirname, '..');
const STAGE1_DIR = path.join(BASE_DIR, 'output', 'stage1');
const STAGE2_DIR = path.join(BASE_DIR, 'output', 'stage2');
const NOVA_LANG_DIR = path.join(BASE_DIR, 'nova_lang');

const codegen = require('../output/Nova.Compiler.CodeGen/index.js');
const parser = require('../output/Nova.Compiler.Parser/index.js');
const tokenizer = require('../output/Nova.Compiler.Tokenizer/index.js');
const types = require('../output/Nova.Compiler.Types/index.js');
const tc = require('../output/Nova.Compiler.TypeChecker/index.js');

const MODULES = [
  { name: 'Ast', path: 'src/Nova/Compiler/Ast.purs', deps: [] },
  { name: 'Types', path: 'src/Nova/Compiler/Types.purs', deps: ['src/Nova/Compiler/Ast.purs'] },
  { name: 'Tokenizer', path: 'src/Nova/Compiler/Tokenizer.purs', deps: [] },
  { name: 'Unify', path: 'src/Nova/Compiler/Unify.purs', deps: ['src/Nova/Compiler/Types.purs'] },
  { name: 'TypeChecker', path: 'src/Nova/Compiler/TypeChecker.purs', deps: ['src/Nova/Compiler/Ast.purs', 'src/Nova/Compiler/Types.purs', 'src/Nova/Compiler/Unify.purs'] },
  { name: 'CodeGen', path: 'src/Nova/Compiler/CodeGen.purs', deps: ['src/Nova/Compiler/Ast.purs'] },
  { name: 'Parser', path: 'src/Nova/Compiler/Parser.purs', deps: ['src/Nova/Compiler/Ast.purs', 'src/Nova/Compiler/Tokenizer.purs'] },
  { name: 'Dependencies', path: 'src/Nova/Compiler/Dependencies.purs', deps: ['src/Nova/Compiler/Ast.purs'] },
];

function ensureDir(dir) {
  if (!fs.existsSync(dir)) {
    fs.mkdirSync(dir, { recursive: true });
  }
}

function compileWithJS(name, sourcePath, deps) {
  console.log(`  [JS] Compiling ${name}...`);

  // Parse deps first
  let depDecls = [];
  for (const depPath of deps) {
    const src = fs.readFileSync(path.join(BASE_DIR, depPath), 'utf8');
    const tokens = tokenizer.tokenize(src);
    const result = parser.parseModule(tokens);
    if (!(result.constructor && result.constructor.name === 'Left')) {
      depDecls = depDecls.concat(result.value0.value0.declarations);
    }
  }

  const src = fs.readFileSync(path.join(BASE_DIR, sourcePath), 'utf8');
  const tokens = tokenizer.tokenize(src);
  const result = parser.parseModule(tokens);

  if (result.constructor && result.constructor.name === 'Left') {
    console.log(`    Parse error: ${result.value0}`);
    return null;
  }

  const mod = result.value0.value0;
  const allDecls = depDecls.concat(mod.declarations);
  const check = tc.checkModule(types.emptyEnv)(allDecls);

  if (check.constructor && check.constructor.name === 'Left') {
    console.log(`    Type error: ${JSON.stringify(check.value0)}`);
    return null;
  }

  const code = codegen.genModule(mod);
  console.log(`    Generated ${code.split('\n').length} lines`);
  return code;
}

function compileWithElixir(name, sourcePath, outputPath) {
  console.log(`  [Elixir] Compiling ${name}...`);

  const absSourcePath = path.join(BASE_DIR, sourcePath);
  const absOutputPath = outputPath;

  try {
    const result = execSync(
      `mix run stage2_compile.exs "${absSourcePath}" "${absOutputPath}"`,
      {
        cwd: NOVA_LANG_DIR,
        encoding: 'utf8',
        stdio: ['pipe', 'pipe', 'pipe'],
        maxBuffer: 10 * 1024 * 1024
      }
    );
    // Read the generated file
    if (fs.existsSync(absOutputPath)) {
      const code = fs.readFileSync(absOutputPath, 'utf8');
      console.log(`    Generated ${code.split('\n').length} lines`);
      return code;
    }
    return null;
  } catch (error) {
    // Check stderr for the "Generated" message
    if (error.stderr && error.stderr.includes('Generated')) {
      if (fs.existsSync(absOutputPath)) {
        const code = fs.readFileSync(absOutputPath, 'utf8');
        console.log(`    Generated ${code.split('\n').length} lines`);
        return code;
      }
    }
    console.log(`    Error: ${error.stderr || error.message}`);
    return null;
  }
}

function compareOutputs(name, stage1, stage2) {
  if (stage1 === null || stage2 === null) {
    return { name, status: 'error', message: 'One or both stages failed' };
  }

  // Normalize whitespace for comparison
  const norm1 = stage1.trim().replace(/\r\n/g, '\n');
  const norm2 = stage2.trim().replace(/\r\n/g, '\n');

  if (norm1 === norm2) {
    return { name, status: 'match', lines: norm1.split('\n').length };
  } else {
    // Find first difference
    const lines1 = norm1.split('\n');
    const lines2 = norm2.split('\n');
    let firstDiff = -1;
    for (let i = 0; i < Math.max(lines1.length, lines2.length); i++) {
      if (lines1[i] !== lines2[i]) {
        firstDiff = i + 1;
        break;
      }
    }
    return {
      name,
      status: 'differ',
      firstDiffLine: firstDiff,
      stage1Lines: lines1.length,
      stage2Lines: lines2.length,
      stage1Sample: (lines1[firstDiff - 1] || '<missing>').substring(0, 80),
      stage2Sample: (lines2[firstDiff - 1] || '<missing>').substring(0, 80)
    };
  }
}

async function main() {
  console.log('Bootstrap Comparison Script');
  console.log('===========================\n');

  // Ensure output directories exist
  ensureDir(STAGE1_DIR);
  ensureDir(STAGE2_DIR);

  // Check if nova_lang is compiled
  console.log('Checking nova_lang compilation...');
  try {
    execSync(`cd "${NOVA_LANG_DIR}" && mix compile`, { encoding: 'utf8', stdio: 'pipe' });
    console.log('  nova_lang compiled successfully\n');
  } catch (error) {
    console.log('  Error compiling nova_lang:', error.message);
    console.log('  Please ensure nova_lang compiles before running this script.\n');
    process.exit(1);
  }

  const results = [];

  // Stage 1: Compile with JS (PureScript-compiled)
  console.log('Stage 1: Compiling with PureScript/JS compiler...');
  for (const mod of MODULES) {
    const code = compileWithJS(mod.name, mod.path, mod.deps);
    if (code) {
      fs.writeFileSync(path.join(STAGE1_DIR, `${mod.name}.ex`), code);
    }
  }
  console.log('');

  // Stage 2: Compile with Elixir (self-hosted)
  console.log('Stage 2: Compiling with Elixir/BEAM compiler...');
  for (const mod of MODULES) {
    const outputPath = path.join(STAGE2_DIR, `${mod.name}.ex`);
    const code = compileWithElixir(mod.name, mod.path, outputPath);
    // Code is already written by the Elixir script
  }
  console.log('');

  // Compare outputs
  console.log('Comparing Stage 1 and Stage 2 outputs...');
  console.log('-'.repeat(60));

  for (const mod of MODULES) {
    const stage1Path = path.join(STAGE1_DIR, `${mod.name}.ex`);
    const stage2Path = path.join(STAGE2_DIR, `${mod.name}.ex`);

    const stage1 = fs.existsSync(stage1Path) ? fs.readFileSync(stage1Path, 'utf8') : null;
    const stage2 = fs.existsSync(stage2Path) ? fs.readFileSync(stage2Path, 'utf8') : null;

    const result = compareOutputs(mod.name, stage1, stage2);
    results.push(result);

    if (result.status === 'match') {
      console.log(`  ✓ ${mod.name}: MATCH (${result.lines} lines)`);
    } else if (result.status === 'differ') {
      console.log(`  ✗ ${mod.name}: DIFFER at line ${result.firstDiffLine}`);
      console.log(`      Stage 1 (${result.stage1Lines} lines): ${result.stage1Sample}...`);
      console.log(`      Stage 2 (${result.stage2Lines} lines): ${result.stage2Sample}...`);
    } else {
      console.log(`  ? ${mod.name}: ${result.message}`);
    }
  }

  console.log('-'.repeat(60));

  // Summary
  const matches = results.filter(r => r.status === 'match').length;
  const differs = results.filter(r => r.status === 'differ').length;
  const errors = results.filter(r => r.status === 'error').length;

  console.log(`\nSummary: ${matches} match, ${differs} differ, ${errors} errors`);

  if (matches === MODULES.length) {
    console.log('\n🎉 Bootstrap verification PASSED! Stage 1 and Stage 2 outputs are identical.');
    process.exit(0);
  } else {
    console.log('\n⚠️  Bootstrap verification INCOMPLETE. Some outputs differ or failed.');
    process.exit(1);
  }
}

main().catch(err => {
  console.error('Fatal error:', err);
  process.exit(1);
});
