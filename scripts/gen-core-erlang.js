#!/usr/bin/env node
const compiler = require('../output/Nova.Compiler.CodeGenCoreErlang');
const fs = require('fs');
const path = require('path');

const srcFiles = [
  'src/Nova/Compiler/Ast.purs',
  'src/Nova/Compiler/Types.purs',
  'src/Nova/Compiler/Tokenizer.purs',
  'src/Nova/Compiler/Unify.purs',
  'src/Nova/Compiler/TypeChecker.purs',
  'src/Nova/Compiler/CodeGen.purs',
  'src/Nova/Compiler/Parser.purs',
  'src/Nova/Compiler/Dependencies.purs',
  'src/Nova/Compiler/CodeGenCoreErlang.purs'
];

const parser = require('../output/Nova.Compiler.Parser/index.js');
const tokenizer = require('../output/Nova.Compiler.Tokenizer/index.js');

const BASE_DIR = path.resolve(__dirname, '..');
const outDir = path.join(BASE_DIR, 'output/core_erlang');

if (!fs.existsSync(outDir)) fs.mkdirSync(outDir, { recursive: true });

// Clean old files
fs.readdirSync(outDir).forEach(f => {
  if (f.endsWith('.core') || f.endsWith('.beam')) {
    fs.unlinkSync(path.join(outDir, f));
  }
});

srcFiles.forEach(srcFile => {
  const source = fs.readFileSync(path.join(BASE_DIR, srcFile), 'utf8');
  const tokens = tokenizer.tokenize(source);
  const parseResult = parser.parseModule(tokens);

  if (parseResult.constructor && parseResult.constructor.name === 'Left') {
    console.error('Parse error in ' + srcFile + ': ' + parseResult.value0);
    return;
  }

  const ast = parseResult.value0.value0;
  const core = compiler.genModule(ast);
  const match = core.match(/^module '([^']+)'/);
  const modName = match ? match[1] : ast.name.toLowerCase().replace(/\./g, '_');
  const outFile = path.join(outDir, modName + '.core');
  fs.writeFileSync(outFile, core);
  console.log('Generated: ' + outFile);
});
