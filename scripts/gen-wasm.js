#!/usr/bin/env node
// Generate WASM (WAT) output from Nova source files

const fs = require('fs');
const path = require('path');

// Load PureScript output
const CodeGenWasm = require('../output/Nova.Compiler.CodeGenWasm');
const Parser = require('../output/Nova.Compiler.Parser');
const Tokenizer = require('../output/Nova.Compiler.Tokenizer');

function parseModule(source) {
  const tokens = Tokenizer.tokenize(source);
  const result = Parser.parseModule(tokens);

  // Check if it's a Left (error) or Right (success)
  if (result.constructor && result.constructor.name === 'Left') {
    throw new Error(`Parse error: ${JSON.stringify(result.value0)}`);
  }

  // Right contains Tuple(module, remainingTokens)
  return result.value0.value0;
}

function generateWasm(source) {
  const ast = parseModule(source);
  return CodeGenWasm.genModule(ast);
}

// Main
const args = process.argv.slice(2);

if (args.length === 0) {
  console.log('Usage: node gen-wasm.js <source-file>');
  process.exit(1);
}

const sourceFile = args[0];
const source = fs.readFileSync(sourceFile, 'utf8');

try {
  const wat = generateWasm(source);
  console.log(wat);
} catch (err) {
  console.error('Error:', err.message);
  process.exit(1);
}
