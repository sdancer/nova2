#!/usr/bin/env node
// Test lexer performance on all .purs files
// Usage: node scripts/test_lex_perf.js

const fs = require('fs');
const path = require('path');

function findFiles(dir, ext, files = []) {
  if (!fs.existsSync(dir)) return files;
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const fullPath = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      findFiles(fullPath, ext, files);
    } else if (entry.name.endsWith(ext)) {
      files.push(fullPath);
    }
  }
  return files;
}

function formatTime(ms) {
  if (ms >= 1000) return `${(ms / 1000).toFixed(2)}s`;
  return `${ms.toFixed(1)}ms`;
}

async function main() {
  console.log('=== Lexer Performance Test (JS) ===\n');

  // Import the lexer module
  const CstLexer = await import('../output/Nova.Compiler.CstLexer/index.js');

  // Find all .purs files
  const srcFiles = findFiles('./src', '.purs');
  const libFiles = findFiles('./lib', '.purs');
  const allFiles = [...srcFiles, ...libFiles];

  console.log(`Found ${allFiles.length} .purs files\n`);

  let totalBytes = 0;
  let totalTokens = 0;
  let totalLexTime = 0;

  for (const filePath of allFiles) {
    const source = fs.readFileSync(filePath, 'utf8');
    const bytes = Buffer.byteLength(source, 'utf8');

    // Time lexing
    const startTime = performance.now();
    const tokens = CstLexer.lexModule(source);
    const endTime = performance.now();

    const lexTime = endTime - startTime;
    const tokenCount = tokens.length;

    console.log(filePath);
    console.log(`  ${bytes} bytes, ${tokenCount} tokens`);
    console.log(`  Lex: ${formatTime(lexTime)}`);

    totalBytes += bytes;
    totalTokens += tokenCount;
    totalLexTime += lexTime;
  }

  console.log('\n=== Summary ===');
  console.log(`Total files: ${allFiles.length}`);
  console.log(`Total bytes: ${totalBytes}`);
  console.log(`Total tokens: ${totalTokens}`);
  console.log(`Total lex time: ${formatTime(totalLexTime)}`);
  console.log(`Lex rate: ${Math.round(totalBytes / (totalLexTime / 1000))} bytes/sec`);
}

main().catch(err => {
  console.error('Error:', err);
  process.exit(1);
});
