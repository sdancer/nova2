#!/usr/bin/env node
// Test the full Nova compiler pipeline in WASM

const fs = require('fs');
const path = require('path');
const { NovaLinker } = require('./wasm-linker.js');

async function main() {
  const linker = new NovaLinker();
  const rt = linker.runtime;

  // Load all modules
  const wasmDir = '/tmp/wasm_modules';
  const files = fs.readdirSync(wasmDir).filter(f => f.endsWith('.wat'));
  const watFiles = files.map(f => ({
    name: path.basename(f, '.wat'),
    path: path.join(wasmDir, f)
  }));

  console.log('Loading compiler modules...');
  await linker.loadModules(watFiles);
  console.log(`Loaded ${linker.modules.size} modules, ${Object.keys(linker.exports).length} exports\n`);

  // Find the tokenize and parse functions
  const tokenizerMod = linker.getModule('Tokenizer');
  const parserMod = linker.getModule('Parser');

  if (!tokenizerMod || !parserMod) {
    console.log('ERROR: Missing Tokenizer or Parser module');
    return;
  }

  const tokenize = tokenizerMod.exports.tokenize;
  const parseModule = parserMod.exports.parseModule;

  if (!tokenize) {
    console.log('ERROR: tokenize function not found');
    return;
  }

  // Test simple source code
  const testSource = `module Test where

add :: Int -> Int -> Int
add x y = x + y

main = add 1 2
`;

  console.log('=== Test Source ===');
  console.log(testSource);
  console.log('===================\n');

  // Create string in WASM memory
  const sourceId = rt.nextId++;
  rt.strings.set(sourceId, testSource);
  const sourceVal = rt.makeHeapPtr(sourceId);

  // Tokenize
  console.log('Tokenizing...');
  const start = Date.now();
  const tokensVal = tokenize(sourceVal);
  const tokenizeTime = Date.now() - start;

  // Get token array
  const tokensId = rt.unboxHeapPtr(tokensVal);
  const tokens = rt.arrays.get(tokensId) || [];
  console.log(`Tokenized in ${tokenizeTime}ms, got ${tokens.length} tokens\n`);

  // Show first few tokens
  console.log('First 10 tokens:');
  for (let i = 0; i < Math.min(10, tokens.length); i++) {
    const tok = tokens[i];
    console.log(`  [${i}] ${describeToken(rt, tok)}`);
  }
  console.log();

  // Try parsing
  if (parseModule) {
    console.log('Parsing...');
    const parseStart = Date.now();
    try {
      const result = parseModule(tokensVal);
      const parseTime = Date.now() - parseStart;
      console.log(`Parse completed in ${parseTime}ms`);

      // Check result (should be Either String (Tuple Module (Array Token)))
      const tag = rt.getTag(result);
      if (tag === 2) {
        const resId = rt.unboxHeapPtr(result);
        const res = rt.arrays.get(resId);
        if (res) {
          const ctorTag = rt.getCtorTag(res[0]);
          if (ctorTag === 0) {
            // Left = error
            console.log('Parse ERROR:', rt.getString(res[1]));
          } else {
            // Right = success
            console.log('Parse SUCCESS!');
            const tupleId = rt.unboxHeapPtr(res[1]);
            const tuple = rt.arrays.get(tupleId);
            if (tuple) {
              const moduleId = rt.unboxHeapPtr(tuple[0]);
              const mod = rt.arrays.get(moduleId);
              if (mod) {
                console.log('Module parsed successfully');
                // Try to access declarations if available
              }
            }
          }
        }
      } else {
        console.log('Parse result tag:', tag, 'value:', result);
      }
    } catch (err) {
      console.log('Parse error:', err.message);
    }
  } else {
    console.log('parseModule function not found');
  }

  // Test with a real file
  console.log('\n=== Testing with Ast.purs ===');
  const astSource = fs.readFileSync('src/Nova/Compiler/Ast.purs', 'utf8');
  console.log(`Source: ${astSource.length} chars`);

  const astSourceId = rt.nextId++;
  rt.strings.set(astSourceId, astSource);
  const astSourceVal = rt.makeHeapPtr(astSourceId);

  console.log('Tokenizing Ast.purs...');
  const astStart = Date.now();
  const astTokensVal = tokenize(astSourceVal);
  const astTokenizeTime = Date.now() - astStart;

  const astTokensId = rt.unboxHeapPtr(astTokensVal);
  const astTokens = rt.arrays.get(astTokensId) || [];
  console.log(`Tokenized in ${astTokenizeTime}ms, got ${astTokens.length} tokens`);

  if (parseModule) {
    console.log('Parsing Ast.purs...');
    const astParseStart = Date.now();
    try {
      const astResult = parseModule(astTokensVal);
      const astParseTime = Date.now() - astParseStart;

      const tag = rt.getTag(astResult);
      if (tag === 2) {
        const resId = rt.unboxHeapPtr(astResult);
        const res = rt.arrays.get(resId);
        if (res) {
          const ctorTag = rt.getCtorTag(res[0]);
          if (ctorTag === 0) {
            console.log(`Parse ERROR after ${astParseTime}ms:`, rt.getString(res[1]));
          } else {
            console.log(`Parse SUCCESS in ${astParseTime}ms!`);
          }
        }
      }
    } catch (err) {
      console.log('Parse error:', err.message);
    }
  }
}

function describeToken(rt, tok) {
  const tag = rt.getTag(tok);
  if (tag === 2) {
    const id = rt.unboxHeapPtr(tok);
    const arr = rt.arrays.get(id);
    if (arr && arr.length >= 2) {
      const ctorTag = rt.getCtorTag(arr[0]);
      const tokenTypes = ['TokLower', 'TokUpper', 'TokOperator', 'TokInt', 'TokNumber',
                          'TokString', 'TokChar', 'TokKeyword', 'TokDelimiter', 'TokNewline',
                          'TokLayoutStart', 'TokLayoutSep', 'TokLayoutEnd', 'TokEOF', 'TokError'];
      const typeName = tokenTypes[ctorTag] || `Tag${ctorTag}`;
      if (arr.length > 1) {
        const payloadTag = rt.getTag(arr[1]);
        if (payloadTag === 2) {
          const payloadId = rt.unboxHeapPtr(arr[1]);
          if (rt.strings.has(payloadId)) {
            return `${typeName}("${rt.strings.get(payloadId)}")`;
          }
        }
        return `${typeName}`;
      }
      return typeName;
    }
  }
  return `<val:${tok}>`;
}

main().catch(err => {
  console.error('Error:', err);
  process.exit(1);
});
