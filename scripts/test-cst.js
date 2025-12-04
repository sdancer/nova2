const fs = require('fs');
const cstPipeline = require('../output/Nova.Compiler.CstPipeline/index.js');

// Test parsing a simple file first
const testSource = `
module Test where

import Prelude

add :: Int -> Int -> Int
add x y = x + y

data Maybe a = Nothing | Just a
`;

console.log('Testing CST pipeline...');
console.log('='.repeat(50));

const result = cstPipeline.parseModuleCst(testSource);

if (result.constructor && result.constructor.name === 'Left') {
  console.log('Parse error:', result.value0);
} else {
  console.log('Parse succeeded!');
  console.log('Module name:', result.value0.name);
  console.log('Declarations:', result.value0.declarations.length);
  for (const decl of result.value0.declarations) {
    if (decl.constructor) {
      console.log('  -', decl.constructor.name);
    }
  }
}

console.log('');
console.log('='.repeat(50));
console.log('Testing with real compiler file...');

// Try parsing the Ast.purs file
try {
  const astSource = fs.readFileSync('./src/Nova/Compiler/Ast.purs', 'utf8');
  const astResult = cstPipeline.parseModuleCst(astSource);

  if (astResult.constructor && astResult.constructor.name === 'Left') {
    console.log('Parse error on Ast.purs:', astResult.value0);
  } else {
    console.log('Ast.purs parsed successfully!');
    console.log('Module name:', astResult.value0.name);
    console.log('Declarations:', astResult.value0.declarations.length);
  }
} catch (err) {
  console.log('Error:', err.message);
}
