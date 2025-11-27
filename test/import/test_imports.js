// Test the import system
const fs = require('fs');
const path = require('path');

// Load compiled PureScript modules
const codegen = require('../../output/Nova.Compiler.CodeGen/index.js');
const parser = require('../../output/Nova.Compiler.Parser/index.js');
const tokenizer = require('../../output/Nova.Compiler.Tokenizer/index.js');
const types = require('../../output/Nova.Compiler.Types/index.js');
const tc = require('../../output/Nova.Compiler.TypeChecker/index.js');

function parseModule(src) {
  const tokens = tokenizer.tokenize(src);
  const result = parser.parseModule(tokens);
  // result is Either String (Tuple Module (Array Token))
  // Left (error string) has tag = "Left", value0 = error
  // Right (success) has tag = "Right", value0 = Tuple
  if (result.constructor && result.constructor.name === 'Left') {
    throw new Error('Parse error: ' + result.value0);
  }
  // For Right, value0 is the tuple {value0: module, value1: remaining tokens}
  // But PureScript Tuple is {value0, value1}
  const tuple = result.value0 || result;
  return tuple.value0;
}

// Test 1: Parse ModuleA
console.log('=== Test 1: Parse ModuleA ===');
const moduleASrc = fs.readFileSync(path.join(__dirname, 'ModuleA.purs'), 'utf8');
const moduleA = parseModule(moduleASrc);
console.log('ModuleA parsed:', moduleA.name);
console.log('Declarations:', moduleA.declarations.length);

// Type check ModuleA
console.log('\n=== Test 2: Type check ModuleA ===');
const envA = types.emptyEnv;
const checkA = tc.checkModule(envA)(moduleA.declarations);
// Either TCError Env - Left has error, Right has env
if (checkA.constructor && checkA.constructor.name === 'Left') {
  console.log('Type error:', JSON.stringify(checkA.value0));
  process.exit(1);
}
console.log('ModuleA type checked successfully');

// Extract exports from ModuleA
console.log('\n=== Test 3: Extract exports from ModuleA ===');
const exportsA = tc.extractExports(moduleA.declarations);
console.log('Types exported:', Object.keys(exportsA.types).length);
console.log('Constructors exported:', Object.keys(exportsA.constructors).length);
console.log('Type aliases exported:', Object.keys(exportsA.typeAliases).length);

// Show what was exported
console.log('Types:', Object.keys(exportsA.types));
console.log('Constructors:', Object.keys(exportsA.constructors));
console.log('Type aliases:', Object.keys(exportsA.typeAliases));

// Add function types to exports after type checking
const finalEnvA = checkA.value0;  // Right's value0 is the Env
const fullExportsA = tc.addValuesToExports(exportsA)(finalEnvA)(moduleA.declarations);
console.log('Values exported:', Object.keys(fullExportsA.values).length);
console.log('Values:', Object.keys(fullExportsA.values));

// Build registry
console.log('\n=== Test 4: Build registry ===');
let registry = types.emptyRegistry;
registry = types.registerModule(registry)('ModuleA')(fullExportsA);
console.log('Registry has ModuleA:', types.lookupModule(registry)('ModuleA') !== null);

// Parse ModuleB
console.log('\n=== Test 5: Parse ModuleB ===');
const moduleBSrc = fs.readFileSync(path.join(__dirname, 'ModuleB.purs'), 'utf8');
const moduleB = parseModule(moduleBSrc);
console.log('ModuleB parsed:', moduleB.name);
console.log('Declarations:', moduleB.declarations.length);

// Type check ModuleB with registry
console.log('\n=== Test 6: Type check ModuleB with imports ===');
const envB = types.emptyEnv;
const checkB = tc.checkModuleWithRegistry(registry)(envB)(moduleB.declarations);
if (checkB.constructor && checkB.constructor.name === 'Left') {
  console.log('Type error:', JSON.stringify(checkB.value0, null, 2));
  process.exit(1);
}
console.log('ModuleB type checked successfully with imports!');

// Generate code
console.log('\n=== Test 7: Generate Elixir code ===');
const codeA = codegen.genModule(moduleA);
const codeB = codegen.genModule(moduleB);
console.log('ModuleA code lines:', codeA.split('\n').length);
console.log('ModuleB code lines:', codeB.split('\n').length);

console.log('\n=== ModuleA.ex ===');
console.log(codeA);

console.log('\n=== ModuleB.ex ===');
console.log(codeB);

console.log('\n✓ All tests passed!');
