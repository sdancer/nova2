const fs = require('fs');
const coreCodegen = require('../output/Nova.Compiler.CodeGenCoreErlang/index.js');
const parser = require('../output/Nova.Compiler.Parser/index.js');
const tokenizer = require('../output/Nova.Compiler.Tokenizer/index.js');

function compileToCore(path) {
  console.log('Compiling', path, 'to Core Erlang...\n');

  const src = fs.readFileSync(path, 'utf8');
  const tokens = tokenizer.tokenize(src);
  const result = parser.parseModule(tokens);

  if (result.constructor && result.constructor.name === 'Left') {
    console.log('Parse error:', result.value0);
    return null;
  }

  const mod = result.value0.value0;
  const code = coreCodegen.genModule(mod);
  return code;
}

const inputFile = process.argv[2] || './nova_lang/test/purs/SumTest.purs';
const code = compileToCore(inputFile);
if (code) {
  console.log(code);

  // Also write to file
  const outPath = inputFile.replace('.purs', '.core');
  fs.writeFileSync(outPath, code);
  console.log('\nWritten to:', outPath);
}
