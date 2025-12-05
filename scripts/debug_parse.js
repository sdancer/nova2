const cstPipeline = require("../output/Nova.Compiler.CstPipeline/index.js");
const fs = require("fs");

const source = fs.readFileSync("src/Nova/Compiler/CstLexer.purs", "utf8");
const lines = source.split('\n');

// Fine-grained search
for (let endLine = 283; endLine <= 310; endLine++) {
  const partial = lines.slice(0, endLine).join('\n');
  const result = cstPipeline.parseModuleCst(partial);

  let count = 0;
  if (result.value0 && result.value0.declarations) {
    let cur = result.value0.declarations;
    while (cur && cur.value0 !== undefined) {
      count++;
      cur = cur.value1;
    }
  }

  const lineContent = lines[endLine-1] ? lines[endLine-1].substring(0, 60) : '';
  console.log(`Lines 1-${endLine}: ${count} declarations | Line ${endLine}: ${lineContent}`);
}
