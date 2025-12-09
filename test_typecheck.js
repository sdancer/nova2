const c = require("./output/Nova.Compiler.Regenerate/index.js");
const fs = require("fs");

const libSrc = fs.readFileSync("/tmp/test_lib2.purs", "utf8");
const testSrc = fs.readFileSync("/tmp/test_nested.purs", "utf8");

console.log("=== Testing type alias import ===");
try {
  const result = c.compileWithLib(libSrc)(testSrc)();
  console.log("Result:", JSON.stringify(result, null, 2));
} catch (e) {
  console.log("Error:", e.message);
  console.log("Stack:", e.stack);
}
