import { execSync } from "child_process";
import { writeFileSync, unlinkSync } from "fs";
import { tmpdir } from "os";
import { join } from "path";

export const runElixirCode = (code) => () => {
  const tmpFile = join(tmpdir(), `nova_test_${Date.now()}.exs`);
  try {
    writeFileSync(tmpFile, code, "utf8");
    const result = execSync(`elixir "${tmpFile}"`, {
      encoding: "utf8",
      timeout: 10000,
    });
    return result;
  } catch (err) {
    return `ERROR: ${err.message}\n${err.stderr || ""}`;
  } finally {
    try {
      unlinkSync(tmpFile);
    } catch (_) {}
  }
};
