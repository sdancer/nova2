#!/usr/bin/env node
/**
 * Regenerate script using PureScript Regenerate module
 *
 * This is a thin wrapper that provides filesystem operations to the
 * PureScript Regenerate module. All the actual compilation logic is
 * in Nova.Compiler.Regenerate.
 */

const fs = require('fs');
const path = require('path');
const regenerate = require('../output/Nova.Compiler.Regenerate/index.js');
const Maybe = require('../output/Data.Maybe/index.js');

// ============================================================================
// Filesystem Delegates
// ============================================================================

// Create filesystem operations object matching the FileSystem type
const fileSystem = {
  readFile: (filePath) => {
    try {
      if (fs.existsSync(filePath)) {
        const content = fs.readFileSync(filePath, 'utf8');
        return new Maybe.Just(content);
      }
      return Maybe.Nothing.value;
    } catch (e) {
      return Maybe.Nothing.value;
    }
  },

  writeFile: (filePath) => (content) => {
    // Ensure directory exists
    const dir = path.dirname(filePath);
    if (!fs.existsSync(dir)) {
      fs.mkdirSync(dir, { recursive: true });
    }
    fs.writeFileSync(filePath, content);
    return {}; // Unit
  },

  fileExists: (filePath) => {
    return fs.existsSync(filePath);
  },

  listFiles: (dir) => (extension) => {
    const files = [];

    function walk(currentDir) {
      if (!fs.existsSync(currentDir)) return;

      try {
        const entries = fs.readdirSync(currentDir, { withFileTypes: true });
        for (const entry of entries) {
          const fullPath = path.join(currentDir, entry.name);
          if (entry.isDirectory()) {
            walk(fullPath);
          } else if (entry.name.endsWith(extension)) {
            // Normalize to use ./ prefix like the original script
            files.push('./' + fullPath.replace(/\\/g, '/'));
          }
        }
      } catch (e) {
        // Ignore permission errors, etc.
      }
    }

    walk(dir);
    return files;
  }
};

// ============================================================================
// Main
// ============================================================================

console.log('=== Nova Compiler Regeneration (PureScript) ===\n');

// Use default config
const config = regenerate.defaultConfig;

console.log('Source dir:', config.srcBase);
console.log('Library dir:', config.libBase);
console.log('Output dir:', config.outputDir);
console.log('Target dir:', config.targetDir);
console.log('');

// Run regeneration
const result = regenerate.regenerate(fileSystem)(config);

// Print logs
console.log(regenerate.showLogs(result.logs));

// Print summary
console.log('\n=== Summary ===');
console.log('Modules compiled:', result.modulesCompiled);
console.log('Success:', result.success);
