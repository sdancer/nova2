// FFI implementation for FastTokenizer (JavaScript/Node.js side)
// This is a simple implementation that matches the slow tokenizer output
// The fast version will be used on the Elixir side

// Keywords list
const keywords = [
  "foreign", "module", "where", "import", "data", "type",
  "class", "instance", "let", "in", "if", "then", "else",
  "case", "of", "do", "derive", "newtype", "infixl", "infixr", "infix",
  "forall", "as", "hiding"
];

const isKeyword = (s) => keywords.includes(s);
const isAlpha = (c) => (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
const isDigit = (c) => c >= '0' && c <= '9';
const isIdentChar = (c) => isAlpha(c) || isDigit(c) || c === '_' || c === "'";
const isOperatorChar = (c) => "+-*/=<>!:.|\\&$`#@?~^%".includes(c);
const isDelimiter = (c) => "(){}[],;".includes(c);

export const tokenizeFFI = function(source) {
  const tokens = [];
  let pos = 0;
  let line = 1;
  let col = 1;

  while (pos < source.length) {
    const c = source[pos];

    // Newline
    if (c === '\n') {
      tokens.push({ token_type: "tok_newline", value: "\n", line, column: col, pos });
      pos++;
      line++;
      col = 1;
      continue;
    }

    // Whitespace
    if (c === ' ' || c === '\t' || c === '\r') {
      pos++;
      col++;
      continue;
    }

    // Single-line comment
    if (c === '-' && source[pos + 1] === '-') {
      pos += 2;
      col += 2;
      while (pos < source.length && source[pos] !== '\n') {
        pos++;
        col++;
      }
      continue;
    }

    // Block comment
    if (c === '{' && source[pos + 1] === '-') {
      let depth = 1;
      pos += 2;
      col += 2;
      while (pos < source.length && depth > 0) {
        if (source[pos] === '{' && source[pos + 1] === '-') {
          depth++;
          pos += 2;
          col += 2;
        } else if (source[pos] === '-' && source[pos + 1] === '}') {
          depth--;
          pos += 2;
          col += 2;
        } else if (source[pos] === '\n') {
          pos++;
          line++;
          col = 1;
        } else {
          pos++;
          col++;
        }
      }
      continue;
    }

    // String literal
    if (c === '"') {
      const startPos = pos;
      const startCol = col;
      pos++;
      col++;
      let value = "";
      while (pos < source.length && source[pos] !== '"') {
        if (source[pos] === '\\' && pos + 1 < source.length) {
          const esc = source[pos + 1];
          if (esc === 'n') value += '\n';
          else if (esc === 't') value += '\t';
          else if (esc === 'r') value += '\r';
          else if (esc === '\\') value += '\\';
          else if (esc === '"') value += '"';
          else value += esc;
          pos += 2;
          col += 2;
        } else {
          value += source[pos];
          pos++;
          col++;
        }
      }
      pos++; // skip closing quote
      col++;
      tokens.push({ token_type: "tok_string", value, line, column: startCol, pos: startPos });
      continue;
    }

    // Char literal
    if (c === "'") {
      const startPos = pos;
      const startCol = col;
      pos++;
      col++;
      let value = "";
      if (source[pos] === '\\') {
        const esc = source[pos + 1];
        if (esc === 'n') value = '\n';
        else if (esc === 't') value = '\t';
        else if (esc === 'r') value = '\r';
        else if (esc === '\\') value = '\\';
        else if (esc === "'") value = "'";
        else value = esc;
        pos += 2;
        col += 2;
      } else {
        value = source[pos];
        pos++;
        col++;
      }
      pos++; // skip closing quote
      col++;
      tokens.push({ token_type: "tok_char", value, line, column: startCol, pos: startPos });
      continue;
    }

    // Number
    if (isDigit(c)) {
      const startPos = pos;
      const startCol = col;
      let value = "";
      while (pos < source.length && isDigit(source[pos])) {
        value += source[pos];
        pos++;
        col++;
      }
      if (source[pos] === '.' && isDigit(source[pos + 1])) {
        value += source[pos];
        pos++;
        col++;
        while (pos < source.length && isDigit(source[pos])) {
          value += source[pos];
          pos++;
          col++;
        }
      }
      tokens.push({ token_type: "tok_number", value, line, column: startCol, pos: startPos });
      continue;
    }

    // Identifier or keyword
    if (isAlpha(c) || c === '_') {
      const startPos = pos;
      const startCol = col;
      let value = "";
      while (pos < source.length && isIdentChar(source[pos])) {
        value += source[pos];
        pos++;
        col++;
      }
      const tokenType = isKeyword(value) ? "tok_keyword" : "tok_identifier";
      tokens.push({ token_type: tokenType, value, line, column: startCol, pos: startPos });
      continue;
    }

    // Operator
    if (isOperatorChar(c)) {
      const startPos = pos;
      const startCol = col;
      let value = "";
      while (pos < source.length && isOperatorChar(source[pos])) {
        value += source[pos];
        pos++;
        col++;
      }
      tokens.push({ token_type: "tok_operator", value, line, column: startCol, pos: startPos });
      continue;
    }

    // Delimiter
    if (isDelimiter(c)) {
      tokens.push({ token_type: "tok_delimiter", value: c, line, column: col, pos });
      pos++;
      col++;
      continue;
    }

    // Unrecognized
    tokens.push({ token_type: "tok_unrecognized", value: c, line, column: col, pos });
    pos++;
    col++;
  }

  return tokens;
};
