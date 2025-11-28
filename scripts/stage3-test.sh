#!/bin/bash
# Stage 3 Bootstrap Test
# Compile all modules using Stage 2 output and compare with Stage 1

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
NOVA_DIR="$ROOT_DIR/nova_lang"
STAGE1_DIR="$ROOT_DIR/output"
STAGE2_DIR="$STAGE1_DIR/stage2"
STAGE3_DIR="$STAGE1_DIR/stage3"

MODULES=(Ast Types Tokenizer Unify TypeChecker CodeGen Parser Dependencies)

echo "Stage 3 Bootstrap Test"
echo "======================"
echo ""

# Ensure stage3 output directory exists
mkdir -p "$STAGE3_DIR"

cd "$NOVA_DIR"

# Recompile nova_lang
echo "Compiling nova_lang..."
rm -rf _build
mix compile 2>&1 | grep -E "(Compiled|Generated|error)" || true

echo ""
echo "Compiling modules with Stage 2..."

PASS=0
FAIL=0
ERRORS=0

for mod in "${MODULES[@]}"; do
    printf "  [Stage3] Compiling %s..." "$mod"
    SRC="$ROOT_DIR/src/Nova/Compiler/$mod.purs"
    OUT="$STAGE3_DIR/$mod.ex"

    if mix run stage3_compile.exs --load-stage2 "$SRC" "$OUT" 2>&1 | grep -q "Generated"; then
        LINES=$(wc -l < "$OUT")
        printf " Generated %d lines\n" "$LINES"

        # Compare with Stage 1
        STAGE1_FILE="$STAGE1_DIR/$mod.ex"
        if diff -q "$STAGE1_FILE" "$OUT" > /dev/null 2>&1; then
            echo "    ✓ MATCH with Stage 1"
            ((PASS++))
        else
            echo "    ✗ DIFFERS from Stage 1"
            ((FAIL++))
            diff "$STAGE1_FILE" "$OUT" | head -20
        fi
    else
        echo " ERROR"
        ((ERRORS++))
    fi
done

echo ""
echo "----------------------------------------"
echo "Summary: $PASS match, $FAIL differ, $ERRORS errors"

if [ "$FAIL" -eq 0 ] && [ "$ERRORS" -eq 0 ]; then
    echo ""
    echo "🎉 Stage 3 bootstrap PASSED! The compiler can compile itself!"
    exit 0
else
    echo ""
    echo "❌ Stage 3 bootstrap FAILED"
    exit 1
fi
