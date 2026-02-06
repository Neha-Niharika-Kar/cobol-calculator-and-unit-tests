#!/bin/bash
#########################################################################
# COBOL Calculator Build Script                                         #
# Compiles the calculator program and all test programs                 #
#########################################################################

set -e  # Exit on error

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "========================================"
echo "COBOL Calculator - Build Script"
echo "========================================"
echo ""

# Create necessary directories
echo "Creating build directories..."
mkdir -p build
mkdir -p reports/results
mkdir -p reports/coverage

# Check if GnuCOBOL is installed
if ! command -v cobc &> /dev/null; then
    echo -e "${RED}✗ Error: GnuCOBOL compiler (cobc) not found${NC}"
    echo "Please install GnuCOBOL: brew install gnu-cobol (macOS) or apt-get install gnucobol (Linux)"
    exit 1
fi

echo -e "${GREEN}✓ GnuCOBOL compiler found${NC}"
cobc --version | head -n 1
echo ""

# Compile main calculator program
echo "Compiling main calculator program..."
if cobc -m -o build/CALCULATOR src/CALCULATOR.cbl 2>&1 | tee build/calculator-compile.log; then
    echo -e "${GREEN}✓ CALCULATOR compiled successfully${NC}"
else
    echo -e "${RED}✗ CALCULATOR compilation failed${NC}"
    cat build/calculator-compile.log
    exit 1
fi
echo ""

# Compile test programs
echo "Compiling test programs..."
test_count=0
test_success=0
test_failed=0

for test_file in tests/unit/*.cbl; do
    if [ -f "$test_file" ]; then
        test_name=$(basename "$test_file" .cbl)
        ((test_count++))
        
        echo -n "  Compiling $test_name... "
        
        if cobc -x -o "build/$test_name" "$test_file" 2>&1 | tee "build/${test_name}-compile.log" > /dev/null; then
            echo -e "${GREEN}✓${NC}"
            ((test_success++))
        else
            echo -e "${RED}✗${NC}"
            echo "    Error details in build/${test_name}-compile.log"
            ((test_failed++))
        fi
    fi
done

echo ""
echo "========================================"
echo "Build Summary"
echo "========================================"
echo "Calculator:      ✓ Compiled"
echo "Test Programs:   $test_success/$test_count compiled successfully"

if [ $test_failed -gt 0 ]; then
    echo -e "${RED}Failed:          $test_failed test program(s)${NC}"
    echo "========================================"
    exit 1
else
    echo -e "${GREEN}Status:          BUILD SUCCESSFUL${NC}"
    echo "========================================"
    echo ""
    echo "Next step: Run './test-runner.sh' to execute tests"
    exit 0
fi

# Made with Bob
