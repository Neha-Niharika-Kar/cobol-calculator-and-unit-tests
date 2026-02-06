#!/bin/bash
#########################################################################
# COBOL Calculator Test Runner                                          #
# Executes all unit tests and generates summary report                  #
#########################################################################

set -e  # Exit on error

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "========================================"
echo "COBOL Calculator - Test Runner"
echo "========================================"
echo ""

# Check if build directory exists
if [ ! -d "build" ]; then
    echo -e "${RED}✗ Build directory not found${NC}"
    echo "Please run './build.sh' first"
    exit 1
fi

# Check if calculator module exists (could be .dylib on macOS or .so on Linux)
if [ ! -f "build/CALCULATOR.dylib" ] && [ ! -f "build/CALCULATOR.so" ] && [ ! -f "build/CALCULATOR" ]; then
    echo -e "${RED}✗ CALCULATOR module not found${NC}"
    echo "Please run './build.sh' first"
    exit 1
fi

# Set library path for module loading
export COB_LIBRARY_PATH="./build:$COB_LIBRARY_PATH"
export DYLD_LIBRARY_PATH="./build:$DYLD_LIBRARY_PATH"
export LD_LIBRARY_PATH="./build:$LD_LIBRARY_PATH"

# Initialize counters
total_suites=0
passed_suites=0
failed_suites=0
total_tests=0
passed_tests=0
failed_tests=0

# Create reports directory
mkdir -p reports/results

# Get timestamp for report
timestamp=$(date +"%Y-%m-%d_%H-%M-%S")
report_file="reports/results/test-report-${timestamp}.txt"

# Start report
{
    echo "========================================"
    echo "COBOL Calculator Test Report"
    echo "========================================"
    echo "Date: $(date)"
    echo "========================================"
    echo ""
} > "$report_file"

# Function to run a test suite
run_test_suite() {
    local test_prog=$1
    local test_name=$(basename "$test_prog")
    
    echo -e "${BLUE}Running $test_name...${NC}"
    ((total_suites++))
    
    # Run the test and capture output
    local log_file="reports/results/${test_name}.log"
    
    if ./"$test_prog" > "$log_file" 2>&1; then
        echo -e "${GREEN}✓ $test_name PASSED${NC}"
        ((passed_suites++))
        
        # Extract test counts from log (remove leading zeros to avoid octal interpretation)
        local suite_total=$(grep "Total Tests:" "$log_file" | awk '{print $3}' | sed 's/^0*//')
        local suite_passed=$(grep "Passed:" "$log_file" | awk '{print $2}' | sed 's/^0*//')
        local suite_failed=$(grep "Failed:" "$log_file" | awk '{print $2}' | sed 's/^0*//')
        
        # Handle empty strings (convert to 0)
        suite_total=${suite_total:-0}
        suite_passed=${suite_passed:-0}
        suite_failed=${suite_failed:-0}
        
        if [ "$suite_total" -gt 0 ] 2>/dev/null; then
            ((total_tests += suite_total))
            ((passed_tests += suite_passed))
            ((failed_tests += suite_failed))
        fi
    else
        echo -e "${RED}✗ $test_name FAILED${NC}"
        ((failed_suites++))
        
        # Extract test counts even on failure (remove leading zeros)
        local suite_total=$(grep "Total Tests:" "$log_file" | awk '{print $3}' | sed 's/^0*//')
        local suite_passed=$(grep "Passed:" "$log_file" | awk '{print $2}' | sed 's/^0*//')
        local suite_failed=$(grep "Failed:" "$log_file" | awk '{print $2}' | sed 's/^0*//')
        
        # Handle empty strings (convert to 0)
        suite_total=${suite_total:-0}
        suite_passed=${suite_passed:-0}
        suite_failed=${suite_failed:-0}
        
        if [ "$suite_total" -gt 0 ] 2>/dev/null; then
            ((total_tests += suite_total))
            ((passed_tests += suite_passed))
            ((failed_tests += suite_failed))
        fi
    fi
    
    # Append to report
    cat "$log_file" >> "$report_file"
    echo "" >> "$report_file"
    echo ""
}

# Run all test programs
echo "Executing test suites..."
echo ""

for test_prog in build/TEST-*; do
    if [ -x "$test_prog" ]; then
        run_test_suite "$test_prog"
    fi
done

# Calculate success rate
if [ $total_tests -gt 0 ]; then
    success_rate=$((passed_tests * 100 / total_tests))
else
    success_rate=0
fi

# Display final summary
echo "========================================"
echo "FINAL TEST SUMMARY"
echo "========================================"
echo "Test Suites:"
echo "  Total:        $total_suites"
echo "  Passed:       $passed_suites"
echo "  Failed:       $failed_suites"
echo ""
echo "Individual Tests:"
echo "  Total:        $total_tests"
echo "  Passed:       $passed_tests"
echo "  Failed:       $failed_tests"
echo "  Success Rate: ${success_rate}%"
echo ""

if [ $failed_tests -eq 0 ] && [ $failed_suites -eq 0 ]; then
    echo -e "${GREEN}Status:       ALL TESTS PASSED ✓${NC}"
    test_status=0
else
    echo -e "${RED}Status:       SOME TESTS FAILED ✗${NC}"
    test_status=1
fi

echo "========================================"
echo ""
echo "Detailed report saved to: $report_file"
echo ""

# Append summary to report
{
    echo "========================================"
    echo "FINAL TEST SUMMARY"
    echo "========================================"
    echo "Test Suites:"
    echo "  Total:        $total_suites"
    echo "  Passed:       $passed_suites"
    echo "  Failed:       $failed_suites"
    echo ""
    echo "Individual Tests:"
    echo "  Total:        $total_tests"
    echo "  Passed:       $passed_tests"
    echo "  Failed:       $failed_tests"
    echo "  Success Rate: ${success_rate}%"
    echo ""
    if [ $test_status -eq 0 ]; then
        echo "Status:       ALL TESTS PASSED ✓"
    else
        echo "Status:       SOME TESTS FAILED ✗"
    fi
    echo "========================================"
} >> "$report_file"

exit $test_status

# Made with Bob
