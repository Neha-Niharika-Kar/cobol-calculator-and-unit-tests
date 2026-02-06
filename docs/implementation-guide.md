# COBOL Calculator - Implementation Guide

## Overview
This guide provides detailed instructions for implementing the COBOL calculator program and its comprehensive unit test suite using GnuCOBOL and the COBOL-Unit-Test framework.

---

## 1. Prerequisites

### Required Software
- **GnuCOBOL Compiler** (version 3.0 or higher)
  - Installation: `brew install gnu-cobol` (macOS) or `apt-get install gnucobol` (Linux)
- **COBOL-Unit-Test Framework**
  - GitHub: https://github.com/neopragma/cobol-unit-test
- **Text Editor/IDE** with COBOL syntax support
  - VS Code with COBOL extension recommended

### Environment Setup
```bash
# Verify GnuCOBOL installation
cobc --version

# Set COBOL compiler flags
export COB_CFLAGS="-Wall -Wextra"
export COB_LDFLAGS="-lm"
```

---

## 2. Project Structure Setup

### Directory Layout
```
cobol-calculator/
├── src/
│   ├── CALCULATOR.cbl          # Main calculator program
│   ├── CALC-OPERATIONS.cpy     # Copybook for operations
│   └── CALC-DATA.cpy           # Copybook for data structures
├── tests/
│   ├── unit/
│   │   ├── TEST-ADD.cbl        # Addition tests
│   │   ├── TEST-SUB.cbl        # Subtraction tests
│   │   ├── TEST-MUL.cbl        # Multiplication tests
│   │   └── TEST-DIV.cbl        # Division tests
│   ├── integration/
│   │   └── TEST-SUITE.cbl      # Integration test suite
│   └── test-runner.sh          # Test execution script
├── test-data/
│   ├── normal-cases.txt        # Normal test data
│   ├── edge-cases.txt          # Edge case test data
│   └── error-cases.txt         # Error condition test data
├── config/
│   ├── test-config.conf        # Test configuration
│   └── compiler-options.conf   # Compiler settings
├── docs/
│   ├── test-plan.md           # Test plan (already created)
│   ├── test-specs.md          # Test specifications (already created)
│   └── test-results.md        # Test execution results
├── reports/
│   ├── coverage/              # Code coverage reports
│   └── results/               # Test result logs
├── build/
│   └── (compiled objects)
└── README.md                   # Project documentation
```

---

## 3. Calculator Program Architecture

### 3.1 Main Program Structure (CALCULATOR.cbl)

```
IDENTIFICATION DIVISION.
PROGRAM-ID. CALCULATOR.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 WS-OPERAND-1         PIC S9(9)V99.
    01 WS-OPERAND-2         PIC S9(9)V99.
    01 WS-OPERATION         PIC X.
    01 WS-RESULT            PIC S9(9)V99.
    01 WS-STATUS            PIC X(20).
    01 WS-ERROR-MSG         PIC X(50).

PROCEDURE DIVISION.
    MAIN-LOGIC.
        PERFORM INITIALIZE-PROGRAM
        PERFORM VALIDATE-INPUT
        PERFORM EXECUTE-OPERATION
        PERFORM DISPLAY-RESULT
        STOP RUN.

    INITIALIZE-PROGRAM.
        MOVE SPACES TO WS-STATUS
        MOVE SPACES TO WS-ERROR-MSG
        MOVE ZERO TO WS-RESULT.

    VALIDATE-INPUT.
        [Input validation logic]

    EXECUTE-OPERATION.
        EVALUATE WS-OPERATION
            WHEN 'A' PERFORM ADD-OPERATION
            WHEN 'S' PERFORM SUBTRACT-OPERATION
            WHEN 'M' PERFORM MULTIPLY-OPERATION
            WHEN 'D' PERFORM DIVIDE-OPERATION
            WHEN OTHER PERFORM INVALID-OPERATION
        END-EVALUATE.

    [Individual operation paragraphs]
```

### 3.2 Key Design Principles

1. **Modularity**: Each operation in separate paragraph
2. **Error Handling**: Comprehensive validation and error reporting
3. **Testability**: Clear input/output interfaces
4. **Maintainability**: Well-commented, structured code
5. **Reusability**: Use copybooks for shared data structures

---

## 4. Unit Test Implementation

### 4.1 Test Structure Pattern

Each test file follows this pattern:

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. TEST-ADD.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 TEST-COUNTER         PIC 9(3) VALUE 0.
    01 PASS-COUNTER         PIC 9(3) VALUE 0.
    01 FAIL-COUNTER         PIC 9(3) VALUE 0.
    
    01 TEST-INPUT.
        05 TEST-OP1         PIC S9(9)V99.
        05 TEST-OP2         PIC S9(9)V99.
        05 TEST-OPERATION   PIC X.
    
    01 TEST-EXPECTED.
        05 EXPECTED-RESULT  PIC S9(9)V99.
        05 EXPECTED-STATUS  PIC X(20).
    
    01 TEST-ACTUAL.
        05 ACTUAL-RESULT    PIC S9(9)V99.
        05 ACTUAL-STATUS    PIC X(20).

PROCEDURE DIVISION.
    MAIN-TEST-LOGIC.
        PERFORM SETUP-TEST-SUITE
        PERFORM RUN-ALL-TESTS
        PERFORM DISPLAY-TEST-SUMMARY
        STOP RUN.

    SETUP-TEST-SUITE.
        DISPLAY "Starting Addition Tests..."
        MOVE ZERO TO TEST-COUNTER
        MOVE ZERO TO PASS-COUNTER
        MOVE ZERO TO FAIL-COUNTER.

    RUN-ALL-TESTS.
        PERFORM TEST-ADD-001
        PERFORM TEST-ADD-002
        [... more tests ...]

    TEST-ADD-001.
        ADD 1 TO TEST-COUNTER
        MOVE 5 TO TEST-OP1
        MOVE 3 TO TEST-OP2
        MOVE 'A' TO TEST-OPERATION
        MOVE 8 TO EXPECTED-RESULT
        MOVE 'SUCCESS' TO EXPECTED-STATUS
        
        CALL 'CALCULATOR' USING TEST-INPUT TEST-ACTUAL
        
        PERFORM ASSERT-EQUALS.

    ASSERT-EQUALS.
        IF ACTUAL-RESULT = EXPECTED-RESULT AND
           ACTUAL-STATUS = EXPECTED-STATUS
            ADD 1 TO PASS-COUNTER
            DISPLAY "PASS: Test " TEST-COUNTER
        ELSE
            ADD 1 TO FAIL-COUNTER
            DISPLAY "FAIL: Test " TEST-COUNTER
            DISPLAY "  Expected: " EXPECTED-RESULT " " EXPECTED-STATUS
            DISPLAY "  Actual:   " ACTUAL-RESULT " " ACTUAL-STATUS
        END-IF.
```

### 4.2 Test Execution Flow

```mermaid
graph LR
    A[Start Test Suite] --> B[Setup Test Environment]
    B --> C[Load Test Data]
    C --> D[Execute Test Cases]
    D --> E[Collect Results]
    E --> F[Generate Report]
    F --> G[Cleanup]
    G --> H[End]
```

---

## 5. Test Data Management

### 5.1 Test Data File Format

**normal-cases.txt**
```
# Test ID|Operand1|Operand2|Operation|Expected Result|Expected Status
ADD-001|5|3|A|8|SUCCESS
ADD-002|10|0|A|10|SUCCESS
SUB-001|10|3|S|7|SUCCESS
MUL-001|5|3|M|15|SUCCESS
DIV-001|10|2|D|5|SUCCESS
```

**edge-cases.txt**
```
# Test ID|Operand1|Operand2|Operation|Expected Result|Expected Status
ADD-005|-5|3|A|-2|SUCCESS
SUB-005|3|10|S|-7|SUCCESS
MUL-005|-5|3|M|-15|SUCCESS
DIV-005|-10|2|D|-5|SUCCESS
```

**error-cases.txt**
```
# Test ID|Operand1|Operand2|Operation|Expected Result|Expected Status
DIV-009|10|0|D|N/A|DIVIDE-BY-ZERO
ERR-001|10|5|X|N/A|ERROR
```

### 5.2 Data Loading Strategy

```cobol
READ-TEST-DATA.
    OPEN INPUT TEST-DATA-FILE
    PERFORM UNTIL END-OF-FILE
        READ TEST-DATA-FILE INTO TEST-RECORD
        AT END
            SET END-OF-FILE TO TRUE
        NOT AT END
            PERFORM PARSE-TEST-RECORD
            PERFORM EXECUTE-TEST-CASE
        END-READ
    END-PERFORM
    CLOSE TEST-DATA-FILE.
```

---

## 6. Compilation and Build Process

### 6.1 Compilation Commands

**Compile Main Program:**
```bash
cobc -x -o calculator src/CALCULATOR.cbl
```

**Compile Test Programs:**
```bash
cobc -x -o test-add tests/unit/TEST-ADD.cbl
cobc -x -o test-sub tests/unit/TEST-SUB.cbl
cobc -x -o test-mul tests/unit/TEST-MUL.cbl
cobc -x -o test-div tests/unit/TEST-DIV.cbl
```

**Compile with Debug Info:**
```bash
cobc -x -g -debug -o calculator-debug src/CALCULATOR.cbl
```

### 6.2 Build Script (build.sh)

```bash
#!/bin/bash

echo "Building COBOL Calculator..."

# Create build directory
mkdir -p build

# Compile main program
cobc -x -o build/calculator src/CALCULATOR.cbl
if [ $? -eq 0 ]; then
    echo "✓ Calculator compiled successfully"
else
    echo "✗ Calculator compilation failed"
    exit 1
fi

# Compile test programs
for test_file in tests/unit/*.cbl; do
    test_name=$(basename "$test_file" .cbl)
    cobc -x -o "build/$test_name" "$test_file"
    if [ $? -eq 0 ]; then
        echo "✓ $test_name compiled successfully"
    else
        echo "✗ $test_name compilation failed"
        exit 1
    fi
done

echo "Build complete!"
```

---

## 7. Test Execution

### 7.1 Test Runner Script (test-runner.sh)

```bash
#!/bin/bash

echo "========================================="
echo "COBOL Calculator Test Suite"
echo "========================================="
echo ""

# Initialize counters
total_tests=0
passed_tests=0
failed_tests=0

# Run each test program
for test_prog in build/TEST-*; do
    if [ -x "$test_prog" ]; then
        echo "Running $(basename $test_prog)..."
        ./"$test_prog" > "reports/results/$(basename $test_prog).log" 2>&1
        
        # Check exit status
        if [ $? -eq 0 ]; then
            ((passed_tests++))
            echo "✓ PASSED"
        else
            ((failed_tests++))
            echo "✗ FAILED"
        fi
        ((total_tests++))
        echo ""
    fi
done

# Display summary
echo "========================================="
echo "Test Summary"
echo "========================================="
echo "Total Tests:  $total_tests"
echo "Passed:       $passed_tests"
echo "Failed:       $failed_tests"
echo "Success Rate: $(( passed_tests * 100 / total_tests ))%"
echo "========================================="

# Exit with appropriate code
if [ $failed_tests -eq 0 ]; then
    exit 0
else
    exit 1
fi
```

### 7.2 Running Tests

```bash
# Make scripts executable
chmod +x build.sh test-runner.sh

# Build project
./build.sh

# Run all tests
./test-runner.sh

# Run specific test
./build/TEST-ADD

# Run with verbose output
./test-runner.sh -v
```

---

## 8. Code Coverage Analysis

### 8.1 Coverage Tools

For GnuCOBOL, use `gcov` for coverage analysis:

```bash
# Compile with coverage flags
cobc -x -fprofile-arcs -ftest-coverage -o calculator src/CALCULATOR.cbl

# Run tests
./test-runner.sh

# Generate coverage report
gcov src/CALCULATOR.cbl

# View coverage
cat CALCULATOR.cbl.gcov
```

### 8.2 Coverage Goals

- **Statement Coverage**: 100%
- **Branch Coverage**: 95%+
- **Function Coverage**: 100%

---

## 9. Continuous Integration Setup

### 9.1 CI Pipeline (GitHub Actions Example)

```yaml
name: COBOL Calculator CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v2
    
    - name: Install GnuCOBOL
      run: |
        sudo apt-get update
        sudo apt-get install -y gnucobol
    
    - name: Build Project
      run: ./build.sh
    
    - name: Run Tests
      run: ./test-runner.sh
    
    - name: Generate Coverage
      run: |
        gcov src/CALCULATOR.cbl
        bash <(curl -s https://codecov.io/bash)
    
    - name: Archive Test Results
      uses: actions/upload-artifact@v2
      with:
        name: test-results
        path: reports/results/
```

---

## 10. Best Practices

### 10.1 Code Quality
- Use meaningful variable names
- Add comments for complex logic
- Follow COBOL naming conventions
- Keep paragraphs focused and small
- Use copybooks for shared code

### 10.2 Testing Best Practices
- Write tests before implementation (TDD)
- Test one thing per test case
- Use descriptive test names
- Keep tests independent
- Clean up test data after execution

### 10.3 Maintenance
- Update tests when code changes
- Review test coverage regularly
- Refactor tests for clarity
- Document test failures
- Version control all test artifacts

---

## 11. Troubleshooting

### Common Issues

**Issue**: Compilation errors
- **Solution**: Check COBOL syntax, verify copybook paths

**Issue**: Tests fail unexpectedly
- **Solution**: Verify test data format, check numeric precision

**Issue**: Division by zero not caught
- **Solution**: Ensure proper error handling in DIVIDE operation

**Issue**: Test results inconsistent
- **Solution**: Initialize all variables, check for data contamination

---

## 12. Next Steps After Planning

Once this plan is approved:

1. **Switch to Code Mode** to implement:
   - Calculator program structure
   - Individual operation paragraphs
   - Error handling logic
   - Test programs for each operation
   - Test data files
   - Build and test scripts

2. **Execute and Validate**:
   - Run all tests
   - Verify coverage
   - Generate reports
   - Document results

3. **Iterate and Improve**:
   - Address any failures
   - Optimize performance
   - Enhance error messages
   - Add additional test cases as needed

---

## 13. Success Metrics

The implementation will be considered successful when:

- ✓ All 38 test cases pass
- ✓ Code coverage ≥ 90%
- ✓ Zero compilation warnings
- ✓ All error conditions properly handled
- ✓ Documentation complete
- ✓ Build and test scripts functional
- ✓ CI pipeline operational
