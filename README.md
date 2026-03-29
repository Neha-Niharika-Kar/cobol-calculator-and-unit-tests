# COBOL Calculator with Unit Testing

A comprehensive COBOL calculator program with a complete unit testing framework, demonstrating modern software development practices applied to COBOL.

## 🎯 Project Overview

This project implements a basic calculator in COBOL that performs four arithmetic operations (addition, subtraction, multiplication, and division) with comprehensive unit testing, error handling, and automated build/test scripts.

## ✨ Features

- **Four Basic Operations**: Addition, Subtraction, Multiplication, Division
- **Comprehensive Error Handling**: Division by zero, invalid operations
- **38 Unit Tests**: Covering normal cases, edge cases, and error conditions
- **Automated Build System**: Shell scripts for compilation and testing
- **Detailed Test Reports**: Automated test execution with summary reports
- **Well-Documented**: Complete documentation and implementation guides

## 📋 Prerequisites

### Required Software

1. **GnuCOBOL Compiler** (version 3.0 or higher)
   
   **macOS:**
   ```bash
   brew install gnu-cobol
   ```
   
   **Ubuntu/Debian:**
   ```bash
   sudo apt-get update
   sudo apt-get install gnucobol
   ```
   
   **Fedora/RHEL:**
   ```bash
   sudo dnf install gnucobol
   ```

2. **Bash Shell** (pre-installed on macOS/Linux)

3. **Text Editor** (VS Code with COBOL extension recommended)

### Verify Installation

```bash
cobc --version
```

Expected output: `cobc (GnuCOBOL) 3.x.x`

## 🚀 Quick Start

### 1. Clone or Navigate to Project

```bash
cd /Users/nehaniharikakar/Documents/cobol-calculator
```

### 2. Build the Project

```bash
chmod +x build.sh test-runner.sh
./build.sh
```

This will:
- Create necessary directories
- Compile the CALCULATOR program
- Compile all test programs
- Display build summary

### 3. Run Tests

```bash
./test-runner.sh
```

This will:
- Execute all unit test suites
- Display real-time test results
- Generate detailed test report
- Show final summary with success rate

### 4. Review results
- Check console output
- Review `reports/results/` for detailed logs

## 📁 Project Structure

```
cobol-calculator/
├── src/
│   └── CALCULATOR.cbl          # Main calculator program
├── tests/
│   └── unit/
│       ├── TEST-ADD.cbl        # Addition tests (8 tests)
│       ├── TEST-SUB.cbl        # Subtraction tests (7 tests)
│       ├── TEST-MUL.cbl        # Multiplication tests (8 tests)
│       ├── TEST-DIV.cbl        # Division tests (14 tests)
│       └── TEST-ERROR.cbl      # Error handling tests (4 tests)
├── test-data/
│   ├── normal-cases.txt        # Normal test cases
│   ├── edge-cases.txt          # Edge case test data
│   └── error-cases.txt         # Error condition test data
├── docs/
│   ├── cobol-calculator-test-plan.md
│   ├── test-case-specifications.md
│   ├── implementation-guide.md
│   └── PLANNING-SUMMARY.md
├── build/                      # Compiled programs (created by build.sh)
├── reports/
│   ├── results/               # Test execution logs
│   └── coverage/              # Code coverage reports
├── config/                     # Configuration files
├── build.sh                    # Build automation script
├── test-runner.sh             # Test execution script
└── README.md                   # This file
```

## 🧪 Test Coverage

### Test Distribution

| Category | Test Count | Description |
|----------|-----------|-------------|
| Addition | 8 tests | Normal and edge cases for addition |
| Subtraction | 7 tests | Normal and edge cases for subtraction |
| Multiplication | 8 tests | Normal and edge cases for multiplication |
| Division | 14 tests | Normal, edge, and error cases for division |
| Error Handling | 4 tests | Invalid operation codes |
| **Total** | **41 tests** | Comprehensive coverage |

### Test Categories

- **Normal Cases**: Standard operations with typical inputs
- **Edge Cases**: Negative numbers, large numbers, small decimals
- **Error Cases**: Division by zero, invalid operations

## 💻 Usage Examples

### Using the Calculator Program

The calculator can be called from other COBOL programs:

```cobol
01  INPUT-DATA.
    05  OPERAND-1        PIC S9(9)V99 VALUE 10.00.
    05  OPERAND-2        PIC S9(9)V99 VALUE 5.00.
    05  OPERATION        PIC X VALUE "A".

01  OUTPUT-DATA.
    05  RESULT           PIC S9(9)V99.
    05  STATUS           PIC X(20).

CALL "CALCULATOR" USING INPUT-DATA OUTPUT-DATA.
```

### Operation Codes

- `A` - Addition
- `S` - Subtraction
- `M` - Multiplication
- `D` - Division

### Status Codes

- `SUCCESS` - Operation completed successfully
- `DIVIDE-BY-ZERO` - Attempted division by zero
- `ERROR` - Invalid operation code

## 📊 Test Results

After running `./test-runner.sh`, you'll see:

```
========================================
FINAL TEST SUMMARY
========================================
Test Suites:
  Total:        5
  Passed:       5
  Failed:       0

Individual Tests:
  Total:        41
  Passed:       41
  Failed:       0
  Success Rate: 100%

Status:       ALL TESTS PASSED ✓
========================================
```

Detailed logs are saved in `reports/results/`

## 📖 Documentation

Comprehensive documentation is available in the `docs/` directory:

1. **[Test Plan](docs/cobol-calculator-test-plan.md)** - Overall testing strategy
2. **[Test Specifications](docs/test-case-specifications.md)** - Detailed test cases
3. **[Implementation Guide](docs/implementation-guide.md)** - Step-by-step implementation
4. **[Planning Summary](docs/PLANNING-SUMMARY.md)** - Project planning overview

## 🔧 Development

### Adding New Tests

1. Create a new test file in `tests/unit/`
2. Follow the existing test structure pattern
3. Update `build.sh` if needed
4. Run `./build.sh` and `./test-runner.sh`

### Modifying the Calculator

1. Edit `src/CALCULATOR.cbl`
2. Rebuild: `./build.sh`
3. Run tests: `./test-runner.sh`
4. Verify all tests still pass

## 🐛 Troubleshooting

### Build Fails

**Problem**: `cobc: command not found`
**Solution**: Install GnuCOBOL (see Prerequisites)

**Problem**: Compilation errors
**Solution**: Check COBOL syntax, verify file paths

### Tests Fail

**Problem**: Unexpected test failures
**Solution**: 
1. Check test logs in `reports/results/`
2. Verify calculator logic
3. Review test expectations

### Permission Denied

**Problem**: Cannot execute scripts
**Solution**: 
```bash
chmod +x build.sh test-runner.sh
```

## 🎓 Learning Resources

- [GnuCOBOL Documentation](https://gnucobol.sourceforge.io/)
- [COBOL Programming Guide](https://www.ibm.com/docs/en/cobol-zos)
- [Unit Testing Best Practices](docs/implementation-guide.md)

## 📄 License

This project is created for educational purposes.
- Implementation Date: 2026-02-06
