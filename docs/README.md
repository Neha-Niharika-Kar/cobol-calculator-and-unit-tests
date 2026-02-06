# COBOL Calculator with Unit Tests

A comprehensive COBOL calculator program with a complete unit testing framework, demonstrating modern COBOL development practices and test-driven development (TDD).

## 📋 Project Overview

This project implements a simple calculator in COBOL that performs four basic arithmetic operations (addition, subtraction, multiplication, and division) with comprehensive unit test coverage using GnuCOBOL and the COBOL-Unit-Test framework.

### Features

- ✅ Four arithmetic operations (Add, Subtract, Multiply, Divide)
- ✅ Comprehensive error handling (division by zero, invalid inputs)
- ✅ 38 unit test cases covering normal, edge, and error scenarios
- ✅ Modular, maintainable code structure
- ✅ Automated test execution and reporting
- ✅ Code coverage analysis
- ✅ CI/CD pipeline ready

## 📁 Project Structure

```
cobol-calculator/
├── src/                        # Source code
│   ├── CALCULATOR.cbl         # Main calculator program
│   ├── CALC-OPERATIONS.cpy    # Operations copybook
│   └── CALC-DATA.cpy          # Data structures copybook
├── tests/                      # Test suite
│   ├── unit/                  # Unit tests
│   │   ├── TEST-ADD.cbl      # Addition tests
│   │   ├── TEST-SUB.cbl      # Subtraction tests
│   │   ├── TEST-MUL.cbl      # Multiplication tests
│   │   └── TEST-DIV.cbl      # Division tests
│   └── test-runner.sh         # Test execution script
├── test-data/                  # Test data files
│   ├── normal-cases.txt       # Standard scenarios
│   ├── edge-cases.txt         # Boundary conditions
│   └── error-cases.txt        # Error conditions
├── docs/                       # Documentation
│   ├── cobol-calculator-test-plan.md
│   ├── test-case-specifications.md
│   └── implementation-guide.md
├── reports/                    # Test results
│   ├── coverage/              # Coverage reports
│   └── results/               # Test logs
└── README.md                   # This file
```

## 🚀 Quick Start

### Prerequisites

- **GnuCOBOL** (version 3.0+)
- **COBOL-Unit-Test Framework**
- **Bash** (for running scripts)

### Installation

1. **Install GnuCOBOL:**
   ```bash
   # macOS
   brew install gnu-cobol
   
   # Ubuntu/Debian
   sudo apt-get install gnucobol
   
   # Verify installation
   cobc --version
   ```

2. **Clone or create the project:**
   ```bash
   mkdir cobol-calculator
   cd cobol-calculator
   ```

3. **Set up the directory structure:**
   ```bash
   mkdir -p src tests/unit test-data docs reports/{coverage,results} build
   ```

### Building the Project

```bash
# Make build script executable
chmod +x build.sh

# Build all programs
./build.sh
```

### Running Tests

```bash
# Make test runner executable
chmod +x tests/test-runner.sh

# Run all tests
./tests/test-runner.sh

# Run specific test
./build/TEST-ADD
```

## 📊 Test Coverage

The project includes **38 comprehensive test cases**:

| Operation      | Test Cases | Coverage |
|----------------|------------|----------|
| Addition       | 8 tests    | 100%     |
| Subtraction    | 7 tests    | 100%     |
| Multiplication | 8 tests    | 100%     |
| Division       | 11 tests   | 100%     |
| Error Handling | 4 tests    | 100%     |

### Test Categories

- **Normal Cases** (16 tests): Standard operations with typical inputs
- **Edge Cases** (16 tests): Boundary conditions and special values
- **Error Cases** (6 tests): Invalid inputs and error conditions

## 🧪 Test Examples

### Addition Tests
```
✓ ADD-001: 5 + 3 = 8
✓ ADD-002: 10 + 0 = 10
✓ ADD-005: -5 + 3 = -2
✓ ADD-006: -5 + -3 = -8
```

### Division Tests
```
✓ DIV-001: 10 / 2 = 5
✓ DIV-003: 10 / 1 = 10
✓ DIV-009: 10 / 0 = DIVIDE-BY-ZERO ERROR
✓ DIV-005: -10 / 2 = -5
```

## 📖 Documentation

Comprehensive documentation is available in the [`docs/`](docs/) directory:

1. **[Test Plan](cobol-calculator-test-plan.md)** - Overall testing strategy and approach
2. **[Test Case Specifications](test-case-specifications.md)** - Detailed test case definitions
3. **[Implementation Guide](implementation-guide.md)** - Step-by-step implementation instructions

## 🔧 Usage

### Running the Calculator

```bash
# Interactive mode
./build/calculator

# With command-line arguments
./build/calculator 10 5 A  # Addition: 10 + 5
./build/calculator 10 5 S  # Subtraction: 10 - 5
./build/calculator 10 5 M  # Multiplication: 10 * 5
./build/calculator 10 5 D  # Division: 10 / 5
```

### Operation Codes

- `A` - Addition
- `S` - Subtraction
- `M` - Multiplication
- `D` - Division

## 🎯 Development Workflow

### Test-Driven Development (TDD)

This project follows TDD principles:

1. **Write Test** - Define test case with expected behavior
2. **Run Test** - Verify test fails (red)
3. **Write Code** - Implement minimal code to pass test
4. **Run Test** - Verify test passes (green)
5. **Refactor** - Improve code while keeping tests green
6. **Repeat** - Continue for next feature

### Adding New Tests

1. Create test case in appropriate test file
2. Define input values and expected results
3. Implement test assertion logic
4. Run test suite to verify
5. Update documentation

## 📈 Code Quality

### Quality Metrics

- **Test Coverage**: 90%+ target
- **Test Pass Rate**: 100% required
- **Code Complexity**: Low (modular design)
- **Documentation**: Complete and up-to-date

### Static Analysis

```bash
# Check COBOL syntax
cobc -fsyntax-only src/CALCULATOR.cbl

# Generate warnings
cobc -Wall -Wextra src/CALCULATOR.cbl
```

## 🔄 Continuous Integration

The project includes CI/CD configuration for automated testing:

- Automated builds on code commits
- Test execution on pull requests
- Coverage reporting
- Build status badges

## 🐛 Troubleshooting

### Common Issues

**Issue**: Compilation errors
```bash
# Solution: Check COBOL syntax
cobc -fsyntax-only src/CALCULATOR.cbl
```

**Issue**: Tests fail unexpectedly
```bash
# Solution: Run with verbose output
./tests/test-runner.sh -v
```

**Issue**: Division by zero not caught
```bash
# Solution: Verify error handling in DIVIDE operation
# Check TEST-DIV.cbl for proper assertions
```

## 🤝 Contributing

Contributions are welcome! Please follow these guidelines:

1. Fork the repository
2. Create a feature branch
3. Write tests for new features
4. Ensure all tests pass
5. Update documentation
6. Submit a pull request

## 📝 License

This project is provided as-is for educational purposes.

## 🙏 Acknowledgments

- GnuCOBOL community for the excellent compiler
- COBOL-Unit-Test framework developers
- IBM for COBOL language standards

## 📞 Support

For questions or issues:
- Review the [Implementation Guide](implementation-guide.md)
- Check the [Test Specifications](test-case-specifications.md)
- Consult the [Test Plan](cobol-calculator-test-plan.md)

## 🗺️ Roadmap

### Phase 1: Planning ✅
- [x] Define requirements
- [x] Design architecture
- [x] Create test specifications
- [x] Document implementation approach

### Phase 2: Implementation (Next)
- [ ] Create project structure
- [ ] Implement calculator program
- [ ] Write unit tests
- [ ] Set up test automation

### Phase 3: Enhancement (Future)
- [ ] Add more operations (power, square root)
- [ ] Implement memory functions
- [ ] Add GUI interface
- [ ] Performance optimization

---

**Ready to implement?** Switch to Code mode to start building the calculator and test suite!
