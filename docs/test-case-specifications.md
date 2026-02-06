# COBOL Calculator - Test Case Specifications

## Document Information
- **Project**: COBOL Calculator Unit Testing
- **Version**: 1.0
- **Date**: 2026-02-06
- **Test Framework**: GnuCOBOL with COBOL-Unit-Test

## Test Case Template

Each test case follows this structure:
- **Test ID**: Unique identifier
- **Test Name**: Descriptive name
- **Category**: Normal/Edge/Error
- **Operation**: ADD/SUB/MUL/DIV
- **Input**: Operand1, Operand2, Operation
- **Expected Output**: Result value
- **Expected Status**: SUCCESS/ERROR/DIVIDE-BY-ZERO
- **Priority**: High/Medium/Low

---

## 1. Addition Operation Test Cases

### 1.1 Normal Cases

#### ADD-001: Basic Positive Addition
- **Test Name**: Add two positive integers
- **Category**: Normal
- **Operation**: ADD
- **Input**: 
  - Operand1: 5
  - Operand2: 3
  - Operation: A
- **Expected Output**: 8
- **Expected Status**: SUCCESS
- **Priority**: High

#### ADD-002: Addition with Zero
- **Test Name**: Add zero to a number
- **Category**: Normal
- **Operation**: ADD
- **Input**: 
  - Operand1: 10
  - Operand2: 0
  - Operation: A
- **Expected Output**: 10
- **Expected Status**: SUCCESS
- **Priority**: High

#### ADD-003: Decimal Addition
- **Test Name**: Add two decimal numbers
- **Category**: Normal
- **Operation**: ADD
- **Input**: 
  - Operand1: 5.5
  - Operand2: 3.3
  - Operation: A
- **Expected Output**: 8.8
- **Expected Status**: SUCCESS
- **Priority**: High

#### ADD-004: Zero Plus Zero
- **Test Name**: Add zero to zero
- **Category**: Normal
- **Operation**: ADD
- **Input**: 
  - Operand1: 0
  - Operand2: 0
  - Operation: A
- **Expected Output**: 0
- **Expected Status**: SUCCESS
- **Priority**: Medium

### 1.2 Edge Cases

#### ADD-005: Negative Number Addition
- **Test Name**: Add negative and positive numbers
- **Category**: Edge
- **Operation**: ADD
- **Input**: 
  - Operand1: -5
  - Operand2: 3
  - Operation: A
- **Expected Output**: -2
- **Expected Status**: SUCCESS
- **Priority**: High

#### ADD-006: Two Negative Numbers
- **Test Name**: Add two negative numbers
- **Category**: Edge
- **Operation**: ADD
- **Input**: 
  - Operand1: -5
  - Operand2: -3
  - Operation: A
- **Expected Output**: -8
- **Expected Status**: SUCCESS
- **Priority**: High

#### ADD-007: Large Numbers
- **Test Name**: Add large numbers
- **Category**: Edge
- **Operation**: ADD
- **Input**: 
  - Operand1: 999999
  - Operand2: 1
  - Operation: A
- **Expected Output**: 1000000
- **Expected Status**: SUCCESS
- **Priority**: Medium

#### ADD-008: Very Small Decimals
- **Test Name**: Add very small decimal numbers
- **Category**: Edge
- **Operation**: ADD
- **Input**: 
  - Operand1: 0.001
  - Operand2: 0.002
  - Operation: A
- **Expected Output**: 0.003
- **Expected Status**: SUCCESS
- **Priority**: Low

---

## 2. Subtraction Operation Test Cases

### 2.1 Normal Cases

#### SUB-001: Basic Subtraction
- **Test Name**: Subtract smaller from larger
- **Category**: Normal
- **Operation**: SUB
- **Input**: 
  - Operand1: 10
  - Operand2: 3
  - Operation: S
- **Expected Output**: 7
- **Expected Status**: SUCCESS
- **Priority**: High

#### SUB-002: Subtract from Zero
- **Test Name**: Subtract number from zero
- **Category**: Normal
- **Operation**: SUB
- **Input**: 
  - Operand1: 0
  - Operand2: 5
  - Operation: S
- **Expected Output**: -5
- **Expected Status**: SUCCESS
- **Priority**: High

#### SUB-003: Subtract Zero
- **Test Name**: Subtract zero from number
- **Category**: Normal
- **Operation**: SUB
- **Input**: 
  - Operand1: 10
  - Operand2: 0
  - Operation: S
- **Expected Output**: 10
- **Expected Status**: SUCCESS
- **Priority**: High

#### SUB-004: Decimal Subtraction
- **Test Name**: Subtract decimal numbers
- **Category**: Normal
- **Operation**: SUB
- **Input**: 
  - Operand1: 10.5
  - Operand2: 3.2
  - Operation: S
- **Expected Output**: 7.3
- **Expected Status**: SUCCESS
- **Priority**: High

### 2.2 Edge Cases

#### SUB-005: Result is Negative
- **Test Name**: Subtract larger from smaller
- **Category**: Edge
- **Operation**: SUB
- **Input**: 
  - Operand1: 3
  - Operand2: 10
  - Operation: S
- **Expected Output**: -7
- **Expected Status**: SUCCESS
- **Priority**: High

#### SUB-006: Negative Operands
- **Test Name**: Subtract negative from positive
- **Category**: Edge
- **Operation**: SUB
- **Input**: 
  - Operand1: 5
  - Operand2: -3
  - Operation: S
- **Expected Output**: 8
- **Expected Status**: SUCCESS
- **Priority**: High

#### SUB-007: Same Numbers
- **Test Name**: Subtract number from itself
- **Category**: Edge
- **Operation**: SUB
- **Input**: 
  - Operand1: 100
  - Operand2: 100
  - Operation: S
- **Expected Output**: 0
- **Expected Status**: SUCCESS
- **Priority**: Medium

---

## 3. Multiplication Operation Test Cases

### 3.1 Normal Cases

#### MUL-001: Basic Multiplication
- **Test Name**: Multiply two positive integers
- **Category**: Normal
- **Operation**: MUL
- **Input**: 
  - Operand1: 5
  - Operand2: 3
  - Operation: M
- **Expected Output**: 15
- **Expected Status**: SUCCESS
- **Priority**: High

#### MUL-002: Multiply by One
- **Test Name**: Multiply number by one
- **Category**: Normal
- **Operation**: MUL
- **Input**: 
  - Operand1: 10
  - Operand2: 1
  - Operation: M
- **Expected Output**: 10
- **Expected Status**: SUCCESS
- **Priority**: High

#### MUL-003: Multiply by Zero
- **Test Name**: Multiply number by zero
- **Category**: Normal
- **Operation**: MUL
- **Input**: 
  - Operand1: 100
  - Operand2: 0
  - Operation: M
- **Expected Output**: 0
- **Expected Status**: SUCCESS
- **Priority**: High

#### MUL-004: Decimal Multiplication
- **Test Name**: Multiply decimal numbers
- **Category**: Normal
- **Operation**: MUL
- **Input**: 
  - Operand1: 2.5
  - Operand2: 4
  - Operation: M
- **Expected Output**: 10.0
- **Expected Status**: SUCCESS
- **Priority**: High

### 3.2 Edge Cases

#### MUL-005: Negative Multiplication
- **Test Name**: Multiply negative by positive
- **Category**: Edge
- **Operation**: MUL
- **Input**: 
  - Operand1: -5
  - Operand2: 3
  - Operation: M
- **Expected Output**: -15
- **Expected Status**: SUCCESS
- **Priority**: High

#### MUL-006: Two Negatives
- **Test Name**: Multiply two negative numbers
- **Category**: Edge
- **Operation**: MUL
- **Input**: 
  - Operand1: -5
  - Operand2: -3
  - Operation: M
- **Expected Output**: 15
- **Expected Status**: SUCCESS
- **Priority**: High

#### MUL-007: Large Number Multiplication
- **Test Name**: Multiply large numbers
- **Category**: Edge
- **Operation**: MUL
- **Input**: 
  - Operand1: 1000
  - Operand2: 1000
  - Operation: M
- **Expected Output**: 1000000
- **Expected Status**: SUCCESS
- **Priority**: Medium

#### MUL-008: Fraction Multiplication
- **Test Name**: Multiply fractions
- **Category**: Edge
- **Operation**: MUL
- **Input**: 
  - Operand1: 0.5
  - Operand2: 0.5
  - Operation: M
- **Expected Output**: 0.25
- **Expected Status**: SUCCESS
- **Priority**: Medium

---

## 4. Division Operation Test Cases

### 4.1 Normal Cases

#### DIV-001: Basic Division
- **Test Name**: Divide evenly
- **Category**: Normal
- **Operation**: DIV
- **Input**: 
  - Operand1: 10
  - Operand2: 2
  - Operation: D
- **Expected Output**: 5
- **Expected Status**: SUCCESS
- **Priority**: High

#### DIV-002: Division with Remainder
- **Test Name**: Divide with decimal result
- **Category**: Normal
- **Operation**: DIV
- **Input**: 
  - Operand1: 10
  - Operand2: 3
  - Operation: D
- **Expected Output**: 3.333 (or 3.33 depending on precision)
- **Expected Status**: SUCCESS
- **Priority**: High

#### DIV-003: Divide by One
- **Test Name**: Divide number by one
- **Category**: Normal
- **Operation**: DIV
- **Input**: 
  - Operand1: 10
  - Operand2: 1
  - Operation: D
- **Expected Output**: 10
- **Expected Status**: SUCCESS
- **Priority**: High

#### DIV-004: Zero Divided by Number
- **Test Name**: Divide zero by number
- **Category**: Normal
- **Operation**: DIV
- **Input**: 
  - Operand1: 0
  - Operand2: 5
  - Operation: D
- **Expected Output**: 0
- **Expected Status**: SUCCESS
- **Priority**: High

### 4.2 Edge Cases

#### DIV-005: Negative Division
- **Test Name**: Divide negative by positive
- **Category**: Edge
- **Operation**: DIV
- **Input**: 
  - Operand1: -10
  - Operand2: 2
  - Operation: D
- **Expected Output**: -5
- **Expected Status**: SUCCESS
- **Priority**: High

#### DIV-006: Two Negatives Division
- **Test Name**: Divide negative by negative
- **Category**: Edge
- **Operation**: DIV
- **Input**: 
  - Operand1: -10
  - Operand2: -2
  - Operation: D
- **Expected Output**: 5
- **Expected Status**: SUCCESS
- **Priority**: High

#### DIV-007: Decimal Division
- **Test Name**: Divide decimal numbers
- **Category**: Edge
- **Operation**: DIV
- **Input**: 
  - Operand1: 5.5
  - Operand2: 2.5
  - Operation: D
- **Expected Output**: 2.2
- **Expected Status**: SUCCESS
- **Priority**: Medium

#### DIV-008: Small Number Division
- **Test Name**: Divide by large number
- **Category**: Edge
- **Operation**: DIV
- **Input**: 
  - Operand1: 1
  - Operand2: 1000
  - Operation: D
- **Expected Output**: 0.001
- **Expected Status**: SUCCESS
- **Priority**: Low

### 4.3 Error Cases

#### DIV-009: Division by Zero
- **Test Name**: Divide by zero error
- **Category**: Error
- **Operation**: DIV
- **Input**: 
  - Operand1: 10
  - Operand2: 0
  - Operation: D
- **Expected Output**: N/A
- **Expected Status**: DIVIDE-BY-ZERO
- **Priority**: High

#### DIV-010: Zero by Zero
- **Test Name**: Divide zero by zero
- **Category**: Error
- **Operation**: DIV
- **Input**: 
  - Operand1: 0
  - Operand2: 0
  - Operation: D
- **Expected Output**: N/A
- **Expected Status**: DIVIDE-BY-ZERO
- **Priority**: High

---

## 5. Error Handling Test Cases

### 5.1 Invalid Operation Code

#### ERR-001: Invalid Operation
- **Test Name**: Invalid operation code
- **Category**: Error
- **Operation**: INVALID
- **Input**: 
  - Operand1: 10
  - Operand2: 5
  - Operation: X
- **Expected Output**: N/A
- **Expected Status**: ERROR
- **Priority**: High

#### ERR-002: Empty Operation
- **Test Name**: No operation specified
- **Category**: Error
- **Operation**: INVALID
- **Input**: 
  - Operand1: 10
  - Operand2: 5
  - Operation: (empty)
- **Expected Output**: N/A
- **Expected Status**: ERROR
- **Priority**: Medium

### 5.2 Invalid Input Data

#### ERR-003: Non-Numeric Input
- **Test Name**: Non-numeric operand
- **Category**: Error
- **Operation**: ADD
- **Input**: 
  - Operand1: ABC
  - Operand2: 5
  - Operation: A
- **Expected Output**: N/A
- **Expected Status**: ERROR
- **Priority**: High

#### ERR-004: Missing Operand
- **Test Name**: Missing second operand
- **Category**: Error
- **Operation**: ADD
- **Input**: 
  - Operand1: 10
  - Operand2: (empty)
  - Operation: A
- **Expected Output**: N/A
- **Expected Status**: ERROR
- **Priority**: High

---

## 6. Test Execution Summary

### Total Test Cases by Category
- **Normal Cases**: 16
- **Edge Cases**: 16
- **Error Cases**: 6
- **Total**: 38 test cases

### Test Cases by Operation
- **Addition**: 8 tests
- **Subtraction**: 7 tests
- **Multiplication**: 8 tests
- **Division**: 11 tests
- **Error Handling**: 4 tests

### Priority Distribution
- **High Priority**: 28 tests
- **Medium Priority**: 8 tests
- **Low Priority**: 2 tests

---

## 7. Test Data Files Format

### normal-cases.txt
```
# Format: OPERAND1|OPERAND2|OPERATION|EXPECTED_RESULT|EXPECTED_STATUS
5|3|A|8|SUCCESS
10|3|S|7|SUCCESS
5|3|M|15|SUCCESS
10|2|D|5|SUCCESS
```

### edge-cases.txt
```
# Format: OPERAND1|OPERAND2|OPERATION|EXPECTED_RESULT|EXPECTED_STATUS
-5|3|A|-2|SUCCESS
3|10|S|-7|SUCCESS
-5|3|M|-15|SUCCESS
-10|2|D|-5|SUCCESS
```

### error-cases.txt
```
# Format: OPERAND1|OPERAND2|OPERATION|EXPECTED_RESULT|EXPECTED_STATUS
10|0|D|N/A|DIVIDE-BY-ZERO
10|5|X|N/A|ERROR
ABC|5|A|N/A|ERROR
```

---

## 8. Acceptance Criteria

For the test suite to be considered complete and successful:

1. **Coverage**: All 38 test cases must be implemented
2. **Pass Rate**: 100% of tests must pass
3. **Error Handling**: All error conditions properly caught and reported
4. **Documentation**: Each test case properly documented with clear assertions
5. **Repeatability**: Tests produce consistent results across multiple runs
6. **Performance**: Each test completes within 1 second
7. **Maintainability**: Test code follows COBOL best practices and is well-commented

---

## 9. Test Execution Order

Tests should be executed in this order:
1. Normal operation tests (verify basic functionality)
2. Edge case tests (verify boundary conditions)
3. Error handling tests (verify error detection)

This ensures that basic functionality is validated before testing more complex scenarios.

---

## 10. Notes for Implementation

- Use COBOL EVALUATE statement for operation selection
- Implement proper numeric validation before operations
- Use COBOL exception handling for divide-by-zero
- Store test results in a structured format for reporting
- Include timestamps for test execution tracking
- Log all test inputs and outputs for debugging
