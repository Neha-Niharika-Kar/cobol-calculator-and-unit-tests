       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-MUL.
       DATE-WRITTEN. 2026-02-06.
      *****************************************************************
      * Unit Tests for Multiplication Operation                       *
      * Tests normal cases, edge cases, and boundary conditions       *
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TEST-COUNTER            PIC 9(3) VALUE 0.
       01  PASS-COUNTER            PIC 9(3) VALUE 0.
       01  FAIL-COUNTER            PIC 9(3) VALUE 0.
       
       01  TEST-INPUT.
           05  TEST-OP1            PIC S9(9)V99.
           05  TEST-OP2            PIC S9(9)V99.
           05  TEST-OPERATION      PIC X.
       
       01  TEST-OUTPUT.
           05  TEST-RESULT         PIC S9(9)V99.
           05  TEST-STATUS         PIC X(20).
       
       01  EXPECTED-RESULT         PIC S9(9)V99.
       01  EXPECTED-STATUS         PIC X(20).
       
       01  TEST-NAME               PIC X(50).
       01  DISPLAY-RESULT          PIC -(9)9.99.
       01  DISPLAY-EXPECTED        PIC -(9)9.99.
       
       PROCEDURE DIVISION.
       
       MAIN-TEST-LOGIC.
           DISPLAY "========================================"
           DISPLAY "MULTIPLICATION OPERATION TESTS"
           DISPLAY "========================================"
           DISPLAY " "
           
           PERFORM SETUP-TEST-SUITE
           PERFORM RUN-ALL-TESTS
           PERFORM DISPLAY-TEST-SUMMARY
           
           IF FAIL-COUNTER > 0
               STOP RUN RETURNING 1
           ELSE
               STOP RUN RETURNING 0
           END-IF.
       
       SETUP-TEST-SUITE.
           MOVE ZERO TO TEST-COUNTER
           MOVE ZERO TO PASS-COUNTER
           MOVE ZERO TO FAIL-COUNTER
           MOVE "M" TO TEST-OPERATION.
       
       RUN-ALL-TESTS.
      *    Normal Cases
           PERFORM TEST-MUL-001
           PERFORM TEST-MUL-002
           PERFORM TEST-MUL-003
           PERFORM TEST-MUL-004
           
      *    Edge Cases
           PERFORM TEST-MUL-005
           PERFORM TEST-MUL-006
           PERFORM TEST-MUL-007
           PERFORM TEST-MUL-008.
       
       TEST-MUL-001.
           MOVE "MUL-001: Positive integers (5 * 3)" TO TEST-NAME
           MOVE 5.00 TO TEST-OP1
           MOVE 3.00 TO TEST-OP2
           MOVE 15.00 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-MUL-002.
           MOVE "MUL-002: Multiply by zero (10 * 0)" TO TEST-NAME
           MOVE 10.00 TO TEST-OP1
           MOVE 0.00 TO TEST-OP2
           MOVE 0.00 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-MUL-003.
           MOVE "MUL-003: Decimal numbers (7.50 * 2)" 
               TO TEST-NAME
           MOVE 7.50 TO TEST-OP1
           MOVE 2.00 TO TEST-OP2
           MOVE 15.00 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-MUL-004.
           MOVE "MUL-004: Decimal result (12.25 * 4)" TO TEST-NAME
           MOVE 12.25 TO TEST-OP1
           MOVE 4.00 TO TEST-OP2
           MOVE 49.00 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-MUL-005.
           MOVE "MUL-005: Negative * Positive (-5 * 3)" 
               TO TEST-NAME
           MOVE -5.00 TO TEST-OP1
           MOVE 3.00 TO TEST-OP2
           MOVE -15.00 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-MUL-006.
           MOVE "MUL-006: Both negative (-5 * -3)" TO TEST-NAME
           MOVE -5.00 TO TEST-OP1
           MOVE -3.00 TO TEST-OP2
           MOVE 15.00 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-MUL-007.
           MOVE "MUL-007: Very small decimals (0.01 * 0.01)" 
               TO TEST-NAME
           MOVE 0.01 TO TEST-OP1
           MOVE 0.01 TO TEST-OP2
           MOVE 0.00 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-MUL-008.
           MOVE "MUL-008: Large number (999999.99 * 10)" 
               TO TEST-NAME
           MOVE 999999.99 TO TEST-OP1
           MOVE 10.00 TO TEST-OP2
           MOVE 9999999.90 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       EXECUTE-TEST.
           ADD 1 TO TEST-COUNTER
           
           CALL "CALCULATOR" USING TEST-INPUT TEST-OUTPUT
           
           PERFORM ASSERT-EQUALS.
       
       ASSERT-EQUALS.
           IF TEST-RESULT = EXPECTED-RESULT AND
              TEST-STATUS = EXPECTED-STATUS
               ADD 1 TO PASS-COUNTER
               DISPLAY "PASS: " TEST-NAME
           ELSE
               ADD 1 TO FAIL-COUNTER
               DISPLAY "FAIL: " TEST-NAME
               MOVE TEST-RESULT TO DISPLAY-RESULT
               MOVE EXPECTED-RESULT TO DISPLAY-EXPECTED
               DISPLAY "  Expected: " DISPLAY-EXPECTED 
                   " [" EXPECTED-STATUS "]"
               DISPLAY "  Actual:   " DISPLAY-RESULT 
                   " [" TEST-STATUS "]"
           END-IF.
       
       DISPLAY-TEST-SUMMARY.
           DISPLAY " "
           DISPLAY "========================================"
           DISPLAY "TEST SUMMARY - MULTIPLICATION"
           DISPLAY "========================================"
           DISPLAY "Total Tests:  " TEST-COUNTER
           DISPLAY "Passed:       " PASS-COUNTER
           DISPLAY "Failed:       " FAIL-COUNTER
           
           IF FAIL-COUNTER = 0
               DISPLAY "Status:       ALL TESTS PASSED"
           ELSE
               DISPLAY "Status:       SOME TESTS FAILED"
           END-IF
           
           DISPLAY "========================================"
           DISPLAY " ".
       
       END PROGRAM TEST-MUL.