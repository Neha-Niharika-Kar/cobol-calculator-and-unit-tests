       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-DIV.
       DATE-WRITTEN. 2026-02-06.
      *****************************************************************
      * Unit Tests for Division Operation                             *
      * Tests normal cases, edge cases, error conditions              *
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
           DISPLAY "DIVISION OPERATION TESTS"
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
           MOVE "D" TO TEST-OPERATION.
       
       RUN-ALL-TESTS.
      *    Normal Cases
           PERFORM TEST-DIV-001
           PERFORM TEST-DIV-002
           PERFORM TEST-DIV-003
           PERFORM TEST-DIV-004
           
      *    Edge Cases
           PERFORM TEST-DIV-005
           PERFORM TEST-DIV-006
           PERFORM TEST-DIV-007
           PERFORM TEST-DIV-008
           PERFORM TEST-DIV-010
           PERFORM TEST-DIV-011
           
      *    Error Cases
           PERFORM TEST-DIV-009
           PERFORM TEST-DIV-012
           PERFORM TEST-DIV-013
           PERFORM TEST-DIV-014.
       
       TEST-DIV-001.
           MOVE "DIV-001: Simple division (10 / 2)" TO TEST-NAME
           MOVE 10.00 TO TEST-OP1
           MOVE 2.00 TO TEST-OP2
           MOVE 5.00 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-DIV-002.
           MOVE "DIV-002: Even division (15 / 3)" TO TEST-NAME
           MOVE 15.00 TO TEST-OP1
           MOVE 3.00 TO TEST-OP2
           MOVE 5.00 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-DIV-003.
           MOVE "DIV-003: Large dividend (100 / 4)" TO TEST-NAME
           MOVE 100.00 TO TEST-OP1
           MOVE 4.00 TO TEST-OP2
           MOVE 25.00 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-DIV-004.
           MOVE "DIV-004: Decimal division (7.50 / 2.50)" 
               TO TEST-NAME
           MOVE 7.50 TO TEST-OP1
           MOVE 2.50 TO TEST-OP2
           MOVE 3.00 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-DIV-005.
           MOVE "DIV-005: Negative dividend (-10 / 2)" 
               TO TEST-NAME
           MOVE -10.00 TO TEST-OP1
           MOVE 2.00 TO TEST-OP2
           MOVE -5.00 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-DIV-006.
           MOVE "DIV-006: Both negative (-10 / -2)" TO TEST-NAME
           MOVE -10.00 TO TEST-OP1
           MOVE -2.00 TO TEST-OP2
           MOVE 5.00 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-DIV-007.
           MOVE "DIV-007: Negative divisor (10 / -2)" TO TEST-NAME
           MOVE 10.00 TO TEST-OP1
           MOVE -2.00 TO TEST-OP2
           MOVE -5.00 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-DIV-008.
           MOVE "DIV-008: Small decimals (0.01 / 0.01)" 
               TO TEST-NAME
           MOVE 0.01 TO TEST-OP1
           MOVE 0.01 TO TEST-OP2
           MOVE 1.00 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-DIV-009.
           MOVE "DIV-009: Divide by zero (10 / 0)" TO TEST-NAME
           MOVE 10.00 TO TEST-OP1
           MOVE 0.00 TO TEST-OP2
           MOVE 0.00 TO EXPECTED-RESULT
           MOVE "DIVIDE-BY-ZERO" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-DIV-010.
           MOVE "DIV-010: Result with decimals (1 / 3)" 
               TO TEST-NAME
           MOVE 1.00 TO TEST-OP1
           MOVE 3.00 TO TEST-OP2
           MOVE 0.33 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-DIV-011.
           MOVE "DIV-011: Large number (999999999.99 / 1)" 
               TO TEST-NAME
           MOVE 999999999.99 TO TEST-OP1
           MOVE 1.00 TO TEST-OP2
           MOVE 999999999.99 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-DIV-012.
           MOVE "DIV-012: Large dividend by zero (100 / 0)" 
               TO TEST-NAME
           MOVE 100.00 TO TEST-OP1
           MOVE 0.00 TO TEST-OP2
           MOVE 0.00 TO EXPECTED-RESULT
           MOVE "DIVIDE-BY-ZERO" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-DIV-013.
           MOVE "DIV-013: Negative by zero (-50 / 0)" 
               TO TEST-NAME
           MOVE -50.00 TO TEST-OP1
           MOVE 0.00 TO TEST-OP2
           MOVE 0.00 TO EXPECTED-RESULT
           MOVE "DIVIDE-BY-ZERO" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-DIV-014.
           MOVE "DIV-014: Zero by zero (0 / 0)" TO TEST-NAME
           MOVE 0.00 TO TEST-OP1
           MOVE 0.00 TO TEST-OP2
           MOVE 0.00 TO EXPECTED-RESULT
           MOVE "DIVIDE-BY-ZERO" TO EXPECTED-STATUS
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
           DISPLAY "TEST SUMMARY - DIVISION"
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
       
       END PROGRAM TEST-DIV.