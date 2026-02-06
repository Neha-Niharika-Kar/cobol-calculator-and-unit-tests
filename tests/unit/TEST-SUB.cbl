       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-SUB.
       DATE-WRITTEN. 2026-02-06.
      *****************************************************************
      * Unit Tests for Subtraction Operation                          *
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
           DISPLAY "SUBTRACTION OPERATION TESTS"
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
           MOVE "S" TO TEST-OPERATION.
       
       RUN-ALL-TESTS.
      *    Normal Cases
           PERFORM TEST-SUB-001
           PERFORM TEST-SUB-002
           PERFORM TEST-SUB-003
           PERFORM TEST-SUB-004
           
      *    Edge Cases
           PERFORM TEST-SUB-005
           PERFORM TEST-SUB-006
           PERFORM TEST-SUB-007.
       
       TEST-SUB-001.
           MOVE "SUB-001: Positive integers (10 - 3)" TO TEST-NAME
           MOVE 10.00 TO TEST-OP1
           MOVE 3.00 TO TEST-OP2
           MOVE 7.00 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-SUB-002.
           MOVE "SUB-002: Equal numbers (5 - 5)" TO TEST-NAME
           MOVE 5.00 TO TEST-OP1
           MOVE 5.00 TO TEST-OP2
           MOVE 0.00 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-SUB-003.
           MOVE "SUB-003: Decimal numbers (100 - 25.50)" 
               TO TEST-NAME
           MOVE 100.00 TO TEST-OP1
           MOVE 25.50 TO TEST-OP2
           MOVE 74.50 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-SUB-004.
           MOVE "SUB-004: Subtract zero (50 - 0)" TO TEST-NAME
           MOVE 50.00 TO TEST-OP1
           MOVE 0.00 TO TEST-OP2
           MOVE 50.00 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-SUB-005.
           MOVE "SUB-005: Result negative (3 - 10)" TO TEST-NAME
           MOVE 3.00 TO TEST-OP1
           MOVE 10.00 TO TEST-OP2
           MOVE -7.00 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-SUB-006.
           MOVE "SUB-006: Both negative (-5 - -3)" TO TEST-NAME
           MOVE -5.00 TO TEST-OP1
           MOVE -3.00 TO TEST-OP2
           MOVE -2.00 TO EXPECTED-RESULT
           MOVE "SUCCESS" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-SUB-007.
           MOVE "SUB-007: Negative - Positive (-10 - 5)" 
               TO TEST-NAME
           MOVE -10.00 TO TEST-OP1
           MOVE 5.00 TO TEST-OP2
           MOVE -15.00 TO EXPECTED-RESULT
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
           DISPLAY "TEST SUMMARY - SUBTRACTION"
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
       
       END PROGRAM TEST-SUB.