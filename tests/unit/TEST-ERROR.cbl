       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-ERROR.
       DATE-WRITTEN. 2026-02-06.
      *****************************************************************
      * Unit Tests for Error Handling                                 *
      * Tests invalid operations and error conditions                 *
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
           DISPLAY "ERROR HANDLING TESTS"
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
           MOVE ZERO TO FAIL-COUNTER.
       
       RUN-ALL-TESTS.
      *    Invalid Operation Tests
           PERFORM TEST-ERR-001
           PERFORM TEST-ERR-002
           PERFORM TEST-ERR-003
           PERFORM TEST-ERR-004.
       
       TEST-ERR-001.
           MOVE "ERR-001: Invalid operation 'X'" TO TEST-NAME
           MOVE 10.00 TO TEST-OP1
           MOVE 5.00 TO TEST-OP2
           MOVE "X" TO TEST-OPERATION
           MOVE 0.00 TO EXPECTED-RESULT
           MOVE "ERROR" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-ERR-002.
           MOVE "ERR-002: Invalid operation 'Z'" TO TEST-NAME
           MOVE 10.00 TO TEST-OP1
           MOVE 5.00 TO TEST-OP2
           MOVE "Z" TO TEST-OPERATION
           MOVE 0.00 TO EXPECTED-RESULT
           MOVE "ERROR" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-ERR-003.
           MOVE "ERR-003: Invalid operation '1'" TO TEST-NAME
           MOVE 10.00 TO TEST-OP1
           MOVE 5.00 TO TEST-OP2
           MOVE "1" TO TEST-OPERATION
           MOVE 0.00 TO EXPECTED-RESULT
           MOVE "ERROR" TO EXPECTED-STATUS
           PERFORM EXECUTE-TEST.
       
       TEST-ERR-004.
           MOVE "ERR-004: Invalid operation (space)" TO TEST-NAME
           MOVE 10.00 TO TEST-OP1
           MOVE 5.00 TO TEST-OP2
           MOVE " " TO TEST-OPERATION
           MOVE 0.00 TO EXPECTED-RESULT
           MOVE "ERROR" TO EXPECTED-STATUS
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
           DISPLAY "TEST SUMMARY - ERROR HANDLING"
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
       
       END PROGRAM TEST-ERROR.