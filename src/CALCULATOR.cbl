       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATOR.
       DATE-WRITTEN. 2026-02-06.
      *****************************************************************
      * COBOL Calculator Program                                       *
      * Performs basic arithmetic operations: +, -, *, /              *
      * Includes comprehensive error handling                          *
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-OPERAND-1            PIC S9(9)V99 VALUE ZEROS.
       01  WS-OPERAND-2            PIC S9(9)V99 VALUE ZEROS.
       01  WS-OPERATION            PIC X VALUE SPACE.
       01  WS-RESULT               PIC S9(9)V99 VALUE ZEROS.
       01  WS-STATUS               PIC X(20) VALUE SPACES.
       01  WS-ERROR-MSG            PIC X(50) VALUE SPACES.
       
       01  WS-DISPLAY-RESULT       PIC -(9)9.99.
       01  WS-DISPLAY-OP1          PIC -(9)9.99.
       01  WS-DISPLAY-OP2          PIC -(9)9.99.
       
       LINKAGE SECTION.
       01  LS-INPUT-DATA.
           05  LS-OPERAND-1        PIC S9(9)V99.
           05  LS-OPERAND-2        PIC S9(9)V99.
           05  LS-OPERATION        PIC X.
       
       01  LS-OUTPUT-DATA.
           05  LS-RESULT           PIC S9(9)V99.
           05  LS-STATUS           PIC X(20).
       
       PROCEDURE DIVISION USING LS-INPUT-DATA LS-OUTPUT-DATA.
       
       MAIN-LOGIC.
           PERFORM INITIALIZE-PROGRAM
           PERFORM VALIDATE-INPUT
           IF WS-STATUS = "SUCCESS" OR WS-STATUS = SPACES
               PERFORM EXECUTE-OPERATION
           END-IF
           PERFORM RETURN-RESULTS
           GOBACK.
       
       INITIALIZE-PROGRAM.
           MOVE SPACES TO WS-STATUS
           MOVE SPACES TO WS-ERROR-MSG
           MOVE ZEROS TO WS-RESULT
           MOVE LS-OPERAND-1 TO WS-OPERAND-1
           MOVE LS-OPERAND-2 TO WS-OPERAND-2
           MOVE LS-OPERATION TO WS-OPERATION.
       
       VALIDATE-INPUT.
      *    Check if operation code is valid
           IF WS-OPERATION NOT = "A" AND
              WS-OPERATION NOT = "S" AND
              WS-OPERATION NOT = "M" AND
              WS-OPERATION NOT = "D"
               MOVE "ERROR" TO WS-STATUS
               MOVE "INVALID OPERATION CODE" TO WS-ERROR-MSG
           END-IF.
       
       EXECUTE-OPERATION.
           EVALUATE WS-OPERATION
               WHEN "A"
                   PERFORM ADD-OPERATION
               WHEN "S"
                   PERFORM SUBTRACT-OPERATION
               WHEN "M"
                   PERFORM MULTIPLY-OPERATION
               WHEN "D"
                   PERFORM DIVIDE-OPERATION
               WHEN OTHER
                   PERFORM INVALID-OPERATION
           END-EVALUATE.
       
       ADD-OPERATION.
           ADD WS-OPERAND-1 TO WS-OPERAND-2 GIVING WS-RESULT
           MOVE "SUCCESS" TO WS-STATUS.
       
       SUBTRACT-OPERATION.
           SUBTRACT WS-OPERAND-2 FROM WS-OPERAND-1 GIVING WS-RESULT
           MOVE "SUCCESS" TO WS-STATUS.
       
       MULTIPLY-OPERATION.
           MULTIPLY WS-OPERAND-1 BY WS-OPERAND-2 GIVING WS-RESULT
           MOVE "SUCCESS" TO WS-STATUS.
       
       DIVIDE-OPERATION.
           IF WS-OPERAND-2 = ZERO
               MOVE "DIVIDE-BY-ZERO" TO WS-STATUS
               MOVE "CANNOT DIVIDE BY ZERO" TO WS-ERROR-MSG
               MOVE ZEROS TO WS-RESULT
           ELSE
               DIVIDE WS-OPERAND-1 BY WS-OPERAND-2 GIVING WS-RESULT
               MOVE "SUCCESS" TO WS-STATUS
           END-IF.
       
       INVALID-OPERATION.
           MOVE "ERROR" TO WS-STATUS
           MOVE "INVALID OPERATION" TO WS-ERROR-MSG
           MOVE ZEROS TO WS-RESULT.
       
       RETURN-RESULTS.
           MOVE WS-RESULT TO LS-RESULT
           MOVE WS-STATUS TO LS-STATUS.
       
       END PROGRAM CALCULATOR.
