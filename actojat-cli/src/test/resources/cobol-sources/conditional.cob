 IDENTIFICATION DIVISION.
 PROGRAM-ID. IfThenElseAndConditions.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 n PIC 9 VALUE 5.
  01 a PIC 9(2) VALUE 25.
  01 b PIC 9(2) VALUE 15.
  01 c PIC 9(3) VALUE 100.

 PROCEDURE DIVISION.
 MainProgram.
      IF n NOT GREATER THAN 20 THEN
        DISPLAY "correct"
      ELSE
        DISPLAY "notcorrect"
      END-IF

      IF n LESS THAN 10 THEN
        DISPLAY "yeah"
      END-IF
      IF n NOT LESS THAN 10 THEN
        DISPLAY "notlezz"
      END-IF

      IF n NOT EQUAL TO 775 THEN
        DISPLAY "noteq"
      ELSE
        DISPLAY "eq"
      END-IF

      IF n <= 33 THEN
        DISPLAY "lteq33"
      END-IF
      IF n LESS THAN OR EQUAL TO 77 THEN
        DISPLAY "ltort77"
      END-IF

      IF a < (b + (c / 2)) THEN
        DISPLAY "correct"
      END-IF

      STOP RUN.
