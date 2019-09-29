 IDENTIFICATION DIVISION.
 PROGRAM-ID. Conditions.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 n PIC 9(5) VALUE 5.

 PROCEDURE DIVISION.
 MainProgram.
      IF n GREATER THAN 10 THEN
        DISPLAY "oops"
      END-IF
      IF n NOT GREATER THAN 20 THEN
        DISPLAY "correct"
      END-IF

      IF n LESS THAN 10 THEN
        DISPLAY "yeah"
      END-IF
      IF n NOT LESS THAN 10 THEN
        DISPLAY "damn"
      END-IF

      IF n > 100 THEN
        DISPLAY "oops2"
      END-IF
      IF n NOT > 200 THEN
        DISPLAY "correct2"
      END-IF

      IF n < 110 THEN
        DISPLAY "yeah2"
      END-IF
      IF n NOT < 110 THEN
        DISPLAY "damn2"
      END-IF

      IF n = 5 THEN
        DISPLAY "eqqqq"
      END-IF
      IF n NOT = 5 THEN
        DISPLAY "noteqqqq"
      END-IF

      IF n EQUAL TO 445 THEN
        DISPLAY "eqqqq2"
      END-IF
      IF n NOT EQUAL TO 775 THEN
        DISPLAY "noteqqqq2"
      END-IF

      IF n >= 123 THEN
        DISPLAY "goe11111"
      END-IF
      IF n GREATER THAN OR EQUAL TO 1550 THEN
        DISPLAY "gtoet2323"
      END-IF

      IF n <= 33 THEN
        DISPLAY "lteq33"
      END-IF
      IF n LESS THAN OR EQUAL TO 77112 THEN
        DISPLAY "ltort774444444"
      END-IF

      STOP RUN.
