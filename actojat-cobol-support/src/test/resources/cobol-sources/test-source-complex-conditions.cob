 IDENTIFICATION DIVISION.
 PROGRAM-ID. ComplexConditions.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 a PIC 9 VALUE 25.
 01 b PIC 9 VALUE 15.
 01 c PIC 9 VALUE 100.

 PROCEDURE DIVISION.
 MainProgram.
      IF a GREATER THAN b THEN
        DISPLAY "great0r"
      END-IF
      IF a < (c / 33) THEN
        DISPLAY "oneAE"
      END-IF
      IF a < (b + (c / 2)) THEN
        DISPLAY "correct"
      END-IF
      STOP RUN.
