 IDENTIFICATION DIVISION.
 PROGRAM-ID. LrExpressions.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 a PIC 9(5) VALUE 25.
 01 b PIC 9(5) VALUE 15.
 01 c PIC 9(5) VALUE 100.

 PROCEDURE DIVISION.
 MainProgram.
      IF (c / 33) < a THEN
        DISPLAY "oneAE"
      END-IF
      IF (b + (c / 2)) < a THEN
        DISPLAY "correct"
      END-IF
      STOP RUN.
