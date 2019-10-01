 IDENTIFICATION DIVISION.
 PROGRAM-ID. PerformUntilTest.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 MyVar PIC 9(5) VALUE 1.
 01 VeryVariable PIC 9(5) VALUE 1.

 PROCEDURE DIVISION.
 MainProgram.
      PERFORM UNTIL MyVar = 13
        DISPLAY "Inline!"
      END-PERFORM.

      PERFORM DisplayRockOn
      UNTIL VeryVariable = 8

      DISPLAY "Done!".
      STOP RUN.

DisplayRockOn.
      DISPLAY "Rock".
      DISPLAY "on!".
