 IDENTIFICATION DIVISION.
 PROGRAM-ID. PerformTimesTest.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 n PIC 4 VALUE 3.

 PROCEDURE DIVISION.
 MainProgram.
      PERFORM n TIMES
        DISPLAY "Inline!"
      END-PERFORM
      PERFORM DisplayHelloWorld 15 TIMES.
      STOP RUN.

DisplayHelloWorld.
      DISPLAY "Hello".
      DISPLAY "World!".
