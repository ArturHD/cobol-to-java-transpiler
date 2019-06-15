 IDENTIFICATION DIVISION.
 PROGRAM-ID. SimpleLoop.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 VeryVariable PIC 9 VALUE 1.

 PROCEDURE DIVISION.
 MainProgram.
      PERFORM DisplayHelloWorld WITH TEST BEFORE
        UNTIL VeryVariable = 8
      DISPLAY "ImDone!".
      STOP RUN.

 DisplayHelloWorld.
      DISPLAY "Rock".
      DISPLAY "on!".
