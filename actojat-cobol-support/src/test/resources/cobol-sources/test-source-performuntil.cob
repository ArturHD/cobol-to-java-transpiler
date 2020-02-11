 IDENTIFICATION DIVISION.
 PROGRAM-ID. SimpleWhileLoop.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 VeryVariable PIC 9(5) VALUE 1.

 PROCEDURE DIVISION.
 MainProgram.
      PERFORM DisplayHelloWorld
        UNTIL VeryVariable = 8
      DISPLAY "Im done!".
      STOP RUN.

 DisplayHelloWorld.
      DISPLAY "Rock".
      DISPLAY "on!".
