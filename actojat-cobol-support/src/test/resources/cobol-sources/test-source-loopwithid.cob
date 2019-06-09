 IDENTIFICATION DIVISION.
 PROGRAM-ID. LoopWithId.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 n PIC 9 VALUE 5.

 PROCEDURE DIVISION.
 MainProgram.
      PERFORM DisplayHelloWorld n TIMES.
      STOP RUN.

 DisplayHelloWorld.
      DISPLAY "Hello".
      DISPLAY "World!".

 DoSomethingElse.
      DISPLAY "Something".
      DISPLAY "else!".
