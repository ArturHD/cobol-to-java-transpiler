 IDENTIFICATION DIVISION.
 PROGRAM-ID. SimpleLoop.

 PROCEDURE DIVISION.
 MainProgram.
      PERFORM DisplayHelloWorld 15 TIMES.
      STOP RUN.

 DisplayHelloWorld.
      DISPLAY "Hello".
      DISPLAY "World!".
