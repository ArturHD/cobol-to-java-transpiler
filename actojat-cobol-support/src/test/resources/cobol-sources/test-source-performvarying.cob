 IDENTIFICATION DIVISION.
 PROGRAM-ID. SimpleVaryingLoop.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 MyCounter PIC 9(5) VALUE 1.

 PROCEDURE DIVISION.
 MainProgram.
      PERFORM DisplaySomething
        VARYING MyCounter FROM 10 BY 2
        UNTIL MyCounter = 20
      DISPLAY "Im done!".
      STOP RUN.

 DisplaySomething.
      DISPLAY "Im".
      DISPLAY "varying".
