 IDENTIFICATION DIVISION.
 PROGRAM-ID. WhileLoopWithTest.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 VeryVeryVariable PIC 9 VALUE 1.

 PROCEDURE DIVISION.
 MainProgram.
      PERFORM DisplayImAWhileLoop WITH TEST BEFORE
        UNTIL VeryVeryVariable = 2
      PERFORM DisplayImADoWhileLoop WITH TEST AFTER
        UNTIL VeryVeryVariable = 2
      DISPLAY "Done!".
      STOP RUN.

 DisplayImAWhileLoop.
      DISPLAY "whileLoop".

 DisplayImADoWhileLoop.
      DISPLAY "doWhileLoop".
