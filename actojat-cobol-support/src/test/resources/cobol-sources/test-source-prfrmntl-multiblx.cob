 IDENTIFICATION DIVISION.
 PROGRAM-ID. WhileLoopzWMB.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 VeryVariable PIC 9(5) VALUE 1.

 PROCEDURE DIVISION.
 MainProgram.
      PERFORM DisplayOne THROUGH DisplayTwo
        UNTIL VeryVariable = 12
      DISPLAY "Aaaannnd".
      PERFORM DisplayOne THROUGH DisplayThree WITH TEST BEFORE
        UNTIL VeryVariable = 8
      DISPLAY "ImDone!".
      STOP RUN.

 DisplayOne.
      DISPLAY "Rock".

 DisplayTwo.
      DISPLAY "on!".

 DisplayThree.
      DISPLAY "Baby!".
