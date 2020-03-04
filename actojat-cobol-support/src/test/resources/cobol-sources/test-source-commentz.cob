 IDENTIFICATION DIVISION.
 PROGRAM-ID. Commentz.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
* I'm a comment and I "belong" to MyCounter:
 01 MyCounter PIC 9 VALUE 3.

 PROCEDURE DIVISION.
 MainProgram.
      PERFORM MyCounter TIMES
*> This is an inline comment: its allowed for the
        DISPLAY "Inline!"
      END-PERFORM
*> ...non-fixed (i.e. the "free") format!
      DISPLAY "Done!"
      STOP RUN.
