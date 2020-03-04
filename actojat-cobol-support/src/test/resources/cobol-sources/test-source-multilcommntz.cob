 *> Comments should be allowed before
 *> divison-, paragraph-, etc keywords!
 IDENTIFICATION DIVISION.
 * here, without the >
 PROGRAM-ID. Multilcommntz.
 *> and here...
 AUTHOR. pp.

**** and here as well!
 ENVIRONMENT DIVISION.
 *> and maybe here?
 CONFIGURATION SECTION.
 * and
 * multiple lines
 * here!
 SPECIAL-NAMES.

* rock
*and
 DATA DIVISION.
 *> roll,
 WORKING-STORAGE SECTION.
 * baby!
 01 MyCounter PIC 9 VALUE 3.

*** test test
 PROCEDURE DIVISION.
 * test!
 MainProgram.
 *> a loop:
      PERFORM MyCounter TIMES
*> And they should be allowed
/* like this, too!
        DISPLAY "Inline!"
      END-PERFORM
* A comment without the > at its beginning!
      DISPLAY "Done!"
      STOP RUN.
* A closing comment.
