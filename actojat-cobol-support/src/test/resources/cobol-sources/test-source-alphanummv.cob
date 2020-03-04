 IDENTIFICATION DIVISION.
 PROGRAM-ID. AlphaNumMv.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 Surname PIC X(6) VALUE "Chuck ".
 01 TruncateName PIC X(2) VALUE "ab".
 01 FillName PIC X(12) VALUE "123456789012".

 PROCEDURE DIVISION.
 MainProgram.
      MOVE "Arnold" TO Surname
      MOVE "Brnold" TO TruncateName
      MOVE "Crnold" TO FillName
      STOP RUN.
