 IDENTIFICATION DIVISION.
 PROGRAM-ID. HierarchData.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 n PIC 9 VALUE 5.
 01 Complexx.
   02 aaaaa PIC X(08).
   02 bbbbb PIC X(08).
   02 ccccc PIC X(08).
   02 More-Complexx.
      03 ddddd PIC X(01).
      03 eeeee PIC X(01).
      03 fffff PIC X(01).
      03 FILENUM.
         04 ggggg PIC X(01).
         04 hhhhh PIC X(05).
      03 iiiii PIC 9(6).
      03 jjjjj PIC 9(6).

 01 Somewhat-Complex.
   02 kkkkk PIC X(02) value "xy".
   02 lllll PIC X(08).

 01 Xelpmoc.
   02 mmmmm PIC X(08).
   02 nnnnn PIC X(02).

 01 m PICTURE 9(4) VALUE 1234.

 PROCEDURE DIVISION.
 MainProgram.
      IF (n + m) < 10 THEN
        DISPLAY "Yeah"
      END-IF
      STOP RUN.
