      * 
      * 14JY0123 崔　禎　文
      *
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      Exercise03.
       
      *ENVIRONMENT      DIVISION.
       
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 SUJI1         PIC 9(3).
       01 SUJI2         PIC 9(3).
       01 GOKEI         PIC 9(4).
       
       PROCEDURE        DIVISION.
           DISPLAY      "ひとつめの数字を入力してください”
           ACCEPT       SUJI1
           DISPLAY      "ふたつめの数字を入力してください”
       　　ACCEPT       SUJI2
           COMPUTE      GOKEI = SUJI1 + SUJI2
           DISPLAY      "数字の合計は" GOKEI "です”
       
           STOP         RUN.
