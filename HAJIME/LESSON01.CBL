      * 
      * 14JY0123 崔　禎　文
      *
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      LESSON01.
       
      *ENVIRONMENT      DIVISION.
       
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 NAMAE         PIC X(10).
       01 SUJI1         PIC 9(4).
       01 GOKEI         PIC 9(2).
       
       PROCEDURE        DIVISION.
           DISPLAY      "あなたのなまえは？”
           ACCEPT       NAMAE
           DISPLAY      "あなたが生まれたとしは？”
       　　ACCEPT       SUJI1
           COMPUTE      GOKEI = 2014 - SUJI1
           DISPLAY      NAMAE "は今年" GOKEI "歳です”
       
           STOP         RUN.
