      * 
      * 14JY0123 崔　禎　文
      *
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      LESSON03.

       
      *ENVIRONMENT      DIVISION.
       
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 NAMAE         PIC X(10).
       01 SUJI1         PIC 9(4).
       01 SUGI2         PIC 9(4).
       01 GOKEI         PIC 9(2).
       01 TOSI          PIC X(12).
       
       PROCEDURE        DIVISION.
           DISPLAY      "あなたのなまえは？”
           ACCEPT       NAMAE
           DISPLAY      "あなたが生まれたとしは？”
       　　ACCEPT       SUJI1
           DISPLAY      "今年は？”
           ACCEPT       SUGI2 
           COMPUTE      GOKEI = SUGI2 - SUJI1      
       　　DISPLAY      NAMAE "は今年" GOKEI "歳です”
　　　　　 IF           TOSI >= 20 
             THEN 
       　　　　DISPLAY "成人です。"
             ELSE 
       　　　　DISPLAY　"未成年です。"
           END-IF
           STOP         RUN.
