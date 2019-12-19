      * 
      * 14JY0123 崔　禎　文
      *
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      LESSON 07.

       
      *ENVIRONMENT      DIVISION.
       
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 SUJI1         PIC 9(3).
       01 SUJI2         PIC 9(3).
       01 SUJI3         PIC 9(3).
       01 GOKEI         PIC 9(3).
          
       PROCEDURE        DIVISION.
           DISPLAY      "COBOLの点数入力してください。”       
       　　ACCEPT       SUJI1
           DISPLAY      "アルゴの点数入力してください。”
           ACCEPT       SUJI2     
       　　DISPLAY      "情報数学の点数入力してください。”
           ACCEPT       SUJI3 
           COMPUTE      GOKEI = （ SUJI3 + SUJI2 + SUJI1 ) / 3      
       　　DISPLAY      "3学科の平均は" GOKEI "です”
           IF           30 >= GOKEI
             THEN
           DISPLAY      "がんばりましょう！”
       　　END-IF
                    
           

           STOP         RUN.
