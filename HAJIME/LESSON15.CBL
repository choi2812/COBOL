      * 
      * 14JY0123 崔　禎　文
      *
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      LESSON15.

       
      *ENVIRONMENT      DIVISION.
       
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 W-TOTAL        PIC 9(3) VALUE 0. 
       01 W-NUM          PIC 9(2).
       01 W-CNT          PIC 9(2) VALUE 0. 
       01 W-CNT2         PIC 9(1).
       01 W-AVE          PIC 9(2).

          
       PROCEDURE        DIVISION.       
       
         PERFORM UNTIL W-NUM = 00
               DISPLAY "数値を入力してください。(２桁）"
               ACCEPT  W-NUM            
             IF W-NUM < 40
             THEN DISPLAY "★"
             ELSE IF W-NUM >= 70
                      THEN DISPLAY "◆"
      　　　　　　　　ELSE DISPLAY "■"
                  END-IF
             END-IF

       　　　　COMPUTE W-TOTAL = W-TOTAL + W-NUM
      　　　　 COMPUTE W-CNT = W-CNT + 1 
         END-PERFORM              
             COMPUTE W-CNT2 = W-CNT - 1
             COMPUTE W-AVE = W-TOTAL / W-CNT2  　
             DISPLAY "入力件数は" W-CNT2  "で　平均は" W-AVE "です。"        

           STOP         RUN.
