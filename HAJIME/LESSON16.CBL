      * 
      * 14JY0123 崔　禎　文
      *
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      LESSON16.

       
      *ENVIRONMENT      DIVISION.
       
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 W-TOTAL        PIC 9(3) VALUE 0. 
       01 W-NUM          PIC 9(2).
       01 W-CNT          PIC 9(2) VALUE 0. 
       01 W-AVE          PIC 9(2).

          
       PROCEDURE        DIVISION.  
           DISPLAY "数値を入力してください。(２桁）"
           ACCEPT  W-NUM 
           
             PERFORM UNTIL W-NUM = 00
             IF W-NUM < 40
                THEN DISPLAY "★"
                 ELSE IF W-NUM >= 70
                         THEN DISPLAY "◆"
      　　　　　　　　   ELSE DISPLAY "■"
                      END-IF
             END-IF

       　　　COMPUTE W-TOTAL = W-TOTAL + W-NUM
      　　 　COMPUTE W-CNT = W-CNT + 1
           　DISPLAY "数値を入力してください。(２桁）"
           　ACCEPT  W-NUM

           END-PERFORM              
           COMPUTE W-AVE = W-TOTAL / W-CNT  　
           DISPLAY "入力件数は" W-CNT  "で　平均は" W-AVE "です。"        

           STOP         RUN.
