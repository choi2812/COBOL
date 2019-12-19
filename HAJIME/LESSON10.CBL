      * 
      * 14JY0123 崔　禎　文
      *
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      LESSON10.

       
      *ENVIRONMENT      DIVISION.
       
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 W-TOTAL       PIC 9(3) VALUE 0. 
       01 W-NUM         PIC 9(3).
       01 W-CNT         PIC 9(1) VALUE 0. 
       01 SAIKO      　 PIC 9(3) VALUE 0.
       01 SAITE       　PIC 9(3) VALUE 100.


          
       PROCEDURE        DIVISION.       
       
         PERFORM 5 TIMES
               DISPLAY "COBOLの点数を入力してください。"
               ACCEPT W-NUM 
               IF W-NUM > SAIKO
                  THEN MOVE W-NUM TO SAIKO 
               END-IF
       　　　  IF W-NUM < SAITE
       　　　　　THEN MOVE W-NUM TO SAITE
               END-IF
   
              COMPUTE W-TOTAL = W-TOTAL + W-NUM
　　　　　　　 COMPUTE W-CNT = W-CNT + 1
               DISPLAY W-CNT "人目の点数は" W-NUM "です。"
       　END-PERFORM
             
             DISPLAY "*************************"
             DISPLAY  "最高点は" SAIKO "です。" 
       　　　DISPLAY  "最低点は" SAITE "です。" 
              

           STOP         RUN.
