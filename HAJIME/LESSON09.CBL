      * 
      * 14JY0123 崔　禎　文
      *
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      LESSON09.
.
       
      *ENVIRONMENT      DIVISION.
       
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 W-TOTAL        PIC 9(3)　VALUE 0. 
       01 W-NUM         PIC 9(3).
       01 W-CNT         PIC 9(1) VALUE 0. 
       01 W-AVE         PIC 9(3).

          
       PROCEDURE        DIVISION.       
       
         PERFORM 5 TIMES
               DISPLAY "COBOLの点数を入力してください。"
               ACCEPT W-NUM 
       　　　　COMPUTE W-TOTAL = W-TOTAL + W-NUM
　　　　　　　 COMPUTE W-CNT = W-CNT + 1
               DISPLAY W-CNT "人目の点数は" W-NUM "です。"
       　END-PERFORM
             
             DISPLAY "*************************"
       　　　COMPUTE W-AVE = W-TOTAL / W-CNT　
             DISPLAY W-CNT "人目の平均は" W-AVE "です。"        

           STOP         RUN.
