      * 
      * 14JY0123 崔　禎　文
      *
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      LESSON11.

       
      *ENVIRONMENT      DIVISION.
       
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 W-TOTAL        PIC 9(3) VALUE 0. 
       01 W-NUM          PIC 9(2).
       01 W-CNT          PIC 9(1) VALUE 0. 
       01 W-AVE          PIC 9(2).

          
       PROCEDURE        DIVISION.       
       
         PERFORM 6 TIMES
               DISPLAY "数値を入力してください。(２桁）"
               ACCEPT  W-NUM 
       　　　　COMPUTE W-TOTAL = W-TOTAL + W-NUM
       　　　　COMPUTE W-CNT = W-CNT + 1
         END-PERFORM
             
       　　　COMPUTE W-AVE = W-TOTAL / W-CNT 　
             DISPLAY "平均は" W-AVE "です。"        

           STOP         RUN.
