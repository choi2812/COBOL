      * 
      * 14JY0123 崔　禎　文
      *
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      LESSON08.

       
      *ENVIRONMENT      DIVISION.
       
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 SUJI1         PIC 9(3).
       01 SUJI2         PIC 9(3).
       01 SUJI3         PIC 9(3).
       01 SUJI4         PIC 9(3).
       01 SUJI5         PIC 9(3).
       01 GOKEI         PIC 9(3).
       01 HEIKIN        PIC 9(3).
          
       PROCEDURE        DIVISION.
           DISPLAY      "COBOLの点数入力してください。”       
       　　ACCEPT       SUJI1  
           DISPLAY      "1人目の点数は" SUJI1 "です。" 
           DISPLAY      "COBOLの点数入力してください。”     
           ACCEPT       SUJI2         
           DISPLAY      "2人目の点数は" SUJI2 "です。" 
       　　DISPLAY      "COBOLの点数入力してください。”     
           ACCEPT       SUJI3         
           DISPLAY      "3人目の点数は" SUJI3 "です。"
          DISPLAY      "COBOLの点数入力してください。”     
           ACCEPT       SUJI4         
           DISPLAY      "4人目の点数は" SUJI4 "です。"
           DISPLAY      "COBOLの点数入力してください。”     
           ACCEPT       SUJI5         
           DISPLAY      "5人目の点数は" SUJI5 "です。" 
           COMPUTE      GOKEI = SUJI5 + SUJI4 + SUJI3 + SUJI2 + SUJI1
         　COMPUTE      HEIKIN  = GOKEI / 5

       　　DISPLAY      "******************************"      
       　　DISPLAY      "5人の平均は" HEIKIN "点です。”
                               
           

           STOP         RUN.
