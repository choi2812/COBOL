      * 
      * 14JY0123 ���@���@��
      *
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      LESSON10.

       
      *ENVIRONMENT      DIVISION.
       
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 W-TOTAL       PIC 9(3) VALUE 0. 
       01 W-NUM         PIC 9(3).
       01 W-CNT         PIC 9(1) VALUE 0. 
       01 SAIKO      �@ PIC 9(3) VALUE 0.
       01 SAITE       �@PIC 9(3) VALUE 100.


          
       PROCEDURE        DIVISION.       
       
         PERFORM 5 TIMES
               DISPLAY "COBOL�̓_������͂��Ă��������B"
               ACCEPT W-NUM 
               IF W-NUM > SAIKO
                  THEN MOVE W-NUM TO SAIKO 
               END-IF
       �@�@�@  IF W-NUM < SAITE
       �@�@�@�@�@THEN MOVE W-NUM TO SAITE
               END-IF
   
              COMPUTE W-TOTAL = W-TOTAL + W-NUM
�@�@�@�@�@�@�@ COMPUTE W-CNT = W-CNT + 1
               DISPLAY W-CNT "�l�ڂ̓_����" W-NUM "�ł��B"
       �@END-PERFORM
             
             DISPLAY "*************************"
             DISPLAY  "�ō��_��" SAIKO "�ł��B" 
       �@�@�@DISPLAY  "�Œ�_��" SAITE "�ł��B" 
              

           STOP         RUN.