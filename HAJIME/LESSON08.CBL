      * 
      * 14JY0123 ���@���@��
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
           DISPLAY      "COBOL�̓_�����͂��Ă��������B�h       
       �@�@ACCEPT       SUJI1  
           DISPLAY      "1�l�ڂ̓_����" SUJI1 "�ł��B" 
           DISPLAY      "COBOL�̓_�����͂��Ă��������B�h     
           ACCEPT       SUJI2         
           DISPLAY      "2�l�ڂ̓_����" SUJI2 "�ł��B" 
       �@�@DISPLAY      "COBOL�̓_�����͂��Ă��������B�h     
           ACCEPT       SUJI3         
           DISPLAY      "3�l�ڂ̓_����" SUJI3 "�ł��B"
          DISPLAY      "COBOL�̓_�����͂��Ă��������B�h     
           ACCEPT       SUJI4         
           DISPLAY      "4�l�ڂ̓_����" SUJI4 "�ł��B"
           DISPLAY      "COBOL�̓_�����͂��Ă��������B�h     
           ACCEPT       SUJI5         
           DISPLAY      "5�l�ڂ̓_����" SUJI5 "�ł��B" 
           COMPUTE      GOKEI = SUJI5 + SUJI4 + SUJI3 + SUJI2 + SUJI1
         �@COMPUTE      HEIKIN  = GOKEI / 5

       �@�@DISPLAY      "******************************"      
       �@�@DISPLAY      "5�l�̕��ς�" HEIKIN "�_�ł��B�h
                               
           

           STOP         RUN.