      * 
      * 14JY0123 ���@���@��
      *
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      LESSON06.

       
      *ENVIRONMENT      DIVISION.
       
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 SUJI1         PIC 9(3).
       01 SUJI2         PIC 9(3).
       01 SUJI3         PIC 9(3).
       01 GOKEI         PIC 9(3).
          
       PROCEDURE        DIVISION.
           DISPLAY      "COBOL�̓_�����͂��Ă��������B�h       
       �@�@ACCEPT       SUJI1
           DISPLAY      "�A���S�̓_�����͂��Ă��������B�h
           ACCEPT       SUJI2     
       �@�@DISPLAY      "��񐔊w�̓_�����͂��Ă��������B�h
           ACCEPT       SUJI3 
           COMPUTE      GOKEI = �i SUJI3 + SUJI2 + SUJI1 ) / 3      
       �@�@DISPLAY      "3�w�Ȃ̕��ς�" GOKEI "�ł��h
           IF           80 <= GOKEI
             THEN
           DISPLAY      "��ς悭�����܂����I�h
       �@�@END-IF
                    
           

           STOP         RUN.