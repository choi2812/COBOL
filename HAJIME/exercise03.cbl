      * 
      * 14JY0123 ���@���@��
      *
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      Exercise03.
       
      *ENVIRONMENT      DIVISION.
       
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 SUJI1         PIC 9(3).
       01 SUJI2         PIC 9(3).
       01 GOKEI         PIC 9(4).
       
       PROCEDURE        DIVISION.
           DISPLAY      "�ЂƂ߂̐�������͂��Ă��������h
           ACCEPT       SUJI1
           DISPLAY      "�ӂ��߂̐�������͂��Ă��������h
       �@�@ACCEPT       SUJI2
           COMPUTE      GOKEI = SUJI1 + SUJI2
           DISPLAY      "�����̍��v��" GOKEI "�ł��h
       
           STOP         RUN.
