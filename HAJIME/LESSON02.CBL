      * 
      * 14JY0123 ���@���@��
      *
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      LESSON02.

       
      *ENVIRONMENT      DIVISION.
       
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 NAMAE         PIC X(10).
       01 SUJI1         PIC 9(4).
       01 SUGI2         PIC 9(4).
       01 GOKEI         PIC 9(2).
       
       PROCEDURE        DIVISION.
           DISPLAY      "���Ȃ��̂Ȃ܂��́H�h
           ACCEPT       NAMAE
           DISPLAY      "���Ȃ������܂ꂽ�Ƃ��́H�h
       �@�@ACCEPT       SUJI1
           DISPLAY      "���N�́H�h
           ACCEPT       SUGI2 
           COMPUTE      GOKEI = SUGI2 - SUJI1
           DISPLAY      NAMAE "�͍��N" GOKEI "�΂ł��h
       
           STOP         RUN.