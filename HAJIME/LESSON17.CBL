      * 
      * 14JY0123 ���@���@��
      *
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      LESSON17.

       
      *ENVIRONMENT      DIVISION.
       
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.   
       01 NAMAE          PIC X(10).
       01 SUJI1          PIC 9(3). 
       01 SUJI2          PIC 9(3).
       01 SUJI3          PIC 9(3). 
       01 SUJI-AVE       PIC 9(3).
       01 COBOL-PLUS     PIC 9(3) VALUE 0.
       01 JOHO-PLUS      PIC 9(3) VALUE 0.
       01 ARUGO-PLUS     PIC 9(3) VALUE 0.
       01 COBOL-AVE      PIC 9(3).
       01 JOHO-AVE       PIC 9(3).
       01 ARUGO-AVE      PIC 9(3).

          
       PROCEDURE        DIVISION.    
           
           PERFORM 3 TIMES
            DISPLAY "���O����͂��Ă��������B"
            ACCEPT   NAMAE  
            DISPLAY "COBOL�̓_������͂��Ă��������B"
            ACCEPT   SUJI1
            COMPUTE  COBOL-PLUS = SUJI1 + COBOL-PLUS
            DISPLAY "��񐔊w�̓_������͂��Ă��������B"
            ACCEPT   SUJI2    
            COMPUTE  JOHO-PLUS = SUJI2 + JOHO-PLUS
            DISPLAY "�A���S���Y���̓_��������͂��Ă��������B"
            ACCEPT   SUJI3 
            COMPUTE  ARUGO-PLUS  = SUJI3 + ARUGO-PLUS 
            COMPUTE  SUJI-AVE = SUJI1 + SUJI2 + SUJI3 / 3 
            DISPLAY  NAMAE "��3�Ȗڂ̕��ς�" SUJI-AVE "�_�ł��B" 
            DISPLAY  " "  �@     
           END-PERFORM   
            COMPUTE COBOL-AVE = COBOL-PLUS / 3
            COMPUTE JOHO-AVE = JOHO-PLUS / 3
            COMPUTE ARUGO-AVE = ARUGO-PLUS / 3    
            DISPLAY "COBOL�̕��ϓ_��" COBOL-AVE "�ł��B"   
            DISPLAY "��񐔊w�̕��ϓ_��" JOHO-AVE "�ł��B"
            DISPLAY "�A���S���Y���̕��ϓ_��" ARUGO-AVE "�ł��B"

       �@�@�@�@�@
           STOP         RUN.