       IDENTIFICATION       DIVISION.
       PROGRAM-ID           Exercise03.
       
      *ENVIRONMENT          DIVISION.
       
       DATA                 DIVISION.
       WORKING-STORAGE      SECTION.
       01   SUJI1           PIC 9(3).
       01   SUJI2           PIC 9(3).
       01   GOUKEI          PIC 9(4).
       
       PROCEDURE            DIVISION.
           DISPLAY  "ひとつめの数字を入力してください"
           ACCEPT   SUJI1
           DISPLAY  "ふたつめの数字を入力してください"
           ACCEPT   SUJI2
           COMPUTE  GOUKEI = SUJI1 + SUJI2
           DISPLAY  "数字の合計は" GOUKEI "です"
                                               
           STOP RUN. 
