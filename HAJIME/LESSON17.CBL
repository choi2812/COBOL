      * 
      * 14JY0123 崔　禎　文
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
            DISPLAY "名前を入力してください。"
            ACCEPT   NAMAE  
            DISPLAY "COBOLの点数を入力してください。"
            ACCEPT   SUJI1
            COMPUTE  COBOL-PLUS = SUJI1 + COBOL-PLUS
            DISPLAY "情報数学の点数を入力してください。"
            ACCEPT   SUJI2    
            COMPUTE  JOHO-PLUS = SUJI2 + JOHO-PLUS
            DISPLAY "アルゴリズムの点数をを入力してください。"
            ACCEPT   SUJI3 
            COMPUTE  ARUGO-PLUS  = SUJI3 + ARUGO-PLUS 
            COMPUTE  SUJI-AVE = SUJI1 + SUJI2 + SUJI3 / 3 
            DISPLAY  NAMAE "の3科目の平均は" SUJI-AVE "点です。" 
            DISPLAY  " "  　     
           END-PERFORM   
            COMPUTE COBOL-AVE = COBOL-PLUS / 3
            COMPUTE JOHO-AVE = JOHO-PLUS / 3
            COMPUTE ARUGO-AVE = ARUGO-PLUS / 3    
            DISPLAY "COBOLの平均点は" COBOL-AVE "です。"   
            DISPLAY "情報数学の平均点は" JOHO-AVE "です。"
            DISPLAY "アルゴリズムの平均点は" ARUGO-AVE "です。"

       　　　　　
           STOP         RUN.
