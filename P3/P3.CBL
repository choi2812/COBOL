      *
      * P3
      *14JY0123 崔禎文
      *
       IDENTIFICATION            DIVISION.
       PROGRAM-ID.               P2. 
       ENVIRONMENT               DIVISION. 
       INPUT-OUTPUT              SECTION.
       FILE-CONTROL.
           SELECT  IN-FILE     ASSIGN  "仕入.txt"
                                    ORGANIZATION LINE SEQUENTIAL.
           SELECT  PRINT-FILE  ASSIGN  "P3.DOC"
                                    ORGANIZATION LINE SEQUENTIAL. 
      *
       DATA                      DIVISION.
       FILE                      SECTION. 
       FD  IN-FILE.
       01  I-REC.
           05  I-NO              PIC X(05).
           05  I-NAME            PIC X(10).
           05  I-S-DATE          PIC 9(06).
           05  I-S-NO            PIC X(03).
           05  I-S-NAME          PIC X(10).
           05  I-SURYO           PIC 9(04).
           05  I-TANKA           PIC 9(05).
      *
       FD  PRINT-FILE.
       01  P-REC                 PIC X(78).
      * 
       WORKING-STORAGE           SECTION.
       01  END-FLG               PIC X(01)  VALUE SPACE.   
      *
       01  MEISAI.
           05                    PIC X(02)  VALUE SPACE.
           05  M-NO              PIC X(05).
           05                    PIC X(03)  VALUE SPACE.
           05  M-NAME            PIC X(10).
           05                    PIC X(01)  VALUE SPACE.
           05  M-S-DATE          PIC 99/99/99.
           05                    PIC X(01)  VALUE SPACE.
           05  M-S-NO            PIC X(03).
           05                    PIC X(01)  VALUE SPACE.
           05  M-S-NAME          PIC X(10).
           05                    PIC X(04)  VALUE SPACE.
           05  M-SURYO           PIC Z,ZZ9.
           05                    PIC X(02)  VALUE SPACE.
           05  M-TANKA           PIC Z,ZZ9.
           05                    PIC X(04)  VALUE SPACE. 
           05  M-KINGAKU         PIC Z,ZZZ,ZZ9. 
      *
       01  HEAD.
           05                    PIC X(02)  VALUE SPACE.
           05  NAMAE              PIC X(15)  VALUE 
               "14JY0123 崔禎文". 
           05                    PIC X(07)  VALUE SPACE.
           05  H-SIIRE           PIC X(26)  VALUE 
               "***　仕入データ一覧表　***".
           05                    PIC X(11)  VALUE SPACE. 
           05  H-SURYOU          PIC X(06)  VALUE "日付：".
      * 
       01  HIZUKE                PIC 99/99/99.
      * 
       01  HEAD2.   
           05                    PIC X(02)  VALUE SPACE.
           05 H-SHOHIN           PIC X(04)  VALUE "商品". 
           05                    PIC X(16)  VALUE SPACE.
           05 H-SIIRE            PIC X(04)  VALUE "仕入".
           05                    PIC X(24)  VALUE SPACE. 
           05 H-SURYOU           PIC X(04)  VALUE "数量".
           05                    PIC X(04)  VALUE SPACE.
           05 H-TANKA            PIC X(04)  VALUE "単価".
           05                    PIC X(08)  VALUE SPACE.
           05 H-TANKA            PIC X(04)  VALUE "金額".
      * 
       01  HEAD3.
           05                    PIC X(02)  VALUE SPACE.
           05 H-M-NO             PIC X(02)  VALUE "NO".
           05                    PIC X(05)  VALUE SPACE.
           05 H-M-NAME           PIC X(04)  VALUE "名前".
           05                    PIC X(09)  VALUE SPACE.
           05 H-M-S-DATE         PIC X(04)  VALUE "日付".  
           05                    PIC X(05)  VALUE SPACE.
           05 H-M-S-NO           PIC X(02)  VALUE "NO".
           05                    PIC X(02)  VALUE SPACE.
           05 H-M-S-NAME         PIC X(04)  VALUE "名前".        
      * 
       01  KINGAKU               PIC 9(18) VALUE 0. 
       01  G-KINGAKU             PIC 9(18) VALUE 0.
       01  A-SPACE               PIC X(30) VALUE SPACE.
      * 
       01  FOOT.
           05                    PIC X(50) VALUE SPACE.
           05                    PIC X(14) VALUE "合計：".
           05  GOKEI             PIC Z,ZZZ,ZZ9.
       PROCEDURE DIVISION.
            OPEN    INPUT  IN-FILE OUTPUT PRINT-FILE     
            WRITE   P-REC  FROM  HEAD AFTER 1 
            ACCEPT HIZUKE FROM DATE
            WRITE   P-REC  FROM  HIZUKE  AFTER 0
            WRITE   P-REC  FROM  A-SPACE AFTER 1  
            WRITE   P-REC  FROM  HEAD2 AFTER　1
            WRITE   P-REC　FROM  HEAD3 AFTER  1

            READ    IN-FILE
                AT END MOVE "E" TO END-FLG
            END-READ
            PERFORM UNTIL END-FLG = "E" 
                MOVE I-NO TO M-NO
                MOVE I-NAME TO M-NAME 
                MOVE I-S-DATE TO M-S-DATE
                MOVE I-S-NO TO M-S-NO
                MOVE I-S-NAME TO M-S-NAME
                MOVE I-SURYO TO M-SURYO
                MOVE I-TANKA TO M-TANKA
                COMPUTE M-KINGAKU = I-TANKA * I-SURYO 
                COMPUTE KINGAKU = I-TANKA * I-SURYO
                COMPUTE G-KINGAKU = G-KINGAKU + KINGAKU      
                WRITE P-REC FROM MEISAI AFTER 1
                READ    IN-FILE
                    AT END MOVE "E" TO END-FLG
                END-READ
            END-PERFORM  
            MOVE G-KINGAKU TO GOKEI
            WRITE P-REC FROM FOOT AFTER 2
            CLOSE IN-FILE PRINT-FILE    
            DISPLAY "END"
            STOP RUN.
