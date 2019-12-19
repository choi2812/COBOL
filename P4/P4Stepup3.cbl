      *87m
      *14JY0123 崔禎文
      *

       IDENTIFICATION            DIVISION.
       PROGRAM-ID.               P4. 
      *
       ENVIRONMENT               DIVISION. 
       INPUT-OUTPUT              SECTION.
       FILE-CONTROL.
           SELECT  IN-FILE     ASSIGN  "仕入.txt"
                                    ORGANIZATION LINE SEQUENTIAL.
           SELECT  PRINT-FILE  ASSIGN  "P4Stepup3.DOC"
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
       01  L-CNT                 PIC 9(03).
       01　N-PAGEKAZU            PIC 9(03) VALUE 0.
       01  N-NUM                 PIC 9(03) VALUE 0.   
       01  KINGAKU               PIC 9(18) VALUE 0. 
       01  G-KINGAKU             PIC 9(18) VALUE 0.
       01  A-SPACE               PIC X(30) VALUE SPACE.
       01  N-PAGEGOKEI           PIC 9(18) VALUE 0.
      *
       01  MEISAI.
           05  M-SEQ             PIC Z9.
           05                    PIC X(01)  VALUE SPACE.
           05  M-NO              PIC X(05).
           05                    PIC X(04)  VALUE SPACE.
           05  M-NAME            PIC X(10).
           05                    PIC X(01)  VALUE SPACE.
           05  M-S-DATE          PIC 99/99/99.
           05                    PIC X(01)  VALUE SPACE.
           05  M-S-NO            PIC X(03).
           05                    PIC X(01)  VALUE SPACE.
           05  M-S-NAME          PIC X(10).
           05                    PIC X(01)  VALUE SPACE.
           05  M-SURYO           PIC Z,ZZ9.
           05                    PIC X(02)  VALUE SPACE.
           05  M-TANKA           PIC Z,ZZ9.
           05                    PIC X(04)  VALUE SPACE.
           05  M-KINGAKU         PIC ZZZ,ZZ9. 
      *
       01  HEAD-1.
           05                    PIC X(09)  VALUE "14JY0123".
           05                    PIC X(15)  VALUE "崔禎文".
           05                    PIC X(29)  VALUE
                                 "*** 仕入データ一覧表".
           05                    PIC X(06)  VALUE "日付：".
           05 H1-DATE            PIC 99/99/99.
           05                    PIC X(02)  VALUE SPACE.
           05                    PIC X(05)  VALUE "PAGE:".
           05 H1-PAGE            PIC ZZ9.
      *
       01  HEAD-2.
           05                    PIC X(23)  VALUE  "SEQ 商品".
           05                    PIC X(25)  VALUE  "仕入".
           05                    PIC X(23)  VALUE  
                                 "数量   単価       金額". 
       01  HEAD-3.
           05                    PIC X(33)  VALUE
                                "      NO    名前       日付".
           05                    PIC X(27)  VALUE
                                "NO  名前".
      *
       01  FOOT-1.
           05                    PIC X(51)  VALUE SPACE.
           05                    PIC X(10)  VALUE "合計：".
           05   F1-GOKEI          PIC Z,ZZZ,ZZ9.     
       
       01  FOOT-2.
           05                    PIC X(46)  VALUE SPACE.
           05                    PIC X(15)  VALUE "ページ合計：".
           05   PAGEGOKEI        PIC Z,ZZZ,ZZ9. 

       PROCEDURE DIVISION.
            OPEN    INPUT  IN-FILE OUTPUT PRINT-FILE   
            MOVE   21   TO L-CNT  
            ACCEPT H1-DATE FROM DATE
            READ    IN-FILE
                AT END MOVE "E" TO END-FLG
            END-READ 
            PERFORM UNTIL END-FLG = "E"   
                 IF L-CNT >= 20
                    THEN    
                      MOVE SPACE TO P-REC  
                      WRITE P-REC AFTER PAGE      
                      COMPUTE N-PAGEKAZU = N-PAGEKAZU + 1
                      MOVE N-PAGEKAZU TO H1-PAGE     
                      WRITE P-REC FROM HEAD-1 AFTER 1
                      WRITE P-REC FROM HEAD-2 AFTER 2
                      WRITE P-REC FROM HEAD-3 AFTER 1  
                      INITIALIZE L-CNT
                    ELSE
                      CONTINUE
                END-IF
                COMPUTE N-NUM = N-NUM + 1
                MOVE N-NUM TO M-SEQ 
                MOVE I-NO TO M-NO
                MOVE I-NAME TO M-NAME 
                MOVE I-S-DATE TO M-S-DATE
                MOVE I-S-NO TO M-S-NO
                MOVE I-S-NAME TO M-S-NAME
                MOVE I-SURYO TO M-SURYO
                MOVE I-TANKA TO M-TANKA 
                COMPUTE L-CNT = L-CNT + 1 
                COMPUTE M-KINGAKU = I-TANKA * I-SURYO 
                COMPUTE KINGAKU = I-TANKA * I-SURYO
                COMPUTE G-KINGAKU = G-KINGAKU + KINGAKU
                COMPUTE N-PAGEGOKEI = N-PAGEGOKEI + KINGAKU
                MOVE N-PAGEGOKEI TO PAGEGOKEI
      　　　　　WRITE P-REC FROM MEISAI AFTER 1  
                READ    IN-FILE
                    AT END MOVE "E" TO END-FLG    
                END-READ
                IF L-CNT >= 20 
                  WRITE P-REC FROM FOOT-2 AFTER  3
                END-IF

            END-PERFORM
                WRITE P-REC FROM FOOT-2 AFTER  3
            MOVE G-KINGAKU TO F1-GOKEI
            WRITE P-REC FROM FOOT-1 AFTER 3
            CLOSE IN-FILE PRINT-FILE    
            DISPLAY "END"
            STOP RUN.


