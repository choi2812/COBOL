000100 IDENTIFICATION            DIVISION.
000200 PROGRAM-ID.               P7. 
000300*
000400 ENVIRONMENT               DIVISION. 
000500 INPUT-OUTPUT              SECTION.
000600 FILE-CONTROL.
000700     SELECT  IN-FILE     ASSIGN  "仕入.txt"
000800                              ORGANIZATION LINE SEQUENTIAL.
000900     SELECT  PRINT-FILE  ASSIGN  "P7.DOC"
001000                              ORGANIZATION LINE SEQUENTIAL. 
001100*
001200 DATA                      DIVISION.
001300 FILE                      SECTION. 
001400 FD  IN-FILE.
001500 01  I-REC.
001600     05  I-NO.
001700         10                PIC X(04).
001800         10  I-IDX         PIC 9(01).
001900     05  I-NAME            PIC X(10).
002000     05  I-S-DATE          PIC 9(06).
002100     05  I-S-NO            PIC X(03).
002200     05  I-S-NAME          PIC X(10).
002300     05  I-SURYO           PIC 9(04).
002400     05  I-TANKA           PIC 9(05).
002500*
002600 FD  PRINT-FILE.
002700 01  P-REC                 PIC X(78).
002800* 
002900 WORKING-STORAGE           SECTION. 
       01  END-FLG               PIC X(01)  VALUE SPACE.
       01  L-CNT                 PIC 9(03).
       01　N-PAGEKAZU            PIC 9(03)  VALUE 0.
       01  N-NUM                 PIC 9(03)  VALUE 0.   
       01  KINGAKU               PIC 9(18) VALUE 0. 
       01  G-KINGAKU             PIC 9(18) VALUE 0.
       01  A-SPACE               PIC X(30) VALUE SPACE.   
       01  I                     PIC 9(01).
       01  NUM                   PIC 9(01).   
      *

       01 SUMMRY-TBL.
            05  SURYOU-SUM         PIC 9(04) OCCURS 5 TIMES.
            05  KINGAKU-SUM        PIC 9(07) OCCURS 5 TIMES.
      *
004400 01  MEISAI.
004500     05  M-SEQ             PIC Z9.
004600     05                    PIC X(01)  VALUE SPACE.
004700     05  M-NO              PIC X(05).
004800     05                    PIC X(04)  VALUE SPACE.
004900     05  M-NAME            PIC X(10).
005000     05                    PIC X(01)  VALUE SPACE.
005100     05  M-S-DATE          PIC 99/99/99.
005200     05                    PIC X(01)  VALUE SPACE.
005300     05  M-S-NO            PIC X(03).
005400     05                    PIC X(01)  VALUE SPACE.
005500     05  M-S-NAME          PIC X(10).
005600     05                    PIC X(01)  VALUE SPACE.
005700     05  M-SURYO           PIC Z,ZZ9.
005800     05                    PIC X(02)  VALUE SPACE.
005900     05  M-TANKA           PIC Z,ZZ9.
006000     05                    PIC X(04)  VALUE SPACE.
006100     05  M-KINGAKU         PIC ZZZ,ZZ9. 
006200*
006300 01  HEAD-1.
006400     05                    PIC X(09)  VALUE "14JY0123".
006500     05                    PIC X(15)  VALUE "崔禎文".
006600     05                    PIC X(29)  VALUE
006700                           "*** 仕入データ一覧表".
006800     05                    PIC X(06)  VALUE "日付：".
006900     05 H-DATE             PIC 99/99/99.
007000     05                    PIC X(02)  VALUE SPACE.
007100     05                    PIC X(05)  VALUE "PAGE:".
007200     05 H-PAGE             PIC ZZ9.
007300*
007400 01  HEAD-2.
007500     05                    PIC X(23)  VALUE  "SEQ 商品".
007600     05                    PIC X(25)  VALUE  "仕入".
007700     05                    PIC X(23)  VALUE  
007800                           "数量   単価       金額". 
007900 01  HEAD-3.
008000     05                    PIC X(33)  VALUE
008100                          "      NO    名前       日付".
008200     05                    PIC X(27)  VALUE
008300                          "NO  名前".
008400*
008500 01  FOOT-1.
008600     05                    PIC X(37)  VALUE SPACE.
008700     05                    PIC X(05)  VALUE "小計(".
008800     05  F-IDX             PIC 9(01).
008900     05                    PIC X(02)  VALUE "):".
009000     05  F-SURYO           PIC ZZ,ZZ9.
009100     05                    PIC X(09)  VALUE SPACE.
009200     05  F-KINGAKU         PIC ZZ,ZZZ,ZZ9.
         PROCEDURE DIVISION.        
            OPEN    INPUT  IN-FILE OUTPUT PRINT-FILE
            INITIALIZE  SUMMRY-TBL    
            MOVE   21   TO L-CNT  
            ACCEPT H-DATE  FROM DATE
            READ    IN-FILE
                AT END MOVE "E" TO END-FLG
            END-READ
            PERFORM UNTIL END-FLG = "E" 
                IF L-CNT >= 20
                    THEN    
                      MOVE SPACE TO P-REC
                      WRITE P-REC AFTER PAGE      
                      COMPUTE N-PAGEKAZU = N-PAGEKAZU + 1
                      MOVE N-PAGEKAZU TO H-PAGE    
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
                COMPUTE SURYOU-SUM(I-IDX) = SURYOU-SUM(I-IDX) + I-SURYO
                MOVE I-TANKA TO M-TANKA 
                COMPUTE L-CNT = L-CNT + 1 
                COMPUTE M-KINGAKU = I-TANKA * I-SURYO 
                COMPUTE KINGAKU = I-TANKA * I-SURYO
                COMPUTE KINGAKU-SUM(I-IDX) = KINGAKU-SUM(I-IDX) + 
                        KINGAKU    
                WRITE P-REC FROM MEISAI AFTER 1
                READ    IN-FILE
                    AT END MOVE "E" TO END-FLG
                END-READ
            END-PERFORM   

            PERFORM VARYING NUM  FROM 1 BY 1 UNTIL NUM > 5
                MOVE NUM TO F-IDX 
                MOVE SURYOU-SUM(NUM) TO F-SURYO 
                MOVE KINGAKU-SUM(NUM) TO F-KINGAKU 
                WRITE P-REC FROM  FOOT-1 AFTER 1
            END-PERFORM
            CLOSE IN-FILE PRINT-FILE    
            DISPLAY "END"
            STOP RUN.
                       
       
    















































