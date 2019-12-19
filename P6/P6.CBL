000100 IDENTIFICATION            DIVISION.
000200 PROGRAM-ID.               P06. 
000300*
000400 ENVIRONMENT               DIVISION. 
000500 INPUT-OUTPUT              SECTION.
000600 FILE-CONTROL.
000700     SELECT  IN-FILE     ASSIGN  "仕入整列.txt"
000800                              ORGANIZATION LINE SEQUENTIAL.
000900     SELECT  PRINT-FILE  ASSIGN  "P06.DOC"
001000                              ORGANIZATION LINE SEQUENTIAL. 
001100*
001200 DATA                      DIVISION.
001300 FILE                      SECTION. 
001400 FD  IN-FILE.
001500 01  I-REC.
001600     05  I-NO              PIC X(05).
001700     05  I-NAME            PIC X(10).
001800     05  I-S-DATE          PIC 9(06).
001900     05  I-S-NO            PIC X(03).
002000     05  I-S-NAME          PIC X(10).
002100     05  I-SURYO           PIC 9(04).
002200     05  I-TANKA           PIC 9(05).   
002300*
002400 FD  PRINT-FILE.
002500 01  P-REC                 PIC X(78).
002600* 
002700 WORKING-STORAGE           SECTION.
002800 01  END-FLG               PIC X(01)  VALUE SPACE.
002900 01  L-CNT                 PIC 9(03).
003000 01　N-PAGEKAZU            PIC 9(03)  VALUE 0.
003100 01  N-NUM                 PIC 9(03)  VALUE 0.   
003200 01  KINGAKU               PIC 9(18) VALUE 0. 
003300 01  G-KINGAKU             PIC 9(18) VALUE 0.
003400 01  A-SPACE               PIC X(30) VALUE SPACE.    
003500 01  HOZON                 PIC X(05) VALUE SPACE.
003600 01  SHOKEI                PIC 9(07) VALUE 0.
003700 01  MEISAI.
003800     05  M-SEQ             PIC Z9.
003900     05                    PIC X(01)  VALUE SPACE.
004000     05  M-NO              PIC X(05).
004100     05                    PIC X(04)  VALUE SPACE.
004200     05  M-NAME            PIC X(10).
004300     05                    PIC X(01)  VALUE SPACE.
004400     05  M-S-DATE          PIC 99/99/99.
004500     05                    PIC X(01)  VALUE SPACE.
004600     05  M-S-NO            PIC X(03).
004700     05                    PIC X(01)  VALUE SPACE.
004800     05  M-S-NAME          PIC X(10).
004900     05                    PIC X(01)  VALUE SPACE.
005000     05  M-SURYO           PIC Z,ZZ9.
005100     05                    PIC X(02)  VALUE SPACE.
005200     05  M-TANKA           PIC Z,ZZ9.
005300     05                    PIC X(02)  VALUE SPACE.
005400     05  M-KINGAKU         PIC Z,ZZZ,ZZ9. 
005500*
005600 01  HEAD-1.
005700     05                    PIC X(09)  VALUE "14jy0123".
005800     05                    PIC X(15)  VALUE "崔禎文".
005900     05                    PIC X(29)  VALUE
006000                           "*** 仕入データ一覧表".
006100     05                    PIC X(06)  VALUE "日付：".
006200     05 H-DATE             PIC 99/99/99.
006300     05                    PIC X(02)  VALUE SPACE.
006400     05                    PIC X(05)  VALUE "PAGE:".
006500     05 H-PAGE             PIC ZZ9.
006600*
006700 01  HEAD-2.
006800     05                    PIC X(23)  VALUE  "SEQ 商品".
006900     05                    PIC X(25)  VALUE  "仕入".
007000     05                    PIC X(23)  VALUE  
007100                           "数量   単価       金額". 
007200 01  HEAD-3.
007300     05                    PIC X(33)  VALUE
007400                          "      NO    名前       日付".
007500     05                    PIC X(27)  VALUE
007600                          "NO  名前".
007700*
007800 01  FOOT-1.
007900     05                    PIC X(51)  VALUE SPACE.
008000     05                    PIC X(10)  VALUE "小計：".
008100     05  F-SHOKEI          PIC Z,ZZZ,ZZ9.
008200*
008300 01  FOOT-2.
008400     05                    PIC X(51)  VALUE SPACE.
008500     05                    PIC X(10)  VALUE "合計：".
008600     05  F-GOKEI           PIC Z,ZZZ,ZZ9.
008700* 
008800  PROCEDURE        DIVISION.    
008900 MOOO.
009000      OPEN    INPUT  IN-FILE OUTPUT PRINT-FILE   
009100      MOVE   21   TO L-CNT 
009200      ACCEPT H-DATE FROM DATE
009300      READ    IN-FILE
009400          AT END MOVE "E" TO END-FLG
009500      END-READ
009600      PERFORM UNTIL END-FLG = "E"
009700          MOVE I-NO TO HOZON
009800          PERFORM UNTIL I-NO NOT = HOZON OR END-FLG = "E"   
009900              IF L-CNT >= 20
010000              THEN    
010100                MOVE SPACE TO P-REC
010200                WRITE P-REC AFTER PAGE      
010300                COMPUTE N-PAGEKAZU = N-PAGEKAZU + 1
010400                MOVE N-PAGEKAZU TO H-PAGE     
010500                WRITE P-REC FROM HEAD-1 AFTER 1
010600                WRITE P-REC FROM HEAD-2 AFTER 2
010700                WRITE P-REC FROM HEAD-3 AFTER 1  
010800                INITIALIZE L-CNT
010900              ELSE
011000                CONTINUE
011100              END-IF
011200
011300              COMPUTE N-NUM = N-NUM + 1
011400              MOVE N-NUM TO M-SEQ 
011500              MOVE I-NO TO M-NO
011600              MOVE I-NAME TO M-NAME 
011700              MOVE I-S-DATE TO M-S-DATE
011800              MOVE I-S-NO TO M-S-NO
011900              MOVE I-S-NAME TO M-S-NAME
012000              MOVE I-SURYO TO M-SURYO
012100              MOVE I-TANKA TO M-TANKA 
012200              COMPUTE L-CNT = L-CNT + 1 
012300              COMPUTE M-KINGAKU = I-TANKA * I-SURYO 
012400              COMPUTE KINGAKU = I-TANKA * I-SURYO   
012500              COMPUTE SHOKEI = KINGAKU +SHOKEI
012600              COMPUTE G-KINGAKU = G-KINGAKU + KINGAKU      
012700              WRITE P-REC FROM MEISAI AFTER 1
012800              READ    IN-FILE
012900                  AT END MOVE "E" TO END-FLG
013000              END-READ  
013100          END-PERFORM
013200          MOVE SHOKEI TO F-SHOKEI
013300          WRITE P-REC FROM FOOT-1 AFTER 1
013400          INITIALIZE SHOKEI
013500      END-PERFORM    
013600  
013700      MOVE G-KINGAKU TO F-GOKEI 
013800      WRITE P-REC FROM FOOT-2 AFTER 2
013900      CLOSE IN-FILE PRINT-FILE    
014000      DISPLAY "END"
014100      STOP RUN.
014200
