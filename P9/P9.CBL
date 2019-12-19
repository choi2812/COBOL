       IDENTIFICATION            DIVISION.
       PROGRAM-ID.               P9. 
      *
       ENVIRONMENT               DIVISION. 
       INPUT-OUTPUT              SECTION.
       FILE-CONTROL.
           SELECT  IN-FILE     ASSIGN  "仕入3.txt"
                                    ORGANIZATION LINE SEQUENTIAL.
           SELECT  PRINT-FILE  ASSIGN  "P9.DOC"
                                    ORGANIZATION LINE SEQUENTIAL. 
      *
       DATA                      DIVISION.
       FILE                      SECTION. 
       FD  IN-FILE.
       01  I-REC.
           05  I-NO              PIC X(05).
           05  I-S-DATE          PIC 9(06).
           05  I-S-NO            PIC X(03).
           05  I-SURYO           PIC 9(04).
      *
       FD  PRINT-FILE.
       01  P-REC                 PIC X(78).
      * 
       WORKING-STORAGE           SECTION.
       01  END-FLG               PIC X(01)  VALUE SPACE.   
       01  W-SEQ                 PIC 9(02)  VALUE 0.
       01  L-CNT                 PIC 9(02)  VALUE 20.
       01  P-CNT                 PIC 9(03)  VALUE 0. 
       01  I                     PIC 9(02). 
       01  WORKC.
           05 W-KINGAKU          PIC 9(08).
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
           05  M-SURYO           PIC Z,ZZ9.
           05                    PIC X(05)  VALUE SPACE.
           05  M-TANKA           PIC ZZ,ZZ9.
           05                    PIC X(01)  VALUE SPACE.
           05  M-KINGAKU         PIC ZZ,ZZZ,ZZ9. 
      *
       01  HEAD-1.
           05                    PIC X(09)  VALUE "14JY0123".
           05                    PIC X(15)  VALUE "崔禎文".
           05                    PIC X(29)  VALUE
                                 "*** 仕入データ一覧表".
           05                    PIC X(06)  VALUE "日付：".
           05 H-DATE             PIC 99/99/99.
           05                    PIC X(02)  VALUE SPACE.
           05                    PIC X(05)  VALUE "PAGE:".
           05 H-PAGE             PIC ZZ9.
      *
       01  HEAD-2.
           05                    PIC X(23)  VALUE  "SEQ 商品".
           05                    PIC X(14)  VALUE  "仕入".
           05                    PIC X(33)  VALUE  
                                 "数量       単価       金額". 
       01  HEAD-3.
           05                    PIC X(33)  VALUE
                                "      NO    名前       日付".
           05                    PIC X(27)  VALUE
                                "NO  ".
          01  TBL1.
           03                   PIC X(25)
                                  VALUE "CBB03キャベツ  0105001155".
           03                   PIC X(25)
                                  VALUE "CRR02人参      0231002541".
           03                   PIC X(25)
                                  VALUE "LTT04レタス    0241502656".
           03                   PIC X(25)
                                  VALUE "RDS01大根      0136501501".
           03                   PIC X(25)
                                  VALUE "SPN05ホウレン草0015800173".
       01  TBL2                 REDEFINES  TBL1.
           03                   OCCURS 5.
             05  T-NO           PIC X(05).
             05  T-NAME         PIC X(10).
             05  T-S-TANKA      PIC 9(05).
             05  T-H-TANKA      PIC 9(05).

      *
                                              
         PROCEDURE                DIVISION.
       START-RTN.
           OPEN  INPUT  IN-FILE  OUTPUT  PRINT-FILE.
           PERFORM UNTIL END-FLG = "E"
             READ IN-FILE
               AT END
                 MOVE "E" TO END-FLG
               NOT AT END
                 PERFORM WORK-MEDASI
                 PERFORM WORK-L-WRITE
             END-READ
           END-PERFORM
           CLOSE  IN-FILE  PRINT-FILE
           DISPLAY "end。"
           STOP  RUN.
       WORK-MEDASI.
             IF L-CNT > 19 THEN
               ACCEPT  H-DATE FROM  DATE
               ADD 1 TO P-CNT
               MOVE P-CNT TO H-PAGE
               WRITE P-REC FROM HEAD-1 AFTER PAGE
               WRITE P-REC FROM HEAD-2 AFTER 2
               WRITE P-REC FROM HEAD-3 AFTER 1
               INITIALIZE L-CNT
             END-IF.

       WORK-L-WRITE.
             ADD 1 TO W-SEQ
             ADD 1 TO L-CNT
             MOVE 1 TO I
                 MOVE "*エラ-*" TO M-NAME
                 MOVE 0 TO M-TANKA
                 MOVE 0 TO M-KINGAKU
             PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               IF T-NO(I) = I-NO
                 THEN
                   COMPUTE W-KINGAKU = T-S-TANKA(I) * I-SURYO
                   MOVE T-NAME(I) TO M-NAME
                   MOVE T-S-TANKA(I) TO M-TANKA
                   MOVE W-KINGAKU TO M-KINGAKU
                 ELSE CONTINUE
               END-IF
             END-PERFORM
             MOVE W-SEQ TO M-SEQ
             MOVE I-NO TO M-NO
             MOVE I-S-DATE TO M-S-DATE
             MOVE I-S-NO TO M-S-NO
             MOVE I-SURYO TO M-SURYO
             WRITE P-REC FROM MEISAI AFTER 1.
