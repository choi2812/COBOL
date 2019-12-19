       IDENTIFICATION            DIVISION.
       PROGRAM-ID.               P10. 
      *
       ENVIRONMENT               DIVISION. 
       INPUT-OUTPUT              SECTION.
       FILE-CONTROL.
           SELECT  IN-FILE     ASSIGN  "仕入TRAN.txt"
                                    ORGANIZATION LINE SEQUENTIAL.
           SELECT  OUT-FILE    ASSIGN  "P10.DOC"
                                    ORGANIZATION LINE SEQUENTIAL.
           SELECT  SHOHIN-FILE ASSIGN  "ISAM仕入先マスタ"
                                    ORGANIZATION INDEXED
                                    ACCESS MODE RANDOM
                                    RECORD KEY S-CODE.
      *
       DATA                      DIVISION.
       FILE                      SECTION. 
       FD  IN-FILE.
       01  I-REC.
           05  I-CODE            PIC X(01).
           05  I-DATA.
               07  I-S-CODE      PIC X(03).
               07  I-SYOTEN      PIC X(20).
               07  I-TEL         PIC X(12).
      *
       FD  OUT-FILE.
       01  O-REC                 PIC X(78).
      *
       FD  SHOHIN-FILE.
       01  S-REC.
           05  S-CODE            PIC X(03).
           05  S-SYOTEN          PIC X(20).
           05  S-TEL             PIC X(12).
      * 
       WORKING-STORAGE           SECTION.
       01  END-FLG               PIC X(01)  VALUE SPACE.   
      * 
       01  MEISAI.
           05                    PIC X(05)  VALUE SPACE.
           05  M-CODE            PIC X(01).
           05                    PIC X(08)  VALUE SPACE.
           05  M-DATA.
               07  M-S-CODE      PIC X(03).
               07                PIC X(20)  VALUE SPACE.
               07  M-TEL         PIC X(12)  VALUE SPACE.
           05                    PIC X(03)  VALUE SPACE.
           05  M-COMMENT         PIC X(16). 
      *
       01  HEAD-1.
           05                    PIC X(09)  VALUE "14JY0123".
           05                    PIC X(15)  VALUE "崔　禎　文".
           05                    PIC X(29)  VALUE
                                 "*** 商品マスタ更新".
           05                    PIC X(06)  VALUE "日付：".
           05 H-DATE             PIC 99/99/99.
      *
       01  HEAD-2.
           05                    PIC X(13)  VALUE  "処理コード".
           05                    PIC X(38)  VALUE  "仕入TRANレコード".
           05                    PIC X(08)  VALUE  "コメント".
      *                                                        
       PROCEDURE                 DIVISION.
       MAIN-SHORI.
        OPEN INPUT IN-FILE
             OUTPUT OUT-FILE
             I-O SHOHIN-FILE
      *
       ACCEPT H-DATE FROM DATE
       WRITE O-REC FROM HEAD-1 AFTER PAGE
       WRITE O-REC FROM HEAD-2 AFTER 2 
      *
       READ IN-FILE
            AT END
                MOVE "E" TO END-FLG
       END-READ
      *
       PERFORM UNTIL END-FLG = "E" 
            MOVE SPACE TO MEISAI
            MOVE I-CODE TO M-CODE
            
            EVALUATE I-CODE
                WHEN "T" PERFORM  ADD-DATA-PROC
                WHEN "K" PERFORM　UPDATE-TEL-PROC
                WHEN "S" PERFORM  DELETE-DATA-PROC
                WHEN OTHER
                    MOVE "処理コ－ドエラ－" TO M-COMMENT
                    MOVE I-DATA TO M-DATA
            END-EVALUATE

            WRITE  O-REC  FROM  MEISAI AFTER 1

            READ IN-FILE
               AT END MOVE "E" TO END-FLG
            END-READ
        END-PERFORM

           CLOSE IN-FILE  SHOHIN-FILE OUT-FILE
           DISPLAY "end"
           STOP RUN.

       ADD-DATA-PROC.
           MOVE I-S-CODE TO S-CODE
           MOVE I-SYOTEN TO S-SYOTEN
           MOVE I-TEL TO S-TEL
           WRITE S-REC
             INVALID KEY
               MOVE "WRITEエラー"  TO M-COMMENT
             NOT INVALID KEY
               MOVE I-DATA TO M-DATA
               MOVE "追加" TO M-COMMENT
           END-WRITE.

       UPDATE-TEL-PROC.
           MOVE I-S-CODE TO S-CODE
           MOVE I-SYOTEN TO S-SYOTEN
           MOVE I-TEL TO S-TEL
           READ SHOHIN-FILE
             INVALID KEY
               MOVE "READエラー" TO M-COMMENT
             NOT INVALID KEY
               REWRITE S-REC
                 INVALID KEY
                   MOVE "REWRITEエラー"  TO M-COMMENT
                 NOT INVALID KEY
                   MOVE "変更" TO M-COMMENT
                   MOVE I-TEL TO M-TEL
               END-REWRITE
           END-READ.

       DELETE-DATA-PROC.
           MOVE I-S-CODE TO S-CODE

           DELETE SHOHIN-FILE
             INVALID KEY
               MOVE "DELETEエラー"  TO M-COMMENT
             NOT INVALID KEY
               MOVE "削除" TO M-COMMENT
           END-DELETE.

              
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       


            
      
