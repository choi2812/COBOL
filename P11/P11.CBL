       IDENTIFICATION                   DIVISION.
       PROGRAM-ID.                      P11.
      *
       ENVIRONMENT                      DIVISION.
       INPUT-OUTPUT                     SECTION.
       FILE-CONTROL.
           SELECT  MAS-FILE             ASSIGN  
                                        "仕入先MASTER.TXT"
                                        ORGANIZATION LINE SEQUENTIAL.
           SELECT  TRAN-FILE            ASSIGN  
                                        "仕入先TRAN.TXT"
                                        ORGANIZATION LINE SEQUENTIAL.
           SELECT  N-MAS-FILE           ASSIGN  
                                        "仕入先NEWMASTER.TXT"
                                        ORGANIZATION LINE SEQUENTIAL.
           SELECT  LIST-FILE            ASSIGN  "P11.DOC"
                                        ORGANIZATION LINE SEQUENTIAL.
      *
       DATA                             DIVISION.
       FILE                             SECTION.
       FD  MAS-FILE.
       01  MAS-REC.   
           03  M-CODE                   PIC X(03). 
           03  M-NAME                   PIC X(10).
           03  M-ADL                    PIC X(10).
           03  M-TEL                    PIC X(12).
       FD  TRAN-FILE.
       01  TRAN-REC.   
           03  S-CODE                   PIC X(01). 
           03  K-CODE                   PIC X(01).
           03  K-DATE.
               04  T-CODE               PIC X(03).
               04  T-DATE.
                   05  T-NAME               PIC X(10).
                   05  T-ADL                PIC X(10).
                   05  T-TEL                PIC X(12).
                   05  T-COM                PIC X(05).     
       FD  N-MAS-FILE.
       01  N-MAS-REC.   
           03  N-CODE                   PIC X(03). 
           03  N-NAME                   PIC X(10).
           03  N-ADL                    PIC X(10).
           03  N-TEL                    PIC X(12).        
       FD  LIST-FILE.
       01  LIST-REC                     PIC X(78).    
      *
       WORKING-STORAGE                  SECTION.
       01  OOMIDASHI.                       
           03                           PIC X(08)   VALUE "14JY0000".
           03                           PIC X(01)   VALUE SPACE.
           03                           PIC X(10)   VALUE "日本　電子".
           03                           PIC X(01)   VALUE SPACE.
           03                           PIC X(32)   
           　VALUE "***仕入先マスタ更新（１対１）***".
           03                           PIC X(03)   VALUE SPACE.
           03                           PIC X(06)   VALUE "日付：".
           03  HIDUKE                   PIC 99/99/99.
       01  KOMIDASHI.
           03                           PIC X(03)   VALUE SPACE.
           03                           PIC X(04)   VALUE "番号".
           03                           PIC X(01)   VALUE SPACE.
           03                           PIC X(06)   VALUE "新名前".
           03                           PIC X(05)   VALUE SPACE.
           03                           PIC X(06)   VALUE "旧名前".
           03                           PIC X(05)   VALUE SPACE.
           03                           PIC X(06)   VALUE "新住所".
           03                           PIC X(05)   VALUE SPACE.
           03                           PIC X(06)   VALUE "旧住所".
           03                           PIC X(05)   VALUE SPACE.
           03                           PIC X(05)   VALUE "新TEL".
           03                           PIC X(08)   VALUE SPACE.
           03                           PIC X(05)   VALUE "旧TEL". 
       01  KOUGYOU                      PIC X(78)   VALUE SPACE.
       01  MEISAI.
           03  MEI-SCODE                PIC X(01).
           03                           PIC X(01)   VALUE SPACE.
           03  MEI-KCODE                PIC X(01).
           03                           PIC X(01)   VALUE SPACE.
           03  MEI-CODE                 PIC X(03).
           03                           PIC X(01)   VALUE SPACE.
           03  MEI-DATE.          
               05  MEI-NNAME            PIC X(10).
               05  S1                   PIC X(01)   VALUE SPACE.
               05  MEI-KNAME            PIC X(10).
               05  S2                   PIC X(01)   VALUE SPACE.
               05  MEI-NADL             PIC X(10).
               05  S3                   PIC X(01)   VALUE SPACE.
               05  MEI-KADL             PIC X(10).
               05  S4                   PIC X(01)   VALUE SPACE.
               05  MEI-NTEL             PIC X(12).
               05  S5                   PIC X(01)   VALUE SPACE.
               05  MEI-KTEL             PIC X(12).
      * 
       PROCEDURE                DIVISION.
       INIT-RTN.
           OPEN  INPUT  MAS-FILE  TRAN-FILE
           OPEN  OUTPUT  N-MAS-FILE  LIST-FILE.

       MAIN-RTN.
           PERFORM  MIDASHI-RTN
           PERFORM  MAS-READ
           PERFORM  TRAN-READ
           PERFORM UNTIL M-CODE = HIGH-VALUE  AND T-CODE = HIGH-VALUE
             EVALUATE TRUE
               WHEN  M-CODE = T-CODE
                 PERFORM UPDATA-RTN
               WHEN  M-CODE < T-CODE
                 PERFORM COPY-RTN
               WHEN  M-CODE > T-CODE
                 PERFORM ADD-RTN
             END-EVALUATE
           END-PERFORM.

       END-RTN.
           DISPLAY "プログラム正常終了"
           CLOSE  MAS-FILE  TRAN-FILE  N-MAS-FILE  LIST-FILE
           STOP RUN.
      *
       MIDASHI-RTN.
           ACCEPT HIDUKE FROM DATE
           WRITE LIST-REC FROM OOMIDASHI AFTER 1
           WRITE LIST-REC FROM KOMIDASHI AFTER 2
           WRITE LIST-REC FROM KOUGYOU AFTER 1.

      *
       UPDATA-RTN.
           MOVE  SPACE  TO  MEI-DATE
           MOVE  S-CODE TO  MEI-SCODE
           MOVE  K-CODE TO  MEI-KCODE
           MOVE  T-CODE TO  MEI-CODE
           MOVE  MAS-REC TO N-MAS-REC
           EVALUATE TRUE
             WHEN  S-CODE = "U"
               EVALUATE TRUE
                 WHEN K-CODE = 1
                   MOVE T-DATE TO N-NAME
                   WRITE N-MAS-REC AFTER 1
                   MOVE T-DATE TO MEI-NNAME
                   MOVE M-NAME TO MEI-KNAME
                   WRITE LIST-REC FROM MEISAI AFTER 1
                   PERFORM TRAN-READ
                   PERFORM MAS-READ
                 WHEN K-CODE = 2
                   MOVE T-DATE TO N-ADL
                   WRITE N-MAS-REC AFTER 1
                   MOVE T-DATE TO MEI-NADL
                   MOVE M-ADL TO MEI-KADL
                   WRITE LIST-REC FROM MEISAI AFTER 1
                   PERFORM TRAN-READ
                   PERFORM MAS-READ
                 WHEN K-CODE = 3
                   MOVE T-DATE TO N-TEL
                   WRITE N-MAS-REC AFTER 1
                   MOVE T-DATE TO MEI-NTEL
                   MOVE M-TEL TO MEI-KTEL
                   WRITE LIST-REC FROM MEISAI AFTER 1
                   PERFORM TRAN-READ
                   PERFORM MAS-READ
                 WHEN OTHER
                   MOVE "更新コ－ドエラ－" TO MEI-DATE
                   WRITE LIST-REC FROM MEISAI AFTER 1
                   PERFORM TRAN-READ
               END-EVALUATE
             WHEN S-CODE = "D"
               MOVE M-NAME TO MEI-KNAME
               MOVE M-ADL TO MEI-KADL
               MOVE M-TEL TO MEI-KTEL
               WRITE LIST-REC FROM MEISAI AFTER 1
               PERFORM TRAN-READ
               PERFORM MAS-READ
             WHEN OTHER
               MOVE "処理コ－ドエラ－１" TO MEI-DATE
               WRITE LIST-REC FROM MEISAI AFTER 1
               PERFORM TRAN-READ
           END-EVALUATE.

       COPY-RTN.
           MOVE  MAS-REC TO N-MAS-REC
           WRITE N-MAS-REC AFTER 1
           PERFORM MAS-READ.

       ADD-RTN.
           MOVE  SPACE  TO  MEI-DATE
           MOVE  S-CODE TO  MEI-SCODE
           MOVE  K-CODE TO  MEI-KCODE
           MOVE  T-CODE TO  MEI-CODE
           IF S-CODE = "A"
             THEN
               MOVE K-DATE TO N-MAS-REC
               WRITE N-MAS-REC AFTER 1
               MOVE T-NAME TO MEI-NNAME
               MOVE T-ADL TO MEI-NADL
               MOVE T-TEL TO MEI-NTEL
               WRITE LIST-REC FROM MEISAI AFTER 1
               PERFORM TRAN-READ
             ELSE
               MOVE "処理コ－ドエラ－２" TO MEI-DATE
               WRITE LIST-REC FROM MEISAI AFTER 1
               PERFORM TRAN-READ
           END-IF.

       MAS-READ.
           READ  MAS-FILE
             AT END
               MOVE HIGH-VALUE TO M-CODE
           END-READ.

       TRAN-READ.
           READ  TRAN-FILE
             AT END
               MOVE HIGH-VALUE TO T-CODE
           END-READ.


