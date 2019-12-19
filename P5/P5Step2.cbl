      * 
      * 14JY0123 崔　禎　文
      * P5Step2
      *
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      P5.

       
       ENVIRONMENT      DIVISION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
            SELECT  IN-FILE     ASSIGN  "仕入.TXT"
                                    ORGANIZATION LINE SEQUENTIAL.
            SELECT  SORT-FILE   ASSIGN  "SORT-TEMP".   
            SELECT  PRINT-FILE  ASSIGN  "P5Step2.DOC"       
                                    ORGANIZATION LINE SEQUENTIAL. 
            
      *
       DATA             DIVISION.
       FILE             SECTION.
       FD   IN-FILE.
       01    I-REC.
            03   I-NUM       PIC X(5).
            03   I-NAME      PIC X(10).
            03   I-DATE      PIC 9(6).
            03   I-HOME      PIC X(3).
            03   I-HNAME     PIC X(10).
            03   I-KAZU      PIC 9(4).
            03   I-PRICE     PIC 9(5).
      * 
       FD   PRINT-FILE.   
       01    P-REC     PIC X(78).
      *
       SD  SORT-FILE.
       01 S-REC.
            03   S-NUM       PIC X(5).
            03   S-NAME      PIC X(10).
            03   S-DATE      PIC 9(6).
            03   S-HOME      PIC X(3).
            03   S-HNAME     PIC X(10).
            03   S-KAZU      PIC 9(4).
            03   S-PRICE     PIC 9(5).
            03   S-AMOUNT    PIC 9(6).  
      *
       WORKING-STORAGE  SECTION.   
       01  END-FLG       PIC X(1)    VALUE SPACE.
       01  RANK          PIC 9(2) VALUE 0 .
       01  L-CNT                 PIC 9(03).
       01　N-PAGEKAZU            PIC 9(03)  VALUE 0.
       01  N-NUM                 PIC 9(03)  VALUE 0.   
       01  KINGAKU               PIC 9(18) VALUE 0. 
       01  G-KINGAKU             PIC 9(18) VALUE 0.
       01  A-SPACE               PIC X(30) VALUE SPACE.  
       01  MEISAI.
           05  M-RANK            PIC Z9.
           05                    PIC X(01)  VALUE SPACE.
           05  M-NUM             PIC X(05).
           05                    PIC X(04)  VALUE SPACE.
           05  M-NAME            PIC X(10).
           05                    PIC X(01)  VALUE SPACE.
           05  M-DATE            PIC 99/99/99.
           05                    PIC X(01)  VALUE SPACE.
           05  M-HOME            PIC X(03).
           05                    PIC X(01)  VALUE SPACE.
           05  M-HNAME           PIC X(10).
           05                    PIC X(01)  VALUE SPACE.
           05  M-KAZU            PIC Z,ZZ9.
           05                    PIC X(02)  VALUE SPACE.
           05  M-PRICE           PIC Z,ZZ9.
           05                    PIC X(04)  VALUE SPACE.
           05  M-AMOUNT          PIC ZZZ,ZZ9. 
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
           05                    PIC X(23)  VALUE  "RANK 商品".
           05                    PIC X(25)  VALUE  "仕入".
           05                    PIC X(23)  VALUE  
                                 "数量   単価       金額". 
       01  HEAD-3.
           05                    PIC X(33)  VALUE
                                "      NO    名前       日付".
           05                    PIC X(27)  VALUE
                                "NO  名前".


      *    
       PROCEDURE        DIVISION.    
       MOOO.
          SORT  SORT-FILE
            
            ON  ASCENDING KEY S-NUM   
                DESCENDING KEY S-HOME  
                ASCENDING KEY S-DATE
            INPUT PROCEDURE COMPUTE-PROC            
            OUTPUT PROCEDURE PANK-PROC      
      *
            DISPLAY "終わりました。"

            STOP RUN.   
      * 
       COMPUTE-PROC.
            OPEN INPUT IN-FILE
           
            READ IN-FILE 
                AT END MOVE "E" TO END-FLG
            END-READ
      * 
            PERFORM UNTIL END-FLG = "E" 
                COMPUTE RANK = RANK + 1 
                MOVE I-NUM   TO  S-NUM 
                MOVE I-NAME  TO  S-NAME  
                MOVE I-DATE  TO  S-DATE
                MOVE I-HOME  TO  S-HOME
                MOVE I-HNAME TO  S-HNAME
                MOVE I-KAZU  TO  S-KAZU
                MOVE I-PRICE TO  S-PRICE
                COMPUTE S-AMOUNT = I-KAZU * I-PRICE
                
                RELEASE S-REC             
      
                READ IN-FILE
                    AT END MOVE "E" TO END-FLG
                END-READ
            END-PERFORM
      *
            CLOSE IN-FILE.
      *
       PANK-PROC.
            OPEN OUTPUT PRINT-FILE 
            MOVE SPACE TO END-FLG 
            COMPUTE N-PAGEKAZU = N-PAGEKAZU + 1
            ACCEPT H1-DATE FROM DATE 
            WRITE P-REC FROM HEAD-1 AFTER PAGE
            WRITE P-REC FROM HEAD-2 AFTER 2
            WRITE P-REC FROM HEAD-3 AFTER 1
       
            RETURN SORT-FILE 
                AT END MOVE "E" TO END-FLG 
            END-RETURN
      * 
            PERFORM UNTIL END-FLG = "E"
                IF N-NUM < 25
                    THEN 
                        ADD 1 TO RANK
                        ADD 1 TO N-NUM
                        MOVE S-NAME  TO  M-NAME  
                        MOVE S-DATE  TO  M-DATE
                        MOVE S-HOME  TO  M-HOME
                        MOVE S-HNAME TO  M-HNAME
                        MOVE S-KAZU  TO  M-KAZU
                        MOVE S-PRICE TO  M-PRICE 
                        MOVE S-AMOUNT TO  M-AMOUNT
                        WRITE P-REC FROM MEISAI AFTER 1
                    ELSE
                        COMPUTE N-PAGEKAZU = N-PAGEKAZU + 1
                        ACCEPT H1-DATE FROM DATE 
                        WRITE P-REC FROM HEAD-1 AFTER PAGE
                        WRITE P-REC FROM HEAD-2 AFTER 2
                        WRITE P-REC FROM HEAD-3 AFTER 1
                        MOVE 0 TO M-RANK 
                        MOVE 0 TO N-NUM
                END-IF
                    RETURN SORT-FILE 
                        AT END MOVE "E" TO END-FLG
                    END-RETURN
            END-PERFORM
            
            CLOSE PRINT-FILE.    
                        
            
