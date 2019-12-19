      * 
      * 14JY0123 崔　禎　文
      * P5Step1
      *
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      P5Step1.

       
       ENVIRONMENT      DIVISION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
            SELECT  IN-FILE     ASSIGN  "仕入.TXT"
                                    ORGANIZATION LINE SEQUENTIAL.
            SELECT  SORT-FILE   ASSIGN  "SORT-TEMP".   
            SELECT  PRINT-FILE  ASSIGN  "P5Step1.TXT"
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
       01    P-REC     PIC X(52).
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
            03   S-AMOUNT    PIC 9(9).  
      *
       WORKING-STORAGE  SECTION.   
       01 END-FLG       PIC X(1)    VALUE SPACE. 
       01 M-REC.
            03   M-NUM       PIC X(5).
            03   M-NAME      PIC X(10).
            03   M-DATE      PIC 9(6).
            03   M-HOME      PIC X(3).
            03   M-HNAME     PIC X(10).
            03   M-KAZU      PIC 9(4).
            03   M-PRICE     PIC 9(5).
            03   M-AMOUNT    PIC 9(9).
      *    
       PROCEDURE        DIVISION.    
       MOOO.
          SORT  SORT-FILE
            
            ON  DESCENDING KEY S-AMOUNT  
                ASCENDING KEY S-PRICE  
                DESCENDING KEY S-KAZU
            
            INPUT PROCEDURE COMPUTE-PROC
            OUTPUT PROCEDURE RANK-PROC
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
                MOVE I-NUM TO S-NUM 
                MOVE I-NAME TO S-NAME
                MOVE I-DATE TO S-DATE
                MOVE I-HOME  TO S-HOME 
                MOVE I-HNAME TO S-HNAME
                MOVE I-KAZU TO  S-KAZU 
                MOVE I-PRICE TO  S-PRICE
                COMPUTE S-AMOUNT = I-PRICE * I-KAZU
                RELEASE S-REC
      *
                READ IN-FILE
                    AT END MOVE "E" TO END-FLG
                END-READ
            END-PERFORM
      *
            CLOSE IN-FILE.
      * 
       RANK-PROC.
            OPEN OUTPUT PRINT-FILE
            MOVE  SPACE TO END-FLG 
            RETURN SORT-FILE 
                AT END MOVE "E" TO END-FLG
            END-RETURN
      *
            PERFORM UNTIL END-FLG = "E" 
                MOVE S-NUM TO M-NUM 
                MOVE S-NAME  TO M-NAME 
                MOVE S-DATE  TO M-DATE 
                MOVE S-HOME  TO M-HOME 
                MOVE S-HNAME TO M-HNAME 
                MOVE S-KAZU  TO M-KAZU
                MOVE S-PRICE  TO M-PRICE
                MOVE S-AMOUNT TO M-AMOUNT
                WRITE P-REC FROM M-REC
            
                RETURN SORT-FILE 
                    AT END MOVE "E" TO END-FLG 
                END-RETURN
           END-PERFORM
       
            CLOSE PRINT-FILE.
