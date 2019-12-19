      * 
      * 14JY0123 崔　禎　文
      *
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      P1.

       
       ENVIRONMENT      DIVISION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
            SELECT  IN-FILE ASSIGN  "仕入.TXT"
                                    ORGANIZATION LINE SEQUENTIAL.
            SELECT  PRINT-FILE ASSIGN  "P1.DOC"
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
       WORKING-STORAGE  SECTION.   
       01 END-FLG       PIC X(1)    VALUE SPACE.
      *    
       PROCEDURE        DIVISION.    
       MOOO.
            OPEN INPUT IN-FILE  OUTPUT PRINT-FILE   
            PERFORM UNTIL END-FLG = "E"
                READ    IN-FILE
                    AT END   
                        MOVE "E" TO END-FLG 
                    NOT AT END       
                        WRITE P-REC FROM I-REC AFTER 1
                END-READ    
            END-PERFORM
            CLOSE   IN-FILE  PRINT-FILE
            DISPLAY "終わりました。"

            STOP RUN.
