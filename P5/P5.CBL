      * 
      * 14JY0123 崔　禎　文
      *
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      P5.

       
       ENVIRONMENT      DIVISION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
            SELECT  IN-FILE     ASSIGN  "仕入.TXT"
                                    ORGANIZATION LINE SEQUENTIAL.
            SELECT  SORT-FILE   ASSIGN  "SORT-TEMP".   
            SELECT  PRINT-FILE  ASSIGN  "仕入整列.TXT"
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
       01    P-REC     PIC X(43).
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
      *
       WORKING-STORAGE  SECTION. 
       01 END-FLG       PIC X(1)    VALUE SPACE.
      *    
       PROCEDURE        DIVISION.    
       MOOO.
          SORT  SORT-FILE
            
            ON  ASCENDING KEY S-NUM 
                DESCENDING KEY S-HOME
                ASCENDING KEY S-DATE
               
            USING IN-FILE
            
            GIVING PRINT-FILE      
      *
            DISPLAY "終わりました。"

            STOP RUN.
     
