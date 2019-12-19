      *
      *14JY0123 ›Á’õ•¶
      *
       IDENTIFICATION            DIVISION.
       PROGRAM-ID.               P2. 
       ENVIRONMENT               DIVISION. 
       INPUT-OUTPUT              SECTION.
       FILE-CONTROL.
           SELECT  IN-FILE     ASSIGN  "Žd“ü.txt"
                                    ORGANIZATION LINE SEQUENTIAL.
           SELECT  PRINT-FILE  ASSIGN  "P2.DOC"
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
      *
       01  MEISAI.
           05  M-NO              PIC X(05).
           05                    PIC X(01)  VALUE SPACE.
           05  M-NAME            PIC X(10).
           05                    PIC X(01)  VALUE SPACE.
           05  M-S-DATE          PIC 99/99/99.
           05                    PIC X(01)  VALUE SPACE.
           05  M-S-NO            PIC X(03).
           05                    PIC X(01)  VALUE SPACE.
           05  M-S-NAME          PIC X(10).
           05                    PIC X(01)  VALUE SPACE.
           05  M-SURYO           PIC Z,ZZ9.
           05                    PIC X(01)  VALUE SPACE.
           05  M-TANKA           PIC Z,ZZ9.
           05                    PIC X(01)  VALUE SPACE.
           05  M-KINGAKU         PIC Z,ZZZ,ZZ9. 
           
       PROCEDURE DIVISION.
            OPEN    INPUT  IN-FILE OUTPUT PRINT-FILE
            READ    IN-FILE
                AT END MOVE "E" TO END-FLG
            END-READ
            PERFORM UNTIL END-FLG = "E" 
                MOVE I-NO TO M-NO
                MOVE I-NAME TO M-NAME 
                MOVE I-S-DATE TO M-S-DATE
                MOVE I-S-NO TO M-S-NO
                MOVE I-S-NAME TO M-S-NAME
                MOVE I-SURYO TO M-SURYO
                MOVE I-TANKA TO M-TANKA
                COMPUTE M-KINGAKU = I-TANKA * I-SURYO
                WRITE P-REC FROM MEISAI AFTER 1
                READ    IN-FILE
                    AT END MOVE "E" TO END-FLG
                END-READ
            END-PERFORM
            CLOSE IN-FILE PRINT-FILE    
            DISPLAY "END"
            STOP RUN.

       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       

        
      
