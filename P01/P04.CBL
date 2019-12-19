       IDENTIFICATION       DIVISION.
       PROGRAM-ID           Exercise03.
       
      *ENVIRONMENT          DIVISION.
       
       DATA                 DIVISION.
       WORKING-STORAGE      SECTION.
       01   SUJI1           PIC 9(3).
       01   SUJI2           PIC 9(3).
       01   GOUKEI          PIC 9(4).
       
       PROCEDURE            DIVISION.
           DISPLAY  "‚Ð‚Æ‚Â‚ß‚Ì”Žš‚ð“ü—Í‚µ‚Ä‚­‚¾‚³‚¢"
           ACCEPT   SUJI1
           DISPLAY  "‚Ó‚½‚Â‚ß‚Ì”Žš‚ð“ü—Í‚µ‚Ä‚­‚¾‚³‚¢"
           ACCEPT   SUJI2
           COMPUTE  GOUKEI = SUJI1 + SUJI2
           DISPLAY  "”Žš‚Ì‡Œv‚Í" GOUKEI "‚Å‚·"
                                               
           STOP RUN. 
