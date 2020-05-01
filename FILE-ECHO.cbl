      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-ECHO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT STUFF ASSIGN TO WS-FILE-NAME.

       DATA DIVISION.
       FILE SECTION.
           FD STUFF.
           01 STUFF-FILE.
               05 STUFF-STUFF PIC X(100000).
       WORKING-STORAGE SECTION.
           01 WS-FILE-NAME PIC X(100).
           01 WS-STUFF.
               05 WS-STUFF-STUFF PIC X(100000).
           01 WS-EOF PIC A(3) VALUE "NO".
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Enter file path for the file you want to read:".
           ACCEPT WS-FILE-NAME.
           OPEN INPUT STUFF.
           PERFORM UNTIL WS-EOF="YES"
               READ STUFF INTO WS-STUFF
                   AT END MOVE "YES" TO WS-EOF
                   NOT AT END DISPLAY WS-STUFF
               END-READ
           END-PERFORM.
           CLOSE STUFF.
           STOP RUN.

           PRINT-FILE-PARA.
       END PROGRAM FILE-ECHO.
