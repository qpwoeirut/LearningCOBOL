      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXTERNAL-CALL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 WS-INPUT PIC X(1000).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Enter string to send to echo util:"
           ACCEPT WS-INPUT.

           CALL "ECHO-UTIL" USING WS-INPUT.
           STOP RUN.
       END PROGRAM EXTERNAL-CALL.
