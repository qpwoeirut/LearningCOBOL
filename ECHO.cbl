      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ECHO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-INPUT PIC X(100).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           ACCEPT WS-INPUT.
           DISPLAY WS-INPUT.
           STOP RUN.
       END PROGRAM ECHO.
