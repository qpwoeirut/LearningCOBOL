      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ECHO-UTIL.
       DATA DIVISION.
       LINKAGE SECTION.
           01 LS-INPUT PIC X(1000).
       PROCEDURE DIVISION USING LS-INPUT.
           DISPLAY "Running ECHO-UTIL".
           DISPLAY LS-INPUT.
           STOP RUN.
       END PROGRAM ECHO-UTIL.
