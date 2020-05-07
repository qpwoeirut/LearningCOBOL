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
           01 LS-INPUT PIC X(1000) VALUE " ".
       PROCEDURE DIVISION USING LS-INPUT.
           DISPLAY "Running ECHO-UTIL".
           DISPLAY LS-INPUT.
           GOBACK.
       END PROGRAM ECHO-UTIL.
