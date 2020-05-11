      ******************************************************************
      * Author: Stanley Zhong
      * Date: Updated 5/11/2020
      * Purpose: Program that prints input passed to it
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ECHO-UTIL.
       DATA DIVISION.
       LINKAGE SECTION.
           01 LS-INPUT PIC X(1000).
       PROCEDURE DIVISION USING LS-INPUT.
           DISPLAY "Running ECHO-UTIL"
           DISPLAY LS-INPUT

           GOBACK.
       END PROGRAM ECHO-UTIL.
