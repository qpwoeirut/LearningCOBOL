      ******************************************************************
      * Author: Stanley Zhong
      * Date: 4/30/2020, Updated 5/11/2020
      * Purpose: Print the current time and date
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLOCK.
       DATA DIVISION.
           FILE SECTION.
           WORKING-STORAGE SECTION.
               01 WS-CURRENT-DATE-DATA.
                   05 WS-CURRENT-DATE.
                       10 WS-CURRENT-YEAR         PIC 9(04).
                       10 WS-CURRENT-MONTH        PIC 9(02).
                       10 WS-CURRENT-DAY          PIC 9(02).
                   05 WS-CURRENT-TIME.
                       10 WS-CURRENT-HOURS        PIC 9(02).
                       10 WS-CURRENT-MINUTE       PIC 9(02).
                       10 WS-CURRENT-SECOND       PIC 9(02).
                       10 WS-CURRENT-MILLISECONDS PIC 9(02).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE FUNCTION CURRENT-DATE to WS-CURRENT-DATE-DATA
           DISPLAY "MM/DD/YY: "WS-CURRENT-MONTH"/"WS-CURRENT-DAY"/"
               WS-CURRENT-YEAR
           DISPLAY "Time: "WS-CURRENT-HOURS":"WS-CURRENT-MINUTE":"
               WS-CURRENT-SECOND"."WS-CURRENT-MILLISECONDS
           STOP RUN.
       END PROGRAM CLOCK.
