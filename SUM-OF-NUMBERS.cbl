      ******************************************************************
      * Author: Stanley Zhong
      * Date: 4/22/2020, Updated 5/11/2020
      * Purpose: Takes two numbers and outputs the sum
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUM-OF-NUMBERS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-NUM-1 PIC S9(6).
           01 WS-NUM-2 PIC S9(6).
           01 WS-SUM PIC S9(7).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Input 2 integers with 5 digits or less:"
           ACCEPT WS-NUM-1
           ACCEPT WS-NUM-2

           ADD WS-NUM-1 WS-NUM-2 TO WS-SUM
           DISPLAY WS-SUM
           STOP RUN.
       END PROGRAM SUM-OF-NUMBERS.
