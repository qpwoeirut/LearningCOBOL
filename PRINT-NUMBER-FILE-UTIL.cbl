      ******************************************************************
      * Author: Stanley Zhong
      * Date: 5/11/2020
      * Purpose: Utility to print a file of numbers
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRINT-NUMBER-FILE-UTIL.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT FS-NUMBERS-FILE ASSIGN TO WS-FILE-NAME.
       DATA DIVISION.
       FILE SECTION.
           FD FS-NUMBERS-FILE.
               01 FS-NUMBER PIC 9(10).
       WORKING-STORAGE SECTION.
           01 WS-FILE-NAME PIC X(100).
           01 WS-NUMBER PIC 9(10).
           01 WS-EOF PIC A(3) VALUE "NO".
       LINKAGE SECTION.
           01 LS-FILE-NAME PIC X(100).
       PROCEDURE DIVISION USING LS-FILE-NAME.
           MOVE LS-FILE-NAME TO WS-FILE-NAME.
           OPEN INPUT FS-NUMBERS-FILE
               PERFORM UNTIL WS-EOF = "YES"
                   READ FS-NUMBERS-FILE INTO WS-NUMBER
                       AT END MOVE "YES" TO WS-EOF
                       NOT AT END DISPLAY WS-NUMBER
                   END-READ
               END-PERFORM
           CLOSE FS-NUMBERS-FILE

           STOP RUN.
       END PROGRAM PRINT-NUMBER-FILE-UTIL.
