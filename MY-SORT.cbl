      ******************************************************************
      * Author: Stanley Zhong
      * Date: 5/11/2020
      * Purpose: Sort a file of numbers and write it to another file
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MY-SORT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT FS-INPUT-FILE ASSIGN TO WS-INPUT-FILE-NAME.
               SELECT FS-OUTPUT-FILE ASSIGN TO WS-OUTPUT-FILE-NAME.
               SELECT FS-WORK-FILE ASSIGN TO "tmp_work.txt".
       DATA DIVISION.
       FILE SECTION.
           FD FS-INPUT-FILE.
               01 FS-INPUT-NUMBER PIC 9(10).
           FD FS-OUTPUT-FILE.
               01 FS-OUTPUT-NUMBER PIC 9(10).

           SD FS-WORK-FILE.
               01 FS-WORK-NUMBER PIC 9(10).
       WORKING-STORAGE SECTION.
           01 WS-INPUT-FILE-NAME PIC X(100).
           01 WS-OUTPUT-FILE-NAME PIC X(100).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Input the path of the file to sort:"
           ACCEPT WS-INPUT-FILE-NAME

           DISPLAY "Input the path of the file for the sorted numbers (c
      -     "an be the same):"
           ACCEPT WS-OUTPUT-FILE-NAME

           SORT FS-WORK-FILE ON ASCENDING KEY FS-INPUT-NUMBER
           USING FS-INPUT-FILE GIVING FS-OUTPUT-FILE

           CALL "PRINT-NUMBER-FILE-UTIL" USING WS-OUTPUT-FILE-NAME

           STOP RUN.
       END PROGRAM MY-SORT.
