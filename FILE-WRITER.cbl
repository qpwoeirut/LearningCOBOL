      ******************************************************************
      * Author: Stanley Zhong
      * Date: 4/30/2020, Updated 5/11/2020
      * Purpose: Write numbers to a file
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-WRITER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT ARRAY-FILE ASSIGN TO WS-FILE-PATH
                   ORGANIZATION IS SEQUENTIAL
                   ACCESS IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           FD ARRAY-FILE.
           01 ARRAY-NUMBER PIC 9(10).

       WORKING-STORAGE SECTION.
           01 WS-INPUT PIC X(10).
           01 WS-FILE-PATH PIC X(100).

           01 WS-SIZE PIC 9(6).
           01 WS-NUMBER PIC 9(10).

           01 WS-DISPLAY PIC Z(9)9.
           01 WS-SIZE-DISPLAY PIC Z(5)9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Enter the path of the file for these numbers:"
           ACCEPT WS-FILE-PATH

           MOVE WS-SIZE TO WS-SIZE-DISPLAY
           DISPLAY "Enter numbers, one per line (type 'q' to quit):"

           OPEN OUTPUT ARRAY-FILE
           ACCEPT WS-INPUT
           PERFORM UNTIL WS-INPUT = "q"
               MOVE WS-INPUT TO ARRAY-NUMBER
               WRITE ARRAY-NUMBER
               ACCEPT WS-INPUT
           END-PERFORM

           CLOSE ARRAY-FILE

           STOP RUN.


       END PROGRAM FILE-WRITER.
