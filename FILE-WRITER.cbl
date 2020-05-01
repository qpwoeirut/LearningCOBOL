      ******************************************************************
      * Author: Stanley Zhong
      * Date: 4/30/2020
      * Purpose: Write numbers to a file
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILE-WRITER.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT ARRAY ASSIGN TO WS-FILE-NAME
                   ORGANIZATION IS SEQUENTIAL
                   ACCESS IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           FD ARRAY.
           01 ARRAY-FILE.
               05 ARRAY-NUMBER PIC 9(10).

       WORKING-STORAGE SECTION.
           01 WS-FILE-NAME PIC X(100) VALUE "../array.txt".

           01 WS-SIZE PIC 9(5).
           01 WS-NUMBERS.
               05 WS-NUM PIC 9(10) OCCURS 100 TIMES INDEXED BY I.

           01 WS-DISPLAY PIC Z(9)9.
           01 WS-SIZE-DISPLAY PIC Z(4)9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Enter N, the size of the array you want to store:"
           ACCEPT WS-SIZE.

           MOVE WS-SIZE TO WS-SIZE-DISPLAY.
           DISPLAY "Enter "WS-SIZE-DISPLAY" numbers, one per line:"

           PERFORM GET-INPUT-PARA
           VARYING I FROM 1 BY 1
             UNTIL I > WS-SIZE.

           OPEN OUTPUT ARRAY.
               PERFORM WRITE-ENTRY-PARA
               VARYING I FROM 1 BY 1
                 UNTIL I > WS-SIZE.
           CLOSE ARRAY.

           STOP RUN.

           GET-INPUT-PARA.
               ACCEPT WS-NUM(I).

           WRITE-ENTRY-PARA.
               MOVE WS-NUM(I) TO ARRAY-NUMBER.
               WRITE ARRAY-FILE.


       END PROGRAM FILE-WRITER.
