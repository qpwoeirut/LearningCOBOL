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
           01 ARRAY-DATA.
               05 ARRAY-NUMBER PIC 9(10).

       WORKING-STORAGE SECTION.
           01 WS-FILE-PATH PIC X(100).

           01 WS-SIZE PIC 9(6).
           01 WS-NUMBERS.
               05 WS-NUM PIC 9(10) OCCURS 1000000 TIMES INDEXED BY I.

           01 WS-DISPLAY PIC Z(9)9.
           01 WS-SIZE-DISPLAY PIC Z(5)9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Enter the path of the file for these numbers:"
           ACCEPT WS-FILE-PATH

           DISPLAY "Enter N (less than one million), the size of the arr
      -     "ay you want to store:"
           ACCEPT WS-SIZE

           MOVE WS-SIZE TO WS-SIZE-DISPLAY
           DISPLAY "Enter "WS-SIZE-DISPLAY" numbers, one per line:"

           PERFORM GET-INPUT-PARA
           VARYING I FROM 1 BY 1
               UNTIL I > WS-SIZE

           OPEN OUTPUT ARRAY-FILE
               PERFORM WRITE-ENTRY-PARA
               VARYING I FROM 1 BY 1
                 UNTIL I > WS-SIZE
           CLOSE ARRAY-FILE

           STOP RUN.

           GET-INPUT-PARA.
               ACCEPT WS-NUM(I).

           WRITE-ENTRY-PARA.
               MOVE WS-NUM(I) TO ARRAY-NUMBER
               WRITE ARRAY-DATA.


       END PROGRAM FILE-WRITER.
