      ******************************************************************
      * Author: Stanley Zhong
      * Date: 4/30/2020
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MY-SORT.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-SIZE PIC 9(5).
           01 WS-NUMBERS.
               05 WS-NUM PIC 9(10) OCCURS 100 TIMES INDEXED BY I.

           01 WS-DISPLAY PIC Z(9)9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Enter N, the size of the array you want to sort:"
           ACCEPT WS-SIZE.

           MOVE WS-SIZE TO WS-DISPLAY.
           DISPLAY "Enter "WS-DISPLAY" numbers, one per line:"

           PERFORM GET-INPUT-PARA
           VARYING I FROM 1 BY 1
             UNTIL I > WS-SIZE.

           PERFORM PRINT-PARA
           VARYING I FROM 1 BY 1
             UNTIL I > WS-SIZE.

           STOP RUN.

           GET-INPUT-PARA.
               ACCEPT WS-NUM(I).

           PRINT-PARA.
               MOVE WS-NUM(I) TO WS-DISPLAY.
               DISPLAY WS-DISPLAY.

       END PROGRAM MY-SORT.
