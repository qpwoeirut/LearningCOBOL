      ******************************************************************
      * Author: Stanley Zhong
      * Date: 4/22/2020, Updated 5/11/2020
      * Purpose: Implementation of FizzBuzz
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIZZ-BUZZ.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-NUM PIC 9(3) VALUE 1.
           01 WS-THREE PIC X(4) VALUE "FIZZ".
           01 WS-FIVE PIC X(4) VALUE "BUZZ".
           01 WS-SEVEN PIC X(5) VALUE "SEVEN".
           01 WS-LEN PIC 9(3) VALUE 1.
           01 WS-CONCAT PIC X(13).
       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM PRINT-PARA UNTIL WS-NUM > 500

           STOP RUN.

       PRINT-PARA.
           MOVE 1 TO WS-LEN.
           IF FUNCTION MOD(WS-NUM, 3) = 0
               STRING WS-THREE DELIMITED BY SIZE
                 INTO WS-CONCAT
                 WITH POINTER WS-LEN
               END-STRING
           END-IF.
           IF FUNCTION MOD(WS-NUM, 5) = 0
               STRING WS-FIVE DELIMITED BY SIZE
                 INTO WS-CONCAT
                 WITH POINTER WS-LEN
               END-STRING
           END-IF.
           IF FUNCTION MOD(WS-NUM, 7) = 0
               STRING WS-SEVEN DELIMITED BY SIZE
                 INTO WS-CONCAT
                 WITH POINTER WS-LEN
               END-STRING
           END-IF.


           if WS-LEN = 1
               STRING WS-NUM DELIMITED BY SIZE
                 INTO WS-CONCAT
                 WITH POINTER WS-LEN
               END-STRING
           END-IF.

           SUBTRACT 1 FROM WS-LEN.
           DISPLAY WS-CONCAT(1 : WS-LEN)

           ADD 1 TO WS-NUM.

       END PROGRAM FIZZ-BUZZ.
