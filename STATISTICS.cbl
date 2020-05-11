      ******************************************************************
      * Author: Stanley Zhong
      * Date: 5/7/2020, Updated 5/11/2020
      * Purpose: Read non-negative numbers from file and print sum, min,
      *          max, mean, range, and total count of numbers OR
      *          write numbers to file
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STATISTICS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT FS-NUMBERS-FILE ASSIGN TO WS-FILE-PATH
                   ORGANIZATION IS SEQUENTIAL
                   ACCESS IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
           FD FS-NUMBERS-FILE.
           01 FS-NUMBERS-DATA.
               05 FS-NUMBER PIC 9(10).
       WORKING-STORAGE SECTION.
           01 WS-FILE-NAME PIC X(100).
           01 WS-FILE-PATH PIC X(100).

           01 WS-NUMBER PIC 9(10).

           01 WS-DATA.
               05 WS-TOTAL-COUNT PIC 9(20) VALUE 0.
               05 WS-SUM PIC 9(20) VALUE 0.
               05 WS-MIN PIC 9(20) VALUE 99999999999999999999.
               05 WS-MAX PIC 9(20) VALUE 0.
               05 WS-MEAN PIC 9(20).
               05 WS-RANGE PIC 9(20).

           01 WS-COMMAND PIC A(1).
           01 WS-INPUT PIC X(10).

           01 WS-DISPLAY PIC Z(19)9.

           01 WS-EOF PIC X(3) VALUE "NO".
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Enter the relative path to the file of numbers"
           ACCEPT WS-FILE-PATH

           DISPLAY "Enter 'w' to write or 's' to get statistics"
           ACCEPT WS-COMMAND

           IF WS-COMMAND = "w"
               DISPLAY "Enter numbers (up to 1e10, exclusive) or type 'q
      -         "uit' to quit"
               OPEN OUTPUT FS-NUMBERS-FILE
               ACCEPT WS-INPUT
               PERFORM WRITE-NUMBER-PARA UNTIL WS-INPUT = "quit"
               CLOSE FS-NUMBERS-FILE
           ELSE IF WS-COMMAND = "s"
               DISPLAY "Reading file..."
               OPEN INPUT FS-NUMBERS-FILE
               PERFORM PROCESS-NUMBER-PARA UNTIL WS-EOF="YES"
               CLOSE FS-NUMBERS-FILE

               SUBTRACT WS-MIN FROM WS-MAX GIVING WS-RANGE
               DIVIDE WS-SUM BY WS-TOTAL-COUNT GIVING WS-MEAN

               PERFORM DISPLAY-STATS-PARA
           ELSE
               DISPLAY "Invalid option"
           END-IF

           STOP RUN.

       WRITE-NUMBER-PARA.
           MOVE WS-INPUT TO FS-NUMBER
           WRITE FS-NUMBERS-DATA
           ACCEPT WS-INPUT.


       PROCESS-NUMBER-PARA.
           READ FS-NUMBERS-FILE INTO WS-NUMBER
               AT END
                   MOVE "YES" TO WS-EOF
               NOT AT END
                   ADD 1 TO WS-TOTAL-COUNT
                   ADD WS-NUMBER TO WS-SUM
                   IF WS-NUMBER < WS-MIN
                       MOVE WS-NUMBER TO WS-MIN
                   END-IF

                   IF WS-NUMBER > WS-MAX
                       MOVE WS-NUMBER TO WS-MAX
                   END-IF
           END-READ.


       DISPLAY-STATS-PARA.
           MOVE WS-TOTAL-COUNT TO WS-DISPLAY
           DISPLAY "# of numbers:"WS-DISPLAY

           MOVE WS-SUM TO WS-DISPLAY
           DISPLAY "sum:         "WS-DISPLAY

           MOVE WS-MIN TO WS-DISPLAY
           DISPLAY "min:         "WS-DISPLAY

           MOVE WS-MAX TO WS-DISPLAY
           DISPLAY "max:         "WS-DISPLAY

           MOVE WS-MEAN TO WS-DISPLAY
           DISPLAY "mean:        "WS-DISPLAY

           MOVE WS-RANGE TO WS-DISPLAY
           DISPLAY "range:       "WS-DISPLAY.


       END PROGRAM STATISTICS.
