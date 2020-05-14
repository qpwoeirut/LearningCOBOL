      ******************************************************************
      * Author: Stanley Zhong
      * Date: 5/11/2020
      * Purpose: Create a basic ledger system that keeps track of money
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 WS-INPUT PIC 9(2) VALUE 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM DISPLAY-HELP-PARA

           PERFORM UNTIL WS-INPUT = 0
               IF WS-INPUT = 1
                   PERFORM DISPLAY-HELP-PARA
               ELSE IF WS-INPUT = 2

               ELSE IF WS-INPUT = 3

               ELSE IF WS-INPUT = 4

               ELSE
                   DISPLAY "Invalid option!"
                   PERFORM DISPLAY-HELP-PARA
               END-IF
           END-PERFORM

           STOP RUN.

       DISPLAY-HELP-PARA.
           DISPLAY "Bank interface:"
           DISPLAY "0) Quit"
           DISPLAY "1) Display this help message"
           DISPLAY "2) Get account information and history"
           DISPLAY "3) Update account balance"
           DISPLAY "4) Add interest"
           .



       END PROGRAM BANK.
