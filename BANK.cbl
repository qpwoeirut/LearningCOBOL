      ******************************************************************
      * Author: Stanley Zhong
      * Date: 5/11/2020, Updated 5/14/2020
      * Purpose: Create a basic ledger system that keeps track of money
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT FS-USERS-FILE ASSIGN TO "BankUsers.txt"
               ORGANIZATION IS INDEXED
               ACCESS       IS DYNAMIC
               RECORD KEY   IS FS-USER-ID
               FILE STATUS  IS WS-USERS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
           FD FS-USERS-FILE.
               01 FS-USER.
                   05 FS-USER-ID PIC 9(18).
                   05 FS-NAME PIC X(100).
                   05 FS-BALANCE PIC $$$,$$$,$$$,$$$,$$9.99-.
       WORKING-STORAGE SECTION.
           01 WS-USERS-FILE-STATUS PIC X(2).
           01 WS-COMMAND PIC 9(2) VALUE 1.

           01 WS-USER.
               05 WS-USER-ID PIC 9(18).
               05 WS-NAME PIC X(100).
               05 WS-BALANCE PIC $$$,$$$,$$$,$$$,$$9.99-.

           01 WS-MAX-ID PIC 9(18) VALUE 100000000000000000.
           01 WS-RANDOM PIC V9(38).
           01 WS-ID-COLLISION PIC X(3).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-DISPLAY-HELP-PARA

           DISPLAY "Enter a command:"
           ACCEPT WS-COMMAND
           PERFORM UNTIL WS-COMMAND = 0
               IF WS-COMMAND = 1
                   PERFORM 100-DISPLAY-HELP-PARA
               ELSE
                   IF WS-COMMAND = 2
                       PERFORM 200-REGISTER-ACCOUNT-PARA
                   ELSE
                       IF WS-COMMAND = 3
                           PERFORM 300-GET-ACCOUNT-INFO-PARA
                       ELSE
                           IF WS-COMMAND = 4
                               PERFORM 400-UPDATE-ACCOUNT-BALANCE-PARA
                           ELSE
                               IF WS-COMMAND = 5
                                   PERFORM 500-UPDATE-ALL-INTEREST-PARA
                               ELSE
                                   DISPLAY "Invalid option!"
                                   PERFORM 100-DISPLAY-HELP-PARA
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF

               DISPLAY "Enter a command:"
               ACCEPT WS-COMMAND
           END-PERFORM

           STOP RUN.

       100-DISPLAY-HELP-PARA.
           DISPLAY "Bank interface:"
           DISPLAY "0) Quit"
           DISPLAY "1) Display this help message"
           DISPLAY "2) Register account"
           DISPLAY "3) Get account information and history"
           DISPLAY "4) Update account balance"
           DISPLAY "5) Add interest"
           .

       200-REGISTER-ACCOUNT-PARA.
           OPEN I-O FS-USERS-FILE
               IF WS-USERS-FILE-STATUS = '35'
                   CLOSE FS-USERS-FILE
                   OPEN OUTPUT FS-USERS-FILE
                   CLOSE FS-USERS-FILE
                   OPEN I-O FS-USERS-FILE
               END-IF

               DISPLAY "Enter name:"
               ACCEPT FS-NAME

               MOVE 0 TO FS-BALANCE

               MOVE "YES" TO WS-ID-COLLISION
               PERFORM UNTIL WS-ID-COLLISION = "NO"
                   MOVE FUNCTION RANDOM() TO WS-RANDOM
                   MULTIPLY WS-MAX-ID BY WS-RANDOM GIVING FS-USER-ID
                   ADD WS-MAX-ID TO FS-USER-ID
                   WRITE FS-USER
                       INVALID KEY DISPLAY "random id collision"
                       NOT INVALID KEY
                           DISPLAY "Registration successful"
                           DISPLAY "Remember your id: "FS-USER-ID
                           MOVE "NO" TO WS-ID-COLLISION
                   END-WRITE
               END-PERFORM
           CLOSE FS-USERS-FILE
           .

       300-GET-ACCOUNT-INFO-PARA.
           DISPLAY "Enter id"
           ACCEPT FS-USER-ID

           OPEN INPUT FS-USERS-FILE
               READ FS-USERS-FILE
                   INVALID KEY DISPLAY "ID does not exist"
                   NOT INVALID KEY
                       DISPLAY "ID:                          "FS-USER-ID
                       DISPLAY "Name:                        "FS-NAME
                       DISPLAY "Balance: "FS-BALANCE
               END-READ
           CLOSE FS-USERS-FILE
           .

       400-UPDATE-ACCOUNT-BALANCE-PARA.
           DISPLAY "Enter amount of money to transact"
           .

       500-UPDATE-ALL-INTEREST-PARA.
           DISPLAY "You all got 0% interest! Congrats!"
           .

       END PROGRAM BANK.
