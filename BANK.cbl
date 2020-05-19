      ******************************************************************
      * Author: Stanley Zhong
      * Date: 5/11/2020, Updated 5/18/2020
      * Purpose: Create a basic ledger system that keeps track of money
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT FS-USERS-FILE ASSIGN TO "bank_users.txt"
               ORGANIZATION IS INDEXED
               ACCESS       IS DYNAMIC
               RECORD KEY   IS FS-USER-ID
               FILE STATUS  IS WS-USERS-FILE-STATUS.
           SELECT FS-TRANSACTION-FILE ASSIGN TO WS-TRANSACTION-FILENAME
               ORGANIZATION IS RELATIVE
               ACCESS       IS DYNAMIC
               RELATIVE KEY IS WS-TRANSACTION-NUMBER
               FILE STATUS  IS WS-TRANSACTION-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
           FD FS-USERS-FILE.
               01 FS-USER.
                   05 FS-USER-ID           PIC 9(16).
                   05 FS-NAME              PIC X(100).
                   05 FS-BALANCE           PIC S9(18)V99.
                   05 FS-TRANSACTION-COUNT PIC 9(18).
           FD FS-TRANSACTION-FILE.
               01 FS-TRANSACTION.
                   05 FS-TRANSACTION-NUMBER PIC 9(18).
                   05 FS-TRANSACTION-TIMESTAMP.
                       10 FS-START-DATE.
                           15 FS-START-YEAR          PIC 9(4).
                           15 FS-START-MONTH         PIC 9(2).
                           15 FS-START-DAY           PIC 9(2).
                       10 FS-START-TIME.
                           15 FS-START-HOURS         PIC 9(2).
                           15 FS-START-MINUTE        PIC 9(2).
                           15 FS-START-SECOND        PIC 9(2).
                           15 FS-START-MILLISECONDS  PIC 9(2).
                   05 FS-TRANSACTION-AMOUNT      PIC S9(18)V99.
                   05 FS-TRANSACTION-END-BALANCE PIC S9(18)V99.
       WORKING-STORAGE SECTION.
           01 WS-USERS-FILE-STATUS       PIC X(2).
           01 WS-TRANSACTION-FILENAME    PIC X(50).
           01 WS-TRANSACTION-FILE-STATUS PIC X(2).
           01 WS-TRANSACTION-NUMBER      PIC 9(18).

           01 WS-EOF PIC A(3).
           01 WS-ABORT PIC A(3) VALUE "NO".

           01 WS-COMMAND PIC 9(2) VALUE 1.

           01 WS-USER.
               05 WS-USER-ID PIC 9(16).
               05 WS-NAME    PIC X(100).
               05 WS-BALANCE PIC S9(18)V99.

           01 WS-DISPLAY              PIC X(30).
           01 WS-DISPLAY-INDEX        PIC 9(2).
           01 WS-DISPLAY-MONEY-TMP    PIC S9(18)V99.
           01 WS-DISPLAY-MONEY-FORMAT PIC $$$,$$$,$$$,$$$,$$$,$$9.99.
           01 WS-DISPLAY-NUMBER-TMP   PIC Z(17)9.

           01 WS-MIN-ID       PIC 9(16) VALUE 1000000000000000.
           01 WS-RANDOM       PIC V9(16).
           01 WS-ID-COLLISION PIC X(3).

           01 WS-TRANSACTION-AMOUNT PIC S9(18)V99.

           01 WS-START-DATE-DATA.
               05 WS-START-DATE.
                   10 WS-START-YEAR          PIC 9(4).
                   10 WS-START-MONTH         PIC 9(2).
                   10 WS-START-DAY           PIC 9(2).
               05 WS-START-TIME.
                   10 WS-START-HOURS         PIC 9(2).
                   10 WS-START-MINUTE        PIC 9(2).
                   10 WS-START-SECOND        PIC 9(2).
                   10 WS-START-MILLISECONDS  PIC 9(2).
           01 WS-ECHO PIC S9(18)V99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE "bank_transactions/folder_exists_test"
           TO WS-TRANSACTION-FILENAME
           OPEN OUTPUT FS-TRANSACTION-FILE
               IF WS-TRANSACTION-FILE-STATUS = '30'
                   DISPLAY "I/O error during initialization"
                   DISPLAY "Check to make sure the bank_transactions fol
      -                    "der exists"
                   MOVE "YES" TO WS-ABORT
               END-IF
           CLOSE FS-TRANSACTION-FILE

           IF WS-ABORT = "NO"
               MOVE FUNCTION CURRENT-DATE TO WS-START-DATE-DATA
               MOVE FUNCTION RANDOM(WS-START-MILLISECONDS) TO WS-RANDOM

               MOVE 1 TO WS-COMMAND
               PERFORM UNTIL WS-COMMAND = 0
                   PERFORM 000-PERFORM-COMMAND-PARA
                   DISPLAY ' '
                   DISPLAY "Enter a command:"
                   ACCEPT WS-COMMAND
               END-PERFORM
           END-IF

           STOP RUN.


       000-PERFORM-COMMAND-PARA.
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
                            PERFORM
                               400-LIST-ACCOUNT-TRANSACTION-HISTORY-PARA
                       ELSE
                           IF WS-COMMAND = 5
                               PERFORM 500-UPDATE-ACCOUNT-PARA
                           ELSE
                               IF WS-COMMAND = 6
                                   PERFORM 600-UPDATE-ALL-INTEREST-PARA
                               ELSE
                                   DISPLAY "Invalid option!"
                                   PERFORM 100-DISPLAY-HELP-PARA
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
           .


       100-DISPLAY-HELP-PARA.
           DISPLAY "Bank interface:"
           DISPLAY "0) Quit"
           DISPLAY "1) Display this help message"
           DISPLAY "2) Register account"
           DISPLAY "3) Get account information"
           DISPLAY "4) List account transaction history"
           DISPLAY "5) Update account balance"
           DISPLAY "6) Add interest"
           .


       200-REGISTER-ACCOUNT-PARA.
           OPEN I-O FS-USERS-FILE
               IF WS-USERS-FILE-STATUS = '35'
                   PERFORM 210-CREATE-USERS-FILE
               END-IF

               DISPLAY "Enter name:"
               ACCEPT FS-NAME

               MOVE 0 TO FS-BALANCE
               MOVE 1 TO FS-TRANSACTION-COUNT

               MOVE "YES" TO WS-ID-COLLISION
               PERFORM UNTIL WS-ID-COLLISION = "NO"
                   MOVE FUNCTION RANDOM TO WS-RANDOM
                   MULTIPLY WS-MIN-ID BY WS-RANDOM GIVING FS-USER-ID
                   ADD WS-MIN-ID TO FS-USER-ID
                   WRITE FS-USER
                       NOT INVALID KEY
                           DISPLAY "Registration successful!"
                           DISPLAY "Remember your id: "FS-USER-ID
                           PERFORM 220-CREATE-TRANSACTION-FILE
                           MOVE "NO" TO WS-ID-COLLISION
                   END-WRITE
               END-PERFORM
           CLOSE FS-USERS-FILE
           .


       210-CREATE-USERS-FILE.
           CLOSE FS-USERS-FILE
           OPEN OUTPUT FS-USERS-FILE
           CLOSE FS-USERS-FILE
           OPEN I-O FS-USERS-FILE
           .


       220-CREATE-TRANSACTION-FILE.
           STRING "bank_transactions/"FS-USER-ID".txt" DELIMITED BY SIZE
             INTO WS-TRANSACTION-FILENAME
           DISPLAY "Transaction file: "WS-TRANSACTION-FILENAME
           MOVE 1 TO WS-TRANSACTION-NUMBER
           MOVE WS-TRANSACTION-NUMBER TO FS-TRANSACTION-NUMBER
           MOVE FUNCTION CURRENT-DATE TO FS-TRANSACTION-TIMESTAMP
           MOVE 0 TO FS-TRANSACTION-AMOUNT
           MOVE 0 TO FS-TRANSACTION-END-BALANCE

           OPEN OUTPUT FS-TRANSACTION-FILE
               WRITE FS-TRANSACTION
                   INVALID KEY
                       DISPLAY "INVALID KEY - "WS-TRANSACTION-NUMBER
               END-WRITE
           CLOSE FS-TRANSACTION-FILE
           .


       300-GET-ACCOUNT-INFO-PARA.
           DISPLAY "Enter account ID:"
           ACCEPT FS-USER-ID

           OPEN INPUT FS-USERS-FILE
               READ FS-USERS-FILE
                   INVALID KEY DISPLAY "ID does not exist"
                   NOT INVALID KEY
                       DISPLAY "ID:                "FS-USER-ID
                       DISPLAY "Name:              "FS-NAME

                       MOVE FS-BALANCE TO WS-DISPLAY-MONEY-TMP
                       PERFORM CALCULATE-MONEY-DISPLAY-PARA
                       DISPLAY "Balance:           "WS-DISPLAY

                       MOVE FS-TRANSACTION-COUNT
                         TO WS-DISPLAY-NUMBER-TMP
                       PERFORM CALCULATE-NUMBER-DISPLAY-PARA
                       DISPLAY "# of transactions: "WS-DISPLAY
               END-READ
           CLOSE FS-USERS-FILE
           .


       400-LIST-ACCOUNT-TRANSACTION-HISTORY-PARA.
           DISPLAY "Enter account ID:"
           ACCEPT WS-USER-ID

           MOVE WS-USER-ID TO FS-USER-ID
           OPEN INPUT FS-USERS-FILE
               READ FS-USERS-FILE
                   NOT INVALID KEY
                       MOVE FS-TRANSACTION-COUNT
                         TO WS-TRANSACTION-NUMBER
           CLOSE FS-USERS-FILE

           STRING "bank_transactions/"WS-USER-ID".txt" DELIMITED BY SIZE
             INTO WS-TRANSACTION-FILENAME

           OPEN INPUT FS-TRANSACTION-FILE
               IF WS-TRANSACTION-FILE-STATUS = '35'
                   DISPLAY "Account does not exist"
               ELSE
                   PERFORM 410-PRINT-TRANSACTION-PARA
                   VARYING WS-TRANSACTION-NUMBER
                   FROM FS-TRANSACTION-COUNT
                   BY -1
                   UNTIL WS-TRANSACTION-NUMBER < 1
               END-IF
           CLOSE FS-TRANSACTION-FILE
           .


       410-PRINT-TRANSACTION-PARA.
           READ FS-TRANSACTION-FILE RECORD
               INVALID KEY DISPLAY "INVALID KEY - "WS-TRANSACTION-NUMBER
               NOT INVALID KEY
                   MOVE FS-TRANSACTION-NUMBER TO WS-DISPLAY-NUMBER-TMP
                   PERFORM CALCULATE-NUMBER-DISPLAY-PARA
                   DISPLAY "Transaction #"WS-DISPLAY

                   MOVE FS-TRANSACTION-AMOUNT TO WS-DISPLAY-MONEY-TMP
                   PERFORM CALCULATE-MONEY-DISPLAY-PARA
                   DISPLAY "Amount:     "WS-DISPLAY

                   MOVE FS-TRANSACTION-END-BALANCE
                     TO WS-DISPLAY-MONEY-TMP
                   PERFORM CALCULATE-MONEY-DISPLAY-PARA
                   DISPLAY "Balance:    "WS-DISPLAY
           END-READ
           .


       500-UPDATE-ACCOUNT-PARA.
           DISPLAY "Enter account ID:"
           ACCEPT WS-USER-ID
           DISPLAY "Enter amount of money to transact:"
           ACCEPT WS-TRANSACTION-AMOUNT

           MOVE WS-USER TO FS-USER
           OPEN I-O FS-USERS-FILE
               READ FS-USERS-FILE
                   INVALID KEY
                       DISPLAY "ID does not exist"
                   NOT INVALID KEY
                       ADD WS-TRANSACTION-AMOUNT TO FS-BALANCE
                       ADD 1 TO FS-TRANSACTION-COUNT
                       PERFORM 510-UPDATE-ACCOUNT-IN-FILE-PARA
                       MOVE FS-TRANSACTION-COUNT
                         TO WS-TRANSACTION-NUMBER
                       PERFORM 520-ADD-TRANSACTION-PARA
               END-READ
           CLOSE FS-USERS-FILE
           .


       510-UPDATE-ACCOUNT-IN-FILE-PARA.
           REWRITE FS-USER
               INVALID KEY
                   DISPLAY "INVALID ID - "FS-USER-ID
               NOT INVALID KEY
                   DISPLAY "Balance update successful"
           END-REWRITE
           .


       520-ADD-TRANSACTION-PARA.
           MOVE WS-TRANSACTION-NUMBER TO FS-TRANSACTION-NUMBER
           MOVE FUNCTION CURRENT-DATE TO FS-TRANSACTION-TIMESTAMP
           MOVE WS-TRANSACTION-AMOUNT TO FS-TRANSACTION-AMOUNT
           MOVE FS-BALANCE            TO FS-TRANSACTION-END-BALANCE

           STRING "bank_transactions/"WS-USER-ID".txt" DELIMITED BY SIZE
             INTO WS-TRANSACTION-FILENAME
           OPEN I-O FS-TRANSACTION-FILE
               WRITE FS-TRANSACTION
                   INVALID KEY
                       DISPLAY "INVALID KEY - "WS-TRANSACTION-NUMBER
               END-WRITE
           CLOSE FS-TRANSACTION-FILE
           .


       600-UPDATE-ALL-INTEREST-PARA.
           DISPLAY "You all got 0% interest! Congrats!"
           .


       CALCULATE-MONEY-DISPLAY-PARA.
           MOVE 1 TO WS-DISPLAY-INDEX
           MOVE WS-DISPLAY-MONEY-TMP TO WS-DISPLAY-MONEY-FORMAT

           PERFORM
               UNTIL WS-DISPLAY-MONEY-FORMAT(WS-DISPLAY-INDEX:1) <> ' '
               ADD 1 TO WS-DISPLAY-INDEX
           END-PERFORM

           IF WS-DISPLAY-MONEY-TMP IS NEGATIVE
               STRING "-" DELIMITED BY SIZE
                      WS-DISPLAY-MONEY-FORMAT(WS-DISPLAY-INDEX:)
                          DELIMITED BY SIZE
                 INTO WS-DISPLAY
           ELSE
               MOVE WS-DISPLAY-MONEY-FORMAT(WS-DISPLAY-INDEX:)
                 TO WS-DISPLAY
           END-IF
           .

       CALCULATE-NUMBER-DISPLAY-PARA.
           MOVE 1 TO WS-DISPLAY-INDEX
           PERFORM
             UNTIL WS-DISPLAY-NUMBER-TMP(WS-DISPLAY-INDEX:1) <> ' '
               ADD 1 TO WS-DISPLAY-INDEX
           END-PERFORM

           MOVE WS-DISPLAY-NUMBER-TMP(WS-DISPLAY-INDEX:) TO WS-DISPLAY
           .


       END PROGRAM BANK.
