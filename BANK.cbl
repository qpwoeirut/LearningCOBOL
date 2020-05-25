      ******************************************************************
      * Author: Stanley Zhong
      * Date: 5/11/2020, Updated 5/25/2020
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
                   05 FS-TRANSACTION-NUMBER      PIC 9(18).
                   05 FS-TRANSACTION-DESCRIPTION PIC X(50).
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
           01 WS-TRANSACTION-AMOUNT PIC S9(18)V99.

           01 WS-STOP-NUMBER PIC 9(18).

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
           01 WS-DISPLAY-NUMBER   PIC Z(17)9.

           01 WS-STR-LEN PIC 9(5).

           01 WS-MIN-ID       PIC 9(16) VALUE 1000000000000000.
           01 WS-RANDOM       PIC V9(16).
           01 WS-ID-COLLISION PIC X(3).

           01 WS-INTEREST-RATE PIC 99V9(5).
           01 WS-INTEREST-PCT  PIC ZZ9.9(3).

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
           01 WS-END-DATE-DATA.
               05 WS-END-DATE.
                   10 WS-END-YEAR          PIC 9(4).
                   10 WS-END-MONTH         PIC 9(2).
                   10 WS-END-DAY           PIC 9(2).
               05 WS-END-TIME PIC 9(8) VALUE 0.

           01 WS-START-AS-DAYS   PIC 9(9).
           01 WS-CURRENT-AS-DAYS PIC 9(9).
           01 WS-END-AS-DAYS     PIC 9(9).

           01 WS-TOTAL-CHANGE PIC S9(18)V9(5).

           01 WS-NUMBER-TITLE PIC X(18) VALUE "                 #".
           01 WS-DESCRIPTION-TITLE PIC X(50) VALUE "DESCRIPTION".
           01 WS-CHANGE-TITLE PIC X(30) VALUE "CHANGE".
           01 WS-BALANCE-TITLE PIC X(30) VALUE "BALANCE".

           01 WS-TRANSACTION-STRING PIC X(150).

           01 WS-DATE-INPUT PIC 9(4).
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
                                   IF WS-COMMAND = 7
                                     PERFORM 700-GENERATE-STATEMENT-PARA
                                   ELSE
                                       DISPLAY "Invalid option!"
                                       PERFORM 100-DISPLAY-HELP-PARA
                                   END-IF
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
           DISPLAY "7) Generate statement for time period"
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

           MOVE 1 TO WS-TRANSACTION-NUMBER
           MOVE WS-TRANSACTION-NUMBER TO FS-TRANSACTION-NUMBER
           MOVE "Account initialization" TO FS-TRANSACTION-DESCRIPTION
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

                       MOVE FS-TRANSACTION-COUNT TO WS-DISPLAY-NUMBER
                       PERFORM CALCULATE-NUMBER-DISPLAY-PARA
                       DISPLAY "# of transactions: "WS-DISPLAY
               END-READ
           CLOSE FS-USERS-FILE
           .


       400-LIST-ACCOUNT-TRANSACTION-HISTORY-PARA.
           DISPLAY "Enter account ID:"
           ACCEPT WS-USER-ID

           DISPLAY "Enter number of transactions to see:"
           ACCEPT WS-STOP-NUMBER

           MOVE WS-USER-ID TO FS-USER-ID
           OPEN INPUT FS-USERS-FILE
               READ FS-USERS-FILE
                   NOT INVALID KEY
                       MOVE FS-TRANSACTION-COUNT
                         TO WS-TRANSACTION-NUMBER
               END-READ
           CLOSE FS-USERS-FILE

           IF WS-STOP-NUMBER GREATER THAN WS-TRANSACTION-NUMBER
               MOVE 1 TO WS-STOP-NUMBER
           ELSE
               SUBTRACT WS-STOP-NUMBER FROM WS-TRANSACTION-NUMBER
               GIVING WS-STOP-NUMBER
               ADD 1 TO WS-STOP-NUMBER
               IF WS-STOP-NUMBER LESS THAN 1
                   MOVE 1 TO WS-STOP-NUMBER
               END-IF
           END-IF

           STRING "bank_transactions/"WS-USER-ID".txt" DELIMITED BY SIZE
             INTO WS-TRANSACTION-FILENAME

           OPEN INPUT FS-TRANSACTION-FILE
               IF WS-TRANSACTION-FILE-STATUS = '35'
                   DISPLAY "ID does not exist"
               ELSE
                   PERFORM 410-PRINT-TRANSACTION-PARA
                   VARYING WS-TRANSACTION-NUMBER
                   FROM FS-TRANSACTION-COUNT
                   BY -1
                   UNTIL WS-TRANSACTION-NUMBER < WS-STOP-NUMBER
               END-IF
           CLOSE FS-TRANSACTION-FILE
           .


       410-PRINT-TRANSACTION-PARA.
           READ FS-TRANSACTION-FILE RECORD
               INVALID KEY DISPLAY "INVALID KEY - "WS-TRANSACTION-NUMBER
               NOT INVALID KEY
                   MOVE FS-TRANSACTION-NUMBER TO WS-DISPLAY-NUMBER
                   PERFORM CALCULATE-NUMBER-DISPLAY-PARA
                   DISPLAY "Transaction #"WS-DISPLAY

                   DISPLAY "Description: "FS-TRANSACTION-DESCRIPTION

                   MOVE FS-TRANSACTION-AMOUNT TO WS-DISPLAY-MONEY-TMP
                   PERFORM CALCULATE-MONEY-DISPLAY-PARA
                   DISPLAY "Amount:      "WS-DISPLAY

                   MOVE FS-TRANSACTION-END-BALANCE
                     TO WS-DISPLAY-MONEY-TMP
                   PERFORM CALCULATE-MONEY-DISPLAY-PARA
                   DISPLAY "Balance:     "WS-DISPLAY
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
                       PERFORM 510-UPDATE-USER-ACCOUNT-PARA
               END-READ
           CLOSE FS-USERS-FILE
           .


       510-UPDATE-USER-ACCOUNT-PARA.
           ADD WS-TRANSACTION-AMOUNT  TO FS-BALANCE
           ADD 1 TO FS-TRANSACTION-COUNT
           PERFORM 520-UPDATE-ACCOUNT-IN-FILE-PARA

           MOVE FS-TRANSACTION-COUNT  TO WS-TRANSACTION-NUMBER
           MOVE WS-TRANSACTION-NUMBER TO FS-TRANSACTION-NUMBER
           MOVE FUNCTION CURRENT-DATE TO FS-TRANSACTION-TIMESTAMP
           MOVE WS-TRANSACTION-AMOUNT TO FS-TRANSACTION-AMOUNT
           MOVE FS-BALANCE            TO FS-TRANSACTION-END-BALANCE

           DISPLAY "Add a description for this transaction (up to 50 cha
      -            "racters)"
           ACCEPT FS-TRANSACTION-DESCRIPTION

           PERFORM ADD-TRANSACTION-PARA
           .


       520-UPDATE-ACCOUNT-IN-FILE-PARA.
           REWRITE FS-USER
               INVALID KEY
                   DISPLAY "INVALID ID - "FS-USER-ID
               NOT INVALID KEY
                   DISPLAY "Balance update successful"
           END-REWRITE
           .


       600-UPDATE-ALL-INTEREST-PARA.
           DISPLAY "Input interest, as a decimal (like 0.01)"
           ACCEPT WS-INTEREST-RATE
           MOVE "NO" TO WS-EOF
           OPEN I-O FS-USERS-FILE
               PERFORM UNTIL WS-EOF = "YES"
                   READ FS-USERS-FILE NEXT RECORD
                       AT END
                           MOVE "YES" TO WS-EOF
                       NOT AT END
                           PERFORM 610-ADD-USER-INTEREST-PARA
                   END-READ
               END-PERFORM
           CLOSE FS-USERS-FILE
           .


       610-ADD-USER-INTEREST-PARA.
           MOVE FS-USER-ID TO WS-USER-ID
           MULTIPLY FS-BALANCE BY WS-INTEREST-RATE
           GIVING WS-TRANSACTION-AMOUNT

           ADD WS-TRANSACTION-AMOUNT TO FS-BALANCE
           ADD 1 TO FS-TRANSACTION-COUNT
           PERFORM 620-UPDATE-ACCOUNT-IN-FILE-PARA

           MOVE FS-TRANSACTION-COUNT TO WS-TRANSACTION-NUMBER
           MOVE WS-TRANSACTION-NUMBER TO FS-TRANSACTION-NUMBER
           MULTIPLY WS-INTEREST-RATE BY 100 GIVING WS-INTEREST-PCT

           MOVE " " TO FS-TRANSACTION-DESCRIPTION
           STRING "Interest of " DELIMITED BY SIZE
                  WS-INTEREST-PCT DELIMITED BY SIZE
                  "%" DELIMITED BY SIZE
             INTO FS-TRANSACTION-DESCRIPTION
           MOVE FUNCTION CURRENT-DATE TO FS-TRANSACTION-TIMESTAMP
           MOVE WS-TRANSACTION-AMOUNT TO FS-TRANSACTION-AMOUNT
           MOVE FS-BALANCE TO FS-TRANSACTION-END-BALANCE

           PERFORM ADD-TRANSACTION-PARA
           .


       620-UPDATE-ACCOUNT-IN-FILE-PARA.
           REWRITE FS-USER
               INVALID KEY
                   DISPLAY "INVALID ID - "FS-USER-ID
           END-REWRITE
           .


       700-GENERATE-STATEMENT-PARA.
           DISPLAY "Enter account ID:"
           ACCEPT WS-USER-ID

           PERFORM 710-ACCEPT-START-AND-END-DATES-PARA

           MOVE WS-USER-ID TO FS-USER-ID
           OPEN INPUT FS-USERS-FILE
               READ FS-USERS-FILE
                   NOT INVALID KEY
                       MOVE FS-TRANSACTION-COUNT
                         TO WS-TRANSACTION-NUMBER
                       MOVE FS-USER TO WS-USER
               END-READ
           CLOSE FS-USERS-FILE

           STRING "bank_transactions/"WS-USER-ID".txt" DELIMITED BY SIZE
             INTO WS-TRANSACTION-FILENAME

           MOVE 0 TO WS-CURRENT-AS-DAYS
           MOVE 0 TO WS-TOTAL-CHANGE
           OPEN INPUT FS-TRANSACTION-FILE
               IF WS-TRANSACTION-FILE-STATUS = '35'
                   DISPLAY "ID does not exist"
               ELSE
                   DISPLAY "Name: "WS-NAME
                   DISPLAY "ID:   "WS-USER-ID
                   DISPLAY "From "WS-START-MONTH'/'WS-START-DAY'/'
                           WS-START-YEAR" to "WS-END-MONTH'/'WS-END-DAY
                           '/'WS-END-YEAR
                   DISPLAY WS-NUMBER-TITLE' 'WS-DESCRIPTION-TITLE' '
                           WS-CHANGE-TITLE' 'WS-BALANCE-TITLE
                   PERFORM 720-READ-TRANSACTION-PARA
                     UNTIL WS-CURRENT-AS-DAYS > WS-END-AS-DAYS

                   MOVE WS-TOTAL-CHANGE TO WS-DISPLAY-MONEY-TMP
                   PERFORM CALCULATE-MONEY-DISPLAY-PARA
                   DISPLAY "Total Change: "WS-DISPLAY
               END-IF
           CLOSE FS-TRANSACTION-FILE
           .


       710-ACCEPT-START-AND-END-DATES-PARA.
           MOVE FUNCTION CURRENT-DATE TO WS-START-DATE-DATA
           MOVE WS-START-DATE-DATA TO WS-END-DATE-DATA

           DISPLAY "Enter start year (or leave blank for current)"
           ACCEPT WS-DATE-INPUT
           IF WS-DATE-INPUT <> 0
               MOVE WS-DATE-INPUT TO WS-START-YEAR
               PERFORM UNTIL WS-START-YEAR > 1600
                   DISPLAY "Start year is too early! Please renter:"
                   ACCEPT WS-DATE-INPUT
                   MOVE WS-DATE-INPUT TO WS-START-YEAR
               END-PERFORM
           END-IF

           DISPLAY "Enter start month (or leave blank for current)"
           ACCEPT WS-DATE-INPUT
           IF WS-DATE-INPUT <> 0
               MOVE WS-DATE-INPUT TO WS-START-MONTH
           END-IF

           DISPLAY "Enter start day (or leave blank for current)"
           ACCEPT WS-DATE-INPUT
           IF WS-DATE-INPUT <> 0
               MOVE WS-DATE-INPUT TO WS-START-DAY
           END-IF

           DISPLAY "Enter end year (or leave blank for current)"
           ACCEPT WS-DATE-INPUT
           IF WS-DATE-INPUT <> 0
               MOVE WS-DATE-INPUT TO WS-END-YEAR
           END-IF

           DISPLAY "Enter end month (or leave blank for current)"
           ACCEPT WS-DATE-INPUT
           IF WS-DATE-INPUT <> 0
               MOVE WS-DATE-INPUT TO WS-END-MONTH
           END-IF

           DISPLAY "Enter end day (or leave blank for current)"
           ACCEPT WS-DATE-INPUT
           IF WS-DATE-INPUT <> 0
               MOVE WS-DATE-INPUT TO WS-END-DAY
           END-IF

           CALL "DATE-TO-DAYS" USING WS-START-YEAR WS-START-MONTH
                                     WS-START-DAY WS-START-AS-DAYS

           CALL "DATE-TO-DAYS" USING WS-END-YEAR WS-END-MONTH
                                     WS-END-DAY WS-END-AS-DAYS

           IF WS-START-AS-DAYS > WS-END-AS-DAYS
               DISPLAY "Start date is after end date. Please retry."
               PERFORM 710-ACCEPT-START-AND-END-DATES-PARA
           END-IF
           .


       720-READ-TRANSACTION-PARA.
           READ FS-TRANSACTION-FILE NEXT RECORD
               AT END
                   ADD 1 TO WS-END-AS-DAYS GIVING WS-CURRENT-AS-DAYS
               NOT AT END
                   CALL "DATE-TO-DAYS" USING FS-START-YEAR
                                             FS-START-MONTH
                                             FS-START-DAY
                                             WS-CURRENT-AS-DAYS

                   IF WS-START-AS-DAYS <= WS-CURRENT-AS-DAYS AND
                      WS-CURRENT-AS-DAYS <= WS-END-AS-DAYS
                       ADD FS-TRANSACTION-AMOUNT TO WS-TOTAL-CHANGE
                       MOVE FS-TRANSACTION-NUMBER
                         TO WS-DISPLAY-NUMBER

                       MOVE 1 TO WS-STR-LEN
                       STRING WS-DISPLAY-NUMBER
                              ' '
                              FS-TRANSACTION-DESCRIPTION
                              ' '
                         INTO WS-TRANSACTION-STRING
                         WITH POINTER WS-STR-LEN

                       MOVE FS-TRANSACTION-AMOUNT
                         TO WS-DISPLAY-MONEY-TMP
                       PERFORM CALCULATE-MONEY-DISPLAY-PARA

                       STRING WS-DISPLAY
                         INTO WS-TRANSACTION-STRING
                         WITH POINTER WS-STR-LEN

                       STRING ' '
                         INTO WS-TRANSACTION-STRING
                         WITH POINTER WS-STR-LEN

                       MOVE FS-TRANSACTION-END-BALANCE
                         TO WS-DISPLAY-MONEY-TMP
                       PERFORM CALCULATE-MONEY-DISPLAY-PARA
                       STRING WS-DISPLAY
                         INTO WS-TRANSACTION-STRING
                         WITH POINTER WS-STR-LEN

                       DISPLAY WS-TRANSACTION-STRING
                   END-IF
           END-READ
           .


       ADD-TRANSACTION-PARA.
           STRING "bank_transactions/"WS-USER-ID".txt" DELIMITED BY SIZE
             INTO WS-TRANSACTION-FILENAME
           OPEN I-O FS-TRANSACTION-FILE
               WRITE FS-TRANSACTION
                   INVALID KEY
                       DISPLAY "INVALID KEY - "WS-TRANSACTION-NUMBER
               END-WRITE
           CLOSE FS-TRANSACTION-FILE
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
             UNTIL WS-DISPLAY-NUMBER(WS-DISPLAY-INDEX:1) <> ' '
               ADD 1 TO WS-DISPLAY-INDEX
           END-PERFORM

           MOVE WS-DISPLAY-NUMBER(WS-DISPLAY-INDEX:) TO WS-DISPLAY
           .


       END PROGRAM BANK.
