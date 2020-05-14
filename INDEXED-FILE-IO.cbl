      ******************************************************************
      * Author: Stanley Zhong
      * Date: 5/11/2020
      * Purpose: Write/update a file record by index
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WRITE-FILE-BY-INDEX.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT FS-RECORD-FILE ASSIGN TO WS-FILE-NAME
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS DYNAMIC
               RECORD KEY   IS FS-RECORD-ID
               FILE STATUS  IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
           FD FS-RECORD-FILE RECORD CONTAINS 126 CHARACTERS.
               01 FS-RECORD-DATA.
                   05 FS-RECORD-ID            PIC 9(10).
                   05 FS-NAME                 PIC X(100).
      *>              05 FS-TIMESTAMP.
      *>                  10 FS-CURRENT-DATE.
      *>                      15 FS-YEAR         PIC 9(04).
      *>                      15 FS-MONTH        PIC 9(02).
      *>                      15 FS-DAY          PIC 9(02).
      *>                  10 FS-TIME.
      *>                      15 FS-HOURS        PIC 9(02).
      *>                      15 FS-MINUTE       PIC 9(02).
      *>                      15 FS-SECOND       PIC 9(02).
      *>                      15 FS-MILLISECONDS PIC 9(02).
       WORKING-STORAGE SECTION.
           01 WS-FILE-STATUS              PIC X(2).
           01 WS-FILE-NAME                PIC X(100).
           01 WS-RECORD-DATA.
               05 WS-RECORD-ID            PIC 9(10).
               05 WS-NAME                 PIC X(100).
      *>          05 WS-TIMESTAMP.
      *>              10 WS-CURRENT-DATE.
      *>                  15 WS-YEAR         PIC 9(04).
      *>                  15 WS-MONTH        PIC 9(02).
      *>                  15 WS-DAY          PIC 9(02).
      *>              10 WS-TIME.
      *>                  15 WS-HOURS        PIC 9(02).
      *>                  15 WS-MINUTE       PIC 9(02).
      *>                  15 WS-SECOND       PIC 9(02).
      *>                  15 WS-MILLISECONDS PIC 9(02).

           01 WS-COMMAND PIC 9(2) VALUE 0.
           01 WS-INPUT-ID PIC 9(10).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Enter file name"
           ACCEPT WS-FILE-NAME

           OPEN I-O FS-RECORD-FILE

           PERFORM PRINT-HELP-PARA
           DISPLAY "Enter a command:"
           ACCEPT WS-COMMAND
           PERFORM UNTIL WS-COMMAND = 0
               IF WS-COMMAND = 1
                   PERFORM READ-RECORD-PARA
               ELSE
                   IF WS-COMMAND = 2
                       PERFORM WRITE-RECORD-PARA
                   ELSE
                       DISPLAY "Invalid option"
                       PERFORM PRINT-HELP-PARA
                   END-IF
               END-IF

               DISPLAY "Enter a command:"
               ACCEPT WS-COMMAND
           END-PERFORM

           CLOSE FS-RECORD-FILE

           STOP RUN.

       PRINT-HELP-PARA.
           DISPLAY "This is the help message"
           DISPLAY "0) Quit"
           DISPLAY "1) Read record by id"
           DISPLAY "2) Write record by id"
           .

       READ-RECORD-PARA.
           IF WS-FILE-STATUS = '35'
               DISPLAY "ERROR: file does not exist - try writing first"
           ELSE
               DISPLAY "Enter ID to read"
               ACCEPT WS-INPUT-ID
               MOVE WS-INPUT-ID TO FS-RECORD-ID
               READ FS-RECORD-FILE INTO FS-RECORD-DATA
                   INVALID KEY
                       DISPLAY "Key is invalid"
                   NOT INVALID KEY
                       DISPLAY "Name: "FS-NAME
               END-READ
           END-IF
           .

       WRITE-RECORD-PARA.
           DISPLAY "Enter ID to write"
           ACCEPT WS-RECORD-ID
           DISPLAY "Enter name:"
           ACCEPT WS-NAME
      *>      MOVE FUNCTION CURRENT-DATE TO WS-TIMESTAMP

           MOVE WS-RECORD-DATA TO FS-RECORD-DATA
           WRITE FS-RECORD-DATA
               INVALID KEY
                   DISPLAY "invalid write: "FS-RECORD-ID
               NOT INVALID KEY
                   DISPLAY "write key: "FS-RECORD-ID
           END-WRITE
           .


       END PROGRAM WRITE-FILE-BY-INDEX.
