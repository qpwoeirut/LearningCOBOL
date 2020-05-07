      ******************************************************************
      * Author: Stanley Zhong
      * Date: 5/7/2020
      * Purpose: Print the start and end time of the program
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TIME-IT.
       DATA DIVISION.
           FILE SECTION.
           WORKING-STORAGE SECTION.
               01 WS-START-DATE-INTEGER   PIC 9(9).
               01 WS-START-DATE-DATA.
                   05 WS-START-DATE.
                       10 WS-START-YEAR         PIC 9(4).
                       10 WS-START-MONTH        PIC 9(2).
                       10 WS-START-DAY          PIC 9(2).
                   05 WS-START-TIME.
                       10 WS-START-HOURS        PIC 9(2).
                       10 WS-START-MINUTE       PIC 9(2).
                       10 WS-START-SECOND       PIC 9(2).
                       10 WS-START-MILLISECONDS PIC 9(2).

               01 WS-CURRENT-DATE-INTEGER PIC 9(9).
               01 WS-CURRENT-DATE-DATA.
                   05 WS-CURRENT-DATE.
                       10 WS-CURRENT-YEAR         PIC 9(4).
                       10 WS-CURRENT-MONTH        PIC 9(2).
                       10 WS-CURRENT-DAY          PIC 9(2).
                   05 WS-CURRENT-TIME.
                       10 WS-CURRENT-HOURS        PIC 9(2).
                       10 WS-CURRENT-MINUTE       PIC 9(2).
                       10 WS-CURRENT-SECOND       PIC 9(2).
                       10 WS-CURRENT-MILLISECONDS PIC 9(2).

               01 WS-DATE-DIFFERENCE.
                   10 WS-DAY-DIFFERENCE         PIC 9(6).
                   10 WS-HOUR-DIFFERENCE        PIC 9(2).
                   10 WS-MINUTE-DIFFERENCE      PIC 9(2).
                   10 WS-SECOND-DIFFERENCE      PIC 9(2).
                   10 WS-MILLISECOND-DIFFERENCE PIC 9(2).
               01 WS-MESSAGE PIC X(1000) VALUE "Calling ECHO-UTIL".
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE FUNCTION CURRENT-DATE TO WS-START-DATE-DATA.
           CALL "DATE-TO-INTEGER"
           USING WS-START-YEAR, WS-START-MONTH, WS-START-DAY,
           WS-START-DATE-INTEGER.

           DISPLAY WS-START-DATE-INTEGER.

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA.
           CALL "DATE-TO-INTEGER"
           USING WS-CURRENT-YEAR, WS-CURRENT-MONTH, WS-CURRENT-DAY,
           WS-CURRENT-DATE-INTEGER.

           DISPLAY WS-CURRENT-DATE-INTEGER.

           SUBTRACT WS-CURRENT-DATE-INTEGER
           FROM WS-START-DATE-INTEGER
           GIVING WS-DAY-DIFFERENCE.

           DISPLAY WS-DAY-DIFFERENCE.

           PERFORM GET-TIME-DIFFERENCE-PARA.


           STOP RUN.

       GET-TIME-DIFFERENCE-PARA.
           SUBTRACT WS-CURRENT-HOURS
           FROM WS-START-HOURS
           GIVING WS-HOUR-DIFFERENCE.

           SUBTRACT WS-CURRENT-MINUTE
           FROM WS-START-MINUTE
           GIVING WS-MINUTE-DIFFERENCE.

           SUBTRACT WS-CURRENT-SECOND
           FROM WS-START-SECOND
           GIVING WS-SECOND-DIFFERENCE.

           IF WS-SECOND-DIFFERENCE < 0
               ADD 60 TO WS-SECOND-DIFFERENCE
               SUBTRACT 1 FROM WS-MINUTE-DIFFERENCE
           END-IF.


           IF WS-MINUTE-DIFFERENCE < 0
               ADD 60 TO WS-MINUTE-DIFFERENCE
               SUBTRACT 1 FROM WS-HOUR-DIFFERENCE
           END-IF.

           DISPLAY WS-MINUTE-DIFFERENCE.
           DISPLAY WS-SECOND-DIFFERENCE.

       END PROGRAM TIME-IT.


       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATE-TO-INTEGER.

       DATA DIVISION.
       LINKAGE SECTION.
           01 LS-YEAR PIC 9(4).
           01 LS-MONTH PIC 9(2).
           01 LS-DAY PIC 9(2).
           01 LS-OUTPUT PIC 9(9).
       PROCEDURE DIVISION USING LS-YEAR LS-MONTH LS-DAY LS-OUTPUT.
       MAIN-PROCEDURE.
           MOVE 0 TO LS-OUTPUT.
           ADD LS-YEAR TO LS-OUTPUT.
           MULTIPLY 100 BY LS-OUTPUT.
           ADD LS-MONTH TO LS-OUTPUT.
           MULTIPLY 100 BY LS-OUTPUT.
           ADD LS-DAY TO LS-OUTPUT.

           MOVE FUNCTION INTEGER-OF-DATE(LS-OUTPUT) TO LS-OUTPUT.

           GOBACK.
       END PROGRAM DATE-TO-INTEGER.
