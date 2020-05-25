      ******************************************************************
      * Author: Stanley Zhong
      * Date: 5/21/2020, Updated 5/25/2020
      * Purpose: Convert year, month, day to number of days since
      *          12/31/1600
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATE-TO-DAYS.

       DATA DIVISION.
       LINKAGE SECTION.
           01 LS-YEAR PIC 9(4).
           01 LS-MONTH PIC 9(2).
           01 LS-DAY PIC 9(2).
           01 LS-OUTPUT PIC 9(9).
       PROCEDURE DIVISION USING LS-YEAR LS-MONTH LS-DAY LS-OUTPUT.
       MAIN-PROCEDURE.
           MOVE 0 TO LS-OUTPUT
           ADD LS-YEAR TO LS-OUTPUT
           MULTIPLY 100 BY LS-OUTPUT
           ADD LS-MONTH TO LS-OUTPUT
           MULTIPLY 100 BY LS-OUTPUT
           ADD LS-DAY TO LS-OUTPUT

           MOVE FUNCTION INTEGER-OF-DATE(LS-OUTPUT) TO LS-OUTPUT

           GOBACK.
       END PROGRAM DATE-TO-DAYS.
