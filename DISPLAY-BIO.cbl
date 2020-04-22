      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-BIO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-NAME PIC A(50) VALUE 'Stanley'.
           01 WS-SCHOOL.
               05 WS-GRADE PIC 9(2) VALUE 9.
               05 WS-SCHOOL-NAME PIC X(30) VALUE 'Gunn High School'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "My name is "WS-NAME.
            DISPLAY "I am in Grade "WS-GRADE.
            DISPLAY "I attend "WS-SCHOOL-NAME.
            STOP RUN.
       END PROGRAM DISPLAY-BIO.
