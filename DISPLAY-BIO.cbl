      ******************************************************************
      * Author: Stanley Zhong
      * Date: Updated 5/11/2020
      * Purpose: Print some variables about me
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY-BIO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 WS-NAME PIC A(50) VALUE 'Stanley'.
           01 WS-AGE PIC 9(3) VALUE 14.
           01 WS-SCHOOL.
               05 WS-GRADE PIC 9(2) VALUE 9.
               05 WS-SCHOOL-NAME PIC X(30) VALUE 'Gunn High School'.
           01 WS-DISPLAY-AGE PIC ZZ9.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "My name is "WS-NAME
            MOVE WS-AGE TO WS-DISPLAY-AGE.
            DISPLAY "I am "WS-DISPLAY-AGE" years old"
            DISPLAY "I am in Grade "WS-GRADE
            DISPLAY "I attend "WS-SCHOOL-NAME

            STOP RUN.
       END PROGRAM DISPLAY-BIO.
