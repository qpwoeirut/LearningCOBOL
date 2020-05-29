      ******************************************************************
      * Author: Stanley Zhong
      * Date: 5/25/2020, Updated 5/28/2020
      * Purpose: Store employee data and generate payroll and W2 forms
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
               SELECT FS-EMPLOYEE-FILE ASSIGN TO "employees.txt"
                                       ORGANIZATION IS INDEXED
                                       ACCESS       IS DYNAMIC
                                       RECORD KEY   IS FS-EMPLOYEE-SSN
                                       FILE STATUS  IS
                                       WS-EMPLOYEE-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
           FD FS-EMPLOYEE-FILE.
           01 FS-EMPLOYEE.
               05 FS-EMPLOYEE-NAME PIC X(50).
               05 FS-EMPLOYEE-SSN  PIC 9(9).
               05 FS-EMPLOYEE-BIRTH-DATE.
                   10 FS-EMPLOYEE-BIRTH-YEAR  PIC 9(4).
                   10 FS-EMPLOYEE-BIRTH-MONTH PIC 9(2).
                   10 FS-EMPLOYEE-BIRTH-DAY   PIC 9(2).
               05 FS-EMPLOYEE-ADDRESS.
                   10 FS-EMPLOYEE-ADDRESS-STREET PIC X(100).
                   10 FS-EMPLOYEE-ADDRESS-APT    PIC X(20).
                   10 FS-EMPLOYEE-ADDRESS-CITY   PIC X(100).
                   10 FS-EMPLOYEE-ADDRESS-REGION PIC X(2).
                   10 FS-EMPLOYEE-ADDRESS-ZIP    PIC 9(5).
               05 FS-EMPLOYEE-GENDER         PIC X(1).
               05 FS-EMPLOYEE-MARITAL-STATUS PIC X(9).
               05 FS-EMPLOYEE-HOURLY-WAGE        PIC 9(18)V99.
               05 FS-EMPLOYEE-GROSS-PAY-THIS-YEAR     PIC 9(18)V99.
               05 FS-EMPLOYEE-WITHHELD-THIS-YEAR PIC 9(18)V99.
               05 FS-EMPLOYEE-LAST-PAID-DATE.
                   10 FS-EMPLOYEE-LAST-PAID-YEAR  PIC 9(4).
                   10 FS-EMPLOYEE-LAST-PAID-MONTH PIC 9(2).
       WORKING-STORAGE SECTION.
           01 WS-EMPLOYEE-FILE-STATUS PIC XX.

      *>      01 WS-CURRENT-DATE.
      *>          05 WS-CURRENT-YEAR PIC 9(4).
      *>          05 WS-CURRENT-MONTH PIC 9(2).
      *>          05 WS-CURRENT-DAY PIC 9(2).

           01 WS-COMMAND PIC 99 VALUE 1.
           01 WS-YES-NO PIC X.

           01 WS-MONTH PIC 9(2).
           01 WS-YEAR PIC 9(4).

           01 WS-WORKING-HOURS PIC 9(5).
           01 WS-GROSS-PAY     PIC 9(18)V99.
           01 WS-TAX-RATE      PIC 9(18)V9(5).
           01 WS-TAX-WITHHELD  PIC 9(18)V99.
           01 WS-NET-PAY       PIC S9(18)V99.

           01 WS-DISPLAY              PIC X(30).
           01 WS-DISPLAY-INDEX        PIC 9(2).
           01 WS-DISPLAY-MONEY-TMP    PIC S9(18)V99.
           01 WS-DISPLAY-MONEY-FORMAT PIC $$$,$$$,$$$,$$$,$$$,$$9.99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "Welcome to the employee salary management system!"

           PERFORM UNTIL WS-COMMAND = 0
               IF WS-COMMAND = 1
                   PERFORM 100-DISPLAY-HELP-MESSAGE-PARA
               ELSE
                   IF WS-COMMAND = 2
                       PERFORM 200-ADD-EMPLOYEE-RECORD-PARA
                   ELSE
                       IF WS-COMMAND = 3
                           PERFORM 300-EDIT-EMPLOYEE-RECORD-PARA
                       ELSE
                           IF WS-COMMAND = 4
                               PERFORM 400-REMOVE-EMPLOYEE-RECORD-PARA
                           ELSE
                           IF WS-COMMAND = 5
                               PERFORM 500-DISPLAY-EMPLOYEE-RECORD-PARA
                           ELSE
                           IF WS-COMMAND = 6
                               PERFORM
                                   600-CALCULATE-PAYCHECK-FOR-MONTH-PARA
                           ELSE
                               DISPLAY "Invalid command!"
                               PERFORM 100-DISPLAY-HELP-MESSAGE-PARA
                           END-IF
                           END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF

               DISPLAY "Enter command"
               ACCEPT WS-COMMAND
           END-PERFORM
           STOP RUN.


       100-DISPLAY-HELP-MESSAGE-PARA.
           DISPLAY "Options:"
           DISPLAY "0) Quit"
           DISPLAY "1) Display this help message"
           DISPLAY "2) Add employee data"
           DISPLAY "3) Edit employee data"
           DISPLAY "4) Remove employee data"
           DISPLAY "5) View employee data"
           DISPLAY "6) Generate paycheck for employee"
      *>      DISPLAY "7) Generate W2 form for employee"
           .


       200-ADD-EMPLOYEE-RECORD-PARA.
           PERFORM 210-ACCEPT-EMPLOYEE-DATA-PARA

           OPEN I-O FS-EMPLOYEE-FILE
               IF WS-EMPLOYEE-FILE-STATUS = '35'
                   PERFORM CREATE-EMPLOYEE-FILE-PARA
                   OPEN I-O FS-EMPLOYEE-FILE
               END-IF

               WRITE FS-EMPLOYEE
                   INVALID KEY
                       DISPLAY "This employee already exists in the data
      -                        "base - try editing their data instead"
                   NOT INVALID KEY
                       DISPLAY "Employee added"
               END-WRITE
           CLOSE FS-EMPLOYEE-FILE
           .


       210-ACCEPT-EMPLOYEE-DATA-PARA.
           PERFORM ACCEPT-EMPLOYEE-NAME-PARA
           PERFORM ACCEPT-EMPLOYEE-SSN-PARA
           PERFORM ACCEPT-EMPLOYEE-BIRTH-DATE-PARA
           PERFORM ACCEPT-EMPLOYEE-ADDRESS-DATA-PARA
           PERFORM ACCEPT-EMPLOYEE-GENDER-PARA
           PERFORM ACCEPT-EMPLOYEE-MARITAL-STATUS-PARA
           PERFORM ACCEPT-EMPLOYEE-SALARY-PARA
           MOVE 0 TO FS-EMPLOYEE-GROSS-PAY-THIS-YEAR
           MOVE 0 TO FS-EMPLOYEE-WITHHELD-THIS-YEAR
           MOVE FUNCTION CURRENT-DATE TO FS-EMPLOYEE-LAST-PAID-DATE
           .


       300-EDIT-EMPLOYEE-RECORD-PARA.
           PERFORM ACCEPT-EMPLOYEE-SSN-PARA
           OPEN I-O FS-EMPLOYEE-FILE
               IF WS-EMPLOYEE-FILE-STATUS = '35'
                   PERFORM CREATE-EMPLOYEE-FILE-PARA
                   OPEN I-O FS-EMPLOYEE-FILE
               END-IF
               READ FS-EMPLOYEE-FILE
                   INVALID KEY
                       DISPLAY "Unable to find employee in database"
                   NOT INVALID KEY
                       PERFORM DISPLAY-EMPLOYEE-DATA-PARA
                       PERFORM EDIT-SPECIFIC-DATA-PARA
                       REWRITE FS-EMPLOYEE
                           INVALID KEY
                               DISPLAY "Unable to edit employee data"
                           NOT INVALID KEY
                               DISPLAY "Updated employee data"
                       END-REWRITE
               END-READ
           CLOSE FS-EMPLOYEE-FILE
           .


       400-REMOVE-EMPLOYEE-RECORD-PARA.
           PERFORM ACCEPT-EMPLOYEE-SSN-PARA
           OPEN I-O FS-EMPLOYEE-FILE
               IF WS-EMPLOYEE-FILE-STATUS = '35'
                   PERFORM CREATE-EMPLOYEE-FILE-PARA
                   OPEN I-O FS-EMPLOYEE-FILE
               END-IF
               DELETE FS-EMPLOYEE-FILE RECORD
                   INVALID KEY
                       DISPLAY "Employee not found in the database"
                   NOT INVALID KEY
                       DISPLAY "Employee deleted"
               END-DELETE
           CLOSE FS-EMPLOYEE-FILE
           .


       500-DISPLAY-EMPLOYEE-RECORD-PARA.
           PERFORM ACCEPT-EMPLOYEE-SSN-PARA
           OPEN INPUT FS-EMPLOYEE-FILE
               IF WS-EMPLOYEE-FILE-STATUS = '35'
                   PERFORM CREATE-EMPLOYEE-FILE-PARA
                   OPEN INPUT FS-EMPLOYEE-FILE
               END-IF
               READ FS-EMPLOYEE-FILE
                   INVALID KEY
                       DISPLAY "Employee not found in the database"
                   NOT INVALID KEY
                       PERFORM DISPLAY-EMPLOYEE-DATA-PARA
               END-READ
           CLOSE FS-EMPLOYEE-FILE
           .


       600-CALCULATE-PAYCHECK-FOR-MONTH-PARA.
           PERFORM ACCEPT-EMPLOYEE-SSN-PARA
           OPEN INPUT FS-EMPLOYEE-FILE
               IF WS-EMPLOYEE-FILE-STATUS = '35'
                   PERFORM CREATE-EMPLOYEE-FILE-PARA
                   OPEN INPUT FS-EMPLOYEE-FILE
               END-IF
               READ FS-EMPLOYEE-FILE
                   INVALID KEY
                       DISPLAY "Employee not found in the database"
               END-READ
           CLOSE FS-EMPLOYEE-FILE

           PERFORM 610-ACCEPT-PAYCHECK-TIME-PARA

           IF (WS-YEAR < FS-EMPLOYEE-LAST-PAID-YEAR) OR
              (WS-YEAR <= FS-EMPLOYEE-LAST-PAID-YEAR AND
               WS-MONTH <= FS-EMPLOYEE-LAST-PAID-MONTH)
               DISPLAY "Employee has already had paycheck calculated for
      -                " this month and/or a month afterwards. Do you wa
      -                "nt to continue? (y/n)"
               ACCEPT WS-YES-NO

               MOVE FUNCTION LOWER-CASE(WS-YES-NO) TO WS-YES-NO

               IF WS-YES-NO <> 'y'
                   EXIT PARAGRAPH
               END-IF
           END-IF

           PERFORM 620-ACCEPT-PAYCHECK-DATA-PARA

           MULTIPLY WS-WORKING-HOURS BY FS-EMPLOYEE-HOURLY-WAGE
           GIVING WS-GROSS-PAY

           MULTIPLY WS-GROSS-PAY BY WS-TAX-RATE GIVING WS-TAX-WITHHELD

           ADD WS-GROSS-PAY TO FS-EMPLOYEE-GROSS-PAY-THIS-YEAR
           ADD WS-TAX-WITHHELD TO FS-EMPLOYEE-WITHHELD-THIS-YEAR


           DISPLAY "Month of    : "WS-YEAR"/"WS-MONTH

           MOVE WS-GROSS-PAY TO WS-DISPLAY-MONEY-TMP
           PERFORM CALCULATE-MONEY-DISPLAY-PARA
           DISPLAY "Gross Pay   : "WS-DISPLAY

           MOVE WS-TAX-WITHHELD TO WS-DISPLAY-MONEY-TMP
           PERFORM CALCULATE-MONEY-DISPLAY-PARA
           DISPLAY "Withheld Tax: "WS-DISPLAY

           SUBTRACT WS-GROSS-PAY FROM WS-TAX-WITHHELD GIVING WS-NET-PAY
           MOVE WS-NET-PAY TO WS-DISPLAY-MONEY-TMP
           PERFORM CALCULATE-MONEY-DISPLAY-PARA
           DISPLAY "Net Pay     : "WS-DISPLAY


           MOVE WS-MONTH TO FS-EMPLOYEE-LAST-PAID-MONTH
           MOVE WS-YEAR  TO FS-EMPLOYEE-LAST-PAID-YEAR

           PERFORM 630-REWRITE-EMPLOYEE-RECORD-PARA
           .


       610-ACCEPT-PAYCHECK-TIME-PARA.
           DISPLAY "What month do you want to calculate a paycheck for (
      -            "in numbers, i.e January is 1, February is 2, etc?"
           ACCEPT WS-MONTH
           DISPLAY "What year was this month in (in yyyy format)?"
           ACCEPT WS-YEAR
           .


       620-ACCEPT-PAYCHECK-DATA-PARA.
           DISPLAY "How many hours did the employee work in this month?"
           ACCEPT WS-WORKING-HOURS

           DISPLAY "What total tax rate applies to this employee (as a d
      -            "ecimal, ie 0.05)?"
           ACCEPT WS-TAX-RATE
           .


       630-REWRITE-EMPLOYEE-RECORD-PARA.
           OPEN I-O FS-EMPLOYEE-FILE
               REWRITE FS-EMPLOYEE
                   INVALID KEY
                       DISPLAY "Unable to update pay and tax totals for
      -                        "employee"
                   NOT INVALID KEY
                       DISPLAY "Updated pay and tax totals for employee"
               END-REWRITE
           CLOSE FS-EMPLOYEE-FILE
           .


       ACCEPT-EMPLOYEE-NAME-PARA.
           DISPLAY "Enter name of employee:"
           ACCEPT FS-EMPLOYEE-NAME
           .


       ACCEPT-EMPLOYEE-SSN-PARA.
           DISPLAY "Enter Social Security Number of employee (digits onl
      -            "y):"
           ACCEPT FS-EMPLOYEE-SSN
           .


       ACCEPT-EMPLOYEE-BIRTH-DATE-PARA.
           DISPLAY "Enter birth date of employee in yyyymmdd format:"
           ACCEPT FS-EMPLOYEE-BIRTH-DATE
           .


       ACCEPT-EMPLOYEE-ADDRESS-DATA-PARA.
           DISPLAY "Enter street name of employee address:"
           ACCEPT FS-EMPLOYEE-ADDRESS-STREET

           DISPLAY "Enter apartment/suite of employee (leave blank for n
      -            "one)"
           ACCEPT FS-EMPLOYEE-ADDRESS-APT

           DISPLAY "Enter city of employee address:"
           ACCEPT FS-EMPLOYEE-ADDRESS-CITY

           DISPLAY "Enter two-letter state/region of employee address:"
           ACCEPT FS-EMPLOYEE-ADDRESS-REGION

           DISPLAY "Enter zip code of employee address:"
           ACCEPT FS-EMPLOYEE-ADDRESS-ZIP
           .


       ACCEPT-EMPLOYEE-GENDER-PARA.
           DISPLAY "Enter gender of employee (F for female, M for male,
      -            "or O for other):"
           ACCEPT FS-EMPLOYEE-GENDER
           .


       ACCEPT-EMPLOYEE-MARITAL-STATUS-PARA.
           DISPLAY "Enter marital status of employee (Single, Married, D
      -            "ivorced, Separated, or Other):"
           ACCEPT FS-EMPLOYEE-MARITAL-STATUS
           .


       ACCEPT-EMPLOYEE-SALARY-PARA.
           DISPLAY "Enter hourly wage of employee in decimal format (no
      -            "currency symbol, ie 123.45)"
           ACCEPT FS-EMPLOYEE-HOURLY-WAGE
           .


       EDIT-SPECIFIC-DATA-PARA.
           PERFORM UNTIL WS-COMMAND = 0
               PERFORM PRINT-EDIT-DATA-HELP-PARA
               ACCEPT WS-COMMAND
               IF WS-COMMAND = 1
                   PERFORM ACCEPT-EMPLOYEE-NAME-PARA
               ELSE
                   IF WS-COMMAND = 2
                       PERFORM ACCEPT-EMPLOYEE-BIRTH-DATE-PARA
                   ELSE
                       IF WS-COMMAND = 3
                           PERFORM ACCEPT-EMPLOYEE-ADDRESS-DATA-PARA
                       ELSE
                       IF WS-COMMAND = 4
                           PERFORM ACCEPT-EMPLOYEE-GENDER-PARA
                       ELSE
                       IF WS-COMMAND = 5
                           PERFORM ACCEPT-EMPLOYEE-MARITAL-STATUS-PARA
                       ELSE
                           IF WS-COMMAND = 6
                               PERFORM ACCEPT-EMPLOYEE-SALARY-PARA
                           ELSE
                               IF WS-COMMAND = 7
                                   DISPLAY "Enter total gross pay of emp
      -                                    "loyee, year-to-date"
                                  ACCEPT FS-EMPLOYEE-GROSS-PAY-THIS-YEAR
                               ELSE IF WS-COMMAND = 8
                                   DISPLAY "Enter total withheld tax mon
      -                                   "ey of employee, year-to-date"
                                   ACCEPT FS-EMPLOYEE-WITHHELD-THIS-YEAR
                               ELSE
                                   PERFORM PRINT-EDIT-DATA-HELP-PARA
                               END-IF
                               END-IF
                           END-IF
                       END-IF
                       END-IF
                       END-IF
                   END-IF
               END-IF

           END-PERFORM
           .


       PRINT-EDIT-DATA-HELP-PARA.
           DISPLAY "Enter corresponding number for data entry:"
           DISPLAY "1) Name"
           DISPLAY "2) Date of Birth"
           DISPLAY "3) Address"
           DISPLAY "4) Gender"
           DISPLAY "5) Marital Status"
           DISPLAY "6) Hourly Wage"
           DISPLAY "7) Gross pay, year-to-date"
           DISPLAY "8) Withheld tax money, year-to-date"
           DISPLAY "SSN can't be edited. To change the SSN, delete this
      -            "entry and create a new one"
           DISPLAY "Enter 0 to stop and save"
           .


       DISPLAY-EMPLOYEE-DATA-PARA.
           DISPLAY "Name: "FS-EMPLOYEE-NAME
           DISPLAY "SSN : "FS-EMPLOYEE-SSN
           DISPLAY "DOB (yyyymmdd): "FS-EMPLOYEE-BIRTH-YEAR"/"
                                     FS-EMPLOYEE-BIRTH-MONTH"/"
                                     FS-EMPLOYEE-BIRTH-DAY

           DISPLAY "Street      : "FS-EMPLOYEE-ADDRESS-STREET
           DISPLAY "Apt         : "FS-EMPLOYEE-ADDRESS-APT
           DISPLAY "City        : "FS-EMPLOYEE-ADDRESS-CITY
           DISPLAY "State/Region: "FS-EMPLOYEE-ADDRESS-REGION
           DISPLAY "Zip code    : "FS-EMPLOYEE-ADDRESS-ZIP

           DISPLAY "Last paid month   : "FS-EMPLOYEE-LAST-PAID-YEAR"/"
                                         FS-EMPLOYEE-LAST-PAID-MONTH

           MOVE FS-EMPLOYEE-HOURLY-WAGE TO WS-DISPLAY-MONEY-TMP
           PERFORM CALCULATE-MONEY-DISPLAY-PARA
           DISPLAY "Wages per hour    : "WS-DISPLAY

           MOVE FS-EMPLOYEE-GROSS-PAY-THIS-YEAR TO WS-DISPLAY-MONEY-TMP
           PERFORM CALCULATE-MONEY-DISPLAY-PARA
           DISPLAY "Gross pay YTD     : "WS-DISPLAY

           MOVE FS-EMPLOYEE-WITHHELD-THIS-YEAR TO WS-DISPLAY-MONEY-TMP
           PERFORM CALCULATE-MONEY-DISPLAY-PARA
           DISPLAY "Withheld taxes YTD: "WS-DISPLAY
           .


       CREATE-EMPLOYEE-FILE-PARA.
           CLOSE FS-EMPLOYEE-FILE
           OPEN OUTPUT FS-EMPLOYEE-FILE
           CLOSE FS-EMPLOYEE-FILE
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


       END PROGRAM PAYROLL.
