       IDENTIFICATION DIVISION.
       PROGRAM-ID.
           CH12EX01.
      ***********************************************************
      *    program to convert currency from foreign units to    *
      *      U.S. dollars - illustrates interactive loading of  *
      *      table and interactive dialog for searching table   *
      ***********************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  EXCHANGE-TABLE.
           05  COUNTRY OCCURS 10 TIMES INDEXED BY X1.
               10  COUNTRY-CODE            PIC X(3).
               10  EXCHANGE-RATE           PIC 9(4)V9(4).
       01  INPUT-AREA.
           05  CODE-IN                 PIC X(3).
           05  AMOUNT-IN               PIC 9(6)V9(2).
       01  WS-RESULT                   PIC 9(6)V9(2).
       01  MORE-DATA                   PIC X(3) VALUE 'YES'.
       01  COLOR-CODES.
           05  BLACK                   PIC 9(1) VALUE 0.
           05  BLUE                    PIC 9(1) VALUE 1.
           05  GREEN                   PIC 9(1) VALUE 2.
           05  CYAN                    PIC 9(1) VALUE 3.
           05  RED                     PIC 9(1) VALUE 4.
           05  MAGENTA                 PIC 9(1) VALUE 5.
           05  BROWN                   PIC 9(1) VALUE 6.
           05  WHITE                   PIC 9(1) VALUE 7.
       SCREEN SECTION.
       01  LOAD-SCREEN.
           05  FOREGROUND-COLOR WHITE
               HIGHLIGHT
               BACKGROUND-COLOR BLUE.
               10  BLANK SCREEN.
               10  LINE 1 COLUMN 1 VALUE 'LOAD EXCHANGE RATES'.
               10  LINE 5 COLUMN 1 VALUE 'COUNTRY CODE: '.
               10  PIC X(3) TO COUNTRY-CODE (X1).
               10  COLUMN 25 VALUE 'NUMBER TO A U. S. DOLLAR: '.
               10  PIC ZZZ9.9999 TO EXCHANGE-RATE (X1).
       01  INQUIRY-SCREEN.
           05  FOREGROUND-COLOR WHITE
               HIGHLIGHT
               BACKGROUND-COLOR GREEN.
               10  BLANK SCREEN.
               10  LINE 1 COLUMN 1 VALUE 'CONVERT CURRENCY'.
               10  LINE 5 COLUMN 1 VALUE 'COUNTRY CODE: '.
               10  PIC X(3) TO CODE-IN.
               10  COLUMN 25 VALUE 'AMOUNT: '.
               10  PIC ZZZ,ZZ9.99 TO AMOUNT-IN.
       01  RESULT-SCREEN.
           05  FOREGROUND-COLOR WHITE
               HIGHLIGHT
               BACKGROUND-COLOR GREEN.
               10  LINE 8 COLUMN 1 VALUE 'U. S. DOLLAR AMOUNT: '.
               10  PIC $ZZZ,ZZ9.99 FROM WS-RESULT
                   FOREGROUND-COLOR BROWN
                   HIGHLIGHT.
       01  AGAIN-SCREEN.
           05  FOREGROUND-COLOR WHITE
               HIGHLIGHT.
               10  LINE 11 COLUMN 1 VALUE 'MORE DATA? (YES OR NO): '.
               10  PIC X(3) TO MORE-DATA.
       01  ERROR-SCREEN.
           05  LINE 8 COLUMN 1 VALUE 'CODE NOT IN TABLE'
               FOREGROUND-COLOR BROWN
               HIGHLIGHT
               BACKGROUND-COLOR RED.
       PROCEDURE DIVISION.
       100-LOAD-TABLE.
           PERFORM VARYING X1 FROM 1 BY 1 UNTIL X1 > 10
               DISPLAY LOAD-SCREEN
               ACCEPT LOAD-SCREEN
           END-PERFORM.
       200-PROCESS-RTN.
           PERFORM UNTIL MORE-DATA = 'NO'
               DISPLAY INQUIRY-SCREEN
               ACCEPT INQUIRY-SCREEN
               SET X1 TO 1
               SEARCH COUNTRY
                   AT END DISPLAY ERROR-SCREEN
                   WHEN CODE-IN = COUNTRY-CODE (X1)
                       DIVIDE EXCHANGE-RATE (X1) INTO AMOUNT-IN
                           GIVING WS-RESULT
                       DISPLAY RESULT-SCREEN
               END-SEARCH
               DISPLAY AGAIN-SCREEN
               ACCEPT AGAIN-SCREEN
           END-PERFORM.
