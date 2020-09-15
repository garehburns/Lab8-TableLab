       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TABLELab.
       AUTHOR.        GARRETT BURNS.
      ******************************************************************
      *        CS370           TABLE LAB
      *
      *   IN THIS LAB YOU WILL ARE WORKING WITH WEATHER STATION DATA.
      *   THERE ARE 10 WEATHER STATIONS.  YOUR INPUT FILE HAS A
      *   STATION CODE.  THIS INFORMATION IS CODED IN A TABLE IN
      *   WORKING STORAGE.  YOUR WILL NEED TO:
      *                1.  CODE THE REDEFINES FOR THE TABLE
      *                2.  YOU NEED TO FIND THE STATION NAME AND
      *                    MOVE IT TO THE DETAIL LINE
      *                3.  You will also need to find and print
      *                    the high and low temp for each station
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

            SELECT WEATHER-FILE
                ASSIGN TO 'WEATHER.TXT'
                ORGANIZATION IS LINE SEQUENTIAL.
            SELECT REPORT-FILE
                ASSIGN TO 'REPORTCGB.TXT'.

       DATA DIVISION.
       FILE SECTION.

       FD WEATHER-FILE
          RECORD CONTAINS 39 CHARACTERS.

       01 WEATHER-ITEM.
          05 WI-WEATHER-STATION PIC X(3).
	      05 WI-WEATHER-ARRAY OCCURS 12 TIMES PIC 9(3).
  
       FD REPORT-FILE.

       01 REPORT-ITEM PIC X(80).

       WORKING-STORAGE SECTION.

       01 END-OF-FILE-FLAG  PIC X  VALUE SPACE.
          88 MORE-RECORDS            VALUE 'Y'.
          88 NO-MORE-RECORDS         VALUE 'N'.

       01 STATION-TEXT.
          05        PIC X(20) VALUE "AZOKalamazoo".
          05        PIC X(20) VALUE "BUFBuffalo".
          05        PIC X(20) VALUE "CVGCincinnati".
          05        PIC X(20) VALUE "GRRGrand Rapids".
          05        PIC X(20) VALUE "HOUHouston-Hobby".
          05        PIC X(20) VALUE "LAXLos Angeles".
          05        PIC X(20) VALUE "MDWChicago-Midway".
          05        PIC X(20) VALUE "MKGMuskegon".
          05        PIC X(20) VALUE "NRTTokyo-Narita".
          05        PIC X(20) VALUE "ORDChicago-O'Hare".

      *CODE THE REDEFINES OF THE ABOVE STATION-TEST INFORMATION. USE
      *AN INDEX

       01  STATION-TABLE REDEFINES STATION-TEXT.
           05  STATION-ITEM OCCURS 10 TIMES
               INDEXED BY STATION-INDEX.
               10  STATION-CODE     PIC X(3).
               10  STATION-NAME     PIC X(17).

       01 WS-HIGH-TEMP  PIC 9(3).
       01 WS-LOW-TEMP   PIC 9(3).

       01 SUB-X  PIC 99.

       01 HEADER-LINE.
          05        PIC X(25) VALUE SPACES.
          05        PIC X(25) VALUE '12 Hour Weather Summary'.

       01 HEADER-LINE-2.
          05        PIC X(31) VALUE 'Station'.
          05        PIC X(9)  VALUE 'High'.
          05        PIC X(3)  VALUE 'Low'.

       01 DETAIL-LINE.
          05 DETAIL-STATION-NAME  PIC X(17).
          05                      PIC X(15)    VALUE SPACES.
          05 DETAIL-HIGH-TEMP     PIC ZZ9.
          05                      PIC X(5)     VALUE SPACES.
          05 DETAIL-LOW-TEMP      PIC ZZ9.



       PROCEDURE DIVISION.

       100-MAIN.

           PERFORM 200-HOUSEKEEPING
           PERFORM 300-READ-ROUTINE
           PERFORM 600-EOJ-ROUTINE
           STOP RUN

          .

       200-HOUSEKEEPING.

           OPEN INPUT WEATHER-FILE
                OUTPUT REPORT-FILE
           PERFORM 700-PRINT-THE-HEADERS

          .

       300-READ-ROUTINE.

           PERFORM UNTIL NO-MORE-RECORDS
           READ WEATHER-FILE
               AT END
                   MOVE 'N' TO END-OF-FILE-FLAG
               NOT AT END
                   PERFORM 400-SEARCH-ROUTINE
            END-READ
            END-PERFORM
          .

       400-SEARCH-ROUTINE.

      * USE THE TABLE TO LOOKUP THE STATION NAME.  CODE A SEARCH WITH
      * AN INDEX.  IF THE TABLE NAME CAN'T BE FOUND PUT ERROR IN THE
      * NAME FIELD FOR THE STATION.  IF A NAME IS FOUND MOVE IT TO
      * THE DETAIL LINE


           SET STATION-INDEX TO 1
           SEARCH STATION-ITEM
           AT END MOVE 'NOT FOUND'TO DETAIL-STATION-NAME
               
           WHEN WI-WEATHER-STATION = STATION-CODE(STATION-INDEX)
               MOVE STATION-NAME(STATION-INDEX) TO DETAIL-STATION-NAME
                  
      *    ^^^                  
      *    COMPARES INPUT WEATHER STATION TO THE INDEX WE MADE 
      *    AND MOVES THE CORRESPONDING NAME TO DETAIL-LINE





           MOVE ZERO TO WS-HIGH-TEMP
           MOVE 999  TO WS-LOW-TEMP

      * FINDS THE HIGH AND LOW TEMPERATURES

           PERFORM 500-FIND-THE-HI-AND-LO
                VARYING SUB-X FROM 1 BY 1
                  UNTIL SUB-X > 12

           MOVE WS-HIGH-TEMP TO DETAIL-HIGH-TEMP
           MOVE WS-LOW-TEMP  TO DETAIL-LOW-TEMP

           WRITE REPORT-ITEM FROM DETAIL-LINE
               AFTER ADVANCING 1 LINE
          .

       500-FIND-THE-HI-AND-LO.
      *CODE FOR FINDING THE HIGH AND LOW TEMP HERE

      **** TWO IF-STATEMENTS ****
      
      *    HIGH TEMP
           IF WI-WEATHER-ARRAY(SUB-X) >= WS-HIGH-TEMP
               MOVE WI-WEATHER-ARRAY(SUB-X) TO WS-HIGH-TEMP
           END-IF
        
      *    LOW TEMP
           IF WI-WEATHER-ARRAY(SUB-X) <= WS-LOW-TEMP
               MOVE WI-WEATHER-ARRAY(SUB-X) TO WS-LOW-TEMP
           END-IF

          .

       600-EOJ-ROUTINE.
           CLOSE WEATHER-FILE
                 REPORT-FILE
          .
       700-PRINT-THE-HEADERS.
           WRITE REPORT-ITEM FROM HEADER-LINE
           AFTER ADVANCING 2 LINES
           WRITE REPORT-ITEM FROM HEADER-LINE-2
           AFTER ADVANCING 2 LINES
          .


