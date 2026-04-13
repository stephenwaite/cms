       IDENTIFICATION DIVISION.
       PROGRAM-ID. garnoscan.
      *================================================================
      * GARNO CAPACITY SCANNER
      * Reads GARFILE sequentially and reports on garno prefix usage.
      * For each 3-letter prefix, shows:
      *   - Count of garnos allocated
      *   - Highest NUM-3 value seen (positions 4-6 of key)
      *   - Remaining capacity before overflow
      *   - Percent consumed (from that prefix's starting day)
      *================================================================

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GARFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
             LOCK MODE MANUAL.

           SELECT RPTFILE ASSIGN TO "GARNOSCAN.RPT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  GARFILE.
           COPY garfile.CPY.    

       FD RPTFILE.
       01 RPT-REC              PIC X(132).

       WORKING-STORAGE SECTION.

       01 WS-FS                PIC XX.
       01 WS-EOF               PIC 9 VALUE 0.

      * Garno key breakdown
       01 WS-GARNO-PARTS.
           03 WS-PREFIX         PIC X(3).
           03 WS-NUM3           PIC X(3).
           03 WS-XYZ            PIC X.
           03 WS-SUFFIX         PIC X.

      * Table to accumulate stats per prefix
      * Adjust max entries if you have more than 500 unique prefixes
       01 WS-MAX-PREFIXES      PIC 9(4) VALUE 2000.
       01 WS-PREFIX-COUNT       PIC 9(4) VALUE 0.

       01 WS-PREFIX-TABLE.
           03 WS-ENTRY OCCURS 2000 TIMES.
               05 WT-PREFIX     PIC X(3).
               05 WT-COUNT      PIC 9(5) VALUE 0.
               05 WT-MIN-NUM3   PIC 9(3) VALUE 999.
               05 WT-MAX-NUM3   PIC 9(3) VALUE 0.
               05 WT-MAX-XYZ    PIC 9   VALUE 0.

       01 WS-IDX               PIC 9(4).
       01 WS-FOUND             PIC 9 VALUE 0.
       01 WS-NUM3-NUM          PIC 9(3).
       01 WS-XYZ-NUM           PIC 9.
       01 WS-TOTAL-RECS        PIC 9(7) VALUE 0.
       01 WS-GARNO-RECS        PIC 9(7) VALUE 0.
       01 WS-REMAINING         PIC 9(5).
       01 WS-CAPACITY          PIC 9(5).
       01 WS-PCT               PIC 9(3).

      * Sort work fields
       01 WS-I                 PIC 9(4).
       01 WS-J                 PIC 9(4).
       01 WS-SWAP-ENTRY.
           03 SW-PREFIX         PIC X(3).
           03 SW-COUNT          PIC 9(5).
           03 SW-MIN-NUM3       PIC 9(3).
           03 SW-MAX-NUM3       PIC 9(3).
           03 SW-MAX-XYZ        PIC 9.

      * Report lines
       01 RPT-HEADER1.
           03 FILLER PIC X(50) VALUE
              "GARNO PREFIX CAPACITY REPORT".
       01 RPT-HEADER2.
           03 FILLER PIC X(132) VALUE ALL "-".
       01 RPT-HEADER3.
           03 FILLER PIC X(8) VALUE "PREFIX".
           03 FILLER PIC X(3) VALUE SPACES.
           03 FILLER PIC X(7) VALUE "COUNT".
           03 FILLER PIC X(3) VALUE SPACES.
           03 FILLER PIC X(9) VALUE "LOW-DAY".
           03 FILLER PIC X(3) VALUE SPACES.
           03 FILLER PIC X(10) VALUE "HIGH-DAY".
           03 FILLER PIC X(3) VALUE SPACES.
           03 FILLER PIC X(7) VALUE "HI-XYZ".
           03 FILLER PIC X(3) VALUE SPACES.
           03 FILLER PIC X(11) VALUE "REMAINING".
           03 FILLER PIC X(3) VALUE SPACES.
           03 FILLER PIC X(7) VALUE "PCT".

       01 RPT-DETAIL.
           03 RD-PREFIX         PIC X(3).
           03 FILLER            PIC X(8) VALUE SPACES.
           03 RD-COUNT          PIC Z(4)9.
           03 FILLER            PIC X(5) VALUE SPACES.
           03 RD-MIN-NUM3       PIC ZZ9.
           03 FILLER            PIC X(6) VALUE SPACES.
           03 RD-MAX-NUM3       PIC ZZ9.
           03 FILLER            PIC X(7) VALUE SPACES.
           03 RD-MAX-XYZ        PIC 9.
           03 FILLER            PIC X(5) VALUE SPACES.
           03 RD-REMAINING      PIC Z(4)9.
           03 FILLER            PIC X(5) VALUE SPACES.
           03 RD-PCT            PIC ZZ9.
           03 FILLER            PIC X VALUE "%".

       01 RPT-SUMMARY.
           03 FILLER PIC X(30) VALUE
              "TOTAL RECORDS READ:       ".
           03 RS-TOTAL          PIC Z(6)9.
       01 RPT-SUMMARY2.
           03 FILLER PIC X(30) VALUE
              "GARNO RECORDS (ending G):  ".
           03 RS-GARNO          PIC Z(6)9.
       01 RPT-SUMMARY3.
           03 FILLER PIC X(30) VALUE
              "UNIQUE PREFIXES:           ".
           03 RS-PREFIXES       PIC Z(6)9.

       01 RPT-WARNING.
           03 FILLER PIC X(4) VALUE "*** ".
           03 RW-PREFIX         PIC X(3).
           03 FILLER PIC X(35) VALUE
              " - HIGH-DAY ABOVE 900 - NEAR LIMIT".

       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT GARFILE

           OPEN OUTPUT RPTFILE

           PERFORM READ-ALL-RECORDS
           PERFORM SORT-BY-REMAINING
           PERFORM WRITE-REPORT

           CLOSE GARFILE
           CLOSE RPTFILE

           DISPLAY "SCAN COMPLETE. SEE GARNOSCAN.RPT"
           DISPLAY "TOTAL RECORDS: " WS-TOTAL-RECS
           DISPLAY "GARNO RECORDS: " WS-GARNO-RECS
           DISPLAY "UNIQUE PREFIXES: " WS-PREFIX-COUNT
           STOP RUN.

       READ-ALL-RECORDS.
           MOVE 0 TO WS-EOF
           READ GARFILE NEXT
               AT END MOVE 1 TO WS-EOF
           END-READ
           PERFORM UNTIL WS-EOF = 1
               ADD 1 TO WS-TOTAL-RECS
               MOVE G-GARNO TO WS-GARNO-PARTS
      * Only process garnos ending in "G"
               IF WS-SUFFIX = "G"
                   ADD 1 TO WS-GARNO-RECS
                   PERFORM PROCESS-GARNO
               END-IF
               READ GARFILE NEXT
                   AT END MOVE 1 TO WS-EOF
               END-READ
           END-PERFORM.
       READ-ALL-EXIT.
           EXIT.

       PROCESS-GARNO.
      * Validate NUM-3 is numeric
           IF WS-NUM3 IS NUMERIC
               MOVE WS-NUM3 TO WS-NUM3-NUM
           ELSE
               EXIT PARAGRAPH
           END-IF

           IF WS-XYZ IS NUMERIC
               MOVE WS-XYZ TO WS-XYZ-NUM
           ELSE
               MOVE 0 TO WS-XYZ-NUM
           END-IF

      * Look for existing prefix in table
           MOVE 0 TO WS-FOUND
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-PREFIX-COUNT
                  OR WS-FOUND = 1
               IF WT-PREFIX(WS-IDX) = WS-PREFIX
                   MOVE 1 TO WS-FOUND
               END-IF
           END-PERFORM

           IF WS-FOUND = 1
               SUBTRACT 1 FROM WS-IDX
               ADD 1 TO WT-COUNT(WS-IDX)
               IF WS-NUM3-NUM > WT-MAX-NUM3(WS-IDX)
                   MOVE WS-NUM3-NUM TO WT-MAX-NUM3(WS-IDX)
                   MOVE WS-XYZ-NUM TO WT-MAX-XYZ(WS-IDX)
               END-IF
               IF WS-NUM3-NUM < WT-MIN-NUM3(WS-IDX)
                   MOVE WS-NUM3-NUM TO WT-MIN-NUM3(WS-IDX)
               END-IF
           ELSE
               IF WS-PREFIX-COUNT < WS-MAX-PREFIXES
                   ADD 1 TO WS-PREFIX-COUNT
                   MOVE WS-PREFIX TO
                       WT-PREFIX(WS-PREFIX-COUNT)
                   MOVE 1 TO WT-COUNT(WS-PREFIX-COUNT)
                   MOVE WS-NUM3-NUM TO
                       WT-MIN-NUM3(WS-PREFIX-COUNT)
                   MOVE WS-NUM3-NUM TO
                       WT-MAX-NUM3(WS-PREFIX-COUNT)
                   MOVE WS-XYZ-NUM TO
                       WT-MAX-XYZ(WS-PREFIX-COUNT)
               END-IF
           END-IF.

       SORT-BY-REMAINING.
      * Bubble sort by MAX-NUM3 descending (most consumed first)
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I >= WS-PREFIX-COUNT
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > WS-PREFIX-COUNT - WS-I
                   IF WT-MAX-NUM3(WS-J) <
                      WT-MAX-NUM3(WS-J + 1)
                       MOVE WS-ENTRY(WS-J)
                           TO WS-SWAP-ENTRY
                       MOVE WS-ENTRY(WS-J + 1)
                           TO WS-ENTRY(WS-J)
                       MOVE WS-SWAP-ENTRY
                           TO WS-ENTRY(WS-J + 1)
                   END-IF
               END-PERFORM
           END-PERFORM.

       WRITE-REPORT.
           WRITE RPT-REC FROM RPT-HEADER1
           WRITE RPT-REC FROM RPT-HEADER2
           WRITE RPT-REC FROM RPT-HEADER3
           WRITE RPT-REC FROM RPT-HEADER2

           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-PREFIX-COUNT

               MOVE SPACES TO RPT-DETAIL
               MOVE WT-PREFIX(WS-IDX) TO RD-PREFIX
               MOVE WT-COUNT(WS-IDX) TO RD-COUNT
               MOVE WT-MIN-NUM3(WS-IDX) TO RD-MIN-NUM3
               MOVE WT-MAX-NUM3(WS-IDX) TO RD-MAX-NUM3
               MOVE WT-MAX-XYZ(WS-IDX) TO RD-MAX-XYZ

      * Remaining = (999 - MAX-NUM3) * 9
      *           + (9 - MAX-XYZ) if on the max day
               COMPUTE WS-REMAINING =
                   (999 - WT-MAX-NUM3(WS-IDX)) * 9
                   + (9 - WT-MAX-XYZ(WS-IDX))

      * Capacity from starting day
               COMPUTE WS-CAPACITY =
                   (999 - WT-MIN-NUM3(WS-IDX) + 1) * 9

               IF WS-CAPACITY > 0
                   COMPUTE WS-PCT =
                       ((WS-CAPACITY - WS-REMAINING)
                        * 100) / WS-CAPACITY
               ELSE
                   MOVE 100 TO WS-PCT
               END-IF

               MOVE WS-REMAINING TO RD-REMAINING
               MOVE WS-PCT TO RD-PCT

               WRITE RPT-REC FROM RPT-DETAIL
           END-PERFORM

           WRITE RPT-REC FROM RPT-HEADER2

      * Write warnings for any prefix with HIGH-DAY > 900
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-PREFIX-COUNT
               IF WT-MAX-NUM3(WS-IDX) > 900
                   MOVE SPACES TO RPT-WARNING
                   MOVE WT-PREFIX(WS-IDX) TO RW-PREFIX
                   WRITE RPT-REC FROM RPT-WARNING
               END-IF
           END-PERFORM

           WRITE RPT-REC FROM RPT-HEADER2
           MOVE WS-TOTAL-RECS TO RS-TOTAL
           WRITE RPT-REC FROM RPT-SUMMARY
           MOVE WS-GARNO-RECS TO RS-GARNO
           WRITE RPT-REC FROM RPT-SUMMARY2
           MOVE WS-PREFIX-COUNT TO RS-PREFIXES
           WRITE RPT-REC FROM RPT-SUMMARY3.
