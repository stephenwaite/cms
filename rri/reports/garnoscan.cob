       IDENTIFICATION DIVISION.
       PROGRAM-ID. GARNOSCAN.
      *================================================================
      * GARNO CAPACITY SCANNER - REVISED V3
      * Uses gap detection to separate organic allocation from
      * scattered rename artifacts. The first gap > 50 in NUM-3
      * sequence marks the end of organic allocation.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GARFILE ASSIGN TO "S35"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS G-GARNO
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
       01 WS-EOF               PIC 9 VALUE 0.
      * Garno key breakdown
       01 WS-GARNO-PARTS.
           03 WS-PREFIX         PIC X(3).
           03 WS-NUM3           PIC X(3).
           03 WS-XYZ            PIC X.
           03 WS-SUFFIX         PIC X.
      * Gap detection threshold
       01 WS-GAP-THRESHOLD     PIC 9(3) VALUE 50.
      * Table to accumulate stats per prefix
       01 WS-MAX-PREFIXES      PIC 9(4) VALUE 4000.
       01 WS-PREFIX-COUNT      PIC 9(4) VALUE 0.
       01 WS-PREFIX-TABLE.
           03 WS-ENTRY OCCURS 4000 TIMES.
               05 WT-PREFIX     PIC X(3).
               05 WT-COUNT      PIC 9(5) VALUE 0.
               05 WT-MIN-NUM3   PIC 9(3) VALUE 999.
      * Tracking during scan
               05 WT-PREV-NUM3  PIC 9(3) VALUE 0.
               05 WT-PREV-XYZ   PIC 9   VALUE 0.
      * Organic frontier (below first big gap)
               05 WT-ORG-NUM3   PIC 9(3) VALUE 0.
               05 WT-ORG-XYZ    PIC 9   VALUE 0.
               05 WT-GAP-FOUND  PIC 9   VALUE 0.
      * Overall max including renames
               05 WT-TRUE-MAX   PIC 9(3) VALUE 0.
               05 WT-TRUE-XYZ   PIC 9   VALUE 0.
       01 WS-IDX               PIC 9(4).
       01 WS-FOUND             PIC 9 VALUE 0.
       01 WS-NUM3-NUM          PIC 9(3).
       01 WS-XYZ-NUM           PIC 9.
       01 WS-GAP               PIC 9(4).
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
           03 SW-PREV-NUM3      PIC 9(3).
           03 SW-PREV-XYZ       PIC 9.
           03 SW-ORG-NUM3       PIC 9(3).
           03 SW-ORG-XYZ        PIC 9.
           03 SW-GAP-FOUND      PIC 9.
           03 SW-TRUE-MAX       PIC 9(3).
           03 SW-TRUE-XYZ       PIC 9.
      * Report lines
       01 RPT-HEADER1.
           03 FILLER PIC X(60) VALUE
              "GARNO PREFIX CAPACITY REPORT (GAP DETECTION V3)".
       01 RPT-HEADER2.
           03 FILLER PIC X(132) VALUE ALL "-".
       01 RPT-HEADER3.
           03 FILLER PIC X(8)  VALUE "PREFIX".
           03 FILLER PIC X(2)  VALUE SPACES.
           03 FILLER PIC X(7)  VALUE "COUNT".
           03 FILLER PIC X(2)  VALUE SPACES.
           03 FILLER PIC X(8)  VALUE "ORG-HI".
           03 FILLER PIC X(2)  VALUE SPACES.
           03 FILLER PIC X(7)  VALUE "O-XYZ".
           03 FILLER PIC X(2)  VALUE SPACES.
           03 FILLER PIC X(10) VALUE "TRUE-MAX".
           03 FILLER PIC X(2)  VALUE SPACES.
           03 FILLER PIC X(11) VALUE "REMAINING".
           03 FILLER PIC X(2)  VALUE SPACES.
           03 FILLER PIC X(5)  VALUE "PCT".
           03 FILLER PIC X(2)  VALUE SPACES.
           03 FILLER PIC X(8)  VALUE "RENAMES?".
       01 RPT-DETAIL.
           03 RD-PREFIX         PIC X(3).
           03 FILLER            PIC X(7) VALUE SPACES.
           03 RD-COUNT          PIC Z(4)9.
           03 FILLER            PIC X(4) VALUE SPACES.
           03 RD-ORG-NUM3       PIC ZZ9.
           03 FILLER            PIC X(5) VALUE SPACES.
           03 RD-ORG-XYZ        PIC 9.
           03 FILLER            PIC X(5) VALUE SPACES.
           03 RD-TRUE-MAX       PIC ZZ9.
           03 FILLER            PIC X(7) VALUE SPACES.
           03 RD-REMAINING      PIC Z(4)9.
           03 FILLER            PIC X(4) VALUE SPACES.
           03 RD-PCT            PIC ZZ9.
           03 FILLER            PIC X    VALUE "%".
           03 FILLER            PIC X(3) VALUE SPACES.
           03 RD-RENAME-FLAG    PIC X(3).
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
       01 RPT-SUMMARY4.
           03 FILLER PIC X(30) VALUE
              "GAP THRESHOLD:             ".
           03 RS-THRESH         PIC ZZ9.
       01 RPT-WARNING.
           03 FILLER PIC X(4) VALUE "*** ".
           03 RW-PREFIX         PIC X(3).
           03 RW-MSG            PIC X(50).
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT GARFILE
           OPEN OUTPUT RPTFILE
           PERFORM READ-ALL-RECORDS
           PERFORM FINALIZE-PREFIXES
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
      * Update overall true max
               IF WS-NUM3-NUM > WT-TRUE-MAX(WS-IDX)
                   MOVE WS-NUM3-NUM
                       TO WT-TRUE-MAX(WS-IDX)
                   MOVE WS-XYZ-NUM
                       TO WT-TRUE-XYZ(WS-IDX)
               ELSE
                   IF WS-NUM3-NUM = WT-TRUE-MAX(WS-IDX)
                   AND WS-XYZ-NUM > WT-TRUE-XYZ(WS-IDX)
                       MOVE WS-XYZ-NUM
                           TO WT-TRUE-XYZ(WS-IDX)
                   END-IF
               END-IF
      * Update min
               IF WS-NUM3-NUM < WT-MIN-NUM3(WS-IDX)
                   MOVE WS-NUM3-NUM
                       TO WT-MIN-NUM3(WS-IDX)
               END-IF
      * Gap detection - records arrive in NUM-3 order
               IF WS-NUM3-NUM > WT-PREV-NUM3(WS-IDX)
      * New NUM-3 level - check for gap
                   COMPUTE WS-GAP =
                       WS-NUM3-NUM
                       - WT-PREV-NUM3(WS-IDX)
                   IF WS-GAP > WS-GAP-THRESHOLD
                   AND WT-GAP-FOUND(WS-IDX) = 0
      * First big gap found - lock in organic frontier
                       MOVE WT-PREV-NUM3(WS-IDX)
                           TO WT-ORG-NUM3(WS-IDX)
                       MOVE WT-PREV-XYZ(WS-IDX)
                           TO WT-ORG-XYZ(WS-IDX)
                       MOVE 1 TO WT-GAP-FOUND(WS-IDX)
                   END-IF
                   MOVE WS-NUM3-NUM
                       TO WT-PREV-NUM3(WS-IDX)
                   MOVE WS-XYZ-NUM
                       TO WT-PREV-XYZ(WS-IDX)
               ELSE
      * Same NUM-3 - track highest XYZ
                   IF WS-NUM3-NUM = WT-PREV-NUM3(WS-IDX)
                   AND WS-XYZ-NUM > WT-PREV-XYZ(WS-IDX)
                       MOVE WS-XYZ-NUM
                           TO WT-PREV-XYZ(WS-IDX)
                   END-IF
               END-IF
           ELSE
      * New prefix
               IF WS-PREFIX-COUNT < WS-MAX-PREFIXES
                   ADD 1 TO WS-PREFIX-COUNT
                   MOVE WS-PREFIX TO
                       WT-PREFIX(WS-PREFIX-COUNT)
                   MOVE 1 TO WT-COUNT(WS-PREFIX-COUNT)
                   MOVE WS-NUM3-NUM TO
                       WT-MIN-NUM3(WS-PREFIX-COUNT)
                   MOVE WS-NUM3-NUM TO
                       WT-PREV-NUM3(WS-PREFIX-COUNT)
                   MOVE WS-XYZ-NUM TO
                       WT-PREV-XYZ(WS-PREFIX-COUNT)
                   MOVE WS-NUM3-NUM TO
                       WT-TRUE-MAX(WS-PREFIX-COUNT)
                   MOVE WS-XYZ-NUM TO
                       WT-TRUE-XYZ(WS-PREFIX-COUNT)
                   MOVE 0 TO
                       WT-ORG-NUM3(WS-PREFIX-COUNT)
                   MOVE 0 TO
                       WT-ORG-XYZ(WS-PREFIX-COUNT)
                   MOVE 0 TO
                       WT-GAP-FOUND(WS-PREFIX-COUNT)
               END-IF
           END-IF.
       FINALIZE-PREFIXES.
      * For prefixes where no big gap was found,
      * the organic frontier IS the true max
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-PREFIX-COUNT
               IF WT-GAP-FOUND(WS-IDX) = 0
                   MOVE WT-PREV-NUM3(WS-IDX)
                       TO WT-ORG-NUM3(WS-IDX)
                   MOVE WT-PREV-XYZ(WS-IDX)
                       TO WT-ORG-XYZ(WS-IDX)
               END-IF
           END-PERFORM.
       SORT-BY-REMAINING.
      * Bubble sort by ORG-NUM3 descending (most consumed first)
           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I >= WS-PREFIX-COUNT
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > WS-PREFIX-COUNT - WS-I
                   IF WT-ORG-NUM3(WS-J) <
                      WT-ORG-NUM3(WS-J + 1)
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
               MOVE WT-ORG-NUM3(WS-IDX) TO RD-ORG-NUM3
               MOVE WT-ORG-XYZ(WS-IDX) TO RD-ORG-XYZ
               MOVE WT-TRUE-MAX(WS-IDX) TO RD-TRUE-MAX
      * Remaining based on organic frontier
               COMPUTE WS-REMAINING =
                   (999 - WT-ORG-NUM3(WS-IDX)) * 9
                   + (9 - WT-ORG-XYZ(WS-IDX))
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
      * Flag if renames detected
               IF WT-GAP-FOUND(WS-IDX) = 1
                   MOVE "YES" TO RD-RENAME-FLAG
               ELSE
                   MOVE "   " TO RD-RENAME-FLAG
               END-IF
               WRITE RPT-REC FROM RPT-DETAIL
           END-PERFORM
           WRITE RPT-REC FROM RPT-HEADER2
      * Warnings for organic frontier above 900
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-PREFIX-COUNT
               IF WT-ORG-NUM3(WS-IDX) > 900
                   MOVE SPACES TO RPT-WARNING
                   MOVE WT-PREFIX(WS-IDX) TO RW-PREFIX
                   IF WT-GAP-FOUND(WS-IDX) = 1
                       MOVE
                       " ORGANIC >900, RENAMES ABOVE"
                           TO RW-MSG
                   ELSE
                       MOVE
                       " ORGANIC >900, NO RENAMES"
                           TO RW-MSG
                   END-IF
                   WRITE RPT-REC FROM RPT-WARNING
               END-IF
           END-PERFORM
           WRITE RPT-REC FROM RPT-HEADER2
           MOVE WS-TOTAL-RECS TO RS-TOTAL
           WRITE RPT-REC FROM RPT-SUMMARY
           MOVE WS-GARNO-RECS TO RS-GARNO
           WRITE RPT-REC FROM RPT-SUMMARY2
           MOVE WS-PREFIX-COUNT TO RS-PREFIXES
           WRITE RPT-REC FROM RPT-SUMMARY3
           MOVE WS-GAP-THRESHOLD TO RS-THRESH
           WRITE RPT-REC FROM RPT-SUMMARY4.