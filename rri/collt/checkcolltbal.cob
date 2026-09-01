      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
      *
      * checkcolltbal -- reconcile a kin018 collections export that was
      * never sent.  for each guarantor in the export, rebuild the
      * placement figures from CHARCUR/PAYCUR using kin018's own
      * arithmetic, then split off anything that has posted since, so
      * accounts that have paid can be pulled before the file goes out.
      *
      * placement population is CC-PAYCODE "018" with CC-DATE-A equal
      * to the run date of the kin018 job -- set PLACEDATE to that.
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. checkcolltbal.
       AUTHOR. SID WAITE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILEIN ASSIGN TO "S30"
             ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS WS-FI-STAT.

           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL
             FILE STATUS IS WS-CC-STAT.

           SELECT PAYCUR ASSIGN TO "S40" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
             LOCK MODE MANUAL
             FILE STATUS IS WS-PC-STAT.

           SELECT FILEOUT ASSIGN TO "S45"
             ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS WS-FO-STAT.

       DATA DIVISION.

       FILE SECTION.

       FD  CHARCUR.
           COPY CHARCUR.CPY.

       FD  PAYCUR.
           COPY PAYCUR.CPY.

      *----------------------------------------------------------------
      *    the kin018 export.  offsets counted straight off FILEOUT01
      *    in kin018, commas included:
      *      1-8     FO-GARNO
      *      173-182 FO-DISCHR   max CC-DATE-T, MM/DD/CCYY
      *      477-483 FO-CHG      ZZZZ.99
      *      485-491 FO-PAY      ZZZZ.99
      *      493-499 FO-ADJ      ZZZZ.99
      *      503-509 FO-BAL      ZZZZ.99
      *    record ends at 509; FILEOUT101 was X(520).
      *----------------------------------------------------------------
       FD  FILEIN.
       01  FILEIN-REC.
           05  FI-GARNO            PIC X(8).
           05  FILLER              PIC X(164).
           05  FI-DISCHR           PIC X(10).
           05  FILLER              PIC X(294).
           05  FI-CHG              PIC X(7).
           05  FILLER              PIC X.
           05  FI-PAY              PIC X(7).
           05  FILLER              PIC X.
           05  FI-ADJ              PIC X(7).
           05  FILLER              PIC X(3).
           05  FI-BAL              PIC X(7).
           05  FILLER              PIC X(11).

       FD  FILEOUT.
       01  FILEOUT01               PIC X(160).

       WORKING-STORAGE SECTION.

       01  WS-FI-STAT              PIC XX VALUE "00".
       01  WS-FO-STAT              PIC XX VALUE "00".
       01  WS-CC-STAT              PIC XX VALUE "00".
       01  WS-PC-STAT              PIC XX VALUE "00".

      *    kin018 run date, CCYYMMDD, from the PLACEDATE variable
       01  WS-PLACED               PIC X(8) VALUE SPACES.
       01  WS-PLACED-EDIT          PIC X(10).

       01  WS-GARNO                PIC X(8).

      *    expected figures parsed out of the export line
       01  EXP-CHG                 PIC S9(6)V99 VALUE 0.
       01  EXP-PAY                 PIC S9(6)V99 VALUE 0.
       01  EXP-ADJ                 PIC S9(6)V99 VALUE 0.
       01  EXP-BAL                 PIC S9(6)V99 VALUE 0.
       01  WS-BAL-OK               PIC X VALUE "N".
           88  BAL-VALID                 VALUE "Y".

      *    rebuilt as of the placement date
       01  THEN-CHG                PIC S9(6)V99 VALUE 0.
       01  THEN-PAY                PIC S9(6)V99 VALUE 0.
       01  THEN-ADJ                PIC S9(6)V99 VALUE 0.
       01  THEN-BAL                PIC S9(6)V99 VALUE 0.

      *    and as of right now
       01  NOW-BAL                 PIC S9(6)V99 VALUE 0.
       01  PAID-SINCE              PIC S9(6)V99 VALUE 0.
       01  WS-DRIFT                PIC S9(6)V99 VALUE 0.

      *    per charge line
       01  L-TOT-NOW               PIC S9(6)V99 VALUE 0.
       01  L-TOT-THEN              PIC S9(6)V99 VALUE 0.
       01  L-PAY                   PIC S9(6)V99 VALUE 0.
       01  L-ADJ                   PIC S9(6)V99 VALUE 0.
       01  L-SINCE                 PIC S9(6)V99 VALUE 0.

       01  WS-LINES                PIC 9(4) VALUE 0.
       01  WS-MAXDOS               PIC X(8) VALUE SPACES.
       01  WS-DISCHR-CMP           PIC X(8) VALUE SPACES.

      *    NUMVAL guard
       01  WS-BAD                  PIC X VALUE "N".
       01  WS-BI                   PIC 9(4) VALUE 0.
       01  WS-DIG                  PIC 9(4) VALUE 0.
       01  WS-CH                   PIC X.
       01  WS-PARSE                PIC X(7).
       01  WS-VALUE                PIC S9(6)V99 VALUE 0.

       01  WS-ACTION               PIC X(24).

       01  WS-GRAND.
           05  GT-EXP-BAL          PIC S9(9)V99 VALUE 0.
           05  GT-THEN-BAL         PIC S9(9)V99 VALUE 0.
           05  GT-NOW-BAL          PIC S9(9)V99 VALUE 0.
           05  GT-SINCE            PIC S9(9)V99 VALUE 0.
           05  GT-SENDABLE         PIC S9(9)V99 VALUE 0.

       01  WS-COUNTS.
           05  WS-READ-CNT         PIC 9(6) VALUE 0.
           05  WS-BADBAL-CNT       PIC 9(6) VALUE 0.
           05  WS-NOTFND-CNT       PIC 9(6) VALUE 0.
           05  WS-RECON-CNT        PIC 9(6) VALUE 0.
           05  WS-UNRECON-CNT      PIC 9(6) VALUE 0.
           05  WS-SEND-CNT         PIC 9(6) VALUE 0.
           05  WS-REDUCE-CNT       PIC 9(6) VALUE 0.
           05  WS-PULL-CNT         PIC 9(6) VALUE 0.
           05  WS-DOSDIFF-CNT      PIC 9(6) VALUE 0.

       01  HEAD-LINE.
           05  FILLER  PIC X(8)  VALUE "GARNO".
           05  FILLER  PIC XX    VALUE SPACE.
           05  FILLER  PIC X(4)  VALUE " LNS".
           05  FILLER  PIC XX    VALUE SPACE.
           05  FILLER  PIC X(11) VALUE "   FILE BAL".
           05  FILLER  PIC XX    VALUE SPACE.
           05  FILLER  PIC X(11) VALUE "  REBUILT@P".
           05  FILLER  PIC XX    VALUE SPACE.
           05  FILLER  PIC X(11) VALUE "  PAID SNCE".
           05  FILLER  PIC XX    VALUE SPACE.
           05  FILLER  PIC X(11) VALUE "    BAL NOW".
           05  FILLER  PIC XX    VALUE SPACE.
           05  FILLER  PIC X(11) VALUE "      DRIFT".
           05  FILLER  PIC XX    VALUE SPACE.
           05  FILLER  PIC X(24) VALUE "ACTION".

       01  DETAIL-LINE.
           05  DL-GARNO    PIC X(8).
           05  FILLER      PIC XX VALUE SPACE.
           05  DL-LINES    PIC ZZZ9.
           05  FILLER      PIC XX VALUE SPACE.
           05  DL-EXP      PIC -Z,ZZZ,ZZ9.99.
           05  FILLER      PIC XX VALUE SPACE.
           05  DL-THEN     PIC -Z,ZZZ,ZZ9.99.
           05  FILLER      PIC XX VALUE SPACE.
           05  DL-SINCE    PIC -Z,ZZZ,ZZ9.99.
           05  FILLER      PIC XX VALUE SPACE.
           05  DL-NOW      PIC -Z,ZZZ,ZZ9.99.
           05  FILLER      PIC XX VALUE SPACE.
           05  DL-DRIFT    PIC -Z,ZZZ,ZZ9.99.
           05  FILLER      PIC XX VALUE SPACE.
           05  DL-ACTION   PIC X(24).

       01  TOTAL-LINE.
           05  FILLER      PIC X(14) VALUE "TOTALS".
           05  TL-EXP      PIC -Z,ZZZ,ZZ9.99.
           05  FILLER      PIC XX VALUE SPACE.
           05  TL-THEN     PIC -Z,ZZZ,ZZ9.99.
           05  FILLER      PIC XX VALUE SPACE.
           05  TL-SINCE    PIC -Z,ZZZ,ZZ9.99.
           05  FILLER      PIC XX VALUE SPACE.
           05  TL-NOW      PIC -Z,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.

       P0.
           DISPLAY "PLACEDATE" UPON ENVIRONMENT-NAME.
           ACCEPT WS-PLACED FROM ENVIRONMENT-VALUE.

           IF WS-PLACED = SPACES OR WS-PLACED NOT NUMERIC
               DISPLAY "PLACEDATE must be set to the kin018 run date"
               DISPLAY "as CCYYMMDD, e.g. PLACEDATE=20250815"
               STOP RUN
           END-IF.

           MOVE SPACES TO WS-PLACED-EDIT.
           STRING WS-PLACED(5:2) "/" WS-PLACED(7:2) "/"
                  WS-PLACED(1:4) DELIMITED BY SIZE
             INTO WS-PLACED-EDIT
           END-STRING.

           OPEN INPUT CHARCUR PAYCUR FILEIN.
           OPEN OUTPUT FILEOUT.

           IF WS-CC-STAT NOT = "00"
               DISPLAY "OPEN CHARCUR FAILED, STATUS " WS-CC-STAT
               STOP RUN
           END-IF.
           IF WS-PC-STAT NOT = "00"
               DISPLAY "OPEN PAYCUR FAILED, STATUS " WS-PC-STAT
               STOP RUN
           END-IF.
           IF WS-FI-STAT NOT = "00"
               DISPLAY "OPEN FILEIN FAILED, STATUS " WS-FI-STAT
               STOP RUN
           END-IF.

           DISPLAY "PLACEMENT DATE " WS-PLACED-EDIT
                   "  (CC-DATE-A = " WS-PLACED ")".
           DISPLAY " ".
           MOVE HEAD-LINE TO FILEOUT01.
           WRITE FILEOUT01.
           DISPLAY HEAD-LINE.

      *----------------------------------------------------------------
       R1.
           READ FILEIN
             AT END
               GO TO R90
           END-READ.

           IF FI-GARNO = SPACES
               GO TO R1
           END-IF.

           ADD 1 TO WS-READ-CNT.
           MOVE FI-GARNO TO WS-GARNO.

           MOVE "Y" TO WS-BAL-OK.
           MOVE FI-CHG TO WS-PARSE. PERFORM P-PARSE.
             MOVE WS-VALUE TO EXP-CHG.
           MOVE FI-PAY TO WS-PARSE. PERFORM P-PARSE.
             MOVE WS-VALUE TO EXP-PAY.
           MOVE FI-ADJ TO WS-PARSE. PERFORM P-PARSE.
             MOVE WS-VALUE TO EXP-ADJ.
           MOVE FI-BAL TO WS-PARSE. PERFORM P-PARSE.
             MOVE WS-VALUE TO EXP-BAL.

           IF NOT BAL-VALID
               ADD 1 TO WS-BADBAL-CNT
               DISPLAY "UNPARSABLE AMOUNTS, LINE " WS-READ-CNT
                       " GARNO " WS-GARNO
                       " BAL [" FI-BAL "]"
           END-IF.

           MOVE 0 TO THEN-CHG THEN-PAY THEN-ADJ THEN-BAL.
           MOVE 0 TO NOW-BAL PAID-SINCE WS-LINES.
           MOVE SPACES TO WS-MAXDOS.

      *----------------------------------------------------------------
      *    every 018 charge line this guarantor got placed with on the
      *    kin018 run date
      *----------------------------------------------------------------
       R2.
           MOVE WS-GARNO TO CC-KEY8.
           MOVE LOW-VALUES TO CC-KEY3.
           START CHARCUR KEY NOT < CHARCUR-KEY
             INVALID KEY
               GO TO R6
           END-START.

       R3.
           READ CHARCUR NEXT
             AT END
               GO TO R6
           END-READ.

           IF CC-KEY8 NOT = WS-GARNO
               GO TO R6
           END-IF.

      *    alphanumeric compare on purpose -- CC-PAYCODE is PIC 999 and
      *    a numeric compare trips on any legacy record holding spaces
           IF CC-PAYCODE NOT = "018"
               GO TO R3
           END-IF.

           IF CC-DATE-A NOT = WS-PLACED
               GO TO R3
           END-IF.

           PERFORM P-SCAN-PAY.

      *    kin018 only counted a line whose balance was positive at the
      *    time, so judge inclusion on the rebuilt figure, not today's
           IF L-TOT-THEN NOT > 0
               GO TO R3
           END-IF.

           ADD 1 TO WS-LINES.
           ADD CC-AMOUNT TO THEN-CHG.
           ADD L-PAY     TO THEN-PAY.
           ADD L-ADJ     TO THEN-ADJ.
           ADD L-TOT-THEN TO THEN-BAL.
           ADD L-TOT-NOW  TO NOW-BAL.
           ADD L-SINCE    TO PAID-SINCE.

           IF CC-DATE-T > WS-MAXDOS
               MOVE CC-DATE-T TO WS-MAXDOS
           END-IF.

           GO TO R3.

      *----------------------------------------------------------------
       R6.
           COMPUTE WS-DRIFT = THEN-BAL - EXP-BAL.

           ADD EXP-BAL    TO GT-EXP-BAL.
           ADD THEN-BAL   TO GT-THEN-BAL.
           ADD NOW-BAL    TO GT-NOW-BAL.
           ADD PAID-SINCE TO GT-SINCE.

           EVALUATE TRUE
             WHEN WS-LINES = 0
               MOVE "PULL - NOT IN CHARCUR"  TO WS-ACTION
               ADD 1 TO WS-PULL-CNT
               ADD 1 TO WS-NOTFND-CNT
             WHEN NOW-BAL < 0
               MOVE "PULL - CREDIT BALANCE"  TO WS-ACTION
               ADD 1 TO WS-PULL-CNT
             WHEN NOW-BAL = 0
               MOVE "PULL - PAID IN FULL"    TO WS-ACTION
               ADD 1 TO WS-PULL-CNT
             WHEN PAID-SINCE NOT = 0
               MOVE "SEND - REDUCED"         TO WS-ACTION
               ADD 1 TO WS-REDUCE-CNT
               ADD NOW-BAL TO GT-SENDABLE
             WHEN OTHER
               MOVE "SEND"                   TO WS-ACTION
               ADD 1 TO WS-SEND-CNT
               ADD NOW-BAL TO GT-SENDABLE
           END-EVALUATE.

      *    the rebuild is only trustworthy if it reproduces what the
      *    file said at placement time
           IF WS-LINES > 0 AND BAL-VALID
               IF WS-DRIFT = 0
                   ADD 1 TO WS-RECON-CNT
               ELSE
                   ADD 1 TO WS-UNRECON-CNT
                   MOVE "CHECK - NO REBUILD"  TO WS-ACTION
               END-IF
           END-IF.

           MOVE SPACES TO DETAIL-LINE.
           MOVE WS-GARNO    TO DL-GARNO.
           MOVE WS-LINES    TO DL-LINES.
           MOVE EXP-BAL     TO DL-EXP.
           MOVE THEN-BAL    TO DL-THEN.
           MOVE PAID-SINCE  TO DL-SINCE.
           MOVE NOW-BAL     TO DL-NOW.
           MOVE WS-DRIFT    TO DL-DRIFT.
           MOVE WS-ACTION   TO DL-ACTION.

           MOVE DETAIL-LINE TO FILEOUT01.
           WRITE FILEOUT01.

           IF WS-ACTION NOT = "SEND"
               DISPLAY DETAIL-LINE
           END-IF.

      *    cross-check col 173 against the latest placed service date
           IF WS-LINES > 0
               MOVE SPACES TO WS-DISCHR-CMP
               STRING FI-DISCHR(7:4) FI-DISCHR(1:2) FI-DISCHR(4:2)
                 DELIMITED BY SIZE INTO WS-DISCHR-CMP
               END-STRING
               IF WS-DISCHR-CMP NOT = WS-MAXDOS
                   ADD 1 TO WS-DOSDIFF-CNT
                   DISPLAY "  DISCHR MISMATCH " WS-GARNO
                           " FILE [" FI-DISCHR "]"
                           " CHARCUR [" WS-MAXDOS "]"
               END-IF
           END-IF.

           GO TO R1.

      *----------------------------------------------------------------
      *    kin018's P3 -- payments matched on claim number.  PC-AMOUNT
      *    is negative, hence the adds.  PC-DENIAL "14" is an
      *    adjustment rather than a payment.  anything posted after the
      *    placement date is drift, split out separately.
      *----------------------------------------------------------------
       P-SCAN-PAY.
           MOVE CC-AMOUNT TO L-TOT-NOW.
           MOVE 0 TO L-PAY L-ADJ L-SINCE.

           MOVE CC-KEY8 TO PC-KEY8.
           MOVE LOW-VALUES TO PC-KEY3.
           START PAYCUR KEY NOT < PAYCUR-KEY
             INVALID KEY
               GO TO P-SCAN-X
           END-START.

       P-SCAN-1.
           READ PAYCUR NEXT
             AT END
               GO TO P-SCAN-X
           END-READ.

           IF PC-KEY8 NOT = CC-KEY8
               GO TO P-SCAN-X
           END-IF.

           IF PC-CLAIM NOT = CC-CLAIM
               GO TO P-SCAN-1
           END-IF.

           COMPUTE L-TOT-NOW = L-TOT-NOW + PC-AMOUNT.

           IF PC-DATE-T > WS-PLACED
               COMPUTE L-SINCE = L-SINCE - PC-AMOUNT
           ELSE
               IF PC-DENIAL = "14"
                   COMPUTE L-ADJ = L-ADJ - PC-AMOUNT
               ELSE
                   COMPUTE L-PAY = L-PAY - PC-AMOUNT
               END-IF
           END-IF.

           GO TO P-SCAN-1.

       P-SCAN-X.
           COMPUTE L-TOT-THEN = L-TOT-NOW + L-SINCE.

      *----------------------------------------------------------------
      *    ZZZZ.99 out of kin018, so digits, spaces and one point.
      *    validate before NUMVAL -- NUMVAL returns a number for junk
      *    rather than failing, and a silently wrong expected balance
      *    is worse than a loud one.
      *----------------------------------------------------------------
       P-PARSE.
           MOVE 0 TO WS-VALUE WS-DIG.
           MOVE "N" TO WS-BAD.

           IF WS-PARSE = SPACES
               MOVE "N" TO WS-BAL-OK
               GO TO P-PARSE-X
           END-IF.

           PERFORM VARYING WS-BI FROM 1 BY 1 UNTIL WS-BI > 7
               MOVE WS-PARSE(WS-BI:1) TO WS-CH
               EVALUATE WS-CH
                 WHEN SPACE
                 WHEN "."
                 WHEN ","
                 WHEN "-"
                   CONTINUE
                 WHEN "0" THRU "9"
                   ADD 1 TO WS-DIG
                 WHEN OTHER
                   MOVE "Y" TO WS-BAD
               END-EVALUATE
           END-PERFORM.

           IF WS-BAD = "N" AND WS-DIG > 0
               COMPUTE WS-VALUE = FUNCTION NUMVAL (WS-PARSE)
           ELSE
               MOVE "N" TO WS-BAL-OK
           END-IF.

       P-PARSE-X.
           EXIT.

      *----------------------------------------------------------------
       R90.
           MOVE SPACES TO TOTAL-LINE.
           MOVE "TOTALS"    TO TOTAL-LINE(1:14).
           MOVE GT-EXP-BAL  TO TL-EXP.
           MOVE GT-THEN-BAL TO TL-THEN.
           MOVE GT-SINCE    TO TL-SINCE.
           MOVE GT-NOW-BAL  TO TL-NOW.
           MOVE TOTAL-LINE TO FILEOUT01.
           WRITE FILEOUT01.
           DISPLAY " ".
           DISPLAY TOTAL-LINE.

       R99.
           DISPLAY " ".
           DISPLAY "GUARANTORS IN FILE : " WS-READ-CNT.
           DISPLAY "REBUILD AGREED     : " WS-RECON-CNT.
           DISPLAY "REBUILD DISAGREED  : " WS-UNRECON-CNT.
           DISPLAY "NOT FOUND IN CHARCUR: " WS-NOTFND-CNT.
           DISPLAY "UNPARSABLE AMOUNTS : " WS-BADBAL-CNT.
           DISPLAY "DISCHR MISMATCHES  : " WS-DOSDIFF-CNT.
           DISPLAY " ".
           DISPLAY "SEND AS IS         : " WS-SEND-CNT.
           DISPLAY "SEND REDUCED       : " WS-REDUCE-CNT.
           DISPLAY "PULL FROM FILE     : " WS-PULL-CNT.
           DISPLAY "AMT TO AGENCY      : " GT-SENDABLE.
           CLOSE CHARCUR PAYCUR FILEIN FILEOUT.
           STOP RUN.