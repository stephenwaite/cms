      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3

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
      *    input record
      *      1-8     account key
      *      173-182 date of service, MM/DD/CCYY
      *      503-514 expected balance, e.g. "1026.90"
      *    narrow FI-BAL if a different field starts before 515.
      *    over-declaring the record is safe on LINE SEQUENTIAL --
      *    short lines get space padded -- but under-declaring
      *    truncates, which is what hid col 173 the first time round.
      *----------------------------------------------------------------
       FD  FILEIN.
       01  FILEIN-REC.
           05  FI-KEY8             PIC X(8).
           05  FILLER              PIC X(164).
           05  FI-DOS.
               10  FI-DOS-MM       PIC XX.
               10  FI-DOS-SL1      PIC X.
               10  FI-DOS-DD       PIC XX.
               10  FI-DOS-SL2      PIC X.
               10  FI-DOS-CCYY     PIC X(4).
           05  FILLER              PIC X(320).
           05  FI-BAL              PIC X(12).
           05  FILLER              PIC X(86).

       FD  FILEOUT.
       01  FILEOUT01               PIC X(132).

       WORKING-STORAGE SECTION.

       01  WS-FI-STAT              PIC XX VALUE "00".
       01  WS-FO-STAT              PIC XX VALUE "00".
       01  WS-CC-STAT              PIC XX VALUE "00".
       01  WS-PC-STAT              PIC XX VALUE "00".

      *    set to "Y" for a one-time run that dumps the first 20 charge
      *    records so you can eyeball the CC-DATE-T layout
       01  WS-DEBUG                PIC X VALUE "N".
           88  DEBUG-ON                  VALUE "Y".
       01  WS-DEBUG-CNT            PIC 9(4) VALUE 0.

      *    held in WS -- the FD record areas are overwritten by every
      *    READ NEXT on CHARCUR / PAYCUR
       01  WS-KEY8                 PIC X(8).
       01  WS-DOS-EDIT             PIC X(10).

      *    the date of service in CHARCUR's stored layout.  CC-DATE-T
      *    is X(8); pick the branch in P-EDIT-DOS that matches.
       01  WS-CHG-DATE             PIC X(8).

       01  WS-DOS-OK               PIC X VALUE "N".
           88  DOS-VALID                 VALUE "Y".

      *    col 503 parse
       01  WS-FILE-BAL             PIC S9(6)V99 VALUE 0.
       01  WS-VARIANCE             PIC S9(6)V99 VALUE 0.
       01  WS-BAL-OK               PIC X VALUE "N".
           88  BAL-VALID                 VALUE "Y".
       01  WS-BAL-BAD              PIC X VALUE "N".
       01  WS-BI                   PIC 9(4) VALUE 0.
       01  WS-DIG-CNT              PIC 9(4) VALUE 0.
       01  WS-CH                   PIC X.

      *----------------------------------------------------------------
      *    claim numbers found on the 018 charges for this key + DOS.
      *    payments are counted only when PC-CLAIM is in this table.
      *----------------------------------------------------------------
       01  WS-CLAIM-TAB.
           05  WS-CLAIM-CNT        PIC 9(4) VALUE 0.
           05  WS-CLAIM-ENT        OCCURS 200 TIMES PIC X(6).
       01  WS-CT-IX                PIC 9(4) VALUE 0.
       01  WS-CLAIM-HIT            PIC X VALUE "N".
           88  CLAIM-HIT                 VALUE "Y".
       01  WS-CLAIM-OVF            PIC X VALUE "N".
           88  CLAIM-OVERFLOW            VALUE "Y".

       01  CHARGE-TOT              PIC S9(6)V99 VALUE 0.
       01  PAYMENT-TOT             PIC S9(6)V99 VALUE 0.
       01  UNAPPLIED-TOT           PIC S9(6)V99 VALUE 0.
       01  CLAIM-TOT               PIC S9(6)V99 VALUE 0.

       01  WS-GRAND.
           05  GT-CHARGES          PIC S9(9)V99 VALUE 0.
           05  GT-PAYMENTS         PIC S9(9)V99 VALUE 0.
           05  GT-COMPUTED         PIC S9(9)V99 VALUE 0.
           05  GT-EXPECTED         PIC S9(9)V99 VALUE 0.
           05  GT-UNAPPLIED        PIC S9(9)V99 VALUE 0.

       01  WS-COUNTS.
           05  WS-READ-CNT         PIC 9(6) VALUE 0.
           05  WS-SKIP-CNT         PIC 9(6) VALUE 0.
           05  WS-BADBAL-CNT       PIC 9(6) VALUE 0.
           05  WS-NOCHG-CNT        PIC 9(6) VALUE 0.
           05  WS-MATCH-CNT        PIC 9(6) VALUE 0.
           05  WS-DIFF-CNT         PIC 9(6) VALUE 0.
           05  WS-OPEN-CNT         PIC 9(6) VALUE 0.
           05  WS-ZERO-CNT         PIC 9(6) VALUE 0.
           05  WS-CRED-CNT         PIC 9(6) VALUE 0.
           05  WS-OVF-CNT          PIC 9(6) VALUE 0.

       01  HEAD-LINE.
           05  FILLER              PIC X(8)  VALUE "KEY".
           05  FILLER              PIC XX    VALUE SPACE.
           05  FILLER              PIC X(10) VALUE "DOS".
           05  FILLER              PIC XX    VALUE SPACE.
           05  FILLER              PIC X(3)  VALUE "CLM".
           05  FILLER              PIC XX    VALUE SPACE.
           05  FILLER              PIC X(13) VALUE "      CHARGES".
           05  FILLER              PIC XX    VALUE SPACE.
           05  FILLER              PIC X(13) VALUE "     PAYMENTS".
           05  FILLER              PIC XX    VALUE SPACE.
           05  FILLER              PIC X(13) VALUE "     COMPUTED".
           05  FILLER              PIC XX    VALUE SPACE.
           05  FILLER              PIC X(13) VALUE "     EXPECTED".
           05  FILLER              PIC XX    VALUE SPACE.
           05  FILLER              PIC X(13) VALUE "     VARIANCE".

       01  DETAIL-LINE.
           05  DL-KEY8             PIC X(8).
           05  FILLER              PIC XX    VALUE SPACE.
           05  DL-DOS              PIC X(10).
           05  FILLER              PIC XX    VALUE SPACE.
           05  DL-CLAIMS           PIC ZZ9.
           05  FILLER              PIC XX    VALUE SPACE.
           05  DL-CHARGES          PIC -Z,ZZZ,ZZ9.99.
           05  FILLER              PIC XX    VALUE SPACE.
           05  DL-PAYMENTS         PIC -Z,ZZZ,ZZ9.99.
           05  FILLER              PIC XX    VALUE SPACE.
           05  DL-COMPUTED         PIC -Z,ZZZ,ZZ9.99.
           05  FILLER              PIC XX    VALUE SPACE.
           05  DL-EXPECTED         PIC -Z,ZZZ,ZZ9.99.
           05  FILLER              PIC XX    VALUE SPACE.
           05  DL-VARIANCE         PIC -Z,ZZZ,ZZ9.99.

       01  TOTAL-LINE.
           05  FILLER              PIC X(25) VALUE "TOTALS".
           05  TL-CHARGES          PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER              PIC XX    VALUE SPACE.
           05  TL-PAYMENTS         PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER              PIC XX    VALUE SPACE.
           05  TL-COMPUTED         PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER              PIC XX    VALUE SPACE.
           05  TL-EXPECTED         PIC -ZZ,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.

       P0.
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

           MOVE HEAD-LINE TO FILEOUT01.
           WRITE FILEOUT01.
           DISPLAY HEAD-LINE.

       R1.
           READ FILEIN
             AT END
               GO TO R90
           END-READ.

           ADD 1 TO WS-READ-CNT.
           MOVE FI-KEY8 TO WS-KEY8.
           PERFORM P-EDIT-DOS.
           PERFORM P-EDIT-BAL.

           IF NOT DOS-VALID
               ADD 1 TO WS-SKIP-CNT
               DISPLAY "BAD DOS, LINE " WS-READ-CNT
                       " KEY " WS-KEY8 " DOS [" FI-DOS "]"
               GO TO R1
           END-IF.

           IF NOT BAL-VALID
               ADD 1 TO WS-BADBAL-CNT
               DISPLAY "BAD BALANCE, LINE " WS-READ-CNT
                       " KEY " WS-KEY8 " COL503 [" FI-BAL "]"
           END-IF.

           MOVE 0 TO CHARGE-TOT PAYMENT-TOT UNAPPLIED-TOT CLAIM-TOT.
           MOVE 0 TO WS-CLAIM-CNT.
           MOVE "N" TO WS-CLAIM-OVF.

      *----------------------------------------------------------------
      *    pass 1 -- 018 charges for this key on this date of service.
      *    collect the distinct claim numbers as we go.
      *----------------------------------------------------------------
       R2.
           MOVE WS-KEY8 TO CC-KEY8.
           MOVE LOW-VALUES TO CC-KEY3.
           START CHARCUR KEY NOT < CHARCUR-KEY
             INVALID KEY
               GO TO R4
           END-START.

       R3.
           READ CHARCUR NEXT
             AT END
               GO TO R4
           END-READ.

           IF CC-KEY8 NOT = WS-KEY8
               GO TO R4
           END-IF.

           IF DEBUG-ON AND WS-DEBUG-CNT < 20
               ADD 1 TO WS-DEBUG-CNT
               DISPLAY "DBG KEY " CC-KEY8 "/" CC-KEY3
                       " CLAIM [" CC-CLAIM "]"
                       " PAYCODE [" CC-PAYCODE "]"
                       " DATE-T [" CC-DATE-T "]"
                       " DAT1 [" CC-DAT1 "]"
                       " AMT " CC-AMOUNT
           END-IF.

      *    alphanumeric compare on purpose -- CC-PAYCODE is PIC 999 but
      *    a numeric compare aborts under -fnumeric-check if any legacy
      *    record has spaces in it
           IF CC-PAYCODE NOT = "018"
               GO TO R3
           END-IF.

           IF CC-DATE-T NOT = WS-CHG-DATE
               GO TO R3
           END-IF.

           ADD CC-AMOUNT TO CHARGE-TOT.
           PERFORM P-ADD-CLAIM.
           GO TO R3.

      *----------------------------------------------------------------
      *    pass 2 -- payments for this key whose PC-CLAIM matches one of
      *    the claims collected above.  PAYCUR carries no service date,
      *    so the claim number is the only link back to the DOS.
      *----------------------------------------------------------------
       R4.
           IF WS-CLAIM-CNT = 0
               ADD 1 TO WS-NOCHG-CNT
               GO TO R6
           END-IF.

           IF CLAIM-OVERFLOW
               ADD 1 TO WS-OVF-CNT
               DISPLAY "CLAIM TABLE FULL, KEY " WS-KEY8
                       " DOS " WS-DOS-EDIT " -- TOTALS INCOMPLETE"
           END-IF.

           MOVE WS-KEY8 TO PC-KEY8.
           MOVE LOW-VALUES TO PC-KEY3.
           START PAYCUR KEY NOT < PAYCUR-KEY
             INVALID KEY
               GO TO R6
           END-START.

       R5.
           READ PAYCUR NEXT
             AT END
               GO TO R6
           END-READ.

           IF PC-KEY8 NOT = WS-KEY8
               GO TO R6
           END-IF.

      *    unapplied money -- reported separately, not netted
           IF PC-CLAIM = SPACES OR PC-CLAIM = ZEROS
               ADD PC-AMOUNT TO UNAPPLIED-TOT
               GO TO R5
           END-IF.

           PERFORM P-FIND-CLAIM.
           IF NOT CLAIM-HIT
               GO TO R5
           END-IF.

           ADD PC-AMOUNT TO PAYMENT-TOT.
           GO TO R5.

      *----------------------------------------------------------------
      *    reconcile computed balance against col 503
      *----------------------------------------------------------------
       R6.
      *    >>> if PAYCUR stores payments as negative, change this to
      *    >>> COMPUTE CLAIM-TOT = CHARGE-TOT + PAYMENT-TOT
           COMPUTE CLAIM-TOT = CHARGE-TOT - PAYMENT-TOT.
           COMPUTE WS-VARIANCE = CLAIM-TOT - WS-FILE-BAL.

           ADD CHARGE-TOT    TO GT-CHARGES.
           ADD PAYMENT-TOT   TO GT-PAYMENTS.
           ADD CLAIM-TOT     TO GT-COMPUTED.
           ADD WS-FILE-BAL   TO GT-EXPECTED.
           ADD UNAPPLIED-TOT TO GT-UNAPPLIED.

           MOVE SPACES TO DETAIL-LINE.
           MOVE WS-KEY8      TO DL-KEY8.
           MOVE WS-DOS-EDIT  TO DL-DOS.
           MOVE WS-CLAIM-CNT TO DL-CLAIMS.
           MOVE CHARGE-TOT   TO DL-CHARGES.
           MOVE PAYMENT-TOT  TO DL-PAYMENTS.
           MOVE CLAIM-TOT    TO DL-COMPUTED.
           MOVE WS-FILE-BAL  TO DL-EXPECTED.
           MOVE WS-VARIANCE  TO DL-VARIANCE.

           EVALUATE TRUE
             WHEN CLAIM-TOT > 0
               ADD 1 TO WS-OPEN-CNT
             WHEN CLAIM-TOT < 0
               ADD 1 TO WS-CRED-CNT
             WHEN OTHER
               ADD 1 TO WS-ZERO-CNT
           END-EVALUATE.

           MOVE DETAIL-LINE TO FILEOUT01.
           WRITE FILEOUT01.

           EVALUATE TRUE
             WHEN NOT BAL-VALID
               DISPLAY DETAIL-LINE " NO EXPECTED BALANCE"
             WHEN WS-CLAIM-CNT = 0
               ADD 1 TO WS-DIFF-CNT
               DISPLAY DETAIL-LINE " NO 018 CHARGES ON THIS DOS"
             WHEN WS-VARIANCE = 0
               ADD 1 TO WS-MATCH-CNT
             WHEN OTHER
               ADD 1 TO WS-DIFF-CNT
               DISPLAY DETAIL-LINE " *** VARIANCE ***"
           END-EVALUATE.

           IF UNAPPLIED-TOT NOT = 0
               DISPLAY "  UNAPPLIED ON KEY " WS-KEY8
                       " " UNAPPLIED-TOT
           END-IF.

           GO TO R1.

      *----------------------------------------------------------------
      *    validate MM/DD/CCYY and build the CHARCUR comparison date
      *----------------------------------------------------------------
       P-EDIT-DOS.
           MOVE "N" TO WS-DOS-OK.
           MOVE SPACES TO WS-DOS-EDIT WS-CHG-DATE.

           IF FI-DOS = SPACES
               GO TO P-EDIT-DOS-X
           END-IF.
           IF FI-DOS-SL1 NOT = "/" OR FI-DOS-SL2 NOT = "/"
               GO TO P-EDIT-DOS-X
           END-IF.
           IF FI-DOS-MM   NOT NUMERIC
             OR FI-DOS-DD   NOT NUMERIC
             OR FI-DOS-CCYY NOT NUMERIC
               GO TO P-EDIT-DOS-X
           END-IF.

           MOVE FI-DOS TO WS-DOS-EDIT.

      *    ---- pick ONE of the three, delete the rest ----

      *    CCYYMMDD
           STRING FI-DOS-CCYY FI-DOS-MM FI-DOS-DD
             DELIMITED BY SIZE INTO WS-CHG-DATE
           END-STRING.

      *    MMDDCCYY
      *    STRING FI-DOS-MM FI-DOS-DD FI-DOS-CCYY
      *      DELIMITED BY SIZE INTO WS-CHG-DATE
      *    END-STRING.

      *    MM/DD/YY
      *    STRING FI-DOS-MM "/" FI-DOS-DD "/" FI-DOS-CCYY(3:2)
      *      DELIMITED BY SIZE INTO WS-CHG-DATE
      *    END-STRING.

           MOVE "Y" TO WS-DOS-OK.

       P-EDIT-DOS-X.
           EXIT.

      *----------------------------------------------------------------
      *    parse the expected balance out of col 503.  tolerant of
      *    leading spaces, a currency sign, commas and a trailing or
      *    leading minus; anything else marks the line unusable rather
      *    than letting NUMVAL return garbage.
      *----------------------------------------------------------------
       P-EDIT-BAL.
           MOVE "N" TO WS-BAL-OK.
           MOVE "N" TO WS-BAL-BAD.
           MOVE 0 TO WS-FILE-BAL.
           MOVE 0 TO WS-DIG-CNT.

           IF FI-BAL NOT = SPACES
               PERFORM VARYING WS-BI FROM 1 BY 1
                 UNTIL WS-BI > LENGTH OF FI-BAL
                   MOVE FI-BAL(WS-BI:1) TO WS-CH
                   EVALUATE WS-CH
                     WHEN SPACE
                     WHEN ","
                     WHEN "."
                     WHEN "-"
                     WHEN "+"
                     WHEN "$"
                       CONTINUE
                     WHEN "0" THRU "9"
                       ADD 1 TO WS-DIG-CNT
                     WHEN OTHER
                       MOVE "Y" TO WS-BAL-BAD
                   END-EVALUATE
               END-PERFORM

               IF WS-BAL-BAD = "N" AND WS-DIG-CNT > 0
                   COMPUTE WS-FILE-BAL = FUNCTION NUMVAL (FI-BAL)
                   MOVE "Y" TO WS-BAL-OK
               END-IF
           END-IF.

      *----------------------------------------------------------------
       P-ADD-CLAIM.
           MOVE "N" TO WS-CLAIM-HIT.
           PERFORM VARYING WS-CT-IX FROM 1 BY 1
             UNTIL WS-CT-IX > WS-CLAIM-CNT OR CLAIM-HIT
               IF WS-CLAIM-ENT(WS-CT-IX) = CC-CLAIM
                   MOVE "Y" TO WS-CLAIM-HIT
               END-IF
           END-PERFORM.

           IF NOT CLAIM-HIT
               IF WS-CLAIM-CNT < 200
                   ADD 1 TO WS-CLAIM-CNT
                   MOVE CC-CLAIM TO WS-CLAIM-ENT(WS-CLAIM-CNT)
               ELSE
                   MOVE "Y" TO WS-CLAIM-OVF
               END-IF
           END-IF.

      *----------------------------------------------------------------
       P-FIND-CLAIM.
           MOVE "N" TO WS-CLAIM-HIT.
           PERFORM VARYING WS-CT-IX FROM 1 BY 1
             UNTIL WS-CT-IX > WS-CLAIM-CNT OR CLAIM-HIT
               IF WS-CLAIM-ENT(WS-CT-IX) = PC-CLAIM
                   MOVE "Y" TO WS-CLAIM-HIT
               END-IF
           END-PERFORM.

      *----------------------------------------------------------------
       R90.
           MOVE SPACES TO TOTAL-LINE.
           MOVE "TOTALS"      TO TOTAL-LINE(1:25).
           MOVE GT-CHARGES    TO TL-CHARGES.
           MOVE GT-PAYMENTS   TO TL-PAYMENTS.
           MOVE GT-COMPUTED   TO TL-COMPUTED.
           MOVE GT-EXPECTED   TO TL-EXPECTED.
           MOVE TOTAL-LINE TO FILEOUT01.
           WRITE FILEOUT01.
           DISPLAY " ".
           DISPLAY TOTAL-LINE.

       R99.
           DISPLAY " ".
           DISPLAY "LINES READ:        " WS-READ-CNT.
           DISPLAY "BAD DOS SKIPPED:   " WS-SKIP-CNT.
           DISPLAY "BAD COL 503:       " WS-BADBAL-CNT.
           DISPLAY "NO 018 CHARGES:    " WS-NOCHG-CNT.
           DISPLAY "BALANCES MATCHED:  " WS-MATCH-CNT.
           DISPLAY "BALANCES DIFFERED: " WS-DIFF-CNT.
           DISPLAY "OPEN BALANCES:     " WS-OPEN-CNT.
           DISPLAY "CREDIT BALANCES:   " WS-CRED-CNT.
           DISPLAY "ZERO BALANCES:     " WS-ZERO-CNT.
           DISPLAY "CLAIM TAB OVERFL:  " WS-OVF-CNT.
           DISPLAY "UNAPPLIED TOTAL:   " GT-UNAPPLIED.
           CLOSE CHARCUR PAYCUR FILEIN FILEOUT.
           STOP RUN.