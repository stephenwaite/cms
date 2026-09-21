      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <stephen.waite@cmsvt.com>
      * @author  Claude
      * @copyright Copyright (c) 2026 cms
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. woins01.
      *
      * Write off OLD charges still pending insurance.
      *
      * Structurally a cousin of adj197f, but instead of aligning the
      * balance to a MEDFILE2020 allowed amount it writes the whole
      * remaining balance off to zero.  Selection is age-driven rather
      * than FILEIN-driven:
      *
      *   - Scans CHARCUR on the CC-PAYCODE alternate key for the
      *     pending-insurance bucket (CC-PAYCODE = "197").
      *   - Keeps only charges dated on or before a runtime CUTOFF
      *     (CC-DATE-T <= CUTOFF), i.e. the "old" ones.
      *   - Sums PAYCUR activity for the same account+claim
      *     (PC-AMOUNT is signed-negative) to derive the balance:
      *         BALANCE = CC-AMOUNT + TOTALPAY
      *   - Posts a write-off to PAYFILE that zeroes the balance:
      *         WRITE-OFF = 0 - BALANCE      (always negative here)
      *     Only charges with BALANCE > 0 are written off; zero and
      *     credit balances are skipped.
      *
      * A report line is written for every posted write-off so the run
      * can be reviewed in the sandbox before the PAYFILE is imported.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
               ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
               LOCK MODE MANUAL.
           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
               ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
               LOCK MODE MANUAL.
           SELECT PAYCUR ASSIGN TO "S40" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
               LOCK MODE MANUAL.
           SELECT PAYFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC RECORD KEY IS PAYFILE-KEY
               LOCK MODE MANUAL.
           SELECT REPORTF ASSIGN TO "S60"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  GARFILE.
           COPY GARFILE.CPY.
       FD  CHARCUR.
           COPY CHARCUR.CPY.
       FD  PAYCUR.
           COPY PAYCUR.CPY.
       FD  PAYFILE.
           COPY PAYFILE.CPY.
       FD  REPORTF.
       01  REPORT-REC                 PIC X(132).
       WORKING-STORAGE SECTION.
       77  TOTALPAY                   PIC S9(7)V99 COMP-3 VALUE ZERO.
       77  BALANCE                    PIC S9(7)V99 COMP-3 VALUE ZERO.
       77  WRITE-OFF                  PIC S9(7)V99 COMP-3 VALUE ZERO.
       77  TOT-WO                     PIC S9(9)V99 COMP-3 VALUE ZERO.
       77  CUTOFF                     PIC X(8).
       77  WS-RUNDATE                 PIC X(8).
       77  XYZ                        PIC 9(3) VALUE 0.
      *
       77  CNT-197                    PIC 9(7) VALUE 0.
       77  CNT-NEW                    PIC 9(7) VALUE 0.
       77  CNT-NOBAL                  PIC 9(7) VALUE 0.
       77  CNT-POSTED                 PIC 9(7) VALUE 0.
       77  CNT-DUP                    PIC 9(7) VALUE 0.
      *
       01  TIME-NOW.
           05  TN-HHMMSS              PIC X(6).
           05  FILLER                 PIC X(2).
      *
      * ---- posting defaults (flagged in the reply; change here) ----
       01  WS-POST.
           05  P-BATCH                PIC X(6)  VALUE "WOINS ".
           05  P-DENIAL               PIC XX    VALUE SPACES.
      *
       01  HDR-LINE.
           05  FILLER   PIC X(10) VALUE "ACCOUNT   ".
           05  FILLER   PIC X(17) VALUE "CLAIM            ".
           05  FILLER   PIC X(10) VALUE "DOS       ".
           05  FILLER   PIC X(13) VALUE "PROC         ".
           05  FILLER   PIC X(15) VALUE "        CHARGE ".
           05  FILLER   PIC X(15) VALUE "          PAID ".
           05  FILLER   PIC X(15) VALUE "       BALANCE ".
           05  FILLER   PIC X(15) VALUE "     WRITE-OFF ".
           05  FILLER   PIC X(22) VALUE "NAME".
      *
       01  DETAIL-LINE.
           05  DL-ACCT     PIC X(8).
           05  FILLER      PIC XX    VALUE SPACES.
           05  DL-CLAIM    PIC X(15).
           05  FILLER      PIC XX    VALUE SPACES.
           05  DL-DOS      PIC X(8).
           05  FILLER      PIC XX    VALUE SPACES.
           05  DL-PROC     PIC X(11).
           05  FILLER      PIC XX    VALUE SPACES.
           05  DL-CHARGE   PIC Z,ZZZ,ZZ9.99-.
           05  FILLER      PIC XX    VALUE SPACES.
           05  DL-PAID     PIC Z,ZZZ,ZZ9.99-.
           05  FILLER      PIC XX    VALUE SPACES.
           05  DL-BAL      PIC Z,ZZZ,ZZ9.99-.
           05  FILLER      PIC XX    VALUE SPACES.
           05  DL-WO       PIC Z,ZZZ,ZZ9.99-.
           05  FILLER      PIC XX    VALUE SPACES.
           05  DL-NAME     PIC X(22).
      *
       01  TOT-LINE.
           05  FILLER      PIC X(60) VALUE
               "TOTAL WRITTEN OFF".
           05  TL-WO       PIC Z,ZZZ,ZZZ,ZZ9.99-.
      *
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "Write off 197 charges dated on or before "
                   "(YYYYMMDD): " WITH NO ADVANCING.
           ACCEPT CUTOFF.
           IF CUTOFF = SPACES
              DISPLAY "Cutoff date required."
              STOP RUN 1.
           OPEN INPUT  CHARCUR PAYCUR GARFILE
           OPEN I-O    PAYFILE
           OPEN OUTPUT REPORTF.
           ACCEPT WS-RUNDATE FROM DATE YYYYMMDD.
           WRITE REPORT-REC FROM HDR-LINE.
           MOVE SPACES TO REPORT-REC.
           WRITE REPORT-REC.
      *
           MOVE "197" TO CC-PAYCODE.
           START CHARCUR KEY >= CC-PAYCODE
                INVALID KEY GO TO P-DONE
           END-START.
       P00.
           READ CHARCUR NEXT AT END GO TO P-DONE END-READ.
           IF CC-PAYCODE NOT = "197" GO TO P-DONE.
           ADD 1 TO CNT-197.
           IF CC-DATE-T > CUTOFF
              ADD 1 TO CNT-NEW
              GO TO P00.
      *
           MOVE ZERO       TO TOTALPAY
           MOVE CC-KEY8    TO PC-KEY8
           MOVE LOW-VALUES TO PC-KEY3.
           START PAYCUR KEY >= PAYCUR-KEY
                INVALID KEY GO TO P-EVAL
           END-START.
       P1.
           READ PAYCUR NEXT AT END GO TO P-EVAL END-READ.
           IF PC-KEY8  NOT = CC-KEY8  GO TO P-EVAL.
           IF PC-CLAIM NOT = CC-CLAIM GO TO P1.
           ADD PC-AMOUNT TO TOTALPAY.
           GO TO P1.
       P-EVAL.
           COMPUTE BALANCE = CC-AMOUNT + TOTALPAY.
           IF BALANCE NOT > 0
              ADD 1 TO CNT-NOBAL
              GO TO P00.
           COMPUTE WRITE-OFF = 0 - BALANCE.
           PERFORM GET-NAME.
           PERFORM POST-WO.
           PERFORM WRITE-DETAIL.
           GO TO P00.
      *
       GET-NAME.
           MOVE CC-KEY8 TO G-GARNO.
           READ GARFILE INVALID KEY
                MOVE SPACES TO G-GARNAME
           END-READ.
      *
       POST-WO.
           MOVE CC-KEY8 TO PD-KEY8.
           MOVE ZERO    TO XYZ.
       P3.
           ADD 1 TO XYZ.
           MOVE XYZ TO PD-KEY3.
           READ PAYFILE INVALID KEY GO TO P4.
           GO TO P3.
       P4.
           ACCEPT TIME-NOW FROM TIME.
           MOVE G-GARNAME  TO PD-NAME.
           MOVE WRITE-OFF  TO PD-AMOUNT.
           MOVE CC-PAYCODE TO PD-PAYCODE.
           MOVE P-DENIAL   TO PD-DENIAL.
           MOVE CC-CLAIM   TO PD-CLAIM.
           MOVE WS-RUNDATE TO PD-DATE-T.
           MOVE WS-RUNDATE TO PD-DATE-E.
           MOVE TN-HHMMSS  TO PD-ORDER.
           MOVE P-BATCH    TO PD-BATCH.
           WRITE PAYFILE01
               INVALID KEY
                   ADD 1 TO CNT-DUP
               NOT INVALID KEY
                   ADD 1         TO CNT-POSTED
                   ADD WRITE-OFF TO TOT-WO
           END-WRITE.
      *
       WRITE-DETAIL.
           MOVE CC-KEY8    TO DL-ACCT.
           MOVE CC-CLAIM   TO DL-CLAIM.
           MOVE CC-DATE-T  TO DL-DOS.
           MOVE CC-PROC1   TO DL-PROC.
           MOVE CC-AMOUNT  TO DL-CHARGE.
           MOVE TOTALPAY   TO DL-PAID.
           MOVE BALANCE    TO DL-BAL.
           MOVE WRITE-OFF  TO DL-WO.
           MOVE G-GARNAME  TO DL-NAME.
           WRITE REPORT-REC FROM DETAIL-LINE.
      *
       P-DONE.
           MOVE SPACES TO REPORT-REC.
           WRITE REPORT-REC.
           MOVE TOT-WO TO TL-WO.
           WRITE REPORT-REC FROM TOT-LINE.
           DISPLAY "197 CHARGES SCANNED: " CNT-197.
           DISPLAY "NEWER THAN CUTOFF:   " CNT-NEW.
           DISPLAY "NO BALANCE (SKIP):   " CNT-NOBAL.
           DISPLAY "WRITE-OFFS POSTED:   " CNT-POSTED.
           DISPLAY "DUP PAY KEY (SKIP):  " CNT-DUP.
           CLOSE GARFILE CHARCUR PAYCUR PAYFILE REPORTF.
           STOP RUN.
