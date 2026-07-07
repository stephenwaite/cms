      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <stephen.waite@cmsvt.com>
      * @author  Claude
      * @copyright Copyright (c) 2026 cms
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. woperins01.
      *
      * Write off charges still pending insurance, driven by a FILEIN
      * list of CHARCUR keys (same drive as adj197f).  Each FILEIN
      * record carries an 11-byte charcur key (FI-KEY8 + FI-KEY3); the
      * matching CHARCUR record is read at random.  Selection of which
      * charges (e.g. aged 197s) is done upstream when the list is built.
      *
      *   - Skips keys that don't resolve to a CHARCUR record.
      *   - Skips charges that aren't pending insurance (CC-PAYCODE
      *     NOT = "197") as a guard on the fed list.
      *   - Sums PAYCUR activity for the same account+claim
      *     (PC-AMOUNT is signed-negative) to derive the balance:
      *         BALANCE = CC-AMOUNT + TOTALPAY
      *   - Posts a write-off to PAYFILE that zeroes the balance:
      *         WRITE-OFF = 0 - BALANCE      (always negative here)
      *     Only charges with BALANCE > 0 are written off; zero and
      *     credit balances are skipped.
      *   - Denial code "AA" (auto adjust); PD-ORDER carries the
      *     charge date (CC-DATE-T) as the reference.
      *
      * A report line is written for every posted write-off.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO "S55"
               ORGANIZATION IS LINE SEQUENTIAL.
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
       FD  FILEIN.
       01  FILEIN-REC.
           05  FI-KEY8                PIC X(8).
           05  FI-KEY3                PIC X(3).
           05  FILLER                 PIC X(69).
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
       77  WS-RUNDATE                 PIC X(8).
       77  XYZ                        PIC 9(3) VALUE 0.
      *
       77  CNT-IN                     PIC 9(7) VALUE 0.
       77  CNT-NOTFND                 PIC 9(7) VALUE 0.
       77  CNT-NOBAL                  PIC 9(7) VALUE 0.
       77  CNT-POSTED                 PIC 9(7) VALUE 0.
       77  CNT-DUP                    PIC 9(7) VALUE 0.
      *
      * ---- posting defaults (flagged in the reply; change here) ----
       01  WS-POST.
           05  P-BATCH                PIC X(6)  VALUE "WOINS ".
           05  P-DENIAL               PIC XX    VALUE "AA".
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
           OPEN INPUT  FILEIN CHARCUR PAYCUR GARFILE
           OPEN I-O    PAYFILE
           OPEN OUTPUT REPORTF.
           ACCEPT WS-RUNDATE FROM DATE YYYYMMDD.
           WRITE REPORT-REC FROM HDR-LINE.
           MOVE SPACES TO REPORT-REC.
           WRITE REPORT-REC.
       P00.
           READ FILEIN AT END GO TO P-DONE END-READ.
           ADD 1 TO CNT-IN.
           MOVE FI-KEY8 TO CC-KEY8.
           MOVE FI-KEY3 TO CC-KEY3.
           READ CHARCUR INVALID KEY
                DISPLAY "NO CHARGE: " FI-KEY8 " " FI-KEY3
                ADD 1 TO CNT-NOTFND
                GO TO P00
           END-READ.
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
           MOVE G-GARNAME  TO PD-NAME.
           MOVE WRITE-OFF  TO PD-AMOUNT.
           MOVE CC-PAYCODE TO PD-PAYCODE.
           MOVE P-DENIAL   TO PD-DENIAL.
           MOVE CC-CLAIM   TO PD-CLAIM.
           MOVE CC-DATE-T  TO PD-DATE-T.
           MOVE WS-RUNDATE TO PD-DATE-E.
           MOVE SPACES     TO PD-ORDER.
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
           DISPLAY "KEYS READ:           " CNT-IN.
           DISPLAY "CHARGE NOT FOUND:    " CNT-NOTFND.
           DISPLAY "NO BALANCE (SKIP):   " CNT-NOBAL.
           DISPLAY "WRITE-OFFS POSTED:   " CNT-POSTED.
           DISPLAY "DUP PAY KEY (SKIP):  " CNT-DUP.
           CLOSE FILEIN GARFILE CHARCUR PAYCUR PAYFILE REPORTF.
           STOP RUN.
