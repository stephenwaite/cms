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
      *   - The paycode is taken from the charge itself
      *     (PD-PAYCODE = CC-PAYCODE); no paycode filter is applied,
      *     so the fed list decides what gets written off.
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
               FILE STATUS IS PF-STAT
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
       77  CNT-NOSLOT                 PIC 9(7) VALUE 0.
      *
      * ---- debug scaffolding ----
       77  PF-STAT                    PIC XX    VALUE "00".
       77  DBG-SW                     PIC X     VALUE "N".
           88  DEBUG-ON                         VALUE "Y".
       77  CNT-DBG                    PIC 9(3)  VALUE 0.
       77  DBG-MAX                    PIC 9(3)  VALUE 5.
       77  MAX-SLOT                   PIC 9(3)  VALUE 999.
       77  POST-OK-SW                 PIC X     VALUE "N".
           88  POST-OK                          VALUE "Y".
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
      * debug is driven by the environment, not a prompt - an ACCEPT
      * into a data field pulls in the screen manager, which garbles
      * DISPLAY output and hangs waiting for a field terminator.
           ACCEPT DBG-SW FROM ENVIRONMENT "WOPERINS_DEBUG".
           IF DBG-SW = "y" MOVE "Y" TO DBG-SW.
           IF DEBUG-ON
              DISPLAY "DEBUG ON - first " DBG-MAX
                      " writes detailed" UPON SYSERR.
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
                DISPLAY "NO CHARGE: [" FI-KEY8 "][" FI-KEY3 "]"
                        UPON SYSERR
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
           MOVE "N" TO POST-OK-SW.
           PERFORM POST-WO THRU POST-EXIT.
           IF POST-OK
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
      * clear the record area first - a failed READ in P3 leaves a
      * stale PAYFILE01 behind, and the MOVEs below don't cover
      * every field in the copybook.  INITIALIZE (not MOVE SPACES):
      * spaces in a COMP-3 field is invalid packed data.
           INITIALIZE PAYFILE01.
           MOVE CC-KEY8 TO PD-KEY8.
           MOVE ZERO    TO XYZ.
      * find the first free PD-KEY3 slot for this account
       P3.
           IF XYZ NOT < MAX-SLOT
              DISPLAY "NO FREE SLOT: " CC-KEY8 " (KEY3 hit " MAX-SLOT
                      ") - SKIPPED" UPON SYSERR
              ADD 1 TO CNT-NOSLOT
              GO TO POST-EXIT.
           ADD 1 TO XYZ.
           MOVE XYZ TO PD-KEY3.
           READ PAYFILE
               INVALID KEY
                   GO TO P4
           END-READ.
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
      *
           IF DEBUG-ON PERFORM DBG-BEFORE THRU DBG-BEFORE-X.
      *
           MOVE "00" TO PF-STAT.
           WRITE PAYFILE01
               INVALID KEY
                   DISPLAY "WRITE FAIL stat=[" PF-STAT "] key=["
                           PD-KEY8 "/" PD-KEY3 "] amt=[" PD-AMOUNT "]"
                           UPON SYSERR
                   ADD 1 TO CNT-DUP
               NOT INVALID KEY
                   MOVE "Y"      TO POST-OK-SW
                   ADD 1         TO CNT-POSTED
                   ADD WRITE-OFF TO TOT-WO
           END-WRITE.
           IF DEBUG-ON PERFORM DBG-AFTER THRU DBG-AFTER-X.
       POST-EXIT.
           EXIT.
      *
       DBG-BEFORE.
           IF CNT-DBG NOT < DBG-MAX GO TO DBG-BEFORE-X.
           ADD 1 TO CNT-DBG.
           DISPLAY " " UPON SYSERR.
           DISPLAY "--- PAYFILE record to write (" CNT-DBG " of "
                   DBG-MAX ") ---" UPON SYSERR.
           DISPLAY "  CHARCUR key  [" CC-KEY8 "/" CC-KEY3 "]"
                   UPON SYSERR.
           DISPLAY "  slot XYZ     [" XYZ "]" UPON SYSERR.
           DISPLAY "  PD-KEY8      [" PD-KEY8 "]" UPON SYSERR.
           DISPLAY "  PD-KEY3      [" PD-KEY3 "]" UPON SYSERR.
           DISPLAY "  PD-NAME      [" PD-NAME "]" UPON SYSERR.
           DISPLAY "  PD-AMOUNT    [" PD-AMOUNT "]" UPON SYSERR.
           DISPLAY "  PD-PAYCODE   [" PD-PAYCODE "]" UPON SYSERR.
           DISPLAY "  PD-DENIAL    [" PD-DENIAL "]" UPON SYSERR.
           DISPLAY "  PD-CLAIM     [" PD-CLAIM "]" UPON SYSERR.
           DISPLAY "  PD-DATE-T    [" PD-DATE-T "] chg date"
                   UPON SYSERR.
           DISPLAY "  PD-DATE-E    [" PD-DATE-E "] run date"
                   UPON SYSERR.
           DISPLAY "  PD-ORDER     [" PD-ORDER "]" UPON SYSERR.
           DISPLAY "  PD-BATCH     [" PD-BATCH "]" UPON SYSERR.
           DISPLAY "  CHARGE " CC-AMOUNT "  PAID " TOTALPAY
                   "  BAL " BALANCE "  WO " WRITE-OFF UPON SYSERR.
       DBG-BEFORE-X.
           EXIT.
      *
       DBG-AFTER.
           IF CNT-DBG NOT < DBG-MAX GO TO DBG-AFTER-X.
           DISPLAY "  WRITE stat=[" PF-STAT "]  posted=" CNT-POSTED
                   "  failed=" CNT-DUP UPON SYSERR.
       DBG-AFTER-X.
           EXIT.
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
           DISPLAY "KEYS READ:           " CNT-IN UPON SYSERR.
           DISPLAY "CHARGE NOT FOUND:    " CNT-NOTFND UPON SYSERR.
           DISPLAY "NO BALANCE (SKIP):   " CNT-NOBAL UPON SYSERR.
           DISPLAY "WRITE-OFFS POSTED:   " CNT-POSTED UPON SYSERR.
           DISPLAY "WRITE FAILED (SKIP): " CNT-DUP UPON SYSERR.
           DISPLAY "NO FREE SLOT (SKIP): " CNT-NOSLOT UPON SYSERR.
           CLOSE FILEIN GARFILE CHARCUR PAYCUR PAYFILE REPORTF.
           STOP RUN.
