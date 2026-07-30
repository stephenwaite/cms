      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <stephen.waite@cmsvt.com>
      * @copyright Copyright (c) 2026 cms
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. adj197f.
      *
      * Same report as adj197, but driven by a FILEIN list of CHARCUR
      * keys instead of scanning the CC-PAYCODE alternate key.  Each
      * FILEIN record carries an 11-byte charcur key (KEY8 + KEY3);
      * the matching CHARCUR record is read at random.
      *
      * For each charge with CC-PAYCODE = "197" (pending ins) it walks
      * PAYCUR for the same account+claim and splits the (signed-
      * negative) activity into two buckets:
      *     CASHPAID - everything except PC-DENIAL = "14"
      *     ADJ14    - the PC-DENIAL = "14" insurance adjustments
      * Charges with a "003" Medicare payment on the claim are skipped.
      * Looks up the Medicare allowed amount from MEDFILE2020 keyed by
      * CC-PROC1.  DUE is the allowed plus the cash paid (CASHPAID is
      * signed-negative); the "14" adjustments are shown in INS-ADJ but
      * are NOT netted into DUE:
      *     DUE-AMT = MED-AMT + CASHPAID  (CASHPAID = TOTALPAY - ADJ14)
      *
      * A write-off adjustment is posted to PAYFILE for each reported
      * charge so that the resulting balance lands on DUE:
      *     WRITE-OFF = DUE-AMT - BALANCE   (posted only when < 0)
      * The write-off is capped at the open balance, so a charge is
      * never driven into a credit balance (if DUE is negative the
      * charge is written down to zero, no further).
      * Positive/zero write-off means nothing to post.
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
           SELECT MEDFILE2020 ASSIGN TO "S45" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS MED-KEY
               LOCK MODE MANUAL.
           SELECT REPORTF ASSIGN TO "S60"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PAYFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC RECORD KEY IS PAYFILE-KEY
               FILE STATUS IS WS-PF-FS
               LOCK MODE MANUAL.
       DATA DIVISION.
       FILE SECTION.
       FD  FILEIN.
       01  FILEIN01.
           05  FI-KEY8                 PIC X(8).
           05  FI-KEY3                 PIC X(3).
       FD  GARFILE.
           COPY GARFILE.CPY.
       FD  CHARCUR.
           COPY CHARCUR.CPY.
       FD  PAYCUR.
           COPY PAYCUR.CPY.
       FD  MEDFILE2020.
       01  MEDFILE202001.
           02  MED-KEY.
               03  MED-KEY1            PIC X(5).
               03  MED-KEY2            PIC XX.
           02  MED-AMT                 PIC 9(4)V99.
       FD  REPORTF.
       01  REPORT-REC                  PIC X(140).
       FD  PAYFILE.
       01  PAYFILE01.
           02  PAYFILE-KEY.
               03  PD-KEY8             PIC X(8).
               03  PD-KEY3             PIC XXX.
           02  PD-NAME                 PIC X(24).
           02  PD-AMOUNT               PIC S9(4)V99.
           02  PD-PAYCODE              PIC XXX.
           02  PD-DENIAL               PIC XX.
           02  PD-CLAIM                PIC X(6).
           02  PD-DATE-T               PIC X(8).
           02  PD-DATE-E               PIC X(8).
           02  PD-ORDER                PIC X(6).
           02  PD-BATCH                PIC X(6).
       WORKING-STORAGE SECTION.
       01  WS-WORK.
           05  TOTALPAY                PIC S9(7)V99 VALUE 0.
           05  CASHPAID                PIC S9(7)V99 VALUE 0.
           05  ADJ14                   PIC S9(7)V99 VALUE 0.
           05  BALANCE                 PIC S9(7)V99 VALUE 0.
           05  DUE-AMT                 PIC S9(7)V99 VALUE 0.
           05  WRITE-OFF               PIC S9(7)V99 VALUE 0.
           05  TOT-ADJ14               PIC S9(9)V99 VALUE 0.
           05  TOT-DUE                 PIC S9(9)V99 VALUE 0.
           05  TOT-POST                PIC S9(9)V99 VALUE 0.
           05  WS-NAME                 PIC X(25)    VALUE SPACES.
           05  MCR-PAID                PIC X        VALUE "N".
           05  WS-PF-FS                PIC XX       VALUE "00".
       01  WS-POST.
           05  WS-CURDATE              PIC X(21)    VALUE SPACES.
           05  WS-RUNDATE              PIC X(8)     VALUE SPACES.
           05  P-PAYCODE               PIC XXX      VALUE "197".
           05  P-DENIAL                PIC XX       VALUE "14".
           05  P-BATCH                 PIC X(6)     VALUE "ADJ197".
       01  WS-COUNTS.
           05  CNT-IN                  PIC 9(7) VALUE 0.
           05  CNT-NOTFND              PIC 9(7) VALUE 0.
           05  CNT-NOT197              PIC 9(7) VALUE 0.
           05  CNT-197                 PIC 9(7) VALUE 0.
           05  CNT-RPT                 PIC 9(7) VALUE 0.
           05  CNT-NOBAL               PIC 9(7) VALUE 0.
           05  CNT-NOFEE               PIC 9(7) VALUE 0.
           05  CNT-MCRPAID             PIC 9(7) VALUE 0.
           05  CNT-POSTED              PIC 9(7) VALUE 0.
           05  CNT-DUP                 PIC 9(7) VALUE 0.
       01  HDR-1.
           05  FILLER  PIC X(45) VALUE
               "PENDING INS 197 - ADJUSTMENT TO MED ALLOWED".
       01  HDR-2.
           05  FILLER  PIC X(2)  VALUE SPACES.
           05  FILLER  PIC X(10) VALUE "ACCOUNT".
           05  FILLER  PIC X(8)  VALUE "CLAIM".
           05  FILLER  PIC X(10) VALUE "DOS".
           05  FILLER  PIC X(9)  VALUE "PROC".
           05  FILLER  PIC X(11) VALUE "   CHARGE".
           05  FILLER  PIC X(12) VALUE "      PAID".
           05  FILLER  PIC X(12) VALUE "   INS-ADJ".
           05  FILLER  PIC X(12) VALUE "   BALANCE".
           05  FILLER  PIC X(11) VALUE "  ALLOWED".
           05  FILLER  PIC X(12) VALUE "      DUE".
           05  FILLER  PIC X(6)  VALUE "NAME".
       01  DET-LINE.
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-ACCT                 PIC X(8).
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-CLAIM                PIC X(6).
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-DOS                  PIC X(8).
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-PROC                 PIC X(7).
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-CHARGE               PIC ZZ,ZZ9.99.
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-PAID                 PIC -ZZ,ZZ9.99.
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-ADJ14                PIC -ZZ,ZZ9.99.
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-BAL                  PIC -ZZ,ZZ9.99.
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-ALLOW                PIC ZZ,ZZ9.99.
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-DUE                  PIC -ZZ,ZZ9.99.
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-NAME                 PIC X(25).
       01  TOT-LINE.
           05  FILLER     PIC X(20) VALUE "TOTAL INS-ADJ/DUE:  ".
           05  TL-ADJ14   PIC -Z,ZZZ,ZZ9.99.
           05  FILLER     PIC X(3)  VALUE SPACES.
           05  TL-DUE     PIC -Z,ZZZ,ZZ9.99.
       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT FILEIN GARFILE CHARCUR PAYCUR MEDFILE2020.
           OPEN OUTPUT REPORTF PAYFILE.
           DISPLAY "PF OPEN FS=" WS-PF-FS UPON SYSERR.
           IF WS-PF-FS NOT = "00"
               DISPLAY "PAYFILE OPEN FAILED - ABORTING" UPON SYSERR
               STOP RUN.
           MOVE FUNCTION CURRENT-DATE TO WS-CURDATE.
           MOVE WS-CURDATE (1:8) TO WS-RUNDATE.
           WRITE REPORT-REC FROM HDR-1.
           MOVE SPACES TO REPORT-REC.
           WRITE REPORT-REC.
           WRITE REPORT-REC FROM HDR-2.
       NEXT-KEY.
           READ FILEIN AT END GO TO DONE END-READ.
           ADD 1 TO CNT-IN.
           MOVE FI-KEY8 TO CC-KEY8.
           MOVE FI-KEY3 TO CC-KEY3.
           READ CHARCUR
               INVALID KEY
                   ADD 1 TO CNT-NOTFND
                   GO TO NEXT-KEY
           END-READ.
           IF CC-PAYCODE NOT = "197"
               ADD 1 TO CNT-NOT197
               GO TO NEXT-KEY.
           ADD 1 TO CNT-197.
      *
      *    sum payments for this account + claim
      *
           MOVE 0          TO TOTALPAY.
           MOVE 0          TO ADJ14.
           MOVE "N"        TO MCR-PAID.
           MOVE CC-KEY8    TO PC-KEY8.
           MOVE LOW-VALUES TO PC-KEY3.
           START PAYCUR KEY NOT < PAYCUR-KEY
               INVALID KEY GO TO EVAL
           END-START.
       PAY-LOOP.
           READ PAYCUR NEXT AT END GO TO EVAL END-READ.
           IF PC-KEY8  NOT = CC-KEY8  GO TO EVAL.
           IF PC-CLAIM NOT = CC-CLAIM GO TO PAY-LOOP.
           ADD PC-AMOUNT TO TOTALPAY.
           IF PC-DENIAL = "14"
               ADD PC-AMOUNT TO ADJ14.
           IF PC-PAYCODE = "003"
               MOVE "Y" TO MCR-PAID.
           GO TO PAY-LOOP.
       EVAL.
           IF MCR-PAID = "Y"
               ADD 1 TO CNT-MCRPAID
               GO TO NEXT-KEY.
           COMPUTE BALANCE  = CC-AMOUNT + TOTALPAY.
           COMPUTE CASHPAID = TOTALPAY - ADJ14.
           IF BALANCE NOT > 0
               ADD 1 TO CNT-NOBAL
               GO TO NEXT-KEY.
      *
      *    look up Medicare allowed by CC-PROC1 (cpt + modifier)
      *
           MOVE CC-PROC1 TO MED-KEY.
           READ MEDFILE2020
               INVALID KEY
                   ADD 1 TO CNT-NOFEE
                   PERFORM GET-NAME
                   PERFORM WRITE-NOFEE
                   GO TO NEXT-KEY
           END-READ.
           COMPUTE DUE-AMT = MED-AMT + CASHPAID.
      *
      *    cap the write-off at the open balance so we never drive
      *    a charge into a credit balance
      *
           IF DUE-AMT < 0
               COMPUTE WRITE-OFF = 0 - BALANCE
               MOVE 0 TO DUE-AMT
           ELSE
               COMPUTE WRITE-OFF = DUE-AMT - BALANCE
           END-IF.
           PERFORM GET-NAME.
           IF WRITE-OFF < 0
               PERFORM WRITE-PAY.
           ADD 1       TO CNT-RPT.
           ADD ADJ14   TO TOT-ADJ14.
           ADD DUE-AMT TO TOT-DUE.
           PERFORM WRITE-DETAIL.
           GO TO NEXT-KEY.
       GET-NAME.
           MOVE SPACES  TO WS-NAME.
           MOVE CC-KEY8 TO G-GARNO.
           READ GARFILE
               INVALID KEY     MOVE SPACES    TO WS-NAME
               NOT INVALID KEY MOVE G-GARNAME TO WS-NAME
           END-READ.
       WRITE-DETAIL.
           MOVE CC-KEY8   TO DL-ACCT.
           MOVE CC-CLAIM  TO DL-CLAIM.
           MOVE CC-DATE-T TO DL-DOS.
           MOVE CC-PROC1  TO DL-PROC.
           MOVE CC-AMOUNT TO DL-CHARGE.
           MOVE CASHPAID  TO DL-PAID.
           MOVE ADJ14     TO DL-ADJ14.
           MOVE BALANCE   TO DL-BAL.
           MOVE MED-AMT   TO DL-ALLOW.
           MOVE DUE-AMT   TO DL-DUE.
           MOVE WS-NAME   TO DL-NAME.
           WRITE REPORT-REC FROM DET-LINE.
       WRITE-NOFEE.
           MOVE CC-KEY8   TO DL-ACCT.
           MOVE CC-CLAIM  TO DL-CLAIM.
           MOVE CC-DATE-T TO DL-DOS.
           MOVE CC-PROC1  TO DL-PROC.
           MOVE CC-AMOUNT TO DL-CHARGE.
           MOVE CASHPAID  TO DL-PAID.
           MOVE ADJ14     TO DL-ADJ14.
           MOVE BALANCE   TO DL-BAL.
           MOVE 0         TO DL-ALLOW.
           MOVE 0         TO DL-DUE.
           MOVE "*** NO FEE SCHEDULE ENTRY" TO DL-NAME.
           WRITE REPORT-REC FROM DET-LINE.
       WRITE-PAY.
           MOVE SPACES     TO PAYFILE01.
           MOVE CC-KEY8    TO PD-KEY8.
           MOVE CC-KEY3    TO PD-KEY3.
           MOVE WS-NAME    TO PD-NAME.
           MOVE WRITE-OFF  TO PD-AMOUNT.
           MOVE P-PAYCODE  TO PD-PAYCODE.
           MOVE P-DENIAL   TO PD-DENIAL.
           MOVE CC-CLAIM   TO PD-CLAIM.
           MOVE WS-RUNDATE TO PD-DATE-T.
           MOVE WS-RUNDATE TO PD-DATE-E.
           MOVE SPACES     TO PD-ORDER.
           MOVE P-BATCH    TO PD-BATCH.
           WRITE PAYFILE01
               INVALID KEY
                   ADD 1 TO CNT-DUP
               NOT INVALID KEY
                   ADD 1         TO CNT-POSTED
                   ADD WRITE-OFF TO TOT-POST
           END-WRITE.
           IF WS-PF-FS NOT = "00" AND WS-PF-FS NOT = "02"
               DISPLAY "PF WRITE FS=" WS-PF-FS " KEY=" PD-KEY8 PD-KEY3
                   UPON SYSERR.
       DONE.
           MOVE SPACES TO REPORT-REC.
           WRITE REPORT-REC.
           MOVE TOT-ADJ14 TO TL-ADJ14.
           MOVE TOT-DUE   TO TL-DUE.
           WRITE REPORT-REC FROM TOT-LINE.
           DISPLAY "KEYS READ:           " CNT-IN.
           DISPLAY "CHARGE NOT FOUND:    " CNT-NOTFND.
           DISPLAY "NOT PAYCODE 197:     " CNT-NOT197.
           DISPLAY "197 CHARGES:         " CNT-197.
           DISPLAY "REPORTED (ADJ):      " CNT-RPT.
           DISPLAY "NO BALANCE DUE:      " CNT-NOBAL.
           DISPLAY "NO FEE SCHED ENTRY:  " CNT-NOFEE.
           DISPLAY "MEDICARE PAID (SKIP):" CNT-MCRPAID.
           DISPLAY "PAYMENTS POSTED:     " CNT-POSTED.
           DISPLAY "DUP PAY KEY (SKIP):  " CNT-DUP.
           CLOSE FILEIN GARFILE CHARCUR PAYCUR MEDFILE2020 REPORTF
                 PAYFILE.
           STOP RUN.
