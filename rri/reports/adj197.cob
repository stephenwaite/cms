      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <stephen.waite@cmsvt.com>
      * @copyright Copyright (c) 2026 cms
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. adj197.
      *
      * Reads CHARCUR charges with CC-PAYCODE = "197" (pending ins).
      * For each, sums PAYCUR payments for the same account+claim
      * (payments stored signed-negative), derives the balance due
      * (CC-AMOUNT + TOTALPAY), looks up the Medicare allowed amount
      * from MEDFILE2020 keyed by CC-PROC1, and reports the remaining
      * amount: allowed less what has already been paid.  Because
      * payments are stored signed-negative:
      *     ADJ-AMT = MED-AMT - (-TOTALPAY) = MED-AMT + TOTALPAY
      * Positive = still owed up to the allowed amount; negative = paid
      * past the allowed (nothing to collect / possible refund).
      * Report only.  Posts nothing.
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
           SELECT MEDFILE2020 ASSIGN TO "S45" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS MED-KEY
               LOCK MODE MANUAL.
           SELECT REPORTF ASSIGN TO "S50"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
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
       01  REPORT-REC                  PIC X(132).
       WORKING-STORAGE SECTION.
       01  WS-WORK.
           05  TOTALPAY                PIC S9(7)V99 VALUE 0.
           05  BALANCE                 PIC S9(7)V99 VALUE 0.
           05  ADJ-AMT                 PIC S9(7)V99 VALUE 0.
           05  TOT-ADJ                 PIC S9(9)V99 VALUE 0.
           05  WS-NAME                 PIC X(25)    VALUE SPACES.
       01  WS-COUNTS.
           05  CNT-197                 PIC 9(7) VALUE 0.
           05  CNT-RPT                 PIC 9(7) VALUE 0.
           05  CNT-NOBAL               PIC 9(7) VALUE 0.
           05  CNT-NOFEE               PIC 9(7) VALUE 0.
       01  HDR-1.
           05  FILLER  PIC X(45) VALUE
               "PENDING INS 197 - ADJUSTMENT TO MED ALLOWED".
       01  HDR-2.
           05  FILLER  PIC X(2)  VALUE SPACES.
           05  FILLER  PIC X(10) VALUE "ACCOUNT".
           05  FILLER  PIC X(8)  VALUE "CLAIM".
           05  FILLER  PIC X(9)  VALUE "PROC".
           05  FILLER  PIC X(11) VALUE "   CHARGE".
           05  FILLER  PIC X(12) VALUE "      PAID".
           05  FILLER  PIC X(12) VALUE "   BALANCE".
           05  FILLER  PIC X(11) VALUE "  ALLOWED".
           05  FILLER  PIC X(12) VALUE "      ADJ".
           05  FILLER  PIC X(6)  VALUE "NAME".
       01  DET-LINE.
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-ACCT                 PIC X(8).
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-CLAIM                PIC X(6).
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-PROC                 PIC X(7).
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-CHARGE               PIC ZZ,ZZ9.99.
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-PAID                 PIC -ZZ,ZZ9.99.
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-BAL                  PIC -ZZ,ZZ9.99.
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-ALLOW                PIC ZZ,ZZ9.99.
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-ADJ                  PIC -ZZ,ZZ9.99.
           05  FILLER                  PIC X(2)  VALUE SPACES.
           05  DL-NAME                 PIC X(25).
       01  TOT-LINE.
           05  FILLER     PIC X(20) VALUE "TOTAL ADJUSTMENT:   ".
           05  TL-ADJ     PIC -Z,ZZZ,ZZ9.99.
       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT GARFILE CHARCUR PAYCUR MEDFILE2020.
           OPEN OUTPUT REPORTF.
           WRITE REPORT-REC FROM HDR-1.
           MOVE SPACES TO REPORT-REC.
           WRITE REPORT-REC.
           WRITE REPORT-REC FROM HDR-2.
           MOVE "197" TO CC-PAYCODE.
           START CHARCUR KEY NOT < CC-PAYCODE
               INVALID KEY
                   DISPLAY "NO 197 CHARGES FOUND"
                   GO TO DONE
           END-START.
       NEXT-CHG.
           READ CHARCUR NEXT AT END GO TO DONE END-READ.
           IF CC-PAYCODE NOT = "197" GO TO DONE.
           ADD 1 TO CNT-197.
      *
      *    sum payments for this account + claim
      *
           MOVE 0          TO TOTALPAY.
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
           GO TO PAY-LOOP.
       EVAL.
           COMPUTE BALANCE = CC-AMOUNT + TOTALPAY.
           IF BALANCE NOT > 0
               ADD 1 TO CNT-NOBAL
               GO TO NEXT-CHG.
      *
      *    look up Medicare allowed by CC-PROC1 (cpt + modifier)
      *
           MOVE CC-PROC1 TO MED-KEY.
           READ MEDFILE2020
               INVALID KEY
                   ADD 1 TO CNT-NOFEE
                   PERFORM WRITE-NOFEE
                   GO TO NEXT-CHG
           END-READ.
           COMPUTE ADJ-AMT = MED-AMT - (0 - TOTALPAY).
           ADD 1 TO CNT-RPT.
           ADD ADJ-AMT TO TOT-ADJ.
           PERFORM WRITE-DETAIL.
           GO TO NEXT-CHG.
       WRITE-DETAIL.
           MOVE SPACES  TO WS-NAME.
           MOVE CC-KEY8 TO G-GARNO.
           READ GARFILE
               INVALID KEY     MOVE SPACES    TO WS-NAME
               NOT INVALID KEY MOVE G-GARNAME TO WS-NAME
           END-READ.
           MOVE CC-KEY8   TO DL-ACCT.
           MOVE CC-CLAIM  TO DL-CLAIM.
           MOVE CC-PROC1  TO DL-PROC.
           MOVE CC-AMOUNT TO DL-CHARGE.
           MOVE TOTALPAY  TO DL-PAID.
           MOVE BALANCE   TO DL-BAL.
           MOVE MED-AMT   TO DL-ALLOW.
           MOVE ADJ-AMT   TO DL-ADJ.
           MOVE WS-NAME   TO DL-NAME.
           WRITE REPORT-REC FROM DET-LINE.
       WRITE-NOFEE.
           MOVE CC-KEY8   TO DL-ACCT.
           MOVE CC-CLAIM  TO DL-CLAIM.
           MOVE CC-PROC1  TO DL-PROC.
           MOVE CC-AMOUNT TO DL-CHARGE.
           MOVE TOTALPAY  TO DL-PAID.
           MOVE BALANCE   TO DL-BAL.
           MOVE 0         TO DL-ALLOW.
           MOVE 0         TO DL-ADJ.
           MOVE "*** NO FEE SCHEDULE ENTRY" TO DL-NAME.
           WRITE REPORT-REC FROM DET-LINE.
       DONE.
           MOVE SPACES TO REPORT-REC.
           WRITE REPORT-REC.
           MOVE TOT-ADJ TO TL-ADJ.
           WRITE REPORT-REC FROM TOT-LINE.
           DISPLAY "197 CHARGES READ:    " CNT-197.
           DISPLAY "REPORTED (ADJ):      " CNT-RPT.
           DISPLAY "NO BALANCE DUE:      " CNT-NOBAL.
           DISPLAY "NO FEE SCHED ENTRY:  " CNT-NOFEE.
           CLOSE GARFILE CHARCUR PAYCUR MEDFILE2020 REPORTF.
           STOP RUN.
