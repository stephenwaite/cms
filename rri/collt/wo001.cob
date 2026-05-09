      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <stephen.waite@cmsvt.com>
      * @author  Claude (Anthropic) <https://claude.ai>
      * @copyright Copyright (c) 2026 cms 
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. wo001.
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
           SELECT PAYFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS PAYFILE-KEY
             LOCK MODE MANUAL.
           SELECT INFILE ASSIGN TO "S50"
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
       FD  INFILE.
       01  INREC.
           05 IN-KEY8                 PIC X(8).
           05 IN-KEY3                 PIC X(3).
           05 FILLER                  PIC X(69).
       WORKING-STORAGE SECTION.
       77  TOTALPAY                   PIC S9(7)V99 COMP-3 VALUE ZERO.
       77  WO-AMT                     PIC S9(7)V99 COMP-3.
       77  TODAY                      PIC X(8).
       77  XYZ                        PIC 9(3).
       01  TIME-NOW.
           05 TN-HHMMSS               PIC X(6).
           05 FILLER                  PIC X(2).
       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT  INFILE CHARCUR PAYCUR GARFILE
           OPEN I-O    PAYFILE
           ACCEPT TODAY FROM DATE YYYYMMDD.
       P00.
           READ INFILE AT END GO TO P-DONE END-READ.
           MOVE IN-KEY8 TO CC-KEY8
           MOVE IN-KEY3 TO CC-KEY3.
           READ CHARCUR INVALID KEY
                DISPLAY "NO CHARGE: " IN-KEY8 " " IN-KEY3
                GO TO P00
           END-READ.
           MOVE CC-KEY8 TO G-GARNO.
           READ GARFILE INVALID KEY
                DISPLAY "NO GAR: " CC-KEY8
                MOVE SPACES TO G-GARNAME
           END-READ.
           MOVE ZERO       TO TOTALPAY
           MOVE CC-KEY8    TO PC-KEY8
           MOVE LOW-VALUES TO PC-KEY3.
           START PAYCUR KEY >= PAYCUR-KEY
                INVALID KEY GO TO P-WO
           END-START.
       P1.
           READ PAYCUR NEXT AT END GO TO P-WO END-READ.
           IF PC-KEY8  NOT = CC-KEY8  GO TO P-WO.
           IF PC-CLAIM NOT = CC-CLAIM GO TO P1.
           ADD PC-AMOUNT TO TOTALPAY.
           GO TO P1.
       P-WO.
           COMPUTE WO-AMT = CC-AMOUNT - TOTALPAY.
           IF WO-AMT NOT > 0
              DISPLAY "SKIP " IN-KEY8 " " IN-KEY3
                      " CLM=" CC-CLAIM " BAL=" WO-AMT
              GO TO P00.
           MOVE CC-KEY8 TO PD-KEY8
           MOVE ZERO    TO XYZ.
       P3.
           ADD 1 TO XYZ.
           MOVE XYZ TO PD-KEY3.
           READ PAYFILE INVALID GO TO P4.
           GO TO P3.
       P4.
           ACCEPT TIME-NOW FROM TIME.
           MOVE G-GARNAME  TO PD-NAME
           MOVE WO-AMT     TO PD-AMOUNT
           MOVE "013"      TO PD-PAYCODE
           MOVE SPACES     TO PD-DENIAL
           MOVE CC-CLAIM   TO PD-CLAIM
           MOVE TODAY      TO PD-DATE-T
           MOVE TODAY      TO PD-DATE-E
           MOVE TN-HHMMSS  TO PD-ORDER
           MOVE SPACES     TO PD-BATCH.
           WRITE PAYFILE01 INVALID KEY
                DISPLAY "DUP: " PD-KEY8 " " PD-KEY3
                GO TO P00
           END-WRITE.
           DISPLAY "WO " IN-KEY8 " " IN-KEY3
                   " CLM=" CC-CLAIM " AMT=" WO-AMT.
           ACCEPT OMITTED.        
           GO TO P00.
       P-DONE.
           CLOSE INFILE CHARCUR PAYCUR GARFILE PAYFILE.
           STOP RUN.