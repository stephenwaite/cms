      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <stephen.waite@cmsvt.com>
      * @author  Claude (Anthropic) <https://claude.ai>
      * @copyright Copyright (c) 2026 cms 
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. delgar.
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
           SELECT GARIN ASSIGN TO "S50"
             ORGANIZATION LINE SEQUENTIAL.
           SELECT GARBACK ASSIGN TO "S55"
             ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  GARFILE.
           COPY GARFILE.CPY.
       FD  CHARCUR.
           COPY CHARCUR.CPY.
       FD  PAYCUR.
           COPY PAYCUR.CPY.
       FD  GARIN.
       01  GARIN01.
           02 GI-GARNO                PIC X(8).
           02 FILLER                  PIC X(307).
       FD  GARBACK.
       01  GARBACK01                  PIC X(315).
       WORKING-STORAGE SECTION.
       77  READ-CNT                   PIC 9(7) VALUE ZERO.
       77  DEL-CNT                    PIC 9(7) VALUE ZERO.
       77  KEEP-CHAR                  PIC 9(7) VALUE ZERO.
       77  KEEP-PAY                   PIC 9(7) VALUE ZERO.
       77  KEEP-INS                   PIC 9(7) VALUE ZERO.
       77  NOGAR-CNT                  PIC 9(7) VALUE ZERO.
       77  HOLD8                      PIC X(8).
       77  SKIP-FLAG                  PIC 9.
       77  CONFIRM                    PIC X.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "DELETE orphaned GARFILE records.".
           DISPLAY "Only deletes when CHARCUR + PAYCUR empty and".
           DISPLAY "G-INSPEND = 0.  This is destructive.".
           DISPLAY "Confirm with Y: " WITH NO ADVANCING.
           ACCEPT CONFIRM.
           IF CONFIRM NOT = "Y"
              DISPLAY "Cancelled."
              STOP RUN.
           OPEN INPUT  GARIN
           OPEN I-O    GARFILE
           OPEN INPUT  CHARCUR PAYCUR
           OPEN OUTPUT GARBACK.

       P1.
           READ GARIN AT END GO TO P-DONE END-READ.
           ADD 1 TO READ-CNT.
           MOVE GI-GARNO TO HOLD8.

      *>   Read the GARFILE record.  Skip if already gone.
           MOVE HOLD8 TO G-GARNO.
           READ GARFILE INVALID KEY
                ADD 1 TO NOGAR-CNT
                GO TO P1
           END-READ.

           MOVE 0 TO SKIP-FLAG.

      *>   Safety 1: pending insurance money?
           IF G-INSPEND NOT = 0
              ADD 1 TO KEEP-INS
              MOVE 1 TO SKIP-FLAG.

      *>   Safety 2: any CHARCUR records remain for this KEY8?
           IF SKIP-FLAG = 0
              PERFORM CHECK-CHARCUR.

      *>   Safety 3: any PAYCUR records remain for this KEY8?
           IF SKIP-FLAG = 0
              PERFORM CHECK-PAYCUR.

      *>   All clear -> back up and delete
           IF SKIP-FLAG = 0
              WRITE GARBACK01 FROM GARFILE01
              MOVE HOLD8 TO G-GARNO
              READ GARFILE WITH LOCK INVALID KEY
                   GO TO P1
              END-READ
              DELETE GARFILE RECORD
              ADD 1 TO DEL-CNT.

           GO TO P1.

      *>   ─── Probe CHARCUR for any record with this KEY8 ───
       CHECK-CHARCUR.
           MOVE HOLD8      TO CC-KEY8
           MOVE LOW-VALUES TO CC-KEY3.
           START CHARCUR KEY >= CHARCUR-KEY
                INVALID KEY GO TO CHECK-CHARCUR-EXIT
           END-START.
           READ CHARCUR NEXT AT END
                GO TO CHECK-CHARCUR-EXIT
           END-READ.
           IF CC-KEY8 = HOLD8
              ADD 1 TO KEEP-CHAR
              MOVE 1 TO SKIP-FLAG.
       CHECK-CHARCUR-EXIT.
           EXIT.

      *>   ─── Probe PAYCUR for any record with this KEY8 ───
       CHECK-PAYCUR.
           MOVE HOLD8      TO PC-KEY8
           MOVE LOW-VALUES TO PC-KEY3.
           START PAYCUR KEY >= PAYCUR-KEY
                INVALID KEY GO TO CHECK-PAYCUR-EXIT
           END-START.
           READ PAYCUR NEXT AT END
                GO TO CHECK-PAYCUR-EXIT
           END-READ.
           IF PC-KEY8 = HOLD8
              ADD 1 TO KEEP-PAY
              MOVE 1 TO SKIP-FLAG.
       CHECK-PAYCUR-EXIT.
           EXIT.

       P-DONE.
           DISPLAY "Read:              " READ-CNT.
           DISPLAY "Deleted:           " DEL-CNT.
           DISPLAY "Kept (charges):    " KEEP-CHAR.
           DISPLAY "Kept (payments):   " KEEP-PAY.
           DISPLAY "Kept (ins pending):" KEEP-INS.
           DISPLAY "GAR not found:     " NOGAR-CNT.
           CLOSE GARIN GARFILE CHARCUR PAYCUR GARBACK.
           STOP RUN.