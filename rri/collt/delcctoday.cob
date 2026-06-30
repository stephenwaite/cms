      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <stephen.waite@cmsvt.com>
      * @author  Claude (Anthropic) <https://claude.ai>
      * @copyright Copyright (c) 2026 cms 
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. delcctoday.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL.
           SELECT CCBACK ASSIGN TO "S55"
             ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  CHARCUR.
           COPY CHARCUR.CPY.
       FD  CCBACK.
       01  CCBACK01                   PIC X(160).
       WORKING-STORAGE SECTION.
       77  POST-DATE                  PIC X(8) VALUE "20260630".
       77  READ-CNT                   PIC 9(7) VALUE ZERO.
       77  HIT-CNT                    PIC 9(7) VALUE ZERO.
       77  DEL-CNT                    PIC 9(7) VALUE ZERO.
       77  BAD-CNT                    PIC 9(7) VALUE ZERO.
       77  IX                         PIC 9(5).
       77  CONFIRM                    PIC X.
       01  KEY-TABLE.
           02 SAVED-KEY OCCURS 1000 TIMES PIC X(11).
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "DELETE CHARCUR charges with CC-DATE-P = "
                   POST-DATE.
           DISPLAY "This is destructive. Confirm with Y: "
                   WITH NO ADVANCING.
           ACCEPT CONFIRM.
           IF CONFIRM NOT = "Y"
              DISPLAY "Cancelled."
              STOP RUN.
           OPEN I-O    CHARCUR
           OPEN OUTPUT CCBACK.

      *>   ── Pass 1: walk CHARCUR, collect matching keys, back up ──
           MOVE LOW-VALUES TO CHARCUR-KEY.
           START CHARCUR KEY >= CHARCUR-KEY
                INVALID KEY GO TO P-DEL
           END-START.
       P-SCAN.
           READ CHARCUR NEXT AT END GO TO P-DEL END-READ.
           ADD 1 TO READ-CNT.
           IF CC-DATE-P = POST-DATE
              ADD 1 TO HIT-CNT
              IF HIT-CNT > 1000
                 DISPLAY "TABLE OVERFLOW - more than 1000 matches"
                 DISPLAY "Aborting before any delete."
                 CLOSE CHARCUR CCBACK
                 STOP RUN
              END-IF
              MOVE CHARCUR-KEY TO SAVED-KEY(HIT-CNT)
              WRITE CCBACK01 FROM CHARCUR01
           END-IF.
           GO TO P-SCAN.

      *>   ── Pass 2: delete the collected keys by direct read ──
       P-DEL.
           IF HIT-CNT = 0
              DISPLAY "No charges found for " POST-DATE
              GO TO P-DONE.
           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > HIT-CNT
              MOVE SAVED-KEY(IX) TO CHARCUR-KEY
              READ CHARCUR WITH LOCK
                   INVALID KEY
                      ADD 1 TO BAD-CNT
                   NOT INVALID KEY
                      DELETE CHARCUR RECORD
                      ADD 1 TO DEL-CNT
              END-READ
           END-PERFORM.

       P-DONE.
           DISPLAY "Records scanned:  " READ-CNT.
           DISPLAY "Matched date:     " HIT-CNT.
           DISPLAY "Deleted:          " DEL-CNT.
           DISPLAY "Not found at del: " BAD-CNT.
           CLOSE CHARCUR CCBACK.
           STOP RUN.