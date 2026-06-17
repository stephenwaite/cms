      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <stephen.waite@cmsvt.com>
      * @author  Claude (Anthropic) <https://claude.ai>
      * @copyright Copyright (c) 2026 cms 
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. delcc.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL.
           SELECT FILEIN ASSIGN TO "S35"
             ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  CHARCUR.
           COPY CHARCUR.CPY.
       FD  FILEIN.
       01  FILEIN01.
           02 FI-KEY                  PIC X(11).
           02 FILLER                  PIC X(149).
       WORKING-STORAGE SECTION.
       77  DEL-CNT                    PIC 9(7) VALUE ZERO.
       77  BAD-CNT                    PIC 9(7) VALUE ZERO.
       77  READ-CNT                   PIC 9(7) VALUE ZERO.
       77  CONFIRM                    PIC X.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "DELETE charges from CHARCUR listed in FILEIN.".
           DISPLAY "This is destructive. Confirm with Y: "
                   WITH NO ADVANCING.
           ACCEPT CONFIRM.
           IF CONFIRM NOT = "Y"
              DISPLAY "Cancelled."
              STOP RUN.
           OPEN I-O   CHARCUR
           OPEN INPUT FILEIN.
       P1.
           READ FILEIN AT END GO TO P2 END-READ.
           ADD 1 TO READ-CNT.
           MOVE FI-KEY TO CHARCUR-KEY.
           READ CHARCUR WITH LOCK
                INVALID KEY
                   DISPLAY "BAD: " FI-KEY
                   ADD 1 TO BAD-CNT
                   GO TO P1
           END-READ.
           DELETE CHARCUR RECORD.
           ADD 1 TO DEL-CNT.
           GO TO P1.
       P2.
           DISPLAY "Read:    " READ-CNT.
           DISPLAY "Deleted: " DEL-CNT.
           DISPLAY "Not found: " BAD-CNT.
           CLOSE CHARCUR FILEIN.
           STOP RUN.