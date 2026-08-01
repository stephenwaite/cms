      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <stephen.waite@cmsvt.com>
      * @author  Claude (Anthropic) <https://claude.ai>
      * @copyright Copyright (c) 2026 cms 
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. delmplr.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MPLRFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS MPLR-KEY
             LOCK MODE MANUAL.
           SELECT GARIN ASSIGN TO "S50"
             ORGANIZATION LINE SEQUENTIAL.
           SELECT MPLRBACK ASSIGN TO "S55"
             ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  MPLRFILE.
           COPY MPLRFILE.CPY.
       FD  GARIN.
       01  GARIN01.
           02 GI-GARNO                PIC X(8).
           02 FILLER                  PIC X(307).
       FD  MPLRBACK.
       01  MPLRBACK01                 PIC X(160).
       WORKING-STORAGE SECTION.
       77  READ-CNT                   PIC 9(7) VALUE ZERO.
       77  DEL-CNT                    PIC 9(7) VALUE ZERO.
       77  NOREC-CNT                  PIC 9(7) VALUE ZERO.
       77  CONFIRM                    PIC X.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "DELETE MPLRFILE records for archived GARNOs.".
           DISPLAY "This is destructive. Confirm with Y: "
                   WITH NO ADVANCING.
           ACCEPT CONFIRM.
           IF CONFIRM NOT = "Y"
              DISPLAY "Cancelled."
              STOP RUN.
           OPEN INPUT  GARIN
           OPEN I-O    MPLRFILE
           OPEN OUTPUT MPLRBACK.
       P1.
           READ GARIN AT END GO TO P-DONE END-READ.
           ADD 1 TO READ-CNT.
           MOVE GI-GARNO TO MPLR-KEY.
           READ MPLRFILE WITH LOCK
                INVALID KEY
                   ADD 1 TO NOREC-CNT
                   GO TO P1
           END-READ.
           WRITE MPLRBACK01 FROM MPLRFILE01.
           DELETE MPLRFILE RECORD.
           ADD 1 TO DEL-CNT.
           GO TO P1.
       P-DONE.
           DISPLAY "GARNOs read:       " READ-CNT.
           DISPLAY "MPLR deleted:      " DEL-CNT.
           DISPLAY "No MPLR record:    " NOREC-CNT.
           CLOSE GARIN MPLRFILE MPLRBACK.
           STOP RUN.