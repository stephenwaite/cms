      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <stephen.waite@cmsvt.com>
      * @author  Claude (Anthropic) <https://claude.ai>
      * @copyright Copyright (c) 2026 cms 
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. delauth.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AUTHFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS AUTH-KEY
             LOCK MODE MANUAL.
           SELECT GARIN ASSIGN TO "S50"
             ORGANIZATION LINE SEQUENTIAL.
           SELECT AUTHBACK ASSIGN TO "S55"
             ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  AUTHFILE.
           COPY AUTHFILE.CPY.
       FD  GARIN.
       01  GARIN01.
           02 GI-GARNO                PIC X(8).
           02 FILLER                  PIC X(307).
       FD  AUTHBACK.
       01  AUTHBACK01                 PIC X(42).
       WORKING-STORAGE SECTION.
       77  READ-CNT                   PIC 9(7) VALUE ZERO.
       77  DEL-CNT                    PIC 9(7) VALUE ZERO.
       77  GAR-HIT                    PIC 9(7) VALUE ZERO.
       77  HOLD8                      PIC X(8).
       77  CONFIRM                    PIC X.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "DELETE AUTHFILE records for archived GARNOs.".
           DISPLAY "This is destructive. Confirm with Y: "
                   WITH NO ADVANCING.
           ACCEPT CONFIRM.
           IF CONFIRM NOT = "Y"
              DISPLAY "Cancelled."
              STOP RUN.
           OPEN INPUT  GARIN
           OPEN I-O    AUTHFILE
           OPEN OUTPUT AUTHBACK.

       P1.
           READ GARIN AT END GO TO P-DONE END-READ.
           ADD 1 TO READ-CNT.
           MOVE GI-GARNO TO HOLD8.

      *>   Position at the first AUTHFILE record for this GARNO
           MOVE HOLD8      TO AUTH-KEY8
           MOVE LOW-VALUES TO AUTH-KEY6.
           START AUTHFILE KEY >= AUTH-KEY
                INVALID KEY GO TO P1
           END-START.
           MOVE 0 TO GAR-HIT.
       P2.
           READ AUTHFILE NEXT AT END GO TO P3 END-READ.
           IF AUTH-KEY8 NOT = HOLD8 GO TO P3.
           WRITE AUTHBACK01 FROM AUTHFILE01.
           DELETE AUTHFILE RECORD.
           ADD 1 TO DEL-CNT.
           GO TO P2.
       P3.
           GO TO P1.
       P-DONE.
           DISPLAY "GARNOs read:        " READ-CNT.
           DISPLAY "AUTH records deleted:" DEL-CNT.
           CLOSE GARIN AUTHFILE AUTHBACK.
           STOP RUN.