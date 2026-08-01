      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <stephen.waite@cmsvt.com>
      * @author  Claude (Anthropic) <https://claude.ai>
      * @copyright Copyright (c) 2026 cms 
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. actpass1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACTFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS A-ACTNO
             ALTERNATE RECORD KEY IS A-GARNO WITH DUPLICATES
             ALTERNATE RECORD KEY IS NAME-KEY WITH DUPLICATES
             LOCK MODE MANUAL.
           SELECT GARIN ASSIGN TO "S50"
             ORGANIZATION LINE SEQUENTIAL.
           SELECT WORKOUT ASSIGN TO "S55"
             ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  ACTFILE.
           COPY ACTFILE.CPY.
       FD  GARIN.
       01  GARIN01.
           02 GI-GARNO                PIC X(8).
           02 FILLER                  PIC X(307).
       FD  WORKOUT.
       01  WORKOUT01                  PIC X(8).
       WORKING-STORAGE SECTION.
       77  READ-CNT                   PIC 9(7) VALUE ZERO.
       77  HIT-CNT                    PIC 9(7) VALUE ZERO.
       77  NOREC-CNT                  PIC 9(7) VALUE ZERO.
       77  HOLD8                      PIC X(8).
       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT  GARIN
           OPEN INPUT  ACTFILE
           OPEN OUTPUT WORKOUT.
       P1.
           READ GARIN AT END GO TO P-DONE END-READ.
           ADD 1 TO READ-CNT.
           MOVE GI-GARNO TO HOLD8.
           MOVE HOLD8 TO A-GARNO.
           START ACTFILE KEY >= A-GARNO
                INVALID KEY
                   ADD 1 TO NOREC-CNT
                   GO TO P1
           END-START.
       P2.
           READ ACTFILE NEXT AT END GO TO P1 END-READ.
           IF A-GARNO NOT = HOLD8 GO TO P1.
           WRITE WORKOUT01 FROM A-ACTNO.
           ADD 1 TO HIT-CNT.
           GO TO P2.
       P-DONE.
           DISPLAY "GARNOs read:        " READ-CNT.
           DISPLAY "ACTNOs to clear:    " HIT-CNT.
           DISPLAY "GARNOs with no ACT: " NOREC-CNT.
           CLOSE GARIN ACTFILE WORKOUT.
           STOP RUN.