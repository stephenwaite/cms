      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <stephen.waite@cmsvt.com>
      * @author  Claude (Anthropic) <https://claude.ai>
      * @copyright Copyright (c) 2026 cms 
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. actpass2.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACTFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS A-ACTNO
             ALTERNATE RECORD KEY IS A-GARNO WITH DUPLICATES
             ALTERNATE RECORD KEY IS NAME-KEY WITH DUPLICATES
             LOCK MODE MANUAL.
           SELECT WORKIN ASSIGN TO "S50"
             ORGANIZATION LINE SEQUENTIAL.
           SELECT ACTBACK ASSIGN TO "S55"
             ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  ACTFILE.
           COPY ACTFILE.CPY.
       FD  WORKIN.
       01  WORKIN01                   PIC X(8).
       FD  ACTBACK.
       01  ACTBACK01                  PIC X(318).
       WORKING-STORAGE SECTION.
       77  READ-CNT                   PIC 9(7) VALUE ZERO.
       77  CLR-CNT                    PIC 9(7) VALUE ZERO.
       77  NOREC-CNT                  PIC 9(7) VALUE ZERO.
       77  CONFIRM                    PIC X.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "CLEAR A-GARNO on ACTFILE (pass 2).".
           DISPLAY "This modifies records. Confirm with Y: "
                   WITH NO ADVANCING.
           ACCEPT CONFIRM.
           IF CONFIRM NOT = "Y"
              DISPLAY "Cancelled."
              STOP RUN.
           OPEN INPUT WORKIN
           OPEN I-O   ACTFILE
           OPEN OUTPUT ACTBACK.
       P1.
           READ WORKIN AT END GO TO P-DONE END-READ.
           ADD 1 TO READ-CNT.
           MOVE WORKIN01 TO A-ACTNO.
           READ ACTFILE WITH LOCK
                INVALID KEY
                   ADD 1 TO NOREC-CNT
                   GO TO P1
           END-READ.
           WRITE ACTBACK01 FROM ACTFILE01.
           MOVE SPACES TO A-GARNO.
           REWRITE ACTFILE01 INVALID KEY
                DISPLAY "REWRITE FAIL: " A-ACTNO
                GO TO P1
           END-REWRITE.
           ADD 1 TO CLR-CNT.
           GO TO P1.
       P-DONE.
           DISPLAY "ACTNOs read:      " READ-CNT.
           DISPLAY "A-GARNO cleared:  " CLR-CNT.
           DISPLAY "ACT not found:    " NOREC-CNT.
           CLOSE WORKIN ACTFILE ACTBACK.
           STOP RUN.