      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <stephen.waite@cmsvt.com>
      * @author  Claude (Anthropic) <https://claude.ai>
      * @copyright Copyright (c) 2026 cms 
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. delkin.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KINFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS KINFILE-KEY
             LOCK MODE MANUAL.
           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL.
           SELECT KINBACK ASSIGN TO "S55"
             ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  KINFILE.
           COPY KINFILE.CPY.
       FD  CHARCUR.
           COPY CHARCUR.CPY.
       FD  KINBACK.
       01  KINBACK01                  PIC X(72).
       WORKING-STORAGE SECTION.
       77  READ-CNT                   PIC 9(7) VALUE ZERO.
       77  DEL-CNT                    PIC 9(7) VALUE ZERO.
       77  KEEP-CNT                   PIC 9(7) VALUE ZERO.
       77  CONFIRM                    PIC X.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "DELETE KINFILE records whose charge is gone.".
           DISPLAY "This is destructive. Confirm with Y: "
                   WITH NO ADVANCING.
           ACCEPT CONFIRM.
           IF CONFIRM NOT = "Y"
              DISPLAY "Cancelled."
              STOP RUN.
           OPEN I-O    KINFILE
           OPEN INPUT  CHARCUR
           OPEN OUTPUT KINBACK.
           MOVE LOW-VALUES TO KINFILE-KEY.
           START KINFILE KEY >= KINFILE-KEY
                INVALID KEY GO TO P-DONE
           END-START.
       P1.
           READ KINFILE NEXT AT END GO TO P-DONE END-READ.
           ADD 1 TO READ-CNT.
      *>   Probe CHARCUR using the back-reference
           MOVE KIN-CHARCUR-KEY TO CHARCUR-KEY.
           READ CHARCUR INVALID KEY
      *>      Charge is gone -> orphaned KIN record, delete it
                WRITE KINBACK01 FROM KINFILE01
                DELETE KINFILE RECORD
                ADD 1 TO DEL-CNT
                GO TO P1
           END-READ.
      *>   Charge still exists -> keep the KIN record
           ADD 1 TO KEEP-CNT.
           GO TO P1.
       P-DONE.
           DISPLAY "KIN records read:    " READ-CNT.
           DISPLAY "Deleted (charge gone):" DEL-CNT.
           DISPLAY "Kept (charge exists): " KEEP-CNT.
           CLOSE KINFILE CHARCUR KINBACK.
           STOP RUN.