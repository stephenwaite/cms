      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <stephen.waite@cmsvt.com>
      * @author  Claude (Anthropic) <https://claude.ai>
      * @copyright Copyright (c) 2026 cms 
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. addcc.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL.
           SELECT INFILE ASSIGN TO "S50"
             ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  CHARCUR.
           COPY CHARCUR.CPY.
       FD  INFILE.
       01  INREC                      PIC X(160).
       WORKING-STORAGE SECTION.
       77  READ-CNT                   PIC 9(7) VALUE ZERO.
       77  ADD-CNT                    PIC 9(7) VALUE ZERO.
       77  DUP-CNT                    PIC 9(7) VALUE ZERO.
       77  CONFIRM                    PIC X.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "RESTORE archived charges into CHARCUR.".
           DISPLAY "Confirm with Y: " WITH NO ADVANCING.
           ACCEPT CONFIRM.
           IF CONFIRM NOT = "Y"
              DISPLAY "Cancelled."
              STOP RUN.
           OPEN INPUT INFILE
           OPEN I-O   CHARCUR.
       P1.
           READ INFILE AT END GO TO P-DONE END-READ.
           ADD 1 TO READ-CNT.
           MOVE INREC TO CHARCUR01.
           WRITE CHARCUR01 INVALID KEY
                DISPLAY "DUP (already exists): " CC-KEY8 " " CC-KEY3
                ADD 1 TO DUP-CNT
           NOT INVALID KEY
                ADD 1 TO ADD-CNT
           END-WRITE.
           GO TO P1.
       P-DONE.
           DISPLAY "Read:    " READ-CNT.
           DISPLAY "Added:   " ADD-CNT.
           DISPLAY "Dup/skip:" DUP-CNT.
           CLOSE INFILE CHARCUR.
           STOP RUN.