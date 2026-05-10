* @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <stephen.waite@cmsvt.com>
      * @author  Claude (Anthropic) <https://claude.ai>
      * @copyright Copyright (c) 2026 cms 
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. wo004.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL.
           SELECT FILEOUT ASSIGN TO "S50"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  CHARCUR.
           COPY CHARCUR.CPY.
       FD  FILEOUT.
       01  FILEOUT01                  PIC X(8).
       WORKING-STORAGE SECTION.
       77  TO-DATE                    PIC X(8).
       77  HOLD8                      PIC X(8).
       77  CNT                        PIC 9(7) VALUE ZERO.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "Archive charges on or before (YYYYMMDD): "
                   WITH NO ADVANCING.
           ACCEPT TO-DATE.
           IF TO-DATE = SPACES
              DISPLAY "Date required."
              STOP RUN.
           OPEN INPUT  CHARCUR
           OPEN OUTPUT FILEOUT.
           MOVE LOW-VALUES TO CHARCUR-KEY.
           START CHARCUR KEY >= CHARCUR-KEY
                INVALID KEY GO TO P-DONE
           END-START.
           MOVE LOW-VALUES TO HOLD8.
       P00.
           READ CHARCUR NEXT AT END GO TO P-DONE END-READ.
           IF CC-DATE-T > TO-DATE GO TO P00.
           IF CC-KEY8 = HOLD8     GO TO P00.
           MOVE CC-KEY8 TO HOLD8.
           WRITE FILEOUT01 FROM CC-KEY8.
           ADD 1 TO CNT.
           GO TO P00.
       P-DONE.
           DISPLAY "GARNOs written: " CNT.
           CLOSE CHARCUR FILEOUT.
           STOP RUN.