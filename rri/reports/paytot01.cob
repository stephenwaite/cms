******************************************************************
      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2026 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
      *
      * Total payments by place of service for a posting-date range.
      * Buckets: place 3, 5, E, N each shown separately, plus CHCRR
      * for everything else (the comparison number).
      * Reads AGEDATE for the low/high posting date window.
      * Skips standard adjustment paycodes (007-009, 011-017) and
      * denials (DI, 14, 15).
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. paytot01.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AGEDATE ASSIGN TO "S30"
               ORGANIZATION LINE SEQUENTIAL.
           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
               ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES.
           SELECT PAYCUR ASSIGN TO "S65" ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL RECORD KEY IS PAYCUR-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD  AGEDATE
           DATA RECORD IS AGEDATE01.
       01  AGEDATE01.
           02 LOW-DATE  PIC X(8).
           02 HIGH-DATE PIC X(8).
       FD  PAYCUR.
           COPY PAYCUR.CPY.
       FD  CHARCUR.
           COPY CHARCUR.CPY.
       WORKING-STORAGE SECTION.
       01  TOT-PLACE-3   PIC S9(7)V99 VALUE 0.
       01  TOT-PLACE-5   PIC S9(7)V99 VALUE 0.
       01  TOT-PLACE-E   PIC S9(7)V99 VALUE 0.
       01  TOT-PLACE-N   PIC S9(7)V99 VALUE 0.
       01  TOT-CHCRR     PIC S9(7)V99 VALUE 0.
       01  TOT-NOMATCH   PIC S9(7)V99 VALUE 0.
       01  CNT-PLACE-3   PIC 9(7) VALUE 0.
       01  CNT-PLACE-5   PIC 9(7) VALUE 0.
       01  CNT-PLACE-E   PIC 9(7) VALUE 0.
       01  CNT-PLACE-N   PIC 9(7) VALUE 0.
       01  CNT-CHCRR     PIC 9(7) VALUE 0.
       01  CNT-NOMATCH   PIC 9(7) VALUE 0.
       01  EDIT-AMT      PIC $$,$$$,$$9.99-.
       01  EDIT-CNT      PIC ZZZ,ZZ9.
       01  FOUND-FLG     PIC X VALUE "N".
       01  SAVE-PLACE    PIC X VALUE SPACE.
       PROCEDURE DIVISION.
        P0.
           OPEN INPUT AGEDATE CHARCUR PAYCUR.
           READ AGEDATE.
           DISPLAY "POSTING DATE RANGE: " LOW-DATE " TO " HIGH-DATE.
        P1.
           READ PAYCUR AT END GO TO P2.
           IF (PC-PAYCODE = "007" OR "008" OR "009" OR "011"
               OR "012" OR "013" OR "014" OR "015" OR "016" OR "017")
               OR (PC-DENIAL = "DI" OR "15" OR "14")
                   GO TO P1
           END-IF
           IF PC-DATE-T < LOW-DATE OR > HIGH-DATE GO TO P1.
           PERFORM Z1 THRU Z1-EXIT.
           IF FOUND-FLG = "N"
               ADD PC-AMOUNT TO TOT-NOMATCH
               ADD 1 TO CNT-NOMATCH
               GO TO P1
           END-IF.
           IF SAVE-PLACE = "3"
               ADD PC-AMOUNT TO TOT-PLACE-3
               ADD 1 TO CNT-PLACE-3
           ELSE IF SAVE-PLACE = "5"
               ADD PC-AMOUNT TO TOT-PLACE-5
               ADD 1 TO CNT-PLACE-5
           ELSE IF SAVE-PLACE = "E"
               ADD PC-AMOUNT TO TOT-PLACE-E
               ADD 1 TO CNT-PLACE-E
           ELSE IF SAVE-PLACE = "N"
               ADD PC-AMOUNT TO TOT-PLACE-N
               ADD 1 TO CNT-PLACE-N
           ELSE
               ADD PC-AMOUNT TO TOT-CHCRR
               ADD 1 TO CNT-CHCRR.
           GO TO P1.
        Z1.
           MOVE "N" TO FOUND-FLG
           MOVE SPACE TO SAVE-PLACE
           MOVE PC-KEY8 TO CC-KEY8
           MOVE SPACE TO CC-KEY3
           START CHARCUR KEY > CHARCUR-KEY
               INVALID GO TO Z1-EXIT.
        Z2.
           READ CHARCUR NEXT AT END GO TO Z1-EXIT.
           IF CC-KEY8 NOT = PC-KEY8 GO TO Z1-EXIT.
           IF CC-CLAIM NOT = PC-CLAIM GO TO Z2.
           MOVE "Y" TO FOUND-FLG
           MOVE CC-PLACE TO SAVE-PLACE.
        Z1-EXIT.
           EXIT.
        P2.
           DISPLAY " ".
           DISPLAY "PLACE  COUNT        AMOUNT".
           DISPLAY "-----  -----------  ----------------".
           MOVE CNT-PLACE-3 TO EDIT-CNT.
           MOVE TOT-PLACE-3 TO EDIT-AMT.
           DISPLAY "  3    " EDIT-CNT "  " EDIT-AMT.
           MOVE CNT-PLACE-5 TO EDIT-CNT.
           MOVE TOT-PLACE-5 TO EDIT-AMT.
           DISPLAY "  5    " EDIT-CNT "  " EDIT-AMT.
           MOVE CNT-PLACE-E TO EDIT-CNT.
           MOVE TOT-PLACE-E TO EDIT-AMT.
           DISPLAY "  E    " EDIT-CNT "  " EDIT-AMT.
           MOVE CNT-PLACE-N TO EDIT-CNT.
           MOVE TOT-PLACE-N TO EDIT-AMT.
           DISPLAY "  N    " EDIT-CNT "  " EDIT-AMT.
           MOVE CNT-CHCRR TO EDIT-CNT.
           MOVE TOT-CHCRR TO EDIT-AMT.
           DISPLAY "CHCRR  " EDIT-CNT "  " EDIT-AMT.
           MOVE CNT-NOMATCH TO EDIT-CNT.
           MOVE TOT-NOMATCH TO EDIT-AMT.
           DISPLAY "NOCHG  " EDIT-CNT "  " EDIT-AMT.
           CLOSE AGEDATE CHARCUR PAYCUR.
           STOP RUN.