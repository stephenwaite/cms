      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NEI146.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARCUR ASSIGN TO "S30"
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS CHARCUR-KEY
               ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
               LOCK MODE MANUAL.
           SELECT PAYFILE ASSIGN TO "S35"
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS PAYFILE-KEY
               LOCK MODE MANUAL.
           SELECT KINFILE ASSIGN TO "S40"
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS KINFILE-KEY
               ALTERNATE RECORD KEY IS KIN-DATE-E WITH DUPLICATES
               ALTERNATE RECORD KEY IS KIN-STAT WITH DUPLICATES
               LOCK MODE MANUAL.
       DATA DIVISION.
       FILE SECTION.
       FD  KINFILE.
           COPY "kinfile.cpy".
       FD  PAYFILE.
           COPY "payfile.cpy".
       FD  CHARCUR.
           COPY "charcur.cpy".
       WORKING-STORAGE SECTION.
       01  X PIC 99.
       01  Y PIC 99.
       01  Z PIC 99.
       01  A PIC 99.
       01  ALF6.
           02 ALF4 PIC X(4).
           02 ALF2 PIC XX.
       01  PAYDATE PIC X(8).
       01  XYZ PIC 999.
       01  NUM6 PIC 9(6).
       01  KINBACK01 PIC X(74).
       01  ALF1 PIC X.
       01  NEF-6 PIC Z,ZZ9.99CR.

       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT CHARCUR PAYFILE.
           OPEN I-O KINFILE.
           ACCEPT KIN-DATE-E FROM CENTURY-DATE.
       P1.
           READ PAYFILE NEXT AT END GO TO P9.
        
           IF PD-PAYCODE = "197" AND PD-DENIAL = "14"
               NEXT SENTENCE
           ELSE
           IF (PD-PAYCODE > "006" AND < "010")
           OR (PD-PAYCODE > "010" AND < "019")
           OR (PD-DENIAL = "08" OR "AA" OR "15")
               GO TO P1
           END-IF.
           
           MOVE PD-KEY8 TO CC-KEY8.
           MOVE SPACE TO CC-KEY3.
           START CHARCUR KEY NOT < CHARCUR-KEY
               INVALID DISPLAY "CHARCUR START INVALID: " PD-KEY8
               GO TO P1.
       P2.
           READ CHARCUR NEXT AT END
               GO TO P1.
           IF CC-KEY8 NOT = PD-KEY8
               GO TO P1.
           IF CC-CLAIM NOT = PD-CLAIM
               GO TO P2.
           IF CC-COLLT NOT = "1"
               GO TO P2.
           MOVE PD-DATE-T TO KIN-DATE-T.
           MOVE PD-NAME TO KIN-NAME.
           MOVE PD-AMOUNT TO KIN-AMOUNT.
           MOVE PD-DENIAL TO KIN-DENIAL.
           MOVE CHARCUR-KEY TO KIN-CHARCUR-KEY.
           MOVE PD-PAYCODE TO KIN-PAYCODE.
           MOVE "0" TO KIN-STAT.
           MOVE PD-KEY8 TO KIN-KEY8.
           MOVE SPACE TO KIN-KEY3.
           MOVE KINFILE01 TO KINBACK01.
           MOVE 0 TO XYZ.
       P3.
           ADD 1 TO XYZ.
           MOVE XYZ TO KIN-KEY3.
           READ KINFILE INVALID KEY GO TO P4.
           GO TO P3.
       P4.
           MOVE KINBACK01 TO KINFILE01.
           MOVE XYZ TO KIN-KEY3.
           WRITE KINFILE01.
           GO TO P1.
       P9.
           CLOSE CHARCUR PAYFILE KINFILE.
           STOP RUN.