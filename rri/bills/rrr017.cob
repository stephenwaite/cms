      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrr017.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL.
           SELECT AGEDATE ASSIGN TO "S35" ORGANIZATION
             LINE SEQUENTIAL.
           SELECT GARFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
             ACCESS MODE IS RANDOM RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
             LOCK MODE MANUAL.
           SELECT PAYCUR ASSIGN TO "S45" ORGANIZATION IS INDEXED
             ACCESS IS SEQUENTIAL RECORD KEY IS PAYCUR-KEY
             LOCK MODE MANUAL.
           SELECT FILEOUT ASSIGN TO "S50" ORGANIZATION
             LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  FILEOUT.
       01  FILEOUT01.
           02 FO-1 PIC X(11).
           02 FO-2 PIC X(11).
       FD  AGEDATE.
       01   AGEDATE01 PIC 9(8).
       FD  CHARCUR
      *    BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS CHARCUR01.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC PIC X(11).
           02 CC-MOD2 PIC XX.
           02 CC-MOD3 PIC XX.
           02 CC-MOD4 PIC XX.
           02 CC-AMOUNT PIC S9(4)V99.
           02 CC-DOCR PIC X(3).
           02 CC-DOCP PIC X(2).
           02 CC-PAYCODE PIC XXX.
           02 CC-STUD PIC X.
           02 CC-WORK PIC XX.
           02 CC-DAT1 PIC X(8).
           02 CC-RESULT PIC X.
           02 CC-ACT PIC X.
           02 CC-SORCREF PIC X.
           02 CC-COLLT PIC X.
           02 CC-AUTH PIC X.
           02 CC-PAPER PIC X.
           02 CC-PLACE PIC X.
           02 CC-EPSDT PIC X.
           02 CC-DATE-T PIC X(8).
           02 CC-DATE-A PIC X(8).
           02 CC-DATE-P PIC X(8).
           02 CC-REC-STAT PIC X.
           02 CC-DX2 PIC X(7).
           02 CC-DX3 PIC X(7).
           02 CC-ACC-TYPE PIC X.
           02 CC-DATE-M PIC X(8).
           02 CC-ASSIGN PIC X.
           02 CC-NEIC-ASSIGN PIC X.
           02 CC-DX4 PIC X(7).
           02 CC-DX5 PIC X(7).
           02 CC-DX6 PIC X(7).
           02 CC-FUTURE PIC X(6).
       FD  PAYCUR
           BLOCK CONTAINS 6 RECORDS
           DATA RECORD IS PAYCUR01.
       01  PAYCUR01.
           02 PAYCUR-KEY.
             03 PC-KEY8 PIC X(8).
             03 PC-KEY3 PIC XXX.
           02 PC-AMOUNT PIC S9(4)V99.
           02 PC-PAYCODE PIC 999.
           02 PC-DENIAL PIC XX.
           02 PC-CLAIM PIC X(6).
           02 PC-DATE-T PIC 9(8).
           02 PC-DATE-E PIC X(8).
           02 PC-BATCH PIC X(6).
       FD GARFILE
           BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS G-MASTER.
       01 G-MASTER.
           02 G-GARNO PIC X(8).
           02 G-GARNAME PIC X(24).
           02 G-BILLADD PIC X(22).
           02 G-STREET PIC X(22).
           02 G-CITY PIC X(18).
           02 G-STATE PIC X(2).
           02 G-ZIP PIC X(9).
           02 G-COLLT PIC X.
           02 G-PHONE PIC X(10).
           02 G-SEX PIC X.
           02 G-RELATE PIC X.
           02 G-MSTAT PIC X.
           02 G-DOB PIC X(8).
           02 G-DUNNING PIC X.
           02 G-ACCTSTAT PIC X.
           02 G-PR-MPLR PIC X(4).
           02 G-PRINS PIC XXX.
           02 G-PR-ASSIGN PIC X.
           02 G-PR-OFFICE PIC X(4).
           02 G-PR-GROUP PIC X(12).
           02 G-PRIPOL PIC X(14).
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-SE-MPLR PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-SE-OFFICE PIC X(4).
           02 G-SE-GROUP PIC X(12).
           02 G-SECPOL PIC X(14).
           02 G-SENAME PIC X(24).
           02 G-SE-RELATE PIC X.
           02 G-INSPEND PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.
           02 G-ACCT PIC X(8).
           02 G-PRGRPNAME PIC X(15).
           02 G-SEGRPNAME PIC X(15).
       WORKING-STORAGE SECTION.
       01  AMT PIC S9(4)V99.
       01  NEWDATE PIC 9(8).
       01  HOLDGARNO PIC X(8) VALUE SPACE.
       PROCEDURE DIVISION.

       0005-START.

           OPEN INPUT GARFILE PAYCUR AGEDATE.
           OPEN I-O   CHARCUR OUTPUT FILEOUT.
           READ AGEDATE AT END DISPLAY "NO AGEDATE" GO TO P4.
           MOVE AGEDATE01 TO NEWDATE
           READ AGEDATE AT END DISPLAY "NO AGEDATE" GO TO P4.

       P1. 
           READ PAYCUR AT END  GO TO P4.

       P1-1.
      *     IF PC-DATE-T < AGEDATE01 GO TO P1.
           IF (PC-PAYCODE = 001 OR 021 OR 022 OR 062) 
           OR (PC-PAYCODE > 074 AND < 100)
           NEXT SENTENCE ELSE GO TO P1.
           COMPUTE AMT = -1 * PC-AMOUNT.
           IF AMT < 5 GO TO P1.
           IF PC-KEY8 NOT = HOLDGARNO
            MOVE PC-KEY8 TO HOLDGARNO
            MOVE PC-KEY8 TO G-GARNO
            READ GARFILE INVALID 
            DISPLAY G-GARNO GO TO P1
            END-READ
           END-IF.
           IF G-LASTBILL > PC-DATE-T GO TO P1.
           IF G-DUNNING > "3" GO TO P1.
           MOVE G-GARNO TO CC-KEY8 MOVE SPACE TO CC-KEY3
           START CHARCUR KEY > CHARCUR-KEY INVALID GO TO P1.

       P2.  
           READ CHARCUR NEXT WITH LOCK AT END GO TO P3.
           IF CC-KEY8 NOT = G-GARNO GO TO P3.
           IF CC-ASSIGN = "A" GO TO P2.
           IF CC-DATE-A = "00000000" GO TO P2.
           MOVE PAYCUR-KEY TO FO-2
           MOVE CHARCUR-KEY TO FO-1
           WRITE FILEOUT01
           MOVE NEWDATE TO CC-DATE-A
           REWRITE CHARCUR01
           GO TO P2.

       P3.
           READ PAYCUR AT END GO TO P4.
           IF PC-KEY8 NOT = G-GARNO GO TO P1-1.
           GO TO P3.

       P4.
           CLOSE CHARCUR.
           STOP RUN.
