      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrix024.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AGEDATE ASSIGN TO "S30"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT PAYCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.
           SELECT CHARCUR ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT FILEOUT2 ASSIGN TO "S45"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT GARFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
       FD GARFILE
           BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS GARFILE01.
       01 GARFILE01.
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
           02 G-PR-GROUP PIC X(10).
           02 G-PRIPOL PIC X(16).
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-SE-MPLR PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-SE-OFFICE PIC X(4).
           02 G-SE-GROUP PIC X(10).
           02 G-SECPOL PIC X(16).
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

       FD  FILEOUT2.
       01  FILEOUT201.
           02 FO-1 PIC XX.
           02 FO-2 PIC X(8).
           02 FO-3 PIC XXX.
           02 FO-4 PIC S9(4)V99.
           02 FO-5 PIC X(24).
           02 FO-6 PIC X(8).
           02 FO-7 PIC X(11).
           02 FO-8 PIC X(8).
           02 FO-9 PIC XX.
           02 FO-10 PIC X(8).
           02 FO-CHARGE PIC S9(4)V99.
           02 FO-KEY PIC X(11).
           02 FO-GROUP PIC X(10).
       FD  CHARCUR
           BLOCK CONTAINS 5 RECORDS
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
           02 CC-AGE PIC X.
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
       FD  AGEDATE
           DATA RECORD IS AGEDATE01.
       01  AGEDATE01.
           02 AGD1LOW PIC X(8).
           02 AGD2HIGH PIC X(8).
       FD  PAYCUR
           BLOCK CONTAINS 6 RECORDS
           DATA RECORD IS PAYCUR01.
       01  PAYCUR01.
           02 PAYCUR-KEY.
             03 PC-KEY8 PIC X(8).
             03 PC-KEY3 PIC XXX.
           02 PC-AMOUNT PIC S9(4)V99.
           02 PC-PAYCODE PIC XXX.
           02 PC-DENIAL PIC XX.
           02 PC-CLAIM PIC X(6).
           02 PC-DATE-T PIC X(8).
           02 PC-DATE-E PIC X(8).
           02 PC-BATCH PIC X(6).
       WORKING-STORAGE SECTION.
       01  NUM6 PIC 9(6).
       01 TOT-AMOUNT PIC S9(4)V99.
       PROCEDURE DIVISION.
       P00.
           OPEN INPUT PAYCUR AGEDATE CHARCUR GARFILE.
           OPEN OUTPUT FILEOUT2.
           READ AGEDATE AT END GO TO P2.
           
       P1. READ PAYCUR AT END GO TO P2.
           IF PC-DATE-T < AGD1LOW GO TO P1.
           IF PC-DENIAL = "14" OR "15"
             GO TO P1
           END-IF
           IF PC-PAYCODE = "010" OR "011" OR "012" OR "013"
                       OR "014" OR "015" GO TO P1.
           
           MOVE PC-KEY8 TO G-GARNO
           READ GARFILE INVALID GO TO P1.
           PERFORM Z1 THRU Z1-EXIT
           GO TO P1.
       Z1. MOVE PC-KEY8 TO CC-KEY8
           MOVE "000" TO CC-KEY3
           START CHARCUR KEY > CHARCUR-KEY INVALID GO TO Z1-EXIT.
       Z2. READ CHARCUR NEXT AT END GO TO Z1-EXIT.
           IF CC-DATE-T > AGD2HIGH OR < AGD1LOW GO TO Z2.
           IF CC-AMOUNT = 0 GO TO Z2.
           IF CC-KEY8 NOT = PC-KEY8 GO TO Z1-EXIT.
           IF CC-CLAIM NOT = PC-CLAIM GO TO Z2.
           COMPUTE TOT-AMOUNT = -1 * PC-AMOUNT
           IF CC-AMOUNT = TOT-AMOUNT GO TO Z1-EXIT.
           MOVE CC-KEY8 TO G-GARNO
           MOVE "01" TO FO-1
           MOVE PC-DATE-T TO FO-2
           MOVE PC-PAYCODE TO FO-3
           MOVE PC-AMOUNT TO FO-4
           MOVE G-PRGRPNAME TO FO-5
           MOVE G-PR-GROUP TO FO-GROUP
           MOVE CC-DATE-T TO FO-6
           MOVE CC-PROC TO FO-7
           MOVE CC-AMOUNT TO FO-CHARGE
           MOVE PC-DATE-E TO FO-8
           MOVE PC-DENIAL TO FO-9
           MOVE PC-DATE-E TO FO-10
           MOVE CHARCUR-KEY TO FO-KEY
           WRITE FILEOUT201.
       Z1-EXIT. EXIT.
       P2.
           CLOSE FILEOUT2. STOP RUN.
