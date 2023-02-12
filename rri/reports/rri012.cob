      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rri012.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AGEDATE ASSIGN TO "S30"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT FILEOUT ASSIGN TO "S40"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT PARMOUT ASSIGN TO "S45"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT DOCFILE ASSIGN TO "S50"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT GARFILE ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT PATFILE ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM RECORD KEY IS P-PATNO
           ALTERNATE RECORD KEY IS P-GARNO WITH DUPLICATES
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

       FD PATFILE
           BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS P-MASTER.
       01 P-MASTER.
           02 P-PATNO PIC X(8).
           02 P-GARNO PIC X(8).
           02 P-PATNAME PIC X(24).
           02 P-SEX PIC X.
           02 P-RELATE PIC X.
           02 P-MSTAT PIC X.
           02 P-DOB PIC X(8).
       FD  DOCFILE.
       01  DOCFILE01.
           02 DF1 PIC X.
           02 DF2 PIC X.
           02 DF3 PIC X(22).
       FD  AGEDATE.
       01  AGEDATE01.
           02 LOWDATE PIC X(8).
           02 HIGHDATE PIC X(8).
       FD  PARMOUT
           DATA RECORD IS PARMOUT01.
       01  PARMOUT01 PIC X(16).
       FD  CHARCUR
           BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS CHARCUR01.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID.
             03 CC-PATID1 PIC X(7).
             03 CC-PATID8 PIC X.
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC. 
              03 CC-PROC1 PIC XXXX.
              03 CC-PROC2 PIC X(5).
              03 CC-PROC3 PIC XX.
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
       FD FILEOUT
           DATA RECORD IS FILEOUT01.
       01  FILEOUT01.
           02 FO-DOCP PIC XX.
           02 FO-DUM PIC X.
           02 FO-SERVICE PIC X.
           02 F-PROC PIC X(11).
           02 F-AMOUNT PIC 9(4)V99.
           02 F-IO PIC X.
           02 FO-NAME PIC X(24).
           02 FO-DATE PIC X(8).
       WORKING-STORAGE SECTION.
       01  PLACE-TAB01.
           02 PLACE-TAB OCCURS 26 TIMES.
             03 PL-TAB PIC X.
             03 PL-NUM PIC X.
             03 PL-NAME PIC X(22).
       01 MON-TAB-RE01.
           02 FILLER PIC X(27) VALUE "JANUARY  FEBRUARY MARCH    ".
           02 FILLER PIC X(27) VALUE "APRIL    MAY      JUNE     ".
           02 FILLER PIC X(27) VALUE "JULY     AUGUST   SEPTEMBER".
           02 FILER PIC X(27) VALUE "OCTOBER  NOVEMBER DECEMBER ".
       01  MON-TAB01 REDEFINES MON-TAB-RE01.
           02 MON-TAB PIC X(9) OCCURS 12 TIMES.
       01  PLINDX PIC 99 VALUE 0.
       01  LOW-CLAIM PIC X(6).
       01  HIGH-CLAIM PIC X(6).
       01  X PIC 99.
       01  Y PIC 99.
       01  CC-PL PIC X.
       01  PROC-DEPT PIC XX.
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT DOCFILE GARFILE PATFILE.
           READ DOCFILE AT END GO TO P00.
       P00. READ DOCFILE AT END GO TO P000.
           ADD 1 TO PLINDX.
           MOVE DF1 TO PL-TAB(PLINDX)
           MOVE DF2 TO PL-NUM(PLINDX)
           MOVE DF3 TO PL-NAME(PLINDX).
           GO TO P00.
       P000.
           OPEN INPUT AGEDATE CHARCUR OUTPUT FILEOUT.
           READ AGEDATE.
           OPEN OUTPUT PARMOUT.
           MOVE AGEDATE01 TO PARMOUT01.
           WRITE PARMOUT01. CLOSE PARMOUT.
       P1. READ CHARCUR AT END GO TO P3.
      *    IF NOT ( CC-PLACE = "C" OR "M" OR "R") GO TO P1.
           IF CC-DATE-T < LOWDATE OR > HIGHDATE GO TO P1.
      *     IF NOT ( CC-PROC2 = "77057" OR "G0202") GO TO P1.
      *     IF NOT ( CC-DIAG = "V7611" OR "V7612") GO TO P1.

      *     IF (CC-PROC1 < "8000" OR > "8099") GO TO P1.
           MOVE "01" TO PROC-DEPT.
       P2.
           PERFORM DF-SEARCH.
           IF CC-SERVICE < "1" OR > "7" MOVE "4" TO FO-SERVICE
           ELSE MOVE CC-SERVICE TO FO-SERVICE.
           PERFORM B1
      *     IF G-PRINS NOT = "003" GO  TO P1.
           MOVE "1" TO FO-DUM.
           MOVE "01" TO FO-DOCP
      *     MOVE CC-DOCP TO FO-DOCP
           MOVE CC-PROC TO F-PROC 
           MOVE CC-AMOUNT TO F-AMOUNT
      *     MOVE 0 TO F-AMOUNT
           MOVE CC-DATE-T TO FO-DATE
           WRITE FILEOUT01
           GO TO P1.
       DF-SEARCH. MOVE "2" TO F-IO.
           PERFORM DF-SEARCH2 VARYING Y FROM 1 BY 1 UNTIL Y > PLINDX.
       DF-SEARCH2. IF CC-PLACE = PL-TAB(Y) AND PL-NUM(Y) = "3"
           MOVE "1" TO F-IO.
           IF CC-PLACE = PL-TAB(Y)
           MOVE PLINDX TO Y.
       B1. MOVE CC-PATID TO G-GARNO
           READ GARFILE INVALID MOVE SPACE TO G-GARNAME.
           MOVE G-GARNAME TO FO-NAME.
       B2. MOVE CC-PATID TO P-PATNO
           READ PATFILE INVALID MOVE SPACE TO P-PATNAME.
           MOVE P-PATNAME TO FO-NAME.
       P3. CLOSE FILEOUT.
           STOP RUN.
