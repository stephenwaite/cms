      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rripl014.
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

           SELECT FILEOUT ASSIGN TO "S40"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT PARMOUT ASSIGN TO "S45"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT DOCFILE ASSIGN TO "S50"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT GARFILE ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES.

           SELECT PATFILE ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM RECORD KEY IS P-PATNO
           ALTERNATE RECORD KEY IS P-GARNO WITH DUPLICATES.

           SELECT PAYCUR ASSIGN TO "S65" ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL RECORD KEY IS PAYCUR-KEY.

       DATA DIVISION.
       FILE SECTION.
       FD  PAYCUR
           BLOCK CONTAINS 3 RECORDS
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
       FD  AGEDATE
           DATA RECORD IS AGEDATE01.
       01  AGEDATE01. 
           02 LOW-DATE PIC X(8).
           02 HIGH-DATE PIC X(8).
       FD  PARMOUT.
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
       FD FILEOUT
           DATA RECORD IS FILEOUT01.
       01  FILEOUT01.
           02 FO-DOCP PIC XX.
           02 FO-DUM PIC X.
           02 FO-SERVICE PIC X.
           02 F-PROC PIC X(11).
           02 F-AMOUNT PIC S9(4)V99.
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
       01 MON-TAB01 REDEFINES MON-TAB-RE01.
           02 MON-TAB PIC X(9) OCCURS 12 TIMES.
     
       01  PLINDX PIC 99 VALUE 0.
       01  LOW-CLAIM PIC X(6).
       01  HIGH-CLAIM PIC X(6).
       01  X PIC 99.
       01  Y PIC 99.
       01  CC-PL PIC X.
       01  FLAG PIC 9.
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT DOCFILE GARFILE PATFILE.
           READ DOCFILE AT END GO TO P00.

       P00. 
           READ DOCFILE AT END GO TO P000.
           ADD 1 TO PLINDX.
           MOVE DF1 TO PL-TAB(PLINDX)
           MOVE DF2 TO PL-NUM(PLINDX)
           MOVE DF3 TO PL-NAME(PLINDX).
           GO TO P00.

       P000.
           OPEN INPUT AGEDATE CHARCUR PAYCUR OUTPUT FILEOUT.
           READ AGEDATE.
           OPEN OUTPUT PARMOUT.
           MOVE AGEDATE01 TO PARMOUT01.
           WRITE PARMOUT01.
           CLOSE PARMOUT.

       P1. 
           READ PAYCUR AT END GO TO P2.

           IF (PC-PAYCODE = "007" OR "008" OR "009" OR "011" 
             OR "012" OR "013" OR "014" OR "015" OR "016" OR "017" ) 
             OR (PC-DENIAL = "DI" OR "15" OR "14")
             GO TO P1
           END-IF

           IF PC-DATE-T < LOW-DATE OR > HIGH-DATE GO TO P1.

           PERFORM Z1 THRU Z1-EXIT

           PERFORM B1

           MOVE "1" TO FO-SERVICE.
           MOVE "1" TO FO-DUM.
           MOVE "1" TO F-IO.
           MOVE "01" TO FO-DOCP
           MOVE CC-PROC TO F-PROC
           MOVE PC-AMOUNT TO F-AMOUNT
           MOVE PC-DATE-T TO FO-DATE
           MOVE G-GARNAME TO FO-NAME
           WRITE FILEOUT01
           GO TO P1.

       DF-SEARCH. 
           MOVE "2" TO F-IO.
           PERFORM DF-SEARCH2 VARYING Y FROM 1 BY 1 UNTIL Y > PLINDX.

       DF-SEARCH2. 
           IF CC-PLACE = PL-TAB(Y) AND PL-NUM(Y) = "3"
           MOVE "1" TO F-IO.
           
           IF CC-PLACE = PL-TAB(Y)
           MOVE PLINDX TO Y.

       B1. 
           MOVE CC-PATID TO G-GARNO
           READ GARFILE INVALID MOVE SPACE TO G-GARNAME.

       B2. 
           MOVE CC-PATID TO P-PATNO
           READ PATFILE INVALID MOVE SPACE TO P-PATNAME.
           MOVE P-PATNAME TO FO-NAME.

       Z1.
           MOVE PC-KEY8 TO CC-KEY8
           MOVE SPACE TO CC-KEY3
           START CHARCUR KEY > CHARCUR-KEY INVALID GO TO Z1-EXIT.

       Z2. 
           READ CHARCUR NEXT AT END GO TO Z1-EXIT.

           IF CC-KEY8 NOT = PC-KEY8 GO TO Z1-EXIT.

           IF CC-CLAIM NOT = PC-CLAIM GO TO Z2.
           
           IF CC-DATE-T < "20200101" OR > "20201130"
             GO TO Z2.           

       Z1-EXIT. 
           EXIT.

       P2. 
           CLOSE FILEOUT.
           STOP RUN.
