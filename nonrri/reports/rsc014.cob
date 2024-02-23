      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SID014.
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
           ACCESS MODE IS RANDOM RECORD KEY IS G-GARNO.
           SELECT PATFILE ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM RECORD KEY IS P-PATNO
           ALTERNATE RECORD KEY IS P-GARNO WITH DUPLICATES.
           SELECT PAYCUR ASSIGN TO "S65" ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL RECORD KEY IS PAYCUR-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD  PAYCUR.
           COPY "paycur.cpy" IN "C:\Users\sid\cms\copylib".

       FD  GARFILE.
           COPY "garfile.cpy" IN "C:\Users\sid\cms\copylib".
       
       FD  PATFILE.
           COPY "patfile.cpy" IN "C:\Users\sid\cms\copylib".
       
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
       FD  CHARCUR.
           COPY "charcur.cpy" IN "C:\Users\sid\cms\copylib".
       
       FD FILEOUT
           DATA RECORD IS FILEOUT01.
       01  FILEOUT01.
           02 FO-DOCP PIC XX.
           02 FO-DUM PIC X.
           02 FO-SERVICE PIC X.
           02 F-PROC PIC X(7).
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
           OPEN INPUT AGEDATE CHARCUR PAYCUR OUTPUT FILEOUT.
           READ AGEDATE.
           OPEN OUTPUT PARMOUT.
           MOVE AGEDATE01 TO PARMOUT01.
           WRITE PARMOUT01. CLOSE PARMOUT.
       P1. READ PAYCUR AT END GO TO P2.
           IF (PC-PAYCODE = "007" OR "008" OR "009" OR "011" 
           OR "012" OR "013" OR "014" OR "015" OR "016" OR "017" ) 
           OR (PC-DENIAL = "DI" OR "15" OR "14")
           GO TO P1.
           IF PC-DATE-E < LOW-DATE OR > HIGH-DATE GO TO P1.
           PERFORM Z1 THRU Z1-EXIT
           PERFORM DF-SEARCH.
           IF CC-SERVICE < "1" OR > "7" MOVE "4" TO FO-SERVICE
           ELSE MOVE CC-SERVICE TO FO-SERVICE.
           IF CC-PATID8 = "G" PERFORM B1
           ELSE PERFORM B2.
           MOVE "1" TO FO-DUM.
           MOVE "01" TO FO-DOCP
           MOVE CC-PROC(1:5) TO F-PROC MOVE PC-AMOUNT TO F-AMOUNT
           MOVE PC-DATE-T TO FO-DATE
           MOVE "01" TO FO-DOCP
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
       Z1. MOVE PC-KEY8 TO CC-KEY8
           MOVE SPACE TO CC-KEY3
           START CHARCUR KEY > CHARCUR-KEY INVALID GO TO Z1-EXIT.
       Z2. READ CHARCUR NEXT AT END GO TO Z1-EXIT.
           IF CC-KEY8 NOT = PC-KEY8 GO TO Z1-EXIT.
           IF CC-CLAIM NOT = PC-CLAIM GO TO Z2.
       Z1-EXIT. EXIT.
       P2. CLOSE FILEOUT.
           STOP RUN.
