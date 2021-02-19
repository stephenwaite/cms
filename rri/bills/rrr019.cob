      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.

       PROGRAM-ID. rrr019.

       AUTHOR. S WAITE.

       DATE-COMPILED. TODAY.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT BILLSORT ASSIGN TO "S25" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT BILLCOURT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT BILLBAD ASSIGN TO "S40" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT PAYCUR ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.

           SELECT CHARCUR ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.

           SELECT GARFILE ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
       FD  BILLSORT.
       01  BILLSORT01.
           02 BS-1 PIC X(8).
           02 BS-2 PIC X(24).
           02 BS-3 PIC X(9).
           02 BS-4 PIC X.
           02 BS-5 PIC S9(5)V99.
           02 BS-6 PIC XXX.
           02 BS-7 PIC 9(7).

       FD  FILEOUT.
       01  FILEOUT01.
           02 FO-1 PIC X(8).
           02 FO-2 PIC X(24).
           02 FO-3 PIC X(9).
           02 FO-4 PIC X.
           02 FO-5 PIC S9(5)V99.
           02 FO-6 PIC XXX.
           02 FO-7 PIC 9(7).
           
       FD  CHARCUR.
           copy charcur.cpy in "c:\users\sid\cms\copylib\rri".

       FD  BILLBAD.
       01  BILLBAD01.
           02 FILEBAD-KEY.
             03 BD-KEY8 PIC X(8).
             03 BD-KEY3 PIC XXX.
           02 BD-PATID PIC X(8).
           02 BD-CLAIM PIC X(6).
           02 BD-SERVICE PIC X.
           02 BD-DIAG PIC X(7).
           02 BD-PROC PIC X(11).
           02 BD-MOD2 PIC XX.
           02 BD-MOD3 PIC XX.
           02 BD-MOD4 PIC XX.
           02 BD-AMOUNT PIC S9(4)V99.
           02 BD-DOCR PIC X(3).
           02 BD-DOCP PIC X(2).
           02 BD-PAYCODE PIC XXX.
           02 BD-STUD PIC X.
           02 BD-WORK PIC XX.
           02 BD-DAT1 PIC X(8).
           02 BD-RESULT PIC X.
           02 BD-ACT PIC X.
           02 BD-SORCREF PIC X.
           02 BD-COLLT PIC X.
           02 BD-AGE PIC X.
           02 BD-PAPER PIC X.
           02 BD-PLACE PIC X.
           02 BD-EPSDT PIC X.
           02 BD-DATE-T PIC X(8).
           02 BD-DATE-A PIC X(8).
           02 BD-DATE-P PIC X(8).
           02 BD-REC-STAT PIC X.
           02 BD-DX2 PIC X(7).
           02 BD-DX3 PIC X(7).
           02 BD-ACC-TYPE PIC X.
           02 BD-DATE-M PIC X(8).
           02 BD-ASSIGN PIC X.
           02 BD-NEIC-ASSIGN PIC X.
           02 BD-DX4 PIC X(7).
           02 BD-DX5 PIC X(7).
           02 BD-DX6 PIC X(7).
           02 BD-FUTURE PIC X(6).

       FD  BILLCOURT.
       01  BILLCOURT01.
           02 FILECOURT-KEY.
             03 CR-KEY8 PIC X(8).
             03 CR-KEY3 PIC XXX.
           02 CR-PATID PIC X(8).
           02 CR-CLAIM PIC X(6).
           02 CR-SERVICE PIC X.
           02 CR-DIAG PIC X(7).
           02 CR-PROC PIC X(11).
           02 CR-MOD2 PIC XX.
           02 CR-MOD3 PIC XX.
           02 CR-MOD4 PIC XX.
           02 CR-AMOUNT PIC S9(4)V99.
           02 CR-DOCR PIC X(3).
           02 CR-DOCP PIC X(2).
           02 CR-PAYCODE PIC XXX.
           02 CR-STUD PIC X.
           02 CR-WORK PIC XX.
           02 CR-DAT1 PIC X(8).
           02 CR-RESULT PIC X.
           02 CR-ACT PIC X.
           02 CR-SORCREF PIC X.
           02 CR-COLLT PIC X.
           02 CR-AGE PIC X.
           02 CR-PAPER PIC X.
           02 CR-PLACE PIC X.
           02 CR-EPSDT PIC X.
           02 CR-DATE-T PIC X(8).
           02 CR-DATE-A PIC X(8).
           02 CR-DATE-P PIC X(8).
           02 CR-REC-STAT PIC X.
           02 CR-DX2 PIC X(7).
           02 CR-DX3 PIC X(7).
           02 CR-ACC-TYPE PIC X.
           02 CR-DATE-M PIC X(8).
           02 CR-ASSIGN PIC X.
           02 CR-NEIC-ASSIGN PIC X.
           02 CR-DX4 PIC X(7).
           02 CR-DX5 PIC X(7).
           02 CR-DX6 PIC X(7).
           02 CR-FUTURE PIC X(6).

       FD  PAYCUR.
           copy PAYcur.cpy in "c:\users\sid\cms\copylib\rri".

       FD  GARFILE.
           copy GARFILE.cpy in "c:\users\sid\cms\copylib\rri".

       WORKING-STORAGE SECTION.    
       01  PHR01.
           02 PHR02 OCCURS 990 TIMES.
             03 PHR-CLAIM PIC X(6).
             03 PHR-PAYCODE PIC XXX.
             03 PHR-AMOUNT PIC S9(4)V99.
             03 PHR-FLAG PIC 9.
       01  CHR01.
           02 CHR02 OCCURS 990 TIMES.
             03 CHR-KEY PIC X(11).
             03 CHR-BAL PIC S9(4)V99.
       01     PHR PIC 999.
       01     CHR PIC 999.
       01     Y PIC 999.
       01     CLAIM-TOT PIC S9(5)V99.
       01     BAL-FWD PIC S9(5)V99.

       PROCEDURE DIVISION.

       P0.
           OPEN OUTPUT FILEOUT BILLBAD BILLCOURT.
           OPEN INPUT GARFILE CHARCUR PAYCUR 
           OPEN INPUT BILLSORT.

       R1. 
           READ BILLSORT AT END GO TO R20.
           IF BS-4 NOT = "4" GO TO R1.
           MOVE BS-1 TO G-GARNO
           READ GARFILE INVALID DISPLAY "BAD " BS-1 GO TO R1.
           IF G-DUNNING NOT = "4" GO TO R1.

       R3. 
           MOVE 0 TO PHR CHR BAL-FWD
           MOVE G-GARNO TO PC-KEY8
           MOVE ZEROES TO PC-KEY3
           START PAYCUR KEY NOT < PAYCUR-KEY INVALID GO TO R1-1.

       R8. 
           READ PAYCUR NEXT AT END GO TO R1-1.
           IF G-GARNO NOT = PC-KEY8 GO TO R1-1.
           ADD 1 TO PHR.
           IF PHR > 990 DISPLAY G-GARNO " "  G-GARNAME
           GO TO R1.
           MOVE PC-CLAIM TO PHR-CLAIM(PHR)
           MOVE PC-PAYCODE TO PHR-PAYCODE(PHR)
           MOVE PC-AMOUNT TO PHR-AMOUNT(PHR)
           GO TO R8.

       R1-1.
           MOVE G-GARNO TO CC-KEY8.
           MOVE SPACE TO CC-KEY3.
           START CHARCUR KEY NOT < CHARCUR-KEY INVALID GO TO R7.

       R6. 
           READ CHARCUR NEXT AT END GO TO R7.
           IF G-GARNO NOT = CC-KEY8 GO TO R7.
           IF CC-ASSIGN = "A" GO TO R6.
           COMPUTE CLAIM-TOT = CC-AMOUNT
           PERFORM CC1 
           ADD CLAIM-TOT TO BAL-FWD
           GO TO R6.
            
       R7.
      *     WRITE OFF SMALL BALANCES AS BAD DEBT OR COURTESIES
      *     OR PASS THOUGH FOR PRINTING COLLECTIONS ACCOUNTS
           IF  (G-PRINS = "003" OR "028")
           OR (G-SEINS = "003" OR "028")
            PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > CHR
             MOVE CHR-KEY(Y) TO CHARCUR-KEY
             READ CHARCUR 
              INVALID 
               DISPLAY "BAD KEY ?? " CHARCUR-KEY
              NOT INVALID
               WRITE BILLCOURT01 FROM CHARCUR01
             END-READ
            END-PERFORM
           GO TO R1.
           
           IF BAL-FWD >  24.99 
            WRITE FILEOUT01 FROM BILLSORT01
            GO TO R1.
           PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > CHR
            MOVE CHR-KEY(Y) TO CHARCUR-KEY
            READ CHARCUR 
             INVALID 
              DISPLAY "BAD KEY ?? " CHARCUR-KEY
             NOT INVALID
              WRITE BILLBAD01 FROM CHARCUR01
            END-READ
           END-PERFORM
           GO TO R1.

       CC1.  
           PERFORM PH2 VARYING Y FROM 1 BY 1 UNTIL Y > PHR
      *     IF CLAIM-TOT > 0
           ADD 1 TO CHR
           COMPUTE CHR-BAL(CHR) = CLAIM-TOT
           MOVE CHARCUR-KEY TO CHR-KEY(CHR).
           
       PH2. 
           IF CC-CLAIM = PHR-CLAIM(Y)
           ADD PHR-AMOUNT(Y) TO CLAIM-TOT.
           
       R20. 
           CLOSE FILEOUT BILLBAD BILLCOURT. 
           STOP RUN.
