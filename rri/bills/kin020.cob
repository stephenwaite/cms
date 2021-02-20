      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. kin020.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PAYCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.

           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.

           SELECT BILLSORT ASSIGN TO "S40" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S45" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  BILLSORT.
       01  BILLSORT01.
           02 BS-1 PIC X(8).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(160).

       FD  CHARCUR.
           copy charcur.cpy in "c:\Users\sid\cms\copylib\rri".

       FD  PAYCUR.
           copy paycur.cpy in "c:\Users\sid\cms\copylib\rri".

       WORKING-STORAGE SECTION.    
       01  PHR01.
           02 PHR02 OCCURS 990 TIMES.
             03 PHR-CLAIM PIC X(6).
             03 PHR-PAYCODE PIC XXX.
             03 PHR-AMOUNT PIC S9(4)V99.
       01  PHR PIC 999.
       01  Y PIC 999.
       01  CLAIM-TOT PIC S9(5)V99.
       01  ALF1 PIC X.

       PROCEDURE DIVISION.

       P0.
           OPEN OUTPUT FILEOUT.
           OPEN INPUT BILLSORT CHARCUR PAYCUR.

       R1. 
           READ BILLSORT AT END GO TO R20.
           MOVE 0 TO PHR 
           MOVE BS-1 TO PC-KEY8
           MOVE SPACE TO PC-KEY3
           START PAYCUR KEY NOT < PAYCUR-KEY INVALID GO TO R1-1.
       R8. 
           READ PAYCUR NEXT AT END GO TO R1-1.
           IF BS-1 NOT = PC-KEY8 GO TO R1-1.
           ADD 1 TO PHR.
           IF PHR > 990 DISPLAY PC-KEY8
           GO TO R1.
           MOVE PC-CLAIM TO PHR-CLAIM(PHR)
           MOVE PC-PAYCODE TO PHR-PAYCODE(PHR)
           MOVE PC-AMOUNT TO PHR-AMOUNT(PHR)
           GO TO R8.
       R1-1.
           MOVE BS-1 TO CC-KEY8.
           MOVE SPACE TO CC-KEY3.
           START CHARCUR KEY NOT < CHARCUR-KEY INVALID GO TO R1.
       R6. 
           READ CHARCUR NEXT AT END GO TO R1.
           IF BS-1 NOT = CC-KEY8 GO TO R1.
           IF CC-ASSIGN = "A" GO TO R6.
           IF CC-COLLT = "1"
           DISPLAY CHARCUR-KEY
      *     ACCEPT ALF1
           GO TO R6
           END-IF
           COMPUTE CLAIM-TOT = CC-AMOUNT
           PERFORM PH2
           IF CLAIM-TOT > 0
           WRITE FILEOUT01 FROM CHARCUR01.
           GO TO R6.
       PH2. 
           IF CC-CLAIM = PHR-CLAIM(Y)
           ADD PHR-AMOUNT(Y) TO CLAIM-TOT.
       R20. 
           CLOSE FILEOUT.
           STOP RUN.
