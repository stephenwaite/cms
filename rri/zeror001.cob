      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3

       IDENTIFICATION DIVISION.
       PROGRAM-ID. zeror001.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT PAYCUR ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
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
           02 G-GARNAME.
             03 GN-1 PIC X.
             03 GN-2 PIC X(23).
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
      * 127     
           02 G-DUNNING PIC X.
           02 G-ACCTSTAT PIC 9.
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
           02 G-COPAY PIC S9(5)V99.
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
       01  CLAIM-TOT PIC S9(6)V99.
       01  GARBACK PIC X(315).              

       PROCEDURE DIVISION.

       P0.
           OPEN INPUT GARFILE.
           OPEN INPUT CHARCUR PAYCUR.

       R1.
           READ GARFILE NEXT
             AT END
               GO TO R99.

           MOVE 0 TO CLAIM-TOT
           MOVE G-GARNO TO PC-KEY8.
           MOVE SPACE TO PC-KEY3.
           START PAYCUR KEY NOT < PAYCUR-KEY
             INVALID
               GO TO R3.

       R2. 
           READ PAYCUR NEXT
             AT END
               GO TO R3.

           IF G-GARNO NOT = PC-KEY8
             GO TO R3.

           ADD PC-AMOUNT TO CLAIM-TOT
           GO TO R2.

       R3.
           MOVE G-GARNO TO CC-KEY8.
           MOVE SPACE TO CC-KEY3.
           START CHARCUR KEY NOT < CHARCUR-KEY
             INVALID
               GO TO R5.

       R4. 
           READ CHARCUR NEXT
             AT END
               GO TO R5.

           IF G-GARNO NOT = CC-KEY8 GO TO R5.

           ADD CC-AMOUNT TO CLAIM-TOT
           GO TO R4.

       R5.
           IF CLAIM-TOT NOT = 0
             MOVE "1" TO G-DELETE
             PERFORM R6
             GO TO R1
           END-IF  

           MOVE SPACE TO G-DELETE
           MOVE "1" TO G-DUNNING
      *     DISPLAY G-GARNO " " G-DELETE " " G-DUNNING
           PERFORM R6     
           GO TO R1.    

       R6.    
           MOVE GARFILE01 TO GARBACK
           CLOSE GARFILE           
           OPEN I-O GARFILE
           MOVE GARBACK(1:8) TO G-GARNO
           READ GARFILE WITH LOCK
             INVALID
               DISPLAY "COULD NOT READ GARFILE WITH LOCK"
               GO TO R1
           END-READ
           MOVE GARBACK TO GARFILE01
      *     DISPLAY G-GARNO " " G-DELETE " " G-DUNNING 
      *     DISPLAY " "
           
           REWRITE GARFILE01.
           CLOSE GARFILE
           OPEN INPUT GARFILE.
           MOVE GARBACK(1:8) TO G-GARNO
           START GARFILE KEY > G-GARNO
             INVALID 
               DISPLAY "LAST GARNO? " G-GARNO
      *         ACCEPT OMITTED
               GO TO R99.

       R99.
           CLOSE GARFILE CHARCUR PAYCUR 
           STOP RUN.
