      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HVRCN08.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO "S25"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT PAYCUR ASSIGN  TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.
           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT FILEOUT ASSIGN TO "S40"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT GARFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC  RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT PAYFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC  RECORD KEY IS PAYFILE-KEY
           LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PAYFILE.
       01  PAYFILE01.
           02 PAYFILE-KEY.
             03 PD-KEY8 PIC X(8).
             03 PD-KEY3 PIC XXX.
           02 PD-NAME PIC X(24).
           02 PD-AMOUNT PIC S9(4)V99.
           02 PD-PAYCODE PIC XXX.
           02 PD-DENIAL PIC XX.
           02 PD-CLAIM PIC X(6).
           02 PD-DATE-T PIC X(8).
           02 PD-DATE-E PIC X(8).
           02 PD-ORDER PIC X(6).
           02 PD-BATCH PIC X(6).
       FD  FILEIN.
       01  FI-1 PIC X(11).
       FD GARFILE.
       01 G-MASTER.
           02 G-GARNO PIC X(8).
           02 G-GARNAME PIC X(24).
           02 G-BILLADD PIC X(22).
           02 G-STREET PIC X(22).
           02 G-CITY PIC X(18).
           02 G-STATE PIC X(2).
           02 G-ZIP PIC X(9).
           02 G-COLLT PIC X.
           02 G-PHONE. 
              03 G-PHONE1 PIC XXX.
              03 G-PHONE2 PIC XXX.
              03 G-PHONE3 PIC XXXX.
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
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
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
      *    BLOCK CONTAINS 3 RECORDS
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
       FD  FILEOUT.
       01  FILEOUT01 PIC X(160).
       WORKING-STORAGE SECTION.
       01 TOT-CLAIM PIC 9(4)V99.
       01  AMT0FLAG PIC 9.
       PROCEDURE DIVISION.
       P0. OPEN INPUT FILEIN CHARCUR PAYCUR GARFILE PAYFILE.
           OPEN OUTPUT FILEOUT.
       P1. 
           READ FILEIN AT END GO TO P99.
           MOVE FI-1 TO CHARCUR-KEY
           READ CHARCUR INVALID 
              DISPLAY "BADKEY  " FI-1
              ACCEPT OMITTED
              GO TO P1
           END-READ.

           MOVE CC-KEY8 TO G-GARNO
           READ GARFILE INVALID
              DISPLAY "BAD GARNO  " CC-KEY8
              ACCEPT OMITTED
              GO TO P1
           END-READ  
           MOVE CC-KEY8 TO PD-KEY8
           MOVE SPACE TO PD-KEY3
           START PAYFILE KEY NOT < PAYFILE-KEY INVALID
              DISPLAY "NO PAYFILE PAYMENTS " CC-KEY8
              ACCEPT OMITTED
              GO TO P1
           END-START
           COMPUTE TOT-CLAIM = CC-AMOUNT.
           MOVE 0 TO AMT0FLAG.
       P2. READ PAYFILE NEXT AT END GO TO P3.
           IF PD-KEY8 NOT = CC-KEY8 GO TO P3.
           IF PD-CLAIM NOT = CC-CLAIM GO TO P2.
           IF PD-AMOUNT = 0  
              MOVE 1 TO AMT0FLAG
           END-IF
           IF PD-DENIAL = "14"
              MOVE 0 TO AMT0FLAG
           END-IF
           IF PD-PAYCODE = "001" 
               WRITE FILEOUT01 FROM CHARCUR01
               GO TO P1
           END-IF    
               
           IF (PD-PAYCODE NOT = G-PRINS)
              AND
              (PD-PAYCODE NOT = G-SEINS)
              AND
              (PD-PAYCODE NOT = G-SEINS)
               WRITE FILEOUT01 FROM CHARCUR01
               GO TO P1
           END-IF
           COMPUTE TOT-CLAIM = TOT-CLAIM + PD-AMOUNT
           GO TO P2.           
       P3.
           IF AMT0FLAG = 1
               WRITE FILEOUT01 FROM CHARCUR01
               GO TO P1
           END-IF     
           MOVE CC-KEY8 TO PC-KEY8
           MOVE SPACE TO PC-KEY3
           START PAYCUR KEY NOT < PAYCUR-KEY INVALID
              GO TO P5
           END-START.
           
       P4. READ PAYCUR NEXT AT END GO TO P5.
           IF PC-KEY8 NOT = CC-KEY8 GO TO P5.
           IF PC-CLAIM NOT = CC-CLAIM GO TO P4.
           COMPUTE TOT-CLAIM = TOT-CLAIM + PC-AMOUNT
           GO TO P5.           
       
       P5.
           IF TOT-CLAIM < 0
             WRITE FILEOUT01 FROM CHARCUR01
           END-IF
           GO TO P1.
       P99. CLOSE FILEOUT GARFILE CHARCUR PAYCUR.
           STOP RUN.
