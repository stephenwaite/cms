      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rri184.
       AUTHOR. S WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE IS MANUAL.
           SELECT PAYCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE IS MANUAL.
           SELECT PAYFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS IS RANDOM RECORD KEY IS PAYFILE-KEY
           LOCK MODE IS MANUAL.
           SELECT CHARCUR ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE IS MANUAL.
           SELECT FILEOUT ASSIGN TO "S50"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILEIN ASSIGN TO "S55"
           ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  GARFILE
      *    BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS G-MASTER.
       01  G-MASTER.
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
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
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
       FD  PAYFILE
      *    BLOCK CONTAINS 4 RECORDS
           DATA RECORD IS PAYFILE01.
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
       01  FILEOUT01 PIC X(11).
       FD  FILEIN.
       01  FILEIN01.
           02 FI-1 PIC X(11).
       WORKING-STORAGE SECTION.
       01  CLAIM-TOT PIC S9(4)V99.
       01  SAVEPAY PIC X(80).
       01  HOLD-KEY PIC X(11).
       01  XYZ PIC 999.
       01  FLAG PIC 9.
       01  ALF1 PIC X.
      *
       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT CHARCUR GARFILE FILEIN
           OPEN INPUT PAYCUR OUTPUT FILEOUT
           OPEN I-O PAYFILE
           MOVE SPACE TO PD-BATCH
           DISPLAY "PAYDATE T = TODAY   OR C = CHARGE DATE"
           ACCEPT ALF1
           IF NOT (ALF1 = "T" OR "C")
               DISPLAY "BAD DATE" 
               GO TO P2
           END-IF

           ACCEPT PD-DATE-T FROM CENTURY-DATE
           MOVE PD-DATE-T TO PD-DATE-E
           DISPLAY "PAYCODE 012 OR 013"
           ACCEPT PD-PAYCODE.
           IF NOT (PD-PAYCODE = "012" OR "013")
               DISPLAY "INVALID" 
               GO TO P2
           END-IF.    
       P1. 
           READ FILEIN AT END 
               GO TO P2
           END-READ    
           MOVE FI-1 TO CHARCUR-KEY
           READ CHARCUR INVALID 
               DISPLAY "BAD "
               GO TO P1
           END-READ

           MOVE CC-AMOUNT TO CLAIM-TOT
           PERFORM F1 THRU F1-EXIT
           MOVE CC-KEY8 TO G-GARNO
           READ GARFILE INVALID
               MOVE SPACE TO G-GARNAME
           END-READ    
           
           WRITE FILEOUT01 FROM CHARCUR-KEY.
           MOVE G-GARNAME TO PD-NAME
           
           IF CLAIM-TOT = 0
               GO TO P1
           END-IF

           COMPUTE PD-AMOUNT = -1 * CLAIM-TOT
           MOVE SPACE TO PD-DENIAL
           MOVE CC-CLAIM TO PD-CLAIM
           MOVE SPACE TO PAYFILE-KEY
           MOVE 0 TO XYZ
           MOVE CC-KEY8 TO PD-KEY8
           IF ALF1 = "C"
               MOVE CC-DATE-T TO PD-DATE-T 
           END-IF
           
           MOVE PAYFILE01 TO SAVEPAY.
       P5. 
           ADD 1 TO XYZ.
           MOVE XYZ TO PD-KEY3.
           MOVE PAYFILE-KEY TO HOLD-KEY.
           READ PAYFILE INVALID 
               GO TO P6
           END-READ    
           GO TO P5.
       P6. 
           MOVE SAVEPAY TO PAYFILE01.
           MOVE HOLD-KEY TO PAYFILE-KEY.
           ACCEPT PD-ORDER FROM TIME.
           WRITE PAYFILE01.
           GO TO P1.
       F1. 
           MOVE CC-KEY8 TO PC-KEY8.
           MOVE "000" TO PC-KEY3.
           MOVE 0 TO FLAG.
           START PAYCUR KEY > PAYCUR-KEY INVALID 
               GO TO F1-EXIT
           END-START.    
       F3. 
           READ PAYCUR NEXT AT END
               GO TO F1-EXIT
           END-READ

           IF PC-KEY8 NOT = CC-KEY8
               GO TO F1-EXIT
           END-IF

           IF PC-CLAIM NOT = CC-CLAIM
               GO TO F3
           END-IF

           ADD PC-AMOUNT TO CLAIM-TOT.
           GO TO F3.
       F1-EXIT. 
           EXIT.
       P2.
           CLOSE PAYFILE CHARCUR GARFILE.
           STOP RUN.
