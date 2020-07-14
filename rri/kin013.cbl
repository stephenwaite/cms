      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. kin013.
       AUTHOR. S WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PAYFILE-KEY
           LOCK MODE MANUAL.
           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT PAYCUR ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.
           SELECT GARFILE ASSIGN TO "S45"     ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT ERRFILE ASSIGN TO "S50" ORGANIZATION
           LINE SEQUENTIAL.        

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
           02 CC-PROC.
              03 CC-PROC1 PIC X(4).
              03 CC-PROC2 PIC X(7).
           02 CC-MOD2 PIC XX.
           02 CC-MOD3 PIC XX.
           02 CC-MOD4 PIC XX.
           02 CC-AMOUNT PIC S9(4)V99.
           02 CC-DOCR PIC X(3).
           02 CC-DOCP PIC 9(2).
           02 CC-PAYCODE PIC 999.
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
           02 CC-IOPAT PIC X.
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

       FD  ERRFILE.
       01  ERRFILE01 PIC X(160).
                         
       WORKING-STORAGE SECTION.
       01 TOTCLAIM PIC S9(4)V99.
       01 SAVEPAYFILEO1 PIC X(80).
       01 XYZ PIC 999.

       PROCEDURE DIVISION.
       P0.
           OPEN OUTPUT ERRFILE.
           OPEN INPUT PAYCUR CHARCUR GARFILE.
           OPEN I-O PAYFILE.
       P1. 
           READ CHARCUR NEXT AT END 
               GO TO P6 
           END-READ

           IF CC-PAYCODE NOT = "018"
               GO TO P1 
           END-IF

           IF CC-DATE-T > "20130630"
               GO TO P1 
           END-IF

           PERFORM S4 THRU S4-EXIT.
           
           IF TOTCLAIM = 0 
               GO TO P1
           END-IF

           IF TOTCLAIM < 0 
               MOVE CHARCUR01 TO ERRFILE01
               WRITE ERRFILE01
               GO TO P1
           END-IF            

      *    TIME TO WRITE OFF WITH BAD DEBT, 013
           MOVE CC-KEY8 TO PD-KEY8 G-GARNO
           READ GARFILE INVALID
               MOVE SPACE TO G-GARNAME
           END-READ

           MOVE G-GARNAME TO PD-NAME
           MOVE TOTCLAIM TO PD-AMOUNT
           MOVE "013" TO PD-PAYCODE
           MOVE "AA" TO PD-DENIAL
           MOVE CC-CLAIM TO PD-CLAIM
           MOVE CC-DATE-T TO PD-DATE-T PD-DATE-E
           MOVE SPACE TO PD-ORDER PD-BATCH           
           MOVE 0 TO XYZ
           MOVE PAYFILE01 TO SAVEPAYFILEO1.
       WH13.
           ADD 1 TO XYZ
           MOVE XYZ TO PD-KEY3
           READ PAYFILE INVALID 
               GO TO WH14
           END-READ

           IF XYZ = 999
               DISPLAY "TOO MANY RECORDS CALL CMS"
               DISPLAY CHARCUR-KEY    
               ACCEPT OMITTED
               GO TO P1 
           ELSE
               GO TO WH13
           END-IF.
       WH14.
           MOVE SAVEPAYFILEO1 TO PAYFILE01
           MOVE XYZ TO PD-KEY3
           WRITE PAYFILE01
           GO TO P1.

       S4. 
           MOVE CC-AMOUNT TO TOTCLAIM
           MOVE CC-KEY8 TO PC-KEY8.
           MOVE "000" TO PC-KEY3.
           START PAYCUR KEY NOT < PAYCUR-KEY INVALID 
               GO TO S4-EXIT
           END-START.    
       S7. 
           READ PAYCUR NEXT AT END 
               GO TO S4-EXIT
           END-READ

           IF PC-KEY8 NOT = CC-KEY8 
               GO TO S4-EXIT
           END-IF

           IF PC-CLAIM NOT = CC-CLAIM
               GO TO S7
           END-IF

           ADD PC-AMOUNT TO TOTCLAIM 
           GO TO S7.
       S4-EXIT.
           EXIT.
       P6. 
           CLOSE ERRFILE PAYCUR CHARCUR GARFILE PAYFILE.
           STOP RUN.
              
