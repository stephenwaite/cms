      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. kin011.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT KINFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS KINFILE-KEY
           ALTERNATE RECORD KEY IS KIN-DATE-E WITH DUPLICATES
           ALTERNATE RECORD KEY IS KIN-STAT WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT INSFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT PAYCUR ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.
           SELECT CHARCUR ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
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
              03 CC-PROC0 PIC X(4).
              03 CC-PROC1 PIC X(5).
              03 CC-PROC2 PIC XX.
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
       FD  INSFILE
     *     BLOCK CONTAINS 6 RECORDS
           DATA RECORD IS INSFILE01.
       01  INSFILE01.
           02 INS-KEY PIC XXX.
           02 INS-NAME PIC X(22).
           02 INS-STREET PIC X(24).
           02 INS-CITY PIC X(15).
           02 INS-STATE PIC XX.
           02 INS-ZIP PIC X(9).
           02 INS-ASSIGN PIC X.
           02 INS-CLAIMTYPE PIC X.
           02 INS-NEIC PIC X(5).
           02 INS-NEICLEVEL PIC X.
           02 INS-NEIC-ASSIGN PIC X.
           02 INS-PPO PIC X.
           02 INS-PRVNUM PIC X(10).
           02 INS-HMO PIC X(3).
           02 INS-STATUS PIC X.
           02 INS-LEVEL PIC X.
           02 INS-LASTDATE PIC X(8).
           02 INS-CAID PIC XXX.
           02 INS-REFWARN PIC X.
           02 INS-FUTURE PIC X(8).
       FD  FILEOUT.
       01  FILEOUT01.
           02 FO-ACCT PIC X(8).
           02 FILLER PIC X VALUE "*".
           02 FO-NAME PIC X(24).
           02 FILLER PIC X VALUE "*".
           02 FO-PAYCODE PIC XXX.
           02 FILLER PIC X VALUE "*".
           02 FO-INSNAME PIC X(28).
           02 FILLER PIC X VALUE "*".
           02 FO-DATE PIC X(10).
           02 FILLER PIC X VALUE "*".
           02 FO-AMOUNT PIC X(10).
           02 FILLER PIC X VALUE "*".
           02 FO-REDUCT PIC X(10).
           02 FILLER PIC X VALUE "*".
           02 FO-PIF PIC XXX.
       FD  KINFILE.
       01  KINFILE01.
           02 KINFILE-KEY.
             03 KIN-KEY8 PIC X(8).
             03 KIN-KEY3 PIC XXX.
           02 KIN-NAME PIC X(24).
           02 KIN-AMOUNT PIC S9(4)V99.
           02 KIN-PAYCODE PIC XXX.
           02 KIN-DENIAL PIC XX.
           02 KIN-DATE-T PIC X(8).
           02 KIN-DATE-E PIC X(8).
           02 KIN-STAT PIC X.
           02 KIN-CHARCUR-KEY PIC X(11).
       WORKING-STORAGE SECTION.
       01  X PIC 99.
       01  Y PIC 99.
       01  Z PIC 99.
       01  A PIC 99.
       01  ALF6.
           02 ALF4 PIC X(4).
           02 ALF2 PIC XX.
       01  PAYDATE PIC X(8).
       01  XYZ PIC 999.
       01  NUM6 PIC 9(6).
       01  KINBACK01 PIC X(73).
       01  ALF1 PIC X.
       01  NEF-6 PIC Z,ZZ9.99CR.
       01  TOT-CLAIM PIC S9(4)V99.
       01  REDUCT PIC S9(4)V99.

       PROCEDURE DIVISION.

       0005-START.
           OPEN I-O KINFILE
           OPEN INPUT INSFILE CHARCUR PAYCUR
           OPEN OUTPUT FILEOUT.

           MOVE "ACCT" TO FO-ACCT
           MOVE "NAME" TO FO-NAME
           MOVE SPACE TO FO-PAYCODE
           MOVE "PAYOR" TO FO-INSNAME
           MOVE "DATE" TO FO-DATE
           MOVE "PAID" TO FO-AMOUNT
           MOVE "WRITE-OFF" TO FO-REDUCT
           MOVE "BAL" TO FO-PIF
           WRITE FILEOUT01.

           MOVE "0" TO KIN-STAT
           START KINFILE KEY NOT <  KIN-STAT
             INVALID
               GO TO P9.

       P1.
           READ KINFILE NEXT WITH LOCK
             AT END
               GO TO P9.

           IF KIN-STAT NOT = "0" GO TO P9.

           MOVE KIN-KEY8 TO FO-ACCT
           MOVE KIN-NAME TO FO-NAME
           MOVE KIN-PAYCODE TO FO-PAYCODE
           MOVE KIN-PAYCODE TO INS-KEY

           READ INSFILE
             INVALID
               MOVE SPACE TO INS-NAME
           END-READ

           MOVE INS-NAME TO FO-INSNAME
           MOVE KIN-DATE-T TO FO-DATE
           MOVE KIN-AMOUNT TO NEF-6
           MOVE NEF-6 TO FO-AMOUNT
           MOVE SPACE TO FO-PIF

           PERFORM PAID-1 THRU PAID-1-EXIT

           MOVE REDUCT TO NEF-6
           MOVE NEF-6 TO FO-REDUCT
           WRITE FILEOUT01

           MOVE "1" TO KIN-STAT
           ACCEPT KIN-DATE-E FROM CENTURY-DATE.
           REWRITE KINFILE01
           GO TO P1.

       PAID-1.

           MOVE KIN-CHARCUR-KEY TO CHARCUR-KEY
           READ CHARCUR
             INVALID
               GO TO PAID-1-EXIT.

           MOVE CC-KEY8 TO PC-KEY8
           MOVE SPACE TO PC-KEY3
           START PAYCUR KEY NOT < PAYCUR-KEY
             INVALID
               GO TO PAID-1-EXIT.

           MOVE CC-AMOUNT TO TOT-CLAIM.
           MOVE 0 to REDUCT.

       PAID-2.
           READ PAYCUR NEXT
             AT END
               GO TO PAID-3.

           IF PC-KEY8 NOT = CC-KEY8 GO TO PAID-3.

           IF PC-CLAIM NOT = CC-CLAIM GO TO PAID-2.

           IF PC-DENIAL = "14" ADD PC-AMOUNT TO REDUCT.

           ADD PC-AMOUNT TO TOT-CLAIM
           GO TO PAID-2.

       PAID-3.
           IF TOT-CLAIM = 0 MOVE "PIF" TO FO-PIF.

       PAID-1-EXIT.
           EXIT.

       P9.
           CLOSE FILEOUT KINFILE INSFILE CHARCUR PAYCUR.
           STOP RUN.
