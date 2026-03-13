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
       FD  PAYCUR.
           COPY "paycur.cpy".
      
       FD  CHARCUR.
           COPY "charcur.cpy".

       FD  INSFILE.
           COPY "insfile.cpy".
     
       FD  FILEOUT.
       01  FILEOUT01.
           02 FO-ACCT PIC X(8).
           02 FILLER PIC X VALUE X"09".
           02 FO-NAME PIC X(24).
           02 FILLER PIC X VALUE X"09".
           02 FO-PAYCODE PIC XXX.
           02 FILLER PIC X VALUE X"09".
           02 FO-INSNAME PIC X(28).
           02 FILLER PIC X VALUE X"09".
           02 FO-DATE PIC X(10).
           02 FILLER PIC X VALUE X"09".
           02 FO-AMOUNT PIC X(10).
           02 FILLER PIC X VALUE X"09".
           02 FO-AMTCR PIC XX.
           02 FILLER PIC X VALUE X"09".
           02 FO-REDUCT PIC X(10).
           02 FILLER PIC X VALUE X"09".
           02 FO-REDUCTCR PIC XX.
           02 FILLER PIC X VALUE X"09".
           02 FO-PIF PIC XXX.

       FD  KINFILE.
           COPY "kinfile.cpy".

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
       01  NEF-6 PIC Z,ZZ9.99.
       01  NEF-ABS PIC S9(4)V99.
       01  TOT-CLAIM PIC S9(4)V99.
       01  REDUCT PIC S9(4)V99.

       PROCEDURE DIVISION.

       0005-START.
           OPEN I-O KINFILE
           OPEN INPUT INSFILE CHARCUR PAYCUR
           OPEN OUTPUT FILEOUT.

           MOVE "ACCT" TO FO-ACCT
           MOVE "NAME" TO FO-NAME
           MOVE "PAYCODE" TO FO-PAYCODE
           MOVE "PAYOR" TO FO-INSNAME
           MOVE "DATE" TO FO-DATE
           MOVE "PAID" TO FO-AMOUNT
           MOVE "WRITE-OFF" TO FO-REDUCT
           MOVE "BAL" TO FO-PIF
           MOVE "CR" TO FO-AMTCR.
           MOVE "CR" TO FO-REDUCTCR.
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
           COMPUTE NEF-ABS = FUNCTION ABS(KIN-AMOUNT)
           MOVE NEF-ABS TO NEF-6
           MOVE NEF-6 TO FO-AMOUNT
           IF KIN-AMOUNT < 0
               MOVE "CR" TO FO-AMTCR
           ELSE
               MOVE SPACE TO FO-AMTCR.
           MOVE SPACE TO FO-PIF

           PERFORM PAID-1 THRU PAID-1-EXIT

           MOVE REDUCT TO NEF-6
           COMPUTE NEF-ABS = FUNCTION ABS(REDUCT)
           MOVE NEF-ABS TO NEF-6
           MOVE NEF-6 TO FO-REDUCT
           IF REDUCT < 0
               MOVE "CR" TO FO-REDUCTCR
           ELSE
               MOVE SPACE TO FO-REDUCTCR.
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
