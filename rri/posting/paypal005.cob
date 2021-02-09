      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. paypal005.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILEIN ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT GARFILE ASSIGN TO "S35"     ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE MANUAL.

           SELECT CHARCUR ASSIGN TO "S40"     ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.

           SELECT ERROR-FILE ASSIGN TO "S45" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S50" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT PAYFILE ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PAYFILE-KEY
           LOCK MODE MANUAL.

           SELECT PAYCUR ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.

       DATA DIVISION.

       FILE SECTION.

       FD  FILEIN.
       01  FILEIN01.
           02 fi-date pic x(8).
           02 filler pic x value space.
           02 fi-name pic x(30).
           02 filler pic x value space.
           02 FI-GARNO PIC X(8).
           02 FILLER PIC X VALUE SPACES.
           02 FI-DEDUCT PIC 9(4)V99.
           02 FILLER PIC X VALUE SPACES.
           02 FI-PAID PIC 9(4)V99.

       FD  PAYCUR.
           COPY PAYCUR.CPY IN "C:\Users\sid\cms\copylib".

       FD  PAYFILE.
           COPY PAYFILE.CPY IN "C:\Users\sid\cms\copylib".

       FD  ERROR-FILE.
       01  ERROR-FILE01 PIC X(132).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(133).
       
       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  GARFILE.
           COPY garfile.CPY IN "C:\Users\sid\cms\copylib\rri".
          
       WORKING-STORAGE SECTION.
       
       01  TOT-AMT PIC S9(4)V99.
       01  CLAIM-TOT PIC S9(5)V99.
       01  FLAG PIC 9 VALUE 0.
       01  TOT-PAY PIC S9(5)V99 VALUE 0.
       01  X PIC 99.
       01  Y PIC 99.
       01  Z PIC 99.
       01  A PIC 99.
       01 HOLDKEY PIC X(11).
       01  TEST-DATE.
           05  T-CC            PIC 99.
           05  T-YY            PIC 99.
           05  T-MM            PIC 99.
           05  T-DD            PIC 99.
       01  INPUT-DATE.
           02 T-MM PIC XX.
           02 T-DD PIC XX.
           02 T-CC PIC XX.
           02 T-YY PIC XX.
       01  ALF6.
           02 ALF4 PIC X(4).
           02 ALF2 PIC XX.
       01  PAYDATE PIC X(8).
       01  XYZ PIC 999.
       01  NUM6 PIC 9(6).
       01  PAYBACK01 PIC X(80).
       01  ALF1 PIC X.
       01  NEF-6 PIC Z,ZZ9.99CR.

       PROCEDURE DIVISION.

       0005-START.
           OPEN INPUT FILEIN CHARCUR GARFILE PAYCUR.
           OPEN OUTPUT ERROR-FILE FILEOUT.
           OPEN I-O PAYFILE.       

       P1.
           READ FILEIN 
             AT END 
               GO TO P9.

           MOVE FI-DATE TO PAYDATE

           MOVE FI-GARNO TO G-GARNO
           
           READ GARFILE 
             INVALID 
               GO TO E1.
           
           MOVE G-GARNO TO CC-KEY8
           MOVE SPACE TO CC-KEY3
           START CHARCUR KEY NOT < CHARCUR-KEY 
             INVALID 
               GO TO E1.

       P2. 
           READ CHARCUR NEXT 
             AT END 
               GO TO E1.

           IF CC-KEY8 NOT = G-GARNO 
             GO TO E1.           

           MOVE FI-PAID TO NEF-6
           DISPLAY NEF-6 " PAID"
           MOVE FI-DEDUCT TO NEF-6
           DISPLAY NEF-6 " DEDUCT"

      *     MOVE 0 TO FLAG
      *     PERFORM A1 THRU A3

      *     IF FLAG = 1 GO TO P2.

           MOVE SPACE TO PD-DENIAL 
           MOVE "077" TO PD-PAYCODE
           ACCEPT PD-ORDER FROM TIME.
           MOVE PAYDATE TO PD-DATE-T
           ACCEPT PD-DATE-E FROM DATE YYYYMMDD.
           MOVE SPACE TO PD-BATCH
           MOVE CC-CLAIM TO PD-CLAIM
           MOVE G-GARNO TO PD-KEY8
           MOVE SPACE TO PD-KEY3
           MOVE G-GARNAME TO PD-NAME
           MOVE 0 TO PD-AMOUNT
           MOVE PAYFILE01 TO PAYBACK01
           PERFORM S4 THRU S5 
           MOVE 0 TO XYZ
          
           IF FI-PAID NOT = 0
             IF CLAIM-TOT NOT < FI-PAID 
               COMPUTE PD-AMOUNT =  -1 * FI-PAID
             ELSE
               COMPUTE PD-AMOUNT = -1 * CLAIM-TOT
             END-IF
             
             COMPUTE FI-PAID = FI-PAID + PD-AMOUNT
             WRITE FILEOUT01 FROM CHARCUR01
             MOVE PAYFILE01 TO PAYBACK01
             PERFORM P3 THRU P4

           END-IF
           
           ADD PD-AMOUNT TO CLAIM-TOT

           IF CLAIM-TOT = 0 GO TO P2.

           IF CLAIM-TOT NOT < FI-DEDUCT 
             COMPUTE PD-AMOUNT =  -1 * FI-DEDUCT
           ELSE
             COMPUTE PD-AMOUNT = -1 * CLAIM-TOT
           END-IF

           COMPUTE FI-DEDUCT = FI-DEDUCT + PD-AMOUNT
           MOVE "14" TO PD-DENIAL
           MOVE PAYFILE01 TO PAYBACK01
           PERFORM P3 THRU P4

           IF FI-DEDUCT = 0 GO TO P1.
           
           GO TO P2.

       P3.
           ADD 1 TO XYZ
           MOVE XYZ TO PD-KEY3
           READ PAYFILE INVALID KEY GO TO P4.
           GO TO P3.

       P4.
           MOVE PAYBACK01 TO PAYFILE01
           MOVE XYZ TO PD-KEY3
           IF PD-AMOUNT NOT = 0
           WRITE PAYFILE01
           DISPLAY PAYFILE-KEY " " PD-NAME
           DISPLAY "RECORD IS ADDED"
           END-IF.

       S4.
           MOVE CC-AMOUNT TO CLAIM-TOT
           MOVE CC-KEY8 TO PC-KEY8
           MOVE "000" TO PC-KEY3.
           START PAYCUR KEY NOT < PAYCUR-KEY INVALID GO TO S5.

       S41.
           READ PAYCUR NEXT AT END GO TO S5.
           IF PC-KEY8 NOT = CC-KEY8 GO TO S5.
           IF PC-CLAIM NOT = CC-CLAIM GO TO S41. 
           ADD PC-AMOUNT TO CLAIM-TOT.
           GO TO S41.

       S5.
           EXIT.

       A1.
           MOVE CC-KEY8 TO PD-KEY8
           MOVE "000" TO PD-KEY3.
           START PAYFILE KEY NOT <  PAYFILE-KEY INVALID GO TO A3.

       A2. 
           READ PAYFILE NEXT AT END GO TO A3.

           IF PD-KEY8 NOT = CC-KEY8 GO TO A3.

           IF PD-CLAIM NOT = CC-CLAIM GO TO A2.

           MOVE 1 TO FLAG.

       A3.
           EXIT.

       E1.
           IF NOT (FI-PAID = 0 AND FI-DEDUCT = 0)
           WRITE ERROR-FILE01 FROM FILEIN01.
           GO TO P1.

       P9.
           CLOSE CHARCUR GARFILE ERROR-FILE FILEOUT PAYFILE PAYCUR
             FILEIN

           STOP RUN.
