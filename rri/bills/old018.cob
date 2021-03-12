      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PED018.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BILLPARM ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT BILLDATE ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT TB-BILL ASSIGN TO "S40" ORGANIZATION
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
           SELECT INSFILE ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT MVP-BILL ASSIGN TO "S65" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  MVP-BILL.
       01  MVP-BILL01 PIC X(8).
       FD  INSFILE
           BLOCK CONTAINS 6 RECORDS
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
       
       FD  BILLPARM
           DATA RECORD IS BILLPARM01.
       01  BILLPARM01.
           02 PF1 PIC S999.
           02 PF2 PIC S999.
           02 PF3 PIC S999.
           02 PF4 PIC X(40).
       FD  BILLDATE
           DATA RECORD IS BILLDATE01.
       01  BILLDATE01.
           02 BILL-DATE PIC X(8).
           02 TC1 PIC X.
           02 TC2 PIC X.
           02 TC3 PIC X.
           02 TC4 PIC X.
       FD  TB-BILL
           DATA RECORD IS TB-BILL01.
       01  TB-BILL01.

           02 TB-1 PIC X(8).
           02 TB-2 PIC X(24).
           02 TB-3 PIC X(9).
           02 TB-4 PIC X.
           02 TB-5 PIC S9(5)V99.
           02 TB-6 PIC XXX.
           02 TB-7 PIC 9(7).
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
       01  PHR01.
           02 PHR02 OCCURS 990 TIMES.
             03 PHR-CLAIM PIC X(6).
             03 PHR-PAYCODE PIC XXX.
             03 PHR-AMOUNT PIC S9(4)V99.
             03 PHR-FLAG PIC 9.
       01  MON-TAB01RE.
           02 FILLER PIC X(18) VALUE "000031059090120151".
           02 FILLER PIC X(18) VALUE "181212243273304334".
       01  MON-TAB01 REDEFINES MON-TAB01RE.
           02 MON-TAB PIC 999 OCCURS 12 TIMES.
       01  LEAP-TAB01RE.
           02 FILLER PIC X(18) VALUE "000031060091121152".
           02 FILLER PIC X(18) VALUE "182213244274305335".
       01  LEAP-TAB01 REDEFINES LEAP-TAB01RE.
           02 LEAP-TAB PIC 999 OCCURS 12 TIMES.
       01  DAY-TEST-1.
           02 DY1 PIC 9999.
           02 DM1 PIC 99.
           02 DD1 PIC 99.
       01  DAY-TEST-2.
           02 DY2 PIC 9999.
           02 DM2 PIC 99.
           02 DD2 PIC 99.
       01     NEW-CHARGE PIC S9(6)V99.
       01     DAY1 PIC S999.
       01     DAY2 PIC S999.
       01     AMOUNT-DUE PIC S9(5)V99.
       01     BAL-FWD PIC S9(5)V99.
       01     DAYS PIC S9999.
       01     QY1 PIC S9999.
       01     QY2 PIC S9999.
       01     QDAY1 PIC S999.
       01     QDAY2 PIC S999.
       01     PHR PIC 999.
       01     A PIC 999.
       01     B PIC 999.
       01     C PIC S999.
       01     D PIC 999.
       01     X PIC 999.
       01     Y PIC 999.
       01     Z PIC 999.
       01     X-BALCUR PIC S9(5)V99.
       01     X-BAL30 PIC S9(5)V99.
       01     X-BAL60 PIC S9(5)V99.
       01     X-BALCOL PIC S9(5)V99.
       01     FLAG PIC 9.
       01     PAY-FLAG PIC 9.
       01     CHAR-FLAG PIC 9.
       01     CLAIM-TOT PIC S9(6)V99.
       PROCEDURE DIVISION.
       P0.
           OPEN OUTPUT TB-BILL MVP-BILL.
           OPEN I-O GARFILE.
           OPEN INPUT CHARCUR PAYCUR INSFILE BILLPARM.
           OPEN INPUT BILLDATE.
           READ BILLDATE AT END GO TO R20.
           READ BILLPARM AT END GO TO R20.
           MOVE SPACE TO G-GARNO
           START GARFILE KEY > G-GARNO INVALID GO TO R20.
       R1. READ GARFILE NEXT WITH LOCK AT END GO TO R20.
           IF G-ACCTSTAT NOT NUMERIC
           DISPLAY G-GARNO " " G-GARNAME "   G-ACCTSTAT NOT NUMBER"
           MOVE 1 TO G-ACCTSTAT.
           IF G-ACCTSTAT = 9 GO TO R1.
           IF G-ACCTSTAT = 7 
           DISPLAY G-GARNO " " G-GARNAME "   ACCTSTAT=7"
           MOVE "0" TO TB-4 
           GO TO R10.
           IF G-ACCTSTAT = 8 MOVE "1" TO TB-4 
           DISPLAY G-GARNO " " G-GARNAME "   ACCTSTAT=8"
           MOVE 1 TO G-ACCTSTAT
           GO TO R10.
           IF G-BILLCYCLE < "1" OR > "4" 
           MOVE TC1 TO G-BILLCYCLE.
           IF G-DUNNING NOT NUMERIC MOVE "1" TO G-DUNNING.
       R3. MOVE 0 TO PHR X-BALCUR X-BAL30 X-BAL60 X-BALCOL NEW-CHARGE
            CHAR-FLAG.
           MOVE G-GARNO TO PC-KEY8.
           MOVE ZEROES TO PC-KEY3.
           START PAYCUR KEY > PAYCUR-KEY INVALID GO TO R1-1.
       R8. READ PAYCUR NEXT AT END GO TO R1-1.
           IF G-GARNO NOT = PC-KEY8 GO TO R1-1.
           ADD 1 TO PHR.
           IF PHR > 990 DISPLAY G-GARNO " "  G-GARNAME
           GO TO R1.
           MOVE 0 TO PHR-FLAG(PHR)
      *     IF (PC-DATE-E > G-LASTBILL) AND (PC-DATE-E > "20020930")
      *     AND (PC-PAYCODE < "007" OR > "016")
      *     AND NOT (PC-DENIAL = "DI" OR "CP" OR "14" OR "15" OR "AA")
      *     MOVE 1 TO PHR-FLAG(PHR)
      *     ELSE MOVE 0 TO PHR-FLAG(PHR).
           MOVE PC-CLAIM TO PHR-CLAIM(PHR)
           MOVE PC-PAYCODE TO PHR-PAYCODE(PHR)
           MOVE PC-AMOUNT TO PHR-AMOUNT(PHR)
           GO TO R8.
       R1-1.
           MOVE G-GARNO TO CC-KEY8.
           MOVE ZEROES TO CC-KEY3.
           START CHARCUR KEY > CHARCUR-KEY INVALID GO TO R7.
       R6. READ CHARCUR NEXT AT END GO TO R7.
           IF G-GARNO NOT = CC-KEY8 GO TO R7.
           IF CC-ASSIGN = "A" GO TO R6.
           COMPUTE CLAIM-TOT = CC-AMOUNT
           PERFORM CC1 THRU CC-EXIT.
      *  NEW CHARGE SINCE LAST BILL WITH A BALANCE.
           IF (CC-DATE-P > G-LASTBILL) AND (G-LASTBILL > "20020930")
           AND (CLAIM-TOT > 0) 
           DISPLAY G-GARNO " " G-GARNAME " " CC-DATE-P " CHR ENTRY"
           MOVE 1 TO CHAR-FLAG.
           GO TO R6.

      *  UNASSIGNED PAYMENT SINCE LAST BILL.
      *  SEND A BILL IF ACCOUNT LATER IS SHOWN TO HAVE A BALANCE.
      *     MOVE 0 TO PAY-FLAG 
      *     PERFORM PH3 VARYING Y FROM 1 BY 1 UNTIL Y > PHR
      *     IF PAY-FLAG = 1 MOVE 1 TO CHAR-FLAG
      *     GO TO R6.

      *  ASSIGNED PAYMENT MADE SINCE LAST BILL AND CLAIM HAS A BALANCE.
      *  THIS PROVIDES IMMEDIATE BILLING AFTER ASSIGNED INSURANCE PAYS.
      *     IF  (CLAIM-TOT > 0) 
      *     MOVE 0 TO PAY-FLAG 
      *     PERFORM PH4 VARYING Y FROM 1 BY 1 UNTIL Y > PHR
      *     IF PAY-FLAG = 1 MOVE 1 TO CHAR-FLAG.
      *      GO TO R6.


       R7.
           ADD X-BALCUR X-BAL30 X-BAL60 X-BALCOL GIVING BAL-FWD.
      *    DONT BILL SMALL BALANCES
           IF BAL-FWD < 3.00 GO TO R1.
      
      *    NEW PAYMENT OR NEW CHARGES SINCE LAST BILL WEEK
           IF G-DUNNING < "2"
            IF CHAR-FLAG = 1 
              MOVE "1" TO TB-4 
               IF (G-BILLCYCLE = TC1 OR TC2)
               GO TO R10
               END-IF
            END-IF
           END-IF.

      *    STILL BILLING COLLECTION ACCOUNTS 
      *    UNTIL ALL CHARGES IN COLLECTION
      *    OR A BUDGET ACCOUNT  -- COLLT = 5
      *    BUT NEEDS TO BE ON BILLING CYCLE
           IF (G-DUNNING > "1") 
           AND (G-BILLCYCLE = TC1 OR TC2)
           AND ((X-BALCUR > 0 OR X-BAL30  > 0 OR X-BAL60 > 0)) 
           DISPLAY G-GARNO " " G-GARNAME "  NOT ALL IN COLLECT"
           MOVE "1" TO TB-4 
           MOVE TC1 TO G-BILLCYCLE
           GO TO R10.

      *    BILLING SENT TO OFFICE FOR COLLT VALUES
      *    FOR ACCOUNT ON CYCLE.
      *  FIRST: NEW COLLECTS FROM LAST MONTH BILLING
           IF (G-DUNNING = "2") 
           AND (G-BILLCYCLE = TC1)
           MOVE "3" TO G-DUNNING
           MOVE "3" TO TB-4 GO TO R10.

      * SECOND: LAST BILLING EVER UNLESS NEW CHARGES/PAYMENTS
           IF (G-DUNNING = "3") 
           AND (G-BILLCYCLE = TC1)
           MOVE "4" TO G-DUNNING
           MOVE "4" TO TB-4 GO TO R10.

      *    NO BILLS FOR OTHER DUNNING VALUES.
           IF G-DUNNING > "3" GO TO R1.

      *    NEW COLLECTION ACCOUNTS. SET DUNNING TO 2.
           IF X-BALCOL > 0 AND G-DUNNING = "1"
           MOVE "2" TO G-DUNNING
      *    mail out the new collects sorted by zip
           MOVE "1" TO TB-4 
           MOVE TC1 TO G-BILLCYCLE
           GO TO R10.

      *    FINALLY!
      *    REGULAR MONTHLY BILLING BY DEFINITION:
      *    NOTHING NEW SINCE LAST WEEK
      *    BALANCE OF AT LEAST $2.00
      *    DUNNING VALUE = 1
      *    ON CYCLE ACCOUNT
           IF NOT (G-BILLCYCLE = TC1 OR TC2) GO TO R1.
           MOVE "1" TO TB-4.
       R10.
           MOVE G-GARNO TO TB-1
           IF GN-1 = "1" MOVE GN-2 TO TB-2
           ELSE MOVE G-GARNAME TO TB-2.
           MOVE G-ZIP TO TB-3
           MOVE G-PRINS TO TB-6
           MOVE BAL-FWD TO TB-5 TB-7.
           IF TB-4 < "2" MOVE ZERO TO TB-6 TB-7.
           WRITE TB-BILL01
           MOVE BILL-DATE TO G-LASTBILL
      *     REWRITE GARFILE01
           GO TO R1.

      *     IF G-PRINS = "256" OR "349"
      *     WRITE MVP-BILL01 FROM G-GARNO
      *     ELSE
      *     END-IF


       CC1.
           PERFORM PH2 VARYING Y FROM 1 BY 1 UNTIL Y > PHR.
           IF CLAIM-TOT = 0 GO TO CC-EXIT.
           IF CC-DATE-A = "00000000" ADD CLAIM-TOT TO X-BALCUR
           GO TO CC-EXIT.
           MOVE CC-DATE-A TO DAY-TEST-1.
           MOVE 0 TO D.
           DIVIDE DY1 BY 4 GIVING B REMAINDER D.
           IF D = 0 COMPUTE DAY1 = LEAP-TAB(DM1) + DD1
           ELSE COMPUTE DAY1 = MON-TAB(DM1) + DD1.
           MOVE BILL-DATE TO DAY-TEST-2.
           MOVE 0 TO D.
           DIVIDE DY2 BY 4 GIVING B REMAINDER D.
           IF D = 0 COMPUTE DAY2 = LEAP-TAB(DM2) + DD2
           ELSE COMPUTE DAY2 = MON-TAB(DM2) + DD2.
           MOVE DY2 TO QY2
           MOVE DY1 TO QY1
           MOVE DAY1 TO QDAY1
           MOVE DAY2 TO QDAY2.
           COMPUTE DAYS = 365 * (QY2 - QY1) + QDAY2 - QDAY1
           ON SIZE ERROR MOVE 991 TO DAYS.
           IF DAYS > PF3 ADD CLAIM-TOT TO X-BALCOL GO TO CC-EXIT.
           IF DAYS > PF2 ADD CLAIM-TOT TO X-BAL60 GO TO CC-EXIT.
           IF DAYS > PF1 ADD CLAIM-TOT TO X-BAL30 GO TO CC-EXIT.
           ADD CLAIM-TOT TO X-BALCUR.
       CC-EXIT. EXIT.
       PH2. IF CC-CLAIM = PHR-CLAIM(Y)
           ADD PHR-AMOUNT(Y) TO CLAIM-TOT.
       PH3. IF (CC-CLAIM = PHR-CLAIM(Y))
            AND (PHR-FLAG(Y) = 1 )
            AND (PHR-PAYCODE(Y) NOT = "001")
            MOVE PHR-PAYCODE(Y) TO INS-KEY
             READ INSFILE INVALID MOVE "U" TO INS-ASSIGN
             END-READ
              IF INS-ASSIGN = "U" 
               MOVE PHR TO Y
               MOVE 1 TO PAY-FLAG
              END-IF
            END-IF.
       PH4. IF CC-CLAIM = PHR-CLAIM(Y)
            AND PHR-FLAG(Y) = 1 
            MOVE PHR-PAYCODE(Y) TO INS-KEY
             READ INSFILE INVALID MOVE "A" TO INS-ASSIGN
             END-READ
              IF INS-ASSIGN = "A" 
               MOVE PHR TO Y
               MOVE 1 TO PAY-FLAG
              END-IF
            END-IF.
       R20. CLOSE GARFILE TB-BILL MVP-BILL. STOP RUN.
