      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3

       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrr018.
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

           SELECT EMAILAUTHFILE ASSIGN TO "S70" ORGANIZATION INDEXED
             ACCESS MODE DYNAMIC RECORD KEY IS EA-KEY
             ALTERNATE RECORD KEY IS EA-MEDREC WITH DUPLICATES
             ALTERNATE RECORD KEY IS EA-NAME WITH DUPLICATES
             ALTERNATE RECORD KEY IS EA-EMAIL WITH DUPLICATES
             ALTERNATE RECORD KEY IS EA-AUTH WITH DUPLICATES
             ALTERNATE RECORD KEY IS EA-DATE-E WITH DUPLICATES
             ALTERNATE RECORD KEY IS EA-SSN WITH DUPLICATES
             LOCK MODE MANUAL.

       DATA DIVISION.

       FILE SECTION.
       FD  MVP-BILL.
       01  MVP-BILL01 PIC X(8).

       FD  INSFILE.
           copy insfile.cpy in "c:\users\sid\cms\copylib\rri".
       
       FD  BILLPARM.
       01  BILLPARM01.
           02 PF1 PIC S999.
           02 PF2 PIC S999.
           02 PF3 PIC S999.
           02 PF4 PIC X(40).

       FD  BILLDATE.
       01  BILLDATE01.
           02 BILL-DATE PIC X(8).
           02 TC1 PIC X.
           02 TC2 PIC X.
           02 TC3 PIC X.
           02 TC4 PIC X.

       FD  TB-BILL.
       01  TB-BILL01.
           02 TB-1 PIC X(8).
           02 TB-2 PIC X(24).
           02 TB-3 PIC X(9).
           02 TB-4 PIC X.
           02 TB-5 PIC S9(5)V99.
           02 TB-6 PIC XXX.
           02 TB-7 PIC 9(7).
           02 tb-email pic x(30).

       FD  CHARCUR.
           copy charcur.cpy in "c:\users\sid\cms\copylib\rri".

       FD  PAYCUR.
           copy paycur.cpy in "c:\users\sid\cms\copylib\rri".

       FD  GARFILE.
           copy garfile.cpy in "c:\users\sid\cms\copylib\rri".

       FD  emailauthfile.
           copy emailauthfile.cpy in "c:\users\sid\cms\copylib\rri".


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

       01  NEW-CHARGE PIC S9(6)V99.
       01  DAY1 PIC S999.
       01  DAY2 PIC S999.
       01  AMOUNT-DUE PIC S9(5)V99.
       01  BAL-FWD PIC S9(5)V99.
       01  DAYS PIC S9999.
       01  QY1 PIC S9999.
       01  QY2 PIC S9999.
       01  QDAY1 PIC S999.
       01  QDAY2 PIC S999.
       01  PHR PIC 999.
       01  A PIC 999.
       01  B PIC 999.
       01  C PIC S999.
       01  D PIC 999.
       01  X PIC 999.
       01  Y PIC 999.
       01  Z PIC 999.
       01  X-BALCUR PIC S9(5)V99.
       01  X-BAL30 PIC S9(5)V99.
       01  X-BAL60 PIC S9(5)V99.
       01  X-BALCOL PIC S9(5)V99.
       01  FLAG PIC 9.
       01  PAY-FLAG PIC 9.
       01  CHAR-FLAG PIC 9.
       01  CLAIM-TOT PIC S9(6)V99.
       01  hold-email pic x(30).

       PROCEDURE DIVISION.
       P0.
           OPEN OUTPUT TB-BILL MVP-BILL.
           OPEN I-O GARFILE.
           OPEN INPUT CHARCUR PAYCUR INSFILE BILLPARM BILLDATE
             emailauthfile.
           
           READ BILLDATE AT END GO TO R20.
           
           READ BILLPARM AT END GO TO R20.
           
           MOVE SPACE TO G-GARNO
           
           START GARFILE KEY > G-GARNO INVALID GO TO R20.

       R1. 
           READ GARFILE NEXT WITH LOCK
             AT END
              GO TO R20.
                      
           IF G-ACCTSTAT NOT NUMERIC
             DISPLAY G-GARNO " " G-GARNAME " G-ACCTSTAT NOT NUMBER"
             MOVE 1 TO G-ACCTSTAT.
           
           IF G-ACCTSTAT = 9 GO TO R1.

           move space to hold-email.
           move g-acct to ea-medrec.
           start emailauthfile key not < ea-medrec
             invalid
               display "no entry in emailauthssnfile".
               
           perform email-1 thru email-exit.   
           
           IF G-ACCTSTAT = 7 
             DISPLAY G-GARNO " " G-GARNAME " ACCTSTAT=7"
             MOVE "0" TO TB-4 
             GO TO R10.

           IF G-ACCTSTAT = 8
             MOVE "1" TO TB-4 
             DISPLAY G-GARNO " " G-GARNAME " ACCTSTAT=8"
             MOVE 1 TO G-ACCTSTAT
             GO TO R10.

           IF G-BILLCYCLE < "1" OR > "4" 
             MOVE TC1 TO G-BILLCYCLE.

           IF G-DUNNING NOT NUMERIC MOVE "1" TO G-DUNNING.

       R3. 
           MOVE 0 TO PHR X-BALCUR X-BAL30 X-BAL60 X-BALCOL NEW-CHARGE
             CHAR-FLAG.
           MOVE G-GARNO TO PC-KEY8.
           MOVE ZEROES TO PC-KEY3.
           START PAYCUR KEY > PAYCUR-KEY
             INVALID
               GO TO R1-1.

       R8. 
           READ PAYCUR NEXT
             AT END
               GO TO R1-1.

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

       R6. 
           READ CHARCUR NEXT AT END GO TO R7.
           
           IF G-GARNO NOT = CC-KEY8 GO TO R7.
           
           IF CC-ASSIGN = "A" GO TO R6.
           
           COMPUTE CLAIM-TOT = CC-AMOUNT
           
           PERFORM CC1 THRU CC-EXIT.

      *  NEW CHARGE SINCE LAST BILL WITH A BALANCE.           
           IF (CC-DATE-P > G-LASTBILL) AND (G-LASTBILL > "20180930")
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
             AND ((X-BALCUR > 0 OR X-BAL30 > 0 OR X-BAL60 > 0)) 
               DISPLAY G-GARNO " " G-GARNAME "  NOT ALL IN COLLECT"
               MOVE "1" TO TB-4 
               MOVE TC1 TO G-BILLCYCLE
               GO TO R10.

      *    BILLING SENT TO OFFICE FOR COLLT VALUES
      *    FOR ACCOUNT ON CYCLE.
      *    FIRST: NEW COLLECTS FROM LAST MONTH BILLING
           IF (G-DUNNING = "2") AND (G-BILLCYCLE = TC1)
             MOVE "3" TO G-DUNNING
             MOVE "3" TO TB-4 
             GO TO R10.

      * SECOND: LAST BILLING EVER UNLESS NEW CHARGES/PAYMENTS
           IF (G-DUNNING = "3") AND (G-BILLCYCLE = TC1)
             MOVE "4" TO G-DUNNING
             MOVE "4" TO TB-4 
             GO TO R10.

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

           IF G-GARNAME(1:1) = "1"
             MOVE G-GARNAME(2:23) TO TB-2
           ELSE 
             MOVE G-GARNAME TO TB-2.

           MOVE G-ZIP TO TB-3
           MOVE G-PRINS TO TB-6
           MOVE BAL-FWD TO TB-5 TB-7 
           move hold-email to tb-email            

           IF TB-4 < "2" 
             MOVE ZERO TO TB-6 TB-7.
             
           WRITE TB-BILL01
           
           MOVE BILL-DATE TO G-LASTBILL
           REWRITE GARFILE01
           
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
           
           IF D = 0 
             COMPUTE DAY1 = LEAP-TAB(DM1) + DD1
           ELSE 
             COMPUTE DAY1 = MON-TAB(DM1) + DD1.

           MOVE BILL-DATE TO DAY-TEST-2.
           MOVE 0 TO D.
           DIVIDE DY2 BY 4 GIVING B REMAINDER D.

           IF D = 0 
             COMPUTE DAY2 = LEAP-TAB(DM2) + DD2
           ELSE 
             COMPUTE DAY2 = MON-TAB(DM2) + DD2.

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

       CC-EXIT. 
           EXIT.

       PH2. 
           IF CC-CLAIM = PHR-CLAIM(Y)
           ADD PHR-AMOUNT(Y) TO CLAIM-TOT.

       PH3. 
           IF (CC-CLAIM = PHR-CLAIM(Y))
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

       PH4. 
           IF CC-CLAIM = PHR-CLAIM(Y)
            AND PHR-FLAG(Y) = 1 
            MOVE PHR-PAYCODE(Y) TO INS-KEY
             READ INSFILE INVALID MOVE "A" TO INS-ASSIGN
             END-READ
              IF INS-ASSIGN = "A" 
               MOVE PHR TO Y
               MOVE 1 TO PAY-FLAG
              END-IF
            END-IF.

       email-1.           
           read emailauthfile next
             at end
               go to email-exit.    

           if ea-medrec not = g-acct
             go to email-exit.

           move ea-email to hold-email.

           go to email-1.

       email-exit.
           exit.                            

       R20. 
           CLOSE billparm billdate tb-BILL paycur charcur
             garfile insfile MVP-BILL emailauthfile. 
           STOP RUN.
