      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. dour015.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT TB-BILL ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT PAYCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY.

           SELECT CHARCUR ASSIGN TO "S40"     ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC        RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES.

           SELECT OUT-FILE ASSIGN TO "S45" ORGANIZATION 
           LINE SEQUENTIAL.

           SELECT BILLDATE ASSIGN TO "S50" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT GARFILE ASSIGN TO "S55" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES.

       DATA DIVISION.

       FILE SECTION.

       FD  GARFILE.
           copy garfile.cpy in "c:\users\sid\cms\copylib\rri".

       FD  CHARCUR.
           copy charcur.cpy in "c:\users\sid\cms\copylib\rri".

       FD  PAYCUR.
           copy paycur.cpy in "c:\users\sid\cms\copylib\rri".

       FD  TB-BILL.
       01  TB-BILL01.
           02 TB-1 PIC X(8).
           02 TB-2 PIC X(24).
           02 TB-3 PIC X(9).
           02 TB-4 PIC X.
           02 TB-6 PIC X.
           02 TB-5 PIC S9(5)V99.
           02 TB-DOC PIC XX.
           02 TB-NAME PIC X(24).

       FD  OUT-FILE.
       01  OUT01. 
           02 TO-1 PIC X(76).
           02 TO-2 PIC X.

       FD  BILLDATE.
       01  BILLDATE01.
           02 BILL-DATE PIC X(8).
           02 THIS-CYCLE PIC X(4).
           02 FILLER PIC X.
           02 DD-0 PIC X.
           02 DD-1 PIC X.
           02 DD-2 PIC X.
           02 DD-3 PIC X.
           02 DD-4 PIC X.
           02 DD-5 PIC X.
           02 DD-6 PIC X.

       WORKING-STORAGE SECTION.
       01  CHR01.
           02 CHR02 OCCURS 500 TIMES.
             03 CHR-DATE-T PIC X(8).
             03 CHR-DATE-A PIC X(8).
             03 CHR-CLAIM PIC X(6).
             03 CHR-AMOUNT PIC S9(4)V99.
             03 CHR-PAYCODE PIC X(3).
             03 CHR-PROC PIC X(11).
             03 CHR-DIAG PIC X(5).
             03 CHR-DOCR PIC XXX.
             03 CHR-PATID PIC X(8).
             03 CHR-REC-STAT PIC X.
             03 CHR-ASSIGN PIC X.

       01  PHR01.
           02 PHR02 OCCURS 800 TIMES.
             03 PHR-DATE PIC X(8).
             03 PHR-CLAIM PIC X(6).
             03 PHR-AMOUNT PIC S9(4)V99.
             03 PHR-PAYCODE PIC XXX.
             03 PHR-DENIAL PIC XX.
             03 PHR-STAT PIC 9.

       01  TAB1101.
           02 TAB11 PIC X OCCURS 11 TIMES.

       01  TAB2001.
           02 TAB20 PIC X OCCURS 20 TIMES.

       01  TABA2401.
           02 TABA24 PIC X OCCURS 24 TIMES.
       01  TABB2401.
           02 TABB24 PIC X OCCURS 24 TIMES.
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
       01  INPUT-DATE.
           05 T-MM  PIC XX.
           05 T-DD  PIC XX.
           05 T-CC  PIC XX.
           05 T-YY  PIC XX.
       01  TEST-DATE.
           05 T-CC  PIC XX.
           05 T-YY  PIC XX.
           05 T-MM  PIC XX.
           05 T-DD  PIC XX.
       01  HOLD-MASTER PIC X(187).
       01  FLAGP PIC 9.
       01  SUM-DATE PIC X(8).
       01     BILL-COUNT PIC 9(5) VALUE 0.
       01     QY1 PIC S9999.
       01     QY2 PIC S9999.
       01     QDAY1 PIC S999.
       01     QDAY2 PIC S999.
       01     DAY1 PIC S999.
       01     DAY2 PIC S999.
       01     FLAG PIC 9.
       01     SNUM-6 PIC S9(4)V99.
       01     TEST-BUSNAME.
             03 TEST-BUSNAME-1 PIC X.
             03 TEST-BUSNAME-2 PIC X(23).
       01     CLAIM-TOT PIC S9(5)V99.
       01     REF-IND PIC 999.
       01     LINE-CTR PIC 99.
       01     BILL-PAGE PIC 99.
       01     AMOUNT-DUE PIC S9(5)V99.
       01     BAL-FWD PIC S9(5)V99.
       01     DAYS PIC S9999.
       01     AT-3.
             03 AT-3-1 PIC XX.
             03 AT-3-2 PIC X.
       01     NAME-FIRST PIC X(24).
       01     NAME-MIDDLE PIC X(24).
       01     NAME-LAST PIC X(24).
       01     NAME-TEST PIC X(24).
       01     PHR-IND PIC 999.
       01     CHR-IND PIC 999.
       01     TOTCHAR PIC S9(4)V99.
       01     TOTPAY PIC S9(4)V99.
       01     RIGHT-8 PIC X(8) JUST RIGHT.
       01  LLLL PIC X(8).
       01     RIGHT-2 PIC XX JUST RIGHT.
       01     RIGHT-3 PIC XXX JUST RIGHT.
       01     RIGHT-4 PIC X(4) JUST RIGHT.
       01     ALF-1 PIC X.
       01     ALF-2 PIC XX.
       01     ALF-3 PIC XXX.
       01     ALF-4 PIC XXXX.
       01  ALF-5 PIC X(5).
       01     ALF-9 PIC X(9).
       01     ALF-6 PIC X(6).
       01     ALF-7 PIC X(7).
       01     ALF-8 PIC X(8).
       01     ALF-11 PIC X(11).
       01     ALF-13 PIC X(13).
       01     ALF-14 PIC X(14).
       01     EIGHTPARTID.
             03 FILLER PIC X(7).
             03 PART-8 PIC X.
       01     ABC PIC XXX.
       01     XYZ PIC 999.
       01     RIGHT-5 PIC X(5) JUST RIGHT.
       01     RIGHT-7 PIC X(7) JUST RIGHT.
       01     NUM-2 PIC 99.
       01     NUM-3 PIC 999.
       01     NUM-9 PIC 9(9).
       01     NUM-6 PIC 9(6).
       01     A PIC 999.
       01     B PIC 999.
       01     C PIC S999.
       01     D PIC 999.
       01     X PIC 999.
       01     Y PIC 999.
       01     Z PIC 999.
       01     ANS PIC X.
       01    X-INSPEND PIC S9(5)V99.
       01     G-BALCUR PIC S9(5)V99.
       01     G-BAL30  PIC S9(5)V99.
       01     G-BAL60  PIC S9(5)V99.
       01     G-BALCOL  PIC S9(5)V99.
       01  TMC PIC S9(5)V99.
       01  TMP PIC S9(5)V99.
       01  PAB PIC S9(5)V99.
       01  BILLDATE6 PIC XXXXXX.
       01  DATE6 PIC XXXXXX.
       01  RUNBAL PIC S9(5)V99.
       01  PAYFLAG PIC 9 VALUE 0.
       01  L6F7 PIC ZZZ9.99CR.
       01  XB-4 PIC X VALUE "1".
       01  CHNL-X PIC X VALUE H"1A".
       01  CHNL-Y PIC X VALUE H"1C".

       PROCEDURE DIVISION.

       P0.
           OPEN OUTPUT OUT-FILE.
           OPEN INPUT CHARCUR PAYCUR TB-BILL BILLDATE GARFILE.
           READ BILLDATE AT END DISPLAY "NO BILLDATE" GO TO R20.
           MOVE BILL-DATE TO BILLDATE6.
           MOVE BILL-DATE TO DAY-TEST-1 MOVE 0 TO DD1
           MOVE DAY-TEST-1 TO SUM-DATE.

       R1.  
           READ TB-BILL AT END GO TO R20.

           MOVE TB-BILL01 TO TO-1
           IF TB-4 > "1" 
             MOVE "3" TO TO-2
             WRITE OUT01 
             GO TO R1.

           MOVE 0 TO TO-2 CHR-IND PHR-IND LINE-CTR AMOUNT-DUE
             G-BALCUR G-BAL30 G-BAL60 G-BALCOL
           MOVE TB-1 TO CC-KEY8
           MOVE SPACE TO CC-KEY3
           START CHARCUR KEY > CHARCUR-KEY INVALID GO TO R7.

       R6. 
           READ CHARCUR NEXT AT END GO TO R7.
           
           IF TB-1 NOT = CC-KEY8 GO TO R7.
           
           ADD 1 TO CHR-IND.
           
           IF CHR-IND > 500 
             DISPLAY TB-1  " CHRS"
             GO TO R1.

           IF CC-DATE-A = "00000000" 
             MOVE BILL-DATE TO CHR-DATE-A(CHR-IND)
           ELSE 
             MOVE CC-DATE-A TO CHR-DATE-A(CHR-IND).

           MOVE CC-DIAG TO CHR-DIAG(CHR-IND)
           MOVE CC-DATE-T TO CHR-DATE-T(CHR-IND)
           MOVE CC-CLAIM TO CHR-CLAIM(CHR-IND)
           MOVE CC-AMOUNT TO CHR-AMOUNT(CHR-IND)
           MOVE CC-PAYCODE TO CHR-PAYCODE(CHR-IND)
           MOVE CC-DOCR TO CHR-DOCR(CHR-IND)
           MOVE CC-PROC TO CHR-PROC(CHR-IND)
           MOVE CC-PATID TO CHR-PATID(CHR-IND)
           MOVE CC-REC-STAT TO CHR-REC-STAT(CHR-IND)
           MOVE CC-ASSIGN TO CHR-ASSIGN(CHR-IND)
           GO TO R6.

       R7. 
           MOVE TB-1 TO PC-KEY8
           MOVE SPACE TO PC-KEY3
           START PAYCUR KEY > PAYCUR-KEY INVALID GO TO R9.

       R8. 
           READ PAYCUR NEXT AT END GO TO R9.
           
           IF TB-1 NOT = PC-KEY8 
             GO TO R9.
           
           ADD 1 TO PHR-IND.

           IF PHR-IND > 800 
             DISPLAY TB-1 " PAY"
             GO TO R1.

           MOVE PC-DATE-T TO PHR-DATE(PHR-IND)
           MOVE PC-CLAIM TO PHR-CLAIM(PHR-IND)
           MOVE PC-AMOUNT TO PHR-AMOUNT(PHR-IND)
           MOVE PC-PAYCODE TO PHR-PAYCODE(PHR-IND)
           MOVE 0 TO PHR-STAT(PHR-IND)
           MOVE PC-DENIAL TO PHR-DENIAL(PHR-IND).
           GO TO R8.

******* START HERE TO DETERMINE AMOUNT DUE AND INSURANCE PENDING *******
******* AND AGING ON CURRENT,PAST DUE AND DELINQUENT *******
       R9.
           PERFORM CC1 THRU CC-EXIT VARYING X FROM 1 BY 1 UNTIL
             X > CHR-IND.
           
           MOVE TB-1 TO G-GARNO
           
           READ GARFILE INVALID GO TO R15.
           
           IF G-PRINS = "001" ADD 2 TO LINE-CTR.
           
           IF G-PRINS = "091" ADD 2 TO LINE-CTR.

       R15. 
           PERFORM Q2-0 THRU Q10 VARYING A FROM 1 BY 1 UNTIL A >
           CHR-IND.
           WRITE OUT01
           GO TO R1.

       Q2-0. 
           MOVE CHR-AMOUNT(A) TO CLAIM-TOT MOVE 0 TO FLAGP.
           PERFORM PH3 VARYING X FROM 1 BY 1 UNTIL X > PHR-IND.
           IF (CLAIM-TOT NOT = 0) OR (FLAGP NOT = 0) GO TO Q2
           ELSE GO TO Q10.

******* WRITE THE DETAIL LINES ON BILL FOR CHARGES *******
       Q2. 
           IF LINE-CTR > 14
           MOVE 1 TO TO-2
           MOVE CHR-IND TO A
           GO TO Q10.
           ADD 1 TO LINE-CTR.
           PERFORM T24 THRU T30 VARYING X FROM 1 BY 1 UNTIL X > PHR-IND.

       Q10. 
           EXIT.

       T24. IF PHR-CLAIM(X) NOT = CHR-CLAIM(A) GO TO T30.
           IF LINE-CTR > 14
           MOVE 1 TO TO-2
           MOVE CHR-IND TO A
           MOVE PHR-IND TO X
           GO TO T30.
           ADD 1 TO LINE-CTR.

       T30. 
           EXIT.

       CC1.
           MOVE CHR-AMOUNT(X) TO CLAIM-TOT.
           PERFORM PH2 VARYING Y FROM 1 BY 1 UNTIL Y > PHR-IND.

           IF CHR-ASSIGN(X) = "A"
             ADD CLAIM-TOT TO X-INSPEND 
             GO TO CC-EXIT.

           MOVE CHR-DATE-A(X) TO DAY-TEST-1.
           MOVE 0 TO D.
           DIVIDE DY1 BY 4 GIVING B REMAINDER D

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
             ON SIZE ERROR MOVE 998 TO DAYS.
      *    DISPLAY DAYS " " PF3 " " CLAIM-TOT " " G-BALCOL
           ADD CLAIM-TOT TO G-BALCUR.

       CC-EXIT.
           EXIT.

       PH2.
           IF CHR-CLAIM(X) = PHR-CLAIM(Y)
             ADD PHR-AMOUNT(Y) CLAIM-TOT GIVING CLAIM-TOT.

       PH3.
           IF CHR-CLAIM(A) = PHR-CLAIM(X) 
             ADD PHR-AMOUNT(X) TO CLAIM-TOT.

           IF (PHR-DATE(X) > SUM-DATE) AND (CHR-CLAIM(A) =
             PHR-CLAIM(X)) 
      *    DISPLAY PHR-DATE(X) " " SUM-DATE
               MOVE 1 TO FLAGP 
               MOVE PHR-IND TO X.

       PH4. 
           IF CHR-CLAIM(A) = PHR-CLAIM(Y)
             ADD PHR-AMOUNT(Y) TO CLAIM-TOT.
           
           WRITE OUT01 
           GO TO R1.

       R20.
           CLOSE tb-bill paycur charcur OUT-FILE billdate garfile.
           STOP RUN.
