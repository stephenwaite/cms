      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. gra010.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DOCPARM ASSIGN TO "S30"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT COLFILE ASSIGN TO "S35"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT SMALLFILE ASSIGN TO "S40"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT CREDITFILE ASSIGN TO "S45"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT OUT-FILE2 ASSIGN TO "S50"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT BILLDATE ASSIGN TO "S60"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT DATAOUT ASSIGN TO "S65"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT PAYCUR ASSIGN TO "S70" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY.
           SELECT CHARCUR ASSIGN TO "S75" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES.
           SELECT GARFILE ASSIGN TO "S80" ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL RECORD KEY IS G-GARNO.
           SELECT BILLPARM ASSIGN TO "S85"
           ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  DATAOUT.
       01  DATAOUT01.
           02 DO-1 PIC XX.
           02 DO-2 PIC XXX.
           02 DO-3 PIC X.
           02 DO-4 PIC S9(6)V99.
           02 DO-5 PIC X(11).
       FD  DOCPARM.
       01  DOCPARM01.
           02 DOCNUM PIC 99.
           02 DOCNAME PIC X(40).
       FD  COLFILE
           DATA RECORD IS COLFILE01.
       01  COLFILE01.
           02 CLF0 PIC X(24).
           02 CLF1 PIC X(8).
           02 CLF2 PIC S9(4)V99.
           02 CLF3 PIC XXX.
           02 CLF4 PIC X(6).
           02 CLF5 PIC X(8).
       FD  CREDITFILE
           DATA RECORD IS CREDIT01.
       01  CREDIT01.
           02 CR-GARNO PIC X(8).
           02 CR-FIL1 PIC X.
           02 CR-PRINS PIC XXX.
           02 CR-FIL2 PIC X.
           02 CR-SEINS PIC XXX.
           02 CR-FIL3 PIC X.
           02 CR-NAME PIC X(24).
           02 CR-FIL4 PIC X.
           02 CR-AMT PIC X(10). 
           02 CR-FIL5 PIC X(78).
       FD  SMALLFILE.
       01  SMALL01 PIC X(130).
       FD  OUT-FILE2.
       01  OUT-FILE201 PIC X(83).
       FD  BILLPARM.
       01  BILLPARM01.
           02 PF1 PIC S999.
           02 PF2 PIC S999.
           02 PF3 PIC S999.
           02 PF4 PIC X(40).
       FD  BILLDATE.
       01  BILLDATE01.
           02 LOW-DATE PIC X(8).
           02 BILL-DATE.
             03 BD-1 PIC 9999.
             03 BD-2 PIC 99.
             03 BD-3 PIC 99.
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
           02 CC-PROC PIC X(7).
           02 CC-MOD2 PIC XX.
           02 CC-MOD3 PIC XX.
           02 CC-MOD4 PIC XX.
           02 CC-AMOUNT PIC S9(4)V99.
           02 CC-DOCR PIC X(3).
           02 CC-DOCP PIC 99.
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
           02 CC-DATE-P PIC 9(8).
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
           02 PC-DATE-E. 
            03 PC-D1 PIC 9999.
            03 PC-D2 PIC 99.
            03 PC-D3 PIC 99.
           02 PC-BATCH PIC X(6).
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
           02 G-INSPEND PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.
       WORKING-STORAGE SECTION.
       01  PHR01.
           02 PHR02 OCCURS 990 TIMES.
             03 PHR-CLAIM PIC X(6).
             03 PHR-AMOUNT PIC S9(4)V99.
             03 PHR-PAYCODE PIC XXX.
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
       01 LINE-20.
           02 L20F0 PIC X.
           02 L20F0-1 PIC X.
           02 L20F1 PIC X(24).
           02 L20F2 PIC X(8).
           02 L20F3 PIC S9(4)V99.
           02 L20F4 PIC S9(4)V99.
           02 L20F5 PIC S9(4)V99.
           02 L20F6 PIC S9(4)V99.
           02 L20F7 PIC S9(4)V99.
           02 L20F8 PIC S9(7)V99.
           02 L20LB PIC X(8).
           02 L20STAT PIC X.
           02 AGE-STAT PIC X.
       01     DAY1 PIC S9999.
       01     DAY2 PIC S9999.
       01     CLAIM-TOT PIC S9(6)V99.
       01     DAYS PIC S9999.
       01     QY1 PIC S9999.
       01     QY2 PIC S9999.
       01     QDAY1 PIC S9999.
       01     QDAY2 PIC S9999.
       01     CHR PIC 999.
       01     PHR PIC 999.
       01     PHR-IND PIC 999.
       01     CHR-IND PIC 999.
       01     TOTCHAR PIC S9(4)V99.
       01     TOTPAY PIC S9(4)V99.
       01     XYZ PIC 999.
       01     A PIC 9.
       01     B PIC 999.
       01     C PIC 999.
       01     X PIC 9999.
       01     Y PIC 9999.
       01  Z PIC 999.
       01     XX PIC 9999 VALUE 0.
       01     SNUM7 PIC S9(7)V99.
       01     SNUM72 PIC S9(7)V99 VALUE 0.
       01     SNUM73 PIC S9(7)V99 VALUE 0.
       01     SNUM74 PIC S9(7)V99 VALUE 0.
       01     SNUM75 PIC S9(7)V99 VALUE 0.
       01     SNUM76 PIC S9(7)V99 VALUE 0.
       01     NAME-L PIC X(24).
       01     NAME-F PIC X(24).
       01     NAME-M PIC X(24).
       01     XCUR PIC S9(5)V99.
       01     XBAL30 PIC S9(5)V99.
       01     XBAL60 PIC S9(5)V99.
       01     XCOL PIC S9(5)V99.
       01     XINS PIC S9(6)V99.
       01     SNUM1 PIC S9(7)V99.
       01     NEF-10 PIC Z,ZZ9.99CR.
       01     NEF-11 PIC ZZ,ZZ9.99CR.

       01 TABA2401.
            02 TABA24 PIC X OCCURS 24 TIMES.
       01 DAY-TEST-1.
           02 DY1 PIC 9999.
           02 DM1 PIC 99.
           02 DD1 PIC 99.
       01  DAY-TEST-2.
           02 DY2 PIC 9999.
           02 DM2 PIC 99.
           02 DD2 PIC 99.
       01  CNTR PIC 9999 VALUE 0.
       01  NUMOFDOCS PIC 99 VALUE 0.
       01  DOCTAB01.
           02 DOCTAB PIC X(40) OCCURS 23 TIMES.
       01  NUM-3 PIC 999.
       01  TOT-CRED PIC S9(5)V99.
       PROCEDURE DIVISION.
       P0.
           COMPUTE TOT-CRED = 0.
           OPEN INPUT DOCPARM.
           OPEN OUTPUT OUT-FILE2 SMALLFILE CREDITFILE.
           OPEN OUTPUT DATAOUT COLFILE.
           OPEN INPUT GARFILE.
           OPEN INPUT CHARCUR.
           OPEN INPUT PAYCUR.
           OPEN INPUT BILLDATE.
           OPEN INPUT BILLPARM.
           READ BILLPARM AT END DISPLAY "NO BILLPARM" GO TO Z1.
           MOVE 0 TO X.
           MOVE SPACE TO CREDIT01
           STRING  "CREDIT BALANCE ACCOUNTS: " PF4 DELIMITED BY
           "!!" INTO CREDIT01 
           WRITE CREDIT01 AFTER PAGE.
           MOVE SPACE TO CREDIT01
           MOVE "/" TO CR-FIL2
           MOVE SPACE TO SMALL01
           STRING  "SMALL BALANCE ACCOUNTS: " PF4 DELIMITED BY
           "!!" INTO SMALL01 
           WRITE SMALL01 AFTER PAGE.

           READ BILLDATE AT END DISPLAY "NO BILLDATE" GO TO Z1.
       D0. READ DOCPARM AT END GO TO R1.
           ADD 1 TO NUMOFDOCS
           MOVE DOCNAME TO DOCTAB(DOCNUM) GO TO D0.

       R1. READ GARFILE AT END GO TO Z1.
           IF G-GARNO NOT = "MCD2951G" GO TO R1.
      *    ADD 1 TO CNTR IF CNTR > 50 GO TO Z1.
           MOVE 0 TO XCUR XBAL30 XBAL60 XCOL XINS.
           MOVE 0 TO XYZ
           MOVE 0 TO CHR-IND PHR-IND
           MOVE G-GARNO TO PC-KEY8
           MOVE XYZ TO PC-KEY3
           START PAYCUR KEY > PAYCUR-KEY INVALID GO TO R2.
       R8. READ PAYCUR NEXT AT END GO TO R2.
           IF G-GARNO NOT = PC-KEY8 GO TO R2.
           IF PC-DATE-E > BILL-DATE GO TO R8.
           ADD 1 TO PHR-IND.
           IF PHR-IND > 990 DISPLAY G-GARNO " "  G-GARNAME
           GO TO R1.
           MOVE PC-CLAIM TO PHR-CLAIM(PHR-IND)
           MOVE PC-AMOUNT TO PHR-AMOUNT(PHR-IND)
           MOVE PC-PAYCODE TO PHR-PAYCODE(PHR-IND).
           IF G-DUNNING < "4" GO TO R8.
           IF ( BD-1 = PC-D1) AND ( BD-2 = PC-D2 ) AND ( PC-PAYCODE
           NOT = 18 )
           MOVE G-GARNAME TO CLF0
           MOVE G-GARNO TO CLF1
           MOVE PC-AMOUNT TO CLF2
           MOVE PC-PAYCODE TO CLF3
           MOVE PC-CLAIM TO CLF4
           MOVE PC-DATE-T TO CLF5
           WRITE COLFILE01.
           GO TO R8.
       R2. MOVE 0 TO XYZ.
           MOVE G-GARNO TO CC-KEY8
           MOVE XYZ TO CC-KEY3
           START CHARCUR KEY > CHARCUR-KEY INVALID GO TO R10.
       R6. READ CHARCUR NEXT AT END GO TO R10.
           IF G-GARNO NOT = CC-KEY8 GO TO R10.
           IF CC-DATE-P > BILL-DATE GO TO R6.
       R6-12.
           IF CC-DATE-A = ZERO MOVE BILL-DATE TO CC-DATE-A.
           PERFORM CH1 THRU CH-EXIT.
           GO TO R6.
       R10.
           ADD XINS XCUR XBAL30 XBAL60 XCOL GIVING SNUM1.
           IF SNUM1 < 0 
           MOVE G-GARNO TO CR-GARNO
           MOVE G-GARNAME TO CR-NAME
           MOVE G-PRINS TO CR-PRINS
           MOVE G-SEINS TO CR-SEINS
           ADD SNUM1 TO TOT-CRED
           MOVE SNUM1 TO NEF-10
           MOVE NEF-10 TO CR-AMT
           WRITE CREDIT01.
           IF SNUM1 > 0 AND < 2.00 
           MOVE G-GARNO TO CR-GARNO
           MOVE G-GARNAME TO CR-NAME
           MOVE G-PRINS TO CR-PRINS
           MOVE G-SEINS TO CR-SEINS
           MOVE SNUM1 TO NEF-10
           MOVE NEF-10 TO CR-AMT
           MOVE SPACE TO SMALL01
           WRITE SMALL01 FROM CREDIT01.
           IF SNUM1  = 0 GO TO R1.
           MOVE G-DUNNING TO L20F0
           MOVE G-COLLT TO L20F0-1
           MOVE G-GARNAME TO L20F1
           MOVE G-GARNO TO L20F2
           MOVE G-LASTBILL TO L20LB
           MOVE G-ACCTSTAT TO L20STAT
           MOVE XINS TO L20F3
           MOVE XCUR TO L20F4
           MOVE XBAL30 TO L20F5
           MOVE XBAL60 TO L20F6
           MOVE XCOL TO L20F7
           MOVE SNUM1 TO L20F8
           MOVE 0 TO AGE-STAT
           IF XCUR > 0   MOVE "1" TO AGE-STAT.
           IF XBAL30 > 0 MOVE "2" TO AGE-STAT.
           IF XBAL60 > 0 MOVE "3" TO AGE-STAT.
           IF XCOL > 0   MOVE "4" TO AGE-STAT.
           WRITE OUT-FILE201 FROM LINE-20
           GO TO R1.

******* START HERE TO DETERMINE AMOUNT DUE AND INSURANCE PENDING *******
******* AND AGING ON CURRENT,PAST DUE AND DELINQUENT *******

       CH1. MOVE CC-AMOUNT TO CLAIM-TOT.
           PERFORM PH2 VARYING Y FROM 1 BY 1 UNTIL Y > PHR-IND.
           IF CLAIM-TOT = 0 GO TO CH-EXIT.
******* AGING TO FIND WHICH BUCKET CLAIM BALANCE IS IN *******
       CH1-1. MOVE CC-DATE-A TO DAY-TEST-1.
           MOVE 0 TO C.
           DIVIDE DY1 BY 4 GIVING B REMAINDER C.
           IF C = 0
               COMPUTE DAY1 = LEAP-TAB(DM1) + DD1
           ELSE
               COMPUTE DAY1 = MON-TAB(DM1) + DD1
           END-IF
           MOVE BILL-DATE TO DAY-TEST-2.
           MOVE 0 TO C.
           DIVIDE DY2 BY 4 GIVING B REMAINDER C.
           IF C = 0
                COMPUTE DAY2 = LEAP-TAB(DM2) + DD2
           ELSE
                COMPUTE DAY2 = MON-TAB(DM2) + DD2
           END-IF
           MOVE DY2 TO QY2
           MOVE DY1 TO QY1
           MOVE DAY1 TO QDAY1
           MOVE DAY2 TO QDAY2.
           COMPUTE DAYS = 365 * (QY2 - QY1) + QDAY2 - QDAY1
                ON SIZE ERROR MOVE 9000 TO DAYS.
           MOVE 1 TO A.
           IF DAYS > PF3 MOVE 4 TO A
           GO TO CH2.
           IF DAYS > PF2 MOVE 3 TO A
           GO TO CH2.
           IF DAYS > PF1 MOVE 2 TO A.
       CH2.
           IF CC-ASSIGN = "A" AND A > 4 MOVE 3 TO A .
           MOVE CC-DOCP TO DO-1
           MOVE CC-PAYCODE TO DO-2
           MOVE A TO DO-3
           MOVE CLAIM-TOT TO DO-4
           MOVE CHARCUR-KEY TO DO-5
           WRITE DATAOUT01.
           IF CC-ASSIGN = "A"
           ADD CLAIM-TOT TO XINS GO TO CH-EXIT.
           IF DAYS > PF3 
             ADD CLAIM-TOT TO XCOL 
             DISPLAY CLAIM-TOT " CLAIM-TOT"
             ACCEPT OMITTED  
             GO TO CH-EXIT.
           IF DAYS > PF2 ADD CLAIM-TOT TO XBAL60 GO TO CH-EXIT.
           IF DAYS > PF1 ADD CLAIM-TOT TO XBAL30 GO TO CH-EXIT.
           ADD CLAIM-TOT TO XCUR.

       CH-EXIT.
           EXIT.

       PH2.
           IF CC-CLAIM = PHR-CLAIM(Y)
      *       DISPLAY PHR-CLAIM(Y) " " PHR-AMOUNT(Y) " " PHR-PAYCODE(Y)
      *       ACCEPT OMITTED  
             ADD PHR-AMOUNT(Y) CLAIM-TOT GIVING CLAIM-TOT.

       Z1.
           MOVE SPACE TO CREDIT01
           WRITE CREDIT01
           MOVE TOT-CRED TO NEF-11
           STRING "TOTAL CREDIT BALANCE = " NEF-11 DELIMITED
            BY SIZE INTO CREDIT01
           WRITE CREDIT01
           CLOSE SMALLFILE CREDITFILE DATAOUT
           STOP RUN.
