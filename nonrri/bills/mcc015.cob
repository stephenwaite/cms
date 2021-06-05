      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MCC015.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAGE-PLUS ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT INSFILE ASSIGN TO "S35"     ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC        RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT BILLDATE ASSIGN TO "S40" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT TB-BILL ASSIGN TO "S45" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT OUT-FILE ASSIGN TO "S50" ORGANIZATION 
           LINE SEQUENTIAL.
           SELECT PAYCUR ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.
           SELECT CHARCUR ASSIGN TO "S60"     ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC        RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT GARFILE ASSIGN TO "S65"     ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC        RECORD KEY IS G-GARNO
           LOCK MODE MANUAL.
           SELECT PATFILE ASSIGN TO "S70"     ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC        RECORD KEY IS P-PATNO
           ALTERNATE RECORD KEY IS P-GARNO WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT PROCFILE ASSIGN TO "S75"      ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC        RECORD KEY IS PROC-KEY
           LOCK MODE MANUAL.
           SELECT BILLPARM ASSIGN TO "S80" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT DIAGFILE ASSIGN TO "S85" ORGANIZATION IS INDEXED
           ACCESS IS RANDOM     RECORD KEY IS DIAG-KEY
           ALTERNATE RECORD KEY IS DIAG-TITLE WITH DUPLICATES
           LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
       FD  GARFILE
      *    BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS GARFILE01.
       01  GARFILE01.
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


       FD  CHARCUR.
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
       FD  DIAGFILE
           BLOCK CONTAINS 8 RECORDS
           DATA RECORD IS DIAG01.
       01  DIAG01.
           02 DIAG-KEY PIC X(7).
           02 DIAG-TITLE PIC X(61).
           02 DIAG-MEDB PIC X(5).
       FD  PAGE-PLUS
           DATA RECORD IS PAGE-PLUS01.
       01  PAGE-PLUS01.
           02 PP-0 PIC X.
           02 FILLER PIC X VALUE SPACE.
           02 PP-1 PIC X(24).
           02 FILLER PIC X VALUE SPACE.
           02 PP-2 PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 PP-3 PIC X(9).
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
           02 THIS-CYCLE PIC X(4).
           02 FILLER PIC X.
           02 DD-0 PIC X.
           02 DD-1 PIC X.
           02 DD-2 PIC X.
           02 DD-3 PIC X.
           02 DD-4 PIC X.
           02 DD-5 PIC X.
           02 DD-6 PIC X.
       FD  TB-BILL
           DATA RECORD IS TB-BILL01.
       01  TB-BILL01.
           02 TB-1 PIC X(8).
           02 FILLER PIC X(33).
           02 TB-4 PIC X.
       FD PATFILE
           BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS P-MASTER.
       01 P-MASTER.
           02 P-PATNO PIC X(8).
           02 P-GARNO PIC X(8).
           02 P-PATNAME PIC X(24).
           02 P-SEX PIC X.
           02 P-RELATE PIC X.
           02 P-MSTAT PIC X.
           02 P-DOB PIC X(8).
       FD OUT-FILE
           DATA RECORD IS OUT01.
       01  OUT01 PIC X(84).
       01  CHNL-2 PIC X.
       01  CHNL-3 PIC X.
       FD  PROCFILE
           DATA RECORD IS PROC01.
       01  PROC01.
           02 PROC-KEY PIC X(7).
           02 PROC-OLD PIC X(7).
           02 PROC-TYPE PIC X.
           02 PROC-BCBS PIC X(4).
           02 PROC-TITLE PIC X(28).
           02 PROC-AMOUNT PIC S9(4)V99.
           02 CARE-AMOUNT PIC S9(4)V99.
       WORKING-STORAGE SECTION.
       01  CHR01.
           02 CHR02 OCCURS 500 TIMES.
             03 CHR-DATE-T PIC X(8).
             03 CHR-DATE-A PIC X(8).
             03 CHR-CLAIM PIC X(6).
             03 CHR-AMOUNT PIC S9(4)V99.
             03 CHR-PAYCODE PIC X(3).
             03 CHR-PROC PIC X(7).
             03 CHR-DIAG PIC X(7).
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
       01 TAB2001.
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
       01  LINE-0.
           02 L0F1 PIC X(10) VALUE SPACE.
           02 L0F2 PIC X(24).
           02 FILLER PIC X(29) VALUE SPACE.
           02 L1F3 PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 L1F7 PIC XX.
       01 LINE-1.
           02 FILLER PIC X(8) VALUE SPACE.
           02 F11 PIC X(2) VALUE SPACE.
           02 L1F2 PIC X(22).
           02 F12 PIC X(22) VALUE SPACE.
           02 L1F1 PIC X(8).
           02 F13 PIC X VALUE SPACE.
           02 L1F4 PIC X.
           02 L1F5 PIC X.
           02 L1F51 PIC X.
           02 FILLER PIC X VALUE SPACE.
           02 L2F3 PIC X(10).
       01 LINE-2.
           02 F21 PIC X(10) VALUE SPACE.
           02 L2F2 PIC X(22).
           02 F22 PIC X(3) VALUE SPACE.
           02 F23 PIC X(3) VALUE SPACE.
           02 FILLER PIC X(25) VALUE SPACE.
           02 L2F4 PIC XXX.
           02 F24 PIC X VALUE SPACE.
           02 L2F5 PIC X(12).
       01  LINE-3.
           02 FILLER PIC X(10) VALUE SPACE.
           02 L3F1 PIC X(34).
           02 FILLER PIC X(10) VALUE SPACE.
           02 L2F1 PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 L3F2 PIC XXX.
           02 FILLER PIC X VALUE SPACE.
           02 L3F5 PIC X(12).
       01 LINE-PHONE.
           02 FILLER PIC X(63) VALUE SPACE.
           02 L0F3 PIC X(10).
       01  LINE-4.
           02 L4F1 PIC X(8).
           02 F41 PIC X(9).
           02 L4F2 PIC X(24).
           02 F42 PIC X(3).
           02 L4F3 PIC X(5).
           02 F43 PIC X(2).
           02 L4F4 PIC XXX.
           02 F44 PIC X(10).
           02 L4F5 PIC X(9).
       01 LINE-5.
           02 L5F1 PIC X(8).
           02 F51 PIC X.
           02 L5F22 PIC X(22).
           02 F52 PIC X.
           02 L5F23 PIC X(13).
           02 L5F3 PIC XX.
           02 F53 PIC X(17).
           02 L5F4 PIC ZZZ9.99CR.
       01  LINE-6.
           02 L6F1 PIC X(8).
           02 F61 PIC XX.
           02 L6F2 PIC X(28).
           02 F63 PIC XX.
           02 L6F4 PIC X(5).
           02 F64 PIC XX.
           02 L6-PROC PIC X(7).
           02 F65 PIC X.
           02 L6F6 PIC ZZZ9.99CR.
           02 FILLER PIC X(10) VALUE SPACE.
           02 L6F7X PIC X(9).
       01  LINE-6P.
           02 FILLER PIC X(65).
           02 L6PF1 PIC X(11).
       01  LINE-6A.
           02 F6A1 PIC X(9).
           02 L6AF1 PIC X(9).
           02 L6AF2 PIC X(15).
       01  LINE-7.
           02 L7F1 PIC ZZZZ9.99CR.
           02 F71 PIC X.
           02 L7F2 PIC ZZZ9.99CR.
           02 F72 PIC X.
           02 L7F3 PIC ZZZ9.99CR.
           02 F73 PIC X.
           02 L7F4 PIC ZZZ9.99CR.
           02 F74 PIC X.
           02 L7F5 PIC ZZZ9.99CR.
           02 F75 PIC X.
           02 L7F6 PIC ZZZ9.99CR.
           02 FILLER PIC X(11) VALUE SPACE.
           02 L7F7 PIC ZZZZ9.99CR.
       01 LINE-8.
           02 L8F1 PIC X(24).
           02 F81 PIC X.
           02 L8F2 PIC X(8).
           02 F82 PIC X.
           02 L8F3 PIC 999.
           02 F83 PIC X.
           02 L8F4 PIC X(27).
       01  WSL1F1.
           02 WSL1F1M PIC XX.
           02 FILLER PIC X VALUE " ".
           02 WSL1F1D PIC XX.
           02 FILLER PIC X VALUE " ".
           02 WSL1F1Y PIC XX.
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
       01 HOLD-MASTER PIC X(187).
       01  FLAGP PIC 9.
       01  FLAG-PAY PIC 9.
       01  PAYDATE6 PIC X(6).
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
       01 LLLL PIC X(8).
       01     RIGHT-2 PIC XX JUST RIGHT.
       01     RIGHT-3 PIC XXX JUST RIGHT.
       01     RIGHT-4 PIC X(4) JUST RIGHT.
       01     ALF-1 PIC X.
       01     ALF-2 PIC XX.
       01     ALF-3 PIC XXX.
       01     ALF-4 PIC XXXX.
       01  ALF-5 PIC X(5).
       01     ALF-9. 
           02 ALF-91 PIC X(5).
           02 ALF-92 PIC X(4).
       01     ALF-9X. 
           02 ALF-9X1 PIC X(5).
           02 ALF-9X-DASH PIC X.
           02 ALF-9X2 PIC X(4).

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
       01  PAB PIC S9(5)V99.
       01  BILLDATE6 PIC XXXXXX.
       01  DATE6 PIC XXXXXX.
       01  RUNBAL PIC S9(5)V99.
       01  L6F7 PIC ZZZ9.99CR.
       01  CHNL-X PIC X VALUE H"1A".
       01  CHNL-Y PIC X VALUE H"1C".
       PROCEDURE DIVISION.
       P0.
           OPEN OUTPUT PAGE-PLUS.
           OPEN OUTPUT OUT-FILE.
      *     OPEN INPUT CHARCUR GARFILE.
           OPEN I-O GARFILE CHARCUR.
           OPEN INPUT PAYCUR.
           OPEN INPUT PROCFILE DIAGFILE.
           OPEN INPUT PATFILE INSFILE.
           OPEN INPUT TB-BILL.
           OPEN INPUT BILLDATE.
           OPEN INPUT BILLPARM.
           READ BILLPARM AT END DISPLAY "NO BILLPARM" GO TO R20.
           ACCEPT BILL-DATE FROM CENTURY-DATE.
           MOVE BILL-DATE TO TEST-DATE.
           MOVE CORR TEST-DATE TO INPUT-DATE.
           MOVE T-MM OF INPUT-DATE TO WSL1F1M.
           MOVE T-DD OF INPUT-DATE TO WSL1F1D.
           MOVE T-YY OF INPUT-DATE TO WSL1F1Y.
           MOVE WSL1F1 TO LLLL.
           READ BILLDATE AT END DISPLAY "NO BILLDATE" GO TO R20.
           MOVE BILL-DATE TO BILLDATE6.
           MOVE BILL-DATE TO DAY-TEST-1 MOVE 0 TO DD1
           MOVE DAY-TEST-1 TO SUM-DATE.
       R1.  READ TB-BILL AT END GO TO R20.
           MOVE TB-1 TO G-GARNO.
           READ GARFILE INVALID
           DISPLAY G-GARNO " NOT RETURNED FROM SORT" GO TO R1.
******* READ IN ALL   RECORDS TO TABLES FOR THIS GUARANTOR *******
           MOVE SPACES TO L2F1 L3F1
           MOVE 0 TO XYZ RUNBAL
           MOVE 0 TO CHR-IND PHR-IND LINE-CTR 
           G-BALCUR G-BAL30 G-BAL60 G-BALCOL X-INSPEND
           TMC  PAB
           MOVE 1 TO BILL-PAGE
           MOVE 0 TO XYZ.
           MOVE BILL-DATE TO BILLDATE6.
           MOVE G-GARNO TO CC-KEY8
           MOVE XYZ TO CC-KEY3
           START CHARCUR KEY > CHARCUR-KEY INVALID GO TO R7.
       R6. READ CHARCUR NEXT AT END GO TO R7.
           IF G-GARNO NOT = CC-KEY8 GO TO R7.
           ADD 1 TO CHR-IND.
           IF CHR-IND > 500 DISPLAY G-GARNO " " G-GARNAME " CHRS"
           GO TO R1.
           IF CC-DATE-A = "00000000" MOVE BILL-DATE TO
           CHR-DATE-A(CHR-IND)
           ELSE MOVE CC-DATE-A TO CHR-DATE-A(CHR-IND).
           IF CC-DIAG = "0000000" MOVE CC-DIAG TO DIAG-MEDB
           ELSE MOVE CC-DIAG TO DIAG-KEY
           READ DIAGFILE INVALID MOVE CC-DIAG TO DIAG-MEDB.
           MOVE DIAG-MEDB TO CHR-DIAG(CHR-IND)
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
       R7. MOVE 0 TO XYZ.
           MOVE G-GARNO TO PC-KEY8
           MOVE XYZ TO PC-KEY3
           START PAYCUR KEY > PAYCUR-KEY INVALID GO TO R9.
       R8. READ PAYCUR NEXT AT END GO TO R9.
           IF G-GARNO NOT = PC-KEY8 GO TO R9.
           ADD 1 TO PHR-IND.
           IF PHR-IND > 800 DISPLAY G-GARNO " "  G-GARNAME " PAY"
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


******* SORT ALL TABLES BY DATE *******

       R12. IF CHR-IND > 1
           SUBTRACT 1 FROM CHR-IND GIVING Y
           PERFORM S10 VARYING X FROM 1 BY 1 UNTIL X > Y.
           IF PHR-IND > 1
           SUBTRACT 1 FROM PHR-IND GIVING Y
           PERFORM S12 VARYING X FROM 1 BY 1 UNTIL X > Y.
       FF. GO TO R13.

******* START WRITING OUTPUT *******
       R13. ADD G-BALCUR G-BAL30 G-BAL60 G-BALCOL GIVING AMOUNT-DUE.

           MOVE SPACES TO LINE-1 LINE-2 LINE-3 LINE-4 LINE-5 LINE-6
           LINE-7 LINE-8 LINE-6A LINE-PHONE.
           MOVE SPACES TO F11 F12 F13.
           MOVE TB-4 TO L1F4.
           MOVE G-DUNNING TO L1F5.
           MOVE G-COLLT TO L1F51.
           MOVE G-GARNAME TO TEST-BUSNAME.
           IF TEST-BUSNAME-1 = "1" MOVE TEST-BUSNAME-2
           TO L0F2 GO TO R14.
           MOVE SPACES TO NAME-LAST NAME-FIRST NAME-MIDDLE
           UNSTRING G-GARNAME DELIMITED BY ";" INTO NAME-LAST
           NAME-FIRST NAME-MIDDLE.
           MOVE SPACES TO TABA2401.
           MOVE NAME-LAST TO TABA2401.
           PERFORM T5 VARYING C FROM 24 BY -1 UNTIL C < 1.
           MOVE TABA2401 TO NAME-LAST.
           MOVE SPACES TO TABA2401.
           MOVE NAME-FIRST TO TABA2401.
           PERFORM T5 VARYING C FROM 24 BY -1 UNTIL C < 1.
           MOVE TABA2401 TO NAME-FIRST.
           MOVE SPACES TO TABA2401.
           MOVE NAME-MIDDLE TO TABA2401.
           PERFORM T5 VARYING C FROM 1 BY -1 UNTIL C < 1.
           MOVE TABA2401 TO NAME-MIDDLE.
           MOVE SPACES TO L0F2
           IF NAME-MIDDLE = SPACES
           STRING NAME-FIRST " " NAME-LAST DELIMITED BY "@" INTO L0F2
           ELSE STRING NAME-FIRST " " NAME-MIDDLE " " NAME-LAST
           DELIMITED BY "@" INTO L0F2.
       R14. MOVE G-GARNO TO L1F3.
           MOVE BILL-DATE TO TEST-DATE.
           MOVE CORR TEST-DATE TO INPUT-DATE.
           MOVE T-MM OF INPUT-DATE TO WSL1F1M.
           MOVE T-DD OF INPUT-DATE TO WSL1F1D.
           MOVE T-YY OF INPUT-DATE TO WSL1F1Y.
           MOVE WSL1F1 TO L2F1 MOVE LLLL TO L1F1.
           MOVE SPACES TO WSL1F1.
           WRITE OUT01 FROM LINE-0 AFTER PAGE.
           MOVE G-BILLADD TO L1F2.
           WRITE OUT01 FROM LINE-1
           MOVE G-STREET TO L2F2.
           MOVE G-LASTBILL TO L2F3.
           MOVE G-PRINS TO L2F4.
           MOVE G-PRIPOL TO L2F5.
           IF G-PRINS = "003"
           MOVE SPACE TO L2F5.
           WRITE OUT01 FROM LINE-2.
           MOVE SPACES TO TAB2001.
           MOVE G-CITY TO TAB2001.
           MOVE G-STATE TO AT-3-1.
           MOVE "@" TO AT-3-2.
           MOVE SPACES TO L3F1.
           MOVE SPACE TO ALF-9X
           IF G-ZIP = ZEROES GO TO R14-0.
           IF G-ZIP NOT NUMERIC MOVE G-ZIP TO ALF-9X GO TO R14-0.
           MOVE G-ZIP TO ALF-9.
           MOVE ALF-91 TO ALF-9X1 
           MOVE "-" TO ALF-9X-DASH
           MOVE ALF-92 TO ALF-9X2.
       R14-0.
           IF TAB20(18) NOT = " " MOVE "," TO TAB20(19)
           MOVE "@" TO TAB20(20)
           STRING TAB2001 " " AT-3 " " ALF-9X DELIMITED BY "@" INTO
           L3F1 GO TO R14-1.
           PERFORM Q1 VARYING C FROM 17 BY -1 UNTIL C < 1.
           STRING TAB2001 " " AT-3 " " ALF-9X DELIMITED BY "@" INTO
           L3F1.
       R14-1.
           INSPECT L3F1 REPLACING ALL "," BY " ".
           MOVE G-SEINS TO L3F2.
           MOVE G-SECPOL TO L3F5.
           IF G-SEINS = "003"
           MOVE SPACE TO L3F5.
           WRITE OUT01 FROM LINE-3.
           MOVE G-PHONE TO L0F3
           WRITE OUT01 FROM LINE-PHONE.
           WRITE CHNL-2 FROM CHNL-X.
       R15. PERFORM Q2 THRU Q10 VARYING A FROM 1 BY 1 UNTIL A >
           CHR-IND.
           IF G-BALCUR < 0 ADD G-BALCUR TO G-BALCOL
           MOVE 0 TO G-BALCUR.
           IF G-BAL30 < 0 ADD G-BAL30 TO G-BALCOL
           MOVE 0 TO G-BAL30.
           IF G-BAL60 < 0 ADD G-BAL60 TO G-BALCOL
           MOVE 0 TO G-BAL60.
           IF G-BALCOL < 0 ADD G-BALCOL TO G-BAL60
           MOVE 0 TO G-BALCOL.
           IF G-BAL60 < 0 ADD G-BAL60 TO G-BAL30
           MOVE 0 TO G-BAL60.
           IF G-BAL30 < 0 ADD G-BAL30 TO G-BALCUR
           MOVE 0 TO G-BAL30.
           IF (LINE-CTR < 15)
           AND (G-BAL60 > 0 AND G-DUNNING = "1") MOVE SPACE TO OUT01
           MOVE "PLEASE CONTACT US REGARDING YOUR OVERDUE ACCOUNT."
           TO OUT01 WRITE OUT01
           ADD 1 TO LINE-CTR.
           ADD AMOUNT-DUE X-INSPEND GIVING BAL-FWD.
           MOVE BAL-FWD TO L7F1
           MOVE X-INSPEND TO L7F2
           MOVE G-BALCUR TO L7F3
           MOVE G-BAL30 TO L7F4
           MOVE G-BAL60 TO L7F5
           MOVE G-BALCOL TO L7F6
           MOVE AMOUNT-DUE TO L7F7
           MOVE SPACES TO F71 F72 F73 F74 F75.
           IF LINE-CTR = 15 THEN
           WRITE OUT01 FROM LINE-7 AFTER 2
           ELSE WRITE CHNL-3 FROM CHNL-Y
           WRITE OUT01 FROM LINE-7.
           MOVE SPACES TO L8F1.
           IF BILL-PAGE > 1 MOVE G-GARNAME TO PP-1
           MOVE G-GARNO TO PP-2 MOVE BILL-PAGE TO PP-3
           MOVE TB-4 TO PP-0
           WRITE PAGE-PLUS01.
           ADD 1 TO BILL-COUNT.
           IF TEST-BUSNAME-1 = "1" MOVE TEST-BUSNAME-2 TO L8F1
           ELSE MOVE G-GARNAME TO L8F1.
           MOVE G-GARNO TO L8F2
           MOVE BILL-PAGE TO L8F3
           INSPECT L8F3 REPLACING LEADING "0" BY " ".
           MOVE SPACES TO F81 F82 F83.
           IF G-BALCOL > 0 MOVE "                         "
           TO L8F4 GO TO R15-1.
           IF G-BAL60 > 0 MOVE "                           "
           TO L8F4 GO TO R15-1.
           IF G-BAL30 > 0 MOVE "                         "
           TO L8F4 GO TO R15-1.
           MOVE SPACES TO L8F4.
       R15-1.
           WRITE OUT01 FROM LINE-8  AFTER 2.
      *     GO TO R1.
           MOVE G-GARNO TO ALF-8.
           MOVE 0 TO X Y.
           MOVE BILL-DATE TO G-LASTBILL.
           IF G-ASSIGNM = "2" MOVE "1" TO G-ASSIGNM.
      *     REWRITE GARFILE01.
           STRING ALF-8 "000" DELIMITED BY "@" INTO CHARCUR-KEY.
           START CHARCUR KEY > CHARCUR-KEY INVALID GO TO R1.
       R16.  READ CHARCUR NEXT AT END GO TO R1.
           IF CC-KEY8 NOT = ALF-8 GO TO R1.
           IF (CC-ASSIGN = "A") AND
           (CC-REC-STAT = "1" OR "3") GO TO R16.
           IF CC-ASSIGN = "A" GO TO R16-1.
           IF (CC-REC-STAT = "1" OR "3") 
           AND (CC-DATE-A NOT = "00000000") GO TO R16.
           IF CC-DATE-A = "00000000"
           MOVE BILL-DATE TO CC-DATE-A.
       R16-1.
           IF CC-REC-STAT = "0" MOVE "1" TO CC-REC-STAT.
           IF CC-REC-STAT = "2" MOVE "3" TO CC-REC-STAT.
      *     REWRITE CHARCUR01
           GO TO R16.
       R20.
           CLOSE CHARCUR GARFILE PAGE-PLUS OUT-FILE 
           STOP RUN.

******* FIND LAST CHARACTER IN CITY NAME *******
       Q1. IF TAB20(C) NOT = " "
           ADD 1 C GIVING B
           MOVE "," TO TAB20(B)
           ADD 1 TO B
           MOVE "@" TO TAB20(B)
           MOVE 1 TO C.

******* WRITE THE DETAIL LINES ON BILL FOR CHARGES *******
       Q2. IF CHR-REC-STAT(A) NOT = "X" GO TO Q10.
           IF CHR-ASSIGN(A) = "A" 
           MOVE 0 TO FLAG
           PERFORM PH5 VARYING Y FROM 1 BY 1 UNTIL Y > PHR-IND
            IF FLAG = 0 
             GO TO Q10
            END-IF
           END-IF
           MOVE CHR-DATE-T(A) TO TEST-DATE.
           MOVE CORR TEST-DATE TO INPUT-DATE.
           INSPECT T-MM OF INPUT-DATE REPLACING LEADING "0" BY " ".
           MOVE T-MM OF INPUT-DATE TO WSL1F1M.
           MOVE T-DD OF INPUT-DATE TO WSL1F1D.
           MOVE T-YY OF INPUT-DATE TO WSL1F1Y.
           MOVE WSL1F1 TO L6F1.
           MOVE CHR-PROC(A) TO PROC-KEY.
           READ PROCFILE INVALID MOVE SPACE TO PROC-TITLE.
           MOVE PROC-TITLE TO L6F2.
           MOVE SPACE TO L6F4
      *     MOVE CHR-DIAG(A) TO L6F4.
           MOVE CHR-PROC(A) TO L6-PROC
           MOVE CHR-AMOUNT(A) TO L6F6.
******* SEARCH PAYMENT TABLE FOR PAYMENTS AGAINST THIS CLAIM *******
       Q51.
           MOVE 0 TO FLAG.
           IF LINE-CTR >  14
      *    WRITE CHNL-3 FROM CHNL-Y
           MOVE G-GARNO TO L4F1 MOVE SPACE TO F41
           MOVE L0F2 TO L4F2 MOVE "PAGE " TO L4F3
           MOVE SPACE TO F42
           MOVE BILL-PAGE TO L4F4
           INSPECT L4F4 REPLACING LEADING "0" BY " "
           MOVE SPACE TO F43 MOVE "CONTINUED" TO L4F5
           WRITE OUT01 FROM LINE-4 AFTER 4
           WRITE OUT01 FROM LINE-0 AFTER PAGE
           WRITE OUT01 FROM LINE-1
           WRITE OUT01 FROM LINE-2
           WRITE OUT01 FROM LINE-3
           MOVE BILL-PAGE TO L4F4
           WRITE CHNL-2 FROM CHNL-X
           MOVE 0 TO LINE-CTR
           ADD 1 TO BILL-PAGE.
           IF CHR-ASSIGN(A) = "A"
           MOVE LINE-6 TO LINE-6P
           MOVE "PENDING INS" TO L6PF1
           WRITE OUT01 FROM LINE-6P
           GO TO Q90.
           MOVE CHR-AMOUNT(A) TO CLAIM-TOT
           PERFORM PH4 VARYING Y FROM 1 BY 1 UNTIL Y > PHR-IND
           MOVE CLAIM-TOT TO L6F7
           IF CLAIM-TOT = 0 MOVE SPACE TO L6F7X ELSE MOVE L6F7 TO L6F7X.
           WRITE OUT01 FROM LINE-6.
       Q90.
           ADD 1 TO LINE-CTR.
           PERFORM T24 THRU Z10 VARYING B FROM 1 BY 1 UNTIL B >
           PHR-IND.
       Q10. EXIT.
       T5. IF TABA24(C) NOT = " "
           ADD 1 TO C
           MOVE "@" TO TABA24(C)
           MOVE 1 TO C.
       T22. ADD 1 TO D MOVE TABA24(Z) TO TAB11(D).
       T23. ADD 1 TO D MOVE TABB24(Z) TO TAB11(D).
       T24. IF PHR-CLAIM(B) NOT = CHR-CLAIM(A) GO TO Z10.
           GO TO Z2-1.
       T20. IF TABA24(C) NOT = " " MOVE C TO X
           MOVE 1 TO C.
       T21. IF TABB24(C) NOT = " " MOVE C TO Z
           MOVE 1 TO C.
       Z2-1.
           IF LINE-CTR >  14
           MOVE G-GARNO TO L4F1 MOVE SPACE TO F41
           MOVE L0F2 TO L4F2 MOVE "PAGE " TO L4F3
           MOVE SPACE TO F42
           MOVE BILL-PAGE TO L4F4
           INSPECT L4F4 REPLACING LEADING "0" BY " "
           MOVE SPACE TO F43 MOVE "CONTINUED" TO L4F5
           WRITE OUT01 FROM LINE-4 AFTER 4
           WRITE OUT01 FROM LINE-0 AFTER PAGE
           WRITE OUT01 FROM LINE-1
           WRITE OUT01 FROM LINE-2
           WRITE OUT01 FROM LINE-3
           MOVE BILL-PAGE TO L4F4
           WRITE CHNL-2 FROM CHNL-X
           MOVE 0 TO LINE-CTR
           ADD 1 TO BILL-PAGE.
       T25. MOVE PHR-DATE(B) TO TEST-DATE.
           MOVE CORR TEST-DATE TO INPUT-DATE.
           INSPECT T-MM OF INPUT-DATE REPLACING LEADING "0" BY " ".
           MOVE T-MM OF INPUT-DATE TO WSL1F1M.
           MOVE T-DD OF INPUT-DATE TO WSL1F1D.
           MOVE T-YY OF INPUT-DATE TO WSL1F1Y.
           MOVE WSL1F1 TO L5F1.
      *    MOVE "PAYMT " TO L5F21.
           MOVE PHR-PAYCODE(B) TO INS-KEY
           READ INSFILE INVALID MOVE SPACE TO INS-NAME.
           MOVE INS-NAME TO L5F22.
           IF L5F22 = SPACE MOVE "OTHER INSURANCE " TO L5F22.
           MOVE SPACE TO L5F23.
           IF PHR-DENIAL(B) = "DD" MOVE "DEDCT NOT MET"
           TO L5F23.
           IF PHR-DENIAL(B) = "NC" MOVE "NONCOVER SERV"
           TO L5F23.
           IF PHR-DENIAL(B) = "NR" MOVE "NO INS. REPLY"
           TO L5F23.
           IF PHR-DENIAL(B) = "CE" MOVE "INVLD CERT NO"
           TO L5F23.
           IF PHR-DENIAL(B) = "DA" MOVE "NONCOVER DATE"
           TO L5F23.
           IF PHR-DENIAL(B) = "BE" MOVE "BILLING ERROR" TO
           L5F23.
           IF PHR-DENIAL(B) = "CP" MOVE "COPAY        "
           TO L5F23.
           IF PHR-DENIAL(B) = "14" MOVE "REDUCTION    " TO
           L5F23.
       T26. MOVE PHR-AMOUNT(B) TO L5F4.
           MOVE SPACE TO L5F3.
           MOVE SPACES TO F51 F52 F53.
           WRITE OUT01 FROM LINE-5.
           ADD 1 TO LINE-CTR.
           MOVE "XXXXXXXX" TO PHR-DATE(B).
       Z10. EXIT.
       S10. ADD 1 X GIVING Z.
           PERFORM S11 VARYING A FROM Z BY 1 UNTIL A > CHR-IND.
       S11. IF CHR-DATE-T(X) > CHR-DATE-T(A)
           MOVE CHR-DATE-T(X) TO ALF-8
           MOVE CHR-DATE-T(A) TO CHR-DATE-T(X)
           MOVE ALF-8 TO CHR-DATE-T(A)
           MOVE CHR-DATE-A(X) TO ALF-8
           MOVE CHR-DATE-A(A) TO CHR-DATE-A(X)
           MOVE ALF-8 TO CHR-DATE-A(A)
           MOVE CHR-CLAIM(X) TO ALF-6
           MOVE CHR-CLAIM(A) TO CHR-CLAIM(X)
           MOVE ALF-6 TO CHR-CLAIM(A)
           MOVE CHR-DOCR(X) TO ALF-3
           MOVE CHR-DOCR(A) TO CHR-DOCR(X)
           MOVE ALF-3 TO CHR-DOCR(A)
           MOVE CHR-AMOUNT(X) TO SNUM-6
           MOVE CHR-AMOUNT(A) TO CHR-AMOUNT(X)
           MOVE SNUM-6 TO CHR-AMOUNT(A)
           MOVE CHR-PAYCODE(X) TO ALF-3
           MOVE CHR-PAYCODE(A) TO CHR-PAYCODE(X)
           MOVE ALF-3 TO CHR-PAYCODE(A)
           MOVE CHR-PROC(X) TO ALF-7
           MOVE CHR-PROC(A) TO CHR-PROC(X)
           MOVE ALF-7 TO CHR-PROC(A)
           MOVE CHR-DIAG(X) TO ALF-7
           MOVE CHR-DIAG(A) TO CHR-DIAG(X)
           MOVE ALF-7 TO CHR-DIAG(A)
           MOVE CHR-PATID(X) TO ALF-8
           MOVE CHR-PATID(A) TO CHR-PATID(X)
           MOVE ALF-8 TO CHR-PATID(A)
           MOVE CHR-REC-STAT(X) TO ALF-1
           MOVE CHR-REC-STAT(A) TO CHR-REC-STAT(X)
           MOVE ALF-1 TO CHR-REC-STAT(A)
           MOVE CHR-ASSIGN(X) TO ALF-1
           MOVE CHR-ASSIGN(A) TO CHR-ASSIGN(X)
           MOVE ALF-1 TO CHR-ASSIGN(A).
       S12. ADD 1 X GIVING Z.
           PERFORM S13 VARYING A FROM Z BY 1 UNTIL A > PHR-IND.
       S13. IF (PHR-DATE(X) > PHR-DATE(A)) OR
           (PHR-DATE(X) = PHR-DATE(A) AND PHR-PAYCODE(X) >
           PHR-PAYCODE(A))
           MOVE PHR-DATE(X) TO ALF-8
           MOVE PHR-DATE(A) TO PHR-DATE(X)
           MOVE ALF-8 TO PHR-DATE(A)
           MOVE PHR-CLAIM(X) TO ALF-6
           MOVE PHR-CLAIM(A) TO PHR-CLAIM(X)
           MOVE ALF-6 TO PHR-CLAIM(A)
           MOVE PHR-AMOUNT(X) TO SNUM-6
           MOVE PHR-AMOUNT(A) TO PHR-AMOUNT(X)
           MOVE SNUM-6 TO PHR-AMOUNT(A)
           MOVE PHR-PAYCODE(X) TO ALF-3
           MOVE PHR-PAYCODE(A) TO PHR-PAYCODE(X)
           MOVE ALF-3 TO PHR-PAYCODE(A)
           MOVE PHR-STAT(X) TO ALF-1
           MOVE PHR-STAT(A) TO PHR-STAT(X)
           MOVE ALF-1 TO PHR-STAT(A)
           MOVE PHR-DENIAL(X) TO ALF-2
           MOVE PHR-DENIAL(A) TO PHR-DENIAL(X)
           MOVE ALF-2 TO PHR-DENIAL(A).
       CC1.
           MOVE CHR-AMOUNT(X) TO CLAIM-TOT.
           MOVE CHR-DATE-T(X) TO DATE6
           MOVE "X" TO CHR-REC-STAT(X).
           MOVE 0 TO FLAG-PAY
           PERFORM PH2 THRU PH2-EXIT VARYING Y FROM 1 BY 1 UNTIL Y >
           PHR-IND.
           MOVE CHR-DATE-T(X) TO DATE6
           IF (CLAIM-TOT = 0) AND (DATE6 < BILLDATE6)
           AND (FLAG-PAY = 0)
           MOVE "3" TO CHR-REC-STAT(X).
           IF CHR-ASSIGN(X) = "A"
           ADD CLAIM-TOT TO X-INSPEND GO TO CC-EXIT.
           MOVE CHR-DATE-A(X) TO DAY-TEST-1.
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
           ON SIZE ERROR MOVE 998 TO DAYS.
           IF DAYS > PF3 ADD CLAIM-TOT TO G-BALCOL GO TO CC-EXIT.
           IF DAYS > PF2 ADD CLAIM-TOT TO G-BAL60 GO TO CC-EXIT.
           IF DAYS > PF1 ADD CLAIM-TOT TO G-BAL30 GO TO CC-EXIT.
           ADD CLAIM-TOT TO G-BALCUR.
       CC-EXIT. EXIT.
       PH2. IF CHR-CLAIM(X) NOT = PHR-CLAIM(Y) GO TO PH2-EXIT.
           ADD PHR-AMOUNT(Y) CLAIM-TOT GIVING CLAIM-TOT.
           MOVE PHR-DATE(Y) TO PAYDATE6
           IF PAYDATE6 NOT < BILLDATE6 MOVE 1 TO FLAG-PAY.
       PH2-EXIT. EXIT.
       PH4. IF CHR-CLAIM(A) = PHR-CLAIM(Y)
           ADD PHR-AMOUNT(Y) TO CLAIM-TOT.

       PH5. IF (CHR-CLAIM(A) = PHR-CLAIM(Y))
            AND (PHR-PAYCODE(Y) = "001" OR "021" OR "022")
            MOVE 1 TO FLAG
            MOVE PHR-IND TO Y.
