       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRI205SID4.
       AUTHOR. S WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT INSFILE ASSIGN TO "S30"     ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC        RECORD KEY IS INS-KEY
                ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
                ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
                ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
                ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
                ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
                ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
                LOCK MODE MANUAL.
            SELECT PAYFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
                ACCESS MODE IS DYNAMIC   RECORD KEY IS PAYFILE-KEY
                LOCK MODE MANUAL.
            SELECT PAYCUR ASSIGN TO "S40" ORGANIZATION IS INDEXED
                ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
                LOCK MODE MANUAL.
            SELECT GARFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
                ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
                LOCK MODE MANUAL.
            SELECT PATFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
                ACCESS MODE IS DYNAMIC   RECORD KEY IS P-PATNO
                ALTERNATE RECORD KEY IS P-GARNO WITH DUPLICATES
                LOCK MODE MANUAL.
            SELECT CHARCUR ASSIGN TO "S55" ORGANIZATION IS INDEXED
                ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
                ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
                LOCK MODE MANUAL.
            SELECT CHARFILE ASSIGN TO "S60" ORGANIZATION IS INDEXED
                ACCESS IS DYNAMIC   RECORD KEY IS CHARFILE-KEY
                LOCK MODE MANUAL.
                SELECT PROCFILE ASSIGN TO "S75" ORGANIZATION IS INDEXED
                ACCESS IS RANDOM RECORD KEY IS PROC-KEY
                LOCK MODE MANUAL.
            SELECT BILLDATE ASSIGN TO "S120" ORGANIZATION LINE
                SEQUENTIAL.
            SELECT OUT-FILE ASSIGN TO "S125".
            SELECT BILLPARM ASSIGN TO "S130"
                ORGANIZATION LINE SEQUENTIAL.
            SELECT PREPRINTFILE ASSIGN TO "S135"
                ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PREPRINTFILE.
       01  PREPRINTFILE01 PIC X(84).
       FD  INSFILE.
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
       FD  GARFILE.
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
       FD PATFILE.
       01 P-MASTER.
            02 P-PATNO PIC X(8).
            02 P-GARNO PIC X(8).
            02 P-PATNAME PIC X(24).
            02 P-SEX PIC X.
            02 P-RELATE PIC X.
            02 P-MSTAT PIC X.
            02 P-DOB PIC X(8).

       FD  CHARFILE.
       01  CHARFILE01.
           02 CHARFILE-KEY.
             03 CD-KEY8 PIC X(8).
             03 CD-KEY3 PIC XXX.
           02 CD-PATID PIC X(8).
           02 CD-CLAIM PIC X(6).
           02 CD-SERVICE PIC X.
           02 CD-DIAG PIC X(5).
           02 CD-PROC PIC X(7).
           02 CD-MOD2 PIC XX.
           02 CD-MOD3 PIC XX.
           02 CD-MOD4 PIC XX.
           02 CD-AMOUNT PIC S9(4)V99.
           02 CD-DOCR PIC X(3).
           02 CD-DOCP PIC X(2).
           02 CD-PAYCODE PIC XXX.
           02 CD-STAT PIC X.
           02 CD-WORK PIC XX.
           02 CD-DAT1 PIC X(8).
           02 CD-RESULT PIC X.
           02 CD-ACT PIC X.
           02 CD-SORCREF PIC X.
           02 CD-COLLT PIC X.
           02 CD-AUTH PIC X.
           02 CD-PAPER PIC X.
           02 CD-PLACE PIC X.
           02 CD-NAME PIC X(24).
           02 CD-ESPDT PIC X.
           02 CD-DATE-T PIC X(8).
           02 CD-DATE-E PIC X(8).
           02 CD-ORDER PIC X(6).
           02 CD-DX2 PIC X(5).
           02 CD-DX3 PIC X(5).
           02 CD-DATE-A PIC X(8).
           02 CD-ACC-TYPE PIC X.
           02 CD-DATE-M PIC X(8).
           02 CD-ASSIGN PIC X.
           02 CD-NEIC-ASSIGN PIC X.
           02 CD-FUTURE PIC X(6).

       FD  PAYFILE.
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


       FD  CHARCUR.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(5).
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
           02 CC-DX2 PIC X(5).
           02 CC-DX3 PIC X(5).
           02 CC-ACC-TYPE PIC X.
           02 CC-DATE-M PIC X(8).
           02 CC-ASSIGN PIC X.
           02 CC-NEIC-ASSIGN PIC X.
           02 CC-FUTURE PIC X(6).
       FD  BILLPARM.
       01  BILLPARM01.
            02 PF1 PIC S999.
            02 PF2 PIC S999.
            02 PF3 PIC S999.
            02 PF4 PIC X(40).
       FD  BILLDATE.
       01  BILLDATE01.
            02 BILL-DATE PIC X(8).
            02 BILL-CYCLE PIC X(4).
       FD  PAYCUR.
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
       FD  OUT-FILE.
       01  OUT01 PIC X(84).
       01  CHNL-2 PIC X.  
       01  CHNL-3 PIC X.
       FD  PROCFILE.
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
            02 CHR02 OCCURS 990 TIMES.
                03 CHR-DATE-T PIC X(8).
                03 CHR-DATE-A PIC X(8).
                03 CHR-CLAIM PIC X(6).
                03 CHR-AMOUNT PIC S9(4)V99.
                03 CHR-PAYCODE PIC X(3).
                03 CHR-PATID PIC X(8).
                03 CHR-PROC PIC X(7).
                    03 CHR-DIAG PIC X(7).
                03 CHR-ASSIGN PIC X.
                03 CHR-REC-STAT PIC X.
                03 CHR-TOT PIC S9(5)V99.
       01  PHR01.
            02 PHR02 OCCURS 990 TIMES.
                03 PHR-DATE PIC X(8).
                03 PHR-CLAIM PIC X(6).
                03 PHR-AMOUNT PIC S9(4)V99.
                03 PHR-PAYCODE PIC XXX.
                03 PHR-DENIAL PIC XX.
       
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

       01 DAY-TEST-1.
            02 DY1 PIC 9999.
            02 DM1 PIC 99.
            02 DD1 PIC 99.
       01  DAY-TEST-2.
            02 DY2 PIC 9999.
            02 DM2 PIC 99.
            02 DD2 PIC 99.
       01  NOINS PIC X(40) VALUE "IF YOU HAVE HEALTH INSURANCE CONTACT U
      -    "S.".                                             
       01  N7699 PIC X(71) VALUE "ATTACH THIS BILL TO YOUR INSURANCE FOR
      -    "M. YOU MUST BILL YOUR INSURANCE. ".
       01  N9398 PIC X(65) VALUE "SUBMIT THIS STATEMENT TO YOUR EMPLOYER
      -    " FOR PROMPT CLAIM PAYMENT. ".
       01  WEBILL PIC X(30) VALUE "WE HAVE BILLED YOUR INSURANCE.".
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
            02 FILLER PIC X(4) VALUE SPACE.
            02 L5F2.
            03 L5F21 PIC X(6).
            03 L5F22 PIC X(22).
            03 L5F23 PIC X(15).
            02 L5F3 PIC X(6).
            02 FILLER PIC X(3) VALUE SPACE.
            02 L5F4 PIC ZZZ9.99CR.
            02 FILLER PIC XX VALUE SPACE.
      *    02 L5F5 PIC ZZZ9.99CR.
       01  LINE-6.
            02 L6F1 PIC X(8).
            02 F61 PIC XX.
            02 L6F2 PIC X(16).
            02 F62 PIC X.
            02 L6F3 PIC X(11).
            02 F63 PIC XX.
                02 L6F4 PIC X(7).
                02 F64 PIC X.
            02 L6-PROC PIC X(7).
            02 F65 PIC X.
            02 L6F6 PIC ZZZ9.99CR.
            02 FILLER PIC X(9) VALUE SPACE.
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
            02 FILLER PIC X(12) VALUE SPACE.
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
       01  T-DATE. 
            02 YEAR4.
                04 CC-T PIC XX.
                04 YY-T PIC XX.
            02 MM-T    PIC XX.   
            02 DD-T    PIC XX.

       01  FLAGP PIC 9.
       01  SUM-DATE. 
           02 SUM-DATE-1 PIC X(6).
           02 SUM-DATE-2 PIC XX.
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
       01     RIGHT-2 PIC XX JUST RIGHT.
       01     RIGHT-3 PIC XXX JUST RIGHT.
       01     RIGHT-4 PIC X(4) JUST RIGHT.
       01     ALF-1 PIC X.
       01     ALF-2 PIC XX.
       01     ALF-3 PIC XXX.
       01     ALF-4 PIC XXXX.
       01     ALF-5 PIC X(5).
       01     ALF-9 PIC X(9).
       01     ALF-6 PIC X(6).
       01     ALF-7 PIC X(7).
       01     ALF-8 PIC X(8).
       01     ALF-11 PIC X(11).
       01     ALF-13 PIC X(13).
       01     ALF-14 PIC X(14).
       01     ABC PIC XXX.
       01     XYZ PIC 999.
       01     RIGHT-5 PIC X(5) JUST RIGHT.
       01     RIGHT-7 PIC X(7) JUST RIGHT.
       01     NUM-2 PIC 99.
       01     NUM-21 PIC 99.
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
       01     X-INSPEND PIC S9(5)V99.
       01     G-BALCUR PIC S9(5)V99.
       01     G-BAL30  PIC S9(5)V99.
       01     G-BAL60  PIC S9(5)V99.
       01     G-BALCOL  PIC S9(5)V99.
       01  LLLL PIC X(8).
       01  L6F7 PIC ZZZ9.99CR.
       01  CHNL-X PIC X VALUE H"1A".
       01  CHNL-Y PIC X VALUE H"1C".
       01  PAGE-FLAG PIC 9 VALUE 0.
       01  HD101 PIC X(84).
       01  HD201 PIC X(84).
       01  HD301 PIC X(84).
       01  HD401 PIC X(84).
       01  HD501 PIC X(84).
       01  HD601 PIC X(84).
       01  HD701 PIC X(84).
       01  HD801 PIC X(84). 
       01  HD901 PIC X(84). 
       01  HD1001 PIC X(84). 
       01  HD1101 PIC X(84). 
       01  LABEL01 PIC X(84).
       01  FOOT01 PIC X(84).
       01  TB-DATELOW PIC X(8).
       01  TB-DATEHIGH PIC X(8).
       01  TB-INS PIC XXX.
       01  TB-PAY PIC XXX.
       01  TB-ALL PIC X.
       01  IN-FIELD.
           04 IN-FIELD-10.
            05  IN-FIELD-9.
                06 IN-FIELD-8.
                07  IN-FIELD-7.
                08  IN-FIELD-6.
                09  IN-FIELD-5.
                    10  IN-FIELD-4.
                    11  IN-FIELD-3.
                    12  IN-FIELD-2.
                    13  IN-FIELD-1  PIC X.
                    13  FILLER PIC X.
                    12  FILLER  PIC X.
                    11  FILLER    PIC X.
                    10  FILLER      PIC X.
                09  FILLER        PIC X.
                08  FILLER          PIC X.
                07 FILLER  PIC X.
                06 FILLER PIC X.
                05 FILLER PIC X.
                04  FILLER              PIC X(5).
       01 IN-FIELD-TAB01 REDEFINES IN-FIELD.
           02 IN-FIELD-TAB   PIC X OCCURS 15 TIMES.
       01  TAB1101.
           02 TAB11 PIC X OCCURS 11 TIMES.
       01     EIGHTPARTID.
           03 FILLER PIC X(7).
           03 PART-8 PIC X.
       01 TB-SHORT PIC X.
       01 TB-PATPAID PIC X.
       LINKAGE SECTION.
       01  PB1 PIC 9.
       01  TB-GARNO PIC X(8).
       PROCEDURE DIVISION USING PB1 TB-GARNO.
       P0.
            OPEN OUTPUT OUT-FILE INPUT CHARCUR PAYFILE.
            OPEN INPUT GARFILE CHARFILE.
            OPEN INPUT PAYCUR PREPRINTFILE PATFILE.
            OPEN INPUT PROCFILE.
            OPEN INPUT BILLDATE.
            OPEN INPUT INSFILE.
            OPEN INPUT BILLPARM.
            
            MOVE SPACE TO HD101 HD201 HD301 HD401 HD501 HD601 HD701
            HD801 HD901 HD1001 HD1101
            READ PREPRINTFILE AT END GO TO P00.
            MOVE PREPRINTFILE01 TO HD101.
            READ PREPRINTFILE AT END GO TO P00.
            MOVE PREPRINTFILE01 TO HD201.
            READ PREPRINTFILE AT END GO TO P00.
            MOVE PREPRINTFILE01 TO HD301.
            READ PREPRINTFILE AT END GO TO P00.
            MOVE PREPRINTFILE01 TO HD401.
            READ PREPRINTFILE AT END GO TO P00.
            MOVE PREPRINTFILE01 TO HD501.
            READ PREPRINTFILE AT END GO TO P00.
            MOVE PREPRINTFILE01 TO HD601.
            READ PREPRINTFILE AT END GO TO P00.
            MOVE PREPRINTFILE01 TO HD701.
            READ PREPRINTFILE AT END GO TO P00.
            MOVE PREPRINTFILE01 TO HD801.
            READ PREPRINTFILE AT END GO TO P00.
            MOVE PREPRINTFILE01 TO HD901.
            READ PREPRINTFILE AT END GO TO P00.
            MOVE PREPRINTFILE01 TO HD1001.
            READ PREPRINTFILE AT END GO TO P00.
            MOVE PREPRINTFILE01 TO HD1101.
            READ PREPRINTFILE AT END GO TO P00.
            MOVE PREPRINTFILE01 TO LABEL01.
            READ PREPRINTFILE AT END GO TO P00.
            MOVE PREPRINTFILE01 TO FOOT01.
            IF PB1 = 1 GO TO TB-QUICK.
       P00.
             READ BILLPARM AT END DISPLAY "NO BILLPARM" GO TO R20.
             ACCEPT BILL-DATE FROM CENTURY-DATE.
             MOVE BILL-DATE TO TEST-DATE.
             MOVE CORR TEST-DATE TO INPUT-DATE.
             MOVE T-MM OF INPUT-DATE TO WSL1F1M.
             MOVE T-DD OF INPUT-DATE TO WSL1F1D.
             MOVE T-YY OF INPUT-DATE TO WSL1F1Y.
             MOVE WSL1F1 TO LLLL.
             READ BILLDATE AT END DISPLAY "NO BILLDATE" GO TO R20.
             ACCEPT BILL-DATE FROM CENTURY-DATE.
             MOVE BILL-DATE TO SUM-DATE
             MOVE "00" TO SUM-DATE-2.

       TB-QUICK.
             DISPLAY "1 = PATIENT OWED BALANCES    2 = PAID-UP CHARGES"
             DISPLAY "3 = PATIENT PAID CHARGES     4 = ONLY CHARGES"
            DISPLAY "5 = COMPLETE LISTING         6 = ONLY INS. PENDING" 
             DISPLAY "<CR> = YOU SELECT".
             ACCEPT TB-SHORT 
             IF TB-SHORT = SPACE OR "3" GO TO TB-LOW.
             IF TB-SHORT = "E" GO TO R20.
             IF TB-SHORT = "?"
             DISPLAY "SELECT THE TYPE OF BILL TO PRINT FROM THE LIST"
             DISPLAY "OR <CR> TO SELECT A DIFFERENT PRINTING"
             DISPLAY "E = QUIT."
             GO TO TB-QUICK.
             IF TB-SHORT > "0" AND < "7" NEXT SENTENCE 
             ELSE DISPLAY "INVALID" GO TO TB-QUICK.
             IF TB-SHORT = "1"
             MOVE "00000000" TO TB-DATELOW
             MOVE "99999999" TO TB-DATEHIGH
             MOVE SPACE TO TB-ALL
             MOVE SPACE TO TB-INS
             MOVE SPACE TO TB-PAY
             GO TO R1.
             IF TB-SHORT = "2"
             MOVE "00000000" TO TB-DATELOW
             MOVE "99999999" TO TB-DATEHIGH
             MOVE "O" TO TB-ALL
             MOVE "Y" TO TB-INS
             MOVE SPACE TO TB-PAY
             GO TO R1.
             IF TB-SHORT = "3"
             MOVE "00000000" TO TB-DATELOW
             MOVE "99999999" TO TB-DATEHIGH
             MOVE "Y" TO TB-ALL
             MOVE "Y" TO TB-INS
             GO TO TB-LOW.
             IF TB-SHORT = "4"
             MOVE "00000000" TO TB-DATELOW
             MOVE "99999999" TO TB-DATEHIGH
             MOVE SPACE TO TB-ALL
             MOVE "Y" TO TB-INS
             MOVE "Y" TO TB-PAY
             GO TO R1.
             IF TB-SHORT = "5"
             MOVE "00000000" TO TB-DATELOW
             MOVE "99999999" TO TB-DATEHIGH
             MOVE "Y" TO TB-ALL
             MOVE "Y" TO TB-INS
             MOVE SPACE TO TB-PAY
             GO TO R1.
             IF TB-SHORT = "6"
             MOVE "00000000" TO TB-DATELOW
             MOVE "99999999" TO TB-DATEHIGH
             MOVE "Y" TO TB-ALL
             MOVE "O" TO TB-INS
             MOVE SPACE TO TB-PAY
             GO TO R1.

       TB-LOW. 
            DISPLAY "LOW DATE"
            ACCEPT TB-DATELOW
            IF TB-DATELOW = "?"
            DISPLAY "OLDEST CHARGE TO PRINT DD MMDD MMDDYY MMDDYYYY"
            DISPLAY "Q = QUICK BILL: PERSONAL BALANCE DUE ON CHARGES"
            DISPLAY "E = NO PRINTING."
            GO TO TB-LOW.
            IF TB-DATELOW = "E" GO TO R20.
            IF TB-DATELOW = "Q"
            MOVE "00000000" TO TB-DATELOW
            MOVE "99999999" TO TB-DATEHIGH
            MOVE SPACE TO TB-ALL
            MOVE SPACE TO TB-INS
            MOVE SPACE TO TB-PAY
            GO TO R1.
            IF TB-DATELOW = SPACE MOVE "00000000" TO TB-DATELOW.
            MOVE TB-DATELOW TO IN-FIELD-8
            PERFORM SET-DATE1
            MOVE IN-FIELD-8 TO TB-DATELOW.
       TB-HIGH. 
            DISPLAY "HIGH DATE"
            ACCEPT TB-DATEHIGH
            IF TB-DATEHIGH = "?"
            DISPLAY "NEWEST CHARGE TO PRINT  DD MMDD MMDDYY MMDDYYYY"
            DISPLAY "B = BACK."
            DISPLAY "E = QUIT."
            GO TO TB-HIGH. 
            IF TB-DATEHIGH = "B" GO TO TB-LOW.
            IF TB-DATEHIGH = "E" GO TO R20.
            IF TB-DATEHIGH = SPACE MOVE "99999999" TO TB-DATEHIGH.
            MOVE TB-DATEHIGH TO IN-FIELD-8
            PERFORM SET-DATE1
            MOVE IN-FIELD-8 TO TB-DATEHIGH.
            IF TB-SHORT = "5" 
                MOVE "Y" TO TB-ALL
                MOVE "Y" TO TB-INS
                MOVE SPACE TO TB-PAY
            END-IF
            MOVE "N" TO TB-PATPAID
            IF TB-SHORT = "3" OR "5" GO TO R1.
       TB-A. 
            DISPLAY "SHOW ALL BALANCES?"
            ACCEPT TB-ALL.
            IF TB-ALL = "?"
            DISPLAY "Y = YES,  O = ONLY PAID-UP CHARGES, <CR> = NO "
            " 0 BALANCE CHARGES"
            DISPLAY "B = BACK."
            DISPLAY "E = QUIT."
            GO TO TB-A. 
            IF TB-ALL = "B" GO TO TB-HIGH.
            IF TB-ALL = "E" GO TO R20.
            IF TB-ALL NOT = "Y" AND NOT = "O" MOVE SPACE TO TB-ALL.

       TB-I. 
            DISPLAY "SHOW INSURANCE PENDING CHARGES?"
            ACCEPT TB-INS.
            IF TB-INS = "?"
            DISPLAY "Y = YES,  O = ONLY INSURANCE PENDING, <CR> = NO"
            DISPLAY "ONLY ONE INSURANCE CODE. (TYPE THE CODE  XXX)"
            DISPLAY "B = BACK."
            DISPLAY "E = QUIT."
            GO TO TB-I. 
            IF TB-INS = "B" GO TO TB-A.
            IF TB-INS = "E" GO TO R20.
            IF (TB-INS NUMERIC) AND (TB-INS NOT = "000")  GO TO TB-P.
            IF TB-INS NOT = "Y" AND NOT = "O" MOVE SPACE TO TB-INS.
       TB-P. 
            DISPLAY "EXCLUDE ALL PAYMENTS ON CHARGES?"
            ACCEPT TB-PAY.
            IF TB-PAY = "?"
            DISPLAY "<CR> = NO"
            DISPLAY "Y = EXCLUDE ALL PAYMENTS SHOWN ON STATEMENT"
           DISPLAY "SHOW CHARGES WITH THIS PAYMENT CODE. TYPE THE CODE."
            DISPLAY "B = BACK."
            DISPLAY "E = QUIT."
            GO TO TB-P. 
            IF TB-PAY = "B" GO TO TB-I.
            IF TB-PAY = "E" GO TO R20.
            IF (TB-PAY NUMERIC) AND (TB-PAY NOT = "000")  GO TO R1.   
            IF TB-PAY NOT = "Y" AND NOT = "P" MOVE SPACE TO TB-PAY.
       
       R1. 
      *     DISPLAY "PC = (turn on cp-to-printer) TERMINAL = <CR>"
      *        " X = NO PRINTING"
      *     ACCEPT ALF-1 
      *    IF ALF-1 = "X" GO TO R20.
           MOVE TB-GARNO TO G-GARNO
           READ GARFILE 
               INVALID
                   DISPLAY G-GARNO " INVALID ACCT" 
                   GO TO R20.

      ******* READ IN ALL   RECORDS TO TABLES FOR THIS GUARANTOR *******
           MOVE SPACES TO EIGHTPARTID.
           MOVE 0 TO X-INSPEND.
           MOVE 0 TO XYZ
           MOVE 0 TO CHR-IND PHR-IND
             LINE-CTR G-BALCUR G-BAL30 G-BAL60 G-BALCOL 
           MOVE 1 TO BILL-PAGE.
           MOVE G-GARNO TO CC-KEY8
           MOVE XYZ TO CC-KEY3
           START CHARCUR KEY > CHARCUR-KEY 
               INVALID 
                   GO TO R7.

       R6. 
           READ CHARCUR NEXT 
               AT END 
                   GO TO R7.

           IF G-GARNO NOT = CC-KEY8 GO TO R7.
    
           IF CC-DATE-T < TB-DATELOW OR > TB-DATEHIGH
               GO TO R6.
           IF (TB-INS = "O" AND CC-ASSIGN = "U")
               OR (TB-INS = SPACE AND CC-ASSIGN = "A")
               OR ((TB-INS NUMERIC) AND (CC-PAYCODE NOT = TB-INS))
               GO TO R6.

           ADD 1 TO CHR-IND.
           IF CHR-IND = 991 DISPLAY G-GARNO " " G-GARNAME
               GO TO R20.
           IF CC-DATE-A = "00000000" MOVE BILL-DATE TO
               CHR-DATE-A(CHR-IND)
           ELSE 
               MOVE CC-DATE-A TO CHR-DATE-A(CHR-IND).
           
           MOVE CC-DATE-T TO CHR-DATE-T(CHR-IND)
           MOVE CC-CLAIM TO CHR-CLAIM(CHR-IND)
           MOVE CC-AMOUNT TO CHR-AMOUNT(CHR-IND)
           MOVE CC-PAYCODE TO CHR-PAYCODE(CHR-IND)
           MOVE CC-PATID TO CHR-PATID(CHR-IND)
           MOVE CC-ASSIGN TO CHR-ASSIGN(CHR-IND)
           MOVE CC-PROC TO CHR-PROC(CHR-IND)
           MOVE CC-DIAG TO CHR-DIAG(CHR-IND)
           MOVE CC-REC-STAT TO CHR-REC-STAT(CHR-IND)
           GO TO R6.

       R7. 
           IF (TB-PAY NOT = SPACE) AND (TB-PAY NOT = "P" )
               AND (TB-PAY NOT NUMERIC) AND (TB-SHORT NOT = "3")
               GO TO R9.
    
           MOVE 0 TO XYZ.
           MOVE G-GARNO TO PC-KEY8
           MOVE XYZ TO PC-KEY3
           START PAYCUR KEY > PAYCUR-KEY 
               INVALID
                   GO TO R9.

       R8. 
           READ PAYCUR NEXT 
               AT END
               GO TO R9.

           IF G-GARNO NOT = PC-KEY8 GO TO R9.
       
           ADD 1 TO PHR-IND.
           IF PHR-IND > 990 
               DISPLAY G-GARNO " "  G-GARNAME
               GO TO R20.

           MOVE PC-DATE-T TO PHR-DATE(PHR-IND)
           MOVE PC-CLAIM TO PHR-CLAIM(PHR-IND)
           MOVE PC-AMOUNT TO PHR-AMOUNT(PHR-IND)
           MOVE PC-PAYCODE TO PHR-PAYCODE(PHR-IND)
           MOVE PC-DENIAL TO PHR-DENIAL(PHR-IND).
           GO TO R8.

       R9. 
           MOVE G-GARNO TO CD-KEY8
           MOVE XYZ TO CD-KEY3
           START CHARFILE KEY > CHARFILE-KEY
               INVALID
                   GO TO R10.

       S6. 
           READ CHARFILE NEXT 
               AT END
                   GO TO R10.

            IF G-GARNO NOT = CD-KEY8 GO TO R10.
            IF CD-DATE-T  < TB-DATELOW OR > TB-DATEHIGH
            GO TO S6.
            IF (TB-INS = "O"  AND CD-ASSIGN = "U") 
            OR (TB-INS = SPACE AND CD-ASSIGN = "A") 
            OR ((TB-INS NUMERIC) AND (CD-PAYCODE NOT = TB-INS)) 
            GO TO S6.
            ADD 1 TO CHR-IND.
            IF CHR-IND > 500 DISPLAY G-GARNO " " G-GARNAME " CHRS"
            GO TO R20.
            IF CD-DATE-A = "00000000" MOVE BILL-DATE TO
            CHR-DATE-A(CHR-IND)
            ELSE MOVE CD-DATE-A TO CHR-DATE-A(CHR-IND).
            MOVE CD-DATE-T TO CHR-DATE-T(CHR-IND)
            MOVE CD-CLAIM TO CHR-CLAIM(CHR-IND)
            MOVE CD-AMOUNT TO CHR-AMOUNT(CHR-IND)
            MOVE CD-PAYCODE TO CHR-PAYCODE(CHR-IND)
            MOVE CD-PROC TO CHR-PROC(CHR-IND)
            MOVE CD-DIAG TO CHR-DIAG(CHR-IND)
            MOVE CD-PATID TO CHR-PATID(CHR-IND)
            MOVE CD-ASSIGN TO CHR-ASSIGN(CHR-IND)
            MOVE CD-STAT TO CHR-REC-STAT(CHR-IND)
            GO TO S6.

       R10. 
            IF (TB-PAY NOT = SPACE) AND (TB-PAY NOT = "P" )
            AND (TB-PAY NOT NUMERIC) GO TO R11.
            MOVE 0 TO XYZ.
            MOVE G-GARNO TO PD-KEY8
            MOVE XYZ TO PD-KEY3
            START PAYFILE KEY > PAYFILE-KEY INVALID GO TO R11.

       S8. 
           READ PAYFILE NEXT AT END GO TO R11.
            IF G-GARNO NOT = PD-KEY8 GO TO R11.
            ADD 1 TO PHR-IND.
            IF PHR-IND > 990 DISPLAY G-GARNO " "  G-GARNAME " PAY"
            GO TO R20.
            MOVE PD-DATE-T TO PHR-DATE(PHR-IND)
            MOVE PD-CLAIM TO PHR-CLAIM(PHR-IND)
            MOVE PD-AMOUNT TO PHR-AMOUNT(PHR-IND)
            MOVE PD-PAYCODE TO PHR-PAYCODE(PHR-IND)
            MOVE PD-DENIAL TO PHR-DENIAL(PHR-IND).
            GO TO S8.

******* START HERE TO DETERMINE AMOUNT DUE AND INSURANCE PENDING *******
******* AND AGING ON CURRENT,PAST DUE AND DELINQUENT *******
       R11.
            PERFORM CC1 THRU CC-EXIT VARYING X FROM 1 BY 1 UNTIL
            X > CHR-IND.


******* SORT ALL TABLES BY DATE *******

       R11-1. 
           IF CHR-IND > 1
            SUBTRACT 1 FROM CHR-IND GIVING Y
            PERFORM S10 VARYING X FROM 1 BY 1 UNTIL X > Y.
            IF PHR-IND > 1
            SUBTRACT 1 FROM PHR-IND GIVING Y
            PERFORM S12 VARYING X FROM 1 BY 1 UNTIL X > Y.

       FF. 
           GO TO R13.

******* START WRITING OUTPUT *******
       R13. 
           ADD G-BALCUR G-BAL30 G-BAL60 G-BALCOL GIVING AMOUNT-DUE.

            MOVE G-PHONE TO L0F3.
            MOVE SPACES TO LINE-4
            LINE-7 LINE-8 LINE-6A.
            MOVE SPACE TO L1F4.
            MOVE G-DUNNING TO L1F5
            MOVE G-COLLT TO L1F51
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
       R14.
             MOVE G-GARNO TO L1F3.
             MOVE BILL-DATE TO TEST-DATE.
             MOVE CORR TEST-DATE TO INPUT-DATE.
             MOVE T-MM OF INPUT-DATE TO WSL1F1M.
             MOVE T-DD OF INPUT-DATE TO WSL1F1D.
             MOVE T-YY OF INPUT-DATE TO WSL1F1Y.
             MOVE WSL1F1 TO L2F1 MOVE LLLL TO L1F1.
             MOVE SPACES TO WSL1F1.
             MOVE G-BILLADD TO L1F2.
             IF G-DOB = "00000000" MOVE "XX" TO L1F7
             ELSE MOVE BILL-DATE TO DAY-TEST-1
             MOVE G-DOB TO DAY-TEST-2
             COMPUTE NUM-2 = DY1 - DY2
             MOVE NUM-2 TO L1F7.
             MOVE G-BILLADD TO L1F2.
             MOVE SPACES TO F21 F22 F23 F24.
             MOVE G-STREET TO L2F2.
             IF (L1F2 NOT = SPACE) AND (L2F2 = SPACE)
             MOVE L1F2 TO L2F2
             MOVE SPACE TO L1F2.
             IF L1F2 = SPACE AND NUM-2 < 18
             MOVE "C/O PARENT            " TO L1F2.
             MOVE G-LASTBILL TO L2F3.
             MOVE G-PRINS TO L2F4.
             MOVE G-PRIPOL TO L2F5.
           
           IF L2F2 = SPACE 
           MOVE L1F2 TO L2F2
           MOVE SPACE TO L1F2.

           PERFORM HEADER-1

             WRITE OUT01 FROM LINE-0.
             WRITE OUT01 FROM LINE-1
             WRITE OUT01 FROM LINE-2.
             MOVE SPACES TO TAB2001.
             MOVE G-CITY TO TAB2001.
             MOVE G-STATE TO AT-3-1.
             MOVE "@" TO AT-3-2.
             MOVE SPACES TO L3F1.
             IF G-ZIP = ZEROES MOVE SPACE TO ALF-9
             ELSE MOVE G-ZIP TO ALF-9.
             IF TAB20(18) NOT = " " MOVE "," TO TAB20(19)
             MOVE "@" TO TAB20(20)
             STRING TAB2001 " " AT-3 " " ALF-9 DELIMITED BY "@" INTO
             L3F1 GO TO R14-1.
             PERFORM Q1 VARYING C FROM 17 BY -1 UNTIL C < 1.
             STRING TAB2001 " " AT-3 " " ALF-9 DELIMITED BY "@" INTO
             L3F1.
      
       R14-1.
             INSPECT L3F1 REPLACING ALL "," BY " ".
             MOVE G-SEINS TO L3F2.
             MOVE G-SECPOL TO L3F5.
             WRITE OUT01 FROM LINE-3.
             WRITE OUT01 FROM LINE-PHONE
           WRITE OUT01 FROM LABEL01 AFTER 2.

             IF G-PRINS = "001" WRITE OUT01 FROM NOINS ADD 2 TO LINE-CTR
             WRITE OUT01 FROM LINE-4 GO TO R15.
             IF G-PRINS = "091" WRITE OUT01 FROM N9398 ADD 2 TO LINE-CTR
             WRITE OUT01 FROM LINE-4.

       R15. 
           PERFORM Q2-0 THRU Q10 VARYING A FROM 1 BY 1 
             UNTIL A > CHR-IND.
             
           IF (LINE-CTR < 62)
               AND (G-BAL60 > 0) 
             AND (G-DUNNING = "1") 
             MOVE SPACE TO OUT01
             MOVE "PLEASE CONTACT US REGARDING YOUR ACCOUNT." TO OUT01 
             WRITE OUT01
               ADD 1 TO LINE-CTR.

             IF (LINE-CTR < 62) AND (G-DUNNING = "2") 
             MOVE SPACE TO OUT01
               MOVE "WE WILL ACCEPT INSTALLMENT PAYMENTS." TO OUT01 
             WRITE OUT01
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
             MOVE SPACE TO OUT01
             WRITE OUT01 AFTER 2
             WRITE OUT01 FROM FOOT01.
             WRITE OUT01 FROM LINE-7 
             MOVE SPACES TO L8F1.
             
           IF TEST-BUSNAME-1 = "1" 
             MOVE TEST-BUSNAME-2 TO L8F1
             ELSE 
             MOVE G-GARNAME TO L8F1.

             MOVE G-GARNO TO L8F2
             MOVE BILL-PAGE TO L8F3
             INSPECT L8F3 REPLACING LEADING "0" BY " ".
             MOVE SPACES TO F81 F82 F83.
      *     IF G-BALCOL > 0 MOVE "COLLECTION REVIEW ACCOUNT"
      *     TO L8F4 GO TO R15-1.
      *     IF G-BAL60 > 0 MOVE "YOUR ACCOUNT IS DELINQUENT."
      *     TO L8F4 GO TO R15-1.
      *     IF G-BAL30 > 0 MOVE "YOUR ACCOUNT IS PAST DUE."
      *     TO L8F4 GO TO R15-1.
           MOVE SPACES TO L8F4.

       R15-1.
           WRITE OUT01 FROM LINE-8  AFTER 2.
           GO TO R20.
******* FIND LAST CHARACTER IN CITY NAME *******
       Q1. 
           IF TAB20(C) NOT = " "
               ADD 1 C GIVING B
               MOVE "," TO TAB20(B)
               ADD 1 TO B
               MOVE "@" TO TAB20(B)
               MOVE 1 TO C.
       
       Q2-0. 
            IF (CHR-TOT(A) NOT = 0) AND (TB-ALL = "O") 
            OR (CHR-TOT(A) = 0) AND (TB-ALL = SPACE) GO TO Q10.
            IF (TB-SHORT = "3") MOVE 0 TO FLAG
            PERFORM PAY-2 VARYING Y FROM 1 BY 1 UNTIL Y > PHR-IND
            IF FLAG = 1 GO TO Q2
                END-IF
            IF FLAG = 0 GO TO Q10
            END-IF
            END-IF
            IF (TB-PAY NUMERIC) MOVE 0 TO FLAG
            PERFORM PAY-1 VARYING Y FROM 1 BY 1 UNTIL Y > PHR-IND
            IF FLAG = 0 GO TO Q10.

******* WRITE THE DETAIL LINES ON BILL FOR CHARGES *******
       Q2. 
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
           MOVE PROC-KEY TO L6-PROC.
           MOVE CHR-DIAG(A) TO L6F4.
           IF CHR-DIAG(A) = "0000000" MOVE "       " TO L6F4.
           MOVE CHR-AMOUNT(A) TO L6F6.
           MOVE CHR-TOT(A) TO L6F7
           IF CHR-TOT(A) = 0 
           MOVE SPACE TO L6F7X ELSE MOVE L6F7 TO L6F7X.
           IF CHR-PATID(A) = G-GARNO MOVE G-GARNAME TO NAME-TEST
           GO TO Q4-1.
           MOVE CHR-PATID(A) TO P-PATNO.
           READ PATFILE INVALID MOVE SPACES TO P-PATNAME.
           MOVE P-PATNAME TO NAME-TEST.
       Q4-1.
           MOVE SPACES TO TABA2401 TABB2401.
           UNSTRING NAME-TEST DELIMITED BY ";" INTO TABB2401 TABA2401.
           IF TEST-BUSNAME-1 NOT = "1"
           MOVE G-GARNAME TO NAME-LAST
           MOVE TABA2401 TO TAB1101 GO TO Q5-1.
           MOVE CHR-PATID(A) TO EIGHTPARTID.
           IF PART-8 = "G" MOVE TEST-BUSNAME-2 TO TAB1101 GO TO Q5-1.
            PERFORM T20 VARYING C FROM 24 BY -1 UNTIL C < 1.
           PERFORM T21 VARYING C FROM 24 BY -1 UNTIL C < 1.
           IF Z > 8 MOVE 2 TO X MOVE " " TO TABA24(X)
           MOVE 9 TO Y GO TO Q5.
           MOVE Z TO Y.
           SUBTRACT Z FROM 11 GIVING D.
           IF X < D ADD 1 TO X
           MOVE " " TO TABA24(X)
           ELSE MOVE " " TO TABA24(D)
           MOVE D TO X.

       Q5. 
           MOVE SPACES TO TAB1101.
           MOVE 0 TO D.
           PERFORM T22 VARYING Z FROM 1 BY 1 UNTIL Z > X.
           PERFORM T23 VARYING Z FROM 1 BY 1 UNTIL Z > Y.

       Q5-1.
           MOVE SPACES TO L6F3.
           MOVE TAB1101 TO L6F3.
           MOVE CHR-AMOUNT(A) TO L6F6.

******* SEARCH PAYMENT TABLE FOR PAYMENTS AGAINST THIS CLAIM *******
       Q51.
           MOVE 0 TO FLAG.
           PERFORM T24 THRU T30 VARYING X FROM 1 BY 1 UNTIL X > PHR-IND.
           IF FLAG = 1 GO TO Q10.
           IF LINE-CTR > 41
             WRITE CHNL-3 FROM CHNL-Y
             MOVE G-GARNO TO L4F1 MOVE SPACE TO F41
             MOVE L0F2 TO L4F2 MOVE "PAGE " TO L4F3
             MOVE SPACE TO F42
             MOVE BILL-PAGE TO L4F4
             INSPECT L4F4 REPLACING LEADING "0" BY " "
             MOVE SPACE TO F43 MOVE "CONTINUED" TO L4F5
             WRITE OUT01 FROM LINE-4 AFTER 3
      *     PERFORM HEADER-1
           WRITE OUT01 FROM LINE-0 
           WRITE OUT01 FROM LINE-1
           WRITE OUT01 FROM LINE-2
           WRITE OUT01 FROM LINE-3
           WRITE OUT01 FROM LINE-PHONE
      *     WRITE OUT01 FROM LABEL01 AFTER 2
           MOVE 0 TO LINE-CTR
           ADD 1 TO BILL-PAGE.
           IF CHR-ASSIGN(A) = "A"
           MOVE LINE-6 TO LINE-6P
           MOVE "PENDING INS" TO L6PF1
           WRITE OUT01 FROM LINE-6P GO TO Q90.
           MOVE CHR-TOT(A) TO L6F7
           
           IF CHR-TOT(A) = 0 
             MOVE SPACE TO L6F7X 
           ELSE 
             MOVE L6F7 TO L6F7X.

           WRITE OUT01 FROM LINE-6.
           ADD 1 TO LINE-CTR.
       Q90.
           PERFORM T24 THRU T30 VARYING B FROM 1 BY 1 UNTIL B >
           PHR-IND.
       Q10. EXIT.
       T5. IF TABA24(C) NOT = " "
           ADD 1 TO C
           MOVE "@" TO TABA24(C)
           MOVE 1 TO C.
       T20. IF TABA24(C) NOT = " " MOVE C TO X
           MOVE 1 TO C.
       T21. IF TABB24(C) NOT = " " MOVE C TO Z
           MOVE 1 TO C.
       T22. ADD 1 TO D MOVE TABA24(Z) TO TAB11(D).
       T23. ADD 1 TO D MOVE TABB24(Z) TO TAB11(D).
       T24. IF PHR-CLAIM(X) NOT = CHR-CLAIM(A) GO TO T30.
           IF FLAG = 1 GO TO T25.
           IF LINE-CTR > 41
           WRITE CHNL-3 FROM CHNL-Y
           MOVE G-GARNO TO L4F1 MOVE SPACE TO F41
           MOVE L0F2 TO L4F2 MOVE "PAGE " TO L4F3
           MOVE SPACE TO F42
           MOVE BILL-PAGE TO L4F4
           INSPECT L4F4 REPLACING LEADING "0" BY " "
           MOVE SPACE TO F43 MOVE "CONTINUED" TO L4F5
           WRITE OUT01 FROM LINE-4 AFTER 3
      *     PERFORM HEADER-1
           WRITE OUT01 FROM LINE-0 
           WRITE OUT01 FROM LINE-1
           WRITE OUT01 FROM LINE-2
           WRITE OUT01 FROM LINE-3
           WRITE OUT01 FROM LINE-PHONE
           WRITE CHNL-2 FROM CHNL-X
           MOVE 0 TO LINE-CTR
           ADD 1 TO BILL-PAGE.
           MOVE 1 TO FLAG.
           IF CHR-ASSIGN(A) = "U"
           WRITE OUT01 FROM LINE-6
           ELSE MOVE LINE-6 TO LINE-6P
           MOVE "PENDING INS" TO L6PF1
           WRITE OUT01 FROM LINE-6P.
           ADD 1 TO LINE-CTR.
       T25. MOVE PHR-DATE(X) TO TEST-DATE.
           MOVE CORR TEST-DATE TO INPUT-DATE.
           INSPECT T-MM OF INPUT-DATE REPLACING LEADING "0" BY " ".
           MOVE T-MM OF INPUT-DATE TO WSL1F1M.
           MOVE T-DD OF INPUT-DATE TO WSL1F1D.
           MOVE T-YY OF INPUT-DATE TO WSL1F1Y.
           MOVE WSL1F1 TO L5F1.
           MOVE "PAYMT " TO L5F21.
           MOVE PHR-PAYCODE(X) TO INS-KEY
           READ INSFILE INVALID MOVE "MISC.    " TO INS-NAME.
           MOVE INS-NAME TO L5F22.
           IF L5F22 = SPACE MOVE "MISC. INS." TO L5F22.
           MOVE SPACE TO L5F23.
           IF PHR-DENIAL(X) = "DD" MOVE "DEDCT NOT MET"
           TO L5F23.
           IF PHR-DENIAL(X) = "14" MOVE "INS. REDUCT. "
           TO L5F23.
           IF PHR-DENIAL(X) = "NC" MOVE "NONCOVER SERV"
           TO L5F23.
           IF PHR-DENIAL(X) = "NR" MOVE "NO INS. REPLY"
           TO L5F23.
           IF PHR-DENIAL(X) = "CE" MOVE "INVLD CERT NO"
           TO L5F23.
           IF PHR-DENIAL(X) = "DA" MOVE "NONCOVER DATE"
           TO L5F23.
           IF PHR-DENIAL(X) = "BE" MOVE "BILLING ERROR" TO
             L5F23.
           IF PHR-DENIAL(X) = "PP" MOVE "PATIENT PAID " TO
             L5F23.
           
           IF PHR-DENIAL(X) = "CB" MOVE "COLL. BUREAU " 
           TO L5F23.
           
           IF PHR-DENIAL(X) = "RE" MOVE "REDUCE ERROR"
            TO L5F23.
            IF PHR-DENIAL(X) = "IN" MOVE "INSURE ERROR"
            TO L5F23.
            IF PHR-DENIAL(X) = "CP" MOVE "COPAY AMOUNT"
            TO L5F23.
            IF PHR-DENIAL(X) = "PI" MOVE "PRIMARY PAID"
            TO L5F23.
            IF PHR-DENIAL(X) = "WC" MOVE "WORK COMP   "
            TO L5F23.
            IF PHR-DENIAL(X) = "TO" MOVE "TOO OLD INS."
            TO L5F23.
            IF PHR-DENIAL(X) = "DI" MOVE "DEFERRED ADJ"
            TO L5F23.
            IF PHR-DENIAL(X) = "ON" MOVE "NON-NETWORK "
            TO L5F23.
            IF PHR-DENIAL(X) = "RF" MOVE "NO REFERRAL " 
            TO L5F23.
            IF PHR-DENIAL(X) = "OI" MOVE "OTHER INS.  "
            TO L5F23.
            IF INS-KEY > "009" AND < "020"
            MOVE SPACE TO L5F21.

       T26. 
           MOVE PHR-AMOUNT(X) TO L5F4.
           MOVE SPACE TO L5F3.
      *    MOVE PHR-CLAIM(X) TO L5F3.
           IF LINE-CTR > 41
           WRITE CHNL-3 FROM CHNL-Y
           MOVE G-GARNO TO L4F1 MOVE SPACE TO F41
           MOVE L0F2 TO L4F2 MOVE "PAGE " TO L4F3
           MOVE SPACE TO F42
           MOVE BILL-PAGE TO L4F4
           INSPECT L4F4 REPLACING LEADING "0" BY " "
           MOVE SPACE TO F43 MOVE "CONTINUED" TO L4F5
           WRITE OUT01 FROM LINE-4 AFTER 3
      *     PERFORM HEADER-1
           WRITE OUT01 FROM LINE-0 
           WRITE OUT01 FROM LINE-1
           WRITE OUT01 FROM LINE-2
           WRITE OUT01 FROM LINE-3
           WRITE OUT01 FROM LINE-PHONE
      *     WRITE OUT01 FROM LABEL01 AFTER 2
           MOVE 0 TO LINE-CTR
           ADD 1 TO BILL-PAGE.
           WRITE OUT01 FROM LINE-5.
           ADD 1 TO LINE-CTR.
           GO TO T30.
       T30. EXIT.
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
           MOVE CHR-AMOUNT(X) TO SNUM-6
           MOVE CHR-AMOUNT(A) TO CHR-AMOUNT(X)
           MOVE SNUM-6 TO CHR-AMOUNT(A)
           MOVE CHR-PAYCODE(X) TO ALF-3
           MOVE CHR-PAYCODE(A) TO CHR-PAYCODE(X)
           MOVE ALF-3 TO CHR-PAYCODE(A)
           MOVE CHR-PROC(X) TO ALF-7
           MOVE CHR-PROC(A) TO CHR-PROC(X)
           MOVE ALF-7 TO CHR-PROC(A)
           MOVE CHR-ASSIGN(X) TO ALF-1
           MOVE CHR-ASSIGN(A) TO CHR-ASSIGN(X)
           MOVE CHR-REC-STAT(X) TO ALF-1
           MOVE CHR-REC-STAT(A) TO CHR-REC-STAT(X)
           MOVE ALF-1 TO CHR-REC-STAT(A)
           MOVE CHR-TOT(X) TO CLAIM-TOT
           MOVE CHR-TOT(A) TO CHR-TOT(X)
           MOVE CLAIM-TOT TO CHR-TOT(A)
           MOVE CHR-DIAG(X) TO ALF-7
           MOVE CHR-DIAG(A) TO CHR-DIAG(X)
           MOVE ALF-7 TO CHR-DIAG(A).
       S12. ADD 1 X GIVING Z.
           PERFORM S13 VARYING A FROM Z BY 1 UNTIL A > PHR-IND.
       S13. IF PHR-DATE(X) > PHR-DATE(A)
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
           MOVE PHR-DENIAL(X) TO ALF-1
           MOVE PHR-DENIAL(A) TO PHR-DENIAL(X)
           MOVE ALF-1 TO PHR-DENIAL(A).
           IF PHR-DATE(X) = PHR-DATE(A) AND PHR-PAYCODE(X) >
           PHR-PAYCODE(A)
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
           MOVE PHR-DENIAL(X) TO ALF-1
           MOVE PHR-DENIAL(A) TO PHR-DENIAL(X)
           MOVE ALF-1 TO PHR-DENIAL(A).
       CC1.
           MOVE CHR-AMOUNT(X) TO CLAIM-TOT.
           PERFORM PH2 VARYING Y FROM 1 BY 1 UNTIL Y > PHR-IND.
           MOVE  CLAIM-TOT TO CHR-TOT(X).
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
      *    DISPLAY DAYS " " PF3 " " CLAIM-TOT " " G-BALCOL
           IF DAYS > PF3 ADD CLAIM-TOT TO G-BALCOL GO TO CC-EXIT.
           IF DAYS > PF2 ADD CLAIM-TOT TO G-BAL60 GO TO CC-EXIT.
           IF DAYS > PF1 ADD CLAIM-TOT TO G-BAL30 GO TO CC-EXIT.
           ADD CLAIM-TOT TO G-BALCUR.
       CC-EXIT. EXIT.
       PH2. IF CHR-CLAIM(X) = PHR-CLAIM(Y)
           ADD PHR-AMOUNT(Y) CLAIM-TOT GIVING CLAIM-TOT.
       PAY-1. IF CHR-CLAIM(A) = PHR-CLAIM(Y)
           AND PHR-PAYCODE(Y) = TB-PAY MOVE 1 TO FLAG.
       PAY-2. IF CHR-CLAIM(A) = PHR-CLAIM(Y)
           IF PHR-PAYCODE(Y) = "001" OR "021" OR "022"
           MOVE 1 TO FLAG.
       
       SET-DATE.
           ACCEPT T-DATE FROM CENTURY-DATE
           IF TB-DATELOW = SPACE MOVE  "00000000" TO TB-DATELOW.
           IF TB-DATEHIGH = SPACE MOVE "99999999" TO TB-DATEHIGH.
           MOVE TB-DATELOW TO IN-FIELD-8
           PERFORM SET-DATE1
           MOVE IN-FIELD-8 TO TB-DATELOW
           MOVE TB-DATEHIGH TO IN-FIELD-8
           PERFORM SET-DATE1
           MOVE IN-FIELD-8 TO TB-DATEHIGH.
       SET-DATE1.
           ACCEPT T-DATE FROM CENTURY-DATE 
           IF IN-FIELD-2 NUMERIC AND
           IN-FIELD-TAB(3) = " " AND IN-FIELD-TAB(4) = " "
           AND IN-FIELD-TAB(5) = " " AND IN-FIELD-TAB(6) = " "
           AND IN-FIELD-TAB(7) = " " AND IN-FIELD-TAB(8) = " "
           MOVE T-DATE TO TEST-DATE
           MOVE CORR TEST-DATE TO INPUT-DATE
           MOVE IN-FIELD-2 TO T-DD OF INPUT-DATE
           MOVE INPUT-DATE TO IN-FIELD-8.
           IF IN-FIELD-4 NUMERIC AND
           IN-FIELD-TAB(5) = " " AND IN-FIELD-TAB(6) = " "
           AND IN-FIELD-TAB(7) = " " AND IN-FIELD-TAB(8) = " "
           STRING IN-FIELD-4 YEAR4 DELIMITED BY "!!" INTO IN-FIELD-8.
           IF IN-FIELD-6 NUMERIC 
           AND IN-FIELD-TAB(7) = " " AND IN-FIELD-TAB(8) = " "           
           AND IN-FIELD-TAB(5) = "9" AND IN-FIELD-TAB(6) = "9"
           STRING IN-FIELD-4 "1999" DELIMITED BY "!!" INTO IN-FIELD-8.
       
           IF IN-FIELD-6 NUMERIC 
           AND IN-FIELD-TAB(7) = " " AND IN-FIELD-TAB(8) = " "           
           MOVE T-DATE TO TEST-DATE
           MOVE IN-FIELD-8 TO INPUT-DATE
           MOVE T-CC OF INPUT-DATE TO T-YY OF INPUT-DATE
           MOVE T-CC OF TEST-DATE TO T-CC OF INPUT-DATE
           MOVE INPUT-DATE TO IN-FIELD-8.
           IF IN-FIELD-8 NUMERIC MOVE IN-FIELD-8 TO INPUT-DATE
           MOVE CORR INPUT-DATE TO TEST-DATE
           MOVE TEST-DATE TO IN-FIELD-8
           ELSE MOVE SPACE TO IN-FIELD-8.
       HEADER-1.
           IF PAGE-FLAG = 0
           MOVE 1 TO PAGE-FLAG 
           WRITE OUT01 FROM HD101
           ELSE
           WRITE OUT01 FROM HD101 AFTER PAGE.
           WRITE OUT01 FROM HD201
           WRITE OUT01 FROM HD301
           WRITE OUT01 FROM HD401
           WRITE OUT01 FROM HD501
           WRITE OUT01 FROM HD601
           WRITE OUT01 FROM HD701
           WRITE OUT01 FROM HD801
           WRITE OUT01 FROM HD901
           WRITE OUT01 FROM HD1001
           WRITE OUT01 FROM HD1101.
       R20.
           CLOSE OUT-FILE CHARCUR PAYFILE GARFILE CHARFILE PAYCUR 
           PREPRINTFILE PATFILE PROCFILE BILLDATE INSFILE BILLPARM.
           CALL "SYSTEM" USING "PRINT-BILL $HOME/W1$tid".
           EXIT PROGRAM.
