      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. zzzz009.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMNTFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS CMNT-KEY
           LOCK MODE MANUAL.
           SELECT BILLPARM ASSIGN TO "S35"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT INSFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC   RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT OUT-FILE ASSIGN TO "S45"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT PAYCUR ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.
           SELECT CHARCUR ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC   RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT GARFILE ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC   RECORD KEY IS G-GARNO
           LOCK MODE MANUAL.
           SELECT PROCFILE ASSIGN TO "S65" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC   RECORD KEY IS PROC-KEY
           LOCK MODE MANUAL.
           SELECT CHARFILE ASSIGN TO "S70" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC   RECORD KEY IS CHARFILE-KEY
           LOCK MODE MANUAL.
           SELECT PAYFILE ASSIGN TO "S75" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC   RECORD KEY IS PAYFILE-KEY
           LOCK MODE MANUAL.
           SELECT REFPHY ASSIGN TO "S80" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC   RECORD KEY IS REF-KEY
           ALTERNATE RECORD KEY IS REF-BSNUM  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CRNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-UPIN  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CDNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-NAME  WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT DOCFILE ASSIGN TO "S85"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILEIN ASSIGN TO "S90"
           ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  GARFILE
      *    BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS G-MASTER.
       01  G-MASTER.
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
           02 G-PR-GROUP PIC X(10).
           02 G-PRIPOL PIC X(16).
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-SE-MPLR PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
           02 G-SE-GROUP PIC X(10).
           02 G-SECPOL PIC X(16).
           02 G-SENAME PIC X(24).
           02 G-SE-RELATE PIC X.
           02 G-COPAY PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.

       FD  CHARFILE
      *    BLOCK CONTAINS 2 RECORDS
           DATA RECORD IS CHARFILE01.
       01  CHARFILE01.
           02 CHARFILE-KEY.
             03 CD-KEY8 PIC X(8).
             03 CD-KEY3 PIC XXX.
           02 CD-PATID PIC X(8).
           02 CD-CLAIM PIC X(6).
           02 CD-SERVICE PIC X.
           02 CD-DIAG PIC X(7).
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
           02 CD-DX2 PIC X(7).
           02 CD-DX3 PIC X(7).
           02 CD-DATE-A PIC X(8).
           02 CD-ACC-TYPE PIC X.
           02 CD-DATE-M PIC X(8).
           02 CD-ASSIGN PIC X.
           02 CD-NEIC-ASSIGN PIC X.
           02 CD-DX4 PIC X(7).
           02 CD-DX5 PIC X(7).
           02 CD-DX6 PIC X(7).
           02 CD-FUTURE PIC X(6).

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
           02 INS-KEY PIC 999.
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

       FD  FILEIN.
       01  FILEIN01.
           02 FI-1 PIC X(8).
           02 FI-2 PIC XXX.
       FD  CMNTFILE
           BLOCK CONTAINS 2 RECORDS
           DATA RECORD IS CMNTFILE01.
       01  CMNTFILE01.
           02 CMNT-KEY.
             03 CMNT8 PIC X(8).
             03 CMNT3 PIC XXX.
           02 CMNT-TITLE PIC X(80).
           02 CMNT-DATE-E PIC X(8).
       FD  DOCFILE.
       01  DOCFILE01.
           02 DF1 PIC X.
           02 DF2 PIC X.
           02 DF3 PIC X(22).
       FD  REFPHY
           BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS REFPHY01.
       01  REFPHY01.
           02 REF-KEY PIC XXX.
           02 REF-BSNUM PIC X(5).
           02 REF-CRNUM PIC X(6).
           02 REF-UPIN PIC X(6).
           02 REF-CDNUM PIC X(7).
           02 REF-NAME PIC X(24).
           02 REF-KP PIC X(7).
           02 REF-FUTURE PIC XXX.
       FD BILLPARM
           DATA RECORD IS BILLPARM01.
       01  BILLPARM01.
           02 PF1 PIC 999.
           02 PF2 PIC 999.
           02 PF3 PIC 999.
       FD OUT-FILE
           DATA RECORD IS OUT01.
       01  OUT01 PIC X(86).
       FD PROCFILE
           DATA RECORD PROCFILE01.
       01 PROCFILE01.
           02 PROC-KEY.
             03 PROC5 PIC X(5).
             03 PROCXX PIC XX.
           02 PROC-NDC PIC X.
           02 PROC-OLD PIC X(6).
           02 PROC-TYPE PIC X.
           02 PROC-BCBS PIC X(4).
           02 PROC-TITLE PIC X(28).
           02 PROC-AMOUNT PIC S9(4)V99.
           02 CARE-AMOUNT PIC S9(4)V99.

       WORKING-STORAGE SECTION.
       01  CHR01.
           02 CHR02 OCCURS 970 TIMES.
             03 CHR-PLACE PIC X.
             03 CHR-DAT1 PIC X(8).
             03 CHR-DATE-T PIC X(8).
             03 CHR-DATE-A PIC X(8).
             03 CHR-CLAIM PIC X(6).
             03 CHR-AMOUNT PIC S9(4)V99.
             03 CHR-PAYCODE PIC X(3).
             03 CHR-ASSIGN PIC X.
             03 CHR-PROC PIC X(7).
             03 CHR-MOD2 PIC XX.
             03 CHR-UNIT PIC XX.
             03 CHR-DIAG PIC X(7).
             03 CHR-DOCR PIC XXX.
             03 CHR-DOCP PIC XX.
             03 CHR-PAPER PIC X.
             03 CHR-PATID PIC X(8).
             03 CHR-W PIC X.
             03 CHR-REC-STAT PIC X.
       01  PHR01.
           02 PHR02 OCCURS 970 TIMES.
             03 PHR-DATE PIC X(8).
             03 PHR-CLAIM PIC X(6).
             03 PHR-AMOUNT PIC S9(4)V99.
             03 PHR-PAYCODE PIC 999.
             03 PHR-DENIAL PIC XX.
             03 PHR-W PIC X.
       01  PLACE-TAB01.
           02 PLACE-TAB OCCURS 26 TIMES.
             03 PL-TAB PIC X.
             03 PL-NUM PIC X.
             03 PL-NAME PIC X(22).
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
       01  INS-TAB01.
           02 INS-TAB PIC X(22) OCCURS 999 TIMES.
       01 DAY-TEST-1.
           02 DY1 PIC 9999.
           02 DM1 PIC 99.
           02 DD1 PIC 99.
       01  DAY-TEST-2.
           02 DY2 PIC 9999.
           02 DM2 PIC 99.
           02 DD2 PIC 99.
       01 LINE-1.
           02 L1F1 PIC X(8).
           02 F11 PIC X(2).
           02 L1F2 PIC X(24).
           02 F12 PIC X.
           02 L1F3 PIC X(8).
           02 FILLER PIC XX VALUE SPACE.
           02 L1F6 PIC X(22).
       01 LINE-2.
           02 F21 PIC X(10).
           02 L2F2 PIC X(22).
           02 F22 PIC X(3).
           02 L2F3 PIC X(10).
           02 F23 PIC X(3).
           02 L2F4 PIC XXX.
           02 FILLER PIC X VALUE SPACE.
           02 L2F4C PIC XX.
           02 FILLER PIC X VALUE SPACE.
           02 L2FG PIC X(7).
           02 FILLER PIC X VALUE SPACE.
           02 L2F5 PIC X(16).
           02 F25 PIC X.
           02 L2F6 PIC XXX.
       01  LINE-3.
           02 L2F1 PIC X(8).
           02 F31 PIC X(2).
           02 L3F1 PIC X(27).
           02 F32 PIC X(11).
           02 L3F2 PIC XXX.
           02 FILLER PIC X VALUE SPACE.
           02 L3F2C PIC XX.
           02 FILLER PIC X VALUE SPACE.
           02 L3F3 PIC X(7).
           02 FILLER PIC X VALUE SPACE.
           02 L3F4 PIC X(9).
           02 FILLER PIC X VALUE SPACE.
           02 L3F5 PIC XXX.
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
           02 FILLER PIC XX VALUE SPACE.
           02 L5F1 PIC X(8).
           02 F51 PIC XX.
           02 L5F2.
           03 L5F21 PIC X(6).
           03 L5F22 PIC X(21).
           03 FILLER PIC X VALUE SPACE.
           03 L5F23 PIC X(2).
           03 FILLER PIC X VALUE SPACE.
           02 L5F3 PIC X(6).
           02 F53 PIC X(8).
           02 L5F4 PIC ZZZ9.99CR.
           02 F54 PIC XX.
           02 W5 PIC X.
           02 L5F5 PIC ZZZ9.99CR.
       01  LINE-6.
           02 L6F1 PIC X(8).
           02 F61 PIC XX.
           02 L6F2 PIC X(28).
           02 F63 PIC X.
           02 L6F4 PIC X(6).
           02 F64 PIC X.
           02 L6F5 PIC ZZZ9.99CR.
           02 W6 PIC X.
           02 F65 PIC X(9).
           02 L6F6 PIC ZZZ9.99CR.
       01  LINE-6P.
           02 FILLER PIC X(64).
           02 L6PF1 PIC X(11).
       01  LINE-6A.
           02 F6A1 PIC X(9).
           02 L6AF1 PIC X(9).
           02 L6AF2 PIC X(15).
       01  LINE-7.
           02 L7F1 PIC ZZZ9.99CR.
           02 F71 PIC X.
           02 L7F2 PIC ZZZ9.99CR.
           02 F72 PIC X.
           02 L7F3 PIC ZZZ9.99CR.
           02 F73 PIC X.
           02 L7F4 PIC ZZZ9.99CR.
           02 F74 PIC XXX.
           02 L7F5 PIC ZZZ9.99CR.
           02 F75 PIC X(11).
           02 L7F6 PIC ZZZ9.99CR.
       01  LINE-7-1.
           02 L711 PIC X(7) VALUE "AMT DUE".
           02 FILLER PIC XXX VALUE SPACE.
           02 L712 PIC X(7) VALUE "PEN INS".
           02 FILLER PIC X(5) VALUE SPACES.
           02 L713 PIC X(5) VALUE "CURR.".
           02 FILLER PIC X(5) VALUE SPACE.
           02 L714 PIC X(5) VALUE "PAST.".
           02 FILLER PIC X(7) VALUE SPACE.
           02 L715 PIC X(5) VALUE "DELQ.".
           02 FILLER PIC X(15) VALUE SPACE.
           02 L716 PIC X(5) VALUE "COLL.".
       01 LINE-8.
           02 L8F1 PIC X(24).
           02 F81 PIC X.
           02 L8F2 PIC X(8).
           02 F82 PIC X.
           02 L8F3 PIC XXX.
           02 F83 PIC X.
           02 L8F4 PIC X(27).
       01  LINE-9.
           02 L9F1 PIC X(15).
           02 F92 PIC X VALUE SPACE.
           02 L9F11 PIC XXX.
           02 FILLER PIC X VALUE SPACE.
           02 L9F2 PIC X(11).
           02 FILLER PIC X VALUE SPACE.
           02 L9MOD2 PIC XX.
           02 FILLER PIC X VALUE SPACE.
           02 L9UNIT PIC XX.
           02 F93 PIC X VALUE SPACE.
           02 L9F3 PIC X(7).
           02 F94 PIC X VALUE SPACE.
           02 L9F4 PIC X.
           02 F95 PIC X VALUE SPACE.
           02 L9F5 PIC X(8).
           02 F96 PIC X VALUE SPACE.
           02 L9F6 PIC XX.
           02 F97 PIC X VALUE SPACE.
           02 L9F7 PIC X(15).
           02 FILLER PIC X VALUE SPACE.
           02 L9F8 PIC X(8).
           02 L9-PAPER PIC X.
           02 L9F9 PIC X.
       01  HEADING-1.
           02 FILLER PIC X(12) VALUE "DATE        ".
           02 FILLER PIC X(29) VALUE "DESCRIPTION OF SERVICE       ".
           02 FILLER PIC X(8) VALUE "CLAIM   ".
           02 FILLER PIC X(8) VALUE "CHARGE  ".
           02 FILLER PIC X(7) VALUE "PAYMENT".
           02 FILLER PIC X(4) VALUE SPACES.
           02 FILLER PIC X(7) VALUE "CLM BAL".
       01  WSL1F1.
           02 WSL1F1M PIC XX.
           02 FILLER PIC X VALUE " ".
           02 WSL1F1D PIC XX.
           02 FILLER PIC X VALUE " ".
           02 WSL1F1Y PIC XX.
       01  INPUT-DATE-S.
           05 T-MM  PIC XX.
           05 T-DD  PIC XX.
           05 T-YY  PIC XX.
       01  TEST-DATE.
           05 T-CC  PIC XX.
           05 T-YY  PIC XX.
           05 T-MM  PIC XX.
           05 T-DD  PIC XX.
       01  CONSTANTS.
           02 ACTION PIC XX.
           02 ANS PIC X(24).
           02 DATAIN PIC X(24).
           02 BILLDATE PIC X(8).
           02 CH-HI PIC 999.
           02 PH-HI PIC 999.
           02 CC-LOW PIC 999.
           02 PC-LOW PIC 999.
           02 DAY1 PIC 999.
           02 DAY2 PIC 999.
           02 FLAG PIC 9.
           02 SNUM-6 PIC S9(4)V99.
           02 TEST-BUSNAME.
             03 TEST-BUSNAME-1 PIC X.
             03 TEST-BUSNAME-2 PIC X(23).
           02 CLAIM-TOT PIC S9(4)V99.
           02 HOLD-CHARHIS-KEY.
             03 HOLD-CHARHIS-KEY-8 PIC X(8).
             03 FILLER PIC XXX.
           02 HOLD-PAYHIS-KEY.
             03 HOLD-PAYHIS-KEY-8 PIC X(8).
             03 FILLER PIC XXX.
           02 REF-IND PIC 999.
           02 LINE-CTR PIC 99.
           02 BILL-PAGE PIC 99.
           02 AMOUNT-DUE PIC S9(4)V99.
           02 BAL-FWD PIC S9(4)V99.
           02 DAYS PIC 9999.
           02 AT-3.
             03 AT-3-1 PIC XX.
             03 AT-3-2 PIC X.
           02 BUSNAME.
             03 BUSNAME-1 PIC X.
             03 BUSNAME-2 PIC X(23).
           02 NAME-FIRST PIC X(24).
           02 NAME-MIDDLE PIC X(24).
           02 NAME-LAST.
             03 NL-3 PIC XXX.
             03 FILLER PIC X(21).
           02 NAME-TEST.
             03 NT-3 PIC XXX.
             03 NT-21 PIC X(21).
           02 CHR PIC 999.
           02 PHR PIC 999.
           02 PHR-IND PIC 999.
           02 CHR-IND PIC 999.
           02 TOTCHAR PIC S9(4)V99.
           02 TOTPAY PIC S9(4)V99.
           02 HOLD-CHARCUR-KEY.
             03 HOLD-CHARCUR-KEY-8 PIC X(8).
             03 FILLER PIC XXX.
           02 HOLD-PAYCUR-KEY.
             03 HOLD-PAYCUR-KEY-8 PIC X(8).
             03 FILLER PIC XXX.
           02 RIGHT-8 PIC X(8) JUST RIGHT.
           02 RIGHT-2 PIC XX JUST RIGHT.
           02 RIGHT-3 PIC XXX JUST RIGHT.
           02 RIGHT-4 PIC X(4) JUST RIGHT.
           02 ALF-1 PIC X.
           02 XALF-1 PIC X.
           02 ALF-2 PIC XX.
           02 ALF-3 PIC XXX.
           02 ALF-4 PIC XXXX.
           02 ALF-5 PIC X(5).
           02 ALF-6 PIC X(6).
           02 ALF-7 PIC X(7).
           02 ALF-8 PIC X(8).
           02 ALF-11 PIC X(11).
           02 ALF-13 PIC X(13).
           02 ALF-14 PIC X(14).
           02 EIGHTPARTID.
             03 FILLER PIC X(7).
             03 PART-8 PIC X.
           02 ABC PIC XXX.
           02 XYZ PIC 999.
           02 RIGHT-5 PIC X(5) JUST RIGHT.
           02 RIGHT-7 PIC X(7) JUST RIGHT.
           02 NUM-2 PIC 99.
           02 NUM-3 PIC 999.
           02 NUM-9 PIC 9(9).
           02 NUM-6 PIC 9(6).
           02 A PIC 999.
           02 B PIC 999.
           02 C PIC S999.
           02 D PIC 999.
           02 X PIC 999.
           02 Y PIC 999.
           02 Z PIC 999.
       01 LINE-OUT  PIC X(80).
       01     XTRA PIC X.
       01     PLINDX PIC 99 VALUE 0.
       01     G-BALCUR PIC S9(5)V99.
       01     G-BAL30  PIC S9(5)V99.
       01     G-BAL60  PIC S9(5)V99.
       01     G-BALCOL  PIC S9(5)V99.
       01     X-INSPEND  PIC S9(5)V99.
       01 LLTAB2401.
           02 LLTAB24 PIC X OCCURS 24 TIMES.
       01 FFTAB1001.
           02 FFTAB10 PIC X OCCURS 10 TIMES.
       01  FF PIC 99.
       01 LL PIC 99.
       01 XLLTAB2401.
           02 XLLTAB24 PIC X OCCURS 24 TIMES.
       01 XFFTAB1001.
           02 XFFTAB10 PIC X OCCURS 10 TIMES.
       01 FLAGX PIC 9.
       01 ENDFLAG PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT DOCFILE REFPHY BILLPARM GARFILE CHARCUR
           PAYCUR CHARFILE PAYFILE PROCFILE CMNTFILE
           INSFILE OUTPUT OUT-FILE.
           OPEN INPUT FILEIN.
           READ DOCFILE AT END GO TO P00.
       P00. READ DOCFILE AT END GO TO P000.
           ADD 1 TO PLINDX.
           MOVE DF1 TO PL-TAB(PLINDX)
           MOVE DF2 TO PL-NUM(PLINDX)
           MOVE DF3 TO PL-NAME(PLINDX)
           GO TO P00.
       P000.
           MOVE SPACE TO INS-TAB01.
           READ BILLPARM AT END GO TO D1.
       D1. READ INSFILE NEXT AT END GO TO R1.
           MOVE INS-NAME TO INS-TAB(INS-KEY).
           GO TO D1.

       R1-0. READ FILEIN AT END GO TO R20.
       R1. IF ENDFLAG = 1 GO TO R20.
           MOVE FI-1     TO G-GARNO READ  GARFILE INVALID
           GO TO R1-0.
           IF G-LASTBILL NOT NUMERIC MOVE "00000000" TO G-LASTBILL.
           MOVE G-GARNO TO CC-KEY8
           ACCEPT ALF-8 FROM CENTURY-DATE.
           IF G-LASTBILL = ZERO MOVE ALF-8 TO BILLDATE
           ELSE MOVE G-LASTBILL TO BILLDATE.
******* READ IN ALL   RECORDS TO TABLES FOR THIS GUARANTOR *******
           MOVE SPACES TO L1F1 L2F1 L3F1.
           MOVE 0 TO G-BALCOL X-INSPEND
            G-BALCUR G-BAL30 G-BAL60   BAL-FWD.
           MOVE 0 TO XYZ.
           MOVE 0 TO CHR-IND PHR-IND CHR PHR CH-HI PH-HI CC-LOW PC-LOW.
           MOVE FILEIN01 TO CHARCUR-KEY.
       R6. READ CHARCUR INVALID GO TO R1-1.
           IF G-GARNO NOT = CC-KEY8 GO TO R7.
           ADD 1 TO CHR-IND.
           IF CHR-IND > 970
           DISPLAY "TOO MANY CHARGES. USE 9X"
           DISPLAY CHARCUR-KEY " CHARCUR-KEY"
           GO TO R1.
           IF CC-DATE-A = "00000000" MOVE BILLDATE TO
           CHR-DATE-A(CHR-IND)
           MOVE "N" TO CHR-W(CHR-IND)
           ELSE MOVE CC-DATE-A TO CHR-DATE-A(CHR-IND)
           MOVE "B" TO CHR-W(CHR-IND).
           MOVE CC-DAT1 TO CHR-DAT1(CHR-IND)
           MOVE CC-WORK TO CHR-UNIT(CHR-IND)
           MOVE CC-DIAG TO CHR-DIAG(CHR-IND)
           MOVE CC-PLACE TO CHR-PLACE(CHR-IND)
           MOVE CC-DATE-T TO CHR-DATE-T(CHR-IND)
           MOVE CC-CLAIM TO CHR-CLAIM(CHR-IND)
           MOVE CC-AMOUNT TO CHR-AMOUNT(CHR-IND)
           MOVE CC-PAYCODE TO CHR-PAYCODE(CHR-IND)
           MOVE CC-ASSIGN TO CHR-ASSIGN(CHR-IND)
           MOVE CC-PAPER TO CHR-PAPER(CHR-IND)
           MOVE CC-DOCR TO CHR-DOCR(CHR-IND)
           MOVE CC-DOCP TO CHR-DOCP(CHR-IND)
           MOVE CC-PROC TO CHR-PROC(CHR-IND)
           MOVE CC-MOD2 TO CHR-MOD2(CHR-IND)

           MOVE CC-REC-STAT TO CHR-REC-STAT(CHR-IND)
           MOVE CC-PATID TO CHR-PATID(CHR-IND).
       R1-1.
           READ FILEIN   AT END MOVE 1 TO ENDFLAG GO TO R7.
           IF FI-1 NOT = G-GARNO GO TO R7.
           MOVE FILEIN01 TO CHARCUR-KEY.
           GO TO R6.
       R7. MOVE 0 TO XYZ.
           MOVE G-GARNO TO PC-KEY8. MOVE "000" TO PC-KEY3.
           START PAYCUR KEY > PAYCUR-KEY INVALID GO TO R9.
       R8. READ PAYCUR NEXT AT END GO TO R9.
           IF G-GARNO NOT = PC-KEY8 GO TO R9.
           ADD 1 TO PHR-IND.
           IF PHR-IND > 970
           DISPLAY "TOO MANY PAYMENTS USE 9X"
           DISPLAY PAYCUR-KEY " PAYCUR-KEY"
           GO TO R1.
           MOVE PC-DATE-T TO PHR-DATE(PHR-IND)
           MOVE PC-CLAIM TO PHR-CLAIM(PHR-IND)
           MOVE PC-AMOUNT TO PHR-AMOUNT(PHR-IND)
           MOVE PC-PAYCODE TO PHR-PAYCODE(PHR-IND)
           MOVE "P" TO PHR-W(PHR-IND)
           MOVE PC-DENIAL TO PHR-DENIAL(PHR-IND).
           GO TO R8.
       R9. MOVE 0 TO XYZ.
           MOVE G-GARNO TO CD-KEY8. MOVE "000" TO CD-KEY3.
           START CHARFILE KEY > CHARFILE-KEY INVALID GO TO D7.
       D6. READ CHARFILE NEXT AT END GO TO D7.
           IF G-GARNO NOT = CD-KEY8 GO TO D7.
           ADD 1 TO CHR-IND.
           IF CHR-IND > 970
           DISPLAY "TOO MANY CHARGES. USE 9X"
           DISPLAY CHARFILE-KEY " CHARFILE-KEY"
           GO TO R1.
           MOVE CD-DATE-A TO CHR-DATE-A(CHR-IND).
           MOVE CD-DAT1 TO CHR-DAT1(CHR-IND)
           MOVE CD-WORK TO CHR-UNIT(CHR-IND)
           MOVE CD-DIAG TO CHR-DIAG(CHR-IND)
           MOVE CD-PLACE TO CHR-PLACE(CHR-IND)
           MOVE CD-DATE-T TO CHR-DATE-T(CHR-IND)
           MOVE CD-CLAIM TO CHR-CLAIM(CHR-IND)
           MOVE CD-AMOUNT TO CHR-AMOUNT(CHR-IND)
           MOVE CD-PAYCODE TO CHR-PAYCODE(CHR-IND)
           MOVE CD-DOCR TO CHR-DOCR(CHR-IND)
           MOVE CD-DOCP TO CHR-DOCP(CHR-IND)
           MOVE CD-PROC TO CHR-PROC(CHR-IND)
           MOVE CD-MOD2 TO CHR-MOD2(CHR-IND)
           MOVE CD-ASSIGN TO CHR-ASSIGN(CHR-IND)
           MOVE CD-PAPER TO CHR-PAPER(CHR-IND)
           MOVE "D" TO CHR-W(CHR-IND)
           MOVE "0" TO CHR-REC-STAT(CHR-IND)
           MOVE CD-PATID TO CHR-PATID(CHR-IND).
           GO TO D6.
       D7. MOVE 0 TO XYZ.
           MOVE G-GARNO TO PD-KEY8. MOVE "000" TO PD-KEY3.
           START PAYFILE KEY > PAYFILE-KEY INVALID GO TO D9.
       D8. READ PAYFILE NEXT AT END GO TO D9.
           IF G-GARNO NOT = PD-KEY8 GO TO D9.
           ADD 1 TO PHR-IND.
           IF PHR-IND > 970
           DISPLAY "TOO MANY PAYMENTS. USE 9X"
           DISPLAY PAYFILE-KEY " PAYFILE-KEY"
           GO TO R1.
           MOVE PD-DATE-T TO PHR-DATE(PHR-IND)
           MOVE PD-CLAIM TO PHR-CLAIM(PHR-IND)
           MOVE PD-AMOUNT TO PHR-AMOUNT(PHR-IND)
           MOVE PD-PAYCODE TO PHR-PAYCODE(PHR-IND)
           MOVE "D" TO PHR-W(PHR-IND)
           MOVE PD-DENIAL TO PHR-DENIAL(PHR-IND).
           GO TO D8.

******* START HERE TO DETERMINE AMOUNT DUE AND INSURANCE PENDING *******
******* AND AGING ON CURRENT,PAST DUE AND DELINQUENT *******
       D9.
           ACCEPT ALF-8 FROM CENTURY-DATE.
           PERFORM CC1 THRU CC-EXIT VARYING X FROM 1 BY 1 UNTIL
           X > CHR-IND.
           GO TO R10.

       CC1. IF CHR-ASSIGN(X) = "A"
           GO TO CC2.
           MOVE CHR-AMOUNT(X) TO CLAIM-TOT.
           PERFORM PH2 VARYING Y FROM 1 BY 1 UNTIL Y > PHR-IND.
           IF CHR-REC-STAT(X) = "0" OR "2" ADD CLAIM-TOT TO G-BALCUR
           GO TO CC-EXIT.
           MOVE CHR-DATE-A(X) TO DAY-TEST-1.
           MOVE 0 TO D.
           DIVIDE DY1 BY 4 GIVING B REMAINDER D.
           IF D = 0 COMPUTE DAY1 = LEAP-TAB(DM1) + DD1
           ELSE COMPUTE DAY1 = MON-TAB(DM1) + DD1.
           MOVE BILLDATE TO DAY-TEST-2.
           MOVE 0 TO D.
           DIVIDE DY2 BY 4 GIVING B REMAINDER D.
           IF D = 0 COMPUTE DAY2 = LEAP-TAB(DM2) + DD2
           ELSE COMPUTE DAY2 = MON-TAB(DM2) + DD2.
           COMPUTE DAYS = 365 * (DY2 - DY1) + DAY2 - DAY1
           IF DAYS > PF3 ADD CLAIM-TOT TO G-BALCOL GO TO CC-EXIT.
           IF DAYS > PF2 ADD CLAIM-TOT TO G-BAL60 GO TO CC-EXIT.
           IF DAYS > PF1 ADD CLAIM-TOT TO G-BAL30 GO TO CC-EXIT.
           ADD CLAIM-TOT TO G-BALCUR. GO TO CC-EXIT.
       CC2. MOVE CHR-AMOUNT(X) TO CLAIM-TOT
           PERFORM PH2 VARYING Y FROM 1 BY 1 UNTIL Y > PHR-IND.
           ADD CLAIM-TOT TO X-INSPEND.

       CC-EXIT. EXIT.
       PH2. IF CHR-CLAIM(X) = PHR-CLAIM(Y)
           ADD PHR-AMOUNT(Y) CLAIM-TOT GIVING CLAIM-TOT.

******* SORT ALL TABLES BY DATE *******

       R10. IF CHR-IND > 1
           SUBTRACT 1 FROM CHR-IND GIVING Y
           PERFORM S10 VARYING X FROM 1 BY 1 UNTIL X > Y.
           IF PHR-IND > 1
           SUBTRACT 1 FROM PHR-IND GIVING Y
           PERFORM S12 VARYING X FROM 1 BY 1 UNTIL X > Y.

******* START WRITING OUTPUT *******
       R13. ADD G-BALCUR G-BAL30 G-BAL60 G-BALCOL GIVING AMOUNT-DUE.

           MOVE SPACES TO LINE-1 LINE-2 LINE-3 LINE-4 LINE-5 LINE-6
           LINE-8 LINE-6A.
           MOVE SPACES TO F11 F12  .
           MOVE G-GARNAME TO TEST-BUSNAME.
           IF TEST-BUSNAME-1 = "1" MOVE TEST-BUSNAME-2
           TO L1F2 GO TO R14.
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
           MOVE SPACES TO L1F2
           IF NAME-MIDDLE = SPACES
           STRING NAME-FIRST " " NAME-LAST DELIMITED BY "@" INTO L1F2
           ELSE STRING NAME-FIRST " " NAME-MIDDLE " " NAME-LAST
           DELIMITED BY "@" INTO L1F2.
       R14. MOVE G-GARNO TO L1F3.
           MOVE BILLDATE TO TEST-DATE.
           MOVE CORR TEST-DATE TO INPUT-DATE-S.
           MOVE T-MM OF INPUT-DATE-S TO WSL1F1M.
           MOVE T-DD OF INPUT-DATE-S TO WSL1F1D.
           MOVE T-YY OF INPUT-DATE-S TO WSL1F1Y.
           MOVE WSL1F1 TO L1F1 L2F1.
           MOVE SPACES TO WSL1F1.
           MOVE G-BILLADD TO L1F6.
           WRITE OUT01 FROM LINE-1.
           MOVE SPACES TO F21 F22 F23 F25.
           MOVE G-STREET TO L2F2.
           MOVE SPACE TO L2F3
           MOVE G-PRINS TO L2F4.
           MOVE G-PR-GROUP TO L2FG
           MOVE G-PRIPOL TO L2F5.
           WRITE OUT01 FROM LINE-2.
           MOVE SPACES TO TAB2001.
           MOVE G-CITY TO TAB2001.
           MOVE G-STATE TO AT-3-1.
           MOVE "@" TO AT-3-2.
           MOVE SPACES TO L3F1.
           IF TAB20(18) NOT = " " MOVE "," TO TAB20(19)
           MOVE "@" TO TAB20(20)
           STRING TAB2001 " " AT-3 " " G-ZIP DELIMITED BY "@" INTO
           L3F1 GO TO R14-1.
           PERFORM Q1 VARYING C FROM 17 BY -1 UNTIL C < 1.
           STRING TAB2001 " " AT-3 " " G-ZIP DELIMITED BY "@" INTO
           L3F1.
       R14-1.
           MOVE SPACES TO F31 F32
           MOVE G-SEINS TO L3F2
           MOVE G-SE-GROUP TO L3F3 MOVE G-SECPOL TO L3F4
           WRITE OUT01 FROM LINE-3.
           WRITE OUT01 FROM LINE-4.
           WRITE OUT01 FROM HEADING-1.
       R15. PERFORM Q2 THRU Q10 VARYING A FROM 1 BY 1 UNTIL A >
            CHR-IND.
           MOVE AMOUNT-DUE TO L7F1
           MOVE X-INSPEND TO L7F2
           MOVE G-BALCUR TO L7F3
           MOVE G-BAL30 TO L7F4
           MOVE G-BAL60 TO L7F5
           MOVE G-BALCOL TO L7F6
           MOVE SPACES TO F71 F72 F73 F74 F75.
           WRITE OUT01 FROM LINE-4.
           WRITE OUT01 FROM LINE-7-1.
           WRITE OUT01 FROM LINE-7
           MOVE SPACES TO L8F1
           UNSTRING NAME-LAST DELIMITED BY "@" INTO L8F1
           MOVE G-GARNO TO L8F2
           MOVE SPACE TO L8F3
           MOVE SPACES TO F81 F82 F83.
           IF G-BALCOL > 0 MOVE "COLLECTION REVIEW ACCOUNT"
           TO L8F4 GO TO R15-1.
           IF G-BAL60 > 0 MOVE "YOUR ACCOUNT IS DELINQUENT."
           TO L8F4 GO TO R15-1.
           IF G-BAL30 > 0 MOVE "YOUR ACCOUNT IS PAST DUE."
           TO L8F4 GO TO R15-1.
           MOVE SPACES TO L8F4.
       R15-1.
           WRITE OUT01 FROM LINE-8  AFTER 2.
           MOVE SPACE TO OUT01 WRITE OUT01
           MOVE G-GARNO TO CMNT8 MOVE "000" TO CMNT3.
           START CMNTFILE KEY > CMNT-KEY INVALID GO TO R1.
       CMNT-1. READ CMNTFILE NEXT AT END GO TO CMNT-2.
           IF CMNT8 NOT = G-GARNO GO TO CMNT-2.
           WRITE OUT01 FROM CMNT-TITLE GO TO CMNT-1.
       CMNT-2. MOVE SPACE TO OUT01 WRITE OUT01 AFTER 2 GO TO R1.
       R20. CLOSE OUT-FILE GARFILE CHARCUR CHARFILE PAYFILE
           PROCFILE REFPHY INSFILE CMNTFILE. STOP RUN.

******* FIND LAST CHARACTER IN CITY NAME *******
       Q1. IF TAB20(C) NOT = " "
           ADD 1 C GIVING B
           MOVE "," TO TAB20(B)
           ADD 1 TO B
           MOVE "@" TO TAB20(B)
           MOVE 1 TO C.

******* WRITE THE DETAIL LINES ON BILL FOR CHARGES *******
       Q2. MOVE CHR-DATE-T(A) TO TEST-DATE.
           MOVE CORR TEST-DATE TO INPUT-DATE-S.
           INSPECT T-MM OF INPUT-DATE-S REPLACING LEADING "0" BY " ".
           MOVE T-MM OF INPUT-DATE-S TO WSL1F1M.
           MOVE T-DD OF INPUT-DATE-S TO WSL1F1D.
           MOVE T-YY OF INPUT-DATE-S TO WSL1F1Y.
           MOVE WSL1F1 TO L6F1.
           MOVE G-GARNAME TO NAME-TEST.
       Q4. MOVE SPACES TO L6F2.
           MOVE CHR-PROC(A) TO PROC-KEY.
           READ PROCFILE INVALID GO TO Q4-1.
           MOVE PROC-TITLE TO L6F2.

       Q4-1.
           MOVE CHR-CLAIM(A) TO L6F4.
           MOVE CHR-AMOUNT(A) TO L6F5 L6F6.
******* SEARCH PAYMENT TABLE FOR PAYMENTS AGAINST THIS CLAIM *******
       Q51.
           MOVE CHR-DOCR(A) TO REF-KEY.
           READ REFPHY INVALID MOVE SPACE TO REF-NAME.
           MOVE REF-NAME TO L9F1.
           MOVE CHR-PAYCODE(A) TO L9F11
           MOVE CHR-PROC(A) TO L9F2.
           MOVE CHR-MOD2(A) TO L9MOD2
           MOVE CHR-UNIT(A) TO L9UNIT
           MOVE CHR-DIAG(A) TO L9F3.
           MOVE CHR-PLACE(A) TO L9F4.
           IF CHR-DAT1(A) NOT = "00000000"
           MOVE CHR-DAT1(A) TO L9F5
           ELSE MOVE SPACE TO L9F5.
           MOVE CHR-DOCP(A) TO L9F6.
           MOVE CHR-DATE-A(A) TO L9F8.
           MOVE CHR-PAPER(A) TO L9-PAPER
           MOVE CHR-REC-STAT(A) TO L9F9.
           PERFORM DF-SEARCH.
           MOVE 0 TO FLAG.
           MOVE CHR-AMOUNT(A) TO CLAIM-TOT.
           MOVE CHR-W(A) TO W6.
           PERFORM T24 THRU T30 VARYING X FROM 1 BY 1 UNTIL X > PHR-IND.
           IF FLAG = 1 GO TO Q10.
           IF CHR-ASSIGN(A) = "A"
           MOVE LINE-6 TO LINE-6P
           MOVE "PENDING INS" TO L6PF1
           WRITE OUT01 FROM LINE-6P
           ELSE WRITE OUT01 FROM LINE-6.
      *    IF XTRA NOT = SPACE
           WRITE OUT01 FROM LINE-9.
       Q10. EXIT.
       T5. IF TABA24(C) NOT = " "
           ADD 1 TO C
           MOVE "@" TO TABA24(C)
           MOVE 1 TO C.
       T22. ADD 1 TO D MOVE TABA24(Z) TO TAB11(D).
       T23. ADD 1 TO D MOVE TABB24(Z) TO TAB11(D).
       T20. IF TABA24(C) NOT = " " MOVE C TO X
           MOVE 1 TO C.
       T21. IF TABB24(C) NOT = " " MOVE C TO Z
           MOVE 1 TO C.
       T24. IF PHR-CLAIM(X) NOT = CHR-CLAIM(A) GO TO T30.
           IF FLAG = 1 GO TO T25.
           MOVE 1 TO FLAG.
           MOVE SPACES TO F61 F63 F64 F65.
           IF CHR-ASSIGN(A) = "U"
           WRITE OUT01 FROM LINE-6
           ELSE MOVE LINE-6 TO LINE-6P
           MOVE "PENDING INS" TO L6PF1
           WRITE OUT01 FROM LINE-6P.
      *    IF XTRA NOT = SPACE
           WRITE OUT01 FROM LINE-9.
       T25. MOVE PHR-DATE(X) TO TEST-DATE.
           MOVE CORR TEST-DATE TO INPUT-DATE-S.
           INSPECT T-MM OF INPUT-DATE-S REPLACING LEADING "0" BY " ".
           MOVE T-MM OF INPUT-DATE-S TO WSL1F1M.
           MOVE T-DD OF INPUT-DATE-S TO WSL1F1D.
           MOVE T-YY OF INPUT-DATE-S TO WSL1F1Y.
           MOVE WSL1F1 TO L5F1.
           MOVE "PAYMT " TO L5F21.
           MOVE PHR-PAYCODE(X) TO NUM-3.
           MOVE INS-TAB(NUM-3) TO L5F22.
      *    IF L5F22 = SPACE MOVE "MISC. INS." TO L5F22.
           MOVE SPACE TO L5F23.
           IF PHR-DENIAL(X) = "DD" MOVE "DEDCT NOT MET"
           TO L5F23.
           IF PHR-DENIAL(X) = "NC" MOVE "NONCOVER SERV"
           TO L5F23.
           IF PHR-DENIAL(X) = "NR" MOVE "NO INS. REPLY"
           TO L5F23.
           IF PHR-DENIAL(X) = "CE" MOVE "INVLD CERT NO"
           TO L5F23.
           IF PHR-DENIAL(X) = "DA" MOVE "NONCOVER DATE"
           TO L5F23.
           IF PHR-PAYCODE (X) = 008 OR 015 MOVE SPACE TO L5F21.
           IF NOT (PHR-DENIAL(X) = "  " OR "DD") MOVE SPACE TO L5F21.
           MOVE PHR-DENIAL(X) TO L5F23.
       T26. MOVE PHR-AMOUNT(X) TO L5F4.
           MOVE PHR-CLAIM(X) TO L5F3.
           MOVE SPACES TO F51  F53 F54.
           ADD PHR-AMOUNT(X) TO CLAIM-TOT.
           MOVE PHR-W(X) TO W5.
           MOVE CLAIM-TOT TO L5F5.
           WRITE OUT01 FROM LINE-5.
           GO TO T30.
       T29. IF CHR-CLAIM(A) = PHR-CLAIM(Y)
           ADD PHR-AMOUNT(Y) TO SNUM-6.
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
           MOVE CHR-DAT1(X) TO ALF-8
           MOVE CHR-DAT1(A) TO CHR-DAT1(X)
           MOVE ALF-8 TO CHR-DAT1(A)

           MOVE CHR-UNIT(X) TO ALF-2
           MOVE CHR-UNIT(A) TO CHR-UNIT(X)
           MOVE ALF-5 TO CHR-UNIT(A)

           MOVE CHR-DIAG(X) TO ALF-7
           MOVE CHR-DIAG(A) TO CHR-DIAG(X)
           MOVE ALF-7 TO CHR-DIAG(A)
           MOVE CHR-PLACE(X) TO ALF-1
           MOVE CHR-PLACE(A) TO CHR-PLACE(X)
           MOVE ALF-1 TO CHR-PLACE(A)
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
           MOVE CHR-MOD2(X) TO ALF-2
           MOVE CHR-MOD2(A) TO CHR-MOD2(X)
           MOVE ALF-2 TO CHR-MOD2(A)
           MOVE CHR-PROC(X) TO ALF-7
           MOVE CHR-PROC(A) TO CHR-PROC(X)
           MOVE ALF-7 TO CHR-PROC(A)
           MOVE CHR-PAPER(X) TO ALF-1
           MOVE CHR-PAPER(A) TO CHR-PAPER(X)
           MOVE ALF-1 TO CHR-PAPER(A)
           MOVE CHR-PATID(X) TO ALF-8
           MOVE CHR-PATID(A) TO CHR-PATID(X)
           MOVE ALF-8 TO CHR-PATID(A)
           MOVE CHR-W(X) TO ALF-1
           MOVE CHR-W(A) TO CHR-W(X)
           MOVE ALF-1 TO CHR-W(A).
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
           MOVE PHR-DENIAL(X) TO ALF-2
           MOVE PHR-DENIAL(A) TO PHR-DENIAL(X)
           MOVE ALF-2 TO PHR-DENIAL(A)
           MOVE PHR-W(X) TO ALF-1
           MOVE PHR-W(A) TO PHR-W(X)
           MOVE ALF-1 TO PHR-W(A).
       2400-FIND.
           MOVE ANS TO DATAIN.
           MOVE SPACES TO  FFTAB1001 ACTION NAME-LAST.
           UNSTRING DATAIN DELIMITED BY "," INTO ACTION NAME-LAST
           FFTAB1001.
           MOVE NAME-LAST TO LLTAB2401.
           MOVE 0 TO FF LL.
           PERFORM FF-NAME VARYING X FROM 10 BY -1 UNTIL X < 1.
           PERFORM LL-NAME VARYING X FROM 24 BY -1 UNTIL X < 1.
           IF LL = 0 MOVE 1 TO LL.
           IF LLTAB24(LL) = "/"
           MOVE " " TO LLTAB24(LL) MOVE 24 TO LL.
           MOVE SPACE TO G-GARNO.
           IF ACTION = "SG" MOVE NAME-LAST TO G-GARNO
           ELSE MOVE NL-3 TO G-GARNO.
           START GARFILE KEY > G-GARNO INVALID
           DISPLAY " END OF FILE" GO TO R1.
       2400-FIND-20.
           MOVE 0 TO X.
           MOVE 0 TO FLAG.
           PERFORM 2400-SEARCH THRU 2400-SEARCH-EXIT.
           IF FLAG = 0 GO TO 2400-FIND-QUES.
           IF FLAG = 1 DISPLAY "END OF FILE FOUND"
           ELSE DISPLAY "NO MORE MATCHES ON NAME".
           DISPLAY "END OF SEARCH"
           GO TO R1.
       2400-FIND-QUES.
           DISPLAY "?".
           ACCEPT ANS.
           IF ANS = SPACES GO TO 2400-FIND-20.
           IF ANS = "?"
           DISPLAY "A <CR> WILL PRODUCE 10 MORE NAMES"
           DISPLAY "ANYTHING ELSE WILL RETURN YOU TO THE OPTION COMMAND"
           GO TO 2400-FIND-QUES.
           GO TO R1.
       2400-SEARCH.
           IF X > 10 GO TO 2400-SEARCH-EXIT.
           READ GARFILE NEXT AT END MOVE 1 TO FLAG
           GO TO 2400-SEARCH-EXIT.
           IF ACTION = "SG" GO TO 2400-S.
           MOVE SPACE TO NAME-TEST XFFTAB1001.
           UNSTRING G-GARNAME DELIMITED BY ";" INTO NAME-TEST XFFTAB1001
           IF NT-3 = NL-3 NEXT SENTENCE
           ELSE MOVE 2 TO FLAG GO TO 2400-SEARCH-EXIT.
           MOVE NAME-TEST TO XLLTAB2401.
           MOVE 0 TO FLAGX PERFORM AQ1 VARYING Y FROM 1 BY 1 UNTIL
           Y > LL.
           IF FLAGX = 1 GO TO 2400-SEARCH.
           PERFORM AQ2 VARYING Y FROM 1 BY 1 UNTIL Y > FF.
           IF FLAGX = 1 GO TO 2400-SEARCH.
       2400-S.
           MOVE SPACE TO LINE-OUT
           STRING G-GARNO "^" G-GARNAME "^" G-CITY "^" G-DOB
           DELIMITED BY "   " INTO LINE-OUT
           INSPECT LINE-OUT REPLACING ALL "^" BY " "
           DISPLAY LINE-OUT
           ADD 1 TO X
           GO TO 2400-SEARCH.
       2400-SEARCH-EXIT.
           EXIT.
       2400-FIND-EXIT. EXIT.



       FF-NAME. IF FFTAB10(X) NOT = SPACE MOVE X TO FF MOVE 1 TO X.
       LL-NAME. IF LLTAB24(X) NOT = SPACE MOVE X TO LL MOVE 1 TO X.
       AQ1. IF LLTAB24(Y) NOT = XLLTAB24(Y) MOVE 1 TO FLAGX.
       AQ2. IF XFFTAB10(Y) NOT = FFTAB10(Y) MOVE 1 TO FLAGX.
       DF-SEARCH. MOVE SPACE TO L9F7.
           PERFORM DF-SEARCH2 VARYING Y FROM 1 BY 1 UNTIL Y > PLINDX.
       DF-SEARCH2. IF CHR-PLACE(A) = PL-TAB(Y) MOVE PL-NAME(Y) TO L9F7
           MOVE PLINDX TO Y.
       LG1.
           READ GARFILE INVALID DISPLAY "INVALID" GO TO R1.
           DISPLAY G-GARNO " " G-GARNAME
           MOVE SPACE TO LINE-OUT
           STRING G-STREET "^" G-CITY "^" G-STATE "^" G-ZIP DELIMITED
           BY "   " INTO LINE-OUT
           INSPECT LINE-OUT REPLACING ALL "^" BY " ".
           DISPLAY LINE-OUT.
           DISPLAY G-PRINS " " G-PRIPOL " " G-PR-GROUP
           DISPLAY G-SEINS " " G-SECPOL " " G-SE-GROUP
           DISPLAY "SX=" G-SEX " MS=" G-COLLT " RE=" G-RELATE
           " G-DUN=" G-DUNNING " ACST=" G-ACCTSTAT " DOB " G-DOB
           GO TO R1.
