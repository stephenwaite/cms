       IDENTIFICATION DIVISION.
       PROGRAM-ID. ina002.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TAGDIAG ASSIGN TO "S20" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS TAG-KEY
           ALTERNATE RECORD KEY IS TAG-ICD9 WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT PAYFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PAYFILE-KEY
           LOCK MODE MANUAL.
           SELECT INSFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT CLAIMFILE ASSIGN TO "S170" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CLAIM-KEY
           LOCK MODE MANUAL
           STATUS IS CLAIMFILE-STAT.
           SELECT DOCPARM ASSIGN TO "S85" ORGANIZATION IS
           LINE SEQUENTIAL.
           SELECT CHARFILE ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHAR-KEY
           LOCK MODE MANUAL
           STATUS IS CHARFILE-STAT.
           SELECT GARFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
           LOCK MODE MANUAL.
           SELECT PATFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS P-PATNO
           ALTERNATE RECORD KEY IS P-GARNO WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT DIAGFILE ASSIGN TO "S70" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS DIAG-KEY
           ALTERNATE RECORD KEY IS DIAG-TITLE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT PROCFILE ASSIGN TO "S75" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PROC-KEY
           LOCK MODE MANUAL.
           SELECT REFPHY ASSIGN TO "S80" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS REF-KEY
           ALTERNATE RECORD KEY IS REF-BSNUM  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CRNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-UPIN  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CDNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-NAME  WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT PARMNDEX ASSIGN TO "S175" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PARM-KEY
           LOCK MODE MANUAL
           STATUS IS PARMNDEX-STAT.
           SELECT AUTHFILE ASSIGN TO "S190" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS AUTH-KEY
           LOCK MODE MANUAL
           STATUS IS AUTH-STAT.
           SELECT MPLRFILE ASSIGN TO "S165" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS MPLR-KEY
           LOCK MODE IS MANUAL.
           SELECT GAPFILE ASSIGN TO "S90" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS GAPKEY
           ALTERNATE RECORD KEY IS GAP-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-STATE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT FILEOUT ASSIGN TO "S200" ORGANIZATION
           LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  TAGDIAG.
       01  TAGDIAG01.
           02 TAG-KEY.
              03 TAG-7 PIC X(7).
              03 TAG-5 PIC X(5).
           02 TAG-ICD9.
              03 tag-icd9-5 PIC X(5).
              03 tag-icd9-7 PIC X(7).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(40).
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
       FD GAPFILE.
       01 GAPFILE01.
           02 GAPKEY PIC X(7).
           02 GAP-NAME PIC X(25).
           02 GAP-ADDR PIC X(22).
           02 GAP-CITY PIC X(15).
           02 GAP-STATE PIC XX.
           02 GAP-ZIP PIC X(9).
           02 GAP-TYPE PIC X.
           02 GAP-FUTURE PIC X(40).
       FD  MPLRFILE.
       01  MPLRFILE01.
           02 MPLR-KEY PIC X(8). 
           02 MPLR-NAME PIC X(22).
           02 MPLR-STREET PIC X(24).
           02 MPLR-CITY PIC X(15).
           02 MPLR-STATE PIC XX.
           02 MPLR-ZIP PIC X(9).
           02 MPLR-CLAIMNO PIC X(15).
           02 MPLR-TRINS PIC XXX.
           02 MPLR-TR-ASSIGN PIC X.
           02 MPLR-TR-GROUP PIC X(10).
           02 MPLR-TRIPOL PIC X(16).
           02 MPLR-TR-NAME PIC X(24).
           02 MPLR-TR-RELATE PIC X.
           02 MPLR-FUTURE PIC X(6).
       FD  AUTHFILE
           BLOCK CONTAINS 6 RECORDS
           DATA RECORD IS AUTHFILE01.
       01  AUTHFILE01.
           02 AUTH-KEY.
              03 AUTH-KEY8 PIC X(8).
              03 AUTH-KEY6 PIC X(6).
           02 AUTH-NUM PIC X(15).
           02 AUTH-QNTY PIC XX.
           02 AUTH-DATE-E PIC X(8).
           02 AUTH-NDC PIC X(11).
           02 AUTH-FILLER PIC X(30).

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
       FD  PARMNDEX.
       01  PARMNDEX01.
           02 PARM-KEY.
             03 PM-KEY8 PIC X(8).
             03 PM-KEY3 PIC XXX.
           02 PM-DATA PIC X(64).
       FD  DOCPARM
           DATA RECORD IS DOCPARM01.
       01  DOCPARM01.
           02 DP1.
             03 DP-1-1 PIC X.
             03 DP-1-2 PIC X.
           02 DP-2 PIC X(22).
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
           02 REF-NPI PIC X(10).
       FD  CHARFILE
           BLOCK CONTAINS 2 RECORDS
           DATA RECORD IS CHARFILE01.
       01  CHARFILE01.
           02 CHAR-KEY. 
              03 CD-KEY8 PIC X(8).
              03 CD-KEY3 PIC XXX.
           02 PATID PIC X(8).
           02 CLAIM PIC X(6).
           02 SERVICE PIC X.
           02 DIAG PIC X(7).
           02 PROC.
             03 PROC1 PIC X(5).
             03 PROC2 PIC XX.
           02 MOD2 PIC XX.
           02 MOD3 PIC XX.
           02 MOD4 PIC XX.
           02 AMOUNT PIC S9(4)V99.
           02 DOCR PIC X(3).
           02 DOCP PIC X(2).
           02 PAYCODE PIC XXX.
           02 STAT PIC X.
           02 WORK PIC XX.
           02 DAT1 PIC X(8).
           02 RESULT PIC X.
           02 ACT PIC X.
           02 CD-SORCREF PIC X.
           02 CD-COLLT PIC X.
           02 CD-AUTH PIC X.
           02 PAPER PIC X.
           02 PLACE PIC X.
           02 NAME PIC X(24).
           02 CD-EPSDT PIC X.
           02 DATE-T PIC X(8).
           02 DATE-E PIC X(8).
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
       FD GARFILE
           BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS G-MASTER.
       01 G-MASTER.
           02 G-GARNO PIC X(8).
           02 G-GARNAME PIC X(24).
           02 G-BILLADD PIC X(22).
           02 G-STREET PIC X(22).
           02 G-CITY PIC X(18).
           02 G-STATE PIC X(2).
           02 G-ZIP PIC X(9).
           02 G-COLLT PIC X.
           02 G-PHONE. 
              03 G-PHONE1 PIC XXX.
              03 G-PHONE2 PIC XXX.
              03 G-PHONE3 PIC XXXX.
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
       FD  DIAGFILE.
       01  DIAG01.
           02 DIAG-KEY.
              03 diag-9 PIC X(5).
              03 diag-10 pic xx.
           02 DIAG-TITLE.
             03 DIAG-T1 PIC XXXXX.
             03 DIAG-T2 PIC X(56).
           02 DIAG-MEDB PIC X(5).
       FD  PROCFILE
           DATA RECORD IS PROC01.
       01  PROC01.
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
       FD  CLAIMFILE
           DATA RECORD IS CLAIM01.
       01  CLAIM01.
           02 CLAIM-KEY PIC X.
           02 CLAIMNO PIC 9(6).
       WORKING-STORAGE SECTION.
       01  QQQ PIC X(185).
       01  TABDX01.
           02 TABDX PIC X(7) OCCURS 6 TIMES.
       01  PARMNDEX-STAT PIC XX.
       01  CHARFILE-STAT PIC XX.
       01  CLAIMFILE-STAT PIC XX.
       01  AUTH-STAT PIC XX.
       01  PLACE-TAB01.
           02 PLACE-TAB OCCURS 26 TIMES.
             03 PL-TAB PIC X.
             03 PL-NUM PIC X.
             03 PL-NAME PIC X(22).
       01  PLINDX PIC 99.
       01  ANS          PIC X(5).
       01  ACTION       PIC XXX.
       01  DATAIN       PIC X(30).
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
       01 LEN-TAB01-RE.
           02 FILLER PIC X(30) VALUE "110806010711060302030102080602".
           02 FILLER PIC X(30) VALUE "240108010101020107070801010802".
           02 FILLER PIC X(12) VALUE "01010107".
       01 LEN-TAB01 REDEFINES LEN-TAB01-RE.
           02 LEN-TAB   PIC 99 OCCURS 34 TIMES.

       01  Q           PIC 99 VALUE ZERO.
      * NUMERIC FIELD TESTS
       01  FIELD-CODE          PIC XX JUST RIGHT.
      * HOLDS THE POSITION OF A FIELD WITHIN IT'S DATA SET
       01  DES-CONSTANT.
               10 FILLER PIC X(24) VALUE "REC NO  PATNO   CLAIM   ".
               10 FILLER PIC X(24) VALUE "TYP SERVDX-1    PROCEDUR".
               10 FILLER PIC X(24) VALUE "CHARGE  REF PHYSDOCTOR  ".
               10 FILLER PIC X(24) VALUE "PAYCODE REC-STATUNITS   ".
               10 FILLER PIC X(24) VALUE "ACCDATE PAPER   PL. SERV".
               10 FILLER PIC X(24) VALUE "PATNAME EPSDTFPLTRANDATE".
               10 FILLER PIC X(24) VALUE "RESULT  ACTION  AUTHNUM ".
               10 FILLER PIC X(24) VALUE "MOD2    SORCREF DX-2    ".
               10 FILLER PIC X(24) VALUE "DX-3    CLM-DATECOLLT   ".
               10 FILLER PIC X(24) VALUE "ACC-TYPEADMIT-DTMOD3    ".
               10 FILLER PIC X(24) VALUE "XXXX    ASSIGN  NEICASGN".
               10 FILLER PIC X(8) VALUE  "DX-4    ".
      *         DX-5    DX-6    ".

       01  GD-TABLE REDEFINES DES-CONSTANT.
           05 DESC-FLD OCCURS 34 TIMES INDEXED BY INDX.
               10 DES-KEY  PIC X(8).
       
       01  ADD-FIELD-TABLE.
            05 ADD-CON.
             10 FILLER PIC X(36) VALUE
                 "021518060524253422301920082309121328".
             10 FILLER PIC X(10) VALUE "2911273233".
       01  ADD-FIELD-TABLE-RE REDEFINES ADD-FIELD-TABLE.     
            05 ADD-FLD PIC 99 OCCURS 23 TIMES INDEXED BY ADD-KEY.
       01  MULT-ADD PIC X(46).
       01  DISPLAY-DOB.
           02 T-CC PIC XX.
           02 T-YY PIC XX.
           02 T-MM PIC XX.
           02 T-DD PIC XX.
       01  DISPLAY-DATE.
           05 T-MM  PIC 99.
           05 FILLER PIC X VALUE "/".
           05 T-DD  PIC 99.
           05 FILLER PIC X VALUE "/".
           05 T-CC  PIC 99.
           05 T-YY  PIC 99.
       01  DISPLAY-DATE-TOO.
           05 T-MM  PIC 99.
           05 FILLER PIC X VALUE "/".
           05 T-DD  PIC 99.
           05 FILLER PIC X VALUE "/".
           05 T-CC  PIC 99.
           05 T-YY  PIC 99.
       01  INPUT-DATE.
           05 T-MM  PIC 99.
           05 T-DD  PIC 99.
           05 T-CC  PIC 99.
           05 T-YY  PIC 99.
       01  TEST-DATE.
           05 T-CC  PIC 99.
           05 T-YY  PIC 99.
           05 T-MM  PIC 99.
           05 T-DD  PIC 99.
       01  TEST-DATE-S.
           05 T-CC  PIC 99.
           05 T-YY  PIC 99.
           05 T-MM  PIC 99.
           05 T-DD  PIC 99.
       01  DISP-DATE.
           05 T-MM PIC 99.
           05 FILLER PIC X VALUE "/".
           05 T-DD PIC 99.
           05 FILLER PIC X VALUE "/".
           05 T-CC PIC 99.
           05 T-YY PIC 99.
       01  MONTH-TABLE-CONS.
           05  FILLER PIC X(24) VALUE "312931303130313130313031".
       01  MONTH-TABLE REDEFINES MONTH-TABLE-CONS.
           05  DAYS-IN-MONTH OCCURS 12 TIMES PIC 99.
       01  NEF-2 PIC Z9.
       01  NEF-5    PIC Z,ZZZ.ZZ.
       01  NEF-8    PIC ZZZ,ZZZ.ZZCR.
       01  NEF-D    PIC ZZ,ZZZ.99CR.

       01  HOLD-MASTER  .
           02 HOLD-ID PIC X(11).
           02 FILLER PIC X(174).
       01  T-DATE. 
           02 YEAR4.
              04 CC-T PIC XX.
              04 YY-T PIC XX.
           02 MM-T    PIC XX.   
           02 DD-T    PIC XX.
       01     HIGH-PLACE PIC X.
       01     HIGH-DOC PIC XX.
       01     MULT PIC 99.
       01     PART11.
             03 PART8 PIC X(8).
             03 FILLER PIC XXX.
       01  SAVE-GARNO PIC X(8).
       01  SAVEPAY PIC X(80).
       01  save-key8 pic x(8).
       01  save-date-t pic x(8).
       01  kmc-cntr pic 99.
       01  kmctab01.
           02 kmc-tab pic x(30) occurs 8 times.
       01     HOLD-CLAIM PIC X(6).
       01     HOLD-PATID PIC X(8).
       01     HOLD-DATE-T PIC X(8).
       01     HOLD8 PIC X(8).
       01     NEF-7 PIC ZZ,ZZ9.99.
       01     BIG-AMOUNT PIC S9(4)V99.
       01     RIGHT-8 PIC X(8) JUST RIGHT.
       01     RIGHT-2 PIC XX JUST RIGHT.
       01     RIGHT-3 PIC XXX JUST RIGHT.
       01     RIGHT-4 PIC X(4) JUST RIGHT.
       01     PAY-TYPE PIC XX.
       01     ALF-1 PIC X.
       01     XALF-1 PIC X.
       01     NAME-LAST.
             03 NL-3.
               04 NL-31 PIC X.
               04 NL-32 PIC X.
               04 NL-33 PIC X.
             03 FILLER PIC X(21).
       01     NAME-TEST.
             03 NT-3.
               04 NT-31 PIC X.
               04 NT-32 PIC X.
               04 NT-33 PIC X.
             03 NT-21 PIC X(21).
       01     ALF-2 PIC XX.
       01     ALFX-2.
           02 ALFX-21 PIC X.
           02 ALFX-22 PIC X.
       01     ALF-3 PIC XXX.
       01     ALF-4 PIC XXXX.
       01     ALF-5 PIC X(5).
       01     ALF-6 PIC X(6).
       01     ALF-7 PIC X(7).
       01     ALF-8 PIC X(8).
       01  ALF-7X.
           02 ALF-7X5 PIC X(5).
           02 ALF-7X2 PIC XX.
       01  ALF-5X.
           02 ALF-5X1 PIC X.
           02 ALF-5X4 PIC X(4).
       01     ALF-11 PIC X(11).
       01     EIGHTPARTID.
             03 FILLER PIC X(7).
             03 EIGHT-1 PIC X.
       01     ABC PIC XXX.
       01     XYZ PIC 999.
       01     SIGN-DOLLAR PIC X(4).
       01     RIGHT-5 PIC X(5) JUST RIGHT.
       01     RIGHT-7 PIC X(7) JUST RIGHT.
       01     CENTS PIC XX.
       01     NUM1 PIC 9.
       01     NUM-2 PIC 99.
       01     NUM-3 PIC 999.
       01     NUM-6 PIC 9(6).
       01     A PIC 99.
       01     B PIC 99.
       01     C PIC 99.
       01     D PIC 99.
       01     X PIC 99.
       01     Z PIC 99.
       01     NUM2 PIC 99.
       01     Y PIC 99.
       01     YYY PIC 999.
       01     RETURN-FLAG PIC 9.
       01     diag-FLAG PIC 9.
       01     FLAG   PIC 9.
       01  OVERRIDE-AMOUNT PIC 9(4)V99.
       01     TOT-AMOUNT PIC S9(6)V99.
       01  TEMP-FIELD01 PIC X(80).
       01  DF-TAB01.
           02 DF-TAB  PIC 9 OCCURS 34 TIMES.
       01  GAR-TAB01.
           02 GAR-TAB PIC X(8) OCCURS 9 TIMES.
       01  LAST-CHARGE01.
           02 LAST-CHARGE PIC X(11) OCCURS 9 TIMES.
       01  FLAGX PIC 9.
       01  LLTAB2401.
           02 LLTAB24 PIC X OCCURS 24 TIMES.
       01  FFTAB1001.
           02 FFTAB10 PIC X OCCURS 10 TIMES.
       01  FF PIC 99.
       01  LL PIC 99.
       01  XLLTAB2401.
           02 XLLTAB24 PIC X OCCURS 24 TIMES.
       01  XFFTAB1001.
           02 XFFTAB10 PIC X OCCURS 10 TIMES.
       01  PD PIC 9 VALUE 1.
       01  DC PIC 9 VALUE 0.
       01  DD PIC 9 VALUE 1.
       01  HP PIC 9 VALUE 0.
       01  ALF-19 PIC X(19).
       01  ALF-15 PIC X(15).
       01  BELL0 USAGE INDEX.
       01  LAST-INDX USAGE INDEX.
       01  LAST-ADD-KEY USAGE INDEX.
       01  BUFFER-PACK01 PIC X(25).
       01  X-AMOUNT PIC S9(4)V99 VALUE 0.
       01  RATE01.
           02 RATE-1 PIC 99.
           02 RATE-2 PIC 99.
           02 RATE-3 PIC 99.
           02 RATE-4 PIC X(18).
       01 RATETABLES.
          02 RATETAB02 OCCURS 15 TIMES.
             03 RATE-PC PIC 99. 
             03 RATE-AMT PIC 99.
             03 RATE-NAME PIC X(18).
       01  GARPAT1 PIC 9 VALUE 0.
       01  CO-PAY-FLAG PIC 9.
       01  NDC-TAB01.
           02 NDC-TAB02 OCCURS 9 TIMES.
            03 NDC-TAB PIC X(7).
            03 NDC-NUM PIC X(11).
            03 NDC-NAME PIC X(17).
       01  NDC-PROC PIC X(7).
       01  NDC-CNTR PIC 9.
       01  XAUTH-NDC PIC X(11).
       01     PAYFLAG   PIC XXX.
       01  kmc-flag pic 9.
       01  TAGTAB01.
           02 TAGTAB PIC X(7) OCCURS 20 TIMES.
       LINKAGE SECTION.
       01  CHAR1 PIC 9.
       PROCEDURE DIVISION USING CHAR1.
       0005-START.
           SET BELL0 TO 7
           OPEN INPUT TAGDIAG.
           OPEN I-O PARMNDEX.
           OPEN I-O CHARFILE CLAIMFILE AUTHFILE.
           OPEN INPUT INSFILE PAYFILE.
           OPEN INPUT GARFILE.
           OPEN INPUT PATFILE.
           OPEN INPUT PROCFILE.
           OPEN INPUT REFPHY.
           OPEN INPUT DOCPARM.
           OPEN INPUT DIAGFILE.
           OPEN INPUT MPLRFILE.
           OPEN INPUT GAPFILE.
           OPEN OUTPUT FILEOUT.
           IF CHAR1 = 1 GO TO 1000-ACTION.
           MOVE 0 TO CO-PAY-FLAG
           MOVE 1 TO CHAR1
           MOVE SPACE TO CD-FUTURE
           MOVE "01" TO DOCP
           MOVE 0 TO STAT PAPER CD-COLLT CD-AUTH
           MOVE "2" TO CD-EPSDT
           MOVE 0 TO AMOUNT.
           MOVE "00000000" TO CD-DATE-A CD-DATE-M
           MOVE ALL ZEROES TO DF-TAB01.
           ACCEPT T-DATE FROM DATE YYYYMMDD.
           READ DOCPARM AT END GO TO 9100-CLOSE-MASTER-FILE.
           MOVE DP1 TO HIGH-DOC.
           IF HIGH-DOC = "01" MOVE 1 TO DF-TAB(9)
           MOVE "01" TO DOCP.
           MOVE 0 TO PLINDX.
           move 0 to kmc-flag
           MOVE HIGH-DOC TO NUM-2
           PERFORM DOCRATE-1 NUM-2 TIMES.
           GO TO P00.
       DOCRATE-1. 
           READ DOCPARM AT END GO TO P00.
           MOVE DOCPARM01 TO RATE01
           MOVE RATE-2 TO RATE-PC(RATE-1)
           COMPUTE RATE-AMT(RATE-1) = RATE-3
           MOVE RATE-4 TO RATE-NAME(RATE-1).
      *     DISPLAY RATE-PC(RATE-1) " " RATE-AMT(RATE-1) " "
      *     RATE-NAME(RATE-1).
           
       P00. READ DOCPARM AT END GO TO P0.
           if dp-1-1 = "X"
             and dp-1-2 = "X"
             and dp-2  = "X"
             move 1 to kmc-flag
             MOVE 1 TO HP
             go to P00
           end-if
           MOVE DP-1-1 TO HIGH-PLACE
           ADD 1 TO PLINDX
           MOVE DP-1-1 TO PL-TAB(PLINDX)
           MOVE DP-1-2 TO PL-NUM(PLINDX)
           MOVE DP-2 TO PL-NAME(PLINDX)
           GO TO P00.
       P0.
           MOVE ADD-CON TO MULT-ADD.
           MOVE 23 TO MULT.
           MOVE CHARFILE01 TO QQQ.
           MOVE "DF2" TO ACTION.
           MOVE SPACE TO PM-KEY8
           MOVE SPACE TO PM-KEY3
           MOVE "DF2" TO PM-KEY8
           START PARMNDEX KEY NOT <  PARM-KEY INVALID 
           DISPLAY "NO DEFAULTED FIELDS" GO TO 1000-ACTION.
       PM1. READ PARMNDEX NEXT AT END PERFORM LDF2 GO TO 1000-ACTION.
            IF PM-KEY8 NOT = "DF2" PERFORM LDF2 GO TO 1000-ACTION.
            MOVE PM-KEY3 TO ALF-3
            MOVE ALF-3 TO NUM-3
            SET INDX TO NUM-3
            MOVE PM-DATA TO IN-FIELD
            GO TO 2061-GO-TO.
       1000-ACTION.
           MOVE 23 TO MULT.
           MOVE MULT-ADD TO ADD-CON.
           MOVE SPACES TO ACTION.
           DISPLAY "OPTION,ID  DAILY CHRGS".
           ACCEPT DATAIN.
           IF DATAIN = "END" GO TO 9100-CLOSE-MASTER-FILE.
           IF DATAIN = "?"
           DISPLAY " GP = GUARANTOR-DEPENDENT ROUTINES"
           DISPLAY "CHARGE ROUTINES A,C,D,L,F <OPT>,<KEY> FORMAT"
           DISPLAY "GARNO ROUTINES FA,FG,SG,LG <OPT>,<GARNO> FORMAT"
           DISPLAY "DEPENDENT ROUTINES FP,SP,LP <OPT>,<PATNO> FORMAT"
           DISPLAY "DF,<FIELD CODE>,<VALUE> TO DEFAULT A FIELD"
      *    IF DATAIN = "?"
           DISPLAY "LD OR LD,? TO LIST THE DEFAULTED VALUES"
           DISPLAY "LI,<INS. CODE> TO LIST AN INSURANCE RECORD"
           DISPLAY "CD = LIST CHARGE, DD = LIST DEFAULT : ON EACH ADD"
           DISPLAY "RP,<FIELD CODE OR ALL> TO RE-PROMPT  <FIELD CODE>"
           DISPLAY "/ = USE PREVIOUS VALUE FOR THIS PROMPT ON ADD"
           DISPLAY "PH = PHYSICIAN NAME DISPLAY CP = CO-PAY PROMPT"
           DISPLAY "SDV = SAVE DEFAULTED VALUES AFTER EXIT"
           DISPLAY "END = END THE JOB"
            if kmc-flag = 1
              display "PCF, acct  = PRINT CLAIM FORM FOR THE ACCT"
            end-if
           GO TO 1000-ACTION.
           IF DATAIN = "GP"
           CLOSE GARFILE PATFILE MPLRFILE INSFILE GAPFILE CHARFILE
           CLAIMFILE PAYFILE
           CALL "tri001" USING GARPAT1
           MOVE 1 TO GARPAT1
           OPEN INPUT GARFILE PATFILE MPLRFILE INSFILE GAPFILE PAYFILE
           OPEN I-O CLAIMFILE CHARFILE
           GO TO 1000-ACTION.
      *    IF DATAIN = "GP" CALL "GAR-PAT" USING GARPAT1
      *    MOVE 1 TO GARPAT1
      *    GO TO 1000-ACTION.
           MOVE SPACE TO ALF-3 BUFFER-PACK01.
           UNSTRING DATAIN DELIMITED BY "=" INTO BUFFER-PACK01 ALF-3.
           IF ALF-3 NOT = SPACE MOVE BUFFER-PACK01 TO DATAIN.
           MOVE SPACE TO RIGHT-3.
           UNSTRING ALF-3 DELIMITED BY " " INTO RIGHT-3.
           INSPECT RIGHT-3 REPLACING ALL " " BY "0".
           IF RIGHT-3 NOT NUMERIC DISPLAY "PAYCODE NOT NUMERIC"
           GO TO 1000-ACTION.
           MOVE RIGHT-3 TO ALF-3.
           UNSTRING DATAIN DELIMITED BY "," INTO ACTION CHAR-KEY.
           IF ACTION = "A" GO TO 1200-ADD-PROCESS.
           IF ACTION = "F"
            MOVE SPACE TO PAYFLAG
            IF CHAR-KEY (1:3) NUMERIC
             MOVE CHAR-KEY(1:3) TO PAYFLAG
             MOVE SPACE TO CHAR-KEY
            END-IF
           MOVE CHARFILE01 TO QQQ
           PERFORM 1200-FIND THRU  1200-FIND-EXIT
           MOVE  QQQ TO CHARFILE01
           GO TO 1000-ACTION.
           IF ACTION = "SG" OR "FG" GO TO 2400-FIND.
           IF ACTION = "SP" OR "FP" GO TO 3600-FIND.
           IF ACTION = "DF" OR "RP" GO TO DF1.
           IF ACTION = "LP" GO TO 10-LP.
           IF ACTION = "LI" MOVE CHAR-KEY TO INS-KEY
           PERFORM LI-1 THRU LI-1-EXIT GO TO 1000-ACTION.
           IF ACTION = "CD" PERFORM CD-1 GO TO 1000-ACTION.
           IF ACTION = "DD" PERFORM DD-1 GO TO 1000-ACTION.
           IF ACTION = "PH" PERFORM PH-1 GO TO 1000-ACTION.
           IF ACTION = "CP" PERFORM CP-1 GO TO 1000-ACTION.
           IF ACTION = "LG" PERFORM 10-LG THRU 10-LG-EXIT
           GO TO 1000-ACTION.
           IF ACTION = "FA" PERFORM FA-1 THRU FA-1-EXIT
           GO TO 1000-ACTION.
           IF ACTION = "SDV" GO TO SDV1.
           IF ACTION NOT = "LD" GO TO AC-1.
           IF CHAR-KEY = "?"
           DISPLAY "4=TYPE 5=DIAG 6=PROC 8=REF PHYS 9=DOC 11=REC-STAT"
           DISPLAY "12=UNITS 13=ACC. DATE  15=PLACE 18=TRANS DATE"
           DISPLAY "22=MOD2 23=SORCREF  24=DX2 25=DX3 26=CLM-DATE"
           DISPLAY "27=COLLT 28=ACC-TYPE 29=ADMIT-DT 30=MOD3 31=XXXX"
           DISPLAY "34=DX4"
           MOVE SPACE TO CHAR-KEY
           GO TO 1000-ACTION.
           PERFORM LDF2 GO TO 1000-ACTION.
       LDF2.
           IF DF-TAB(4) = 1 DISPLAY "(4)TYPE= " SERVICE.
           IF DF-TAB(5) = 1 DISPLAY "(5)DIAG= " DIAG.
           IF DF-TAB(24) = 1 DISPLAY "(24)DX2 = " CD-DX2.
           IF DF-TAB(25) = 1 DISPLAY "(25)DX3 = " CD-DX3.
           IF DF-TAB(34) = 1 DISPLAY "(34)DX4 = " CD-DX4.
      *     IF DF-TAB(35) = 1 DISPLAY "(35)DX5 = " CD-DX5.
      *     IF DF-TAB(36) = 1 DISPLAY "(36)DX6 = " CD-DX6.
           IF DF-TAB(6) = 1 DISPLAY "(6)PROC= " PROC.
           IF DF-TAB(8) = 1 DISPLAY "(8)REF PHY= " DOCR.
           IF DF-TAB(9) = 1 DISPLAY "(9)DR= " DOCP.
           IF DF-TAB(11) = 1 DISPLAY "(11)REC-STAT= " STAT.
           IF DF-TAB(12) = 1 DISPLAY "(12)UNITS= " WORK.
           IF DF-TAB(13) = 1 MOVE DAT1 TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           DISPLAY "(13)ACC DATE= " DISPLAY-DATE.
           IF DF-TAB(15) = 1 DISPLAY "(15)PLACE= " PLACE.
           IF DF-TAB(18) = 1 MOVE DATE-T TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           DISPLAY "(18)TRANS DATE= " DISPLAY-DATE.
           IF DF-TAB(22) = "1" DISPLAY "(22)MOD2= " MOD2.
           IF DF-TAB(23) = 1 DISPLAY "(23)SORCREF= " CD-SORCREF.
           IF DF-TAB(26) = 1 MOVE CD-DATE-A TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           DISPLAY "(26)CLM-DATE= " DISPLAY-DATE.
           IF DF-TAB(27) = 1 DISPLAY "(27)COLLT= " CD-COLLT.
           IF DF-TAB(28) = 1 DISPLAY "(28)ACC-TYPE= " CD-ACC-TYPE.
           IF DF-TAB(29) = 1 MOVE CD-DATE-M TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           DISPLAY "(29)ADMIT-DT = " DISPLAY-DATE.
           IF DF-TAB(30) = 1 DISPLAY "(30)MOD3= " MOD3.
       DF1. MOVE SPACE TO ABC ALF-2 IN-FIELD.
           IF ACTION = "DF"
           UNSTRING DATAIN DELIMITED BY "," INTO ABC ALF-2 IN-FIELD
           ELSE UNSTRING DATAIN DELIMITED BY "," INTO ABC ALF-2.
           IF ACTION = "RP" AND ALF-2 = "AL" MOVE ALL ZEROES
           TO DF-TAB01 DISPLAY "NO DEFAULTED VALUES" GO TO 1000-ACTION.
           MOVE SPACE TO RIGHT-2
           UNSTRING ALF-2 DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING LEADING " " BY "0".
           IF ( RIGHT-2 NOT NUMERIC ) OR ( RIGHT-2 < "04" )
           OR ( RIGHT-2 = "07" OR "10" OR "14" OR "16" OR "17" )
           OR (RIGHT-2 = "19" OR "20" OR "31" OR "32" OR "33")
           DISPLAY "INVALID DEFAULT FIELD"
           GO TO 1000-ACTION.
           MOVE RIGHT-2 TO NUM-2.
           IF ABC = "RP" MOVE 0 TO DF-TAB(NUM-2) GO TO 1000-ACTION.
           SET INDX TO NUM-2.
           GO TO 2061-GO-TO.
       AC-1.
           IF (ACTION = "C" OR "D" OR "L")
           OR (kmc-flag = 1 AND ACTION = "PCF") NEXT SENTENCE
           ELSE DISPLAY "WHAT ?" GO TO 1000-ACTION.
           IF CHAR-KEY = "1" OR "2" OR "3" OR "4" OR "5" OR "6"
           OR "7" OR "8" OR "9" MOVE CHAR-KEY TO IN-FIELD-1
           MOVE IN-FIELD-1 TO FLAG
           MOVE LAST-CHARGE(FLAG) TO CHAR-KEY.
           MOVE CHARFILE01 TO QQQ
           READ CHARFILE INVALID DISPLAY CHAR-KEY " NOT ON FILE"
             GO TO 1000-ACTION.
           IF ACTION = "PCF"
           DISPLAY NAME " " DATE-T(5:2) "-" DATE-T(7:2) "-" DATE-T(1:2)
           GO TO KMC-1.
       1150-CONFIRM-IDN.
           DISPLAY CHAR-KEY " " NAME.
           IF ACTION = "L"
           PERFORM LR1 MOVE QQQ TO CHARFILE01 GO TO 1000-ACTION.
           READ CHARFILE WITH LOCK INVALID DISPLAY "NOT ON FILE"
           GO TO 1000-ACTION.
           IF CHARFILE-STAT NOT = "00"
           DISPLAY "STATUS = " CHARFILE-STAT
           DISPLAY "RECORD LOCKED"
           GO TO 1000-ACTION.
           IF ACTION = "D" PERFORM 1300DEL1 THRU 1300DEL1-EXIT
           MOVE QQQ TO CHARFILE01 GO TO 1000-ACTION.
      *    IF ACTION = "C"
           GO TO 1400-CHANGE-PROCESS.
      *    DISPLAY "INVALID OPTION" GO TO 1000-ACTION.
       1200-ADD-PROCESS.
           MOVE "1" TO RESULT 
           MOVE "4" TO ACT
           MOVE "2" TO CD-EPSDT 
           MOVE "0" TO CD-AUTH
           MOVE SPACE TO MOD2 MOD3 MOD4.
       M1. PERFORM 1205-ADD-LOOP VARYING ADD-KEY FROM 1 BY 1
             UNTIL ADD-KEY > MULT.
           IF IN-FIELD = "X" DISPLAY "NO UPDATE" GO TO 1000-ACTION.
           ACCEPT CD-ORDER FROM TIME.
           ACCEPT DATE-E FROM DATE YYYYMMDD.
           IF PROC1 = "09999" MOVE "001" TO PAYCODE.
           IF PROC-NDC = "1" MOVE "1" TO CD-AUTH.
           MOVE CHARFILE01 TO HOLD-MASTER.
           MOVE 0 TO XYZ.
       1210-ADD-PROCESS.
           ADD 1 TO XYZ.
           MOVE XYZ TO ABC.
           STRING SAVE-GARNO ABC DELIMITED BY "@" INTO CHAR-KEY.
           READ CHARFILE INVALID KEY MOVE CHAR-KEY TO HOLD-ID
           GO TO 1220-ADD-PROCESS.
           IF XYZ = 999 DISPLAY "THERE ARE 999 CHARGE TRANSACTIONS"
           DISPLAY "FOR THIS GUARANTOR ON FILE. THIS IS THE LIMIT!"
           DISPLAY "THE CHARGE FILE MUST BE POSTED BEFORE ANY MORE"
           DISPLAY "CHARGES CAN BE PROCESSED"
           GO TO 1000-ACTION.
           GO TO 1210-ADD-PROCESS.
       1220-ADD-PROCESS.
           MOVE HOLD-MASTER TO CHARFILE01.
           MOVE HOLD-ID TO CHAR-KEY.
           MOVE "A" TO CLAIM-KEY.
           READ CLAIMFILE WITH LOCK INVALID KEY DISPLAY "STOP"
           DISPLAY "THIS IS VERY BAD!. CALL C.M.S. IMMEDIATELY"
           DISPLAY "ALL PROCESSING SHOULD TERMINATE UNTIL THIS PROBLEM"
           DISPLAY "IS RESOLVED. THIS PROGRAM IS TERMINATED"
           GO TO 9100-CLOSE-MASTER-FILE.
           IF CLAIMFILE-STAT NOT = "00" 
           DISPLAY "STATUS = " CLAIMFILE-STAT
           DISPLAY "FINDING CLAIM NUMBER"
           GO TO 1220-ADD-PROCESS.
           IF CLAIMNO < 100000
           DISPLAY "THE CLAIM FILE NEEDS FIXING. THIS LAST CHARGE"
           DISPLAY "HAS NOT BEEN ADDED. CALL CMS ABOUT THIS PROBLEM"
           DISPLAY "NO CHARGES CAN BE ADDED UNTIL IT IS FIXED!"
           GO TO 9100-CLOSE-MASTER-FILE.
           MOVE CLAIMNO TO CLAIM.
           ADD 1 TO CLAIMNO.
           REWRITE CLAIM01.
           UNLOCK CLAIMFILE RECORD
           IF INS-REFWARN   = "1"
           MOVE "1 " TO IN-FIELD-2
           PERFORM AUTH-1 THRU AUTH-1-EXIT.
           MOVE DOCP TO NUM-2
           MOVE AMOUNT TO X-AMOUNT
           IF (RATE-PC(NUM-2) NOT = 0)
           AND ( AMOUNT > RATE-AMT(NUM-2))
           AND (PLACE = "1")
           AND ((PROC1 > "992  " AND < "99299")
               OR (PROC1 > "993  " AND < "99399"))
           COMPUTE AMOUNT = (1 - RATE-PC(NUM-2) / 100) * AMOUNT
           MOVE AMOUNT TO NEF-8
           DISPLAY "ADJUSTED FEE " NEF-8.
           IF (WORK > "01") and (kmc-flag = 1)
           MOVE WORK TO NUM2
           COMPUTE AMOUNT = NUM2 * AMOUNT
           MOVE AMOUNT TO NEF-8
           DISPLAY "UNIT MULTIPLIED FEE " NEF-8
           END-IF
           PERFORM DX-1 THRU DX-1-EXIT
           PERFORM DXMOD-1
           move zeroes to cd-dx5 cd-dx6
           WRITE CHARFILE01 INVALID KEY DISPLAY "NO UPDATE"
           DISPLAY "THIS PROGRAM HAS BEEN TERMINATED"
           GO TO 9100-CLOSE-MASTER-FILE.
           IF AMOUNT NOT = X-AMOUNT
           MOVE X-AMOUNT TO AMOUNT.
           IF DC = 1 PERFORM LR1.
           DISPLAY CHAR-KEY " " CLAIM " RECORD IS ADDED".
           MOVE 0 TO FLAGX
           IF (PROC-NDC  = "1")
      *     AND
      *       ((PAYCODE = "004" OR "064" OR "141")
      *       OR ( G-SEINS = "004" OR "064" OR "141"))

             PERFORM NDC-1 THRU NDC-1-EXIT
             IF PROC-NDC = "1"
               IF XAUTH-NDC = SPACE
                DISPLAY "NO NDC WAS FOUND FOR THIS CPT CODE"
                DISPLAY "IN THE FEE SCHEDULE."
                DISPLAY "YOU WILL HAVE TO ENTER IS MANUALLY"
                DISPLAY "FOR THIS RECORD USING THE CHANGE MODE"
                DISPLAY "ON FIELD 21."
                ACCEPT ALF-1
               END-IF
              MOVE " " TO PROC-NDC
              PERFORM ADD-AUTH THRU ADD-AUTH-EXIT
      *        MOVE CHARFILE01 TO HOLD-MASTER
      *        GO TO 1210-ADD-PROCESS
             END-IF
           END-IF
           IF (PAYCODE = "003" OR "028" OR "141") 
               AND (CD-DATE-M = "00000000") 
            AND (PROC1 = "11719" OR "11055" OR "11056" OR "11057" 
                OR "G0127")
            MOVE 1 TO FLAGX
            DISPLAY "ENTER DATE LAST SEEN FIELD 29" BELL0
            DISPLAY "AND REFERRING PHYSICIAN IN FIELD 8" BELL0
           END-IF.
           IF (PAYCODE = "003" OR "028" OR "141")
            AND (CD-DATE-M = "00000000") 
            AND (PLACE NOT NUMERIC)
            MOVE 0 TO FLAG
            PERFORM INPAT-1 THRU INPAT-1-EXIT VARYING X
                             FROM 1 BY 1 UNTIL X > PLINDX
             IF FLAG = 1
              DISPLAY "IN-PATIENT: HOSP ADMIT DATE  FIELD 29" BELL0
              MOVE 1 TO FLAGX
             END-IF
           END-IF.
            IF (DOCR = "000") 
             AND  ((PROC > "99240  " AND < "99281  ")
                OR (PROC = "76942  " OR "76872  "))

             DISPLAY "REF. PHYS. REQUIRED IN FIELD 8" BELL0
             MOVE 1 TO FLAGX
             END-IF.
           IF FLAGX = 1
              MOVE "CC" TO ACTION MOVE CHARFILE01 TO QQQ
              MOVE HOLD-ID TO CHAR-KEY GO TO CC-1
           END-IF.
           IF DD = 1 PERFORM LDF2.
       M2. DISPLAY "MORE CHARGES ? " G-GARNAME.
           ACCEPT ANS.
           IF ANS = "?"
           DISPLAY "1=PROC                  2=DATE "
           DISPLAY "3=DATE PROC             4=PROC DIAGS"  
           DISPLAY "5=DATE PROC DIAGS        6=PLACE PROC DIAGS"  
           DISPLAY "7=DATE PLACE PROC DIAGS"
           DISPLAY "8=DATE PROC DIAGS ADMIT/LAST SEEN FIELD #29"
           DISPLAY "Y= ALL PROMPTS  <CR> = NO"
           DISPLAY "CC=CHANGE RECORD JUST ADDED BEFORE CONTINUING"
           DISPLAY "L= LIST THE RECORD JUST ADDED"
           GO TO M2.
           IF ANS = SPACE OR "0" GO TO M3.
           IF ANS = "L" PERFORM LR1 GO TO M2.
           IF ANS = "CC   " MOVE "CC" TO ACTION MOVE CHARFILE01 TO QQQ
           MOVE HOLD-ID TO CHAR-KEY GO TO CC-1.
           IF ANS = "1" OR "2" OR "3" OR "4" OR "5" OR "6" OR "7"
           OR "8" OR "Y" NEXT SENTENCE ELSE DISPLAY "INVALID" GO TO M2.
           IF ANS = "1" MOVE "06" TO ADD-CON MOVE 1 TO MULT.
           IF ANS = "2" MOVE "18" TO ADD-CON MOVE 1 TO MULT.
           IF ANS = "3" MOVE "1806" TO ADD-CON MOVE 2 TO MULT.
           IF ANS = "4" MOVE "0605242534" TO ADD-CON MOVE 5 TO MULT.
           IF ANS = "5"
                 MOVE "180605242534" TO ADD-CON MOVE 6 TO MULT.
           IF ANS = "6"
                 MOVE "150605242534" TO ADD-CON MOVE 6 TO MULT.
           IF ANS = "7"
                 MOVE "18150605242534" TO ADD-CON MOVE 7 TO MULT.
           IF ANS = "8"
                 MOVE "18060524253429" TO ADD-CON MOVE 7 TO MULT.
           IF ANS = "Y" MOVE MULT-ADD TO ADD-CON      MOVE 23 TO MULT.              
           GO TO M1.
       M3.
           if kmc-flag = 1  go to kmc-1.

           IF G-COPAY = 0 OR CO-PAY-FLAG = 1 GO TO 1000-ACTION.
           MOVE G-COPAY TO NEF-7.
       M4.
           DISPLAY "CO-PAY " NEF-7.
           DISPLAY "Y = YES, PAID THE COPAY FOR CHARGES JUST ENTERED" 
           DISPLAY " ANYTHING ELSE =  NO COPAY ".
           ACCEPT ANS
           IF ANS = "?" GO TO M4.
           IF ANS NOT = "Y" GO TO 1000-ACTION.
           DISPLAY "1=CASH  21=CHECK  22=CREDIT CARD"
           ACCEPT PAY-TYPE
           IF NOT (PAY-TYPE = "1" OR "21" OR "22") GO TO M4.
           MOVE CHARFILE01 TO QQQ
           MOVE DATE-T TO HOLD-DATE-T
           MOVE CD-KEY8 TO HOLD8
           MOVE PATID TO HOLD-PATID
           MOVE 0 TO BIG-AMOUNT
           MOVE "000000" TO HOLD-CLAIM
           PERFORM M5 THRU M10
           MOVE QQQ TO CHARFILE01
           GO TO 1000-ACTION.
       M5. MOVE HOLD8 TO CD-KEY8
           MOVE SPACE TO CD-KEY3
           START CHARFILE KEY NOT < CHAR-KEY INVALID
           DISPLAY "NO RECORDS FOUND" GO TO M10.
       M6. READ CHARFILE NEXT AT END GO TO M7.
           IF CD-KEY8 NOT = HOLD8 GO TO M7.
           IF PLACE NOT = "1" GO TO M6.
           IF SERVICE NOT = "6" GO TO M6.
           IF PATID NOT = HOLD-PATID GO TO M6.
           IF DATE-T = HOLD-DATE-T
           AND AMOUNT > BIG-AMOUNT
           MOVE AMOUNT TO BIG-AMOUNT 
           MOVE CLAIM TO HOLD-CLAIM.
           GO TO M6.
       M7. IF HOLD-CLAIM = "000000" OR BIG-AMOUNT = 0    
           OR BIG-AMOUNT < G-COPAY 
           DISPLAY "NO VALID CHARGE FOUND TO APPLY COPAY TO"
           GO TO M10.
           close payfile
           OPEN I-O PAYFILE     
           MOVE HOLD8 TO PD-KEY8
           MOVE SPACE TO PD-KEY3
           MOVE HOLD-CLAIM TO PD-CLAIM
           IF PAY-TYPE = "1"
           MOVE "001" TO PD-PAYCODE.
           IF PAY-TYPE = "21"
           MOVE "021" TO PD-PAYCODE.
           IF PAY-TYPE = "22"
           MOVE "022" TO PD-PAYCODE.
           MOVE "CP" TO PD-DENIAL
           MOVE HOLD-DATE-T TO PD-DATE-T
           ACCEPT PD-ORDER FROM TIME
           ACCEPT PD-DATE-E FROM DATE YYYYMMDD
           MOVE SPACE TO PD-BATCH
           COMPUTE PD-AMOUNT =  -1 * G-COPAY 
           MOVE G-GARNAME TO PD-NAME
           MOVE PAYFILE01 TO SAVEPAY
           MOVE 0 TO XYZ
           MOVE HOLD8 TO PD-KEY8.
       M8. ADD 1 TO XYZ
           MOVE XYZ TO PD-KEY3
           READ PAYFILE INVALID GO TO M9.
           IF PD-DENIAL = "CP" 
           AND PD-DATE-T = HOLD-DATE-T
           AND PD-CLAIM = HOLD-CLAIM
           DISPLAY "THERE IS A COPAY RECORD ALREADY POSTED"
           DISPLAY "FOR THIS DATE AND CHARGE" 
           GO TO M9-1.
           ADD 1 TO XYZ
           IF XYZ > 990 DISPLAY "PROBLEM ENCOUNTERED"
           DISPLAY "NO COPAY ADDED" 
           GO TO M9-1.
           GO TO M8.
       M9. MOVE SAVEPAY TO PAYFILE01
           MOVE XYZ TO PD-KEY3
           WRITE PAYFILE01
           DISPLAY "COPAY PAYMENT ADDED".
       M9-1.    
           CLOSE PAYFILE.
           open input payfile.
           
       M10. EXIT.
       kmc-1.
           DISPLAY "PRINT CLAIM FORMS? Y = YES"
           ACCEPT ALF-1
           IF ALF-1 NOT = "Y" GO TO 1000-ACTION.
           MOVE 0 TO KMC-CNTR
           move date-t to save-date-t
           move cd-key8 to save-key8
           move space to cd-key3
           start charfile key not < char-key invalid
           display "no charges yet" go to 1000-action.
       kmc-2.
           read charfile next at end go to kmc-3.
           if cd-key8 not = save-key8 go to kmc-3.
           if date-t not = save-date-t go to kmc-2.
           STRING PAYCODE CD-KEY8 CHAR-KEY DATE-T CD-ASSIGN
           PLACE DOCP PAPER DELIMITED BY SIZE
           INTO FILEOUT01
           WRITE FILEOUT01
           ADD 1 TO KMC-CNTR
           go to kmc-2.
       kmc-3.
           if kmc-cntr = 0
            display "no charges yet" go to 1000-action.
           close charfile fileout
           call "SYSTEM" using "pap-4d"
           open i-o charfile
           open output fileout
           go to 1000-action.
       NDC-1.
           MOVE 0 TO NDC-CNTR.
           MOVE PROC(1:5) TO PROC-KEY
           START PROCFILE KEY NOT > PROC-KEY INVALID
           DISPLAY "AN NDC # NEEDS TO BE ADDED FOR"
           DISPLAY "THIS PROC. CODE IN THE FEE SCHEDULE"
           MOVE " " TO PROC-NDC
           GO TO NDC-1-EXIT.
       NDC-2.
           READ PROCFILE NEXT AT END GO TO NDC-3.
           IF PROC-KEY(1:5) = "J8499"
           MOVE 1 TO NDC-CNTR
           STRING "J99" PROC-KEY(6:2) DELIMITED BY SIZE INTO
           NDC-TAB(NDC-CNTR)
           MOVE PROC-TITLE(1:11) TO NDC-NUM(NDC-CNTR)
           MOVE PROC-TITLE(12:17) TO NDC-NAME(NDC-CNTR)
           GO TO NDC-3.

           IF PROC-KEY(1:5) NOT = PROC(1:5) GO TO NDC-3.
           IF PROC-KEY(6:1) NOT = "N" GO TO NDC-2.
           ADD 1 TO NDC-CNTR
           MOVE PROC-KEY TO NDC-TAB(NDC-CNTR)
           MOVE PROC-TITLE(1:11) TO NDC-NUM(NDC-CNTR)
           MOVE PROC-TITLE(12:17) TO NDC-NAME(NDC-CNTR)
           GO TO NDC-2.
       NDC-3.
           IF NDC-CNTR = 0
           DISPLAY "AN NDC # NEEDS TO BE ADDED FOR"
           DISPLAY "THIS PROC. CODE IN THE FEE SCHEDULE"
           MOVE " " TO PROC-NDC
           GO TO NDC-1-EXIT.
           IF NDC-CNTR = 1
            MOVE NDC-TAB(1) TO PROC
            MOVE "1" TO PROC-NDC
            MOVE NDC-NUM(1) TO XAUTH-NDC
            MOVE 0 TO AMOUNT
            GO TO NDC-1-EXIT
           END-IF.
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > NDC-CNTR
           DISPLAY X " " NDC-NUM(X) " " NDC-NAME(X)
           END-PERFORM.
       NDC-4.
           DISPLAY "SELECT 1,2,3 ETC., FROM THE LIST"
           DISPLAY "X = NO SELECTION MADE: NDC NEEDS ADDING."
           ACCEPT ALF-1
           IF ALF-1 = "X" DISPLAY "NO NDC NUMBER SELECTED"
            MOVE " " TO PROC-NDC
            GO TO NDC-1-EXIT
           END-IF
           IF (ALF-1 NOT NUMERIC) OR ALF-1 = "0"
            DISPLAY "BAD SELECT. USE 1 OR 2, ETC."
            GO TO NDC-4
           END-IF
           MOVE ALF-1 TO NUM1
           IF NUM1 > NDC-CNTR
            DISPLAY "BAD SELECT. TOO LARGE"
            GO TO NDC-4
           END-IF
           MOVE NDC-TAB(NUM1) TO PROC
           MOVE NDC-NUM(NUM1) TO XAUTH-NDC
           MOVE 0 TO AMOUNT
           MOVE "1" TO PROC-NDC.
       NDC-1-EXIT.
           EXIT.
       ADD-AUTH.
            MOVE CD-KEY8 TO AUTH-KEY8.
            MOVE CLAIM TO AUTH-KEY6.
            READ AUTHFILE WITH LOCK INVALID
             MOVE XAUTH-NDC TO AUTH-NDC
             MOVE SPACE TO AUTH-NUM  AUTH-QNTY AUTH-FILLER
             ACCEPT AUTH-DATE-E FROM DATE YYYYMMDD
             WRITE AUTHFILE01
             GO TO ADD-AUTH-EXIT
            END-READ
            MOVE XAUTH-NDC TO AUTH-NDC
            REWRITE AUTHFILE01.
       ADD-AUTH-EXIT.
            EXIT.
       1205-ADD-LOOP.
           SET INDX TO ADD-FLD(ADD-KEY).
           PERFORM 2050-DISPLAY THRU 4910DEE.
       1200-FIND. START CHARFILE KEY > CHAR-KEY INVALID
           DISPLAY " END OF FILE" GO TO 1200-FIND-EXIT.
       1200-FIND-20.
           MOVE 0 TO X.
           MOVE 0 TO FLAG.
           PERFORM 1200-SEARCH THRU 1200-SEARCH-EXIT.
           IF FLAG = 1 DISPLAY "END OF FILE FOUND"
           DISPLAY "END OF SEARCH"
           GO TO 1200-FIND-EXIT.
       1200-FIND-QUES.
           DISPLAY "?".
           ACCEPT ANS.
           IF ANS = SPACES GO TO 1200-FIND-20.
           IF ANS = "?"
           DISPLAY "A <CR> WILL PRODUCE 10 MORE NAMES"
           DISPLAY "ANYTHING ELSE WILL RETURN YOU TO THE OPTION COMMAND"
           GO TO 1200-FIND-QUES.
           GO TO 1200-FIND-EXIT.

       1200-SEARCH.
           READ CHARFILE NEXT AT END MOVE 1 TO FLAG
           GO TO 1200-SEARCH-EXIT.
           MOVE SPACE TO ALF-1
           IF PAPER NOT NUMERIC MOVE PAPER TO ALF-1.
           MOVE AMOUNT TO NEF-5
           MOVE DATE-T TO TEST-DATE-S
           MOVE CORR TEST-DATE-S TO DISP-DATE
           IF (PAYFLAG = SPACE) OR PAYFLAG = PAYCODE
            ADD 1 TO X
            DISPLAY X " " CHAR-KEY " " DISP-DATE " " PROC MOD2 " "
            MOD3 " " NEF-5 " " PAYCODE " " DIAG " " ALF-1 " " NAME
            MOVE CHAR-KEY TO LAST-CHARGE(X)
           END-IF
           IF X > 8 GO TO 1200-SEARCH-EXIT.
           GO TO 1200-SEARCH.

       1200-SEARCH-EXIT.
           EXIT.
       1200-FIND-EXIT. EXIT.
       10-LG.
           IF CHAR-KEY = "1" OR "2" OR "3" OR "4" OR "5" OR "6"
           OR "7" OR "8" OR "9" MOVE CHAR-KEY TO IN-FIELD-1
           MOVE IN-FIELD-1 TO FLAG MOVE GAR-TAB(FLAG) TO CHAR-KEY.
           MOVE CHAR-KEY TO G-GARNO
           READ GARFILE INVALID DISPLAY "INVALID" GO TO 10-LG-EXIT.
           MOVE G-GARNO TO MPLR-KEY
           MOVE "1" TO G-TRINSIND
           READ MPLRFILE INVALID MOVE "0" TO G-TRINSIND.
       10-LG-1-0.
           MOVE G-DOB TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           INSPECT T-MM OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           INSPECT T-DD OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           DISPLAY  G-GARNO  " " DISPLAY-DATE " " G-GARNAME
           DISPLAY G-BILLADD
           DISPLAY G-STREET
           DISPLAY G-CITY " " G-STATE " " G-ZIP
           DISPLAY G-PHONE1 "-" G-PHONE2 "-" G-PHONE3
           DISPLAY G-PRINS " " G-PR-ASSIGN " " G-PRIPOL " " 
           G-PR-GROUP " " G-PR-RELATE " " G-PRNAME
           DISPLAY G-SEINS " " G-SE-ASSIGN " " G-SECPOL " " 
           G-SE-GROUP " "  G-SE-RELATE " " G-SENAME
           IF (G-TRINSIND = "1") AND (G-TRINS NOT = "001")
           DISPLAY G-TRINS " " MPLR-TR-ASSIGN " " MPLR-TRIPOL " " 
           MPLR-TR-GROUP " " MPLR-TR-RELATE " " MPLR-TR-NAME.
           MOVE G-PR-GROUP TO GAPKEY
           READ GAPFILE INVALID
           MOVE SPACE TO GAP-NAME GAP-ADDR GAP-CITY GAP-STATE GAP-ZIP
           GAP-TYPE.
           MOVE SPACE TO ALF-4
           IF GAP-TYPE = "X" MOVE "CRSS" TO ALF-4.
           IF G-SEINS = "062"
           DISPLAY ALF-4 " " GAP-NAME " " GAP-ADDR " " GAP-CITY " "
           GAP-STATE " " GAP-ZIP.
           IF G-TRINSIND = "1" AND MPLR-NAME NOT = SPACE
           DISPLAY "WORK COMP ADDRESS"
           DISPLAY MPLR-NAME " " MPLR-STREET " " MPLR-CITY " "
           MPLR-STATE " " MPLR-ZIP 
           DISPLAY "CLAIM # = " MPLR-CLAIMNO.
           MOVE G-LASTBILL TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           IF G-LASTBILL NOT = "00000000"
           INSPECT T-MM OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           INSPECT T-DD OF DISPLAY-DATE REPLACING LEADING "0" BY " ".
           DISPLAY "COLLT GENDER RELATE DUNNING ACCSTAT  LASTBILL CYCLE"
           DISPLAY "  " G-COLLT "     " G-SEX "       " G-RELATE
           "      " G-DUNNING "        " G-ACCTSTAT
           "   " DISPLAY-DATE "  " G-BILLCYCLE.
           IF G-COPAY NOT = 0 MOVE G-COPAY TO NEF-D
           DISPLAY " CO-PAY =" NEF-D.

       10-LG-EXIT. EXIT.
       10-LP. 
           IF CHAR-KEY = "1" OR "2" OR "3" OR "4" OR "5" OR "6"
           OR "7" OR "8" OR "9" MOVE CHAR-KEY TO IN-FIELD-1
           MOVE IN-FIELD-1 TO FLAG MOVE GAR-TAB(FLAG) TO CHAR-KEY.
           MOVE CHAR-KEY TO P-PATNO.
           READ PATFILE INVALID DISPLAY "INVALID" GO TO 1000-ACTION.
           MOVE P-DOB TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           INSPECT T-MM OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           INSPECT T-DD OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           MOVE P-GARNO TO G-GARNO.
           READ GARFILE INVALID MOVE "NO GARNO" TO G-GARNAME.
           DISPLAY P-PATNO " " P-PATNAME
           DISPLAY "SX=" P-SEX " MS=" P-MSTAT " RE=" P-RELATE
           " DOB " DISPLAY-DATE
           DISPLAY "GUAR " G-GARNO " " G-GARNAME
           GO TO 1000-ACTION.
       1300DEL1.
           MOVE 0 TO FLAG
           PERFORM DONT-DELETE thru dont-delete-exit
           IF FLAG = 1
           DISPLAY "THIS CHARGE HAS PAYMENTS!"
           GO TO 1000-action
           end-if.


           DISPLAY "DELETE  Y/N ?".
           DISPLAY "DIAG =" DIAG " PROC. = " PROC " DATE = " DATE-T.
           DISPLAY NAME.
           DISPLAY SPACES.
           ACCEPT ANS.
           IF ANS = "?"
           DISPLAY "Y = DELETE THE RECORD DISPLAYED"
           DISPLAY "N = NO DELETE PERFORMED; RETURN TO FUNCTION"
           GO TO 1300DEL1.
           IF ANS = "N" DISPLAY "NO DELETE" GO TO 1300DEL1-EXIT.
           IF ANS NOT = "Y" GO TO 1300DEL1.
           DELETE CHARFILE RECORD.
           CLOSE CHARFILE OPEN I-O CHARFILE.
           DISPLAY "RECORD DELETED".
       1300DEL1-EXIT. EXIT.
       dont-delete.
           move cd-key8 to pd-key8
           move space to pd-key3
           start payfile  key not < payfile-key
             invalid go to dont-delete-exit.
       dont1.
           read payfile next at end go to dont-delete-exit.
           if pd-key8 not = cd-key8 go to dont-delete-exit.
           if pd-claim not = claim go to dont1.
           move 1 to flag.
       dont-delete-exit.
           exit.

       CC-1.
             READ CHARFILE WITH LOCK INVALID DISPLAY "BAD"
              MOVE QQQ TO CHARFILE01
              GO TO M2.
             IF CHARFILE-STAT NOT = "00" 
              DISPLAY "STATUS = " CHARFILE-STAT
              DISPLAY "RECORD BEING CHANGED"
              MOVE QQQ TO CHARFILE01
             GO TO M2.
       1400-CHANGE-PROCESS.
           DISPLAY "FIELD CODE,DATA?".
           ACCEPT DATAIN.
           IF DATAIN = "X"
               DISPLAY "NO CHANGE" MOVE QQQ TO CHARFILE01
           GO TO 1000-ACTION.
           IF DATAIN = "L" PERFORM LR1 GO TO 1400-CHANGE-PROCESS.
           IF DATAIN = "UP"
              IF ((DATE-T > "20150930") AND
                  (DIAG(6:2) = "??"
                  OR CD-DX2(6:2) = "??"
                  OR CD-DX3(6:2) = "??"
                  OR CD-DX4(6:2) = "??"))
      *            OR CD-DX5(6:2) = "??"
      *            OR CD-DX6(6:2) = "??"))
               OR ((DATE-T < "20151001") AND
                     NOT ((DIAG(6:2) = "??" OR "00")
                 AND (CD-DX2(6:2) = "??" OR "00")
                 AND (CD-DX3(6:2) = "??" OR "00")
                 AND (CD-DX4(6:2) = "??" OR "00")))
      *           AND (CD-DX5(6:2) = "??" OR "00")
      *           AND (CD-DX6(6:2) = "??" OR "00")))
               DISPLAY "DATE CONFLICT WITH DIAGNOSES"
               DISPLAY "CHANGE THE DATE TO MATCH DIAGNOSES"
               DISPLAY "OR CHANGE ALL THE DIAGNOSES"
               GO TO 1400-CHANGE-PROCESS
              END-IF
            GO TO 5000-WRITE-CHARFILE
           END-IF
           IF DATAIN = "?"
           DISPLAY "ENTER THE FIELD # AND DATA FOR THAT FIELD"
           DISPLAY "TO BE CHANGED. VALID CODES ARE:"
           DISPLAY "  2=PATID    4=TYP SERV   5=DIAG      6=PROC" 
           DISPLAY "  7=AMOUNT   8=REF PHYS   9=PRVDR    10=INS CODE"
           DISPLAY "11=STATUS  12=UNITS     13=ACC DATE 14=PAPER CLM"
           DISPLAY "15=PLACE   17=ESPDTFPL  18=CHRGDATE 19=RESULT" 
           DISPLAY "20=ACTION  21=AUTH/NDC  22=MOD2     23=SORCREF" 
           DISPLAY "24=DX-2    25=DX-3      26=CLM-DATE 27=COLLT"
           DISPLAY "28=ACCTYP  29=ADMITDT   30=MOD3     32=ACCT-ASSIGN" 
           DISPLAY "33=CHGR-ASSIGN"
           DISPLAY "34=DX-4"
      *       35=DX-5  36=DX-6"
           DISPLAY "UP = UPDATE CHANGES MADE"
           DISPLAY "X = NO UPDATE"
           DISPLAY "L = LIST RECORD"
           GO TO 1400-CHANGE-PROCESS.
           MOVE SPACES TO RIGHT-2 IN-FIELD.
           UNSTRING DATAIN DELIMITED BY "," INTO RIGHT-2 IN-FIELD.
           INSPECT RIGHT-2 REPLACING LEADING SPACE BY "0".
           IF RIGHT-2 NOT NUMERIC
               DISPLAY "FIELD-CODE MUST BE NUMERIC"
               GO TO 1400-CHANGE-PROCESS.
           IF RIGHT-2 = "00" OR "01" OR "03" OR "16" OR "31"
           OR RIGHT-2 > "36" OR < "00" DISPLAY "INVALID FIELD-CODE"
               GO TO 1400-CHANGE-PROCESS.
           MOVE RIGHT-2 TO NUM-2
           SET INDX TO NUM-2
           GO TO 2060-GO-TO.
       2050-DISPLAY.
           IF ACTION NOT = "A" GO TO 2050-D.
           IF DF-TAB(INDX) = 1 GO TO 4900DEE.
       2050-D.
           IF HIGH-DOC = "01" AND INDX = 9 GO TO 4900DEE.
           IF (SERVICE NOT = 0) AND (INDX = 4) GO TO 4900DEE.
           IF (CD-ASSIGN NOT = "S") AND (INDX = 32) GO TO 4900DEE.
           IF (CD-NEIC-ASSIGN NOT = "S") AND (INDX = 33) GO TO 4900DEE.
           IF INDX = 4 DISPLAY "PROMPT ".
           DISPLAY DESC-FLD(INDX) "?".
       2051-INPUT.
           ACCEPT IN-FIELD.
           IF IN-FIELD = "BK" GO TO 2062-BACK.
           IF IN-FIELD = "LC" PERFORM LR1 GO TO 2050-DISPLAY.
           IF IN-FIELD = "X" SET ADD-KEY TO 98
             GO TO 4910DEE.
           GO TO 2060-GO-TO.
       2062-BACK.
           IF ADD-KEY < 2 MOVE "X" TO IN-FIELD
           SET ADD-KEY TO 98 GO TO 4910DEE.
           PERFORM 2062-BACK2
           IF DF-TAB(INDX) = 1 GO TO 2062-BACK.
           IF HIGH-DOC = "01" AND INDX = 9 GO TO 2062-BACK.
           IF INDX = 20 AND LAST-ADD-KEY = 20
           GO TO 2050-DISPLAY.
           IF INDX = 20 AND LAST-ADD-KEY = 30
           PERFORM 2062-BACK2 2 TIMES 
           GO TO 2050-DISPLAY.
           IF INDX = 20 AND LAST-ADD-KEY = 22
           PERFORM 2062-BACK2 3 TIMES
           GO TO 2050-DISPLAY.
           IF INDX = 20 AND LAST-ADD-KEY =06
           PERFORM 2062-BACK2 4 TIMES
           GO TO 2050-DISPLAY.
           IF INDX < 19 OR > 20 GO TO 2050-DISPLAY.
           IF (G-PRINS NOT = "004") AND (G-SEINS NOT = "004") AND
              (G-PRINS NOT = "064") AND (G-SEINS NOT = "064")
           GO TO 2062-BACK.
           IF NOT ( PROC1 = "99391" OR "99392" OR "99393" OR "99394"
           OR "99395" OR "99381" OR "99382" OR "99383" OR "99384"
           OR "99385")
           GO TO 2062-BACK.
           GO TO 2050-DISPLAY.
       2062-BACK2.
           SET ADD-KEY DOWN BY 1
           SET INDX TO ADD-FLD(ADD-KEY).
       2060-GO-TO.
           MOVE 0 TO FLAG.
           IF INDX = 3 GO TO 2061-GO-TO.
           MOVE LEN-TAB(INDX) TO Q ADD 1 TO Q.
           IF IN-FIELD-TAB(Q) NOT = " " MOVE "1" TO FLAG
           DISPLAY "DATA TOO LONG, MUST NOT BE GREATER "
             "THAN " LEN-TAB(INDX) ".".
           IF FLAG = 1 AND ACTION = "C" GO TO 1400-CHANGE-PROCESS.
           IF FLAG = 1 AND ACTION = "A" GO TO 2050-DISPLAY.
       2061-GO-TO.
           GO TO 2100-CHAR-KEY
           2180-PATID 2100-CLAIM  2130-SERVICE 2150-DIAG 2140-PROC
           2200-AMOUNT 2160-DOCR 2165-DOCP 2170-PAYCODE 2740-STAT
            2750-WORK 2190-DAT1 1450-PAPER 2180-PLACE 2120-NAME
           2450-EPSDT 4000-DATE-T 2195-RESULT 2196-ACTION 2-AUTH 2-MOD2
           2-SORCREF 2-DX2 2-DX3 2-DATE-A 2-COLLT 2-ACC-TYPE 2-ADMIT
           2-MOD3 2-XXXX 2-ASSIGN 2-NEIC-ASSIGN 2-DX4
      *      2-DX5 2-DX6
           DEPENDING ON INDX.
       2000TI.
           IF ACTION = "A" GO TO 2050-DISPLAY.
           IF ACTION = "C" GO TO 1400-CHANGE-PROCESS.
           IF ACTION = "CC" GO TO 1400-CHANGE-PROCESS.
           IF ACTION =  "DF" OR "FG" OR "SG" OR "FP" OR "SP" OR "S"
           OR "FA" GO TO 1000-ACTION.
           IF ACTION = "DF2" GO TO PM1.
       2100-CHAR-KEY.
           IF IN-FIELD = "?"
           DISPLAY "ENTER  A GUARANTOR ACCT #"
           GO TO 2000TI.
           MOVE IN-FIELD-8 TO G-GARNO.
           READ GARFILE INVALID KEY DISPLAY " NOT ON FILE"
           GO TO 2000TI.
           DISPLAY G-GARNAME.
       2120-CHAR-KEY.
           MOVE 0 TO X.
       2130-CHAR-KEY.
           ADD 1 TO X.
           MOVE SPACE TO ALF-11.
           MOVE X TO ABC.
           STRING IN-FIELD-8 ABC DELIMITED BY "@" INTO ALF-11.
           MOVE ALF-11 TO CHAR-KEY.
           READ CHARFILE INVALID KEY GO TO 4900DEE.
           ADD 1 TO X.
           IF X = 999 DISPLAY "THIS ACCOUNT HAS 999 TRANSACTIONS"
           DISPLAY "IN THE FILE ALREADY. NO MORE ARE ALLOWED."
           GO TO 2000TI
           GO TO 2130-CHAR-KEY.
       2400-FIND.
           MOVE SPACES TO FFTAB1001 ACTION NAME-LAST.
           UNSTRING DATAIN DELIMITED BY "," INTO ACTION NAME-LAST
           FFTAB1001.
           MOVE SPACE TO G-GARNO.
           IF ACTION = "SG" MOVE NAME-LAST TO G-GARNO
           ELSE MOVE NL-3 TO G-GARNO.
           IF LLTAB24(1) = "*" MOVE SPACE TO G-GARNO.
           START GARFILE KEY NOT < G-GARNO INVALID
           DISPLAY " END OF FILE" BELL0  GO TO 2000TI.
           MOVE NAME-LAST TO LLTAB2401.
           MOVE 0 TO FF LL.
           PERFORM FF-NAME VARYING X FROM 10 BY -1 UNTIL X < 1.
           PERFORM LL-NAME VARYING X FROM 24 BY -1 UNTIL X < 1.
           IF LL = 0 MOVE 1 TO LL.
           IF LLTAB24(LL) = "/"
           MOVE " " TO LLTAB24(LL) MOVE 24 TO LL.
       2400-FIND-20.
           MOVE 0 TO X YYY.
           MOVE 0 TO FLAG.
           PERFORM 2400-SEARCH THRU 2400-SEARCH-EXIT.
           IF FLAG = 0 GO TO 2400-FIND-QUES.
           IF FLAG = 1 DISPLAY "END OF FILE FOUND" BELL0
           ELSE DISPLAY "NO MORE MATCHES ON NAME" BELL0.
           DISPLAY "END OF SEARCH"
           GO TO 1000-ACTION.
       2400-FIND-QUES.
           DISPLAY "?".
           ACCEPT ANS.
           IF ANS = SPACES GO TO 2400-FIND-20.
           IF ANS = "?"
           DISPLAY "A <CR> WILL PRODUCE 10 MORE NAMES"
           DISPLAY "ANYTHING ELSE WILL RETURN YOU TO THE OPTION COMMAND"
           GO TO 2400-FIND-QUES.
           GO TO 1000-ACTION.
       2400-SEARCH.
           IF X > 8 GO TO 2400-SEARCH-EXIT.
           READ GARFILE NEXT AT END MOVE 1 TO FLAG
           GO TO 2400-SEARCH-EXIT.
           ADD 1 TO YYY.
           IF YYY < 990 GO TO P456.
           DISPLAY "CONTINUE LOOKING ? <CR> FOR YES".
           ACCEPT DATAIN.
           IF DATAIN NOT = SPACE GO TO 2400-SEARCH-EXIT.
           MOVE 0 TO YYY.
       P456.
           IF ALF-3 NOT = "000" AND ALF-3 NOT = G-PRINS AND ALF-3 NOT =
           G-SEINS AND ALF-3 NOT = G-TRINS GO TO 2400-SEARCH.
           IF ACTION = "SG" GO TO 2400-S.
           MOVE SPACE TO NAME-TEST XFFTAB1001.
           UNSTRING G-GARNAME DELIMITED BY ";" INTO NAME-TEST
           XFFTAB1001.
           IF ((NT-31 = NL-31) AND (NL-32 = SPACE OR NL-33 = SPACE))
           OR (NT-3 = NL-3) OR (LLTAB24(1) = "*") NEXT SENTENCE
           ELSE MOVE 2 TO FLAG GO TO 2400-SEARCH-EXIT.
           MOVE NAME-TEST TO XLLTAB2401.
           MOVE 0 TO FLAGX 
           IF LLTAB24(1) NOT = "*"
           PERFORM AQ1 VARYING Y FROM 1 BY 1 UNTIL Y > LL.
           IF FLAGX = 1 GO TO 2400-SEARCH.
           PERFORM AQ2 VARYING Y FROM 1 BY 1 UNTIL Y > FF.
           IF FLAGX = 1 GO TO 2400-SEARCH.
       2400-S.
           ADD 1 TO X
           MOVE 0 TO YYY
           MOVE G-GARNO TO GAR-TAB(X)
           MOVE G-DOB TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           INSPECT T-MM OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           INSPECT T-DD OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           DISPLAY X " " G-GARNO " " DISPLAY-DATE  " " G-GARNAME
           " " G-CITY " " G-PRINS "/" G-SEINS "/" G-TRINS
           GO TO 2400-SEARCH.
       2400-SEARCH-EXIT.
           EXIT.
       FF-NAME. IF FFTAB10(X) NOT = SPACE MOVE X TO FF MOVE 1 TO X.
       LL-NAME. IF LLTAB24(X) NOT = SPACE MOVE X TO LL MOVE 1 TO X.
       AQ1. IF LLTAB24(Y) NOT = XLLTAB24(Y) MOVE 1 TO FLAGX.
       AQ2. IF XFFTAB10(Y) NOT = FFTAB10(Y) MOVE 1 TO FLAGX.
       3600-FIND.
           MOVE SPACES TO  FFTAB1001 ACTION NAME-LAST.
           UNSTRING DATAIN DELIMITED BY "," INTO ACTION NAME-LAST
           FFTAB1001.
           MOVE SPACE TO P-PATNO.
           IF ACTION = "SP" MOVE NAME-LAST TO P-PATNO
           ELSE MOVE NL-3 TO P-PATNO.
           IF LLTAB24(1) = "*" MOVE SPACE TO P-PATNO.
           START PATFILE KEY NOT < P-PATNO INVALID
           DISPLAY P-PATNO
           DISPLAY " END OF FILE" GO TO 2000TI.
           MOVE NAME-LAST TO LLTAB2401.
           MOVE 0 TO FF LL.
           PERFORM FF-NAME VARYING X FROM 10 BY -1 UNTIL X < 1.
           PERFORM LL-NAME VARYING X FROM 24 BY -1 UNTIL X < 1.
           IF LL = 0 MOVE 1 TO LL.
           IF LLTAB24(LL) = "/"
           MOVE " " TO LLTAB24(LL) MOVE 24 TO LL.
       3600-FIND-20.
           MOVE 0 TO X.
           MOVE 0 TO FLAG YYY.
           PERFORM 3600-SEARCH THRU 3600-SEARCH-EXIT.
           IF FLAG = 0 GO TO 3600-FIND-QUES.
           IF FLAG = 1 DISPLAY "END OF FILE FOUND"
           ELSE DISPLAY "NO MORE MATCHES ON NAMES".
           DISPLAY "END OF SEARCH"
           GO TO 2000TI.
       3600-FIND-QUES.
           DISPLAY "?".
           ACCEPT ANS.
           IF ANS = SPACES GO TO 3600-FIND-20.
           IF ANS = "?"
           DISPLAY "A <CR> WILL PRODUCE 10 MORE NAMES"
           DISPLAY "ANYTHING ELSE WILL END THE SEARCH"
           GO TO 3600-FIND-QUES.
           GO TO 2000TI.
       3600-SEARCH.
           IF X > 8 GO TO 3600-SEARCH-EXIT.
           READ PATFILE NEXT AT END MOVE 1 TO FLAG
           GO TO 3600-SEARCH-EXIT.
           MOVE P-GARNO TO G-GARNO
           READ GARFILE INVALID KEY DISPLAY "THIS PATIENT HAS NO "
           "VALID GUARANTOR #."
           DISPLAY "THIS MUST NOT HAPPEN. CALL THE DATA CENTER"
           " IMMEDIATELY"
           DISPLAY "THIS PROGRAM TERMINATED"
           GO TO 9100-CLOSE-MASTER-FILE.
           ADD 1 TO YYY.
           IF YYY < 990 GO TO P789.
           DISPLAY "CONTINUE LOOKING ? <CR> FOR YES".
           ACCEPT DATAIN.
           IF DATAIN NOT = SPACE GO TO 3600-SEARCH-EXIT.
           MOVE 0 TO YYY.
       P789.
           IF ALF-3 NOT = "000" AND ALF-3 NOT = G-PRINS AND ALF-3 NOT =
           G-SEINS AND ALF-3 NOT = G-TRINS GO TO 3600-SEARCH.
           IF ACTION = "SP" GO TO 3600-S.
           MOVE SPACE TO NAME-TEST XFFTAB1001.
           UNSTRING P-PATNAME DELIMITED BY ";" INTO NAME-TEST XFFTAB1001
           IF ((NT-31 = NL-31) AND (NL-32 = SPACE OR NL-33 = SPACE))
           OR (NT-3 = NL-3) OR (LLTAB24(1) = "*") NEXT SENTENCE
           ELSE MOVE 2 TO FLAG GO TO 3600-SEARCH-EXIT.
           MOVE NAME-TEST TO XLLTAB2401.
           MOVE 0 TO FLAGX 
           IF LLTAB24(1) NOT = "*"
           PERFORM AQ1 VARYING Y FROM 1 BY 1 UNTIL Y > LL.
           IF FLAGX = 1 GO TO 3600-SEARCH.
           PERFORM AQ2 VARYING Y FROM 1 BY 1 UNTIL Y > FF.
           IF FLAGX = 1 GO TO 3600-SEARCH.
       3600-S.
           ADD 1 TO X
           MOVE 0 TO YYY
           MOVE P-PATNO TO GAR-TAB(X)
           MOVE P-DOB TO DISPLAY-DOB
           MOVE CORR DISPLAY-DOB TO DISPLAY-DATE
           INSPECT T-MM OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           INSPECT T-DD OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           MOVE G-GARNAME TO ALF-19
           DISPLAY X " " P-PATNO " " DISPLAY-DATE " " P-PATNAME " "
           G-PRINS "/" G-SEINS "/" G-TRINS " "  ALF-19
           GO TO 3600-SEARCH.
       3600-SEARCH-EXIT.
           EXIT.

       2120-NAME.
           DISPLAY "CAN""T CHANGE NAME ON A TRANSACTION!".
           DISPLAY "CHANGE THE PATIENT ACCT # TO GET A CHANGE OF NAME.".
           GO TO 2000TI.
       2180-PATID.
           IF IN-FIELD = "?"
           DISPLAY "ENTER EITHER A PATIENT OR GUARANTOR ID"
           DISPLAY "AS THE PATIENT OF RECORD."
           GO TO 2000TI.
           IF (IN-FIELD-TAB(1) > "0" ) AND ( IN-FIELD-TAB(1) < "9"
           OR IN-FIELD-TAB(1) = "9" ) AND ( IN-FIELD-TAB(2) = " "
           AND IN-FIELD-TAB(3) = " " AND IN-FIELD-TAB(4) = " " )
            MOVE IN-FIELD-1 TO FLAG
           MOVE GAR-TAB(FLAG) TO IN-FIELD-8.
           MOVE IN-FIELD-8 TO EIGHTPARTID.
           IF EIGHT-1 NOT = "P" AND EIGHT-1 NOT = "G"
           DISPLAY "INVALID" GO TO 2000TI.
           IF EIGHT-1 = "G" GO TO 2140-PATID.
           MOVE IN-FIELD-8 TO P-PATNO.
           READ PATFILE INVALID KEY DISPLAY "NOT ON FILE"
           GO TO 2000TI.
           MOVE P-GARNO TO G-GARNO.
           READ GARFILE INVALID KEY DISPLAY "THIS PATIENT HAS NO "
           "VALID GUARANTOR #."
           DISPLAY "THIS MUST NOT HAPPEN. CALL DATA CENTER IMMEDIATELY"
           DISPLAY "THIS PROGRAM IS TERMINATED"
           GO TO 9100-CLOSE-MASTER-FILE.
           IF G-ACCTSTAT = "0" DISPLAY "THIS ACCT HAS BEEN CLOSED"
           GO TO 2000TI.
           IF (G-PRINS = "003" OR "004" OR "064") OR 
           (G-SEINS = "003" OR "004" OR "064")
           DISPLAY "GARANTOR FOR THIS PATIENT IS " G-PRINS "/" G-SEINS
           DISPLAY "AND SHOULD NOT HAVE A PATIENT UNDER THIS ACCOUNT"
           GO TO 2000TI.
           MOVE SPACE TO TEMP-FIELD01.
           STRING G-GARNO ": " G-GARNAME ": " G-STREET ": " G-CITY ": "
           G-STATE DELIMITED BY "   " INTO TEMP-FIELD01.
           DISPLAY P-PATNAME.
           DISPLAY TEMP-FIELD01.
           DISPLAY "INS " G-PRINS " " G-PRIPOL " / " G-SEINS " "
           G-SECPOL.
       2181-PATID.
           MOVE IN-FIELD-8 TO PATID
           MOVE P-PATNAME TO NAME
           GO TO 2183-PATID.
       2140-PATID.
           IF (ACTION = "C") AND (PATID NOT = G-GARNO)
           DISPLAY "GARNO NOT OWNER OF RECORD. INVALID" GO TO 2000TI.
           MOVE IN-FIELD-8 TO G-GARNO.
           READ GARFILE INVALID KEY DISPLAY "NOT ON FILE"
           GO TO 2000TI.
           IF G-ACCTSTAT = "0" DISPLAY "THIS ACCT HAS BEEN CLOSED"
           GO TO 2000TI.
           MOVE SPACE TO TEMP-FIELD01.
           STRING G-GARNO ": " G-GARNAME ": " G-STREET ": " G-CITY ": "
           G-STATE DELIMITED BY "   " INTO TEMP-FIELD01.
           DISPLAY TEMP-FIELD01.
           DISPLAY "INS " G-PRINS " " G-PRIPOL " / " G-SEINS " "
           G-SECPOL " / " G-TRINS.
           MOVE IN-FIELD-8 TO PATID
           MOVE G-GARNAME TO NAME.
       2183-PATID.
           MOVE G-PRINS TO PAYCODE
           MOVE G-PRINS TO INS-KEY
           READ INSFILE INVALID DISPLAY "NO VALID INS." GO TO 2000TI.
           IF INS-REFWARN = "1"
           DISPLAY "ALL CHARGES REQUIRE A REF. PHYS.".
           DISPLAY INS-KEY " " INS-ASSIGN " " INS-CLAIMTYPE " "
           INS-NAME.
           MOVE G-PR-ASSIGN TO CD-ASSIGN
           MOVE INS-NEIC-ASSIGN TO CD-NEIC-ASSIGN
           MOVE INS-CLAIMTYPE TO PAPER.
           MOVE G-GARNO TO SAVE-GARNO
           GO TO 4900DEE.
       2100-CLAIM.
            DISPLAY "CLAIM NUMBERS CANT""T BE CHANGED"
           GO TO 1400-CHANGE-PROCESS.
       2130-SERVICE.
           IF ACTION = "SDV" MOVE SERVICE TO PM-DATA GO TO SDV4.
           IF IN-FIELD = "/" MOVE SERVICE TO IN-FIELD DISPLAY SERVICE.
           IF IN-FIELD = "?"
           DISPLAY "ENTER THE TYPE OF SERVICE. CODES ARE"
           DISPLAY "1=SURG. 2=ASST. SURG. 3=ANESTH. 4=LAB"
           DISPLAY "5=X-RAY 6=MEDICAL 7=IMMUNIZ."
               GO TO 2000TI.
           IF IN-FIELD-1 > "0" AND < "8"
           MOVE IN-FIELD-1 TO SERVICE GO TO 4900DEE
           ELSE MOVE "?" TO IN-FIELD GO TO 2130-SERVICE.
       2150-DIAG.
           IF ACTION = "SDV" MOVE DIAG TO PM-DATA GO TO SDV4.
           IF IN-FIELD = "/" MOVE DIAG TO IN-FIELD DISPLAY DIAG.
           IF IN-FIELD = "?"
               DISPLAY "ENTER A DIAGNOSIS CODE."
           DISPLAY " OR TYPE F TO SEARCH FOR A DIAGNOSIS CODE"
           DISPLAY " OR TYPE M TO MAP ICD9 TO ICD10 CODES"
           DISPLAY "BY INSERTING THE FIRST LETTER OF DIAGNOSIS"
           DISPLAY "DESCRIPTION"
           DISPLAY "OR A <CR> IF NO DIAGNOSIS IS NECESSARY."
               GO TO 2000TI.
           IF IN-FIELD = "0000000" OR SPACES MOVE ZEROES TO DIAG
           GO TO 4900DEE.
           MOVE 0 TO RETURN-FLAG
           PERFORM  CD10 THRU CD10-EXIT
           IF RETURN-FLAG = 1 GO TO 2000TI.
           MOVE IN-FIELD-7 TO DIAG.
           GO TO 4900DEE.
       2-DX2.
           IF ACTION = "SDV" MOVE CD-DX2 TO PM-DATA GO TO SDV4.
           IF ACTION = "DF2" MOVE PM-DATA TO CD-DX2 GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE CD-DX2 TO IN-FIELD DISPLAY CD-DX2.
           IF IN-FIELD = "?"
               DISPLAY "ENTER A SECOND DX CODE."
           DISPLAY " OR TYPE F TO SEARCH FOR A DIAGNOSIS CODE"
           DISPLAY " OR TYPE M TO MAP ICD9 TO ICD10 CODES"
           DISPLAY "BY INSERTING THE FIRST LETTER OF DIAGNOSIS"
           DISPLAY "DESCRIPTION"
           DISPLAY "OR A <CR> IF NO DIAGNOSIS IS NECESSARY."
               GO TO 2000TI.
           IF IN-FIELD = SPACES MOVE ZEROES TO CD-DX2
           GO TO 4900DEE.
           MOVE 0 TO RETURN-FLAG
           PERFORM  CD10 THRU CD10-EXIT
           IF RETURN-FLAG = 1 GO TO 2000TI.
           MOVE IN-FIELD-7 TO CD-DX2.
           GO TO 4900DEE.
       2-DX3.
           IF ACTION = "SDV" MOVE CD-DX3 TO PM-DATA GO TO SDV4.
           IF ACTION = "DF2" MOVE PM-DATA TO CD-DX3 GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE CD-DX3 TO IN-FIELD DISPLAY CD-DX3.
           IF IN-FIELD = "?"
               DISPLAY "ENTER A THIRD DX  CODE."
           DISPLAY " OR TYPE F TO SEARCH FOR A DIAGNOSIS CODE"
           DISPLAY " OR TYPE M TO MAP ICD9 TO ICD10 CODES"
           DISPLAY "BY INSERTING THE FIRST LETTER OF DIAGNOSIS"
           DISPLAY "DESCRIPTION"
           DISPLAY "OR A <CR> IF NO DIAGNOSIS IS NECESSARY."
               GO TO 2000TI.
           IF IN-FIELD = SPACES MOVE ZEROES TO CD-DX3
           GO TO 4900DEE.
           MOVE 0 TO RETURN-FLAG
           PERFORM  CD10 THRU CD10-EXIT
           IF RETURN-FLAG = 1 GO TO 2000TI.
           MOVE IN-FIELD-7 TO CD-DX3.
           GO TO 4900DEE.
       2-DX4.
           IF ACTION = "SDV" MOVE CD-DX4 TO PM-DATA GO TO SDV4.
           IF ACTION = "DF2" MOVE PM-DATA TO CD-DX4 GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE CD-DX4 TO IN-FIELD DISPLAY CD-DX4.
           IF IN-FIELD = "?"
               DISPLAY "ENTER A FOUTRH DX  CODE."
           DISPLAY " OR TYPE F TO SEARCH FOR A DIAGNOSIS CODE"
           DISPLAY " OR TYPE M TO MAP ICD9 TO ICD10 CODES"
           DISPLAY "BY INSERTING THE FIRST LETTER OF DIAGNOSIS"
           DISPLAY "DESCRIPTION"
           DISPLAY "OR A <CR> IF NO DIAGNOSIS IS NECESSARY."
               GO TO 2000TI.
           IF IN-FIELD = SPACES MOVE ZEROES TO CD-DX4
           GO TO 4900DEE.
           MOVE 0 TO RETURN-FLAG
           PERFORM  CD10 THRU CD10-EXIT
           IF RETURN-FLAG = 1 GO TO 2000TI.
           MOVE IN-FIELD-7 TO CD-DX4.
           GO TO 4900DEE.
       2-DX5.
           IF ACTION = "SDV" MOVE CD-DX5 TO PM-DATA GO TO SDV4.
           IF ACTION = "DF2" MOVE PM-DATA TO CD-DX5 GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE CD-DX5 TO IN-FIELD DISPLAY CD-DX5.
           IF IN-FIELD = "?"
               DISPLAY "ENTER A FIFTH DX  CODE."
           DISPLAY " OR TYPE F TO SEARCH FOR A DIAGNOSIS CODE"
           DISPLAY " OR TYPE M TO MAP ICD9 TO ICD10 CODES"
           DISPLAY "BY INSERTING THE FIRST LETTER OF DIAGNOSIS"
           DISPLAY "DESCRIPTION"
           DISPLAY "OR A <CR> IF NO DIAGNOSIS IS NECESSARY."
               GO TO 2000TI.
           IF IN-FIELD = SPACES MOVE ZEROES TO CD-DX5
           GO TO 4900DEE.
           MOVE 0 TO RETURN-FLAG
           PERFORM  CD10 THRU CD10-EXIT
           IF RETURN-FLAG = 1 GO TO 2000TI.
           MOVE IN-FIELD-7 TO CD-DX5.
           GO TO 4900DEE.
       2-DX6.
           IF ACTION = "SDV" MOVE CD-DX6 TO PM-DATA GO TO SDV4.
           IF ACTION = "DF2" MOVE PM-DATA TO CD-DX6 GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE CD-DX6 TO IN-FIELD DISPLAY CD-DX6.
           IF IN-FIELD = "?"
               DISPLAY "ENTER A SIXTH DX  CODE."
           DISPLAY " OR TYPE F TO SEARCH FOR A DIAGNOSIS CODE"
           DISPLAY " OR TYPE M TO MAP ICD9 TO ICD10 CODES"
           DISPLAY "BY INSERTING THE FIRST LETTER OF DIAGNOSIS"
           DISPLAY "DESCRIPTION"
           DISPLAY "OR A <CR> IF NO DIAGNOSIS IS NECESSARY."
               GO TO 2000TI.
           IF IN-FIELD = SPACES MOVE ZEROES TO CD-DX6
           GO TO 4900DEE.
           MOVE 0 TO RETURN-FLAG
           PERFORM  CD10 THRU CD10-EXIT
           IF RETURN-FLAG = 1 GO TO 2000TI.
           MOVE IN-FIELD-7 TO CD-DX6.
           GO TO 4900DEE.

       2140-PROC.
           IF ACTION = "SDV" MOVE PROC TO PM-DATA GO TO SDV4.
           IF ACTION = "DF2" MOVE PM-DATA TO PROC GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE PROC TO IN-FIELD DISPLAY PROC.
           IF IN-FIELD = "?"
               DISPLAY "ENTER A PROCEDURE CODE."
           DISPLAY " AND OPTIONALLY OVERRIDE TABLE DRIVEN CHARGE"
           DISPLAY "AMOUNT BY APPENDING CHARGE AMOUNT."
           DISPLAY "EX: 99213 60  WOULD CREATE A $60.00 CHARGE FOR"
           DISPLAY "PROCEDURE 99213"
           DISPLAY "TYPE F TO SEARCH PROCEDURE FILE."
           GO TO 2000TI.
           IF IN-FIELD = "F" GO TO 2150-PROC.
           MOVE SPACES TO ALF-7 ALF-7X RIGHT-5
           MOVE 0 TO FLAG OVERRIDE-AMOUNT.
           UNSTRING IN-FIELD DELIMITED BY " " INTO ALF-7X ALF-7.
           IF ALF-7 NOT = SPACES MOVE 3 TO FLAG.
           MOVE ALF-7X TO PROC-KEY
           READ PROCFILE INVALID KEY
           DISPLAY "PROCEDURE CODE NOT IN FILE"
           GO TO 2000TI.
           IF PROC-TYPE = "*" MOVE PROC-TITLE TO IN-FIELD 
           DISPLAY IN-FIELD-7
           GO TO 2140-PROC.
           IF PROC-KEY > "ZZ     " DISPLAY "OLD CODE INVALID" GO TO
           2000TI.
      *     IF IN-FIELD-TAB(5) = "F"
      *       MOVE PROC-KEY TO PROC
      *       MOVE PROC-TYPE TO SERVICE
      *       MOVE 0 TO AMOUNT
      *      GO TO 4900DEE.
           MOVE PROC-KEY TO PROC
           MOVE PROC-TYPE TO SERVICE.
           IF ((G-PRINS = "003" OR "028" OR "093" OR "098")
           AND (CARE-AMOUNT = 0) AND (FLAG = 0))
           OR ((G-PRINS NOT = "003" AND G-PRINS NOT = "028" AND
           G-PRINS NOT = "093" AND G-PRINS NOT = "098")
           AND (PROC-AMOUNT = 0)  AND (FLAG = 0))
           DISPLAY "AMOUNT NOT SET FOR THIS PROCEDURE IN FILE"
           DISPLAY PROC-TITLE
           DISPLAY "AMOUNT MUST BE ADDED WHEN PROCEDURE IS ENTERED"
           DISPLAY " USING THE OVERRIDE FEATURE" GO TO 2000TI.
           IF FLAG = 3 DISPLAY PROC-TITLE GO TO 2200-AMOUNT.
           IF G-PRINS = "003" OR "028" OR "093" OR "098"
           MOVE CARE-AMOUNT TO AMOUNT NEF-5
           ELSE MOVE PROC-AMOUNT TO NEF-5 AMOUNT.
           DISPLAY NEF-5 " " PROC-TITLE GO TO 4900DEE.
       2150-PROC. DISPLAY "SEARCH KEY".
           ACCEPT PROC-KEY.
           IF PROC-KEY = "?"
           DISPLAY "TYPE A CODE FROM WHICH POINT TO START SEARCH."
           DISPLAY "THE FILE IS IN INCREASING PROC. # ORDER"
           DISPLAY "TYPE X TO BACK TO PROMPT FOR PROCEDURE."
           GO TO 2150-PROC.
           IF PROC = "X" GO TO 2000TI.
           START PROCFILE KEY NOT < PROC-KEY INVALID
           DISPLAY "END OF FILE" GO TO 2000TI.
           MOVE 0 TO X.
       2160-PROC. READ PROCFILE NEXT AT END DISPLAY
           "END OF FILE" GO TO 2000TI.
           IF X > 5 DISPLAY "?"
           ACCEPT ANS
           MOVE 0 TO X
           IF ANS NOT = SPACE GO TO 2000TI.
           MOVE SPACE TO RIGHT-8
           MOVE PROC-AMOUNT TO NEF-8
           IF CARE-AMOUNT NOT = PROC-AMOUNT
           MOVE CARE-AMOUNT TO NEF-5  MOVE NEF-5 TO RIGHT-8.
           DISPLAY PROC-KEY " "  NEF-8 " " RIGHT-8 " "
           PROC-TYPE " " PROC-NDC " " PROC-TITLE
           ADD 1 TO X. GO TO 2160-PROC.
       2200-AMOUNT.
           IF IN-FIELD = "?"
           DISPLAY "ENTER THE AMOUNT OF CHARGE"
           DISPLAY "EXAMPLE: $1,234,56 = 1234.56"
           DISPLAY "CENTS PORTION ASSUMED TO BE 00 IF NOT TYPED"
           DISPLAY "DECIMAL POINT IS OPTIONAL WHEN THIS IS THE CASE"
           GO TO 2000TI.
           MOVE SPACES TO SIGN-DOLLAR CENTS.
           IF FLAG = 3
           MOVE ALF-7 TO IN-FIELD.
           UNSTRING IN-FIELD DELIMITED BY "." INTO SIGN-DOLLAR CENTS.
           IF CENTS = SPACES MOVE "00" TO CENTS.
           IF CENTS NOT NUMERIC DISPLAY "INVALID"
           GO TO 2000TI.
           MOVE SPACES TO RIGHT-4.
           UNSTRING SIGN-DOLLAR DELIMITED BY " " INTO RIGHT-4
           INSPECT RIGHT-4 REPLACING LEADING " " BY "0"
           IF RIGHT-4 NOT NUMERIC DISPLAY "NOT NUMERIC"
           GO TO 2000TI.
           STRING RIGHT-4 CENTS DELIMITED BY "Z" INTO ALF-6
           MOVE ALF-6 TO NUM-6
           DIVIDE NUM-6 BY 100 GIVING AMOUNT.
           IF FLAG = 3 MOVE 0 TO FLAG MOVE AMOUNT TO NEF-8
           DISPLAY NEF-8 " ".
            GO TO 4900DEE.
       2160-DOCR.
           IF ACTION = "SDV" MOVE DOCR TO PM-DATA GO TO SDV4.
           IF ACTION = "DF2" MOVE PM-DATA TO DOCR GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE DOCR TO IN-FIELD DISPLAY DOCR.
           IF IN-FIELD = "?"
           DISPLAY "ENTER THE REFERRING PHYS. CODE"
           DISPLAY "OR TYPE F TO SEARCH FOR CODE"
           DISPLAY "A <CR> IF NO REFERRING PHYSICIAN."
           GO TO 2000TI.
           IF ((IN-FIELD-3 = SPACE)
           AND (PROC > "99240  " AND < "99281  "))
           OR ((INS-REFWARN = "1") AND (IN-FIELD-3 = SPACE))
           DISPLAY "REFERRING PHYS. REQUIRED ON THIS CHARGE." BELL0.
           IF IN-FIELD-3 = "000" OR SPACES MOVE ZEROES TO DOCR
           GO TO 4900DEE.
           IF IN-FIELD = "F" GO TO 1REF-SEARCH.
           MOVE IN-FIELD-3 TO REF-KEY.
           READ REFPHY INVALID DISPLAY "NOT ON FILE"
               GO TO 2000TI.
           DISPLAY REF-NAME
           MOVE IN-FIELD-3 TO DOCR
           GO TO 4900DEE.
       1REF-SEARCH.
           DISPLAY "SEARCH KEY ?".
           ACCEPT REF-NAME.
           IF REF-NAME = "?"
           DISPLAY "ENTER ANY PART OF LAST NAME"
           DISPLAY "OR 1ST LETTER OF LAST NAME TO SEARCH BY KEY"
           GO TO 1REF-SEARCH.
           IF REF-NAME  = SPACE GO TO 2000TI.
           MOVE REF-NAME TO IN-FIELD
           IF IN-FIELD-TAB(2) = SPACE AND IN-FIELD-TAB(3) = SPACE
           MOVE IN-FIELD-TAB(1) TO ALF-1
           STRING ALF-1 "00" DELIMITED BY " " INTO REF-KEY
           GO TO 1REF-KEY.
           START REFPHY KEY NOT < REF-NAME INVALID
           DISPLAY "END OF FILE" GO TO 2000TI.
           MOVE 0 TO X
           GO TO 3REF.
       1REF-KEY.
           START REFPHY KEY > REF-KEY INVALID
           DISPLAY "END OF FILE" GO TO 2000TI.
           MOVE 0 TO X.
       3REF. READ REFPHY NEXT AT END
           DISPLAY "END OF FILE" GO TO 2000TI.
           ADD 1 TO X
           IF X > 5
           ACCEPT ANS
           MOVE 0 TO X
           IF ANS NOT = SPACE GO TO 2000TI.
           DISPLAY REF-KEY " " REF-BSNUM " " REF-CRNUM " " REF-UPIN " "
           REF-CDNUM " " REF-NPI " " REF-NAME
           GO TO 3REF.
       2-SORCREF.
           IF ACTION = "SDV" MOVE CD-SORCREF TO PM-DATA GO TO SDV4.
           IF ACTION = "DF2" MOVE PM-DATA TO CD-SORCREF GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE CD-SORCREF TO IN-FIELD
           DISPLAY CD-SORCREF.
           IF IN-FIELD = "?"
           DISPLAY "ENTER THE SOURCE OF REFERRAL CODE"
           DISPLAY "1=SELF 2=MD 3=PUBLIC HEALTH CLINIC "
           DISPLAY "4=HOME HEALTH OR VISITING NURSE "
           DISPLAY "5=OTHER PRACTIONER"
           GO TO 2000TI.
           IF IN-FIELD-1 = SPACE MOVE "1" TO IN-FIELD-1
           DISPLAY "SELF".
           IF IN-FIELD-1 = "1" OR "2" OR "3" OR "4" OR "5"
           MOVE IN-FIELD-1 TO CD-SORCREF GO TO 4900DEE.
           DISPLAY IN-FIELD-1 " BAD" GO TO 2000TI.
       2165-DOCP.
           IF ACTION = "SDV" MOVE DOCP TO PM-DATA GO TO SDV4.
           IF ACTION = "DF2" MOVE PM-DATA TO DOCP GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE DOCP TO IN-FIELD DISPLAY DOCP.
           IF IN-FIELD = "?"
           DISPLAY "ENTER THE LOCAL DOCTOR CODE (2-DIGITS)"
           GO TO 2000TI.
           MOVE SPACES TO RIGHT-2
           UNSTRING IN-FIELD-2 DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING LEADING " " BY "0"
           IF RIGHT-2 NOT NUMERIC DISPLAY "NOT NUMERIC"
           GO TO 2000TI.
           IF RIGHT-2 < "01" OR > HIGH-DOC DISPLAY "INVALID"
           GO TO 2000TI.
           MOVE RIGHT-2 TO NUM-2
           IF RATE-PC(NUM-2) = 99
           DISPLAY "DOCTOR " NUM-2 " CANNOT BE USED."
           GO TO 2000TI.
           MOVE RIGHT-2 TO DOCP 
           IF HP = 1 
           MOVE DOCP TO NUM-2
           DISPLAY RATE-NAME(NUM-2).
           GO TO 4900DEE.
       2170-PAYCODE.
           IF IN-FIELD = "?"
           DISPLAY "TYPE A 3-DIGIT NUMBER FOR A PAYORCODE"
           GO TO 2000TI.
           MOVE SPACE TO RIGHT-3
           UNSTRING IN-FIELD-3 DELIMITED BY " " INTO RIGHT-3
           INSPECT RIGHT-3 REPLACING ALL " " BY "0".
           MOVE RIGHT-3 TO INS-KEY
           READ INSFILE INVALID DISPLAY "INVALID INSURANCE CODE"
           GO TO 2000TI.
           DISPLAY "ACCT " INS-ASSIGN "  CLM " INS-NEIC-ASSIGN
           "    " INS-NAME
           DISPLAY "ASSIGNMENT ATTRIBUTES ARE NOW CHANGED AS ABOVE"
           MOVE INS-ASSIGN TO CD-ASSIGN
           MOVE INS-NEIC-ASSIGN TO CD-NEIC-ASSIGN
           MOVE INS-CLAIMTYPE TO PAPER
           MOVE INS-KEY TO PAYCODE GO TO 4900DEE.
       2740-STAT.
           IF ACTION = "SDV" MOVE STAT TO PM-DATA GO TO SDV4.
           IF ACTION = "DF2" MOVE PM-DATA TO STAT GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE STAT TO IN-FIELD DISPLAY STAT.
           IF IN-FIELD = "?"
           DISPLAY "0=NEW 1=BEEN BILLED 2=CLAIM SENT 3=BILL/CLAIM SENT"
           GO TO 2000TI.
           IF IN-FIELD-1 = "0" OR "1" OR "2" OR "3" NEXT SENTENCE
           ELSE DISPLAY "INVALID" GO TO 2000TI.
           MOVE IN-FIELD-1 TO STAT GO TO 4900DEE.
       2750-WORK.
           IF ACTION = "SDV" MOVE WORK TO PM-DATA GO TO SDV4.
           IF ACTION = "DF2" MOVE PM-DATA TO WORK GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE WORK TO IN-FIELD DISPLAY WORK.
           IF IN-FIELD = "?"
           DISPLAY "TYPE # OF UNITS REP. BY CHARGE"
           DISPLAY "1-99 ARE VALID. <CR> = 1"
           GO TO 2000TI.
           IF IN-FIELD-2 = "01" OR SPACE MOVE "01" TO WORK
           GO TO 4900DEE.
           MOVE SPACE TO RIGHT-2
           UNSTRING IN-FIELD-2 DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING LEADING " " BY "0"
           IF RIGHT-2 = "00" MOVE "01" TO RIGHT-2.
           IF RIGHT-2 NOT NUMERIC DISPLAY "NOT NUMERIC" GO TO 2000TI.
           IF RIGHT-2 > "09" DISPLAY "CHECK THIS".
           MOVE RIGHT-2 TO WORK
           IF RIGHT-2 = "01" GO TO 4900DEE.
           IF (PROC = "17003  " OR "1700359" OR "11056  " OR "95044  ")
           OR (PROC > "J      " AND < "JZZZZZZ")
           NEXT SENTENCE 
           ELSE GO TO 4900DEE.
           IF PROC = "J1245  " GO TO 4900DEE.
           MOVE RIGHT-2 TO NUM-2
           MOVE PROC TO PROC-KEY
           READ PROCFILE INVALID DISPLAY "BAD" GO TO 4900DEE.
           IF PROC-AMOUNT = 0 
           DISPLAY "MULTIPLIER NOT IN EFFECT"
           DISPLAY "PRICE MUST BE SET SEPARATELY, IF NECESSARY"
           GO TO 4900DEE.
           IF OVERRIDE-AMOUNT = 0
              MULTIPLY NUM-2 BY PROC-AMOUNT GIVING AMOUNT
              MOVE PROC-AMOUNT TO NEF-5
           ELSE
              MULTIPLY NUM-2 BY OVERRIDE-AMOUNT GIVING AMOUNT
              MOVE OVERRIDE-AMOUNT TO NEF-5
           END-IF
             MOVE AMOUNT TO NEF-8
           DISPLAY "NEW AMOUNT = " NUM-2 " x " NEF-5 " = " NEF-8
           GO TO 4900DEE.
       1450-PAPER. IF IN-FIELD = "?"
           DISPLAY "P= CLAIM FORM SENT AUTOMATICALLY"
           DISPLAY "O= CLAIM FORM DELIVERED TO OFFICE"
           DISPLAY "<CR> IF NEITHER" GO TO 2000TI.
           IF IN-FIELD-1 = "P" OR "O"  DISPLAY "PAPER CLAIM"
            MOVE IN-FIELD-1 TO PAPER GO TO 4900DEE.
           IF IN-FIELD-1 NOT = " " DISPLAY "INVALID" GO TO 2000TI.
           MOVE " " TO PAPER GO TO 4900DEE.
       2180-PLACE.
           IF ACTION = "SDV" MOVE PLACE TO PM-DATA GO TO SDV4.
           IF ACTION = "DF2" MOVE PM-DATA TO PLACE GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE PLACE TO IN-FIELD DISPLAY PLACE.
           IF IN-FIELD = "?"
           DISPLAY "ENTER PLACE OF SERVICE CODE."
           DISPLAY "1=OFFICE 2=PAT. HOME"
           DISPLAY "7=REHAB. 8=OTHER"
           DISPLAY "FF=SEARCH PLACE OF SERVICE CODES"
           DISPLAY "OR LETTER CODE FROM A TO " HIGH-PLACE
               GO TO 2000TI.
           IF IN-FIELD-1 = " "        DISPLAY "INVALID" GO TO 2000TI.
           IF IN-FIELD = "1" OR "2" OR "7" OR "8"
           MOVE IN-FIELD-1 TO PLACE GO TO 4900DEE.
           MOVE 1 TO FLAG
           PERFORM 4500-PL1 THRU 4500-PL1-EXIT VARYING X
           FROM 1 BY 1 UNTIL X > PLINDX
           IF IN-FIELD-2 = "FF" GO TO 2000TI.
           IF FLAG = 1 DISPLAY "INVALID" GO TO 2000TI.
           MOVE IN-FIELD-1 TO PLACE GO TO 4900DEE.
       4500-PL1.
           IF IN-FIELD-2 = "FF"
            MOVE 0 TO FLAG
            DISPLAY PL-TAB(X) " " PL-NUM(X) " "
            PL-NAME(X) GO TO 4500-PL1-EXIT.
           IF PL-TAB(X) = IN-FIELD-1 DISPLAY PL-NAME(X)
            MOVE 0 TO FLAG 
            MOVE PLINDX TO X.
       4500-PL1-EXIT. EXIT.
       INPAT-1.
           IF (PL-TAB(X) = PLACE) 
            AND (PL-NUM(X) = 3) 
            MOVE 1 TO FLAG 
            MOVE PLINDX TO X.
       INPAT-1-EXIT. EXIT.

       2450-EPSDT.
           IF IN-FIELD = "?"
           DISPLAY "ESPDT AND FAMILY PLANNING FOR VT. MEDICAID"
           DISPLAY "1=BOTH  2=NEITHER  3=EPSDT  4=FAM.PL."
           GO TO 2000TI.
           IF IN-FIELD = SPACE MOVE "2" TO IN-FIELD-1
           DISPLAY "NEITHER".
           IF IN-FIELD-1 = "1" OR "2" OR "3" OR "4"
           MOVE IN-FIELD-1 TO CD-EPSDT GO TO 4900DEE.
           DISPLAY "INVALID" GO TO 2000TI.
       2190-DAT1.
           IF ACTION = "SDV" MOVE DAT1 TO PM-DATA GO TO SDV4.
           IF ACTION = "DF2" MOVE PM-DATA TO DAT1 GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE DAT1 TO TEST-DATE
           MOVE CORR TEST-DATE TO INPUT-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           MOVE INPUT-DATE TO IN-FIELD DISPLAY DISPLAY-DATE.
           IF IN-FIELD = "?"
           DISPLAY "ENTER ACCIDENT/SYMPTOM DATE   T = TODAY""S DATE"
           DISPLAY " OR DD OR MMDD OR MMDDYY OR MMDDYYYY"
           GO TO 2000TI.
           IF IN-FIELD = "00000000" OR SPACE MOVE ZEROES TO DAT1
           GO TO 4900DEE.
           IF IN-FIELD-8 = "T" ACCEPT DAT1 FROM DATE YYYYMMDD 
           GO TO 4900DEE.
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
           STRING IN-FIELD-4 "1999" DELIMITED BY "!!" INTO IN-FIELD-8
           DISPLAY "1999 ASSUMED".
           IF IN-FIELD-6 NUMERIC 
           AND IN-FIELD-TAB(7) = " " AND IN-FIELD-TAB(8) = " "           
           MOVE T-DATE TO TEST-DATE
           MOVE IN-FIELD-8 TO INPUT-DATE
           MOVE T-CC OF INPUT-DATE TO T-YY OF INPUT-DATE
           MOVE T-CC OF TEST-DATE TO T-CC OF INPUT-DATE
           DISPLAY T-CC OF INPUT-DATE T-YY OF INPUT-DATE " ASSUMED"
           MOVE INPUT-DATE TO IN-FIELD-8.
           IF IN-FIELD-8 NUMERIC NEXT SENTENCE
           ELSE DISPLAY "INVALID" GO TO 2000TI.
           MOVE IN-FIELD-8 TO INPUT-DATE.
           IF T-MM OF INPUT-DATE < 01 OR > 12
              DISPLAY "INVALID" GO TO 2000TI.
           IF T-DD OF INPUT-DATE > DAYS-IN-MONTH(T-MM OF INPUT-DATE)
               DISPLAY "INVALID" GO TO 2000TI.
           MOVE CORR INPUT-DATE TO TEST-DATE
           IF T-DATE < TEST-DATE DISPLAY " A FUTURE DATE" BELL0 
           GO TO 2000TI.
           IF TEST-DATE < "19990101" DISPLAY "VERY OLD" BELL0.
           MOVE TEST-DATE TO DAT1
           GO TO 4900DEE.
       2195-RESULT.
           IF IN-FIELD = "?"
           DISPLAY "ENTER MEDICAID RESULT CODE"
           DISPLAY "1 = NORMAL 2 = ABNORMAL"
           GO TO 2000TI.
           IF IN-FIELD-1 = "1" OR "2" MOVE IN-FIELD-1 TO RESULT
           GO TO 4900DEE.
           DISPLAY "INVALID" GO TO 2000TI.
       2196-ACTION.
           IF IN-FIELD = "?"
           DISPLAY "ENTER MEDICAID ACTION CODE"
           DISPLAY "1=APPT. MADE OFF SITE 2=CARE INSTIT."
           DISPLAY "3=NO APPT. MADE 4=ALREADY UNDER CARE"
           DISPLAY "5=REFER FOR TREAT. 6=DIAG/TREAT NOT AVAIL."
           DISPLAY "7=PATIENT REFUSED TREATMEMT"
           GO TO 2000TI.
           IF IN-FIELD-1 > "0" AND < "8" MOVE IN-FIELD-1 TO ACT
           GO TO 4900DEE.
           DISPLAY "INVALID" GO TO 2000TI.
       4000-DATE-T.
           IF ACTION = "SDV" MOVE DATE-T TO PM-DATA GO TO SDV4.
           IF ACTION = "DF2" MOVE PM-DATA TO DATE-T GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE DATE-T TO TEST-DATE
           MOVE CORR TEST-DATE TO INPUT-DATE 
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           MOVE INPUT-DATE TO IN-FIELD DISPLAY DISPLAY-DATE.
           IF IN-FIELD = "?"
           DISPLAY "DATE OF SERVICE    T = TODAY""S DATE"
           DISPLAY "OR DD OR MMDD OR MMDDYY OR MMDDYYYY FORMAT"
           GO TO 2000TI.
           IF IN-FIELD-8 = "T" ACCEPT DATE-T FROM DATE YYYYMMDD
           GO TO 4900DEE.
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
           STRING IN-FIELD-4 "1999" DELIMITED BY "!!" INTO IN-FIELD-8
           DISPLAY "1999 ASSUMED".
           IF IN-FIELD-6 NUMERIC 
           AND IN-FIELD-TAB(7) = " " AND IN-FIELD-TAB(8) = " "           
           MOVE T-DATE TO TEST-DATE
           MOVE IN-FIELD-8 TO INPUT-DATE
           MOVE T-CC OF INPUT-DATE TO T-YY OF INPUT-DATE
           MOVE T-CC OF TEST-DATE TO T-CC OF INPUT-DATE
           DISPLAY T-CC OF INPUT-DATE T-YY OF INPUT-DATE " ASSUMED"
           MOVE INPUT-DATE TO IN-FIELD-8.
           IF IN-FIELD-8 NUMERIC NEXT SENTENCE ELSE DISPLAY "INVALID"
           GO TO 2000TI.
           MOVE IN-FIELD-8 TO INPUT-DATE.
           IF T-MM OF INPUT-DATE < 01 OR > 12
           DISPLAY "INVALID" GO TO 2000TI.
           IF T-DD OF INPUT-DATE > DAYS-IN-MONTH(T-MM OF INPUT-DATE)
           DISPLAY "INVALID" GO TO 2000TI.
           MOVE CORR INPUT-DATE TO TEST-DATE
           IF TEST-DATE > T-DATE
           DISPLAY "A FUTURE DATE: INVALID" BELL0
           GO TO 2000TI.
           IF TEST-DATE < "19990101" DISPLAY "VERY OLD" BELL0.
           MOVE TEST-DATE TO DATE-T
             IF ACTION = "CC"
              IF ((DATE-T > "20150930") AND
                  (DIAG(6:2) = "??"
                  OR CD-DX2(6:2) = "??"
                  OR CD-DX3(6:2) = "??"
                  OR CD-DX4(6:2) = "??"))
               OR ((DATE-T < "20151001")
                 AND NOT
                     ((DIAG(6:2) = "??"  OR "00")
                 AND (CD-DX2(6:2) = "??" OR "00")
                 AND (CD-DX3(6:2) = "??" OR "00")
                 AND (CD-DX4(6:2) = "??" OR "00")))

               DISPLAY "DATE CONFLICT WITH DIAGNOSES"
               DISPLAY "CHANGE THE DATE TO MATCH DIAGNOSES"
               DISPLAY "OR CHANGE ALL THE DIAGNOSES"
               GO TO 1400-CHANGE-PROCESS
              END-IF
             END-IF
           GO TO 4900DEE.
       2-AUTH.
           IF IN-FIELD = "?"
           DISPLAY "1 = ADD A NEW AUTH. NUMBER"
           DISPLAY "2 = REVISE A AUTH. NUMBER"
           DISPLAY "3 = UNSET(BLANK OUT) AUTH. NUMBER"
           DISPLAY "4 = ADD A NEW NDC NUMBER"
           DISPLAY "5 = REVISE AN NDC NUMBER"
           DISPLAY "6 = UNSET(BLANK OUT) AN NDC NUMBER"
           DISPLAY "X  = NO ACTION TAKEN"
           GO TO 2000TI.
           IF IN-FIELD = "X" GO TO 4900DEE.
           IF NOT (IN-FIELD-2 = "1 " OR "2 " OR "3 " OR "4 "
                   OR "5 " OR "6 ")
               DISPLAY "INVALID"
               GO TO 2000TI.
           MOVE 0 TO FLAG 
      *     MOVE CD-AUTH TO ALF-1
           PERFORM AUTH-1 THRU AUTH-1-EXIT
           GO TO 4900DEE.
       2-MOD2.
           IF ACTION = "SDV" MOVE MOD2 TO PM-DATA GO TO SDV4.
           IF ACTION = "DF2" MOVE PM-DATA TO MOD2 GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE MOD2 TO IN-FIELD DISPLAY MOD2.
           IF IN-FIELD = "?"
           DISPLAY "TYPE THE 2ND MODIFIER OR <CR>"
           GO TO 2000TI.
           MOVE IN-FIELD-2 TO ALFX-2
           IF ALFX-21 = SPACE
           OR ALFX-22 = SPACE
           MOVE SPACE TO IN-FIELD-2.
           MOVE IN-FIELD-2 TO MOD2 GO TO 4900DEE.
       2-MOD3.
           IF ACTION = "SDV" MOVE MOD3 TO PM-DATA GO TO SDV4.
           IF ACTION = "DF2" MOVE PM-DATA TO MOD3 GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE MOD3 TO IN-FIELD DISPLAY MOD3.
           IF IN-FIELD = "?"
           DISPLAY "TYPE THE 3RD MODIFIER OR <CR>"
           GO TO 2000TI.
           MOVE IN-FIELD-2 TO ALFX-2
           IF ALFX-21 = SPACE
           OR ALFX-22 = SPACE
           MOVE SPACE TO IN-FIELD-2.
           MOVE IN-FIELD-2 TO MOD3 GO TO 4900DEE.
       2-XXXX.
           IF IN-FIELD = "?"
           DISPLAY "NOT A DEFINE FIELD" GO TO 2000TI.
       2-ASSIGN.
           IF ACTION = "SDV" MOVE CD-ASSIGN TO PM-DATA GO TO SDV4.
      *    IF ACTION = "DF2" MOVE PM-DATA TO CD-ASSIGN GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE CD-ASSIGN TO IN-FIELD
           DISPLAY CD-ASSIGN.
           IF IN-FIELD = "?"
           DISPLAY "A=ASSIGNED U= UNASSIGNED"
           GO TO 2000TI.
           IF IN-FIELD = "A" OR "U"
           MOVE IN-FIELD-1 TO CD-ASSIGN GO TO 4900DEE.
           DISPLAY "INVALID" GO TO 2000TI.
       2-NEIC-ASSIGN.
           IF ACTION = "SDV" MOVE CD-NEIC-ASSIGN TO PM-DATA GO TO SDV4.
      *    IF ACTION = "DF2" MOVE PM-DATA TO CD-NEIC-ASSIGN
      *    GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE CD-NEIC-ASSIGN TO IN-FIELD
           DISPLAY CD-NEIC-ASSIGN.
           IF IN-FIELD = "?"
           DISPLAY "A=ASSIGNED U=UNASSIGNED, ASSIGNMENT FOR CLAIM"
           GO TO 2000TI.
           IF IN-FIELD = "A" OR "U"
           MOVE IN-FIELD-1 TO CD-NEIC-ASSIGN GO TO 4900DEE.
           DISPLAY "INVALID" GO TO 2000TI.
       2-DATE-A.
           IF ACTION = "SDV" MOVE CD-DATE-A TO PM-DATA GO TO SDV4.
           IF ACTION = "DF2" MOVE PM-DATA TO CD-DATE-A GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE CD-DATE-A TO  TEST-DATE
           MOVE CORR TEST-DATE TO INPUT-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           MOVE INPUT-DATE TO IN-FIELD DISPLAY DISPLAY-DATE.
           IF IN-FIELD = "?"
           DISPLAY "ENTER THE DATE THE CLAIM WAS SENT MMDDYYYY FORMAT"
           DISPLAY "T = TODAY""S DATE"
           GO TO 2000TI.
           IF IN-FIELD-8 = "T" ACCEPT CD-DATE-A FROM DATE YYYYMMDD
           GO TO 4900DEE.
           IF IN-FIELD-8 NUMERIC NEXT SENTENCE ELSE DISPLAY "INVALID"
           GO TO 2000TI.
           MOVE IN-FIELD-8 TO INPUT-DATE.
           IF T-MM OF INPUT-DATE < 01 OR > 12
           DISPLAY "INVALID" GO TO 2000TI.
           IF T-DD OF INPUT-DATE > DAYS-IN-MONTH(T-MM OF INPUT-DATE)
           DISPLAY "INVALID" GO TO 2000TI.
           MOVE CORR INPUT-DATE TO TEST-DATE
           MOVE TEST-DATE TO CD-DATE-A.
           IF DATE-T > CD-DATE-A DISPLAY "UNLIKELY" GO TO 2000TI.
           GO TO 4900DEE.
       2-COLLT.
           IF ACTION = "SDV" MOVE CD-COLLT TO PM-DATA GO TO SDV4.
           IF ACTION = "DF2" MOVE PM-DATA TO CD-COLLT GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE CD-COLLT TO IN-FIELD DISPLAY CD-COLLT.
           IF IN-FIELD = "?"
           DISPLAY "1-9 TO DEFINE A COLLECTION AGENCY THIS CHARGE"
           DISPLAY "IS GOING TO BE SENT TO"
           DISPLAY " 0 = NOT IN COLLECTION"
           GO TO 2000TI.
           IF IN-FIELD-1 NOT NUMERIC DISPLAY "INVALID" GO TO 2000TI.
           MOVE IN-FIELD-1 TO CD-COLLT.
           GO TO 4900DEE.
       2-ACC-TYPE.
           IF ACTION = "SDV" MOVE CD-ACC-TYPE TO PM-DATA GO TO SDV4.
           IF ACTION = "DF2" MOVE PM-DATA TO CD-ACC-TYPE GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE CD-ACC-TYPE TO IN-FIELD
           DISPLAY CD-ACC-TYPE.
           IF IN-FIELD = "?"
           DISPLAY "<CR> = NOT AN ACCIDENT 1=EMPLOYM. 2=CAR 3=OTHER"
           GO TO 2000TI.
           IF IN-FIELD = SPACE OR "1" OR "2" OR "3" MOVE IN-FIELD-1
           TO CD-ACC-TYPE GO TO 4900DEE.
           DISPLAY "INVALID" GO TO 2000TI.
       2-ADMIT.
           IF ACTION = "SDV" MOVE CD-DATE-M TO PM-DATA GO TO SDV4.
           IF ACTION = "DF2" MOVE PM-DATA TO CD-DATE-M GO TO 4900DEE.
           IF IN-FIELD = "/" MOVE CD-DATE-M TO TEST-DATE
           MOVE CORR TEST-DATE TO INPUT-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           MOVE INPUT-DATE TO IN-FIELD DISPLAY DISPLAY-DATE.
           IF IN-FIELD = "?" 
           DISPLAY "ENTER HOSPITAL ADMIT DATE"
           DISPLAY "DD OR MMDD OR MMDDYY OR MMDDYYYY FORMAT"
           GO TO 2000TI.
           IF IN-FIELD-8 = SPACE MOVE "00000000" TO CD-DATE-M
           GO TO 4900DEE.
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
           STRING IN-FIELD-4 "1999" DELIMITED BY "!!" INTO IN-FIELD-8
           DISPLAY "1999 ASSUMED".
           IF IN-FIELD-6 NUMERIC 
           AND IN-FIELD-TAB(7) = " " AND IN-FIELD-TAB(8) = " "           
           MOVE T-DATE TO TEST-DATE
           MOVE IN-FIELD-8 TO INPUT-DATE
           MOVE T-CC OF INPUT-DATE TO T-YY OF INPUT-DATE
           MOVE T-CC OF TEST-DATE TO T-CC OF INPUT-DATE
           DISPLAY T-CC OF INPUT-DATE T-YY OF INPUT-DATE " ASSUMED"
           MOVE INPUT-DATE TO IN-FIELD-8.
           IF IN-FIELD-8 NUMERIC NEXT SENTENCE ELSE DISPLAY "INVALID"
           GO TO 2000TI.
           MOVE IN-FIELD-8 TO INPUT-DATE.
           IF T-MM OF INPUT-DATE < 01 OR > 12
           DISPLAY "INVALID" GO TO 2000TI.
           IF T-DD OF INPUT-DATE > DAYS-IN-MONTH(T-MM OF INPUT-DATE)
           DISPLAY "INVALID" GO TO 2000TI.
           MOVE CORR INPUT-DATE TO TEST-DATE
           IF T-DATE < TEST-DATE DISPLAY "FUTURE DATE" BELL0 
           GO TO 2000TI.
           IF TEST-DATE < "19990101" DISPLAY "VERY OLD" BELL0.
           MOVE TEST-DATE TO CD-DATE-M.
           GO TO 4900DEE.
       4900DEE.
           IF ACTION = "DF2" OR "DF" MOVE 1 TO DF-TAB(INDX)
           MOVE CHARFILE01 TO QQQ.
           IF ACTION = "DF2" GO TO PM1.
           IF ACTION = "SDV" GO TO SDV4.
           IF ACTION = "DF" GO TO 1000-ACTION.
           IF ACTION = "C" GO TO 1400-CHANGE-PROCESS.
           SET LAST-ADD-KEY TO ADD-KEY
           IF ACTION = "A" AND INDX = 30 NEXT SENTENCE
           ELSE GO TO 4910DEE.
           IF ( G-PRINS = "004" OR G-SEINS = "004" OR
           G-PRINS = "064" OR G-SEINS = "064") AND
           ( PROC1 = "99391" OR "99392" OR "99393" OR "99394"
           OR "99395" OR "99381" OR "99382" OR "99383" OR "99384"
           OR "99385")
           GO TO 4910DEE.
           SET ADD-KEY UP BY 2.
       4910DEE.
           EXIT.
       LR1.
           MOVE AMOUNT TO NEF-8
           MOVE DAT1 TO TEST-DATE MOVE CORR TEST-DATE
           TO DISPLAY-DATE
           MOVE DATE-T TO TEST-DATE MOVE CORR TEST-DATE
           TO DISPLAY-DATE-TOO
           
           DISPLAY "PATIENT NAME: " NAME
           DISPLAY CHAR-KEY " " PATID " " CLAIM " " PLACE "  "
           PROC " " MOD2 "   " MOD3  NEF-8
           DISPLAY
           "KEY         PATIENT  CLAIM  PL PROC    MOD2 MOD3   $$$"
           DISPLAY " "
           DISPLAY DIAG " " CD-DX2 " " CD-DX3 " " CD-DX4
      *     " " CD-DX5  " " CD-DX6
           DISPLAY "DX1     DX2     DX3     DX4"
      *          DX5     DX6"
           DISPLAY " "
           DISPLAY SERVICE "    " DOCP "    " DOCR "   " WORK "    "
           DISPLAY-DATE " "  CD-ACC-TYPE "    " DISPLAY-DATE-TOO " "
           PAYCODE "  " RESULT "    " ACT "   " CD-EPSDT "     "
           CD-SORCREF
           DISPLAY "TYPE PRVD  REF   UNIT     ACCDT  ACCTP    DATE    IN
      -    "S  RSLT ACT EPSDT SOCR"
           DISPLAY " "
           MOVE CD-DATE-A TO TEST-DATE MOVE CORR TEST-DATE
           TO DISPLAY-DATE
           MOVE CD-DATE-M TO TEST-DATE MOVE CORR TEST-DATE
           TO DISPLAY-DATE-TOO
           
           DISPLAY CD-ASSIGN "    " CD-NEIC-ASSIGN "        "
           CD-COLLT "      "
           PAPER "      " STAT "       " DISPLAY-DATE
           "    " CD-AUTH "    " DISPLAY-DATE-TOO

           DISPLAY "ASGN CLM-ASGN COLLT  PAPER  STATUS   CLM-DATE  AUT
      -    "   ADMIT-DT"
           MOVE DATE-E TO TEST-DATE MOVE CORR TEST-DATE
           TO DISPLAY-DATE
           
           DISPLAY "RECORD ENTERED: " DISPLAY-DATE
           IF CD-AUTH = "1" PERFORM READ-AUTH
            IF AUTH-NUM NOT = SPACE
             DISPLAY "AUTHNUM = " AUTH-NUM
            END-IF
            IF AUTH-NDC NOT = SPACE
             DISPLAY "NDCNUM = " AUTH-NDC
            END-IF
           END-IF.
       5000-WRITE-CHARFILE.
           MOVE CHAR-KEY TO G-GARNO
           READ GARFILE INVALID DISPLAY "BAD GARNO" GO TO 1000-ACTION.
           IF PAYCODE NOT = "001" AND PAYCODE NOT = G-PRINS
           AND PAYCODE NOT = G-SEINS AND PAYCODE NOT = G-TRINS
           DISPLAY "INSURANCE CODE ON THIS RECORD DOES NOT MATCH"
           DISPLAY "ANY INSURANCE CODE FOR THE ACCOUNT"
           DISPLAY "THIS MAY NOT BE VALID.  THIS IS ONLY A WARNING".
           PERFORM DX-1 THRU DX-1-EXIT.
           PERFORM DXMOD-1
           REWRITE CHARFILE01 INVALID KEY DISPLAY "NO UPDATE."
           DISPLAY "THIS SHOULD NOT HAPPEN! CONTACT THE DATA CENTER."
           DISPLAY "THIS PROGRAM IS TERMINATED!"
           GO TO 9100-CLOSE-MASTER-FILE.
           UNLOCK CHARFILE RECORD.
      *    CLOSE CHARFILE. OPEN I-O CHARFILE.
           MOVE QQQ TO CHARFILE01.
           DISPLAY "UPDATE MADE".
           IF ACTION = "CC" MOVE "A" TO ACTION GO TO M2.
           GO TO 1000-ACTION.
       FA-1. IF CHAR-KEY = "1" OR "2" OR "3" OR "4" OR "5" OR "6"
           OR "7" OR "8" OR "9" MOVE CHAR-KEY TO IN-FIELD-1
           MOVE IN-FIELD-1 TO FLAG MOVE GAR-TAB(FLAG) TO CHAR-KEY.
           MOVE CHAR-KEY TO G-GARNO
           READ GARFILE INVALID DISPLAY "INVALID" GO TO FA-1-EXIT.
           DISPLAY G-GARNO " " G-PRINS "/" G-SEINS "/" G-TRINS 
           " " G-GARNAME
           MOVE G-DOB TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           INSPECT T-MM OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           INSPECT T-DD OF DISPLAY-DATE REPLACING LEADING "0" BY " "

           DISPLAY DISPLAY-DATE " " G-PRIPOL " " G-SECPOL
           MOVE G-GARNO TO P-GARNO
           START PATFILE KEY NOT < P-GARNO INVALID GO TO FA-1-EXIT.
       FA-2. READ PATFILE NEXT AT END GO TO FA-1-EXIT.
           IF P-GARNO NOT = G-GARNO GO TO FA-1-EXIT.
           MOVE P-DOB TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           INSPECT T-MM OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           INSPECT T-DD OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           DISPLAY P-PATNO " " DISPLAY-DATE " " P-SEX " " P-PATNAME
           GO TO FA-2.
       FA-1-EXIT. EXIT.
       LI-1.
           READ INSFILE INVALID DISPLAY "INVALID" GO TO LI-1-EXIT.
           DISPLAY INS-KEY " " INS-NAME " " INS-STREET " " INS-CITY
           " " INS-STATE " " INS-ZIP
           DISPLAY INS-ASSIGN "    " INS-CLAIMTYPE "   " INS-NEIC
           "    " INS-NEICLEVEL " " INS-NEIC-ASSIGN "    " INS-PPO
           "  " INS-PRVNUM "    " INS-HMO " " INS-STATUS " "
           INS-LASTDATE "   "    INS-CAID "     "  INS-REFWARN
           DISPLAY "ASGM TYP NEIC NCLVL NCSM PPO PPONUM    HMO STAT LAST
      -    " DATE VTCAID# REFPRMT".
       LI-1-EXIT. EXIT.
       CD-1. IF DC = 0 MOVE 1 TO DC DISPLAY "CHARGE LIST"
           ELSE MOVE 0 TO DC DISPLAY "NO CHARGE LIST AFTER ADD".

       DD-1. IF DD = 0 MOVE 1 TO DD DISPLAY "DEFAULT LIST"
           ELSE MOVE 0 TO DD DISPLAY "NO DEFAULT LIST AFTER ADD".
       PH-1. IF HP = 0 MOVE 1 TO HP DISPLAY "PHYS. NAME DISPLAY"
           ELSE MOVE 0 TO HP DISPLAY "NO PHYS. NAME DISPLAY".
       CP-1. IF CO-PAY-FLAG = 0 MOVE 1 TO CO-PAY-FLAG 
           DISPLAY "NO CO-PAY PROMPT"
           ELSE MOVE 0 TO CO-PAY-FLAG 
           DISPLAY "CO-PAY PROMPT AFTER ADD".

       SDV1. MOVE SPACE TO PM-KEY8
           MOVE "DF2" TO PM-KEY8
           MOVE SPACE TO PM-KEY3
           MOVE 3 TO X
           MOVE "SDV" TO ACTION.
       SDV2. ADD 1 TO X IF X > 34 GO TO 1000-ACTION.
           IF X = 6 OR 7 OR 10 OR 14 OR 16 OR 17 OR 19 OR 20
           OR 21 OR 31 OR 32 OR 33 GO TO SDV2.
           COMPUTE NUM-3 = X
           MOVE NUM-3 TO PM-KEY3
           READ PARMNDEX WITH LOCK INVALID GO TO SDV3.
           IF DF-TAB(X) = 0 DELETE PARMNDEX RECORD
           GO TO SDV2.
           UNLOCK PARMNDEX RECORD.
       SDV3.
      *    DISPLAY PARMNDEX01 " " PARMNDEX-STAT
           IF DF-TAB(X) = 0 GO TO SDV2.
           SET INDX TO X
           MOVE SPACE TO PM-DATA.
           GO TO 2061-GO-TO.
       SDV4. READ PARMNDEX WITH LOCK INVALID WRITE PARMNDEX01
           UNLOCK PARMNDEX RECORD GO TO SDV2.
           REWRITE PARMNDEX01 INVALID DISPLAY "BAD " PARMNDEX01.
           UNLOCK PARMNDEX RECORD
           GO TO SDV2.
       READ-AUTH.
           MOVE CD-KEY8 TO AUTH-KEY8
           MOVE CLAIM TO AUTH-KEY6
           READ AUTHFILE INVALID MOVE "1" TO FLAG
           MOVE SPACE TO AUTH-NUM AUTH-NDC.
      *     IF AUTH-NUM = SPACE AND FLAG = 0 MOVE 1 TO FLAG.
      *     IF AUTH-NUM NOT = SPACE AND FLAG = 0 MOVE 2 TO FLAG.
       AUTH-1.
           MOVE CD-KEY8 TO AUTH-KEY8
           MOVE CLAIM TO AUTH-KEY6
           READ AUTHFILE WITH LOCK INVALID GO TO AUTH-4.
           IF IN-FIELD-2 = "1 " OR "2 " OR "3 "
            DISPLAY "AUTH NUMBER = " AUTH-NUM
            GO TO AUTH-2
           END-IF
           IF IN-FIELD-2 = "4 " OR "5 " OR "6 "
            DISPLAY "NDC NUMBER = " AUTH-NDC
            GO TO AUTH-3
           END-IF.
           GO TO AUTH-1-EXIT.
       AUTH-2.
           DISPLAY "AUTHORIZATION NUMBER"
           ACCEPT ALF-15
           IF ALF-15 = "?" 
            DISPLAY "ENTER THE AUTH. NUMBER OR <CR> TO UNSET IT "
            DISPLAY "BK OR X  = NO CHANGE"
            GO TO AUTH-2
           END-IF
           IF ALF-15 = "BK" OR "X" 
            DISPLAY "PRIOR AUTH. NUMBER NOT CHANGED"
            GO TO AUTH-1-EXIT
           END-IF
           MOVE "1" TO CD-AUTH
           MOVE ALF-15 TO AUTH-NUM
           REWRITE AUTHFILE01
           GO TO AUTH-1-EXIT.
       AUTH-3.
           DISPLAY "NDC NUMBER"
           ACCEPT ALF-11
           IF ALF-11 = "?" 
           DISPLAY "ENTER THE 11-DIGIT NDC NUMBER"
           DISPLAY "AFFIX A LEADING ZERO IF ONLY 10-DIGITS LONG"
           DISPLAY "OR A <CR> TO UNSET(BLANK OUT)" 
           DISPLAY "BK OR X  = NO CHANGE"
           GO TO AUTH-3.
           IF ALF-11 = "BK" OR "X"   
           DISPLAY "PRIOR NDC NUMBER NOT CHANGED"
           GO TO AUTH-1-EXIT.
           IF ALF-11 NOT NUMERIC DISPLAY "INVALID" GO TO AUTH-3.
           MOVE ALF-11 TO AUTH-NDC
           MOVE "1" TO CD-AUTH
           REWRITE AUTHFILE01
           GO TO AUTH-1-EXIT.
       AUTH-4. 
           IF IN-FIELD-2 = "2 " OR "3 "
            DISPLAY "AUTHORIZATION # MISSING!"
            GO TO AUTH-1-EXIT
           END-IF
           IF IN-FIELD-2 = "5 " OR "6 "
            DISPLAY "NDC # MISSING!"
            GO TO AUTH-1-EXIT
           END-IF
           IF IN-FIELD-2 = "5 " OR "6 " GO TO AUTH-6.
       AUTH-5.
           DISPLAY "AUTHORIZATION NUMBER"
           ACCEPT ALF-15
           IF ALF-15 = "?" 
           DISPLAY "ENTER THE AUTH. NUMBER OR <CR> TO UNSET IT "
           DISPLAY "BK OR X  = NO CHANGE"
           GO TO AUTH-2.
           IF ALF-15 = "BK" OR "X" 
           DISPLAY "PRIOR AUTH. NUMBER NOT CHANGED"
           GO TO AUTH-1-EXIT.
           MOVE "1" TO CD-AUTH
           MOVE ALF-15 TO AUTH-NUM
           MOVE "01" TO AUTH-QNTY
           MOVE SPACE TO AUTH-NDC
           MOVE SPACE TO AUTH-FILLER
           ACCEPT AUTH-DATE-E FROM DATE YYYYMMDD
           WRITE AUTHFILE01 
           GO TO AUTH-1-EXIT.
       AUTH-6.
           DISPLAY "NDC NUMBER"
           ACCEPT ALF-11
           IF ALF-11 = "?" 
            DISPLAY "ENTER THE 11-DIGIT NDC NUMBER"
            DISPLAY "AFFIX A LEADING ZERO IF ONLY 10-DIGITS LONG"
            DISPLAY "OR A <CR> TO UNSET(BLANK OUT)" 
            DISPLAY "BK OR X  = NO CHANGE"
           GO TO AUTH-3.
           IF ALF-11 = "BK" OR "X" 
            DISPLAY "PRIOR NDC NUMBER NOT CHANGED"
           GO TO AUTH-1-EXIT.
           IF ALF-11 NOT NUMERIC DISPLAY "INVALID"
            GO TO AUTH-3.
           MOVE ALF-11 TO AUTH-NDC
           MOVE "1" TO CD-AUTH
           MOVE SPACE TO AUTH-QNTY
           MOVE SPACE TO AUTH-FILLER
           MOVE SPACE TO AUTH-NUM
           ACCEPT AUTH-DATE-E FROM DATE YYYYMMDD
           WRITE AUTHFILE01. 

       AUTH-1-EXIT. EXIT.
       DX-1.
           IF DIAG = "0000000"
             AND CD-DX2 = "0000000" AND CD-DX3 = "0000000"
             AND CD-DX4 = "0000000"
      *       AND CD-DX5 = "0000000" AND CD-DX6 = "0000000"
              DISPLAY "ALERT !!  NO DIAGS PRESENT ON CHARGE." 
              ACCEPT ANS
              GO TO DX-1-EXIT
           END-IF

           MOVE SPACE TO TABDX01
           STRING DIAG CD-DX2 CD-DX3 CD-DX4
      *     CD-DX5 CD-DX6
           DELIMITED BY SIZE INTO TABDX01
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 3
           COMPUTE Z = X + 1
            PERFORM VARYING Y FROM Z BY 1 UNTIL Y > 4
             IF X NOT = Y
              IF TABDX(X) = "0000000" 
                MOVE TABDX(Y) TO TABDX(X)
                MOVE "0000000" TO TABDX(Y)
                 IF TABDX(X) NOT = "0000000"
                   MOVE 6 TO Y
                 END-IF
              END-IF
             END-IF
            END-PERFORM
           END-PERFORM
           MOVE TABDX(1) TO DIAG
           MOVE TABDX(2) TO CD-DX2
           MOVE TABDX(3) TO CD-DX3
           MOVE TABDX(4) TO CD-DX4.
      *     MOVE TABDX(5) TO CD-DX5
      *     MOVE TABDX(6) TO CD-DX6.
       DX-1-EXIT. EXIT.

       dxmod-1.
           IF (PROC2 NOT = SPACE)
             IF PROC2 = MOD2
             MOVE MOD3 TO MOD2
             MOVE SPACE TO MOD3
             END-IF
           END-IF
           IF (PROC2 NOT = SPACE)
             IF PROC2 = MOD2
             MOVE MOD3 TO MOD2
             MOVE SPACE TO MOD3
             END-IF
           END-IF
           IF (MOD2 = MOD3)
           AND (MOD2 NOT = SPACE)
           MOVE SPACE TO MOD3
           END-IF
           
           IF (proc2 = MOD3)
           MOVE SPACE TO MOD3
           END-IF
           IF (mod2 = space)
           and (MOD3 not = space)
           move mod3 to mod2
           MOVE SPACE TO MOD3
           END-IF.

       CD10.
           IF IN-FIELD = "F" GO TO 1DIAG-SEARCH.
           IF IN-FIELD = "M" GO TO 1MAP.
           IF (IN-FIELD-TAB(1) NUMERIC) OR (IN-FIELD-TAB(1) = "V")
            MOVE SPACE TO ALF-7
            STRING IN-FIELD-7(1:5) "??" DELIMITED BY SIZE INTO ALF-7
            MOVE ALF-7 TO IN-FIELD-7
           END-IF.
           MOVE IN-FIELD-7 TO DIAG-KEY.
           READ DIAGFILE INVALID DISPLAY "NOT ON FILE"
            MOVE 1 TO RETURN-FLAG
           END-READ
           IF (DATE-T < "20151001" AND DIAG-KEY(6:2) NOT = "??")
            DISPLAY "USE ICD9 CODE WITH THIS DATE"
            MOVE 1 TO RETURN-FLAG
           GO TO CD10-EXIT.
           IF (DATE-T > "20150930" AND DIAG-KEY(6:2) = "??")
            DISPLAY "USE ICD10 CODE WITH THIS DATE"
            MOVE 1 TO RETURN-FLAG
           GO TO CD10-EXIT.
           DISPLAY diag-TITLE
           GO TO CD10-EXIT.


       1DIAG-SEARCH.
           MOVE 1 TO FLAG
           IF DATE-T < "20151001" MOVE 9 TO FLAG.
           DISPLAY "SEARCH KEY ?".
           ACCEPT DIAG-TITLE.
           IF DIAG-TITLE = "?"
           DISPLAY "ICD9 BY TITLE, TYPE AT LEAST 1ST 2 LETTERS"
           DISPLAY "ICD9 BY CODE, TYPE AT LEAST 1ST 2 NUMBERS"
           DISPLAY "ICD9 V CODES, TYPE V AND THEN AT LEAST 1 #"
           DISPLAY " "
           DISPLAY "ICD10 BY TITLE, TYPE AT LEAST ONE LETTER"
           DISPLAY "ICD10 BY CODE, TYPE A LETTER THEN AT LEAST 1 #"
           GO TO 1DIAG-SEARCH
           END-IF.
           IF DIAG-TITLE = SPACE
           MOVE 1 TO RETURN-FLAG

           GO TO CD10-EXIT.

           IF (FLAG = 9)
             IF
              ((DIAG-TITLE(1:1) = "V") AND (DIAG-TITLE(2:1) NUMERIC))
              OR (DIAG-TITLE(1:1) NUMERIC)
                MOVE DIAG-Title(1:5) TO DIAG-KEY
                GO TO 4DIAG
             END-IF
           END-IF

           IF FLAG = 1

               IF (DIAG-TITLE(1:1) ALPHABETIC)
                 AND
                  (DIAG-TITLE(2:1) NUMERIC)
                    MOVE DIAG-Title(1:5) TO DIAG-KEY
                    GO TO 4DIAG
               END-IF
           END-IF

           IF (FLAG NOT = 9)
             AND (DIAG-TITLE(1:1) ALPHABETIC)
             AND (DIAG-TITLE(2:1) NUMERIC)
              MOVE DIAG-title(1:7) TO DIAG-KEY
              GO TO 4DIAG
           END-IF.

           START DIAGFILE KEY NOT < DIAG-TITLE INVALID
           DISPLAY "END OF FILE"
           MOVE 1 TO RETURN-FLAG
           GO TO CD10-EXIT.
           MOVE 0 TO X.
           GO TO 3DIAG.
       4DIAG.
           START DIAGFILE KEY NOT < DIAG-KEY INVALID
           DISPLAY "END OF FILE"
           MOVE 1 TO RETURN-FLAG
           GO TO CD10-EXIT.
           MOVE 0 TO X.
       3DIAG.
           READ DIAGFILE NEXT AT END
           DISPLAY "END OF FILE"
           GO TO 3DIAG-0.
            IF FLAG = 9 AND DIAG-KEY >= "V9199??"
             DISPLAY "END OF FILE"
             GO TO 3DIAG-0
            END-IF
           IF ((FLAG = 9) AND (DIAG-KEY(6:2) not = "??"))
             OR ((FLAG = 1) AND(DIAG-KEY(6:2)    = "??"))
             GO TO 3DIAG
           END-IF.
           ADD 1 TO X
           MOVE X TO NEF-2
             DISPLAY NEF-2 " " DIAG-KEY " " DIAG-MEDB " " DIAG-TITLE
           MOVE DIAG-KEY TO TAGTAB(X) 

           IF X < 20 GO TO 3DIAG.
       3DIAG-0.
            DISPLAY "SELECT ?"
            ACCEPT ANS.
             IF ANS = "?"
               DISPLAY "PICK A NUMBER,X TO QUIT,ENTER FOR MORE"
               GO TO 3DIAG-0
             END-IF
             IF ANS = SPACE
             MOVE 0 TO X GO TO 3DIAG
             END-IF
           IF ANS = "X"
            MOVE 1 TO RETURN-FLAG
            GO TO CD10-EXIT.
           MOVE ANS(1:2) TO ALF-2
           MOVE SPACE TO RIGHT-2
           UNSTRING ALF-2 DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING LEADING " " BY "0"
           MOVE RIGHT-2 TO ALF-2
           IF ALF-2 NUMERIC
             MOVE ALF-2 TO NUM-2
             IF (NUM-2 > 0) AND (NUM-2 <= X)
              MOVE TAGTAB(NUM-2) TO IN-FIELD-7
              move 0 to RETURN-flag
              go to CD10-exit
             END-IF
           END-IF.
           MOVE 0 TO X
           GO TO 1DIAG-SEARCH.



       1MAP.
           DISPLAY "ENTER A VALID ICD9 CODE OR X TO QUIT."
           ACCEPT ALF-5
           IF ALF-5 = "X" GO TO 9MAP.
           MOVE ALF-5 TO TAG-ICD9-5
           MOVE SPACE TO TAG-ICD9-7
           START TAGDIAG KEY NOT < TAG-ICD9
             INVALID  GO TO 9MAP
           END-START.
           MOVE 0 TO Y.
       2MAP.
           READ TAGDIAG NEXT AT END GO TO 4MAP.
           IF TAG-ICD9-5 NOT = ALF-5 GO TO 4MAP.
           ADD 1 TO Y
           MOVE TAG-ICD9-7 TO TAGTAB(Y)
           MOVE TAG-ICD9-7 TO DIAG-KEY
           READ DIAGFILE INVALID GO TO 2MAP.
           MOVE Y TO NEF-2

           DISPLAY NEF-2 " " TAG-ICD9-7 " " DIAG-TITLE.
       3MAP.
           IF Y < 20 GO TO 2MAP.
       4MAP.
           IF Y = 0
           GO TO 1MAP
           END-IF
           DISPLAY "CHOOSE FROM THE LIST".
           IF Y = 20
            DISPLAY " OR CONTINUE LOOKING"
           END-IF
           ACCEPT ALF-2.

           IF ALF-2 = "?"
           DISPLAY "A = USE A DIFFERENT ICD9 CODE"
           DISPLAY "X = STOP THE MAPPING"
           DISPLAY "PICK A NUMBER FROM THE LIST"
           DISPLAY " TO MAKE A SELECTION"
           DISPLAY "ENTER KEY TO CONTINUE THE MAPPING"
           GO TO 4MAP.
           IF ALF-2 = "A" GO TO 1MAP.
           IF ALF-2 = "X" GO TO 9MAP.
           IF ALF-2 = SPACE MOVE 0 TO Y GO TO 2MAP.
           MOVE SPACE TO RIGHT-2
           UNSTRING ALF-2 DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING LEADING " " BY "0"
           MOVE RIGHT-2 TO ALF-2
           IF ALF-2 NUMERIC
             MOVE ALF-2 TO NUM-2
             IF (NUM-2 > 0) AND (NUM-2 <= Y)
              MOVE TAGTAB(NUM-2) TO IN-FIELD-7
              IF DATE-T < "20151001"
               DISPLAY "MUST USE ICD9 CODE FOR THIS DATE"
               DISPLAY DATE-T(5:2) "-" DATE-T(7:2) "-" DATE-T(1:4)
               ACCEPT ANS
               MOVE 1 TO RETURN-FLAG
              END-IF
              MOVE IN-FIELD-7 TO DIAG-KEY
              READ DIAGFILE INVALID CONTINUE
              END-READ
              DISPLAY DIAG-TITLE
              GO TO CD10-EXIT
             END-IF
           END-IF
            GO TO 4MAP.
       9MAP.
           MOVE 1 TO RETURN-flag.

       CD10-EXIT.
           EXIT.

       9100-CLOSE-MASTER-FILE.
           CLOSE CLAIMFILE CHARFILE AUTHFILE GARFILE PATFILE INSFILE.
           CLOSE PROCFILE REFPHY PARMNDEX DOCPARM DIAGFILE MPLRFILE
           fileout GAPFILE PAYFILE TAGDIAG.
           DISPLAY "CHARGE TRANSACTION MAINTENANCE PROGRAM HAS ENDED".
           EXIT PROGRAM.

