      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ina004.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TAGDIAG ASSIGN TO "S20" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS TAG-KEY
           ALTERNATE RECORD KEY IS TAG-ICD9 WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT INSFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT PAYFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYFILE-KEY
           LOCK MODE MANUAL
           STATUS IS PAYFILE-STAT.
           SELECT PAYCUR ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.
           SELECT GARFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC  RECORD KEY IS G-GARNO
           LOCK MODE MANUAL.
           SELECT PATFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS P-PATNO
           ALTERNATE RECORD KEY IS P-GARNO WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT CHARCUR ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL
           STATUS IS CHARCUR-STAT.
           SELECT CHARFILE ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS CHARFILE-KEY
           LOCK MODE MANUAL.
           SELECT CMNTFILE ASSIGN TO "S65" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS CMNT-KEY
           LOCK MODE MANUAL.
           SELECT DIAGFILE ASSIGN TO "S70" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS DIAG-KEY
           ALTERNATE RECORD KEY IS DIAG-TITLE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT PROCFILE ASSIGN TO "S75" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS PROC-KEY
           LOCK MODE MANUAL.
           SELECT REFPHY ASSIGN TO "S80" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS REF-KEY
           ALTERNATE RECORD KEY IS REF-BSNUM  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CRNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-UPIN  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CDNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-NAME  WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT DOCPARM ASSIGN TO "S85" ORGANIZATION IS
           LINE SEQUENTIAL.
           SELECT GAPFILE ASSIGN TO "S90" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS GAPKEY
           ALTERNATE RECORD KEY IS GAP-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-STATE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT AUTHFILE ASSIGN TO "S190" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS AUTH-KEY
           LOCK MODE MANUAL
           STATUS IS AUTH-STAT.
           SELECT MPLRFILE ASSIGN TO "S165" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS MPLR-KEY
           LOCK MODE IS MANUAL.
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
       FD  DOCPARM
           DATA RECORD IS DOCPARM01.
       01  DOCPARM01.
           02 DPX.
             03 DP-1-1 PIC X.
             03 DP-1-2 PIC X.
           02 DP-2 PIC X(22).
       FD  REFPHY
      *    BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS REFPHY01.
       01  REFPHY01.
           02 REF-KEY PIC XXX.
           02 REF-BSNUM PIC X(5).
           02 REF-CRNUM PIC X(6).
           02 REF-UPIN PIC X(6).
           02 REF-CDNUM PIC X(7).
           02 REF-NAME PIC X(24).
           02 REF-NPI PIC X(10).
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
           02 PROC-OLD PIC X(7).
           02 PROC-TYPE PIC X.
           02 PROC-BCBS PIC X(4).
           02 PROC-TITLE PIC X(28).
           02 PROC-AMOUNT PIC S9(4)V99.
           02 CARE-AMOUNT PIC S9(4)V99.
       FD  CMNTFILE
      *    BLOCK CONTAINS 2 RECORDS
           DATA RECORD IS CMNTFILE01.
       01  CMNTFILE01.
           02 CMNT-KEY.
             03 CM-KEY8 PIC X(8).
             03 CM-KEY3 PIC XXX.
           02 CMNT PIC X(78).
           02 CMNT2 PIC XX.
           02 CMNT-DATE-E PIC X(8).
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
       FD GARFILE
      *    BLOCK CONTAINS 3 RECORDS
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
           02 G-INSPEND PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.
       FD PATFILE
      *    BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS P-MASTER.
       01 P-MASTER.
           02 P-PATNO PIC X(8).
           02 P-GARNO PIC X(8).
           02 P-PATNAME PIC X(24).
           02 P-SEX PIC X.
           02 P-RELATE PIC X.
           02 P-MSTAT PIC X.
           02 P-DOB PIC X(8).
       WORKING-STORAGE SECTION.
       01 PAY-TAB01.
           02 PAY-TAB02 OCCURS 990 TIMES INDEXED BY PAY-IND.
               03 PAYC-TAB PIC X(6).
               03 PAYA-TAB PIC S9(4)V99.
               03 PAYD-TAB PIC X(8).
               03 PAYPC-TAB PIC XXX.
       01 S-TAB01.
           02 S-TAB02 OCCURS 990 TIMES INDEXED BY P-IND.
               03 C-TAB PIC X(6).
               03 A-TAB PIC S9(4)V99.
             03 D-TAB PIC X(8).
             03 PC-TAB PIC XXX.
             03 DN-TAB PIC XX.
             03 DE-TAB PIC X(8).
       01  TEST-DATE.
           05  T-CC            PIC XX.
           05  T-YY            PIC XX.
           05  T-MM            PIC XX.
           05  T-DD            PIC XX.
       01  TEST-DATE1.
           05  T-CC            PIC XX.
           05  T-YY            PIC XX.
           05  T-MM            PIC XX.
           05  T-DD            PIC XX.

       01  DISPLAY-DATE-CD PIC X(10).
       01  DISPLAY-DATE-CC PIC X(10).
       01  DISPLAY-DATE.
           02 T-MM PIC XX.
           02 FILLER PIC X VALUE "/".
           02 T-DD PIC XX.
           02 FILLER PIC X VALUE "/".
           02 T-CC PIC XX.
           02 T-YY PIC XX.
       01  DISPLAY-DATE-TOO.
           02 T-MM PIC XX.
           02 FILLER PIC X VALUE "/".
           02 T-DD PIC XX.
           02 FILLER PIC X VALUE "/".
           02 T-CC PIC XX.
           02 T-YY PIC XX.
       01  HOLDASSIGN PIC X.
       01  PLACE-TAB01.
           02 PLACE-TAB OCCURS 26 TIMES.
             03 PL-TAB PIC X.
             03 PL-NUM PIC X.
             03 PL-NAME PIC X(22).
       01  HIGH-DOC PIC XX.
       01  HIGH-PLACE PIC X.
       01  BELL0 USAGE INDEX.
       01  PAYFILE-STAT.
           02 PAYFILE-STAT1 PIC X.
           02 PAYFILE-STAT2 PIC X.
       01  CHARCUR-STAT.
           02 CHARCUR-STAT1 PIC X.
           02 CHARCUR-STAT2 PIC X.
       01  AUTH-STAT PIC XX.
       01  ANS          PIC XXX.
       01  ACTION       PIC XXX.
       01  DATAIN       PIC X(24).
       01  STATX               PIC XXX VALUE "O-K".
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
           02 FILLER PIC X(14) VALUE "11240703020608".
        01 LEN-TAB01 REDEFINES LEN-TAB01-RE.
           02 LEN-TAB   PIC 99 OCCURS 7 TIMES.
       01  Q           PIC 99 VALUE ZERO.
      * HOLDS THE POSITION OF A FIELD WITHIN IT'S DATA SET
       01  GD-CONSTANT.
           05  DES-CONSTANT.
               10 FIL PIC X(24) VALUE "GARNO   NAME    AMOUNT  ".
               10 FIL PIC X(24) VALUE "PAYCODE DENIAL  CLAIM   ".
               10 FIL PIC X(8) VALUE "DATE    ".
       01  GD-TABLE REDEFINES GD-CONSTANT.
           05 DESC-FLD OCCURS 7 TIMES INDEXED BY INDX.
               10 DES-KEY  PIC X(8).
       
       01  ADD-FIELD-CONS.
             06 FILLER PIC X(12) VALUE "010304050706".
       01  ADD-FIELD-TABLE REDEFINES ADD-FIELD-CONS.
             06 ADD-FLD PIC 99 OCCURS 6 TIMES INDEXED BY ADD-KEY.
       
       01 CCLEN-TAB01-RE.
           02 FILLER PIC X(30) VALUE "110806010707060302030102080602".
           02 FILLER PIC X(24) VALUE "240108010106010107070801".
           02 FILLER PIC X(14) VALUE "01080202010107".
       01 CCLEN-TAB01 REDEFINES CCLEN-TAB01-RE.
           10 CCLEN-TAB   PIC 99 OCCURS 34 TIMES.
       01  CCDES-CONSTANT.
               10 FILLER PIC X(24) VALUE "REC NO  PATNO   CLAIM   ".
               10 FILLER PIC X(24) VALUE "TYP SERVDX1     PROCEDUR".
               10 FILLER PIC X(24) VALUE "CHARGE  REF PHYSDOCTOR  ".
               10 FILLER PIC X(24) VALUE "PAYCODE FLD 11  UNITS   ".
               10 FILLER PIC X(24) VALUE "ACCDATE PAPER   PL. SERV".
               10 FILLER PIC X(24) VALUE "FLD 16  FLD 17  TRANDATE".
               10 FILLER PIC X(24) VALUE "RESULT  ACTION  MODIFY 2".
               10 FILLER PIC X(24) VALUE "REC-STATSORCREF DX-2    ".
               10 FILLER PIC X(24) VALUE "DX-3    CLM-DATECOLLT   ".
               10 FILLER PIC X(24) VALUE "ACC-TYPEADMIT-DTMOD3    ".
               10 FILLER PIC X(24) VALUE "XXXX    ASSIGN  NEICASGN".
               10 FILLER PIC X(24) VALUE "DX4     ".
      *         DX5     DX6     ".

       01  CCGD-TABLE REDEFINES CCDES-CONSTANT.
             05 CCDES-KEY PIC X(8) OCCURS 34 TIMES INDEXED BY CCINDX.
       01  INPUT-DATE.
           05 T-MM  PIC 99.
           05 T-DD  PIC 99.
           05 T-CC  PIC 99.
           05 T-YY  PIC 99.
       
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
       01  NEF2 PIC ZZ.
       01   NEF-1    PIC ZZ.9  .
       01   NEF-2    PIC ZZZ.9  .
       01   NEF-3    PIC ZZZ.99  .
       01   NEF-4    PIC Z,ZZZ.99  .
       01   NEF-5    PIC Z,ZZZ.99CR.
       01   NEF-8    PIC ZZZ,ZZZ.99CR.
       01   NEF-D PIC ZZ,ZZZ.99CR.

       01  FLAG-TABLE01.
           02 FLAG-TABLE PIC 99 OCCURS 7 TIMES.
       01  CCFLAG-TABLE01.
           02 CCFLAG-TABLE PIC 99 OCCURS 27 TIMES.
       01  BUFFER-PACK01 PIC X(25).
       01  VALUE-BUFFER01 PIC X(25).
       01  HOLD-MASTER  .
           02 HOLD-ID PIC X(11).
           02 FILLER PIC X(59).
       01  PAYBACK01 PIC X(80).
       01  CURBACK PIC X(156).
       01  HOLD-DIST PIC X(70).
       01     DF-AMOUNT PIC S9(4)V99.
       01     DF-DATE PIC X(8).
       01     DF-PAYCODE PIC XXX.
       01     DF-DENIAL PIC XX.
       01     TOT-AMOUNT PIC S9(6)V99.
       01  TOT-BAL PIC S9(6)V99.
       01     SAVE-GARNO PIC X(8).
       01     RIGHT-8 PIC X(8) JUST RIGHT.
       01     RIGHT-2 PIC XX JUST RIGHT.
       01     RIGHT-4 PIC X(4) JUST RIGHT.
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
       01  NAME15 PIC X(15).
       01     ALF-1 PIC X.
       01     XALF-1 PIC X.
       01  ALF-2 PIC XX.
       01   ALF-3.
           02 ALF-3-1 PIC X.
           02 ALF-3-2 PIC XX.
       01  ALF-4 PIC X(4).
       01     ALF-5 PIC X(5).
       01     ALF-6 PIC X(6).
       01     ALF-7 PIC X(7).
       01     ALF-8 PIC X(8).
       01     ALF10 PIC X(10).
       01     ALF81 PIC X(8).
       01     ALF82 PIC X(8).
       01     ALF-T PIC X(8).
       01     ALF-11 PIC X(11).
       01     ALF-13 PIC X(13).
       01     ALF-14 PIC X(14).
       01     ALF-15 PIC X(15).
       01     ALF20 PIC X(20).
       01     ALF78 PIC X(78).
       01  DATE-OF-CHARGE PIC X(8) VALUE SPACE. 
       01     CLAIM PIC X(6).
       01     EIGHTPARTID.
             03 FILLER PIC X(7).
             03 EIGHT-1 PIC X.
       01     ABC PIC XXX.
       01     XYZ PIC 999.
       01     SIGN-DOLLAR PIC X(4).
       01     RIGHT-5 PIC X(5) JUST RIGHT.
       01     RIGHT-3 PIC XXX JUST RIGHT.
       01     RIGHT-7 PIC X(7) JUST RIGHT.
       01     CENTS PIC XX.
       01     NUM-3 PIC 999.
       01   YIND USAGE INDEX.
       01  XIND USAGE IS INDEX.
       01     NUM-9 PIC 9(9).
       01     NUM-6 PIC 9(6).
       01  SNUM-6 PIC S9(5)V99.
       01     A PIC 99.
       01     B PIC 99.
       01     C PIC 99.
       01     D PIC 99.
       01     X PIC 99.
       01     Y PIC 99.
       01   Z PIC 9.
       01     AA USAGE INDEX.
       01     XX USAGE INDEX.
       01     YY USAGE INDEX.
       01  ZZ USAGE IS INDEX.
       01     LINE-CNTR PIC 999.
       01     NUMBER-OF-FIELDS PIC 99.
       01     FLAG   PIC 9.
       01     RETURN-FLAG   PIC 9.
       01     WORK1  PIC X(20) JUST RIGHT.
       01     LOW-NUM-X  PIC XX JUST RIGHT.
       01     HIGH-NUM-X PIC XX JUST RIGHT.
       01     LOW-NUM    PIC 99.
       01     HIGH-NUM   PIC 99.
       01     TOTAL-LENGTH  PIC 999.
       01     CHAR-COUNTER PIC 99 VALUE ZERO.
       01  ORDER-8.
           02 ORDER-6 PIC X(6).
           02 FILLER PIC XX.
       01 TEMP-FIELD01.
           02 TEMP-FIELD  PIC X  OCCURS 80 TIMES.
       01 DISPLAY-FIELD01.
           02 DISPLAY-FIELD PIC 99 OCCURS 7 TIMES.
       01  LAST-TAB01.
           02 LAST-TAB PIC X(8) OCCURS 9 TIMES.
       01  LAST-PAT01.
           02 LAST-PAT PIC X(8) OCCURS 9 TIMES.
       01  LAST-CLAIM01.
           02 LAST-CLAIM PIC X(6) OCCURS 9 TIMES.
       01  LASTPAY-TAB01.
           02 LASTPAY-TAB PIC X(11) OCCURS 9 TIMES.
       01  CCKEY-TAB01.
           02 CCKEY-TAB PIC X(11) OCCURS 9 TIMES.
       01  NUMBACK PIC 9 VALUE 0.
       01  FZ PIC S9 VALUE 1.
       01  DI PIC S9 VALUE -1.
       01  DR PIC S9 VALUE -1.
       01  TOT-CLAIM PIC S9(5)V99.
       01  FLAGX PIC 9 VALUE 0.
       01 LLTAB2401.
           02 LLTAB24 PIC X OCCURS 24 TIMES.
       01 FFTAB1001.
           02 FFTAB10 PIC X OCCURS 10 TIMES.
       01  FF USAGE IS INDEX.
       01 LL USAGE IS INDEX.
       01 XLLTAB2401.
           02 XLLTAB24 PIC X OCCURS 24 TIMES.
       01 XFFTAB1001.
           02 XFFTAB10 PIC X OCCURS 10 TIMES.
       01 YYY PIC 999.
       01 X-AMOUNT PIC S9(4)V99.
       01 CM PIC 9 VALUE 1.
       01  TOT-ASSIGNED PIC S9(6)V99.
       01  TOT-UNASSIGNED PIC S9(6)V99.
       01  ALF-1-1 PIC X.
       01  T-DATE. 
           02 YEAR4.
              04 CC-T PIC XX.
              04 YY-T PIC XX.
           02 MM-T    PIC XX.   
           02 DD-T    PIC XX.
       01     PART11.
             03 PART8 PIC X(8).
             03 FILLER PIC XXX.
       01 GARPAT1 PIC 9 VALUE 0.
       01 CHAR1 PIC 9 VALUE 0.
       01 PB1 PIC 9 VALUE 0.
       01 DP PIC S9 VALUE 1.
       01 IP PIC 9 VALUE 1.
       01  CURRENT-BATCH PIC X(8) VALUE "00000000".
       01  CBN PIC X(10).
       01  KEYFLAG PIC 9.
       01 NUM-2 PIC 99.
       01  PLINDX PIC 99.
       01  CD PIC 9 VALUE 0.
       01  DATE-OF-CHARGE-LOW PIC X(8) VALUE SPACE. 
       01  DATE-OF-CHARGE-HIGH PIC X(8) VALUE SPACE. 
       01  UPDOWN PIC X.
       01  NAME14 PIC X(14).  
       01  SGFLAG PIC 9.
       01  TODAY-DATE.
           02 TT-CC PIC XX.
           02 TT-YY PIC XX.
           02 TT-MM PIC XX.
           02 TT-DD PIC XX.
       01  DATE-S.
           02 S-MM PIC XX.
           02 S-DD PIC XX.
           02 S-CC PIC XX.
           02 S-YY PIC XX.
       01  TABDX01.
           02 TABDX PIC X(7) OCCURS 6 TIMES.
       01  TAGTAB01.
           02 TAGTAB PIC X(7) OCCURS 20 TIMES.
       01  DIAG-FLAG PIC 9.
       PROCEDURE DIVISION.
       0005-START.
      *     DISPLAY INITIAL WINDOW.
           COMPUTE DP = -1 * DP
           OPEN INPUT DOCPARM.
           READ DOCPARM AT END GO TO 9100-CLOSE-MASTER-FILE.
           MOVE DPX TO HIGH-DOC.
      *    SET PLINDX TO 0.
           MOVE 0 TO PLINDX.
       PX0. READ DOCPARM AT END GO TO P00.
           IF DP-1-1 = "0" GO TO PX0.
           MOVE DP-1-1 TO HIGH-PLACE.
           ADD 1 TO PLINDX
           MOVE DP-1-1 TO PL-TAB(PLINDX)
           MOVE DP-1-2 TO PL-NUM(PLINDX)
           MOVE DP-2 TO PL-NAME(PLINDX) GO TO PX0.
       P00.
           ACCEPT T-DATE FROM CENTURY-DATE.
           SET BELL0 TO 7.
           MOVE ZERO TO DF-DATE DF-PAYCODE DF-DENIAL.
           MOVE 10 TO DF-AMOUNT.
           OPEN I-O PAYFILE.
           OPEN I-O CHARCUR.
           OPEN I-O AUTHFILE.
           OPEN OUTPUT FILEOUT
           OPEN INPUT GARFILE TAGDIAG.
           OPEN INPUT CMNTFILE.
           OPEN INPUT PATFILE.
           OPEN INPUT CHARFILE.
           OPEN INPUT PAYCUR.
           OPEN INPUT REFPHY.
           OPEN INPUT PROCFILE.
           OPEN INPUT DIAGFILE.
           OPEN INPUT GAPFILE.
           OPEN INPUT INSFILE.
           OPEN INPUT MPLRFILE.
           MOVE ALL " " TO LAST-TAB01 LAST-CLAIM01 
           CCKEY-TAB01 LASTPAY-TAB01 LAST-PAT01.
           SET ADD-KEY TO 0.
      * 999-A.
           MOVE T-DATE TO CURRENT-BATCH.
      *     DISPLAY "NON-BATCH METHOD SET"
      *     DISPLAY "TO USE BATCH METHOD TYPE BAT".
       1000-ACTION.
           MOVE ALL "0" TO DISPLAY-FIELD01.
           MOVE SPACES TO ACTION PAYFILE-KEY.
           MOVE ZERO TO FLAG-TABLE01.
           MOVE 0 TO NUMBER-OF-FIELDS.
           DISPLAY "OPTION,ID PAYMENTS".
           ACCEPT DATAIN.
           IF DATAIN = "GP"
           CLOSE GARFILE
           CLOSE PATFILE
           CLOSE MPLRFILE
           CLOSE INSFILE
           CLOSE GAPFILE
           CALL "/home/sidw/tri001.b" USING GARPAT1
           MOVE 1 TO GARPAT1
           OPEN INPUT GARFILE PATFILE MPLRFILE INSFILE GAPFILE
           GO TO 1000-ACTION.
           IF DATAIN = "AC"
           CLOSE CHARFILE
           CLOSE CHARCUR
           CLOSE PAYFILE
           CLOSE FILEOUT
           CLOSE PAYCUR
           CLOSE PROCFILE
           CLOSE TAGDIAG
           CLOSE DIAGFILE
           CALL "/home/sidw/ina002.b" USING CHAR1
           MOVE 1 TO CHAR1
           OPEN INPUT CHARFILE PAYCUR PROCFILE TAGDIAG DIAGFILE
           OPEN OUTPUT FILEOUT
           OPEN I-O CHARCUR PAYFILE
           GO TO 1000-ACTION.
      *     IF DATAIN = "NONBAT" GO TO 999-A.
      *     IF DATAIN = "BAT" CALL "/home/sidw/tri007.b" 
      *     USING CURRENT-BATCH CBN
      *     GO TO 1000-ACTION.
           IF DATAIN = "CD" PERFORM CD1 GO TO 1000-ACTION.
           IF DATAIN = "IP" PERFORM IP-1 GO TO 1000-ACTION.
           IF DATAIN = "CM" PERFORM CM-1 GO TO 1000-ACTION.
      *    IF DATAIN = "RC" GO TO P0.
           IF DATAIN = "DR" PERFORM DR2 GO TO 1000-ACTION.
           IF DATAIN = "DP" PERFORM DPX1 GO TO 1000-ACTION.
           IF DATAIN = "FZ" PERFORM FZ1 GO TO 1000-ACTION.
           IF DATAIN = "DI" PERFORM DI1 GO TO 1000-ACTION.
           IF DATAIN = "END" GO TO 9100-CLOSE-MASTER-FILE.
           IF DATAIN = "?"
           DISPLAY "GP = GAR-PAT-INS-MPLR ROUTINES."
           DISPLAY "AC = DAILY CHARGES."
           DISPLAY "FPC,LPC,CPC =POSTED CHRG ROUTINE."
           DISPLAY "A,D,C  <GARNO+KEY> PAYMENT RECORD."
           DISPLAY "FC = FIND CHARGES FCC = ONLY CHARGES FX=2ND LINE."
           DISPLAY "FT = FIND TOTALS, INSURANCE AND PERSONAL."
           DISPLAY "FA,FG,SG,LG = GUARANTOR ROUTINES."
           DISPLAY "FP,SP,LP = PATIENT ROUTINES."
           DISPLAY "DA,DE,PC,AM,<VALUE> TO DEFAULT DATE,DENIAL,PAYCODE,"
           "AMOUNT."
           DISPLAY "DA,DE,PC,AM,<0> TO RESET FOR PROMPT."
           DISPLAY "LD OR LD,?  LIST OF DEFAULT FIELDS SET."
           DISPLAY "CONDITIONAL DEFAULT SETTINGS SWITCHES"
           DISPLAY "DR = REDUCTION PROMPT  DI = DEFERRED INCOME PROMPT"
           DISPLAY "FZ = ZERO BALANCES     CM = COMMENT LISTING"
           DISPLAY "DP = PATIENT NAME      IP = INSURANCE"
           DISPLAY "LI= LIST INS. FI=FIND INS.  CD=CHARGE DATE PROMPT." 
           DISPLAY "COM,# TO ADD A COMMENT RECORD THE ACCT #."
           DISPLAY "HS,HSC,HSP,# TO SEARCH HISTORY RECORDS."
           DISPLAY "PB,<GARNO> TO PRINT BILL. LB,<GARNO> LAB FORM"
           DISPLAY "PCF TO PRINT A POSTED CHARGE 1500-HCFA FORM"
           DISPLAY "RA = RE-AGE CHARGES TO CURRENT END = END THE JOB."
           GO TO 1000-ACTION.
           MOVE SPACE TO ALF-3 BUFFER-PACK01.
           UNSTRING DATAIN DELIMITED BY "=" INTO BUFFER-PACK01 ALF-3.
           IF ALF-3 NOT = SPACE MOVE BUFFER-PACK01 TO DATAIN.
           UNSTRING DATAIN DELIMITED BY "," INTO ACTION PAYFILE-KEY.
           MOVE SPACE TO RIGHT-3.
           UNSTRING ALF-3 DELIMITED BY " " INTO RIGHT-3.
           INSPECT RIGHT-3 REPLACING ALL " " BY "0".
           IF RIGHT-3 NOT NUMERIC DISPLAY "PAYCODE NOT NUMERIC"
           GO TO 1000-ACTION.
           MOVE RIGHT-3 TO ALF-3.

           IF (PAYFILE-KEY = "1" OR "2" OR "3" OR "4" OR "5" OR
           "6" OR "7" OR "8" OR "9") MOVE 1 TO KEYFLAG
           ELSE MOVE 0 TO KEYFLAG.

           IF KEYFLAG = 1 AND ACTION = "D"
           MOVE PAYFILE-KEY TO IN-FIELD
           MOVE IN-FIELD-1 TO FLAG
           MOVE LASTPAY-TAB(FLAG) TO PAYFILE-KEY GO TO 1300DEL.

           IF KEYFLAG = 1 AND ACTION = "C"
           MOVE PAYFILE-KEY TO IN-FIELD
           MOVE IN-FIELD-1 TO FLAG
           MOVE LASTPAY-TAB(FLAG) TO PAYFILE-KEY
           GO TO 1400-CHANGE-IT.

           IF KEYFLAG = 1 AND ACTION = "FPC"
           MOVE PAYFILE-KEY TO IN-FIELD
           MOVE IN-FIELD-1 TO FLAG
           MOVE LAST-TAB(FLAG) TO CC-KEY8 MOVE "000" TO CC-KEY3
           GO TO CC-1200-FIND.
           IF ACTION = "FPC" MOVE SPACE TO CHARCUR-KEY
           MOVE PAYFILE-KEY TO CHARCUR-KEY GO TO CC-1200-FIND.

           IF KEYFLAG = 1 AND ACTION = "HS" OR "HSC" OR "HSP"
           MOVE PAYFILE-KEY TO IN-FIELD
           MOVE IN-FIELD-1 TO FLAG
           MOVE LAST-TAB(FLAG) TO ALF-8
           CALL "/home/sidw/tri011.b" USING ACTION ALF-8 
           GO TO 1000-ACTION.
           IF ACTION = "HS" OR "HSC" OR "HSP"
           MOVE PD-KEY8 TO ALF-8 CALL "/home/sidw/tri011.b"
           USING ACTION ALF-8 GO TO 1000-ACTION.

           IF KEYFLAG = 1 AND ACTION = "CPC"
           MOVE PAYFILE-KEY TO IN-FIELD
           MOVE IN-FIELD-1 TO FLAG
           MOVE CCKEY-TAB(FLAG) TO CHARCUR-KEY
           GO TO 1399CPC.
           IF ACTION = "CPC" MOVE PAYFILE-KEY TO CHARCUR-KEY
           GO TO 1399CPC.

           IF KEYFLAG = 1 AND ACTION = "LPC"
           MOVE PAYFILE-KEY TO IN-FIELD
           MOVE IN-FIELD-1 TO FLAG
           MOVE CCKEY-TAB(FLAG) TO CHARCUR-KEY
           PERFORM LC-0 THRU LC-0-EXIT GO TO 1000-ACTION.
           IF ACTION = "LPC" MOVE PAYFILE-KEY TO CHARCUR-KEY
           PERFORM LC-0 THRU LC-0-EXIT GO TO 1000-ACTION.

           IF ACTION = "COM"
           MOVE PAYFILE-KEY TO IN-FIELD
           MOVE IN-FIELD-1 TO FLAG
           MOVE LAST-TAB(FLAG) TO G-GARNO
           READ GARFILE
             invalid
               MOVE PAYFILE-KEY TO G-GARNO
               READ GARFILE
                 invalid
                 DISPLAY "bad read on garfile " G-GARNO
                 go to 1000-ACTION  
               end-read    
           end-read      

           DISPLAY G-GARNO " " G-GARNAME " " G-PRINS "/" G-SEINS
           MOVE G-GARNO TO ALF-8
           CLOSE CMNTFILE CALL "/home/sidw/tri000.b" USING ALF-8
           OPEN INPUT CMNTFILE  GO TO 1000-ACTION.

           IF KEYFLAG = 1 AND ACTION = "PB"
           MOVE PAYFILE-KEY TO IN-FIELD
           MOVE IN-FIELD-1 TO FLAG
           MOVE LAST-TAB(FLAG) TO G-GARNO
           CLOSE CHARCUR
           CLOSE PAYFILE
           CALL "/home/sidw/tri205.b" USING PB1 G-GARNO
           MOVE 1 TO PB1
           OPEN I-O CHARCUR PAYFILE
           GO TO 1000-ACTION.
           
           IF KEYFLAG = 1 AND ACTION = "LB"
           MOVE PAYFILE-KEY TO IN-FIELD
           MOVE IN-FIELD-1 TO FLAG
           MOVE LAST-TAB(FLAG) TO G-GARNO
           CLOSE CHARCUR
           CLOSE PAYFILE
           CLOSE TAGDIAG
           CLOSE GARFILE
           CLOSE INSFILE
           CLOSE GAPFILE
           CALL "/home/sidw/mcc222.b" USING G-GARNO
           OPEN I-O CHARCUR PAYFILE
           OPEN INPUT TAGDIAG GARFILE INSFILE GAPFILE
           GO TO 1000-ACTION.
           IF KEYFLAG = 1 AND ACTION = "PCF"
           MOVE PAYFILE-KEY TO IN-FIELD
           MOVE IN-FIELD-1 TO FLAG
           MOVE CCKEY-TAB(FLAG) TO CHARCUR-KEY
           GO TO 10-PR.

           IF KEYFLAG = 1 AND ACTION = "CC"
           MOVE PAYFILE-KEY TO IN-FIELD
           MOVE IN-FIELD-1 TO FLAG
           MOVE LAST-TAB(FLAG) TO G-GARNO
           GO TO CC-1.

           IF KEYFLAG = 1 AND ACTION = "RA"
           MOVE PAYFILE-KEY TO IN-FIELD
           MOVE IN-FIELD-1 TO FLAG
           MOVE LAST-TAB(FLAG) TO G-GARNO
           GO TO RA-1.

           IF KEYFLAG = 1 AND ACTION = "LP"
           MOVE PAYFILE-KEY TO IN-FIELD
           MOVE IN-FIELD-1 TO FLAG
           MOVE LAST-PAT(FLAG) TO PAYFILE-KEY
           PERFORM LP-1 THRU LP-1-EXIT GO TO
           1000-ACTION.
           
           IF KEYFLAG = 1 
           MOVE PAYFILE-KEY TO IN-FIELD
           MOVE IN-FIELD-1 TO FLAG
           MOVE LAST-TAB(FLAG) TO PAYFILE-KEY.
           IF NOT (ACTION = "FC" OR  "FT" OR  "FCC" OR "FX")
           GO TO PPPP.
           MOVE PD-KEY8 TO G-GARNO.
           MOVE PAYFILE01 TO PAYBACK01.
           PERFORM FP1 THRU FP1-EXIT
           MOVE PAYBACK01 TO PAYFILE01.
           IF ACTION = "FT" MOVE TOT-ASSIGNED TO NEF-8
           DISPLAY "INSURANCE = " NEF-8
           MOVE TOT-UNASSIGNED TO NEF-8
           DISPLAY "PERSONAL  = " NEF-8.
           IF CM = 1 PERFORM CM-2 THRU CM-4.
           GO TO 1000-ACTION.
       PPPP.
           IF ACTION = "LG" PERFORM LG-1 THRU LG-1-EXIT GO TO
           1000-ACTION.
           IF ACTION = "LP" PERFORM LP-1 THRU LP-1-EXIT GO TO
           1000-ACTION.
      *     IF ACTION = "A" AND CURRENT-BATCH = "00000000"
      *     DISPLAY "CURRENT BATCH NUMBER NOT SET" BELL
      *     GO TO 1000-ACTION.
           IF ACTION = "A" GO TO 1200-ADD-PROCESS.
           IF ACTION = "F" GO TO 1200-FIND.
           IF ACTION = "SG" OR "FG" GO TO 2400-FIND.
           IF ACTION = "SP" OR "FP" GO TO 3600-FIND.
           IF ACTION = "FA" PERFORM FA-1 THRU FA-1-EXIT
           GO TO 1000-ACTION.
           IF ACTION = "FC" OR "FX" MOVE PD-KEY8 TO G-GARNO PERFORM FP1
           THRU FP1-EXIT GO TO 1000-ACTION.
           IF ACTION = "FT" MOVE PD-KEY8 TO G-GARNO
           PERFORM FP1 THRU FP1-EXIT
           MOVE TOT-ASSIGNED TO NEF-8
           DISPLAY "INSURANCE = " NEF-8
           MOVE TOT-UNASSIGNED TO NEF-8
           DISPLAY "PERSONAL  = " NEF-8
           GO TO 1000-ACTION.
      *     IF ACTION = "B
      *      DISPLAY "CURRENT BATCH = " CURRENT-BATCH
      *     " " CBN
      *     GO TO 1000-ACTION.
           IF ACTION = "DA" GO TO DA1.
           IF ACTION = "DE" GO TO DE1.
           IF ACTION = "PC" GO TO PC1.
           IF ACTION = "AM" GO TO AM1.
           IF ACTION = "LD"
           GO TO LD1.
           IF ACTION = "FI" PERFORM INS-1 THRU INS-1-EXIT
           GO TO 1000-ACTION.
           IF ACTION = "LI" PERFORM LI-1 THRU LI-1-EXIT
           GO TO 1000-ACTION.
           IF ACTION = "RA" GO TO RA-1.
           DISPLAY "WHAT ?" GO TO 1000-ACTION.
       LG-1.
           MOVE PAYFILE-KEY TO G-GARNO
           IF G-GARNO = "1" OR "2" OR "3" OR "4" OR "5" OR "6"
           OR "7" OR "8" OR "9" MOVE G-GARNO TO FLAG
           MOVE LAST-TAB(FLAG) TO G-GARNO.
           READ GARFILE INVALID DISPLAY "INVALID" GO TO LG-1-EXIT.
           MOVE G-GARNO TO MPLR-KEY
           MOVE "1" TO G-TRINSIND
           READ MPLRFILE INVALID MOVE "0" TO G-TRINSIND.
       LG-1-0.
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
           "   " DISPLAY-DATE " " G-BILLCYCLE.
           IF G-INSPEND NOT = 0 MOVE G-INSPEND TO NEF-D
           DISPLAY " CO-PAY =" NEF-D.
       LG-1-EXIT. EXIT.
       LP-1.
           MOVE PAYFILE-KEY TO P-PATNO
           IF P-PATNO = "1" OR "2" OR "3" OR "4" OR "5" OR "6"
           OR "7" OR "8" OR "9" MOVE P-PATNO TO FLAG
           MOVE LAST-PAT(FLAG) TO P-PATNO.
       LP-1-0.
           READ PATFILE INVALID DISPLAY "INVALID" GO TO LP-1-EXIT.
           MOVE P-GARNO TO G-GARNO.
           READ GARFILE INVALID MOVE "NO GARNO" TO G-GARNAME.
           MOVE P-DOB TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           INSPECT T-MM OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           INSPECT T-DD OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           DISPLAY P-PATNO " " P-PATNAME
           DISPLAY "SX=" P-SEX " MS=" P-MSTAT " RE=" P-RELATE
           " DOB " DISPLAY-DATE
           DISPLAY "GUAR " G-GARNO " " G-GARNAME.
       LP-1-EXIT. EXIT.
       1200-ADD-PROCESS.
           MOVE ZEROES TO PD-CLAIM   PD-AMOUNT   PD-PAYCODE
             PD-DATE-T PD-DENIAL PD-BATCH.
           MOVE SPACES TO PD-NAME PAYFILE-KEY.
       M1. PERFORM 1205-ADD-LOOP VARYING ADD-KEY FROM 1 BY 1
             UNTIL ADD-KEY > 6.
       M4. IF IN-FIELD = "X" DISPLAY "NO UPDATE" GO TO 1000-ACTION.
           IF IN-FIELD = "!)(!" GO TO M2.
      *    IF PD-PAYCODE = "060" MOVE 0 TO PD-AMOUNT.
       DR1.
           ACCEPT ORDER-8 FROM TIME
           MOVE ORDER-6 TO PD-ORDER.
           MOVE CURRENT-BATCH TO PD-DATE-E
      *    ACCEPT PD-DATE-E FROM CENTURY-DATE.
           IF ((PD-PAYCODE = "010" OR "015" OR "019")
           OR (PD-DENIAL = "08" OR "15"))
           AND (PD-AMOUNT < 0)
           COMPUTE PD-AMOUNT = -1 * PD-AMOUNT.
           MOVE PAYFILE01 TO PAYBACK01
           MOVE 0 TO XYZ.
       DR1-1. ADD 1 TO XYZ MOVE XYZ TO PD-KEY3
           IF XYZ = 999 DISPLAY "NO UPDATE"
           DISPLAY "THIS SHOULD NOT HAVE HAPPENED! CONTACT DATA CENTER."
           DISPLAY "THIS PROGRAM HAS BEEN TERMINATED"
           GO TO 9100-CLOSE-MASTER-FILE.
           WRITE PAYFILE01 INVALID KEY GO TO DR1-1.
           DISPLAY PAYFILE-KEY " " PD-NAME.
           DISPLAY "RECORD IS ADDED".
           MOVE PAYFILE01 TO PAYBACK01.
           MOVE 0 TO FLAGX.
           MOVE PD-KEY3 TO XYZ.
           IF DI = 1 PERFORM WH1 THRU WH1-EXIT.
       DR1-EXIT. EXIT.
       WO1. IF DR = -1 GO TO M2.
           IF (PD-PAYCODE > "006" AND < "020") 
           AND (PD-PAYCODE NOT = "018")
           DISPLAY "REDUCTIONS ON ADJUSTMENT RECORDS ARE INVALID."
           GO TO M2.
           DISPLAY "REDUCE?".
           ACCEPT IN-FIELD
           IF IN-FIELD = "?" DISPLAY "ALL TO WRITE-OFF BAL."
           DISPLAY "<CR> NOT TO WRITE OFF ANYTHING"
           DISPLAY "ENTER AMOUNT OF WRITE OFF" GO TO WO1.
           IF IN-FIELD = "N" OR SPACE GO TO M2.
           IF IN-FIELD = "ALL" GO TO WO3.
           MOVE SPACE TO DATAIN ALF-3.
           IF IN-FIELD = "FC" OR "FX" MOVE PAYFILE01 TO PAYBACK01
           PERFORM FP1 THRU FP1-EXIT MOVE PAYBACK01 TO PAYFILE01
           GO TO WO1.
           MOVE SPACES TO SIGN-DOLLAR CENTS.
           UNSTRING IN-FIELD DELIMITED BY "." INTO SIGN-DOLLAR CENTS.
           IF CENTS = SPACES MOVE "00" TO CENTS.
           IF CENTS NOT NUMERIC DISPLAY "INVALID" GO TO WO1.
           MOVE SPACES TO RIGHT-4.
           UNSTRING SIGN-DOLLAR DELIMITED BY " " INTO RIGHT-4
           INSPECT RIGHT-4 REPLACING LEADING " " BY "0"
           IF RIGHT-4 NOT NUMERIC DISPLAY "INVALID" GO TO WO1.
           STRING RIGHT-4 CENTS DELIMITED BY "Z" INTO ALF-6
           MOVE ALF-6 TO NUM-6
           DIVIDE NUM-6 BY -100 GIVING PD-AMOUNT.
           MOVE PAYFILE01 TO PAYBACK01.
           MOVE 1 TO FLAGX MOVE PD-AMOUNT TO X-AMOUNT.
      *    GO TO WO4.
       WO3.
           PERFORM WO6 THRU WO12.
           MOVE PAYBACK01 TO PAYFILE01.
           IF FLAG = 1 DISPLAY "CHARGE NOT FOUND. NO REDUCTION WRITTEN."
           MOVE 0 TO FLAG GO TO M2.
           IF TOT-CLAIM = 0 DISPLAY "CLAIM BAL. 0".
           IF TOT-CLAIM < 0 DISPLAY "CREDIT BAL. CLAIM".
           IF TOT-CLAIM NOT > 0 DISPLAY "NO REDUCTION RECORD WRITTEN"
           GO TO M2.
           MULTIPLY -1 BY TOT-CLAIM GIVING PD-AMOUNT.
           IF FLAGX = 0 GO TO WO4.
           IF X-AMOUNT < PD-AMOUNT
           DISPLAY "THIS WILL CAUSE A CREDIT BALANCE" GO TO WO1.
           MOVE X-AMOUNT TO PD-AMOUNT.
       WO4.
           MOVE "14" TO PD-DENIAL
           ACCEPT ORDER-8 FROM TIME
           MOVE ORDER-6 TO PD-ORDER
           MOVE CURRENT-BATCH TO PD-DATE-E
           MOVE PAYFILE01 TO PAYBACK01.
       WO13. ADD 1 TO XYZ.
           MOVE XYZ TO PD-KEY3.
           READ PAYFILE INVALID GO TO WO14.
           IF XYZ = 999 DISPLAY "TOO MANY RECORDS CALL CMS"
           GO TO 1000-ACTION ELSE GO TO WO13.
       WO14. MOVE PD-AMOUNT TO NEF-8 DISPLAY "REDUCTION " NEF-8.
           MOVE PAYBACK01 TO PAYFILE01 MOVE XYZ TO PD-KEY3.
           WRITE PAYFILE01 INVALID GO TO WO13.
       M2. DISPLAY "MORE PAYMENTS ?".
           MOVE PD-KEY3 TO XYZ.
           ACCEPT ANS.
           IF ANS = "?" DISPLAY "Y=YES  <CR>=NO"
           GO TO M2.
           IF ANS = SPACE GO TO 1000-ACTION.
           IF ANS NOT = "Y" GO TO M2.
           GO TO M5.
       WH1. 
           IF (PD-PAYCODE > "006" AND < "020") 
           AND (PD-PAYCODE NOT = "018")
           DISPLAY "WITHHOLDS ON ADJUSTMENT RECORDS ARE INVALID."
           GO TO WH1-EXIT.
           DISPLAY "WITHHOLD?".
           ACCEPT IN-FIELD
           IF IN-FIELD = "?" 
           DISPLAY "<CR> = NO WITHHOLD FOR THIS CHARGE"
           DISPLAY "OR ENTER AMOUNT OF WITHHOLD" GO TO WH1.
           IF IN-FIELD = "N" OR "0" OR SPACE GO TO WH1-EXIT.
           MOVE SPACE TO DATAIN ALF-3.
           IF IN-FIELD = "FC" OR "FX" MOVE PAYFILE01 TO PAYBACK01
           PERFORM FP1 THRU FP1-EXIT MOVE PAYBACK01 TO PAYFILE01
           GO TO WH1.
           MOVE SPACES TO SIGN-DOLLAR CENTS.
           UNSTRING IN-FIELD DELIMITED BY "." INTO SIGN-DOLLAR CENTS.
           IF CENTS = SPACES MOVE "00" TO CENTS.
           IF CENTS NOT NUMERIC DISPLAY "INVALID" GO TO WH1.
           MOVE SPACES TO RIGHT-4.
           UNSTRING SIGN-DOLLAR DELIMITED BY " " INTO RIGHT-4
           INSPECT RIGHT-4 REPLACING LEADING " " BY "0"
           IF RIGHT-4 NOT NUMERIC DISPLAY "INVALID" GO TO WH1.
           STRING RIGHT-4 CENTS DELIMITED BY "Z" INTO ALF-6
           MOVE ALF-6 TO NUM-6
           DIVIDE NUM-6 BY -100 GIVING PD-AMOUNT.
           MOVE PAYFILE01 TO PAYBACK01.
           MOVE 1 TO FLAGX MOVE PD-AMOUNT TO X-AMOUNT.
      *    GO TO WO4.
       WH3.
           PERFORM WO6 THRU WO12.
           MOVE PAYBACK01 TO PAYFILE01.
           IF FLAG = 1 DISPLAY "CHARGE NOT FOUND. NO WITHHOLD WRITTEN."
           MOVE 0 TO FLAG GO TO WH1-EXIT.
           IF TOT-CLAIM = 0 DISPLAY "CLAIM BAL. 0".
           IF TOT-CLAIM < 0 DISPLAY "CREDIT BAL. CLAIM".
           IF TOT-CLAIM NOT > 0 DISPLAY "NO WITHHOLD WRITTEN"
           GO TO WH1-EXIT.
           MULTIPLY -1 BY TOT-CLAIM GIVING PD-AMOUNT.
           IF FLAGX = 0 GO TO WH4.
           IF X-AMOUNT < PD-AMOUNT
           DISPLAY "THIS WILL CAUSE A CREDIT BALANCE" GO TO WH1.
           MOVE X-AMOUNT TO PD-AMOUNT.
       WH4.
           MOVE "DI" TO PD-DENIAL
           ACCEPT ORDER-8 FROM TIME
           MOVE ORDER-6 TO PD-ORDER
           MOVE CURRENT-BATCH TO PD-DATE-E
           MOVE PAYFILE01 TO PAYBACK01.
       WH13. ADD 1 TO XYZ.
           MOVE XYZ TO PD-KEY3.
           READ PAYFILE INVALID GO TO WH14.
           IF XYZ = 999 DISPLAY "TOO MANY RECORDS CALL CMS"
           GO TO 1000-ACTION ELSE GO TO WH13.
       WH14. MOVE PD-AMOUNT TO NEF-8 DISPLAY "WITHHOLD " NEF-8.
           MOVE PAYBACK01 TO PAYFILE01 MOVE XYZ TO PD-KEY3.
           WRITE PAYFILE01 INVALID GO TO WH13.
       WH1-EXIT. EXIT.

       M5. ADD 1 TO XYZ.
           MOVE XYZ TO PD-KEY3.
           READ PAYFILE INVALID GO TO M5-EXIT.
           GO TO M5.
       M5-EXIT. EXIT.
       M6. PERFORM 1205-ADD-LOOP VARYING ADD-KEY FROM 2 BY 1
           UNTIL ADD-KEY > 6.
           GO TO M4.
       DA1.
           IF PAYFILE-KEY = "0" MOVE "00000000" TO DF-DATE
           DISPLAY "PROMPT FOR DATE" GO TO 1000-ACTION.
           MOVE PAYFILE-KEY TO IN-FIELD.
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
           IF IN-FIELD-8 NOT NUMERIC DISPLAY "INVALID DATE" GO TO
           1000-ACTION.
           MOVE IN-FIELD-8 TO INPUT-DATE.
           IF T-MM OF INPUT-DATE < 01 OR > 12
           DISPLAY "INVALID MONTH" GO TO 1000-ACTION.
           IF (T-DD OF INPUT-DATE > DAYS-IN-MONTH(T-MM OF INPUT-DATE))
             OR (T-DD OF INPUT-DATE = 00)
           DISPLAY "INVALID DAY" GO TO 1000-ACTION.
           MOVE CORR INPUT-DATE TO TEST-DATE
      *     IF T-DATE < TEST-DATE DISPLAY "FUTURE DATE NOT ALLOWED"
      *     GO TO 1000-ACTION.
           IF TEST-DATE < "19990101" DISPLAY "VERY OLD" BELL.
           MOVE TEST-DATE TO DF-DATE GO TO 1000-ACTION.

       DE1.
           IF PAYFILE-KEY = "0" MOVE "00" TO DF-DENIAL
           DISPLAY "PROMPT FOR DENIAL" GO TO 1000-ACTION.
           MOVE PAYFILE-KEY TO IN-FIELD-2
           IF IN-FIELD-2 = "  " OR "NC" OR "DD" OR "NR" OR "CB"
           OR "CP" OR "CI" OR "TM" OR "NM"
           MOVE IN-FIELD-2 TO DF-DENIAL
           ELSE DISPLAY "INVALID DENIAL CODE". GO TO 1000-ACTION.
       PC1.
           IF PAYFILE-KEY = "0" MOVE "000" TO DF-PAYCODE
           DISPLAY "PROMPT FOR PAYORCODE" GO TO 1000-ACTION.
           MOVE SPACE TO RIGHT-3
           UNSTRING PAYFILE-KEY DELIMITED BY " " INTO RIGHT-3
           INSPECT RIGHT-3 REPLACING ALL " " BY "0"
           MOVE RIGHT-3 TO PAYFILE-KEY
           MOVE PAYFILE-KEY TO IN-FIELD-3.
           IF IN-FIELD-3 = "010" OR "015" OR "019" OR "017"
           DISPLAY "CAN""T DEFAULT DEBIT CODES" GO TO 1000-ACTION.
           IF IN-FIELD-3 NOT NUMERIC
           OR IN-FIELD-3 = "000" DISPLAY "INVALID PAYCODE"
           GO TO 1000-ACTION ELSE MOVE IN-FIELD-3 TO DF-PAYCODE
           GO TO 1000-ACTION.
       AM1.
           IF PAYFILE-KEY = "0" MOVE 10 TO DF-AMOUNT
           DISPLAY "PROMPT FOR AMOUNT" GO TO 1000-ACTION.
           MOVE SPACES TO SIGN-DOLLAR CENTS.
           UNSTRING PAYFILE-KEY DELIMITED BY "." INTO SIGN-DOLLAR CENTS.
           IF CENTS = SPACES MOVE "00" TO CENTS.
           IF CENTS NOT NUMERIC DISPLAY "INVALID" GO TO 1000-ACTION.
           MOVE SPACES TO RIGHT-4.
           UNSTRING SIGN-DOLLAR DELIMITED BY " " INTO RIGHT-4
           INSPECT RIGHT-4 REPLACING LEADING " " BY "0"
           IF RIGHT-4 NOT NUMERIC DISPLAY "INVALID" GO TO 1000-ACTION.
           STRING RIGHT-4 CENTS DELIMITED BY "Z" INTO ALF-6
           MOVE ALF-6 TO NUM-6
           DIVIDE NUM-6 BY -100 GIVING DF-AMOUNT
           GO TO 1000-ACTION.
       LD1. IF DF-PAYCODE = "000" NEXT SENTENCE
           ELSE DISPLAY "PAYCODE " DF-PAYCODE.
           IF DF-AMOUNT > 0 NEXT SENTENCE
           ELSE MOVE DF-AMOUNT TO NEF-8
           DISPLAY "AMOUNT " NEF-8.
           IF DF-DENIAL = "00" NEXT SENTENCE
           ELSE DISPLAY "DENIAL " DF-DENIAL.
           IF DF-DATE NOT = "00000000" 
           MOVE DF-DATE TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           DISPLAY "DATE " DISPLAY-DATE.
           IF FZ = -1
           DISPLAY "INCLUDE ZERO BALANCED CLAIMS." ELSE
           DISPLAY "NO LISTING FOR 0 BAL. CLAIMS.".
           IF DI = -1
           DISPLAY "NO DEFERRED INCOME PROMPT." ELSE
           DISPLAY "PROMPT FOR DEFERRED INCOME.". 
           IF DR = -1
           DISPLAY "NO PROMPT FOR REDUCTION." ELSE
           DISPLAY "PROMPT FOR REDUCTION.".
           IF CD = 0
           DISPLAY "NO PROMPT FOR CHARGE DATE" 
           ELSE DISPLAY "PROMPT FOR CHARGE ON FC AND FCC COMMAND.".
           GO TO 1000-ACTION.
       FP1.
            READ GARFILE INVALID DISPLAY " NOT ON FILE"
           GO TO 1000-ACTION.
           DISPLAY G-PRINS "/" G-SEINS "/" G-TRINS " " G-GARNAME
           MOVE G-GARNO TO PC-KEY8 MOVE "000" TO PC-KEY3.
           MOVE 0 TO TOT-UNASSIGNED TOT-ASSIGNED.
           SET P-IND TO 1
           IF CD = 1 PERFORM CD-SET THRU CD-EXIT.
           IF IN-FIELD = "FCC" OR ACTION = "FCC"
           DISPLAY "CHRGS ONLY" GO TO FC1.
           START PAYCUR KEY > PAYCUR-KEY INVALID GO TO FP5.
       FP4. READ PAYCUR NEXT AT END GO TO FP5.
           IF PC-KEY8 NOT = G-GARNO GO TO FP5.
           MOVE PC-CLAIM TO C-TAB(P-IND)
           MOVE PC-AMOUNT TO A-TAB(P-IND)
           MOVE PC-DATE-T TO D-TAB(P-IND)
           MOVE PC-DATE-E TO DE-TAB(P-IND)

           MOVE PC-PAYCODE TO PC-TAB(P-IND)
           MOVE PC-DENIAL TO DN-TAB(P-IND)
           IF P-IND = 990 DISPLAY "990 + PAYMENTS WERE FOUND"
           DISPLAY "ONLY 990 WILL BE USED" GO TO FC1.
           SET P-IND UP BY 1
           GO TO FP4.
       FP5. MOVE G-GARNO TO PD-KEY8 MOVE "000" TO PD-KEY3.
      *    IF P-IND > 1
      *    SET P-IND DOWN BY 1
           START PAYFILE KEY > PAYFILE-KEY INVALID GO TO FC1.
       FP6. READ PAYFILE NEXT AT END GO TO FC1.
           IF PD-KEY8 NOT = G-GARNO GO TO FC1.
           MOVE PD-CLAIM TO C-TAB(P-IND)
           MOVE PD-AMOUNT TO A-TAB(P-IND)
           MOVE PD-DATE-T TO D-TAB(P-IND)
           MOVE PD-DATE-E TO DE-TAB(P-IND)

           MOVE PD-PAYCODE TO PC-TAB(P-IND)
           MOVE PD-DENIAL TO DN-TAB(P-IND)
           IF P-IND = 990
           DISPLAY "ONLY 990 WILL BE USED" GO TO FC1.
           SET P-IND UP BY 1
           GO TO FP6.
       FC1. MOVE G-GARNO TO CC-KEY8 MOVE "000" TO CC-KEY3.
           MOVE 0 TO Y Z.
           SET P-IND DOWN BY 1.
           START CHARCUR KEY > CHARCUR-KEY INVALID GO TO FC6.
       FC3. READ CHARCUR NEXT AT END GO TO FC6.
           IF CC-KEY8 NOT = G-GARNO GO TO FC6.
           IF ALF-3 = "000" OR ALF-3 = CC-PAYCODE NEXT SENTENCE
           ELSE GO TO FC3.
           IF (CD = 0)
           OR ((CD = 1) AND (DATE-OF-CHARGE = CC-DATE-T OR SPACE))
           NEXT SENTENCE
           ELSE GO TO FC3.
           IF (IN-FIELD = "FT") OR (ACTION = "FT")
           MOVE CC-CLAIM TO CLAIM
           MOVE CC-AMOUNT TO TOT-AMOUNT
           MOVE CC-ASSIGN TO ALF-1-1
           PERFORM FT1 GO TO FC3.
           MOVE CC-AMOUNT TO TOT-AMOUNT NEF-8
           MOVE CC-CLAIM TO CLAIM
           MOVE CC-DATE-T TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           MOVE DISPLAY-DATE TO DISPLAY-DATE-CC
           MOVE SPACE TO ALF-5
           IF DP NOT = 1 GO TO FC33-0.
           MOVE G-GARNAME TO DATAIN
           MOVE CC-PATID TO EIGHTPARTID
           IF EIGHT-1 = "P" MOVE CC-PATID TO P-PATNO
           READ PATFILE  MOVE P-PATNAME TO DATAIN.
           UNSTRING DATAIN DELIMITED BY ";" INTO ALF-14 ALF-5.
       FC33-0.
           IF IN-FIELD = "FCC" OR ACTION = "FCC" GO TO FC33.
           IF FZ = -1 GO TO FC33.
           PERFORM FPZ VARYING XIND FROM 1 BY 1 UNTIL XIND > P-IND.
           IF TOT-AMOUNT = 0 GO TO FC3.
           MOVE CC-AMOUNT TO TOT-AMOUNT.
       FC33.
           ADD 1 TO Z
           MOVE CC-CLAIM TO LAST-CLAIM(Z)
           MOVE CHARCUR-KEY TO CCKEY-TAB(Z)
           DISPLAY Z " " ALF-5 " " DISPLAY-DATE-CC " PC " CC-PAYCODE
           " CLAIM " CC-CLAIM " AMOUNT " NEF-8 " HS " CC-PROC 
           " " CC-DIAG " " CC-DOCP
           IF CC-COLLT = "1" DISPLAY "COLLECTION " BELL.
           IF CC-AUTH = "1" PERFORM READ-AUTH 
      *     ADD 1 TO Z
            IF AUTH-NUM NOT = SPACE
             DISPLAY "AUTHNUM= " AUTH-NUM
            END-IF
            IF AUTH-NDC NOT = SPACE
             DISPLAY "AUTHNDC= " AUTH-NDC
            END-IF
           END-IF
           MOVE SPACE TO ANS
           IF IN-FIELD = "FX" OR ACTION = "FX" PERFORM FXCC-1.
           IF (IN-FIELD = "FCC" OR ACTION = "FCC") NEXT SENTENCE
           ELSE
           PERFORM FPS VARYING XIND FROM 1 BY 1 UNTIL XIND > P-IND.
           IF ANS NOT = SPACES GO TO FP1-EXIT.
           IF Z = 9 MOVE 0 TO Z MOVE 0 TO Y ACCEPT ANS
           IF ANS NOT = SPACES GO TO FP1-EXIT.
           GO TO FC3.
       FC6. MOVE G-GARNO TO CD-KEY8 MOVE "000" TO CD-KEY3.
           START CHARFILE KEY > CHARFILE-KEY INVALID GO TO FP1-EXIT.
       FC5. READ CHARFILE NEXT AT END GO TO FP1-EXIT.
           IF CD-KEY8 NOT = G-GARNO GO TO FP1-EXIT.
           IF ALF-3 =  "000" OR ALF-3 = CD-PAYCODE NEXT SENTENCE
           ELSE GO TO FC5.
           IF (CD = 0)
           OR ((CD = 1) AND (DATE-OF-CHARGE = CD-DATE-T OR SPACE))
           NEXT SENTENCE
           ELSE GO TO FC5.
           IF (IN-FIELD = "FT") OR (ACTION = "FT")
           MOVE CD-CLAIM TO CLAIM
           MOVE CD-AMOUNT TO TOT-AMOUNT
           MOVE CD-ASSIGN TO ALF-1-1
           PERFORM FT1 GO TO FC5.
           MOVE CD-AMOUNT TO TOT-AMOUNT NEF-8
           MOVE CD-CLAIM TO CLAIM
           MOVE CD-DATE-T TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           MOVE DISPLAY-DATE TO DISPLAY-DATE-CD
           MOVE SPACE TO ALF-5
           IF DP NOT = 1 GO TO FC55-0.
           MOVE G-GARNAME TO DATAIN
           MOVE CD-PATID TO EIGHTPARTID
           IF EIGHT-1 = "P" MOVE CD-PATID TO P-PATNO
           READ PATFILE  MOVE P-PATNAME TO DATAIN.
           UNSTRING DATAIN DELIMITED BY ";" INTO ALF-14 ALF-5.
       FC55-0.
           IF IN-FIELD = "FX" OR ACTION = "FX" PERFORM FXCD-1.
           IF IN-FIELD = "FCC" OR ACTION = "FCC" GO TO FC55.
           IF FZ = -1 GO TO FC55.
           PERFORM FPZ VARYING XIND FROM 1 BY 1 UNTIL XIND > P-IND.
           IF TOT-AMOUNT = 0 GO TO FC5.
           MOVE CD-AMOUNT TO TOT-AMOUNT.
       FC55.
           ADD 1 TO Z.
           MOVE CD-CLAIM TO LAST-CLAIM(Z)
           MOVE SPACE TO CCKEY-TAB(Z).
           DISPLAY Z " " ALF-5 " " DISPLAY-DATE-CD " PC " CD-PAYCODE
           " CLAIM " CD-CLAIM " AMOUNT " NEF-8 
           " DL " CD-PROC " " CD-DIAG " " CD-DOCP
           IF CD-COLLT = "1" DISPLAY "COLLECTION" BELL.
           IF CD-PAPER = "B" DISPLAY "CLAIM SENT".
           IF CD-AUTH = "1" PERFORM READ-AUTH-CD 
      *     ADD 1 TO Z
            IF AUTH-NUM NOT = SPACE
             DISPLAY "AUTHNUM= " AUTH-NUM
            END-IF
            IF AUTH-NDC NOT = SPACE
             DISPLAY "AUTHNDC= " AUTH-NDC
            END-IF
           END-IF
           MOVE SPACE TO ANS
           IF (IN-FIELD = "FCC" OR ACTION = "FCC") NEXT SENTENCE
           ELSE
           PERFORM FPS VARYING XIND FROM 1 BY 1 UNTIL XIND > P-IND.
           IF ANS NOT = SPACES GO TO FP1-EXIT.
           IF Z = 9 MOVE 0 TO Z MOVE 0 TO Y ACCEPT ANS
           IF ANS NOT = SPACES GO TO FP1-EXIT.
           GO TO FC5.
       FPS. IF C-TAB(XIND) = CLAIM
           ADD A-TAB(XIND) TO TOT-AMOUNT
           MOVE A-TAB(XIND) TO NEF-8
           MOVE NEF-8 TO ALF-11
           MOVE TOT-AMOUNT TO NEF-8
           MOVE D-TAB(XIND) TO TEST-DATE
           MOVE DE-TAB(XIND) TO TEST-DATE1


           IF (IN-FIELD = "FX" OR ACTION = "FX")
           DISPLAY "        "
           T-MM OF TEST-DATE "/" T-DD OF TEST-DATE "/"
           T-CC OF TEST-DATE  T-YY OF TEST-DATE
           "    " PC-TAB(XIND) " " DN-TAB(XIND) 
           "     " ALF-11 "  " NEF-8 "              "
           T-MM OF TEST-DATE1 "/" T-DD OF TEST-DATE1 "/"
           T-CC OF TEST-DATE1  T-YY OF TEST-DATE1
           ELSE
           DISPLAY "        "
           T-MM OF TEST-DATE "/" T-DD OF TEST-DATE "/"
           T-CC OF TEST-DATE  T-YY OF TEST-DATE
           "    " PC-TAB(XIND) " " DN-TAB(XIND) 
           "     " ALF-11 "  " NEF-8 
           END-IF

           ADD 1 TO Y
           IF Y > 13
           MOVE 0 TO Y
           ACCEPT ANS.
       FP1-EXIT. EXIT.
       FXCC-1.
           MOVE CC-PROC TO PROC-KEY
           READ PROCFILE INVALID DISPLAY "BAD PROC." 
           END-READ
           IF CC-DOCR NOT = "000"
            MOVE CC-DOCR TO REF-KEY
            READ REFPHY INVALID DISPLAY "BAD REF"
            END-READ
           END-IF
           MOVE SPACE TO ALF10
           IF CC-DAT1 NOT = "00000000" 
            MOVE CC-DAT1 TO TEST-DATE
            MOVE CORR TEST-DATE TO DISPLAY-DATE
            MOVE DISPLAY-DATE TO ALF10
           END-IF
           DISPLAY "        "   PROC-TITLE " " REF-NAME " " 
                         ALF10.
           ADD 1 TO Y.
       FXCD-1.
           MOVE CD-PROC TO PROC-KEY
           READ PROCFILE INVALID DISPLAY "BAD PROC." 
           END-READ
           IF CD-DOCR NOT = "000"
            MOVE CD-DOCR TO REF-KEY
            READ REFPHY INVALID DISPLAY "BAD REF"
            END-READ
           END-IF
           MOVE SPACE TO ALF10
           IF CD-DAT1 NOT = "00000000" 
            MOVE CD-DAT1 TO TEST-DATE
            MOVE CORR TEST-DATE TO DISPLAY-DATE
            MOVE DISPLAY-DATE TO ALF10
           END-IF
           DISPLAY "       "  PROC-TITLE " " REF-NAME " " 
           ALF10.
           ADD 1 TO Y.

       FPZ.  IF C-TAB(XIND) = CLAIM
           ADD A-TAB(XIND) TO TOT-AMOUNT.
       FT1. PERFORM FPZ VARYING XIND FROM 1 BY 1 UNTIL XIND
           > P-IND.
           IF ALF-1-1 = "A" ADD TOT-AMOUNT TO TOT-ASSIGNED
           ELSE ADD TOT-AMOUNT TO TOT-UNASSIGNED.
       1205-ADD-LOOP.
           SET INDX TO ADD-FLD(ADD-KEY).
           PERFORM 2050-DISPLAY THRU 4910DEE.
       1200-FIND. START PAYFILE KEY > PAYFILE-KEY INVALID
           DISPLAY " END OF FILE" GO TO 1000-ACTION.
       1200-FIND-20.
           MOVE 0 TO X.
           MOVE 0 TO FLAG.
           PERFORM 1200-SEARCH THRU 1200-SEARCH-EXIT.
           IF FLAG = 1 DISPLAY "END OF FILE FOUND"
           DISPLAY "END OF SEARCH"
           GO TO 1000-ACTION.
       1200-FIND-QUES.
           DISPLAY "?".
           ACCEPT ANS.
           IF ANS = SPACES GO TO 1200-FIND-20.
           IF ANS = "?"
           DISPLAY "A <CR> WILL PRODUCE 9 MORE NAMES"
           DISPLAY "ANYTHING ELSE WILL RETURN YOU TO THE OPTION COMMAND"
           GO TO 1200-FIND-QUES.
           GO TO 1000-ACTION.
       1200-SEARCH.
           ADD 1 TO X.
           IF X > 9 GO TO 1200-SEARCH-EXIT.
           READ PAYFILE NEXT AT END MOVE 1 TO FLAG
           GO TO 1200-SEARCH-EXIT.
           MOVE PD-DATE-T TO TEST-DATE
           MOVE CORR TEST-DATE TO DISP-DATE
           MOVE PD-AMOUNT TO NEF-5
           MOVE PAYFILE-KEY TO LASTPAY-TAB(X).
           DISPLAY X " " PD-KEY8  " " DISP-DATE " " NEF-5 " " PD-PAYCODE
           " " PD-CLAIM  " " PD-DENIAL " " PD-DATE-E " " PD-NAME
           GO TO 1200-SEARCH.
       1200-SEARCH-EXIT.
           EXIT.
       1300DEL.
           READ PAYFILE WITH LOCK INVALID DISPLAY "NOT ON FILE"
           GO TO 1000-ACTION.
           IF PAYFILE-STAT NOT = "00"
           DISPLAY "STATUS = " PAYFILE-STAT
           DISPLAY "RECORD BEING USED. NO DELETE CAN BE MADE"
           GO TO 1000-ACTION.
           MOVE PD-DATE-T TO TEST-DATE
           MOVE CORR TEST-DATE TO DISP-DATE
           MOVE PD-AMOUNT TO NEF-5
           DISPLAY "   " PD-KEY8  " " DISP-DATE " " NEF-5 " " PD-PAYCODE
           " " PD-CLAIM  " " PD-DENIAL " " PD-NAME.
       1300DEL1. DISPLAY "DELETE Y/N ?".
           ACCEPT ANS.
           IF ANS = "?"
           DISPLAY "Y = YES TO DELETE THE RECORD DISPLAYED"
           DISPLAY "N = NO"
           GO TO 1300DEL1.
           IF ANS NOT = "Y" DISPLAY "NO DELETE"
           UNLOCK PAYFILE RECORD
           GO TO 1000-ACTION.
           DELETE PAYFILE RECORD.
      *    ISQUIET PAYFILE WITH 1 AND 3
           DISPLAY "RECORD DELETED" GO TO 1000-ACTION.
       1400-CHANGE-IT.
           READ PAYFILE WITH LOCK INVALID KEY DISPLAY "NOT ON FILE"
             GO TO 1000-ACTION.
           IF PAYFILE-STAT NOT = "00" 
           DISPLAY "RECORD BEING CHANGED"
           DISPLAY "STATUS = " PAYFILE-STAT
           GO TO 1000-ACTION.
       1400-CHANGE-PROCESS.
           MOVE SPACES TO RIGHT-2 IN-FIELD.
           DISPLAY "FIELD CODE,DATA?".
           ACCEPT DATAIN.
           IF DATAIN = "X"
               DISPLAY "NO CHANGE"  GO TO 1000-ACTION.
           IF DATAIN = "L" PERFORM LPAY-1 GO TO 1400-CHANGE-PROCESS.
           IF DATAIN = "UP"

             GO TO 5000-WRITE-PAYFILE.
           IF DATAIN = "?"
               DISPLAY "UP TO UPDATE CHANGES MADE"
               DISPLAY "L TO LIST THE RECORD"
               DISPLAY "   OR"
               DISPLAY "ENTER THE FIELD #,(NEW DATA)"
               DISPLAY "SEPARATED BY A COMMA. THE VALID FIELDS ARE:"
           DISPLAY "3=AMOUNT 4=PAYORCODE 5=DENIAL CODE 6=CLAIM 7=DATE"
               GO TO 1400-CHANGE-PROCESS.
           UNSTRING DATAIN DELIMITED BY "," INTO RIGHT-2 IN-FIELD.
           INSPECT RIGHT-2 REPLACING LEADING SPACE BY "0".
           IF RIGHT-2 NOT NUMERIC
               DISPLAY "FIELD CODE NOT NUMERIC"
               GO TO 1400-CHANGE-PROCESS.
           IF RIGHT-2 = "00"
               DISPLAY "FIELD CODE CANNOT BE ZERO OR BLANK"
               GO TO 1400-CHANGE-PROCESS.
           IF RIGHT-2 > "02" AND RIGHT-2 < "08"
           MOVE RIGHT-2 TO LOW-NUM
           SET INDX TO LOW-NUM
           GO TO 2060-GO-TO.
           DISPLAY "FIELD # MUST BE 3-7 ".
           GO TO 1400-CHANGE-PROCESS.
       2050-DISPLAY.
           IF ACTION NOT = "A" GO TO 2050-D.
           IF INDX = 3 OR 4 OR 5 OR 7 NEXT SENTENCE
           ELSE GO TO 2050-D.
           IF INDX = 3 AND DF-AMOUNT = 10 GO TO 2050-D.
           IF INDX = 4 AND DF-PAYCODE = "000" GO TO 2050-D.
           IF INDX = 5 AND DF-DENIAL = "00" GO TO 2050-D.
           IF INDX = 7 AND DF-DATE = "00000000" GO TO 2050-D.
           IF INDX = 3 MOVE DF-AMOUNT TO PD-AMOUNT GO TO 4900DEE.
           IF INDX = 4 MOVE DF-PAYCODE TO PD-PAYCODE GO TO 4900DEE.
           IF INDX = 5 MOVE DF-DENIAL TO PD-DENIAL GO TO 4900DEE.
           IF INDX = 7 MOVE DF-DATE TO PD-DATE-T GO TO 4900DEE.
       2050-D.
           DISPLAY DESC-FLD(INDX) "?".
       2051-INPUT.
           ACCEPT IN-FIELD.
           IF IN-FIELD = "FT"
           MOVE PAYFILE01 TO PAYBACK01
           PERFORM FP1 THRU FP1-EXIT
           MOVE PAYBACK01 TO PAYFILE01
           MOVE TOT-ASSIGNED TO NEF-8
           DISPLAY "INSURANCE = " NEF-8
           MOVE TOT-UNASSIGNED TO NEF-8
           DISPLAY "PERSONAL  = " NEF-8
           GO TO 2050-D.
           IF IN-FIELD = "BK" GO TO 2062-BACK.
           IF IN-FIELD = "X" SET ADD-KEY TO 8
             GO TO 4910DEE.
           GO TO 2060-GO-TO.
       2062-BACK.
            IF ADD-KEY = 1 MOVE "X" TO IN-FIELD
           SET ADD-KEY TO 8 GO TO 4910DEE.
           IF INDX = 3
           MOVE DISPLAY-FIELD(NUMBER-OF-FIELDS) TO X
           MOVE 0 TO FLAG-TABLE(X)
           SUBTRACT 1 FROM NUMBER-OF-FIELDS
           MOVE DISPLAY-FIELD(NUMBER-OF-FIELDS) TO X
           MOVE 0 TO FLAG-TABLE(X)
           SUBTRACT 1 FROM NUMBER-OF-FIELDS
           SET ADD-KEY DOWN BY 1
           SET INDX TO ADD-FLD(ADD-KEY)
           GO TO 2050-DISPLAY.
           MOVE 0 TO FLAG-TABLE(NUMBER-OF-FIELDS)
           SUBTRACT 1 FROM NUMBER-OF-FIELDS
           SET ADD-KEY DOWN BY 1
           SET INDX TO ADD-FLD(ADD-KEY)
           GO TO 2050-DISPLAY.
       2060-GO-TO.
           MOVE 0 TO FLAG.
           MOVE LEN-TAB(INDX) TO Q ADD 1 TO Q.
           IF IN-FIELD-TAB(Q) NOT = " " MOVE "1" TO FLAG.
           IF FLAG = 1 DISPLAY "DATA TOO LONG, MUST NOT BE GREATER "
             "THAN " LEN-TAB(INDX) ".".
           IF FLAG = 1 AND ACTION = "C" GO TO 1400-CHANGE-PROCESS.
           IF FLAG = 1 AND ACTION = "A" GO TO 2050-DISPLAY.
       2061-GO-TO.
           GO TO
           2100-PAYFILE-KEY 2120-NAME 2200-AMOUNT 2170-PAYCODE
           2300-DENIAL
           2100-CLAIM 4000-DATE-T
           DEPENDING ON INDX.
       2000TI.
           IF ACTION = "A" GO TO 2050-DISPLAY.
           IF ACTION = "C" GO TO 1400-CHANGE-PROCESS.
           IF ACTION = "FG" OR "SG" OR "FP" OR "SP" OR "S"
           GO TO 1000-ACTION.
       2100-PAYFILE-KEY.
           IF IN-FIELD = "?"
           DISPLAY "ENTER  A GUARANTOR ACCT #"
           GO TO 2000TI.
           IF (IN-FIELD-TAB(1) > "0" ) AND ( IN-FIELD-TAB(1) < "9"
           OR IN-FIELD-TAB(1) = "9" ) AND ( IN-FIELD-TAB(2) = " "
           AND IN-FIELD-TAB(3) = " " AND IN-FIELD-TAB(4) = " " )
            MOVE IN-FIELD-1 TO FLAG
           MOVE LAST-TAB(FLAG) TO IN-FIELD-8.
           MOVE IN-FIELD-8 TO G-GARNO.
           READ GARFILE INVALID KEY DISPLAY " NOT ON FILE"
           GO TO 2000TI.
           MOVE G-GARNAME TO PD-NAME.
           ADD 1 TO NUMBER-OF-FIELDS.
           MOVE 2 TO DISPLAY-FIELD(NUMBER-OF-FIELDS).
           DISPLAY G-GARNO " " G-PRINS "/" G-SEINS "/" G-TRINS
           " " G-GARNAME.
           IF CM = 1 PERFORM CM-2 THRU CM-4.
           IF G-DUNNING > "3" DISPLAY "COL ACCT " G-DUNNING "/" G-COLLT
           " " BELL.
           IF G-PRINS = "012" OR G-SEINS = "012" OR G-TRINS = "012"
           DISPLAY "COURTESY ACCOUNT" BELL.
           MOVE G-GARNO TO SAVE-GARNO.
       2120-PAYFILE-KEY.
           MOVE 0 TO XYZ.
       2130-PAYFILE-KEY.
           ADD 1 TO XYZ.
           MOVE SPACE TO ALF-11.
           MOVE XYZ TO ABC.
           STRING IN-FIELD-8 ABC DELIMITED BY "@" INTO ALF-11.
           MOVE ALF-11 TO PAYFILE-KEY.
           READ PAYFILE INVALID KEY GO TO 4900DEE.
           IF XYZ = 999 DISPLAY "THIS ACCOUNT HAS 999 TRANSACTIONS"
           DISPLAY "IN THE FILE ALREADY. NO MORE ARE ALLOWED."
           GO TO 2000TI.
           GO TO 2130-PAYFILE-KEY.
       2400-FIND.
           MOVE SPACES TO  FFTAB1001 ACTION NAME-LAST.
           UNSTRING DATAIN DELIMITED BY "," INTO ACTION NAME-LAST
           FFTAB1001.
           MOVE NAME-LAST TO LLTAB2401.
           SET FF TO 0
           SET LL TO 1
           PERFORM FF-NAME VARYING XIND FROM 10 BY -1 UNTIL XIND < 1.
           PERFORM LL-NAME VARYING XIND FROM 24 BY -1 UNTIL XIND < 1.
           IF LL = 0 SET LL TO 1.
           IF LLTAB24(LL) = "/"
           MOVE " " TO LLTAB24(LL) SET LL TO 24.
           MOVE SPACE TO G-GARNO.
           IF ACTION = "SG" MOVE NAME-LAST TO G-GARNO
           ELSE MOVE NL-3 TO G-GARNO.
           IF LLTAB24(1) = "*" MOVE SPACE TO G-GARNO.
           START GARFILE KEY > G-GARNO INVALID
           DISPLAY " END OF FILE" GO TO 2000TI.
       2400-FIND-20.
           MOVE 0 TO X YYY.
           MOVE 0 TO FLAG SGFLAG.
           PERFORM 2400-SEARCH THRU 2400-SEARCH-EXIT.
           IF SGFLAG = 1 GO TO 1000-ACTION.
           IF FLAG = 0 GO TO 2400-FIND-QUES.
           IF FLAG = 1 DISPLAY "END OF FILE FOUND"
           ELSE DISPLAY "NO MORE MATCHES ON NAME".
           DISPLAY "END OF SEARCH"
           GO TO 1000-ACTION.
       2400-FIND-QUES.
           DISPLAY "?".
           ACCEPT ANS.
           IF ANS = SPACES GO TO 2400-FIND-20.
           IF ANS = "?"
           DISPLAY "A <CR> WILL PRODUCE 9 MORE NAMES"
           DISPLAY "ANYTHING ELSE WILL RETURN YOU TO THE OPTION COMMAND"
           GO TO 2400-FIND-QUES.
           GO TO 1000-ACTION.
       2400-SEARCH.
           IF X = 9 GO TO 2400-SEARCH-EXIT.
           READ GARFILE NEXT AT END MOVE 1 TO FLAG
           GO TO 2400-SEARCH-EXIT.
           ADD 1 TO YYY.
           IF YYY < 990 GO TO P456.
           MOVE 1 TO SGFLAG
           DISPLAY "CONTINUE LOOKING ? <CR> FOR YES".
           ACCEPT DATAIN.
           IF DATAIN NOT = SPACE GO TO 2400-SEARCH-EXIT.
           MOVE 0 TO YYY SGFLAG.
       P456.
           IF ACTION = "SG" GO TO 2400-S.
           MOVE SPACE TO NAME-TEST XFFTAB1001.
           UNSTRING G-GARNAME DELIMITED BY ";" INTO NAME-TEST XFFTAB1001.
           IF ((NT-31 = NL-31) AND (NL-32 = SPACE OR NL-33 = SPACE))
           OR (NT-3 = NL-3) OR (LLTAB24(1) = "*") NEXT SENTENCE
           ELSE MOVE 2 TO FLAG GO TO 2400-SEARCH-EXIT.
           MOVE NAME-TEST TO XLLTAB2401.
           MOVE 0 TO FLAGX 
           IF LLTAB24(1) NOT = "*"
           PERFORM AQ1 VARYING YIND FROM 1 BY 1 UNTIL YIND > LL.
           IF FLAGX = 1 GO TO 2400-SEARCH.
           PERFORM AQ2 VARYING YIND FROM 1 BY 1 UNTIL YIND > FF.
           IF FLAGX = 1 GO TO 2400-SEARCH.
       2400-S.
           IF ALF-3 NOT = "000" AND ALF-3 NOT = G-PRINS AND ALF-3 NOT =
           G-SEINS AND ALF-3 NOT = G-TRINS GO TO 2400-SEARCH.
           ADD 1 TO X
           MOVE 0 TO YYY
           MOVE G-DOB TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           INSPECT T-MM OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           INSPECT T-DD OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           DISPLAY X " " G-GARNO " " DISPLAY-DATE " " G-GARNAME 
           " " G-PRINS " " G-PRIPOL " " G-SEINS " " G-TRINS
           MOVE G-GARNO TO LAST-TAB(X)
           GO TO 2400-SEARCH.
       2400-SEARCH-EXIT.
           EXIT.
       3600-FIND.
           MOVE SPACES TO  ACTION FFTAB1001 NAME-LAST.
           UNSTRING DATAIN DELIMITED BY "," INTO ACTION NAME-LAST
           FFTAB1001.
           MOVE SPACE TO P-PATNO.
           IF ACTION = "SP" MOVE NAME-LAST TO P-PATNO
           ELSE MOVE NL-3 TO P-PATNO.
           IF LLTAB24(1) = "*" MOVE SPACE TO P-PATNO.
           START PATFILE KEY > P-PATNO INVALID
           DISPLAY " END OF FILE" GO TO 2000TI.
           MOVE NAME-LAST TO LLTAB2401.
           SET FF TO 0
           SET LL TO 1
           PERFORM FF-NAME VARYING XIND FROM 10 BY -1 UNTIL XIND < 1.
           PERFORM LL-NAME VARYING XIND FROM 24 BY -1 UNTIL XIND < 1.
           IF LL = 0 SET LL TO 1.
           IF LLTAB24(LL) = "/"
           MOVE " " TO LLTAB24(LL) SET LL TO 24.
       3600-FIND-20.
           MOVE 0 TO X.
           MOVE 0 TO FLAG.
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
           IF X = 9 GO TO 3600-SEARCH-EXIT.
           READ PATFILE NEXT AT END MOVE 1 TO FLAG
           GO TO 3600-SEARCH-EXIT.
           MOVE P-GARNO TO G-GARNO.
           READ GARFILE INVALID KEY DISPLAY "THIS PATIENT HAS NO "
           "VALID GUARANTOR #."
           GO TO 9100-CLOSE-MASTER-FILE.
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
           PERFORM AQ1 VARYING YIND FROM 1 BY 1 UNTIL YIND > LL.
           IF FLAGX = 1 GO TO 3600-SEARCH.
           PERFORM AQ2 VARYING YIND FROM 1 BY 1 UNTIL YIND > FF.
           IF FLAGX = 1 GO TO 3600-SEARCH.
       3600-S.
           ADD 1 TO X
           MOVE G-GARNO TO LAST-TAB(X)
           MOVE P-PATNO TO LAST-PAT(X)
           MOVE P-DOB TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           INSPECT T-MM OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           INSPECT T-DD OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           DISPLAY X " " P-PATNO " " P-PATNAME " " P-GARNO " " 
           G-PRINS "/" G-SEINS "/" G-TRINS " " G-CITY
           GO TO 3600-SEARCH.
       3600-SEARCH-EXIT.
           EXIT.
       2120-NAME.
           DISPLAY "CAN""T CHANGE NAME ON A TRANSACTION!".
           DISPLAY "CHANGE THE GUAR. ACCT # TO GET A CHANGE OF NAME.".
           GO TO 2000TI.
       2200-AMOUNT.
           IF IN-FIELD = "?"
           DISPLAY "ENTER THE AMOUNT OF PAY"
           DISPLAY "EXAMPLE: $1,234,56 = 1234.56"
           DISPLAY "CENTS PORTION ASSUMED TO BE 00 IF NOT TYPED"
           DISPLAY "DECIMAL POINT IS OPTIONAL WHEN THIS IS THE CASE"
           DISPLAY "FC TO FIND CLAIMS FOR THIS ACCOUNT"
           GO TO 2000TI.
           IF IN-FIELD-2 = "FZ" PERFORM FZ1 GO TO 2000TI.
           IF IN-FIELD-1 = SPACE DISPLAY "INVALID" GO TO 2000TI.
           MOVE SPACE TO DATAIN ALF-3.
           UNSTRING IN-FIELD DELIMITED BY "=" INTO DATAIN ALF-3.
           IF ALF-3 = SPACE MOVE "000" TO ALF-3 GO TO AX1.
           IF (ALF-3 NOT NUMERIC) OR (ALF-3 = "000") 
            OR (DATAIN NOT = "FC" AND NOT = "FX") 
            DISPLAY "INVALID" GO TO 2000TI.
           MOVE DATAIN TO IN-FIELD.
       AX1.
           IF IN-FIELD = "FC" OR "FT" OR "FCC" OR "FX" MOVE PAYFILE01
           TO PAYBACK01
           PERFORM FP1 THRU FP1-EXIT MOVE PAYBACK01 TO PAYFILE01
           GO TO 2000TI.
           MOVE SPACES TO SIGN-DOLLAR CENTS.
           UNSTRING IN-FIELD DELIMITED BY "." INTO SIGN-DOLLAR CENTS.
           INSPECT CENTS REPLACING ALL " " BY "0".
           IF CENTS NOT NUMERIC MOVE "?" TO IN-FIELD
           GO TO 2200-AMOUNT.
           MOVE SPACES TO RIGHT-4.
           UNSTRING SIGN-DOLLAR DELIMITED BY " " INTO RIGHT-4
           INSPECT RIGHT-4 REPLACING LEADING " " BY "0"
           IF RIGHT-4 NOT NUMERIC MOVE "?" TO IN-FIELD
           GO TO 2200-AMOUNT.
           STRING RIGHT-4 CENTS DELIMITED BY "Z" INTO ALF-6
           MOVE ALF-6 TO NUM-6
           DIVIDE NUM-6 BY -100 GIVING PD-AMOUNT
            GO TO 4900DEE.
       2170-PAYCODE.
           IF IN-FIELD = "?"
           DISPLAY "ENTER 3 DIGIT PAYCODE/INSURANCE CODE"
           DISPLAY "P FOR PRIMARY INS, S FOR DECONDARY INS"
           DISPLAY "FI = FIND INSURANCE CODE"
           GO TO 2000TI.
           IF IN-FIELD = "FI" PERFORM INS-1 THRU INS-1-EXIT
           GO TO 2000TI.
           IF IN-FIELD = "FC" OR "FT" OR "FCC" OR "FX" MOVE PAYFILE01
           TO PAYBACK01 PERFORM FP1 THRU FP1-EXIT
           MOVE PAYBACK01 TO PAYFILE01
           GO TO 2000TI.
           IF IN-FIELD = "P" MOVE G-PRINS TO IN-FIELD.
           IF IN-FIELD = "S" MOVE G-SEINS TO IN-FIELD.
           MOVE SPACES TO RIGHT-3
           UNSTRING IN-FIELD-3 DELIMITED BY " " INTO RIGHT-3
           INSPECT RIGHT-3 REPLACING LEADING " " BY "0"
           IF RIGHT-3 NOT NUMERIC DISPLAY "INVALID" GO TO 2000TI.
           IF RIGHT-3 < "001" DISPLAY "INVALID" GO TO 2000TI.
           MOVE RIGHT-3 TO INS-KEY
           READ INSFILE INVALID DISPLAY "UNDEFINED INSURANCE CODE"
           GO TO 2000TI.
           IF IP = 1 DISPLAY INS-NAME.
            IF (INS-ASSIGN = "A")
            AND NOT (INS-KEY > "009" AND < "023")
            AND (INS-KEY NOT = G-PRINS) 
            AND (INS-KEY NOT = G-SEINS) 
            AND (INS-KEY NOT = G-TRINS)
            DISPLAY "PAYCODE DOES NOT MATCH INSURANCE COVERAGE"
            DISPLAY "USE THIS CODE ? Y"
            ACCEPT ANS
            IF ANS NOT = "Y" GO TO 2000TI
            END-IF
           END-IF
           IF ((RIGHT-3 = "019" OR "010" OR "015" OR "017")
           AND (PD-AMOUNT < 0.00))
           OR ((PD-AMOUNT > 0.00) AND (RIGHT-3 NOT = "010"
           AND NOT = "015" AND NOT = "017" AND NOT = "019"))
           COMPUTE PD-AMOUNT = -1 * PD-AMOUNT.
           MOVE RIGHT-3 TO PD-PAYCODE GO TO 4900DEE.
       4000-DATE-T.
           IF IN-FIELD = "?"
           DISPLAY "PAY DATE DD OR MMDD OR  MMDDYY OR MMDDYYYY FORMAT"
           DISPLAY "TODAY""S DATE, <CR>"
               GO TO 2000TI.
           IF IN-FIELD = "FC" OR "FT" OR "FCC" OR "FX" MOVE PAYFILE01
           TO PAYBACK01 PERFORM FP1 THRU FP1-EXIT
           MOVE PAYBACK01 TO PAYFILE01
           GO TO 2000TI.
           IF IN-FIELD-8 = SPACES ACCEPT PD-DATE-T FROM CENTURY-DATE
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
           IF IN-FIELD-8 NUMERIC NEXT SENTENCE ELSE MOVE "?"
           TO IN-FIELD GO TO 4000-DATE-T.
           MOVE IN-FIELD-8 TO INPUT-DATE.
           IF T-MM OF INPUT-DATE < 01 OR > 12
               MOVE "?" TO IN-FIELD
           GO TO 4000-DATE-T.
           IF (T-DD OF INPUT-DATE > DAYS-IN-MONTH(T-MM OF INPUT-DATE))
             OR (T-DD OF INPUT-DATE = 00)
               MOVE "?" TO IN-FIELD
           GO TO 4000-DATE-T.
           MOVE CORR INPUT-DATE TO TEST-DATE
           IF T-DATE < TEST-DATE 
            DISPLAY "FUTURE DATE NOT ALLOWED"
            DISPLAY "TO USE TYPE:   YES"
            ACCEPT ALF-3
             IF ALF-3 = "YES"
             MOVE TEST-DATE TO PD-DATE-T
             GO TO 4900DEE
             END-IF
           MOVE "?" TO IN-FIELD GO TO 4000-DATE-T.
           MOVE TEST-DATE TO PD-DATE-T
           GO TO 4900DEE.
       2100-CLAIM.
           IF IN-FIELD = "?"
           DISPLAY "ENTER THE 6-DIGIT CLAIM NUMBER, OR"
           DISPLAY "D = DISTRIBUTE AGAINST ANY UNASSIGNED CLAIM "
           DISPLAY "OR FC TO FIND CLAIM AGAIN" GO TO 2000TI.
           MOVE SPACE TO DATAIN ALF-3.
           IF IN-FIELD-2 = "FZ" PERFORM FZ1 GO TO 2000TI.
           UNSTRING IN-FIELD DELIMITED BY "=" INTO DATAIN ALF-3.
           IF ALF-3 = SPACE MOVE "000" TO ALF-3 GO TO AX2.
           IF (ALF-3 NOT NUMERIC) OR (ALF-3 = "000") OR
           (DATAIN NOT = "FC" AND NOT = "FT" AND NOT = "FX") 
           DISPLAY "INVALID"
           GO TO 2000TI.
           MOVE DATAIN TO IN-FIELD.
       AX2.
           IF IN-FIELD = "FC" OR "FT" OR "FCC" OR "FX" MOVE PAYFILE01
           TO PAYBACK01 PERFORM FP1 THRU FP1-EXIT
           MOVE PAYBACK01 TO PAYFILE01
           GO TO 2000TI.
           IF ACTION = "A" AND
           IN-FIELD = "D" MOVE PAYFILE01 TO PAYBACK01
           MOVE PD-AMOUNT TO TOT-AMOUNT
           MOVE 0 TO XYZ
           PERFORM DC1 THRU DC1-EXIT MOVE PAYBACK01 TO PAYFILE01
           GO TO 4900DEE.
           IF IN-FIELD-TAB(1) NOT = "G" GO TO AX3.
           STRING IN-FIELD-TAB(2) IN-FIELD-TAB(3) IN-FIELD-TAB(4)
           DELIMITED BY "#" INTO IN-FIELD-3.
           MOVE SPACE TO RIGHT-3
           UNSTRING IN-FIELD-3 DELIMITED BY " " INTO RIGHT-3
           INSPECT RIGHT-3 REPLACING LEADING " " BY "0".
           MOVE G-GARNO TO CC-KEY8 MOVE RIGHT-3 TO CC-KEY3.
           READ CHARCUR INVALID DISPLAY "INVALID" GO TO 2000TI.
           MOVE CC-CLAIM TO PD-CLAIM GO TO 4900DEE.
       AX3.
           IF ((IN-FIELD-TAB(1) > "0" AND < "9")
           OR (IN-FIELD-TAB(1) = "9"))
           AND (IN-FIELD-TAB(2) = " " AND IN-FIELD-TAB(3) = " ")
           MOVE IN-FIELD-TAB(1) TO X MOVE LAST-CLAIM(X) TO IN-FIELD.
           IF IN-FIELD-6 NOT NUMERIC MOVE "?" TO IN-FIELD
           GO TO 2100-CLAIM.
           MOVE IN-FIELD-6 TO PD-CLAIM
           MOVE 0 TO FLAG
           PERFORM CUR-1 THRU CUR-END
           IF FLAG = 1 GO TO 4900DEE.
           PERFORM DAL-1 THRU DAL-END
           IF FLAG = 1 GO TO 4900DEE.
           MOVE "?" TO IN-FIELD GO TO 2000TI.
       2300-DENIAL.
           IF IN-FIELD = "?"
           DISPLAY "07 = CASH CREDIT      08 = CASH DEBIT"
           DISPLAY "14 = REDUCTION CREDIT 15 = NON-CASH DEBIT "
           DISPLAY " "
           DISPLAY "DD = DEDUCT. NOT MET  NC = NON COVERED SERVICE"
           DISPLAY "NR = NO INS. RESPONSE CE = BAD CERT. # "
           DISPLAY "BE = BILLING ERROR    RE = REDUCTION ERROR"
           DISPLAY "CB = CREDIT BUREAU    IN = INSURANCE ERROR"
           DISPLAY "CP = COPAY AMOUNT     PI = PRIMARY INS PAID"
           DISPLAY "WC = WORK COMP        TO = TOO OLD FOR INS."
           DISPLAY "DI = DEFERRED INCOME  ON = OUT-OF-NETWORK"
           DISPLAY "DA = NON-COVERED DATE RF = NO REFERRAL   " 
           DISPLAY "PP = PAID PATIENT     OI = OTHER INSURANCE"
           DISPLAY "RS = NO PAT. RSPONC <CR> = NO DENIAL CODE"
           DISPLAY "CI = CO-INSURANCE     TM = TOO MANY OCCURANCES"
           DISPLAY "NF = NON-SUFF. FUNDS  NM = NOT MEDICALLY NECESSARY"
           GO TO 2000TI.
           IF IN-FIELD-2 = "DD" OR "NC" OR "NR" OR "CE" OR "DA" OR "BE"
           OR SPACE OR "CB" OR "14" OR "CP" OR "WC" OR "TO" OR "DI"
           OR "IN" OR "RE" OR "PI" OR "ON" OR "RF" OR "PP" OR "07"
           OR "08" OR "15" OR "OI" OR "RS" OR "CI" OR "TM" OR "NF"
           OR "NM"
           NEXT SENTENCE ELSE MOVE "?" TO IN-FIELD GO TO 2300-DENIAL.
           IF (IN-FIELD-2 = "14" OR "07" OR "08" OR "15" OR "DI") AND
           ((PD-PAYCODE > "006" AND < "020") AND
           (PD-PAYCODE NOT = "018"))
           DISPLAY "ADJUSTMENT DENIALS ON ADJUSTMENT RECORDS NOT VALID"
           GO TO 2000TI.
      *     IF IN-FIELD-2 = "14" DISPLAY "INS. REDUCTION" BELL.
           MOVE IN-FIELD-2 TO PD-DENIAL GO TO 4900DEE.
       4900DEE.
           IF FLAG-TABLE(INDX) NOT = 1
           MOVE 1 TO FLAG-TABLE(INDX)
           ADD 1 TO NUMBER-OF-FIELDS
           MOVE INDX TO DISPLAY-FIELD(NUMBER-OF-FIELDS).
       4905DEE.
           IF ACTION = "C" GO TO 1400-CHANGE-PROCESS.
       4910DEE.
           EXIT.
       5000-WRITE-PAYFILE.
           IF (PD-DENIAL = "14" OR "07" OR "08" OR "15" OR "DI") 
           AND ((PD-PAYCODE > "006" AND < "020") 
           AND (PD-PAYCODE NOT = "018"))
           DISPLAY "ADJUSTMENT DENIALS ON ADJUSTMENT RECORDS = INVALID"
           GO TO 1400-CHANGE-PROCESS.
           IF (PD-AMOUNT < 0)
           AND 
           ((PD-PAYCODE = "010" OR "015" OR "019")
           OR (PD-DENIAL = "08" OR "15"))
           COMPUTE PD-AMOUNT = -1 * PD-AMOUNT.
           IF (PD-AMOUNT > 0)
           AND  NOT ((PD-PAYCODE = "010" OR "015" OR "019")
           OR (PD-DENIAL = "08" OR "15"))
           COMPUTE PD-AMOUNT = -1 * PD-AMOUNT.

           MOVE PAYFILE01 TO PAYBACK01
           REWRITE PAYFILE01 INVALID KEY DISPLAY "NO UPDATE."
           DISPLAY "THIS SHOULD NOT HAPPEN! CONTACT THE DATA CENTER."
           DISPLAY "THIS PROGRAM IS TERMINATED!"
           GO TO 9100-CLOSE-MASTER-FILE.
           UNLOCK PAYFILE RECORD
           DISPLAY "UPDATE MADE"  GO TO 1000-ACTION.

       CUR-1.
           MOVE G-GARNO TO CC-KEY8 MOVE "000" TO CC-KEY3.
           START CHARCUR KEY > CHARCUR-KEY INVALID GO TO CUR-END.
       CUR-2. READ CHARCUR NEXT AT END GO TO CUR-END.
           IF CC-KEY8 NOT = G-GARNO GO TO CUR-END.
           IF CC-CLAIM NOT = PD-CLAIM GO TO CUR-2.
            IF (INS-ASSIGN = "A")
            AND (CC-ASSIGN = "A")
            AND NOT (INS-KEY > "009" AND < "023")
            AND (PD-PAYCODE NOT = G-PRINS) 
            AND (PD-PAYCODE NOT = G-SEINS) 
            AND (PD-PAYCODE NOT = G-TRINS)
            AND (PD-PAYCODE NOT = CC-PAYCODE)
            DISPLAY "PAYCODE DOES NOT MATCH INSURANCE COVERAGE"
            DISPLAY "USE THIS CODE ? Y"
            ACCEPT ANS
            IF ANS NOT = "Y" GO TO CUR-END
            END-IF
           END-IF
           MOVE 1 TO FLAG.
       CUR-END. EXIT.
       DAL-1.
           MOVE G-GARNO TO CD-KEY8 MOVE "000" TO CD-KEY3.
           START CHARFILE KEY > CHARFILE-KEY INVALID GO TO DAL-END.
       DAL-2. READ CHARFILE NEXT AT END GO TO DAL-END.
           IF CD-KEY8 NOT = G-GARNO GO TO DAL-END.
           IF CD-CLAIM NOT = PD-CLAIM GO TO DAL-2.
            IF (INS-ASSIGN = "A")
            AND (CD-ASSIGN = "A")
            AND NOT (INS-KEY > "009" AND < "023")
            AND (PD-PAYCODE NOT = G-PRINS) 
            AND (PD-PAYCODE NOT = G-SEINS) 
            AND (PD-PAYCODE NOT = G-TRINS)
            AND (PD-PAYCODE NOT = CD-PAYCODE)
            DISPLAY "PAYCODE DOES NOT MATCH INSURANCE COVERAGE"
            DISPLAY "USE THIS CODE ? Y"
            ACCEPT ANS
            IF ANS NOT = "Y" GO TO DAL-END
            END-IF
           END-IF
           MOVE 1 TO FLAG.
       DAL-END. EXIT.
       DC1. MOVE "000" TO CC-KEY3 MOVE G-GARNO TO CC-KEY8.
           IF ( PD-PAYCODE = "010" OR "015" OR "019" OR "017" )
           OR (PD-DENIAL = "08" OR "15")
           OR ( PD-AMOUNT NOT < 0 )
           DISPLAY "CAN""T DISTRIBUTE DEBIT TYPE PAYMENTS"
           SET ADD-KEY DOWN BY 1 GO TO DC1-EXIT.
           SET P-IND TO 1.
           SET PAY-IND TO 1
           PERFORM DP1 THRU DP1-EXIT
           SET PAY-IND DOWN BY 1
           IF PAY-IND = 990 DISPLAY "ACCT TOO LARGE TO USE D ROUTINE"
           GO TO DC1-EXIT.
           START CHARCUR KEY > CHARCUR-KEY INVALID GO TO DC5.
       DC4.   READ CHARCUR NEXT AT END GO TO DC5.
           IF CC-KEY8 NOT = G-GARNO GO TO DC5.
           IF CC-ASSIGN = "A" GO TO DC4.
           IF P-IND = 990 DISPLAY "THIS ACCOUNT HAS MORE THAN "
           DISPLAY "900 CHARGES AND CAN""T BE DISTRIBUTED"
           SET ADD-KEY DOWN BY 1
           GO TO DC1-EXIT.
           MOVE CC-PAYCODE TO PC-TAB(P-IND)
           MOVE CC-CLAIM TO C-TAB(P-IND)
           MOVE CC-AMOUNT TO A-TAB(P-IND)
           MOVE CC-DATE-T TO D-TAB(P-IND).
           PERFORM DP2 VARYING XX FROM 1 BY 1 UNTIL XX > PAY-IND
           IF A-TAB(P-IND) > 0 SET P-IND UP BY 1.
           GO TO DC4.
       DC5.   MOVE "000" TO CD-KEY3 MOVE G-GARNO TO CD-KEY8.
           START CHARFILE KEY > CHARFILE-KEY INVALID GO TO DC-SORT.
       DC6.   READ CHARFILE NEXT AT END GO TO DC-SORT.
           IF CD-KEY8 NOT = G-GARNO GO TO DC-SORT.
           IF CD-ASSIGN = "A" GO TO DC6.
           IF P-IND > 990 DISPLAY "THIS ACCOUNT HAS MORE THAN "
           DISPLAY "990 CHARGES AND CAN""T BE DISTRIBUTED"
           SET ADD-KEY DOWN BY 1
           GO TO DC1-EXIT.
           MOVE CD-PAYCODE TO PC-TAB(P-IND)
           MOVE CD-CLAIM TO C-TAB(P-IND)
           MOVE CD-AMOUNT TO A-TAB(P-IND)
           MOVE CD-DATE-T TO D-TAB(P-IND).
           PERFORM DP2 VARYING XX FROM 1 BY 1 UNTIL XX > PAY-IND
           IF A-TAB(P-IND) > 0 SET P-IND UP BY 1.
           GO TO DC6.
       DC-SORT.
           SET P-IND DOWN BY 1
           IF P-IND = 0 DISPLAY "NO PERSONAL BALANCE DUE"
           SET ADD-KEY DOWN BY 1
           GO TO DC1-EXIT.
           MOVE 0 TO TOT-BAL.
           PERFORM SD5 VARYING XX FROM 1 BY 1 UNTIL XX > P-IND.
           MOVE PAYBACK01 TO PAYFILE01.
      *    IF PD-PAYCODE NOT = "01" PERFORM SD4 VARYING XX FROM 1
      *    BY 1 UNTIL XX > IND.
           SET XX TO P-IND
           SET XX DOWN BY 1
           PERFORM SD1 VARYING YY FROM 1 BY 1 UNTIL YY > XX.
      *    DISPLAY TOT-BAL " " TOT-AMOUNT.
           MOVE TOT-BAL TO NEF-8
           DISPLAY P-IND " CLAIM(S) TO BE PAID TOTALING " NEF-8.
           ADD TOT-BAL TOT-AMOUNT GIVING TOT-BAL.
           IF TOT-BAL < 0 DISPLAY "CANT""T OVERPAY BY DISTRIBUTION"
           SET ADD-KEY DOWN BY 1 GO TO DC1-EXIT.
           PERFORM SD3 THRU SD3-EXIT VARYING XX FROM 1 BY 1 UNTIL XX >
           P-IND.
           MOVE "!)(!" TO IN-FIELD.
       DC1-EXIT. EXIT.
       SD1. SET ZZ TO YY
           SET ZZ UP BY 1
           PERFORM SD2 VARYING AA FROM ZZ BY 1 UNTIL AA > P-IND.
       SD2. IF D-TAB(AA) < D-TAB(YY)
           MOVE D-TAB(AA) TO ALF-8
           MOVE D-TAB(YY) TO D-TAB(AA)
           MOVE ALF-8 TO D-TAB(YY)

           MOVE DE-TAB(AA) TO ALF-8
           MOVE DE-TAB(YY) TO DE-TAB(AA)
           MOVE ALF-8 TO DE-TAB(YY)

           MOVE A-TAB(AA) TO SNUM-6
           MOVE A-TAB(YY) TO A-TAB(AA)
           MOVE SNUM-6 TO A-TAB(YY)
           MOVE C-TAB(AA) TO ALF-6
           MOVE C-TAB(YY) TO C-TAB(AA)
           MOVE ALF-6 TO C-TAB(YY)
           MOVE PC-TAB(AA) TO ALF-3
           MOVE PC-TAB(YY) TO PC-TAB(AA)
           MOVE ALF-3 TO PC-TAB(YY)
           MOVE DN-TAB(AA) TO ALF-2
           MOVE DN-TAB(YY) TO DN-TAB(AA)
           MOVE ALF-2 TO DN-TAB(YY).
        SD3. ADD 1 TO XYZ.
           MOVE XYZ TO PD-KEY3.
           READ PAYFILE INVALID GO TO SD3-1.
           GO TO SD3.
        SD3-1.
           IF TOT-AMOUNT NOT < 0 GO TO SD3-EXIT.
           MOVE PAYBACK01 TO PAYFILE01
           MOVE XYZ TO PD-KEY3
           MULTIPLY A-TAB(XX) BY -1 GIVING PD-AMOUNT.
           MOVE C-TAB(XX) TO PD-CLAIM.
           IF TOT-AMOUNT > PD-AMOUNT MOVE TOT-AMOUNT TO PD-AMOUNT
           PERFORM DR1 THRU DR1-EXIT
           SET XX TO P-IND
           ELSE PERFORM DR1 THRU DR1-EXIT
           COMPUTE TOT-AMOUNT = TOT-AMOUNT - PD-AMOUNT.
       SD3-EXIT. EXIT.
       SD4. IF PC-TAB(XX) = PD-PAYCODE MOVE ZERO TO D-TAB(XX).
       SD5. ADD A-TAB(XX) TO TOT-BAL.
       DP1. MOVE G-GARNO TO PC-KEY8 MOVE "000" TO PC-KEY3.
           START PAYCUR KEY > PAYCUR-KEY INVALID GO TO DP5.
       DP4. READ PAYCUR NEXT AT END GO TO DP5.
           IF PC-KEY8 NOT = G-GARNO GO TO DP5.
           MOVE PC-CLAIM TO PAYC-TAB(PAY-IND)
           MOVE PC-AMOUNT TO PAYA-TAB(PAY-IND)
           MOVE PC-DATE-T TO PAYD-TAB(PAY-IND)
           SET PAY-IND UP BY 1
           IF PAY-IND > 990 DISPLAY "TOO MANY PAYMENTS"
           GO TO DP1-EXIT.
           GO TO DP4.
       DP5. MOVE G-GARNO TO PD-KEY8 MOVE "000" TO PD-KEY3.
           START PAYFILE KEY > PAYFILE-KEY INVALID GO TO DP1-EXIT.
       DP6. READ PAYFILE NEXT AT END GO TO DP1-EXIT.
           IF PD-KEY8 NOT = G-GARNO GO TO DP1-EXIT.
           MOVE PD-CLAIM TO PAYC-TAB(PAY-IND)
           MOVE PD-AMOUNT TO PAYA-TAB(PAY-IND)
           MOVE PD-DATE-T TO PAYD-TAB(PAY-IND)
           SET PAY-IND UP BY 1
           IF PAY-IND > 990 DISPLAY "TOO MANY PAYMENTS"
           GO TO DP1-EXIT.
           GO TO DP6.
       DP1-EXIT. EXIT.
       DP2.
           IF PAYC-TAB(XX) = C-TAB(P-IND)
      *    DISPLAY PAYA-TAB(XX) " " A-TAB(P-IND)
           ADD PAYA-TAB(XX) TO A-TAB(P-IND)
           IF PAYPC-TAB(XX) = PC-TAB(P-IND) MOVE "XXX" TO
           PC-TAB(P-IND).
       DPX1. MULTIPLY DP BY -1 GIVING DP.
           IF DP = 1
           DISPLAY "DISPLAY PATIENT NAME ON CHARGE" ELSE
           DISPLAY "NO DISPLAY OF PATIENT NAME".
       DR2. MULTIPLY DR BY -1 GIVING DR.
           IF DR = 1
           DISPLAY "PROMPT FOR REDUCTION PAYMENT" ELSE
           DISPLAY "NO REDUCTION PROMPT".
       DI1.
           MULTIPLY DI BY -1 GIVING DI.
           IF DI = 1
           DISPLAY "DEFFERED INCOME PROMPT" ELSE
           DISPLAY "NO DEFFERED INCOME PROMPT".
       
       FZ1.
           MULTIPLY FZ BY -1 GIVING FZ.
           IF FZ = -1
           DISPLAY "INCLUDE ZERO BALANCED CLAIMS" ELSE
           DISPLAY "NO LISTING FOR 0 BAL. CLAIMS".
       CD1.
           IF CD = 1
           MOVE 0 TO CD
           DISPLAY "NO PROMPT FOR CHARGE DATE" 
           ELSE MOVE 1 TO CD
           DISPLAY "PROMPT FOR CHARGE ON FC AND FCC COMMAND.".
       WO6. MOVE G-GARNO TO CC-KEY8 MOVE "000" TO CC-KEY3.
           MOVE 1 TO FLAG.
           MOVE 0 TO TOT-CLAIM.
           START CHARCUR KEY > CHARCUR-KEY INVALID GO TO WO81.
       WO7. READ CHARCUR NEXT AT END GO TO WO81.
           IF G-GARNO NOT = CC-KEY8 GO TO WO81.
           IF CC-CLAIM NOT = PD-CLAIM GO TO WO7.
           MOVE CC-AMOUNT TO TOT-CLAIM. MOVE 0 TO FLAG. GO TO WO8.
       WO81. MOVE G-GARNO TO CD-KEY8 MOVE "000" TO CD-KEY3.
           START CHARFILE KEY > CHARFILE-KEY INVALID GO TO WO12.
       WO82. READ CHARFILE NEXT AT END GO TO WO12.
           IF G-GARNO NOT = CD-KEY8 GO TO WO12.
           IF CD-CLAIM NOT = PD-CLAIM GO TO WO82.
           MOVE CD-AMOUNT TO TOT-CLAIM. MOVE 0 TO FLAG. GO TO WO8.
       WO8. MOVE G-GARNO TO PC-KEY8 MOVE "000" TO PC-KEY3.
           START PAYCUR KEY > PAYCUR-KEY INVALID GO TO WO10.
       WO9. READ PAYCUR NEXT AT END GO TO WO10.
           IF G-GARNO NOT = PC-KEY8 GO TO WO10.
           IF PC-CLAIM = PD-CLAIM ADD PC-AMOUNT TO TOT-CLAIM.
           GO TO WO9.
       WO10. MOVE PD-CLAIM TO IN-FIELD-6.
           MOVE G-GARNO TO PD-KEY8 MOVE "000" TO PD-KEY3.
           START PAYFILE KEY > PAYFILE-KEY INVALID GO TO WO12.
       WO11. READ PAYFILE NEXT AT END GO TO WO12.
           IF G-GARNO NOT = PD-KEY8 GO TO WO12.
           IF PD-CLAIM = IN-FIELD-6 ADD PD-AMOUNT TO TOT-CLAIM.
           GO TO WO11.
       WO12. EXIT.
       FF-NAME. IF FFTAB10(XIND) NOT = SPACE SET FF TO XIND
           SET XIND TO 1.
       LL-NAME. IF LLTAB24(XIND) NOT = SPACE SET LL TO XIND
           SET XIND TO 1.
       AQ1. IF LLTAB24(YIND) NOT = XLLTAB24(YIND) MOVE 1 TO FLAGX.
       AQ2. IF XFFTAB10(YIND) NOT = FFTAB10(YIND) MOVE 1 TO FLAGX.
       IP-1. IF IP = 0 MOVE 1 TO IP DISPLAY "LIST INSURANCE"
             ELSE MOVE 0 TO IP DISPLAY "NO INSURANCE DISPLAY".
       CM-1. IF CM = 0 MOVE 1 TO CM DISPLAY "LIST COMMENTS"
           ELSE MOVE 0 TO CM DISPLAY "COMMENTS NOT READ".
       CM-2. MOVE G-GARNO TO CM-KEY8 MOVE "000" TO CM-KEY3.
           START CMNTFILE KEY NOT < CMNT-KEY INVALID GO TO CM-4.
       CM-3. READ CMNTFILE NEXT AT END GO TO CM-4.
           IF CM-KEY8 NOT = G-GARNO GO TO CM-4.
           ADD 1 TO Y
           IF Y > 13 ACCEPT ANS
           MOVE 0 TO Y.
           DISPLAY CMNT GO TO CM-3.
       CM-4. EXIT.
       FA-1. MOVE PD-KEY8 TO G-GARNO.
           READ GARFILE INVALID DISPLAY "INVALID" GO TO FA-1-EXIT.
           MOVE G-DOB TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           INSPECT T-MM OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           INSPECT T-DD OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           DISPLAY G-GARNO " " G-PRINS "/" G-SEINS "/" G-TRINS
           " " G-GARNAME
           DISPLAY DISPLAY-DATE " " G-PRIPOL " " G-SECPOL
           MOVE G-GARNO TO P-GARNO
           START PATFILE KEY NOT < P-GARNO INVALID GO TO FA-1-EXIT.
       FA-2. READ PATFILE NEXT AT END GO TO FA-1-EXIT.
           IF P-GARNO NOT = G-GARNO GO TO FA-1-EXIT.
           MOVE P-DOB TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           INSPECT T-MM OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           INSPECT T-DD OF DISPLAY-DATE REPLACING LEADING "0" BY " "
           DISPLAY P-PATNO " " DISPLAY-DATE  " " P-SEX " " P-PATNAME 
           GO TO FA-2.
       FA-1-EXIT. EXIT.
       LPAY-1. DISPLAY PAYFILE-KEY " " PD-NAME " "
           MOVE PD-AMOUNT TO NEF-8
           DISPLAY "3 = " NEF-8 " 4 = " PD-PAYCODE
           " 5 = " PD-DENIAL " 6 = " PD-CLAIM " 7 = " PD-DATE-T
           "  BAT =  " PD-DATE-E.
       1399CPC. READ CHARCUR WITH LOCK INVALID DISPLAY "INVALID"
           GO TO 1000-ACTION.
           IF CHARCUR-STAT NOT = "00" 
           DISPLAY "RECORD IS BEING USED"
           DISPLAY "TRY AGAIN LATER" 
           DISPLAY "STATUS = " CHARCUR-STAT
           GO TO 1000-ACTION.
           MOVE CC-ASSIGN TO HOLDASSIGN
           MOVE CC-PATID TO EIGHTPARTID
           IF EIGHT-1 = "P" MOVE CC-PATID TO P-PATNO
           READ PATFILE INVALID DISPLAY "NO PATIENT FOR THIS RECORD"
           GO TO 1000-ACTION.
           IF EIGHT-1 = "P" DISPLAY P-PATNAME GO TO 1400CPC.
           MOVE CC-PATID TO G-GARNO
           READ GARFILE INVALID DISPLAY "INVALID ACCT FOR THIS RECORD"
           GO TO 1000-ACTION.
           DISPLAY G-GARNAME.
       1400CPC.
           DISPLAY "FIELD CODE,DATA?  POSTED CHRG"
           ACCEPT DATAIN.
           IF DATAIN = "X"
           UNLOCK CHARCUR RECORD
               DISPLAY "NO CHANGE"  GO TO 1000-ACTION.

           IF DATAIN = "UP"

              IF ((CC-DATE-T > "20150930") AND
                  (CC-DIAG(6:2) = "??"
                  OR CC-DX2(6:2) = "??"
                  OR CC-DX3(6:2) = "??"
                  OR CC-DX4(6:2) = "??"))
               OR ((CC-DATE-T < "20151001")
               AND NOT
                     ((CC-DIAG(6:2) = "??" OR "00")
                 AND (CC-DX2(6:2) = "??" OR "00")
                 AND (CC-DX3(6:2) = "??" OR "00")
                 AND (CC-DX4(6:2) = "??" OR "00")))
               DISPLAY "DATE CONFLICT WITH DIAGNOSES"
               DISPLAY "CHANGE THE DATE TO MATCH DIAGNOSES"
               DISPLAY "OR CHANGE ALL THE DIAGNOSES"
               GO TO 1400CPC
              END-IF
           GO TO 5000-WRITE-CHARCUR
           END-IF

           IF DATAIN = "L" PERFORM LC-1 GO TO 1400CPC.
           IF DATAIN = "?"
           DISPLAY "ENTER THE FIELD # AND DATA FOR THAT FIELD"
           DISPLAY "TO BE CHANGED. VALID CODES ARE:"
           DISPLAY "  2=PATID   4=TYPE SERV   5=DIAG     6=PROC" 
           DISPLAY "  7=AMOUNT  8=REF PHYS    9=PRVDR   10=INS"
           DISPLAY "12=UNITS  13=ACC DATE   14=PAPER   15=PLACE"
           DISPLAY "17=EPSDT  18=CHRGDATE   19=RESULT  20=ACTION"
           DISPLAY "21=MOD2   22=REC-STAT   23=SORCREF 24=DX2" 
           DISPLAY "25=DX3    26=CLM-DATE   27=COLLT   28=ACCTYPE" 
           DISPLAY "29=ADMTDT 30=MOD3       31=AUTH/NDC 32=ASSIGN" 
           DISPLAY "33=NEICASGM  34=DX4"
      *      35=DX5 36=DX6 "
           DISPLAY "L TO LIST THE RECORD"
           DISPLAY "UP = UPDATE CHANGES"
           DISPLAY "X = NO UPDATE"
           GO TO 1400CPC.
           MOVE SPACES TO RIGHT-2 IN-FIELD.
           UNSTRING DATAIN DELIMITED BY "," INTO RIGHT-2 IN-FIELD.
           INSPECT RIGHT-2 REPLACING LEADING SPACE BY "0".
           IF RIGHT-2 NOT NUMERIC
               DISPLAY "FIELD-CODE MUST BE NUMERIC"
               GO TO 1400CPC.
           IF RIGHT-2 = "00" OR "01" OR "03" OR "16" 
           OR RIGHT-2 > "34" DISPLAY "INVALID FIELD-CODE"
               GO TO 1400CPC.
           MOVE RIGHT-2 TO NUM-2
           SET CCINDX TO NUM-2
           GO TO CC-2060-GO-TO.
       CC-2050.
           DISPLAY CCDES-KEY(CCINDX) "?".
       CC-2051.
           ACCEPT IN-FIELD.
           IF IN-FIELD = "L" PERFORM LC-1 GO TO CC-2050.
       CC-2060-GO-TO.
           MOVE 0 TO FLAG.
           IF CCINDX = 6 GO TO CC-2061.
           MOVE CCLEN-TAB(CCINDX) TO Q ADD 1 TO Q.
           IF IN-FIELD-TAB(Q) NOT = " " MOVE "1" TO FLAG
           DISPLAY "DATA TOO LONG, MUST NOT BE GREATER "
             "THAN " CCLEN-TAB(CCINDX) ".".
           IF FLAG = 1 AND ACTION = "CPC" GO TO 1400CPC.
           IF FLAG = 1 AND ACTION = "APC" GO TO CC-2050.
       CC-2061.
           GO TO 2100-CHAR-KEY
           CC-2-PATID CC-2100-CLAIM CC-2130-SERVICE CC-2150-DIAG
           CC-2140-PROC CC-2200-AMOUNT CC-2160-DOCR CC-2165-DOCP
           CC-2170-PAYCODE 4900CPC CC-2750-WORK CC-2190-DAT1 CC-2-PAPER
           CC-2180-PLACE 4900CPC CC-2EPSDT CC-4000-DATE-T CC-2195-RESULT
           CC-2196-ACTION CC-2-MOD2 CC-2-REC-STAT CC-2-SORCREF
           CC-2-DX2 CC-2-DX3 CC-2-DATE-A CC-2-COLLT
           CC-2-ACC-TYPE CC-2-ADMIT CC-2-MOD3 CC-2-AUTH CC-2-ASSIGN
           CC-2-NEIC-ASSIGN CC-2-DX4
      *     CC-2-DX5 CC-2-DX6
           DEPENDING ON CCINDX.
       CC-2000TI.
           IF ACTION = "CPC" GO TO 1400CPC.
           GO TO 1000-ACTION.
       2100-CHAR-KEY.
           IF IN-FIELD = "?"
           DISPLAY "ENTER  A GUARANTOR ACCT #"
           GO TO CC-2000TI.
           MOVE IN-FIELD-8 TO G-GARNO.
           READ GARFILE INVALID KEY DISPLAY " NOT ON FILE"
           GO TO CC-2000TI.
           DISPLAY G-GARNAME.
       2120-CHAR-KEY.
           MOVE 0 TO X.
       2130-CHARCUR-KEY.
           ADD 1 TO X.
           MOVE SPACE TO ALF-11.
           MOVE X TO ABC.
           STRING IN-FIELD-8 ABC DELIMITED BY "@" INTO ALF-11.
           MOVE ALF-11 TO CHARCUR-KEY.
           READ CHARCUR INVALID KEY GO TO 4900CPC.
           ADD 1 TO X.
           IF X = 999 DISPLAY "THIS ACCOUNT HAS 999 TRANSACTIONS"
           DISPLAY "IN THE FILE ALREADY. NO MORE ARE ALLOWED."
           GO TO CC-2000TI
           GO TO 2130-CHARCUR-KEY.
       CC-2-PATID.
           MOVE IN-FIELD-8 TO P-PATNO EIGHTPARTID.
           IF EIGHT-1 = "G" GO TO CC-3-PATID.
           READ PATFILE INVALID DISPLAY "INVALID"
           GO TO 1400CPC.
           MOVE CHARCUR-KEY TO PART11.
           IF PART8 NOT = P-GARNO
           DISPLAY P-PATNAME  " IS NOT A DEPENDENT FOR THIS GARNO"
           GO TO 1400CPC.
           MOVE P-PATNO TO CC-PATID
           DISPLAY  P-PATNAME
           GO TO 4900CPC.
       CC-3-PATID. MOVE CHARCUR-KEY TO PART11.
           IF IN-FIELD-8 = PART8 MOVE PART8 TO CC-PATID
           DISPLAY "GUARANTOR IS PATIENT" GO TO 4900CPC.
           DISPLAY "INVALID" GO TO 1400CPC.
       CC-2100-CLAIM.
            DISPLAY "CLAIM NUMBERS CANT""T BE CHANGED"
           GO TO 1400CPC.
       CC-2130-SERVICE.
           IF IN-FIELD = "?"
           DISPLAY "ENTER THE TYPE OF SERVICE. CODES ARE"
           DISPLAY "1=SURG. 2=ASST. SURG. 3=ANESTH. 4=LAB"
           DISPLAY "5=X-RAY 6=MEDICAL 7=IMMUNIZ."
               GO TO CC-2000TI.
           IF IN-FIELD-1 > "0" AND < "8"
           MOVE IN-FIELD-1 TO CC-SERVICE GO TO 4900CPC
           ELSE MOVE "?" TO IN-FIELD GO TO CC-2130-SERVICE.
       CC-2150-DIAG.
           IF IN-FIELD = "?"
               DISPLAY "ENTER A DIAGNOSIS CODE."
           DISPLAY " OR TYPE F TO SEARCH FOR A DIAGNOSIS CODE"
           DISPLAY "BY NUMBER OR DESCRIPTION"
           DISPLAY "OR A <CR> IF NO DIAGNOSIS IS NECESSARY."
           DISPLAY " TYPE F TO FIND DIAGNOSIS CODES"
           DISPLAY " OR TYPE M TO MAP ICD9 TO ICD10 CODES"
               GO TO CC-2000TI.
           IF IN-FIELD = SPACES MOVE ZEROES TO CC-DIAG
           GO TO 4900CPC.
           MOVE 0 TO RETURN-FLAG
           PERFORM  CC10 THRU CC10-EXIT
           IF RETURN-FLAG = 1 GO TO CC-2000TI.
           MOVE IN-FIELD-7 TO CC-DIAG.
           GO TO 4900CPC.
       CC-2-DX2.
           IF IN-FIELD = "?"
               DISPLAY "ENTER A SECOND DX CODE."
           DISPLAY " OR TYPE F TO SEARCH FOR A DIAGNOSIS CODE"
           DISPLAY "BY NUMBER OR DESCRIPTION"
           DISPLAY "OR A <CR> IF NO DIAGNOSIS IS NECESSARY."
           DISPLAY " TYPE F TO FIND DIAGNOSIS CODES"
           DISPLAY " OR TYPE M TO MAP ICD9 TO ICD10 CODES"
               GO TO CC-2000TI.
           IF IN-FIELD = "0000000" OR SPACES MOVE ZEROES TO CC-DX2
           GO TO 4900CPC.
           MOVE 0 TO RETURN-FLAG
           PERFORM  CC10 THRU CC10-EXIT
           IF RETURN-FLAG = 1 GO TO CC-2000TI.
           MOVE IN-FIELD-7 TO CC-DX2.
           GO TO 4900CPC.
       CC-2-DX3.
           IF IN-FIELD = "?"
               DISPLAY "ENTER A THIRD DX  CODE."
           DISPLAY " OR TYPE F TO SEARCH FOR A DIAGNOSIS CODE"
           DISPLAY "BY NUMBER OR DESCRIPTION"
           DISPLAY "OR A <CR> IF NO DIAGNOSIS IS NECESSARY."
           DISPLAY " TYPE F TO FIND DIAGNOSIS CODES"
           DISPLAY " OR TYPE M TO MAP ICD9 TO ICD10 CODES"
               GO TO CC-2000TI.
           IF IN-FIELD = "0000000" OR SPACES MOVE ZEROES TO CC-DX3
           GO TO 4900CPC.
           MOVE 0 TO RETURN-FLAG
           PERFORM  CC10 THRU CC10-EXIT
           IF RETURN-FLAG = 1 GO TO CC-2000TI.
           MOVE IN-FIELD-7 TO CC-DX3.
           GO TO 4900CPC.
       CC-2-DX4.
           IF IN-FIELD = "?"
               DISPLAY "ENTER A FOURTH DX  CODE."
           DISPLAY " OR TYPE F TO SEARCH FOR A DIAGNOSIS CODE"
           DISPLAY "BY NUMBER OR DESCRIPTION"
           DISPLAY "OR A <CR> IF NO DIAGNOSIS IS NECESSARY."
           DISPLAY " TYPE F TO FIND DIAGNOSIS CODES"
           DISPLAY " OR TYPE M TO MAP ICD9 TO ICD10 CODES"
               GO TO CC-2000TI.
           IF IN-FIELD = "0000000" OR SPACES MOVE ZEROES TO CC-DX4
           GO TO 4900CPC.
           MOVE 0 TO RETURN-FLAG
           PERFORM  CC10 THRU CC10-EXIT
           IF RETURN-FLAG = 1 GO TO CC-2000TI.
           MOVE IN-FIELD-7 TO CC-DX4.
           GO TO 4900CPC.
       CC-2-DX5.
           IF IN-FIELD = "?"
               DISPLAY "ENTER A FIFTH DX  CODE."
           DISPLAY " OR TYPE F TO SEARCH FOR A DIAGNOSIS CODE"
           DISPLAY "BY NUMBER OR DESCRIPTION"
           DISPLAY "OR A <CR> IF NO DIAGNOSIS IS NECESSARY."
           DISPLAY " TYPE F TO FIND DIAGNOSIS CODES"
           DISPLAY " OR TYPE M TO MAP ICD9 TO ICD10 CODES"
               GO TO CC-2000TI.
           IF IN-FIELD = "0000000" OR SPACES MOVE ZEROES TO CC-DX5
           GO TO 4900CPC.
           MOVE 0 TO RETURN-FLAG
           PERFORM  CC10 THRU CC10-EXIT
           IF RETURN-FLAG = 1 GO TO CC-2000TI.
           MOVE IN-FIELD-7 TO CC-DX5.
           GO TO 4900CPC.
       CC-2-DX6.
           IF IN-FIELD = "?"
               DISPLAY "ENTER A SIXTH DX  CODE."
           DISPLAY " OR TYPE F TO SEARCH FOR A DIAGNOSIS CODE"
           DISPLAY "BY NUMBER OR DESCRIPTION"
           DISPLAY "OR A <CR> IF NO DIAGNOSIS IS NECESSARY."
           DISPLAY " TYPE F TO FIND DIAGNOSIS CODES"
           DISPLAY " OR TYPE M TO MAP ICD9 TO ICD10 CODES"
               GO TO CC-2000TI.
           IF IN-FIELD = "0000000" OR SPACES MOVE ZEROES TO CC-DX6
           GO TO 4900CPC.
           MOVE 0 TO RETURN-FLAG
           PERFORM  CC10 THRU CC10-EXIT
           IF RETURN-FLAG = 1 GO TO CC-2000TI.
           MOVE IN-FIELD-7 TO CC-DX6.
           GO TO 4900CPC.
       CC-2140-PROC.
           IF IN-FIELD = "?"
               DISPLAY "ENTER A PROCEDURE CODE."
           DISPLAY "TYPE F TO SEARCH PROCEDURE FILE."
           GO TO CC-2000TI.
           IF IN-FIELD = "F" GO TO CC-2150-PROC.
           MOVE IN-FIELD-7 TO PROC-KEY.
           READ PROCFILE INVALID KEY
           DISPLAY "PROCEDURE CODE NOT IN FILE"
           GO TO CC-2000TI.
           MOVE PROC-KEY TO CC-PROC.
           GO TO 4900CPC.
       CC-2150-PROC. DISPLAY "SEARCH KEY".
           ACCEPT PROC-KEY.
           IF PROC-KEY = "?"
           DISPLAY "TYPE 1-5 DIGITS FROM WHICH POINT TO START SEARCH."
           DISPLAY "THE FILE IS IN INCREASING PROC. # ORDER"
           DISPLAY "TYPE X TO BACK TO PROMPT FOR PROCEDURE."
           GO TO CC-2150-PROC.
           IF CC-PROC = "X" GO TO CC-2000TI.
           START PROCFILE KEY NOT < PROC-KEY INVALID
           DISPLAY "END OF FILE" GO TO CC-2000TI.
           MOVE 0 TO X.
       CC-2160-PROC. READ PROCFILE NEXT AT END DISPLAY
           "END OF FILE" GO TO CC-2000TI.
           IF X > 5 DISPLAY "?"
           ACCEPT ANS
           MOVE 0 TO X
           MOVE SPACE TO RIGHT-8
           IF ANS NOT = SPACE GO TO CC-2000TI.
           MOVE PROC-AMOUNT TO NEF-8.
           IF CARE-AMOUNT NOT = PROC-AMOUNT
           MOVE CARE-AMOUNT TO NEF-5  MOVE NEF-5 TO RIGHT-8.
           DISPLAY PROC-KEY " " PROC-OLD " " NEF-8 " " RIGHT-8 " "
           PROC-TYPE " " PROC-TITLE
           ADD 1 TO X. GO TO CC-2160-PROC.
       CC-2200-AMOUNT.
           IF IN-FIELD = "?"
           DISPLAY "ENTER THE CC-AMOUNT OF CHARGE"
           DISPLAY "EXAMPLE: $1,234,56 = 1234.56"
           DISPLAY "CENTS PORTION ASSUMED TO BE 00 IF NOT TYPED"
           DISPLAY "DECIMAL POINT IS OPTIONAL WHEN THIS IS THE CASE"
           GO TO CC-2000TI.
           MOVE SPACES TO SIGN-DOLLAR CENTS.
           IF ACTION = "A"
           MOVE ALF-7 TO IN-FIELD.
           UNSTRING IN-FIELD DELIMITED BY "." INTO SIGN-DOLLAR CENTS.
           IF CENTS = SPACES MOVE "00" TO CENTS.
           IF CENTS NOT NUMERIC DISPLAY "INVALID"
           GO TO CC-2000TI.
           MOVE SPACES TO RIGHT-4.
           UNSTRING SIGN-DOLLAR DELIMITED BY " " INTO RIGHT-4
           INSPECT RIGHT-4 REPLACING LEADING " " BY "0"
           IF RIGHT-4 NOT NUMERIC DISPLAY "NOT NUMERIC"
           GO TO CC-2000TI.
           STRING RIGHT-4 CENTS DELIMITED BY "Z" INTO ALF-6
           MOVE ALF-6 TO NUM-6
           DIVIDE NUM-6 BY 100 GIVING CC-AMOUNT
            GO TO 4900CPC.
       CC-2160-DOCR.
           IF IN-FIELD = "?"
           DISPLAY "ENTER THE REFERRING PHYS. CODE"
           DISPLAY "OR TYPE F TO SEARCH FOR CODE"
           DISPLAY "A <CR> IF NO REFERRING PHYSICIAN."
           GO TO CC-2000TI.
           IF IN-FIELD-3 = SPACES MOVE ZEROES TO CC-DOCR GO TO 4900CPC.
           IF IN-FIELD = "F" GO TO CC-1REF-SEARCH.
           MOVE IN-FIELD-3 TO REF-KEY.
           READ REFPHY INVALID DISPLAY "NOT ON FILE"
               GO TO CC-2000TI.
           DISPLAY REF-NAME.
           MOVE IN-FIELD-3 TO CC-DOCR  .
           GO TO 4900CPC.
       CC-1REF-SEARCH.
           DISPLAY "SEARCH KEY ?".
           ACCEPT REF-NAME.
           IF REF-NAME = "?"
           DISPLAY "TYPE PART OF LAST NAME"
           DISPLAY "OR A <CR> TO END THE SEARCH"
           GO TO CC-1REF-SEARCH.
           IF REF-NAME = SPACE GO TO CC-2000TI.
           START REFPHY KEY > REF-NAME INVALID
           DISPLAY "END OF FILE" GO TO CC-2000TI.
           MOVE 0 TO X.
       CC-3REF. READ REFPHY NEXT AT END
           DISPLAY "END OF FILE" GO TO CC-2000TI.
           ADD 1 TO X.
           IF X > 5
           ACCEPT ANS
           MOVE 0 TO X
           IF ANS NOT = SPACE GO TO CC-2000TI.
           DISPLAY REF-KEY " " REF-BSNUM " " REF-CRNUM " " REF-UPIN " "
           REF-CDNUM " " REF-NPI " " REF-NAME
           GO TO CC-3REF.
       CC-2165-DOCP.
           IF IN-FIELD = "?"
           DISPLAY "ENTER THE LOCAL DOCTOR CODE (2-DIGITS)"
           GO TO CC-2000TI.
           MOVE SPACES TO RIGHT-2.
           UNSTRING IN-FIELD-2 DELIMITED BY " " INTO RIGHT-2.
           INSPECT RIGHT-2 REPLACING LEADING " " BY "0".
           IF RIGHT-2 NOT NUMERIC DISPLAY "NOT NUMERIC"
           GO TO CC-2000TI.
           IF RIGHT-2 = "00" MOVE "01" TO RIGHT-2.
           IF RIGHT-2 < "01" OR > HIGH-DOC DISPLAY "INVALID"
           GO TO CC-2000TI.
           MOVE RIGHT-2 TO CC-DOCP
           GO TO 4900CPC.
       CC-2170-PAYCODE.
           IF IN-FIELD = "?"
           DISPLAY "ENTER A PAYORCODE CODE "
               GO TO CC-2000TI.
           MOVE SPACES TO RIGHT-3.
           UNSTRING IN-FIELD-3 DELIMITED BY " " INTO RIGHT-3
           INSPECT RIGHT-3 REPLACING ALL " " BY "0"
           MOVE RIGHT-3 TO INS-KEY
           READ INSFILE INVALID DISPLAY "NOT A DEFINED INSURANCE"
           GO TO CC-2000TI.
           DISPLAY "ACCT " INS-ASSIGN "  CLM " INS-NEIC-ASSIGN
           " CLAIM-TYPE  " INS-CLAIMTYPE  "    " INS-NAME
           DISPLAY "ASSIGNMENT ATTRIBUTES ARE NOW CHANGED AS ABOVE"
           MOVE INS-ASSIGN TO CC-ASSIGN
           MOVE INS-NEIC-ASSIGN TO CC-NEIC-ASSIGN
           MOVE INS-CLAIMTYPE TO CC-PAPER
           MOVE INS-KEY TO CC-PAYCODE GO TO 4900CPC.
       CC-2750-WORK.
           IF IN-FIELD = "?"
           DISPLAY "TYPE # OF INITS REP. BY CHARGE"
           DISPLAY "1-9 ARE VALID. <CR> = 1"
           GO TO CC-2000TI.
           MOVE SPACE TO RIGHT-2
           UNSTRING IN-FIELD-2 DELIMITED BY " " INTO RIGHT-2.
           INSPECT RIGHT-2 REPLACING LEADING " " BY "0".
           IF (RIGHT-2 NOT NUMERIC) OR (RIGHT-2 = "00")
           DISPLAY "INVALID" GO TO CC-2000TI.
           MOVE RIGHT-2 TO CC-WORK
           IF CC-WORK > "09" DISPLAY "CHECK THIS!".
           GO TO 4900CPC.
       CC-2-PAPER. IF IN-FIELD = "?"
           DISPLAY "P = CLAIM FORM SENT AUTOMATICALLY"
           DISPLAY "O = CLAIM FORM SENT TO YOUR OFFICE"
           DISPLAY "A = CLAIM FORM ALREADY PROCESSED"
           DISPLAY "<CR> IF NEITHER" GO TO CC-2000TI.
           IF IN-FIELD-1 = "P" DISPLAY "CLAIM FORM MAILED AUTOMATICALLY"
           MOVE "P" TO CC-PAPER GO TO 4900CPC.
           IF IN-FIELD-1 = "O" DISPLAY "PAPER CLAIM SENT TO YOUR OFFICE"
           MOVE "O" TO CC-PAPER GO TO 4900CPC.
           IF IN-FIELD-1 NOT = " " DISPLAY "INVALID" GO TO CC-2000TI.
           MOVE " " TO CC-PAPER GO TO 4900CPC.
       CC-2180-PLACE.
           IF IN-FIELD = "?"
           DISPLAY "ENTER CC-PLACE OF CC-SERVICE CODE."
           DISPLAY "1=OFFICE 2=PAT. HOME"
           DISPLAY "7=REHAB. 8=OTHER"
           DISPLAY "FF=SEARCH CC-PLACE OF CC-SERVICE CODES"
           DISPLAY "OR LETTER CODE FROM A TO " HIGH-PLACE
               GO TO CC-2000TI.
           IF IN-FIELD-1 = " " OR "0" OR "^"
              DISPLAY "INVALID" GO TO CC-2000TI.
           IF IN-FIELD = "1" OR "2" OR "7" OR "8"
           MOVE IN-FIELD-1 TO CC-PLACE GO TO 4900CPC.
           MOVE 1 TO FLAG
           PERFORM CC-4500-PL1 THRU CC-4500-PL1-EXIT VARYING X
           FROM 1 BY 1 UNTIL X > PLINDX
           IF IN-FIELD-2 = "FF" GO TO CC-2000TI.
           IF FLAG = 1 DISPLAY "INVALID" GO TO CC-2000TI.
           MOVE IN-FIELD-1 TO CC-PLACE GO TO 4900CPC.
       CC-4500-PL1.
           IF IN-FIELD-2 = "FF"
           MOVE 0 TO FLAG
           DISPLAY PL-TAB(X) " " PL-NUM(X) " "
           PL-NAME(X) GO TO CC-4500-PL1-EXIT.
           IF PL-TAB(X) = IN-FIELD-1 DISPLAY PL-NAME(X)
           MOVE 0 TO FLAG MOVE PLINDX TO X.
       CC-4500-PL1-EXIT. EXIT.
       CC-2190-DAT1.
           IF IN-FIELD = "?"
           DISPLAY "ENTER ACCIDENT/SYMPTOM DATE IN MMDDYY FORMAT"
           DISPLAY "<CR> IF NOT AN ACCIDENT OR SYMPTOM DATE NOT KNOWN"
               GO TO CC-2000TI.
           IF IN-FIELD = SPACE MOVE "00000000" TO CC-DAT1
           GO TO 4900CPC.
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
           IF IN-FIELD-8 NUMERIC NEXT SENTENCE ELSE MOVE "?"
           TO IN-FIELD GO TO CC-2190-DAT1.
           MOVE IN-FIELD-8 TO INPUT-DATE.
           IF T-MM OF INPUT-DATE < 01 OR > 12
               MOVE "?" TO IN-FIELD
           GO TO CC-2190-DAT1.
           IF (T-DD OF INPUT-DATE > DAYS-IN-MONTH(T-MM OF INPUT-DATE))
            OR (T-DD OF INPUT-DATE = 00)
               MOVE "?" TO IN-FIELD
           GO TO CC-2190-DAT1.
           MOVE CORR INPUT-DATE TO TEST-DATE
           IF T-DATE < TEST-DATE DISPLAY "FUTURE DATE NOT ALLOWED"
           MOVE "?" TO IN-FIELD GO TO CC-2190-DAT1.
           MOVE TEST-DATE TO CC-DAT1
           GO TO 4900CPC.

       CC-2EPSDT.
           IF IN-FIELD = "?"
           DISPLAY "ESPDT AND FAMILY PLANNING FOR VT. MEDICAID"
           DISPLAY "1=BOTH  2=NEITHER  3=EPSDT  4=FAM.PL."
           GO TO CC-2000TI.
           IF IN-FIELD = SPACE MOVE "2" TO IN-FIELD-1
           DISPLAY "NEITHER".
           IF IN-FIELD-1 = "1" OR "2" OR "3" OR "4"
           MOVE IN-FIELD-1 TO CC-EPSDT GO TO 4900CPC.
           DISPLAY "INVALID" GO TO CC-2000TI.
       CC-2195-RESULT.
           IF IN-FIELD = "?"
           DISPLAY "ENTER MEDICAID CC-RESULT CODE"
           DISPLAY "1=NORMAL 2=ABNORMAL 3=UP TO DATE 4=NOT DONE"
           DISPLAY "5=IMMUN. UP TO DATE 6=MORE IMMUN. REQUIRED"
           GO TO CC-2000TI.
           IF IN-FIELD-1 < "1" OR > "6" DISPLAY "INVALID"
           GO TO CC-2000TI ELSE MOVE IN-FIELD-1 TO CC-RESULT
           GO TO 4900CPC.
       CC-2196-ACTION.
           IF IN-FIELD = "?"
           DISPLAY "ENTER MEDICAID ACTION CODE"
           DISPLAY "1=APPT. MADE OFF SITE 2=CARE INSTIT."
           DISPLAY "3=NO APPT. MADE 4=A;READY UNDER CARE"
           DISPLAY "5=REFER FOR TREAT. 6=DIAG/TREAT NOT AVAIL."
           DISPLAY "7=PATIENT REFUSED TREATMEMT"
           GO TO CC-2000TI.
           IF IN-FIELD-1 < "1" OR > "7" DISPLAY "INVALID"
           GO TO CC-2000TI ELSE MOVE IN-FIELD-1 TO CC-ACT GO TO 4900CPC.
       CC-4000-DATE-T.
           IF IN-FIELD = "?"
           DISPLAY " DATE DD OR MMDD OR  MMDDYY OR MMDDYYYY FORMAT"
               GO TO CC-2000TI.
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
           IF IN-FIELD-8 NUMERIC NEXT SENTENCE ELSE MOVE "?"
           TO IN-FIELD GO TO CC-4000-DATE-T.
           MOVE IN-FIELD-8 TO INPUT-DATE.
           IF T-MM OF INPUT-DATE < 01 OR > 12
               MOVE "?" TO IN-FIELD
           GO TO CC-4000-DATE-T.
           IF (T-DD OF INPUT-DATE > DAYS-IN-MONTH(T-MM OF INPUT-DATE))
             OR (T-DD OF INPUT-DATE = 00)
               MOVE "?" TO IN-FIELD
           GO TO CC-4000-DATE-T.
           MOVE CORR INPUT-DATE TO TEST-DATE
      *     IF T-DATE < TEST-DATE DISPLAY "FUTURE DATE NOT ALLOWED"
      *     MOVE "?" TO IN-FIELD GO TO CC-4000-DATE-T.
           MOVE TEST-DATE TO CC-DATE-T
           GO TO 4900CPC.
       CC-2-MOD2.
           IF IN-FIELD = "?"
           DISPLAY "TYPE THE 2ND MODIFIER OR <CR>"
           GO TO CC-2000TI.
           MOVE IN-FIELD-2 TO CC-MOD2 GO TO 4900CPC.
       CC-2-REC-STAT.
           IF IN-FIELD = "?"
           DISPLAY "0=NEW 1=BEEN BILLED 2=CLAIM SENT 3=BILL/CLAIM SENT"
           GO TO CC-2000TI.
           IF IN-FIELD-1 = "0" OR "1" OR "2" OR "3" NEXT SENTENCE
           ELSE DISPLAY "INVALID" GO TO CC-2000TI.
           MOVE IN-FIELD-1 TO CC-REC-STAT GO TO 4900CPC.
           GO TO 4900CPC.
       CC-2-SORCREF.
           IF IN-FIELD = "?"
           DISPLAY "ENTER THE SOURCE OF REFERRAL CODE"
           DISPLAY "1=SELF 2=MD 3=PUBLIC HEALTH CLINIC "
           DISPLAY "4=HOME HEALTH OR VISITING NURSE "
           DISPLAY "5=OTHER PRACTIONER"
           IF IN-FIELD-1 = SPACE MOVE "1" TO IN-FIELD-1
           DISPLAY "SELF".
           IF IN-FIELD-1 = "1" OR "2" OR "3" OR "4" OR "5"
           MOVE IN-FIELD-1 TO CC-SORCREF GO TO 4900CPC.
           DISPLAY IN-FIELD-1 " BAD" GO TO CC-2000TI.
       CC-2-DATE-A.
           IF IN-FIELD = "?"
           DISPLAY "ENTER THE DATE THE CC-CLAIM WAS SENT MMDDYY FORMAT"
           DISPLAY "T = TODAY""S DATE"
           GO TO CC-2000TI.
           IF IN-FIELD-8 = "T" ACCEPT CC-DATE-A FROM CENTURY-DATE
           GO TO 4900CPC.
           IF IN-FIELD-8 NUMERIC NEXT SENTENCE ELSE DISPLAY "INVALID"
           GO TO CC-2000TI.
           MOVE IN-FIELD-8 TO INPUT-DATE.
           IF T-MM OF INPUT-DATE < 01 OR > 12
           DISPLAY "INVALID" GO TO CC-2000TI.
           IF T-DD OF INPUT-DATE > DAYS-IN-MONTH(T-MM OF INPUT-DATE)
           DISPLAY "INVALID" GO TO CC-2000TI.
           MOVE CORR INPUT-DATE TO TEST-DATE
           MOVE TEST-DATE TO CC-DATE-A.
           IF T-DATE < CC-DATE-A DISPLAY "FUTURE DATE" BELL
           GO TO CC-2000TI.
           GO TO 4900CPC.
       CC-2-COLLT.
           IF IN-FIELD = "?"
           DISPLAY "1-9 TO DEFINE A COLLECTION AGENCY THIS CHARGE"
           DISPLAY "IS GOING TO BE SENT TO"
           DISPLAY " 0 = NOT IN COLLECTION"
           GO TO CC-2000TI.
           IF IN-FIELD-1 NOT NUMERIC DISPLAY "INVALID" GO TO CC-2000TI.
           MOVE IN-FIELD-1 TO CC-COLLT.
           GO TO 4900CPC.
       CC-2-MOD3.
           IF IN-FIELD = "?"
           DISPLAY "TYPE THE 3RD MODIFIER OR <CR>"
           GO TO CC-2000TI.
           MOVE IN-FIELD-2 TO CC-MOD3 GO TO 4900CPC.
       CC-2-ASSIGN.
           IF IN-FIELD = "?"
           DISPLAY "A=ASSIGNED U= UNASSIGNED"
           GO TO CC-2000TI.
           IF IN-FIELD = "A" OR "U"
           MOVE IN-FIELD-1 TO CC-ASSIGN GO TO 4900CPC.
           DISPLAY "INVALID" GO TO CC-2000TI.
       CC-2-NEIC-ASSIGN.
           IF IN-FIELD = "?"
           DISPLAY "A=ASSIGNED U=UNASSIGNED  FOR INSURANCE ASSIGNMENT"
           GO TO CC-2000TI.
           IF IN-FIELD = "A" OR "U"
           MOVE IN-FIELD-1 TO CC-NEIC-ASSIGN GO TO 4900CPC.
           DISPLAY "INVALID" GO TO CC-2000TI.
       CC-2-ACC-TYPE.
           IF IN-FIELD = "?"
           DISPLAY "<CR> = NOT AN ACCIDENT 1=EMPLOYM. 2=CAR 3=OTHER"
           GO TO CC-2000TI.
           IF IN-FIELD = SPACE OR "1" OR "2" OR "3" MOVE IN-FIELD-1
           TO CC-ACC-TYPE GO TO 4900CPC.
           DISPLAY "INVALID" GO TO CC-2000TI.
       CC-2-ADMIT.
           IF IN-FIELD = "?"
           DISPLAY "ENTER THE HOSPITAL ADMIT DATE"
           GO TO CC-2000TI.
           IF IN-FIELD-8 = SPACE MOVE "00000000" TO CC-DATE-M
           GO TO 4900CPC.
           IF IN-FIELD-8 NUMERIC NEXT SENTENCE ELSE DISPLAY "INVALID"
           GO TO CC-2000TI.
           MOVE IN-FIELD-8 TO INPUT-DATE.
           IF T-MM OF INPUT-DATE < 01 OR > 12
           DISPLAY "INVALID" GO TO CC-2000TI.
           IF T-DD OF INPUT-DATE > DAYS-IN-MONTH(T-MM OF INPUT-DATE)
           DISPLAY "INVALID" GO TO CC-2000TI.
           MOVE CORR INPUT-DATE TO TEST-DATE
           MOVE TEST-DATE TO CC-DATE-M.
           IF T-DATE < CC-DATE-M DISPLAY "FUTURE DATE" BELL
           GO TO CC-2000TI.
           GO TO 4900CPC.
       CC-2-AUTH.

           IF IN-FIELD = "?"
           DISPLAY "1 = ADD A NEW AUTH. NUMBER"
           DISPLAY "2 = REVISE A AUTH. NUMBER"
           DISPLAY "3 = UNSET(BLANK OUT) AUTH. NUMBER"
           DISPLAY "4 = ADD A NEW NDC NUMBER"
           DISPLAY "5 = REVISE AN NDC NUMBER"
           DISPLAY "6 = UNSET(BLANK OUT) AN NDC NUMBER"
           DISPLAY "X  = NO ACTION TAKEN"
           GO TO CC-2000TI.
           IF IN-FIELD = "X" GO TO 4900DEE.
           IF NOT (IN-FIELD-2 = "1 " OR "2 " OR "3 " OR "4 "
                   OR "5 " OR "6 ")
               DISPLAY "INVALID"
               GO TO CC-2000TI.
           MOVE 0 TO FLAG 
      *     MOVE CD-AUTH TO ALF-1
           PERFORM AUTH-1 THRU AUTH-1-EXIT
           GO TO 4900CPC.

       4900CPC.
           GO TO 1400CPC.
       4910CPC.
           EXIT.
       5000-WRITE-CHARCUR.
           IF CC-ASSIGN = "S" OR CC-NEIC-ASSIGN = "S"
           DISPLAY "SET THE ASSIGNMENT FIELDS TO A OR U"
           GO TO CC-2000TI.
           IF CC-REC-STAT < "2" GO TO 5WC1.
       5WC.  DISPLAY "DO YOU WISH TO SET STATUS TO RESUBMIT CLAIM? Y/N".
           ACCEPT ANS.
           IF ANS = "?"
           DISPLAY "TYPE Y AND THIS ASSIGNED CHARGE WILL BE REPROCESSED"
           DISPLAY "ELECTRONICALLY OR BY CLAIM FORM"
           DISPLAY "TYPE N AND THE RECORD WILL NOT BE SUBMITTED AGAIN."
           GO TO 5WC.
           IF ANS = "N" GO TO 5WC1.
           IF ANS NOT = "Y" GO TO 5WC.
           IF CC-PAPER = "A" MOVE "P" TO CC-PAPER.
           IF CC-REC-STAT = "3" MOVE "1" TO CC-REC-STAT.
           IF CC-REC-STAT = "2" MOVE "0" TO CC-REC-STAT.
       5WC1.
           IF HOLDASSIGN = CC-ASSIGN GO TO 5WC2.
           DISPLAY "ASSIGNMENT MODE HAS CHANGED, SO.."
           DISPLAY "RE-AGED TO CURRENT".
           MOVE "00000000" TO CC-DATE-A.
       5WC2.
           MOVE CC-KEY8 TO G-GARNO
           READ GARFILE INVALID DISPLAY "BAD GARNO " G-GARNO
           GO TO 1000-ACTION.
           IF CC-PAYCODE NOT = "001" AND CC-PAYCODE NOT = G-PRINS
           AND CC-PAYCODE NOT = G-SEINS AND CC-PAYCODE NOT = G-TRINS
           DISPLAY "INSURANCE CODE ON THIS RECORD DOES NOT MATCH"
           DISPLAY "EITHER INSURANCE CODE FOR THE ACCOUNT"
           DISPLAY "THIS MAY NOT BE VALID.  THIS IS ONLY A WARNING".
           PERFORM DX-1 THRU DX-1-EXIT.



           MOVE CHARCUR01 TO CURBACK.
           REWRITE CHARCUR01 INVALID KEY DISPLAY "NO UPDATE."
           DISPLAY "THIS SHOULD NOT HAPPEN! CONTACT THE DATA CENTER."
           DISPLAY "THIS PROGRAM IS TERMINATED!"
           GO TO 9100-CLOSE-MASTER-FILE.
           UNLOCK CHARCUR RECORD
      *    ISQUIET CHARCUR WITH 1 AND 1
           DISPLAY "UPDATE MADE"  GO TO 1000-ACTION.
       CC-1200-FIND. START CHARCUR KEY > CHARCUR-KEY INVALID
           DISPLAY " END OF FILE" GO TO 1000-ACTION.
           MOVE "D" TO UPDOWN.
       CC-1200-FIND-20.
           MOVE 0 TO X.
           MOVE 0 TO FLAG.
           PERFORM CC-1200-SEARCH THRU CC-1200-SEARCH-EXIT.
           IF FLAG = 1 DISPLAY "END OF SEARCH"
           GO TO 1000-ACTION.
       CC-1200-FIND-QUES.
           DISPLAY "? " UPDOWN.
           ACCEPT ANS.
           IF ANS = SPACE  GO TO CC-1200-FIND-20.
           IF ANS = "?"
           DISPLAY "U OR D TO SCAN UP OR DOWN"
           DISPLAY "A <CR> WILL PRODUCE 9 MORE RECORDS"
           DISPLAY "ALL ELSE BACK TO OPTION"
           GO TO CC-1200-FIND-QUES.
           IF ANS = "U" MOVE "U" TO UPDOWN GO TO CC-1200-FIND-20.
           IF ANS = "D" MOVE "D" TO UPDOWN GO TO CC-1200-FIND-20.
           GO TO 1000-ACTION.
       CC-1200-SEARCH.
           ADD 1 TO X.
           IF X > 9 GO TO CC-1200-SEARCH-EXIT.
           IF UPDOWN = "D" GO TO CC-1200-DOWN.
           GO TO CC-1200-UP.
       CC-1200-DOWN.
           READ CHARCUR NEXT AT END MOVE 1 TO FLAG
           DISPLAY "END OF FILE" 
           GO TO CC-1200-SEARCH-EXIT.
           GO TO CC-1200-HERE.
       CC-1200-UP.
           READ CHARCUR PREVIOUS AT END MOVE 1 TO FLAG
           DISPLAY "BEGINNING OF FILE"
           GO TO CC-1200-SEARCH-EXIT.
       CC-1200-HERE.
           MOVE SPACES TO TEMP-FIELD01.
           MOVE CHARCUR-KEY TO CCKEY-TAB(X)
           MOVE CC-PATID TO EIGHTPARTID.
           IF EIGHT-1 = "G" GO TO CC-1200-S.
           MOVE CC-PATID TO P-PATNO.
           READ PATFILE INVALID MOVE "*** NO PATNO ***" TO P-PATNAME.
           MOVE P-PATNAME TO NAME14.
           GO TO CC-1200-S1.
       CC-1200-S.
           MOVE CC-PATID TO G-GARNO.
           READ GARFILE INVALID MOVE "*** NO NAME ***" TO G-GARNAME.
           MOVE G-GARNAME TO NAME14.
       CC-1200-S1.
           MOVE CC-AMOUNT TO NEF-8
           MOVE CC-DATE-T TO TEST-DATE
           MOVE CORR TEST-DATE TO DISP-DATE
           DISPLAY X " " CHARCUR-KEY " " DISP-DATE " " 
           CC-PROC CC-MOD2 " " NEF-8 " " CC-PAYCODE " " CC-CLAIM " " 
           CC-PAPER " " NAME14
           IF CC-AUTH =  "1" PERFORM READ-AUTH
           MOVE 0 TO FLAG
            IF AUTH-NUM NOT = SPACE
             DISPLAY "AUTHNUM= " AUTH-NUM
            END-IF
            IF AUTH-NDC NOT = SPACE
             DISPLAY "AUTHNDC= " AUTH-NDC
            END-IF
           END-IF
           
           GO TO CC-1200-SEARCH.
       CC-1200-SEARCH-EXIT.   EXIT.
       LC-0. READ CHARCUR INVALID DISPLAY "INVALID" GO TO LC-0-EXIT.
           PERFORM LC-1.
       LC-0-EXIT. EXIT.
       LC-1.
           MOVE CC-AMOUNT TO NEF-8
           MOVE CC-DAT1 TO TEST-DATE MOVE CORR TEST-DATE
           TO DISPLAY-DATE
           MOVE CC-DATE-T TO TEST-DATE MOVE CORR TEST-DATE
           TO DISPLAY-DATE-TOO
           PERFORM LC-2.

           DISPLAY CHARCUR-KEY " " CC-PATID " "CC-CLAIM " " CC-PLACE
           "  " CC-PROC " " CC-MOD2  "   " CC-MOD3  NEF-8
           DISPLAY
           "KEY         PATIENT  CLAIM  PL PROC    MOD2 MOD3   $$$"


           DISPLAY " "
           DISPLAY CC-DIAG " " CC-DX2 " " CC-DX3 " " CC-DX4
      *     " " CC-DX5 " " CC-DX6
           DISPLAY "DX1     DX2     DX3     DX4"
      *         DX5     DX6"

           DISPLAY " "
           DISPLAY CC-SERVICE "    " CC-DOCP "    " CC-DOCR "   "
           CC-WORK "    " DISPLAY-DATE " "  CC-ACC-TYPE "     " 
           DISPLAY-DATE-TOO " " CC-PAYCODE "  " CC-RESULT "    " 
           CC-ACT "   " CC-EPSDT "     " CC-SORCREF
           DISPLAY "TYPE PRVD  REF   UNIT     ACCDT  ACCTP    DATE     "
           "INS RSLT ACT EPSDT SOCR"
           DISPLAY " "
           MOVE CC-DATE-A TO TEST-DATE MOVE CORR TEST-DATE
           TO DISPLAY-DATE
           MOVE CC-DATE-M TO TEST-DATE MOVE CORR TEST-DATE
           TO DISPLAY-DATE-TOO

           DISPLAY CC-ASSIGN "    " CC-NEIC-ASSIGN "        "
           CC-COLLT "      "
           CC-PAPER "      " CC-REC-STAT "       " DISPLAY-DATE
           "    " CC-AUTH "    " DISPLAY-DATE-TOO

           DISPLAY "ASGN CLM-ASGN COLLT  PAPER  STATUS   CLM-DATE "
           " AUTH/NDC  ADMIT-DT"
           MOVE CC-DATE-P TO TEST-DATE MOVE CORR TEST-DATE
           TO DISPLAY-DATE
           DISPLAY "RECORD ENTERED: " DISPLAY-DATE.
           IF CC-AUTH = "1" PERFORM READ-AUTH
            IF AUTH-NUM NOT = SPACE
             DISPLAY "AUTHNUM = " AUTH-NUM
            END-IF
            IF AUTH-NDC NOT = SPACE
             DISPLAY "NDCNUM = " AUTH-NDC
            END-IF
           END-IF.

       LC-2. MOVE CC-PATID TO EIGHTPARTID
           IF EIGHT-1 = "P" PERFORM LC-4.
           IF EIGHT-1 = "G" PERFORM LC-3.
       LC-3.
           MOVE CC-PATID TO G-GARNO
           READ GARFILE INVALID DISPLAY "NO GARNO " G-GARNO.
           DISPLAY G-GARNAME.
       LC-4. MOVE CC-PATID TO P-PATNO
           READ PATFILE INVALID DISPLAY "NO DEPENDANT RECORD " P-PATNO.
           DISPLAY P-PATNAME.
       CC-1.
           READ GARFILE INVALID DISPLAY "INVALID" GO TO 1000-ACTION.
           DISPLAY G-GARNO " " G-GARNAME
           MOVE SPACE TO CC-KEY3
           MOVE G-GARNO TO CC-KEY8
           START CHARCUR KEY > CHARCUR-KEY INVALID DISPLAY "NO RECORDS"
           GO TO 1000-ACTION.
       CC-2. READ CHARCUR NEXT WITH LOCK AT END GO TO 1000-ACTION.
           IF CC-KEY8 NOT = G-GARNO GO TO 1000-ACTION.
           IF CC-COLLT = "1" GO TO CC-2.
           MOVE CC-AMOUNT TO NEF-8
           MOVE CC-DATE-T TO TEST-DATE
           MOVE CORR TEST-DATE TO DISP-DATE
           DISPLAY CC-PAYCODE " " CC-PROC " " DISP-DATE " " NEF-8.
           DISPLAY "CODE FOR COLLECTION? Y/N/X"
           ACCEPT ANS
           IF ANS = "X" GO TO 1000-ACTION.
           IF ANS = "Y"
           MOVE "1" TO CC-COLLT
           REWRITE CHARCUR01
           DISPLAY "COLLECTION" GO TO CC-2.
           DISPLAY "BYPASSED" GO TO CC-2.
       RA-1.
           READ GARFILE INVALID DISPLAY "INVALID" GO TO 1000-ACTION.
           DISPLAY G-GARNO " " G-GARNAME
           IF G-DUNNING = "1" GO TO RA-1-1.
           DISPLAY "YOU MUST CHANGE DUNNING TO 1 ON THIS ACCOUNT"
           DISPLAY "TO START RE-BILLING THE PATIENT. DO IT NOW? Y/N"
           ACCEPT ANS 
           IF ANS NOT = "Y" GO TO RA-1-1.
           MOVE G-GARNO TO ALF-8
           CLOSE GARFILE
           OPEN I-O GARFILE
           MOVE ALF-8 TO G-GARNO
           READ GARFILE WITH LOCK INVALID DISPLAY "INVALID" 
           CLOSE GARFILE
           OPEN INPUT GARFILE
           GO TO  1000-ACTION.
           MOVE "1" TO G-DUNNING
           REWRITE G-MASTER
           CLOSE GARFILE
           OPEN INPUT GARFILE.
           DISPLAY "DUNNING CHANGED TO 1".
       RA-1-1.   
           MOVE SPACE TO CC-KEY3
           MOVE G-GARNO TO CC-KEY8
           START CHARCUR KEY > CHARCUR-KEY INVALID DISPLAY "NO RECORDS"
           GO TO 1000-ACTION.
       RA-2. READ CHARCUR NEXT WITH LOCK AT END GO TO 1000-ACTION.
           IF CC-KEY8 NOT = G-GARNO GO TO 1000-ACTION.
           IF CC-ASSIGN = "A" OR CC-DATE-A = "00000000" GO TO RA-2.
           MOVE CC-AMOUNT TO NEF-8
           MOVE CC-DATE-T TO TEST-DATE
           MOVE CORR TEST-DATE TO DISP-DATE
           DISPLAY CC-PAYCODE " " CC-PROC " " DISP-DATE " " NEF-8
           MOVE CC-DATE-A TO TEST-DATE
           MOVE CORR TEST-DATE TO DISP-DATE
           DISPLAY "AGE-DATE =  " DISP-DATE
           DISPLAY "RE-AGE TO CURRENT? Y=YES  <CR>=NO  X=QUIT"
           ACCEPT ANS
           IF ANS = "X" GO TO 1000-ACTION.
           IF ANS = "Y"
           MOVE "00000000" TO CC-DATE-A
           REWRITE CHARCUR01
           DISPLAY "RE-AGED" GO TO RA-2.
           DISPLAY "BYPASSED" GO TO RA-2.
       
       LI-1.
           MOVE PAYFILE-KEY TO INS-KEY
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
       INS-1. DISPLAY "SEARCH TYPE"
           DISPLAY "1 = BY NUMBER "
           DISPLAY "2 = BY NAME"
           DISPLAY "3 = BY ASSIGNMENT CODE"
           DISPLAY "4 = BY NEIC CODE"
           DISPLAY "5 = BY NEIC ASSIGNM CODE"
           DISPLAY "6 = BY CLAIM-TYPE"
           DISPLAY "7 = BY CITY".
       INS-2.  ACCEPT IN-FIELD.
           IF IN-FIELD = "?"
           DISPLAY "X = BACK OR 1-6 SEARCH METHOD"
           GO TO INS-2.
           IF IN-FIELD = "X" DISPLAY "END SEARCH" GO TO INS-1-EXIT.
           IF IN-FIELD = "1" OR "2" OR "3" OR "4" OR "5" OR "6" OR "7"
           NEXT SENTENCE ELSE DISPLAY "BAD" GO TO INS-2.
           MOVE IN-FIELD-1 TO ALF-1.
       INS-3. DISPLAY "STARTING POINT?".
           ACCEPT IN-FIELD.
           IF IN-FIELD = "?"
           DISPLAY "ENTER WHERE TO START LOOKING"
           DISPLAY "BK = BACK TO SEARCH METHOD"
           DISPLAY "OR X = BACK TO OPTION" GO TO INS-3.
           IF IN-FIELD = "X" GO TO INS-1-EXIT.
           IF IN-FIELD = "BK" GO TO INS-1.
           MOVE 0 TO X
           IF ALF-1 = "1" MOVE SPACE TO RIGHT-3
           UNSTRING IN-FIELD DELIMITED BY " " INTO RIGHT-3
           INSPECT RIGHT-3 REPLACING LEADING " " BY "0"
           MOVE RIGHT-3 TO INS-KEY
           MOVE "NUMBER  " TO ALF-8
           START INSFILE KEY NOT < INS-KEY INVALID GO TO INS-1-END.
           IF ALF-1 = "2" MOVE IN-FIELD TO INS-NAME
           MOVE "NAME    " TO ALF-8
           START INSFILE KEY NOT < INS-NAME INVALID GO TO INS-1-END.
           IF ALF-1 = "3" MOVE IN-FIELD TO INS-ASSIGN
           MOVE "ASSIGNMT" TO ALF-8
           START INSFILE KEY NOT < INS-ASSIGN INVALID GO TO INS-1-END.
           IF ALF-1 = "4" MOVE IN-FIELD TO INS-NEIC
           MOVE "NEICCODE" TO ALF-8
           START INSFILE KEY NOT < INS-NEIC INVALID GO TO INS-1-END.
           IF ALF-1 = "5" MOVE IN-FIELD TO INS-NEIC-ASSIGN
           MOVE "NEICASGM" TO ALF-8
           START INSFILE KEY NOT < INS-NEIC-ASSIGN INVALID 
           GO TO INS-1-END.
           IF ALF-1 = "6" 
           MOVE IN-FIELD TO INS-CLAIMTYPE
           MOVE "CLM-TYPE" TO ALF-8
           START INSFILE KEY NOT < INS-CLAIMTYPE INVALID 
           GO TO INS-1-END.
           IF ALF-1 = "7" 
           MOVE IN-FIELD TO INS-CITY
           MOVE "CITY    " TO ALF-8
           START INSFILE KEY NOT < INS-CITY INVALID 
           GO TO INS-1-END.
       INS-4. READ INSFILE NEXT AT END DISPLAY "END OF FILE"
           GO TO INS-1-EXIT.
           DISPLAY INS-KEY " " INS-NAME " " INS-ASSIGN " " INS-CLAIMTYPE
           " " INS-NEIC " " INS-NEIC-ASSIGN
           " " INS-STATUS " " INS-CITY " " INS-STREET
           ADD 1 TO X
           IF X > 8 MOVE 0 TO X DISPLAY "BY " ALF-8 ACCEPT ANS
           IF ANS NOT = SPACE GO TO INS-1-EXIT.
           GO TO INS-4.
       INS-1-END. DISPLAY "END OF FILE".
       INS-1-EXIT. EXIT.
       READ-AUTH-CD.
           MOVE CD-KEY8 TO AUTH-KEY8
           MOVE CD-CLAIM TO AUTH-KEY6
           READ AUTHFILE INVALID MOVE "1" TO FLAG
           MOVE SPACE TO AUTH-NUM AUTH-NDC.

       READ-AUTH.
           MOVE CC-KEY8 TO AUTH-KEY8
           MOVE CC-CLAIM TO AUTH-KEY6
           READ AUTHFILE INVALID MOVE "1" TO FLAG
           MOVE SPACE TO AUTH-NUM AUTH-NDC.
       AUTH-1.
           MOVE CC-KEY8 TO AUTH-KEY8
           MOVE CC-CLAIM TO AUTH-KEY6
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
           MOVE "1" TO CC-AUTH
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
           IF NOT (ALF-11 NUMERIC OR ALF-11 = SPACE)
           DISPLAY "INVALID" GO TO AUTH-3.
           MOVE ALF-11 TO AUTH-NDC
           MOVE "1" TO CC-AUTH
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
           IF IN-FIELD-2 = "4 " OR "5 " OR "6 " GO TO AUTH-6.
       AUTH-5.
           DISPLAY "AUTHORIZATION NUMBER"
           ACCEPT ALF-15
           IF ALF-15 = "?" 
           DISPLAY "ENTER THE AUTH. NUMBER OR <CR> TO UNSET IT "
           DISPLAY "BK OR X  = NO CHANGE"
           GO TO AUTH-5.
           IF ALF-15 = "BK" OR "X" 
           DISPLAY "PRIOR AUTH. NUMBER NOT CHANGED"
           GO TO AUTH-1-EXIT.
           MOVE "1" TO CC-AUTH
           MOVE ALF-15 TO AUTH-NUM
           MOVE "01" TO AUTH-QNTY
           MOVE SPACE TO AUTH-NDC
           MOVE SPACE TO AUTH-FILLER
           ACCEPT AUTH-DATE-E FROM CENTURY-DATE
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
           GO TO AUTH-6.
           IF ALF-11 = "BK" OR "X" 
            DISPLAY "PRIOR NDC NUMBER NOT CHANGED"
           GO TO AUTH-1-EXIT.
           IF NOT (ALF-11 NUMERIC OR ALF-11 = SPACE)
           DISPLAY "INVALID" GO TO AUTH-6.
           MOVE ALF-11 TO AUTH-NDC
           MOVE "1" TO CC-AUTH
           MOVE SPACE TO AUTH-QNTY
           MOVE SPACE TO AUTH-FILLER
           MOVE SPACE TO AUTH-NUM
           ACCEPT AUTH-DATE-E FROM CENTURY-DATE
           WRITE AUTHFILE01. 
       AUTH-1-EXIT.
           EXIT.

       10-PR. 
           MOVE CC-KEY8 TO G-GARNO
           READ GARFILE INVALID DISPLAY "BAD ACCT # "  
           GO TO 1000-ACTION.
           READ CHARCUR INVALID DISPLAY "BAD SELECTION"
           GO TO 1000-ACTION.
           IF CC-PAYCODE = G-PRINS OR G-SEINS OR G-TRINS OR "001"
           OR "091" NEXT SENTENCE
           ELSE DISPLAY "THE INSURANCE CODE ON THE CHARGE MUST MATCH"
           DISPLAY "ONE OF THE 3 INSURANCES CODES ON THE ACCOUNT."
           GO TO 1000-ACTION.
           MOVE "2" TO FILEOUT01
           IF CC-PAYCODE = "004" OR "064"
           MOVE "4" TO FILEOUT01.
           IF CC-PAYCODE = "003" OR "028" OR "062" OR "043"
           MOVE "3" TO FILEOUT01.
           WRITE FILEOUT01
           MOVE SPACE TO FILEOUT01

           STRING CC-PAYCODE CC-KEY8 CHARCUR-KEY CC-DATE-T CC-ASSIGN
           CC-PLACE CC-DOCP CC-PAPER 
           DELIMITED BY "!!!!" INTO FILEOUT01
           WRITE FILEOUT01.
           CLOSE FILEOUT
           CALL "SYSTEM" USING "pap-4"
           OPEN OUTPUT FILEOUT
           GO TO 1000-ACTION.

       CD-SET.
           DISPLAY "CHARGE DATE?"
           ACCEPT DATE-OF-CHARGE
           IF DATE-OF-CHARGE = "?"
           DISPLAY "ENTER DATE OF CHARGE TO LIST"  
           DISPLAY "DD MMDD MMDDYY OR MMDDYYYY FORMAT"
           DISPLAY "<CR> = ALL CHARGE DATES"
           GO TO CD-SET.
           IF DATE-OF-CHARGE = SPACE GO TO CD-EXIT.
           MOVE DATE-OF-CHARGE TO IN-FIELD-8
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
      *     DISPLAY T-CC OF INPUT-DATE T-YY OF INPUT-DATE " ASSUMED"
           MOVE INPUT-DATE TO IN-FIELD-8.
           IF IN-FIELD-8 NOT NUMERIC DISPLAY "BAD" GO TO CD-SET.
           MOVE IN-FIELD-8 TO INPUT-DATE.
           IF T-MM OF INPUT-DATE < 01 OR > 12
           DISPLAY "BAD" GO TO CD-SET.
           IF T-DD OF INPUT-DATE > DAYS-IN-MONTH(T-MM OF INPUT-DATE)
           DISPLAY "BAD" GO TO CD-SET.    
           MOVE CORR INPUT-DATE TO TEST-DATE
           IF T-DATE < TEST-DATE DISPLAY "FUTURE DATE NOT ALLOWED"
           DISPLAY "BAD" GO TO CD-SET.
           MOVE TEST-DATE TO DATE-OF-CHARGE.
       CD-EXIT. EXIT.
       D3. 
           MOVE ALF-T TO IN-FIELD-8
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
           DISPLAY T-CC OF INPUT-DATE T-YY OF INPUT-DATE " ASSUMED".
           MOVE INPUT-DATE TO ALF-T.
       D3-EXIT. EXIT.


       DX-1.
           IF CC-DIAG = "0000000"
             AND CC-DX2 = "0000000" AND CC-DX3 = "0000000"
             AND CC-DX4 = "0000000"
      *       AND CC-DX5 = "0000000"
      *       AND CC-DX6 = "0000000"
              DISPLAY "ALERT !!  NO DIAGS PRESENT ON CHARGE." 
              ACCEPT ANS
              GO TO DX-1-EXIT
           END-IF

           MOVE SPACE TO TABDX01
           STRING CC-DIAG CC-DX2 CC-DX3 CC-DX4
      *     CC-DX5 CC-DX6
           DELIMITED BY SIZE INTO TABDX01
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 3
           COMPUTE Z = X + 1
            PERFORM VARYING Y FROM Z BY 1 UNTIL Y > 4
             IF X NOT = Y
              IF TABDX(X) = "0000000" 
                MOVE TABDX(Y) TO TABDX(X)
                MOVE "0000000" TO TABDX(Y)
                 IF TABDX(X) NOT = "0000000"
                   MOVE 5 TO Y
                 END-IF
              END-IF
             END-IF
            END-PERFORM
           END-PERFORM
           MOVE TABDX(1) TO CC-DIAG
           MOVE TABDX(2) TO CC-DX2
           MOVE TABDX(3) TO CC-DX3
           MOVE TABDX(4) TO CC-DX4.
      *     MOVE TABDX(5) TO CC-DX5
      *     MOVE TABDX(6) TO CC-DX6.
       DX-1-EXIT. EXIT.


       CC10.
           IF IN-FIELD-7 = "F" GO TO 1DIAG-SEARCH.
           IF IN-FIELD-7 = "M" GO TO 1MAP.
           IF (IN-FIELD-TAB(1) NUMERIC) OR (IN-FIELD-TAB(1) = "V")
            MOVE SPACE TO ALF-7
            STRING IN-FIELD-7(1:5) "??" DELIMITED BY SIZE INTO ALF-7
            MOVE ALF-7 TO IN-FIELD-7
           END-IF.
           MOVE IN-FIELD-7 TO DIAG-KEY.
           READ DIAGFILE INVALID DISPLAY "NOT ON FILE"
           END-READ
           IF DIAG-TITLE(1:1) = "?" AND CC-DATE-T > "20160930"
            DISPLAY "THIS CODE IS INACTIVE"
           GO TO CC10-EXIT.
           IF (CC-DATE-T < "20151001" AND DIAG-KEY(6:2) NOT = "??")
            DISPLAY "USE ICD9 CODE WITH THIS DATE"
           GO TO CC10-EXIT.
           IF (CC-DATE-T > "20150930" AND DIAG-KEY(6:2) = "??")
            DISPLAY "USE  ICD10 CODE WITH THIS DATE"
           GO TO CC10-EXIT.
           DISPLAY DIAG-TITLE
           GO TO CC10-EXIT.


       1DIAG-SEARCH.
           MOVE 1 TO FLAG
           IF CC-DATE-T < "20151001" MOVE 9 TO FLAG.
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
           GO TO CC10-EXIT.
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
           GO TO CC10-EXIT.
           MOVE 0 TO X.
           GO TO 3DIAG.
       4DIAG.
           START DIAGFILE KEY NOT < DIAG-KEY INVALID
           DISPLAY "END OF FILE"
           MOVE 1 TO RETURN-FLAG
           GO TO CC10-EXIT.
           MOVE 0 TO X.
       3DIAG.
           READ DIAGFILE NEXT AT END
           DISPLAY "END OF FILE"
           MOVE 1 TO RETURN-FLAG
           GO TO 3DIAG-0.
           IF DIAG-TITLE(1:1) = "?" GO TO 3DIAG.

            IF FLAG = 9 AND DIAG-KEY >= "V9199??"
             DISPLAY "END OF FILE"
             GO TO 3DIAG-0
            END-IF
           IF ((FLAG = 9) AND (DIAG-KEY(6:2) not = "??"))
             OR ((FLAG = 1) AND(DIAG-KEY(6:2)    = "??"))
             GO TO 3DIAG
           END-IF.
           ADD 1 TO X
           MOVE X TO NEF2
             DISPLAY NEF2 " " DIAG-KEY " " DIAG-MEDB " " DIAG-TITLE
           MOVE DIAG-KEY TO TAGTAB(X) 

           IF X < 20 GO TO 3DIAG.
       3DIAG-0.
            IF RETURN-FLAG = 1 GO TO CC10-EXIT.
            DISPLAY "SELECT ?"
            ACCEPT ANS.
             IF ANS = "?"
               DISPLAY "PICK A NUMBER,X TO QUIT,ENTER FOR MORE"
               GO TO 3DIAG-0
             END-IF
             IF ANS = SPACE
                IF RETURN-FLAG = 1
                   GO TO CC10-EXIT
                END-IF
             MOVE 0 TO X GO TO 3DIAG
             END-IF
           IF ANS = "X" GO TO CC10-EXIT.
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
              go to CC10-exit
             END-IF
           END-IF.
           MOVE 0 TO X
           GO TO 1DIAG-SEARCH.
       1MAP.
           DISPLAY "ENTER A VALID ICD9"
           DISPLAY "X TO QUIT."
           ACCEPT ALF-5.
           MOVE ALF-5 TO TAG-ICD9-5
           MOVE SPACE TO TAG-ICD9-7
           START TAGDIAG KEY NOT < TAG-ICD9
             INVALID  GO TO CC10-EXIT
           END-START.
           MOVE 0 TO Y.
       2MAP.
           READ TAGDIAG NEXT AT END GO TO 4MAP.
           IF TAG-ICD9-5 NOT = ALF-5 GO TO 4MAP.
           MOVE TAG-ICD9-7 TO DIAG-KEY
           READ DIAGFILE INVALID GO TO 2MAP.
           IF DIAG-TITLE(1:1) = "?" GO TO 2MAP.

           ADD 1 TO Y
           MOVE TAG-ICD9-7 TO TAGTAB(Y)
           MOVE Y TO NEF2
           DISPLAY NEF2 " " TAG-ICD9-7 " " DIAG-TITLE.
       3MAP.
           IF Y = 0 GO TO 1MAP
           IF Y < 20 GO TO 2MAP.
       4MAP.
           DISPLAY "CHOOSE FROM THE LIST".
           ACCEPT ALF-2.

           IF ALF-2 = "?"
           DISPLAY "A = USE A DIFFERENT ICD9 CODE"
           DISPLAY "X = STOP THE MAPPING"
           DISPLAY "PICK A NUMBER FROM THE LIST"
           DISPLAY " TO MAKE A SELECTION"
           DISPLAY "ENTER KEY TO CONTINUE THE MAPPING"
           GO TO 4MAP.

           IF ALF-2 = "A" GO TO 1MAP.
           IF ALF-2 = "X" GO TO CC10-EXIT.
           IF ALF-2 = SPACE MOVE 0 TO Y GO TO 2MAP.
           MOVE SPACE TO RIGHT-2
           UNSTRING ALF-2 DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING LEADING " " BY "0"
           MOVE RIGHT-2 TO ALF-2
           IF ALF-2 NUMERIC
             MOVE ALF-2 TO NUM-2
             IF (NUM-2 > 0) AND (NUM-2 <= Y)
              MOVE TAGTAB(NUM-2) TO IN-FIELD-7
               IF CD-DATE-T < "20151001"
                DISPLAY "MUST USE ICD9 CODE FOR THIS DATE"
                DISPLAY
                CD-DATE-T(5:2) "-"CD-DATE-T(7:2) "-" CD-DATE-T(1:4)
                ACCEPT OMITTED
                GO TO CC10-EXIT
               END-IF
              MOVE 0 TO DIAG-FLAG
              GO TO CC10-EXIT
             END-IF
           END-IF
            GO TO 4MAP.

       CC10-EXIT.
           EXIT.



       9100-CLOSE-MASTER-FILE.
           CLOSE PAYFILE.
           CLOSE CHARCUR.
           CLOSE AUTHFILE.
           CLOSE GARFILE
           CLOSE PATFILE
           CLOSE CHARFILE
           CLOSE CMNTFILE
           CLOSE INSFILE.
           CLOSE DIAGFILE
           CLOSE PROCFILE
           CLOSE REFPHY.
           STOP RUN.
