      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
        IDENTIFICATION DIVISION.
       PROGRAM-ID. rpg002.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT NEWINS ASSIGN TO "S20" ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC RECORD KEY IS NEW-KEY
               LOCK MODE MANUAL.

           SELECT DIAGFILE ASSIGN TO "S25" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS DIAG-KEY
           ALTERNATE RECORD KEY IS DIAG-TITLE WITH DUPLICATES
           LOCK MODE MANUAL.

           SELECT RPGACTFILE ASSIGN TO "S30"     ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC        RECORD KEY IS RPG-ACTNO
           ALTERNATE RECORD KEY IS RPG-GARNO WITH DUPLICATES
           ALTERNATE RECORD KEY IS NAME-KEY WITH DUPLICATES
           LOCK MODE MANUAL.
         
           SELECT RPGCHARFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS RPGCHARFILE-KEY
           LOCK MODE MANUAL.
           
           SELECT RPGINSFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS RPGINS-KEY
           ALTERNATE RECORD KEY IS RPGINS-TITLE WITH DUPLICATES
           ALTERNATE RECORD KEY IS RPGINS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS RPGINS-STATE WITH DUPLICATES
           ALTERNATE RECORD KEY IS RPGINS-CMS WITH DUPLICATES
           ALTERNATE RECORD KEY IS RPGINS-GAP WITH DUPLICATES
           LOCK MODE MANUAL.
         
           SELECT RPGPROCFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS RPGPROC-KEY
           LOCK MODE MANUAL.

           SELECT FILEIN ASSIGN TO "S50"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S55"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT INSFILE ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
           LOCK MODE MANUAL.
         
           SELECT REFPHY ASSIGN TO "S65" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS REF-KEY
           ALTERNATE RECORD KEY IS REF-BSNUM  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CRNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-UPIN  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CDNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-NAME  WITH DUPLICATES
           LOCK MODE MANUAL.
         
           SELECT FILEOUT2 ASSIGN TO "S70"
           ORGANIZATION LINE SEQUENTIAL.
           
           SELECT NPIFILE ASSIGN TO "S75" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS NPI-KEY
             ALTERNATE RECORD KEY IS NPI-NAME WITH DUPLICATES
             ALTERNATE RECORD KEY IS NPI-REFKEY WITH DUPLICATES
             LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
       FD  NPIFILE.
       01  NPIFILE01.
           02 NPI-KEY PIC X(10).
           02 NPI-NAME PIC X(24).
           02 NPI-REFKEY PIC X(3).
           02 NPI-PLACE PIC X.
       FD  NEWINS.
       01  NEWINS01.
           02 NEW-KEY PIC X(32).
           02 NEW-GARNAME PIC X(24).
           02 NEW-BILLADD PIC X(22).
           02 NEW-STREET PIC X(22).
           02 NEW-CITY PIC X(18).
           02 NEW-STATE PIC X(2).
           02 NEW-ZIP PIC X(9).
           02 NEW-COLLT PIC X.
           02 NEW-PHONE.
             03 NEW-PHONE1 PIC XXX.
             03 NEW-PHONE2 PIC XXX.
             03 NEW-PHONE3 PIC X(4).
           02 NEW-SEX PIC X.
           02 NEW-RELATE PIC X.
           02 NEW-MSTAT PIC X.
           02 NEW-DOB PIC X(8).
           02 NEW-DUNNING PIC X.
           02 NEW-ACCTSTAT PIC X.
           02 NEW-PR-MPLR PIC X(4).
           02 NEW-PRINS PIC XXX.
           02 NEW-PR-ASSIGN PIC X.
           02 NEW-PR-OFFICE PIC X(4).
           02 NEW-PR-GROUP PIC X(10).
           02 NEW-PRIPOL.
             03 NEW-PRIPOL1 PIC X(9).
             03 NEW-PRIPOL2 PIC XXX.
             03 NEW-PR-FILLER PIC X(4).
           02 NEW-PRNAME PIC X(24).
           02 NEW-PR-RELATE PIC X.
           02 NEW-SE-MPLR PIC X(4).
           02 NEW-SEINS PIC XXX.
           02 NEW-SE-ASSIGN PIC X.
           02 NEW-TRINSIND PIC X.
           02 NEW-TRINS PIC XXX.
           02 NEW-SE-GROUP PIC X(10).
           02 NEW-SECPOL.
             03 NEW-SECPOL1 PIC X(9).
             03 NEW-SECPOL2 PIC XXX.
             03 NEW-SE-FILLER PIC X(4).
           02 NEW-SENAME PIC X(24).
           02 NEW-SE-RELATE PIC X.
           02 NEW-INSPEND PIC X(7).
           02 NEW-LASTBILL PIC X(8).
           02 NEW-ASSIGNM PIC X.
           02 NEW-PRIVATE PIC X.
           02 NEW-BILLCYCLE PIC X.
           02 NEW-DELETE PIC X.
           02 NEW-FILLER PIC XXX.
           02 NEW-GARNO PIC X(8).
           02 NEW-PRGRPNAME PIC X(15).
           02 NEW-SEGRPNAME PIC X(15).
           02 NEW-NAME-KEY PIC XXX.
           
       FD  DIAGFILE.
       01  DIAG01.
           02 DIAG-KEY.
              03 diag-9 PIC X(5).
              03 diag-10 pic xx.
           02 DIAG-TITLE.
             03 DIAG-T1 PIC XXXXX.
             03 DIAG-T2 PIC X(56).
           02 DIAG-MEDB PIC X(5).
       FD  RPGPROCFILE.
       01  RPGPROCFILE01.
           02 RPGPROC-KEY.
             03 RPGPROC-KEY1 PIC X(7).
             03 RPGPROC-KEY2 PIC X(4).
           02 RPGPROC-TYPE PIC X.
           02 RPGPROC-TITLE. 
              03 RPG-NT1 PIC X(4).
              03 RPG-NT2 PIC X(24).
           02 RPGPROC-AMOUNT PIC 9(4)V99.
       
       FD  RPGCHARFILE.
       01  RPGCHARFILE01.
           02 RPGCHARFILE-KEY.
             03 RPG-KEY8 PIC X(32).
             03 RPG-KEY3 PIC XXX.
           02 RPG-PATID PIC X(8).
           02 RPG-CLAIM PIC X(6).
           02 RPG-SERVICE PIC X.
           02 RPG-DIAG PIC X(7).
           02 RPG-PROC. 
              03 RPG-PROC1 PIC X(4).
              03 RPG-PROC2 PIC X(7).
           02 RPG-MOD2 PIC XX.
           02 RPG-MOD3 PIC XX.
           02 RPG-MOD4 PIC XX.
           02 RPG-AMOUNT PIC X(6).
           02 RPG-DOCR PIC X(3).
           02 RPG-DOCP PIC X(2).
           02 RPG-PAYCODE PIC XXX.
           02 RPG-STAT PIC X.
           02 RPG-WORK PIC XX.
           02 RPG-DAT1 PIC X(8).
           02 RPG-RESULT PIC X.
           02 RPG-ACT PIC X.
           02 RPG-SORCREF PIC X.
           02 RPG-COLLECT PIC X.
           02 RPG-AUTH PIC X.
           02 RPG-PAPER PIC X.
           02 RPG-PLACE PIC X.
           02 RPG-NAME PIC X(24).
           02 RPG-EPSDT PIC X.
           02 RPG-DATE-T PIC X(8).
           02 RPG-DATE-E PIC X(8).
           02 RPG-ORDER PIC X(6).
           02 RPG-DX2 PIC X(7).
           02 RPG-DX3 PIC X(7).
           02 RPG-DATE-A PIC X(8).
           02 RPG-ACC-TYPE PIC X.
           02 RPG-DATE-M PIC X(8).
           02 RPG-ASSIGN PIC X.
           02 RPG-NEIC-ASSIGN PIC X.
           02 RPG-DX4 PIC X(7).
           02 RPG-DX5 PIC X(7).
           02 RPG-DX6 PIC X(7).
           02 RPG-FUTURE PIC X(6).
       FD  RPGACTFILE.
       01  RPGACTFILE01.
           02 RPG-ACTNO PIC X(32).
           02 RPG-GARNAME PIC X(24).
           02 RPG-BILLADD PIC X(22).
           02 RPG-STREET PIC X(22).
           02 RPG-CITY PIC X(18).
           02 RPG-STATE PIC X(2).
           02 RPG-ZIP PIC X(9).
           02 RPG-COLLT PIC X.
           02 RPG-PHONE.
             03 RPG-PHONE1 PIC XXX.
             03 RPG-PHONE2 PIC XXX.
             03 RPG-PHONE3 PIC X(4).
           02 RPG-SEX PIC X.
           02 RPG-RELATE PIC X.
           02 RPG-MSTAT PIC X.
           02 RPG-DOB PIC X(8).
           02 RPG-DUNNING PIC X.
           02 RPG-ACCTSTAT PIC X.
           02 RPG-PR-MPLR PIC X(4).
           02 RPG-PRINS PIC XXX.
           02 RPG-PR-ASSIGN PIC X.
           02 RPG-PR-OFFICE PIC X(4).
           02 RPG-PR-GROUP PIC X(10).
           02 RPG-PRIPOL.
             03 RPG-PRIPOL1 PIC X(9).
             03 RPG-PRIPOL2 PIC XXX.
             03 RPG-PR-FILLER PIC X(4).
           02 RPG-PRNAME PIC X(24).
           02 RPG-PR-RELATE PIC X.
           02 RPG-SE-MPLR PIC X(4).
           02 RPG-SEINS PIC XXX.
           02 RPG-SE-ASSIGN PIC X.
           02 RPG-TRINSIND PIC X.
           02 RPG-TRINS PIC XXX.
           02 RPG-SE-GROUP PIC X(10).
           02 RPG-SECPOL.
             03 RPG-SECPOL1 PIC X(9).
             03 RPG-SECPOL2 PIC XXX.
             03 RPG-SE-FILLER PIC X(4).
           02 RPG-SENAME PIC X(24).
           02 RPG-SE-RELATE PIC X.
           02 RPG-INSPEND PIC X(7).
           02 RPG-LASTBILL PIC X(8).
           02 RPG-ASSIGNM PIC X.
           02 RPG-PRIVATE PIC X.
           02 RPG-BILLCYCLE PIC X.
           02 RPG-DELETE PIC X.
           02 RPG-FILLER PIC XXX.
           02 RPG-GARNO PIC X(8).
           02 RPG-PRGRPNAME PIC X(15).
           02 RPG-SEGRPNAME PIC X(15).
           02 NAME-KEY PIC XXX.

       FD  RPGINSFILE.
       01  RPGINSFILE01.   
           02 RPGINS-KEY PIC X(12).
           02 RPGINS-TITLE PIC X(40).
           02 RPGINS-BOX PIC X(40).
           02 RPGINS-STREET PIC X(40).
           02 RPGINS-CITY PIC X(20).
           02 RPGINS-STATE PIC XX.
           02 RPGINS-ZIP PIC X(9).
           02 RPGINS-PHONE PIC X(10).
           02 RPGINS-CMS PIC XXX.
           02 RPGINS-GAP PIC X(7).
           02 RPGINS-FUTURE PIC X.
       
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
       FD  FILEOUT.
       01  FILEOUT01 PIC X(800).
       FD  FILEOUT2.
       01  FILEOUT201 PIC X(800).
       FD  FILEIN.
       01  FILEIN01.
           02 FI-PATNAMEL PIC X(24).
           02 FI-PATNAMEF PIC X(24).
           02 FI-PAT-STR1 PIC X(22).
           02 FI-PAT-STR2 PIC X(22).
           02 FI-PAT-CITY PIC X(18).
           02 FI-PAT-STATE PIC XX.
           02 FI-PAT-ZIP PIC X(9).
           02 FI-PAT-DOB PIC X(10).
           02 FI-PAT-SEX PIC X.
           02 FI-PRIM-ALFA PIC X(10).
           02 FI-PRIM-NAME PIC X(30).
           02 FI-PRIM-STR1 PIC X(22).
           02 FI-PRIM-CITY PIC X(18).
           02 FI-PRIM-STATE PIC XX.
           02 FI-PRIM-ZIP PIC X(10).
           02 FI-PRIM-GRP PIC X(10).
           02 FI-PRIM-POL PIC X(16).
           02 FI-PRIM-NAMEL PIC X(24).
           02 FI-PRIM-NAMEF PIC X(24).
           02 FI-PRIM-SUBSEX PIC X(10).
           02 FI-PRIM-SUBRELATE PIC XXXX.
           02 FI-SEC-ALFA PIC X(10).
           02 FI-SEC-NAME PIC X(30).
           02 FI-SEC-STR1 PIC X(22).
           02 FI-SEC-CITY PIC X(18).
           02 FI-SEC-STATE PIC XX.
           02 FI-SEC-ZIP PIC X(10).
           02 FI-SEC-GRP PIC X(10).
           02 FI-SEC-POL PIC X(16).
           02 FI-SEC-NAMEL PIC X(24).
           02 FI-SEC-NAMEF PIC X(24).
           02 FI-SEC-SUBSEX PIC X(10).
           02 FI-SEC-SUBRELATE PIC XXXX.
           02 FI-PROC.
               03 FI-PROC1 PIC X(5).
               03 FI-PROC2 PIC XX.
           02 FI-DX1 PIC X(8).
           02 FI-DX2 PIC X(8).
           02 FI-DX3 PIC X(8).
           02 FI-DX4 PIC X(8).
           02 FI-DATE-T PIC X(10).
           02 FI-PROVNPI PIC X(10).
           02 FI-DAT1 PIC X(10).
           02 FI-3RD-ALFA PIC X(10).
           02 FI-3RD-POL PIC X(16).
           02 FI-4TH-ALFA PIC X(10).
           02 FI-4TH-POL PIC X(16).

       FD  REFPHY
           BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS REFPHY01.
       01  REFPHY01.
           02 REF-KEY PIC XXX.
           02 REF-BSNUM PIC X(5).
           02 REF-CRNUM PIC X(6).
           02 REF-UPIN PIC X(6).
           02 REF-CDNUM PIC X(7).
           02 REF-NAME.
             03 REF-TITLE4 PIC XXXX.
             03 FILLER PIC X(20).
           02 REF-KP PIC X(7).
           02 REF-FUTURE PIC XXX.
       WORKING-STORAGE SECTION.

       01 XXXPOL.
          02 XXXPOL-1 PIC XXX.
          02 XXXPOL-2 PIC X(11).
       01  NAME-TEST  PIC X(25).
       01  NAME-LAST  PIC X(24).
       01  NAME-FIRST  PIC X(24).
       01  NAME-MIDDLE PIC X(24).
       01  TEST-DATE.
           02 T-YYYY PIC XXXX.
           02 T-MM PIC XX.
           02 T-DD PIC XX.
       01  DISPLAY-DATE.
           02 DD-MM PIC XX.
           02 FILLER PIC X.
           02 DD-DD PIC XX.
           02 FILLER PIC X.
           02 DD-YYYY PIC XXXX.
       01  DOB-DATE.
           02 DD-YYYY PIC XXXX.
           02 DD-MM PIC XX.
           02 DD-DD PIC XX.
       01  DATE-TODAY PIC X(8).
       01  SAVEMASTER.
           02 S-ACTNO PIC X(32).
           02 S-GARNAME PIC X(24).
           02 S-BILLADD PIC X(22).
           02 S-STREET PIC X(22).
           02 S-CITY PIC X(18).
           02 S-STATE PIC X(2).
           02 S-ZIP PIC X(9).
           02 S-COLLT PIC X.
           02 S-PHONE PIC X(10).
           02 S-SEX PIC X.
           02 S-RELATE PIC X.
           02 S-MSTAT PIC X.
           02 S-DOB PIC X(8).
           02 S-DUNNING PIC X.
           02 S-ACCTSTAT PIC X.
           02 S-PR-MPLR PIC X(4).
           02 S-PRINS PIC XXX.
           02 S-PR-ASSIGN PIC X.
           02 S-PR-OFFICE PIC X(4).
           02 S-PR-GROUP PIC X(10).
           02 S-PRIPOL PIC X(16).
           02 S-PRNAME PIC X(24).
           02 S-PR-RELATE PIC X.
           02 S-SE-MPLR PIC X(4).
           02 S-SEINS PIC XXX.
           02 S-SE-ASSIGN PIC X.
           02 S-TRINSIND PIC X.
           02 S-TRINS PIC XXX.
           02 S-SE-GROUP PIC X(10).
           02 S-SECPOL PIC X(16).
           02 S-SENAME PIC X(24).
           02 S-SE-RELATE PIC X.
           02 S-INSPEND PIC X(7).
           02 S-LASTBILL PIC X(8).
           02 S-ASSIGNM PIC X.
           02 S-PRIVATE PIC X.
           02 S-BILLCYCLE PIC X.
           02 S-DELETE PIC X.
           02 S-FILLER PIC XXX.
           02 S-GARNO PIC X(8).
           02 S-PRGRPNAME PIC X(15).
           02 S-SEGRPNAME PIC X(15).
           02 S-NAME-KEY PIC XXX.
       01  SAVE-RPGCHARFILE PIC X(213).
       01     RIGHT-2 PIC XX JUST RIGHT.
       01     ALF-1 PIC X.
       01     XALF-1 PIC X.
       01  ALF-3 PIC XXX.
       01  ALF-4 PIC XXXX.
       01     ALF-5 PIC X(5).
       01     ALF-7 PIC X(7).
       01     ALF-7X. 
              02 ALF-7X1 PIC X(5).
              02 ALF-7X2 PIC XX.
       01  ALF-8 PIC X(8).
       01  ALF-9.
           02 ALF-9-1.
             03 ALF-9-11 PIC X.
             03 FILLER PIC X.
           02 ALF-9-2 PIC X(7).
       01     ALF-13 PIC X(13).
       01     YEARDAY.
             03 YEAR-1.
               04 YD1 PIC X.
               04 YD2 PIC X.
             03 DAY3 PIC XXX.
       01     ABC PIC X.
       01     XYZ PIC 999.
       01     RIGHT-4 PIC X(4) JUST RIGHT.
       01  ALF12 PIC X(12).
       01  ALF-6 PIC X(6).
       01   RIGHT-3 PIC XXX JUST RIGHT.
       01     RIGHT-5 PIC X(5) JUST RIGHT.
       01     RIGHT-7 PIC X(7) JUST RIGHT.
       01     RIGHT-8 PIC X(8) JUST RIGHT.
       01     A PIC 99.
       01     B PIC 99.
       01     C PIC 99.
       01     D PIC 99.
       01     X PIC 99.
       01     Y PIC 99.
       01  Z PIC 999.
       01     FLAG   PIC 9.
       01  FLAGX PIC 9.
       01  ENDFLAG PIC 9 VALUE 0.
       01  DATE-X PIC X(8).
       01  IN-FIELD-8 PIC X(8).
       01  IN-FIELD PIC X(15).
       01  IN-FIELD-6 PIC X(6).
       01  NAME1.
           02 NAME11 PIC X.
           02 NAME12 PIC X(23).
       01  HOLDGARNO PIC X(8).
       01  NAMETAB01.
           02 NAMETAB PIC X(24) OCCURS 90 TIMES.
       01  NAMEKEYTAB01.
           02 NAMEKEYTAB PIC XXX OCCURS 90 TIMES.
       01  NUMKEYTAB01.
           02 NUMKEYTAB PIC X(4) OCCURS 90 TIMES.
       01  ALF-1X PIC X.
       01  ALF-2 PIC XX.
       01  REFFLAG PIC 9.
       01  X-IP PIC X(8).
       01  X-LASTNAME PIC X(16).
       01  X-FIRSTNAME PIC X(9).
       01  X-SUBNAME PIC X(30).
       01  X-SSN PIC X(9).
       01  X-RELATE PIC XX.
       01  Y-RELATE PIC XX.
       01  RELATECODE PIC X.
       01  GARRELATE PIC X.
       01  POLTEST.
           02 POLTEST1.
             03 POLTEST1-1 PIC X.
             03 POLTEST1-2 PIC X.
           02 FILLER PIC X(7).
           02 POLTEST2 PIC XX.
           02 FILLER PIC XXX.
       01  ALF4 PIC X(4).
       01  ALF6 PIC X(6).
       01  ALF8TEST.
           02 ALF8TEST1 PIC XX.
           02 FILLER PIC X(6).
       01  ALF8 PIC X(8).
       01  ALF8X PIC X(8).

       01  MONTH-TABLE-CONS.
           05  FILLER PIC X(24) VALUE "312931303130313130313031".
       01  MONTH-TABLE REDEFINES MONTH-TABLE-CONS.
           05  DAYS-IN-MONTH OCCURS 12 TIMES PIC 99.
       01  SERVDATE PIC X(8).
       01  REF PIC XXX.
       01  IOPAT PIC X.
       01  PLANNUM PIC 9.
       01  TAB1601.
           02 TAB16 PIC X OCCURS 16 TIMES.
       01  NEWTAB01.
           02 NEWTAB PIC X OCCURS 16 TIMES.
       01  YNDX USAGE IS INDEX.
       01  IN-FIELD-1 PIC X.
       01  IN-FIELD-3 PIC XXX.
       01  IN-FIELD-4 PIC X(4).
       01  IN-FIELD-15 PIC X(15).
       01  IN-FIELD-14 PIC X(14).
       01  X-MEDREC.
              02 X-MEDREC0 PIC XX VALUE "00".
              02 X-MEDREC1 PIC XX.
              02 X-MEDREC2 PIC XX.
              02 X-MEDREC3 PIC XX.
       01  ALF5 PIC X(5).
       01  alf7 pic x(7).
       01  ALF-14.
           02 ALF-14-1.
              03 ALF-14-11 PIC X.
              03 ALF-14-12 PIC X.
              03 ALF-14-13 PIC X.
           02 ALF-14-2 PIC X(9).
           02 ALF-14-3 PIC XX.
       01  ALF14X.
           02 ALF14X-12 PIC X(12).
           02 ALF14X-2 PIC XX.
       01  ZIPCODE.
           02 ZIPCODE-1-5 PIC X(5).
           02 ZIPCODE-6 PIC X.
           02 ZIPCODE-7-10 PIC X(4).
       01  ANS PIC XXX.
       01  ALFATAB01.
           02 ALFATAB PIC X OCCURS 8 TIMES.
       01  DIAGTAB01.
           02 DIAGTAB PIC X OCCURS 7 TIMES.
       01  DIAG-ARRAY01.
           02 DIAG-ARRAY PIC X(7) OCCURS 4 TIMES.
       01  CNTR PIC 99.
       01  ALF1 PIC X.
       01  CNTR-X PIC 9.
       01  CNTR-Y PIC 9.
       01  NUM4 PIC 9(4).
       01  NUM6 PIC 9(6).
       01  ALF22 PIC X(22).
       01  ALF22X PIC X(22).
       01  INSTAB01.
           02 INSTAB PIC X(12) OCCURS 50 TIMES.
       01  INSCMS01.
           02 INSCMS PIC XXX OCCURS 50 TIMES.
       01  TALLYX PIC 9.
       01  TALLYRT PIC 9.
       01  TALLYlT PIC 9.
       01  TALLYRIT PIC 9.
       01  TALLYLIT PIC 9.
       01  TALLYR PIC 9.
       01  TALLYL PIC 9.
       01  PRIM-DOB PIC X(8).
       01  SECDOB PIC X(8).

       PROCEDURE DIVISION.
       0005-START.
           OPEN OUTPUT NEWINS
           CLOSE NEWINS.
           OPEN I-O NEWINS RPGACTFILE RPGCHARFILE RPGINSFILE.
           OPEN INPUT RPGPROCFILE REFPHY INSFILE
                      NPIFILE FILEIN DIAGFILE.
           OPEN OUTPUT FILEOUT FILEOUT2.
           ACCEPT DATE-TODAY  FROM CENTURY-DATE.

       P1. 
           PERFORM INIT-1.
           
           READ FILEIN
             AT END
               GO TO P99
           END-READ

           IF FI-PROC1 = "76499" GO TO P1.

           IF FI-PROC1 = "73620" MOVE "73630" TO FI-PROC1.

           IF FI-PROC1 = "73070" MOVE "73060" TO FI-PROC1.

           STRING FI-PATNAMEL ";" FI-PATNAMEF DELIMITED BY "  " INTO
               RPG-GARNAME
           MOVE RPG-GARNAME(1:3) TO NAME-KEY


           MOVE SPACE TO RPG-DOB TEST-DATE
           UNSTRING FI-PAT-DOB DELIMITED BY "/" INTO T-MM T-DD T-YYYY

           MOVE SPACE TO RIGHT-2
           UNSTRING T-MM DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING ALL " " BY "0"
           MOVE RIGHT-2 TO T-MM

           MOVE SPACE TO RIGHT-2
           UNSTRING T-DD DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING ALL " " BY "0"
           MOVE RIGHT-2 TO T-DD
           MOVE TEST-DATE TO RPG-DOB

           MOVE SPACE TO RPG-ACTNO
           STRING RPG-GARNAME RPG-DOB DELIMITED BY SIZE INTO RPG-ACTNO.
           MOVE FI-PAT-STR1 TO RPG-BILLADD
           MOVE FI-PAT-STR2 TO RPG-STREET
           MOVE FI-PAT-CITY TO RPG-CITY
           MOVE FI-PAT-STATE TO RPG-STATE
           MOVE FI-PAT-ZIP TO RPG-ZIP
           
           IF RPG-ZIP(1:5) = " "
               MOVE SPACE TO ALF5
               STRING "0" RPG-ZIP(1:4) DELIMITED BY SIZE INTO ALF5
               MOVE SPACE TO RPG-ZIP
               MOVE ALF5 TO RPG-ZIP
           END-IF

           MOVE "M" TO RPG-SEX
           
           IF FI-PAT-SEX = "F"
               MOVE "F" TO RPG-SEX
           END-IF

           MOVE "2" TO RPG-RELATE
           
           IF RPG-SEX = "F"
               MOVE "K" TO RPG-RELATE
           END-IF    
           
           IF FI-SEC-SUBRELATE = SPACE
               MOVE FI-PRIM-SUBRELATE TO FI-SEC-SUBRELATE
           END-IF

           IF FI-SEC-SUBSEX = SPACE
               MOVE FI-PRIM-SUBSEX TO FI-SEC-SUBSEX
           END-IF

           IF FI-PRIM-ALFA = SPACE 
               MOVE "0" TO FI-PRIM-ALFA
           END-IF
           
           IF FI-SEC-ALFA = SPACE 
               MOVE "0" TO FI-SEC-ALFA
           END-IF

           STRING FI-PRIM-NAMEL ";" FI-PRIM-NAMEF 
               DELIMITED BY "  " INTO RPG-PRNAME
           
           IF FI-PRIM-SUBRELATE = "C"
               MOVE "4" TO RPG-RELATE
               IF RPG-SEX = "F"
                   MOVE "M" TO RPG-RELATE
               END-IF
           END-IF

           IF FI-PRIM-GRP = "0" MOVE SPACE TO FI-PRIM-GRP.

           IF FI-SEC-GRP = "0" MOVE SPACE TO FI-SEC-GRP.
           
           MOVE FI-PRIM-GRP TO RPG-PR-GROUP
           MOVE SPACE TO TAB1601
           MOVE FI-PRIM-POL TO RPG-PRIPOL

           MOVE RPG-RELATE TO RPG-PR-RELATE
           MOVE RPG-GARNAME TO RPG-PRNAME
           GO TO SEC-1.
           
           MOVE "2" TO RPG-PR-RELATE
           
           IF FI-PRIM-SUBRELATE = " " MOVE RPG-RELATE TO RPG-PR-RELATE.
           
           IF FI-PRIM-SUBRELATE = "P" AND RPG-SEX = "M" 
           MOVE "K" TO RPG-PR-RELATE.

       SEC-1.    
           IF FI-SEC-ALFA = SPACE 
               MOVE "0" TO FI-SEC-ALFA
           END-IF
           
           STRING FI-SEC-NAMEL ";" FI-SEC-NAMEF 
               DELIMITED BY "  " INTO RPG-SENAME
           
           IF FI-SEC-SUBRELATE = "C"
               MOVE "4" TO RPG-RELATE
               IF RPG-SEX = "F"
                   MOVE "M" TO RPG-RELATE
               END-IF
           END-IF

           IF RPG-SENAME = ";"
               MOVE RPG-GARNAME TO RPG-SENAME
           END-IF
           
           MOVE FI-SEC-GRP TO RPG-SE-GROUP
           MOVE FI-SEC-POL TO RPG-SECPOL
           
           IF RPG-GARNAME = RPG-SENAME
               MOVE RPG-RELATE TO RPG-SE-RELATE
               GO TO RPGINS-1
           END-IF

           MOVE "2" TO RPG-SE-RELATE

           IF FI-SEC-SUBRELATE = " " MOVE RPG-RELATE TO RPG-SE-RELATE.
           
           IF FI-SEC-SUBRELATE = "P" AND RPG-SEX = "M" 
               MOVE "K" TO RPG-SE-RELATE
           END-IF.    

       RPGINS-1.
           MOVE SPACE TO RPGINS-KEY
           MOVE FI-PRIM-ALFA TO RPGINS-KEY
           READ RPGINSFILE
             INVALID
               DISPLAY FI-PATNAMEL " " FI-PATNAMEF " "
                   FI-PRIM-ALFA "  IS INVALID "
               DISPLAY "FIX IT AND RESTART PROGRAM"
               ACCEPT ANS
               GO TO P99
           END-READ 

           MOVE RPGINS-CMS TO INS-KEY
           READ INSFILE
             INVALID
               DISPLAY FI-PATNAMEL " " FI-PATNAMEF " "
                   FI-PRIM-ALFA "  " RPGINS-KEY " FOR CMS IS INVALID"
               DISPLAY "FIX IT AND RESTART PROGRAM"
               ACCEPT ANS
               GO TO P99
           END-READ 

           MOVE INS-KEY TO RPG-PRINS
           MOVE INS-CLAIMTYPE TO RPG-PAPER
           MOVE INS-ASSIGN TO RPG-PR-ASSIGN
           MOVE INS-ASSIGN TO RPG-ASSIGN
           MOVE INS-NEIC-ASSIGN TO RPG-NEIC-ASSIGN.

           MOVE SPACE TO RPGINS-KEY
           MOVE FI-SEC-ALFA TO RPGINS-KEY
           READ RPGINSFILE
             INVALID
               DISPLAY FI-PATNAMEL " " FI-PATNAMEF " "
                   FI-SEC-ALFA "  IS INVALID "
               DISPLAY "FIX IT AND RESTART PROGRAM"
               ACCEPT ANS
               GO TO P99
           END-READ 

           MOVE RPGINS-CMS TO INS-KEY
           READ INSFILE
             INVALID
               DISPLAY FI-PATNAMEL " " FI-PATNAMEF " "
                   FI-SEC-ALFA "  " RPGINS-KEY " FOR CMS IS INVALID"
               DISPLAY "FIX IT AND RESTART PROGRAM"
               ACCEPT ANS
               GO TO P99
           END-READ
           
           MOVE INS-KEY TO RPG-SEINS
           MOVE INS-ASSIGN TO RPG-SE-ASSIGN
           
           IF FI-PRIM-ALFA = "1" OR "26" OR "84" 
               PERFORM OLD-1 THRU OLD-1-EXIT
           END-IF.
                    
       P1-1.
           IF RPG-PRINS = "268"
              MOVE RPG-PRIPOL TO POLTEST
              IF (POLTEST1-1 = "R" AND POLTEST1-2 IS NUMERIC)
                  MOVE "006" TO RPG-PRINS RPG-PAYCODE
                  MOVE "E" TO RPG-PAPER
              END-IF
           END-IF

           IF RPG-PRINS = "001"
               MOVE SPACE TO RPG-PRNAME
               MOVE "U" TO RPG-PR-ASSIGN
               MOVE SPACE TO RPG-PR-RELATE
           END-IF

           IF RPG-SEINS = "001"
               MOVE SPACE TO RPG-SENAME
               MOVE "U" TO RPG-SE-ASSIGN
               MOVE SPACE TO RPG-SE-RELATE
           END-IF

           MOVE RPGACTFILE01 TO SAVEMASTER
           READ RPGACTFILE 
             INVALID
               MOVE SAVEMASTER TO RPGACTFILE01
               WRITE RPGACTFILE01 
               END-WRITE
             NOT INVALID
               MOVE RPG-GARNO TO ALF8
               MOVE SAVEMASTER TO RPGACTFILE01
               MOVE ALF8 TO RPG-GARNO
               REWRITE RPGACTFILE01
               END-REWRITE
           END-READ

           MOVE RPGACTFILE01 TO NEWINS01
           WRITE NEWINS01
             INVALID
               REWRITE NEWINS01
               END-REWRITE
           END-WRITE

           MOVE RPG-GARNAME TO RPG-NAME
           MOVE RPG-PRINS TO RPG-PAYCODE
           MOVE SPACE TO RPG-DATE-T TEST-DATE
           UNSTRING FI-DATE-T DELIMITED BY "/" INTO T-MM T-DD T-YYYY
           
           IF T-YYYY(3:2) = SPACE
               MOVE T-YYYY(1:2) TO T-YYYY(3:2)
               MOVE "20" TO T-YYYY(1:2)
           END-IF

           MOVE SPACE TO RIGHT-2
           UNSTRING T-MM DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING ALL " " BY "0"
           MOVE RIGHT-2 TO T-MM

           MOVE SPACE TO RIGHT-2
           UNSTRING T-DD DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING ALL " " BY "0"
           MOVE RIGHT-2 TO T-DD
           MOVE TEST-DATE TO RPG-DATE-T

           MOVE SPACE TO RPG-DAT1 TEST-DATE
           
           IF T-YYYY(3:2) = SPACE
               MOVE T-YYYY(1:2) TO T-YYYY(3:2)
               MOVE "20" TO T-YYYY(1:2)
           END-IF
           
           UNSTRING FI-DAT1 DELIMITED BY "/" INTO T-MM T-DD T-YYYY
           MOVE SPACE TO RIGHT-2
           UNSTRING T-MM DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING ALL " " BY "0"
           MOVE RIGHT-2 TO T-MM

           MOVE SPACE TO RIGHT-2
           UNSTRING T-DD DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING ALL " " BY "0"
           MOVE RIGHT-2 TO T-DD
           MOVE TEST-DATE TO RPG-DAT1
           
           IF RPG-DAT1 NOT NUMERIC
               MOVE "00000000" TO RPG-DAT1
           END-IF

           MOVE FI-PROC TO ALF-7X
           MOVE "26" TO  ALF-7X2
           MOVE ALF-7X TO RPGPROC-KEY1
           MOVE SPACE TO RPGPROC-KEY2
           READ RPGPROCFILE
             INVALID 
               DISPLAY "BAD PROC " FI-PROC
               MOVE SPACE TO FILEOUT201
               STRING RPG-GARNAME " " FI-PROC " " FI-DATE-T " BAD PROC"
               DELIMITED BY SIZE INTO FILEOUT201
               WRITE FILEOUT01
               MOVE SPACE TO RPG-NT1
               MOVE 0 TO RPGPROC-AMOUNT
           END-READ     
           
           COMPUTE NUM6 = (100 * RPGPROC-AMOUNT)
           MOVE NUM6 TO RPG-AMOUNT
           MOVE RPG-NT1 TO RPG-PROC1
           MOVE ALF-7X TO RPG-PROC2
           MOVE "5" TO RPG-SERVICE.

       DIAG-XX.
           MOVE SPACES TO DIAG-ARRAY01
           MOVE FI-DX1 TO ALFATAB01
           PERFORM DIAG-1

           MOVE DIAGTAB01 TO DIAG-ARRAY(1)
           MOVE FI-DX2 TO ALFATAB01
           PERFORM DIAG-1

           MOVE DIAGTAB01 TO DIAG-ARRAY(2)
           MOVE FI-DX3 TO ALFATAB01
           PERFORM DIAG-1

           MOVE DIAGTAB01 TO DIAG-ARRAY(3)
           MOVE FI-DX4 TO ALFATAB01
           PERFORM DIAG-1

           MOVE DIAGTAB01 TO DIAG-ARRAY(4)
           MOVE 0 TO CNTR-X
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 4              
             MOVE DIAG-ARRAY(X) TO DIAG-KEY
             READ DIAGFILE
               INVALID
                 MOVE "0000000" TO DIAG-ARRAY(X)
               NOT INVALID
                 ADD 1 TO CNTR-X
                 MOVE DIAG-KEY TO DIAG-ARRAY(CNTR-X)              
             END-READ
           END-PERFORM

           ADD 1 TO CNTR-X GIVING CNTR-Y
           
           PERFORM VARYING X FROM CNTR-Y BY 1 UNTIL X > 4
               MOVE "0000000" TO DIAG-ARRAY(X)
           END-PERFORM

           MOVE SPACE TO RPG-MOD2
           MOVE 0 TO TALLYRIT TALLYLIT TALLYRT TALLYLT TALLYR TALLYL
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > CNTR-X
               IF DIAG-ARRAY(X) NOT = "0000000"
                   MOVE DIAG-ARRAY(X) TO DIAG-KEY
                   READ DIAGFILE
                     INVALID
                       CONTINUE
                     NOT INVALID  
                       INSPECT DIAG-TITLE TALLYING TALLYRIT
                           FOR ALL " RIGHT "
                       INSPECT DIAG-TITLE TALLYING TALLYLIT
                           FOR ALL " LEFT "
                       INSPECT DIAG-TITLE TALLYING TALLYRT
                           FOR ALL " RT "
                       INSPECT DIAG-TITLE TALLYING TALLYLT
                           FOR ALL " LT "
                       INSPECT DIAG-TITLE TALLYING TALLYR
                           FOR ALL " R "
                       INSPECT DIAG-TITLE TALLYING TALLYL
                           FOR ALL " L "
                       COMPUTE TALLYX = TALLYRIT +
                                        TALLYLIT +
                                        TALLYRT  +
                                        TALLYLT  + 
                                        TALLYR   +
                                        TALLYL
                       IF TALLYX NOT = 0
                           IF TALLYRIT > 0 OR
                              TALLYRT  > 0 OR
                              TALLYR   > 0
                               MOVE "RT" TO RPG-MOD2
                           ELSE
                               MOVE "LT" TO RPG-MOD2
                           END-IF
                       END-IF
                   END-READ
               END-IF
           END-PERFORM
           
           IF ((RPG-DIAG > "79999" AND < "90000") OR
               (RPG-DX2 > "79999" AND < "90000")  OR
               (RPG-DX3 > "79999" AND < "90000")) AND
               (RPG-DAT1 = ZEROES)
               MOVE RPG-DATE-T TO RPG-DAT1
           END-IF


           MOVE RPG-DOB TO RPG-GARNO.
           MOVE 0 TO REFFLAG FLAG.
           MOVE DIAG-ARRAY(1) TO RPG-DIAG
           MOVE DIAG-ARRAY(2) TO RPG-DX2
           MOVE DIAG-ARRAY(3) TO RPG-DX3
           MOVE DIAG-ARRAY(4) TO RPG-DX4.

       4A-REF.
           MOVE FI-PROVNPI TO NPI-KEY
           READ NPIFILE
             INVALID
               DISPLAY FI-PATNAMEL " " FI-PATNAMEF " " FI-PROVNPI
                   " BAD NPI"
               ACCEPT OMITTED
               WRITE FILEOUT201 FROM FILEIN01
               GO TO P1
           END-READ
           
           MOVE NPI-REFKEY TO RPG-DOCR
           MOVE NPI-PLACE TO RPG-PLACE.

       P2.
           IF RPG-PROC1 = "1402"
               MOVE "1405" TO RPG-PROC1
           END-IF
               
           IF RPG-PROC = "72110"
               MOVE SPACE TO RPG-MOD2
           END-IF    
           
           IF RPG-PROC = "72100"
               MOVE SPACE TO RPG-MOD2
           END-IF    

           MOVE RPG-ACTNO TO RPG-KEY8
           MOVE 0 TO XYZ
           MOVE RPGCHARFILE01 TO SAVE-RPGCHARFILE.

       ADD-X.
           ADD 1 TO XYZ
           MOVE XYZ TO RPG-KEY3
           READ RPGCHARFILE
             INVALID
               MOVE SAVE-RPGCHARFILE TO RPGCHARFILE01
               MOVE XYZ TO RPG-KEY3
               WRITE RPGCHARFILE01
               GO TO P1
           END-READ

           GO TO ADD-X.

       OLD-1.
           IF FI-PRIM-ALFA = "1" AND 
              FI-PRIM-NAME = SPACE
               MOVE "001" TO RPG-PRINS RPG-SEINS RPG-PAYCODE
               MOVE "U" TO RPG-PR-ASSIGN RPG-ASSIGN RPG-NEIC-ASSIGN
               MOVE SPACE TO RPG-PRNAME RPG-PR-RELATE
               MOVE SPACE TO RPG-SENAME RPG-SE-RELATE RPG-SE-RELATE
               GO TO OLD-1-EXIT
           END-IF
           
           DISPLAY FI-PRIM-NAME(1:20) " " FI-PRIM-STR1(1:20)
               " " FI-PRIM-CITY " " FI-PRIM-STATE " " FI-PRIM-ZIP

           MOVE 0 TO CNTR ENDFLAG
           DISPLAY "BEGIN A SEARCH BY:"
           DISPLAY "                  1 = NAME"
           DISPLAY "                  2 = CITY"
           DISPLAY "                  3 = STATE"
           DISPLAY "                  9 = ADD A NEW CODE"
           ACCEPT ALF1.
           
           IF ALF1 = "9"
               MOVE 1 TO NUM4
               PERFORM ADD-1 THRU ADD-1-EXIT
               GO TO OLD-1-EXIT
           END-IF
           
           IF ALF1 = "1"
               DISPLAY "ENTER A NAME"
               ACCEPT RPGINS-TITLE
               START RPGINSFILE KEY NOT < RPGINS-TITLE
                 INVALID
                   GO TO OLD-1
               END-START
               GO TO RPGINS-2
           END-IF

           IF ALF1 = "2"
               DISPLAY "ENTER A CITY"
               ACCEPT RPGINS-CITY
               START RPGINSFILE KEY NOT < RPGINS-CITY
                 INVALID
                   GO TO OLD-1
               END-START
               GO TO RPGINS-2
           END-IF

           IF ALF1 = "3"
              DISPLAY "ENTER A STATE"
              ACCEPT RPGINS-STATE
              START RPGINSFILE KEY NOT < RPGINS-STATE
                     INVALID GO TO OLD-1
              END-START
            GO TO RPGINS-2
           END-IF
           
           DISPLAY "BAD SELECTION CATEGORY"
           GO TO OLD-1.

       RPGINS-2.
           READ RPGINSFILE NEXT
             AT END
               MOVE 1 TO ENDFLAG
               GO TO RPGINS-2-1
           END-READ

           ADD 1 TO CNTR
           MOVE RPGINS-KEY TO INSTAB(CNTR)
           MOVE RPGINS-CMS TO INSCMS(CNTR)
           DISPLAY CNTR " " RPGINS-TITLE(1:20) " " RPGINS-BOX(1:20)
               " " RPGINS-CITY " " RPGINS-STATE FI-PRIM-ZIP
               
           IF CNTR < 9 AND
              ENDFLAG NOT = 1
               GO TO RPGINS-2
           END-IF.

       RPGINS-2-1.
            MOVE 0 TO CNTR
            DISPLAY " "
            DISPLAY FI-PRIM-NAME(1:20) " " FI-PRIM-STR1(1:20)
               " " FI-PRIM-CITY " " FI-PRIM-STATE " " FI-PRIM-ZIP.
       RPGINS-2-2.
            ACCEPT ANS
            IF ANS = "?"
             DISPLAY "BK = SELECT A DIFFERENT SEARCH CATEGORY"
             DISPLAY  "ENTER TO CONTINUE WITH THIS CATEGORY"
             DISPLAY " OR A NUMBER FROM 1-15 FOR YOUR SELECTION"
             GO TO RPGINS-2-2
            END-IF
            IF ANS = "BK" 
             GO TO OLD-1
            END-IF
            IF ENDFLAG = 1 AND ANS = SPACE
               GO TO OLD-1
            END-IF
            IF ANS = SPACE
              GO TO RPGINS-2
            END-IF
           MOVE SPACE TO RIGHT-2
           UNSTRING ANS DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING ALL " " BY "0"
            IF RIGHT-2 NOT NUMERIC OR RIGHT-2 = "00"
             DISPLAY " BAD ANSWER TRY AGAIN"
             GO TO RPGINS-2-2
            END-IF
            
           MOVE RIGHT-2 TO X
           MOVE INSTAB(X) TO RPG-PR-GROUP
           MOVE "P" TO RPG-PAPER
           MOVE "A" TO RPG-PR-ASSIGN RPG-ASSIGN RPG-NEIC-ASSIGN
           IF RPG-PRNAME = SPACE
             MOVE RPG-GARNAME TO RPG-PRNAME
             MOVE RPG-RELATE TO RPG-PR-RELATE
           END-IF
           IF FI-PRIM-ALFA = "1" MOVE "076" TO RPGINS-CMS
              MOVE "076" TO RPG-PRINS RPG-PAYCODE
           END-IF
           IF FI-PRIM-ALFA = "26" MOVE "091" TO RPGINS-CMS
              MOVE "091" TO RPG-PRINS RPG-PAYCODE
           END-IF
           IF FI-PRIM-ALFA = "84" MOVE "091" TO RPGINS-CMS
              MOVE "091" TO RPG-PRINS RPG-PAYCODE
           END-IF.

       OLD-1-EXIT.
           EXIT.

       ADD-1.
           MOVE NUM4 TO ALF4
           INSPECT ALF4 REPLACING LEADING "0" BY " "
           MOVE SPACE TO RPGINS-KEY
           STRING FI-PRIM-NAME(1:5) ALF4 DELIMITED BY SIZE
             INTO RPGINS-KEY 
           MOVE RPGINS-KEY TO ALF12
           READ RPGINSFILE INVALID GO TO ADD-2.
           ADD 1 TO NUM4
           GO TO ADD-1.
       ADD-2. 
           MOVE ALF12 TO RPGINS-KEY
           MOVE FI-PRIM-NAME TO RPGINS-TITLE
           MOVE FI-PRIM-STR1 TO RPGINS-BOX
           MOVE SPACE  TO RPGINS-STREET
           MOVE FI-PRIM-CITY TO RPGINS-CITY
           MOVE FI-PRIM-STATE TO RPGINS-STATE
           MOVE FI-PRIM-ZIP TO RPGINS-ZIP
           MOVE SPACE TO RPGINS-PHONE
           MOVE SPACE TO RPGINS-GAP
           MOVE SPACE TO RPGINS-FUTURE
           IF RPGINS-ZIP(1:5) = " "
            MOVE SPACE TO ALF5
            STRING "0" RPGINS-ZIP(1:4) DELIMITED BY SIZE INTO ALF5
            MOVE SPACE TO RPGINS-ZIP
            MOVE ALF5 TO RPGINS-ZIP
           END-IF
           IF FI-PRIM-ALFA = "1" MOVE "076" TO RPGINS-CMS.
           IF FI-PRIM-ALFA = "26" MOVE "091" TO RPGINS-CMS.
           IF FI-PRIM-ALFA = "84" MOVE "091" TO RPGINS-CMS.
           WRITE RPGINSFILE01.
           MOVE "A" TO RPG-ASSIGN
           MOVE "A" TO RPG-NEIC-ASSIGN
           MOVE "A" TO RPG-PR-ASSIGN
           MOVE "P" TO RPG-PAPER

           MOVE RPGINS-CMS TO RPG-PRINS
           MOVE RPGINS-CMS TO RPG-PAYCODE
           MOVE RPGINS-KEY TO RPG-PR-GROUP.

       ADD-1-EXIT.
           EXIT.

       INIT-1.
           MOVE SPACE TO RPGACTFILE01 RPGCHARFILE01
           MOVE ZEROES TO RPG-LASTBILL RPG-INSPEND 
           MOVE "1" TO RPG-DUNNING RPG-ACCTSTAT
           MOVE "0" TO RPG-COLLT RPG-TRINSIND
           MOVE "001" TO RPG-SEINS RPG-TRINS RPG-PAYCODE
           MOVE "U" TO RPG-SE-ASSIGN
           MOVE "06" TO RPG-DOCP
           MOVE "1" TO RPG-RESULT 
           MOVE "01" TO RPG-WORK 
           MOVE "4" TO RPG-ACT
           MOVE "2" TO RPG-EPSDT 
           MOVE "2" TO RPG-SORCREF 
           MOVE "0" TO RPG-AUTH
           MOVE "0" TO RPG-STAT
           MOVE "0" TO RPG-COLLECT
           MOVE SPACE TO RPG-ACC-TYPE

           IF FI-PRIM-ALFA = "26" MOVE "2" TO RPG-ACC-TYPE.

           IF FI-PRIM-ALFA = "84" MOVE "1" TO RPG-ACC-TYPE.

           MOVE SPACE TO RPG-FUTURE
           MOVE SPACE TO RPG-PATID RPG-CLAIM
           MOVE ZEROES TO RPG-DAT1 RPG-DATE-A RPG-DATE-M
           MOVE SPACE TO RPG-MOD3 RPG-MOD4
           MOVE DATE-TODAY TO RPG-DATE-E
           MOVE RPG-GARNAME TO RPG-NAME.

       DIAG-1.
           MOVE 0 TO CNTR
           PERFORM A1 VARYING X FROM 1 BY 1 UNTIL X > 8.
           
       A1. 
           IF ALFATAB(X) NOT = "."
           ADD 1 TO CNTR 
           MOVE ALFATAB(X) TO DIAGTAB(CNTR)
           IF DIAGTAB(CNTR) = SPACE
           MOVE " " TO DIAGTAB(CNTR)
           END-IF.
       ALREADY-1.    
           DISPLAY S-ACTNO
           DISPLAY RPG-BILLADD " " S-BILLADD 
           DISPLAY RPG-STREET " " S-STREET
           DISPLAY RPG-CITY " " S-CITY 
           DISPLAY RPG-STATE " " S-STATE 
           DISPLAY RPG-PRINS RPG-PRIPOL " " S-PRINS S-PRIPOL
           DISPLAY RPG-SEINS RPG-SECPOL " " S-SEINS S-SECPOL.
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
           MOVE IN-FIELD TO ALF-1.
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

       INS-4.
           READ INSFILE NEXT AT END DISPLAY "END OF FILE"
           GO TO INS-1-EXIT.
           DISPLAY INS-KEY " " INS-NAME " " INS-STREET 
           " " INS-CITY " " INS-STATE " " INS-CLAIMTYPE 
           ADD 1 TO X
           IF X > 8 MOVE 0 TO X DISPLAY "BY " ALF-8 ACCEPT ANS
           IF ANS NOT = SPACE GO TO INS-1-EXIT.
           GO TO INS-4.

       INS-1-END.
           DISPLAY "END OF FILE".

       INS-1-EXIT.
           EXIT.

       P99.
           CLOSE NEWINS RPGACTFILE RPGCHARFILE RPGPROCFILE RPGINSFILE
               REFPHY INSFILE FILEOUT FILEOUT2 NPIFILE DIAGFILE.
           DISPLAY "RPG XRAY DATA ENTRY PROGRAM HAS ENDED".
           STOP RUN.
