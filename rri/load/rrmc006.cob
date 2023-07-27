      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrmc006.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ACTFILE ASSIGN TO       "S30" ORGANIZATION INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS A-ACTNO
               ALTERNATE RECORD KEY IS A-GARNO WITH DUPLICATES
               ALTERNATE RECORD KEY IS NAME-KEY WITH DUPLICATES.

           SELECT ORDFILE ASSIGN TO       "S35" ORGANIZATION INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS ORDNO
               ALTERNATE RECORD KEY IS C-DATE-E WITH DUPLICATES.
           
           SELECT REFPHY ASSIGN TO        "S40" ORGANIZATION INDEXED
               ACCESS IS DYNAMIC RECORD KEY IS REF-KEY
               ALTERNATE RECORD KEY IS REF-BSNUM  WITH DUPLICATES
               ALTERNATE RECORD KEY IS REF-CRNUM WITH DUPLICATES
               ALTERNATE RECORD KEY IS REF-UPIN  WITH DUPLICATES
               ALTERNATE RECORD KEY IS REF-CDNUM WITH DUPLICATES
               ALTERNATE RECORD KEY IS REF-NAME  WITH DUPLICATES.
           
           SELECT FILEIN ASSIGN TO        "S60" ORGANIZATION 
               LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO       "S65" ORGANIZATION 
               LINE SEQUENTIAL.

           SELECT MOBLFILE ASSIGN TO      "S70" ORGANIZATION INDEXED
               ACCESS IS RANDOM RECORD KEY IS MOBL-KEY.

           SELECT INSFILE ASSIGN TO       "S75" ORGANIZATION INDEXED
               ACCESS IS DYNAMIC RECORD KEY IS INS-KEY
               ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES.

           SELECT HOSPFILE ASSIGN TO      "S80" ORGANIZATION INDEXED
               ACCESS IS DYNAMIC RECORD KEY IS HOSP-KEY
               ALTERNATE RECORD KEY IS H-INS-KEY WITH DUPLICATES
               ALTERNATE RECORD KEY IS H-INS-NAME WITH DUPLICATES.

           SELECT COMPFILE ASSIGN TO      "S85" ORGANIZATION INDEXED
               ACCESS MODE DYNAMIC RECORD KEY IS COMP-KEY
               LOCK MODE MANUAL.

           SELECT EMAILAUTHFILE ASSIGN TO "S90" ORGANIZATION INDEXED
               ACCESS MODE DYNAMIC RECORD KEY IS EA-KEY
               ALTERNATE RECORD KEY IS EA-MEDREC WITH DUPLICATES
               ALTERNATE RECORD KEY IS EA-NAME WITH DUPLICATES
               ALTERNATE RECORD KEY IS EA-EMAIL WITH DUPLICATES
               ALTERNATE RECORD KEY IS EA-AUTH WITH DUPLICATES
               ALTERNATE RECORD KEY IS EA-DATE-E WITH DUPLICATES
               ALTERNATE RECORD KEY IS EA-SSN WITH DUPLICATES
               LOCK MODE MANUAL.

           SELECT ERRFILE ASSIGN TO       "S95" ORGANIZATION
               LINE SEQUENTIAL.

           SELECT GARFILE ASSIGN TO "S100" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES.
            
       DATA DIVISION.
       FILE SECTION.
       FD  ERRFILE.
       01  ERRFILE01 PIC X(120).

       FD  EMAILAUTHFILE.
           copy emailauthfile.cpy in "c:\users\sid\cms\copylib\rri".           

       FD  COMPFILE.
       01  COMPFILE01.
           02 COMP-KEY.
             03 COMP-MEDREC PIC X(8).
             03 COMP-DATE PIC X(8).
             03 COMP-PROC PIC X(4).
           02 COMP-INSNAME1 PIC X(25).
           02 COMP-INSCONTACT1 PIC X(25).
           02 COMP-INSADDR11 PIC X(20).
           02 COMP-INSADDR21 PIC X(15).
           02 COMP-INSCITY1 PIC X(20).
           02 COMP-INSSTATE1 PIC XX.
           02 COMP-INSZIP1 PIC X(10).
           02 COMP-INSPHONE1 PIC X(12).

       FD  INSFILE.
           copy insfile.cpy in "c:\users\sid\cms\copylib".

       FD  MOBLFILE.
       01  MOBLFILE01.
           02 MOBL-KEY PIC X(4).
           02 MOBL-RRI PIC X(4).
           02 MOBL-CPT PIC X(7).
           02 MOBL-TITLE PIC X(28).

       FD  HOSPFILE
           DATA RECORD IS HOSPFILE01.
       01  HOSPFILE01.
           02 HOSP-KEY PIC X(5).
           02 H-INS-KEY PIC XXX.
           02 H-INS-NAME PIC X(18).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).

       FD  FILEIN.
       01  FILEIN01.
           02 FI-1 PIC XX.
           02 FI-2 PIC X(1068).

       FD  REFPHY
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
           02 REF-NPI PIC X(10).

       FD  ORDFILE.
           copy "ordfile.cpy" in "c:\Users\sid\cms\copylib\rri".

       FD  ACTFILE
           DATA RECORD IS ACTFILE01.
       01  ACTFILE01.
           02 A-ACTNO PIC X(8).
           02 A-GARNAME PIC X(24).
           02 A-BILLADD PIC X(22).
           02 A-STREET PIC X(22).
           02 A-CITY PIC X(18).
           02 A-STATE PIC X(2).
           02 A-ZIP PIC X(9).
           02 A-COLLT PIC X.
           02 A-PHONE.
             03 A-PHONE1 PIC XXX.
             03 A-PHONE2 PIC XXX.
             03 A-PHONE3 PIC X(4).
           02 A-SEX PIC X.
           02 A-RELATE PIC X.
           02 A-MSTAT PIC X.
           02 A-DOB PIC X(8).
           02 A-DUNNING PIC X.
           02 A-ACCTSTAT PIC X.
           02 A-PR-MPLR PIC X(4).
           02 A-PRINS PIC XXX.
           02 A-PR-ASSIGN PIC X.
           02 A-PR-OFFICE PIC X(4).
           02 A-PR-GROUP.
              03 A-PR-GROUP2 PIC XX.
              03 FILLER PIC X(8).
           02 A-PRIPOL.
             03 A-PRIPOL1 PIC X(9).
             03 A-PRIPOL2 PIC XXX.
             03 A-PR-FILLER PIC X(4).
           02 A-PRNAME PIC X(24).
           02 A-PR-RELATE PIC X.
           02 A-SE-MPLR PIC X(4).
           02 A-SEINS PIC XXX.
           02 A-SE-ASSIGN PIC X.
           02 A-TRINSIND PIC X.
           02 A-TRINS PIC XXX.
           02 A-SE-GROUP. 
              03 A-SE-GROUP2 PIC XX.
              03 FILLER PIC X(8).
           02 A-SECPOL.
             03 A-SECPOL1 PIC X(9).
             03 A-SECPOL2 PIC XXX.
             03 A-SE-FILLER PIC X(4).
           02 A-SENAME PIC X(24).
           02 A-SE-RELATE PIC X.
           02 A-INSPEND PIC X(7).
           02 A-LASTBILL PIC X(8).
           02 A-ASSIGNM PIC X.
           02 A-PRIVATE PIC X.
           02 A-BILLCYCLE PIC X.
           02 A-DELETE PIC X.
           02 A-FILLER PIC XXX.
           02 A-GARNO PIC X(8).
           02 A-PRGRPNAME PIC X(15).
           02 A-SEGRPNAME PIC X(15).
           02 NAME-KEY PIC XXX.

       FD  GARFILE.
           COPY garfile.CPY IN "C:\Users\sid\cms\copylib\rri".    

       WORKING-STORAGE SECTION.

       01  REC101.
           02 R1-1 PIC XX.
           02 R1-PATNUM PIC X(7).
           02 R1-PATNAME.
             03 R1-PATNAME-L PIC X(19).
             03 R1-PATNAME-F PIC X(12).
           02 R1-PATADDR1 PIC X(25).
           02 R1-PATADDR2 PIC X(25).
           02 R1-PATCITY PIC X(25).
           02 R1-PATSTATE PIC XX.
           02 R1-PATZIP PIC X(10).
             02 R1-ADMIT. 
              03 R1-ADMITMM PIC XX.
              03 FILLER PIC X.
              03 R1-ADMITDD PIC XX.
              03 FILLER PIC X.
              03 R1-ADMITYY PIC XXXX.
           02 R1-ADMITTIME PIC X(5).
           02 R1-WORKCOMP PIC X.
           02 R1-GARNAME.
             03 R1-GARNAME1.
               05 R1-GARNAME1-L PIC X(21).
               05 R1-GARNAME1-F PIC X(14).
           02 R1-GARADDR1 PIC X(25).
           02 R1-GARADDR2 PIC X(25).
           02 R1-GARCITY PIC X(25).
           02 R1-GARSTATE PIC XX.
           02 R1-GARZIP PIC X(10).
           02 R1-EMAIL PIC X(30).
           02 R1-IP1 PIC X(5).
           02 R1-ID1 PIC X(30).
           02 R1-CERT11 PIC X(20).
           02 R1-GRP1 PIC X(20).
           02 R1-GRPNAME11 PIC X(30).
           02 R1-SUBNAME11 PIC X(35).
           02 R1-EMPLOYNAME11 PIC X(30).
           02 R1-GENDER11 PIC X.
           02 FILLER PIC X.
           02 R1-DOB11 PIC X(10).
      * 478
           02 R1-SSN11 PIC X(9).
           02 R1-RELATE1 PIC XX.
           02 INSURANCE-1.
            03 R1-INSNAME1 PIC X(25).
            03 R1-INSCONTACT1 PIC X(25).
            03 R1-INSADDR11 PIC X(20).
            03 R1-INSADDR21 PIC X(15).
            03 R1-INSCITY1 PIC X(20).
            03 R1-INSSTATE1 PIC XX.
            03 R1-INSZIP1 PIC X(10).
            03 R1-INSPHONE1 PIC X(12).
           02  R1-AUTH PIC X(20).
      * 638
           02 R1-IP2 PIC X(5).
           02 R1-ID2 PIC X(30).
           02 R1-CERT22 PIC X(20).
           02 R1-GRP2 PIC X(20).
           02 R1-GRPNAME22 PIC X(30).
           02 R1-SUBNAME22 PIC X(35).
           02 R1-EMPLOYNAME22 PIC X(30).
           02 R1-GENDER22 PIC X.
           02 FILLER PIC X.
           02 R1-DOB22 PIC X(10).
           02 R1-SSN22 PIC X(9).
           02 R1-RELATE2 PIC XX.
      * 365
           02 INSURANCE-2.
            03 R1-INSNAME2 PIC X(25).
            03 R1-INSCONTACT2 PIC X(25).
            03 R1-INSADDR12 PIC X(20).
            03 R1-INSADDR22 PIC X(15).
            03 R1-INSCITY2 PIC X(20).
            03 R1-INSSTATE2 PIC XX.
            03 R1-INSZIP2 PIC X(10).
            03 R1-INSPHONE2 PIC X(12).
      *    02 R1-IO PIC X(4).
      *    02 FILLER PIC X(2).
      * 135


       01  REC201.
           02 R2-1 PIC XX.
           02 R2-ACC.
             03 R2-ACCMM PIC XX.
             03 R2-ACCDD PIC XX.
             03 R2-ACCYY PIC XX.
             03 R2-ACCHHMM PIC X(5).
      * 13       
           02 R2-REFDOC.
             03 R2-REFDOC4 PIC XXXX.
             03 R2-REFDOC22 PIC X(18).
      * 35       
           02 R2-DIAG PIC X(130).
      * 165
           02 R2-IP3 PIC X(5).
           02 R2-ID3 PIC X(30).
           02 R2-CERT33 PIC X(20).
           02 R2-GRP3 PIC X(20).
           02 R2-GRPNAME33 PIC X(30).
           02 R2-SUBNAME33 PIC X(35).
           02 R2-EMPLOYNAME33 PIC X(30).
           02 R2-GENDER33 PIC X.
           02 FILLER PIC X.
           02 R2-DOB33 PIC X(10).
           02 R2-SSN33 PIC X(9).
           02 R2-RELATE3 PIC XX.
      * 358     
           02 INSURANCE-3.
            03  R2-INSNAME3 PIC X(25).
            03  R2-INSCONTACT3 PIC X(25).
            03  R2-INSADDR13 PIC X(20).
            03  R2-INSADDR23 PIC X(15).
            03  R2-INSCITY3 PIC X(20).
            03  R2-INSSTATE3 PIC XX.
            03  R2-INSZIP3 PIC X(10).
            03  R2-INSPHONE3 PIC X(12).
           02 FILLER PIC X(20).

           02 R2-IP4 PIC X(5).
           02 R2-ID4 PIC X(30).
           02 R2-CERT44 PIC X(20).
           02 R2-GRP4 PIC X(20).
           02 R2-GRPNAME44 PIC X(30).
           02 R2-SUBNAME44 PIC X(35).
           02 R2-EMPLOYNAME44 PIC X(30).
           02 R2-GENDER44 PIC X.
           02 FILLER PIC X.
           02 R2-DOB44 PIC X(10).
           02 R2-SSN44 PIC X(9).
           02 R2-RELATE4 PIC XX.
           02 INSURANCE-4.
            03  R2-INSNAME4 PIC X(25).
            03  R2-INSCONTACT4 PIC X(25).
            03  R2-INSADDR14 PIC X(20).
            03  R2-INSADDR24 PIC X(15).
            03  R2-INSCITY4 PIC X(20).
            03  R2-INSSTATE4 PIC XX.
            03  R2-INSZIP4 PIC X(10).
            03  R2-INSPHONE4 PIC X(12).
            02 FILLER PIC X(20).
           02 R2-MEDREC.
             03 R2-MEDREC1 PIC XX.
             03 FILLER PIC X.
             03 R2-MEDREC2 PIC XX.
             03 FILLER PIC X.
             03 R2-MEDREC3 PIC XX.
           02 R2-EMPNAME PIC X(30).
           02 R2-EMPLOYER-ADDR1 PIC X(25).
           02 R2-EMPLOYER-ADDR2 PIC X(25).
           02 R2-EMPLOYER-CITY PIC X(25).
           02 R2-EMPLOYER-STATE PIC XX.
           02 R2-EMPLOYER-ZIP PIC X(10).
           02 R2-GUARSSN PIC X(9).
           02 R2-PHONE.
             03 R2-PHONE1 PIC XXX.
             03 FILLER PIC X.
             03 R2-PHONE2 PIC XXX.
             03 FILLER PIC X.
             03 R2-PHONE3 PIC X(4).
           02 R2-DOC PIC X(8).
           02 R2-REFIND PIC X.
      * col 1005     
           02 R2-NPI PIC X(10).
           02 R2-ADMIT-DATE-DUPLICATE PIC X(6).
           02 R2-PATSEX PIC X.
           02 R2-PATDOB.
             03 R2-DOBMM PIC XX.
             03 R2-DOBDD PIC XX.
             03 R2-DOBYY PIC XX.
           02 R2-DISCHARGE PIC X(6).
      *     02 FILLER PIC X(7).
       01  REC301.
           02 R3-1 PIC XX.
           02 R3-IND PIC XXX.
           02 R3-DEPT PIC XX.
           02 R3-GLC PIC X.
           02 R3-PROC.
             03 R3-PROC1 PIC X.
             03 FILLER PIC XXX.
           02 R3-DATE. 
              03 R3-DATEMM PIC XX.
              03 R3-DATEDD PIC XX.
              03 R3-DATEYY PIC XX.
           02 R3-UNIT PIC XXX.
      * col 22     
           02 R3-CLINICAL PIC X(40).
           02 FILLER PIC X(24).
      * col 86     
           02 R3-PLACE PIC X(4).
           02 R3-DOCP PIC X(4).
           02 R3-DOCPFILLER PIC X(18).
      * col 112     
           02 R3-CPT PIC X(5).
           02 FILLER PIC X(3).
      * col 120     
           02 R3-HCPCS PIC X(5).
           02 FILLER PIC X(3).
      * col 128     
           02 R3-MOD1 PIC X(2).
           02 FILLER PIC X.
      * col 131     
           02 R3-MOD2 PIC X(2).
           02 FILLER PIC X.
      * col 134
           02 R3-MOD3 PIC X(2).
           02 FILLER PIC X(5).
      * col 141     
           02 R3-OBSERV PIC X(5).
           02 FILLER PIC X(35).
      * col 181     
           02 R3-LOCO PIC X(4).
           02 FILLER PIC X(36).
           02 R3-ACCESSION PIC X(7).

       01  NAME-TEST PIC X(25).
       01  NAME-LAST PIC X(24).
       01  NAME-FIRST          PIC X(24).
       01  NAME-MIDDLE         PIC X(24).
       01  TDATE.
           02 TCC PIC XX.
           02 TYY PIC XX.
           02 TMM PIC XX.
           02 TDD PIC XX.
       01  INPUT-DATE.
           02 T-MM PIC 99.
           02 T-DD PIC 99.
           02 T-CC PIC 99.
           02 T-YY PIC 99.
       01  TEST-DATE.
           02 T-CC PIC 99.
           02 T-YY PIC 99.
           02 T-MM PIC 99.
           02 T-DD PIC 99.
       01  A-DATE.
           05 A-MM  PIC 99.
           05 A-DD  PIC 99.
           05 A-CC  PIC 99.
           05 A-YY  PIC 99.
       01  B-DATE.
           05 B-CC  PIC 99.
           05 B-YY  PIC 99.
           05 B-MM  PIC 99.
           05 B-DD  PIC 99.
       01  OLD PIC 999.
       01  DATE-TODAY PIC X(8).
       01  SAVEMASTER.
           02 S-ACTNO PIC X(8).
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
       01     RIGHT-2 PIC XX JUST RIGHT.
       01     ALF-1 PIC X.
       01     XALF-1 PIC X.
       01     ALF-3 PIC XXX.
       01     ALF-4 PIC XXXX.
       01     ALF-5 PIC X(5).
       01     ALF-7 PIC X(7).
       01     ALF-8 PIC X(8).
       01     ALF-13 PIC X(13).
       01     YEARDAY.
             03 YEAR-1.
               04 YD1 PIC X.
               04 YD2 PIC X.
             03 DAY3 PIC XXX.
       01     ABC PIC X.
       01     XYZ PIC 999.
       01     RIGHT-4 PIC X(4) JUST RIGHT.
       01     ALF-6 PIC X(6).
       01     RIGHT-3 PIC XXX JUST RIGHT.
       01     RIGHT-5 PIC X(5) JUST RIGHT.
       01     RIGHT-7 PIC X(7) JUST RIGHT.
       01     A PIC 99.
       01     B PIC 99.
       01     C PIC 99.
       01     D PIC 99.
       01     X PIC 99.
       01     Y PIC 99.
       01  Z PIC 999.
       01  FLAG PIC 9.
       01  FLAGX PIC 9.
       01  DATE-X PIC X(8).
       01  IN-FIELD-8 PIC X(8).
       01  IN-FIELD-6 PIC X(6).
       01  SAVEORD PIC X(211).
       01  NAME1.
           02 NAME11 PIC X.
           02 NAME12 PIC X(23).
       01  HOLDGARNO PIC X(8).
       01  NAMETAB01.
           02 NAMETAB PIC X(30) OCCURS 90 TIMES.
       01  NAMEKEYTAB01.
           02 NAMEKEYTAB PIC XXX OCCURS 90 TIMES.
       01  NUMKEYTAB01.
           02 NUMKEYTAB PIC X(4) OCCURS 90 TIMES.
       01  IPLAN PIC XXXX.
       01  XPLAN PIC XXX.
       01  CITY1 PIC X(24).
       01  CITY2 PIC X(24).
       01  CITY3 PIC X(24).
       01  CITY4 PIC X(24).
       01  ALF-1X PIC X.
       01  ALF-2 PIC XX.
       01  REFFLAG PIC 9.
       01  LNAME PIC X(20).
       01  FNAME PIC X(20).
       01  X-IP PIC X(8).
       01  X-CERT PIC X(16).
       01  X-GRP PIC X(10).
       01  X-GRPNAME PIC X(15).
       01  X-LASTNAME PIC X(16).
       01  X-FIRSTNAME PIC X(9).
       01  X-SUBNAME PIC X(30).
       01  X-SSN PIC X(9).
       01  X-RELATE PIC XX.
       01  X-GENDER PIC X.
       01  Y-RELATE PIC XX.
       01  RELATECODE PIC X.
       01  GARRELATE PIC X.
       01  POLTEST.
           02 POLTEST1.
             03 POLTEST1-1 PIC X.
             03 POLTEST1-2 PIC X.
           02 FILLER PIC X(7).
           02 POLTEST2 PIC XX.
           02 FILLER PIC X(5).
       01  ACTNO.
           02 ACTNO2 PIC XX.
           02 ACTNO6 PIC X(6).
       01  ALF4 PIC X(4).
       01  ALF6 PIC X(6).
       01  ALF8TEST.
           02 ALF8TEST1 PIC XX.
           02 FILLER PIC X(6).
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
       01  IN-FIELD-16 PIC X(16).
       01  X-MEDREC.
              02 X-MEDREC0 PIC XX VALUE "00".
              02 X-MEDREC1 PIC XX.
              02 X-MEDREC2 PIC XX.
              02 X-MEDREC3 PIC XX.
       01  ALF-16.
           02 ALF-16-1.
              03 ALF-16-11 PIC X.
              03 ALF-16-12 PIC X.
              03 ALF-16-13 PIC X.
           02 ALF-16-2 PIC X(9).
           02 ALF-16-3 PIC X(4).
       01  ALF-BCBSVT.
           02 ALF-BCBSVT-1 PIC X(4).
           02 ALF-BCBSVT-2 PIC X(9).
           02 ALF-BCBSVT-3 PIC X(3).      
       01  ZIPCODE.
           02 ZIPCODE-1-5 PIC X(5).
           02 ZIPCODE-6 PIC X.
           02 ZIPCODE-7-10 PIC X(4).
       01  NUM2 PIC 99.
       01  ANS PIC X.
       01  LASTNAME PIC X(22).
       01  FIRSTNAME PIC X(22).
       01  ALFATAB1RE.
           02 FILLER PIC X(36) VALUE
           "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".
       01  ALFATAB101 REDEFINES ALFATAB1RE.
           02 ALFATAB1 PIC X OCCURS 36 TIMES.
       01  ALFATAB2RE.
           02 FILLER PIC X(36) VALUE
           "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".
       01  ALFATAB201 REDEFINES ALFATAB2RE.
           02 ALFATAB2 PIC X OCCURS 36 TIMES.
       01  TEST-KEY PIC XXX.
       01  TEST-NAME.
           02 TN1 PIC X.
           02 TN14 PIC X(23).

       01  AUTH-FLAG PIC X.     

       01  PRIOR-INS PIC X(3).

       PROCEDURE DIVISION.
       0005-START.
           OPEN I-O ACTFILE EMAILAUTHFILE ORDFILE COMPFILE.
           OPEN INPUT HOSPFILE REFPHY INSFILE FILEIN MOBLFILE GARFILE.
           OPEN OUTPUT FILEOUT ERRFILE.

       10-ACTION.
           DISPLAY "REPORT DATE, MMDDYYYY".
           ACCEPT IN-FIELD-8.

           IF IN-FIELD-8 NOT NUMERIC 
               DISPLAY "NOT NUMERIC"
               GO TO 10-ACTION
           END-IF     

           MOVE IN-FIELD-8 TO INPUT-DATE
           
           IF T-MM OF INPUT-DATE < 01 OR > 12
               DISPLAY "BAD MONTH"
               GO TO 10-ACTION
           END-IF

           IF T-DD OF INPUT-DATE > DAYS-IN-MONTH(T-MM OF INPUT-DATE)
               DISPLAY "DATE MUST BE NUMERIC"
               GO TO 10-ACTION
           END-IF

           MOVE CORR INPUT-DATE TO TEST-DATE
           MOVE TEST-DATE TO DATE-X.
           ACCEPT DATE-TODAY FROM CENTURY-DATE.

           PERFORM INIT-1.

       P1.
           READ FILEIN AT END
               GO TO 9100CMF
           END-READ.  

       P1-1.
           IF FI-1 NOT = "##"
               DISPLAY "FIRST RECORD NOT A ## ? " FILEIN01
               ACCEPT OMITTED
               GO TO P1
           END-IF

           MOVE FILEIN01 TO REC101.

           READ FILEIN
             AT END
               DISPLAY "BAD ENDING"
               ACCEPT OMITTED
               GO TO 9100CMF
           END-READ

           IF FI-1 = "##" OR "$$"
               DISPLAY "BAD GUAR REC." 
               DISPLAY FILEIN01
               ACCEPT OMITTED
               GO TO 9100CMF
           END-IF

           MOVE FILEIN01 TO REC201.

           MOVE R2-MEDREC1 TO X-MEDREC1
           MOVE R2-MEDREC2 TO X-MEDREC2
           MOVE R2-MEDREC3 TO X-MEDREC3
           MOVE X-MEDREC TO R2-MEDREC
           
           IF R2-MEDREC = "00000000"
               DISPLAY "MRN IS ZEROES FOR " R1-PATNAME
               ACCEPT OMITTED
           END-IF

      *    WILL SEE IF PRIOR INS IS BETTER THAN MISC INS 
      *    LATER IN REPLACE-2
           MOVE R2-MEDREC TO A-ACTNO
           READ ACTFILE
             INVALID
               MOVE SPACE TO G-GARNO
               MOVE "095" TO PRIOR-INS
             NOT INVALID
               MOVE A-GARNO TO G-GARNO
               READ GARFILE
                 INVALID
                   MOVE "095" TO PRIOR-INS
                 NOT INVALID 
                   MOVE G-PRINS TO PRIOR-INS
               END-READ   
           END-READ

           MOVE SPACE TO A-GARNAME LNAME FNAME
           UNSTRING R1-PATNAME DELIMITED BY ", " INTO LNAME FNAME
           STRING LNAME ";" FNAME DELIMITED BY "  " INTO A-GARNAME

           MOVE R1-GARZIP TO ZIPCODE
           
           IF ZIPCODE-6 = "-"
               MOVE SPACE TO ZIPCODE-6 ZIPCODE-7-10
           END-IF

           MOVE ZIPCODE TO A-ZIP.
           
           IF R1-GARSTATE NOT = SPACE
               MOVE R1-GARCITY TO A-CITY
               MOVE R1-GARSTATE TO A-STATE
               GO TO Q1
           END-IF

           MOVE SPACE TO CITY1 CITY2 CITY3 CITY4
           UNSTRING R1-GARCITY DELIMITED BY " " INTO CITY1
               CITY2 CITY3 CITY4.
           
           IF (CITY2 = SPACE) AND (CITY3 = SPACE) AND (CITY4 = SPACE)
               MOVE "VT" TO A-STATE
               MOVE CITY1 TO A-CITY
               GO TO Q1
           END-IF

           IF CITY4 NOT = SPACE
               MOVE CITY4 TO A-STATE
               STRING CITY1 "%" CITY2 "%" CITY3 DELIMITED BY " " INTO
                   A-CITY
               INSPECT A-CITY REPLACING ALL "%" BY " "
               GO TO Q1
           END-IF

           IF CITY3 NOT = SPACE
               MOVE CITY3 TO A-STATE
               STRING CITY1 "%" CITY2 DELIMITED BY " " INTO A-CITY
               INSPECT A-CITY REPLACING ALL "%" BY " "
               GO TO Q1
           END-IF

           MOVE CITY1 TO A-CITY
           MOVE CITY2 TO A-STATE.

       Q1.
           IF A-STATE = "T "
               MOVE "VT" TO A-STATE
               MOVE SPACE TO CITY1 CITY2
               UNSTRING A-CITY DELIMITED BY " VT " INTO CITY1 CITY2
               MOVE CITY1 TO A-CITY
           END-IF

           MOVE R2-PHONE1 TO A-PHONE1
           MOVE R2-PHONE2 TO A-PHONE2
           MOVE R2-PHONE3 TO A-PHONE3
           MOVE R1-GARADDR1 TO A-BILLADD
           MOVE R1-GARADDR2 TO A-STREET
           
           IF R1-GARADDR1 = SPACE AND
              R1-GARADDR2 = SPACE AND
              A-CITY = SPACE

               MOVE R1-PATADDR1 TO A-BILLADD
               MOVE R1-PATADDR2 TO A-STREET
               MOVE R1-PATCITY TO A-CITY
               MOVE R1-PATSTATE TO A-STATE
               MOVE R1-PATZIP TO A-ZIP
           END-IF

           IF A-CITY = SPACE OR A-STATE = space or a-zip = space
             MOVE SPACE TO ERRFILE01
             STRING A-GARNAME " HAS A BAD ADDRESS, MEDREC # " R2-MEDREC
               DELIMITED BY SIZE INTO ERRFILE01
             WRITE ERRFILE01
             display "bad address, will be listed, any key to proceed"
             display a-garname " " r2-medrec  
             accept omitted.             

           INSPECT A-BILLADD REPLACING ALL "*" BY " "
           INSPECT A-STREET REPLACING ALL "*" BY " "
           MOVE "00000000" TO A-DOB
           MOVE "20" TO TCC
           MOVE R2-DOBYY TO TYY
           MOVE R2-DOBMM TO TMM
           MOVE R2-DOBDD TO TDD
           
           IF DATE-TODAY < TDATE
               MOVE "19" TO TCC
           END-IF

           MOVE TDATE TO A-DOB
           MOVE SPACE TO C-DATE-A
           STRING "20" R2-ACCYY R2-ACCMM R2-ACCDD DELIMITED BY "!@#"
               INTO C-DATE-A.
           
           IF C-DATE-A = "20      "
               MOVE "00000000" TO C-DATE-A
           END-IF

           IF DATE-TODAY < C-DATE-A
               STRING "19" R2-ACCYY R2-ACCMM R2-ACCDD DELIMITED BY "!@#"
               INTO C-DATE-A
           END-IF
               
           IF C-DATE-A NOT NUMERIC
               DISPLAY C-DATE-A  " BAD DATE"
              MOVE "00000000" TO C-DATE-A
           END-IF
              
           MOVE SPACE TO A-GARNO.

           PERFORM NPI-METHOD THRU NPI-EXIT.
           
           IF FLAG = 1
             MOVE REF-KEY TO REF
             GO TO P2
           END-IF

           IF (R2-REFDOC NOT = SPACE) AND (R2-NPI(10:1) NOT = SPACE)
             DISPLAY "WOULD YOU LIKE TO ADD " R2-REFDOC  " NPI " R2-NPI
               " AUTOMATICALLY? Type Y for YES."
             ACCEPT ANS
             IF ANS = "Y"
               PERFORM P62                 
             END-IF  
           END-IF
 
           IF FLAG = 1
             MOVE REF-KEY TO REF
             GO TO P2
           END-IF.

       P2-0.     
           DISPLAY "ENTER 3 CHAR REFPHY CODE FOR PROVIDER " R2-REFDOC
           DISPLAY "AFTER ADDING to DB WITH RRI-62"
           DISPLAY " PT " R1-PATNAME " ADDR " R1-GARCITY " " R1-GARSTATE
           ACCEPT REF
           MOVE REF TO REF-KEY
           READ REFPHY
             INVALID
               GO TO P2-0
           END-READ

           GO TO P2.

       4Z-REF.
           MOVE R2-REFDOC4 TO ALF4
           MOVE 0 TO Y
           MOVE SPACE TO REF-KEY
           MOVE ALF4(1:1) TO REF-KEY
           START REFPHY KEY NOT < REF-KEY
             INVALID
               GO TO REF-2
           END-START.

       4B-REF.
           READ REFPHY NEXT
             AT END
               GO TO REF-2
           END-READ

           IF REF-NPI = SPACE
               GO TO 4B-REF
           END-IF    
           
           IF REF-NAME(1:1) > REF-KEY(1:1)
               GO TO REF-2
           END-IF    
           
           IF REF-NAME(1:4) NOT = ALF4
               GO TO 4B-REF
           END-IF    
           
           ADD 1 TO Y
           MOVE R2-DOC TO NUMKEYTAB(Y)
           MOVE REF-NAME TO NAMETAB(Y)
           MOVE REF-KEY TO NAMEKEYTAB(Y)
           GO TO 4B-REF.

       REF-2.
           IF Y = 0 GO TO P2-0.

       REF-3.
           PERFORM REF-PICK VARYING X FROM 1 BY 1 UNTIL X > Y.
           DISPLAY R2-REFDOC " " R2-DOC
           ACCEPT ALF-3
           MOVE ALF-3 TO REF-KEY
           READ REFPHY
             INVALID
               MOVE SPACE TO RIGHT-2
               UNSTRING ALF-3(1:2) DELIMITED BY " " INTO RIGHT-2
               INSPECT RIGHT-2 REPLACING ALL " " BY "0"
               MOVE RIGHT-2 TO ALF-2
               IF( ALF-2 NOT NUMERIC) OR (ALF-2 = "00")
                   DISPLAY "BAD PICK"
                   GO TO REF-3
               END-IF

               MOVE ALF-2 TO X
               IF X = 0 OR X > Y
                   DISPLAY "BAD CHOICE. DO IT AGAIN!"
                   GO TO REF-3
               END-IF

               MOVE NAMEKEYTAB(X) TO REF

             NOT INVALID
               CONTINUE
           END-READ

           GO TO P2.

       REF-PICK.
           DISPLAY X " " NAMETAB(X) " " NAMEKEYTAB(X)
                   " " NUMKEYTAB(X).

       NPI-METHOD.
           MOVE SPACE TO REF-KEY
           MOVE R2-REFDOC(1:1) TO REF-KEY
           MOVE 0 TO FLAG.
           START REFPHY KEY NOT < REF-KEY
             INVALID
               GO TO NPI-EXIT
           END-START.

       NPI-1.
           READ REFPHY NEXT
             AT END
               GO TO NPI-EXIT
           END-READ    
           
           IF REF-NPI = SPACE
               GO TO NPI-1
           END-IF    
           
           IF REF-NAME(1:1) > R2-REFDOC(1:1)
               GO TO NPI-EXIT
           END-IF    
           
           IF REF-NPI = R2-NPI
               MOVE 1 TO FLAG
               GO TO NPI-EXIT
           END-IF
           
           GO TO NPI-1.

       NPI-EXIT.
           EXIT.

       P2.
           IF (R2-DOBYY NOT NUMERIC) OR (R2-DOBMM NOT NUMERIC)
               OR (R2-DOBDD NOT NUMERIC)
               DISPLAY R2-PATDOB " " R1-PATNAME 
               MOVE "000000" TO R2-PATDOB
           END-IF

           MOVE 20 TO A-CC
           MOVE R2-DOBYY TO A-YY
           MOVE R2-DOBMM TO A-MM
           MOVE R2-DOBDD TO A-DD
           MOVE A-DATE TO INPUT-DATE
           MOVE CORR INPUT-DATE TO TEST-DATE
           
           IF DATE-TODAY < TEST-DATE
               MOVE 19 TO A-CC
           END-IF

           MOVE DATE-TODAY TO B-DATE
           COMPUTE OLD = 100 * (B-CC - A-CC) + B-YY - A-YY 
           
           IF (A-MM > B-MM) OR ((A-MM = B-MM) AND (A-DD > B-DD))
               SUBTRACT 1 FROM OLD.
           
           IF A-DOB < "19       " AND A-DOB > "000000000" ADD 99 TO OLD.
           
           IF R2-PATSEX NOT = "F" MOVE "M" TO R2-PATSEX.
           
           MOVE R2-PATSEX TO A-SEX.
           
           IF OLD > 23 MOVE "2" TO A-RELATE ELSE MOVE "4" TO A-RELATE.
           
           IF A-SEX = "F" AND A-RELATE = "4" MOVE "M" TO A-RELATE.
           
           IF A-SEX = "F" AND A-RELATE = "2" MOVE "K" TO A-RELATE.

           MOVE "001" TO A-PRINS A-SEINS A-TRINS
           MOVE SPACE TO A-PRIPOL A-PR-GROUP A-PRNAME A-PR-RELATE
                         A-PR-MPLR A-PRGRPNAME
           MOVE SPACE TO A-SECPOL A-SE-GROUP A-SENAME A-SE-RELATE
                         A-SE-MPLR A-SEGRPNAME
           MOVE "U" TO A-PR-ASSIGN A-SE-ASSIGN
           
           MOVE R1-IP1 TO X-IP
           MOVE R1-CERT11 TO X-CERT
           MOVE R1-GRP1 TO X-GRP
           MOVE R1-GRPNAME11 TO X-GRPNAME
           MOVE R1-SSN11 TO X-SSN
           MOVE R1-SUBNAME11 TO X-SUBNAME
           MOVE R1-RELATE1 TO X-RELATE
           MOVE R1-GENDER11 TO X-GENDER
           MOVE 1 TO PLANNUM
           PERFORM SEL-PRINS THRU SEL-PRINS-EXIT
           
           IF R1-IP1 = "00433" OR "00698" OR "00699" OR "00830"
               OR "00930"
             IF G-GARNO = SPACE
               DISPLAY R1-PATNAME INSURANCE-1
               DISPLAY " IS A MISC INS FROM RRMC FOR " R2-MEDREC
               " USE 095 SINCE NO RECENT GARNO"
             ELSE
               DISPLAY R1-PATNAME INSURANCE-1
               DISPLAY " IS A MISC INS FROM RRMC FOR " R2-MEDREC 
               ", USE INS " PRIOR-INS " FROM RECENT GARNO " G-GARNO
             END-IF 
             DISPLAY "? Y FOR YES, ANYTHING ELSE TO LOOK FOR INS."
             ACCEPT ANS
             IF ANS = "Y"
               MOVE PRIOR-INS TO A-PRINS
               MOVE PRIOR-INS TO INS-KEY
               READ INSFILE
                 INVALID
                   DISPLAY "NO INS ON FILE" 
                   PERFORM REPLACE-2 THRU REPLACE-2-EXIT
               END-READ
               DISPLAY INS-NAME     
               MOVE INS-ASSIGN TO A-PR-ASSIGN
             ELSE
               PERFORM REPLACE-2 THRU REPLACE-2-EXIT  
             END-IF
           END-IF

      *     IF R1-IP1 = "00931" OR "00932" GO TO P2-3.
           
           IF FLAG = 1
               MOVE 2 TO PLANNUM
               GO TO P2-1
           END-IF               

           MOVE R1-IP2 TO X-IP
           MOVE R1-CERT22 TO X-CERT
           MOVE R1-GRP2 TO X-GRP
           MOVE R1-GRPNAME22 TO X-GRPNAME
           MOVE R1-SSN22 TO X-SSN
           MOVE R1-SUBNAME22 TO X-SUBNAME
           MOVE R1-RELATE2 TO X-RELATE
           MOVE R1-GENDER22 TO X-GENDER

           PERFORM SEL-PRINS THRU SEL-PRINS-EXIT

           IF FLAG = 1
               MOVE 3 TO PLANNUM
               GO TO P2-1
           END-IF    
           
           MOVE R2-IP3 TO X-IP
           MOVE R2-CERT33 TO X-CERT
           MOVE R2-GRP3 TO X-GRP
           MOVE R2-GRPNAME33 TO X-GRPNAME
           MOVE R2-SSN33 TO X-SSN
           MOVE R2-SUBNAME33 TO X-SUBNAME
           MOVE R2-RELATE3 TO X-RELATE
           MOVE R2-GENDER33 TO X-GENDER

           PERFORM SEL-PRINS THRU SEL-PRINS-EXIT

           IF FLAG = 1
               MOVE 4 TO PLANNUM
               GO TO P2-1
           END-IF    
           
           MOVE R2-IP4 TO X-IP
           MOVE R2-CERT44 TO X-CERT
           MOVE R2-GRP4 TO X-GRP
           MOVE R2-GRPNAME44 TO X-GRPNAME
           MOVE R2-SSN44 TO X-SSN
           MOVE R2-SUBNAME44 TO X-SUBNAME
           MOVE R2-RELATE4 TO X-RELATE
           MOVE R1-GENDER22 TO X-GENDER

           PERFORM SEL-PRINS THRU SEL-PRINS-EXIT
           
           GO TO P2-2.

       P2-1.
           IF PLANNUM = 2
               MOVE R1-IP2 TO X-IP
               MOVE R1-CERT22 TO X-CERT
               MOVE R1-GRP2 TO X-GRP
               MOVE R1-GRPNAME22 TO X-GRPNAME
               MOVE R1-SSN22 TO X-SSN
               MOVE R1-SUBNAME22 TO X-SUBNAME
               MOVE R1-GENDER22 TO X-GENDER
               MOVE R1-RELATE2 TO X-RELATE
           END-IF    

           IF PLANNUM = 3
           MOVE R2-IP3 TO X-IP
           MOVE R2-CERT33 TO X-CERT
           MOVE R2-GRP3 TO X-GRP
           MOVE R2-GRPNAME33 TO X-GRPNAME
           MOVE R2-SSN33 TO X-SSN
           MOVE R2-SUBNAME33 TO X-SUBNAME
           MOVE R2-GENDER33 TO X-GENDER
           MOVE R2-RELATE3 TO X-RELATE.

           IF PLANNUM = 4
           MOVE R2-IP4 TO X-IP
           MOVE R2-CERT44 TO X-CERT
           MOVE R2-GRP4 TO X-GRP
           MOVE R2-GRPNAME44 TO X-GRPNAME
           MOVE R2-SSN44 TO X-SSN
           MOVE R2-SUBNAME44 TO X-SUBNAME
           MOVE R2-GENDER44 TO X-GENDER
           MOVE R2-RELATE4 TO X-RELATE.
           
           PERFORM SEL-SEINS THRU SEL-SEINS-EXIT.
           
           IF X-IP = "00698   " OR "00699   "
             PERFORM REPLACE-3 THRU REPLACE-3-EXIT.
           
           GO TO P2-2.

       SEL-PRINS.
           MOVE 0 TO FLAG
           MOVE X-IP TO HOSP-KEY

           IF X-IP = SPACE
             GO TO SEL-PRINS-EXIT
           END-IF     
           
           READ HOSPFILE
             INVALID
               MOVE X-CERT TO A-PRIPOL
               MOVE X-GRP TO A-PR-GROUP
               MOVE SPACE TO FILEOUT01
               STRING "HOSP=" X-IP " " R2-MEDREC " " A-GARNAME
               " BAD HOSPITAL CODE" DELIMITED BY "?/?" INTO FILEOUT01
               DISPLAY FILEOUT01
               WRITE FILEOUT01
               GO TO SEL-PRINS-EXIT
           END-READ
    
           MOVE H-INS-KEY TO INS-KEY
           READ INSFILE
             INVALID
               DISPLAY H-INS-KEY " MISSING"
               DISPLAY HOSP-KEY " " R2-MEDREC " " A-GARNAME
               MOVE SPACE TO FILEOUT01
               STRING "HOSP=" HOSP-KEY " " H-INS-KEY " " R2-MEDREC " "
               A-GARNAME " BAD INSURANCE CODE" DELIMITED BY SIZE
               INTO FILEOUT01 WRITE FILEOUT01
               MOVE "001" TO H-INS-KEY
           END-READ    
           
           MOVE INS-ASSIGN TO A-PR-ASSIGN.
           MOVE 1 TO FLAG
           MOVE H-INS-KEY TO A-PRINS
           MOVE X-CERT   TO A-PRIPOL
           MOVE X-GRP TO A-PR-GROUP
           MOVE X-GRPNAME TO A-PRGRPNAME
           
           IF X-SUBNAME = "SELF" OR "UNK" OR SPACE OR "X" OR "XX"
               MOVE A-GARNAME TO A-PRNAME
               GO TO SEL-1
           END-IF    

           MOVE SPACE TO A-PRNAME LNAME FNAME
           UNSTRING X-SUBNAME DELIMITED BY ", " OR "," INTO LNAME FNAME
           
           IF FNAME = SPACE
               MOVE SPACE TO LNAME FNAME
               UNSTRING X-SUBNAME DELIMITED BY " " INTO FNAME LNAME
               STRING LNAME ";" FNAME
               DELIMITED BY "  " INTO A-PRNAME
           ELSE
               STRING LNAME ";" FNAME DELIMITED BY "  " INTO A-PRNAME
           END-IF.

       SEL-1.
           IF X-CERT = SPACE
               MOVE X-SSN TO A-PRIPOL
           END-IF    

           IF NOT (X-GENDER = "F" OR "M")
               DISPLAY X-SUBNAME  " GENDER F OR M"
               
               IF X-SUBNAME = SPACE
                   DISPLAY A-GARNAME
               END-IF
               
               ACCEPT X-GENDER
           END-IF

           IF X-GENDER = "F" 
               MOVE "K" TO RELATECODE
           ELSE 
               MOVE "2" TO RELATECODE
           END-IF    
           
           MOVE X-RELATE TO Y-RELATE
           
           PERFORM RELATE-1 THRU RELATE-1-EXIT
           
           MOVE RELATECODE TO A-PR-RELATE.
           
           IF A-PRINS = "091"
               MOVE R1-EMPLOYNAME11 TO A-PRGRPNAME
           END-IF.    
           
       SEL-PRINS-EXIT.
           EXIT.

       SEL-SEINS.
           MOVE 0 TO FLAG
           MOVE X-IP TO HOSP-KEY

           IF X-IP = SPACE OR X-IP = "00951   "
              DISPLAY "X-IP IS 00951 IN SEL-SEINS"
                ACCEPT OMITTED
                
               GO TO SEL-SEINS-EXIT.

           READ HOSPFILE INVALID
             MOVE X-CERT TO A-SECPOL
             MOVE X-GRP TO A-SE-GROUP
             MOVE SPACE TO FILEOUT01
             STRING "HOSP=" HOSP-KEY " " R2-MEDREC " " A-GARNAME
             " BAD HOSPITAL CODE" DELIMITED BY "?/?" INTO FILEOUT01
             DISPLAY FILEOUT01
             WRITE FILEOUT01
             GO TO SEL-SEINS-EXIT.

           MOVE H-INS-KEY TO INS-KEY
           READ INSFILE
             INVALID
               DISPLAY H-INS-KEY " MISSING"
               DISPLAY HOSP-KEY " " R2-MEDREC " " A-GARNAME
               MOVE SPACE TO FILEOUT01
               STRING "HOSP=" HOSP-KEY " " H-INS-KEY " " R2-MEDREC " "
                 A-GARNAME " BAD INSURANCE CODE" DELIMITED BY ".."
                 INTO FILEOUT01 WRITE FILEOUT01
               MOVE "001" TO H-INS-KEY.

           MOVE INS-ASSIGN TO A-SE-ASSIGN.
           MOVE 1 TO FLAG
           MOVE H-INS-KEY TO A-SEINS
           MOVE X-CERT    TO A-SECPOL
           MOVE X-GRP     TO A-SE-GROUP
           MOVE X-GRPNAME TO A-SEGRPNAME

           IF X-SUBNAME = "SELF" OR "UNK" OR SPACE OR "X" OR "XX"
             MOVE A-GARNAME TO A-SENAME
             GO TO SEL-2.

           MOVE SPACE TO A-SENAME LNAME FNAME
           UNSTRING X-SUBNAME DELIMITED BY ", " OR ","
             INTO LNAME FNAME
    
           IF FNAME = SPACE
             MOVE SPACE TO LNAME FNAME
             UNSTRING X-SUBNAME DELIMITED BY " " INTO FNAME LNAME
             STRING LNAME ";" FNAME DELIMITED BY "  " INTO A-SENAME
           ELSE
             STRING LNAME ";" FNAME DELIMITED BY "  " INTO A-SENAME
           END-IF.

       SEL-2.
           IF X-CERT = SPACE
             MOVE X-SSN TO A-SECPOL.

           IF NOT (X-GENDER = "F" OR "M")
             DISPLAY X-SUBNAME " GENDER F OR M"
             IF X-SUBNAME = SPACE
               DISPLAY A-GARNAME
             END-IF
             ACCEPT X-GENDER 
           END-IF
           
           IF X-GENDER = "F" 
             MOVE "K" TO RELATECODE
           ELSE
             MOVE "2" TO RELATECODE.

           MOVE X-RELATE TO Y-RELATE
           PERFORM RELATE-1 THRU RELATE-1-EXIT
           MOVE RELATECODE TO A-SE-RELATE.
           
           IF X-IP = "00058   " AND A-PRINS = "003"
             MOVE "0005199     " TO A-PR-GROUP
             MOVE "062" TO A-SEINS.

           IF X-IP = "00061   " AND A-PRINS = "003"
             MOVE "0000731     " TO A-PR-GROUP
             MOVE "062" TO A-SEINS.

           IF A-SEINS = "091"
             MOVE R1-EMPLOYNAME22 TO A-SEGRPNAME.

           IF R1-IP1 = "00951"  
              STRING "HAVE TO REVINS IN GP FOR " R2-MEDREC " "
                 A-GARNAME " AND CHANGE INS IN AC" DELIMITED BY ".."
                 INTO FILEOUT01 
              WRITE FILEOUT01.

       SEL-SEINS-EXIT. 
           EXIT.

       RELATE-1.
           IF (Y-RELATE = "03" OR "04" OR "06"
             OR "07" OR "13" OR "14" OR "17")
             AND (X-GENDER = "M")
             MOVE "2" TO RELATECODE
             GO TO RELATE-1-EXIT.

           IF (Y-RELATE = "03" OR "04" OR "06"
             OR "07" OR "13" OR "14" OR "17")
             AND (X-GENDER = "F")
             MOVE "K" TO RELATECODE
             GO TO RELATE-1-EXIT.

           IF (Y-RELATE = "05")
             AND (X-GENDER = "2")
             MOVE "2" TO RELATECODE
             GO TO RELATE-1-EXIT.

           IF (Y-RELATE = "05")
             AND (X-GENDER = "F")
             MOVE "K" TO RELATECODE
             GO TO RELATE-1-EXIT.

       RELATE-1-EXIT.
           EXIT.

       REPLACE-2.
           DISPLAY R1-PATNAME INSURANCE-1
           DISPLAY "ENTER A PRIMARY INS CODE MAYBE ELECTRONIC?"
           ACCEPT A-PRINS
           MOVE A-PRINS TO INS-KEY
           READ INSFILE
             INVALID
               DISPLAY "BAD"
               GO TO REPLACE-2
           END-READ
           
           DISPLAY INS-NAME.

       REPLACE-2-EXIT.
           EXIT.
           
       REPLACE-3.
           DISPLAY R1-PATNAME

           IF A-PRINS = "003"
             DISPLAY "MEDICARE PATIENT"
           END-IF

           DISPLAY R1-PATNAME INSURANCE-2
           DISPLAY "ENTER 2NDARY INS CODE"
           ACCEPT A-SEINS
           MOVE A-SEINS TO INS-KEY
           READ INSFILE
             INVALID
               DISPLAY "BAD"
               GO TO REPLACE-3
           END-READ.

       REPLACE-3-EXIT.
           EXIT.

       INIT-1.
           MOVE SPACE TO ACTFILE01 
           MOVE "00000000" TO A-LASTBILL
           MOVE "0000000" TO A-INSPEND
           MOVE "1" TO A-DUNNING A-ACCTSTAT
           MOVE "0" TO A-COLLT A-TRINSIND
           MOVE "001" TO A-TRINS.

       P2-2.
           IF A-PRINS = "004"
             MOVE A-RELATE TO A-PR-RELATE.
           
           IF A-SEINS = "004"
             MOVE A-RELATE TO A-SE-RELATE.
           
           IF A-PRINS NOT = "245"
             SET YNDX TO 1
             MOVE A-PRIPOL TO TAB1601
             MOVE SPACE TO NEWTAB01
             PERFORM PACK-1 VARYING X FROM 1 BY 1 UNTIL X > 16
             MOVE NEWTAB01 TO A-PRIPOL
           END-IF    
           
           IF A-SEINS NOT = "245"
             SET YNDX TO 1
             MOVE A-SECPOL TO TAB1601
             MOVE SPACE TO NEWTAB01
             PERFORM PACK-1 VARYING X FROM 1 BY 1 UNTIL X > 16
             MOVE NEWTAB01 TO A-SECPOL
           END-IF

           IF (A-PRINS = "003")
             MOVE A-PRIPOL TO POLTEST
             IF ((POLTEST1 = "WA" OR "MA" OR "WD")
               OR (POLTEST1-1 = "A"))
               MOVE "028" TO A-PRINS
             END-IF
           END-IF

           IF (A-SEINS = "003")
             MOVE A-SECPOL TO POLTEST
             IF ((POLTEST1 = "WA" OR "MA" OR "WD")
               OR (POLTEST1-1 = "A"))
                 MOVE "028" TO A-SEINS
             END-IF
           END-IF
           
           IF (A-PRINS = "003") AND (A-SEINS NOT = "062")
             MOVE SPACE TO A-PR-GROUP
           END-IF

           IF A-SEINS = "003"
             MOVE SPACE TO A-SE-GROUP
           END-IF

           MOVE SPACE TO A-PR-OFFICE.
           
           IF A-PRINS = "121"
             MOVE "X100" TO A-PR-OFFICE
           END-IF

           IF (A-PRIPOL = A-PR-GROUP)
             MOVE SPACE TO A-PR-GROUP
           END-IF

           IF (A-SECPOL = A-SE-GROUP)
             MOVE SPACE TO A-SE-GROUP
           END-IF

           IF (A-PRINS = "003" AND A-SEINS = "005")
             AND (A-PRIPOL1 = A-SECPOL1)
             MOVE A-PRIPOL2 TO A-SECPOL2
           END-IF
   
           MOVE A-PRIPOL TO ALF-16

           IF (ALF-16-11 ALPHABETIC) AND (ALF-16-12 ALPHABETIC)
               AND (ALF-16-13 ALPHABETIC) AND (ALF-16-2 NUMERIC)
               AND ((A-PRINS = "025" OR "029" OR "031" OR "044" 
               OR "033" OR "075" OR "094" OR "096")
               OR (A-PRINS > "233" AND < "245"))
               MOVE "268" TO A-PRINS
               MOVE "A" TO A-PR-ASSIGN
           END-IF

      *    standard bcbsvt policy prefix
           IF (A-PRINS = "268") 
               AND ((ALF-16-11 = "Z") AND (ALF-16-12 = "I"))
               MOVE "002" TO A-PRINS
           END-IF

      *    other bcbsvt policy prefixes
           IF ((A-PRINS = "268")
               AND  ((ALF-16-1 = "FAC") OR (ALF-16-1 = "EVT")
               OR (ALF-16-1 = "FVT") OR (ALF-16-1 = "VEI")
               OR (ALF-16-1 = "FAO")))
               MOVE "002" TO A-PRINS
           END-IF

           IF (A-PRINS = "268") AND (A-SEINS = "149")
               MOVE A-SEINS TO A-PRINS
               MOVE "001" TO A-SEINS
               MOVE A-SECPOL TO A-PRIPOL
               MOVE SPACE TO A-SECPOL
               MOVE SPACE TO ERRFILE01
               STRING "SWITCHEROO FOR BCBS EMPIRE " A-GARNAME " "
                   R2-MEDREC " " A-PRIPOL 
                   DELIMITED BY SIZE INTO ERRFILE01
               WRITE ERRFILE01
           END-IF

      *    BCBS Empire
           IF (A-PRINS = "268") AND
             (
               (ALF-16-11 = "Y") AND 
               (ALF-16-12 = "L") AND
               (ALF-16-13 = "S")
             )           
             MOVE "149" TO A-PRINS
             MOVE ALF-16-2 TO A-PRIPOL
           END-IF           

      *    if not bcbsvt policy move to out of state ins code
           IF ((A-PRINS = "002")
               AND  NOT ((ALF-16-1 = "FAC") OR (ALF-16-1 = "EVT")
               OR (ALF-16-1 = "FVT") OR (ALF-16-1 = "VEI")
               OR (ALF-16-1 = "FAO") OR ALF-16-1(1:2) = "ZI"))
               MOVE "268" TO A-PRINS
           END-IF

      *    add V to bcbsvt policy prefix         
           IF (A-PRINS = "002") AND (ALF-16(4:1) NOT = "V")
               MOVE SPACE TO ERRFILE01
               STRING "ADDING V TO 002 POLICY FOR " A-GARNAME " "
                   R2-MEDREC " " A-PRIPOL 
                   DELIMITED BY SIZE INTO ERRFILE01
               WRITE ERRFILE01
               MOVE SPACE TO ALF-BCBSVT
               STRING ALF-16-1 "V" DELIMITED BY SIZE INTO ALF-BCBSVT-1
               MOVE ALF-16-2 TO ALF-BCBSVT-2
               MOVE ALF-16-3(1:3) TO ALF-BCBSVT-3
               MOVE ALF-BCBSVT TO A-PRIPOL
           END-IF
           
           IF (A-PRINS = "268")
               AND ((ALF-16-11 NOT ALPHABETIC)
               OR (ALF-16-12 NOT ALPHABETIC) 
               OR (ALF-16-13 NOT ALPHABETIC))
               MOVE "076" TO A-PRINS
               MOVE "U" TO A-PR-ASSIGN
           END-IF    

      *    for empire plan 
           IF (A-PRINS = "268")
               AND (ALF-16-1 = "YLS")
               AND (A-SEINS = "116")
               MOVE "116" TO A-PRINS
               MOVE "001" TO A-SEINS
               MOVE A-SECPOL TO A-PRIPOL
               MOVE A-SE-GROUP TO A-PR-GROUP
               MOVE SPACE TO A-SECPOL
               MOVE SPACE TO A-SE-GROUP
           END-IF
           
           IF (A-PRINS = "076") 
               AND ((ALF-16-11 ALPHABETIC)
               OR (ALF-16-12 ALPHABETIC) 
               OR (ALF-16-13 ALPHABETIC))

               MOVE "268" TO A-PRINS
               MOVE A-SECPOL TO ALF-16

               IF (ALF-16-11 ALPHABETIC) AND (ALF-16-12 ALPHABETIC)
                   AND (ALF-16-13 ALPHABETIC) 
                   AND ((A-SEINS = "025" OR "029" OR "031" OR "044"  
                   OR "033" OR "075" OR "094" OR "096")
                   OR (A-SEINS > "233" AND < "245"))           
                   MOVE "268" TO A-SEINS
               END-IF
           END-IF             

           IF (A-SEINS = "268") 
               AND ((ALF-16-11 NOT ALPHABETIC)
               OR (ALF-16-12 NOT ALPHABETIC) 
               OR (ALF-16-13 NOT ALPHABETIC))
               MOVE "U" TO A-SE-ASSIGN
           END-IF

           IF (A-SEINS = "076") 
               AND ((ALF-16-11 ALPHABETIC)
               AND  (ALF-16-12 ALPHABETIC) 
               AND  (ALF-16-13 ALPHABETIC))            
               MOVE "268" TO A-SEINS
		       END-IF

           IF A-PRINS = "076" AND A-SEINS = "076"
               MOVE "075" TO A-SEINS
           END-IF
           
           IF A-PRINS = "002" AND A-SEINS = "002"
             MOVE "023" TO A-SEINS.
           
           IF (A-PRINS = "003") AND (A-SEINS = "715") 
             MOVE "062" TO A-SEINS
             MOVE "0099001     " TO A-PR-GROUP
             MOVE "A" TO A-SE-ASSIGN.

           IF (A-PRINS = A-SEINS)
               AND (A-PR-ASSIGN = "A")
               MOVE "020" TO A-SEINS
               MOVE SPACE TO ERRFILE01
               STRING "PRI AND SEC SAME INS " A-GARNAME " "
                   R2-MEDREC " " A-PRIPOL 
                   DELIMITED BY SIZE INTO ERRFILE01
               WRITE ERRFILE01
               DISPLAY A-PRINS "  SAME AS SECONDARY INSURANCE "
                   R2-MEDREC
           END-IF.                        

       P2-3.
           MOVE R2-MEDREC TO A-ACTNO
           MOVE A-GARNAME TO NAME-KEY

           IF A-PRIPOL = "X" OR "UNK" MOVE SPACE TO A-PRIPOL.

           IF A-SECPOL = "X" OR "UNK" MOVE SPACE TO A-SECPOL.

           IF A-PR-GROUP = "X" OR "UNK" OR "NA"
                       MOVE SPACE TO A-PR-GROUP.

           IF A-SE-GROUP = "X" OR "UNK" OR "NA"
                       MOVE SPACE TO A-SE-GROUP.

           IF A-PRINS = "004"
               MOVE A-GARNAME TO A-PRNAME
               MOVE A-RELATE TO A-PR-RELATE
           END-IF

           IF A-SEINS = "004"
               MOVE A-GARNAME TO A-SENAME
               MOVE A-RELATE TO A-SE-RELATE
           END-IF

      *    since receive auth in R1 instead of R3 $$ charge record 
      *    might as well perform this somewhere in this paragraph
           PERFORM EA-1 THRU EA-1-EXIT.
           
           IF A-DOB(1:2) = "20" AND A-PRINS = "003"
               MOVE "19" TO A-DOB(1:2)
           END-IF

	       IF A-PRINS = "268" AND A-PR-ASSIGN = "U"
	         MOVE "A" TO A-PR-ASSIGN
	       END-IF

           IF A-SEINS = "268" AND A-SE-ASSIGN = "U"
	         MOVE "A" TO A-SE-ASSIGN
	       END-IF 

	       MOVE ACTFILE01 TO SAVEMASTER.
           
           READ ACTFILE
             INVALID
               MOVE SAVEMASTER TO ACTFILE01
               WRITE ACTFILE01
               GO TO B1
           END-READ

           MOVE A-GARNO TO S-GARNO  
           MOVE SAVEMASTER TO ACTFILE01
           REWRITE ACTFILE01.
           GO TO B1.
           
       B1. 
           READ FILEIN
             AT END
               GO TO 9100CMF
           END-READ

           IF FI-1 NOT = "$$"
               IF AUTH-FLAG = 1
                 WRITE EMAILAUTHFILE01
               end-if  
               GO TO P1-1
           END-IF     
           
           MOVE FILEIN01 TO REC301.                     

       B1-1.
           MOVE SPACE TO RIGHT-4
           MOVE "0001"  TO C-ORDER
           MOVE R3-DATEMM TO A-MM
           MOVE R3-DATEDD TO A-DD
           MOVE R3-DATEYY TO A-YY
           MOVE 20 TO A-CC
           MOVE A-DATE TO INPUT-DATE 
           MOVE CORR INPUT-DATE TO TEST-DATE
           MOVE CORR INPUT-DATE TO TEST-DATE
           MOVE TEST-DATE TO C-DATE-T 
           MOVE TEST-DATE TO EA-DATE-E

           MOVE DATE-X TO C-DATE-E
           MOVE SPACE TO C-DATE-ADMIT
           STRING R1-ADMITYY R1-ADMITMM R1-ADMITDD DELIMITED
             BY SIZE INTO C-DATE-ADMIT.

           MOVE "5" TO C-IOPAT
           
           IF R3-PLACE = "EMER" MOVE "E" TO C-IOPAT.
      *    maybe someday will need O for POS     
      *     IF R3-OBSERV = "OBSER" MOVE "O" TO C-IOPAT.
           
           IF R3-PLACE = "INPT" 
             MOVE "3" TO C-IOPAT
             IF C-DATE-ADMIT = SPACE
               STRING A-GARNAME " WAS SENT AS INPT WITH NO ADMIT DATE "
                 "THIS MUST BE FIXED OR CLAIM FILE WILL REJECT."
                 DELIMITED BY SIZE INTO ERRFILE01
               WRITE ERRFILE01
             end-if
           end-if

           IF R3-OBSERV = "OBSER" 
               STRING A-GARNAME " WAS UNDER OBSERVATION, USING POS OUTP"
                " FOR CPT " R3-CPT " AND DATE " R3-DATE
                 DELIMITED BY SIZE INTO ERRFILE01
               WRITE ERRFILE01
           end-if 

           IF ((REF = "A3Z"
               OR "B1T" OR "B51" OR "B7C" 
               OR "D55" OR "D3Z"
               OR "F4J" OR "F34" OR "F2S"
               OR "G0T" OR "G0A" OR "G36" OR "G4U"
               OR "H1B" OR "H27" 
               OR "J06" 
               OR "L4Q" 
               OR "M8S" 
               OR "R1D" OR "R2A" 
               OR "SAH" OR "S7O" OR "S91" OR "SAG" OR "S1O"
               OR "V12" 
               OR "W2I" 
               OR "Z0I") AND
             C-IOPAT NOT = "E")
             MOVE "E" TO C-IOPAT
             MOVE SPACE TO ERRFILE01
             STRING A-GARNAME " WAS REFFERED FROM ED BY " REF
               " CHANGING POS TO E"
               DELIMITED BY SIZE INTO ERRFILE01
               WRITE ERRFILE01
           END-IF
                      
           MOVE REF TO C-REF.
           MOVE R3-PROC TO C-PROC
           
           IF R3-IND = "CAN"
               MOVE "-" TO C-IND
           ELSE
               MOVE " " TO C-IND
           END-IF

           MOVE "00" TO C-DOCP
                                            
           IF R3-DOCP = "MITC" MOVE "06" TO C-DOCP.
                      
           IF R3-DOCP = "SHEL" MOVE "08" TO C-DOCP.
           
           IF R3-DOCP = "HUMM" MOVE "09" TO C-DOCP.
           
           IF R3-DOCP = "BOYE" MOVE "10" TO C-DOCP.
           
           MOVE R3-CLINICAL TO C-CLINICAL
           MOVE R2-DIAG TO C-ADMIT-DIAG
           MOVE R1-PATNUM TO C-VISITNO
           MOVE R3-CPT TO C-CPT
           MOVE R3-MOD1 TO C-MOD2
           MOVE R3-MOD2 TO C-MOD3
           MOVE R3-MOD3 TO C-MOD4
           STRING R3-MOD1 R3-MOD2 R3-MOD3 DELIMITED BY SIZE 
               INTO C-MOD2 

      *  new format which has hcpcs and a mod             
           IF R3-CPT = SPACE            
               MOVE R3-HCPCS TO C-CPT           
               IF R3-HCPCS = SPACE
                   MOVE SPACE TO ERRFILE01
                   STRING A-GARNAME " HAD NO CPT " R3-CPT
                        " AND NO HCPCS TO BILL WITH " R3-HCPCS 
                        ". Call somebody."
                   DELIMITED BY SIZE INTO ERRFILE01
                   WRITE ERRFILE01
                   GO TO B1
               END-IF    
           END-IF

           MOVE "001" TO ORD3
           MOVE A-ACTNO TO ORD8
           MOVE ORDFILE01 TO SAVEORD
           MOVE 1 TO XYZ.

       B2. 
           READ ORDFILE
             INVALID
               GO TO B3
           END-READ

           ADD 1 TO XYZ
           MOVE XYZ TO ORD3
           GO TO B2.

       B3. 
           MOVE SAVEORD TO ORDFILE01
           MOVE XYZ TO ORD3
           WRITE ORDFILE01
           
           IF (R1-IP1 = "00931" OR "00932")
             OR (A-PRINS = "075" OR "076" OR "095")
             PERFORM WORK-COMP.
           
           IF A-SEINS = "075" OR "076" OR "095"
             PERFORM WORK-COMP2.
           
           GO TO B1.

       WORK-COMP.
           MOVE SPACE TO COMPFILE01
           STRING A-ACTNO C-DATE-T C-PROC INSURANCE-1
           DELIMITED BY SIZE INTO COMPFILE01
           WRITE COMPFILE01
             INVALID
               CONTINUE.
           
           IF R1-INSNAME1 = SPACE
              MOVE SPACE TO ERRFILE01
              STRING A-GARNAME " BLANK WORK COMP1 ADDRESS "
                A-PRINS " " A-SEINS
              DELIMITED BY SIZE INTO ERRFILE01
              WRITE ERRFILE01
           END-IF.    

       WORK-COMP2.
           MOVE SPACE TO COMPFILE01
           STRING A-ACTNO C-DATE-T C-PROC INSURANCE-2
           DELIMITED BY SIZE INTO COMPFILE01
           WRITE COMPFILE01
             INVALID
               CONTINUE
           END-WRITE

           IF R1-INSNAME2 = SPACE
             MOVE SPACE TO COMPFILE01
             STRING A-ACTNO C-DATE-T C-PROC INSURANCE-3
             DELIMITED BY SIZE INTO COMPFILE01           
             WRITE COMPFILE01
               INVALID 
                 CONTINUE
             END-WRITE
           END-IF.        

       PACK-1.            
           IF NOT (TAB16(X) = " " OR "-" OR "/")
             MOVE TAB16(X) TO NEWTAB(YNDX)
             SET YNDX UP BY 1
           END-IF.

       EA-1.
           IF R1-EMAIL = SPACE AND 
             R1-AUTH = SPACE AND 
             R2-GUARSSN = "00000000"
             MOVE 0 TO AUTH-FLAG             
             GO TO EA-1-EXIT.          

      *    get last record in emailauthssnfile
           MOVE 999999 TO EA-KEY
           START EMAILAUTHFILE KEY < EA-KEY 
             INVALID
               DISPLAY "BAD START ON EMAILAUTHSSNFILE " EA-KEY
               ACCEPT OMITTED
               GO TO EA-1-EXIT
           END-START

           READ EMAILAUTHFILE PREVIOUS 
             AT END
               MOVE 0 TO EA-KEY
           END-READ

           MOVE A-ACTNO TO EA-MEDREC
           ADD 1 TO EA-KEY
           MOVE A-GARNAME TO EA-NAME
           MOVE R2-GUARSSN TO EA-SSN
           MOVE R1-EMAIL TO EA-EMAIL
           MOVE R1-AUTH TO EA-AUTH
           MOVE 1 TO AUTH-FLAG.

       EA-1-EXIT.
           EXIT.

       EMAIL-CHECK.    

       P62.     
           INSPECT R2-REFDOC REPLACING ALL "," BY ";"
           
           PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > 36
             PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 36
               STRING R2-REFDOC(1:1) ALFATAB1(Z) ALFATAB2(Y) 
                 DELIMITED BY SIZE INTO TEST-KEY
               MOVE TEST-KEY TO REF-KEY
               READ REFPHY
                 INVALID
      *             DISPLAY TEST-KEY " INVALID"
                   MOVE 1 TO FLAG
                   MOVE 37 TO Y
                   MOVE 37 TO Z
                 NOT INVALID
      *             DISPLAY TEST-KEY " NOT INVALID"
                   IF REF-NAME = TEST-NAME
                     DISPLAY REF-KEY " " REF-NAME
                     DISPLAY "ON FILE ALREADY. FALLING BACK TO RRI-62"
                     MOVE 37 TO Y
                     MOVE 37 TO Z
                   END-IF
               END-READ
             END-PERFORM
           END-PERFORM 

           IF FLAG = 1            
             CLOSE REFPHY
             OPEN I-O REFPHY
             MOVE TEST-KEY TO REF-KEY             
             MOVE SPACE TO REF-BSNUM
             MOVE SPACE TO REF-CRNUM
             MOVE SPACE TO REF-UPIN
             MOVE SPACE TO REF-CDNUM
             MOVE R2-REFDOC TO REF-NAME
             MOVE R2-NPI TO REF-NPI
             WRITE REFPHY01
               INVALID
                 DISPLAY "BAD WRITE ON REFPHY"  
             END-WRITE
             CLOSE REFPHY
             OPEN INPUT REFPHY
           END-IF.          

       9100CMF.
           CLOSE ACTFILE EMAILAUTHFILE ORDFILE COMPFILE
                 REFPHY HOSPFILE INSFILE FILEOUT ERRFILE
                 FILEIN MOBLFILE GARFILE.
           DISPLAY "RRMC DATA FILE LOAD HAS ENDED".
           STOP RUN.
