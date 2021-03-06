       IDENTIFICATION DIVISION.
       PROGRAM-ID. cob000.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARFILE ASSIGN TO "S20" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS CHARFILE-KEY
           LOCK MODE MANUAL.
           SELECT PAYFILE ASSIGN TO "S25" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYFILE-KEY
           LOCK MODE MANUAL.
           SELECT PARMNDEX ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PARM-KEY
           LOCK MODE MANUAL.
           SELECT CLAIMFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CLAIM-KEY
           LOCK MODE MANUAL.
           SELECT PATFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS P-PATNO
           ALTERNATE RECORD KEY IS P-GARNO WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT GARFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC  RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT INSFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT REFPHY ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS REF-KEY
           ALTERNATE RECORD KEY IS REF-BSNUM  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CRNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-UPIN  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CDNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-NAME  WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT PROCFILE ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS PROC-KEY
           LOCK MODE MANUAL.
           SELECT CMNTFILE ASSIGN TO "S65" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS CMNT-KEY
           LOCK MODE MANUAL.
           SELECT AUTHFILE ASSIGN TO "S70" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS AUTH-KEY
           LOCK MODE MANUAL.
           SELECT COMPFILE ASSIGN TO "S75" ORGANIZATION IS INDEXED
           ACCESS MODE DYNAMIC RECORD KEY IS COMP-KEY
           LOCK MODE MANUAL.
           SELECT MPLRFILE ASSIGN TO "S80" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS MPLR-KEY
           LOCK MODE IS MANUAL.
           SELECT PAYCUR ASSIGN TO "S90" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.
           SELECT ADDRFILE ASSIGN TO "S95" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS ADDR-KEY
           ALTERNATE RECORD KEY IS ADDR-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS ADDR-STREET WITH DUPLICATES
           ALTERNATE RECORD KEY IS ADDR-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS ADDR-STATE WITH DUPLICATES
           ALTERNATE RECORD KEY IS ADDR-ZIP WITH DUPLICATES
           LOCK MODE IS MANUAL.
           SELECT RPGPROCFILE ASSIGN TO "S100" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS RPGPROC-KEY
           LOCK MODE MANUAL.
           SELECT HOSPFILE ASSIGN TO "S105" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS HOSP-KEY
           ALTERNATE RECORD KEY IS H-INS-KEY WITH DUPLICATES
           ALTERNATE RECORD KEY IS H-INS-NAME WITH DUPLICATES.

           SELECT RPGACTFILE ASSIGN TO "S115" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS RPG-ACTNO
             ALTERNATE RECORD KEY IS RPG-GARNO WITH DUPLICATES
             ALTERNATE RECORD KEY IS RPGNAME-KEY WITH DUPLICATES
             LOCK MODE MANUAL.
           SELECT RPGINSFILE ASSIGN TO "S120" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS RPGINS-KEY
           ALTERNATE RECORD KEY IS RPGINS-TITLE WITH DUPLICATES
           ALTERNATE RECORD KEY IS RPGINS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS RPGINS-STATE WITH DUPLICATES
           ALTERNATE RECORD KEY IS RPGINS-CMS WITH DUPLICATES
           ALTERNATE RECORD KEY IS RPGINS-GAP WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT HISFILE ASSIGN TO "S125" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS HISFILE-KEY.
           SELECT BATCHFILE ASSIGN TO "S130" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS BATCH-KEY
           ALTERNATE RECORD KEY IS BA-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS BA-DATE-A WITH DUPLICATES
           ALTERNATE RECORD KEY IS BA-STAT WITH DUPLICATES.

           SELECT KINFILE ASSIGN TO "S140" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS KINFILE-KEY
           ALTERNATE RECORD KEY IS KIN-DATE-E WITH DUPLICATES
           ALTERNATE RECORD KEY IS KIN-STAT WITH DUPLICATES
           LOCK MODE MANUAL.

           SELECT FILE1 ASSIGN TO "S200" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE2 ASSIGN TO "S250" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE3 ASSIGN TO "S300" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE4 ASSIGN TO "S350" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE5 ASSIGN TO "S400" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE6 ASSIGN TO "S450" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE7 ASSIGN TO "S500" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE8 ASSIGN TO "S550" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE9 ASSIGN TO "S600" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE10 ASSIGN TO "S650" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE11 ASSIGN TO "S700" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE12 ASSIGN TO "S750" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE13 ASSIGN TO "S800" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE14 ASSIGN TO "S850" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE15 ASSIGN TO "S900" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE16 ASSIGN TO "S950" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE17 ASSIGN TO "S1000" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE18 ASSIGN TO "S1050" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE20 ASSIGN TO "S1150" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE21 ASSIGN TO "S1200" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE22 ASSIGN TO "S1250" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE23 ASSIGN TO "S1300" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT FILE28 ASSIGN TO "S1400" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S1450" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FILE1.
       01  FILE101 PIC X(189).

       FD  FILE2.
       01  FILE201 PIC X(80).

       FD  FILE3.
       01  FILE301 PIC X(75).

       FD  FILE4.
       01  FILE401 PIC X(7).

       FD  FILE5.
       01  FILE501 PIC X(51).

       FD  FILE6.
       01  FILE601 PIC X(315).

       FD  FILE7.
       01  FILE701 PIC X(120).

       FD  FILE8.
       01  FILE801 PIC X(61).

       FD  FILE9.
       01  FILE901 PIC X(46).

       FD  FILE10.
       01  FILE1001 PIC X(99).

       FD  FILE11.
       01  FILE1101 PIC X(42).

       FD  FILE12.
       01  FILE1201 PIC X(149).

       FD  FILE13.
       01  FILE1301 PIC X(156).

       FD  FILE14.
       01  FILE1401 PIC X(160).

       FD  FILE15.
       01  FILE1501 PIC X(50).

       FD  FILE16.
       01  FILE1601 PIC X(82).

       FD  FILE17.
       01  FILE1701 PIC X(46).

       FD  FILE18.
       01  FILE1801 PIC X(26).

       FD  FILE20.
       01  FILE2001 PIC X(342).

       FD  FILE21.
       01  FILE2101 PIC X(184).

       FD  FILE22.
       01  FILE2201 PIC X(135).

       FD  FILE23.
       01  FILE2301 PIC X(39).

       FD  FILE28.
       01  FILE2801 PIC X(74).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(360).

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
           02 CD-PROC PIC X(11).
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
       FD  PARMNDEX.
       01  PARMNDEX01.
           02 PARM-KEY.
             03 PM-KEY8 PIC X(8).
             03 PM-KEY3 PIC XXX.
           02 PM-DATA PIC X(64).
       FD  CLAIMFILE.
       01  CLAIMFILE01.
           02 CLAIM-KEY PIC X.
           02 CLAIMNO PIC 9(6).
       FD PATFILE.
       01 PATFILE01.
           02 P-PATNO PIC X(8).
           02 P-GARNO PIC X(8).
           02 P-PATNAME PIC X(24).
           02 P-SEX PIC X.
           02 P-RELATE PIC X.
           02 P-MSTAT PIC X.
           02 P-DOB PIC X(8).
       FD GARFILE.
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
           02 G-DOB. 
              03 G-DOBYY PIC XXXX.
              03 G-DOBMM PIC XX.
              03 G-DOBDD PIC XX.
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
           02 G-ADDRCODE PIC X(4).
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
           02 G-ACCT PIC X(8).
           02 G-PRGRPNAME PIC X(15).
           02 G-SEGRPNAME PIC X(15).
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
       FD  REFPHY.
       01  REFPHY01.
           02 REF-KEY PIC XXX.
           02 REF-BSNUM PIC X(5).
           02 REF-CRNUM PIC X(6).
           02 REF-UPIN PIC X(6).
           02 REF-CDNUM PIC X(7).
           02 REF-NAME PIC X(24).
           02 REF-KP PIC X(7).
           02 REF-FUTURE PIC XXX.
       FD  PROCFILE.
       01  PROCFILE01.
           02 PROC-KEY.
             03 PROC-KEY1 PIC X(4).
             03 PROC-KEY2 PIC X(7).
           02 PROC-TYPE PIC X.
           02 PROC-TITLE PIC X(28).
           02 PROC-AMOUNT PIC 9(4)V99.
       FD  CMNTFILE.
       01  CMNTFILE01.
           02 CMNT-KEY.
             03 CM-KEY8 PIC X(8).
             03 CM-KEY3 PIC XXX.
           02 CMNT PIC X(80).
           02 CMNT-DATE-E PIC X(8).
       FD  AUTHFILE.
       01  AUTHFILE01.
           02 AUTH-KEY.
              03 AUTH-KEY8 PIC X(8).
              03 AUTH-KEY6 PIC X(6).
           02 AUTH-NUM PIC X(15).
           02 AUTH-QNTY PIC XX.
           02 AUTH-DATE-E PIC X(8).
           02 AUTH-FILLER PIC XXX.
       FD COMPFILE.
       01  COMPFILE01.
           02 COMP-KEY.
             03 COMP-MEDREC PIC X(8).
             03 COMP-DATE   PIC X(8).
             03 COMP-PROC PIC X(4).
           02 COMP-NAME     PIC X(25).
           02 COMP-CONTACT  PIC X(25).
           02 COMP-ADDR1    PIC X(20).
           02 COMP-ADDR2    PIC X(15).
           02 COMP-CITY     PIC X(20).
           02 COMP-STATE    PIC XX.
           02 COMP-ZIP      PIC X(10).
           02 COMP-PHONE    PIC X(12).

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


       FD  ADDRFILE.
       01  ADDRFILE01.
           02 ADDR-KEY PIC X(4). 
           02 ADDR-NAME PIC X(22).
           02 ADDR-STREET PIC X(24).
           02 ADDR-CITY PIC X(15).
           02 ADDR-STATE PIC XX.
           02 ADDR-ZIP PIC X(9).
           02 ADDR-FUTURE PIC X(6).
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

       FD  HOSPFILE.
       01  HOSPFILE01.
           02 HOSP-KEY PIC X(5).
           02 H-INS-KEY PIC XXX.
           02 H-INS-NAME PIC X(18).
       FD RPGACTFILE.
       01 RPGACTFILE01.
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
           02 RPGNAME-KEY PIC XXX.
       FD  RPGINSFILE.
       01  RPGINSFILE01.   
           02 RPGINS-KEY.  
              03 RPGINS-KEY10 PIC X(10).
              03 RPGINS-KEY2 PIC XX.
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
       FD  HISFILE.
       01  HISFILE01.
           02 HISFILE-KEY.
             03 HI-KEY8 PIC X(8).
             03 HI-CLAIM PIC X(6).
             03 HI-REC-TYPE PIC X.
             03 HI-KEY4 PIC XXXX.
           02 HI-PATID PIC X(8).
           02 HI-SERVICE PIC X.
           02 HI-DIAG PIC X(5).
           02 HI-PROC PIC X(11).
           02 HI-MOD2 PIC XX.
           02 HI-MOD3 PIC XX.
           02 HI-MOD4 PIC XX.
           02 HI-AMOUNTX PIC X(6).
           02 HI-DOCR PIC X(3).
           02 HI-DOCP PIC X(2).
           02 HI-PAYCODE PIC XXX.
           02 HI-STUD PIC X.
           02 HI-WORK PIC XX.
           02 HI-DAT1 PIC X(8).
           02 HI-RESULT PIC X.
           02 HI-ACT PIC X.
           02 HI-SORCREF PIC X.
           02 HI-COLLT PIC X.
           02 HI-AGE PIC X.
           02 HI-PAPER PIC X.
           02 HI-PLACE PIC X.
           02 HI-EPSDT PIC X.
           02 HI-DATE-T.
             03 HI-DATE-TCC PIC XX.
             03 HI-DATE-TYY PIC XX.
             03 HI-DATE-TMM PIC XX.
             03 HI-DATE-TDD PIC XX.
           02 HI-DATE-A PIC X(8).
           02 HI-DATE-E PIC X(8).
           02 HI-REC-STAT PIC X.
           02 HI-DX2 PIC X(5).
           02 HI-DX3 PIC X(5).
           02 HI-AHI-TYPE PIC X.
           02 HI-DATE-M PIC X(8).
           02 HI-ASSIGN PIC X.
           02 HI-NEIC-ASSIGN PIC X.
           02 HI-FUTURE PIC X(6).
       FD  BATCHFILE.
       01  BATCHFILE01.
           02 BATCH-KEY PIC X(6).
           02 BA-NAME PIC X(10).
           02 BA-DATE-A PIC X(8).
           02 BA-DATE-C PIC X(8).
           02 BA-AMT PIC S9(8)V99.
           02 BA-STAT  PIC X.

       FD  KINFILE.
       01  KINFILE01.
           02 KINFILE-KEY.
             03 KIN-KEY8 PIC X(8).
             03 KIN-KEY3 PIC XXX.
           02 KIN-NAME PIC X(24).
           02 KIN-AMOUNT PIC S9(4)V99.
           02 KIN-PAYCODE PIC XXX.
           02 KIN-DENIAL PIC XX.
           02 KIN-DATE-T PIC X(8).
           02 KIN-DATE-E PIC X(8).
           02 KIN-STAT PIC X.
           02 KIN-CHARCUR-KEY PIC X(11).

       WORKING-STORAGE SECTION.
       01  NUM1 PIC 9.
       01  cntr pic 999 value 0.
       PROCEDURE DIVISION.
       P0.
           OPEN output charfile payfile parmndex claimfile patfile
             garfile insfile refphy procfile cmntfile authfile
             compfile mplrfile paycur addrfile rpgprocfile 
             hisfile hospfile rpgactfile rpginsfile batchfile
             kinfile  FILEOUT.

           close charfile payfile parmndex claimfile patfile
             garfile insfile refphy procfile cmntfile authfile
             compfile mplrfile paycur addrfile rpgprocfile 
             hisfile hospfile rpgactfile rpginsfile batchfile
              kinfile.

           open i-o charfile payfile parmndex claimfile patfile
             garfile insfile refphy procfile cmntfile authfile
             compfile mplrfile         paycur addrfile rpgprocfile 
             hisfile hospfile rpgactfile rpginsfile batchfile
              kinfile.

           open input file1 file2 file3 file4 file5 file6
           file7 file8 file9 file10 file11 file12
           file13 file15 file16 file17 file18
           file20 file21 file22 file23 file28.
           
       p1.
           read file1 at end close charfile go to p2.
           move file101 to charfile01
           write charfile01
            invalid 
             move space to fileout01
             string "file1 " file101 delimited by size into fileout01
             write fileout01
             end-write
           end-write
           go to p1.
       p2.
           read file2 at end close payfile go to p3.
           move file201 to payfile01
           write payfile01
             invalid
             move space to fileout01
             string "file2 " file201 delimited by size into fileout01
             write fileout01
             end-write
           end-write
           go to p2.
       p3.
           read file3 at end close parmndex go to p4.
           move file301 to parmndex01
           write parmndex01
             invalid
             move space to fileout01
             string "file3 " file301 delimited by size into fileout01
             write fileout01
             end-write
           end-write
           go to p3.
       p4.
           read file4 at end close claimfile go to p5.
           move file401 to claimfile01
           write claimfile01
           invalid          
             move space to fileout01
             string "file4 " file401 delimited by size into fileout01
             write fileout01
             end-write
           end-write
           go to p4.
       p5.
           read file5 at end close patfile  go to p6.
           move file501 to patfile01
           write patfile01
             invalid          
             move space to fileout01
             string "file5 " file501 delimited by size into fileout01
             write fileout01
             end-write
           end-write
           go to p5.
       p6.
           read file6 at end close garfile go to p7.
           move file601 to garfile01
           write garfile01             
             invalid          
             move space to fileout01
             string "file6 " file601 delimited by size into fileout01
             write fileout01
             end-write
           end-write         
           go to p6.
       p7.
           read file7 at end close insfile go to p8.
           move file701 to insfile01
           write insfile01
             invalid          
             move space to fileout01
             string "file7 " file701 delimited by size into fileout01
             write fileout01
             end-write
           end-write                    
           go to p7.
       p8.
           read file8 at end close refphy go to p9.
           move file801 to refphy01
           write refphy01
             invalid      
             move space to fileout01
             string "file8 " file801 delimited by size into fileout01
             write fileout01
             end-write
           end-write  
           go to p8.
       p9.
           read file9 at end close procfile go to p10.
           move file901 to procfile01 
           write procfile01
             invalid      
             move space to fileout01
             string "file9 " file901 delimited by size into fileout01
             write fileout01
             end-write
           end-write  
           go to p9.
       p10.
           read file10 at end close cmntfile go to p11.
           move file1001 to cmntfile01 
           write cmntfile01
             invalid      
             move space to fileout01
             string "file10 " file1001 delimited by size into fileout01
             write fileout01
             end-write
           end-write  
           go to p10.
       p11.
           read file11 at end close authfile go to p12.
           move file1101 to authfile01 
           write authfile01
             invalid      
             move space to fileout01
             string "file11 " file1101 delimited by size into fileout01
             write fileout01
             end-write
           end-write  
           go to p11.
       p12.
           read file12 at end close compfile go to p13.
           move file1201 to compfile01 
           write compfile01
             invalid      
             move space to fileout01
             string "file12 " file1201 delimited by size into fileout01
             write fileout01
             end-write
           end-write  
           go to p12.
       p13.
           read file13 at end close mplrfile go to p15.
           move file1301 to mplrfile01 
           write mplrfile01
             invalid      
             move space to fileout01
             string "file13 " file1301 delimited by size into fileout01
             write fileout01
             end-write
           end-write  
           go to p13.
       p15.
           read file15 at end close paycur go to p16.
           move file1501 to paycur01 
           write paycur01
             invalid      
             move space to fileout01
             string "file15 " file1501 delimited by size into fileout01
             write fileout01
             end-write
           end-write  
           go to p15.
       p16.
           read file16 at end close addrfile go to p17.
           move file1601 to addrfile01
           write addrfile01
             invalid      
             move space to fileout01
             string "file16 " file1601 delimited by size into fileout01
             write fileout01
             end-write
           end-write  
           go to p16.
       p17.
           read file17 at end close rpgprocfile go to p18.
           move file1701 to rpgprocfile01
           write rpgprocfile01
             invalid      
             move space to fileout01
             string "file17 " file701 delimited by size into fileout01
             write fileout01
             end-write
           end-write  
           go to p17.
       p18.
           read file18 at end close hospfile go to p20.
           move file1801 to hospfile01
           write hospfile01
             invalid      
             move space to fileout01
             string "file18 " file1801 delimited by size into fileout01
             write fileout01
             end-write
           end-write  
           go to p18.
       p20.
           read file20 at end close rpgactfile go to p21.
           move file2001 to rpgactfile01 
           write rpgactfile01
             invalid      
             move space to fileout01
             string "file20 " file2001 delimited by size into fileout01
             write fileout01
             end-write
           end-write  
           go to p20.
       p21.
           read file21 at end close rpginsfile go to p22.
           move file2101 to rpginsfile01 
           write rpginsfile01
             invalid      
             move space to fileout01
             string "file21 " file2101 delimited by size into fileout01
             write fileout01
             end-write
           end-write  
           go to p21.
       p22.
           read file22 at end close hisfile go to p23.
           move file2201 to hisfile01
           write hisfile01
             invalid      
             move space to fileout01
             string "file22 " file2201 delimited by size into fileout01
             write fileout01
             end-write
           end-write  
           go to p22.
       p23.
           read file23 at end close batchfile go to p28.
           move file2301 to batchfile01
           write batchfile01
             invalid      
             move space to fileout01
             string "file23 " file2301 delimited by size into fileout01
             write fileout01
             end-write
           end-write  
           go to p23.

       p28.
           read file28 at end close kinfile go to p99.
           move file2801 to kinfile01
           write kinfile01
             invalid      
             move space to fileout01
             string "file28 " file2801 delimited by size into fileout01
             write fileout01
             end-write
           end-write  
           go to p28.

       p99.
           close FILEOUT.
           close FILE1 FILE2 FILE3 FILE4 FILE5 FILE6 FILE7 FILE8
                 FILE9 FILE10 FILE11 FILE12 FILE13 FILE15
                 FILE16 FILE17 FILE18 FILE20 FILE21 FILE22
                 FILE23  FILE28

           STOP RUN.
