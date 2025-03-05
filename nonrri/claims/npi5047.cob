      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. npi5047.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT GARFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
           LOCK MODE MANUAL.
           SELECT PATFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS P-PATNO
           ALTERNATE RECORD KEY IS P-GARNO WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT SEGFILE ASSIGN TO "S45" ORGANIZATION 
           LINE SEQUENTIAL.
           SELECT FILEIN ASSIGN TO "S50" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT ERRFILE ASSIGN TO "S55" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT PARMFILE ASSIGN TO "S60" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT REFPHY ASSIGN TO "S65" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS REF-KEY
           ALTERNATE RECORD KEY IS REF-BSNUM  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CRNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-UPIN  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CDNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-NAME  WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT DIAGFILE ASSIGN TO "S70" ORGANIZATION IS INDEXED
           ACCESS IS RANDOM RECORD KEY IS DIAG-KEY
           ALTERNATE RECORD KEY IS DIAG-TITLE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT AUTHFILE ASSIGN TO "S75" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS AUTH-KEY
           LOCK MODE MANUAL.
           SELECT MPLRFILE ASSIGN TO "S80" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC        RECORD KEY IS MPLR-KEY
           LOCK MODE MANUAL.
           SELECT INSFILE ASSIGN TO "S85" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT GAPFILE ASSIGN TO "S90" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS GAPKEY
           ALTERNATE RECORD KEY IS GAP-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-STATE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT PAYCUR ASSIGN TO "S95" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.
           SELECT PLACEFILE ASSIGN TO "S100" ORGANIZATION 
           LINE SEQUENTIAL.
           SELECT PARMFILE2 ASSIGN TO "S105" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT HIPCLAIMFILE ASSIGN TO "S110" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS HIP-KEY
           LOCK MODE MANUAL.
           SELECT PROVCAID ASSIGN TO "S115" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS PROV-KEY
           ALTERNATE RECORD KEY IS PROV-NPI WITH DUPLICATES
           ALTERNATE RECORD KEY IS PROV-TAX WITH DUPLICATES
           ALTERNATE RECORD KEY IS PROV-NAME WITH DUPLICATES
           LOCK MODE MANUAL.
       DATA DIVISION.
       FILE SECTION.
       FD  PROVCAID.
       01  PROVCAID01.
           02 PROV-KEY PIC X(7).
           02 PROV-NAME PIC X(24).
           02 PROV-NPI PIC X(10).
           02 PROV-TAX PIC X(10).
           02 PROV-STREET PIC X(20).
           02 PROV-CITY PIC X(20).
           02 PROV-STATE PIC XX.
           02 PROV-ZIP PIC X(5).
       FD  HIPCLAIMFILE.
       01  HIPCLAIMFILE01.
           02 HIP-KEY PIC X.
           02 HIP-NUM PIC 9(9).
       FD  PLACEFILE.
       01  PLACEFILE01.
           02 DF1 PIC X.
           02 DF2 PIC X.
           02 DF3 PIC X(22).
           02 DF4 PIC X(18).
           02 DF5 PIC X(15).
           02 DF6 PIC XX.
           02 DF7 PIC X(9).
       
       FD PARMFILE.
       01  PARMFILE01 PIC X(75).
       FD PARMFILE2.
       01  PARMFILE201 PIC X(15).
       
       FD  ERRFILE.
       01  ERRFILE01.
           02 EF-1 PIC X(11).
           02 FILLER PIC X VALUE SPACE.
           02 EF-2 PIC X(9).
           02 FILLER PIC X VALUE SPACE.
           02 EF-3 PIC X(16).
           02 EF-4 PIC X(20).
           02 FILLER PIC X VALUE SPACE.
           02 EF-5 PIC X(10).

       FD  SEGFILE.
       01  SEGFILE01 PIC X(160).
       FD  FILEIN.
       01  FILEIN01.
           02 FILEIN-KEY.
             03 FI-KEY8 PIC X(8).
             03 FI-KEY3 PIC XXX.
           02 FI-PATID.
             03 FI-PATID7 PIC X(7).
             03 FI-PATID8 PIC X.
           02 FI-CLAIM PIC X(6).
           02 FI-SERVICE PIC X.
           02 FI-DIAG PIC X(7).
           02 FI-PROC.
             03 FI-PROC1 PIC X(5).
             03 FI-PROC2 PIC XX.
           02 FI-MOD2 PIC XX.
           02 FI-MOD3 PIC XX.
           02 FI-MOD4 PIC XX.
           02 FI-AMOUNT PIC S9(4)V99.
           02 FI-DOCR PIC X(3).
           02 FI-DOCP PIC 99.
           02 FI-PAYCODE PIC XXX.
           02 FI-STUD PIC X.
           02 FI-WORK PIC 99.
           02 FI-DAT1 PIC X(8).
           02 FI-RESULT PIC X.
           02 FI-ACTION PIC X.
           02 FI-SORCREF PIC X.
           02 FI-COLLT PIC X.
           02 FI-AGE PIC X.
           02 FI-PAPER PIC X.
           02 FI-PLACE PIC X.
           02 FI-IOPAT PIC X.
           02 FI-DATE-T PIC X(8).
           02 FI-DATE-A PIC X(8).
           02 FI-DATE-P PIC X(8).
           02 FI-REC-STAT PIC X.
           02 FI-DX2 PIC X(7).
           02 FI-DX3 PIC X(7).
           02 FI-ACC-TYPE PIC X.
           02 FI-DATE-M PIC X(8).
           02 FI-ASSIGN PIC X.
           02 FI-NEIC-ASSIGN PIC X.
           02 FI-DX4 PIC X(7).
           02 FI-DX5 PIC X(7).
           02 FI-DX6 PIC X(7).
           02 FI-FUTURE PIC X(6).

       FD  CHARCUR
           BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS CHARCUR01.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID.
             03 CC-PATID7 PIC X(7).
             03 CC-PATID8 PIC X.
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC.
             03 CC-PROC1 PIC X(5).
             03 CC-PROC2 PIC XX.
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
           02 CC-ACTION PIC X.
           02 CC-SORCREF PIC X.
           02 CC-COLLT PIC X.
           02 CC-AGE PIC X.
           02 CC-PAPER PIC X.
           02 CC-PLACE PIC X.
           02 CC-IOPAT PIC X.
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
       FD  DIAGFILE.
       01  DIAG01.
           02 DIAG-KEY PIC X(7).
           02 DIAG-TITLE PIC X(61).
           02 DIAG-MEDB PIC X(5).

       WORKING-STORAGE SECTION.
       01 ISA01.
           02 ISA-0 PIC XXX VALUE "ISA".
           02 ISA-S0 PIC X VALUE "*".
           02 ISA-1 PIC XX.
           02 ISA-S1 PIC X VALUE "*".
           02 ISA-2 PIC X(10).
           02 ISA-S2 PIC X VALUE "*".
           02 ISA-3 PIC XX.
           02 ISA-S3 PIC X VALUE "*".
           02 ISA-4 PIC X(10).
           02 ISA-S4 PIC X VALUE "*".
           02 ISA-5 PIC XX.
           02 ISA-S5 PIC X VALUE "*".
           02 ISA-6 PIC X(15).
           02 ISA-S6 PIC X VALUE "*".
           02 ISA-7 PIC XX.
           02 ISA-S7 PIC X VALUE "*".
           02 ISA-8 PIC X(15).
           02 ISA-S8 PIC X VALUE "*".
           02 ISA-9 PIC X(6).
           02 ISA-S9 PIC X VALUE "*".
           02 ISA-10 PIC X(4).
           02 ISA-S10 PIC X VALUE "*".
           02 ISA-11 PIC X.
           02 ISA-S11 PIC X VALUE "*".
           02 ISA-12 PIC X(5).
           02 ISA-S12 PIC X VALUE "*".
           02 ISA-13 PIC X(9).
           02 ISA-S13 PIC X VALUE "*".
           02 ISA-14 PIC X.
           02 ISA-S14 PIC X VALUE "*".
           02 ISA-15 PIC X.
           02 ISA-S15 PIC X VALUE "*".
           02 ISA-16 PIC X.
           02 ISA-S16 PIC X VALUE "*".
           02 ISA-END PIC X VALUE "~".
       01 IEA01.
           02 IEA-0 PIC XXX VALUE "IEA".
           02 IEA-S0 PIC X VALUE "*".
           02 IEA-1 PIC X VALUE "1".
           02 IEA-END PIC X VALUE "~".
       01 GS01.
           02 GS-0 PIC XX VALUE "GS".
           02 GS-S0 PIC X VALUE "*".
           02 GS-1 PIC XX VALUE "HC".
           02 GS-S1 PIC X VALUE "*".
           02 GS-2 PIC X(9) VALUE "701100357".
           02 GS-S2 PIC X VALUE "*".
           02 GS-3 PIC X(9) VALUE "822287119".
           02 GS-S3 PIC X VALUE "*".
           02 GS-4 PIC X(8).
           02 GS-S4 PIC X VALUE "*".
           02 GS-5 PIC X(4).
           02 GS-S5 PIC X VALUE "*".
           02 GS-NUM PIC X(9).
           02 GS-S6 PIC X VALUE "*".
           02 GS-7 PIC X VALUE "X".
           02 GS-S7 PIC X VALUE "*".
           02 GS-8 PIC X(12) VALUE "005010X222A1".
           02 GS-S8 PIC X VALUE "*".
           02 GS-END PIC X VALUE "~".
       01  ST01.
           02 ST-0 PIC XX VALUE "ST".
           02 ST-S0 PIC X VALUE "*".
           02 ST-1 PIC XXX VALUE "837".
           02 ST-S1 PIC X VALUE "*".
           02 ST-NUM PIC X(9).
           02 ST-S2 PIC X VALUE "*".
           02 ST-CONVENT-REF PIC X(12) VALUE "005010X222A1".
           02 ST-END PIC X VALUE "~".
       01  SE01.
           02 SE-0 PIC XX VALUE "SE".
           02 SE-S0 PIC X VALUE "*".
           02 SE-CNTR PIC X(9).
           02 SE-S1 PIC X VALUE "*".
           02 SE-NUM PIC X(9).
           02 SE-END PIC X VALUE "~".
       01  GE01.
           02 GE-0 PIC XX VALUE "GE".
           02 GE-S0 PIC X VALUE "*".
           02 GE-CNTR PIC 9 VALUE 1.
           02 GE-S1 PIC X VALUE "*".
           02 GE-NUM PIC X(9).
           02 GE-END PIC X VALUE "~".
       01  BHT01.
           02 BHT-0 PIC XXX VALUE "BHT".
           02 BHT-S0 PIC X VALUE "*".
           02 BHT-1 PIC X(4) VALUE "0019".
           02 BHT-S1 PIC X VALUE "*".
           02 BHT-2 PIC XX VALUE "00".
           02 BHT-S2 PIC X VALUE "*".
           02 BHT-NUM PIC X(9).
           02 BHT-S3 PIC X VALUE "*".
           02 BHT-DATE PIC X(8).
           02 BHT-S4 PIC X VALUE "*".
           02 BHT-TIME PIC X(4).
           02 BHT-S5 PIC X VALUE "*".
           02 BHT-6  PIC XX VALUE "CH".
           02 BHT-END PIC X VALUE "~".
       01 REF01.
           02 REF-0 PIC XXX VALUE "REF".
           02 REF-S0 PIC X VALUE "*".
           02 REF-CODE PIC X(30).
           02 REF-S1 PIC X VALUE "*".
           02 REF-ID PIC X(30). 
           02 REF-S2 PIC X VALUE "*".
           02 REF-3 PIC XX VALUE SPACE.
           02 REF-S3 PIC X VALUE "*".
           02 REF-4 PIC XX VALUE SPACE.
           02 REF-S4 PIC X VALUE "*".
           02 REF-END PIC X VALUE "~".
       01 SAVE-DOCREF01.
           02 SAVE-DOCREF-0 PIC XXX VALUE "REF".
           02 SAVE-DOCREF-S0 PIC X VALUE "*".
           02 SAVE-DOCREF-CODE PIC X(30).
           02 SAVE-DOCREF-S1 PIC X VALUE "*".
           02 SAVE-DOCREF-ID PIC X(30). 
           02 SAVE-DOCREF-S2 PIC X VALUE "*".
           02 SAVE-DOCREF-3 PIC XX VALUE SPACE.
           02 SAVE-DOCREF-S3 PIC X VALUE "*".
           02 SAVE-DOCREF-4 PIC XX VALUE SPACE.
           02 SAVE-DOCREF-S4 PIC X VALUE "*".
           02 SAVE-DOCREF-END PIC X VALUE "~".

       01  SUBM01.
           02 SUBM-0 PIC XXX VALUE "NM1".
           02 SUBM-S0 PIC X VALUE "*".
           02 SUBM-1 PIC XX VALUE "41".
           02 SUBM-S1 PIC X VALUE "*".
           02 SUBM-2 PIC X VALUE "2".
           02 SUBM-S2 PIC X VALUE "*".
           02 SUBM-3 PIC X(25) VALUE "CARE MANAGEMENT SOLUTIONS".
           02 SUBM-S3 PIC X VALUE "*".
           02 SUBM-S4 PIC X VALUE "*".
           02 SUBM-S5 PIC X VALUE "*".
           02 SUBM-S51 PIC X VALUE "*".
           02 SUBM-S6 PIC X VALUE "*".
           02 SUBM-8 PIC XX VALUE "46".
           02 SUBM-S7 PIC X VALUE "*".
           02 SUBM-NUM PIC X(3) VALUE "D57".
           02 SUBM-END PIC X VALUE "~".
       01  SUBPER01.
           02 SUBPER-0 PIC XXX VALUE "PER".
           02 SUBPER-S0 PIC X VALUE "*".
           02 SUBPER-1 PIC XX VALUE "IC". 
           02 SUBPER-S1 PIC X VALUE "*".
           02 SUBPER-2 PIC X(9) VALUE "S WAITE".
           02 SUBPER-S2 PIC X VALUE "*".
           02 SUBPER-3 PIC XX VALUE "TE".
           02 SUBPER-S3 PIC X VALUE "*".
           02 SUBPER-4  PIC X(10) VALUE "8003718685".
           02 SUBPER-S4 PIC X VALUE "*".
           02 SUBPER-5 PIC XX VALUE "FX".
           02 SUBPER-S5 PIC X VALUE "*".
           02 SUBPER-6  PIC X(10) VALUE "8027705175".
           02 SUBPER-S6 PIC X VALUE "*".
           02 SUBPER-7 PIC XX VALUE "EM".
           02 SUBPER-S7 PIC X VALUE "*".
           02 SUBPER-8  PIC X(17) VALUE "cmswest@sover.net".
           02 SUBPER-S9 PIC X VALUE "*".
           02 SUBPER-END PIC X VALUE "~".
       01  INSNM01.
           02 INSNM-1 PIC XXX VALUE "NM1".
           02 INSNM-S0 PIC X VALUE "*".
           02 INSNM-2 PIC XX VALUE "40".
           02 INSNM-S1 PIC X VALUE "*".
           02 INSNM-3  PIC X VALUE "2".
           02 INSNM-S2 PIC X VALUE "*".
           02 INSNM-NAME PIC X(35).
           02 INSNM-S3 PIC X VALUE "*".
           02 INSNM-S4 PIC X VALUE "*".
           02 INSNM-S5 PIC X VALUE "*".
           02 INSNM-S51 PIC X VALUE "*".
           02 INSNM-S6 PIC X VALUE "*".
           02 INSNM-8  PIC XX VALUE "46".
           02 INSNM-S7 PIC X VALUE "*".
           02 INSNM-NUM PIC X(9).
           02 INSNM-END PIC X VALUE "~".
       01 HL01.
           02 HL-0 PIC XX VALUE "HL".
           02 HL-S0 PIC X VALUE "*".
           02 HL-NUMX PIC X(5).
           02 HL-S1 PIC X VALUE "*".
           02 HL-PARENT PIC X(5).
           02 HL-S2 PIC X VALUE "*".
           02 HL-CODE PIC X(4).
           02 HL-S3 PIC X VALUE "*".
           02 HL-CHILD PIC X.
           02 HL-S4 PIC X VALUE "*".
           02 HL-END PIC X VALUE "~".
       01 PRV01.
           02 PRV-0 PIC XXX VALUE "PRV".
           02 PRV-S0 PIC X VALUE "*".
           02 PRV-1 PIC XX.
           02 PRV-S1 PIC X VALUE "*".
           02 PRV-2 PIC XXX VALUE "PXC".
           02 PRV-S2 PIC X VALUE "*".
           02 PRV-TAX PIC X(10).
           02 PRV-END PIC X VALUE "~".
       01  PER01.
           02 PER-0 PIC XXX VALUE "PER".
           02 PER-S0 PIC X VALUE "*".
           02 PER-1 PIC XX VALUE "IC". 
           02 PER-S1 PIC X VALUE "*".
           02 PER-CONTACT PIC X(30).
           02 PER-S2 PIC X VALUE "*".
           02 PER-3 PIC XX VALUE "TE".
           02 PER-STE PIC X VALUE "*".
           02 PER-PHONE PIC X(10).
           02 PER-S3 PIC X VALUE "*".
           02 PER-S4 PIC X VALUE "*".
           02 PER-S5 PIC X VALUE "*".
           02 PER-S6 PIC X VALUE "*".
           02 PER-S7 PIC X VALUE "*".
           02 PER-S8 PIC X VALUE "*".
           02 PER-S9 PIC X VALUE "*".
           02 PER-END PIC X VALUE "~".
       01 NM101.
           02 NM1-0 PIC XXX VALUE "NM1".
           02 NM1-S0 PIC X VALUE "*".
           02 NM1-1 PIC XXX.
           02 NM1-S1 PIC X VALUE "*".
           02 NM1-SOLO PIC X.
           02 NM1-S2 PIC X VALUE "*".
           02 NM1-NAMEL PIC X(40).
           02 NM1-S3 PIC X VALUE "*".
           02 NM1-NAMEF PIC X(25).
           02 NM1-S4 PIC X VALUE "*".
           02 NM1-NAMEM PIC X.
           02 NM1-S5 PIC X VALUE "*".
           02 NM1-S51 PIC X VALUE "*".
           02 NM1-NAMES PIC XXX.
           02 NM1-S6 PIC X VALUE "*".
           02 NM1-EINSS PIC XX.
           02 NM1-S7 PIC X VALUE "*".
           02 NM1-CODE PIC X(16).
           02 NM1-END PIC X VALUE "~".
       01 SAVE-DOCNM101.
           02 SAVE-DOCNM1-0 PIC XXX VALUE "NM1".
           02 SAVE-DOCNM1-S0 PIC X VALUE "*".
           02 SAVE-DOCNM1-1 PIC XXX.
           02 SAVE-DOCNM1-S1 PIC X VALUE "*".
           02 SAVE-DOCNM1-SOLO PIC X.
           02 SAVE-DOCNM1-S2 PIC X VALUE "*".
           02 SAVE-DOCNM1-NAMEL PIC X(40).
           02 SAVE-DOCNM1-S3 PIC X VALUE "*".
           02 SAVE-DOCNM1-NAMEF PIC X(25).
           02 SAVE-DOCNM1-S4 PIC X VALUE "*".
           02 SAVE-DOCNM1-NAMEM PIC X.
           02 SAVE-DOCNM1-S5 PIC X VALUE "*".
           02 SAVE-DOCNM1-S51 PIC X VALUE "*".
           02 SAVE-DOCNM1-NAMES PIC XXX.
           02 SAVE-DOCNM1-S6 PIC X VALUE "*".
           02 SAVE-DOCNM1-EINSS PIC XX.
           02 SAVE-DOCNM1-S7 PIC X VALUE "*".
           02 SAVE-DOCNM1-CODE PIC X(14).
           02 SAVE-DOCNM1-END PIC X VALUE "~".
       01 SAVE-PRV01.
           02 SAVE-PRV-0 PIC XXX VALUE "PRV".
           02 SAVE-PRV-S0 PIC X VALUE "*".
           02 SAVE-PRV-1 PIC XX VALUE "PE".
           02 PRV-S1 PIC X VALUE "*".
           02 SAVE-PRV-2 PIC XXX VALUE "PXC".
           02 SAVE-PRV-S2 PIC X VALUE "*".
           02 SAVE-PRV-TAX PIC X(10).
           02 SAVE-PRV-END PIC X VALUE "~".

       01 RECNM101.
           02 RECNM1-0 PIC XXX VALUE "NM1".
           02 RECNM1-S0 PIC X VALUE "*".
           02 RECNM1-1 PIC XXX VALUE "40 ".
           02 RECNM1-S1 PIC X VALUE "*".
           02 RECNM1-SOLO PIC X VALUE "2".
           02 RECNM1-S2 PIC X VALUE "*".
           02 RECNM1-NAMEL PIC X(11) VALUE "VT MEDICAID".
           02 RECNM1-S3 PIC X VALUE "*".
           02 RECNM1-S4 PIC X VALUE "*".
           02 RECNM1-S5 PIC X VALUE "*".
           02 RECNM1-S51 PIC X VALUE "*".
           02 RECNM1-S6 PIC X VALUE "*".
           02 RECNM1-8 PIC XX VALUE "46".
           02 RECNM1-S7 PIC X VALUE "*".
           02 RECNM1-CODE PIC X(9) VALUE "822287119".
           02 RECNM1-END PIC X VALUE "~".

       01 N301.
           02 N3-0 PIC XX VALUE "N3".
           02 N3-S0 PIC X VALUE "*".
           02 N3-STREET PIC X(24).
           02 N3-S1 PIC X VALUE "*".
           02 N3-BILLADD PIC X(24).
           02 N3-S2 PIC X VALUE "*".
           02 N3-END PIC X VALUE "~".
       01 N401.
           02 N4-0 PIC XX VALUE "N4".
           02 N4-S0 PIC X VALUE "*".
           02 N4-CITY PIC X(20).
           02 N4-S1 PIC X VALUE "*".
           02 N4-STATE PIC XX.
           02 N4-S2 PIC X VALUE "*".
           02 N4-ZIP PIC X(9).
           02 N4-S3 PIC X VALUE "*".
           02 N4-S4 PIC X VALUE "*".
           02 N4-S5 PIC X VALUE "*".
           02 N4-S6 PIC X VALUE "*".
           02 N4-END PIC X VALUE "~".
       01 SBR01.
           02 SBR-0 PIC XXX VALUE "SBR".
           02 SBR-S0 PIC X VALUE "*".
           02 SBR-PST PIC X.
           02 SBR-S1 PIC X VALUE "*".
           02 SBR-RELATE PIC XX.
           02 SBR-S2 PIC X VALUE "*".
           02 SBR-GROUP PIC X(12).
           02 SBR-S3 PIC X VALUE "*".
           02 SBR-GRNAME PIC X(12) VALUE SPACE.
           02 SBR-S4 PIC X VALUE "*".
           02 SBR-TYPE PIC XX.
           02 SBR-S5 PIC X VALUE "*".
           02 SBR-6 PIC X.
           02 SBR-S6 PIC X VALUE "*".
           02 SBR-7 PIC X.
           02 SBR-S7 PIC X VALUE "*".
           02 SBR-8 PIC X.
           02 SBR-S8 PIC X VALUE "*".
           02 SBR-INSCODE PIC XX.
           02 SBR-S9 PIC X VALUE "*".
           02 SBR-END PIC X VALUE "~".
       01 DMG01.
           02 DMG-0 PIC XXX VALUE "DMG".
           02 DMG-S0 PIC X VALUE "*".
           02 DMG-1 PIC XX VALUE "D8".
           02 DMG-S1 PIC X VALUE "*".
           02 DMG-DOB PIC X(8).
           02 DMG-S2 PIC X VALUE "*".
           02 DMG-GENDER PIC X.
           02 DMG-S3 PIC X VALUE "*".
           02 DMG-S4 PIC X VALUE "*".
           02 DMG-S5 PIC X VALUE "*".
           02 DMG-S6 PIC X VALUE "*".
           02 DMG-S7 PIC X VALUE "*".
           02 DMG-S8 PIC X VALUE "*".
           02 DMG-S9 PIC X VALUE "*".
           02 DMG-END PIC X VALUE "~".
       01 PAT01.
           02 PAT-0 PIC XXX VALUE "PAT".
           02 PAT-S0 PIC X VALUE "*".
           02 PAT-RELATE PIC XX.
           02 PAT-S1 PIC X VALUE "*".
           02 PAT-LOCATE PIC X.
           02 PAT-S2 PIC X VALUE "*".
           02 PAT-EMPLOYM PIC XX.
           02 PAT-S3 PIC X VALUE "*".
           02 PAT-STUD PIC X.
           02 PAT-S4 PIC X VALUE "*".
           02 PAT-QUAL PIC XX .
           02 PAT-S5 PIC X VALUE "*".
           02 PAT-DATE PIC X(8).
           02 PAT-S6 PIC X VALUE "*".
           02 PAT-MEAS PIC XX.
           02 PAT-S7 PIC X VALUE "*".
           02 PAT-WT PIC X.
           02 PAT-S8 PIC X VALUE "*".
           02 PAT-PREGO PIC X.
           02 PAT-S9 PIC X VALUE "*".
           02 PAT-END PIC X VALUE "~".
       01 CLM01.
           02 CLM-0 PIC XXX VALUE "CLM".
           02 CLM-S0 PIC X VALUE "*".
           02 CLM-1 PIC X(16).
           02 CLM-S1 PIC X VALUE "*".
           02 CLM-2 PIC X(8).
           02 CLM-S2 PIC X VALUE "*".
           02 CLM-3 PIC XX VALUE SPACE.
           02 CLM-S3 PIC X VALUE "*".
           02 CLM-4 PIC X VALUE SPACE.
           02 CLM-S4 PIC X VALUE "*".
           02 CLM-5 PIC XX.
           02 CLM-S52 PIC X VALUE ":".
           02 CLM-5-2 PIC X VALUE "B".
           02 CLM-COLON-PLACE PIC X VALUE ":".
           02 CLM-FREQ PIC X VALUE "1".
           02 CLM-S5 PIC X VALUE "*".
           02 CLM-6 PIC X VALUE "Y".
           02 CLM-S6 PIC X VALUE "*".
           02 CLM-7 PIC X VALUE "A".
           02 CLM-S7 PIC X VALUE "*".
           02 CLM-8 PIC X VALUE "Y".             
           02 CLM-S8 PIC X VALUE "*".
           02 CLM-9 PIC X VALUE "Y".             
           02 CLM-S9 PIC X VALUE "*".
           02 CLM-10 PIC X VALUE " ".             
           02 CLM-S10 PIC X VALUE "*".
           02 CLM-11 PIC XX.             
           02 CLM-COLON-ACCIDENT PIC X VALUE SPACE. 
           02 CLM-S11 PIC X VALUE "*".
           02 CLM-12 PIC XXX.             
           02 CLM-S12 PIC X VALUE "*".
           02 CLM-13 PIC X.             
           02 CLM-S13 PIC X VALUE "*".
           02 CLM-14 PIC XXX.             
           02 CLM-S14 PIC X VALUE "*".
           02 CLM-15 PIC X.             
           02 CLM-S15 PIC X VALUE "*".
           02 CLM-16 PIC X.             
           02 CLM-S16 PIC X VALUE "*".
           02 CLM-17 PIC XX.             
           02 CLM-S17 PIC X VALUE "*".
           02 CLM-18 PIC X.             
           02 CLM-S18 PIC X VALUE "*".
           02 CLM-19 PIC XX.             
           02 CLM-S19 PIC X VALUE "*".
           02 CLM-20 PIC XX.             
           02 CLM-END PIC X VALUE "~".
           
       01 HI901.
           02 HI9-0 PIC XX VALUE "HI".
           02 HI9-S0 PIC X VALUE "*".
           02 HI9-1C PIC XXX VALUE "BK:".
           02 HI9-DX1 PIC X(5).
           02 HI9-DIAG-FILLER PIC X(108).
           02 HI9-END PIC X VALUE "~".
       01 HI1001.
           02 HI10-0 PIC XX VALUE "HI".
           02 HI10-S0 PIC X VALUE "*".
           02 HI10-1C PIC XXXX VALUE "ABK:".
           02 HI10-DX1 PIC X(7).
           02 HI10-DIAG-FILLER PIC X(144).
           02 HI10-END PIC X VALUE "~".

       01 LX01.
           02 LX-0 PIC XX VALUE "LX".
           02 LX-S0 PIC X VALUE "*".
           02 LX-1 PIC XX.
           02 LX-END PIC X VALUE "~".
       01 SV101.
           02 SV1-0 PIC XXX VALUE "SV1".
           02 SV1-S0 PIC X VALUE "*".
           02 SV1-1 PIC XX VALUE "HC".
           02 SV1-S1 PIC X VALUE ":".
           02 SV1-PROC  PIC X(7).
           02 SV1-MOD-FILLER PIC X(33).
           02 SV1-S6 PIC X VALUE "*".
           02 SV1-AMT  PIC X(7).
           02 SV1-S7 PIC X VALUE "*".
           02 SV1-8  PIC XX VALUE "UN".
           02 SV1-S8 PIC X VALUE "*".
           02 SV1-WORK PIC XX.
           02 SV1-S9 PIC X VALUE "*".
           02 SV1-PLACE PIC XX.
           02 SV1-S10 PIC X VALUE "*".
           02 SV1-S11 PIC X VALUE "*".
           02 SV1-PT PIC X(36).
           02 SV1-S15 PIC X VALUE "*".
           02 SV1-S16 PIC X VALUE "*".
           02 SV1-EMER PIC X.
           02 SV1-S115 PIC XX VALUE "**".
           02 SV1-EPSDT PIC XX.
           02 SV1-S116 PIC X VALUE "*".
           02 SV1-FAMILY PIC X.
           02 SV1-S117 PIC XXX VALUE "***".
           02 SV1-COPAY PIC X.
           02 SV1-END PIC X VALUE "~".
       01  DTP01.
           02 DTP-0 PIC XXX VALUE "DTP".
           02 DTP-S0 PIC X VALUE "*".
           02 DTP-1 PIC XXX.
           02 DTP-S1 PIC X VALUE "*".
           02 DTP-2 PIC XX VALUE "D8".
           02 DTP-S2 PIC X VALUE "*".
           02 DTP-3 PIC X(8).
           02 DTP-END PIC X VALUE "~".
       01  LIN01.
           02 LIN-0 PIC XXX VALUE "LIN".
           02 LIN-S0 PIC X VALUE "*".
           02 LIN-S1 PIC X VALUE "*".
           02 LIN-1 PIC XX VALUE "N4".
           02 LIN-S2 PIC X VALUE "*".
           02 LIN-NDC PIC X(11).
           02 LIN-END PIC X VALUE "~".
       01  CTP01.
           02 CTP-0 PIC XXX VALUE "CTP".
           02 CTP-S012 PIC XXX VALUE "***".
           02 CTP-3 PIC X(8).
           02 CTP-S3 PIC X VALUE "*".
           02 CTP-4 PIC XX.
           02 CTP-S4 PIC X VALUE "*".
           02 CTP-5 PIC XX VALUE "UN".
           02 CPT-END PIC X VALUE "~".

       01  OI01.
           02 OI-0 PIC XX VALUE "OI".
           02 OI-S12 PIC XXX VALUE "***".
           02 OI-3 PIC X VALUE "Y".
           02 OI-S4 PIC X VALUE "*".
           02 OI-4 PIC X VALUE "B".
           02 OI-S56 PIC Xx VALUE "**".
           02 OI-6 PIC X VALUE "Y".
           02 OI-END PIC X VALUE "~".
       01  TEST-DATE.
           05 T-CC  PIC XX.
           05 T-YY  PIC XX.
           05 T-MM  PIC XX.
           05 T-DD  PIC XX.
       01  DISPLAY-DATE.
           05 T-MM PIC XX.
           05 FILLER PIC X VALUE "/".
           05 T-DD PIC XX.
           05 FILLER PIC X VALUE "/".
           05 T-CC PIC XX.
           05 T-YY PIC XX.
       01  ALF14.
           02 ALF14-3 PIC XXX.
           02 ALF14-9 PIC X(9).
           02 FILLER PIC XX.
       01  DATE-X PIC X(8).
       01  TIME-X. 
           02 TIME-HHMM PIC X(4).
           02 FILLER PIC X(4).
       01  FLAG PIC 9.
       01  END-FLAG PIC 9 VALUE 0.
       01  GAP-FLAG PIC 9.
       01  CNTR PIC 99.
       01  DIAG-CNTR PIC 99.
       01  DX-CNTR-PT PIC 99.
       01  X PIC 99.
       01  DIAG-X PIC X(7).
       01  Y PIC 99.
       01  A PIC 99.
       01  B PIC 99.
       01  C PIC 999.
       01  D PIC 999.
       01  DIAG9-ARRAY01.
           02 DIAG9-ARRAY OCCURS 12 TIMES.
              03 DIAG9-BF PIC XXXX.
              03 DIAG9-CODE PIC X(5).
       01  DIAG10-ARRAY01.
           02 DIAG10-ARRAY OCCURS 12 TIMES.
              03 DIAG10-BF PIC XXXXX.
              03 DIAG10-CODE PIC X(7).
              
       01  MOD-ARRAY01.
           02 MOD-ARRAY OCCURS 4 TIMES.
              03 MOD-C PIC X.
              03 MOD-CODE PIC XX.
       01  DIAG-POINTER01.
           02 DIAG-POINTER OCCURS 12 TIMES.
              03 DIAG-PT PIC Z9.
              03 DIAG-C PIC X.
       01  DIAGTAB01.
           02 DIAGTAB PIC X(7) OCCURS 12 TIMES.
       01  FILETAB01.
           02 FILETAB PIC X(156) OCCURS 50 TIMES.
       01  tab11601.
           02 TAB116 PIC X OCCURS 116 TIMES.
       01  ALF116 PIC X(116).
       01 GROUP-3 PIC XXX.
       01 ALF1 PIC X.
       01  ALF-4.
           02 ALF-4-1 PIC XX.
           02 ALF-4-2 PIC XX.
       01 ALF10 PIC X(10).
       01 ALF20 PIC X(20).
       01 ALF5 PIC X(5).
       01 ALF7.
          02 ALF71 PIC XX.
          02 ALF72 PIC X(5).
       01 ALF9 PIC X(9).
       01 ALFS PIC X(5).
       01 ALFS8 PIC X(8).
       01 ALFS9 PIC X(9).
       01 ALF5Z PIC ZZZZZ.
       01 ALF9Z PIC ZZZZZZZZZ.
       01 ALF5NUM PIC X(5).
       01 ALF9NUM PIC X(9).
       01 NUM7 PIC 9(5)V99. 
       01 ALF8 PIC X(8).
       01 ALF8Z PIC ZZZZ9.99.
       01 ALF8NUM PIC X(8).
       01 NUM5 PIC 9(5). 
       01 NUM9 PIC 9(9).
       01 NUM2 PIC 99. 
       01 HL-NUMPARENT PIC 9(5).
       01 HL-NUM PIC 9(5) VALUE 0.
       01 PARM01.
           02 PM-1 PIC XX.
           02 FILLER PIC X.
           02 PM-2 PIC X(10).
           02 FILLER PIC X.
           02 PM-3 PIC X(9).
           02 FILLER PIC X.
           02 PM-4 PIC X(8).
           02 FILLER PIC X.
           02 PM-5 PIC X(31).
           02 FILLER PIC X.
           02 PM-6 PIC X(10).
       01 EINSS PIC X(9).
       01 EINSS-TYPE PIC X.
       01 PARMLAST PIC X(15).
       01 PARMFIRST PIC X(15).
       01 PARMMIDDLE PIC X.
       01 ORG-NAME PIC X(40).
       01 ORG-CITY PIC X(30).
       01 ORG-STATE PIC XX.
       01 ORG-ZIP PIC X(9).
       01 INSTYPE-CODE PIC XXX.
       01 INSGROUP-CODE PIC X(12).
       01 INSGROUP-LEG PIC X(7).
       01  SUBMIT01.
           02 SUBMIT-1 PIC X(8).
           02 SUBMIT-2 PIC XX.
       01 TELE-PHONE PIC X(10).
       01 ORG-STREET PIC X(24).
       01 EIN-CODE PIC X(12).
       01 CONTACT-NAME PIC X(30).
       01  PLACE-TAB01.
           02 PLACE-TAB OCCURS 29 TIMES.
             03 PL-TAB PIC X.
             03 PL-NUM PIC X.
             03 PL-NAME PIC X(22).
             03 PL-STREET PIC X(18).
             03 PL-CITY PIC X(15).
             03 PL-STATE PIC XX.
             03 PL-ZIP PIC X(9).
       01  PLINDX PIC 99 VALUE 0.
       01  CC-PL PIC X.
       01 HL-NUMPRV-SAVE PIC X(5).
       01 HL-SBR-SAVE PIC X(5).
       01 X-RELATE PIC X.
       01 SBR-RELATEHOLD PIC X.
       01 SUB-RELATE PIC X.
       01 SUB-NAME PIC X(24).
       01 SUB-GROUP PIC X(10).
       01 SUB-POLICY PIC X(16).
       01 TOT-AMOUNT PIC 9(4)V99.
       01 PLACE-POINTER PIC 99.
       01 SAVE01 PIC X(156).
       01 X-MOD.
          02 X-MOD1 PIC XX.
          02 X-MOD2 PIC XX.
          02 X-MOD3 PIC XX.
       01 NAME-1 PIC X(24).
       01 NAME-2 PIC X(24).

       01  HOLD-FILEIN01.
           02 HOLD-FILEIN-KEY.
             03 HOLD-KEY8 PIC X(8).
             03 HOLD-KEY3 PIC XXX.
           02 HOLD-PATID.
             03 HOLD-PATID7 PIC X(7).
             03 HOLD-PATID8 PIC X.
           02 HOLD-CLAIM PIC X(6).
           02 HOLD-SERVICE PIC X.
           02 HOLD-DIAG PIC X(7).
           02 HOLD-PROC.
             03 HOLD-PROC1 PIC X(5).
             03 HOLD-PROC2 PIC XX.
           02 HOLD-MOD2 PIC XX.
           02 HOLD-MOD3 PIC XX.
           02 HOLD-MOD4 PIC XX.
           02 HOLD-AMOUNT PIC S9(4)V99.
           02 HOLD-DOCR PIC X(3).
           02 HOLD-DOCP PIC 99.
           02 HOLD-PAYCODE PIC XXX.
           02 HOLD-STUD PIC X.
           02 HOLD-WORK PIC 99.
           02 HOLD-DAT1 PIC X(8).
           02 HOLD-RESULT PIC X.
           02 HOLD-ACTION PIC X.
           02 HOLD-SORCREF PIC X.
           02 HOLD-COLLT PIC X.
           02 HOLD-AGE PIC X.
           02 HOLD-PAPER PIC X.
           02 HOLD-PLACE PIC X.
           02 HOLD-IOPAT PIC X.
           02 HOLD-DATE-T PIC X(8).
           02 HOLD-DATE-A PIC X(8).
           02 HOLD-DATE-P PIC X(8).
           02 HOLD-REC-STAT PIC X.
           02 HOLD-DX2 PIC X(7).
           02 HOLD-DX3 PIC X(7).
           02 HOLD-ACC-TYPE PIC X.
           02 HOLD-DATE-M PIC X(8).
           02 HOLD-ASSIGN PIC X.
           02 HOLD-NEIC-ASSIGN PIC X.
           02 HOLD-DX4 PIC X(7).
           02 HOLD-DX5 PIC X(7).
           02 HOLD-DX6 PIC X(7).
           02 HOLD-FUTURE PIC X(6).



       01  MAMMO-FLAG PIC 9.
       01  CLIA-FLAG PIC 9.
       01  CLIA-NUM PIC X(12).
       01  GROUP-TAX PIC X(10).
       01  AGEX.
           02 AGEXYY PIC 9999.
           02 AGEXMMDD PIC XXXX.
       01  AGEY.
           02 AGEYYY PIC 9999.
           02 AGEYMMDD PIC XXXX.
       01 AGEZ PIC 999.
       01  DOC-TAB01.
           02 DOC-TAB02 OCCURS 90 TIMES.
              03 DOC-TAX PIC X(10).
              03 DOC-SS PIC X(9).
              03 DOC-NUM PIC X(8).
              03 DOC-LASTNAME PIC X(20).
              03 DOC-FIRSTNAME PIC X(10).
              03 DOC-MI PIC X.
              03 DOC-NPI PIC X(10).
       PROCEDURE DIVISION.
       P0. 
           OPEN INPUT FILEIN GARFILE PATFILE INSFILE REFPHY
            AUTHFILE MPLRFILE DIAGFILE PLACEFILE GAPFILE PARMFILE
            PARMFILE2 PROVCAID.
           OPEN OUTPUT SEGFILE ERRFILE.
           OPEN I-O HIPCLAIMFILE
           MOVE "A" TO HIP-KEY
           READ HIPCLAIMFILE WITH LOCK INVALID 
           DISPLAY "BAD HIPCLAIMFILE"
           GO TO P99.
           COMPUTE NUM9 = HIP-NUM
           PERFORM NUM-LEFT9
           MOVE ALF9NUM TO GS-NUM
           MOVE ALF9NUM TO GE-NUM
           ADD 1 TO HIP-NUM
           OPEN I-O CHARCUR.
           PERFORM ISA-1 THRU ISA-EXIT
           PERFORM A0 THRU A0-EXIT.
           MOVE SPACE TO SEGFILE01
      *     WRITE SEGFILE01 FROM ISA01
           ACCEPT TIME-X FROM TIME
           MOVE TIME-HHMM TO BHT-TIME GS-5.
           ACCEPT BHT-DATE FROM CENTURY-DATE.
           MOVE BHT-DATE TO GS-4.
           
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM GS01
           MOVE SPACE TO SEGFILE01
           
           COMPUTE NUM9 = HIP-NUM
           PERFORM NUM-LEFT9
           MOVE ALF9NUM TO ST-NUM
           MOVE ALF9NUM TO SE-NUM
           

           ADD 1 TO HIP-NUM
           WRITE SEGFILE01 FROM ST01.
           
           COMPUTE NUM9 = HIP-NUM
           PERFORM NUM-LEFT9
           MOVE ALF9NUM TO BHT-NUM
           
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM BHT01.
           MOVE "87 " TO REF-CODE
           MOVE "005010X222A1" TO REF-ID
           MOVE SPACE TO SEGFILE01
      *     WRITE SEGFILE01 FROM REF01.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SUBM01.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SUBPER01.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM RECNM101.


       P00. READ PLACEFILE AT END GO TO P000.
           ADD 1 TO PLINDX.
           MOVE DF1 TO PL-TAB(PLINDX)
           MOVE DF2 TO PL-NUM(PLINDX)
           MOVE DF3 TO PL-NAME(PLINDX)
           MOVE DF4 TO PL-STREET(PLINDX)
           MOVE DF5 TO PL-CITY(PLINDX)
           MOVE DF6 TO PL-STATE(PLINDX)
           MOVE DF7 TO PL-ZIP(PLINDX)
           GO TO P00.

       P000.    
           READ FILEIN AT END GO TO P98.
           MOVE FILEIN01 TO HOLD-FILEIN01
           MOVE 0 TO CNTR DIAG-CNTR TOT-AMOUNT MAMMO-FLAG
           PERFORM DF-SEARCH
           PERFORM 2000A THRU 2000B
           GO TO P1-1.
           
       P0000.
           MOVE SAVE01 TO HOLD-FILEIN01.
           PERFORM DF-SEARCH.
           MOVE 0 TO CNTR DIAG-CNTR TOT-AMOUNT MAMMO-FLAG CLIA-FLAG
           GO TO P1-1.
       P1. READ FILEIN AT END MOVE 1 TO END-FLAG GO TO P2.
       P1-1. 
           IF DIAG-CNTR > 11 GO TO P2.
           IF  FI-PLACE = HOLD-PLACE
            AND FI-KEY8 = HOLD-KEY8
            AND FI-PATID = HOLD-PATID
            AND FI-DOCP = HOLD-DOCP
            AND FI-DOCR = HOLD-DOCR
            AND FI-DAT1 = HOLD-DAT1
            AND FI-DATE-T = HOLD-DATE-T
            AND FI-ACC-TYPE = HOLD-ACC-TYPE
            AND CNTR < 50
            PERFORM DIAG-1 THRU DIAG-EXIT 
             IF DIAG-CNTR > 12
               GO TO P2
             END-IF
            IF FI-SERVICE = "4"
               MOVE 1 TO CLIA-FLAG
            END-IF
            ADD 1 TO CNTR 
            MOVE FILEIN01 TO FILETAB(CNTR)
            ADD FI-AMOUNT TO TOT-AMOUNT
            GO TO P1
           END-IF.
       P2.  
            MOVE FILEIN01 TO SAVE01
            PERFORM 2300CLM
            PERFORM HI-DIAG THRU HI-DIAG-EXIT.
            PERFORM 2310A THRU 2310A-EXIT.
      *      IF EINSS-TYPE = "E" PERFORM 2310B. 
            PERFORM 2310D 
            PERFORM 2320A THRU 2320A-EXIT
            PERFORM 2400SRV THRU 2400SRV-EXIT
                   VARYING X FROM 1 BY 1 UNTIL X > CNTR
           IF END-FLAG = 1 GO TO P98.
           MOVE SAVE01 TO FILEIN01
           IF FI-DOCP NOT = HOLD-DOCP 
           MOVE FILEIN01 TO HOLD-FILEIN01

           PERFORM DOCP-1.
           MOVE FILEIN01 TO HOLD-FILEIN01
           PERFORM 2000B 
           GO TO P0000.
           
       DIAG-1.
           IF FI-DIAG = "0000000"  GO TO DIAG-EXIT.
           MOVE FI-DIAG TO DIAG-X
           MOVE 0 TO FLAG
           PERFORM DIAG-2 VARYING X FROM 1 BY 1 UNTIL X > DIAG-CNTR
           IF FLAG = 0
           ADD 1 TO DIAG-CNTR
           MOVE FI-DIAG TO DIAGTAB(DIAG-CNTR).

           IF FI-DX2 = "0000000"  GO TO DIAG-EXIT.
           MOVE FI-DX2 TO DIAG-X.
           MOVE 0 TO FLAG
           PERFORM DIAG-2 VARYING X FROM 1 BY 1 UNTIL X > DIAG-CNTR
           IF FLAG = 0
           ADD 1 TO DIAG-CNTR
           MOVE FI-DX2 TO DIAGTAB(DIAG-CNTR).

           IF FI-DX3 = "0000000"  GO TO DIAG-EXIT.
           MOVE FI-DX3 TO DIAG-X
           MOVE 0 TO FLAG
           PERFORM DIAG-2 VARYING X FROM 1 BY 1 UNTIL X > DIAG-CNTR
           IF FLAG = 0
           ADD 1 TO DIAG-CNTR
           MOVE FI-DX3 TO DIAGTAB(DIAG-CNTR).
           IF FI-DX4 = "0000000"  GO TO DIAG-EXIT.
           MOVE FI-DX4 TO DIAG-X
           MOVE 0 TO FLAG
           PERFORM DIAG-2 VARYING X FROM 1 BY 1 UNTIL X > DIAG-CNTR
           IF FLAG = 0
           ADD 1 TO DIAG-CNTR
           MOVE FI-DX4 TO DIAGTAB(DIAG-CNTR).
      *     IF FI-DX5 = "0000000"  GO TO DIAG-EXIT.
      *     MOVE FI-DX5 TO DIAG-X
      *     MOVE 0 TO FLAG
      *     PERFORM DIAG-2 VARYING X FROM 1 BY 1 UNTIL X > DIAG-CNTR
      *     IF FLAG = 0
      *     ADD 1 TO DIAG-CNTR
      *     MOVE FI-DX5 TO DIAGTAB(DIAG-CNTR).
      *     IF FI-DX6 = "0000000"  GO TO DIAG-EXIT.
      *     MOVE FI-DX6 TO DIAG-X
      *     MOVE 0 TO FLAG
      *     PERFORM DIAG-2 VARYING X FROM 1 BY 1 UNTIL X > DIAG-CNTR
      *     IF FLAG = 0
      *     ADD 1 TO DIAG-CNTR
      *     MOVE FI-DX6 TO DIAGTAB(DIAG-CNTR).
       DIAG-EXIT.
           EXIT.
       DIAG-2. IF DIAGTAB(X) = DIAG-X
           MOVE DIAG-CNTR TO X
           MOVE 1 TO FLAG.

       2000A. 
           ADD 1 TO HL-NUM
           MOVE HL-NUM TO HL-NUMPRV-SAVE
           COMPUTE NUM5 = HL-NUM
           PERFORM NUM-LEFT
           MOVE ALF5NUM TO HL-NUMX
           MOVE SPACE TO HL-PARENT
           MOVE "20  " TO HL-CODE
           MOVE "1" TO HL-CHILD
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM HL01
      *     IF EINSS-TYPE = "S"
           MOVE "BI" TO PRV-1
           MOVE GROUP-TAX TO PRV-TAX
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM PRV01.
           PERFORM DOCP-1.

      *  BILLING PROVIDER/ADDRESS
       2010AA.
           MOVE "85 " TO NM1-1
           MOVE "1" TO NM1-SOLO
           MOVE "34" TO NM1-EINSS
           MOVE SPACE TO NM1-CODE NM1-NAMEL NM1-NAMEF
           NM1-NAMEM NM1-NAMES
           MOVE DOC-LASTNAME(HOLD-DOCP) TO NM1-NAMEL
           MOVE DOC-FIRSTNAME(HOLD-DOCP) TO NM1-NAMEF
           MOVE DOC-MI(HOLD-DOCP) TO NM1-NAMEM
           MOVE DOC-SS(HOLD-DOCP) TO NM1-CODE
           MOVE SPACE TO SEGFILE01
      *     WRITE SEGFILE01 FROM NM101.
      *     PERFORM PLACE-OF-SERVICE
           MOVE SPACE TO N3-STREET N3-BILLADD
           MOVE PL-STREET(PLACE-POINTER) TO N3-STREET
           MOVE SPACE TO SEGFILE01
      *     WRITE SEGFILE01 FROM N301
           MOVE SPACE TO N4-CITY N4-STATE N4-ZIP
           MOVE PL-CITY(PLACE-POINTER) TO N4-CITY
           MOVE PL-STATE(PLACE-POINTER) TO N4-STATE
           MOVE PL-ZIP(PLACE-POINTER) TO N4-ZIP
           IF N4-ZIP(6:4) = SPACE
            MOVE "9999" TO N4-ZIP(6:4)
           END-IF
           MOVE SPACE TO SEGFILE01
      *     WRITE SEGFILE01 FROM N401.
           MOVE DOC-NUM(HOLD-DOCP) TO REF-ID
           MOVE INSTYPE-CODE TO REF-CODE
           MOVE SPACE TO SEGFILE01.
      *     WRITE SEGFILE01 FROM REF01.

           MOVE CONTACT-NAME TO PER-CONTACT
           MOVE TELE-PHONE TO PER-PHONE
           MOVE SPACE TO SEGFILE01.
      *     WRITE SEGFILE01 FROM PER01.

      *   PAY-TO PROVIDER/ADDRESS
       2010AB.    
           IF EINSS-TYPE = "E"
           MOVE "2" TO NM1-SOLO
           MOVE ORG-NAME TO NM1-NAMEL
           MOVE SPACE TO NM1-NAMEF NM1-NAMEM NM1-NAMES
           MOVE "24" TO NM1-EINSS
           ELSE 
           MOVE "1" TO NM1-SOLO
           MOVE PARMLAST TO NM1-NAMEL
           MOVE PARMFIRST TO NM1-NAMEF
           MOVE PARMMIDDLE TO NM1-NAMEM
           MOVE "34" TO NM1-EINSS.
           MOVE "XX" TO NM1-EINSS.
           MOVE "85" TO NM1-1
           MOVE SPACE TO NM1-CODE
           MOVE INSGROUP-CODE TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101
           MOVE SPACE TO N3-STREET N3-BILLADD
           MOVE ORG-STREET TO N3-STREET
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N301
           MOVE SPACE TO N4-CITY N4-STATE N4-ZIP
           MOVE ORG-CITY TO N4-CITY
           MOVE ORG-STATE TO N4-STATE
           MOVE ORG-ZIP TO N4-ZIP.
           IF N4-ZIP(6:4) = SPACE
            MOVE "9999" TO N4-ZIP(6:4)
           END-IF.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N401
           MOVE SPACE TO REF-CODE REF-ID
           MOVE INSTYPE-CODE TO REF-CODE
           MOVE INSGROUP-LEG TO REF-ID
           MOVE SPACE TO SEGFILE01.
      *     WRITE SEGFILE01 FROM REF01.
           MOVE SPACE TO REF-CODE REF-ID
           IF EINSS-TYPE = "E"
            MOVE "EI" TO REF-CODE
           ELSE
            MOVE "SY" TO REF-CODE
           END-IF
           MOVE EIN-CODE TO REF-ID
           MOVE SPACE TO SEGFILE01.
           WRITE SEGFILE01 FROM REF01.

       2000B.
           ADD 1 TO HL-NUM
           COMPUTE NUM5 = HL-NUM
           PERFORM NUM-LEFT
           MOVE ALF5NUM TO HL-NUMX
           MOVE HL-NUMPRV-SAVE TO NUM5
           PERFORM NUM-LEFT
           MOVE ALF5NUM TO HL-PARENT
           MOVE "22  " TO HL-CODE
           PERFORM SUBSCRIBER-1 THRU SUBSCRIBER-EXIT.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM HL01
           MOVE SPACE TO SEGFILE01
      *     MOVE "PE" TO PRV-1
      *     MOVE DOC-TAX(HOLD-DOCP) TO PRV-TAX
      *     WRITE SEGFILE01 FROM PRV01
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SBR01
           PERFORM 2010BA.
           PERFORM 2010BB.

       2010BA.
           MOVE "IL " TO NM1-1
           MOVE "1" TO NM1-SOLO
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES
           UNSTRING G-GARNAME DELIMITED BY "; " OR ";" INTO
           NM1-NAMEL NM1-NAMEF NM1-NAMEM
           MOVE SPACE TO NAME-1 NAME-2
           UNSTRING NM1-NAMEL DELIMITED BY " " INTO NAME-1 NAME-2
           IF NAME-2 = "JR" OR "SR" OR "II" OR "III"
           MOVE SPACE TO NM1-NAMEL NM1-NAMES
           MOVE NAME-1 TO NM1-NAMEL
           MOVE NAME-2 TO NM1-NAMES.
           IF NM1-NAMEM = SPACE
           MOVE SPACE TO NAME-1 NAME-2
           UNSTRING NM1-NAMEF DELIMITED BY " " INTO NAME-1 NAME-2
            IF NAME-2 NOT = SPACE
             MOVE SPACE TO NM1-NAMEF NM1-NAMEM
             MOVE NAME-1 TO NM1-NAMEF
             MOVE NAME-2 TO NM1-NAMEM
            END-IF
           END-IF.
           MOVE SPACE TO NM1-CODE
           MOVE G-PRIPOL TO NM1-CODE
           MOVE "MI" TO NM1-EINSS
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
           MOVE SPACE TO N3-STREET N3-BILLADD
           MOVE G-BILLADD TO N3-STREET
           MOVE G-STREET TO N3-BILLADD
           IF G-BILLADD = SPACE
           MOVE G-STREET TO N3-STREET
           MOVE SPACE TO N3-BILLADD.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N301.
           MOVE SPACE TO N4-CITY N4-STATE N4-ZIP
           MOVE G-CITY TO N4-CITY
           MOVE G-STATE TO N4-STATE
           MOVE G-ZIP TO N4-ZIP
           IF N4-ZIP(6:4) = SPACE
            MOVE "9999" TO N4-ZIP(6:4)
           END-IF.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N401.
           MOVE G-DOB TO DMG-DOB
           MOVE G-SEX TO DMG-GENDER
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM DMG01.
       2010BB.
           MOVE "PR " TO NM1-1
           MOVE "2" TO NM1-SOLO
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES
           MOVE "VT MEDICAID" TO NM1-NAMEL
           MOVE "PI" TO NM1-EINSS
           MOVE "822287119" TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
           MOVE SPACE TO N3-STREET N3-BILLADD
           MOVE "PO BOX 888" TO N3-STREET
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N301.
           MOVE SPACE TO N4-CITY N4-STATE N4-ZIP
           MOVE "WILLISTON" TO N4-CITY
           MOVE "VT" TO N4-STATE
           MOVE "05495" TO N4-ZIP
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N401.
       2300CLM.
           MOVE HOLD-KEY8 TO SUBMIT-1
           MOVE SUBMIT01 TO CLM-1
           COMPUTE NUM7 = TOT-AMOUNT
           PERFORM AMT-LEFT
           MOVE ALF8NUM TO CLM-2
           MOVE SPACE TO CLM-11 CLM-COLON-ACCIDENT
           IF HOLD-DAT1 NOT = ZEROES
           PERFORM ACCIDENT-1 THRU ACCIDENT-EXIT.
           MOVE SPACE TO SEGFILE01
           MOVE SPACE TO CLM-12
           IF NOT (HOLD-PAYCODE = "004" OR "064")
            MOVE "03 " TO CLM-12.
           WRITE SEGFILE01 FROM CLM01.
           IF HOLD-DAT1 NOT = ZEROES
           MOVE "439" TO DTP-1
           MOVE HOLD-DAT1 TO DTP-3
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM DTP01.
           IF (CLM-5 = "21")
            MOVE "435" TO DTP-1
             IF HOLD-DATE-M = "00000000"
             MOVE "17760704" TO HOLD-DATE-M
             END-IF
            MOVE HOLD-DATE-M TO DTP-3
            MOVE SPACE TO SEGFILE01
            WRITE SEGFILE01 FROM DTP01
           END-IF.
           IF MAMMO-FLAG= 1
           MOVE SPACE TO REF-CODE REF-ID SEGFILE01
           MOVE "EW" TO REF-CODE
           MOVE "134668    " TO REF-ID
           WRITE SEGFILE01 FROM REF01.
           IF CLIA-FLAG = 1
           AND (CLIA-NUM NOT = SPACES)
           MOVE "X4" TO REF-CODE
           MOVE SPACE TO REF-ID
           MOVE CLIA-NUM TO REF-ID
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM REF01.


       ACCIDENT-1.
           MOVE "OA" TO CLM-11.
      *     MOVE ":" TO CLM-COLON-ACCIDENT.
       ACCIDENT-EXIT.
           EXIT.



       2310D.
           IF HOLD-PLACE NOT = "2"
           MOVE "77 " TO NM1-1
            IF HOLD-PLACE = "4"
             MOVE "IL " TO NM1-1
            END-IF
           MOVE "2" TO NM1-SOLO
           MOVE PL-NAME(PLACE-POINTER) TO NM1-NAMEL
           MOVE SPACE TO NM1-NAMEF NM1-NAMEM NM1-NAMES
           MOVE "  " TO NM1-EINSS
           MOVE SPACE TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101
           MOVE SPACE TO N3-STREET N3-BILLADD
           MOVE PL-STREET(PLACE-POINTER) TO N3-STREET
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N301
           MOVE SPACE TO N4-CITY N4-STATE N4-ZIP
           MOVE PL-CITY(PLACE-POINTER) TO N4-CITY
           MOVE PL-STATE(PLACE-POINTER) TO N4-STATE
           MOVE PL-ZIP(PLACE-POINTER) TO N4-ZIP
           IF N4-ZIP(6:4) = SPACE
            MOVE "9999" TO N4-ZIP(6:4)
           END-IF
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N401.
       2320A.
           GO TO 2320A-EXIT.
           IF G-SEINS = "001" OR "012" OR "075" OR "076"
           GO TO 2320A-EXIT.
           IF G-SEINS = "005" PERFORM CMP-1 
           GO TO 2320A-EXIT.
           IF G-SEINS = "004" OR "064" PERFORM CAID-1 
           GO TO 2320A-EXIT.
           MOVE 0 TO GAP-FLAG
           IF G-SEINS = "062" PERFORM GAP-1 THRU GAP-1-EXIT. 
       2320A-EXIT. EXIT.
       CMP-1.
           MOVE "S" TO SBR-PST
           MOVE G-SE-GROUP TO SBR-GROUP
           MOVE "18" TO SBR-RELATE
           MOVE "SP" TO SBR-TYPE.
           MOVE SPACE TO SBR-6 SBR-7 SBR-8
           MOVE "MB" TO SBR-INSCODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SBR01.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM OI01.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM DMG01.
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES 
           UNSTRING G-SENAME DELIMITED BY ";" INTO
           NM1-NAMEL NM1-NAMEF NM1-NAMEM
           MOVE SPACE TO NAME-1 NAME-2
           UNSTRING NM1-NAMEL DELIMITED BY " " INTO NAME-1 NAME-2
           IF NAME-2 = "JR" OR "SR" OR "II" OR "III"
           MOVE SPACE TO NM1-NAMEL NM1-NAMES
           MOVE NAME-1 TO NM1-NAMEL
           MOVE NAME-2 TO NM1-NAMES.
           IF NM1-NAMEM = SPACE
           MOVE SPACE TO NAME-1 NAME-2
           UNSTRING NM1-NAMEF DELIMITED BY ALL " " INTO NAME-1 NAME-2
            IF NAME-2 NOT = SPACE
             MOVE SPACE TO NM1-NAMEF NM1-NAMEM
             MOVE NAME-1 TO NM1-NAMEF
             MOVE NAME-2 TO NM1-NAMEM
            END-IF
           END-IF.

           MOVE "1" TO NM1-SOLO
           MOVE "IL" TO NM1-1
           MOVE "MI" TO NM1-EINSS
           MOVE SPACE TO NM1-CODE
           MOVE G-SECPOL TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
           MOVE SPACE TO N3-STREET N3-BILLADD
           MOVE G-BILLADD TO N3-STREET
           MOVE G-STREET TO N3-BILLADD
           IF G-BILLADD = SPACE
           MOVE G-STREET TO N3-STREET
           MOVE SPACE TO N3-BILLADD.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N301.
           MOVE SPACE TO N4-CITY N4-STATE N4-ZIP
           MOVE G-CITY TO N4-CITY
           MOVE G-STATE TO N4-STATE
           MOVE G-ZIP TO N4-ZIP
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N401.
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES 
           MOVE "MEDICOMP" TO NM1-NAMEL
           MOVE "2" TO NM1-SOLO
           MOVE "PR" TO NM1-1
           MOVE "PI" TO NM1-EINSS
           MOVE SPACE TO NM1-CODE
           MOVE "00026" TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
       
       CAID-1.
           MOVE "S" TO SBR-PST
           MOVE G-SE-GROUP TO SBR-GROUP
           MOVE "18" TO SBR-RELATE
           MOVE "MC" TO SBR-TYPE.
           MOVE SPACE TO SBR-6 SBR-7 SBR-8
           MOVE "MC" TO SBR-INSCODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SBR01.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM OI01.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM DMG01.
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES 
           UNSTRING G-SENAME DELIMITED BY ";" INTO
           NM1-NAMEL NM1-NAMEF NM1-NAMEM
           MOVE SPACE TO NAME-1 NAME-2
           UNSTRING NM1-NAMEL DELIMITED BY " " INTO NAME-1 NAME-2
           IF NAME-2 = "JR" OR "SR" OR "II" OR "III"
           MOVE SPACE TO NM1-NAMEL NM1-NAMES
           MOVE NAME-1 TO NM1-NAMEL
           MOVE NAME-2 TO NM1-NAMES.
           IF NM1-NAMEM = SPACE
           MOVE SPACE TO NAME-1 NAME-2
           UNSTRING NM1-NAMEF DELIMITED BY ALL " " INTO NAME-1 NAME-2
            IF NAME-2 NOT = SPACE
             MOVE SPACE TO NM1-NAMEF NM1-NAMEM
             MOVE NAME-1 TO NM1-NAMEF
             MOVE NAME-2 TO NM1-NAMEM
            END-IF
           END-IF.

           MOVE "1" TO NM1-SOLO
           MOVE "IL" TO NM1-1
           MOVE "MI" TO NM1-EINSS
           MOVE SPACE TO NM1-CODE
           MOVE G-SECPOL TO ALF9
           MOVE ALF9 TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
           MOVE SPACE TO N3-STREET N3-BILLADD
           MOVE G-BILLADD TO N3-STREET
           MOVE G-STREET TO N3-BILLADD
           IF G-BILLADD = SPACE
           MOVE G-STREET TO N3-STREET
           MOVE SPACE TO N3-BILLADD.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N301.
           MOVE SPACE TO N4-CITY N4-STATE N4-ZIP
           MOVE G-CITY TO N4-CITY
           MOVE G-STATE TO N4-STATE
           MOVE G-ZIP TO N4-ZIP
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N401.
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES 
           MOVE "VT MEDICAID" TO NM1-NAMEL
           MOVE "2" TO NM1-SOLO
           MOVE "PR" TO NM1-1
           MOVE "PI" TO NM1-EINSS
           MOVE SPACE TO NM1-CODE
           MOVE "VTXIX" TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
       GAP-1. 
           MOVE G-PR-GROUP TO GAPKEY
           READ GAPFILE INVALID MOVE 1 TO GAP-FLAG GO TO GAP-1-EXIT.
           MOVE "S" TO SBR-PST
           MOVE G-SE-GROUP TO SBR-GROUP
           IF G-RELATE = G-SE-RELATE
           MOVE "18" TO SBR-RELATE
           ELSE MOVE "01" TO SBR-RELATE.
           IF GAP-TYPE = "X"
           MOVE "MI" TO SBR-TYPE
           ELSE MOVE "SP" TO SBR-TYPE.
           MOVE SPACE TO SBR-6 SBR-7 SBR-8
           MOVE "CI" TO SBR-INSCODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SBR01.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM OI01.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM DMG01.
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES 
           UNSTRING G-SENAME DELIMITED BY ";" INTO
           NM1-NAMEL NM1-NAMEF NM1-NAMEM
           MOVE SPACE TO NAME-1 NAME-2
           UNSTRING NM1-NAMEL DELIMITED BY " " INTO NAME-1 NAME-2
           IF NAME-2 = "JR" OR "SR" OR "II" OR "III" OR "IV"
           MOVE SPACE TO NM1-NAMEL NM1-NAMES
           MOVE NAME-1 TO NM1-NAMEL
           MOVE NAME-2 TO NM1-NAMES.
           IF NM1-NAMEM = SPACE
           MOVE SPACE TO NAME-1 NAME-2
           UNSTRING NM1-NAMEF DELIMITED BY ALL " " INTO NAME-1 NAME-2
            IF NAME-2 NOT = SPACE
             MOVE SPACE TO NM1-NAMEF NM1-NAMEM
             MOVE NAME-1 TO NM1-NAMEF
             MOVE NAME-2 TO NM1-NAMEM
            END-IF
           END-IF.
           MOVE "1" TO NM1-SOLO
           MOVE "IL" TO NM1-1
           MOVE "MI" TO NM1-EINSS
           MOVE SPACE TO NM1-CODE
           MOVE G-SECPOL TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
           MOVE SPACE TO N3-STREET N3-BILLADD
           MOVE G-BILLADD TO N3-STREET
           MOVE G-STREET TO N3-BILLADD
           IF G-BILLADD = SPACE
           MOVE G-STREET TO N3-STREET
           MOVE SPACE TO N3-BILLADD.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N301.
           MOVE SPACE TO N4-CITY N4-STATE N4-ZIP
           MOVE G-CITY TO N4-CITY
           MOVE G-STATE TO N4-STATE
           MOVE G-ZIP TO N4-ZIP
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N401.
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES 
           MOVE GAP-NAME TO NM1-NAMEL
           MOVE "2" TO NM1-SOLO
           MOVE "PR" TO NM1-1
           MOVE "PI" TO NM1-EINSS
           MOVE SPACE TO NM1-CODE
           MOVE GAPKEY TO ALF7
           MOVE ALF72 TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
       GAP-1-EXIT. EXIT.
       2400SRV.
           MOVE FILETAB(X) TO FILEIN01
           MOVE FI-PROC1 TO SV1-PROC.
           MOVE SPACE TO SV1-EPSDT
           IF ((FI-PROC1 = "W9020")
           OR (FI-PROC1 > "99380" AND < "99387")
           OR (FI-PROC1 > "99390" AND < "99397"))
           MOVE G-DOB TO AGEX  
           MOVE FI-DATE-T TO AGEY
           COMPUTE AGEZ = AGEYYY - AGEXYY
            IF AGEZ = 21 AND AGEXMMDD > AGEYMMDD
             COMPUTE AGEZ = 20
            END-IF
            IF AGEZ < 21
           MOVE "Y" TO SV1-EPSDT
           MOVE "EP" TO FI-PROC2
            END-IF
           END-IF.
           MOVE SPACE TO SV1-FAMILY

           IF ((HOLD-DATE-T < "20151001")
           AND  (CC-DIAG > "V25  " AND < "V2700"))
           OR ((CC-DATE-T > "20150930")
           AND (CC-DIAG > "Z2999  " AND < "Z3200  "))

           OR (CC-PROC > "58599ZZ" AND < "58620  ")
           OR (CC-PROC = "90699ZM" OR "99070ZM" OR "90040ZM")
           OR (CC-PROC = "58982  " OR "58983  " OR "58984  ")
           MOVE "Y" TO SV1-FAMILY.
           IF NOT (CC-PAYCODE = 004 OR 064)
           MOVE SPACE TO SV1-FAMILY.
           PERFORM SV-MOD
           COMPUTE NUM7 = FI-AMOUNT
           PERFORM AMT-LEFT
           MOVE ALF8NUM TO SV1-AMT
           COMPUTE NUM5 = FI-WORK
           PERFORM NUM-LEFT
           MOVE ALF5NUM TO SV1-WORK
           MOVE SPACE TO SV1-PLACE

           MOVE 0 TO DX-CNTR-PT
           MOVE SPACE TO DIAG-POINTER01
           MOVE FI-DIAG TO DIAG-X
           PERFORM DIAG-3 VARYING A FROM 1 BY 1 UNTIL A > 12
           MOVE FI-DX2 TO DIAG-X
           PERFORM DIAG-3 VARYING A FROM 1 BY 1 UNTIL A > 12
           MOVE FI-DX3 TO DIAG-X
           PERFORM DIAG-3 VARYING A FROM 1 BY 1 UNTIL A > 12
           MOVE FI-DX4 TO DIAG-X
           PERFORM DIAG-3 VARYING A FROM 1 BY 1 UNTIL A > 12
      *     MOVE FI-DX5 TO DIAG-X
      *     PERFORM DIAG-3 VARYING A FROM 1 BY 1 UNTIL A > 12
      *     MOVE FI-DX6 TO DIAG-X
      *     PERFORM DIAG-3 VARYING A FROM 1 BY 1 UNTIL A > 12



           MOVE SPACE TO DIAG-C(DX-CNTR-PT)
           MOVE DIAG-POINTER01 TO SV1-PT
           GO TO SV1-0.

       DIAG-3.
           IF DIAGTAB(A) = DIAG-X
           ADD 1 TO DX-CNTR-PT
           MOVE A TO DIAG-PT(DX-CNTR-PT) 
           MOVE ":" TO DIAG-C(DX-CNTR-PT)
           MOVE 12 TO A.

       SV1-0..
           MOVE " " TO SV1-EMER
           IF CLM-5 = "23" MOVE "Y" TO SV1-EMER.
           COMPUTE NUM5 = X
           PERFORM NUM-LEFT
           MOVE ALF5NUM TO LX-1
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM LX01.
           MOVE SPACE TO SV1-COPAY.
       2400srv-1.
           MOVE SPACE TO tab11601
           MOVE 0 TO D
           PERFORM VARYING C FROM 1 BY 1 UNTIL C > 116
            IF  SV101(C:1) NOT = " "
              ADD 1 TO D
              MOVE SV101(C:1) TO TAB116(D)
            END-IF
           END-PERFORM
           MOVE tab11601 TO ALF116
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM ALF116.
           MOVE "472" TO DTP-1
           MOVE FI-DATE-T TO DTP-3
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM DTP01.
           PERFORM 2410 THRU 2410-EXIT.
           PERFORM 2420A.
           MOVE FILEIN-KEY TO CHARCUR-KEY
           READ CHARCUR WITH LOCK INVALID GO TO 2400SRV-EXIT.
           IF CC-REC-STAT = "0" MOVE "2" TO CC-REC-STAT.
           IF CC-REC-STAT = "1" MOVE "3" TO CC-REC-STAT.
           MOVE BHT-DATE TO CC-DATE-A.
           REWRITE CHARCUR01.
       2400SRV-EXIT.  EXIT.

       2410.
           MOVE FI-KEY8 TO AUTH-KEY8
           MOVE FI-CLAIM TO AUTH-KEY6
           READ AUTHFILE INVALID GO TO 2410-EXIT.
           IF AUTH-NDC NOT NUMERIC GO TO 2410-EXIT.
           MOVE AUTH-NDC TO LIN-NDC
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM LIN01.
           MOVE FI-WORK TO NUM2
           COMPUTE NUM7= FI-AMOUNT / NUM2
           PERFORM AMT-LEFT
      *     MOVE ALF8NUM TO CTP-3
           MOVE SPACE TO CTP-3
           COMPUTE NUM5= NUM2
           PERFORM NUM-LEFT
           MOVE ALF5NUM TO CTP-4
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM CTP01.

       2410-EXIT.
           EXIT.

       2420A.
           MOVE "82 " TO NM1-1
           MOVE "1" TO NM1-SOLO
           MOVE "XX" TO NM1-EINSS
           MOVE SPACE TO NM1-CODE NM1-NAMEL NM1-NAMEF
           NM1-NAMEM NM1-NAMES
           MOVE DOC-LASTNAME(FI-DOCP) TO NM1-NAMEL
           MOVE DOC-FIRSTNAME(FI-DOCP) TO NM1-NAMEF
           MOVE DOC-MI(FI-DOCP) TO NM1-NAMEM
           MOVE DOC-NPI(FI-DOCP) TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
           MOVE "PE" TO PRV-1
           MOVE DOC-TAX(FI-DOCP) TO PRV-TAX
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM PRV01.          

       2310A.
           IF HOLD-DOCR = "000" GO TO REF-2.
           MOVE HOLD-DOCR TO REF-KEY
           READ REFPHY INVALID GO TO REF-2.
           MOVE "DN " TO NM1-1
           MOVE "1" TO NM1-SOLO
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM
           UNSTRING REF-NAME DELIMITED BY ", " OR " ,"
           OR " , " OR "," OR ";" INTO NM1-NAMEL NM1-NAMEF
           MOVE SPACE TO NM1-NAMES NM1-EINSS NM1-CODE
           MOVE "XX" TO NM1-EINSS
           MOVE REF-NPI TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
           MOVE REF-CDNUM TO PROV-KEY
           READ PROVCAID INVALID MOVE "207Q00000X" TO PROV-TAX.
           MOVE "PE" TO PRV-1
           MOVE "PXC" TO PRV-2
           MOVE PROV-TAX TO PRV-TAX
           MOVE SPACE TO SEGFILE01.
      *     WRITE SEGFILE01 FROM PRV01.
      *     GO TO 2310A-EXIT.
       REF-2.
           IF INSGROUP-CODE NOT = DOC-NPI(HOLD-DOCP)
           MOVE "82" TO NM1-1
           MOVE "1" TO NM1-SOLO
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM
           MOVE SPACE TO NM1-NAMES NM1-EINSS NM1-CODE
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM
           MOVE DOC-LASTNAME(HOLD-DOCP) TO NM1-NAMEL
           MOVE DOC-FIRSTNAME(HOLD-DOCP) TO NM1-NAMEF
           MOVE DOC-MI(HOLD-DOCP) TO NM1-NAMEM
           MOVE "XX" TO NM1-EINSS
           MOVE DOC-NPI(HOLD-DOCP) TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101
           MOVE "PE" TO PRV-1
           MOVE "PXC" TO PRV-2
           MOVE DOC-TAX(HOLD-DOCP) TO PRV-TAX
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM PRV01.

       2310A-EXIT.
           EXIT.

       2310B.
           MOVE "82 " TO NM1-1
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM
           MOVE INSGROUP-CODE TO NM1-CODE
           MOVE ORG-NAME TO NM1-NAMEL
           MOVE "2" TO NM1-SOLO
           MOVE "XX" TO NM1-EINSS
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101
           MOVE "PE" TO PRV-1
           MOVE "PXC" TO PRV-2
           MOVE GROUP-TAX TO PRV-TAX
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM PRV01.

       DOCP-1.
           MOVE DOC-LASTNAME(HOLD-DOCP) TO SAVE-DOCNM1-NAMEL 
           MOVE DOC-FIRSTNAME(HOLD-DOCP) TO SAVE-DOCNM1-NAMEF 
           MOVE DOC-MI(HOLD-DOCP) TO SAVE-DOCNM1-NAMES 
           MOVE DOC-NPI(HOLD-DOCP) TO SAVE-DOCNM1-CODE
           MOVE DOC-TAX(HOLD-DOCP) TO SAVE-PRV-TAX
           MOVE "82 " TO SAVE-DOCNM1-1
           MOVE "1" TO SAVE-DOCNM1-SOLO
           MOVE SPACE TO SAVE-DOCNM1-NAMES 
           MOVE "XX" TO SAVE-DOCNM1-EINSS
           IF EINSS-TYPE = "E"
            MOVE "EI " TO SAVE-DOCREF-CODE
           ELSE
            MOVE "SY" TO SAVE-DOCREF-CODE
           END-IF
           MOVE EIN-CODE TO SAVE-DOCREF-ID.


       SUBSCRIBER-1.
           MOVE HOLD-KEY8 TO G-GARNO
           READ GARFILE INVALID DISPLAY "BAD BAD BAD"
           GO TO P99.
           IF G-TRINS NOT = "001"
           MOVE "0" TO MPLR-TR-RELATE
           MOVE G-GARNO TO MPLR-KEY
           READ MPLRFILE INVALID CONTINUE
           END-READ
           END-IF.
           MOVE G-RELATE TO SUB-RELATE
           MOVE G-GARNAME TO SUB-NAME
           MOVE G-PRIPOL TO SUB-POLICY
           MOVE SPACE TO SUB-GROUP
           MOVE "P" TO SBR-PST
           IF HOLD-PAYCODE = G-SEINS 
                MOVE "S" TO SBR-PST
                MOVE G-SE-RELATE TO SUB-RELATE
                MOVE G-SENAME TO SUB-NAME
                MOVE G-SECPOL TO SUB-POLICY
                MOVE G-SE-GROUP TO SUB-GROUP.
           IF (HOLD-PAYCODE = G-TRINS) AND (MPLR-TR-RELATE NOT = "0")
                MOVE "T" TO SBR-PST
                MOVE MPLR-TR-RELATE TO SUB-RELATE
                MOVE MPLR-TR-NAME TO SUB-NAME
                MOVE MPLR-TRIPOL TO SUB-POLICY
                MOVE MPLR-TR-GROUP TO SUB-GROUP.
           MOVE G-RELATE TO X-RELATE.
           IF HOLD-PATID8 = "P" PERFORM PAT-READ.
           IF X-RELATE = "0"  
                MOVE G-RELATE TO X-RELATE.
           IF X-RELATE = SUB-RELATE  
              MOVE "18" TO SBR-RELATE
              GO TO SUBSCRIBER-2.
           IF (X-RELATE = "2" OR "K") 
              AND (SUB-RELATE = "2" OR "K")
           MOVE "01" TO SBR-RELATEHOLD GO TO SUBSCRIBER-2.
           IF (X-RELATE = "8" OR "Q") MOVE "29" TO SBR-RELATEHOLD
           GO TO SUBSCRIBER-2.
           IF (X-RELATE = "4" OR "M") MOVE "02" TO SBR-RELATEHOLD
           GO TO SUBSCRIBER-2.
           IF (X-RELATE = "5" OR "N") MOVE "17" TO SBR-RELATEHOLD
           GO TO SUBSCRIBER-2.
       SUBSCRIBER-2.
           MOVE SPACE TO SBR-GROUP
           MOVE "0    " TO HL-CHILD
           MOVE "MC" TO SBR-INSCODE
           IF SBR-PST = "S"
             IF G-PRINS = "091"
               MOVE "12" TO SBR-TYPE
                IF HOLD-ACC-TYPE = "2"
                MOVE "14" TO SBR-TYPE
                END-IF
                IF HOLD-ACC-TYPE = "1"
                MOVE "15" TO SBR-TYPE
                END-IF
             END-IF
           END-IF.
           MOVE SPACE TO SBR-TYPE.

       SUBSCRIBER-EXIT. EXIT.
       2000B-PAT.
           

       PAT-READ. 
           MOVE HOLD-PATID TO P-PATNO
           READ PATFILE INVALID MOVE "0" TO P-RELATE.
           MOVE P-RELATE TO X-RELATE.
       NUM-LEFT.
           MOVE NUM5 TO ALF5Z ALFS
           MOVE SPACE TO ALF5NUM
           MOVE ALF5Z TO ALF5
           UNSTRING ALF5 DELIMITED ALL " " INTO ALFS ALF5NUM.
       NUM-LEFT9.
           MOVE NUM9 TO ALF9Z
           MOVE SPACE TO ALF9NUM
           MOVE ALF9Z TO ALF9 ALFS9
           UNSTRING ALF9 DELIMITED ALL " " INTO ALFS9 ALF9NUM.
       AMT-LEFT.
           MOVE  NUM7 TO ALF8Z
           MOVE SPACE TO ALF8NUM ALFS8
           MOVE ALF8Z TO ALF8
           UNSTRING ALF8 DELIMITED ALL " " INTO ALFS8 ALF8NUM.

       2000C.
      * ISA RECORD.
       ISA-1. 
      *  AUTHORIZATION INFO QUALIFIER
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-1.
      *  AUTHORIZATION INFO
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-2.
      *  SECURITY INFO QUAL
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-3.
      *  SECURITY INFO
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-4.
      *  INTERCHANGE ID QUALIFIER
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-5.
      *  INTERCHANGE SENDER ID
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-6.
      *  INTERCHANGE ID QUALIFIER
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-7.
      *  INTERCHANGE RECEIVER ID
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-8.
      *  INTERCHANGE DATE
           READ PARMFILE2 AT END GO TO A0-EXIT.
           ACCEPT ISA-9 FROM DATE.
      *  INTERCHANGE TIME
           READ PARMFILE2 AT END GO TO A0-EXIT.
           ACCEPT TIME-X FROM TIME
           MOVE TIME-HHMM TO ISA-10.
      *  INTERCHAGNE CONTROL STANDARDS ID
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-11.
      *  INTERCHANGE CONTROL VERSION CODE
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-12.
      *  INTERCHANGE CONTROL NUMBER
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-13.
      *  ACKNOWLEDGEMENT REQUESTED
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-14.
      *  USAGE INDICATOR
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-15.
      *  COMPONENT ELEMENT SEPARATOR
           READ PARMFILE2 AT END GO TO A0-EXIT.
           MOVE PARMFILE201 TO ISA-16.
       ISA-EXIT. EXIT.
      * TAX ID NUMBER 
       A0.    READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO EIN-CODE.
      * TAX ID TYPE
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO EINSS-TYPE.
      * CONTACT NAME #
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO CONTACT-NAME.
      * TELEPHONE #
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO TELE-PHONE.
      * INSURANCE-CODE #
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO INSTYPE-CODE.
      * INSURANCE-GROUP #
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE SPACE TO INSGROUP-CODE INSGROUP-LEG
           UNSTRING PARMFILE01 DELIMITED BY " " INTO
           INSGROUP-CODE INSGROUP-LEG.
      * SUBMITER ID INDICATORS (2)
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO SUBMIT-2.
      * ORGANIZATION NAME
           READ PARMFILE AT END GO TO A0-EXIT.
           IF EINSS-TYPE = "S"
           MOVE SPACE TO PARMLAST PARMFIRST PARMMIDDLE
           UNSTRING PARMFILE01 DELIMITED BY ";"
           INTO PARMLAST PARMFIRST PARMMIDDLE
           ELSE MOVE PARMFILE01 TO ORG-NAME.
      * GROUP'S STREET
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO ORG-STREET
      * GROUP'S CITY
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO ORG-CITY
      * GROUP'S STATE
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO ORG-STATE
      * GROUP'S ZIP CODE
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE SPACE TO ORG-ZIP
           MOVE PARMFILE01 TO ORG-ZIP
      * GROUP 3-CHARACTER MNEMONIC CODE
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO GROUP-3.
      *   CLIA-NUMBER
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO CLIA-NUM. 

      * ACCT-TAXONOMY.
           READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO GROUP-TAX.

      * INDIVIDUAL PROVIDER DATA
       A1. READ PARMFILE AT END GO TO A0-EXIT.
           MOVE PARMFILE01 TO PARM01
           MOVE PM-1 TO NUM2
           MOVE PM-2 TO DOC-TAX(NUM2)
           MOVE PM-3 TO DOC-SS(NUM2)
           MOVE PM-4 TO DOC-NUM(NUM2)
           MOVE SPACE TO ALF20 ALF10 ALF1
           UNSTRING PM-5 DELIMITED BY ";" INTO ALF20 ALF10 ALF1
           MOVE ALF20 TO DOC-LASTNAME(NUM2)
           MOVE ALF10 TO DOC-FIRSTNAME(NUM2)
           MOVE ALF1 TO DOC-MI(NUM2)
           MOVE PM-6 TO DOC-NPI(NUM2)
           GO TO A1.
       A0-EXIT.  EXIT.

       DF-SEARCH. 
           MOVE 0 TO FLAG.
           MOVE "2" TO CC-PL
           MOVE "12" TO CLM-5
           PERFORM DF-SEARCH2 VARYING Y FROM 1 BY 1 UNTIL Y > PLINDX.
       DF-SEARCH2.  
           IF HOLD-PLACE = PL-TAB(Y) 
           MOVE PL-NUM(Y) TO CC-PL
           MOVE Y TO PLACE-POINTER
           PERFORM PLACE-OF-SERVICE
           MOVE PLINDX TO Y.
       PLACE-OF-SERVICE.
           IF CC-PL = "1" MOVE "11" TO CLM-5.
           IF CC-PL = "3" MOVE "21" TO CLM-5.
           IF CC-PL = "4" MOVE "32" TO CLM-5.
           IF CC-PL = "5" MOVE "22" TO CLM-5.
           IF CC-PL = "6" MOVE "81" TO CLM-5.
           IF CC-PL = "7" MOVE "61" TO CLM-5.
           IF CC-PL = "8" MOVE "99" TO CLM-5.
           IF CC-PL = "E" MOVE "23" TO CLM-5. 
           IF CC-PL = "K" MOVE "31" TO CLM-5.
           IF CC-PL = "D" MOVE "33" TO CLM-5.
           IF CC-PL = "Q" MOVE "13" TO CLM-5.
           IF CC-PL = "S" MOVE "03" TO CLM-5.
           IF CC-PL = "T" MOVE "99" TO CLM-5.
           IF CC-PL = "Z" MOVE "02" TO CLM-5.
           IF HOLD-PROC1 > "99216" AND < "99221" MOVE "22" TO CLM-5.
           IF HOLD-PROC1 > "99220" AND < "99224" MOVE "21" TO CLM-5.
           IF HOLD-PROC1 > "99230" AND < "99234" MOVE "21" TO CLM-5.
           IF HOLD-PROC1 = "99238" MOVE "21" TO CLM-5.
           IF HOLD-PROC1 > "99250" AND < "99256" MOVE "21" TO CLM-5.
           IF HOLD-PROC1 > "99260" AND < "99264" MOVE "21" TO CLM-5.
           IF HOLD-PROC1 > "99280" AND < "99289" MOVE "23" TO CLM-5.
           IF HOLD-PROC1 > "99320" AND < "99324" MOVE "33" TO CLM-5.
           IF HOLD-PROC1 > "99330" AND < "99334" MOVE "33" TO CLM-5.


       HI-DIAG.
           IF HOLD-DATE-T > "20150930" GO TO HI-DIAG10.
           MOVE HOLD-DIAG TO DIAG-KEY
           READ DIAGFILE INVALID MOVE SPACE TO DIAG-MEDB.
           MOVE DIAG-MEDB TO HI9-DX1
           MOVE SPACE TO HI9-DIAG-FILLER DIAG9-ARRAY01

           PERFORM VARYING X FROM 2 BY 1 UNTIL X > DIAG-CNTR
            MOVE DIAGTAB(X) TO DIAG-KEY
             READ DIAGFILE INVALID CONTINUE
             END-READ
            MOVE "*BF:" TO DIAG9-BF(X - 1)
            MOVE DIAG-MEDB TO DIAG9-CODE(X - 1)
           END-PERFORM.
           MOVE DIAG9-ARRAY01 TO HI9-DIAG-FILLER
           WRITE SEGFILE01 FROM HI901.
           GO TO HI-DIAG-EXIT.
       HI-DIAG10.
           MOVE HOLD-DIAG TO DIAG-KEY
           READ DIAGFILE INVALID MOVE SPACE TO DIAG-MEDB.
           MOVE DIAG-KEY TO HI10-DX1
           MOVE SPACE TO HI10-DIAG-FILLER DIAG10-ARRAY01

           PERFORM VARYING X FROM 2 BY 1 UNTIL X > DIAG-CNTR
            MOVE DIAGTAB(X) TO DIAG10-CODE(X - 1)
            MOVE "*ABF:" TO DIAG10-BF(X - 1)
           END-PERFORM.
           MOVE DIAG10-ARRAY01 TO HI10-DIAG-FILLER
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM HI1001.
       HI-DIAG-EXIT.
           EXIT.
       SV-MOD.
           MOVE SPACE TO SV1-MOD-FILLER MOD-ARRAY01
            MOVE FI-PROC2 TO X-MOD1
           MOVE FI-MOD2 TO X-MOD2
           MOVE FI-MOD3 TO X-MOD3.
           IF X-MOD1 = SPACE AND X-MOD2 = SPACE
              MOVE X-MOD3 TO X-MOD1
              MOVE SPACE TO X-MOD3.
           IF X-MOD1 = SPACE 
              MOVE X-MOD2 TO X-MOD1
              MOVE X-MOD3 TO X-MOD2
              MOVE SPACE TO X-MOD3.
           IF X-MOD2 = SPACE
              MOVE X-MOD3 TO X-MOD2
              MOVE SPACE TO X-MOD3.
           IF X-MOD1 NOT = SPACE
             AND X-MOD2 = SPACE
              MOVE X-MOD3 TO X-MOD2
              MOVE SPACE TO X-MOD3.

           IF X-MOD1 NOT = SPACE
           MOVE ":" TO MOD-C(1)
           MOVE X-MOD1 TO MOD-CODE(1).
           IF X-MOD2 NOT = SPACE
           MOVE ":" TO MOD-C(2)
           MOVE X-MOD2 TO MOD-CODE(2).
           IF X-MOD3 NOT = SPACE
           MOVE ":" TO MOD-C(3)
           MOVE X-MOD3 TO MOD-CODE(3).
           MOVE MOD-ARRAY01 TO SV1-MOD-FILLER.
       P98.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SE01
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM GE01
           MOVE SPACE TO SEGFILE01.
      *     WRITE SEGFILE01 FROM IEA01.

       P99. 
            REWRITE HIPCLAIMFILE01.
            CLOSE GARFILE HIPCLAIMFILE CHARCUR ERRFILE.
            STOP RUN.
