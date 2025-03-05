       PROGRAM-ID. x270.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO "S25" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT GARFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
           LOCK MODE MANUAL.
           SELECT SEGFILE ASSIGN TO "S40" ORGANIZATION 
           LINE SEQUENTIAL.
           SELECT PARMFILE ASSIGN TO "S45" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT AUTHFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS AUTH-KEY
           LOCK MODE MANUAL.
           SELECT HIPCLAIMFILE ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS HIP-KEY
           LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FILEIN.
       01  FILEIN01 PIC X(8).

       FD  HIPCLAIMFILE.
       01  HIPCLAIMFILE01.
           02 HIP-KEY PIC X.
           02 HIP-NUM PIC 9(9).

       FD PARMFILE.
       01  PARMFILE01 PIC X(75).

       FD  SEGFILE.
       01  SEGFILE01 PIC X(120).
       
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
           02 CC-DIAG PIC X(5).
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
           02 CC-DX2 PIC X(5).
           02 CC-DX3 PIC X(5).
           02 CC-ACC-TYPE PIC X.
           02 CC-DATE-M PIC X(8).
           02 CC-ASSIGN PIC X.
           02 CC-NEIC-ASSIGN PIC X.
           02 CC-FREQ PIC X.
           02 CC-FUTURE PIC X(5).
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
           02 AUTH-FILLER PIC XXX.
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
           02 GS-1 PIC XX VALUE "HS".
           02 GS-S1 PIC X VALUE "*".
           02 GS-2 PIC X(9) VALUE "701100357".
           02 GS-S2 PIC X VALUE "*".
           02 GS-3 PIC X(9) VALUE "752548221".
           02 GS-S3 PIC X VALUE "*".
           02 GS-4 PIC X(8).
           02 GS-S4 PIC X VALUE "*".
           02 GS-5 PIC X(4).
           02 GS-S5 PIC X VALUE "*".
           02 GS-NUM PIC X(9).
           02 GS-S6 PIC X VALUE "*".
           02 GS-7 PIC X VALUE "X".
           02 GS-S7 PIC X VALUE "*".
           02 GS-8 PIC X(12) VALUE "004010X092A1".
           02 GS-S8 PIC X VALUE "*".
           02 GS-END PIC X VALUE "~".
       01  ST01.
           02 ST-0 PIC XX VALUE "ST".
           02 ST-S0 PIC X VALUE "*".
           02 ST-1 PIC XXX VALUE "270".
           02 ST-S1 PIC X VALUE "*".
           02 ST-NUM PIC X(9).
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
           02 BHT-1 PIC X(4) VALUE "0022".
           02 BHT-S1 PIC X VALUE "*".
           02 BHT-2 PIC XX VALUE "13".
           02 BHT-S2 PIC X VALUE "*".
           02 BHT-NUM PIC X(9).
           02 BHT-S3 PIC X VALUE "*".
           02 BHT-DATE PIC X(8).
           02 BHT-S4 PIC X VALUE "*".
           02 BHT-TIME PIC X(4).
           02 BHT-S5 PIC X VALUE "*".
           02 BHT-6  PIC X VALUE "*".
           02 BHT-END PIC X VALUE "~".
       01  TRN01.
           02 TRN-0 PIC XXX VALUE "TRN".
           02 TRN-S0 PIC X VALUE "*".
           02 TRN-1 PIC X VALUE "1".
           02 TRN-S1 PIC X VALUE "*".
           02 TRN-2 PIC X(8).
           02 TRN-S2 PIC X VALUE "*".
           02 TRN-3 PIC X(10) VALUE "9701100357".

           02 TRN-END PIC X VALUE "~".

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
           02 SUBM-3 PIC X(29) VALUE "COMPUTERIZED MEDICAL SERVICES".
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
           02 PRV-1 PIC XX VALUE "BI".
           02 PRV-S1 PIC X VALUE "*".
           02 PRV-2 PIC XX VALUE "ZZ".
           02 PRV-S2 PIC X VALUE "*".
           02 PRV-TAX PIC X(10).
           02 PRV-END PIC X VALUE "~".
       01 SAVE-PRV01.
           02 SAVE-PRV-0 PIC XXX VALUE "PRV".
           02 SAVE-PRV-S0 PIC X VALUE "*".
           02 SAVE-PRV-1 PIC XX VALUE "PE".
           02 PRV-S1 PIC X VALUE "*".
           02 SAVE-PRV-2 PIC XX VALUE "ZZ".
           02 SAVE-PRV-S2 PIC X VALUE "*".
           02 SAVE-PRV-TAX PIC X(10).
           02 SAVE-PRV-END PIC X VALUE "~".

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
           02 SAVE-DOCNM1-CODE PIC X(16).
           02 SAVE-DOCNM1-END PIC X VALUE "~".

       01 RECNM101.
           02 RECNM1-0 PIC XXX VALUE "NM1".
           02 RECNM1-S0 PIC X VALUE "*".
           02 RECNM1-1 PIC XXX VALUE "PR ".
           02 RECNM1-S1 PIC X VALUE "*".
           02 RECNM1-SOLO PIC X VALUE "2".
           02 RECNM1-S2 PIC X VALUE "*".
           02 RECNM1-NAMEL PIC X(11) VALUE "VT MEDICAID".
           02 RECNM1-S3 PIC X VALUE "*".
           02 RECNM1-S4 PIC X VALUE "*".
           02 RECNM1-S5 PIC X VALUE "*".
           02 RECNM1-S51 PIC X VALUE "*".
           02 RECNM1-S6 PIC X VALUE "*".
           02 RECNM1-8 PIC XX VALUE "PI".
           02 RECNM1-S7 PIC X VALUE "*".
           02 RECNM1-CODE PIC X(9) VALUE "752548221".
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
           
       01  DTP01.
           02 DTP-0 PIC XXX VALUE "DTP".
           02 DTP-S0 PIC X VALUE "*".
           02 DTP-1 PIC XXX.
           02 DTP-S1 PIC X VALUE "*".
           02 DTP-2 PIC XX VALUE "D8".
           02 DTP-S2 PIC X VALUE "*".
           02 DTP-3 PIC X(8).
           02 DTP-END PIC X VALUE "~".
       01  EQ01.
           02 EQ-0 PIC XX VALUE "EQ".
           02 EQ-S0 PIC X VALUE "*".
           02 EQ-1 PIC XX.
           02 EQ-S1 PIC X VALUE "*".
           02 EQ-PROC  PIC X(5).
           02 EQ-MOD-FILLER PIC X(12).
           02 EQ-END PIC X VALUE "~".

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
       01  DATE-X PIC X(8).
       01  TIME-X. 
           02 TIME-HHMM PIC X(4).
           02 FILLER PIC X(4).
       01  FLAG PIC 9.
       01  END-FLAG PIC 9 VALUE 0.
       01  GAP-FLAG PIC 9.
       01  CNTR PIC 99.
       01  DIAG-CNTR PIC 99.
       01  DX-CNTR-PT PIC 9.
       01  X PIC 99.
       01  DIAG-X PIC X(5).
       01  Y PIC 99.
       01  A PIC 9.
       01  FILETAB01.
           02 FILETAB PIC X(133) OCCURS 50 TIMES.
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
       01  DOC-TAB01.
           02 DOC-TAB02 OCCURS 90 TIMES.
              03 DOC-TAX PIC X(10).
              03 DOC-SS PIC X(9).
              03 DOC-NUM PIC X(8).
              03 DOC-LASTNAME PIC X(20).
              03 DOC-FIRSTNAME PIC X(10).
              03 DOC-MI PIC X.
              03 DOC-NPI PIC X(10).
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
       01 ORG-ZIP PIC X(5).
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
       01 SAVE01 PIC X(133).
       01 X-MOD.
          02 X-MOD1 PIC XX.
          02 X-MOD2 PIC XX.
          02 X-MOD3 PIC XX.
       01  MOD-ARRAY01.
           02 MOD-ARRAY OCCURS 4 TIMES.
              03 MOD-C PIC X.
              03 MOD-CODE PIC XX.
       01 NAME-1 PIC X(24).
       01 NAME-2 PIC X(24).
       01  MAMMO-FLAG PIC 9.
       01  CLIA-NUM PIC X(12).
       01  GROUP-TAX PIC X(10).
       PROCEDURE DIVISION.
       P0. 
           OPEN INPUT FILEIN GARFILE CHARCUR AUTHFILE PARMFILE
           OPEN OUTPUT SEGFILE
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
           PERFORM A0 THRU A0-EXIT.

           MOVE SPACE TO SEGFILE01
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


           ADD 1 TO HL-NUM
           COMPUTE NUM5 = HL-NUM
           PERFORM NUM-LEFT
           MOVE ALF5NUM TO HL-NUMX
           MOVE HL-NUMPRV-SAVE TO NUM5
           PERFORM NUM-LEFT
           MOVE SPACE TO HL-PARENT
           MOVE "20  " TO HL-CODE
           MOVE "1" TO HL-CHILD
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM HL01
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM RECNM101

           ADD 1 TO HL-NUM
           COMPUTE NUM5 = HL-NUM
           PERFORM NUM-LEFT
           MOVE ALF5NUM TO HL-NUMX
           MOVE HL-NUMPRV-SAVE TO NUM5
           MOVE "1" TO HL-PARENT
           MOVE "21  " TO HL-CODE
           MOVE "1" TO HL-CHILD
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM HL01

            MOVE "XX" TO NM1-EINSS
           IF EINSS-TYPE = "E"
            MOVE "2" TO NM1-SOLO
            MOVE ORG-NAME TO NM1-NAMEL
            MOVE SPACE TO NM1-NAMEF NM1-NAMEM NM1-NAMES
           ELSE 
            MOVE "1" TO NM1-SOLO
            MOVE PARMLAST TO NM1-NAMEL
            MOVE PARMFIRST TO NM1-NAMEF
            MOVE PARMMIDDLE TO NM1-NAMEM
           END-IF.
           MOVE "1P" TO NM1-1
           MOVE SPACE TO NM1-CODE
           MOVE INSGROUP-CODE TO NM1-CODE
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01  FROM NM101.

           MOVE SPACE TO REF-CODE REF-ID
           MOVE "EO" TO REF-CODE
           MOVE "701100357" TO REF-ID
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM REF01.

           MOVE SPACE TO N3-STREET N3-BILLADD
           MOVE ORG-STREET TO N3-STREET
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N301
           MOVE SPACE TO N4-CITY N4-STATE N4-ZIP
           MOVE ORG-CITY TO N4-CITY
           MOVE ORG-STATE TO N4-STATE
           MOVE ORG-ZIP TO N4-ZIP
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM N401
      * PER
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SUBPER01.
      * PRV
           MOVE "RF" TO PRV-1
           MOVE "ZZ" TO PRV-2
           MOVE GROUP-TAX TO PRV-TAX
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM PRV01.
      * 2000C
       P1.
           READ FILEIN AT END GO TO P99.
           MOVE FILEIN01 TO G-GARNO
           READ GARFILE INVALID CONTINUE.
           MOVE 1 TO FLAG.

      * CREATE THE RECORD FOR ELIGIBILITY
       P2.  
           ADD 1 TO HL-NUM
           MOVE HL-NUM TO HL-NUMPRV-SAVE
           COMPUTE NUM5 = HL-NUM
           PERFORM NUM-LEFT
           MOVE ALF5NUM TO HL-NUMX
           MOVE "2    " TO HL-PARENT
           MOVE "22  " TO HL-CODE
           MOVE "0" TO HL-CHILD
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM HL01.
           MOVE G-GARNO TO TRN-2
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM TRN01.
       2100C.
           MOVE "IL " TO NM1-1
           MOVE "1" TO NM1-SOLO
           IF G-PRNAME = SPACE MOVE G-GARNAME TO G-PRNAME.
           MOVE SPACE TO NM1-NAMEL NM1-NAMEF NM1-NAMEM NM1-NAMES
           UNSTRING G-PRNAME DELIMITED BY ";" INTO
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
           IF G-SEINS = "004" OR "064"
           MOVE G-SECPOL TO NM1-CODE
           END-IF
           MOVE "MI" TO NM1-EINSS
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM NM101.
           MOVE SPACE TO REF-CODE REf-ID
           MOVE "EJ" tO REF-CODE
           MOVE G-GARNO TO REF-ID
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM REF01.
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
           MOVE G-DOB TO DMG-DOB
           MOVE G-SEX TO DMG-GENDER
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM DMG01.
           MOVE "307" TO DTP-1
           MOVE "20101001" TO DTP-3
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM DTP01.
           MOVE "30" TO EQ-1
           MOVE SPACE TO EQ-PROC EQ-MOD-FILLER
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM EQ01.
           GO TO P1.
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
       P99.
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM SE01
           MOVE SPACE TO SEGFILE01
           WRITE SEGFILE01 FROM GE01
           MOVE SPACE TO SEGFILE01.
            REWRITE HIPCLAIMFILE01.
            CLOSE GARFILE HIPCLAIMFILE CHARCUR.
            STOP RUN.
