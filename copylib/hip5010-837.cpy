       01  ISA01.
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

       01  IEA01.
           02 IEA-0 PIC XXX VALUE "IEA".
           02 IEA-S0 PIC X VALUE "*".
           02 IEA-1 PIC X VALUE "1".
           02 IEA-END PIC X VALUE "~".
           
       01  GS01.
           02 GS-0 PIC XX VALUE "GS".
           02 GS-S0 PIC X VALUE "*".
           02 GS-1 PIC XX VALUE "HC".
           02 GS-S1 PIC X VALUE "*".
           02 GS-2 PIC X(9) VALUE "030353360".
           02 GS-S2 PIC X VALUE "*".
           02 GS-3 PIC X(9) VALUE "133052274".
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
           02 ST-NUM PIC 9(4).
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

       01  REF01.
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

       01  NTE01.
           02 NTE-0 PIC XXX VALUE "NTE".
           02 NTE-S0 PIC X VALUE "*".
           02 NTE-1 PIC XXX.
           02 NTE-S1 PIC X VALUE "*".
           02 NTE-2 PIC X(80).
           02 NTE-END PIC X VALUE "~".
       
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
           02 SUBM-NUM PIC X(9) VALUE "030353360".
           02 SUBM-END PIC X VALUE "~".

       01  SUBPER01.
           02 SUBPER-0 PIC XXX VALUE "PER".
           02 SUBPER-S0 PIC X VALUE "*".
           02 SUBPER-1 PIC XX VALUE "IC". 
           02 SUBPER-S1 PIC X VALUE "*".
           02 SUBPER-2 PIC X(20) VALUE "S WAITE".
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
           02 SUBPER-8  PIC X(20) VALUE "cmswest@sover.net".
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

       01  HL01.
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

       01  PRV01.
           02 PRV-0 PIC XXX VALUE "PRV".
           02 PRV-S0 PIC X VALUE "*".
           02 PRV-1 PIC XX VALUE "BI".
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
           02 PER-S4 PIC XX VALUE "*".
           02 PER-S5 PIC X VALUE "*".
           02 PER-S6 PIC X(10) VALUE "*".
           02 PER-S7 PIC X VALUE "*".
           02 PER-S8 PIC XX VALUE "*".
           02 PER-S9 PIC X VALUE "*".
           02 PER-S10 PIC X(20).
           02 PER-END PIC X VALUE "~".

       01  NM101.
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

       01  SAVE-DOCNM101.
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

       01  RECNM101.
           02 RECNM1-0 PIC XXX VALUE "NM1".
           02 RECNM1-S0 PIC X VALUE "*".
           02 RECNM1-1 PIC XXX VALUE "40 ".
           02 RECNM1-S1 PIC X VALUE "*".
           02 RECNM1-SOLO PIC X VALUE "2".
           02 RECNM1-S2 PIC X VALUE "*".
           02 RECNM1-NAMEL PIC X(22).
           02 RECNM1-S3 PIC X VALUE "*".
           02 RECNM1-S4 PIC X VALUE "*".
           02 RECNM1-S5 PIC X VALUE "*".
           02 RECNM1-S51 PIC X VALUE "*".
           02 RECNM1-S6 PIC X VALUE "*".
           02 RECNM1-8 PIC XX VALUE "46".
           02 RECNM1-S7 PIC X VALUE "*".
           02 RECNM1-CODE PIC X(5).
           02 RECNM1-END PIC X VALUE "~".

       01  N301.
           02 N3-0 PIC XX VALUE "N3".
           02 N3-S0 PIC X VALUE "*".
           02 N3-STREET PIC X(24).
           02 N3-S1 PIC X VALUE "*".
           02 N3-BILLADD PIC X(24).
           02 N3-S2 PIC X VALUE "*".
           02 N3-END PIC X VALUE "~".

       01  SAVEPAT-N301 PIC X(120).

       01  N401.
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

       01  SAVEPAT-N401 PIC X(120).

       01  SBR01.
           02 SBR-0 PIC XXX VALUE "SBR".
           02 SBR-S0 PIC X VALUE "*".
           02 SBR-PST PIC X.
           02 SBR-S1 PIC X VALUE "*".
           02 SBR-RELATE PIC XX.
           02 SBR-S2 PIC X VALUE "*".
           02 SBR-GROUP PIC X(10).
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

       01  DMG01.
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

       01  PAT01.
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

       01  CLM01.
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
           02 CLM-52 PIC X VALUE ":".
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
           02 CLM-10 PIC X VALUE "P".             
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

       01  HI901.
           02 HI9-0 PIC XX VALUE "HI".
           02 HI9-S0 PIC X VALUE "*".
           02 HI9-1C PIC XXX VALUE "BK:".
           02 HI9-DX1 PIC X(5).
           02 HI9-DIAG-FILLER PIC X(108).
           02 HI9-END PIC X VALUE "~".

       01  HI1001.
           02 HI10-0 PIC XX VALUE "HI".
           02 HI10-S0 PIC X VALUE "*".
           02 HI10-1C PIC XXXX VALUE "ABK:".
           02 HI10-DX1 PIC X(7).
           02 HI10-DIAG-FILLER PIC X(144).
           02 HI10-END PIC X VALUE "~".

       01  LX01.
           02 LX-0 PIC XX VALUE "LX".
           02 LX-S0 PIC X VALUE "*".
           02 LX-1 PIC XX.
           02 LX-END PIC X VALUE "~".

       01  SV101.
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
           02 SV1-S115 PIC X VALUE "*".
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

       01  D8TP01.
           02 D8TP-0 PIC XXX VALUE "DTP".
           02 D8TP-S0 PIC X VALUE "*".
           02 D8TP-1 PIC XXX.
           02 D8TP-S1 PIC X VALUE "*".
           02 D8TP-2 PIC XXX VALUE "RD8".
           02 D8TP-S2 PIC X VALUE "*".
           02 D8TP-3 PIC X(17).
           02 D8TP-END PIC X VALUE "~".
           
       01  OI01.
           02 OI-0 PIC XX VALUE "OI".
           02 OI-S12 PIC XXX VALUE "***".
           02 OI-3 PIC X VALUE "Y".
           02 OI-S4 PIC X VALUE "*".
           02 OI-4 PIC X VALUE "P".
           02 OI-S56 PIC XX VALUE "**".
           02 OI-6 PIC X VALUE "Y".
           02 OI-END PIC X VALUE "~".
      
       01  AMT01.
           02 AMT-0 PIC XXX VALUE "AMT".
           02 AMT-S0 PIC X VALUE "*".
           02 AMT-1 PIC XXX.
           02 AMT-S1 PIC X VALUE "*".
           02 AMT-2 PIC X(8).
           02 AMT-END PIC X VALUE "~".

       01  CAS01.
           02 CAS-0 PIC XXX VALUE "CAS".
           02 CAS-S0 PIC X VALUE "*".
           02 CAS-1 PIC XX.
           02 CAS-S1 PIC X VALUE "*".
           02 CAS-2 PIC XXX.
           02 CAS-S2 PIC X VALUE "*".
           02 CAS-3 PIC X(8).
           02 CAS-S3 PIC X VALUE "*".
           02 CAS-4 PIC X.
           02 CAS-END PIC X VALUE "~".
               
       01  SVD01.
           02 SVD-0 PIC XXX VALUE "SVD".
           02 SVD-S1 PIC X VALUE "*".
           02 SVD-1 PIC XXXXX.
           02 SVD-S3 PIC X VALUE "*".
           02 SVD-2 PIC X(8).
           02 SVD-S2 PIC X VALUE "*".
           02 SVD-3 PIC X(20).
           02 SVD-S34 PIC XX VALUE "**".
           02 SVD-4 PIC XX.
           02 SVD-END PIC X VALUE "~".