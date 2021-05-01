       IDENTIFICATION DIVISION.
       PROGRAM-ID. carer303.cob.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT PARMFILE ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT CAREFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS CARE-KEY
           LOCK MODE MANUAL.
       DATA DIVISION.
       FILE SECTION.
       FD  CAREFILE.
       01  CAREFILE01.
           02 CARE-KEY.
              03 CR-KEY8 PIC X(8).
              03 CR-DATE PIC X(8).
              03 CR-PROC PIC X(5).
              03 CR-MOD1 PIC XX.
              03 CR-MOD2 PIC XX.
           02 CR-PAYDATE PIC X(8).
           02 CR-DOCP    PIC X(6).
           02 CR-POS     PIC XX.
           02 CR-BILLED PIC 9(4)V99.
           02 CR-ALLOWED PIC 9(4)V99.
           02 CR-DEDUCT  PIC 9(4)V99.
           02 CR-PAYED   PIC 9(4)V99.
           02 CR-DENIAL1 PIC X(4).
           02 CR-DENIAL2 PIC X(4).
           02 CR-DENIAL3 PIC X(4).
           02 CR-DENIAL4 PIC X(4).
           02 CR-PAYDENIAL PIC X(4).
           02 CR-ICN PIC X(13).
           02 CR-CK-EFT PIC X(9).
           02 CR-INSNAME PIC X(30).
       FD  PARMFILE.
       01  PARMFILE01 PIC X(40).
       FD FILEIN.
       01  FILEIN01.
           02 F0.
             03 F1 PIC XXX.
             03 F2 PIC X(4).
           02 F3 PIC X(113).
       WORKING-STORAGE SECTION.
       01  BACKFILE01.
           02 BACK-KEY.
              03 BK-KEY8 PIC X(8).
              03 BK-DATE PIC X(8).
              03 BK-PROC PIC X(5).
              03 BK-MOD1 PIC XX.
              03 BK-MOD2 PIC XX.
           02 BK-PAYDATE PIC X(8).
           02 BK-DOCP    PIC X(6).
           02 BK-POS     PIC XX.
           02 BK-BILLED PIC 9(4)V99.
           02 BK-ALLOWED PIC 9(4)V99.
           02 BK-DEDUCT  PIC 9(4)V99.
           02 BK-PAYED   PIC 9(4)V99.
           02 BK-DENIAL1 PIC X(4).
           02 BK-DENIAL2 PIC X(4).
           02 BK-DENIAL3 PIC X(4).
           02 BK-DENIAL4 PIC X(4).
           02 BK-PAYDENIAL PIC X(4).
           02 BK-ICN PIC X(13).
           02 BK-CK-EFT PIC X(9).
           02 BK-INSNAME PIC X(30).
       
       01 AMT01.
          02 AMT-0 PIC XXX.
          02 AMT-1 PIC XX.
          02 AMT-2 PIC X(8).
          02 AMT-3 PIC X.
       01 CAS01.
          02 CAS-0 PIC XXX.
          02 CAS-1 PIC XX.
          02 CAS-2 PIC X(5).
          02 CAS-3 PIC X(8).
          02 CAS-4 PIC XX.
          02 CAS-5 PIC X(5).
          02 CAS-6 PIC X(8).
          02 CAS-7 PIC XX.
          02 CAS-8 PIC X(5).
          02 CAS-9 PIC X(8).
          02 CAS-10 PIC XX.
          02 CAS-11 PIC X(5).
          02 CAS-12 PIC X(8).
          02 CAS-13 PIC XX.
          02 CAS-14 PIC X(5).
          02 CAS-15 PIC X(8).
          02 CAS-16 PIC XX.
          02 CAS-17 PIC X(5).
          02 CAS-18 PIC X(8).
          02 CAS-19 PIC XX.
       01 BPR01.
          02 BPR-0 PIC XXX.
          02 BPR-1 PIC XX.
          02 BPR-2 PIC X(9).
          02 BPR-3 PIC X.
          02 BPR-4 PIC XXX.
          02 BPR-5 PIC X(10).
          02 BPR-6 PIC XX.
          02 BPR-7 PIC X(12).
          02 BPR-8 PIC XXX.
          02 BPR-9 PIC X(35).
          02 BPR-10 PIC X(10).
          02 BPR-11 PIC X(9).
          02 BPR-12 PIC XX.
          02 BPR-13 PIC X(12).
          02 BPR-14 PIC XXX.
          02 BPR-15 PIC X(35).
          02 BPR-16 PIC X(8).
       01 CLMCAS01.
          02 CLMCAS-0 PIC XXX.
          02 CLMCAS-1 PIC XX.
          02 CLMCAS-2 PIC X(5).
          02 CLMCAS-3 PIC X(8).
          02 CLMCAS-4 PIC XX.
          02 CLMCAS-5 PIC X(5).
          02 CLMCAS-6 PIC X(8).
          02 CLMCAS-7 PIC XX.
          02 CLMCAS-8 PIC X(5).
          02 CLMCAS-9 PIC X(8).
          02 CLMCAS-10 PIC XX.
          02 CLMCAS-11 PIC X(5).
          02 CLMCAS-12 PIC X(8).
          02 CLMCAS-13 PIC XX.
          02 CLMCAS-14 PIC X(5).
          02 CLMCAS-15 PIC X(8).
          02 CLMCAS-16 PIC XX.
          02 CLMCAS-17 PIC X(5).
          02 CLMCAS-18 PIC X(8).
          02 CLMCAS-19 PIC XX.

       01 CLP01.
          02 CLP-0 PIC XXX.
          02 CLP-1 PIC X(14).
          02 CLP-2CLMSTAT PIC XX.
          02 CLP-3TOTCLMCHG PIC X(8).
          02 CLP-4TOTCLMPAY PIC X(8).
          02 CLP-5PATRESP PIC X(8).
          02 CLP-6PLANCODE PIC XX.
          02 CLP-7ICN PIC X(13).
          02 CLP-8FACILITY PIC XX.
          02 CLP-9FREQ PIC X.
          02 CLP-10PATSTAT PIC X(4).
          02 CLP-11DRG PIC X.
          02 CLP-12QUAN PIC XXX.
          02 CLP-13PERCENT PIC XXX.
       01 TRN01.
          02 TRN-0 PIC XXX.
          02 TRN-1 PIC X.
          02 TRN-2 PIC X(9).
          02 TRN-3 PIC X(9). 
       01 DTM01.
          02 DTM-0 PIC XXX.
          02 DTM-1 PIC XXX.
          02 DTM-2 PIC X(8).
          
       01 REF01.
          02 REF-0 PIC XXX.
          02 REF-1 PIC XXX.
          02 REF-2 PIC X(30).
       01 MOA01.  
          02 MOA-0 PIC XXX.
          02 MOA-1 PIC X.
          02 MOA-2 PIC X.
          02 MOA-DN1 PIC X(4).
          02 MOA-DN2 PIC X(4).
          02 MOA-DN3 PIC X(4).
          02 MOA-DN4 PIC X(4).
       01 SVC01.
           02 SVC-0 PIC XXX.
           02 SVC-1PROCMOD PIC X(17).
           02 SVC-2CHRGAMT PIC X(8).
           02 SVC-3PAYAMT  PIC X(8).
           02 SVC-4NUBC PIC XXX.
           02 SVC-5QUAN PIC X(5).
           02 SVC-6COMPOSITE PIC X(80).
           02 SVC-7QUAN PIC X(5).
           
       01 NM101.
           02 NM1-0 PIC XXX.
           02 NM1-1 PIC XXX.
           02 NM1-SOLO PIC X.
           02 NM1-NAMEL PIC X(24).
           02 NM1-NAMEF PIC X(24).
           02 NM1-NAMEM PIC X.
           02 NM1-NAMES PIC XXX.
           02 NM1-EINSS PIC XX.
           02 NM1-PREFIX PIC XX.
           02 NM1-CODE PIC X(14).
           
       01 TS301.
          02 TS3-0 PIC XXX.
          02 TS3-1 PIC X(7).
          02 TS3-2 PIC XX.
          02 TS3-3 PIC X(8).
          02 TS3-4 PIC XXXX.
          02 TS3-5TOTCLM PIC X(9).
          02 TS3-6TOTCVR PIC X(9).
          02 TS3-7TOTNONCVR PIC X(9).
          02 TS3-8TOTDENY PIC X(9).
          02 TS3-9TOTPAID PIC X(9).
          02 TS3-10INTEREST PIC X(9).
          02 TS3-11TOTCONADJ PIC X(9).
          02 TS3-12TOTGR PIC X(9).
          02 TS3-13TOTMSP PIC X(9).
          02 TS3-14TOTBLOOD PIC X(9).
          02 TS3-15TOTNONLABCHRG PIC X(9).
          02 TS3-16TOTCOINS PIC X(9).
          02 TS3-17TOTHCPCSCHG PIC X(9).
          02 TS3-18TOTHCPCSPAY PIC X(9).
          02 TS3-19TOTDEDUCT PIC X(9).
          02 TS3-20TOTPC PIC X(9).
          02 TS3-21TOTMSPLIAB PIC X(9).
          02 TS3-22TOTPATPAY PIC X(9).
          02 TS3-23TOTPIPCNTR PIC X(9).
          02 TS3-24TOTPIPPAY PIC X(9).

       01  XYZ PIC 999.
       01  TOT-AMT PIC S9(4)V99.
       01  PAYBACK PIC X(80).
       01  KEY11.
           02 KEY3 PIC XXX.
           02 KEY8 PIC X(8).
       01  CLAIM-TOT PIC S9(4)V99.
       01  FLAG PIC 9 VALUE 0.
       01  FLAGEND PIC 9 VALUE 0.
       01  TOT-PAY PIC S9(5)V99 VALUE 0.
       01  TOT-CHARGE PIC S9(5)V99 VALUE 0.
       01  TOT-REDUCE PIC S9(5)V99 VALUE 0.
       01 FLAGY PIC 9.
       01 FIND-CNTR PIC 99.
       01 CNTRY PIC 99.
       01 CNTR PIC 99.
       01  MULTCHAR PIC 99.
       01  X PIC 99.
       01  Y PIC 99.
       01  Z PIC 999.
       01  A PIC 99.
       01 HOLDKEY PIC X(11).
       01 HOLDAMT PIC S9(4)V99.
       01  RIGHT-4 PIC X(4) JUST RIGHT.
       01  ALF-3 PIC XXX.
       01  ALF3 PIC XXX.
       01  ALF-7.
           02 ALF-71 PIC X(5).
           02 ALF-72 PIC XX.
       01  ALF-6 PIC X(6).
       01  NUM-6 PIC 9(6).
       01  AMOUNT-X PIC S9(4)V99.
       01  ALF8. 
           02 ALF8-1 PIC X.
           02 ALF8-7 PIC X(7).
       01  ALF9 PIC X(9).
       01  ALF-11.
           02 ALF-11-4 PIC X(4).
           02 ALF-11-7 PIC X(7).
       01  ALF-17.
           02 FILLER PIC XXX.
           02 ALF-14 PIC X(14).
       01  INPUT-DATE.
           05 T-MM  PIC XX.
           05 T-DD  PIC XX.
           05 T-CC  PIC XX.
           05 T-YY  PIC XX.
       01  OVERFLAG PIC 9.
       01  CO-PAY-PAID PIC S9(7)V99.
       01 DATE-X PIC X(8).
       01 DATE-CC PIC X(8).
       01 SVC-CNTR PIC 99.
       01 CAS-CNTR PIC 99.
       01 ALLW-TAB01.
          02 ALLW-TAB PIC 9(4)V99 OCCURS 64 TIMES.
       01 SVC-TAB01.
          02 SVC-TAB PIC X(120) OCCURS 64 TIMES.
       01 REF-TAB01.
          02 REF-TAB PIC X(6) OCCURS 64 TIMES.
       01 REF-PL01.
          02 REF-PL PIC X(2) OCCURS 64 TIMES.
       01 SVC-DATE01.
          02 SVC-DATE PIC X(8) OCCURS 64 TIMES.
       01 FOUND-TAB01.
          02 FOUND-KEY PIC X(11) OCCURS 64 TIMES.
       01 CAS-TAB01.
          02 CAS-TAB PIC X(120) OCCURS 64 TIMES.
       01 CAS-SVC01.
          02 CAS-SVC PIC 99 OCCURS 64 TIMES.
       01 FILE01 PIC X(120).
       01 SAVELASTNAME PIC X(24).
       01 SAVEFIRSTNAME PIC X(24).
       01 SAVEPOLICY PIC X(14).
       01 SAVEFILE01 PIC X(120).
       01 CC-PROC1X PIC X(5).
       01 CC-PROC2X PIC XX.
       01 CC-MOD2X PIC XX.
       01 CC-MOD3X PIC XX.
       01  CENTS PIC XX.
       01  SIGN-DOLLAR PIC XXXX.
       01 LNAME PIC X(24).
       01 FNAME PIC X(24).
       01  DISPLAY-DATE.
           05 T-MM  PIC 99.
           05 FILLER PIC X VALUE "/".
           05 T-DD  PIC 99.
           05 FILLER PIC X VALUE "/".
           05 T-CC  PIC 99.
           05 T-YY  PIC 99.
       01  TEST-DATE.
           05 T-CC  PIC 99.
           05 T-YY  PIC 99.
           05 T-MM  PIC 99.
           05 T-DD  PIC 99.
       01  ORDER-8.
           02 ORDER-6 PIC X(6).
           02 FILLER PIC XX.
       01  NAR-KEY01.
           02 NAR-KEY PIC XXX OCCURS 216 TIMES.
       01  NAR-CNTR01.
           02 NAR-CNTR PIC 999 OCCURS 216 TIMES.
       01  DENIAL-CNTR PIC 99.
       01  EF-TAB01.
           02 EF-TAB PIC X(4) OCCURS 64 TIMES.
       01  MNGD-CARE PIC XXX.
       01  TITLE-FLAG PIC 9 VALUE 0.
       01  ALF6 PIC X(6).
       01  INS-REDUCE PIC S9(5)V99.
       01  ALF25 PIC X(25).
       01  NEF-2 PIC Z9.
       01  PROV-1 PIC X(10).
       01  PROV-2 PIC X(10).
       01  PROV-FED PIC X(9).
       01  PROV-LEG PIC X(6).

       01  IN-NPI PIC X(10).
       01  IN-FEDID PIC X(9).
       01  IN-LEG PIC X(6).
       01 N101.
           02 N1-0 PIC XX.
           02 N1-1 PIC XX.
           02 N1-2 PIC X(20).
           02 N1-3 PIC XX.
           02 N1-4 PIC X(10).

       01  ANS PIC X.
       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT FILEIN PARMFILE.
           READ PARMFILE AT END GO TO P99.
           READ PARMFILE AT END GO TO P99.
           MOVE SPACE TO PROV-1 PROV-2
           UNSTRING PARMFILE01 DELIMITED BY " " INTO PROV-1 PROV-2
           READ PARMFILE AT END GO TO P99.
           MOVE PARMFILE01 TO PROV-FED.
           READ PARMFILE AT END GO TO P99.
           MOVE PARMFILE01 TO PROV-LEG.
           OPEN I-O CAREFILE.
       P00.
           
           MOVE SPACE TO FILEIN01 IN-LEG IN-NPI
           READ FILEIN AT END GO TO P99.
           IF F1 NOT = "BPR" GO TO P00.
           MOVE SPACE TO BPR01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO 
           BPR-0 BPR-1 BPR-2 BPR-3 BPR-4 BPR-5 BPR-6 BPR-7 BPR-8 
           BPR-9 BPR-10 BPR-11 BPR-12 BPR-13 BPR-14 BPR-15 BPR-16.
           MOVE BPR-16 TO CR-PAYDATE.


           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P99.
           IF F1 NOT = "TRN"
           DISPLAY " NO TRN RECORD AFTER BPR RECORD"
           DISPLAY BPR01
           ACCEPT ALF25
           GO TO P00.
           MOVE SPACE TO TRN01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO 
           TRN-0 TRN-1 TRN-2 TRN-3. 
           MOVE TRN-2 TO CR-CK-EFT.
      
       P000.
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P99.
           IF NOT FILEIN01(1:5) = "N1*PE" GO TO P000.   
           MOVE SPACE TO N101
           UNSTRING FILEIN01 DELIMITED BY "*" INTO
           N1-0 N1-1 N1-2 N1-3 N1-4
           IF N1-3 = "FI" MOVE N1-4 TO IN-FEDID.
           IF N1-3 = "XX" MOVE N1-4 TO IN-NPI. 


       P0000.
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P99.
           IF F1 NOT = "REF" GO TO P0000.
           MOVE SPACE TO REF01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO
           REF-0 REF-1 REF-2
           IF REF-1 = "1C" MOVE REF-2 TO IN-LEG.
           IF NOT ((PROV-1 = IN-NPI) OR (PROV-2 = IN-NPI))
                GO TO P00.
           
       P1-CLP. 
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P99.
       XX.
           IF F1 NOT = "CLP" GO TO P1-CLP.
       P1-CLP-1.
           MOVE SPACE TO CLP01
            CR-ICN CR-CK-EFT CR-INSNAME
           UNSTRING FILEIN01 DELIMITED BY "*" INTO
           CLP-0 CLP-1 CLP-2CLMSTAT CLP-3TOTCLMCHG CLP-4TOTCLMPAY 
           CLP-5PATRESP CLP-6PLANCODE CLP-7ICN CLP-8FACILITY 
           CLP-9FREQ CLP-10PATSTAT CLP-11DRG CLP-12QUAN CLP-13PERCENT.
           MOVE CLP-1 TO CR-KEY8
           MOVE CLP-7ICN TO CR-ICN
           MOVE SPACE TO NM101 CLMCAS01.
           MOVE SPACE TO SVC-DATE01
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 64
           MOVE 0 TO ALLW-TAB(X)
           END-PERFORM
           MOVE 0 TO CAS-CNTR
           MOVE 0 TO SVC-CNTR.
       P1-MOA.    
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P99.
           IF F1 = "SVC" GO TO P1-SVC-LOOP-0.
           IF F1 = "MOA"
           MOVE SPACE TO MOA01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO
           MOA-0 MOA-1 MOA-2 MOA-DN1  MOA-DN2 MOA-DN3  MOA-DN4 
           MOVE MOA-DN1 TO CR-DENIAL1
           MOVE MOA-DN2 TO CR-DENIAL2
           MOVE MOA-DN3 TO CR-DENIAL3
           MOVE MOA-DN4 TO CR-DENIAL4
           GO TO P1-MOA.
           IF F1 = "NM1" AND F2 = "*QC*"
           MOVE SPACE TO NM101
           UNSTRING FILEIN01 DELIMITED BY "*" INTO
            NM1-0 NM1-1 NM1-SOLO NM1-NAMEL NM1-NAMEF NM1-NAMEM 
            NM1-NAMES NM1-EINSS NM1-PREFIX NM1-CODE
           MOVE NM1-NAMEL TO SAVELASTNAME
           MOVE NM1-NAMEF TO SAVEFIRSTNAME
           MOVE NM1-CODE TO SAVEPOLICY
           GO TO P1-MOA.
           IF F1 = "NM1" AND F2 = "*TT*"
             MOVE SPACE TO NM101 CR-INSNAME
             UNSTRING FILEIN01 DELIMITED BY "*" INTO
               NM1-0 NM1-1 NM1-SOLO NM1-NAMEL NM1-NAMEF NM1-NAMEM 
               NM1-NAMES NM1-EINSS NM1-PREFIX NM1-CODE
               MOVE NM1-NAMEL TO CR-INSNAME.

               
           GO TO P1-MOA.

          
       P1-NM1.
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P99.
           IF F1 = "SVC" GO TO P1-SVC-LOOP-0.
           IF F1 = "CLP" OR "SE*" 
            MOVE FILEIN01 TO SAVEFILE01
            GO TO P2-SVC-LOOP.
           IF F1 = "CAS" 
             MOVE SPACE TO CAS01
             UNSTRING FILEIN01 DELIMITED BY "*" INTO
             CAS-0 CAS-1 CAS-2 CAS-3 CAS-4 CAS-5 CAS-6 CAS-7 
             CAS-8 CAS-9 CAS-10 CAS-11 CAS-12 CAS-13 CAS-14 
             CAS-15 CAS-16 CAS-17 CAS-18 CAS-19 
             MOVE CAS01 TO CLMCAS01
             MOVE SPACE TO CAS01
            GO TO P1-NM1.
           IF F1 = "DTM" AND F2 = "*232"
            MOVE SPACE TO DTM01
            UNSTRING FILEIN01 DELIMITED BY "*" INTO
            DTM-0 DTM-1 DTM-2 
            MOVE DTM-2 TO DATE-CC SVC-DATE(1).
           GO TO P1-NM1.
       P1-SVC-LOOP.  
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P2-SVC-LOOP.
           
           IF F1 = "CLP" OR "SE*" 
            MOVE FILEIN01 TO SAVEFILE01
            GO TO P2-SVC-LOOP.
       P1-SVC-LOOP-0.    
           IF F1 = "SVC" 
           ADD 1 TO SVC-CNTR
           MOVE FILEIN01 TO SVC-TAB(SVC-CNTR)
           GO TO P1-SVC-LOOP.
           
           IF F1 = "CAS" 
           ADD 1 TO CAS-CNTR
           MOVE FILEIN01 TO CAS-TAB(CAS-CNTR)
           MOVE SVC-CNTR TO CAS-SVC(CAS-CNTR)
           GO TO P1-SVC-LOOP.
           
           IF F1 = "DTM" AND F2 = "*472"
           MOVE SPACE TO DTM01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO 
           DTM-0 DTM-1 DTM-2
           MOVE DTM-2 TO SVC-DATE(SVC-CNTR)
           GO TO P1-SVC-LOOP.

           IF F1 = "REF" AND F2 = "*1C*"
           MOVE SPACE TO REF01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO 
           REF-0 REF-1 REF-2
           MOVE REF-2 TO REF-TAB(SVC-CNTR)
           GO TO P1-SVC-LOOP.
           
           IF F1 = "REF" AND F2 = "*LU*"
           MOVE SPACE TO REF01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO 
           REF-0 REF-1 REF-2
           MOVE REF-2 TO REF-PL(SVC-CNTR)
           GO TO P1-SVC-LOOP.

           IF F1 = "AMT" AND F2 = "*B6*"
           MOVE SPACE TO AMT01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO 
           AMT-0 AMT-1 AMT-2
           MOVE SPACE TO ALF8
           MOVE AMT-2 TO ALF8
           PERFORM AMOUNT-1
           MOVE AMOUNT-X TO ALLW-TAB(SVC-CNTR)
           GO TO P1-SVC-LOOP.

           GO TO P1-SVC-LOOP.

      * WRITE THE CAREFILE RECORD
      
       P2-SVC-LOOP.
           PERFORM P5-SVC-LOOP THRU P5-SVC-LOOP-EXIT 
             VARYING X FROM 1 BY 1 UNTIL X > SVC-CNTR
           MOVE SAVEFILE01 TO FILEIN01
           IF F1 = "CLP" GO TO P1-CLP-1.
           GO TO P00.
       P5-SVC-LOOP.
           MOVE SPACE TO FILEIN01
           MOVE SVC-TAB(X) TO FILEIN01
           MOVE SPACE TO SVC01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO 
           SVC-0 SVC-1PROCMOD SVC-2CHRGAMT SVC-3PAYAMT SVC-4NUBC 
           SVC-5QUAN SVC-6COMPOSITE SVC-7QUAN.
           MOVE SPACE TO ALF8
           MOVE SVC-3PAYAMT TO ALF8
           IF ALF8-1 = "-" GO TO P5-SVC-LOOP-EXIT.
           PERFORM AMOUNT-1
           MULTIPLY AMOUNT-X BY -1 GIVING CR-PAYED
            MOVE SPACE TO ALF-17 CR-PROC CR-MOD1 CR-MOD2
            MOVE SVC-1PROCMOD TO ALF-17
            UNSTRING ALF-14 DELIMITED BY ":" INTO 
                                    CR-PROC CR-MOD1 CR-MOD2
            MOVE REF-PL(X) TO CR-POS
            MOVE SVC-DATE(X) TO CR-DATE
            MOVE REF-TAB(X) TO CR-DOCP
            MOVE SVC-2CHRGAMT TO ALF8
            PERFORM AMOUNT-1
            MOVE AMOUNT-X TO CR-BILLED
            MOVE 0 TO CR-DEDUCT 
            MOVE SPACE TO CR-PAYDENIAL
            PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > CAS-CNTR
            IF CAS-SVC(Z) = X
              MOVE SPACE TO CAS01 
              MOVE CAS-TAB(Z) TO FILEIN01
              UNSTRING FILEIN01 DELIMITED BY "*" INTO
              CAS-0 CAS-1 CAS-2 CAS-3 CAS-4 CAS-5 CAS-6 CAS-7 
              CAS-8 CAS-9 CAS-10 CAS-11 CAS-12 CAS-13 CAS-14 
              CAS-15 CAS-16 CAS-17 CAS-18 CAS-19 
              IF CR-PAYDENIAL = SPACE
               MOVE CAS-2 TO CR-PAYDENIAL
              END-IF
              
              IF (CAS-2 = "1  " OR "126" OR "25 " OR "37 ") 
               IF CAS-3 NOT = SPACE
                MOVE SPACE TO ALF8
                MOVE CAS-3 TO ALF8
                PERFORM AMOUNT-1
                COMPUTE CR-DEDUCT = CR-DEDUCT + AMOUNT-X
               END-IF
              END-IF
              IF (CAS-5 = "1  " OR "126" OR "25 " OR "37 ") 
               IF CAS-6 NOT = SPACE
                MOVE SPACE TO ALF8
                MOVE CAS-6 TO ALF8
                PERFORM AMOUNT-1
                COMPUTE CR-DEDUCT = CR-DEDUCT + AMOUNT-X
               END-IF
              END-IF
              IF (CAS-8 = "1  " OR "126" OR "25 " OR "37 ") 
               IF CAS-9 NOT = SPACE
                MOVE SPACE TO ALF8
                MOVE CAS-9 TO ALF8
                PERFORM AMOUNT-1
                COMPUTE CR-DEDUCT = CR-DEDUCT + AMOUNT-X
               END-IF
              END-IF
              IF (CAS-11 = "1  " OR "126" OR "25 " OR "37 ")  
               IF CAS-12 NOT = SPACE
                MOVE SPACE TO ALF8
                MOVE CAS-12 TO ALF8
                PERFORM AMOUNT-1
                COMPUTE CR-DEDUCT = CR-DEDUCT + AMOUNT-X
               END-IF
              END-IF
              IF (CAS-14 = "1  " OR "126" OR "25 " OR "37 ") 
               IF CAS-15 NOT = SPACE
                MOVE SPACE TO ALF8
                MOVE CAS-15 TO ALF8
                PERFORM AMOUNT-1
                COMPUTE CR-DEDUCT = CR-DEDUCT + AMOUNT-X
               END-IF
              END-IF
              IF (CAS-17 = "1  " OR "126" OR "25 " OR "37 ") 
               IF CAS-18 NOT = SPACE
                MOVE SPACE TO ALF8
                MOVE CAS-18 TO ALF8
                PERFORM AMOUNT-1
                COMPUTE CR-DEDUCT = CR-DEDUCT + AMOUNT-X
               END-IF
              END-IF
            END-IF
            END-PERFORM.
           COMPUTE CR-ALLOWED = ALLW-TAB(X)
      *     IF (CR-DENIAL1 = "MA18" OR CR-DENIAL2 = "MA18"
      *     OR CR-DENIAL2 = "MA18"  OR CR-DENIAL3 = "MA18")
      *     AND (CR-PAYED = 0) 
      *     AND (CR-ALLOWED = 0)
      *     AND (CR-DEDUCT = 0)
      *     GO TO P5-SVC-LOOP-EXIT.
           IF CR-MOD2 = "GA" MOVE SPACE TO CR-MOD2.
           MOVE CLP-7ICN TO CR-ICN
           MOVE BPR-16 TO CR-PAYDATE
           MOVE CAREFILE01 TO BACKFILE01.
           READ CAREFILE WITH LOCK INVALID GO TO A4-1.
           IF BK-ALLOWED > CR-ALLOWED GO TO A4-2.
           IF CR-PAYED = 0 AND BK-PAYED  NOT = 0 GO TO A4-2.
           GO TO P5-SVC-LOOP-EXIT.
       A4-1.
           WRITE CAREFILE01 INVALID
           DISPLAY CARE-KEY " WRITE FAILED".
           GO TO P5-SVC-LOOP-EXIT.
       A4-2.
           MOVE BACKFILE01 TO CAREFILE01
           REWRITE CAREFILE01 INVALID
           DISPLAY CARE-KEY " REWRITE FAILED".
       P5-SVC-LOOP-EXIT.
           EXIT.
       AMOUNT-1.
           MOVE SPACES TO SIGN-DOLLAR CENTS.
           IF ALF8-1 = "-"
           UNSTRING ALF8-7 DELIMITED BY "." INTO SIGN-DOLLAR CENTS
           ELSE
           UNSTRING ALF8 DELIMITED BY "." INTO SIGN-DOLLAR CENTS
           END-IF.
           INSPECT CENTS REPLACING ALL " " BY "0".
           MOVE SPACES TO RIGHT-4.
           UNSTRING SIGN-DOLLAR DELIMITED BY " " INTO RIGHT-4
           INSPECT RIGHT-4 REPLACING LEADING " " BY "0"
           STRING RIGHT-4 CENTS DELIMITED BY SIZE INTO ALF-6
           MOVE ALF-6 TO NUM-6
           DIVIDE NUM-6 BY 100 GIVING AMOUNT-X.
           IF ALF8-1 = "-"
           COMPUTE AMOUNT-X = -1 * AMOUNT-X.
       P99.
           CLOSE CAREFILE FILEIN PARMFILE
           STOP RUN.
