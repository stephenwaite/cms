       IDENTIFICATION DIVISION.
       PROGRAM-ID. carer304.
      * @package cms
      * @author s waite
      * @author Claude
      * backfill loader: parse 835 like carer303 but write ONLY
      * caredetl. no carefile I-O. takebacks (negative pay)
      * are recorded with sign. idempotent per remit.
      * S30 835 filein, S35 parmfile, S41 caredetl
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO "S30" ORGANIZATION
               LINE SEQUENTIAL.
           SELECT PARMFILE ASSIGN TO "S35" ORGANIZATION
               LINE SEQUENTIAL.
           SELECT CAREDETL ASSIGN TO "S41"
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS DT-KEY
               LOCK MODE MANUAL
               FILE STATUS IS WS-DFS.
       DATA DIVISION.
       FILE SECTION.
       FD  CAREDETL.
       01  CAREDETL01.
           02 DT-KEY.
              03 DT-KEY8 PIC X(8).
              03 DT-DATE PIC X(8).
              03 DT-PROC PIC X(5).
              03 DT-MOD1 PIC XX.
              03 DT-MOD2 PIC XX.
              03 DT-PAYDATE PIC X(8).
              03 DT-CK-EFT PIC X(9).
              03 DT-ICN PIC X(13).
              03 DT-SEQ PIC 9.
           02 DT-DOCP    PIC X(6).
           02 DT-POS     PIC XX.
           02 DT-BILLED PIC 9(4)V99.
           02 DT-ALLOWED PIC 9(4)V99.
           02 DT-DEDUCT  PIC 9(4)V99.
           02 DT-PAYED   PIC S9(4)V99.
           02 DT-DENIAL1 PIC X(4).
           02 DT-DENIAL2 PIC X(4).
           02 DT-DENIAL3 PIC X(4).
           02 DT-DENIAL4 PIC X(4).
           02 DT-PAYDENIAL PIC X(4).
           02 DT-INSNAME PIC X(30).
       FD  PARMFILE.
       01  PARMFILE01 PIC X(40).
       FD  FILEIN.
       01  FILEIN01.
           02 F0.
              03 F1 PIC XXX.
              03 F2 PIC X(4).
           02 F3 PIC X(113).
       WORKING-STORAGE SECTION.
      * carefile record image, working storage only
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
       01  HOLDDETL01.
           02 HD-KEY.
              03 HD-KEY8 PIC X(8).
              03 HD-DATE PIC X(8).
              03 HD-PROC PIC X(5).
              03 HD-MOD1 PIC XX.
              03 HD-MOD2 PIC XX.
              03 HD-PAYDATE PIC X(8).
              03 HD-CK-EFT PIC X(9).
              03 HD-ICN PIC X(13).
              03 HD-SEQ PIC 9.
           02 HD-DOCP    PIC X(6).
           02 HD-POS     PIC XX.
           02 HD-BILLED PIC 9(4)V99.
           02 HD-ALLOWED PIC 9(4)V99.
           02 HD-DEDUCT  PIC 9(4)V99.
           02 HD-PAYED   PIC S9(4)V99.
           02 HD-DENIAL1 PIC X(4).
           02 HD-DENIAL2 PIC X(4).
           02 HD-DENIAL3 PIC X(4).
           02 HD-DENIAL4 PIC X(4).
           02 HD-PAYDENIAL PIC X(4).
           02 HD-INSNAME PIC X(30).
       01  AMT01.
           02 AMT-0 PIC XXX.
           02 AMT-1 PIC XX.
           02 AMT-2 PIC X(8).
           02 AMT-3 PIC X.
       01  CAS01.
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
       01  BPR01.
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
       01  CLMCAS01 PIC X(120).
       01  CLP01.
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
       01  TRN01.
           02 TRN-0 PIC XXX.
           02 TRN-1 PIC X.
           02 TRN-2 PIC X(9).
           02 TRN-3 PIC X(9).
       01  DTM01.
           02 DTM-0 PIC XXX.
           02 DTM-1 PIC XXX.
           02 DTM-2 PIC X(8).
       01  REF01.
           02 REF-0 PIC XXX.
           02 REF-1 PIC XXX.
           02 REF-2 PIC X(30).
       01  MOA01.
           02 MOA-0 PIC XXX.
           02 MOA-1 PIC X.
           02 MOA-2 PIC X.
           02 MOA-DN1 PIC X(4).
           02 MOA-DN2 PIC X(4).
           02 MOA-DN3 PIC X(4).
           02 MOA-DN4 PIC X(4).
       01  SVC01.
           02 SVC-0 PIC XXX.
           02 SVC-1PROCMOD PIC X(17).
           02 SVC-2CHRGAMT PIC X(8).
           02 SVC-3PAYAMT  PIC X(8).
           02 SVC-4NUBC PIC XXX.
           02 SVC-5QUAN PIC X(5).
           02 SVC-6COMPOSITE PIC X(80).
           02 SVC-7QUAN PIC X(5).
       01  NM101.
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
       01  N101.
           02 N1-0 PIC XX.
           02 N1-1 PIC XX.
           02 N1-2 PIC X(20).
           02 N1-3 PIC XX.
           02 N1-4 PIC X(10).
       01  SVC-CNTR PIC 99.
       01  CAS-CNTR PIC 99.
       01  ALLW-TAB01.
           02 ALLW-TAB PIC 9(4)V99 OCCURS 64 TIMES.
       01  SVC-TAB01.
           02 SVC-TAB PIC X(120) OCCURS 64 TIMES.
       01  REF-TAB01.
           02 REF-TAB PIC X(6) OCCURS 64 TIMES.
       01  REF-PL01.
           02 REF-PL PIC X(2) OCCURS 64 TIMES.
       01  SVC-DATE01.
           02 SVC-DATE PIC X(8) OCCURS 64 TIMES.
       01  CAS-TAB01.
           02 CAS-TAB PIC X(120) OCCURS 64 TIMES.
       01  CAS-SVC01.
           02 CAS-SVC PIC 99 OCCURS 64 TIMES.
       01  SAVEFILE01 PIC X(120).
       01  X PIC 99.
       01  Z PIC 999.
       01  ALF-17.
           02 FILLER PIC XXX.
           02 ALF-14 PIC X(14).
       01  ALF8.
           02 ALF8-1 PIC X.
           02 ALF8-7 PIC X(7).
       01  SIGN-DOLLAR PIC XXXX.
       01  CENTS PIC XX.
       01  RIGHT-4 PIC X(4) JUST RIGHT.
       01  ALF-6 PIC X(6).
       01  NUM-6 PIC 9(6).
       01  AMOUNT-X PIC S9(4)V99.
       01  PROV-1 PIC X(10).
       01  PROV-2 PIC X(10).
       01  PROV-FED PIC X(9).
       01  PROV-LEG PIC X(6).
       01  IN-NPI PIC X(10).
       01  IN-FEDID PIC X(9).
       01  IN-LEG PIC X(6).
       01  WS-DFS PIC XX VALUE SPACES.
       01  WS-PAYED-S PIC S9(4)V99 VALUE ZERO.
       01  TAKEBACK PIC 9 VALUE 0.
       01  WS-WROTE PIC 9(6) VALUE ZERO.
       01  WS-DUPES PIC 9(6) VALUE ZERO.
       01  WS-TAKEBACKS PIC 9(6) VALUE ZERO.
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
           OPEN I-O CAREDETL.
           IF WS-DFS = "35"
               OPEN OUTPUT CAREDETL
               CLOSE CAREDETL
               OPEN I-O CAREDETL.
           IF WS-DFS NOT = "00"
               DISPLAY "carer304: caredetl open fs " WS-DFS
                   UPON SYSERR
               STOP RUN.
       P00.
           MOVE SPACE TO FILEIN01 IN-LEG IN-NPI
           READ FILEIN AT END GO TO P99.
           IF F1 NOT = "BPR" GO TO P00.
           MOVE SPACE TO BPR01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO
                BPR-0 BPR-1 BPR-2 BPR-3 BPR-4 BPR-5 BPR-6 BPR-7
                BPR-8 BPR-9 BPR-10 BPR-11 BPR-12 BPR-13 BPR-14
                BPR-15 BPR-16.
           MOVE BPR-16 TO CR-PAYDATE.
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P99.
           IF F1 NOT = "TRN"
               DISPLAY "carer304: no TRN after BPR" UPON SYSERR
               DISPLAY BPR01 UPON SYSERR
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
           IF F1 NOT = "CLP" GO TO P1-CLP.
       P1-CLP-1.
           MOVE SPACE TO CLP01
               CR-ICN CR-INSNAME
           UNSTRING FILEIN01 DELIMITED BY "*" INTO
                CLP-0 CLP-1 CLP-2CLMSTAT CLP-3TOTCLMCHG
                CLP-4TOTCLMPAY CLP-5PATRESP CLP-6PLANCODE CLP-7ICN
                CLP-8FACILITY CLP-9FREQ CLP-10PATSTAT CLP-11DRG
                CLP-12QUAN CLP-13PERCENT.
           MOVE CLP-1 TO CR-KEY8
           MOVE CLP-7ICN TO CR-ICN
           MOVE SPACE TO NM101 CLMCAS01
           MOVE SPACE TO SVC-DATE01
           MOVE SPACE TO CR-DENIAL1 CR-DENIAL2
               CR-DENIAL3 CR-DENIAL4
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
                    MOA-0 MOA-1 MOA-2 MOA-DN1 MOA-DN2 MOA-DN3
                    MOA-DN4
               MOVE MOA-DN1 TO CR-DENIAL1
               MOVE MOA-DN2 TO CR-DENIAL2
               MOVE MOA-DN3 TO CR-DENIAL3
               MOVE MOA-DN4 TO CR-DENIAL4
               GO TO P1-MOA.
           IF F1 = "NM1" AND F2 = "*TT*"
               MOVE SPACE TO NM101 CR-INSNAME
               UNSTRING FILEIN01 DELIMITED BY "*" INTO
                    NM1-0 NM1-1 NM1-SOLO NM1-NAMEL NM1-NAMEF
                    NM1-NAMEM NM1-NAMES NM1-EINSS NM1-PREFIX
                    NM1-CODE
               MOVE NM1-NAMEL TO CR-INSNAME.
           GO TO P1-MOA.
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
      * WRITE THE CAREDETL RECORDS
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
                SVC-0 SVC-1PROCMOD SVC-2CHRGAMT SVC-3PAYAMT
                SVC-4NUBC SVC-5QUAN SVC-6COMPOSITE SVC-7QUAN.
           MOVE SPACE TO ALF8
           MOVE SVC-3PAYAMT TO ALF8
           MOVE 0 TO TAKEBACK
           IF ALF8-1 = "-" MOVE 1 TO TAKEBACK.
           PERFORM AMOUNT-1
           MOVE AMOUNT-X TO WS-PAYED-S
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
                   CAS-0 CAS-1 CAS-2 CAS-3 CAS-4 CAS-5 CAS-6
                   CAS-7 CAS-8 CAS-9 CAS-10 CAS-11 CAS-12
                   CAS-13 CAS-14 CAS-15 CAS-16 CAS-17 CAS-18
                   CAS-19
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
           IF CR-MOD2 = "GA" MOVE SPACE TO CR-MOD2.
           MOVE CLP-7ICN TO CR-ICN
           MOVE BPR-16 TO CR-PAYDATE
           IF TAKEBACK = 1 ADD 1 TO WS-TAKEBACKS.
           PERFORM D-DETL THRU D-DETL-EXIT.
       P5-SVC-LOOP-EXIT.
           EXIT.
      * append payment detail to caredetl
       D-DETL.
           INITIALIZE CAREDETL01
           MOVE CR-KEY8    TO DT-KEY8
           MOVE CR-DATE    TO DT-DATE
           MOVE CR-PROC    TO DT-PROC
           MOVE CR-MOD1    TO DT-MOD1
           MOVE CR-MOD2    TO DT-MOD2
           MOVE CR-PAYDATE TO DT-PAYDATE
           MOVE CR-CK-EFT  TO DT-CK-EFT
           MOVE CR-ICN     TO DT-ICN
           MOVE 0          TO DT-SEQ
           MOVE CR-DOCP    TO DT-DOCP
           MOVE CR-POS     TO DT-POS
           MOVE CR-BILLED  TO DT-BILLED
           MOVE CR-ALLOWED TO DT-ALLOWED
           MOVE CR-DEDUCT  TO DT-DEDUCT
           MOVE WS-PAYED-S TO DT-PAYED
           MOVE CR-DENIAL1 TO DT-DENIAL1
           MOVE CR-DENIAL2 TO DT-DENIAL2
           MOVE CR-DENIAL3 TO DT-DENIAL3
           MOVE CR-DENIAL4 TO DT-DENIAL4
           MOVE CR-PAYDENIAL TO DT-PAYDENIAL
           MOVE CR-INSNAME TO DT-INSNAME.
       D-DETL-W.
           WRITE CAREDETL01 INVALID GO TO D-DETL-DUP.
           ADD 1 TO WS-WROTE
           GO TO D-DETL-EXIT.
       D-DETL-DUP.
           MOVE CAREDETL01 TO HOLDDETL01
           READ CAREDETL
           IF WS-DFS NOT = "00"
               DISPLAY "carer304: caredetl dup read fs " WS-DFS
                   " " HD-KEY UPON SYSERR
               GO TO D-DETL-EXIT.
           IF DT-PAYED = HD-PAYED AND DT-BILLED = HD-BILLED
      * same remit re-run, already posted
               ADD 1 TO WS-DUPES
               GO TO D-DETL-EXIT.
           MOVE HOLDDETL01 TO CAREDETL01
           ADD 1 TO DT-SEQ
           IF DT-SEQ > 8
               DISPLAY "carer304: caredetl seq overflow " DT-KEY
                   UPON SYSERR
               GO TO D-DETL-EXIT.
           GO TO D-DETL-W.
       D-DETL-EXIT.
           EXIT.
       AMOUNT-1.
           MOVE SPACES TO SIGN-DOLLAR CENTS.
           IF ALF8-1 = "-"
               UNSTRING ALF8-7 DELIMITED BY "." INTO
                   SIGN-DOLLAR CENTS
           ELSE
               UNSTRING ALF8 DELIMITED BY "." INTO
                   SIGN-DOLLAR CENTS
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
           CLOSE CAREDETL FILEIN PARMFILE
           DISPLAY "carer304: wrote " WS-WROTE
               " dupes " WS-DUPES
               " takebacks " WS-TAKEBACKS UPON SYSERR
           STOP RUN.
