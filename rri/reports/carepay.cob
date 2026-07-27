       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAREPAY.
      * @package cms
      * @author s waite
      * @author Claude
      * line by line payment report for a garno / dos / cpt
      * params via environment: GARNO, DOS, CPT
      * S1 carefile, S2 report out
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CAREFILE ASSIGN TO "S1"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CARE-KEY
               FILE STATUS IS WS-FS.
           SELECT PRTFILE ASSIGN TO "S2"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS2.
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
       FD  PRTFILE.
       01  PRT-REC PIC X(132).
       WORKING-STORAGE SECTION.
       01  WS-FS         PIC XX VALUE SPACES.
       01  WS-FS2        PIC XX VALUE SPACES.
       01  WS-GARNO      PIC X(8) VALUE SPACES.
       01  WS-DOS        PIC X(8) VALUE SPACES.
       01  WS-CPT        PIC X(5) VALUE SPACES.
       01  WS-CNT        PIC 9(4) VALUE ZERO.
       01  WS-TOT-BILLED PIC 9(6)V99 VALUE ZERO.
       01  WS-TOT-ALLOW  PIC 9(6)V99 VALUE ZERO.
       01  WS-TOT-DEDUCT PIC 9(6)V99 VALUE ZERO.
       01  WS-TOT-PAYED  PIC 9(6)V99 VALUE ZERO.
       01  HDR1.
           02 FILLER PIC X(18) VALUE "payments for garno".
           02 FILLER PIC X VALUE SPACE.
           02 H1-GARNO PIC X(8).
           02 FILLER PIC X(5) VALUE " dos ".
           02 H1-DOS PIC X(8).
           02 FILLER PIC X(5) VALUE " cpt ".
           02 H1-CPT PIC X(5).
       01  HDR2.
           02 FILLER PIC X(9)  VALUE "paydate".
           02 FILLER PIC X(5)  VALUE "mods".
           02 FILLER PIC X(9)  VALUE "  billed".
           02 FILLER PIC X(9)  VALUE " allowed".
           02 FILLER PIC X(9)  VALUE "  deduct".
           02 FILLER PIC X(9)  VALUE "   payed".
           02 FILLER PIC X(10) VALUE " ck-eft".
           02 FILLER PIC X(14) VALUE " icn".
           02 FILLER PIC X(9)  VALUE " denial".
           02 FILLER PIC X(30) VALUE " insname".
       01  DTL.
           02 D-PAYDATE PIC X(8).
           02 FILLER    PIC X VALUE SPACE.
           02 D-MOD1    PIC XX.
           02 D-MOD2    PIC XX.
           02 FILLER    PIC X VALUE SPACE.
           02 D-BILLED  PIC Z(4)9.99.
           02 FILLER    PIC X VALUE SPACE.
           02 D-ALLOWED PIC Z(4)9.99.
           02 FILLER    PIC X VALUE SPACE.
           02 D-DEDUCT  PIC Z(4)9.99.
           02 FILLER    PIC X VALUE SPACE.
           02 D-PAYED   PIC Z(4)9.99.
           02 FILLER    PIC X VALUE SPACE.
           02 D-CK-EFT  PIC X(9).
           02 FILLER    PIC X VALUE SPACE.
           02 D-ICN     PIC X(13).
           02 FILLER    PIC X VALUE SPACE.
           02 D-DENIAL  PIC X(8).
           02 FILLER    PIC X VALUE SPACE.
           02 D-INSNAME PIC X(30).
       01  SUM1.
           02 FILLER PIC X(9) VALUE "totals".
           02 FILLER PIC X(5) VALUE SPACES.
           02 S-BILLED  PIC Z(5)9.99.
           02 FILLER    PIC X VALUE SPACE.
           02 S-ALLOWED PIC Z(5)9.99.
           02 FILLER    PIC X VALUE SPACE.
           02 S-DEDUCT  PIC Z(5)9.99.
           02 FILLER    PIC X VALUE SPACE.
           02 S-PAYED   PIC Z(5)9.99.
       01  SUM2.
           02 S-CNT  PIC Z(3)9.
           02 FILLER PIC X(11) VALUE " payment(s)".
       PROCEDURE DIVISION.
       P00.
           ACCEPT WS-GARNO FROM ENVIRONMENT "GARNO".
           ACCEPT WS-DOS FROM ENVIRONMENT "DOS".
           ACCEPT WS-CPT FROM ENVIRONMENT "CPT".
           IF WS-GARNO = SPACES OR WS-DOS = SPACES
               OR WS-CPT = SPACES
               DISPLAY "carepay: need GARNO DOS CPT in environment"
                   UPON SYSERR
               STOP RUN.
           OPEN INPUT CAREFILE.
           IF WS-FS NOT = "00"
               DISPLAY "carepay: carefile open fs " WS-FS UPON SYSERR
               STOP RUN.
           OPEN OUTPUT PRTFILE.
           IF WS-FS2 NOT = "00"
               DISPLAY "carepay: prtfile open fs " WS-FS2 UPON SYSERR
               CLOSE CAREFILE
               STOP RUN.
           MOVE WS-GARNO TO H1-GARNO.
           MOVE WS-DOS   TO H1-DOS.
           MOVE WS-CPT   TO H1-CPT.
           WRITE PRT-REC FROM HDR1.
           WRITE PRT-REC FROM HDR2.
           MOVE WS-GARNO TO CR-KEY8.
           MOVE WS-DOS   TO CR-DATE.
           MOVE WS-CPT   TO CR-PROC.
           MOVE LOW-VALUES TO CR-MOD1 CR-MOD2.
           START CAREFILE KEY NOT < CARE-KEY.
           IF WS-FS NOT = "00"
               GO TO P9.
       P1.
           READ CAREFILE NEXT.
           IF WS-FS NOT = "00"
               GO TO P9.
           IF CR-KEY8 NOT = WS-GARNO
               OR CR-DATE NOT = WS-DOS
               OR CR-PROC NOT = WS-CPT
               GO TO P9.
           MOVE CR-PAYDATE TO D-PAYDATE.
           MOVE CR-MOD1    TO D-MOD1.
           MOVE CR-MOD2    TO D-MOD2.
           MOVE CR-BILLED  TO D-BILLED.
           MOVE CR-ALLOWED TO D-ALLOWED.
           MOVE CR-DEDUCT  TO D-DEDUCT.
           MOVE CR-PAYED   TO D-PAYED.
           MOVE CR-CK-EFT  TO D-CK-EFT.
           MOVE CR-ICN     TO D-ICN.
           MOVE CR-PAYDENIAL TO D-DENIAL.
           IF CR-PAYDENIAL = SPACES
               MOVE CR-DENIAL1 TO D-DENIAL.
           MOVE CR-INSNAME TO D-INSNAME.
           WRITE PRT-REC FROM DTL.
           ADD CR-BILLED  TO WS-TOT-BILLED.
           ADD CR-ALLOWED TO WS-TOT-ALLOW.
           ADD CR-DEDUCT  TO WS-TOT-DEDUCT.
           ADD CR-PAYED   TO WS-TOT-PAYED.
           ADD 1 TO WS-CNT.
           GO TO P1.
       P9.
           MOVE SPACES TO PRT-REC.
           WRITE PRT-REC.
           MOVE WS-TOT-BILLED TO S-BILLED.
           MOVE WS-TOT-ALLOW  TO S-ALLOWED.
           MOVE WS-TOT-DEDUCT TO S-DEDUCT.
           MOVE WS-TOT-PAYED  TO S-PAYED.
           WRITE PRT-REC FROM SUM1.
           MOVE WS-CNT TO S-CNT.
           WRITE PRT-REC FROM SUM2.
           CLOSE CAREFILE PRTFILE.
           DISPLAY "carepay: " WS-CNT " payment(s) written"
               UPON SYSERR.
           STOP RUN.
