       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAREPAY.
      * @package cms
      * @author s waite
      * @author Claude
      * line by line payment report for a garno / dos / cpt
      * reads caredetl (payment history), not carefile
      * params via environment: GARNO, DOS, CPT
      * S1 caredetl, S2 report out
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CAREDETL ASSIGN TO "S1"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS DT-KEY
               FILE STATUS IS WS-FS.
           SELECT PRTFILE ASSIGN TO "S2"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS2.
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
           02 DT-BILLED PIC S9(4)V99.
           02 DT-ALLOWED PIC S9(4)V99.
           02 DT-DEDUCT  PIC S9(4)V99.
           02 DT-PAYED   PIC S9(4)V99.
           02 DT-DENIAL1 PIC X(4).
           02 DT-DENIAL2 PIC X(4).
           02 DT-DENIAL3 PIC X(4).
           02 DT-DENIAL4 PIC X(4).
           02 DT-PAYDENIAL PIC X(4).
           02 DT-INSNAME PIC X(30).
           02 DT-TB PIC X.
           02 DT-ADJ01.
              03 DT-ADJ OCCURS 6 TIMES.
                 04 DT-GRP PIC XX.
                 04 DT-RC PIC X(5).
                 04 DT-AMT PIC S9(4)V99.
       FD  PRTFILE.
       01  PRT-REC PIC X(132).
       WORKING-STORAGE SECTION.
       01  WS-FS         PIC XX VALUE SPACES.
       01  WS-FS2        PIC XX VALUE SPACES.
       01  WS-GARNO      PIC X(8) VALUE SPACES.
       01  WS-DOS        PIC X(8) VALUE SPACES.
       01  WS-CPT        PIC X(5) VALUE SPACES.
       01  WS-CNT        PIC 9(4) VALUE ZERO.
       01  ADJ-LINE      PIC X(132).
       01  PTR           PIC 999.
       01  A             PIC 9.
       01  WS-AMT-E      PIC Z(4)9.99-.
       01  WS-TOT-BILLED PIC S9(6)V99 VALUE ZERO.
       01  WS-TOT-ALLOW  PIC S9(6)V99 VALUE ZERO.
       01  WS-TOT-DEDUCT PIC S9(6)V99 VALUE ZERO.
       01  WS-TOT-PAYED  PIC S9(6)V99 VALUE ZERO.
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
           02 FILLER PIC X(10) VALUE "  billed".
           02 FILLER PIC X(10) VALUE " allowed".
           02 FILLER PIC X(10) VALUE "  deduct".
           02 FILLER PIC X(10) VALUE "   payed".
           02 FILLER PIC X(3)  VALUE "tb".
           02 FILLER PIC X(10) VALUE "ck-eft".
           02 FILLER PIC X(14) VALUE "icn".
           02 FILLER PIC X(9)  VALUE "denial".
           02 FILLER PIC X(30) VALUE "insname".
       01  DTL.
           02 D-PAYDATE PIC X(8).
           02 FILLER    PIC X VALUE SPACE.
           02 D-MOD1    PIC XX.
           02 D-MOD2    PIC XX.
           02 FILLER    PIC X VALUE SPACE.
           02 D-BILLED  PIC Z(4)9.99-.
           02 FILLER    PIC X VALUE SPACE.
           02 D-ALLOWED PIC Z(4)9.99-.
           02 FILLER    PIC X VALUE SPACE.
           02 D-DEDUCT  PIC Z(4)9.99-.
           02 FILLER    PIC X VALUE SPACE.
           02 D-PAYED   PIC Z(4)9.99-.
           02 FILLER    PIC X VALUE SPACE.
           02 D-TB      PIC XX.
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
           02 S-BILLED  PIC Z(5)9.99-.
           02 FILLER    PIC X VALUE SPACE.
           02 S-ALLOWED PIC Z(5)9.99-.
           02 FILLER    PIC X VALUE SPACE.
           02 S-DEDUCT  PIC Z(5)9.99-.
           02 FILLER    PIC X VALUE SPACE.
           02 S-PAYED   PIC Z(5)9.99-.
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
           OPEN INPUT CAREDETL.
           IF WS-FS NOT = "00"
               DISPLAY "carepay: caredetl open fs " WS-FS UPON SYSERR
               STOP RUN.
           OPEN OUTPUT PRTFILE.
           IF WS-FS2 NOT = "00"
               DISPLAY "carepay: prtfile open fs " WS-FS2 UPON SYSERR
               CLOSE CAREDETL
               STOP RUN.
           MOVE WS-GARNO TO H1-GARNO.
           MOVE WS-DOS   TO H1-DOS.
           MOVE WS-CPT   TO H1-CPT.
           WRITE PRT-REC FROM HDR1.
           WRITE PRT-REC FROM HDR2.
           INITIALIZE CAREDETL01.
           MOVE WS-GARNO TO DT-KEY8.
           MOVE WS-DOS   TO DT-DATE.
           MOVE WS-CPT   TO DT-PROC.
           MOVE LOW-VALUES TO DT-MOD1 DT-MOD2 DT-PAYDATE
               DT-CK-EFT DT-ICN.
           MOVE 0 TO DT-SEQ.
           START CAREDETL KEY NOT < DT-KEY.
           IF WS-FS NOT = "00" AND WS-FS NOT = "02"
               GO TO P9.
       P1.
           READ CAREDETL NEXT.
           IF WS-FS = "10"
               GO TO P9.
           IF WS-FS NOT = "00" AND WS-FS NOT = "02"
               DISPLAY "carepay: read fs " WS-FS UPON SYSERR
               GO TO P9.
           IF DT-KEY8 NOT = WS-GARNO
               OR DT-DATE NOT = WS-DOS
               OR DT-PROC NOT = WS-CPT
               GO TO P9.
           MOVE DT-PAYDATE TO D-PAYDATE.
           MOVE DT-MOD1    TO D-MOD1.
           MOVE DT-MOD2    TO D-MOD2.
           MOVE DT-BILLED  TO D-BILLED.
           MOVE DT-ALLOWED TO D-ALLOWED.
           MOVE DT-DEDUCT  TO D-DEDUCT.
           MOVE DT-PAYED   TO D-PAYED.
           MOVE SPACE TO D-TB.
           IF DT-TB = "T" OR DT-PAYED < 0
               MOVE "TB" TO D-TB.
           MOVE DT-CK-EFT  TO D-CK-EFT.
           MOVE DT-ICN     TO D-ICN.
           MOVE DT-PAYDENIAL TO D-DENIAL.
           IF DT-PAYDENIAL = SPACES
               MOVE DT-DENIAL1 TO D-DENIAL.
           MOVE DT-INSNAME TO D-INSNAME.
           WRITE PRT-REC FROM DTL.
           MOVE SPACES TO ADJ-LINE
           MOVE 10 TO PTR
           PERFORM VARYING A FROM 1 BY 1 UNTIL A > 6
               IF DT-RC(A) NOT = SPACES
                   MOVE DT-AMT(A) TO WS-AMT-E
                   STRING DT-GRP(A) DT-RC(A) " " WS-AMT-E "  "
                       DELIMITED BY SIZE INTO ADJ-LINE
                       WITH POINTER PTR
               END-IF
           END-PERFORM
           IF PTR > 10
               WRITE PRT-REC FROM ADJ-LINE.
           ADD DT-BILLED  TO WS-TOT-BILLED.
           ADD DT-ALLOWED TO WS-TOT-ALLOW.
           ADD DT-DEDUCT  TO WS-TOT-DEDUCT.
           ADD DT-PAYED   TO WS-TOT-PAYED.
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
           CLOSE CAREDETL PRTFILE.
           DISPLAY "carepay: " WS-CNT " payment(s) written"
               UPON SYSERR.
           STOP RUN.
