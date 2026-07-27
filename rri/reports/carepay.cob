       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAREPAY.
      * @package cms
      * @author s waite
      * @author Claude
      * list all payments on carefile for a garno / dos / cpt
      * params via environment: GARNO, DOS, CPT
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CAREFILE ASSIGN TO "S1"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CARE-KEY
               FILE STATUS IS WS-FS.
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
       WORKING-STORAGE SECTION.
       01  WS-FS         PIC XX VALUE SPACES.
       01  WS-GARNO      PIC X(8) VALUE SPACES.
       01  WS-DOS        PIC X(8) VALUE SPACES.
       01  WS-CPT        PIC X(5) VALUE SPACES.
       01  WS-CNT        PIC 9(4) VALUE ZERO.
       01  WS-TOT-PAYED  PIC 9(6)V99 VALUE ZERO.
       01  WS-PAYED-E    PIC Z(3)9.99.
       01  WS-ALLOWED-E  PIC Z(3)9.99.
       01  WS-DEDUCT-E   PIC Z(3)9.99.
       01  WS-TOT-E      PIC Z(5)9.99.
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
               DISPLAY "carepay: open fs " WS-FS UPON SYSERR
               STOP RUN.
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
           MOVE CR-PAYED   TO WS-PAYED-E.
           MOVE CR-ALLOWED TO WS-ALLOWED-E.
           MOVE CR-DEDUCT  TO WS-DEDUCT-E.
           DISPLAY CR-KEY8 " " CR-DATE " " CR-PROC " "
               CR-MOD1 CR-MOD2
               " paydate " CR-PAYDATE
               " payed "   WS-PAYED-E
               " allow "   WS-ALLOWED-E
               " deduct "  WS-DEDUCT-E
               " ck-eft "  CR-CK-EFT
               " " CR-INSNAME
               UPON SYSERR.
           ADD CR-PAYED TO WS-TOT-PAYED.
           ADD 1 TO WS-CNT.
           GO TO P1.
       P9.
           MOVE WS-TOT-PAYED TO WS-TOT-E.
           DISPLAY "carepay: " WS-CNT " payment(s), total payed "
               WS-TOT-E UPON SYSERR.
           CLOSE CAREFILE.
           STOP RUN.
