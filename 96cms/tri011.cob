       IDENTIFICATION DIVISION.
       PROGRAM-ID. HIS011.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HISFILE ASSIGN TO "S185" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS HISFILE-KEY
           LOCK MODE MANUAL.
       DATA DIVISION.
       FILE SECTION.
       FD  HISFILE
           BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS HISFILE01.
       01  HISFILE01.
           02 HISFILE-KEY.
             03 HI-KEY8 PIC X(8).
             03 HI-CLAIM PIC X(6).
             03 HI-REC-TYPE PIC X.
             03 HI-KEY4 PIC XXXX.
           02 HI-PATID PIC X(8).
           02 HI-SERVICE PIC X.
           02 HI-DIAG PIC X(5).
           02 HI-PROC PIC X(7).
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
       WORKING-STORAGE SECTION.
       01 PAYHIS01.
           02 PAYHIS-KEY.
             03 PH-KEY8 PIC X(8).
             03 PH-CLAIM PIC X(6).
             03 PH-REC-TYPE PIC X.
             03 PH-KEY4 PIC XXXX.
           02 PC1-IND PIC 9.
           02 PC1-AMOUNT PIC S9(4)V99.
           02 PC1-PAYCODE PIC XXX.
           02 PC1-DENIAL PIC XX.
           02 PC1-DATE-T PIC X(8).
           02 PC1-DATE-E PIC X(8).
           02 PC1-BATCH  PIC X(6).
           02 PC1-FUTURE PIC X(10).
           02 PC2-IND PIC 9.
           02 PC2-AMOUNT PIC S9(4)V99.
           02 PC2-PAYCODE PIC XXX.
           02 PC2-DENIAL PIC XX.
           02 PC2-DATE-T PIC X(8).
           02 PC2-DATE-E PIC X(8).
           02 PC2-BATCH  PIC X(6).
           02 PC2-FUTURE PIC X(10).
           02 PH-FUTURE PIC X(24).
       01  NEF-8    PIC Z,ZZZ.99CR.
       01  X USAGE IS INDEX.
       01  ANS PIC X.
       01  YYY PIC 999.
       01  ALF-7 PIC X(7) VALUE SPACE.
       01  HI-AMOUNT PIC S9(4)V99.
       01  HI-AMOUNTY PIC 9(6).
       LINKAGE SECTION.
       01 ACTION PIC XXX.
       01 G-GARNO PIC X(8).
       PROCEDURE DIVISION USING ACTION G-GARNO.
       P0.
           OPEN INPUT HISFILE
           MOVE 0 TO YYY
           MOVE SPACE TO HISFILE-KEY
           MOVE G-GARNO TO HI-KEY8.
           START HISFILE KEY > HISFILE-KEY INVALID
           DISPLAY "NO HISTORY RECORDS FOUND" GO TO P5.
       P1. READ HISFILE NEXT AT END GO TO P5.
           IF HI-KEY8 NOT = G-GARNO GO TO P5.
           IF HI-REC-TYPE = "1" GO TO A1.
           IF ACTION = "HSC" GO TO P1.
           GO TO A2.
       A1.
           IF ACTION = "HSP" GO TO P1.
           MOVE HI-AMOUNTX TO HI-AMOUNTY
           COMPUTE HI-AMOUNT = HI-AMOUNTY / 100
           MOVE HI-AMOUNT TO NEF-8
           DISPLAY HI-PATID " "  HI-DATE-TMM "/" 
           HI-DATE-TDD "/" HI-DATE-TCC HI-DATE-TYY
           " PC " HI-PAYCODE " CLAIM " HI-CLAIM
           " AMOUNT " NEF-8 " " HI-PROC " " HI-DIAG " " HI-DOCP
           ADD 1 TO YYY
           IF YYY > 15 MOVE 0 TO YYY ACCEPT ANS
           IF ANS NOT = SPACE GO TO P5.
           GO TO P1.
       A2.
           MOVE HISFILE01 TO PAYHIS01
           MOVE SPACE TO ANS
           IF PC1-IND = 1 PERFORM C1.
           IF ANS NOT = SPACE GO TO P5.
           IF PC2-IND = 2 PERFORM C2.
           IF ANS NOT = SPACE GO TO P5.
           GO TO P1.
       C1. MOVE PC1-AMOUNT TO NEF-8
           MOVE PC1-DATE-T TO HI-DATE-T
           MOVE PC1-PAYCODE TO HI-PAYCODE
           MOVE PC1-DENIAL TO HI-WORK
           PERFORM A3 THRU A3-EXIT.
       C2. MOVE PC2-AMOUNT TO NEF-8
           MOVE PC2-DATE-T TO HI-DATE-T
           MOVE PC2-PAYCODE TO HI-PAYCODE
           MOVE PC2-DENIAL TO HI-WORK
           PERFORM A3 THRU A3-EXIT.
       A3.
      *    MOVE HI-AMOUNT TO NEF-8
           DISPLAY "          "  " "  HI-DATE-TMM "/" HI-DATE-TDD 
           "/" HI-DATE-TCC HI-DATE-TYY
           "    " HI-PAYCODE " " HI-WORK
           "     " NEF-8
           ADD 1 TO YYY
           IF YYY > 15 MOVE 0 TO YYY ACCEPT ANS.
      *    IF ANS NOT = SPACE GO TO P5.
      *    GO TO P1.
       A3-EXIT. EXIT.
       P5.  CLOSE HISFILE
           EXIT PROGRAM.
