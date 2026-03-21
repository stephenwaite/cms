       IDENTIFICATION DIVISION.
       PROGRAM-ID. zeror003.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
               ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
               LOCK MODE MANUAL.
           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
               ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
               LOCK MODE MANUAL.
           SELECT PAYCUR ASSIGN TO "S40" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
               LOCK MODE MANUAL.
           SELECT FILEIN ASSIGN TO "S50" ORGANIZATION IS
               LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  CHARCUR.
           COPY CHARCUR.CPY.
       FD  PAYCUR.
           COPY PAYCUR.CPY.
       FD  GARFILE.
           COPY GARFILE.CPY.
       FD  FILEIN.
       01  FILEIN01.
           05  FI-GARNO            PIC X(8).
           05  FILLER              PIC X(164).
           05  FI-DATE.
               10  FI-MM           PIC X(2).
               10  FILLER          PIC X.
               10  FI-DD           PIC X(2).
               10  FILLER          PIC X.
               10  FI-YYYY         PIC X(4).
           05  FILLER              PIC X(339).
       WORKING-STORAGE SECTION.
       01  CLAIM-TOT               PIC S9(6)V99.
       01  CLAIM-DISP              PIC -ZZZ,ZZZ.99.
       01  WS-DATE                 PIC X(8).
       01  WS-PAY-TABLE.
           05  WS-PAY-ENTRY        OCCURS 200 TIMES
                                   INDEXED BY WS-IDX.
               10  WS-PC-CLAIM     PIC X(8).
               10  WS-PC-AMOUNT    PIC S9(6)V99.
       01  WS-PAY-CNT              PIC S9(4) COMP VALUE 0.
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT GARFILE CHARCUR PAYCUR FILEIN.
       R1.
           READ FILEIN
               AT END
                   GO TO R99.
           MOVE 0 TO CLAIM-TOT.
           MOVE 0 TO WS-PAY-CNT.
      *    -- CONVERT MM/DD/YYYY TO YYYYMMDD --
           MOVE FI-YYYY TO WS-DATE(1:4).
           MOVE FI-MM   TO WS-DATE(5:2).
           MOVE FI-DD   TO WS-DATE(7:2).
           DISPLAY "FILEIN GARNO: " FI-GARNO " DATE: " WS-DATE.
           MOVE FI-GARNO TO G-GARNO.
           READ GARFILE
               INVALID KEY
                   DISPLAY "GARNO NOT FOUND: " FI-GARNO
                   GO TO R1
           END-READ.
           DISPLAY "GARFILE READ OK: " G-GARNO.
      *    -- LOAD PAYCUR INTO TABLE --
           MOVE G-GARNO TO PC-KEY8.
           MOVE SPACE TO PC-KEY3.
           START PAYCUR KEY NOT < PAYCUR-KEY
               INVALID
                   DISPLAY "PAYCUR START INVALID FOR " G-GARNO
                   GO TO R3.
       R2.
           READ PAYCUR NEXT
               AT END
                   GO TO R3.
           IF G-GARNO NOT = PC-KEY8
               DISPLAY "PAYCUR KEY BREAK AT " PC-KEY8
               GO TO R3.
           DISPLAY "PAYCUR: CLAIM=" PC-CLAIM " AMT=" PC-AMOUNT.
           IF WS-PAY-CNT < 200
               ADD 1 TO WS-PAY-CNT
               MOVE PC-CLAIM  TO WS-PC-CLAIM(WS-PAY-CNT)
               MOVE PC-AMOUNT TO WS-PC-AMOUNT(WS-PAY-CNT)
           ELSE
               DISPLAY "PAY TABLE FULL FOR GARNO " G-GARNO
           END-IF.
           GO TO R2.
       R3.
           DISPLAY "PAYCUR LOADED " WS-PAY-CNT " ENTRIES FOR " G-GARNO.
      *    -- SUM CHARCUR, MATCH AGAINST PAY TABLE --
           MOVE G-GARNO TO CC-KEY8.
           MOVE SPACE TO CC-KEY3.
           START CHARCUR KEY NOT < CHARCUR-KEY
               INVALID
                   DISPLAY "CHARCUR START INVALID FOR " G-GARNO
                   GO TO R5.
       R4.
           READ CHARCUR NEXT
               AT END
                   GO TO R5.
           IF G-GARNO NOT = CC-KEY8
               DISPLAY "CHARCUR KEY BREAK AT " CC-KEY8
               GO TO R5.
           DISPLAY "CHARCUR: CLAIM=" CC-CLAIM " DATE-T=" CC-DATE-T
               " PAYCODE=" CC-PAYCODE " AMT=" CC-AMOUNT.
           IF CC-PAYCODE NOT = "018"
               DISPLAY "  PAYCODE SKIP: " CC-PAYCODE
               GO TO R4.
           IF CC-DATE-T > WS-DATE
               DISPLAY "  DATE-T SKIP: " CC-DATE-T " > " WS-DATE
               GO TO R4.
           DISPLAY "  MATCH, ADDING " CC-AMOUNT.
           ADD CC-AMOUNT TO CLAIM-TOT.
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-PAY-CNT
               IF WS-PC-CLAIM(WS-IDX) = CC-CLAIM
                   DISPLAY "  PAY MATCH: CLAIM=" WS-PC-CLAIM(WS-IDX)
                       " AMT=" WS-PC-AMOUNT(WS-IDX)
                   ADD WS-PC-AMOUNT(WS-IDX) TO CLAIM-TOT
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           GO TO R4.
       R5.
           MOVE CLAIM-TOT TO CLAIM-DISP.
           DISPLAY "RESULT: " G-GARNO " " WS-DATE
               " BALANCE: " CLAIM-DISP.
           GO TO R1.
       R99.
           CLOSE GARFILE CHARCUR PAYCUR FILEIN.
           STOP RUN.