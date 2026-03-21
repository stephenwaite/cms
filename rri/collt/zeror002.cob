       IDENTIFICATION DIVISION.
       PROGRAM-ID. zeror002.
       AUTHOR. SID WAITE.
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
           05  FI-GARNO        PIC X(8).
           05  FILLER          PIC X(513).
       WORKING-STORAGE SECTION.
       01  CLAIM-TOT           PIC S9(6)V99.
       01  CLAIM-DISP          PIC -ZZZ,ZZZ.99.
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT GARFILE CHARCUR PAYCUR FILEIN.
       R1.
           READ FILEIN
               AT END
                   GO TO R99.
           MOVE 0 TO CLAIM-TOT.
           MOVE FI-GARNO TO G-GARNO.
           READ GARFILE
               INVALID KEY
                   DISPLAY "GARNO NOT FOUND: " FI-GARNO
                   GO TO R1
           END-READ.
      *    -- SUM PAYCUR --
           MOVE G-GARNO TO PC-KEY8.
           MOVE SPACE TO PC-KEY3.
           START PAYCUR KEY NOT < PAYCUR-KEY
               INVALID
                   GO TO R3.
       R2.
           READ PAYCUR NEXT
               AT END
                   GO TO R3.
           IF G-GARNO NOT = PC-KEY8
               GO TO R3.
           ADD PC-AMOUNT TO CLAIM-TOT.
           GO TO R2.
       R3.
      *    -- SUM CHARCUR --
           MOVE G-GARNO TO CC-KEY8.
           MOVE SPACE TO CC-KEY3.
           START CHARCUR KEY NOT < CHARCUR-KEY
               INVALID
                   GO TO R5.
       R4.
           READ CHARCUR NEXT
               AT END
                   GO TO R5.
           IF G-GARNO NOT = CC-KEY8
               GO TO R5.
           ADD CC-AMOUNT TO CLAIM-TOT.
           GO TO R4.
       R5.
           MOVE CLAIM-TOT TO CLAIM-DISP.
           DISPLAY G-GARNO " BALANCE: " CLAIM-DISP.
           GO TO R1.
       R99.
           CLOSE GARFILE CHARCUR PAYCUR FILEIN.
           STOP RUN.