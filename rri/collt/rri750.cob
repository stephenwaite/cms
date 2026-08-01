      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <stephen.waite@cmsvt.com>
      * @author  Claude (Anthropic) <https://claude.ai>
      * @copyright Copyright (c) 2026 cms 
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rri750.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
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
           SELECT FILEIN ASSIGN TO "S50"
             ORGANIZATION LINE SEQUENTIAL.
           SELECT PARMFILE ASSIGN TO "S55"
             ORGANIZATION LINE SEQUENTIAL.
           SELECT GAROUT ASSIGN TO "S60"
             ORGANIZATION LINE SEQUENTIAL.
           SELECT CHAROUT ASSIGN TO "S65"
             ORGANIZATION LINE SEQUENTIAL.
           SELECT PAYOUT ASSIGN TO "S70"
             ORGANIZATION LINE SEQUENTIAL.
           SELECT CHARNEW ASSIGN TO "S75"
             ORGANIZATION LINE SEQUENTIAL.
           SELECT PAYNEW ASSIGN TO "S80"
             ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  GARFILE.
           COPY GARFILE.CPY.
       FD  CHARCUR.
           COPY CHARCUR.CPY.
       FD  PAYCUR.
           COPY PAYCUR.CPY.
       FD  FILEIN.
       01  FILEIN01                   PIC X(8).
       FD  PARMFILE.
       01  PARMFILE01.
           02 PF1                     PIC 9(8).
           02 PF2                     PIC 9(8).
       FD  GAROUT.
       01  GAROUT01                   PIC X(315).
       FD  CHAROUT.
       01  CHAROUT01                  PIC X(160).
       FD  PAYOUT.
       01  PAYOUT01                   PIC X(50).
       FD  CHARNEW.
       01  CHARNEW01                  PIC X(160).
       FD  PAYNEW.
       01  PAYNEW01                   PIC X(50).
       WORKING-STORAGE SECTION.
       01  PHR01.
           02 PHR02 OCCURS 999 TIMES INDEXED BY PHR.
             03 PHR-KEY8              PIC X(8).
             03 PHR-KEY3              PIC 999.
             03 PHR-AMOUNT            PIC S9(4)V99.
             03 PHR-PAYCODE           PIC 999.
             03 PHR-DENIAL            PIC XX.
             03 PHR-CLAIM             PIC 9(6).
             03 PHR-DATE-T            PIC 9(8).
             03 PHR-DATE-E            PIC 9(8).
             03 PHR-BATCH             PIC X(6).
       01  CLAIM-TOT                  PIC S9(6)V99.
       01  X USAGE IS INDEX.
       01  FLAGX                      PIC 9.
       01  GARFLAG                    PIC 9.
       77  ACCT-CNT                   PIC 9(7) VALUE ZERO.
       77  ARCH-CNT                   PIC 9(7) VALUE ZERO.
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT  GARFILE PARMFILE FILEIN
           OPEN OUTPUT GAROUT PAYOUT CHAROUT CHARNEW PAYNEW
           OPEN INPUT  PAYCUR CHARCUR.
           READ PARMFILE AT END
                DISPLAY "NO DELETE RANGE"
                GO TO R20
           END-READ.
           DISPLAY "PF1 (archive charges before): " PF1.
           DISPLAY "PF2 (skip if activity after): " PF2.
       P1.
           READ FILEIN AT END GO TO R20.
           ADD 1 TO ACCT-CNT.
           IF FUNCTION MOD(ACCT-CNT, 1000) = 0
              DISPLAY ACCT-CNT " scanned, " ARCH-CNT " archived"
           END-IF.
           MOVE FILEIN01 TO G-GARNO.
           START GARFILE KEY NOT < G-GARNO INVALID GO TO P1.
       R0.
           READ GARFILE NEXT AT END GO TO P1.
           IF G-GARNO NOT = FILEIN01 GO TO P1.
           SET PHR TO 1.
           MOVE 0       TO GARFLAG.
           MOVE G-GARNO TO PC-KEY8
           MOVE 000     TO PC-KEY3.
           START PAYCUR KEY > PAYCUR-KEY INVALID GO TO R2.
       R5.
           READ PAYCUR NEXT AT END GO TO R2.
           IF G-GARNO NOT = PC-KEY8 GO TO R2.
           IF PHR > 999
              DISPLAY G-GARNO " " G-GARNAME " PHR OVERFLOW"
              GO TO R0.
           MOVE PC-KEY8    TO PHR-KEY8(PHR)
           MOVE PC-KEY3    TO PHR-KEY3(PHR)
           MOVE PC-AMOUNT  TO PHR-AMOUNT(PHR)
           MOVE PC-PAYCODE TO PHR-PAYCODE(PHR)
           MOVE PC-DENIAL  TO PHR-DENIAL(PHR)
           MOVE PC-CLAIM   TO PHR-CLAIM(PHR)
           MOVE PC-DATE-T  TO PHR-DATE-T(PHR)
           MOVE PC-DATE-E  TO PHR-DATE-E(PHR)
           MOVE PC-BATCH   TO PHR-BATCH(PHR)
           SET PHR UP BY 1.
           GO TO R5.
       R2.
           SET PHR DOWN BY 1.
           MOVE G-GARNO TO CC-KEY8
           MOVE "000"   TO CC-KEY3.
           START CHARCUR KEY > CHARCUR-KEY INVALID GO TO R30.
       R6.
           READ CHARCUR NEXT AT END GO TO R30.
           IF G-GARNO NOT = CC-KEY8 GO TO R30.
           IF CC-DATE-T > PF1
              WRITE CHARNEW01 FROM CHARCUR01
              PERFORM A6 THRU A6-EXIT
                VARYING X FROM 1 BY 1 UNTIL X > PHR
              GO TO R6.
           COMPUTE CLAIM-TOT = CC-AMOUNT
           MOVE 0 TO FLAGX
           PERFORM PH3 VARYING X FROM 1 BY 1 UNTIL X > PHR.
           IF (CLAIM-TOT NOT = 0) OR (FLAGX NOT = 0)
              WRITE CHARNEW01 FROM CHARCUR01
              PERFORM A6 THRU A6-EXIT
                VARYING X FROM 1 BY 1 UNTIL X > PHR
              GO TO R6.
           PERFORM A5 THRU A5-EXIT
              VARYING X FROM 1 BY 1 UNTIL X > PHR.
           MOVE 1 TO GARFLAG.
           WRITE CHAROUT01 FROM CHARCUR01.
           GO TO R6.
       PH3.
           IF CC-CLAIM = PHR-CLAIM(X)
              ADD PHR-AMOUNT(X) CLAIM-TOT GIVING CLAIM-TOT
              IF PHR-DATE-T(X) > PF2
                 MOVE 1 TO FLAGX.
       A5.
           IF PHR-CLAIM(X) NOT = CC-CLAIM GO TO A5-EXIT.
           WRITE PAYOUT01 FROM PHR02(X).
       A5-EXIT. EXIT.
       A6.
           IF PHR-CLAIM(X) NOT = CC-CLAIM GO TO A6-EXIT.
           WRITE PAYNEW01 FROM PHR02(X).
       A6-EXIT. EXIT.
       R30.
           IF GARFLAG = 1
              WRITE GAROUT01 FROM GARFILE01
              ADD 1 TO ARCH-CNT.
           GO TO P1.
       R20.
           DISPLAY "Accounts scanned: " ACCT-CNT.
           DISPLAY "Accounts archived (1+ charge): " ARCH-CNT.
           CLOSE GARFILE CHARCUR PAYCUR FILEIN PARMFILE
                 GAROUT CHAROUT PAYOUT CHARNEW PAYNEW.
           STOP RUN.