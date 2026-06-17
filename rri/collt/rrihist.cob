      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <stephen.waite@cmsvt.com>
      * @author  Claude (Anthropic) <https://claude.ai>
      * @copyright Copyright (c) 2026 cms 
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrihist.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHAROUT ASSIGN TO "S65"
             ORGANIZATION LINE SEQUENTIAL.
           SELECT HISFILE ASSIGN TO "S70" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS HISFILE-KEY.
           SELECT PAYOUT  ASSIGN TO "S85"
             ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  CHAROUT.
       01  CHAROUT01.
           02 CO-KEY8                 PIC X(8).
           02 CO-KEY3                 PIC X(3).
           02 CO-PATID                PIC X(8).
           02 CO-CLAIM                PIC X(6).
           02 CO-SERVICE              PIC X.
           02 CO-DIAG                 PIC X(7).
           02 CO-PROC                 PIC X(11).
           02 CO-MOD2                 PIC XX.
           02 CO-MOD3                 PIC XX.
           02 CO-MOD4                 PIC XX.
           02 CO-AMOUNT               PIC X(6).
           02 CO-DOCR                 PIC X(3).
           02 CO-DOCP                 PIC X(2).
           02 CO-PAYCODE              PIC X(3).
           02 CO-STUD                 PIC X.
           02 CO-WORK                 PIC XX.
           02 CO-DAT1                 PIC X(8).
           02 CO-RESULT               PIC X.
           02 CO-ACT                  PIC X.
           02 CO-SORCREF              PIC X.
           02 CO-COLLT                PIC X.
           02 CO-AUTH                 PIC X.
           02 CO-PAPER                PIC X.
           02 CO-PLACE                PIC X.
           02 CO-EPSDT                PIC X.
           02 CO-DATE-T               PIC X(8).
           02 CO-DATE-A               PIC X(8).
           02 CO-DATE-P               PIC X(8).
           02 CO-REC-STAT             PIC X.
           02 CO-DX2                  PIC X(7).
           02 CO-DX3                  PIC X(7).
           02 CO-ACC-TYPE             PIC X.
           02 CO-DATE-M               PIC X(8).
           02 CO-ASSIGN               PIC X.
           02 CO-NEIC-ASSIGN          PIC X.
           02 CO-DX4                  PIC X(7).
           02 CO-QP1                  PIC XX.
           02 CO-QP2                  PIC XX.
           02 CO-VISITNO              PIC X(7).
           02 CO-QP3                  PIC XX.
           02 CO-FUTURE               PIC X(7).
       FD  PAYOUT.
       01  PAYOUT01.
           02 PO-KEY8                 PIC X(8).
           02 PO-KEY3                 PIC X(3).
           02 PO-AMOUNT               PIC X(6).
           02 PO-PAYCODE              PIC X(3).
           02 PO-DENIAL               PIC XX.
           02 PO-CLAIM                PIC X(6).
           02 PO-DATE-T               PIC X(8).
           02 PO-DATE-E               PIC X(8).
           02 PO-BATCH                PIC X(6).
       FD  HISFILE.
           COPY HISFILE.CPY.
       WORKING-STORAGE SECTION.
       01  PAYHIS01.
           02 PH-KEY.
              03 PH-KEY8              PIC X(8).
              03 PH-CLAIM             PIC X(6).
              03 PH-REC-TYPE          PIC X.
              03 PH-KEY4              PIC X(4).
           02 PC1.
              03 PC1-IND              PIC X.
              03 PC1-AMOUNT           PIC X(6).
              03 PC1-PAYCODE          PIC X(3).
              03 PC1-DENIAL           PIC XX.
              03 PC1-DATE-T           PIC X(8).
              03 PC1-DATE-E           PIC X(8).
              03 PC1-BATCH            PIC X(6).
           02 PC1-PAD                 PIC X(10).
           02 PC2.
              03 PC2-IND              PIC X.
              03 PC2-AMOUNT           PIC X(6).
              03 PC2-PAYCODE          PIC X(3).
              03 PC2-DENIAL           PIC XX.
              03 PC2-DATE-T           PIC X(8).
              03 PC2-DATE-E           PIC X(8).
              03 PC2-BATCH            PIC X(6).
           02 PH-FUTURE               PIC X(38).
       01  PHR01.
           02 PHR02 OCCURS 999 TIMES.
              03 PHR-AMOUNT           PIC X(6).
              03 PHR-PAYCODE          PIC X(3).
              03 PHR-DENIAL           PIC XX.
              03 PHR-CLAIM            PIC X(6).
              03 PHR-DATE-T           PIC X(8).
              03 PHR-DATE-E           PIC X(8).
              03 PHR-BATCH            PIC X(6).
       77  HOLD8                      PIC X(8).
       77  PHR-CNT                    PIC 9(4) VALUE ZERO.
       77  PC-SLOT                    PIC 9    VALUE ZERO.
       77  KEY4                       PIC 9(4) VALUE ZERO.
       77  PXR                        PIC 9(4).
       77  CHARS-CNT                  PIC 9(7) VALUE ZERO.
       77  PAYS-CNT                   PIC 9(7) VALUE ZERO.
       77  EOF-PAYOUT                 PIC X    VALUE "N".
       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT  CHAROUT PAYOUT
           OPEN I-O    HISFILE.
           MOVE LOW-VALUES TO HOLD8.
           READ PAYOUT AT END MOVE "Y" TO EOF-PAYOUT END-READ.

       P00.
           READ CHAROUT AT END GO TO P-DONE END-READ.
           ADD 1 TO CHARS-CNT.

           IF CO-KEY8 NOT = HOLD8
              MOVE CO-KEY8 TO HOLD8
              MOVE 0 TO PHR-CNT
              PERFORM LOAD-PHR.

           MOVE 0 TO KEY4.
       P-CHRG-KEY.
           ADD 1 TO KEY4.
           MOVE CO-KEY8  TO HS-KEY8
           MOVE CO-CLAIM TO HS-CLAIM
           MOVE "1"      TO HS-REC-TYPE
           MOVE KEY4     TO HS-KEY4.
           READ HISFILE INVALID GO TO P-CHRG-WRITE.
           GO TO P-CHRG-KEY.

       P-CHRG-WRITE.
           MOVE SPACES   TO HISFILE01.
           MOVE CO-KEY8  TO HS-KEY8
           MOVE CO-CLAIM TO HS-CLAIM
           MOVE "1"      TO HS-REC-TYPE
           MOVE KEY4     TO HS-KEY4.
           MOVE CO-PATID(1:7)  TO HS-PATID7
           MOVE CO-PATID(8:1)  TO HS-PATID1
           MOVE CO-SERVICE     TO HS-SERVICE
           MOVE CO-DIAG(1:5)   TO HS-DIAG
           MOVE CO-PROC        TO HS-PROC
           MOVE CO-MOD2        TO HS-MOD2
           MOVE CO-MOD3        TO HS-MOD3
           MOVE CO-MOD4        TO HS-MOD4
           MOVE CO-AMOUNT      TO HS-AMOUNT
           MOVE CO-DOCR        TO HS-DOCR
           MOVE CO-DOCP        TO HS-DOCP
           MOVE CO-PAYCODE     TO HS-PAYCODE
           MOVE CO-STUD        TO HS-STUD
           MOVE CO-WORK        TO HS-WORK
           MOVE CO-DAT1        TO HS-DAT1
           MOVE CO-RESULT      TO HS-RESULT
           MOVE CO-ACT         TO HS-ACT
           MOVE CO-SORCREF     TO HS-SORCREF
           MOVE CO-COLLT       TO HS-COLLT
           MOVE CO-AUTH        TO HS-AGE
           MOVE CO-PAPER       TO HS-PAPER
           MOVE CO-PLACE       TO HS-PLACE
           MOVE CO-EPSDT       TO HS-EPSDT
           MOVE CO-DATE-T      TO HS-DATE-T
           MOVE CO-DATE-A      TO HS-DATE-A
           MOVE CO-DATE-P      TO HS-DATE-E
           MOVE CO-REC-STAT    TO HS-REC-STAT
           MOVE CO-DX2(1:5)    TO HS-DX2
           MOVE CO-DX3(1:5)    TO HS-DX3
           MOVE CO-ACC-TYPE    TO HS-ACC-TYPE
           MOVE CO-DATE-M      TO HS-DATE-M
           MOVE CO-ASSIGN      TO HS-ASSIGN
           MOVE CO-NEIC-ASSIGN TO HS-NEIC-ASSIGN
           MOVE SPACES         TO HS-FUTURE.
           WRITE HISFILE01 INVALID KEY
                DISPLAY "DUP CHARGE: " HS-KEY8 " " HS-CLAIM
                        " " HS-KEY4
           END-WRITE.

           MOVE 0 TO PC-SLOT.
           PERFORM PACK-PAYS
              VARYING PXR FROM 1 BY 1 UNTIL PXR > PHR-CNT.

           IF PC-SLOT = 1
              PERFORM WRITE-PAYHIS THRU P-PAY-WRITE.   

           GO TO P00.

       LOAD-PHR.
           IF EOF-PAYOUT = "N" AND PO-KEY8 = HOLD8
              ADD 1 TO PHR-CNT
              IF PHR-CNT > 999
                 DISPLAY "PHR OVERFLOW: " HOLD8
              ELSE
                 MOVE PO-AMOUNT  TO PHR-AMOUNT(PHR-CNT)
                 MOVE PO-PAYCODE TO PHR-PAYCODE(PHR-CNT)
                 MOVE PO-DENIAL  TO PHR-DENIAL(PHR-CNT)
                 MOVE PO-CLAIM   TO PHR-CLAIM(PHR-CNT)
                 MOVE PO-DATE-T  TO PHR-DATE-T(PHR-CNT)
                 MOVE PO-DATE-E  TO PHR-DATE-E(PHR-CNT)
                 MOVE PO-BATCH   TO PHR-BATCH(PHR-CNT)
              END-IF
              READ PAYOUT AT END MOVE "Y" TO EOF-PAYOUT END-READ
              GO TO LOAD-PHR.

       PACK-PAYS.
           IF PHR-CLAIM(PXR) = CO-CLAIM
              IF PC-SLOT = 0
                 MOVE "1"               TO PC1-IND
                 MOVE PHR-AMOUNT(PXR)   TO PC1-AMOUNT
                 MOVE PHR-PAYCODE(PXR)  TO PC1-PAYCODE
                 MOVE PHR-DENIAL(PXR)   TO PC1-DENIAL
                 MOVE PHR-DATE-T(PXR)   TO PC1-DATE-T
                 MOVE PHR-DATE-E(PXR)   TO PC1-DATE-E
                 MOVE PHR-BATCH(PXR)    TO PC1-BATCH
                 MOVE SPACES            TO PC1-PAD
                 MOVE "0"               TO PC2-IND
                 MOVE "000000"          TO PC2-AMOUNT
                 MOVE "000"             TO PC2-PAYCODE
                 MOVE SPACES            TO PC2-DENIAL
                 MOVE "00000000"        TO PC2-DATE-T PC2-DATE-E
                 MOVE "000000"          TO PC2-BATCH
                 MOVE 1                 TO PC-SLOT
                 ADD 1 TO PAYS-CNT
              ELSE
                 MOVE "2"               TO PC2-IND
                 MOVE PHR-AMOUNT(PXR)   TO PC2-AMOUNT
                 MOVE PHR-PAYCODE(PXR)  TO PC2-PAYCODE
                 MOVE PHR-DENIAL(PXR)   TO PC2-DENIAL
                 MOVE PHR-DATE-T(PXR)   TO PC2-DATE-T
                 MOVE PHR-DATE-E(PXR)   TO PC2-DATE-E
                 MOVE PHR-BATCH(PXR)    TO PC2-BATCH
                 ADD 1 TO PAYS-CNT
                 PERFORM WRITE-PAYHIS THRU P-PAY-WRITE
              END-IF
           END-IF.

       WRITE-PAYHIS.
           MOVE CO-KEY8  TO PH-KEY8
           MOVE CO-CLAIM TO PH-CLAIM
           MOVE "2"      TO PH-REC-TYPE
           MOVE SPACES   TO PH-FUTURE.
       P-PAY-KEY.
           ADD 1 TO KEY4.
           MOVE KEY4 TO PH-KEY4
           MOVE PH-KEY8  TO HS-KEY8
           MOVE PH-CLAIM TO HS-CLAIM
           MOVE "2"      TO HS-REC-TYPE
           MOVE KEY4     TO HS-KEY4.
           READ HISFILE INVALID GO TO P-PAY-WRITE.
           GO TO P-PAY-KEY.
       P-PAY-WRITE.
           WRITE HISFILE01 FROM PAYHIS01 INVALID KEY
                DISPLAY "DUP PAY: " PH-KEY8 " " PH-CLAIM
                        " " PH-KEY4
           END-WRITE.
           MOVE 0 TO PC-SLOT.

       P-DONE.
           DISPLAY "Charges written: " CHARS-CNT.
           DISPLAY "Payments packed: " PAYS-CNT.
           DISPLAY "Press any key to exit..."
           CLOSE CHAROUT PAYOUT HISFILE.
           STOP RUN.