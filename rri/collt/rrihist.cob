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
           02 FILLER                  PIC X(135).
       FD  PAYOUT.
       01  PAYOUT01.
           02 PO-KEY8                 PIC X(8).
           02 PO-KEY3                 PIC 999.
           02 PO-AMOUNT               PIC S9(4)V99.
           02 PO-PAYCODE              PIC 999.
           02 PO-DENIAL               PIC XX.
           02 PO-CLAIM                PIC 9(6).
           02 PO-DATE-T               PIC 9(8).
           02 PO-DATE-E               PIC 9(8).
           02 PO-BATCH                PIC X(6).
       FD  HISFILE
           BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS HISFILE01.
       01  HISFILE01.
           02 HISFILE-KEY.
              03 HS-KEY8              PIC X(8).
              03 HS-CLAIM             PIC X(6).
              03 HS-REC-TYPE          PIC X.
              03 HS-KEY4              PIC X(4).
           02 HS-BODY                 PIC X(116).
       01  PAYHIS01 REDEFINES HISFILE01.
           02 PH-KEY.
              03 PH-KEY8              PIC X(8).
              03 PH-CLAIM             PIC X(6).
              03 PH-REC-TYPE          PIC X.
              03 PH-KEY4              PIC X(4).
           02 PC1.
              03 PC1-IND              PIC X.
              03 PC1-AMOUNT           PIC S9(4)V99.
              03 PC1-PAYCODE          PIC X(3).
              03 PC1-DENIAL           PIC XX.
              03 PC1-DATE-T           PIC X(8).
              03 PC1-DATE-E           PIC X(8).
              03 PC1-BATCH            PIC X(6).
           02 PC1-PAD                 PIC X(10).
           02 PC2.
              03 PC2-IND              PIC X.
              03 PC2-AMOUNT           PIC S9(4)V99.
              03 PC2-PAYCODE          PIC X(3).
              03 PC2-DENIAL           PIC XX.
              03 PC2-DATE-T           PIC X(8).
              03 PC2-DATE-E           PIC X(8).
              03 PC2-BATCH            PIC X(6).
           02 PH-FUTURE               PIC X(38).
       WORKING-STORAGE SECTION.
       01  PHR01.
           02 PHR02 OCCURS 999 TIMES INDEXED BY PXR.
              03 PHR-AMOUNT           PIC S9(4)V99.
              03 PHR-PAYCODE          PIC 999.
              03 PHR-DENIAL           PIC XX.
              03 PHR-CLAIM            PIC 9(6).
              03 PHR-DATE-T           PIC 9(8).
              03 PHR-DATE-E           PIC 9(8).
              03 PHR-BATCH            PIC X(6).
       77  HOLD8                      PIC X(8).
       77  PHR-CNT                    PIC 9(4) VALUE ZERO.
       77  PC-SLOT                    PIC 9    VALUE ZERO.
       77  KEY4                       PIC 9(4) VALUE ZERO.
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

      *>   When KEY8 changes, reload PHR table from PAYOUT
           IF CO-KEY8 NOT = HOLD8
              MOVE CO-KEY8 TO HOLD8
              MOVE 0 TO PHR-CNT
              PERFORM LOAD-PHR.

      *>   Reset KEY4 probe for each charge
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
           MOVE CHAROUT01 TO HISFILE01
           MOVE CO-KEY8   TO HS-KEY8
           MOVE CO-CLAIM  TO HS-CLAIM
           MOVE "1"       TO HS-REC-TYPE
           MOVE KEY4      TO HS-KEY4.
           WRITE HISFILE01 INVALID KEY
                DISPLAY "DUP CHARGE: " HS-KEY8 " " HS-CLAIM
                        " " HS-KEY4
           END-WRITE.

      *>   Pack payments for this claim into PC1/PC2 pairs
           MOVE 0 TO PC-SLOT.
           PERFORM PACK-PAYS
              VARYING PXR FROM 1 BY 1 UNTIL PXR > PHR-CNT.

      *>   Flush any partial pair (odd payment count for this claim)
           IF PC-SLOT = 1
              PERFORM WRITE-PAYHIS.

           GO TO P00.

      *>   ─── Load all payments for HOLD8 into PHR table ───
       LOAD-PHR.
           IF EOF-PAYOUT = "Y" EXIT PARAGRAPH.
           IF PO-KEY8 NOT = HOLD8 EXIT PARAGRAPH.
           ADD 1 TO PHR-CNT.
           IF PHR-CNT > 999
              DISPLAY "PHR OVERFLOW: " HOLD8
              GO TO LOAD-PHR-NEXT.
           MOVE PO-AMOUNT  TO PHR-AMOUNT(PHR-CNT)
           MOVE PO-PAYCODE TO PHR-PAYCODE(PHR-CNT)
           MOVE PO-DENIAL  TO PHR-DENIAL(PHR-CNT)
           MOVE PO-CLAIM   TO PHR-CLAIM(PHR-CNT)
           MOVE PO-DATE-T  TO PHR-DATE-T(PHR-CNT)
           MOVE PO-DATE-E  TO PHR-DATE-E(PHR-CNT)
           MOVE PO-BATCH   TO PHR-BATCH(PHR-CNT).
       LOAD-PHR-NEXT.
           READ PAYOUT AT END
                MOVE "Y" TO EOF-PAYOUT
                EXIT PARAGRAPH
           END-READ.
           GO TO LOAD-PHR.

      *>   ─── Pack a single payment into PC1 or PC2 ───
       PACK-PAYS.
           IF PHR-CLAIM(PXR) NOT = CO-CLAIM EXIT PARAGRAPH.
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
              MOVE ZERO              TO PC2-AMOUNT
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
              PERFORM WRITE-PAYHIS.

      *>   ─── Write the payment record, probe for next KEY4 ───
       WRITE-PAYHIS.
           MOVE CO-KEY8  TO PH-KEY8
           MOVE CO-CLAIM TO PH-CLAIM
           MOVE "2"      TO PH-REC-TYPE
           MOVE SPACES   TO PH-FUTURE.
       P-PAY-KEY.
           ADD 1 TO KEY4.
           MOVE KEY4 TO PH-KEY4.
           READ HISFILE INVALID GO TO P-PAY-WRITE.
           GO TO P-PAY-KEY.
       P-PAY-WRITE.
           WRITE HISFILE01 INVALID KEY
                DISPLAY "DUP PAY: " PH-KEY8 " " PH-CLAIM
                        " " PH-KEY4
           END-WRITE.
           MOVE 0 TO PC-SLOT.

       P-DONE.
           DISPLAY "Charges written: " CHARS-CNT.
           DISPLAY "Payments packed: " PAYS-CNT.
           CLOSE CHAROUT PAYOUT HISFILE.
           STOP RUN.