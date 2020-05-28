       IDENTIFICATION DIVISION.
       PROGRAM-ID. sid816.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO "S30"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S35"
           ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  FILEIN.
       01  filein01.
           02 PAYCUR-KEY.
             03 PC-KEY8 PIC X(8).
             03 PC-KEY3 PIC XXX.
           02 PC-AMOUNT1 PIC X(5).
           02 PCS       PIC X.
           02 PC-PAYCODE PIC XXX.
           02 PC-DENIAL PIC XX.
           02 PC-CLAIM PIC X(6).
           02 PC-DATE-T PIC X(8).
           02 PC-DATE-E PIC 9(8).
           02 PC-BATCH PIC X(6).
       FD  FILEOUT.
       01  FILEOUT01 PIC X(50).
       WORKING-STORAGE SECTION.
       01  ANS PIC X.

       PROCEDURE DIVISION.
       P0.
           OPEN INPUT FILEIN OUTPUT FILEOUT.
       P1.
           READ filein NEXT AT END GO TO P99.
           IF PCS = "}" OR "!" MOVE "p" TO PCS
             GO TO P2.

           IF PCS = "J" MOVE "q" TO PCS
             GO TO P2.

           IF PCS = "K" MOVE "r" TO PCS
             GO TO P2.

           IF PCS = "L" MOVE "s" TO PCS
             GO TO P2.

           IF PCS = "M" MOVE "t" TO PCS
             GO TO P2.

           IF PCS = "N" MOVE "u" TO PCS
             GO TO P2.

           IF PCS = "O" MOVE "v" TO PCS
             GO TO P2.

           IF PCS = "P" MOVE "w" TO PCS
             GO TO P2.

           IF PCS = "Q" MOVE "x" TO PCS
             GO TO P2.

           IF PCS = "R" MOVE "y" TO PCS.


       P2.
           write fileout01 from filein01
           go to p1.
       P99.
           CLOSE filein FILEOUT.
           STOP RUN.
