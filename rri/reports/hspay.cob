      * @package cms
      * @author  s waite <cmswest@sover.net>
      * @author  Claude
      * healthspring pc underpayment: pluck payerid 63092 lines
      * from iedidetl, expected = medfile x .98 (contract seq).
      * tsv output with header row for calc import.
      * end-of-line tag for sorting:
      *   (blank) underpaid, in TOTAL DUE
      *   OK      within +/- 1.00 of expected, excluded
      *   OVER    paid above expected, excluded
      *   TB      takeback, excluded
      *   NO26    mod1 not 26, excluded, EXP approximate
      *   NOFEE   proc/mod not in medfile, excluded
      * S1 iedidetl  S31 medfile  S35 report (tsv)
      * DOSYEAR env: "2026" (default) or "all"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. hspay.
       AUTHOR. S WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CAREDETL ASSIGN TO "S1" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS DT-KEY
           FILE STATUS IS WS-FS.
           SELECT MEDFILE2020 ASSIGN TO "S31" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS MED-KEY
           FILE STATUS IS WS-FS2.
           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
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
           02 DT-SENDER  PIC X(15).
           02 DT-PAYERID PIC X(10).
       FD  MEDFILE2020.
       01  MEDFILE202001.
           02  MED-KEY.
               03  MED-KEY1 PIC X(5).
               03  MED-KEY2 PIC XX.
           02  MED-AMT      PIC 9(4)V99.
       FD FILEOUT.
       01 FILEOUT01 PIC X(132).
       WORKING-STORAGE SECTION.
       01 TB           PIC X VALUE X"09".
       01 WS-FS        PIC XX.
       01 WS-FS2       PIC XX.
       01 WS-YEAR      PIC X(4).
       01 WS-TAG       PIC X(5).
       01 WS-EXPECT    PIC 9(5)V99.
       01 WS-DELTA     PIC S9(5)V99.
       01 WS-TOT-DELTA PIC S9(8)V99 VALUE 0.
       01 CT-LINES     PIC 9(5) VALUE 0.
       01 CT-NOFEE     PIC 9(5) VALUE 0.
       01 CT-NOMOD     PIC 9(5) VALUE 0.
       01 CT-TB        PIC 9(5) VALUE 0.
       01 CT-OK        PIC 9(5) VALUE 0.
       01 CT-OVER      PIC 9(5) VALUE 0.
       01 D-PAID       PIC -ZZZ9.99.
       01 D-EXPECT     PIC -ZZZ9.99.
       01 D-DELTA      PIC -ZZZZ9.99.
       01 D-TOT        PIC -Z(6)9.99.
       01 D-CT         PIC ZZZZ9.
      *
       PROCEDURE DIVISION.
        P0.
           ACCEPT WS-YEAR FROM ENVIRONMENT "DOSYEAR"
           IF WS-YEAR = SPACES
               MOVE "2026" TO WS-YEAR
           END-IF
           OPEN INPUT CAREDETL MEDFILE2020
                OUTPUT FILEOUT.
           MOVE SPACE TO FILEOUT01
           STRING "KEY8" TB "ICN" TB "PROC" TB "MOD" TB
               "DOS" TB "PAYDATE" TB "PAID" TB "EXP" TB
               "DUE" TB "TAG" DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           MOVE SPACE TO DT-KEY.
           START CAREDETL KEY NOT < DT-KEY INVALID
               GO TO P99
           END-START.
        P1.
           READ CAREDETL NEXT AT END
               GO TO P99
           END-READ
           IF DT-PAYERID NOT = "63092"
               GO TO P1
           END-IF
           IF WS-YEAR NOT = "all"
             AND DT-DATE(1:4) NOT = WS-YEAR
               GO TO P1
           END-IF
           MOVE SPACE TO WS-TAG
           IF DT-TB = "T"
               MOVE "TB"   TO WS-TAG
               ADD 1 TO CT-TB
           ELSE
               IF DT-MOD1 NOT = "26"
                   MOVE "NO26" TO WS-TAG
                   ADD 1 TO CT-NOMOD
               END-IF
           END-IF
           MOVE DT-PROC TO MED-KEY1
           MOVE DT-MOD1 TO MED-KEY2
           READ MEDFILE2020 INVALID
               ADD 1 TO CT-NOFEE
               MOVE "NOFEE" TO WS-TAG
               MOVE SPACE TO FILEOUT01
               STRING DT-KEY8 TB DT-ICN TB DT-PROC TB DT-MOD1
                   TB DT-DATE TB DT-PAYDATE TB TB TB TB WS-TAG
                   DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P1
           END-READ
           COMPUTE WS-EXPECT ROUNDED = MED-AMT * .98
           COMPUTE WS-DELTA = WS-EXPECT - DT-PAYED
           IF WS-TAG = SPACE
               IF WS-DELTA < 1.00 AND WS-DELTA > -1.00
                   MOVE "OK"   TO WS-TAG
                   ADD 1 TO CT-OK
               ELSE
                   IF WS-DELTA < 0
                       MOVE "OVER" TO WS-TAG
                       ADD 1 TO CT-OVER
                   ELSE
                       ADD WS-DELTA TO WS-TOT-DELTA
                       ADD 1 TO CT-LINES
                   END-IF
               END-IF
           END-IF
           MOVE DT-PAYED  TO D-PAID
           MOVE WS-EXPECT TO D-EXPECT
           MOVE WS-DELTA  TO D-DELTA
           MOVE SPACE TO FILEOUT01
           STRING DT-KEY8 TB DT-ICN TB DT-PROC TB DT-MOD1
               TB DT-DATE TB DT-PAYDATE TB D-PAID TB D-EXPECT
               TB D-DELTA TB WS-TAG
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           GO TO P1.
        P99.
           MOVE CT-LINES TO D-CT
           MOVE WS-TOT-DELTA TO D-TOT
           MOVE SPACE TO FILEOUT01
           STRING "LINES" TB D-CT TB "TOTAL DUE" TB D-TOT
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           MOVE CT-OK TO D-CT
           MOVE SPACE TO FILEOUT01
           STRING "AT-RATE" TB D-CT
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           MOVE CT-OVER TO D-CT
           MOVE SPACE TO FILEOUT01
           STRING "OVERPAID" TB D-CT
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           MOVE CT-TB TO D-CT
           MOVE SPACE TO FILEOUT01
           STRING "TAKEBACK" TB D-CT
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           MOVE CT-NOMOD TO D-CT
           MOVE SPACE TO FILEOUT01
           STRING "NO-26" TB D-CT
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           MOVE CT-NOFEE TO D-CT
           MOVE SPACE TO FILEOUT01
           STRING "NO-FEE" TB D-CT
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           CLOSE CAREDETL MEDFILE2020 FILEOUT.
           STOP RUN.