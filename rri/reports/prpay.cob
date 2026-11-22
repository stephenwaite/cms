      * @package cms
      * @author  s waite <cmswest@sover.net>
      * @author  Claude
      * payer pay listing: every iedidetl 835 line whose payer
      * name (dt-prname, n1*pr) contains PAYER, for a look at how
      * a payer is paying, against a rate when one is given.
      *   A/B  allowed / billed
      *   A/M  allowed / medfile fee (blank: proc/mod not on file
      *        or no medfile)
      * PR = PR-group adjustments summed (patient cost-share).
      * ADJ = every adjustment on the line, group-reason amount.
      * with RATE (the allowed share of billed, .90 or 90):
      *   EXP    billed x rate, the allowed amount expected
      *   SHORT  EXP - allowed
      * the test is on allowed, not paid, so a line paid less for
      * patient cost-share or another payer is not counted.
      * TAG:
      *   (blank) allowed short of EXP, in TOTAL SHORT
      *           (no RATE: a line with none of the tags below)
      *   OK      allowed within +/- 1.00 of EXP, excluded
      *   OVER    allowed above EXP, excluded
      *   TB      takeback, excluded
      *   DENY    nothing allowed and nothing paid, excluded
      *   COB     not at rate and a reason 23 adjustment (another
      *           payer paid first), excluded
      *   MCARE   not at rate and allowed within 2 pct of the
      *           medfile fee (priced as medicare), excluded
      * tsv output with header row for calc import.
      * S1 iedidetl  S31 medfile  S35 report (tsv)  S50 mbrxfile
      * use the medfile of the DOSYEAR asked for.
      * PAYER env: part of the payer name, upper case (required)
      * RATE env: optional, EXP SHORT and OK/OVER left out without
      * MBR env: "Y" adds a last column MEMBID, the member id the
      *   payer returned (mbrxfile, as erar301 reads it). left out
      *   unless asked for.
      * DOSYEAR env: "2026" (default) or "all"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prpay.
       AUTHOR. S WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CAREDETL ASSIGN TO "S1" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS DT-KEY
           FILE STATUS IS WS-FS.
           SELECT OPTIONAL MEDFILE2020 ASSIGN TO "S31"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS MED-KEY
           FILE STATUS IS WS-FS2.
           SELECT OPTIONAL MBRXFILE ASSIGN TO "S50"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS MX-KEY
           FILE STATUS IS WS-FS3.
           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  CAREDETL.
           COPY caredetl.cpy.
       FD  MEDFILE2020.
       01  MEDFILE202001.
           02  MED-KEY.
               03  MED-KEY1 PIC X(5).
               03  MED-KEY2 PIC XX.
           02  MED-AMT      PIC 9(4)V99.
       FD  MBRXFILE.
           COPY mbrxfile.cpy.
       FD FILEOUT.
       01 FILEOUT01 PIC X(340).
       WORKING-STORAGE SECTION.
       01 WS-TAB       PIC X VALUE X"09".
       01 WS-FS        PIC XX.
       01 WS-FS2       PIC XX.
       01 WS-MED       PIC X VALUE "Y".
       01 WS-YEAR      PIC X(4).
       01 WS-PAYER     PIC X(30).
       01 WS-PLEN      PIC 99.
       01 WS-HIT       PIC 99.
       01 WS-TAG       PIC X(5).
       01 WS-PR        PIC S9(5)V99.
       01 WS-RATIO     PIC S9(3)V99.
       01 WS-ADJ       PIC X(100).
       01 WS-FS3       PIC XX.
       01 WS-MBR       PIC X VALUE SPACE.
       01 WS-MEMB      PIC X(20).
       01 WS-OPTR      PIC 999.
       01 WS-COB       PIC X.
       01 WS-MFEE      PIC X.
       01 WS-LOW       PIC S9(5)V99.
       01 WS-HIGH      PIC S9(5)V99.
       01 CT-COB       PIC 9(5) VALUE 0.
       01 CT-MCARE     PIC 9(5) VALUE 0.
       01 WS-RATE-X    PIC X(8).
       01 WS-RATE      PIC 9(3)V9(4) VALUE 0.
       01 WS-RDIV      PIC 9(5) VALUE 1.
       01 WS-RDOT      PIC X VALUE "N".
       01 WS-RDIG      PIC 9.
       01 WS-EXPECT    PIC S9(5)V99.
       01 WS-SHORT     PIC S9(5)V99.
       01 WS-TOT-SHORT PIC S9(8)V99 VALUE 0.
       01 CT-SHORT     PIC 9(5) VALUE 0.
       01 CT-OK        PIC 9(5) VALUE 0.
       01 CT-OVER      PIC 9(5) VALUE 0.
       01 K            PIC 99.
       01 D-EXPECT     PIC X(9).
       01 D-SHORT      PIC X(9).
       01 D-EXPN       PIC -----9.99.
       01 WS-PTR       PIC 999.
       01 WS-TOT-BILL  PIC S9(8)V99 VALUE 0.
       01 WS-TOT-ALLOW PIC S9(8)V99 VALUE 0.
       01 WS-TOT-PAID  PIC S9(8)V99 VALUE 0.
       01 CT-LINES     PIC 9(5) VALUE 0.
       01 CT-TB        PIC 9(5) VALUE 0.
       01 CT-DENY      PIC 9(5) VALUE 0.
       01 J            PIC 9.
       01 D-BILL       PIC ----9.99.
       01 D-ALLOW      PIC ----9.99.
       01 D-PAID       PIC ----9.99.
       01 D-PR         PIC ----9.99.
       01 D-MED        PIC X(8).
       01 D-MEDN       PIC ZZZZ9.99.
       01 D-RAB        PIC X(7).
       01 D-RAM        PIC X(7).
       01 D-RATIO      PIC ---9.99.
       01 D-AMT        PIC ----9.99.
       01 D-TOT        PIC -Z(6)9.99.
       01 D-CT         PIC ZZZZ9.
      *
       PROCEDURE DIVISION.
        P0.
           ACCEPT WS-YEAR FROM ENVIRONMENT "DOSYEAR"
           IF WS-YEAR = SPACES
               MOVE "2026" TO WS-YEAR
           END-IF
           MOVE SPACE TO WS-PAYER
           ACCEPT WS-PAYER FROM ENVIRONMENT "PAYER"
           IF WS-PAYER = SPACES
               DISPLAY "prpay: set PAYER to part of the payer name"
               STOP RUN
           END-IF
      * rate: digits and one point; above 1 is a percent
           MOVE SPACE TO WS-RATE-X
           ACCEPT WS-RATE-X FROM ENVIRONMENT "RATE"
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 8
               IF WS-RATE-X(K:1) = "."
                   MOVE "Y" TO WS-RDOT
               END-IF
               IF WS-RATE-X(K:1) NUMERIC
                   MOVE WS-RATE-X(K:1) TO WS-RDIG
                   COMPUTE WS-RATE = WS-RATE * 10 + WS-RDIG
                   IF WS-RDOT = "Y"
                       COMPUTE WS-RDIV = WS-RDIV * 10
                   END-IF
               END-IF
           END-PERFORM
           COMPUTE WS-RATE = WS-RATE / WS-RDIV
           IF WS-RATE > 1
               COMPUTE WS-RATE = WS-RATE / 100
           END-IF
           MOVE 30 TO WS-PLEN
           PERFORM UNTIL WS-PLEN = 1
                   OR WS-PAYER(WS-PLEN:1) NOT = SPACE
               SUBTRACT 1 FROM WS-PLEN
           END-PERFORM
           OPEN INPUT CAREDETL
           IF WS-FS NOT = "00"
               DISPLAY "prpay: iedidetl open fs " WS-FS
               STOP RUN
           END-IF
      * optional: no medfile (fs 05) leaves the fee columns blank
           OPEN INPUT MEDFILE2020
           IF WS-FS2 NOT = "00"
               MOVE "N" TO WS-MED
               DISPLAY "prpay: no medfile, fs " WS-FS2
               IF WS-FS2 = "05"
                   CLOSE MEDFILE2020
               END-IF
           END-IF
           ACCEPT WS-MBR FROM ENVIRONMENT "MBR"
           IF WS-MBR = "y"
               MOVE "Y" TO WS-MBR
           END-IF
           IF WS-MBR = "Y"
               OPEN INPUT MBRXFILE
               IF WS-FS3 NOT = "00"
                   DISPLAY "prpay: no mbrxfile, fs " WS-FS3
                   IF WS-FS3 = "05"
                       CLOSE MBRXFILE
                   END-IF
                   MOVE "X" TO WS-MBR
               END-IF
           END-IF
           OPEN OUTPUT FILEOUT
           MOVE SPACE TO FILEOUT01
           MOVE 1 TO WS-OPTR
           STRING "KEY8" WS-TAB "ICN" WS-TAB "PAYER" WS-TAB
               "TRACE" WS-TAB "PROC" WS-TAB "MOD1" WS-TAB
               "MOD2" WS-TAB "DOS" WS-TAB "PAYDATE" WS-TAB
               "BILLED" WS-TAB "ALLOWED" WS-TAB "PAID" WS-TAB
               "PR" WS-TAB "MEDFEE" WS-TAB "A/B" WS-TAB
               "A/M" WS-TAB "EXP" WS-TAB "SHORT" WS-TAB
               "TAG" WS-TAB "ADJ"
               DELIMITED BY SIZE INTO FILEOUT01
               WITH POINTER WS-OPTR
           IF WS-MBR NOT = SPACE
               STRING WS-TAB "MEMBID"
                   DELIMITED BY SIZE INTO FILEOUT01
                   WITH POINTER WS-OPTR
           END-IF
           WRITE FILEOUT01
           MOVE SPACE TO DT-KEY.
           START CAREDETL KEY NOT < DT-KEY INVALID
               GO TO P99
           END-START.
        P1.
           READ CAREDETL NEXT AT END
               GO TO P99
           END-READ
           MOVE 0 TO WS-HIT
           INSPECT DT-PRNAME TALLYING WS-HIT
               FOR ALL WS-PAYER(1:WS-PLEN)
           IF WS-HIT = 0
               GO TO P1
           END-IF
           IF WS-YEAR NOT = "all"
             AND DT-DATE(1:4) NOT = WS-YEAR
               GO TO P1
           END-IF
           ADD 1 TO CT-LINES
           MOVE SPACE TO WS-TAG
           IF DT-TB = "T"
               MOVE "TB" TO WS-TAG
               ADD 1 TO CT-TB
           ELSE
               IF DT-ALLOWED = 0 AND DT-PAYED = 0
                   MOVE "DENY" TO WS-TAG
                   ADD 1 TO CT-DENY
               END-IF
           END-IF
           ADD DT-BILLED  TO WS-TOT-BILL
           ADD DT-ALLOWED TO WS-TOT-ALLOW
           ADD DT-PAYED   TO WS-TOT-PAID
      * patient responsibility and the adjustment list
           MOVE 0 TO WS-PR
           MOVE "N" TO WS-COB WS-MFEE
           MOVE SPACE TO WS-ADJ
           MOVE 1 TO WS-PTR
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 6
               IF DT-GRP(J) = "PR"
                   ADD DT-AMT(J) TO WS-PR
               END-IF
               IF DT-RC(J) = "23"
                   MOVE "Y" TO WS-COB
               END-IF
               IF DT-GRP(J) NOT = SPACE
                   MOVE DT-AMT(J) TO D-AMT
                   STRING DT-GRP(J) "-" DELIMITED BY SIZE
                       DT-RC(J) DELIMITED BY SPACE
                       " " D-AMT "  " DELIMITED BY SIZE
                       INTO WS-ADJ POINTER WS-PTR
               END-IF
           END-PERFORM
           MOVE SPACE TO D-RAB D-RAM D-MED
           IF DT-BILLED NOT = 0
               COMPUTE WS-RATIO ROUNDED = DT-ALLOWED / DT-BILLED
               MOVE WS-RATIO TO D-RATIO
               MOVE D-RATIO TO D-RAB
           END-IF
           IF WS-MED = "Y"
               MOVE DT-PROC TO MED-KEY1
               MOVE DT-MOD1 TO MED-KEY2
               READ MEDFILE2020
                   INVALID CONTINUE
                   NOT INVALID
                       MOVE MED-AMT TO D-MEDN
                       MOVE D-MEDN TO D-MED
                       IF MED-AMT NOT = 0
                           COMPUTE WS-RATIO ROUNDED =
                               DT-ALLOWED / MED-AMT
                           MOVE WS-RATIO TO D-RATIO
                           MOVE D-RATIO TO D-RAM
                           COMPUTE WS-LOW = MED-AMT * .98
                           COMPUTE WS-HIGH = MED-AMT * 1.02
                           IF DT-ALLOWED NOT < WS-LOW
                             AND DT-ALLOWED NOT > WS-HIGH
                               MOVE "Y" TO WS-MFEE
                           END-IF
                       END-IF
               END-READ
           END-IF
           MOVE SPACE TO D-EXPECT D-SHORT
           IF WS-RATE NOT = 0
               COMPUTE WS-EXPECT ROUNDED = DT-BILLED * WS-RATE
               COMPUTE WS-SHORT = WS-EXPECT - DT-ALLOWED
               MOVE WS-EXPECT TO D-EXPN
               MOVE D-EXPN TO D-EXPECT
               MOVE WS-SHORT TO D-EXPN
               MOVE D-EXPN TO D-SHORT
               IF WS-TAG = SPACE
                   IF WS-SHORT < 1.00 AND WS-SHORT > -1.00
                       MOVE "OK" TO WS-TAG
                       ADD 1 TO CT-OK
                   ELSE
                       IF WS-SHORT < 0
                           MOVE "OVER" TO WS-TAG
                           ADD 1 TO CT-OVER
                       END-IF
                   END-IF
               END-IF
           END-IF
      * not at rate (or no rate given): another payer first, or
      * priced as medicare; what is left is the shortfall
           IF WS-TAG = SPACE
               IF WS-COB = "Y"
                   MOVE "COB" TO WS-TAG
                   ADD 1 TO CT-COB
               ELSE
                   IF WS-MFEE = "Y"
                       MOVE "MCARE" TO WS-TAG
                       ADD 1 TO CT-MCARE
                   ELSE
                       IF WS-RATE NOT = 0
                           ADD WS-SHORT TO WS-TOT-SHORT
                           ADD 1 TO CT-SHORT
                       END-IF
                   END-IF
               END-IF
           END-IF
           MOVE SPACE TO WS-MEMB
           IF WS-MBR = "Y"
               MOVE DT-KEY8 TO MX-KEY8
               MOVE DT-PAYERID TO MX-PAYERID
               IF MX-PAYERID = SPACE
                   MOVE DT-SENDER TO MX-PAYERID
               END-IF
               READ MBRXFILE
                   INVALID CONTINUE
                   NOT INVALID MOVE MX-MEMBID TO WS-MEMB
               END-READ
           END-IF
           MOVE DT-BILLED  TO D-BILL
           MOVE DT-ALLOWED TO D-ALLOW
           MOVE DT-PAYED   TO D-PAID
           MOVE WS-PR      TO D-PR
           MOVE SPACE TO FILEOUT01
           MOVE 1 TO WS-OPTR
           STRING DT-KEY8 WS-TAB DT-ICN WS-TAB DT-PRNAME WS-TAB
               DT-CK-EFT WS-TAB DT-PROC WS-TAB DT-MOD1 WS-TAB
               DT-MOD2 WS-TAB DT-DATE WS-TAB DT-PAYDATE WS-TAB
               D-BILL WS-TAB D-ALLOW WS-TAB D-PAID WS-TAB
               D-PR WS-TAB D-MED WS-TAB D-RAB WS-TAB
               D-RAM WS-TAB D-EXPECT WS-TAB D-SHORT WS-TAB
               WS-TAG WS-TAB WS-ADJ
               DELIMITED BY SIZE INTO FILEOUT01
               WITH POINTER WS-OPTR
           IF WS-MBR NOT = SPACE
               STRING WS-TAB WS-MEMB
                   DELIMITED BY SIZE INTO FILEOUT01
                   WITH POINTER WS-OPTR
           END-IF
           WRITE FILEOUT01
           GO TO P1.
        P99.
           MOVE CT-LINES TO D-CT
           MOVE SPACE TO FILEOUT01
           STRING "LINES" WS-TAB D-CT
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           MOVE WS-TOT-BILL TO D-TOT
           MOVE SPACE TO FILEOUT01
           STRING "BILLED" WS-TAB D-TOT
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           MOVE WS-TOT-ALLOW TO D-TOT
           MOVE SPACE TO FILEOUT01
           STRING "ALLOWED" WS-TAB D-TOT
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           MOVE WS-TOT-PAID TO D-TOT
           MOVE SPACE TO FILEOUT01
           STRING "PAID" WS-TAB D-TOT
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           MOVE CT-TB TO D-CT
           MOVE SPACE TO FILEOUT01
           STRING "TAKEBACK" WS-TAB D-CT
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           MOVE CT-DENY TO D-CT
           MOVE SPACE TO FILEOUT01
           STRING "DENIED" WS-TAB D-CT
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           MOVE CT-COB TO D-CT
           MOVE SPACE TO FILEOUT01
           STRING "COB" WS-TAB D-CT
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           MOVE CT-MCARE TO D-CT
           MOVE SPACE TO FILEOUT01
           STRING "MCARE" WS-TAB D-CT
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           IF WS-RATE NOT = 0
               MOVE CT-SHORT TO D-CT
               MOVE WS-TOT-SHORT TO D-TOT
               MOVE SPACE TO FILEOUT01
               STRING "SHORT" WS-TAB D-CT WS-TAB "TOTAL SHORT"
                   WS-TAB D-TOT DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               MOVE CT-OK TO D-CT
               MOVE SPACE TO FILEOUT01
               STRING "AT-RATE" WS-TAB D-CT
                   DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               MOVE CT-OVER TO D-CT
               MOVE SPACE TO FILEOUT01
               STRING "OVER" WS-TAB D-CT
                   DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
           END-IF
           CLOSE CAREDETL FILEOUT
           IF WS-MBR = "Y"
               CLOSE MBRXFILE
           END-IF
           IF WS-MED = "Y"
               CLOSE MEDFILE2020
           END-IF
           STOP RUN.
