      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @author  Claude
      * @copyright Copyright (c) 2026 cms <cmswest@sover.net>
      *
      *  cptfind - read a tab delimited worklist carrying MRN (G-ACCT)
      *            and DOS, resolve MRN to GARNO through the GARFILE
      *            G-ACCT alternate key, then scan CHARCUR for that
      *            GARNO looking for a charge on DOS matching the
      *            wanted CPT (default 70450).  Reports GARNO and the
      *            full CHARCUR-KEY of each hit.
      *
      *  input layout (tab delimited, optional header row):
      *     1 MRN   2 FIN   3 NAME (LNAME;FNAME M)   4 INS   5 DOS
      *     00009999  99999999  LNAME;FNAME M  003  03/30/2026
      *
      *  slots
      *     S30 garfile   S35 charcur   S55 filein   S60 report
      *
      *  environment
      *     CPTWANT   cpt to hunt, default 70450
      *     CPTDEBUG  1 = stderr trace of every charcur record read
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cptfind.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT GARFILE ASSIGN TO "S30"    ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC        RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           FILE STATUS IS GAR-STAT
           LOCK MODE MANUAL.

           SELECT CHARCUR ASSIGN TO "S35"    ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC        RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           FILE STATUS IS CHR-STAT
           LOCK MODE MANUAL.

           SELECT FILEIN ASSIGN TO "S55" ORGANIZATION
           LINE SEQUENTIAL
           FILE STATUS IS FIN-STAT.

           SELECT REPORT-FILE ASSIGN TO "S60" ORGANIZATION
           LINE SEQUENTIAL
           FILE STATUS IS RPT-STAT.

       DATA DIVISION.
       FILE SECTION.

       FD  GARFILE.
           COPY garfile.CPY.

       FD  CHARCUR.
           COPY CHARCUR.CPY.

       FD  FILEIN.
       01  FILEIN01 PIC X(256).

       FD  REPORT-FILE.
       01  REPORT-FILE01 PIC X(132).

       WORKING-STORAGE SECTION.

       01  STATUS-FLAGS.
           02 GAR-STAT PIC XX VALUE SPACE.
           02 CHR-STAT PIC XX VALUE SPACE.
           02 FIN-STAT PIC XX VALUE SPACE.
           02 RPT-STAT PIC XX VALUE SPACE.

       01  WS-TAB PIC X VALUE X"09".

       01  WS-SW.
           02 EOF-FLAG      PIC 9 VALUE 0.
           02 GAR-EOF       PIC 9 VALUE 0.
           02 CHR-EOF       PIC 9 VALUE 0.
           02 HIT-FLAG      PIC 9 VALUE 0.
           02 ACCT-HIT      PIC 9 VALUE 0.
           02 DEBUG-FLAG    PIC 9 VALUE 0.

       01  WS-ENV.
           02 ENV-CPT   PIC X(10) VALUE SPACE.
           02 ENV-DEBUG PIC X(10) VALUE SPACE.

       01  WS-PARM.
           02 CPT-WANT PIC X(5) VALUE "70450".

       01  WS-IN.
           02 F-MRN  PIC X(20).
           02 F-FIN  PIC X(20).
           02 F-NAME PIC X(40).
           02 F-INS  PIC X(10).
           02 F-DOS  PIC X(20).

       01  WS-WORK.
           02 WS-ACCT     PIC X(20).
           02 SAVE-ACCT   PIC X(20).
           02 SAVE-GARNO  PIC X(11).
           02 WS-NAME3    PIC XXX.
           02 WS-NAMCHK   PIC X.
           02 WS-DATE-T   PIC X(8).

       01  WS-DATE-PARTS.
           02 DP-M PIC X(4).
           02 DP-D PIC X(4).
           02 DP-Y PIC X(4).
           02 DP-MM PIC XX.
           02 DP-DD PIC XX.
           02 DP-YYYY PIC X(4).

       01  WS-CNTR.
           02 C-IN     PIC 9(7) VALUE 0.
           02 C-SKIP   PIC 9(7) VALUE 0.
           02 C-NOACCT PIC 9(7) VALUE 0.
           02 C-NOCHG  PIC 9(7) VALUE 0.
           02 C-HIT    PIC 9(7) VALUE 0.
           02 C-GAR    PIC 9(7) VALUE 0.

       01  WS-TRACE PIC X(132).

       01  HDR-1.
           02 FILLER PIC X(10) VALUE "MRN".
           02 FILLER PIC X(11) VALUE "DOS".
           02 FILLER PIC X(12) VALUE "GARNO".
           02 FILLER PIC X(14) VALUE "CHARCUR-KEY".
           02 FILLER PIC X(7)  VALUE "CPT".
           02 FILLER PIC X(11) VALUE "MODS".
           02 FILLER PIC X(11) VALUE "AMOUNT".
           02 FILLER PIC X(8)  VALUE "CLAIM".
           02 FILLER PIC X(5)  VALUE "PAY".
           02 FILLER PIC X(4)  VALUE "NCK".

       01  HDR-2 PIC X(132) VALUE ALL "-".

       01  DL1.
           02 DL-MRN PIC X(9).
           02 FILLER PIC X VALUE SPACE.
           02 DL-DOS PIC X(10).
           02 FILLER PIC X VALUE SPACE.
           02 DL-GARNO PIC X(11).
           02 FILLER PIC X VALUE SPACE.
           02 DL-CCKEY PIC X(13).
           02 FILLER PIC X VALUE SPACE.
           02 DL-CPT PIC X(6).
           02 FILLER PIC X VALUE SPACE.
           02 DL-MODS PIC X(10).
           02 FILLER PIC X VALUE SPACE.
           02 DL-AMT PIC ZZZZ9.99.
           02 FILLER PIC XXX VALUE SPACE.
           02 DL-CLAIM PIC X(7).
           02 FILLER PIC X VALUE SPACE.
           02 DL-PAY PIC X(4).
           02 FILLER PIC X VALUE SPACE.
           02 DL-NCK PIC X.

       01  TL1.
           02 FILLER PIC X(22) VALUE SPACE.
           02 TL-LIT PIC X(30).
           02 TL-CNT PIC ZZZZZZ9.

       PROCEDURE DIVISION.

       P00.
           MOVE SPACE TO ENV-CPT ENV-DEBUG
           ACCEPT ENV-CPT FROM ENVIRONMENT "CPTWANT"
             ON EXCEPTION
               CONTINUE
           END-ACCEPT
           IF ENV-CPT NOT = SPACE
               MOVE ENV-CPT(1:5) TO CPT-WANT
           END-IF

           ACCEPT ENV-DEBUG FROM ENVIRONMENT "CPTDEBUG"
             ON EXCEPTION
               CONTINUE
           END-ACCEPT
           IF ENV-DEBUG(1:1) = "1"
               MOVE 1 TO DEBUG-FLAG
           END-IF

           OPEN INPUT GARFILE
           IF GAR-STAT NOT = "00"
               DISPLAY "cptfind: garfile open " GAR-STAT UPON SYSERR
               STOP RUN
           END-IF

           OPEN INPUT CHARCUR
           IF CHR-STAT NOT = "00"
               DISPLAY "cptfind: charcur open " CHR-STAT UPON SYSERR
               STOP RUN
           END-IF

           OPEN INPUT FILEIN
           IF FIN-STAT NOT = "00"
               DISPLAY "cptfind: filein open " FIN-STAT UPON SYSERR
               STOP RUN
           END-IF

           OPEN OUTPUT REPORT-FILE
           IF RPT-STAT NOT = "00"
               DISPLAY "cptfind: report open " RPT-STAT UPON SYSERR
               STOP RUN
           END-IF

           MOVE SPACE TO REPORT-FILE01
           STRING "cptfind - cpt " CPT-WANT DELIMITED BY SIZE
               INTO REPORT-FILE01
           WRITE REPORT-FILE01
           MOVE SPACE TO REPORT-FILE01
           WRITE REPORT-FILE01 FROM HDR-1
           MOVE SPACE TO REPORT-FILE01
           WRITE REPORT-FILE01 FROM HDR-2.

      *  ---- read the worklist --------------------------------------

       P1.
           MOVE SPACE TO FILEIN01
           READ FILEIN
             AT END
               MOVE 1 TO EOF-FLAG
           END-READ

           IF EOF-FLAG = 1
               GO TO P9
           END-IF

           IF FILEIN01 = SPACE
               GO TO P1
           END-IF

           MOVE SPACE TO F-MRN F-FIN F-NAME F-INS F-DOS
           UNSTRING FILEIN01 DELIMITED BY WS-TAB
               INTO F-MRN F-FIN F-NAME F-INS F-DOS
           END-UNSTRING

           IF F-MRN(1:3) = "MRN"
               GO TO P1
           END-IF

           ADD 1 TO C-IN

           PERFORM NORM-DATE

           IF WS-DATE-T = SPACE OR F-MRN = SPACE
               ADD 1 TO C-SKIP
               MOVE SPACE TO DL1
               MOVE F-MRN TO DL-MRN
               MOVE F-DOS TO DL-DOS
               MOVE "*BAD INPUT" TO DL-GARNO
               PERFORM WRITE-LINE
               GO TO P1
           END-IF

           MOVE SPACE TO WS-ACCT
           MOVE F-MRN TO WS-ACCT
           MOVE SPACE TO WS-NAME3
           MOVE F-NAME(1:3) TO WS-NAME3

           MOVE 0 TO ACCT-HIT HIT-FLAG GAR-EOF.

      *  ---- mrn to garno through the g-acct alternate key ----------

       P2.
           MOVE SPACE TO G-ACCT
           MOVE WS-ACCT TO G-ACCT
           MOVE G-ACCT TO SAVE-ACCT

           START GARFILE KEY NOT < G-ACCT
             INVALID
               MOVE 1 TO GAR-EOF
           END-START

           IF GAR-EOF = 1
               GO TO P8-NOACCT
           END-IF.

       P3.
           READ GARFILE NEXT
             AT END
               MOVE 1 TO GAR-EOF
           END-READ

           IF GAR-EOF = 1
               GO TO P8-NOACCT
           END-IF

           IF G-ACCT NOT = SAVE-ACCT
               GO TO P8-NOACCT
           END-IF

           MOVE 1 TO ACCT-HIT
           ADD 1 TO C-GAR
           MOVE G-GARNO TO SAVE-GARNO

           IF DEBUG-FLAG = 1
               MOVE SPACE TO WS-TRACE
               STRING "GAR acct=" SAVE-ACCT(1:12)
                      " garno=" SAVE-GARNO DELIMITED BY SIZE
                   INTO WS-TRACE
               DISPLAY WS-TRACE UPON SYSERR
           END-IF

           MOVE 0 TO CHR-EOF.

      *  ---- scan that garno's charges -----------------------------

       P4.
           MOVE SAVE-GARNO TO CC-KEY8
           MOVE "000" TO CC-KEY3

           START CHARCUR KEY NOT < CHARCUR-KEY
             INVALID
               MOVE 1 TO CHR-EOF
           END-START

           IF CHR-EOF = 1
               GO TO P3
           END-IF.

       P5.
           READ CHARCUR NEXT
             AT END
               MOVE 1 TO CHR-EOF
           END-READ

           IF CHR-EOF = 1
               GO TO P3
           END-IF

           IF CC-KEY8 NOT = SAVE-GARNO
               GO TO P3
           END-IF

           IF DEBUG-FLAG = 1
               MOVE SPACE TO WS-TRACE
               STRING "CHG key=" CHARCUR-KEY
                      " dos=" CC-DATE-T
                      " cpt=" CC-CPT
                      " want=" WS-DATE-T " " CPT-WANT
                   DELIMITED BY SIZE INTO WS-TRACE
               DISPLAY WS-TRACE UPON SYSERR
           END-IF

           IF CC-DATE-T NOT = WS-DATE-T
               GO TO P5
           END-IF

           IF CC-CPT NOT = CPT-WANT
               GO TO P5
           END-IF

           MOVE 1 TO HIT-FLAG
           ADD 1 TO C-HIT

           MOVE SPACE TO WS-NAMCHK
           IF WS-NAME3 NOT = SPACE
               IF SAVE-GARNO(1:3) NOT = WS-NAME3
                   MOVE "*" TO WS-NAMCHK
               END-IF
           END-IF

           MOVE SPACE TO DL1
           MOVE F-MRN TO DL-MRN
           MOVE F-DOS TO DL-DOS
           MOVE SAVE-GARNO TO DL-GARNO
           MOVE CHARCUR-KEY TO DL-CCKEY
           MOVE CC-CPT TO DL-CPT
           MOVE SPACE TO DL-MODS
           STRING CC-MOD " " CC-MOD2 " " CC-MOD3
               DELIMITED BY SIZE INTO DL-MODS
           MOVE CC-AMOUNT TO DL-AMT
           MOVE CC-CLAIM TO DL-CLAIM
           MOVE CC-PAYCODE TO DL-PAY
           MOVE WS-NAMCHK TO DL-NCK
           PERFORM WRITE-LINE

           GO TO P5.

      *  ---- nothing found for this worklist row --------------------

       P8-NOACCT.
           IF ACCT-HIT = 0
               ADD 1 TO C-NOACCT
               MOVE SPACE TO DL1
               MOVE F-MRN TO DL-MRN
               MOVE F-DOS TO DL-DOS
               MOVE "*NO ACCT" TO DL-GARNO
               PERFORM WRITE-LINE
               GO TO P1
           END-IF

           IF HIT-FLAG = 0
               ADD 1 TO C-NOCHG
               MOVE SPACE TO DL1
               MOVE F-MRN TO DL-MRN
               MOVE F-DOS TO DL-DOS
               MOVE SAVE-GARNO TO DL-GARNO
               MOVE "*NO CHARGE" TO DL-CCKEY
               MOVE CPT-WANT TO DL-CPT
               PERFORM WRITE-LINE
           END-IF

           GO TO P1.

      *  ---- wrap up ------------------------------------------------

       P9.
           MOVE SPACE TO REPORT-FILE01
           WRITE REPORT-FILE01
           MOVE SPACE TO REPORT-FILE01
           WRITE REPORT-FILE01 FROM HDR-2

           MOVE "INPUT ROWS" TO TL-LIT
           MOVE C-IN TO TL-CNT
           WRITE REPORT-FILE01 FROM TL1

           MOVE "GARNOS MATCHED ON MRN" TO TL-LIT
           MOVE C-GAR TO TL-CNT
           WRITE REPORT-FILE01 FROM TL1

           MOVE "CHARGES FOUND" TO TL-LIT
           MOVE C-HIT TO TL-CNT
           WRITE REPORT-FILE01 FROM TL1

           MOVE "MRN NOT IN GARFILE" TO TL-LIT
           MOVE C-NOACCT TO TL-CNT
           WRITE REPORT-FILE01 FROM TL1

           MOVE "NO CHARGE ON DOS" TO TL-LIT
           MOVE C-NOCHG TO TL-CNT
           WRITE REPORT-FILE01 FROM TL1

           MOVE "UNPARSABLE ROWS" TO TL-LIT
           MOVE C-SKIP TO TL-CNT
           WRITE REPORT-FILE01 FROM TL1

           CLOSE GARFILE CHARCUR FILEIN REPORT-FILE
           STOP RUN.

      *  ---- mm/dd/yyyy to ccyymmdd --------------------------------

       NORM-DATE.
           MOVE SPACE TO WS-DATE-T
           MOVE SPACE TO DP-M DP-D DP-Y DP-MM DP-DD DP-YYYY
           UNSTRING F-DOS DELIMITED BY "/" INTO DP-M DP-D DP-Y
           END-UNSTRING

           IF DP-M(2:1) = SPACE
               MOVE "0" TO DP-MM(1:1)
               MOVE DP-M(1:1) TO DP-MM(2:1)
           ELSE
               MOVE DP-M(1:2) TO DP-MM
           END-IF

           IF DP-D(2:1) = SPACE
               MOVE "0" TO DP-DD(1:1)
               MOVE DP-D(1:1) TO DP-DD(2:1)
           ELSE
               MOVE DP-D(1:2) TO DP-DD
           END-IF

           IF DP-Y(3:1) = SPACE
               MOVE "20" TO DP-YYYY(1:2)
               MOVE DP-Y(1:2) TO DP-YYYY(3:2)
           ELSE
               MOVE DP-Y(1:4) TO DP-YYYY
           END-IF

           IF DP-MM NOT NUMERIC OR DP-DD NOT NUMERIC
               OR DP-YYYY NOT NUMERIC
               MOVE SPACE TO WS-DATE-T
           ELSE
               STRING DP-YYYY DP-MM DP-DD DELIMITED BY SIZE
                   INTO WS-DATE-T
           END-IF.

       WRITE-LINE.
           MOVE SPACE TO REPORT-FILE01
           WRITE REPORT-FILE01 FROM DL1.
