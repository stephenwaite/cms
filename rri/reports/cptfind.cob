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
      *  a row with no tab in it is read as the fixed layout:
      *     cols  1-8   MRN, zero filled on the left (G-ACCT)
      *     cols  9-11  "000"
      *     cols 24-31  DOS, CCYYMMDD
      *     00999999000            20260330
      *  a fixed row that does not fit (cols 1-8 or the DOS not
      *  numeric, cols 9-11 not "000") is reported as *BAD INPUT.
      *
      *  slots
      *     S30 garfile   S35 charcur   S40 carefile   S55 filein
      *     S60 report    S65 lvam rows, optional
      *     S45 caredetl, optional: the medicare 835 detail file
      *
      *  medicare icn: carefile keeps one entry per line and
      *  carer303 only replaces it when a later remit allows more
      *  or pays a line the first one did not, and it never sees a
      *  takeback, so its icn can be the claim medicare has since
      *  reprocessed or taken back.  when S45 is set the line's
      *  history in caredetl decides instead, for every account
      *  (carefile is only asked when the primary is 003): the
      *  newest remit that is not a takeback, a line with an
      *  allowed or paid amount before a denial.  SRC on the
      *  report says where the icn came from:
      *     DETL   caredetl
      *     DETL*  caredetl, and not the icn carefile holds
      *     CARE   nothing in caredetl for the line, carefile's
      *     TB     taken back after the last payment, or only
      *            takebacks on file: no lvam row
      *     NOID   neither primary nor secondary is 003, or the
      *            policy is blank: no lvam row
      *
      *  lvam rows: when S65 is set, each charge found with a
      *  medicare icn is also written "|" delimited, to be put in
      *  the medicare contractor's LVAM request sheet by
      *  php/rri/lvam_fill.php:
      *     1 NAME (as garfile holds it)
      *     2 medicare id, the policy of whichever of primary and
      *       secondary is 003
      *     3 DOS CCYYMMDD   4 ICN
      *     5 CPT and the mods of the remit entry taken
      *     6 PAID on that entry   7 GARNO   8 G-ACCT
      *     9 ALLOWED and 10 DEDUCTIBLE on that entry: a line
      *       paid 0 was denied (allowed 0) or went to the
      *       deductible
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

           SELECT CAREFILE ASSIGN TO "S40"   ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC        RECORD KEY IS CARE-KEY
           FILE STATUS IS CAR-STAT
           LOCK MODE MANUAL.

           SELECT FILEIN ASSIGN TO "S55" ORGANIZATION
           LINE SEQUENTIAL
           FILE STATUS IS FIN-STAT.

           SELECT REPORT-FILE ASSIGN TO "S60" ORGANIZATION
           LINE SEQUENTIAL
           FILE STATUS IS RPT-STAT.

           SELECT LVAM-FILE ASSIGN TO "S65" ORGANIZATION
           LINE SEQUENTIAL
           FILE STATUS IS LVM-STAT.

           SELECT CAREDETL ASSIGN TO "S45"   ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC        RECORD KEY IS DT-KEY
           FILE STATUS IS DTL-STAT
           LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.

       FD  GARFILE.
           COPY garfile.CPY.

       FD  CHARCUR.
           COPY CHARCUR.CPY.

       FD  CAREFILE.
           COPY carefile.CPY.

       FD  FILEIN.
       01  FILEIN01 PIC X(256).

       FD  REPORT-FILE.
       01  REPORT-FILE01 PIC X(160).

       FD  LVAM-FILE.
       01  LVAM-FILE01 PIC X(160).

       FD  CAREDETL.
           COPY caredetl.CPY.

       WORKING-STORAGE SECTION.

       01  STATUS-FLAGS.
           02 GAR-STAT PIC XX VALUE SPACE.
           02 CHR-STAT PIC XX VALUE SPACE.
           02 CAR-STAT PIC XX VALUE SPACE.
           02 FIN-STAT PIC XX VALUE SPACE.
           02 RPT-STAT PIC XX VALUE SPACE.
           02 LVM-STAT PIC XX VALUE SPACE.
           02 DTL-STAT PIC XX VALUE SPACE.

       01  WS-TAB PIC X VALUE X"09".

       01  WS-SW.
           02 EOF-FLAG      PIC 9 VALUE 0.
           02 GAR-EOF       PIC 9 VALUE 0.
           02 CHR-EOF       PIC 9 VALUE 0.
           02 CAR-EOF       PIC 9 VALUE 0.
           02 HIT-FLAG      PIC 9 VALUE 0.
           02 ACCT-HIT      PIC 9 VALUE 0.
           02 DEBUG-FLAG    PIC 9 VALUE 0.
           02 LVAM-FLAG     PIC 9 VALUE 0.
           02 DETL-FLAG     PIC 9 VALUE 0.
           02 DTL-EOF       PIC 9 VALUE 0.

       01  WS-ENV.
           02 ENV-CPT   PIC X(10) VALUE SPACE.
           02 ENV-DEBUG PIC X(10) VALUE SPACE.
           02 ENV-WIDTH PIC X(10) VALUE SPACE.
           02 ENV-W2    PIC XX VALUE SPACE.
           02 ENV-LVAM  PIC X(80) VALUE SPACE.
           02 ENV-DETL  PIC X(80) VALUE SPACE.

       01  WS-PARM.
           02 CPT-WANT  PIC X(5) VALUE "70450".
           02 MRN-WIDTH PIC 99 VALUE 8.

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
           02 SAVE-PRINS  PIC X(4).
           02 SAVE-DOB    PIC X(10).
           02 SAVE-POL    PIC X(20).
           02 SAVE-ICN    PIC X(13).
           02 SAVE-NAME   PIC X(24).
           02 SAVE-MOD1   PIC XX.
           02 SAVE-MOD2   PIC XX.
           02 SAVE-PAID   PIC 9(4)V99 VALUE 0.
           02 SAVE-ALLOWED PIC 9(4)V99 VALUE 0.
           02 SAVE-DEDUCT PIC 9(4)V99 VALUE 0.
           02 TAB-CNT     PIC 999 VALUE 0.
           02 SAVE-MBI    PIC X(20).
           02 SAVE-SRC    PIC X(5).

      *  caredetl entries for the line: 1 = those with the charge's
      *  mods, 2 = all of them.  PK-RANK 2 = allowed or paid,
      *  1 = denied, 0 = no entry that is not a takeback.

       01  WS-PICK.
           02 PK-SET OCCURS 2 TIMES.
              03 PK-ROWS    PIC 9(4).
              03 PK-RANK    PIC 9.
              03 PK-PAYDATE PIC X(8).
              03 PK-ICN     PIC X(13).
              03 PK-MOD1    PIC XX.
              03 PK-MOD2    PIC XX.
              03 PK-PAID    PIC S9(4)V99.
              03 PK-ALLOWED PIC S9(4)V99.
              03 PK-DEDUCT  PIC S9(4)V99.
              03 PK-TBDATE  PIC X(8).
           02 K             PIC 9 VALUE 0.
           02 WS-RANK       PIC 9 VALUE 0.

       01  WS-ICN-WORK.
           02 FB-ICN      PIC X(13) VALUE SPACE.
           02 FB-MOD1     PIC XX VALUE SPACE.
           02 FB-MOD2     PIC XX VALUE SPACE.
           02 FB-PAID     PIC 9(4)V99 VALUE 0.
           02 FB-ALLOWED  PIC 9(4)V99 VALUE 0.
           02 FB-DEDUCT   PIC 9(4)V99 VALUE 0.
           02 EXACT-FLAG  PIC 9 VALUE 0.
           02 WS-MODX     PIC XX.
           02 WS-MODY     PIC XX.
           02 WS-NAME3    PIC XXX.
           02 WS-NAMCHK   PIC X.
           02 WS-DATE-T   PIC X(8).

      *  g-acct is stored zero filled on the left, so the worklist mrn
      *  is stripped of leading zeros and padded back out to
      *  MRN-WIDTH before the start.  442973 and 00442973 both land
      *  on 00442973.

       01  WS-MRN-WORK.
           02 WS-BARE     PIC X(20).
           02 WS-PAD      PIC X(20).
           02 WS-LEN      PIC 99 VALUE 0.
           02 BARE-LEN    PIC 99 VALUE 0.
           02 PAD-OFF     PIC 99 VALUE 0.
           02 WS-I        PIC 99 VALUE 0.
           02 WS-J        PIC 99 VALUE 0.
           02 ZFLAG       PIC 9 VALUE 0.

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
           02 C-ICN    PIC 9(7) VALUE 0.
           02 C-LVAM   PIC 9(7) VALUE 0.
           02 C-DETL   PIC 9(7) VALUE 0.
           02 C-DIFF   PIC 9(7) VALUE 0.
           02 C-TB     PIC 9(7) VALUE 0.
           02 C-NOID   PIC 9(7) VALUE 0.

       01  WS-LVAM.
           02 LV-PROC  PIC X(11).
           02 LV-PAID  PIC ZZZ9.99.
           02 LV-ALLOWED PIC ZZZ9.99.
           02 LV-DEDUCT PIC ZZZ9.99.

       01  WS-TRACE PIC X(132).

       01  HDR-1.
           02 FILLER PIC X(10) VALUE "MRN".
           02 FILLER PIC X(11) VALUE "DOS".
           02 FILLER PIC X(12) VALUE "GARNO".
           02 FILLER PIC X(5)  VALUE "INS".
           02 FILLER PIC X(11) VALUE "DOB".
           02 FILLER PIC X(21) VALUE "PRIPOL".
           02 FILLER PIC X(14) VALUE "CHARCUR-KEY".
           02 FILLER PIC X(7)  VALUE "CPT".
           02 FILLER PIC X(11) VALUE "MODS".
           02 FILLER PIC X(11) VALUE "AMOUNT".
           02 FILLER PIC X(8)  VALUE "CLAIM".
           02 FILLER PIC X(5)  VALUE "PAY".
           02 FILLER PIC X(14) VALUE "MEDICARE ICN".
           02 FILLER PIC X(4)  VALUE "NCK".
           02 HDR-SRC PIC X(5) VALUE SPACE.

       01  HDR-2 PIC X(160) VALUE ALL "-".

       01  DL1.
           02 DL-MRN PIC X(9).
           02 FILLER PIC X VALUE SPACE.
           02 DL-DOS PIC X(10).
           02 FILLER PIC X VALUE SPACE.
           02 DL-GARNO PIC X(11).
           02 FILLER PIC X VALUE SPACE.
           02 DL-PRINS PIC X(4).
           02 FILLER PIC X VALUE SPACE.
           02 DL-DOB PIC X(10).
           02 FILLER PIC X VALUE SPACE.
           02 DL-POL PIC X(20).
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
           02 DL-ICN PIC X(13).
           02 FILLER PIC X VALUE SPACE.
           02 DL-NCK PIC X.
           02 FILLER PIC XXX VALUE SPACE.
           02 DL-SRC PIC X(5).

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

           ACCEPT ENV-WIDTH FROM ENVIRONMENT "MRNWIDTH"
             ON EXCEPTION
               CONTINUE
           END-ACCEPT
           MOVE SPACE TO ENV-W2
           IF ENV-WIDTH NOT = SPACE
               IF ENV-WIDTH(2:1) = SPACE
                   MOVE "0" TO ENV-W2(1:1)
                   MOVE ENV-WIDTH(1:1) TO ENV-W2(2:1)
               ELSE
                   MOVE ENV-WIDTH(1:2) TO ENV-W2
               END-IF
               IF ENV-W2 NUMERIC
                   MOVE ENV-W2 TO MRN-WIDTH
               END-IF
           END-IF
           IF MRN-WIDTH < 1 OR MRN-WIDTH > 20
               MOVE 8 TO MRN-WIDTH
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

           OPEN INPUT CAREFILE
           IF CAR-STAT NOT = "00"
               DISPLAY "cptfind: carefile open " CAR-STAT UPON SYSERR
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

           ACCEPT ENV-LVAM FROM ENVIRONMENT "S65"
             ON EXCEPTION
               CONTINUE
           END-ACCEPT
           IF ENV-LVAM NOT = SPACE
               OPEN OUTPUT LVAM-FILE
               IF LVM-STAT NOT = "00"
                   DISPLAY "cptfind: lvam open " LVM-STAT UPON SYSERR
                   STOP RUN
               END-IF
               MOVE 1 TO LVAM-FLAG
           END-IF

           ACCEPT ENV-DETL FROM ENVIRONMENT "S45"
             ON EXCEPTION
               CONTINUE
           END-ACCEPT
           IF ENV-DETL NOT = SPACE
               OPEN INPUT CAREDETL
               IF DTL-STAT NOT = "00"
                   DISPLAY "cptfind: caredetl open " DTL-STAT
                       UPON SYSERR
                   STOP RUN
               END-IF
               MOVE 1 TO DETL-FLAG
               MOVE "SRC" TO HDR-SRC
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

           INSPECT FILEIN01 REPLACING ALL X"0D" BY " "

           IF FILEIN01 = SPACE
               GO TO P1
           END-IF

           MOVE 0 TO TAB-CNT
           INSPECT FILEIN01 TALLYING TAB-CNT FOR ALL WS-TAB

           MOVE SPACE TO F-MRN F-FIN F-NAME F-INS F-DOS
           IF TAB-CNT = 0
               PERFORM FIXED-ROW
           ELSE
               UNSTRING FILEIN01 DELIMITED BY WS-TAB
                   INTO F-MRN F-FIN F-NAME F-INS F-DOS
               END-UNSTRING
               IF F-MRN(1:3) = "MRN"
                   GO TO P1
               END-IF
           END-IF

           ADD 1 TO C-IN

           IF TAB-CNT NOT = 0
               PERFORM NORM-DATE
           END-IF

           IF WS-DATE-T = SPACE OR F-MRN = SPACE
               ADD 1 TO C-SKIP
               MOVE SPACE TO DL1
               MOVE F-MRN TO DL-MRN
               MOVE F-DOS TO DL-DOS
               MOVE "*BAD INPUT" TO DL-GARNO
               PERFORM WRITE-LINE
               GO TO P1
           END-IF

           MOVE SPACE TO WS-NAME3
           MOVE F-NAME(1:3) TO WS-NAME3

           PERFORM NORM-MRN THRU NORM-MRN-EXIT

           MOVE 0 TO ACCT-HIT HIT-FLAG GAR-EOF.

      *  ---- mrn to garno through the g-acct alternate key ----------

       P2.
           MOVE SPACE TO G-ACCT
           MOVE WS-ACCT TO G-ACCT
           MOVE G-ACCT TO SAVE-ACCT

           IF DEBUG-FLAG = 1
               MOVE SPACE TO WS-TRACE
               STRING "ACCT [" SAVE-ACCT "]" DELIMITED BY SIZE
                   INTO WS-TRACE
               DISPLAY WS-TRACE UPON SYSERR
           END-IF

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
           MOVE SPACE TO SAVE-PRINS
           MOVE G-PRINS TO SAVE-PRINS
           MOVE SPACE TO SAVE-DOB
           MOVE G-DOB TO SAVE-DOB
           MOVE SPACE TO SAVE-POL
           MOVE G-PRIPOL TO SAVE-POL
           MOVE G-GARNAME TO SAVE-NAME
           MOVE SPACE TO SAVE-MBI
           IF G-PRINS = "003"
               MOVE G-PRIPOL TO SAVE-MBI
           ELSE
               IF G-SEINS = "003"
                   MOVE G-SECPOL TO SAVE-MBI
               END-IF
           END-IF

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

           PERFORM LOOK-ICN THRU LOOK-ICN-EXIT
           MOVE SPACE TO SAVE-SRC
           IF DETL-FLAG = 1
               PERFORM LOOK-DETL THRU LOOK-DETL-EXIT
           END-IF
           IF SAVE-ICN NOT = SPACE
               ADD 1 TO C-ICN
               IF SAVE-MBI = SPACE AND SAVE-SRC NOT = "TB"
                   MOVE "NOID" TO SAVE-SRC
                   ADD 1 TO C-NOID
               END-IF
           END-IF

           MOVE SPACE TO DL1
           MOVE F-MRN TO DL-MRN
           MOVE F-DOS TO DL-DOS
           MOVE SAVE-GARNO TO DL-GARNO
           MOVE SAVE-PRINS TO DL-PRINS
           MOVE SAVE-DOB TO DL-DOB
           MOVE SAVE-POL TO DL-POL
           MOVE CHARCUR-KEY TO DL-CCKEY
           MOVE CC-CPT TO DL-CPT
           MOVE SPACE TO DL-MODS
           STRING CC-MOD " " CC-MOD2 " " CC-MOD3
               DELIMITED BY SIZE INTO DL-MODS
           MOVE CC-AMOUNT TO DL-AMT
           MOVE CC-CLAIM TO DL-CLAIM
           MOVE CC-PAYCODE TO DL-PAY
           MOVE SAVE-ICN TO DL-ICN
           MOVE WS-NAMCHK TO DL-NCK
           MOVE SAVE-SRC TO DL-SRC
           PERFORM WRITE-LINE

           IF LVAM-FLAG = 1 AND SAVE-ICN NOT = SPACE
               AND SAVE-SRC NOT = "TB" AND SAVE-SRC NOT = "NOID"
               PERFORM WRITE-LVAM
           END-IF

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
               MOVE SAVE-PRINS TO DL-PRINS
               MOVE SAVE-DOB TO DL-DOB
               MOVE SAVE-POL TO DL-POL
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

           MOVE "MEDICARE ICNS FOUND" TO TL-LIT
           MOVE C-ICN TO TL-CNT
           WRITE REPORT-FILE01 FROM TL1

           IF DETL-FLAG = 1
               MOVE "LINES FOUND IN CAREDETL" TO TL-LIT
               MOVE C-DETL TO TL-CNT
               WRITE REPORT-FILE01 FROM TL1
               MOVE "ICN NOT THE ONE IN CAREFILE" TO TL-LIT
               MOVE C-DIFF TO TL-CNT
               WRITE REPORT-FILE01 FROM TL1
               MOVE "TAKEN BACK, NO LVAM ROW" TO TL-LIT
               MOVE C-TB TO TL-CNT
               WRITE REPORT-FILE01 FROM TL1
               CLOSE CAREDETL
           END-IF

           IF C-NOID > 0
               MOVE "NO MEDICARE ID, NO LVAM ROW" TO TL-LIT
               MOVE C-NOID TO TL-CNT
               WRITE REPORT-FILE01 FROM TL1
           END-IF

           IF LVAM-FLAG = 1
               MOVE "LVAM ROWS WRITTEN" TO TL-LIT
               MOVE C-LVAM TO TL-CNT
               WRITE REPORT-FILE01 FROM TL1
               CLOSE LVAM-FILE
           END-IF

           CLOSE GARFILE CHARCUR CAREFILE FILEIN REPORT-FILE
           STOP RUN.

      *  ---- medicare icn from carefile ----------------------------
      *
      *  only for ins 003.  CARE-KEY is garno + dos + proc + mod1 +
      *  mod2, so the start lands straight on the line - no scan of
      *  the whole account.  mods are left low on the start and the
      *  prefix is walked, because the mods carefile holds need not
      *  match what charcur holds (26 vs blank).  an exact mod match
      *  wins; otherwise the first entry carrying an icn is taken.
      *  the mods and the paid, allowed and deductible amounts of
      *  the entry taken are kept for the lvam row.

       LOOK-ICN.
           MOVE SPACE TO SAVE-ICN FB-ICN
           MOVE SPACE TO SAVE-MOD1 SAVE-MOD2 FB-MOD1 FB-MOD2
           MOVE 0 TO SAVE-PAID FB-PAID
           MOVE 0 TO SAVE-ALLOWED FB-ALLOWED SAVE-DEDUCT FB-DEDUCT
           MOVE 0 TO CAR-EOF EXACT-FLAG

           IF SAVE-PRINS NOT = "003"
               GO TO LOOK-ICN-EXIT
           END-IF

           MOVE SPACE TO CARE-KEY
           MOVE SAVE-GARNO TO CR-KEY8
           MOVE WS-DATE-T TO CR-DATE
           MOVE CPT-WANT TO CR-PROC
           MOVE SPACE TO CR-MOD1 CR-MOD2

           START CAREFILE KEY NOT < CARE-KEY
             INVALID
               MOVE 1 TO CAR-EOF
           END-START

           IF CAR-EOF = 1
               GO TO LOOK-ICN-EXIT
           END-IF.

       LOOK-ICN-1.
           READ CAREFILE NEXT
             AT END
               MOVE 1 TO CAR-EOF
           END-READ

           IF CAR-EOF = 1
               GO TO LOOK-ICN-DONE
           END-IF

           IF CR-KEY8 NOT = SAVE-GARNO
               GO TO LOOK-ICN-DONE
           END-IF

           IF CR-DATE NOT = WS-DATE-T
               GO TO LOOK-ICN-DONE
           END-IF

           IF CR-PROC NOT = CPT-WANT
               GO TO LOOK-ICN-DONE
           END-IF

           IF DEBUG-FLAG = 1
               MOVE SPACE TO WS-TRACE
               STRING "CAR " CR-KEY8 " " CR-DATE " " CR-PROC
                      " mod=" CR-MOD1 CR-MOD2
                      " icn=" CR-ICN
                      " ins=" CR-INSNAME DELIMITED BY SIZE
                   INTO WS-TRACE
               DISPLAY WS-TRACE UPON SYSERR
           END-IF

           IF CR-ICN = SPACE
               GO TO LOOK-ICN-1
           END-IF

           IF FB-ICN = SPACE
               MOVE CR-ICN TO FB-ICN
               MOVE CR-MOD1 TO FB-MOD1
               MOVE CR-MOD2 TO FB-MOD2
               IF CR-PAID NUMERIC
                   MOVE CR-PAID TO FB-PAID
               END-IF
               IF CR-ALLOWED NUMERIC
                   MOVE CR-ALLOWED TO FB-ALLOWED
               END-IF
               IF CR-DEDUCT NUMERIC
                   MOVE CR-DEDUCT TO FB-DEDUCT
               END-IF
           END-IF

           MOVE SPACE TO WS-MODX WS-MODY
           MOVE CC-MOD TO WS-MODX
           MOVE CC-MOD2 TO WS-MODY

           IF CR-MOD1 = WS-MODX AND CR-MOD2 = WS-MODY
               MOVE CR-ICN TO SAVE-ICN
               MOVE CR-MOD1 TO SAVE-MOD1
               MOVE CR-MOD2 TO SAVE-MOD2
               IF CR-PAID NUMERIC
                   MOVE CR-PAID TO SAVE-PAID
               END-IF
               IF CR-ALLOWED NUMERIC
                   MOVE CR-ALLOWED TO SAVE-ALLOWED
               END-IF
               IF CR-DEDUCT NUMERIC
                   MOVE CR-DEDUCT TO SAVE-DEDUCT
               END-IF
               MOVE 1 TO EXACT-FLAG
               GO TO LOOK-ICN-DONE
           END-IF

           GO TO LOOK-ICN-1.

       LOOK-ICN-DONE.
           IF EXACT-FLAG = 0
               MOVE FB-ICN TO SAVE-ICN
               MOVE FB-MOD1 TO SAVE-MOD1
               MOVE FB-MOD2 TO SAVE-MOD2
               MOVE FB-PAID TO SAVE-PAID
               MOVE FB-ALLOWED TO SAVE-ALLOWED
               MOVE FB-DEDUCT TO SAVE-DEDUCT
           END-IF.

       LOOK-ICN-EXIT.
           EXIT.

      *  ---- medicare icn from caredetl ----------------------------
      *
      *  every remit entry for garno + dos + cpt is read.  the
      *  entries with the charge's mods decide when there are any,
      *  otherwise all of them do, as the mods on the remit need
      *  not be the ones charcur holds.

       LOOK-DETL.
           MOVE 0 TO DTL-EOF
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 2
               MOVE 0 TO PK-ROWS(K) PK-RANK(K) PK-PAID(K)
               MOVE 0 TO PK-ALLOWED(K) PK-DEDUCT(K)
               MOVE SPACE TO PK-PAYDATE(K) PK-ICN(K) PK-TBDATE(K)
               MOVE SPACE TO PK-MOD1(K) PK-MOD2(K)
           END-PERFORM

           MOVE SPACE TO WS-MODX WS-MODY
           MOVE CC-MOD TO WS-MODX
           MOVE CC-MOD2 TO WS-MODY

           MOVE SPACE TO DT-KEY
           MOVE SAVE-GARNO TO DT-KEY8
           MOVE WS-DATE-T TO DT-DATE
           MOVE CPT-WANT TO DT-PROC

           START CAREDETL KEY NOT < DT-KEY
             INVALID
               MOVE 1 TO DTL-EOF
           END-START

           IF DTL-EOF = 1
               GO TO LOOK-DETL-DONE
           END-IF.

       LOOK-DETL-1.
           READ CAREDETL NEXT
             AT END
               MOVE 1 TO DTL-EOF
           END-READ

           IF DTL-EOF = 1
               GO TO LOOK-DETL-DONE
           END-IF

           IF DT-KEY8 NOT = SAVE-GARNO OR DT-DATE NOT = WS-DATE-T
               OR DT-PROC NOT = CPT-WANT
               GO TO LOOK-DETL-DONE
           END-IF

           IF DEBUG-FLAG = 1
               MOVE SPACE TO WS-TRACE
               STRING "DTL " DT-KEY8 " " DT-DATE " " DT-PROC
                      " mod=" DT-MOD1 DT-MOD2
                      " paid=" DT-PAYDATE
                      " icn=" DT-ICN
                      " tb=" DT-TB DELIMITED BY SIZE
                   INTO WS-TRACE
               DISPLAY WS-TRACE UPON SYSERR
           END-IF

           MOVE 2 TO K
           PERFORM TALLY-DETL
           IF DT-MOD1 = WS-MODX AND DT-MOD2 = WS-MODY
               MOVE 1 TO K
               PERFORM TALLY-DETL
           END-IF

           GO TO LOOK-DETL-1.

       LOOK-DETL-DONE.
           MOVE 1 TO K
           IF PK-ROWS(1) = 0
               MOVE 2 TO K
           END-IF

           IF PK-ROWS(K) = 0
               IF SAVE-ICN NOT = SPACE
                   MOVE "CARE" TO SAVE-SRC
               END-IF
               GO TO LOOK-DETL-EXIT
           END-IF

           ADD 1 TO C-DETL

           IF PK-RANK(K) = 0 OR PK-TBDATE(K) > PK-PAYDATE(K)
               MOVE "TB" TO SAVE-SRC
               ADD 1 TO C-TB
               IF PK-ICN(K) NOT = SPACE
                   MOVE PK-ICN(K) TO SAVE-ICN
               END-IF
               GO TO LOOK-DETL-EXIT
           END-IF

           MOVE "DETL" TO SAVE-SRC
           IF SAVE-ICN NOT = SPACE AND SAVE-ICN NOT = PK-ICN(K)
               MOVE "DETL*" TO SAVE-SRC
               ADD 1 TO C-DIFF
           END-IF
           MOVE PK-ICN(K) TO SAVE-ICN
           MOVE PK-MOD1(K) TO SAVE-MOD1
           MOVE PK-MOD2(K) TO SAVE-MOD2
           MOVE PK-PAID(K) TO SAVE-PAID
           MOVE PK-ALLOWED(K) TO SAVE-ALLOWED
           MOVE PK-DEDUCT(K) TO SAVE-DEDUCT.

       LOOK-DETL-EXIT.
           EXIT.

      *  one caredetl entry into set K.  a takeback only moves the
      *  set's last takeback date.  of the others the best is kept:
      *  allowed or paid before denied, then the newest pay date,
      *  then the higher icn.

       TALLY-DETL.
           ADD 1 TO PK-ROWS(K)
           IF DT-TB = "T"
               IF DT-PAYDATE > PK-TBDATE(K)
                   MOVE DT-PAYDATE TO PK-TBDATE(K)
               END-IF
           ELSE
               MOVE 1 TO WS-RANK
               IF (DT-ALLOWED NUMERIC AND DT-ALLOWED > 0)
                   OR (DT-PAYED NUMERIC AND DT-PAYED > 0)
                   MOVE 2 TO WS-RANK
               END-IF
               IF WS-RANK > PK-RANK(K)
                   OR (WS-RANK = PK-RANK(K)
                       AND DT-PAYDATE > PK-PAYDATE(K))
                   OR (WS-RANK = PK-RANK(K)
                       AND DT-PAYDATE = PK-PAYDATE(K)
                       AND DT-ICN > PK-ICN(K))
                   MOVE WS-RANK TO PK-RANK(K)
                   MOVE DT-PAYDATE TO PK-PAYDATE(K)
                   MOVE DT-ICN TO PK-ICN(K)
                   MOVE DT-MOD1 TO PK-MOD1(K)
                   MOVE DT-MOD2 TO PK-MOD2(K)
                   MOVE 0 TO PK-PAID(K) PK-ALLOWED(K) PK-DEDUCT(K)
                   IF DT-PAYED NUMERIC
                       MOVE DT-PAYED TO PK-PAID(K)
                   END-IF
                   IF DT-ALLOWED NUMERIC
                       MOVE DT-ALLOWED TO PK-ALLOWED(K)
                   END-IF
                   IF DT-DEDUCT NUMERIC
                       MOVE DT-DEDUCT TO PK-DEDUCT(K)
                   END-IF
               END-IF
           END-IF.

      *  ---- fixed layout row ---------------------------------------
      *
      *  cols 1-8 are handed to NORM-MRN like a worklist mrn, so
      *  00999999 lands on the same g-acct as 999999.  WS-DATE-T
      *  stays blank on a row that does not fit, which reports it
      *  as *BAD INPUT.

       FIXED-ROW.
           MOVE SPACE TO WS-DATE-T
           MOVE FILEIN01(1:8) TO F-MRN
           MOVE FILEIN01(24:8) TO F-DOS

           IF FILEIN01(1:8) NUMERIC AND FILEIN01(9:3) = "000"
               AND FILEIN01(24:8) NUMERIC
               MOVE FILEIN01(24:8) TO WS-DATE-T
               MOVE SPACE TO F-DOS
               STRING FILEIN01(28:2) "/" FILEIN01(30:2) "/"
                      FILEIN01(24:4) DELIMITED BY SIZE
                   INTO F-DOS
           END-IF.

      *  ---- lvam row -----------------------------------------------

       WRITE-LVAM.
           MOVE SPACE TO LVAM-FILE01 LV-PROC
           STRING CC-CPT " " SAVE-MOD1 " " SAVE-MOD2
               DELIMITED BY SIZE INTO LV-PROC
           MOVE SAVE-PAID TO LV-PAID
           MOVE SAVE-ALLOWED TO LV-ALLOWED
           MOVE SAVE-DEDUCT TO LV-DEDUCT
           STRING SAVE-NAME "|" SAVE-MBI "|" WS-DATE-T "|"
                  SAVE-ICN "|" LV-PROC "|" LV-PAID "|"
                  SAVE-GARNO "|" SAVE-ACCT(1:8) "|"
                  LV-ALLOWED "|" LV-DEDUCT
               DELIMITED BY SIZE INTO LVAM-FILE01
           WRITE LVAM-FILE01
           IF LVM-STAT NOT = "00"
               DISPLAY "cptfind: lvam write " LVM-STAT UPON SYSERR
               STOP RUN
           END-IF
           ADD 1 TO C-LVAM.

      *  ---- mrn to zero filled g-acct -----------------------------
      *
      *  strip leading zeros, then pad back to MRN-WIDTH.

       NORM-MRN.
           MOVE SPACE TO WS-ACCT WS-BARE WS-PAD
           MOVE 0 TO WS-LEN BARE-LEN ZFLAG WS-J

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 20
               IF F-MRN(WS-I:1) NOT = SPACE
                   MOVE WS-I TO WS-LEN
               END-IF
           END-PERFORM

           IF WS-LEN = 0
               GO TO NORM-MRN-EXIT
           END-IF

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LEN
               IF ZFLAG = 0 AND F-MRN(WS-I:1) = "0"
                   CONTINUE
               ELSE
                   MOVE 1 TO ZFLAG
                   ADD 1 TO WS-J
                   MOVE F-MRN(WS-I:1) TO WS-BARE(WS-J:1)
               END-IF
           END-PERFORM

           IF WS-J = 0
               MOVE "0" TO WS-BARE(1:1)
               MOVE 1 TO WS-J
           END-IF

           MOVE WS-J TO BARE-LEN

           IF BARE-LEN NOT < MRN-WIDTH
               MOVE WS-BARE TO WS-ACCT
           ELSE
               MOVE ALL "0" TO WS-PAD(1:MRN-WIDTH)
               COMPUTE PAD-OFF = MRN-WIDTH - BARE-LEN + 1
               MOVE WS-BARE(1:BARE-LEN) TO WS-PAD(PAD-OFF:BARE-LEN)
               MOVE WS-PAD TO WS-ACCT
           END-IF.

       NORM-MRN-EXIT.
           EXIT.

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
