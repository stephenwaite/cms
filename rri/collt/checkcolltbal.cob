      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
      *
      * checkcolltbal -- reconcile a kin018 collections export that was
      * never sent.  rebuilds each guarantor's placement figures from
      * CHARCUR/PAYCUR using kin018's own arithmetic, then splits off
      * anything posted since, so paid accounts can be pulled.
      *
      * all report output goes to S45.  the ACUCOBOL terminal manager
      * clears the screen, so DISPLAY is used only for fatal startup
      * errors -- read the report file, not the console.
      *
      * env:
      *   PLACEDATE  CCYYMMDD run date of the kin018 job   (required)
      *   SELMODE    DATEA | COLLT | ANY018                (def DATEA)
      *   CENSUS     Y to tally CC-DATE-A across all 018   (def N)
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. checkcolltbal.
       AUTHOR. SID WAITE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILEIN ASSIGN TO "S30"
             ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS WS-FI-STAT.

           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL
             FILE STATUS IS WS-CC-STAT.

           SELECT PAYCUR ASSIGN TO "S40" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
             LOCK MODE MANUAL
             FILE STATUS IS WS-PC-STAT.

           SELECT FILEOUT ASSIGN TO "S45"
             ORGANIZATION IS LINE SEQUENTIAL
             FILE STATUS IS WS-FO-STAT.

       DATA DIVISION.

       FILE SECTION.

       FD  CHARCUR.
           COPY CHARCUR.CPY.

       FD  PAYCUR.
           COPY PAYCUR.CPY.

      *----------------------------------------------------------------
      *    the kin018 export.  offsets counted off FILEOUT01 in kin018,
      *    commas included:  1-8 FO-GARNO, 173-182 FO-DISCHR,
      *    477-483 FO-CHG, 485-491 FO-PAY, 493-499 FO-ADJ,
      *    503-509 FO-BAL.  record ends at 509.
      *----------------------------------------------------------------
       FD  FILEIN.
       01  FILEIN-REC.
           05  FI-GARNO            PIC X(8).
           05  FILLER              PIC X(164).
           05  FI-DISCHR           PIC X(10).
           05  FILLER              PIC X(294).
           05  FI-CHG              PIC X(7).
           05  FILLER              PIC X.
           05  FI-PAY              PIC X(7).
           05  FILLER              PIC X.
           05  FI-ADJ              PIC X(7).
           05  FILLER              PIC X(3).
           05  FI-BAL              PIC X(7).
           05  FILLER              PIC X(11).

       FD  FILEOUT.
       01  FILEOUT01               PIC X(160).

       WORKING-STORAGE SECTION.

       01  WS-FI-STAT              PIC XX VALUE "00".
       01  WS-FO-STAT              PIC XX VALUE "00".
       01  WS-CC-STAT              PIC XX VALUE "00".
       01  WS-PC-STAT              PIC XX VALUE "00".

       01  WS-PLACED               PIC X(8) VALUE SPACES.
       01  WS-PLACED-EDIT          PIC X(10).

       01  WS-SELMODE              PIC X(8) VALUE "DATEA".
           88  SEL-DATEA                 VALUE "DATEA".
           88  SEL-COLLT                 VALUE "COLLT".
           88  SEL-ANY                   VALUE "ANY018".

       01  WS-CENSUS               PIC X VALUE "N".

      *----------------------------------------------------------------
      *    the export is not reliably sorted by guarantor and the same
      *    guarantor can appear on several lines -- kin018's group break
      *    fires whenever its S50 extract goes out of key order.  load
      *    the whole file and sum by guarantor before comparing.
      *----------------------------------------------------------------
       01  EXP-TAB.
           05  EXP-CNT             PIC 9(5) VALUE 0.
           05  EXP-ENT             OCCURS 5000 TIMES.
               10  E-GARNO         PIC X(8).
               10  E-CHG           PIC S9(7)V99.
               10  E-PAY           PIC S9(7)V99.
               10  E-ADJ           PIC S9(7)V99.
               10  E-BAL           PIC S9(7)V99.
               10  E-DISCHR        PIC X(8).
               10  E-FILELINES     PIC 9(3).
               10  E-BADAMT        PIC X.
       01  EXP-MAX                 PIC 9(5) VALUE 5000.
       01  EXP-OVF                 PIC 9(6) VALUE 0.
       01  WS-IX                   PIC 9(5) VALUE 0.
       01  WS-JX                   PIC 9(5) VALUE 0.
       01  WS-FOUND                PIC X VALUE "N".
       01  WS-SPLIT-CNT            PIC 9(6) VALUE 0.

       01  WS-GARNO                PIC X(8).

       01  EXP-CHG                 PIC S9(7)V99 VALUE 0.
       01  EXP-PAY                 PIC S9(7)V99 VALUE 0.
       01  EXP-ADJ                 PIC S9(7)V99 VALUE 0.
       01  EXP-BAL                 PIC S9(7)V99 VALUE 0.
       01  WS-BAL-OK               PIC X VALUE "N".
           88  BAL-VALID                 VALUE "Y".

       01  THEN-CHG                PIC S9(7)V99 VALUE 0.
       01  THEN-PAY                PIC S9(7)V99 VALUE 0.
       01  THEN-ADJ                PIC S9(7)V99 VALUE 0.
       01  THEN-BAL                PIC S9(7)V99 VALUE 0.
       01  NOW-BAL                 PIC S9(7)V99 VALUE 0.
       01  PAID-SINCE              PIC S9(7)V99 VALUE 0.
       01  WS-DRIFT                PIC S9(7)V99 VALUE 0.

       01  L-TOT-NOW               PIC S9(6)V99 VALUE 0.
       01  L-TOT-THEN              PIC S9(6)V99 VALUE 0.
       01  L-PAY                   PIC S9(6)V99 VALUE 0.
       01  L-ADJ                   PIC S9(6)V99 VALUE 0.
       01  L-SINCE                 PIC S9(6)V99 VALUE 0.

       01  WS-LINES                PIC 9(4) VALUE 0.
       01  WS-MAXDOS               PIC X(8) VALUE SPACES.
       01  WS-DISCHR-CMP           PIC X(8) VALUE SPACES.

      *    every distinct CC-DATE-A this guarantor carries on its 018
      *    lines, so a no-match can say what it does have instead
       01  GDT-TAB.
           05  GDT-CNT             PIC 99 VALUE 0.
           05  GDT-ENT             OCCURS 12 TIMES.
               10  GDT-DATE        PIC X(8).
               10  GDT-RECS        PIC 9(4).
       01  GDT-IX                  PIC 99 VALUE 0.
       01  GDT-018                 PIC 9(4) VALUE 0.
       01  GDT-COLLT               PIC 9(4) VALUE 0.
       01  GDT-LINE                PIC X(120).
       01  GDT-POS                 PIC 9(3) VALUE 0.
       01  GDT-FRAG                PIC X(20).

      *    NUMVAL guard
       01  WS-BAD                  PIC X VALUE "N".
       01  WS-BI                   PIC 9(4) VALUE 0.
       01  WS-DIG                  PIC 9(4) VALUE 0.
       01  WS-CH                   PIC X.
       01  WS-PARSE                PIC X(7).
       01  WS-VALUE                PIC S9(7)V99 VALUE 0.

       01  WS-ACTION               PIC X(24).

      *----------------------------------------------------------------
      *    census keeps the 60 most recent CC-DATE-A values, and always
      *    counts the target date separately so it cannot be crowded out
      *----------------------------------------------------------------
       01  CEN-TAB.
           05  CEN-CNT             PIC 99 VALUE 0.
           05  CEN-ENT             OCCURS 60 TIMES.
               10  CEN-DATE        PIC X(8).
               10  CEN-RECS        PIC 9(7).
               10  CEN-AMT         PIC S9(9)V99.
       01  CEN-IX                  PIC 99 VALUE 0.
       01  CEN-LOW                 PIC 99 VALUE 0.
       01  CEN-HIT                 PIC X VALUE "N".
       01  CEN-TOTAL               PIC 9(7) VALUE 0.
       01  CEN-DROP                PIC 9(7) VALUE 0.
       01  CEN-TGT-RECS            PIC 9(7) VALUE 0.
       01  CEN-TGT-AMT             PIC S9(9)V99 VALUE 0.
       01  CEN-COLLT-RECS          PIC 9(7) VALUE 0.
       01  CEN-SWAP.
           05  SW-DATE             PIC X(8).
           05  SW-RECS             PIC 9(7).
           05  SW-AMT              PIC S9(9)V99.

       01  WS-GRAND.
           05  GT-EXP-BAL          PIC S9(9)V99 VALUE 0.
           05  GT-THEN-BAL         PIC S9(9)V99 VALUE 0.
           05  GT-NOW-BAL          PIC S9(9)V99 VALUE 0.
           05  GT-SINCE            PIC S9(9)V99 VALUE 0.
           05  GT-SENDABLE         PIC S9(9)V99 VALUE 0.

       01  WS-COUNTS.
           05  WS-FILELINE-CNT     PIC 9(6) VALUE 0.
           05  WS-GAR-CNT          PIC 9(6) VALUE 0.
           05  WS-BADBAL-CNT       PIC 9(6) VALUE 0.
           05  WS-NOTFND-CNT       PIC 9(6) VALUE 0.
           05  WS-RECON-CNT        PIC 9(6) VALUE 0.
           05  WS-UNRECON-CNT      PIC 9(6) VALUE 0.
           05  WS-SEND-CNT         PIC 9(6) VALUE 0.
           05  WS-REDUCE-CNT       PIC 9(6) VALUE 0.
           05  WS-PULL-CNT         PIC 9(6) VALUE 0.
           05  WS-DOSDIFF-CNT      PIC 9(6) VALUE 0.

       01  OUT-LINE                PIC X(160).

       01  HEAD-LINE.
           05  FILLER  PIC X(8)  VALUE "GARNO".
           05  FILLER  PIC XX    VALUE SPACE.
           05  FILLER  PIC X(4)  VALUE " LNS".
           05  FILLER  PIC XX    VALUE SPACE.
           05  FILLER  PIC X(4)  VALUE " FLN".
           05  FILLER  PIC XX    VALUE SPACE.
           05  FILLER  PIC X(13) VALUE "     FILE BAL".
           05  FILLER  PIC XX    VALUE SPACE.
           05  FILLER  PIC X(13) VALUE "    REBUILT@P".
           05  FILLER  PIC XX    VALUE SPACE.
           05  FILLER  PIC X(13) VALUE "    PAID SNCE".
           05  FILLER  PIC XX    VALUE SPACE.
           05  FILLER  PIC X(13) VALUE "      BAL NOW".
           05  FILLER  PIC XX    VALUE SPACE.
           05  FILLER  PIC X(13) VALUE "        DRIFT".
           05  FILLER  PIC XX    VALUE SPACE.
           05  FILLER  PIC X(24) VALUE "ACTION".

       01  DETAIL-LINE.
           05  DL-GARNO    PIC X(8).
           05  FILLER      PIC XX VALUE SPACE.
           05  DL-LINES    PIC ZZZ9.
           05  FILLER      PIC XX VALUE SPACE.
           05  DL-FLINES   PIC ZZZ9.
           05  FILLER      PIC XX VALUE SPACE.
           05  DL-EXP      PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER      PIC XX VALUE SPACE.
           05  DL-THEN     PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER      PIC XX VALUE SPACE.
           05  DL-SINCE    PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER      PIC XX VALUE SPACE.
           05  DL-NOW      PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER      PIC XX VALUE SPACE.
           05  DL-DRIFT    PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER      PIC XX VALUE SPACE.
           05  DL-ACTION   PIC X(24).

       01  TOTAL-LINE.
           05  FILLER      PIC X(22) VALUE "TOTALS".
           05  TL-EXP      PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER      PIC XX VALUE SPACE.
           05  TL-THEN     PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER      PIC XX VALUE SPACE.
           05  TL-SINCE    PIC -ZZ,ZZZ,ZZ9.99.
           05  FILLER      PIC XX VALUE SPACE.
           05  TL-NOW      PIC -ZZ,ZZZ,ZZ9.99.

       01  CEN-LINE.
           05  FILLER      PIC X(4) VALUE SPACE.
           05  CL-DATE     PIC X(10).
           05  FILLER      PIC XX   VALUE SPACE.
           05  CL-RECS     PIC ZZZ,ZZ9.
           05  FILLER      PIC XX   VALUE SPACE.
           05  CL-AMT      PIC -ZZ,ZZZ,ZZ9.99.

       01  MSG-LINE.
           05  ML-TEXT     PIC X(30).
           05  ML-VAL      PIC X(60).

       PROCEDURE DIVISION.

      *================================================================
       P0.
           DISPLAY "PLACEDATE" UPON ENVIRONMENT-NAME.
           ACCEPT WS-PLACED FROM ENVIRONMENT-VALUE.
           IF WS-PLACED = SPACES OR WS-PLACED NOT NUMERIC
               DISPLAY "PLACEDATE must be the kin018 run date, CCYYMMDD"
               STOP RUN
           END-IF.

           DISPLAY "SELMODE" UPON ENVIRONMENT-NAME.
           ACCEPT WS-SELMODE FROM ENVIRONMENT-VALUE.
           IF WS-SELMODE = SPACES
               MOVE "DATEA" TO WS-SELMODE
           END-IF.
           IF NOT SEL-DATEA AND NOT SEL-COLLT AND NOT SEL-ANY
               DISPLAY "SELMODE must be DATEA, COLLT or ANY018"
               STOP RUN
           END-IF.

           DISPLAY "CENSUS" UPON ENVIRONMENT-NAME.
           ACCEPT WS-CENSUS FROM ENVIRONMENT-VALUE.

           MOVE SPACES TO WS-PLACED-EDIT.
           STRING WS-PLACED(5:2) "/" WS-PLACED(7:2) "/"
                  WS-PLACED(1:4) DELIMITED BY SIZE
             INTO WS-PLACED-EDIT
           END-STRING.

           OPEN INPUT CHARCUR PAYCUR FILEIN.
           OPEN OUTPUT FILEOUT.

           IF WS-CC-STAT NOT = "00"
               DISPLAY "OPEN CHARCUR FAILED, STATUS " WS-CC-STAT
               STOP RUN
           END-IF.
           IF WS-PC-STAT NOT = "00"
               DISPLAY "OPEN PAYCUR FAILED, STATUS " WS-PC-STAT
               STOP RUN
           END-IF.
           IF WS-FI-STAT NOT = "00"
               DISPLAY "OPEN FILEIN FAILED, STATUS " WS-FI-STAT
               STOP RUN
           END-IF.

           MOVE SPACES TO MSG-LINE.
           MOVE "PLACEMENT DATE" TO ML-TEXT.
           MOVE WS-PLACED-EDIT TO ML-VAL.
           MOVE MSG-LINE TO OUT-LINE
           PERFORM P-WRITE.
           MOVE SPACES TO MSG-LINE.
           MOVE "SELECTOR" TO ML-TEXT.
           MOVE WS-SELMODE TO ML-VAL.
           MOVE MSG-LINE TO OUT-LINE
           PERFORM P-WRITE.
           MOVE SPACES TO OUT-LINE
           PERFORM P-WRITE.

           IF WS-CENSUS = "Y"
               PERFORM P-CENSUS THRU P-CENSUS-X
           END-IF.

           PERFORM P-LOAD THRU P-LOAD-X.

           IF EXP-CNT = 0
               MOVE "NO USABLE LINES IN EXPORT FILE" TO OUT-LINE
               PERFORM P-WRITE
               GO TO R99
           END-IF.

           MOVE HEAD-LINE TO OUT-LINE
           PERFORM P-WRITE.
           MOVE 0 TO WS-IX.

      *================================================================
       R2.
           ADD 1 TO WS-IX.
           IF WS-IX > EXP-CNT
               GO TO R90
           END-IF.

           ADD 1 TO WS-GAR-CNT.
           MOVE E-GARNO(WS-IX)  TO WS-GARNO.
           MOVE E-CHG(WS-IX)    TO EXP-CHG.
           MOVE E-PAY(WS-IX)    TO EXP-PAY.
           MOVE E-ADJ(WS-IX)    TO EXP-ADJ.
           MOVE E-BAL(WS-IX)    TO EXP-BAL.
           IF E-BADAMT(WS-IX) = "Y"
               MOVE "N" TO WS-BAL-OK
               ADD 1 TO WS-BADBAL-CNT
           ELSE
               MOVE "Y" TO WS-BAL-OK
           END-IF.

           MOVE 0 TO THEN-CHG THEN-PAY THEN-ADJ THEN-BAL.
           MOVE 0 TO NOW-BAL PAID-SINCE WS-LINES.
           MOVE 0 TO GDT-CNT GDT-018 GDT-COLLT.
           MOVE SPACES TO WS-MAXDOS.

           PERFORM P-CHAR THRU P-CHAR-X.
           PERFORM P-CLASS THRU P-CLASS-X.
           GO TO R2.

      *================================================================
      *    load and aggregate the export file
      *================================================================
       P-LOAD.
           MOVE 0 TO EXP-CNT WS-FILELINE-CNT EXP-OVF WS-SPLIT-CNT.

       P-LOAD-1.
           READ FILEIN
             AT END
               GO TO P-LOAD-RPT
           END-READ.

           IF FI-GARNO = SPACES
               GO TO P-LOAD-1
           END-IF.

           ADD 1 TO WS-FILELINE-CNT.

           MOVE "Y" TO WS-BAL-OK.
           MOVE FI-CHG TO WS-PARSE
           PERFORM P-PARSE THRU P-PARSE-X.
             MOVE WS-VALUE TO EXP-CHG.
           MOVE FI-PAY TO WS-PARSE
           PERFORM P-PARSE THRU P-PARSE-X.
             MOVE WS-VALUE TO EXP-PAY.
           MOVE FI-ADJ TO WS-PARSE
           PERFORM P-PARSE THRU P-PARSE-X.
             MOVE WS-VALUE TO EXP-ADJ.
           MOVE FI-BAL TO WS-PARSE
           PERFORM P-PARSE THRU P-PARSE-X.
             MOVE WS-VALUE TO EXP-BAL.

           MOVE SPACES TO WS-DISCHR-CMP.
           IF FI-DISCHR NOT = SPACES
               STRING FI-DISCHR(7:4) FI-DISCHR(1:2) FI-DISCHR(4:2)
                 DELIMITED BY SIZE INTO WS-DISCHR-CMP
               END-STRING
           END-IF.

           MOVE "N" TO WS-FOUND.
           PERFORM VARYING WS-JX FROM 1 BY 1
             UNTIL WS-JX > EXP-CNT OR WS-FOUND = "Y"
               IF E-GARNO(WS-JX) = FI-GARNO
                   MOVE "Y" TO WS-FOUND
                   ADD EXP-CHG TO E-CHG(WS-JX)
                   ADD EXP-PAY TO E-PAY(WS-JX)
                   ADD EXP-ADJ TO E-ADJ(WS-JX)
                   ADD EXP-BAL TO E-BAL(WS-JX)
                   ADD 1 TO E-FILELINES(WS-JX)
                   ADD 1 TO WS-SPLIT-CNT
                   IF WS-DISCHR-CMP > E-DISCHR(WS-JX)
                       MOVE WS-DISCHR-CMP TO E-DISCHR(WS-JX)
                   END-IF
                   IF NOT BAL-VALID
                       MOVE "Y" TO E-BADAMT(WS-JX)
                   END-IF
               END-IF
           END-PERFORM.

           IF WS-FOUND = "N"
               IF EXP-CNT < EXP-MAX
                   ADD 1 TO EXP-CNT
                   MOVE FI-GARNO       TO E-GARNO(EXP-CNT)
                   MOVE EXP-CHG        TO E-CHG(EXP-CNT)
                   MOVE EXP-PAY        TO E-PAY(EXP-CNT)
                   MOVE EXP-ADJ        TO E-ADJ(EXP-CNT)
                   MOVE EXP-BAL        TO E-BAL(EXP-CNT)
                   MOVE WS-DISCHR-CMP  TO E-DISCHR(EXP-CNT)
                   MOVE 1              TO E-FILELINES(EXP-CNT)
                   IF BAL-VALID
                       MOVE "N" TO E-BADAMT(EXP-CNT)
                   ELSE
                       MOVE "Y" TO E-BADAMT(EXP-CNT)
                   END-IF
               ELSE
                   ADD 1 TO EXP-OVF
               END-IF
           END-IF.

           GO TO P-LOAD-1.

       P-LOAD-RPT.
           MOVE SPACES TO MSG-LINE.
           MOVE "EXPORT LINES READ" TO ML-TEXT.
           MOVE WS-FILELINE-CNT TO ML-VAL.
           MOVE MSG-LINE TO OUT-LINE
           PERFORM P-WRITE.
           MOVE SPACES TO MSG-LINE.
           MOVE "DISTINCT GUARANTORS" TO ML-TEXT.
           MOVE EXP-CNT TO ML-VAL.
           MOVE MSG-LINE TO OUT-LINE
           PERFORM P-WRITE.
           MOVE SPACES TO MSG-LINE.
           MOVE "EXTRA LINES MERGED" TO ML-TEXT.
           MOVE WS-SPLIT-CNT TO ML-VAL.
           MOVE MSG-LINE TO OUT-LINE
           PERFORM P-WRITE.
           IF EXP-OVF > 0
               MOVE SPACES TO MSG-LINE
               MOVE "*** TABLE FULL, DROPPED" TO ML-TEXT
               MOVE EXP-OVF TO ML-VAL
               MOVE MSG-LINE TO OUT-LINE
               PERFORM P-WRITE
           END-IF.
           MOVE SPACES TO OUT-LINE
           PERFORM P-WRITE.

       P-LOAD-X.
           EXIT.

      *================================================================
      *    walk this guarantor's charges
      *================================================================
       P-CHAR.
           MOVE WS-GARNO TO CC-KEY8.
           MOVE LOW-VALUES TO CC-KEY3.
           START CHARCUR KEY NOT < CHARCUR-KEY
             INVALID KEY
               GO TO P-CHAR-X
           END-START.

       P-CHAR-1.
           READ CHARCUR NEXT
             AT END
               GO TO P-CHAR-X
           END-READ.

           IF CC-KEY8 NOT = WS-GARNO
               GO TO P-CHAR-X
           END-IF.

      *    alphanumeric compare on purpose -- CC-PAYCODE is PIC 999 and
      *    a numeric compare trips on any legacy record holding spaces
           IF CC-PAYCODE NOT = "018"
               GO TO P-CHAR-1
           END-IF.

           ADD 1 TO GDT-018.
           IF CC-COLLT = "1"
               ADD 1 TO GDT-COLLT
           END-IF.
           PERFORM P-GDT.

           EVALUATE TRUE
             WHEN SEL-DATEA
               IF CC-DATE-A NOT = WS-PLACED
                   GO TO P-CHAR-1
               END-IF
             WHEN SEL-COLLT
               IF CC-COLLT NOT = "1"
                   GO TO P-CHAR-1
               END-IF
             WHEN SEL-ANY
               CONTINUE
           END-EVALUATE.

           PERFORM P-SCAN-PAY THRU P-SCAN-X.

      *    kin018 skipped any line whose balance was not positive at the
      *    time, so judge inclusion on the rebuilt figure, not today's
           IF L-TOT-THEN NOT > 0
               GO TO P-CHAR-1
           END-IF.

           ADD 1 TO WS-LINES.
           ADD CC-AMOUNT  TO THEN-CHG.
           ADD L-PAY      TO THEN-PAY.
           ADD L-ADJ      TO THEN-ADJ.
           ADD L-TOT-THEN TO THEN-BAL.
           ADD L-TOT-NOW  TO NOW-BAL.
           ADD L-SINCE    TO PAID-SINCE.

           IF CC-DATE-T > WS-MAXDOS
               MOVE CC-DATE-T TO WS-MAXDOS
           END-IF.

           GO TO P-CHAR-1.

       P-CHAR-X.
           EXIT.

      *----------------------------------------------------------------
      *    tally distinct CC-DATE-A for this guarantor's 018 lines
       P-GDT.
           MOVE "N" TO WS-FOUND.
           PERFORM VARYING GDT-IX FROM 1 BY 1
             UNTIL GDT-IX > GDT-CNT OR WS-FOUND = "Y"
               IF GDT-DATE(GDT-IX) = CC-DATE-A
                   MOVE "Y" TO WS-FOUND
                   ADD 1 TO GDT-RECS(GDT-IX)
               END-IF
           END-PERFORM.
           IF WS-FOUND = "N" AND GDT-CNT < 12
               ADD 1 TO GDT-CNT
               MOVE CC-DATE-A TO GDT-DATE(GDT-CNT)
               MOVE 1 TO GDT-RECS(GDT-CNT)
           END-IF.

      *================================================================
      *    classify and report one guarantor
      *================================================================
       P-CLASS.
           COMPUTE WS-DRIFT = THEN-BAL - EXP-BAL.

           ADD EXP-BAL    TO GT-EXP-BAL.
           ADD THEN-BAL   TO GT-THEN-BAL.
           ADD NOW-BAL    TO GT-NOW-BAL.
           ADD PAID-SINCE TO GT-SINCE.

           EVALUATE TRUE
             WHEN WS-LINES = 0
               MOVE "CHECK - NO 018 MATCH"   TO WS-ACTION
               ADD 1 TO WS-NOTFND-CNT
             WHEN NOW-BAL < 0
               MOVE "PULL - CREDIT BALANCE"  TO WS-ACTION
               ADD 1 TO WS-PULL-CNT
             WHEN NOW-BAL = 0
               MOVE "PULL - PAID IN FULL"    TO WS-ACTION
               ADD 1 TO WS-PULL-CNT
             WHEN PAID-SINCE NOT = 0
               MOVE "SEND - REDUCED"         TO WS-ACTION
               ADD 1 TO WS-REDUCE-CNT
               ADD NOW-BAL TO GT-SENDABLE
             WHEN OTHER
               MOVE "SEND"                   TO WS-ACTION
               ADD 1 TO WS-SEND-CNT
               ADD NOW-BAL TO GT-SENDABLE
           END-EVALUATE.

           IF WS-LINES > 0 AND BAL-VALID
               IF WS-DRIFT = 0
                   ADD 1 TO WS-RECON-CNT
               ELSE
                   ADD 1 TO WS-UNRECON-CNT
                   MOVE "CHECK - NO REBUILD"  TO WS-ACTION
               END-IF
           END-IF.

           MOVE SPACES TO DETAIL-LINE.
           MOVE WS-GARNO           TO DL-GARNO.
           MOVE WS-LINES           TO DL-LINES.
           MOVE E-FILELINES(WS-IX) TO DL-FLINES.
           MOVE EXP-BAL            TO DL-EXP.
           MOVE THEN-BAL           TO DL-THEN.
           MOVE PAID-SINCE         TO DL-SINCE.
           MOVE NOW-BAL            TO DL-NOW.
           MOVE WS-DRIFT           TO DL-DRIFT.
           MOVE WS-ACTION          TO DL-ACTION.
           MOVE DETAIL-LINE TO OUT-LINE
           PERFORM P-WRITE.

      *    a no-match is far more useful with the dates it does carry
           IF WS-LINES = 0
               MOVE SPACES TO GDT-LINE
               MOVE 1 TO GDT-POS
               STRING "    018 lines " GDT-018
                      ", collt-flagged " GDT-COLLT
                      ", CC-DATE-A:"
                 DELIMITED BY SIZE INTO GDT-LINE
                 WITH POINTER GDT-POS
               END-STRING
               PERFORM VARYING GDT-IX FROM 1 BY 1
                 UNTIL GDT-IX > GDT-CNT
                   MOVE SPACES TO GDT-FRAG
                   IF GDT-DATE(GDT-IX) NUMERIC
                       STRING " " GDT-DATE(GDT-IX)(5:2) "/"
                              GDT-DATE(GDT-IX)(7:2) "/"
                              GDT-DATE(GDT-IX)(1:4)
                         DELIMITED BY SIZE INTO GDT-FRAG
                       END-STRING
                   ELSE
                       MOVE " [nondate]" TO GDT-FRAG
                   END-IF
                   IF GDT-POS < 100
                       STRING GDT-FRAG(1:11) DELIMITED BY SIZE
                         INTO GDT-LINE WITH POINTER GDT-POS
                       END-STRING
                   END-IF
               END-PERFORM
               MOVE GDT-LINE TO OUT-LINE
               PERFORM P-WRITE
           END-IF.

           IF WS-LINES > 0
               IF E-DISCHR(WS-IX) NOT = WS-MAXDOS
                   ADD 1 TO WS-DOSDIFF-CNT
                   MOVE SPACES TO GDT-LINE
                   STRING "    DISCHR file [" E-DISCHR(WS-IX)
                          "] charcur [" WS-MAXDOS "]"
                     DELIMITED BY SIZE INTO GDT-LINE
                   END-STRING
                   MOVE GDT-LINE TO OUT-LINE
                   PERFORM P-WRITE
               END-IF
           END-IF.

       P-CLASS-X.
           EXIT.

      *================================================================
      *    kin018's P3 -- payments matched on claim number.  PC-AMOUNT
      *    is negative, hence the adds.  PC-DENIAL "14" is an
      *    adjustment.  anything posted after the placement date is
      *    drift and is split out separately.
      *================================================================
       P-SCAN-PAY.
           MOVE CC-AMOUNT TO L-TOT-NOW.
           MOVE 0 TO L-PAY L-ADJ L-SINCE.

           MOVE CC-KEY8 TO PC-KEY8.
           MOVE LOW-VALUES TO PC-KEY3.
           START PAYCUR KEY NOT < PAYCUR-KEY
             INVALID KEY
               GO TO P-SCAN-X
           END-START.

       P-SCAN-1.
           READ PAYCUR NEXT
             AT END
               GO TO P-SCAN-X
           END-READ.

           IF PC-KEY8 NOT = CC-KEY8
               GO TO P-SCAN-X
           END-IF.

           IF PC-CLAIM NOT = CC-CLAIM
               GO TO P-SCAN-1
           END-IF.

           COMPUTE L-TOT-NOW = L-TOT-NOW + PC-AMOUNT.

           IF PC-DATE-T > WS-PLACED
               COMPUTE L-SINCE = L-SINCE - PC-AMOUNT
           ELSE
               IF PC-DENIAL = "14"
                   COMPUTE L-ADJ = L-ADJ - PC-AMOUNT
               ELSE
                   COMPUTE L-PAY = L-PAY - PC-AMOUNT
               END-IF
           END-IF.

           GO TO P-SCAN-1.

       P-SCAN-X.
           COMPUTE L-TOT-THEN = L-TOT-NOW + L-SINCE.

      *================================================================
      *    census over the CC-PAYCODE alternate key.  keeps the 60 most
      *    recent dates -- when the table is full, the oldest entry is
      *    evicted rather than the new date being dropped.
      *================================================================
       P-CENSUS.
           MOVE "--- CC-DATE-A census, all 018 charges ---" TO OUT-LINE.
           PERFORM P-WRITE.
           MOVE 0 TO CEN-CNT CEN-TOTAL CEN-DROP.
           MOVE 0 TO CEN-TGT-RECS CEN-TGT-AMT CEN-COLLT-RECS.
           MOVE 018 TO CC-PAYCODE.
           START CHARCUR KEY NOT < CC-PAYCODE
             INVALID KEY
               MOVE "  no 018 charges at all" TO OUT-LINE
               PERFORM P-WRITE
               GO TO P-CENSUS-X
           END-START.

       P-CENSUS-1.
           READ CHARCUR NEXT
             AT END
               GO TO P-CENSUS-RPT
           END-READ.
           IF CC-PAYCODE NOT = "018"
               GO TO P-CENSUS-RPT
           END-IF.

           ADD 1 TO CEN-TOTAL.
           IF CC-COLLT = "1"
               ADD 1 TO CEN-COLLT-RECS
           END-IF.
           IF CC-DATE-A = WS-PLACED
               ADD 1 TO CEN-TGT-RECS
               ADD CC-AMOUNT TO CEN-TGT-AMT
           END-IF.

           MOVE "N" TO CEN-HIT.
           PERFORM VARYING CEN-IX FROM 1 BY 1
             UNTIL CEN-IX > CEN-CNT OR CEN-HIT = "Y"
               IF CEN-DATE(CEN-IX) = CC-DATE-A
                   MOVE "Y" TO CEN-HIT
                   ADD 1 TO CEN-RECS(CEN-IX)
                   ADD CC-AMOUNT TO CEN-AMT(CEN-IX)
               END-IF
           END-PERFORM.

           IF CEN-HIT = "Y"
               GO TO P-CENSUS-1
           END-IF.

           IF CEN-CNT < 60
               ADD 1 TO CEN-CNT
               MOVE CC-DATE-A TO CEN-DATE(CEN-CNT)
               MOVE 1 TO CEN-RECS(CEN-CNT)
               MOVE CC-AMOUNT TO CEN-AMT(CEN-CNT)
               GO TO P-CENSUS-1
           END-IF.

      *    table full -- evict the oldest date if this one is newer
           MOVE 1 TO CEN-LOW.
           PERFORM VARYING CEN-IX FROM 2 BY 1 UNTIL CEN-IX > 60
               IF CEN-DATE(CEN-IX) < CEN-DATE(CEN-LOW)
                   MOVE CEN-IX TO CEN-LOW
               END-IF
           END-PERFORM.
           IF CC-DATE-A > CEN-DATE(CEN-LOW)
               ADD CEN-RECS(CEN-LOW) TO CEN-DROP
               MOVE CC-DATE-A TO CEN-DATE(CEN-LOW)
               MOVE 1 TO CEN-RECS(CEN-LOW)
               MOVE CC-AMOUNT TO CEN-AMT(CEN-LOW)
           ELSE
               ADD 1 TO CEN-DROP
           END-IF.
           GO TO P-CENSUS-1.

       P-CENSUS-RPT.
           IF CEN-TOTAL = 0
               MOVE "  no 018 charges at all" TO OUT-LINE
               PERFORM P-WRITE
               GO TO P-CENSUS-X
           END-IF.

      *    sort descending by date so recent batches read first
           PERFORM VARYING CEN-IX FROM 1 BY 1 UNTIL CEN-IX > CEN-CNT
               PERFORM VARYING GDT-IX FROM 1 BY 1
                 UNTIL GDT-IX >= CEN-CNT
                   IF CEN-DATE(GDT-IX) < CEN-DATE(GDT-IX + 1)
                       MOVE CEN-ENT(GDT-IX)     TO CEN-SWAP
                       MOVE CEN-ENT(GDT-IX + 1) TO CEN-ENT(GDT-IX)
                       MOVE CEN-SWAP        TO CEN-ENT(GDT-IX + 1)
                   END-IF
               END-PERFORM
           END-PERFORM.

           PERFORM VARYING CEN-IX FROM 1 BY 1 UNTIL CEN-IX > CEN-CNT
               MOVE SPACES TO CEN-LINE
               IF CEN-DATE(CEN-IX) NUMERIC
                   STRING CEN-DATE(CEN-IX)(5:2) "/"
                          CEN-DATE(CEN-IX)(7:2) "/"
                          CEN-DATE(CEN-IX)(1:4)
                     DELIMITED BY SIZE INTO CL-DATE
                   END-STRING
               ELSE
                   MOVE "[nondate]" TO CL-DATE
               END-IF
               MOVE CEN-RECS(CEN-IX) TO CL-RECS
               MOVE CEN-AMT(CEN-IX)  TO CL-AMT
               MOVE CEN-LINE TO OUT-LINE
               PERFORM P-WRITE
           END-PERFORM.

           MOVE SPACES TO MSG-LINE.
           MOVE "  total 018 records" TO ML-TEXT.
           MOVE CEN-TOTAL TO ML-VAL.
           MOVE MSG-LINE TO OUT-LINE
           PERFORM P-WRITE.
           MOVE SPACES TO MSG-LINE.
           MOVE "  collt-flagged" TO ML-TEXT.
           MOVE CEN-COLLT-RECS TO ML-VAL.
           MOVE MSG-LINE TO OUT-LINE
           PERFORM P-WRITE.
           MOVE SPACES TO MSG-LINE.
           MOVE "  ON TARGET DATE" TO ML-TEXT.
           MOVE CEN-TGT-RECS TO ML-VAL.
           MOVE MSG-LINE TO OUT-LINE
           PERFORM P-WRITE.
           MOVE SPACES TO CEN-LINE.
           MOVE "  target charge total" TO CL-DATE.
           MOVE CEN-TGT-AMT TO CL-AMT.
           MOVE CEN-LINE TO OUT-LINE
           PERFORM P-WRITE.
           IF CEN-DROP > 0
               MOVE SPACES TO MSG-LINE
               MOVE "  older dates not shown" TO ML-TEXT
               MOVE CEN-DROP TO ML-VAL
               MOVE MSG-LINE TO OUT-LINE
               PERFORM P-WRITE
           END-IF.
           MOVE "---" TO OUT-LINE
           PERFORM P-WRITE.
           MOVE SPACES TO OUT-LINE
           PERFORM P-WRITE.

       P-CENSUS-X.
           EXIT.

      *================================================================
       P-PARSE.
           MOVE 0 TO WS-VALUE WS-DIG.
           MOVE "N" TO WS-BAD.
           IF WS-PARSE = SPACES
               MOVE "N" TO WS-BAL-OK
               GO TO P-PARSE-X
           END-IF.
           PERFORM VARYING WS-BI FROM 1 BY 1 UNTIL WS-BI > 7
               MOVE WS-PARSE(WS-BI:1) TO WS-CH
               EVALUATE WS-CH
                 WHEN SPACE
                 WHEN "."
                 WHEN ","
                 WHEN "-"
                   CONTINUE
                 WHEN "0" THRU "9"
                   ADD 1 TO WS-DIG
                 WHEN OTHER
                   MOVE "Y" TO WS-BAD
               END-EVALUATE
           END-PERFORM.
           IF WS-BAD = "N" AND WS-DIG > 0
               COMPUTE WS-VALUE = FUNCTION NUMVAL (WS-PARSE)
           ELSE
               MOVE "N" TO WS-BAL-OK
           END-IF.

       P-PARSE-X.
           EXIT.

      *================================================================
       P-WRITE.
           MOVE OUT-LINE TO FILEOUT01.
           WRITE FILEOUT01.

      *================================================================
       R90.
           MOVE SPACES TO OUT-LINE
           PERFORM P-WRITE.
           MOVE SPACES TO TOTAL-LINE.
           MOVE "TOTALS"     TO TOTAL-LINE(1:22).
           MOVE GT-EXP-BAL   TO TL-EXP.
           MOVE GT-THEN-BAL  TO TL-THEN.
           MOVE GT-SINCE     TO TL-SINCE.
           MOVE GT-NOW-BAL   TO TL-NOW.
           MOVE TOTAL-LINE TO OUT-LINE
           PERFORM P-WRITE.
           MOVE SPACES TO OUT-LINE
           PERFORM P-WRITE.

           MOVE "GUARANTORS         " TO ML-TEXT.
             MOVE WS-GAR-CNT TO ML-VAL.
             MOVE MSG-LINE TO OUT-LINE
             PERFORM P-WRITE.
           MOVE "REBUILD AGREED     " TO ML-TEXT.
             MOVE WS-RECON-CNT TO ML-VAL.
             MOVE MSG-LINE TO OUT-LINE
             PERFORM P-WRITE.
           MOVE "REBUILD DISAGREED  " TO ML-TEXT.
             MOVE WS-UNRECON-CNT TO ML-VAL.
             MOVE MSG-LINE TO OUT-LINE
             PERFORM P-WRITE.
           MOVE "NO 018 MATCH       " TO ML-TEXT.
             MOVE WS-NOTFND-CNT TO ML-VAL.
             MOVE MSG-LINE TO OUT-LINE
             PERFORM P-WRITE.
           MOVE "UNPARSABLE AMOUNTS " TO ML-TEXT.
             MOVE WS-BADBAL-CNT TO ML-VAL.
             MOVE MSG-LINE TO OUT-LINE
             PERFORM P-WRITE.
           MOVE "DISCHR MISMATCHES  " TO ML-TEXT.
             MOVE WS-DOSDIFF-CNT TO ML-VAL.
             MOVE MSG-LINE TO OUT-LINE
             PERFORM P-WRITE.
           MOVE SPACES TO OUT-LINE
           PERFORM P-WRITE.
           MOVE "SEND AS IS         " TO ML-TEXT.
             MOVE WS-SEND-CNT TO ML-VAL.
             MOVE MSG-LINE TO OUT-LINE
             PERFORM P-WRITE.
           MOVE "SEND REDUCED       " TO ML-TEXT.
             MOVE WS-REDUCE-CNT TO ML-VAL.
             MOVE MSG-LINE TO OUT-LINE
             PERFORM P-WRITE.
           MOVE "PULL FROM FILE     " TO ML-TEXT.
             MOVE WS-PULL-CNT TO ML-VAL.
             MOVE MSG-LINE TO OUT-LINE
             PERFORM P-WRITE.
           MOVE SPACES TO CEN-LINE.
             MOVE "AMT TO AGENCY" TO CL-DATE.
             MOVE GT-SENDABLE TO CL-AMT.
             MOVE CEN-LINE TO OUT-LINE
             PERFORM P-WRITE.

       R99.
           CLOSE CHARCUR PAYCUR FILEIN FILEOUT.
           STOP RUN.