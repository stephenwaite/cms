      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2026 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
      *
      * xxxr054 - post the outliers adjust_fees.php listed on the
      *           console to the procedure file.  nothing else in the
      *           file is touched, xxxr053 still does the percent.
      *
      *   S31  input  the captured php listing, one line per outlier
      *               cpt ratio nn.nn > 10, rad fee $nnnn cms allows
      *               $nnnn.nn new fee is $nnnn
      *   S30  i-o    procedure file
      *   S35  output audit report
      *
      * the amount in the listing has NOT been bumped 2.5 percent
      * yet, so the run order is
      *
      *   1  xxxr054  pull the outliers back in line
      *   2  xxxr053  2.5 percent across the board
      *
      * which lands on the same number the php wrote to 2027_fees.
      * the other way round gives back the 2.5 percent on every
      * outlier.
      *
      * the php rates one fee per cpt, whichever cdm sorted last in
      * the wsid dump, but a cpt can sit on several cdms at several
      * prices.  so we take the allowable off the listing and rate
      * each proc record on its own amount with the same 10x and 5x
      * rule.  a record already inside the band is listed and left
      * alone.
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. xxxr054.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT PROCFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PROC-KEY
           LOCK MODE MANUAL.

           SELECT OUTLIER ASSIGN TO "S31"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S35"
           ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  PROCFILE
           DATA RECORD PROCFILE01.
       01  PROCFILE01.
           02 PROC-KEY.
             03 PROC-KEY1 PIC X(4).
             03 PROC-KEY2.
               04 PROC-CPT PIC X(5).
               04 PROC-MOD PIC XX.
           02 PROC-TYPE PIC X.
           02 PROC-TITLE PIC X(28).
           02 PROC-AMOUNT PIC 9(4)V99.

       FD  OUTLIER.
       01  OUTLIER01 PIC X(120).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(110).

       WORKING-STORAGE SECTION.

      * the band the php works to
       01  HI-MULT PIC 99 VALUE 10.
       01  LO-MULT PIC 99 VALUE 5.

       01  EOF-OUT PIC X VALUE "N".
           88 END-OF-OUT VALUE "Y".
       01  EOF-PROC PIC X VALUE "N".
           88 END-OF-PROC VALUE "Y".
       01  FOUND-SW PIC X VALUE "N".
           88 CPT-FOUND VALUE "Y".

       01  MAX-OUT PIC 9(5) VALUE 999.
       01  OUT-COUNT PIC 9(5) VALUE 0.
       01  SUB1 PIC 9(5) VALUE 0.

      * the php print widths move with the size of the numbers, so
      * split on the dollar signs and the words, never on columns
       01  D1 PIC X(60).
       01  D2 PIC X(40).
       01  D3 PIC X(40).
       01  D4 PIC X(20).
       01  W-CODE PIC X(10).
       01  W-LIT PIC X(10).
       01  W-RATIO PIC X(12).
       01  W-DIR PIC X(4).
       01  W-OLD PIC X(20).
       01  W-ALLOW PIC X(20).
       01  N-RATIO PIC 9(5)V99 VALUE 0.
       01  N-OLD PIC 9(7)V99 VALUE 0.
       01  N-ALLOW PIC 9(7)V99 VALUE 0.
       01  N-NEW PIC 9(7)V99 VALUE 0.

       01  W-RAT PIC 9(5)V99 VALUE 0.
       01  W-NEW PIC 9(7)V99 VALUE 0.
      * the php rounds the new fee to whole dollars, so do the same
       01  W-WHOLE PIC 9(7) VALUE 0.

       01  CNT-LINE PIC 9(6) VALUE 0.
       01  CNT-BAD PIC 9(6) VALUE 0.
       01  CNT-READ PIC 9(6) VALUE 0.
       01  CNT-HIT PIC 9(6) VALUE 0.
       01  CNT-CUT PIC 9(6) VALUE 0.
       01  CNT-UP PIC 9(6) VALUE 0.
       01  CNT-OK PIC 9(6) VALUE 0.
       01  CNT-RERATE PIC 9(6) VALUE 0.
       01  CNT-CANARY PIC 9(6) VALUE 0.
       01  CNT-UNUSED PIC 9(6) VALUE 0.

       01  OUT-TABLE.
           02 OUT-ENTRY OCCURS 1 TO 999 TIMES
              DEPENDING ON OUT-COUNT
              INDEXED BY OT-IDX.
             03 OT-CPT PIC X(5).
             03 OT-DIR PIC X.
             03 OT-RATIO PIC 9(5)V99.
             03 OT-OLD PIC 9(4)V99.
             03 OT-ALLOW PIC 9(4)V99.
             03 OT-NEW PIC 9(4)V99.
             03 OT-HITS PIC 9(5).

       01  WS-DATE.
           02 WS-YY PIC 9(4).
           02 WS-MM PIC 99.
           02 WS-DD PIC 99.
           02 FILLER PIC X(13).

       01  RPT-HD1.
           02 FILLER PIC X(36) VALUE
              "XXXR054  2027 OUTLIER FIX  RUN DATE".
           02 HD-MM PIC 99.
           02 FILLER PIC X VALUE "/".
           02 HD-DD PIC 99.
           02 FILLER PIC X VALUE "/".
           02 HD-YY PIC 9(4).
           02 FILLER PIC X(4) VALUE SPACES.
           02 FILLER PIC X(6) VALUE "BAND  ".
           02 HD-LO PIC Z9.
           02 FILLER PIC X(4) VALUE " TO ".
           02 HD-HI PIC Z9.
           02 FILLER PIC X(15) VALUE " TIMES ALLOWED ".

       01  RPT-HD2.
           02 FILLER PIC X(43) VALUE
              "CDM/CPT/MOD  DESCRIPTION".
           02 FILLER PIC X(8) VALUE "   ALLOW".
           02 FILLER PIC XX VALUE SPACES.
           02 FILLER PIC X(8) VALUE "     OLD".
           02 FILLER PIC XX VALUE SPACES.
           02 FILLER PIC X(8) VALUE "   RATIO".
           02 FILLER PIC XX VALUE SPACES.
           02 FILLER PIC X(8) VALUE "     NEW".
           02 FILLER PIC XX VALUE SPACES.
           02 FILLER PIC X(14) VALUE "FLAG".

       01  RPT-DET.
           02 RD-KEY PIC X(11).
           02 FILLER PIC XX VALUE SPACES.
           02 RD-NAME PIC X(28).
           02 FILLER PIC XX VALUE SPACES.
           02 RD-ALLOW PIC Z,ZZ9.99.
           02 FILLER PIC XX VALUE SPACES.
           02 RD-OLD PIC Z,ZZ9.99.
           02 FILLER PIC XX VALUE SPACES.
           02 RD-RATIO PIC ZZZZ9.99.
           02 FILLER PIC XX VALUE SPACES.
           02 RD-NEW PIC Z,ZZ9.99.
           02 FILLER PIC XX VALUE SPACES.
           02 RD-FLAG PIC X(14).

       01  RPT-TOT.
           02 RT-TEXT PIC X(36).
           02 RT-CNT PIC ZZZ,ZZ9.

       01  RPT-UNU.
           02 FILLER PIC X(20) VALUE "  NOT IN PROC FILE  ".
           02 RU-CPT PIC X(5).
           02 FILLER PIC XX VALUE SPACES.
           02 RU-NEW PIC Z,ZZ9.99.

       PROCEDURE DIVISION.

       P0.
           PERFORM P-LOAD
           PERFORM P-OPEN-RPT
           PERFORM P-UPDATE
           PERFORM P-UNUSED
           PERFORM P-TOTALS
           STOP RUN.

      * load the php listing into core
       P-LOAD.
           OPEN INPUT OUTLIER
           PERFORM UNTIL END-OF-OUT
             READ OUTLIER
               AT END
                 SET END-OF-OUT TO TRUE
               NOT AT END
                 PERFORM P-LOAD-1
             END-READ
           END-PERFORM
           CLOSE OUTLIER.

       P-LOAD-1.
           IF OUTLIER01 = SPACES
             GO TO P-LOAD-X
           END-IF
           ADD 1 TO CNT-LINE
           MOVE SPACES TO D1 D2 D3 D4
           MOVE SPACES TO W-CODE W-LIT W-RATIO W-DIR W-OLD W-ALLOW
           UNSTRING OUTLIER01 DELIMITED BY "$"
             INTO D1 D2 D3 D4
           END-UNSTRING
           UNSTRING D1 DELIMITED BY ALL " "
             INTO W-CODE W-LIT W-RATIO W-DIR
           END-UNSTRING
           UNSTRING D2 DELIMITED BY " cms"
             INTO W-OLD
           END-UNSTRING
           UNSTRING D3 DELIMITED BY " new"
             INTO W-ALLOW
           END-UNSTRING

           IF W-CODE = SPACES OR D4 = SPACES
             ADD 1 TO CNT-BAD
             GO TO P-LOAD-X
           END-IF
           IF W-LIT NOT = "ratio"
             ADD 1 TO CNT-BAD
             GO TO P-LOAD-X
           END-IF
           IF W-DIR NOT = ">" AND W-DIR NOT = "<"
             ADD 1 TO CNT-BAD
             GO TO P-LOAD-X
           END-IF

           COMPUTE N-RATIO = FUNCTION NUMVAL (W-RATIO)
           COMPUTE N-OLD = FUNCTION NUMVAL (W-OLD)
           COMPUTE N-ALLOW = FUNCTION NUMVAL (W-ALLOW)
           COMPUTE N-NEW = FUNCTION NUMVAL (D4)

           IF N-ALLOW = 0 OR N-ALLOW > 9999
             ADD 1 TO CNT-BAD
             GO TO P-LOAD-X
           END-IF
           IF N-NEW = 0 OR N-NEW > 9999
             ADD 1 TO CNT-BAD
             GO TO P-LOAD-X
           END-IF
           IF OUT-COUNT NOT < MAX-OUT
             ADD 1 TO CNT-BAD
             GO TO P-LOAD-X
           END-IF

           ADD 1 TO OUT-COUNT
           MOVE W-CODE (1:5) TO OT-CPT (OUT-COUNT)
           MOVE W-DIR (1:1) TO OT-DIR (OUT-COUNT)
           MOVE N-RATIO TO OT-RATIO (OUT-COUNT)
           MOVE N-OLD TO OT-OLD (OUT-COUNT)
           MOVE N-ALLOW TO OT-ALLOW (OUT-COUNT)
           MOVE N-NEW TO OT-NEW (OUT-COUNT)
           MOVE 0 TO OT-HITS (OUT-COUNT).

       P-LOAD-X.
           EXIT.

       P-OPEN-RPT.
           MOVE FUNCTION CURRENT-DATE TO WS-DATE
           MOVE WS-MM TO HD-MM
           MOVE WS-DD TO HD-DD
           MOVE WS-YY TO HD-YY
           MOVE LO-MULT TO HD-LO
           MOVE HI-MULT TO HD-HI
           OPEN OUTPUT FILEOUT
           MOVE RPT-HD1 TO FILEOUT01
           WRITE FILEOUT01
           MOVE SPACES TO FILEOUT01
           WRITE FILEOUT01
           MOVE RPT-HD2 TO FILEOUT01
           WRITE FILEOUT01.

      * walk the proc file, touch only the outlier cpts
       P-UPDATE.
           OPEN I-O PROCFILE
           PERFORM UNTIL END-OF-PROC
             READ PROCFILE NEXT WITH LOCK
               AT END
                 SET END-OF-PROC TO TRUE
               NOT AT END
                 PERFORM P-PROC-1
             END-READ
           END-PERFORM
           CLOSE PROCFILE.

       P-PROC-1.
           ADD 1 TO CNT-READ
           IF PROC-AMOUNT = 0
             UNLOCK PROCFILE
           ELSE
             PERFORM P-FIND
             IF CPT-FOUND
               PERFORM P-RATE
             ELSE
               UNLOCK PROCFILE
             END-IF
           END-IF.

       P-FIND.
           MOVE "N" TO FOUND-SW
           IF OUT-COUNT > 0
             SET OT-IDX TO 1
             SEARCH OUT-ENTRY
               AT END
                 MOVE "N" TO FOUND-SW
               WHEN OT-CPT (OT-IDX) = PROC-CPT
                 MOVE "Y" TO FOUND-SW
             END-SEARCH
           END-IF.

      * rate this record on its own amount, same rule as the php
       P-RATE.
           ADD 1 TO CNT-HIT
           ADD 1 TO OT-HITS (OT-IDX)
           MOVE SPACES TO RPT-DET
           MOVE PROC-KEY TO RD-KEY
           MOVE PROC-TITLE TO RD-NAME
           MOVE OT-ALLOW (OT-IDX) TO RD-ALLOW
           MOVE PROC-AMOUNT TO RD-OLD

           COMPUTE W-RAT ROUNDED = PROC-AMOUNT / OT-ALLOW (OT-IDX)
           MOVE W-RAT TO RD-RATIO

           IF W-RAT > HI-MULT
             COMPUTE W-WHOLE ROUNDED = HI-MULT * OT-ALLOW (OT-IDX)
             MOVE W-WHOLE TO W-NEW
             ADD 1 TO CNT-CUT
             PERFORM P-POST
           ELSE
             IF W-RAT < LO-MULT
               COMPUTE W-WHOLE ROUNDED = LO-MULT * OT-ALLOW (OT-IDX)
               MOVE W-WHOLE TO W-NEW
               ADD 1 TO CNT-UP
               PERFORM P-POST
             ELSE
               ADD 1 TO CNT-OK
               MOVE PROC-AMOUNT TO RD-NEW
               MOVE "IN BAND, KEPT" TO RD-FLAG
               UNLOCK PROCFILE
             END-IF
           END-IF

           MOVE RPT-DET TO FILEOUT01
           WRITE FILEOUT01.

       P-POST.
      * this is the cdm the php rated, so our answer has to match it
           IF PROC-AMOUNT = OT-OLD (OT-IDX)
             IF W-NEW NOT = OT-NEW (OT-IDX)
               ADD 1 TO CNT-CANARY
               MOVE "PHP DISAGREES" TO RD-FLAG
             END-IF
           ELSE
             ADD 1 TO CNT-RERATE
             MOVE "OWN FEE RATED" TO RD-FLAG
           END-IF
           MOVE W-NEW TO RD-NEW
           MOVE W-NEW TO PROC-AMOUNT
           REWRITE PROCFILE01.

      * outlier cpts that never turned up in the proc file
       P-UNUSED.
           MOVE SPACES TO FILEOUT01
           WRITE FILEOUT01
           PERFORM VARYING SUB1 FROM 1 BY 1
             UNTIL SUB1 > OUT-COUNT
             IF OT-HITS (SUB1) = 0
               ADD 1 TO CNT-UNUSED
               MOVE OT-CPT (SUB1) TO RU-CPT
               MOVE OT-NEW (SUB1) TO RU-NEW
               MOVE RPT-UNU TO FILEOUT01
               WRITE FILEOUT01
             END-IF
           END-PERFORM.

       P-TOTALS.
           MOVE SPACES TO FILEOUT01
           WRITE FILEOUT01
           MOVE "OUTLIER LINES READ" TO RT-TEXT
           MOVE CNT-LINE TO RT-CNT
           PERFORM P-TOT-1
           MOVE "OUTLIER LINES LOADED" TO RT-TEXT
           MOVE OUT-COUNT TO RT-CNT
           PERFORM P-TOT-1
           MOVE "OUTLIER LINES UNREADABLE" TO RT-TEXT
           MOVE CNT-BAD TO RT-CNT
           PERFORM P-TOT-1
           MOVE "PROC RECORDS READ" TO RT-TEXT
           MOVE CNT-READ TO RT-CNT
           PERFORM P-TOT-1
           MOVE "PROC RECORDS ON OUTLIER CPTS" TO RT-TEXT
           MOVE CNT-HIT TO RT-CNT
           PERFORM P-TOT-1
           MOVE "  CUT BACK TO HIGH MULTIPLE" TO RT-TEXT
           MOVE CNT-CUT TO RT-CNT
           PERFORM P-TOT-1
           MOVE "  RAISED TO LOW MULTIPLE" TO RT-TEXT
           MOVE CNT-UP TO RT-CNT
           PERFORM P-TOT-1
           MOVE "  IN BAND ON OWN FEE, LEFT" TO RT-TEXT
           MOVE CNT-OK TO RT-CNT
           PERFORM P-TOT-1
           MOVE "  RATED ON A FEE PHP NEVER SAW" TO RT-TEXT
           MOVE CNT-RERATE TO RT-CNT
           PERFORM P-TOT-1
           MOVE "  DISAGREED WITH THE PHP" TO RT-TEXT
           MOVE CNT-CANARY TO RT-CNT
           PERFORM P-TOT-1
           MOVE "OUTLIER CPTS NOT IN PROC FILE" TO RT-TEXT
           MOVE CNT-UNUSED TO RT-CNT
           PERFORM P-TOT-1
           CLOSE FILEOUT.

       P-TOT-1.
           MOVE RPT-TOT TO FILEOUT01
           WRITE FILEOUT01.
