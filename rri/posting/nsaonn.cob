      * @package cms
      * @author  s waite <cmswest@sover.net>
      * @author  Claude
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
      *
      * nsaonn - No Surprises Act open negotiation notices from an 835.
      * picks service lines flagged LQ*HE*N860 / N877 (the same NSA
      * test hiproa uses), skips reversals (CLP02 22), and writes one
      * notice per ST/SE as markdown, from nsaonn-template.md, for
      * review and print to pdf. the offer is left as [ENTER OFFER].
      * a review block in an html comment (hidden when printed) holds
      * the payer, check, 835 payment date, the send-by deadline and
      * the per-line amounts.
      *
      * S30 835 filein, S35 parmfile, S40 procfile (optional),
      * S45 notice .md out, S50 template .md
      * parmfile lines: 1 party name, 2 party type, 3 services
      * descriptor, 4 group npi, 5 signer, 6 relationship,
      * 7 mailing address, 8 telephone, 9 email,
      * 10 notice date yyyymmdd (blank = today)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. nsaonn.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO "S30" ORGANIZATION
               LINE SEQUENTIAL.
           SELECT PARMFILE ASSIGN TO "S35" ORGANIZATION
               LINE SEQUENTIAL.
           SELECT PROCFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
               ACCESS IS SEQUENTIAL RECORD KEY IS PROC-KEY
               LOCK MODE MANUAL
               FILE STATUS IS PROC-FS.
           SELECT NOTICE-FILE ASSIGN TO "S45" ORGANIZATION
               LINE SEQUENTIAL.
           SELECT TEMPLATE-FILE ASSIGN TO "S50" ORGANIZATION
               LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  FILEIN.
       01  FILEIN01 PIC X(1200).
       FD  PARMFILE.
       01  PARMFILE01 PIC X(80).
       FD  PROCFILE.
           COPY procfile.cpy.
       FD  NOTICE-FILE.
       01  NOTICE01 PIC X(900).
       FD  TEMPLATE-FILE.
       01  TEMPLATE01 PIC X(400).

       WORKING-STORAGE SECTION.
       01  PROC-FS        PIC XX.
       01  PROC-OK        PIC 9 VALUE 0.
       01  EOF-IN         PIC 9 VALUE 0.
       01  EOF-PARM       PIC 9 VALUE 0.
       01  EOF-TPL        PIC 9 VALUE 0.
       01  NOTICE-CNT     PIC 99 VALUE 0.
       01  CUR-DATE21     PIC X(21).
       01  COMP-SEP       PIC X VALUE ":".

      * elements of the current segment and of a composite
       01  ELEMS.
           02 E           PIC X(80) OCCURS 20 TIMES.
       01  COMPS.
           02 C           PIC X(10) OCCURS 6 TIMES.
       01  K              PIC 99.

      * parmfile
       01  P-PARTY        PIC X(60) VALUE SPACE.
       01  P-TYPE         PIC X(60) VALUE SPACE.
       01  P-SERVICES     PIC X(80) VALUE SPACE.
       01  P-NPI          PIC X(10) VALUE SPACE.
       01  P-SIGNER       PIC X(60) VALUE SPACE.
       01  P-RELATE       PIC X(60) VALUE SPACE.
       01  P-ADDRESS      PIC X(80) VALUE SPACE.
       01  P-PHONE        PIC X(30) VALUE SPACE.
       01  P-EMAIL        PIC X(60) VALUE SPACE.
       01  P-DATE         PIC X(8)  VALUE SPACE.
       01  P-DATE-N REDEFINES P-DATE PIC 9(8).

      * transaction set
       01  TS-PAYER       PIC X(60).
       01  TS-PAYERID     PIC X(15).
       01  TS-PAYDATE     PIC X(8).
       01  TS-PAYDATE-N REDEFINES TS-PAYDATE PIC 9(8).
       01  TS-CHECK       PIC X(30).
       01  TS-PE-NPI      PIC X(10).

      * claim
       01  CL-ACCT        PIC X(20).
       01  CL-STAT        PIC XX.
       01  CL-ICN         PIC X(30).
       01  CL-PAT         PIC X(40).
       01  CL-NPI         PIC X(10).
       01  CL-RNAME       PIC X(50).
       01  CL-DOS         PIC X(8).

      * service line being collected
       01  IN-SVC         PIC 9 VALUE 0.
       01  SV-NSA         PIC 9 VALUE 0.
       01  SV-CPT         PIC X(5).
       01  SV-M1          PIC XX.
       01  SV-CODE        PIC X(17).
       01  SV-CHG         PIC S9(7)V99.
       01  SV-PAID        PIC S9(7)V99.
       01  SV-ALLOW       PIC S9(7)V99.
       01  SV-DOS         PIC X(8).
       01  SV-CAS         PIC X(60).
       01  CAS-TMP        PIC X(60).
       01  S-PTR          PIC 999.

      * rows for the notice being built
       01  ROW-CNT        PIC 99 VALUE 0.
       01  ROW-OVER       PIC 9 VALUE 0.
       01  R              PIC 99.
       01  ROWS01.
           02 ROW OCCURS 99 TIMES.
              03 R-DESC   PIC X(28).
              03 R-ICN    PIC X(30).
              03 R-PROV   PIC X(80).
              03 R-DOS    PIC X(8).
              03 R-CODE   PIC X(17).
              03 R-PAID   PIC S9(7)V99.
              03 R-CHG    PIC S9(7)V99.
              03 R-ALLOW  PIC S9(7)V99.
              03 R-ACCT   PIC X(20).
              03 R-PAT    PIC X(40).
              03 R-CAS    PIC X(60).
      * procfile is keyed cdm + cpt + mod, the 835 only has the cpt,
      * so it is read once into a table
       01  PT-CNT         PIC 9(4) VALUE 0.
       01  PT-I           PIC 9(4).
       01  PT-HIT         PIC 9(4).
       01  PT-TAB01.
           02 PT OCCURS 4000 TIMES.
              03 PT-CPT   PIC X(5).
              03 PT-MOD   PIC XX.
              03 PT-TITLE PIC X(28).
       01  PROV-NAME      PIC X(60).
       01  PROV-NPI       PIC X(10).

      * dates. integers are days from INTEGER-OF-DATE, 0 = monday
       01  D-INT          PIC 9(8).
       01  D-YMD.
           02 D-YYYY      PIC 9(4).
           02 D-MM        PIC 99.
           02 D-DD        PIC 99.
       01  D-YMD-N REDEFINES D-YMD PIC 9(8).
       01  D-DOW          PIC 9.
       01  BD-OK          PIC 9.
       01  BD-N           PIC 99.
       01  BD-CNT         PIC 99.
       01  ND-INT         PIC 9(8).
       01  EN-INT         PIC 9(8).
       01  IB-INT         PIC 9(8).
       01  SB-INT         PIC 9(8) VALUE 0.
       01  HOL-YEAR       PIC 9(4) VALUE 0.
       01  HOL-TAB01.
           02 HOL         PIC 9(8) OCCURS 12 TIMES.
       01  H-I            PIC 99.
       01  H-Y            PIC 9(4).
       01  H-M            PIC 99.
       01  H-T            PIC 9.
       01  H-N            PIC 9.
       01  H-MMDD         PIC 9(4).
       01  H-F            PIC 9(8).
       01  H-FD           PIC 9.
       01  H-OUT          PIC 9(8).
       01  MONTHS-V.
           02 FILLER PIC X(9) VALUE "January".
           02 FILLER PIC X(9) VALUE "February".
           02 FILLER PIC X(9) VALUE "March".
           02 FILLER PIC X(9) VALUE "April".
           02 FILLER PIC X(9) VALUE "May".
           02 FILLER PIC X(9) VALUE "June".
           02 FILLER PIC X(9) VALUE "July".
           02 FILLER PIC X(9) VALUE "August".
           02 FILLER PIC X(9) VALUE "September".
           02 FILLER PIC X(9) VALUE "October".
           02 FILLER PIC X(9) VALUE "November".
           02 FILLER PIC X(9) VALUE "December".
       01  MONTHS-R REDEFINES MONTHS-V.
           02 MON-NAME PIC X(9) OCCURS 12 TIMES.
       01  DAYS-V.
           02 FILLER PIC X(9) VALUE "Monday".
           02 FILLER PIC X(9) VALUE "Tuesday".
           02 FILLER PIC X(9) VALUE "Wednesday".
           02 FILLER PIC X(9) VALUE "Thursday".
           02 FILLER PIC X(9) VALUE "Friday".
           02 FILLER PIC X(9) VALUE "Saturday".
           02 FILLER PIC X(9) VALUE "Sunday".
       01  DAYS-R REDEFINES DAYS-V.
           02 DAY-NAME PIC X(9) OCCURS 7 TIMES.
       01  LD-WEEKDAY     PIC 9 VALUE 0.
       01  LD-D2          PIC XX.
       01  LD-TEXT        PIC X(40).
       01  ND-TEXT        PIC X(40).
       01  EN-TEXT        PIC X(40).
       01  IB-TEXT        PIC X(40).
       01  SB-TEXT        PIC X(40).
       01  SHORT-DATE     PIC X(10).

      * money and text helpers
       01  MONEY-IN       PIC S9(7)V99.
       01  MONEY-ED       PIC $$,$$$,$$9.99.
       01  MONEY-TXT      PIC X(14).
       01  LEAD           PIC 99.
       01  NUM-TXT        PIC X(3).
       01  ROW-NUM        PIC Z9.
       01  WORK-VAL       PIC X(200).
       01  V-LEN          PIC 999.
       01  V-I            PIC S999.
       01  TOK            PIC X(20).
       01  TOK-LEN        PIC 99.
       01  L-POS          PIC 999.
       01  LINE-BUF       PIC X(900).
       01  LINE-NEW       PIC X(900).

       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT FILEIN PARMFILE TEMPLATE-FILE
           CLOSE TEMPLATE-FILE
           OPEN OUTPUT NOTICE-FILE
           OPEN INPUT PROCFILE
           IF PROC-FS = "00"
               MOVE 1 TO PROC-OK
               PERFORM LOAD-PROCS
               CLOSE PROCFILE
           END-IF
           PERFORM READ-PARMS
           PERFORM NEW-TS.

       P00.
           MOVE SPACE TO FILEIN01
           READ FILEIN
               AT END
                   GO TO P9
           END-READ
           INSPECT FILEIN01 REPLACING ALL "~" BY SPACE
           MOVE SPACE TO ELEMS
           UNSTRING FILEIN01 DELIMITED BY "*" INTO
               E(1) E(2) E(3) E(4) E(5) E(6) E(7) E(8) E(9) E(10)
               E(11) E(12) E(13) E(14) E(15) E(16) E(17) E(18)
               E(19) E(20)

           EVALUATE E(1)
               WHEN "ISA"
                   IF E(17) NOT = SPACE
                       MOVE E(17)(1:1) TO COMP-SEP
                   END-IF
               WHEN "ST"
                   PERFORM FLUSH-SVC
                   PERFORM NEW-TS
               WHEN "BPR"
                   MOVE E(17) TO TS-PAYDATE
               WHEN "TRN"
                   MOVE E(3) TO TS-CHECK
               WHEN "N1"
                   IF E(2) = "PR"
                       MOVE E(3) TO TS-PAYER
                       IF TS-PAYERID = SPACE
                           MOVE E(5) TO TS-PAYERID
                       END-IF
                   END-IF
                   IF E(2) = "PE" AND E(4) = "XX"
                       MOVE E(5) TO TS-PE-NPI
                   END-IF
               WHEN "REF"
                   IF E(2) = "2U"
                       MOVE E(3) TO TS-PAYERID
                   END-IF
               WHEN "CLP"
                   PERFORM FLUSH-SVC
                   MOVE E(2) TO CL-ACCT
                   MOVE E(3) TO CL-STAT
                   MOVE E(8) TO CL-ICN
                   MOVE SPACE TO CL-PAT CL-NPI CL-RNAME CL-DOS
               WHEN "NM1"
                   PERFORM P-NM1
               WHEN "DTM"
                   IF E(2) = "472"
                       IF IN-SVC = 1
                           MOVE E(3) TO SV-DOS
                       ELSE
                           MOVE E(3) TO CL-DOS
                       END-IF
                   END-IF
                   IF E(2) = "232" AND IN-SVC = 0 AND CL-DOS = SPACE
                       MOVE E(3) TO CL-DOS
                   END-IF
               WHEN "SVC"
                   PERFORM FLUSH-SVC
                   PERFORM P-SVC
               WHEN "CAS"
                   IF IN-SVC = 1
                       MOVE SPACE TO CAS-TMP
                       MOVE 1 TO S-PTR
                       STRING SV-CAS DELIMITED BY "  "
                           " " DELIMITED BY SIZE
                           E(2) DELIMITED BY SPACE
                           "-" DELIMITED BY SIZE
                           E(3) DELIMITED BY SPACE
                           " " DELIMITED BY SIZE
                           E(4) DELIMITED BY SPACE
                           INTO CAS-TMP WITH POINTER S-PTR
                       MOVE CAS-TMP TO SV-CAS
                   END-IF
               WHEN "LQ"
                   IF IN-SVC = 1 AND E(2) = "HE"
                       AND (E(3) = "N860" OR E(3) = "N877")
                       MOVE 1 TO SV-NSA
                   END-IF
               WHEN "AMT"
                   IF IN-SVC = 1 AND E(2) = "B6"
                       MOVE E(3) TO WORK-VAL
                       PERFORM NUM-OF
                       MOVE MONEY-IN TO SV-ALLOW
                   END-IF
               WHEN "SE"
                   PERFORM FLUSH-SVC
                   IF ROW-CNT > 0
                       PERFORM WRITE-NOTICE
                   END-IF
                   PERFORM NEW-TS
           END-EVALUATE
           GO TO P00.

       P9.
           PERFORM FLUSH-SVC
           IF ROW-CNT > 0
               PERFORM WRITE-NOTICE
           END-IF
           IF NOTICE-CNT = 0
               MOVE "<!-- no N860 / N877 lines in this 835 -->"
                   TO NOTICE01
               WRITE NOTICE01
           END-IF
           CLOSE FILEIN PARMFILE NOTICE-FILE
           STOP RUN.

       LOAD-PROCS.
           PERFORM UNTIL PROC-FS NOT = "00" OR PT-CNT = 4000
               READ PROCFILE
                   AT END
                       MOVE "10" TO PROC-FS
                   NOT AT END
                       ADD 1 TO PT-CNT
                       MOVE PROC-CPT TO PT-CPT(PT-CNT)
                       MOVE PROC-MOD TO PT-MOD(PT-CNT)
                       MOVE PROC-TITLE TO PT-TITLE(PT-CNT)
               END-READ
           END-PERFORM.

       READ-PARMS.
           MOVE 0 TO EOF-PARM
           PERFORM READ-PARM MOVE PARMFILE01 TO P-PARTY
           PERFORM READ-PARM MOVE PARMFILE01 TO P-TYPE
           PERFORM READ-PARM MOVE PARMFILE01 TO P-SERVICES
           PERFORM READ-PARM MOVE PARMFILE01 TO P-NPI
           PERFORM READ-PARM MOVE PARMFILE01 TO P-SIGNER
           PERFORM READ-PARM MOVE PARMFILE01 TO P-RELATE
           PERFORM READ-PARM MOVE PARMFILE01 TO P-ADDRESS
           PERFORM READ-PARM MOVE PARMFILE01 TO P-PHONE
           PERFORM READ-PARM MOVE PARMFILE01 TO P-EMAIL
           PERFORM READ-PARM MOVE PARMFILE01 TO P-DATE
           IF P-DATE NOT NUMERIC
               MOVE FUNCTION CURRENT-DATE TO CUR-DATE21
               MOVE CUR-DATE21(1:8) TO P-DATE
           END-IF.

       READ-PARM.
           MOVE SPACE TO PARMFILE01
           IF EOF-PARM = 0
               READ PARMFILE
                   AT END
                       MOVE 1 TO EOF-PARM
                       MOVE SPACE TO PARMFILE01
               END-READ
           END-IF.

       NEW-TS.
           MOVE SPACE TO TS-PAYER TS-PAYERID TS-PAYDATE TS-CHECK
               TS-PE-NPI CL-ACCT CL-STAT CL-ICN CL-PAT CL-NPI
               CL-RNAME CL-DOS
           MOVE 0 TO ROW-CNT ROW-OVER IN-SVC SV-NSA.

       P-NM1.
           IF E(2) = "QC"
               MOVE SPACE TO CL-PAT
               MOVE 1 TO S-PTR
               STRING E(4) DELIMITED BY "  " INTO CL-PAT
                   WITH POINTER S-PTR
               IF E(5) NOT = SPACE
                   STRING ", " E(5) DELIMITED BY "  " INTO CL-PAT
                       WITH POINTER S-PTR
               END-IF
           END-IF
           IF E(2) = "82"
               MOVE E(10) TO CL-NPI
               MOVE SPACE TO CL-RNAME
               IF E(4) NOT = SPACE
                   MOVE 1 TO S-PTR
                   STRING E(4) DELIMITED BY "  " INTO CL-RNAME
                       WITH POINTER S-PTR
                   IF E(5) NOT = SPACE
                       STRING ", " E(5) DELIMITED BY "  "
                           INTO CL-RNAME WITH POINTER S-PTR
                   END-IF
               END-IF
           END-IF.

       P-SVC.
           MOVE 1 TO IN-SVC
           MOVE 0 TO SV-NSA SV-ALLOW
           MOVE SPACE TO SV-DOS SV-CAS SV-CODE COMPS
           UNSTRING E(2) DELIMITED BY COMP-SEP INTO
               C(1) C(2) C(3) C(4) C(5) C(6)
           MOVE C(2) TO SV-CPT
           MOVE C(3) TO SV-M1
           MOVE 1 TO S-PTR
           STRING C(2) DELIMITED BY SPACE INTO SV-CODE
               WITH POINTER S-PTR
           PERFORM VARYING K FROM 3 BY 1 UNTIL K > 6
               IF C(K) NOT = SPACE
                   STRING "-" C(K) DELIMITED BY SPACE INTO SV-CODE
                       WITH POINTER S-PTR
               END-IF
           END-PERFORM
           MOVE E(3) TO WORK-VAL
           PERFORM NUM-OF
           MOVE MONEY-IN TO SV-CHG
           MOVE E(4) TO WORK-VAL
           PERFORM NUM-OF
           MOVE MONEY-IN TO SV-PAID.

      * the line just finished becomes a notice row if it is flagged
       FLUSH-SVC.
           IF IN-SVC = 1 AND SV-NSA = 1 AND CL-STAT NOT = "22"
               IF ROW-CNT < 99
                   ADD 1 TO ROW-CNT
                   PERFORM FILL-ROW
               ELSE
                   MOVE 1 TO ROW-OVER
               END-IF
           END-IF
           MOVE 0 TO IN-SVC SV-NSA.

       FILL-ROW.
           MOVE ROW-CNT TO R
           MOVE SPACE TO ROW(R)
      *    cpt + mod, else cpt with no mod, else any entry for the cpt
           MOVE 0 TO PT-HIT
           PERFORM VARYING PT-I FROM 1 BY 1
               UNTIL PT-I > PT-CNT OR PT-HIT > 0
               IF PT-CPT(PT-I) = SV-CPT AND PT-MOD(PT-I) = SV-M1
                   MOVE PT-I TO PT-HIT
               END-IF
           END-PERFORM
           PERFORM VARYING PT-I FROM 1 BY 1
               UNTIL PT-I > PT-CNT OR PT-HIT > 0
               IF PT-CPT(PT-I) = SV-CPT AND PT-MOD(PT-I) = SPACE
                   MOVE PT-I TO PT-HIT
               END-IF
           END-PERFORM
           PERFORM VARYING PT-I FROM 1 BY 1
               UNTIL PT-I > PT-CNT OR PT-HIT > 0
               IF PT-CPT(PT-I) = SV-CPT
                   MOVE PT-I TO PT-HIT
               END-IF
           END-PERFORM
           IF PT-HIT > 0
               MOVE PT-TITLE(PT-HIT) TO R-DESC(R)
           ELSE
               STRING "CPT " SV-CPT DELIMITED BY SIZE INTO R-DESC(R)
           END-IF
           MOVE CL-ICN TO R-ICN(R)
           MOVE P-PARTY TO PROV-NAME
           IF CL-RNAME NOT = SPACE
               MOVE CL-RNAME TO PROV-NAME
           END-IF
           MOVE CL-NPI TO PROV-NPI
           IF PROV-NPI = SPACE
               MOVE TS-PE-NPI TO PROV-NPI
           END-IF
           IF PROV-NPI = SPACE
               MOVE P-NPI TO PROV-NPI
           END-IF
           MOVE 1 TO S-PTR
           STRING PROV-NAME DELIMITED BY "  "
               ", NPI " DELIMITED BY SIZE
               PROV-NPI DELIMITED BY SPACE INTO R-PROV(R)
               WITH POINTER S-PTR
           MOVE SV-DOS TO R-DOS(R)
           IF R-DOS(R) = SPACE
               MOVE CL-DOS TO R-DOS(R)
           END-IF
           MOVE SV-CODE TO R-CODE(R)
           MOVE SV-PAID TO R-PAID(R)
           MOVE SV-CHG TO R-CHG(R)
           MOVE SV-ALLOW TO R-ALLOW(R)
           MOVE CL-ACCT TO R-ACCT(R)
           MOVE CL-PAT TO R-PAT(R)
           MOVE SV-CAS TO R-CAS(R).

      * one notice: dates, then the template with tokens filled in
       WRITE-NOTICE.
           IF NOTICE-CNT > 0
               MOVE SPACE TO NOTICE01
               WRITE NOTICE01
               MOVE '<div style="page-break-after: always;"></div>'
                   TO NOTICE01
               WRITE NOTICE01
               MOVE SPACE TO NOTICE01
               WRITE NOTICE01
           END-IF
           ADD 1 TO NOTICE-CNT

           COMPUTE ND-INT = FUNCTION INTEGER-OF-DATE(P-DATE-N)
           MOVE ND-INT TO D-INT
           MOVE 0 TO LD-WEEKDAY
           PERFORM LONG-DATE
           MOVE LD-TEXT TO ND-TEXT
           MOVE ND-INT TO D-INT
           MOVE 30 TO BD-N
           PERFORM ADD-BD
           MOVE D-INT TO EN-INT
           PERFORM LONG-DATE
           MOVE LD-TEXT TO EN-TEXT
           MOVE 4 TO BD-N
           PERFORM ADD-BD
           MOVE D-INT TO IB-INT
           PERFORM LONG-DATE
           MOVE LD-TEXT TO IB-TEXT

      *    send-by: the payment date counts as day 1 of the 30
           MOVE 0 TO SB-INT
           MOVE SPACE TO SB-TEXT
           IF TS-PAYDATE NUMERIC
               COMPUTE D-INT = FUNCTION INTEGER-OF-DATE(TS-PAYDATE-N)
               PERFORM IS-BUSDAY
               MOVE 29 TO BD-N
               IF BD-OK = 0
                   MOVE 30 TO BD-N
               END-IF
               PERFORM ADD-BD
               MOVE D-INT TO SB-INT
               MOVE 1 TO LD-WEEKDAY
               PERFORM LONG-DATE
               MOVE LD-TEXT TO SB-TEXT
               MOVE 0 TO LD-WEEKDAY
           END-IF

           MOVE 0 TO EOF-TPL
           OPEN INPUT TEMPLATE-FILE
           PERFORM UNTIL EOF-TPL = 1
               MOVE SPACE TO TEMPLATE01
               READ TEMPLATE-FILE
                   AT END
                       MOVE 1 TO EOF-TPL
                   NOT AT END
                       PERFORM TEMPLATE-LINE
               END-READ
           END-PERFORM
           CLOSE TEMPLATE-FILE.

       TEMPLATE-LINE.
           EVALUATE TRUE
               WHEN TEMPLATE01 = "{REVIEW}"
                   PERFORM WRITE-REVIEW
               WHEN TEMPLATE01 = "{ROWS}"
                   PERFORM WRITE-ROW VARYING R FROM 1 BY 1
                       UNTIL R > ROW-CNT
               WHEN OTHER
                   MOVE TEMPLATE01 TO LINE-BUF
                   MOVE "{NOTICE_DATE}" TO TOK MOVE 13 TO TOK-LEN
                   MOVE ND-TEXT TO WORK-VAL PERFORM REPLACE-TOK
                   MOVE "{PARTY_TYPE}" TO TOK MOVE 12 TO TOK-LEN
                   MOVE P-TYPE TO WORK-VAL PERFORM REPLACE-TOK
                   MOVE "{PARTY}" TO TOK MOVE 7 TO TOK-LEN
                   MOVE P-PARTY TO WORK-VAL PERFORM REPLACE-TOK
                   MOVE "{SERVICES}" TO TOK MOVE 10 TO TOK-LEN
                   MOVE P-SERVICES TO WORK-VAL PERFORM REPLACE-TOK
                   MOVE "{PAYER}" TO TOK MOVE 7 TO TOK-LEN
                   MOVE TS-PAYER TO WORK-VAL PERFORM REPLACE-TOK
                   MOVE "{END_NEG}" TO TOK MOVE 9 TO TOK-LEN
                   MOVE EN-TEXT TO WORK-VAL PERFORM REPLACE-TOK
                   MOVE "{IDR_BY}" TO TOK MOVE 8 TO TOK-LEN
                   MOVE IB-TEXT TO WORK-VAL PERFORM REPLACE-TOK
                   MOVE "{SIGNER}" TO TOK MOVE 8 TO TOK-LEN
                   MOVE P-SIGNER TO WORK-VAL PERFORM REPLACE-TOK
                   MOVE "{RELATIONSHIP}" TO TOK MOVE 14 TO TOK-LEN
                   MOVE P-RELATE TO WORK-VAL PERFORM REPLACE-TOK
                   MOVE "{ADDRESS}" TO TOK MOVE 9 TO TOK-LEN
                   MOVE P-ADDRESS TO WORK-VAL PERFORM REPLACE-TOK
                   MOVE "{PHONE}" TO TOK MOVE 7 TO TOK-LEN
                   MOVE P-PHONE TO WORK-VAL PERFORM REPLACE-TOK
                   MOVE "{EMAIL}" TO TOK MOVE 7 TO TOK-LEN
                   MOVE P-EMAIL TO WORK-VAL PERFORM REPLACE-TOK
                   MOVE LINE-BUF TO NOTICE01
                   WRITE NOTICE01
           END-EVALUATE.

      * hidden when rendered or printed - keep "--" out of it
       WRITE-REVIEW.
           MOVE "<!-- REVIEW: not part of the notice, hidden when"
               TO NOTICE01
           WRITE NOTICE01
           MOVE "rendered or printed. Fill in the Offer column first."
               TO NOTICE01
           WRITE NOTICE01
           MOVE SPACE TO NOTICE01
           MOVE 1 TO S-PTR
           STRING "Payer: " DELIMITED BY SIZE
               TS-PAYER DELIMITED BY "  "
               " (payer id " DELIMITED BY SIZE
               TS-PAYERID DELIMITED BY SPACE
               ")  Check/EFT: " DELIMITED BY SIZE
               TS-CHECK DELIMITED BY SPACE
               INTO NOTICE01 WITH POINTER S-PTR
           WRITE NOTICE01
           MOVE TS-PAYDATE TO WORK-VAL
           PERFORM SHORT-OF
           MOVE SPACE TO NOTICE01
           STRING "Payment date on the 835 (BPR16): " SHORT-DATE
               DELIMITED BY SIZE INTO NOTICE01
           WRITE NOTICE01
           MOVE SPACE TO NOTICE01
           MOVE 1 TO S-PTR
           STRING "SEND THIS NOTICE BY: " DELIMITED BY SIZE
               SB-TEXT DELIMITED BY "  "
               INTO NOTICE01 WITH POINTER S-PTR
           WRITE NOTICE01
           MOVE "(30th business day counting the 835 payment date as"
               TO NOTICE01
           WRITE NOTICE01
           MOVE "day 1. Check it against the day the remit was"
               TO NOTICE01
           WRITE NOTICE01
           MOVE "actually received.)" TO NOTICE01
           WRITE NOTICE01
           IF SB-INT > 0 AND ND-INT > SB-INT
               MOVE "*** THE NOTICE DATE IS AFTER THE SEND-BY DATE ***"
                   TO NOTICE01
               WRITE NOTICE01
           END-IF
           IF ROW-OVER = 1
               MOVE "*** MORE THAN 99 LINES, ONLY THE FIRST 99 ARE HERE"
                   TO NOTICE01
               WRITE NOTICE01
           END-IF
           PERFORM REVIEW-ROW VARYING R FROM 1 BY 1
               UNTIL R > ROW-CNT
           MOVE "-->" TO NOTICE01
           WRITE NOTICE01.

       REVIEW-ROW.
           PERFORM ROW-NUM-OF
           MOVE R-DOS(R) TO WORK-VAL
           PERFORM SHORT-OF
           MOVE SPACE TO NOTICE01
           MOVE 1 TO S-PTR
           STRING NUM-TXT DELIMITED BY SPACE
               ". " DELIMITED BY SIZE
               R-ACCT(R) DELIMITED BY SPACE
               "  " DELIMITED BY SIZE
               R-PAT(R) DELIMITED BY "  "
               "  " DELIMITED BY SIZE
               SHORT-DATE DELIMITED BY SPACE
               "  " DELIMITED BY SIZE
               R-CODE(R) DELIMITED BY SPACE
               INTO NOTICE01 WITH POINTER S-PTR
           MOVE R-CHG(R) TO MONEY-IN
           PERFORM MONEY-OF
           STRING "  billed " DELIMITED BY SIZE
               MONEY-TXT DELIMITED BY SPACE
               INTO NOTICE01 WITH POINTER S-PTR
           MOVE R-ALLOW(R) TO MONEY-IN
           PERFORM MONEY-OF
           STRING "  allowed " DELIMITED BY SIZE
               MONEY-TXT DELIMITED BY SPACE
               INTO NOTICE01 WITH POINTER S-PTR
           MOVE R-PAID(R) TO MONEY-IN
           PERFORM MONEY-OF
           STRING "  paid " DELIMITED BY SIZE
               MONEY-TXT DELIMITED BY SPACE
               "  CAS" DELIMITED BY SIZE
               R-CAS(R) DELIMITED BY "  "
               INTO NOTICE01 WITH POINTER S-PTR
           WRITE NOTICE01.

      * R to NUM-TXT with no leading space
       ROW-NUM-OF.
           MOVE R TO ROW-NUM
           MOVE ROW-NUM TO NUM-TXT
           IF R < 10
               MOVE ROW-NUM(2:1) TO NUM-TXT
           END-IF.

       WRITE-ROW.
           PERFORM ROW-NUM-OF
           MOVE R-DOS(R) TO WORK-VAL
           PERFORM SHORT-OF
           MOVE "N/A" TO MONEY-TXT
           IF R-PAID(R) NOT = 0
               MOVE R-PAID(R) TO MONEY-IN
               PERFORM MONEY-OF
           END-IF
           MOVE SPACE TO NOTICE01
           MOVE 1 TO S-PTR
           STRING "| " DELIMITED BY SIZE
               NUM-TXT DELIMITED BY SPACE
               ". | " DELIMITED BY SIZE
               R-DESC(R) DELIMITED BY "  "
               " | " DELIMITED BY SIZE
               R-ICN(R) DELIMITED BY SPACE
               " | " DELIMITED BY SIZE
               R-PROV(R) DELIMITED BY "  "
               " | " DELIMITED BY SIZE
               SHORT-DATE DELIMITED BY SPACE
               " | " DELIMITED BY SIZE
               R-CODE(R) DELIMITED BY SPACE
               " | " DELIMITED BY SIZE
               MONEY-TXT DELIMITED BY SPACE
               " | [ENTER OFFER] |" DELIMITED BY SIZE
               INTO NOTICE01 WITH POINTER S-PTR
           WRITE NOTICE01.

      * replace every TOK(1:TOK-LEN) in LINE-BUF with trimmed WORK-VAL
       REPLACE-TOK.
           PERFORM VAL-LEN
           PERFORM VARYING L-POS FROM 1 BY 1 UNTIL L-POS > 880
               IF LINE-BUF(L-POS:TOK-LEN) = TOK(1:TOK-LEN)
                   MOVE SPACE TO LINE-NEW
                   IF L-POS > 1
                       MOVE LINE-BUF(1:L-POS - 1) TO LINE-NEW
                   END-IF
                   IF V-LEN > 0
                       MOVE WORK-VAL(1:V-LEN) TO LINE-NEW(L-POS:V-LEN)
                   END-IF
                   MOVE LINE-BUF(L-POS + TOK-LEN:)
                       TO LINE-NEW(L-POS + V-LEN:)
                   MOVE LINE-NEW TO LINE-BUF
                   COMPUTE L-POS = L-POS + V-LEN - 1
               END-IF
           END-PERFORM.

       VAL-LEN.
           MOVE 0 TO V-LEN
           PERFORM VARYING V-I FROM 200 BY -1
               UNTIL V-I < 1 OR V-LEN > 0
               IF WORK-VAL(V-I:1) NOT = SPACE
                   MOVE V-I TO V-LEN
               END-IF
           END-PERFORM.

      * WORK-VAL holds an 835 amount, result in MONEY-IN
       NUM-OF.
           MOVE 0 TO MONEY-IN
           IF WORK-VAL NOT = SPACE
               COMPUTE MONEY-IN = FUNCTION NUMVAL(WORK-VAL)
           END-IF.

      * MONEY-IN to MONEY-TXT, e.g. $1,234.56, no leading spaces
       MONEY-OF.
           MOVE MONEY-IN TO MONEY-ED
           MOVE 0 TO LEAD
           INSPECT MONEY-ED TALLYING LEAD FOR LEADING SPACE
           MOVE SPACE TO MONEY-TXT
           MOVE MONEY-ED(LEAD + 1:) TO MONEY-TXT.

      * WORK-VAL(1:8) yyyymmdd to SHORT-DATE mm/dd/yyyy
       SHORT-OF.
           MOVE SPACE TO SHORT-DATE
           IF WORK-VAL(1:8) NUMERIC
               STRING WORK-VAL(5:2) "/" WORK-VAL(7:2) "/"
                   WORK-VAL(1:4) DELIMITED BY SIZE INTO SHORT-DATE
           END-IF.

      * D-INT to LD-TEXT, e.g. October 5, 2026 (weekday first when
      * LD-WEEKDAY = 1)
       LONG-DATE.
           COMPUTE D-YMD-N = FUNCTION DATE-OF-INTEGER(D-INT)
           COMPUTE D-DOW = FUNCTION MOD(D-INT - 1, 7)
           MOVE D-DD TO LD-D2
           IF D-DD < 10
               MOVE D-DD(2:1) TO LD-D2
           END-IF
           MOVE SPACE TO LD-TEXT
           MOVE 1 TO S-PTR
           IF LD-WEEKDAY = 1
               STRING DAY-NAME(D-DOW + 1) DELIMITED BY SPACE ", "
                   DELIMITED BY SIZE INTO LD-TEXT WITH POINTER S-PTR
           END-IF
           STRING MON-NAME(D-MM) DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               LD-D2 DELIMITED BY SPACE
               ", " DELIMITED BY SIZE
               D-YYYY DELIMITED BY SIZE
               INTO LD-TEXT WITH POINTER S-PTR.

      * add BD-N business days to D-INT
       ADD-BD.
           MOVE 0 TO BD-CNT
           PERFORM UNTIL BD-CNT = BD-N
               ADD 1 TO D-INT
               PERFORM IS-BUSDAY
               IF BD-OK = 1
                   ADD 1 TO BD-CNT
               END-IF
           END-PERFORM.

      * BD-OK = 1 when D-INT is not a weekend or a federal holiday
       IS-BUSDAY.
           MOVE 1 TO BD-OK
           COMPUTE D-DOW = FUNCTION MOD(D-INT - 1, 7)
           IF D-DOW > 4
               MOVE 0 TO BD-OK
           ELSE
               COMPUTE D-YMD-N = FUNCTION DATE-OF-INTEGER(D-INT)
               IF D-YYYY NOT = HOL-YEAR
                   MOVE D-YYYY TO H-Y
                   PERFORM LOAD-HOL
               END-IF
               PERFORM VARYING H-I FROM 1 BY 1 UNTIL H-I > 12
                   IF HOL(H-I) = D-INT
                       MOVE 0 TO BD-OK
                   END-IF
               END-PERFORM
           END-IF.

      * federal holidays for year H-Y as observed, plus next year's
      * new year's day, which is observed on dec 31 when it is a sat
       LOAD-HOL.
           MOVE H-Y TO HOL-YEAR
           MOVE 0101 TO H-MMDD PERFORM FIXED-HOL MOVE H-OUT TO HOL(1)
           MOVE 0619 TO H-MMDD PERFORM FIXED-HOL MOVE H-OUT TO HOL(2)
           MOVE 0704 TO H-MMDD PERFORM FIXED-HOL MOVE H-OUT TO HOL(3)
           MOVE 1111 TO H-MMDD PERFORM FIXED-HOL MOVE H-OUT TO HOL(4)
           MOVE 1225 TO H-MMDD PERFORM FIXED-HOL MOVE H-OUT TO HOL(5)
      *    mlk, presidents, labor, columbus: nth monday
           MOVE 1 TO H-M MOVE 0 TO H-T MOVE 3 TO H-N
           PERFORM NTH-HOL MOVE H-OUT TO HOL(6)
           MOVE 2 TO H-M MOVE 0 TO H-T MOVE 3 TO H-N
           PERFORM NTH-HOL MOVE H-OUT TO HOL(7)
           MOVE 9 TO H-M MOVE 0 TO H-T MOVE 1 TO H-N
           PERFORM NTH-HOL MOVE H-OUT TO HOL(8)
           MOVE 10 TO H-M MOVE 0 TO H-T MOVE 2 TO H-N
           PERFORM NTH-HOL MOVE H-OUT TO HOL(9)
      *    thanksgiving: 4th thursday
           MOVE 11 TO H-M MOVE 3 TO H-T MOVE 4 TO H-N
           PERFORM NTH-HOL MOVE H-OUT TO HOL(10)
      *    memorial day: last monday in may
           COMPUTE H-F = FUNCTION INTEGER-OF-DATE(H-Y * 10000 + 0531)
           COMPUTE H-FD = FUNCTION MOD(H-F - 1, 7)
           COMPUTE HOL(11) = H-F - H-FD
           ADD 1 TO H-Y
           MOVE 0101 TO H-MMDD PERFORM FIXED-HOL MOVE H-OUT TO HOL(12)
           SUBTRACT 1 FROM H-Y.

      * H-Y / H-MMDD, moved to fri when a sat and mon when a sun
       FIXED-HOL.
           COMPUTE H-F = FUNCTION INTEGER-OF-DATE(H-Y * 10000 + H-MMDD)
           COMPUTE H-FD = FUNCTION MOD(H-F - 1, 7)
           MOVE H-F TO H-OUT
           IF H-FD = 5
               SUBTRACT 1 FROM H-OUT
           END-IF
           IF H-FD = 6
               ADD 1 TO H-OUT
           END-IF.

      * the H-N'th weekday H-T (0 = monday) of month H-M in year H-Y
       NTH-HOL.
           COMPUTE H-F =
               FUNCTION INTEGER-OF-DATE(H-Y * 10000 + H-M * 100 + 1)
           COMPUTE H-FD = FUNCTION MOD(H-F - 1, 7)
           COMPUTE H-OUT = H-F + FUNCTION MOD(H-T - H-FD + 7, 7)
               + 7 * (H-N - 1).
