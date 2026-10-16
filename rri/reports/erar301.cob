      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
      *    remittance detail print from caredetl (iedidetl 835 load),
      *    laid out like y10k/carer301 but every amount and adjustment
      *    is the 835's own - nothing is calculated - and the heading
      *    says it is a provider print of the era, not a payer notice.
      *    for appeal attachments.
      * S30 parmfile  line 1 provider name, line 2 npi (the same
      *               parmfile vaccnrecon uses)
      * S35 reqfile   garno, then dos ccyymmdd lines (none = all)
      * S40 caredetl S45 garfile S50 mbrxfile S55 print out
       IDENTIFICATION DIVISION.
       PROGRAM-ID. erar301.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARMFILE ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT REQFILE ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT CAREDETL ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS DT-KEY
           LOCK MODE MANUAL.
           SELECT GARFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT OPTIONAL MBRXFILE ASSIGN TO "S50"
           ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS MX-KEY
           LOCK MODE MANUAL.
           SELECT FILEOUT ASSIGN TO "S55" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PARMFILE.
       01  PARMFILE01 PIC X(80).
       FD  REQFILE.
       01  REQFILE01 PIC X(40).
       FD  CAREDETL.
           COPY caredetl.cpy.
       FD  GARFILE.
           COPY garfile.cpy.
       FD  MBRXFILE.
           COPY mbrxfile.cpy.
       FD  FILEOUT.
       01  FILEOUT01 PIC X(132).

       WORKING-STORAGE SECTION.
       01  PV-NAME PIC X(40).
       01  PV-NPI PIC X(10).
       01  REQ-GARNO PIC X(8).
       01  DOS-CNT PIC 99 VALUE 0.
       01  DOS-TAB01.
           02 DOS-TAB PIC X(8) OCCURS 40 TIMES.
       01  X PIC 999.
       01  Y PIC 999.
       01  Z PIC 9.
       01  FLAG PIC 9.

      *    caredetl rows for the garno, sorted remit / icn / dos
       01  RW-CNT PIC 999 VALUE 0.
       01  RW-TAB01.
           02 RW-TAB OCCURS 300 TIMES.
             03 RW-SORT PIC X(48).
             03 RW-REC PIC X(272).
       01  RW-HOLD.
           02 RH-SORT PIC X(48).
           02 RH-REC PIC X(272).
       01  RW-OVER PIC 9 VALUE 0.

       01  CUR-REMIT PIC X(17).
       01  CUR-ICN PIC X(13).
       01  THIS-REMIT PIC X(17).

       01  VT-NAME PIC X(24).
       01  VT-LAST PIC X(24).
       01  VT-FIRST PIC X(24).
       01  VT-ID PIC X(20).

       01  TOT-BILLED PIC S9(6)V99.
       01  TOT-ALLOWED PIC S9(6)V99.
       01  TOT-DEDUCT PIC S9(6)V99.
       01  TOT-PAID PIC S9(6)V99.
       01  TOT-PR PIC S9(6)V99.
       01  RM-PAID PIC S9(7)V99.

       01  WS-YMD PIC X(8).
       01  WS-YMD-R REDEFINES WS-YMD.
           02 WS-Y PIC X(4).
           02 WS-M PIC XX.
           02 WS-D PIC XX.
       01  WS-MDY PIC X(10).
       01  WS-TODAY PIC X(8).
       01  WS-ED PIC X(14).
       01  WS-EDP PIC 99.
       01  ADJ-ED PIC -----9.99.
       01  ADJ-TXT PIC X(60).
       01  APTR PIC 99.

       01  LINE-H1.
           02 FILLER PIC XX VALUE SPACE.
           02 FILLER PIC X(45) VALUE
              "REMITTANCE DETAIL FROM 835 ELECTRONIC REMIT".
           02 FILLER PIC X(8) VALUE "PAYER: ".
           02 H1-PAYER PIC X(30).
       01  LINE-H2.
           02 FILLER PIC XX VALUE SPACE.
           02 FILLER PIC X(10) VALUE "PROVIDER: ".
           02 H2-PROV PIC X(35).
           02 FILLER PIC X(6) VALUE "NPI: ".
           02 H2-NPI PIC X(12).
           02 FILLER PIC X(10) VALUE "PAYER ID: ".
           02 H2-PAYID PIC X(10).
           02 FILLER PIC X(13) VALUE "  ISA SENDER ".
           02 H2-SENDER PIC X(15).
       01  LINE-H3.
           02 FILLER PIC XX VALUE SPACE.
           02 FILLER PIC X(17) VALUE "CHECK/EFT TRACE: ".
           02 H3-CK-EFT PIC X(12).
           02 FILLER PIC X(14) VALUE "PAYMENT DATE: ".
           02 H3-PAYDATE PIC X(10).
       01  LINE-H4.
           02 FILLER PIC XX VALUE SPACE.
           02 FILLER PIC X(37) VALUE
              "PRINTED BY THE PROVIDER FROM THE 835 ".
           02 FILLER PIC X(40) VALUE
              "ERA ON FILE, NOT A PAYER-ISSUED NOTICE. ".
           02 FILLER PIC X(9) VALUE "PRINTED  ".
           02 H4-TODAY PIC X(10).
       01  LINE-COL.
           02 FILLER PIC XX VALUE SPACE.
           02 FILLER PIC X(12) VALUE "SERV DATE".
           02 FILLER PIC X(4) VALUE "POS".
           02 FILLER PIC X(15) VALUE "PROC  MODS".
           02 FILLER PIC X(10) VALUE "  BILLED".
           02 FILLER PIC X(10) VALUE " ALLOWED".
           02 FILLER PIC X(10) VALUE "  DEDUCT".
           02 FILLER PIC X(10) VALUE " PROV PD".
           02 FILLER PIC XX VALUE SPACE.
           02 FILLER PIC X(30) VALUE "ADJUSTMENTS GRP-RC AMOUNT".
       01  LINE-CLM.
           02 FILLER PIC XX VALUE SPACE.
           02 FILLER PIC X(6) VALUE "NAME: ".
           02 CL-NAME PIC X(28).
           02 FILLER PIC X(8) VALUE "MEMBER: ".
           02 CL-MEMB PIC X(21).
           02 FILLER PIC X(6) VALUE "ACNT: ".
           02 CL-GARNO PIC X(10).
           02 FILLER PIC X(13) VALUE "PAYER CLAIM: ".
           02 CL-ICN PIC X(14).
           02 CL-TB PIC X(10).
       01  LINE-FWD.
           02 FILLER PIC XX VALUE SPACE.
           02 FILLER PIC X(31) VALUE "CLAIM INFORMATION FORWARDED TO:".
           02 FILLER PIC X VALUE SPACE.
           02 FW-INSNAME PIC X(30).
       01  LINE-SVC.
           02 FILLER PIC XX VALUE SPACE.
           02 SV-DOS PIC X(12).
           02 SV-POS PIC X(4).
           02 SV-PROC PIC X(6).
           02 SV-MOD1 PIC X(3).
           02 SV-MOD2 PIC X(6).
           02 SV-BILLED PIC -----9.99.
           02 FILLER PIC X.
           02 SV-ALLOWED PIC -----9.99.
           02 FILLER PIC X.
           02 SV-DEDUCT PIC -----9.99.
           02 FILLER PIC X.
           02 SV-PAID PIC -----9.99.
           02 FILLER PIC XX VALUE SPACE.
           02 SV-ADJ PIC X(50).
       01  LINE-ADJ2.
           02 FILLER PIC X(83) VALUE SPACE.
           02 A2-ADJ PIC X(49).
       01  LINE-TOT.
           02 FILLER PIC XX VALUE SPACE.
           02 FILLER PIC X(9) VALUE "PT RESP ".
           02 TT-PR PIC -----9.99.
           02 FILLER PIC X(3) VALUE SPACE.
           02 FILLER PIC X(18) VALUE "CLAIM TOTALS".
           02 TT-BILLED PIC -----9.99.
           02 FILLER PIC X.
           02 TT-ALLOWED PIC -----9.99.
           02 FILLER PIC X.
           02 TT-DEDUCT PIC -----9.99.
           02 FILLER PIC X.
           02 TT-PAID PIC -----9.99.
       01  LINE-RMT.
           02 FILLER PIC XX VALUE SPACE.
           02 FILLER PIC X(45) VALUE
              "PROVIDER PAID ON THESE CLAIMS THIS REMIT".
           02 RT-PAID PIC ------9.99.
       01  UNDERLINE01.
           02 FILLER PIC XX VALUE SPACE.
           02 UL-1 PIC X(128).

       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT PARMFILE REQFILE CAREDETL GARFILE MBRXFILE.
           OPEN OUTPUT FILEOUT.
           MOVE ALL "_" TO UL-1
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-TODAY
           MOVE SPACE TO PV-NAME PV-NPI
           READ PARMFILE AT END GO TO P0.
           MOVE PARMFILE01 TO PV-NAME
           READ PARMFILE AT END GO TO P0.
           MOVE PARMFILE01 TO PV-NPI.
       P0.
           READ REQFILE AT END
             DISPLAY "erar301: no garno in request file"
             GO TO P99.
           MOVE REQFILE01(1:8) TO REQ-GARNO G-GARNO
           READ GARFILE INVALID
             DISPLAY "erar301: " REQ-GARNO " BAD GARNO"
             GO TO P99.
       P00.
           READ REQFILE AT END GO TO P00-X.
           IF REQFILE01(1:8) = SPACE GO TO P00.
           IF DOS-CNT < 40
             ADD 1 TO DOS-CNT
             MOVE REQFILE01(1:8) TO DOS-TAB(DOS-CNT).
           GO TO P00.
       P00-X.
           PERFORM LOAD-1 THRU LOAD-1-EXIT
           IF RW-CNT = 0
             DISPLAY "erar301: " REQ-GARNO " nothing in caredetl"
             GO TO P99.
           IF RW-OVER = 1
             DISPLAY "erar301: " REQ-GARNO " over 300 lines, give dos".
           PERFORM SORT-1
           PERFORM NAME-1

           MOVE SPACE TO CUR-REMIT CUR-ICN
           MOVE 0 TO RM-PAID
           PERFORM PRT-1 THRU PRT-1-EXIT
             VARYING X FROM 1 BY 1 UNTIL X > RW-CNT
           PERFORM CLM-TOT
           PERFORM RMT-TOT
           DISPLAY "erar301: " REQ-GARNO " lines " RW-CNT.
           GO TO P99.

       LOAD-1.
           MOVE LOW-VALUES TO DT-KEY
           MOVE REQ-GARNO TO DT-KEY8
           START CAREDETL KEY NOT < DT-KEY INVALID
             GO TO LOAD-1-EXIT.
       LOAD-1-1.
           READ CAREDETL NEXT AT END GO TO LOAD-1-EXIT.
           IF DT-KEY8 NOT = REQ-GARNO GO TO LOAD-1-EXIT.
           IF DOS-CNT > 0
             MOVE 0 TO FLAG
             PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > DOS-CNT
               IF DOS-TAB(Y) = DT-DATE MOVE 1 TO FLAG END-IF
             END-PERFORM
             IF FLAG = 0 GO TO LOAD-1-1.
           IF RW-CNT = 300
             MOVE 1 TO RW-OVER
             GO TO LOAD-1-EXIT.
           ADD 1 TO RW-CNT
           MOVE CAREDETL01 TO RW-REC(RW-CNT)
           MOVE SPACE TO RW-SORT(RW-CNT)
           STRING DT-PAYDATE DT-CK-EFT DT-ICN DT-DATE DT-PROC
             DT-MOD1 DT-MOD2 DT-SEQ DELIMITED BY SIZE
             INTO RW-SORT(RW-CNT)
           GO TO LOAD-1-1.
       LOAD-1-EXIT.
           EXIT.

      *    insertion sort on rw-sort
       SORT-1.
           PERFORM VARYING X FROM 2 BY 1 UNTIL X > RW-CNT
             MOVE RW-TAB(X) TO RW-HOLD
             MOVE X TO Y
             MOVE 0 TO FLAG
             PERFORM UNTIL Y < 2 OR FLAG = 1
               IF RW-SORT(Y - 1) > RH-SORT
                 MOVE RW-TAB(Y - 1) TO RW-TAB(Y)
                 SUBTRACT 1 FROM Y
               ELSE
                 MOVE 1 TO FLAG
               END-IF
             END-PERFORM
             MOVE RW-HOLD TO RW-TAB(Y)
           END-PERFORM.

       NAME-1.
           MOVE G-GARNAME TO VT-NAME
           IF G-PRNAME NOT = SPACE MOVE G-PRNAME TO VT-NAME.
           MOVE SPACE TO VT-LAST VT-FIRST
           UNSTRING VT-NAME DELIMITED BY ";" INTO VT-LAST VT-FIRST.

      *    member id the payer sent back, else our policy nbr
       MEMB-1.
           MOVE G-PRIPOL TO VT-ID
           MOVE REQ-GARNO TO MX-KEY8
           MOVE DT-PAYERID TO MX-PAYERID
           IF MX-PAYERID = SPACE MOVE DT-SENDER TO MX-PAYERID.
           READ MBRXFILE
             INVALID CONTINUE
             NOT INVALID MOVE MX-MEMBID TO VT-ID
           END-READ.

       PRT-1.
           MOVE RW-REC(X) TO CAREDETL01
           MOVE SPACE TO THIS-REMIT
           STRING DT-PAYDATE DT-CK-EFT DELIMITED BY SIZE
             INTO THIS-REMIT
           IF THIS-REMIT NOT = CUR-REMIT
             IF CUR-REMIT NOT = SPACE
               PERFORM CLM-TOT
               PERFORM RMT-TOT
             END-IF
             PERFORM HEAD-1
             MOVE THIS-REMIT TO CUR-REMIT
             MOVE SPACE TO CUR-ICN
             MOVE 0 TO RM-PAID
           END-IF
           IF DT-ICN NOT = CUR-ICN
             IF CUR-ICN NOT = SPACE PERFORM CLM-TOT END-IF
             PERFORM CLM-HEAD
             MOVE DT-ICN TO CUR-ICN
           END-IF
           PERFORM SVC-1.
       PRT-1-EXIT.
           EXIT.

       HEAD-1.
           MOVE DT-PRNAME TO H1-PAYER
           MOVE PV-NAME TO H2-PROV
           MOVE PV-NPI TO H2-NPI
           MOVE DT-PAYERID TO H2-PAYID
           MOVE DT-SENDER TO H2-SENDER
           MOVE DT-CK-EFT TO H3-CK-EFT
           MOVE DT-PAYDATE TO WS-YMD
           PERFORM FMT-DATE
           MOVE WS-MDY TO H3-PAYDATE
           MOVE WS-TODAY TO WS-YMD
           PERFORM FMT-DATE
           MOVE WS-MDY TO H4-TODAY
      *    form feed on its own line, no write advancing mixed in
           IF CUR-REMIT NOT = SPACE
             MOVE SPACE TO FILEOUT01
             MOVE X"0C" TO FILEOUT01(1:1)
             WRITE FILEOUT01
           END-IF
           WRITE FILEOUT01 FROM LINE-H1
           WRITE FILEOUT01 FROM LINE-H2
           WRITE FILEOUT01 FROM LINE-H3
           WRITE FILEOUT01 FROM LINE-H4
           MOVE SPACE TO FILEOUT01
           WRITE FILEOUT01
           WRITE FILEOUT01 FROM LINE-COL
           WRITE FILEOUT01 FROM UNDERLINE01.

       CLM-HEAD.
           PERFORM MEMB-1
           MOVE SPACE TO CL-NAME
           STRING VT-LAST DELIMITED BY "  " ", " DELIMITED BY SIZE
             VT-FIRST DELIMITED BY "  " INTO CL-NAME
           MOVE VT-ID TO CL-MEMB
           MOVE REQ-GARNO TO CL-GARNO
           MOVE DT-ICN TO CL-ICN
           MOVE SPACE TO CL-TB
           IF DT-TB = "T" MOVE "REVERSAL" TO CL-TB.
           MOVE DT-INSNAME TO FW-INSNAME
           MOVE SPACE TO FILEOUT01
           WRITE FILEOUT01
           WRITE FILEOUT01 FROM LINE-CLM
           MOVE 0 TO TOT-BILLED TOT-ALLOWED TOT-DEDUCT TOT-PAID
             TOT-PR.

       SVC-1.
           MOVE SPACE TO LINE-SVC
           MOVE DT-DATE TO WS-YMD
           PERFORM FMT-DATE
           MOVE WS-MDY TO SV-DOS
           MOVE DT-POS TO SV-POS
           MOVE DT-PROC TO SV-PROC
           MOVE DT-MOD1 TO SV-MOD1
           MOVE DT-MOD2 TO SV-MOD2
           MOVE DT-BILLED TO SV-BILLED
           MOVE DT-ALLOWED TO SV-ALLOWED
           MOVE DT-DEDUCT TO SV-DEDUCT
           MOVE DT-PAYED TO SV-PAID
           ADD DT-BILLED TO TOT-BILLED
           ADD DT-ALLOWED TO TOT-ALLOWED
           ADD DT-DEDUCT TO TOT-DEDUCT
           ADD DT-PAYED TO TOT-PAID RM-PAID
           MOVE SPACE TO ADJ-TXT
           MOVE 1 TO APTR
           PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > 3
             PERFORM ADJ-1
           END-PERFORM
           MOVE ADJ-TXT TO SV-ADJ
           WRITE FILEOUT01 FROM LINE-SVC
           MOVE SPACE TO ADJ-TXT
           MOVE 1 TO APTR
           PERFORM VARYING Z FROM 4 BY 1 UNTIL Z > 6
             PERFORM ADJ-1
           END-PERFORM
           IF ADJ-TXT NOT = SPACE
             MOVE ADJ-TXT TO A2-ADJ
             WRITE FILEOUT01 FROM LINE-ADJ2.

      *    one cas triplet as GRP-RC AMOUNT
       ADJ-1.
           IF DT-RC(Z) NOT = SPACE
             IF DT-GRP(Z) = "PR" ADD DT-AMT(Z) TO TOT-PR END-IF
             MOVE DT-AMT(Z) TO ADJ-ED
             MOVE ADJ-ED TO WS-ED
             PERFORM LTRIM-1
             STRING DT-GRP(Z) "-" DELIMITED BY SIZE
               DT-RC(Z) DELIMITED BY " "
               " " DELIMITED BY SIZE
               WS-ED(WS-EDP:) DELIMITED BY " "
               INTO ADJ-TXT WITH POINTER APTR
             ADD 2 TO APTR
           END-IF.

       CLM-TOT.
           IF FW-INSNAME NOT = SPACE
             WRITE FILEOUT01 FROM LINE-FWD.
           MOVE TOT-PR TO TT-PR
           MOVE TOT-BILLED TO TT-BILLED
           MOVE TOT-ALLOWED TO TT-ALLOWED
           MOVE TOT-DEDUCT TO TT-DEDUCT
           MOVE TOT-PAID TO TT-PAID
           WRITE FILEOUT01 FROM LINE-TOT.

       RMT-TOT.
           MOVE RM-PAID TO RT-PAID
           MOVE SPACE TO FILEOUT01
           WRITE FILEOUT01
           WRITE FILEOUT01 FROM LINE-RMT
           WRITE FILEOUT01 FROM UNDERLINE01.

       LTRIM-1.
           MOVE 0 TO WS-EDP
           INSPECT WS-ED TALLYING WS-EDP FOR LEADING SPACE
           ADD 1 TO WS-EDP.

       FMT-DATE.
           MOVE SPACE TO WS-MDY
           IF WS-YMD NOT = SPACE
             STRING WS-M "/" WS-D "/" WS-Y DELIMITED BY SIZE
               INTO WS-MDY.

       P99.
           CLOSE PARMFILE REQFILE CAREDETL GARFILE MBRXFILE FILEOUT
           STOP RUN.
