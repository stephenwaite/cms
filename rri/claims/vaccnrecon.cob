      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
      *    va ccn (optum) medical claim reconsideration, timely filing.
      *    writes a KEY=value field sheet for one veteran that
      *    python/vaccn_recon_fill.py lays onto the optum form.
      *    claim nbr + carc 29 from caredetl, proof of filing from
      *    the 277 history, va id from mbrxfile, referral from authfile
      * S30 parm39    the 837 parm: 1 tin, 3 contact name, 4 phone,
      *               6 npi (+ legacy), 8 name, 9-12 street city st zip
      * S35 reqfile   garno, then dos ccyymmdd lines (none = every
      *               dos with a carc 29 denial in caredetl)
      * S40 garfile S45 charcur S50 authfile S55 caredetl
      * S60 h277file S65 mbrxfile S70 field sheet out
       IDENTIFICATION DIVISION.
       PROGRAM-ID. vaccnrecon.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARMFILE ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT REQFILE ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT GARFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT CHARCUR ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT AUTHFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS AUTH-KEY
           LOCK MODE MANUAL.
           SELECT CAREDETL ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS DT-KEY
           LOCK MODE MANUAL.
           SELECT OPTIONAL H277FILE ASSIGN TO "S60"
           ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS H277-KEY
           ALTERNATE RECORD KEY IS H277-PAYID WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT OPTIONAL MBRXFILE ASSIGN TO "S65"
           ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS MX-KEY
           LOCK MODE MANUAL.
           SELECT FILEOUT ASSIGN TO "S70" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PARMFILE.
       01  PARMFILE01 PIC X(80).
       FD  REQFILE.
       01  REQFILE01 PIC X(40).
       FD  GARFILE.
           COPY garfile.cpy.
       FD  CHARCUR.
           COPY charcur.cpy.
       FD  AUTHFILE.
           COPY authfile.cpy.
       FD  CAREDETL.
           COPY caredetl.cpy.
       FD  H277FILE.
           COPY h277file.cpy.
       FD  MBRXFILE.
           COPY mbrxfile.cpy.
       FD  FILEOUT.
       01  FILEOUT01 PIC X(700).

       WORKING-STORAGE SECTION.
       01  PV-NAME PIC X(40).
       01  PV-NPI PIC X(10).
       01  PV-TIN PIC X(10).
       01  PV-STREET PIC X(40).
       01  PV-CITY PIC X(30).
       01  PV-STATE PIC XX.
       01  PV-ZIP PIC X(10).
       01  PV-CNAME PIC X(40).
       01  PV-CPHONE PIC X(20).
       01  PV-CEMAIL PIC X(60).
       01  PV-LIMIT PIC X(4).
       01  LIMIT-DAYS PIC 9(4) VALUE 180.

       01  REQ-GARNO PIC X(8).
       01  DOS-CNT PIC 99 VALUE 0.
       01  DOS-TAB01.
           02 DOS-TAB PIC X(8) OCCURS 40 TIMES.
       01  X PIC 99.
       01  Y PIC 99.
       01  FLAG PIC 9.
       01  WARN-CNT PIC 99 VALUE 0.

      *    one claim (dos) at a time
       01  CL-DOS PIC X(8).
       01  CL-ICN PIC X(13).
       01  CL-PAYDATE PIC X(8).
       01  CL-29 PIC X.
       01  CL-PAYERID PIC X(10).
       01  CL-AMT PIC S9(6)V99.
       01  CL-CLAIM PIC X(6).
       01  CL-PAYCODE PIC XXX.
       01  CL-REF PIC X(15).
       01  CL-RCV PIC X(8).
       01  CL-TRACE PIC X(30).
       01  CL-STAT PIC X(12).
       01  CL-DAYS PIC S9(5).
       01  ROW-29 PIC X.

      *    veteran, taken from the first claim
       01  VT-PAYCODE PIC XXX VALUE SPACE.
       01  VT-PAYERID PIC X(10) VALUE SPACE.
       01  VT-NAME PIC X(24).
       01  VT-POL PIC X(16).
       01  VT-LAST PIC X(24).
       01  VT-FIRST PIC X(24).
       01  VT-MI PIC X(24).
       01  VT-ID PIC X(20).
       01  MX-BEST PIC X(8).

       01  WS-TODAY PIC X(8).
       01  WS-YMD PIC X(8).
       01  WS-YMD-R REDEFINES WS-YMD.
           02 WS-Y PIC X(4).
           02 WS-M PIC XX.
           02 WS-D PIC XX.
       01  WS-MDY PIC X(10).
       01  WS-N8A PIC 9(8).
       01  WS-N8B PIC 9(8).
       01  WS-ED PIC X(14).
       01  WS-EDP PIC 99.
       01  AMT-ED PIC $$$$,$$9.99.
       01  DAYS-ED PIC ZZZZ9.
       01  LIM-ED PIC ZZZ9.
       01  CPTR PIC 999.
       01  WS-CMT PIC X(600).
       01  WS-MSG PIC X(120).

       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT PARMFILE REQFILE GARFILE CHARCUR AUTHFILE
             CAREDETL H277FILE MBRXFILE.
           OPEN OUTPUT FILEOUT.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-TODAY
           PERFORM PARM-1 THRU PARM-1-EXIT.

           READ REQFILE AT END
             DISPLAY "vaccnrecon: no garno in request file"
             GO TO P99.
           MOVE REQFILE01(1:8) TO REQ-GARNO G-GARNO
           READ GARFILE INVALID
             DISPLAY "vaccnrecon: " REQ-GARNO " BAD GARNO"
             GO TO P99.

       P00.
           READ REQFILE AT END GO TO P00-X.
           IF REQFILE01(1:8) = SPACE GO TO P00.
           IF DOS-CNT < 40
             ADD 1 TO DOS-CNT
             MOVE REQFILE01(1:8) TO DOS-TAB(DOS-CNT).
           GO TO P00.
       P00-X.
           IF DOS-CNT = 0 PERFORM FIND-29 THRU FIND-29-EXIT.
           IF DOS-CNT = 0
             DISPLAY "vaccnrecon: " REQ-GARNO
               " no carc 29 denials in caredetl, give dos lines"
             GO TO P99.

           PERFORM HEAD-1.
           PERFORM CLM-1 THRU CLM-1-EXIT
             VARYING Y FROM 1 BY 1 UNTIL Y > DOS-CNT.
           PERFORM VET-1.

           MOVE SPACE TO FILEOUT01
           STRING "COMMENT=The remittance detail from the 835 is"
             " attached. Please reprocess for payment."
             DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           DISPLAY "vaccnrecon: " REQ-GARNO " claims " DOS-CNT
             " warnings " WARN-CNT.
           GO TO P99.

      *    provider block, same order every run
       PARM-1.
           MOVE SPACE TO PV-NAME PV-NPI PV-TIN PV-STREET PV-CITY
             PV-STATE PV-ZIP PV-CNAME PV-CPHONE PV-CEMAIL PV-LIMIT
           MOVE 0 TO X.
       PARM-1-1.
           READ PARMFILE AT END GO TO PARM-1-EXIT.
           ADD 1 TO X
           EVALUATE X
             WHEN 1 MOVE PARMFILE01 TO PV-TIN
             WHEN 3 MOVE PARMFILE01 TO PV-CNAME
             WHEN 4 MOVE PARMFILE01 TO PV-CPHONE
             WHEN 6 UNSTRING PARMFILE01 DELIMITED BY " " INTO PV-NPI
             WHEN 8 MOVE PARMFILE01 TO PV-NAME
             WHEN 9 MOVE PARMFILE01 TO PV-STREET
             WHEN 10 MOVE PARMFILE01 TO PV-CITY
             WHEN 11 MOVE PARMFILE01 TO PV-STATE
             WHEN 12 MOVE PARMFILE01 TO PV-ZIP
           END-EVALUATE
           IF X < 12 GO TO PARM-1-1.
       PARM-1-EXIT.
           EXIT.

      *    no dos given: every dos on caredetl with a carc 29
       FIND-29.
           MOVE LOW-VALUES TO DT-KEY
           MOVE REQ-GARNO TO DT-KEY8
           START CAREDETL KEY NOT < DT-KEY INVALID
             GO TO FIND-29-EXIT.
       FIND-29-1.
           READ CAREDETL NEXT AT END GO TO FIND-29-EXIT.
           IF DT-KEY8 NOT = REQ-GARNO GO TO FIND-29-EXIT.
           PERFORM CHK-29
           IF ROW-29 NOT = "Y" GO TO FIND-29-1.
           MOVE 0 TO FLAG
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > DOS-CNT
             IF DOS-TAB(X) = DT-DATE MOVE 1 TO FLAG END-IF
           END-PERFORM
           IF FLAG = 0 AND DOS-CNT < 40
             ADD 1 TO DOS-CNT
             MOVE DT-DATE TO DOS-TAB(DOS-CNT).
           GO TO FIND-29-1.
       FIND-29-EXIT.
           EXIT.

       CHK-29.
           MOVE "N" TO ROW-29
           IF DT-PAYDENIAL = "29" MOVE "Y" TO ROW-29.
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 6
             IF DT-RC(X) = "29" MOVE "Y" TO ROW-29 END-IF
           END-PERFORM.

       HEAD-1.
           MOVE WS-TODAY TO WS-YMD
           PERFORM FMT-DATE
           MOVE "FORM=VACCN-MED-RECON" TO FILEOUT01
           WRITE FILEOUT01
           MOVE SPACE TO FILEOUT01
           STRING "GARNO=" REQ-GARNO DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           MOVE SPACE TO FILEOUT01
           STRING "DATE=" WS-MDY DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           MOVE SPACE TO FILEOUT01
           STRING "PROV_NAME=" PV-NAME DELIMITED BY SIZE
             INTO FILEOUT01
           WRITE FILEOUT01
           MOVE SPACE TO FILEOUT01
           STRING "NPI=" PV-NPI DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           MOVE SPACE TO FILEOUT01
           STRING "TIN=" PV-TIN DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           MOVE SPACE TO FILEOUT01
           STRING "BILL_STREET=" PV-STREET DELIMITED BY SIZE
             INTO FILEOUT01
           WRITE FILEOUT01
           MOVE SPACE TO FILEOUT01
           STRING "BILL_CITY=" PV-CITY DELIMITED BY SIZE
             INTO FILEOUT01
           WRITE FILEOUT01
           MOVE SPACE TO FILEOUT01
           STRING "BILL_STATE=" PV-STATE DELIMITED BY SIZE
             INTO FILEOUT01
           WRITE FILEOUT01
           MOVE SPACE TO FILEOUT01
           STRING "BILL_ZIP=" PV-ZIP DELIMITED BY SIZE
             INTO FILEOUT01
           WRITE FILEOUT01
           MOVE SPACE TO FILEOUT01
           STRING "CONTACT_NAME=" PV-CNAME DELIMITED BY SIZE
             INTO FILEOUT01
           WRITE FILEOUT01
           MOVE SPACE TO FILEOUT01
           STRING "CONTACT_PHONE=" PV-CPHONE DELIMITED BY SIZE
             INTO FILEOUT01
           WRITE FILEOUT01
           MOVE SPACE TO FILEOUT01
           STRING "CONTACT_EMAIL=" PV-CEMAIL DELIMITED BY SIZE
             INTO FILEOUT01
           WRITE FILEOUT01
           MOVE "CONTACT_SAME=Y" TO FILEOUT01
           WRITE FILEOUT01
           MOVE "REASON=TIMELY" TO FILEOUT01
           WRITE FILEOUT01.

      *    one dos: gather, then write CLAIM and COMMENT lines
       CLM-1.
           MOVE DOS-TAB(Y) TO CL-DOS
           MOVE SPACE TO CL-ICN CL-PAYDATE CL-PAYERID CL-CLAIM
             CL-PAYCODE CL-REF CL-RCV CL-TRACE CL-STAT
           MOVE "N" TO CL-29
           MOVE 0 TO CL-AMT CL-DAYS
           PERFORM DETL-1 THRU DETL-1-EXIT
           PERFORM CHAR-1 THRU CHAR-1-EXIT
           PERFORM AUTH-1
           PERFORM H277-1 THRU H277-1-EXIT

           IF VT-PAYCODE = SPACE MOVE CL-PAYCODE TO VT-PAYCODE.
           IF VT-PAYERID = SPACE MOVE CL-PAYERID TO VT-PAYERID.

           IF CL-ICN = SPACE
             MOVE "no 835 claim number in caredetl" TO WS-MSG
             PERFORM WARN-1
           ELSE IF CL-29 NOT = "Y"
             MOVE "835 has no carc 29 timely denial" TO WS-MSG
             PERFORM WARN-1.
           IF CL-REF = SPACE
             MOVE "no va referral in authfile" TO WS-MSG
             PERFORM WARN-1.
           IF CL-RCV = SPACE
             MOVE "no 277CA acceptance in 277 history" TO WS-MSG
             PERFORM WARN-1
           ELSE
             MOVE CL-RCV TO WS-N8A
             MOVE CL-DOS TO WS-N8B
             COMPUTE CL-DAYS = FUNCTION INTEGER-OF-DATE(WS-N8A)
                             - FUNCTION INTEGER-OF-DATE(WS-N8B)
             IF CL-DAYS > LIMIT-DAYS
               MOVE "277CA received after the timely limit" TO WS-MSG
               PERFORM WARN-1.

           MOVE CL-DOS TO WS-YMD
           PERFORM FMT-DATE
           MOVE SPACE TO FILEOUT01
           STRING "CLAIM=" CL-ICN "|" WS-MDY "|" CL-REF
             DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           PERFORM CMT-1 THRU CMT-1-EXIT
           MOVE SPACE TO FILEOUT01
           STRING "COMMENT=" WS-CMT DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01.
       CLM-1-EXIT.
           EXIT.

      *    caredetl: prefer a carc 29 row, latest remit
       DETL-1.
           MOVE LOW-VALUES TO DT-KEY
           MOVE REQ-GARNO TO DT-KEY8
           MOVE CL-DOS TO DT-DATE
           START CAREDETL KEY NOT < DT-KEY INVALID
             GO TO DETL-1-EXIT.
       DETL-1-1.
           READ CAREDETL NEXT AT END GO TO DETL-1-EXIT.
           IF DT-KEY8 NOT = REQ-GARNO OR DT-DATE NOT = CL-DOS
             GO TO DETL-1-EXIT.
           PERFORM CHK-29
           IF ROW-29 = "N" AND CL-29 = "Y" GO TO DETL-1-1.
           IF ROW-29 = CL-29 AND DT-PAYDATE < CL-PAYDATE
             GO TO DETL-1-1.
           MOVE ROW-29 TO CL-29
           MOVE DT-ICN TO CL-ICN
           MOVE DT-PAYDATE TO CL-PAYDATE
           MOVE DT-PAYERID TO CL-PAYERID
           GO TO DETL-1-1.
       DETL-1-EXIT.
           EXIT.

       CHAR-1.
           MOVE REQ-GARNO TO CC-KEY8
           MOVE LOW-VALUES TO CC-KEY3
           START CHARCUR KEY NOT < CHARCUR-KEY INVALID
             GO TO CHAR-1-EXIT.
       CHAR-1-1.
           READ CHARCUR NEXT AT END GO TO CHAR-1-EXIT.
           IF CC-KEY8 NOT = REQ-GARNO GO TO CHAR-1-EXIT.
           IF CC-DATE-T NOT = CL-DOS GO TO CHAR-1-1.
           ADD CC-AMOUNT TO CL-AMT
           IF CL-CLAIM = SPACE
             MOVE CC-CLAIM TO CL-CLAIM
             MOVE CC-PAYCODE TO CL-PAYCODE.
           GO TO CHAR-1-1.
       CHAR-1-EXIT.
           EXIT.

       AUTH-1.
           IF CL-CLAIM NOT = SPACE
             MOVE REQ-GARNO TO AUTH-KEY8
             MOVE CL-CLAIM TO AUTH-KEY6
             READ AUTHFILE
               INVALID CONTINUE
               NOT INVALID MOVE AUTH-NUM TO CL-REF
             END-READ
           END-IF.

      *    earliest accepted 277CA (a1/a2, wq) for this dos from va ccn
      *    optum only - h277file also holds the medicare (ngs) 277s
       H277-1.
           MOVE LOW-VALUES TO H277-KEY
           MOVE REQ-GARNO TO H277-KEY8
           START H277FILE KEY NOT < H277-KEY INVALID
             GO TO H277-1-EXIT.
       H277-1-1.
           READ H277FILE NEXT AT END GO TO H277-1-EXIT.
           IF H277-KEY8 NOT = REQ-GARNO GO TO H277-1-EXIT.
           IF H277-DOS NOT = CL-DOS GO TO H277-1-1.
           IF H277-PAYID NOT = "VACCN" GO TO H277-1-1.
           IF NOT (H277-STAT(1:3) = "A1:" OR "A2:") GO TO H277-1-1.
           IF H277-ACTION NOT = "WQ" GO TO H277-1-1.
           IF CL-RCV NOT = SPACE AND H277-RECDATE NOT < CL-RCV
             GO TO H277-1-1.
           MOVE H277-RECDATE TO CL-RCV
           MOVE H277-TRACE TO CL-TRACE
           MOVE H277-STAT TO CL-STAT
           GO TO H277-1-1.
       H277-1-EXIT.
           EXIT.

      *    veteran name/id: insured on the va coverage, va id from
      *    the 835 when we have it (g-pripol is only 16)
       VET-1.
           MOVE G-GARNAME TO VT-NAME
           MOVE G-PRIPOL TO VT-POL
           IF VT-PAYCODE = G-PRINS AND G-PRNAME NOT = SPACE
             MOVE G-PRNAME TO VT-NAME.
           IF VT-PAYCODE = G-SEINS AND VT-PAYCODE NOT = G-PRINS
             MOVE G-SECPOL TO VT-POL
             IF G-SENAME NOT = SPACE MOVE G-SENAME TO VT-NAME.
           MOVE SPACE TO VT-LAST VT-FIRST VT-MI
           UNSTRING VT-NAME DELIMITED BY ";" INTO
             VT-LAST VT-FIRST VT-MI
           MOVE VT-POL TO VT-ID
           PERFORM MBRX-1 THRU MBRX-1-EXIT
           MOVE SPACE TO FILEOUT01
           STRING "VET_LAST=" VT-LAST DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           MOVE SPACE TO FILEOUT01
           STRING "VET_FIRST=" VT-FIRST DELIMITED BY SIZE
             INTO FILEOUT01
           WRITE FILEOUT01
           MOVE SPACE TO FILEOUT01
           STRING "VET_ID=" VT-ID DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01.

       MBRX-1.
           MOVE SPACE TO MX-BEST
           MOVE LOW-VALUES TO MX-KEY
           MOVE REQ-GARNO TO MX-KEY8
           START MBRXFILE KEY NOT < MX-KEY INVALID
             GO TO MBRX-1-EXIT.
       MBRX-1-1.
           READ MBRXFILE NEXT AT END GO TO MBRX-1-EXIT.
           IF MX-KEY8 NOT = REQ-GARNO GO TO MBRX-1-EXIT.
           IF MX-PAYERID = VT-PAYERID AND VT-PAYERID NOT = SPACE
             MOVE MX-MEMBID TO VT-ID
             GO TO MBRX-1-EXIT.
           IF MX-BEST = SPACE OR MX-DATE > MX-BEST
             MOVE MX-DATE TO MX-BEST
             MOVE MX-MEMBID TO VT-ID.
           GO TO MBRX-1-1.
       MBRX-1-EXIT.
           EXIT.

       CMT-1.
           MOVE SPACE TO WS-CMT
           MOVE 1 TO CPTR
           MOVE CL-DOS TO WS-YMD
           PERFORM FMT-DATE
           STRING "DOS " WS-MDY DELIMITED BY SIZE
             INTO WS-CMT WITH POINTER CPTR
           IF CL-ICN NOT = SPACE
             STRING ", claim " CL-ICN DELIMITED BY "  "
               INTO WS-CMT WITH POINTER CPTR.
           IF CL-AMT NOT = 0
             MOVE CL-AMT TO AMT-ED
             MOVE AMT-ED TO WS-ED
             PERFORM LTRIM-1
             STRING ", billed " WS-ED(WS-EDP:) DELIMITED BY "  "
               INTO WS-CMT WITH POINTER CPTR.
           IF CL-29 = "Y"
             MOVE CL-PAYDATE TO WS-YMD
             PERFORM FMT-DATE
             STRING ": denied for timely filing (CARC 29) on the"
               " remittance dated " WS-MDY ". "
               DELIMITED BY SIZE INTO WS-CMT WITH POINTER CPTR
           ELSE
             STRING ": denied for timely filing. "
               DELIMITED BY SIZE INTO WS-CMT WITH POINTER CPTR.
           IF CL-RCV = SPACE GO TO CMT-1-EXIT.
           MOVE CL-RCV TO WS-YMD
           PERFORM FMT-DATE
           STRING "VA CCN Optum received this claim " WS-MDY
             " and accepted it for adjudication (277CA "
             DELIMITED BY SIZE INTO WS-CMT WITH POINTER CPTR
           STRING CL-STAT DELIMITED BY " "
             ", trace " DELIMITED BY SIZE
             CL-TRACE DELIMITED BY " "
             "), " DELIMITED BY SIZE
             INTO WS-CMT WITH POINTER CPTR
           MOVE CL-DAYS TO DAYS-ED
           MOVE DAYS-ED TO WS-ED
           PERFORM LTRIM-1
           STRING WS-ED(WS-EDP:) DELIMITED BY " "
             " days after the date of service" DELIMITED BY SIZE
             INTO WS-CMT WITH POINTER CPTR
           IF CL-DAYS NOT > LIMIT-DAYS
             MOVE LIMIT-DAYS TO LIM-ED
             MOVE LIM-ED TO WS-ED
             PERFORM LTRIM-1
             STRING ", within the " DELIMITED BY SIZE
               WS-ED(WS-EDP:) DELIMITED BY " "
               "-day timely filing limit" DELIMITED BY SIZE
               INTO WS-CMT WITH POINTER CPTR.
           STRING "." DELIMITED BY SIZE INTO WS-CMT WITH POINTER CPTR.
       CMT-1-EXIT.
           EXIT.

       LTRIM-1.
           MOVE 0 TO WS-EDP
           INSPECT WS-ED TALLYING WS-EDP FOR LEADING SPACE
           ADD 1 TO WS-EDP.

       FMT-DATE.
           MOVE SPACE TO WS-MDY
           IF WS-YMD NOT = SPACE
             STRING WS-M "/" WS-D "/" WS-Y DELIMITED BY SIZE
               INTO WS-MDY.

       WARN-1.
           ADD 1 TO WARN-CNT
           MOVE SPACE TO FILEOUT01
           STRING "WARN=" CL-DOS " " WS-MSG DELIMITED BY SIZE
             INTO FILEOUT01
           WRITE FILEOUT01
           DISPLAY "vaccnrecon: " REQ-GARNO " " CL-DOS " " WS-MSG.

       P99.
           CLOSE PARMFILE REQFILE GARFILE CHARCUR AUTHFILE
             CAREDETL H277FILE MBRXFILE FILEOUT
           STOP RUN.
