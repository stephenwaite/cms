       IDENTIFICATION DIVISION.
       PROGRAM-ID. mea003.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARNEW ASSIGN TO "S30"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CHARNEW-KEY
               LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO "S35"
               ORGANIZATION LINE SEQUENTIAL.

           SELECT GARFILE ASSIGN TO "S40"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS G-GARNO
               ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
               LOCK MODE MANUAL.

       DATA DIVISION.

       FILE SECTION.

       FD  FILEOUT.
       01  FILEOUT01 PIC X(70).

       FD  CHARNEW.
           COPY "charnew.cpy".

       FD  GARFILE.
           COPY "garfile.cpy".

       WORKING-STORAGE SECTION.

       01  HOLD8        PIC X(8).
       01  WS-MEAS-PFX  PIC X(12).

       01  SVC-DATE.
           02  SVC-YYYY PIC 9999.
           02  SVC-MMDD PIC 9999.

       01  G-DATE.
           02  G-YYYY   PIC 9999.
           02  G-MMDD   PIC 9999.

       01  G-AGE        PIC 999.

      *----------------------------------------------------------------
      *  CPT sets defined as 88-levels on a single working field.
      *  WS-CPT is loaded from CD-CPT at the top of P1 before any test.
      *
      *  Paycode / Measure mapping:
      *    009  QMM26        pure – AAA screening
      *    010  364 only     pure – pulmonary nodule F/U (age >= 35)
      *    012  405 only     pure – paycode-003 records
      *    013  406 only     pure – paycode-003 records
      *    014  364 + 405    hybrid (age gate for 364 portion)
      *    015  364 + 406    hybrid (age gate for 364 portion)
      *    016  405 + 406    hybrid – paycode-003 records
      *    017  364+405+406  hybrid (age gate for 364 portion)
      *----------------------------------------------------------------

       01  WS-CPT PIC X(5).

      *--- QMM26 / Paycode 009 ---
           88  CPT-QMM26        VALUE "76706".

      *--- Pure 364 (no overlap with 405 or 406) ---
           88  CPT-364-ONLY
                   VALUE "75571" "75572" "75573" "75574"
                         "72128" "72129" "72130"
                         "74174" "74175".

      *--- Pure 405 (no overlap with 364 or 406) ---
           88  CPT-405-ONLY
                   VALUE "72131" "72191" "72192" "72193" "72194"
                         "72195" "72196" "72197" "72198"
                         "74181" "74182" "74183".

      *--- Pure 406 (no overlap with 364 or 405) ---
           88  CPT-406-ONLY
                   VALUE "70471" "70486" "70487" "70488"
                         "70540" "70542" "70543"
                         "72141" "72142" "72156"
                         "71550" "71551" "71552".

      *--- Hybrid 364 + 405  (paycode 014) ---
           88  CPT-364-405
                   VALUE "71275"
                         "74150" "74160" "74170"
                         "74176" "74177" "74178".

      *--- Hybrid 364 + 406  (paycode 015) ---
           88  CPT-364-406
                   VALUE "70490" "70491" "70492" "70498"
                         "72125" "72126" "72127".

      *--- Hybrid 405 + 406  (paycode 016) ---
           88  CPT-405-406
                   VALUE "71271" "71555".

      *--- Hybrid 364 + 405 + 406  (paycode 017) ---
           88  CPT-364-405-406
                   VALUE "71250" "71260" "71270".

       PROCEDURE DIVISION.

       0005-START.
           OPEN I-O   CHARNEW
           OPEN OUTPUT FILEOUT
           OPEN INPUT  GARFILE
           MOVE SPACE TO CHARNEW-KEY.

      *----------------------------------------------------------------
       P1.
           READ CHARNEW NEXT WITH LOCK
               AT END GO TO P2
           END-READ

           MOVE CD-CPT TO WS-CPT

      *--- QMM26 / Paycode 009 -----------------------------------------
      *    ACR registry measure – above paycode-003 check intentionally
      *    Patient must be age 50 or older
           IF CPT-QMM26
               PERFORM CHECK-AGE
               IF G-AGE < 50
                   GO TO P1
               END-IF
               MOVE "009"   TO CD-PAYCODE
               MOVE "QMM26" TO WS-MEAS-PFX
               PERFORM WRITE-RECORD
               GO TO P1
           END-IF

      *--- Pure 364 / Paycode 010 --------------------------------------
      *    ACR registry measure – above paycode-003 check intentionally
      *    Patient must be age 35 or older
           IF CPT-364-ONLY
               PERFORM CHECK-AGE
               IF G-AGE < 35
                   GO TO P1
               END-IF
               MOVE "010" TO CD-PAYCODE
               MOVE "364" TO WS-MEAS-PFX
               PERFORM WRITE-RECORD
               GO TO P1
           END-IF

      *--- Hybrid 364+405 / Paycode 014 or 012 -------------------------
      *    If age < 35: 364 portion does not qualify.
      *      If also paycode 003 -> write as pure 405 (012)
      *      Otherwise skip entirely
      *    If age >= 35 and paycode 003 -> write as hybrid 364+405 (014)
      *    If age >= 35 and paycode != 003 -> write as pure 364 (010)
           IF CPT-364-405
               PERFORM CHECK-AGE
               IF G-AGE < 35
                   IF CD-PAYCODE = "003"
                       MOVE "012"     TO CD-PAYCODE
                       MOVE "405"     TO WS-MEAS-PFX
                       PERFORM WRITE-RECORD
                   END-IF
                   GO TO P1
               END-IF
               IF CD-PAYCODE = "003"
                   MOVE "014"     TO CD-PAYCODE
                   MOVE "364/405" TO WS-MEAS-PFX
               ELSE
                   MOVE "010"     TO CD-PAYCODE
                   MOVE "364"     TO WS-MEAS-PFX
               END-IF
               PERFORM WRITE-RECORD
               GO TO P1
           END-IF

      *--- Hybrid 364+406 / Paycode 015 or 013 -------------------------
      *    Same age-gate logic as 364+405 above
           IF CPT-364-406
               PERFORM CHECK-AGE
               IF G-AGE < 35
                   IF CD-PAYCODE = "003"
                       MOVE "013"     TO CD-PAYCODE
                       MOVE "406"     TO WS-MEAS-PFX
                       PERFORM WRITE-RECORD
                   END-IF
                   GO TO P1
               END-IF
               IF CD-PAYCODE = "003"
                   MOVE "015"     TO CD-PAYCODE
                   MOVE "364/406" TO WS-MEAS-PFX
               ELSE
                   MOVE "010"     TO CD-PAYCODE
                   MOVE "364"     TO WS-MEAS-PFX
               END-IF
               PERFORM WRITE-RECORD
               GO TO P1
           END-IF

      *--- Hybrid 364+405+406 / Paycode 017, 016, or 010 ---------------
      *    If age < 35: 364 portion does not qualify.
      *      If also paycode 003 -> write as pure 405+406 hybrid (016)
      *      Otherwise skip
      *    If age >= 35 and paycode 003 -> write as full hybrid (017)
      *    If age >= 35 and paycode != 003 -> write as pure 364 (010)
           IF CPT-364-405-406
               PERFORM CHECK-AGE
               IF G-AGE < 35
                   IF CD-PAYCODE = "003"
                       MOVE "016"     TO CD-PAYCODE
                       MOVE "405/406" TO WS-MEAS-PFX
                       PERFORM WRITE-RECORD
                   END-IF
                   GO TO P1
               END-IF
               IF CD-PAYCODE = "003"
                   MOVE "017"         TO CD-PAYCODE
                   MOVE "364/405/406" TO WS-MEAS-PFX
               ELSE
                   MOVE "010"         TO CD-PAYCODE
                   MOVE "364"         TO WS-MEAS-PFX
               END-IF
               PERFORM WRITE-RECORD
               GO TO P1
           END-IF

      *--- Below this point only paycode 003 records qualify -----------
           IF CD-PAYCODE NOT = "003"
               GO TO P1
           END-IF

      *--- Pure 405 / Paycode 012 ---------------------------------------
           IF CPT-405-ONLY
               MOVE "012" TO CD-PAYCODE
               MOVE "405" TO WS-MEAS-PFX
               PERFORM WRITE-RECORD
               GO TO P1
           END-IF

      *--- Pure 406 / Paycode 013 ---------------------------------------
           IF CPT-406-ONLY
               MOVE "013" TO CD-PAYCODE
               MOVE "406" TO WS-MEAS-PFX
               PERFORM WRITE-RECORD
               GO TO P1
           END-IF

      *--- Hybrid 405+406 / Paycode 016 --------------------------------
           IF CPT-405-406
               MOVE "016"     TO CD-PAYCODE
               MOVE "405/406" TO WS-MEAS-PFX
               PERFORM WRITE-RECORD
               GO TO P1
           END-IF

           GO TO P1.

      *----------------------------------------------------------------
       CHECK-AGE.
           MOVE CD-KEY8 TO G-GARNO
           READ GARFILE INVALID
               DISPLAY "GARNO NOT AVAILABLE FOR SOME UNKNOWN REASON"
               DISPLAY "PLEASE RECORD THIS FACT " CD-KEY8
               GO TO P1
           END-READ
           MOVE G-DOB    TO G-DATE
           MOVE CD-DATE-T TO SVC-DATE
           COMPUTE G-AGE = SVC-YYYY - G-YYYY.

      *----------------------------------------------------------------
       WRITE-RECORD.
           MOVE SPACE TO FILEOUT01
           STRING WS-MEAS-PFX " " CD-PAYCODE " " CD-CPT " "
                  CD-DATE-T " " CD-KEY8 " " CD-NAME
                  DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           REWRITE CHARNEW01.

      *----------------------------------------------------------------
       P2.
           CLOSE CHARNEW
           CLOSE FILEOUT
           CLOSE GARFILE.
           STOP RUN.