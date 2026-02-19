      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE
      *          GNU General Public License 3
      *
      *  Paycode / Measure mapping:
      *    009  QMM26        pure  AAA screening (age >= 50)
      *    010  364 only     pure  pulmonary nodule F/U (age >= 35)
      *    012  405 only     pure  paycode-003 records
      *    013  406 only     pure  paycode-003 records
      *    014  364 + 405    hybrid (364 age gate applies)
      *    015  364 + 406    hybrid (364 age gate applies)
      *    016  405 + 406    hybrid paycode-003 records
      *    017  364+405+406  hybrid (364 age gate applies)
      *
      *  NOTE: Hybrid paycodes 014-017 require two or three quality
      *  responses per record. This program uses CD-QP1 for the first
      *  measure, CD-QP2 for the second, and CD-QP3 for the third.
      *  Verify that charnew.cpy exposes CD-QP2 and CD-QP3.
      *----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. xxx003c.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT TAGDIAG ASSIGN TO "S25"
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS TAG-KEY
               ALTERNATE RECORD KEY IS TAG-ICD9 WITH DUPLICATES
               LOCK MODE MANUAL.

           SELECT CHARNEW ASSIGN TO "S30"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CHARNEW-KEY
               LOCK MODE MANUAL STATUS IS CHARNEW-STAT.

           SELECT FILE-OUT ASSIGN TO "S35"
               ORGANIZATION LINE SEQUENTIAL.

           SELECT ALLOWFILE ASSIGN TO "S40"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ALW-KEY.

           SELECT DIAGFILE ASSIGN TO "S45"
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS DIAG-KEY
               ALTERNATE RECORD KEY IS DIAG-TITLE WITH DUPLICATES.

           SELECT PROCFILE ASSIGN TO "S50"
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS PROC-KEY.

           SELECT GARFILE ASSIGN TO "S55"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS G-GARNO
               ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
               LOCK MODE MANUAL.

           SELECT DIAG9FILE ASSIGN TO "S60"
               ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC
               RECORD KEY IS DIAG9-KEY
               ALTERNATE RECORD KEY IS DIAG9-TITLE WITH DUPLICATES.

           SELECT OUTFILE ASSIGN TO "S65"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT FILEOUT2 ASSIGN TO "S70"
               ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  TAGDIAG.
       01  TAGDIAG01.
           02 TAG-KEY.
               03 TAG-7     PIC X(7).
               03 TAG-5     PIC X(5).
           02 TAG-ICD9.
               03 tag-icd9-5 PIC X(5).
               03 tag-icd9-7 PIC X(7).

       FD  GARFILE.
           COPY garfile.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD ALLOWFILE
           DATA RECORD ALLOWFILE01.
       01 ALLOWFILE01.
           02 ALW-KEY.
               03 ALW-PROC  PIC X(7).
               03 ALW-DIAG  PIC X(7).
           02 ALW-FLAG       PIC X.

       FD  PROCFILE.
           COPY procfile.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  DIAGFILE
           BLOCK CONTAINS 8 RECORDS
           DATA RECORD IS DIAG01.
       01  DIAG01.
           02 DIAG-KEY       PIC X(7).
           02 DIAG-TITLE.
               03 DIAG-T1    PIC XXX.
               03 DIAG-T2    PIC X(58).
           02 DIAG-MEDB      PIC X(5).

       FD  DIAG9FILE.
       01  DIAG901.
           02 DIAG9-KEY      PIC X(5).
           02 DIAG9-TITLE    PIC X(25).
           02 DIAG9-MEDB     PIC X(5).

       FD  CHARNEW.
           COPY charnew.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  FILE-OUT.
       01  FILE-OUT01.
           02 FO-DATE        PIC X(8).
           02 FILLER         PIC X VALUE SPACE.
           02 FO-NAME        PIC X(19).
           02 FILLER         PIC X VALUE SPACE.
           02 FO-PAYCODE     PIC X(3).
           02 FILLER         PIC X VALUE SPACE.
           02 FO-PROC        PIC X(11).
           02 FILLER         PIC X VALUE SPACE.
           02 FO-MODS        PIC X(8).
           02 FILLER         PIC X VALUE SPACE.
           02 FO-TITLE       PIC X(17).
           02 FILLER         PIC X VALUE SPACE.
           02 FO-REFPHY      PIC X(10).
           02 FILLER         PIC X.
           02 FO-KEY         PIC X(11).

       FD  OUTFILE.
       01  OUTFILE01         PIC X(132).

       FD  FILEOUT2.
       01  FILEOUT201        PIC X(40).

       WORKING-STORAGE SECTION.

       01  BELL0             USAGE INDEX.
       01  HOLD8             PIC X(8) VALUE SPACE.
       01  HOLD7             PIC X(7).
       01  XX                PIC 9 VALUE 0.

       01  IN-FIELD.
           04 IN-FIELD-10.
               05 IN-FIELD-9.
                   06 IN-FIELD-8.
                       07 IN-FIELD-7.
                           08 IN-FIELD-6.
                               09 IN-FIELD-5.
                                   10 IN-FIELD-4.
                                       11 IN-FIELD-3.
                                           12 IN-FIELD-2.
                                               13 IN-FIELD-1 PIC X.
                                               13 FILLER     PIC X.
                                           12 FILLER         PIC X.
                                       11 FILLER             PIC X.
                                   10 FILLER                 PIC X.
                               09 FILLER                     PIC X.
                           08 FILLER                         PIC X.
                       07 FILLER                             PIC X.
                   06 FILLER                                 PIC X.
               05 FILLER                                     PIC X.
           04 FILLER                                         PIC X(5).

       01 IN-FIELD-TAB01 REDEFINES IN-FIELD.
           02 IN-FIELD-TAB   PIC X OCCURS 15 TIMES.

       01  X                 PIC 99.
       01  ANS.
           02 ANS1           PIC X.
           02 FILLER         PIC XX.
       01  FLAG              PIC 9 VALUE 0.
       01  ALF1              PIC X.
       01  ALF-2             PIC XX.
       01  RIGHT-2           PIC XX JUST RIGHT.
       01  NEF2              PIC Z9.
       01  ALF-5             PIC X(5).
       01  ALF-7             PIC X(7).
       01  ALF8              PIC X(8).
       01  ALF6              PIC X(6).
       01  DIAG2             PIC X.
       01  DIAG-FLAG         PIC 9.
       01  TAGTAB01.
           02 TAGTAB         PIC X(7) OCCURS 20 TIMES.
       01  Y                 PIC 99.
       01  NUM-2             PIC 99.
       01  HOLD-DIAG         PIC X(7).
       01  HOLD-DOCP         PIC X(2).
       01  CHARNEW-BACK      PIC X(253).
       01  CHARNEW-STAT.
           02 CHARNEW-STAT1  PIC X.
           02 CHARNEW-STAT2  PIC X.

       PROCEDURE DIVISION.

      *----------------------------------------------------------------
       P0.
           SET BELL0 TO 7.
           OPEN INPUT DIAGFILE FILE-OUT PROCFILE ALLOWFILE GARFILE
                               TAGDIAG DIAG9FILE.
           OPEN EXTEND OUTFILE.
           OPEN INPUT CHARNEW.
           OPEN OUTPUT FILEOUT2.
           DISPLAY "0 = start new, 1 = skip ahead to undone"
           ACCEPT ALF1.

      *----------------------------------------------------------------
       P1.
           READ FILE-OUT AT END
               GO TO P99
           END-READ

           MOVE FO-KEY TO CHARNEW-KEY
           READ CHARNEW INVALID
               DISPLAY "CHARGE RECORD NOT AVAILABLE FOR SOME UNKNOWN "
                       "REASON " FO-KEY
               GO TO P1
           END-READ

      *    skip to undone if coded and prompted
           IF CD-DIAG NOT = "0000000" AND ALF1 = "1"
               GO TO P1
           END-IF

      *    skip unread which are handled in P1-0
           IF CD-DOCP = "02"
               GO TO P1
           END-IF.

      *----------------------------------------------------------------
       P1-0.
           PERFORM P2
           DISPLAY "MRN " G-ACCT " DOS " FO-DATE " " G-GARNO
                   " CHARGE KEY " FO-PROC

      *    auto-DOC and auto-code G1004 for AUC program
           IF CD-PROC2 = "G1004  "
               IF FO-KEY(1:8) = G-GARNO
                   MOVE HOLD-DOCP TO CD-DOCP
                   IF HOLD7 NOT = "0000000"
                       MOVE HOLD7 TO CD-DIAG
                   END-IF
               ELSE
                   MOVE "02"      TO CD-DOCP
                   MOVE "0000000" TO CD-DIAG
               END-IF
               DISPLAY "Autocoded G1004 with " CD-DIAG
               DISPLAY " "
               PERFORM RE-WRITE-CHARNEW THRU RE-WRITE-CHARNEW-EXIT
               GO TO P1
           END-IF

           IF CD-DOCP = "00"
               PERFORM 10-GR
               DISPLAY "RRMC tape says study not read, is it read now?"
               DISPLAY "If so enter doc ## or 02 to leave unread."
               DISPLAY "Type ? for our radiologist doc ##s ie 06"
               ACCEPT CD-DOCP
               IF (CD-DOCP = "? ")
                   PERFORM LIST-DOC
                   MOVE "00" TO CD-DOCP
                   GO TO P1-0
               END-IF
               IF NOT (CD-DOCP = "02" OR "06" OR "08" OR "09"
                       OR "10")
                   DISPLAY "not on the menu, pick again, "
                           "use 02 for not read please."
                   MOVE "00" TO CD-DOCP
                   GO TO P1-0
               END-IF
               IF CD-DOCP = "02"
                   MOVE SPACE TO OUTFILE01
                   STRING FO-KEY " USING 02 FOR DOCP AS NOT READ"
                          CD-PAYCODE " DOS " FO-DATE " PROC " FO-PROC
                          DELIMITED BY SIZE INTO OUTFILE01
                   WRITE OUTFILE01
               END-IF
           END-IF

           MOVE CD-DOCP TO HOLD-DOCP

      *    auto-code call back mammos
           IF (CD-PROC1 = "1446" OR "1447" OR "1448")
               DISPLAY "Autocoding call back mammo with R928"
               DISPLAY "Displaying read though, if looks odd, "
                       "note to change code later please."
               DISPLAY " "
               PERFORM 10-GR
               MOVE "R928   " TO CD-DIAG
               IF (CD-PAYCODE = "003" OR "028" OR "074" OR "141"
                   OR "200" OR "245" OR "270" OR "409" OR "663"
                   OR "697" OR "868" OR "912")
                   IF (CD-PROC1 = "1446")
                       MOVE "LT" TO CD-MOD2
                       MOVE "10937706526" TO CD-PROC
                   ELSE IF (CD-PROC1 = "1447")
                       MOVE "RT" TO CD-MOD2
                       MOVE "10947706526" TO CD-PROC
                   ELSE IF (CD-PROC1 = "1448")
                       MOVE "  " TO CD-MOD2
                       MOVE "10957706626" TO CD-PROC
                       MOVE 184 TO CD-AMOUNT
                   END-IF
               ELSE
                   MOVE "52" TO CD-MOD2
                   IF (CD-PROC1 = "1448")
                       MOVE "  " TO CD-MOD2
                   END-IF
               END-IF
               PERFORM RE-WRITE-CHARNEW THRU RE-WRITE-CHARNEW-EXIT
               GO TO P1
           END-IF

      *    auto-code tomosynthesis
           IF CD-PROC1 = "1449"
               MOVE "Z1231  " TO CD-DIAG
               DISPLAY "Autocoded tomosynthesis"
               DISPLAY " "
               PERFORM RE-WRITE-CHARNEW THRU RE-WRITE-CHARNEW-EXIT
               GO TO P1
           END-IF

      *    auto-code uni tomo
           IF (CD-PROC1 = "1456")
               DISPLAY "unilateral tomosynthesis -> added modifier 52"
               DISPLAY " "
               MOVE "Z1231  " TO CD-DIAG
               MOVE "52"      TO CD-MOD2
               PERFORM RE-WRITE-CHARNEW THRU RE-WRITE-CHARNEW-EXIT
               GO TO P1
           END-IF.

      *    high risk screen mammos
           IF (CD-PROC1 = "1097" OR "1098")
               DISPLAY "autocoded high risk screen w Z1231"
               DISPLAY " "
               MOVE "Z1231  " TO CD-DIAG
               PERFORM RE-WRITE-CHARNEW THRU RE-WRITE-CHARNEW-EXIT
               GO TO P1
           END-IF

      *    unilateral screening mammo - needs assessment, falls through
           IF (CD-PROC1 = "1099" OR "1442")
               DISPLAY "unilateral screening mammogram -> added mod 52"
               DISPLAY " "
               MOVE "Z1231  " TO CD-DIAG
               MOVE "52"      TO CD-MOD2
           END-IF

      *    same day screen with diag exam
           IF (CD-PROC1 = "1091" OR "1092" OR "1441")
               DISPLAY "adding GG mod to same day screen with dx exam"
               DISPLAY " "
               MOVE "GG" TO CD-MOD2
           END-IF

      *    quick code diag mammos
           IF (CD-PROC1 = "1093" OR "1094" OR "1095")
               IF CD-DOCP = "02"
                   GO TO P1-1
               END-IF
               DISPLAY "Quick code of diag mammo?"
               DISPLAY "Hit Y for R92.8"
               DISPLAY " "
               ACCEPT ANS1
               IF ANS1 = "Y"
                   MOVE "R928   " TO CD-DIAG
                   PERFORM RE-WRITE-CHARNEW THRU RE-WRITE-CHARNEW-EXIT
                   GO TO P1
               END-IF
           END-IF

           IF (CD-PROC1 = "1450" AND ANS1 = "Y")
               MOVE "R928   " TO CD-DIAG
               DISPLAY "Autocoded diag tomosynthesis"
               DISPLAY " "
               PERFORM RE-WRITE-CHARNEW THRU RE-WRITE-CHARNEW-EXIT
               GO TO P1
           END-IF

      *    us extremity non vasc 76882
           IF (CD-PROC1 = "2098")
               IF CD-DOCP = "02"
                   GO TO P1-1
               END-IF
               DISPLAY "Mod for 76882? RT, LT, or enter for none."
               DISPLAY " "
               ACCEPT ANS
               IF NOT (ANS = "RT " OR "LT " OR "   ")
                   DISPLAY "BAD MOD, TRY AGAIN PLEASE"
                   GO TO P1-0
               END-IF
               MOVE ANS TO CD-MOD2
           END-IF

      *    bilat studies
           IF ((CD-PROC1 = "1204" OR "1284" OR "1285" OR "3082")
               AND CD-MOD2 = "50")
               IF CD-DOCP = "02"
                   GO TO P1-1
               END-IF
               DISPLAY "Quick code of bilat study? Will be asked twice"
                       " due to VT Medicaid RT LT."
               DISPLAY "Y for M170, bilat OA of knee"
               DISPLAY " "
               ACCEPT ANS1
               IF ANS1 = "Y"
                   MOVE "M170   " TO CD-DIAG
                   PERFORM RE-WRITE-CHARNEW THRU RE-WRITE-CHARNEW-EXIT
                   GO TO P1
               END-IF
               DISPLAY "then what about knee pain?"
               DISPLAY "Y for M25561 and M25562"
               DISPLAY " "
               ACCEPT ANS1
               IF ANS1 = "Y"
                   MOVE "M25561 " TO CD-DIAG
                   MOVE "M25562 " TO CD-DX2
                   PERFORM RE-WRITE-CHARNEW THRU RE-WRITE-CHARNEW-EXIT
                   GO TO P1
               END-IF
           END-IF

      *    quick code bilat orbits
           IF (CD-PROC1 = "1204" AND CD-MOD2 = "50")
               COMPUTE CD-AMOUNT = 2 * CD-AMOUNT
               IF CD-DOCP = "02"
                   GO TO P1-1
               END-IF
               DISPLAY "Quick code of bilat orbits?"
               DISPLAY "Hit Y for Z87821 "
               DISPLAY " "
               ACCEPT ANS1
               IF ANS1 = "Y"
                   MOVE "Z87821 " TO CD-DIAG
                   PERFORM RE-WRITE-CHARNEW THRU RE-WRITE-CHARNEW-EXIT
                   GO TO P1
               END-IF
           END-IF

      *    auto-code call back ultrasounds
           IF (CD-PROC1 = "2190" OR "2191" OR "2192")
               DISPLAY "Autocode screening call back US with R928?"
               DISPLAY "Hit Y for R92.8"
               DISPLAY " "
               ACCEPT ANS1
               IF ANS1 = "Y"
                   MOVE "R928   " TO CD-DIAG
                   PERFORM RE-WRITE-CHARNEW THRU RE-WRITE-CHARNEW-EXIT
                   GO TO P1
               END-IF
           END-IF

      *    auto-code calcium screen
           IF CD-PROC1 = "5270"
               IF (CD-PAYCODE = "003" OR "028" OR "074" OR "141"
                   OR "200" OR "245" OR "270" OR "409" OR "663"
                   OR "697" OR "868" OR "912")
                   MOVE "GY" TO CD-MOD2
                   PERFORM RE-WRITE-CHARNEW THRU RE-WRITE-CHARNEW-EXIT
               END-IF
               DISPLAY "Autocode calcium screen?"
               DISPLAY "Hit Y for Z136"
               DISPLAY " "
               ACCEPT ANS1
               IF ANS1 = "Y"
                   MOVE "Z136   " TO CD-DIAG
                   PERFORM RE-WRITE-CHARNEW THRU RE-WRITE-CHARNEW-EXIT
                   GO TO P1
               END-IF
           END-IF.

      *----------------------------------------------------------------
       P1-1.
      *    Skip quality measure assessment for already-classified
      *    quality paycodes (pure and hybrid) and for mammo auto-codes
           IF (CD-PAYCODE = "008" OR "009" OR "010" OR "011" OR "012"
               OR "013" OR "014" OR "015" OR "016" OR "017")
               GO TO P2
           END-IF

      *    mammo codes
           IF (CD-PROC2 NOT = "7706726")
               GO TO P2
           END-IF
           MOVE "Z1231  " TO CD-DIAG
           DISPLAY "Screening mammo -> auto coded "
                   CD-KEY8 " " FO-NAME
           DISPLAY " "
           PERFORM RE-WRITE-CHARNEW THRU RE-WRITE-CHARNEW-EXIT
           GO TO P1.

      *----------------------------------------------------------------
       P2.
           MOVE FO-KEY(1:8) TO G-GARNO
           READ GARFILE INVALID
               DISPLAY "GARNO NOT AVAILABLE FOR SOME UNKNOWN REASON"
               DISPLAY "PLEASE RECORD THIS FACT " FO-KEY(1:8)
               CONTINUE
           END-READ.

      *----------------------------------------------------------------
       P2-00.
           DISPLAY CD-DATE-T(5:2) "-" CD-DATE-T(7:2) "-"
                   CD-DATE-T(1:4)
                   " " CD-PROC2 " " CD-MOD2 " " CD-MOD3
                   " " CD-PAYCODE " " CD-PLACE
                   " " G-DOB(5:2) "-" G-DOB(7:2) "-" G-DOB(1:4)
                   " " G-ACCT " " FO-NAME.
           MOVE FO-PROC TO PROC-KEY
           READ PROCFILE INVALID
               DISPLAY "bad " FO-PROC
           END-READ
           DISPLAY PROC-TITLE
           IF CD-DAT1 NOT = "00000000"
               DISPLAY "accident!"
           END-IF
           DISPLAY CD-CLINICAL
           DISPLAY CD-ADMIT-DIAG
           DISPLAY " ".

      *----------------------------------------------------------------
       P2-0.
      *--- Handle unread studies for any quality paycode ---------------
           IF (CD-PAYCODE = "009" OR "010" OR "012" OR "013"
               OR "014" OR "015" OR "016" OR "017")
               IF CD-DOCP = "02"
                   DISPLAY "Skipping assessment so will need to code"
                   DISPLAY "quality codes as well once study is read"
                   DISPLAY "writing measure code log to track"
                   MOVE SPACE TO OUTFILE01
                   STRING FO-KEY
                          " unread, code assessment when read "
                          CD-PAYCODE " DOS " FO-DATE
                          " PROC " FO-PROC
                          DELIMITED BY SIZE INTO OUTFILE01
                   WRITE OUTFILE01
                   PERFORM RE-WRITE-CHARNEW THRU RE-WRITE-CHARNEW-EXIT
                   GO TO P1
               END-IF
           END-IF

      *--- QMM26 / Paycode 009 -----------------------------------------
           IF CD-PAYCODE = "009"
               DISPLAY " QMM26: type ? or 1 or 2 or 3 or 4 or 5 or G"
               ACCEPT CD-QP1
               IF CD-QP1 = "G"
                   PERFORM 10-GR
                   GO TO P2-0
               END-IF
               IF CD-QP1 = "?"
                   DISPLAY "Screening AAA pt >= 50 yo"
                   DISPLAY " 1 = incl rec < 5.5cm in size"
                   DISPLAY " 2 = incl rec > 5.5cm in size"
                   DISPLAY " 3 = negative for AAA"
                   DISPLAY " 4 = performance not met with rec for f/u"
                   DISPLAY " 5 = den exception, pt under act surveil"
                   GO TO P2-0
               END-IF
               IF NOT (CD-QP1 = "1 " OR "2 " OR "3 " OR "4 " OR "5 ")
                   GO TO P2-0
               END-IF
           END-IF

      *--- Measure 364 / Paycode 010 -----------------------------------
           IF CD-PAYCODE = "010"
               DISPLAY " Measure 364: <Enter> no lesion or 1 or 2"
                       " or 3 or ? for help or G to grab read"
               ACCEPT CD-QP1
               IF CD-QP1 = "G"
                   PERFORM 10-GR
                   GO TO P2-0
               END-IF
               IF CD-QP1 = "?"
                   PERFORM HELP-364
                   GO TO P2-0
               END-IF
               IF NOT (CD-QP1 = "1 " OR "2 " OR "3 " OR SPACE)
                   GO TO P2-0
               END-IF
           END-IF

      *--- Measure 405 / Paycode 012 -----------------------------------
           IF CD-PAYCODE = "012"
               DISPLAY " Measure 405: type ? or 1 or 2 or 3 or"
                       " <Enter> or G to grab read"
               ACCEPT CD-QP1
               IF CD-QP1 = "G"
                   PERFORM 10-GR
                   GO TO P2-0
               END-IF
               IF CD-QP1 = "?"
                   PERFORM HELP-405
                   GO TO P2-0
               END-IF
               IF NOT (CD-QP1 = "1 " OR "2 " OR "3 " OR SPACE)
                   GO TO P2-0
               END-IF
           END-IF

      *--- Measure 406 / Paycode 013 -----------------------------------
           IF CD-PAYCODE = "013"
               DISPLAY " Measure 406: <Enter> no lesion or 1 or 2"
                       " or 3 or ? for help or G to grab read"
               ACCEPT CD-QP1
               IF CD-QP1 = "G"
                   PERFORM 10-GR
                   GO TO P2-0
               END-IF
               IF CD-QP1 = "?"
                   PERFORM HELP-406
                   GO TO P2-0
               END-IF
               IF NOT (CD-QP1 = "1 " OR "2 " OR "3 " OR SPACE)
                   GO TO P2-0
               END-IF
           END-IF

      *--- Hybrid 364+405 / Paycode 014 --------------------------------
           IF CD-PAYCODE = "014"
               DISPLAY "=== PAYCODE 014: MEASURES 364 AND 405 ==="
      *        Prompt for Measure 364 first -> CD-QP1
               DISPLAY " Measure 364: <Enter> no lesion or 1 or 2"
                       " or 3 or ? for help or G to grab read"
               ACCEPT CD-QP1
               IF CD-QP1 = "G"
                   PERFORM 10-GR
                   GO TO P2-0
               END-IF
               IF CD-QP1 = "?"
                   PERFORM HELP-364
                   GO TO P2-0
               END-IF
               IF NOT (CD-QP1 = "1 " OR "2 " OR "3 " OR SPACE)
                   GO TO P2-0
               END-IF
      *        Now prompt for Measure 405 -> CD-QP2
               DISPLAY " Measure 405: type ? or 1 or 2 or 3 or <Enter>"
               ACCEPT CD-QP2
               IF CD-QP2 = "?"
                   PERFORM HELP-405
                   DISPLAY " Measure 405: type 1 or 2 or 3 or <Enter>"
                   ACCEPT CD-QP2
               END-IF
               IF NOT (CD-QP2 = "1 " OR "2 " OR "3 " OR SPACE)
                   DISPLAY "Invalid 405 response, re-enter"
                   DISPLAY " Measure 405: type 1 or 2 or 3 or <Enter>"
                   ACCEPT CD-QP2
               END-IF
           END-IF

      *--- Hybrid 364+406 / Paycode 015 --------------------------------
           IF CD-PAYCODE = "015"
               DISPLAY "=== PAYCODE 015: MEASURES 364 AND 406 ==="
      *        Prompt for Measure 364 first -> CD-QP1
               DISPLAY " Measure 364: <Enter> no lesion or 1 or 2"
                       " or 3 or ? for help or G to grab read"
               ACCEPT CD-QP1
               IF CD-QP1 = "G"
                   PERFORM 10-GR
                   GO TO P2-0
               END-IF
               IF CD-QP1 = "?"
                   PERFORM HELP-364
                   GO TO P2-0
               END-IF
               IF NOT (CD-QP1 = "1 " OR "2 " OR "3 " OR SPACE)
                   GO TO P2-0
               END-IF
      *        Now prompt for Measure 406 -> CD-QP2
               DISPLAY " Measure 406: <Enter> no lesion or 1 or 2"
                       " or 3 or ? for help"
               ACCEPT CD-QP2
               IF CD-QP2 = "?"
                   PERFORM HELP-406
                   DISPLAY " Measure 406: type 1 or 2 or 3 or <Enter>"
                   ACCEPT CD-QP2
               END-IF
               IF NOT (CD-QP2 = "1 " OR "2 " OR "3 " OR SPACE)
                   DISPLAY "Invalid 406 response, re-enter"
                   DISPLAY " Measure 406: type 1 or 2 or 3 or <Enter>"
                   ACCEPT CD-QP2
               END-IF
           END-IF

      *--- Hybrid 405+406 / Paycode 016 --------------------------------
           IF CD-PAYCODE = "016"
               DISPLAY "=== PAYCODE 016: MEASURES 405 AND 406 ==="
      *        Prompt for Measure 405 -> CD-QP1
               DISPLAY " Measure 405: type ? or 1 or 2 or 3 or <Enter>"
               ACCEPT CD-QP1
               IF CD-QP1 = "?"
                   PERFORM HELP-405
                   DISPLAY " Measure 405: type 1 or 2 or 3 or <Enter>"
                   ACCEPT CD-QP1
               END-IF
               IF NOT (CD-QP1 = "1 " OR "2 " OR "3 " OR SPACE)
                   GO TO P2-0
               END-IF
      *        Now prompt for Measure 406 -> CD-QP2
               DISPLAY " Measure 406: <Enter> no lesion or 1 or 2"
                       " or 3 or ? for help"
               ACCEPT CD-QP2
               IF CD-QP2 = "?"
                   PERFORM HELP-406
                   DISPLAY " Measure 406: type 1 or 2 or 3 or <Enter>"
                   ACCEPT CD-QP2
               END-IF
               IF NOT (CD-QP2 = "1 " OR "2 " OR "3 " OR SPACE)
                   DISPLAY "Invalid 406 response, re-enter"
                   DISPLAY " Measure 406: type 1 or 2 or 3 or <Enter>"
                   ACCEPT CD-QP2
               END-IF
           END-IF

      *--- Hybrid 364+405+406 / Paycode 017 ---------------------------
           IF CD-PAYCODE = "017"
               DISPLAY "=== PAYCODE 017: MEASURES 364, 405, AND 406 ==="
      *        Prompt for Measure 364 first -> CD-QP1
               DISPLAY " Measure 364: <Enter> no lesion or 1 or 2"
                       " or 3 or ? for help or G to grab read"
               ACCEPT CD-QP1
               IF CD-QP1 = "G"
                   PERFORM 10-GR
                   GO TO P2-0
               END-IF
               IF CD-QP1 = "?"
                   PERFORM HELP-364
                   GO TO P2-0
               END-IF
               IF NOT (CD-QP1 = "1 " OR "2 " OR "3 " OR SPACE)
                   GO TO P2-0
               END-IF
      *        Prompt for Measure 405 -> CD-QP2
               DISPLAY " Measure 405: type ? or 1 or 2 or 3 or <Enter>"
               ACCEPT CD-QP2
               IF CD-QP2 = "?"
                   PERFORM HELP-405
                   DISPLAY " Measure 405: type 1 or 2 or 3 or <Enter>"
                   ACCEPT CD-QP2
               END-IF
               IF NOT (CD-QP2 = "1 " OR "2 " OR "3 " OR SPACE)
                   DISPLAY "Invalid 405 response, re-enter"
                   DISPLAY " Measure 405: type 1 or 2 or 3 or <Enter>"
                   ACCEPT CD-QP2
               END-IF
      *        Prompt for Measure 406 -> CD-QP3
               DISPLAY " Measure 406: <Enter> no lesion or 1 or 2"
                       " or 3 or ? for help"
               ACCEPT CD-QP3
               IF CD-QP3 = "?"
                   PERFORM HELP-406
                   DISPLAY " Measure 406: type 1 or 2 or 3 or <Enter>"
                   ACCEPT CD-QP3
               END-IF
               IF NOT (CD-QP3 = "1 " OR "2 " OR "3 " OR SPACE)
                   DISPLAY "Invalid 406 response, re-enter"
                   DISPLAY " Measure 406: type 1 or 2 or 3 or <Enter>"
                   ACCEPT CD-QP3
               END-IF
           END-IF.

      *----------------------------------------------------------------
       P2-000.
           IF CD-DOCP = "02"
               DISPLAY "Skipping since not read"
               PERFORM RE-WRITE-CHARNEW THRU RE-WRITE-CHARNEW-EXIT
               GO TO P1
           END-IF

           IF (CD-PROC1 = "5232" OR "5233")
               DISPLAY "LD lung screen -> auto coded"
               DISPLAY " "
               MOVE "Z87891 " TO CD-DIAG
               PERFORM RE-WRITE-CHARNEW THRU RE-WRITE-CHARNEW-EXIT
               GO TO P1
           END-IF

           DISPLAY " DIAG? "
           ACCEPT IN-FIELD-7.

           IF IN-FIELD-7 = "?"
               DISPLAY "DIAG, END, B, F, M or S"
               GO TO P2-00
           END-IF

           IF IN-FIELD-7 = "F" OR "M"
               MOVE 1 TO DIAG-FLAG
               PERFORM CD10 THRU CD10-EXIT
               IF DIAG-FLAG = 0
                   GO TO P2-9
               END-IF
               IF DIAG-FLAG = 1
                   GO TO P2-00
               END-IF
           END-IF

           IF IN-FIELD-7 = "S"
               MOVE 1 TO DIAG-FLAG
               MOVE SPACE TO IN-FIELD-7
               PERFORM A4
               IF DIAG-FLAG = 0
                   GO TO P2-9
               END-IF
               IF DIAG-FLAG = 1
                   GO TO P2-00
               END-IF
           END-IF

           IF IN-FIELD-7 = "END"
               GO TO P99
           END-IF

           IF IN-FIELD-7 = "B"
               DISPLAY FO-KEY " HAS BEEN SKIPPED"
               DISPLAY "PLEASE RECORD THIS FACT"
               MOVE SPACE   TO HOLD-DIAG
               MOVE "0000000" TO HOLD7
               GO TO P1
           END-IF

           IF IN-FIELD-7 = "."
               MOVE HOLD7 TO IN-FIELD-7
           END-IF

           IF IN-FIELD-7 = "G"
               PERFORM 10-GR
               GO TO P2-0
           END-IF.

           MOVE IN-FIELD-7 TO DIAG-KEY
           MOVE SPACE TO HOLD-DIAG
           READ DIAGFILE INVALID
               DISPLAY "NOT ON FILE" BELL0
               GO TO P2-00
           END-READ
           IF CD-DATE-T > "20160930" AND DIAG-TITLE(1:1) = "?"
               DISPLAY DIAG-KEY " WAS INACTIVATED"
               ACCEPT OMITTED
               GO TO P2-00
           END-IF.

      *----------------------------------------------------------------
       P2-9.
           MOVE IN-FIELD-7 TO DIAG-KEY
           READ DIAGFILE
               INVALID DISPLAY "BAD CODE " IN-FIELD-7
               GO TO P2-000
           END-READ
           IF CD-DATE-T > "20160930" AND DIAG-TITLE(1:1) = "?"
               DISPLAY DIAG-KEY " WAS INACTIVATED"
               ACCEPT OMITTED
               GO TO P2-00
           END-IF
           DISPLAY DIAG-KEY " " DIAG-TITLE
           IF DIAG2 = SPACE
               MOVE DIAG-KEY TO CD-DIAG
               MOVE CD-DIAG  TO HOLD7
           ELSE
               MOVE DIAG-KEY TO CD-DX2
           END-IF
           IF CD-DIAG = CD-DX2
               MOVE "0000000" TO CD-DX2
           END-IF
           PERFORM RE-WRITE-CHARNEW THRU RE-WRITE-CHARNEW-EXIT
           MOVE CD-DIAG TO HOLD7
           IF DIAG2 NOT = SPACE
               MOVE SPACE TO DIAG2
               GO TO P1.

      *----------------------------------------------------------------
       P3.
           DISPLAY "2nd diag? Y".
           ACCEPT DIAG2.
           IF DIAG2 NOT = "Y"
               MOVE SPACE TO DIAG2
               GO TO P1
           END-IF
           DISPLAY FO-DATE " " FO-KEY " " FO-PROC " " CD-PAYCODE
                   " " CD-PLACE " " G-DOB " " G-ACCT " " FO-NAME.
           DISPLAY PROC-TITLE
           IF CD-DAT1 NOT = "00000000"
               DISPLAY "accident"
           END-IF
           DISPLAY CD-CLINICAL
           DISPLAY CD-ADMIT-DIAG
           DISPLAY " ".
           DISPLAY HOLD7
           GO TO P2-000.

      *----------------------------------------------------------------
      *  HELP paragraphs - one per measure to keep P2-0 clean
      *----------------------------------------------------------------
       HELP-364.
           DISPLAY "CT studies for patients 35 and older with "
           DISPLAY "incidental pulmonary nodule and not active"
           DISPLAY "cancer dx, or heavy smoker or lung cx scr"
           DISPLAY " 1 = follow up rec documented, perf met"
           DISPLAY " 2 = medical reasons for no f/u, "
                   "denom exception"
           DISPLAY " 3 = perf not met, no f/u and reason"
           DISPLAY " <Enter> = no lesion".

       HELP-405.
           DISPLAY "Cystic renal lesion that is simple appearing"
           DISPLAY " or Adrenal lesion <= 1.0 cm"
           DISPLAY " or Adrenal lesion > 1.0 cm but <= 4.0 cm"
           DISPLAY " classified as likely benign by unenhanced CT,"
           DISPLAY " washout protocol CT, or MRI with in- and"
           DISPLAY " opposed-phase sequences or equivalent protocol"
           DISPLAY " 1 = imaging not recommended"
           DISPLAY " 2 = medical necessity"
           DISPLAY " 3 = imaging recommended (unusual)"
           DISPLAY " <Enter> = no lesion".

       HELP-406.
           DISPLAY "CT, CTA, or MR studies of chest or neck"
           DISPLAY "for patients aged 18 and older with "
           DISPLAY "no known thyroid disease and an "
           DISPLAY "incidentally-detected thyroid nodule"
           DISPLAY " < 1.0 cm noted"
           DISPLAY " 1 = follow up images recommended, oh no"
           DISPLAY " 2 = medical reasons for f/u images, ok"
           DISPLAY " 3 = f/u not recommended, perf not met,"
           DISPLAY "     an inverse measure so very good we hope".

      *----------------------------------------------------------------
       A4.
           MOVE CD-PROC2  TO ALW-PROC
           MOVE IN-FIELD-7 TO ALW-DIAG
           READ ALLOWFILE
               INVALID
                   PERFORM A5 THRU A5-EXIT
               NOT INVALID
                   MOVE 0 TO DIAG-FLAG
           END-READ.

      *----------------------------------------------------------------
       A5.
           MOVE SPACE TO ALW-DIAG
           MOVE 0 TO X.

       A5-0.
           START ALLOWFILE KEY NOT < ALW-KEY
               INVALID
                   DISPLAY "NO DIAGS YET"
                   GO TO A5-EXIT
           END-START.

       A5-1.
           READ ALLOWFILE NEXT
               AT END GO TO A5-2
           END-READ
           MOVE ALW-DIAG TO DIAG-KEY
           IF (CD-DATE-T > "20150930") AND (ALW-DIAG(6:2) = "??")
               GO TO A5-1
           END-IF
           IF ALW-PROC NOT = CD-PROC2
               GO TO A5-2
           END-IF
           READ DIAGFILE INVALID
               DISPLAY DIAG-KEY " INVALID DIAG-KEY"
               MOVE SPACE TO DIAG-TITLE
           END-READ
           IF DIAG-TITLE(1:1) = "?"
               GO TO A5-1
           END-IF
           IF ALW-FLAG = "1"
               MOVE "POLICY" TO ALF6
           ELSE
               MOVE SPACE TO ALF6
           END-IF
           ADD 1 TO X
           MOVE X TO NEF2.
           DISPLAY NEF2 " " ALW-DIAG " " ALF6 " " DIAG-TITLE.
           MOVE DIAG-KEY TO TAGTAB(X)
           IF X < 20
               GO TO A5-1
           END-IF.

       A5-11.
           DISPLAY "SELECT FROM THE LIST"
           ACCEPT ANS
           IF ANS = "?"
               DISPLAY "PICK A NUMBER FROM THE LIST"
               DISPLAY "X TO QUIT, ENTER TO CONTINUE LISTING"
               DISPLAY "TYPE A STARTING POINT FOR CODES TO LIST"
               GO TO A5-11
           END-IF
           IF ANS = SPACE
               MOVE 0 TO X
               GO TO A5-1
           END-IF
           IF ANS = "E  "
               MOVE 1 TO DIAG-FLAG
               GO TO A5-EXIT
           END-IF
           MOVE ANS(1:2) TO ALF-2
           IF ALF-2 = SPACE
               MOVE 0 TO X
               GO TO A5-1
           END-IF
           MOVE SPACE TO RIGHT-2
           UNSTRING ALF-2 DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING LEADING " " BY "0"
           MOVE RIGHT-2 TO ALF-2
           IF ALF-2 NUMERIC
               MOVE ALF-2 TO NUM-2
               IF (NUM-2 > 0) AND (NUM-2 <= X)
                   MOVE TAGTAB(NUM-2) TO IN-FIELD-7
                   MOVE 0 TO DIAG-FLAG
                   GO TO A5-EXIT
               END-IF
           END-IF
           GO TO A5-EXIT.

       A5-2.
           DISPLAY "USE THE CODE TYPED? " HOLD-DIAG "  Y/N".
           DISPLAY " OR SELECT FROM THE LIST"
           ACCEPT ANS
           IF ANS = "Y  "
               MOVE 0 TO DIAG-FLAG
               GO TO A5-EXIT
           END-IF
           IF ANS = "N  "
               MOVE 1 TO DIAG-FLAG
               GO TO A5-EXIT
           END-IF
           MOVE ANS(1:2) TO ALF-2
           IF ALF-2 = SPACE
               MOVE 0 TO X
               GO TO A5-1
           END-IF
           MOVE SPACE TO RIGHT-2
           UNSTRING ALF-2 DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING LEADING " " BY "0"
           MOVE RIGHT-2 TO ALF-2
           IF ALF-2 NUMERIC
               MOVE ALF-2 TO NUM-2
               IF (NUM-2 > 0) AND (NUM-2 <= X)
                   MOVE TAGTAB(NUM-2) TO IN-FIELD-7
                   MOVE 0 TO DIAG-FLAG
                   GO TO A5-EXIT
               END-IF
           END-IF.

       A5-EXIT.
           EXIT.

      *----------------------------------------------------------------
       LIST-DOC.
           DISPLAY "02 = not read, send to error list to check later"
           DISPLAY "06 is Dan Mitchell MD"
           DISPLAY "08 is Trent Shelton DO"
           DISPLAY "09 is Jed Hummel MD"
           DISPLAY "10 is Andy Boyer MD".

      *----------------------------------------------------------------
       CD10.
           IF IN-FIELD-7 = "F"
               GO TO 1DIAG-SEARCH
           END-IF
           IF IN-FIELD-7 = "M"
               GO TO 1MAP
           END-IF
           IF (IN-FIELD-TAB(1) NUMERIC) OR (IN-FIELD-TAB(1) = "V")
               MOVE SPACE TO ALF-7
               STRING IN-FIELD-7(1:5) "??"
                      DELIMITED BY SIZE INTO ALF-7
               MOVE ALF-7 TO IN-FIELD-7
           END-IF
           MOVE IN-FIELD-7 TO DIAG-KEY.
           READ DIAGFILE INVALID
               DISPLAY "NOT ON FILE"
           END-READ
           IF (CD-DATE-T > "20150930" AND DIAG-KEY(6:2) = "??")
               DISPLAY "USE ICD10 CODE WITH THIS DATE"
               GO TO CD10-EXIT
           END-IF
           GO TO CD10-EXIT.

      *----------------------------------------------------------------
       1DIAG-SEARCH.
           MOVE 1 TO FLAG
           DISPLAY "SEARCH KEY ?"
           ACCEPT DIAG-TITLE
           IF DIAG-TITLE = "?"
               DISPLAY "ICD10 BY TITLE, TYPE AT LEAST ONE LETTER"
               DISPLAY "ICD10 BY CODE, TYPE A LETTER THEN AT LEAST 1 #"
               GO TO 1DIAG-SEARCH
           END-IF
           IF DIAG-TITLE = SPACE
               GO TO CD10-EXIT
           END-IF
           IF (FLAG NOT = 9) AND (DIAG-TITLE(1:1) ALPHABETIC)
               AND (DIAG-TITLE(2:1) NUMERIC)
               MOVE DIAG-TITLE(1:5) TO DIAG-KEY
               GO TO 4DIAG
           END-IF
           START DIAGFILE KEY NOT < DIAG-TITLE INVALID
               DISPLAY "END OF FILE"
               GO TO CD10-EXIT
           END-START
           MOVE 0 TO X.
           GO TO 3DIAG.

       4DIAG.
           START DIAGFILE KEY NOT < DIAG-KEY INVALID
               DISPLAY "END OF FILE"
               GO TO CD10-EXIT
           END-START
           MOVE 0 TO X.

       3DIAG.
           READ DIAGFILE NEXT AT END
               DISPLAY "END OF FILE"
               GO TO 3DIAG-0
           END-READ
           IF DIAG-TITLE(1:1) = "?"
               GO TO 3DIAG
           END-IF
           IF ((FLAG = 1) AND (DIAG-KEY(6:2) = "??"))
               GO TO 3DIAG
           END-IF
           ADD 1 TO X
           MOVE X TO NEF2
           DISPLAY NEF2 " " DIAG-KEY " " DIAG-MEDB " " DIAG-TITLE
           MOVE DIAG-KEY TO TAGTAB(X)
           IF X < 20
               GO TO 3DIAG
           END-IF.

       3DIAG-0.
           DISPLAY "SELECT ?"
           ACCEPT ANS.
           IF ANS = "?"
               DISPLAY "PICK A NUMBER,X TO QUIT,ENTER FOR MORE"
               GO TO 3DIAG-0
           END-IF
           IF ANS = SPACE
               MOVE 0 TO X
               GO TO 3DIAG
           END-IF
           IF ANS = "X"
               GO TO CD10-EXIT
           END-IF
           MOVE ANS(1:2) TO ALF-2
           MOVE SPACE TO RIGHT-2
           UNSTRING ALF-2 DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING LEADING " " BY "0"
           MOVE RIGHT-2 TO ALF-2
           IF ALF-2 NUMERIC
               MOVE ALF-2 TO NUM-2
               IF (NUM-2 > 0) AND (NUM-2 <= X)
                   MOVE TAGTAB(NUM-2) TO IN-FIELD-7
                   MOVE 0 TO DIAG-FLAG
                   GO TO CD10-EXIT
               END-IF
           END-IF
           MOVE 0 TO X
           GO TO 1DIAG-SEARCH.

      *----------------------------------------------------------------
       1MAP.
           DISPLAY "ENTER A VALID ICD9 CODE OR F TO SEARCH ICD9"
           DISPLAY "X TO QUIT."
           ACCEPT ALF-5.
           IF ALF-5 = "F"
               MOVE 1 TO DIAG-FLAG
               PERFORM F1 THRU F1-EXIT
               IF DIAG-FLAG = 1
                   GO TO 1MAP
               END-IF
           END-IF
           MOVE ALF-5 TO TAG-ICD9-5
           MOVE SPACE TO TAG-ICD9-7
           START TAGDIAG KEY NOT < TAG-ICD9 INVALID
               GO TO CD10-EXIT
           END-START
           MOVE 0 TO Y.

       2MAP.
           READ TAGDIAG NEXT AT END
               GO TO 4MAP
           END-READ
           IF TAG-ICD9-5 NOT = ALF-5
               GO TO 4MAP
           END-IF
           MOVE TAG-ICD9-7 TO DIAG-KEY
           READ DIAGFILE INVALID
               GO TO 2MAP
           END-READ
           IF DIAG-TITLE(1:1) = "?"
               GO TO 2MAP
           END-IF
           ADD 1 TO Y
           MOVE TAG-ICD9-7 TO TAGTAB(Y)
           MOVE Y TO NEF2
           DISPLAY NEF2 " " TAG-ICD9-7 " " DIAG-TITLE.

       3MAP.
           IF Y = 0
               GO TO 1MAP
           END-IF
           IF Y < 20
               GO TO 2MAP
           END-IF.

       4MAP.
           DISPLAY "CHOOSE FROM THE LIST".
           ACCEPT ALF-2.
           IF ALF-2 = "?"
               DISPLAY "A = USE A DIFFERENT ICD9 CODE"
               DISPLAY "X = STOP THE MAPPING"
               DISPLAY "PICK A NUMBER FROM THE LIST"
               DISPLAY " TO MAKE A SELECTION"
               DISPLAY "ENTER KEY TO CONTINUE THE MAPPING"
               GO TO 4MAP
           END-IF
           IF ALF-2 = "A"
               GO TO 1MAP
           END-IF
           IF ALF-2 = "X"
               GO TO CD10-EXIT
           END-IF
           IF ALF-2 = SPACE
               MOVE 0 TO Y
               GO TO 2MAP
           END-IF
           MOVE SPACE TO RIGHT-2
           UNSTRING ALF-2 DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING LEADING " " BY "0"
           MOVE RIGHT-2 TO ALF-2
           IF ALF-2 NUMERIC
               MOVE ALF-2 TO NUM-2
               IF (NUM-2 > 0) AND (NUM-2 <= Y)
                   MOVE TAGTAB(NUM-2) TO IN-FIELD-7
                   MOVE 0 TO DIAG-FLAG
                   GO TO CD10-EXIT
               END-IF
           END-IF
           GO TO 4MAP.

       CD10-EXIT.
           EXIT.

      *----------------------------------------------------------------
       F1.
           MOVE 1 TO DIAG-FLAG
           DISPLAY "ENTER A STARTING POINT"
           ACCEPT DIAG9-TITLE.
           IF DIAG9-TITLE = "X"
               GO TO F1-EXIT
           END-IF
           IF (((DIAG9-TITLE(1:1) = "V") AND
                (DIAG9-TITLE(2:1) NUMERIC)) OR
               ((DIAG9-TITLE(1:1) NUMERIC) AND
                (DIAG9-TITLE(2:1) NUMERIC)))
               MOVE DIAG9-TITLE TO DIAG9-KEY
               MOVE 0 TO X
               GO TO F4DIAG
           END-IF
           START DIAG9FILE KEY NOT < DIAG9-TITLE INVALID
               DISPLAY "END OF FILE"
               GO TO F1
           END-START
           MOVE 0 TO X.
           GO TO F3DIAG.

       F4DIAG.
           START DIAG9FILE KEY NOT < DIAG9-KEY INVALID
               DISPLAY "END OF FILE"
               GO TO F1
           END-START
           MOVE 0 TO X.

       F3DIAG.
           READ DIAG9FILE NEXT AT END
               DISPLAY "END OF FILE"
               GO TO F3DIAG-0
           END-READ
           ADD 1 TO X
           MOVE X TO NEF2
           DISPLAY NEF2 " " DIAG9-KEY " " DIAG9-TITLE
           MOVE DIAG9-KEY TO TAGTAB(X)
           IF X < 20
               GO TO F3DIAG
           END-IF.

       F3DIAG-0.
           DISPLAY "SELECT ?"
           ACCEPT ANS.
           IF ANS = "?"
               DISPLAY "PICK A NUMBER,X TO QUIT,ENTER FOR MORE"
               GO TO 3DIAG-0
           END-IF
           IF ANS = SPACE
               MOVE 0 TO X
               GO TO F3DIAG
           END-IF
           IF ANS = "X"
               GO TO F1-EXIT
           END-IF
           MOVE ANS(1:2) TO ALF-2
           MOVE SPACE TO RIGHT-2
           UNSTRING ALF-2 DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING LEADING " " BY "0"
           MOVE RIGHT-2 TO ALF-2
           IF ALF-2 NUMERIC
               MOVE ALF-2 TO NUM-2
               IF (NUM-2 > 0) AND (NUM-2 <= X)
                   MOVE TAGTAB(NUM-2) TO ALF-5
                   MOVE 0 TO DIAG-FLAG
                   GO TO F1-EXIT
               END-IF
           END-IF
           MOVE 0 TO X
           GO TO F1.

       F1-EXIT.
           EXIT.

      *----------------------------------------------------------------
       RE-WRITE-CHARNEW.
           MOVE CHARNEW01 TO CHARNEW-BACK
           CLOSE CHARNEW
           OPEN I-O CHARNEW
           MOVE CHARNEW-BACK TO CHARNEW01
           REWRITE CHARNEW01 INVALID
               DISPLAY FILE-OUT01
               DISPLAY "THAT'S ODD. RECORD NOT MODIFIED AT THIS TIME."
               ACCEPT OMITTED
               DISPLAY CHARNEW-STAT
               CLOSE CHARNEW
               OPEN INPUT CHARNEW
               GO TO RE-WRITE-CHARNEW-EXIT
           END-REWRITE
           CLOSE CHARNEW
           OPEN INPUT CHARNEW.

       RE-WRITE-CHARNEW-EXIT.
           EXIT.

      *----------------------------------------------------------------
       10-GR.
           MOVE SPACE TO FILEOUT201
           STRING G-ACCT CD-VISITNO CHARNEW-KEY CD-DATE-T
                  DELIMITED BY SIZE INTO FILEOUT201
           WRITE FILEOUT201.
           CLOSE FILEOUT2
           CALL "SYSTEM" USING "emr-4"
           OPEN OUTPUT FILEOUT2.

      *----------------------------------------------------------------
       P99.
           CLOSE CHARNEW PROCFILE GARFILE DIAGFILE DIAG9FILE
                 ALLOWFILE FILE-OUT OUTFILE TAGDIAG FILEOUT2.
           STOP RUN.