      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NEI151.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INSFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES.
           SELECT CHARFILE ASSIGN TO "S35"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT OUT ASSIGN TO "S40"
           ORGANIZATION LINE SEQUENTIAL.   
           SELECT INFILE ASSIGN TO "S45"
           ORGANIZATION LINE SEQUENTIAL. 
       DATA DIVISION.
       FILE SECTION.
       FD  INSFILE
     *     BLOCK CONTAINS 6 RECORDS
           DATA RECORD IS INSFILE01.
       01  INSFILE01.
           02 INS-KEY PIC XXX.
           02 INS-NAME PIC X(22).
           02 INS-STREET PIC X(24).
           02 INS-CITY PIC X(15).
           02 INS-STATE PIC XX.
           02 INS-ZIP PIC X(9).
           02 INS-ASSIGN PIC X.
           02 INS-CLAIMTYPE PIC X.
           02 INS-NEIC PIC X(5).
           02 INS-NEICLEVEL PIC X.
           02 INS-NEIC-ASSIGN PIC X.
           02 INS-PPO PIC X.
           02 INS-PRVNUM PIC X(10).
           02 INS-HMO PIC X(3).
           02 INS-STATUS PIC X.
           02 INS-LEVEL PIC X.
           02 INS-LASTDATE PIC X(8).
           02 INS-CAID PIC XXX.
           02 INS-REFWARN PIC X.
           02 INS-FUTURE PIC X(8).
       FD  INFILE.
       01  INFILE01.
           02 INF1 PIC XXX.
           02 FILLER PIC X(13).
       FD OUT.
       01  OUT01 PIC X(80).
       FD  CHARFILE
           DATA RECORD IS CHARFILE01.
       01  CHARFILE01.
           02 CHARFILE-KEY.
             03 CD-KEY8 PIC X(8).
             03 CD-KEY3 PIC XXX.
           02 CD-PATID PIC X(8).
           02 CD-CLAIM PIC X(6).
           02 CD-SERVICE PIC X.
           02 CD-DIAG PIC X(7).
           02 CD-PROC PIC X(7).
           02 CD-MOD2 PIC XX.
           02 CD-MOD3 PIC XX.
           02 CD-MOD4 PIC XX.
           02 CD-AMOUNT PIC S9(4)V99.
           02 CD-DOCR PIC X(3).
           02 CD-DOCP PIC X(2).
           02 CD-PAYCODE PIC 999.
           02 CD-STAT PIC X.
           02 CD-WORK PIC XX.
           02 CD-DAT1 PIC X(8).
           02 CD-RESULT PIC X.
           02 CD-ACT PIC X.
           02 CD-SORCREF PIC X.
           02 CD-COLLT PIC X.
           02 CD-AGE PIC X.
           02 CD-PAPER PIC X.
           02 CD-PLACE PIC X.
           02 CD-NAME PIC X(24).
           02 CD-IOPAT PIC X.
           02 CD-DATE-T PIC X(8).
           02 CD-DATE-E PIC X(8).
           02 CD-ORDER PIC X(6).
           02 CD-DX2 PIC X(7).
           02 CD-DX3 PIC X(7).
           02 CD-DATE-A PIC X(8).
           02 CD-ACC-TYPE PIC X.
           02 CD-DATE-M PIC X(8).
           02 CD-ASSIGN PIC X.
           02 CD-NEIC-ASSIGN PIC X.
           02 CD-DX4 PIC X(7).
           02 CD-DX5 PIC X(7).
           02 CD-DX6 PIC X(7).
           02 CD-FUTURE PIC X(6).
       WORKING-STORAGE SECTION.
       01  TITLE01 PIC X(80) VALUE SPACE.
       01  T101.
           02 T11 PIC X(8) VALUE "GARNO   ".
           02 T12 PIC X(7) VALUE " PROC  ".
           02 T13 PIC X(11) VALUE "    AMOUNT ".
       01  T201.
           02 T21 PIC X(8) VALUE "GARNO   ".
           02 T22 PIC X(7) VALUE " PROC  ".
           02 T23 PIC X(12) VALUE "    AMOUNT  ".
           02 T24 PIC X(17) VALUE " NAME            ".
       01  T301.
           02 T31 PIC X(8) VALUE "GARNO   ".
           02 T32 PIC X(7) VALUE " PROC  ".
           02 T33 PIC X(12) VALUE "    AMOUNT  ".
           02 T34 PIC X(9) VALUE " TRN DATE".
           02 T35 PIC X(3) VALUE " PC".
       01  T401.
           02 T41 PIC X(8) VALUE "GARNO   ".
           02 T42 PIC X(12) VALUE "   AMOUNT   ".
           02 T43 PIC X(2) VALUE "PC".
           02 T44 PIC X(10) VALUE "  TRN DATE".
           02 T45 PIC X(11) VALUE "   ENT DATE".
       01  T501.
           02 T51 PIC X(8) VALUE "GARNO   ".
           02 T52 PIC X(7) VALUE " PROC  ".
           02 T53 PIC X(12) VALUE "    AMOUNT  ".
           02 T54 PIC X(3) VALUE " PC".
           02 T56 PIC X(10) VALUE "  TRN DATE".
           02 T57 PIC X(11) VALUE "   ENT DATE".
       01  T601.
           02 T61 PIC X(8) VALUE "GARNO   ".
           02 T62 PIC X(17) VALUE " NAME            ".
           02 T63 PIC X(7) VALUE " PROC  ".
           02 T64 PIC X(11) VALUE "    AMOUNT ".
           02 T65 PIC X(4) VALUE "  PC".
           02 T66 PIC X(10) VALUE "  TRN DATE".
           02 T67 PIC X(11) VALUE "   ENT DATE".
       01  L101.
           02 L11 PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 L12 PIC X(7).
           02 FILLER PIC X VALUE SPACE.
           02 L13 PIC Z,ZZ9.99CR.
       01  L201.
           02 L21 PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 L22 PIC X(7).
           02 FILLER PIC X VALUE SPACE.
           02 L23 PIC Z,ZZ9.99CR.
           02 FILLER PIC X VALUE SPACE.
           02 L24 PIC X(16).
       01  L301.
           02 L31 PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 L32 PIC X(7).
           02 FILLER PIC X VALUE SPACE.
           02 L33 PIC Z,ZZ9.99CR.
           02 FILLER PIC X VALUE SPACE.
           02 L34 PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 L35 PIC XX.
       01  L401.
           02 L41 PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 L42 PIC Z,ZZ9.99CR.
           02 FILLER PIC X VALUE SPACE.
           02 L43 PIC XXX.
           02 FILLER PIC X VALUE SPACE.
           02 L44 PIC X(8).
           02 FILLER PIC XXX VALUE SPACE.
           02 L45 PIC X(8).
       01  L501.
           02 L51 PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 L52 PIC X(7).
           02 FILLER PIC X VALUE SPACE.
           02 L53 PIC Z,ZZ9.99CR.
           02 FILLER PIC X VALUE SPACE.
           02 L54 PIC XXX.
           02 FILLER PIC X VALUE SPACE.
           02 L55 PIC X(8).
           02 FILLER PIC XXX VALUE SPACE.
           02 L56 PIC X(8).
       01  L601.
           02 L61 PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 L62 PIC X(16).
           02 FILLER PIC X VALUE SPACE.
           02 L63 PIC X(7).
           02 FILLER PIC X VALUE SPACE.
           02 L64 PIC Z,ZZ9.99CR.
           02 FILLER PIC X VALUE SPACE.
           02 L65 PIC XXX.
           02 FILLER PIC X VALUE SPACE.
           02 L66 PIC X(8).
           02 FILLER PIC XXX VALUE SPACE.
           02 L67 PIC X(8).
       01  CHAR-CNT01.
           02 CHAR-CNT PIC 99999 OCCURS 999 TIMES.
       01  CHAR-AMT01.
           02 CHAR-AMT PIC S9(8)V99 OCCURS 999 TIMES.
       01  CNT-TOT01.
           02 CNT-TOT PIC 99999 OCCURS 999 TIMES.
       01  AMT-TOT01.
           02 AMT-TOT PIC S9(8)V99 OCCURS 999 TIMES.
       01  LINE-2.
           02 L2F1 PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 L2F2 PIC X(16).
           02 FILLER PIC X VALUE SPACE.
           02 L2F3 PIC X(6).
           02 FILLER PIC X VALUE SPACE.
           02 L2F4 PIC XX.
           02 FILLER PIC X VALUE SPACE.
           02 L2F41 PIC XX.
           02 FILLER PIC X VALUE SPACE.
           02 L2F42 PIC X(4).
           02 FILLER PIC X VALUE SPACE.
           02 L2F5 PIC Z,ZZZ.99CR.
           02 FILLER PIC X VALUE SPACE.
           02 L2F51 PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 L2F6 PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 L2F7 PIC X(8).
       01 LINE-3.
           02 F31 PIC X VALUE "(".
           02 L3F1 PIC ZZZ.
           02 F32 PIC XX VALUE ") ".
           02 L3F2 PIC X(18).
           02 F23 PIC X(12) VALUE SPACE.
           02 L3F3 PIC ZZZ9.
           02 F24 PIC XXX VALUE SPACE.
           02 L3F4 PIC ZZ,ZZZ,ZZ9.99CR.
       01  LINE-4.
           02 F41 PIC X(11).
           02 L4F1 PIC X(19).
           02 F42 PIC X(4) VALUE SPACE.
           02 L4F2 PIC ZZ,ZZ9.
           02 F43 PIC XXX VALUE SPACE.
           02 L4F3 PIC ZZ,ZZZ,ZZ9.99CR.
       01 LINE-5.
           05 F51 PIC X(4) VALUE SPACE.
           05 L5F1 PIC X(18) VALUE "PAYORCODE         ".
           05 F52 PIC X(14) VALUE SPACE.
           05 L5F3 PIC X(6) VALUE "NUMBER".
           05 F54 PIC X(10) VALUE SPACE.
           05 L5F4 PIC X(6) VALUE "AMOUNT".
       01  LINE-6.
           02 F61 PIC X(8) VALUE "ACCT. # ".
           02 F62 PIC X(16) VALUE "  GUARANTOR NAME".
           02 F64 PIC X(6) VALUE " CLAIM".
           02 F65 PIC X(3) VALUE " PC".
           02 F651 PIC X(4) VALUE "  DR".
           02 F652 PIC X(7) VALUE " PROC  ".
           02 F66 PIC X(13) VALUE "       AMOUNT".
           02 FILLER PIC X VALUE SPACE.
           02 F67 PIC X(8) VALUE "CHR DATE".
           02 FILLER PIC X VALUE SPACE.
           02 F68 PIC X(10) VALUE "ENTRY DATE".
           02 FILLER PIC X VALUE SPACE.
           02 F69 PIC X(4) VALUE "TIME".
       01  HOLD-DATE PIC X(8).
       01  TEST-DATE.
           02 T-CC PIC XX.
           02 T-YY PIC XX.
           02 T-MM PIC XX.
           02 T-DD PIC XX.
       01  INPUT-DATE.
           02 T-MM PIC XX.
           02 T-DD PIC XX.
           02 T-CC PIC XX.
           02 T-YY PIC XX.
       01  DISPLAY-DATE.
           02 T-MM PIC XX.
           02 FILLER PIC X VALUE "/".
           02 T-DD PIC XX.
           02 FILLER PIC X VALUE "/".
           02 T-CC PIC XX.
           02 T-YY PIC XX.
       01  X PIC 999.
       01  Y PIC 9999.
       01  NUM3 PIC 999.
       01  Z PIC 999.
       01  PARTCNT PIC 9999 VALUE 0.
       01  PARTCHAR PIC S9(8)V99 VALUE 0.
       01  ADJ-CNT PIC 9999.
       01  ANS PIC X(20).
       01  ADJ-AMT PIC S9(8)V99.
       01  DATE-S.
           02 S-CC PIC XX.
           02 S-YY PIC XX.
           02 S-MM PIC XX.
           02 S-DD PIC XX.
       01  TODAY-T.
           02 TT-CC PIC XX.
           02 TT-YY PIC XX.
           02 TT-MM PIC XX.
           02 TT-DD PIC XX.
       01  ENTER01.
           02 LOW-ENTER PIC 9(8).
           02 HIGH-ENTER PIC 9(8).
       01  DATE01.
           02 LOW-DATE PIC 9(8).
           02 HIGH-DATE PIC 9(8).
       01  PCNUM PIC 999.
       01  LISTIT PIC X.
       01  FORMOUT PIC 9.
       01  FLAG PIC 9.
       01  CNTR PIC 99 VALUE 0.
       PROCEDURE DIVISION.
       0005-START.
           PERFORM B1 VARYING Y FROM 1 BY 1 UNTIL Y > 999.
           OPEN INPUT CHARFILE INSFILE INFILE.
           OPEN OUTPUT OUT.
           MOVE ZERO TO CD-DATE-E.
       A1. READ INFILE AT END GO TO R20.
           IF INFILE01 = "1" MOVE "1" TO LISTIT
           ELSE MOVE "0" TO LISTIT.
           READ INFILE AT END GO TO R20.
           IF LISTIT = "1" MOVE INFILE01 TO FORMOUT
           ELSE MOVE 0 TO FORMOUT.
           MOVE INFILE01 TO FORMOUT.
           READ INFILE AT END GO TO R20.
           MOVE INFILE01 TO ENTER01.
           READ INFILE AT END GO TO R20.
           MOVE INFILE01 TO DATE01.
           READ INFILE AT END GO TO R20.
           MOVE INF1 TO PCNUM.
           IF FORMOUT = 1 MOVE T101 TO TITLE01.
           IF FORMOUT = 2 MOVE T201 TO TITLE01.
           IF FORMOUT = 3 MOVE T301 TO TITLE01.
           IF FORMOUT = 4 MOVE T401 TO TITLE01.
           IF FORMOUT = 5 MOVE T501 TO TITLE01.
           IF FORMOUT = 6 MOVE T601 TO TITLE01.
       P1. READ CHARFILE AT END GO TO P5.
           IF ( CD-DATE-E < LOW-ENTER OR > HIGH-ENTER )
           OR ( CD-DATE-T < LOW-DATE OR > HIGH-DATE )
           OR ( PCNUM NOT = 0 AND CD-PAYCODE NOT = PCNUM ) GO TO P1.
           MOVE CD-DATE-E TO TEST-DATE HOLD-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           MOVE DISPLAY-DATE TO L2F6
           WRITE OUT01 FROM TITLE01.
           GO TO P4.
       P2. READ CHARFILE AT END GO TO P5.
           IF ( CD-DATE-E < LOW-ENTER OR > HIGH-ENTER )
           OR ( CD-DATE-T < LOW-DATE OR > HIGH-DATE )
           OR ( PCNUM NOT = 0 AND CD-PAYCODE NOT = PCNUM ) GO TO P2.
       P4. IF CD-DATE-E NOT = HOLD-DATE PERFORM A3
           WRITE OUT01 FROM TITLE01 AFTER 2.
           MOVE CD-PAYCODE TO X.
           ADD 1 TO CHAR-CNT(X) ADD CD-AMOUNT TO CHAR-AMT(X).
           IF FORMOUT = 0 GO TO P2.
           GO TO L1 L2 L3 L4 L5 L6 DEPENDING ON FORMOUT.
       L1.
           MOVE CD-KEY8 TO L11
           MOVE CD-PROC TO L12
           MOVE CD-AMOUNT TO L13
           WRITE OUT01 FROM L101
           GO TO P7.
       L2.
           MOVE CD-KEY8 TO L21
           MOVE CD-PROC TO L22
           MOVE CD-AMOUNT TO L23
           MOVE CD-NAME TO L24
           WRITE OUT01 FROM L201
           GO TO P7.
       L3.
           MOVE CD-KEY8 TO L31
           MOVE CD-PROC TO L32
           MOVE CD-AMOUNT TO L33
           MOVE CD-DATE-T TO TEST-DATE
           MOVE CORR TEST-DATE TO INPUT-DATE
           MOVE INPUT-DATE TO L34
           MOVE CD-PAYCODE                 TO L35
           WRITE OUT01 FROM L301
           GO TO P7.
       L4.
           MOVE CD-KEY8 TO L41
           MOVE CD-AMOUNT TO L42
           MOVE CD-PAYCODE                 TO L43
           MOVE CD-DATE-T TO TEST-DATE
           MOVE CORR TEST-DATE TO INPUT-DATE
           MOVE INPUT-DATE TO L44
           MOVE CD-DATE-E TO TEST-DATE
           MOVE CORR TEST-DATE TO INPUT-DATE
           MOVE INPUT-DATE TO L45
           WRITE OUT01 FROM L401
           GO TO P7.
       L5.
           MOVE CD-KEY8 TO L51
           MOVE CD-PROC TO L52
           MOVE CD-AMOUNT TO L53
           MOVE CD-PAYCODE                 TO L54
           MOVE CD-DATE-T TO TEST-DATE
           MOVE CORR TEST-DATE TO INPUT-DATE
           MOVE INPUT-DATE TO L55
           MOVE CD-DATE-E TO TEST-DATE
           MOVE CORR TEST-DATE TO INPUT-DATE
           MOVE INPUT-DATE TO L56
           WRITE OUT01 FROM L501
           GO TO P7.
       L6.
           MOVE CD-KEY8 TO L61
           MOVE CD-NAME TO L62
           MOVE CD-PROC TO L63
           MOVE CD-AMOUNT TO L64
           MOVE CD-PAYCODE TO L65
           MOVE CD-DATE-T TO TEST-DATE
           MOVE CORR TEST-DATE TO INPUT-DATE
           MOVE INPUT-DATE TO L66
           MOVE CD-DATE-E TO TEST-DATE
           MOVE CORR TEST-DATE TO INPUT-DATE
           MOVE INPUT-DATE TO L67
           WRITE OUT01 FROM L601
           GO TO P7.
       P7. ADD 1 TO CNTR.
           IF CNTR > 10 WRITE OUT01 FROM TITLE01 AFTER 2
           MOVE 0 TO CNTR. GO TO P2.
       A3.
           WRITE OUT01 FROM LINE-5 AFTER 2.
           MOVE 0 TO ADJ-CNT ADJ-AMT.
           MOVE L2F6 TO L4F1.
           PERFORM B3 VARYING Y FROM 1 BY 1 UNTIL Y > 999.
           MOVE ALL "*" TO OUT01.
           WRITE OUT01.
           MOVE "TOTAL ADJ." TO F41.
           MOVE ADJ-CNT TO L4F2
           MOVE ADJ-AMT TO L4F3.
           WRITE OUT01 FROM LINE-4.
           COMPUTE ADJ-CNT = PARTCNT - ADJ-CNT.
           COMPUTE ADJ-AMT = PARTCHAR - ADJ-AMT.
           MOVE "TOTAL CHRG" TO F41.
           MOVE ADJ-CNT TO L4F2
           MOVE ADJ-AMT TO L4F3.
           WRITE OUT01 FROM LINE-4.
           MOVE ALL "*" TO OUT01.
           WRITE OUT01.
           MOVE PARTCNT TO L4F2
           MOVE PARTCHAR TO L4F3
           MOVE "TOTALS FOR" TO F41
           WRITE OUT01 FROM LINE-4
           MOVE ALL " -" TO OUT01.
           WRITE OUT01.
           WRITE OUT01.
           MOVE CD-DATE-E TO HOLD-DATE TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           MOVE DISPLAY-DATE TO L2F6.
           MOVE 0 TO PARTCNT PARTCHAR ADJ-CNT ADJ-AMT.
       B1. MOVE 0 TO CHAR-CNT(Y) CHAR-AMT(Y) CNT-TOT(Y) AMT-TOT(Y).
       B2. MOVE 0 TO CHAR-CNT(Y) CHAR-AMT(Y).
       B3. IF CHAR-CNT(Y) > 0
           MOVE CHAR-CNT(Y) TO L3F3
           MOVE CHAR-AMT(Y) TO L3F4
           MOVE Y TO L3F1
           COMPUTE NUM3 = Y
           MOVE NUM3 TO INS-KEY
           MOVE "***BAD CODE***" TO INS-NAME
           READ INSFILE
           MOVE INS-NAME TO L3F2
           ADD CHAR-CNT(Y) TO PARTCNT CNT-TOT(Y)
           ADD CHAR-AMT(Y) TO PARTCHAR AMT-TOT(Y)
           WRITE OUT01 FROM LINE-3.
           IF Y > 9 AND < 18
           ADD CHAR-CNT(Y) TO ADJ-CNT
           ADD CHAR-AMT(Y) TO ADJ-AMT.
           MOVE 0 TO CHAR-CNT(Y) CHAR-AMT(Y).
       B4. IF CNT-TOT(Y) > 0
           ADD CNT-TOT(Y) TO PARTCNT ADD AMT-TOT(Y) TO PARTCHAR
           MOVE CNT-TOT(Y) TO L3F3
           MOVE AMT-TOT(Y) TO L3F4
           MOVE Y TO L3F1
           COMPUTE NUM3 = Y
           MOVE NUM3 TO INS-KEY
           MOVE "***BAD CODE***" TO INS-NAME
           READ INSFILE
           MOVE INS-NAME TO L3F2
           WRITE OUT01 FROM LINE-3.
           IF (Y > 9 AND < 20 ) AND ( Y NOT = 18 )
           ADD CNT-TOT(Y) TO ADJ-CNT
           ADD AMT-TOT(Y) TO ADJ-AMT.
       P5. PERFORM A3.
           WRITE OUT01 FROM LINE-5 AFTER 2.
           MOVE 0 TO ADJ-CNT ADJ-AMT PARTCNT PARTCHAR.
           MOVE "ALL REC " TO L4F1.
           PERFORM B4 VARYING Y FROM 1 BY 1 UNTIL Y > 999.
           MOVE ALL "*" TO OUT01.
           WRITE OUT01.
           MOVE "TOTAL ADJ." TO F41.
           MOVE ADJ-CNT TO L4F2
           MOVE ADJ-AMT TO L4F3.
           WRITE OUT01 FROM LINE-4.
           COMPUTE ADJ-CNT = PARTCNT - ADJ-CNT.
           COMPUTE ADJ-AMT = PARTCHAR - ADJ-AMT.
           MOVE "TOTAL CHRG" TO F41.
           MOVE ADJ-CNT TO L4F2
           MOVE ADJ-AMT TO L4F3.
           WRITE OUT01 FROM LINE-4.
           MOVE ALL " -" TO OUT01.
           WRITE OUT01.
           MOVE PARTCNT TO L4F2
           MOVE PARTCHAR TO L4F3
           MOVE "TOTALS FOR" TO F41
           WRITE OUT01 FROM LINE-4
           MOVE ALL "*" TO OUT01.
           WRITE OUT01.
           WRITE OUT01.
       R20.
           CLOSE INSFILE OUT.
           STOP RUN.
