      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE
      *          GNU General Public License 3
      *
      *  FLAG / Paycode mapping used in A1:
      *    926  QMM26    paycode 009  ACR registry CSV only
      *    364  Mea 364  paycode 010  ACR registry CSV only
      *    405  Mea 405  paycode 012  G-codes only
      *    406  Mea 406  paycode 013  G-codes only
      *    914  364+405  paycode 014  ACR CSV (364) + G-codes (405)
      *                              CD-QP1=364, CD-QP2=405
      *    915  364+406  paycode 015  ACR CSV (364) + G-codes (406)
      *                              CD-QP1=364, CD-QP2=406
      *    916  405+406  paycode 016  G-codes for both 405 and 406
      *                              CD-QP1=405, CD-QP2=406
      *    917  364+405+406 paycode 017  ACR CSV (364) + G-codes (405)
      *                              + G-codes (406)
      *                              CD-QP1=364, CD-QP2=405, CD-QP3=406
      *----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. mea004.
       AUTHOR. S WAITE.
       DATE-COMPILED. TODAY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARFILE  ASSIGN TO "S30"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CHARFILE-KEY
               LOCK MODE MANUAL.

           SELECT FILEOUT   ASSIGN TO "S35"
               ORGANIZATION LINE SEQUENTIAL.

           SELECT CLAIMFILE ASSIGN TO "S40"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CLAIM-KEY
               LOCK MODE MANUAL.

           SELECT GARFILE   ASSIGN TO "S45"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS G-GARNO
               ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
               LOCK MODE MANUAL.

           SELECT FILEOUT2  ASSIGN TO "S50"
               ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  CHARFILE.
           COPY "charfile.cpy" IN "C:\Users\sid\cms\copylib\rri".

       FD  FILEOUT.
       01  FILEOUT01             PIC X(80).

       FD  FILEOUT2.
       01  FILEOUT201            PIC X(160).

       FD  CLAIMFILE.
       01  CLAIM01.
           02 CLAIM-KEY          PIC X.
           02 CLAIMNO            PIC 9(6).

       FD  GARFILE.
           COPY "garfile.cpy" IN "C:\Users\sid\cms\copylib\rri".

       WORKING-STORAGE SECTION.

       01  CHARBACK01.
           02 CHARBACK-KEY.
               03 BK-KEY8        PIC X(8).
               03 BK-KEY3        PIC XXX.
           02 BK-PATID           PIC X(8).
           02 BK-CLAIM           PIC X(6).
           02 BK-SERVICE         PIC X.
           02 BK-DIAG            PIC X(7).
           02 BK-PROC.
               03 BK-PROC0       PIC X(4).
               03 BK-PROC1       PIC X(5).
               03 BK-PROC2       PIC XX.
           02 BK-MOD2            PIC XX.
           02 BK-MOD3            PIC XX.
           02 BK-MOD4            PIC XX.
           02 BK-AMOUNT          PIC S9(4)V99.
           02 BK-DOCR            PIC X(3).
           02 BK-DOCP            PIC X(2).
           02 BK-PAYCODE         PIC XXX.
           02 BK-REC-STAT        PIC X.
           02 BK-WORK            PIC XX.
           02 BK-DAT1            PIC X(8).
           02 BK-RESULT          PIC X.
           02 BK-ACT             PIC X.
           02 BK-SORCREF         PIC X.
           02 BK-COLLT           PIC X.
           02 BK-AUTH            PIC X.
           02 BK-PAPER           PIC X.
           02 BK-PLACE           PIC X.
           02 BK-NAME            PIC X(24).
           02 BK-ESPDT           PIC X.
           02 BK-DATE-T          PIC X(8).
           02 BK-DATE-E          PIC X(8).
           02 BK-ORDER           PIC X(6).
           02 BK-DX2             PIC X(7).
           02 BK-DX3             PIC X(7).
           02 BK-DATE-A          PIC X(8).
           02 BK-ACC-TYPE        PIC X.
           02 BK-DATE-M          PIC X(8).
           02 BK-ASSIGN          PIC X.
           02 BK-NEIC-ASSIGN     PIC X.
           02 BK-DX4             PIC X(7).
           02 BK-QP1             PIC XX.
           02 BK-QP2             PIC XX.
           02 BK-QP3             PIC XX.
           02 FILLER             PIC X.
           02 BK-DX6             PIC X(7).
           02 BK-FUTURE          PIC X(6).

       01  XYZ                   PIC 999.
       01  HOLD8                 PIC X(8).
       01  FLAG                  PIC 999.
       01  HOLD-ID               PIC X(11).
       01  X-PROC                PIC X(11).
       01  PROC-HOLD             PIC X(5).
       01  ALF1                  PIC X.

       01  NPI-TABLE.
           05 NPI-ENTRY OCCURS 15 TIMES INDEXED BY NPI-IDX.
               10 NPI-TBL-KEY    PIC X(2).
               10 NPI-TBL-NUMBER PIC X(10).

       01  WS-LOOKUP-KEY         PIC X(2).
       01  WS-NPI-RESULT         PIC X(10).
       01  WS-FOUND-FLAG         PIC X VALUE 'N'.

       PROCEDURE DIVISION.

      *----------------------------------------------------------------
       0005-START.
           OPEN I-O CHARFILE CLAIMFILE.
           OPEN OUTPUT FILEOUT FILEOUT2.
           OPEN INPUT GARFILE.
           MOVE SPACE TO CHARFILE-KEY.
           MOVE "A" TO CLAIM-KEY.
           READ CLAIMFILE WITH LOCK
               INVALID KEY
                   DISPLAY "BAD CLAIM #"
                   ACCEPT ALF1
                   GO TO P2
           END-READ
           PERFORM INIT-NPI-TABLE
           MOVE SPACE TO CHARFILE-KEY.

      *----------------------------------------------------------------
       P0.
           START CHARFILE KEY NOT < CHARFILE-KEY INVALID
               GO TO P2
           END-START.

      *----------------------------------------------------------------
       P1.
           READ CHARFILE NEXT WITH LOCK
               AT END GO TO P2
           END-READ

           IF NOT (CD-PAYCODE = "009" OR "010" OR "012" OR "013"
                   OR "014" OR "015" OR "016" OR "017")
               GO TO P1
           END-IF

           MOVE CHARFILE01 TO CHARBACK01
           MOVE CD-PROC1 TO PROC-HOLD
           MOVE 'N' TO WS-FOUND-FLAG

           PERFORM VARYING NPI-IDX FROM 1 BY 1
               UNTIL NPI-IDX > 15 OR WS-FOUND-FLAG = 'Y'
               IF NPI-TBL-KEY(NPI-IDX) = CD-DOCP
                   MOVE NPI-TBL-NUMBER(NPI-IDX) TO WS-NPI-RESULT
                   MOVE 'Y' TO WS-FOUND-FLAG
               END-IF
           END-PERFORM

      *--- QMM26 / Paycode 009 -----------------------------------------
           IF CD-PAYCODE = "009"
               MOVE SPACE TO FILEOUT01
               STRING "M26 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               MOVE 926 TO FLAG
               PERFORM GET-INS
               MOVE G-PRINS TO CD-PAYCODE
               REWRITE CHARFILE01
               UNLOCK CHARFILE RECORD
               PERFORM A1 THRU A1-EXIT
               GO TO P0
           END-IF

      *--- Measure 364 / Paycode 010 -----------------------------------
           IF CD-PAYCODE = "010"
               MOVE SPACE TO FILEOUT01
               STRING "364 " CD-PROC1 " " CD-DATE-T " " CD-NAME
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               MOVE 364 TO FLAG
               PERFORM GET-INS
               MOVE G-PRINS TO CD-PAYCODE
               REWRITE CHARFILE01
               UNLOCK CHARFILE RECORD
               PERFORM A1 THRU A1-EXIT
               GO TO P0
           END-IF

      *--- Measure 405 / Paycode 012 -----------------------------------
           IF CD-PAYCODE = "012"
               MOVE SPACE TO FILEOUT01
               STRING "405 " CD-PROC1 " " CD-DATE-T " " CD-NAME
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               MOVE 405 TO FLAG
               MOVE "003" TO CD-PAYCODE
               REWRITE CHARFILE01
               UNLOCK CHARFILE RECORD
               PERFORM A1 THRU A1-EXIT
               GO TO P0
           END-IF

      *--- Measure 406 / Paycode 013 -----------------------------------
           IF CD-PAYCODE = "013"
               MOVE SPACE TO FILEOUT01
               STRING "406 " CD-PROC1 " " CD-DATE-T " " CD-NAME
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               MOVE 406 TO FLAG
               MOVE "003" TO CD-PAYCODE
               REWRITE CHARFILE01
               UNLOCK CHARFILE RECORD
               PERFORM A1 THRU A1-EXIT
               GO TO P0
           END-IF

      *--- Hybrid 364+405 / Paycode 014 --------------------------------
      *    CD-QP1 = Measure 364 response
      *    CD-QP2 = Measure 405 response
           IF CD-PAYCODE = "014"
               MOVE SPACE TO FILEOUT01
               STRING "364/405 " CD-PROC1 " " CD-DATE-T " " CD-NAME
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               MOVE 914 TO FLAG
               PERFORM GET-INS
               MOVE G-PRINS TO CD-PAYCODE
               REWRITE CHARFILE01
               UNLOCK CHARFILE RECORD
               PERFORM A1 THRU A1-EXIT
               GO TO P0
           END-IF

      *--- Hybrid 364+406 / Paycode 015 --------------------------------
      *    CD-QP1 = Measure 364 response
      *    CD-QP2 = Measure 406 response
           IF CD-PAYCODE = "015"
               MOVE SPACE TO FILEOUT01
               STRING "364/406 " CD-PROC1 " " CD-DATE-T " " CD-NAME
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               MOVE 915 TO FLAG
               PERFORM GET-INS
               MOVE G-PRINS TO CD-PAYCODE
               REWRITE CHARFILE01
               UNLOCK CHARFILE RECORD
               PERFORM A1 THRU A1-EXIT
               GO TO P0
           END-IF

      *--- Hybrid 405+406 / Paycode 016 --------------------------------
      *    CD-QP1 = Measure 405 response
      *    CD-QP2 = Measure 406 response
           IF CD-PAYCODE = "016"
               MOVE SPACE TO FILEOUT01
               STRING "405/406 " CD-PROC1 " " CD-DATE-T " " CD-NAME
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               MOVE 916 TO FLAG
               MOVE "003" TO CD-PAYCODE
               REWRITE CHARFILE01
               UNLOCK CHARFILE RECORD
               PERFORM A1 THRU A1-EXIT
               GO TO P0
           END-IF

      *--- Hybrid 364+405+406 / Paycode 017 ---------------------------
      *    CD-QP1 = Measure 364 response
      *    CD-QP2 = Measure 405 response
      *    CD-QP3 = Measure 406 response
           IF CD-PAYCODE = "017"
               MOVE SPACE TO FILEOUT01
               STRING "364/405/406 " CD-PROC1 " " CD-DATE-T " "
                      CD-NAME DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               MOVE 917 TO FLAG
               PERFORM GET-INS
               MOVE G-PRINS TO CD-PAYCODE
               REWRITE CHARFILE01
               UNLOCK CHARFILE RECORD
               PERFORM A1 THRU A1-EXIT
               GO TO P0
           END-IF

           GO TO P1.

      *----------------------------------------------------------------
      *  A1 - Create quality code charge records and/or ACR CSV output
      *----------------------------------------------------------------
       A1.
           MOVE 0 TO XYZ

      *=== Measure 405 only (FLAG=405) =================================
           IF FLAG = 405
               PERFORM A1-405-GCODES
               GO TO A1-EXIT
           END-IF

      *=== Measure 406 only (FLAG=406) =================================
           IF FLAG = 406
               PERFORM A1-406-GCODES
               GO TO A1-EXIT
           END-IF

      *=== QMM26 only (FLAG=926) =======================================
           IF FLAG = 926
               PERFORM A1-926-CSV
               GO TO A1-EXIT
           END-IF

      *=== Measure 364 only (FLAG=364) =================================
           IF FLAG = 364
               PERFORM A1-364-CSV
               GO TO A1-EXIT
           END-IF

      *=== Hybrid 364+405 (FLAG=914) ===================================
      *    CD-QP1 drives 364 CSV; CD-QP2 drives 405 G-codes
           IF FLAG = 914
               PERFORM A1-364-CSV
               PERFORM A1-405-GCODES-QP2
               GO TO A1-EXIT
           END-IF

      *=== Hybrid 364+406 (FLAG=915) ===================================
      *    CD-QP1 drives 364 CSV; CD-QP2 drives 406 G-codes
           IF FLAG = 915
               PERFORM A1-364-CSV
               PERFORM A1-406-GCODES-QP2
               GO TO A1-EXIT
           END-IF

      *=== Hybrid 405+406 (FLAG=916) ===================================
      *    CD-QP1 drives 405 G-codes; CD-QP2 drives 406 G-codes
           IF FLAG = 916
               PERFORM A1-405-GCODES
               PERFORM A1-406-GCODES-QP2
               GO TO A1-EXIT
           END-IF

      *=== Hybrid 364+405+406 (FLAG=917) ===============================
      *    CD-QP1 drives 364 CSV; CD-QP2 drives 405 G-codes
      *    CD-QP3 drives 406 G-codes
           IF FLAG = 917
               PERFORM A1-364-CSV
               PERFORM A1-405-GCODES-QP2
               PERFORM A1-406-GCODES-QP3
               GO TO A1-EXIT
           END-IF.

       A1-EXIT.
           EXIT.

      *----------------------------------------------------------------
      *  A1-405-GCODES: Create 405 G-code charges using CD-QP1
      *----------------------------------------------------------------
       A1-405-GCODES.
           IF CD-QP1 = "1 "
               MOVE "0000G9548  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           END-IF
           IF CD-QP1 = "2 "
               MOVE SPACE TO FILEOUT01
               STRING "405 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                      " DENOMINATOR EXCEPTION!"
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               MOVE "0000G9549  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           END-IF
           IF CD-QP1 = "3 "
               MOVE SPACE TO FILEOUT01
               STRING "405 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                      " PERFORMANCE NOT MET!"
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               MOVE "0000G9550  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           END-IF
           IF CD-QP1 = "1 " OR "2 " OR "3 "
               MOVE "0000G9547  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           ELSE
               MOVE "0000G9551  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           END-IF.

      *----------------------------------------------------------------
      *  A1-405-GCODES-QP2: Create 405 G-code charges using CD-QP2
      *  Used by hybrid paycodes 014 and 917 where QP1 is used by 364
      *----------------------------------------------------------------
       A1-405-GCODES-QP2.
           IF CD-QP2 = "1 "
               MOVE "0000G9548  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           END-IF
           IF CD-QP2 = "2 "
               MOVE SPACE TO FILEOUT01
               STRING "405 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                      " DENOMINATOR EXCEPTION!"
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               MOVE "0000G9549  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           END-IF
           IF CD-QP2 = "3 "
               MOVE SPACE TO FILEOUT01
               STRING "405 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                      " PERFORMANCE NOT MET!"
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               MOVE "0000G9550  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           END-IF
           IF CD-QP2 = "1 " OR "2 " OR "3 "
               MOVE "0000G9547  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           ELSE
               MOVE "0000G9551  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           END-IF.

      *----------------------------------------------------------------
      *  A1-406-GCODES: Create 406 G-code charges using CD-QP1
      *----------------------------------------------------------------
       A1-406-GCODES.
           IF CD-QP1 = "1 "
               MOVE "0000G9554  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           END-IF
           IF CD-QP1 = "2 "
               MOVE "0000G9555  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           END-IF
           IF CD-QP1 = "3 "
               MOVE "0000G9556  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
               MOVE SPACE TO FILEOUT01
               STRING "406 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                      " PERFORMANCE NOT MET which is great"
                      " it's an inverse measure :)"
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
           END-IF
           IF CD-QP1 = "1 " OR "2 " OR "3 "
               MOVE "0000G9552  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           ELSE
               MOVE "0000G9557  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           END-IF.

      *----------------------------------------------------------------
      *  A1-406-GCODES-QP2: Create 406 G-code charges using CD-QP2
      *  Used by hybrid paycodes 015 and 916 where QP1 is used by
      *  364 or 405 respectively
      *----------------------------------------------------------------
       A1-406-GCODES-QP2.
           IF CD-QP2 = "1 "
               MOVE "0000G9554  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           END-IF
           IF CD-QP2 = "2 "
               MOVE "0000G9555  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           END-IF
           IF CD-QP2 = "3 "
               MOVE "0000G9556  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
               MOVE SPACE TO FILEOUT01
               STRING "406 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                      " PERFORMANCE NOT MET which is great"
                      " it's an inverse measure :)"
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
           END-IF
           IF CD-QP2 = "1 " OR "2 " OR "3 "
               MOVE "0000G9552  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           ELSE
               MOVE "0000G9557  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           END-IF.

      *----------------------------------------------------------------
      *  A1-406-GCODES-QP3: Create 406 G-code charges using CD-QP3
      *  Used by hybrid paycode 017 where QP1=364 and QP2=405
      *----------------------------------------------------------------
       A1-406-GCODES-QP3.
           IF CD-QP3 = "1 "
               MOVE "0000G9554  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           END-IF
           IF CD-QP3 = "2 "
               MOVE "0000G9555  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           END-IF
           IF CD-QP3 = "3 "
               MOVE "0000G9556  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
               MOVE SPACE TO FILEOUT01
               STRING "406 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                      " PERFORMANCE NOT MET which is great"
                      " it's an inverse measure :)"
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
           END-IF
           IF CD-QP3 = "1 " OR "2 " OR "3 "
               MOVE "0000G9552  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           ELSE
               MOVE "0000G9557  " TO X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000"
                      DELIMITED BY SIZE INTO CHARFILE-KEY
           END-IF.

      *----------------------------------------------------------------
      *  A1-926-CSV: ACR registry CSV output for QMM26 using CD-QP1
      *----------------------------------------------------------------
       A1-926-CSV.
           IF CD-QP1 = "1 "
               MOVE SPACE TO FILEOUT201
               STRING CD-DATE-T "," G-ACCT "," G-DOB "," G-SEX ","
                      G-PRINS "," G-PRIPOL ",QMM26," CD-PROC1 ","
                      CD-DIAG ",PM002," CD-QP1 "," CD-QP2 ","
                      WS-NPI-RESULT ",1" CD-VISITNO
                      DELIMITED BY SIZE INTO FILEOUT201
               WRITE FILEOUT201
               MOVE SPACE TO FILEOUT01
               STRING "QMM26 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                      " PERFORMANCE MET"
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
           END-IF
           IF CD-QP1 = "2 "
               MOVE SPACE TO FILEOUT201
               STRING CD-DATE-T "," G-ACCT "," G-DOB "," G-SEX ","
                      G-PRINS "," G-PRIPOL ",QMM26," CD-PROC1 ","
                      CD-DIAG ",PM102," CD-QP1 "," CD-QP2 ","
                      WS-NPI-RESULT ",1" CD-VISITNO
                      DELIMITED BY SIZE INTO FILEOUT201
               WRITE FILEOUT201
               MOVE SPACE TO FILEOUT01
               STRING "QMM26 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                      " PERFORMANCE MET"
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
           END-IF
           IF CD-QP1 = "3 "
               MOVE SPACE TO FILEOUT201
               STRING CD-DATE-T "," G-ACCT "," G-DOB "," G-SEX ","
                      G-PRINS "," G-PRIPOL ",QMM26," CD-PROC1 ","
                      CD-DIAG ",PM202," CD-QP1 "," CD-QP2 ","
                      WS-NPI-RESULT ",1" CD-VISITNO
                      DELIMITED BY SIZE INTO FILEOUT201
               WRITE FILEOUT201
               MOVE SPACE TO FILEOUT01
               STRING "QMM26 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                      " NEGATIVE FOR AAA"
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
           END-IF
           IF CD-QP1 = "4 "
               MOVE SPACE TO FILEOUT201
               STRING CD-DATE-T "," G-ACCT "," G-DOB "," G-SEX ","
                      G-PRINS "," G-PRIPOL ",QMM26," CD-PROC1 ","
                      CD-DIAG ",PNM02," CD-QP1 "," CD-QP2 ","
                      WS-NPI-RESULT ",1" CD-VISITNO
                      DELIMITED BY SIZE INTO FILEOUT201
               WRITE FILEOUT201
               MOVE SPACE TO FILEOUT01
               STRING "QMM26 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                      " *** PERFORMANCE NOT MET!"
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
           END-IF
           IF CD-QP1 = "5 "
               MOVE SPACE TO FILEOUT201
               STRING CD-DATE-T "," G-ACCT "," G-DOB "," G-SEX ","
                      G-PRINS "," G-PRIPOL ",QMM26," CD-PROC1 ","
                      CD-DIAG ",PE002," CD-QP1 "," CD-QP2 ","
                      WS-NPI-RESULT ",1" CD-VISITNO
                      DELIMITED BY SIZE INTO FILEOUT201
               WRITE FILEOUT201
               MOVE SPACE TO FILEOUT01
               STRING "QMM26 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                      " *** DENOMINATOR EXCEPTION!"
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
           END-IF.

      *----------------------------------------------------------------
      *  A1-364-CSV: ACR registry CSV output for Measure 364
      *  Always uses CD-QP1 (even in hybrid paycodes, 364 owns QP1)
      *----------------------------------------------------------------
       A1-364-CSV.
           IF CD-QP1 = "1 "
               MOVE SPACE TO FILEOUT201
               STRING CD-DATE-T "," G-ACCT "," G-DOB "," G-SEX ","
                      G-PRINS "," G-PRIPOL ",364," CD-PROC1 ","
                      CD-DIAG ",G9345," CD-QP1 "," CD-QP2 ","
                      WS-NPI-RESULT ",1" CD-VISITNO
                      DELIMITED BY SIZE INTO FILEOUT201
               WRITE FILEOUT201
               MOVE SPACE TO FILEOUT01
               STRING "364 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                      " PERFORMANCE MET"
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
           END-IF
           IF CD-QP1 = "2 "
               MOVE SPACE TO FILEOUT201
               STRING CD-DATE-T "," G-ACCT "," G-DOB "," G-SEX ","
                      G-PRINS "," G-PRIPOL ",364," CD-PROC1 ","
                      CD-DIAG ",G9755," CD-QP1 "," CD-QP2 ","
                      WS-NPI-RESULT ",1" CD-VISITNO
                      DELIMITED BY SIZE INTO FILEOUT201
               WRITE FILEOUT201
               MOVE SPACE TO FILEOUT01
               STRING "364 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                      " DENOMINATOR EXCLUSION M1018"
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
           END-IF
           IF CD-QP1 = "3 "
               MOVE SPACE TO FILEOUT201
               STRING CD-DATE-T "," G-ACCT "," G-DOB "," G-SEX ","
                      G-PRINS "," G-PRIPOL ",364," CD-PROC1 ","
                      CD-DIAG ",G9347," CD-QP1 "," CD-QP2 ","
                      WS-NPI-RESULT ",1" CD-VISITNO
                      DELIMITED BY SIZE INTO FILEOUT201
               WRITE FILEOUT201
               MOVE SPACE TO FILEOUT01
               STRING "364 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                      " PERFORMANCE NOT MET"
                      DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
           END-IF.

      *----------------------------------------------------------------
       B1.
           ADD 1 TO XYZ
           MOVE XYZ TO CD-KEY3
           MOVE BK-KEY8 TO CD-KEY8
           READ CHARFILE INVALID KEY
               MOVE CHARFILE-KEY TO HOLD-ID
               GO TO B2
           END-READ
           IF XYZ = 999
               DISPLAY "THERE ARE 999 CHARGE TRANSACTIONS"
               ACCEPT ALF1
               GO TO P2
           END-IF
           GO TO B1.

       B2.
           MOVE CHARBACK01 TO CHARFILE01
           MOVE HOLD-ID    TO CHARFILE-KEY
           MOVE X-PROC     TO CD-PROC
           MOVE "0000000"  TO CD-DX2 CD-DX3 CD-DX4
           MOVE 0          TO CD-AMOUNT
           MOVE 003        TO CD-PAYCODE
           MOVE SPACE      TO CD-MOD2 CD-MOD3 CD-MOD4
           ADD 1 TO CLAIMNO
           MOVE CLAIMNO    TO CD-CLAIM
           WRITE CHARFILE01.

      *----------------------------------------------------------------
       GET-INS.
           MOVE CD-KEY8 TO G-GARNO
           READ GARFILE INVALID
               DISPLAY "GARNO NOT AVAILABLE FOR SOME UNKNOWN REASON"
               DISPLAY "PLEASE RECORD THIS FACT " CD-KEY8
               GO TO P1
           END-READ.

      *----------------------------------------------------------------
       INIT-NPI-TABLE.
           PERFORM VARYING NPI-IDX FROM 1 BY 1 UNTIL NPI-IDX > 15
               MOVE SPACES TO NPI-TBL-KEY(NPI-IDX)
               MOVE SPACES TO NPI-TBL-NUMBER(NPI-IDX)
           END-PERFORM
           MOVE '06' TO NPI-TBL-KEY(1)
           MOVE '1194737833' TO NPI-TBL-NUMBER(1)
           MOVE '08' TO NPI-TBL-KEY(2)
           MOVE '1407002355' TO NPI-TBL-NUMBER(2)
           MOVE '09' TO NPI-TBL-KEY(3)
           MOVE '1174889182' TO NPI-TBL-NUMBER(3)
           MOVE '10' TO NPI-TBL-KEY(4)
           MOVE '1487884953' TO NPI-TBL-NUMBER(4).

      *----------------------------------------------------------------
       P2.
           REWRITE CLAIM01
           CLOSE CHARFILE CLAIMFILE FILEOUT FILEOUT2 GARFILE.
           STOP RUN.
