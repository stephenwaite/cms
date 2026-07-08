      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @author  Claude
      * @copyright Copyright (c) 2026 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
      *
      * derived from find-cdm.  S45 holds TWO proc codes:
      *   line 1 = target proc, line 2 = companion proc.
      * codes are 11 bytes - 4 byte facility cdm, 5 byte cpt, 2 byte mod
      *
      * pass 1 tables (patid,dos,visitno) for every companion charge in
      * the date window.  pass 2 rescans for the target proc and flags:
      *   S  companion same patid, same dos, SAME visitno
      *   D  companion same patid, same dos, DIFFERENT visitno
      *   N  no companion on that dos
      * cpt parenthetical bars the pair in the same SESSION - S rows are
      * the exposure, D rows are the ones you defend.
      *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. find-cdm-pair.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARDATE ASSIGN TO "S25"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT PAYDATE ASSIGN TO "S30"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES.
           SELECT FILEOUT ASSIGN TO "S40"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT CCPROCIN ASSIGN TO "S45"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT DOCFILE ASSIGN TO "S50"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT GARFILE ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES.
           SELECT PAYCUR ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD  PAYCUR.
       COPY PAYCUR.CPY.
       FD  CHARCUR.
       COPY CHARCUR.CPY.
       FD  GARFILE.
       COPY GARFILE.CPY.
       FD  DOCFILE.
       COPY DOCFILE.CPY.
       FD  CHARDATE.
       01  CHARDATE01.
           02 LOW-CHARDATE PIC X(8).
           02 HIGH-CHARDATE PIC X(8).
       FD  PAYDATE.
       01  PAYDATE01.
           02 LOW-PAYDATE PIC X(8).
           02 HIGH-PAYDATE PIC X(8).
       FD  CCPROCIN.
       01  CCPROCIN01 PIC X(11).
       FD  FILEOUT.
       01  FILEOUT01.
           02 FO-DOCP PIC XX.
           02 FO-DUM PIC X.
           02 FO-SERVICE PIC X.
           02 FO-PROC PIC X(11).
           02 FO-DIAG PIC X(7).
           02 FO-AMOUNT PIC S9(7)V99.
           02 FO-IO PIC X.
           02 FO-NAME PIC X(24).
           02 FO-MRN PIC X(8).
           02 FO-DATE PIC X(10).
           02 FO-CKEY PIC X(12).
           02 FO-FILLER PIC X VALUE SPACE.
           02 FO-INS PIC X(3).
           02 FO-MSG PIC X(20).
      *    visit level flag - S same visit, D diff visit, N no pair
           02 FO-FIL2 PIC X VALUE SPACE.
           02 FO-FLAG PIC X.
           02 FO-FIL3 PIC X VALUE SPACE.
           02 FO-VISIT PIC X(7).
           02 FO-FIL4 PIC X VALUE SPACE.
           02 FO-CVIS PIC X(7).
       WORKING-STORAGE SECTION.
       01  PLACE-TAB01.
           02 PLACE-TAB OCCURS 26 TIMES.
              03 PL-TAB PIC X.
              03 PL-NUM PIC X.
              03 PL-NAME PIC X(22).
       01  MON-TAB-RE01.
           02 FILLER PIC X(27) VALUE "JANUARY  FEBRUARY MARCH    ".
           02 FILLER PIC X(27) VALUE "APRIL    MAY      JUNE     ".
           02 FILLER PIC X(27) VALUE "JULY     AUGUST   SEPTEMBER".
           02 FILER PIC X(27) VALUE "OCTOBER  NOVEMBER DECEMBER ".
       01  MON-TAB01 REDEFINES MON-TAB-RE01.
           02 MON-TAB PIC X(9) OCCURS 12 TIMES.
       01  PLINDX PIC 99 VALUE 0.
       01  LOW-CLAIM PIC X(6).
       01  HIGH-CLAIM PIC X(6).
       01  X PIC 99.
       01  Y PIC 99.
       01  CC-PL PIC X.
       01  FLAG PIC 9.
       01  TOT-AMOUNT PIC S9(7)V99.
       01  ALF4 PIC X.
       01  ALF5 PIC X.
      *    the two proc codes fed in on S45
       01  TARGET-PROC PIC X(11).
       01  COMPAN-PROC PIC X(11).
      *    companion charges found in the date window
       01  CO-TAB01.
           02 CO-ENT OCCURS 20000 TIMES.
              03 CO-PATID PIC X(8).
              03 CO-DATE PIC X(8).
              03 CO-VISIT PIC X(7).
       01  CO-CNT PIC 9(5) VALUE 0.
       01  CO-MAX PIC 9(5) VALUE 20000.
       01  OVFL-SW PIC X VALUE "N".
       01  SI PIC 9(5) VALUE 0.
      *    DHIT - companion same dos.  VHIT - companion same visit.
       01  DHIT-CNT PIC 9(5) VALUE 0.
       01  VHIT-CNT PIC 9(5) VALUE 0.
       01  HIT-SW PIC X VALUE "N".
       01  WS-CVIS PIC X(7).
       01  NEED-CNT PIC 9 VALUE 1.
       01  READ-CNT PIC 9(7) VALUE 0.
       01  WRIT-CNT PIC 9(7) VALUE 0.
       01  SAME-CNT PIC 9(7) VALUE 0.
       01  DIFF-CNT PIC 9(7) VALUE 0.
       01  NONE-CNT PIC 9(7) VALUE 0.
      *    CCYYMMDD in, MM/DD/CCYY out
       01  WS-DATE.
           02 WD-CCYY PIC X(4).
           02 WD-MM PIC XX.
           02 WD-DD PIC XX.
       01  FMT-DATE.
           02 FD-MM PIC XX.
           02 FILLER PIC X VALUE "/".
           02 FD-DD PIC XX.
           02 FILLER PIC X VALUE "/".
           02 FD-CCYY PIC X(4).
       PROCEDURE DIVISION.
        P0.
           OPEN INPUT DOCFILE GARFILE CHARDATE PAYDATE CHARCUR PAYCUR
                  CCPROCIN.
           OPEN OUTPUT FILEOUT.
           MOVE SPACES TO CCPROCIN01.
           READ CCPROCIN
             AT END
               DISPLAY "S45 EMPTY - NEED TARGET PROC" UPON SYSERR
               GO TO P98.
           MOVE CCPROCIN01 TO TARGET-PROC.
           MOVE SPACES TO CCPROCIN01.
           READ CCPROCIN
             AT END
               DISPLAY "S45 NEEDS 2ND LINE - COMPANION" UPON SYSERR
               GO TO P98.
           MOVE CCPROCIN01 TO COMPAN-PROC.
           READ CHARDATE.
      *    same code both lines means we need two charges on the DOS
           IF TARGET-PROC = COMPAN-PROC
              MOVE 2 TO NEED-CNT
           END-IF.
           DISPLAY "TARGET    [" TARGET-PROC "]" UPON SYSERR.
           DISPLAY "COMPANION [" COMPAN-PROC "]" UPON SYSERR.
           DISPLAY "ONLY MEDICARE? Y/N"
           ACCEPT ALF4.
           DISPLAY "ONLY PAIRS? Y/N"
           ACCEPT ALF5.
      *
      *    PASS 1 - table the companion charges
      *
        P1.
           READ CHARCUR
             AT END
               GO TO P1X.
           IF CC-PROC NOT = COMPAN-PROC
              GO TO P1.
           IF CC-DATE-T < LOW-CHARDATE OR > HIGH-CHARDATE
              GO TO P1.
           IF CO-CNT = CO-MAX
              MOVE "Y" TO OVFL-SW
              GO TO P1.
           ADD 1 TO CO-CNT.
           MOVE CC-PATID TO CO-PATID(CO-CNT).
           MOVE CC-DATE-T TO CO-DATE(CO-CNT).
           MOVE CC-VISITNO TO CO-VISIT(CO-CNT).
           GO TO P1.
        P1X.
           DISPLAY "COMPANION CHARGES TABLED " CO-CNT UPON SYSERR.
           IF OVFL-SW = "Y"
              DISPLAY "*** TABLE FULL - RESULTS INCOMPLETE ***"
                UPON SYSERR
           END-IF.
           CLOSE CHARCUR.
           OPEN INPUT CHARCUR.
      *
      *    PASS 2 - target charges, test dos then visitno
      *
        P2.
           READ CHARCUR
             AT END
               GO TO P99.
           IF CC-PROC NOT = TARGET-PROC
              GO TO P2.
           IF CC-DATE-T < LOW-CHARDATE OR > HIGH-CHARDATE
              GO TO P2.
           ADD 1 TO READ-CNT.
           MOVE 0 TO DHIT-CNT.
           MOVE 0 TO VHIT-CNT.
           MOVE SPACES TO WS-CVIS.
           MOVE 0 TO SI.
        P3.
           IF SI = CO-CNT
              GO TO P3X.
           ADD 1 TO SI.
           IF CO-PATID(SI) NOT = CC-PATID
              GO TO P3.
           IF CO-DATE(SI) NOT = CC-DATE-T
              GO TO P3.
           ADD 1 TO DHIT-CNT.
           IF WS-CVIS = SPACES
              MOVE CO-VISIT(SI) TO WS-CVIS
           END-IF.
           IF CO-VISIT(SI) NOT = CC-VISITNO
              GO TO P3.
           ADD 1 TO VHIT-CNT.
           MOVE CO-VISIT(SI) TO WS-CVIS.
           GO TO P3.
        P3X.
           MOVE "N" TO HIT-SW.
           IF DHIT-CNT NOT < NEED-CNT
              MOVE "D" TO HIT-SW
           END-IF.
           IF VHIT-CNT NOT < NEED-CNT
              MOVE "S" TO HIT-SW
           END-IF.
           IF HIT-SW = "N" AND ALF5 = "Y"
              GO TO P2.
        WRITE-FO.
           MOVE SPACE TO FILEOUT01.
           MOVE CC-PATID TO G-GARNO
           READ GARFILE
           INVALID
           MOVE SPACE TO G-GARNAME.
      *    ONLY REPORT MEDICARE ON ACCOUNT
           IF G-PRINS NOT = "003" AND ALF4 = "Y"
              GO TO P2.
           MOVE G-PRINS TO FO-INS
           MOVE G-GARNAME TO FO-NAME
           MOVE G-ACCT TO FO-MRN
           MOVE "1" TO FO-SERVICE.
           MOVE "1" TO FO-DUM.
           MOVE "1" TO FO-IO.
           MOVE CC-DOCP TO FO-DOCP
           MOVE CC-PROC TO FO-PROC
           MOVE CC-DATE-T TO WS-DATE
           MOVE WD-MM TO FD-MM
           MOVE WD-DD TO FD-DD
           MOVE WD-CCYY TO FD-CCYY
           MOVE FMT-DATE TO FO-DATE
           MOVE CC-DIAG TO FO-DIAG
           MOVE G-GARNAME TO FO-NAME
           MOVE HIT-SW TO FO-FLAG
           MOVE CC-VISITNO TO FO-VISIT
           MOVE WS-CVIS TO FO-CVIS
           STRING " " CHARCUR-KEY DELIMITED SIZE INTO FO-CKEY
           IF HIT-SW = "S"
              STRING "SAME VISIT" DELIMITED SIZE INTO FO-MSG
              ADD 1 TO SAME-CNT
           END-IF
           IF HIT-SW = "D"
              STRING "DIFF VISIT" DELIMITED SIZE INTO FO-MSG
              ADD 1 TO DIFF-CNT
           END-IF
           IF HIT-SW = "N"
              STRING "NO PAIR" DELIMITED SIZE INTO FO-MSG
              ADD 1 TO NONE-CNT
           END-IF
           IF CC-AMOUNT = 0
              STRING "CHARGE ZEROED" DELIMITED SIZE INTO FO-MSG
           END-IF
           WRITE FILEOUT01.
           ADD 1 TO WRIT-CNT.
           GO TO P2.
        P99.
           DISPLAY "TARGET CHARGES IN WINDOW " READ-CNT UPON SYSERR.
           DISPLAY "ROWS WRITTEN             " WRIT-CNT UPON SYSERR.
           DISPLAY "S SAME VISIT             " SAME-CNT UPON SYSERR.
           DISPLAY "D DIFF VISIT             " DIFF-CNT UPON SYSERR.
           DISPLAY "N NO PAIR                " NONE-CNT UPON SYSERR.
        P98.
           CLOSE DOCFILE GARFILE CHARDATE PAYDATE CHARCUR
                  PAYCUR CCPROCIN FILEOUT.
           STOP RUN.
