      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrr022.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO "S30"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT INSFILE ASSIGN TO "S35"     ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC        RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES.
           SELECT FILEOUT ASSIGN TO "S40" 
           ORGANIZATION LINE SEQUENTIAL.
           SELECT PARMNAME ASSIGN TO "S45"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT AGEDATE ASSIGN TO "S50"
           ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  INSFILE.
           copy insfile.cpy in "c:\users\sid\cms\copylib".

       FD  AGEDATE.
       01  DATE-X.
           02 YY-X PIC XXXX.
           02 MM-X PIC XX.
           02 DD-X PIC XX.

       FD PARMNAME
           DATA RECORD IS PARMNAME01.
       01  PARMNAME01 PIC X(40).

       FD  FILEOUT
           DATA RECORD FILEOUT01.
       01  FILEOUT01 PIC X(148).

       FD  FILEIN.
       01  FILEIN01.
           02 FO-PAYCODE PIC XXX.
           02 FO-DATE-T PIC X(8).
           02 FO-GARNAME PIC X(18).
           02 FILLER PIC X VALUE SPACE.
           02 FO-PRINS  PIC XXX.
           02 FILLER PIC X.
           02 FO-SEINS PIC XXX.
           02 FILLER PIC X.
           02 FO-TRINS PIC XXX.
           02 FO-PATID PIC X(8).
           02 FO-CLAIM PIC X(6).
           02 FO-DIAG PIC X(7).
           02 FO-PROC PIC X(11).
           02 FO-MOD2 PIC XX.
           02 FO-AMOUNT PIC S9(4)V99.
           02 FO-DOCP PIC X(2).
           02 FO-PG PIC X(10).
           02 FO-PP PIC X(16).
           02 FO-SP PIC X(16).
           02 FO-AGE PIC X(8).
           02 FO-PLACE PIC X.
           02 FO-KEY PIC X(11).
           02 CR-DATE PIC X(8).

       WORKING-STORAGE SECTION.
       01  LINE-1.
           02 F11 PIC X(29) VALUE "INSURANCE CLAIMS PENDING     ".
           02 L1F1 PIC X(40).
           02 F12 PIC XXX VALUE SPACE.
           02 L1F2 PIC X(17).
            02 F13 PIC XX VALUE SPACE.
             02 L1F3MM PIC XX.
             02 FILLER PIC X VALUE "/".
             02 L1F3DD PIC XX.
             02 FILLER PIC X VALUE "/".
             02 L1F3YY PIC XXXX.
           02 F14 PIC XX VALUE SPACE.
           02 F15 PIC X(5) VALUE "PAGE ".
           02 L1F4 PIC Z,ZZ9.
       01  LINE-2.
           02 L2F1 PIC X(13) VALUE "RECORD KEY   ".
           02 L2F2 PIC X(4) VALUE "NAME".
           02 FILLER PIC X(17) VALUE SPACE.
           02 L2F3 PIC X(17) VALUE "PRIMARY INSURANCE".
           02 FILLER PIC X VALUE SPACE.
           02 L2F4 PIC X(16) VALUE "SECONDARY INSUR.".
           02 FILLER PIC X(7) VALUE SPACE.
           02 L2F5 PIC X(11) VALUE "PROC       ".
           02 FILLER PIC XXX VALUE SPACE.
           02 L2F6 PIC X(7) VALUE "CLAIM  ".
           02 L2F7 PIC X(9) VALUE " AMOUNT  ".
           02 FILLER PIC X VALUE SPACE.
           02 L2F8 PIC X(9) VALUE "DATE     ".
           02 FILLER PIC XX VALUE SPACE.
           02 L2F9 PIC X(9) VALUE "CLM AGE  ".
           02 FILLER PIC X VALUE SPACE.
           02 FILLER PIC XX VALUE "DR".
           02 FILLER PIC X VALUE SPACE.
           02 FILLER PIC XX VALUE "PL".
           02 FILLER PIC X VALUE SPACE.
           02 L2F10 PIC X(9) VALUE "CARE DATE".

       01  LINE-3.
           02 L3F1 PIC X(11).
           02 F31 PIC X VALUE SPACE.
           02 L3F2 PIC X(18).
           02 F32 PIC X VALUE SPACE.
           02 L3F3 PIC X(3).
           02 F34 PIC X VALUE "/".
           02 L3F4 PIC X(3).
           02 F34A PIC X VALUE "/".
           02 L3F4A PIC X(3).
           02 F35 PIC X VALUE SPACE.
           02 L3F5 PIC X(14).
           02 F38 PIC X VALUE SPACE.
           02 L3F7 PIC X(12).
           02 FILLER PIC X(5) VALUE SPACE.
           02 L3F8 PIC X(11).
           02 L3F9 PIC XX.
           02 FILLER PIC X VALUE SPACE.
           02 L3F10 PIC X(6).
           02 F39 PIC X VALUE SPACE.
           02 L3F11 PIC Z,ZZ9.99CR.
           02 F310 PIC X VALUE SPACE.
           02 L3F12 PIC X(8).
           02 F311 PIC X(4) VALUE SPACE.
           02 L3F13 PIC X(8).
           02 F312 PIC X VALUE SPACE.
           02 L3F15 PIC XX.
           02 F314 PIC X VALUE SPACE.
           02 L3F16 PIC XX.
           02 F315 PIC X VALUE SPACE.
           02 L3F17 PIC X(8).

       01  LINE-4.
           02 F41 PIC X(11) VALUE "TOTALS FOR ".
           02 L4F1 PIC X(17).
           02 F42 PIC X(5) VALUE " FOR ".
           02 L4F2 PIC X(10).
           02 FILLER PIC XX VALUE SPACE.
           02 L4F5 PIC XXX.
           02 F43 PIC X(9) VALUE " CLAIMS: ".
           02 L4F3 PIC ZZ,ZZ9.
           02 F44 PIC X(9) VALUE " AMOUNT: ".
           02 L4F4 PIC ZZ,Z(3),ZZ9.99CR.
       01  LINE-5.
           02 F51 PIC X(39) VALUE "GRAND TOTAL 3RD PARTY  CLAIMS: ".
           02 L5F1 PIC ZZZ,ZZ9.
           02 F52 PIC X(9) VALUE " AMOUNT: ".
           02 L5F2 PIC ZZ,Z(3),ZZ9.99CR.
       01  INPUT-DATE.
           05 T-MM  PIC 99.
           05 T-DD  PIC 99.
           05 T-YY  PIC 9999.
       01  TEST-DATE.
           05 T-YY  PIC 9999.
           05 T-MM  PIC 99.
           05 T-DD  PIC 99.
        01 MON-TAB-RE01.
           02 FILLER PIC X(27) VALUE "JANUARY  FEBRUARY MARCH    ".
          02 FILLER PIC X(27) VALUE "APRIL    MAY      JUNE     ".
          02 FILLER PIC X(27) VALUE "JULY     AUGUST   SEPTEMBER".
          02 FILER PIC X(27) VALUE "OCTOBER  NOVEMBER DECEMBER ".
       01 MON-TAB01 REDEFINES MON-TAB-RE01.
           02 MON-TAB PIC X(9) OCCURS 12 TIMES.
       01  CNT-PC PIC 99999.
       01  AMT-PC PIC S9(8)V99.
       01  CNT-TOT PIC 99999.
       01  AMT-TOT PIC S9(8)V99.
       01  LINE-X PIC 99.
       01  PAGE-X PIC 9999.
       01  HOLD-CODE PIC 999.
       01  HOLD-GARNO PIC X(8).
       01 STARTFLAG PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT AGEDATE. READ AGEDATE.
           ACCEPT DATE-X FROM CENTURY-DATE.
           OPEN INPUT FILEIN.
           OPEN INPUT INSFILE PARMNAME.
           OPEN OUTPUT FILEOUT.
           MOVE DD-X TO L1F3DD MOVE MM-X TO L1F3MM
           MOVE YY-X TO L1F3YY.
           MOVE 0 TO CNT-TOT AMT-TOT.
           READ PARMNAME. MOVE PARMNAME01 TO L1F1.
       P1. READ FILEIN AT END GO TO P8.
       P1-1. MOVE FO-PAYCODE TO HOLD-CODE.
           MOVE FO-PATID TO HOLD-GARNO.
           MOVE 0 TO CNT-PC AMT-PC.
           MOVE FO-PAYCODE TO INS-KEY
           READ INSFILE INVALID MOVE "** MISSING **" TO INS-NAME.
           IF INS-KEY = "001" MOVE "***MISCODED INS***" TO INS-NAME.
           MOVE INS-NAME TO L1F2.
           MOVE FO-PAYCODE TO F12.
           MOVE 1 TO LINE-X.
           MOVE 1 TO PAGE-X.
           MOVE PAGE-X TO L1F4.
           IF STARTFLAG = 0 MOVE 1 TO STARTFLAG
           WRITE FILEOUT01 FROM LINE-1 AFTER PAGE
           ELSE WRITE FILEOUT01 FROM LINE-1 AFTER 3.
           WRITE FILEOUT01 FROM LINE-2 AFTER 2.
           GO TO A3.
       P2. READ FILEIN AT END GO TO P8.
           IF FO-PAYCODE NOT = HOLD-CODE
           PERFORM S1
           GO TO P1-1.
       A3.
           ADD 1 TO CNT-PC.
           ADD FO-AMOUNT TO AMT-PC.
           MOVE FO-DOCP TO L3F15
           MOVE FO-PLACE TO L3F16
           MOVE FO-DATE-T TO TEST-DATE.
           MOVE CORR TEST-DATE TO INPUT-DATE.
           MOVE INPUT-DATE TO L3F12.
           MOVE FO-AGE TO TEST-DATE.
           MOVE CORR TEST-DATE TO INPUT-DATE.
           MOVE INPUT-DATE TO L3F13.
           MOVE FO-KEY TO L3F1.
           MOVE FO-GARNAME TO L3F2.
           MOVE FO-PRINS TO L3F3
           MOVE FO-SEINS TO L3F4
           MOVE FO-TRINS TO L3F4A
           MOVE FO-PP TO L3F5.
           MOVE FO-SP TO L3F7.
           MOVE FO-PROC TO L3F8.
           MOVE FO-MOD2 TO L3F9
           MOVE FO-CLAIM TO L3F10.
           MOVE FO-AMOUNT TO L3F11.
           MOVE CR-DATE TO L3F17.
           IF LINE-X > 55 ADD 1 TO PAGE-X
           MOVE 0 TO LINE-X
           MOVE PAGE-X TO L1F4
           WRITE FILEOUT01 FROM LINE-1 AFTER PAGE
           WRITE FILEOUT01 FROM LINE-2 AFTER 2.
           ADD 1 TO LINE-X.
           WRITE FILEOUT01 FROM LINE-3 AFTER 1.
           GO TO P2.
       S1. MOVE CNT-PC TO L4F3.
           MOVE AMT-PC TO L4F4.
           ADD CNT-PC TO CNT-TOT.
           ADD AMT-PC TO AMT-TOT.
           MOVE HOLD-CODE TO INS-KEY
           READ INSFILE INVALID MOVE "** MISSING **" TO INS-NAME.
           MOVE INS-NAME TO L4F1.
           MOVE "ALL MONTHS" TO L4F2.
           WRITE FILEOUT01 FROM LINE-4 AFTER 2.
       P8.
           PERFORM S1
           MOVE CNT-TOT TO L5F1.
           MOVE AMT-TOT TO L5F2.
           WRITE FILEOUT01 FROM LINE-5 AFTER 2.
       P9. CLOSE  FILEIN PARMNAME
           FILEOUT.
           STOP RUN.
