      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. gra012i.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TOT-RECORDS ASSIGN TO "S30" 
           ORGANIZATION LINE SEQUENTIAL.
           SELECT INSFILE ASSIGN TO "S35"     ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC        RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES.
           SELECT PARMNAME ASSIGN TO "S40"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT AGEDATE ASSIGN TO "S45"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT CHARCUR ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES.
           SELECT FILEOUT ASSIGN TO "S55"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT GARFILE ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS RANDOM RECORD KEY IS G-GARNO.
       DATA DIVISION.
       FILE SECTION.
       FD  GARFILE
      *    BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS G-MASTER.
       01  G-MASTER.
           02 G-GARNO PIC X(8).
           02 G-GARNAME PIC X(24).
           02 G-BILLADD PIC X(22).
           02 G-STREET PIC X(22).
           02 G-CITY PIC X(18).
           02 G-STATE PIC X(2).
           02 G-ZIP PIC X(9).
           02 G-COLLT PIC X.
           02 G-PHONE PIC X(10).
           02 G-SEX PIC X.
           02 G-RELATE PIC X.
           02 G-MSTAT PIC X.
           02 G-DOB PIC X(8).
           02 G-DUNNING PIC X.
           02 G-ACCTSTAT PIC X.
           02 G-PR-MPLR PIC X(4).
           02 G-PRINS PIC 999.
           02 G-PR-ASSIGN PIC X.
           02 G-PR-OFFICE PIC X(4).
           02 G-PR-GROUP PIC X(12).
           02 G-PRIPOL PIC X(14).
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-SE-MPLR PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
           02 G-SE-GROUP PIC X(12).
           02 G-SECPOL PIC X(14).
           02 G-SENAME PIC X(24).
           02 G-SE-RELATE PIC X.
           02 G-COPAY PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.


       FD  CHARCUR
      *    BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS CHARCUR01.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC PIC X(7).
           02 CC-MOD2 PIC XX.
           02 CC-MOD3 PIC XX.
           02 CC-MOD4 PIC XX.
           02 CC-AMOUNT PIC S9(4)V99.
           02 CC-DOCR PIC X(3).
           02 CC-DOCP PIC X(2).
           02 CC-PAYCODE PIC XXX.
           02 CC-STUD PIC X.
           02 CC-WORK PIC XX.
           02 CC-DAT1 PIC X(8).
           02 CC-RESULT PIC X.
           02 CC-ACT PIC X.
           02 CC-SORCREF PIC X.
           02 CC-COLLT PIC X.
           02 CC-AUTH PIC X.
           02 CC-PAPER PIC X.
           02 CC-PLACE PIC X.
           02 CC-EPSDT PIC X.
           02 CC-DATE-T PIC X(8).
           02 CC-DATE-A PIC X(8).
           02 CC-DATE-P PIC X(8).
           02 CC-REC-STAT PIC X.
           02 CC-DX2 PIC X(7).
           02 CC-DX3 PIC X(7).
           02 CC-ACC-TYPE PIC X.
           02 CC-DATE-M PIC X(8).
           02 CC-ASSIGN PIC X.
           02 CC-NEIC-ASSIGN PIC X.
           02 CC-DX4 PIC X(7).
           02 CC-DX5 PIC X(7).
           02 CC-DX6 PIC X(7).
           02 CC-FUTURE PIC X(6).
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
       FD  TOT-RECORDS.
       01  TOT-RECORDS01.
           02 TR-1 PIC 9999.
           02 TR-2 PIC 99.
           02 TR-3 PIC 9.
           02 TR-4 PIC 99.
           02 TR-5 PIC S9(8)V99.
       FD  PARMNAME
           DATA RECORD IS PARMNAME01.
       01   PARMNAME01 PIC X(40).
       FD  AGEDATE
           DATA RECORD IS AGEDATE01.
       01  AGEDATE01.
           02 DATE-LOW PIC X(8).
           02 DATE-HIGH PIC X(8).
       FD FILEOUT
           DATA RECORD IS FILEOUT01.
       01  FILEOUT01 PIC X(133).
       WORKING-STORAGE SECTION.
        01 MON-TAB-RE01.
          02 FILLER PIC X(27) VALUE "JANUARY  FEBRUARY MARCH    ".
          02 FILLER PIC X(27) VALUE "APRIL    MAY      JUNE     ".
          02 FILLER PIC X(27) VALUE "JULY     AUGUST   SEPTEMBER".
          02 FILER PIC X(27) VALUE "OCTOBER  NOVEMBER DECEMBER ".
       01 MON-TAB01 REDEFINES MON-TAB-RE01.
           02 MON-TAB PIC X(9) OCCURS 12 TIMES.
       01  DOT01.
           02 DOT02 PIC X(50).
           02 DOT03 PIC X(82).
       01 LINE-1.
           02 F11 PIC X(20) VALUE "CHARGE ANALYSIS FOR ".
           02 L1F1 PIC X(40).
           02 F12 PIC X(20) VALUE SPACE.
           02 L1LOW PIC X(10).
           02 F13 PIC X(6) VALUE " THRU ".
           02 L1HIGH PIC X(10).
           02 F14 PIC XXX VALUE SPACE.
           02 F15 PIC X(5) VALUE "PAGE ".
           02 L1F4 PIC ZZZ9.
       01 LINE-2.
           02 F21 PIC X(4) VALUE SPACE.
           02 L2F1 PIC X(9) VALUE "PAYORCODE".
           02 F22 PIC X(14) VALUE SPACE.
           02 L2F3 PIC X(6) VALUE "NUMBER".
           02 F24 PIC X(10) VALUE SPACE.
           02 L2F4 PIC X(6) VALUE "AMOUNT".
           02 F25 PIC XX VALUE SPACE.
           02 L26 PIC X(6) VALUE "  %%% ".
       01 LINE-3.
           02 F31 PIC X VALUE "(".
           02 L3F1 PIC 9(3).
           02 F32 PIC XX VALUE ") ".
           02 L3F2 PIC X(18).
           02 F23 PIC X(4) VALUE SPACE.
           02 L3F3 PIC ZZZ9.
           02 F24 PIC XXX VALUE SPACE.
           02 L3F4 PIC ZZ,ZZZ,ZZ9.99CR.
           02 FILLER PIC X(4) VALUE SPACE.
           02 L3F5 PIC Z99.9.
       01  LINE-4.
           02 F41 PIC X(11).
           02 L4F1 PIC X(10).
           02 F42 PIC X(4) VALUE SPACE.
           02 L4F2 PIC ZZ,ZZ9.
           02 F43 PIC XXX VALUE SPACE.
           02 L4F3 PIC ZZ,ZZZ,ZZ9.99CR.
       01 CNT01.
           02 CNT PIC 99999 OCCURS 999 TIMES.
       01 TOT01.
           02 TOT PIC S9(8)V99 OCCURS 999 TIMES.
       
       01  TOT-CNT PIC 999999 VALUE 0.
       01  TOT-AMT PIC S9(8)V99 VALUE 0.
       01  ADJ-CNT PIC 999999 VALUE 0.
       01  ADJ-TOT PIC S9(8)V99 VALUE 0.
       01  LINE-X PIC 99 VALUE 0.
       01  PAGE-X PIC 9999.
       01  X PIC 99.
       01  Y PIC 99.
       01  Z PIC 9999.
       01  TOTALPAY PIC S9(8)V99 VALUE 0.
       01 NUM-3 PIC 999.
       01  TEST-DATE.
           05  T-CC            PIC XX.
           05  T-YY            PIC XX.
           05  T-MM            PIC XX.
           05  T-DD            PIC XX.
       01  DISPLAY-DATE.
           02 T-MM PIC XX.
           02 FILLER PIC X VALUE "/".
           02 T-DD PIC XX.
           02 FILLER PIC X VALUE "/".
           02 T-CC PIC XX.
           02 T-YY PIC XX.
       PROCEDURE DIVISION.
       P00.
           OPEN EXTEND TOT-RECORDS.
           OPEN INPUT AGEDATE CHARCUR OUTPUT FILEOUT.
           OPEN INPUT GARFILE PARMNAME INSFILE.
           PERFORM A2 VARYING Z FROM 1 BY 1 UNTIL Z > 999.
           READ PARMNAME.
           READ AGEDATE.
           MOVE DATE-LOW TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           MOVE DISPLAY-DATE TO L1LOW 
           MOVE DATE-HIGH TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           MOVE DISPLAY-DATE TO L1HIGH.
       P1. READ CHARCUR AT END GO TO P2.
           IF (CC-DATE-P < DATE-LOW OR > DATE-HIGH) GO TO P1.
           MOVE CC-KEY8 TO G-GARNO.
           READ GARFILE INVALID DISPLAY CC-KEY8 GO TO P1.
           ADD CC-AMOUNT TO TOTALPAY
           ADD 1 TO CNT(G-PRINS)
           ADD CC-AMOUNT TO TOT(G-PRINS)
           GO TO P1.
       P2. MOVE PARMNAME01 TO L1F1.
           MOVE 1 TO L1F4 PAGE-X.
           PERFORM L1.
           PERFORM P5 THRU P5-EXIT VARYING Z FROM 1 BY 1 UNTIL Z > 999.
           MOVE  DOT01 TO FILEOUT01. WRITE FILEOUT01.
           MOVE "TOTALS FOR " TO F41.
           MOVE TOT-CNT TO L4F2.
           MOVE TOT-AMT TO L4F3.
           WRITE FILEOUT01 FROM LINE-4 AFTER 2.
           MOVE  DOT01 TO FILEOUT01. WRITE FILEOUT01.
           WRITE FILEOUT01.
           CLOSE FILEOUT TOT-RECORDS.
           STOP RUN.
       P5. IF CNT(Z) = 0 GO TO P5-EXIT.
           COMPUTE NUM-3 = Z
           MOVE NUM-3 TO L3F1 INS-KEY
           READ INSFILE INVALID MOVE SPACE TO INS-NAME.
           MOVE INS-NAME TO L3F2.
           MOVE CNT(Z) TO L3F3.
           MOVE TOT(Z) TO L3F4.
           COMPUTE L3F5 = (100 * TOT(Z)) / TOTALPAY.
           ADD CNT(Z) TO TOT-CNT.
           ADD TOT(Z) TO TOT-AMT.
           IF LINE-X > 55
           MOVE 0 TO LINE-X
           ADD 1 TO PAGE-X
           MOVE PAGE-X TO L1F4
           PERFORM L1.
           WRITE FILEOUT01 FROM LINE-3
           MOVE Z TO TR-4
           MOVE TOT(Z) TO TR-5
           WRITE TOT-RECORDS01.
           ADD 1 TO LINE-X.
       P5-EXIT. EXIT.
       L1.
           WRITE FILEOUT01 FROM LINE-1 AFTER PAGE.
           WRITE FILEOUT01 FROM LINE-2 AFTER 2
           MOVE SPACE TO FILEOUT01 WRITE FILEOUT01.
       A2. MOVE 0 TO TOT(Z) CNT(Z).
