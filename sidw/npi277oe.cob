      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. npi277.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARMFILE ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEIN ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT GARFILE ASSIGN TO "S40"     ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC        RECORD KEY IS G-GARNO
           LOCK MODE MANUAL.
           SELECT FILEOUT1 ASSIGN TO "S45" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT CSCFILE ASSIGN TO "S50" ORGANIZATION INDEXED
           ACCESS DYNAMIC RECORD KEY IS CSC-KEY
           LOCK MODE MANUAL.
           SELECT CSCCFILE ASSIGN TO "S55" ORGANIZATION INDEXED
           ACCESS DYNAMIC RECORD KEY IS CSCC-KEY
           LOCK MODE MANUAL.
           SELECT CHARCUR ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT FILEOUT2 ASSIGN TO "S70" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEOUT3 ASSIGN TO "S75" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
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
       FD  CSCCFILE.
       01  CSCCFILE01.
           02 CSCC-KEY PIC XXX.
           02 CSCC-TITLE PIC X(240).
       FD  CSCFILE.
       01  CSCFILE01.
           02 CSC-KEY PIC XXX.
           02 CSC-TITLE PIC X(261).

       FD  PARMFILE.
       01  PARMFILE01 PIC X(40).
       FD FILEOUT1.
       01 FILEOUT101 PIC X(160).
       FD FILEOUT2.
       01 FILEOUT201 PIC X(158).
       FD FILEOUT3.
       01 FILEOUT301 PIC X(130).

       FD FILEIN.
       01  FILEIN01.
           02 F0.
             03 F1 PIC XXX.
             03 F2 PIC X(4).
           02 F3 PIC X(153).
       FD GARFILE.
       01 G-MASTER.
           02 G-GARNO.
             03 ID1 PIC XXX.
             03 ID2 PIC XXX.
             03 ID3 PIC XX.
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
           02 G-PRINS PIC XXX.
           02 G-PR-ASSIGN PIC X.
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
           02 G-PR-GROUP PIC X(10).
           02 G-PRIPOL0.
             03 G-PRIPOL PIC X(9).
             03 G-PR-SUFX PIC XXX.
             03 G-PR-FILLER PIC X(4).
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-SE-MPLR PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-SE-OFFICE PIC X(4).
           02 G-SE-GROUP PIC X(10).
           02 G-SECPOL0. 
              03 G-SECPOL PIC X(9).
              03 G-SE-FILLER PIC X(7).
           02 G-SENAME PIC X(24).
           02 G-SE-RELATE PIC X.
           02 G-INSPEND PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.

       WORKING-STORAGE SECTION.
       01  DTP01.
           02 DTP-0 PIC XXX.
           02 DTP-1 PIC XXX.
           02 DTP-2 PIC XX.
           02 DTP-3 PIC X(8).
       01  REF01.
           02 REF-0 PIC XXX.
           02 REF-1 PIC XXX.
           02 REF-2 PIC X(30).
         
       01  SVC01.
           02 SVC-0 PIC XXX.
           02 SVC-1PROCMOD PIC X(17).
           02 SVC-2CHRGAMT PIC X(8).
           02 SVC-3PAYAMT  PIC X(8).
           02 SVC-4NUBC PIC XXX.
           02 SVC-5QUAN PIC X(5).
           02 SVC-6COMPOSITE PIC X(80).
           02 SVC-7QUAN PIC X(5).
       01 N101.
           02 N1-0 PIC XX.
           02 N1-1 PIC XX.
           02 N1-2 PIC X(20).
           02 N1-3 PIC XX.
           02 N1-4 PIC X(10).

       01 NM101.
           02 NM1-0 PIC XXX.
           02 NM1-1 PIC XXX.
           02 NM1-SOLO PIC X.
           02 NM1-NAMEL PIC X(24).
           02 NM1-NAMEF PIC X(24).
           02 NM1-NAMEM PIC X.
           02 NM1-NAMES PIC XXX.
           02 NM1-EINSS PIC XX.
           02 NM1-PREFIX PIC XX.
           02 NM1-CODE PIC X(16).
       01  DISPLAY-DATE.
           05 T-MM  PIC 99.
           05 FILLER PIC X VALUE "/".
           05 T-DD  PIC 99.
           05 FILLER PIC X VALUE "/".
           05 T-CC  PIC 99.
           05 T-YY  PIC 99.
       01  TEST-DATE.
           05 T-CC  PIC 99.
           05 T-YY  PIC 99.
           05 T-MM  PIC 99.
           05 T-DD  PIC 99.
       01  PROV-1 PIC X(10).
       01  PROV-2 PIC X(10).
       01  PROV-FED PIC X(9).
       01  PROV-LEG PIC X(6).

       01  IN-NPI PIC X(10).
       01  IN-FEDID PIC X(9).
       01  IN-LEG PIC X(6).
       01  TRN01.
           02 TRN-0 PIC XXX.
           02 TRN-1 PIC X.
           02 TRN-2 PIC X(12).
       01  STC01.
           02 STC-0 PIC XXX.
           02 STC-1 PIC X(12).
           02 STC-2 PIC X(8).
           02 STC-3 PIC XX.
           02 STC-4 PIC X(9).
           02 STC-5 PIC X.
           02 STC-6 PIC X.
           02 STC-7 PIC X.
           02 STC-8 PIC X.
           02 STC-9 PIC X.
           02 STC-10 PIC X(12).
           02 STC-11 PIC X.
           02 STC-12 PIC X(118).

       01  ALF1 PIC X.
       01  CODE1 PIC XX.
       01  CODE2 PIC XXX.
       01  CODE3 PIC XX.
       01  CNTR PIC 9(6) VALUE 0.
       01  x pic 9999.
       01  stc-cntr pic 9999.
       01  stc-tab01.
           02 stc-tab pic x(120) occurs 4000 times.
       01  dtp-tab01.
           02 dtp-tab pic x(120) occurs 4000 times.
       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT FILEIN CSCFILE CSCCFILE GARFILE
           CHARCUR PARMFILE.
           OPEN OUTPUT FILEOUT1 FILEOUT2 FILEOUT3.

           READ PARMFILE AT END GO TO P99.
           write fileout301 from parmfile01 after page.
           READ PARMFILE AT END GO TO P99.
           MOVE SPACE TO PROV-1 PROV-2
           UNSTRING PARMFILE01 DELIMITED BY " " INTO PROV-1 PROV-2.
       P00.
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P99.
       XX.
           IF FILEIN01(1:6) NOT = "NM1*85" GO TO P00.
            MOVE SPACE TO NM101
            UNSTRING FILEIN01 DELIMITED BY "*" INTO
            NM1-0 NM1-1 NM1-SOLO NM1-NAMEL NM1-NAMEF NM1-NAMEM 
            NM1-NAMES NM1-EINSS NM1-PREFIX NM1-CODE
           IF NOT (NM1-CODE(1:10) = PROV-1
                      OR NM1-CODE(1:10) = PROV-2) GO TO P00.
       P000.
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P99.
           IF FILEIN01(1:3) = "GE" OR "GS" GO TO XX.
           IF FILEIN01(1:6) NOT = "TRN*2*" GO TO P000.
           MOVE SPACE TO TRN01
           UNSTRING FILEIN01 DELIMITED BY "*" INTO TRN-0 TRN-1 TRN-2
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P99.
           if filein01(1:3) NOT = "STC"
            DISPLAY G-GARNO " " F1 " RECORD SHOULD HAVE BEEN STC"
            DISPLAY "FIX FILE BEFORE RUNNING AGAIN"
            ACCEPT OMITTED   
            GO TO P99.

           move 0 to stc-cntr
           GO TO XXX0-1.
       xxx0.
           MOVE SPACE TO FILEIN01.
           READ FILEIN AT END GO TO P99.
       XXX0-1.
           if filein01(1:2) = "HL" OR "SE" OR "GS" OR "GE"
             GO TO XXX1
           END-IF.
           if filein01(1:3) = "STC"
           if stc-cntr = 400
            display "400 stc records"
            accept omitted
           end-if

           add 1 to stc-cntr
           move filein01 to stc-tab(stc-cntr)
           go to xxx0
           end-if

           if filein01(1:3) = "DTP"
             move filein01 to dtp-tab(stc-cntr)
           END-IF.
           go to xxx0.
       xxx1.
           perform XX1 THRU XX1-EXIT
             varying x from 1 by 1 until x > stc-cntr.
           go to xx1-1.
       XX1.
           MOVE SPACE TO STC01
           UNSTRING STC-TAB(X) DELIMITED BY "*"
            INTO STC-0 STC-1 STC-2 STC-3 STC-4 STC-5 STC-6 STC-7 STC-8
            STC-9 STC-10 STC-11 STC-12
           MOVE SPACE TO CODE1 CODE2 CODE3
           UNSTRING STC-1 DELIMITED BY ":" INTO CODE1 CODE2 CODE3

           IF STC-3 NOT = "U " GO TO XX1-EXIT.

           IF CODE1 = "A3" AND CODE2 = "21 "
             UNSTRING STC-10 DELIMiTED BY ":" INTO CODE1 CODE2 CODE3
           end-if    

           move space to dtp01
           unstring dtp-tab(x) delimited by "*" into
             dtp-0 dtp-1 dtp-2 dtp-3
           
           PERFORM CODE-X.
           PERFORM XX6.
           PERFORM XX2-1 THRU XX5.

       XX1-EXIT.
           EXIT.

       XX1-1.
           IF FILEIN01(1:2) = "SE" OR "GS" OR "GE" GO TO XX.
           GO TO P000.
           
       XX7.
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P99.
           IF F1 NOT = "STC"
            DISPLAY G-GARNO " " F1 " RECORD SHOULD HAVE BEEN STC"
            DISPLAY "FIX FILE BEFORE RUNNING AGAIN"
            ACCEPT OMITTED
            GO TO P99.
           MOVE SPACE TO STC01
           UNSTRING FILEIN01 DELIMITED BY "*"
           INTO STC-0 STC-1 STC-2 STC-3 STC-4
           MOVE SPACE TO CODE1 CODE2 CODE3
           UNSTRING STC-1 DELIMITED BY ":" INTO CODE1 CODE2 CODE3
           PERFORM CODE-X
           PERFORM XX6.
       XX8.
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P99.
           IF F1 = "SE*" GO TO P00.
           IF F1 = "HL*" GO TO P000.
           IF F1 = "SVC" GO TO XX7.
           GO TO XX8.
       XX2-1.
           MOVE G-GARNO TO CC-KEY8
           MOVE SPACE TO CC-KEY3
           START CHARCUR KEY NOT < CHARCUR-KEY INVALID GO TO XX5.
       XX2.
           READ CHARCUR NEXT AT END GO TO XX5.
           IF CC-KEY8 NOT = G-GARNO GO TO XX5.
           IF CC-PAYCODE NOT = "003" GO TO XX2.
           IF CC-DATE-T NOT = DTP-3 GO TO XX2.

           WRITE FILEOUT101 FROM CHARCUR01
           GO TO XX2.
       XX5.
           EXIT.
       XX6.
           MOVE SPACE TO FILEOUT301
           MOVE DTP-3 TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           STRING
           CSCC-KEY " " CSC-KEY " " NM1-NAMEL " " TRN-2(1:12)
           " " G-GARNAME  " " DISPLAY-DATE
            DELIMITED BY SIZE INTO FILEOUT301
           WRITE FILEOUT301
           
           MOVE SPACE TO FILEOUT301
           MOVE CSCC-TITLE(1:120) TO FILEOUT301
           WRITE FILEOUT301
           
           MOVE SPACE TO FILEOUT301
           MOVE CSCC-TITLE(121:120) TO FILEOUT301
           WRITE FILEOUT301
           
           MOVE SPACE TO FILEOUT301
           MOVE CSC-TITLE(1:130) TO FILEOUT301
           WRITE FILEOUT301
           
           MOVE SPACE TO FILEOUT301
           MOVE CSC-TITLE(131:131) TO FILEOUT301
           
           IF FILEOUT301 NOT = SPACE
            WRITE FILEOUT301
           END-IF
           
           IF STC-12 NOT = SPACE
             MOVE STC-12 TO FILEOUT301
             WRITE FILEOUT301
           END-IF.

       CODE-X.
           MOVE CODE1 TO CSCC-KEY
           MOVE SPACE TO CSC-TITLE CSCC-TITLE
           READ CSCCFILE INVALID DISPLAY CODE1 " BAD CODE1"
           ACCEPT OMITTED
           END-READ.
           MOVE CODE2 TO CSC-KEY
           READ CSCFILE INVALID DISPLAY CODE2 " BAD CODE2"
           ACCEPT OMITTED
           END-READ.

       P99.
           CLOSE CSCFILE CSCCFILE GARFILE CHARCUR
                 FILEOUT1 FILEOUT2 FILEOUT3
           STOP RUN.
