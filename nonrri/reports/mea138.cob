      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. mea138.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT GARFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC  RECORD KEY IS G-GARNO
           LOCK MODE MANUAL.
           SELECT FILEOUT ASSIGN TO "S40" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT DIAGFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS DIAG-KEY
           ALTERNATE RECORD KEY IS DIAG-TITLE WITH DUPLICATES
           LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FILEOUT.
       01  FILEOUT01.
       
           02 KEY-OUT PIC X(11).
           02 PROV-FNAME PIC X(20).
           02 PROV-LNAME PIC X(20).
           02 PAT-FNAME PIC X(20).
           02 PAT-LNAME PIC X(20).
           02 PAT-DOB PIC X(20).
           02 PAT-SEX PIC X(20).
           02 DATE1 PIC X(20).
           02 DESC  PIC X(20).
           02 F-ICD PIC X(11).
           02 ICD10 PIC X(20).
           02 F-PROC PIC X(7).

       FD  DIAGFILE.
       01  DIAG01.
           02 DIAG-KEY.
              03 diag-9 PIC X(5).
              03 diag-10 pic xx.
           02 DIAG-TITLE.
             03 DIAG-T1 PIC XXXXX.
             03 DIAG-T2 PIC X(56).
           02 DIAG-MEDB PIC X(5).
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
           02 G-PRINS PIC XXX.
           02 G-PR-ASSIGN PIC X.
           02 G-PR-OFFICE PIC X(4).
           02 G-PR-GROUP PIC X(10).
           02 G-PRIPOL PIC X(16).
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-SE-MPLR PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
           02 G-SE-GROUP PIC X(10).
           02 G-SECPOL PIC X(16).
           02 G-SENAME PIC X(24).
           02 G-SE-RELATE PIC X.
           02 G-COPAY PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.
       FD  CHARCUR.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC.
              03 CC-PROC1.
                 04 CC-PROC2 PIC X(5).
                 04 CC-PROC3 PIC XX.
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
       WORKING-STORAGE SECTION.
       01  ALF1 PIC X.
       01  DX-0 PIC X(7).

       PROCEDURE DIVISION.

       0005-START.

           OPEN INPUT DIAGFILE GARFILE CHARCUR OUTPUT FILEOUT.
           
       P1. 
           READ CHARCUR NEXT
             AT END
               GO TO P99.

      *     IF CC-PLACE NOT = "1" 
      *       GO TO P1.

           IF NOT (CC-DATE-T(1:4) = "2020" )
             GO TO P1.
             
      *     IF NOT (CC-PROC2 = "99201" OR "99202" OR "99203"
      *                  OR "99204" OR "99205" OR "99212" OR "99213"
      *                  OR "99214" OR "99215")
           GO TO P1
           END-IF.

           MOVE SPACE TO DX-0
           IF  (CC-DIAG = "C430   " or "C4310  " or "C43111 "
                    or "C43112 " or "C43121 " or "C43122 "
                    or "C4320  " or "C4321  " or "C4322  "
                    OR "C4330  " or "C4331  " or "C4339  " or "C434   "
                    OR "C4351  " or "C4352  " or "C4359  " or "C4360  "
                    OR "C4361  " or "C4362  " or "C4370  " or "C4371  "
                    OR "C4372  " or "C438   " or "C439   " or "D030   "
                    OR "D0310  " or "D03111 " or "D03112 " or "D03121 "
                    or "D03122 " or "D0312  " or "D0320  "
                    OR "D0321  " or "D0322  " or "D0330  " or "D0339  "
                    OR "D034   " or "D0351  " or "D0352  " or "D0359  "
                    OR "D0360  " or "D0361  " or "D0362  " or "D0370  "
                    OR "D0371  " or "D0372  " or "D038   " or "D039   ")
               MOVE CC-DIAG TO DX-0
               GO TO P2
           END-IF
                    
           IF  (CC-DX2 = "C430   " or "C4310  " or "C43111 "
                    or "C43112 " or "C43121 " or "C43122 "
                    or "C4320  " or "C4321  " or "C4322  "
                    OR "C4330  " or "C4331  " or "C4339  " or "C434   "
                    OR "C4351  " or "C4352  " or "C4359  " or "C4360  "
                    OR "C4361  " or "C4362  " or "C4370  " or "C4371  "
                    OR "C4372  " or "C438   " or "C439   " or "D030   "
                    OR "D0310  " or "D03111 " or "D03112 " or "D03121 "
                    or "D03122 " or "D0312  " or "D0320  "
                    OR "D0321  " or "D0322  " or "D0330  " or "D0339  "
                    OR "D034   " or "D0351  " or "D0352  " or "D0359  "
                    OR "D0360  " or "D0361  " or "D0362  " or "D0370  "
                    OR "D0371  " or "D0372  " or "D038   " or "D039   ")
               MOVE CC-DX2 TO DX-0
               GO TO P2
           END-IF

           IF  (CC-DX3 = "C430   " or "C4310  " or "C43111 "
                    or "C43112 " or "C43121 " or "C43122 "
                    or "C4320  " or "C4321  " or "C4322  "
                    OR "C4330  " or "C4331  " or "C4339  " or "C434   "
                    OR "C4351  " or "C4352  " or "C4359  " or "C4360  "
                    OR "C4361  " or "C4362  " or "C4370  " or "C4371  "
                    OR "C4372  " or "C438   " or "C439   " or "D030   "
                    OR "D0310  " or "D03111 " or "D03112 " or "D03121 "
                    or "D03122 " or "D0312  " or "D0320  "
                    OR "D0321  " or "D0322  " or "D0330  " or "D0339  "
                    OR "D034   " or "D0351  " or "D0352  " or "D0359  "
                    OR "D0360  " or "D0361  " or "D0362  " or "D0370  "
                    OR "D0371  " or "D0372  " or "D038   " or "D039   ")
               MOVE CC-DX3 TO DX-0
               GO TO P2
           END-IF

           IF  (CC-DX = "C430   " or "C4310  " or "C43111 "
                    or "C43112 " or "C43121 " or "C43122 "
                    or "C4320  " or "C4321  " or "C4322  "
                    OR "C4330  " or "C4331  " or "C4339  " or "C434   "
                    OR "C4351  " or "C4352  " or "C4359  " or "C4360  "
                    OR "C4361  " or "C4362  " or "C4370  " or "C4371  "
                    OR "C4372  " or "C438   " or "C439   " or "D030   "
                    OR "D0310  " or "D03111 " or "D03112 " or "D03121 "
                    or "D03122 " or "D0312  " or "D0320  "
                    OR "D0321  " or "D0322  " or "D0330  " or "D0339  "
                    OR "D034   " or "D0351  " or "D0352  " or "D0359  "
                    OR "D0360  " or "D0361  " or "D0362  " or "D0370  "
                    OR "D0371  " or "D0372  " or "D038   " or "D039   ")
               MOVE CC-DX4 TO DX-0
               GO TO P2
           END-IF
           
           GO TO P1.
       P2.
           MOVE CC-KEY8 TO G-GARNO
           READ GARFILE INVALID DISPLAY G-GARNO " HAS NO ACCT"
           ACCEPT ALF1
           GO TO P1
           END-READ.
      *     IF G-PRINS NOT = "003" GO TO P1.

           MOVE SPACE TO PROV-FNAME PROV-LNAME
           MOVE "DANIEL" TO PROV-FNAME
           MOVE "MCCAULIFFE" TO PROV-LNAME
           MOVE SPACE TO PAT-LNAME PAT-FNAME
           IF CC-DOCP = "02"
             MOVE "KERRY" TO PROV-FNAME
             MOVE "LANE" TO PROV-LNAME.

           UNSTRING G-GARNAME DELIMITED BY ";"
             INTO PAT-LNAME PAT-FNAME ALF1
           
           MOVE SPACE TO PAT-DOB
           STRING G-DOB(5:2) "/" G-DOB(7:2) "/" G-DOB(1:4)
           DELIMITED BY SIZE INTO PAT-DOB
           MOVE SPACE TO PAT-SEX
           MOVE "MALE  " TO PAT-SEX
           IF G-SEX = "F"
           MOVE "FEMALE" TO PAT-SEX.
           MOVE DX-0 TO DIAG-KEY
           READ DIAGFILE INVALID 
             MOVE DX-0 TO ICD10
            NOT INVALID
             MOVE DX-0 TO ICD10
           END-READ
           MOVE SPACE TO DATE1
           STRING CC-DATE-T(5:2) "/" CC-DATE-T(7:2) "/" CC-DATE-T(1:4)
           DELIMITED BY SIZE INTO DATE1
           MOVE CHARCUR-KEY TO KEY-OUT
           MOVE "ICD10" TO DESC
           MOVE CC-PROC2 TO F-PROC
           MOVE "7010F  " TO F-ICD
           WRITE FILEOUT01
           GO TO P1.
       P99.
           CLOSE GARFILE CHARCUR FILEOUT.
           STOP RUN.
