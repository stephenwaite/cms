      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. mea137.
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

       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib".
     
      
       FD  GARFILE.
           COPY GARFILE.CPY IN "C:\Users\sid\cms\copylib".

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

           IF CC-PLACE NOT = "1" 
             GO TO P1.

           IF NOT (CC-DATE-T(1:4) = "2020" )
             GO TO P1.
             
           IF NOT (CC-PROC(1:5) = "99201" OR "99202" OR "99203"
             OR "99204" OR "99205" OR "99212" OR "99213"
             OR "99214" OR "99215")
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
                    or "D03122 " or "D0320  "
                    OR "D0321  " or "D0322  " or "D0330  " or "D0339  "
                    OR "D034   " or "D0351  " or "D0352  " or "D0359  "
                    OR "D0360  " or "D0361  " or "D0362  " or "D0370  "
                    OR "D0371  " or "D0372  " or "D038   " or "D039   "
                    OR "Z85820 " OR "Z86006 ")

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
                    or "D03122 " or "D0320  "
                    OR "D0321  " or "D0322  " or "D0330  " or "D0339  "
                    OR "D034   " or "D0351  " or "D0352  " or "D0359  "
                    OR "D0360  " or "D0361  " or "D0362  " or "D0370  "
                    OR "D0371  " or "D0372  " or "D038   " or "D039   "
                    OR "Z85820 " OR "Z86006 ")

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
                    or "D03122 " or "D0320  "
                    OR "D0321  " or "D0322  " or "D0330  " or "D0339  "
                    OR "D034   " or "D0351  " or "D0352  " or "D0359  "
                    OR "D0360  " or "D0361  " or "D0362  " or "D0370  "
                    OR "D0371  " or "D0372  " or "D038   " or "D039   "
                    OR "Z85820 " OR "Z86006 ")
               MOVE CC-DX3 TO DX-0
               GO TO P2
           END-IF

           IF  (CC-DX4 = "C430   " or "C4310  " or "C43111 "
                    or "C43112 " or "C43121 " or "C43122 "
                    or "C4320  " or "C4321  " or "C4322  "
                    OR "C4330  " or "C4331  " or "C4339  " or "C434   "
                    OR "C4351  " or "C4352  " or "C4359  " or "C4360  "
                    OR "C4361  " or "C4362  " or "C4370  " or "C4371  "
                    OR "C4372  " or "C438   " or "C439   " or "D030   "
                    OR "D0310  " or "D03111 " or "D03112 " or "D03121 "
                    or "D03122 " or "D0320  "
                    OR "D0321  " or "D0322  " or "D0330  " or "D0339  "
                    OR "D034   " or "D0351  " or "D0352  " or "D0359  "
                    OR "D0360  " or "D0361  " or "D0362  " or "D0370  "
                    OR "D0371  " or "D0372  " or "D038   " or "D039   "
                    OR "Z85820 " OR "Z86006 ")  
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
           MOVE CC-PROC TO F-PROC
           MOVE "7010F  " TO F-ICD
           WRITE FILEOUT01
           GO TO P1.
       P99.
           CLOSE GARFILE CHARCUR FILEOUT.
           STOP RUN.
