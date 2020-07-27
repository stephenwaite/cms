      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. mea004.
       AUTHOR. S WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARFILE ASSIGN TO  "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARFILE-KEY
           LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO   "S35" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT CLAIMFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CLAIM-KEY
           LOCK MODE MANUAL.
       DATA DIVISION.
       FILE SECTION.
       FD  CLAIMFILE.
       01  CLAIM01.
           02 CLAIM-KEY PIC X.
           02 CLAIMNO PIC 9(6).
       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).
       FD  CHARFILE
      *    BLOCK CONTAINS 2 RECORDS
           DATA RECORD IS CHARFILE01.
       01  CHARFILE01.
           02 CHARFILE-KEY.
             03 CD-KEY8 PIC X(8).
             03 CD-KEY3 PIC XXX.
           02 CD-PATID PIC X(8).
           02 CD-CLAIM PIC X(6).
           02 CD-SERVICE PIC X.
           02 CD-DIAG PIC X(7).
           02 CD-PROC.
              03 CD-PROC0 PIC X(4).
              03 CD-PROC1 PIC X(5).
              03 CD-PROC2 PIC XX.
           02 CD-MOD2 PIC XX.
           02 CD-MOD3 PIC XX.
           02 CD-MOD4 PIC XX.
           02 CD-AMOUNT PIC S9(4)V99.
           02 CD-DOCR PIC X(3).
           02 CD-DOCP PIC X(2).
           02 CD-PAYCODE PIC XXX.
           02 CD-REC-STAT PIC X.
           02 CD-WORK PIC XX.
           02 CD-DAT1 PIC X(8).
           02 CD-RESULT PIC X.
           02 CD-ACT PIC X.
           02 CD-SORCREF PIC X.
           02 CD-COLLT PIC X.
           02 CD-AUTH PIC X.
           02 CD-PAPER PIC X.
           02 CD-PLACE PIC X.
           02 CD-NAME PIC X(24).
           02 CD-ESPDT PIC X.
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
       01  CHARBACK01.
           02 CHARBACK-KEY.
             03 BK-KEY8 PIC X(8).
             03 BK-KEY3 PIC XXX.
           02 BK-PATID PIC X(8).
           02 BK-CLAIM PIC X(6).
           02 BK-SERVICE PIC X.
           02 BK-DIAG PIC X(7).
           02 BK-PROC.
              03 BK-PROC0 PIC X(4).
              03 BK-PROC1 PIC X(5).
              03 BK-PROC2 PIC XX.
           02 BK-MOD2 PIC XX.
           02 BK-MOD3 PIC XX.
           02 BK-MOD4 PIC XX.
           02 BK-AMOUNT PIC S9(4)V99.
           02 BK-DOCR PIC X(3).
           02 BK-DOCP PIC X(2).
           02 BK-PAYCODE PIC XXX.
           02 BK-REC-STAT PIC X.
           02 BK-WORK PIC XX.
           02 BK-DAT1 PIC X(8).
           02 BK-RESULT PIC X.
           02 BK-ACT PIC X.
           02 BK-SORCREF PIC X.
           02 BK-COLLT PIC X.
           02 BK-AUTH PIC X.
           02 BK-PAPER PIC X.
           02 BK-PLACE PIC X.
           02 BK-NAME PIC X(24).
           02 BK-ESPDT PIC X.
           02 BK-DATE-T PIC X(8).
           02 BK-DATE-E PIC X(8).
           02 BK-ORDER PIC X(6).
           02 BK-DX2 PIC X(7).
           02 BK-DX3 PIC X(7).
           02 BK-DATE-A PIC X(8).
           02 BK-ACC-TYPE PIC X.
           02 BK-DATE-M PIC X(8).
           02 BK-ASSIGN PIC X.
           02 BK-NEIC-ASSIGN PIC X.
           02 BK-DX4 PIC X(7).
           02 BK-DX5 PIC X(7).
           02 BK-DX6 PIC X(7).
           02 BK-FUTURE PIC X(6).
       01  XYZ PIC 999.
       01  HOLD8 PIC X(8).
       01  FLAG PIC 999.
       01  HOLD-ID PIC X(11).
       01  SAVE-KEY PIC X(11).
       01  ALF1 PIC X.
       PROCEDURE DIVISION.
       0005-START.
           OPEN I-O CHARFILE CLAIMFILE. 
           OPEN OUTPUT FILEOUT.
           MOVE SPACE TO CHARFILE-KEY.
           MOVE "A" TO CLAIM-KEY.
           READ CLAIMFILE WITH LOCK INVALID KEY 
               DISPLAY "BAD CLAIM #"
               ACCEPT ALF1
               GO TO P2
           END-READ    
           MOVE SPACE TO CHARFILE-KEY.
       P0.
           START CHARFILE KEY NOT < CHARFILE-KEY INVALID
               GO TO P2
           END-START.    
       P1. 
           READ CHARFILE NEXT WITH LOCK AT END
               GO TO P2
           END-READ

           IF NOT (CD-PAYCODE = "008" OR "009" OR "010" OR "011" 
               OR "012" OR "013" OR "014" OR "015")
               GO TO P1
           END-IF

           IF CD-PAYCODE = "008"
               MOVE SPACE TO FILEOUT01
               STRING "145 " CD-PROC1 " " CD-DATE-T " " CD-NAME
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01 
               MOVE 145 TO FLAG
               MOVE "003" TO CD-PAYCODE
               MOVE CHARFILE01 TO CHARBACK01
               REWRITE CHARFILE01
               UNLOCK CHARFILE RECORD
               PERFORM A1 THRU A1-EXIT
               GO TO P0
           END-IF.
	
           IF CD-PAYCODE = "009"
               MOVE SPACE TO FILEOUT01
               STRING "146 " CD-PROC1 " " CD-DATE-T " " CD-NAME
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01 
               MOVE 146 TO FLAG
               MOVE "003" TO CD-PAYCODE
               MOVE CHARFILE01 TO CHARBACK01
               REWRITE CHARFILE01
               UNLOCK CHARFILE RECORD
               PERFORM A1 THRU A1-EXIT
               GO TO P0
           END-IF.

           IF CD-PAYCODE = "010"
               MOVE SPACE TO FILEOUT01
               STRING "147 " CD-PROC1 " " CD-DATE-T " " CD-NAME
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01 
               MOVE 147 TO FLAG
               MOVE "003" TO CD-PAYCODE
               MOVE CHARFILE01 TO CHARBACK01
               REWRITE CHARFILE01
               UNLOCK CHARFILE RECORD
               PERFORM A1 THRU A1-EXIT
               GO TO P0
           END-IF.

           IF CD-PAYCODE = "011"
               MOVE SPACE TO FILEOUT01
               STRING "195 " CD-PROC1 " " CD-DATE-T " " CD-NAME
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01 
               MOVE 195 TO FLAG
               MOVE "003" TO CD-PAYCODE
               MOVE CHARFILE01 TO CHARBACK01
               REWRITE CHARFILE01
               UNLOCK CHARFILE RECORD
               PERFORM A1 THRU A1-EXIT
               GO TO P0
           END-IF.
	
           IF CD-PAYCODE = "012"
               MOVE SPACE TO FILEOUT01
               STRING "405 " CD-PROC1 " " CD-DATE-T " " CD-NAME
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01 
      *         DISPLAY "MEASURE 405"
      *         DISPLAY CHARFILE01
               MOVE 405 TO FLAG
               MOVE "003" TO CD-PAYCODE
               MOVE CHARFILE01 TO CHARBACK01
               REWRITE CHARFILE01
               UNLOCK CHARFILE RECORD
               PERFORM A1 THRU A1-EXIT
               GO TO P0
           END-IF.
	
           IF CD-PAYCODE = "013"
               MOVE SPACE TO FILEOUT01
               STRING "406 " CD-PROC1 " " CD-DATE-T " " CD-NAME
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01 
      *         DISPLAY "MEASURE 406"
      *         DISPLAY CHARFILE01
               MOVE 406 TO FLAG
               MOVE "003" TO CD-PAYCODE
               MOVE CHARFILE01 TO CHARBACK01
               REWRITE CHARFILE01
               UNLOCK CHARFILE RECORD
               PERFORM A1 THRU A1-EXIT
               GO TO P0
           END-IF
           
           IF CD-PAYCODE = "014"
               MOVE SPACE TO FILEOUT01
               STRING "436 " CD-PROC1 " " CD-DATE-T " " CD-NAME
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01 
               MOVE 436 TO FLAG
               MOVE "003" TO CD-PAYCODE
               MOVE CHARFILE01 TO CHARBACK01
               REWRITE CHARFILE01
               UNLOCK CHARFILE RECORD
               PERFORM A1 THRU A1-EXIT
               GO TO P0
           END-IF

           IF CD-PAYCODE = "015"
               MOVE SPACE TO FILEOUT01
               STRING "195 " CD-PROC1 " " CD-DATE-T " " CD-NAME
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               STRING "406 " CD-PROC1 " " CD-DATE-T " " CD-NAME
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               STRING "436 " CD-PROC1 " " CD-DATE-T " " CD-NAME
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01 
      *    using 999 to flag 2 measures         
               MOVE 999 TO FLAG
               MOVE "003" TO CD-PAYCODE
               MOVE CHARFILE01 TO CHARBACK01
               REWRITE CHARFILE01
               UNLOCK CHARFILE RECORD
               PERFORM A1 THRU A1-EXIT
               GO TO P0
           END-IF

           GO TO P1.
       A1. 
           MOVE 0 TO CD-AMOUNT
           MOVE 0 TO XYZ.
           MOVE SPACE TO CD-MOD2 CD-MOD3

           IF FLAG = 145
               MOVE "0000G9500  " TO CD-PROC
               MOVE CHARFILE01 TO CHARBACK01
               PERFORM B1 THRU B2
               MOVE SPACE TO CHARFILE-KEY
               STRING CD-KEY8 "000" DELIMITED BY SIZE INTO CHARFILE-KEY
               GO TO A1-EXIT
           END-IF

           IF FLAG = 146
               MOVE "00003341F  " TO CD-PROC
               MOVE "Z1231  " TO CD-DIAG

      *    paragraph A1-1 adds HCPCS for measure 225, mammo reminder         
               IF CD-MOD4 = "1 "
                   GO TO A1-1
               END-IF

               IF CD-MOD4 = "0 "
                   MOVE "00003340F  " TO CD-PROC
                   GO TO A1-1
               END-IF

               IF CD-MOD4 = "2 "
                   MOVE "00003342F  " TO CD-PROC
                   GO TO A1-1
               END-IF

               IF CD-MOD4 = "3 "
                   MOVE "00003343F  " TO CD-PROC
                   GO TO A1-1
               END-IF

               IF CD-MOD4 = "4 "
                   MOVE "00003344F  " TO CD-PROC
                   GO TO A1-1
               END-IF

               IF CD-MOD4 = "5 "
                   MOVE "00003345F  " TO CD-PROC
                   GO TO A1-1
               END-IF

               IF CD-MOD4 = "6 "
                   MOVE "00003350F  " TO CD-PROC
                   GO TO A1-1
               END-IF
           END-IF

           IF FLAG = 147
               MOVE "00003570F  " TO CD-PROC
               MOVE CD-MOD4 TO CD-PROC(10:2)
               MOVE CHARFILE01 TO CHARBACK01
               PERFORM B1 THRU B2
               MOVE SPACE TO CHARFILE-KEY
               STRING CD-KEY8 "000" DELIMITED BY SIZE INTO CHARFILE-KEY
               GO TO A1-EXIT
           END-IF

           IF FLAG = 195
               MOVE "00003100F  " TO CD-PROC
               MOVE CD-MOD4 TO CD-PROC(10:2)
               MOVE CHARFILE01 TO CHARBACK01
               PERFORM B1 THRU B2
               MOVE SPACE TO CHARFILE-KEY
               STRING CD-KEY8 "000" DELIMITED BY SIZE INTO CHARFILE-KEY

      *    handle multi-qpp-cpts by adding measure 145
               IF (CD-PROC = "36222" OR "36223" OR "36224"
                   OR "37215" OR "37216" OR "37217" OR "37218")
                   MOVE "0000G9500  " TO CD-PROC
                   MOVE CHARFILE01 TO CHARBACK01
                   PERFORM B1 THRU B2
                   MOVE SPACE TO CHARFILE-KEY
                   STRING CD-KEY8 "000"
                       DELIMITED BY SIZE INTO CHARFILE-KEY
               END-IF

               GO TO A1-EXIT
           END-IF.
	
           IF FLAG = 405
               IF CD-MOD4 = "1 "
                   MOVE "0000G9550  " TO CD-PROC
                   MOVE CHARFILE01 TO CHARBACK01
                   PERFORM B1 THRU B2
               END-IF

               IF CD-MOD4 = "2 "
                   MOVE "0000G9549  " TO CD-PROC
                   MOVE CHARFILE01 TO CHARBACK01
                   PERFORM B1 THRU B2
               END-IF

               IF CD-MOD4 = "3 "
                   MOVE "0000G9548  " TO CD-PROC
                   MOVE CHARFILE01 TO CHARBACK01
                   PERFORM B1 THRU B2
               END-IF
           
               IF CD-MOD4 = "1 " OR "2 " OR "3 "
                   MOVE "0000G9547  " TO CD-PROC
               ELSE
                   MOVE "0000G9551  " TO CD-PROC
               END-IF
               
               MOVE CHARFILE01 TO CHARBACK01
               PERFORM B1 THRU B2
               MOVE SPACE TO CHARFILE-KEY
               STRING CD-KEY8 "000" DELIMITED BY SIZE INTO CHARFILE-KEY

      *    handle multi-qpp-cpts by doing mea 436
               IF (CD-PROC = "74150" OR "74160" OR "74170"
                   OR "74176" OR "74177" OR "74178")
                   MOVE "0000G9637  " TO CD-PROC
                   MOVE CHARFILE01 TO CHARBACK01
                   PERFORM B1 THRU B2
                   MOVE SPACE TO CHARFILE-KEY
                   STRING CD-KEY8 "000"
                       DELIMITED BY SIZE INTO CHARFILE-KEY
               END-IF

               GO TO A1-EXIT
           END-IF
	
           IF FLAG = 406
               IF CD-MOD4 = "1 "
                   MOVE "0000G9556  " TO CD-PROC
                   MOVE CHARFILE01 TO CHARBACK01
                   PERFORM B1 THRU B2
               END-IF
           
               IF CD-MOD4 = "2 "
                   MOVE "0000G9555  " TO CD-PROC
                   MOVE CHARFILE01 TO CHARBACK01
                   PERFORM B1 THRU B2
               END-IF

               IF CD-MOD4 = "3 "
                   MOVE "0000G9554  " TO CD-PROC
                   MOVE CHARFILE01 TO CHARBACK01
                   PERFORM B1 THRU B2
               END-IF

               IF CD-MOD4 = "1 " OR "2 " OR "3 "
                   MOVE "0000G9552  " TO CD-PROC
               ELSE
                   MOVE "0000G9557  " TO CD-PROC
               END-IF
             
               MOVE CHARFILE01 TO CHARBACK01
               PERFORM B1 THRU B2
               MOVE SPACE TO CHARFILE-KEY
               STRING CD-KEY8 "000" DELIMITED BY SIZE INTO CHARFILE-KEY

      *    more multi qpp cpts
               IF (CD-PROC = "70490" OR "70491" OR "70492"
                   OR "71250" OR "71260" OR "71270" OR "71275"
                   OR "72125" OR "72126" OR "72127")
                   MOVE "0000G9637  " TO CD-PROC
                   MOVE CHARFILE01 TO CHARBACK01
                   PERFORM B1 THRU B2
                   MOVE SPACE TO CHARFILE-KEY
                   STRING CD-KEY8 "000"
                       DELIMITED BY SIZE INTO CHARFILE-KEY
               END-IF

               GO TO A1-EXIT
           END-IF.

           IF FLAG = 436
               MOVE "0000G9637  " TO CD-PROC
               MOVE CHARFILE01 TO CHARBACK01
               PERFORM B1 THRU B2
               MOVE SPACE TO CHARFILE-KEY
               STRING CD-KEY8 "000" DELIMITED BY SIZE INTO CHARFILE-KEY
               GO TO A1-EXIT
           END-IF

      *    flag 999 is cpt 70498 and meas 195, 406 and 436
      *    so need to add just copy it from above
           IF FLAG = 999
      *    cut and paste in meas 195 logic from above     
               MOVE "00003100F  " TO CD-PROC
               MOVE CD-MOD4 TO CD-PROC(10:2)
               MOVE CHARFILE01 TO CHARBACK01
               PERFORM B1 THRU B2
               MOVE SPACE TO CHARFILE-KEY
               STRING CD-KEY8 "000" DELIMITED BY SIZE INTO CHARFILE-KEY
      *    cut and paste meas 406 logic using spare CD-DX5 field
               IF CD-DX5 = "1 "
                   MOVE "0000G9556  " TO CD-PROC
                   MOVE CHARFILE01 TO CHARBACK01
                   PERFORM B1 THRU B2
               END-IF
           
               IF CD-DX5 = "2 "
                   MOVE "0000G9555  " TO CD-PROC
                   MOVE CHARFILE01 TO CHARBACK01
                   PERFORM B1 THRU B2
               END-IF

               IF CD-DX5 = "3 "
                   MOVE "0000G9554  " TO CD-PROC
                   MOVE CHARFILE01 TO CHARBACK01
                   PERFORM B1 THRU B2
               END-IF
      *    measure 406 has an additional hcpcs based on response
               IF CD-DX5 = "1 " OR "2 " OR "3 "
                   MOVE "0000G9552  " TO CD-PROC
               ELSE
                   MOVE "0000G9557  " TO CD-PROC
               END-IF
               MOVE CHARFILE01 TO CHARBACK01
               PERFORM B1 THRU B2
               MOVE SPACE TO CHARFILE-KEY
               STRING CD-KEY8 "000" DELIMITED BY SIZE INTO CHARFILE-KEY

      *    finally, don't forget new measure 436!
               MOVE "0000G9637  " TO CD-PROC
               MOVE CHARFILE01 TO CHARBACK01
               PERFORM B1 THRU B2
               MOVE SPACE TO CHARFILE-KEY
               STRING CD-KEY8 "000" DELIMITED BY SIZE INTO CHARFILE-KEY
               GO TO A1-EXIT
           END-IF.
       A1-1.
           STRING "225 " CD-PROC1 " " CD-DATE-T " " CD-NAME
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           MOVE CHARFILE01 TO CHARBACK01
           PERFORM B1 THRU B2
           MOVE "00007025F  " TO CD-PROC
           MOVE CHARFILE01 TO CHARBACK01
           PERFORM B1 THRU B2 
           STRING CD-KEY8 "000" DELIMITED BY SIZE INTO CHARFILE-KEY
           GO TO A1-EXIT.
       B1.
           ADD 1 TO XYZ.
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
           MOVE HOLD-ID TO CHARFILE-KEY
           ADD 1 TO CLAIMNO
           MOVE CLAIMNO TO CD-CLAIM
           MOVE CHARFILE-KEY TO SAVE-KEY
           WRITE CHARFILE01.
       A1-EXIT.
           EXIT.
       P2.
           REWRITE CLAIM01
           CLOSE CHARFILE CLAIMFILE.
           STOP RUN.
