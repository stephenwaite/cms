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
       
           SELECT CHARFILE  ASSIGN TO  "S30" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS CHARFILE-KEY
               LOCK MODE MANUAL.

           SELECT FILEOUT   ASSIGN TO   "S35" ORGANIZATION
               LINE SEQUENTIAL.

           SELECT CLAIMFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS CLAIM-KEY
               LOCK MODE MANUAL.

           SELECT GARFILE   ASSIGN TO "S45" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
               ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
               LOCK MODE MANUAL.

           SELECT FILEOUT2  ASSIGN TO "S50" ORGANIZATION
               LINE SEQUENTIAL.
    

       DATA DIVISION.

       FILE SECTION.
       
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
           02 CD-QP1 PIC XX.
           02 CD-QP2 PIC XX.
           02 CD-DX5-3 PIC X(3).
           02 CD-DX6 PIC X(7).
           02 CD-FUTURE PIC X(6).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).

       FD  FILEOUT2.
       01  FILEOUT201 PIC X(120).

       FD  CLAIMFILE.
       01  CLAIM01.
           02 CLAIM-KEY PIC X.
           02 CLAIMNO PIC 9(6).

       FD  GARFILE.
           COPY "garfile.cpy" IN "C:\Users\sid\cms\copylib\rri".     

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
       01  X-PROC PIC X(11).
       01  PROC-HOLD PIC X(5).
       01  ALF1 PIC X.

       PROCEDURE DIVISION.

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
           
           MOVE SPACE TO CHARFILE-KEY.

       P0.
           START CHARFILE KEY NOT < CHARFILE-KEY INVALID
               GO TO P2
           END-START.    

       P1. 
           READ CHARFILE NEXT WITH LOCK
             AT END
               GO TO P2
           END-READ          

           IF NOT (CD-PAYCODE = "009" OR "010" OR "012" 
               OR "013" OR "014")
               GO TO P1
           END-IF

           MOVE CHARFILE01 TO CHARBACK01
           MOVE CD-PROC1 TO PROC-HOLD           

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

           IF CD-PAYCODE = "010"
               MOVE SPACE TO FILEOUT01
               STRING "N15 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
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

           IF CD-PAYCODE = "011"
               MOVE SPACE TO FILEOUT01
               STRING "N19 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01 
               MOVE 919 TO FLAG
               PERFORM GET-INS
               MOVE G-PRINS TO CD-PAYCODE
               REWRITE CHARFILE01
               UNLOCK CHARFILE RECORD
               PERFORM A1 THRU A1-EXIT
               GO TO P0
           END-IF
	
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
           
           IF CD-PAYCODE = "014"
               MOVE SPACE TO FILEOUT01
               STRING "436 " CD-PROC1 " " CD-DATE-T " " CD-NAME
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01 
               MOVE 436 TO FLAG
               MOVE "003" TO CD-PAYCODE
               REWRITE CHARFILE01
               UNLOCK CHARFILE RECORD
               PERFORM A1 THRU A1-EXIT
               GO TO P0
           END-IF

           GO TO P1.

      *  create quality code charges     
       A1. 
      *  set key counter to 0, increment in B1 
           MOVE 0 TO XYZ

           IF FLAG = 405
               IF CD-QP1 = "1 "
                   MOVE "0000G9548  " TO  X-PROC
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
                   MOVE "0000G9549  " TO  X-PROC
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
                   MOVE "0000G9550  " TO  X-PROC
                   PERFORM B1 THRU B2
                   STRING CD-KEY8 "000"
                       DELIMITED BY SIZE INTO CHARFILE-KEY
               END-IF

               IF CD-QP1 = "1 " OR "2 " OR "3 "      
                   MOVE "0000G9547  " TO  X-PROC
                   PERFORM B1 THRU B2
                   STRING CD-KEY8 "000"
                       DELIMITED BY SIZE INTO CHARFILE-KEY
               ELSE
                   MOVE "0000G9551  " TO  X-PROC
                   PERFORM B1 THRU B2
                   STRING CD-KEY8 "000" 
                       DELIMITED BY SIZE INTO CHARFILE-KEY     
               END-IF
                              
      *    handle multi-qpp-cpts by doing mea 436
               IF (PROC-HOLD = "71250" OR "71260" OR "71270"
                   OR "71271" OR "71275" OR "72131" OR "72191"
                   OR "72192" OR "72193" OR "72194"
                   OR "74150" OR "74160" OR "74170"
                   OR "74176" OR "74177" OR "74178")
                   MOVE SPACE TO FILEOUT01              
                   STRING "436 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                          " REPORTED."
                   DELIMITED BY SIZE INTO FILEOUT01
                   WRITE FILEOUT01
                   MOVE "0000G9637  " TO  X-PROC
                   PERFORM B1 THRU B2
                   STRING CD-KEY8 "000"
                       DELIMITED BY SIZE INTO CHARFILE-KEY
               END-IF

               GO TO A1-EXIT
           END-IF
	
           IF FLAG = 406
               IF CD-QP1 = "1 "                    
                   MOVE "0000G9554  " TO  X-PROC
                   PERFORM B1 THRU B2
                   STRING CD-KEY8 "000"
                        DELIMITED BY SIZE INTO CHARFILE-KEY
               END-IF
           
               IF CD-QP1 = "2 "
                   MOVE "0000G9555  " TO  X-PROC
                   PERFORM B1 THRU B2
                   STRING CD-KEY8 "000"
                        DELIMITED BY SIZE INTO CHARFILE-KEY
               END-IF

               IF CD-QP1 = "3 "
                   MOVE "0000G9556  " TO  X-PROC
                   PERFORM B1 THRU B2
                   STRING CD-KEY8 "000"
                        DELIMITED BY SIZE INTO CHARFILE-KEY
      *  log measure fails
                   MOVE SPACE TO FILEOUT01              
                   STRING "406 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                          " PERFORMANCE NOT MET which is great "
                          " it's an inverse measure :)"   
                   DELIMITED BY SIZE INTO FILEOUT01
                   WRITE FILEOUT01
               END-IF

               IF (CD-QP1 = "1 " OR "2 " OR "3 ")
                   MOVE "0000G9552  " TO  X-PROC
                   PERFORM B1 THRU B2
                   STRING CD-KEY8 "000"
                        DELIMITED BY SIZE INTO CHARFILE-KEY
               ELSE
                   MOVE "0000G9557  " TO  X-PROC
                   PERFORM B1 THRU B2
                   STRING CD-KEY8 "000" 
                       DELIMITED BY SIZE INTO CHARFILE-KEY
               END-IF
             
              
      *    handle multi-qpp-cpts by doing mea 436
               IF (PROC-HOLD = "70486" OR "70487" OR "70488"
                   OR "70490" OR "70491" OR "70492" OR "70498"
                   OR "71250" OR "71260" OR "71270" OR "71271"
                   OR "71275" OR "72125" OR "72126" OR "72127")
                   MOVE SPACE TO FILEOUT01              
                   STRING "436 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                          " REPORTED."
                   DELIMITED BY SIZE INTO FILEOUT01
                   WRITE FILEOUT01
                   MOVE "0000G9637  " TO  X-PROC
                   PERFORM B1 THRU B2
                   STRING CD-KEY8 "000"
                       DELIMITED BY SIZE INTO CHARFILE-KEY
               END-IF
               GO TO A1-EXIT
           END-IF

           IF FLAG = 436
               MOVE "0000G9637  " TO  X-PROC
               PERFORM B1 THRU B2
               STRING CD-KEY8 "000" DELIMITED BY SIZE INTO CHARFILE-KEY
               GO TO A1-EXIT
           END-IF.

      *    measure 195 retired in 2022
      *    measure 076 retired in 2023
      *    measure 147 retired in 2024
      *    Dan picked 3 measures from acr qcdr
           
           IF FLAG = 926
               IF CD-QP1 = "1 "
      *    create comma delimited file to upload to acr registry                              
                   MOVE SPACE TO FILEOUT201              
                   STRING CD-DATE-T "," G-GARNO "," G-DOB "," G-SEX ","
                       G-PRINS  "," G-PRIPOL ",QMM26," CD-PROC1  ","
                       CD-DIAG ",PM002," CD-CLAIM
                   DELIMITED BY SIZE INTO FILEOUT201
                   WRITE FILEOUT201
      *    size delimited file for output to coders                                                
                   MOVE SPACE TO FILEOUT01              
                   STRING "QMM26 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                       " PERFORMANCE MET"
                   DELIMITED BY SIZE INTO FILEOUT01
                   WRITE FILEOUT01                          
               END-IF
           
               IF CD-QP1 = "2 "
                   MOVE SPACE TO FILEOUT201              
                   STRING CD-DATE-T "," G-GARNO "," G-DOB "," G-SEX ","
                       G-PRINS "," G-PRIPOL ",QMM26," CD-PROC1 "," 
                       CD-DIAG ",PM102," CD-CLAIM
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
                   STRING CD-DATE-T "," G-GARNO "," G-DOB "," G-SEX ","
                       G-PRINS "," G-PRIPOL ",QMM26," CD-PROC1 ","
                       CD-DIAG ",PM202," CD-CLAIM
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
                   STRING CD-DATE-T "," G-GARNO "," G-DOB "," G-SEX ","
                       G-PRINS "," G-PRIPOL ",QMM26," CD-PROC1 ","
                       CD-DIAG ",PNM02," CD-CLAIM
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
                   STRING CD-DATE-T "," G-GARNO "," G-DOB "," G-SEX ","
                       G-PRINS "," G-PRIPOL ",QMM26," CD-PROC1 ","
                       CD-DIAG ",PE002," CD-CLAIM
                   DELIMITED BY SIZE INTO FILEOUT201
                   WRITE FILEOUT201
                   MOVE SPACE TO FILEOUT01              
                   STRING "QMM26 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                       " *** DENOMINATOR EXCEPTION!"
                   DELIMITED BY SIZE INTO FILEOUT01
                   WRITE FILEOUT01
               END-IF

               GO TO A1-EXIT

           END-IF

           IF FLAG = 915
               IF CD-QP1 = "1 "
      *    create comma delimited file to upload to acr registry                              
                   MOVE SPACE TO FILEOUT201              
                   STRING CD-DATE-T "," G-GARNO "," G-DOB "," G-SEX ","
                       G-PRINS "," G-PRIPOL ",MSN15," CD-PROC1 ","
                       CD-DIAG ",PM004," CD-CLAIM
                   DELIMITED BY SIZE INTO FILEOUT201
                   WRITE FILEOUT201
      *    size delimited file for output to coders                                                
                   MOVE SPACE TO FILEOUT01              
                   STRING "MSN15 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                       " PERFORMANCE MET"
                   DELIMITED BY SIZE INTO FILEOUT01
                   WRITE FILEOUT01                          
               END-IF
           
               IF CD-QP1 = "2 "
                   MOVE SPACE TO FILEOUT201              
                   STRING CD-DATE-T "," G-GARNO "," G-DOB "," G-SEX ","
                       G-PRINS "," G-PRIPOL ",MSN15," CD-PROC1 ","
                       CD-DIAG ",PNM04," CD-CLAIM
                   DELIMITED BY SIZE INTO FILEOUT201
                   WRITE FILEOUT201
                   MOVE SPACE TO FILEOUT01              
                   STRING "MSN15 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                       " *** PERFORMANCE NOT MET!"
                   DELIMITED BY SIZE INTO FILEOUT01
                   WRITE FILEOUT01
               END-IF

               IF CD-QP1 = "3 "
                   MOVE SPACE TO FILEOUT201              
                   STRING CD-DATE-T "," G-GARNO "," G-DOB "," G-SEX ","
                       G-PRINS "," G-PRIPOL ",MSN15," CD-PROC1 ","
                       CD-DIAG ",PE004," CD-CLAIM
                   DELIMITED BY SIZE INTO FILEOUT201
                   WRITE FILEOUT201
                   MOVE SPACE TO FILEOUT01              
                   STRING "MSN15 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                       " *** DENOMINATOR EXCEPTION!"
                   DELIMITED BY SIZE INTO FILEOUT01
                   WRITE FILEOUT01
               END-IF

               IF CD-QP1 = SPACE
                   MOVE SPACE TO FILEOUT01              
                   STRING "MSN15 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                       " NO NODULE(S)"
                   DELIMITED BY SIZE INTO FILEOUT01
                   WRITE FILEOUT01
               END-IF

               GO TO A1-EXIT
               
           END-IF

           IF FLAG = 919
               IF CD-QP1 = "1 "
      *    create comma delimited file to upload to acr registry                              
                   MOVE SPACE TO FILEOUT201              
                   STRING CD-DATE-T "," G-GARNO "," G-DOB "," G-SEX ","
                       G-PRINS "," G-PRIPOL ",QMM19," CD-PROC1 ","
                       CD-DIAG ",PM019," CD-CLAIM
                   DELIMITED BY SIZE INTO FILEOUT201
                   WRITE FILEOUT201
      *    size delimited file for output to coders                                                
                   MOVE SPACE TO FILEOUT01              
                   STRING "QMM19 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                       " PERFORMANCE MET"
                   DELIMITED BY SIZE INTO FILEOUT01
                   WRITE FILEOUT01                          
               END-IF
           
               IF CD-QP1 = "2 "
                   MOVE SPACE TO FILEOUT201              
                   STRING CD-DATE-T "," G-GARNO "," G-DOB "," G-SEX ","
                       G-PRINS "," G-PRIPOL ",QMM19," CD-PROC1 ","
                       CD-DIAG ",PNM19," CD-CLAIM
                   DELIMITED BY SIZE INTO FILEOUT201
                   WRITE FILEOUT201
                   MOVE SPACE TO FILEOUT01              
                   STRING "QMM19 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                       " *** PERFORMANCE NOT MET!"
                   DELIMITED BY SIZE INTO FILEOUT01
                   WRITE FILEOUT01
               END-IF

               IF CD-QP1 = "3 "
                   MOVE SPACE TO FILEOUT201              
                   STRING CD-DATE-T "," G-GARNO "," G-DOB "," G-SEX ","
                       G-PRINS "," G-PRIPOL ",MSN15," CD-PROC1 ","
                       CD-DIAG ",PE004," CD-CLAIM
                   DELIMITED BY SIZE INTO FILEOUT201
                   WRITE FILEOUT201
                   MOVE SPACE TO FILEOUT01              
                   STRING "MSN15 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                       " *** DENOMINATOR EXCEPTION!"
                   DELIMITED BY SIZE INTO FILEOUT01
                   WRITE FILEOUT01
               END-IF

               IF CD-QP1 = SPACE
                   MOVE SPACE TO FILEOUT01              
                   STRING "QMM19 " CD-PROC1 " " CD-DATE-T " " CD-KEY8
                       " NO OSTEOPENIA"
                   DELIMITED BY SIZE INTO FILEOUT01
                   WRITE FILEOUT01
               END-IF

               GO TO A1-EXIT
               
           END-IF.

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
           MOVE HOLD-ID TO CHARFILE-KEY
           MOVE X-PROC TO CD-PROC
           MOVE "0000000" TO CD-DX2 CD-DX3 CD-DX4
           MOVE 0 TO CD-AMOUNT
           MOVE 003 TO CD-PAYCODE
           MOVE SPACE TO CD-MOD2 CD-MOD3 CD-MOD4
           ADD 1 TO CLAIMNO
           MOVE CLAIMNO TO CD-CLAIM
           WRITE CHARFILE01.

       A1-EXIT.
           EXIT.

       GET-INS.
           MOVE CD-KEY8 TO G-GARNO
           READ GARFILE INVALID               
               DISPLAY "GARNO NOT AVAILABLE FOR SOME UNKNOWN REASON"
               DISPLAY "PLEASE RECORD THIS FACT " CD-KEY8
               GO TO P1
           END-READ.    

       P2.
           REWRITE CLAIM01
           CLOSE CHARFILE CLAIMFILE FILEOUT.
           STOP RUN.
