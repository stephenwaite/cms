      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ste005.
       AUTHOR. S WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT FILEIN ASSIGN TO  "S30" ORGANIZATION IS 
           LINE SEQUENTIAL.
	  
	   SELECT CHARCUR ASSIGN TO "S40" ORGANIZATION IS INDEXED
	       ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
	       ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES.
	   
	   SELECT GARFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
	       ACCESS IS DYNAMIC RECORD KEY IS G-GARNO.
	  
       DATA DIVISION.
       FILE SECTION.
       
       FD FILEIN.
       01 FILEIN01 PIC X(11).
       
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
	   02 G-INSPEND PIC S9(5)V99.
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
       
       WORKING-STORAGE SECTION.
       
       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT FILEIN GARFILE.
           OPEN I-O CHARCUR.
       P0.
           READ FILEIN AT END
               GO TO P99
           END-READ.

           MOVE FILEIN01 TO CHARCUR-KEY
           
           READ CHARCUR WITH LOCK INVALID KEY
               GO TO P0
           END-READ

           MOVE CC-KEY8 TO G-GARNO

           READ GARFILE INVALID KEY
              GO TO P0
           END-READ   

           MOVE G-SEINS TO CC-PAYCODE
           MOVE G-SE-ASSIGN TO CC-ASSIGN CC-NEIC-ASSIGN
           MOVE "2" TO CC-REC-STAT
           MOVE "20210707" TO CC-DATE-A
           move "1" to CC-PLACE
           move "95" to cc-mod3
           REWRITE CHARCUR01
           GO TO P0.
       P99.
           CLOSE FILEIN GARFILE CHARCUR.
           STOP RUN.
