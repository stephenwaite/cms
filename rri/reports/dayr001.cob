      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. dayr001.
       AUTHOR. S WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS SEQUENTIAL RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT FILEIN ASSIGN TO "S35" ORGANIZATION LINE
           SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S40" ORGANIZATION LINE
           SEQUENTIAL.
           SELECT FILEOUT2 ASSIGN TO "S45" ORGANIZATION LINE
           SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD FILEIN.
       01  FILEIN01. 
           02 FI-LOW PIC X(8).
           02 FI-HIGH PIC X(8).
       FD FILEOUT.
       01  FILEOUT01 PIC X(20).
       FD FILEOUT2.
       01  FILEOUT201 PIC X(160).

       FD  CHARCUR
           BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS CHARCUR01.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC. 
               03 CC-PROC0 PIC X(4).
               03 CC-PROC1 PIC X(7).
           02 CC-MOD2 PIC XX.
           02 CC-MOD3 PIC XX.
           02 CC-MOD4 PIC XX.
           02 CC-AMOUNT PIC S9(4)V99.
           02 CC-DOCR PIC X(3).
           02 CC-DOCP PIC 9(2).
           02 CC-PAYCODE PIC XXX.
           02 CC-STUD PIC X.
           02 CC-WORK PIC XX.
           02 CC-DAT1 PIC X(8).
           02 CC-RESULT PIC X.
           02 CC-ACT PIC X.
           02 CC-SORCREF PIC X.
           02 CC-COLLT PIC X.
           02 CC-AGE PIC X.
           02 CC-PAPER PIC X.
           02 CC-PLACE PIC X.
           02 CC-IOPAT PIC X.
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
       01  DATE-X.
           02 FILLER PIC X(6).
           02 DD PIC 99.
       01  DAYTAB01.
           02 DAYTAB PIC 9999 OCCURS 31 TIMES.
       01  X PIC 99.
       01  NEF-4 PIC ZZZ9.
       01  DAYTOT PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT FILEIN CHARCUR 
             OUTPUT FILEOUT FILEOUT2

           READ FILEIN 
             AT END 
               GO TO P99.

           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 31
             MOVE 0 TO DAYTAB(X)
           END-PERFORM.

       P1. 
           READ CHARCUR 
             AT END 
               GO TO P99.

           IF CC-DATE-T < FI-LOW 
             OR > FI-HIGH 
             GO TO P1.

           IF NOT (CC-PLACE = "3" or "5" or "E") 
             GO TO P1.

           IF CC-PROC(9:1) = "F"
            OR CC-PROC1(1:5) = "G1004" 
                GO TO P1.

           MOVE CC-DATE-T TO DATE-X
           ADD 1 TO DAYTAB(DD)
           WRITE FILEOUT201 FROM CHARCUR01
           GO TO P1.

       P99.  
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 31
             MOVE SPACE TO FILEOUT01
             MOVE DAYTAB(X) TO NEF-4
             ADD DAYTAB(X) TO DAYTOT
             STRING X "  " NEF-4 DELIMITED BY "!!" INTO FILEOUT01
             WRITE FILEOUT01
           END-PERFORM.
           
           MOVE DAYTOT TO NEF-4
           STRING  "TOTAL  " NEF-4 DELIMITED BY "!!" INTO FILEOUT01
           WRITE FILEOUT01

           CLOSE FILEOUT CHARCUR FILEOUT2.
           STOP RUN.

