      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. kin030.
       AUTHOR. SID WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           
           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           
           SELECT FILEIN ASSIGN TO "S35" ORGANIZATION 
           LINE SEQUENTIAL.
          
       DATA DIVISION.
       FILE SECTION.
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
           02 CC-PROC PIC X(11).
           02 CC-MOD2 PIC XX.
           02 CC-MOD3 PIC XX.
           02 CC-MOD4 PIC XX.
           02 CC-AMOUNT PIC S9(4)V99.
           02 CC-DOCR PIC X(3).
           02 CC-DOCP PIC 9(2).
           02 CC-PAYCODE PIC 999.
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
       FD  FILEIN.
       01  FILEIN01.
           02 FILEIN-KEY.
             03 FI-KEY8 PIC X(8).
             03 FI-KEY3 PIC XXX.
           02 FI-PATID PIC X(8).
           02 FI-CLAIM PIC X(6).
           02 FI-SERVICE PIC X.
           02 FI-DIAG PIC X(7).
           02 FI-PROC PIC X(11).
           02 FI-MOD2 PIC XX.
           02 FI-MOD3 PIC XX.
           02 FI-MOD4 PIC XX.
           02 FI-AMOUNT PIC S9(4)V99.
           02 FI-DOCR PIC X(3).
           02 FI-DOCP PIC X(2).
           02 FI-PAYCODE PIC 999.
           02 FI-STUD PIC X.
           02 FI-WORK PIC XX.
           02 FI-DAT1 PIC X(8).
           02 FI-RESULT PIC X.
           02 FI-ACT PIC X.
           02 FI-SORCREF PIC X.
           02 FI-COLLT PIC X.
           02 FI-AGE PIC X.
           02 FI-PAPER PIC X.
           02 FI-PLACE PIC X.
           02 FI-EPSDT PIC X.
           02 FI-DATE-T PIC X(8).
           02 FI-DATE-A PIC X(8).
           02 FI-DATE-P PIC X(8).
           02 FI-REC-STAT PIC X.
           02 FI-DX2 PIC X(7).
           02 FI-DX3 PIC X(7).
           02 FI-ACC-TYPE PIC X.
           02 CC-DATE-M PIC X(8).
           02 FI-ASSIGN PIC X.
           02 FI-NEIC-ASSIGN PIC X.
           02 FI-DX4 PIC X(7).
           02 FI-DX5 PIC X(7).
           02 FI-DX6 PIC X(7).
           02 FI-FUTURE PIC X(6).
 
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT FILEIN.
           OPEN I-O CHARCUR.
       P1.
           READ FILEIN AT END
               GO TO P99
           END-READ
               
           MOVE FILEIN-KEY TO CHARCUR-KEY
           READ CHARCUR WITH LOCK INVALID
               DISPLAY CHARCUR-KEY " BAD"
           END-READ        
           GO TO P1

           MOVE "018" TO CC-PAYCODE
           REWRITE CHARCUR01.           
       P99.
           CLOSE CHARCUR FILEIN.
           STOP RUN.
