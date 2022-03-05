      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. mit001.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AGEDATE ASSIGN TO "S25" ORGANIZATION
             LINE SEQUENTIAL.
           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL.
           SELECT FILEIN ASSIGN TO "S35" ORGANIZATION
             LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S40" ORGANIZATION
             LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  AGEDATE.
       01  AGEDATE01.
           02 AGE-LOW PIC X(8).
           02 AGE-HIGH PIC X(8).
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
           02 CC-PROC.
              03 CC-PROC0 PIC X(4).
              03 CC-PROC1 PIC X(7).
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
       FD FILEIN.
       01  FILEIN01.
           02 FI-1 PIC X(7).
       FD  FILEOUT.
       01  FILEOUT01 PIC X(160).
       WORKING-STORAGE SECTION.
       01  HOLD8 PIC X(8).
       01  HOLD-DATE PIC X(8).
       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT CHARCUR FILEIN AGEDATE.
           OPEN OUTPUT FILEOUT.
           READ AGEDATE AT END CONTINUE.
       P1. READ FILEIN  AT END GO TO P99.
           MOVE SPACE TO CHARCUR-KEY
           START CHARCUR KEY NOT < CHARCUR-KEY INVALID GO TO P1.
       P2.
           READ CHARCUR NEXT AT END GO TO P1.
           IF CC-PROC1 NOT = FI-1 GO TO P2.
           IF CC-DATE-T < AGE-LOW OR > AGE-HIGH GO TO P2.
           MOVE CC-DATE-T TO HOLD-DATE
           MOVE CC-KEY8 TO HOLD8           
           MOVE SPACE TO CC-KEY3
           START CHARCUR KEY NOT < CHARCUR-KEY INVALID GO TO P2.
       P3.
           READ CHARCUR NEXT AT END GO TO P4.
           IF CC-KEY8 NOT = HOLD8 GO TO P4.
           IF CC-DATE-T NOT = HOLD-DATE GO TO P3.
           WRITE FILEOUT01 FROM CHARCUR01
           GO TO P3.
       P4.
           MOVE HOLD8 TO CC-KEY8
           MOVE "999" TO CC-KEY3
           START CHARCUR KEY NOT < CHARCUR-KEY INVALID GO TO P1.
           GO TO P2.
       P99. 
           CLOSE CHARCUR FILEIN FILEOUT AGEDATE. 
           STOP RUN.
