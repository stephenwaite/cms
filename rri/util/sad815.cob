      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NEI815.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYFILE-KEY
           LOCK MODE MANUAL.
           SELECT FILEIN ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S40" ORGANIZATION
           LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       
       FD  FILEIN.
       01  FILEIN01.
           02 FI-GARNO PIC X(8).
           02 FI-MID PIC X(69).
       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).
       FD  PAYFILE
           BLOCK CONTAINS 4 RECORDS
           DATA RECORD IS PAYFILE01.
       01  PAYFILE01.
           02 PAYFILE-KEY.
             03 PD-KEY8 PIC X(8).
             03 PD-KEY3 PIC XXX.
           02 PD-NAME PIC X(24).
           02 PD-AMOUNT PIC S9(4)V99.
           02 PD-PAYCODE PIC XXX.
           02 PD-DENIAL PIC XX.
           02 PD-CLAIM PIC X(6).
           02 PD-DATE-T PIC X(8).
           02 PD-DATE-E PIC X(8).
           02 PD-ORDER PIC X(6).
           02 PD-BATCH PIC X(6).
       WORKING-STORAGE SECTION.
       01 DATE-X. 
              03 X-DATE-TYYYY PIC X(4).
              03 X-DATE-TMMDD PIC X(4).
       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT FILEIN PAYFILE OUTPUT FILEOUT.
       P1.
           READ FILEIN AT END GO TO P99.
           MOVE SPACE TO PD-KEY3
           MOVE FI-GARNO TO PD-KEY8
           START PAYFILE KEY NOT < PAYFILE-KEY INVALID GO TO P1.
       P2.
           READ PAYFILE NEXT AT END GO TO P1.
           IF PD-KEY8 NOT = FI-GARNO GO TO P1.
           IF NOT (PD-PAYCODE = "001" OR "021" OR "022" OR "077") 
             GO TO P2.
           MOVE FILEIN01 TO FILEOUT01
           WRITE FILEOUT01
           GO TO P1.
       P99.
           CLOSE PAYFILE FILEOUT.
           STOP RUN.
