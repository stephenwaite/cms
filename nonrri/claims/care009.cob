      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. care009.
       AUTHOR. SID WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS RANDOM RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS  CC-PAYCODE WITH DUPLICATES.
           SELECT GARFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS IS RANDOM RECORD KEY IS G-GARNO.
           SELECT CAREFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS CARE-KEY
           LOCK MODE MANUAL.
           SELECT FILEIN ASSIGN TO "S45" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S50" ORGANIZATION 
           LINE SEQUENTIAL.
           SELECT FILEOUT2 ASSIGN TO "S55" ORGANIZATION 
           LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  FILEIN.
       01  FILEIN01.
           02 FI-PC PIC 999.
           02 FI-PATID.
             03 FI-PATID7 PIC X(7).
             03 FI-PATID8 PIC X.
           02 FI-KEY PIC X(11).
           02 FI-DATE PIC X(8).
           02 FI-ASSIGN PIC X.
           02 FI-PLACE PIC X.
           02 FI-DOC PIC XX.
           02 FI-PAPER PIC X.
           02 FILLER PIC X(27).
           02 FI-PS PIC X.
           02 FILLER PIC X(70).
       FD FILEOUT.
       01  FILEOUT01 PIC X(133).
       FD FILEOUT2.
       01  FILEOUT201 PIC X(133).

       FD  CHARCUR
           BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS CHARCUR01.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8.
               04 CC-KEY81 PIC X(7).
               04 CC-KEY82 PIC X.
             03 CC-KEY3 PIC XXX.
           02 CC-PATID.
             03 CC-PATID7 PIC X(7).
             03 CC-PATID8 PIC X.
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG pic x(7).
           02 CC-PROC.
             03 CC-PROC1 PIC X(5).
             03 CC-PROC2 PIC XX.
           02 CC-MOD2 PIC XX.
           02 CC-MOD3 PIC XX.
           02 CC-MOD4 PIC XX.
           02 CC-AMOUNT PIC S9(4)V99.
           02 CC-DOCR PIC X(3).
           02 CC-DOCP PIC 99.
           02 CC-PAYCODE PIC 999.
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
       FD GARFILE
           BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS G-MASTER.
       01 G-MASTER.
           02 G-GARNO PIC X(8).
           02 G-GARNAME.
             03 GN1 PIC X.
             03 GN2 PIC X(23).
           02 G-BILLADD.
             03 G-BILLADD1 PIC X.
             03 G-BILLADD2 PIC X(21).
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
       FD  CAREFILE.
       01  CAREFILE01.
           02 CARE-KEY.
              03 CR-KEY8 PIC X(8).
              03 CR-DATE PIC X(8).
              03 CR-PROC PIC X(5).
              03 CR-MOD1 PIC XX.
              03 CR-MOD2 PIC XX.
           02 CR-PAYDATE PIC X(8).
           02 CR-DOCP    PIC X(6).
           02 CR-POS     PIC XX.
           02 CR-BILLED PIC 9(4)V99.
           02 CR-ALLOWED PIC 9(4)V99.
           02 CR-DEDUCT  PIC 9(4)V99.
           02 CR-PAYED   PIC 9(4)V99.
           02 CR-DENIAL1 PIC X(4).
           02 CR-DENIAL2 PIC X(4).
           02 CR-DENIAL3 PIC X(4).
           02 CR-DENIAL4 PIC X(4).
           02 CR-PAYDENIAL PIC X(4).
           02 CR-ICN PIC X(13).
           02 CR-CK-EFT PIC X(9).
           02 CR-INSNAME PIC X(30).

       WORKING-STORAGE SECTION.
       01  ALF1 PIC X.
       PROCEDURE DIVISION.
       P0.
           OPEN OUTPUT FILEOUT FILEOUT2
                INPUT FILEIN CHARCUR GARFILE CAREFILE.
       P1. READ FILEIN AT END GO TO P99.
           IF FI-PS = "0" PERFORM A1 GO TO P1.
           MOVE FI-KEY TO CHARCUR-KEY
           READ CHARCUR INVALID
           DISPLAY FI-KEY
           ACCEPT ALF1
           PERFORM A1
           GO TO P1.
           MOVE CC-KEY8 TO G-GARNO
           READ GARFILE INVALID
           DISPLAY CC-KEY8
           ACCEPT ALF1
           PERFORM A1
           GO TO P1.
           IF G-PRINS NOT = "003"  PERFORM A1 GO TO P1.
           MOVE CC-KEY8 TO CR-KEY8
           MOVE CC-DATE-T TO CR-DATE
           MOVE CC-PROC1 TO CR-PROC
           MOVE SPACE TO CR-PROC CR-MOD1 CR-MOD2
           START CAREFILE KEY NOT < CARE-KEY INVALID
           PERFORM A1 GO TO P1.
           
       P2. READ CAREFILE NEXT AT END PERFORM A1 GO TO P1.
           IF CR-KEY8 NOT = CC-KEY8 PERFORM A1 GO TO P1.
           IF CR-DATE NOT = CC-DATE-T GO TO P2.
           IF CR-PROC NOT = CC-PROC1  GO TO P2.
           IF CR-DENIAL1 = "MA18" OR "N89 " PERFORM A2 GO TO P1.
           IF CR-DENIAL2 = "MA18" OR "N89 " PERFORM A2 GO TO P1.
           IF CR-DENIAL3 = "MA18" OR "N89 " PERFORM A2 GO TO P1.
           IF CR-DENIAL4 = "MA18" OR "N89 " PERFORM A2 GO TO P1.
           IF CR-INSNAME NOT = SPACE PERFORM A2
           GO TO P1.
           PERFORM A1.
           GO TO P1.
       A1.
           WRITE FILEOUT01 FROM FILEIN01.
       A2.
           WRITE FILEOUT201 FROM FILEIN01.

       P99.
           CLOSE FILEOUT FILEOUT2
           STOP RUN.
