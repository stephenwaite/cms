      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIDRCIA.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL RECORD KEY IS CHARFILE-KEY.
           SELECT FILEOUT ASSIGN TO "S35"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT GARFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC  RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES.
           SELECT PROCFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC  RECORD KEY IS PROC-KEY.
           SELECT REFPHY ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS REF-KEY
           ALTERNATE RECORD KEY IS REF-BSNUM  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CRNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-UPIN  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CDNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-NAME  WITH DUPLICATES.
       DATA DIVISION.
       FILE SECTION.
       FD  REFPHY
           BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS REFPHY01.
       01  REFPHY01.
           02 REF-KEY PIC XXX.
           02 REF-BSNUM PIC X(5).
           02 REF-CRNUM PIC X(6).
           02 REF-UPIN PIC X(6).
           02 REF-CDNUM PIC X(7).
           02 REF-NAME PIC X(24).
           02 REF-KP PIC X(7).
           02 REF-FUTURE PIC XXX.
       FD PROCFILE
           DATA RECORD PROCFILE01.
       01 PROCFILE01.
           02 PROC-KEY PIC X(11).
           02 PROC-TYPE PIC X.
           02 PROC-TITLE PIC X(28).
           02 PROC-AMOUNT PIC 9(4)V99.
       FD  CHARFILE
           BLOCK CONTAINS 2 RECORDS
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
           03 CD-PROC1 PIC X(4).
           03 CD-PROC2 PIC X(7).
           02 CD-MOD2 PIC XX.
           02 CD-MOD3 PIC XX.
           02 CD-MOD4 PIC XX.
           02 CD-AMOUNT PIC S9(4)V99.
           02 CD-DOCR PIC X(3).
           02 CD-DOCP PIC X(2).
           02 CD-PAYCODE PIC XXX.
           02 CD-STAT PIC X.
           02 CD-WORK PIC XX.
           02 CD-DAT1 PIC X(8).
           02 CD-RESULT PIC X.
           02 CD-ACT PIC X.
           02 CD-SORCREF PIC X.
           02 CD-COLLT PIC X.
           02 CD-AGE PIC X.
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
           02 C-CLINICAL.
             03 CD-CLIN1 PIC X(4).
             03 CD-CLIN2 PIC X(36).
           02 CD-ADMIT-DIAG PIC X(30).
       FD FILEOUT.
       01 FILEOUT01 PIC X(177).
       FD GARFILE
           BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS GARFILE01.
       01 GARFILE01.
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
           02 G-SE-OFFICE PIC X(4).
           02 G-SE-GROUP PIC X(10).
           02 G-SECPOL PIC X(16).
           02 G-SENAME PIC X(24).
           02 G-SE-RELATE PIC X.
           02 G-INSPEND PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.
           02 G-ACCT PIC X(8).
           02 G-PRGRPNAME PIC X(15).
           02 G-SEGRPNAME PIC X(15).
       WORKING-STORAGE SECTION.
       01 FILE-OUT01.
           02 FO-1 PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 FO-3 PIC X(19).
           02 FILLER PIC X VALUE SPACE.
           02 FO-31 PIC X(3).
           02 FILLER PIC X VALUE SPACE.
           02 FO-4 PIC X(11).
           02 FILLER PIC X VALUE SPACE.
           02 FO-5 PIC X(17).
           02 FILLER PIC X VALUE SPACE.
           02 FO-6 PIC X(10).
           02 FILLER PIC X VALUE SPACE.
           02 FO-2 PIC X(11).
           02 FILLER PIC X VALUE SPACE.
           02 FILLER PIC X(6) VALUE " -----".
           02 CLIN PIC X(40).
           02 FILLER PIC X(5) VALUE SPACE.
           02 ADMIT-DIAG PIC X(20).
           02 FILLER PIC X VALUE SPACE.
           02 SORTDIAG PIC X(9).
       01  CONSTANTS.
           02 CNTR PIC 999999 VALUE 148136.
           02 A PIC S9(8)V99 VALUE 0.
           02 X PIC 999999 VALUE 999999.
           02 Y PIC 999999 VALUE 0.
       01  HOLD8 PIC X(8) VALUE SPACE.
       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT PROCFILE CHARFILE GARFILE REFPHY.
           OPEN OUTPUT FILEOUT.
       P1. READ CHARFILE AT END  GO TO P3.
           GO TO P2.
           IF CD-PROC1 > "4999" GO TO P2.
           IF CD-PAYCODE = "091" GO TO P2.
           IF CD-PAYCODE = "002" GO TO I02.
           IF CD-ASSIGN = "U" GO TO P1.
       P2.
           MOVE CD-KEY8 TO G-GARNO.
           READ GARFILE INVALID GO TO P1.
           MOVE CD-DATE-T TO FO-1
           MOVE CHARFILE-KEY TO FO-2
           MOVE G-GARNAME TO FO-3
           MOVE CD-PAYCODE TO FO-31
           MOVE CD-PROC TO FO-4
           MOVE CD-PROC TO PROC-KEY READ PROCFILE
           INVALID MOVE SPACE TO PROC-TITLE.
           MOVE PROC-TITLE TO FO-5
           MOVE CD-DOCR TO REF-KEY
           READ REFPHY INVALID MOVE SPACE TO REF-NAME.
           MOVE REF-NAME TO FO-6.
           IF CD-CLIN1 = "XXXX" MOVE "\/\/" TO CD-CLIN1.
           MOVE C-CLINICAL TO CLIN
           MOVE CD-ADMIT-DIAG TO ADMIT-DIAG
           IF CD-CLIN1 = "\/\/" MOVE G-GARNAME TO SORTDIAG
           ELSE MOVE SPACE TO SORTDIAG.
           WRITE FILEOUT01 FROM FILE-OUT01
           GO TO P1.
       I02.
           IF (CD-PROC2 < "7000000" OR CD-PROC1 > "4999")
           OR (CD-DAT1 NOT = "000000")
           GO TO P2 ELSE GO TO P1.
       P3. CLOSE FILEOUT PROCFILE REFPHY GARFILE. STOP RUN.
