      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. er136.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILEIN ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT GARFILE ASSIGN TO "S35"     ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE MANUAL.

           SELECT CHARCUR ASSIGN TO "S40"     ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.

           SELECT ERROR-FILE ASSIGN TO "S45" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S50" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT FILEOUT2 ASSIGN TO "S55" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD ERROR-FILE.
       01 ERROR-FILE01 PIC X(132).

       FD FILEOUT.
       01 FILEOUT01 PIC X(136).

       FD FILEOUT2.
       01 FILEOUT201 PIC X(136).

       FD  FILEIN
           RECORD CONTAINS 1 TO 124 CHARACTERS.
       01  FILEIN01.
           02 FI-NAME PIC X(20).
           02 FILLER PIC X VALUE SPACE.
           02 FI-POL PIC X(11).
           02 FILLER PIC X VALUE SPACE.
           02 FI-DATE PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 FI-PROC PIC X(5).
           02 FILLER PIC X(7) VALUE SPACE.
           02 FI-GARNO PIC X(8).
           02 FILLER PIC X(53).
           02 FI-DENIAL PIC XXX.
           02 FILLER PIC X(6).

       FD  CHARCUR.
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
              03 CC-PROC1 PIC X(5).
              03 CC-PROC2 PIC XX.
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
           02 CC-AGE PIC X.
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

       FD GARFILE.
       01 G-MASTER.
           02 G-GARNO.
             03 ID1 PIC XXX.
             03 ID2 PIC XXX.
             03 ID3 PIC XX.
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
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
           02 G-PR-GROUP PIC X(12).
           02 G-PRIPOL0.
             03 G-PRIPOL PIC X(9).
             03 G-PR-SUFX PIC XXX.
             03 G-PR-FILLER PIC XX.
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-SE-MPLR PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-SE-OFFICE PIC X(4).
           02 G-SE-GROUP PIC X(12).
           02 G-SECPOL0. 
              03 G-SECPOL PIC X(9).
              03 G-SE-FILLER PIC X(5).
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

       01  HL01.
           02 HL-1 PIC X(40) VALUE SPACE.
           02 FILLER PIC X(21) VALUE SPACE.
           02 HL-2 PIC X(27) VALUE " MEDICARE  UNPOSTED LIST   ".
           02 FILLER PIC X(5) VALUE SPACE.
           02 HL-3 PIC X(10).
       01  ALF1 PIC X.
       01  TOT-AMT PIC S9(4)V99.
       01  CLAIM-TOT PIC S9(5)V99.
       01  FLAG PIC 9 VALUE 0.
       01  TOT-PAY PIC S9(5)V99 VALUE 0.
       01  X PIC 99.
       01  Y PIC 99.
       01  Z PIC 99.
       01  A PIC 99.
       01 HOLDKEY PIC X(11).
       01  TEST-DATE.
           05  T-CC            PIC 99.
           05  T-YY            PIC 99.
           05  T-MM            PIC 99.
           05  T-DD            PIC 99.
       01  INPUT-DATE.
           02 T-MM PIC XX.
           02 T-DD PIC XX.
           02 T-CC PIC XX.
           02 T-YY PIC XX.

       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT FILEIN CHARCUR GARFILE.
           OPEN OUTPUT ERROR-FILE FILEOUT FILEOUT2.
       P1.
           MOVE SPACE TO FILEIN01
           READ FILEIN
             AT END
               GO TO P9.

           MOVE FI-GARNO TO G-GARNO
           READ GARFILE
             INVALID
               GO TO E1.

           IF FI-DATE = SPACE  OR FI-PROC = SPACE GO TO E1.
           
           MOVE FI-DATE TO INPUT-DATE
           MOVE CORR INPUT-DATE TO TEST-DATE
           MOVE G-GARNO TO CC-KEY8
           MOVE SPACE TO CC-KEY3
           START CHARCUR KEY NOT < CHARCUR-KEY
             INVALID
               GO TO E1.

       P2. 
           READ CHARCUR NEXT
             AT END
               GO TO E1.

           IF CC-KEY8 NOT = G-GARNO GO TO E1.

           IF NOT
              ((CC-DATE-T = TEST-DATE) AND (CC-PROC1 = FI-PROC))
           GO TO P2.

           IF CC-PAYCODE = "001"
             DISPLAY G-GARNAME " " CC-PAYCODE
             ACCEPT ALF1
           GO TO P1.
           
           WRITE FILEOUT201 FROM FILEIN01
           GO TO P1.
           
       E1. 
           WRITE ERROR-FILE01 FROM FILEIN01
           GO TO P1.
       P9.
           CLOSE CHARCUR GARFILE ERROR-FILE FILEOUT FILEOUT2
           STOP RUN.
