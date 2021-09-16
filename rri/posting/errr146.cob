      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
        IDENTIFICATION DIVISION.
       PROGRAM-ID. errr146.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT GARFILE ASSIGN TO "S35"     ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC        RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT CHARCUR ASSIGN TO "S40"     ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC        RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT ERROR-FILE ASSIGN TO "S45" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S50" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT PAYFILE ASSIGN TO "S55"     ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC        RECORD KEY IS PAYFILE-KEY
           LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ERROR-FILE.
       01  ERROR-FILE01 PIC X(132).
       FD  FILEOUT.
       01  FILEOUT01 PIC X(160).
       FD  FILEIN.
       01  FILEIN01.
           02 F1-NAME PIC X(10).
           02 FILLER PIC X VALUE SPACE.
           02 FI-PRIPOL PIC X(9).
           02 FILLER PIC X VALUE SPACE.
           02 FI-DATE PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 FI-PROC PIC X(5).
           02 FI-MOD PIC XX.
           02 FI-MOD2 PIC XX.
           02 FI-MOD3 PIC XX.
           02 FILLER PIC X VALUE SPACE.
           02 FI-GARNO PIC X(8).
           02 FILLER PIC X(5).
           02 FI-CHARGE PIC X(7).
           02 FILLER PIC X.
           02 FI-SIGN PIC X.
           02 FILLER PIC X.
           02 FI-PAID PIC X(7).
           02 FILLER PIC X(17).
           02 FI-STATUS PIC X(3).
           02 FILLER PIC X(46).
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
       FD GARFILE
           BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS G-MASTER.
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
       01  TEST-DATE.
           05  T-CC            PIC XX.
           05  T-YY            PIC XX.
           05  T-MM            PIC XX.
           05  T-DD            PIC XX.
       01  INPUT-DATE.
           02 T-MM PIC XX.
           02 T-DD PIC XX.
           02 T-CC PIC XX.
           02 T-YY PIC XX.
       01  FI-DOLLAR-PAID PIC X(4).
       01  FI-CENT-PAID PIC XX.
       01  ALF6.
           02 ALF4 PIC X(4).
           02 ALF2 PIC XX.
       01  NUM6 PIC 9(6).
       01  FI-CHG PIC S9(4)V99.
       01  PAYBACK01 PIC X(80).
       01  PAYDATE PIC X(8).
       01  XYZ PIC 999.
       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT FILEIN CHARCUR GARFILE
           OUTPUT ERROR-FILE FILEOUT
           I-O PAYFILE.
           MOVE SPACE TO ERROR-FILE01
           READ FILEIN AT END CONTINUE.
           MOVE FILEIN01(1:8) TO PAYDATE.
       P1.
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P9.
           MOVE FI-GARNO TO G-GARNO
           READ GARFILE INVALID GO TO E1.
           IF FI-PAID = "   0.00" GO TO E1.
           IF FI-DATE = SPACE GO TO E1.
           IF FI-STATUS = "22 " GO TO E1.
           MOVE FI-DATE TO INPUT-DATE
           MOVE CORR INPUT-DATE TO TEST-DATE
           MOVE SPACE TO  FI-DOLLAR-PAID FI-CENT-PAID
           UNSTRING FI-CHARGE DELIMITED BY "." INTO
                        FI-DOLLAR-PAID FI-CENT-PAID
           INSPECT FI-DOLLAR-PAID REPLACING ALL " " BY "0"
           MOVE FI-DOLLAR-PAID TO ALF4
           MOVE FI-CENT-PAID TO ALF2
           MOVE ALF6 TO NUM6
           COMPUTE FI-CHG = (NUM6 / 100)
      *     DISPLAY FI-CHG
      *     ACCEPT ALF2
           MOVE SPACE TO  FI-DOLLAR-PAID FI-CENT-PAID
           UNSTRING FI-PAID DELIMITED BY "." INTO
                        FI-DOLLAR-PAID FI-CENT-PAID
           INSPECT FI-DOLLAR-PAID REPLACING ALL " " BY "0"
           MOVE FI-DOLLAR-PAID TO ALF4
           INSPECT FI-CENT-PAID REPLACING ALL " " BY "0"
           MOVE FI-CENT-PAID TO ALF2
           MOVE ALF6 TO NUM6
           MOVE SPACE TO PD-DENIAL
           COMPUTE PD-AMOUNT =  -1 * (NUM6 / 100)
           IF FI-SIGN = "-"
            MOVE "08" TO PD-DENIAL
            COMPUTE PD-AMOUNT =  -1 * PD-AMOUNT
           END-IF.
           MOVE G-GARNO TO CC-KEY8
           MOVE SPACE TO CC-KEY3
           START CHARCUR KEY NOT < CHARCUR-KEY INVALID GO TO E1.
       P2. 
           READ CHARCUR NEXT AT END GO TO E1.
           IF CC-KEY8 NOT = G-GARNO GO TO E1.
           IF CC-DATE-T NOT = TEST-DATE GO TO P2.
           IF (CC-PROC1 = FI-PROC) AND
              (CC-PROC2 = FI-MOD) AND
              (CC-MOD2 = FI-MOD2) AND
              (CC-MOD3 = FI-MOD3) GO TO P3.
      *      DISPLAY G-GARNAME
      *      DISPLAY CC-AMOUNT
      *      DISPLAY FI-CHG
      *      DISPLAY FI-PROC
      *      ACCEPT ALF2.
           IF (FI-PROC = SPACE) AND (CC-AMOUNT = FI-CHG) GO TO P3.
           GO TO P2.
       P3.
           MOVE PAYDATE TO PD-DATE-T
           MOVE PAYDATE TO PD-DATE-E
           ACCEPT PD-ORDER FROM TIME
           MOVE SPACE TO PD-BATCH
           MOVE CC-CLAIM TO PD-CLAIM
           MOVE G-GARNO TO PD-KEY8
           MOVE SPACE TO PD-KEY3
           MOVE G-GARNAME TO PD-NAME
           MOVE "004" TO PD-PAYCODE.
           MOVE PAYFILE01 TO PAYBACK01
           MOVE 0 TO XYZ.
           PERFORM P4 THRU P5.
           WRITE FILEOUT01 FROM CHARCUR01.
           GO TO P1. 
       P4.
           ADD 1 TO XYZ
           MOVE XYZ TO PD-KEY3
           READ PAYFILE INVALID KEY GO TO P5.
           GO TO P4.
       P5.
           MOVE PAYBACK01 TO PAYFILE01
           MOVE XYZ TO PD-KEY3
           WRITE PAYFILE01
           DISPLAY PAYFILE-KEY " " PD-NAME.
           DISPLAY "RECORD IS ADDED".
       E1. WRITE ERROR-FILE01 FROM FILEIN01
           GO TO P1.  
       P9.
           CLOSE CHARCUR GARFILE FILEOUT ERROR-FILE PAYFILE
           STOP RUN.
