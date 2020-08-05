      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. npir036.
       AUTHOR. SWAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC    RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEIN ASSIGN TO "S40" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT PAPEROUT ASSIGN TO "S45" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT GARFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT DIAGFILE ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS IS RANDOM RECORD KEY IS DIAG-KEY
           ALTERNATE RECORD KEY IS DIAG-TITLE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT REFPHY ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS REF-KEY
           ALTERNATE RECORD KEY IS REF-BSNUM  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CRNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-UPIN  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CDNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-NAME  WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT GAPFILE ASSIGN TO "S65" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS GAPKEY
           ALTERNATE RECORD KEY IS GAP-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-STATE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT ERRORFILE ASSIGN TO "S70" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT PROCFILE ASSIGN TO "S75" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS PROC-KEY
           LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PROCFILE
           DATA RECORD PROCFILE01.
       01  PROCFILE01.
           02 PROC-KEY.
             03 PROC-KEY1 PIC X(4).
             03 PROC-KEY2 PIC X(7).
           02 PROC-TYPE PIC X.
           02 PROC-TITLE PIC X(28).
           02 PROC-AMOUNT PIC 9(4)V99.
       
       FD  ERRORFILE
           DATA RECORD IS ERRORFILE01.
       01  ERRORFILE01.
           02 EF1 PIC X(12).
           02 EF2 PIC X(43).
           02 EF3 PIC X(24).
       
       FD  REFPHY
      *    BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS REFPHY01.
       01  REFPHY01.
           02 REF-KEY PIC XXX.
           02 REF-BSNUM PIC X(5).
           02 REF-CRNUM PIC X(6).
           02 REF-UPIN PIC X(6).
           02 REF-CDNUM PIC X(7).
           02 REF-NAME PIC X(24).
           02 REF-NPI PIC X(10).
       FD GAPFILE.
       01 GAPFILE01.
           02 GAPKEY PIC X(7).
           02 GAP-NAME PIC X(25).
           02 GAP-ADDR PIC X(22).
           02 GAP-CITY PIC X(15).
           02 GAP-STATE PIC XX.
           02 GAP-ZIP PIC X(9).
           02 GAP-TYPE PIC X.
           02 GAP-FUTURE PIC X(40).
       FD  DIAGFILE
           BLOCK CONTAINS 15 RECORDS
           DATA RECORD IS DIAG01.
       01  DIAG01.
           02 DIAG-KEY PIC X(7).
           02 DIAG-TITLE PIC X(61).
           02 DIAG-MEDB PIC X(5).
       
       FD GARFILE
           BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS G-MASTER.
       01 G-MASTER.
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
           02 G-DOB.
              03 G-DOBYY PIC X(4).
              03 G-DOBMM PIC XX.
              03 G-DOBDD PIC XX.
           02 G-DUNNING PIC X.
           02 G-ACCTSTAT PIC X.
           02 G-PR-MPLR PIC X(4).
           02 G-PRINS PIC XXX.
           02 G-PR-ASSIGN PIC X.
           02 G-PR-OFFICE PIC X(4).
           02 G-PR-GROUP PIC X(10).
           02 G-PRIPOL0.
             03 G-PRIPOL PIC X(9).
             03 G-PR-SUFX PIC XXX.
             03 G-PR-FILLER PIC X(4).
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-SE-MPLR PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
           02 G-SE-GROUP PIC X(10).
           02 G-SECPOL0.
             03 G-SECPOL PIC X(9).
             03 G-SE-SUFX PIC XXX.
             03 G-SE-FILLER PIC X(4).
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
       FD  PAPEROUT.
       01  PAPEROUT01.
           02 FO-PC PIC 999.
           02 FO-PATID.
             03 FO-PATID7 PIC X(7).
             03 FO-PATID8 PIC X.
           02 FO-KEY PIC X(11).
           02 FO-DATE PIC X(8).
           02 FO-ASSIGN PIC X.
           02 FO-PLACE PIC X.
           02 FO-DOC PIC XX.
           02 filler pic x(16).
           02 fo-paper pic x.
       FD  FILEIN.
       01  FILEIN01 PIC XXX.
       FD FILEOUT.
       01  FILEOUT01 PIC X(160).
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
       01  DIAGFLAG PIC 9.
       01  ALF7 PIC X(7).
       01  NAMEFIRST PIC X(24).
       01  NAMELAST PIC X(24).
       01  ALF16.
           02 ALF16-3. 
              03 ALF16-31 PIC XX.
              03 ALF16-32 PIC X.
           02 ALF16-9 PIC X(9).
           02 ALF-END PIC X(4).
       01 ALF9.
          02 ALF9-1 PIC X.
          02 ALF9-8 PIC X(8).

       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT FILEIN CHARCUR REFPHY GARFILE 
           GAPFILE DIAGFILE PROCFILE.
           OPEN OUTPUT PAPEROUT FILEOUT ERRORFILE.
           MOVE SPACE TO ERRORFILE01
           MOVE "BCBSVT ELECTRONIC CLAIMS ERRORS" TO ERRORFILE01
           WRITE ERRORFILE01.

       P0. 
           READ FILEIN AT END GO TO P6.
           MOVE FILEIN01 TO CC-PAYCODE
           START CHARCUR KEY NOT < CC-PAYCODE INVALID GO TO P0.
       P1. 
           READ CHARCUR NEXT AT END GO TO P0.
           IF CC-PAYCODE NOT = FILEIN01 GO TO P0.
           IF CC-PROC1 < "00100  "
           OR CC-CLAIM = "999995"
           OR CC-REC-STAT > "1"
           OR CC-AMOUNT = 0
           GO TO P1.
           
           IF CC-DOCP = "02"
               MOVE "BAD DOC ## AND/OR DIAG ?" TO EF2
               PERFORM S1 
               GO TO P1
           END-IF 
               
           MOVE CC-KEY8 TO G-GARNO.
           READ GARFILE INVALID MOVE "NO GARNO" TO EF2
            PERFORM S1 GO TO P1.
           IF (CC-PAYCODE NOT = G-PRINS)
           OR (CC-PAPER = "P" OR "O") 
           PERFORM PAPER-1 GO TO P1.
           GO TO TEST-IT.
       PAPER-1.
           move space to paperout01
           move cc-paper to fo-paper
           MOVE CC-PAYCODE TO FO-PC
           MOVE CC-PATID TO FO-PATID
           MOVE CHARCUR-KEY TO FO-KEY
           MOVE CC-DATE-T TO FO-DATE 
           MOVE CC-ASSIGN TO FO-ASSIGN.
           MOVE CC-PLACE TO FO-PLACE 
           MOVE CC-DOCP TO FO-DOC 
           WRITE PAPEROUT01 .
       TEST-IT.
           IF G-DOB NOT NUMERIC
           MOVE "BAD DOB" TO EF2
            PERFORM S1 
            GO TO P1.

           IF G-PRIPOL = SPACE
           MOVE "POLICY MISSING" TO EF2
            PERFORM S1 
            GO TO P1.
    
           MOVE CC-PROC TO PROC-KEY.
           READ PROCFILE INVALID
           MOVE "INVALID PROCEDURE CODE" TO EF2 
            PERFORM S1 
             GO TO P1.
           
           IF CC-DIAG = "0000000" MOVE "NO DIAG" TO EF2 
             PERFORM S1
              GO TO P1.
           IF ( CC-DIAG > "79999" AND < "90000" )
            AND ( CC-DAT1 = "00000000" )
            MOVE CC-DATE-T TO CC-DAT1
            MOVE "NO ACC. DATE, BUT SENT" TO EF2
             PERFORM S1 
            END-IF.
           MOVE CC-DIAG TO DIAG-KEY
           READ DIAGFILE INVALID MOVE "OLD DIAG CODE" TO EF2
            MOVE CC-DIAG TO EF3 
             PERFORM S1 
              GO TO P1.
           MOVE 0 TO DIAGFLAG
           IF CC-DX2 NOT = "0000000" MOVE CC-DX2 TO ALF7
            MOVE 0 TO DIAGFLAG
            PERFORM DIAG-CHECK.
           IF DIAGFLAG = 1 MOVE "OLD DX2 CODE" TO EF2 
             PERFORM S1
              GO TO P1.
           IF CC-DX3 NOT = "0000000" MOVE CC-DX3 TO ALF7
            MOVE 0 TO DIAGFLAG
            PERFORM DIAG-CHECK.
           IF DIAGFLAG = 1 MOVE "OLD DX3 CODE" TO EF2 
             PERFORM S1
              GO TO P1.
           IF CC-DX4 NOT = "0000000" MOVE CC-DX4 TO ALF7
            MOVE 0 TO DIAGFLAG
            PERFORM DIAG-CHECK.
           IF DIAGFLAG = 1 MOVE "OLD DX4 CODE" TO EF2 
             PERFORM S1
              GO TO P1.
           IF CC-PROC1 > "99240" AND < "99280"
            AND CC-DOCR = "000"
            MOVE "CONSULT NEEDS REF MD" TO EF2 
             PERFORM S1 
              GO TO P1.
           IF CC-DOCR = "000" 
           WRITE FILEOUT01 FROM CHARCUR01 
           GO TO P1.
           MOVE CC-DOCR TO REF-KEY.
           READ REFPHY INVALID MOVE "INVALID" TO REF-CDNUM
           MOVE SPACE TO REF-NAME.
           IF  (REF-NPI = SPACE)
            MOVE SPACE TO EF2
            STRING CC-DOCR " " REF-NAME " NO NPI"
            DELIMITED BY "**" INTO EF2
             PERFORM S1 
              GO TO P1.
           MOVE SPACE TO NAMELAST NAMEFIRST
           UNSTRING REF-NAME DELIMITED BY 
           "; " OR ";" OR " ; " OR " ," OR ", " OR " , " OR ","   
           INTO NAMELAST NAMEFIRST
            IF NAMEFIRST = SPACE
            STRING CC-DOCR  " " REF-NAME " NAME FORMAT ERROR" 
            DELIMITED BY "!!" INTO EF2 
            PERFORM S1
            GO TO P1.
           WRITE FILEOUT01 FROM CHARCUR01 
           GO TO P1.
       
       DIAG-CHECK.
           MOVE 0 TO DIAGFLAG
           MOVE ALF7 TO DIAG-KEY
           READ DIAGFILE INVALID MOVE 1 TO DIAGFLAG.

       S1. 
           MOVE CHARCUR-KEY TO EF1 
           MOVE G-GARNAME TO EF3
           WRITE ERRORFILE01.

       P6. CLOSE FILEOUT PAPEROUT ERRORFILE.
           CLOSE GARFILE DIAGFILE REFPHY GAPFILE PROCFILE
           STOP RUN.
