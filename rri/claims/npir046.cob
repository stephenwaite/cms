      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. npir046.
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
           
           SELECT PLACEFILE ASSIGN TO "S80" ORGANIZATION 
            LINE SEQUENTIAL.
           
           SELECT PARMFILE ASSIGN TO "S85" ORGANIZATION 
            LINE SEQUENTIAL.
           
           SELECT PROVCAID ASSIGN TO "S90" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS PROV-KEY
           ALTERNATE RECORD KEY IS PROV-NPI WITH DUPLICATES
           ALTERNATE RECORD KEY IS PROV-TAX WITH DUPLICATES
           ALTERNATE RECORD KEY IS PROV-NAME WITH DUPLICATES
           LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PROVCAID.
       01  PROVCAID01.
           02 PROV-KEY PIC X(7).
           02 PROV-NAME PIC X(24).
           02 PROV-NPI PIC X(10).
           02 PROV-TAX PIC X(10).
           02 PROV-STREET PIC X(20).
           02 PROV-CITY PIC X(20).
           02 PROV-STATE PIC XX.
           02 PROV-ZIP PIC X(5).

       FD  PARMFILE.
       01  PARMFILE01 PIC X(7).
       FD  PLACEFILE.
       01  PLACEFILE01.
           02 DF1 PIC X.
           02 DF2 PIC X.
           02 DF3 PIC X(22).
           02 DF4 PIC X(18).
           02 DF5 PIC X(15).
           02 DF6 PIC XX.
           02 DF7 PIC X(9).
       
       FD  PROCFILE
           DATA RECORD PROCFILE01.
       01  PROCFILE01.
           02 PROC-KEY.
             03 PROC-KEY1 PIC X(4).
             03 PROC-KEY2 PIC X(7).
           02 PROC-TYPE PIC X.
           02 PROC-TITLE PIC X(28).
           02 PROC-AMOUNT PIC 9(4)V99.

       FD  ERRORFILE.
       01  ERRORFILE01.
           02 EF1 PIC X(34).
           02 FILLER PIC X.
           02 EF2 PIC X(22).
           02 FILLER PIC X.
           02 EF3 PIC X(22).       

       FD  REFPHY.
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
           
       FD  DIAGFILE.
       01  DIAG01.
           02 DIAG-KEY PIC X(7).
           02 DIAG-TITLE PIC X(61).
           02 DIAG-MEDB PIC X(5).
       
       FD GARFILE.
           COPY garfile.CPY IN "C:\Users\sid\cms\copylib\rri".           
           
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
           02 FO-FILLER PIC X(16).
           02 FO-PAPER PIC X.

       FD  FILEIN.
       01  FILEIN01 PIC XXX.

       FD  FILEOUT.
       01  FILEOUT01 PIC X(160).

       FD  CHARCUR.
           COPY charcur.CPY IN "C:\Users\sid\cms\copylib\rri".                     

       WORKING-STORAGE SECTION.
       01  PLACE-TAB01.
           02 PLACE-TAB OCCURS 29 TIMES.
             03 PL-TAB PIC X.
             03 PL-NUM PIC X.
             03 PL-NAME PIC X(22).
             03 PL-STREET PIC X(18).
             03 PL-CITY PIC X(15).
             03 PL-STATE PIC XX.
             03 PL-ZIP PIC X(9).

       01  PLINDX PIC 99 VALUE 0.
       01  DIAGFLAG PIC 9.
       01  ALF7 PIC X(7).
       01  FLAG PIC 9.
       01  Y PIC 99.
      * 01  CNTR PIC 99 VALUE 0.
       01  NAMEFIRST PIC X(24).
       01  NAMELAST PIC X(24).

       PROCEDURE DIVISION.

       0005-START.
           OPEN INPUT PLACEFILE FILEIN REFPHY GARFILE 
           GAPFILE DIAGFILE PROCFILE PARMFILE PROVCAID.
           OPEN OUTPUT PAPEROUT FILEOUT ERRORFILE.
           MOVE "MEDICAID ELECTRONIC CLAIMS ERRORS" TO ERRORFILE01
           WRITE ERRORFILE01
           MOVE SPACE TO ERRORFILE01.
           MOVE SPACE TO FO-FILLER.
           OPEN I-O CHARCUR.
           READ PARMFILE AT END GO TO P00.

       P00. 
           READ PLACEFILE AT END GO TO P0.
           ADD 1 TO PLINDX.
           MOVE DF1 TO PL-TAB(PLINDX)
           MOVE DF2 TO PL-NUM(PLINDX)
           MOVE DF3 TO PL-NAME(PLINDX)
           MOVE DF4 TO PL-STREET(PLINDX)
           MOVE DF5 TO PL-CITY(PLINDX)
           MOVE DF6 TO PL-STATE(PLINDX)
           MOVE DF7 TO PL-ZIP(PLINDX)
           GO TO P00.
       
       P0. 
           READ FILEIN AT END GO TO P6.
           MOVE FILEIN01 TO CC-PAYCODE
           START CHARCUR KEY NOT < CC-PAYCODE INVALID GO TO P0.

       P1. 
           MOVE SPACE TO ERRORFILE01

           READ CHARCUR NEXT WITH LOCK AT END GO TO P0.
           
           IF CC-PAYCODE NOT = FILEIN01 GO TO P0.
           
           IF CC-PROC1 < "00100  "
             OR CC-CLAIM = "999995"
             OR CC-REC-STAT > "1"
             GO TO P1.

           MOVE CC-KEY8 TO G-GARNO.
           READ GARFILE 
             INVALID
               MOVE "NO GARNO" TO EF2
               PERFORM S1 
               GO TO P1
           END-READ    

           IF (FILEIN01 NOT = G-PRINS)
             AND (FILEIN01 NOT = G-SEINS)
             AND (FILEIN01 NOT = G-TRINS)
             MOVE SPACE TO ERRORFILE01
             MOVE "NO INS. MATCH" TO EF2
             PERFORM S1
             GO TO P1
           END-IF    

           IF (G-DOB = "00000000")
               OR (G-DOB NOT NUMERIC)
               MOVE "NO D.O.B.   " TO EF2
               PERFORM S1
               GO TO P1
           END-IF    

           IF CC-DOCP = "02"
               MOVE "02 FOR DOCP, STILL UNREAD?" TO EF2
               PERFORM S1
               GO TO P1
           END-IF    

           IF (G-PRINS = CC-PAYCODE)
             AND ((G-PRIPOL(1:9) NUMERIC) OR (G-PRIPOL = SPACE))
             STRING G-PRIPOL " BAD POLICY #"
               DELIMITED BY SIZE INTO EF2
             PERFORM S1
             GO TO P1.

           IF (G-SEINS = CC-PAYCODE)
               AND ((G-SECPOL(1:9) NUMERIC) OR (G-SECPOL = SPACE))
               STRING G-SECPOL " BAD POLICY #" 
                 DELIMITED BY SIZE INTO EF2
               PERFORM S1
               GO TO P1
           END-IF    

           IF (G-BILLADD = SPACE) AND (G-STREET = SPACE)
             MOVE "NO ADDRESS" TO EF2
             PERFORM S1
             GO TO P1
           END-IF    

           IF CC-PAPER = "O"
             PERFORM PAPER-1
           GO TO P1.

           IF (G-TRINS  = "004" OR "281")
             PERFORM PAPER-1
           GO TO P1.

           IF (G-SEINS = "004" OR "281") GO TO P1.

       TEST-IT.
           IF CC-DIAG = "00000"
               MOVE "NO DIAG" TO EF2
               PERFORM S1
               GO TO P1
           END-IF    

           MOVE 0 TO DIAGFLAG
           MOVE CC-DIAG TO DIAG-KEY
           READ DIAGFILE 
             INVALID
               STRING "OLD DIAG CODE " CC-DIAG 
                 DELIMITED BY SIZE INTO EF2 
               PERFORM S1
               GO TO P1.

           IF CC-DX2 NOT = "0000000"
             MOVE CC-DX2 TO ALF7
             MOVE 0 TO DIAGFLAG
             PERFORM DIAG-CHECK.

           IF DIAGFLAG = 1
             STRING "OLD DX2 CODE " CC-DX2
               DELIMITED BY SIZE INTO EF2 
             PERFORM S1
             GO TO P1.

           IF CC-DX3 NOT = "0000000"
             MOVE CC-DX3 TO ALF7
             MOVE 0 TO DIAGFLAG
             PERFORM DIAG-CHECK.

           IF DIAGFLAG = 1
               STRING "OLD DX3 CODE " CC-DX3 
                 DELIMITED BY SIZE INTO EF2
               PERFORM S1
               GO TO P1
           END-IF 

           IF CC-DX4 NOT = "0000000"
             MOVE CC-DX4 TO ALF7
             MOVE 0 TO DIAGFLAG
             PERFORM DIAG-CHECK.

           IF DIAGFLAG = 1
             STRING "OLD DX4 CODE " CC-DX4 
               DELIMITED BY SIZE INTO EF3
             PERFORM S1
             GO TO P1.

           IF (G-SEINS = "004" OR "281")
             OR (G-TRINS = "004" OR "281")
             PERFORM PAPER-1
             GO TO P1.

           MOVE CC-DOCR TO REF-KEY
           READ REFPHY INVALID 
               STRING CC-DOCR " IS AN INVALID REF-KEY" 
                   DELIMITED BY "**" INTO EF2
               PERFORM S1 
               GO TO P1
           END-READ
            
           IF REF-NPI = SPACE
               STRING CC-DOCR " NPI MISSING          "
                   DELIMITED BY "**" INTO EF2
               PERFORM S1 
               GO TO P1
           END-IF. 

       TEST-IT2.
           MOVE CC-PROC TO PROC-KEY.
           READ PROCFILE INVALID
               MOVE "INVALID PROCEDURE CODE" TO EF2
               PERFORM S1
               GO TO P1
           END-READ

           WRITE FILEOUT01 FROM CHARCUR01.
           GO TO P1.

       DIAG-CHECK.
           MOVE 0 TO DIAGFLAG
           MOVE ALF7 TO DIAG-KEY
           READ DIAGFILE 
             INVALID 
               MOVE 1 TO DIAGFLAG.

       S1.
           STRING CHARCUR-KEY " " CC-PROC1 " " CC-AMOUNT " " 
             CC-DATE-T(5:2) "-" CC-DATE-T(7:2) "-" CC-DATE-T(3:2)
               DELIMITED BY SIZE INTO EF1 

           MOVE CC-KEY8 TO G-GARNO
           READ GARFILE 
             INVALID
               MOVE SPACE TO G-GARNAME.

           MOVE G-GARNAME TO EF3
           WRITE ERRORFILE01.

       PAPER-1.
           MOVE CC-PAYCODE TO FO-PC
           MOVE CC-PATID TO FO-PATID
           MOVE CHARCUR-KEY TO FO-KEY
           MOVE CC-DATE-T TO FO-DATE 
           MOVE CC-ASSIGN TO FO-ASSIGN.
           MOVE CC-PLACE TO FO-PLACE 
           MOVE CC-DOCP TO FO-DOC 
           MOVE CC-PAPER TO FO-PAPER
           WRITE PAPEROUT01.

       P6. CLOSE FILEOUT PAPEROUT ERRORFILE.
           CLOSE GARFILE DIAGFILE REFPHY GAPFILE PROCFILE
           STOP RUN.
