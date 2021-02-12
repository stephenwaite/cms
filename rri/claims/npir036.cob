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

       FD  PROCFILE.
           copy procfile.cpy in "c:\users\sid\cms\copylib\rri".

       FD  ERRORFILE.
       01  ERRORFILE01.
           02 EF1 PIC X(34).
           02 filler pic x.
           02 EF2 PIC X(22).
           02 filler pic x.
           02 EF3 PIC X(22).
       
       FD  REFPHY.
           copy refphy.cpy in "C:\Users\sid\cms\copylib".
           
       FD  GAPFILE.
           copy gapfile.cpy in "C:\Users\sid\cms\copylib".      

       FD  DIAGFILE.
           copy diagfile.cpy in "C:\Users\sid\cms\copylib".
       
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
           02 filler pic x(16).
           02 fo-paper pic x.

       FD  FILEIN.
       01  FILEIN01 PIC XXX.

       FD  FILEOUT.
       01  FILEOUT01 PIC X(160).

       FD  CHARCUR.
           COPY charcur.CPY IN "C:\Users\sid\cms\copylib\rri".           

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
       01  ALF9.
           02 ALF9-1 PIC X.
           02 ALF9-8 PIC X(8).

       PROCEDURE DIVISION.

       0005-START.
           OPEN INPUT FILEIN CHARCUR REFPHY GARFILE 
             GAPFILE DIAGFILE PROCFILE.
           
           OPEN OUTPUT PAPEROUT FILEOUT ERRORFILE.
           
           MOVE "BCBSVT ELECTRONIC CLAIMS ERRORS" TO ERRORFILE01
           WRITE ERRORFILE01.

       P0. 
           READ FILEIN
             AT END
               GO TO P6.

           MOVE FILEIN01 TO CC-PAYCODE
           START CHARCUR KEY NOT < CC-PAYCODE
             INVALID
               GO TO P0.

       P1. 
           MOVE SPACE TO ERRORFILE01

           READ CHARCUR NEXT
             AT END
               GO TO P0.

           IF CC-PAYCODE NOT = FILEIN01 GO TO P0.

           IF CC-PROC1 < "00100  "
             OR CC-CLAIM = "999995"
             OR CC-REC-STAT > "1"
             OR CC-AMOUNT = 0
             GO TO P1.                      
               
           MOVE CC-KEY8 TO G-GARNO.
           
           READ GARFILE INVALID 
               MOVE "NO GARNO" TO EF2
               PERFORM S1
               GO TO P1
           END-READ

           MOVE G-GARNAME TO EF3

           IF CC-DOCP = "02"
             MOVE "BAD DOC ##, UNREAD?" TO EF2
             PERFORM S1 
             GO TO P1
           END-IF

           IF (CC-PAYCODE NOT = G-PRINS)
               OR (CC-PAPER = "P" OR "O") 
               PERFORM PAPER-1
               GO TO P1
           END-IF
               
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
           WRITE PAPEROUT01.

       TEST-IT.
           IF G-DOB NOT NUMERIC
             STRING "BAD DOB " G-DOB DELIMITED BY SIZE INTO EF2
             PERFORM S1 
             GO TO P1.

           IF G-PRIPOL = SPACE
             MOVE "POLICY MISSING" TO EF2
             PERFORM S1 
             GO TO P1.
    
           MOVE CC-PROC TO PROC-KEY.
           READ PROCFILE 
             INVALID
               MOVE "INVALID PROCEDURE CODE" TO EF2 
             PERFORM S1 
             GO TO P1.
           
           IF CC-DIAG = "0000000"
             MOVE "NO DIAG" TO EF2 
             PERFORM S1
             GO TO P1.

           IF (CC-DIAG > "79999" AND < "90000" )
             AND (CC-DAT1 = "00000000" )
             MOVE CC-DATE-T TO CC-DAT1
             MOVE "NO ACC. DATE, BUT SENT" TO EF2
             PERFORM S1 
             END-IF.

           MOVE CC-DIAG TO DIAG-KEY
           READ DIAGFILE
             INVALID
               STRING "OLD DIAG CODE " CC-DIAG 
                 DELIMITED BY SIZE INTO EF2  
               PERFORM S1 
               GO TO P1.
               
           MOVE 0 TO DIAGFLAG
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
             GO TO P1.

           IF CC-DX4 NOT = "0000000" 
             MOVE CC-DX4 TO ALF7
             MOVE 0 TO DIAGFLAG
             PERFORM DIAG-CHECK.

           IF DIAGFLAG = 1 
             STRING "OLD DX4 CODE " CC-DX4
               DELIMITED BY SIZE INTO EF2 
             PERFORM S1
             GO TO P1.
                                  
           MOVE CC-DOCR TO REF-KEY.
           READ REFPHY 
             INVALID 
               MOVE "INVALID" TO REF-CDNUM
               MOVE SPACE TO REF-NAME.

           IF (REF-NPI = SPACE)
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
           READ DIAGFILE 
             INVALID 
               MOVE 1 TO DIAGFLAG.

       S1. 
           STRING CHARCUR-KEY " " CC-PROC1 " " CC-AMOUNT " " 
             CC-DATE-T(5:2) "-" CC-DATE-T(7:2) "-" CC-DATE-T(3:2)
             DELIMITED BY SIZE INTO EF1
           WRITE ERRORFILE01.

       P6. CLOSE FILEIN FILEOUT CHARCUR PAPEROUT ERRORFILE.
           CLOSE GARFILE DIAGFILE REFPHY GAPFILE PROCFILE
           STOP RUN.
