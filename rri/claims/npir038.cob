      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. npir038.
       AUTHOR. SWAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS CHARCUR-KEY
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

           SELECT FILEOUT2 ASSIGN TO "S85" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  PLACEFILE.
       01  PLACEFILE01.
           02 DF1 PIC X.
           02 DF2 PIC X.
           02 DF3 PIC X(22).
           02 DF4 PIC X(18).
           02 DF5 PIC X(15).
           02 DF6 PIC XX.
           02 DF7 PIC X(9).

       FD  PROCFILE.
           COPY procfile.CPY IN "C:\Users\sid\cms\copylib\rri".           
      
       FD  ERRORFILE
           DATA RECORD IS ERRORFILE01.
       01  ERRORFILE01.
           02 EF1 PIC X(34).
           02 FILLER PIC X.
           02 EF2 PIC X(22).
           02 FILLER PIC X.
           02 EF3 PIC X(22).
           
       FD  REFPHY.
           COPY refphy.CPY IN "C:\Users\sid\cms\copylib".           
      
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
           DATA RECORD IS DIAG01.
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

       FD  FILEIN.
       01  FILEIN01 PIC XXX.

       FD  FILEOUT.
       01  FILEOUT01 PIC X(160).

       FD  FILEOUT2.
       01  FILEOUT201 PIC X(160).

       FD  CHARCUR.
           COPY charcur.CPY IN "C:\Users\sid\cms\copylib\rri".           

       WORKING-STORAGE SECTION.
       01  PLACE-TAB01.
           02 PLACE-TAB OCCURS 26 TIMES.
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
       01  CNTR PIC 99 VALUE 0.
       01  NAMEFIRST PIC X(24).
       01  NAMELAST PIC X(24).
       01  CLM-5 PIC XX.
       01  MD01.
           02 MD1 PIC X.
           02 MD2 PIC X.
           02 MD3 PIC X.
           02 MD4 PIC X.
           02 MD5 PIC X.
           02 MD6 PIC X.
           02 MD7 PIC X.
           02 MD8 PIC X.
           02 MD9 PIC X.
           02 MD10 PIC X.
           02 MD11 PIC X.

       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT PLACEFILE FILEIN CHARCUR REFPHY GARFILE 
               GAPFILE DIAGFILE PROCFILE.
           OPEN OUTPUT PAPEROUT FILEOUT FILEOUT2 ERRORFILE.
           MOVE SPACE TO ERRORFILE01
           MOVE "MEDICARE ELECTRONIC CLAIMS ERRORS" TO ERRORFILE01
           WRITE ERRORFILE01.

       P00.
           READ PLACEFILE
             AT END
               GO TO P0
           END-READ

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
           READ FILEIN
             AT END
               GO TO P6
           END-READ

           MOVE FILEIN01 TO CC-PAYCODE

           START CHARCUR KEY NOT < CC-PAYCODE
             INVALID
               GO TO P0
           END-START.    

       P1. 
           MOVE SPACE TO ERRORFILE01           
           
           READ CHARCUR NEXT
             AT END
               GO TO P0
           END-READ

           IF CC-PAYCODE NOT = FILEIN01
               GO TO P0
           END-IF    
           
           IF CC-REC-STAT > "1"
               GO TO P1
           END-IF 

           MOVE CC-KEY8 TO G-GARNO
           READ GARFILE
             INVALID 
               MOVE "NO GARNO" TO EF2
               PERFORM S1 
               GO TO P1
           END-READ    

           MOVE G-GARNAME TO EF3
           
           IF G-DOB NOT NUMERIC
               STRING "BAD DOB " G-DOB DELIMITED BY SIZE INTO EF2
               PERFORM S1 
               GO TO P1
           END-IF

           IF G-BILLADD = SPACE AND G-STREET = SPACE
               MOVE "MISSING STREET ADDRESS" TO EF2
               PERFORM S1 
               GO TO P1
           END-IF    

           IF NOT (G-PRINS = "003" OR 
                   G-SEINS = "003" OR
                   G-TRINS = "003")
               MOVE "NO MEDICARE INSURANCE DEFINED" TO EF2
               PERFORM S1 
               GO TO P1
           END-IF

           IF (G-PRINS = "003" AND G-SEINS = "003") OR
              (G-PRINS = "003" AND G-TRINS = "003")
               MOVE "2 MEDICARE INSURANCES DEFINED!" TO EF2
               PERFORM S1 
               GO TO P1 
           END-IF               

           IF (G-SEINS = "003" AND CC-PAPER = "E")
               WRITE FILEOUT201 FROM CHARCUR01
               GO TO P1
           END-IF

           IF (G-TRINS = "003")
               PERFORM PAPER-1
               GO TO P1
           END-IF
           
           IF (G-SEINS = "004" OR "005" OR "062")
               IF ((G-SECPOL = SPACE) OR (G-SENAME = SPACE)
                    OR (G-SE-RELATE = SPACE))
                   MOVE "MISSING 2ND-DARY INSURANCE INFO" TO EF2
                   PERFORM S1
                   GO TO P1
               END-IF
           END-IF

           IF G-PRIPOL(1:9) NOT NUMERIC
               MOVE 0 TO FLAG
               PERFORM MBI-CHECK
               IF FLAG = 1
                   STRING "MEDICARE POLICY IS INVALID " G-PRIPOL(1:11)
                     DELIMITED BY SIZE INTO EF2
                   PERFORM S1 
                   GO TO P1           
               END-IF
           END-IF   
           
           IF G-ZIP(1:5) NOT NUMERIC 
               STRING "BAD ZIP CODE " G-ZIP(1:5)
                 DELIMITED BY SIZE INTO EF2
               PERFORM S1 
               GO TO P1
           END-IF     

           IF CC-DOCP = "02"
               MOVE "BAD DOC ##, UNREAD?" TO EF2
               PERFORM S1 
               GO TO P1
           END-IF                      
                           
           IF CC-PAPER = "P" OR "O"
               PERFORM PAPER-1
               GO TO P1
           END-IF    

           IF CC-PAYCODE = "003"
               GO TO TEST-IT
           END-IF    

           GO TO P1.

       PAPER-1.
           MOVE CC-PAYCODE TO FO-PC
           MOVE CC-PATID TO FO-PATID
           MOVE CHARCUR-KEY TO FO-KEY
           MOVE CC-DATE-T TO FO-DATE 
           MOVE CC-ASSIGN TO FO-ASSIGN.
           MOVE CC-PLACE TO FO-PLACE 
           MOVE CC-DOCP TO FO-DOC 
           WRITE PAPEROUT01.

       TEST-IT.           
           IF CC-DIAG = "0000000"
             MOVE SPACE TO EF2
             MOVE "NO DIAG" TO EF2 
             PERFORM S1
             GO TO P1
           END-IF    

           MOVE CC-DIAG TO DIAG-KEY
           READ DIAGFILE INVALID 
             STRING "OLD DIAG CODE " CC-DIAG 
               DELIMITED BY SIZE INTO EF2 
             PERFORM S1 
             GO TO P1
           END-READ
           
           MOVE 0 TO DIAGFLAG

           IF CC-DX2 NOT = "0000000"
               MOVE CC-DX2 TO ALF7
               MOVE 0 TO DIAGFLAG
               PERFORM DIAG-CHECK
           END-IF

           IF DIAGFLAG = 1 
             STRING "OLD DX2 CODE " CC-DX2
               DELIMITED BY SIZE INTO EF2 
             PERFORM S1
             GO TO P1
           END-IF

           IF CC-DX3 NOT = "0000000"
               MOVE CC-DX3 TO ALF7
               MOVE 0 TO DIAGFLAG
               PERFORM DIAG-CHECK
           END-IF

           IF DIAGFLAG = 1 
               STRING "OLD DX3 CODE " CC-DX3 
                 DELIMITED BY SIZE INTO EF2
               PERFORM S1
               GO TO P1
           END-IF

           IF CC-DX4 NOT = "0000000"
               MOVE CC-DX4 TO ALF7
               MOVE 0 TO DIAGFLAG
               PERFORM DIAG-CHECK
           END-IF

           IF DIAGFLAG = 1 
               STRING "OLD DX4 CODE " CC-DX4 
                 DELIMITED BY SIZE INTO EF3
               PERFORM S1
               GO TO P1
           END-IF    
                     
           MOVE CC-DOCR TO REF-KEY.
           READ REFPHY
             INVALID
               MOVE "INVALID" TO REF-CDNUM
               MOVE SPACE TO REF-NAME
           END-READ

           IF REF-CDNUM = "INVALID"
               STRING CC-DOCR " IS NOT A VALID REF. PHYS. CODE"
               DELIMITED BY "**" INTO EF2
               PERFORM S1 
               GO TO P1
           END-IF    

           IF REF-NPI NOT NUMERIC
               STRING CC-DOCR " / HAS NO NPI" DELIMITED BY "**" INTO EF2
               PERFORM S1 
               GO TO P1
           END-IF

           IF REF-NAME NOT = SPACE
               MOVE SPACE TO NAMELAST NAMEFIRST
               UNSTRING REF-NAME DELIMITED BY 
               "; " OR ";" OR " ; " OR " ," OR ", " OR " , " OR ","  
               INTO NAMELAST NAMEFIRST
           
               IF NAMEFIRST = SPACE
                   MOVE SPACE TO EF2
                   STRING CC-DOCR  " " REF-NAME
                          " NAME OF REFERRING ERROR" 
                   DELIMITED BY "!!" INTO EF2 
                   PERFORM S1
                   GO TO P1
               END-IF
           END-IF

           MOVE CC-PROC TO PROC-KEY.
           READ PROCFILE
             INVALID
               MOVE SPACE TO EF2
               MOVE "INVALID PROCEDURE CODE" TO EF2
               PERFORM S1
               GO TO P1
           END-READ

           MOVE 0 TO FLAG
           
           IF CC-PLACE = "3" AND CC-DATE-M = "00000000"  
               MOVE SPACE TO EF2
               STRING CC-PROC1 " / " CC-PLACE
                      " IN-PATIENT ADMIT DATE MISSING"
               DELIMITED BY "!" INTO EF2
               PERFORM S1 
               GO TO P1
           END-IF
           
           WRITE FILEOUT01 FROM CHARCUR01 
           GO TO P1.

       GAP-1.
           MOVE CC-KEY8 TO G-GARNO
           READ GARFILE
             INVALID
               MOVE 1 TO FLAG 
               MOVE SPACE TO EF2
               STRING CC-KEY8 " BAD ACCOUNT #" DELIMITED BY "//"
               INTO EF2
               PERFORM S1 
               GO TO GAP-1-EXIT
           END-READ

           MOVE G-GARNAME TO EF3
           MOVE G-PR-GROUP TO GAPKEY
           READ GAPFILE
             INVALID 
               MOVE SPACE TO EF2
               STRING G-GARNO " INVALID MEDIGAP CODE" DELIMITED BY "//"
               INTO EF2
               PERFORM S1 
               MOVE 1 TO FLAG 
               GO TO GAP-1-EXIT
           END-READ

           IF GAP-TYPE = "X" OR "Y"
               MOVE 1 TO FLAG.
           
           IF (G-SECPOL = SPACE) OR (G-SENAME = SPACE)
               OR (G-SE-RELATE = SPACE)
               MOVE SPACE TO EF2
               STRING G-GARNO " MISSING 2ND-DARY INSURANCE " 
               DELIMITED BY "//" INTO EF2
               PERFORM S1
               GO TO GAP-1-EXIT
           END-IF.

       GAP-1-EXIT.
           EXIT.

       DIAG-CHECK.
           MOVE 0 TO DIAGFLAG
           MOVE ALF7 TO DIAG-KEY
           READ DIAGFILE
             INVALID
               MOVE 1 TO DIAGFLAG
           END-READ.    

       S1. 
           STRING CHARCUR-KEY " " CC-PROC1 " " CC-AMOUNT " " 
             CC-DATE-T(5:2) "-" CC-DATE-T(7:2) "-" CC-DATE-T(3:2)
               DELIMITED BY SIZE INTO EF1 
           WRITE ERRORFILE01.

       MBI-CHECK.
           MOVE G-PRIPOL(1:11) TO MD01
           MOVE 0 TO FLAG
           
           IF (MD1 NOT NUMERIC) OR (MD1 = "0")
               DISPLAY "1ST POSITION NOT NUMERIC  " MD1
               MOVE 1 TO FLAG
           END-IF
           
           IF (MD2 NUMERIC) OR ((MD2 ALPHABETIC) AND 
               (MD2 = "S" OR "L" OR "O" OR "I" OR "B" OR "Z")) OR 
               NOT (MD2 ALPHABETIC  OR MD2 NUMERIC)
               DISPLAY "2ND POSITION IS INVALID  " MD2
               MOVE 1 TO FLAG
           END-IF             
           
           IF NOT((MD3 NUMERIC)OR ((MD3 ALPHABETIC) AND
               (MD3 NOT  = "S" OR "L" OR "O" OR "I" OR "B" OR "Z")))
               DISPLAY "3RD POSITION IS INVALID  " MD3
               MOVE 1 TO FLAG
           END-IF           
           
           IF MD4 NOT NUMERIC
              DISPLAY "4TH POSITION NOT NUMERIC  " MD4
              MOVE 1 TO FLAG
           END-IF   

           IF (MD5 NUMERIC) OR ((MD5 ALPHABETIC) AND 
                (MD5 = "S" OR "L" OR "O" OR "I" OR "B" OR "Z"))
               OR NOT (MD5 ALPHABETIC  OR MD5 NUMERIC)
               DISPLAY "5TH POSITION IS INVALID  " MD5
               MOVE 1 TO FLAG
           END-IF        
           
           IF NOT ((MD6 NUMERIC) OR ((MD6 ALPHABETIC) AND
               (MD6 NOT = "S" OR "L" OR "O" OR "I" OR "B" OR "Z")))
               OR NOT (MD6 ALPHABETIC  OR MD6 NUMERIC)
               DISPLAY "6TH POSITION IS INVALID  " MD6
               MOVE 1 TO FLAG
           END-IF 
           
           IF MD7 NOT NUMERIC
               DISPLAY "7TH POSITION NOT NUMERIC  " MD7
               MOVE 1 TO FLAG
           END-IF   
           
           IF (MD8 NUMERIC) OR ((MD8 ALPHABETIC) AND 
              (MD8 = "S" OR "L" OR "O" OR "I" OR "B" OR "Z"))
               OR NOT (MD8 ALPHABETIC  OR MD8 NUMERIC)
               DISPLAY "8TH POSITION IS INVALID  " MD8
               MOVE 1 TO FLAG
           END-IF  
           
           IF (MD9 NUMERIC) OR ((MD9 ALPHABETIC) AND 
                (MD9 = "S" OR "L" OR "O" OR "I" OR "B" OR "Z"))
               OR NOT (MD9 ALPHABETIC  OR MD9 NUMERIC)
               DISPLAY "9TH POSITION IS INVALID  " MD9
               MOVE 1 TO FLAG
           END-IF  
           
           IF MD10 NOT NUMERIC
               DISPLAY "10TH POSITION NOT NUMERIC  " MD10
               MOVE 1 TO FLAG
           END-IF   
           
           IF MD11 NOT NUMERIC
               DISPLAY "11TH POSITION NOT NUMERIC  " MD11
               MOVE 1 TO FLAG
           END-IF.   

       P6. 
           CLOSE FILEOUT FILEOUT2 PAPEROUT ERRORFILE.
           CLOSE GARFILE DIAGFILE REFPHY GAPFILE PROCFILE
           STOP RUN.
