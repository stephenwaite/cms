      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. npir078.
       AUTHOR. SWAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT INSIN ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.
       
           SELECT FILE-OUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
       
           SELECT CHARCUR ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC    RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.                 
       
           SELECT FILEOUT ASSIGN TO "S50" ORGANIZATION
           LINE SEQUENTIAL.
       
           SELECT DOCFILE ASSIGN TO "S55" ORGANIZATION
           LINE SEQUENTIAL.
       
           SELECT GARFILE ASSIGN TO "S60"     ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM         RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE MANUAL.
       
           SELECT FILEIN   ASSIGN TO "S65" ORGANIZATION
           LINE SEQUENTIAL.
       
           SELECT INSFILE ASSIGN TO "S70"    ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC        RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
           LOCK MODE MANUAL.
       
           SELECT REFPHY ASSIGN TO "S75" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS REF-KEY
           ALTERNATE RECORD KEY IS REF-BSNUM  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CRNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-UPIN  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CDNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-NAME  WITH DUPLICATES
           LOCK MODE MANUAL.
       
           SELECT ERRORFILE ASSIGN TO "S80" ORGANIZATION
           LINE SEQUENTIAL.
       
           SELECT FILEOUT2 ASSIGN TO "S85" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT AUTHFILE ASSIGN TO "S90" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS AUTH-KEY
           LOCK MODE MANUAL.           

       DATA DIVISION.

       FILE SECTION.

       FD  ERRORFILE.
       01  ERRORFILE01.
           02 EF1 PIC X(34).
           02 FILLER PIC X.
           02 EF2 PIC X(22).
           02 FILLER PIC X.
           02 EF3 PIC X(22).

       FD  REFPHY.
           COPY refphy.CPY IN "C:\Users\sid\cms\copylib".                  

       FD  INSFILE.
           COPY insfile.CPY IN "C:\Users\sid\cms\copylib".                  

       FD  FILEIN.
       01  FILEIN01 PIC 999.

       FD GARFILE.
           COPY garfile.CPY IN "C:\Users\sid\cms\copylib\rri".        

       FD  DOCFILE.
       01  DOCFILE01.
           02 DF-1 PIC 99.
           02 DF-2 PIC 99.

       FD  FILEOUT.
       01  FILEOUT01 PIC X(165).

       FD  FILEOUT2.
       01  FILEOUT201 PIC X(165).
      
       FD  INSIN.
       01  INSIN01.
           02 INS-1 PIC 999.
           02 INS-2 PIC XX.

       FD  FILE-OUT.
       01  FILE-OUT01.
           02 FO-PC PIC XXX.
           02 FO-PATID PIC X(8).
           02 FILEOUT-KEY PIC X(11).
           02 FO-DATE PIC X(8).
           02 FO-ASSIGN PIC X.
           02 FO-PLACE PIC X.
           02 FO-DOCP PIC XX.
           02 FILLER PIC X(16).
           02 FO-PAPER PIC X.

       FD  CHARCUR.
           COPY charcur.CPY IN "C:\Users\sid\cms\copylib\rri".           

       FD  AUTHFILE.
           COPY authfile.CPY IN "C:\Users\sid\cms\copylib\rri".           
                 

       WORKING-STORAGE SECTION.
       01  X USAGE IS INDEX.
       01  INSTAB01.
           02 INSTAB PIC 99 OCCURS 999 TIMES.
      *
       01  DOCTAB01.
           02 DOCTAB PIC 99 OCCURS 20 TIMES.
       01  NUM3 PIC 999.
       01  ALF11 PIC X(11).
       01  TEST-DATE.
           05  T-CC            PIC XX.
           05  T-YY            PIC XX.
           05  T-MM            PIC XX.
           05  T-DD            PIC XX.
       01  DISPLAY-DATE.
           02 T-MM PIC XX.
           02 FILLER PIC X VALUE "/".
           02 T-DD PIC XX.
           02 FILLER PIC X VALUE "/".
           02 T-CC PIC XX.
           02 T-YY PIC XX.
       01  ALF2 PIC XX. 

       PROCEDURE DIVISION.
       P0.
           OPEN OUTPUT FILE-OUT FILEOUT FILEOUT2 ERRORFILE.
           
           OPEN INPUT INSIN CHARCUR DOCFILE GARFILE
               FILEIN INSFILE REFPHY AUTHFILE.
           
           MOVE SPACE TO ERRORFILE01
           MOVE "  COMMERCIAL ELECTRONIC CLAIMS ERRORS      "
            TO ERRORFILE01
           WRITE ERRORFILE01
           MOVE SPACE TO ERRORFILE01.

           PERFORM A4 VARYING X FROM 1 BY 1 UNTIL X > 999.
           PERFORM A3 VARYING X FROM 1 BY 1 UNTIL X > 20.

       PZ. 
           READ DOCFILE
             AT END
               GO TO P00
           END-READ

           MOVE DF-2 TO DOCTAB(DF-1)
           GO TO PZ.

       P00.
           READ INSIN
             AT END
               GO TO P000
           END-READ

           MOVE INS-2 TO INSTAB(INS-1)
           GO TO P00.

       P000.
           MOVE 100 TO CC-PAYCODE
           
           START CHARCUR KEY NOT < CC-PAYCODE
             INVALID
               GO TO P6
           END-START.      

       P1.
           MOVE SPACE TO ERRORFILE01

           READ CHARCUR NEXT
             AT END
               GO TO P6
           END-READ

           IF CC-PAYCODE = 160 OR 197
               GO TO P1
           END-IF    
           
           IF CC-PAYCODE > 199
               GO TO P6
           END-IF

           PERFORM A1 THRU A2
           GO TO P1.

       A1.
           IF CC-PROC = "1      " OR "2       "
               GO TO A2
           END-IF    
           
           IF (CC-PROC < "00100  ") AND (CC-AMOUNT = 0)
               GO TO A2
           END-IF    

      *    medicare advantage might like to see quality codes
      *    maybe even the G1004 for AUC
      *    so if we receive a G1004 from rrmc then we should look
      *    to see if we should report any applicable measures 
      *    are you up for the challenge?
                 
           IF CC-AMOUNT = 0
               GO TO A2
           END-IF    
           
           IF CC-REC-STAT > "1"
               GO TO A2
           END-IF    

           MOVE CC-KEY8 TO G-GARNO

           READ GARFILE
             INVALID
               MOVE "BAD GARNO           " TO EF2
               PERFORM E1 
               GO TO A2
           END-READ

           MOVE G-GARNAME TO EF3
           MOVE G-PRINS TO NUM3           
           
           IF CC-DOCP = 02
               MOVE "NO READING DOC        " TO EF2
               PERFORM E1
               GO TO A2
           END-IF

      *     IF DOCTAB(CC-DOCP) = 99
      *       OR INSTAB(CC-PAYCODE) = 99
      *         GO TO A2
      *     END-IF

      *     IF INSTAB(CC-PAYCODE) NOT = 0
      *        MOVE INSTAB(CC-PAYCODE) TO CC-DOCP
      *     END-IF

      *     IF DOCTAB(CC-DOCP) NOT = 0
      *         MOVE DOCTAB(CC-DOCP) TO CC-DOCP
      *     END-IF

           IF CC-PAPER = "O"
               PERFORM PAPER-1
               GO TO A2
           END-IF          
           
           IF G-STREET = SPACE AND G-BILLADD = SPACE
               MOVE "ADDRESS IS BLANK" TO EF2
               PERFORM E1 
               GO TO A2
           END-IF

           IF G-DOB NOT NUMERIC
               MOVE "BAD DOB           " TO EF2               
               PERFORM E1 
               GO TO A2
           END-IF

           IF (NUM3 NOT = CC-PAYCODE) AND (CC-PAPER = "E")
               MOVE "P" TO CC-PAPER.
           
           IF CC-PAYCODE = 153 OR "122" OR "123" MOVE "P" TO CC-PAPER.
           
           IF CC-PAPER = "E" GO TO A1-1.
           
           PERFORM PAPER-1 GO TO A2.

       A1-1.
           MOVE CC-PAYCODE TO INS-KEY
           
           READ INSFILE
             INVALID
               GO TO A2
           END-READ

           IF INS-NEIC = SPACE DISPLAY CHARCUR01 GO TO A2.
           
           IF INS-CITY = SPACE OR INS-STREET = SPACE
             OR INS-STATE = SPACE OR INS-ZIP = SPACE
             MOVE  "NO INS. ADDRESS " TO EF2
             PERFORM E1 
             GO TO A2.
           
           IF CC-DOCP = "00"
             MOVE "NO DOCP              " TO EF2 
             PERFORM E1 
             GO TO A2.

           IF INS-NEIC = "57106" AND CC-DATE-M = "00000000"
             AND CC-PLACE = "3"
             MOVE "ADMIT DATE - TRICARE " TO EF2 
             PERFORM E1 
             GO TO A2.

           IF INS-NEIC = SPACE
             MOVE "NO NEIC CODE PRESENT" TO EF2
             PERFORM E1 
             GO TO A2.

           IF G-PRIPOL = SPACE
             MOVE "POLICY NUMBER MISSING" TO EF2
             PERFORM E1 
             GO TO A2.

           IF G-PRIPOL = ZEROES
             MOVE "POLICY CANT BE 0" TO EF2
             PERFORM E1 
             GO TO A2.

           IF INS-NEIC = "23742"
             PERFORM PAPER-1 
             GO TO P1.

           IF G-PR-GROUP = G-PRIPOL
             MOVE "GRP & POLICY ARE THE SAME" TO EF2
             PERFORM E1 
             GO TO A2.
           
           IF (INS-NEIC = "14165" OR "SX073")
             MOVE G-PRIPOL TO ALF11
             IF (ALF11 NOT NUMERIC)
               IF (G-PRINS = "256" OR "349" OR "576")
                 MOVE "BAD POLICY NUMBER " TO EF2
                 PERFORM E1
                 GO TO A2.              

           IF CC-DIAG = "0000000"
             MOVE "NO DIAG. ON CHARGE " TO EF2
             PERFORM E1
             GO TO A2
           END-IF

      *     IF (INS-NEIC = "VACCN")
      *         MOVE CC-KEY8 TO AUTH-KEY8
      *         MOVE CC-CLAIM TO AUTH-KEY6
      *         READ AUTHFILE                  
      *           INVALID 
      *             STRING "NO AUTH " CC-PROC " " CC-AMOUNT
      *               DELIMITED BY SIZE INTO EF2
      *             PERFORM E1
      *             GO TO A2
      *         END-READ
      *     END-IF    
                    
           MOVE SPACE TO FILEOUT01
           STRING CHARCUR01 INS-NEIC DELIMITED BY SIZE
             INTO FILEOUT01

           IF INS-NEIC = "14165"
             WRITE FILEOUT201 FROM FILEOUT01
           ELSE
             WRITE FILEOUT01
           END-IF

           GO TO A2.

       PAPER-1.
           
           MOVE SPACE TO FILE-OUT01
           MOVE CC-PAYCODE TO FO-PC
           MOVE CC-PATID TO  FO-PATID 
           MOVE CHARCUR-KEY TO FILEOUT-KEY
           MOVE CC-DATE-T TO FO-DATE 
           MOVE CC-ASSIGN TO FO-ASSIGN
           MOVE CC-PLACE TO FO-PLACE 
           MOVE CC-DOCP TO FO-DOCP
           MOVE CC-PAPER TO FO-PAPER
           WRITE FILE-OUT01.

       A2.
           EXIT.

       A4. 
           MOVE 0 TO INSTAB(X).

       A3. 
           MOVE 0 TO DOCTAB(X).

       E1.
           STRING CHARCUR-KEY " " CC-PROC1 " " CC-AMOUNT " " 
             CC-DATE-T(5:2) "-" CC-DATE-T(7:2) "-" CC-DATE-T(3:2)
             DELIMITED BY SIZE INTO EF1 
           WRITE ERRORFILE01.       
       
       P6. 
           READ FILEIN
             AT END
               GO TO P9
           END-READ

           MOVE FILEIN01 TO CC-PAYCODE
           START CHARCUR KEY NOT < CC-PAYCODE
             INVALID
               GO TO P6
           END-START.    

       P7. 
           READ CHARCUR NEXT
             AT END
               GO TO P6
           END-READ

           IF CC-PAYCODE NOT = FILEIN01 GO TO P6.
           
           PERFORM A1 THRU A2 GO TO P7.

       P9.
           CLOSE INSIN FILE-OUT CHARCUR  
             FILEOUT DOCFILE GARFILE FILEIN
             REFPHY ERRORFILE FILEOUT2 AUTHFILE.

           STOP RUN.
