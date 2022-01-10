      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
        IDENTIFICATION DIVISION.
       PROGRAM-ID. npi062.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REFPHY ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS REF-KEY
             ALTERNATE RECORD KEY IS REF-BSNUM  WITH DUPLICATES
             ALTERNATE RECORD KEY IS REF-CRNUM WITH DUPLICATES
             ALTERNATE RECORD KEY IS REF-UPIN  WITH DUPLICATES
             ALTERNATE RECORD KEY IS REF-CDNUM WITH DUPLICATES
             ALTERNATE RECORD KEY IS REF-NAME  WITH DUPLICATES
             LOCK MODE MANUAL.

           SELECT NPIFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS NPI-KEY
             ALTERNATE RECORD KEY IS NPI-NAME WITH DUPLICATES
             ALTERNATE RECORD KEY IS NPI-REFKEY WITH DUPLICATES
             LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
       FD  REFPHY.
       01  REFPHY01.
           02 REF-KEY PIC XXX.
           02 REF-BSNUM PIC X(5).
           02 REF-CRNUM PIC X(6).
           02 REF-UPIN PIC X(6).
           02 REF-CDNUM PIC X(7).
           02 REF-NAME PIC X(24).
           02 REF-NPI PIC X(10).
       FD  NPIFILE.
       01  NPIFILE01.
           02 NPI-KEY PIC X(10).
           02 NPI-NAME PIC X(24).
           02 NPI-REFKEY PIC X(3).
           02 NPI-PLACE PIC X.
       WORKING-STORAGE SECTION.
       01  PART-NAME PIC X(24).
       01  ANS PIC XXX.
       01  CNTR PIC 99.
       01  ALF10 PIC X(10).
       01  ALF-8.
             02 ALF-84 PIC X(4).
             02 ALF-8E PIC X(4).
       01  NUM1 PIC 9.

       PROCEDURE DIVISION.
       P0.
           OPEN INPUT REFPHY I-O NPIFILE.
       P1.
           DISPLAY "A,C,F, OR END"
           DISPLAY "ALSO FR TO SEARCH REFPHY BY NAME"
           ACCEPT ANS
           IF ANS = "END" GO TO P99.
           IF NOT (ANS = "A" OR "C" OR "F" OR "FR") GO TO P1.
           IF ANS = "A" GO TO A-1.
           IF ANS = "C" GO TO C-1.
           IF ANS = "F" GO TO F-1.
           IF ANS = "FR" GO TO REF-1.
           GO TO P1.
       A-1.
           DISPLAY "ENTER THE NPI NUMBER"
           ACCEPT NPI-KEY
           IF NPI-KEY = SPACE GO TO P1.
           READ NPIFILE INVALID GO TO A-2.
           DISPLAY NPI-KEY " " NPI-NAME " " NPI-PLACE
           "  ALREADY IN FILE"
           IF NPI-PLACE = SPACE
             DISPLAY " USE CHANGE TO MODIFY THE PLACE OF SERVICE"
             DISPLAY " FOR THIS NPI, HOWEVER."
           END-IF
           GO TO P1.
           
       A-2.
           PERFORM A-3 THRU A-EXIT
           GO TO P1.

       A-3.
           DISPLAY "ENTER ANY PART OF LAST NAME"
           ACCEPT PART-NAME
           IF PART-NAME = "X" OR SPACE
             GO TO A-EXIT
           END-IF
           MOVE PART-NAME TO REF-NAME
           START REFPHY KEY NOT < REF-NAME
             INVALID
               DISPLAY "PART NAME NOT FOUND IN THE FILE"
               DISPLAY "MAYBE YOU WILL HAVE TO ADD THIS"
               DISPLAY "PROVIDER TO THE REF. FILE USING RRI-62"
               DISPLAY "SINCE THE PROVIDER HAS TO EXIST THERE FIRST!!"
               GO TO A-3
               END-START.
        A-4.
           READ REFPHY NEXT 
             AT END
               DISPLAY "PART NAME NOT FOUND IN THE FILE"
               GO TO A-3
           END-READ

           IF REF-NAME(1:1) > PART-NAME(1:1)
             DISPLAY "PART NAME NOT FOUND IN THE FILE"
             GO TO A-2
           END-IF.

        A-5.
           IF REF-NPI = NPI-KEY
             DISPLAY REF-KEY " " REF-NAME " " REF-NPI
             DISPLAY "A = ADD THIS: N = DON'T ADD, LOOK FURTHER"
             ACCEPT ANS
             IF NOT (ANS = "A" OR "N")
               GO TO A-4
             END-IF
           
             IF ANS = "N"
               GO TO A-4
             END-IF.

       A-6.
           DISPLAY "ACCEPTED " REF-KEY
           DISPLAY "ENTER A VALID PLACE CODE TO THIS CHCRR PROVIDER"
           DISPLAY "OR X = BACK TO THE BEGINNING"

           DISPLAY "C=CAST, M=METTOWEE, R=RUTLAND".
           ACCEPT NPI-PLACE.
           
           IF NPI-PLACE = "X" GO TO A-EXIT.

           IF NOT (NPI-PLACE = "A" OR "B" OR "C" OR "M" OR "P" OR 
                               "R" OR "S") 
             GO TO A-6
           END-IF    
           
           MOVE REF-NAME TO NPI-NAME
           MOVE REF-KEY TO NPI-REFKEY           

           WRITE NPIFILE01
             INVALID
               DISPLAY "ALREADY ON FILE"
               GO TO P1
           END-WRITE.
        
       A-EXIT.
           EXIT.

       C-1.
           DISPLAY "ENTER THE NPI"
           DISPLAY " X = BACK TO OPTIONS"
           ACCEPT NPI-KEY

           IF NPI-KEY = "X" GO TO P1.

           IF NPI-KEY NOT NUMERIC
             DISPLAY "10-DIGITS PLEASE!"
             GO TO C-1
           END-IF

           READ NPIFILE WITH LOCK
             INVALID
               DISPLAY " NOT ON FILE"
               GO TO C-1
           END-READ

           DISPLAY  NPI-KEY " " NPI-NAME " " NPI-REFKEY " " NPI-PLACE.

       C-2.
           DISPLAY "CHANGE OPTIONS: 1=REF-KEY 2=PLACE".
           ACCEPT ALF-8

           IF ALF-8 = "0" OR ALF-8 > "0" AND < "7"
             MOVE ALF-8 TO NUM1
             GO TO C-3 C-4 DEPENDING ON NUM1.

           IF ALF-8 = "X" DISPLAY "NO CHANGE" GO TO C-1.

           IF ALF-8 = "?"
             DISPLAY "ENTER 1, 2 OR X".

           GO TO C-2.

       C-3.
           DISPLAY "CURRENT REF-KEY ON " NPI-KEY           
           MOVE NPI-KEY TO REF-KEY
           READ REFPHY WITH LOCK 
             INVALID 
             DISPLAY "NO REF PHY ATTACHED"
             NOT INVALID
               DISPLAY  REF-KEY " " REF-BSNUM " " REF-CRNUM " " 
                 REF-UPIN " " REF-CDNUM " " REF-NPI " " REF-NAME
           END-READ

           DISPLAY "ENTER THE REF-KEY, X TO GO BACK"
           DISPLAY "USE RRI-62 TO FIND THE REF-KEY"
           ACCEPT ANS

           IF ANS = "X" GO TO C-1.

           MOVE ANS TO REF-KEY
           READ REFPHY WITH LOCK 
             INVALID 
             DISPLAY "NOT VALID, TRY AGAIN"
             GO TO C-3
             NOT INVALID
               DISPLAY  REF-KEY " " REF-BSNUM " " REF-CRNUM " " 
                 REF-UPIN " " REF-CDNUM " " REF-NPI " " REF-NAME
           END-READ

           DISPLAY "ACCEPT THIS REF? Y"
           ACCEPT ANS

           IF ANS NOT = "Y" GO TO C-3.

           MOVE REF-KEY TO NPI-REFKEY
           GO TO RE-WRITE-NPI.

       C-4.
           DISPLAY "PLACE OF SERVICE"
           DISPLAY " X = BACK TO OPTIONS"

           ACCEPT NPI-PLACE.
           
           IF NPI-PLACE = "X" GO TO P1.
           
           IF NOT (NPI-PLACE = "A" OR "B" OR "C" OR "M" OR "P" OR 
                               "R" OR "S")
               GO TO C-1
           END-IF           

           GO TO RE-WRITE-NPI.
       
       RE-WRITE-NPI.           
           REWRITE NPIFILE01
             INVALID
               CONTINUE
           END-REWRITE

           DISPLAY NPI-KEY " " NPI-NAME " " NPI-PLACE
           
           GO TO P1.
           
       F-1.
           MOVE 0 TO CNTR.
           DISPLAY "ENTER ANY PART OF THE NPI OR NAME"
           DISPLAY " X = BACK TO OPTIONS"
           ACCEPT ALF10
           IF ALF10 = "X" GO TO P1.
           IF ALF10(1:1) NUMERIC
             MOVE ALF10 TO NPI-KEY
             START NPIFILE KEY NOT < NPI-KEY
              INVALID
               DISPLAY "TOO BIG FOR NPI NUMBERS IN THE FILE"
               GO TO F-1
             END-START
            GO TO F-2
           END-IF

           MOVE ALF10 TO NPI-NAME
           START NPIFILE KEY NOT < NPI-NAME
             INVALID
              DISPLAY "NAME NOT IN THE FILE"
              GO TO F-1
           END-START.
       F-2.
           READ NPIFILE NEXT AT END GO TO F-1.
           ADD 1 TO CNTR
           DISPLAY NPI-KEY " " NPI-NAME " " NPI-PLACE
           IF CNTR <  15
             GO TO F-2
           END-IF
           ACCEPT ANS
           IF ANS = SPACE
             MOVE 0 TO CNTR
             GO TO F-2
           END-IF
           GO TO P1.
       REF-1.
           DISPLAY "NAME TO SEARCH BY"
           DISPLAY "X = BACK TO OPTIONS"
           ACCEPT REF-NAME
           IF REF-NAME = "X" GO TO P1.
           START REFPHY KEY NOT < REF-NAME
            INVALID
             DISPLAY "NAME NOT IN FILE"
             GO TO REF-1
           END-START.
           MOVE 0 TO CNTR.
       REF-2.
           READ REFPHY NEXT AT END GO TO REF-1.
           ADD 1 TO CNTR
           DISPLAY REF-KEY " " REF-NAME " " REF-NPI
           IF CNTR < 15 GO TO REF-2.
           ACCEPT ANS
           IF ANS = SPACE
             MOVE 0 TO CNTR
             GO TO REF-2
           END-IF
           GO TO P1.

       P99.
           CLOSE REFPHY NPIFILE
           STOP RUN.
