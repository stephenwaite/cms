      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. garno-due.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT GARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC  RECORD KEY IS G-GARNO
             LOCK MODE MANUAL.    
           
           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
               ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
               LOCK MODE MANUAL.
           
           SELECT PAYCUR ASSIGN TO "S40" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
             LOCK MODE MANUAL.

           SELECT FILEIN ASSIGN TO  "S45" ORGANIZATION LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S50" ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       
       FD  GARFILE.
           copy garfile.cpy in "c:\users\sid\cms\copylib".

       FD  PAYCUR.
           copy paycur.cpy in "c:\users\sid\cms\copylib".    

       FD  CHARCUR.
           copy charcur.cpy in "c:\users\sid\cms\copylib".
           
       FD  FILEIN.
       01  FILEIN01. 
           02 FI-1 PIC X(8).      
           02 FI-2 PIC X(70).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(78).


       WORKING-STORAGE SECTION. 

       01  TOT-ASSIGNED PIC S9(6)V99.
       01  TOT-UNASSIGNED PIC S9(6)V99.
       
       PROCEDURE DIVISION.

       P0.
           OPEN INPUT GARFILE PAYCUR CHARCUR FILEIN.
           OPEN OUTPUT FILEOUT.

       P0-1.
           READ FILEIN
             AT END
               GO TO P99.    

       R1.
           READ GARFILE 
             INVALID 
               DISPLAY " NOT ON FILE"
               GO TO P99.

           MOVE G-GARNO TO PC-KEY8 
           MOVE "000" TO PC-KEY3.
           MOVE 0 TO TOT-UNASSIGNED TOT-ASSIGNED.
           START PAYCUR KEY  NOT < PAYCUR-KEY 
             INVALID 
               GO TO FP5.

       R2. 
           READ PAYCUR NEXT 
             AT END 
               GO TO R3.
           
           IF PC-KEY8 NOT = G-GARNO GO TO R3.

           MOVE PC-AMOUNT TO A-TAB(P-IND)
           MOVE PC-DATE-T TO D-TAB(P-IND)
           MOVE PC-DATE-E TO DE-TAB(P-IND)

           MOVE PC-PAYCODE TO PC-TAB(P-IND)
           MOVE PC-DENIAL TO DN-TAB(P-IND)
           IF P-IND = 990 DISPLAY "990 + PAYMENTS WERE FOUND"
           DISPLAY "ONLY 990 WILL BE USED" GO TO P99.
           SET P-IND UP BY 1
           GO TO FP4.

       R3. 
           READ CHARCUR NEXT 
             AT END 
               GO TO FC6.

           IF CC-KEY8 NOT = G-GARNO GO TO FC6.

           MOVE CC-CLAIM TO CLAIM
           MOVE CC-AMOUNT TO TOT-AMOUNT
           MOVE CC-ASSIGN TO ALF-1-1
           PERFORM FT1 
           GO TO FC3.

       FPS. 
           IF C-TAB(XIND) = CLAIM
             ADD A-TAB(XIND) TO TOT-AMOUNT
             MOVE A-TAB(XIND) TO NEF-8
             MOVE NEF-8 TO ALF-11
             MOVE TOT-AMOUNT TO NEF-8
             MOVE D-TAB(XIND) TO TEST-DATE
             MOVE DE-TAB(XIND) TO TEST-DATE1.

       FP1-EXIT. 
           EXIT.

       FT1.
           PERFORM FPZ VARYING XIND FROM 1 BY 1 UNTIL XIND > P-IND.
           
           IF ALF-1-1 = "A" 
             ADD TOT-AMOUNT TO TOT-ASSIGNED
           ELSE 
             ADD TOT-AMOUNT TO TOT-UNASSIGNED.    
       
       P99.
           CLOSE GARFILE PAYCUR CHARCUR FILEIN FILEOUT.
           STOP RUN.