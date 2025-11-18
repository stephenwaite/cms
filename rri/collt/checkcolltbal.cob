      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3

       IDENTIFICATION DIVISION.
       PROGRAM-ID. checkcolltbal.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILEIN ASSIGN TO "S30" organization is
             LINE sequential.  
           
           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL.
           
           SELECT PAYCUR ASSIGN TO "S40" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
             LOCK MODE MANUAL.

       DATA DIVISION.
       
       FILE SECTION.

       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  PAYCUR.
           COPY PAYCUR.CPY IN "C:\Users\sid\cms\copylib".
      
       fd  filein.
       01  filein01 pic x(8).
       
       WORKING-STORAGE SECTION.    

       01  CLAIM-TOT PIC S9(6)V99.
       01  GARBACK PIC X(315).              

       PROCEDURE DIVISION.

       P0.
           OPEN INPUT CHARCUR PAYCUR FILEIN.

       R1.
           READ FILEIN
             AT END
               GO TO R99.

           MOVE 0 TO CLAIM-TOT
           MOVE filein01 TO PC-KEY8.
           MOVE SPACE TO PC-KEY3.
           START PAYCUR KEY NOT < PAYCUR-KEY
             INVALID
               GO TO R3.
              

       R2. 
           READ PAYCUR NEXT
             AT END
               GO TO R3.

           IF filein01 NOT = PC-KEY8
             GO TO R3.

           ADD PC-AMOUNT TO CLAIM-TOT
           GO TO R2.

       R3.
           MOVE filein01 TO CC-KEY8.
           MOVE SPACE TO CC-KEY3.
           START CHARCUR KEY NOT < CHARCUR-KEY
             INVALID
               GO TO R5.

       R4. 
           READ CHARCUR NEXT
             AT END
               GO TO R5.

           IF filein01 NOT = CC-KEY8 GO TO R5.

           IF CC-PAYCODE NOT = "018" GO TO R4.

           ADD CC-AMOUNT TO CLAIM-TOT
           GO TO R4.

       R5.
           IF CLAIM-TOT NOT = 0
             
           ELSE
             
           END-IF

           IF CLAIM-TOT NOT > 0
               
           END-IF  
           
           GO TO R1.    

       
       

       R99.
           CLOSE GARFILE CHARCUR PAYCUR fileout
           STOP RUN.
