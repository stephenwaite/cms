      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. chcrevins.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES.

           SELECT FILEOUT ASSIGN TO "S40"
             ORGANIZATION LINE SEQUENTIAL.

           SELECT FILEIN ASSIGN TO "S45"
             ORGANIZATION LINE SEQUENTIAL.

           SELECT GARFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES.
           
       DATA DIVISION.

       FILE SECTION.
       
       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib\rri".             
      
       FD  GARFILE.
           COPY GARFILE.CPY IN "C:\Users\sid\cms\copylib\rri".      
      
       FD  FILEIN.
       01  FILEIN01 PIC X(189).
       
       FD  FILEOUT.
       01  FILEOUT01 PIC X(189).
           
       WORKING-STORAGE SECTION.
       01  HOLD-8 PIC X(8).

       PROCEDURE DIVISION.
       
       P0.
           OPEN INPUT FILEIN.
           OPEN I-O GARFILE CHARCUR.
           OPEN OUTPUT FILEOUT.

       P0-1.    
           READ FILEIN AT END GO TO P99.
           MOVE FILEIN01(1:8) TO G-GARNO

           IF HOLD-8 = G-GARNO
             GO TO P0-1.
           
           MOVE G-GARNO TO HOLD-8
           
           READ GARFILE 
             INVALID
               DISPLAY "INVALID, SHOULDN'T BE"
               GO TO P0-1.
           

           MOVE G-SEINS TO G-PRINS
           MOVE "001" TO G-SEINS
           MOVE G-SECPOL TO G-PRIPOL
           MOVE SPACE TO G-SECPOL
           MOVE G-SENAME TO G-PRNAME
           MOVE SPACE TO G-SENAME
           MOVE G-SE-ASSIGN TO G-PR-ASSIGN
           MOVE "U" TO G-SE-ASSIGN
           MOVE G-RELATE TO G-PR-RELATE
           MOVE SPACE TO G-SE-RELATE
           REWRITE GARFILE01

           MOVE G-GARNO TO CC-KEY8
           MOVE SPACE TO CC-KEY3
           START CHARCUR KEY NOT LESS THAN CHARCUR-KEY
             INVALID
               DISPLAY "HOW THE HECK IS THAT INVALID"
               GO TO P0-1.

       P1.     
           READ CHARCUR NEXT    
             AT END
               GO TO P0-1.

           IF CC-KEY8 NOT = G-GARNO
             GO TO P0-1.
          
           MOVE "004" TO CC-PAYCODE 
           MOVE "A" TO CC-ASSIGN
           MOVE "A" TO CC-NEIC-ASSIGN


           write fileout01 from  CHARCUR01

           REWRITE CHARCUR01.
           GO TO P1.


       P99. 
           CLOSE GARFILE CHARCUR FILEIN FILEOUT.
           STOP RUN.
