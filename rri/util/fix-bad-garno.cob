      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3

       IDENTIFICATION DIVISION.
       PROGRAM-ID. ari_inventory.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILEIN ASSIGN TO    "S25" organization 
             line sequential.

           SELECT GARFILE ASSIGN TO   "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
             LOCK MODE MANUAL.
           
           SELECT CHARCUR ASSIGN TO   "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL.
           
           SELECT PAYCUR ASSIGN TO    "S40" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
             LOCK MODE MANUAL.           

           select fileout assign to   "S45" organization 
             line sequential.

       DATA DIVISION.
       
       FILE SECTION.

       fd  filein.
       01  filein01.
           02 fi-acct pic x(8).           

       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  PAYCUR.
           COPY PAYCUR.CPY IN "C:\Users\sid\cms\copylib".
      
       FD  GARFILE.
           COPY GARFILE.CPY IN "C:\Users\sid\cms\copylib\rri".
       
       fd  fileout.
       01  fileout01 pic x(200).
       
       WORKING-STORAGE SECTION.    

       01  CLAIM-TOT PIC S9(4)V99.
       01  GARBACK PIC X(315).          
       01  numx pic x(7).    
       01  SIGN-DOLLAR PIC X(4).
       01  CENTS PIC XX.
       01  RIGHT-4 PIC X(4) JUST RIGHT.
       01  ALF6 PIC X(6).
       01  NUM6 PIC 9(6).
       01  NUM-6 PIC S9(4)V99.

       PROCEDURE DIVISION.

       P0.
           OPEN INPUT filein GARFILE CHARCUR PAYCUR.
           open output fileout.

       P1. 
           MOVE SPACE TO filein01
           READ filein
             at end 
               go to p99.

           MOVE fi-acct to g-garno.    

       p2.                    
           READ GARFILE with lock
             invalid 
               display "garno no good, try again"
               accept omitted
               go to p99
           end-read.              
       
       p4.    
           MOVE fi-acct TO PC-KEY8.
           MOVE SPACE TO PC-KEY3.
           START PAYCUR KEY NOT < PAYCUR-KEY
             INVALID
               GO TO R3.

           write fileout01 from garfile01.    

       R2. 
           READ PAYCUR NEXT
             AT END
               GO TO R3.

           IF G-GARNO NOT = PC-KEY8
             GO TO R3.

           write fileout01 from paycur01
           GO TO R2.

       R3.
           MOVE fi-acct TO CC-KEY8.
           MOVE SPACE TO CC-KEY3.
           START CHARCUR KEY NOT < CHARCUR-KEY
             INVALID
               GO TO R5.

       R4. 
           READ CHARCUR NEXT
             AT END
               GO TO R5.

           IF G-GARNO NOT = CC-KEY8 GO TO R5.

           write fileout01 from charcur01
           GO TO R4.

       R5.
           display "will do something special here with " g-garno
           accept omitted.
           
                    

       p99.
           CLOSE filein GARFILE CHARCUR PAYCUR fileout.
           STOP RUN.

      