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

           SELECT ERROR-gar ASSIGN TO "S45" ORGANIZATION
             line sequential.
           
           SELECT error-amt ASSIGN TO "S50" ORGANIZATION 
             LINE SEQUENTIAL.

           select fileout assign to   "S55" organization 
             line sequential.

       DATA DIVISION.
       
       FILE SECTION.

       fd  filein.
       01  filein01.
           02 filler pic x(9).
           02 fi-acct pic x(8).
           02 filler pic x(73).
           02 fi-bal pic x(7).
           02 filler pic x(10).
           02 fi-stat pic x(6).

       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  PAYCUR.
           COPY PAYCUR.CPY IN "C:\Users\sid\cms\copylib".
      
       FD  GARFILE.
           COPY GARFILE.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  error-gar.
       01  error-gar01 PIC X(120).

       FD  error-amt.
       01  error-amt01 PIC X(120).

       fd  fileout.
       01  fileout01 pic x(120).
       
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
           open output error-gar error-amt fileout.

       P1. 
           MOVE SPACE TO filein01
           READ filein
             at end 
               go to p99.

           MOVE fi-acct to g-garno.    

       p2.                    
           READ GARFILE with lock
             invalid 
               write error-gar01 from filein01
               go to p1
           end-read.              

       p3.    
           move space to numx
           move fi-bal to numx
           perform a1.

       p4.    

           MOVE 0 TO CLAIM-TOT
           MOVE G-GARNO TO PC-KEY8.
           MOVE SPACE TO PC-KEY3.
           START PAYCUR KEY NOT < PAYCUR-KEY
             INVALID
               GO TO R3.

       R2. 
           READ PAYCUR NEXT
             AT END
               GO TO R3.

           IF G-GARNO NOT = PC-KEY8
             GO TO R3.

           ADD PC-AMOUNT TO CLAIM-TOT
           GO TO R2.

       R3.
           MOVE G-GARNO TO CC-KEY8.
           MOVE SPACE TO CC-KEY3.
           START CHARCUR KEY NOT < CHARCUR-KEY
             INVALID
               GO TO R5.

       R4. 
           READ CHARCUR NEXT
             AT END
               GO TO R5.

           IF G-GARNO NOT = CC-KEY8 GO TO R5.

           ADD CC-AMOUNT TO CLAIM-TOT
           GO TO R4.

       R5.
           if claim-tot = 0
             go to P1
           end-if

           IF CLAIM-TOT not = num-6
             string g-garno " our total " claim-tot 
             " their total " num-6 delimited by size
             into error-amt01
             write error-amt01 
             
             GO TO p1
           END-IF

           write fileout01 from filein01.

           go to p1.  
                    

       p99.
           CLOSE filein GARFILE CHARCUR PAYCUR error-gar error-amt
             fileout.
           STOP RUN.

       a1.
           MOVE SPACE TO SIGN-DOLLAR CENTS
           UNSTRING NUMX DELIMITED BY "." INTO SIGN-DOLLAR CENTS.
                                 
           MOVE SPACES TO RIGHT-4.
           move SIGN-DOLLAR to right-4
           INSPECT RIGHT-4 REPLACING LEADING " " BY "0"
           
           IF RIGHT-4 NOT NUMERIC
             DISPLAY FILEIN01 " DOLLARS not numeric"
             ACCEPT omitted
             GO TO P1.

           STRING RIGHT-4 CENTS DELIMITED BY SIZE INTO ALF6
           MOVE ALF6 TO NUM6
           DIVIDE NUM6 BY 100 GIVING NUM-6.