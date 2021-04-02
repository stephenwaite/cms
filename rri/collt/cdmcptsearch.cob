      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3

       IDENTIFICATION DIVISION.
       PROGRAM-ID. cdmcptsearch.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT GARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
             LOCK MODE MANUAL.
           
           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL.
           
           SELECT PAYCUR ASSIGN TO "S40" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
             LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO "S45" organization is
             LINE sequential.  

       DATA DIVISION.
       
       FILE SECTION.

       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  PAYCUR.
           COPY PAYCUR.CPY IN "C:\Users\sid\cms\copylib".
      
       FD  GARFILE.
           COPY GARFILE.CPY IN "C:\Users\sid\cms\copylib\rri".

       fd  fileout.
       01  fileout01 pic x(200).
       
       WORKING-STORAGE SECTION.    

       01  CLAIM-TOT PIC S9(6)V99.
       01  GARBACK PIC X(315).              

       PROCEDURE DIVISION.

       P0.
           OPEN INPUT GARFILE CHARCUR PAYCUR.
           open output fileout.      

       R1. 
           READ CHARCUR NEXT
             AT END
               GO TO R99.             

           IF CC-PROC NOT = "00007312026" GO TO R1.

           MOVE 0 TO CLAIM-TOT
           ADD CC-AMOUNT TO CLAIM-TOT

           MOVE CC-KEY8 TO PC-KEY8.
           MOVE SPACE TO PC-KEY3.
           START PAYCUR KEY NOT < PAYCUR-KEY
             INVALID
               GO TO R5. 
       
       R2. 
           READ PAYCUR NEXT
             AT END
               GO TO R5.

           IF PC-KEY8 NOT = CC-KEY8
             GO TO R5.

           IF PC-CLAIM NOT = CC-CLAIM
             GO TO R2.  

           ADD PC-AMOUNT TO CLAIM-TOT
           GO TO R2.    

       R5.
           IF CLAIM-TOT NOT = 0
             DISPLAY "CLAIM-TOT " CLAIM-TOT
             ACCEPT omitted
             WRITE FILEOUT01 FROM CHARCUR01.                                  
           
           GO TO R1.    

       R99.
           CLOSE GARFILE CHARCUR PAYCUR fileout
           STOP RUN.
