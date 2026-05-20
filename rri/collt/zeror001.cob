      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3

       IDENTIFICATION DIVISION.
       PROGRAM-ID. zeror001.
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
           COPY CHARCUR.CPY.

       FD  PAYCUR.
           COPY PAYCUR.CPY.
      
       FD  GARFILE.
           COPY GARFILE.CPY.

       fd  fileout.
       01  fileout01 pic x(11).
       
       WORKING-STORAGE SECTION.    

       01  CLAIM-TOT PIC S9(6)V99.

       PROCEDURE DIVISION.

       P0.
           OPEN I-O GARFILE CHARCUR
           OPEN INPUT PAYCUR.
           OPEN OUTPUT FILEOUT.

       R1.
           READ GARFILE NEXT WITH LOCK
             AT END
               GO TO R99.

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
           IF G-GARNO = "BER1334G"
               DISPLAY G-GARNO " CLAIM-TOT=[" CLAIM-TOT "]"
           END-IF
               
           IF CLAIM-TOT > 0
               IF G-DELETE NOT = "1"
                   MOVE "1" TO G-DELETE
                   REWRITE GARFILE01
               END-IF
           ELSE
               IF G-DELETE NOT = SPACE
                   MOVE SPACE TO G-DELETE
                   REWRITE GARFILE01
               END-IF
           END-IF

           IF CLAIM-TOT NOT > 0
               PERFORM R7 THRU R7-EXIT
           END-IF  
           
           GO TO R1.    

       R7.    
           MOVE G-GARNO TO CC-KEY8
           MOVE SPACE TO CC-KEY3
           START CHARCUR KEY NOT < CHARCUR-KEY
             invalid
               GO TO R7-EXIT
           end-start.
       
       R7-1.
           READ CHARCUR NEXT WITH LOCK             
             AT END 
               GO TO R7-EXIT  
           END-READ

           if cc-key8 not = G-GARNO go to r7-exit.
           if cc-assign  = "A" go to r7-1.
           IF CC-date-a = "00000000" go to r7-1.

           move "00000000" to cc-date-a
           REWRITE charcur01.
           move charcur-key to fileout01
           write fileout01.
           go to r7-1.
     
       r7-exit.
           exit.

       R99.
           CLOSE GARFILE CHARCUR PAYCUR fileout
           STOP RUN.
