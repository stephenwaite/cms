      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrr017.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL.

           SELECT AGEDATE ASSIGN TO "S35" ORGANIZATION
             LINE SEQUENTIAL.

           SELECT GARFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
             ACCESS MODE IS RANDOM RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
             LOCK MODE MANUAL.

           SELECT PAYCUR ASSIGN TO "S45" ORGANIZATION IS INDEXED
             ACCESS IS SEQUENTIAL RECORD KEY IS PAYCUR-KEY
             LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO "S50" ORGANIZATION
             LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  FILEOUT.
       01  FILEOUT01.
           02 FO-1 PIC X(11).
           02 FO-2 PIC X(11).

       FD  AGEDATE.
       01  AGEDATE01 PIC 9(8).

       FD  CHARCUR.
           copy charcur.cpy in "c:\users\sid\cms\copylib\rri".

       FD  PAYCUR.
           copy paycur.cpy in "c:\users\sid\cms\copylib\rri".

       FD  GARFILE.
           copy garfile.cpy in "c:\users\sid\cms\copylib\rri".

       WORKING-STORAGE SECTION.
       01  AMT PIC S9(4)V99.
       01  NEWDATE PIC 9(8).
       01  HOLDGARNO PIC X(8) VALUE SPACE.

       PROCEDURE DIVISION.

       0005-START.

           OPEN INPUT GARFILE PAYCUR AGEDATE.
           OPEN I-O   CHARCUR 
           open OUTPUT FILEOUT.

           READ AGEDATE 
             AT END 
               DISPLAY "NO AGEDATE" 
               GO TO P4.

           MOVE AGEDATE01 TO NEWDATE

           READ AGEDATE 
             AT END 
               DISPLAY "NO AGEDATE" 
               GO TO P4.

      *    read again to get prev cycle billdate   
           READ AGEDATE 
             AT END 
               DISPLAY "NO AGEDATE" 
               GO TO P4.    

       P1. 
           READ PAYCUR 
             AT END  
               GO TO P4.

       P1-1.      
           IF PC-DATE-T < AGEDATE01 GO TO P1.

           IF (PC-PAYCODE = 001 OR 021 OR 022 OR 062) 
             OR (PC-PAYCODE > 074 AND < 100)
             NEXT SENTENCE 
           ELSE 
             GO TO P1.

           COMPUTE AMT = -1 * PC-AMOUNT.

           IF AMT < 5 GO TO P1.

           IF PC-KEY8 NOT = HOLDGARNO
             MOVE PC-KEY8 TO HOLDGARNO
             MOVE PC-KEY8 TO G-GARNO
             READ GARFILE
              INVALID 
                DISPLAY G-GARNO 
                GO TO P1
             END-READ
           END-IF.

           IF G-LASTBILL > PC-DATE-T GO TO P1.

           IF G-DUNNING > "3" GO TO P1.

           MOVE G-GARNO TO CC-KEY8
           MOVE SPACE TO CC-KEY3
           START CHARCUR KEY > CHARCUR-KEY
             INVALID
               GO TO P1.

       P2.  
           READ CHARCUR NEXT WITH LOCK 
             AT END 
               GO TO P3.

           IF CC-KEY8 NOT = G-GARNO GO TO P3.

           IF CC-ASSIGN = "A" GO TO P2.

           IF CC-DATE-A = "00000000" GO TO P2.

           MOVE PAYCUR-KEY TO FO-2
           MOVE CHARCUR-KEY TO FO-1
           WRITE FILEOUT01

      *    Set claim age to billdate since is a current payment
           MOVE NEWDATE TO CC-DATE-A
           REWRITE CHARCUR01
           GO TO P2.

       P3.
           READ PAYCUR
             AT END 
               GO TO P4.

           IF PC-KEY8 NOT = G-GARNO GO TO P1-1.

           GO TO P3.

       P4.
           CLOSE CHARCUR agedate garfile paycur fileout.
           STOP RUN.
