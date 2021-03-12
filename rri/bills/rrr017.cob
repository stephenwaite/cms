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
             ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
             LOCK MODE MANUAL.

           SELECT PAYCUR ASSIGN TO "S45" ORGANIZATION IS INDEXED
             ACCESS IS dynamic RECORD KEY IS PAYCUR-KEY
             LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO "S50" ORGANIZATION
             LINE SEQUENTIAL.

           SELECT errfile ASSIGN TO "S55" ORGANIZATION
             LINE SEQUENTIAL.             

       DATA DIVISION.

       FILE SECTION.
       
       FD  FILEOUT.
       01  FILEOUT01.
           02 FO-1 PIC X(11).
           02 FO-2 PIC X(11).

       FD  AGEDATE.
       01   AGEDATE01 PIC 9(8).

       FD  CHARCUR.
       copy charcur.cpy in "c:\Users\sid\cms\copylib\rri".

       FD  PAYCUR.
       copy paycur.cpy in "c:\Users\sid\cms\copylib\rri".

       FD  GARFILE.
       copy garfile.cpy in "c:\Users\sid\cms\copylib\rri".

       FD  errfile.
       01  errfile01 pic x(80).

       WORKING-STORAGE SECTION.

       01  AMT PIC S9(4)V99.

       01  NEWDATE PIC 9(8).

       01  HOLDGARNO PIC X(8) VALUE SPACE.

       01  errmsg pic x(80).

       PROCEDURE DIVISION.

       0005-START.

           OPEN INPUT GARFILE PAYCUR AGEDATE.
           OPEN I-O   CHARCUR
           open OUTPUT FILEOUT errfile.

           READ AGEDATE AT END DISPLAY "NO AGEDATE" GO TO P4.

           MOVE AGEDATE01 TO NEWDATE

           READ AGEDATE AT END DISPLAY "NO AGEDATE" GO TO P4.

           READ AGEDATE AT END DISPLAY "NO AGEDATE" GO TO P4.

           move high-values to paycur-key.
      
       p0.
           start paycur key < paycur-key 
             invalid 
               move space to errmsg
               string "couldn't start paycur " paycur-key 
                 delimited by size into errmsg 
               perform perr
               go to p4. 

       P1.                    
           READ PAYCUR previous
             AT end
             GO TO P4
           END-READ  

           IF PC-DATE-T < AGEDATE01 GO TO P1.

           IF NOT (PC-PAYCODE = 001 OR 021 OR 022 OR 062 OR 075 OR 077) 
             GO TO P1.

           COMPUTE AMT = -1 * PC-AMOUNT.

           IF AMT < 10 GO TO P1.

           IF PC-KEY8 NOT = HOLDGARNO
             MOVE PC-KEY8 TO HOLDGARNO
             MOVE PC-KEY8 TO G-GARNO
             READ GARFILE 
               INVALID 
                 move space to errmsg
                 string "invalid read of garno " g-garno
                   delimited by size into errmsg 
                 perform perr  
                 GO TO P1
             END-READ
           END-IF.

      *    if last bill date for garno is > actual date of payment
           IF G-LASTBILL > PC-DATE-T GO TO P1.

           IF G-DUNNING > "3" GO TO P1.

      *    we have a payment record that we will use to reage the 
      *    charges on this account

           MOVE G-GARNO TO CC-KEY8 
           MOVE SPACE TO CC-KEY3
           START CHARCUR KEY > CHARCUR-KEY 
             INVALID
               move space to errmsg
               string "a payment with an invalid read of charcur for " 
                 g-garno delimited by size into errmsg 
               perform perr 
               GO TO P1.

       P2.  
           READ CHARCUR NEXT WITH LOCK 
             AT END 
               MOVE "000" TO PC-KEY3
               GO TO P0.

           IF CC-KEY8 NOT = G-GARNO 
               MOVE "000" TO PC-KEY3
             GO TO P0.

           IF CC-ASSIGN = "A" GO TO P2.

           IF CC-DATE-A = "00000000" GO TO P2.

           MOVE PAYCUR-KEY TO FO-2
           MOVE CHARCUR-KEY TO FO-1
           WRITE FILEOUT01

           MOVE NEWDATE TO CC-DATE-A
           REWRITE CHARCUR01

      *    go to p2 since other charges on bill might not have 
      *    had payment but should be reaged
           
           GO TO P2.

       
       perr.
           move space to errfile01
           string errmsg delimited by size into errfile01
           write errfile01.           

       P4.
           CLOSE garfile paycur agedate CHARCUR fileout errfile.

           STOP RUN.
