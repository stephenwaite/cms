      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rri400.

       AUTHOR. SID WAITE.

       DATE-COMPILED. TODAY.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT GARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES.

           SELECT PARMFILE ASSIGN TO "S35"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S40"
           ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  GARFILE.
           copy garfile.cpy in "c:\users\sid\cms\copylib\rri".

       FD FILEOUT.
       01 FILEOUT01 PIC X(8).

       FD PARMFILE.
       01 PARMFILE01 PIC X.

       PROCEDURE DIVISION.
       P0.
           OPEN I-O GARFILE 
           open INPUT PARMFILE FILEOUT.
           
           READ PARMFILE 
             AT END 
               DISPLAY "NO PARMFILE" 
               GO TO P2.

       P1.  
           READ FILEOUT
             AT END
              GO TO P2.

           MOVE FILEOUT01 TO G-GARNO 
           READ GARFILE 
             INVALID
               GO TO P1.

           MOVE PARMFILE01 TO G-DUNNING 
           REWRITE GARFILE01. 
           GO TO P1.

       P2. 
           CLOSE GARFILE parmfile fileout. 
           STOP RUN.
