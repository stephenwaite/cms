      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rri221.
       AUTHOR. SWAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ORDFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC    
           RECORD KEY IS ORDNO
           ALTERNATE RECORD KEY IS C-DATE-E WITH DUPLICATES.
       
           SELECT FILE-IN ASSIGN TO "S35" ORGANIZATION LINE
           SEQUENTIAL.
       
       DATA DIVISION.

       FILE SECTION.

       FD  FILE-IN.
       01  FI-1 PIC X(11).

       FD  ORDFILE.
           copy "ordfile.cpy" in "c:\Users\sid\cms\copylib\rri".
       
       PROCEDURE DIVISION.
       
       P0.
           OPEN I-O ORDFILE
           OPEN INPUT FILE-IN.
       
       P1. 
           READ FILE-IN
             AT END
               GO TO P2
           END-READ
               
           MOVE FI-1 TO ORDNO
           
           READ ORDFILE WITH LOCK
             INVALID
               GO TO P1
           END-READ

           DELETE ORDFILE RECORD
             INVALID
               GO TO P1
           END-DELETE

           GO TO P1.
       
       P2. 
           CLOSE ORDFILE FILE-IN.
           STOP RUN.
