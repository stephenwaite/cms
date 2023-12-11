      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. read-charcur-prev.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARCUR ASSIGN TO "S50" ORGANIZATION INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO "S60" ORGANIZATION LINE SEQUENTIAL.  

       DATA DIVISION.
       FILE SECTION.

       FD  CHARCUR.
           COPY charcur.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  FILEOUT.
       01  FILEOUT01 PIC X(160).    
       

       WORKING-STORAGE SECTION.
       
       
       PROCEDURE DIVISION.

       P0.

           OPEN INPUT CHARCUR
           OPEN OUTPUT FILEOUT.

           MOVE "ZZZZZZZZZZZ" TO CHARCUR-KEY
           
           START CHARCUR KEY NOT > CHARCUR-KEY
             INVALID
               DISPLAY "COULDN'T START CHARCUR"
               GO TO P99.

       P1.

           READ CHARCUR PREVIOUS
             AT END 
               GO TO P99.

           WRITE FILEOUT01 FROM CHARCUR01.

           GO TO P1.
           
           
       P99. 
           CLOSE CHARCUR FILEOUT.
           STOP RUN.
