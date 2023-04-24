      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. find-paper-claim-by-clm-age.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL.          

           SELECT FILEIN ASSIGN TO "S35"
             ORGANIZATION LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S40"
             ORGANIZATION LINE SEQUENTIAL.  
           
       DATA DIVISION.
       FILE SECTION.
       
       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  FILEIN.
       01  FILEIN01 PIC X(8).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(160).

       working-storage section.

       PROCEDURE DIVISION.
       P0.
           OPEN INPUT FILEIN.
           OPEN INPUT CHARCUR
           OPEN OUTPUT FILEOUT.

           READ FILEIN 
             AT END 
               GO TO P99.

           START CHARCUR KEY NOT < LOW-VALUES.    

       P00.        
           READ CHARCUR
             AT END 
               GO TO P99.

           IF ((CC-PAPER = "P" OR CC-PAPER = "O")
              AND CC-DATE-A = FILEIN01)
             WRITE FILEOUT01 FROM CHARCUR01.
           
           GO TO P00.

       P99.
           CLOSE CHARCUR FILEIN FILEOUT.
           
           STOP RUN.
