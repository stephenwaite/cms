      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. insLoadCheck.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILEOUT ASSIGN TO "S40"
             ORGANIZATION LINE SEQUENTIAL.

           SELECT FILEIN ASSIGN TO "S45"
             ORGANIZATION LINE SEQUENTIAL.

           SELECT GARFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES.
           
       DATA DIVISION.

       FILE SECTION.
       
       FD  GARFILE.
           COPY GARFILE.CPY IN "C:\Users\sid\cms\copylib\rri".      
      
       FD  FILEIN.
       01  FILEIN01 PIC X(189).
       
       FD  FILEOUT.
       01  FILEOUT01 PIC X(120).
           
       WORKING-STORAGE SECTION.
       01  HOLD-8 PIC X(8).

       PROCEDURE DIVISION.
       
       P0.
           OPEN INPUT FILEIN.
           OPEN INPUT GARFILE.
           OPEN OUTPUT FILEOUT.

       P0-1.    
           READ FILEIN AT END GO TO P99.
           MOVE FILEIN01(1:8) TO G-GARNO

           IF HOLD-8 = G-GARNO
             GO TO P0-1.
           
           MOVE G-GARNO TO HOLD-8
           
           READ GARFILE 
             INVALID
               DISPLAY "INVALID, SHOULDN'T BE"
               GO TO P0-1.
           
           IF G-PRINS = "001" GO TO P0-1.
           IF G-SEINS = "001" GO TO P0-1.
           
           MOVE SPACE TO FILEOUT01.
           
           STRING G-PRINS " pri " G-SEINS " sec " G-GARNO
             DELIMITED BY SIZE INTO FILEOUT01.

           WRITE FILEOUT01.

           GO TO P0-1.  

       P99. 
           CLOSE GARFILE FILEIN FILEOUT.
           STOP RUN.
