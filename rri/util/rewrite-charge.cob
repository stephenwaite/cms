      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rewrite-charge.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARCUR ASSIGN TO "S50" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES.

           SELECT FILEOUT ASSIGN TO "S60" ORGANIZATION LINE SEQUENTIAL.  
  

       DATA DIVISION.

       FILE SECTION.
       
       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib\rri".

       FD  FILEOUT.
       01  FILEOUT01 PIC X(160).                  
           
       WORKING-STORAGE SECTION.
       
       01  OLD-CHARCUR-KEY PIC X(11).
       01  NEW-KEY PIC X(11).

       
       PROCEDURE DIVISION.
       
       P0.
           OPEN INPUT CHARCUR.
           OPEN OUTPUT FILEOUT.

           MOVE "LU;3041G001" TO CHARCUR-KEY.

           START CHARCUR KEY = CHARCUR-KEY
             INVALID
               DISPLAY "COULDN'T START CHARCUR"
               GO TO P99.

       P1.    
           READ CHARCUR WITH LOCK
      *       MOVE FILEIN01 TO CHARCUR01  
      *       DISPLAY "CAN REWRITE " CHARCUR01
      *       REWRITE CHARCUR01.
              MOVE "LU 3041G001" TO CHARCUR-KEY
              MOVE "LU 3041G" TO CC-PATID
              WRITE FILEOUT01 FROM CHARCUR01.

           
       P99. 
           CLOSE CHARCUR FILEOUT.
           STOP RUN.
