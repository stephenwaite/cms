      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ste005.
       AUTHOR. S WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT FILEIN ASSIGN TO  "S30" ORGANIZATION IS 
           LINE SEQUENTIAL.
	 
       SELECT CHARCUR ASSIGN TO       "S40" ORGANIZATION INDEXED
               ACCESS IS DYNAMIC RECORD KEY IS CHARCUR-KEY
               ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
               LOCK MODE MANUAL.           

       SELECT GARFILE ASSIGN TO "S50"     ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC        RECORD KEY IS G-GARNO
               ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
               LOCK MODE MANUAL.
	  
       DATA DIVISION.
       FILE SECTION.
       
       FD FILEIN.
       01 FILEIN01 PIC X(11).
       
       FD  GARFILE.
           COPY garfile.CPY IN "C:\Users\sid\cms\copylib\rri".                
       FD  CHARCUR.
           COPY charcur.CPY IN "C:\Users\sid\cms\copylib\rri".                
       
       WORKING-STORAGE SECTION.
       
       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT FILEIN GARFILE.
           OPEN I-O CHARCUR.
       P0.
           READ FILEIN AT END
               GO TO P99
           END-READ.

           MOVE FILEIN01 TO CHARCUR-KEY
           
           READ CHARCUR WITH LOCK INVALID KEY
               GO TO P0
           END-READ

           MOVE CC-KEY8 TO G-GARNO

           READ GARFILE INVALID KEY
              GO TO P0
           END-READ   

           MOVE G-SEINS TO CC-PAYCODE
           MOVE G-SE-ASSIGN TO CC-ASSIGN CC-NEIC-ASSIGN
           MOVE "2" TO CC-REC-STAT
           MOVE "20210816" TO CC-DATE-A
           REWRITE CHARCUR01
           GO TO P0.
           
       P99.
           CLOSE FILEIN GARFILE CHARCUR.
           STOP RUN.
