      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rsc052.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PROCFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS PROC-KEY.
           
           SELECT FILEOUT ASSIGN TO "S40" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  PROCFILE.
           COPY procfile.CPY IN "C:\Users\sid\cms\copylib".
       
       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).

       WORKING-STORAGE SECTION.
       
       PROCEDURE DIVISION.

       P0. 
           open I-O PROCFILE.
           OPEN OUTPUT FILEOUT.

       P1. 
           READ PROCFILE NEXT WITH LOCK 
             AT END 
               GO TO P99.

           IF PROC-AMOUNT = 0 GO TO P1.

           COMPUTE CARE-AMOUNT = PROC-AMOUNT
           REWRITE PROCFILE01
           GO TO P1.

       P99. 
           CLOSE PROCFILE FILEOUT. 
           STOP RUN.
