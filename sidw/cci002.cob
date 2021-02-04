      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cci002.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PROCFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC  RECORD KEY IS PROC-KEY
             LOCK MODE MANUAL.

           SELECT PROCcci ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC  RECORD KEY IS PROCcci-KEY
             LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.

       FD  PROCFILE.
           COPY procfile.CPY IN "C:\Users\sid\cms\copylib".

       FD  PROCcci.
       01  PROCcci01.
           02 PROCcci-KEY.
             03 PROCcci-KEY1 PIC X(5).

       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
       
       0005-START.
           open output proccci
           close proccci
           open i-o proccci
           OPEN INPUT procfile.
           
       P1.
           READ procfile next 
             AT END 
               GO TO P2.

           move proc-key to proccci-key

           read proccci 
             invalid
               write proccci01
             end-read
           GO TO P1.
       P2. 
           CLOSE proccci 
           STOP RUN.
