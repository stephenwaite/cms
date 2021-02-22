      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrr250.
       AUTHOR. S WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILEIN ASSIGN TO "S30" ORGANIZATION LINE
             SEQUENTIAL.

           SELECT GARFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
             LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.             

       FD  FILEIN.
       01  FI1 PIC X(8).
      
       FD  GARFILE.
           COPY garfile.CPY IN "C:\Users\sid\cms\copylib\rri".

       PROCEDURE DIVISION.

       P0.
           OPEN INPUT FILEIN
           OPEN I-O GARFILE.

       P1.      
           READ FILEIN AT END GO TO P99.

           MOVE FI1 TO G-GARNO           

           READ GARFILE WITH LOCK INVALID
             DISPLAY FI1 "  HUH?"
             ACCEPT OMITTED
             GO TO P1
           END-READ

           DISPLAY "going to delete " GARFILE01.
           ACCEPT OMITTED
           DELETE GARFILE RECORD.
           GO TO P1.

       P99.
           CLOSE FILEIN GARFILE.


