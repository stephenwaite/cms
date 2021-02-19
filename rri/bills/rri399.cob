      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.

       PROGRAM-ID. rri399.

       AUTHOR. SID WAITE.

       DATE-COMPILED. TODAY.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT GARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES.

           SELECT FILEOUT ASSIGN TO "S35"
           ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  GARFILE.
           copy garfile.cpy in "c:\users\sid\cms\copylib\rri".

       FD FILEOUT.
       01 FILEOUT01 PIC X(8).

       PROCEDURE DIVISION.

       P0.
           OPEN INPUT GARFILE OUTPUT FILEOUT.

       P1. 
           READ GARFILE AT END GO TO P2.
           
           IF G-DUNNING = "2" WRITE FILEOUT01 FROM G-GARNO.
           
           GO TO P1.

       P2. 
           CLOSE FILEOUT. 
           STOP RUN.
