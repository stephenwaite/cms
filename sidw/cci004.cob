      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TWO001.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO "S30"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT ccifile ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS cci-KEY
           LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
       FD  cciFILE.
       01  cciFILE01.
           02 CCI-KEY.
             03 cci-KEY1 PIC X(5).
             03 cci-KEY2 PIC X(5).
           02 CCI-IND PIC X.
       FD  FILEIN.
       01  FILEIN01 PIC X(11).
       WORKING-STORAGE SECTION.
       01  HOLDKEY PIC X VALUE SPACE.
       PROCEDURE DIVISION.
       0005-START.
           open OUTPUT CCIFILE
           OPEN INPUT fileIN.
           go to p2.
       P1.
           READ FILEIN AT END GO TO P2.
           MOVE FILEIN01 TO CCIFILE01
           write cciFILE01
           end-WRITE
           GO TO P1.
       P2. 
           CLOSE ccifile filein
           STOP RUN.
