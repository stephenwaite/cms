      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cci000.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT CCIFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS CCI-KEY
           LOCK MODE MANUAL.

           SELECT FILEIN ASSIGN TO "S35" ORGANIZATION IS
           LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
       FD  FILEIN.
       01  FILEIN01.
           02 FI-1 PIC X(5).
           02 FILLER PIC XXX.
           02 FI-2 PIC X(5).

       FD  CCIFILE.
       01  CCIFILE01.
           02 CCI-KEY.
             03 CCI-KEY1 PIC X(5).
             03 CCI-KEY2 PIC X(5).
           02 CCI-1 PIC X.
       WORKING-STORAGE SECTION.
       01  HOLDKEY PIC X(807) VALUE SPACE.
       PROCEDURE DIVISION.
       0005-START.
           open INPUT FILEIN
           open i-o CCIFILE.
       P1.
           READ FILEIN AT END GO TO P2.
           MOVE FI-1 TO CCI-KEY1
           MOVE FI-2 TO CCI-KEY2
           read CCIFILE invalid
             DISPLAY CCI-KEY " NOT ON FILE"
             GO TO P1
           END-READ
           DELETE CCIFILE RECORD
           DISPLAY FILEIN01 " RECORD DELETED"
           ACCEPT OMITTED
           GO TO P1.
       P2. 
           CLOSE CCIFILE FILEIN 
           STOP RUN.
