      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cci003.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT FILEIN ASSIGN TO "S30"
             ORGANIZATION LINE SEQUENTIAL.

           SELECT CCIfile ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS CCI-KEY
             LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO "S40"
             ORGANIZATION LINE SEQUENTIAL.

           SELECT PROCCCI ASSIGN TO "S45" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC  RECORD KEY IS PROCCCI-KEY
             LOCK MODE MANUAL.

       DATA DIVISION.

       FILE SECTION.

       FD  PROCCCI.
       01  PROCCCI01.
           02 PROCCCI-KEY PIC X(5).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(11).

       FD  FILEIN.
       01  FILEIN01.
           02 FI-1 PIC X(5).
           02 FI-2 PIC x(5).
           02 FI-3 PIC X.

       FD  CCIFILE.
       01  CCIFILE01.
           02 CCI-KEY.
             03 CCI-KEY1 PIC X(5).
             03 CCI-KEY2 PIC X(5).
           02 CCI-IND PIC X.

       WORKING-STORAGE SECTION.

       01  HOLDKEY PIC X VALUE SPACE.

       PROCEDURE DIVISION.

       0005-START.
           OPEN INPUT FILEIN CCIfile PROCCCI
                OUTPUT FILEOUT.

       P1.
           READ FILEIN 
             AT END 
               GO TO P2.

           MOVE FI-1 TO PROCCCI-KEY

           READ PROCCCI 
             INVALID 
               GO TO P1.

           MOVE FI-2 TO PROCCCI-KEY

           READ PROCCCI 
             INVALID 
               GO TO P1.

           MOVE FI-1 TO CCI-key1
           MOVE FI-2 TO CCI-key2
           
           read CCIfile 
             INVALID
               write FILEOUT01 FROM FILEIN01.
             GO TO P1.

       P2. 
           CLOSE CCIfile FILEOUT FILEIN PROCCCI
           STOP RUN.
