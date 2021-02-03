      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cci001.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILEIN ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  FILEIN.
       01  FILEIN01 PIC X(32).
       
       FD  FILEOUT.
       01  FILEOUT01 PIC X(11).

       WORKING-STORAGE SECTION.
       01  f1 pic x(5).
       01  f2 pic x(5).
       01  f3 pic x.
       01  f4 pic x.
       01  f5 pic x.
       01  f6 pic x.
       
       01  tab-1 pic X value H"09".

       PROCEDURE DIVISION.

       0005-START.
           
           OPEN INPUT FILEIN 
           open OUTPUT FILEOUT.

       P2.
           READ FILEIN 
             AT END 
               GO TO P3.

           move space to fileout01
           move space to f1 f2 f3 f4 f5 f6

           unstring FILEIN01 delimited by tab-1 into
              f1 f2 f3 f4 f5 f6

           string f1 f2 f6 delimited by size into FILEOUT01   

           WRITE FILEOUT01
           GO TO P2.

           
       P3. 
           CLOSE FILEOUT 
           STOP RUN.
