      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ste006.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ACTFILE ASSIGN TO       "S30" ORGANIZATION INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS A-ACTNO
               ALTERNATE RECORD KEY IS A-GARNO WITH DUPLICATES
               ALTERNATE RECORD KEY IS NAME-KEY WITH DUPLICATES.

           SELECT FILEOUT ASSIGN TO       "S35" ORGANIZATION 
               LINE SEQUENTIAL.

            
       DATA DIVISION.
       FILE SECTION.
       
       FD  ACTFILE.
           copy actfile.cpy in "c:\users\sid\cms\copylib\rri".   

       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).

       01  LNAME PIC X(20).
       01  FNAME PIC X(20).

            
     
       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT ACTFILE.
           OPEN OUTPUT FILEOUT.
           MOVE LOW-VALUES TO A-ACTNO.


       10-ACTION.

           READ ACTFILE NEXT AT END GO TO P99.

           UNSTRING A-GARNAME DELIMITED BY "," INTO LNAME FNAME

           IF FNAME = SPACE GO TO 10-ACTION.

           DISPLAY LNAME " " FNAME.

           GO TO 10-ACTION.

       P99.
           CLOSE ACTFILE FILEOUT.
           STOP RUN.