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

           SELECT GARFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES.    


            
       DATA DIVISION.
       FILE SECTION.
       
       FD  ACTFILE.
           copy actfile.cpy in "c:\users\sid\cms\copylib\rri".   

       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).

       FD  GARFILE.
           copy garfile.cpy in "c:\users\sid\cms\copylib\rri".  
       WORKING-STORAGE SECTION.

       01  LNAME PIC X(20).
       01  FNAME PIC X(20).

            
     
       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT ACTFILE GARFILE.
           OPEN OUTPUT FILEOUT.
      *     MOVE LOW-VALUES TO A-ACTNO.
           MOVE LOW-VALUES TO G-GARNO.


       10-ACTION.
      *     READ ACTFILE NEXT AT END GO TO P99.
           READ GARFILE NEXT AT END GO TO P99.

           move space to lname fname

           UNSTRING G-GARNAME DELIMITED BY "," INTO LNAME FNAME.


           IF FNAME = SPACE GO TO 10-ACTION.
      *     IF NOT (A-ACTNO = "00061290" OR "00061292" OR "00061293")
      *        GO TO 10-ACTION.

           DISPLAY G-GARNAME
           DISPLAY G-GARNO
           DISPLAY LNAME " LNAME " FNAME " FNAME " G-ACCT " G-ACCT"
      *     ACCEPT OMITTED

      *     IF LNAME not = FNAME display lname " NOT equals " fname
      *       accept omitted.

      *     MOVE A-GARNO TO G-GARNO 

      *     READ GARFILE
      *       INVALID
      *         DISPLAY "INVALID READ OF GARNO " G-GARNO
      *       NOT INVALID
      *         DISPLAY GARFILE01.


           GO TO 10-ACTION.

       P99.
           CLOSE ACTFILE FILEOUT.
           STOP RUN.