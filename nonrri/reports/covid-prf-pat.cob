      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. danpl014.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARDATE ASSIGN TO "S25"
             ORGANIZATION LINE SEQUENTIAL.
          
           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS SEQUENTIAL RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES.

           SELECT FILEOUT ASSIGN TO "S40"
             ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib".             
      
       FD  CHARDATE.
       01  CHARDATE01. 
           02 LOW-CHARDATE PIC X(8).
           02 HIGH-CHARDATE PIC X(8).               

       FD  FILEOUT.
       01  FILEOUT01 pic x(80).          

       WORKING-STORAGE SECTION.
       01  tot-inpt pic 999999.
       01  tot-outp pic 999999.
       01  tot-emer pic 999999.

       PROCEDURE DIVISION.
       
       P0.
           OPEN INPUT CHARDATE CHARCUR.
           OPEN OUTPUT FILEOUT.
           READ CHARDATE.
           move 0 to tot-inpt tot-outp tot-emer.
           move low-values to charcur-key.

       P1. 
           READ CHARCUR next
             AT END
               GO TO P99.
          
           IF CC-DATE-T < LOW-CHARDATE OR > HIGH-CHARDATE
             GO TO P1.         

           if cc-place = "C"
             add 1 to tot-inpt
             go to p1
           else 
             add 1 to tot-outp
             go to p1.  
      
           
       P99.
           string low-chardate " " high-chardate 
             delimited by size into fileout01.
           write fileout01.
           move space to fileout01.
           string "inpt   emer   outp" delimited by size
             into fileout01.
           write fileout01.
           move space to fileout01.
           string tot-inpt " " tot-emer " " tot-outp delimited by size
             into fileout01.
           write fileout01.

           CLOSE CHARDATE CHARCUR FILEOUT.
           STOP RUN.
