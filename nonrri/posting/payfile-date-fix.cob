      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. payfile-date-fix.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

	     SELECT PAYFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
	       ACCESS MODE IS DYNAMIC RECORD KEY IS PAYFILE-KEY.
	   
       DATA DIVISION.

       FILE SECTION.     

       FD  PAYFILE.
           COPY PAYFILE.CPY IN "C:\Users\sid\cms\copylib".
	            
       PROCEDURE DIVISION.

       0005-START.	     
	         OPEN I-O PAYFILE.	   
             
       P1. 
           READ PAYFILE NEXT WITH LOCK
             AT END
               GO TO P99.

             accept pd-date-e from date yyyymmdd
             REWRITE PAYFILE01.  
      
           GO TO P1.

       P99. 
           CLOSE PAYFILE
           STOP RUN.