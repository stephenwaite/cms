      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3

       IDENTIFICATION DIVISION.
       PROGRAM-ID. billsort-prins-001.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILEIN ASSIGN TO    "S25" organization 
             line sequential.

           SELECT GARFILE ASSIGN TO   "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
             LOCK MODE MANUAL.

           select fileout assign to   "S45" organization 
             line sequential.

       DATA DIVISION.
       
       FILE SECTION.

       fd  filein.
       01  filein01.
           02 fi-acct pic x(8).           
      
       FD  GARFILE.
           COPY GARFILE.CPY IN "C:\Users\sid\cms\copylib\rri".
       
       fd  fileout.
       01  fileout01 pic x(200).
       
       WORKING-STORAGE SECTION.    

       PROCEDURE DIVISION.

       P0.
           OPEN INPUT filein GARFILE.
           open output fileout.

       P1. 
           MOVE SPACE TO filein01
           READ filein NEXT
             at end 
               go to p99.

           MOVE fi-acct to g-garno.    

       p2.                    
           READ GARFILE with lock
             invalid 
               display g-garno " not valid, try again"
               accept omitted
               go to p99
           end-read.              
       
       
           if G-PRINS = "001"
             write fileout01 from garfile01.  

           GO TO P1.         

       p99.
           CLOSE filein GARFILE fileout.
           STOP RUN.

      