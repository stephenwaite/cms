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

           SELECT CHARCUR ASSIGN TO   "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL.  

           select fileout assign to   "S45" organization 
             line sequential.

           select fileout2 assign to  "S50" organization 
             line sequential.  

       DATA DIVISION.
       
       FILE SECTION.

       fd  filein.
       01  filein01.
           02 fi-acct pic x(8).           
      
       FD  GARFILE.
           COPY GARFILE.CPY IN "C:\Users\sid\cms\copylib\rri".
       
       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib\rri".

       fd  fileout.
       01  fileout01 pic x(200).

       fd  fileout2.
       01  fileout201 pic x(200).
       
       WORKING-STORAGE SECTION.    

       PROCEDURE DIVISION.

       P0.
           OPEN INPUT filein GARFILE CHARCUR.
           open output fileout fileout2.

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
              if G-PRIPOL(1:1) NOT = SPACE
                AND G-PRIPOL(9:1) NOT = SPACE
                AND G-PRIPOL(10:1) = SPACE
                AND G-PRIPOL(1:9) NOT = "000000000"
                 if g-garno = "GRI1203G"
                    DISPLAY G-PRIPOL
                    ACCEPT OMITTED.
                    
                 move g-garno to CC-KEY8
                 move "999" to cc-key3
                 start charcur key not > charcur-key
                   INVALID DISPLAY "NO CHARGES" 
                   ACCEPT OMITTED
                   go to p1
                 END-START  
                 READ CHARCUR PREVIOUS
                   AT END
                     DISPLAY "NO CHARGES" 
                     ACCEPT OMITTED
                     go to p1
                 END-READ
                 move space to fileout01
                 write fileout01 from charcur01
              else
               move space to fileout201
               write fileout201 from garfile01      
             end-if 
           END-IF      
           
           GO TO P1.         

       p99.
           CLOSE filein GARFILE charcur fileout fileout2.
           STOP RUN.

      