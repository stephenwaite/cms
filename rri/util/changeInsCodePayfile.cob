      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. changeInsCodePayfile.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.           
           
           SELECT GARFILE ASSIGN TO   "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
             LOCK MODE MANUAL.

           SELECT PAYFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS PAYFILE-KEY.

       DATA DIVISION.

       FILE SECTION.     
       
       FD  GARfile.
           COPY GARfile.CPY IN "C:\Users\sid\cms\copylib\rri".                         

       FD  payfile.
           COPY payfile.CPY IN "C:\Users\sid\cms\copylib".                         

       WORKING-STORAGE SECTION.      
       
       PROCEDURE DIVISION.
       
       P0.
           OPEN I-O payfile.
           OPEN INPUT GARFILE.
   
       P1. 
           READ payfile next WITH LOCK
             AT END
               GO TO P99.

           MOVE PD-KEY8 TO G-GARNO
           READ GARFILE 
               INVALID GO TO P1.


           IF PD-PAYCODE NOT = G-PRINS
               MOVE G-PRINS TO PD-PAYCODE
               REWRITE PAYFILE01.

           go to P1.
      

       P99. 
           CLOSE PAYFILE GARFILE.
           STOP RUN.
