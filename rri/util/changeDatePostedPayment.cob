      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. changeDatePostedPayment.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.           

           SELECT PAYCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY.

       DATA DIVISION.

       FILE SECTION.     
       
       FD  paycur.
           COPY paycur.CPY IN "C:\Users\sid\cms\copylib".                         

       WORKING-STORAGE SECTION.      
       
       PROCEDURE DIVISION.
       
       P0.
           OPEN I-O paycur.
   
       P1. 
           READ paycur next WITH LOCK
             AT END
               GO TO P99.

           IF PC-DATE-T NOT = "20210828"
             GO TO P1.                                                                   

           DISPLAY "HERE IS THE paycur RECORD TO BE CHANGED " paycur01
           ACCEPT omitted       

           MOVE "20210528" TO PC-DATE-T           
           
           REWRITE paycur01.

           go to P1.
      

       P99. 
           CLOSE paycur
           STOP RUN.
