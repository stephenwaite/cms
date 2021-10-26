      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. del-chg-by-e-dt.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.           

           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS SEQUENTIAL RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES.          

       DATA DIVISION.

       FILE SECTION.     
       
       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib\rri".                         

       WORKING-STORAGE SECTION.      
       
       PROCEDURE DIVISION.
       
       P0.
           OPEN I-O CHARCUR.
   
       P1. 
           READ CHARCUR WITH LOCK
             AT END
               GO TO P99.

           IF CC-DATE-P NOT = "20210612"
             GO TO P1.                                                                   

           DISPLAY "HERE IS THE CHARCUR RECORD TO BE DELETED " CHARCUR01
           ACCEPT omitted                      
           
           DELETE CHARCUR RECORD.

           go to P1.
      

       P99. 
           CLOSE CHARCUR
           STOP RUN.
