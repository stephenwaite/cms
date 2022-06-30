      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3

       IDENTIFICATION DIVISION.
       PROGRAM-ID. trpl001.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT GARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
             LOCK MODE MANUAL.
           
           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL.

           SELECT MPLRFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC        RECORD KEY IS MPLR-KEY
             LOCK MODE MANUAL.  

           SELECT FILEOUT ASSIGN TO "S45" organization is
             LINE sequential.  

       DATA DIVISION.
       
       FILE SECTION.

       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib\rri".
      
       FD  GARFILE.
           COPY GARFILE.CPY IN "C:\Users\sid\cms\copylib\rri".

        FD  MPLRFILE.
           COPY MPLRFILE.CPY IN "C:\Users\sid\cms\copylib\rri".    

       fd  fileout.
       01  fileout01 pic x(132).
       
       WORKING-STORAGE SECTION.    

       PROCEDURE DIVISION.

       P0.
           OPEN INPUT GARFILE CHARCUR MPLRFILE.
           open output fileout.

       R1.
           READ GARFILE NEXT
             AT END
               GO TO R99.

           IF G-TRINS = "004"
             MOVE G-GARNO TO MPLR-KEY
             READ MPLRFILE
               INVALID 
                 move space to fileout01
                 DISPLAY "no 04 in tertiary in mplrfile " g-garno 
                 string "no 04 in tertiary in mplrfile " g-garno 
                   delimited by size into fileout01
                 write fileout01  
                 GO TO R1
             END-READ
           ELSE
             GO TO R1  
           END-IF

           MOVE G-GARNO TO CC-KEY8
           move space to CC-KEY3
           START CHARCUR key not > CHARCUR-KEY.

       R2.
           READ CHARCUR NEXT
             AT END
               GO TO R99.

           IF CC-KEY8 NOT = G-GARNO
             GO TO R1.

           IF CC-DATE-T < "20210101"
             GO TO R1.

           move space to fileout01
           DISPLAY CHARCUR-KEY " CHARCUR-KEY " g-garno 
           string CHARCUR-KEY " CHARCUR-KEY " g-garno 
             delimited by size into fileout01
           write fileout01  
           
           GO TO R2.    

       R99.
           CLOSE GARFILE CHARCUR MPLRFILE fileout
           STOP RUN.
