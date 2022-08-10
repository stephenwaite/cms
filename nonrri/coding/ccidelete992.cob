      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ccidelete992.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CCIFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC RECORD KEY IS CCI-KEY
               LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.

       FD  CCIFILE.
           COPY ccifile.CPY IN "C:\Users\sid\cms\copylib".                           
       
       WORKING-STORAGE SECTION.
       01  ANS PIC XXX.
       01  X PIC 99.
       01  ALF2 PIC XX.
       01  ALF11 PIC X(11).
       01  ALF11X PIC X(11).
       01  RIGHT-2 PIC XX JUST RIGHT.
       01  NUM2 PIC 99.

       01  CCI-TB01.
           02 CCI-TAB PIC X(11) OCCURS 20 TIMES.
       
       PROCEDURE DIVISION.
       
       0005-START.
           OPEN INPUT CCIFILE
           MOVE SPACE TO CCI-KEY.
       
       P0.
           START CCIFILE KEY NOT < CCI-KEY
             INVALID
               GO TO P2.

       P1. 
           READ CCIFILE NEXT
             AT END
               GO TO P2.

           IF CCI-KEY1(1:3) = "992"
              OR CCI-KEY2(1:3) = "992"
                DISPLAY CCI-KEY1 " " CCI-KEY2 " " CCI-IND
            CLOSE CCIFILE
           OPEN I-O CCIFILE
           DELETE CCIFILE RECORD
           CLOSE CCIFILE           
           DISPLAY " RECORD DELETED"
           OPEN INPUT CCIFILE
           END-IF

           GO TO P1.     

       P2. 
           CLOSE CCIFILE
           STOP RUN.
