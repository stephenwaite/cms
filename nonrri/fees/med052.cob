      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. med052.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PROCFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS PROC-KEY.
           
           SELECT medfile ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS MED-KEY
             LOCK MODE MANUAL.
           
           SELECT FILEOUT ASSIGN TO "S40" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       
       FD  medfile.
       01  medfile01.
           02 MED-KEY.
             03 MED-KEY1 PIC X(5).
             03 MED-KEY2 PIC XX.
           02 MED-AMOUNT PIC 9(4)V99.

       FD  PROCFILE.
           COPY procfile.CPY IN "C:\Users\sid\cms\copylib".
       
       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).

       WORKING-STORAGE SECTION.
       01  DISPLAY-DATE.
           02 T-MM PIC XX.
           02 FILLER PIC X VALUE "/".
           02 T-DD PIC XX.
           02 FILLER PIC X VALUE "/".
           02 T-CC PIC XX.
           02 T-YY PIC XX.
       
       PROCEDURE DIVISION.

       P0. 
           OPEN INPUT medfile 
           open I-O PROCFILE.
           OPEN OUTPUT FILEOUT.

       P1. 
           READ PROCFILE NEXT WITH LOCK 
             AT END 
               GO TO P99.

           IF PROC-AMOUNT = 0 GO TO P1.

           MOVE PROC-KEY TO MED-KEY
           MOVE SPACE TO MED-KEY2

           READ medfile 
             INVALID
               WRITE FILEOUT01 FROM PROCFILE01
               GO TO P1.

           COMPUTE CARE-AMOUNT = MED-AMOUNT
           REWRITE PROCFILE01
           GO TO P1.

       P99. 
           CLOSE medfile PROCFILE FILEOUT. 
           STOP RUN.
