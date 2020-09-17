      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. xxxr053.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT PROCFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PROC-KEY
           LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO "S35"
           ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  FILEOUT.
       01  FILEOUT01.
           02 FO-1 PIC X(11).
           02 FILLER PIC X VALUE SPACE.
           02 FO-NAME PIC X(28).
           02 FILLER PIC X VALUE SPACE.
      *     02 FO-OLDAMT PIC Z,ZZ9.99.
           02 FILLER PIC X VALUE SPACE.
           02 FO-NEWAMT PIC Z,ZZ9.99.
           
       FD  PROCFILE
           DATA RECORD PROCFILE01.
       01  PROCFILE01.
           02 PROC-KEY.
             03 PROC-KEY1 PIC X(4).
             03 PROC-KEY2 PIC X(7).
           02 PROC-TYPE PIC X.
           02 PROC-TITLE PIC X(28).
           02 PROC-AMOUNT PIC 9(4)V99.

       WORKING-STORAGE SECTION.

       01  DIFF PIC 999.
       01  NUM PIC 9(4)V99.
       01  NUM1 PIC 9(7).
       01  NUM2 PIC 99.
       01  NUM3 PIC 99.
       01  NUM-4 PIC 9(4).
       01  NUM4 PIC 9(4).
       01  NEF-11 PIC ZZ,ZZ9.99.
       01  NEF-12 PIC ZZ,ZZ9.99.
       01  NEF-13 PIC ZZ,ZZ9.99.
       01  ALF-4.
           02 ALF4-1 PIC XX.
           02 ALF-42 PIC XX.
       01  ALF-2.
           02 ALF-21 PIC X.
           02 ALF-22 PIC X.
       01  ALF-6.
           02 ALF-61 PIC XXXX.
           02 ALF-62 PIC XX.
       01  NUM-1 PIC 9(7).

       PROCEDURE DIVISION.

       P0. 
           OPEN I-O PROCFILE
           OPEN OUTPUT FILEOUT.

       P1. 
           READ PROCFILE NEXT WITH LOCK
             AT END
              GO TO P2
           END-READ

           IF PROC-AMOUNT = 0
               GO TO P1
           END-IF

      *     MOVE PROC-AMOUNT TO FO-OLDAMT
           MOVE PROC-TITLE TO FO-NAME
           MOVE PROC-KEY2 TO FO-1
           MOVE PROC-AMOUNT TO NEF-12
           COMPUTE NUM1 = 1030 * PROC-AMOUNT
           COMPUTE NUM4 = NUM1 / 1000
           COMPUTE DIFF = (NUM1 - (NUM4 * 1000)) / 10.

       P1-0.

           IF DIFF > 99
               COMPUTE DIFF = DIFF - 100
               GO TO P1-0
           END-IF

           MOVE 0 TO NUM2.    
           
           IF DIFF = 0 
               GO TO P1-000
           END-IF    
           
           IF DIFF < 51 
               GO TO P1-000
           END-IF    
           
           ADD 1 TO NUM4 GO TO P1-000.

       P1-000.
           COMPUTE NUM = (100 * NUM4 + NUM2) / 100
           COMPUTE PROC-AMOUNT = NUM
           MOVE PROC-AMOUNT TO FO-NEWAMT
           WRITE FILEOUT01
           REWRITE PROCFILE01.
           GO TO P1.

       P2. 
           CLOSE PROCFILE FILEOUT
           STOP RUN.
