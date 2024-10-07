      * @package cms
      * @link    https://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2024 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. icd10010.
       AUTHOR. S WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT DIAGFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC RECORD KEY IS DIAG-KEY
               ALTERNATE RECORD KEY IS DIAG-TITLE WITH DUPLICATES
               LOCK MODE MANUAL.
           SELECT FILEIN ASSIGN TO "S35"
               ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  DIAGFILE.
       01  DIAG01.
           02 DIAG-KEY PIC X(7).
           02 DIAG-TITLE PIC X(61).
           02 DIAG-MEDB PIC X(5).

       FD  FILEIN.
       01  FILEIN01 PIC X(90).
       
       PROCEDURE DIVISION.
       
       P0. 
           OPEN INPUT FILEIN
           OPEN I-O DIAGFILE. 

       P1.
           MOVE SPACE TO FILEIN01.
           READ FILEIN AT END GO TO P9.

           IF FILEIN01(15:1) = "0"
               GO TO P1
           END-IF
           
           MOVE FILEIN01(7:7) TO DIAG-KEY
           MOVE FILEIN01(17:61) TO DIAG-TITLE
           MOVE SPACE TO DIAG-MEDB
           WRITE DIAG01
               INVALID
                   DISPLAY "INVALID ADD"
                   DISPLAY FILEIN01
                   ACCEPT OMITTED
           END-WRITE
           
           GO TO P1.
           

       P9. 
           CLOSE DIAGFILE FILEIN. 
           STOP RUN.
