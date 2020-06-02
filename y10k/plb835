      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. plb835.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO "S30".
           SELECT FILEOUT ASSIGN TO "S35"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILEOUT2 ASSIGN TO "S40"
           ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FILEIN.
       01  FI-1 PIC X.
     
       FD  FILEOUT.
       01  FILEOUT01 PIC X(360).
       FD  FILEOUT2.
       01  FILEOUT201 PIC X(360).

           
       WORKING-STORAGE SECTION.
       01 IN-TAB01.
          02 IN-TAB PIC X OCCURS 360 TIMES.
       01 X PIC 9999.
       01 CNTR PIC 999999 VALUE 1.
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT FILEIN OUTPUT FILEOUT FILEOUT2.
           MOVE 0 TO X
           MOVE SPACE TO IN-TAB01 FILEOUT01.
       P1.
           READ FILEIN AT END GO TO P99.
           IF FI-1 NOT = "~"
               IF X >360
                   DISPLAY X " " IN-TAB(1) IN-TAB(2) IN-TAB(3)
                   IN-TAB(4) " " CNTR
                   STRING CNTR " " IN-TAB01
                   DELIMITED BY SIZE INTO FILEOUT201
                   WRITE FILEOUT201
                   DISPLAY "SEGMENT > 360 BYTES TIME TO REPROGRAM"
                   ACCEPT OMITTED
                   GO TO P1
               END-IF
               ADD 1 TO X
               MOVE FI-1 TO IN-TAB(X)
               GO TO P1
           END-IF
           MOVE SPACE TO FILEOUT01
           WRITE FILEOUT01 FROM IN-TAB01
           MOVE 0 TO X
           MOVE SPACE TO FILEOUT01 IN-TAB01
           GO TO P1.
       P99.
           CLOSE FILEOUT FILEOUT2.
           STOP RUN.
