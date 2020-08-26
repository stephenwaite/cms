      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CHARY2K.
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
       01  FILEOUT01.
           02 FO-1  PIC XX.
           02 FO-2 PIC X(1068).

       FD  FILEOUT2.
       01  FILEOUT201.
           02 FO-Y PIC ZZZ9.
           02 FILLER PIC X VALUE SPACE.
           02 FO-X PIC ZZZ9.
           02 FILLER PIC X(10).
       01  ERROUT2 PIC X(1070).    

       WORKING-STORAGE SECTION.
       01  S-CNTR PIC 9 VALUE 0.
       01  X PIC 9999 VALUE 0.
       01  Y PIC 9999 VALUE 0.
       01  A PIC 9999.
       01  B PIC 9999.
       01  CODE-X PIC X.
       01  CODE-Y PIC X.
       01  Z PIC 9999.
       01  HOLD-1 PIC X.
       01  TAB01.
             02 TAB2 PIC X OCCURS 4000 TIMES.
       01  FILL01.
             02 FILL2 PIC X OCCURS 1068 TIMES.
       01 ALF1 PIC X.

       PROCEDURE DIVISION.
       P0.
           OPEN INPUT FILEIN
           OPEN OUTPUT FILEOUT FILEOUT2.
           MOVE SPACE TO TAB01.
           
           READ FILEIN AT END GO TO P99.
           
           MOVE FI-1 TO ALF1
           
           READ FILEIN AT END GO TO P99.
           
           IF NOT (FI-1 = "#" AND ALF1 = "#")
           DISPLAY "BAD BEGINNING TO FILE"
           ACCEPT ALF1
           GO TO P99.
       P1. 
           READ FILEIN AT END GO TO P99.
           IF FI-1 NOT = "+"
            ADD 1 TO X
            MOVE FI-1 TO TAB2(X)
           GO TO P1.
           MOVE FI-1 TO ALF1
           READ FILEIN AT END GO TO P99.
           IF FI-1 NOT = "+"
            ADD 1 TO X
            MOVE ALF1 TO TAB2(X)
            ADD 1 TO X
            MOVE FI-1 TO TAB2(X)
            GO TO P1.
            MOVE "##" TO FO-1
            MOVE TAB01 TO FO-2
            WRITE FILEOUT01.
           MOVE 0 TO X
           MOVE SPACE TO TAB01.
       P2.
           READ FILEIN AT END GO TO P99.
           IF NOT (FI-1 = "$")
            ADD 1 TO X
            MOVE FI-1 TO TAB2(X)
           GO TO P2.
             MOVE FI-1 TO ALF1
             READ FILEIN AT END GO TO P99
             END-READ
           IF FI-1 NOT = "$"
            ADD 1 TO X
            MOVE ALF1 TO TAB2(X)
            ADD 1 TO X
            MOVE FI-1 TO TAB2(X)
           GO TO P2.
            MOVE "++" TO FO-1
            MOVE TAB01 TO FO-2
            WRITE FILEOUT01.
           MOVE 0 TO X
           MOVE SPACE TO TAB01.
       P3.
           READ FILEIN AT END GO TO P99.
           IF NOT (FI-1 = "$" OR "#")
            ADD 1 TO X
            MOVE FI-1 TO TAB2(X)
           GO TO P3.
             MOVE FI-1 TO ALF1
             READ FILEIN AT END GO TO P99
             END-READ
           IF FI-1 NOT = ALF1
            ADD 1 TO X
            MOVE ALF1 TO TAB2(X)
            ADD 1 TO X
            MOVE FI-1 TO TAB2(X)
           GO TO P3.
            MOVE "$$" TO FO-1
            MOVE TAB01 TO FO-2
            WRITE FILEOUT01
            MOVE 0 TO X
            MOVE SPACE TO TAB01
            IF ALF1 = "#" GO TO P1.
            MOVE 0 TO X
            MOVE SPACE TO TAB01
            GO TO P3.
       P99.
            MOVE "$$" TO FO-1
            MOVE TAB01 TO FO-2
            WRITE FILEOUT01
            CLOSE FILEOUT FILEOUT2 FILEIN
            STOP RUN.
