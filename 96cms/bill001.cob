      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. bill001.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT FILEIN ASSIGN TO "S30"
            ORGANIZATION LINE SEQUENTIAL.
            SELECT PARMFILE ASSIGN TO "S35"
            ORGANIZATION LINE SEQUENTIAL.
            SELECT FILEOUT ASSIGN TO "S40"
            ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  PARMFILE.
       01 PARMFILE01. 
           02 PF1 PIC X.
           02 PF2 PIC X.
           02 PF3 PIC X.
           02 PF4 PIC X.
           02 PF5 PIC X(40).
       FD  FILEIN.
       01 FILEIN01.
           02 FILLER PIC X(41).
           02 FI-1 PIC X(1).
       FD  FILEOUT.
       01 FILEOUT01. 
           02 FO-1 PIC 9(4).
           02 FILLER PIC X VALUE SPACE.
           02 FO-2 PIC Z,ZZ9.99.
           02 FILLER PIC X VALUE SPACE.
           02 FO-3 PIC X(40).
       WORKING-STORAGE SECTION.
       01  X PIC 999.
       01 CNTR PIC 9999 VALUE 0.
       01  HOLD8 PIC X(8) VALUE SPACE.
       PROCEDURE DIVISION.
       A0.
           OPEN EXTEND FILEOUT.
           OPEN INPUT FILEIN PARMFILE.
           READ PARMFILE AT END GO TO P3.
       P1. READ FILEIN AT END GO TO P3.
           IF FI-1 = PF1 OR PF2 OR PF3 OR PF4 ADD 1 TO CNTR.
           GO TO P1.
       P3. MOVE PF5 TO FO-3
           MOVE CNTR TO FO-1
           COMPUTE FO-2 = CNTR * .86
           WRITE FILEOUT01.
           CLOSE FILEOUT. STOP RUN.
