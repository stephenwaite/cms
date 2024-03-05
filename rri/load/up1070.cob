      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. up1070.
       AUTHOR. S WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  FILEOUT.
       01  FILEOUT01 PIC X(1070).
       FD  FILEIN
           BLOCK CONTAINS 1 TO 1070 CHARACTERS.
       01  FILEIN01 PIC X(1070).
       WORKING-STORAGE SECTION.
       01  TABIN01.
           02 TABIN PIC X OCCURS 1070 TIMES.
       01 LOWERALFA PIC X(26).
       01 UPPERALFA PIC X(26).
       01  LOWERTAB01.
           02 LOWERTAB PIC X OCCURS 26 TIMES.
       01  UPPERTAB01.
           02 UPPERTAB PIC X OCCURS 26 TIMES.
       01 X PIC 9999.
       01 Y PIC 99.
       01 ALF1 PIC X.

       PROCEDURE DIVISION.
       0005-START.
           MOVE "abcdefghijklmnopqrstuvwxyz" TO LOWERALFA
           MOVE "ABCDEFGHIJKLMNOPQRSTUVWXYZ" TO UPPERALFA
           MOVE LOWERALFA TO LOWERTAB01
           MOVE UPPERALFA TO UPPERTAB01.
           OPEN INPUT FILEIN OUTPUT FILEOUT.
       P1. MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P2.
           MOVE SPACE TO TABIN01 FILEOUT01 
           MOVE FILEIN01 TO TABIN01
           if filein01(1:2) = "##"
           PERFORM A1 VARYING X FROM 1 BY 1 UNTIL X > 265
           perform a1 varying x from 296 by 1 until x > 1070
           else
           
           PERFORM A1 VARYING X FROM 1 BY 1 UNTIL X > 1070
           end-if
           WRITE FILEOUT01 FROM TABIN01
           GO TO P1.
       A1. IF TABIN(X) NOT = SPACE
           PERFORM A2 VARYING Y FROM 1 BY 1 UNTIL Y > 26.
       A2. IF TABIN(X) = LOWERTAB(Y)
           MOVE UPPERTAB(Y) TO TABIN(X).
       P2.
           CLOSE FILEOUT FILEIN.
           STOP RUN.
