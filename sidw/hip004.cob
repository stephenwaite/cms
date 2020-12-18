      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. hip004.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILEIN ASSIGN TO "S30"
           ORGANIZATION LINE SEQUENTIAL.
       
           SELECT FILEOUT ASSIGN TO "S35"
           ORGANIZATION LINE SEQUENTIAL.
       
       DATA DIVISION.
       
       FILE SECTION.
       
       FD  FILEIN.
       01  FILEIN01. 
           02 FI-1 PIC XXX.
           02 FILLER PIC X(157).
       
       FD  FILEOUT.
       01  FILEOUT01 PIC X(160).
       
       WORKING-STORAGE SECTION.
       
       01 X PIC 999.
       01 INTAB01.
          02 INTAB PIC X OCCURS 160 TIMES.
       
       PROCEDURE DIVISION.
       
       P0.
           OPEN INPUT FILEIN OUTPUT FILEOUT .
       
       P1. 
           READ FILEIN AT END GO TO P99.
           
           IF NOT (FI-1 = "REF" OR "NM1" OR "N3*"
            OR "N4*" OR "SBR" OR "PAT" OR "CLM") 
           
           WRITE FILEOUT01 FROM FILEIN01 
           GO TO P1.

           MOVE FILEIN01 TO INTAB01
       
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 160
             IF INTAB(X) =
               "%" OR "@" OR "[" OR "]" OR "_" OR "{" OR "}" 
               OR "\" OR "|" OR "<" OR ">" OR "#" OR "$" 
               MOVE SPACE TO INTAB(X) 
             END-IF
             
             IF INTAB(X) = "~"
               MOVE 161 TO X              
             END-IF             
           END-PERFORM
           
           WRITE FILEOUT01 FROM INTAB01
           GO TO P1.
       
       P99.
           CLOSE FILEOUT 
           STOP RUN.
