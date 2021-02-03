      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TWO001.
       AUTHOR. SID WAITE.
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
       FD  FILEIN.
       01  F1 PIC X(200).
       FD  FILEOUT.
       01  FILEOUT01 PIC X(11).

       WORKING-STORAGE SECTION.
       01  FL1 PIC X(5).
       01  FL2 PIC X(5).
       01  FL3 PIC X(5).
       01  FL4 PIC X(8).
       01  FL5 PIC X(8).
       01  FL6 PIC X(5).
       01  TAB-X PIC X VALUE H"09".
       PROCEDURE DIVISION.
       0005-START.
           
           OPEN INPUT FILEIN OUTPUT FILEOUT.
       P2.
           MOVE SPACE TO F1.
           READ FILEIN AT END GO TO P3.
           MOVE SPACE TO FL1 FL2 FL3 FL4 FL5 FL6
           UNSTRING F1 DELIMITED BY TAB-X INTO
           FL1 FL2 FL3 FL4 FL5 FL6
           move space to fileout01
           STRING  FL1 FL2 FL6 DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           GO TO P2.

           
       P3. 
           CLOSE FILEOUT 
           STOP RUN.
