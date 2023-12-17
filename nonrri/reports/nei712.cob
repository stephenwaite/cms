      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NEI712.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARDAL ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARDAL-KEY.
           SELECT CHARDALBK ASSIGN TO "S35"
           ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  CHARDAL
           BLOCK CONTAINS 4 RECORDS
           DATA RECORD IS CHARDAL01.
       01  CHARDAL01.
           02 CHARDAL-KEY PIC X(11).
           02 FILLER PIC X(174).
       FD  CHARDALBK
           DATA RECORD IS CHARDALBK01.
       01  CHARDALBK01 PIC X(185).
       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT CHARDAL OUTPUT CHARDALBK.
           MOVE SPACE TO CHARDAL-KEY
           START CHARDAL KEY NOT < CHARDAL-KEY INVALID GO TO P2.
       P1. READ CHARDAL NEXT AT END  GO TO P2.
           WRITE CHARDALBK01 FROM CHARDAL01
           GO TO P1.
       P2. CLOSE CHARDALBK. STOP RUN.
