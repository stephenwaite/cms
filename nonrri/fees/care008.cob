      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. care008.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT  medfile ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS MED-KEY
             LOCK MODE MANUAL.

           SELECT FILEIN ASSIGN TO "S35"
             ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       
       FILE SECTION.

       FD FILEIN.
       01  FILEIN01 pic x(120).
           
       FD  medfile.
       01  medfile01.
           02 MED-KEY.
             03 MED-KEY1 PIC X(5).
             03 MED-KEY2 PIC XX.
           02 MED-AMT PIC 9(4)V99.
           
       WORKING-STORAGE SECTION.

       01  FI-1 PIC X.
       01  FI-CPT PIC X(5).
       01  FI-MOD PIC XX.
       01  FI-AMT PIC X(7).
       01  FI-NONPAR PIC X(7).
       01  FI-LIMIT PIC X(7).
       01  FI-EFFDATE PIC X(10).
       01  NUM2 PIC 99.
       01  NUM4 PIC 9999.
       01  ALF2 PIC XX.
       01  ALF5 PIC X(5).
       01  DOLLARX PIC X(4).
       01  CENTX PIC XX.
       01  right-4 pic xxxx just right.
       01  X-AMT PIC 9(4)V99.

       PROCEDURE DIVISION.
       
       P0.
           open output medfile
           close medfile
           OPEN INPUT FILEIN.
           OPEN I-O medfile.

       P1.
           READ FILEIN 
             AT END 
               GO TO P99.

           MOVE SPACE TO FI-1 FI-CPT FI-MOD FI-AMT.    

           UNSTRING FILEIN01 DELIMITED BY "," INTO     
             FI-1 FI-CPT FI-MOD FI-AMT

           IF FI-1 = "C" GO TO P1.

           IF FI-1 = "#" GO TO P1.

           IF FI-MOD = "TC" GO TO P1.

       P1-1.
           MOVE SPACE TO RIGHT-4 CENTX
           UNSTRING FI-AMT DELIMITED BY "." INTO right-4 CENTX
           MOVE FI-CPT TO MED-KEY1
           MOVE FI-MOD TO MED-KEY2
           INSPECT RIGHT-4 REPLACING ALL " " BY "0".
           INSPECT CENTX REPLACING  ALL " " BY "0".

           IF CENTX NOT NUMERIC
             DISPLAY FI-CPT " CENTX NOT NUM " CENTX
             ACCEPT OMITTED
           END-IF
           
           IF RIGHT-4 NOT NUMERIC
             DISPLAY FI-CPT " RIGHT-4 NOT NUM " RIGHT-4
             ACCEPT OMITTED
           END-IF

           MOVE RIGHT-4 TO NUM4
           move CENTX to num2
      *     display right-4
      *     display centx
           COMPUTE MED-AMT = num4 + (num2 / 100)
           WRITE medfile01
           GO TO P1.

       
       P99.           
           CLOSE medfile FILEIN.
           STOP RUN.
