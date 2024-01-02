      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ccc803.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC     RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.

       DATA DIVISION.

       FILE SECTION.
       FD  CHARCUR
           DATA RECORD IS CHARCUR01.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC PIC X(7).
           02 CC-MOD2 PIC XX.
           02 CC-MOD3 PIC XX.
           02 CC-MOD4 PIC XX.
           02 CC-AMOUNT PIC S9(4)V99.
           02 CC-DOCR PIC X(3).
           02 CC-DOCP PIC X(2).
           02 CC-PAYCODE PIC XXX.
           02 CC-STUD PIC X.
           02 CC-WORK PIC XX.
           02 CC-DAT1 PIC X(8).
           02 CC-RESULT PIC X.
           02 CC-ACT PIC X.
           02 CC-SORCREF PIC X.
           02 CC-COLLT PIC X.
           02 CC-AGE PIC X.
           02 CC-PAPER PIC X.
           02 CC-PLACE PIC X.
           02 CC-EPSDT PIC X.
           02 CC-DATE-T PIC X(8).
           02 CC-DATE-A. 
              03  CC-DATEYYYYMM-A PIC X(6).
              03  CC-DATEDD-A PIC XX.
           02 CC-DATE-P PIC X(8).
           02 CC-REC-STAT PIC X.
           02 CC-DX2 PIC X(7).
           02 CC-DX3 PIC X(7).
           02 CC-ACC-TYPE PIC X.
           02 CC-DATE-M PIC X(8).
           02 CC-ASSIGN PIC X.
           02 CC-NEIC-ASSIGN PIC X.
           02 CC-DX4 PIC X(7).
           02 CC-DX5 PIC X(7).
           02 CC-DX6 PIC X(7).
           02 CC-FUTURE PIC X(6).

       WORKING-STORAGE SECTION.
       01  CONSTANTS.
           02 CNTR PIC 999999 VALUE 0.

       PROCEDURE DIVISION.

       0005-START.
           OPEN I-O CHARCUR.
           MOVE SPACE TO CHARCUR-KEY
           START CHARCUR KEY > CHARCUR-KEY
             INVALID
               GO TO P2
           END-START.

       P1.
           READ CHARCUR NEXT WITH LOCK
             AT END
               GO TO P2
           END-READ

           IF CC-ASSIGN = "A"
               GO TO P1
           END-IF

           IF CC-DATEYYYYMM-A < "202309"
               GO TO P1
           END-IF

           IF CC-DATEYYYYMM-A = "202309"
               MOVE "20231031" TO CC-DATE-A
               GO TO A2
           END-IF

           IF CC-DATEYYYYMM-A = "202310"
               MOVE "20231130" TO CC-DATE-A
               GO TO A2
           END-IF

           IF CC-DATEYYYYMM-A = "202311"
               MOVE "20231231" TO CC-DATE-A
               GO TO A2
           END-IF

           GO TO P1.
       A2.  
           REWRITE CHARCUR01
           GO TO P1.
           
       P2.
           CLOSE CHARCUR.
           DISPLAY CNTR
           STOP RUN.
