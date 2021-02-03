      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrr259.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARFILE-KEY
             LOCK MODE MANUAL.
       
       DATA DIVISION.
       
       FILE SECTION.
       
       FD  CHARFILE
           BLOCK CONTAINS 4 RECORDS
           DATA RECORD IS CHARFILE01.
       01  CHARFILE01.
           02 CHARFILE-KEY.
             03 CD-KEY8 PIC X(8).
             03 CD-KEY3 PIC XXX.
           02 CD-PATID PIC X(8).
           02 CD-CLAIM PIC X(6).
           02 CD-SERVICE PIC X.
           02 CD-DIAG PIC X(7).
           02 CD-PROC PIC X(11).
           02 CD-MOD2 PIC XX.
           02 CD-MOD3 PIC XX.
           02 CD-MOD4 PIC XX.
           02 CD-AMOUNT PIC S9(4)V99.
           02 CD-DOCR PIC X(3).
           02 CD-DOCP PIC X(2).
           02 CD-PAYCODE PIC XXX.
           02 CD-STAT PIC X.
           02 CD-WORK PIC XX.
           02 CD-DAT1 PIC X(8).
           02 CD-RESULT PIC X.
           02 CD-ACT PIC X.
           02 CD-SORCREF PIC X.
           02 CD-COLLT PIC X.
           02 CD-AGE PIC X.
           02 CD-PAPER PIC X.
           02 CD-PLACE PIC X.
           02 CD-NAME PIC X(24).
           02 CD-EPSDT PIC X.
           02 CD-DATE-T PIC X(8).
           02 CD-DATE-E PIC X(8).
           02 CD-ORDER PIC X(6).
           02 CD-DX2 PIC X(7).
           02 CD-DX3 PIC X(7).
           02 CD-DATE-A PIC X(8).
           02 CD-ACC-TYPE PIC X.
           02 CD-DATE-M PIC X(8).
           02 CD-ASSIGN PIC X.
           02 CD-NEIC-ASSIGN PIC X.
           02 CD-DX4 PIC X(7).
           02 CD-DX5 PIC X(7).
           02 CD-DX6 PIC X(7).
           02 CD-FUTURE PIC X(6).
       WORKING-STORAGE SECTION.
       01  ANS PIC XXX.
       01  DATE-LOW PIC X(8).
       01  DATE-HIGH PIC X(8).
       01  NUMTAB01.
           02 NUMTAB PIC XX OCCURS 8 TIMES.
       01  HOLD8 PIC X(8) VALUE SPACE.
       01  HOLDDOC PIC XX VALUE "01".
       01  X PIC 9.
       01  Y PIC 9.
       01  ALF1 PIC X.
       01  ALF2 PIC XX.
       01  RIGHT-2 PIC XX JUST RIGHT.
       PROCEDURE DIVISION.
       0005-START.
           MOVE "06" TO NUMTAB(1).
           OPEN I-O CHARFILE.
           MOVE SPACE TO CHARFILE-KEY.
           MOVE 0 TO Y.
       P00.
           DISPLAY "ENTER NUMBERS I.E. 1689,169..., E = END".
           ACCEPT ALF1
           IF ALF1 = "E" AND Y = 0 DISPLAY "BAD" GO TO P00.
           IF ALF1 = "E" GO TO P0.
           IF NOT (ALF1 = "1" OR "6" OR "8" OR "9")
           DISPLAY "BAD" GO TO P00.
           ADD 1 TO Y
           IF Y > 8 MOVE 8 TO Y GO TO P0.
           MOVE SPACE TO RIGHT-2
           UNSTRING ALF1 DELIMITED " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING LEADING " " BY "0".
           IF RIGHT-2 = "01" MOVE "10" TO RIGHT-2.
           MOVE RIGHT-2 TO NUMTAB(Y) 
           GO TO P00.
           
       P0. DISPLAY "ENTER LOW DATE YYYYMMDD".
           ACCEPT DATE-LOW
           IF DATE-LOW = "ALL" MOVE "000000" TO DATE-LOW
           MOVE "999999" TO DATE-HIGH GO TO P000.
           IF DATE-LOW = "END" DISPLAY "END" GO TO P2.
       P-00. DISPLAY "ENTER HIGH DATE YYMMDD".
            ACCEPT DATE-HIGH.
            IF DATE-HIGH = "BK" GO TO P0.
            IF DATE-HIGH = "ALL" MOVE "000000" TO DATE-LOW
            MOVE "999999" TO DATE-HIGH GO TO P000.
            IF DATE-HIGH = "END" DISPLAY "END" GO TO P2.
            IF DATE-HIGH = "S" MOVE DATE-LOW TO DATE-HIGH
            DISPLAY "SAME AS LOW DATE" GO TO P000.
            GO TO P-00.
            
       P000.
            MOVE 0 TO X.   
            DISPLAY "READY TO GO Y/N".
            ACCEPT ANS
            IF ANS = "Y" 
            MOVE NUMTAB(1) TO HOLDDOC GO TO P1.
            IF ANS  = "N" GO TO P000.
       P1. READ CHARFILE NEXT WITH LOCK AT END  GO TO P2.
           IF CD-DATE-T < DATE-LOW OR CD-DATE-T > DATE-HIGH
           GO TO P1.
       P1-0.
           IF CD-KEY8 = HOLD8 
           MOVE HOLDDOC TO CD-DOCP
           DISPLAY HOLDDOC " " CD-DATE-T " " CD-PROC " " CD-NAME
           GO TO P1-1.
       P1-00.
           ADD 1 TO X
           IF X > Y MOVE 1 TO X.
           MOVE NUMTAB(X) TO HOLDDOC
           MOVE CD-KEY8 TO HOLD8
           GO TO P1-0.
       P1-1. REWRITE CHARFILE01. GO TO P1.
       P2.
           CLOSE CHARFILE.
           STOP RUN.
