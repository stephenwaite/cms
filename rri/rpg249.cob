      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rpg249.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT RPGCHARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS SEQUENTIAL RECORD KEY IS RPGCHARFILE-KEY
             LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION LINE
           SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  FILEOUT.
       01 FILEOUT01 PIC X(32).
       FD  RPGCHARFILE
           DATA RECORD IS RPGCHARFILE01.
       01  RPGCHARFILE01.
           02 RPGCHARFILE-KEY.
             03 RPG-KEY8 PIC X(32).
             03 RPG-KEY3 PIC XXX.
           02 RPG-PATID PIC X(8).
           02 RPG-CLAIM PIC X(6).
           02 RPG-SERVICE PIC X.
           02 RPG-DIAG PIC X(7).
           02 RPG-PROC. 
              03 RPG-PROC1 PIC X(4).
              03 RPG-PROC2 PIC X(7).
           02 RPG-MOD2 PIC XX.
           02 RPG-MOD3 PIC XX.
           02 RPG-MOD4 PIC XX.
           02 RPG-AMOUNT PIC X(6).
           02 RPG-DOCR PIC X(3).
           02 RPG-DOCP PIC X(2).
           02 RPG-PAYCODE PIC XXX.
           02 RPG-STAT PIC X.
           02 RPG-WORK PIC XX.
           02 RPG-DAT1 PIC X(8).
           02 RPG-RESULT PIC X.
           02 RPG-ACT PIC X.
           02 RPG-SORCREF PIC X.
           02 RPG-COLLECT PIC X.
           02 RPG-AUTH PIC X.
           02 RPG-PAPER PIC X.
           02 RPG-PLACE PIC X.
           02 RPG-NAME PIC X(24).
           02 RPG-EPSDT PIC X.
           02 RPG-DATE-T PIC X(8).
           02 RPG-DATE-E PIC X(8).
           02 RPG-ORDER PIC X(6).
           02 RPG-DX2 PIC X(7).
           02 RPG-DX3 PIC X(7).
           02 RPG-DATE-A PIC X(8).
           02 RPG-ACC-TYPE PIC X.
           02 RPG-DATE-M PIC X(8).
           02 RPG-ASSIGN PIC X.
           02 RPG-NEIC-ASSIGN PIC X.
           02 RPG-DX4 PIC X(7).
           02 RPG-DX5 PIC X(7).
           02 RPG-DX6 PIC X(7).
           02 RPG-FUTURE PIC X(6).

       WORKING-STORAGE SECTION.
       01 HOLD-CHAR PIC X(32) VALUE SPACE.
       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT RPGCHARFILE OUTPUT FILEOUT.
       P1.
           READ RPGCHARFILE AT END GO TO P2.
          
           IF RPG-KEY8 = HOLD-CHAR GO TO P1.
          
           WRITE FILEOUT01 FROM RPG-KEY8
           MOVE RPG-KEY8 TO HOLD-CHAR
          
           GO TO P1.

       P2. CLOSE RPGCHARFILE FILEOUT.
           STOP RUN.
