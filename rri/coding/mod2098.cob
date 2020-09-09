      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. mod2098.
       AUTHOR. S WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS CHARFILE-KEY
               LOCK MODE MANUAL.

           SELECT DIAGFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC RECORD KEY IS DIAG-KEY
               ALTERNATE RECORD KEY IS DIAG-TITLE WITH DUPLICATES.

           SELECT FILEOUT ASSIGN TO  "S40" ORGANIZATION
               LINE SEQUENTIAL.   

       DATA DIVISION.

       FILE SECTION.

       FD  CHARFILE.
       01  CHARFILE01.
           02 CHARFILE-KEY.
             03 CD-KEY8 PIC X(8).
             03 CD-KEY3 PIC XXX.
           02 CD-PATID PIC X(8).
           02 CD-CLAIM PIC X(6).
           02 CD-SERVICE PIC X.
           02 CD-DIAG PIC X(7).
           02 CD-PROC.
              03 CD-PROC0 PIC X(4).
              03 CD-PROC5 PIC X(5).
              03 CD-PROC2 PIC XX.
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
           02 CD-AUTH PIC X.
           02 CD-PAPER PIC X.
           02 CD-PLACE PIC X.
           02 CD-NAME PIC X(24).
           02 CD-ESPDT PIC X.
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

       FD  DIAGFILE
           DATA RECORD IS DIAG01.
       01  DIAG01.
           02 DIAG-KEY PIC X(7).
           02 DIAG-TITLE.
             03 DIAG-T1 PIC XXX.
             03 DIAG-T2 PIC X(58).
           02 DIAG-MEDB PIC X(5).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).

       WORKING-STORAGE SECTION.
       01  TALLYX PIC 9.
       01  TALLYRT PIC 9.
       01  TALLYlT PIC 9.
       01  TALLYRIT PIC 9.
       01  TALLYLIT PIC 9.
       01  TALLYR PIC 9.
       01  TALLYL PIC 9.

       01  CNTR PIC 9(7) VALUE 0.
       01  ANS PIC X.

       PROCEDURE DIVISION.

       0005-START.
           OPEN I-O CHARFILE.
           OPEN INPUT DIAGFILE.
           OPEN OUTPUT FILEOUT.

       P1.
           READ CHARFILE NEXT WITH LOCK
             AT END 
               GO TO P99
           END-READ

           IF CD-PROC0 NOT = "2098" 
               GO TO P1
           END-IF  

           IF CD-MOD2 NOT = SPACE 
               GO TO P2
           END-IF  
           
           MOVE "RT" TO CD-MOD2
           MOVE 0 TO TALLYRIT TALLYLIT TALLYRT TALLYLT TALLYR TALLYL
           MOVE CD-DIAG TO DIAG-KEY
           READ DIAGFILE
             INVALID
               CONTINUE
             NOT INVALID    
               INSPECT DIAG-TITLE TALLYING TALLYRIT FOR ALL " RIGHT "
               INSPECT DIAG-TITLE TALLYING TALLYLIT FOR ALL " LEFT "
               INSPECT DIAG-TITLE TALLYING TALLYRT  FOR ALL " RT "
               INSPECT DIAG-TITLE TALLYING TALLYLT  FOR ALL " LT "
               INSPECT DIAG-TITLE TALLYING TALLYR   FOR ALL " R "
               INSPECT DIAG-TITLE TALLYING TALLYL   FOR ALL " L "

               COMPUTE TALLYX = TALLYRIT + TALLYLIT + TALLYRT + TALLYLT
                                + TALLYR + TALLYL
              
               IF TALLYX NOT = 0
                   IF TALLYRIT > 0 OR TALLYRT > 0 OR TALLYR > 0
                       MOVE "RT" TO CD-MOD2
                   ELSE
                       MOVE "LT" TO CD-MOD2
                   END-IF
               END-IF
           END-READ
           
           REWRITE CHARFILE01
           
           STRING CD-PROC5 " " CD-MOD2 " " CD-MOD3 " " CD-MOD4 " " 
                  CD-NAME " " DIAG-TITLE
                  DELIMITED BY SIZE INTO FILEOUT01.
           WRITE FILEOUT01. 
           GO TO P1.

       P2.
           IF CD-MOD3 NOT = SPACE 
               GO TO P3
           END-IF       

           MOVE "RT" TO CD-MOD3
           MOVE 0 TO TALLYRIT TALLYLIT TALLYRT TALLYLT TALLYR TALLYL
           MOVE CD-DIAG TO DIAG-KEY
           READ DIAGFILE
             INVALID 
               CONTINUE
             NOT INVALID
               INSPECT DIAG-TITLE TALLYING TALLYRIT FOR ALL " RIGHT "
               INSPECT DIAG-TITLE TALLYING TALLYLIT FOR ALL " LEFT "
               INSPECT DIAG-TITLE TALLYING TALLYRT FOR ALL " RT "
               INSPECT DIAG-TITLE TALLYING TALLYLT FOR ALL " LT "
               INSPECT DIAG-TITLE TALLYING TALLYR FOR ALL " R "
               INSPECT DIAG-TITLE TALLYING TALLYL FOR ALL " L "

               COMPUTE TALLYX = TALLYRIT + TALLYLIT + TALLYRT + TALLYLT
                                + TALLYR + TALLYL
                
               IF TALLYX NOT = 0
                   IF TALLYRIT > 0 OR TALLYRT > 0 OR TALLYR > 0
                       MOVE "RT" TO CD-MOD3
                   ELSE
                       MOVE "LT" TO CD-MOD3
                   END-IF
               END-IF
           END-READ
           
           REWRITE CHARFILE01
           
           STRING CD-PROC5 " " CD-MOD2 " " CD-MOD3 " " CD-MOD4 " " 
                  CD-NAME " " DIAG-TITLE
                  DELIMITED BY SIZE INTO FILEOUT01.
           WRITE FILEOUT01.       
           GO TO P1.

       P3.           
           IF CD-MOD4 NOT = SPACE 
               DISPLAY "NO MORE MODS FOR CPT 76882"
               ACCEPT ANS 
               GO TO P1   
           END-IF       

           MOVE "RT" TO CD-MOD4
           MOVE 0 TO TALLYRIT TALLYLIT TALLYRT TALLYLT TALLYR TALLYL
           MOVE CD-DIAG TO DIAG-KEY
           READ DIAGFILE
             INVALID 
               CONTINUE
             NOT INVALID
               INSPECT DIAG-TITLE TALLYING TALLYRIT FOR ALL " RIGHT "
               INSPECT DIAG-TITLE TALLYING TALLYLIT FOR ALL " LEFT "
               INSPECT DIAG-TITLE TALLYING TALLYRT FOR ALL " RT "
               INSPECT DIAG-TITLE TALLYING TALLYLT FOR ALL " LT "
               INSPECT DIAG-TITLE TALLYING TALLYR FOR ALL " R "
               INSPECT DIAG-TITLE TALLYING TALLYL FOR ALL " L "

               COMPUTE TALLYX = TALLYRIT + TALLYLIT + TALLYRT + TALLYLT
                                + TALLYR + TALLYL
                
               IF TALLYX NOT = 0
                   IF TALLYRIT > 0 OR TALLYRT > 0 OR TALLYR > 0
                       MOVE "RT" TO CD-MOD4
                   ELSE
                       MOVE "LT" TO CD-MOD4
                   END-IF
               END-IF
           END-READ
           
           REWRITE CHARFILE01
           
           STRING CD-PROC5 " " CD-MOD2 " " CD-MOD3 " " CD-MOD4 " " 
                  CD-NAME " " DIAG-TITLE
                  DELIMITED BY SIZE INTO FILEOUT01.
           WRITE FILEOUT01.       
           GO TO P1.       

       
       P99.
           CLOSE CHARFILE DIAGFILE FILEOUT.
           STOP RUN.

