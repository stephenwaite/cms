      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rtltd.
       AUTHOR. S WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC  RECORD KEY IS CHARFILE-KEY
               LOCK MODE MANUAL.

           SELECT FILEIN ASSIGN TO   "S35" ORGANIZATION
               LINE SEQUENTIAL.

           SELECT PROCFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS PROC-KEY.

           SELECT FILEOUT ASSIGN TO  "S45" ORGANIZATION IS LINE 
               SEQUENTIAL.    
       DATA DIVISION.
       FILE SECTION.
       FD  CHARFILE
      *    BLOCK CONTAINS 2 RECORDS
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
           02 CD-ACD-TYPE PIC X.
           02 CD-DATE-M PIC X(8).
           02 CD-ASSIGN PIC X.
           02 CD-NEIC-ASSIGN PIC X.
           02 CD-DX4 PIC X(7).
           02 CD-DX5 PIC X(7).
           02 CD-DX6 PIC X(7).
           02 CD-FUTURE PIC X(6).

       FD PROCFILE
           DATA RECORD PROCFILE01.
       01 PROCFILE01.
           02 PROC-KEY PIC X(11).
           02 PROC-TYPE PIC X.
           02 PROC-TITLE PIC X(28). 
           02 PROC-AMOUNT PIC 9(4)V99.
       FD FILEIN.
       01  FILEIN01.
           02 FI-CHARFILE-KEY.
             03 FI-KEY8 PIC X(8).
             03 FI-KEY3 PIC XXX.
           02 FI-PATID PIC X(8).
           02 FI-CLAIM PIC X(6).
           02 FI-SERVICE PIC X.
           02 FI-DIAG PIC X(7).
           02 FI-PROC. 
              03 FI-PROC0 PIC X(4).
              03 FI-PROC1 PIC X(7).
           02 FI-MOD2 PIC XX.
           02 FI-MOD3 PIC XX.
           02 FI-MOD4 PIC XX.
           02 FI-AMOUNT PIC S9(4)V99.
           02 FI-DOCR PIC X(3).
           02 FI-DOCP PIC X(2).
           02 FI-PAYCODE PIC XXX.
           02 FI-STAT PIC X.
           02 FI-WORK PIC XX.
           02 FI-DAT1 PIC X(8).
           02 FI-RESULT PIC X.
           02 FI-ACT PIC X.
           02 FI-SORCREF PIC X.
           02 FI-COLLT PIC X.
           02 FI-AUTH PIC X.
           02 FI-PAPER PIC X.
           02 FI-PLACE PIC X.
           02 FI-NAME PIC X(24).
           02 FI-ESPDT PIC X.
           02 FI-DATE-T PIC X(8).
           02 FI-DATE-E PIC X(8).
           02 FI-ORDER PIC X(6).
           02 FI-DX2 PIC X(7).
           02 FI-DX3 PIC X(7).
           02 FI-DATE-A PIC X(8).
           02 FI-ACD-TYPE PIC X.
           02 FI-DATE-M PIC X(8).
           02 FI-ASSIGN PIC X.
           02 FI-NEIC-ASSIGN PIC X.
           02 FI-DX4 PIC X(7).
           02 FI-DX5 PIC X(7).
           02 FI-DX6 PIC X(7).
           02 FI-FUTURE PIC X(6).
       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).    
       WORKING-STORAGE SECTION.
       01  ALF28 PIC X(28).
       01  ALF28X PIC X(28).
       01  MOD PIC XX.
       01  TALLYX PIC 9.
       01  TALLYRT PIC 9.
       01  TALLYlT PIC 9.
       01  TALLYRIT PIC 9.
       01  TALLYLIT PIC 9.
       
       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT PROCFILE FILEIN.
           OPEN I-O CHARFILE.
           OPEN OUTPUT FILEOUT.
       P1.
           READ FILEIN AT END
               GO TO P99
           END-READ

           MOVE FI-PROC TO PROC-KEY
           READ PROCFILE INVALID
               STRING "BAD PROC " FI-PROC 
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P1
           END-READ

           MOVE SPACE TO ALF28 ALF28X
           UNSTRING PROC-TITLE DELIMITED BY " LTD"
               INTO ALF28 ALF28X.
           
           IF ALF28X NOT = SPACE 
               GO TO P1
           END-IF

           MOVE SPACE TO MOD
           IF PROC-TITLE(24:5) = " LEFT"
               MOVE "LT" TO MOD 
               GO TO P2
           END-IF
           
           IF PROC-TITLE(1:5) = "LEFT "
               MOVE "LT" TO MOD
               GO TO P2
           END-IF

           IF PROC-TITLE(23:6) = " RIGHT"
               MOVE "RT" TO MOD
               GO TO P2
           END-IF

           IF PROC-TITLE(1:6) = "RIGHT "
               MOVE "RT" TO MOD
               GO TO P2
           END-IF
               
           MOVE 0 TO TALLYRIT TALLYLIT TALLYRT TALLYLT
           INSPECT PROC-TITLE TALLYING TALLYRIT FOR ALL " RIGHT "
           INSPECT PROC-TITLE TALLYING TALLYLIT FOR ALL " LEFT "
           INSPECT PROC-TITLE TALLYING TALLYRT  FOR ALL " RT "
           INSPECT PROC-TITLE TALLYING TALLYLT  FOR ALL " LT "

           COMPUTE TALLYX = TALLYRIT + TALLYLIT + TALLYRT + TALLYLT    

           IF TALLYX = 0
               GO TO P1 
           END-IF

           IF TALLYRIT > 0 OR TALLYRT > 0
               MOVE "RT" TO MOD
           ELSE
               MOVE "LT" TO MOD
           END-IF.
       P2.
           MOVE FI-CHARFILE-KEY TO CHARFILE-KEY
           READ CHARFILE WITH LOCK INVALID
               STRING "INVALID KEY " FI-CHARFILE-KEY 
               DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P1
           END-READ
           
           IF CD-MOD2 = "76" AND CD-MOD3 = "76"
               MOVE MOD TO CD-MOD2
               GO TO P3
           END-IF    
       
           IF CD-MOD2 = SPACE
               MOVE MOD TO CD-MOD2
               GO TO P3
           END-IF

           IF CD-MOD2 = "76" AND CD-MOD3 = SPACE
               MOVE "76" TO CD-MOD3
               MOVE MOD TO CD-MOD2
               GO TO P3
           END-IF

           IF CD-MOD3 = SPACE
               MOVE MOD TO CD-MOD3
               GO TO P3
           END-IF

           IF CD-MOD4 = SPACE
               MOVE MOD TO CD-MOD4
               GO TO P3
           END-IF.    
       P3. 
           IF CD-MOD2 = CD-MOD3
               MOVE SPACE TO CD-MOD3
           END-IF

           IF CD-MOD2 = CD-MOD4
               MOVE SPACE TO CD-MOD4
           END-IF
               
           IF CD-MOD3 = CD-MOD4
               MOVE SPACE TO CD-MOD4
           END-IF

           STRING "MOD " MOD " PROC " CD-PROC " MOD2 " CD-MOD2
                  " MOD3 " CD-MOD3 " MOD4 " CD-MOD4
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01

           REWRITE CHARFILE01.
           GO TO P1.
       P4.
           IF (FI-PROC0 = "1118" OR "1119" OR "1149" OR "1150" OR
                          "1151" OR "1152" OR "1153" OR "1154" OR 
                          "1188" OR "1192")
               GO TO P5
           END-IF.    
       P5.
           MOVE SPACE TO MOD
           
           IF FI-PROC0 = "1188"
               MOVE "FA" TO MOD
           END-IF    
           
           IF FI-PROC0 = "1192"
               MOVE "F1" TO MOD
           END-IF    
           
           IF FI-PROC0 = "1149"
               MOVE "F2" TO MOD
           END-IF

           IF FI-PROC0 = "1151"
               MOVE "F3" TO MOD
           END-IF

           IF FI-PROC0 = "1153"
               MOVE "F4" TO MOD
           END-IF

           IF FI-PROC0 = "1118"
               MOVE "F5" TO MOD
           END-IF

           IF FI-PROC0 = "1119"
               MOVE "F6" TO MOD
           END-IF

           IF FI-PROC0 = "1150"
               MOVE "F7" TO MOD
           END-IF

           IF FI-PROC0 = "1152"
               MOVE "F8" TO MOD
           END-IF

           IF FI-PROC0 = "1154"
               MOVE "F9" TO MOD
           END-IF

           GO TO P2.
       P99.
           CLOSE CHARFILE FILEIN PROCFILE FILEOUT. 
           STOP RUN.
