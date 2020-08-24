      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ster038.
       AUTHOR. SWAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC    RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.

          

       DATA DIVISION.
       FILE SECTION.
      
       FD FILEOUT.
       01  FILEOUT01 PIC X(160).
       FD  CHARCUR
           BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS CHARCUR01.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC.
              03 CC-PROC0 PIC X(4).
              03 CC-PROC1 PIC X(5).
              03 CC-PROC2 PIC XX.
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
           02 CC-IOPAT PIC X.
           02 CC-DATE-T PIC X(8).
           02 CC-DATE-A PIC X(8).
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
       01  FLAG PIC X.

       PROCEDURE DIVISION.
       0005-START.
           OPEN I-O CHARCUR
      *     OPEN INPUT CHARCUR 
           OPEN OUTPUT FILEOUT.
       P1. 
           MOVE 0 TO FLAG
           READ CHARCUR NEXT
             AT END
               GO TO P6
           END-READ

           IF CC-DX2 = SPACE
               MOVE 1 TO FLAG
               MOVE SPACE TO FILEOUT01
               STRING CHARCUR-KEY " DX-2 " CC-DX2 DELIMITED BY SIZE
                   INTO FILEOUT01
               WRITE FILEOUT01    
               MOVE "0000000" TO CC-DX2
           END-IF

           IF CC-DX3 = SPACE
               MOVE 1 TO FLAG 
               MOVE SPACE TO FILEOUT01
               STRING CHARCUR-KEY " DX-3 " CC-DX3 DELIMITED BY SIZE
                   INTO FILEOUT01
               WRITE FILEOUT01    
               MOVE "0000000" TO CC-DX3
           END-IF

           IF CC-DX4 = SPACE
               MOVE 1 TO FLAG
               MOVE SPACE TO FILEOUT01
               STRING CHARCUR-KEY " DX-4 " CC-DX4 DELIMITED BY SIZE
                   INTO FILEOUT01
               WRITE FILEOUT01
               MOVE "0000000" TO CC-DX4
           END-IF

           IF FLAG = 1
               REWRITE CHARCUR01
           END-IF
                          
           GO TO P1.
           
       P6. 
           CLOSE FILEOUT CHARCUR.
           STOP RUN.
