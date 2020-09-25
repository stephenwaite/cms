      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrr70030.
       AUTHOR. S WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARFILE-KEY.
           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD FILEOUT.
       01 FILEOUT01 PIC X(80).
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
              03 CD-PROC5 pic x(5).
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
       WORKING-STORAGE SECTION.
       01 CHARBACK PIC X(11).
       01  SAVE-KEY01.
           02 SAVE-KEY PIC X(11) OCCURS 2 TIMES.
       01  X PIC 99.
       01 HOLDIT PIC X(8).
      *
       PROCEDURE DIVISION.
       P0.
           OPEN I-O CHARFILE
           OUTPUT FILEOUT.
           MOVE SPACE TO CHARFILE-KEY.
       P0-1. 
           MOVE 0 TO X.
           
           START CHARFILE KEY NOT < CHARFILE-KEY INVALID
               GO TO P99
           END-START

           READ CHARFILE NEXT AT END
               GO TO P99
           END-READ

           MOVE CD-KEY8 TO HOLDIT
           GO TO P1-1.
       P1. 
           READ CHARFILE NEXT AT END
               GO TO P99
           END-READ

           IF CD-KEY8 NOT = HOLDIT
               GO TO P2
           END-IF.     
       P1-1.        
           IF CD-PROC0 NOT = "1204"
               GO TO P1
           END-IF
          
           ADD 1 TO X
         
           MOVE CHARFILE-KEY TO SAVE-KEY(X)
           GO TO P1.
       P2.
           IF X NOT = 2
               MOVE 0 TO X
               MOVE CD-KEY8 TO HOLDIT
               GO TO P1-1
           END-IF.    
          
           MOVE SAVE-KEY(1) TO CHARFILE-KEY
           READ CHARFILE WITH LOCK INVALID
               DISPLAY "WEIRD"           
           END-READ

           IF CD-MOD2 = "59"
               MOVE "RT" TO CD-MOD2
               MOVE "59" TO CD-MOD3
           ELSE
               MOVE "RT" TO CD-MOD2
           END-IF

           REWRITE CHARFILE01.        

           STRING "ADDED RT MOD TO " CD-PROC " FOR " CD-KEY8 " DOS "
               CD-DATE-T DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01    


           MOVE SAVE-KEY(2) TO CHARFILE-KEY
           READ CHARFILE WITH LOCK INVALID
               DISPLAY "WEIRD"           
           END-READ

           IF CD-MOD2 = "59"
               MOVE "LT" TO CD-MOD2
               MOVE "59" TO CD-MOD3
           ELSE
               MOVE "LT" TO CD-MOD2
           END-IF

           REWRITE CHARFILE01.

           STRING "ADDED LT MOD TO " CD-PROC " FOR " CD-KEY8 " DOS "
               CD-DATE-T DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
                    
      *  SINCE 999 IS THE LARGEST CD-KY3 POSSIBLE 
      *  THE NEXT RECORD AFTER THE START IN PO-1 HAS TO
      *  BE THE VERY NEXT ACCOUNT
           MOVE "999" TO CD-KEY3
           GO TO P0-1.
        P99.
           CLOSE CHARFILE FILEOUT. 
           STOP RUN.
