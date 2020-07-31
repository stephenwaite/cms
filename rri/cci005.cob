      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cci005.
       AUTHOR. s WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARCUR ASSIGN TO  "S25" ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC RECORD KEY IS CHARCUR-KEY
               ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
               LOCK MODE MANUAL.

           SELECT CHARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS CHARFILE-KEY
               LOCK MODE MANUAL.

           SELECT CCIFILE ASSIGN TO  "S35" ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC RECORD KEY IS CCI-KEY
               LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO  "S40" ORGANIZATION
               LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  CHARCUR
      *    BLOCK CONTAINS 3 RECORDS
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
           02 CC-AUTH PIC X.
           02 CC-PAPER PIC X.
           02 CC-PLACE PIC X.
           02 CC-EPSDT PIC X.
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
       FD  CCIFILE.
       01  CCIFILE01.
           02 CCI-KEY.
             03 CCI-KEY1 PIC X(5).
             03 CCI-KEY2 PIC X(5).
           02 CCI-IND PIC X.
       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).
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
           02 CD-PROC. 
               03 CD-PROC0 PIC X(4).
               03 CD-PROC1 PIC X(5).
               03 CD-PROC3 PIC XX.
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
       01  DATE-TAB01.
           02 DATE-TAB PIC 9(8) OCCURS 90 TIMES.
           02 KEY-TAB PIC X(11) OCCURS 90 TIMES.
           02 PROC-TAB PIC X(5) OCCURS 90 TIMES.
           02 DOC-TAB PIC   XX  OCCURS 90 TIMES.
           02 MOD2-TAB PIC  XX  OCCURS 90 TIMES.
           02 MOD3-TAB PIC  XX  OCCURS 90 TIMES.
       01  X PIC 99.
       01  Y PIC 99.
       01  Z PIC 99.
       01  T PIC 99.
       01  A PIC 99.
       01  B PIC 99.
       01  C PIC 99.
       01  FLAG PIC 9.
       01  FLAGX PIC 9 VALUE 0.
       01  IND-X PIC X.
       01  HOLD-IT PIC X(8).
       01  ALF1 PIC X.
       01  ALF2 PIC XX.
       01  ALF5 PIC X(5).
       01  ALF8 PIC X(8).
       01  ALF11 PIC X(11).
      *
       PROCEDURE DIVISION.
       P0.
           OPEN I-O CHARFILE 
           OPEN INPUT CCIFILE CHARCUR.
           OPEN OUTPUT FILEOUT
           MOVE SPACE TO CHARFILE-KEY.
       P0-1.
           START CHARFILE KEY NOT < CHARFILE-KEY INVALID
               GO TO P4
           END-START.    
       P1.
           READ CHARFILE NEXT AT END
               GO TO P4
           END-READ.
       P1-1.
           MOVE CD-KEY8 TO HOLD-IT
           MOVE 1 TO X
           MOVE CHARFILE-KEY TO KEY-TAB(X)
           MOVE CD-PROC1     TO PROC-TAB(X)
           MOVE CD-DOCP      TO DOC-TAB(X)
           MOVE CD-MOD2      TO MOD2-TAB(X)
           MOVE CD-MOD3      TO MOD3-TAB(X)
           MOVE CD-DATE-T    TO DATE-TAB(X).
       P2.
           READ CHARFILE NEXT AT END
               PERFORM CHARCUR-CHECK THRU CHARCUR-CHECK-EXIT
               GO TO P4
           END-READ

           IF CD-AMOUNT = 0
               GO TO P2
           END-IF

           IF CD-KEY8 NOT = HOLD-IT
               PERFORM CHARCUR-CHECK THRU CHARCUR-CHECK-EXIT
               GO TO P14
           END-IF

           DISPLAY CHARFILE-KEY
           ACCEPT OMITTED
           
           ADD 1 TO X
           MOVE CHARFILE-KEY TO KEY-TAB(X)
           MOVE CD-PROC1 TO PROC-TAB(X)
           MOVE CD-DOCP TO DOC-TAB(X)
           MOVE CD-MOD2 TO MOD2-TAB(X)
           MOVE CD-MOD3 TO MOD3-TAB(X)
           MOVE CD-DATE-T TO DATE-TAB(X)
           GO TO P2.
       CHARCUR-CHECK.
           MOVE HOLD-IT TO CC-KEY8
           MOVE SPACE   TO CC-KEY3
           START CHARCUR KEY NOT < CHARCUR-KEY INVALID
               GO TO CHARCUR-CHECK-EXIT
           END-START.
       CHARCUR-CHECK-1.    
           READ CHARCUR NEXT AT END
               GO TO CHARCUR-CHECK-EXIT
           END-READ

           IF CC-KEY8 NOT = HOLD-IT
               GO TO CHARCUR-CHECK-EXIT
           END-IF

           IF CC-DATE-T NOT = DATE-TAB(1)
               GO TO CHARCUR-CHECK-1
           END-IF

           ADD 1 TO X
           MOVE SPACE TO KEY-TAB(X)
           MOVE CC-PROC1  TO PROC-TAB(X)
           MOVE CC-DOCP   TO DOC-TAB(X)
           MOVE CC-MOD2   TO MOD2-TAB(X)
           MOVE CC-MOD3   TO MOD3-TAB(X)
           MOVE CC-DATE-T TO DATE-TAB(X)
           GO TO CHARCUR-CHECK-1.
       CHARCUR-CHECK-EXIT.
           EXIT.        
       P14.
           MOVE CD-KEY8 TO HOLD-IT
           IF X < 2
               MOVE SPACE TO CD-KEY3
               GO TO P0-1
           END-IF

           COMPUTE Y = X - 1
      *    figure out 76/77 mods     
           PERFORM P15 THRU P17 VARYING Z FROM 1 BY 1 UNTIL Z > Y.
      *    determine 59 mod     
           PERFORM P18 THRU P20 VARYING Z FROM 1 BY 1 UNTIL Z > Y.
           MOVE HOLD-IT TO CD-KEY8
           MOVE SPACE   TO CD-KEY3
           GO TO P0-1.
       P15.
           IF KEY-TAB(Z) = SPACE                  
               GO TO P17
           END-IF    
           COMPUTE B = Z + 1
           PERFORM P16 THRU P16-EXIT VARYING A FROM B BY 1 UNTIL A > X
           GO TO P17.
       P16.           
           IF (DATE-TAB(A) = DATE-TAB(Z))
               AND (PROC-TAB(A) = PROC-TAB(Z)) 
               AND (MOD2-TAB(A) = MOD2-TAB(Z))                  

               IF KEY-TAB(A) = SPACE
                   STRING "MOD 76/77 NEEDED DUE TO POSTED CHARGE, DOS " 
                   DATE-TAB(A) ", CPT " PROC-TAB(A) 
                   ", MOD2 " MOD2-TAB(A) " FOR " KEY-TAB(Z)(1:8)
                   DELIMITED BY SIZE INTO FILEOUT01
                   WRITE FILEOUT01
                   MOVE KEY-TAB(Z) TO CHARFILE-KEY
               ELSE
                   MOVE KEY-TAB(A) TO CHARFILE-KEY
               END-IF    

               READ CHARFILE WITH LOCK INVALID
                   DISPLAY CHARFILE-KEY " LOCKED"
                   GO TO P16-EXIT
               END-READ   

               IF CD-MOD2 = SPACE
                   MOVE "76" TO CD-MOD2
                   IF DOC-TAB(A) NOT = DOC-TAB(Z)
                       MOVE "77" TO CD-MOD2
                   END-IF 
                   GO TO P16-1   
               END-IF 

               IF (CD-MOD2 NOT = "76" OR NOT = "77")
                   MOVE "76" TO CD-MOD3
                   IF DOC-TAB(A) NOT = DOC-TAB(Z)
                       MOVE "77" TO CD-MOD3
                   END-IF
                   GO TO P16-1
               END-IF                   
           END-IF
           GO TO P16-EXIT.        
       P16-1.
           STRING KEY-TAB(A) " " KEY-TAB(Z) " "
               "ADDING MODS FOR " CD-NAME " DOS " CD-DATE-T " MOD2 "
               CD-MOD2 " MOD3 " CD-MOD3 
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01               
           REWRITE CHARFILE01.
       P16-EXIT.
           EXIT.
       P17.
           EXIT.
       P18.
           IF KEY-TAB(Z) = SPACE                  
      *   THE FOLLOWING GO TO WAS GO TO P17 NOT GO TO P20
               GO TO P20
           END-IF 
           COMPUTE B = Z + 1
           PERFORM P19 THRU P19-EXIT VARYING A FROM B BY 1 UNTIL A > X
           GO TO P20.
       P19.
           IF (DATE-TAB(A) NOT = DATE-TAB(Z)) 
               OR (PROC-TAB(A) = PROC-TAB(Z))
               GO TO P19-EXIT
           END-IF

           MOVE 0 TO FLAG
           PERFORM CCI-1 THRU CCI-3
           
           IF FLAG = 0
               GO TO P19-EXIT
           END-IF

           IF IND-X = "0"
               MOVE SPACE TO FILEOUT01
               STRING KEY-TAB(A) " " KEY-TAB(Z) " "
                   "CAN NOT BILL THESE 2 TOGETHER PER NCCI."
                   DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01        
               GO TO P19-EXIT
           END-IF.
       P19-1.
           IF FLAG = 2
               MOVE KEY-TAB(Z) TO CHARFILE-KEY
           ELSE
               IF KEY-TAB(A) = SPACE
                   STRING "MOD 59 MISSED, DOS " DATE-TAB(A)
                   ", CPT " PROC-TAB(A) ", MOD2 " MOD2-TAB(A)
                   " FOR " KEY-TAB(Z)(1:8)
                   DELIMITED BY SIZE INTO FILEOUT01
                   WRITE FILEOUT01
                   GO TO P19-EXIT
               ELSE
                   MOVE KEY-TAB(A) TO CHARFILE-KEY
               END-IF    
           END-IF

           IF KEY-TAB(A) = SPACE
               DISPLAY "KEY TAB A IS SPACE " KEY-TAB(A)
               ACCEPT OMITTED
           END-IF    
           
           READ CHARFILE WITH LOCK INVALID 
               GO TO P19-EXIT
           END-READ

           MOVE SPACE TO FILEOUT01
           STRING KEY-TAB(A) " " KEY-TAB(Z) " "
               "ADDING MOD 59 FOR " CD-NAME " DOS " CD-DATE-T
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01  

           IF CD-MOD2 = SPACE
               MOVE "59" TO CD-MOD2
           END-IF
           
           IF (CD-MOD2 NOT = SPACE)
               AND (CD-MOD2 NOT = "59")
               MOVE "59" TO CD-MOD3
           END-IF

           REWRITE CHARFILE01.
       P19-EXIT.
           EXIT.
       P20.
           EXIT.
       CCI-1.
           MOVE PROC-TAB(Z) TO CCI-KEY1
           MOVE PROC-TAB(A) TO CCI-KEY2
           READ CCIFILE INVALID
               GO TO CCI-2
           END-READ
           MOVE CCI-IND TO IND-X
           MOVE 1 TO FLAG
           GO TO CCI-3.
       CCI-2.
           MOVE PROC-TAB(A) TO CCI-KEY1
           MOVE PROC-TAB(Z) TO CCI-KEY2
           READ CCIFILE INVALID
               GO TO CCI-3
           END-READ
           MOVE CCI-IND TO IND-X
           MOVE 2 TO FLAG.
       CCI-3.
           EXIT.
       P4.
           IF X > 1
               COMPUTE Y = X - 1
               PERFORM P15 THRU P17 VARYING Z FROM 1 BY 1 UNTIL Z > Y
               PERFORM P18 THRU P20 VARYING Z FROM 1 BY 1 UNTIL Z > Y
           END-IF
           CLOSE CHARFILE FILEOUT CHARCUR. 
           STOP RUN.
