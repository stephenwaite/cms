      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrr253.
       AUTHOR. S WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARNEW ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARNEW-KEY.
           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD FILEOUT.
       01 FILEOUT01 PIC X(40).
       FD  CHARNEW
           BLOCK CONTAINS 4 RECORDS
           DATA RECORD IS CHARNEW01.
       01  CHARNEW01.
           02 CHARNEW-KEY.
             03 CD-KEY8 PIC X(8).
             03 CD-KEY3 PIC XXX.
           02 CD-PATID PIC X(8).
           02 CD-CLAIM PIC X(6).
           02 CD-SERVICE PIC X.
           02 CD-DIAG PIC X(7).
           02 CD-PROC.
             03 CD-PROC1 PIC X(4).
             03 CD-PROC2 PIC X(7).
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
           02 CD-DATE-T PIC 9(8).
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
           02 CD-QP1 PIC XX.
           02 CD-QP2 PIC XX.
           02 CD-DX5-3 PIC X(3).
           02 CD-DX6 PIC X(7).
           02 CD-CLINICAL PIC X(40).
           02 CD-ADMIT-DIAG PIC X(30).
       WORKING-STORAGE SECTION.
       01 CHARBACK PIC X(11).
       01 DATE-TAB01.
           02 DATE-TAB PIC 9(8) OCCURS 90 TIMES.
           02 PROC-TAB PIC X(11) OCCURS 90 TIMES.
           02 CHARGE-TAB PIC X(7) OCCURS 90 TIMES.
           02 MOD-TAB PIC X(2) OCCURS 90 TIMES.
           02 KEY-TAB PIC X(11) OCCURS 90 TIMES.
       01  X PIC 99.
       01  Y PIC 99.
       01 Z PIC 99.
       01 T PIC 99.
       01 A PIC 99.
       01 FLAGX PIC 9 VALUE 0.
       01 HOLDIT PIC X(8).
      *
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT CHARNEW OUTPUT FILEOUT.
           MOVE SPACE TO CHARNEW-KEY.

       P0-1. 
           START CHARNEW KEY NOT < CHARNEW-KEY
             INVALID
               GO TO P4
           END-START.    

       P1. 
           READ CHARNEW NEXT
             AT END
               GO TO P4
           END-READ.    
           
       P1-1.
           MOVE CD-KEY8 TO HOLDIT
           MOVE 1 TO X
           MOVE CHARNEW-KEY TO KEY-TAB(X)
           MOVE CD-DATE-T    TO DATE-TAB(X)
           MOVE CD-PROC2     TO CHARGE-TAB(X)
           MOVE CD-PROC      TO PROC-TAB(X).

       P2. 
           READ CHARNEW NEXT
             AT END
               GO TO P4
           END-READ

           IF CD-KEY8 NOT = HOLDIT
               GO TO P14
           END-IF

           ADD 1 TO X
           MOVE CD-DATE-T    TO DATE-TAB(X)
           MOVE CHARNEW-KEY  TO KEY-TAB(X)
           MOVE CD-PROC2     TO CHARGE-TAB(X)
           MOVE CD-PROC      TO PROC-TAB(X)
           MOVE CD-MOD2      TO MOD-TAB(X)
           GO TO P2.

       P14.
           IF X < 2
               GO TO P1-1
           END-IF

           MOVE CHARNEW-KEY TO CHARBACK.
           PERFORM P15
           MOVE CHARBACK TO CHARNEW-KEY
           GO TO P0-1.
       P15.
           SUBTRACT 1 FROM X GIVING Y.
           PERFORM C1 THRU C1-EXIT VARYING Z FROM 1 BY 1 UNTIL Z > Y.

       C1. 
           IF DATE-TAB(Z) = 0
               GO TO C1-EXIT
           END-IF

           MOVE 0 TO FLAGX.
           ADD 1 TO Z GIVING A 
           PERFORM C2 THRU C2-EXIT VARYING T FROM A BY 1 UNTIL T > X.

       C1-EXIT.
           EXIT.

       C2. 
           IF (DATE-TAB(Z) NOT = DATE-TAB(T)) 
               OR (CHARGE-TAB(Z) NOT =  CHARGE-TAB(T))
               OR (MOD-TAB(Z) NOT = MOD-TAB(T)) 
               OR (DATE-TAB(T) = 0)
               GO TO C2-EXIT
           END-IF    
           STRING KEY-TAB(Z)" -- " PROC-TAB(Z) DELIMITED BY SIZE
               INTO FILEOUT01
           WRITE FILEOUT01
           STRING KEY-TAB(T)" -- " PROC-TAB(T) DELIMITED BY SIZE
               INTO FILEOUT01
           WRITE FILEOUT01
           MOVE 0 TO DATE-TAB(T).
       C2-EXIT. 
           EXIT.
       P4. 
           PERFORM P15.
           CLOSE CHARNEW FILEOUT. 
           STOP RUN.
