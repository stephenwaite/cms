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

       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).

       FD  CHARNEW.
           COPY charnew.CPY IN "C:\Users\sid\cms\copylib\rri".           
      
       WORKING-STORAGE SECTION.

       01  CHARBACK PIC X(11).

       01  DATE-TAB01.
           02 KEY-TAB    PIC X(11)   OCCURS 90 TIMES.
           02 PROC-TAB   PIC X(11)   OCCURS 90 TIMES.
           02 CHARGE-TAB PIC  X(7)   OCCURS 90 TIMES.
           02 MOD-TAB    PIC  X(2)   OCCURS 90 TIMES.
           02 DATE-TAB   PIC  9(8)   OCCURS 90 TIMES.

       01  X PIC 99.
       01  Y PIC 99.
       01  Z PIC 99.
       01  T PIC 99.
       01  A PIC 99.
       01  FLAGX PIC 9 VALUE 0.
       01  HOLDIT PIC X(8).

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
           MOVE CHARNEW-KEY  TO KEY-TAB(X)
           MOVE CD-PROC      TO PROC-TAB(X).
           MOVE CD-PROC2     TO CHARGE-TAB(X)
           MOVE CD-MOD2      TO MOD-TAB(X)
           MOVE CD-DATE-T    TO DATE-TAB(X).

       P2. 
           READ CHARNEW NEXT
             AT END
               GO TO P4
           END-READ

           IF CD-KEY8 NOT = HOLDIT
               GO TO P14
           END-IF

           ADD 1 TO X
           MOVE CHARNEW-KEY  TO KEY-TAB(X)
           MOVE CD-PROC      TO PROC-TAB(X)
           MOVE CD-PROC2     TO CHARGE-TAB(X)
           MOVE CD-MOD2      TO MOD-TAB(X)
           MOVE CD-DATE-T    TO DATE-TAB(X)
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

           IF proc-tab(z)(1:4) = "0000" OR "0001"
             GO TO C1-EXIT
           end-if  

           MOVE 0 TO FLAGX.
           ADD 1 TO Z GIVING A 
           PERFORM C2 THRU C2-EXIT VARYING T FROM A BY 1 UNTIL T > X.

       C1-EXIT.
           EXIT.

       C2. 
      *     display key-tab(z) " " mod-tab(z)
      *     display key-tab(t) " " mod-tab(t)
      *     accept omitted

           IF (DATE-TAB(Z) NOT = DATE-TAB(T)) 
             OR (CHARGE-TAB(Z) NOT = CHARGE-TAB(T))
             OR (MOD-TAB(Z) NOT = MOD-TAB(T)) 
             OR (DATE-TAB(T) = 0)
             OR (charge-tab(T)(1:5) = "G1004")
             OR (CHARGE-TAB(Z)(1:5) = "G1004")  

             IF (DATE-TAB(Z) = DATE-TAB(T)) 
               AND (CHARGE-TAB(Z) = CHARGE-TAB(T))
               AND (MOD-TAB(Z) NOT = MOD-TAB(T)) 
               STRING "MOD IS DIFF. MANUALLY ADD 76/77 MOD DUE TO AUC? "
                 KEY-TAB(T) delimited by size INTO FILEOUT01
               write FILEOUT01  
               MOVE SPACE TO FILEOUT01               
             END-IF  

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
