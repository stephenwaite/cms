      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rri222.
       AUTHOR. SWAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ACTFILE ASSIGN TO  "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC 
           RECORD KEY IS A-ACTNO
           ALTERNATE RECORD KEY IS A-GARNO WITH DUPLICATES
           ALTERNATE RECORD KEY IS NAME-KEY WITH DUPLICATES.

           SELECT FILE-OUT ASSIGN TO "S35" ORGANIZATION LINE
           SEQUENTIAL.

           SELECT ORDFILE ASSIGN TO  "S40" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC
           RECORD KEY IS ORDNO
           ALTERNATE RECORD KEY IS C-DATE-E WITH DUPLICATES.

       DATA DIVISION.

       FILE SECTION.

       FD  ORDFILE.
           copy "ordfile.cpy" in "c:\Users\sid\cms\copylib\rri".

       FD ACTFILE
           DATA RECORD IS ACTFILE01.
       01 ACTFILE01.
           02 A-ACTNO PIC X(8).
           02 A-GARNAME.
             03 A-GN1 PIC XXX.
             03 A-GN2 PIC X(21).
           02 A-BILLADD PIC X(22).
           02 A-STREET PIC X(22).
           02 A-CITY PIC X(18).
           02 A-STATE PIC X(2).
           02 A-ZIP PIC X(9).
           02 A-COLLT PIC X.
           02 A-PHONE.
             03 A-PHONE1 PIC XXX.
             03 A-PHONE2 PIC XXX.
             03 A-PHONE3 PIC X(4).
           02 A-SEX PIC X.
           02 A-RELATE PIC X.
           02 A-MSTAT PIC X.
           02 A-DOB PIC X(8).
           02 A-DUNNING PIC X.
           02 A-ACCTSTAT PIC X.
           02 A-PR-MPLR PIC X(4).
           02 A-PRINS PIC XXX.
           02 A-PR-ASSIGN PIC X.
           02 A-PR-OFFICE PIC X(4).
           02 A-PR-GROUP PIC X(10).
           02 A-PRIPOL.
             03 A-PRIPOL1 PIC X(9).
             03 A-PRIPOL2 PIC XXX.
             03 A-PR-FILLER PIC X(4).
           02 A-PRNAME PIC X(24).
           02 A-PR-RELATE PIC X.
           02 A-SE-MPLR PIC X(4).
           02 A-SEINS PIC XXX.
           02 A-SE-ASSIGN PIC X.
           02 A-SE-OFFICE PIC X(4).
           02 A-SE-GROUP PIC X(10).
           02 A-SECPOL.
             03 A-SECPOL1 PIC X(9).
             03 A-SECPOL2 PIC XXX.
             03 A-SE-FILLER PIC X(4).
           02 A-SENAME PIC X(24).
           02 A-SE-RELATE PIC X.
           02 A-INSPEND PIC X(7).
           02 A-LASTBILL PIC X(8).
           02 A-ASSIGNM PIC X.
           02 A-PRIVATE PIC X.
           02 A-BILLCYCLE PIC X.
           02 A-DELETE PIC X.
           02 A-FILLER PIC XXX.
           02 A-GARNO PIC X(8).
           02 A-PRGRPNAME PIC X(15).
           02 A-SEGRPNAME PIC X(15).
           02 NAME-KEY PIC XXX.

       FD FILE-OUT
           DATA RECORD IS FILE-OUT01.
       01  FILE-OUT01 PIC X(133).

       WORKING-STORAGE SECTION.

       01 ORD-TAB01.
           02 ORD-TAB02 OCCURS 300 TIMES.
             03 ORD PIC 9(4).
             03 CHARGE1 PIC 9(4).
             03 CHARGE2 PIC X.
             03 DATE-T PIC 9(8).
             03 ORD-KEY PIC 999.

       01  LINE-1.
           02 L1F0 PIC X(8).
           02 L1F1 PIC XXX.
           02 FILLER PIC X VALUE " ".
           02 L1F2 PIC XXXX.
           02 FILLER PIC X VALUE " ".
           02 L1F3.
             03 L1F31 PIC X(4).
             03 L1F32 PIC X.
           02 FILLER PIC X VALUE " ".
           02 L1F4 PIC X(8).
           02 FILLER PIC X VALUE " ".
           02 L1F5 PIC X(24).
           02 FILLER PIC X VALUE SPACE.
           02 L1F6 PIC X(24).

       01  A PIC 999.
       01  X PIC 999.
       01  T PIC 999.
       01  Y PIC 999.
       01 Z PIC 999.
       01  NUM4 PIC 9(4).
       01  FLAG PIC 9.

       PROCEDURE DIVISION.
       P0.
           OPEN INPUT ORDFILE ACTFILE
           OPEN OUTPUT FILE-OUT
           MOVE SPACE TO ORDNO
           START ORDFILE KEY > ORDNO
             INVALID
               GO TO P4
           END-START.

       P1. 
           READ ORDFILE NEXT
             AT END
               GO TO P4
           END-READ.

       P1-1.
           MOVE ORD8 TO A-ACTNO
           READ ACTFILE
             INVALID
               DISPLAY A-ACTNO " NOT ON FILE???"
               GO TO P1
           END-READ.

           MOVE 0 TO X
           GO TO P2-1.

       P2.
           READ ORDFILE NEXT
             AT END 
               GO TO P4
           END-READ    
           
           IF ORD8 NOT = A-ACTNO
               GO TO P3
           END-IF.    

       P2-1.
           ADD 1 TO X 
           MOVE C-ORDER TO ORD(X)
           MOVE C-CHARGE1 TO CHARGE1(X) 
           MOVE C-DATE-T TO DATE-T(X)
           MOVE C-CHARGE2 TO CHARGE2(X)
           MOVE ORD3 TO ORD-KEY(X)
           GO TO P2.
           
       P3.
           MOVE 0 TO FLAG
           PERFORM A1 THRU A1-EXIT 
               VARYING Y FROM 1 BY 1 UNTIL Y > X.
           
       P3-1.
           GO TO P1-1.

       A1.
           IF (CHARGE2(Y) = "-") AND (DATE-T(Y) NOT = 0)
               PERFORM A2 VARYING T FROM 1 BY 1 UNTIL T > X
           ELSE 
               GO TO A1-EXIT
           END-IF    
           
           IF FLAG = 0 
               PERFORM A3
           END-IF    
           
           IF FLAG = 1 
               PERFORM A4
               MOVE 0 TO FLAG
           END-IF.

       A1-EXIT.
           EXIT.

       A2.
           IF (CHARGE1(Y) = CHARGE1(T))
               AND (CHARGE2(T) = " ") AND (DATE-T(T) NOT = 0)
               AND (DATE-T(Y) NOT < DATE-T(T))
               MOVE 1 TO FLAG
               MOVE T TO Z
           END-IF.    

       A3.
           MOVE A-ACTNO TO L1F0
           MOVE "000" TO L1F1
           MOVE ORD(Y) TO L1F2
           MOVE CHARGE1(Y) TO L1F31
           MOVE CHARGE2(Y) TO L1F32
           MOVE DATE-T(Y) TO L1F4
           MOVE A-GARNAME TO L1F5
           MOVE "*** DANGLING CREDIT ***" TO L1F6
           WRITE FILE-OUT01 FROM LINE-1.

       A4.
           MOVE SPACE TO L1F6
           MOVE A-ACTNO TO L1F0 
           MOVE ORD-KEY(Z) TO L1F1
           MOVE ORD(Z) TO L1F2
           MOVE CHARGE1(Z) TO L1F31
           MOVE CHARGE2(Z) TO L1F32
           MOVE DATE-T(Z) TO L1F4
           MOVE A-GARNAME TO L1F5
           WRITE FILE-OUT01 FROM LINE-1
           
           MOVE A-ACTNO TO L1F0
           MOVE ORD-KEY(Y) TO L1F1
           MOVE ORD(Y) TO L1F2
           MOVE CHARGE1(Y) TO L1F31 
           MOVE CHARGE2(Y) TO L1F32
           MOVE DATE-T(Y) TO L1F4
           MOVE A-GARNAME TO L1F5
           WRITE FILE-OUT01 FROM LINE-1
           
           MOVE 0 TO DATE-T(Y) DATE-T(Z).

       P4.
           PERFORM P3
           CLOSE FILE-OUT ORDFILE ACTFILE.
           STOP RUN.
