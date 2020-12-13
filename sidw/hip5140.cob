       IDENTIFICATION DIVISION.
       PROGRAM-ID. hip5140.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO "S30" ORGANIZATION LINE SEQUENTIAL.
           
           SELECT FILEOUT ASSIGN TO "S35".
           
           SELECT PARMFILE ASSIGN TO "S40" ORGANIZATION LINE SEQUENTIAL.
           
           SELECT REMITIN ASSIGN TO "S45".

       DATA DIVISION.
       FILE SECTION.
       FD  PARMFILE.
       01  PARMFILE01 PIC X(40).
       FD  FILEIN.
       01  FILEIN01.
           02.
             03 FI-1 PIC XXX.
             03 FI-2 PIC X(4).
           02 FI-3 PIC X(113).
       FD  FILEOUT.
       01  FILEOUT01 PIC X.
       FD  REMITIN.
       01  REMITIN01 PIC X.

       WORKING-STORAGE SECTION.
       01  ALF1 PIC X.
       01  A PIC 999.
       01  X PIC 999.
       01  Y PIC 999.
       01  Z PIC 999.
       01  REF-1 PIC XXX.
       01  REF-2 PIC XX.
       01  REF-3 PIC X(6).
       01  SAVE-TAB01.
           02 SAVE-TAB PIC X(150) OCCURS 20 TIMES.
       01  TEST-TAB01.
           02 TEST-TAB PIC X OCCURS 185 TIMES.
       01  REMIT-TAB01.
           02 REMIT-TAB PIC X OCCURS 106 TIMES.
       01  PROV-1 PIC X(10).
       01  PROV-2 PIC X(10).
       01  PROV-FED PIC X(9).
       01  PROV-LEG PIC X(6).

       01  IN-NPI PIC X(10).
       01  IN-FEDID PIC X(9).
       01  IN-LEG PIC X(6).
       01  N101.
           02 N1-0 PIC XX.
           02 N1-1 PIC XX.
           02 N1-2 PIC X(20).
           02 N1-3 PIC XX.
           02 N1-4 PIC X(10).
       01  ANS PIC X.

       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT FILEIN PARMFILE REMITIN OUTPUT FILEOUT. 

           READ PARMFILE AT END GO TO P99.
           READ PARMFILE AT END GO TO P99.
           MOVE SPACE TO PROV-1 PROV-2
           UNSTRING PARMFILE01 DELIMITED BY " " INTO PROV-1 PROV-2
           READ PARMFILE AT END GO TO P99.
           MOVE PARMFILE01 TO PROV-FED.
           READ PARMFILE AT END GO TO P99.
           MOVE PARMFILE01 TO PROV-LEG.

       P000.
           READ FILEIN AT END GO TO P99.

           IF FI-1 NOT = "ISA" GO TO P000.
           
           MOVE FILEIN01 TO SAVE-TAB(1).

       P00.
           READ FILEIN AT END GO TO P99.

           IF FI-1 NOT = "GS*" GO TO P00.
           
           MOVE FILEIN01 TO SAVE-TAB(2).

       P0.
           READ FILEIN AT END GO TO P99.

           IF FI-1 NOT = "ST*" GO TO P0.
           
           MOVE FILEIN01 TO SAVE-TAB(3).
           MOVE 3 TO X.
           
       P1-2.
           READ FILEIN
             AT END 
             GO TO P99.
           
           IF FILEIN01(1:6) NOT = "N1*PE*"
             ADD 1 TO X
             MOVE FILEIN01 TO SAVE-TAB(X)
             GO TO P1-2
           END-IF

           MOVE SPACE TO N101
           UNSTRING FILEIN01 DELIMITED BY "*" INTO
           N1-0 N1-1 N1-2 N1-3 N1-4
           
           IF NOT ((N1-4 = PROV-1) OR (N1-4 = PROV-2)) GO TO P0.
           
           ADD 1 TO X
           MOVE FILEIN01 TO SAVE-TAB(X).

           PERFORM WRITE-THE-TOP
             VARYING A FROM 1 BY 1 UNTIL A > X 

           PERFORM WRITE-THE-BODY.

       P2. 
           READ FILEIN
             AT END 
               DISPLAY "BAD" 
               GO TO P99.
           
           PERFORM WRITE-THE-BODY
           
           IF FI-1 NOT = "SE*" GO TO P2.
           
      *     PERFORM WRITE-THE-END
           
           GO TO P0.

       WRITE-THE-TOP.
           MOVE SPACE TO TEST-TAB01
           MOVE SAVE-TAB(A) TO TEST-TAB01
           PERFORM TEST-1.

       WRITE-THE-BODY.
           MOVE SPACE TO TEST-TAB01
           MOVE FILEIN01 TO TEST-TAB01
           PERFORM TEST-1.

       WRITE-THE-END.
           MOVE SPACE TO FILEOUT01.
           MOVE "GE*6*90~" to FILEOUT01
           WRITE FILEOUT01
           MOVE SPACE TO FILEOUT01
           MOVE "IEA*1*999999~" TO FILEOUT01
           WRITE FILEOUT01.

       TEST-1.
           PERFORM VARYING Y FROM 185 BY -1 UNTIL Y < 1
             IF TEST-TAB(Y) NOT = SPACE
               ADD 1 TO Y
               MOVE "~" TO TEST-TAB(Y)
               PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > Y
                 WRITE FILEOUT01 FROM TEST-TAB(Z)
               END-PERFORM
               MOVE 1 TO Y
             END-IF
           END-PERFORM.

       P99. CLOSE REMITIN FILEIN PARMFILE FILEOUT.
            STOP RUN.
