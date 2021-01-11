        IDENTIFICATION DIVISION.
       PROGRAM-ID. hip5147.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S35".
           SELECT PARMFILE ASSIGN TO "S40" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT REMITIN ASSIGN TO "S45".

       DATA DIVISION.
       FILE SECTION.
       FD  PARMFILE.
       01  PARMFILE01 PIC X(40).
       FD FILEIN.
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
       01  X PIC 999.
       01  Y PIC 999.
       01  Z PIC 999.
       01  REF-1 PIC XXX.
       01  REF-2 PIC XX.
       01  REF-3 PIC X(7).

       01  SAVE-TAB01.
           02 SAVE-TAB PIC X(150) OCCURS 14 TIMES.
           
       01  TEST-TAB01.
           02 TEST-TAB PIC X OCCURS 260 TIMES.

       01  REMIT-TAB01.
           02 REMIT-TAB PIC X OCCURS 106 TIMES.

       01  PROV-1 PIC X(10).

       01  PROV-2 PIC X(10).
       01  PROV-FED PIC X(9).
       01  PROV-LEG PIC X(7).

       01  IN-NPI PIC X(10).
       01  IN-FEDID PIC X(9).
       01  IN-LEG PIC X(7).

       01 N101.
           02 N1-0 PIC XX.
           02 N1-1 PIC XX.
           02 N1-2 PIC X(20).
           02 N1-3 PIC XX.
           02 N1-4 PIC X(10).
       01  GS01.
           02 GS-0 PIC XX.
           02 GS-1 PIC XX.
           02 GS-2 PIC X(20).
           02 GS-3 PIC X(10).
           02 GS-4 PIC X(8).
           02 GS-5 PIC X(10).
           02 GS-6 PIC X(6). 
       01  ISA01.
           02 ISA-0 PIC XX.
           02 ISA-1 PIC XX.
           02 ISA-2 PIC X(9).
           02 ISA-3 PIC XX.
           02 ISA-4 PIC X(9).
           02 ISA-5 PIC XX.
           02 ISA-6 PIC X(15).   
           02 ISA-7 PIC XX.
           02 ISA-8 PIC X(15).
           02 ISA-9 PIC X(6). 
           02 ISA-10 PIC X(4).
           02 ISA-11 PIC X.
           02 ISA-12 PIC X(5).
           02 ISA-13 PIC X(9).
           02 ISA-14 PIC X.
           02 ISA-15 PIC X.                 

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

           MOVE SPACE TO REMIT-TAB01
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 106
             READ REMITIN
               AT END
                 GO TO P99
             END-READ
             WRITE FILEOUT01 FROM REMITIN01
           END-PERFORM
           
           MOVE SPACE TO REMITIN01
           PERFORM VARYING X FROM 1 BY 1 UNTIL REMITIN01 = "~"
             READ REMITIN 
               AT END 
                 GO TO P99
             END-READ
             WRITE FILEOUT01 FROM REMITIN01
           END-PERFORM.

       P1. 
           READ FILEIN AT END GO TO P99.           

           IF FI-1 = "ISA"
             MOVE SPACE TO   ISA01
             UNSTRING FILEIN01 DELIMITED BY "*" INTO
               ISA-0 ISA-1 ISA-2 ISA-3 ISA-4 ISA-5 ISA-6 ISA-7
               ISA-8 ISA-9 ISA-10 ISA-11 ISA-12 ISA-13
           end-if    

           IF FI-1 = "GS*"
             MOVE SPACE TO GS01
             UNSTRING FILEIN01 DELIMITED BY "*" INTO
               GS-0 GS-1 GS-2 GS-3 GS-4 GS-5 GS-6  
           end-if    

           IF FI-1 = "GE*" GO TO P1.

           IF FI-1 NOT = "ST*" GO TO P1.

           MOVE SPACE TO SAVE-TAB01 IN-LEG IN-NPI
           MOVE 1 TO X
           MOVE FILEIN01 TO SAVE-TAB(X)
           PERFORM VARYING X FROM 2 BY 1 UNTIL X > 9
             READ FILEIN
               AT END 
                 GO TO P99
             END-READ
            MOVE FILEIN01 TO SAVE-TAB(X)
           END-PERFORM.

           READ FILEIN AT END GO TO P99.

           IF FI-1 NOT = "N1*"
             DISPLAY FILEIN01 " BAD"
             GO TO P99.

           MOVE SPACE TO N101
           UNSTRING FILEIN01 DELIMITED BY "*" INTO
             N1-0 N1-1 N1-2 N1-3 N1-4
           
           IF N1-3 = "FI" MOVE N1-4 TO IN-FEDID.
           
           IF N1-3 = "XX" MOVE N1-4 TO IN-NPI. 
           
           MOVE 10 TO X
           MOVE FILEIN01 TO SAVE-TAB(X)
           PERFORM VARYING X FROM 11 BY 1 UNTIL X > 12
             READ FILEIN 
               AT END 
                 GO TO P99
             END-READ
             MOVE FILEIN01 TO SAVE-TAB(X)
           END-PERFORM.

           READ FILEIN 
             AT END
               GO TO P99.

           MOVE SPACE TO REF-1 REF-2 REF-3
           UNSTRING FILEIN01 DELIMITED BY "*" INTO REF-1 REF-2 REF-3
           
           IF REF-2 = "PQ" MOVE REF-3 TO IN-LEG.

           MOVE 13 TO X
           MOVE FILEIN01 TO SAVE-TAB(13)

           READ FILEIN 
             AT END
               GO TO P99.

           MOVE SPACE TO REF-1 REF-2 REF-3
           UNSTRING FILEIN01 DELIMITED BY "*" INTO REF-1 REF-2 REF-3
           
           IF REF-2 = "TJ" MOVE REF-3 TO PROV-FED.

           MOVE 14 TO X
           MOVE FILEIN01 TO SAVE-TAB(14)

           IF NOT ((IN-NPI = PROV-1 OR PROV-2) OR (PROV-LEG = IN-LEG))
             GO TO P1.
           
           PERFORM WRITE-THE-TOP VARYING X FROM 1 BY 1 UNTIL X > 14.
                     
       P2. 
           READ FILEIN 
             AT END
               DISPLAY "BAD"
               GO TO P99.

           PERFORM WRITE-THE-BODY
           
           IF FI-1 = "SE*" 
             PERFORM WRITE-THE-END
             GO TO P1.
           
           GO TO P2.

       WRITE-THE-TOP.
           MOVE SPACE TO TEST-TAB01
           MOVE SAVE-TAB(X) TO TEST-TAB01
           PERFORM TEST-1.

       WRITE-THE-BODY.
           MOVE SPACE TO TEST-TAB01
           MOVE FILEIN01 TO TEST-TAB01
           PERFORM TEST-1.

       WRITE-THE-END.
           MOVE SPACE TO TEST-TAB01.
           STRING "GE*15*" GS-6 DELIMITED BY SIZE
             INTO TEST-TAB01
           PERFORM TEST-1
           MOVE SPACE TO TEST-TAB01.
           STRING "IEA*1*" ISA-13 DELIMITED BY SIZE
             INTO TEST-TAB01
           PERFORM TEST-1.           

       TEST-1.
           PERFORM VARYING Y FROM 260 BY -1 UNTIL Y < 1
             IF TEST-TAB(Y) NOT = SPACE
               ADD 1 TO Y
               MOVE "~" TO TEST-TAB(Y)
               PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > Y
                 WRITE FILEOUT01 FROM TEST-TAB(Z)
               END-PERFORM
               MOVE 1 TO Y
             END-IF
           END-PERFORM.

       P99. 
           CLOSE REMITIN FILEIN PARMFILE FILEOUT.
           STOP RUN.
