       IDENTIFICATION DIVISION.
       PROGRAM-ID. med007.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           
           SELECT MEDFILE2020 ASSIGN TO "S30" ORGANIZATION INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS MED-KEY
               LOCK MODE MANUAL.

           SELECT FILEIN ASSIGN TO "S35"
               ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD FILEIN.
       01  FILEIN01.
           02 FI-1 PIC X.
           02 FILLER PIC X(5).
           02 FI-2 PIC X(5).
           02 FILLER PIC X(15).
           02 FI-3 PIC XX.
           02 FILLER PIC X(17).
           02 FI-4 PIC X(7).
       FD MEDFILE2020.
       01 MEDFILE202001.
           02 MED-KEY.
             03 MED-KEY1 PIC X(5).
             03 MED-KEY2 PIC XX.
           02 MED-AMT PIC 9(4)V99.
           
       WORKING-STORAGE SECTION.
       01  FI-CPT PIC X(5).
       01  FI-MOD PIC XX.
       01  FI-AMT PIC X(7).
       01  NUM2 PIC 99.
       01  NUM4 PIC 9999.
       01  ALF2 PIC XX.
       01  ALF5 PIC X(5).
       01  DOLLARX PIC X(4).
       01  CENTX PIC XX.
       01  RIGHT-4 pic X(4) JUST RIGHT.
       01  X-AMT PIC 9(4)V99.
       PROCEDURE DIVISION.
       P0.
           OPEN OUTPUT MEDFILE2020
           CLOSE MEDFILE2020
           OPEN INPUT FILEIN.
           OPEN I-O MEDFILE2020.

       P1.
           READ FILEIN AT END
               GO TO P99
           END-READ

           MOVE FI-4 TO FI-AMT
           MOVE FI-3 TO FI-MOD
           MOVE FI-2 TO FI-CPT
           
           IF FI-1 = "C"
               GO TO P1
           END-IF

           IF FI-MOD = "TC" 
               GO TO P1
           END-IF

           IF (FI-CPT(1:1) = "7") AND (FI-MOD NOT = "26" )
               GO TO P1
           END-IF.
       P1-1.          
           MOVE SPACE TO RIGHT-4 CENTX
           UNSTRING FI-AMT DELIMITED BY "." INTO RIGHT-4 CENTX
           MOVE FI-CPT TO MED-KEY1
           MOVE FI-MOD TO MED-KEY2
           INSPECT RIGHT-4 REPLACING ALL " " BY "0".
           INSPECT CENTX REPLACING ALL " " BY "0".
           
           IF CENTX NOT NUMERIC
               DISPLAY FI-CPT " CENTX NOT NUM " CENTX
               ACCEPT OMITTED
           END-IF
           
           IF RIGHT-4 NOT NUMERIC
               DISPLAY FI-CPT " RIGHT-4 NOT NUM " RIGHT-4
               ACCEPT OMITTED
           END-IF

           MOVE RIGHT-4 TO NUM4
           MOVE CENTX to NUM2
      
           COMPUTE MED-AMT = NUM4 + (NUM2 / 100)
           
           IF FI-1 = "#"
               COMPUTE X-AMT = MED-AMT
               READ MEDFILE2020 WITH LOCK
                 INVALID
                   WRITE MEDFILE202001
                   END-WRITE
                 NOT INVALID
                   MOVE X-AMT TO MED-AMT
                   REWRITE MEDFILE202001           
                   END-REWRITE
               END-READ
               GO TO P1
           END-IF
           
           WRITE MEDFILE202001
           GO TO P1.
       P99.
           CLOSE MEDFILE2020 FILEIN.
           STOP RUN.
