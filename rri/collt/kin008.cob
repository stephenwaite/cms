       IDENTIFICATION DIVISION.
       PROGRAM-ID. kin008.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       
           SELECT FILEIN ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FILEIN.
       01  FILEIN01.
           02 FI-GARNO PIC X(8).
           02 FILLER PIC X VALUE SPACES.
           02 FI-DEDUCT PIC 9(4)V99.
           02 FILLER PIC X VALUE SPACES.
           02 FI-PAID PIC 9(4)V99.

       FD FILEOUT.
       01 FILEOUT01.
           02 FO-GARNO PIC X(8).
           02 FILLER PIC X VALUE SPACES.
           02 FO-DEDUCT PIC 9(4)V99.
           02 FILLER PIC X VALUE SPACES.
           02 FO-PAID PIC 9(4)V99.

       WORKING-STORAGE SECTION.
       
       01  HOLD-8  PIC X(8).
       01  HOLD-PD PIC 9(4)V99.
       01  HOLD-RED PIC 9(4)V99.

       PROCEDURE DIVISION.

       0005-START.

           OPEN INPUT FILEIN
           OPEN OUTPUT FILEOUT.
           READ FILEIN 
             AT END
               GO TO P9.

           MOVE FI-GARNO TO HOLD-8
           MOVE FI-DEDUCT TO HOLD-RED
           MOVE FI-PAID TO HOLD-PD.

       P1.
           READ FILEIN 
             AT END
               GO TO P9.

           IF FI-GARNO = HOLD-8
             ADD FI-PAID TO HOLD-PD
             ADD FI-DEDUCT TO HOLD-RED
             GO TO P1.

           MOVE HOLD-8 TO FO-GARNO
           MOVE HOLD-RED TO FO-DEDUCT
           MOVE HOLD-PD TO FO-PAID
           WRITE FILEOUT01
           
           MOVE FI-GARNO TO HOLD-8
           MOVE FI-DEDUCT TO HOLD-RED
           MOVE FI-PAID TO HOLD-PD.
           GO TO P1.

       P9.

           MOVE HOLD-8 TO FO-GARNO
           MOVE HOLD-RED TO FO-DEDUCT
           MOVE HOLD-PD TO FO-PAID
           WRITE FILEOUT01

           CLOSE FILEOUT
           STOP RUN.
