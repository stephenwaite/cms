       IDENTIFICATION DIVISION.
       PROGRAM-ID. fix-fee-charfile.
       AUTHOR. SWAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PROCFILE ASSIGN TO "S25" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS PROC-KEY.

           SELECT CHARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC RECORD KEY IS CHARFILE-KEY
               LOCK MODE MANUAL.
           
           SELECT FILEOUT ASSIGN TO  "S35" ORGANIZATION 
               LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).

       FD  CHARFILE.
           COPY charfile.CPY IN "C:\Users\sid\cms\copylib".

       FD  PROCFILE.
           COPY procfile.CPY IN "C:\Users\sid\cms\copylib".

       WORKING-STORAGE SECTION.

       01  HOLDIT PIC X(49).
       01  X PIC 9999.
       01  CLAIM-TOT PIC S9(5)V99.
       01  HOLD-DOCP PIC X(2).
       01  HOLD-DX PIC X(7).

       PROCEDURE DIVISION.

       P0.
           OPEN INPUT PROCFILE.
           OPEN I-O CHARFILE.
           OPEN OUTPUT FILEOUT.

           MOVE SPACE TO CHARFILE-KEY.

           START CHARFILE KEY NOT < CHARFILE-KEY
             INVALID 
               DISPLAY "NO RECORDS FOUND"
               GO TO P6.  

       P1. 
           READ CHARFILE NEXT WITH LOCK
             AT END
               GO TO P6
           END-READ.
           
       P2. 
           IF CD-PAYCODE NOT = "003"
               GO TO P1
           END-IF

           IF CD-STAT > 1
             GO TO P1
           end-if

           MOVE CD-PROC TO PROC-KEY

           READ PROCFILE
             invalid
               DISPLAY CD-PROC " NOT IN PROCFILE"
               GO TO P1
           end-read                

           STRING "CHARGE FOR " CD-KEY8 " CPT " CD-PROC
             "UPDATING FEE FROM " CD-AMOUNT " TO " CARE-AMOUNT   
             DELIMITED BY SIZE INTO FILEOUT01       
           WRITE FILEOUT01

           MOVE CARE-AMOUNT TO CD-AMOUNT
           REWRITE CHARFILE01
           GO TO P1.

       P6. 
           CLOSE PROCFILE CHARFILE FILEOUT. 
           STOP RUN.
