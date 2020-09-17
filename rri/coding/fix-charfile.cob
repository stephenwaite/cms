       IDENTIFICATION DIVISION.
       PROGRAM-ID. fix-charfile.
       AUTHOR. SID WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
               ACCESS IS SEQUENTIAL RECORD KEY IS CHARFILE-KEY
               LOCK MODE MANUAL.
           
           SELECT FILEOUT ASSIGN TO  "S35" ORGANIZATION 
               LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).

       FD  CHARFILE
           DATA RECORD IS CHARFILE01.
       01  CHARFILE01.
           02 CHARFILE-KEY.
             03 CD-KEY8 PIC X(8).
             03 CD-KEY3 PIC XXX.
           02 CD-PATID PIC X(8).
           02 CD-CLAIM PIC X(6).
           02 CD-SERVICE PIC X.
           02 CD-DIAG PIC X(7).
           02 CD-PROC1 PIC X(4). 
           02 CD-PROC2 PIC X(7).
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
           02 CD-QP1 PIC XX.
           02 CD-QP2 PIC XX.
           02 CD-DX5-3 PIC X(3).
           02 CD-DX6 PIC X(7).
           02 CD-FUTURE PIC X(6).

       WORKING-STORAGE SECTION.

       01  HOLDIT PIC X(49).
       01  X PIC 9999.
       01  CLAIM-TOT PIC S9(5)V99.
       01  HOLD-DOCP PIC X(2).
       01  HOLD-DX PIC X(7).

       PROCEDURE DIVISION.

       P0.
           OPEN I-O CHARFILE.
           OPEN OUTPUT FILEOUT.

       P1. 
           READ CHARFILE
             AT END
               GO TO P6
           END-READ.
           
       P2. 
           IF CD-PROC2 NOT = "G1004  "
               MOVE CD-DOCP TO HOLD-DOCP
               MOVE CD-DIAG TO HOLD-DX           
               GO TO P1
           END-IF

           STRING "CHARGE FOR " CD-KEY8 " HAS AUC " CD-PROC2
                  "MOVE DOCP " HOLD-DOCP " AND DIAG " HOLD-DX   
               DELIMITED BY SIZE INTO FILEOUT01       
           WRITE FILEOUT01

           MOVE HOLD-DOCP TO CD-DOCP
           MOVE HOLD-DX TO CD-DIAG
           REWRITE CHARFILE01
           GO TO P1.

       P6. 
           CLOSE CHARFILE FILEOUT. 
           STOP RUN.
