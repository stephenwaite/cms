       IDENTIFICATION DIVISION.
       PROGRAM-ID. pay006.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYMASTER ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL RECORD KEY IS ACCT.
           SELECT PAYBACK ASSIGN TO "S35"
           ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  PAYMASTER.
       01  PAYMASTER01.
           02 ACCT.
             03 ACCT1 PIC XX.
             03 ACCT2 PIC XXX.
           02 NAME PIC X(24).
           02 DATEX PIC X(6).
           02 GROSS PIC 9(5)V99.
           02 FED PIC 9(5)V99.
           02 FICA PIC 9(5)V99.
           02 STATE PIC 9(5)V99.
           02 NET PIC 9(5)V99.
           02 FILLER PIC X(15) VALUE SPACE.
       FD  PAYBACK
           DATA RECORD IS PAYBACK01.
       01  PAYBACK01 PIC X(85).
       WORKING-STORAGE SECTION.
       01  LOW-DATE PIC X(6).
       01  HIGH-DATE PIC X(6).
       PROCEDURE DIVISION.
       P0. OPEN INPUT PAYMASTER OUTPUT PAYBACK.
           DISPLAY "LOW DATE   YYMMDD"
           ACCEPT LOW-DATE
           DISPLAY "HIGH DATE YYMMDD"
           ACCEPT HIGH-DATE.
       P1. READ PAYMASTER AT END GO TO P2.
           IF DATEX < LOW-DATE OR DATEX > HIGH-DATE GO TO P1.
           MOVE PAYMASTER01 TO PAYBACK01
           WRITE PAYBACK01 GO TO P1.
       P2. CLOSE PAYBACK. 
           STOP RUN.
