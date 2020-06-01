       IDENTIFICATION DIVISION.
       PROGRAM-ID. RRR062.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYCUR ASSIGN TO "S90" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.
           SELECT FILE15 ASSIGN TO "S900" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S1350" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  FILE15.
       01  FILE1501 PIC X(50).
       FD  FILEOUT.
       01  FILEOUT01 PIC X(160).
       FD PAYCUR.
       01  PAYCUR01.
           02 PAYCUR-KEY.
             03 PC-KEY8 PIC X(8).
             03 PC-KEY3 PIC XXX.
           02 PC-AMOUNT PIC S9(4)V99.
           02 PC-PAYCODE PIC XXX.
           02 PC-DENIAL PIC XX.
           02 PC-CLAIM PIC X(6).
           02 PC-DATE-T PIC X(8).
           02 PC-DATE-E PIC X(8).
           02 PC-BATCH PIC X(6).
       WORKING-STORAGE SECTION.
       01  NUM1 PIC 9.
       01  cntr pic 999 value 0.
       PROCEDURE DIVISION.
       P0.
           OPEN output paycur FILEOUT.
           close paycur.
           open output paycur.
           open input file15.
       p15.
           read file15 at end close paycur go to p99.
           move file1501 to paycur01. 
           write paycur01
             invalid      
             move space to fileout01
             string "file15 " file1501 delimited by size into fileout01
             write fileout01
             GO TO p15
           end-write.  
           go to p15.

       p99.
           close FILEOUT file15.
           stop run.