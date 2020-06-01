       IDENTIFICATION DIVISION.
       PROGRAM-ID. cob057.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PROCFILE ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS PROC-KEY
           LOCK MODE MANUAL.

           SELECT FILE9 ASSIGN TO "S600" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S1450" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  FILE9.
       01  FILE901 PIC X(46).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(360).


       FD  PROCFILE.
       01  PROCFILE01.
           02 PROC-KEY.
             03 PROC-KEY1 PIC X(4).
             03 PROC-KEY2 PIC X(7).
           02 PROC-TYPE PIC X.
           02 PROC-TITLE PIC X(28).
           02 PROC-AMOUNT PIC 9(4)V99.

       WORKING-STORAGE SECTION.
       01  NUM1 PIC 9.
       01  cntr pic 999 value 0.
       01  ans pic 9.
       PROCEDURE DIVISION.
       P0.
           OPEN output procfile FILEOUT.
           close procfile.
           open i-o procfile.
           open input file9.

       p9.
           read file9 at end close procfile go to p99.
           move file901 to procfile01
           write procfile01 invalid
               move space to fileout01
               string "file9 " file901 delimited by size
                   into fileout01
               write fileout01
               end-write
           end-write
           go to p9.

       p99.
           close FILEOUT.
           close FILE9.
           STOP RUN.
