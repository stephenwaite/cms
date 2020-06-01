       IDENTIFICATION DIVISION.
       PROGRAM-ID. cobr030.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE19 ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FILE19.
       01  FILE1901 PIC X(318).
       FD  FILEOUT.
       01  FILEOUT01 pic x(318).
       WORKING-STORAGE SECTION.
       01  NUM1 PIC 9.
       01  cntr pic 9999 value 0.
       01  ALF8 PIC X(8).
       PROCEDURE DIVISION.
       P0.
           open input file19.
           OPEN OUTPUT FILEOUT.
        p19.
           read file19 at end go to p99.
           move FILE1901  to fileout01
           move fileout01(1:8) to ALF8
           write fileout01 FROM ALF8
           move FILE1901  to fileout01
           write fileout01 
           go to p19.

       p99.
           close file19 fileout.
           STOP RUN.
