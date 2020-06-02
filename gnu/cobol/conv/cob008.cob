       IDENTIFICATION DIVISION.
       PROGRAM-ID. cob008.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE13 ASSIGN TO "S850" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S851" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FILE13.
       01  FILE1301 PIC X(156).
       FD  FILEOUT.
       01  FILEOUT01 pic x(156).
       WORKING-STORAGE SECTION.
       01  NUM1 PIC 9.
       01  cntr pic 9999 value 0.
       01  ALF11 PIC X(11).
       PROCEDURE DIVISION.
       P0.
           open input file13.
           OPEN OUTPUT FILEOUT.
        p13.
           read file13 at end go to p99.
           move FILE1301  to fileout01
           move fileout01(1:11) to ALF11
           write fileout01 FROM ALF11
           move FILE1301  to fileout01
           write fileout01 
           go to p13.

       p99.
           close file13 fileout.
           STOP RUN.
