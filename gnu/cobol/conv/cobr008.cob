       IDENTIFICATION DIVISION.
       PROGRAM-ID. cobr008.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE14 ASSIGN TO "S850" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S851" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FILE14.
       01  FILE1401 PIC X(160).
       FD  FILEOUT.
       01  FILEOUT01.
           02 FO-2  PIC X(160). 
       01  FILEOUT01X.
           02 FO-2X PIC X(11).
       WORKING-STORAGE SECTION.
       01  NUM1 PIC 9.
       01  cntr pic 9999 value 0.
       PROCEDURE DIVISION.
       P0.
           open input file14.
           OPEN OUTPUT FILEOUT.
        p14.
           read file14 at end go to p99.
           move space to fileout01 fileout01x
           move file1401(1:11) to FO-2x
           write fileout01x
           MOVE file1401 to FO-2
           write fileout01 
           go to p14.

       p99.
           close file14 fileout.
           STOP RUN.
