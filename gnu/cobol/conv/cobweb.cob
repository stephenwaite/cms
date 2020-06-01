       IDENTIFICATION DIVISION.
       PROGRAM-ID. cobweb.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.


           SELECT WEBFILE ASSIGN TO "S80" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS WEB-KEY
           LOCK MODE IS MANUAL.


           SELECT FILE11 ASSIGN TO "S800"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S999"
           ORGANIZATION LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.

       FD  FILE11.
       01  FILE1101 PIC X(12).

       FD  WEBFILE.
       01  WEBFILE01.
           02 WEB-KEY PIC X(8).
           02 WEB-NUM PIC 9(4).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(130).

       WORKING-STORAGE SECTION.
       01  NUM1 PIC 9.
       PROCEDURE DIVISION.
       P0.
           OPEN output  WEBFILE fileout.
           close  WEBFILE.

           OPEN I-O  WEBFILE.
           open input FILE11.

       p11.
           read FILE11 at end go to p99
           end-read
           move FILE1101 to WEBFILE01
           write WEBFILE01
           invalid 
           MOVE SPACE TO FILEOUT01
           write fileout01 from file1101
           end-write.
           go to p11.


       p99.
           CLOSE  WEBFILE FILE11 FILEOUT.
           STOP RUN.
