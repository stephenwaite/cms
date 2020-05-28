       IDENTIFICATION DIVISION.
       PROGRAM-ID. email.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMAILAUTHSSNFILE ASSIGN TO "S30"
               ORGANIZATION IS INDEXED
           ACCESS MODE DYNAMIC RECORD KEY IS EA-KEY
           ALTERNATE RECORD KEY IS EA-MEDREC WITH DUPLICATES
           ALTERNATE RECORD KEY IS EA-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS EA-EMAIL WITH DUPLICATES
           ALTERNATE RECORD KEY IS EA-AUTH WITH DUPLICATES
           ALTERNATE RECORD KEY IS EA-DATE-E WITH DUPLICATES
           ALTERNATE RECORD KEY IS EA-SSN WITH DUPLICATES
               LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  EMAILAUTHSSNFILE.
       01  EMAILAUTHSSNFILE01.
           02 EA-KEY PIC 9(6).
           02 EA-MEDREC PIC X(8).
           02 EA-NAME PIC X(24).
           02 EA-EMAIL PIC X(30).
           02 EA-AUTH PIC X(20).
           02 EA-DATE-E PIC X(8).
           02 EA-SSN PIC X(9).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(360).


       WORKING-STORAGE SECTION.
       01  NUM1 PIC 9.
       01  cntr pic 999 value 0.
       PROCEDURE DIVISION.
       P0.
           OPEN I-O EMAILAUTHSSNFILE.
           OPEN OUTPUT FILEOUT.

       P1.
           READ EMAILAUTHSSNFILE
           read file27 at end close emailauthssnfile go to p28.
           move file2701 to emailauthssnfile01
           write emailauthssnfile01
             invalid
             move space to fileout01
             string "file27 " file2701 delimited by size into fileout01
             write fileout01
             end-write
           end-write
           go to p27.
       p28.
       p99.
           close FILEOUT.
           close FILE27.

           STOP RUN.
