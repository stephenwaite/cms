       IDENTIFICATION DIVISION.
       PROGRAM-ID. charfile-new.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CHARFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS CHARFILE-KEY
           LOCK MODE MANUAL.
       DATA DIVISION.
       FILE SECTION.
           FD  CHARFILE
      *    BLOCK CONTAINS 2 RECORDS
                   DATA RECORD IS CHARFILE01.
           01  CHARFILE01.
                   02 CHARFILE-KEY.
                     03 CD-KEY8 PIC X(8).
                     03 CD-KEY3 PIC XXX.
                   02 CD-PATID PIC X(8).
                   02 CD-CLAIM PIC X(6).
                   02 CD-SERVICE PIC X.
                   02 CD-DIAG PIC X(7).
                   02 CD-PROC PIC X(11).
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
                   02 CD-DX5 PIC X(7).
                   02 CD-DX6 PIC X(7).
                   02 CD-FUTURE PIC X(6).
       PROCEDURE DIVISION.
       P0.
           OPEN output charfile.
           close charfile.
           STOP RUN.
