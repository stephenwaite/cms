       IDENTIFICATION DIVISION.
       PROGRAM-ID. payfile-unl.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
                      ACCESS MODE IS DYNAMIC RECORD KEY IS PAYFILE-KEY
                      LOCK MODE MANUAL.
           SELECT FILEOUT ASSIGN TO "S40" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

        FD  PAYFILE.
              01  PAYFILE01.
                  02 PAYFILE-KEY.
                    03 PD-KEY8 PIC X(8).
                    03 PD-KEY3 PIC XXX.
                  02 PD-NAME PIC X(24).
                  02 PD-AMOUNT PIC S9(4)V99.
                  02 PD-PAYCODE PIC XXX.
                  02 PD-DENIAL PIC XX.
                  02 PD-CLAIM PIC X(6).
                  02 PD-DATE-T PIC X(8).
                  02 PD-DATE-E PIC X(8).
                  02 PD-ORDER PIC X(6).
                  02 PD-BATCH PIC X(6).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).


       WORKING-STORAGE SECTION.
       01  NUM1 PIC 9.
       01  cntr pic 999 value 0.
       PROCEDURE DIVISION.
       P0.
           OPEN output FILEOUT.
           open input payfile.

       p27.
           read payfile next at end
               go to p99
           end-read

           move payfile01 to fileout01
           write fileout01
           go to p27.
       p99.
           close FILEOUT.
           close payfile.

           STOP RUN.
