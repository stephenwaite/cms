       IDENTIFICATION DIVISION.
       PROGRAM-ID. payfile-new.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
                      ACCESS MODE IS DYNAMIC RECORD KEY IS PAYFILE-KEY
                      LOCK MODE MANUAL.
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
       PROCEDURE DIVISION.
       P0.
           OPEN output payfile.
           close payfile.
           STOP RUN.
