       IDENTIFICATION DIVISION.
       PROGRAM-ID. dupr803.
       AUTHOR. SID WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
               ACCESS IS SEQUENTIAL RECORD KEY IS CHARFILE-KEY
               LOCK MODE MANUAL.
           SELECT CHARCUR ASSIGN TO  "S35" ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC RECORD KEY IS CHARCUR-KEY
               ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES.
           SELECT FILEOUT ASSIGN TO  "S40" ORGANIZATION 
               LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).
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
           02 CD-PROC1 PIC X(4). 
           02 CD-PROC2 PIC X(7).
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
       FD  CHARCUR
           BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS CHARCUR01.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC1 PIC X(4). 
           02 CC-PROC2 PIC X(7).
           02 CC-MOD2 PIC XX.
           02 CC-MOD3 PIC XX.
           02 CC-MOD4 PIC XX.
           02 CC-AMOUNT PIC S9(4)V99.
           02 CC-DOCR PIC X(3).
           02 CC-DOCP PIC X(2).
           02 CC-PAYCODE PIC 999.
           02 CC-STUD PIC X.
           02 CC-WORK PIC XX.
           02 CC-DAT1 PIC X(8).
           02 CC-RESULT PIC X.
           02 CC-ACT PIC X.
           02 CC-SORCREF PIC X.
           02 CC-COLLT PIC X.
           02 CC-AGE PIC X.
           02 CC-PAPER PIC X.
           02 CC-PLACE PIC X.
           02 CC-EPSDT PIC X.
           02 CC-DATE-T PIC X(8).
           02 CC-DATE-A PIC X(8).
           02 CC-DATE-P PIC X(8).
           02 CC-REC-STAT PIC X.
           02 CC-DX2 PIC X(7).
           02 CC-DX3 PIC X(7).
           02 CC-ACC-TYPE PIC X.
           02 CC-DATE-M PIC X(8).
           02 CC-ASSIGN PIC X.
           02 CC-NEIC-ASSIGN PIC X.
           02 CC-DX4 PIC X(7).
           02 CC-DX5 PIC X(7).
           02 CC-DX7 PIC X(7).
           02 CC-FUTURE PIC X(6).
       WORKING-STORAGE SECTION.
       01  HOLDIT PIC X(49).
       01  X PIC 9999.
       01  CLAIM-TOT PIC S9(5)V99.
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT CHARCUR CHARFILE.
           OPEN OUTPUT FILEOUT.
       P1. 
           READ CHARFILE AT END
               GO TO P6
           END-READ
           MOVE CD-KEY8 TO CC-KEY8.
           MOVE "000" TO CC-KEY3.
           START CHARCUR KEY NOT < CHARCUR-KEY INVALID 
               GO TO P1
           END-START.    
       S7. 
           READ CHARCUR NEXT AT END
               GO TO P1
           END-READ    
           IF CC-KEY8 NOT = CD-KEY8 
               GO TO P1
           END-IF

           IF (CC-DATE-T = CD-DATE-T) AND (CC-PROC2 = CD-PROC2)
               STRING "CHARGE FOR " CD-KEY8 " HAS SAME DOS " CD-DATE-T
                      " AND SAME PROC " CD-PROC2
               DELIMITED BY SIZE INTO FILEOUT01       
               WRITE FILEOUT01
               GO TO P1
           END-IF

           GO TO S7.
       P6. 
           CLOSE CHARCUR CHARFILE FILEOUT. 
           STOP RUN.
