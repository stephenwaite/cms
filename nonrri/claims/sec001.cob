       IDENTIFICATION DIVISION.
       PROGRAM-ID. sec001.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
           LOCK MODE MANUAL.
           SELECT FILEIN ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S40" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT INSFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
           LOCK MODE IS MANUAL.
           SELECT PARMFILE ASSIGN TO "S50" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT MPLRFILE ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS MPLR-KEY
           LOCK MODE IS MANUAL.
           SELECT GAPFILE ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS GAPKEY
           ALTERNATE RECORD KEY IS GAP-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-STATE WITH DUPLICATES
           LOCK MODE IS MANUAL.
           SELECT PROGFILE ASSIGN TO "S65" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD PROGFILE.
       01 PROGFILE01 PIC XXX.
       FD GAPFILE.
       01 GAPFILE01.
           02 GAPKEY PIC X(7).
           02 GAP-NAME PIC X(25).
           02 GAP-ADDR PIC X(22).
           02 GAP-CITY PIC X(15).
           02 GAP-STATE PIC XX.
           02 GAP-ZIP PIC X(9).
           02 GAP-TYPE PIC X.
           02 GAP-FUTURE PIC X(40).
       FD  MPLRFILE.
       01  MPLRFILE01.
           02 MPLR-KEY PIC X(8). 
           02 MPLR-NAME PIC X(22).
           02 MPLR-STREET PIC X(24).
           02 MPLR-CITY PIC X(15).
           02 MPLR-STATE PIC XX.
           02 MPLR-ZIP PIC X(9).
           02 MPLR-CLAIMNO PIC X(15).
           02 MPLR-TRINS PIC XXX.
           02 MPLR-TR-ASSIGN PIC X.
           02 MPLR-TR-GROUP PIC X(12).
           02 MPLR-TRIPOL PIC X(14).
           02 MPLR-TR-NAME PIC X(24).
           02 MPLR-TR-RELATE PIC X.
           02 MPLR-FUTURE PIC X(6).
       FD  PARMFILE.
       01  PARMFILE01 PIC X(9).
       FD  INSFILE
           BLOCK CONTAINS 6 RECORDS
           DATA RECORD IS INSFILE01.
       01  INSFILE01.
           02 INS-KEY PIC XXX.
           02 INS-NAME PIC X(22).
           02 INS-STREET PIC X(24).
           02 INS-CITY PIC X(15).
           02 INS-STATE PIC XX.
           02 INS-ZIP PIC X(9).
           02 INS-ASSIGN PIC X.
           02 INS-CLAIMTYPE PIC X.
           02 INS-NEIC PIC X(5).
           02 INS-NEICLEVEL PIC X.
           02 INS-NEIC-ASSIGN PIC X.
           02 INS-PPO PIC X.
           02 INS-PRVNUM PIC X(10).
           02 INS-HMO PIC X(3).
           02 INS-STATUS PIC X.
           02 INS-LEVEL PIC X.
           02 INS-LASTDATE PIC X(8).
           02 INS-CAID PIC XXX.
           02 INS-REFWARN PIC X.
           02 INS-FUTURE PIC X(8).
       FD FILEIN.
       01 FILEIN01.
           02 FI-INS PIC X(3).
           02 FI-PATID PIC X(8).
           02 FI-KEY8 PIC X(8).
           02 FILLER PIC X(15).
           02 FI-PAPER PIC X.
           02 FI-FILLER PIC X(16).
       FD FILEOUT.
       01 FILEOUT01. 
          02 FO-1 PIC X(51).
          02 FO-PS PIC X.
          02 FO-CLIENT PIC X(9).
          02 FO-FILLER PIC X.
          02 FO-GROUP PIC X.
          02 FO-GAPKEY PIC X(7).
          02 FO-CITY PIC X(15).
          02 FO-STREET PIC X(24).
          02 FO-NAME PIC X(22).
          02 FO-PROG PIC XXX.
       FD GARFILE.
       01 GARFILE01.
           02 G-GARNO PIC X(8).
           02 G-GARNAME PIC X(24).
           02 G-BILLADD PIC X(22).
           02 G-STREET PIC X(22).
           02 G-CITY PIC X(18).
           02 G-STATE PIC X(2).
           02 G-ZIP. 
              03 GZIP5 PIC X(5).
              03 GZIP4 PIC X(4).
           02 G-COLLT PIC X.
           02 G-PHONE PIC X(10).
           02 G-SEX PIC X.
           02 G-RELATE PIC X.
           02 G-MSTAT PIC X.
           02 G-DOB.
              03 G-DOBYY PIC X(4). 
              03 G-DOBMM PIC XX.
              03 G-DOBDD PIC XX.
           02 G-DUNNING PIC X.
           02 G-ACCTSTAT PIC X.
           02 G-PR-MPLR PIC X(4).
           02 G-PRINS PIC XXX.
           02 G-PR-ASSIGN PIC X.
           02 G-PR-OFFICE PIC X(4).
           02 G-PR-GROUP PIC X(12).
           02 G-PRIPOL PIC X(14).
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-SE-MPLR PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
           02 G-SE-GROUP PIC X(12).
           02 G-SECPOL PIC X(14).
           02 G-SENAME PIC X(24).
           02 G-SE-RELATE PIC X.
           02 G-COPAY PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.
       WORKING-STORAGE SECTION.
       01  TEST-DATE.
           05  T-CC            PIC XX.
           05  T-YY            PIC XX.
           05  T-MM            PIC XX.
           05  T-DD            PIC XX.
       01  ALF8 PIC X(8).
       01  ALF24 PIC X(24).
       01  ALF25 PIC X(24).
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT GARFILE MPLRFILE GAPFILE FILEIN INSFILE PARMFILE
           PROGFILE.
           OPEN OUTPUT FILEOUT.
           READ PROGFILE AT END GO TO P99.
           READ PARMFILE AT END GO TO P99.
           
       P1. 
           READ FILEIN AT END GO TO P99.
           
           MOVE SPACE TO FILEOUT01
           MOVE FILEIN01 TO FO-1
           MOVE PARMFILE01 TO FO-CLIENT
           IF FI-PAPER = "O"
             MOVE SPACE TO FO-STREET
             MOVE SPACE TO FO-CITY
             MOVE SPACE TO FO-NAME
             MOVE SPACE TO FO-GAPKEY
             MOVE "0" TO FO-GROUP
             MOVE PROGFILE01 TO FO-PROG
             WRITE FILEOUT01 GO TO P1.

           MOVE FI-KEY8 TO G-GARNO
           READ GARFILE 
             INVALID DISPLAY "BAD " FI-KEY8 
             GO TO P1.

           MOVE FI-INS TO INS-KEY
           READ INSFILE 
             INVALID 
               MOVE SPACE TO INSFILE01.
           
           IF NOT (G-PRINS = "003" AND FI-INS = "062") GO TO P2.
           
           MOVE "2" TO FO-GROUP 
           MOVE G-PR-GROUP TO FO-GAPKEY GAPKEY
           READ GAPFILE INVALID MOVE SPACE TO GAP-CITY GAP-ADDR.
           MOVE GAP-CITY TO FO-CITY
           MOVE GAP-ADDR TO FO-STREET
           MOVE GAP-NAME TO FO-NAME
           
           IF GAP-CITY = SPACE MOVE "0" TO FO-GROUP.
           
           IF FI-PAPER = "O" MOVE "0" TO FO-GROUP.
           
           MOVE PROGFILE01 TO FO-PROG
           WRITE FILEOUT01 
           GO TO P1.

       P2. 
           MOVE PROGFILE01 TO FO-PROG
           IF FI-INS = "091" PERFORM P5.
           
           IF (FI-INS = "091") AND (FO-CITY = SPACE)
             MOVE SPACE TO FO-STREET
             MOVE SPACE TO FO-NAME
             MOVE "0" TO FO-GROUP
             WRITE FILEOUT01 
             GO TO P1.

           IF (FI-INS = "091")
             MOVE "3" TO FO-GROUP
             WRITE FILEOUT01 GO TO P1.
            
           IF (FI-INS = G-TRINS) 
             MOVE "0" TO FO-GROUP 
             MOVE SPACE TO FO-CITY
             MOVE SPACE TO FO-STREET
             MOVE SPACE TO FO-NAME
             WRITE FILEOUT01 
             GO TO P1.

           IF (FI-INS = G-SEINS) AND (G-PRINS NOT = "003")
             MOVE SPACE TO FO-CITY FO-STREET FO-NAME
             MOVE 0 TO FO-GROUP
             WRITE FILEOUT01 
             GO TO P1.
             
           IF (FI-INS = G-SEINS) AND (G-PRINS = "003")
             MOVE "2" TO FO-GROUP 
             MOVE INS-CITY TO FO-CITY
             MOVE INS-STREET TO FO-STREET
             MOVE INS-NAME TO FO-NAME
             WRITE FILEOUT01 
             GO TO P1.
            
           IF FI-INS = G-PRINS 
             MOVE "3" TO FO-GROUP 
             MOVE INS-CITY TO FO-CITY
             MOVE INS-STREET TO FO-STREET
             MOVE INS-NAME TO FO-NAME
             IF INS-CITY = SPACE
               MOVE "0" TO FO-GROUP
             END-IF
             WRITE FILEOUT01 
             GO TO P1.

           MOVE "0" TO FO-GROUP
           WRITE FILEOUT01 
           GO TO P1.
       P5. 
           MOVE FI-KEY8 TO MPLR-KEY
           READ MPLRFILE 
             INVALID 
               MOVE SPACE TO MPLR-NAME MPLR-CITY MPLR-STREET.
               MOVE MPLR-CITY TO FO-CITY
               MOVE MPLR-STREET TO FO-STREET
               MOVE MPLR-NAME TO FO-NAME.

       P99. 
           CLOSE FILEOUT GARFILE MPLRFILE GAPFILE
           STOP RUN.
