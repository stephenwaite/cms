       IDENTIFICATION DIVISION.
       PROGRAM-ID. GARNOCNT.
      *================================================================
      * Count garnos per 3-character prefix.
      * Outputs one line per prefix: prefix + space + count.
      *================================================================
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GARFILE ASSIGN TO "S35"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS G-GARNO
               ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
               LOCK MODE MANUAL.
           SELECT FILEOUT ASSIGN TO "S40"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  GARFILE.
           COPY garfile.CPY.
       FD  FILEOUT.
       01  FILEOUT01             PIC X(15).
       WORKING-STORAGE SECTION.
       01  ALF3                  PIC XXX.
       01  NUM5                  PIC 9(5).
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT GARFILE OUTPUT FILEOUT
           READ GARFILE NEXT
               AT END GO TO P2
           END-READ
           MOVE G-GARNO(1:3) TO ALF3
           MOVE 1 TO NUM5.
       P1.
           READ GARFILE NEXT
               AT END GO TO P2
           END-READ
           IF G-GARNO(1:3) NOT = ALF3
               MOVE SPACE TO FILEOUT01
               STRING ALF3 " " NUM5
                   INTO FILEOUT01
               WRITE FILEOUT01
               MOVE G-GARNO(1:3) TO ALF3
               MOVE 1 TO NUM5
               GO TO P1
           END-IF
           ADD 1 TO NUM5.
           GO TO P1.
       P2.
           MOVE SPACE TO FILEOUT01
           STRING ALF3 " " NUM5
               INTO FILEOUT01
           WRITE FILEOUT01
           CLOSE GARFILE FILEOUT
           STOP RUN.
