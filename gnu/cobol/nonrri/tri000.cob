       IDENTIFICATION DIVISION.
       PROGRAM-ID. tri000.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CMNTFILE ASSIGN TO "S65" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CMNTFILE-KEY
           LOCK MODE IS MANUAL
           STATUS IS CMNT-STAT.
       DATA DIVISION.
       FILE SECTION.
       FD  CMNTFILE
           BLOCK CONTAINS 2 RECORDS
           DATA RECORD IS CMNTFILE01.
       01  CMNTFILE01.
           02 CMNTFILE-KEY.
             03 CMNT-KEY8 PIC X(8).
             03 CMNT-KEY3 PIC XXX.
           02 CMNT-DATA PIC X(80).
           02 CMNT-DATE-E PIC X(8).
       WORKING-STORAGE SECTION.
       01  ANS PIC XXX.
       01  CMNT-STAT PIC XX.
       01  DATAIN       PIC X(80).
       01 CMNTTAB01.
          02 CMNTTAB PIC X(3) OCCURS 8 TIMES.
       01  XYZ PIC 999.
       01  ALF-3.
           02 ALF3-1 PIC XX.
           02 ALF3-2 PIC X.
       01  X PIC 9.
       01  ALF-1 PIC X.
       LINKAGE SECTION.
       01  GARNO PIC X(8).
       PROCEDURE DIVISION USING GARNO.
       0005-START.
           OPEN I-O CMNTFILE.
           MOVE ALL SPACE TO CMNTTAB01.
       P1.
           DISPLAY "OPTION, ID   COMMENTS"
           ACCEPT ANS.
           IF ANS = "?"
           DISPLAY "A = ADD"
           DISPLAY "F = FIND ALL COMMENTS FOR THIS ACCOUNT"
           DISPLAY "D = DELETE A COMMENT FROM THIS ACCOUNT"
           DISPLAY "END OR X  = END ADDING COMMENTS"
           GO TO P1.
           IF ANS = "A" GO TO A1.
           IF ANS = "X" OR "END" GO TO P2.
           IF ANS = "D" GO TO D1.
           IF ANS = "F" GO TO F1.
           DISPLAY "INVALID" GO TO P1.
           GO TO P1.
       A1. DISPLAY "ENTER A LINE OF COMMENT."
           ACCEPT DATAIN
           IF DATAIN = "?"
           DISPLAY "ENTER THE COMMENT YOU WANT TO MAKE"
           DISPLAY "OR X TO END ADDING COMMENTS"
           DISPLAY "BK TO ISSUE OTHER COMMENT COMMANDS"
           GO TO A1.
           IF DATAIN = "X" GO TO P2.
           IF DATAIN = "BK" GO TO P1.
           MOVE GARNO TO CMNT-KEY8
           MOVE 0 TO XYZ.
       A2. ADD 1 TO XYZ MOVE XYZ TO CMNT-KEY3
           IF XYZ > 990 DISPLAY "TOO MANY COMMENTS ALREADY"
           GO TO  P1.
           MOVE DATAIN TO CMNT-DATA
           ACCEPT CMNT-DATE-E FROM DATE YYYYMMDD.
           WRITE CMNTFILE01 INVALID KEY GO TO A2.
           DISPLAY "ADDED"
           GO TO A1.
       F1. MOVE SPACE TO CMNT-KEY3
           MOVE GARNO TO CMNT-KEY8
           START CMNTFILE KEY > CMNTFILE-KEY INVALID GO TO P1.
           MOVE 0 TO X.
       F2. READ CMNTFILE NEXT AT END GO TO P1.
           IF CMNT-KEY8 NOT = GARNO GO TO P1.
           ADD 1 TO X
           MOVE CMNT-KEY3 TO CMNTTAB(X)
           DISPLAY X " " CMNT-DATA
           IF X > 7 DISPLAY "?"
           MOVE 0 TO X
           ACCEPT ANS
           IF ANS NOT = " " GO TO P1.
           GO TO F2.
        D1. DISPLAY "ENTER # 1-8 FROM THE LIST?"
            ACCEPT ANS
            IF ANS = "?"
            DISPLAY "X =  END THE DELETE FUNCTION"
            DISPLAY "ENTER THE 1-8 NUMBER FROM THE LIST"
            DISPLAY "DISPLAYED FROM THE F COMMAND"
            GO TO D1.
            IF ANS = "X" GO TO P1.
            IF ANS = "1" OR "2" OR "3" OR "4" OR "5" OR "6" OR "7"
            OR "8" GO TO D2.
            DISPLAY "INVALID" GO TO D1.
        D2. MOVE GARNO TO CMNT-KEY8
            MOVE ANS TO ALF-1
            MOVE ALF-1 TO X
            MOVE CMNTTAB(X) TO CMNT-KEY3
            DISPLAY CMNTFILE-KEY
            READ CMNTFILE WITH LOCK INVALID DISPLAY "RECORD NOT FOUND"
            GO TO D1.
            IF CMNT-STAT NOT = "00" DISPLAY "RECORD IS LOCKED"
            GO TO D1.
            DISPLAY CMNT-DATA
            DISPLAY "DELETE Y/N"
            ACCEPT ANS
            IF ANS NOT = "Y" DISPLAY "RECORD NOT DELETED" GO TO D1.
            DELETE CMNTFILE RECORD INVALID DISPLAY CMNT-STAT GO TO P1.
            DISPLAY "DELETED!" GO TO D1.

       P2. CLOSE CMNTFILE. 
           EXIT PROGRAM.
