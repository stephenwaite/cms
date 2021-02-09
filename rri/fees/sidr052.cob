       IDENTIFICATION DIVISION.
       PROGRAM-ID. sidr052.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PROCFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS PROC-KEY
               LOCK MODE MANUAL STATUS IS PROC-STAT.
           
           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
               LINE SEQUENTIAL.
           
           SELECT BILLPARM ASSIGN TO "S40" ORGANIZATION
               LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  BILLPARM.
       01  BILLPARM01.
           02 FILLER PIC X(9).
           02 BP-2 PIC X(30).
       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).
       
       FD  PROCFILE.
           COPY procfile.CPY IN "C:\Users\sid\cms\copylib\rri".

       
       WORKING-STORAGE SECTION.
       01  SAVE-PROCFILE01 PIC X(46).
       01  PROC-STAT PIC XX.
       01  O4 PIC X VALUE SPACE.
       01  PK01.
           02 PK1 PIC X(4).
           02 PK2 PIC X(7).
       01  O301.
           02 F5 PIC X(7) VALUE "       ".
           02 F6.
             03 F61 PIC X(4).
             03 FILLER PIC X VALUE SPACE.
             03 F62 PIC X(7).
           02 FILLER PIC X VALUE SPACE.
           02 F7 PIC X.
           02 FILLER PIC X VALUE SPACE.
           02 F8 PIC X(28).
           02 F9 PIC X VALUE " ".
           02 F10 PIC ZZ,ZZ9.99.
       01  O101.
           02 FILLER PIC X(15) VALUE SPACE.
           02 O1F1 PIC X(45).
           02 FILLER PIC X(7) VALUE SPACE.
           02 O1F2 PIC 999.
       01  O201.
           02 FILLER PIC X(21) VALUE SPACE.
           02 O2F1 PIC X(12).
           02 FILLER PIC XXX VALUE SPACES.
           02 O2F2 PIC X(18).
       01  NUM-2 PIC 99.
       01  RIGHT-2 PIC XX JUST RIGHT.
       01  RIGHT-4 PIC XXXX JUST RIGHT.
       01  ALF-7 PIC X(8).
       01  CENTS PIC XX.
       01  SIGN-DOLLAR PIC XXXX.
       01  X PIC 99.
       01  NEF-7 PIC ZZ,ZZ9.99.
       01  ANS PIC X(32).
       01  FUNC PIC X(8).
       01  ALF5 PIC X(5).
       01  ALF-6 PIC X(6).
       01  NUM-6 PIC 9(6).
       01  ALF4 PIC XXXX.
       01  ALF7 PIC X(7).
       01  ALF1 PIC X.
       01  NUM1 PIC 9.
       01  PROCTAB01.
           02 PROCTAB PIC X(11) OCCURS 9 TIMES.
       01  DATE-X.
           02 DD-YYYY PIC XXXX.
           02 DD-MM PIC XX.
           02 DD-DD PIC XX.
       PROCEDURE DIVISION.
       P0. 
           OPEN INPUT PROCFILE.
           OPEN OUTPUT FILEOUT.
           OPEN INPUT BILLPARM.
           READ BILLPARM AT END
               GO TO S1
           END-READ.    
       S1. 
           DISPLAY "FUNCTION ?"
           ACCEPT ANS
           
           IF ANS = "END"
               GO TO Z1
           END-IF

           IF ANS = "?"
               DISPLAY "A = ADD A PROCEDURE CODE"
               DISPLAY "C = CHANGE A PROCEDURE CODE"
               DISPLAY "D = DELETE A PROCEDURE CODE(BEWARE)"
               DISPLAY "L = LIST A SINGLE PROCEDURE CODE"
               DISPLAY "F = LIST 20 PROCEDURE CODES AT A TIME"
               DISPLAY "S = search by CPT"    
               DISPLAY "    STARTING WITH CODE AFTER THE ONE TYPED IN"
               DISPLAY "PFS = PRINT FEE SCHEDULE TO LINE PRINTER"
               DISPLAY "END = END THE PROGRAM"
               GO TO S1
           END-IF

           MOVE SPACES TO FUNC PROC-KEY
           UNSTRING ANS DELIMITED BY "," INTO FUNC PROC-KEY
           IF FUNC = "D" OR "A" OR "C" OR "L" OR "F" OR "S" OR "PFS"
               NEXT SENTENCE 
           ELSE 
               DISPLAY "WHAT?"
               GO TO S1
           END-IF.

           IF PROC-KEY = "1" OR "2" OR "3" OR "4" OR "5" OR "6"
                         OR "7" OR "8" OR "9"
               MOVE PROC-KEY TO ALF1
               MOVE ALF1 TO NUM1
               MOVE PROCTAB(NUM1) TO PROC-KEY
           END-IF
           
           IF FUNC = "L"
               PERFORM L1
               GO TO S1
           END-IF

           IF FUNC = "A"
               GO TO A1
           END-IF

           IF FUNC = "C"
               PERFORM L1
               GO TO C1
           END-IF

           IF FUNC = "D"
               PERFORM L1
               GO TO D1
           END-IF

           IF FUNC = "F"
               GO TO F1
           END-IF

           IF FUNC = "S"
               GO TO ST1
           END-IF    
              
           IF FUNC = "PFS"
               GO TO PFS1
           END-IF.    
       L1. 
           READ PROCFILE INVALID
               DISPLAY "NOT ON FILE"
               GO TO S1
           END-READ

           MOVE PROC-AMOUNT TO NEF-7.
           DISPLAY PROC-CDM " " PROC-CPT ":" PROC-MOD " " PROC-TYPE " "
                   PROC-TITLE " " NEF-7 " " PROC-KEY.
       F1. 
           MOVE 0 TO X.
           MOVE SPACE TO ANS.
           START PROCFILE KEY NOT < PROC-KEY INVALID
               DISPLAY "END OF FILE"
               GO TO S1
           END-START.
       F2. 
           READ PROCFILE NEXT AT END 
               DISPLAY "END OF FILE" 
               GO TO S1
           END-READ

           MOVE PROC-AMOUNT TO NEF-7.
           
           IF X > 8 
               ACCEPT ANS
               MOVE 0 TO X
           END-IF
           
           IF ANS NOT = SPACE
               GO TO S1
           END-IF

           ADD 1 TO X
           MOVE PROC-KEY TO PROCTAB(X).
           DISPLAY X " " PROC-CDM " " PROC-CPT ":" PROC-MOD " "
                   PROC-TYPE " " PROC-TITLE " " NEF-7 " " PROC-KEY.
           GO TO F2.
       A1. 
           READ PROCFILE INVALID
               GO TO A4
           END-READ

           DISPLAY "ALREADY ON FILE"
           PERFORM L1 
           GO TO S1.
       A4. 
           DISPLAY "ENTER THE TITLE FOR THE PROCEDURE".
           ACCEPT PROC-TITLE.
           IF PROC-TITLE = "?"
               DISPLAY "X = BACK TO FUNCTION; NO ADD"
               DISPLAY "TYPE IN UP TO 32 CHARACTER TITLE FOR"
               DISPLAY "PROCEDURE"
               DISPLAY "A DISPLAY OF THE FIRST 32 CHARACTERS WILL OCCUR"
               DISPLAY "FOR YOUR APPROVAL/DISAPPROVAL"
               GO TO A4
           END-IF

           IF PROC-TITLE = "X"
               DISPLAY "NO UPDATE"
               GO TO S1
           END-IF

           DISPLAY PROC-TITLE.
       A5. 
           DISPLAY "OK ?".
           ACCEPT ANS.
           IF ANS = "?"
               DISPLAY "TYPE Y TO ACCEPT THE TITLE"
               DISPLAY "TYPE N TO REJECT THIS TITLE"
               DISPLAY "AND RETURN TO ENTER A NEW TITLE"
               GO TO A5
           END-IF

           IF ANS = "N"
               GO TO A4
           END-IF

           IF ANS NOT = "Y"
               MOVE "?" TO ANS
               GO TO A5
           END-IF.    
       T1. 
           DISPLAY "TYPE OF SERVICE: 1 OR 5".
           ACCEPT PROC-TYPE.
           
           IF NOT (PROC-TYPE = "1" OR = "5")
               DISPLAY "TRY AGAIN PLEASE"
               GO TO T1
           END-IF.            
       A6. 
           DISPLAY "FEE ?".
           ACCEPT ALF-7.
           IF ALF-7 = "?"
               DISPLAY "BK = BACK TO TITLE"
               DISPLAY "X = BACK TO FUNCTION; NO UPDATE"
               DISPLAY "ENTER THE AMOUNT OF THE FEE"
               DISPLAY "EX: $78.00 = 78.00, OR 78 OR 78."
               DISPLAY "EX: 70.10 = 70.10  ONLY"
               GO TO A6
           END-IF

           IF ALF-7 = "BK" 
               GO TO A4
           END-IF

           MOVE SPACES TO SIGN-DOLLAR CENTS.
           UNSTRING ALF-7 DELIMITED BY "." INTO SIGN-DOLLAR CENTS.
           IF CENTS = SPACES 
               MOVE "00" TO CENTS
           END-IF

           IF CENTS NOT NUMERIC
               MOVE "?" TO ALF-7
               GO TO A6
           END-IF

           MOVE SPACES TO RIGHT-4.
           UNSTRING SIGN-DOLLAR DELIMITED BY " " INTO RIGHT-4
           INSPECT RIGHT-4 REPLACING LEADING " " BY "0"
           IF RIGHT-4 NOT NUMERIC 
               MOVE "?" TO ALF-7
               GO TO A6
           END-IF

           STRING RIGHT-4 CENTS DELIMITED BY SIZE INTO ALF-6
           MOVE ALF-6 TO NUM-6
           DIVIDE NUM-6 BY 100 GIVING PROC-AMOUNT.
           CLOSE PROCFILE
           OPEN I-O PROCFILE
           WRITE PROCFILE01 INVALID
               DISPLAY "RECORD WAS ADDED"
               DISPLAY "MOMENTS AGO BY ANOTHER USER"
               CLOSE PROCFILE
               OPEN INPUT PROCFILE    
               PERFORM L1
               GO TO S1
           END-WRITE

           CLOSE PROCFILE
           OPEN INPUT PROCFILE
           DISPLAY "RECORD ADDED"
           PERFORM L1
           GO TO S1.
       C1. 
           DISPLAY "FIELD # ?".
           ACCEPT ANS.
           IF ANS = "?"
               DISPLAY "VALID CODES ARE 1 = TITLE 2 = AMOUNT 3 = TYPE"
               DISPLAY " X = BACK TO FUNCTION"
               GO TO C1
           END-IF

           IF ANS = "X" 
               DISPLAY "NO UPDATE"
               GO TO S1
           END-IF

           MOVE SPACE TO RIGHT-2.
           UNSTRING ANS DELIMITED BY " " INTO RIGHT-2.
           INSPECT RIGHT-2 REPLACING LEADING " " BY "0".
           IF RIGHT-2 NOT NUMERIC DISPLAY "NOT NUMERIC" GO TO C1.
           MOVE RIGHT-2 TO NUM-2.
           IF NUM-2 < 1 OR > 3 DISPLAY "INVALID FIELD #" GO TO C1.

           GO TO 3F 4F 5F DEPENDING ON NUM-2.
       3F. 
           DISPLAY "NEW TITLE ?".
           ACCEPT PROC-TITLE.
           
           IF PROC-TITLE = "?"
               DISPLAY "X = BACK TO FUNCTION"
               DISPLAY "TYPE IN TITLE UP TO 32 CHARACTERS"
               GO TO 3F
           END-IF

           IF PROC-TITLE = "X"
               DISPLAY "NO UPDATE" 
               GO TO S1
           END-IF

           DISPLAY PROC-TITLE.
       3F1.
           DISPLAY "ACCEPT Y/N ?"
           ACCEPT ANS.
           
           IF ANS = "?"
               DISPLAY "Y = ACCEPT, N = RETRY, X = BACK TO FUNCTION"
               GO TO 3F1
           END-IF

           IF ANS = "X"
               DISPLAY "NO UPDATE" 
               GO TO S1
           END-IF

           IF ANS = "N"
               GO TO 3F
           END-IF

           IF ANS NOT = "Y" 
               GO TO 3F1
           END-IF

           GO TO R1.
       4F.
           DISPLAY "ENTER NEW FEE".
           ACCEPT ALF-7.
           IF ALF-7 = "?"
               DISPLAY "X = BACK TO TITLE"
               DISPLAY "X = BACK TO FUNCTION; NO UPDATE"
               DISPLAY "ENTER THE AMOUNT OF THE FEE"
               DISPLAY "EX: $78.00 = 78.00, OR 78 OR 78."
               DISPLAY "EX: 70.10 = 70.10  ONLY"
               GO TO 4F
           END-IF

           MOVE SPACES TO SIGN-DOLLAR CENTS
           UNSTRING ALF-7 DELIMITED BY "." INTO SIGN-DOLLAR CENTS.
           
           IF CENTS = SPACES
               MOVE "00" TO CENTS
           END-IF

           IF CENTS NOT NUMERIC
               MOVE "?" TO ALF-7
               GO TO 4F
           END-IF

           MOVE SPACES TO RIGHT-4.
           UNSTRING SIGN-DOLLAR DELIMITED BY " " INTO RIGHT-4
           INSPECT RIGHT-4 REPLACING LEADING " " BY "0"
           
           IF RIGHT-4 NOT NUMERIC 
               DISPLAY "?"
               GO TO 4F
           END-IF
           
           STRING RIGHT-4 CENTS DELIMITED BY SIZE INTO ALF-6
           MOVE ALF-6 TO NUM-6
           DIVIDE NUM-6 BY 100 GIVING PROC-AMOUNT.
      *    time to rewrite procfile
           GO TO R1.
       5F. 
           DISPLAY "TYPE 1 OR 5 OR X TO CANCEL".
           ACCEPT PROC-TYPE.
           
           IF PROC-TYPE = "X"
              DISPLAY "NO UPDATE" 
              GO TO S1
           END-IF
           
           IF NOT (PROC-TYPE = "1" OR = "5")
               DISPLAY "??? " PROC-TYPE
               GO TO 5F
           END-IF    
          
           GO TO R1.
       D1. 
           DISPLAY PROC-KEY " " PROC-TITLE.
           DISPLAY "DELETE Y/N ?".
           ACCEPT ANS.
           
           IF ANS NOT = "Y" 
               DISPLAY "NO DELETE" 
               GO TO S1
           END-IF    
               
           MOVE PROCFILE01 TO SAVE-PROCFILE01
           CLOSE PROCFILE
           OPEN I-O PROCFILE
           MOVE SAVE-PROCFILE01 TO PROCFILE01
           READ PROCFILE WITH LOCK INVALID
               IF PROC-STAT NOT = "00" 
                   DISPLAY PROC-STAT " RECORD NOT CHANGED"
                   CLOSE PROCFILE
                   OPEN INPUT PROCFILE
                   GO TO S1
               END-IF    
           END-READ
           DELETE PROCFILE RECORD
           DISPLAY "RECORD DELETED"
           GO TO S1.                               
       PFS1.
           ACCEPT DATE-X FROM DATE YYYYMMDD.
           DISPLAY "INCLUDE 0 AMOUNTS? Y"
           ACCEPT ALF1.
           MOVE SPACE TO O2F2
           STRING DD-MM "-" DD-DD"-" DD-YYYY DELIMITED BY SIZE 
           INTO O2F2.
           MOVE "0000" TO ALF4.
           MOVE 1 TO O1F2.
           MOVE BP-2 TO O1F1.
           MOVE "FEE SCHEDULE" TO O2F1.
           MOVE 0 TO X.
           PERFORM PFS3.
           MOVE SPACE TO PROC-KEY.
           START PROCFILE KEY > PROC-KEY INVALID GO TO S1.
       PFS2.
           READ PROCFILE NEXT AT END GO TO S1.
           IF (PROC-AMOUNT = 0) AND ALF1 NOT = "Y" GO TO PFS2.
           ADD 1 TO X.
           IF X > 50 ADD 1 TO O1F2
           PERFORM PFS3
           MOVE 1 TO X.
           MOVE PROC-KEY TO PK01.
           MOVE PK1 TO F61 MOVE PK2 TO F62.
           MOVE PROC-TYPE TO F7
           MOVE PROC-TITLE TO F8.
           MOVE PROC-AMOUNT TO F10.
           IF PK1 = ALF4  MOVE SPACE TO F61 ELSE 
           MOVE PK1 TO ALF4.
           WRITE FILEOUT01 FROM O301.
           GO TO PFS2.
       PFS3.
           MOVE SPACE TO FILEOUT01. WRITE FILEOUT01 AFTER PAGE.
           WRITE FILEOUT01 FROM O101
           WRITE FILEOUT01 FROM O201 AFTER 2.
       R1.
           MOVE PROCFILE01 TO SAVE-PROCFILE01
           CLOSE PROCFILE
           OPEN I-O PROCFILE

           READ PROCFILE WITH LOCK INVALID
               IF PROC-STAT NOT = "00" 
                   DISPLAY PROC-STAT " RECORD NOT CHANGED"
                   CLOSE PROCFILE
                   OPEN INPUT PROCFILE
                   GO TO S1
               END-IF    
           END-READ

           MOVE SAVE-PROCFILE01 TO PROCFILE01                     
           REWRITE PROCFILE01.
           DISPLAY "RECORD CHANGED"
           CLOSE PROCFILE
           OPEN INPUT PROCFILE
           GO TO S1.
       ST1.
           MOVE SPACE TO PROC-KEY
           DISPLAY "ENTER 5 DIGIT CPT CODE"
           ACCEPT ALF5

           IF ALF5(5:1) NOT NUMERIC
               DISPLAY "NOT A VALID CPT, TRY AGAIN"
               GO TO ST1
           END-IF

           START PROCFILE KEY NOT < PROC-KEY INVALID
               DISPLAY "END OF FILE"
               GO TO S1
           END-START
           MOVE 0 TO X.
       ST2.
           READ PROCFILE NEXT AT END 
               DISPLAY "END OF FILE" 
               GO TO S1
           END-READ               
              
           IF PROC-AMOUNT = 0
               GO TO ST2
           END-IF

           IF ALF5 NOT = PROC-KEY(5:5)
               GO TO ST2
           END-IF

           MOVE PROC-AMOUNT TO NEF-7   
           DISPLAY PROC-CDM " " PROC-CPT ":" PROC-MOD " "
                   PROC-TYPE " " PROC-TITLE " " NEF-7 " " PROC-KEY

           ADD 1 TO X
           IF X > 29
               ACCEPT OMITTED
               MOVE 0 TO X
           END-IF
           GO TO ST2.
            
       Z1.
           CLOSE PROCFILE FILEOUT BILLPARM.
           STOP RUN.
