      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cci006.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CCIFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC RECORD KEY IS CCI-KEY
               LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.

       FD  CCIFILE.
           COPY ccifile.CPY IN "C:\Users\sid\cms\copylib".                           
       
       WORKING-STORAGE SECTION.
       01  ANS PIC XXX.
       01  X PIC 99.
       01  ALF2 PIC XX.
       01  ALF11 PIC X(11).
       01  ALF11X PIC X(11).
       01  RIGHT-2 PIC XX JUST RIGHT.
       01  NUM2 PIC 99.

       01  CCI-TB01.
           02 CCI-TAB PIC X(11) OCCURS 20 TIMES.
       
       PROCEDURE DIVISION.
       
       0005-START.
           OPEN INPUT CCIFILE.
       
       P1.
           DISPLAY "OPTION ?".
           ACCEPT ANS

           IF ANS ="?"
             DISPLAY "E = END"
             DISPLAY "F = SEARCH"
             DISPLAY "A = ADD"
             DISPLAY "D = DELETE"
             GO TO P1
           END-IF

           IF NOT (ANS = "E" OR "F" OR "A" OR "D")
             DISPLAY "BAD CHOICE"
             GO TO P1
           END-IF

           IF ANS = "E" GO TO P2.
           
           IF ANS = "F" GO TO FIND-1.
           
           IF ANS = "A" GO TO ADD-1.
           
           IF ANS = "D" GO TO DEL-1.
           
           GO TO P1.

       FIND-1.
           DISPLAY "ENTER ANY PART OF CODE1"
           ACCEPT CCI-key1

           IF CCI-key1 = "?"
               DISPLAY "X = BACK TO OPTION"
               GO TO FIND-1
           END-IF

           MOVE SPACE TO CCI-key2
           START CCIFILE KEY NOT < CCI-KEY
             INVALID
               DISPLAY "NO RECORDS"
               GO TO FIND-1
           END-START
           
           MOVE 0 TO X.

       FIND-2.
           READ CCIFILE NEXT
             AT END
               DISPLAY "END OF FILE"
               GO TO P1
           END-READ
           
           ADD 1 TO X
           DISPLAY X " " CCI-KEY " " CCI-ind
           MOVE CCIFILE01 TO CCI-TAB(X)
           
           IF X < 20
               GO TO FIND-2
           END-IF

           ACCEPT ANS
           
           IF ANS NOT = SPACE
               GO TO P1
           END-IF
           
           MOVE 0 TO X
           
           GO TO FIND-2.

       ADD-1.
           DISPLAY "TYPE ALL 11 CHARACTERS: NO SPACES"
           ACCEPT CCIFILE01

           IF CCIFILE01 NOT NUMERIC
               DISPLAY "WRONG FORMAT TRY AGAIN"
               GO TO ADD-1
           END-IF
           
           IF NOT (CCIFILE01(11:1) = "0" OR "1" OR "9")
               DISPLAY "WRONG INDICATOR VALUE"
               GO TO ADD-1
           END-IF
           
           CLOSE CCIFILE
           OPEN I-O CCIFILE

           WRITE CCIFILE01
             INVALID
               DISPLAY "ALREADY EXISTS"
               DISPLAY "DELETE FIRST, THEN ADD"
               DISPLAY "IF YOU ARE TRYING TO CHANGE"
               DISPLAY "ONLY THE INDICATOR VALUE"
             NOT invalid
               DISPLAY "RECORD ADDED " CCIFILE01
           END-WRITE

           CLOSE CCIFILE
           OPEN INPUT CCIFILE
           
           GO TO P1.

       DEL-1.
           DISPLAY "SELECT FROM THE LIST FOUND"
           DISPLAY "USING THE F COMMAND"
           DISPLAY "OR TYPE IN THE 10 DIGITS"
           DISPLAY "ON THE 2 CPT CODES"
           ACCEPT ALF11

           IF ALF11 = "X"
               GO TO P1
           END-IF                       
           
           MOVE SPACE TO RIGHT-2 ALF11X

           UNSTRING ALF11(1:2) DELIMITED BY " " INTO RIGHT-2
           INSPECT RIGHT-2 REPLACING LEADING " " BY "0"
           
           IF RIGHT-2 NOT NUMERIC
             DISPLAY RIGHT-2 " IS A BAD SELECTION FROM THE LIST"
               GO TO DEL-1
           END-IF

           MOVE RIGHT-2 TO NUM2
           MOVE CCI-TAB(NUM2) TO CCI-KEY
           DISPLAY CCIFILE01
           
           READ CCIFILE INVALID
               DISPLAY " NO SUCH RECORD"
               GO TO DEL-1
           END-READ

           DISPLAY "OKAY TO DELETE Y,N?"
           ACCEPT ANS
             
           IF ANS NOT = "Y"
             GO TO DEL-1
           END-IF

           CLOSE CCIFILE
           OPEN I-O CCIFILE
           DELETE CCIFILE RECORD
           CLOSE CCIFILE           
           DISPLAY " RECORD DELETED"
           OPEN INPUT CCIFILE  
           
           GO TO P1.

       P2. 
           CLOSE CCIFILE
           STOP RUN.
