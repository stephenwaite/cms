      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. hosp001.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *    SPECIAL-NAMES.
      *    "OPCOM" IS OPCOM.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT HOSPFILE ASSIGN TO "S30" ORGANIZATION INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS HOSP-KEY
             ALTERNATE RECORD KEY IS H-INS-KEY WITH DUPLICATES
             ALTERNATE RECORD KEY IS H-INS-NAME WITH DUPLICATES.
 
           SELECT INSFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS INS-KEY
             ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
             ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
             ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
             ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
             ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
             ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES.                          

       DATA DIVISION.

       FILE SECTION.

       FD  HOSPFILE
           DATA RECORD IS HOSPFILE01.
       01  HOSPFILE01.
           02 HOSP-KEY PIC X(5).
           02 H-INS-KEY PIC XXX.
           02 H-INS-NAME PIC X(18).

       FD  INSFILE.
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

       WORKING-STORAGE SECTION.
       01  ANS          PIC XXX.
       01  ACTION.
           02 ACT-1 PIC X.
           02 ACT-2 PIC XX.
       01  DATAIN   PIC X(30).
       01  HOSPSTAT PIC XX.
       01  STATX    PIC XXX VALUE "O-K".
       01  IN-FIELD.          
           03  IN-FIELD-20.
             04  IN-FIELD-15.
               05 IN-FIELD-10.
                 06  IN-FIELD-9.
                   07  IN-FIELD-8.
                     08  IN-FIELD-7.
                       09  IN-FIELD-6.
                         10  IN-FIELD-5.
                           11  IN-FIELD-4.
                             12  IN-FIELD-3.
                               13  IN-FIELD-2.
                                 14  IN-FIELD-1 PIC X.
                                 14  FILLER  PIC X.
                               13  FILLER  PIC X.
                             12  FILLER  PIC X.
                           11  FILLER  PIC X.
                         10  FILLER  PIC X.
                       09  FILLER  PIC X.
                     08 FILLER  PIC X.
                   07 FILLER  PIC X.
                 06 FILLER  PIC X.
               05 FILLER  PIC X(5).
             04 FILLER  PIC X(5).
           03 FILLER  PIC X(5).
           02 FILLER  PIC X(16).

       01  IN-FIELD-TAB01 REDEFINES IN-FIELD.
           02 IN-FIELD-TAB   PIC X OCCURS 41 TIMES.
       
       01  Q           PIC 99 VALUE ZERO.

       01  FIELD-CODE  PIC XX JUST RIGHT.

       01  HOSP-LEN-TAB01-RE.
           02 FILLER PIC X(6) VALUE "050318".

       01  HOSP-LEN-TAB01 REDEFINES HOSP-LEN-TAB01-RE.
           02 HOSP-LEN-TAB PIC 99 OCCURS 3 TIMES.

       01  HOSP-GD-TABLE.
           05 HOSP-DES-CONSTANT.
               10 FILLER PIC X(24) VALUE "        INS-KEY TITLE   ".
           05 HOSP-DESC-FLD REDEFINES HOSP-DES-CONSTANT 
                OCCURS 3 TIMES INDEXED BY HOSP-INDX.
               10 HOSP-DES-KEY PIC X(8).
       01  HOSP-ADD-FIELD-TABLE.
             05 HOSP-ADD-CON PIC X(18) VALUE "010203040506070809".
             05 HOSP-A-FLD PIC 99 REDEFINES HOSP-ADD-CON 
                OCCURS 9 TIMES INDEXED BY HOSP-A-KEY.
       01     NAME-LAST.
             03 NL-3.
               04 NL-31 PIC X.
               04 NL-32 PIC X.
               04 NL-33 PIC X.
             03 FILLER PIC X(21).
       01     NAME-TEST.
             03 NT-3.
               04 NT-31 PIC X.
               04 NT-32 PIC X.
               04 NT-33 PIC X.
             03 NT-21 PIC X(21).
       01  NAME-FIRST          PIC X(24).
       01  NAME-MIDDLE         PIC X(24).
       01  STATE-TABLE-CONSTANT.
           05  FILLER   PIC X(26) VALUE "AKALARAZCACNCOCTDCDEFLGAHI".
           05  FILLER   PIC X(24) VALUE "IAIDILINKSKYLAMAMDMEMIMN".
           05  FILLER   PIC X(24) VALUE "MOMSMTNCNDNENHNJNMNVNYOH".
           05  FILLER   PIC X(24) VALUE "OKORPAPRRISCSDTNTXUTVAVT".
           05  FILLER   PIC X(8)  VALUE "WAWIWVWY".
       01  STATE-TABLE REDEFINES STATE-TABLE-CONSTANT.
           05  STATE-2  PIC XX OCCURS 53 TIMES ASCENDING KEY STATE-2
                                         INDEXED BY S-2. 
       01  INPUT-DATE.
           05  T-MM          PIC 99.
           05  T-DD          PIC 99.
           05  T-YY          PIC 99.
           05  T-CC          PIC X.
       01  TEST-DATE.
           05  T-CC          PIC X.
           05  T-YY            PIC 99.
           05  T-MM            PIC 99.
           05  T-DD            PIC 99.
       01  INPUT-DATE-S.
           05 T-MM  PIC 99.
           05 T-DD  PIC 99.
           05 T-YY  PIC 99.
       01  TEST-DATE-S.
           05 T-YY  PIC 99.
           05 T-MM  PIC 99.
           05 T-DD  PIC 99.
       01  DISPLAY-DATE.
           02 DD-MM PIC XX.
           02 FILLER PIC X VALUE "/".
           02 DD-DD PIC XX.
           02 FILLER PIC X VALUE "/".
           02 DD-YY PIC XX.
       01  MONTH-TABLE-CONS.
           05  FILLER PIC X(24) VALUE "312931303130313130313031".
       01  MONTH-TABLE REDEFINES MONTH-TABLE-CONS.
           05  DAYS-IN-MONTH OCCURS 12 TIMES PIC 99.

       01 NUM-EDIT-FIELDS.
           02  NEF-1    PIC ZZ.9  .
           02  NEF-2    PIC ZZZ.9  .
           02  NEF-3    PIC ZZZ.99  .
           02  NEF-4    PIC Z,ZZZ.99  .
           02  NEF-5    PIC Z,ZZZ.99-.
           02 NEF-A PIC Z,ZZZ.99-.
           02 NEF-B PIC Z,ZZZ.99-.
           02 NEF-C PIC Z,ZZZ.99-.
           02 NEF-D PIC ZZ,ZZZ.99CR.

       01  HOSPBACK PIC X(185).
       01     TOWN-FLAG PIC 9.
       01     BUSNAME.
           03 BUSNAME1 PIC X.
           03 BUSNAME23 PIC X(23).
       01     RIGHT-2 PIC XX JUST RIGHT.
       01     ALF-1 PIC X.
       01     XALF-1 PIC X.
       01     ALF4 PIC X(4).
       01     ALF-5 PIC X(5).
       01     ALF-7 PIC X(7).
       01     ALF-13 PIC X(13).
       01     ALF-14 PIC X(14).
       01     PART24.
             03 THREEPARTID PIC XXX.
             03 FILLER PIC X(21).
       01     PART24TEST.
             03 TEST3 PIC XXX.
             03 FILLER PIC X(21).
       01     SIXPARTID PIC X(6).
       01     EIGHTPARTID PIC X(8).
       01     YEARDAY.
             03 YEAR-1.
               04 YD1 PIC X.
               04 YD2 PIC X.
             03 DAY3 PIC XXX.
       01     ABC PIC X.
       01     XYZ PIC 9.
       01     RIGHT-4 PIC X(4) JUST RIGHT.
       01     SIGN-DOLLAR PIC X(4).
       01     CENTS PIC XX.
       01  NUM-6 PIC 9(6).
       01  ALF-6 PIC X(6).
       01  ALF-8 PIC X(8).
       01  ALF-10.
           02 ALF-10-1 PIC XXX.
           02 ALF-10-2 PIC X(7).
       01  RIGHT-3 PIC XXX JUST RIGHT.
       01  RIGHT-5 PIC X(5) JUST RIGHT.
       01     RIGHT-7 PIC X(7) JUST RIGHT.
       01    NUM-1 PIC 9.
       01     NUM-3 PIC 999.
       01     NUM-9 PIC 9(9).
       01     NUM-7 PIC S9(7).
       01     A PIC 99.
       01     B PIC 99.
       01     C PIC 99.
       01     D PIC 99.
       01     X PIC 99.
       01     Y PIC 99.
       01  Z PIC 999.
       01     FLAG   PIC 9.
       01  FLAGX PIC 9.
       01     WORK1  PIC X(20) JUST RIGHT.
       01     NAME-CYCLE.
             03 NC1 PIC X.
             03 NC2 PIC X.
             03 FILLER PIC X(22).
       01  TEMP-FIELD01 PIC X(75).
       01  LLTAB2401.
           02 LLTAB24 PIC X OCCURS 24 TIMES.
       01  FFTAB1001.
           02 FFTAB10 PIC X OCCURS 10 TIMES.
       01  FF PIC 99.
       01  LL PIC 99.
       01  XLLTAB2401.
           02 XLLTAB24 PIC X OCCURS 24 TIMES.
       01  XFFTAB1001.
           02 XFFTAB10 PIC X OCCURS 10 TIMES.
       01  NP PIC S9 VALUE 0.
       01  GAR-TAB01.
           02 GAR-TAB PIC X(5) OCCURS 9 TIMES.
       01  ALF20 PIC X(20).
       01  ALF20A PIC X(20).
       01  GARNO PIC X(5).
       01  TABKEY01.
           02 TABKEY PIC X(8) OCCURS 9 TIMES.
       01  SAVECODE PIC XXX VALUE SPACE.
       01  NUM2 PIC 99.

       PROCEDURE DIVISION.

       0005-START.
           OPEN I-O HOSPFILE.
           OPEN INPUT INSFILE.

       1000-ACTION.
           MOVE SPACES TO ACTION GARNO
           DISPLAY "OPTION,ID HOSPINS"
           ACCEPT DATAIN
           
           IF DATAIN = "END"
             GO TO 9100-CLOSE-MASTER-FILE
           END-IF

           IF DATAIN = "?"
             DISPLAY "INSURE. RTNE. <LI,AI,CI,FI>,<INS-CODE>"
             DISPLAY "END = END PROGRAM"
             GO TO 1000-ACTION
           END-IF

           UNSTRING DATAIN DELIMITED BY "," INTO ACTION GARNO

           IF GARNO = "1" OR "2" OR "3" OR "4" OR "5" OR "6"
             OR "7" OR "8" OR "9"
             MOVE GARNO TO ALF-1
             MOVE ALF-1 TO NUM-1
             MOVE GAR-TAB(NUM-1) TO GARNO.

           IF ACTION = "LI" OR "AI" OR "CI" OR "FI"  
             NEXT SENTENCE
           ELSE
             DISPLAY "WHAT?"
             GO TO 1000-ACTION.

           IF ACTION = "AI" 
             GO TO 2000-AI.

           IF ACTION = "FI" OR "LI" OR "CI" 
             GO TO HOSP-LOOK.
           
       HOSP-LOOK.
           IF ACTION = "FI" 
             PERFORM HOSP-1 THRU HOSP-1-EXIT
             GO TO 1000-ACTION.
           
           MOVE GARNO TO HOSP-KEY
           READ HOSPFILE WITH LOCK
             INVALID
               DISPLAY "INVALID"
               GO TO 1000-ACTION.

           IF HOSPSTAT = "61"
             DISPLAY "RECORD BEING USED"
             GO TO 1000-ACTION.
           
           IF ACTION = "LI" 
             PERFORM LI-1
             UNLOCK HOSPFILE RECORD
             GO TO 1000-ACTION.

           GO TO HOSP-CP.
      * BEGIN HOSPURANCE FILE MAINTAINENCE ROUTINE *

       2000-AI.
           MOVE SPACE TO HOSPFILE01
           PERFORM HOSP-ADD-LOOP VARYING HOSP-A-KEY FROM 1 BY 1
             UNTIL HOSP-A-KEY > 9

           IF IN-FIELD = "X"
             DISPLAY "NO UPDATE"
             GO TO 1000-ACTION.

           WRITE HOSPFILE01
             INVALID KEY
               DISPLAY "NO UPDATE"
               DISPLAY "SHOULD NOT HAVE HAPPENED! CONTACT DATA CENTER."
               DISPLAY "THIS PROGRAM HAS BEEN TERMINATED"
               GO TO 9100-CLOSE-MASTER-FILE.

           DISPLAY HOSP-KEY " " H-INS-KEY " " H-INS-NAME.
           DISPLAY "RECORD IS ADDED".
           GO TO 1000-ACTION.

       HOSP-ADD-LOOP.
           SET HOSP-INDX TO HOSP-A-FLD(HOSP-A-KEY).
           PERFORM HOSP-DISPLAY THRU HOSPDEEX.

       HOSP-CP.
           DISPLAY "FIELD CODE,DATA? HOSP.".
           ACCEPT DATAIN.
           IF DATAIN = "X"
             DISPLAY "NO CHANGE"
             GO TO 1000-ACTION.

           IF DATAIN = "UP"
             GO TO 5000-WRITE-HOSPFILE.

           IF DATAIN = "LI"
             PERFORM LI-1 THRU LI-1-EXIT
             GO TO HOSP-CP.

           IF DATAIN = "?"
             DISPLAY "ENTER A FIELD CODE <COMMA> AND NEW DATA."
             DISPLAY "UP = UPDATE CHANGES"
             DISPLAY " X = NO CHANGE"
             DISPLAY "LI = LIST "
             DISPLAY "FIELD CODES: 2=INS-KEY 3=INS-NAME"
             GO TO HOSP-CP.

           MOVE SPACE TO FIELD-CODE IN-FIELD
           UNSTRING DATAIN DELIMITED BY "," INTO FIELD-CODE IN-FIELD.
           INSPECT FIELD-CODE REPLACING LEADING SPACE BY "0".

           IF FIELD-CODE NOT NUMERIC
             DISPLAY "FIELD-CODE MUST BE 2 DIGITS AND NUMERIC"
             GO TO HOSP-CP.

           IF FIELD-CODE = "00"
             DISPLAY "FIELD CODE CANNOT BE ZERO"
             GO TO HOSP-CP.

           IF (FIELD-CODE = "02" OR "03")
             MOVE FIELD-CODE TO NUM2
             SET HOSP-INDX TO NUM2
             GO TO HOSP-GO-TO.
           
           DISPLAY "FIELD # MUST BE 2 OR 3"
           GO TO HOSP-CP.

       HOSP-DISPLAY.
           DISPLAY HOSP-DESC-FLD(HOSP-INDX) "?".

       HOSP-INPUT.
           MOVE SPACE TO IN-FIELD
           ACCEPT IN-FIELD.
           IF IN-FIELD = "??"
             DISPLAY "LI = LIST HOSPFILE RECORD BUILT SO FAR"
             DISPLAY "X = CANCEL ADD OF THIS RECORD"
             DISPLAY "BK = BACK TO PREVIOUS PROMPT"
             DISPLAY "? = DATA NEEDED FOR PROMPT"
             DISPLAY "OR ENTER DATA NEEDED"
             GO TO HOSP-DISPLAY.
             
           IF IN-FIELD = "BK" AND HOSP-A-KEY = 1 MOVE "X" TO IN-FIELD
             SET HOSP-A-KEY TO 10 GO TO HOSPDEEX.

           IF IN-FIELD = "BK" GO TO HOSP-BACK.

           IF IN-FIELD = "LI" PERFORM LI-1 GO TO HOSP-DISPLAY.

           IF IN-FIELD = "X" SET HOSP-A-KEY TO 9
             GO TO HOSPDEEX.

           GO TO HOSP-GO-TO.

       HOSP-BACK.
           SET HOSP-A-KEY DOWN BY 1
           SET HOSP-INDX TO HOSP-A-FLD(HOSP-A-KEY)
           GO TO HOSP-DISPLAY.

       HOSP-GO-TO.
           MOVE 0 TO FLAG.
           MOVE HOSP-LEN-TAB(HOSP-INDX) TO Q
           ADD 1 TO Q.

           IF IN-FIELD-TAB(Q) NOT = " "
             MOVE 1 TO FLAG.
           
           IF FLAG = 1
             DISPLAY "DATA TOO LONG, MUST NOT BE GREATER "
               "THAN " HOSP-LEN-TAB(HOSP-INDX) ".".

           IF FLAG = 1 AND ACT-1 = "C" GO TO HOSP-CP.

           IF FLAG = 1 AND ACT-1 = "A" GO TO HOSP-DISPLAY.

           GO TO HOSP-CP 2-HOSP 3-HOSP DEPENDING ON HOSP-INDX.   
                        
       2-HOSP.
           IF IN-FIELD = "?" 
             DISPLAY "ENTER A 3-DIGIT NUMBER OR F TO FIND ONE"
             GO TO HOSP-TI.
    
           IF IN-FIELD = "F"
             PERFORM INS-1 THRU INS-1-EXIT
             GO TO HOSP-TI.
        
           MOVE SPACE TO RIGHT-3
           UNSTRING IN-FIELD-3 DELIMITED BY " " INTO RIGHT-3
           INSPECT RIGHT-3 REPLACING LEADING " " BY "0"
         
           IF (RIGHT-3 NOT NUMERIC) OR (RIGHT-3 < "001")
             DISPLAY "INVALID NUMBER"
             GO TO HOSP-TI.

           MOVE RIGHT-3 TO INS-KEY
           READ INSFILE
             INVALID
               DISPLAY "BAD CODE" 
               GO TO HOSP-TI.

           DISPLAY INS-NAME
           MOVE RIGHT-3 TO H-INS-KEY
           GO TO HOSPDEE.       
 
       3-HOSP.
           IF IN-FIELD = "?"
              DISPLAY "ENTER NAME OF THE INSURANCE"
              DISPLAY "ON PERSONAL STATEMENTS AND CLAIM FORMS"
              GO TO HOSP-TI.
              
           IF IN-FIELD-1 = SPACE OR IN-FIELD = SPACE
             DISPLAY "LEADING BLANKS OR A BLANK NAME IS NOT ALLOWED"
             GO TO HOSP-TI.
           
           MOVE IN-FIELD TO H-INS-NAME
           GO TO HOSPDEE.           

       HOSP-TI.
           IF ACT-1 = "A" GO TO HOSP-DISPLAY.
           
           IF ACT-1 = "C" GO TO HOSP-CP.
           
           IF ACT-1 = "S" OR "F" GO TO 1000-ACTION.
      
       HOSPDEE. 
           IF ACT-1 = "C"
             GO TO HOSP-CP.

       HOSPDEEX.
           EXIT.

       5000-WRITE-HOSPFILE.
           REWRITE HOSPFILE01 
             INVALID KEY
               DISPLAY "NO UPDATE."
               DISPLAY "SHOULD NOT HAPPEN! CONTACT THE DATA CENTER."
               DISPLAY "THIS PROGRAM IS TERMINATED!"
               GO TO 9100-CLOSE-MASTER-FILE.
           
           UNLOCK HOSPFILE RECORD
           DISPLAY "UPDATE MADE"
           GO TO 1000-ACTION.

       LI-1.
           DISPLAY HOSP-KEY " " H-INS-KEY " " H-INS-NAME.
           
       LI-1-EXIT.
           EXIT.

       HOSP-1.
           DISPLAY "HOSPFILE SEARCH"
           DISPLAY "1 = BY RRMC HOSP CODE"
           DISPLAY "2 = BY RRI INS CODE"
           DISPLAY "3 = BY HOSPFILE NAME".

       HOSP-2.
           ACCEPT IN-FIELD-1.
           IF IN-FIELD-1 = "?"
             DISPLAY "X = BACK OR 1-3 SEARCH METHOD"
             GO TO INS-2.
           
           IF IN-FIELD-1 = "X"
             DISPLAY "END SEARCH"
             GO TO HOSP-1-EXIT.

           IF IN-FIELD-1 = "1" OR "2" OR "3"
             NEXT SENTENCE
           ELSE
             DISPLAY "BAD"
             GO TO HOSP-2.

           MOVE IN-FIELD-1 TO ALF-1.

       HOSP-3.
           DISPLAY "STARTING POINT?".
           ACCEPT IN-FIELD-5.
           IF IN-FIELD-5 = "?"
             DISPLAY "ENTER WHERE TO START LOOKING"
             DISPLAY "BK = BACK TO SEARCH METHOD"
             DISPLAY "OR X = BACK TO OPTION"
             GO TO HOSP-3.

           IF IN-FIELD-5 = "X" GO TO HOSP-1-EXIT.

           IF IN-FIELD-5 = "BK" GO TO HOSP-1.

           MOVE 0 TO X

           IF ALF-1 = "1" 
             MOVE IN-FIELD-5 TO HOSP-KEY
             MOVE "RRMC    " TO ALF-8
             START HOSPFILE KEY NOT < HOSP-KEY
               INVALID
                 GO TO HOSP-3
             END-START
             GO TO HOSP-4
           END-IF

           IF ALF-1 = "2" 
            MOVE IN-FIELD-5 TO H-INS-KEY
            MOVE "RRI INS " TO ALF-8
             START HOSPFILE KEY NOT < H-INS-KEY
               INVALID
                 GO TO HOSP-3
             END-START
            GO TO HOSP-4
           END-IF
           
           IF ALF-1 = "3"  
            MOVE IN-FIELD-5 TO H-INS-NAME
            MOVE "NAME    " TO ALF-8
             START HOSPFILE KEY NOT < H-INS-NAME
               INVALID
                 GO TO HOSP-3
             END-START
             GO TO HOSP-4
           END-IF
                      
           GO TO HOSP-3.

       HOSP-4.
           READ HOSPFILE NEXT
             AT END
               DISPLAY "END OF FILE"
               GO TO HOSP-1-EXIT.

           ADD 1 TO X
           MOVE HOSP-KEY TO GAR-TAB(X)
           DISPLAY X " " HOSP-KEY " " H-INS-KEY " " H-INS-NAME             

           IF X = 9
             MOVE 0 TO X
             DISPLAY "BY " ALF-8
             ACCEPT ANS
             IF ANS NOT = SPACE
               GO TO HOSP-1-EXIT.
             GO TO HOSP-4.

       HOSP-1-EXIT.
           EXIT.

       INS-1.
           DISPLAY "INSFILE SEARCH TYPE"
           DISPLAY "1 = BY NUMBER "
           DISPLAY "2 = BY NAME"
           DISPLAY "3 = BY ASSIGNMENT CODE"
           DISPLAY "4 = BY NEIC CODE"
           DISPLAY "5 = BY NEIC ASSIGNM CODE"
           DISPLAY "6 = BY CLAIM-TYPE"
           DISPLAY "7 = BY CITY".

       INS-2.
           ACCEPT IN-FIELD.
           
           IF IN-FIELD = "?"
             DISPLAY "X = BACK OR 1-6 SEARCH METHOD"
             GO TO INS-2.
           
           IF IN-FIELD = "X" DISPLAY "END SEARCH" GO TO INS-1-EXIT.
           
           IF IN-FIELD = "1" OR "2" OR "3" OR "4" OR "5" OR "6" OR "7"
             NEXT SENTENCE
           ELSE
             DISPLAY "BAD"
             GO TO INS-2.

           MOVE IN-FIELD-1 TO ALF-1.

       INS-3.
           DISPLAY "STARTING POINT?".
           ACCEPT IN-FIELD.
           IF IN-FIELD = "?"
             DISPLAY "ENTER WHERE TO START LOOKING"
             DISPLAY "BK = BACK TO SEARCH METHOD"
             DISPLAY "OR X = BACK TO OPTION"
             GO TO INS-3.

           IF IN-FIELD = "X" GO TO INS-1-EXIT.

           IF IN-FIELD = "BK" GO TO INS-1.

           MOVE 0 TO X
           IF ALF-1 = "1"
             MOVE SPACE TO RIGHT-3
             UNSTRING IN-FIELD DELIMITED BY " " INTO RIGHT-3
             INSPECT RIGHT-3 REPLACING LEADING " " BY "0"
             MOVE RIGHT-3 TO INS-KEY
             MOVE "NUMBER  " TO ALF-8
             START INSFILE KEY NOT < INS-KEY
               INVALID
                 GO TO INS-1-END.

           IF ALF-1 = "2"
             MOVE IN-FIELD TO INS-NAME
             MOVE "NAME    " TO ALF-8
             START INSFILE KEY NOT < INS-NAME
               INVALID
                 GO TO INS-1-END.

           IF ALF-1 = "3"
             MOVE IN-FIELD TO INS-ASSIGN
             MOVE "ASSIGNMT" TO ALF-8
             START INSFILE KEY NOT < INS-ASSIGN
               INVALID
                 GO TO INS-1-END.

           IF ALF-1 = "4" 
             MOVE IN-FIELD TO INS-NEIC
             MOVE "NEICCODE" TO ALF-8
             START INSFILE KEY NOT < INS-NEIC
               INVALID
                 GO TO INS-1-END.

           IF ALF-1 = "5"
             MOVE IN-FIELD TO INS-NEIC-ASSIGN
             MOVE "NEICASGM" TO ALF-8
             START INSFILE KEY NOT < INS-NEIC-ASSIGN
               INVALID 
               GO TO INS-1-END.

           IF ALF-1 = "6" 
             MOVE IN-FIELD TO INS-CLAIMTYPE
             MOVE "CLM-TYPE" TO ALF-8
             START INSFILE KEY NOT < INS-CLAIMTYPE
               INVALID 
               GO TO INS-1-END.

           IF ALF-1 = "7" 
             MOVE IN-FIELD TO INS-CITY
             MOVE "CITY    " TO ALF-8
             START INSFILE KEY NOT < INS-CITY
               INVALID 
               GO TO INS-1-END.

       INS-4. 
           READ INSFILE NEXT
             AT END
               DISPLAY "END OF FILE"
               GO TO INS-1-EXIT.

           DISPLAY INS-KEY " " INS-NAME " " INS-STREET " " INS-CITY
           " " INS-STATE " " INS-ZIP
           ADD 1 TO X
           IF X > 8 
             MOVE 0 TO X
             DISPLAY "BY " ALF-8
             ACCEPT ANS
             IF ANS NOT = SPACE
               GO TO INS-1-EXIT.               
             GO TO INS-4.

       INS-1-END.
           DISPLAY "END OF FILE".

       INS-1-EXIT.
            EXIT.

       
       9100-CLOSE-MASTER-FILE.
           CLOSE HOSPFILE INSFILE.
           DISPLAY "RRMC INSURANCE MAPPING ROUTINE HAS ENDED".
           STOP RUN.

