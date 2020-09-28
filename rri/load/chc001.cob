      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. chc001.
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
           ALTERNATE RECORD KEY IS HOSP-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS HOSP-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS HOSP-STATE WITH DUPLICATES
           ALTERNATE RECORD KEY IS HOSP-INS-KEY WITH DUPLICATES
           ALTERNATE RECORD KEY IS HOSP-GAP WITH DUPLICATES
           STATUS IS HOSPSTAT.
           SELECT INSFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES.
           SELECT GAPFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS GAPKEY
           ALTERNATE RECORD KEY IS GAP-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-STATE WITH DUPLICATES
           LOCK MODE IS MANUAL.
       DATA DIVISION.
       FILE SECTION.
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
       FD  HOSPFILE.
       01  HOSPFILE01.   
           02 HOSP-KEY  PIC X(12).
           02 HOSP-NAME PIC X(40).
           02 HOSP-BOX PIC X(40).
           02 HOSP-STREET PIC X(40).
           02 HOSP-CITY PIC X(20).
           02 HOSP-STATE PIC XX.
           02 HOSP-ZIP PIC X(9).
           02 HOSP-PHONE PIC X(10).
           02 HOSP-INS-KEY PIC XXX.
           02 HOSP-GAP PIC X(7).
           02 HOSP-FUTURE PIC X.
       WORKING-STORAGE SECTION.
       01  ANS          PIC XXX.
       01  ACTION.
           02 ACT-1 PIC X.
           02 ACT-2 PIC XX.
       01  DATAIN       PIC X(30).
       01  HOSPSTAT PIC XX.
       01  STATX               PIC XXX VALUE "O-K".
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
                             14  IN-FIELD-1  PIC X.
                             14  FILLER PIC X.
                           13  FILLER  PIC X.
                         12  FILLER    PIC X.
                       11  FILLER      PIC X.
                     10  FILLER        PIC X.
                   09  FILLER          PIC X.
                 08 FILLER  PIC X.
               07 FILLER PIC X.
             06 FILLER PIC X.
            05  FILLER              PIC X(5).
            04    FILLER                PIC X(5).
           03  FILLER                  PIC X(5).
           02  FILLER                PIC X(16).
       01 IN-FIELD-TAB01 REDEFINES IN-FIELD.
           02 IN-FIELD-TAB   PIC X OCCURS 41 TIMES.
       01  Q           PIC 99 VALUE ZERO.
       01  FIELD-CODE          PIC XX JUST RIGHT.
       01 HOSP-LEN-TAB01-RE.
           02 FILLER PIC X(18) VALUE "124040402002091009".
       01 HOSP-LEN-TAB01 REDEFINES HOSP-LEN-TAB01-RE.
           02 HOSP-LEN-TAB   PIC 99 OCCURS 9 TIMES.
       01  HOSP-GD-TABLE.
           05  HOSP-DES-CONSTANT.
               10 FILLER PIC X(24) VALUE "CHC-CODECHC-NAMECHC-BOX ".
               10 FILLER PIC X(24) VALUE "CHC-STRTCHC-CITYCHC-STAT".
               10 FILLER PIC X(24) VALUE "CHC-ZIP RRI-INS GAPCODE ".
           05 HOSP-DESC-FLD REDEFINES HOSP-DES-CONSTANT 
              OCCURS 9 TIMES INDEXED BY HOSP-INDX.
               10 HOSP-DES-KEY  PIC X(8).
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
       01 TEMP-FIELD01 PIC X(75).
       01 LLTAB2401.
           02 LLTAB24 PIC X OCCURS 24 TIMES.
       01 FFTAB1001.
           02 FFTAB10 PIC X OCCURS 10 TIMES.
       01  FF PIC 99.
       01 LL PIC 99.
       01 XLLTAB2401.
           02 XLLTAB24 PIC X OCCURS 24 TIMES.
       01 XFFTAB1001.
           02 XFFTAB10 PIC X OCCURS 10 TIMES.
       01 NP PIC S9 VALUE 0.
       01  GAR-TAB01.
           02 GAR-TAB PIC X(8) OCCURS 9 TIMES.
       01 ALF20 PIC X(20).
       01  ALF20A PIC X(20).
       01 GARNO PIC X(12).
       01 TABKEY01.
          02 TABKEY PIC X(8) OCCURS 9 TIMES.
       01  SAVECODE PIC XXX VALUE SPACE.
       01 NUM2 PIC 99.
       PROCEDURE DIVISION.
       0005-START.
           OPEN I-O HOSPFILE INPUT INSFILE GAPFILE.
       1000-ACTION.
           MOVE SPACES TO ACTION GARNO.
           DISPLAY "OPTION,ID  HOSPINS".
           ACCEPT DATAIN.
           IF DATAIN = "END" GO TO 9100-CLOSE-MASTER-FILE.
           IF DATAIN = "?"
           DISPLAY "INSURE. RTNE. <LI,AI,CI,FI,FL>,<INS-CODE>"
           DISPLAY "GAP"
           DISPLAY "END = END PROGRAM"
               GO TO 1000-ACTION.
           UNSTRING DATAIN DELIMITED BY "," INTO ACTION GARNO.
           IF GARNO = "1" OR "2" OR "3" OR "4" OR "5" OR "6"
           OR "7" OR "8" OR "9" MOVE GARNO TO ALF-1
           MOVE ALF-1 TO NUM-1 MOVE TABKEY(NUM-1) TO GARNO.
           IF ACTION = "LI" OR "AI" OR "CI" OR "FI" OR "SC" 
           OR "DI" OR "FL" OR "GAP"
           NEXT SENTENCE ELSE DISPLAY "WHAT?" GO TO 1000-ACTION.
           IF ACTION = "AI" GO TO 2000-AI.
           IF ACTION = "FI" OR "LI" OR "CI" GO TO HOSP-LOOK.
           IF ACTION = "SC" GO TO SC-1.
           IF ACTION = "DI" GO TO DI-INS.
           IF ACTION = "FL" PERFORM INS-1 THRU INS-1-EXIT
           GO TO 1000-ACTION.
           IF ACTION = "GAP" PERFORM GAP-1 THRU GAP-1-EXIT.
           GO TO 1000-ACTION.

       DI-INS. MOVE GARNO TO HOSP-KEY
               READ HOSPFILE WITH LOCK INVALID DISPLAY "INVALID"
               GO TO 1000-ACTION.
               DISPLAY HOSP-KEY " " HOSP-INS-KEY " " HOSP-GAP 
                                                 " " HOSP-NAME
               DISPLAY HOSP-BOX " " HOSP-STREET 
               DISPLAY HOSP-CITY " " HOSP-STATE 
               DISPLAY "DELETE Y/N".
               ACCEPT ACTION
               IF ACTION NOT = "Y" DISPLAY "NO DELETE"
               GO TO 1000-ACTION.
               DELETE HOSPFILE RECORD.
               GO TO 1000-ACTION.
       SC-1. DISPLAY "STARTING CHC-CODE"
             ACCEPT ALF4
             IF ALF4 = SPACE OR "X" OR "END" OR "E"
             GO TO 1000-ACTION.
           IF ALF4 NUMERIC MOVE ALF4 TO HOSP-KEY
           START HOSPFILE KEY NOT < HOSP-KEY INVALID GO TO SC-1.
           IF ALF4 NUMERIC GO TO SC-2.
             MOVE ALF4 TO HOSP-NAME
             START HOSPFILE KEY NOT < HOSP-NAME INVALID
             DISPLAY "END OF FILE" GO TO SC-1.
       SC-2. READ HOSPFILE NEXT WITH LOCK AT END DISPLAY "END OF FILE"
             GO TO SC-1.
       SC-3.
             DISPLAY HOSP-KEY " " HOSP-INS-KEY " " HOSP-NAME
             ACCEPT INS-KEY
             IF INS-KEY = "?"
             DISPLAY "ENTER THE RRI INSCODE"
             DISPLAY "OR F TO SEARCH FOR IT"
             DISPLAY "OR X OR END OR E OR BK"
             DISPLAY "OR SPACE TO SKIP TO THE NEXT"
             DISPLAY "OR . TO REPEAT THE LAST CODE USED"
             GO TO SC-2.
             IF INS-KEY = "X" OR "E" OR "END" OR "BK" GO TO SC-1.
             IF INS-KEY = SPACE GO TO SC-2.
           IF INS-KEY = "F" PERFORM INS-1 THRU INS-1-EXIT GO TO SC-3.
           IF INS-KEY = "." MOVE SAVECODE TO INS-KEY.
           READ INSFILE INVALID DISPLAY "INVALID" GO TO SC-3.
           DISPLAY INS-NAME
           MOVE INS-KEY TO HOSP-INS-KEY SAVECODE
           REWRITE HOSPFILE01 GO TO SC-2.
       HOSP-LOOK.
           IF ACTION = "FI" PERFORM HOSP-1 THRU HOSP-1-EXIT
           GO TO 1000-ACTION.
           MOVE GARNO TO HOSP-KEY
           READ HOSPFILE WITH LOCK INVALID DISPLAY "INVALID"
           GO TO 1000-ACTION.
           IF HOSPSTAT = "61" DISPLAY "RECORD BEING USED"
           GO TO 1000-ACTION.
           IF ACTION = "LI" PERFORM LI-1 UNLOCK HOSPFILE RECORD
           GO TO 1000-ACTION.
           GO TO HOSP-CP.
      * BEGIN HOSPURANCE FILE MAINTAINENCE ROUTINE *

       2000-AI.
           MOVE SPACE TO HOSPFILE01
           PERFORM HOSP-ADD-LOOP VARYING HOSP-A-KEY FROM 1 BY 1
             UNTIL HOSP-A-KEY > 9
           IF IN-FIELD = "X" DISPLAY "NO UPDATE" GO TO 1000-ACTION.
           WRITE HOSPFILE01 INVALID KEY DISPLAY "NO UPDATE"
           DISPLAY "THIS SHOULD NOT HAVE HAPPENED! CONTACT DATA CENTER."
           DISPLAY "THIS PROGRAM HAS BEEN TERMINATED"
           GO TO 9100-CLOSE-MASTER-FILE.
           DISPLAY HOSP-KEY " " HOSP-INS-KEY " " HOSP-GAP " " HOSP-NAME.
           DISPLAY "RECORD IS ADDED".
           GO TO 1000-ACTION.
       HOSP-ADD-LOOP.
           SET HOSP-INDX TO HOSP-A-FLD(HOSP-A-KEY).
           PERFORM HOSP-DISPLAY THRU HOSPDEEX.
       HOSP-CP.
           MOVE SPACES TO FIELD-CODE IN-FIELD.
           DISPLAY "FIELD CODE,DATA?  HOSP.".
           ACCEPT DATAIN.
           IF DATAIN = "X"
               DISPLAY "NO CHANGE"  GO TO 1000-ACTION.
           IF DATAIN = "UP"  GO TO 5000-WRITE-HOSPFILE.
           IF DATAIN = "LI"
           PERFORM LI-1 THRU LI-1-EXIT GO TO HOSP-CP.
           IF DATAIN = "?"
           DISPLAY "ENTER A FIELD CODE <COMMA> AND NEW DATA."
               DISPLAY "UP = UPDATE CHANGES"
           DISPLAY " X = NO CHANGE"
           DISPLAY "LI = LIST "
           DISPLAY "FIELD CODES: 2=NAME 3=PO BOX 4=STREET"
           DISPLAY "             5=CITY 6=STATE  7=ZIP   "
           DISPLAY "             8=INS-CODE      9=GAP"   
           GO TO HOSP-CP.
           MOVE SPACE TO FIELD-CODE IN-FIELD
           UNSTRING DATAIN DELIMITED BY "," INTO FIELD-CODE IN-FIELD.
           INSPECT FIELD-CODE REPLACING LEADING SPACE BY "0".
           IF FIELD-CODE NOT NUMERIC
               DISPLAY "FIELD-CODE MUST BE 2 DIGITS AND NUMERIC"
               GO TO HOSP-CP.
           IF FIELD-CODE = "00"
               DISPLAY "FIELD CODE CANNOT BE ZERO OR BLANK"
               GO TO HOSP-CP.
           IF ( FIELD-CODE > "01" AND  < "10" )
           MOVE FIELD-CODE TO NUM2
           SET HOSP-INDX TO NUM2
           GO TO HOSP-GO-TO.
           DISPLAY "FIELD # MUST BE 2 THRU 9"
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
           MOVE HOSP-LEN-TAB(HOSP-INDX) TO Q ADD 1 TO Q.
           IF IN-FIELD-TAB(Q) NOT = " " MOVE 1 TO FLAG.
           IF FLAG = 1 DISPLAY "DATA TOO LONG, MUST NOT BE GREATER "
             "THAN " HOSP-LEN-TAB(HOSP-INDX) ".".
           IF FLAG = 1 AND ACT-1 = "C" GO TO HOSP-CP.
           IF FLAG = 1 AND ACT-1 = "A" GO TO HOSP-DISPLAY.
           GO TO   
           1-HOSP 2-HOSP 3-HOSP 4-HOSP 5-HOSP 6-HOSP 7-HOSP 8-HOSP
           9-HOSP
           DEPENDING ON HOSP-INDX.                     
       HOSP-TI.
           IF ACT-1 = "A" GO TO HOSP-DISPLAY.
           IF ACT-1 = "C" GO TO HOSP-CP.
           IF ACT-1 = "S" OR "F" GO TO 1000-ACTION.
       1-HOSP.
           IF IN-FIELD = "?" DISPLAY "ENTER 10 CHARACTER CHCRR CODE"
           GO TO HOSP-TI.
           MOVE IN-FIELD TO HOSP-KEY
           READ HOSPFILE INVALID GO TO HOSPDEE.
           DISPLAY "ALREADY ON FILE"
           PERFORM LI-1 THRU LI-1-EXIT GO TO HOSP-TI.
       2-HOSP.
           IF IN-FIELD = "?"
              DISPLAY "ENTER NAME OF THE INSURANCE"
              DISPLAY "ON PERSONAL STATEMENTS AND CLAIM FORMS"
              GO TO HOSP-TI.
              
           IF IN-FIELD-1 = SPACE OR IN-FIELD = SPACE
           DISPLAY "LEADING BLANKS OR A BLANK NAME IS NOT ALLOWED"
           GO TO HOSP-TI.
           MOVE IN-FIELD TO HOSP-NAME GO TO HOSPDEE.
       3-HOSP.
           IF IN-FIELD = "?"
               DISPLAY "ENTER PO BOX OF THE INSURANCE"
              GO TO HOSP-TI.
           MOVE IN-FIELD TO HOSP-BOX GO TO HOSPDEE.
       4-HOSP.
           IF IN-FIELD = "?"
               DISPLAY "ENTER STREET OF THE INSURANCE"
              GO TO HOSP-TI.
           MOVE IN-FIELD TO HOSP-STREET GO TO HOSPDEE.
       5-HOSP.
           IF IN-FIELD = "?"
               DISPLAY "ENTER CITY OF THE INSURANCE"
              GO TO HOSP-TI.
           MOVE IN-FIELD TO HOSP-CITY GO TO HOSPDEE.
       
       6-HOSP.
           IF IN-FIELD = "?"
               DISPLAY "ENTER STATE OF THE INSURANCE"
              GO TO HOSP-TI.
           MOVE IN-FIELD TO HOSP-STATE GO TO HOSPDEE.
       7-HOSP.
           IF IN-FIELD = "?"
               DISPLAY "ENTER ZIP OF THE INSURANCE"
              GO TO HOSP-TI.
           MOVE IN-FIELD TO HOSP-ZIP GO TO HOSPDEE.
       8-HOSP.
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
           DISPLAY "INVALID NUMBER" GO TO HOSP-TI.
           MOVE RIGHT-3 TO INS-KEY
           READ INSFILE INVALID DISPLAY "BAD CODE" 
           GO TO HOSP-TI.
           DISPLAY INS-NAME
           MOVE RIGHT-3 TO HOSP-INS-KEY
           GO TO HOSPDEE.
       9-HOSP.
           IF IN-FIELD = "?"
              DISPLAY "GAPCODE WHEN MEDICARE IS PRIMARY OR F TO FIND"
              GO TO HOSP-TI.
           IF IN-FIELD = SPACE
           MOVE SPACE TO HOSP-GAP
           GO TO HOSPDEE.
           IF IN-FIELD = "F"
           PERFORM GAP-1 THRU GAP-1-EXIT
           GO TO HOSP-TI.
           MOVE SPACE TO RIGHT-7
           UNSTRING IN-FIELD DELIMITED BY " " INTO RIGHT-7
           INSPECT RIGHT-7 REPLACING LEADING " " BY "0"
           MOVE RIGHT-7 TO IN-FIELD
           MOVE IN-FIELD TO GAPKEY
           READ GAPFILE INVALID DISPLAY "INVALID CODE"
           GO TO HOSP-TI.
           MOVE IN-FIELD TO HOSP-GAP
           GO TO HOSPDEE.

       HOSPDEE. IF ACT-1 = "C" GO TO HOSP-CP.
       HOSPDEEX. EXIT.
       5000-WRITE-HOSPFILE.
           REWRITE HOSPFILE01 INVALID KEY DISPLAY "NO UPDATE."
           DISPLAY "THIS SHOULD NOT HAPPEN! CONTACT THE DATA CENTER."
           DISPLAY "THIS PROGRAM IS TERMINATED!"
           GO TO 9100-CLOSE-MASTER-FILE.
           UNLOCK HOSPFILE RECORD
           DISPLAY "UPDATE MADE"  GO TO 1000-ACTION.
       LI-1.
           DISPLAY HOSP-KEY " " HOSP-INS-KEY " " HOSP-GAP " " HOSP-NAME
           DISPLAY HOSP-BOX " " HOSP-STREET
           DISPLAY HOSP-CITY " " HOSP-STATE " " HOSP-ZIP.
       LI-1-EXIT. EXIT.
       HOSP-1.
           DISPLAY "HOSPFILE SEARCH"
           DISPLAY "1 = BY NUMBER "
           DISPLAY "2 = BY NAME"
           DISPLAY "3 = BY CITY"
           DISPLAY "4 = BY STATE"
           DISPLAY "5 = BY RRI INS CODE"
           DISPLAY "6 = BY GAP CODE".
       HOSP-2.  ACCEPT IN-FIELD.
           IF IN-FIELD = "?"
           DISPLAY "X = BACK OR 1-6 SEARCH METHOD"
           GO TO INS-2.
           IF IN-FIELD-1 = "X" DISPLAY "END SEARCH" GO TO HOSP-1-EXIT.
           IF IN-FIELD-1 = "1" OR "2" OR "3" OR "4" OR "5" OR "6"
           NEXT SENTENCE ELSE DISPLAY "BAD" GO TO HOSP-2.
           MOVE IN-FIELD-1 TO ALF-1.
       HOSP-3. DISPLAY "STARTING POINT?".
           ACCEPT IN-FIELD.
           IF IN-FIELD = "?"
           DISPLAY "ENTER WHERE TO START LOOKING"
           DISPLAY "BK = BACK TO SEARCH METHOD"
           DISPLAY "OR X = BACK TO OPTION" GO TO HOSP-3.
           IF IN-FIELD = "X" GO TO HOSP-1-EXIT.
           IF IN-FIELD = "BK" GO TO HOSP-1.
           MOVE 0 TO X

           IF ALF-1 = "1" 
            MOVE IN-FIELD TO HOSP-KEY
            MOVE "NUMBER  " TO ALF-8
             START HOSPFILE KEY NOT < HOSP-KEY INVALID GO TO HOSP-3
             END-START
            GO TO HOSP-4
           END-IF

           IF ALF-1 = "2" 
            MOVE IN-FIELD TO HOSP-NAME
            MOVE "NAME    " TO ALF-8
             START HOSPFILE KEY NOT < HOSP-NAME INVALID GO TO HOSP-3
             END-START
            GO TO HOSP-4
           END-IF
           
           IF ALF-1 = "3"  
            MOVE IN-FIELD TO HOSP-CITY
            MOVE "CITY    " TO ALF-8
             START HOSPFILE KEY NOT < HOSP-CITY INVALID GO TO HOSP-3
             END-START
            GO TO HOSP-4
           END-IF
           
           IF ALF-1 = "4" 
            MOVE IN-FIELD TO HOSP-STATE
            MOVE "STATE   " TO ALF-8
             START HOSPFILE KEY NOT < HOSP-STATE INVALID GO TO HOSP-3
             END-START
            GO TO HOSP-4
           END-IF
           
           IF ALF-1 = "5" 
            MOVE IN-FIELD TO HOSP-INS-KEY
            MOVE "RRI INS " TO ALF-8
             START HOSPFILE KEY NOT < HOSP-INS-KEY INVALID GO TO HOSP-3
             END-START
            GO TO HOSP-4
           END-IF
           
           IF ALF-1 = "6" 
            MOVE IN-FIELD TO HOSP-GAP
            MOVE "GAPCODE " TO ALF-8
             START HOSPFILE KEY NOT < HOSP-GAP INVALID GO TO HOSP-3
             END-START
            GO TO HOSP-4
           END-IF
           GO TO HOSP-3.

       HOSP-4. READ HOSPFILE NEXT AT END DISPLAY "END OF FILE"
           GO TO HOSP-1-EXIT.
           MOVE SPACE TO ALF-7
           IF INS-STATUS = "1" MOVE "CLOSED " TO ALF-7.
           DISPLAY " "
           DISPLAY HOSP-KEY " " HOSP-INS-KEY " " HOSP-GAP
           DISPLAY HOSP-NAME
           DISPLAY HOSP-BOX " " HOSP-STREET 
           DISPLAY  HOSP-CITY " " HOSP-STATE " " HOSP-ZIP
           ADD 1 TO X
           IF X > 3 MOVE 0 TO X DISPLAY "BY " ALF-8 ACCEPT ANS
           IF ANS NOT = SPACE GO TO HOSP-1-EXIT.
           GO TO HOSP-4.
       HOSP-1-EXIT. EXIT.

       INS-1. DISPLAY "INSFILE SEARCH TYPE"
           DISPLAY "1 = BY NUMBER "
           DISPLAY "2 = BY NAME"
           DISPLAY "3 = BY ASSIGNMENT CODE"
           DISPLAY "4 = BY NEIC CODE"
           DISPLAY "5 = BY NEIC ASSIGNM CODE"
           DISPLAY "6 = BY CLAIM-TYPE"
           DISPLAY "7 = BY CITY".
       INS-2.  ACCEPT IN-FIELD.
           IF IN-FIELD = "?"
           DISPLAY "X = BACK OR 1-6 SEARCH METHOD"
           GO TO INS-2.
           IF IN-FIELD = "X" DISPLAY "END SEARCH" GO TO INS-1-EXIT.
           IF IN-FIELD = "1" OR "2" OR "3" OR "4" OR "5" OR "6" OR "7"
           NEXT SENTENCE ELSE DISPLAY "BAD" GO TO INS-2.
           MOVE IN-FIELD-1 TO ALF-1.
       INS-3. DISPLAY "STARTING POINT?".
           ACCEPT IN-FIELD.
           IF IN-FIELD = "?"
           DISPLAY "ENTER WHERE TO START LOOKING"
           DISPLAY "BK = BACK TO SEARCH METHOD"
           DISPLAY "OR X = BACK TO OPTION" GO TO INS-3.
           IF IN-FIELD = "X" GO TO INS-1-EXIT.
           IF IN-FIELD = "BK" GO TO INS-1.
           MOVE 0 TO X
           IF ALF-1 = "1" MOVE SPACE TO RIGHT-3
           UNSTRING IN-FIELD DELIMITED BY " " INTO RIGHT-3
           INSPECT RIGHT-3 REPLACING LEADING " " BY "0"
           MOVE RIGHT-3 TO INS-KEY
           MOVE "NUMBER  " TO ALF-8
           START INSFILE KEY NOT < INS-KEY INVALID GO TO INS-1-END.
           IF ALF-1 = "2" MOVE IN-FIELD TO INS-NAME
           MOVE "NAME    " TO ALF-8
           START INSFILE KEY NOT < INS-NAME INVALID GO TO INS-1-END.
           IF ALF-1 = "3" MOVE IN-FIELD TO INS-ASSIGN
           MOVE "ASSIGNMT" TO ALF-8
           START INSFILE KEY NOT < INS-ASSIGN INVALID GO TO INS-1-END.
           IF ALF-1 = "4" MOVE IN-FIELD TO INS-NEIC
           MOVE "NEICCODE" TO ALF-8
           START INSFILE KEY NOT < INS-NEIC INVALID GO TO INS-1-END.
           IF ALF-1 = "5" MOVE IN-FIELD TO INS-NEIC-ASSIGN
           MOVE "NEICASGM" TO ALF-8
           START INSFILE KEY NOT < INS-NEIC-ASSIGN INVALID 
           GO TO INS-1-END.
           IF ALF-1 = "6" 
           MOVE IN-FIELD TO INS-CLAIMTYPE
           MOVE "CLM-TYPE" TO ALF-8
           START INSFILE KEY NOT < INS-CLAIMTYPE INVALID 
           GO TO INS-1-END.
           IF ALF-1 = "7" 
           MOVE IN-FIELD TO INS-CITY
           MOVE "CITY    " TO ALF-8
           START INSFILE KEY NOT < INS-CITY INVALID 
           GO TO INS-1-END.
       INS-4. READ INSFILE NEXT AT END DISPLAY "END OF FILE"
           GO TO INS-1-EXIT.
           DISPLAY INS-KEY " " INS-NAME " " INS-STREET " " INS-CITY
           " " INS-STATE " " INS-ZIP
           ADD 1 TO X
           IF X > 8 MOVE 0 TO X DISPLAY "BY " ALF-8 ACCEPT ANS
           IF ANS NOT = SPACE GO TO INS-1-EXIT.
           GO TO INS-4.
       INS-1-END. DISPLAY "END OF FILE".
       INS-1-EXIT. EXIT.
       GAP-1. 
           DISPLAY "SEARCH TYPE".
           DISPLAY "1 = BY NUMBER "
           DISPLAY "2 = BY NAME"
           DISPLAY "3 = BY CITY"
           DISPLAY "4 = BY STATE".
       GAP-2.  ACCEPT IN-FIELD.
           IF IN-FIELD = "?"
           DISPLAY "X = BACK OR 1-4 SEARCH METHOD"
           GO TO GAP-2.
           IF IN-FIELD = "X" DISPLAY "END SEARCH" GO TO GAP-1-EXIT.
           IF IN-FIELD = "1" OR "2" OR "3" OR "4" NEXT SENTENCE
           ELSE DISPLAY "BAD" GO TO GAP-2.
           MOVE IN-FIELD-1 TO ALF-1.
       GAP-3. DISPLAY "STARTING POINT?".
           ACCEPT IN-FIELD.
           IF IN-FIELD = "?"
           DISPLAY "ENTER WHERE TO START LOOKING"
           DISPLAY "BK = BACK TO SEARCH METHOD"
           DISPLAY "OR X = BACK TO OPTION" GO TO GAP-3.
           IF IN-FIELD = "X" GO TO GAP-1-EXIT.
           IF IN-FIELD = "BK" GO TO GAP-1.
           MOVE 0 TO X
           IF ALF-1 = "1" MOVE SPACE TO RIGHT-7
           UNSTRING IN-FIELD DELIMITED BY " " INTO RIGHT-7
           INSPECT RIGHT-7 REPLACING LEADING " " BY "0"
           MOVE RIGHT-7 TO GAPKEY
           MOVE "NUMBER" TO ALF-6
           START GAPFILE KEY NOT < GAPKEY 
           INVALID GO TO GAP-3
           NOT INVALID GO TO GAP-4.
           IF ALF-1 = "2" MOVE IN-FIELD TO GAP-NAME
           MOVE "TITLE " TO ALF-6
           START GAPFILE KEY NOT < GAP-NAME 
           INVALID GO TO GAP-3
           NOT INVALID GO TO GAP-4.
           IF ALF-1 = "3" MOVE IN-FIELD TO GAP-CITY
           MOVE "CITY  " TO ALF-6
           START GAPFILE KEY NOT < GAP-CITY 
           INVALID GO TO GAP-3
           NOT INVALID GO TO GAP-4.
           MOVE IN-FIELD TO GAP-STATE
           MOVE "STATE " TO ALF-6
           START GAPFILE KEY NOT < GAP-STATE
           INVALID GO TO GAP-3.
       GAP-4. READ GAPFILE NEXT AT END DISPLAY "END OF FILE"
           GO TO GAP-1-EXIT.
           MOVE GAP-ZIP TO ALF-5
           MOVE GAPKEY TO ALF-7
           INSPECT ALF-7 REPLACING LEADING "0" BY " "
           MOVE SPACE TO ALF4
           IF GAP-TYPE = "X" MOVE "CRSS" TO ALF4.
           MOVE GAP-NAME TO ALF20
           MOVE GAP-ADDR TO ALF20A
           DISPLAY ALF4 " " ALF-7 " " ALF20 " " ALF20A " " GAP-CITY " "
           GAP-STATE " " ALF-5
           ADD 1 TO X
           IF X > 8 MOVE 0 TO X DISPLAY "BY " ALF-6 ACCEPT ANS
           IF ANS NOT = SPACE GO TO GAP-1-EXIT.
           GO TO GAP-4.
       GAP-1-EXIT. EXIT.

       9100-CLOSE-MASTER-FILE.
           CLOSE HOSPFILE.
           DISPLAY "HOSP-INSURANCE ROUTINE HAS ENDED".
           STOP RUN.

