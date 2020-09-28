      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
        IDENTIFICATION DIVISION.
       PROGRAM-ID. sid062.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REFPHY ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS REF-KEY
             ALTERNATE RECORD KEY IS REF-BSNUM  WITH DUPLICATES
             ALTERNATE RECORD KEY IS REF-CRNUM WITH DUPLICATES
             ALTERNATE RECORD KEY IS REF-UPIN  WITH DUPLICATES
             ALTERNATE RECORD KEY IS REF-CDNUM WITH DUPLICATES
             ALTERNATE RECORD KEY IS REF-NAME  WITH DUPLICATES
             LOCK MODE MANUAL
             STATUS IS REF-STAT.
           SELECT PROVCAID ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS PROV-KEY
           ALTERNATE RECORD KEY IS PROV-NPI WITH DUPLICATES
           ALTERNATE RECORD KEY IS PROV-TAX WITH DUPLICATES
           ALTERNATE RECORD KEY IS PROV-NAME WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT UPINFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS UPIN-PROVNUM
             ALTERNATE RECORD KEY IS UPIN-NAME WITH DUPLICATES
             ALTERNATE RECORD KEY IS UPIN-KEY     WITH DUPLICATES
             ALTERNATE RECORD KEY IS UPIN-CITY WITH DUPLICATES
             LOCK MODE MANUAL.
       DATA DIVISION.
       FILE SECTION.
       FD  PROVCAID.
       01  PROVCAID01.
           02 PROV-KEY PIC X(7).
           02 PROV-NAME PIC X(24).
           02 PROV-NPI PIC X(10).
           02 PROV-TAX PIC X(10).
           02 PROV-STREET PIC X(20).
           02 PROV-CITY PIC X(20).
           02 PROV-STATE PIC XX.
           02 PROV-ZIP PIC X(5).
       FD  REFPHY.
       01  REFPHY01.
           02 REF-KEY PIC XXX.
           02 REF-BSNUM PIC X(5).
           02 REF-CRNUM PIC X(6).
           02 REF-UPIN PIC X(6).
           02 REF-CDNUM PIC X(7).
           02 REF-NAME PIC X(24).
           02 REF-NPI PIC X(10).
       FD  UPINFILE.
       01  UPIN01.
           02 UPIN-PROVNUM.
             03 UPIN-PROVNUM1 PIC XX.
             03 UPIN-PROVNUM2 PIC X(4).
           02 UPIN-NAME PIC X(33).
           02 UPIN-KEY PIC X(6).
           02 UPIN-CITY PIC X(18).
           02 UPIN-STATE PIC XX.
           02 UPIN-ZIP PIC X(5).
       WORKING-STORAGE SECTION.
       01  NUM1 PIC 9.
       01  REF-STAT PIC XX.
       01  TEST-NAME.
           02 TN1 PIC X.
           02 TN14 PIC X(23).
       01  TEST-KEY PIC XXX.
       01  TEST-NPI PIC X(10).
       01  Z PIC 99.
       01  Y PIC 99.
       01  X PIC 99.
       01  ANS PIC X(8).
       01  FUNC PIC X(8).
       01  ALF-8.
             02 ALF-84 PIC X(4).
             02 ALF-8E PIC X(4).
       01  RIGHT-8 PIC X(8) JUST RIGHT.
       01  RIGHT-7 PIC X(7) JUST RIGHT.
       01  ALF-4.
           02 ALF-41 PIC X.
           02 ALF-43 PIC XXX.
       01  ALF4 PIC X(4).
       01  ALF-10 PIC X(10).
       01  ALF24 PIC X(24).
       01  KEYTAB01.
           02 KEYTAB PIC X(8) OCCURS 9 TIMES.
       01  ALF1 PIC X.
       01  RIGHT-5 PIC X(5) JUST RIGHT.
       01  ALF-5 PIC X(5).
       01  ALF-7 PIC X(7).
       01  NAMELAST PIC X(24).
       01  NAMEFIRST PIC X(24).
       01  PROVTAB01.
           02 PROVTAB PIC X(7) OCCURS 9 TIMES.
       01  ALFATAB1RE.
           02 FILLER PIC X(36) VALUE
           "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".
       01  ALFATAB101 REDEFINES ALFATAB1RE.
           02 ALFATAB1 PIC X OCCURS 36 TIMES.
       01  ALFATAB2RE.
           02 FILLER PIC X(36) VALUE
           "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ".
       01  ALFATAB201 REDEFINES ALFATAB2RE.
           02 ALFATAB2 PIC X OCCURS 36 TIMES.
       01  FLAG PIC 9.
       PROCEDURE DIVISION.
       P0. OPEN I-O REFPHY INPUT PROVCAID UPINFILE.
       S1. DISPLAY "FUNCTION ?".
           ACCEPT ANS.
           IF ANS = "END" GO TO Z1.
           IF ANS = "?"
           DISPLAY "A = ADD A PHYSICIAN CODE"
           DISPLAY "C = CHANGE B/S, CARE/CAID, OR UPIN #"
      *    DISPLAY "D = DELETE A PHYSICIAN CODE(BEWARE)"
           DISPLAY "L = LIST A SINGLE PHYSICIAN CODE"
           DISPLAY "F = LIST 5 PHYSICIAN CODES AT A TIME"
           DISPLAY "NT = SEARCH FOR NPI AND TAXONOMY CODES"
           DISPLAY "US = SEARCH FOR A UPIN #"
           DISPLAY "AU = ADD DIRECTLY FROM UPIN FILE"
           DISPLAY "END = END THE PROGRAM"
           GO TO S1.
           MOVE SPACES TO FUNC REF-KEY.
           UNSTRING ANS DELIMITED BY "," INTO FUNC REF-KEY.
           IF FUNC = "A" OR "C" OR "L" OR "F" OR "D"  OR "NT"
           OR "US" OR "AU"
           NEXT SENTENCE ELSE DISPLAY "WHAT?" GO TO S1.
           IF FUNC = "L" GO TO L1.
           IF FUNC = "A" PERFORM A1 THRU A1-EXIT GO TO S1.
           IF FUNC = "C" GO TO C1.
           IF FUNC = "D" GO TO D1.
           IF FUNC = "F" GO TO F1.
           IF FUNC = "AU" GO TO AU-1.
           IF FUNC = "US" PERFORM US1 THRU US-EXIT
           GO TO S1.
           IF FUNC = "NT" PERFORM PROV-1 THRU PROV-EXIT.
           GO TO S1.
                                 
       L1. READ REFPHY INVALID DISPLAY "NOT ON FILE" GO TO S1
           END-READ
           DISPLAY  REF-KEY " " REF-BSNUM " " REF-CRNUM " " REF-UPIN
           " " REF-CDNUM " " REF-NPI " " REF-NAME
           GO TO S1.
       F1. MOVE 0 TO X.
           MOVE SPACE TO ANS.
           IF REF-KEY(2:1) = " "
           START REFPHY KEY NOT < REF-KEY
           INVALID
            DISPLAY "END OF FILE" GO TO S1
           NOT INVALID GO TO F2
           END-START.
           MOVE SPACE TO REF-NAME 
           MOVE REF-KEY TO REF-NAME
           START REFPHY KEY NOT < REF-NAME INVALID
           DISPLAY "END OF FILE" GO TO S1.
       F2. READ REFPHY NEXT AT END DISPLAY "END OF FILE"
           GO TO S1
           END-READ

           ADD 1 TO X.
           IF X = 6 ACCEPT ANS
           MOVE 0 TO X
           IF ANS NOT = SPACE GO TO S1.
           DISPLAY  REF-KEY " " REF-BSNUM " " REF-CRNUM " " REF-UPIN
           " " REF-CDNUM " " REF-NPI " " REF-NAME
           GO TO F2.
       A1.
           IF REF-KEY(1:1) NUMERIC AND REF-KEY(2:1) = SPACE
           PERFORM B1 THRU B1-EXIT GO TO A1-EXIT.
           DISPLAY "LASTNAME;FIRSTNAME".
           ACCEPT TEST-NAME.
           IF TEST-NAME = "?"
             DISPLAY "BK = BACK TO FUNCTION"
             DISPLAY "TYPE IN LAST NAME,FIRST INITIAL "
             GO TO A1.
           
           IF TEST-NAME = "X" OR "BK"
             DISPLAY "NO UPDATE"
             GO TO A1-EXIT.

           MOVE SPACE TO NAMELAST NAMEFIRST
           UNSTRING TEST-NAME DELIMITED BY ";" INTO
             NAMELAST NAMEFIRST

           IF (NAMEFIRST = SPACE) OR (NAMELAST = SPACE)
             DISPLAY "FIRST AND LAST NAMES ARE MANDATORY."
             GO TO A1.
           IF NAMEFIRST(1:1) NOT ALPHABETIC
           DISPLAY "INVALID FIRST NAME"
           GO TO A1.
           INSPECT TEST-NAME REPLACING ALL "*" BY " ".
           INSPECT TEST-NAME REPLACING ALL "~" BY " ".
           INSPECT TEST-NAME REPLACING ALL "/" BY " ".
           INSPECT TEST-NAME REPLACING ALL ":" BY " ".
           INSPECT TEST-NAME REPLACING ALL "#" BY " ".
       A3.
           MOVE SPACE TO TEST-KEY.
           MOVE 0 TO FLAG
           
           PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > 36
             PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 36
               STRING TN1 ALFATAB1(Z) ALFATAB2(Y) DELIMITED BY SIZE
                 INTO TEST-KEY
               MOVE TEST-KEY TO REF-KEY
               READ REFPHY
                 INVALID
      *             DISPLAY TEST-KEY " INVALID"
                   MOVE 1 TO FLAG
                   MOVE 37 TO Y
                   MOVE 37 TO Z
                 NOT INVALID
      *             DISPLAY TEST-KEY " NOT INVALID"
                   IF REF-NAME = TEST-NAME DISPLAY REF-KEY " " REF-NAME
                     DISPLAY "ON FILE ALREADY. MAKE NAME UNIQUE TO ADD!"
                     MOVE 37 TO Y
                     MOVE 37 TO Z
                     GO TO A1
                   END-IF
               END-READ
             END-PERFORM
           END-PERFORM

           IF FLAG = 0  DISPLAY "TOO MANY PHYS. WITH SAME LETTER!"
             GO TO A1
           END-IF.

       A4.
           MOVE TEST-KEY TO REF-KEY.
           MOVE TEST-NAME TO REF-NAME.
       A5. DISPLAY "B/S # UP TO 5-DIGITS".
           ACCEPT ALF-5.
           IF ALF-5 = "?"
           DISPLAY "ENTER THE BLUE SHIELD NUMBER"
           DISPLAY "ENTER BK TO BACK TO NAME"
           DISPLAY "ENTER X TO RETURN TO FUNCTION"
           GO TO A5.
           IF ALF-5 = "BK" GO TO A1.
           IF ALF-5 = "X" GO TO S1.
           MOVE SPACE TO RIGHT-5
           UNSTRING ALF-5 DELIMITED BY " " INTO RIGHT-5
           INSPECT RIGHT-5 REPLACING LEADING " " BY "0"
           MOVE RIGHT-5 TO REF-BSNUM.
           INSPECT REF-BSNUM REPLACING ALL "#" BY " ".
       A6. DISPLAY "MEDICARE # 6 CHARACTERS".
           ACCEPT ALF-8.
           IF ALF-8 = "?"
           DISPLAY "ENTER THE MEDICARE #"
           DISPLAY "INCLUDE THE 2 LEADING ALFA CHARACTERS"
           DISPLAY "ENTER BK TO BACK TO NAME"
           DISPLAY "ENTER X TO RETURN TO FUNCTION"
           GO TO A6.
           IF ALF-8 = "BK" GO TO A5.
           IF ALF-8 = "X" GO TO S1.
           MOVE ALF-8 TO REF-CRNUM.
           INSPECT REF-CRNUM REPLACING ALL "#" BY " ".
       A7. DISPLAY "ENTER THE UPIN #".
           ACCEPT ALF-8.
           IF ALF-8 = "?"
           DISPLAY "ENTER THE 6 CHARACTER UNIQUE UPIN CODE"
           DISPLAY "ENTER BK TO BACK TO NAME"
           DISPLAY "ENTER X TO RETURN TO FUNCTION"
           GO TO A5.
           IF ALF-8 = "BK" GO TO A6.
           IF ALF-8 = "X" GO TO S1.
           MOVE ALF-8 TO REF-UPIN.
           INSPECT REF-UPIN REPLACING ALL "#" BY " ".
       A8. DISPLAY "MEDICAID # 7 CHARACTERS".
           ACCEPT ALF-8.
           IF ALF-8 = "?"
           DISPLAY "ENTER THE MEDICAID #"
           DISPLAY "OR F TO FIND # "
           DISPLAY "ENTER BK TO BACK TO NAME"
           DISPLAY "ENTER X TO RETURN TO FUNCTION"
           GO TO A8.
           IF ALF-8 = "BK" GO TO A7.
           IF ALF-8 = "X" GO TO S1.
           IF ALF-8 = "F" 
            PERFORM PROV-1 THRU PROV-EXIT
           GO TO A8.
           MOVE ALF-8 TO PROV-KEY
           READ PROVCAID INVALID
           DISPLAY "INVALID NUMBER; BUT STORED ANYWAY"
           END-READ.
           MOVE PROV-KEY TO REF-CDNUM.

       A9. DISPLAY "NPI NUMBER".
           ACCEPT ALF-10.
           IF ALF-10 = "?"
           DISPLAY "ENTER THE NPI PROVIDER # 10-DIGITS"
           DISPLAY "NT = SEARCH FOR NPI ANF TAXONOMY CODES"
           DISPLAY "ENTER BK TO BACK TO MEDICAID"
           DISPLAY "ENTER X TO RETURN TO FUNCTION"
           GO TO A9.
           IF ALF-10 = "BK" GO TO A8.
           IF ALF-10 = "X" GO TO S1.
           IF ALF-10 = "NT" PERFORM PROV-1 THRU PROV-EXIT
           GO TO A9.

           IF ALF-10 = SPACE  DISPLAY "NPI NOT KNOWN"
           MOVE SPACE TO REF-NPI GO TO A5-1.
           IF (ALF-10 NOT NUMERIC) OR (ALF-10 = "0000000000")
           DISPLAY "NOT VALID" GO TO A9.
           MOVE ALF-10 TO REF-NPI.
       A5-1.
           WRITE REFPHY01
           END-WRITE
           DISPLAY "RECORD ADDED KEY IS " REF-KEY
           CLOSE REFPHY OPEN I-O REFPHY.
       A1-EXIT. EXIT.
       B1.
           MOVE REF-KEY(1:1) TO Y
           MOVE PROVTAB(Y) TO PROV-KEY
           READ PROVCAID
           INVALID DISPLAY "BAD KEY "  Y
                   GO TO B1-EXIT.
           MOVE SPACE TO REFPHY01
           MOVE SPACE TO TEST-KEY.
           MOVE 0 TO FLAG
            PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > 36
             PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 36
             STRING PROV-NAME(1:1)  ALFATAB1(Z) ALFATAB2(Y) 
             DELIMITED BY SIZE INTO TEST-KEY
             MOVE TEST-KEY TO REF-KEY

             READ REFPHY
           INVALID
      *        DISPLAY TEST-KEY " INVALID"
              MOVE 1 TO FLAG
              MOVE 37 TO Y
              MOVE 37 TO Z
           NOT INVALID
      *      DISPLAY TEST-KEY " NOT INVALID"
            IF REF-NAME = PROV-NAME DISPLAY REF-KEY " " REF-NAME
             DISPLAY "ON FILE ALREADY. MAKE NAME UNIQUE TO ADD!"
              MOVE 37 TO Y
              MOVE 37 TO Z
              GO TO B1-EXIT
            END-IF
             END-READ

            END-PERFORM
            END-PERFORM.
           IF REF-CDNUM = PROV-KEY AND REF-NPI = PROV-NPI
           DISPLAY "ON FILE ALREADY "  REF-KEY " " REF-NPI " " REF-NAME
           GO TO B1-EXIT
           END-IF
           IF REF-CDNUM = PROV-KEY AND REF-NPI = SPACE
            AND PROV-NAME(1:3) = REF-NAME(1:3)
            DISPLAY "ON FILE ALREADY WITH NO NPI!"
            DISPLAY REF-KEY " " REF-NAME
            DISPLAY "MODIFY NPI  Y/N"
            ACCEPT ANS
             IF ANS = "Y"
              READ REFPHY WITH LOCK
                MOVE PROV-NPI TO REF-NPI
                DISPLAY REFPHY01
                REWRITE REFPHY01
                 GO TO B1-EXIT
             END-IF
            MOVE TEST-KEY TO REF-KEY
            MOVE PROV-NAME TO REF-NAME
            MOVE PROV-KEY TO REF-CDNUM
            MOVE PROV-NPI TO REF-NPI
            WRITE REFPHY01
             DISPLAY REF-KEY " " REF-NAME
           END-IF.
       B1-EXIT.
           EXIT.
       C1. READ REFPHY WITH LOCK INVALID DISPLAY "NOT ON FILE"
           GO TO S1
           END-READ

           DISPLAY  REF-KEY " " REF-BSNUM " " REF-CRNUM " " REF-UPIN
           " " REF-CDNUM " " REF-NPI " " REF-NAME.
       C2. DISPLAY "1=B/S  2=CARE  3=UPIN  4=CAID 5=NPI 6=NAME".
           ACCEPT ALF-8
           IF ALF-8 = "0" OR ALF-8 > "0" AND < "7" MOVE ALF-8 TO NUM1
           GO TO C3 C4 C5 C6 C7 C8 DEPENDING ON NUM1.
           IF ALF-8 = "X" DISPLAY "NO CHANGE" GO TO S1.
           IF ALF-8 = "?"
           DISPLAY "ENTER 1,2,3, OR X".
           GO TO C2.
       C3.
           DISPLAY "B/S #"
           ACCEPT ALF-5
           IF ALF-5 = "?" DISPLAY "NEW # OR X"
           GO TO C3.
           IF ALF-5 = "X" GO TO S1.
           MOVE SPACE TO RIGHT-5
           UNSTRING ALF-5 DELIMITED BY " " INTO RIGHT-5
           INSPECT RIGHT-5 REPLACING LEADING " " BY "0"
           MOVE RIGHT-5 TO REF-BSNUM
           INSPECT REF-BSNUM REPLACING ALL "#" BY " ".
           GO TO C5-1.
       C4.
           DISPLAY "CARE #"
           ACCEPT ALF-8
           IF ALF-8 = "?" DISPLAY "NEW #, US OR X"
           GO TO C4.
           IF ALF-8 = "US" PERFORM US1 THRU US-EXIT GO TO C4.
           IF ALF-8 = "X" GO TO S1.
           MOVE ALF-8 TO UPIN-PROVNUM
           READ UPINFILE INVALID DISPLAY "NOT ON FILE"
            DISPLAY "THIS MAY OR MAY NOT BE A VALID CARE/CAID #"
            DISPLAY "BUT IT WILL BE ASSUMED TO BE CORRECT!"
            GO TO C4-1
           END-READ

           DISPLAY UPIN-NAME " UPIN: " UPIN-KEY.
           DISPLAY "CHANGE UPIN TOO? Y/N".
           DISPLAY "OLD UPIN IS " REF-UPIN.
           ACCEPT ANS
           IF ANS = "Y" MOVE UPIN-KEY TO REF-UPIN.
           INSPECT REF-UPIN REPLACING ALL "#" BY " ".
       C4-1. MOVE ALF-8 TO REF-CRNUM
             GO TO C5-1.
       C6.
           DISPLAY "CAID #"
           ACCEPT ALF-8
           IF ALF-8 = "?" 
           DISPLAY "NEW #, OR F TO FIND, OR X TO QUIT"
           GO TO C6.
           IF ALF-8 = "X" GO TO S1.
           IF ALF-8 = "F" 
            PERFORM PROV-1 THRU PROV-EXIT 
           GO TO C6.
           MOVE SPACE TO RIGHT-7
           UNSTRING ALF-8 DELIMITED BY " " INTO RIGHT-7
           INSPECT RIGHT-7 REPLACING LEADING " " BY "0".
           MOVE RIGHT-7 TO PROV-KEY
           READ PROVCAID INVALID
           GO TO C6
           END-READ
           MOVE PROV-KEY TO REF-CDNUM.
           GO TO C5-1.
       C5.
           DISPLAY "NEW UPIN #"
           ACCEPT ALF-8
           IF ALF-8 = "?" DISPLAY "NEW # US OR X"
           GO TO C5.
           IF ALF-8 = "US" PERFORM US1 THRU US-EXIT GO TO C5.
           IF ALF-8 = "X" GO TO S1.
           MOVE ALF-8 TO UPIN-KEY
           START UPINFILE KEY NOT < UPIN-KEY INVALID
           DISPLAY "THIS MAY OR MAY NOT BE A VALID UPIN CODE"
           DISPLAY "BUT IT WILL BE ASSUMED TO BE CORRECT!"
           MOVE ALF-8 TO REF-UPIN GO TO C5-1.
           READ UPINFILE NEXT AT END DISPLAY "MAY BE BAD "
           MOVE ALF-8 TO REF-UPIN GO TO C5-1.
           IF UPIN-KEY NOT = ALF-8 DISPLAY "MAY BE BAD "
           MOVE ALF-8 TO REF-UPIN GO TO C5-1.
           DISPLAY UPIN-NAME " UPIN: " UPIN-KEY.
           MOVE ALF-8 TO REF-UPIN.
           INSPECT REF-UPIN REPLACING ALL "#" BY " ".
           GO TO C5-1.
       C7. DISPLAY "NPI NUMBER".
           ACCEPT ALF-10.
           IF ALF-10 = "?"
           DISPLAY "ENTER THE NPI # 10-DIGITS"
           DISPLAY "NT = SEARCH FOR NPI AND TAXONOMY CODES"
           DISPLAY "ENTER BK TO BACK TO MEDICAID"
           DISPLAY "ENTER X TO RETURN TO FUNCTION"
           GO TO C7.
           IF ALF-10 = "NT" PERFORM PROV-1 THRU PROV-EXIT
           GO TO C7.
           IF ALF-10 = "X" GO TO S1.
           IF ALF-10 = SPACE DISPLAY "NPI NOT KNOWN"
           MOVE SPACE TO REF-NPI GO TO C5-1.
           IF ALF-10 NOT NUMERIC DISPLAY "INVALID" GO TO C7.
           MOVE ALF-10 TO REF-NPI.
           GO TO C5-1.
       C8. DISPLAY "LASTNAME;FIRST NAME --  FORMAT".
           ACCEPT ALF24.
           IF ALF-7 = "?"
           DISPLAY "ENTER THE NAME OF THE PROVIDER"
           DISPLAY "ENTER X TO RETURN TO FUNCTION"
           GO TO C8.
           IF ALF24 = "X" GO TO S1.
           MOVE SPACE TO NAMELAST NAMEFIRST
           UNSTRING ALF24 DELIMITED BY ";" INTO
           NAMELAST NAMEFIRST
           IF (NAMEFIRST = SPACE) OR (NAMELAST = SPACE)
           DISPLAY "BOTH FIRST AND LAST NAMES ARE MANDATORY!"
           GO TO C8.
           IF ALF24(1:1) NOT = REF-NAME(1:1)
           DISPLAY "LASTNAME MUST START WITH PREVIOUS NAME LETTER"
           GO TO C8.
           MOVE ALF24 TO REF-NAME.
           INSPECT REF-NAME REPLACING ALL "*" BY " ".
           INSPECT REF-NAME REPLACING ALL "~" BY " ".
           INSPECT REF-NAME REPLACING ALL "/" BY " ".
           INSPECT REF-NAME REPLACING ALL ":" BY " ".
           INSPECT REF-NAME REPLACING ALL "#" BY " ".
       C5-1.
           REWRITE REFPHY01. CLOSE REFPHY. OPEN I-O REFPHY.
           UNLOCK REFPHY RECORD
           GO TO S1.
       D1. READ REFPHY WITH LOCK INVALID DISPLAY "NOT ON FILE"
           GO TO S1.
           DISPLAY  REF-KEY " " REF-BSNUM " " REF-CRNUM " " REF-UPIN
           " " REF-CDNUM " " REF-NPI " " REF-NAME.
       D2. DISPLAY "DELETE Y/N ?".
           ACCEPT ANS.
           IF ANS = "N" DISPLAY "NO DELETE" UNLOCK REFPHY RECORD
           GO TO S1.
           IF ANS = "Y" DELETE REFPHY RECORD
           DISPLAY "RECORD DELETED" GO TO S1.
           GO TO D2.
       US1. DISPLAY "1 = BY LAST NAME"
           DISPLAY "2 = BY CARE #"
           DISPLAY "3 = BY UPIN #"
           DISPLAY "4 = BY CITY  "
           DISPLAY "X = NO SEARCH"
           ACCEPT ALF-4
           IF ALF-4 = "?" DISPLAY "X OR 1-4" GO TO US1.
           IF ALF-4 = "X" GO TO US-EXIT.
           IF ALF-4 = "1" OR "2" OR "3" OR "4" GO TO US1-1
           ELSE GO TO US1.
       US1-1. DISPLAY "STARTING POINT".
           MOVE 0 TO X
           ACCEPT ALF24
           IF ALF24 = "?" DISPLAY "X OR 1-4" GO TO US1-1.
           IF ALF-4 = "1" MOVE ALF24 TO UPIN-NAME
           START UPINFILE KEY NOT < UPIN-NAME INVALID 
           DISPLAY "END OF FILE" GO TO US1.
           IF ALF-4 = "2" MOVE ALF24 TO UPIN-PROVNUM
           START UPINFILE KEY NOT < UPIN-PROVNUM INVALID
           DISPLAY "END OF FILE" GO TO US1.
           IF ALF-4 = "3" MOVE ALF24 TO UPIN-KEY
           START UPINFILE KEY NOT < UPIN-KEY INVALID
           DISPLAY "END OF FILE" GO TO US1.
           IF ALF-4 = "4" MOVE ALF24 TO UPIN-CITY
           START UPINFILE KEY NOT < UPIN-CITY INVALID 
           DISPLAY "END OF FILE" GO TO US1.
       US2. READ UPINFILE NEXT AT END DISPLAY "END OF FILE"
           GO TO US-EXIT.
           ADD 1 TO X
           DISPLAY UPIN-PROVNUM " " UPIN-KEY " " UPIN-NAME " " UPIN-CITY
           IF X > 8 DISPLAY "?" MOVE 0 TO X ACCEPT ANS
           IF ANS NOT = SPACE GO TO US-EXIT.
           GO TO US2.
       US-EXIT. EXIT.
       PROV-1. 
           DISPLAY "1 = BY CAID #"
           DISPLAY "2 = BY LAST NAME"
           DISPLAY "3 = BY NPI "
           DISPLAY "4 = BY TAXONOMY"
           DISPLAY "X = NO SEARCH"
           ACCEPT ALF-4
           IF ALF-4 = "?"  GO TO PROV-1.
           IF ALF-4 = "X" GO TO PROV-EXIT.
           IF ALF-4 = "1" OR "2" OR "3" OR "4" OR "5" GO TO PROV-2.
           GO TO PROV-1.
       PROV-2.  
           DISPLAY "STARTING POINT FOR SEARCH".
           MOVE 0 TO X
           ACCEPT ALF24
           IF ALF24 = "?" DISPLAY "START PT." GO TO PROV-2.
           IF ALF-4 = "1" 
           MOVE ALF24 TO PROV-KEY
           START PROVCAID KEY NOT < PROV-KEY INVALID
           DISPLAY "END OF FILE" GO TO PROV-1.
           IF ALF-4 = "2" MOVE ALF24 TO PROV-NAME
           START PROVCAID KEY NOT < PROV-NAME INVALID 
           DISPLAY "END OF FILE" GO TO PROV-1.
           IF ALF-4 = "3" 
           MOVE ALF24 TO PROV-NPI
           START PROVCAID KEY NOT < PROV-NPI INVALID
           DISPLAY "END OF FILE" GO TO PROV-1.
           IF ALF-4 = "4" 
           MOVE ALF24 TO PROV-TAX
           START PROVCAID KEY NOT < PROV-TAX INVALID
           DISPLAY "END OF FILE" GO TO PROV-1.

           MOVE 0 TO Y.
       PROV-3. 
           READ PROVCAID NEXT AT END DISPLAY "END OF FILE"
           GO TO PROV-EXIT.
           ADD 1 TO Y
           DISPLAY Y " " PROV-KEY " " PROV-NAME " " PROV-NPI " "
           PROV-TAX " " PROV-CITY
           MOVE PROV-KEY TO PROVTAB(Y)
           IF Y > 8
            MOVE 0 TO Y  
            ACCEPT ANS
            IF ANS NOT = SPACE 
             GO TO PROV-EXIT
            END-IF
           END-IF.
           GO TO PROV-3.
       PROV-EXIT. 
           EXIT.

       AU-1. DISPLAY "CARE # OR F TO FIND A VALID UPIN".
             ACCEPT UPIN-PROVNUM
             IF UPIN-PROVNUM = "X" GO TO S1.
             IF UPIN-PROVNUM = "F"
           PERFORM US1 THRU US-EXIT GO TO AU-1.
             READ UPINFILE INVALID DISPLAY "BAD UPIN" GO TO AU-1.
           DISPLAY UPIN-PROVNUM " " UPIN-KEY " " UPIN-NAME " " UPIN-CITY
           " ".
       AU-2. DISPLAY "ADD Y/N"
             ACCEPT ALF1
             IF ALF1 NOT = "Y" GO TO AU-1.
           MOVE UPIN-NAME TO TEST-NAME
           MOVE 1 TO X
           MOVE SPACE TO TEST-KEY.
       AU-3.
           STRING TN1 X DELIMITED BY "?" INTO REF-KEY
           READ REFPHY INVALID GO TO AU-4.
           ADD 1 TO X GO TO AU-3.
       AU-4.
           MOVE UPIN-PROVNUM TO REF-CRNUM
           MOVE UPIN-PROVNUM TO REF-CDNUM
           MOVE SPACE TO REF-BSNUM
           MOVE UPIN-NAME TO REF-NAME
           MOVE UPIN-KEY TO REF-UPIN
           MOVE SPACE TO REF-NPI
           WRITE REFPHY01  INVALID DISPLAY REF-KEY " BAD"
           GO TO S1.
           DISPLAY REF-KEY " " REF-NAME
           GO TO AU-1.
       ALFA.
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 26
            PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 36
             STRING TN1 ALFATAB1(X) ALFATAB2(Y) DELIMITED BY SIZE
             INTO TEST-KEY
             MOVE TEST-KEY TO REF-KEY
             READ REFPHY INVALID
              MOVE 1 TO FLAG
              MOVE 36 TO Y
              MOVE 26 TO X
             END-READ
            END-PERFORM
           END-PERFORM.
       Z1. CLOSE REFPHY. DISPLAY "END OF PHYSICIAN FILE MAINTANENCE".
           STOP RUN.
