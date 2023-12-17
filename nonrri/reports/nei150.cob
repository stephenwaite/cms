      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NEI150.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUT ASSIGN TO "S30"
           ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD OUT.
       01  OUT01 PIC X(16).
       WORKING-STORAGE SECTION.
       01  INPUT-DATE-S.
           02 T-MM PIC XX.
           02 FILLER PIC X VALUE "/".
           02 T-DD PIC XX.
           02 FILLER PIC X VALUE "/".
           02 T-CC PIC XX.
           02 T-YY PIC XX.
       01  DATE-S.
           02 S-MM PIC XX.
           02 S-DD PIC XX.
           02 S-CC PIC XX.
           02 S-YY PIC XX.

       01  TODAY-DATE.
           02 TT-CC PIC XX.
           02 TT-YY PIC XX.
           02 TT-MM PIC XX.
           02 TT-DD PIC XX.
       01  ENTER01.
           02 LOW-ENTER PIC X(8).
           02 HIGH-ENTER PIC X(8).
       01  DATE01.
           02 LOW-DATE PIC X(8).
           02 HIGH-DATE PIC X(8).
       01  RT-3 PIC XXX JUST RIGHT.
       01  ALF81 PIC X(8).
       01  ALF82 PIC X(8).
       01  ALF-T PIC X(8).
       01  FLAG PIC 9.
       01  PCNUM PIC XXX.
       01  LISTVALUE PIC X.
       01  FORMOUT PIC X.
       01  ANS PIC X(20).
       PROCEDURE DIVISION.
       0005-START.
           OPEN OUTPUT OUT.
       A1. DISPLAY "DATA ENTRY RANGE"
           DISPLAY "DD OR MMDD OR MMDDYY OR MMDDYYYY"
           ACCEPT ANS.
           IF ANS = "END" MOVE "Z" TO OUT01 WRITE OUT01
           GO TO R20.
           IF ANS = "?"
           DISPLAY "T = DATA ENTERED TODAY"
           DISPLAY "ALL = ALL RECORDS"
           DISPLAY "LOWDATE HIGHDATE  MISSING PARTS ASSUMED CURR."
           DISPLAY "SEPARATE DATES WITH - OR TO"
           DISPLAY "1028 - 1104  ASSUMES CURRENT YEAR."
           DISPLAY "DIFFERENT YEARS REQUIRES FULL DATE MMDDYYYY."
           GO TO A1.
           IF ANS = "ALL" MOVE ZERO TO LOW-ENTER 
           MOVE "99999999" TO HIGH-ENTER
           GO TO A2.
           IF ANS = "T" ACCEPT LOW-ENTER FROM CENTURY-DATE
           MOVE LOW-ENTER TO HIGH-ENTER GO TO A2.
           MOVE 0 TO FLAG.
           PERFORM D1 THRU D1-EXIT.
           IF FLAG = 1 DISPLAY "INVALID" GO TO A1.
           MOVE ALF81 TO LOW-ENTER.
           MOVE ALF82 TO HIGH-ENTER.
       A2. DISPLAY "TRANSACTION DATE RANGE".
           ACCEPT ANS.
           IF ANS = "?"
           DISPLAY "ENTER RANGE OF DATES ON TRANSACTIONS"
           DISPLAY "SAME RULES APPLY AS FOR DATE ENTERED"
           DISPLAY "DD OR MMDD OR MMDDYY OR MMDDYYYY"
           DISPLAY "BK = BACK TO DATE ENTERED RANGE"
           GO TO A2.
           IF ANS = "BK" GO TO A1.
           IF ANS = "ALL" MOVE ZERO TO LOW-DATE 
           MOVE "99999999" TO HIGH-DATE
           GO TO A3.
           IF ANS = "T" ACCEPT LOW-DATE FROM CENTURY-DATE
           MOVE LOW-DATE TO HIGH-DATE GO TO A3.
           MOVE 0 TO FLAG.
           PERFORM D1 THRU D1-EXIT.
           IF FLAG = 1 DISPLAY "INVALID" GO TO A2.
           MOVE ALF81 TO LOW-DATE.
           MOVE ALF82 TO HIGH-DATE.
       A3. DISPLAY "PAYORCODE".
           ACCEPT ANS
           IF ANS = "?"
           DISPLAY "<CR> TO INCLUDE ALL PAYORCODES"
           DISPLAY "ANY PAYORCODE"
           DISPLAY "BK = BACK TO TRANSACTION DATE RANGE"
           GO TO A3.
           IF ANS = "BK" GO TO A2.
           IF ANS = SPACE MOVE "000" TO PCNUM GO TO A4.
           UNSTRING ANS DELIMITED BY " " INTO RT-3.
           INSPECT RT-3 REPLACING ALL " " BY "0".
           IF (RT-3 NOT NUMERIC ) OR ( RT-3 = "000" )
           DISPLAY "INVALID" GO TO A3.
           MOVE RT-3 TO PCNUM.
       A4. DISPLAY "LIST DETAIL Y/N".
           ACCEPT ANS.
           IF ANS = "?"
           DISPLAY "N = PRINT TOTALS FOR REQUESTED RECORDS"
           DISPLAY "Y = DISPLAY BOTH TOTALS AND DETAIL"
           DISPLAY "BK = BACK TO PAYORCODE"
           GO TO A4.
           IF ANS = "BK" GO TO A3.
           IF ANS = "N" MOVE "0" TO LISTVALUE ANS GO TO A6.
           IF ANS NOT = "Y" GO TO A4.
           MOVE "1" TO LISTVALUE.
       A5. DISPLAY "FORMAT FOR PRINTING".
           ACCEPT ANS.
           IF ANS = "?"
           DISPLAY "1 = GARNO PROC $ "
           DISPLAY "2 = GARNO PROC $ NAME"
           DISPLAY "3 = GARNO PROC $ TRANS DATE PAYCODE"
           DISPLAY "4 = GARNO $ PAYCODE TRANS DATE ENTRY DATE"
           DISPLAY "5 = GARNO PROC $ PAYCODE TRANS DATE ENTRY DATE"
           DISPLAY "6 = GARNO NAME PROC $ PC TRANS DATE ENTRY DATE"
           DISPLAY "SELECT NUMBER OF PRINTED FORMAT DESIRED"
           GO TO A5.
           IF ANS = "BK" GO TO A4.
           IF ANS < "1" OR > "6" DISPLAY "INVALID " GO TO A5.
       A6.
           MOVE ANS TO FORMOUT.
           WRITE OUT01 FROM LISTVALUE.
           WRITE OUT01 FROM FORMOUT.
           WRITE OUT01 FROM ENTER01.
           WRITE OUT01 FROM DATE01.
           WRITE OUT01 FROM PCNUM.
       R20. CLOSE OUT. STOP RUN.
       D1.
           MOVE SPACE TO ALF81 ALF82.
           UNSTRING ANS DELIMITED BY " TO " OR " - " OR "TO " OR "-"
           OR " TO" OR " -" INTO ALF81 ALF82.
           IF ALF81 = SPACE MOVE "00000000" TO ALF81.
           IF ALF82 = SPACE MOVE ALL "9" TO ALF82.
           MOVE 0 TO FLAG
           MOVE ALF81 TO ALF-T
           IF ALF-T NOT = "00000000"
           PERFORM D3 THRU D3-EXIT.
           IF FLAG = 1 GO TO D1-EXIT.
           MOVE ALF-T TO ALF81.
           MOVE ALF82 TO ALF-T.
           IF ALF-T NOT = "99999999"
           PERFORM D3 THRU D3-EXIT.
           MOVE ALF-T TO ALF82
           IF ALF81 > ALF82 MOVE 1 TO FLAG
           DISPLAY "LOWDATE > HIGHDATE !".
       D1-EXIT. EXIT.
       D3. 
           INSPECT ALF-T REPLACING ALL " " BY "0".
           IF ALF-T NOT NUMERIC DISPLAY "NOT NUMERIC"
           MOVE 1 TO FLAG GO TO D3-EXIT.
      *     DISPLAY ALF-T " ALF-T NUMERICIZED"
           IF ALF-T = "00000000"  OR ALF-T = "99999999" GO TO D3-EXIT.
           MOVE ALF-T TO DATE-S.
           IF S-CC = "00" AND S-YY = "00" AND S-DD = "00"
           MOVE S-MM TO S-DD MOVE "00" TO S-MM.
           ACCEPT TODAY-DATE FROM CENTURY-DATE.
      *     DISPLAY TODAY-DATE " TODAY'S DATE"
           IF S-CC = "00" MOVE TT-CC TO S-CC.
           IF S-YY = "00" MOVE TT-YY TO S-YY.
           IF S-MM = "00" MOVE TT-MM TO S-MM.
           IF S-MM > "12" MOVE 1 TO FLAG GO TO D3-EXIT.
           IF S-DD > "31" MOVE 1 TO FLAG GO TO D3-EXIT.
           MOVE S-CC TO TT-CC 
           MOVE S-YY TO TT-YY
           MOVE S-MM TO TT-MM
           MOVE S-DD TO TT-DD
      *     DISPLAY TODAY-DATE " ALF-T FIXED"
           MOVE TODAY-DATE TO ALF-T.
       D3-EXIT. EXIT.
