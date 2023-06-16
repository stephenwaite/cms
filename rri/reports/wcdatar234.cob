      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. wcdatar234.
       AUTHOR. SID WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARMFILE ASSIGN TO "S30"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT GARFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES.
           SELECT AGEDATE ASSIGN TO "S40"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S45"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT INSIN ASSIGN TO "S50"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILE-OUT ASSIGN TO "S55"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT CHARCUR ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS dynamic RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES.
           SELECT PAYCUR ASSIGN TO "S65" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PAYCUR-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD  GARFILE.
           copy garfile.cpy in "c:\users\sid\cms\copylib\rri".

       FD  PARMFILE.
       01  PARMFILE01 PIC 9.

       FD  AGEDATE.
       01  AGEDATE01.
           02 LOW-DATE PIC X(8).
           02 HIGH-DATE PIC X(8).

       FD  FILEOUT
           DATA RECORD IS FILEOUT01.
       01  FILEOUT01.
           02 FO-PAYCODE PIC XXX.
           02 CC-DATE-T PIC X(8).
           02 FO-GARNAME PIC X(18).
           02 FILLER PIC X VALUE SPACE.
           02 FO-PRINS  PIC XXX.
           02 FILLER PIC X VALUE "/".
           02 FO-SEINS PIC XXX.
           02 FILLER PIC X VALUE "/".
           02 FO-TRINS PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-DIAG PIC X(7).
           02 FO-PROC PIC X(7).
           02 CC-MOD2 PIC XX.
           02 FO-AMOUNT PIC S9(4)V99.
           02 CC-DOCP PIC X(2).
           02 FO-PG PIC X(10).
           02 FO-PP PIC X(16).
           02 FO-SP PIC X(16).
           02 FO-AGE PIC X(8).
           02 FO-KEY PIC X(11).
       FD  PAYCUR.
           copy paycur.cpy in "c:\users\sid\cms\copylib".

       FD  INSIN.
       01  INSIN01 PIC 999.

       FD FILE-OUT.
       01  FILE-OUT01 PIC X(156).

       FD  CHARCUR.
           copy charcur.cpy in "c:\users\sid\cms\copylib\rri".

       WORKING-STORAGE SECTION.
       01  X PIC 9999.
       01  INSTAB01.
           02 INSTAB PIC 999 OCCURS 999 TIMES.
       01  CLAIM-TOT PIC S9(5)V99.
       01  DATE-X.
           02 DATE-YYYY PIC 9(4).
           02 DATE-MM PIC 99.
           02 DATE-DD PIC 99.
       01  DATE-Y PIC X(8).

       PROCEDURE DIVISION.
       P0.
           OPEN INPUT AGEDATE PARMFILE.
           READ PARMFILE AT END DISPLAY "NO DATE" GO TO P6.
           ACCEPT DATE-X FROM CENTURY-DATE
           IF DATE-MM NOT > PARMFILE01
           COMPUTE DATE-MM = 14 - PARMFILE01
           COMPUTE DATE-YYYY = DATE-YYYY - 1
           ELSE 
           COMPUTE DATE-MM = DATE-MM - PARMFILE01
           END-IF.
           MOVE DATE-X TO DATE-Y.
           READ AGEDATE AT END DISPLAY "NO AGEDATE FILE" GO TO P6.
           OPEN INPUT GARFILE PAYCUR INSIN CHARCUR.
           OPEN OUTPUT FILEOUT FILE-OUT.
           PERFORM A1 VARYING X FROM 1 BY 1 UNTIL X > 999.

       P00. 
           READ INSIN AT END GO TO P1.
           MOVE 1 TO INSTAB(INSIN01) GO TO P00.

       P1. 
           READ CHARCUR next
             AT END 
               GO TO P6.

           IF CC-ASSIGN NOT = "A" GO TO P1.
           IF CC-PAYCODE = 018 GO TO P1.
      *     IF CC-DATE-T of CHARCUR01 < LOW-DATE GO TO P1.
      *    IGNORE CLAIM AGE DATE     
      *     IF CC-DATE-A of CHARCUR01 > HIGH-DATE GO TO P1.
      *    IGNORE RECENT CLAIMS     
      *     IF CC-DATE-T of CHARCUR01 > HIGH-DATE GO TO P1.
           IF NOT (CC-ACC-TYPE = "0" OR "1")
             GO TO P1.


      *     IF (CC-DATE-A NOT = "00000000")
      *     AND (CC-DATE-A > DATE-Y) GO TO P1.
           
           IF INSTAB(CC-PAYCODE) = 1 GO TO P1.

           MOVE CC-AMOUNT TO CLAIM-TOT
           PERFORM S4 THRU S4-EXIT.
           IF CLAIM-TOT > 0 NEXT SENTENCE ELSE GO TO P1.
           MOVE CORR CHARCUR01 TO FILEOUT01
           MOVE CC-PAYCODE TO FO-PAYCODE
           MOVE CHARCUR-KEY TO FO-KEY
           MOVE CLAIM-TOT TO FO-AMOUNT
           STRING CC-PROC1 CC-PROC2 DELIMITED BY SIZE
             INTO FO-PROC
           MOVE CC-KEY8 OF CHARCUR01 TO G-GARNO
           READ GARFILE INVALID GO TO P1.
           MOVE G-GARNAME TO FO-GARNAME
           MOVE G-PRINS TO FO-PRINS
           MOVE G-SEINS TO FO-SEINS
           MOVE G-TRINS TO FO-TRINS
           MOVE G-PR-GROUP TO FO-PG
           MOVE G-PRIPOL TO FO-PP
           MOVE G-SECPOL TO FO-SP
           MOVE CC-DATE-A TO FO-AGE.
           IF (G-PRINS NOT = CC-PAYCODE) 
           AND (G-SEINS NOT = CC-PAYCODE) 
           AND (G-TRINS NOT = CC-PAYCODE)
           MOVE SPACE TO FO-PP
           MOVE CC-PAYCODE TO FO-PP
           MOVE 001 TO FO-PAYCODE.
           WRITE FILEOUT01.
           WRITE FILE-OUT01 FROM CHARCUR01.
           GO TO P1.
       A1. MOVE 0 TO INSTAB(X).
       S4. MOVE CC-KEY8 TO PC-KEY8.
           MOVE "000" TO PC-KEY3.
           START PAYCUR KEY > PAYCUR-KEY INVALID GO TO S4-EXIT.
       S7. READ PAYCUR NEXT AT END GO TO S4-EXIT.
           IF PC-KEY8 NOT = CC-KEY8 GO TO S4-EXIT.
           IF PC-CLAIM NOT = CC-CLAIM OF CHARCUR01 GO TO S7.
           ADD PC-AMOUNT TO CLAIM-TOT GO TO S7.
       S4-EXIT. EXIT.
       P6. CLOSE FILEOUT FILE-OUT. STOP RUN.
