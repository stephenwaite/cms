      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. oner234.
       AUTHOR. SWAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARMFILE ASSIGN TO "S30"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT GARFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT AGEDATE ASSIGN TO "S40"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S45"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT INSIN ASSIGN TO "S50"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILE-OUT ASSIGN TO "S55"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT CHARCUR ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT PAYCUR ASSIGN TO "S65" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.
           SELECT PAYFILE ASSIGN TO "S70" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PAYFILE-KEY
           LOCK MODE MANUAL.
           SELECT FILEOUT3 ASSIGN TO "S75"
           ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FILEOUT3.
       01  FILEOUT301 PIC X(160).
       FD  GARFILE
      *    BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS G-MASTER.
       01  G-MASTER.
           02 G-GARNO PIC X(8).
           02 G-GARNAME PIC X(24).
           02 G-BILLADD PIC X(22).
           02 G-STREET PIC X(22).
           02 G-CITY PIC X(18).
           02 G-STATE PIC X(2).
           02 G-ZIP PIC X(9).
           02 G-COLLT PIC X.
           02 G-PHONE PIC X(10).
           02 G-SEX PIC X.
           02 G-RELATE PIC X.
           02 G-MSTAT PIC X.
           02 G-DOB PIC X(8).
           02 G-DUNNING PIC X.
           02 G-ACCTSTAT PIC X.
           02 G-PR-MPLR PIC X(4).
           02 G-PRINS PIC XXX.
           02 G-PR-ASSIGN PIC X.
           02 G-PR-OFFICE PIC X(4).
           02 G-PR-GROUP PIC X(10).
           02 G-PRIPOL PIC X(16).
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-SE-MPLR PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
           02 G-SE-GROUP PIC X(10).
           02 G-SECPOL PIC X(16).
           02 G-SENAME PIC X(24).
           02 G-SE-RELATE PIC X.
           02 G-COPAY PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.
           02 G-ACCT PIC X(8).
           02 G-PRGRPNAME PIC X(15).
           02 G-SEGRPNAME PIC X(15).

       FD  PARMFILE.
       01  PARMFILE01 PIC 9.
       FD  AGEDATE.
       01  AGEDATE01.
           02 AD1 PIC 99.
           02 AD2 PIC 99.
           02 AD3 PIC 99.
       FD  FILEOUT
           DATA RECORD IS FILEOUT01.
       01  FILEOUT01.
           02 FO-PAYCODE PIC XXX.
           02 CC-DATE-T PIC X(8).
           02 FO-GARNAME PIC X(18).
           02 FILLER PIC X VALUE SPACE.
           02 FO-PRINS  PIC XXX.
           02 FILLER PIC X.
           02 FO-SEINS PIC XXX.
           02 FILLER PIC X.
           02 FO-TRINS PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-DIAG PIC X(7).
           02 CC-PROC PIC X(11).
           02 CC-MOD2 PIC XX.
           02 FO-AMOUNT PIC S9(4)V99.
           02 FO-DOCP PIC X(2).
           02 FO-PG PIC X(10).
           02 FO-PP PIC X(16).
           02 FO-SP PIC X(16).
           02 FO-AGE PIC X(8).
           02 FO-PLACE PIC X.
           02 FO-KEY PIC X(11).
       FD  PAYCUR
           BLOCK CONTAINS 6 RECORDS
           DATA RECORD IS PAYCUR01.
       01  PAYCUR01.
           02 PAYCUR-KEY.
             03 PC-KEY8 PIC X(8).
             03 PC-KEY3 PIC XXX.
           02 PC-AMOUNT PIC S9(4)V99.
           02 PC-PAYCODE PIC XXX.
           02 PC-DENIAL PIC XX.
           02 PC-CLAIM PIC X(6).
           02 PC-DATE-T PIC X(8).
           02 PC-DATE-E PIC X(8).
           02 PC-BATCH PIC X(6).
       FD  PAYFILE
      *    BLOCK CONTAINS 4 RECORDS
           DATA RECORD IS PAYFILE01.
       01  PAYFILE01.
           02 PAYFILE-KEY.
             03 PD-KEY8 PIC X(8).
             03 PD-KEY3 PIC XXX.
           02 PD-NAME PIC X(24).
           02 PD-AMOUNT PIC S9(4)V99.
           02 PD-PAYCODE PIC XXX.
           02 PD-DENIAL PIC XX.
           02 PD-CLAIM PIC X(6).
           02 PD-DATE-T PIC X(8).
           02 PD-DATE-E PIC X(8).
           02 PD-ORDER PIC X(6).
           02 PD-BATCH PIC X(6).

       FD  INSIN
           DATA RECORD IS INSIN01.
       01  INSIN01 PIC 999.
       FD FILE-OUT
           DATA RECORD IS FILE-OUT01.
       01  FILE-OUT01.
           02 FILEOUT-KEY PIC X(11).
           02 FO-PC PIC XXX.
           02 FO-PATID PIC X(8).
           02 FO-DATE PIC X(8).
           02 FO-ASSIGN PIC X.
           02 FO-PLACE1 PIC X.
           02 FO-DOC PIC XX.
       FD  CHARCUR
           BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS CHARCUR01.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC PIC X(11).
           02 CC-MOD2 PIC XX.
           02 CC-MOD3 PIC XX.
           02 CC-MOD4 PIC XX.
           02 CC-AMOUNT PIC S9(4)V99.
           02 CC-DOCR PIC X(3).
           02 CC-DOCP PIC X(2).
           02 CC-PAYCODE PIC 999.
           02 CC-STUD PIC X.
           02 CC-WORK PIC XX.
           02 CC-DAT1 PIC X(8).
           02 CC-RESULT PIC X.
           02 CC-ACT PIC X.
           02 CC-SORCREF PIC X.
           02 CC-COLLT PIC X.
           02 CC-AGE PIC X.
           02 CC-PAPER PIC X.
           02 CC-PLACE PIC X.
           02 CC-EPSDT PIC X.
           02 CC-DATE-T PIC X(8).
           02 CC-DATE-A PIC X(8).
           02 CC-DATE-P PIC X(8).
           02 CC-REC-STAT PIC X.
           02 CC-DX2 PIC X(7).
           02 CC-DX3 PIC X(7).
           02 CC-ACC-TYPE PIC X.
           02 CC-DATE-M PIC X(8).
           02 CC-ASSIGN PIC X.
           02 CC-NEIC-ASSIGN PIC X.
           02 CC-DX4 PIC X(7).
           02 CC-DX5 PIC X(7).
           02 CC-DX6 PIC X(7).
           02 CC-FUTURE PIC X(6).
       WORKING-STORAGE SECTION.
       01  ALF1 PIC X.
       01  X PIC 9999.
       01  INSTAB01.
           02 INSTAB PIC 999 OCCURS 999 TIMES.
       01  CLAIM-TOT PIC S9(5)V99.
       01  ANS PIC XXX.
       01  NUM3 PIC 999.
       01  ALF2 PIC X.
       01  ALF3 PIC X.
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT AGEDATE PARMFILE.
      *     READ PARMFILE AT END DISPLAY "NO DATE" GO TO P6.
      *     READ AGEDATE AT END DISPLAY "NO AGEDATE FILE" GO TO P6.
           OPEN INPUT GARFILE PAYCUR PAYFILE INSIN CHARCUR.
           OPEN OUTPUT FILEOUT FILE-OUT FILEOUT3.
           DISPLAY "ENTER A 3-DIGIT INSURANCE CODE  X= QUIT"
           ACCEPT ANS
           IF ANS = "X  " GO TO P6.
           DISPLAY " A=ONLY ASSIGNED  U=ONLY UNASSIGNED B=BOTH"
           ACCEPT ALF2.
           IF NOT (ALF2 = "A" OR "U") MOVE "B" TO ALF2.
           MOVE ANS TO NUM3
           DISPLAY "P=PRIMARY S=SECONDARY <CR> = BOTH"
           ACCEPT ALF1.
           IF NOT (ALF1 = "P" OR "S") MOVE " " TO ALF1.
           DISPLAY "SHOW ZERO AMOUNT CHARGES? Y/N".
           ACCEPT ALF3.
           
           MOVE NUM3 TO CC-PAYCODE
           START CHARCUR KEY NOT < CC-PAYCODE INVALID GO TO P6.

       P1. 
           READ CHARCUR NEXT AT END GO TO P6.
           IF CC-PAYCODE NOT = NUM3 GO TO P6.
           IF CC-ASSIGN = "U" AND ALF2 = "A" GO TO P1.
           IF CC-ASSIGN = "A" AND ALF2 = "U" GO TO P1.
           IF ALF3 = "N"
            IF CC-AMOUNT = 0
              GO TO P1
            END-IF
           END-IF.
           COMPUTE CLAIM-TOT = CC-AMOUNT
           PERFORM S4 THRU S4-EXIT.
           IF CLAIM-TOT NOT > 0 GO TO P1.
           MOVE CORR CHARCUR01 TO FILEOUT01
           MOVE CC-PAYCODE TO FO-PAYCODE
           MOVE CHARCUR-KEY TO FO-KEY
           COMPUTE FO-AMOUNT = CLAIM-TOT
           MOVE CC-KEY8 OF CHARCUR01 TO G-GARNO
           READ GARFILE INVALID  GO TO P1.
           IF NOT ((ALF1 = "P" AND (FO-PAYCODE = G-PRINS))
           OR (ALF1 = "S" AND (FO-PAYCODE = G-SEINS))
           OR (ALF1 = SPACE))
      *     DISPLAY G-PRINS " " G-SEINS
      *     DISPLAY CC-KEY8
      *     DISPLAY ALF1 " ALF1 " FO-PAYCODE " FO-PAYCODE"
      *     ACCEPT ANS
           GO TO P1
           END-IF.
           MOVE G-GARNAME TO FO-GARNAME
           MOVE G-PR-GROUP TO FO-PG
           MOVE G-PRINS TO FO-PRINS
           MOVE G-SEINS TO FO-SEINS
           MOVE G-TRINS TO FO-TRINS
           MOVE G-PRIPOL TO FO-PP
           MOVE G-SECPOL TO FO-SP
           MOVE CC-DATE-A TO FO-AGE
           MOVE CC-DOCP TO FO-DOCP
           MOVE CC-PLACE TO FO-PLACE.
           IF (G-PRINS NOT = FO-PAYCODE) 
           AND (G-SEINS NOT = FO-PAYCODE) 
           AND (G-TRINS NOT = FO-PAYCODE) 
           MOVE SPACE TO FO-PP
           MOVE FO-PAYCODE TO FO-PP
           MOVE 001 TO FO-PAYCODE.
           WRITE FILEOUT01.
           WRITE FILEOUT301 FROM CHARCUR01
           MOVE CHARCUR-KEY OF CHARCUR01 TO FILEOUT-KEY
           MOVE CC-PATID OF CHARCUR01 TO FO-PATID OF FILE-OUT01
           MOVE CC-PAYCODE TO FO-PC OF FILE-OUT01
           MOVE CC-DATE-T OF CHARCUR01 TO FO-DATE OF FILE-OUT01
           MOVE CC-ASSIGN OF CHARCUR01 TO FO-ASSIGN OF FILE-OUT01
           MOVE CC-PLACE OF CHARCUR01 TO FO-PLACE1 OF FILE-OUT01
           MOVE CC-DOCP OF CHARCUR01 TO FO-DOC OF FILE-OUT01
           WRITE FILE-OUT01.
           GO TO P1.
       A1. MOVE 0 TO INSTAB(X).
       S4. MOVE CC-KEY8 TO PC-KEY8.
           MOVE "000" TO PC-KEY3.
           START PAYCUR KEY NOT < PAYCUR-KEY INVALID GO TO S5.
       S7. READ PAYCUR NEXT AT END GO TO S5.
           IF PC-KEY8 NOT = CC-KEY8 GO TO S5.
           IF PC-CLAIM NOT = CC-CLAIM OF CHARCUR01 GO TO S7.
           ADD PC-AMOUNT TO CLAIM-TOT GO TO S7.
       S5. MOVE CC-KEY8 TO PD-KEY8.
           MOVE "000" TO PD-KEY3.
           START PAYFILE KEY NOT < PAYFILE-KEY INVALID GO TO S4-EXIT.
       S6. READ PAYFILE NEXT AT END GO TO S4-EXIT.
           IF PD-KEY8 NOT = CC-KEY8 GO TO S4-EXIT.
           IF PD-CLAIM NOT = CC-CLAIM OF CHARCUR01 GO TO S6.
           ADD PD-AMOUNT TO CLAIM-TOT GO TO S6.
       S4-EXIT. EXIT.
       P6. CLOSE FILEOUT FILE-OUT FILEOUT3. STOP RUN.
