      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. one234.
       AUTHOR. SWAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PARMFILE ASSIGN TO "S30"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT GARFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
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

       FD  GARFILE.
           COPY GARFILE.CPY IN "C:\Users\sid\cms\copylib".     

       FD  PAYCUR.
           COPY PAYCUR.CPY IN "C:\Users\sid\cms\copylib".      

       FD  CHARCUR.
           COPY CHARCUR.CPY IN "C:\Users\sid\cms\copylib".             
       
       FD  PARMFILE.
       01  PARMFILE01 PIC 9.

       FD  AGEDATE.
       01  AGEDATE01.
           02 AD1 PIC 99.
           02 AD2 PIC 99.
           02 AD3 PIC 99.

       FD  FILEOUT.
       01  FILEOUT01.
           02 FO-PAYCODE PIC XXX.
           02 FO-DATE-T PIC X(8).
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
           02 CC-PROC PIC X(7).
           02 CC-MOD2 PIC XX.
           02 FO-AMOUNT PIC S9(4)V99.
           02 CC-DOCP PIC X(2).
           02 FO-PG PIC X(10).
           02 FO-PP PIC X(16).
           02 FO-SP PIC X(16).
           02 FO-AGE PIC X(8).
           02 FO-KEY PIC X(11).
 
       FD  PAYFILE.
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

       FD  INSIN.
       01  INSIN01 PIC 999.

       FD FILE-OUT.
       01  FILE-OUT01.
           02 FO-PC PIC XXX.
           02 FO-PATID PIC X(8).
           02 FILEOUT-KEY PIC X(11).
           02 FO-DATE PIC X(8).
           02 FO-ASSIGN PIC X.
           02 FO-PLACE PIC X.
           02 FO-DOC PIC XX.

       FD  FILEOUT3.
       01  FILEOUT301 PIC X(156).


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
           MOVE ANS TO CC-PAYCODE
           DISPLAY "P=PRIMARY S=SECONDARY <CR> = BOTH"
           ACCEPT ALF1.
           IF NOT (ALF1 = "P" OR "S") MOVE " " TO ALF1.
           DISPLAY "SHOW ZERO AMOUNT CHARGES? Y/N".
           ACCEPT ALF3.
           
           START CHARCUR KEY NOT < CC-PAYCODE INVALID GO TO P6.

       P1. 
           READ CHARCUR NEXT AT END GO TO P6.
           IF CC-PAYCODE NOT = ANS GO TO P6.
           IF CC-ASSIGN = "U" AND ALF2 = "A" GO TO P1.
           IF CC-ASSIGN = "A" AND ALF2 = "U" GO TO P1.
           IF (ALF3 = "N") AND (CC-AMOUNT = 0 ) GO TO P1.
           COMPUTE CLAIM-TOT = CC-AMOUNT
           PERFORM S4 THRU S4-EXIT.
      *     IF CLAIM-TOT NOT > 0 GO TO P1.
           MOVE CORR CHARCUR01 TO FILEOUT01
           MOVE CC-PAYCODE TO FO-PAYCODE
           MOVE CHARCUR-KEY TO FO-KEY
           COMPUTE FO-AMOUNT = CLAIM-TOT
           MOVE CC-KEY8 OF CHARCUR01 TO G-GARNO
           READ GARFILE INVALID  GO TO P1.
           IF NOT ((ALF1 = "P" AND (CC-PAYCODE = G-PRINS))
           OR (ALF1 = "S" AND (CC-PAYCODE = G-SEINS))
           OR (ALF1 = SPACE))
           GO TO P1
           END-IF.
           WRITE FILEOUT301 FROM CHARCUR01
           MOVE CHARCUR-KEY OF CHARCUR01 TO FILEOUT-KEY
           MOVE CC-PATID OF CHARCUR01 TO FO-PATID OF FILE-OUT01
           MOVE CC-PAYCODE TO FO-PC OF FILE-OUT01
           MOVE CC-DATE-T OF CHARCUR01 TO FO-DATE OF FILE-OUT01
           MOVE CC-ASSIGN OF CHARCUR01 TO FO-ASSIGN OF FILE-OUT01
           MOVE CC-PLACE OF CHARCUR01 TO FO-PLACE OF FILE-OUT01
           MOVE CC-DOCP OF CHARCUR01 TO FO-DOC OF FILE-OUT01
           MOVE CC-DATE-T TO FO-DATE-T
           MOVE G-GARNAME TO FO-GARNAME
           MOVE G-PRIPOL TO FO-PP
           MOVE G-SECPOL TO FO-SP
           MOVE CC-DATE-A TO FO-AGE
           MOVE G-PRINS TO FO-PRINS
           MOVE G-SEINS TO FO-SEINS
           WRITE FILE-OUT01.
           WRITE FILEOUT01
           GO TO P1.

       A1.
           MOVE 0 TO INSTAB(X).

       S4.
           MOVE CC-KEY8 TO PC-KEY8.
           MOVE "000" TO PC-KEY3.
           START PAYCUR KEY NOT < PAYCUR-KEY INVALID GO TO S5.

       S7.
           READ PAYCUR NEXT AT END GO TO S5.
           IF PC-KEY8 NOT = CC-KEY8 GO TO S5.
           IF PC-CLAIM NOT = CC-CLAIM OF CHARCUR01 GO TO S7.
           ADD PC-AMOUNT TO CLAIM-TOT GO TO S7.

       S5.
           MOVE CC-KEY8 TO PD-KEY8.
           MOVE "000" TO PD-KEY3.
           START PAYFILE KEY NOT < PAYFILE-KEY INVALID GO TO S4-EXIT.

       S6.
           READ PAYFILE NEXT AT END GO TO S4-EXIT.
           IF PD-KEY8 NOT = CC-KEY8 GO TO S4-EXIT.
           IF PD-CLAIM NOT = CC-CLAIM OF CHARCUR01 GO TO S6.
           ADD PD-AMOUNT TO CLAIM-TOT GO TO S6.

       S4-EXIT.
           EXIT.
           
       P6.
           CLOSE FILEOUT FILE-OUT FILEOUT3. STOP RUN.
