      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrr134.
       AUTHOR. SWAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT INSIN ASSIGN TO "S30" ORGANIZATION LINE
           SEQUENTIAL.

           SELECT FILE-OUT ASSIGN TO "S35" ORGANIZATION LINE
           SEQUENTIAL.

           SELECT CHARCUR ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS IS SEQUENTIAL RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES.

           SELECT PAYCUR ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PAYCUR-KEY.

           SELECT GARFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS IS RANDOM RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES.

           SELECT FILEOUT2 ASSIGN TO "S55" ORGANIZATION LINE
           SEQUENTIAL.

           SELECT CAREFILE ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS CARE-KEY
           LOCK MODE MANUAL.

           SELECT ERRORFILE ASSIGN TO "S65" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  CAREFILE.
       01  CAREFILE01.
           02 CARE-KEY.
              03 CR-KEY8 PIC X(8).
              03 CR-DATE PIC X(8).
              03 CR-PROC PIC X(5).
              03 CR-MOD1 PIC XX.
              03 CR-MOD2 PIC XX.
           02 CR-PAYDATE PIC X(8).
           02 CR-DOCP    PIC X(6).
           02 CR-POS     PIC XX.
           02 CR-BILLED PIC 9(4)V99.
           02 CR-ALLOWED PIC 9(4)V99.
           02 CR-DEDUCT  PIC 9(4)V99.
           02 CR-PAYED   PIC 9(4)V99.
           02 CR-DENIAL1 PIC X(4).
           02 CR-DENIAL2 PIC X(4).
           02 CR-DENIAL3 PIC X(4).
           02 CR-DENIAL4 PIC X(4).
           02 CR-PAYDENIAL PIC X(4).
           02 CR-ICN PIC X(13).
           02 CR-CK-EFT PIC X(9).
           02 CR-INSNAME PIC X(30).

       FD  PAYCUR
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

       FD  INSIN
           DATA RECORD IS INSIN01.
       01  INSIN01.
           02 INS-1 PIC 999.
           02 INS-2 PIC X.
           
       FD FILE-OUT
           DATA RECORD IS FILE-OUT01.
       01  FILE-OUT01.
           02 FO-PC PIC XXX.
           02 FO-PATID PIC X(8).
           02 FILEOUT-KEY PIC X(11).
           02 FO-DATE PIC X(8).
           02 FO-ASSIGN PIC X.
           02 FO-PLACE PIC X.
           02 FO-DOCP PIC XX.
           02 FO-CHCRR PIC X(12).
           02 FILLER PIC X(4).
           02 FO-PAPER PIC X.

       FD  CHARCUR
           DATA RECORD IS CHARCUR01.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC.
              03 CC-PROC0 PIC X(4).
              03 CC-PROC1 PIC X(5).
              03 CC-PROC2 PIC XX.
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
           02 CC-IOPAT PIC X.
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

       FD GARFILE
           DATA RECORD IS G-MASTER.
       01 G-MASTER.
           02 G-GARNO PIC X(8).
           02 G-GARNAME.
             03 GN1 PIC X.
             03 GN2 PIC X(23).
           02 G-BILLADD.
             03 G-BILLADD1 PIC X.
             03 G-BILLADD2 PIC X(21).
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
           02 G-SE-OFFICE PIC X(4).
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

       FD  FILEOUT2.
       01  FILEOUT201 PIC X(160).
        
       FD  ERRORFILE
           DATA RECORD IS ERRORFILE01.
       01  ERRORFILE01.
           02 EF1 PIC X(12).
           02 EF2 PIC X(37).
           02 EF3 PIC X(24).
       
       WORKING-STORAGE SECTION.
       01  X USAGE IS INDEX.
       01  INSTAB01.
           02 INSTAB PIC 999 OCCURS 999 TIMES.
       01  TOT-AMT PIC S9(4)V99.
      
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT PAYCUR INSIN CHARCUR GARFILE CAREFILE
           OUTPUT FILE-OUT FILEOUT2 ERRORFILE.
           MOVE SPACE TO FILE-OUT01
           PERFORM A1 VARYING X FROM 1 BY 1 UNTIL X > 999.

       P00. 
           READ INSIN
             AT END
               GO TO P1
           END-READ

           IF INS-2 = " "
               MOVE 1 TO INSTAB(INS-1)
           ELSE
               MOVE 2 TO INSTAB(INS-1)
           END-IF
           
           GO TO P00.

       P1. 
           READ CHARCUR
             AT END
               GO TO P6
           END-READ

           IF CC-DATE-T < "20170801"
               GO TO P1
           END-IF    

           IF CC-AMOUNT = 0
              GO TO P1 
           END-IF   
           
           IF INSTAB(CC-PAYCODE) = 2 
               GO TO P1
           END-IF    
           
           IF CC-REC-STAT > "1" GO TO P1.
      
           IF CC-PAYCODE = 003 OR 004 OR 028 OR 064 OR 197
               GO TO P1
           END-IF    
           
           IF CC-DOCP = "02"
               MOVE SPACE TO ERRORFILE01
               MOVE "BAD DOC ## AND/OR DIAG ?" TO EF2
               PERFORM S1 
               GO TO P1
           END-IF
           
           IF CC-PAPER = "P" OR "O" GO TO P1-1.

           IF CC-ASSIGN = "U" GO TO P1.

           IF CC-PAYCODE = 062 GO TO P1-3.

           IF CC-PAPER = " " GO TO P1.  

       P1-1.
           MOVE CC-KEY8 TO G-GARNO
           READ GARFILE
             INVALID
               DISPLAY CHARCUR-KEY " BAD GARNO"
               GO TO P1.

           IF (CC-PAYCODE = G-PRINS)
             AND (CC-PAPER = "E")
             GO TO P1.

           MOVE SPACE TO FO-CHCRR
           IF (CC-PLACE = "M" OR "C")
             IF CC-PAYCODE = G-PRINS
               MOVE G-PR-GROUP TO FO-CHCRR
             END-IF
             IF CC-PAYCODE = G-SEINS
               MOVE G-SE-GROUP TO FO-CHCRR
             END-IF
           END-IF
           
           MOVE CHARCUR-KEY TO FILEOUT-KEY
           MOVE CC-PATID TO FO-PATID.
           MOVE CC-PAYCODE TO FO-PC
           MOVE CC-DATE-T TO FO-DATE
           MOVE CC-NEIC-ASSIGN TO FO-ASSIGN
           MOVE CC-PLACE TO FO-PLACE
           MOVE CC-DOCP TO FO-DOCP
           MOVE CC-PAPER TO FO-PAPER
           WRITE FILE-OUT01
           WRITE FILEOUT201 FROM CHARCUR01
           GO TO P1.
       P1-3.
           MOVE CC-KEY8 TO CR-KEY8
           MOVE CC-PROC1 TO CR-PROC
           MOVE CC-DATE-T TO CR-DATE
           MOVE SPACE TO CR-MOD1 CR-MOD2.
           START CAREFILE KEY NOT < CARE-KEY INVALID GO TO P1-1.
       P1-4.
           READ CAREFILE NEXT AT END GO TO P1-1.
           IF CR-KEY8 NOT = CC-KEY8 GO TO P1-1.
           IF CR-DATE NOT = CC-DATE-T GO TO P1-1.
           IF CR-PROC NOT = CC-PROC GO TO P1-4.
           IF CR-INSNAME NOT = SPACE GO TO P1.
           GO TO P1-1.

       A1. 
           MOVE 0 TO INSTAB(X).
       S1. 
           MOVE CHARCUR-KEY TO EF1 
           WRITE ERRORFILE01.
       
       S4.
           MOVE CC-KEY8 TO PC-KEY8.
           MOVE "000" TO PC-KEY3.
           COMPUTE TOT-AMT = CC-AMOUNT
           START PAYCUR KEY > PAYCUR-KEY INVALID GO TO S4-EXIT.
       S7. 
           READ PAYCUR NEXT AT END GO TO S4-EXIT.
           IF PC-KEY8 NOT = CC-KEY8 GO TO S4-EXIT.
           IF PC-CLAIM NOT = CC-CLAIM GO TO S7.
           ADD PC-AMOUNT TO TOT-AMT GO TO S7.
       S4-EXIT. 
           EXIT.
       P6. 
           CLOSE FILE-OUT FILEOUT2 ERRORFILE
                 PAYCUR INSIN CHARCUR GARFILE CAREFILE. 
           STOP RUN.
