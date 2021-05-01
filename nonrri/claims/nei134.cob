       IDENTIFICATION DIVISION.
       PROGRAM-ID. nei134.
       AUTHOR. SID WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INSIN ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILE-OUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT CHARCUR ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS IS SEQUENTIAL RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES.
           SELECT PAYCUR ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PAYCUR-KEY.
           SELECT GAPFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS GAPKEY
           ALTERNATE RECORD KEY IS GAP-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-STATE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT GARFILE ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS G-GARNO
           LOCK MODE MANUAL.
           SELECT INSFILE ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT CAREFILE ASSIGN TO "S65" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS CARE-KEY
           LOCK MODE MANUAL.
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
           02 CR-PAYDATE. 
              03 CR-CC PIC 99.
              03 CR-YY PIC 99.
              03 CR-MM PIC 99.
              03 CR-DD PIC 99.
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
       FD  INSFILE
     *     BLOCK CONTAINS 6 RECORDS
           DATA RECORD IS INSFILE01.
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

       FD GARFILE
           BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS G-MASTER.
       01 G-MASTER.
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
           02 G-PR-GROUP.
             03 G-PR-GROUP-0 PIC XX.
             03 G-PR-GROUP-1 PIC X(5).
             03 G-PR-GROUP-2 PIC X(5).
           02 G-PRIPOL0.
             03 G-PRIPOL PIC X(9).
             03 G-PR-SUFX PIC XXX.
             03 G-PR-FILLER PIC XX.
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-SE-MPLR PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-SE-OFFICE PIC X(4).
           02 G-SE-GROUP PIC X(12).
           02 G-SECPOL0.
             03 G-SECPOL PIC X(9).
             03 G-SE-SUFX PIC XXX.
             03 G-SE-FILLER PIC XX.
           02 G-SENAME PIC X(24).
           02 G-SE-RELATE PIC X.
           02 G-INSPEND PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.
       
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
           02 FO-PAPER PIC X.

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
           02 CC-PROC.
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
       WORKING-STORAGE SECTION.
       01  X USAGE IS INDEX.
      * 01  X PIC 9999.
       01  INSTAB01.
           02 INSTAB PIC 999 OCCURS 999 TIMES.
       01  ALF3 PIC XXX.
       01  TODAYDATE.
           02 T-CC PIC 99.
           02 T-YY PIC 99.
           02 T-MM PIC 99.
           02 T-DD PIC 99.
       01  diff pic 99.    
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT PAYCUR INSIN CHARCUR GARFILE
           GAPFILE INSFILE CAREFILE
           OUTPUT FILE-OUT.

           accept todaydate from DATE YYYYMMDD

           PERFORM A1 VARYING X FROM 1 BY 1 UNTIL X = 999.

       P00. 
           READ INSIN AT END GO TO P1.
           IF INS-2 = " "
             MOVE 1 TO INSTAB(INS-1)
           ELSE 
             MOVE 2 TO INSTAB(INS-1). 

           GO TO P00.

       P1. 
           READ CHARCUR AT END GO TO P6.

           IF CC-DATE-T < "20200101" GO TO P1.
           
           IF CC-REC-STAT > "1" GO TO P1.
           
           IF CC-PAYCODE = 003 OR 004 OR 028 OR 064 GO TO P1.
                                 
           IF CC-PAPER = "O" OR "P" GO TO P1-1.
                      
           IF INSTAB(CC-PAYCODE) = 2 GO TO P1.
           
           IF CC-AMOUNT = 0 GO TO P1.
                      
           IF CC-ASSIGN NOT = "A" GO TO P1.
           
           IF CC-PAYCODE = 62 GO TO P1-3.
           
           MOVE CC-KEY8 TO G-GARNO
           READ GARFILE INVALID GO TO P1-1.
           
           MOVE CC-PAYCODE TO ALF3
           
           IF ALF3 = G-PRINS GO TO P1.
           
           MOVE CC-PAYCODE TO INS-KEY
           
           READ INSFILE INVALID GO TO P1-1.
           
           IF G-PRINS NOT = "003" GO TO P1-1.
      *     IF INS-CAID = "EE " GO TO P1.
      *     IF CC-PAYCODE = 002 OR 005 OR 006 OR 074 GO TO P1.

       P1-1.
           IF ( CC-PAPER = " " )
              AND ( CC-PAYCODE NOT = 62 )
              MOVE "P" TO CC-PAPER.

           MOVE CHARCUR-KEY TO FILEOUT-KEY
           MOVE CC-PATID TO FO-PATID.
           MOVE CC-PAYCODE TO FO-PC
           MOVE CC-DATE-T TO FO-DATE
           MOVE CC-NEIC-ASSIGN TO FO-ASSIGN
           MOVE CC-PLACE TO FO-PLACE
           MOVE CC-DOCP TO FO-DOCP
           MOVE CC-PAPER TO FO-PAPER
           WRITE FILE-OUT01
           GO TO P1.

       GAP-1.
           MOVE CC-KEY8 TO G-GARNO
           READ GARFILE INVALID GO TO P1-1.
           MOVE G-PR-GROUP TO GAPKEY
           READ GAPFILE INVALID GO TO P1-1.
           IF GAP-TYPE = "X" OR "Y" GO TO P1.
           IF CC-ASSIGN = "A" GO TO P1-1.
           GO TO P1.

       P1-2.
           PERFORM S4 THRU S4-EXIT.
           IF CC-AMOUNT NOT > 0 GO TO P1.
           GO TO P1-1.

       P1-3.
           MOVE CC-KEY8 TO CR-KEY8
           MOVE CC-PROC1 TO CR-PROC
           MOVE CC-DATE-T TO CR-DATE
           MOVE SPACE TO CR-MOD1 CR-MOD2
           START CAREFILE KEY NOT < CARE-KEY 
             INVALID 
               GO TO P1-1.

       P1-4.
           READ CAREFILE NEXT AT END GO TO P1-1.
           
           IF CR-KEY8 NOT = CC-KEY8 GO TO P1-1.
           
           IF CR-DATE NOT = CC-DATE-T GO TO P1-1.
           
           IF CR-PROC NOT = CC-PROC GO TO P1-4.
           
      *    can we add a 60 day dos check here?
           IF CR-INSNAME NOT = SPACE
             compute diff = 12 * (t-yy - cr-yy) + t-mm - cr-mm
             if diff > 2
               go to P1
             end-if
           end-if                       
           
           GO TO P1-1.

       A1. 
           MOVE 0 TO INSTAB(X).

       S4. 
           MOVE CC-KEY8 TO PC-KEY8.
           MOVE "000" TO PC-KEY3.
           START PAYCUR KEY > PAYCUR-KEY
             INVALID 
               GO TO S4-EXIT.

       S7. 
           READ PAYCUR NEXT AT END GO TO S4-EXIT.
           
           IF PC-KEY8 NOT = CC-KEY8 GO TO S4-EXIT.
           
           IF PC-CLAIM NOT = CC-CLAIM GO TO S7.
           
           ADD PC-AMOUNT TO CC-AMOUNT GO TO S7.

       S4-EXIT. 
           EXIT.

       P6. 
           CLOSE FILE-OUT. 
           STOP RUN.
