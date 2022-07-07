      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SID014.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AGEDATE ASSIGN TO "S25"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT PAYCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.
           SELECT PROCFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PROC-KEY
           LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO "S45"
           ORGANIZATION LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  PAYCUR
           BLOCK CONTAINS 3 RECORDS
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
       FD  AGEDATE.
       01  AGEDATE01. 
           02 LOW-DATE PIC X(8).
           02 HIGH-DATE PIC X(8).
       FD  CHARCUR.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID.
             03 CC-PATID1 PIC X(7).
             03 CC-PATID8 PIC X.
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC.
              03 CC-PROC0 PIC X(4).
              03 CC-PROC1 PIC X(7).
           02 CC-MOD2 PIC XX.
           02 CC-MOD3 PIC XX.
           02 CC-MOD4 PIC XX.
           02 CC-AMOUNT PIC S9(4)V99.
           02 CC-DOCR PIC X(3).
           02 CC-DOCP PIC X(2).
           02 CC-PAYCODE PIC XXX.
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
       FD FILEOUT
           DATA RECORD IS FILEOUT01.
       01  FILEOUT01.
           02 FO-CPT  PIC X(7).
           02 FILLER PIC X VALUE SPACE.
           02 FO-NAME PIC X(28).
           02 FILLER PIC X VALUE SPACE.
           02 FO-AMT PIC Z,ZZZ,ZZ9.99.
           02 FILLER PIC X VALUE SPACE.
           02 FO-AMT-CHG PIC Z,ZZZ,ZZ9.99.
           02 FILLER PIC X VALUE SPACE.
           02 FO-CNTR-CHG PIC 9(6).
           02 FILLER PIC X VALUE SPACE.
           02 FO-AVG-PAY PIC Z,ZZ9.99.

       FD  PROCFILE
           DATA RECORD PROCFILE01.
       01  PROCFILE01.
           02 PROC-KEY.
             03 PROC-KEY1 PIC X(4).
             03 PROC-KEY2 PIC X(7).
           02 PROC-TYPE PIC X.
           02 PROC-TITLE PIC X(28).
           02 PROC-AMOUNT PIC 9(4)V99.
       WORKING-STORAGE SECTION.
       01  LOW-CLAIM PIC X(6).
       01  HIGH-CLAIM PIC X(6).
       01  X PIC 9999.
       01  CNTR PIC 9999 VALUE 0.
       01  FLAG PIC 9.
       01  TAB01.
           02 TAB02 OCCURS 1500 TIMES.
             03 TAB-CPT PIC X(7).
             03 TAB-NAME PIC X(28).
             03 TAB-AMT PIC S9(7)V99.
             03 TAB-CNTR-CHG PIC 9(6).
             03 TAB-AMT-CHG PIC S9(7)V99.
             03 TAB-CLAIM-NO PIC X(6).
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT AGEDATE CHARCUR PAYCUR PROCFILE 
                OUTPUT FILEOUT.
            READ AGEDATE AT END CONTINUE.
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 1500 
            MOVE SPACE TO TAB-CPT(X) TAB-NAME(X)
            MOVE 0 TO TAB-AMT(X) TAB-CNTR-CHG(X) TAB-AMT-CHG(X)
           END-PERFORM.
           
       P1. READ PAYCUR NEXT AT END GO TO P2.
           IF (PC-PAYCODE = "007" OR "008" OR "009" OR "011" 
           OR "012" OR "013" OR "014" OR "015" OR "016" OR "017" ) 
           OR (PC-DENIAL = "DI" OR "15" OR "14")
           GO TO P1.
           IF PC-AMOUNT = 0 GO TO P1.
           IF PC-DATE-T < LOW-DATE GO TO P1.
      *     display paycur01
           PERFORM Z1 THRU Z1-EXIT
           GO TO P1.
       Z1. MOVE PC-KEY8 TO CC-KEY8
           MOVE SPACE TO CC-KEY3
           MOVE 0 TO FLAG.
           START CHARCUR KEY NOT < CHARCUR-KEY INVALID GO TO Z1-EXIT.
       Z2. READ CHARCUR NEXT AT END GO TO Z1-EXIT.
           IF CC-KEY8 NOT = PC-KEY8 GO TO Z1-EXIT.
           IF CC-CLAIM NOT = PC-CLAIM GO TO Z2.
           IF CC-DATE-T < LOW-DATE GO TO Z2.
           IF CC-DATE-T > HIGH-DATE GO TO Z2.
      *     DISPLAY CC-DATE-T 

           PERFORM VARYING X FROM 1 BY 1 UNTIL X > CNTR
            IF TAB-CPT(X) = CC-PROC1
             ADD PC-AMOUNT TO TAB-AMT(X)
             IF CC-CLAIM NOT = TAB-CLAIM-NO(X)  
               ADD 1 TO TAB-CNTR-CHG(X)
               ADD CC-AMOUNT TO TAB-AMT-CHG(X)
             END-IF
             MOVE CC-CLAIM TO TAB-CLAIM-NO(X)
             MOVE 1 TO FLAG
             MOVE CNTR TO X
      *       display cc-proc1 " cc-proc1 first entry"
            END-IF
           END-PERFORM.

           IF FLAG = 1 GO TO Z1-EXIT.

           ADD 1 TO CNTR
           MOVE CC-PROC TO PROC-KEY
           READ PROCFILE INVALID MOVE SPACE TO PROC-TITLE
           END-READ.
           MOVE CC-PROC1 TO TAB-CPT(CNTR)
           MOVE PROC-TITLE TO TAB-NAME(CNTR)
           ADD PC-AMOUNT TO TAB-AMT(CNTR)
           MOVE CC-CLAIM TO TAB-CLAIM-NO(CNTR)
           ADD 1 TO TAB-CNTR-CHG(CNTR)
           ADD CC-AMOUNT TO TAB-AMT-CHG(CNTR).
      *     display cc-proc1 " cc-proc1 another entry"
      *     accept omitted.

       Z1-EXIT. EXIT.
       P2.
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > CNTR
            MOVE TAB-CPT(X) TO FO-CPT
            MOVE TAB-AMT(X) TO FO-AMT
            MOVE TAB-NAME(X) TO FO-NAME
            MOVE TAB-AMT-CHG(X) TO FO-AMT-CHG
            MOVE TAB-CNTR-CHG(X) TO FO-CNTR-CHG
            COMPUTE FO-AVG-PAY = TAB-AMT(X) / TAB-CNTR-CHG(X)
            WRITE FILEOUT01
           END-PERFORM.
           CLOSE FILEOUT CHARCUR PAYCUR.
           STOP RUN.
