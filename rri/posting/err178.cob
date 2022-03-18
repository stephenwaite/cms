      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
        IDENTIFICATION DIVISION.
       PROGRAM-ID. err178.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILEIN ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.
       
           SELECT GARFILE ASSIGN TO "S35"     ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE MANUAL.
       
           SELECT CHARCUR ASSIGN TO "S40"     ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT ERROR-FILE ASSIGN TO "S45" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S50" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT PAYFILE ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC        RECORD KEY IS PAYFILE-KEY
           LOCK MODE MANUAL.
           SELECT PAYCUR ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           LOCK MODE MANUAL.
           SELECT INSFILE ASSIGN TO "S65" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
           LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
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
       FD ERROR-FILE.
       01 ERROR-FILE01 PIC X(132).
       FD FILEOUT.
       01 FILEOUT01 PIC X(160).
       FD FILEIN.
       01  FILEIN01.
           02 FI-NAME PIC X(20).
           02 FILLER PIC X VALUE SPACE.
           02 FI-POL PIC X(11).
           02 FILLER PIC X VALUE SPACE.
           02 FI-DATE PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 FI-PAYDATE PIC X(8).
           02 FILLER PIC X.
           02 FI-PROC.
             03 FI-PROC1 PIC X(5).
             03 FI-PROC2 PIC XX.
           02 FILLER PIC XXX VALUE SPACE.
           02 FI-GARNO PIC X(8).
           02 FILLER PIC X(14).
           02 FI-ADJ PIC X.
           02 FILLER PIC X.
           02 FI-DOLLAR-PAID PIC XXXX.
           02 FILLER PIC X.     
           02 FI-CENT-PAID PIC XX.
           02 FILLER PIC XX.
           02 FI-DOLLAR-REDUCE PIC XXXX.
           02 FILLER PIC X.     
           02 FI-CENT-REDUCE PIC XX.
           02 FILLER PIC X(39).
           02 FI-PAYORID PIC X(5).
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
              03 CC-PROC0 PIC XXXX.
              03 CC-PROC1 PIC X(5).
              03 CC-PROC2 PIC XX.
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
           02 CC-DX3 PIC X(7).
           02 CC-DX3 PIC X(7).
           02 CC-DX3 PIC X(7).
           02 CC-FUTURE PIC X(6).
       FD GARFILE
           BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS G-MASTER.
       01 G-MASTER.
           02 G-GARNO.
             03 ID1 PIC XXX.
             03 ID2 PIC XXX.
             03 ID3 PIC XX.
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
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
           02 G-PR-GROUP PIC X(12).
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
              03 G-SE-FILLER PIC X(5).
           02 G-SENAME PIC X(24).
           02 G-SE-RELATE PIC X.
           02 G-INSPEND PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.
           02 G-ACCT PIC X(8).
           02 G-PRGRPNAME PIC X(15).
           02 G-SEGRPNAME PIC X(15).

       WORKING-STORAGE SECTION.

       01  HL01.
           02 HL-1 PIC X(40) VALUE SPACE.
           02 FILLER PIC X(21) VALUE SPACE.
           02 HL-2 PIC X(27) VALUE "  NEIC     UNPOSTED LIST   ".
           02 FILLER PIC X(5) VALUE SPACE.
           02 HL-3 PIC X(10).
       
       01  TOT-AMT PIC S9(4)V99.
       01  CLAIM-TOT PIC S9(5)V99.
       01  FLAG PIC 9 VALUE 0.
       01  TOT-PAY PIC S9(5)V99 VALUE 0.
       01  X PIC 99.
       01  Y PIC 99.
       01  Z PIC 99.
       01  A PIC 99.
       01  HOLDKEY PIC X(11).
       01  TEST-DATE.
           05  T-CC            PIC 99.
           05  T-YY            PIC 99.
           05  T-MM            PIC 99.
           05  T-DD            PIC 99.
       01  INPUT-DATE.
           02 T-MM PIC XX.
           02 T-DD PIC XX.
           02 T-CC PIC XX.
           02 T-YY PIC XX.
       01  ALF6.
           02 ALF4 PIC X(4).
           02 ALF2 PIC XX.
       01  PAYDATE PIC X(8).
       01  XYZ PIC 999.
       01  NUM6 PIC 9(6).
       01  PAYBACK01 PIC X(80).
       01  ALF1 PIC X.
       01  payx PIC S9(4)V99.

       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT FILEIN CHARCUR GARFILE PAYCUR INSFILE.
           OPEN OUTPUT ERROR-FILE FILEOUT.
           OPEN I-O PAYFILE.
       P1.
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P9.
           IF FI-DATE = SPACE  OR FI-PROC = SPACE GO TO E1.
           
           MOVE FI-GARNO TO G-GARNO
           READ GARFILE 
             INVALID 
               GO TO E1.

           MOVE FI-PAYDATE TO PAYDATE
           MOVE FI-DATE TO INPUT-DATE
           MOVE CORR INPUT-DATE TO TEST-DATE
           MOVE G-GARNO TO CC-KEY8
           MOVE SPACE TO CC-KEY3
           START CHARCUR KEY NOT < CHARCUR-KEY INVALID GO TO E1.
           
       P2. 
           READ CHARCUR NEXT AT END GO TO E1.
           
           IF CC-KEY8 NOT = G-GARNO GO TO E1.
           
           IF NOT (CC-DATE-T = TEST-DATE AND CC-PROC1 = FI-PROC1)
            GO TO P2.
                      
           DISPLAY "INCOMING TEST-DATE " TEST-DATE " PROC1 " FI-PROC1
           DISPLAY "INCOMING CC DATE " CC-DATE-T " CC-PROC1 " CC-PROC1

           ACCEPT OMITTED

           WRITE FILEOUT01 FROM CHARCUR01
           
           INSPECT FI-DOLLAR-PAID REPLACING ALL " " BY "0"
           MOVE FI-DOLLAR-PAID TO ALF4
           MOVE FI-CENT-PAID TO ALF2
           MOVE ALF6 TO NUM6
           COMPUTE PD-AMOUNT =  -1 * (NUM6 / 100)
           MOVE 0 TO PAYX
           INSPECT FI-DOLLAR-REDUCE REPLACING ALL " " BY "0"
           MOVE FI-DOLLAR-REDUCE TO ALF4
           MOVE FI-CENT-REDUCE TO ALF2
           MOVE ALF6 TO NUM6
           COMPUTE Payx =  -1 * (NUM6 / 100)
            
           if (payx = 0) and (pd-amount = 0) AND
             ((FILEIN01(119:1) = "2" OR FILEIN01(119:1) = "4") AND
             ((FILEIN01(123:1) = "1") OR FILEIN01(127:1) = "1") OR 
              FILEIN01(123:2) = "23") 
             GO TO P2-2
           END-IF
           
           if (payx = 0) and (pd-amount = 0)
              GO TO E1
           end-if.

       P2-2.      
           MOVE SPACE TO PD-DENIAL
           
           IF FI-ADJ = "-" GO TO E1.           
           
           MOVE PAYDATE TO PD-DATE-T
           MOVE PAYDATE TO PD-DATE-E
           ACCEPT PD-ORDER FROM TIME
           MOVE SPACE TO PD-BATCH
           MOVE CC-CLAIM TO PD-CLAIM
           MOVE G-GARNO TO PD-KEY8
           MOVE SPACE TO PD-KEY3
           MOVE G-GARNAME TO PD-NAME
           
           IF CC-PAYCODE = "062"
              MOVE "062" TO PD-PAYCODE
              GO TO P7-NEXT
           END-IF   
              
           IF (CC-PAYCODE = G-PRINS)
            AND (CC-PAYCODE NOT = "001")
             MOVE CC-PAYCODE TO PD-PAYCODE
             GO TO P7-NEXT
           END-IF  

           IF CC-PAYCODE = G-SEINS 
            AND (CC-PAYCODE NOT = "001")
             MOVE CC-PAYCODE TO PD-PAYCODE
             GO TO P7-NEXT
           END-IF
           
           MOVE "076" TO PD-PAYCODE
           
           MOVE FI-PAYORID TO INS-NEIC
           START INSFILE KEY NOT < INS-NEIC INVALID 
           GO TO P4-NEXT.

       P3-NEXT.
           READ INSFILE NEXT AT END
             GO TO P4-NEXT
           END-READ  
           IF INS-NEIC = FI-PAYORID 
             MOVE INS-KEY TO PD-PAYCODE
             GO TO P7-NEXT
           END-IF
           GO TO P3-NEXT.           
       P4-NEXT.
           MOVE G-PRINS TO INS-KEY
           READ INSFILE INVALID GO TO P5-NEXT.
           IF INS-NEIC  = FI-PAYORID  
           MOVE G-PRINS TO PD-PAYCODE
           GO TO P7-NEXT.
       P5-NEXT.
           MOVE G-SEINS TO INS-KEY
           READ INSFILE INVALID GO TO P6-NEXT.
           IF INS-NEIC  = FI-PAYORID 
           MOVE G-SEINS TO PD-PAYCODE
           GO TO P7-NEXT.
       
       P6-NEXT.
           MOVE G-TRINS TO INS-KEY
           READ INSFILE INVALID GO TO P7-NEXT.
           IF INS-NEIC  = FI-PAYORID 
           MOVE G-TRINS TO PD-PAYCODE.

       P7-NEXT.  
           IF PD-PAYCODE = "001"
              MOVE "076" TO PD-PAYCODE
           END-IF  

           MOVE PAYFILE01 TO PAYBACK01
           MOVE 0 TO FLAG
           PERFORM A1 THRU A4
           
           IF FLAG = 1 GO TO E1.
           MOVE PAYBACK01 TO PAYFILE01
           MOVE 0 TO XYZ.
           PERFORM P3 THRU P4.
           WRITE FILEOUT01 FROM CHARCUR01.
           IF Payx = 0 GO TO P1.
           move payx to pd-amount.
       P2-1.
           COMPUTE CLAIM-TOT = CC-AMOUNT + PD-AMOUNT
           IF CLAIM-TOT = 0 GO TO P1.           
           MOVE "14" TO PD-DENIAL
           MOVE PAYFILE01 TO PAYBACK01
           PERFORM P3 THRU P4
           GO TO P1.
       P3.
           ADD 1 TO XYZ
           MOVE XYZ TO PD-KEY3
           READ PAYFILE INVALID KEY GO TO P4.
           GO TO P3.
       P4.
           MOVE PAYBACK01 TO PAYFILE01
           MOVE XYZ TO PD-KEY3
           WRITE PAYFILE01
           DISPLAY PAYFILE-KEY " " PD-NAME.
           DISPLAY "RECORD IS ADDED".
       S4. 
           MOVE CC-KEY8 TO PC-KEY8 
           MOVE "000" TO PC-KEY3.
           START PAYCUR KEY NOT <  PAYCUR-KEY INVALID GO TO S5.
           COMPUTE CLAIM-TOT = CC-AMOUNT + PD-AMOUNT.
       S41.
           READ PAYCUR NEXT AT END GO TO S5.
           IF PC-KEY8 NOT = CC-KEY8 GO TO S5.
           IF PC-CLAIM NOT = CC-CLAIM GO TO S41. 
      *     IF PC-DENIAL = "CP" 
      *     MOVE 1 TO FLAG.
      *    IF PC-PAYCODE = "001" OR "021" OR "022" 
      *     MOVE 1 TO FLAG.
           ADD PC-AMOUNT TO CLAIM-TOT.
           GO TO S41.
       S5. EXIT.
       A1.
           MOVE CC-KEY8 TO PD-KEY8
           MOVE "000" TO PD-KEY3.
           START PAYFILE KEY NOT <  PAYFILE-KEY
             INVALID 
             GO TO A4.
       A2.
           READ PAYFILE NEXT AT END GO TO A4.
           
           IF PD-KEY8 NOT = CC-KEY8 GO TO A4.
           IF PD-CLAIM NOT = CC-CLAIM GO TO A2.
           MOVE 1 TO FLAG.
       A4.
           EXIT.
       E1.           
           MOVE SPACE TO ERROR-FILE01
           WRITE ERROR-FILE01 FROM FILEIN01
           GO TO P1.
       P9.
           CLOSE CHARCUR GARFILE ERROR-FILE FILEOUT PAYFILE PAYCUR
                 INSFILE
           STOP RUN.
