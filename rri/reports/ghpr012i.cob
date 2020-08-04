      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ghpr012i.
       AUTHOR. S WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PAYCUR ASSIGN TO      "S30" ORGANIZATION INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
               LOCK MODE MANUAL.
          
           SELECT INSFILE ASSIGN TO     "S35" ORGANIZATION INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS INS-KEY
               ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES.
           
           SELECT PARMNAME ASSIGN TO    "S40"
               ORGANIZATION LINE SEQUENTIAL.

           SELECT AGEDATE ASSIGN TO     "S45"
               ORGANIZATION LINE SEQUENTIAL.

           SELECT CHARCUR ASSIGN TO     "S50" ORGANIZATION INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
               ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES.

           SELECT FILEOUT ASSIGN TO     "S55"
               ORGANIZATION LINE SEQUENTIAL.

           SELECT GARFILE ASSIGN TO     "S60" ORGANIZATION INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
               ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
               LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PAYCUR
      *     BLOCK CONTAINS 6 RECORDS
           DATA RECORD IS PAYCUR01.
       01  PAYCUR01.
           02 PAYCUR-KEY.
             03 PC-KEY8 PIC X(8).
             03 PC-KEY3 PIC XXX.
           02 PC-AMOUNT PIC S9(4)V99.
           02 PC-PAYCODE PIC 999.
           02 PC-DENIAL PIC XX.
           02 PC-CLAIM PIC X(6).
           02 PC-DATE-T PIC X(8).
           02 PC-DATE-E PIC X(8).
           02 PC-BATCH PIC X(6).

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
           02 G-PRINS PIC 999.
           02 G-PR-ASSIGN PIC X.
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
           02 G-PR-GROUP PIC X(10).
           02 G-PRIPOL0.
             03 G-PRIPOL PIC X(9).
             03 G-PR-SUFX PIC XXX.
             03 G-PR-FILLER PIC X(4).
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-SE-MPLR PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-SE-OFFICE PIC X(4).
           02 G-SE-GROUP PIC X(10).
           02 G-SECPOL0. 
              03 G-SECPOL PIC X(9).
              03 G-SE-FILLER PIC X(7).
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


       FD  CHARCUR
      *     BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS CHARCUR01.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC1 PIC X(4). 
           02 CC-PROC2 PIC X(7).
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
           02 CC-DX7 PIC X(7).
           02 CC-FUTURE PIC X(6).

       FD  INSFILE
     *     BLOCK CONTAINS 6 RECORDS
           DATA RECORD IS INSFILE01.
       01  INSFILE01.
           02 INS-KEY PIC 999.
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
       
       FD  PARMNAME
           DATA RECORD IS PARMNAME01.
       01   PARMNAME01 PIC X(40).
       
       FD  AGEDATE
           DATA RECORD IS AGEDATE01.
       01  AGEDATE01.
           02 DATE-LOW PIC X(8).
           02 DATE-HIGH PIC X(8).

       FD FILEOUT
           DATA RECORD IS FILEOUT01.
       01  FILEOUT01 PIC X(133).
       
       WORKING-STORAGE SECTION.
       01 MON-TAB-RE01.
          02 FILLER PIC X(27) VALUE "JANUARY  FEBRUARY MARCH    ".
          02 FILLER PIC X(27) VALUE "APRIL    MAY      JUNE     ".
          02 FILLER PIC X(27) VALUE "JULY     AUGUST   SEPTEMBER".
          02 FILER PIC X(27) VALUE "OCTOBER  NOVEMBER DECEMBER ".
       01 MON-TAB01 REDEFINES MON-TAB-RE01.
           02 MON-TAB PIC X(9) OCCURS 12 TIMES.
       01  DOT01.
           02 DOT02 PIC X(50).
           02 DOT03 PIC X(82).
       01 LINE-1.
           02 F11 PIC X(20) VALUE "CHARGE ANALYSIS FOR ".
           02 L1F1 PIC X(28).
           02 F12 PIC XXX VALUE SPACE.
           02 L1LOW PIC X(10).
           02 F13 PIC X(6) VALUE " THRU ".
           02 L1HIGH PIC X(10).
           02 F14 PIC XXX VALUE SPACE.
           02 F15 PIC X(5) VALUE "PAGE ".
           02 L1F4 PIC ZZZ9.
       01 LINE-2.
           02 F21 PIC X(4) VALUE SPACE.
           02 L2F1 PIC X(9) VALUE "PAYER".
           02 F22 PIC X(16) VALUE SPACE.
           02 L2F3 PIC X(14) VALUE "CHARGE AMOUNT".
           02 F24 PIC X(6) VALUE SPACE.
           02 L2F4 PIC X(14) VALUE "PAYMENT AMOUNT".

       01 LINE-3.
           02 L3F2 PIC X(30).
           02 F23 PIC X(4) VALUE SPACE.
           02 L3F3 PIC ZZ,ZZZ,ZZ9.99.
           02 F24 PIC XXX VALUE SPACE.
           02 L3F4 PIC ZZ,ZZZ,ZZ9.99.
       01  LINE-4.
           02 F41 PIC X(30) VALUE "TOTALS                        ".
           02 L4F1 PIC X(4) VALUE SPACES.
           02 L4F2 PIC ZZ,ZZZ,ZZ9.99.
           02 F43 PIC XXX VALUE SPACE.
           02 L4F3 PIC ZZ,ZZZ,ZZ9.99.

       01 CNT01.
           02 CNT PIC 99999 OCCURS 5 TIMES.

       01 TOT01.
           02 TOT OCCURS 5 TIMES.
             03 TOT-CHG PIC 9(8)V99.
             03 TOT-PAY PIC S9(8)V99.
       
       01  TOT-CNT PIC 999999 VALUE 0.
       01  TOT-AMT PIC S9(8)V99 VALUE 0.
       01  PAY-CNT PIC 999999 VALUE 0.
       01  PAY-TOT PIC S9(8)V99 VALUE 0.
       01  LINE-X PIC 99 VALUE 0.
       01  PAGE-X PIC 9999.
       01  X PIC 99.
       01  Y PIC 99.
       01  Z PIC 9999.
       01  TOTALPAY PIC S9(8)V99 VALUE 0.
       01  NUM-3 PIC 999.
       01  TEST-DATE.
           05  T-CC            PIC XX.
           05  T-YY            PIC XX.
           05  T-MM            PIC XX.
           05  T-DD            PIC XX.
       01  DISPLAY-DATE.
           02 T-MM PIC XX.
           02 FILLER PIC X VALUE "/".
           02 T-DD PIC XX.
           02 FILLER PIC X VALUE "/".
           02 T-CC PIC XX.
           02 T-YY PIC XX.
       01  ROW-X PIC 9.  
       
       01  TITLE-VALUES.
           02 FIL1 PIC X(30) VALUE "MEDICAID                      ".
           02 FIL2 PIC X(30) VALUE "MEDICARE                      ".
           02 FIL3 PIC X(30) VALUE "COMMERCIAL INSURANCE          ".
           02 FIL4 PIC X(30) VALUE "TRICARE                       ".
           02 FIL5 PIC X(30) VALUE "SELF PAY, CO-INS AND CO-PAYS   ".
       
       01  TITLE-TAB01 REDEFINES TITLE-VALUES.
           02 TITLE-TAB PIC X(30) OCCURS 5 TIMES.     

       01  SAVE-CHG PIC 9(8)V99.
       01  SAVE-PAY PIC S9(8)V99.

       PROCEDURE DIVISION.
       P00.
           OPEN INPUT AGEDATE CHARCUR GARFILE PARMNAME INSFILE PAYCUR
           OPEN OUTPUT FILEOUT.
           PERFORM A2 VARYING Z FROM 1 BY 1 UNTIL Z > 5.
           READ PARMNAME.
           READ AGEDATE.
           MOVE DATE-LOW TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           MOVE DISPLAY-DATE TO L1LOW 
           MOVE DATE-HIGH TO TEST-DATE
           MOVE CORR TEST-DATE TO DISPLAY-DATE
           MOVE DISPLAY-DATE TO L1HIGH.
       P1. 
           READ CHARCUR NEXT AT END
               GO TO P2
           END-READ

           IF (CC-DATE-T < DATE-LOW OR > DATE-HIGH) 
               GO TO P1
           END-IF

           MOVE CC-KEY8 TO G-GARNO.
           READ GARFILE INVALID 
               DISPLAY CC-KEY8 
               GO TO P1
           END-READ

      *  THE DEFINITION OF EACH ROW ACCORDING TO THE 
      *  LAYOUT OF THE REQUIRED REPORT     

           IF G-PRINS = "004" OR "064"
               MOVE 1 TO ROW-X
               GO TO P1-0
           END-IF    
      
           IF G-PRINS = "003" OR "028"
               MOVE 2 TO ROW-X
               GO TO P1-0
           END-IF    
      
           IF G-PRINS = "141" 
               MOVE 4 TO ROW-X 
               GO TO P1-0
           END-IF    
      
           IF G-PRINS = "001" 
                MOVE 5 TO ROW-X
                GO TO P1-0
           END-IF                  
      
      *  THE OTHER PAYCODES FALL INTO CATEGORY 3         
            MOVE 3 TO ROW-X.
       P1-0.     
           COMPUTE TOT-CHG(ROW-X) = TOT-CHG(ROW-X) + CC-AMOUNT
           
           MOVE CC-KEY8 TO PC-KEY8
           MOVE SPACE TO PC-KEY3

           START PAYCUR KEY NOT < PAYCUR-KEY INVALID
               GO TO P1    
           END-START

           PERFORM S1 THRU S1-EXIT
           GO TO P1.

          
       P2. 
           MOVE PARMNAME01 TO L1F1.
           MOVE 1 TO L1F4 PAGE-X.
           PERFORM L1.
           PERFORM P5 THRU P5-EXIT VARYING Z FROM 1 BY 1 UNTIL Z > 5.
           MOVE DOT01 TO FILEOUT01.
           WRITE FILEOUT01.
           MOVE SAVE-CHG TO SAVE-PAY
           PERFORM VARYING Z FROM 1 BY 1 UNTIL Z > 5
             ADD TOT-CHG(Z) TO SAVE-CHG
             ADD TOT-PAY(Z) TO SAVE-PAY
           END-PERFORM 
           MOVE SPACE TO FILEOUT01
           WRITE FILEOUT01
           MOVE SAVE-CHG TO L4F2
           MOVE SAVE-PAY TO L4F3
           WRITE FILEOUT01 FROM LINE-4
           CLOSE AGEDATE CHARCUR GARFILE PARMNAME INSFILE PAYCUR 
                 FILEOUT
           STOP RUN.
       P5. 
      *     IF TOT-CHG(Z) = 0 AND 
      *         GO TO P5-EXIT
      *     END-IF
           MOVE TITLE-TAB(Z) TO L3F2.
           MOVE TOT-CHG(Z) TO L3F3.
           MOVE TOT-PAY(Z) TO L3F4.

           WRITE FILEOUT01 FROM LINE-3.
       P5-EXIT. 
           EXIT.
       L1.
           WRITE FILEOUT01 FROM LINE-1.
           WRITE FILEOUT01 FROM LINE-2 AFTER 2
           MOVE SPACE TO FILEOUT01 WRITE FILEOUT01.
       A2. 
           MOVE 0 TO TOT-CHG(Z) TOT-PAY(Z).
       S1.
           READ PAYCUR NEXT AT END
               GO TO S1-EXIT
           END-READ
           
           IF PC-KEY8 NOT = CC-KEY8
               GO TO S1-EXIT
           END-IF
           
           IF PC-DENIAL = "14" OR "DI" OR "15" 
               GO TO S1
           END-IF    

           IF (PC-PAYCODE > 6 AND < 10) OR (PC-PAYCODE > 10 AND < 18)
               GO TO S1
           END-IF 

           IF PC-CLAIM NOT = CC-CLAIM
               GO TO S1
           END-IF

           IF PC-PAYCODE = G-PRINS
               COMPUTE TOT-PAY(ROW-X) = TOT-PAY(ROW-X) + PC-AMOUNT
           ELSE
               COMPUTE TOT-PAY(5) = TOT-PAY(5) + PC-AMOUNT                 
           END-IF

           GO TO S1.
       S1-EXIT.
           EXIT.

