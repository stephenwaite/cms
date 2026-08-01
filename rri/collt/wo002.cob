      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <stephen.waite@cmsvt.com>
      * @author  Claude (Anthropic) <https://claude.ai>
      * @copyright Copyright (c) 2026 cms 
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. wo002.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
             LOCK MODE MANUAL.
           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
             ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
             LOCK MODE MANUAL.
           SELECT PAYCUR ASSIGN TO "S40" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
             LOCK MODE MANUAL.
           SELECT PAYFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS PAYFILE-KEY
             LOCK MODE MANUAL.
           SELECT OWESFILE ASSIGN TO "S50"
             ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OWEDFILE ASSIGN TO "S55"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  GARFILE.
           COPY GARFILE.CPY.
       FD  CHARCUR.
           COPY CHARCUR.CPY.
       FD  PAYCUR.
           COPY PAYCUR.CPY.
       FD  PAYFILE.
           COPY PAYFILE.CPY.
       FD  OWESFILE.
       01  OWES-REC PIC X(160).
       FD  OWEDFILE.
       01  OWED-REC PIC X(160).
       WORKING-STORAGE SECTION.
       77  TOTALPAY                   PIC S9(7)V99 COMP-3 VALUE ZERO.
       77  WO-AMT                     PIC S9(7)V99 COMP-3.
       77  TODAY                      PIC X(8).
       77  FROM-DATE                  PIC X(8).
       77  TO-DATE                    PIC X(8).
       77  CNT-OWES                   PIC 9(6) VALUE ZERO.
       77  CNT-OWED                   PIC 9(6) VALUE ZERO.
       77  CNT-ZERO                   PIC 9(6) VALUE ZERO.
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "From date (YYYYMMDD): " WITH NO ADVANCING.
           ACCEPT FROM-DATE.
           DISPLAY "To date   (YYYYMMDD): " WITH NO ADVANCING.
           ACCEPT TO-DATE.
           IF FROM-DATE = SPACES OR TO-DATE = SPACES
              DISPLAY "Both dates required."
              STOP RUN.
           IF FROM-DATE > TO-DATE
              DISPLAY "From date must be <= to date."
              STOP RUN.
           OPEN INPUT  CHARCUR PAYCUR GARFILE
           OPEN I-O    PAYFILE
           OPEN OUTPUT OWESFILE OWEDFILE
           ACCEPT TODAY FROM DATE YYYYMMDD.
           MOVE LOW-VALUES TO CHARCUR-KEY.
           START CHARCUR KEY >= CHARCUR-KEY
                INVALID KEY GO TO P-DONE
           END-START.
       P00.
           READ CHARCUR NEXT AT END GO TO P-DONE END-READ.
           IF CC-DATE-T < FROM-DATE GO TO P00.
           IF CC-DATE-T > TO-DATE   GO TO P00.
           MOVE CC-KEY8 TO G-GARNO.
           READ GARFILE INVALID KEY
                DISPLAY "NO GAR: " CC-KEY8
                MOVE SPACES TO G-GARNAME
           END-READ.
           MOVE ZERO       TO TOTALPAY
           MOVE CC-KEY8    TO PC-KEY8
           MOVE LOW-VALUES TO PC-KEY3.
           START PAYCUR KEY >= PAYCUR-KEY
                INVALID KEY GO TO P-EVAL
           END-START.
       P1.
           READ PAYCUR NEXT AT END GO TO P-EVAL END-READ.
           IF PC-KEY8  NOT = CC-KEY8  GO TO P-EVAL.
           IF PC-CLAIM NOT = CC-CLAIM GO TO P1.
           ADD PC-AMOUNT TO TOTALPAY.
           GO TO P1.
       P-EVAL.
           COMPUTE WO-AMT = CC-AMOUNT + TOTALPAY.
           IF WO-AMT = 0
              ADD 1 TO CNT-ZERO
              GO TO P00.
           IF WO-AMT > 0
              WRITE OWES-REC FROM CHARCUR01
              ADD 1 TO CNT-OWES
              DISPLAY "OWES " CC-KEY8 " " CC-KEY3
                      " CLM=" CC-CLAIM " BAL=" WO-AMT
              GO TO P00.
           WRITE OWED-REC FROM CHARCUR01
           ADD 1 TO CNT-OWED
           DISPLAY "OWED " CC-KEY8 " " CC-KEY3
                   " CLM=" CC-CLAIM " BAL=" WO-AMT.   
           GO TO P00.
       P-DONE.
           DISPLAY "OWES (debit balances):  " CNT-OWES.
           DISPLAY "OWED (credit balances): " CNT-OWED.
           DISPLAY "ZERO (paid in full):    " CNT-ZERO.
           CLOSE CHARCUR PAYCUR GARFILE PAYFILE OWESFILE OWEDFILE.
           STOP RUN.