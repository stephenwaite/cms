      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrr334.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYFILE-KEY.
           SELECT PAYCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY.
           SELECT GARFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES.
           SELECT PATFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS P-PATNO
           ALTERNATE RECORD KEY IS P-GARNO WITH DUPLICATES.
           SELECT CHARCUR ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES.
           SELECT CHARFILE ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS CHARFILE-KEY.
           SELECT KEEPBACK ASSIGN TO "S60"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT CMNTFILE ASSIGN TO "S65" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS CMNT-KEY.
           SELECT HISFILE ASSIGN TO "S70" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS HISFILE-KEY.
           SELECT AUTHFILE ASSIGN TO "S75" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS AUTH-KEY.
           SELECT MPLRFILE ASSIGN TO "S80" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS MPLR-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD  KEEPBACK.
       01  KEEPBACK01.
           02 KEEPTYPE PIC XX.
           02 KEEP11.
             03 KEEP8 PIC X(8).
             03 KEEP3 PIC XXX.
             03 KEEPHIS PIC X(8).
       FD  GARFILE
      *    BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS GARFILE01.
       01  GARFILE01.
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
           02 G-PR-GROUP PIC X(12).
           02 G-PRIPOL PIC X(14).
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-SE-MPLR PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
           02 G-SE-GROUP PIC X(12).
           02 G-SECPOL PIC X(14).
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
      *    BLOCK CONTAINS 3 RECORDS
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
           02 CC-PAYCODE PIC XXX.
           02 CC-STUD PIC X.
           02 CC-WORK PIC XX.
           02 CC-DAT1 PIC X(8).
           02 CC-RESULT PIC X.
           02 CC-ACT PIC X.
           02 CC-SORCREF PIC X.
           02 CC-COLLT PIC X.
           02 CC-AUTH PIC X.
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
       FD  CHARFILE
      *    BLOCK CONTAINS 2 RECORDS
           DATA RECORD IS CHARFILE01.
       01  CHARFILE01.
           02 CHARFILE-KEY.
             03 CD-KEY8 PIC X(8).
             03 CD-KEY3 PIC XXX.
           02 CD-PATID PIC X(8).
           02 CD-CLAIM PIC X(6).
           02 CD-SERVICE PIC X.
           02 CD-DIAG PIC X(7).
           02 CD-PROC PIC X(11).
           02 CD-MOD2 PIC XX.
           02 CD-MOD3 PIC XX.
           02 CD-MOD4 PIC XX.
           02 CD-AMOUNT PIC S9(4)V99.
           02 CD-DOCR PIC X(3).
           02 CD-DOCP PIC X(2).
           02 CD-PAYCODE PIC XXX.
           02 CD-STAT PIC X.
           02 CD-WORK PIC XX.
           02 CD-DAT1 PIC X(8).
           02 CD-RESULT PIC X.
           02 CD-ACT PIC X.
           02 CD-SORCREF PIC X.
           02 CD-COLLT PIC X.
           02 CD-AUTH PIC X.
           02 CD-PAPER PIC X.
           02 CD-PLACE PIC X.
           02 CD-NAME PIC X(24).
           02 CD-ESPDT PIC X.
           02 CD-DATE-T PIC X(8).
           02 CD-DATE-E PIC X(8).
           02 CD-ORDER PIC X(6).
           02 CD-DX2 PIC X(7).
           02 CD-DX3 PIC X(7).
           02 CD-DATE-A PIC X(8).
           02 CD-ACC-TYPE PIC X.
           02 CD-DATE-M PIC X(8).
           02 CD-ASSIGN PIC X.
           02 CD-NEIC-ASSIGN PIC X.
           02 CD-DX4 PIC X(7).
           02 CD-DX5 PIC X(7).
           02 CD-DX6 PIC X(7).
           02 CD-FUTURE PIC X(6).

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


       FD  PAYCUR
      *    BLOCK CONTAINS 3 RECORDS
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

       FD PATFILE
      *    BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS PATFILE01.
       01 PATFILE01.
           02 P-PATNO PIC X(8).
           02 P-GARNO PIC X(8).
           02 P-PATNAME PIC X(24).
           02 P-SEX PIC X.
           02 P-RELATE PIC X.
           02 P-MSTAT PIC X.
           02 P-DOB PIC X(8).

       FD  MPLRFILE.
       01  MPLRFILE01.
           02 MPLR-KEY PIC X(8). 
           02 MPLR-NAME PIC X(22).
           02 MPLR-STREET PIC X(24).
           02 MPLR-CITY PIC X(15).
           02 MPLR-STATE PIC XX.
           02 MPLR-ZIP PIC X(9).
           02 MPLR-CLAIMNO PIC X(15).
           02 MPLR-TRINS PIC XXX.
           02 MPLR-TR-ASSIGN PIC X.
           02 MPLR-TR-GROUP PIC X(12).
           02 MPLR-TRIPOL PIC X(14).
           02 MPLR-TR-NAME PIC X(24).
           02 MPLR-TR-RELATE PIC X.
           02 MPLR-FUTURE PIC X(6).

       FD  AUTHFILE
           BLOCK CONTAINS 6 RECORDS
           DATA RECORD IS AUTHFILE01.
       01  AUTHFILE01.
           02 AUTH-KEY.
              03 AUTH-KEY8 PIC X(8).
              03 AUTH-KEY6 PIC X(6).
           02 AUTH-NUM PIC X(15).
           02 AUTH-QNTY PIC XX.
           02 AUTH-DATE-E PIC X(8).
           02 AUTH-FILLER PIC XXX.
       FD  CMNTFILE
      *    BLOCK CONTAINS 2 RECORDS
           DATA RECORD IS CMNTFILE01.
       01  CMNTFILE01.
           02 CMNT-KEY.
             03 CM-KEY8 PIC X(8).
             03 CM-KEY3 PIC XXX.
           02 CMNT PIC X(80).
           02 CMNT-DATE-E PIC X(8).

       FD  HISFILE
           BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS HISFILE01.
       01  HISFILE01.
           02 HISFILE-KEY.
             03 HS-KEY8 PIC X(8).
             03 HS-CLAIM PIC X(6).
             03 HS-REC-TYPE PIC X.
             03 HS-KEY4 PIC XXXX.
           02 HS-PATID. 
              03 HS-PATID7 PIC X(7).
              03 HS-PATID1 PIC X.
           02 HS-SERVICE PIC X.
           02 HS-DIAG PIC X(5).
           02 HS-PROC PIC X(11).
           02 HS-MOD2 PIC XX.
           02 HS-MOD3 PIC XX.
           02 HS-MOD4 PIC XX.
           02 HS-AMOUNT PIC X(6).
           02 HS-DOCR PIC X(3).
           02 HS-DOCP PIC X(2).
           02 HS-PAYCODE PIC XXX.
           02 HS-STUD PIC X.
           02 HS-WORK PIC XX.
           02 HS-DAT1 PIC X(8).
           02 HS-RESULT PIC X.
           02 HS-ACT PIC X.
           02 HS-SORCREF PIC X.
           02 HS-COLLT PIC X.
           02 HS-AGE PIC X.
           02 HS-PAPER PIC X.
           02 HS-PLACE PIC X.
           02 HS-EPSDT PIC X.
           02 HS-DATE-T PIC X(8).
           02 HS-DATE-A PIC X(8).
           02 HS-DATE-E PIC X(8).
           02 HS-REC-STAT PIC X.
           02 HS-DX2 PIC X(5).
           02 HS-DX3 PIC X(5).
           02 HS-ACC-TYPE PIC X.
           02 HS-DATE-M PIC X(8).
           02 HS-ASSIGN PIC X.
           02 HS-NEIC-ASSIGN PIC X.
           02 HS-FUTURE PIC X(6).
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       P0.
           OPEN I-O PAYFILE.
           OPEN I-O CHARFILE.
           OPEN I-O CHARCUR.
           OPEN I-O PAYCUR.
           OPEN I-O PATFILE.
           OPEN I-O GARFILE.
           OPEN I-O CMNTFILE.
           OPEN I-O HISFILE.
           OPEN I-O AUTHFILE.
           OPEN I-O MPLRFILE.
           OPEN INPUT KEEPBACK.
       P1. READ KEEPBACK AT END GO TO P11.
           IF KEEPTYPE = "01" GO TO P2.
           IF KEEPTYPE = "02" GO TO P3.
           IF KEEPTYPE = "03" GO TO P4.
           IF KEEPTYPE = "04" GO TO P5.
           IF KEEPTYPE = "05" GO TO P6.
           IF KEEPTYPE = "06" GO TO P7.
           IF KEEPTYPE = "07" GO TO P1.
           IF KEEPTYPE = "08" GO TO P8.
           IF KEEPTYPE = "09" GO TO P9.
           IF KEEPTYPE = "10" GO TO P10. 
           DISPLAY KEEPTYPE " ???". GO TO P1.
       P2. MOVE KEEP8 TO G-GARNO
           READ GARFILE INVALID DISPLAY G-GARNO " INVALID"
           GO TO P1.
           DELETE GARFILE RECORD INVALID DISPLAY "CANT DELETE " G-GARNO.
           GO TO P1.
       P3. MOVE KEEP11 TO CHARFILE-KEY
           READ CHARFILE INVALID DISPLAY "BAD CHARFILE" GO TO P1.
           DELETE CHARFILE RECORD INVALID DISPLAY "CANT DELETE CD-CHAR".
           GO TO P1.
       P4. MOVE KEEP11 TO PAYFILE-KEY.
           READ PAYFILE INVALID DISPLAY "BAD PAYFILE" GO TO P1.
           DELETE PAYFILE RECORD INVALID DISPLAY "CANT DELETE PD-PAY".
           GO TO P1.
       P5. MOVE KEEP11 TO CHARCUR-KEY
           READ CHARCUR INVALID DISPLAY "BAD CHARCUR" GO TO P1.
           DELETE CHARCUR RECORD INVALID DISPLAY "CANT DELETE CHARCUR".
           GO TO P1.
       P6. MOVE KEEP11 TO PAYCUR-KEY.
           READ PAYCUR INVALID DISPLAY "BAD PAYCUR" GO TO P1.
           DELETE PAYCUR RECORD INVALID DISPLAY "CANT DELETE PAYCUR".
           GO TO P1.
       P7. MOVE KEEP11 TO CMNT-KEY.
           READ CMNTFILE INVALID DISPLAY "BAD CMNTFILE" GO TO P1.
           DELETE CMNTFILE RECORD INVALID DISPLAY "CANT DELETE CMNT".
           GO TO P1.
       P8. MOVE KEEP11 TO HISFILE-KEY.
           READ HISFILE  INVALID DISPLAY "BAD HISFILE"  GO TO P1.
           DELETE HISFILE  RECORD INVALID DISPLAY "CANT DELETE HIST".
           GO TO P1.
       P9. MOVE KEEP11 TO AUTH-KEY.
           READ AUTHFILE  INVALID DISPLAY "BAD AUTHFILE"  GO TO P1.
           DELETE AUTHFILE  RECORD INVALID DISPLAY "CANT DELETE AUTH".
           GO TO P1.
       P10. MOVE KEEP8 TO MPLR-KEY.
           READ MPLRFILE  INVALID DISPLAY "BAD MPLRFILE"  GO TO P1.
           DELETE MPLRFILE  RECORD INVALID DISPLAY "CANT DELETE MPLR".
           GO TO P1.
       P11. CLOSE CHARFILE CHARCUR PAYFILE PAYCUR GARFILE PATFILE
           CMNTFILE HISFILE AUTHFILE MPLRFILE.
           DISPLAY "GARNAME PROGRAM HAS ENDED."
           STOP RUN.
