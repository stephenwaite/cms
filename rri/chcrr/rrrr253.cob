      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CAR253.
       AUTHOR. SID WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARFILE-KEY
           LOCK MODE MANUAL.
           SELECT ERROR-FILE ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT GARFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC  RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE MANUAL.
       DATA DIVISION.
       FILE SECTION.
       FD  CHARFILE
           BLOCK CONTAINS 4 RECORDS
           DATA RECORD IS CHARFILE01.
       01  CHARFILE01.
           02 CHARFILE-KEY.
             03 CD-KEY8 PIC X(8).
             03 CD-KEY3 PIC XXX.
           02 CD-PATID PIC X(8).
           02 CD-CLAIM PIC X(6).
           02 CD-SERVICE PIC X.
           02 CD-DIAG PIC X(7).
           02 CD-PROC0 PIC X(4).
           02 CD-PROC1 PIC X(9).
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
           02 CD-AGE PIC X.
           02 CD-PAPER PIC X.
           02 CD-PLACE PIC X.
           02 CD-NAME PIC X(24).
           02 CD-EPSDT PIC X.
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
       FD GARFILE
      *    BLOCK CONTAINS 3 RECORDS
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
       FD  ERROR-FILE.
       01  ERROR-FILE01.
           02 ER-0 PIC X(8).
           02 FILLER PIC X VALUE SPACE.
           02 ER-1 PIC X(11).
           02 FILLER PIC X VALUE SPACE.
           02 ER-2 PIC X(24).
           02 FILLER PIC X VALUE SPACE.
           02 ER-3 PIC X(22).
           02 FILLER PIC X VALUE SPACE.
           02 ER-4 PIC X(22).
       WORKING-STORAGE SECTION.
       01 CHARBACK PIC X(11).
       01 DATE-TAB01.
           02 DATE-TAB PIC 9(8) OCCURS 90 TIMES.
           02 CHARGE-TAB PIC X(9) OCCURS 90 TIMES.
           02 KEY-TAB PIC X(11) OCCURS 90 TIMES.
           02 WORK-TAB PIC 99 OCCURS 90 TIMES.
           02 AMOUNT-TAB PIC S9(4)V99 OCCURS 90 TIMES.
       01  X PIC 99.
       01  Y PIC 99.
       01 Z PIC 99.
       01 T PIC 99.
       01 A PIC 99.
       01 FLAGX PIC 9 VALUE 0.
      *
       PROCEDURE DIVISION.
       P0.
           OPEN I-O CHARFILE INPUT GARFILE OUTPUT ERROR-FILE.
           MOVE SPACE TO CHARFILE-KEY.
       P0-1.
           START CHARFILE KEY NOT < CHARFILE-KEY INVALID GO TO P4.
       P1.
           READ CHARFILE NEXT AT END GO TO P4.
           IF CD-PAYCODE NOT = "003" GO TO P1.
           IF CD-SERVICE = "1" GO TO P1.
       P1-1.
           MOVE CD-KEY8 TO G-GARNO
           READ GARFILE INVALID
            DISPLAY CD-KEY8 " NO GARNO RECORD" GO TO P1.
           MOVE 1 TO X
           MOVE G-GARNO TO ER-0
           MOVE G-GARNAME TO ER-2
           MOVE CHARFILE-KEY TO KEY-TAB(X)
           MOVE CD-WORK TO WORK-TAB(X)
           MOVE CD-AMOUNT TO AMOUNT-TAB(X)
           MOVE CD-DATE-T TO DATE-TAB(X)
           MOVE CD-PROC1 TO CHARGE-TAB(X).
       P2.
           READ CHARFILE NEXT AT END GO TO P4.
           IF CD-KEY8 NOT = G-GARNO GO TO P14.
           IF CD-PAYCODE NOT = "003" GO TO P2.
           IF CD-SERVICE = "1" GO TO P2.
           ADD 1 TO X
           MOVE CD-DATE-T TO DATE-TAB(X)
           MOVE CHARFILE-KEY TO KEY-TAB(X)
           MOVE CD-WORK TO WORK-TAB(X)
           MOVE CD-AMOUNT TO AMOUNT-TAB(X)
           MOVE CD-PROC1 TO CHARGE-TAB(X)
           GO TO P2.
       P14.
           IF X < 2 GO TO P1-1.
           MOVE CHARFILE-KEY TO CHARBACK.
           PERFORM P15 MOVE CHARBACK TO CHARFILE-KEY GO TO P0-1.
       P15.
           SUBTRACT 1 FROM X GIVING Y.
           PERFORM C1 THRU C1-EXIT VARYING Z FROM 1 BY 1 UNTIL Z > Y.
       C1. IF DATE-TAB(Z) = 0 GO TO C1-EXIT.
           MOVE 0 TO FLAGX.
           ADD 1 Z GIVING A
           PERFORM C2 THRU C2-EXIT VARYING T FROM A BY 1 UNTIL T > X.
           IF FLAGX = 0 GO TO C1-EXIT.
           MOVE KEY-TAB(Z) TO CHARFILE-KEY
           READ CHARFILE INVALID DISPLAY CHARFILE-KEY " INVALID"
           GO TO C1-EXIT.
           MOVE WORK-TAB(Z) TO CD-WORK MOVE AMOUNT-TAB(Z) TO CD-AMOUNT
           REWRITE CHARFILE01 INVALID DISPLAY CHARFILE-KEY
           " BAD REWRITE".
           CLOSE CHARFILE
           OPEN I-O CHARFILE.
       C1-EXIT. EXIT.
       C2. IF (DATE-TAB(Z) NOT = DATE-TAB(T)) OR (CHARGE-TAB(Z)
           NOT =  CHARGE-TAB(T)) OR (DATE-TAB(T) = 0)
           GO TO C2-EXIT.
           ADD AMOUNT-TAB(T) TO AMOUNT-TAB(Z)
           ADD WORK-TAB(T) TO WORK-TAB(Z)
           MOVE 1 TO FLAGX
           MOVE KEY-TAB(Z) TO ER-1
           MOVE SPACE TO ER-3
           STRING G-PRINS "/" G-SEINS  "SAME CHARGES?" DELIMITED
           BY "  " INTO ER-3
           STRING CHARGE-TAB(Z) "/" DATE-TAB(Z) DELIMITED BY "ZZ"
           INTO ER-4
           WRITE ERROR-FILE01
           MOVE 0 TO DATE-TAB(T).
           MOVE KEY-TAB(T) TO CHARFILE-KEY
           READ CHARFILE INVALID DISPLAY CHARFILE-KEY "CANT READ DELETE"
           GO TO C2-EXIT.
      *     DELETE CHARFILE RECORD INVALID DISPLAY CHARFILE-KEY
      *     " CANT DELETE RECORD".
           CLOSE CHARFILE OPEN I-O CHARFILE.
       C2-EXIT. EXIT.
       P4. PERFORM P15 CLOSE ERROR-FILE CHARFILE. STOP RUN.
