      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rri247.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO "S30" ORGANIZATION LINE
           SEQUENTIAL.
           SELECT ACTFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS A-ACTNO
           ALTERNATE RECORD KEY IS A-GARNO WITH DUPLICATES
           ALTERNATE RECORD KEY IS NAME-KEY WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT FILEOUT ASSIGN TO "S40" ORGANIZATION LINE
           SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD FILEIN.
       01 FILEIN01.
          02 FI-1 PIC X(8).
       FD ACTFILE
           BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS ACTFILE01.
       01 ACTFILE01.
           02 A-ACTNO PIC X(8).
           02 A-GARNAME.
             03 A-GN1 PIC XXX.
             03 A-GN2 PIC X(21).
           02 A-BILLADD PIC X(22).
           02 A-STREET PIC X(22).
           02 A-CITY PIC X(18).
           02 A-STATE PIC X(2).
           02 A-ZIP PIC X(9).
           02 A-COLLT PIC X.
           02 A-PHONE.
             03 A-PHONE1 PIC XXX.
             03 A-PHONE2 PIC XXX.
             03 A-PHONE3 PIC X(4).
           02 A-SEX PIC X.
           02 A-RELATE PIC X.
           02 A-MSTAT PIC X.
           02 A-DOB PIC X(8).
           02 A-DUNNING PIC X.
           02 A-ACCTSTAT PIC X.
           02 A-PR-MPLR PIC X(4).
           02 A-PRINS PIC XXX.
           02 A-PR-ASSIGN PIC X.
           02 A-PR-OFFICE PIC X(4).
           02 A-PR-GROUP PIC X(10).
           02 A-PRIPOL.
             03 A-PRIPOL1 PIC X(9).
             03 A-PRIPOL2 PIC XXX.
             03 A-PR-FILLER PIC X(4).
           02 A-PRNAME PIC X(24).
           02 A-PR-RELATE PIC X.
           02 A-SE-MPLR PIC X(4).
           02 A-SEINS PIC XXX.
           02 A-SE-ASSIGN PIC X.
           02 A-TRINSIND PIC X.
           02 A-TRINS PIC XXX.
           02 A-SE-GROUP PIC X(10).
           02 A-SECPOL.
             03 A-SECPOL1 PIC X(9).
             03 A-SECPOL2 PIC XXX.
             02 A-SE-FILLER PIC X(4).
           02 A-SENAME PIC X(24).
           02 A-SE-RELATE PIC X.
           02 A-INSPEND PIC X(7).
           02 A-LASTBILL PIC X(8).
           02 A-ASSIGNM PIC X.
           02 A-PRIVATE PIC X.
           02 A-BILLCYCLE PIC X.
           02 A-DELETE PIC X.
           02 A-FILLER PIC XXX.
           02 A-GARNO PIC X(8).
           02 A-PRGRPNAME PIC X(15).
           02 A-SEGRPNAME PIC X(15).
           02 NAME-KEY PIC XXX.
       FD  FILEOUT.
       01 FILEOUT01 PIC X(309).
       WORKING-STORAGE SECTION.
       01  FLAG PIC 9.
       01  ALF-1 PIC X.
       01  CNTR PIC 99 VALUE 0.
       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT FILEIN.
           OPEN OUTPUT FILEOUT.
           OPEN I-O ACTFILE.
       P1. READ FILEIN AT END GO TO P9.
           MOVE 0 TO FLAG
           MOVE FI-1 TO A-ACTNO
           READ ACTFILE WITH LOCK INVALID DISPLAY "BAD" GO TO P1.
           IF A-PRINS = "001" GO TO P1.
           IF (A-PR-RELATE = "K") AND (A-SE-RELATE = "K" OR " ")
           GO TO P1.
           IF A-PR-RELATE = "K" GO TO S2.
           IF (A-PRNAME = A-GARNAME) AND (A-RELATE = "K")  
           MOVE "K" TO A-PR-RELATE MOVE 1 TO FLAG GO TO S2.
       S1.
           DISPLAY "PRIMARY " A-PR-RELATE " " A-PRNAME " " A-ACTNO
           ADD 1 TO CNTR
           IF CNTR > 20
            ACCEPT ALF-1
            MOVE 0 TO CNTR
           END-IF
           IF ALF-1 = "." PERFORM A1 GO TO S1.
           IF ALF-1 = "2" OR "K" MOVE 1 TO FLAG
           MOVE ALF-1 TO A-PR-RELATE.
           IF A-SE-RELATE = " " GO TO P2.
           IF A-SE-RELATE = "K" GO TO P2.
       S2.
           IF A-SE-RELATE = "K" GO TO P2.
           IF (A-SENAME = A-GARNAME) AND (A-RELATE = "K")  
           MOVE "K" TO A-SE-RELATE MOVE 1 TO FLAG GO TO P2.
           IF A-SEINS = "001" GO TO P2.
           DISPLAY "SECOND. " A-SE-RELATE " " A-SENAME " " A-ACTNO
           ADD 1 TO CNTR
           IF CNTR > 20
            ACCEPT ALF-1
            MOVE 0 TO CNTR
           END-IF
           IF ALF-1 = "." PERFORM A1 GO TO S2.
           IF ALF-1 = "2" OR "K" MOVE 1 TO FLAG
           MOVE ALF-1 TO A-SE-RELATE.
       P2.
           IF FLAG = 1
           WRITE FILEOUT01 FROM ACTFILE01
           REWRITE ACTFILE01.
           GO TO P1.
       A1.
           DISPLAY A-GARNAME
           DISPLAY "SEX  " A-SEX  " RELATE " A-RELATE
           DISPLAY A-PRINS " " A-PR-ASSIGN " " A-PRIPOL " " A-PR-GROUP
           " " A-PR-MPLR " " A-PR-OFFICE " " A-PR-RELATE " " A-PRNAME
           IF A-PRGRPNAME NOT = SPACE DISPLAY A-PRGRPNAME.
           DISPLAY A-SEINS " " A-SE-ASSIGN " " A-SECPOL " " A-SE-GROUP
           " " A-SE-MPLR "      " A-SE-RELATE " " A-SENAME.
       P9.
           ACCEPT ALF-1
           CLOSE ACTFILE FILEIN FILEOUT.
           DISPLAY "FIX RELATE CODES OF INSURANCES".
           STOP RUN.
