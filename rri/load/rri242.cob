      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rri242.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ACTFILE ASSIGN TO "S30"     ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC        RECORD KEY IS A-ACTNO
           ALTERNATE RECORD KEY IS A-GARNO WITH DUPLICATES
           ALTERNATE RECORD KEY IS NAME-KEY WITH DUPLICATES.

           SELECT FILEIN ASSIGN TO "S35"ORGANIZATION 
           LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S40" ORGANIZATION LINE
           SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  FILEIN.
       01  FILEIN01 PIC X(8).

       FD  FILEOUT.
       01  FILEOUT01.
           02 FO-1 PIC X(9).
           02 FO-2 PIC X(4).
           02 FO-3 PIC X(15).
           02 FO-4 PIC X(13).
           02 FO-5 PIC X(16).

       FD ACTFILE
           DATA RECORD IS ACTFILE01.
       01 ACTFILE01.
           02 A-ACTNO PIC X(8).
           02 A-GARNAME PIC X(24).
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
           02 A-SE-OFFICE PIC X(4).
           02 A-SE-GROUP PIC X(10).
           02 A-SECPOL.
             03 A-SECPOL1 PIC X(9).
             03 A-SECPOL2 PIC XXX.
             03 A-SE-FILLER PIC X(4).
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

       WORKING-STORAGE SECTION.
       01  X PIC 99.
       01  ALF7 PIC X(7).
       01 ALF3 PIC XXX.

       PROCEDURE DIVISION.

       0005-START.
           OPEN I-O ACTFILE
           OPEN INPUT FILEIN
           OUTPUT FILEOUT.

       P1.
           READ FILEIN
             AT END
               GO TO P2
           END-READ

           MOVE FILEIN01 TO A-ACTNO
           READ ACTFILE INVALID DISPLAY FILEIN01 " BAD ACTNO"
           GO TO P1.
           IF A-PRINS NOT = "003" GO TO P1.
           IF A-SEINS = "001" OR "004" OR "064" OR "005" GO TO P1.
           MOVE A-SEINS TO ALF3
           
           IF A-SEINS = "025"
           MOVE "0005034" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF (A-SEINS = "108")
           AND (A-SE-GROUP = "0038278" OR "038278" OR "38278")
           MOVE "0005037" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF (A-SEINS = "116")
           MOVE "0000567" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF A-SEINS = "141"
           MOVE "0099003" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF A-SEINS = "209"
           MOVE "0000343" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF A-SEINS = "216"
           MOVE "0005004" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF A-SEINS = "217"
           MOVE "0001177" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF A-SEINS = "218"
           MOVE "0000172" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF A-SEINS = "220"
           MOVE "0005004" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF A-SEINS = "226"
           MOVE "0005154" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF A-SEINS = "227"
           MOVE "0000557" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF A-SEINS = "228"
           MOVE "0000582" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF A-SEINS = "230"
           MOVE "0005023" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF A-SEINS = "231"
           MOVE "0001110" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF A-SEINS = "234"
           MOVE "0000762" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF A-SEINS = "236"
           MOVE "0000873" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF A-SEINS = "237"
           MOVE "0000762" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF A-SEINS = "238"
           MOVE "0005041" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF A-SEINS = "239"
           MOVE "0000111" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF A-SEINS = "240"
           MOVE "0005162" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF A-SEINS = "241"
           MOVE "0005034" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF A-SEINS = "245"
           MOVE "0000701" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           
           IF A-SEINS = "246"
           MOVE "0005012" TO ALF7
           MOVE "062" TO A-SEINS
           MOVE "A" TO A-SE-ASSIGN
           PERFORM A1 GO TO P1.
           GO TO P1.

       A1. 
           MOVE SPACE TO A-PR-GROUP
           MOVE ALF7 TO A-PR-GROUP
           MOVE A-ACTNO TO FO-1
           MOVE ALF3    TO FO-2
           MOVE A-SECPOL TO FO-3
           MOVE A-SE-GROUP TO FO-4
           MOVE A-SEGRPNAME TO FO-5
           WRITE FILEOUT01
           REWRITE ACTFILE01.

       P2.
           CLOSE ACTFILE FILEIN FILEOUT.
           STOP RUN.
