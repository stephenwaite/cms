      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rri249.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ORDFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL RECORD KEY IS ORDNO
           ALTERNATE RECORD KEY IS C-DATE-E WITH DUPLICATES.

           SELECT ORDFILEBK ASSIGN TO "S35" ORGANIZATION LINE
           SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S40" ORGANIZATION LINE
           SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  FILEOUT.
       01 FILEOUT01 PIC X(8).

       FD ORDFILE
           DATA RECORD IS ORDFILE01.
       01 ORDFILE01.
           02 ORDNO.
             03 ORD8 PIC X(8).
             03 ORD3 PIC XXX.
           02 C-PROC1 PIC X(4).
           02 C-PROC2 PIC X.
           02 C-REF PIC XXX.
           02 C-IOPAT PIC X.
           02 C-DATE-A PIC X(8).
           02 C-DATE-T PIC X(8).
           02 C-DATE-ADMIT PIC X(8).
           02 C-ORDER PIC XXXX.
           02 C-CLINICAL PIC X(40).
           02 C-ADMIT-DIAG PIC X(30).
           02 C-DATE-E PIC X(8).
           02 C-CPT PIC X(5).

       FD ORDFILEBK
           DATA RECORD IS ORDFILEBK01.
       01 ORDFILEBK01.
           02 ORDNOBK.
             03 ORD8BK PIC X(8).
             03 ORD3BK PIC XXX.
           02 C-PROC1BK PIC X(4).
           02 C-PROC2BK PIC X.
           02 C-REFBK PIC XXX.
           02 C-IOPATBK PIC X.
           02 C-DATE-ABK PIC X(8).
           02 C-DATE-TBK PIC X(8).
           02 C-DATE-ADMITBK PIC X(8).
           02 C-ORDERBK PIC XXXX.
           02 C-CLINICALBK PIC X(40).
           02 C-ADMIT-DIAGBK PIC X(30).
           02 C-CPTBK PIC X(5).

       WORKING-STORAGE SECTION.
       01  CONSTANTS.
           02 AMT PIC S9999999V99 VALUE 0.
           02 NEF-11 PIC $$,$$$,$$9.99CR.
       01  INPUT-DATE-S.
           05 T-MM  PIC 99.
           05 T-DD  PIC 99.
           05 T-CC  PIC 99.
           05 T-YY  PIC 99.
       01  TEST-DATE-S.
           05 T-CC  PIC 99.
           05 T-YY  PIC 99.
           05 T-MM  PIC 99.
           05 T-DD  PIC 99.
       01 ALF8 PIC X(8).
       01 DATE-X PIC X(8).
       01 DATE-Y PIC X(8).
       01 HOLD-ORD PIC X(8).

       PROCEDURE DIVISION.

       0005-START.
           OPEN INPUT ORDFILE
           OPEN OUTPUT ORDFILEBK FILEOUT.
           MOVE "00000000" TO DATE-X
           MOVE "99991231" TO DATE-Y.

       P1. 
           READ ORDFILE
             AT END
               GO TO P3
           END-READ.

       P1-1.
           IF C-PROC2 = "-"
               GO TO P1
           END-IF

           IF C-DATE-T < DATE-X OR C-DATE-T > DATE-Y
               GO TO P1
           END-IF

           MOVE ORDFILE01 TO ORDFILEBK01
           WRITE ORDFILEBK01.

       P2. 
           READ ORDFILE
             AT END
               GO TO P3
           END-READ

           IF ORD8 = ORD8BK
               GO TO P2
           END-IF

           GO TO P1-1.

       P3.
           WRITE FILEOUT01 FROM DATE-Y.
           CLOSE ORDFILE ORDFILEBK FILEOUT.
           DISPLAY "SEQUENTIAL BACKUP OF ORDFILE ENDED".
           STOP RUN.
