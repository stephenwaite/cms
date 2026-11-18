      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2026 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE
      *          GNU General Public License 3
      *
      *  rrr258: list the charnew charges whose quality measure
      *  questions can be read from the report, for
      *  measure_charnew.php (rri-measure-suggest).
      *
      *  A charge is listed when mea003 flagged it (paycode 009,
      *  010, 012-017), a radiologist has read it (CD-DOCP not 00
      *  and not 02), the coder has not answered (CD-QP1..3 blank)
      *  and no suggestion is on it yet (CD-QS1..3 blank).
      *
      *  out: key(11) acct(8) visitno(7) dos(8) cpt(5) paycode(3)
      *       dob(8) clinical(40) admit diag(23)
      *----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrr258.
       AUTHOR. s WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARNEW ASSIGN TO "S30" ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL RECORD KEY IS CHARNEW-KEY.

           SELECT GARFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
               ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
               LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO "S40" ORGANIZATION
               LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  CHARNEW.
           COPY charnew.CPY.

       FD  GARFILE.
           COPY garfile.CPY.

       FD  FILEOUT.
       01  FILEOUT01 PIC X(113).

       WORKING-STORAGE SECTION.

       01  REC-CNT PIC 9(6) VALUE 0.
       01  FLAG-CNT PIC 9(6) VALUE 0.
       01  OUT-CNT PIC 9(6) VALUE 0.
       01  UNREAD-CNT PIC 9(6) VALUE 0.
       01  DONE-CNT PIC 9(6) VALUE 0.
       01  NOGAR-CNT PIC 9(6) VALUE 0.
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT CHARNEW GARFILE
           OPEN OUTPUT FILEOUT.

       P1.
           READ CHARNEW
             AT END
               GO TO P4
           END-READ

           ADD 1 TO REC-CNT

           IF NOT (CD-PAYCODE = "009" OR "010" OR "012" OR "013"
               OR "014" OR "015" OR "016" OR "017")
               GO TO P1
           END-IF

           ADD 1 TO FLAG-CNT

           IF CD-DOCP = "00" OR "02"
               ADD 1 TO UNREAD-CNT
               GO TO P1
           END-IF

           IF NOT (CD-QP1 = SPACE AND CD-QP2 = SPACE
               AND CD-QP3 = SPACE AND CD-QS1 = SPACE
               AND CD-QS2 = SPACE AND CD-QS3 = SPACE)
               ADD 1 TO DONE-CNT
               GO TO P1
           END-IF

           MOVE CD-KEY8 TO G-GARNO
           READ GARFILE
             INVALID
               DISPLAY CHARNEW-KEY " NO GARFILE RECORD, SKIPPED"
               ADD 1 TO NOGAR-CNT
               GO TO P1
           END-READ

           MOVE SPACE TO FILEOUT01
           STRING CHARNEW-KEY G-ACCT CD-VISITNO CD-DATE-T CD-CPT
               CD-PAYCODE G-DOB CD-CLINICAL CD-ADMIT-DIAG
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           ADD 1 TO OUT-CNT
           GO TO P1.

       P4.
           DISPLAY "RRR258 READ " REC-CNT " FLAGGED " FLAG-CNT
               " LISTED " OUT-CNT
           DISPLAY "  UNREAD " UNREAD-CNT " ANSWERED OR SUGGESTED "
               DONE-CNT " NO GARFILE " NOGAR-CNT
           CLOSE CHARNEW GARFILE FILEOUT.
           STOP RUN.
