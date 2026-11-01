      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2026 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
      *
      *    LIST THE CHARNEW CHARGES THAT CAME IN WITH NO RADIOLOGIST
      *    (CD-DOCP = "00") SO php/rri/rri_docp.php CAN LOOK UP THE
      *    READ IN OPENEMR. G1004 IS SKIPPED, IT IS NEVER READ.
      *    ONE LINE PER CHARGE, FIXED COLUMNS:
      *      1-11  CHARNEW-KEY
      *     12-19  G-ACCT (THE MRN OPENEMR KNOWS)
      *     20-26  CD-VISITNO
      *     27-34  CD-DATE-T
      *     35-39  CD-CPT
      *    RRR256 PUTS THE ANSWERS BACK INTO CHARNEW.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrr255.
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
       01  FILEOUT01 PIC X(39).

       WORKING-STORAGE SECTION.

       01  REC-CNT PIC 9(6) VALUE 0.
       01  OUT-CNT PIC 9(6) VALUE 0.
       01  NOGAR-CNT PIC 9(6) VALUE 0.
      *
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

           IF CD-DOCP NOT = "00" OR CD-CPT = "G1004"
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
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           ADD 1 TO OUT-CNT
           GO TO P1.

       P4.
           DISPLAY "RRR255 READ " REC-CNT " UNREAD 00 " OUT-CNT
               " NO GARFILE " NOGAR-CNT
           CLOSE CHARNEW GARFILE FILEOUT.
           STOP RUN.
