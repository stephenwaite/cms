      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @author  Claude (Anthropic)
      * @copyright Copyright (c) 2026 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
      *
      * garchk01 - scan GARFILE for records whose GARNO prefix does not
      * match the parsed G-GARNAME prefix.  These are the records that
      * break the GARNO-ordered / name-matched search in the maintenance
      * program (a non-matching name prefix mid-range triggers the early
      * "NO MORE MATCHES ON NAME" exit, stranding later GARNOs).
      *
      * Replicates the live search's prefix derivation:
      *   garno prefix = G-GARNO (1:3)
      *   name  prefix = first 3 chars of the ";"-delimited first token
      *                  of G-GARNAME  (same UNSTRING the search uses)
      *
      * Writes mismatches to a line-sequential report and tallies counts.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. garchk01.
       AUTHOR. S WAITE.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT GARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
             ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
             LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO "S45" ORGANIZATION IS
             LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  GARFILE.
           COPY GARFILE.CPY.

       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).

       WORKING-STORAGE SECTION.

       01  NAME-TEST    PIC X(24).
       01  NAME-DISCARD PIC X(24).
       01  GARNO-PFX    PIC XXX.
       01  NAME-PFX     PIC XXX.

       01  CNT-TOTAL    PIC 9(7) VALUE 0.
       01  CNT-MISMATCH PIC 9(7) VALUE 0.

       01  REPORT-LINE.
           05  RL-GARNO    PIC X(8).
           05  FILLER      PIC X(2) VALUE SPACE.
           05  RL-GPFX     PIC X(3).
           05  FILLER      PIC X(2) VALUE SPACE.
           05  RL-NPFX     PIC X(3).
           05  FILLER      PIC X(2) VALUE SPACE.
           05  RL-NAME     PIC X(24).
           05  FILLER      PIC X(2) VALUE SPACE.
           05  RL-ACCT     PIC X(8).

       01  TOT-LINE.
           05  FILLER      PIC X(18) VALUE "RECORDS SCANNED:  ".
           05  TL-TOTAL    PIC ZZZ,ZZZ,ZZ9.
       01  MIS-LINE.
           05  FILLER      PIC X(18) VALUE "PREFIX MISMATCH:  ".
           05  ML-MIS      PIC ZZZ,ZZZ,ZZ9.

       PROCEDURE DIVISION.

       P0.
           OPEN INPUT GARFILE
           OPEN OUTPUT FILEOUT.

       R1.
           READ GARFILE NEXT
             AT END
               GO TO R99.

           ADD 1 TO CNT-TOTAL

      *    derive the two prefixes the live search compares
           MOVE SPACE TO NAME-TEST NAME-DISCARD
           UNSTRING G-GARNAME DELIMITED BY ";"
             INTO NAME-TEST NAME-DISCARD

           MOVE G-GARNO   (1:3) TO GARNO-PFX
           MOVE NAME-TEST (1:3) TO NAME-PFX

           IF GARNO-PFX NOT = NAME-PFX
               ADD 1 TO CNT-MISMATCH
               MOVE SPACE   TO REPORT-LINE
               MOVE G-GARNO   TO RL-GARNO
               MOVE GARNO-PFX TO RL-GPFX
               MOVE NAME-PFX  TO RL-NPFX
               MOVE G-GARNAME TO RL-NAME
               MOVE G-ACCT    TO RL-ACCT
               MOVE REPORT-LINE TO FILEOUT01
               WRITE FILEOUT01
           END-IF

           GO TO R1.

       R99.
           MOVE CNT-TOTAL    TO TL-TOTAL
           MOVE CNT-MISMATCH TO ML-MIS
           DISPLAY TOT-LINE
           DISPLAY MIS-LINE
           CLOSE GARFILE FILEOUT
           STOP RUN.