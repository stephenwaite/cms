      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
      *    load a 277 / 277CA file into the 277 history isam (H277FILE)
      *    safe to rerun on the same file, identical rows are skipped
       IDENTIFICATION DIVISION.
       PROGRAM-ID. h277load.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT OPTIONAL H277FILE ASSIGN TO "S80"
           ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS H277-KEY
           ALTERNATE RECORD KEY IS H277-PAYID WITH DUPLICATES
           LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FILEIN.
       01  FILEIN01 PIC X(256).
       FD  H277FILE.
           COPY h277file.cpy.

       WORKING-STORAGE SECTION.
           COPY h277ws.cpy.

       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT FILEIN.
           OPEN I-O H277FILE.

       P00.
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P99.
           MOVE FILEIN01 TO H-SEG
           PERFORM H277-SEG
           GO TO P00.

       P99.
           PERFORM H277-FLUSH
           DISPLAY "277 HIST ADDED " H277-WCNT " ALREADY ON FILE "
             H277-DCNT
           CLOSE FILEIN H277FILE
           STOP RUN.

           COPY h277proc.cpy.
