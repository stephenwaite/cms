      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
      *    load a 277 / 277CA file into the 277 history isam (H277FILE)
      *    safe to rerun on the same file, identical rows are skipped
      *    S30 parm139 (line 2 = the rri npis, line 3 = the rri tax id,
      *    only claims whose billing provider matches are kept)
      *    S35 277 file, S80 h277hist
       IDENTIFICATION DIVISION.
       PROGRAM-ID. h277load.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARMFILE ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEIN ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT OPTIONAL H277FILE ASSIGN TO "S80"
           ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS H277-KEY
           ALTERNATE RECORD KEY IS H277-PAYID WITH DUPLICATES
           LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PARMFILE.
       01  PARMFILE01 PIC X(40).
       FD  FILEIN.
       01  FILEIN01 PIC X(256).
       FD  H277FILE.
           COPY h277file.cpy.

       WORKING-STORAGE SECTION.
           COPY h277ws.cpy.

       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT PARMFILE.
           READ PARMFILE AT END GO TO P98.
           READ PARMFILE AT END GO TO P98.
           UNSTRING PARMFILE01 DELIMITED BY " " INTO H-PROV1 H-PROV2
           READ PARMFILE
             AT END CONTINUE
             NOT AT END MOVE PARMFILE01 TO H-PROVTIN
           END-READ
           CLOSE PARMFILE
           IF H-PROV1 = SPACE GO TO P98.
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

       P98.
           DISPLAY "h277load: no npis on line 2 of the parm file (S30)"
           STOP RUN.

           COPY h277proc.cpy.
