      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2026 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE
      *          GNU General Public License 3
      *
      *  mea006: write the quality measure answers read from the
      *  reports (measure_charnew.php) into CD-QS1..3 of charnew,
      *  where xxx003c shows them to the coder beside the question.
      *  The coder's answers (CD-QP1..3) are never written here.
      *
      *  A charge is left alone when it is gone from charnew, is
      *  no longer flagged for a measure, or the coder has answered
      *  since the extract. Each record changed is first written
      *  whole to the backup file.
      *
      *  in:  key(11) qs1(2) qs2(2) qs3(2)   a suggestion is the
      *       answer digit and H, M or L; spaces for none
      *----------------------------------------------------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. mea006.
       AUTHOR. s WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILEIN ASSIGN TO "S30" ORGANIZATION
               LINE SEQUENTIAL.

           SELECT CHARNEW ASSIGN TO "S35" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS CHARNEW-KEY
               LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO "S40" ORGANIZATION
               LINE SEQUENTIAL.

           SELECT BAKFILE ASSIGN TO "S45" ORGANIZATION
               LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  FILEIN.
       01  FILEIN01.
           02 FI-KEY  PIC X(11).
           02 FI-QS1  PIC XX.
           02 FI-QS2  PIC XX.
           02 FI-QS3  PIC XX.

       FD  CHARNEW.
           COPY charnew.CPY.

       FD  FILEOUT.
       01  FILEOUT01 PIC X(120).

       FD  BAKFILE.
       01  BAKFILE01 PIC X(300).

       WORKING-STORAGE SECTION.

       01  REC-CNT PIC 9(6) VALUE 0.
       01  UPD-CNT PIC 9(6) VALUE 0.
       01  LEFT-CNT PIC 9(6) VALUE 0.
       PROCEDURE DIVISION.
       P0.
           OPEN INPUT FILEIN
           OPEN I-O CHARNEW
           OPEN OUTPUT FILEOUT BAKFILE.

       P1.
           MOVE SPACE TO FILEIN01
           READ FILEIN
             AT END
               GO TO P4
           END-READ

           IF FILEIN01 = SPACE
               GO TO P1
           END-IF

           ADD 1 TO REC-CNT

           MOVE FI-KEY TO CHARNEW-KEY
           READ CHARNEW
             INVALID
               ADD 1 TO LEFT-CNT
               MOVE SPACE TO FILEOUT01
               STRING FI-KEY " NOT IN CHARNEW ANY MORE"
                   DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P1
           END-READ

           IF NOT (CD-PAYCODE = "009" OR "010" OR "012" OR "013"
               OR "014" OR "015" OR "016" OR "017")
               ADD 1 TO LEFT-CNT
               MOVE SPACE TO FILEOUT01
               STRING FI-KEY " NOT FLAGGED NOW, LEFT ALONE"
                   DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P1
           END-IF

           IF NOT (CD-QP1 = SPACE AND CD-QP2 = SPACE
               AND CD-QP3 = SPACE)
               ADD 1 TO LEFT-CNT
               MOVE SPACE TO FILEOUT01
               STRING FI-KEY " ANSWERED BY THE CODER, LEFT ALONE"
                   DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P1
           END-IF

           MOVE SPACE TO BAKFILE01
           MOVE CHARNEW01 TO BAKFILE01
           WRITE BAKFILE01

           MOVE FI-QS1 TO CD-QS1
           MOVE FI-QS2 TO CD-QS2
           MOVE FI-QS3 TO CD-QS3
           REWRITE CHARNEW01
             INVALID
               ADD 1 TO LEFT-CNT
               MOVE SPACE TO FILEOUT01
               STRING FI-KEY " CANT REWRITE"
                   DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P1
           END-REWRITE

           ADD 1 TO UPD-CNT
           MOVE SPACE TO FILEOUT01
           STRING FI-KEY " " CD-PAYCODE " CPT " CD-CPT
               " DOS " CD-DATE-T " SUGGESTED " FI-QS1 " " FI-QS2
               " " FI-QS3
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           GO TO P1.

       P4.
           MOVE SPACE TO FILEOUT01
           STRING "MEA006 READ " REC-CNT " SET " UPD-CNT
               " LEFT ALONE " LEFT-CNT
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           DISPLAY FILEOUT01
           CLOSE FILEIN CHARNEW FILEOUT BAKFILE.
           STOP RUN.
