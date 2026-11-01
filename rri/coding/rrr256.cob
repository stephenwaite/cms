      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2026 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
      *
      *    PUT THE READING RADIOLOGIST php/rri/rri_docp.php FOUND IN
      *    OPENEMR INTO CD-DOCP ON CHARNEW. INPUT (S30) IS ONE LINE
      *    PER CHARGE RRR255 LISTED:
      *      1-11  CHARNEW-KEY
      *     12-13  DOCP (00 = NOT FOUND)
      *     15-18  SOURCE  SIGN / FEED / OVER / NONE
      *     20-    NOTE
      *    ONLY A CHARGE STILL AT 00 IS CHANGED, AND ONLY TO ONE OF
      *    OUR FOUR RADIOLOGISTS. OVER MEANS ANOTHER OF THE FOUR
      *    SIGNED THAN THE FEED IT CAME IN ON, THE SIGNER IS USED BUT
      *    IT IS ON THE REPORT FOR A CODER TO LOOK AT.
      *    EVERY CHANGED RECORD IS WRITTEN WHOLE TO BAKFILE (S45)
      *    BEFORE THE REWRITE SO IT CAN BE PUT BACK.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrr256.
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
           02 FI-DOCP PIC XX.
           02 FILLER  PIC X.
           02 FI-SRC  PIC X(4).
           02 FILLER  PIC X.
           02 FI-NOTE PIC X(61).

       FD  CHARNEW.
           COPY charnew.CPY.

       FD  FILEOUT.
       01  FILEOUT01 PIC X(120).

       FD  BAKFILE.
       01  BAKFILE01 PIC X(300).

       WORKING-STORAGE SECTION.

       01  REC-CNT PIC 9(6) VALUE 0.
       01  UPD-CNT PIC 9(6) VALUE 0.
       01  OVER-CNT PIC 9(6) VALUE 0.
       01  LEFT-CNT PIC 9(6) VALUE 0.
      *
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

           IF NOT (FI-DOCP = "06" OR "08" OR "09" OR "10")
               ADD 1 TO LEFT-CNT
               MOVE SPACE TO FILEOUT01
               STRING FI-KEY " LEFT AT 00 " FI-NOTE
                   DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P1
           END-IF

           MOVE FI-KEY TO CHARNEW-KEY
           READ CHARNEW
             INVALID
               MOVE SPACE TO FILEOUT01
               STRING FI-KEY " NOT IN CHARNEW ANY MORE"
                   DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P1
           END-READ

      *    A CODER MAY HAVE PICKED ONE SINCE RRR255 RAN
           IF CD-DOCP NOT = "00"
               MOVE SPACE TO FILEOUT01
               STRING FI-KEY " ALREADY " CD-DOCP ", LEFT ALONE"
                   DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P1
           END-IF

           MOVE SPACE TO BAKFILE01
           MOVE CHARNEW01 TO BAKFILE01
           WRITE BAKFILE01

           MOVE FI-DOCP TO CD-DOCP
           REWRITE CHARNEW01
             INVALID
               MOVE SPACE TO FILEOUT01
               STRING FI-KEY " CANT REWRITE"
                   DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P1
           END-REWRITE

           ADD 1 TO UPD-CNT
           MOVE SPACE TO FILEOUT01
           IF FI-SRC = "OVER"
               ADD 1 TO OVER-CNT
               STRING FI-KEY " DOCP " FI-DOCP " CPT " CD-CPT
                   " DOS " CD-DATE-T
                   " SIGNED BY OTHER THAN FEED, CHECK " FI-NOTE
                   DELIMITED BY SIZE INTO FILEOUT01
           ELSE
               STRING FI-KEY " DOCP " FI-DOCP " CPT " CD-CPT
                   " DOS " CD-DATE-T " " FI-SRC " " FI-NOTE
                   DELIMITED BY SIZE INTO FILEOUT01
           END-IF
           WRITE FILEOUT01
           GO TO P1.

       P4.
           MOVE SPACE TO FILEOUT01
           STRING "RRR256 READ " REC-CNT " SET " UPD-CNT
               " OF THOSE OTHER SIGNER " OVER-CNT " LEFT AT 00 "
               LEFT-CNT
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           DISPLAY FILEOUT01

           CLOSE FILEIN CHARNEW FILEOUT BAKFILE.
           STOP RUN.
