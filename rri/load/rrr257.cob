      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2026 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
      *
      *    PUT RRMC006'S SUBSCRIBER FLAGS (RRMC SENT THE SUBSCRIBER AS
      *    SELF BUT THE RELATIONSHIP OR DOB SAYS OTHERWISE) INTO
      *    CMNTFILE SO THEY SHOW ON THE ACCOUNT IN INAR004.
      *    RUN AFTER RRR250 SO NEW ACCOUNTS HAVE A GARNO.
      *    INPUT (S30) IS ONE LINE PER FLAG:
      *       1-8  MEDREC (A-ACTNO)
      *       9-88 COMMENT
      *    A FLAG WHOSE ACCOUNT HAS NO GARNO YET GOES TO CARRY (S45)
      *    FOR THE NEXT RUN. ONE ALREADY ON THE ACCOUNT IS SKIPPED, RRMC
      *    RESENDS VISITS.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrr257.
       AUTHOR. s WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILEIN ASSIGN TO "S30" ORGANIZATION
               LINE SEQUENTIAL.

           SELECT ACTFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS A-ACTNO
               ALTERNATE RECORD KEY IS A-GARNO WITH DUPLICATES
               ALTERNATE RECORD KEY IS NAME-KEY WITH DUPLICATES
               LOCK MODE MANUAL.

           SELECT CMNTFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS CMNT-KEY
               LOCK MODE MANUAL.

           SELECT CARRY ASSIGN TO "S45" ORGANIZATION
               LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S50" ORGANIZATION
               LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  FILEIN.
       01  FILEIN01.
           02 FI-MEDREC PIC X(8).
           02 FI-CMNT PIC X(80).

       FD  ACTFILE.
           COPY actfile.CPY.

       FD  CMNTFILE.
       01  CMNTFILE01.
           02 CMNT-KEY.
             03 CM-KEY8 PIC X(8).
             03 CM-KEY3 PIC XXX.
           02 CMNT PIC X(80).
           02 CMNT-DATE-E PIC X(8).

       FD  CARRY.
       01  CARRY01 PIC X(88).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(120).

       WORKING-STORAGE SECTION.

       01  CM-NO PIC 999.
       01  DUP-FLAG PIC 9.
       01  DATE-TODAY PIC X(8).
       01  REC-CNT PIC 9(6) VALUE 0.
       01  ADD-CNT PIC 9(6) VALUE 0.
       01  DUP-CNT PIC 9(6) VALUE 0.
       01  CARRY-CNT PIC 9(6) VALUE 0.
       01  BAD-CNT PIC 9(6) VALUE 0.
      *
       PROCEDURE DIVISION.
       P0.
           ACCEPT DATE-TODAY FROM DATE YYYYMMDD
           OPEN INPUT FILEIN ACTFILE
           OPEN I-O CMNTFILE
           OPEN OUTPUT CARRY FILEOUT.

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

           MOVE FI-MEDREC TO A-ACTNO
           READ ACTFILE
             INVALID
               MOVE SPACE TO A-GARNO
           END-READ

           IF A-GARNO = SPACE
               WRITE CARRY01 FROM FILEIN01
               ADD 1 TO CARRY-CNT
               MOVE SPACE TO FILEOUT01
               STRING FI-MEDREC " NO GARNO YET, CARRIED"
                   DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P1
           END-IF

           MOVE 0 TO DUP-FLAG
           MOVE A-GARNO TO CM-KEY8
           MOVE "000" TO CM-KEY3
           START CMNTFILE KEY > CMNT-KEY
             INVALID
               GO TO P2
           END-START.

       P1-1.
           READ CMNTFILE NEXT
             AT END
               GO TO P2
           END-READ

           IF CM-KEY8 NOT = A-GARNO
               GO TO P2
           END-IF

           IF CMNT = FI-CMNT
               MOVE 1 TO DUP-FLAG
               GO TO P2
           END-IF

           GO TO P1-1.

       P2.
           IF DUP-FLAG = 1
               ADD 1 TO DUP-CNT
               MOVE SPACE TO FILEOUT01
               STRING A-GARNO " ALREADY HAS IT"
                   DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P1
           END-IF

      *    NEXT FREE KEY THE WAY TRI000 PICKS IT
           MOVE 0 TO CM-NO.

       P2-1.
           ADD 1 TO CM-NO

           IF CM-NO > 990
               ADD 1 TO BAD-CNT
               MOVE SPACE TO FILEOUT01
               STRING A-GARNO " TOO MANY COMMENTS ALREADY"
                   DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P1
           END-IF

           MOVE A-GARNO TO CM-KEY8
           MOVE CM-NO TO CM-KEY3
           READ CMNTFILE
             INVALID
               GO TO P3
           END-READ

           GO TO P2-1.

       P3.
           MOVE SPACE TO CMNTFILE01
           MOVE A-GARNO TO CM-KEY8
           MOVE CM-NO TO CM-KEY3
           MOVE FI-CMNT TO CMNT
           MOVE DATE-TODAY TO CMNT-DATE-E
           WRITE CMNTFILE01
             INVALID
               ADD 1 TO BAD-CNT
               MOVE SPACE TO FILEOUT01
               STRING CMNT-KEY " CANT WRITE"
                   DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P1
           END-WRITE

           ADD 1 TO ADD-CNT
           MOVE SPACE TO FILEOUT01
           STRING CMNT-KEY " ADDED " FI-CMNT
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           GO TO P1.

       P4.
           MOVE SPACE TO FILEOUT01
           STRING "RRR257 READ " REC-CNT " ADDED " ADD-CNT
               " ALREADY THERE " DUP-CNT " NO GARNO YET " CARRY-CNT
               " ERRORS " BAD-CNT
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01
           DISPLAY FILEOUT01

           CLOSE FILEIN ACTFILE CMNTFILE CARRY FILEOUT.
           STOP RUN.
