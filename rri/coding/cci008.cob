      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2026 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
      *
      *    REMOVE DUPLICATE OUTSIDE READS FROM CHARFILE.
      *    ONLY OUTSIDE READS (PLACE OF SERVICE A,B,C,M,N,P,R,S) ARE
      *    LOOKED AT. ONE IS DELETED WHEN THE SAME PATIENT ALREADY HAS
      *    AN OUTSIDE READ WITH THE SAME DOS, CPT AND MOD2, EITHER
      *    POSTED (CHARCUR) OR EARLIER IN CHARFILE. CHARGES WITH ANY
      *    OTHER POS ARE LEFT ALONE. RUN BEFORE CCI005 SO THESE DO NOT
      *    GET A 76/77.
      *    EVERY DELETED RECORD IS WRITTEN WHOLE TO DELFILE (S45)
      *    SO IT CAN BE PUT BACK.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cci008.
       AUTHOR. s WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARCUR ASSIGN TO  "S25" ORGANIZATION IS INDEXED
               ACCESS IS DYNAMIC RECORD KEY IS CHARCUR-KEY
               ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
               LOCK MODE MANUAL.

           SELECT CHARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC RECORD KEY IS CHARFILE-KEY
               LOCK MODE MANUAL.

           SELECT FILEOUT ASSIGN TO  "S40" ORGANIZATION
               LINE SEQUENTIAL.

           SELECT DELFILE ASSIGN TO  "S45" ORGANIZATION
               LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  CHARCUR.
           COPY charcur.CPY.

       FD  FILEOUT.
       01  FILEOUT01 PIC X(120).

       FD  DELFILE.
       01  DELFILE01 PIC X(200).

       FD  CHARFILE.
           COPY charfile.CPY.

       WORKING-STORAGE SECTION.

      *    OUTSIDE READS KEPT SO FAR FOR THE CURRENT PATIENT
       01  KEEP-TAB01.
           02 KEEP-TAB OCCURS 90 TIMES.
             03 KEEP-DATE PIC X(8).
             03 KEEP-CPT  PIC X(5).
             03 KEEP-MOD2 PIC XX.
             03 KEEP-KEY  PIC X(11).

       01  X PIC 999.
       01  A PIC 999.
       01  FLAG PIC 9.
       01  HOLD-IT PIC X(8).
       01  HOLD-KEY PIC X(11).
       01  DEL-CNT PIC 9(6) VALUE 0.
       01  REC-CNT PIC 9(6) VALUE 0.
      *
       PROCEDURE DIVISION.
       P0.
           OPEN I-O CHARFILE
           OPEN INPUT CHARCUR
           OPEN OUTPUT FILEOUT DELFILE
           MOVE SPACE TO CHARFILE-KEY HOLD-IT
           MOVE 0 TO X.

       P0-1.
           START CHARFILE KEY NOT < CHARFILE-KEY
             INVALID
               GO TO P4
           END-START.

       P1.
           READ CHARFILE NEXT
             AT END
               GO TO P4
           END-READ

           ADD 1 TO REC-CNT

           IF CD-KEY8 NOT = HOLD-IT
               MOVE CD-KEY8 TO HOLD-IT
               MOVE 0 TO X
           END-IF

           IF CD-AMOUNT = 0
               GO TO P1
           END-IF

           IF NOT (CD-PLACE = "A" OR "B" OR "C" OR "M" OR "N" OR "P"
                   OR "R" OR "S")
               GO TO P1
           END-IF

      *    DUPE OF A CHARGE ALREADY KEPT IN CHARFILE?
           MOVE 0 TO FLAG
           PERFORM P10 THRU P10-EXIT VARYING A FROM 1 BY 1
               UNTIL A > X OR FLAG = 1

           IF FLAG = 1
               MOVE SPACE TO FILEOUT01
               STRING CHARFILE-KEY " DUPE OF " KEEP-KEY(A - 1)
                   " DOS " CD-DATE-T " CPT " CD-CPT " MOD2 " CD-MOD2
                   " POS " CD-PLACE " DELETED"
                   DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P2
           END-IF

      *    DUPE OF A POSTED CHARGE?
           PERFORM P20 THRU P20-EXIT

           IF FLAG = 1
               MOVE SPACE TO FILEOUT01
               STRING CHARFILE-KEY " DUPE OF POSTED " CHARCUR-KEY
                   " DOS " CD-DATE-T " CPT " CD-CPT " MOD2 " CD-MOD2
                   " POS " CD-PLACE " DELETED"
                   DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P2
           END-IF

           GO TO P3.

       P2.
           MOVE CHARFILE-KEY TO HOLD-KEY
           MOVE SPACE TO DELFILE01
           MOVE CHARFILE01 TO DELFILE01
           WRITE DELFILE01

           DELETE CHARFILE RECORD
             INVALID
               DISPLAY HOLD-KEY " CANT DELETE"
             NOT INVALID
               ADD 1 TO DEL-CNT
           END-DELETE

      *    REPOSITION PAST THE DELETED RECORD
           MOVE HOLD-KEY TO CHARFILE-KEY
           START CHARFILE KEY > CHARFILE-KEY
             INVALID
               GO TO P4
           END-START
           GO TO P1.

      *    KEEP IT AND REMEMBER IT FOR LATER CHARGES
       P3.
           IF X > 89
               MOVE SPACE TO FILEOUT01
               STRING CHARFILE-KEY " TOO MANY CHARGES, NOT CHECKED"
                   DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
               GO TO P1
           END-IF

           ADD 1 TO X
           MOVE CD-DATE-T    TO KEEP-DATE(X)
           MOVE CD-CPT       TO KEEP-CPT(X)
           MOVE CD-MOD2      TO KEEP-MOD2(X)
           MOVE CHARFILE-KEY TO KEEP-KEY(X)
           GO TO P1.

       P10.
           IF KEEP-DATE(A) = CD-DATE-T
               AND KEEP-CPT(A) = CD-CPT
               AND KEEP-MOD2(A) = CD-MOD2
               MOVE 1 TO FLAG
           END-IF.

       P10-EXIT.
           EXIT.

       P20.
           MOVE 0 TO FLAG
           MOVE CD-KEY8 TO CC-KEY8
           MOVE SPACE   TO CC-KEY3
           START CHARCUR KEY NOT < CHARCUR-KEY
             INVALID
               GO TO P20-EXIT
           END-START.

       P20-1.
           READ CHARCUR NEXT
             AT END
               GO TO P20-EXIT
           END-READ

           IF CC-KEY8 NOT = CD-KEY8
               GO TO P20-EXIT
           END-IF

           IF CC-DATE-T = CD-DATE-T
               AND CC-CPT = CD-CPT
               AND CC-MOD2 = CD-MOD2
               AND (CC-PLACE = "A" OR "B" OR "C" OR "M" OR "N" OR "P"
                    OR "R" OR "S")
               MOVE 1 TO FLAG
               GO TO P20-EXIT
           END-IF

           GO TO P20-1.

       P20-EXIT.
           EXIT.

       P4.
           MOVE SPACE TO FILEOUT01
           STRING "CCI008 READ " REC-CNT " DELETED " DEL-CNT
               DELIMITED BY SIZE INTO FILEOUT01
           WRITE FILEOUT01

           CLOSE CHARFILE CHARCUR FILEOUT DELFILE.
           STOP RUN.
