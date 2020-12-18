      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. hip516.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ISAFILE ASSIGN TO "S25"
           ORGANIZATION LINE SEQUENTIAL.
       
           SELECT FILEIN ASSIGN TO "S30"
           ORGANIZATION LINE SEQUENTIAL.
       
           SELECT FILEOUT ASSIGN TO "S35"
           ORGANIZATION LINE SEQUENTIAL.
           
           SELECT FILEOUT2 ASSIGN TO "S40".
       
           SELECT WEBFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS WEB-KEY
           LOCK MODE MANUAL.

       DATA DIVISION.

       FILE SECTION.

       FD  WEBFILE.
       01  WEBFILE01.
           02 WEB-KEY PIC X(8).
           02 WEB-NUM PIC 9999.

       FD  ISAFILE.
       01  ISAFILE01. 
           02 ISA-0 PIC XXX VALUE "ISA".
           02 ISA-S0 PIC X VALUE "*".
           02 ISA-1 PIC XX.
           02 ISA-S1 PIC X VALUE "*".
           02 ISA-2 PIC X(10).
           02 ISA-S2 PIC X VALUE "*".
           02 ISA-3 PIC XX.
           02 ISA-S3 PIC X VALUE "*".
           02 ISA-4 PIC X(10).
           02 ISA-S4 PIC X VALUE "*".
           02 ISA-5 PIC XX.
           02 ISA-S5 PIC X VALUE "*".
           02 ISA-6 PIC X(15).
           02 ISA-S6 PIC X VALUE "*".
           02 ISA-7 PIC XX.
           02 ISA-S7 PIC X VALUE "*".
           02 ISA-8 PIC X(15).
           02 ISA-S8 PIC X VALUE "*".
           02 ISA-9 PIC X(6).
           02 ISA-S9 PIC X VALUE "*".
           02 ISA-10 PIC X(4).
           02 ISA-S10 PIC X VALUE "*".
           02 ISA-11 PIC X.
           02 ISA-S11 PIC X VALUE "*".
           02 ISA-12 PIC X(5).
           02 ISA-S12 PIC X VALUE "*".
           02 ISA-13 PIC 9(9).
           02 ISA-S13 PIC X VALUE "*".
           02 ISA-14 PIC X.
           02 ISA-S14 PIC X VALUE "*".
           02 ISA-15 PIC X.
           02 ISA-S15 PIC X VALUE "*".
           02 ISA-16 PIC X.
           02 ISA-END PIC X VALUE "~".
       
       FD  FILEIN.
       01  FILEIN01. 
           02 FI-2 PIC XX.
           02 FILLER PIC X(158).
     
       FD  FILEOUT.
       01  FILEOUT01 PIC X(160).

       FD  FILEOUT2.
       01  FILEOUT201 PIC X.
           
       WORKING-STORAGE SECTION.

       01 ALF1 PIC X.
       01 X PIC 999.
       01 Y PIC 999.
       01 Z PIC 999.
       01 HIGH-END PIC 999.
       01 CNTR PIC 999.
       01 INTAB01.
          02 INTAB PIC X OCCURS 160 TIMES.
       01 OUTTAB01.
          02 OUTTAB PIC X OCCURS 160 TIMES.
       01 IEA01.
           02 IEA-0 PIC XXX VALUE "IEA".
           02 IEA-S0 PIC X VALUE "*".
           02 IEA-1 PIC X(9).
           02 IEA-S1 PIC X VALUE "*".
           02 IEA-2 PIC X(9). 
           02 IEA-END PIC X VALUE "~".
       01  ST01.
           02 ST-0 PIC XX VALUE "ST".
           02 ST-S0 PIC X VALUE "*".
           02 ST-1 PIC XXX VALUE "837".
           02 ST-S1 PIC X VALUE "*".
           02 ST-NUM PIC 9(4).
           02 ST-S2 PIC X VALUE "*".
           02 ST-CONVENT-REF PIC X(12).
           02 ST-END PIC X VALUE "~".
       01  SE01.
           02 SE-0 PIC XX VALUE "SE".
           02 SE-S0 PIC X VALUE "*".
           02 SE-CNTR PIC X(9).
           02 SE-S1 PIC X VALUE "*".
           02 SE-NUM PIC 9(4).
           02 SE-END PIC X VALUE "~".
       01 GS01.
           02 GS-0 PIC XX VALUE "GS".
           02 GS-S0 PIC X VALUE "*".
           02 GS-1 PIC XX VALUE "HC".
           02 GS-S1 PIC X VALUE "*".
           02 GS-2 PIC X(9).
           02 GS-S2 PIC X VALUE "*".
           02 GS-3 PIC X(9).
           02 GS-S3 PIC X VALUE "*".
           02 GS-4 PIC X(8).
           02 GS-S4 PIC X VALUE "*".
           02 GS-5 PIC X(4).
           02 GS-S5 PIC X VALUE "*".
           02 GS-NUMX PIC X(9).
           02 GS-S6 PIC X VALUE "*".
           02 GS-7 PIC X VALUE "X".
           02 GS-S7 PIC X VALUE "*".
           02 GS-8 PIC X(12) VALUE "005010222XA1".
           02 GS-S8 PIC X VALUE "*".
           02 GS-END PIC X VALUE "~".
       01  GE01.
           02 GE-0 PIC XX VALUE "GE".
           02 GE-S0 PIC X VALUE "*".
           02 GE-CNTRX PIC X(9).
           02 GE-S1 PIC X VALUE "*".
           02 GE-NUMX PIC X(9).
           02 GE-END PIC X VALUE "~".

       01  SEG-NUM PIC 9(9) VALUE 0.
       01  ALF9 PIC X(9).
       01  ALFS9 PIC X(9).
       01  ALF9Z PIC Z(9).
       01  NUM9 PIC 9(9).
       01  ALF9NUM PIC X(9).
       01  END-FLAG PIC 9 VALUE 0.
       01  TIME-X. 
           02 TIME-HHMM PIC X(4).
           02 FILLER PIC X(4).
       01  IEA-NUM PIC 99 VALUE 0.
       01  GS-NUM PIC 9(9).
       01  GS-NUMX-SAVE PIC X(9).
       01  GE-NUMX-SAVE PIC X(9).
       01  SE-NUM-SAVE PIC 9(4).
       01  ST-NUM4 PIC 9(4).

       PROCEDURE DIVISION.

       P0.
           OPEN INPUT ISAFILE FILEIN 
           I-O WEBFILE 
           OUTPUT FILEOUT FILEOUT2.
           ACCEPT WEB-KEY FROM CENTURY-DATE.
           READ WEBFILE WITH LOCK
             INVALID
               MOVE 1 TO WEB-NUM
               WRITE WEBFILE01
               END-WRITE
      *       NOT INVALID
      *         ADD 1 TO WEB-NUM
      *         REWRITE WEBFILE01
           END-READ
           
           READ ISAFILE AT END GO TO P99.
           
           ACCEPT ISA-9 FROM DATE
           ACCEPT TIME-X FROM TIME
           MOVE TIME-HHMM TO ISA-10
           MOVE WEB-NUM TO ISA-13
           MOVE ISA-13 TO IEA-2
           MOVE SPACE TO OUTTAB01
           MOVE ISAFILE01 TO OUTTAB01
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 106
           WRITE FILEOUT201 FROM OUTTAB(X) 
           END-PERFORM.
           REWRITE WEBFILE01.
           MOVE 0 TO ST-NUM4 GS-NUM.
           
       P1.
            READ FILEIN AT END
            MOVE 1 TO END-FLAG 
            MOVE SPACE TO FILEIN01
            COMPUTE NUM9 = IEA-NUM
            PERFORM NUM-LEFT9
            MOVE ALF9NUM TO IEA-1
            MOVE IEA01 TO FILEIN01
            GO TO P1-1.
           IF NOT (FI-2 = "GS" OR "GE") ADD 1 TO SEG-NUM.
           IF FI-2 = "GS"
             MOVE FILEIN01 TO GS01
             ADD 1 TO GS-NUM
             COMPUTE NUM9 = GS-NUM
             PERFORM NUM-LEFT9
             MOVE ALF9NUM TO GS-NUMX
             MOVE GS01 TO FILEIN01
             MOVE GS-NUMX TO GE-NUMX-SAVE
             MOVE 0 TO ST-NUM4
             GO TO P1-1
           END-IF.
           IF FI-2 = "ST"
             MOVE FILEIN01 TO ST01
             ADD 1 TO ST-NUM4
             MOVE ST-NUM4 TO ST-NUM
             MOVE ST-NUM TO SE-NUM-SAVE
             MOVE ST01 TO FILEIN01
             GO TO P1-1
           END-IF.

           IF FI-2 = "SE"
            MOVE SE-NUM-SAVE TO SE-NUM
            COMPUTE NUM9 = SEG-NUM 
            PERFORM NUM-LEFT9
            MOVE ALF9NUM TO SE-CNTR
            MOVE SPACE TO FILEIN01
            MOVE SE01 TO FILEIN01
            MOVE 0 TO SEG-NUM
            GO TO P1-1
           END-IF.
           IF FI-2 = "GE"
           COMPUTE NUM9 = SE-NUM-SAVE
           PERFORM NUM-LEFT9
           MOVE ALF9NUM TO GE-CNTRX
           MOVE GE-NUMX-SAVE TO GE-NUMX
           MOVE SPACE TO FILEIN01
           MOVE GE01 TO FILEIN01
           ADD 1 TO IEA-NUM.
       P1-1.
           MOVE SPACE TO OUTTAB01 INTAB01
           MOVE FILEIN01 TO INTAB01
           MOVE 161 TO CNTR  HIGH-END
           PERFORM VARYING X FROM 160 BY -1 UNTIL X < 1
           IF INTAB(X) = "~"
           MOVE X TO HIGH-END
           MOVE 0 TO X
           END-IF
           END-PERFORM.
           IF HIGH-END = 161
           DISPLAY FILEIN01
           DISPLAY "NO TILDE"
           ACCEPT ALF1
           GO TO P99.
           
           SUBTRACT 1 FROM HIGH-END GIVING Y
           PERFORM VARYING X FROM Y BY -1 UNTIL X < 2
            IF NOT (INTAB(X) = "*" OR " ")
             MOVE 1 TO X
            ELSE
             MOVE SPACE TO INTAB(HIGH-END)
             MOVE "~" TO INTAB(X)
             SUBTRACT 1 FROM HIGH-END
            END-IF
           END-PERFORM
           
           PERFORM P2 VARYING Y FROM HIGH-END BY -1 UNTIL Y < 1
           MOVE SPACE TO INTAB01
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 160
            IF OUTTAB(X) NOT = SPACE
            MOVE X TO Z
            MOVE 161 TO X
            END-IF
            END-PERFORM.
              MOVE 0 TO CNTR
              PERFORM VARYING Y FROM Z BY 1 UNTIL Y > 160
                ADD 1 TO CNTR
                MOVE OUTTAB(Y) TO INTAB(CNTR)  
                WRITE FILEOUT201 FROM OUTTAB(Y)
              END-PERFORM.
            WRITE FILEOUT01 FROM INTAB01
           IF END-FLAG = 1 GO TO P99.
           GO TO P1.
       P2. IF (INTAB(Y) = "*" OR "~")
           PERFORM P3.
           PERFORM WRITE-IT.
       P3.
           PERFORM WRITE-IT.
           SUBTRACT 1 FROM Y. 
           IF INTAB(Y) = "*" GO TO P3.
           PERFORM VARYING Z FROM Y BY -1 UNTIL Z < 1
           IF INTAB(Z) NOT = SPACE
           MOVE Z TO Y
           MOVE 1 TO Z
           END-IF
           END-PERFORM.
           IF INTAB(Y) = "*" GO TO P3.
       WRITE-IT. 
           SUBTRACT 1 FROM CNTR
           MOVE INTAB(Y) TO OUTTAB(CNTR).
       NUM-LEFT9.
           MOVE NUM9 TO ALF9Z
           MOVE SPACE TO ALF9NUM
           MOVE ALF9Z TO ALF9
           UNSTRING ALF9 DELIMITED ALL " " INTO ALFS9 ALF9NUM.
       
       P99.

           CLOSE FILEOUT FILEOUT2 WEBFILE.
           STOP RUN.
