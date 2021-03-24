      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. hip001.
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
           
           SELECT HIPCLAIMFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
             ACCESS MODE IS DYNAMIC RECORD KEY IS HIP-KEY
             LOCK MODE MANUAL.

       DATA DIVISION.

       FILE SECTION.

       FD  HIPCLAIMFILE.
           copy hipclaimfile.cpy in "c:\users\sid\cms\copylib".

       FD  ISAFILE.
           copy isafile.cpy in "c:\users\sid\cms\copylib".
             
       FD  FILEIN.
       01  FILEIN01. 
           02 FI-2 PIC XX.
           02 FILLER PIC X(158).
     
       FD  FILEOUT.
       01  FILEOUT01 PIC X(160).

       FD  FILEOUT2.
       01  FILEOUT201 PIC X.
           
       WORKING-STORAGE SECTION.
       01  ALF1 PIC X.
       01  X PIC 999.
       01  Y PIC 999.
       01  Z PIC 999.
       01  HIGH-END PIC 999.
       01  CNTR PIC 999.

       01  INTAB01.
           02 INTAB PIC X OCCURS 160 TIMES.

       01  OUTTAB01.
           02 OUTTAB PIC X OCCURS 160 TIMES.

       01  IEA01.
           02 IEA-0 PIC XXX VALUE "IEA".
           02 IEA-S0 PIC X VALUE "*".
           02 IEA-1 PIC XX.
           02 IEA-S1 PIC X VALUE "*".
           02 IEA-2 PIC X(9). 
           02 IEA-END PIC X VALUE "~".

       01  ST01.
           02 ST-0 PIC XX VALUE "ST".
           02 ST-S0 PIC X VALUE "*".
           02 ST-1 PIC XXX VALUE "837".
           02 ST-S1 PIC X VALUE "*".
           02 ST-NUM PIC X(9).
           02 ST-END PIC X VALUE "~".

       01  SE01.
           02 SE-0 PIC XX VALUE "SE".
           02 SE-S0 PIC X VALUE "*".
           02 SE-CNTR PIC X(9).
           02 SE-S1 PIC X VALUE "*".
           02 SE-NUM PIC X(9).
           02 SE-END PIC X VALUE "~".

       01  SEG-NUM PIC 9(9) VALUE 0.
       01  ALF9 PIC X(9).
       01  ALFS9 PIC X(9).
       01  ALF9Z PIC Z(9).
       01  NUM9 PIC 9(9).
       01  ALF9NUM PIC X(9).
       01  SAVE-NUM PIC X(9).
       01  END-FLAG PIC 9 VALUE 0.

       01  TIME-X. 
           02 TIME-HHMM PIC X(4).
           02 FILLER PIC X(4).

       01 IEA-NUM PIC 99 VALUE 0.

       PROCEDURE DIVISION.

       P0.
           OPEN INPUT ISAFILE FILEIN 
           OPEN I-O HIPCLAIMFILE 
           OPEN OUTPUT FILEOUT FILEOUT2.

           MOVE "A" TO HIP-KEY
           READ HIPCLAIMFILE WITH LOCK 
             INVALID 
               DISPLAY " BAD HIP-CLAIM"
               ACCEPT ALF9
               GO TO P99.

           READ ISAFILE AT END GO TO P99.

           ACCEPT ISA-9 FROM DATE
           ACCEPT TIME-X FROM TIME
           MOVE TIME-HHMM TO ISA-10
           ADD 1 TO HIP-NUM
           MOVE HIP-NUM TO ISA-13
           MOVE ISA-13 TO IEA-2
           MOVE SPACE TO OUTTAB01
           MOVE ISAFILE01 TO OUTTAB01
      
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 106
             WRITE FILEOUT201 FROM OUTTAB(X) 
             END-PERFORM.
      
           REWRITE HIPCLAIMFILE01.

       P1. 
           READ FILEIN 
             AT END 
               MOVE 1 TO END-FLAG 
               COMPUTE NUM9 = IEA-NUM
               PERFORM NUM-LEFT9
               MOVE ALF9NUM TO IEA-1
               MOVE SPACE TO FILEIN01
               MOVE IEA01 TO FILEIN01
               GO TO P1-1.

           IF NOT (FI-2 = "GS" OR "GE") ADD 1 TO SEG-NUM.

           IF FI-2 = "ST" 
             MOVE FILEIN01 TO ST01
             MOVE ST-NUM TO SAVE-NUM.
           
           IF FI-2 = "SE" 
             MOVE FILEIN01 TO SE01
             MOVE SPACE TO SE-NUM
             MOVE SAVE-NUM TO SE-NUM
             COMPUTE NUM9 = SEG-NUM 
             PERFORM NUM-LEFT9
             MOVE ALF9NUM TO SE-CNTR
             MOVE SPACE TO FILEIN01
             MOVE SE01 TO FILEIN01
             MOVE 0 TO SEG-NUM GO TO P1-1.
           
           IF FI-2 = "GE" 
             ADD 1 TO IEA-NUM.

       P1-1.
           MOVE SPACE TO OUTTAB01 INTAB01
           MOVE FILEIN01 TO INTAB01
           MOVE 161 TO CNTR HIGH-END

           PERFORM VARYING X FROM 160 BY -1 UNTIL X < 1          
             IF INTAB(X) = "~"
               MOVE X TO HIGH-END
               MOVE 0 TO X
             END-IF
           END-PERFORM

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
           END-PERFORM

           MOVE 0 TO CNTR

           PERFORM VARYING Y FROM Z BY 1 UNTIL Y > 160
             ADD 1 TO CNTR
             MOVE OUTTAB(Y) TO INTAB(CNTR)  
             WRITE FILEOUT201 FROM OUTTAB(Y)
           END-PERFORM  
            
           WRITE FILEOUT01 FROM INTAB01
           
           IF END-FLAG = 1 GO TO P99.
           
           GO TO P1.

       P2. 
           IF (INTAB(Y) = "*" OR "~")
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
           CLOSE FILEOUT FILEOUT2 HIPCLAIMFILE.
           STOP RUN.
