      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. mods-02-check-chcrr.
       AUTHOR. s WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARFILE-KEY.

           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT INSFILE ASSIGN TO "S40" ORGANIZATION INDEXED
               ACCESS IS DYNAMIC  RECORD KEY IS INS-KEY
               ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
               ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES.

       DATA DIVISION.

       FILE SECTION.
       
       FD  CHARFILE.
           copy CHARFILE.cpy in "c:\users\sid\cms\copylib\rri".

       FD  INSFILE.
           copy insfile.cpy in "c:\users\sid\cms\copylib".    

       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).   

       PROCEDURE DIVISION.

       P0. 
           OPEN INPUT INSFILE
           OPEN INPUT CHARFILE
           OPEN OUTPUT FILEOUT
           MOVE SPACE TO CHARFILE-KEY
           START CHARFILE KEY NOT < CHARFILE-KEY
             INVALID
               DISPLAY "EMPTY FILE"
               GO TO P2
           END-START.    

       P1. 
           READ CHARFILE NEXT
             AT END
               GO TO P2
           END-READ    
           
           MOVE CD-PAYCODE TO INS-KEY.
           READ INSFILE
             INVALID 
               DISPLAY "BAD INS " CHARFILE01.

           IF  NOT (CD-PAYCODE = "002" OR "074" OR "268")
               GO TO P1.

           IF (CD-PROC1(1:5) = "50200" OR "73000" OR "73030" OR "73110" 
             OR "73120" OR "73140" OR "73201" OR "73273" OR "73564" 
             OR "73600" OR "73630" OR "73660" OR "76641" OR "76642")
             AND CD-MOD2 = SPACE
             AND CD-MOD3 = SPACE
             AND CD-MOD4 = SPACE
               MOVE SPACE TO FILEOUT01
               STRING "CHECK MODS FOR " CD-NAME " " CD-DATE-T " " 
                 CD-PROC DELIMITED BY SIZE INTO FILEOUT01
               WRITE FILEOUT01
           END-IF

           GO TO P1.
       P2. 
           CLOSE CHARFILE FILEOUT INSFILE.
           STOP RUN.
