      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. missing-lat-mods.
       AUTHOR. S WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CHARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARFILE-KEY.

           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).

       FD  CHARFILE.
           COPY "charfile.cpy" IN "C:\Users\sid\cms\copylib\rri".

       WORKING-STORAGE SECTION.
      *
       PROCEDURE DIVISION.

       P0.
           OPEN INPUT CHARFILE
           OUTPUT FILEOUT.
           MOVE SPACE TO CHARFILE-KEY.
                 
           START CHARFILE KEY NOT < CHARFILE-KEY INVALID
               GO TO P99
           END-START.

       p1.    

           READ CHARFILE NEXT AT END
               GO TO P99
           END-READ
            
      *    also adding problematic cxrs that come over with laterality
           IF NOT (CD-PROC1 = "7300026" OR "7303026" OR "7314026")
               GO TO P1
           END-IF
                   
           IF CD-MOD2 = SPACE
             STRING "MISSING LAT MOD ON " CD-PROC " FOR " 
                CD-KEY8 " DOS " CD-DATE-T "?"
                DELIMITED BY SIZE INTO FILEOUT01
             WRITE FILEOUT01
           END-IF
          
           GO TO P1.    
           
        P99.
           CLOSE CHARFILE FILEOUT. 
           STOP RUN.
