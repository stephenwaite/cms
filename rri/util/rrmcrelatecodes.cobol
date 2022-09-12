      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rrmcrelatecodes.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           
           SELECT FILEIN ASSIGN TO        "S60" ORGANIZATION 
               LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO       "S65" ORGANIZATION 
               LINE SEQUENTIAL.

           
            
       DATA DIVISION.
       FILE SECTION.
       
       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).

       FD  FILEIN.
       01  FILEIN01.
           02 FI-1 PIC XX.
           02 FI-2 PIC X(1068).

       01  ans pic x.    

       
       WORKING-STORAGE SECTION.

           copy rec101.cpy in "c:\users\sid\cms\copylib\rri\ws".
           copy rec201.cpy in "c:\users\sid\cms\copylib\rri\ws".
           copy rec301.cpy in "c:\users\sid\cms\copylib\rri\ws".


       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT FILEIN.
           OPEN OUTPUT FILEOUT.

       P1.
           READ FILEIN AT END
               GO TO 9100CMF
           END-READ.  

       P1-1.
           IF FI-1 NOT = "##"
               GO TO P1
           END-IF

           MOVE FILEIN01 TO REC101.
           if not (r1-relate1 = "  " or "01" or "18" or "19" or "23")
             move space to fileout01
             write fileout01 from r1-relate1
             display R1-RELATE1 " R1-RELATE1"
             accept ans.

           go to p1. 

       9100CMF.
           CLOSE FILEOUT FILEIN.
           STOP RUN.
