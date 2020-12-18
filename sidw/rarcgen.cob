      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. rarcgen.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT file1 assign to "S25" LINE SEQUENTIAL.

           SELECT rarcfile ASSIGN TO "S30" ORGANIZATION
           indexed access mode dynamic record key is rarc-key.

           SELECT fileout assign to "S35" line sequential.

       DATA DIVISION.
       
       FILE SECTION.    

       FD  FILE1.
       01  FILE101 PIC X(120).
      
       FD  rarcfile.
       01  rarcfile01.
           02 rarc-key pic x(8).
           02 rarc-reason pic x(112). 

       FD  fileout.
       01  fileout01 pic x(120).       
      
       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.

       P0.
           OPEN output rarcfile fileout.

           close rarcfile.

           open i-o rarcfile.

           open input file1.

       p1.
           move space to file101
           read file1 
             at end
               go to p99
           end-read
                      
           move space to rarcfile01
           move file101 to rarcfile01
           write rarcfile01
             invalid
               move space to fileout01
               string "file1 " file101 delimited by size into fileout01
               write fileout01
               end-write
           end-write

           go to p1.
       
       p99.
           close rarcfile file1 FILEOUT.          

           STOP RUN.       

       
