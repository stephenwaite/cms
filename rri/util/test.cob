      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. test.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       working-storage section.
       01  G-DOB PIC X(8).
       01  WS-COMPARE-DATE-1.
           03  WS-COMP-1-CC                 PIC 9(02).
           03  WS-COMP-1-YY                 PIC 9(02).
           03  WS-COMP-1-MM                 PIC 9(02).
           03  WS-COMP-2-DD                 PIC 9(02). 
       

       PROCEDURE DIVISION.
       P0.
           MOVE "19440501" TO G-DOB
           STRING G-DOB DELIMITED BY SIZE INTO WS-COMPARE-DATE-1.

           IF WS-COMPARE-DATE-1 < 19590101
             DISPLAY "THAT'S OLD"
             ACCEPT OMITTED.
           
           STOP RUN.
