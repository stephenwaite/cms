      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. chc002.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEIN ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FILEOUT.
       01  FILEOUT01.
           02 FO-PATNAMEL PIC X(24).
           02 FO-PATNAMEF PIC X(24).
           02 FO-PAT-STR1 PIC X(22).
           02 FO-PAT-STR2 PIC X(22).
           02 FO-PAT-CITY PIC X(18).
           02 FO-PAT-STATE PIC XX.
           02 FO-PAT-ZIP PIC X(9).
           02 FO-PAT-DOB PIC X(10).
           02 FO-PAT-SEX PIC X.
           02 FO-PRIM-ALFA PIC X(10).
           02 FO-PRIM-NAME PIC X(30).
           02 FO-PRIM-STR1 PIC X(22).
           02 FO-PRIM-CITY PIC X(18).
           02 FO-PRIM-STATE PIC XX.
           02 FO-PRIM-ZIP PIC X(10).
           02 FO-PRIM-POL PIC X(16).
           02 FO-PRIM-DAT1 PIC X(10).
           02 FO-PRIM-NAMEL PIC X(24).
           02 FO-PRIM-NAMEF PIC X(24).
           02 FO-PRIM-SUBSEX PIC X(10).
           02 FO-PRIM-SUBRELATE PIC XXXX.
           02 FO-SEC-ALFA PIC X(10).
           02 FO-SEC-NAME PIC X(30).
           02 FO-SEC-STR1 PIC X(22).
           02 FO-SEC-CITY PIC X(18).
           02 FO-SEC-STATE PIC XX.
           02 FO-SEC-ZIP PIC X(10).
           02 FO-SEC-DAT1 PIC X(8).
           02 FO-SEC-POL PIC X(16).
           02 FO-SEC-NAMEL PIC X(24).
           02 FO-SEC-NAMEF PIC X(24).
           02 FO-SEC-SUBSEX PIC X(10).
           02 FO-SEC-SUBRELATE PIC XXXX.
           02 FO-PROC PIC X(7).
           02 FO-DX1 PIC X(8).
           02 FO-DX2 PIC X(8).
           02 FO-DX3 PIC X(8).
           02 FO-DX4 PIC X(8).
           02 FO-DATE-T PIC X(10).
           02 FO-PROVNPI PIC X(10).
           02 FO-DAT1 PIC X(8).
           02 FO-3RD-ALFA PIC X(10).
           02 FO-3RD-POL PIC X(16).
           02 FO-3RD-NAME PIC X(30).
           02 FO-3RD-CITY PIC X(18).
           02 FO-REND-PROV PIC XX.
           02 FO-POS PIC X.
       FD  FILEIN
           RECORD CONTAINS 1 TO 999 CHARACTERS.
       01  FILEIN01 PIC X(999).
       WORKING-STORAGE SECTION.
       01  TAB-X PIC X VALUE H"09".
       01  ALF9 PIC X(9).

       PROCEDURE DIVISION.
       0005-S0TART.
           OPEN INPUT FILEIN OUTPUT FILEOUT.
           READ FILEIN 
             AT END
              CONTINUE.

       P1. 
           MOVE SPACE TO FILEIN01
           READ FILEIN AT END GO TO P2.
           MOVE SPACE TO FILEOUT01
           UNSTRING FILEIN01 DELIMITED BY TAB-X INTO
             FO-PATNAMEL
             FO-PATNAMEF 
             FO-PAT-STR1 
             FO-PAT-STR2 
             FO-PAT-CITY 
             FO-PAT-STATE 
             FO-PAT-ZIP 
             FO-PAT-DOB 
             FO-PAT-SEX 
             FO-PRIM-ALFA 
             FO-PRIM-NAME 
             FO-PRIM-STR1 
             FO-PRIM-CITY 
             FO-PRIM-STATE 
             FO-PRIM-ZIP 
             FO-PRIM-POL 
             FO-PRIM-DAT1 
             FO-PRIM-NAMEL 
             FO-PRIM-NAMEF 
             FO-PRIM-SUBSEX 
             FO-PRIM-SUBRELATE 
             FO-SEC-ALFA   
             FO-SEC-NAME   
             FO-SEC-STR1   
             FO-SEC-CITY   
             FO-SEC-STATE   
             FO-SEC-ZIP    
             FO-SEC-DAT1     
             FO-SEC-POL     
             FO-SEC-NAMEL   
             FO-SEC-NAMEF  
             FO-SEC-SUBSEX 
             FO-SEC-SUBRELATE
             FO-PROC     
             FO-DX1    
             FO-DX2    
             FO-DX3     
             FO-DX4    
             FO-DATE-T     
             FO-PROVNPI     
             FO-DAT1     
             FO-3RD-ALFA 
             FO-3RD-POL  
             FO-3RD-NAME
             FO-3RD-CITY
             FO-REND-PROV
             FO-POS.
      *     DISPLAY "FO-PRIM-DAT1 " FO-PRIM-DAT1  
      *     ACCEPT OMITTED

           MOVE SPACE TO ALF9
           STRING FO-PRIM-ZIP(1:5) FO-PRIM-ZIP(7:4) INTO ALF9
           MOVE SPACE TO FO-PRIM-ZIP
           MOVE ALF9 TO FO-PRIM-ZIP
           MOVE SPACE TO ALF9
           STRING FO-SEC-ZIP(1:5) FO-SEC-ZIP(7:4) INTO ALF9
           MOVE SPACE TO FO-SEC-ZIP
           MOVE ALF9 TO FO-SEC-ZIP

           WRITE FILEOUT01
           GO TO P1.
       P2.
           CLOSE FILEOUT.
           STOP RUN.

