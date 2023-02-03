      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. chc004.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILEOUT2 ASSIGN TO "S25"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FILEIN ASSIGN TO "S30"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT FILEOUT ASSIGN TO "S35"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT RPGINSFILE ASSIGN TO "S40" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS RPGINS-KEY
           ALTERNATE RECORD KEY IS RPGINS-TITLE WITH DUPLICATES
           ALTERNATE RECORD KEY IS RPGINS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS RPGINS-STATE WITH DUPLICATES
           ALTERNATE RECORD KEY IS RPGINS-CMS WITH DUPLICATES
           ALTERNATE RECORD KEY IS RPGINS-GAP WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT NPIFILE ASSIGN TO "S45" ORGANIZATION IS INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS NPI-KEY
             ALTERNATE RECORD KEY IS NPI-NAME WITH DUPLICATES
             ALTERNATE RECORD KEY IS NPI-REFKEY WITH DUPLICATES
             LOCK MODE MANUAL.
           SELECT RPGPROCFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS RPGPROC-KEY
           LOCK MODE MANUAL.
       DATA DIVISION.
       FILE SECTION.
       FD  RPGPROCFILE.
       01  RPGPROCFILE01.
           02 RPGPROC-KEY PIC X(11).
           02 RPGPROC-TYPE PIC X.
           02 RPGPROC-TITLE PIC X(28).
           02 RPGPROC-AMOUNT PIC 9(4)V99.
       FD  NPIFILE.
       01  NPIFILE01.
           02 NPI-KEY PIC X(10).
           02 NPI-NAME PIC X(24).
           02 NPI-REFKEY PIC X(3).
           02 NPI-PLACE PIC X.

       FD  FILEOUT.
       01  FILEOUT01 PIC X(100).
       FD  FILEOUT2.
       01  FILEOUT201 PIC X(800).
      
       FD  RPGINSFILE.
       01  RPGINSFILE01.   
           02 RPGINS-KEY PIC X(12).
           02 RPGINS-TITLE PIC X(40).
           02 RPGINS-BOX PIC X(40).
           02 RPGINS-STREET PIC X(40).
           02 RPGINS-CITY PIC X(20).
           02 RPGINS-STATE PIC XX.
           02 RPGINS-ZIP PIC X(9).
           02 RPGINS-PHONE PIC X(10).
           02 RPGINS-CMS PIC XXX.
           02 RPGINS-GAP PIC X(7).
           02 RPGINS-FUTURE PIC X.
       FD  FILEIN.
       01  FILEIN01.
           02 FI-PATNAMEL PIC X(24).
           02 FI-PATNAMEF PIC X(24).
           02 FI-PAT-STR1 PIC X(22).
           02 FI-PAT-STR2 PIC X(22).
           02 FI-PAT-CITY PIC X(18).
           02 FI-PAT-STATE PIC XX.
           02 FI-PAT-ZIP PIC X(9).
           02 FI-PAT-DOB PIC X(10).
           02 FI-PAT-SEX PIC X.
           02 PRIME-INS.
             03 FI-PRIM-ALFA PIC X(10).
             03 FI-PRIM-NAME PIC X(30).
             03 FI-PRIM-STR1 PIC X(22).
             03 FI-PRIM-CITY PIC X(18).
             03 FI-PRIM-STATE PIC XX.
             03 FI-PRIM-ZIP PIC X(10).
             03 FI-PRIM-GRP PIC X(10).
             03 FI-PRIM-POL PIC X(16).
             03 FI-PRIM-NAMEL PIC X(24).
             03 FI-PRIM-NAMEF PIC X(24).
             03 FI-PRIM-SUBSEX PIC X(10).
             03 FI-PRIM-SUBRELATE PIC XXXX.
           02 SECOND-INS. 
             03 FI-SEC-ALFA PIC X(10).
             03 FI-SEC-NAME PIC X(30).
             03 FI-SEC-STR1 PIC X(22).
             03 FI-SEC-CITY PIC X(18).
             03 FI-SEC-STATE PIC XX.
             03 FI-SEC-ZIP PIC X(10).
             03 FI-SEC-GRP PIC X(10).
             03 FI-SEC-POL PIC X(16).
             03 FI-SEC-NAMEL PIC X(24).
             03 FI-SEC-NAMEF PIC X(24).
             03 FI-SEC-SUBSEX PIC X(10).
             03 FI-SEC-SUBRELATE PIC XXXX.
           02 FI-PROC.
               03 FI-PROC1 PIC X(5).
               03 FI-PROC2 PIC XX.
           02 FI-DX1 PIC X(8).
           02 FI-DX2 PIC X(8).
           02 FI-DX3 PIC X(8).
           02 FI-DX4 PIC X(8).
           02 FI-DATE-T PIC X(10).
           02 FI-PROVNPI PIC X(10).
           02 FI-DAT1 PIC X(10).
           02 THIRD-INS.
             03 FI-3RD-ALFA PIC X(10).
             03 FI-3RD-POL PIC X(16).
             03 FI-3RD-NAME PIC X(30).
             03 FI-3RD-CITY PIC X(18).

       WORKING-STORAGE SECTION.
       01  TABX01.
           02 TABX OCCURS 4 TIMES.
             03 T-CODE PIC X(10).
             03 T-NAME PIC X(30).
             03 T-STR1 PIC X(22).
             03 T-CITY PIC X(18).
             03 T-STATE PIC XX.
             03 T-ZIP PIC X(9).
             03 T-GRP PIC X(10).
             03 T-POL PIC X(16).
             03 T-NAMEL PIC X(24).
             03 T-NAMEF PIC X(24).
             03 T-SUBSEX PIC X(10).
             03 T-SUBRELATE PIC XXXX.
       01  X PIC 9.
       01  Y PIC 9.
       01  Z PIC 9.
       01  ALF1 PIC X.
       01  ALF1X PIC X.
       01  ALF9X.
           02 A1 PIC X.
           02 A2 PIC X.
           02 A3 PIC X.
           02 A4 PIC X.
           02 A5 PIC X.
           02 A6 PIC X.
           02 A7 PIC X.
           02 A8 PIC X.
           02 A9 PIC X.

       01  ALF5X PIC X(5).    
       01  ALF10X PIC X(10).

       PROCEDURE DIVISION.

       0005-START.
           OPEN INPUT FILEIN RPGINSFILE NPIFILE RPGPROCFILE.
           OPEN OUTPUT FILEOUT FILEOUT2.
       P1.
           READ FILEIN AT END GO TO P99.
           IF FI-PROC1 = "73620" MOVE "73630" TO FI-PROC1.
           IF FI-PROC1 = "73070" MOVE "73060" TO FI-PROC1.
           IF FI-PROC1 = "72072" MOVE "72070" TO FI-PROC1.
           MOVE SPACE TO ALF1 ALF1X
           UNSTRING FI-PRIM-POL DELIMITED BY "+" INTO ALF1 ALF1X
           IF ALF1X NOT = SPACE
             MOVE SPACE TO FILEOUT01
             STRING FI-PATNAMEL " " FI-PATNAMEF " " FI-PRIM-POL
                  " + IN PRI POLICY NUMBER"
             INTO FILEOUT01
             WRITE FILEOUT01
           END-IF
           MOVE SPACE TO ALF1 ALF1X
           UNSTRING FI-SEC-POL DELIMITED BY "+" INTO ALF1 ALF1X
           IF ALF1X NOT = SPACE
             MOVE SPACE TO FILEOUT01
             STRING FI-PATNAMEL " " FI-PATNAMEF " " FI-SEC-POL
               " + IN SEC POLICY NUMBER"
             INTO FILEOUT01
             WRITE FILEOUT01
           END-IF
           MOVE SPACE TO ALF1 ALF1X
           UNSTRING FI-3RD-POL DELIMITED BY "+" INTO ALF1 ALF1X
           IF ALF1X NOT = SPACE
             MOVE SPACE TO FILEOUT01
             STRING FI-PATNAMEL " " FI-PATNAMEF " " FI-3RD-POL
               " + IN TRI POLICY NUMBER"
             INTO FILEOUT01
             WRITE FILEOUT01
           END-IF

           MOVE SPACE TO FILEOUT01

           MOVE SPACE TO ALF9X ALF5X
           MOVE FI-PAT-ZIP TO ALF9X
           PERFORM ZIP-CK
           IF ALF5X NOT = SPACE
             MOVE ALF5X TO FI-PAT-ZIP.

           MOVE SPACE TO ALF10X
           MOVE FI-PRIM-ALFA TO ALF10X
           PERFORM INS-CODE-CHK
           MOVE ALF10X TO FI-PRIM-ALFA
           MOVE FI-PRIM-ALFA TO T-CODE(1)
           MOVE FI-PRIM-NAME TO T-NAME(1)
           MOVE FI-PRIM-STR1 TO T-STR1(1)
           MOVE FI-PRIM-CITY TO T-CITY(1)
           MOVE FI-PRIM-STATE TO T-STATE(1)

           MOVE SPACE TO ALF9X ALF5X
           MOVE FI-PRIM-ZIP TO ALF9X
           PERFORM ZIP-CK
           IF ALF5X NOT = SPACE
             MOVE ALF5X TO FI-PRIM-ZIP.

           MOVE FI-PRIM-ZIP TO T-ZIP(1)
           MOVE FI-PRIM-GRP TO T-GRP(1)
           MOVE FI-PRIM-POL TO T-POL(1)
           MOVE FI-PRIM-NAMEL TO T-NAMEL(1)
           MOVE FI-PRIM-NAMEF TO T-NAMEF(1)
           MOVE FI-PRIM-SUBSEX TO T-SUBSEX(1)
           MOVE FI-PRIM-SUBRELATE TO T-SUBRELATE(1)

           MOVE SPACE TO ALF10X
           MOVE FI-SEC-ALFA TO ALF10X
           PERFORM INS-CODE-CHK
           MOVE ALF10X TO FI-SEC-ALFA
           MOVE FI-SEC-ALFA TO T-CODE(2)
           MOVE FI-SEC-NAME TO T-NAME(2)
           MOVE FI-SEC-STR1 TO T-STR1(2)
           MOVE FI-SEC-CITY TO T-CITY(2)
           MOVE FI-SEC-STATE TO T-STATE(2)
           MOVE SPACE TO ALF9X ALF5X
           MOVE FI-SEC-ZIP TO ALF9X
           PERFORM ZIP-CK
           IF ALF5X NOT = SPACE
             MOVE ALF5X TO FI-SEC-ZIP.
           MOVE FI-SEC-ZIP TO T-ZIP(2)
           MOVE FI-SEC-GRP TO T-GRP(2)
           MOVE FI-SEC-POL TO T-POL(2)
           MOVE FI-SEC-NAMEL TO T-NAMEL(2)
           MOVE FI-SEC-NAMEF TO T-NAMEF(2)
           MOVE FI-SEC-SUBSEX TO T-SUBSEX(2)
           MOVE FI-SEC-SUBRELATE TO T-SUBRELATE(2)

           MOVE SPACE TO ALF10X

           MOVE SPACE TO T-CODE(3) T-NAME(3) T-CITY(3) T-STATE(3)
             T-ZIP(3) T-GRP(3) T-POL(3) T-NAMEL(3) T-NAMEF(3)
             T-SUBSEX(3) T-SUBRELATE(3)
           IF FI-3RD-ALFA NOT = 0
             MOVE FI-3RD-ALFA TO ALF10X
             PERFORM INS-CODE-CHK
             MOVE ALF10X TO FI-3RD-ALFA
             MOVE FI-3RD-ALFA TO T-CODE(3)
             MOVE FI-3RD-NAME TO T-NAME(3)
             MOVE SPACE TO T-STR1(3)
             MOVE FI-3RD-CITY TO T-CITY(3)
             MOVE SPACE TO T-STATE(3)
             MOVE SPACE TO T-ZIP(3)
             MOVE SPACE TO T-GRP(3)
             MOVE FI-3RD-POL TO T-POL(3)
             MOVE SPACE TO T-NAMEL(3)
             MOVE SPACE TO T-NAMEF(3)
             MOVE SPACE TO T-SUBSEX(3)
             MOVE SPACE TO T-SUBRELATE(3)
           END-IF  

           MOVE SPACE TO TABX(4)

           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 3
            IF T-CODE(X) = "30        "
               OR "84        "
               OR "85        "
               OR "39        "
               OR "76        " 
               OR "93        " 
               OR "129       "
               OR "135       "
               OR "139       "
              MOVE SPACE TO TABX(X)
            END-IF

            IF  ((T-CODE(X) = "33        " OR "46        ")
                AND
                (T-POL(X)(4:1) NOT = "V")
                AND (T-POL(X)(1:3) = "EVT" OR "VEI" OR "ZIA" OR "ZIB" OR
                  "ZIE" OR "ZIG" OR "ZII" OR "ZIK" OR "ZIL"))
                MOVE SPACE TO FILEOUT01
                  STRING "OLD 02 POL FOR " FI-PATNAMEL ", " FI-PATNAMEF 
                   " " T-POL(X)
                  DELIMITED BY SIZE INTO FILEOUT01
                WRITE FILEOUT01
                MOVE SPACE TO TABX(X)
            END-IF             

            IF T-CODE(2) = "34        "
                MOVE SPACE TO TABX(2)
            END-IF

            IF T-CODE(1) = "0         "
              AND T-CODE(2) NOT = SPACE
              MOVE TABX(2) TO TABX(1)
            END-IF  

            IF FI-DAT1 NOT = "00/00/00"
                DISPLAY "WE HAVE AN ACC DATE " FI-DAT1
                ACCEPT OMITTED
                IF T-CODE(1) NOT = T-CODE(3)
                  MOVE SPACE TO TABX(3)
                END-IF  
            END-IF        
           END-PERFORM

           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 2
             COMPUTE Y = X + 1
             PERFORM VARYING Z FROM Y BY 1 UNTIL Z > 3
             IF T-CODE(X) = T-CODE(Z)
               MOVE SPACE TO TABX(Z)
             END-IF

             IF T-CODE(X) = "102       "
                AND T-CODE(Z) = "82        "
               MOVE SPACE TO TABX(Z)
             END-IF

             F T-CODE(X) = "82        "
                AND T-CODE(Z) = "102       "
               MOVE SPACE TO TABX(Z)
             END-IF

             IF T-CODE(X) = "33        "
                AND T-CODE(Z) = "46        "
               MOVE SPACE TO TABX(Z)
             END-IF

             IF T-CODE(X) = "46        "
                AND T-CODE(Z) = "33        "
               MOVE SPACE TO TABX(Z)
             END-IF

             END-PERFORM
           END-PERFORM

           PERFORM VARYING X FROM 1 BY 1 UNTIL X > 2
               COMPUTE Y = X + 1
               PERFORM VARYING Z FROM Y BY 1 UNTIL Z > 3
                   IF (TABX(X) = SPACE) AND (TABX(Z) NOT = SPACE)
                      MOVE TABX(Z) TO TABX(X)
                      MOVE SPACE TO TABX(Z)
                      MOVE 4 TO Z
                   END-IF
               END-PERFORM
           END-PERFORM
           
           MOVE FI-PROVNPI TO NPI-KEY
           READ NPIFILE

           INVALID

             DISPLAY FI-PATNAMEL " " FI-PATNAMEF " "
                     FI-PROVNPI " IS MISSING"
               MOVE SPACE TO FILEOUT01
               STRING FI-PATNAMEL " " FI-PATNAMEF " "
                     FI-PROVNPI " IS MISSING" DELIMITED BY SIZE
                    INTO FILEOUT01
                    WRITE FILEOUT01
               MOVE SPACE TO FILEOUT01
               WRITE FILEOUT01

           NOT INVALID

             IF NPI-PLACE = SPACE
              DISPLAY FI-PATNAMEL " " FI-PATNAMEF " "
                      FI-PROVNPI " ?? PLACE CODE??"
                MOVE SPACE TO FILEOUT01
                STRING FI-PATNAMEL " " FI-PATNAMEF " "
                      FI-PROVNPI " ?? PLACE CODE??" DELIMITED BY SIZE
                    INTO FILEOUT01
                    WRITE FILEOUT01
                MOVE SPACE TO FILEOUT01
                WRITE FILEOUT01
             END-IF
           END-READ

           MOVE SPACE TO RPGPROC-KEY
           STRING FI-PROC1 "26    " DELIMITED BY SIZE
           INTO RPGPROC-KEY.
           READ RPGPROCFILE INVALID
            DISPLAY FI-PROC1 " IS INVALID. ADD uSING RPG-52"
            MOVE SPACE TO FILEOUT01
            WRITE FILEOUT01 FROM FI-PROC1
           END-READ.

           IF TABX(3) NOT = SPACE
             DISPLAY "1  " TABX(1)
             DISPLAY "2  " TABX(2)
             DISPLAY "3  " TABX(3)
             MOVE SPACE TO FILEOUT01
             STRING FI-PATNAMEL ", " FI-PATNAMEF 
               DELIMITED BY SIZE INTO FILEOUT01
             WRITE FILEOUT01
             MOVE SPACE TO FILEOUT01  
             WRITE FILEOUT01 FROM TABX(1)
             WRITE FILEOUT01 FROM TABX(2)
             WRITE FILEOUT01 FROM TABX(3)
            MOVE SPACE TO FILEOUT01
            WRITE FILEOUT01

             ACCEPT OMITTED
           END-IF
           MOVE T-CODE(1) TO FI-PRIM-ALFA 
           MOVE T-NAME(1) TO FI-PRIM-NAME
           MOVE T-STR1(1) TO FI-PRIM-STR1
           MOVE T-CITY(1) TO FI-PRIM-CITY
           MOVE T-STATE(1) TO FI-PRIM-STATE

           MOVE T-ZIP(1) TO FI-PRIM-ZIP
           MOVE T-GRP(1) TO FI-PRIM-GRP
           MOVE T-POL(1) TO FI-PRIM-POL
           MOVE T-NAMEL(1) TO FI-PRIM-NAMEL 
           MOVE T-NAMEF(1) TO FI-PRIM-NAMEF 
           MOVE T-SUBSEX(1) TO FI-PRIM-SUBSEX 
           MOVE T-SUBRELATE(1) TO FI-PRIM-SUBRELATE 

           MOVE T-CODE(2) TO FI-SEC-ALFA 
           MOVE T-NAME(2) TO FI-SEC-NAME
           MOVE T-STR1(2) TO FI-SEC-STR1
           MOVE T-CITY(2) TO FI-SEC-CITY
           MOVE T-STATE(2) TO FI-SEC-STATE
           MOVE T-ZIP(2) TO FI-SEC-ZIP
           MOVE T-GRP(2) TO FI-SEC-GRP
           MOVE T-POL(2) TO FI-SEC-POL 
           MOVE T-NAMEL(2) TO FI-SEC-NAMEL 
           MOVE T-NAMEF(2) TO FI-SEC-NAMEF 
           MOVE T-SUBSEX(2) TO FI-SEC-SUBSEX 
           MOVE T-SUBRELATE(2) TO FI-SEC-SUBRELATE 

           MOVE T-CODE(3) TO FI-3RD-ALFA 
           MOVE T-NAME(3) TO FI-3RD-NAME
           MOVE T-CITY(3) TO FI-3RD-CITY
           MOVE T-POL(3)  TO FI-3RD-POL

           WRITE FILEOUT201 FROM FILEIN01

           IF FI-PRIM-ALFA NOT = SPACE
             MOVE FI-PRIM-ALFA TO RPGINS-KEY
             READ RPGINSFILE INVALID
              MOVE SPACE TO FILEOUT01
              STRING "BAD PRI INS, ADD " FI-PRIM-ALFA " " FI-PRIM-NAME
                " " FI-PRIM-STR1 " " FI-PRIM-CITY " " FI-PRIM-STATE " "
                FI-PRIM-ZIP " with chchosp-1"
              DELIMITED BY SIZE INTO FILEOUT01
              WRITE FILEOUT01
            MOVE SPACE TO FILEOUT01
            WRITE FILEOUT01

              MOVE SPACE TO RPGINS-TITLE
             END-READ
            END-IF
           IF FI-SEC-ALFA NOT = SPACE
             MOVE FI-SEC-ALFA TO RPGINS-KEY
             READ RPGINSFILE INVALID
              MOVE SPACE TO FILEOUT01
              STRING "BAD SEC INS, ADD " FI-SEC-ALFA " " FI-SEC-NAME
                " " FI-SEC-STR1 " " FI-SEC-CITY " " FI-SEC-STATE " "
                FI-SEC-ZIP " with chchosp-1"
                DELIMITED BY SIZE INTO FILEOUT01
              WRITE FILEOUT01
            MOVE SPACE TO FILEOUT01
            WRITE FILEOUT01

              MOVE SPACE TO RPGINS-TITLE
             END-READ
           END-IF
           IF   FI-3RD-ALFA NOT = SPACE
             MOVE FI-3RD-ALFA TO RPGINS-KEY
             READ RPGINSFILE INVALID
              MOVE SPACE TO FILEOUT01
              STRING "BAD 3RD INS, ADD " FI-3RD-ALFA " " FI-3RD-NAME
                " " FI-3RD-CITY " with chchosp-1"
                DELIMITED BY SIZE INTO FILEOUT01
              WRITE FILEOUT01
            MOVE SPACE TO FILEOUT01
            WRITE FILEOUT01

              MOVE SPACE TO RPGINS-TITLE
             END-READ
            END-IF
           GO TO P1.

       INS-CODE-CHK.
      *     MOVE FI-PRIM-ALFA TO ALF10X.
           INSPECT ALF10X REPLACING ALL "P" BY " ".
           INSPECT ALF10X REPLACING ALL "I" BY " ".

       ZIP-CK.
           IF A5 = SPACE 
             STRING "0" ALF9X(1:4) INTO ALF5X.
    
       P99.
           CLOSE FILEIN FILEOUT FILEOUT2 NPIFILE RPGINSFILE.
           STOP RUN.
