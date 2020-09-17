      * @PACKAGE CMS
      * @LINK    HTTP://WWW.CMSVT.COM
      * @AUTHOR  S WAITE <CMSWEST@SOVER.NET>
      * @COPYRIGHT COPYRIGHT (C) 2020 CMS <CMSWEST@SOVER.NET>
      * @LICENSE HTTPS://GITHUB.COM/OPENEMR/OPENEMR/BLOB/MASTER/LICENSE GNU GENERAL PUBLIC LICENSE 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STE001.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILEIN ASSIGN TO "S25" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT GARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
           ALTERNATE RECORD KEY IS G-ACCT WITH DUPLICATES
           LOCK MODE MANUAL.

           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC    RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.

           SELECT FILEOUT1 ASSIGN TO "S40" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT FILEOUT2 ASSIGN TO "S45" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT FILEOUT3 ASSIGN TO "S50" ORGANIZATION
           LINE SEQUENTIAL.

           SELECT FILEOUT4 ASSIGN TO "S55" ORGANIZATION
           LINE SEQUENTIAL.           

       DATA DIVISION.

       FILE SECTION.

       FD  FILEIN.
       01  FILEIN01.
           02 FI-CDM PIC X(7).
           02 FILLER PIC X.
           02 FI-PROC PIC X(5).
           02 FILLER PIC X.
           02 FI-DATEX PIC X(8).
           02 FILLER PIC X.
	       02 FI-MEDREC PIC X(6).
	
       FD  CHARCUR
           DATA RECORD IS CHARCUR01.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC0 PIC X(4).
           02 CC-PROC1 PIC X(5).
           02 CC-PROC2 PIC XX.
           02 CC-MOD2 PIC XX.
           02 CC-MOD3 PIC XX.
           02 CC-MOD4 PIC XX.
           02 CC-AMOUNT PIC S9(4)V99.
           02 CC-DOCR PIC X(3).
           02 CC-DOCP PIC X(2).
           02 CC-PAYCODE PIC XXX.
           02 CC-STUD PIC X.
           02 CC-WORK PIC XX.
           02 CC-DAT1 PIC X(8).
           02 CC-RESULT PIC X.
           02 CC-ACT PIC X.
           02 CC-SORCREF PIC X.
           02 CC-COLLT PIC X.
           02 CC-AUTH PIC X.
           02 CC-PAPER PIC X.
           02 CC-PLACE PIC X.
           02 CC-EPSDT PIC X.
           02 CC-DATE-T PIC X(8).
           02 CC-DATE-A PIC X(8).
           02 CC-DATE-P PIC X(8).
           02 CC-REC-STAT PIC X.
           02 CC-DX2 PIC X(7).
           02 CC-DX3 PIC X(7).
           02 CC-ACC-TYPE PIC X.
           02 CC-DATE-M PIC X(8).
           02 CC-ASSIGN PIC X.
           02 CC-NEIC-ASSIGN PIC X.
           02 CC-DX4 PIC X(7).
           02 CC-DX5 PIC X(7).
           02 CC-DX6 PIC X(7).
           02 CC-FUTURE PIC X(6).

       FD  FILEOUT1.
       01  FILEOUT101 PIC X(65).

       FD  FILEOUT2.
       01  FILEOUT201 PIC X(160).

       FD  FILEOUT3.
       01  FILEOUT301 PIC X(160).

       FD  FILEOUT4.
       01  FILEOUT401 PIC X(160).

       FD GARFILE
           DATA RECORD IS GARFILE01.
       01 GARFILE01.
           02 G-GARNO PIC X(8).
           02 G-GARNAME PIC X(24).
           02 G-BILLADD PIC X(22).
           02 G-STREET PIC X(22).
           02 G-CITY PIC X(18).
           02 G-STATE PIC X(2).
           02 G-ZIP PIC X(9).
           02 G-COLLT PIC X.
           02 G-PHONE PIC X(10).
           02 G-SEX PIC X.
           02 G-RELATE PIC X.
           02 G-MSTAT PIC X.
           02 G-DOBCC PIC XX.
           02 G-DOBYY PIC XX.
           02 G-DOBMM PIC XX.
           02 G-DOBDD PIC XX.
           02 G-DUNNING PIC X.
           02 G-ACCTSTAT PIC X.
           02 G-PR-MPLR PIC X(4).
           02 G-PRINS PIC XXX.
           02 G-PR-ASSIGN PIC X.
           02 G-PR-OFFICE PIC X(4).
           02 G-PR-GROUP PIC X(12).
           02 G-PRIPOL PIC X(14).
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-SE-MPLR PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-SE-OFFICE PIC X(4).
           02 G-SE-GROUP PIC X(12).
           02 G-SECPOL PIC X(14).
           02 G-SENAME PIC X(24).
           02 G-SE-RELATE PIC X.
           02 G-INSPEND PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.
           02 G-ACCT PIC X(8).
           02 G-PRGRPNAME PIC X(15).
           02 G-SEGRPNAME PIC X(15).

       WORKING-STORAGE SECTION.
       01  RIGHT-2 PIC XX JUST RIGHT.
       01  RIGHT-2X PIC XX JUST RIGHT.
       01  RIGHT-8 PIC X(8) JUST RIGHT.
       01  FI-YY PIC X(4).
       01  DATEX PIC X(8).
       01  MEDREC8 PIC X(8).
       01  ALF8 PIC X(8).
       01  FI-MEDREC8 PIC X(8).
       
       PROCEDURE DIVISION.
       
       0005-START.
           OPEN INPUT FILEIN GARFILE CHARCUR
           OPEN OUTPUT FILEOUT1 FILEOUT2 FILEOUT3 FILEOUT4.

       P00.
           MOVE SPACE TO FILEIN01
           READ FILEIN
             AT END
               GO TO P2
           END-READ
               
           MOVE SPACE TO ALF8
           MOVE FI-MEDREC TO ALF8
           MOVE SPACE TO RIGHT-8
           UNSTRING ALF8 DELIMITED BY " " INTO RIGHT-8
           INSPECT RIGHT-8 REPLACING LEADING " " BY "0"
           MOVE RIGHT-8 TO FI-MEDREC8
           MOVE FI-MEDREC8 TO G-ACCT
           START GARFILE KEY NOT <  G-ACCT
             INVALID
               GO TO ERR-1
           END-START.

       P0.
           READ GARFILE NEXT
             AT END
               GO TO ERR-1
           END-READ

           IF G-ACCT > FI-MEDREC8
               GO TO ERR-1
           END-IF

           MOVE G-GARNO TO CC-KEY8
           MOVE SPACE TO CC-KEY3
           START CHARCUR KEY NOT < CHARCUR-KEY
             INVALID
               GO TO ERR-1
           END-START.

       P1. 
           READ CHARCUR NEXT
             AT END
               GO TO P0
           END-READ

           IF CC-KEY8 NOT = G-GARNO GO TO P0.
           
           IF CC-DATE-T NOT = FI-DATEX GO TO P1.
           
           IF CC-PROC1 NOT = FI-PROC GO TO P1.
           
           IF CC-DOCP = "00" 
               WRITE FILEOUT301 FROM CHARCUR01
           END-IF

           IF CC-DOCP = "02" 
               WRITE FILEOUT401 FROM CHARCUR01
           END-IF

           WRITE FILEOUT201 FROM CHARCUR01.
           
           GO TO P00.

       ERR-1.
           INSPECT FILEIN01 REPLACING ALL "," BY " "
           WRITE FILEOUT101 FROM FILEIN01.
           GO TO P00.

       P2.
           CLOSE GARFILE CHARCUR FILEOUT1 FILEOUT2
               FILEOUT3 FILEOUT4.
           STOP RUN.

