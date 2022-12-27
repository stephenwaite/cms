      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. scr006.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GARFILE ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
           LOCK MODE MANUAL.
           SELECT CHARCUR ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT AGEDATE ASSIGN TO "S40" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S45" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT INSFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS INS-KEY
           ALTERNATE RECORD KEY IS INS-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-ASSIGN WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-CLAIMTYPE WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC WITH DUPLICATES
           ALTERNATE RECORD KEY IS INS-NEIC-ASSIGN WITH DUPLICATES
           LOCK MODE MANUAL.

           SELECT REFPHY ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS REF-KEY
           ALTERNATE RECORD KEY IS REF-BSNUM  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CRNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-UPIN  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CDNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-NAME  WITH DUPLICATES
           LOCK MODE MANUAL.

       DATA DIVISION.
       FILE SECTION.
       FD  INSFILE
     *     BLOCK CONTAINS 6 RECORDS
           DATA RECORD IS INSFILE01.
       01  INSFILE01.
           02 INS-KEY PIC XXX.
           02 INS-NAME PIC X(22).
           02 INS-STREET PIC X(24).
           02 INS-CITY PIC X(15).
           02 INS-STATE PIC XX.
           02 INS-ZIP PIC X(9).
           02 INS-ASSIGN PIC X.
           02 INS-CLAIMTYPE PIC X.
           02 INS-NEIC PIC X(5).
           02 INS-NEICLEVEL PIC X.
           02 INS-NEIC-ASSIGN PIC X.
           02 INS-PPO PIC X.
           02 INS-PRVNUM PIC X(10).
           02 INS-HMO PIC X(3).
           02 INS-STATUS PIC X.
           02 INS-LEVEL PIC X.
           02 INS-LASTDATE PIC X(8).
           02 INS-CAID PIC XXX.
           02 INS-REFWARN PIC X.
           02 INS-FUTURE PIC X(8).
       FD  AGEDATE.
       01  AGEDATE01.
           02 AGE-LOW PIC X(8).
           02 AGE-HIGH PIC X(8).
       FD FILEOUT.
       01 FILEOUT01 PIC X(200).
       FD  CHARCUR
      *    BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS CHARCUR01.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC PIC X(7).
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
       FD GARFILE.
       01 GARFILE01.
           02 G-GARNO PIC X(8).
           02 G-GARNAME PIC X(24).
           02 G-BILLADD PIC X(22).
           02 G-STREET PIC X(22).
           02 G-CITY PIC X(18).
           02 G-STATE PIC X(2).
           02 G-ZIP. 
              03 GZIP5 PIC X(5).
              03 GZIP4 PIC X(4).
           02 G-COLLT PIC X.
           02 G-PHONE.
              03 G-PH1 PIC XXX.
              03 G-PH2 PIC XXX.
              03 G-PH3 PIC XXXX.
           02 G-SEX PIC X.
           02 G-RELATE PIC X.
           02 G-MSTAT PIC X.
           02 G-DOB. 
              03 G-DOBYY PIC X(4).
              03 G-DOBMM PIC XX.
              03 G-DOBDD PIC XX.
           02 G-DUNNING PIC X.
           02 G-ACCTSTAT PIC X.
           02 G-PR-MPLR PIC X(4).
           02 G-PRINS PIC XXX.
           02 G-PR-ASSIGN PIC X.
           02 G-PR-OFFICE PIC X(4).
           02 G-PR-GROUP. 
              03 PR-GROUP10 PIC X(10).
           02 G-PRIPOL. 
              03 G-PRIPOL2 PIC XX.
              03 G-PRIPOL3 PIC X.
              03 G-PRIPOL4 PIC X.
              03 FILLER PIC X(12).
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-SE-MPLR PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
           02 G-SE-GROUP. 
              03 SE-GROUP10 PIC X(10).
           02 G-SECPOL. 
              03 G-SE-GROUP2 PIC XX.
              03 G-SECPOL14 PIC X(14).
           02 G-SENAME PIC X(24).
           02 G-SE-RELATE PIC X.
           02 G-COPAY PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.
       
       Fd  refphy.
           COPY refphy.cpy IN "C:\Users\sid\cms\copylib".           

       WORKING-STORAGE SECTION.
       01  ALF8 PIC X(8) VALUE SPACE.
       01  ALF34 PIC X(34) VALUE SPACE.
       01  ALF22 PIC X(22).
       01  NAMEF PIC X(24).
       01  NAMEL PIC X(24).
       01  FLAG PIC 9.
       01  SAVEDATE PIC X(8).
       01  TB PIC X VALUE H"09".
       PROCEDURE DIVISION.
       P0.
           OPEN OUTPUT FILEOUT.
           OPEN INPUT GARFILE CHARCUR AGEDATE INSFILE refphy.
           READ AGEDATE AT END CONTINUE.
           MOVE SPACE  TO G-GARNO
           START GARFILE KEY NOT < G-GARNO INVALID GO TO P99.
       P1.
           READ GARFILE NEXT AT END GO TO P99.
           MOVE G-GARNO TO CC-KEY8
           MOVE "   " TO CC-KEY3
           START CHARCUR KEY NOT < CHARCUR-KEY INVALID GO TO P1.

       P2.
           READ CHARCUR next AT END GO TO P1.

           IF CC-KEY8 NOT = G-GARNO GO TO P1.

      *    IF CC-PLACE NOT = "1" GO TO P2.

           IF CC-DATE-T < AGE-LOW OR > AGE-HIGH GO TO P2.

           MOVE G-PRINS TO INS-KEY
           READ INSFILE INVALID MOVE SPACE TO INS-NAME.
           MOVE SPACE TO ALF22

           move cc-docr to ref-key
           read refphy invalid
             move space to ref-name ref-npi.

           IF G-BILLADD = SPACE 
             MOVE G-STREET TO ALF22
           ELSE
             MOVE G-BILLADD TO ALF22
           END-IF

           MOVE SPACE TO FILEOUT01 NAMEL NAMEF
           UNSTRING G-GARNAME DELIMITED BY ";" INTO NAMEL NAMEF.
           
           IF G-PRINS = "003"
             STRING G-GARNO ref-name ref-npi TB NAMEF(1:10) 
               TB NAMEL(1:14)
               TB CC-DATE-T(5:2) "-" CC-DATE-T(7:2) "-" CC-DATE-T(1:4)
               TB G-DOB TB G-SEX
               TB G-BILLADD TB G-STREET TB G-CITY TB G-STATE TB GZIP5 
               TB G-PH1 "-" G-PH2 "-" G-PH3 " TB MEDICARE "
               G-PRIPOL(1:11)
               DELIMITED BY SIZE INTO FILEOUT01
             WRITE FILEOUT01
           ELSE
             STRING G-GARNO ref-name ref-npi TB NAMEF(1:10) 
               TB NAMEL(1:14)
               TB CC-DATE-T(5:2) "-" CC-DATE-T(7:2) "-" CC-DATE-T(1:4)
               TB G-DOB TB G-SEX
               TB G-BILLADD TB G-STREET TB G-CITY TB GZIP5  TB
               G-PH1 "-" G-PH2 "-" G-PH3 TB INS-NAME
               DELIMITED BY SIZE INTO FILEOUT01
             WRITE FILEOUT01.

           MOVE CC-DATE-T TO SAVEDATE.

       P3.
           READ CHARCUR NEXT AT END GO TO P1.
           IF CC-KEY8 NOT = G-GARNO GO TO P1.
      *     IF CC-PLACE NOT = "1" GO TO P2.
           IF CC-DATE-T < AGE-LOW OR > AGE-HIGH GO TO P3.
           IF CC-DATE-T = SAVEDATE GO TO P3.
           MOVE CC-DATE-T TO SAVEDATE
           
           move cc-docr to ref-key
           read refphy invalid
             move space to ref-name ref-npi.

           MOVE SPACE TO FILEOUT01
           STRING ALF8 REF-NAME REF-NPI
           " " CC-DATE-T(5:2) " " CC-DATE-T(7:2) " " CC-DATE-T(1:4)
           DELIMITED BY SIZE INTO FILEOUT01.
           WRITE FILEOUT01.
           GO TO P3.

       P99. 
           CLOSE FILEOUT GARFILE CHARCUR AGEDATE refphy
           STOP RUN.
