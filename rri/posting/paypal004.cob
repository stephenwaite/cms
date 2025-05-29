       IDENTIFICATION DIVISION.
       PROGRAM-ID. paypal004.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILEIN ASSIGN TO "S30" ORGANIZATION
             LINE SEQUENTIAL.
           
           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
             LINE SEQUENTIAL.
           
           SELECT FILEOUT2 ASSIGN TO "S40" ORGANIZATION
             LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.
       
       FD  FILEIN
           RECORD CONTAINS 1 TO 132 CHARACTERS.
       01  FILEIN01 PIC X(132).

       FD  FILEOUT.
       01  FILEOUT01.
           02 FO-DATE PIC X(8).
           02 FILLER PIC X VALUE SPACES.
           02 FO-NAME PIC X(30).
           02 FILLER PIC X value space.
           02 FO-GARNO PIC X(8).
           02 FILLER PIC X VALUE SPACES.
           02 FO-DEDUCT PIC 9(4)V99.
           02 FILLER PIC X VALUE SPACES.
           02 FO-PAID PIC 9(4)V99.
       
       FD  FILEOUT2.
       01  FILEOUT201 PIC X(132).

       WORKING-STORAGE SECTION.
       01  FL1 PIC X.
       01  FL2 PIC X.
       01  FL3 PIC X.
       01  FL4 PIC X.
       01  FL5 PIC X.
       01  FL6 PIC XXX.
       01  FL7 PIC X.
       01  FL8 PIC X.

       01  RIGHT-4 PIC X(4) JUST RIGHT.
       01  SIGN-DOLLAR PIC X(4).
       01  CENTS PIC XX.
       01  NUMX PIC X(7).
       01  NUM6 PIC 9(6).

       01  FO-COMMIS PIC X(6).
       01  FO-PAIDX PIC X(6).
       01  ALF6 PIC X(6).
       01  NUM-6 PIC 9(4)V99.
       01  ALF1 PIC X.

       PROCEDURE DIVISION.

       0005-START.
           OPEN INPUT FILEIN 
           OPEN OUTPUT FILEOUT FILEOUT2.

       P1. 
           MOVE SPACE TO FILEIN01
           READ FILEIN 
             AT END
               GO TO P2.

           MOVE SPACE TO FILEOUT01 FL1 FL2 FL3 FL5 FL6 FL7
           UNSTRING FILEIN01 DELIMITED BY "," INTO
           FO-DATE FO-NAME FO-GARNO FL5 FL6 FL7 FL8 FO-COMMIS FO-PAIDX
           DISPLAY FO-DATE
           DISPLAY FO-GARNO
           DISPLAY FL6
           DISPLAY FO-COMMIS
           DISPLAY FO-PAIDX
      *     ACCEPT omitted
           IF NOT (FL7 = "T")
             MOVE SPACE TO FILEOUT201
             MOVE FILEIN01 TO FILEOUT201
             WRITE FILEOUT201 
             GO TO P1.

           MOVE SPACE TO NUMX
           MOVE FO-COMMIS TO NUMX
           PERFORM A1

           MOVE NUM-6 TO FO-DEDUCT
           MOVE SPACE TO NUMX
           MOVE FO-PAIDX TO NUMX
           PERFORM A1
           
           MOVE NUM-6 TO FO-PAID
           WRITE FILEOUT01
           GO TO P1.

       A1.
           MOVE SPACE TO SIGN-DOLLAR CENTS
           UNSTRING NUMX DELIMITED BY "." INTO SIGN-DOLLAR CENTS.
           INSPECT CENTS REPLACING ALL " " BY "0"
           
           IF CENTS = SPACES MOVE "00" TO CENTS.
           
           IF CENTS NOT NUMERIC
             DISPLAY FILEIN01 " CENTS"
             ACCEPT ALF1
             GO TO P2.

           MOVE SPACES TO RIGHT-4.
           UNSTRING SIGN-DOLLAR DELIMITED BY " " INTO RIGHT-4
           INSPECT RIGHT-4 REPLACING LEADING " " BY "0"
           
           IF RIGHT-4 NOT NUMERIC
             DISPLAY FILEIN01 " DOLLARS"
             ACCEPT ALF1
             GO TO P2.

           STRING RIGHT-4 CENTS DELIMITED BY SIZE INTO ALF6
           MOVE ALF6 TO NUM6
           DIVIDE NUM6 BY 100 GIVING NUM-6.

       P2.
           
           CLOSE FILEIN FILEOUT FILEOUT2
           STOP RUN.

