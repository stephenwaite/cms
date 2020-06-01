       IDENTIFICATION DIVISION.                      
       PROGRAM-ID. cobr031.
      *REMARKS.  CONVERT FROM BINARY TO PRINTABLE HEX.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE13 ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FILE13.
       01  FILE1301 PIC X(318).
       FD  FILEOUT.
       01  FILEOUT01.
           02 fo-2  pic x(36).

       WORKING-STORAGE SECTION.
      * HEXVAL (output) must be twice the size of HEXNUM (input).
      * For example if you have 20 bytes of garbage but
      * want to display it in hex, change the picture
      * of HEXNUM to X(20), of HEXVAL to X(40), then
      * move the garbage to HEXNUM. Ignore DECNUM.
       01  HEXNUM   PIC X(4).
       01  DECNUM   REDEFINES HEXNUM PIC S9(8) COMP.  
       01  HEXNUM3   PIC XXX.      
       01  HEXNUM8  PIC X(8).
       01  HEXVAL6   PIC X(6).
       01  HEXVAL22  PIC X(22).
       01  HEXVAL8    PIC X(8).                          
       01  HEXVAL16   PIC X(16).
       01  HEXSTR    PIC X(16) VALUE "0123456789abcdef".
       01  DEC       PIC S9(4) COMP.                    
       01  FILLER   REDEFINES DEC.                    
        02  FILLER PIC X.                          
        02  DECBYTE PIC X.                          
       01  I   PIC S9(8) COMP.                        
       01  J   PIC S9(8) COMP.                        
       01  Q   PIC S9(8) COMP.                        
       01  R   PIC S9(8) COMP.                        
       01  J1  PIC S9(8) COMP.                        
       01  Q1  PIC S9(8) COMP.                        
       01  R1  PIC S9(8) COMP.
       01  ans pic x.
       01  cntr PIC 9(7) comp-4.
       01  NUM7 PIC 9(7).
       01  alf1 pic x.
       01  paycode pic xxx.
       01  alf9 pic x(9).
       01  tab3601.
           02 tab36 pic x occurs 36 times.
       01  x pic 99.
       01  y pic 99.
       01  z pic 99.
       01  a pic 99.
       01  b pic 99.
       PROCEDURE DIVISION.                              
       P0.
           open input file13.
           OPEN OUTPUT FILEOUT.
           set cntr TO 0.
           READ FILE13 AT END GO TO P99.
           move file1301(316:3) to paycode.
           go to p1-1.
       P1.
           READ FILE13 AT END GO TO P99.
       p1-1.
           if file1301(316:3) not = paycode
            move file1301(316:3) to paycode
            move 0 to cntr
           end-if
           MOVE FILE1301(316:3) TO HEXNUM3
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF HEXNUM3
            COMPUTE J = 2 * I - 1                  
            MOVE HEXNUM3(I:1) TO DECBYTE            
            DIVIDE DEC BY 16 GIVING Q REMAINDER R  
            COMPUTE J1 = J + 1
            COMPUTE Q1 = Q + 1                      
            COMPUTE R1 = R + 1                      
            MOVE HEXSTR(Q1:1) TO HEXVAL6(J:1)        
            MOVE HEXSTR(R1:1) TO HEXVAL6(J1:1)      
           END-PERFORM.
           move space to alf9 fileout01
           string "\" hexval6(1:2) "\" hexval6(3:2) "\" hexval6(5:2)
           delimited by size into alf9
           move alf9 to fo-2
           write fileout01

           MOVE FILE1301(1:8) TO HEXNUM8
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF HEXNUM8
            COMPUTE J = 2 * I - 1                  
            MOVE HEXNUM8(I:1) TO DECBYTE            
            DIVIDE DEC BY 16 GIVING Q REMAINDER R  
            COMPUTE J1 = J + 1                      
            COMPUTE Q1 = Q + 1                      
            COMPUTE R1 = R + 1                      
            MOVE HEXSTR(Q1:1) TO HEXVAL16(J:1)        
            MOVE HEXSTR(R1:1) TO HEXVAL16(J1:1)      
           END-PERFORM.

           set cntr up by 1
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH Of cntr  
            COMPUTE J = 2 * I - 1                  
            MOVE cntr(I:1) TO DECBYTE          
            DIVIDE DEC BY 16 GIVING Q REMAINDER R  
            COMPUTE J1 = J + 1                     
            COMPUTE Q1 = Q + 1                     
            COMPUTE R1 = R + 1                     
            MOVE HEXSTR(Q1:1) TO HEXVAL8(J:1)     
            MOVE HEXSTR(R1:1) TO HEXVAL8(J1:1)    
           END-PERFORM.


           move hexval8(1:1) to alf1
           move hexval8(7:1) to hexval8(1:1)
           move alf1 to hexval8(7:1)

           move hexval8(2:1) to alf1
           move hexval8(8:1) to hexval8(2:1)
           move alf1 to hexval8(8:1)
           
           move hexval8(3:1) to alf1
           move hexval8(5:1) to hexval8(3:1)
           move alf1 to hexval8(5:1)

           move hexval8(4:1) to alf1
           move hexval8(6:1) to hexval8(4:1)
           move alf1 to hexval8(6:1)
           
           move space to fileout01
           string hexval16 hexval8 delimited by size into fo-2
           move space to tab3601
           perform varying x from 1 by 3 until x > 35
           move "\" to tab36(x)
           end-perform
           perform varying x from 1 by 1 until x > 12           
           compute z = 3 * x - 1
           compute y = 2 * x - 1
           compute a = y + 1
           compute b = z + 1
           move fo-2(y:1) to tab36(z)
           move fo-2(a:1) to tab36(b)
           end-perform           
           move tab3601 to fo-2
           write fileout01
           
           go to p1.


       p99.
           close file13 fileout.
           STOP RUN.
