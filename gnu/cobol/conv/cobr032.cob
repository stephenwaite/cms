       IDENTIFICATION DIVISION.                      
       PROGRAM-ID. cobr032.
      *REMARKS.  CONVERT FROM BINARY TO PRINTABLE HEX.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FILE19 ASSIGN TO "S30" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FILE19.
       01  FILE1901 PIC X(318).
       FD  FILEOUT.
       01  FILEOUT01.
           02 fo-2  pic x(45).

       WORKING-STORAGE SECTION.
      * HEXVAL (output) must be twice the size of HEXNUM (input).
      * For example if you have 20 bytes of garbage but
      * want to display it in hex, change the picture
      * of HEXNUM to X(20), of HEXVAL to X(40), then
      * move the garbage to HEXNUM. Ignore DECNUM.
       01  HEXNUM   PIC X(8).
       01  DECNUM   REDEFINES HEXNUM PIC S9(8) COMP.  
       01  HEXNUM8   PIC X(8).      
       01  HEXNUM11  PIC X(11).
       01  HEXVAL6   PIC X(16).
       01  HEXVAL16  PIC X(16).
       01  HEXVAL    PIC X(16).                          
       01  HEXVAL8   PIC X(8).
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
       01  acctcode pic xxx.
       01  alf24 pic x(24).
       01  tab4501.
           02 tab45 pic x occurs 45 times.
       01  x pic 99.
       01  y pic 99.
       01  z pic 99.
       01  a pic 99.
       01  b pic 99.
       PROCEDURE DIVISION.                              
       P0.
           open input FILE19.
           OPEN OUTPUT FILEOUT.
           set cntr TO 0.
           READ FILE19 AT END GO TO P99.
           move FILE1901(316:3) to acctcode.
           go to p1-1.
       P1.
           READ FILE19 AT END GO TO P99.
       p1-1.
           if FILE1901(278:8) not = acctcode
            move FILE1901(278:8) to acctcode
            move 0 to cntr
           end-if
           MOVE FILE1901(278:8) TO HEXNUM
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LENGTH OF HEXNUM
            COMPUTE J = 2 * I - 1                  
            MOVE HEXNUM(I:1) TO DECBYTE            
            DIVIDE DEC BY 16 GIVING Q REMAINDER R  
            COMPUTE J1 = J + 1
            COMPUTE Q1 = Q + 1                      
            COMPUTE R1 = R + 1                      
            MOVE HEXSTR(Q1:1) TO HEXVAL(J:1)        
            MOVE HEXSTR(R1:1) TO HEXVAL(J1:1)      
           END-PERFORM.
           move space to alf24 fileout01
           string "\" hexval(1:2) "\" hexval(3:2) "\" hexval(5:2)
            "\" hexval(7:2) "\" hexval(9:2) "\" hexval(11:2)
            "\" hexval(13:2) "\" hexval(15:2) 
           delimited by size into alf24
           move alf24 to fo-2
           write fileout01

           MOVE FILE1901(1:8) TO HEXNUM8
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
           move space to tab4501
           perform varying x from 1 by 3 until x > 35
           move "\" to tab45(x)
           end-perform
           perform varying x from 1 by 1 until x > 12           
           compute z = 3 * x - 1
           compute y = 2 * x - 1
           compute a = y + 1
           compute b = z + 1
           move fo-2(y:1) to tab45(z)
           move fo-2(a:1) to tab45(b)
           end-perform           
           move tab4501 to fo-2
           write fileout01
           
           go to p1.


       p99.
           close FILE19 fileout.
           STOP RUN.
