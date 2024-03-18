       01  AMT01.
           02 AMT-0 PIC XXX.
           02 AMT-1 PIC XX.
           02 AMT-2 PIC X(9).
           02 AMT-3 PIC X.

       01  BPR01.
           02 BPR-0 PIC XXX.
           02 BPR-1 PIC XX.
           02 BPR-2 PIC X(9).
           02 BPR-3 PIC X.
           02 BPR-4 PIC XXX.
           02 BPR-5 PIC X(10).
           02 BPR-6 PIC XX.
           02 BPR-7 PIC X(12).
           02 BPR-8 PIC XXX.
           02 BPR-9 PIC X(35).
           02 BPR-10 PIC X(10).
           02 BPR-11 PIC X(9).
           02 BPR-12 PIC XX.
           02 BPR-13 PIC X(12).
           02 BPR-14 PIC XXX.
           02 BPR-15 PIC X(35).
           02 BPR-16 PIC X(8).

       01  TRN01.
           02 TRN-0 PIC XXX.
           02 TRN-1 PIC X.
           02 TRN-2 PIC X(30).
           02 TRN-3 PIC X(30).
           02 TRN-4 PIC X(30).

       01  CAS01.
           02 CAS-0 PIC XXX.
           02 CAS-1 PIC XX.
           02 CAS-2 PIC X(5).
           02 CAS-3 PIC X(8).
           02 CAS-4 PIC XX.
           02 CAS-5 PIC X(5).
           02 CAS-6 PIC X(8).
           02 CAS-7 PIC XX.
           02 CAS-8 PIC X(5).
           02 CAS-9 PIC X(8).
           02 CAS-10 PIC XX.
           02 CAS-11 PIC X(5).
           02 CAS-12 PIC X(8).
           02 CAS-13 PIC XX.
           02 CAS-14 PIC X(5).
           02 CAS-15 PIC X(8).
           02 CAS-16 PIC XX.
           02 CAS-17 PIC X(5).
           02 CAS-18 PIC X(8).
           02 CAS-19 PIC XX.
           
       01  CLMCAS01.
           02 CLMCAS-0 PIC XXX.
           02 CLMCAS-1 PIC XX.
           02 CLMCAS-2 PIC X(5).
           02 CLMCAS-3 PIC X(8).
           02 CLMCAS-4 PIC XX.
           02 CLMCAS-5 PIC X(5).
           02 CLMCAS-6 PIC X(8).
           02 CLMCAS-7 PIC XX.
           02 CLMCAS-8 PIC X(5).
           02 CLMCAS-9 PIC X(8).
           02 CLMCAS-10 PIC XX.
           02 CLMCAS-11 PIC X(5).
           02 CLMCAS-12 PIC X(8).
           02 CLMCAS-13 PIC XX.
           02 CLMCAS-14 PIC X(5).
           02 CLMCAS-15 PIC X(8).
           02 CLMCAS-16 PIC XX.
           02 CLMCAS-17 PIC X(5).
           02 CLMCAS-18 PIC X(8).
           02 CLMCAS-19 PIC XX. 

       01  CLP01.
           02 CLP-0 PIC XXX.
           02 CLP-1 PIC X(14).
           02 CLP-2CLMSTAT PIC XX.
           02 CLP-3TOTCLMCHG PIC X(8).
           02 CLP-4TOTCLMPAY PIC X(8).
           02 CLP-5PATRESP PIC X(8).
           02 CLP-6PLANCODE PIC XX.
           02 CLP-7ICN PIC X(30).
           02 CLP-8FACILITY PIC XX.
           02 CLP-9FREQ PIC X.
           02 CLP-10PATSTAT PIC X(4).
           02 CLP-11DRG PIC X.
           02 CLP-12QUAN PIC XXX.
           02 CLP-13PERCENT PIC XXX.
           
       01  DTM01.
           02 DTM-0 PIC XXX.
           02 DTM-1 PIC XXX.
           02 DTM-2 PIC X(8).
           
       01  REF01.
           02 REF-0 PIC XXX.
           02 REF-1 PIC XXX.
           02 REF-2 PIC X(30).
          
       01  SVC01.
           02 SVC-0 PIC XXX.
           02 SVC-1PROCMOD PIC X(17).
           02 SVC-2CHRGAMT PIC X(8).
           02 SVC-3PAYAMT  PIC X(8).
           02 SVC-4NUBC PIC XXX.
           02 SVC-5QUAN PIC X(5).
           02 SVC-6COMPOSITE PIC X(80).
           02 SVC-7QUAN PIC X(5).

       01  N101.
           02 N1-0 PIC XX.
           02 N1-1 PIC XX.
           02 N1-2 PIC X(20).
           02 N1-3 PIC XX.
           02 N1-ID PIC X(10).

       01  NM101.
           02 NM1-0 PIC XXX.
           02 NM1-1 PIC XXX.
           02 NM1-SOLO PIC X.
           02 NM1-NAMEL PIC X(24).
           02 NM1-NAMEF PIC X(24).
           02 NM1-NAMEM PIC X.
           02 NM1-NAMES PIC XXX.
           02 NM1-EINSS PIC XX.
           02 NM1-PREFIX PIC XX.
           02 NM1-CODE0.
             03 NM1-CODE PIC X(9).
             03 NM1-CODE2 PIC XX.


       01  NM1COR01.
           02 NM1COR-0 PIC XXX.
           02 NM1COR-1 PIC XXX.
           02 NM1COR-SOLO PIC X.
           02 NM1COR-NAMEL PIC X(24).
           02 NM1COR-NAMEF PIC X(24).
           02 NM1COR-NAMEM PIC X.
           02 NM1COR-NAMES PIC XXX.
           02 NM1COR-EINSS PIC XX.
           02 NM1COR-PREFIX PIC XX.
           02 NM1COR-CODE PIC X(16).

           
       01  TS301.
           02 TS3-0 PIC XXX.
           02 TS3-1 PIC X(7).
           02 TS3-2 PIC XX.
           02 TS3-3 PIC X(8).
           02 TS3-4 PIC XXXX.
           02 TS3-5TOTCLM PIC X(9).
           02 TS3-6TOTCVR PIC X(9).
           02 TS3-7TOTNONCVR PIC X(9).
           02 TS3-8TOTDENY PIC X(9).
           02 TS3-9TOTPAID PIC X(9).
           02 TS3-10INTEREST PIC X(9).
           02 TS3-11TOTCONADJ PIC X(9).
           02 TS3-12TOTGR PIC X(9).
           02 TS3-13TOTMSP PIC X(9).
           02 TS3-14TOTBLOOD PIC X(9).
           02 TS3-15TOTNONLABCHRG PIC X(9).
           02 TS3-16TOTCOINS PIC X(9).
           02 TS3-17TOTHCPCSCHG PIC X(9).
           02 TS3-18TOTHCPCSPAY PIC X(9).
           02 TS3-19TOTDEDUCT PIC X(9).
           02 TS3-20TOTPC PIC X(9).
           02 TS3-21TOTMSPLIAB PIC X(9).
           02 TS3-22TOTPATPAY PIC X(9).
           02 TS3-23TOTPIPCNTR PIC X(9).
           02 TS3-24TOTPIPPAY PIC X(9).       

       01  LQ01.
           02 LQ-0 PIC XX.
           02 LQ-1 PIC XX.
           02 LQ-2 PIC X(5).
