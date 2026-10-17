      *    835 service line detail, written by iedidetl (278)
      *    dt-prname = remitting payer (n1*pr), dt-insname = crossover
      *    payer the claim was forwarded to (nm1*tt), as in carer304
       01  CAREDETL01.
           02 DT-KEY.
              03 DT-KEY8 PIC X(8).
              03 DT-DATE PIC X(8).
              03 DT-PROC PIC X(5).
              03 DT-MOD1 PIC XX.
              03 DT-MOD2 PIC XX.
              03 DT-PAYDATE PIC X(8).
      *       trn02 check / eft trace, optum sends 10+
              03 DT-CK-EFT PIC X(15).
              03 DT-ICN PIC X(13).
              03 DT-SEQ PIC 9.
           02 DT-DOCP    PIC X(6).
           02 DT-POS     PIC XX.
           02 DT-BILLED PIC S9(4)V99.
           02 DT-ALLOWED PIC S9(4)V99.
           02 DT-DEDUCT  PIC S9(4)V99.
           02 DT-PAYED   PIC S9(4)V99.
           02 DT-DENIAL1 PIC X(4).
           02 DT-DENIAL2 PIC X(4).
           02 DT-DENIAL3 PIC X(4).
           02 DT-DENIAL4 PIC X(4).
           02 DT-PAYDENIAL PIC X(4).
           02 DT-INSNAME PIC X(30).
           02 DT-TB PIC X.
           02 DT-ADJ01.
              03 DT-ADJ OCCURS 6 TIMES.
                 04 DT-GRP PIC XX.
                 04 DT-RC PIC X(5).
                 04 DT-AMT PIC S9(4)V99.
           02 DT-SENDER  PIC X(15).
           02 DT-PAYERID PIC X(10).
           02 DT-PRNAME  PIC X(30).
