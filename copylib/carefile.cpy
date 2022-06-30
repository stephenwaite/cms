       01  CAREFILE01.
           02 CARE-KEY.
              03 CR-KEY8 PIC X(8).
              03 CR-DATE PIC X(8).
              03 CR-PROC PIC X(5).
              03 CR-MOD1 PIC XX.
              03 CR-MOD2 PIC XX.
           02 CR-PAYDATE PIC X(8).
           02 CR-DOCP    PIC X(6).
           02 CR-POS     PIC XX.
           02 CR-BILLED PIC 9(4)V99.
           02 CR-ALLOWED PIC 9(4)V99.
           02 CR-DEDUCT  PIC 9(4)V99.
           02 CR-PAID   PIC 9(4)V99.
           02 CR-DENIAL1 PIC X(4).
           02 CR-DENIAL2 PIC X(4).
           02 CR-DENIAL3 PIC X(4).
           02 CR-DENIAL4 PIC X(4).
           02 CR-PAYDENIAL PIC X(4).
           02 CR-ICN PIC X(13).
           02 CR-CK-EFT PIC X(9).
           02 CR-INSNAME PIC X(30).