       01  ACTFILE01.
           02 A-ACTNO PIC X(8).
           02 A-GARNAME.
             03 A-GN1 PIC XXX.
             03 A-GN2 PIC X(21).
           02 A-BILLADD PIC X(22).
           02 A-STREET PIC X(22).
           02 A-CITY PIC X(18).
           02 A-STATE PIC X(2).
           02 A-ZIP.
             03 A-ZIP5 PIC X(5).
             03 A-ZIP4 PIC X(4).
           02 A-COLLT PIC X.
           02 A-PHONE.
             03 A-PHONE1 PIC XXX.
             03 A-PHONE2 PIC XXX.
             03 A-PHONE3 PIC X(4).
           02 A-SEX PIC X.
           02 A-RELATE PIC X.
           02 A-MSTAT PIC X.
           02 A-DOB PIC X(8).
           02 A-DUNNING PIC X.
           02 A-ACCTSTAT PIC X.
           02 A-PR-MPLR PIC X(4).
           02 A-PRINS PIC XXX.
           02 A-PR-ASSIGN PIC X.
           02 A-PR-OFFICE PIC X(4).
           02 A-PR-GROUP.
             03 A-PR-GROUP2  PIC XX.
             03 FILLER       PIC X(8).
           02 A-PRIPOL.
             03 A-PRIPOL1 PIC X(9).
             03 A-PRIPOL2 PIC XXX.
             03 A-PR-FILLER PIC X(4).
           02 A-PRNAME PIC X(24).
           02 A-PR-RELATE PIC X.
           02 A-SE-MPLR PIC X(4).
           02 A-SEINS PIC XXX.
           02 A-SE-ASSIGN PIC X.
           02 A-TRINSIND PIC X.
           02 A-TRINS PIC XXX.
           02 A-SE-GROUP.
             03 A-SE-GROUP2 PIC XX.
             03 FILLER PIC X(8).
           02 A-SECPOL.
             03 A-SECPOL1 PIC X(9).
             03 A-SECPOL2 PIC XXX.
             03 A-SE-FILLER PIC X(4).
           02 A-SENAME PIC X(24).
           02 A-SE-RELATE PIC X.
           02 A-INSPEND PIC X(7).
           02 A-LASTBILL PIC X(8).
           02 A-ASSIGNM PIC X.
           02 A-PRIVATE PIC X.
           02 A-BILLCYCLE PIC X.
           02 A-DELETE PIC X.
           02 A-FILLER PIC XXX.
           02 A-GARNO PIC X(8).
           02 A-PRGRPNAME PIC X(15).
           02 A-SEGRPNAME PIC X(15).
           02 NAME-KEY PIC XXX.