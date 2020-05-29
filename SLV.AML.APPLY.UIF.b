*-----------------------------------------------------------------------------
* <Rating>-55</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.AML.APPLY.UIF(Y.IN.TYPE,Y.IN.ACCT,Y.IN.CONTRACT,Y.IN.READ,Y.OUT.APPLY,Y.OUT.TYPE)
*-----------------------------------------------------------------------------

** Company Name:  BANCO AZUL
* Developed By:   Abinanthan - Capgemini
* Date  :         2014/11/21 
*------------------------------------------------
* Subroutine Type: API
* Attached to:
* Attached as:     Call routine used by other subroutines
* Primary Purpose: To Check Cash or Non-Cash
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER

    GOSUB INIT ; *
*-----------------------------------------------------------------------------

*** <region name= INIT>
INIT:
*** <desc> </desc>

    Y.OUT.APPLY = 'N'; Y.OUT.TYPE = ''; Y.ACCT.TMP = ''
    Y.TXN.ID.CASH.CHK = Y.IN.CONTRACT
    GOSUB OPEN.FILES
    IF Y.IN.CONTRACT[1,2] EQ 'TT' THEN
        GOSUB TT.PROCESS
    END ELSE
        GOSUB FT.PROCESS
    END
    GOSUB CHECK.TYPE ; *Check the type
    RETURN
*** </region>
TT.PROCESS:
***********

    IF Y.READ THEN
        R.TELLER = '' ; ERR.TT = '' ; NOC.POS = ''
        CALL F.READ(FN.TELLER, Y.TXN.ID.CASH.CHK, R.TELLER, F.TELLER, ERR.TT)
        IF R.TELLER THEN
            Y.DR.CR.MARKER = R.TELLER<TT.TE.DR.CR.MARKER>
            Y.ACCT.1 = R.TELLER<TT.TE.ACCOUNT.1>
            Y.ACCT.2 = R.TELLER<TT.TE.ACCOUNT.2>
        END
    END ELSE
        Y.DR.CR.MARKER = R.NEW(TT.TE.DR.CR.MARKER)
        Y.ACCT.1 = R.NEW(TT.TE.ACCOUNT.1)
        Y.ACCT.2 = R.NEW(TT.TE.ACCOUNT.2)
    END
    IF Y.DR.CR.MARKER EQ 'DEBIT' THEN
        Y.DEBIT.ACCOUNT = Y.ACCT.1
        Y.CREDIT.ACCOUNT = Y.ACCT.2
    END ELSE
        Y.DEBIT.ACCOUNT = Y.ACCT.2
        Y.CREDIT.ACCOUNT = Y.ACCT.1
    END


    RETURN


OPEN.FILES:
**********
    FN.TELLER = 'F.TELLER'
    F.TELLER = ''
    CALL OPF(FN.TELLER, F.TELLER)

    FN.FUNDS.TRANSFER = 'F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER = ''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)
    RETURN

FT.PROCESS:
************

    IF Y.READ THEN
        R.FUNDS.TRANSFER = '' ; ERR.TT = '' ; NOC.POS = ''
        CALL F.READ(FN.FUNDS.TRANSFER, Y.TXN.ID.CASH.CHK, R.FUNDS.TRANSFER, F.FUNDS.TRANSFER, ERR.TT)
        IF R.FUNDS.TRANSFER THEN
            Y.DEBIT.ACCOUNT = R.FUNDS.TRANSFER<FT.DEBIT.ACCT.NO>
            Y.CREDIT.ACCOUNT = R.FUNDS.TRANSFER<FT.CREDIT.ACCT.NO>
        END
    END ELSE
  
        Y.DEBIT.ACCOUNT =  R.NEW(FT.DEBIT.ACCT.NO)
        Y.CREDIT.ACCOUNT = R.NEW(FT.CREDIT.ACCT.NO)
      
      
    END

    RETURN



*-----------------------------------------------------------------------------

*** <region name= CHECK.IN.TYPE>
CHECK.TYPE:
*** <desc>Check the IN type </desc>
    BEGIN CASE

        CASE Y.IN.TYPE EQ 'D' OR Y.IN.TYPE EQ 'C'
            Y.ACCT.TMP = ''
            Y.OUT.APPLY='Y'
            IF Y.IN.TYPE EQ 'D' THEN
                Y.ACCT.TMP = Y.DEBIT.ACCOUNT
            END
            IF Y.IN.TYPE EQ 'C' THEN
                Y.ACCT.TMP = Y.CREDIT.ACCOUNT
            END
            IF Y.IN.ACCT EQ Y.ACCT.TMP THEN
                Y.OUT.TYPE=Y.IN.TYPE
            END ELSE
                Y.OUT.TYPE=''
            END
        CASE Y.IN.TYPE EQ 'A'
            Y.OUT.APPLY='Y'
            IF Y.IN.ACCT EQ Y.DEBIT.ACCOUNT THEN
                Y.OUT.TYPE= 'D'
            END

            IF Y.IN.ACCT EQ Y.CREDIT.ACCOUNT THEN
                Y.OUT.TYPE= 'C'
            END


    END CASE
    RETURN
*** </region>

    END


