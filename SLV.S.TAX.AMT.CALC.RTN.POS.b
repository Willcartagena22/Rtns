*-----------------------------------------------------------------------------
* <Rating>-71</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.S.TAX.AMT.CALC.RTN.POS(PASS.CUSTOMER, PASS.DEAL.AMOUNT,PASS.DEAL.CCY, PASS.CCY.MKT, PASS.CROSS.RATE,PASS.CROSS.CCY,PASS.DWN.CCY, PASS.DATA, PASS.CUST.CDN,R.TAX,TAX.AMOUNT)
*-----------------------------------------------------------------------------
* Company Name:          BANCO AZUL
* Developed By:          Prabhakaran.L, Capgemini
* Date  :                2015/05/11
*------------------------------------------------------------------------------
* Subroutine Type:       CALC.ROUTINE 
* Attached to:           TAX>17(LIOFPOS)
* Attached as:           CALC.ROUTINE routine
* Primary Purpose:       To calculate the TAX.AMOUNT for the transaction.
*------------------------------------------------------------------------------
* Modification History : 
*-------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.ACCOUNT
    $INSERT I_F.CUSTOMER
    $INSERT I_F.TAX
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.TELLER
    $INSERT I_F.TELLER.FINANCIAL.SERVICES
    $INSERT I_F.SLV.PROD.EXENTOS
    $INSERT I_ENQUIRY.COMMON
*-----------------------------------------------------------------------------
    
    GOSUB INIT
    RETURN

INIT:
*****

    FN.ACC = "F.ACCOUNT"
    F.ACC  = ""
    CALL OPF(FN.ACC,F.ACC)

    FN.CUS = "F.CUSTOMER"
    F.CUS  = ""
    CALL OPF(FN.CUS,F.CUS)

    FN.SLV.PROD.EXENTOS = "F.SLV.PROD.EXENTOS"
    F.SLV.PROD.EXENTOS  = ""
    CALL OPF(FN.SLV.PROD.EXENTOS,F.SLV.PROD.EXENTOS)


    Y.TXN.AMT       = PASS.DEAL.AMOUNT
    Y.LOCAL.CCY     = LCCY
    Y.TAX.CCY       = R.TAX<EB.TAX.CURRENCY>
    Y.TAX.CALC.TYP  = R.TAX<EB.TAX.CALC.TYPE>
    Y.TAX.BAND.RATE = R.TAX<EB.TAX.BANDED.RATE>
    Y.TAX.UPTO.AMT  = R.TAX<EB.TAX.UPTO.AMT>

    Y.TAX.CAL.FLAG = ""
    BEGIN CASE
        CASE APPLICATION EQ 'FUNDS.TRANSFER'
            GOSUB PROCESS.FT
        CASE APPLICATION EQ 'TELLER'
            GOSUB PROCESS.TT
    END CASE

    GOSUB TAX.CALC
    RETURN

PROCESS.FT:
***********
    Y.LIMIT.AMOUNT = '' ; Y.INT.TAX.PERCNT = ''
    LOCATE Y.LOCAL.CCY IN Y.TAX.CCY<1,1> SETTING TAX.FT.POS THEN
    Y.LIMIT.AMOUNT   = Y.TAX.UPTO.AMT<1,TAX.FT.POS,1>
    Y.INT.TAX.PERCNT = Y.TAX.BAND.RATE<1,TAX.FT.POS,2>

    Y.DEB.ACC = R.NEW(FT.DEBIT.ACCT.NO)
    GOSUB ACC.PROC
    END
    RETURN

PROCESS.TT:
***********
    Y.LIMIT.AMOUNT = '' ; Y.INT.TAX.PERCNT = ''
    LOCATE Y.LOCAL.CCY IN Y.TAX.CCY<1,1> SETTING TAX.TT.POS THEN
    Y.LIMIT.AMOUNT   = Y.TAX.UPTO.AMT<1,TAX.TT.POS,1>
    Y.INT.TAX.PERCNT = Y.TAX.BAND.RATE<1,TAX.TT.POS,2>
    Y.DEB.ACC = R.NEW(TT.TE.ACCOUNT.2)							;* NEED TO CHECK & CHANGE
    GOSUB ACC.PROC
    RETURN

ACC.PROC:
*********
    ERR.ACC = '' ; R.ACC = ''
    CALL F.READ(FN.ACC,Y.DEB.ACC,R.ACC,F.ACC,ERR.ACC)
    Y.ACC.CATEG = R.ACC<AC.CATEGORY>
    Y.DEB.CUS   = R.ACC<AC.CUSTOMER>
    GOSUB FET.CATE.EXEMPT
    GOSUB CUS.PROC
    END
    RETURN

CUS.PROC:
*********
    Y.APP   = "CUSTOMER"
    Y.FIELD = "LF.CTE.EXE"
    Y.POS   = ""
    CALL MULTI.GET.LOC.REF(Y.APP,Y.FIELD,Y.POS)
    Y.LF.CTE.EXE.POS = Y.POS<1,1>

    R.CUS = '' ; ERR.CUS = ''
    CALL F.READ(FN.CUS,Y.DEB.CUS,R.CUS,F.CUS,ERR.CUS)
    Y.CUS.EXMP.FLAG = R.CUS<EB.CUS.LOCAL.REF,Y.LF.CTE.EXE.POS>
    IF Y.CUS.EXMP.FLAG EQ 'SI' THEN
        Y.TAX.CAL.FLAG = "1"
    END
    RETURN

FET.CATE.EXEMPT:
****************
    PROD.EXENTOS = "" ; R.PROD.EXENTOS = ""
    Y.PROD.EXENTOS = "LIOFPOS"
    CALL F.READ(FN.SLV.PROD.EXENTOS,Y.PROD.EXENTOS,R.PROD.EXENTOS,F.SLV.PROD.EXENTOS,PROD.EXENTOS)
    Y.EXP.CATEG = R.PROD.EXENTOS<PROD.EXENT.EXEMPT.CATEG>
    LOCATE Y.ACC.CATEG IN Y.EXP.CATEG<1,1> SETTING EXM.CATEG THEN
    Y.TAX.CAL.FLAG = "1"
    END
    RETURN

TAX.CALC:
*********
    IF (Y.TXN.AMT GT Y.LIMIT.AMOUNT) AND (Y.TAX.CAL.FLAG NE '1') THEN
        TAX.AMOUNT = (Y.TXN.AMT * Y.INT.TAX.PERCNT)/100
    END
    RETURN
    END
