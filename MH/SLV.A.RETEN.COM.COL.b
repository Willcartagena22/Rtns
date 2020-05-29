*-----------------------------------------------------------------------------
* <Rating>-44</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.A.RETEN.COM.COL
*-----------------------------------------------------------------------------
* Descripcon: La RTN realiza el proceso para generer el OFS para la FT la cual se realiza para 
*             retencion 1%
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.AC.CHARGE.REQUEST
    $INSERT I_F.FUNDS.TRANSFER
*-----------------------------------------------------------------------------
    GOSUB INICIALIZACION
    GOSUB OPENFILE
    GOSUB PROCESS

INICIALIZACION:
    FN.AC.CHARGE = 'F.AC.CHARGE.REQUEST'
    F.AC.CHARGE = ''
    RETURN

OPENFILE:
    CALL OPF(FN.AC.CHARGE,F.AC.CHARGE)
    RETURN

PROCESS:

;*CALCULO DE LA RETENCION 1%
    MONTO.CON.IVA = R.NEW(CHG.TOTAL.CHG.AMT)
    MONTO.SIN.IVA = MONTO.CON.IVA/1.13
    RETENCION.1 = DROUND(MONTO.SIN.IVA * 0.01,2)

*-----------PARAMETROS PARA OFS-FT
    CC.COBRAR =  R.NEW(CHG.DEBIT.ACCOUNT)
    CALL GET.LOC.REF ('AC.CHARGE.REQUEST', 'LF.ACC.DESC', POS)
    LS.ACC.DESC = R.CUST<CHG.LOCAL.REF, POS>
    CC.RETENCION = R.NEW(CHG.LOCAL.REF)<1,POS>
    CARGO.TOTAL = R.NEW(CHG.TOTAL.CHG.AMT)
    ORDER.CUST = ID.NEW
    ORDER.BANK = FIELD(R.NEW(CHG.INPUTTER),'_',2):'-':R.NEW(CHG.CO.CODE)
    ID.PARAM.OFS = 'OFS.RETENCION.COMI.COL'
*-------------RECORD PARA FT
    TRANS.ID = ''
    R.FT = ''
    R.FT<FT.DEBIT.ACCT.NO> = CC.RETENCION
    R.FT<FT.CREDIT.ACCT.NO> = CC.COBRAR
    R.FT<FT.DEBIT.CURRENCY> = 'USD'
    R.FT<FT.DEBIT.AMOUNT> = RETENCION.1
    R.FT<FT.ORDERING.BANK> = ORDER.BANK
    R.FT<FT.ORDERING.CUST> = ORDER.CUST
    R.FT<FT.TRANSACTION.TYPE> ='AC'
    Y.OUT = ''
    
    TEXT.ARCHIVO = R.FT
    GOSUB ESCRIBIR.ARCHIVO
    CALL SLV.UTIL.OFS.TRX(TRANS.ID,R.FT,ID.PARAM.OFS,Y.OUT)
    TEXT.ARCHIVO = Y.OUT
    GOSUB ESCRIBIR.ARCHIVO
    RETURN
*
ESCRIBIR.ARCHIVO:
    DIR.NAME= 'MHLogs'
    R.ID   = 'FT.COMISSION.RETENCION':TODAY:'.txt'
;* hacer que escriba un archivo
    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXT.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
    RETURN

    END
