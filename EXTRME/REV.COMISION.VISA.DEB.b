*-----------------------------------------------------------------------------
* <Rating>-35</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REV.COMISION.VISA.DEB
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.EB.SLV.COMISIONES.TARJETAS
    $INSERT I_F.EB.SLV.GLOBAL.PARAM
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB PROCESS
  

*--------------------------
INIT:
*--------------------------
    FN.FT='F.FUNDS.TRANSFER'
    F.FT=''
    FN.SCT='F.EB.SLV.COMISIONES.TARJETAS'
    F.SCT=''
    FN.SLVPA 		= 'F.EB.SLV.GLOBAL.PARAM'
    F.SLVPA 		= ''
    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''
    ID.PARAM.OFS ='OFS.COM.VISA.DEB'
    TRANS.ID = ''
    STR.ERR = '/-1/'
    

    RETURN


*--------------------------------------------
PROCESS:
*--------------------------------------------

    CALL OPF(FN.FT,F.FT)
    CALL OPF(FN.SCT,F.SCT)
    CALL OPF(FN.SLVPA,F.SLVPA)

;*obtener Cuenta del IVA
    CALL F.READ(FN.SLVPA,'ACC.VISA.DEB.FISC', R.SLVPA, F.SLVPA, E.SLVPA)
    CUENTA.IVA = R.SLVPA<EB.SLV39.VALOR.PARAM>
    
*; obtener ID del OFS
    CALL GET.LOC.REF('FUNDS.TRANSFER','LF.IDOFS.CARD',POS.CMP2)
    ID.OFS = R.NEW(FT.LOCAL.REF)<1,POS.CMP2>
;*obtener el Monto del IVA
    CALL F.READ(FN.SCT, ID.OFS, R.SCT, F.SCT, E.SCT)
    MONTO.IVA.OFS = R.SCT<EB.SLV16.IVAAMOUNT>
    ORIGEN = R.SCT<EB.SLV16.ORIGIN>
;*Obtener valores de los campos en  REV.COMISSION.CARD
    CUENTACREDITO  	= 	R.NEW( FT.CREDIT.ACCT.NO)
    CUENTADEBITO	=	CUENTA.IVA
    ORDENBANCO 		= 	'REV CARGO IVA'
    TIPOTRANSACTION =   R.NEW(FT.TRANSACTION.TYPE)
	GOSUB WRITE

        ;*construir nueva FT
        CALL F.READ(FN.FT,'', R.FT,F.FT,E.FT)

        R.FT<FT.DEBIT.ACCT.NO>    =  CUENTADEBITO
        R.FT<FT.CREDIT.ACCT.NO>   =  CUENTACREDITO
        R.FT<FT.DEBIT.CURRENCY>   =  'USD'
        R.FT<FT.DEBIT.AMOUNT>     =  MONTO.IVA.OFS
        R.FT<FT.ORDERING.BANK>    =  ORDENBANCO
        R.FT<FT.TRANSACTION.TYPE> =  TIPOTRANSACTION

      
		CALL SLV.UTIL.OFS.TRX(TRANS.ID,R.FT,ID.PARAM.OFS,Y.OUT)
       
	
 RETURN

   WRITE:


*** <desc>Persistir en la App Local </desc>

    R.SCT <EB.SLV16.STATUS> = '2'
	R.SCT<EB.SLV16.ORDERING.BANK> = 'REV CARGO IVA'
    CALL F.WRITE (FN.SCT,ID.OFS, R.SCT)
   

    RETURN





END
