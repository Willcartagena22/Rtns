*-----------------------------------------------------------------------------
* <Rating>-58</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.V.PEN.COMM.VISA.DEB

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
    GOSUB WRITE


*--------------------------
INIT:
*--------------------------
    FN.FT='F.FUNDS.TRANSFER'
    F.FT=''
    FN.SCT='F.EB.SLV.COMISIONES.TARJETAS'
    F.SCT=''
    FN.SLVPA= 'F.EB.SLV.GLOBAL.PARAM'
    F.SLVPA= ''
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
    CALL GET.LOC.REF('FUNDS.TRANSFER','LF.OFS.FT.CARGO',POS.CMP2)
    CUENTACLIENTE = R.NEW(FT.LOCAL.REF)<1,POS.CMP2>
    CALL GET.LOC.REF('FUNDS.TRANSFER','LF.ACC.ATM.COM',POS.CMP3)
    CUENTAATM = R.NEW(FT.LOCAL.REF)<1,POS.CMP3>
;*obtener el Monto del IVA
    CALL F.READ(FN.SCT, ID.OFS, R.SCT, F.SCT, E.SCT)
    MONTO.IVA.OFS = R.SCT<EB.SLV16.IVAAMOUNT>
    ORIGEN = R.SCT<EB.SLV16.ORIGIN>
;*Obtener valores de los campos en  REV.COMISSION.CARD
    CUENTACREDITO  	= 	R.NEW( FT.CREDIT.ACCT.NO)
    CUENTADEBITO	=	R.NEW( FT.DEBIT.ACCT.NO)
    ORDENBANCO 		= 	R.NEW(FT.ORDERING.BANK)
    TIPOTRANSACTION =   R.NEW(FT.TRANSACTION.TYPE)
   
   RETURN



LIQUIDAR:
	MONTO1=MONTO*0.13
    MONTO1=DROUND(MONTO1,2)
    GOSUB PAGAR

	
	CUENTADEBITO1=CUENTACLIENTE
	CUENTACREDITO1=CUENTADEBITO
	MONTO1=MONTO
	
	GOSUB PAGAR
	
	MONTO1=MONTO*0.13
    MONTO1=DROUND(MONTO1,2)
    GOSUB PAGAR

    CUENTADEBITO1=CUENTACREDITO
    CUENTACREDITO1=CUENTAATM
    MONTO1=MONTO
    GOSUB PAGAR
    
    MONTO1=MONTO*0.13
    MONTO1=DROUND(MONTO1,2)
    CUENTACREDITO1=CUENTA.IVA
    GOSUB PAGAR
    
    MONTO1=MONTO
  
    R.SCT.STATUS='1'
    STMT.FT = "SELECT ":FN.FT:" LF.IDOFS.CARD EQ '":IDOFS:"' AND CARD.TYPE EQ 'D'"
    CALL EB.READLIST(STMT.FT,FT.LIST,'',NO.OF.FT,FT.ERR)
    FT=FIELD(FT.LIST,FM,1)
    R.SCT<EB.SLV16.ID.OFS> =FT    
    R.SCT<EB.SLV16.STATUS>=R.SCT.STATUS
    R.SCT <EB.SLV16.ERROR> = ''
    CALL F.WRITE (FN.SCT,IDOFS, R.SCT)
*    CALL JOURNAL.UPDATE(FN.SCT)
   
         
RETURN


PAGAR:
    Y.OUT=''
    FT=''
    CALL F.READ(FN.FT,'', R.FT,F.FT,E.FT)
    R.FT<FT.DEBIT.ACCT.NO>    =  CUENTADEBITO1
    R.FT<FT.CREDIT.ACCT.NO>   =  CUENTACREDITO1
    R.FT<FT.DEBIT.CURRENCY>   =  CURRENCY
    R.FT<FT.DEBIT.AMOUNT>     =  MONTO1
    R.FT<FT.ORDERING.BANK>    =  ORDENBANCO
    R.FT<FT.TRANSACTION.TYPE> =  TIPOTRANSACTION
;*Enviando campo local que se utilizara como identificador
    CALL GET.LOC.REF('FUNDS.TRANSFER','LF.IDOFS.CARD', LocPosIdOfsCard)
    R.FT<FT.LOCAL.REF,LocPosIdOfsCard> = IDOFS
;*Llamada de rutina para aplicar OFS

    CALL SLV.UTIL.OFS.TRX(TRANS.ID,R.FT,ID.PARAM.OFS,Y.OUT)



    RETURN


WRITE:

*** <desc>Persistir en la App Local </desc>

    R.SCT <EB.SLV16.STATUS> = '2'

    CALL F.WRITE (FN.SCT,IDOFS, R.SCT)
*    CALL JOURNAL.UPDATE(FN.SCT)


    RETURN




    END
