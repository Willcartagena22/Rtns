*-----------------------------------------------------------------------------
* <Rating>-51</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.VISA.CLOSURE.ACCOUNT
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.COMPANY
    $INSERT I_F.EB.SLV.COMISIONES.TARJETAS
    $INSERT I_F.AA.ACCOUNT.CLOSURE.DETAILS
*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB OPENFILE
GOSUB PROCESS:
INIT:

    FN.FT='F.FUNDS.TRANSFER'
    F.FT=''
    FN.SCT='F.EB.SLV.COMISIONES.TARJETAS'
    F.SCT=''
    FN_SLVPA= 'F.EB.SLV.GLOBAL.PARAM'
    F_SLVPA= ''
    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''
 

    RETURN

OPENFILE:
    CALL OPF(FN.FT,F.FT)
    CALL OPF(FN.SCT,F.SCT)
    CALL OPF(FN.ACC,F.ACC)
    CALL OPF(FN_SLVPA,F_SLVPA)
    
RETURN
CUENTACLIENTE=R.NEW(AA.ACC6.ACCOUNT.NO)


PROCESS:

 STMT.SCT = "SELECT ":FN.SCT:" STATUS EQ '0' AND CARD.TYPE EQ 'D'"
;*Aplicando el Statement
CALL EB.READLIST(STMT.SCT, SCT.LIST,'',NO.OF.SCT,SCT.ERR)


FOR I=0 TO NO.OF.SCT
R.FT = ''
    CALL F.READ(FN.SCT,SCT.LIST<I>,AR.SCT,F.SCT,ERR.SCT)
	CUENTACLIENTE1=AR.SCT<EB.SLV16.ACC.CUSTOMER>	
  IF CUENTACLIENTE EQ CUENTACLIENTE1 THEN 
   STRERR = 'Cuenta posee cargos pendientes'
   GOSUB CRT_ERROR
        
  END
    NEXT I
RETURN

CRT_ERROR:
    ETEXT = STRERR
    AS = 1
    CALL STORE.END.ERROR
    RETURN








END
