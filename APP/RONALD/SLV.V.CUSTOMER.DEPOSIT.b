*-----------------------------------------------------------------------------
* <Rating>-32</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.CUSTOMER.DEPOSIT
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.AA.ARRANGEMENT.ACTIVITY
$INSERT I_F.CUSTOMER
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPEN.FILE
GOSUB PROCESS
RETURN

INIT:
CUSTOMER.FN = 'F.CUSTOMER'
CUSTOMER.F  = ''
RETURN

OPEN.FILE:
CALL OPF(CUSTOMER.FN,CUSTOMER.F)
RETURN

PROCESS:
  CUSTOMER.VERSION    = R.NEW(AA.ARR.ACT.CUSTOMER)
  ;*OBTENEMOS EL DETALLE DEL CLIENTE
  CALL F.READ(CUSTOMER.FN,CUSTOMER.VERSION,CUSTOMER.R,CUSTOMER.F,CUSTOMER.ERR)
  ;*OBTENEMOS EL NUMERO DE NIT
  CALL GET.LOC.REF('CUSTOMER','LF.NIT',POS.NIT.DEPOSIT)
  
  IF CUSTOMER.R<EB.CUS.LOCAL.REF,POS.NIT.DEPOSIT> EQ '' THEN
            AF = AA.ARR.ACT.CUSTOMER
            AV = AA.ARR.ACT.CUSTOMER
            LLAVE.BUSQUEDA = 'No se pueden crear depositos para clientes simplificados'
            ETEXT = LLAVE.BUSQUEDA
            CALL STORE.END.ERROR
  END
  
RETURN


END
