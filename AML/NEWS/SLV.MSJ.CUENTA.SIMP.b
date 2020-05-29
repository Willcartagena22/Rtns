*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.MSJ.CUENTA.SIMP
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.CUSTOMER
$INSERT I_AA.APP.COMMON 
$INSERT I_F.AA.PRODUCT.ACCESS  
$INSERT I_F.AA.ARRANGEMENT
$INSERT I_F.AA.ARRANGEMENT.ACTIVITY 
$INSERT I_F.AA.CUSTOMER

*-----------------------------------------------------------------------------
GOSUB INIT
GOSUB ENVIAR.MENSAJE

INIT:
FN.TABLE.PA = 'F.EB.SLV.GLOBAL.PARAM'
F.TABLE.PA = ''
FN.CUS	=	'F.CUSTOMER'
F.CUS	=	''
CUSTOMER = AA$R.ARRANGEMENT<AA.ARR.CUSTOMER>
CALL OPF(FN.TABLE.PA, F.TABLE.PA)
CALL OPF(FN.CUS, F.CUS)
RETURN

ENVIAR.MENSAJE:
	PARAM.LOG2='SLV.MSJ.CUENTA.SIMP.ACTIVA'
    CALL F.READ(FN.TABLE.PA, PARAM.LOG2, R.TABLE.PA.2, F.TABLE.PA, F.ERR.PA.2)
    TEXTO.MSJ=R.TABLE.PA.2<EB.SLV39.VALOR.PARAM>    
    CALL F.READ(FN.CUS, CUSTOMER, R.CUS, F.CUS, F.ERR.CUS)
    TELEFONO=R.CUS<EB.CUS.SMS.1>
    CALL SLV.ENVIO.MENSAJES.HDM(TELEFONO,TEXTO.MSJ)

RETURN
END
