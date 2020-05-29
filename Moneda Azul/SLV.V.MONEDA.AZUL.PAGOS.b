*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.MONEDA.AZUL.PAGOS
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.EB.SLV.PAGOS.MONEDA.AZUL
$INSERT I_F.TELLER.FINANCIAL.SERVICES
$INSERT I_F.TELLER
$INSERT I_ENQUIRY.COMMON
*-----------------------------------------------------------------------------
    GOSUB INIT
    GOSUB OPENFILE
    GOSUB PROCESS

INIT:
 FN.MONEDA.AZUL='F.EB.SLV.PAGOS.MONEDA.AZUL'
 F.MONEDA.AZUL=''
 FN_TELLER 		= 'F.TELLER.FINANCIAL.SERVICES'
 F_TELLER 		= ''
RETURN

OPENFILE:
CALL OPF(FN.MONEDA.AZUL,F.MONEDA.AZUL)
CALL OPF(FN_TELLER, F_TELLER)
RETURN

PROCESS:
ID_TNX 			= ID.NEW

CALL F.READ(FN_TELLER, ID_TNX, R_TELLER, F_TELLER, E_TELLER)

CUSTOMER= R.NEW(TFS.PRIMARY.CUSTOMER)
MONTO= R.NEW(TFS.AMOUNT.CR)

FECHA=TODAY
R.MON<EB.PMA.CUSTOMER>=CUSTOMER
R.MON<EB.PMA.DATE.TIME>=FECHA
R.MON<EB.PMA.FECHA.PAGO>=FECHA
R.MON<EB.PMA.MONTO>=MONTO
R.MON<EB.PMA.INPUTTER> = OPERATOR
R.MON<EB.PMA.AUTHORISER>= OPERATOR
R.MON<EB.PMA.CURR.NO> += 1
	
CALL GET.LOC.REF('TELLER.FINANCIAL.SERVICES', 'LF.NUM.REF', POS)
RESERVA = R.NEW(TFS.LOCAL.REF)<1,POS>


R.MON<EB.PMA.RESERVA>=RESERVA   

CALL F.WRITE (FN.MONEDA.AZUL,ID_TNX,R.MON)



RETURN




END
