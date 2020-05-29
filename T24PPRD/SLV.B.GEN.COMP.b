*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.B.GEN.COMP
*-----------------------------------------------------------------------------
*autor eurias
*date 20160601
*util invocacion de rutinas generacion masiva de comprobantes por abono a cuenta, pago prestamo Bulk Payment
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
*-----------------------------------------------------------------------------

CALL SLV.B.GEN.COMP.AH
CALL SLV.B.GEN.COMP.BPYMT

END
