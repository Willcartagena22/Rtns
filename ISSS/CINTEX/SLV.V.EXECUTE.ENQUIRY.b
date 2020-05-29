*-----------------------------------------------------------------------------
* <Rating>-21</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.EXECUTE.ENQUIRY
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.ENQUIRY
$INSERT I_ENQUIRY.COMMON
$INSERT I_F.AC.CHARGE.REQUEST
$INSERT I_F.FUNDS.TRANSFER
*-----------------------------------------------------------------------------

GOSUB PROCESS
RETURN

PROCESS:

NOMBRE.VERSION = PGM.VERSION

ITEM.SELECCIONADO = COMI
CODIGO.COLECTOR = FIELD(ITEM.SELECCIONADO,'-',1)

IF NOMBRE.VERSION EQ ',SLV.COBRO.COMISION.COLECTORES' THEN

CALL GET.LOC.REF('AC.CHARGE.REQUEST','LF.COD.CL',POS.NC)
R.NEW(CHG.LOCAL.REF)<1,POS.NC>= CODIGO.COLECTOR
*R.NEW(CHG.CHARGE.AMOUNT) = '3.00'
END
ELSE
CALL GET.LOC.REF('FUNDS.TRANSFER','LF.COD.CL',POSCodigoColector)
R.NEW(FT.LOCAL.REF)<1,POSCodigoColector> = CODIGO.COLECTOR
END

RETURN


ESCRIBIR.ARCHIVO: 
DIR.NAME= 'COLECTORES'
R.ID   = 'RTN_EXECUTE_ENQUIRY':TODAY:'.txt'
;* hacer que escriba un archivo 
OPENSEQ DIR.NAME,R.ID TO SEQ.PTR 
        WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
END 
CLOSESEQ SEQ.PTR 
RETURN

END
