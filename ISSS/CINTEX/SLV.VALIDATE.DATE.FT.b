*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.VALIDATE.DATE.FT
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.FUNDS.TRANSFER
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPEN.FILE
GOSUB PROCESS
RETURN

INIT:
FN.FT = 'F.FUNDS.TRANSFER'
F.FT  = ''
RETURN

OPEN.FILE:
CALL OPF(FN.FT,F.FTT)
RETURN

PROCESS:
FECHA.COMPROBANTE = COMI
FECHA.REGISTRO    = R.NEW(FT.DEBIT.VALUE.DATE)

IF FECHA.COMPROBANTE < FECHA.REGISTRO THEN
            CALL GET.LOC.REF('FUNDS.TRANSFER','LF.FECHA.ABONO',POS.CMP1)
            AF = FT.LOCAL.REF
            AV = POS.CMP1
            LLAVE.BUSQUEDA = 'Fecha comprobante no debe ser menor a la fecha debito'
            ETEXT = LLAVE.BUSQUEDA
            CALL STORE.END.ERROR
END

RETURN


END
