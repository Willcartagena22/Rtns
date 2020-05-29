*-----------------------------------------------------------------------------
* <Rating>-42</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.V.VALIDAR.FECHA.VALOR
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER


    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS


INIT:

    FN_FUNDS.TRANSFER 	= 'F.FUNDS.TRANSFER'
    F_FUNDS.TRANSFER	= ''

    RETURN

OPENFILES:
    CALL OPF(FN_FUNDS.TRANSFER, F_FUNDS.TRANSFER)
    RETURN


PROCESS:
    FECHA.VALOR.DEB=R.NEW(FT.DEBIT.VALUE.DATE)

    FECHA.ABONO = COMI;*R.NEW(FT.LOCAL.REF)<1,POS>

    ;*CALL GET.LOC.REF ('FUNDS.TRANSFER', 'LF.FECHA.ABONO', POS)
    ;*FECHA.ABONO.1 = R.NEW(FT.LOCAL.REF)<1,POS>

    IF FECHA.ABONO LT FECHA.VALOR.DEB THEN
        STRERR = 'Fecha Abono debe de ser mayor o igual a la fecha valor debito'
        GOSUB CRT_ERROR
    END
    RETURN
CRT_ERROR:
    ETEXT = STRERR
    AS = 1
    CALL STORE.END.ERROR
    RETURN


    END
