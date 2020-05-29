*-----------------------------------------------------------------------------
* <Rating>-20</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.AML.CHECK
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.EB.SLV.GLOBAL.PARAM
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB PROCESS
    RETURN

INIT:
    FN.TABLE.PA = 'F.EB.SLV.GLOBAL.PARAM'
    F.TABLE.PA = ''

    CALL OPF(FN.TABLE.PA, F.TABLE.PA)

    RETURN

PROCESS:
    Y.AML.CHECK = 'AML.SCREEN.CHECK'
    CALL F.READ(FN.TABLE.PA, Y.AML.CHECK, R.TABLE.PA, F.TABLE.PA, F.ERR.PA)
    Y.ACTIVATE = R.TABLE.PA<EB.SLV39.VALOR.PARAM>

    IF Y.ACTIVATE EQ '1' THEN
		CALL VL.AUTH.DISPO.ITEMS
    END
    RETURN

    END
