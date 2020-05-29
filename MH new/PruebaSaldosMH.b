*-----------------------------------------------------------------------------
* <Rating>-31</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE PruebaSaldosMH
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_TSS.COMMON
    $INSERT I_GTS.COMMON
    $INSERT I_F.EB.SLV.COL.FRONT.END
    $INSERT I_F.ACCOUNT
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.STMT.ENTRY
*-----------------------------------------------------------------------------

    GOSUB INICIALIZAR
    GOSUB OPENFILE
    GOSUB CUENTA.COLECTOR

    RETURN

INICIALIZAR:
    FN.COLECTOR	= 'F.EB.SLV.COL.FRONT.END'
    F.COLECTOR	= ''

    FN.ACC = 'F.ACCOUNT'
    F.ACC = ''

    FN.FT = 'F.FUNDS.TRANSFER'
    F.FT = ''

    FN.STMT.ENTRY = 'F.STMT.ENTRY'
    F.STMT.ENTRY = ''

;*VARIABLE PARA GENERAR EL JSON A ENVIAR
    JSON.SALDOS = '['

    RETURN

OPENFILE:
    CALL OPF(FN.COLECTOR, F.COLECTOR)
    CALL OPF(FN.ACC,F.ACC)
    CALL OPF(FN.FT, F.FT)
    CALL OPF(FN.STMT.ENTRY,F.STMT.ENTRY)



 RETURN


CUENTA.COLECTOR:

END
