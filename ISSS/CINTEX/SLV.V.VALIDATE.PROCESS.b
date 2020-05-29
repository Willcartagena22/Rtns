*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.VALIDATE.PROCESS
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_TSS.COMMON
$INSERT I_GTS.COMMON
*-----------------------------------------------------------------------------

GOSUB PROCESS
RETURN

PROCESS:

CREDIT.ACC = COMI
DEBIT.ACC  = R.NEW(FT.DEBIT.ACCT.NO)

IF CREDIT.ACC EQ DEBIT.ACC THEN
            LLAVE.BUSQUEDA = 'Cuenta credito no debe ser igual a la cuenta debito'
            ETEXT = LLAVE.BUSQUEDA
            CALL STORE.END.ERROR
END

RETURN

END
