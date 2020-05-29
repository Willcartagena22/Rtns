*-----------------------------------------------------------------------------
* <Rating>-31</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SVL.SET.CREDIT.ACCOUNT.COL
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
RETURN

OPEN.FILE:
RETURN

PROCESS:
CREDIT.ACCOUNT = COMI
ACCOUNT.NUMBER = FIELD(CREDIT.ACCOUNT,'-',1)

DEBIT.ACC = R.NEW(FT.DEBIT.ACCT.NO) 

IF ACCOUNT.NUMBER NE DEBIT.ACC THEN
    R.NEW(FT.CREDIT.ACCT.NO)  = ACCOUNT.NUMBER
END ;* END IF ACCOUNT.NUMBER NE DEBIT.ACC
ELSE
    LLAVE.BUSQUEDA = 'Cuenta credito no debe ser igual a la cuenta debito'
    ETEXT = LLAVE.BUSQUEDA
    CALL STORE.END.ERROR
END 

RETURN


END
