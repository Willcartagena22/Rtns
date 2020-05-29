*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.SET.FECHA.V.CRED
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
FECHA.V.DEB=R.NEW(FT.DEBIT.VALUE.DATE)
R.NEW(FT.CREDIT.VALUE.DATE)=FECHA.V.DEB
RETURN



END
