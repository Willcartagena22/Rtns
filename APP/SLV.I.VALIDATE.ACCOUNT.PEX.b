*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.VALIDATE.ACCOUNT.PEX
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.FUNDS.TRANSFER
$INSERT I_F.EB.SLV.GLOBAL.PARAM


GOSUB INIT
GOSUB OPEN
GOSUB PROCESS

INIT:
    FN.FT 		= 'F.FUNDS.TRANSFER'
    F.FT  		= ''
    
    FN.EB.SLV.GLOBAL.PARAM = 'F.EB.SLV.GLOBAL.PARAM'
    F.EB.SLV.GLOBAL.PARAM = ''
RETURN

OPEN:
CALL OPF(FN.EB.SLV.GLOBAL.PARAM, F.EB.SLV.GLOBAL.PARAM)
CALL OPF(FN.FT, F.FT)
RETURN


PROCESS:
R.NEW(FT.CREDIT.ACCT.NO)='USD1760500100001'
RETURN


*-----------------------------------------------------------------------------

END
