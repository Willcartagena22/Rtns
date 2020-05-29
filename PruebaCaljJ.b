*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE PruebaCaljJ

*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
   	$INSERT I_F.CUSTOMER
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB PROCESS
    GOSUB OPEN

INIT:
    FN.CUS = 'F.CUSTOMER'
    F.CUS  = ''

    RETURN

OPEN:
 CALL OPF(FN.CUS, F.CUS)
RETURN


PROCESS:
;*amoreno@gmail.com
VAR.CLIENTE.ID='102379'
CALL F.READ(FN.CUS,VAR.CLIENTE.ID,R.CUS,F.CUS,Y.ERR)
EMAIL=R.CUS<EB.CUS.EMAIL.1><1,1>
CRT 'EMAIL :':EMAIL
RETURN
