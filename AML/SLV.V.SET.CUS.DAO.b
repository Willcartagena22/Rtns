*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.V.SET.CUS.DAO
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.USER
*-----------------------------------------------------------------------------

    GOSUB INIT
    GOSUB OPEN
    GOSUB PROCESS
    RETURN

INIT:
    FN.CUS 	= 'F.CUSTOMER'
    F.CUS  	= ''
    FN.USER = 'F.USER'
    F.USER	= ''
    Y.DAO 	= R.USER<EB.USE.DEPARTMENT.CODE>
    CRT Y.DAO
    RETURN

OPEN:
	CALL OPF(FN.CUS,F.CUS)
	CALL OPF(FN.USER,F.USER)
    RETURN

PROCESS:
	R.NEW(EB.CUS.ACCOUNT.OFFICER) = Y.DAO
    RETURN

    END
