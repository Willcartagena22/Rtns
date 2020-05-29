*-----------------------------------------------------------------------------
* <Rating>-30</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.GET.RAZON.SOCIAL(Y.CUENTA)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
* Author: Ronald Ortiz
* Date:   20161130
* Util:   Rutina para obtener la razon social del cliente.
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.ACCOUNT
$INSERT I_F.CUSTOMER
*-----------------------------------------------------------------------------

GOSUB INIT
GOSUB OPEN.FILE
GOSUB PROCCESS
RETURN

INIT:

FN.CUSTOMER = 'F.CUSTOMER'
F.CUSTOMER  = ''

FN.ACCOUNT = 'F.ACCOUNT'
F.ACCOUNT  = ''

APP = 'CUSTOMER'

RETURN

OPEN.FILE:
CALL OPF(FN.ACCOUNT,F.ACCOUNT)
CALL OPF(FN.CUSTOMER,F.CUSTOMER)
RETURN


PROCCESS:

ID.CUENTA = Y.CUENTA

CALL F.READ(FN.ACCOUNT,ID.CUENTA,R.ACCOUNT,F.ACCOUNT,ERR.ACCOUNT)

IF R.ACCOUNT THEN

Y.CUSTOMER = R.ACCOUNT<AC.CUSTOMER>

CALL F.READ(FN.CUSTOMER,Y.CUSTOMER,R.CUSTOMER,F.CUSTOMER,ERR.CUSTOMER)

IF R.CUSTOMER THEN
     
     CALL GET.LOC.REF("CUSTOMER","SEGMENT",SEG.POS)
     CUSTOMER.TYPE = R.CUSTOMER<EB.CUS.LOCAL.REF, SEG.POS>
     
     IF CUSTOMER.TYPE EQ "1" THEN
         Y.CUENTA = R.CUSTOMER<EB.CUS.NAME.1>:" ":R.CUSTOMER<EB.CUS.NAME.2>:" ":R.CUSTOMER<EB.CUS.TEXT>:" ":R.CUSTOMER<EB.CUS.FAMILY.NAME>
     END
     ELSE
         CALL GET.LOC.REF(APP,'LF.RAZON.SOCIAL',POS.1)
         Y.CUENTA = R.CUSTOMER<EB.CUS.LOCAL.REF,POS.1>
     END

   
END

END

RETURN

END
