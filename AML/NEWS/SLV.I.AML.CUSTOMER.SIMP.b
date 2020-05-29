*-----------------------------------------------------------------------------
* <Rating>-31</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.AML.CUSTOMER.SIMP
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.CUSTOMER
$INSERT I_F.EB.SLV.AML.CUSTOMER.SIMP
$INSERT I_AA.APP.COMMON 
$INSERT I_F.EB.EXTERNAL.USER 
$INSERT I_F.AA.PRODUCT.ACCESS  
$INSERT I_F.AA.ARRANGEMENT
$INSERT I_F.AA.ARRANGEMENT.ACTIVITY 
$INSERT I_F.AA.CUSTOMER
*-----------------------------------------------------------------------------
GOSUB INICIAR
GOSUB ABRIR
GOSUB PROCESAR
RETURN

INICIAR:
FN.AML='F.EB.SLV.AML.CUSTOMER.SIMP'
F.AML=''
FN.CUS= 'F.CUSTOMER'
F.CUS= ''
EQU ACTIVITY.ACC TO 'ACCOUNTS-NEW-ARRANGEMENT'

RETURN

ABRIR:
CALL OPF(FN.AML, F.AML)
CALL OPF(FN.CUS, F.CUS)
RETURN

PROCESAR:
CUSTOMER=ID.NEW
PROCESS='NO'
DATE.C=TODAY
CALL GET.LOC.REF('CUSTOMER','LF.DUI',POS.DUI)
CALL GET.LOC.REF('CUSTOMER','LF.NOB.NIT',POS.NOB.NIT)
DUI= R.NEW(EB.CUS.LOCAL.REF)<1,POS.DUI>
R.NEW(EB.CUS.LOCAL.REF)<1,POS.NOB.NIT>=R.NEW(EB.CUS.SHORT.NAME)

;*Escribir en App local Aml cuenta simp
R.SCT<EB.EB.84.CUSTOMER> = CUSTOMER
R.SCT<EB.EB.84.PROCESSING> = PROCESS
R.SCT<EB.EB.84.DATE.C>=DATE.C
R.SCT<EB.EB.84.DUI> = DUI
R.SCT<EB.EB.84.RESERVADO.4> = 'NO'
CALL F.WRITE (FN.AML,CUSTOMER, R.SCT)

 
RETURN





END
